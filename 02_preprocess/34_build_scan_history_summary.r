# ==== 34_build_scan_history_summary.r =======================================
##
## PURPOSE:
##   Build compact historical soil-moisture context for the BRIM SCAN Ops Live
##   layer.
##
## DESIGN:
##   - This is a local/heavier preprocessing step, not a daily GitHub Action.
##   - Historical SCAN daily soil moisture is fetched with soilDB::fetchSCAN().
##   - Raw-ish daily history is cached locally so reruns do not repeatedly hit
##     the NRCS/NWCC service.
##   - A compact water-day percentile table is written to both:
##       1. local processed RDS/QA outputs, and
##       2. brim-live-data-feeds/data/input/ for later joining into the hosted
##          SCAN live feed.
##
## WHY THIS EXISTS:
##   The SCAN live layer should eventually show NRCS-style popup plots:
##     historical percentile ribbons + current water-year black trace.
##
##   The historical percentile ribbons are relatively stable and are better
##   built locally.  GitHub Actions should remain focused on current/latest
##   daily feed refreshes.
##
## INPUTS:
##   brim-live-data-feeds/data/input/scan_station_index.csv
##
## OUTPUTS:
##   04_processed_data/rds/scan_sms_daily_history.rds
##   04_processed_data/rds/scan_sms_waterday_percentiles.rds
##   brim-live-data-feeds/data/input/scan_sms_waterday_percentiles.csv
##
## QA:
##   04_processed_data/qa/scan_sms_history_run_summary.csv
##   04_processed_data/qa/scan_sms_history_station_summary.csv
##   04_processed_data/qa/scan_sms_history_depth_summary.csv
## ============================================================================


# ==== 1. Load configuration ==================================================

source("00_config/config_paths.r")


# ==== 2. Load packages =======================================================

required_pkgs <- c(
  "soilDB",
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "tibble",
  "stringr",
  "lubridate"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them before running this local historical preprocessor."
  )
}

suppressPackageStartupMessages({
  library(soilDB)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(stringr)
  library(lubridate)
})


# ==== 3. User-facing switches ================================================

## Set TRUE only when you intentionally want to refetch all site histories from
## NRCS/NWCC.  Normal reruns should use the cached per-site RDS files.
REFRESH_SITE_CACHE <- FALSE

## Use a small pause between requests so this remains polite to the service.
REQUEST_PAUSE_SEC <- 0.15

## Current-water-year data are kept in the daily-history RDS, but excluded from
## the percentile ribbon climatology.  The live feed will provide the current WY
## black trace separately.
EXCLUDE_CURRENT_WY_FROM_PERCENTILES <- TRUE

## Minimum number of distinct historical water years for a percentile row to be
## considered useful for display. Rows below this threshold are retained but
## marked climatology_ok = FALSE.
## RF065b: use a 7-year reference threshold for SCAN daily ribbons. This is a
## station/depth reference period for screening, not a formal climate normal.
MIN_YEARS_FOR_CONTEXT <- 7L

## Standard SCAN/SNOTEL soil-depth display order. soilDB returns depth in cm.
STANDARD_DEPTHS_IN <- c(2, 4, 8, 20, 40)


# ==== 4. Paths ================================================================

SCAN_INDEX_CSV <- file.path("brim-live-data-feeds", "data", "input", "scan_station_index.csv")

DIR$scan_history_cache <- file.path(DIR$raw, "snow_soil_climate", "cache", "scan_sms_history")
dir.create(DIR$scan_history_cache, recursive = TRUE, showWarnings = FALSE)

dir.create(DIR$rds, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR$qa,  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("brim-live-data-feeds", "data", "input"), recursive = TRUE, showWarnings = FALSE)

OUT_DAILY_HISTORY_RDS <- file.path(DIR$rds, "scan_sms_daily_history.rds")
OUT_PERCENTILES_RDS   <- file.path(DIR$rds, "scan_sms_waterday_percentiles.rds")
OUT_PERCENTILES_CSV   <- file.path("brim-live-data-feeds", "data", "input", "scan_sms_waterday_percentiles.csv")

OUT_RUN_SUMMARY_CSV     <- file.path(DIR$qa, "scan_sms_history_run_summary.csv")
OUT_STATION_SUMMARY_CSV <- file.path(DIR$qa, "scan_sms_history_station_summary.csv")
OUT_DEPTH_SUMMARY_CSV   <- file.path(DIR$qa, "scan_sms_history_depth_summary.csv")


# ==== 5. Small helpers ========================================================

pt_scan_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_scan_site_cache_path <- function(site_code) {
  file.path(DIR$scan_history_cache, paste0("scan_sms_site_", site_code, ".rds"))
}

pt_scan_current_water_year <- function(date = Sys.Date()) {
  date <- as.Date(date)
  yr <- lubridate::year(date)
  ifelse(lubridate::month(date) >= 10, yr + 1L, yr)
}

pt_scan_water_year <- function(date) {
  date <- as.Date(date)
  yr <- lubridate::year(date)
  ifelse(lubridate::month(date) >= 10, yr + 1L, yr)
}

pt_scan_water_day <- function(date) {
  date <- as.Date(date)
  wy <- pt_scan_water_year(date)
  wy_start <- as.Date(sprintf("%04d-10-01", wy - 1L))
  as.integer(date - wy_start) + 1L
}

pt_scan_depth_in <- function(depth_cm) {
  depth_cm <- suppressWarnings(as.numeric(depth_cm))

  dplyr::case_when(
    is.na(depth_cm) ~ NA_real_,
    abs(depth_cm - 5)   <= 1.5 ~ 2,
    abs(depth_cm - 10)  <= 2.0 ~ 4,
    abs(depth_cm - 20)  <= 3.0 ~ 8,
    abs(depth_cm - 51)  <= 5.0 ~ 20,
    abs(depth_cm - 102) <= 8.0 ~ 40,
    TRUE ~ round(depth_cm / 2.54, 1)
  )
}

pt_scan_fetch_sms_one_site <- function(site_code, years) {

  site_code <- as.integer(site_code)
  years <- sort(unique(as.integer(years)))
  years <- years[!is.na(years)]

  if (length(years) == 0 || is.na(site_code)) {
    return(tibble::tibble())
  }

  ## Try the efficient multi-year call first. If that fails, fall back to
  ## year-by-year retrieval so one bad year does not lose the whole station.
  out <- tryCatch(
    {
      x <- soilDB::fetchSCAN(
        site.code  = site_code,
        year       = years,
        report     = "SMS",
        timeseries = "Daily",
        tz         = "UTC"
      )
      x$SMS
    },
    error = function(e) {
      message("  Multi-year fetch failed for site ", site_code, ": ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(out)) {
    return(tibble::as_tibble(out))
  }

  pieces <- vector("list", length(years))

  for (i in seq_along(years)) {
    yy <- years[[i]]

    pieces[[i]] <- tryCatch(
      {
        x <- soilDB::fetchSCAN(
          site.code  = site_code,
          year       = yy,
          report     = "SMS",
          timeseries = "Daily",
          tz         = "UTC"
        )
        tibble::as_tibble(x$SMS)
      },
      error = function(e) {
        message("    Year ", yy, " failed for site ", site_code, ": ", conditionMessage(e))
        tibble::tibble()
      }
    )

    Sys.sleep(REQUEST_PAUSE_SEC)
  }

  dplyr::bind_rows(pieces)
}

pt_scan_read_or_fetch_site <- function(site_code, years) {

  cache_path <- pt_scan_site_cache_path(site_code)

  if (!REFRESH_SITE_CACHE && file.exists(cache_path)) {
    return(readRDS(cache_path))
  }

  message("Fetching SCAN daily SMS history for site ", site_code, " (", min(years), "-", max(years), ")")

  out <- pt_scan_fetch_sms_one_site(site_code = site_code, years = years)

  saveRDS(out, cache_path)
  Sys.sleep(REQUEST_PAUSE_SEC)

  out
}


# ==== 6. Read station index ==================================================

if (!file.exists(SCAN_INDEX_CSV)) {
  stop(
    "Missing SCAN live-feed station index: ", SCAN_INDEX_CSV,
    "\nRun source('run_build_map.r'); export_scan_live_inputs() first."
  )
}

message("Reading SCAN live-feed station index: ", SCAN_INDEX_CSV)

scan_index <- readr::read_csv(SCAN_INDEX_CSV, show_col_types = FALSE) |>
  dplyr::mutate(
    site_code = suppressWarnings(as.integer(site_code)),
    start_year = suppressWarnings(as.integer(start_year)),
    start_year = dplyr::if_else(is.na(start_year), 1991L, start_year),
    fetch_start_year = pmax(start_year, 1991L, na.rm = TRUE),
    fetch_end_year = lubridate::year(Sys.Date())
  ) |>
  dplyr::filter(!is.na(site_code)) |>
  dplyr::arrange(site_code)

if (nrow(scan_index) == 0) {
  stop("SCAN station index has no usable site_code values.")
}

current_wy <- pt_scan_current_water_year(Sys.Date())
feed_build_time_utc <- format(lubridate::with_tz(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%SZ")


# ==== 7. Fetch / read daily SMS history ======================================

history_pieces <- vector("list", nrow(scan_index))

for (i in seq_len(nrow(scan_index))) {

  st <- scan_index[i, ]
  years <- seq.int(st$fetch_start_year, st$fetch_end_year)

  raw <- pt_scan_read_or_fetch_site(site_code = st$site_code, years = years)

  if (nrow(raw) == 0) {
    history_pieces[[i]] <- tibble::tibble()
    next
  }

  history_pieces[[i]] <- raw |>
    tibble::as_tibble() |>
    dplyr::transmute(
      station_uid = st$station_uid,
      station_name = st$station_name,
      site_code = as.integer(Site),
      date = as.Date(Date),
      time = as.character(Time),
      datetime_utc = as.character(datetime),
      sms_pct = suppressWarnings(as.numeric(value)),
      depth_cm = suppressWarnings(as.numeric(depth)),
      depth_in = pt_scan_depth_in(depth_cm),
      sensor_id = as.character(sensor.id)
    ) |>
    dplyr::filter(!is.na(date), !is.na(sms_pct), !is.na(depth_in))
}

sms_raw <- dplyr::bind_rows(history_pieces)

if (nrow(sms_raw) == 0) {
  stop("No SCAN SMS history rows were fetched or read from cache.")
}


# ==== 8. Normalize to one daily value per station/depth/date =================
##
## Most stations should have one sensor per standard depth. If duplicate sensors
## occur at the same depth on the same day, keep one explicit daily mean and
## record the number/list of sensors that contributed. This avoids silently
## pretending there was only one sensor while still giving BRIM one clean trace
## per depth.

sms_daily <- sms_raw |>
  dplyr::mutate(
    water_year = pt_scan_water_year(date),
    water_day = pt_scan_water_day(date),
    depth_in = as.numeric(depth_in)
  ) |>
  dplyr::group_by(station_uid, station_name, site_code, date, water_year, water_day, depth_in) |>
  dplyr::summarise(
    sms_pct = mean(sms_pct, na.rm = TRUE),
    sensor_count = dplyr::n_distinct(sensor_id),
    sensor_ids = paste(sort(unique(sensor_id)), collapse = ";"),
    .groups = "drop"
  ) |>
  dplyr::arrange(site_code, depth_in, date)

saveRDS(sms_daily, OUT_DAILY_HISTORY_RDS)
message("Wrote SCAN daily SMS history RDS: ", OUT_DAILY_HISTORY_RDS)


# ==== 9. Build water-day percentile context ==================================

percentile_base <- sms_daily

if (EXCLUDE_CURRENT_WY_FROM_PERCENTILES) {
  percentile_base <- percentile_base |>
    dplyr::filter(water_year < current_wy)
}

sms_percentiles <- percentile_base |>
  dplyr::group_by(station_uid, station_name, site_code, depth_in, water_day) |>
  dplyr::summarise(
    p00 = as.numeric(stats::quantile(sms_pct, probs = 0.00, na.rm = TRUE, names = FALSE, type = 7)),
    p10 = as.numeric(stats::quantile(sms_pct, probs = 0.10, na.rm = TRUE, names = FALSE, type = 7)),
    p30 = as.numeric(stats::quantile(sms_pct, probs = 0.30, na.rm = TRUE, names = FALSE, type = 7)),
    p50 = as.numeric(stats::quantile(sms_pct, probs = 0.50, na.rm = TRUE, names = FALSE, type = 7)),
    p70 = as.numeric(stats::quantile(sms_pct, probs = 0.70, na.rm = TRUE, names = FALSE, type = 7)),
    p90 = as.numeric(stats::quantile(sms_pct, probs = 0.90, na.rm = TRUE, names = FALSE, type = 7)),
    p100 = as.numeric(stats::quantile(sms_pct, probs = 1.00, na.rm = TRUE, names = FALSE, type = 7)),
    n_obs = dplyr::n(),
    n_years = dplyr::n_distinct(water_year),
    years_min = min(water_year, na.rm = TRUE),
    years_max = max(water_year, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    climatology_ok = n_years >= MIN_YEARS_FOR_CONTEXT,
    min_years_for_context = MIN_YEARS_FOR_CONTEXT,
    current_water_year_excluded = EXCLUDE_CURRENT_WY_FROM_PERCENTILES,
    build_time_utc = feed_build_time_utc
  ) |>
  dplyr::arrange(site_code, depth_in, water_day)

saveRDS(sms_percentiles, OUT_PERCENTILES_RDS)
readr::write_csv(sms_percentiles, OUT_PERCENTILES_CSV, na = "")

message("Wrote SCAN water-day percentile RDS: ", OUT_PERCENTILES_RDS)
message("Wrote SCAN water-day percentile CSV for live feed: ", OUT_PERCENTILES_CSV)


# ==== 10. QA summaries ========================================================

station_summary <- sms_daily |>
  dplyr::group_by(station_uid, station_name, site_code) |>
  dplyr::summarise(
    first_date = min(date, na.rm = TRUE),
    last_date = max(date, na.rm = TRUE),
    n_daily_rows = dplyr::n(),
    n_depths = dplyr::n_distinct(depth_in),
    depths_in = paste(sort(unique(depth_in)), collapse = ", "),
    max_sensor_count_same_depth_day = max(sensor_count, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(site_code)

depth_summary <- sms_daily |>
  dplyr::group_by(depth_in) |>
  dplyr::summarise(
    n_stations = dplyr::n_distinct(site_code),
    first_date = min(date, na.rm = TRUE),
    last_date = max(date, na.rm = TRUE),
    n_daily_rows = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(depth_in)

run_summary <- tibble::tibble(
  metric = c(
    "build_time_utc",
    "station_index_rows",
    "stations_with_history",
    "daily_history_rows",
    "percentile_rows",
    "current_water_year",
    "current_water_year_excluded_from_percentiles",
    "min_years_for_context",
    "stations_with_duplicate_same_depth_sensors",
    "output_percentiles_csv"
  ),
  value = as.character(c(
    feed_build_time_utc,
    nrow(scan_index),
    dplyr::n_distinct(sms_daily$site_code),
    nrow(sms_daily),
    nrow(sms_percentiles),
    current_wy,
    EXCLUDE_CURRENT_WY_FROM_PERCENTILES,
    MIN_YEARS_FOR_CONTEXT,
    sum(station_summary$max_sensor_count_same_depth_day > 1, na.rm = TRUE),
    OUT_PERCENTILES_CSV
  ))
)

readr::write_csv(run_summary, OUT_RUN_SUMMARY_CSV, na = "")
readr::write_csv(station_summary, OUT_STATION_SUMMARY_CSV, na = "")
readr::write_csv(depth_summary, OUT_DEPTH_SUMMARY_CSV, na = "")

message("Wrote SCAN SMS history QA summaries:")
message("  ", OUT_RUN_SUMMARY_CSV)
message("  ", OUT_STATION_SUMMARY_CSV)
message("  ", OUT_DEPTH_SUMMARY_CSV)

message("\nSCAN SMS historical context summary:")
print(run_summary)
message("\nDepth summary:")
print(depth_summary)
