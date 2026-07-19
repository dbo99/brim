# ==== 38_export_scan_prior_wy_fallback_for_ops_live.r ========================
##
## RF066:
##   - Tighten Plot B daily-ribbon eligibility from a very permissive
##     30 supported water-days to 200 supported water-days, so short/sparse
##     station-depth records use usable prior-WY fallback traces instead of
##     isolated partial-year ribbons.
##
## RF063c:
##   Harden station-name / station-UID joins so the fallback exporter works
##   whether the daily-history RDS already contains station metadata or not.
##
## RF063b:
##   Export compact usable prior-water-year SCAN soil-moisture traces for
##   Plot B fallback displays. Adds fragment filtering so tiny partial-year
##   tails are not drawn or labeled as useful context.
##
## PURPOSE:
##   Some SCAN station/depth combinations are too new or too sparse for stable
##   daily percentile ribbons, but still have enough recent history to show a
##   useful visual comparison. This script exports up to the last 7 usable
##   completed water-year traces for only those station/depth combinations where
##   the daily percentile product does not meet the Plot B ribbon threshold.
##
## WHY THIS EXISTS:
##   The full SCAN daily-history RDS stays local. The browser-side BRIM map
##   should only fetch compact CSVs from the live-data repo. This file produces:
##
##     brim-live-data-feeds/data/input/scan_sms_prior_wy_fallback_traces.csv
##
##   which is then copied to:
##
##     brim-live-data-feeds/docs/data/scan_sms_prior_wy_fallback_traces.csv
##
##   by 36_publish_scan_context_to_docs_data.r.
##
## WHEN TO RUN:
##   Run after 34_build_scan_history_summary.r / 34_update_scan_soil_moisture_
##   climatology.r, especially after water-year rollover or after changing
##   Plot B ribbon thresholds.
##
## INPUTS:
##   04_processed_data/rds/scan_sms_daily_history.rds
##   brim-live-data-feeds/data/input/scan_sms_waterday_percentiles.csv
##   brim-live-data-feeds/data/input/scan_station_index.csv  [optional names]
##
## OUTPUT:
##   brim-live-data-feeds/data/input/scan_sms_prior_wy_fallback_traces.csv
##
## QA:
##   04_processed_data/qa/scan_sms_prior_wy_fallback_traces_summary.csv
## ============================================================================

# ==== 1. Load configuration ==================================================

source("00_config/config_paths.r")


# ==== 2. Packages ============================================================

required_pkgs <- c("dplyr", "readr", "tibble", "lubridate")

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them before exporting SCAN prior-WY fallback traces."
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(lubridate)
})


# ==== 3. User-facing switches ================================================

DISPLAY_TZ <- "America/Los_Angeles"

## Must match the browser Plot B daily-ribbon logic unless intentionally changed.
## RF065b: 7 years is the SCAN mature-reference threshold.  Below this, Plot B
## uses compact usable prior-WY traces where available rather than implying a
## robust daily percentile envelope.
DAILY_RIBBON_MIN_YEARS <- suppressWarnings(as.integer(Sys.getenv(
  "SCAN_DAILY_RIBBON_MIN_YEARS",
  unset = "7"
)))
if (is.na(DAILY_RIBBON_MIN_YEARS) || DAILY_RIBBON_MIN_YEARS < 3) DAILY_RIBBON_MIN_YEARS <- 7L

## The browser treats a station/depth as having Plot B daily-ribbon context only
## when a substantial part of the water year has at least DAILY_RIBBON_MIN_YEARS
## behind it.  RF066: 200 supported water-days is intentionally less than a
## perfect year, but high enough to prevent odd isolated monthly/late-season
## percentile ribbons from replacing the clearer prior-WY fallback traces.
DAILY_RIBBON_MIN_DAYS <- suppressWarnings(as.integer(Sys.getenv(
  "SCAN_DAILY_RIBBON_MIN_DAYS",
  unset = "200"
)))
if (is.na(DAILY_RIBBON_MIN_DAYS) || DAILY_RIBBON_MIN_DAYS < 1) DAILY_RIBBON_MIN_DAYS <- 200L

## Prior-WY fallback traces: keep this small and readable in the popup.
PRIOR_WY_TRACE_COUNT <- suppressWarnings(as.integer(Sys.getenv(
  "SCAN_PRIOR_WY_FALLBACK_TRACE_COUNT",
  unset = "7"
)))
if (is.na(PRIOR_WY_TRACE_COUNT) || PRIOR_WY_TRACE_COUNT < 1) PRIOR_WY_TRACE_COUNT <- 7L

## Avoid drawing essentially-empty prior-WY traces.  These thresholds are
## intentionally moderate for SCAN: partial records can be useful, but tiny
## installation/removal tails should not be treated as context traces.
MIN_DAILY_ROWS_PER_PRIOR_WY <- suppressWarnings(as.integer(Sys.getenv(
  "SCAN_PRIOR_WY_MIN_DAILY_ROWS",
  unset = "45"
)))
if (is.na(MIN_DAILY_ROWS_PER_PRIOR_WY) || MIN_DAILY_ROWS_PER_PRIOR_WY < 1) {
  MIN_DAILY_ROWS_PER_PRIOR_WY <- 45L
}

MIN_WATERDAY_SPAN_PER_PRIOR_WY <- suppressWarnings(as.integer(Sys.getenv(
  "SCAN_PRIOR_WY_MIN_WATERDAY_SPAN",
  unset = "75"
)))
if (is.na(MIN_WATERDAY_SPAN_PER_PRIOR_WY) || MIN_WATERDAY_SPAN_PER_PRIOR_WY < 1) {
  MIN_WATERDAY_SPAN_PER_PRIOR_WY <- 75L
}


# ==== 4. Paths ================================================================

IN_DAILY_HISTORY_RDS <- file.path(DIR$rds, "scan_sms_daily_history.rds")

IN_PERCENTILES_CSV <- file.path(
  "brim-live-data-feeds", "data", "input", "scan_sms_waterday_percentiles.csv"
)

IN_STATION_INDEX_CSV <- file.path(
  "brim-live-data-feeds", "data", "input", "scan_station_index.csv"
)

OUT_FALLBACK_CSV <- file.path(
  "brim-live-data-feeds", "data", "input", "scan_sms_prior_wy_fallback_traces.csv"
)

OUT_FALLBACK_QA <- file.path(
  DIR$qa,
  "scan_sms_prior_wy_fallback_traces_summary.csv"
)

dir.create(dirname(OUT_FALLBACK_CSV), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(OUT_FALLBACK_QA), recursive = TRUE, showWarnings = FALSE)


# ==== 5. Helpers ==============================================================

pt_scan_current_water_year <- function(date = Sys.Date()) {
  date <- as.Date(date)
  yr <- lubridate::year(date)
  ifelse(lubridate::month(date) >= 10, yr + 1L, yr)
}

pt_scan_empty_fallback <- function() {
  tibble::tibble(
    station_uid = character(),
    station_name = character(),
    site_code = integer(),
    depth_in = numeric(),
    water_year = integer(),
    water_day = integer(),
    obs_date = character(),
    sms_pct = numeric(),
    trace_order = integer(),
    trace_label = character(),
    n_days_in_trace = integer(),
    water_day_min = integer(),
    water_day_max = integer(),
    water_day_span = integer(),
    min_daily_rows_per_prior_wy = integer(),
    min_waterday_span_per_prior_wy = integer(),
    daily_ribbon_min_years = integer(),
    daily_ribbon_min_days = integer(),
    fallback_reason = character()
  )
}


# ==== 6. Read inputs ==========================================================

if (!file.exists(IN_DAILY_HISTORY_RDS)) {
  stop("Missing SCAN daily-history RDS: ", IN_DAILY_HISTORY_RDS)
}

if (!file.exists(IN_PERCENTILES_CSV)) {
  stop("Missing SCAN daily percentile CSV: ", IN_PERCENTILES_CSV)
}

message("Reading SCAN daily-history RDS: ", IN_DAILY_HISTORY_RDS)
daily_history <- readRDS(IN_DAILY_HISTORY_RDS) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    site_code = suppressWarnings(as.integer(.data$site_code)),
    depth_in = suppressWarnings(as.numeric(.data$depth_in)),
    water_year = suppressWarnings(as.integer(.data$water_year)),
    water_day = suppressWarnings(as.integer(.data$water_day)),
    date = as.Date(.data$date),
    sms_pct = suppressWarnings(as.numeric(.data$sms_pct))
  ) |>
  dplyr::filter(
    !is.na(.data$site_code),
    !is.na(.data$depth_in),
    !is.na(.data$water_year),
    !is.na(.data$water_day),
    !is.na(.data$date),
    !is.na(.data$sms_pct)
  )

if (nrow(daily_history) == 0) {
  stop("SCAN daily-history RDS contains no usable rows.")
}

message("Reading SCAN daily percentile context: ", IN_PERCENTILES_CSV)
percentiles <- readr::read_csv(IN_PERCENTILES_CSV, show_col_types = FALSE) |>
  dplyr::mutate(
    site_code = suppressWarnings(as.integer(.data$site_code)),
    depth_in = suppressWarnings(as.numeric(.data$depth_in)),
    water_day = suppressWarnings(as.integer(.data$water_day)),
    n_years = suppressWarnings(as.integer(.data$n_years))
  ) |>
  dplyr::filter(
    !is.na(.data$site_code),
    !is.na(.data$depth_in),
    !is.na(.data$water_day)
  )

station_index <- tibble::tibble(
  site_code = integer(),
  station_uid_lookup = character(),
  station_name_lookup = character()
)

if (file.exists(IN_STATION_INDEX_CSV)) {
  station_index_raw <- readr::read_csv(IN_STATION_INDEX_CSV, show_col_types = FALSE)

  if (!"station_uid" %in% names(station_index_raw)) {
    station_index_raw$station_uid <- NA_character_
  }
  if (!"station_name" %in% names(station_index_raw)) {
    station_index_raw$station_name <- NA_character_
  }

  station_index <- station_index_raw |>
    dplyr::transmute(
      site_code = suppressWarnings(as.integer(.data$site_code)),
      station_uid_lookup = as.character(.data$station_uid),
      station_name_lookup = as.character(.data$station_name)
    ) |>
    dplyr::filter(!is.na(.data$site_code)) |>
    dplyr::distinct(.data$site_code, .keep_all = TRUE)
}


# ==== 7. Identify station/depths needing fallback ============================

local_today <- as.Date(format(Sys.time(), tz = DISPLAY_TZ))
current_wy <- pt_scan_current_water_year(local_today)
completed_wy <- current_wy - 1L

message("Display timezone: ", DISPLAY_TZ)
message("Local date: ", local_today)
message("Current water year excluded from fallback traces: WY", current_wy)
message("Prior-WY fallback traces: up to ", PRIOR_WY_TRACE_COUNT, " usable completed water years")
message("Daily ribbon threshold: ≥", DAILY_RIBBON_MIN_YEARS, " years on at least ", DAILY_RIBBON_MIN_DAYS, " supported water-days")
message("Fallback trace usability threshold: ≥", MIN_DAILY_ROWS_PER_PRIOR_WY, " daily observations and ≥", MIN_WATERDAY_SPAN_PER_PRIOR_WY, " water-day span")

all_station_depths <- daily_history |>
  dplyr::distinct(.data$site_code, .data$depth_in)

ribbon_coverage <- percentiles |>
  dplyr::group_by(.data$site_code, .data$depth_in) |>
  dplyr::summarise(
    ribbon_days_ok = sum(!is.na(.data$n_years) & .data$n_years >= DAILY_RIBBON_MIN_YEARS),
    max_n_years = suppressWarnings(max(.data$n_years, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    ribbon_days_ok = dplyr::coalesce(.data$ribbon_days_ok, 0L),
    max_n_years = dplyr::if_else(is.infinite(.data$max_n_years), NA_integer_, as.integer(.data$max_n_years)),
    daily_ribbon_ok = .data$ribbon_days_ok >= DAILY_RIBBON_MIN_DAYS
  )

fallback_combos <- all_station_depths |>
  dplyr::left_join(ribbon_coverage, by = c("site_code", "depth_in")) |>
  dplyr::mutate(
    ribbon_days_ok = dplyr::coalesce(.data$ribbon_days_ok, 0L),
    daily_ribbon_ok = dplyr::coalesce(.data$daily_ribbon_ok, FALSE),
    fallback_reason = dplyr::case_when(
      .data$daily_ribbon_ok ~ "daily percentile ribbons available",
      .data$ribbon_days_ok == 0 ~ paste0("no water-days meet ≥", DAILY_RIBBON_MIN_YEARS, "-year daily-ribbon threshold"),
      TRUE ~ paste0("only ", .data$ribbon_days_ok, " water-days meet ≥", DAILY_RIBBON_MIN_YEARS, "-year daily-ribbon threshold; need ≥", DAILY_RIBBON_MIN_DAYS)
    )
  ) |>
  dplyr::filter(!.data$daily_ribbon_ok) |>
  dplyr::select(.data$site_code, .data$depth_in, .data$ribbon_days_ok, .data$max_n_years, .data$fallback_reason)

message("Station/depth combinations total: ", nrow(all_station_depths))
message("Station/depth combinations needing prior-WY fallback traces: ", nrow(fallback_combos))


# ==== 8. Export compact fallback traces ======================================

if (nrow(fallback_combos) == 0) {

  fallback_out <- pt_scan_empty_fallback()

} else {

  prior_year_counts <- daily_history |>
    dplyr::semi_join(fallback_combos, by = c("site_code", "depth_in")) |>
    dplyr::filter(.data$water_year < current_wy) |>
    dplyr::group_by(.data$site_code, .data$depth_in, .data$water_year) |>
    dplyr::summarise(
      n_days_in_trace = dplyr::n_distinct(.data$date),
      water_day_min = min(.data$water_day, na.rm = TRUE),
      water_day_max = max(.data$water_day, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      water_day_span = .data$water_day_max - .data$water_day_min + 1L
    ) |>
    dplyr::filter(
      .data$n_days_in_trace >= MIN_DAILY_ROWS_PER_PRIOR_WY,
      .data$water_day_span >= MIN_WATERDAY_SPAN_PER_PRIOR_WY
    ) |>
    dplyr::group_by(.data$site_code, .data$depth_in) |>
    dplyr::arrange(dplyr::desc(.data$water_year), .by_group = TRUE) |>
    dplyr::mutate(trace_order = dplyr::row_number()) |>
    dplyr::filter(.data$trace_order <= PRIOR_WY_TRACE_COUNT) |>
    dplyr::ungroup()

  fallback_out <- daily_history |>
    dplyr::inner_join(
      prior_year_counts,
      by = c("site_code", "depth_in", "water_year")
    ) |>
    dplyr::left_join(
      fallback_combos,
      by = c("site_code", "depth_in")
    ) |>
    dplyr::left_join(station_index, by = "site_code") |>
    dplyr::mutate(
      station_uid_lookup = dplyr::coalesce(
        dplyr::na_if(as.character(.data$station_uid_lookup), ""),
        paste0("NRCS_scan_", .data$site_code)
      ),
      station_name_lookup = dplyr::coalesce(
        dplyr::na_if(as.character(.data$station_name_lookup), ""),
        paste0("SCAN ", .data$site_code)
      )
    ) |>
    dplyr::transmute(
      station_uid = .data$station_uid_lookup,
      station_name = .data$station_name_lookup,
      site_code = as.integer(.data$site_code),
      depth_in = as.numeric(.data$depth_in),
      water_year = as.integer(.data$water_year),
      water_day = as.integer(.data$water_day),
      obs_date = as.character(.data$date),
      sms_pct = round(as.numeric(.data$sms_pct), 2),
      trace_order = as.integer(.data$trace_order),
      trace_label = paste0("WY'", substr(as.character(.data$water_year), 3, 4)),
      n_days_in_trace = as.integer(.data$n_days_in_trace),
      water_day_min = as.integer(.data$water_day_min),
      water_day_max = as.integer(.data$water_day_max),
      water_day_span = as.integer(.data$water_day_span),
      min_daily_rows_per_prior_wy = MIN_DAILY_ROWS_PER_PRIOR_WY,
      min_waterday_span_per_prior_wy = MIN_WATERDAY_SPAN_PER_PRIOR_WY,
      daily_ribbon_min_years = DAILY_RIBBON_MIN_YEARS,
      daily_ribbon_min_days = DAILY_RIBBON_MIN_DAYS,
      fallback_reason = .data$fallback_reason
    ) |>
    dplyr::arrange(.data$site_code, .data$depth_in, .data$trace_order, .data$water_day)
}


# ==== 9. QA and write =========================================================

readr::write_csv(fallback_out, OUT_FALLBACK_CSV, na = "")

qa <- fallback_out |>
  dplyr::group_by(.data$depth_in) |>
  dplyr::summarise(
    station_depths = dplyr::n_distinct(paste(.data$site_code, .data$depth_in)),
    rows = dplyr::n(),
    stations = dplyr::n_distinct(.data$site_code),
    water_year_min = suppressWarnings(min(.data$water_year, na.rm = TRUE)),
    water_year_max = suppressWarnings(max(.data$water_year, na.rm = TRUE)),
    trace_count_max = suppressWarnings(max(.data$trace_order, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    water_year_min = dplyr::if_else(is.infinite(.data$water_year_min), NA_integer_, as.integer(.data$water_year_min)),
    water_year_max = dplyr::if_else(is.infinite(.data$water_year_max), NA_integer_, as.integer(.data$water_year_max)),
    trace_count_max = dplyr::if_else(is.infinite(.data$trace_count_max), NA_integer_, as.integer(.data$trace_count_max)),
    current_wy_excluded = current_wy,
    daily_ribbon_min_years = DAILY_RIBBON_MIN_YEARS,
    daily_ribbon_min_days = DAILY_RIBBON_MIN_DAYS,
    prior_wy_trace_count = PRIOR_WY_TRACE_COUNT,
    min_daily_rows_per_prior_wy = MIN_DAILY_ROWS_PER_PRIOR_WY,
    min_waterday_span_per_prior_wy = MIN_WATERDAY_SPAN_PER_PRIOR_WY,
    fallback_csv = OUT_FALLBACK_CSV
  )

if (nrow(qa) == 0) {
  qa <- tibble::tibble(
    depth_in = numeric(),
    station_depths = integer(),
    rows = integer(),
    stations = integer(),
    water_year_min = integer(),
    water_year_max = integer(),
    trace_count_max = integer(),
    current_wy_excluded = integer(),
    daily_ribbon_min_years = integer(),
    daily_ribbon_min_days = integer(),
    prior_wy_trace_count = integer(),
    min_daily_rows_per_prior_wy = integer(),
    min_waterday_span_per_prior_wy = integer(),
    fallback_csv = character()
  )
}

readr::write_csv(qa, OUT_FALLBACK_QA, na = "")

message("Wrote SCAN usable prior-WY fallback traces CSV: ", OUT_FALLBACK_CSV)
message("Wrote SCAN usable prior-WY fallback traces QA:  ", OUT_FALLBACK_QA)
message("\nSCAN usable prior-WY fallback summary:")
print(qa, n = Inf)

message("\nNext:")
message("  1. Inspect ", OUT_FALLBACK_CSV)
message("  2. Run 02_preprocess/36_publish_scan_context_to_docs_data.r to copy it to docs/data")
message("  3. Upload/replace both data/input and docs/data copies when publishing context updates")
