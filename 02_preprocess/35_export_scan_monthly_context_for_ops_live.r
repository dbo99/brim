# ==== 35_export_scan_monthly_context_for_ops_live.r ==========================
##
## PURPOSE:
##   Export the compact monthly SCAN soil-moisture context table needed by the
##   BRIM Ops Live SCAN popup plots.
##
## WHY THIS EXISTS:
##   The SCAN popup sandbox can read the large local file:
##
##     04_processed_data/rds/scan_sms_daily_history.rds
##
##   The real BRIM HTML map should NOT embed that local RDS, and browser-side
##   Ops Live layers can only fetch web-readable files from the live-data repo.
##   This script converts the local daily-history RDS into a compact CSV that
##   can be committed to:
##
##     brim-live-data-feeds/data/input/scan_sms_monthly_context.csv
##
##   The future SCAN Ops Live helper will use that CSV for Plot A:
##
##     A. Monthly context — recent/available monthly medians
##
##   Daily Plot B uses the existing files:
##
##     scan_soil_moisture_current_wy_trace.csv
##     scan_sms_waterday_percentiles.csv
##
## WHEN TO RUN:
##   Run this after `34_update_scan_soil_moisture_climatology.r`, especially
##   after each water-year rollover / annual climatology update.
##
## NORMAL WORKFLOW:
##   source("02_preprocess/34_update_scan_soil_moisture_climatology.r")
##   source("02_preprocess/35_export_scan_monthly_context_for_ops_live.r")
##
##   Then commit the compact context inputs to the live-feed repo:
##
##   cd brim-live-data-feeds
##   git add data/input/scan_sms_waterday_percentiles.csv
##   git add data/input/scan_sms_monthly_context.csv
##   git commit -m "Update SCAN soil moisture plot context"
##   git push
##
## DESIGN:
##   - Keeps full daily history local as RDS.
##   - Exports only compact monthly values needed for the popup plot.
##   - Uses Los Angeles date logic for the current water-year exclusion message.
##   - Excludes the current water year from the historical monthly reference
##     stats, matching the daily-ribbon/climatology approach.
##   - Shows actual monthly medians for the most recent/available record even
##     when a station is too new for stable monthly reference statistics.
##
## INPUT:
##   04_processed_data/rds/scan_sms_daily_history.rds
##
## OUTPUT:
##   brim-live-data-feeds/data/input/scan_sms_monthly_context.csv
##
## QA:
##   04_processed_data/qa/scan_sms_monthly_context_summary.csv
## ============================================================================


# ==== 1. Load configuration ==================================================

source("00_config/config_paths.r")


# ==== 2. Packages ============================================================

required_pkgs <- c(
  "dplyr",
  "tidyr",
  "readr",
  "tibble",
  "lubridate"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them before exporting SCAN monthly context."
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(lubridate)
})


# ==== 3. User-facing switches ================================================

DISPLAY_TZ <- "America/Los_Angeles"

## Plot A shows the recent/available record, not a full period-of-record
## spaghetti plot.  Ten years matches the sandbox design.
CONTEXT_YEARS_BACK <- suppressWarnings(as.integer(Sys.getenv("SCAN_CONTEXT_YEARS_BACK", unset = "10")))
if (is.na(CONTEXT_YEARS_BACK) || CONTEXT_YEARS_BACK < 3) CONTEXT_YEARS_BACK <- 10L

## Historical monthly reference stats are included only where enough years
## support them.  Actual monthly values are still exported for all stations.
## RF065b: use the same 7-year mature-reference threshold as Plot B. This keeps
## SCAN popup logic simpler: >=7 years = reference ribbons; otherwise show
## observations and/or usable prior-WY traces with clear caveats.
CONTEXT_STAT_MIN_YEARS <- suppressWarnings(as.integer(Sys.getenv("SCAN_CONTEXT_STAT_MIN_YEARS", unset = "7")))
if (is.na(CONTEXT_STAT_MIN_YEARS) || CONTEXT_STAT_MIN_YEARS < 5) CONTEXT_STAT_MIN_YEARS <- 7L

## Require a few daily observations before treating a calendar-month median as
## meaningful.  This keeps isolated one-day fragments from drawing misleading
## monthly points.
MIN_DAYS_PER_MONTH <- suppressWarnings(as.integer(Sys.getenv("SCAN_CONTEXT_MIN_DAYS_PER_MONTH", unset = "5")))
if (is.na(MIN_DAYS_PER_MONTH) || MIN_DAYS_PER_MONTH < 1) MIN_DAYS_PER_MONTH <- 5L


# ==== 4. Paths ================================================================

IN_DAILY_HISTORY_RDS <- file.path(DIR$rds, "scan_sms_daily_history.rds")

OUT_MONTHLY_CONTEXT_CSV <- file.path(
  "brim-live-data-feeds", "data", "input", "scan_sms_monthly_context.csv"
)

OUT_MONTHLY_CONTEXT_QA <- file.path(
  DIR$qa,
  "scan_sms_monthly_context_summary.csv"
)

dir.create(dirname(OUT_MONTHLY_CONTEXT_CSV), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(OUT_MONTHLY_CONTEXT_QA),  recursive = TRUE, showWarnings = FALSE)


# ==== 5. Helpers ==============================================================

pt_scan_current_water_year <- function(date = Sys.Date()) {
  date <- as.Date(date)
  yr <- lubridate::year(date)
  ifelse(lubridate::month(date) >= 10, yr + 1L, yr)
}

pt_scan_mon_year <- function(x) {
  x <- as.Date(x)
  ifelse(is.na(x), NA_character_, format(x, "%b %Y"))
}


# ==== 6. Read daily history ==================================================

if (!file.exists(IN_DAILY_HISTORY_RDS)) {
  stop(
    "Missing SCAN daily-history RDS: ", IN_DAILY_HISTORY_RDS, "\n",
    "Run 02_preprocess/34_update_scan_soil_moisture_climatology.r first."
  )
}

message("Reading SCAN daily-history RDS: ", IN_DAILY_HISTORY_RDS)

daily_history <- readRDS(IN_DAILY_HISTORY_RDS) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    site_code = suppressWarnings(as.integer(.data$site_code)),
    depth_in = suppressWarnings(as.numeric(.data$depth_in)),
    water_year = suppressWarnings(as.integer(.data$water_year)),
    date = as.Date(.data$date),
    sms_pct = suppressWarnings(as.numeric(.data$sms_pct))
  ) |>
  dplyr::filter(
    !is.na(.data$site_code),
    !is.na(.data$depth_in),
    !is.na(.data$date),
    !is.na(.data$sms_pct)
  )

if (nrow(daily_history) == 0) {
  stop("SCAN daily-history RDS contains no usable SMS rows.")
}

local_today <- as.Date(format(Sys.time(), tz = DISPLAY_TZ, usetz = FALSE))
current_wy <- pt_scan_current_water_year(local_today)

message("Display timezone: ", DISPLAY_TZ)
message("Local date: ", local_today)
message("Current water year excluded from monthly reference stats: WY", current_wy)
message("Recent monthly display window: last ", CONTEXT_YEARS_BACK, " years where available")
message("Monthly reference stats require at least ", CONTEXT_STAT_MIN_YEARS, " water years")


# ==== 7. Build station/depth record metadata ==================================

record_meta <- daily_history |>
  dplyr::group_by(.data$site_code, .data$depth_in) |>
  dplyr::summarise(
    record_start_date = min(.data$date, na.rm = TRUE),
    record_end_date   = max(.data$date, na.rm = TRUE),
    record_start_wy   = min(.data$water_year, na.rm = TRUE),
    record_end_wy     = max(.data$water_year, na.rm = TRUE),
    n_daily_rows      = dplyr::n(),
    n_water_years     = dplyr::n_distinct(.data$water_year),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    record_label = paste0(pt_scan_mon_year(.data$record_start_date), "–", pt_scan_mon_year(.data$record_end_date))
  )


# ==== 8. Build actual recent monthly medians =================================

actual_monthly <- daily_history |>
  dplyr::group_by(.data$site_code, .data$depth_in) |>
  dplyr::group_modify(function(x, key) {

    max_date <- max(x$date, na.rm = TRUE)
    first_date <- min(x$date, na.rm = TRUE)
    start_date <- as.Date(max_date %m-% lubridate::years(CONTEXT_YEARS_BACK))
    actual_start_date <- max(start_date, first_date, na.rm = TRUE)

    x |>
      dplyr::filter(.data$date >= actual_start_date) |>
      dplyr::mutate(
        month_date = as.Date(format(.data$date, "%Y-%m-01")),
        calendar_month = lubridate::month(.data$date)
      ) |>
      dplyr::group_by(.data$month_date, .data$calendar_month) |>
      dplyr::summarise(
        actual_sms_pct = stats::median(.data$sms_pct, na.rm = TRUE),
        actual_n_days = dplyr::n(),
        actual_start_date = actual_start_date,
        actual_end_date = max_date,
        .groups = "drop"
      ) |>
      dplyr::filter(
        .data$actual_n_days >= MIN_DAYS_PER_MONTH,
        !is.na(.data$actual_sms_pct)
      )
  }) |>
  dplyr::ungroup()


# ==== 9. Build historical monthly reference statistics ========================

hist_ref_source <- daily_history |>
  dplyr::filter(.data$water_year < current_wy) |>
  dplyr::mutate(calendar_month = lubridate::month(.data$date))

ref_meta <- hist_ref_source |>
  dplyr::group_by(.data$site_code, .data$depth_in) |>
  dplyr::summarise(
    ref_start_date = min(.data$date, na.rm = TRUE),
    ref_end_date   = max(.data$date, na.rm = TRUE),
    ref_start_wy   = min(.data$water_year, na.rm = TRUE),
    ref_end_wy     = max(.data$water_year, na.rm = TRUE),
    ref_n_water_years = dplyr::n_distinct(.data$water_year),
    .groups = "drop"
  )

monthly_ref <- hist_ref_source |>
  dplyr::group_by(.data$site_code, .data$depth_in, .data$calendar_month) |>
  dplyr::summarise(
    ref_p30 = as.numeric(stats::quantile(.data$sms_pct, 0.30, na.rm = TRUE, names = FALSE)),
    ref_p50 = stats::median(.data$sms_pct, na.rm = TRUE),
    ref_p70 = as.numeric(stats::quantile(.data$sms_pct, 0.70, na.rm = TRUE, names = FALSE)),
    ref_n_years = dplyr::n_distinct(.data$water_year[!is.na(.data$sms_pct)]),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    monthly_ref_ok = !is.na(.data$ref_p50) & .data$ref_n_years >= CONTEXT_STAT_MIN_YEARS
  )


# ==== 10. Join compact Plot-A context output =================================

monthly_context <- actual_monthly |>
  dplyr::left_join(
    monthly_ref,
    by = c("site_code", "depth_in", "calendar_month")
  ) |>
  dplyr::left_join(
    record_meta,
    by = c("site_code", "depth_in")
  ) |>
  dplyr::left_join(
    ref_meta,
    by = c("site_code", "depth_in")
  ) |>
  dplyr::mutate(
    context_years_back = CONTEXT_YEARS_BACK,
    context_stat_min_years = CONTEXT_STAT_MIN_YEARS,
    min_days_per_month = MIN_DAYS_PER_MONTH,
    current_wy_excluded = current_wy,
    display_timezone = DISPLAY_TZ,
    monthly_ref_ok = dplyr::coalesce(.data$monthly_ref_ok, FALSE),
    ref_label = dplyr::if_else(
      !is.na(.data$ref_start_date) & !is.na(.data$ref_end_date),
      paste0(pt_scan_mon_year(.data$ref_start_date), "–", pt_scan_mon_year(.data$ref_end_date)),
      NA_character_
    ),
    ref_wy_label = dplyr::if_else(
      !is.na(.data$ref_start_wy) & !is.na(.data$ref_end_wy),
      paste0("WY", .data$ref_start_wy, "–WY", .data$ref_end_wy),
      NA_character_
    )
  ) |>
  dplyr::select(
    .data$site_code,
    .data$depth_in,
    .data$month_date,
    .data$calendar_month,
    .data$actual_sms_pct,
    .data$actual_n_days,
    .data$actual_start_date,
    .data$actual_end_date,
    .data$record_start_date,
    .data$record_end_date,
    .data$record_start_wy,
    .data$record_end_wy,
    .data$record_label,
    .data$ref_p30,
    .data$ref_p50,
    .data$ref_p70,
    .data$ref_n_years,
    .data$monthly_ref_ok,
    .data$ref_start_date,
    .data$ref_end_date,
    .data$ref_start_wy,
    .data$ref_end_wy,
    .data$ref_label,
    .data$ref_wy_label,
    .data$current_wy_excluded,
    .data$context_years_back,
    .data$context_stat_min_years,
    .data$min_days_per_month,
    .data$display_timezone
  ) |>
  dplyr::arrange(.data$site_code, .data$depth_in, .data$month_date)


# ==== 11. Write output and QA =================================================

readr::write_csv(monthly_context, OUT_MONTHLY_CONTEXT_CSV)

summary_tbl <- monthly_context |>
  dplyr::group_by(.data$depth_in) |>
  dplyr::summarise(
    station_depths = dplyr::n_distinct(paste(.data$site_code, .data$depth_in)),
    rows = dplyr::n(),
    rows_with_monthly_ref = sum(.data$monthly_ref_ok, na.rm = TRUE),
    min_record_start = min(.data$record_start_date, na.rm = TRUE),
    max_record_end = max(.data$record_end_date, na.rm = TRUE),
    min_ref_years = suppressWarnings(min(.data$ref_n_years, na.rm = TRUE)),
    max_ref_years = suppressWarnings(max(.data$ref_n_years, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    current_wy_excluded = current_wy,
    context_years_back = CONTEXT_YEARS_BACK,
    context_stat_min_years = CONTEXT_STAT_MIN_YEARS,
    monthly_context_csv = OUT_MONTHLY_CONTEXT_CSV
  )

readr::write_csv(summary_tbl, OUT_MONTHLY_CONTEXT_QA)

message("Wrote SCAN monthly context CSV: ", OUT_MONTHLY_CONTEXT_CSV)
message("Wrote SCAN monthly context QA:  ", OUT_MONTHLY_CONTEXT_QA)

message("\nSCAN monthly context summary:")
print(summary_tbl, n = Inf)

message("\nNext:")
message("  1. Inspect ", OUT_MONTHLY_CONTEXT_CSV)
message("  2. Commit it to the live-feed repo when it looks good:")
message("       cd brim-live-data-feeds")
message("       git add data/input/scan_sms_monthly_context.csv")
message("       git commit -m \"Add SCAN monthly plot context\"")
message("       git push")

invisible(monthly_context)
