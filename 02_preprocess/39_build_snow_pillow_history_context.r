# ==== 39_build_snow_pillow_history_context.r ================================
## SWE026:
##   - Add RDS-first derived-product rebuild mode for O&M/cache safety.
##   - In auto mode, reuse the selected daily-history RDS when it covers the
##     needed complete prior water-year window, avoiding accidental 1990-present
##     provider refetches for derived-product changes.
##   - Provider fetch/site-cache mode still runs when explicitly requested, when
##     the RDS is missing/insufficient, or at water-year rollover when the RDS
##     does not cover the just-completed water year.
## SWE025:
##   - Add compact station/day median-normal products for popup text:
##       * fixed WY1991-WY2020 median SWE
##       * rolling 30 complete-WY median SWE
##   - Keep Plot B percentile ribbons and map context bins on the existing
##     station-specific POA percentile products; medians are text context only.
## SWE021:
##   - Use CDEC SNO ADJ / sensor #82 as the preferred CDEC source for
##     historical SWE context products.
##   - Use CDEC SNOW WC / sensor #3 only as a station-level fallback when no
##     valid #82 adjusted SWE exists in the historical fetch window.
##   - Keep NRCS/SNOTEL WTEQ unchanged and keep existing negative/high-SWE QC.
##
## SWE015:
##   - Export compact prior-water-year fallback traces for stations without
##     enough daily history to draw percentile ribbons in Plot B.
##   - Default fallback is the last 7 complete water years per no-ribbon station.
## SWE010:
##   - Add station-specific analysis-period / period-of-analysis labels to
##     daily and monthly context products for clearer popup plot notes.
##   - Labels use month-year style, e.g., Oct 1990–Sep 2025, to parallel
##     SCAN popup wording while keeping SWE-specific context thresholds.
##
## SWE009a
##   - Add monthly reference percentiles for Plot A (30th, 50th, 70th)
##     using valid historical station-month means.
##   - Preserve SWE007a high/negative-value QC and complete station QA rows.
##
## SWE007a
##   - Add high-positive SWE quality-control filtering before historical
##     percentile/monthly products are built.
##   - Keep all station-index rows in station QA, including no-history rows.
##   - Fix duplicated time-zone label in build_time_local.
##   - Default historical window now starts at 1990-10-01 so WY1991 is included.
##
## PURPOSE:
##   Build compact historical SWE context products for the BRIM Ops Live
##   snow-pillow / SWE layer.
##
## DESIGN:
##   - Run locally from the BRIM / PortaTreasure2 project root.
##   - Read the clean live-feed station index exported by:
##       02_preprocess/38_export_snow_pillow_live_inputs.r
##   - Fetch/cache daily SWE history for:
##       * USDA NRCS / SNOTEL via AWDB REST element WTEQ
##       * CA DWR / CDEC snow sensors via sharpshootR::CDECquery(sensor = 3)
##   - Treat negative and implausibly high provider SWE values as invalid/missing.
##   - Write compact CSV products for browser-side popup context:
##       * daily water-day percentile ribbons for Plot B
##       * recent monthly means for Plot A
##   - Write both source/input copies and GitHub-Pages/browser copies.
##
## WHY THIS IS LOCAL, NOT DAILY GITHUB:
##   Historical backfills are heavier and less frequently needed than latest
##   values.  Run this after water-year rollover, station-list changes, or
##   changes to percentile/monthly aggregation rules.
##
## OUTPUTS:
##   04_processed_data/rds/snow_pillow_swe_daily_history.rds
##   04_processed_data/rds/snow_pillow_swe_waterday_percentiles.rds
##   04_processed_data/rds/snow_pillow_swe_monthly_context.rds
##
##   brim-live-data-feeds/data/input/snow_pillow_swe_waterday_percentiles.csv
##   brim-live-data-feeds/data/input/snow_pillow_swe_monthly_context.csv
##   brim-live-data-feeds/data/input/snow_pillow_swe_prior_wy_fallback_traces.csv
##   brim-live-data-feeds/data/input/snow_pillow_swe_normal_medians.csv
##   brim-live-data-feeds/docs/data/snow_pillow_swe_waterday_percentiles.csv
##   brim-live-data-feeds/docs/data/snow_pillow_swe_monthly_context.csv
##   brim-live-data-feeds/docs/data/snow_pillow_swe_prior_wy_fallback_traces.csv
##   brim-live-data-feeds/docs/data/snow_pillow_swe_normal_medians.csv
##
## QA:
##   04_processed_data/qa/snow_pillow_history_context_run_summary.csv
##   04_processed_data/qa/snow_pillow_history_context_station_summary.csv
## ============================================================================

# ==== 1. Project sanity check ===============================================

if (!dir.exists("00_config") || !dir.exists("brim-live-data-feeds")) {
  stop(
    "Run this script from the PortaTreasure2 / BRIM project root.\n",
    "Expected folders 00_config/ and brim-live-data-feeds/ were not found."
  )
}

source("00_config/config_paths.r")

# ==== 2. Packages ============================================================

required_pkgs <- c(
  "dplyr", "tidyr", "purrr", "readr", "tibble", "stringr",
  "lubridate", "jsonlite", "curl", "sharpshootR"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them before running the snow-pillow historical-context builder."
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(stringr)
  library(lubridate)
  library(jsonlite)
  library(curl)
  library(sharpshootR)
})

# ==== 3. Paths and switches ==================================================

LIVE_REPO_DIR <- file.path(DIR$root, "brim-live-data-feeds")
LIVE_INPUT_DIR <- file.path(LIVE_REPO_DIR, "data", "input")
LIVE_DOCS_DATA_DIR <- file.path(LIVE_REPO_DIR, "docs", "data")

STATION_INDEX_CSV <- file.path(LIVE_INPUT_DIR, "snow_pillow_station_index.csv")

DIR$snow_hist_cache <- file.path(DIR$raw, "snow_soil_climate", "cache", "snow_pillow_swe_history")
dir.create(DIR$snow_hist_cache, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR$rds, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR$qa, showWarnings = FALSE, recursive = TRUE)
dir.create(LIVE_INPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(LIVE_DOCS_DATA_DIR, showWarnings = FALSE, recursive = TRUE)

OUT_DAILY_HISTORY_RDS <- file.path(DIR$rds, "snow_pillow_swe_daily_history.rds")
OUT_WATERDAY_PCT_RDS  <- file.path(DIR$rds, "snow_pillow_swe_waterday_percentiles.rds")
OUT_MONTHLY_CTX_RDS   <- file.path(DIR$rds, "snow_pillow_swe_monthly_context.rds")
OUT_PRIOR_WY_FALLBACK_TRACES_RDS <- file.path(DIR$rds, "snow_pillow_swe_prior_wy_fallback_traces.rds")
OUT_NORMAL_MEDIANS_RDS <- file.path(DIR$rds, "snow_pillow_swe_normal_medians.rds")

OUT_WATERDAY_PCT_INPUT_CSV <- file.path(LIVE_INPUT_DIR, "snow_pillow_swe_waterday_percentiles.csv")
OUT_MONTHLY_CTX_INPUT_CSV  <- file.path(LIVE_INPUT_DIR, "snow_pillow_swe_monthly_context.csv")
OUT_PRIOR_WY_FALLBACK_TRACES_INPUT_CSV <- file.path(LIVE_INPUT_DIR, "snow_pillow_swe_prior_wy_fallback_traces.csv")
OUT_NORMAL_MEDIANS_INPUT_CSV <- file.path(LIVE_INPUT_DIR, "snow_pillow_swe_normal_medians.csv")
OUT_WATERDAY_PCT_DOCS_CSV  <- file.path(LIVE_DOCS_DATA_DIR, "snow_pillow_swe_waterday_percentiles.csv")
OUT_MONTHLY_CTX_DOCS_CSV   <- file.path(LIVE_DOCS_DATA_DIR, "snow_pillow_swe_monthly_context.csv")
OUT_PRIOR_WY_FALLBACK_TRACES_DOCS_CSV <- file.path(LIVE_DOCS_DATA_DIR, "snow_pillow_swe_prior_wy_fallback_traces.csv")
OUT_NORMAL_MEDIANS_DOCS_CSV <- file.path(LIVE_DOCS_DATA_DIR, "snow_pillow_swe_normal_medians.csv")

OUT_RUN_QA_CSV     <- file.path(DIR$qa, "snow_pillow_history_context_run_summary.csv")
OUT_STATION_QA_CSV <- file.path(DIR$qa, "snow_pillow_history_context_station_summary.csv")

LOCAL_TZ <- "America/Los_Angeles"
TODAY_LOCAL <- as.Date(lubridate::with_tz(Sys.time(), LOCAL_TZ))
CURRENT_WY <- {
  yr <- as.integer(format(TODAY_LOCAL, "%Y"))
  mo <- as.integer(format(TODAY_LOCAL, "%m"))
  if (mo >= 10L) yr + 1L else yr
}

## Default to a modern 30+ year window that is useful for normals/context while
## avoiding a very large first-run full-POR backfill.  Change via env var if a
## longer or shorter historical window is desired.
DEFAULT_HISTORY_START_DATE <- as.Date("1990-10-01")
HISTORY_START_DATE <- as.Date(Sys.getenv(
  "SNOW_PILLOW_HISTORY_START_DATE",
  unset = as.character(DEFAULT_HISTORY_START_DATE)
))
HISTORY_END_DATE <- as.Date(Sys.getenv(
  "SNOW_PILLOW_HISTORY_END_DATE",
  unset = as.character(TODAY_LOCAL)
))

REFRESH_SITE_CACHE <- tolower(Sys.getenv("SNOW_PILLOW_HISTORY_REFRESH_CACHE", unset = "false")) %in% c("true", "t", "1", "yes", "y")

## SWE026 O&M/cache safety.  Historical context products are often re-derived
## from the already-selected/cleaned daily-history RDS.  In auto mode, reuse the
## RDS when it covers the complete prior water year needed for reference
## statistics; otherwise fall back to provider fetch/site-cache mode.
##
## Values:
##   auto     = reuse RDS when safely sufficient, otherwise fetch/cache
##   rds      = require/use existing RDS; stop if it is missing/insufficient
##   provider = ignore existing RDS and use provider/site-cache fetch path
HISTORY_SOURCE_MODE <- tolower(Sys.getenv("SNOW_PILLOW_HISTORY_SOURCE_MODE", unset = "auto"))
if (!HISTORY_SOURCE_MODE %in% c("auto", "rds", "rds_only", "provider", "fetch")) {
  warning("Unrecognized SNOW_PILLOW_HISTORY_SOURCE_MODE=", HISTORY_SOURCE_MODE, "; using auto.")
  HISTORY_SOURCE_MODE <- "auto"
}
if (HISTORY_SOURCE_MODE == "rds_only") HISTORY_SOURCE_MODE <- "rds"
if (HISTORY_SOURCE_MODE == "fetch") HISTORY_SOURCE_MODE <- "provider"

PRIOR_COMPLETE_WY <- CURRENT_WY - 1L
PRIOR_COMPLETE_WY_END_DATE <- as.Date(sprintf("%d-09-30", PRIOR_COMPLETE_WY))

STATION_LIMIT <- suppressWarnings(as.integer(Sys.getenv("SNOW_PILLOW_HISTORY_STATION_LIMIT", unset = NA_character_)))

REQUEST_PAUSE_SEC <- suppressWarnings(as.numeric(Sys.getenv("SNOW_PILLOW_HISTORY_REQUEST_PAUSE_SEC", unset = "0.08")))
if (is.na(REQUEST_PAUSE_SEC) || REQUEST_PAUSE_SEC < 0) REQUEST_PAUSE_SEC <- 0.08

AWDB_CHUNK_SIZE <- suppressWarnings(as.integer(Sys.getenv("SNOW_PILLOW_HISTORY_AWDB_CHUNK_SIZE", unset = "40")))
if (is.na(AWDB_CHUNK_SIZE) || AWDB_CHUNK_SIZE < 1) AWDB_CHUNK_SIZE <- 40L

## Daily percentile context.  Rows below the threshold are retained but marked
## context_ok = FALSE so the browser can show "No context" rather than hiding
## the station entirely.
MIN_YEARS_FOR_DAILY_CONTEXT <- suppressWarnings(as.integer(Sys.getenv("SNOW_PILLOW_DAILY_CONTEXT_MIN_YEARS", unset = "10")))
if (is.na(MIN_YEARS_FOR_DAILY_CONTEXT) || MIN_YEARS_FOR_DAILY_CONTEXT < 3) MIN_YEARS_FOR_DAILY_CONTEXT <- 10L

## Popup median-normal text products. These are text-only denominators for the
## latest SWE row and do not replace the POA percentile ribbons/context bins.
NORMAL_FIXED_START_WY <- suppressWarnings(as.integer(Sys.getenv("SNOW_PILLOW_NORMAL_FIXED_START_WY", unset = "1991")))
if (is.na(NORMAL_FIXED_START_WY)) NORMAL_FIXED_START_WY <- 1991L

NORMAL_FIXED_END_WY <- suppressWarnings(as.integer(Sys.getenv("SNOW_PILLOW_NORMAL_FIXED_END_WY", unset = "2020")))
if (is.na(NORMAL_FIXED_END_WY) || NORMAL_FIXED_END_WY < NORMAL_FIXED_START_WY) NORMAL_FIXED_END_WY <- 2020L

NORMAL_ROLLING_YEARS <- suppressWarnings(as.integer(Sys.getenv("SNOW_PILLOW_NORMAL_ROLLING_YEARS", unset = "30")))
if (is.na(NORMAL_ROLLING_YEARS) || NORMAL_ROLLING_YEARS < 5) NORMAL_ROLLING_YEARS <- 30L

NORMAL_ROLLING_END_WY <- CURRENT_WY - 1L
NORMAL_ROLLING_START_WY <- NORMAL_ROLLING_END_WY - NORMAL_ROLLING_YEARS + 1L

NORMAL_MIN_YEARS_FOR_POPUP <- suppressWarnings(as.integer(Sys.getenv("SNOW_PILLOW_NORMAL_MIN_YEARS_FOR_POPUP", unset = "10")))
if (is.na(NORMAL_MIN_YEARS_FOR_POPUP) || NORMAL_MIN_YEARS_FOR_POPUP < 3) NORMAL_MIN_YEARS_FOR_POPUP <- 10L

## Plot B fallback for short-history stations. When a station does not have
## enough prior years to draw daily percentile ribbons, the browser can draw a
## small set of prior complete WY traces instead. Keep this compact because it
## is served to the browser.
PRIOR_WY_FALLBACK_YEARS <- suppressWarnings(as.integer(Sys.getenv("SNOW_PILLOW_PRIOR_WY_FALLBACK_YEARS", unset = "7")))
if (is.na(PRIOR_WY_FALLBACK_YEARS) || PRIOR_WY_FALLBACK_YEARS < 1) PRIOR_WY_FALLBACK_YEARS <- 7L

## A prior WY fallback trace should have enough observations and date span to be
## visually meaningful. This prevents very short partial-year fragments (for
## example, a station installed late in a water year) from being plotted and
## labeled as if they were useful prior-WY traces.
PRIOR_WY_FALLBACK_MIN_OBS <- suppressWarnings(as.integer(Sys.getenv("SNOW_PILLOW_PRIOR_WY_FALLBACK_MIN_OBS", unset = "30")))
if (is.na(PRIOR_WY_FALLBACK_MIN_OBS) || PRIOR_WY_FALLBACK_MIN_OBS < 1) PRIOR_WY_FALLBACK_MIN_OBS <- 30L

PRIOR_WY_FALLBACK_MIN_SPAN_DAYS <- suppressWarnings(as.integer(Sys.getenv("SNOW_PILLOW_PRIOR_WY_FALLBACK_MIN_SPAN_DAYS", unset = "60")))
if (is.na(PRIOR_WY_FALLBACK_MIN_SPAN_DAYS) || PRIOR_WY_FALLBACK_MIN_SPAN_DAYS < 1) PRIOR_WY_FALLBACK_MIN_SPAN_DAYS <- 60L


## Plot A monthly means.  A month needs enough daily observations to represent
## that month.  User preference: start with 10 observations/month.
MONTHLY_MIN_DAILY_OBS <- suppressWarnings(as.integer(Sys.getenv("SNOW_PILLOW_MONTHLY_MIN_DAILY_OBS", unset = "10")))
if (is.na(MONTHLY_MIN_DAILY_OBS) || MONTHLY_MIN_DAILY_OBS < 1) MONTHLY_MIN_DAILY_OBS <- 10L

RECENT_MONTHLY_YEARS <- suppressWarnings(as.integer(Sys.getenv("SNOW_PILLOW_MONTHLY_CONTEXT_YEARS", unset = "10")))
if (is.na(RECENT_MONTHLY_YEARS) || RECENT_MONTHLY_YEARS < 3) RECENT_MONTHLY_YEARS <- 10L

## Plot A monthly reference ribbons.  This is intentionally separate from the
## daily context threshold so the monthly overview can be tuned without
## changing Plot B.
MIN_YEARS_FOR_MONTHLY_CONTEXT <- suppressWarnings(as.integer(Sys.getenv("SNOW_PILLOW_MONTHLY_CONTEXT_MIN_YEARS", unset = "10")))
if (is.na(MIN_YEARS_FOR_MONTHLY_CONTEXT) || MIN_YEARS_FOR_MONTHLY_CONTEXT < 3) MIN_YEARS_FOR_MONTHLY_CONTEXT <- 10L

CDEC_RAW_SWE_SENSOR <- 3L
CDEC_REVISED_SWE_SENSOR <- 82L
CDEC_DAILY_INTERVAL <- "D"

## Historical CDEC source rule.  Use CDEC SNO ADJ / sensor #82 where available
## because it is the revised/adjusted SWE product and avoids many raw #3 sensor
## artifacts.  Use raw SNOW WC / sensor #3 only as a station-level fallback when
## no valid #82 adjusted SWE exists in the fetch window.  Do not fill old #82
## gaps day-by-day with #3 in historical context products.
CDEC_HISTORY_82_MIN_VALID_OBS <- suppressWarnings(as.integer(Sys.getenv(
  "SNOW_PILLOW_CDEC_HISTORY_82_MIN_VALID_OBS",
  unset = "1"
)))
if (is.na(CDEC_HISTORY_82_MIN_VALID_OBS) || CDEC_HISTORY_82_MIN_VALID_OBS < 1) {
  CDEC_HISTORY_82_MIN_VALID_OBS <- 1L
}

## Provider feeds occasionally contain corrupted SWE values. Negative values are
## always invalid. Extremely high positive values can also appear in CDEC history
## as bad sensor/encoding artifacts (for example 32767 or much larger). Keep the
## threshold intentionally high so real Sierra snowpack is not clipped, but
## obviously bad values cannot contaminate percentile ribbons or monthly context.
MAX_VALID_SWE_IN <- suppressWarnings(as.numeric(Sys.getenv("SNOW_PILLOW_MAX_VALID_SWE_IN", unset = "250")))
if (is.na(MAX_VALID_SWE_IN) || MAX_VALID_SWE_IN <= 0) MAX_VALID_SWE_IN <- 250


# ==== 4. Small helpers =======================================================

pt_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

pt_swe_num <- function(x) {
  v <- pt_num(x)
  v[!is.na(v) & v < 0] <- NA_real_
  v[!is.na(v) & v > MAX_VALID_SWE_IN] <- NA_real_
  v
}

pt_negative_swe_count <- function(x) {
  v <- pt_num(x)
  sum(!is.na(v) & v < 0, na.rm = TRUE)
}

pt_high_swe_count <- function(x) {
  v <- pt_num(x)
  sum(!is.na(v) & v > MAX_VALID_SWE_IN, na.rm = TRUE)
}

pt_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = LOCAL_TZ))

  x_chr <- pt_chr(x)
  out <- suppressWarnings(as.Date(x_chr))

  if (all(is.na(out))) {
    out <- suppressWarnings(as.Date(lubridate::ymd_hms(x_chr, quiet = TRUE, tz = "UTC"), tz = LOCAL_TZ))
  }

  if (all(is.na(out))) {
    out <- suppressWarnings(as.Date(lubridate::ymd(x_chr, quiet = TRUE)))
  }

  out
}

pt_water_year <- function(date) {
  date <- as.Date(date)
  yr <- as.integer(format(date, "%Y"))
  mo <- as.integer(format(date, "%m"))
  ifelse(mo >= 10L, yr + 1L, yr)
}

pt_water_day <- function(date) {
  date <- as.Date(date)
  wy <- pt_water_year(date)
  wy_start <- as.Date(sprintf("%d-10-01", wy - 1L))
  as.integer(date - wy_start + 1L)
}

pt_water_month <- function(date) {
  mo <- as.integer(format(as.Date(date), "%m"))
  ifelse(mo >= 10L, mo - 9L, mo + 3L)
}

pt_month_label_from_water_month <- function(water_month) {
  labs <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
  labs[as.integer(water_month)]
}

pt_month_start_water_day <- function(water_month) {
  starts <- c(1L, 32L, 62L, 93L, 124L, 152L, 183L, 213L, 244L, 274L, 305L, 336L)
  starts[as.integer(water_month)]
}

pt_swe_month_date <- function(water_year, water_month) {
  water_year <- suppressWarnings(as.integer(water_year))
  water_month <- suppressWarnings(as.integer(water_month))
  cal_month <- c(10L, 11L, 12L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L)[pmax(1L, pmin(12L, water_month))]
  cal_year <- ifelse(water_month <= 3L, water_year - 1L, water_year)
  as.Date(sprintf("%04d-%02d-01", cal_year, cal_month))
}

pt_mon_year_label <- function(date) {
  date <- suppressWarnings(as.Date(date))
  out <- rep(NA_character_, length(date))
  ok <- !is.na(date)
  out[ok] <- format(date[ok], "%b %Y")
  out
}

pt_period_label <- function(start_date, end_date) {
  start_txt <- pt_mon_year_label(start_date)
  end_txt <- pt_mon_year_label(end_date)
  out <- paste0(start_txt, "–", end_txt)
  out[is.na(start_txt) | is.na(end_txt)] <- NA_character_
  out
}

pt_wy_period_label <- function(start_wy, end_wy) {
  start_wy <- suppressWarnings(as.integer(start_wy))
  end_wy <- suppressWarnings(as.integer(end_wy))
  out <- paste0("WY", start_wy, "–WY", end_wy)
  out[is.na(start_wy) | is.na(end_wy)] <- NA_character_
  out
}

pt_safe_file_id <- function(x) {
  x <- pt_chr(x)
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  x <- gsub("_+", "_", x)
  x
}

first_existing_col <- function(x, choices) {
  hit <- choices[choices %in% names(x)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

pt_empty_history <- function() {
  tibble::tibble(
    station_uid = character(),
    live_provider_key = character(),
    provider = character(),
    provider_station_id = character(),
    station_name = character(),
    obs_date = as.Date(character()),
    raw_swe_in = numeric(),
    swe_in = numeric(),
    source_element = character()
  )
}

pt_station_cache_path <- function(station_uid) {
  file.path(
    DIR$snow_hist_cache,
    paste0(
      "snow_pillow_swe_history_",
      pt_safe_file_id(station_uid),
      "_",
      format(HISTORY_START_DATE, "%Y%m%d"),
      "_",
      format(HISTORY_END_DATE, "%Y%m%d"),
      ".rds"
    )
  )
}

pt_station_cache_path_source <- function(station_uid, source_key) {
  file.path(
    DIR$snow_hist_cache,
    paste0(
      "snow_pillow_swe_history_",
      pt_safe_file_id(source_key),
      "_",
      pt_safe_file_id(station_uid),
      "_",
      format(HISTORY_START_DATE, "%Y%m%d"),
      "_",
      format(HISTORY_END_DATE, "%Y%m%d"),
      ".rds"
    )
  )
}

# ---- 4.1 Robust provider fetch helpers -------------------------------------

PT_FETCH_ATTEMPTS <- suppressWarnings(as.integer(Sys.getenv(
  "SNOW_PILLOW_HISTORY_FETCH_ATTEMPTS",
  unset = "3"
)))
if (is.na(PT_FETCH_ATTEMPTS) || PT_FETCH_ATTEMPTS < 1) PT_FETCH_ATTEMPTS <- 3L

PT_FETCH_TIMEOUT_SEC <- suppressWarnings(as.integer(Sys.getenv(
  "SNOW_PILLOW_HISTORY_FETCH_TIMEOUT_SEC",
  unset = "90"
)))
if (is.na(PT_FETCH_TIMEOUT_SEC) || PT_FETCH_TIMEOUT_SEC < 10) PT_FETCH_TIMEOUT_SEC <- 90L

pt_fetch_text <- function(url, label, attempts = PT_FETCH_ATTEMPTS, timeout_sec = PT_FETCH_TIMEOUT_SEC) {

  label <- pt_chr(label)
  if (is.na(label)) label <- "provider request"

  for (attempt in seq_len(attempts)) {

    handle <- curl::new_handle()
    curl::handle_setopt(
      handle,
      useragent = "BRIM-snow-pillow-history/1.0 (+https://github.com/dbo99/brim-live-data-feeds)",
      followlocation = TRUE,
      timeout = timeout_sec,
      connecttimeout = min(30L, timeout_sec)
    )

    resp <- tryCatch(
      curl::curl_fetch_memory(url, handle = handle),
      error = function(e) e
    )

    if (!inherits(resp, "error")) {
      status <- as.integer(resp$status_code)
      txt <- rawToChar(resp$content)

      if (!is.na(status) && status >= 200L && status < 300L && nzchar(txt)) {
        return(txt)
      }

      message(
        label, " returned HTTP ", status,
        " on attempt ", attempt, "/", attempts,
        if (nzchar(txt)) paste0("; first response chars: ", substr(gsub("\\s+", " ", txt), 1, 160)) else ""
      )
    } else {
      message(
        label, " failed on attempt ", attempt, "/", attempts,
        "; error: ", conditionMessage(resp)
      )
    }

    if (attempt < attempts) Sys.sleep(pmin(10, attempt * 2))
  }

  NULL
}

pt_json_from_url <- function(url, label) {

  txt <- pt_fetch_text(url, label = label)

  if (is.null(txt) || !nzchar(txt)) {
    return(NULL)
  }

  tryCatch(
    jsonlite::fromJSON(txt, flatten = TRUE),
    error = function(e) {
      message(label, " returned text that could not be parsed as JSON; error: ", conditionMessage(e))
      NULL
    }
  )
}

# ==== 5. AWDB / SNOTEL helpers =============================================

parse_awdb_data_response <- function(resp, element_code) {

  if (!is.data.frame(resp) || !"stationTriplet" %in% names(resp) || !"data" %in% names(resp)) {
    return(tibble::tibble(
      nrcs_station_triplet = character(),
      provider_station_id = character(),
      obs_date = as.Date(character()),
      value = numeric(),
      element_code = character()
    ))
  }

  pieces <- vector("list", nrow(resp))

  for (i in seq_len(nrow(resp))) {

    trip <- as.character(resp$stationTriplet[i])
    provider_station_id <- stringr::str_extract(trip, "^\\d+")
    data_i <- resp$data[[i]]

    if (!is.data.frame(data_i) || nrow(data_i) == 0 || !"values" %in% names(data_i)) {
      pieces[[i]] <- tibble::tibble()
      next
    }

    value_pieces <- vector("list", nrow(data_i))

    for (j in seq_len(nrow(data_i))) {

      vals <- data_i$values[[j]]

      if (!is.data.frame(vals) || nrow(vals) == 0) {
        value_pieces[[j]] <- tibble::tibble()
        next
      }

      vals <- tibble::as_tibble(vals)
      date_col <- first_existing_col(vals, c("date", "Date", "datetime", "obsDate"))
      value_col <- first_existing_col(vals, c("value", "Value"))

      if (is.na(date_col) && ncol(vals) >= 1) date_col <- names(vals)[1]
      if (is.na(value_col) && ncol(vals) >= 2) value_col <- names(vals)[2]

      if (is.na(date_col) || is.na(value_col)) {
        value_pieces[[j]] <- tibble::tibble()
        next
      }

      value_pieces[[j]] <- tibble::tibble(
        nrcs_station_triplet = trip,
        provider_station_id = provider_station_id,
        obs_date = pt_date(vals[[date_col]]),
        value = pt_num(vals[[value_col]]),
        element_code = element_code
      ) |>
        dplyr::filter(!is.na(.data$obs_date))
    }

    pieces[[i]] <- dplyr::bind_rows(value_pieces)
  }

  dplyr::bind_rows(pieces)
}

fetch_awdb_wteq <- function(station_triplets) {

  station_triplets <- pt_chr(station_triplets)
  station_triplets <- unique(station_triplets[!is.na(station_triplets)])

  if (length(station_triplets) == 0) {
    return(tibble::tibble(
      nrcs_station_triplet = character(),
      provider_station_id = character(),
      obs_date = as.Date(character()),
      value = numeric(),
      element_code = character()
    ))
  }

  chunks <- split(station_triplets, ceiling(seq_along(station_triplets) / AWDB_CHUNK_SIZE))
  message("AWDB WTEQ chunks: ", length(chunks), " (", length(station_triplets), " stations)")

  purrr::map_dfr(chunks, function(trips) {

    Sys.sleep(REQUEST_PAUSE_SEC)

    awdb_url <- paste0(
      "https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1/data?",
      "stationTriplets=", utils::URLencode(paste(trips, collapse = ","), reserved = TRUE),
      "&elements=WTEQ",
      "&duration=DAILY",
      "&beginDate=", utils::URLencode(as.character(HISTORY_START_DATE), reserved = TRUE),
      "&endDate=", utils::URLencode(as.character(HISTORY_END_DATE), reserved = TRUE),
      "&periodRef=END"
    )

    resp <- try(jsonlite::fromJSON(awdb_url, flatten = TRUE), silent = TRUE)

    if (inherits(resp, "try-error") || is.null(resp) || !is.data.frame(resp) || nrow(resp) == 0) {
      return(tibble::tibble())
    }

    parse_awdb_data_response(resp, element_code = "WTEQ")
  })
}

# ==== 6. CDEC helper =========================================================

pt_empty_cdec_sensor <- function() {
  tibble::tibble(
    provider_station_id = character(),
    obs_date = as.Date(character()),
    value = numeric(),
    sensor_num = integer()
  )
}

pt_parse_cdec_json_response <- function(resp, id, sensor_num) {

  empty_out <- pt_empty_cdec_sensor()

  if (is.null(resp) || !is.data.frame(resp) || nrow(resp) == 0) {
    return(empty_out)
  }

  raw <- tibble::as_tibble(resp)
  names(raw) <- make.names(tolower(names(raw)))

  station_col <- first_existing_col(raw, c("stationid", "station.id", "station_id", "id", "station"))
  date_col    <- first_existing_col(raw, c("date", "datetime", "obsdate", "obs.date", "eventdate", "event.date"))
  value_col   <- first_existing_col(raw, c("value", "sensorvalue", "sensor.value", "obsvalue", "obs.value"))

  if (is.na(date_col) || is.na(value_col)) {
    message(
      "CDEC direct JSON for ", id,
      " sensor ", sensor_num,
      " could not be standardized. Columns: ", paste(names(raw), collapse = ", ")
    )
    return(empty_out)
  }

  provider_station_id <- if (!is.na(station_col)) {
    pt_chr(raw[[station_col]])
  } else {
    rep(id, nrow(raw))
  }

  tibble::tibble(
    provider_station_id = provider_station_id,
    obs_date = pt_date(raw[[date_col]]),
    value = pt_num(raw[[value_col]]),
    sensor_num = as.integer(sensor_num)
  ) |>
    dplyr::filter(!is.na(.data$provider_station_id), !is.na(.data$obs_date))
}

fetch_one_cdec_sensor <- function(id, sensor_num) {

  id <- pt_chr(id)[1]
  sensor_num <- as.integer(sensor_num)

  if (is.na(id)) return(pt_empty_cdec_sensor())

  Sys.sleep(REQUEST_PAUSE_SEC)

  cdec_url <- paste0(
    "https://cdec.water.ca.gov/dynamicapp/req/JSONDataServlet?",
    "Stations=", utils::URLencode(id, reserved = TRUE),
    "&SensorNums=", sensor_num,
    "&dur_code=", CDEC_DAILY_INTERVAL,
    "&Start=", utils::URLencode(as.character(HISTORY_START_DATE), reserved = TRUE),
    "&End=", utils::URLencode(as.character(HISTORY_END_DATE), reserved = TRUE)
  )

  resp <- pt_json_from_url(cdec_url, label = paste0("CDEC direct JSON ", id, " sensor ", sensor_num))
  direct_rows <- pt_parse_cdec_json_response(resp, id = id, sensor_num = sensor_num)

  if (is.data.frame(direct_rows) && nrow(direct_rows) > 0) {
    return(direct_rows)
  }

  message("CDEC direct JSON returned no rows for ", id, " sensor ", sensor_num, "; trying sharpshootR fallback once.")

  raw <- withCallingHandlers(
    tryCatch(
      sharpshootR::CDECquery(
        id = id,
        sensor = sensor_num,
        interval = CDEC_DAILY_INTERVAL,
        start = as.character(HISTORY_START_DATE),
        end = as.character(HISTORY_END_DATE)
      ),
      error = function(e) {
        message("CDEC sharpshootR fallback failed for ", id, " sensor ", sensor_num, "; error: ", conditionMessage(e))
        NULL
      }
    ),
    warning = function(w) {
      message("CDEC sharpshootR fallback warning for ", id, " sensor ", sensor_num, "; warning: ", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (is.null(raw) || !is.data.frame(raw) || nrow(raw) == 0) {
    return(pt_empty_cdec_sensor())
  }

  raw <- tibble::as_tibble(raw)

  station_col <- first_existing_col(raw, c("station_id", "id", "station", "Station", "STATION_ID"))
  date_col    <- first_existing_col(raw, c("datetime", "date", "obs_date", "Date", "DATE"))
  value_col   <- first_existing_col(raw, c("value", "VALUE", "snow_water_content", "SWE"))

  if (is.na(date_col) || is.na(value_col)) {
    return(pt_empty_cdec_sensor())
  }

  provider_station_id <- if (!is.na(station_col)) {
    pt_chr(raw[[station_col]])
  } else {
    rep(id, nrow(raw))
  }

  tibble::tibble(
    provider_station_id = provider_station_id,
    obs_date = pt_date(raw[[date_col]]),
    value = pt_num(raw[[value_col]]),
    sensor_num = sensor_num
  ) |>
    dplyr::filter(!is.na(.data$provider_station_id), !is.na(.data$obs_date))
}

standardize_cdec_history_sensor <- function(sensor_obs, station_row, source_element, source_label, source_note) {

  if (!is.data.frame(sensor_obs) || nrow(sensor_obs) == 0) {
    return(pt_empty_history())
  }

  tibble::as_tibble(sensor_obs) |>
    dplyr::transmute(
      station_uid = station_row$station_uid,
      live_provider_key = station_row$live_provider_key,
      provider = station_row$provider,
      provider_station_id = station_row$provider_station_id,
      station_name = station_row$station_name,
      obs_date = as.Date(.data$obs_date),
      raw_swe_in = pt_num(.data$value),
      swe_in = pt_swe_num(.data$value),
      source_element = source_element,
      swe_source_label = source_label,
      swe_source_note = source_note
    ) |>
    dplyr::filter(!is.na(.data$station_uid), !is.na(.data$obs_date))
}

fetch_one_cdec_swe <- function(station_row) {

  id <- pt_chr(station_row$cdec_id)[1]

  if (is.na(id)) return(pt_empty_history())

  raw82 <- fetch_one_cdec_sensor(id = id, sensor_num = CDEC_REVISED_SWE_SENSOR)
  raw3  <- fetch_one_cdec_sensor(id = id, sensor_num = CDEC_RAW_SWE_SENSOR)

  sensor82_history <- standardize_cdec_history_sensor(
    raw82,
    station_row = station_row,
    source_element = "CDEC_SENSOR_82_SNO_ADJ",
    source_label = "CDEC SNO ADJ (#82)",
    source_note = "CDEC revised/adjusted SWE source used for historical context."
  )

  sensor3_history <- standardize_cdec_history_sensor(
    raw3,
    station_row = station_row,
    source_element = "CDEC_SENSOR_3_FALLBACK_NO_82",
    source_label = "CDEC SNOW WC (#3 fallback; no #82)",
    source_note = "Raw CDEC SNOW WC source used only because no valid #82 adjusted SWE was available in the historical fetch window."
  )

  n_valid82 <- sum(!is.na(sensor82_history$swe_in), na.rm = TRUE)

  if (n_valid82 >= CDEC_HISTORY_82_MIN_VALID_OBS) {
    return(sensor82_history)
  }

  sensor3_history
}

# ==== 7. Read station index ==================================================

if (!file.exists(STATION_INDEX_CSV)) {
  stop(
    "Missing snow-pillow station index: ", STATION_INDEX_CSV,
    "\nRun export_snow_pillow_live_inputs() before building historical context."
  )
}

message("Reading snow-pillow station index: ", STATION_INDEX_CSV)

stations <- readr::read_csv(STATION_INDEX_CSV, show_col_types = FALSE) |>
  dplyr::mutate(
    station_uid = pt_chr(.data$station_uid),
    live_provider_key = pt_chr(.data$live_provider_key),
    provider = pt_chr(.data$provider),
    provider_station_id = pt_chr(.data$provider_station_id),
    station_name = pt_chr(.data$station_name),
    nrcs_station_triplet = pt_chr(.data$nrcs_station_triplet),
    cdec_id = pt_chr(.data$cdec_id)
  ) |>
  dplyr::filter(!is.na(.data$station_uid), !is.na(.data$provider_station_id))

if (!is.na(STATION_LIMIT) && STATION_LIMIT > 0) {
  message("Development station limit active: ", STATION_LIMIT)
  stations <- stations |>
    dplyr::slice_head(n = STATION_LIMIT)
}

message("Snow-pillow history station rows: ", nrow(stations))
message("Historical fetch window: ", as.character(HISTORY_START_DATE), " to ", as.character(HISTORY_END_DATE))
message("Current water year excluded from percentile context: WY", CURRENT_WY)

# ==== 8. Load/fetch/cache station histories =================================

stations <- stations |>
  dplyr::mutate(
    cache_path = dplyr::case_when(
      .data$live_provider_key == "cdec_snow_sensor" ~ vapply(.data$station_uid, pt_station_cache_path_source, character(1), source_key = "cdec_sno_adj_82_hist"),
      TRUE ~ vapply(.data$station_uid, pt_station_cache_path, character(1))
    )
  )

history_source_mode_used <- "provider_or_site_cache"
daily_history_rds_reused <- FALSE
rds_min_date <- as.Date(NA)
rds_max_date <- as.Date(NA)
rds_station_rows <- NA_integer_
rds_station_match_rows <- NA_integer_
rds_reuse_message <- NA_character_

history_raw <- pt_empty_history()
history_daily <- tibble::tibble()
invalid_negative_rows <- NA_integer_
invalid_high_rows <- NA_integer_

try_daily_rds <- HISTORY_SOURCE_MODE %in% c("auto", "rds") &&
  file.exists(OUT_DAILY_HISTORY_RDS) &&
  !REFRESH_SITE_CACHE &&
  is.na(STATION_LIMIT)

if (try_daily_rds) {

  rds_obj <- try(readRDS(OUT_DAILY_HISTORY_RDS), silent = TRUE)

  if (!inherits(rds_obj, "try-error") && is.data.frame(rds_obj)) {

    rds_hist <- tibble::as_tibble(rds_obj)

    required_rds_cols <- c(
      "station_uid", "live_provider_key", "provider", "provider_station_id",
      "station_name", "obs_date", "swe_in", "source_element"
    )

    missing_rds_cols <- setdiff(required_rds_cols, names(rds_hist))

    if (length(missing_rds_cols) == 0 && nrow(rds_hist) > 0) {

      rds_hist <- rds_hist |>
        dplyr::mutate(
          station_uid = pt_chr(.data$station_uid),
          live_provider_key = pt_chr(.data$live_provider_key),
          provider = pt_chr(.data$provider),
          provider_station_id = pt_chr(.data$provider_station_id),
          station_name = pt_chr(.data$station_name),
          obs_date = as.Date(.data$obs_date),
          swe_in = pt_swe_num(.data$swe_in),
          source_element = pt_chr(.data$source_element)
        ) |>
        dplyr::filter(!is.na(.data$station_uid), !is.na(.data$obs_date), !is.na(.data$swe_in))

      rds_min_date <- suppressWarnings(min(rds_hist$obs_date, na.rm = TRUE))
      rds_max_date <- suppressWarnings(max(rds_hist$obs_date, na.rm = TRUE))
      rds_station_rows <- dplyr::n_distinct(rds_hist$station_uid)
      rds_station_match_rows <- length(intersect(stations$station_uid, unique(rds_hist$station_uid)))

      covers_start <- !is.na(rds_min_date) && rds_min_date <= HISTORY_START_DATE
      covers_prior_complete_wy <- !is.na(rds_max_date) && rds_max_date >= PRIOR_COMPLETE_WY_END_DATE
      has_station_coverage <- !is.na(rds_station_match_rows) && rds_station_match_rows >= max(1L, floor(0.90 * nrow(stations)))

      if (covers_start && covers_prior_complete_wy && has_station_coverage) {

        history_source_mode_used <- "existing_selected_daily_history_rds"
        daily_history_rds_reused <- TRUE
        rds_reuse_message <- paste0(
          "Reused selected daily-history RDS; covers ", as.character(rds_min_date),
          " to ", as.character(rds_max_date), ", prior complete WY end required ",
          as.character(PRIOR_COMPLETE_WY_END_DATE), "."
        )

        message("Using existing selected daily-history RDS: ", OUT_DAILY_HISTORY_RDS)
        message("  RDS date coverage: ", as.character(rds_min_date), " to ", as.character(rds_max_date))
        message("  Required complete prior WY coverage through: ", as.character(PRIOR_COMPLETE_WY_END_DATE))
        message("  Station coverage in RDS: ", rds_station_match_rows, " of ", nrow(stations), " station-index rows")

        history_daily <- rds_hist |>
          dplyr::filter(.data$obs_date >= HISTORY_START_DATE, .data$obs_date <= HISTORY_END_DATE) |>
          dplyr::mutate(
            water_year = if ("water_year" %in% names(rds_hist)) as.integer(.data$water_year) else pt_water_year(.data$obs_date),
            water_day = if ("water_day" %in% names(rds_hist)) as.integer(.data$water_day) else pt_water_day(.data$obs_date),
            water_month = if ("water_month" %in% names(rds_hist)) as.integer(.data$water_month) else pt_water_month(.data$obs_date),
            month_label = if ("month_label" %in% names(rds_hist)) pt_chr(.data$month_label) else pt_month_label_from_water_month(.data$water_month),
            month_start_water_day = if ("month_start_water_day" %in% names(rds_hist)) as.integer(.data$month_start_water_day) else pt_month_start_water_day(.data$water_month),
            obs_date_local = as.character(.data$obs_date),
            n_values_same_day = if ("n_values_same_day" %in% names(rds_hist)) as.integer(.data$n_values_same_day) else 1L
          ) |>
          dplyr::select(
            "station_uid", "live_provider_key", "provider", "provider_station_id",
            "station_name", "source_element", "obs_date", "water_year", "water_day",
            "water_month", "month_label", "month_start_water_day", "obs_date_local",
            "swe_in", "n_values_same_day"
          ) |>
          dplyr::arrange(.data$station_uid, .data$obs_date)

        ## RDS mode starts from the already selected/cleaned daily history.  Raw
        ## invalid-provider rows are not present, so raw-QC exclusion counts are
        ## not recomputed in this mode.
        history_raw <- history_daily |>
          dplyr::transmute(
            station_uid = .data$station_uid,
            live_provider_key = .data$live_provider_key,
            provider = .data$provider,
            provider_station_id = .data$provider_station_id,
            station_name = .data$station_name,
            obs_date = .data$obs_date,
            raw_swe_in = .data$swe_in,
            swe_in = .data$swe_in,
            source_element = .data$source_element
          )

        invalid_negative_rows <- NA_integer_
        invalid_high_rows <- NA_integer_

      } else {
        rds_reuse_message <- paste0(
          "Existing RDS not reused; covers_start=", covers_start,
          "; covers_prior_complete_wy=", covers_prior_complete_wy,
          "; station_coverage_ok=", has_station_coverage,
          "; RDS coverage ", as.character(rds_min_date), " to ", as.character(rds_max_date),
          "; required through ", as.character(PRIOR_COMPLETE_WY_END_DATE), "."
        )
      }
    } else {
      rds_reuse_message <- paste0(
        "Existing RDS not reused; missing required columns: ",
        paste(missing_rds_cols, collapse = ", ")
      )
    }
  } else {
    rds_reuse_message <- "Existing RDS not reused; file could not be read as a data.frame."
  }

  if (!daily_history_rds_reused && HISTORY_SOURCE_MODE == "rds") {
    stop(
      "SNOW_PILLOW_HISTORY_SOURCE_MODE='rds' requested, but the existing daily-history RDS is not sufficient.\n",
      rds_reuse_message
    )
  }
}

if (!daily_history_rds_reused) {

  if (!is.na(rds_reuse_message)) message(rds_reuse_message)
  message("Using provider/site-cache history path.")

  existing_cache <- file.exists(stations$cache_path) & !REFRESH_SITE_CACHE

  message("Station history cache hits: ", sum(existing_cache), " of ", nrow(stations))

  history_from_cache <- purrr::map_dfr(stations$cache_path[existing_cache], function(path) {
    x <- try(readRDS(path), silent = TRUE)
    if (inherits(x, "try-error") || !is.data.frame(x)) pt_empty_history() else tibble::as_tibble(x)
  })

  stations_to_fetch <- stations[!existing_cache, , drop = FALSE]

  # ---- 8A. Fetch/cache SNOTEL in AWDB chunks -------------------------------

  snotel_to_fetch <- stations_to_fetch |>
    dplyr::filter(.data$live_provider_key == "nrcs_snotel")

  snotel_history <- pt_empty_history()

  if (nrow(snotel_to_fetch) > 0) {

    awdb_raw <- fetch_awdb_wteq(snotel_to_fetch$nrcs_station_triplet)

    snotel_key <- snotel_to_fetch |>
      dplyr::select(
        "station_uid", "live_provider_key", "provider", "provider_station_id",
        "station_name", "nrcs_station_triplet", "cache_path"
      )

    snotel_history <- awdb_raw |>
      dplyr::left_join(snotel_key, by = "nrcs_station_triplet") |>
      dplyr::transmute(
        station_uid = .data$station_uid,
        live_provider_key = .data$live_provider_key,
        provider = .data$provider,
        provider_station_id = .data$provider_station_id.y,
        station_name = .data$station_name,
        obs_date = as.Date(.data$obs_date),
        raw_swe_in = pt_num(.data$value),
        swe_in = pt_swe_num(.data$value),
        source_element = "WTEQ"
      ) |>
      dplyr::filter(!is.na(.data$station_uid), !is.na(.data$obs_date))

    ## Save per-station caches immediately so interrupted runs can resume.
    split_snotel <- split(snotel_history, snotel_history$station_uid)

    for (uid in names(split_snotel)) {
      cache_path <- snotel_to_fetch$cache_path[match(uid, snotel_to_fetch$station_uid)]
      if (!is.na(cache_path) && length(cache_path) == 1) saveRDS(split_snotel[[uid]], cache_path)
    }

    ## Empty station caches are useful too: they avoid repeated failed calls in a
    ## station-limited development run or a service-gap situation.
    missing_uid <- setdiff(snotel_to_fetch$station_uid, names(split_snotel))
    for (uid in missing_uid) {
      cache_path <- snotel_to_fetch$cache_path[match(uid, snotel_to_fetch$station_uid)]
      if (!is.na(cache_path) && length(cache_path) == 1) saveRDS(pt_empty_history(), cache_path)
    }
  }

  # ---- 8B. Fetch/cache CDEC station-by-station ------------------------------

  cdec_to_fetch <- stations_to_fetch |>
    dplyr::filter(.data$live_provider_key == "cdec_snow_sensor")

  message("CDEC SWE history stations to query: ", nrow(cdec_to_fetch), " (#82 preferred; #3 fallback only if no valid #82)")

  cdec_history <- purrr::pmap_dfr(cdec_to_fetch, function(...) {

    row <- tibble::as_tibble(list(...))
    out <- fetch_one_cdec_swe(row)

    cache_path <- row$cache_path[[1]]
    if (!is.na(cache_path)) saveRDS(out, cache_path)

    out
  })

  history_raw <- dplyr::bind_rows(history_from_cache, snotel_history, cdec_history) |>
    dplyr::mutate(
      ## Re-apply QC after binding so previously cached station histories are
      ## cleaned even when site caches were written by an earlier script version.
      raw_swe_in = pt_num(.data$raw_swe_in),
      swe_in = pt_swe_num(.data$raw_swe_in)
    )

  invalid_negative_rows <- pt_negative_swe_count(history_raw$raw_swe_in)
  invalid_high_rows <- pt_high_swe_count(history_raw$raw_swe_in)

  history_daily <- history_raw |>
    dplyr::filter(!is.na(.data$station_uid), !is.na(.data$obs_date), !is.na(.data$swe_in)) |>
    dplyr::mutate(
      water_year = pt_water_year(.data$obs_date),
      water_day = pt_water_day(.data$obs_date),
      water_month = pt_water_month(.data$obs_date),
      month_label = pt_month_label_from_water_month(.data$water_month),
      month_start_water_day = pt_month_start_water_day(.data$water_month),
      obs_date_local = as.character(.data$obs_date)
    ) |>
    dplyr::group_by(
      .data$station_uid, .data$live_provider_key, .data$provider,
      .data$provider_station_id, .data$station_name, .data$source_element, .data$obs_date,
      .data$water_year, .data$water_day, .data$water_month,
      .data$month_label, .data$month_start_water_day, .data$obs_date_local
    ) |>
    dplyr::summarise(
      swe_in = mean(.data$swe_in, na.rm = TRUE),
      n_values_same_day = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$station_uid, .data$obs_date)
}

# ==== 9. Build daily percentile context =====================================

history_reference <- history_daily |>
  dplyr::filter(.data$water_year < CURRENT_WY)

## Station-specific period of analysis (POA) for daily percentile context.
## The popup notes use the month-year label because it is compact and more
## directly interpretable than a full date stamp in a small map popup.
daily_ref_meta <- history_reference |>
  dplyr::group_by(.data$station_uid) |>
  dplyr::summarise(
    daily_ref_start_date = min(.data$obs_date, na.rm = TRUE),
    daily_ref_end_date   = max(.data$obs_date, na.rm = TRUE),
    daily_ref_start_wy   = min(.data$water_year, na.rm = TRUE),
    daily_ref_end_wy     = max(.data$water_year, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    daily_ref_label = pt_period_label(.data$daily_ref_start_date, .data$daily_ref_end_date),
    daily_ref_wy_label = pt_wy_period_label(.data$daily_ref_start_wy, .data$daily_ref_end_wy)
  )

waterday_percentiles <- history_reference |>
  dplyr::group_by(.data$station_uid, .data$water_day) |>
  dplyr::summarise(
    n_years = dplyr::n_distinct(.data$water_year),
    p00_swe_in = as.numeric(stats::quantile(.data$swe_in, probs = 0.00, na.rm = TRUE, type = 7)),
    p10_swe_in = as.numeric(stats::quantile(.data$swe_in, probs = 0.10, na.rm = TRUE, type = 7)),
    p30_swe_in = as.numeric(stats::quantile(.data$swe_in, probs = 0.30, na.rm = TRUE, type = 7)),
    p50_swe_in = as.numeric(stats::quantile(.data$swe_in, probs = 0.50, na.rm = TRUE, type = 7)),
    p70_swe_in = as.numeric(stats::quantile(.data$swe_in, probs = 0.70, na.rm = TRUE, type = 7)),
    p90_swe_in = as.numeric(stats::quantile(.data$swe_in, probs = 0.90, na.rm = TRUE, type = 7)),
    p100_swe_in = as.numeric(stats::quantile(.data$swe_in, probs = 1.00, na.rm = TRUE, type = 7)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    context_ok = .data$n_years >= MIN_YEARS_FOR_DAILY_CONTEXT,
    min_years_for_context = MIN_YEARS_FOR_DAILY_CONTEXT
  ) |>
  dplyr::left_join(daily_ref_meta, by = "station_uid") |>
  dplyr::left_join(
    stations |>
      dplyr::select("station_uid", "live_provider_key", "provider", "provider_station_id", "station_name"),
    by = "station_uid"
  ) |>
  dplyr::select(
    "station_uid", "live_provider_key", "provider", "provider_station_id", "station_name",
    "water_day", "n_years", "context_ok", "min_years_for_context",
    "daily_ref_start_date", "daily_ref_end_date", "daily_ref_start_wy", "daily_ref_end_wy",
    "daily_ref_label", "daily_ref_wy_label",
    "p00_swe_in", "p10_swe_in", "p30_swe_in", "p50_swe_in",
    "p70_swe_in", "p90_swe_in", "p100_swe_in"
  ) |>
  dplyr::arrange(.data$station_uid, .data$water_day)


# ==== 10. Build text median-normal context ==================================

## These station/day medians support a compact popup row such as:
##   % median    42% vs WY1991-WY2020; 51% vs rolling WY1996-WY2025
## They intentionally do not replace the Plot B POA percentile envelope or the
## map context-bin colors.  Support flags let the browser/latest feed hide weak
## denominators and avoid percent-of-zero displays.
normal_fixed_label <- pt_wy_period_label(NORMAL_FIXED_START_WY, NORMAL_FIXED_END_WY)
normal_rolling_label <- pt_wy_period_label(NORMAL_ROLLING_START_WY, NORMAL_ROLLING_END_WY)

normal_fixed <- history_reference |>
  dplyr::filter(
    .data$water_year >= NORMAL_FIXED_START_WY,
    .data$water_year <= NORMAL_FIXED_END_WY
  ) |>
  dplyr::group_by(.data$station_uid, .data$water_day) |>
  dplyr::summarise(
    normal_fixed_n_years = dplyr::n_distinct(.data$water_year),
    normal_fixed_median_swe_in = stats::median(.data$swe_in, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    normal_fixed_wy_start = NORMAL_FIXED_START_WY,
    normal_fixed_wy_end = NORMAL_FIXED_END_WY,
    normal_fixed_label = normal_fixed_label,
    normal_fixed_full_years = NORMAL_FIXED_END_WY - NORMAL_FIXED_START_WY + 1L,
    normal_min_years_for_popup = NORMAL_MIN_YEARS_FOR_POPUP,
    normal_fixed_support_ok = .data$normal_fixed_n_years >= NORMAL_MIN_YEARS_FOR_POPUP &
      !is.na(.data$normal_fixed_median_swe_in)
  )

normal_rolling <- history_reference |>
  dplyr::filter(
    .data$water_year >= NORMAL_ROLLING_START_WY,
    .data$water_year <= NORMAL_ROLLING_END_WY
  ) |>
  dplyr::group_by(.data$station_uid, .data$water_day) |>
  dplyr::summarise(
    normal_rolling_n_years = dplyr::n_distinct(.data$water_year),
    normal_rolling_median_swe_in = stats::median(.data$swe_in, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    normal_rolling_wy_start = NORMAL_ROLLING_START_WY,
    normal_rolling_wy_end = NORMAL_ROLLING_END_WY,
    normal_rolling_label = normal_rolling_label,
    normal_rolling_full_years = NORMAL_ROLLING_YEARS,
    normal_min_years_for_popup = NORMAL_MIN_YEARS_FOR_POPUP,
    normal_rolling_support_ok = .data$normal_rolling_n_years >= NORMAL_MIN_YEARS_FOR_POPUP &
      !is.na(.data$normal_rolling_median_swe_in)
  )

normal_medians <- dplyr::full_join(
  normal_fixed,
  normal_rolling,
  by = c("station_uid", "water_day", "normal_min_years_for_popup")
) |>
  dplyr::left_join(
    stations |>
      dplyr::select("station_uid", "live_provider_key", "provider", "provider_station_id", "station_name"),
    by = "station_uid"
  ) |>
  dplyr::select(
    "station_uid", "live_provider_key", "provider", "provider_station_id", "station_name", "water_day",
    "normal_min_years_for_popup",
    "normal_fixed_wy_start", "normal_fixed_wy_end", "normal_fixed_label",
    "normal_fixed_full_years", "normal_fixed_n_years", "normal_fixed_support_ok", "normal_fixed_median_swe_in",
    "normal_rolling_wy_start", "normal_rolling_wy_end", "normal_rolling_label",
    "normal_rolling_full_years", "normal_rolling_n_years", "normal_rolling_support_ok", "normal_rolling_median_swe_in"
  ) |>
  dplyr::arrange(.data$station_uid, .data$water_day)

# ==== 10. Build recent monthly context =======================================

recent_wy_min <- CURRENT_WY - RECENT_MONTHLY_YEARS + 1L

## First build one clean station/month table for the entire history.  This lets
## Plot A show recent monthly values while also carrying compact historical
## monthly reference statistics (30th, median, 70th) for the same water-month.
monthly_all <- history_daily |>
  dplyr::group_by(
    .data$station_uid, .data$live_provider_key, .data$provider,
    .data$provider_station_id, .data$station_name, .data$water_year,
    .data$water_month, .data$month_label, .data$month_start_water_day
  ) |>
  dplyr::summarise(
    n_daily_obs = dplyr::n(),
    swe_month_mean_raw_in = mean(.data$swe_in, na.rm = TRUE),
    swe_month_max_in = max(.data$swe_in, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    month_date = pt_swe_month_date(.data$water_year, .data$water_month),
    monthly_mean_ok = .data$n_daily_obs >= MONTHLY_MIN_DAILY_OBS,
    monthly_min_daily_obs = MONTHLY_MIN_DAILY_OBS,
    swe_month_mean_in = dplyr::if_else(.data$monthly_mean_ok, .data$swe_month_mean_raw_in, NA_real_)
  )

monthly_ref_meta <- monthly_all |>
  dplyr::filter(
    .data$water_year < CURRENT_WY,
    .data$monthly_mean_ok,
    !is.na(.data$swe_month_mean_in),
    !is.na(.data$month_date)
  ) |>
  dplyr::group_by(.data$station_uid) |>
  dplyr::summarise(
    monthly_ref_start_date = min(.data$month_date, na.rm = TRUE),
    monthly_ref_end_date   = max(.data$month_date, na.rm = TRUE),
    monthly_ref_start_wy   = min(.data$water_year, na.rm = TRUE),
    monthly_ref_end_wy     = max(.data$water_year, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    monthly_ref_label = pt_period_label(.data$monthly_ref_start_date, .data$monthly_ref_end_date),
    monthly_ref_wy_label = pt_wy_period_label(.data$monthly_ref_start_wy, .data$monthly_ref_end_wy)
  )

monthly_reference <- monthly_all |>
  dplyr::filter(
    .data$water_year < CURRENT_WY,
    .data$monthly_mean_ok,
    !is.na(.data$swe_month_mean_in)
  ) |>
  dplyr::group_by(.data$station_uid, .data$water_month) |>
  dplyr::summarise(
    monthly_ref_n_years = dplyr::n_distinct(.data$water_year),
    monthly_ref_p30_swe_in = as.numeric(stats::quantile(.data$swe_month_mean_in, probs = 0.30, na.rm = TRUE, type = 7)),
    monthly_ref_p50_swe_in = as.numeric(stats::quantile(.data$swe_month_mean_in, probs = 0.50, na.rm = TRUE, type = 7)),
    monthly_ref_p70_swe_in = as.numeric(stats::quantile(.data$swe_month_mean_in, probs = 0.70, na.rm = TRUE, type = 7)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    monthly_ref_ok = .data$monthly_ref_n_years >= MIN_YEARS_FOR_MONTHLY_CONTEXT,
    min_years_for_monthly_context = MIN_YEARS_FOR_MONTHLY_CONTEXT
  ) |>
  dplyr::left_join(monthly_ref_meta, by = "station_uid")

monthly_context <- monthly_all |>
  dplyr::filter(.data$water_year >= recent_wy_min, .data$water_year <= CURRENT_WY) |>
  dplyr::left_join(monthly_reference, by = c("station_uid", "water_month")) |>
  dplyr::mutate(
    monthly_ref_ok = dplyr::coalesce(.data$monthly_ref_ok, FALSE),
    monthly_ref_n_years = suppressWarnings(as.integer(.data$monthly_ref_n_years)),
    min_years_for_monthly_context = dplyr::coalesce(
      suppressWarnings(as.integer(.data$min_years_for_monthly_context)),
      MIN_YEARS_FOR_MONTHLY_CONTEXT
    )
  ) |>
  dplyr::select(
    "station_uid", "live_provider_key", "provider", "provider_station_id", "station_name",
    "water_year", "water_month", "month_label", "month_start_water_day",
    "n_daily_obs", "monthly_min_daily_obs", "monthly_mean_ok",
    "swe_month_mean_in", "swe_month_max_in",
    "monthly_ref_ok", "monthly_ref_n_years", "min_years_for_monthly_context",
    "monthly_ref_start_date", "monthly_ref_end_date", "monthly_ref_start_wy", "monthly_ref_end_wy",
    "monthly_ref_label", "monthly_ref_wy_label",
    "monthly_ref_p30_swe_in", "monthly_ref_p50_swe_in", "monthly_ref_p70_swe_in"
  ) |>
  dplyr::arrange(.data$station_uid, .data$water_year, .data$water_month)

# ==== 11. Build prior-WY fallback traces =====================================

## Stations with too few context_ok daily rows do not draw percentile ribbons in
## Plot B. For these stations only, export a compact set of prior complete WY
## traces so the popup can still show visual context without implying a robust
## percentile envelope.
fallback_station_ids <- waterday_percentiles |>
  dplyr::group_by(.data$station_uid) |>
  dplyr::summarise(
    context_ok_rows = sum(.data$context_ok, na.rm = TRUE),
    max_n_years = suppressWarnings(max(.data$n_years, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  dplyr::filter(.data$context_ok_rows < 30L) |>
  dplyr::pull(.data$station_uid)

prior_wy_fallback_wy_ok <- history_reference |>
  dplyr::filter(.data$station_uid %in% fallback_station_ids) |>
  dplyr::group_by(
    .data$station_uid, .data$live_provider_key, .data$provider,
    .data$provider_station_id, .data$station_name, .data$water_year
  ) |>
  dplyr::summarise(
    fallback_wy_obs = dplyr::n(),
    fallback_wy_min_water_day = suppressWarnings(min(.data$water_day, na.rm = TRUE)),
    fallback_wy_max_water_day = suppressWarnings(max(.data$water_day, na.rm = TRUE)),
    fallback_wy_max_swe_in = suppressWarnings(max(.data$swe_in, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    fallback_wy_span_days = .data$fallback_wy_max_water_day - .data$fallback_wy_min_water_day + 1L,
    fallback_wy_trace_ok = is.finite(.data$fallback_wy_obs) &
      .data$fallback_wy_obs >= PRIOR_WY_FALLBACK_MIN_OBS &
      is.finite(.data$fallback_wy_span_days) &
      .data$fallback_wy_span_days >= PRIOR_WY_FALLBACK_MIN_SPAN_DAYS
  ) |>
  dplyr::filter(.data$fallback_wy_trace_ok) |>
  dplyr::arrange(.data$station_uid, dplyr::desc(.data$water_year)) |>
  dplyr::group_by(.data$station_uid) |>
  dplyr::mutate(
    fallback_wy_rank = dplyr::row_number()
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(.data$fallback_wy_rank <= PRIOR_WY_FALLBACK_YEARS) |>
  dplyr::select(
    "station_uid", "water_year", "fallback_wy_rank",
    "fallback_wy_obs", "fallback_wy_span_days", "fallback_wy_max_swe_in"
  )

prior_wy_fallback_traces <- history_reference |>
  dplyr::inner_join(
    prior_wy_fallback_wy_ok,
    by = c("station_uid", "water_year")
  ) |>
  dplyr::select(
    "station_uid", "live_provider_key", "provider", "provider_station_id", "station_name",
    "water_year", "water_day", "obs_date_local", "swe_in", "fallback_wy_rank",
    "fallback_wy_obs", "fallback_wy_span_days", "fallback_wy_max_swe_in"
  ) |>
  dplyr::arrange(.data$station_uid, dplyr::desc(.data$water_year), .data$water_day)

# ==== 12. QA summaries =======================================================

station_summary_history <- history_daily |>
  dplyr::group_by(.data$station_uid) |>
  dplyr::summarise(
    daily_rows = dplyr::n(),
    first_obs_date = min(.data$obs_date, na.rm = TRUE),
    last_obs_date = max(.data$obs_date, na.rm = TRUE),
    first_water_year = min(.data$water_year, na.rm = TRUE),
    last_water_year = max(.data$water_year, na.rm = TRUE),
    n_water_years = dplyr::n_distinct(.data$water_year),
    positive_swe_rows = sum(.data$swe_in > 0, na.rm = TRUE),
    zero_swe_rows = sum(.data$swe_in == 0, na.rm = TRUE),
    max_swe_in = max(.data$swe_in, na.rm = TRUE),
    .groups = "drop"
  )

station_summary_raw_qc <- history_raw |>
  dplyr::filter(!is.na(.data$station_uid)) |>
  dplyr::group_by(.data$station_uid) |>
  dplyr::summarise(
    invalid_negative_swe_rows = pt_negative_swe_count(.data$raw_swe_in),
    invalid_high_swe_rows = pt_high_swe_count(.data$raw_swe_in),
    .groups = "drop"
  )

station_summary <- stations |>
  dplyr::select("station_uid", "live_provider_key", "provider", "provider_station_id", "station_name") |>
  dplyr::left_join(station_summary_history, by = "station_uid") |>
  dplyr::left_join(station_summary_raw_qc, by = "station_uid") |>
  dplyr::mutate(
    daily_rows = dplyr::coalesce(.data$daily_rows, 0L),
    n_water_years = dplyr::coalesce(.data$n_water_years, 0L),
    positive_swe_rows = dplyr::coalesce(.data$positive_swe_rows, 0L),
    zero_swe_rows = dplyr::coalesce(.data$zero_swe_rows, 0L),
    invalid_negative_swe_rows = dplyr::coalesce(.data$invalid_negative_swe_rows, 0L),
    invalid_high_swe_rows = dplyr::coalesce(.data$invalid_high_swe_rows, 0L)
  ) |>
  dplyr::arrange(.data$live_provider_key, .data$provider_station_id)

run_summary <- tibble::tibble(
  metric = c(
    "build_time_local",
    "history_start_date",
    "history_end_date",
    "current_water_year_excluded_from_percentiles",
    "history_source_mode_requested",
    "history_source_mode_used",
    "daily_history_rds_reused",
    "daily_history_rds_min_date",
    "daily_history_rds_max_date",
    "prior_complete_wy_end_required",
    "daily_history_rds_reuse_message",
    "station_rows",
    "history_daily_rows",
    "stations_with_history",
    "waterday_percentile_rows",
    "stations_with_waterday_percentiles",
    "waterday_rows_context_ok",
    "monthly_context_rows",
    "monthly_context_rows_ok",
    "monthly_min_daily_obs",
    "min_years_for_daily_context",
    "min_years_for_monthly_context",
    "recent_monthly_years",
    "monthly_context_rows_ref_ok",
    "normal_median_rows",
    "normal_fixed_label",
    "normal_rolling_label",
    "normal_min_years_for_popup",
    "normal_fixed_rows_support_ok",
    "normal_rolling_rows_support_ok",
    "cdec_history_rows_selected_sno_adj_82",
    "cdec_history_rows_selected_snow_wc_3_fallback",
    "cdec_history_stations_selected_sno_adj_82",
    "cdec_history_stations_selected_snow_wc_3_fallback",
    "cdec_history_82_min_valid_obs",
    "prior_wy_fallback_station_rows",
    "prior_wy_fallback_trace_rows",
    "prior_wy_fallback_years",
    "prior_wy_fallback_min_obs",
    "prior_wy_fallback_min_span_days",
    "prior_wy_fallback_station_rows_with_traces",
    "max_valid_swe_in",
    "invalid_negative_swe_rows_excluded",
    "invalid_high_swe_rows_excluded"
  ),
  value = as.character(c(
    format(lubridate::with_tz(Sys.time(), LOCAL_TZ), "%Y-%m-%d %I:%M %p %Z"),
    as.character(HISTORY_START_DATE),
    as.character(HISTORY_END_DATE),
    CURRENT_WY,
    HISTORY_SOURCE_MODE,
    history_source_mode_used,
    daily_history_rds_reused,
    as.character(rds_min_date),
    as.character(rds_max_date),
    as.character(PRIOR_COMPLETE_WY_END_DATE),
    rds_reuse_message,
    nrow(stations),
    nrow(history_daily),
    dplyr::n_distinct(history_daily$station_uid),
    nrow(waterday_percentiles),
    dplyr::n_distinct(waterday_percentiles$station_uid),
    sum(waterday_percentiles$context_ok, na.rm = TRUE),
    nrow(monthly_context),
    sum(monthly_context$monthly_mean_ok, na.rm = TRUE),
    MONTHLY_MIN_DAILY_OBS,
    MIN_YEARS_FOR_DAILY_CONTEXT,
    MIN_YEARS_FOR_MONTHLY_CONTEXT,
    RECENT_MONTHLY_YEARS,
    sum(monthly_context$monthly_ref_ok, na.rm = TRUE),
    nrow(normal_medians),
    normal_fixed_label,
    normal_rolling_label,
    NORMAL_MIN_YEARS_FOR_POPUP,
    sum(normal_medians$normal_fixed_support_ok, na.rm = TRUE),
    sum(normal_medians$normal_rolling_support_ok, na.rm = TRUE),
    sum(history_daily$source_element == "CDEC_SENSOR_82_SNO_ADJ", na.rm = TRUE),
    sum(history_daily$source_element == "CDEC_SENSOR_3_FALLBACK_NO_82", na.rm = TRUE),
    dplyr::n_distinct(history_daily$station_uid[history_daily$source_element == "CDEC_SENSOR_82_SNO_ADJ"]),
    dplyr::n_distinct(history_daily$station_uid[history_daily$source_element == "CDEC_SENSOR_3_FALLBACK_NO_82"]),
    CDEC_HISTORY_82_MIN_VALID_OBS,
    length(fallback_station_ids),
    nrow(prior_wy_fallback_traces),
    PRIOR_WY_FALLBACK_YEARS,
    PRIOR_WY_FALLBACK_MIN_OBS,
    PRIOR_WY_FALLBACK_MIN_SPAN_DAYS,
    dplyr::n_distinct(prior_wy_fallback_traces$station_uid),
    MAX_VALID_SWE_IN,
    invalid_negative_rows,
    invalid_high_rows
  ))
)

# ==== 12. Write outputs ======================================================

saveRDS(history_daily, OUT_DAILY_HISTORY_RDS)
saveRDS(waterday_percentiles, OUT_WATERDAY_PCT_RDS)
saveRDS(monthly_context, OUT_MONTHLY_CTX_RDS)
saveRDS(prior_wy_fallback_traces, OUT_PRIOR_WY_FALLBACK_TRACES_RDS)
saveRDS(normal_medians, OUT_NORMAL_MEDIANS_RDS)

readr::write_csv(waterday_percentiles, OUT_WATERDAY_PCT_INPUT_CSV, na = "")
readr::write_csv(monthly_context, OUT_MONTHLY_CTX_INPUT_CSV, na = "")
readr::write_csv(prior_wy_fallback_traces, OUT_PRIOR_WY_FALLBACK_TRACES_INPUT_CSV, na = "")
readr::write_csv(normal_medians, OUT_NORMAL_MEDIANS_INPUT_CSV, na = "")
readr::write_csv(waterday_percentiles, OUT_WATERDAY_PCT_DOCS_CSV, na = "")
readr::write_csv(monthly_context, OUT_MONTHLY_CTX_DOCS_CSV, na = "")
readr::write_csv(prior_wy_fallback_traces, OUT_PRIOR_WY_FALLBACK_TRACES_DOCS_CSV, na = "")
readr::write_csv(normal_medians, OUT_NORMAL_MEDIANS_DOCS_CSV, na = "")

readr::write_csv(run_summary, OUT_RUN_QA_CSV, na = "")
readr::write_csv(station_summary, OUT_STATION_QA_CSV, na = "")

message("\nSnow-pillow historical context build summary:")
print(run_summary, n = Inf)

message("\nStation-history provider summary:")
print(
  station_summary |>
    dplyr::group_by(.data$live_provider_key, .data$provider) |>
    dplyr::summarise(
      stations = dplyr::n(),
      daily_rows = sum(.data$daily_rows, na.rm = TRUE),
      min_first_wy = min(.data$first_water_year, na.rm = TRUE),
      max_last_wy = max(.data$last_water_year, na.rm = TRUE),
      median_years = stats::median(.data$n_water_years, na.rm = TRUE),
      .groups = "drop"
    ),
  n = Inf
)

message("\nWrote snow-pillow historical context products:")
message("  ", OUT_WATERDAY_PCT_INPUT_CSV)
message("  ", OUT_MONTHLY_CTX_INPUT_CSV)
message("  ", OUT_WATERDAY_PCT_DOCS_CSV)
message("  ", OUT_MONTHLY_CTX_DOCS_CSV)
message("  ", OUT_PRIOR_WY_FALLBACK_TRACES_INPUT_CSV)
message("  ", OUT_NORMAL_MEDIANS_INPUT_CSV)
message("  ", OUT_PRIOR_WY_FALLBACK_TRACES_DOCS_CSV)
message("  ", OUT_NORMAL_MEDIANS_DOCS_CSV)
message("\nDone: snow-pillow historical context build complete.")
