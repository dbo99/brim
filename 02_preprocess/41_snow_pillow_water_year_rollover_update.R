# ==== 41_snow_pillow_water_year_rollover_update.R ==========================
## SWE027 - Snow pillow / SWE water-year rollover helper
##
## PURPOSE
##   This script is an intentionally conservative, well-commented helper for
##   the once-per-water-year SWE maintenance task. It is meant for "October me"
##   when the details of the SWE pipeline are no longer fresh.
##
##   The goal at water-year rollover is to add the just-completed water year
##   into BRIM's historical/statistical SWE products while keeping the new
##   current water year out of the reference percentile/median calculations.
##
##   Example at Oct 2026:
##     - current WY becomes WY2027
##     - just-completed WY is WY2026
##     - fixed median remains WY1991-WY2020
##     - rolling 30 complete-WY median becomes WY1997-WY2026
##     - Plot B daily percentile ribbons and monthly context are rebuilt with
##       WY2026 included and WY2027 excluded.
##
## SAFETY DESIGN
##   This script defaults to DRY RUN. It does not run heavy rebuilds unless you
##   explicitly allow it. It mostly prints exactly what should happen and what
##   files should be uploaded.
##
##   To run only checks / print instructions:
##     source("02_preprocess/41_snow_pillow_water_year_rollover_update.R")
##
##   To run the historical context rebuild from this helper:
##     Sys.setenv(SNOW_PILLOW_ROLLOVER_RUN_REBUILD = "true")
##     source("02_preprocess/41_snow_pillow_water_year_rollover_update.R")
##     Sys.unsetenv("SNOW_PILLOW_ROLLOVER_RUN_REBUILD")
##
##   Provider-history refresh is NOT automatic here. By default the underlying
##   SWE026 history builder uses RDS-first / auto mode, reusing the selected
##   daily-history RDS when it already covers the just-completed WY. At real
##   rollover, if the RDS does not cover the just-completed WY, you can decide
##   whether to run provider mode deliberately.
##
## RELATED SCRIPTS
##   02_preprocess/39_build_snow_pillow_history_context.r
##       Builds daily percentile ribbons, monthly context, fallback traces,
##       and the fixed/rolling median product.
##
##   brim-live-data-feeds/scripts/build_snow_pillow_latest.R
##       GitHub Action latest/current-WY feed. Run after context products are
##       uploaded so latest GeoJSON can join any new median/context fields.
##
##   02_preprocess/40_audit_snow_pillow_om_readiness.R
##       O&M audit. Run after rollover rebuild and latest Action.
## ============================================================================

# ---- 1. Project-root sanity check -----------------------------------------

if (!dir.exists("00_config") || !dir.exists("brim-live-data-feeds")) {
  stop(
    "Run this script from the PortaTreasure2 / BRIM project root.\n",
    "Expected folders 00_config/ and brim-live-data-feeds/ were not found.\n",
    "Try: setwd('C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2')"
  )
}

source("00_config/config_paths.r")

required_pkgs <- c("dplyr", "readr", "tibble", "lubridate", "jsonlite")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing required R packages: ", paste(missing_pkgs, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(lubridate)
  library(jsonlite)
})

# ---- 2. User-facing switches ----------------------------------------------

LOCAL_TZ <- "America/Los_Angeles"
TODAY_LOCAL <- as.Date(lubridate::with_tz(Sys.time(), LOCAL_TZ))

current_water_year <- function(date = TODAY_LOCAL) {
  yr <- as.integer(format(as.Date(date), "%Y"))
  mo <- as.integer(format(as.Date(date), "%m"))
  if (mo >= 10L) yr + 1L else yr
}

CURRENT_WY <- current_water_year(TODAY_LOCAL)
JUST_COMPLETED_WY <- CURRENT_WY - 1L
JUST_COMPLETED_WY_END_DATE <- as.Date(sprintf("%d-09-30", JUST_COMPLETED_WY))

## Fixed reference window for the familiar 30-year normal-style median.
## This intentionally does NOT roll each year.
FIXED_NORMAL_START_WY <- 1991L
FIXED_NORMAL_END_WY <- 2020L

## Rolling window uses the last 30 complete water years.
ROLLING_NORMAL_END_WY <- JUST_COMPLETED_WY
ROLLING_NORMAL_START_WY <- ROLLING_NORMAL_END_WY - 29L

## DRY RUN by default. Set SNOW_PILLOW_ROLLOVER_RUN_REBUILD=true only when you
## actually want this helper to call build_snow_pillow_history_context().
RUN_REBUILD <- tolower(Sys.getenv("SNOW_PILLOW_ROLLOVER_RUN_REBUILD", unset = "false")) %in% c("true", "t", "1", "yes", "y")

## Source mode passed to SWE026 history builder when RUN_REBUILD is TRUE.
## - auto: prefer existing selected daily-history RDS if it covers the needed
##         complete WY; otherwise fall back to provider/cache fetch mode.
## - rds:  require RDS reuse; fail if RDS does not cover the needed period.
## - provider: force provider/cache fetch mode deliberately.
HISTORY_SOURCE_MODE <- Sys.getenv("SNOW_PILLOW_ROLLOVER_HISTORY_SOURCE_MODE", unset = "auto")

# ---- 3. Paths ---------------------------------------------------------------

LIVE_REPO_DIR <- file.path(DIR$root, "brim-live-data-feeds")
LIVE_INPUT_DIR <- file.path(LIVE_REPO_DIR, "data", "input")
LIVE_DOCS_DATA_DIR <- file.path(LIVE_REPO_DIR, "docs", "data")

DAILY_HISTORY_RDS <- file.path(DIR$rds, "snow_pillow_swe_daily_history.rds")
RUN_SUMMARY_CSV <- file.path(DIR$qa, "snow_pillow_history_context_run_summary.csv")

CONTEXT_FILES <- tibble::tribble(
  ~product, ~input_path, ~docs_path,
  "daily water-day percentiles",
  file.path(LIVE_INPUT_DIR, "snow_pillow_swe_waterday_percentiles.csv"),
  file.path(LIVE_DOCS_DATA_DIR, "snow_pillow_swe_waterday_percentiles.csv"),
  "monthly context",
  file.path(LIVE_INPUT_DIR, "snow_pillow_swe_monthly_context.csv"),
  file.path(LIVE_DOCS_DATA_DIR, "snow_pillow_swe_monthly_context.csv"),
  "prior-WY fallback traces",
  file.path(LIVE_INPUT_DIR, "snow_pillow_swe_prior_wy_fallback_traces.csv"),
  file.path(LIVE_DOCS_DATA_DIR, "snow_pillow_swe_prior_wy_fallback_traces.csv"),
  "normal medians",
  file.path(LIVE_INPUT_DIR, "snow_pillow_swe_normal_medians.csv"),
  file.path(LIVE_DOCS_DATA_DIR, "snow_pillow_swe_normal_medians.csv")
)

# ---- 4. Lightweight checks --------------------------------------------------

cat("\n============================================================\n")
cat("SWE027 - snow pillow / SWE water-year rollover helper\n")
cat("============================================================\n")
cat("Today local: ", as.character(TODAY_LOCAL), "\n", sep = "")
cat("Current water year: WY", CURRENT_WY, "\n", sep = "")
cat("Just-completed water year: WY", JUST_COMPLETED_WY, "\n", sep = "")
cat("Just-completed WY end date: ", as.character(JUST_COMPLETED_WY_END_DATE), "\n", sep = "")
cat("Fixed median window: WY", FIXED_NORMAL_START_WY, "-WY", FIXED_NORMAL_END_WY, "\n", sep = "")
cat("Rolling 30 complete-WY median window should be: WY", ROLLING_NORMAL_START_WY, "-WY", ROLLING_NORMAL_END_WY, "\n", sep = "")
cat("Run rebuild now: ", RUN_REBUILD, "\n", sep = "")
cat("Requested history source mode if rebuilding: ", HISTORY_SOURCE_MODE, "\n", sep = "")

# ---- 4.1 Existing selected daily-history RDS coverage ----------------------

rds_status <- tibble::tibble(
  rds_path = DAILY_HISTORY_RDS,
  exists = file.exists(DAILY_HISTORY_RDS),
  min_date = as.Date(NA),
  max_date = as.Date(NA),
  covers_just_completed_wy = FALSE,
  rows = NA_integer_,
  stations = NA_integer_
)

if (file.exists(DAILY_HISTORY_RDS)) {
  hist <- readRDS(DAILY_HISTORY_RDS)
  if (is.data.frame(hist) && "obs_date" %in% names(hist)) {
    rds_status$min_date <- min(as.Date(hist$obs_date), na.rm = TRUE)
    rds_status$max_date <- max(as.Date(hist$obs_date), na.rm = TRUE)
    rds_status$covers_just_completed_wy <- !is.na(rds_status$max_date) && rds_status$max_date >= JUST_COMPLETED_WY_END_DATE
    rds_status$rows <- nrow(hist)
    if ("station_uid" %in% names(hist)) {
      rds_status$stations <- dplyr::n_distinct(hist$station_uid)
    }
  }
}

cat("\n==== Selected daily-history RDS coverage ====\n")
print(rds_status, width = Inf)

if (!rds_status$exists) {
  cat("\nNOTE: selected daily-history RDS does not exist. A provider/cache fetch will be needed before rollover products can be rebuilt.\n")
} else if (!isTRUE(rds_status$covers_just_completed_wy)) {
  cat("\nNOTE: selected daily-history RDS does NOT cover the just-completed water year end date.\n")
  cat("      At rollover, run provider/cache refresh deliberately, then rerun this helper/audit.\n")
} else {
  cat("\nOK: selected daily-history RDS covers the just-completed WY end date.\n")
  cat("    Derived products can be rebuilt from RDS-first/auto mode without deep provider refetch.\n")
}

# ---- 4.2 Existing context product status -----------------------------------

context_status <- CONTEXT_FILES |>
  dplyr::mutate(
    input_exists = file.exists(.data$input_path),
    docs_exists = file.exists(.data$docs_path),
    input_mtime = dplyr::if_else(.data$input_exists, as.character(file.info(.data$input_path)$mtime), NA_character_),
    docs_mtime = dplyr::if_else(.data$docs_exists, as.character(file.info(.data$docs_path)$mtime), NA_character_),
    input_size_mb = dplyr::if_else(.data$input_exists, round(file.info(.data$input_path)$size / 1024^2, 3), NA_real_),
    docs_size_mb = dplyr::if_else(.data$docs_exists, round(file.info(.data$docs_path)$size / 1024^2, 3), NA_real_)
  )

cat("\n==== Existing context product files ====\n")
print(context_status, n = Inf, width = Inf)

# ---- 5. Print October checklist --------------------------------------------

rollover_checklist <- tibble::tribble(
  ~step_order, ~task, ~why,
  1, "Confirm current WY advanced after Oct 1", "Latest feeds should begin a new current-WY trace automatically.",
  2, "Decide RDS-first vs provider refresh", "If selected RDS covers the just-completed WY, rebuild derived products from RDS. If not, refresh history deliberately.",
  3, "Run SWE historical context rebuild", "Adds the completed WY into Plot B ribbons, Plot A monthly context, fallback traces, and rolling 30-WY median product while excluding the new current WY.",
  4, "Upload context CSVs to both data/input and docs/data", "Keep repo inputs and browser-hosted files identical.",
  5, "Run SWE latest GitHub Action", "Refresh latest GeoJSON/current-WY trace and join the newest median/context fields.",
  6, "Run SWE024/SWE O&M audit", "Verify docs/input consistency, source fields, stale-delta suppression, normal labels, and row counts.",
  7, "Spot-check BRIM", "Check CDEC #82 station, NRCS station, sparse-history station, stale station, fallback/no-context station, and one high-SWE station."
)

cat("\n==== Water-year rollover checklist ====\n")
print(rollover_checklist, n = Inf, width = Inf)

cat("\n==== Files to upload after a successful rollover rebuild ====\n")
cat("brim-live-data-feeds/data/input/snow_pillow_swe_waterday_percentiles.csv\n")
cat("brim-live-data-feeds/data/input/snow_pillow_swe_monthly_context.csv\n")
cat("brim-live-data-feeds/data/input/snow_pillow_swe_prior_wy_fallback_traces.csv\n")
cat("brim-live-data-feeds/data/input/snow_pillow_swe_normal_medians.csv\n")
cat("brim-live-data-feeds/docs/data/snow_pillow_swe_waterday_percentiles.csv\n")
cat("brim-live-data-feeds/docs/data/snow_pillow_swe_monthly_context.csv\n")
cat("brim-live-data-feeds/docs/data/snow_pillow_swe_prior_wy_fallback_traces.csv\n")
cat("brim-live-data-feeds/docs/data/snow_pillow_swe_normal_medians.csv\n")

# ---- 6. Optional rebuild ----------------------------------------------------

if (RUN_REBUILD) {
  cat("\n============================================================\n")
  cat("RUN_REBUILD is TRUE: running SWE historical context builder\n")
  cat("============================================================\n")
  cat("This will call build_snow_pillow_history_context() through run_build_map.r.\n")
  cat("History source mode: ", HISTORY_SOURCE_MODE, "\n", sep = "")

  old_mode <- Sys.getenv("SNOW_PILLOW_HISTORY_SOURCE_MODE", unset = NA_character_)
  old_refresh <- Sys.getenv("SNOW_PILLOW_HISTORY_REFRESH_CACHE", unset = NA_character_)

  on.exit({
    if (is.na(old_mode)) Sys.unsetenv("SNOW_PILLOW_HISTORY_SOURCE_MODE") else Sys.setenv(SNOW_PILLOW_HISTORY_SOURCE_MODE = old_mode)
    if (is.na(old_refresh)) Sys.unsetenv("SNOW_PILLOW_HISTORY_REFRESH_CACHE") else Sys.setenv(SNOW_PILLOW_HISTORY_REFRESH_CACHE = old_refresh)
  }, add = TRUE)

  Sys.setenv(SNOW_PILLOW_HISTORY_SOURCE_MODE = HISTORY_SOURCE_MODE)
  Sys.setenv(SNOW_PILLOW_HISTORY_REFRESH_CACHE = "false")

  source("run_build_map.r")
  build_snow_pillow_history_context()

  if (file.exists(RUN_SUMMARY_CSV)) {
    cat("\n==== Rebuild summary source/cache fields ====\n")
    readr::read_csv(RUN_SUMMARY_CSV, show_col_types = FALSE) |>
      dplyr::filter(grepl("history_source|rds|prior_complete|normal_", .data$metric)) |>
      print(n = Inf, width = Inf)
  }
} else {
  cat("\nDRY RUN ONLY. No rebuild was run.\n")
  cat("To run the rebuild from this helper, set:\n")
  cat("  Sys.setenv(SNOW_PILLOW_ROLLOVER_RUN_REBUILD = 'true')\n")
  cat("Optional source mode override:\n")
  cat("  Sys.setenv(SNOW_PILLOW_ROLLOVER_HISTORY_SOURCE_MODE = 'auto')  # default\n")
  cat("  Sys.setenv(SNOW_PILLOW_ROLLOVER_HISTORY_SOURCE_MODE = 'rds')   # require RDS reuse\n")
  cat("  Sys.setenv(SNOW_PILLOW_ROLLOVER_HISTORY_SOURCE_MODE = 'provider') # deliberate provider refresh\n")
}

cat("\nDone: SWE027 water-year rollover helper complete.\n")
