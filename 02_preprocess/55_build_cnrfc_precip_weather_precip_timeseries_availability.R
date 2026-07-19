# ==== 55_build_cnrfc_precip_weather_precip_timeseries_availability.R =========
##
## PURPOSE:
##   Start a lightweight, cached mini-audit for whether CNRFC/NWS/WRH station
##   time-series pages expose a precipitation / accumulated-precipitation option.
##
## IMPORTANT:
##   This is intentionally separate from the main station-link audit in 54_.
##   The first goal is a diagnostic, station-by-station evidence table; do not
##   wire these fields into the map until the signals are reviewed.
##
## OUTPUTS:
##   04_processed_data/rds/cnrfc_precip_weather_precip_timeseries_probe.rds
##   04_processed_data/rds/cnrfc_precip_weather_precip_timeseries_matrix.rds
##   04_processed_data/rds/cnrfc_precip_weather_precip_timeseries_summary.rds
##
##   CSV copies are written to 04_processed_data/qa/ with *_latest.csv and
##   timestamped names.  Archive CSVs are also written to:
##   04_processed_data/qa/cnrfc_precip_weather_precip_timeseries_archive/
## ============================================================================

if (!dir.exists("00_config") || !dir.exists("04_processed_data")) {
  stop(
    "Run this script from the BRIM project root. Expected folders like ",
    "00_config/ and 04_processed_data/ were not found."
  )
}

source("00_config/config_paths.r")
source("03_functions/cache_helpers.r")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(stringr)
  library(curl)
})

RUN_TS <- make_timestamp()
PRECIP_TS_SCHEMA_VERSION <- "20260625_patch041f_precip_timeseries_option_probe_v1"

PT_PRECIP_OPTION_PATTERNS <- c(
  accumulated_precipitation = "accumulated[[:space:]_-]*precipitation",
  precipitation = "\\bprecipitation\\b",
  one_hour_precip = "one[[:space:]_-]*hour[[:space:]_-]*precip",
  hourly_precip = "hourly[[:space:]_-]*precip",
  precip_abbrev = "\\bprecip\\b|\\bpcpn\\b|\\bprcp\\b",
  rainfall = "\\brainfall\\b|\\brain\\b"
)

PT_WRH_NEGATIVE_PATTERNS <- c(
  invalid_station = "invalid station|not a valid station",
  station_not_found = "no station found|station[^.]{0,80}not found|unable to find station",
  unknown_station = "unknown station"
)

dir.create(DIR$rds, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR$qa, showWarnings = FALSE, recursive = TRUE)

pt_log <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " | ", ...)
}

pt_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

pt_clean_id <- function(x) {
  x <- toupper(pt_chr(x))
  x[x %in% c("", "NA", "N/A", "NULL", "NONE", "<BLANK>")] <- ""
  x
}

pt_status_ok <- function(status_code) {
  !is.na(status_code) && status_code >= 200L && status_code < 400L
}

pt_cache_fresh <- function(dat, max_age_days) {
  if (!"probe_timestamp" %in% names(dat)) return(rep(FALSE, nrow(dat)))
  stamp <- suppressWarnings(as.POSIXct(dat$probe_timestamp, tz = "UTC"))
  !is.na(stamp) & as.numeric(difftime(Sys.time(), stamp, units = "days")) <= max_age_days
}

pt_match_names <- function(text_low, named_patterns) {
  hits <- names(named_patterns)[vapply(named_patterns, function(pat) {
    grepl(pat, text_low, ignore.case = TRUE, perl = TRUE)
  }, logical(1))]
  paste(hits, collapse = "; ")
}

pt_text_excerpt <- function(text, max_chars = 360L) {
  text <- gsub("\\s+", " ", pt_chr(text))
  if (!nzchar(text)) return("")
  substr(text, 1L, min(nchar(text), max_chars))
}

pt_write_csv_pair <- function(x, stem) {
  ts_path <- file.path(DIR$qa, paste0(stem, "_", RUN_TS, ".csv"))
  latest_path <- file.path(DIR$qa, paste0(stem, "_latest.csv"))
  readr::write_csv(x, ts_path, na = "")
  readr::write_csv(x, latest_path, na = "")
  invisible(list(timestamped = ts_path, latest = latest_path))
}

pt_write_csv_archive <- function(x, stem, archive_subdir = "cnrfc_precip_weather_precip_timeseries_archive") {
  archive_dir <- file.path(DIR$qa, archive_subdir)
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  archive_path <- file.path(archive_dir, paste0(stem, "_", RUN_TS, ".csv"))
  readr::write_csv(x, archive_path, na = "")
  invisible(archive_path)
}

pt_write_rds <- function(x, filename) {
  out <- file.path(DIR$rds, filename)
  saveRDS(x, out)
  invisible(out)
}

pt_read_csv_or_empty <- function(path) {
  if (!file.exists(path)) return(tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

pt_fetch_text <- function(url, timeout_sec = 20L) {
  url <- pt_chr(url)
  if (!nzchar(url)) {
    return(list(status_code = NA_integer_, bytes = NA_integer_, text = "", error = "blank_url"))
  }

  h <- curl::new_handle(
    useragent = "BRIM CNRFC precip time-series option audit; BLM California hydrology",
    timeout = timeout_sec,
    followlocation = TRUE
  )

  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
  if (inherits(res, "error")) {
    return(list(status_code = NA_integer_, bytes = NA_integer_, text = "", error = conditionMessage(res)))
  }

  txt <- tryCatch(rawToChar(res$content), error = function(e) "")
  txt <- iconv(txt, from = "", to = "UTF-8", sub = "byte")

  list(
    status_code = res$status_code,
    bytes = length(res$content),
    text = txt,
    error = NA_character_
  )
}

pt_detect_precip_option <- function(status_code, text) {
  text_low <- tolower(pt_chr(text))
  positive <- pt_match_names(text_low, PT_PRECIP_OPTION_PATTERNS)
  negative <- pt_match_names(text_low, PT_WRH_NEGATIVE_PATTERNS)

  klass <- dplyr::case_when(
    !pt_status_ok(status_code) ~ "http_not_ok",
    nzchar(negative) ~ "negative_or_invalid_station_text",
    nzchar(positive) ~ "static_precip_text_detected_review_needed",
    TRUE ~ "static_precip_text_not_detected"
  )

  list(
    class = klass,
    positive_signals = positive,
    negative_signals = negative,
    excerpt = pt_text_excerpt(text)
  )
}

pt_best_precip_class <- function(main_class, tabular_class, low_class) {
  vals <- c(main_class, tabular_class, low_class)
  vals <- vals[!is.na(vals) & nzchar(vals)]
  dplyr::case_when(
    any(vals == "static_precip_text_detected_review_needed") ~ "static_precip_text_detected_review_needed",
    any(vals == "negative_or_invalid_station_text") ~ "negative_or_invalid_station_text",
    any(vals == "static_precip_text_not_detected") ~ "static_precip_text_not_detected",
    any(vals == "http_not_ok") ~ "http_not_ok",
    TRUE ~ "not_checked"
  )
}

pt_load_ops_station_input <- function() {
  preferred <- file.path(DIR$rds, "cnrfc_precip_weather_noaa_timeseries_ops_map.rds")
  fallback <- file.path(DIR$rds, "cnrfc_precip_weather_stations_map.rds")

  path <- if (file.exists(preferred)) preferred else fallback
  if (!file.exists(path)) {
    stop(
      "Missing Ops time-series station RDS. Run 54_build_cnrfc_precip_weather_link_availability.R first."
    )
  }

  readRDS(path) %>%
    mutate(
      cnrfc_id = pt_clean_id(.data$cnrfc_id),
      wrh_site_id = pt_clean_id(.data$wrh_site_id)
    )
}

pt_select_precip_probe_ids <- function(stations, cache, max_live_checks, force_refresh, force_refresh_ids, cache_max_age_days) {
  force_refresh_ids <- unique(pt_clean_id(force_refresh_ids))
  cache_fresh_ids <- character(0)

  if (nrow(cache) > 0 && !force_refresh) {
    cache <- cache %>% mutate(probe_is_fresh = pt_cache_fresh(., cache_max_age_days))
    cache_fresh_ids <- unique(pt_clean_id(cache$cnrfc_id[cache$probe_is_fresh]))
  }

  target <- stations %>%
    mutate(
      force_refresh_target = .data$cnrfc_id %in% force_refresh_ids | .data$wrh_site_id %in% force_refresh_ids,
      already_fresh = .data$cnrfc_id %in% cache_fresh_ids
    ) %>%
    filter(.data$force_refresh_target | !.data$already_fresh) %>%
    arrange(.data$source_class, .data$cnrfc_id)

  if (is.finite(max_live_checks)) {
    target <- target %>% head(as.integer(max_live_checks))
  }

  target$cnrfc_id
}

pt_probe_precip_station <- function(row, timeout_sec = 20L, request_delay_sec = 0.05) {
  cnrfc_id <- row$cnrfc_id[[1]]
  wrh_site_id <- row$wrh_site_id[[1]]
  pt_log("WRH precip option probe: ", cnrfc_id, " -> ", wrh_site_id)

  main <- pt_fetch_text(row$wrh_timeseries_url[[1]], timeout_sec = timeout_sec)
  main_diag <- pt_detect_precip_option(main$status_code, main$text)

  tabular_url <- if ("wrh_timeseries_tabular_url" %in% names(row)) row$wrh_timeseries_tabular_url[[1]] else ""
  tabular <- pt_fetch_text(tabular_url, timeout_sec = timeout_sec)
  tabular_diag <- pt_detect_precip_option(tabular$status_code, tabular$text)

  low_url <- if ("wrh_low_timeseries_url" %in% names(row)) row$wrh_low_timeseries_url[[1]] else ""
  low <- pt_fetch_text(low_url, timeout_sec = timeout_sec)
  low_diag <- pt_detect_precip_option(low$status_code, low$text)

  best_class <- pt_best_precip_class(main_diag$class, tabular_diag$class, low_diag$class)

  if (request_delay_sec > 0) Sys.sleep(request_delay_sec)

  tibble(
    cnrfc_id = cnrfc_id,
    wrh_site_id = wrh_site_id,
    source_class = row$source_class[[1]],
    wrh_timeseries_url = row$wrh_timeseries_url[[1]],
    wrh_timeseries_tabular_url = tabular_url,
    wrh_low_timeseries_url = low_url,
    wrh_precip_main_status_code = main$status_code,
    wrh_precip_main_bytes = main$bytes,
    wrh_precip_main_class = main_diag$class,
    wrh_precip_main_positive_signals = main_diag$positive_signals,
    wrh_precip_main_negative_signals = main_diag$negative_signals,
    wrh_precip_main_excerpt = main_diag$excerpt,
    wrh_precip_tabular_status_code = tabular$status_code,
    wrh_precip_tabular_bytes = tabular$bytes,
    wrh_precip_tabular_class = tabular_diag$class,
    wrh_precip_tabular_positive_signals = tabular_diag$positive_signals,
    wrh_precip_tabular_negative_signals = tabular_diag$negative_signals,
    wrh_precip_tabular_excerpt = tabular_diag$excerpt,
    wrh_precip_low_status_code = low$status_code,
    wrh_precip_low_bytes = low$bytes,
    wrh_precip_low_class = low_diag$class,
    wrh_precip_low_positive_signals = low_diag$positive_signals,
    wrh_precip_low_negative_signals = low_diag$negative_signals,
    wrh_precip_low_excerpt = low_diag$excerpt,
    wrh_has_precip_timeseries_static_class = best_class,
    wrh_has_precip_timeseries = dplyr::case_when(
      best_class == "static_precip_text_detected_review_needed" ~ TRUE,
      best_class %in% c("static_precip_text_not_detected", "negative_or_invalid_station_text") ~ FALSE,
      TRUE ~ NA
    ),
    wrh_precip_probe_caveat = "WRH pages are browser-driven; static precip text is diagnostic and must be reviewed before map filtering.",
    probe_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    probe_schema_version = PRECIP_TS_SCHEMA_VERSION,
    run_timestamp = RUN_TS
  )
}

# ---- Options ----------------------------------------------------------------

MAX_LIVE_CHECKS <- getOption("BRIM_CNRFC_PRECIP_TS_MAX_LIVE_CHECKS", 30L)
FORCE_REFRESH <- isTRUE(getOption("BRIM_CNRFC_PRECIP_TS_FORCE_REFRESH", FALSE))
FORCE_REFRESH_IDS <- getOption("BRIM_CNRFC_PRECIP_TS_FORCE_REFRESH_IDS", character(0))
CACHE_MAX_AGE_DAYS <- getOption("BRIM_CNRFC_PRECIP_TS_CACHE_MAX_AGE_DAYS", 30L)
REQUEST_DELAY_SEC <- getOption("BRIM_CNRFC_PRECIP_TS_REQUEST_DELAY_SEC", 0.05)
TIMEOUT_SEC <- getOption("BRIM_CNRFC_PRECIP_TS_TIMEOUT_SEC", 20L)

pt_log("CNRFC precip time-series option mini-audit options:")
pt_log("  max_live_checks   = ", MAX_LIVE_CHECKS)
pt_log("  force_refresh     = ", FORCE_REFRESH)
pt_log("  force_refresh_ids = ", paste(pt_clean_id(FORCE_REFRESH_IDS), collapse = ", "))

# ---- Load verified Ops stations and cache -----------------------------------

stations <- pt_load_ops_station_input()
pt_log("Verified Ops time-series station rows: ", nrow(stations))

cache_path <- file.path(DIR$qa, "cnrfc_precip_weather_precip_timeseries_probe_cache.csv")
cache <- pt_read_csv_or_empty(cache_path)
if (nrow(cache) > 0) {
  cache <- cache %>% mutate(cnrfc_id = pt_clean_id(.data$cnrfc_id))
}

probe_ids <- pt_select_precip_probe_ids(
  stations = stations,
  cache = cache,
  max_live_checks = MAX_LIVE_CHECKS,
  force_refresh = FORCE_REFRESH,
  force_refresh_ids = FORCE_REFRESH_IDS,
  cache_max_age_days = CACHE_MAX_AGE_DAYS
)
pt_log("WRH precip option probe IDs selected this run: ", length(probe_ids))

probe_new <- tibble()
if (length(probe_ids) > 0) {
  rows <- stations %>% filter(.data$cnrfc_id %in% probe_ids)
  probe_new <- bind_rows(lapply(seq_len(nrow(rows)), function(i) {
    pt_probe_precip_station(rows[i, ], timeout_sec = TIMEOUT_SEC, request_delay_sec = REQUEST_DELAY_SEC)
  }))
}

cache_base <- if (nrow(cache) > 0) cache else tibble()
cache_updated <- bind_rows(cache_base, probe_new) %>%
  mutate(cnrfc_id = pt_clean_id(.data$cnrfc_id)) %>%
  arrange(desc(.data$probe_timestamp)) %>%
  distinct(.data$cnrfc_id, .keep_all = TRUE)

readr::write_csv(cache_updated, cache_path, na = "")

probe_current <- cache_updated %>%
  mutate(probe_is_fresh = pt_cache_fresh(., CACHE_MAX_AGE_DAYS))

matrix <- stations %>%
  left_join(
    probe_current %>% select(any_of(c(
      "cnrfc_id", "wrh_has_precip_timeseries", "wrh_has_precip_timeseries_static_class",
      "wrh_precip_main_status_code", "wrh_precip_main_class", "wrh_precip_main_positive_signals",
      "wrh_precip_tabular_status_code", "wrh_precip_tabular_class", "wrh_precip_tabular_positive_signals",
      "wrh_precip_low_status_code", "wrh_precip_low_class", "wrh_precip_low_positive_signals",
      "wrh_precip_probe_caveat", "probe_timestamp", "probe_is_fresh", "probe_schema_version"
    ))),
    by = "cnrfc_id"
  ) %>%
  mutate(run_timestamp = RUN_TS)

bins <- matrix %>%
  count(.data$source_class, .data$wrh_has_precip_timeseries_static_class, .data$wrh_has_precip_timeseries, name = "stations") %>%
  arrange(.data$source_class, desc(.data$stations)) %>%
  mutate(run_timestamp = RUN_TS)

summary <- tibble(
  run_timestamp = RUN_TS,
  metric = c(
    "verified_ops_station_rows",
    "probe_ids_selected_this_run",
    "precip_option_cache_rows_after_update",
    "fresh_precip_option_probe_rows",
    "static_precip_text_detected_review_needed",
    "static_precip_text_not_detected",
    "precip_probe_http_not_ok_or_negative"
  ),
  value = c(
    nrow(stations),
    length(probe_ids),
    nrow(cache_updated),
    sum(probe_current$probe_is_fresh, na.rm = TRUE),
    sum(matrix$wrh_has_precip_timeseries_static_class == "static_precip_text_detected_review_needed", na.rm = TRUE),
    sum(matrix$wrh_has_precip_timeseries_static_class == "static_precip_text_not_detected", na.rm = TRUE),
    sum(matrix$wrh_has_precip_timeseries_static_class %in% c("http_not_ok", "negative_or_invalid_station_text"), na.rm = TRUE)
  )
)

probe_rds <- pt_write_rds(probe_current, "cnrfc_precip_weather_precip_timeseries_probe.rds")
matrix_rds <- pt_write_rds(matrix, "cnrfc_precip_weather_precip_timeseries_matrix.rds")
bins_rds <- pt_write_rds(bins, "cnrfc_precip_weather_precip_timeseries_bins.rds")
summary_rds <- pt_write_rds(summary, "cnrfc_precip_weather_precip_timeseries_summary.rds")

pt_write_csv_pair(probe_current, "cnrfc_precip_weather_precip_timeseries_probe")
pt_write_csv_pair(matrix, "cnrfc_precip_weather_precip_timeseries_matrix")
pt_write_csv_pair(bins, "cnrfc_precip_weather_precip_timeseries_bins")
pt_write_csv_pair(summary, "cnrfc_precip_weather_precip_timeseries_summary")

pt_write_csv_archive(probe_current, "cnrfc_precip_weather_precip_timeseries_probe")
pt_write_csv_archive(matrix, "cnrfc_precip_weather_precip_timeseries_matrix")

pt_log("CNRFC precip time-series option mini-audit complete.")
message("  Probe RDS:  ", probe_rds, " (", nrow(probe_current), " rows)")
message("  Matrix RDS: ", matrix_rds, " (", nrow(matrix), " rows)")
message("  Bins RDS:   ", bins_rds, " (", nrow(bins), " rows)")
message("  Summary RDS:", summary_rds, " (", nrow(summary), " rows)")
message("  Archive folder: ", file.path(DIR$qa, "cnrfc_precip_weather_precip_timeseries_archive"))

message("\nPrecip time-series option bins:")
print(bins, n = 30, width = Inf)

message("\nPrecip time-series option summary:")
print(summary, n = 30, width = Inf)

message("\nInterpretation note:")
message("  This is a diagnostic mini-audit only. WRH pages are browser-driven, so static precip text may be generic, under-detected, or over-detected.")
message("  Review cnrfc_precip_weather_precip_timeseries_matrix_latest.csv before using wrh_has_precip_timeseries as a map filter.")
