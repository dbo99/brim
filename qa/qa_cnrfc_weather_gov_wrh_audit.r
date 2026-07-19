# ==== qa_cnrfc_weather_gov_wrh_audit.r =====================================
##
## PURPOSE:
##   Extend the CNRFC product-intelligence work with Weather.gov Western
##   Region Headquarters (WRH) Time Series Viewer candidates.
##
## WHY THIS EXISTS:
##   Many CNRFC/NWSID-style locations can be opened in the Weather.gov WRH
##   Time Series Viewer using the URL form:
##     https://www.weather.gov/wrh/timeseries?site=<ID>
##
##   The viewer is useful because one station may expose precip only, while
##   another may expose temp/RH/dewpoint, wind, snow, SWE, water temperature,
##   or other elements.  However, the page itself warns that data availability
##   varies by station, so the first durable BRIM step is to build a candidate
##   table and a small, polite probe table rather than immediately adding every
##   link to production popups.
##
## DESIGN:
##   This script reuses the most recent CNRFC audit outputs by default.  It
##   does not rerun the full CNRFC inventory unless explicitly requested from
##   run_build_map.r:
##     qa_cnrfc_weather_gov_wrh_audit(rebuild_cnrfc_base = TRUE)
##
##   Default live checks are intentionally sampled.  The WRH viewer page is a
##   browser app and may load generic HTML even when site-specific data are not
##   useful, so the live probe is a smoke/triage check.  A later deeper parser
##   can reverse-engineer or use a more direct observation endpoint if needed.
##
## OUTPUTS:
##   04_processed_data/qa/cnrfc_weather_gov_wrh_candidate_catalog_<timestamp>.csv
##   04_processed_data/qa/cnrfc_weather_gov_wrh_candidate_bins_<timestamp>.csv
##   04_processed_data/qa/cnrfc_weather_gov_wrh_live_probe_<timestamp>.csv
##   04_processed_data/qa/cnrfc_weather_gov_wrh_summary_<timestamp>.csv

# ==== 1. Project and package setup ==========================================

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
  library(stringr)
  library(tibble)
  library(purrr)
})

RUN_TS <- make_timestamp()

pt_log <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " | ", ...)
}

pt_chr <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

pt_clean_id <- function(x) {
  x <- toupper(pt_chr(x))
  x[x %in% c("", "NA", "N/A", "NULL", "NONE", "<BLANK>")] <- NA_character_
  x
}

pt_first_nonblank <- function(x) {
  x <- pt_chr(x)
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else x[[1]]
}

pt_first_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[[1]]
}

pt_latest_file <- function(pattern, dir = DIR$qa) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  files[order(file.info(files)$mtime, decreasing = TRUE)][[1]]
}

pt_url_id <- function(id) {
  utils::URLencode(pt_clean_id(id), reserved = TRUE)
}

pt_has_any <- function(x, pattern) {
  grepl(pattern, x, ignore.case = TRUE)
}

# ==== 2. Options from run_build_map.r =======================================

REBUILD_BASE <- isTRUE(getOption("BRIM_CNRFC_WRH_REBUILD_BASE", FALSE))
RUN_LIVE_CHECKS <- isTRUE(getOption("BRIM_CNRFC_WRH_RUN_LIVE_CHECKS", TRUE))
MAX_LIVE_IDS <- suppressWarnings(as.integer(getOption("BRIM_CNRFC_WRH_MAX_LIVE_IDS", 75L)))
if (is.na(MAX_LIVE_IDS) || MAX_LIVE_IDS < 0L) MAX_LIVE_IDS <- 0L
LIVE_IDS <- pt_clean_id(getOption("BRIM_CNRFC_WRH_LIVE_IDS", character()))
LIVE_IDS <- LIVE_IDS[!is.na(LIVE_IDS)]
REQUEST_DELAY_SEC <- suppressWarnings(as.numeric(getOption("BRIM_CNRFC_WRH_REQUEST_DELAY_SEC", 0.15)))
if (is.na(REQUEST_DELAY_SEC) || REQUEST_DELAY_SEC < 0) REQUEST_DELAY_SEC <- 0

pt_log("Weather.gov WRH audit options:")
pt_log("  rebuild_cnrfc_base = ", REBUILD_BASE)
pt_log("  run_live_checks    = ", RUN_LIVE_CHECKS)
pt_log("  max_live_ids       = ", MAX_LIVE_IDS)
pt_log("  request_delay_sec  = ", REQUEST_DELAY_SEC)

# ==== 3. Optional base CNRFC audit rebuild ==================================

if (REBUILD_BASE) {
  pt_log("Rebuilding base CNRFC product-intelligence audit first.")
  source("qa/qa_cnrfc_product_intelligence_audit.r", local = FALSE)
} else {
  pt_log("Skipping full base CNRFC audit; reusing latest existing CNRFC audit CSVs.")
}

feature_master_path <- pt_latest_file("^cnrfc_feature_master_.*\\.csv$")
product_bins_path <- pt_latest_file("^cnrfc_product_group_bins_by_feature_.*\\.csv$")
id_collision_path <- pt_latest_file("^cnrfc_id_collision_check_.*\\.csv$")

if (is.na(feature_master_path) || !file.exists(feature_master_path)) {
  stop(
    "No cnrfc_feature_master_*.csv found in ", DIR$qa, ".\n",
    "Run qa_cnrfc_product_intelligence_audit() first, or call:\n",
    "  qa_cnrfc_weather_gov_wrh_audit(rebuild_cnrfc_base = TRUE)"
  )
}

pt_log("Using feature master: ", feature_master_path)
if (!is.na(product_bins_path)) pt_log("Using product bins:   ", product_bins_path)
if (!is.na(id_collision_path)) pt_log("Using collision file: ", id_collision_path)

feature_master <- readr::read_csv(feature_master_path, show_col_types = FALSE)
product_bins <- if (!is.na(product_bins_path) && file.exists(product_bins_path)) {
  readr::read_csv(product_bins_path, show_col_types = FALSE)
} else {
  tibble()
}

pt_log("Feature master rows loaded: ", nrow(feature_master))

# ==== 4. Build Weather.gov WRH candidate catalog ============================

pt_log("Building unique Weather.gov WRH candidate IDs from CNRFC feature master.")

weather_candidates <- feature_master |>
  mutate(
    cnrfc_id = pt_clean_id(.data$cnrfc_id),
    feature_type = tolower(pt_chr(.data$feature_type)),
    has_valid_coordinate = as.logical(.data$has_valid_coordinate)
  ) |>
  filter(!is.na(.data$cnrfc_id), .data$cnrfc_id != "") |>
  group_by(.data$cnrfc_id) |>
  summarise(
    feature_types = paste(sort(unique(.data$feature_type)), collapse = ";"),
    display_name = pt_first_nonblank(.data$display_name),
    river_name = pt_first_nonblank(.data$river_name),
    location_name = pt_first_nonblank(.data$location_name),
    lat = pt_first_num(.data$lat),
    lon = pt_first_num(.data$lon),
    elev_ft = pt_first_num(.data$elev_ft),
    has_valid_coordinate = any(.data$has_valid_coordinate, na.rm = TRUE),
    inventory_sources = paste(sort(unique(na.omit(.data$inventory_sources))), collapse = ";"),
    feature_rows = n(),
    .groups = "drop"
  ) |>
  mutate(
    has_precip = pt_has_any(.data$feature_types, "precip"),
    has_recent_precip_index = pt_has_any(.data$feature_types, "recent_index"),
    has_precip_cache = pt_has_any(.data$feature_types, "precip_station_cache"),
    has_river_point = pt_has_any(.data$feature_types, "river"),
    has_reservoir = pt_has_any(.data$feature_types, "reservoir"),
    has_basin = pt_has_any(.data$feature_types, "basin"),
    has_existing_reservoir_layer_crosswalk = pt_has_any(.data$feature_types, "brim_reservoir_layer"),
    wrh_timeseries_url = paste0("https://www.weather.gov/wrh/timeseries?site=", pt_url_id(.data$cnrfc_id)),
    nws_api_observations_url = paste0("https://api.weather.gov/stations/", pt_url_id(.data$cnrfc_id), "/observations?limit=1"),
    weather_gov_candidate_bin = case_when(
      .data$has_precip_cache & (.data$has_river_point | .data$has_reservoir | .data$has_basin) ~
        "WRH: precip cache + hydro/basin ID overlap",
      .data$has_precip_cache ~
        "WRH: BRIM CNRFC precip-cache station candidate",
      .data$has_recent_precip_index ~
        "WRH: recent-index precip/station candidate",
      .data$has_precip ~
        "WRH: broad precip/station catalog candidate",
      .data$has_existing_reservoir_layer_crosswalk ~
        "WRH: existing reservoir-layer crosswalk ID",
      .data$has_reservoir ~
        "WRH: reservoir-point ID candidate",
      .data$has_river_point ~
        "WRH: river-point ID candidate",
      .data$has_basin ~
        "WRH: basin-ID candidate, lower confidence",
      TRUE ~
        "WRH: other CNRFC ID candidate"
    ),
    weather_gov_candidate_confidence = case_when(
      .data$has_precip_cache ~ "high: present in BRIM CNRFC precip cache",
      .data$has_recent_precip_index & .data$has_valid_coordinate ~ "medium: recent-index file with valid coordinate",
      .data$has_precip & .data$has_valid_coordinate ~ "medium-low: broad catalog with valid coordinate",
      .data$has_river_point | .data$has_reservoir ~ "medium-low: hydro point ID may load in WRH viewer",
      .data$has_basin ~ "low: basin ID may not represent a WRH station",
      TRUE ~ "unknown"
    ),
    product_group = "weather_gov_wrh_timeseries_candidate",
    product_label = "Weather.gov WRH Time Series Viewer",
    availability_status = "not_checked",
    availability_note = "Candidate URL only. WRH page says data availability varies by station; live/data availability needs separate confirmation.",
    run_timestamp = RUN_TS
  ) |>
  arrange(
    desc(.data$has_precip_cache),
    desc(.data$has_recent_precip_index),
    desc(.data$has_valid_coordinate),
    .data$weather_gov_candidate_bin,
    .data$cnrfc_id
  )

pt_log("Weather.gov WRH candidate IDs: ", nrow(weather_candidates))

# Keep a compact candidate catalog: one row per unique ID, not one row per
# feature-product combination.  This avoids another large 30+ MB CSV.
wrh_candidate_catalog <- weather_candidates |>
  select(
    .data$cnrfc_id,
    .data$display_name,
    .data$river_name,
    .data$location_name,
    .data$lat,
    .data$lon,
    .data$elev_ft,
    .data$has_valid_coordinate,
    .data$feature_types,
    .data$feature_rows,
    .data$weather_gov_candidate_bin,
    .data$weather_gov_candidate_confidence,
    .data$wrh_timeseries_url,
    .data$nws_api_observations_url,
    .data$product_group,
    .data$product_label,
    .data$availability_status,
    .data$availability_note,
    .data$run_timestamp
  )

wrh_candidate_bins <- wrh_candidate_catalog |>
  count(.data$weather_gov_candidate_bin, .data$weather_gov_candidate_confidence, name = "ids", sort = TRUE)

# Optionally merge CNRFC bins with the WRH candidate class for easier review.
wrh_feature_bin_join <- if (nrow(product_bins) > 0) {
  product_bins |>
    left_join(
      wrh_candidate_catalog |>
        select(
          .data$cnrfc_id,
          .data$weather_gov_candidate_bin,
          .data$weather_gov_candidate_confidence,
          .data$wrh_timeseries_url
        ),
      by = "cnrfc_id"
    ) |>
    mutate(run_timestamp = RUN_TS)
} else {
  tibble()
}

# ==== 5. Sampled live/smoke checks ==========================================

pt_fetch_text <- function(url, timeout_sec = 8) {
  if (requireNamespace("curl", quietly = TRUE)) {
    h <- curl::new_handle(
      timeout = timeout_sec,
      followlocation = TRUE,
      useragent = "BRIM CNRFC Weather.gov WRH QA audit (contact: internal BRIM development)"
    )
    res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
    if (inherits(res, "error")) {
      return(list(ok = FALSE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = conditionMessage(res)))
    }
    txt <- rawToChar(res$content)
    return(list(ok = TRUE, status_code = res$status_code, bytes = length(res$content), text = txt, error = NA_character_))
  }

  old_timeout <- getOption("timeout")
  options(timeout = max(timeout_sec, old_timeout %||% 60))
  on.exit(options(timeout = old_timeout), add = TRUE)
  txt <- tryCatch({
    con <- url(url, open = "rb")
    on.exit(close(con), add = TRUE)
    paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, error = function(e) e)

  if (inherits(txt, "error")) {
    return(list(ok = FALSE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = conditionMessage(txt)))
  }
  list(ok = TRUE, status_code = NA_integer_, bytes = nchar(txt, type = "bytes"), text = txt, error = NA_character_)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

pt_classify_wrh_page <- function(txt, status_code) {
  if (!is.na(status_code) && status_code >= 400) return("wrh_viewer_http_error")
  if (!grepl("Time Series Viewer", txt, fixed = TRUE)) return("wrh_viewer_unexpected_page")

  # The WRH viewer can load a normal page shell while still telling the user
  # that the requested site ID is invalid.  Treat this as unavailable, not as
  # a generic page-load success.  Example page text: "BRUC1 is not a valid
  # station identifier."
  if (grepl("not a valid station identifier|invalid station identifier|not a valid station", txt, ignore.case = TRUE)) {
    return("wrh_viewer_invalid_station_identifier")
  }

  if (grepl("Data availability varies by station", txt, fixed = TRUE)) return("wrh_viewer_page_loads_generic_app")
  "wrh_viewer_page_loads"
}

pt_classify_api_obs <- function(txt, status_code) {
  if (!is.na(status_code) && status_code == 404) return("nws_api_station_not_found")
  if (!is.na(status_code) && status_code >= 400) return("nws_api_http_error")
  if (grepl('"features"\\s*:\\s*\\[\\s*\\]', txt)) return("nws_api_station_found_no_recent_observation")
  if (grepl('"features"\\s*:\\s*\\[\\s*\\{', txt)) return("nws_api_recent_observation_available")
  if (grepl('"temperature"', txt) || grepl('"precipitationLastHour"', txt) || grepl('"relativeHumidity"', txt)) return("nws_api_observation_json_like")
  "nws_api_unclassified_response"
}

pt_extract_api_elements <- function(txt) {
  elems <- c(
    temperature = grepl('"temperature"', txt),
    dewpoint = grepl('"dewpoint"', txt),
    relative_humidity = grepl('"relativeHumidity"', txt),
    wind_speed = grepl('"windSpeed"', txt),
    wind_gust = grepl('"windGust"', txt),
    precip_last_hour = grepl('"precipitationLastHour"', txt),
    precip_last_3hr = grepl('"precipitationLast3Hours"', txt),
    precip_last_6hr = grepl('"precipitationLast6Hours"', txt)
  )
  paste(names(elems)[elems], collapse = ";")
}

live_probe <- tibble()

if (RUN_LIVE_CHECKS && MAX_LIVE_IDS > 0L) {
  pt_log("Selecting sampled Weather.gov WRH live-check IDs.")

  explicit <- tibble(cnrfc_id = LIVE_IDS, explicit_order = seq_along(LIVE_IDS)) |>
    filter(!is.na(.data$cnrfc_id), .data$cnrfc_id != "")

  sample_pool <- wrh_candidate_catalog |>
    mutate(
      explicit_order = match(.data$cnrfc_id, explicit$cnrfc_id),
      priority_score = case_when(
        !is.na(.data$explicit_order) ~ .data$explicit_order,
        grepl("precip-cache", .data$weather_gov_candidate_bin, ignore.case = TRUE) ~ 1000L,
        grepl("recent-index", .data$weather_gov_candidate_bin, ignore.case = TRUE) ~ 2000L,
        grepl("broad precip", .data$weather_gov_candidate_bin, ignore.case = TRUE) ~ 3000L,
        grepl("river-point", .data$weather_gov_candidate_bin, ignore.case = TRUE) ~ 4000L,
        grepl("reservoir", .data$weather_gov_candidate_bin, ignore.case = TRUE) ~ 5000L,
        grepl("basin", .data$weather_gov_candidate_bin, ignore.case = TRUE) ~ 9000L,
        TRUE ~ 8000L
      )
    ) |>
    arrange(.data$priority_score, .data$cnrfc_id) |>
    distinct(.data$cnrfc_id, .keep_all = TRUE) |>
    slice_head(n = MAX_LIVE_IDS)

  pt_log("Live-checking ", nrow(sample_pool), " IDs. This is a smoke/triage check, not a final availability parser.")

  live_probe <- purrr::map_dfr(seq_len(nrow(sample_pool)), function(i) {
    row <- sample_pool[i, ]
    id <- row$cnrfc_id[[1]]
    if (i %% 10 == 1 || i == nrow(sample_pool)) {
      pt_log("  live check ", i, "/", nrow(sample_pool), ": ", id)
    }

    wrh <- pt_fetch_text(row$wrh_timeseries_url[[1]], timeout_sec = 8)
    if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
    api <- pt_fetch_text(row$nws_api_observations_url[[1]], timeout_sec = 8)
    if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)

    tibble(
      cnrfc_id = id,
      display_name = row$display_name[[1]],
      weather_gov_candidate_bin = row$weather_gov_candidate_bin[[1]],
      wrh_timeseries_url = row$wrh_timeseries_url[[1]],
      wrh_probe_ok = wrh$ok,
      wrh_status_code = wrh$status_code,
      wrh_bytes = wrh$bytes,
      wrh_probe_class = pt_classify_wrh_page(wrh$text, wrh$status_code),
      wrh_error = wrh$error,
      nws_api_observations_url = row$nws_api_observations_url[[1]],
      nws_api_probe_ok = api$ok,
      nws_api_status_code = api$status_code,
      nws_api_bytes = api$bytes,
      nws_api_probe_class = pt_classify_api_obs(api$text, api$status_code),
      nws_api_elements_seen = pt_extract_api_elements(api$text),
      nws_api_error = api$error,
      live_check_note = "WRH page is a generic viewer app; station-specific table/chart availability may require a deeper parser. NWS API observations are a related structured smoke check, not full WRH equivalence.",
      checked_pacific = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      run_timestamp = RUN_TS
    )
  })
} else {
  pt_log("Skipping live Weather.gov checks. Candidate URL tables will still be written.")
}

# ==== 6. Write outputs =======================================================

out_candidate_catalog <- file.path(DIR$qa, paste0("cnrfc_weather_gov_wrh_candidate_catalog_", RUN_TS, ".csv"))
out_candidate_bins <- file.path(DIR$qa, paste0("cnrfc_weather_gov_wrh_candidate_bins_", RUN_TS, ".csv"))
out_feature_bins_join <- file.path(DIR$qa, paste0("cnrfc_product_bins_plus_weather_gov_wrh_", RUN_TS, ".csv"))
out_live_probe <- file.path(DIR$qa, paste0("cnrfc_weather_gov_wrh_live_probe_", RUN_TS, ".csv"))
out_summary <- file.path(DIR$qa, paste0("cnrfc_weather_gov_wrh_summary_", RUN_TS, ".csv"))

readr::write_csv(wrh_candidate_catalog, out_candidate_catalog)
readr::write_csv(wrh_candidate_bins, out_candidate_bins)
if (nrow(wrh_feature_bin_join) > 0) readr::write_csv(wrh_feature_bin_join, out_feature_bins_join)
readr::write_csv(live_probe, out_live_probe)

summary_rows <- bind_rows(
  tibble(section = "inputs", metric = "feature_master_path", value = feature_master_path),
  tibble(section = "inputs", metric = "product_bins_path", value = product_bins_path %||% NA_character_),
  tibble(section = "counts", metric = "unique_wrh_candidate_ids", value = as.character(nrow(wrh_candidate_catalog))),
  tibble(section = "counts", metric = "live_probe_ids", value = as.character(nrow(live_probe))),
  wrh_candidate_bins |>
    transmute(
      section = "candidate_bins",
      metric = paste(.data$weather_gov_candidate_bin, .data$weather_gov_candidate_confidence, sep = " | "),
      value = as.character(.data$ids)
    ),
  if (nrow(live_probe) > 0) {
    live_probe |>
      count(.data$wrh_probe_class, .data$nws_api_probe_class, name = "ids") |>
      transmute(
        section = "live_probe_classes",
        metric = paste(.data$wrh_probe_class, .data$nws_api_probe_class, sep = " | "),
        value = as.character(.data$ids)
      )
  } else tibble()
)
readr::write_csv(summary_rows, out_summary)

# ==== 7. Console summary =====================================================

message("Weather.gov WRH time-series audit complete.")
message("  Candidate catalog: ", out_candidate_catalog)
message("  Candidate bins:    ", out_candidate_bins)
if (nrow(wrh_feature_bin_join) > 0) message("  CNRFC bins + WRH:  ", out_feature_bins_join)
message("  Live probe:        ", out_live_probe)
message("  Summary:           ", out_summary)
message("")

message("Weather.gov WRH candidate bins:")
print(wrh_candidate_bins, n = Inf)

if (nrow(live_probe) > 0) {
  message("")
  message("Weather.gov WRH / NWS API live-probe classes:")
  print(
    live_probe |>
      count(.data$wrh_probe_class, .data$nws_api_probe_class, name = "ids", sort = TRUE),
    n = Inf
  )

  message("")
  message("Example live-probe rows:")
  print(
    live_probe |>
      select(.data$cnrfc_id, .data$display_name, .data$weather_gov_candidate_bin, .data$wrh_probe_class, .data$nws_api_probe_class, .data$nws_api_elements_seen) |>
      head(20),
    n = 20
  )
}

message("")
message("Interpretation note:")
message("  WRH Time Series Viewer links are useful popup candidates, but the viewer page itself is a browser app.")
message("  Treat this pass as a candidate/link and smoke-check audit. A later deeper parser should confirm station-specific elements before strong map symbology claims.")
