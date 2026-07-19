# ==== 33_export_scan_live_inputs.r ==========================================
##
## PURPOSE:
##   Export the stable SCAN station/reference inventory needed by the BRIM
##   Ops Live SCAN soil-moisture feed.
##
## INPUT:
##   04_processed_data/rds/scan_stations_wgs84.rds
##
## OUTPUT:
##   brim-live-data-feeds/data/input/scan_station_index.csv
##
## DESIGN:
##   - This is a lightweight bridge from the local BRIM station-reference cache
##     to the GitHub live-data feed repository.
##   - It does not fetch current or recent soil-moisture observations.
##   - It does not read the old recent-observation sidecar RDS.
##   - It intentionally avoids exporting stale current-value columns such as
##     sms_* or scan_selected_soil_moisture_*.
##   - Current values, staleness, percentiles, and plots belong in the Ops Live
##     feed scripts and hosted GeoJSON files, not in this static station index.
##
## HOW TO RUN FROM THE BRIM PROJECT ROOT:
##   source("run_build_map.r")
##   export_scan_live_inputs()
##
## OR DIRECTLY:
##   source("02_preprocess/33_export_scan_live_inputs.r")
## ============================================================================

# ==== 1. Load configuration and packages =====================================

source("00_config/config_paths.r")

required_pkgs <- c("sf", "dplyr", "readr", "stringr", "tibble")
missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them, then rerun 33_export_scan_live_inputs.r."
  )
}

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

# ==== 2. Paths and switches ==================================================

SCAN_STATIONS_RDS <- file.path(DIR$rds, "scan_stations_wgs84.rds")

OUT_SCAN_INDEX_CSV <- Sys.getenv(
  "SCAN_STATION_INDEX_CSV",
  unset = file.path("brim-live-data-feeds", "data", "input", "scan_station_index.csv")
)

OUT_SCAN_QA_CSV <- Sys.getenv(
  "SCAN_STATION_INDEX_QA_CSV",
  unset = file.path(DIR$qa, "scan_live_input_export_summary.csv")
)

WRITE_QA <- TRUE

# ==== 3. Small local helpers =================================================

pt_scan_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_scan_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

pt_scan_col_chr <- function(df, nm) {
  if (nm %in% names(df)) {
    pt_scan_chr(df[[nm]])
  } else {
    rep(NA_character_, nrow(df))
  }
}

pt_scan_col_num <- function(df, nm) {
  if (nm %in% names(df)) {
    pt_scan_num(df[[nm]])
  } else {
    rep(NA_real_, nrow(df))
  }
}

pt_scan_year_from_date <- function(x) {
  x <- pt_scan_chr(x)
  out <- suppressWarnings(as.integer(substr(x, 1, 4)))
  out[is.na(out) | out < 1800 | out > 2100] <- NA_integer_
  out
}

# ==== 4. Read station-reference layer ========================================

if (!file.exists(SCAN_STATIONS_RDS)) {
  stop("Missing SCAN station-reference RDS: ", SCAN_STATIONS_RDS)
}

message("Reading SCAN station-reference layer: ", SCAN_STATIONS_RDS)
scan_sf <- readRDS(SCAN_STATIONS_RDS)

if (!inherits(scan_sf, "sf")) {
  stop("Expected an sf object from ", SCAN_STATIONS_RDS, ", but got: ", paste(class(scan_sf), collapse = ", "))
}

scan_sf <- sf::st_transform(scan_sf, 4326)
scan_df <- sf::st_drop_geometry(scan_sf)

# ---- 4.1 Ensure latitude/longitude are available ---------------------------
##
## The current station-reference layer already carries latitude/longitude, but
## deriving a fallback from geometry makes this bridge robust if the upstream
## schema is simplified later.

coords <- sf::st_coordinates(scan_sf)

if (!"longitude" %in% names(scan_df) || all(is.na(pt_scan_col_num(scan_df, "longitude")))) {
  scan_df$longitude <- coords[, 1]
}

if (!"latitude" %in% names(scan_df) || all(is.na(pt_scan_col_num(scan_df, "latitude")))) {
  scan_df$latitude <- coords[, 2]
}

# ==== 5. Build lean live-feed station index ==================================

scan_index <- tibble::tibble(
  station_uid        = pt_scan_col_chr(scan_df, "station_uid"),
  station_name       = pt_scan_col_chr(scan_df, "station_name"),
  station_type       = pt_scan_col_chr(scan_df, "station_type"),
  station_type_label = pt_scan_col_chr(scan_df, "station_type_label"),
  map_layer          = pt_scan_col_chr(scan_df, "map_layer"),
  network_group      = pt_scan_col_chr(scan_df, "network_group"),
  provider           = pt_scan_col_chr(scan_df, "provider"),
  source_system      = pt_scan_col_chr(scan_df, "source_system"),
  source_layer       = pt_scan_col_chr(scan_df, "source_layer"),
  station_id         = pt_scan_col_chr(scan_df, "station_id"),
  site_num           = pt_scan_col_chr(scan_df, "site_num"),
  station_triplet    = pt_scan_col_chr(scan_df, "station_triplet"),
  shef_id            = pt_scan_col_chr(scan_df, "shef_id"),
  state              = pt_scan_col_chr(scan_df, "state"),
  county             = pt_scan_col_chr(scan_df, "county"),
  basin              = pt_scan_col_chr(scan_df, "basin"),
  huc                = pt_scan_col_chr(scan_df, "huc"),
  elevation_ft       = pt_scan_col_num(scan_df, "elevation_ft"),
  latitude           = round(pt_scan_col_num(scan_df, "latitude"), 6),
  longitude          = round(pt_scan_col_num(scan_df, "longitude"), 6),
  start_date         = pt_scan_col_chr(scan_df, "start_date"),
  start_year         = pt_scan_year_from_date(pt_scan_col_chr(scan_df, "start_date")),
  end_date           = pt_scan_col_chr(scan_df, "end_date"),
  status             = pt_scan_col_chr(scan_df, "status"),
  site_page_url      = pt_scan_col_chr(scan_df, "site_page_url"),
  data_page_url      = pt_scan_col_chr(scan_df, "data_page_url"),
  source_url         = pt_scan_col_chr(scan_df, "source_url")
) |>
  dplyr::mutate(
    ## soilDB::fetchSCAN() uses the numeric NRCS site code. Keep it both as the
    ## source-like character field above and as a parsed integer for the live
    ## feed script.
    site_code = suppressWarnings(as.integer(site_num)),
    lat_lon = ifelse(
      is.na(latitude) | is.na(longitude),
      NA_character_,
      paste0(format(latitude, nsmall = 6, trim = TRUE), ", ", format(longitude, nsmall = 6, trim = TRUE))
    )
  ) |>
  dplyr::select(
    station_uid,
    station_name,
    station_type,
    station_type_label,
    network_group,
    provider,
    source_system,
    station_id,
    site_num,
    site_code,
    station_triplet,
    shef_id,
    state,
    county,
    basin,
    huc,
    elevation_ft,
    latitude,
    longitude,
    lat_lon,
    start_date,
    start_year,
    end_date,
    status,
    site_page_url,
    data_page_url,
    source_url
  ) |>
  dplyr::arrange(state, station_name, site_code)

# ---- 5.1 Guardrails ---------------------------------------------------------

if (nrow(scan_index) == 0) {
  stop("SCAN station index has zero rows; refusing to write live-feed input CSV.")
}

missing_core <- scan_index |>
  dplyr::filter(is.na(site_code) | is.na(latitude) | is.na(longitude))

if (nrow(missing_core) > 0) {
  warning(
    "Some SCAN stations are missing site_code, latitude, or longitude. ",
    "They are retained in the CSV for review, but the live feed may skip them: ",
    nrow(missing_core), " row(s)."
  )
}

# ==== 6. Write outputs =======================================================

if (!dir.exists(dirname(OUT_SCAN_INDEX_CSV))) {
  dir.create(dirname(OUT_SCAN_INDEX_CSV), recursive = TRUE, showWarnings = FALSE)
}

readr::write_csv(scan_index, OUT_SCAN_INDEX_CSV, na = "")
message("Wrote SCAN live-feed station index: ", OUT_SCAN_INDEX_CSV)

if (WRITE_QA) {
  if (!dir.exists(dirname(OUT_SCAN_QA_CSV))) {
    dir.create(dirname(OUT_SCAN_QA_CSV), recursive = TRUE, showWarnings = FALSE)
  }
  
  qa_summary <- tibble::tibble(
    metric = c(
      "run_time_utc",
      "source_rds",
      "output_csv",
      "input_station_rows",
      "exported_station_rows",
      "stations_with_site_code",
      "stations_with_lat_lon",
      "states",
      "min_start_year",
      "max_start_year"
    ),
    value = c(
      format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      SCAN_STATIONS_RDS,
      OUT_SCAN_INDEX_CSV,
      as.character(nrow(scan_df)),
      as.character(nrow(scan_index)),
      as.character(sum(!is.na(scan_index$site_code))),
      as.character(sum(!is.na(scan_index$latitude) & !is.na(scan_index$longitude))),
      paste(sort(unique(na.omit(scan_index$state))), collapse = ", "),
      as.character(suppressWarnings(min(scan_index$start_year, na.rm = TRUE))),
      as.character(suppressWarnings(max(scan_index$start_year, na.rm = TRUE)))
    )
  )
  
  readr::write_csv(qa_summary, OUT_SCAN_QA_CSV, na = "")
  message("Wrote SCAN live-feed input QA summary: ", OUT_SCAN_QA_CSV)
}

# ==== 7. Console summary =====================================================

message("\nSCAN live-feed input export summary:")
message("  Stations in source RDS: ", nrow(scan_df))
message("  Stations exported:      ", nrow(scan_index))
message("  With site_code:         ", sum(!is.na(scan_index$site_code)))
message("  With lat/lon:           ", sum(!is.na(scan_index$latitude) & !is.na(scan_index$longitude)))
message("  States:                 ", paste(sort(unique(na.omit(scan_index$state))), collapse = ", "))

print(utils::head(scan_index, 8))

# ==== 8. End =================================================================
