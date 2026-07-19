# ==== 38_export_snow_pillow_live_inputs.r ===================================
## SWE003a
##
## PURPOSE:
##   Export the stable Snow Pillow / SNOTEL / CDEC snow-sensor station
##   reference inventory needed by the BRIM Ops Live SWE feed.
##
## INPUT:
##   04_processed_data/rds/snow_pillows_wgs84.rds
##
## OUTPUT:
##   brim-live-data-feeds/data/input/snow_pillow_station_index.csv
##
## QA:
##   04_processed_data/qa/snow_pillow_live_input_export_summary.csv
##
## DESIGN:
##   - This is a lightweight bridge from the local BRIM station-reference cache
##     to the GitHub live-data-feed repository.
##   - It does not fetch current/recent SWE observations.
##   - It does not read the old recent-observation sidecar RDS.
##   - It intentionally avoids exporting stale/current columns such as
##     latest_swe_*, latest_snow_depth_*, swe_delta_*, or similar.
##   - The output schema is explicit and lean so stale or unused fields do not
##     quietly accumulate in downstream data products.
##   - Current values, staleness, percentiles, and plots belong in later Ops
##     Live hosted products, not in this station-index file.
##
## HOW TO RUN FROM THE BRIM PROJECT ROOT:
##   source("run_build_map.r")
##   export_snow_pillow_live_inputs()
##
## OR DIRECTLY:
##   source("02_preprocess/38_export_snow_pillow_live_inputs.r")
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
    "\nInstall them, then rerun 38_export_snow_pillow_live_inputs.r."
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

SNOW_PILLOWS_RDS <- file.path(DIR$rds, "snow_pillows_wgs84.rds")

OUT_SNOW_INDEX_CSV <- Sys.getenv(
  "SNOW_PILLOW_STATION_INDEX_CSV",
  unset = file.path("brim-live-data-feeds", "data", "input", "snow_pillow_station_index.csv")
)

OUT_SNOW_QA_CSV <- Sys.getenv(
  "SNOW_PILLOW_STATION_INDEX_QA_CSV",
  unset = file.path(DIR$qa, "snow_pillow_live_input_export_summary.csv")
)

WRITE_QA <- TRUE

## Static/source metadata end dates from NRCS/NWCC active services often use a
## far-future sentinel date.  Treat those as open-ended so the station index and
## popups say "present" instead of showing weird dates such as 2100.
FUTURE_END_DATE_BUFFER_DAYS <- 365L

# ==== 3. Small local helpers =================================================

pt_snow_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_snow_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

pt_snow_col_chr <- function(df, nm) {
  if (nm %in% names(df)) {
    pt_snow_chr(df[[nm]])
  } else {
    rep(NA_character_, nrow(df))
  }
}

pt_snow_col_num <- function(df, nm) {
  if (nm %in% names(df)) {
    pt_snow_num(df[[nm]])
  } else {
    rep(NA_real_, nrow(df))
  }
}

pt_snow_parse_date <- function(x) {
  ## Return a real Date vector even when source values arrive as character,
  ## Date/POSIX, numeric epoch milliseconds, or already-parsed Date numbers.
  ## This avoids base::format.default() treating an unnamed "%Y" argument as
  ## `trim`, which can happen if a Date vector accidentally loses its class.

  if (inherits(x, "Date")) {
    return(as.Date(x))
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x, tz = "America/Los_Angeles"))
  }

  ch <- pt_snow_chr(x)
  out_num <- rep(NA_real_, length(ch))

  ## NRCS/NWCC ArcGIS services commonly provide epoch milliseconds. Depending
  ## on how R prints the source value, those can appear as regular integers or
  ## scientific notation. as.numeric() handles both.
  num <- suppressWarnings(as.numeric(ch))
  epoch_ms <- !is.na(num) & abs(num) > 1e10

  if (any(epoch_ms)) {
    d_epoch <- as.Date(
      as.POSIXct(num[epoch_ms] / 1000, origin = "1970-01-01", tz = "UTC")
    )
    out_num[epoch_ms] <- unclass(d_epoch)
  }

  remaining <- is.na(out_num) & !is.na(ch)

  if (any(remaining)) {
    ## CDEC metadata is usually ISO-like, e.g. 1984-01-01T00:00:00.
    iso_part <- substr(ch[remaining], 1, 10)
    parsed <- suppressWarnings(as.Date(iso_part))

    still_missing <- is.na(parsed)
    if (any(still_missing)) {
      parsed[still_missing] <- suppressWarnings(as.Date(
        ch[remaining][still_missing],
        tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%Y")
      ))
    }

    out_num[remaining] <- unclass(parsed)
  }

  as.Date(out_num, origin = "1970-01-01")
}

pt_snow_as_date <- function(x) {
  ## Lightweight coercion for helper functions that may receive either raw
  ## source values or the already-parsed Date vectors created above.
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }

  if (is.numeric(x)) {
    return(as.Date(x, origin = "1970-01-01"))
  }

  pt_snow_parse_date(x)
}

pt_snow_year <- function(x) {
  d <- pt_snow_as_date(x)
  out <- suppressWarnings(as.integer(format(d, format = "%Y")))
  out[is.na(out) | out < 1800 | out > 2100] <- NA_integer_
  out
}

pt_snow_date_chr <- function(x) {
  d <- pt_snow_as_date(x)
  out <- as.character(d)
  out[is.na(d)] <- NA_character_
  out
}

pt_snow_normalize_end_date <- function(x, status = NA_character_) {
  d <- pt_snow_as_date(x)
  status <- tolower(pt_snow_chr(status))

  far_future <- !is.na(d) & d > (Sys.Date() + FUTURE_END_DATE_BUFFER_DAYS)
  active_like <- !is.na(status) & status %in% c("active", "current", "open", "operational")

  d[far_future | active_like] <- as.Date(NA)
  d
}

pt_snow_period_display <- function(start_date, end_date, status = NA_character_) {
  start_year <- pt_snow_year(start_date)
  end_year <- pt_snow_year(end_date)
  status <- tolower(pt_snow_chr(status))
  active_like <- !is.na(status) & status %in% c("active", "current", "open", "operational")

  out <- rep("Not available", length(start_year))

  has_start <- !is.na(start_year)
  has_end <- !is.na(end_year)

  out[has_start & (active_like | !has_end)] <- paste0(start_year[has_start & (active_like | !has_end)], "–present")
  out[has_start & has_end] <- paste0(start_year[has_start & has_end], "–", end_year[has_start & has_end])
  out[!has_start & has_end] <- paste0("Through ", end_year[!has_start & has_end])

  out
}

pt_snow_station_uid_ok <- function(x) {
  !is.na(x) & nzchar(x)
}

# ==== 4. Read station-reference layer ========================================

if (!file.exists(SNOW_PILLOWS_RDS)) {
  stop("Missing snow pillow station-reference RDS: ", SNOW_PILLOWS_RDS)
}

message("Reading snow pillow station-reference layer: ", SNOW_PILLOWS_RDS)
snow_sf <- readRDS(SNOW_PILLOWS_RDS)

if (!inherits(snow_sf, "sf")) {
  stop(
    "Expected an sf object from ", SNOW_PILLOWS_RDS,
    ", but got: ", paste(class(snow_sf), collapse = ", ")
  )
}

snow_sf <- sf::st_transform(snow_sf, 4326)
snow_df <- sf::st_drop_geometry(snow_sf)

# ---- 4.1 Ensure latitude/longitude are available ---------------------------
##
## The station-reference layer should carry latitude/longitude, but deriving a
## fallback from geometry makes this bridge robust if the upstream schema is
## simplified later.

coords <- sf::st_coordinates(snow_sf)

if (!"longitude" %in% names(snow_df) || all(is.na(pt_snow_col_num(snow_df, "longitude")))) {
  snow_df$longitude <- coords[, 1]
}

if (!"latitude" %in% names(snow_df) || all(is.na(pt_snow_col_num(snow_df, "latitude")))) {
  snow_df$latitude <- coords[, 2]
}

# ==== 5. Build lean live-feed station index ==================================

raw_start_date <- pt_snow_col_chr(snow_df, "start_date")
raw_end_date <- pt_snow_col_chr(snow_df, "end_date")
station_status <- pt_snow_col_chr(snow_df, "status")

record_start_date <- pt_snow_parse_date(raw_start_date)
record_end_date <- pt_snow_normalize_end_date(raw_end_date, station_status)

snow_index <- tibble::tibble(
  station_uid            = pt_snow_col_chr(snow_df, "station_uid"),
  provider               = pt_snow_col_chr(snow_df, "provider"),
  source_system          = pt_snow_col_chr(snow_df, "source_system"),
  station_type           = pt_snow_col_chr(snow_df, "station_type"),
  station_type_label     = pt_snow_col_chr(snow_df, "station_type_label"),
  provider_station_id    = pt_snow_col_chr(snow_df, "station_id"),
  station_name           = pt_snow_col_chr(snow_df, "station_name"),
  state                  = pt_snow_col_chr(snow_df, "state"),
  county                 = pt_snow_col_chr(snow_df, "county"),
  river_basin            = pt_snow_col_chr(snow_df, "basin"),
  huc                    = pt_snow_col_chr(snow_df, "huc"),
  elevation_ft           = pt_snow_col_num(snow_df, "elevation_ft"),
  latitude               = round(pt_snow_col_num(snow_df, "latitude"), 6),
  longitude              = round(pt_snow_col_num(snow_df, "longitude"), 6),
  record_start_date      = pt_snow_date_chr(record_start_date),
  record_start_year      = pt_snow_year(record_start_date),
  record_end_date        = pt_snow_date_chr(record_end_date),
  record_end_year        = pt_snow_year(record_end_date),
  period_of_record       = pt_snow_period_display(record_start_date, record_end_date, station_status),
  station_status         = station_status,
  nrcs_site_num          = pt_snow_col_chr(snow_df, "site_num"),
  nrcs_station_triplet   = pt_snow_col_chr(snow_df, "station_triplet"),
  nrcs_shef_id           = pt_snow_col_chr(snow_df, "shef_id"),
  cdec_id                = pt_snow_col_chr(snow_df, "cdec_id"),
  operator_agency        = pt_snow_col_chr(snow_df, "operator_agency"),
  owner_agency           = pt_snow_col_chr(snow_df, "owner_agency"),
  official_station_url   = pt_snow_col_chr(snow_df, "site_page_url"),
  official_data_url      = pt_snow_col_chr(snow_df, "data_page_url"),
  source_metadata_url    = pt_snow_col_chr(snow_df, "source_url")
) |>
  dplyr::mutate(
    lat_lon = ifelse(
      is.na(latitude) | is.na(longitude),
      NA_character_,
      paste0(format(latitude, nsmall = 6, trim = TRUE), ", ", format(longitude, nsmall = 6, trim = TRUE))
    ),
    ## The later live-feed builders can use this to route provider-specific
    ## fetch logic without repeatedly interpreting station_type/provider text.
    live_provider_key = dplyr::case_when(
      .data$station_type == "snotel" ~ "nrcs_snotel",
      .data$station_type == "cdec_snow_sensor" ~ "cdec_snow_sensor",
      TRUE ~ "unknown"
    )
  ) |>
  dplyr::select(
    station_uid,
    live_provider_key,
    provider,
    source_system,
    station_type,
    station_type_label,
    provider_station_id,
    station_name,
    state,
    county,
    river_basin,
    huc,
    elevation_ft,
    latitude,
    longitude,
    lat_lon,
    record_start_date,
    record_start_year,
    record_end_date,
    record_end_year,
    period_of_record,
    station_status,
    nrcs_site_num,
    nrcs_station_triplet,
    nrcs_shef_id,
    cdec_id,
    operator_agency,
    owner_agency,
    official_station_url,
    official_data_url,
    source_metadata_url
  ) |>
  dplyr::arrange(live_provider_key, state, station_name, provider_station_id)

# ---- 5.1 Guardrails ---------------------------------------------------------

if (nrow(snow_index) == 0) {
  stop("Snow pillow station index has zero rows; refusing to write live-feed input CSV.")
}

if (any(!pt_snow_station_uid_ok(snow_index$station_uid))) {
  stop("Some snow pillow station-index rows are missing station_uid; refusing to write CSV.")
}

if (anyDuplicated(snow_index$station_uid) > 0) {
  dup_ids <- snow_index$station_uid[duplicated(snow_index$station_uid)]
  stop(
    "Duplicate station_uid values found in snow pillow station index; refusing to write CSV. Examples: ",
    paste(utils::head(unique(dup_ids), 10), collapse = ", ")
  )
}

missing_core <- snow_index |>
  dplyr::filter(is.na(provider_station_id) | is.na(latitude) | is.na(longitude))

if (nrow(missing_core) > 0) {
  warning(
    "Some snow pillow stations are missing provider_station_id, latitude, or longitude. ",
    "They are retained in the CSV for review, but the live feed may skip them: ",
    nrow(missing_core), " row(s)."
  )
}

forbidden_patterns <- c(
  "^latest_",
  "^recent_",
  "^swe_delta",
  "^snow_depth_delta",
  "^scan_",
  "soil_moisture"
)

forbidden_cols <- names(snow_index)[
  Reduce(`|`, lapply(forbidden_patterns, function(p) grepl(p, names(snow_index), ignore.case = TRUE)))
]

if (length(forbidden_cols) > 0) {
  stop(
    "Forbidden current/recent-value columns found in snow pillow station index: ",
    paste(forbidden_cols, collapse = ", ")
  )
}

# ==== 6. Write outputs =======================================================

if (!dir.exists(dirname(OUT_SNOW_INDEX_CSV))) {
  dir.create(dirname(OUT_SNOW_INDEX_CSV), recursive = TRUE, showWarnings = FALSE)
}

readr::write_csv(snow_index, OUT_SNOW_INDEX_CSV, na = "")
message("Wrote snow pillow live-feed station index: ", OUT_SNOW_INDEX_CSV)

if (WRITE_QA) {
  if (!dir.exists(dirname(OUT_SNOW_QA_CSV))) {
    dir.create(dirname(OUT_SNOW_QA_CSV), recursive = TRUE, showWarnings = FALSE)
  }

  provider_counts <- snow_index |>
    dplyr::count(live_provider_key, station_type_label, state, name = "n") |>
    dplyr::arrange(live_provider_key, state, station_type_label)

  qa_summary <- tibble::tibble(
    metric = c(
      "run_time_utc",
      "source_rds",
      "output_csv",
      "input_station_rows",
      "exported_station_rows",
      "stations_with_provider_station_id",
      "stations_with_lat_lon",
      "stations_with_period_of_record",
      "nrcs_snotel_rows",
      "cdec_snow_sensor_rows",
      "states",
      "min_record_start_year",
      "max_record_start_year",
      "forbidden_current_value_columns_present",
      "provider_state_counts"
    ),
    value = c(
      format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
      SNOW_PILLOWS_RDS,
      OUT_SNOW_INDEX_CSV,
      as.character(nrow(snow_df)),
      as.character(nrow(snow_index)),
      as.character(sum(!is.na(snow_index$provider_station_id))),
      as.character(sum(!is.na(snow_index$latitude) & !is.na(snow_index$longitude))),
      as.character(sum(!is.na(snow_index$period_of_record) & snow_index$period_of_record != "Not available")),
      as.character(sum(snow_index$live_provider_key == "nrcs_snotel", na.rm = TRUE)),
      as.character(sum(snow_index$live_provider_key == "cdec_snow_sensor", na.rm = TRUE)),
      paste(sort(unique(na.omit(snow_index$state))), collapse = ", "),
      as.character(suppressWarnings(min(snow_index$record_start_year, na.rm = TRUE))),
      as.character(suppressWarnings(max(snow_index$record_start_year, na.rm = TRUE))),
      as.character(length(forbidden_cols) > 0),
      paste0(
        provider_counts$live_provider_key, "/", provider_counts$state, "=", provider_counts$n,
        collapse = "; "
      )
    )
  )

  readr::write_csv(qa_summary, OUT_SNOW_QA_CSV, na = "")
  message("Wrote snow pillow live-feed input QA summary: ", OUT_SNOW_QA_CSV)
}

# ==== 7. Console summary =====================================================

message("\nSnow pillow live-feed input export summary:")
message("  Stations in source RDS:     ", nrow(snow_df))
message("  Stations exported:          ", nrow(snow_index))
message("  NRCS/SNOTEL rows:           ", sum(snow_index$live_provider_key == "nrcs_snotel", na.rm = TRUE))
message("  CDEC snow-sensor rows:      ", sum(snow_index$live_provider_key == "cdec_snow_sensor", na.rm = TRUE))
message("  With provider_station_id:   ", sum(!is.na(snow_index$provider_station_id)))
message("  With lat/lon:               ", sum(!is.na(snow_index$latitude) & !is.na(snow_index$longitude)))
message("  With period of record:      ", sum(!is.na(snow_index$period_of_record) & snow_index$period_of_record != "Not available"))
message("  States:                     ", paste(sort(unique(na.omit(snow_index$state))), collapse = ", "))
message("  Current/recent cols present: ", length(forbidden_cols))

message("\nSnow pillow station-index columns:")
print(names(snow_index))

message("\nFirst 8 snow pillow station-index rows:")
print(utils::head(snow_index, 8))

# ==== 8. End =================================================================
