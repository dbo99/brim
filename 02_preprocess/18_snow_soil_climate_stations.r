# ==== 18_snow_soil_climate_stations.r =======================================
##
## PURPOSE:
##   Build two PT2-ready station point layers:
##
##     1. SCAN stations
##        USDA NRCS / NWCC active SCAN stations.
##
##     2. Snow pillows
##        USDA NRCS / NWCC active SNOTEL stations plus
##        CA DWR / CDEC / California Snow Data active snow sensor metadata.
##
## DESIGN:
##   - Save stable processed RDS files in 04_processed_data/rds.
##   - Cache raw downloads in 01_raw_data/snow_soil_climate/cache.
##   - Do not build Leaflet popup HTML here.
##   - Keep compact metadata and URLs so the map cache script can build popups.
##
## OUTPUTS:
##   04_processed_data/rds/scan_stations_wgs84.rds
##   04_processed_data/rds/snow_pillows_wgs84.rds
##
## QA:
##   04_processed_data/qa/snow_soil_climate_station_summary.csv
##   04_processed_data/qa/snow_soil_climate_missing_core_fields.csv
## ============================================================================


# ==== 1. Load configuration and packages =====================================

source("00_config/config_paths.r")
source("03_functions/spatial_helpers.r")

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(glue)
  library(jsonlite)
  library(readr)
  library(tibble)
})


# ==== 2. User-facing switches ================================================

## First run:
##   ALLOW_DOWNLOADS <- TRUE
##   REFRESH_EXISTING_CACHE <- TRUE or FALSE
##
## Later normal use:
##   ALLOW_DOWNLOADS <- FALSE
##   REFRESH_EXISTING_CACHE <- FALSE

ALLOW_DOWNLOADS <- TRUE
REFRESH_EXISTING_CACHE <- FALSE

WRITE_RDS <- TRUE
WRITE_GPKG <- FALSE
WRITE_QA <- TRUE

## States to request from NRCS/NWCC SNOTEL and SCAN layers.
## CA = 6, NV = 32, OR = 41, AZ = 4
##
## We request a broader state set, then apply a lon/lat bounding-box filter
## after points are standardized. This keeps southern Oregon, western Nevada,
## and western Arizona stations potentially relevant to hydrologic California
## without pulling in the entire interior West.
STATE_FIPS_KEEP <- c(4, 6, 32, 41)

## Broad hydrologic-California screening envelope.
## This is intentionally generous and can be tightened later.
HYDRO_CA_BBOX <- list(
  xmin = -125.0,
  xmax = -113.0,
  ymin = 31.0,
  ymax = 43.0
)


# ==== 3. Source URLs ==========================================================

## USDA NRCS / NWCC ArcGIS service base.
NWCC_BASE <- "https://services.arcgis.com/SXbDpmb7xQkk44JV/ArcGIS/rest/services"

## CA Open Data / CNRA CKAN API endpoint.
CKAN_API_URL <- "https://data.cnra.ca.gov/api/3/action/datastore_search"

## California Snow Data -> Station Metadata resource ID.
CDEC_SNOW_METADATA_RESOURCE_ID <- "49baa289-3cc3-439a-af7a-727486dbf303"

## Direct CSV fallback for CDEC metadata.
CDEC_METADATA_CSV_URL <- paste0(
  "https://data.cnra.ca.gov/dataset/797db683-e9f1-4c4b-93f9-ee8f065313e7/",
  "resource/49baa289-3cc3-439a-af7a-727486dbf303/download/",
  "snow-station-metadata.csv"
)


# ==== 4. Paths ================================================================

DIR$snow_cache <- file.path(DIR$raw, "snow_soil_climate", "cache")
dir.create(DIR$snow_cache, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR$rds, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR$gpkg, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR$qa, showWarnings = FALSE, recursive = TRUE)

CACHE_SNOTEL_RAW <- file.path(DIR$snow_cache, "nrcs_nwcc_snotel_active_raw.rds")
CACHE_SCAN_RAW   <- file.path(DIR$snow_cache, "nrcs_nwcc_scan_active_raw.rds")
CACHE_CDEC_RAW   <- file.path(DIR$snow_cache, "cdec_snow_station_metadata_raw.rds")

OUT_SCAN_RDS <- file.path(DIR$rds, "scan_stations_wgs84.rds")
OUT_SNOW_RDS <- file.path(DIR$rds, "snow_pillows_wgs84.rds")

OUT_SCAN_GPKG <- file.path(DIR$gpkg, "scan_stations_wgs84.gpkg")
OUT_SNOW_GPKG <- file.path(DIR$gpkg, "snow_pillows_wgs84.gpkg")

OUT_SUMMARY_CSV <- file.path(DIR$qa, "snow_soil_climate_station_summary.csv")
OUT_MISSING_CSV <- file.path(DIR$qa, "snow_soil_climate_missing_core_fields.csv")


# ==== 5. Standard schema ======================================================

STANDARD_FIELDS <- c(
  "station_uid",
  "station_name",
  "station_type",
  "station_type_label",
  "map_layer",
  "network_group",
  "provider",
  "source_system",
  "source_layer",
  "station_id",
  "site_num",
  "station_triplet",
  "shef_id",
  "cdec_id",
  "operator_agency",
  "owner_agency",
  "state",
  "county",
  "basin",
  "huc",
  "elevation_ft",
  "latitude",
  "longitude",
  "start_date",
  "end_date",
  "status",
  "site_page_url",
  "data_page_url",
  "source_url",
  "geometry"
)


# ==== 6. Helper functions =====================================================

clean_chr <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  dplyr::na_if(x, "")
}

clean_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

sanitize_id <- function(x) {
  x |>
    as.character() |>
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") |>
    stringr::str_replace_all("^_+|_+$", "")
}

normalize_names <- function(x) {
  x |>
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") |>
    stringr::str_replace_all("^_+|_+$", "") |>
    stringr::str_to_lower()
}

fips_to_state <- function(fips) {
  dplyr::case_when(
    as.integer(fips) == 6  ~ "CA",
    as.integer(fips) == 32 ~ "NV",
    as.integer(fips) == 41 ~ "OR",
    TRUE ~ NA_character_
  )
}

add_missing_standard_fields <- function(x, standard_fields = STANDARD_FIELDS) {
  missing_fields <- setdiff(standard_fields, names(x))
  for (nm in missing_fields) {
    if (nm != "geometry") x[[nm]] <- NA
  }
  x
}

select_standard_fields <- function(x, standard_fields = STANDARD_FIELDS) {
  x |>
    add_missing_standard_fields(standard_fields) |>
    dplyr::select(dplyr::any_of(standard_fields))
}

get_field <- function(x, candidates, default = NA_character_) {
  hit <- intersect(candidates, names(x))
  if (length(hit) == 0) {
    rep(default, nrow(x))
  } else {
    x[[hit[1]]]
  }
}

pick_col <- function(df, patterns, required = FALSE, default = NA_character_) {
  nm_original <- names(df)
  nm_clean <- normalize_names(nm_original)
  hit <- NULL
  
  for (pat in patterns) {
    idx <- which(stringr::str_detect(nm_clean, stringr::regex(pat, ignore_case = TRUE)))
    if (length(idx) > 0) {
      hit <- idx[1]
      break
    }
  }
  
  if (is.null(hit)) {
    if (required) {
      stop(
        "Could not find required column matching any of: ",
        paste(patterns, collapse = ", "),
        "\nAvailable columns are:\n",
        paste(nm_original, collapse = "\n"),
        call. = FALSE
      )
    }
    return(rep(default, nrow(df)))
  }
  
  df[[hit]]
}

load_or_fetch <- function(cache_path, fetch_fun, label) {
  
  if (file.exists(cache_path) && !REFRESH_EXISTING_CACHE) {
    message("Reading cached ", label, ": ", cache_path)
    return(readRDS(cache_path))
  }
  
  if (!ALLOW_DOWNLOADS) {
    stop(
      "\nCache file is missing or refresh was requested, but ALLOW_DOWNLOADS is FALSE.\n",
      "Needed cache: ", cache_path, "\n\n",
      "To fetch/update this source, set ALLOW_DOWNLOADS <- TRUE.\n",
      call. = FALSE
    )
  }
  
  message("Fetching ", label, "...")
  x <- fetch_fun()
  saveRDS(x, cache_path)
  message("Cached ", label, " to: ", cache_path)
  
  x
}


filter_to_hydro_ca_bbox <- function(x, bbox = HYDRO_CA_BBOX, label = "station layer") {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(x)
  }
  
  coords <- sf::st_coordinates(sf::st_transform(x, 4326))
  
  keep <- coords[, 1] >= bbox$xmin &
    coords[, 1] <= bbox$xmax &
    coords[, 2] >= bbox$ymin &
    coords[, 2] <= bbox$ymax
  
  message(
    label,
    ": keeping ",
    sum(keep, na.rm = TRUE),
    " of ",
    length(keep),
    " station(s) inside broad hydrologic-California bbox."
  )
  
  x[keep, , drop = FALSE]
}

# ==== 7. NRCS / NWCC functions ===============================================

build_arcgis_geojson_url <- function(service_name,
                                     layer_id = 0,
                                     where = "1=1",
                                     out_fields = "*",
                                     out_sr = 4326) {
  
  base_url <- glue::glue("{NWCC_BASE}/{service_name}/FeatureServer/{layer_id}/query")
  
  query <- list(
    where = where,
    outFields = out_fields,
    outSR = as.character(out_sr),
    returnGeometry = "true",
    f = "geojson"
  )
  
  paste0(
    base_url,
    "?",
    paste(
      names(query),
      URLencode(unlist(query), reserved = TRUE),
      sep = "=",
      collapse = "&"
    )
  )
}

read_nwcc_layer <- function(service_name, state_fips_keep = STATE_FIPS_KEEP) {
  
  state_where <- glue::glue(
    "fipsStateNumber IN ({paste(state_fips_keep, collapse = ',')})"
  )
  
  url <- build_arcgis_geojson_url(
    service_name = service_name,
    where = state_where
  )
  
  message("Reading NRCS/NWCC layer: ", service_name)
  message(url)
  
  sf::st_read(url, quiet = TRUE) |>
    sf::st_transform(4326)
}

parse_nwcc_site_num <- function(station_triplet, fallback_id = NA_character_) {
  
  station_triplet <- clean_chr(station_triplet)
  fallback_id <- clean_chr(fallback_id)
  
  site_from_triplet <- stringr::str_extract(station_triplet, "^\\d+")
  site_from_fallback <- stringr::str_extract(fallback_id, "\\d+")
  
  dplyr::coalesce(site_from_triplet, site_from_fallback)
}

make_nwcc_site_url <- function(site_num) {
  site_num <- clean_chr(site_num)
  ifelse(
    is.na(site_num),
    NA_character_,
    glue::glue("https://wcc.sc.egov.usda.gov/nwcc/site?sitenum={site_num}")
  )
}

standardize_nwcc_station_layer <- function(x,
                                           station_type,
                                           station_type_label,
                                           map_layer,
                                           source_layer,
                                           provider = "USDA NRCS NWCC",
                                           source_system = "NRCS NWCC",
                                           network_group = "Snow / soil climate stations") {
  
  x <- sf::st_transform(x, 4326)
  coords <- sf::st_coordinates(sf::st_geometry(x))
  
  station_triplet <- clean_chr(get_field(x, c("stationTriplet", "station_triplet")))
  shef_id <- clean_chr(get_field(x, c("shefId", "shef_id", "shefid")))
  
  fallback_id <- clean_chr(get_field(
    x,
    c("stationId", "station_id", "siteNum", "site_num", "site_no", "id", "name")
  ))
  
  site_num <- parse_nwcc_site_num(
    station_triplet = station_triplet,
    fallback_id = dplyr::coalesce(fallback_id, shef_id)
  )
  
  fips_state <- get_field(x, c("fipsStateNumber", "fips_state_number", "stateFips"))
  state <- clean_chr(get_field(x, c("stateCode", "state_code", "state")))
  state <- dplyr::coalesce(state, fips_to_state(fips_state))
  
  latitude <- clean_num(get_field(x, c("latitude", "lat")))
  longitude <- clean_num(get_field(x, c("longitude", "lon", "long")))
  latitude <- dplyr::coalesce(latitude, coords[, 2])
  longitude <- dplyr::coalesce(longitude, coords[, 1])
  
  station_name <- clean_chr(get_field(x, c("name", "stationName", "station_name")))
  elevation_ft <- clean_num(get_field(x, c("elevation", "elev", "elevation_ft")))
  county <- clean_chr(get_field(x, c("countyName", "county_name", "county")))
  huc <- clean_chr(get_field(x, c("huc", "huc8", "HUC")))
  
  start_date <- clean_chr(get_field(
    x,
    c("beginDate", "begin_date", "startDate", "start_date", "reportingSince")
  ))
  
  end_date <- clean_chr(get_field(
    x,
    c("endDate", "end_date", "stopDate", "stop_date")
  ))
  
  site_page_url <- make_nwcc_site_url(site_num)
  
  x |>
    dplyr::mutate(
      station_uid = sanitize_id(glue::glue("NRCS_{station_type}_{site_num}")),
      station_name = station_name,
      station_type = station_type,
      station_type_label = station_type_label,
      map_layer = map_layer,
      network_group = network_group,
      provider = provider,
      source_system = source_system,
      source_layer = source_layer,
      station_id = site_num,
      site_num = site_num,
      station_triplet = station_triplet,
      shef_id = shef_id,
      cdec_id = NA_character_,
      operator_agency = provider,
      owner_agency = NA_character_,
      state = state,
      county = county,
      basin = NA_character_,
      huc = huc,
      elevation_ft = elevation_ft,
      latitude = latitude,
      longitude = longitude,
      start_date = start_date,
      end_date = end_date,
      status = "active",
      site_page_url = site_page_url,
      data_page_url = site_page_url,
      source_url = glue::glue("{NWCC_BASE}/{source_layer}/FeatureServer/0")
    ) |>
    select_standard_fields()
}


# ==== 8. CDEC functions =======================================================

fetch_ckan_resource <- function(resource_id, limit = 50000) {
  
  query_url <- paste0(
    CKAN_API_URL,
    "?resource_id=", URLencode(resource_id, reserved = TRUE),
    "&limit=", limit
  )
  
  message("Trying CKAN datastore API:")
  message(query_url)
  
  x <- jsonlite::fromJSON(query_url, flatten = TRUE)
  
  if (!isTRUE(x$success)) {
    stop("CKAN API call did not return success = TRUE.")
  }
  
  tibble::as_tibble(x$result$records)
}

fetch_csv_fallback <- function(csv_url) {
  
  message("Trying direct CDEC metadata CSV fallback:")
  message(csv_url)
  
  tmp <- tempfile(fileext = ".csv")
  
  try_1 <- try(
    utils::download.file(csv_url, tmp, mode = "wb", quiet = TRUE),
    silent = TRUE
  )
  
  if (inherits(try_1, "try-error")) {
    stop("CDEC CSV fallback failed. Try opening the CSV URL in a browser.", call. = FALSE)
  }
  
  readr::read_csv(tmp, show_col_types = FALSE)
}

fetch_cdec_station_metadata <- function() {
  
  out <- try(fetch_ckan_resource(CDEC_SNOW_METADATA_RESOURCE_ID), silent = TRUE)
  
  if (!inherits(out, "try-error")) {
    return(out)
  }
  
  warning("CKAN API failed; trying direct CSV fallback.")
  fetch_csv_fallback(CDEC_METADATA_CSV_URL)
}

make_cdec_station_url <- function(station_id) {
  station_id <- clean_chr(station_id)
  ifelse(
    is.na(station_id),
    NA_character_,
    glue::glue("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id={station_id}")
  )
}

make_cdec_data_url <- function(station_id) {
  station_id <- clean_chr(station_id)
  ifelse(
    is.na(station_id),
    NA_character_,
    glue::glue("https://cdec.water.ca.gov/dynamicapp/QueryDaily?s={station_id}")
  )
}

standardize_cdec_snow_metadata <- function(cdec_raw,
                                           provider = "CA DWR / CDEC / CCSS",
                                           source_system = "CDEC",
                                           source_layer = "California Snow Data - Station Metadata",
                                           network_group = "Snow / soil climate stations") {
  
  station_id <- clean_chr(
    pick_col(
      cdec_raw,
      patterns = c("^station_id$", "^station$", "^sta$", "station.*id", "station.*code", "^id$"),
      required = TRUE
    )
  )
  
  station_name <- clean_chr(
    pick_col(
      cdec_raw,
      patterns = c("station.*name", "^name$", "site.*name"),
      required = FALSE
    )
  )
  
  latitude <- clean_num(
    pick_col(
      cdec_raw,
      patterns = c("^latitude$", "^lat$", "lat_dd", "decimal.*lat"),
      required = TRUE
    )
  )
  
  longitude <- clean_num(
    pick_col(
      cdec_raw,
      patterns = c("^longitude$", "^lon$", "^long$", "lng", "long_dd", "decimal.*lon"),
      required = TRUE
    )
  )
  
  elevation_ft <- clean_num(
    pick_col(
      cdec_raw,
      patterns = c("^elevation$", "^elev$", "elevation.*ft", "elev.*ft"),
      required = FALSE
    )
  )
  
  operator_agency <- clean_chr(
    pick_col(
      cdec_raw,
      patterns = c(
        "operation.*maintenance",
        "operations.*maintenance",
        "o_m",
        "om_agency",
        "maintenance.*agency",
        "operator",
        "operating.*agency",
        "agency"
      ),
      required = FALSE
    )
  )
  
  start_date <- clean_chr(
    pick_col(cdec_raw, patterns = c("start.*date", "begin.*date", "period.*start", "record.*start"))
  )
  
  end_date <- clean_chr(
    pick_col(cdec_raw, patterns = c("end.*date", "period.*end", "record.*end"))
  )
  
  basin <- clean_chr(
    pick_col(cdec_raw, patterns = c("^basin$", "river.*basin", "watershed", "hydrologic.*area"))
  )
  
  county <- clean_chr(
    pick_col(cdec_raw, patterns = c("^county$", "county.*name"))
  )
  
  status <- clean_chr(
    pick_col(
      cdec_raw,
      patterns = c("^status$", "station.*status", "active"),
      required = FALSE,
      default = "active"
    )
  )
  status <- dplyr::coalesce(status, "active")
  
  cdec_df <- tibble::tibble(
    station_uid = sanitize_id(glue::glue("CDEC_snow_sensor_{station_id}")),
    station_name = station_name,
    station_type = "cdec_snow_sensor",
    station_type_label = "CDEC snow sensor",
    map_layer = "snow_pillows",
    network_group = network_group,
    provider = provider,
    source_system = source_system,
    source_layer = source_layer,
    station_id = station_id,
    site_num = NA_character_,
    station_triplet = NA_character_,
    shef_id = NA_character_,
    cdec_id = station_id,
    operator_agency = operator_agency,
    owner_agency = NA_character_,
    state = "CA",
    county = county,
    basin = basin,
    huc = NA_character_,
    elevation_ft = elevation_ft,
    latitude = latitude,
    longitude = longitude,
    start_date = start_date,
    end_date = end_date,
    status = status,
    site_page_url = make_cdec_station_url(station_id),
    data_page_url = make_cdec_data_url(station_id),
    source_url = "https://data.cnra.ca.gov/dataset/california-snow-data/resource/49baa289-3cc3-439a-af7a-727486dbf303"
  ) |>
    dplyr::filter(
      !is.na(latitude),
      !is.na(longitude),
      longitude < 0,
      latitude > 30,
      latitude < 45
    ) |>
    dplyr::distinct(station_uid, .keep_all = TRUE)
  
  sf::st_as_sf(
    cdec_df,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
    select_standard_fields()
}


# ==== 9. Load or fetch raw sources ===========================================

snotel_raw <- load_or_fetch(
  CACHE_SNOTEL_RAW,
  function() read_nwcc_layer("stations_SNTL_ACTIVE"),
  "NRCS/NWCC active SNOTEL"
)

scan_raw <- load_or_fetch(
  CACHE_SCAN_RAW,
  function() read_nwcc_layer("stations_SCAN_ACTIVE"),
  "NRCS/NWCC active SCAN"
)

cdec_raw <- load_or_fetch(
  CACHE_CDEC_RAW,
  fetch_cdec_station_metadata,
  "CDEC active snow sensor metadata"
)

message("Raw CDEC metadata columns:")
print(names(cdec_raw))


# ==== 10. Standardize =========================================================

snotel_web <- standardize_nwcc_station_layer(
  snotel_raw,
  station_type = "snotel",
  station_type_label = "SNOTEL",
  map_layer = "snow_pillows",
  source_layer = "stations_SNTL_ACTIVE"
)

scan_web <- standardize_nwcc_station_layer(
  scan_raw,
  station_type = "scan",
  station_type_label = "SCAN",
  map_layer = "scan",
  source_layer = "stations_SCAN_ACTIVE"
)

cdec_web <- standardize_cdec_snow_metadata(cdec_raw)


# ==== 11. Split final products ===============================================

scan_stations <- scan_web |>
  sf::st_transform(4326) |>
  filter_to_hydro_ca_bbox(label = "SCAN stations") |>
  dplyr::arrange(state, station_name)

snow_pillows <- dplyr::bind_rows(
  snotel_web,
  cdec_web
) |>
  sf::st_transform(4326) |>
  filter_to_hydro_ca_bbox(label = "snow pillows") |>
  dplyr::arrange(state, station_type, station_name)


# ==== 12. QA summaries ========================================================

all_stations <- dplyr::bind_rows(
  scan_stations,
  snow_pillows
)

station_summary <- all_stations |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::count(map_layer, provider, station_type_label, state, sort = TRUE)

missing_core <- all_stations |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::summarise(
    n_total = dplyr::n(),
    missing_station_name = sum(is.na(station_name)),
    missing_station_id = sum(is.na(station_id)),
    missing_elevation_ft = sum(is.na(elevation_ft)),
    missing_site_page_url = sum(is.na(site_page_url)),
    missing_latitude = sum(is.na(latitude)),
    missing_longitude = sum(is.na(longitude))
  )

message("\nStation counts by layer/provider/type/state:")
print(station_summary, n = Inf)

message("\nMissing core fields:")
print(missing_core, width = 1200)


# ==== 13. Save outputs ========================================================

if (WRITE_QA) {
  readr::write_csv(station_summary, OUT_SUMMARY_CSV)
  readr::write_csv(missing_core, OUT_MISSING_CSV)
  message("Saved station summary QA: ", OUT_SUMMARY_CSV)
  message("Saved missing-field QA: ", OUT_MISSING_CSV)
}

if (WRITE_RDS) {
  saveRDS(scan_stations, OUT_SCAN_RDS)
  saveRDS(snow_pillows, OUT_SNOW_RDS)
  message("Saved SCAN RDS: ", OUT_SCAN_RDS)
  message("Saved snow pillows RDS: ", OUT_SNOW_RDS)
}

if (WRITE_GPKG) {
  sf::st_write(
    scan_stations,
    OUT_SCAN_GPKG,
    layer = "scan_stations_wgs84",
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
  sf::st_write(
    snow_pillows,
    OUT_SNOW_GPKG,
    layer = "snow_pillows_wgs84",
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
  message("Saved SCAN GPKG: ", OUT_SCAN_GPKG)
  message("Saved snow pillows GPKG: ", OUT_SNOW_GPKG)
}


# ==== 14. Final summary =======================================================

message("\nDone: snow / soil climate station preprocessing complete.")
message("Processed RDS:")
message("  ", OUT_SCAN_RDS)
message("  ", OUT_SNOW_RDS)