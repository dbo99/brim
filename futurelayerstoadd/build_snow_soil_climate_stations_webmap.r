# ==== build_snow_soil_climate_stations_webmap.R =============================
#
# Purpose:
#   Build one compact sf data frame for a Leaflet webmap by combining:
#     1) USDA NRCS / NWCC active SNOTEL stations
#     2) USDA NRCS / NWCC active SCAN stations
#     3) CA DWR / CDEC / California Snow Data active snow sensor metadata
#
# Design notes:
#   - This script intentionally does NOT build popup HTML.
#   - It keeps clean URL fields and compact metadata so popups can be built
#     later in the main Leaflet script.
#   - The output schema is designed to allow future station types such as
#     CDEC snow courses, DWR/CDEC precipitation stations, or other local
#     snow-pillow networks to be appended later.
#
# Outputs:
#   data_processed_stations/snow_soil_climate_stations_webmap.rds
#   data_processed_stations/snow_soil_climate_stations_webmap.gpkg
#   data_processed_stations/snow_soil_climate_stations_webmap.geojson
#
# ============================================================================


# ==== 1: Packages ===========================================================

library(sf)
library(dplyr)
library(stringr)
library(glue)
library(jsonlite)
library(readr)
library(tibble)


# ==== 2: User settings ======================================================

OUT_DIR <- "data_processed_stations"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# States to keep from NRCS/NWCC SNOTEL and SCAN layers.
# CA = 6, NV = 32, OR = 41
STATE_FIPS_KEEP <- c(6, 32, 41)

# USDA NRCS / NWCC ArcGIS service base.
NWCC_BASE <- "https://services.arcgis.com/SXbDpmb7xQkk44JV/ArcGIS/rest/services"

# CA Open Data / CNRA CKAN API endpoint.
CKAN_API_URL <- "https://data.cnra.ca.gov/api/3/action/datastore_search"

# California Snow Data -> Station Metadata resource ID.
# This is the CDEC active snow sensor station metadata table.
CDEC_SNOW_METADATA_RESOURCE_ID <- "49baa289-3cc3-439a-af7a-727486dbf303"

# Direct CSV fallback for CDEC metadata.
CDEC_METADATA_CSV_URL <- paste0(
  "https://data.cnra.ca.gov/dataset/797db683-e9f1-4c4b-93f9-ee8f065313e7/",
  "resource/49baa289-3cc3-439a-af7a-727486dbf303/download/",
  "snow-station-metadata.csv"
)


# ==== 3: Standard output schema ============================================
#
# These are the columns the final combined object will keep.
# Shapefiles truncate names, so RDS/GPKG/GeoJSON are preferred.

STANDARD_FIELDS <- c(
  "station_uid",
  "station_name",
  "station_type",
  "station_type_label",
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


# ==== 4: General helper functions ==========================================

clean_chr <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  na_if(x, "")
}

clean_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

sanitize_id <- function(x) {
  x |>
    as.character() |>
    str_replace_all("[^A-Za-z0-9]+", "_") |>
    str_replace_all("^_+|_+$", "")
}

normalize_names <- function(x) {
  x |>
    str_replace_all("[^A-Za-z0-9]+", "_") |>
    str_replace_all("^_+|_+$", "") |>
    str_to_lower()
}

fips_to_state <- function(fips) {
  case_when(
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
    select(any_of(standard_fields))
}

# Get a field from a data frame/sf object using exact candidate names.
get_field <- function(x, candidates, default = NA_character_) {
  hit <- intersect(candidates, names(x))
  if (length(hit) == 0) {
    rep(default, nrow(x))
  } else {
    x[[hit[1]]]
  }
}

# Get a field using flexible regex patterns against normalized names.
pick_col <- function(df, patterns, required = FALSE, default = NA_character_) {
  nm_original <- names(df)
  nm_clean <- normalize_names(nm_original)
  hit <- NULL
  
  for (pat in patterns) {
    idx <- which(str_detect(nm_clean, regex(pat, ignore_case = TRUE)))
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
        paste(nm_original, collapse = "\n")
      )
    }
    return(rep(default, nrow(df)))
  }
  
  df[[hit]]
}


# ==== 5: NRCS / NWCC helper functions ======================================

build_arcgis_geojson_url <- function(service_name,
                                     layer_id = 0,
                                     where = "1=1",
                                     out_fields = "*",
                                     out_sr = 4326) {
  base_url <- glue("{NWCC_BASE}/{service_name}/FeatureServer/{layer_id}/query")
  
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
  state_where <- glue("fipsStateNumber IN ({paste(state_fips_keep, collapse = ',')})")
  
  url <- build_arcgis_geojson_url(
    service_name = service_name,
    where = state_where
  )
  
  message("Reading NRCS/NWCC layer: ", service_name)
  message(url)
  
  st_read(url, quiet = TRUE) |>
    st_transform(4326)
}

parse_nwcc_site_num <- function(station_triplet, fallback_id = NA_character_) {
  station_triplet <- clean_chr(station_triplet)
  fallback_id <- clean_chr(fallback_id)
  
  # Typical NWCC stationTriplet pattern is like "615:NV:SNTL".
  site_from_triplet <- str_extract(station_triplet, "^\\d+")
  
  # Fallback handles strings that include embedded digits.
  site_from_fallback <- str_extract(fallback_id, "\\d+")
  
  coalesce(site_from_triplet, site_from_fallback)
}

make_nwcc_site_url <- function(site_num) {
  site_num <- clean_chr(site_num)
  ifelse(
    is.na(site_num),
    NA_character_,
    glue("https://wcc.sc.egov.usda.gov/nwcc/site?sitenum={site_num}")
  )
}

standardize_nwcc_station_layer <- function(x,
                                           station_type,
                                           station_type_label,
                                           source_layer,
                                           provider = "USDA NRCS NWCC",
                                           source_system = "NRCS NWCC",
                                           network_group = "Snow / soil climate stations") {
  x <- st_transform(x, 4326)
  coords <- st_coordinates(st_geometry(x))
  
  station_triplet <- clean_chr(get_field(x, c("stationTriplet", "station_triplet")))
  shef_id <- clean_chr(get_field(x, c("shefId", "shef_id", "shefid")))
  
  fallback_id <- clean_chr(get_field(
    x,
    c("stationId", "station_id", "siteNum", "site_num", "site_no", "id", "name")
  ))
  
  site_num <- parse_nwcc_site_num(
    station_triplet = station_triplet,
    fallback_id = coalesce(fallback_id, shef_id)
  )
  
  fips_state <- get_field(x, c("fipsStateNumber", "fips_state_number", "stateFips"))
  state <- clean_chr(get_field(x, c("stateCode", "state_code", "state")))
  state <- coalesce(state, fips_to_state(fips_state))
  
  latitude <- clean_num(get_field(x, c("latitude", "lat")))
  longitude <- clean_num(get_field(x, c("longitude", "lon", "long")))
  latitude <- coalesce(latitude, coords[, 2])
  longitude <- coalesce(longitude, coords[, 1])
  
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
  
  out <- x |>
    mutate(
      station_uid = sanitize_id(glue("NRCS_{station_type}_{site_num}")),
      station_name = station_name,
      station_type = station_type,
      station_type_label = station_type_label,
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
      source_url = glue("{NWCC_BASE}/{source_layer}/FeatureServer/0")
    ) |>
    select_standard_fields()
  
  out
}


# ==== 6: CDEC / California Snow Data helper functions =======================

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
    stop(
      "CDEC CSV fallback failed. Try opening the CSV URL in a browser ",
      "and saving it manually, then point read_csv() at the local file."
    )
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
    glue("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id={station_id}")
  )
}

make_cdec_data_url <- function(station_id) {
  station_id <- clean_chr(station_id)
  ifelse(
    is.na(station_id),
    NA_character_,
    glue("https://cdec.water.ca.gov/dynamicapp/QueryDaily?s={station_id}")
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
      patterns = c(
        "^station_id$",
        "^station$",
        "^sta$",
        "station.*id",
        "station.*code",
        "^id$"
      ),
      required = TRUE
    )
  )
  
  station_name <- clean_chr(
    pick_col(
      cdec_raw,
      patterns = c(
        "station.*name",
        "^name$",
        "site.*name"
      ),
      required = FALSE
    )
  )
  
  latitude <- clean_num(
    pick_col(
      cdec_raw,
      patterns = c(
        "^latitude$",
        "^lat$",
        "lat_dd",
        "decimal.*lat"
      ),
      required = TRUE
    )
  )
  
  longitude <- clean_num(
    pick_col(
      cdec_raw,
      patterns = c(
        "^longitude$",
        "^lon$",
        "^long$",
        "lng",
        "long_dd",
        "decimal.*lon"
      ),
      required = TRUE
    )
  )
  
  elevation_ft <- clean_num(
    pick_col(
      cdec_raw,
      patterns = c(
        "^elevation$",
        "^elev$",
        "elevation.*ft",
        "elev.*ft"
      ),
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
    pick_col(
      cdec_raw,
      patterns = c(
        "start.*date",
        "begin.*date",
        "period.*start",
        "record.*start"
      ),
      required = FALSE
    )
  )
  
  end_date <- clean_chr(
    pick_col(
      cdec_raw,
      patterns = c(
        "end.*date",
        "period.*end",
        "record.*end"
      ),
      required = FALSE
    )
  )
  
  basin <- clean_chr(
    pick_col(
      cdec_raw,
      patterns = c(
        "^basin$",
        "river.*basin",
        "watershed",
        "hydrologic.*area"
      ),
      required = FALSE
    )
  )
  
  county <- clean_chr(
    pick_col(
      cdec_raw,
      patterns = c(
        "^county$",
        "county.*name"
      ),
      required = FALSE
    )
  )
  
  # Optional status field if present; otherwise active because this metadata
  # resource is described as active sensor station metadata.
  status <- clean_chr(
    pick_col(
      cdec_raw,
      patterns = c(
        "^status$",
        "station.*status",
        "active"
      ),
      required = FALSE,
      default = "active"
    )
  )
  status <- coalesce(status, "active")
  
  cdec_df <- tibble(
    station_uid = sanitize_id(glue("CDEC_snow_sensor_{station_id}")),
    station_name = station_name,
    station_type = "cdec_snow_sensor",
    station_type_label = "CDEC snow sensor",
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
    filter(
      !is.na(latitude),
      !is.na(longitude),
      longitude < 0,
      latitude > 30,
      latitude < 45
    ) |>
    distinct(station_uid, .keep_all = TRUE)
  
  st_as_sf(
    cdec_df,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
    select_standard_fields()
}


# ==== 7: Read and standardize all three source groups =======================

# ---- 7.1 NRCS active SNOTEL ----

snotel_raw <- read_nwcc_layer("stations_SNTL_ACTIVE")

snotel_web <- standardize_nwcc_station_layer(
  snotel_raw,
  station_type = "snotel",
  station_type_label = "SNOTEL",
  source_layer = "stations_SNTL_ACTIVE"
)

# ---- 7.2 NRCS active SCAN ----

scan_raw <- read_nwcc_layer("stations_SCAN_ACTIVE")

scan_web <- standardize_nwcc_station_layer(
  scan_raw,
  station_type = "scan",
  station_type_label = "SCAN",
  source_layer = "stations_SCAN_ACTIVE"
)

# ---- 7.3 CDEC active snow sensor metadata ----

cdec_raw <- fetch_cdec_station_metadata()

message("Raw CDEC metadata columns:")
print(names(cdec_raw))

saveRDS(
  cdec_raw,
  file.path(OUT_DIR, "cdec_snow_station_metadata_raw.rds")
)

cdec_web <- standardize_cdec_snow_metadata(cdec_raw)


# ==== 8: Combine into one webmap-ready sf object ============================

stations_web <- bind_rows(
  snotel_web,
  scan_web,
  cdec_web
) |>
  st_transform(4326) |>
  arrange(state, station_type, station_name)

# Plain data frame version, if you want non-spatial inspection.
stations_web_df <- stations_web |>
  st_drop_geometry()


# ==== 9: QA summaries =======================================================

message("\nStation counts by source/type/state:")
print(
  stations_web |>
    st_drop_geometry() |>
    count(provider, station_type_label, state, sort = TRUE)
)

message("\nMissing core fields:")
print(
  stations_web |>
    st_drop_geometry() |>
    summarise(
      n_total = n(),
      missing_station_name = sum(is.na(station_name)),
      missing_station_id = sum(is.na(station_id)),
      missing_elevation_ft = sum(is.na(elevation_ft)),
      missing_site_page_url = sum(is.na(site_page_url)),
      missing_latitude = sum(is.na(latitude)),
      missing_longitude = sum(is.na(longitude))
    )
)

message("\nPotential duplicate station_uid values:")
dupes <- stations_web |>
  st_drop_geometry() |>
  count(station_uid) |>
  filter(n > 1)
print(dupes)

message("\nCDEC operator / O&M agency counts:")
print(
  stations_web |>
    st_drop_geometry() |>
    filter(source_system == "CDEC") |>
    count(operator_agency, sort = TRUE)
)

# Optional: inspect stations with missing names.
missing_names <- stations_web |>
  st_drop_geometry() |>
  filter(is.na(station_name))

if (nrow(missing_names) > 0) {
  message("\nStations with missing station_name:")
  print(missing_names)
}


# ==== 10: Save outputs ======================================================

# Best format for reading back into R.
saveRDS(
  stations_web,
  file.path(OUT_DIR, "snow_soil_climate_stations_webmap.rds")
)

# Non-spatial compact table for QA or quick review.
readr::write_csv(
  stations_web_df,
  file.path(OUT_DIR, "snow_soil_climate_stations_webmap_attributes.csv")
)

# Recommended GIS interchange format.
st_write(
  stations_web,
  file.path(OUT_DIR, "snow_soil_climate_stations_webmap.gpkg"),
  layer = "stations",
  delete_dsn = TRUE
)

# Web-friendly format, if you later want to load it directly.
st_write(
  stations_web,
  file.path(OUT_DIR, "snow_soil_climate_stations_webmap.geojson"),
  delete_dsn = TRUE
)

# Shapefile is intentionally omitted as a default because it will truncate
# several important field names. Uncomment only if needed.
# st_write(
#   stations_web,
#   file.path(OUT_DIR, "snow_soil_climate_stations_webmap.shp"),
#   delete_dsn = TRUE
# )


# ==== 11: Optional quick Leaflet preview ====================================
#
# This is only a sanity check. The final map can build richer popups later.

if (requireNamespace("leaflet", quietly = TRUE)) {
  library(leaflet)
  
  preview_label <- glue(
    "{stations_web$station_name} | {stations_web$station_type_label} | {stations_web$elevation_ft} ft"
  )
  
  leaflet(stations_web) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addCircleMarkers(
      radius = 5,
      stroke = TRUE,
      weight = 1,
      fillOpacity = 0.85,
      label = preview_label,
      popup = ~paste0(
        "<strong>", station_name, "</strong><br>",
        "Type: ", station_type_label, "<br>",
        "Provider: ", provider, "<br>",
        "Station ID: ", station_id, "<br>",
        "Elevation: ", elevation_ft, " ft<br>",
        "<a href='", site_page_url, "' target='_blank' rel='noopener noreferrer'>Open station page</a>"
      ),
      group = "Snow / soil climate stations"
    ) |>
    addLayersControl(
      overlayGroups = c("Snow / soil climate stations"),
      options = layersControlOptions(collapsed = FALSE)
    )
}
