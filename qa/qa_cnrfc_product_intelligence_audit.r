# ==== qa_cnrfc_product_intelligence_audit.r ================================
##
## PURPOSE:
##   Build a high-level CNRFC feature and product-intelligence audit for BRIM.
##
## WHY THIS EXISTS:
##   CNRFC uses overlapping short IDs for several different feature families:
##   basin polygons, river forecast points, reservoir points, precip stations,
##   and BRIM's own crosswalked CDEC/USGS/reservoir features.  Before changing
##   the map UI, we need durable tables that say:
##     - which CNRFC-ish IDs BRIM already knows about;
##     - where those IDs came from;
##     - where IDs collide across feature families;
##     - which CNRFC product families are plausible candidates; and
##     - which product-group bins are likely worth showing later.
##
## DESIGN:
##   This script is audit-only.  It does not edit cache products, popups, or the
##   final map.  It intentionally creates tables for review before any layer
##   design decisions are made.
##
## OUTPUTS:
##   04_processed_data/qa/cnrfc_feature_inventory_<timestamp>.csv
##   04_processed_data/qa/cnrfc_feature_master_<timestamp>.csv
##   04_processed_data/qa/cnrfc_id_collision_check_<timestamp>.csv
##   04_processed_data/qa/cnrfc_candidate_product_catalog_<timestamp>.csv
##   04_processed_data/qa/cnrfc_product_group_bins_by_feature_<timestamp>.csv
##   04_processed_data/qa/cnrfc_raw_source_summary_<timestamp>.csv
##   04_processed_data/qa/cnrfc_field_inventory_<timestamp>.csv

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
  library(sf)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
})

RUN_TS <- make_timestamp()

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
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

pt_find_col <- function(df, candidates) {
  nms <- names(df)
  hit <- nms[tolower(nms) %in% tolower(candidates)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

pt_col <- function(df, candidates, default = NA_character_) {
  nm <- pt_find_col(df, candidates)
  if (is.na(nm)) rep(default, nrow(df)) else df[[nm]]
}

pt_drop_geom <- function(x) {
  if (inherits(x, "sf")) sf::st_drop_geometry(x) else as.data.frame(x)
}

pt_norm_feature_type <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x == ""] <- NA_character_
  x
}

pt_feature_key <- function(feature_type, id) {
  id <- pt_clean_id(id)
  feature_type <- pt_norm_feature_type(feature_type)
  out <- paste0(feature_type, ":", id)
  out[is.na(id) | is.na(feature_type)] <- NA_character_
  out
}

pt_url_id <- function(id) {
  utils::URLencode(pt_clean_id(id), reserved = TRUE)
}

pt_file_exists <- function(...) {
  p <- file.path(...)
  if (file.exists(p)) p else NA_character_
}

pt_na_blank <- function(x) {
  x <- pt_chr(x)
  x[x == ""] <- NA_character_
  x
}

pt_first_present <- function(df, candidates, default = NA_character_) {
  nm <- pt_find_col(df, candidates)
  if (is.na(nm)) rep(default, nrow(df)) else df[[nm]]
}

# ==== 2. Output paths ========================================================

out_feature_inventory <- file.path(DIR$qa, paste0("cnrfc_feature_inventory_", RUN_TS, ".csv"))
out_feature_master <- file.path(DIR$qa, paste0("cnrfc_feature_master_", RUN_TS, ".csv"))
out_id_collision <- file.path(DIR$qa, paste0("cnrfc_id_collision_check_", RUN_TS, ".csv"))
out_product_catalog <- file.path(DIR$qa, paste0("cnrfc_candidate_product_catalog_", RUN_TS, ".csv"))
out_product_bins <- file.path(DIR$qa, paste0("cnrfc_product_group_bins_by_feature_", RUN_TS, ".csv"))
out_raw_summary <- file.path(DIR$qa, paste0("cnrfc_raw_source_summary_", RUN_TS, ".csv"))
out_field_inventory <- file.path(DIR$qa, paste0("cnrfc_field_inventory_", RUN_TS, ".csv"))

# ==== 3. Parsers for raw CNRFC text files ===================================

pt_parse_cnrfc_river_catalog <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(tibble())
  }

  lines_raw <- readLines(path, warn = FALSE)
  lines <- lines_raw[nzchar(trimws(lines_raw))]

  nwsid <- stringr::str_extract(lines, "^[A-Z0-9]{5}\\b")
  line_after_nwsid <- stringr::str_trim(stringr::str_replace(lines, "^[A-Z0-9]{5}\\b\\s*", ""))
  datatransmission <- stringr::str_sub(line_after_nwsid, 1, 1)

  quoted_fields <- stringr::str_match_all(lines, '"([^"]*)"')

  channel <- purrr::map_chr(quoted_fields, function(x) {
    if (nrow(x) >= 1 && ncol(x) >= 2) stringr::str_trim(x[1, 2]) else NA_character_
  })

  location <- purrr::map_chr(quoted_fields, function(x) {
    if (nrow(x) >= 2 && ncol(x) >= 2) stringr::str_trim(x[2, 2]) else NA_character_
  })

  optional_label <- purrr::map_chr(quoted_fields, function(x) {
    if (nrow(x) >= 3 && ncol(x) >= 2) stringr::str_trim(x[3, 2]) else NA_character_
  })

  coord_matches <- stringr::str_match(lines, "(-?\\d+\\.\\d{5})\\s+(-?\\d+\\.\\d{5})\\s+(-?\\d{1,5})")

  lat <- suppressWarnings(as.numeric(coord_matches[, 2]))
  lon <- suppressWarnings(as.numeric(coord_matches[, 3]))
  elev_ft <- suppressWarnings(as.numeric(coord_matches[, 4]))

  gage_type_raw <- stringr::str_match(
    lines,
    "(?:-?\\d+(?:\\.\\d{5})?\\s+){3}([A-Za-z]+\\s+[A-Za-z]+)"
  )[, 2]

  gage_type <- stringr::str_to_lower(stringr::str_squish(gage_type_raw))
  gage_class1 <- stringr::str_extract(gage_type, "^[a-z]+")
  gage_class2 <- stringr::str_extract(gage_type, "(?<=\\s)[a-z]+$")

  display_name <- dplyr::case_when(
    !is.na(optional_label) & optional_label != "" ~ optional_label,
    !is.na(channel) & channel != "" & !is.na(location) & location != "" ~ paste0(channel, " - ", location),
    !is.na(channel) & channel != "" ~ channel,
    !is.na(location) & location != "" ~ location,
    TRUE ~ nwsid
  )

  feature_type <- dplyr::case_when(
    gage_class1 == "reservoir" ~ "cnrfc_reservoir_point",
    gage_class1 == "river"     ~ "cnrfc_river_point",
    gage_class1 == "special"   ~ "cnrfc_special_point",
    TRUE                       ~ "cnrfc_river_catalog_other"
  )

  tibble(
    inventory_source = "raw:gagedata_river_txt",
    source_path = path,
    feature_type = feature_type,
    cnrfc_id = pt_clean_id(nwsid),
    display_name = pt_na_blank(display_name),
    river_name = pt_na_blank(channel),
    location_name = pt_na_blank(location),
    forecast_group = NA_character_,
    river_group = NA_character_,
    raw_family = "river_catalog",
    raw_kind = pt_na_blank(gage_type_raw),
    raw_class1 = pt_na_blank(gage_class1),
    raw_class2 = pt_na_blank(gage_class2),
    datatransmission = pt_na_blank(datatransmission),
    cdec_id = NA_character_,
    usgs_id = NA_character_,
    lat = lat,
    lon = lon,
    elev_ft = elev_ft,
    has_valid_coordinate = !is.na(lat) & !is.na(lon) & !(lat == 0 & lon == 0),
    existing_popup_links = NA_character_,
    note = "Parsed from CNRFC river/reservoir text catalog."
  ) |>
    mutate(feature_key = pt_feature_key(.data$feature_type, .data$cnrfc_id)) |>
    select(.data$feature_key, everything())
}

pt_parse_cnrfc_precip_catalog <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(tibble())
  }

  lines_raw <- readLines(path, warn = FALSE)
  lines <- lines_raw[nzchar(trimws(lines_raw))]

  nwsid <- stringr::str_extract(lines, "^[A-Z0-9]{2,5}\\b")
  line_after_nwsid <- stringr::str_trim(stringr::str_replace(lines, "^[A-Z0-9]{2,5}\\b\\s*", ""))
  datatransmission <- stringr::str_sub(line_after_nwsid, 1, 1)
  station <- stringr::str_match(lines, '"([^"]*)"')[, 2] |>
    stringr::str_trim()

  coord_matches <- stringr::str_match(lines, "(-?\\d+\\.\\d{5})\\s+(-?\\d+\\.\\d{5})\\s+(-?\\d{1,5})")
  lat <- suppressWarnings(as.numeric(coord_matches[, 2]))
  lon <- suppressWarnings(as.numeric(coord_matches[, 3]))
  elev_ft <- suppressWarnings(as.numeric(coord_matches[, 4]))
  state <- stringr::str_extract(lines, "[A-Z]{2}$")

  tibble(
    inventory_source = "raw:gagedata_precip_txt",
    source_path = path,
    feature_type = "cnrfc_precip_station_catalog",
    cnrfc_id = pt_clean_id(nwsid),
    display_name = pt_na_blank(station),
    river_name = NA_character_,
    location_name = NA_character_,
    forecast_group = NA_character_,
    river_group = NA_character_,
    raw_family = "precip_catalog",
    raw_kind = pt_na_blank(state),
    raw_class1 = NA_character_,
    raw_class2 = NA_character_,
    datatransmission = pt_na_blank(datatransmission),
    cdec_id = NA_character_,
    usgs_id = NA_character_,
    lat = lat,
    lon = lon,
    elev_ft = elev_ft,
    has_valid_coordinate = !is.na(lat) & !is.na(lon) & !(lat == 0 & lon == 0),
    existing_popup_links = NA_character_,
    note = "Parsed from older broad CNRFC precip/station text catalog."
  ) |>
    mutate(feature_key = pt_feature_key(.data$feature_type, .data$cnrfc_id)) |>
    select(.data$feature_key, everything())
}

pt_parse_cnrfc_precip_morecurrent <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(tibble())
  }

  x <- tryCatch(
    readr::read_delim(path, delim = ";", trim_ws = TRUE, show_col_types = FALSE),
    error = function(e) NULL
  )

  if (is.null(x) || nrow(x) == 0) return(tibble())

  names(x) <- trimws(names(x))

  id_col <- pt_find_col(x, c("NWSID", "id", "nws_id"))
  lat_col <- pt_find_col(x, c("LAT", "lat", "latitude"))
  lon_col <- pt_find_col(x, c("LON", "lon", "longitude"))

  if (is.na(id_col)) return(tibble())

  lat <- if (!is.na(lat_col)) suppressWarnings(as.numeric(x[[lat_col]])) else rep(NA_real_, nrow(x))
  lon <- if (!is.na(lon_col)) suppressWarnings(as.numeric(x[[lon_col]])) else rep(NA_real_, nrow(x))

  tibble(
    inventory_source = "raw:gagedata_precip_morecurrent_txt",
    source_path = path,
    feature_type = "cnrfc_precip_station_recent_index",
    cnrfc_id = pt_clean_id(x[[id_col]]),
    display_name = pt_clean_id(x[[id_col]]),
    river_name = NA_character_,
    location_name = NA_character_,
    forecast_group = NA_character_,
    river_group = NA_character_,
    raw_family = "precip_recent_index",
    raw_kind = NA_character_,
    raw_class1 = NA_character_,
    raw_class2 = NA_character_,
    datatransmission = NA_character_,
    cdec_id = NA_character_,
    usgs_id = NA_character_,
    lat = lat,
    lon = lon,
    elev_ft = NA_real_,
    has_valid_coordinate = !is.na(lat) & !is.na(lon) & !(lat == 0 & lon == 0),
    existing_popup_links = NA_character_,
    note = "Parsed from simplified more-current CNRFC precip coordinate/index text file; not proof of recent reporting."
  ) |>
    mutate(feature_key = pt_feature_key(.data$feature_type, .data$cnrfc_id)) |>
    select(.data$feature_key, everything())
}

# ==== 4. Readers for existing BRIM cache products ===========================

pt_read_cache_feature_inventory <- function(cache_file, feature_type, source_label, id_candidates,
                                            name_candidates = character(),
                                            river_candidates = character(),
                                            location_candidates = character(),
                                            forecast_group_candidates = character(),
                                            river_group_candidates = character(),
                                            cdec_candidates = character(),
                                            usgs_candidates = character(),
                                            raw_kind_candidates = character(),
                                            raw_class1_candidates = character(),
                                            raw_class2_candidates = character()) {
  path <- file.path(DIR$cache_last, cache_file)

  if (!file.exists(path)) {
    return(tibble())
  }

  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj) || nrow(obj) == 0) {
    return(tibble())
  }

  df <- pt_drop_geom(obj)

  id <- pt_clean_id(pt_col(df, id_candidates))
  display_name <- pt_na_blank(pt_col(df, name_candidates))
  river_name <- pt_na_blank(pt_col(df, river_candidates))
  location_name <- pt_na_blank(pt_col(df, location_candidates))
  forecast_group <- pt_na_blank(pt_col(df, forecast_group_candidates))
  river_group <- pt_na_blank(pt_col(df, river_group_candidates))
  cdec_id <- pt_clean_id(pt_col(df, cdec_candidates))
  usgs_id <- pt_clean_id(pt_col(df, usgs_candidates))
  raw_kind <- pt_na_blank(pt_col(df, raw_kind_candidates))
  raw_class1 <- pt_na_blank(pt_col(df, raw_class1_candidates))
  raw_class2 <- pt_na_blank(pt_col(df, raw_class2_candidates))

  ## Coordinates are available only for point layers that preserved lat/lon.
  lat <- suppressWarnings(as.numeric(pt_col(df, c("lat", "LAT", "latitude", "dec_lat_va"), NA_character_)))
  lon <- suppressWarnings(as.numeric(pt_col(df, c("lon", "LON", "longitude", "dec_long_va"), NA_character_)))
  elev_ft <- suppressWarnings(as.numeric(pt_col(df, c("elev_ft", "Elevation", "elevation", "elev", "ELEV"), NA_character_)))

  popup_link_fields <- names(df)[grepl("url|link", names(df), ignore.case = TRUE)]
  existing_popup_links <- if (length(popup_link_fields) > 0) {
    paste(popup_link_fields, collapse = ";")
  } else if ("popup_html" %in% names(df)) {
    "popup_html"
  } else {
    NA_character_
  }

  tibble(
    inventory_source = paste0("cache:", cache_file),
    source_path = path,
    feature_type = feature_type,
    cnrfc_id = id,
    display_name = display_name,
    river_name = river_name,
    location_name = location_name,
    forecast_group = forecast_group,
    river_group = river_group,
    raw_family = source_label,
    raw_kind = raw_kind,
    raw_class1 = raw_class1,
    raw_class2 = raw_class2,
    datatransmission = pt_na_blank(pt_col(df, c("datatransmission", "data_transmission"))),
    cdec_id = cdec_id,
    usgs_id = usgs_id,
    lat = lat,
    lon = lon,
    elev_ft = elev_ft,
    has_valid_coordinate = !is.na(lat) & !is.na(lon) & !(lat == 0 & lon == 0),
    existing_popup_links = existing_popup_links,
    note = "Read from existing BRIM map-ready cache."
  ) |>
    mutate(feature_key = pt_feature_key(.data$feature_type, .data$cnrfc_id)) |>
    select(.data$feature_key, everything())
}

pt_cache_field_inventory <- function(cache_file) {
  path <- file.path(DIR$cache_last, cache_file)

  if (!file.exists(path)) {
    return(tibble(
      cache_file = cache_file,
      field = NA_character_,
      nonblank_rows = NA_integer_,
      rows = NA_integer_,
      note = "missing cache file"
    ))
  }

  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj)) {
    return(tibble(
      cache_file = cache_file,
      field = NA_character_,
      nonblank_rows = NA_integer_,
      rows = NA_integer_,
      note = "could not read cache file"
    ))
  }

  df <- pt_drop_geom(obj)

  tibble(
    cache_file = cache_file,
    field = names(df),
    nonblank_rows = vapply(df, function(v) {
      vv <- trimws(as.character(v))
      sum(!is.na(vv) & vv != "")
    }, numeric(1)),
    rows = nrow(df),
    note = "cache field inventory"
  )
}

# ==== 5. Build feature inventory ============================================

raw_cnrfc_dir <- file.path(DIR$raw, "cnrfc")

raw_river_path <- pt_file_exists(raw_cnrfc_dir, "gage_data_river.txt")
raw_precip_path <- pt_file_exists(raw_cnrfc_dir, "gage_data_precip.txt")
raw_precip_morecurrent_path <- pt_file_exists(raw_cnrfc_dir, "gage_data_precip_morecurrent.txt")

feature_inventory <- bind_rows(
  pt_read_cache_feature_inventory(
    cache_file = "cnrfc_basins_map.rds",
    feature_type = "cnrfc_basin",
    source_label = "basin_polygon_cache",
    id_candidates = c("Basin", "NWSID", "nwsid", "nws_id", "id", "ID"),
    name_candidates = c("Descript", "Description", "Name", "name", "BasinName", "basin_name"),
    forecast_group_candidates = c("ForecastGr", "ForecastGroup", "forecast_group", "FcstGroup")
  ),
  pt_read_cache_feature_inventory(
    cache_file = "cnrfc_fnf_delta_map.rds",
    feature_type = "cnrfc_fnf_sierra_delta_basin",
    source_label = "fnf_sierra_delta_basin_cache",
    id_candidates = c("nws5id", "NWSID", "nwsid", "Basin", "id", "ID"),
    name_candidates = c("River", "river", "Name", "name", "res", "Reservoir"),
    river_candidates = c("River", "river"),
    raw_kind_candidates = c("res", "Reservoir")
  ),
  pt_read_cache_feature_inventory(
    cache_file = "cnrfc_stream_map.rds",
    feature_type = "cnrfc_stream_or_reservoir_point_cache",
    source_label = "stream_reservoir_point_cache",
    id_candidates = c("nwsid", "NWSID", "nws_id", "id", "ID"),
    name_candidates = c("nickname", "Nickname", "display_name", "Name", "name"),
    river_candidates = c("channel", "river", "River"),
    location_candidates = c("loc", "location", "Location"),
    cdec_candidates = c("cdec_id", "CDEC_ID", "station_id"),
    raw_kind_candidates = c("gage_type", "raw_type"),
    raw_class1_candidates = c("gage_class1", "class1"),
    raw_class2_candidates = c("gage_class2", "class2")
  ),
  pt_read_cache_feature_inventory(
    cache_file = "cnrfc_precip_map.rds",
    feature_type = "cnrfc_precip_station_cache",
    source_label = "precip_station_cache",
    id_candidates = c("nwsid", "NWSID", "nws_id", "id", "ID"),
    name_candidates = c("station", "Station", "station_name", "Name", "name"),
    raw_kind_candidates = c("state", "State")
  ),
  pt_read_cache_feature_inventory(
    cache_file = "cdec_reservoir_stations_map.rds",
    feature_type = "brim_reservoir_layer_cdec_cnrfc_crosswalk",
    source_label = "existing_reservoir_ops_cache",
    id_candidates = c("cnrfc_nws_id", "nws_id", "nwsid", "NWSID", "cnrfc_id"),
    name_candidates = c("station_name", "cdec_station_name", "name", "Name"),
    cdec_candidates = c("cdec_id", "station_id", "CDEC_ID"),
    usgs_candidates = c("usgs_site_no", "usgs_id", "site_no"),
    raw_kind_candidates = c("operator", "type", "Type")
  ),
  pt_parse_cnrfc_river_catalog(raw_river_path),
  pt_parse_cnrfc_precip_catalog(raw_precip_path),
  pt_parse_cnrfc_precip_morecurrent(raw_precip_morecurrent_path)
) |>
  filter(!is.na(.data$cnrfc_id), .data$cnrfc_id != "") |>
  mutate(
    run_timestamp = RUN_TS,
    feature_type = pt_norm_feature_type(.data$feature_type),
    product_scope_hint = case_when(
      grepl("basin", .data$feature_type) & grepl("fnf", .data$feature_type) ~ "water_supply_fnf_basin",
      grepl("basin", .data$feature_type) ~ "basin_polygon",
      grepl("reservoir", .data$feature_type) ~ "reservoir_or_reservoir_crosswalk",
      grepl("river", .data$feature_type) ~ "river_forecast_point",
      grepl("precip", .data$feature_type) ~ "precip_station",
      TRUE ~ "other_cnrfc_feature"
    )
  )

# Prefer cache rows over raw rows when building one master feature row per
# namespaced feature type + CNRFC ID.  Raw rows are still preserved in the full
# feature inventory and collision tables.
source_priority <- c(
  "cache:cnrfc_basins_map.rds" = 1L,
  "cache:cnrfc_fnf_delta_map.rds" = 1L,
  "cache:cnrfc_stream_map.rds" = 1L,
  "cache:cnrfc_precip_map.rds" = 1L,
  "cache:cdec_reservoir_stations_map.rds" = 1L,
  "raw:gagedata_river_txt" = 2L,
  "raw:gagedata_precip_txt" = 3L,
  "raw:gagedata_precip_morecurrent_txt" = 4L
)

feature_master <- feature_inventory |>
  mutate(
    source_rank = unname(source_priority[.data$inventory_source]),
    source_rank = if_else(is.na(.data$source_rank), 99L, .data$source_rank)
  ) |>
  arrange(.data$feature_type, .data$cnrfc_id, .data$source_rank) |>
  group_by(.data$feature_type, .data$cnrfc_id) |>
  summarise(
    feature_key = pt_first_nonblank(.data$feature_key),
    display_name = pt_first_nonblank(.data$display_name),
    river_name = pt_first_nonblank(.data$river_name),
    location_name = pt_first_nonblank(.data$location_name),
    forecast_group = pt_first_nonblank(.data$forecast_group),
    river_group = pt_first_nonblank(.data$river_group),
    raw_family = paste(sort(unique(na.omit(.data$raw_family))), collapse = ";"),
    raw_kind = pt_first_nonblank(.data$raw_kind),
    raw_class1 = pt_first_nonblank(.data$raw_class1),
    raw_class2 = pt_first_nonblank(.data$raw_class2),
    datatransmission = pt_first_nonblank(.data$datatransmission),
    cdec_id = pt_first_nonblank(.data$cdec_id),
    usgs_id = pt_first_nonblank(.data$usgs_id),
    lat = pt_first_num(.data$lat),
    lon = pt_first_num(.data$lon),
    elev_ft = pt_first_num(.data$elev_ft),
    has_valid_coordinate = any(.data$has_valid_coordinate, na.rm = TRUE),
    inventory_sources = paste(sort(unique(.data$inventory_source)), collapse = ";"),
    product_scope_hint = pt_first_nonblank(.data$product_scope_hint),
    source_rows = n(),
    run_timestamp = first(.data$run_timestamp),
    .groups = "drop"
  ) |>
  mutate(
    feature_key = if_else(
      is.na(.data$feature_key) | .data$feature_key == "",
      pt_feature_key(.data$feature_type, .data$cnrfc_id),
      .data$feature_key
    )
  )

# ==== 6. ID collision and raw-source summary ================================

id_collision <- feature_inventory |>
  group_by(.data$cnrfc_id) |>
  summarise(
    inventory_rows = n(),
    feature_type_count = n_distinct(.data$feature_type),
    inventory_source_count = n_distinct(.data$inventory_source),
    feature_types = paste(sort(unique(.data$feature_type)), collapse = ";"),
    inventory_sources = paste(sort(unique(.data$inventory_source)), collapse = ";"),
    display_names = paste(head(sort(unique(na.omit(.data$display_name))), 10), collapse = " | "),
    has_basin = any(grepl("basin", .data$feature_type), na.rm = TRUE),
    has_river_point = any(grepl("river", .data$feature_type), na.rm = TRUE),
    has_reservoir = any(grepl("reservoir", .data$feature_type), na.rm = TRUE),
    has_precip = any(grepl("precip", .data$feature_type), na.rm = TRUE),
    has_existing_reservoir_layer_crosswalk = any(.data$feature_type == "brim_reservoir_layer_cdec_cnrfc_crosswalk"),
    run_timestamp = RUN_TS,
    .groups = "drop"
  ) |>
  arrange(desc(.data$feature_type_count), desc(.data$inventory_rows), .data$cnrfc_id)

raw_summary <- feature_inventory |>
  group_by(.data$inventory_source, .data$feature_type, .data$raw_family, .data$raw_kind, .data$raw_class1, .data$raw_class2) |>
  summarise(
    rows = n(),
    unique_ids = n_distinct(.data$cnrfc_id),
    valid_coordinates = sum(.data$has_valid_coordinate, na.rm = TRUE),
    zero_or_missing_coordinates = sum(!.data$has_valid_coordinate, na.rm = TRUE),
    run_timestamp = RUN_TS,
    .groups = "drop"
  ) |>
  arrange(.data$inventory_source, desc(.data$rows))

field_inventory <- bind_rows(
  pt_cache_field_inventory("cnrfc_basins_map.rds"),
  pt_cache_field_inventory("cnrfc_fnf_delta_map.rds"),
  pt_cache_field_inventory("cnrfc_stream_map.rds"),
  pt_cache_field_inventory("cnrfc_precip_map.rds"),
  pt_cache_field_inventory("cdec_reservoir_stations_map.rds")
) |>
  mutate(run_timestamp = RUN_TS)

# ==== 7. Candidate CNRFC product catalog ====================================

pt_product_row <- function(feature, product_group, product_label, url, priority = 50L,
                           intended_popup_group = "supporting", notes = NA_character_) {
  tibble(
    feature_key = feature$feature_key,
    feature_type = feature$feature_type,
    cnrfc_id = feature$cnrfc_id,
    display_name = feature$display_name,
    product_group = product_group,
    product_label = product_label,
    candidate_url = url,
    priority = priority,
    intended_popup_group = intended_popup_group,
    availability_status = "not_checked",
    availability_note = notes,
    run_timestamp = RUN_TS
  )
}

pt_candidate_products_one <- function(feature) {
  id <- feature$cnrfc_id[[1]]
  fid <- pt_url_id(id)
  ftype <- feature$feature_type[[1]]
  out <- list()

  add <- function(product_group, product_label, url, priority = 50L,
                  intended_popup_group = "supporting", notes = NA_character_) {
    out[[length(out) + 1L]] <<- pt_product_row(
      feature = feature,
      product_group = product_group,
      product_label = product_label,
      url = url,
      priority = priority,
      intended_popup_group = intended_popup_group,
      notes = notes
    )
  }

  if (is.na(id) || id == "") return(tibble())

  ## General CNRFC pages that are useful for many point/basin IDs.  Some pages
  ## may load but say a selected product is unavailable; later live-check passes
  ## should parse availability before these become direct production popup links.
  if (grepl("basin", ftype) || grepl("river", ftype) || grepl("reservoir", ftype)) {
    add(
      "ensemble_landing",
      "CNRFC ensemble products page",
      paste0("https://www.cnrfc.noaa.gov/ensembleProduct.php?id=", fid),
      priority = 20L,
      intended_popup_group = "best_or_more_products",
      notes = "Landing page; later parser can identify enabled/disabled product buttons."
    )

    for (prod_id in 1:12) {
      add(
        paste0("ensemble_product_probe_", sprintf("%02d", prod_id)),
        paste0("CNRFC ensemble product ID ", prod_id),
        paste0("https://www.cnrfc.noaa.gov/ensembleProduct.php?id=", fid, "&prodID=", prod_id, "&years=1"),
        priority = 80L + prod_id,
        intended_popup_group = "audit_only",
        notes = "Candidate for availability audit; do not expose all product IDs in popups without review."
      )
    }
  }

  if (grepl("basin", ftype)) {
    add(
      "basin_mean_temperature",
      "CNRFC basin mean temperature forecast",
      paste0("https://www.cnrfc.noaa.gov/temperaturePlots_hc.php?id=", fid),
      priority = 10L,
      intended_popup_group = "best_products",
      notes = "Basin-scale temperature product candidate."
    )

    add(
      "basin_water_supply_fnf",
      "CNRFC water supply / full-natural-flow forecast candidate",
      paste0("https://www.cnrfc.noaa.gov/ensembleProduct.php?id=", fid, "&prodID=9&years=1"),
      priority = 15L,
      intended_popup_group = "best_products",
      notes = "Legacy/current BRIM popup used prodID=9 for water-supply/FNF; live audit should confirm product label and availability."
    )

    add(
      "basin_water_supply_wy_accum_vol_candidate",
      "CNRFC WY accumulated volume candidate",
      paste0("https://www.cnrfc.noaa.gov/ensembleProduct.php?id=", fid, "&prodID=10&years=1"),
      priority = 16L,
      intended_popup_group = "best_products",
      notes = "User observed prodID=10 available for PITC1 WY accumulated volume; live audit should confirm per ID."
    )

    add(
      "qpf_snow_level_summary_reference",
      "CNRFC 6-day QPF / freezing-level summary",
      "https://www.cnrfc.noaa.gov/awipsProducts/RNOHD6RSA.php",
      priority = 25L,
      intended_popup_group = "best_or_reference",
      notes = "General summary page; later parser may match CNRFC IDs or basin names within the table."
    )
  }

  if (grepl("fnf", ftype)) {
    add(
      "fnf_dwr_reference",
      "DWR FNF reference table",
      "https://cdec.water.ca.gov/reportapp/javareports?name=FNF",
      priority = 18L,
      intended_popup_group = "best_products",
      notes = "Relevant to Sierra/Delta FNF basin layer."
    )
  }

  if (grepl("river", ftype) || grepl("reservoir", ftype)) {
    add(
      "deterministic_river_forecast",
      "CNRFC deterministic forecast",
      paste0("https://www.cnrfc.noaa.gov/graphicalRVF.php?id=", fid),
      priority = 30L,
      intended_popup_group = "supporting",
      notes = "Useful as a direct link; BRIM should not host the hydrograph."
    )

    add(
      "observed_river_data",
      "CNRFC observed river data",
      paste0("https://www.cnrfc.noaa.gov/obsRiver_hc.php?id=", fid),
      priority = 31L,
      intended_popup_group = "supporting",
      notes = "Useful as a direct observed-data portal link."
    )
  }

  if (grepl("reservoir", ftype)) {
    add(
      "reservoir_current_page",
      "CNRFC reservoir observed/current page",
      paste0("https://www.cnrfc.noaa.gov/reservoir.php?id=", fid),
      priority = 12L,
      intended_popup_group = "already_handled_by_reservoir_layer_or_best",
      notes = "Existing BRIM Ops Live reservoir layer already handles many reservoir links."
    )

    add(
      "reservoir_release_schedule",
      "CNRFC reservoir release schedule",
      paste0("https://www.cnrfc.noaa.gov/reservoirRelease.php?id=", fid),
      priority = 13L,
      intended_popup_group = "already_handled_by_reservoir_layer_or_best",
      notes = "Existing BRIM Ops Live reservoir layer already handles many release links."
    )
  }

  if (grepl("precip", ftype)) {
    add(
      "precip_station_catalog_candidate",
      "CNRFC precip station/catalog candidate",
      paste0("https://www.cnrfc.noaa.gov/precipMaps.php"),
      priority = 60L,
      intended_popup_group = "reference_only",
      notes = "Static precip catalogs do not prove active/recent reporting. Later checks should distinguish catalog-only versus recently active stations."
    )
  }

  bind_rows(out)
}

candidate_product_catalog <- feature_master |>
  split(seq_len(nrow(feature_master))) |>
  purrr::map_dfr(pt_candidate_products_one) |>
  arrange(.data$feature_type, .data$cnrfc_id, .data$priority, .data$product_group)

# ==== 8. Product-group candidate bins =======================================

product_group_bins <- candidate_product_catalog |>
  group_by(.data$feature_key, .data$feature_type, .data$cnrfc_id, .data$display_name) |>
  summarise(
    product_group_count = n_distinct(.data$product_group),
    has_basin_temperature = any(.data$product_group == "basin_mean_temperature"),
    has_water_supply_fnf_candidate = any(.data$product_group %in% c("basin_water_supply_fnf", "basin_water_supply_wy_accum_vol_candidate", "fnf_dwr_reference")),
    has_ensemble_landing = any(.data$product_group == "ensemble_landing"),
    has_ensemble_product_probes = any(grepl("^ensemble_product_probe_", .data$product_group)),
    has_qpf_snow_level_summary_ref = any(.data$product_group == "qpf_snow_level_summary_reference"),
    has_river_forecast_links = any(.data$product_group %in% c("deterministic_river_forecast", "observed_river_data")),
    has_reservoir_ops_links = any(.data$product_group %in% c("reservoir_current_page", "reservoir_release_schedule")),
    has_precip_catalog_candidate = any(.data$product_group == "precip_station_catalog_candidate"),
    intended_popup_groups = paste(sort(unique(.data$intended_popup_group)), collapse = ";"),
    candidate_product_groups = paste(sort(unique(.data$product_group)), collapse = ";"),
    run_timestamp = RUN_TS,
    .groups = "drop"
  ) |>
  mutate(
    product_group_bin_candidate = case_when(
      .data$has_basin_temperature & .data$has_water_supply_fnf_candidate & .data$has_ensemble_landing & .data$has_qpf_snow_level_summary_ref ~
        "basin: temp + water-supply/FNF + ensemble + QPF/snow-level reference",
      .data$has_basin_temperature & .data$has_water_supply_fnf_candidate & .data$has_ensemble_landing ~
        "basin: temp + water-supply/FNF + ensemble",
      .data$has_basin_temperature & .data$has_water_supply_fnf_candidate ~
        "basin: temp + water-supply/FNF",
      .data$has_basin_temperature & .data$has_ensemble_landing ~
        "basin: temp + ensemble",
      .data$has_water_supply_fnf_candidate & .data$has_ensemble_landing ~
        "basin/FNF: water-supply/FNF + ensemble",
      .data$has_water_supply_fnf_candidate ~
        "FNF/water-supply candidate",
      .data$has_reservoir_ops_links ~
        "reservoir: ops links/crosswalk candidates",
      .data$has_river_forecast_links & .data$has_ensemble_landing ~
        "river point: deterministic/observed + ensemble",
      .data$has_river_forecast_links ~
        "river point: deterministic/observed",
      .data$has_precip_catalog_candidate ~
        "precip: catalog/recent-index candidate",
      TRUE ~ "other / audit-only candidates"
    ),
    ui_priority_hint = case_when(
      grepl("basin:", .data$product_group_bin_candidate) ~ "candidate for CNRFC basin product-availability symbology",
      grepl("FNF", .data$product_group_bin_candidate) ~ "candidate for FNF Sierra/Delta basin treatment",
      grepl("reservoir", .data$product_group_bin_candidate) ~ "mostly already handled by existing Ops Live reservoir layer",
      grepl("river point", .data$product_group_bin_candidate) ~ "supporting point links; likely not a primary BRIM forecast display",
      grepl("precip", .data$product_group_bin_candidate) ~ "requires activity/recent-reporting checks before strong map claims",
      TRUE ~ "review"
    )
  ) |>
  arrange(.data$product_group_bin_candidate, .data$feature_type, .data$cnrfc_id)

# ==== 9. Write outputs =======================================================

readr::write_csv(feature_inventory, out_feature_inventory)
readr::write_csv(feature_master, out_feature_master)
readr::write_csv(id_collision, out_id_collision)
readr::write_csv(candidate_product_catalog, out_product_catalog)
readr::write_csv(product_group_bins, out_product_bins)
readr::write_csv(raw_summary, out_raw_summary)
readr::write_csv(field_inventory, out_field_inventory)

# ==== 10. Console summary ====================================================

message("CNRFC product-intelligence audit complete.")
message("  Feature inventory:     ", out_feature_inventory)
message("  Feature master:        ", out_feature_master)
message("  ID collision check:    ", out_id_collision)
message("  Candidate products:    ", out_product_catalog)
message("  Product-group bins:    ", out_product_bins)
message("  Raw source summary:    ", out_raw_summary)
message("  Field inventory:       ", out_field_inventory)
message("")

message("Feature master counts by type:")
print(
  feature_master |>
    count(.data$feature_type, name = "features", sort = TRUE),
  n = Inf
)

message("")
message("Top ID collision patterns:")
print(
  id_collision |>
    filter(.data$feature_type_count > 1 | .data$inventory_source_count > 1) |>
    select(.data$cnrfc_id, .data$feature_type_count, .data$inventory_rows, .data$feature_types, .data$inventory_sources) |>
    head(20),
  n = 20
)

message("")
message("Candidate product-group bins:")
print(
  product_group_bins |>
    count(.data$product_group_bin_candidate, .data$ui_priority_hint, name = "features", sort = TRUE),
  n = Inf
)

message("")
message("Next review targets:")
message("  1. Open cnrfc_id_collision_check for IDs shared by basins, points, reservoirs, and precip catalogs.")
message("  2. Open cnrfc_product_group_bins_by_feature to look for the one-hand/two-hand product availability bins.")
message("  3. Open cnrfc_candidate_product_catalog and filter for example IDs such as PITC1, NWMC1, ORO/ORDC1, and a few FNF basins.")
message("  4. Do not use this candidate catalog as final popup truth until a later availability-check parser confirms which pages/products are actually available.")

