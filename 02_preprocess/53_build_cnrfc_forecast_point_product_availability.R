# ==== 53_build_cnrfc_forecast_point_product_availability.R ==================
##
## PURPOSE:
##   Build a stable, preprocessed CNRFC forecast-point product-availability
##   table for the future master CNRFC forecast-points layer.
##
## DESIGN NOTE:
##   This is a conservative staging preprocessor.  It does not live-check CNRFC
##   forecast pages yet.  It combines the durable feature inventory from 51_ with
##   the WRU XML product matrix discovered during the QA audits.  The result is a
##   point/ID-level intelligence table that separates the two likely future
##   master point layers:
##     - CNRFC river/reservoir forecast points
##         (deterministic forecast assumed for every forecast point; ensemble
##          status needs later page-specific live checks)
##     - CNRFC precip/weather stations
##         (eventually symbolized by transmission/type such as ASOS/GOES/etc.)
##   It also tracks WRU XML water-supply/flow and precipitation products.
##
##   IMPORTANT: layer membership is intentionally non-exclusive.  A CNRFC ID
##   can be both a river/reservoir forecast point and a precip/weather station.
##   The script therefore writes both the broad review table and two split
##   layer-candidate tables.
##
## OUTPUTS:
##   04_processed_data/rds/cnrfc_forecast_point_product_availability_matrix.rds
##   04_processed_data/rds/cnrfc_forecast_point_product_availability_bins.rds
##   04_processed_data/rds/cnrfc_forecast_point_product_availability_manifest.rds
##   04_processed_data/qa/cnrfc_forecast_point_product_availability_matrix_latest.csv
##   04_processed_data/qa/cnrfc_forecast_point_product_availability_bins_latest.csv
##
##   Map-ready slim outputs added in Patch 035:
##   04_processed_data/rds/cnrfc_active_river_reservoir_forecast_points_map.rds
##   04_processed_data/rds/cnrfc_precip_weather_stations_map.rds
##   04_processed_data/rds/cnrfc_river_reservoir_catalog_review.rds

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
  library(tibble)
  library(stringr)
})

RUN_TS <- make_timestamp()

dir.create(DIR$rds, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR$qa, showWarnings = FALSE, recursive = TRUE)

pt_log <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " | ", ...)
}

pt_latest_file <- function(pattern, dir = DIR$qa) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  files[order(file.info(files)$mtime, decreasing = TRUE)][[1]]
}

pt_read_csv_or_empty <- function(path) {
  if (is.na(path) || !file.exists(path)) return(tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

pt_read_rds_or_csv <- function(rds_path, csv_pattern) {
  if (!is.na(rds_path) && file.exists(rds_path)) {
    return(readRDS(rds_path))
  }
  csv_path <- pt_latest_file(csv_pattern)
  pt_read_csv_or_empty(csv_path)
}

pt_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

pt_has_text <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

pt_collapse <- function(x) {
  x <- unique(trimws(as.character(x[!is.na(x) & nzchar(trimws(as.character(x)))])))
  if (length(x) == 0) return(NA_character_)
  paste(sort(x), collapse = ";")
}

pt_first_nonempty <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) return(NA_character_)
  trimws(x[[1]])
}

pt_any <- function(x) {
  any(isTRUE(x) | (!is.na(x) & x %in% TRUE))
}

pt_safe_detect <- function(x, pattern) {
  stringr::str_detect(pt_chr(x), stringr::regex(pattern, ignore_case = TRUE))
}

# Hand-curated symbolic coordinates for CNRFC Water Supply Index points.
# These are regional index products, not ordinary river/reservoir forecast
# points.  Use stable, Delta-Ops-style symbolic locations so they can appear
# clearly in the future river/reservoir forecast-point layer.  MLIC0 is placed at the
# midpoint of the symbolic Sacramento Valley and San Joaquin Valley index
# locations, rather than at Delta outflow, so it does not imply a Delta
# precipitation/contributing-area centroid.
manual_water_supply_index_points <- tibble::tribble(
  ~cnrfc_id, ~water_supply_index_area, ~water_supply_index_name, ~water_supply_index_symbol_point, ~water_supply_index_lat, ~water_supply_index_lon, ~water_supply_index_url,
  "SACC0",  "Sacramento Valley",    "Sacramento Valley - Water Supply Index", "sac river input", 38.455664,   -121.5016200, "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=SACC0&prodID=9",
  "VNSC0",  "San Joaquin Valley",   "San Joaquin Valley - Water Supply Index", "sj river input",  37.67601249, -121.2662907, "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=VNSC0&prodID=9",
  "MLIC0",  "Central Valley",       "Central Valley - Water Supply Index",     "midpoint of Sac/SJ symbolic inputs", 38.065838245, -121.38395535, "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=MLIC0&prodID=9"
) %>%
  mutate(
    cnrfc_id = toupper(.data$cnrfc_id),
    is_water_supply_index = TRUE,
    water_supply_index_symbol_hint = "special water-supply index point; CNRFC uses square marker; BRIM may use square or distinctive outline"
  )


# Active CNRFC forecast-marker XML endpoints.  These are now the preferred
# authority for the future river/reservoir forecast-point layer.  The broad raw
# river catalog remains useful as review/context, but it is no longer promoted
# to the primary map layer.
active_xml_endpoints <- tibble::tribble(
  ~endpoint_key,              ~active_product_label,                       ~url,                                                   ~expected_visible_markers,
  "river_forecast_combined",  "River Forecast Data for Fcst and Other Pts", "https://www.cnrfc.noaa.gov/data/kml/riverFcst.xml",   287L,
  "reservoir_inflows",        "Reservoir Inflow Points",                   "https://www.cnrfc.noaa.gov/data/kml/rsvrInflow.xml",  104L,
  "reservoir_releases",       "Reservoir Release Points",                  "https://www.cnrfc.noaa.gov/data/kml/rsvrRelease.xml",  61L,
  "ensemble_forecast_points", "Ensemble Forecast Points",                  "https://www.cnrfc.noaa.gov/data/kml/ensPoints.xml",     NA_integer_
)

pt_fetch_url_text <- function(url, timeout = 60) {
  tryCatch({
    if (requireNamespace("curl", quietly = TRUE)) {
      h <- curl::new_handle(
        useragent = "BRIM CNRFC active forecast XML preprocessor",
        followlocation = TRUE,
        timeout = timeout
      )
      res <- curl::curl_fetch_memory(url, handle = h)
      if (is.null(res$status_code) || res$status_code < 200L || res$status_code >= 300L) return(NA_character_)
      raw <- res$content
    } else {
      con <- url(url, open = "rb")
      on.exit(close(con), add = TRUE)
      raw <- readBin(con, what = "raw", n = 50000000L)
    }
    raw <- raw[raw != as.raw(0)]
    txt <- rawToChar(raw, multiple = FALSE)
    txt <- iconv(txt, from = "", to = "UTF-8", sub = "byte")
    ifelse(is.na(txt), "", txt)
  }, error = function(e) {
    NA_character_
  })
}

pt_xml_node_fields <- function(node) {
  attrs <- xml2::xml_attrs(node)
  attr_tbl <- tibble(field_name = character(), field_value = character())
  if (length(attrs) > 0) {
    attr_tbl <- tibble(field_name = names(attrs), field_value = as.character(attrs))
  }

  kids <- xml2::xml_children(node)
  kid_tbl <- tibble(field_name = character(), field_value = character())
  if (length(kids) > 0) {
    kid_tbl <- tibble(
      field_name = xml2::xml_name(kids),
      field_value = vapply(kids, function(k) {
        x <- xml2::xml_text(k)
        x <- gsub("\\s+", " ", as.character(x))
        trimws(substr(x, 1, 800))
      }, character(1))
    ) %>%
      filter(!is.na(.data$field_value), nzchar(trimws(.data$field_value)))
  }

  bind_rows(attr_tbl, kid_tbl)
}

pt_field_value <- function(field_tbl, patterns) {
  if (nrow(field_tbl) == 0) return(NA_character_)
  pat <- paste(patterns, collapse = "|")
  vals <- field_tbl %>%
    filter(str_detect(.data$field_name, regex(pat, ignore_case = TRUE))) %>%
    pull("field_value")
  pt_first_nonempty(vals)
}

pt_parse_num <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_real_)
  suppressWarnings(as.numeric(gsub("[^0-9.\\-]+", "", x)))
}

pt_id_tokens <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(character())
  ids <- unlist(regmatches(txt, gregexpr("(?<![A-Z0-9])[A-Z0-9]{4,6}(?![A-Z0-9])", txt, perl = TRUE)))
  ids <- ids[grepl("^[A-Z][A-Z0-9]{3,5}$", ids)]
  ids <- ids[grepl("[0-9]$", ids)]
  sort(unique(ids))
}

pt_pick_xml_id <- function(field_tbl, blob) {
  pref <- field_tbl %>%
    filter(str_detect(.data$field_name, regex("^(id|ident|identifier|nwsli|station|station_id|lid|location|location_id)$", ignore_case = TRUE))) %>%
    pull("field_value")
  ids <- pt_id_tokens(paste(c(pref, blob), collapse = " "))
  if (length(ids) == 0) return(NA_character_)
  ids[[1]]
}

pt_xml_active_subtype <- function(endpoint_key) {
  dplyr::case_when(
    endpoint_key == "river_forecast_combined" ~ "river_forecast_combined_fcst_and_other_pts",
    endpoint_key == "reservoir_inflows" ~ "reservoir_inflow",
    endpoint_key == "reservoir_releases" ~ "reservoir_release",
    endpoint_key == "ensemble_forecast_points" ~ "ensemble_forecast_point",
    TRUE ~ endpoint_key
  )
}

pt_parse_active_xml_endpoint <- function(endpoint_key, active_product_label, url) {
  txt <- pt_fetch_url_text(url)
  if (is.na(txt) || !nzchar(txt) || str_detect(txt, regex("<html|<!doctype|404 Not Found", ignore_case = TRUE))) {
    return(tibble())
  }
  if (!requireNamespace("xml2", quietly = TRUE)) return(tibble())

  doc <- tryCatch(xml2::read_xml(txt), error = function(e) NULL)
  if (is.null(doc)) return(tibble())

  nodes <- xml2::xml_find_all(doc, ".//*")
  if (length(nodes) == 0) return(tibble())

  rows <- lapply(nodes, function(node) {
    blob <- paste(
      xml2::xml_name(node),
      paste(xml2::xml_attrs(node), collapse = " "),
      xml2::xml_text(node),
      collapse = " "
    )
    field_tbl <- pt_xml_node_fields(node)
    ids_in_blob <- pt_id_tokens(blob)
    if (length(ids_in_blob) == 0 || length(ids_in_blob) > 4) return(NULL)
    id <- pt_pick_xml_id(field_tbl, blob)
    lat <- pt_parse_num(pt_field_value(field_tbl, c("^lat$", "latitude", "lat_deg", "ycoord", "y_coord")))
    lon <- pt_parse_num(pt_field_value(field_tbl, c("^lon$", "^lng$", "longitude", "lon_deg", "xcoord", "x_coord")))
    if (is.na(id) || is.na(lat) || is.na(lon) || abs(lat) > 90 || abs(lon) > 180) return(NULL)

    river_name <- pt_field_value(field_tbl, c("riverName", "river_name", "river"))
    station_name <- pt_field_value(field_tbl, c("stationName", "station_name", "name", "location"))
    display_name_hint <- pt_first_nonempty(c(station_name, river_name, id))

    tibble(
      endpoint_key = endpoint_key,
      active_product_label = active_product_label,
      active_forecast_point_subtype = pt_xml_active_subtype(endpoint_key),
      cnrfc_id = toupper(id),
      active_xml_display_name_hint = display_name_hint,
      active_xml_river_name_hint = river_name,
      active_xml_station_name_hint = station_name,
      active_xml_lat = lat,
      active_xml_lon = lon,
      active_xml_url = url
    )
  })

  bind_rows(rows) %>%
    distinct(.data$endpoint_key, .data$cnrfc_id, .keep_all = TRUE)
}

pt_active_xml_manual_rows <- function() {
  manual_water_supply_index_points %>%
    transmute(
      endpoint_key = "manual_water_supply_index",
      active_product_label = "Water Supply Index",
      active_forecast_point_subtype = "water_supply_index",
      cnrfc_id = .data$cnrfc_id,
      active_xml_display_name_hint = .data$water_supply_index_name,
      active_xml_river_name_hint = NA_character_,
      active_xml_station_name_hint = .data$water_supply_index_area,
      active_xml_lat = as.numeric(.data$water_supply_index_lat),
      active_xml_lon = as.numeric(.data$water_supply_index_lon),
      active_xml_url = .data$water_supply_index_url
    )
}

pt_build_active_xml_matrix <- function() {
  fetched <- lapply(seq_len(nrow(active_xml_endpoints)), function(i) {
    ep <- active_xml_endpoints[i, ]
    Sys.sleep(0.10)
    pt_parse_active_xml_endpoint(ep$endpoint_key, ep$active_product_label, ep$url)
  }) %>%
    bind_rows()

  if (nrow(fetched) == 0) {
    latest <- pt_latest_file("^cnrfc_active_forecast_xml_id_matrix_latest\\.csv$")
    if (!is.na(latest) && file.exists(latest)) {
      pt_log("Active XML live fetch returned no rows; using latest QA cache: ", latest)
      cached <- pt_read_csv_or_empty(latest)
      return(cached %>%
        transmute(
          endpoint_key = as.character(.data$endpoint_key),
          active_product_label = as.character(.data$active_product_label),
          active_forecast_point_subtype = as.character(.data$active_forecast_point_subtype),
          cnrfc_id = toupper(as.character(.data$cnrfc_id)),
          active_xml_display_name_hint = as.character(.data$display_name_hint),
          active_xml_river_name_hint = NA_character_,
          active_xml_station_name_hint = NA_character_,
          active_xml_lat = suppressWarnings(as.numeric(.data$latitude)),
          active_xml_lon = suppressWarnings(as.numeric(.data$longitude)),
          active_xml_url = as.character(.data$endpoint_url)
        ))
    }
  }

  bind_rows(fetched, pt_active_xml_manual_rows()) %>%
    filter(!is.na(.data$cnrfc_id), nzchar(.data$cnrfc_id)) %>%
    distinct(.data$endpoint_key, .data$cnrfc_id, .keep_all = TRUE)
}

pt_summarise_active_xml_by_id <- function(active_xml_matrix) {
  if (nrow(active_xml_matrix) == 0) {
    return(tibble(
      cnrfc_id = character(),
      active_xml_endpoint_keys = character(),
      active_xml_product_labels = character(),
      active_xml_subtypes = character(),
      active_xml_urls = character(),
      active_xml_display_name_hint = character(),
      active_xml_lat = numeric(),
      active_xml_lon = numeric(),
      active_xml_has_any = logical(),
      active_xml_has_river_forecast_combined = logical(),
      active_xml_has_reservoir_inflow = logical(),
      active_xml_has_reservoir_release = logical(),
      active_xml_has_ensemble_forecast_point = logical(),
      active_xml_has_water_supply_index = logical()
    ))
  }

  active_xml_matrix %>%
    group_by(.data$cnrfc_id) %>%
    summarise(
      active_xml_endpoint_keys = pt_collapse(.data$endpoint_key),
      active_xml_product_labels = pt_collapse(.data$active_product_label),
      active_xml_subtypes = pt_collapse(.data$active_forecast_point_subtype),
      active_xml_urls = pt_collapse(.data$active_xml_url),
      active_xml_display_name_hint = pt_first_nonempty(.data$active_xml_display_name_hint),
      active_xml_lat = suppressWarnings(as.numeric(pt_first_nonempty(.data$active_xml_lat))),
      active_xml_lon = suppressWarnings(as.numeric(pt_first_nonempty(.data$active_xml_lon))),
      active_xml_has_any = TRUE,
      active_xml_has_river_forecast_combined = any(.data$endpoint_key == "river_forecast_combined", na.rm = TRUE),
      active_xml_has_reservoir_inflow = any(.data$endpoint_key == "reservoir_inflows", na.rm = TRUE),
      active_xml_has_reservoir_release = any(.data$endpoint_key == "reservoir_releases", na.rm = TRUE),
      active_xml_has_ensemble_forecast_point = any(.data$endpoint_key == "ensemble_forecast_points", na.rm = TRUE),
      active_xml_has_water_supply_index = any(.data$endpoint_key == "manual_water_supply_index", na.rm = TRUE),
      .groups = "drop"
    )
}

# ==== 2. Inputs ============================================================== 

feature_master <- pt_read_rds_or_csv(
  file.path(DIR$rds, "cnrfc_feature_master.rds"),
  "^cnrfc_feature_master_[0-9_]+\\.csv$"
)

if (nrow(feature_master) == 0) {
  stop(
    "No CNRFC feature master found. Run preprocess_cnrfc_feature_inventory() first, ",
    "or run qa_cnrfc_product_intelligence_audit() to create the source CSV."
  )
}

wru_xml_matrix <- pt_read_rds_or_csv(
  file.path(DIR$rds, "cnrfc_wru_xml_product_matrix.rds"),
  "^cnrfc_wru_xml_product_matrix_[0-9_]+\\.csv$"
)

if (nrow(wru_xml_matrix) == 0) {
  stop(
    "No CNRFC WRU XML product matrix found. Run qa_cnrfc_water_resources_update_kml_audit() ",
    "or preprocess_cnrfc_basin_product_availability() first."
  )
}

basin_matrix <- pt_read_rds_or_csv(
  file.path(DIR$rds, "cnrfc_basin_product_availability_matrix.rds"),
  "^cnrfc_basin_product_availability_matrix_[0-9_]+\\.csv$"
)

active_xml_matrix <- pt_build_active_xml_matrix()
active_xml_summary <- pt_summarise_active_xml_by_id(active_xml_matrix)

pt_log("Feature master rows: ", nrow(feature_master))
pt_log("WRU XML matrix rows: ", nrow(wru_xml_matrix))
pt_log("Basin matrix rows: ", nrow(basin_matrix))
pt_log("Active CNRFC XML endpoint rows: ", nrow(active_xml_matrix))
pt_log("Active CNRFC XML unique IDs: ", nrow(active_xml_summary))

needed_feature_cols <- c("feature_type", "cnrfc_id", "display_name")
missing_feature_cols <- setdiff(needed_feature_cols, names(feature_master))
if (length(missing_feature_cols) > 0) {
  stop("Feature master missing required columns: ", paste(missing_feature_cols, collapse = ", "))
}

# Make older/smaller source files tolerant.
for (nm in c(
  "feature_key", "river_name", "location_name", "forecast_group", "river_group",
  "raw_family", "raw_kind", "raw_class1", "raw_class2", "datatransmission",
  "cdec_id", "usgs_id", "lat", "lon", "elev_ft", "has_valid_coordinate",
  "inventory_sources", "product_scope_hint", "source_rows"
)) {
  if (!nm %in% names(feature_master)) feature_master[[nm]] <- NA
}

for (nm in c(
  "cnrfc_id", "in_wru_xml_products", "wru_xml_record_count", "wru_xml_files",
  "wru_xml_product_groups", "wru_xml_record_tags", "wru_xml_labels"
)) {
  if (!nm %in% names(wru_xml_matrix)) wru_xml_matrix[[nm]] <- NA
}

# ==== 3. Feature summary by ID ==============================================

feature_priority <- c(
  cnrfc_river_point = 1L,
  cnrfc_reservoir_point = 2L,
  cnrfc_stream_or_reservoir_point_cache = 3L,
  brim_reservoir_layer_cdec_cnrfc_crosswalk = 4L,
  cnrfc_basin = 5L,
  cnrfc_fnf_sierra_delta_basin = 6L,
  cnrfc_precip_station_cache = 7L,
  cnrfc_precip_station_catalog = 8L,
  cnrfc_precip_station_recent_index = 9L,
  cnrfc_special_point = 10L
)

fm <- feature_master %>%
  mutate(
    cnrfc_id = toupper(trimws(as.character(.data$cnrfc_id))),
    feature_type = as.character(.data$feature_type),
    feature_priority = unname(feature_priority[.data$feature_type]),
    feature_priority = if_else(is.na(.data$feature_priority), 99L, .data$feature_priority),
    lat = suppressWarnings(as.numeric(.data$lat)),
    lon = suppressWarnings(as.numeric(.data$lon)),
    elev_ft = suppressWarnings(as.numeric(.data$elev_ft)),
    has_valid_coordinate = case_when(
      isTRUE(.data$has_valid_coordinate) ~ TRUE,
      is.logical(.data$has_valid_coordinate) ~ .data$has_valid_coordinate,
      tolower(as.character(.data$has_valid_coordinate)) == "true" ~ TRUE,
      TRUE ~ !is.na(.data$lat) & !is.na(.data$lon) & abs(.data$lat) <= 90 & abs(.data$lon) <= 180
    )
  ) %>%
  filter(!is.na(.data$cnrfc_id), nzchar(.data$cnrfc_id))

best_display <- fm %>%
  arrange(.data$cnrfc_id, .data$feature_priority) %>%
  group_by(.data$cnrfc_id) %>%
  summarise(
    display_name = pt_first_nonempty(.data$display_name),
    river_name = pt_first_nonempty(.data$river_name),
    location_name = pt_first_nonempty(.data$location_name),
    forecast_group = pt_first_nonempty(.data$forecast_group),
    river_group = pt_first_nonempty(.data$river_group),
    cdec_id = pt_first_nonempty(.data$cdec_id),
    usgs_id = pt_first_nonempty(.data$usgs_id),
    .groups = "drop"
  )

best_coord <- fm %>%
  filter(isTRUE(.data$has_valid_coordinate) | .data$has_valid_coordinate == TRUE) %>%
  arrange(.data$cnrfc_id, .data$feature_priority) %>%
  group_by(.data$cnrfc_id) %>%
  summarise(
    lat = suppressWarnings(as.numeric(.data$lat[[1]])),
    lon = suppressWarnings(as.numeric(.data$lon[[1]])),
    elev_ft = suppressWarnings(as.numeric(.data$elev_ft[[1]])),
    coordinate_source_type = as.character(.data$feature_type[[1]]),
    .groups = "drop"
  )

feature_flags <- fm %>%
  group_by(.data$cnrfc_id) %>%
  summarise(
    feature_types = pt_collapse(.data$feature_type),
    inventory_sources = pt_collapse(.data$inventory_sources),
    product_scope_hints = pt_collapse(.data$product_scope_hint),
    raw_families = pt_collapse(.data$raw_family),
    raw_kinds = pt_collapse(.data$raw_kind),
    raw_class1_values = pt_collapse(.data$raw_class1),
    raw_class2_values = pt_collapse(.data$raw_class2),
    datatransmission_types = pt_collapse(.data$datatransmission),
    source_rows = suppressWarnings(sum(as.numeric(.data$source_rows), na.rm = TRUE)),
    has_river_point = any(.data$feature_type == "cnrfc_river_point", na.rm = TRUE),
    has_reservoir_point = any(.data$feature_type == "cnrfc_reservoir_point", na.rm = TRUE),
    has_stream_or_reservoir_cache_point = any(.data$feature_type == "cnrfc_stream_or_reservoir_point_cache", na.rm = TRUE),
    has_reservoir_crosswalk = any(.data$feature_type == "brim_reservoir_layer_cdec_cnrfc_crosswalk", na.rm = TRUE),
    has_basin_polygon = any(.data$feature_type == "cnrfc_basin", na.rm = TRUE),
    has_fnf_basin = any(.data$feature_type == "cnrfc_fnf_sierra_delta_basin", na.rm = TRUE),
    has_precip_station_cache = any(.data$feature_type == "cnrfc_precip_station_cache", na.rm = TRUE),
    has_precip_station_catalog = any(.data$feature_type == "cnrfc_precip_station_catalog", na.rm = TRUE),
    has_precip_recent_index = any(.data$feature_type == "cnrfc_precip_station_recent_index", na.rm = TRUE),
    has_special_point = any(.data$feature_type == "cnrfc_special_point", na.rm = TRUE),
    .groups = "drop"
  )

feature_summary <- feature_flags %>%
  left_join(best_display, by = "cnrfc_id") %>%
  left_join(best_coord, by = "cnrfc_id")

# ==== 4. WRU XML product flags ==============================================

wru <- wru_xml_matrix %>%
  transmute(
    cnrfc_id = toupper(trimws(as.character(.data$cnrfc_id))),
    in_wru_xml_products = as.logical(.data$in_wru_xml_products),
    wru_xml_record_count = suppressWarnings(as.integer(.data$wru_xml_record_count)),
    wru_xml_files = as.character(.data$wru_xml_files),
    wru_xml_product_groups = as.character(.data$wru_xml_product_groups),
    wru_xml_record_tags = as.character(.data$wru_xml_record_tags),
    wru_xml_labels = as.character(.data$wru_xml_labels)
  ) %>%
  filter(!is.na(.data$cnrfc_id), nzchar(.data$cnrfc_id)) %>%
  mutate(
    has_wru_forecast_seasonal_volume = pt_safe_detect(.data$wru_xml_product_groups, "forecast_seasonal_volume"),
    has_wru_forecast_water_year_volume = pt_safe_detect(.data$wru_xml_product_groups, "forecast_water_year_volume"),
    has_wru_observed_wy_flow_to_date = pt_safe_detect(.data$wru_xml_product_groups, "observed_water_year_flow_to_date"),
    has_wru_previous_wy_flow_to_date = pt_safe_detect(.data$wru_xml_product_groups, "previous_water_year_flow_to_date"),
    has_wru_precip_products = pt_safe_detect(.data$wru_xml_product_groups, "precip"),
    has_wru_water_supply_or_flow = .data$has_wru_forecast_seasonal_volume |
      .data$has_wru_forecast_water_year_volume |
      .data$has_wru_observed_wy_flow_to_date |
      .data$has_wru_previous_wy_flow_to_date
  )

# ==== 5. Build forecast-point universe ======================================

point_feature_ids <- feature_summary %>%
  filter(
    .data$has_river_point |
      .data$has_reservoir_point |
      .data$has_stream_or_reservoir_cache_point |
      .data$has_reservoir_crosswalk |
      .data$has_special_point
  ) %>%
  pull("cnrfc_id")

precip_weather_ids <- feature_summary %>%
  filter(
    .data$has_precip_station_cache |
      .data$has_precip_station_catalog |
      .data$has_precip_recent_index
  ) %>%
  pull("cnrfc_id")

wru_ids <- wru %>% pull("cnrfc_id")
active_xml_ids <- active_xml_summary %>% pull("cnrfc_id")

forecast_point_ids <- Reduce(union, list(point_feature_ids, precip_weather_ids, wru_ids, active_xml_ids))

matrix <- tibble(cnrfc_id = sort(unique(forecast_point_ids))) %>%
  left_join(feature_summary, by = "cnrfc_id") %>%
  left_join(wru, by = "cnrfc_id") %>%
  left_join(active_xml_summary, by = "cnrfc_id") %>%
  left_join(manual_water_supply_index_points, by = "cnrfc_id")

# Fill logicals and counts after joins.
logical_cols <- names(matrix)[vapply(matrix, is.logical, logical(1))]
for (nm in logical_cols) {
  matrix[[nm]][is.na(matrix[[nm]])] <- FALSE
}

if (!"wru_xml_record_count" %in% names(matrix)) matrix$wru_xml_record_count <- 0L
matrix$wru_xml_record_count[is.na(matrix$wru_xml_record_count)] <- 0L

matrix <- matrix %>%
  mutate(
    is_water_supply_index = if_else(is.na(.data$is_water_supply_index), FALSE, .data$is_water_supply_index),
    active_xml_has_any = if_else(is.na(.data$active_xml_has_any), FALSE, .data$active_xml_has_any),
    active_xml_has_river_forecast_combined = if_else(is.na(.data$active_xml_has_river_forecast_combined), FALSE, .data$active_xml_has_river_forecast_combined),
    active_xml_has_reservoir_inflow = if_else(is.na(.data$active_xml_has_reservoir_inflow), FALSE, .data$active_xml_has_reservoir_inflow),
    active_xml_has_reservoir_release = if_else(is.na(.data$active_xml_has_reservoir_release), FALSE, .data$active_xml_has_reservoir_release),
    active_xml_has_ensemble_forecast_point = if_else(is.na(.data$active_xml_has_ensemble_forecast_point), FALSE, .data$active_xml_has_ensemble_forecast_point),
    active_xml_has_water_supply_index = if_else(is.na(.data$active_xml_has_water_supply_index), FALSE, .data$active_xml_has_water_supply_index),
    is_water_supply_index = .data$is_water_supply_index | .data$active_xml_has_water_supply_index,
    display_name = case_when(
      .data$is_water_supply_index & pt_has_text(.data$water_supply_index_name) ~ .data$water_supply_index_name,
      .data$active_xml_has_any & pt_has_text(.data$active_xml_display_name_hint) ~ .data$active_xml_display_name_hint,
      pt_has_text(.data$display_name) ~ .data$display_name,
      pt_has_text(.data$river_name) & pt_has_text(.data$location_name) ~ paste(.data$river_name, "-", .data$location_name),
      TRUE ~ .data$cnrfc_id
    ),
    lat = case_when(
      .data$is_water_supply_index ~ as.numeric(.data$water_supply_index_lat),
      .data$active_xml_has_any & !is.na(.data$active_xml_lat) ~ as.numeric(.data$active_xml_lat),
      TRUE ~ as.numeric(.data$lat)
    ),
    lon = case_when(
      .data$is_water_supply_index ~ as.numeric(.data$water_supply_index_lon),
      .data$active_xml_has_any & !is.na(.data$active_xml_lon) ~ as.numeric(.data$active_xml_lon),
      TRUE ~ as.numeric(.data$lon)
    ),
    coordinate_source_type = case_when(
      .data$is_water_supply_index ~ "manual_water_supply_index_symbolic_point",
      .data$active_xml_has_any ~ "active_cnrfc_forecast_xml",
      TRUE ~ .data$coordinate_source_type
    ),
    has_any_cnrfc_point = .data$has_river_point |
      .data$has_reservoir_point |
      .data$has_stream_or_reservoir_cache_point |
      .data$has_reservoir_crosswalk |
      .data$has_special_point,
    has_any_wru_xml_product = isTRUE(.data$in_wru_xml_products) |
      (.data$wru_xml_record_count > 0),
    product_bin_observed = case_when(
      .data$active_xml_has_ensemble_forecast_point & .data$active_xml_has_river_forecast_combined ~ "active river forecast XML + ensemble forecast XML",
      .data$active_xml_has_ensemble_forecast_point & (.data$active_xml_has_reservoir_inflow | .data$active_xml_has_reservoir_release) ~ "active reservoir XML + ensemble forecast XML",
      .data$active_xml_has_ensemble_forecast_point ~ "active ensemble forecast XML only",
      .data$active_xml_has_river_forecast_combined ~ "active river forecast XML",
      .data$active_xml_has_reservoir_inflow | .data$active_xml_has_reservoir_release ~ "active reservoir inflow/release XML",
      .data$has_wru_water_supply_or_flow & (.data$has_river_point | .data$has_stream_or_reservoir_cache_point) &
        (.data$has_reservoir_point | .data$has_reservoir_crosswalk) ~ "river/reservoir point + WRU water-supply/flow products",
      .data$has_wru_water_supply_or_flow & (.data$has_reservoir_point | .data$has_reservoir_crosswalk) ~ "reservoir/crosswalk + WRU water-supply/flow products",
      .data$has_wru_water_supply_or_flow & (.data$has_river_point | .data$has_stream_or_reservoir_cache_point) ~ "river forecast point + WRU water-supply/flow products",
      .data$has_wru_water_supply_or_flow ~ "WRU water-supply/flow products only",
      .data$has_wru_precip_products & (.data$has_precip_station_cache | .data$has_precip_station_catalog | .data$has_precip_recent_index) ~ "precip/weather station + WRU precip products",
      (.data$has_precip_station_cache | .data$has_precip_station_catalog | .data$has_precip_recent_index) ~ "precip/weather station without confirmed current WRU products",
      .data$has_wru_precip_products ~ "WRU precip products only",
      .data$has_reservoir_point | .data$has_reservoir_crosswalk ~ "reservoir/crosswalk point without WRU products",
      .data$has_river_point | .data$has_stream_or_reservoir_cache_point ~ "river forecast point without WRU products",
      .data$has_special_point ~ "special point without WRU products",
      TRUE ~ "other WRU/catalog ID"
    ),
    run_timestamp = RUN_TS
  ) %>%
  select(all_of(c(
    "cnrfc_id",
    "display_name",
    "river_name",
    "location_name",
    "forecast_group",
    "river_group",
    "lat",
    "lon",
    "elev_ft",
    "coordinate_source_type",
    "is_water_supply_index",
    "water_supply_index_area",
    "water_supply_index_symbol_point",
    "water_supply_index_symbol_hint",
    "water_supply_index_url",
    "active_xml_has_any",
    "active_xml_endpoint_keys",
    "active_xml_product_labels",
    "active_xml_subtypes",
    "active_xml_urls",
    "active_xml_display_name_hint",
    "active_xml_has_river_forecast_combined",
    "active_xml_has_reservoir_inflow",
    "active_xml_has_reservoir_release",
    "active_xml_has_ensemble_forecast_point",
    "active_xml_has_water_supply_index",
    "feature_types",
    "inventory_sources",
    "product_scope_hints",
    "raw_families",
    "raw_kinds",
    "raw_class1_values",
    "raw_class2_values",
    "datatransmission_types",
    "has_any_cnrfc_point",
    "has_river_point",
    "has_reservoir_point",
    "has_stream_or_reservoir_cache_point",
    "has_reservoir_crosswalk",
    "has_basin_polygon",
    "has_fnf_basin",
    "has_precip_station_cache",
    "has_precip_station_catalog",
    "has_precip_recent_index",
    "has_special_point",
    "has_any_wru_xml_product",
    "wru_xml_record_count",
    "wru_xml_files",
    "wru_xml_product_groups",
    "wru_xml_record_tags",
    "wru_xml_labels",
    "has_wru_water_supply_or_flow",
    "has_wru_forecast_seasonal_volume",
    "has_wru_forecast_water_year_volume",
    "has_wru_observed_wy_flow_to_date",
    "has_wru_previous_wy_flow_to_date",
    "has_wru_precip_products",
    "product_bin_observed",
    "run_timestamp"
  )))

bins <- matrix %>%
  count(.data$product_bin_observed, name = "points") %>%
  arrange(desc(.data$points), .data$product_bin_observed) %>%
  mutate(run_timestamp = RUN_TS)

# ==== 5b. Candidate master forecast-point table =============================
#
# This is a review/staging product for the eventual CNRFC point layers.  It does
# not symbolize the map and does not decide final UI.  It now separates:
#   - ACTIVE/PRIMARY river-reservoir forecast candidates
#       * raw river "forecast" class points
#       * reservoir/crosswalk points
#       * WRU water-supply/flow points
#   - BROAD river catalog points
#       * raw CNRFC river "other" catalog records that should not be treated as
#         official homepage forecast markers until a product-page marker audit
#         confirms them
#   - precip/weather stations
#       * retained even when no current WRU product is confirmed, because
#         historical data may be available from CNRFC or another source.

master_candidates <- matrix %>%
  mutate(
    has_flow_point = .data$has_river_point | .data$has_stream_or_reservoir_cache_point,
    has_reservoir_like_point = .data$has_reservoir_point | .data$has_reservoir_crosswalk,
    has_precip_like_point = .data$has_precip_station_cache |
      .data$has_precip_station_catalog |
      .data$has_precip_recent_index,
    has_water_supply_index = if_else(is.na(.data$is_water_supply_index), FALSE, .data$is_water_supply_index),
    has_active_xml_marker = .data$active_xml_has_river_forecast_combined |
      .data$active_xml_has_reservoir_inflow |
      .data$active_xml_has_reservoir_release |
      .data$active_xml_has_ensemble_forecast_point |
      .data$active_xml_has_water_supply_index,

    raw_river_class2_forecast = pt_safe_detect(.data$raw_class2_values, "\\bforecast\\b"),
    raw_river_class2_other = pt_safe_detect(.data$raw_class2_values, "\\bother\\b"),
    raw_reservoir_class = pt_safe_detect(.data$raw_class1_values, "\\breservoir\\b") |
      pt_safe_detect(.data$raw_class2_values, "\\breservoir\\b"),

    # IMPORTANT:
    # Raw CNRFC "River Other" catalog records are much broader than the active
    # CNRFC homepage marker layers.  Do not promote those to the primary
    # river/reservoir forecast-point product until a later homepage/product-page
    # marker audit confirms them.
    has_active_river_reservoir_forecast_candidate =
      .data$has_active_xml_marker |
      .data$has_water_supply_index,

    include_in_river_reservoir_catalog_review = .data$has_flow_point | .data$has_reservoir_like_point,

    has_broad_river_catalog_only =
      .data$has_flow_point &
      !.data$has_active_river_reservoir_forecast_candidate,

    active_forecast_candidate_basis = case_when(
      .data$has_water_supply_index ~
        "manual water-supply index point",
      .data$active_xml_has_river_forecast_combined & .data$active_xml_has_ensemble_forecast_point ~
        "active riverFcst XML + active ensemble XML",
      .data$active_xml_has_reservoir_inflow & .data$active_xml_has_reservoir_release & .data$active_xml_has_ensemble_forecast_point ~
        "active reservoir inflow/release XML + active ensemble XML",
      .data$active_xml_has_reservoir_inflow & .data$active_xml_has_ensemble_forecast_point ~
        "active reservoir inflow XML + active ensemble XML",
      .data$active_xml_has_reservoir_release & .data$active_xml_has_ensemble_forecast_point ~
        "active reservoir release XML + active ensemble XML",
      .data$active_xml_has_river_forecast_combined ~
        "active riverFcst XML",
      .data$active_xml_has_reservoir_inflow & .data$active_xml_has_reservoir_release ~
        "active reservoir inflow/release XML",
      .data$active_xml_has_reservoir_inflow ~
        "active reservoir inflow XML",
      .data$active_xml_has_reservoir_release ~
        "active reservoir release XML",
      .data$active_xml_has_ensemble_forecast_point ~
        "active ensemble forecast XML",
      (.data$has_flow_point & .data$raw_river_class2_forecast) &
        .data$has_reservoir_like_point &
        .data$has_wru_water_supply_or_flow ~
        "raw river forecast class + reservoir/crosswalk + WRU water-supply/flow",
      (.data$has_flow_point & .data$raw_river_class2_forecast) &
        .data$has_wru_water_supply_or_flow ~
        "raw river forecast class + WRU water-supply/flow",
      .data$has_reservoir_like_point &
        .data$has_wru_water_supply_or_flow ~
        "reservoir/crosswalk + WRU water-supply/flow",
      .data$has_flow_point & .data$raw_river_class2_forecast ~
        "raw river forecast class",
      .data$has_reservoir_like_point ~
        "reservoir/crosswalk source",
      .data$has_wru_water_supply_or_flow ~
        "WRU water-supply/flow product row",
      .data$has_broad_river_catalog_only ~
        "broad raw CNRFC river catalog; not homepage-marker verified",
      TRUE ~ NA_character_
    ),

    river_reservoir_review_status = case_when(
      .data$has_active_river_reservoir_forecast_candidate ~
        "primary forecast-point candidate",
      .data$has_broad_river_catalog_only ~
        "broad raw river catalog; do not treat as active homepage forecast marker yet",
      .data$include_in_river_reservoir_catalog_review ~
        "river/reservoir catalog review record",
      TRUE ~ NA_character_
    ),

    proposed_master_point_layer = case_when(
      .data$has_active_river_reservoir_forecast_candidate ~
        "CNRFC river/reservoir forecast points",
      .data$has_precip_like_point | .data$has_wru_precip_products ~
        "CNRFC precip/weather stations",
      .data$has_broad_river_catalog_only ~
        "review-only / broad CNRFC river catalog",
      TRUE ~
        "review-only / not a primary CNRFC point layer"
    ),

    forecast_point_hydro_type = case_when(
      .data$has_water_supply_index ~ "water_supply_index",
      .data$active_xml_has_river_forecast_combined & (.data$active_xml_has_reservoir_inflow | .data$active_xml_has_reservoir_release) ~ "river/reservoir",
      .data$active_xml_has_reservoir_inflow | .data$active_xml_has_reservoir_release ~ "reservoir",
      .data$active_xml_has_river_forecast_combined ~ "river",
      .data$has_flow_point & .data$has_reservoir_like_point ~ "river/reservoir",
      .data$has_reservoir_like_point ~ "reservoir",
      .data$has_flow_point ~ "river",
      TRUE ~ NA_character_
    ),

    deterministic_forecast_status = case_when(
      .data$has_water_supply_index ~
        "water-supply index product; ordinary deterministic river forecast status not applicable",
      .data$has_active_xml_marker ~
        "active CNRFC XML marker; deterministic/forecast product source confirmed",
      .data$has_broad_river_catalog_only ~
        "not assumed; broad river catalog point needs homepage/product-page marker check",
      TRUE ~ NA_character_
    ),

    ensemble_forecast_status = case_when(
      .data$has_water_supply_index ~
        "CNRFC water-supply index / ensemble product confirmed",
      .data$active_xml_has_ensemble_forecast_point ~
        "active CNRFC ensemble forecast XML marker confirmed",
      .data$has_active_river_reservoir_forecast_candidate &
        .data$has_wru_water_supply_or_flow ~
        "WRU water-supply/flow products confirmed; no active ensemble XML marker",
      .data$has_active_river_reservoir_forecast_candidate ~
        "active deterministic/product XML marker confirmed; no active ensemble XML marker",
      .data$has_broad_river_catalog_only ~
        "not checked; not promoted to forecast layer yet",
      TRUE ~ NA_character_
    ),

    precip_weather_station_type_hint = case_when(
      .data$has_water_supply_index ~ NA_character_,
      .data$has_precip_like_point & pt_has_text(.data$datatransmission_types) ~ .data$datatransmission_types,
      .data$has_precip_like_point & pt_has_text(.data$raw_class1_values) ~ .data$raw_class1_values,
      .data$has_precip_like_point ~ "precip/weather station type not parsed yet",
      TRUE ~ NA_character_
    ),

    precip_weather_data_availability_note = case_when(
      .data$has_water_supply_index ~ NA_character_,
      (.data$has_precip_like_point | .data$has_wru_precip_products) &
        .data$has_wru_precip_products ~
        "Current WRU precip product rows confirmed.",
      .data$has_precip_like_point ~
        "No current WRU product confirmed in this audit; historical data may be available from CNRFC or another source.",
      TRUE ~ NA_character_
    ),

    master_candidate_class = case_when(
      .data$has_water_supply_index ~
        "water-supply index point",
      .data$active_xml_has_river_forecast_combined & .data$active_xml_has_ensemble_forecast_point ~
        "active river forecast XML point with ensemble XML",
      (.data$active_xml_has_reservoir_inflow | .data$active_xml_has_reservoir_release) & .data$active_xml_has_ensemble_forecast_point ~
        "active reservoir inflow/release XML point with ensemble XML",
      .data$active_xml_has_river_forecast_combined ~
        "active river forecast XML point",
      .data$active_xml_has_reservoir_inflow | .data$active_xml_has_reservoir_release ~
        "active reservoir inflow/release XML point",
      .data$active_xml_has_ensemble_forecast_point ~
        "active ensemble forecast XML point",
      .data$has_active_river_reservoir_forecast_candidate &
        .data$has_wru_water_supply_or_flow &
        .data$has_flow_point & .data$has_reservoir_like_point ~
        "primary river/reservoir forecast point with WRU water-supply/flow",
      .data$has_active_river_reservoir_forecast_candidate &
        .data$has_wru_water_supply_or_flow &
        .data$has_flow_point ~
        "primary river forecast point with WRU water-supply/flow",
      .data$has_active_river_reservoir_forecast_candidate &
        .data$has_wru_water_supply_or_flow &
        .data$has_reservoir_like_point ~
        "primary reservoir/crosswalk point with WRU water-supply/flow",
      .data$has_active_river_reservoir_forecast_candidate &
        .data$has_flow_point & .data$raw_river_class2_forecast ~
        "primary raw river forecast-class point",
      .data$has_active_river_reservoir_forecast_candidate &
        .data$has_reservoir_like_point ~
        "primary reservoir/crosswalk forecast point",
      .data$has_wru_water_supply_or_flow ~
        "WRU water-supply/flow ID only",
      .data$has_broad_river_catalog_only ~
        "broad raw river catalog point; not homepage-marker verified",
      .data$has_wru_precip_products & .data$has_precip_like_point ~
        "precip/weather station with WRU precip products",
      .data$has_precip_like_point ~
        "precip/weather station without confirmed current WRU products",
      .data$has_wru_precip_products ~
        "WRU precip product ID only",
      .data$has_special_point ~
        "special point without WRU products",
      TRUE ~
        "other CNRFC catalog/WRU ID"
    ),

    master_layer_recommendation = case_when(
      .data$has_water_supply_index ~
        "include as special water-supply index point in river/reservoir forecast-points layer",
      .data$has_active_river_reservoir_forecast_candidate &
        .data$has_wru_water_supply_or_flow ~
        "candidate for river/reservoir forecast-points layer; WRU products confirmed",
      .data$has_active_river_reservoir_forecast_candidate ~
        "candidate for river/reservoir forecast-points layer; deterministic page assumed, ensemble/live links need later check",
      .data$has_broad_river_catalog_only ~
        "review-only broad river catalog point; do not add to primary forecast-point layer until homepage marker audit confirms it",
      (.data$has_precip_like_point | .data$has_wru_precip_products) &
        .data$has_wru_precip_products ~
        "candidate for precip/weather station layer; symbolize later by station/transmission type",
      .data$has_precip_like_point ~
        "candidate for precip/weather station layer; no current WRU data confirmed, but historical data may be available from CNRFC or another source",
      .data$has_wru_water_supply_or_flow ~
        "WRU water-supply/flow ID without CNRFC point source; review manually",
      TRUE ~
        "defer/review"
    ),

    master_layer_priority = case_when(
      .data$has_water_supply_index ~ 1L,
      .data$has_active_river_reservoir_forecast_candidate &
        .data$has_wru_water_supply_or_flow ~ 1L,
      .data$has_active_river_reservoir_forecast_candidate ~ 2L,
      (.data$has_precip_like_point | .data$has_wru_precip_products) &
        .data$has_wru_precip_products ~ 3L,
      .data$has_precip_like_point ~ 4L,
      .data$has_broad_river_catalog_only ~ 6L,
      .data$has_wru_water_supply_or_flow ~ 7L,
      TRUE ~ 9L
    ),

    availability_confidence = case_when(
      .data$has_water_supply_index ~
        "high: manually placed CNRFC water-supply index with WRU XML product rows",
      .data$has_active_xml_marker & .data$active_xml_has_ensemble_forecast_point ~
        "high: active CNRFC XML marker plus active ensemble XML",
      .data$has_active_xml_marker ~
        "high: active CNRFC XML marker",
      .data$has_active_river_reservoir_forecast_candidate &
        .data$has_wru_water_supply_or_flow ~
        "high: primary CNRFC forecast candidate plus WRU XML product rows",
      .data$has_active_river_reservoir_forecast_candidate ~
        "medium-high: primary CNRFC forecast candidate; product pages need live check",
      .data$has_wru_precip_products & .data$has_precip_like_point ~
        "high: precip station plus WRU XML precip rows",
      .data$has_precip_like_point & (.data$has_precip_station_cache | .data$has_precip_recent_index) ~
        "medium: precip/weather station source present; current WRU product not confirmed",
      .data$has_precip_like_point ~
        "low-medium: precip/weather catalog station; current WRU product not confirmed",
      .data$has_broad_river_catalog_only ~
        "low-medium: broad CNRFC river catalog; not homepage-marker verified",
      .data$has_wru_water_supply_or_flow | .data$has_wru_precip_products ~
        "medium-low: WRU XML rows, no matched primary CNRFC point source",
      TRUE ~
        "low: catalog/supporting ID"
    ),

    include_in_river_reservoir_forecast_points = .data$has_active_river_reservoir_forecast_candidate,
    include_in_precip_weather_stations = !.data$has_water_supply_index &
      (.data$has_precip_like_point | .data$has_wru_precip_products),

    nonexclusive_master_point_layers = case_when(
      .data$include_in_river_reservoir_forecast_points & .data$include_in_precip_weather_stations ~
        "CNRFC river/reservoir forecast points;CNRFC precip/weather stations",
      .data$include_in_river_reservoir_forecast_points ~
        "CNRFC river/reservoir forecast points",
      .data$include_in_precip_weather_stations ~
        "CNRFC precip/weather stations",
      .data$has_broad_river_catalog_only ~
        "review-only / broad CNRFC river catalog",
      TRUE ~
        "review-only / not a primary CNRFC point layer"
    ),

    wru_url = if_else(
      .data$has_any_wru_xml_product,
      "https://www.cnrfc.noaa.gov/water_resources_update.php",
      NA_character_
    )
  ) %>%
  arrange(.data$master_layer_priority, .data$cnrfc_id) %>%
  select(all_of(c(
    "cnrfc_id",
    "display_name",
    "proposed_master_point_layer",
    "nonexclusive_master_point_layers",
    "include_in_river_reservoir_forecast_points",
    "include_in_precip_weather_stations",
    "include_in_river_reservoir_catalog_review",
    "has_active_river_reservoir_forecast_candidate",
    "has_active_xml_marker",
    "active_xml_endpoint_keys",
    "active_xml_product_labels",
    "active_xml_subtypes",
    "active_xml_urls",
    "active_xml_has_river_forecast_combined",
    "active_xml_has_reservoir_inflow",
    "active_xml_has_reservoir_release",
    "active_xml_has_ensemble_forecast_point",
    "active_xml_has_water_supply_index",
    "has_broad_river_catalog_only",
    "active_forecast_candidate_basis",
    "river_reservoir_review_status",
    "forecast_point_hydro_type",
    "has_water_supply_index",
    "water_supply_index_area",
    "water_supply_index_symbol_point",
    "water_supply_index_symbol_hint",
    "water_supply_index_url",
    "deterministic_forecast_status",
    "ensemble_forecast_status",
    "precip_weather_station_type_hint",
    "precip_weather_data_availability_note",
    "master_candidate_class",
    "master_layer_recommendation",
    "master_layer_priority",
    "availability_confidence",
    "lat",
    "lon",
    "elev_ft",
    "coordinate_source_type",
    "forecast_group",
    "river_group",
    "feature_types",
    "inventory_sources",
    "product_scope_hints",
    "raw_families",
    "raw_kinds",
    "raw_class1_values",
    "raw_class2_values",
    "datatransmission_types",
    "has_river_point",
    "has_reservoir_point",
    "has_stream_or_reservoir_cache_point",
    "has_reservoir_crosswalk",
    "has_basin_polygon",
    "has_fnf_basin",
    "has_precip_station_cache",
    "has_precip_station_catalog",
    "has_precip_recent_index",
    "has_any_wru_xml_product",
    "has_wru_water_supply_or_flow",
    "has_wru_precip_products",
    "wru_xml_record_count",
    "wru_xml_files",
    "wru_xml_product_groups",
    "wru_xml_labels",
    "wru_url",
    "product_bin_observed",
    "run_timestamp"
  )))

master_candidate_bins <- master_candidates %>%
  count(
    master_layer_priority,
    proposed_master_point_layer,
    master_candidate_class,
    master_layer_recommendation,
    name = "points"
  ) %>%
  arrange(master_layer_priority, desc(points), master_candidate_class) %>%
  mutate(run_timestamp = RUN_TS)

layer_family_bins <- master_candidates %>%
  count(
    proposed_master_point_layer,
    forecast_point_hydro_type,
    deterministic_forecast_status,
    ensemble_forecast_status,
    precip_weather_station_type_hint,
    name = "points"
  ) %>%
  arrange(proposed_master_point_layer, desc(points), forecast_point_hydro_type) %>%
  mutate(run_timestamp = RUN_TS)

# Non-exclusive split candidate products for the two future point layers.  These
# are the safer map-building inputs because an ID can legitimately belong to
# both the river/reservoir forecast-point layer and the precip/weather layer.
river_reservoir_points <- master_candidates %>%
  filter(.data$include_in_river_reservoir_forecast_points) %>%
  mutate(master_point_layer = "CNRFC river/reservoir forecast points") %>%
  arrange(.data$forecast_point_hydro_type, .data$cnrfc_id)

precip_weather_stations <- master_candidates %>%
  filter(.data$include_in_precip_weather_stations) %>%
  mutate(master_point_layer = "CNRFC precip/weather stations") %>%
  arrange(.data$precip_weather_station_type_hint, .data$cnrfc_id)

# Slim, map-ready outputs.  These are intentionally smaller than the review
# tables above and should be the preferred inputs for 05_map_build once the
# layer is implemented.  Keep audit clutter in master_candidates / catalog
# review, not in these map products.
active_river_reservoir_forecast_points_map <- river_reservoir_points %>%
  filter(!is.na(.data$lat), !is.na(.data$lon), abs(.data$lat) <= 90, abs(.data$lon) <= 180) %>%
  mutate(
    map_layer_key = "cnrfc_active_river_reservoir_forecast_points",
    map_layer_label = "CNRFC forecast points | active XML",
    is_active_river_forecast = .data$active_xml_has_river_forecast_combined,
    is_active_reservoir_inflow = .data$active_xml_has_reservoir_inflow,
    is_active_reservoir_release = .data$active_xml_has_reservoir_release,
    has_active_ensemble_forecast = .data$active_xml_has_ensemble_forecast_point,
    map_point_role = case_when(
      .data$has_water_supply_index ~ "water_supply_index",
      .data$active_xml_has_river_forecast_combined &
        (.data$active_xml_has_reservoir_inflow | .data$active_xml_has_reservoir_release) ~ "river_and_reservoir",
      .data$active_xml_has_reservoir_inflow & .data$active_xml_has_reservoir_release ~ "reservoir_inflow_and_release",
      .data$active_xml_has_reservoir_inflow ~ "reservoir_inflow",
      .data$active_xml_has_reservoir_release ~ "reservoir_release",
      .data$active_xml_has_river_forecast_combined ~ "river_forecast",
      .data$active_xml_has_ensemble_forecast_point ~ "ensemble_forecast_only",
      TRUE ~ "active_forecast_point"
    ),
    map_symbol_shape = case_when(
      .data$has_water_supply_index ~ "square",
      .data$map_point_role %in% c("reservoir_inflow", "reservoir_release", "reservoir_inflow_and_release") ~ "square",
      TRUE ~ "circle"
    ),
    map_symbol_fill_class = case_when(
      .data$has_water_supply_index ~ "water_supply_index",
      .data$map_point_role == "river_and_reservoir" ~ "river_reservoir_overlap",
      .data$map_point_role %in% c("reservoir_inflow", "reservoir_release", "reservoir_inflow_and_release") ~ "reservoir",
      .data$map_point_role == "ensemble_forecast_only" ~ "ensemble",
      TRUE ~ "river"
    ),
    map_symbol_outline_class = if_else(
      .data$has_active_ensemble_forecast | .data$has_water_supply_index,
      "ensemble_or_index_available",
      "deterministic_only_or_not_in_ensemble_xml"
    ),
    map_popup_title = paste0(.data$display_name, " (", .data$cnrfc_id, ")"),
    map_popup_subtitle = case_when(
      .data$has_water_supply_index ~ paste0("CNRFC water-supply index: ", .data$water_supply_index_area),
      .data$map_point_role == "river_and_reservoir" ~ "CNRFC active river + reservoir forecast point",
      .data$map_point_role == "reservoir_inflow_and_release" ~ "CNRFC active reservoir inflow + release point",
      .data$map_point_role == "reservoir_inflow" ~ "CNRFC active reservoir inflow point",
      .data$map_point_role == "reservoir_release" ~ "CNRFC active reservoir release point",
      .data$map_point_role == "river_forecast" ~ "CNRFC active river forecast point",
      .data$map_point_role == "ensemble_forecast_only" ~ "CNRFC active ensemble forecast point",
      TRUE ~ "CNRFC active forecast point"
    ),
    ensemble_status_short = if_else(
      .data$has_active_ensemble_forecast | .data$has_water_supply_index,
      "ensemble/index product confirmed",
      "not in active ensemble XML"
    ),
    cnrfc_ensemble_url = case_when(
      .data$has_water_supply_index & pt_has_text(.data$water_supply_index_url) ~ .data$water_supply_index_url,
      .data$has_active_ensemble_forecast ~ paste0("https://www.cnrfc.noaa.gov/ensembleProduct.php?id=", .data$cnrfc_id, "&prodID=3&days=10"),
      TRUE ~ NA_character_
    ),
    cnrfc_source_url = case_when(
      .data$has_water_supply_index & pt_has_text(.data$water_supply_index_url) ~ .data$water_supply_index_url,
      pt_has_text(.data$active_xml_urls) ~ .data$active_xml_urls,
      TRUE ~ "https://www.cnrfc.noaa.gov/"
    ),
    map_popup_note = case_when(
      .data$has_water_supply_index ~ "Symbolic BRIM location for regional CNRFC water-supply index; not an ordinary gage location.",
      TRUE ~ "Active CNRFC XML marker; broad river catalog records are not used as authority for this layer."
    )
  ) %>%
  select(all_of(c(
    "map_layer_key",
    "map_layer_label",
    "cnrfc_id",
    "display_name",
    "map_popup_title",
    "map_popup_subtitle",
    "lat",
    "lon",
    "elev_ft",
    "coordinate_source_type",
    "forecast_point_hydro_type",
    "map_point_role",
    "map_symbol_shape",
    "map_symbol_fill_class",
    "map_symbol_outline_class",
    "is_active_river_forecast",
    "is_active_reservoir_inflow",
    "is_active_reservoir_release",
    "has_active_ensemble_forecast",
    "has_water_supply_index",
    "water_supply_index_area",
    "active_xml_endpoint_keys",
    "active_xml_subtypes",
    "ensemble_status_short",
    "deterministic_forecast_status",
    "ensemble_forecast_status",
    "availability_confidence",
    "cnrfc_source_url",
    "cnrfc_ensemble_url",
    "map_popup_note",
    "run_timestamp"
  ))) %>%
  arrange(.data$map_point_role, .data$cnrfc_id)

precip_weather_stations_map <- precip_weather_stations %>%
  filter(!is.na(.data$lat), !is.na(.data$lon), abs(.data$lat) <= 90, abs(.data$lon) <= 180) %>%
  mutate(
    map_layer_key = "cnrfc_precip_weather_stations",
    map_layer_label = "CNRFC precip/weather stations",
    has_current_wru_precip_product = .data$has_wru_precip_products,
    also_active_river_reservoir_forecast_point = .data$include_in_river_reservoir_forecast_points,
    map_station_role = case_when(
      .data$has_current_wru_precip_product ~ "current_wru_precip_product",
      .data$also_active_river_reservoir_forecast_point ~ "also_active_forecast_point",
      TRUE ~ "station_no_current_wru_confirmed"
    ),
    map_symbol_shape = "circle",
    map_symbol_fill_class = case_when(
      .data$has_current_wru_precip_product ~ "current_wru_precip",
      .data$also_active_river_reservoir_forecast_point ~ "also_active_forecast_point",
      TRUE ~ "no_current_wru_confirmed"
    ),
    map_popup_title = paste0(.data$display_name, " (", .data$cnrfc_id, ")"),
    map_popup_subtitle = case_when(
      .data$has_current_wru_precip_product ~ "CNRFC precip/weather station with current WRU precip product rows",
      .data$also_active_river_reservoir_forecast_point ~ "CNRFC station also used as active forecast point",
      TRUE ~ "CNRFC precip/weather station"
    ),
    cnrfc_source_url = if_else(
      .data$has_current_wru_precip_product,
      "https://www.cnrfc.noaa.gov/water_resources_update.php",
      "https://www.cnrfc.noaa.gov/"
    ),
    map_popup_note = case_when(
      pt_has_text(.data$precip_weather_data_availability_note) ~ .data$precip_weather_data_availability_note,
      TRUE ~ "No current WRU product confirmed in this audit; historical data may be available from CNRFC or another source."
    )
  ) %>%
  select(all_of(c(
    "map_layer_key",
    "map_layer_label",
    "cnrfc_id",
    "display_name",
    "map_popup_title",
    "map_popup_subtitle",
    "lat",
    "lon",
    "elev_ft",
    "coordinate_source_type",
    "precip_weather_station_type_hint",
    "map_station_role",
    "map_symbol_shape",
    "map_symbol_fill_class",
    "has_current_wru_precip_product",
    "also_active_river_reservoir_forecast_point",
    "datatransmission_types",
    "availability_confidence",
    "cnrfc_source_url",
    "map_popup_note",
    "run_timestamp"
  ))) %>%
  arrange(.data$map_station_role, .data$cnrfc_id)

river_reservoir_catalog_review <- master_candidates %>%
  filter(.data$include_in_river_reservoir_catalog_review) %>%
  mutate(
    review_product = "CNRFC broad river/reservoir catalog review",
    review_note = if_else(
      .data$has_broad_river_catalog_only,
      "Broad raw river catalog record; not promoted to active forecast-point layer because it is absent from active XML endpoints.",
      "Included in active XML forecast-point authority."
    )
  ) %>%
  select(all_of(c(
    "review_product",
    "cnrfc_id",
    "display_name",
    "lat",
    "lon",
    "feature_types",
    "raw_families",
    "raw_kinds",
    "raw_class1_values",
    "raw_class2_values",
    "active_xml_endpoint_keys",
    "active_xml_subtypes",
    "has_active_xml_marker",
    "has_broad_river_catalog_only",
    "river_reservoir_review_status",
    "availability_confidence",
    "review_note",
    "run_timestamp"
  ))) %>%
  arrange(desc(.data$has_active_xml_marker), .data$cnrfc_id)

river_reservoir_map_bins <- active_river_reservoir_forecast_points_map %>%
  count(
    map_point_role,
    forecast_point_hydro_type,
    map_symbol_shape,
    map_symbol_fill_class,
    map_symbol_outline_class,
    name = "points"
  ) %>%
  arrange(desc(.data$points), .data$map_point_role) %>%
  mutate(run_timestamp = RUN_TS)

precip_weather_map_bins <- precip_weather_stations_map %>%
  count(
    map_station_role,
    precip_weather_station_type_hint,
    map_symbol_fill_class,
    name = "points"
  ) %>%
  arrange(desc(.data$points), .data$map_station_role) %>%
  mutate(run_timestamp = RUN_TS)

map_ready_summary <- tibble(
  run_timestamp = RUN_TS,
  metric = c(
    "active_river_reservoir_forecast_points_map_rows",
    "active_river_reservoir_forecast_points_with_valid_coordinates",
    "precip_weather_stations_map_rows",
    "precip_weather_stations_with_current_wru_precip",
    "precip_weather_stations_without_current_wru_precip",
    "river_reservoir_catalog_review_rows",
    "broad_catalog_review_rows_not_promoted",
    "water_supply_index_map_points",
    "active_ensemble_forecast_map_points"
  ),
  value = c(
    nrow(active_river_reservoir_forecast_points_map),
    sum(!is.na(active_river_reservoir_forecast_points_map$lat) & !is.na(active_river_reservoir_forecast_points_map$lon), na.rm = TRUE),
    nrow(precip_weather_stations_map),
    sum(precip_weather_stations_map$has_current_wru_precip_product, na.rm = TRUE),
    sum(!precip_weather_stations_map$has_current_wru_precip_product, na.rm = TRUE),
    nrow(river_reservoir_catalog_review),
    sum(river_reservoir_catalog_review$has_broad_river_catalog_only, na.rm = TRUE),
    sum(active_river_reservoir_forecast_points_map$has_water_supply_index, na.rm = TRUE),
    sum(active_river_reservoir_forecast_points_map$has_active_ensemble_forecast, na.rm = TRUE)
  )
)

river_reservoir_point_bins <- river_reservoir_points %>%
  count(
    master_point_layer,
    forecast_point_hydro_type,
    deterministic_forecast_status,
    ensemble_forecast_status,
    availability_confidence,
    name = "points"
  ) %>%
  arrange(desc(.data$points), .data$forecast_point_hydro_type) %>%
  mutate(run_timestamp = RUN_TS)

precip_weather_station_bins <- precip_weather_stations %>%
  count(
    master_point_layer,
    precip_weather_station_type_hint,
    has_wru_precip_products,
    precip_weather_data_availability_note,
    availability_confidence,
    name = "points"
  ) %>%
  arrange(desc(.data$points), .data$precip_weather_station_type_hint) %>%
  mutate(run_timestamp = RUN_TS)

split_layer_summary <- tibble(
  run_timestamp = RUN_TS,
  metric = c(
    "river_reservoir_forecast_point_records",
    "active_xml_unique_ids_any_endpoint",
    "active_xml_river_forecast_combined_ids",
    "active_xml_reservoir_inflow_ids",
    "active_xml_reservoir_release_ids",
    "active_xml_ensemble_forecast_point_ids",
    "river_reservoir_catalog_review_records",
    "broad_river_catalog_not_primary_records",
    "precip_weather_station_records",
    "ids_in_both_future_point_layers",
    "water_supply_index_points",
    "precip_weather_station_current_wru_confirmed",
    "precip_weather_station_without_current_wru_confirmed"
  ),
  value = c(
    nrow(river_reservoir_points),
    sum(master_candidates$has_active_xml_marker, na.rm = TRUE),
    sum(master_candidates$active_xml_has_river_forecast_combined, na.rm = TRUE),
    sum(master_candidates$active_xml_has_reservoir_inflow, na.rm = TRUE),
    sum(master_candidates$active_xml_has_reservoir_release, na.rm = TRUE),
    sum(master_candidates$active_xml_has_ensemble_forecast_point, na.rm = TRUE),
    sum(master_candidates$include_in_river_reservoir_catalog_review, na.rm = TRUE),
    sum(master_candidates$has_broad_river_catalog_only, na.rm = TRUE),
    nrow(precip_weather_stations),
    sum(master_candidates$include_in_river_reservoir_forecast_points &
          master_candidates$include_in_precip_weather_stations, na.rm = TRUE),
    sum(master_candidates$has_water_supply_index, na.rm = TRUE),
    sum(precip_weather_stations$has_wru_precip_products, na.rm = TRUE),
    sum(!precip_weather_stations$has_wru_precip_products, na.rm = TRUE)
  )
)

summary <- tibble(
  run_timestamp = RUN_TS,
  metric = c(
    "feature_master_rows",
    "wru_xml_matrix_rows",
    "basin_matrix_rows",
    "active_xml_endpoint_rows",
    "active_xml_unique_ids_any_endpoint",
    "active_xml_river_forecast_combined_ids",
    "active_xml_reservoir_inflow_ids",
    "active_xml_reservoir_release_ids",
    "active_xml_ensemble_forecast_point_ids",
    "unique_forecast_point_ids",
    "ids_with_any_cnrfc_point",
    "ids_with_wru_xml_products",
    "ids_with_wru_water_supply_or_flow",
    "water_supply_index_points",
    "ids_with_wru_precip_products",
    "ids_with_valid_coordinates",
    "master_candidate_priority_1",
    "master_candidate_priority_2",
    "master_candidate_priority_3",
    "master_candidate_priority_4",
    "master_candidate_priority_6_broad_river_catalog_review",
    "river_reservoir_forecast_point_layer_candidates_primary",
    "river_reservoir_catalog_review_records",
    "broad_river_catalog_not_primary_records",
    "precip_weather_station_layer_candidates",
    "precip_weather_station_current_wru_confirmed",
    "precip_weather_station_without_current_wru_confirmed",
    "river_reservoir_forecast_point_records_nonexclusive_primary",
    "precip_weather_station_records_nonexclusive",
    "ids_in_both_future_point_layers"
  ),
  value = c(
    nrow(feature_master),
    nrow(wru_xml_matrix),
    nrow(basin_matrix),
    nrow(active_xml_matrix),
    nrow(active_xml_summary),
    sum(master_candidates$active_xml_has_river_forecast_combined, na.rm = TRUE),
    sum(master_candidates$active_xml_has_reservoir_inflow, na.rm = TRUE),
    sum(master_candidates$active_xml_has_reservoir_release, na.rm = TRUE),
    sum(master_candidates$active_xml_has_ensemble_forecast_point, na.rm = TRUE),
    nrow(matrix),
    sum(matrix$has_any_cnrfc_point, na.rm = TRUE),
    sum(matrix$has_any_wru_xml_product, na.rm = TRUE),
    sum(matrix$has_wru_water_supply_or_flow, na.rm = TRUE),
    sum(matrix$is_water_supply_index, na.rm = TRUE),
    sum(matrix$has_wru_precip_products, na.rm = TRUE),
    sum(!is.na(matrix$lat) & !is.na(matrix$lon), na.rm = TRUE),
    sum(master_candidates$master_layer_priority == 1L, na.rm = TRUE),
    sum(master_candidates$master_layer_priority == 2L, na.rm = TRUE),
    sum(master_candidates$master_layer_priority == 3L, na.rm = TRUE),
    sum(master_candidates$master_layer_priority == 4L, na.rm = TRUE),
    sum(master_candidates$master_layer_priority == 6L, na.rm = TRUE),
    sum(master_candidates$proposed_master_point_layer == "CNRFC river/reservoir forecast points", na.rm = TRUE),
    sum(master_candidates$include_in_river_reservoir_catalog_review, na.rm = TRUE),
    sum(master_candidates$has_broad_river_catalog_only, na.rm = TRUE),
    sum(master_candidates$proposed_master_point_layer == "CNRFC precip/weather stations", na.rm = TRUE),
    sum(master_candidates$proposed_master_point_layer == "CNRFC precip/weather stations" &
          master_candidates$has_wru_precip_products, na.rm = TRUE),
    sum(master_candidates$proposed_master_point_layer == "CNRFC precip/weather stations" &
          !master_candidates$has_wru_precip_products, na.rm = TRUE),
    nrow(river_reservoir_points),
    nrow(precip_weather_stations),
    sum(master_candidates$include_in_river_reservoir_forecast_points &
          master_candidates$include_in_precip_weather_stations, na.rm = TRUE)
  )
)

# ==== 6. Write outputs =======================================================

out_matrix <- file.path(DIR$rds, "cnrfc_forecast_point_product_availability_matrix.rds")
out_bins <- file.path(DIR$rds, "cnrfc_forecast_point_product_availability_bins.rds")
out_summary <- file.path(DIR$rds, "cnrfc_forecast_point_product_availability_summary.rds")
out_master_candidates <- file.path(DIR$rds, "cnrfc_forecast_point_master_candidates.rds")
out_master_candidate_bins <- file.path(DIR$rds, "cnrfc_forecast_point_master_candidate_bins.rds")
out_layer_family_bins <- file.path(DIR$rds, "cnrfc_forecast_point_layer_family_bins.rds")
out_river_reservoir_points <- file.path(DIR$rds, "cnrfc_river_reservoir_forecast_points.rds")
out_precip_weather_stations <- file.path(DIR$rds, "cnrfc_precip_weather_stations.rds")
out_river_reservoir_point_bins <- file.path(DIR$rds, "cnrfc_river_reservoir_forecast_point_bins.rds")
out_precip_weather_station_bins <- file.path(DIR$rds, "cnrfc_precip_weather_station_bins.rds")
out_split_layer_summary <- file.path(DIR$rds, "cnrfc_forecast_point_split_layer_summary.rds")
out_active_river_reservoir_map <- file.path(DIR$rds, "cnrfc_active_river_reservoir_forecast_points_map.rds")
out_precip_weather_map <- file.path(DIR$rds, "cnrfc_precip_weather_stations_map.rds")
out_river_reservoir_catalog_review <- file.path(DIR$rds, "cnrfc_river_reservoir_catalog_review.rds")
out_river_reservoir_map_bins <- file.path(DIR$rds, "cnrfc_active_river_reservoir_forecast_points_map_bins.rds")
out_precip_weather_map_bins <- file.path(DIR$rds, "cnrfc_precip_weather_stations_map_bins.rds")
out_map_ready_summary <- file.path(DIR$rds, "cnrfc_forecast_point_map_ready_summary.rds")
out_manifest <- file.path(DIR$rds, "cnrfc_forecast_point_product_availability_manifest.rds")

matrix_csv <- file.path(DIR$qa, paste0("cnrfc_forecast_point_product_availability_matrix_", RUN_TS, ".csv"))
bins_csv <- file.path(DIR$qa, paste0("cnrfc_forecast_point_product_availability_bins_", RUN_TS, ".csv"))
summary_csv <- file.path(DIR$qa, paste0("cnrfc_forecast_point_product_availability_summary_", RUN_TS, ".csv"))
master_candidates_csv <- file.path(DIR$qa, paste0("cnrfc_forecast_point_master_candidates_", RUN_TS, ".csv"))
master_candidate_bins_csv <- file.path(DIR$qa, paste0("cnrfc_forecast_point_master_candidate_bins_", RUN_TS, ".csv"))
layer_family_bins_csv <- file.path(DIR$qa, paste0("cnrfc_forecast_point_layer_family_bins_", RUN_TS, ".csv"))
river_reservoir_points_csv <- file.path(DIR$qa, paste0("cnrfc_river_reservoir_forecast_points_", RUN_TS, ".csv"))
precip_weather_stations_csv <- file.path(DIR$qa, paste0("cnrfc_precip_weather_stations_", RUN_TS, ".csv"))
river_reservoir_point_bins_csv <- file.path(DIR$qa, paste0("cnrfc_river_reservoir_forecast_point_bins_", RUN_TS, ".csv"))
precip_weather_station_bins_csv <- file.path(DIR$qa, paste0("cnrfc_precip_weather_station_bins_", RUN_TS, ".csv"))
split_layer_summary_csv <- file.path(DIR$qa, paste0("cnrfc_forecast_point_split_layer_summary_", RUN_TS, ".csv"))
active_river_reservoir_map_csv <- file.path(DIR$qa, paste0("cnrfc_active_river_reservoir_forecast_points_map_", RUN_TS, ".csv"))
precip_weather_map_csv <- file.path(DIR$qa, paste0("cnrfc_precip_weather_stations_map_", RUN_TS, ".csv"))
river_reservoir_catalog_review_csv <- file.path(DIR$qa, paste0("cnrfc_river_reservoir_catalog_review_", RUN_TS, ".csv"))
river_reservoir_map_bins_csv <- file.path(DIR$qa, paste0("cnrfc_active_river_reservoir_forecast_points_map_bins_", RUN_TS, ".csv"))
precip_weather_map_bins_csv <- file.path(DIR$qa, paste0("cnrfc_precip_weather_stations_map_bins_", RUN_TS, ".csv"))
map_ready_summary_csv <- file.path(DIR$qa, paste0("cnrfc_forecast_point_map_ready_summary_", RUN_TS, ".csv"))
manifest_csv <- file.path(DIR$qa, paste0("cnrfc_forecast_point_product_availability_preprocess_manifest_", RUN_TS, ".csv"))

manifest <- tibble(
  run_timestamp = RUN_TS,
  product = c(
    "forecast_point_matrix",
    "forecast_point_bins",
    "forecast_point_summary",
    "forecast_point_master_candidates",
    "forecast_point_master_candidate_bins",
    "forecast_point_layer_family_bins",
    "river_reservoir_forecast_points",
    "precip_weather_stations",
    "river_reservoir_forecast_point_bins",
    "precip_weather_station_bins",
    "forecast_point_split_layer_summary",
    "active_river_reservoir_forecast_points_map",
    "precip_weather_stations_map",
    "river_reservoir_catalog_review",
    "active_river_reservoir_forecast_points_map_bins",
    "precip_weather_stations_map_bins",
    "forecast_point_map_ready_summary"
  ),
  output_rds = c(
    out_matrix,
    out_bins,
    out_summary,
    out_master_candidates,
    out_master_candidate_bins,
    out_layer_family_bins,
    out_river_reservoir_points,
    out_precip_weather_stations,
    out_river_reservoir_point_bins,
    out_precip_weather_station_bins,
    out_split_layer_summary,
    out_active_river_reservoir_map,
    out_precip_weather_map,
    out_river_reservoir_catalog_review,
    out_river_reservoir_map_bins,
    out_precip_weather_map_bins,
    out_map_ready_summary
  ),
  output_csv = c(
    matrix_csv,
    bins_csv,
    summary_csv,
    master_candidates_csv,
    master_candidate_bins_csv,
    layer_family_bins_csv,
    river_reservoir_points_csv,
    precip_weather_stations_csv,
    river_reservoir_point_bins_csv,
    precip_weather_station_bins_csv,
    split_layer_summary_csv,
    active_river_reservoir_map_csv,
    precip_weather_map_csv,
    river_reservoir_catalog_review_csv,
    river_reservoir_map_bins_csv,
    precip_weather_map_bins_csv,
    map_ready_summary_csv
  ),
  rows = c(
    nrow(matrix),
    nrow(bins),
    nrow(summary),
    nrow(master_candidates),
    nrow(master_candidate_bins),
    nrow(layer_family_bins),
    nrow(river_reservoir_points),
    nrow(precip_weather_stations),
    nrow(river_reservoir_point_bins),
    nrow(precip_weather_station_bins),
    nrow(split_layer_summary),
    nrow(active_river_reservoir_forecast_points_map),
    nrow(precip_weather_stations_map),
    nrow(river_reservoir_catalog_review),
    nrow(river_reservoir_map_bins),
    nrow(precip_weather_map_bins),
    nrow(map_ready_summary)
  )
)

saveRDS(matrix, out_matrix)
saveRDS(bins, out_bins)
saveRDS(summary, out_summary)
saveRDS(master_candidates, out_master_candidates)
saveRDS(master_candidate_bins, out_master_candidate_bins)
saveRDS(layer_family_bins, out_layer_family_bins)
saveRDS(river_reservoir_points, out_river_reservoir_points)
saveRDS(precip_weather_stations, out_precip_weather_stations)
saveRDS(river_reservoir_point_bins, out_river_reservoir_point_bins)
saveRDS(precip_weather_station_bins, out_precip_weather_station_bins)
saveRDS(split_layer_summary, out_split_layer_summary)
saveRDS(active_river_reservoir_forecast_points_map, out_active_river_reservoir_map)
saveRDS(precip_weather_stations_map, out_precip_weather_map)
saveRDS(river_reservoir_catalog_review, out_river_reservoir_catalog_review)
saveRDS(river_reservoir_map_bins, out_river_reservoir_map_bins)
saveRDS(precip_weather_map_bins, out_precip_weather_map_bins)
saveRDS(map_ready_summary, out_map_ready_summary)
saveRDS(manifest, out_manifest)

readr::write_csv(matrix, matrix_csv)
readr::write_csv(bins, bins_csv)
readr::write_csv(summary, summary_csv)
readr::write_csv(master_candidates, master_candidates_csv)
readr::write_csv(master_candidate_bins, master_candidate_bins_csv)
readr::write_csv(layer_family_bins, layer_family_bins_csv)
readr::write_csv(river_reservoir_points, river_reservoir_points_csv)
readr::write_csv(precip_weather_stations, precip_weather_stations_csv)
readr::write_csv(river_reservoir_point_bins, river_reservoir_point_bins_csv)
readr::write_csv(precip_weather_station_bins, precip_weather_station_bins_csv)
readr::write_csv(split_layer_summary, split_layer_summary_csv)
readr::write_csv(active_river_reservoir_forecast_points_map, active_river_reservoir_map_csv)
readr::write_csv(precip_weather_stations_map, precip_weather_map_csv)
readr::write_csv(river_reservoir_catalog_review, river_reservoir_catalog_review_csv)
readr::write_csv(river_reservoir_map_bins, river_reservoir_map_bins_csv)
readr::write_csv(precip_weather_map_bins, precip_weather_map_bins_csv)
readr::write_csv(map_ready_summary, map_ready_summary_csv)
readr::write_csv(manifest, manifest_csv)

# Stable review copies.
readr::write_csv(matrix, file.path(DIR$qa, "cnrfc_forecast_point_product_availability_matrix_latest.csv"))
readr::write_csv(bins, file.path(DIR$qa, "cnrfc_forecast_point_product_availability_bins_latest.csv"))
readr::write_csv(summary, file.path(DIR$qa, "cnrfc_forecast_point_product_availability_summary_latest.csv"))
readr::write_csv(master_candidates, file.path(DIR$qa, "cnrfc_forecast_point_master_candidates_latest.csv"))
readr::write_csv(master_candidate_bins, file.path(DIR$qa, "cnrfc_forecast_point_master_candidate_bins_latest.csv"))
readr::write_csv(layer_family_bins, file.path(DIR$qa, "cnrfc_forecast_point_layer_family_bins_latest.csv"))
readr::write_csv(river_reservoir_points, file.path(DIR$qa, "cnrfc_river_reservoir_forecast_points_latest.csv"))
readr::write_csv(precip_weather_stations, file.path(DIR$qa, "cnrfc_precip_weather_stations_latest.csv"))
readr::write_csv(river_reservoir_point_bins, file.path(DIR$qa, "cnrfc_river_reservoir_forecast_point_bins_latest.csv"))
readr::write_csv(precip_weather_station_bins, file.path(DIR$qa, "cnrfc_precip_weather_station_bins_latest.csv"))
readr::write_csv(split_layer_summary, file.path(DIR$qa, "cnrfc_forecast_point_split_layer_summary_latest.csv"))
readr::write_csv(active_river_reservoir_forecast_points_map, file.path(DIR$qa, "cnrfc_active_river_reservoir_forecast_points_map_latest.csv"))
readr::write_csv(precip_weather_stations_map, file.path(DIR$qa, "cnrfc_precip_weather_stations_map_latest.csv"))
readr::write_csv(river_reservoir_catalog_review, file.path(DIR$qa, "cnrfc_river_reservoir_catalog_review_latest.csv"))
readr::write_csv(river_reservoir_map_bins, file.path(DIR$qa, "cnrfc_active_river_reservoir_forecast_points_map_bins_latest.csv"))
readr::write_csv(precip_weather_map_bins, file.path(DIR$qa, "cnrfc_precip_weather_stations_map_bins_latest.csv"))
readr::write_csv(map_ready_summary, file.path(DIR$qa, "cnrfc_forecast_point_map_ready_summary_latest.csv"))

pt_log("CNRFC forecast-point product availability preprocess complete.")
message("  Forecast point matrix RDS: ", out_matrix, " (", nrow(matrix), " rows)")
message("  Forecast point bins RDS:   ", out_bins, " (", nrow(bins), " rows)")
message("  Summary RDS:               ", out_summary, " (", nrow(summary), " rows)")
message("  Master candidates RDS:     ", out_master_candidates, " (", nrow(master_candidates), " rows)")
message("  Master candidate bins RDS: ", out_master_candidate_bins, " (", nrow(master_candidate_bins), " rows)")
message("  Layer family bins RDS:     ", out_layer_family_bins, " (", nrow(layer_family_bins), " rows)")
message("  River/reservoir points RDS:", out_river_reservoir_points, " (", nrow(river_reservoir_points), " rows)")
message("  Precip/weather points RDS: ", out_precip_weather_stations, " (", nrow(precip_weather_stations), " rows)")
message("  Active rr map RDS:         ", out_active_river_reservoir_map, " (", nrow(active_river_reservoir_forecast_points_map), " rows)")
message("  Precip/weather map RDS:    ", out_precip_weather_map, " (", nrow(precip_weather_stations_map), " rows)")
message("  Catalog review RDS:        ", out_river_reservoir_catalog_review, " (", nrow(river_reservoir_catalog_review), " rows)")
message("  Manifest CSV:              ", manifest_csv)
message("")
message("Observed forecast-point product bins:")
print(bins)
message("")
message("Master candidate review bins:")
print(master_candidate_bins)
message("")
message("Proposed point-layer family bins:")
print(layer_family_bins)
message("")
message("Non-exclusive split layer summary:")
print(split_layer_summary)
message("")
message("River/reservoir point bins:")
print(river_reservoir_point_bins)
message("")
message("Precip/weather station bins:")
print(precip_weather_station_bins)
message("")
message("Map-ready river/reservoir bins:")
print(river_reservoir_map_bins)
message("")
message("Map-ready summary:")
print(map_ready_summary)
message("")
message("Important interpretation:")
message("  This 53_ preprocessor is a staging product for two future CNRFC point layers:")
message("    1. river/reservoir forecast points")
message("    2. precip/weather stations")
message("  Active CNRFC XML endpoints are now the authority for river/reservoir forecast points.")
message("  Ensemble forecast markers come from ensPoints.xml; broad raw river catalog remains review-only.")
message("  Precip/weather stations are retained even when current WRU data are not confirmed;")
message("    the review table notes that historical data may be available from CNRFC or another source.")
message("  Split layer tables are intentionally non-exclusive: one CNRFC ID can appear in both future point layers.")
message("  River/reservoir forecast-point output is now limited to active XML markers plus the 3 manual water-supply index points.")
message("  Patch 035 adds slim map-ready RDS outputs for layer implementation; use those instead of bulky audit tables.")
message("  Broad raw CNRFC River Other catalog IDs are retained in review tables but not promoted to the primary river/reservoir output.")
message("  Basin product availability remains in 52_; this 53_ script does not symbolize or modify the map.")
