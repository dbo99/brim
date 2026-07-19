# ==== 16_swrcb_waterrights_pod.r ============================================
##
## PURPOSE:
##   Build a practical, lightweight RDS layer of California SWRCB / CalWATRS
##   points of diversion (PODs), enriched with selected water-right attributes
##   from the California Water Rights LIST.
##
##   Also create a separate BLM-managed-land clipped POD layer using the current
##   PortaTreasure2 BLM managed core polygon:
##
##     04_processed_data/rds/blm_managed_core_3310.rds
##
## DESIGN:
##   - Keep one row per POD point.
##   - Do not explode duplicate geometries when multiple WR records match a POD.
##   - Collapse duplicate WR values into semicolon-separated text fields.
##   - Save both:
##       1. statewide joined POD/WR layer
##       2. BLM-managed-land clipped POD/WR layer
##
## NORMAL USE AFTER SOURCE DATA ARE CACHED:
##   ALLOW_DOWNLOADS <- FALSE
##   REFRESH_EXISTING_CACHE <- FALSE
##   REBUILD_JOINED_LAYER <- TRUE or FALSE as needed
##   REBUILD_BLM_CLIP <- TRUE when BLM lands or joined POD layer changed
##
## FIRST TEST / FRESH DOWNLOAD:
##   ALLOW_DOWNLOADS <- TRUE
##   REFRESH_EXISTING_CACHE <- TRUE
##
## OUTPUTS:
##   Raw source caches:
##     01_raw_data/swrcb_water_rights/cache/
##
##   Processed RDS:
##     04_processed_data/rds/swrcb_pod_wr_joined_lean_wgs84.rds
##     04_processed_data/rds/swrcb_pod_wr_blm_managed_wgs84.rds
##
##   QA:
##     04_processed_data/qa/swrcb_pod_wr_join_summary.csv
##     04_processed_data/qa/swrcb_pod_wr_blm_clip_summary.csv
##
## OPTIONAL:
##   GeoPackage outputs can be enabled for GIS checking.
## ============================================================================


# ==== 1. Load configuration ==================================================

source("00_config/config_paths.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")


# ==== 2. Load packages =======================================================

packages <- c(
  "sf",
  "dplyr",
  "stringr",
  "tibble",
  "readr",
  "purrr",
  "httr2",
  "jsonlite",
  "janitor"
)

missing_packages <- packages[!packages %in% rownames(installed.packages())]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(readr)
  library(purrr)
  library(httr2)
  library(jsonlite)
  library(janitor)
})


# ==== 3. User-facing switches ================================================

## First test / fresh download:
ALLOW_DOWNLOADS <- FALSE
REFRESH_EXISTING_CACHE <- FALSE

## After the first successful download, usually switch these to:
##   ALLOW_DOWNLOADS <- FALSE
##   REFRESH_EXISTING_CACHE <- FALSE

REBUILD_JOINED_LAYER <- TRUE
REBUILD_BLM_RELEVANT <- TRUE
WRITE_BLM_RELEVANT_RDS <- TRUE
WRITE_BLM_RELEVANT_GPKG <- FALSE
WRITE_JOINED_RDS <- TRUE
WRITE_JOINED_GPKG <- FALSE
WRITE_QA <- TRUE

## ArcGIS FeatureServer chunk settings.
CHUNK_SIZE <- 1000
DOWNLOAD_PAUSE_SEC <- 0.15

## Explicit join columns.
## These are intentionally not auto-detected.
POD_JOIN_COL <- "wr_id"
WR_JOIN_COL  <- "application_number"


# ==== 4. Project paths and output files ======================================

## Raw source cache folder.
DIR$swrcb_cache <- file.path(DIR$raw, "swrcb_water_rights", "cache")
dir.create(DIR$swrcb_cache, showWarnings = FALSE, recursive = TRUE)

## Processed output folders should already exist from config_paths.r, but this
## keeps the script robust when run in a fresh project clone.
dir.create(DIR$rds, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR$gpkg, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR$qa, showWarnings = FALSE, recursive = TRUE)

## Raw source cache files.
POD_RDS_CACHE <- file.path(
  DIR$swrcb_cache,
  "calwatrs_public_point_of_diversion_raw.rds"
)

WR_RDS_CACHE <- file.path(
  DIR$swrcb_cache,
  "ca_open_data_water_rights_list_raw.rds"
)

## Main processed outputs.
OUT_JOINED_RDS <- file.path(
  DIR$rds,
  "swrcb_pod_wr_joined_lean_wgs84.rds"
)

OUT_BLM_RELEVANT_RDS <- file.path(
  DIR$rds,
  "swrcb_pod_wr_blm_relevant_wgs84.rds"
)

OUT_BLM_RELEVANT_GPKG <- file.path(
  DIR$gpkg,
  "swrcb_pod_wr_blm_relevant_wgs84.gpkg"
)

OUT_BLM_RELEVANT_SUMMARY_CSV <- file.path(
  DIR$qa,
  "swrcb_pod_wr_blm_relevant_summary.csv"
)

OUT_BLM_RELEVANT_REASON_COUNTS_CSV <- file.path(
  DIR$qa,
  "swrcb_pod_wr_blm_relevant_reason_counts.csv"
)



OUT_JOINED_GPKG <- file.path(
  DIR$gpkg,
  "swrcb_pod_wr_joined_lean_wgs84.gpkg"
)



OUT_JOIN_SUMMARY_CSV <- file.path(
  DIR$qa,
  "swrcb_pod_wr_join_summary.csv"
)



## Current BLM managed core layer.
BLM_MANAGED_CORE_RDS <- file.path(
  DIR$rds,
  "blm_managed_core_3310.rds"
)


# ==== 5. Source URLs =========================================================

## Official CalWATRS ArcGIS FeatureServer.
## Layer 0 is the Public Point of Diversion layer.
CALWATRS_SERVICE <- "https://cw-gis.waterboards.ca.gov/hosted/rest/services/Public/cal_watrs_gis_water_rights/FeatureServer"
POD_LAYER_URL <- paste0(CALWATRS_SERVICE, "/0")

## Water Rights LIST fallback CSV.
## The script first tries the CA Open Data CKAN API to find the current CSV.
## If that fails, it falls back to this known CSV URL.
WR_LIST_FALLBACK_URL <- "https://data.ca.gov/dataset/9ae95238-12f9-47dd-bc62-e6409920607e/resource/151c067a-088b-42a2-b6ad-99d84b48fb36/download/water_rights_list_2024-01-20.csv"


# ==== 6. Field lists =========================================================

## WR fields to keep.
## Keep this small. Add more later only if needed for popups/symbology/filtering.
WR_KEEP_FIELDS <- c(
  "application_number",
  "wr_water_right_id",
  "certificate_id",
  "permit_id",
  "license_id",
  "water_right_type",
  "water_right_status",
  "priority_date",
  "receipt_date",
  "application_recd_date",
  "application_acceptance_date",
  "application_primary_owner",
  "primary_owner_name",
  "primary_owner_entity_type",
  "relationship_type",
  "face_value_amount",
  "face_value_units",
  "ini_reported_div_amount",
  "ini_reported_div_unit",
  "max_dd_appl",
  "max_dd_units",
  "max_dd_ann",
  "max_storage",
  "max_taken_from_source",
  "recent_water_use_min",
  "water_use_min_unit",
  "recent_water_use_max",
  "water_use_max_unit",
  "use_code",
  "use_status",
  "pod_id",
  "pod_id_gis",
  "pod_status",
  "pod_number",
  "pod_number_gis",
  "pod_name",
  "pod_type",
  "source_type",
  "source_name",
  "watershed",
  "county",
  "huc_12_number",
  "huc_12_name",
  "huc_8_number",
  "huc_8_name"
)

## POD fields to keep.
## Geometry is preserved separately by sf.
POD_KEEP_FIELDS <- c(
  "objectid",
  "wr_id",
  "globalid",
  "primary_owner",
  "app_type",
  "subtype",
  "diversion_type",
  "diversion_rate",
  "diversion_status",
  "pod_status",
  "pod_id",
  "pod_id_gis",
  "pod_name",
  "pod_type",
  "pod_number",
  "pod_number_gis",
  "source_type",
  "source_name",
  "watershed",
  "county",
  "huc_12_number",
  "huc_12_name",
  "huc_8_number",
  "huc_8_name"
)


# ==== 7. Helper functions ====================================================

query_url <- function(layer_or_table_url) {
  paste0(layer_or_table_url, "/query")
}


normalize_id <- function(x) {
  x |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_to_upper()
}


collapse_unique <- function(x, sep = "; ") {
  
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- unique(x[!is.na(x) & x != ""])
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  paste(x, collapse = sep)
}


coerce_sf_attributes_to_character <- function(x) {
  
  geom_col <- attr(x, "sf_column")
  attr_cols <- setdiff(names(x), geom_col)
  
  x[attr_cols] <- lapply(x[attr_cols], as.character)
  
  x
}


stop_with_column_names <- function(data_name, df, missing_col) {
  
  cat("\n\nColumn not found in ", data_name, ":\n", sep = "")
  cat("  ", missing_col, "\n\n", sep = "")
  
  cat("Available columns in ", data_name, ":\n", sep = "")
  cat(names(df), sep = "\n")
  
  stop(
    "\n\nUpdate the explicit join column setting near the top of the script.",
    call. = FALSE
  )
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
      "To fetch/update this source, set:\n",
      "  ALLOW_DOWNLOADS <- TRUE\n",
      "  REFRESH_EXISTING_CACHE <- TRUE   # if replacing existing cache\n",
      call. = FALSE
    )
  }
  
  message("Fetching ", label, " from source...")
  x <- fetch_fun()
  
  saveRDS(x, cache_path)
  message("Cached ", label, " to: ", cache_path)
  
  x
}


safe_numeric <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}


make_clip_summary <- function(full_sf, blm_sf, clipped_sf) {
  
  tibble::tibble(
    layer = c(
      "statewide_joined_pod_wr",
      "blm_managed_clip"
    ),
    rows = c(
      nrow(full_sf),
      nrow(clipped_sf)
    ),
    blm_polygon_rows = c(
      nrow(blm_sf),
      nrow(blm_sf)
    ),
    full_layer_crs = c(
      sf::st_crs(full_sf)$input,
      sf::st_crs(full_sf)$input
    ),
    blm_layer_crs = c(
      sf::st_crs(blm_sf)$input,
      sf::st_crs(blm_sf)$input
    ),
    output_crs = c(
      sf::st_crs(full_sf)$input,
      sf::st_crs(clipped_sf)$input
    )
  )
}


# ==== 8. Source download functions ===========================================

get_arcgis_object_ids <- function(layer_or_table_url) {
  
  message("Getting ObjectIDs from: ", layer_or_table_url)
  
  resp <- httr2::request(query_url(layer_or_table_url)) |>
    httr2::req_method("POST") |>
    httr2::req_body_form(
      where = "1=1",
      returnIdsOnly = "true",
      f = "json"
    ) |>
    httr2::req_perform()
  
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  
  if (is.null(body$objectIds)) {
    stop("No objectIds returned. Check service URL or source availability.", call. = FALSE)
  }
  
  sort(body$objectIds)
}


fetch_pod_layer <- function(chunk_size = CHUNK_SIZE, out_sr = 4326) {
  
  object_ids <- get_arcgis_object_ids(POD_LAYER_URL)
  
  message("Total POD records to download: ", length(object_ids))
  
  id_chunks <- split(
    object_ids,
    ceiling(seq_along(object_ids) / chunk_size)
  )
  
  out <- purrr::map(seq_along(id_chunks), function(i) {
    
    ids <- id_chunks[[i]]
    
    message(
      "Downloading POD chunk ",
      i,
      " of ",
      length(id_chunks),
      " | records: ",
      length(ids)
    )
    
    tmp <- tempfile(fileext = ".geojson")
    
    httr2::request(query_url(POD_LAYER_URL)) |>
      httr2::req_method("POST") |>
      httr2::req_body_form(
        objectIds = paste(ids, collapse = ","),
        outFields = "*",
        returnGeometry = "true",
        outSR = as.character(out_sr),
        f = "geojson"
      ) |>
      httr2::req_perform(path = tmp)
    
    Sys.sleep(DOWNLOAD_PAUSE_SEC)
    
    sf::st_read(tmp, quiet = TRUE) |>
      coerce_sf_attributes_to_character()
  })
  
  dplyr::bind_rows(out) |>
    janitor::clean_names()
}


get_current_wr_list_url <- function() {
  
  ckan_url <- "https://data.ca.gov/api/3/action/package_show?id=water-rights"
  
  message("Trying CA Open Data API for current Water Rights LIST CSV...")
  
  tryCatch({
    
    pkg <- jsonlite::fromJSON(ckan_url, flatten = TRUE)
    
    resources <- tibble::as_tibble(pkg$result$resources)
    
    candidates <- resources |>
      dplyr::filter(
        stringr::str_detect(
          .data$name,
          stringr::regex("California Water Rights LIST", ignore_case = TRUE)
        ),
        stringr::str_detect(
          .data$format,
          stringr::regex("CSV", ignore_case = TRUE)
        )
      )
    
    if (nrow(candidates) < 1) {
      stop("No matching Water Rights LIST CSV resource found.")
    }
    
    candidates$url[[1]]
    
  }, error = function(e) {
    
    message("Could not get current URL from CA Open Data API. Using fallback CSV URL.")
    WR_LIST_FALLBACK_URL
  })
}


fetch_wr_list <- function() {
  
  wr_url <- get_current_wr_list_url()
  
  message("Reading Water Rights LIST from:")
  message(wr_url)
  
  readr::read_csv(
    wr_url,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) |>
    janitor::clean_names()
}


# ==== 9. Load or fetch source data ===========================================

pod_raw <- load_or_fetch(
  cache_path = POD_RDS_CACHE,
  fetch_fun = fetch_pod_layer,
  label = "CalWATRS Public Point of Diversion layer"
)

wr_raw <- load_or_fetch(
  cache_path = WR_RDS_CACHE,
  fetch_fun = fetch_wr_list,
  label = "CA Open Data Water Rights LIST"
)

names(pod_raw) <- janitor::make_clean_names(names(pod_raw))
names(wr_raw)  <- janitor::make_clean_names(names(wr_raw))

pod_attr_raw <- pod_raw |>
  sf::st_drop_geometry()

cat("\nLoaded source data:\n")
cat("  POD rows: ", nrow(pod_raw), " | columns: ", ncol(pod_raw), "\n", sep = "")
cat("  WR rows:  ", nrow(wr_raw),  " | columns: ", ncol(wr_raw),  "\n", sep = "")


# ==== 10. Build statewide joined POD/WR layer ================================

if (REBUILD_JOINED_LAYER || !file.exists(OUT_JOINED_RDS)) {
  
  # ---- 10.1 Validate join columns -------------------------------------------
  
  if (!POD_JOIN_COL %in% names(pod_attr_raw)) {
    stop_with_column_names("POD data", pod_attr_raw, POD_JOIN_COL)
  }
  
  if (!WR_JOIN_COL %in% names(wr_raw)) {
    stop_with_column_names("Water Rights data", wr_raw, WR_JOIN_COL)
  }
  
  cat("\nUsing explicit join columns:\n")
  cat("  POD_JOIN_COL: ", POD_JOIN_COL, "\n", sep = "")
  cat("  WR_JOIN_COL:  ", WR_JOIN_COL,  "\n", sep = "")
  
  
  # ---- 10.2 Reduce POD and WR tables before joining -------------------------
  
  pod_keep_existing <- unique(c(
    POD_JOIN_COL,
    intersect(POD_KEEP_FIELDS, names(pod_attr_raw))
  ))
  
  pod_attr <- pod_attr_raw |>
    dplyr::select(dplyr::all_of(pod_keep_existing))
  
  wr_keep_existing <- unique(c(
    WR_JOIN_COL,
    intersect(WR_KEEP_FIELDS, names(wr_raw))
  ))
  
  wr <- wr_raw |>
    dplyr::select(dplyr::all_of(wr_keep_existing))
  
  cat("\nReduced tables before join:\n")
  cat("  POD kept columns: ", ncol(pod_attr), "\n", sep = "")
  cat("  WR kept columns:  ", ncol(wr), "\n", sep = "")
  
  
  # ---- 10.3 Prepare normalized join keys ------------------------------------
  
  pod_attr_keyed <- pod_attr |>
    dplyr::mutate(
      .pod_row_id = dplyr::row_number(),
      .pod_join_key = normalize_id(.data[[POD_JOIN_COL]])
    )
  
  wr_keyed <- wr |>
    dplyr::mutate(
      .wr_join_key = normalize_id(.data[[WR_JOIN_COL]])
    )
  
  
  # ---- 10.4 Summarize join quality before joining ---------------------------
  
  pod_key_summary <- pod_attr_keyed |>
    dplyr::summarize(
      pod_rows = dplyr::n(),
      pod_rows_with_join_key = sum(!is.na(.pod_join_key) & .pod_join_key != ""),
      unique_pod_join_keys = dplyr::n_distinct(
        .pod_join_key[!is.na(.pod_join_key) & .pod_join_key != ""]
      )
    )
  
  wr_key_summary <- wr_keyed |>
    dplyr::summarize(
      wr_rows = dplyr::n(),
      wr_rows_with_join_key = sum(!is.na(.wr_join_key) & .wr_join_key != ""),
      unique_wr_join_keys = dplyr::n_distinct(
        .wr_join_key[!is.na(.wr_join_key) & .wr_join_key != ""]
      )
    )
  
  match_summary <- pod_attr_keyed |>
    dplyr::summarize(
      pod_rows_matching_at_least_one_wr = sum(
        !is.na(.pod_join_key) &
          .pod_join_key != "" &
          .pod_join_key %in% wr_keyed$.wr_join_key
      ),
      pod_rows_not_matching_wr = sum(
        is.na(.pod_join_key) |
          .pod_join_key == "" |
          !(.pod_join_key %in% wr_keyed$.wr_join_key)
      )
    )
  
  wr_matches_per_key <- wr_keyed |>
    dplyr::filter(!is.na(.wr_join_key), .wr_join_key != "") |>
    dplyr::count(.wr_join_key, name = "wr_records_for_key")
  
  pod_match_counts <- pod_attr_keyed |>
    dplyr::left_join(
      wr_matches_per_key,
      by = c(".pod_join_key" = ".wr_join_key")
    ) |>
    dplyr::mutate(
      wr_records_for_key = dplyr::if_else(
        is.na(.data$wr_records_for_key),
        0L,
        as.integer(.data$wr_records_for_key)
      )
    )
  
  duplicate_match_summary <- pod_match_counts |>
    dplyr::summarize(
      pod_rows_with_zero_wr_matches = sum(.data$wr_records_for_key == 0),
      pod_rows_with_one_wr_match = sum(.data$wr_records_for_key == 1),
      pod_rows_with_multiple_wr_matches = sum(.data$wr_records_for_key > 1),
      max_wr_records_for_one_pod_key = max(.data$wr_records_for_key, na.rm = TRUE)
    )
  
  join_summary <- dplyr::bind_cols(
    pod_key_summary,
    wr_key_summary,
    match_summary,
    duplicate_match_summary
  )
  
  cat("\nJoin summary before output:\n")
  print(join_summary, width = 1200)
  
  if (WRITE_QA) {
    readr::write_csv(join_summary, OUT_JOIN_SUMMARY_CSV)
    cat("\nSaved join QA CSV:\n  ", normalizePath(OUT_JOIN_SUMMARY_CSV), "\n", sep = "")
  }
  
  
  # ---- 10.5 Collapse WR records to one row per application_number -----------
  
  wr_prefixed <- wr_keyed |>
    dplyr::rename_with(
      .fn = ~ paste0("wr_", .x),
      .cols = -c(.wr_join_key)
    )
  
  wr_collapsed <- wr_prefixed |>
    dplyr::filter(!is.na(.wr_join_key), .wr_join_key != "") |>
    dplyr::group_by(.wr_join_key) |>
    dplyr::summarize(
      wr_match_count = dplyr::n(),
      dplyr::across(
        .cols = everything(),
        .fns = collapse_unique
      ),
      .groups = "drop"
    )
  
  cat("\nCollapsed WR table:\n")
  cat("  Rows: ", nrow(wr_collapsed), "\n", sep = "")
  cat("  Columns: ", ncol(wr_collapsed), "\n", sep = "")
  
  
  # ---- 10.6 Join collapsed WR attributes to POD attributes ------------------
  
  joined_attr <- pod_attr_keyed |>
    dplyr::left_join(
      wr_collapsed,
      by = c(".pod_join_key" = ".wr_join_key"),
      relationship = "many-to-one"
    )
  
  if (nrow(joined_attr) != nrow(pod_raw)) {
    stop(
      "Collapsed join unexpectedly changed the number of POD rows. ",
      "This should not happen because WR records were collapsed first.",
      call. = FALSE
    )
  }
  
  
  # ---- 10.7 Rebuild sf object with original geometry ------------------------
  
  pod_wr_joined <- sf::st_sf(
    joined_attr |>
      dplyr::select(-.pod_row_id),
    geometry = sf::st_geometry(pod_raw),
    crs = sf::st_crs(pod_raw)
  )
  
  ## Normalize output CRS to WGS84 for Leaflet/cache consistency.
  if (is.na(sf::st_crs(pod_wr_joined))) {
    warning("POD joined layer has missing CRS. Assuming EPSG:4326.")
    sf::st_crs(pod_wr_joined) <- 4326
  }
  
  pod_wr_joined <- pod_wr_joined |>
    sf::st_transform(4326)
  
  cat("\nFinal statewide joined POD layer:\n")
  cat("  Rows: ", nrow(pod_wr_joined), "\n", sep = "")
  cat("  Columns: ", ncol(pod_wr_joined), "\n", sep = "")
  cat("  CRS: ", sf::st_crs(pod_wr_joined)$input, "\n", sep = "")
  
  
  # ---- 10.8 Save statewide joined outputs -----------------------------------
  
  if (WRITE_JOINED_RDS) {
    saveRDS(pod_wr_joined, OUT_JOINED_RDS)
    cat("\nSaved statewide joined RDS:\n  ", normalizePath(OUT_JOINED_RDS), "\n", sep = "")
  }
  
  if (WRITE_JOINED_GPKG) {
    sf::st_write(
      pod_wr_joined,
      OUT_JOINED_GPKG,
      layer = "swrcb_pod_wr_joined_lean_wgs84",
      delete_dsn = TRUE,
      quiet = TRUE
    )
    
    cat("\nSaved statewide joined GeoPackage:\n  ", normalizePath(OUT_JOINED_GPKG), "\n", sep = "")
  }
  
} else {
  
  message("Reading existing statewide joined POD/WR layer: ", OUT_JOINED_RDS)
  pod_wr_joined <- readRDS(OUT_JOINED_RDS)
}


# ==== 11. Build BLM-relevant POD/WR layer ====================================
##
## PURPOSE:
##   Create a single best-first-cut SWRCB / CalWATRS layer relevant to BLM.
##
## Inclusion logic:
##   Include a POD if either:
##     1. The mapped POD coordinate falls on current BLM-managed land; OR
##     2. Owner/holder text appears to reference BLM.
##
## IMPORTANT:
##   We do not bind two separate subsets. Instead, we add both flags to the
##   statewide one-row-per-POD layer and then filter once. This naturally avoids
##   duplicate mapped points.

if (REBUILD_BLM_RELEVANT || !file.exists(OUT_BLM_RELEVANT_RDS)) {
  
  if (!file.exists(BLM_MANAGED_CORE_RDS)) {
    stop("Missing BLM managed core RDS: ", BLM_MANAGED_CORE_RDS, call. = FALSE)
  }
  
  message("\nReading BLM managed core for POD relevance screening:")
  message("  ", BLM_MANAGED_CORE_RDS)
  
  blm_core <- readRDS(BLM_MANAGED_CORE_RDS)
  
  if (!inherits(blm_core, "sf")) {
    stop("BLM managed core is not an sf object: ", BLM_MANAGED_CORE_RDS, call. = FALSE)
  }
  
  # ---- 11.1 Spatial flag: POD coordinate falls on BLM-managed land ----------
  
  ## Keep BLM geometry only. Dissolve defensively in case the core has more than
  ## one feature. Do not carry BLM attributes into the POD output.
  blm_core_geom <- blm_core |>
    sf::st_make_valid() |>
    dplyr::summarize(
      geometry = sf::st_union(geometry),
      .groups = "drop"
    )
  
  ## Transform PODs into BLM CRS for point-in-polygon test.
  pod_for_clip <- pod_wr_joined |>
    sf::st_transform(sf::st_crs(blm_core_geom))
  
  ## Handle empty geometries safely.
  nonempty <- !sf::st_is_empty(pod_for_clip)
  
  on_blm_managed <- rep(FALSE, nrow(pod_for_clip))
  
  if (any(nonempty)) {
    
    hits <- sf::st_intersects(
      pod_for_clip[nonempty, ],
      blm_core_geom,
      sparse = TRUE
    )
    
    on_blm_managed[nonempty] <- lengths(hits) > 0
  }
  
  # ---- 11.2 Text flag: owner/holder appears to reference BLM ----------------
  
  ## Helper: safely pull a character field.
  get_chr <- function(x, nm) {
    
    if (nm %in% names(x)) {
      out <- as.character(x[[nm]])
    } else {
      out <- rep(NA_character_, nrow(x))
    }
    
    out <- trimws(out)
    out[out == ""] <- NA_character_
    out
  }
  
  owner_text_1 <- get_chr(pod_wr_joined, "wr_primary_owner_name")
  owner_text_2 <- get_chr(pod_wr_joined, "wr_application_primary_owner")
  owner_text_3 <- get_chr(pod_wr_joined, "primary_owner")
  
  blm_owner_search_text <- paste(
    dplyr::coalesce(owner_text_1, ""),
    dplyr::coalesce(owner_text_2, ""),
    dplyr::coalesce(owner_text_3, ""),
    sep = " | "
  )
  
  ## Conservative BLM text search.
  ##
  ## This catches common BLM owner/holder variants but avoids broad terms like
  ## "Interior" alone.
  blm_name_pattern <- stringr::regex(
    paste(
      "\\bUS\\s+BUREAU\\s+OF\\s+LAND\\s+MANAGEMENT\\b",
      "\\bU\\.?S\\.?\\s+BUREAU\\s+OF\\s+LAND\\s+MANAGEMENT\\b",
      "\\bBUREAU\\s+OF\\s+LAND\\s+MANAGEMENT\\b",
      "\\bUNITED\\s+STATES\\s+BUREAU\\s+OF\\s+LAND\\s+MANAGEMENT\\b",
      "\\bUSDI\\s+BLM\\b",
      "\\bDOI\\s+BLM\\b",
      "\\bBLM\\s+DOI\\b",
      "\\bBLM\\b",
      sep = "|"
    ),
    ignore_case = TRUE
  )
  
  blm_name_match <- stringr::str_detect(
    blm_owner_search_text,
    blm_name_pattern
  )
  
  blm_name_match[is.na(blm_name_match)] <- FALSE
  
  # ---- 11.3 Add flags and build final BLM-relevant layer --------------------
  
  pod_wr_joined <- pod_wr_joined |>
    dplyr::mutate(
      on_blm_managed = on_blm_managed,
      blm_name_match = blm_name_match,
      blm_match_text = dplyr::if_else(
        blm_name_match,
        blm_owner_search_text,
        NA_character_
      ),
      blm_include_reason = dplyr::case_when(
        on_blm_managed & blm_name_match ~ "Spatial + name match",
        on_blm_managed                  ~ "Spatial only",
        blm_name_match                  ~ "Name match only",
        TRUE                            ~ NA_character_
      )
    )
  
  pod_wr_blm_relevant <- pod_wr_joined |>
    dplyr::filter(.data$on_blm_managed | .data$blm_name_match) |>
    sf::st_transform(4326)
  
  cat("\nBLM-relevant POD/WR summary:\n")
  cat("  Statewide POD/WR rows: ", nrow(pod_wr_joined), "\n", sep = "")
  cat("  On BLM-managed land: ", sum(pod_wr_joined$on_blm_managed, na.rm = TRUE), "\n", sep = "")
  cat("  BLM owner/name match: ", sum(pod_wr_joined$blm_name_match, na.rm = TRUE), "\n", sep = "")
  cat("  Final BLM-relevant rows: ", nrow(pod_wr_blm_relevant), "\n", sep = "")
  
  blm_relevant_summary <- tibble::tibble(
    metric = c(
      "statewide_joined_pod_wr_rows",
      "on_blm_managed_rows",
      "blm_name_match_rows",
      "spatial_plus_name_match_rows",
      "spatial_only_rows",
      "name_match_only_rows",
      "final_blm_relevant_rows"
    ),
    value = c(
      nrow(pod_wr_joined),
      sum(pod_wr_joined$on_blm_managed, na.rm = TRUE),
      sum(pod_wr_joined$blm_name_match, na.rm = TRUE),
      sum(
        pod_wr_joined$on_blm_managed &
          pod_wr_joined$blm_name_match,
        na.rm = TRUE
      ),
      sum(
        pod_wr_joined$on_blm_managed &
          !pod_wr_joined$blm_name_match,
        na.rm = TRUE
      ),
      sum(
        !pod_wr_joined$on_blm_managed &
          pod_wr_joined$blm_name_match,
        na.rm = TRUE
      ),
      nrow(pod_wr_blm_relevant)
    )
  )
  
  blm_relevant_reason_counts <- pod_wr_blm_relevant |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::count(blm_include_reason, name = "n") |>
    dplyr::arrange(blm_include_reason)
  
  if (WRITE_QA) {
    
    readr::write_csv(
      blm_relevant_summary,
      OUT_BLM_RELEVANT_SUMMARY_CSV
    )
    
    readr::write_csv(
      blm_relevant_reason_counts,
      OUT_BLM_RELEVANT_REASON_COUNTS_CSV
    )
    
    cat(
      "\nSaved BLM-relevant QA CSV:\n  ",
      normalizePath(OUT_BLM_RELEVANT_SUMMARY_CSV),
      "\n",
      sep = ""
    )
    
    cat(
      "Saved BLM-relevant reason-count QA CSV:\n  ",
      normalizePath(OUT_BLM_RELEVANT_REASON_COUNTS_CSV),
      "\n",
      sep = ""
    )
  }
  
  print(blm_relevant_summary, width = 1200)
  print(blm_relevant_reason_counts, n = Inf)
  
  if (WRITE_BLM_RELEVANT_RDS) {
    
    saveRDS(pod_wr_blm_relevant, OUT_BLM_RELEVANT_RDS)
    
    cat(
      "\nSaved BLM-relevant POD/WR RDS:\n  ",
      normalizePath(OUT_BLM_RELEVANT_RDS),
      "\n",
      sep = ""
    )
  }
  
  if (WRITE_BLM_RELEVANT_GPKG) {
    
    sf::st_write(
      pod_wr_blm_relevant,
      OUT_BLM_RELEVANT_GPKG,
      layer = "swrcb_pod_wr_blm_relevant_wgs84",
      delete_dsn = TRUE,
      quiet = TRUE
    )
    
    cat(
      "\nSaved BLM-relevant POD/WR GeoPackage:\n  ",
      normalizePath(OUT_BLM_RELEVANT_GPKG),
      "\n",
      sep = ""
    )
  }
  
} else {
  
  message("Reading existing BLM-relevant POD/WR layer: ", OUT_BLM_RELEVANT_RDS)
  pod_wr_blm_relevant <- readRDS(OUT_BLM_RELEVANT_RDS)
}

# ==== 12. Preview useful fields ==============================================

cat("\nPreview of statewide joined fields useful for later popup logic:\n")

pod_wr_joined |>
  sf::st_drop_geometry() |>
  dplyr::select(
    dplyr::any_of(c(
      "wr_id",
      ".pod_join_key",
      "wr_match_count",
      "wr_application_number",
      "wr_face_value_amount",
      "wr_face_value_units",
      "wr_water_right_type",
      "wr_water_right_status",
      "wr_application_primary_owner",
      "wr_primary_owner_name",
      "primary_owner",
      "pod_status",
      "diversion_status",
      "source_name",
      "watershed",
      "county",
      "on_blm_managed"
    ))
  ) |>
  utils::head(20) |>
  print(width = 1200)

cat("\nPreview of BLM-relevant joined fields useful for later popup logic:\n")

pod_wr_blm_relevant |>
  sf::st_drop_geometry() |>
  dplyr::select(
    dplyr::any_of(c(
      "wr_id",
      ".pod_join_key",
      "wr_match_count",
      "wr_application_number",
      "wr_face_value_amount",
      "wr_face_value_units",
      "wr_water_right_type",
      "wr_water_right_status",
      "wr_application_primary_owner",
      "wr_primary_owner_name",
      "primary_owner",
      "pod_status",
      "diversion_status",
      "source_name",
      "watershed",
      "county",
      "on_blm_managed",
      "blm_name_match",
      "blm_include_reason",
      "blm_match_text"
    ))
  ) |>
  utils::head(20) |>
  print(width = 1200)


# ==== 13. Final summary =======================================================

cat("\nDone: SWRCB / CalWATRS POD + Water Rights preprocessing complete.\n")

cat("\nRaw source caches:\n")
cat("  ", normalizePath(POD_RDS_CACHE, mustWork = FALSE), "\n", sep = "")
cat("  ", normalizePath(WR_RDS_CACHE, mustWork = FALSE), "\n", sep = "")

cat("\nProcessed RDS outputs:\n")
cat("  ", normalizePath(OUT_JOINED_RDS, mustWork = FALSE), "\n", sep = "")
cat("  ", normalizePath(OUT_BLM_RELEVANT_RDS, mustWork = FALSE), "\n", sep = "")

cat("\nQA outputs:\n")
cat("  ", normalizePath(OUT_JOIN_SUMMARY_CSV, mustWork = FALSE), "\n", sep = "")
cat("  ", normalizePath(OUT_BLM_RELEVANT_SUMMARY_CSV, mustWork = FALSE), "\n", sep = "")

cat("\nMap integration status:\n")
cat("  SWRCB / CalWATRS POD integration is active.\n")
cat("  Current preprocessed map input:\n")
cat("    ", normalizePath(OUT_BLM_RELEVANT_RDS, mustWork = FALSE), "\n", sep = "")
cat("  Current map-ready cache target:\n")
cat("    04_processed_data/cache/latest/swrcb_pod_wr_blm_map.rds\n")
cat("  Final map layer name:\n")
cat("    SWRCB PODs relevant to BLM\n")