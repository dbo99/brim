# ==== qa_local_layer_cache_check.r ===========================================
##
## PURPOSE:
##   Reusable QA/QC pass for BRIM local/static map-cache layers.
##
## WHY THIS EXISTS:
##   Local layers are now edited frequently, and many problems are easiest to
##   catch immediately after a cache/map rebuild: missing cache files, empty
##   layers, wrong CRS, broken geometry, missing popup fields, duplicate point
##   IDs, or unexpected coordinate stacks.
##
## HOW TO RUN:
##   From the BRIM project root, either run this script directly or use:
##
##     source("run_build_map.r")
##     qa_local_layer_cache_check()
##
## OUTPUTS:
##   qa/local_layer_cache_check_YYYYMMDD_HHMMSS.csv
##   qa/local_layer_cache_check_duplicates_YYYYMMDD_HHMMSS.csv
##   qa/local_layer_cache_file_sizes_YYYYMMDD_HHMMSS.csv
##
## NOTES:
##   - This script is read-only.  It never modifies cache products.
##   - It uses the local-layer registry when available, but also has a fallback
##     inventory of high-value cache files so it can still run during debugging.
##   - "warning_count" is intentionally simple.  The CSV details explain which
##     checks triggered.

# ==== 1. Project and package setup ===========================================

if (!dir.exists("00_config") || !dir.exists("04_processed_data")) {
  stop(
    "Run this script from the BRIM project root. Expected folders like ",
    "00_config/ and 04_processed_data/ were not found."
  )
}

source("00_config/config_paths.r")
if (file.exists("00_config/config_local_layer_registry.r")) {
  source("00_config/config_local_layer_registry.r")
}

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
}

pt_qa_timestamp <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

pt_qa_chr <- function(x) {
  if (length(x) == 0) return(NA_character_)
  paste(unique(as.character(x)), collapse = ";")
}

pt_qa_blank_fraction <- function(x) {
  if (length(x) == 0) return(NA_real_)
  x <- as.character(x)
  mean(is.na(x) | trimws(x) == "")
}

pt_qa_safe_names <- function(x) {
  tryCatch(names(x), error = function(e) character(0))
}

pt_qa_file_size_mb <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    return(NA_real_)
  }
  round(file.info(path)$size / 1024^2, 3)
}

pt_qa_file_size_pct_250mb <- function(size_mb) {
  if (is.na(size_mb)) {
    return(NA_real_)
  }
  round(100 * size_mb / 250, 1)
}

# ==== 2. Layer inventory =====================================================

pt_qa_fallback_registry <- function() {
  data.frame(
    layer_id = c(
      "blm_core", "blm_diffs", "gw_bull118", "county",
      "huc2", "huc4", "huc6", "huc8", "huc10", "huc12",
      "cnrfc_stream", "cnrfc_precip", "cdec_reservoir_stations",
      "usgs_streamgages", "usgs_wells", "swrcb_pod_wr_blm",
      "springs", "scan_stations", "snow_pillows", "x2_km",
      "water_districts", "rwqcb_regions", "reference_layers"
    ),
    display_name = c(
      "BLM-CA Managed (core)", "BLM Held/Managed Differences",
      "GW – Bull. 118", "Counties",
      "HUC2", "HUC4", "HUC6", "HUC8", "HUC10", "HUC12",
      "CNRFC River & Reservoir", "CNRFC Precip Gages", "CDEC Reservoir Stations",
      "USGS Streamgages", "USGS Wells", "SWRCB POD/WR BLM records",
      "Springs", "SCAN Stations", "Snow Pillows", "CVP/SWP X2 km points",
      "Water Districts", "RWQCB Regions", "Reference Layers"
    ),
    cache_file = c(
      "blm_core_map.rds", "blm_diffs_map.rds", "gw_bull118_map.rds", "county_map.rds",
      rep("huc_all_map.rds", 6),
      "cnrfc_stream_map.rds", "cnrfc_precip_map.rds", "cdec_reservoir_stations_map.rds",
      "usgs_streamgages_map.rds", "usgs_wells_map.rds", "swrcb_pod_wr_blm_map.rds",
      "springs_map.rds", "scan_stations_map.rds", "snow_pillows_map.rds", "x2_km_map.rds",
      "water_districts_map.rds", "rwqcb_regions_map.rds", "reference_layers_all_map.rds"
    ),
    geometry_family = c(
      "polygon", "polygon", "polygon", "polygon",
      rep("polygon", 6),
      "point", "point", "point", "point", "point", "point",
      "point", "point", "point", "point",
      "polygon", "polygon", "mixed"
    ),
    stringsAsFactors = FALSE
  )
}

pt_qa_layer_registry <- function() {
  if (exists("LOCAL_LAYER_REGISTRY")) {
    out <- LOCAL_LAYER_REGISTRY
    out <- out[!is.na(out$cache_file) & out$cache_file != "", , drop = FALSE]
    out <- out[, intersect(
      c("layer_id", "display_name", "cache_file", "geometry_family"),
      names(out)
    ), drop = FALSE]
    if (!"geometry_family" %in% names(out)) out$geometry_family <- NA_character_
    return(out)
  }

  pt_qa_fallback_registry()
}

# Required fields are deliberately modest.  These are sanity checks, not a
# complete schema contract.  Add fields here as new local-layer work matures.
QA_REQUIRED_FIELDS <- list(
  blm_core = c("popup_html"),
  blm_diffs = c("popup_html"),
  gw_bull118 = c("popup_html"),
  county = c("popup_html"),
  cnrfc_stream = c("popup_html"),
  cnrfc_precip = c("popup_html"),
  cdec_reservoir_stations = c("popup_html"),
  usgs_streamgages = c("hover_text", "usgsswpop_site_no", "usgsswpop_common_data"),
  ## USGS Wells uses browser-template popups from compact gwpop_* fields.
  ## Do not require popup_html for this intentionally dense layer.
  usgs_wells = c("hover_text", "gwpop_site_no", "gwpop_common_data"),
  swrcb_wr_list_official = c("pt_swrcb_layer_id", "swrcbpop_water_right", "swrcbpop_screening_source"),
  swrcb_pod_spatial_matches = c("pt_swrcb_layer_id", "swrcbpop_water_right", "swrcbpop_screening_source"),
  swrcb_name_text_candidates = c("pt_swrcb_layer_id", "swrcbpop_water_right", "swrcbpop_screening_source"),
  swrcb_pod_wr_blm = c("pt_swrcb_layer_id", "swrcbpop_water_right", "swrcbpop_screening_source"),
  springs = c("popup_html"),
  scan_stations = c("popup_html"),
  snow_pillows = c("popup_html"),
  x2_km = c("popup_html"),
  water_districts = c("popup_html"),
  rwqcb_regions = c("popup_html")
)

QA_ID_FIELDS <- list(
  usgs_streamgages = c("site_no"),
  usgs_wells = c("site_no"),
  cdec_reservoir_stations = c("station_id", "staid", "cdec_id"),
  scan_stations = c("station_id", "site_id", "station_triplet"),
  snow_pillows = c("station_id", "site_id", "station_triplet"),
  springs = c("spring_id", "site_id", "name"),
  swrcb_wr_list_official = c("pt_swrcb_layer_id", "pod_id", "wr_pod_id", "objectid", "globalid"),
  swrcb_pod_spatial_matches = c("pt_swrcb_layer_id", "pod_id", "wr_pod_id", "objectid", "globalid"),
  swrcb_name_text_candidates = c("pt_swrcb_layer_id", "pod_id", "wr_pod_id", "objectid", "globalid")
)

# ==== 3. Cache object extraction =============================================

pt_qa_read_cache <- function(cache_file) {
  path <- file.path(DIR$cache_last, cache_file)
  if (!file.exists(path)) {
    return(list(ok = FALSE, path = path, object = NULL, error = "missing cache file"))
  }

  out <- tryCatch(
    readRDS(path),
    error = function(e) e
  )

  if (inherits(out, "error")) {
    return(list(ok = FALSE, path = path, object = NULL, error = out$message))
  }

  list(ok = TRUE, path = path, object = out, error = NA_character_)
}

pt_qa_extract_layer <- function(cache_obj, layer_id, display_name) {
  if (inherits(cache_obj, "sf")) {
    return(cache_obj)
  }

  if (is.list(cache_obj)) {
    nms <- names(cache_obj)
    candidates <- unique(c(layer_id, display_name))
    candidates <- candidates[!is.na(candidates) & candidates != ""]
    hit <- candidates[candidates %in% nms]
    if (length(hit) > 0) {
      return(cache_obj[[hit[1]]])
    }

    # HUC cache is normally a named list: huc2, huc4, ... huc12.
    huc_key <- tolower(layer_id)
    if (huc_key %in% nms) {
      return(cache_obj[[huc_key]])
    }

    # Reference-layer cache may be a list with source-specific names.  If no
    # exact match is available, keep the list-level object and report it as a
    # list cache rather than pretending to QA a specific sublayer.
    return(cache_obj)
  }

  cache_obj
}

# ==== 4. Per-layer QA helpers ================================================

pt_qa_bbox_string <- function(x) {
  bb <- tryCatch(sf::st_bbox(x), error = function(e) NULL)
  if (is.null(bb)) return(NA_character_)
  paste(
    paste(names(bb), round(as.numeric(bb), 6), sep = "="),
    collapse = ";"
  )
}

pt_qa_bbox_flags <- function(x) {
  bb <- tryCatch(sf::st_bbox(x), error = function(e) NULL)
  if (is.null(bb)) {
    return(list(western_us_ok = NA, contains_zero_zero = NA))
  }

  xmin <- as.numeric(bb[["xmin"]]); xmax <- as.numeric(bb[["xmax"]])
  ymin <- as.numeric(bb[["ymin"]]); ymax <- as.numeric(bb[["ymax"]])

  western_us_ok <- isTRUE(xmin > -130 && xmax < -100 && ymin > 20 && ymax < 55)
  contains_zero_zero <- isTRUE(xmin <= 0 && xmax >= 0 && ymin <= 0 && ymax >= 0)

  list(western_us_ok = western_us_ok, contains_zero_zero = contains_zero_zero)
}

pt_qa_geom_counts <- function(x) {
  if (!inherits(x, "sf")) {
    return(list(empty_count = NA_integer_, invalid_count = NA_integer_))
  }

  empty_count <- tryCatch(sum(sf::st_is_empty(x)), error = function(e) NA_integer_)
  invalid_count <- tryCatch(sum(!sf::st_is_valid(x)), error = function(e) NA_integer_)

  list(empty_count = empty_count, invalid_count = invalid_count)
}

pt_qa_point_duplicates <- function(x, layer_id) {
  empty <- data.frame(
    layer_id = character(0),
    duplicate_type = character(0),
    key = character(0),
    count = integer(0),
    stringsAsFactors = FALSE
  )

  if (!inherits(x, "sf") || nrow(x) == 0) return(empty)

  geom_types <- unique(as.character(sf::st_geometry_type(x, by_geometry = TRUE)))
  if (!all(geom_types %in% c("POINT", "MULTIPOINT"))) return(empty)

  coords <- tryCatch(sf::st_coordinates(x), error = function(e) NULL)
  if (is.null(coords) || nrow(coords) == 0 || !all(c("X", "Y") %in% colnames(coords))) {
    return(empty)
  }

  coord_key <- paste0(round(coords[, "X"], 6), ",", round(coords[, "Y"], 6))
  coord_tab <- sort(table(coord_key), decreasing = TRUE)
  coord_tab <- coord_tab[coord_tab > 1]

  out <- empty
  if (length(coord_tab) > 0) {
    out <- rbind(
      out,
      data.frame(
        layer_id = layer_id,
        duplicate_type = "same rounded coordinate",
        key = names(coord_tab),
        count = as.integer(coord_tab),
        stringsAsFactors = FALSE
      )
    )
  }

  id_fields <- QA_ID_FIELDS[[layer_id]] %||% character(0)
  id_field <- id_fields[id_fields %in% names(x)][1] %||% NA_character_
  if (!is.na(id_field)) {
    ids <- as.character(x[[id_field]])
    ids <- trimws(ids)
    ids <- ids[!is.na(ids) & ids != ""]
    id_tab <- sort(table(ids), decreasing = TRUE)
    id_tab <- id_tab[id_tab > 1]

    if (length(id_tab) > 0) {
      out <- rbind(
        out,
        data.frame(
          layer_id = layer_id,
          duplicate_type = paste0("same ", id_field),
          key = names(id_tab),
          count = as.integer(id_tab),
          stringsAsFactors = FALSE
        )
      )
    }
  }

  out
}

pt_qa_one_layer <- function(row, cache_read) {
  layer_id <- as.character(row$layer_id)
  display_name <- as.character(row$display_name %||% layer_id)
  cache_file <- as.character(row$cache_file)
  geometry_family <- as.character(row$geometry_family %||% NA_character_)

  base <- data.frame(
    layer_id = layer_id,
    display_name = display_name,
    cache_file = cache_file,
    cache_exists = isTRUE(cache_read$ok),
    cache_path = cache_read$path,
    cache_size_mb = pt_qa_file_size_mb(cache_read$path),
    cache_size_pct_of_250mb = pt_qa_file_size_pct_250mb(pt_qa_file_size_mb(cache_read$path)),
    read_error = cache_read$error %||% NA_character_,
    object_class = NA_character_,
    is_sf = FALSE,
    is_list_cache = FALSE,
    list_names = NA_character_,
    n_features = NA_integer_,
    geometry_family_expected = geometry_family,
    geometry_types = NA_character_,
    crs_epsg = NA_character_,
    bbox = NA_character_,
    western_us_bbox_ok = NA,
    contains_zero_zero = NA,
    empty_geometry_count = NA_integer_,
    invalid_geometry_count = NA_integer_,
    required_fields = NA_character_,
    missing_required_fields = NA_character_,
    required_fields_max_blank_fraction = NA_real_,
    duplicate_coord_groups = NA_integer_,
    max_coord_stack = NA_integer_,
    duplicate_id_groups = NA_integer_,
    warning_count = NA_integer_,
    warnings = NA_character_,
    stringsAsFactors = FALSE
  )

  if (!isTRUE(cache_read$ok)) {
    base$warning_count <- 1L
    base$warnings <- "cache missing or unreadable"
    return(list(summary = base, duplicates = pt_qa_point_duplicates(NULL, layer_id)))
  }

  x <- pt_qa_extract_layer(cache_read$object, layer_id, display_name)
  base$object_class <- paste(class(x), collapse = ";")
  base$is_list_cache <- is.list(x) && !inherits(x, "sf")
  base$list_names <- if (base$is_list_cache) pt_qa_chr(pt_qa_safe_names(x)) else NA_character_

  if (base$is_list_cache) {
    base$warning_count <- 0L
    base$warnings <- "list cache not expanded to a specific sf layer"
    return(list(summary = base, duplicates = pt_qa_point_duplicates(NULL, layer_id)))
  }

  base$is_sf <- inherits(x, "sf")
  base$n_features <- tryCatch(nrow(x), error = function(e) NA_integer_)

  if (!base$is_sf) {
    base$warning_count <- 1L
    base$warnings <- "object is not sf"
    return(list(summary = base, duplicates = pt_qa_point_duplicates(NULL, layer_id)))
  }

  geom_types <- tryCatch(
    unique(as.character(sf::st_geometry_type(x, by_geometry = TRUE))),
    error = function(e) NA_character_
  )
  base$geometry_types <- pt_qa_chr(geom_types)
  base$crs_epsg <- as.character(sf::st_crs(x)$epsg %||% NA_character_)
  base$bbox <- pt_qa_bbox_string(x)

  bbox_flags <- pt_qa_bbox_flags(x)
  base$western_us_bbox_ok <- bbox_flags$western_us_ok
  base$contains_zero_zero <- bbox_flags$contains_zero_zero

  geom_counts <- pt_qa_geom_counts(x)
  base$empty_geometry_count <- geom_counts$empty_count
  base$invalid_geometry_count <- geom_counts$invalid_count

  required_fields <- QA_REQUIRED_FIELDS[[layer_id]] %||% character(0)
  base$required_fields <- if (length(required_fields) > 0) paste(required_fields, collapse = ";") else NA_character_
  missing_fields <- setdiff(required_fields, names(x))
  base$missing_required_fields <- if (length(missing_fields) > 0) paste(missing_fields, collapse = ";") else NA_character_

  present_required <- intersect(required_fields, names(x))
  blank_fracs <- vapply(
    present_required,
    function(nm) pt_qa_blank_fraction(x[[nm]]),
    numeric(1)
  )
  base$required_fields_max_blank_fraction <- if (length(blank_fracs) > 0) max(blank_fracs) else NA_real_

  dup_df <- pt_qa_point_duplicates(x, layer_id)
  if (nrow(dup_df) > 0) {
    coord_dups <- dup_df[dup_df$duplicate_type == "same rounded coordinate", , drop = FALSE]
    id_dups <- dup_df[dup_df$duplicate_type != "same rounded coordinate", , drop = FALSE]
    base$duplicate_coord_groups <- nrow(coord_dups)
    base$max_coord_stack <- if (nrow(coord_dups) > 0) max(coord_dups$count) else 0L
    base$duplicate_id_groups <- nrow(id_dups)
  } else {
    base$duplicate_coord_groups <- 0L
    base$max_coord_stack <- 0L
    base$duplicate_id_groups <- 0L
  }

  warns <- character(0)
  if (is.na(base$n_features) || base$n_features == 0) warns <- c(warns, "zero features")
  if (!identical(base$crs_epsg, "4326")) warns <- c(warns, paste0("CRS is ", base$crs_epsg %||% "unknown", ", expected 4326"))
  if (isTRUE(base$contains_zero_zero)) warns <- c(warns, "bbox contains 0,0")
  if (!is.na(base$western_us_bbox_ok) && !isTRUE(base$western_us_bbox_ok)) warns <- c(warns, "bbox outside broad western-US sanity range")
  if (!is.na(base$empty_geometry_count) && base$empty_geometry_count > 0) warns <- c(warns, paste0(base$empty_geometry_count, " empty geometries"))
  if (!is.na(base$invalid_geometry_count) && base$invalid_geometry_count > 0) warns <- c(warns, paste0(base$invalid_geometry_count, " invalid geometries"))
  if (length(missing_fields) > 0) warns <- c(warns, paste0("missing required fields: ", paste(missing_fields, collapse = ", ")))
  if (!is.na(base$required_fields_max_blank_fraction) && base$required_fields_max_blank_fraction > 0.95) warns <- c(warns, "required field mostly blank")
  if (!is.na(base$max_coord_stack) && base$max_coord_stack >= 10) warns <- c(warns, paste0("large duplicate coordinate stack: ", base$max_coord_stack))
  if (!is.na(base$duplicate_id_groups) && base$duplicate_id_groups > 0) warns <- c(warns, paste0(base$duplicate_id_groups, " duplicate ID groups"))

  base$warning_count <- length(warns)
  base$warnings <- if (length(warns) > 0) paste(warns, collapse = " | ") else "OK"

  list(summary = base, duplicates = dup_df)
}

# ==== 5. Cache-file size summary =============================================

pt_qa_cache_file_size_summary <- function(registry, cache_by_file) {

  files <- unique(registry$cache_file)
  files <- files[!is.na(files) & files != ""]

  rows <- lapply(files, function(cache_file) {

    path <- cache_by_file[[cache_file]]$path %||% file.path(DIR$cache_last, cache_file)
    size_mb <- pt_qa_file_size_mb(path)
    rr <- registry[registry$cache_file == cache_file, , drop = FALSE]

    data.frame(
      cache_file = cache_file,
      cache_path = path,
      cache_exists = file.exists(path),
      cache_size_mb = size_mb,
      cache_size_pct_of_250mb = pt_qa_file_size_pct_250mb(size_mb),
      registry_row_count = nrow(rr),
      layer_ids = paste(unique(as.character(rr$layer_id)), collapse = ";"),
      display_names = paste(unique(as.character(rr$display_name)), collapse = ";"),
      stringsAsFactors = FALSE
    )
  })

  out <- dplyr::bind_rows(rows)
  out <- out[order(-out$cache_size_mb), , drop = FALSE]
  out
}

# ==== 6. Main QA runner =======================================================

pt_qa_local_layer_cache_check <- function() {
  ts <- pt_qa_timestamp()
  dir.create("qa", showWarnings = FALSE, recursive = TRUE)

  registry <- pt_qa_layer_registry()
  registry <- registry[!is.na(registry$cache_file) & registry$cache_file != "", , drop = FALSE]
  registry <- registry[!duplicated(registry[, c("layer_id", "cache_file")]), , drop = FALSE]

  cache_by_file <- lapply(unique(registry$cache_file), pt_qa_read_cache)
  names(cache_by_file) <- unique(registry$cache_file)

  results <- vector("list", nrow(registry))
  for (i in seq_len(nrow(registry))) {
    row <- registry[i, , drop = FALSE]
    results[[i]] <- pt_qa_one_layer(row, cache_by_file[[row$cache_file]])
  }

  summary_df <- dplyr::bind_rows(lapply(results, `[[`, "summary"))
  duplicate_df <- dplyr::bind_rows(lapply(results, `[[`, "duplicates"))
  size_df <- pt_qa_cache_file_size_summary(registry, cache_by_file)

  summary_path <- file.path("qa", paste0("local_layer_cache_check_", ts, ".csv"))
  duplicate_path <- file.path("qa", paste0("local_layer_cache_check_duplicates_", ts, ".csv"))
  size_path <- file.path("qa", paste0("local_layer_cache_file_sizes_", ts, ".csv"))

  utils::write.csv(summary_df, summary_path, row.names = FALSE, na = "")
  utils::write.csv(duplicate_df, duplicate_path, row.names = FALSE, na = "")
  utils::write.csv(size_df, size_path, row.names = FALSE, na = "")

  water_districts_size <- size_df[size_df$cache_file == "water_districts_map.rds", , drop = FALSE]

  message("Local layer cache QA complete.")
  message("  Summary:    ", summary_path)
  message("  Duplicates: ", duplicate_path)
  message("  File sizes: ", size_path)
  if (nrow(water_districts_size) == 1) {
    message(
      "  Water districts cache size: ",
      water_districts_size$cache_size_mb[1],
      " MB (",
      water_districts_size$cache_size_pct_of_250mb[1],
      "% of 250 MB)"
    )
  }
  message("  Layers checked: ", nrow(summary_df))
  message("  Layers with warnings: ", sum(summary_df$warning_count > 0, na.rm = TRUE))

  invisible(list(
    summary = summary_df,
    duplicates = duplicate_df,
    sizes = size_df,
    summary_path = summary_path,
    duplicate_path = duplicate_path,
    size_path = size_path
  ))
}

# Run when sourced by run_build_map.r/run_step or directly from RStudio.
qa_local_layer_cache_check_results <- pt_qa_local_layer_cache_check()
