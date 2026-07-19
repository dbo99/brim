# ==== 05_build_label_cache.r =================================================
##
## PURPOSE:
##   Build map-ready label layers for PortaTreasure2 from cached map layers.
##
## INPUTS:
##   04_processed_data/cache/latest/*.rds
##
## OUTPUTS:
##   04_processed_data/cache/enriched/labels_all_map_<timestamp>.rds
##   04_processed_data/cache/latest/labels_all_map.rds
##   04_processed_data/qa/labels_all_map_qa_<timestamp>.csv
##
## IMPORTANT:
##   Label fields are defined explicitly in:
##
##     00_config/config_labels.r
##
##   The script does not guess field names. If a label field is wrong for a
##   non-empty included layer, the script stops with a clear error.
##

# ==== 1. Load configuration and helper functions =============================

source("00_config/config_paths.r")
source("00_config/config_labels.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")
source("03_functions/label_helpers.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(purrr)
  library(readr)
})

# ==== 3. Run settings ========================================================

RUN_TS <- make_timestamp()
WRITE_QA <- TRUE

## If TRUE, a non-empty layer with a missing explicit label field will stop.
## This is preferred for reproducibility.
STRICT_LABEL_FIELDS <- TRUE

# ==== 4. Confirm label-field config exists ===================================

if (!exists("LABEL_FIELDS")) {
  stop(
    "LABEL_FIELDS was not found. Add the explicit LABEL_FIELDS list to ",
    "00_config/config_labels.r before running this script."
  )
}

# ==== 5. Helper functions ====================================================

get_explicit_label_field <- function(layer_id) {
  
  field <- LABEL_FIELDS[[layer_id]]
  
  if (is.null(field)) {
    return(NULL)
  }
  
  if (!is.character(field) || length(field) != 1) {
    stop(
      "LABEL_FIELDS$", layer_id,
      " must be either NULL or a single character field name."
    )
  }
  
  field
}

check_label_field <- function(x, layer_id, label_field) {
  
  if (is.null(label_field)) {
    message("Label field for ", layer_id, " is NULL; skipping labels.")
    return(FALSE)
  }
  
  if (!inherits(x, "sf")) {
    stop("Layer is not an sf object: ", layer_id)
  }
  
  if (nrow(x) == 0) {
    message("Layer is empty; labels skipped for ", layer_id, ".")
    return(FALSE)
  }
  
  if (!label_field %in% names(x)) {
    
    msg <- paste0(
      "Explicit label field not found for ", layer_id, ": ", label_field,
      "\nAvailable fields: ",
      paste(names(x), collapse = ", "),
      "\n\nFix by editing LABEL_FIELDS$", layer_id,
      " in 00_config/config_labels.r."
    )
    
    if (STRICT_LABEL_FIELDS) {
      stop(msg)
    } else {
      warning(msg)
      return(FALSE)
    }
  }
  
  TRUE
}

add_polygon_label_layer <- function(label_layers, layer_id, x) {
  
  label_field <- get_explicit_label_field(layer_id)
  
  if (!check_label_field(x, layer_id, label_field)) {
    label_layers[[layer_id]] <- pt_empty_label_sf()
    return(label_layers)
  }
  
  label_layers[[layer_id]] <- pt_make_polygon_labels(
    x = x,
    label_id = layer_id,
    label_field = label_field
  )
  
  label_layers
}

add_point_label_layer <- function(label_layers, layer_id, x) {
  
  label_field <- get_explicit_label_field(layer_id)
  
  if (!check_label_field(x, layer_id, label_field)) {
    label_layers[[layer_id]] <- pt_empty_label_sf()
    return(label_layers)
  }
  
  label_layers[[layer_id]] <- pt_make_point_labels(
    x = x,
    label_id = layer_id,
    label_field = label_field
  )
  
  label_layers
}

read_optional_cached_layer <- function(path, label) {
  
  if (!file.exists(path)) {
    message("Optional cached layer not found; skipping labels for ", label, ": ", path)
    return(NULL)
  }
  
  readRDS(path)
}

# ==== 6. Read cached map layers ==============================================
##
## These are map-ready cached layers, not raw/full-resolution preprocessing
## outputs. Labels should be generated from the same cached geometry used by
## the final map.

huc_all <- read_rds_checked(
  file.path(DIR$cache_last, "huc_all_map.rds"),
  "cached HUC layers"
)

gw <- read_rds_checked(
  file.path(DIR$cache_last, "gw_bull118_map.rds"),
  "cached groundwater basins"
)

county <- read_rds_checked(
  file.path(DIR$cache_last, "county_map.rds"),
  "cached counties"
)

project_areas <- read_rds_checked(
  file.path(DIR$cache_last, "project_areas_map.rds"),
  "cached project areas"
)

cnrfc_basins <- read_rds_checked(
  file.path(DIR$cache_last, "cnrfc_basins_map.rds"),
  "cached CNRFC basins"
)

field_office_outer <- read_rds_checked(
  file.path(DIR$cache_last, "field_office_outer_map.rds"),
  "cached field-office outer boundaries"
)

cnrfc_stream <- read_rds_checked(
  file.path(DIR$cache_last, "cnrfc_stream_map.rds"),
  "cached CNRFC stream gages"
)

cnrfc_precip <- read_rds_checked(
  file.path(DIR$cache_last, "cnrfc_precip_map.rds"),
  "cached CNRFC precip gages"
)

usgs_sw <- read_rds_checked(
  file.path(DIR$cache_last, "usgs_streamgages_map.rds"),
  "cached USGS streamgages"
)

reference_layers <- read_rds_checked(
  file.path(DIR$cache_last, "reference_layers_all_map.rds"),
  "cached reference layers"
)

major_conveyance <- read_rds_checked(
  file.path(DIR$cache_last, "major_conveyance_map.rds"),
  "cached major conveyance"
)

water_districts <- read_optional_cached_layer(
  file.path(DIR$cache_last, "water_districts_map.rds"),
  "water districts"
)

# ==== 7. Build HUC labels ====================================================
##
## HUC label fields are explicitly defined in LABEL_FIELDS:
##
##   LABEL_FIELDS$huc2
##   LABEL_FIELDS$huc4
##   ...
##   LABEL_FIELDS$huc12

label_layers <- list()

for (nm in c("huc2", "huc4", "huc6", "huc8", "huc10", "huc12")) {
  
  if (!isTRUE(LABEL_INCLUDE[[nm]])) {
    message("LABEL_INCLUDE$", nm, " is FALSE; skipping.")
    next
  }
  
  if (!nm %in% names(huc_all)) {
    message("Cached HUC layer not found: ", nm, "; skipping.")
    label_layers[[nm]] <- pt_empty_label_sf()
    next
  }
  
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = nm,
    x = huc_all[[nm]]
  )
}

# ==== 8. Build other polygon labels ==========================================

if (isTRUE(LABEL_INCLUDE$gw_bull118)) {
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = "gw_bull118",
    x = gw
  )
}

if (isTRUE(LABEL_INCLUDE$county)) {
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = "county",
    x = county
  )
}

if (isTRUE(LABEL_INCLUDE$project_areas)) {
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = "project_areas",
    x = project_areas
  )
}

if (isTRUE(LABEL_INCLUDE$cnrfc_basins)) {
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = "cnrfc_basins",
    x = cnrfc_basins
  )
}

if (isTRUE(LABEL_INCLUDE$field_office_outer)) {
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = "field_office_outer",
    x = field_office_outer
  )
}

# ---- 8A. Build labels for selected manifest-driven reference layers ---------
##
## Only these three are currently labeled:
##   - fedwilderness
##   - acec
##   - allotments

for (nm in c("fedwilderness", "acec", "allotments")) {
  
  if (!isTRUE(LABEL_INCLUDE[[nm]])) {
    next
  }
  
  if (!nm %in% names(reference_layers)) {
    message("Cached reference layer not found for labels: ", nm)
    label_layers[[nm]] <- pt_empty_label_sf()
    next
  }
  
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = nm,
    x = reference_layers[[nm]]
  )
}

# ---- 8B. Build major conveyance labels -------------------------------------
##
## Major conveyance is a line layer, but label anchors are generated using the
## same centroid-style polygon-label helper. This should place labels near the
## center of each dissolved conveyance feature.

if (isTRUE(LABEL_INCLUDE$major_conveyance)) {
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = "major_conveyance",
    x = major_conveyance
  )
}

# ---- 8C. Build water district labels ---------------------------------------
##
## Water districts can be difficult to identify by polygon hover/click alone
## because many districts overlap or contain smaller interior districts. A
## separate high-zoom label layer gives users a practical identification tool
## without requiring geometry surgery on the polygon layer.
##
## The layer remains toggleable in the Labels section of the main TOC. It is
## clustered with visually hidden cluster icons, so labels do not appear until
## the configured high zoom threshold is reached.

if (isTRUE(LABEL_INCLUDE$water_districts)) {
  
  if (inherits(water_districts, "sf") && nrow(water_districts) > 0) {
    label_layers <- add_polygon_label_layer(
      label_layers = label_layers,
      layer_id = "water_districts",
      x = water_districts
    )
  } else {
    message("Water district label source is missing or empty; labels skipped.")
    label_layers[["water_districts"]] <- pt_empty_label_sf()
  }
}

# ==== 9. Build point labels ==================================================
##
## USGS wells and BLM offices are intentionally excluded in config_labels.r.
## These point label layers should be high-zoom only because they can be dense.

if (isTRUE(LABEL_INCLUDE$cnrfc_stream)) {
  label_layers <- add_point_label_layer(
    label_layers = label_layers,
    layer_id = "cnrfc_stream",
    x = cnrfc_stream
  )
}

if (isTRUE(LABEL_INCLUDE$cnrfc_precip)) {
  label_layers <- add_point_label_layer(
    label_layers = label_layers,
    layer_id = "cnrfc_precip",
    x = cnrfc_precip
  )
}

if (isTRUE(LABEL_INCLUDE$usgs_streamgages)) {
  label_layers <- add_point_label_layer(
    label_layers = label_layers,
    layer_id = "usgs_streamgages",
    x = usgs_sw
  )
}

# ==== 10. Save label cache ===================================================

out_timestamped <- file.path(
  DIR$cache_enr,
  timestamped_name("labels_all_map", "rds", RUN_TS)
)

out_latest <- file.path(
  DIR$cache_last,
  "labels_all_map.rds"
)

save_rds_cached(
  x = label_layers,
  timestamped_path = out_timestamped,
  latest_path = out_latest
)

# ==== 11. Save QA summary ====================================================

if (WRITE_QA) {
  
  qa <- tibble::tibble(
    label_id = names(label_layers),
    explicit_label_field = purrr::map_chr(names(label_layers), function(id) {
      field <- LABEL_FIELDS[[id]]
      if (is.null(field)) return(NA_character_)
      as.character(field)
    }),
    rows = purrr::map_int(label_layers, nrow),
    parent_group = purrr::map_chr(label_layers, function(x) {
      if (nrow(x) == 0) return(NA_character_)
      as.character(x$parent_group[1])
    }),
    label_group = purrr::map_chr(label_layers, function(x) {
      if (nrow(x) == 0) return(NA_character_)
      as.character(x$label_group[1])
    }),
    min_zoom = purrr::map_dbl(label_layers, function(x) {
      if (nrow(x) == 0) return(NA_real_)
      as.numeric(x$min_zoom[1])
    }),
    max_zoom = purrr::map_dbl(label_layers, function(x) {
      if (nrow(x) == 0) return(NA_real_)
      as.numeric(x$max_zoom[1])
    }),
    run_timestamp = RUN_TS
  )
  
  out_qa <- file.path(
    DIR$qa,
    paste0("labels_all_map_qa_", RUN_TS, ".csv")
  )
  
  readr::write_csv(qa, out_qa)
  
  message("Saved label QA CSV: ", out_qa)
  print(qa)
}

# ==== 12. Final summary ======================================================

message("\nDone: label cache built.")
message("Latest label cache:")
message("  ", out_latest)

message("\nLabel rows by layer:")
print(purrr::map_int(label_layers, nrow))