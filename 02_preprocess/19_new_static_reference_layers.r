# ==== 19_new_static_reference_layers.r ===========================================
##
## PURPOSE:
##   Preprocess a small batch of static water / Delta reference shapefiles for
##   PortaTreasure2.
##
## INPUTS:
##   Declared explicitly in:
##     00_config/config_source_files.r
##
##   Current layers:
##     - CNRFC FNF Delta basins
##     - CVP/SWP X2 kilometer reference points
##     - Deltamapr canals
##     - Water districts
##
## OUTPUTS:
##   Full-resolution, Leaflet-ready WGS84 RDS files in:
##     04_processed_data/rds/
##
## DESIGN:
##   This script intentionally keeps only the fields needed for map display,
##   hovers, and popups. Geometry simplification is left to the core map-cache
##   builder so the full processed RDS files remain clean source-of-truth
##   display inputs.
## ============================================================================

# ==== 1. Load configuration and helpers ======================================

source("00_config/config_paths.r")
source("00_config/config_source_files.r")
source("03_functions/spatial_helpers.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

# ==== 3. Small helper functions =============================================

pt_read_shp_checked <- function(path, layer_label) {
  
  if (!file.exists(path)) {
    stop("Missing source shapefile for ", layer_label, ": ", path)
  }
  
  message("Reading ", layer_label, ": ", path)
  
  sf::st_read(path, quiet = TRUE)
}

pt_select_fields_case_insensitive <- function(x, expected_fields, layer_label) {
  
  if (!inherits(x, "sf")) {
    stop("Input is not an sf object for ", layer_label)
  }
  
  original_names <- names(x)
  original_no_geom <- setdiff(original_names, attr(x, "sf_column"))
  
  out <- x
  rename_pairs <- list()
  missing <- character(0)
  
  for (fld in expected_fields) {
    hit <- original_no_geom[tolower(original_no_geom) == tolower(fld)]
    
    if (length(hit) == 0) {
      missing <- c(missing, fld)
    } else {
      rename_pairs[[fld]] <- hit[1]
    }
  }
  
  if (length(missing) > 0) {
    stop(
      "Missing required field(s) for ", layer_label, ": ",
      paste(missing, collapse = ", "),
      "\nAvailable fields: ", paste(original_no_geom, collapse = ", ")
    )
  }
  
  ## Rename matched fields to the expected canonical names. This makes later
  ## cache and popup code stable even if a shapefile stores field names with
  ## different capitalization.
  for (canonical in names(rename_pairs)) {
    actual <- rename_pairs[[canonical]]
    if (!identical(canonical, actual)) {
      names(out)[names(out) == actual] <- canonical
    }
  }
  
  ## For sf objects, dplyr::select() retains the active geometry column even
  ## when it is not explicitly listed. This avoids assuming the geometry column
  ## is literally named "geometry".
  out |>
    dplyr::select(dplyr::all_of(expected_fields))
}

pt_write_processed_rds <- function(x, filename, layer_label) {
  
  out_path <- file.path(DIR$rds, filename)
  
  saveRDS(x, out_path)
  
  message("Saved ", layer_label, ": ", out_path)
  message("  rows: ", nrow(x))
  
  invisible(out_path)
}

pt_prepare_static_layer <- function(path, expected_fields, output_filename, layer_label) {
  
  x <- pt_read_shp_checked(path, layer_label) |>
    pt_select_fields_case_insensitive(
      expected_fields = expected_fields,
      layer_label = layer_label
    ) |>
    to_wgs84()
  
  pt_write_processed_rds(
    x = x,
    filename = output_filename,
    layer_label = layer_label
  )
  
  invisible(x)
}

# ==== 4. Build processed static layers =======================================

cnrfc_fnf_delta <- pt_prepare_static_layer(
  path = SRC$cnrfc_fnf_delta,
  expected_fields = c("River", "nws5id", "res"),
  output_filename = "cnrfc_fnf_delta_wgs84.rds",
  layer_label = "CNRFC FNF Delta basins"
)

x2_km <- pt_prepare_static_layer(
  path = SRC$x2_km,
  expected_fields = c("RKI"),
  output_filename = "x2_km_wgs84.rds",
  layer_label = "CVP/SWP X2 km points"
)

deltamapr_canals <- pt_prepare_static_layer(
  path = SRC$deltamapr_canals,
  expected_fields = c("Name", "Operator", "Conv_Type", "Conv_Sub"),
  output_filename = "deltamapr_canals_wgs84.rds",
  layer_label = "Deltamapr canals"
)

water_districts <- pt_prepare_static_layer(
  path = SRC$water_districts,
  expected_fields = c("AGENCYNAME"),
  output_filename = "water_districts_wgs84.rds",
  layer_label = "water districts"
)

# ==== 5. Final summary =======================================================

message("\nDone: static water / Delta layers preprocessed.")
message("Processed RDS folder:")
message("  ", DIR$rds)
