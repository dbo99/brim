# ==== 07_project_areas_placeholder.r ========================================
##
## PURPOSE:
##   Create a project-area polygon layer for PortaTreasure2.
##
## CURRENT USE:
##   If a project-area shapefile exists at the explicit path declared in
##   config_source_files.r, this script reads it and prepares it for the map.
##
##   If no shapefile exists, this script creates an EMPTY sf polygon layer with
##   the expected schema. This lets the final map always include a Project
##   area(s) layer without breaking.
##
## EXPECTED FUTURE INPUT:
##   01_raw_data/project_areas/project_areas.shp
##
## OUTPUTS:
##   04_processed_data/rds/project_areas_wgs84.rds
##   04_processed_data/rds/project_areas_wgs84_<timestamp>.rds
##   04_processed_data/gpkg/project_areas.gpkg
##   04_processed_data/qa/project_areas_qa_<timestamp>.csv
##

# ==== 1. Load configuration and helper functions =============================

source("00_config/config_paths.r")
source("00_config/config_run_flags.r")
source("00_config/config_source_files.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
  library(tibble)
})

# ==== 3. User-facing switches ================================================

WRITE_GPKG <- TRUE
WRITE_QA   <- TRUE

RUN_TS <- make_timestamp()

# ==== 4. Define output paths =================================================

out_rds_latest <- file.path(DIR$rds, "project_areas_wgs84.rds")
out_rds_timestamped <- file.path(DIR$rds, timestamped_name("project_areas_wgs84", "rds", RUN_TS))

out_gpkg <- file.path(DIR$gpkg, "project_areas.gpkg")
out_qa <- file.path(DIR$qa, paste0("project_areas_qa_", RUN_TS, ".csv"))

# ==== 5. Helper: empty project sf ============================================

make_empty_project_sf <- function() {
  
  sf::st_sf(
    project_id = character(0),
    project_name = character(0),
    project_type = character(0),
    source = character(0),
    geometry = sf::st_sfc(crs = 4326)
  )
}

# ==== 6. Read project shapefile or create placeholder ========================

if (file.exists(SRC$project_areas)) {
  
  message("Reading project-area shapefile:")
  message("  ", SRC$project_areas)
  
  project_raw <- sf::st_read(SRC$project_areas, quiet = TRUE)
  
  ## Preserve original attributes for flexible future popups.
  project_sf <- project_raw |>
    to_wgs84()
  
  ## Add standard fields if they do not already exist.
  if (!"project_id" %in% names(project_sf)) {
    project_sf$project_id <- as.character(seq_len(nrow(project_sf)))
  }
  
  if (!"project_name" %in% names(project_sf)) {
    project_sf$project_name <- paste0("Project area ", seq_len(nrow(project_sf)))
  }
  
  if (!"project_type" %in% names(project_sf)) {
    project_sf$project_type <- "Project area"
  }
  
  if (!"source" %in% names(project_sf)) {
    project_sf$source <- basename(SRC$project_areas)
  }
  
  project_sf <- clean_sf_for_leaflet(project_sf)
  
  source_status <- "source_shapefile_read"
  
} else {
  
  message("No project-area shapefile found at:")
  message("  ", SRC$project_areas)
  message("Creating empty project-area placeholder layer.")
  
  project_sf <- make_empty_project_sf()
  
  source_status <- "empty_placeholder_created"
}

# ==== 7. Save RDS outputs ====================================================

save_rds_cached(
  x = project_sf,
  timestamped_path = out_rds_timestamped,
  latest_path = out_rds_latest
)

# ==== 8. Save GPKG output ====================================================

if (WRITE_GPKG) {
  
  ## st_write can be awkward with empty sf objects. Only write GPKG when rows exist.
  if (nrow(project_sf) > 0) {
    
    sf::st_write(
      project_sf,
      dsn = out_gpkg,
      layer = "project_areas",
      delete_dsn = TRUE,
      quiet = TRUE
    )
    
    message("Saved GPKG: ", out_gpkg)
    
  } else {
    
    message("Skipping GPKG write because project-area layer is empty.")
  }
}

# ==== 9. Save QA output ======================================================

if (WRITE_QA) {
  
  qa <- tibble::tibble(
    check = c(
      "source_status",
      "source_path",
      "output_rows",
      "output_crs_epsg"
    ),
    value = c(
      source_status,
      SRC$project_areas,
      as.character(nrow(project_sf)),
      as.character(sf::st_crs(project_sf)$epsg)
    ),
    run_timestamp = RUN_TS
  )
  
  readr::write_csv(qa, out_qa)
  message("Saved QA CSV: ", out_qa)
  print(qa)
}

# ==== 10. Final summary ======================================================

message("\nDone: project-area preprocessing complete.")
message("Rows saved: ", nrow(project_sf))
message("Latest RDS:")
message("  ", out_rds_latest)