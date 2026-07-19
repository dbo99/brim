# ==== 09_field_office_outer.r ===============================================
##
## PURPOSE:
##   Prepare BLM-California field-office outer boundary polygons for
##   PortaTreasure2.
##
## INPUT:
##   01_raw_data/blm/fo_outer.shp
##
## OUTPUTS:
##   04_processed_data/rds/field_office_outer_wgs84.rds
##   04_processed_data/rds/field_office_outer_wgs84_<timestamp>.rds
##   04_processed_data/gpkg/field_office_outer.gpkg
##   04_processed_data/qa/field_office_outer_qa_<timestamp>.csv
##

# ==== 1. Load configuration and helper functions =============================

source("00_config/config_paths.r")
source("00_config/config_run_flags.r")
source("00_config/config_source_files.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(tibble)
})

# ==== 3. User-facing switches ================================================

WRITE_GPKG <- TRUE
WRITE_QA   <- TRUE

RUN_TS <- make_timestamp()

# ==== 4. Define output paths =================================================

out_rds_latest <- file.path(DIR$rds, "field_office_outer_wgs84.rds")

out_rds_timestamped <- file.path(
  DIR$rds,
  timestamped_name("field_office_outer_wgs84", "rds", RUN_TS)
)

out_gpkg <- file.path(DIR$gpkg, "field_office_outer.gpkg")

out_qa <- file.path(
  DIR$qa,
  paste0("field_office_outer_qa_", RUN_TS, ".csv")
)

# ==== 5. Check input =========================================================

if (!file.exists(SRC$field_office_outer)) {
  stop("Missing field-office outer boundary shapefile: ", SRC$field_office_outer)
}

message("Reading field-office outer boundaries:")
message("  ", SRC$field_office_outer)

# ==== 6. Read and clean layer ================================================

fo_outer_raw <- sf::st_read(
  SRC$field_office_outer,
  quiet = TRUE
)

fo_outer <- fo_outer_raw |>
  to_wgs84() |>
  clean_sf_for_leaflet() |>
  dplyr::mutate(
    source = "BLM field-office outer boundary"
  )

message("Rows read:  ", nrow(fo_outer_raw))
message("Rows saved: ", nrow(fo_outer))

message("Available field-office outer fields:")
print(names(fo_outer))

# ==== 7. Save RDS outputs ====================================================

save_rds_cached(
  x = fo_outer,
  timestamped_path = out_rds_timestamped,
  latest_path = out_rds_latest
)

# ==== 8. Save GPKG output ====================================================

if (WRITE_GPKG) {
  
  sf::st_write(
    fo_outer,
    dsn = out_gpkg,
    layer = "field_office_outer",
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
  message("Saved GPKG: ", out_gpkg)
}

# ==== 9. Save QA output ======================================================

if (WRITE_QA) {
  
  qa <- tibble::tibble(
    check = c(
      "source_file",
      "raw_rows",
      "output_rows",
      "output_crs_epsg"
    ),
    value = c(
      basename(SRC$field_office_outer),
      as.character(nrow(fo_outer_raw)),
      as.character(nrow(fo_outer)),
      as.character(sf::st_crs(fo_outer)$epsg)
    ),
    run_timestamp = RUN_TS
  )
  
  readr::write_csv(qa, out_qa)
  
  message("Saved QA CSV: ", out_qa)
  print(qa)
}

# ==== 10. Final summary ======================================================

message("\nDone: field-office outer boundary preprocessing complete.")
message("Rows saved: ", nrow(fo_outer))
message("Latest RDS:")
message("  ", out_rds_latest)