# ==== 08_cnrfc_basins.r ======================================================
##
## PURPOSE:
##   Prepare CNRFC basin polygons for PortaTreasure2.
##
## INPUT:
##   01_raw_data/cnrfc/CNRFCbasins_noHumboldt.shp
##
## OUTPUTS:
##   04_processed_data/rds/cnrfc_basins_wgs84.rds
##   04_processed_data/rds/cnrfc_basins_wgs84_<timestamp>.rds
##   04_processed_data/gpkg/cnrfc_basins.gpkg
##   04_processed_data/qa/cnrfc_basins_qa_<timestamp>.csv
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

out_rds_latest <- file.path(DIR$rds, "cnrfc_basins_wgs84.rds")

out_rds_timestamped <- file.path(
  DIR$rds,
  timestamped_name("cnrfc_basins_wgs84", "rds", RUN_TS)
)

out_gpkg <- file.path(DIR$gpkg, "cnrfc_basins.gpkg")

out_qa <- file.path(
  DIR$qa,
  paste0("cnrfc_basins_qa_", RUN_TS, ".csv")
)

# ==== 5. Check input =========================================================

if (!file.exists(SRC$cnrfc_basins)) {
  stop("Missing CNRFC basins shapefile: ", SRC$cnrfc_basins)
}

message("Reading CNRFC basins:")
message("  ", SRC$cnrfc_basins)

# ==== 6. Read and clean layer ================================================

cnrfc_basins_raw <- sf::st_read(
  SRC$cnrfc_basins,
  quiet = TRUE
)

cnrfc_basins <- cnrfc_basins_raw |>
  to_wgs84() |>
  clean_sf_for_leaflet() |>
  dplyr::mutate(
    source = "CNRFC basins"
  )

message("Rows read:  ", nrow(cnrfc_basins_raw))
message("Rows saved: ", nrow(cnrfc_basins))

message("Available CNRFC basin fields:")
print(names(cnrfc_basins))

# ==== 7. Save RDS outputs ====================================================

save_rds_cached(
  x = cnrfc_basins,
  timestamped_path = out_rds_timestamped,
  latest_path = out_rds_latest
)

# ==== 8. Save GPKG output ====================================================

if (WRITE_GPKG) {
  
  sf::st_write(
    cnrfc_basins,
    dsn = out_gpkg,
    layer = "cnrfc_basins",
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
      basename(SRC$cnrfc_basins),
      as.character(nrow(cnrfc_basins_raw)),
      as.character(nrow(cnrfc_basins)),
      as.character(sf::st_crs(cnrfc_basins)$epsg)
    ),
    run_timestamp = RUN_TS
  )
  
  readr::write_csv(qa, out_qa)
  
  message("Saved QA CSV: ", out_qa)
  print(qa)
}

# ==== 10. Final summary ======================================================

message("\nDone: CNRFC basin preprocessing complete.")
message("Rows saved: ", nrow(cnrfc_basins))
message("Latest RDS:")
message("  ", out_rds_latest)