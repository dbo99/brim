# ==== 06_blm_offices.r =======================================================
##
## PURPOSE:
##   Create a clean BLM-California offices point layer for PortaTreasure2.
##
## INPUT:
##   01_raw_data/offices/blmcaoffices.csv
##
## EXPECTED FIELDS:
##   offce_name
##   offce_type
##   lon
##   lat
##
## OUTPUTS:
##   04_processed_data/rds/blm_offices_wgs84.rds
##   04_processed_data/rds/blm_offices_wgs84_<timestamp>.rds
##   04_processed_data/gpkg/blm_offices.gpkg
##   04_processed_data/qa/blm_offices_qa_<timestamp>.csv
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

out_rds_latest <- file.path(DIR$rds, "blm_offices_wgs84.rds")
out_rds_timestamped <- file.path(DIR$rds, timestamped_name("blm_offices_wgs84", "rds", RUN_TS))

out_gpkg <- file.path(DIR$gpkg, "blm_offices.gpkg")
out_qa <- file.path(DIR$qa, paste0("blm_offices_qa_", RUN_TS, ".csv"))

# ==== 5. Check input =========================================================

if (!file.exists(SRC$blm_offices)) {
  stop("Missing BLM offices CSV: ", SRC$blm_offices)
}

message("Reading BLM offices CSV:")
message("  ", SRC$blm_offices)

# ==== 6. Read and validate CSV ===============================================

offices_raw <- readr::read_csv(
  SRC$blm_offices,
  show_col_types = FALSE
)

required_fields <- c("offce_name", "offce_type", "lon", "lat")
missing_fields <- setdiff(required_fields, names(offices_raw))

if (length(missing_fields) > 0) {
  stop(
    "BLM offices CSV is missing required field(s): ",
    paste(missing_fields, collapse = ", "),
    "\nAvailable fields: ",
    paste(names(offices_raw), collapse = ", ")
  )
}

# ==== 7. Clean office table ==================================================

offices_clean <- offices_raw |>
  dplyr::mutate(
    offce_name = as.character(offce_name),
    offce_type = tolower(as.character(offce_type)),
    lon = suppressWarnings(as.numeric(lon)),
    lat = suppressWarnings(as.numeric(lat))
  ) |>
  dplyr::filter(!is.na(lon), !is.na(lat)) |>
  dplyr::mutate(
    source = "BLM office",
    office_type_label = dplyr::case_when(
      offce_type == "do"   ~ "District Office",
      offce_type == "fo"   ~ "Field Office",
      offce_type == "caso" ~ "California State Office",
      TRUE                 ~ offce_type
    )
  )

message("Office rows after coordinate filter: ", nrow(offices_clean))

# ==== 8. Convert to sf =======================================================

offices_sf <- offices_clean |>
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  ) |>
  clean_sf_for_leaflet()

# ==== 9. Save RDS outputs ====================================================

save_rds_cached(
  x = offices_sf,
  timestamped_path = out_rds_timestamped,
  latest_path = out_rds_latest
)

# ==== 10. Save GPKG output ===================================================

if (WRITE_GPKG) {
  
  sf::st_write(
    offices_sf,
    dsn = out_gpkg,
    layer = "blm_offices",
    delete_dsn = TRUE,
    quiet = TRUE
  )
  
  message("Saved GPKG: ", out_gpkg)
}

# ==== 11. Save QA output =====================================================

if (WRITE_QA) {
  
  qa <- tibble::tibble(
    check = c(
      "raw_rows",
      "rows_with_valid_coordinates",
      "output_rows",
      "district_or_state_office_rows",
      "field_office_rows"
    ),
    value = c(
      nrow(offices_raw),
      nrow(offices_clean),
      nrow(offices_sf),
      sum(offices_sf$offce_type %in% c("do", "caso"), na.rm = TRUE),
      sum(offices_sf$offce_type == "fo", na.rm = TRUE)
    ),
    run_timestamp = RUN_TS
  )
  
  readr::write_csv(qa, out_qa)
  message("Saved QA CSV: ", out_qa)
  print(qa)
}

# ==== 12. Final summary ======================================================

message("\nDone: BLM offices preprocessing complete.")
message("Rows saved: ", nrow(offices_sf))
message("Latest RDS:")
message("  ", out_rds_latest)