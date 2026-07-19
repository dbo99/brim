# ==== 12_major_conveyance.r ==================================================
##
## PURPOSE:
##   Prepare major California water-conveyance linework for PortaTreasure2.
##
## INPUT:
##   01_raw_data/conveyance/majorconveyance.shp
##
## MAIN PROCESSING:
##   - read raw polyline shapefile
##   - keep only fields needed for map display
##   - group CCWD and Local into one display category
##   - dissolve by Pname + Operator
##   - reproject to EPSG:4326 for Leaflet
##   - save latest and timestamped RDS outputs
##
## OUTPUTS:
##   04_processed_data/rds/major_conveyance_wgs84.rds
##   04_processed_data/rds/major_conveyance_wgs84_<timestamp>.rds
##   04_processed_data/qa/major_conveyance_qa_<timestamp>.csv
##

# ==== 1. Load configuration and helper functions =============================

source("00_config/config_paths.r")
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

# ==== 3. Run settings ========================================================

RUN_TS <- make_timestamp()
WRITE_QA <- TRUE

# ==== 4. Output paths ========================================================

out_rds_latest <- file.path(
  DIR$rds,
  "major_conveyance_wgs84.rds"
)

out_rds_timestamped <- file.path(
  DIR$rds,
  timestamped_name("major_conveyance_wgs84", "rds", RUN_TS)
)

out_qa <- file.path(
  DIR$qa,
  paste0("major_conveyance_qa_", RUN_TS, ".csv")
)

# ==== 5. Check input =========================================================

if (!file.exists(SRC$major_conveyance)) {
  stop("Missing major conveyance shapefile: ", SRC$major_conveyance)
}

message("Reading major conveyance shapefile:")
message("  ", SRC$major_conveyance)

# ==== 6. Read raw layer ======================================================

raw <- sf::st_read(
  SRC$major_conveyance,
  quiet = TRUE
)

message("Raw rows: ", nrow(raw))
message("Raw fields:")
print(names(raw))

required_fields <- c("Pname", "Operator")

missing_fields <- setdiff(required_fields, names(raw))

if (length(missing_fields) > 0) {
  stop(
    "Major conveyance layer is missing required field(s): ",
    paste(missing_fields, collapse = ", "),
    "\nAvailable fields: ",
    paste(names(raw), collapse = ", ")
  )
}

message("Raw Operator counts:")
print(table(raw$Operator, useNA = "always"))

# ==== 7. Slim attributes and classify operators ==============================
##
## Operator categories requested:
##   CCWD + Local -> Local / CCWD
##   Federal      -> Federal
##   State        -> State
##   Fed/State    -> Fed/State
##
## Retain the original Operator field because it is useful in popups.

conveyance_slim <- raw |>
  dplyr::select(
    Pname,
    Operator,
    geometry
  ) |>
  dplyr::mutate(
    operator_group = dplyr::case_when(
      Operator %in% c("CCWD", "Local") ~ "Local / CCWD",
      Operator == "Federal"           ~ "Federal",
      Operator == "State"             ~ "State",
      Operator == "Fed/State"         ~ "Fed/State",
      TRUE                            ~ "Other / unknown"
    )
  )

# ==== 8. Dissolve by Pname + Operator ========================================
##
## IMPORTANT:
##   Dissolve by both Pname and Operator, not Pname alone, because some
##   conveyance systems can have separate segments with different operators.

message("Dissolving conveyance by Pname + Operator...")

major_conveyance <- conveyance_slim |>
  dplyr::group_by(
    Pname,
    Operator,
    operator_group
  ) |>
  dplyr::summarise(
    segment_count = dplyr::n(),
    .groups = "drop"
  ) |>
  to_wgs84() |>
  clean_sf_for_leaflet() |>
  dplyr::mutate(
    source = "Major conveyance"
  )

message("Rows after dissolve: ", nrow(major_conveyance))
message("Dissolved Operator counts:")
print(table(major_conveyance$Operator, useNA = "always"))

message("Dissolved operator_group counts:")
print(table(major_conveyance$operator_group, useNA = "always"))

# ==== 9. Save RDS outputs ====================================================

save_rds_cached(
  x = major_conveyance,
  timestamped_path = out_rds_timestamped,
  latest_path = out_rds_latest
)

# ==== 10. Save QA summary ====================================================

if (WRITE_QA) {
  
  qa <- tibble::tibble(
    check = c(
      "source_file",
      "raw_rows",
      "dissolved_rows",
      "unique_pname",
      "output_crs_epsg"
    ),
    value = c(
      basename(SRC$major_conveyance),
      as.character(nrow(raw)),
      as.character(nrow(major_conveyance)),
      as.character(dplyr::n_distinct(major_conveyance$Pname)),
      as.character(sf::st_crs(major_conveyance)$epsg)
    ),
    run_timestamp = RUN_TS
  )
  
  readr::write_csv(qa, out_qa)
  
  message("Saved QA CSV: ", out_qa)
  print(qa)
}

# ==== 11. Final summary ======================================================

message("\nDone: major conveyance preprocessing complete.")
message("Latest RDS:")
message("  ", out_rds_latest)
message("Rows saved: ", nrow(major_conveyance))
message("Fields saved:")
print(names(major_conveyance))