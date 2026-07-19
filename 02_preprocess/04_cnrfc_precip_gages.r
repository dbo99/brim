# ==== 04_cnrfc_precip_gages.r ===============================================
##
## PURPOSE:
##   Parse CNRFC precipitation-station text data and create a clean sf point
##   layer for PortaTreasure2.
##
## INPUTS:
##   01_raw_data/cnrfc/gage_data_precip.txt
##   01_raw_data/cnrfc/cnrfc_points_cabuffer.shp
##
## OUTPUTS:
##   04_processed_data/rds/CNRFC_allprecipstations_mostlyCaonly_wgs84.rds
##   04_processed_data/rds/CNRFC_allprecipstations_mostlyCaonly_wgs84_<timestamp>.rds
##   04_processed_data/gpkg/cnrfc_precip_gages.gpkg
##   04_processed_data/qa/cnrfc_precip_gages_qa_<timestamp>.csv
##
## NOTES:
##   - Source coordinates are assumed to be NAD83 lon/lat, EPSG:4269.
##   - Output is transformed to WGS84, EPSG:4326, for Leaflet.
##   - The California buffer keeps the CNRFC "mostly California" network,
##     including useful nearby stations in surrounding states.
##

# ==== 1. Load configuration and helper functions =============================

source("00_config/config_paths.r")
source("00_config/config_run_flags.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
})

# ==== 3. User-facing switches for this script ================================

WRITE_GPKG <- TRUE
WRITE_QA   <- TRUE

# ==== 4. Define input and output paths =======================================

infile_precip <- file.path(DIR$raw, "cnrfc", "gage_data_precip.txt")
infile_buffer <- file.path(DIR$raw, "cnrfc", "cnrfc_points_cabuffer.shp")

out_rds_latest <- file.path(
  DIR$rds,
  "CNRFC_allprecipstations_mostlyCaonly_wgs84.rds"
)

out_rds_timestamped <- file.path(
  DIR$rds,
  timestamped_name("CNRFC_allprecipstations_mostlyCaonly_wgs84", "rds")
)

out_gpkg <- file.path(
  DIR$gpkg,
  "cnrfc_precip_gages.gpkg"
)

out_qa <- file.path(
  DIR$qa,
  timestamped_name("cnrfc_precip_gages_qa", "csv")
)

# ==== 5. Check required files =================================================

if (!file.exists(infile_precip)) {
  stop("Missing CNRFC precip-gage text file: ", infile_precip)
}

if (!file.exists(infile_buffer)) {
  stop("Missing CNRFC clipping buffer shapefile: ", infile_buffer)
}

# ==== 6. Read raw CNRFC text file ============================================

message("Reading CNRFC precip-gage text file:")
message("  ", infile_precip)

lines_raw <- readLines(infile_precip, warn = FALSE)

## Remove fully blank lines, if any.
lines <- lines_raw[nzchar(trimws(lines_raw))]

message("Raw lines read: ", length(lines_raw))
message("Nonblank lines retained: ", length(lines))

# ==== 7. Parse fields from each line =========================================

## ---- 7.1 NWS ID -------------------------------------------------------------
## Precip IDs can be 2–5 alphanumeric characters in the legacy parser.
nwsid <- stringr::str_extract(lines, "^[A-Z0-9]{2,5}\\b")

## ---- 7.2 Data transmission code --------------------------------------------
## First character after the NWS ID.
line_after_nwsid <- stringr::str_trim(
  stringr::str_replace(lines, "^[A-Z0-9]{2,5}\\b\\s*", "")
)

datatransmission <- stringr::str_sub(line_after_nwsid, 1, 1)

## ---- 7.3 Station name -------------------------------------------------------
## Station name is the first quoted field.
station <- stringr::str_match(lines, '"([^"]*)"')[, 2] |>
  stringr::str_trim()

## ---- 7.4 Coordinates and elevation -----------------------------------------
## Capture:
##   latitude  = first decimal value
##   longitude = second decimal value
##   elevation = following integer, typically 1–5 digits
coord_matches <- stringr::str_match(
  lines,
  "(-?\\d+\\.\\d{5})\\s+(-?\\d+\\.\\d{5})\\s+(-?\\d{1,5})"
)

lat     <- as.numeric(coord_matches[, 2])
lon     <- as.numeric(coord_matches[, 3])
elev_ft <- as.numeric(coord_matches[, 4])

## ---- 7.5 State --------------------------------------------------------------
## State is usually the final two uppercase letters.
state <- stringr::str_extract(lines, "[A-Z]{2}$")

## Keep the western states / nearby areas useful for the CNRFC map.
valid_states <- c("CA", "NV", "OR", "AZ", "MX")

# ==== 8. Assemble parsed table ===============================================

df_precip <- tibble::tibble(
  nwsid = nwsid,
  datatransmission = datatransmission,
  station = station,
  lat = lat,
  lon = lon,
  elev_ft = elev_ft,
  state = state,
  source = "CNRFC precip station"
) |>
  dplyr::filter(state %in% valid_states)

# ==== 9. QA before spatial processing ========================================

qa_summary <- tibble::tibble(
  check = c(
    "raw_lines",
    "nonblank_lines",
    "parsed_rows_after_state_filter",
    "missing_nwsid",
    "missing_station",
    "missing_lat",
    "missing_lon",
    "missing_elev_ft",
    "missing_state"
  ),
  value = c(
    length(lines_raw),
    length(lines),
    nrow(df_precip),
    sum(is.na(df_precip$nwsid)),
    sum(is.na(df_precip$station)),
    sum(is.na(df_precip$lat)),
    sum(is.na(df_precip$lon)),
    sum(is.na(df_precip$elev_ft)),
    sum(is.na(df_precip$state))
  )
)

message("QA summary before clipping:")
print(qa_summary)

# ==== 10. Convert parsed table to sf =========================================

sf_precip_wgs84 <- df_precip |>
  dplyr::filter(!is.na(lat), !is.na(lon)) |>
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4269,
    remove = FALSE
  ) |>
  sf::st_transform(4326) |>
  clean_sf_for_leaflet()

message("Precip-gage points with valid coordinates: ", nrow(sf_precip_wgs84))

# ==== 11. Clip to CNRFC California buffer ====================================

message("Reading CNRFC clipping buffer:")
message("  ", infile_buffer)

cali_buffer <- sf::st_read(infile_buffer, quiet = TRUE) |>
  to_wgs84()

## Use st_intersects rather than strict st_within so boundary points are retained.
keep <- lengths(sf::st_intersects(sf_precip_wgs84, cali_buffer)) > 0

sf_precip_clipped <- sf_precip_wgs84[keep, , drop = FALSE] |>
  clean_sf_for_leaflet()

message("Precip-gage points after buffer clip: ", nrow(sf_precip_clipped))

# ==== 12. Write outputs ======================================================

## ---- 12.1 RDS outputs -------------------------------------------------------
if (RUN$use_timestamped_outputs) {
  
  save_rds_cached(
    x = sf_precip_clipped,
    timestamped_path = out_rds_timestamped,
    latest_path = out_rds_latest
  )
  
} else {
  
  saveRDS(sf_precip_clipped, out_rds_latest)
  message("Saved latest RDS: ", out_rds_latest)
  
}

## ---- 12.2 GPKG output -------------------------------------------------------
if (WRITE_GPKG) {
  
  sf::st_write(
    sf_precip_clipped,
    dsn = out_gpkg,
    layer = "cnrfc_precip_gages",
    delete_layer = TRUE,
    quiet = TRUE
  )
  
  message("Saved GPKG layer: ", out_gpkg)
}

## ---- 12.3 QA output ---------------------------------------------------------
if (WRITE_QA) {
  
  qa_summary_final <- dplyr::bind_rows(
    qa_summary,
    tibble::tibble(
      check = c(
        "valid_coordinate_points",
        "points_after_buffer_clip"
      ),
      value = c(
        nrow(sf_precip_wgs84),
        nrow(sf_precip_clipped)
      )
    )
  )
  
  readr::write_csv(qa_summary_final, out_qa)
  message("Saved QA CSV: ", out_qa)
}

# ==== 13. Final console summary ==============================================

message("\nDone: CNRFC precip gages preprocessing complete.")
message("Latest RDS:")
message("  ", out_rds_latest)
message("Rows saved: ", nrow(sf_precip_clipped))
message("Columns:")
print(names(sf_precip_clipped))