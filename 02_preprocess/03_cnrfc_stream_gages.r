# ==== 03_cnrfc_stream_gages.r ===============================================
##
## PURPOSE:
##   Parse CNRFC stream-gage station text data and create a clean sf point layer
##   for PortaTreasure2.
##
## INPUTS:
##   01_raw_data/cnrfc/gage_data_river.txt
##   01_raw_data/cnrfc/cnrfc_points_cabuffer.shp
##
## OUTPUTS:
##   04_processed_data/rds/CNRFC_allstreamgages_mostlyCaonly_wgs84.rds
##   04_processed_data/rds/CNRFC_allstreamgages_mostlyCaonly_wgs84_<timestamp>.rds
##   04_processed_data/gpkg/cnrfc_stream_gages.gpkg
##   04_processed_data/qa/cnrfc_stream_gages_qa_<timestamp>.csv
##
## NOTES:
##   - Source coordinates are assumed to be NAD83 lon/lat, EPSG:4269.
##   - Output is transformed to WGS84, EPSG:4326, for Leaflet.
##   - The California buffer is used to keep the "mostly California" CNRFC
##     network, including useful nearby stations in surrounding states.
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

infile_river <- file.path(DIR$raw, "cnrfc", "gage_data_river.txt")
infile_buffer <- file.path(DIR$raw, "cnrfc", "cnrfc_points_cabuffer.shp")

out_rds_latest <- file.path(
  DIR$rds,
  "CNRFC_allstreamgages_mostlyCaonly_wgs84.rds"
)

out_rds_timestamped <- file.path(
  DIR$rds,
  timestamped_name("CNRFC_allstreamgages_mostlyCaonly_wgs84", "rds")
)

out_gpkg <- file.path(
  DIR$gpkg,
  "cnrfc_stream_gages.gpkg"
)

out_qa <- file.path(
  DIR$qa,
  timestamped_name("cnrfc_stream_gages_qa", "csv")
)

# ==== 5. Check required files =================================================

if (!file.exists(infile_river)) {
  stop("Missing CNRFC stream-gage text file: ", infile_river)
}

if (!file.exists(infile_buffer)) {
  stop("Missing CNRFC clipping buffer shapefile: ", infile_buffer)
}

# ==== 6. Read raw CNRFC text file ============================================

message("Reading CNRFC stream-gage text file:")
message("  ", infile_river)

lines_raw <- readLines(infile_river, warn = FALSE)

## Remove fully blank lines, if any.
lines <- lines_raw[nzchar(trimws(lines_raw))]

message("Raw lines read: ", length(lines_raw))
message("Nonblank lines retained: ", length(lines))

# ==== 7. Parse fields from each line =========================================

## ---- 7.1 NWS ID -------------------------------------------------------------
## Old parser assumed a 5-character alphanumeric NWS ID at the beginning.
nwsid <- stringr::str_extract(lines, "^[A-Z0-9]{5}\\b")

## ---- 7.2 Data transmission code --------------------------------------------
## First character after the NWS ID.
line_after_nwsid <- stringr::str_trim(
  stringr::str_replace(lines, "^[A-Z0-9]{5}\\b\\s*", "")
)

datatransmission <- stringr::str_sub(line_after_nwsid, 1, 1)

## ---- 7.3 Quoted fields ------------------------------------------------------
## Stream-gage lines usually contain quoted station/channel fields.
## The first quoted field is treated as channel name.
## The second quoted field is treated as channel/location description.
## The third quoted field, when present, is treated as an optional nickname.
quoted_fields <- stringr::str_match_all(lines, '"([^"]*)"')

channel_name <- purrr::map_chr(quoted_fields, function(x) {
  if (nrow(x) >= 1 && ncol(x) >= 2) stringr::str_trim(x[1, 2]) else NA_character_
})

channel_desc <- purrr::map_chr(quoted_fields, function(x) {
  if (nrow(x) >= 2 && ncol(x) >= 2) stringr::str_trim(x[2, 2]) else NA_character_
})

nickname_raw <- purrr::map_chr(quoted_fields, function(x) {
  if (nrow(x) >= 3 && ncol(x) >= 2) stringr::str_trim(x[3, 2]) else NA_character_
})

## Build a stable display nickname.
## This mirrors the old "channel_name - channel_desc" approach but avoids
## awkward "NA - NA" labels.
nickname <- dplyr::case_when(
  !is.na(channel_name) & !is.na(channel_desc) ~ paste0(channel_name, " - ", channel_desc),
  !is.na(channel_name)                        ~ channel_name,
  !is.na(channel_desc)                        ~ channel_desc,
  !is.na(nickname_raw)                        ~ nickname_raw,
  TRUE                                        ~ nwsid
)

## ---- 7.4 Coordinates and elevation -----------------------------------------
## Capture:
##   latitude  = first decimal value
##   longitude = second decimal value
##   elevation = following integer
coord_matches <- stringr::str_match(
  lines,
  "(-?\\d+\\.\\d{5})\\s+(-?\\d+\\.\\d{5})\\s+(-?\\d{1,5})"
)

lat     <- as.numeric(coord_matches[, 2])
lon     <- as.numeric(coord_matches[, 3])
elev_ft <- as.numeric(coord_matches[, 4])

## ---- 7.5 Gage type ----------------------------------------------------------
## Old parser extracted two words after the coordinate/elevation block, such as:
##   River Other
##   Reservoir Forecast
##   Special Special
gage_type_raw <- stringr::str_match(
  lines,
  "(?:-?\\d+(?:\\.\\d{5})?\\s+){3}([A-Za-z]+\\s+[A-Za-z]+)"
)[, 2]

gage_type <- gage_type_raw |>
  stringr::str_trim() |>
  stringr::str_to_lower()

gage_class1 <- stringr::str_extract(gage_type, "^[a-z]+")
gage_class2 <- stringr::str_extract(gage_type, "(?<=\\s)[a-z]+$")

gage_type_final <- stringr::str_replace(gage_type, "\\s+", ", ")

## ---- 7.6 State from NWS ID --------------------------------------------------
## The old script mapped the fourth character of NWS ID to state.
state_code <- stringr::str_sub(nwsid, 4, 4)

state <- dplyr::case_when(
  state_code == "C" ~ "CA",
  state_code == "O" ~ "OR",
  state_code == "A" ~ "AZ",
  state_code == "N" ~ "NV",
  TRUE              ~ NA_character_
)

# ==== 8. Assemble parsed table ===============================================

df_river <- tibble::tibble(
  nwsid = nwsid,
  datatransmission = datatransmission,
  lat = lat,
  lon = lon,
  channel = channel_name,
  loc = channel_desc,
  nickname = nickname,
  elev_ft = elev_ft,
  gage_type = gage_type_final,
  gage_class1 = gage_class1,
  gage_class2 = gage_class2,
  state = state,
  source = "CNRFC stream gage"
)

# ==== 9. QA before spatial processing ========================================

qa_summary <- tibble::tibble(
  check = c(
    "raw_lines",
    "nonblank_lines",
    "parsed_rows",
    "missing_nwsid",
    "missing_lat",
    "missing_lon",
    "missing_elev_ft",
    "missing_gage_type",
    "missing_state"
  ),
  value = c(
    length(lines_raw),
    length(lines),
    nrow(df_river),
    sum(is.na(df_river$nwsid)),
    sum(is.na(df_river$lat)),
    sum(is.na(df_river$lon)),
    sum(is.na(df_river$elev_ft)),
    sum(is.na(df_river$gage_type)),
    sum(is.na(df_river$state))
  )
)

message("QA summary before clipping:")
print(qa_summary)

# ==== 10. Convert parsed table to sf =========================================

sf_river_wgs84 <- df_river |>
  dplyr::filter(!is.na(lat), !is.na(lon)) |>
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4269,
    remove = FALSE
  ) |>
  sf::st_transform(4326) |>
  clean_sf_for_leaflet()

message("Stream gage points with valid coordinates: ", nrow(sf_river_wgs84))

# ==== 11. Clip to CNRFC California buffer ====================================

message("Reading CNRFC clipping buffer:")
message("  ", infile_buffer)

cali_buffer <- sf::st_read(infile_buffer, quiet = TRUE) |>
  to_wgs84()

## Use st_intersects rather than strict st_within so boundary points are retained.
keep <- lengths(sf::st_intersects(sf_river_wgs84, cali_buffer)) > 0

sf_river_clipped <- sf_river_wgs84[keep, , drop = FALSE] |>
  clean_sf_for_leaflet()

message("Stream gage points after buffer clip: ", nrow(sf_river_clipped))

# ==== 12. Write outputs ======================================================

## ---- 12.1 RDS outputs -------------------------------------------------------
if (RUN$use_timestamped_outputs) {
  
  save_rds_cached(
    x = sf_river_clipped,
    timestamped_path = out_rds_timestamped,
    latest_path = out_rds_latest
  )
  
} else {
  
  saveRDS(sf_river_clipped, out_rds_latest)
  message("Saved latest RDS: ", out_rds_latest)
  
}

## ---- 12.2 GPKG output -------------------------------------------------------
if (WRITE_GPKG) {
  
  sf::st_write(
    sf_river_clipped,
    dsn = out_gpkg,
    layer = "cnrfc_stream_gages",
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
        nrow(sf_river_wgs84),
        nrow(sf_river_clipped)
      )
    )
  )
  
  readr::write_csv(qa_summary_final, out_qa)
  message("Saved QA CSV: ", out_qa)
}

# ==== 13. Final console summary ==============================================

message("\nDone: CNRFC stream gages preprocessing complete.")
message("Latest RDS:")
message("  ", out_rds_latest)
message("Rows saved: ", nrow(sf_river_clipped))
message("Columns:")
print(names(sf_river_clipped))