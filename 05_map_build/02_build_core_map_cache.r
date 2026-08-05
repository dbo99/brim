# ==== 02_build_core_map_cache.r =============================================
##
## PURPOSE:
##   Build simplified, popup-enriched, map-ready cache layers for PortaTreasure2.
##
## INPUTS:
##   Full-resolution processed outputs from:
##     02_preprocess/01_blm_managed_and_held.r
##     02_preprocess/02_huc_gw_county_pct_blm.r
##     02_preprocess/03_cnrfc_stream_gages.r
##     02_preprocess/04_cnrfc_precip_gages.r
##
## OUTPUTS:
##   Timestamped cache:
##     04_processed_data/cache/enriched/*_<timestamp>.rds
##
##   Latest cache:
##     04_processed_data/cache/latest/*.rds
##
## WHY THIS EXISTS:
##   Full-resolution analytical layers are too heavy for a fast standalone
##   Leaflet map. This script creates display-optimized layers while preserving
##   the full analytical layers in 04_processed_data/rds/.
##

# ==== 1. Load configuration and helper functions =============================

source("00_config/config_paths.r")
source("00_config/config_run_flags.r")
source("00_config/config_source_files.r")
source("00_config/config_local_reference_interactions.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")
source("03_functions/bulletin118_data_helpers.r")
source("03_functions/popup_helpers.r")
source("03_functions/local_reference_interaction_helpers.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(purrr)
  library(readr)
  library(htmltools)
  library(rmapshaper)
})

pt_validate_local_reference_config()

# ==== 3. User-facing switches ================================================

BUILD_HUC10 <- TRUE
BUILD_HUC12 <- TRUE

WRITE_QA <- TRUE

## Use one timestamp for all cache files from this run.
RUN_TS <- make_timestamp()

# ==== 4. Simplification settings =============================================
##
## Smaller keep values = more simplification.
## These are map-display values only. They do not affect analytical RDS outputs.

SIMPLIFY_KEEP <- list(
  blm_core = 1.00,
  blm_diffs = 1.00,
  
  huc2 = 0.03,
  huc4 = 0.03,
  huc6 = 0.03,
  huc8 = 0.03,
  huc10 = 0.03,
  huc12 = 0.03,

  
  gw = 0.05,
  county = 0.05,
  calsim3 = 1.00,
  calsim3_nodes = 1.00,
  major_conveyance = 1.00,
  cnrfc_fnf_delta = 0.80,
  x2_km = 1.00,
  deltamapr_canals = 0.90,
  water_districts = 0.12,
  rwqcb_regions = 1.00,
  cnrfc_stream = 1.00,
  cnrfc_precip = 1.00
)

keep_for <- function(layer_id) {
  SIMPLIFY_KEEP[[layer_id]] %||% 0.05
}

## Local null-coalescing helper.
`%||%` <- function(a, b) {
  if (is.null(a) || is.na(a)) b else a
}

# ==== 5. Output helper =======================================================

save_map_cache <- function(x, base_name) {
  
  timestamped_path <- file.path(
    DIR$cache_enr,
    timestamped_name(base_name, "rds", timestamp = RUN_TS)
  )
  
  latest_path <- file.path(
    DIR$cache_last,
    paste0(base_name, ".rds")
  )
  
  save_rds_cached(
    x = x,
    timestamped_path = timestamped_path,
    latest_path = latest_path
  )
}

# ==== 6. Read full-resolution processed layers ===============================

message("Reading full-resolution processed layers...")

blm_core <- read_rds_checked(
  file.path(DIR$rds, "blm_managed_core_3310.rds"),
  "BLM managed core"
)

blm_diffs <- read_rds_checked(
  file.path(DIR$rds, "blm_held_vs_managed_diffs_3310.rds"),
  "BLM held/managed differences"
)

huc_all <- read_rds_checked(
  file.path(DIR$rds, "huc_all_full.rds"),
  "HUC all"
)

gw <- read_rds_checked(
  file.path(DIR$rds, "bull118gw_full.rds"),
  "Bulletin 118 groundwater basins"
)

## Join final 2019 SGMA priority attributes by exact basin/subbasin code.
## This fails unless the 515-to-515 contract is complete and row order,
## geometry, and every existing analytical field are unchanged.
gw_sgma_2019_crosswalk <- pt_read_bulletin118_sgma_crosswalk(
  SRC$bull118_sgma_2019_priority
)
gw <- pt_enrich_bulletin118_sgma_2019(
  gw = gw,
  crosswalk = gw_sgma_2019_crosswalk
)

county <- read_rds_checked(
  file.path(DIR$rds, "county_full.rds"),
  "Counties"
)

cnrfc_stream <- read_rds_checked(
  file.path(DIR$rds, "CNRFC_allstreamgages_mostlyCaonly_wgs84.rds"),
  "CNRFC stream gages"
)

cnrfc_precip <- read_rds_checked(
  file.path(DIR$rds, "CNRFC_allprecipstations_mostlyCaonly_wgs84.rds"),
  "CNRFC precip gages"
)

# ---- Optional CDEC reservoir-station index ---------------------------------
##
## This backend index is created by 02_preprocess/28_reservoir_station_index.r.
## Keep it optional so a clean BRIM cache rebuild still works before the CDEC
## reservoir foundation script has been run.

cdec_reservoir_station_index_path <- file.path(
  DIR$rds,
  "cdec_reservoir_station_index_wgs84.rds"
)

if (file.exists(cdec_reservoir_station_index_path)) {
  message("Reading CDEC reservoir station index: ", cdec_reservoir_station_index_path)
  cdec_reservoir_stations <- readRDS(cdec_reservoir_station_index_path)
} else {
  warning(
    "CDEC reservoir station index not found; CDEC Reservoir Stations layer will be empty. ",
    "Run source('02_preprocess/28_reservoir_station_index.r') to create it."
  )
  cdec_reservoir_stations <- sf::st_sf(
    cdec_id = character(),
    reservoir_name = character(),
    cdec_station_name = character(),
    has_hourly_reservoir_report = logical(),
    has_daily_reservoir_report = logical(),
    operator_agency = character(),
    county = character(),
    geometry = sf::st_sfc(crs = 4326)
  )
}

usgs_sw <- read_rds_checked(
  file.path(DIR$rds, "USGS_SW_final.rds"),
  "USGS streamgages"
)

usgs_gw <- read_rds_checked(
  file.path(DIR$rds, "USGS_GW_final.rds"),
  "USGS groundwater wells"
)


# ---- Optional BLM groundwater-well inventory layers ------------------------
##
## These small Local Monitoring Sites / Records layers are created by:
##   02_preprocess/18_blm_groundwater_well_inventory.r
##
## Keep them optional so a developer can still rebuild the older core cache
## before adding the raw BLM well CSVs.  When absent, write empty map-cache
## layers so final-map reads and Local-panel feature-count registration remain
## predictable.

pt_empty_blm_gw_well_inventory_sf <- function(source_key = character(), source_display = character()) {
  sf::st_sf(
    record_uid = character(),
    source_key = source_key[0],
    source_display = source_display[0],
    source_short = character(),
    layer_name = character(),
    source_record_id = character(),
    well_name_display = character(),
    hover_line1 = character(),
    hover_line2 = character(),
    longitude = numeric(),
    latitude = numeric(),
    well_present_key = character(),
    well_present_display = character(),
    well_monitored_key = character(),
    well_monitored_display = character(),
    depth_to_water_display = character(),
    depth_to_water_sort_ft = numeric(),
    elevation_display = character(),
    popup_html = character(),
    geometry = sf::st_sfc(crs = 4326)
  )
}

blm_gw_well_inventory_combined_path <- file.path(
  DIR$rds,
  "blm_gw_well_inventory_combined_wgs84.rds"
)

if (file.exists(blm_gw_well_inventory_combined_path)) {
  message("Reading BLM groundwater-well inventory: ", blm_gw_well_inventory_combined_path)
  blm_gw_well_inventory_combined <- readRDS(blm_gw_well_inventory_combined_path)
} else {
  warning(
    "BLM groundwater-well inventory RDS not found; NOC and 2025 Mojave-BLM ",
    "Local layers will be empty. Run source('02_preprocess/18_blm_groundwater_well_inventory.r') ",
    "to create it."
  )
  blm_gw_well_inventory_combined <- pt_empty_blm_gw_well_inventory_sf()
}

blm_offices <- read_rds_checked(
  file.path(DIR$rds, "blm_offices_wgs84.rds"),
  "BLM offices"
)

project_areas <- read_rds_checked(
  file.path(DIR$rds, "project_areas_wgs84.rds"),
  "Project areas"
)

cnrfc_basins <- read_rds_checked(
  file.path(DIR$rds, "cnrfc_basins_wgs84.rds"),
  "CNRFC basins"
)

field_office_outer <- read_rds_checked(
  file.path(DIR$rds, "field_office_outer_wgs84.rds"),
  "field-office outer boundaries"
)

calsim3_arcs <- read_rds_checked(
  file.path(DIR$rds, "calsim3_arcs_wgs84.rds"),
  "CalSim3 arcs"
)

calsim3_nodes <- read_rds_checked(
  file.path(DIR$rds, "calsim3_nodes_wgs84.rds"),
  "CalSim3 nodes"
)

major_conveyance <- read_rds_checked(
  file.path(DIR$rds, "major_conveyance_wgs84.rds"),
  "Major conveyance"
)

cnrfc_fnf_delta <- read_rds_checked(
  file.path(DIR$rds, "cnrfc_fnf_delta_wgs84.rds"),
  "CNRFC FNF Delta basins"
)

x2_km <- read_rds_checked(
  file.path(DIR$rds, "x2_km_wgs84.rds"),
  "CVP/SWP X2 km points"
)

deltamapr_canals <- read_rds_checked(
  file.path(DIR$rds, "deltamapr_canals_wgs84.rds"),
  "Deltamapr conveyance"
)

water_districts <- read_rds_checked(
  file.path(DIR$rds, "water_districts_wgs84.rds"),
  "water districts"
)

rwqcb_regions <- read_rds_checked(
  file.path(DIR$rds, "rwqcb_regions_wgs84.rds"),
  "RWQCB regions"
)

swrcb_pod_wr_blm <- read_rds_checked(
  file.path(DIR$rds, "swrcb_pod_wr_blm_relevant_wgs84.rds"),
  "SWRCB / CalWATRS POD water rights relevant to BLM"
)

springs <- read_rds_checked(
  file.path(DIR$rds, "springs_combined_wgs84.rds"),
  "combined springs"
)

# ---- 6a Read snow / soil climate station-reference layers ------------------
##
## RF039:
##   Keep the static SCAN Stations and Snow Pillows layers as stable station /
##   reference layers in the core map cache.
##
##   Do NOT automatically prefer the recent-observation sidecar RDS files here.
##   Those sidecars can make the static map appear to contain current data even
##   when the recent RDS files have not been refreshed for weeks.
##
##   Current/recent soil moisture, SWE, and related time-sensitive observations
##   should be exposed through Ops Live hosted GeoJSON feeds, not embedded in
##   the core cache.

scan_stations <- read_rds_checked(
  file.path(DIR$rds, "scan_stations_wgs84.rds"),
  "SCAN stations station-reference layer"
)

snow_pillows <- read_rds_checked(
  file.path(DIR$rds, "snow_pillows_wgs84.rds"),
  "snow pillows station-reference layer"
)

# ---- 6b. Read reference-layer manifest and processed layers -----------------
##
## These layers were preprocessed by:
##   02_preprocess/11_reference_layers_batch.r
##
## Each one is stored as its own RDS:
##   04_processed_data/rds/reference_<nickname>_wgs84.rds
##
## Here, we read them into a named list using the manifest nickname column.

reference_manifest <- readr::read_csv(
  SRC$reference_layers_manifest,
  show_col_types = FALSE
)

reference_layers_raw <- setNames(
  lapply(reference_manifest$nickname, function(nm) {
    read_rds_checked(
      file.path(DIR$rds, paste0("reference_", nm, "_wgs84.rds")),
      paste0("reference layer: ", nm)
    )
  }),
  reference_manifest$nickname
)

# ---- Sourced cache block: prepare core polygons ---------------------
##
## This large cache-building block was split out of the main orchestrator
## to keep 02_build_core_map_cache.r shorter and easier to maintain.
source("05_map_build/02_cache_blocks/01_prepare_core_polygons.r")

# ==== 8. Add popup fields and display-style fields ===========================
##
## PURPOSE:
##   Add popup_html and simple styling fields to cached map-ready layers.
##
## IMPORTANT:
##   Popup-builder functions live in 03_functions/popup_helpers.r.
##   They should not be defined inside this cache script.

message("Adding popup fields and display-style fields...")

# ---- Sourced cache block: HUC climate/recharge themes ---------------
##
## This large cache-building block was split out of the main orchestrator
## to keep 02_build_core_map_cache.r shorter and easier to maintain.
source("05_map_build/02_cache_blocks/02_cache_huc_climate_theme.r")

# ---- Sourced cache block: CNRFC and USGS point layers ---------------
##
## This large cache-building block was split out of the main orchestrator
## to keep 02_build_core_map_cache.r shorter and easier to maintain.
source("05_map_build/02_cache_blocks/03_cache_stream_usgs_points.r")

# ---- Sourced cache block: admin, water-rights, springs, stations, reference layers -
##
## This large cache-building block was split out of the main orchestrator
## to keep 02_build_core_map_cache.r shorter and easier to maintain.
source("05_map_build/02_cache_blocks/04_cache_admin_water_reference_layers.r")

# ---- Sourced cache block: final point-layer tweaks ------------------
##
## This large cache-building block was split out of the main orchestrator
## to keep 02_build_core_map_cache.r shorter and easier to maintain.
source("05_map_build/02_cache_blocks/05_cache_final_point_tweaks.r")

# ==== 9. Save map-ready cache outputs ========================================

message("Saving map-ready cache outputs...")

save_map_cache(blm_core_map, "blm_core_map")
save_map_cache(blm_diffs_map, "blm_diffs_map")
save_map_cache(huc_map, "huc_all_map")
save_map_cache(gw_map, "gw_bull118_map")
save_map_cache(county_map, "county_map")
save_map_cache(cnrfc_stream_map, "cnrfc_stream_map")
save_map_cache(cnrfc_precip_map, "cnrfc_precip_map")
save_map_cache(cdec_reservoir_stations_map, "cdec_reservoir_stations_map")
save_map_cache(usgs_sw_map, "usgs_streamgages_map")
save_map_cache(usgs_gw_map, "usgs_wells_map")
save_map_cache(blm_noc_drilled_wells_map, "blm_noc_drilled_wells_map")
save_map_cache(mojave_2025_gw_well_inventory_map, "mojave_2025_gw_well_inventory_map")
save_map_cache(swrcb_pod_wr_blm_map, "swrcb_pod_wr_blm_map")
save_map_cache(springs_map, "springs_map")
save_map_cache(scan_stations_map, "scan_stations_map")
save_map_cache(snow_pillows_map, "snow_pillows_map")
save_map_cache(blm_offices_map, "blm_offices_map")
save_map_cache(project_areas_map, "project_areas_map")
save_map_cache(cnrfc_basins_map, "cnrfc_basins_map")
save_map_cache(field_office_outer_map, "field_office_outer_map")
save_map_cache(calsim3_arcs_map, "calsim3_arcs_map")
save_map_cache(calsim3_nodes_map, "calsim3_nodes_map")
save_map_cache(reference_layers_map, "reference_layers_all_map")
save_map_cache(major_conveyance_map, "major_conveyance_map")
save_map_cache(cnrfc_fnf_delta_map, "cnrfc_fnf_delta_map")
save_map_cache(x2_km_map, "x2_km_map")
save_map_cache(deltamapr_canals_map, "deltamapr_canals_map")
save_map_cache(water_districts_map, "water_districts_map")
save_map_cache(rwqcb_regions_map, "rwqcb_regions_map")


# ==== 10. Save QA summary ====================================================
##
## PURPOSE:
##   Save a simple row-count inventory of each map-ready cache layer.
##
## NOTE:
##   HUC layers are stored together in one list object, so each HUC level gets
##   its own QA row but points to the same cache file: huc_all_map.rds.
##
##   Project areas may be empty for now. That is OK; the QA table will show
##   zero rows and confirm that the placeholder cache was created.

if (WRITE_QA) {
  
  qa <- dplyr::bind_rows(
    
    # ---- 10.1 Core BLM layers -----------------------------------------------
    tibble::tibble(
      layer_id = c(
        "blm_core",
        "blm_diffs"
      ),
      rows = c(
        nrow(blm_core_map),
        nrow(blm_diffs_map)
      ),
      cache_file = c(
        "blm_core_map.rds",
        "blm_diffs_map.rds"
      )
    ),
    
    # ---- 10.2 HUC layers -----------------------------------------------------
    tibble::tibble(
      layer_id = names(huc_map),
      rows = purrr::map_int(huc_map, nrow),
      cache_file = "huc_all_map.rds"
    ),
    

    # ---- 10.3 Existing polygon, line, and point layers -----------------------
    
    tibble::tibble(
      layer_id = c(
        "gw_bull118",
        "county",
        "cnrfc_stream",
        "cnrfc_precip",
        "cdec_reservoir_stations",
        "usgs_streamgages",
        "usgs_wells",
        "swrcb_pod_wr_blm",
        "springs",
        "scan_stations",
        "snow_pillows",
        "blm_offices",
        "project_areas",
        "cnrfc_basins",
        "field_office_outer",
        "calsim3_arcs",
        "calsim3_nodes",
        "major_conveyance",
        "cnrfc_fnf_delta",
        "x2_km",
        "deltamapr_canals",
        "water_districts",
        "rwqcb_regions"
      ),
      rows = c(
        nrow(gw_map),
        nrow(county_map),
        nrow(cnrfc_stream_map),
        nrow(cnrfc_precip_map),
        nrow(cdec_reservoir_stations_map),
        nrow(usgs_sw_map),
        nrow(usgs_gw_map),
        nrow(swrcb_pod_wr_blm_map),
        nrow(springs_map),
        nrow(scan_stations_map),
        nrow(snow_pillows_map),
        nrow(blm_offices_map),
        nrow(project_areas_map),
        nrow(cnrfc_basins_map),
        nrow(field_office_outer_map),
        nrow(calsim3_arcs_map),
        nrow(calsim3_nodes_map),
        nrow(major_conveyance_map),
        nrow(cnrfc_fnf_delta_map),
        nrow(x2_km_map),
        nrow(deltamapr_canals_map),
        nrow(water_districts_map),
        nrow(rwqcb_regions_map)
      ),
      cache_file = c(
        "gw_bull118_map.rds",
        "county_map.rds",
        "cnrfc_stream_map.rds",
        "cnrfc_precip_map.rds",
        "cdec_reservoir_stations_map.rds",
        "usgs_streamgages_map.rds",
        "usgs_wells_map.rds",
        "swrcb_pod_wr_blm_map.rds",
        "springs_map.rds",
        "scan_stations_map.rds",
        "snow_pillows_map.rds",
        "blm_offices_map.rds",
        "project_areas_map.rds",
        "cnrfc_basins_map.rds",
        "field_office_outer_map.rds",
        "calsim3_arcs_map.rds",
        "calsim3_nodes_map.rds",
        "major_conveyance_map.rds",
        "cnrfc_fnf_delta_map.rds",
        "x2_km_map.rds",
        "deltamapr_canals_map.rds",
        "water_districts_map.rds",
        "rwqcb_regions_map.rds"
      )
    ),
    # ---- 10.4 Manifest-driven reference layers ------------------------------
    tibble::tibble(
      layer_id = names(reference_layers_map),
      rows = purrr::map_int(reference_layers_map, nrow),
      cache_file = "reference_layers_all_map.rds"
    )
  ) |>
    dplyr::mutate(
      run_timestamp = RUN_TS
    )
  
  out_qa <- file.path(
    DIR$qa,
    paste0("core_map_cache_qa_", RUN_TS, ".csv")
  )
  
  readr::write_csv(qa, out_qa)
  
  message("Saved QA CSV: ", out_qa)
  print(qa)
}
# ==== 11. Final summary ======================================================

message("\nDone: core map-ready cache built.")
message("Latest cache folder:")
message("  ", DIR$cache_last)
message("Timestamped cache folder:")
message("  ", DIR$cache_enr)

message("\nLatest cache files:")
print(list.files(DIR$cache_last, pattern = "_map\\.rds$"))
