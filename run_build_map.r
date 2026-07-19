# ==== run_build_map.r ========================================================
##
## PURPOSE:
##   Main PortaTreasure2 click-runner.
##
## DESIGN GOAL:
##   Minimize typing and minimize TRUE/FALSE switching.
##
##   Each function below represents one logical job, such as:
##     - build final HTML only
##     - rebuild core cache + final HTML
##     - rebuild labels + final HTML
##     - refresh one raw-data layer, then rebuild whatever depends on it
##
## HOW TO USE:
##   1. Open this file in RStudio.
##   2. Put your cursor on ONE function call in Section 7.
##   3. Press Cmd/Ctrl + Enter.
##
## EXAMPLES:
##   Daily build from existing cache:
##     build_final_map_only()
##
##   After popup/symbology edits:
##     rebuild_core_cache_and_map()
##
##   After label edits:
##     rebuild_label_cache_and_map()
##
##   After editing CalSim3 raw-data preprocessing:
##     refresh_calsim3_arcs_and_map()
##
## IMPORTANT:
##   Run this file from the PortaTreasure2 project root.

# ==== 1. Project sanity check ================================================

if (!dir.exists("00_config") || !dir.exists("05_map_build")) {
  stop(
    "Run this script from the PortaTreasure2 project root.\n",
    "Expected folders like 00_config/ and 05_map_build/ were not found."
  )
}

# ==== 2. Helper: run one script with readable console logging ================

format_elapsed <- function(seconds) {
  seconds <- as.numeric(seconds)
  if (is.na(seconds)) return("elapsed time unknown")

  if (seconds < 60) {
    return(paste0(round(seconds, 1), " sec"))
  }

  mins <- floor(seconds / 60)
  secs <- round(seconds %% 60)

  if (mins < 60) {
    return(paste0(mins, " min ", secs, " sec"))
  }

  hrs <- floor(mins / 60)
  mins <- mins %% 60
  paste0(hrs, " hr ", mins, " min ", secs, " sec")
}

run_step <- function(path, label = NULL) {
  
  if (!file.exists(path)) {
    stop("Missing script: ", path)
  }
  
  if (is.null(label)) {
    label <- basename(path)
  }
  
  message("\n============================================================")
  message("[RUN ] ", label)
  message("       ", path)
  message("============================================================")

  step_start <- Sys.time()
  source(path, local = FALSE)
  step_elapsed <- as.numeric(difftime(Sys.time(), step_start, units = "secs"))

  message("[DONE] ", label, " (elapsed: ", format_elapsed(step_elapsed), ")")
}

run_step_clean <- function(path, label = NULL) {
  ## Springs BLM-distance refreshes can create large sf objects.  The normal
  ## run_step() helper sources scripts into the global workspace, which is
  ## convenient for interactive debugging but can leave large geometries in
  ## memory immediately before the final self-contained HTML save.  This clean
  ## variant runs a standalone script in a temporary environment, then removes
  ## that environment and calls gc() before the next step.  The temporary
  ## environment inherits from the global environment so normal project helpers
  ## remain available, while objects created by the script do not persist.

  if (!file.exists(path)) {
    stop("Missing script: ", path)
  }

  if (is.null(label)) {
    label <- basename(path)
  }

  message("\n============================================================")
  message("[RUN ] ", label)
  message("       ", path)
  message("       mode: clean temporary environment")
  message("============================================================")

  step_start <- Sys.time()
  step_env <- new.env(parent = globalenv())
  on.exit({
    # If the step reaches the explicit cleanup below, step_env will already be
    # removed by the time on.exit() runs.  Guard the cleanup so routine successful
    # runs do not emit a confusing "object 'step_env' not found" warning.
    if (exists("step_env", inherits = FALSE)) {
      rm(step_env)
    }
    invisible(gc(verbose = FALSE))
  }, add = TRUE)

  source(path, local = step_env)
  step_elapsed <- as.numeric(difftime(Sys.time(), step_start, units = "secs"))

  rm(step_env)
  invisible(gc(verbose = FALSE))

  message("[DONE] ", label, " (elapsed: ", format_elapsed(step_elapsed), ")")
}

# ==== 3. Central script registry =============================================

SCRIPT_PATHS <- list(
  
  # ---- Preprocessors --------------------------------------------------------
  preprocess_blm_main           = "02_preprocess/01_blm_managed_and_held.r",
  preprocess_huc_gw_county      = "02_preprocess/02_huc_gw_county_pct_blm.r",
  preprocess_cnrfc_stream       = "02_preprocess/03_cnrfc_stream_gages.r",
  preprocess_cnrfc_precip       = "02_preprocess/04_cnrfc_precip_gages.r",

  ## Local dense point-layer distance preprocessors.  These are intentionally
  ## separate from normal map builds because BLM managed-lands geometry changes
  ## over time and point-to-BLM distances are spatially expensive.
  preprocess_cnrfc_local_blm_distance = "02_preprocess/59_update_cnrfc_local_blm_distance_fields.R",
  preprocess_usgs_gw_local_blm_distance = "02_preprocess/60_update_usgs_gw_local_blm_distance_fields.R",
  preprocess_swrcb_pod_blm_distance = "02_preprocess/61_update_swrcb_pod_blm_distance_fields.R",
  preprocess_springs_blm_distance = "02_preprocess/62_update_springs_blm_distance_fields.R",

  ## BLM groundwater-well inventory source normalization and current-BLM
  ## distance sidecar for the NOC BLM-drilled wells and 2025 Mojave-BLM field
  ## check layers.  Keep both separate from normal final HTML builds.
  preprocess_blm_gw_well_inventory = "02_preprocess/18_blm_groundwater_well_inventory.r",
  preprocess_blm_gw_well_inventory_blm_distance = "02_preprocess/63_update_blm_gw_well_inventory_blm_distance_fields.R",

  ## Springs source normalization.  Use this when the NHD/survey source data are
  ## refreshed or when a future inventory, such as Amargosa SOB springs, is
  ## assimilated into the combined Springs layer.
  preprocess_springs            = "02_preprocess/17_springs.r",
  
  ## Update this line if your USGS preprocessor filename differs.
  preprocess_usgs               = "02_preprocess/05_usgs_streamgages_wells.r",
  
  preprocess_blm_offices        = "02_preprocess/06_blm_offices.r",
  preprocess_project_areas      = "02_preprocess/07_project_areas_placeholder.r",
  preprocess_cnrfc_basins       = "02_preprocess/08_cnrfc_basins.r",
  preprocess_field_office_outer = "02_preprocess/09_field_office_outer.r",
  preprocess_calsim3_arcs       = "02_preprocess/10_calsim3_arcs.r",
  
  ## CNRFC product-intelligence preprocessors.
  preprocess_cnrfc_feature_inventory = "02_preprocess/51_build_cnrfc_feature_inventory.R",
  preprocess_cnrfc_basin_product_availability = "02_preprocess/52_build_cnrfc_basin_product_availability.R",
  preprocess_cnrfc_forecast_point_product_availability = "02_preprocess/53_build_cnrfc_forecast_point_product_availability.R",
  preprocess_cnrfc_precip_weather_link_availability = "02_preprocess/54_build_cnrfc_precip_weather_link_availability.R",
  
  ## Static water / Delta layers added after the original core build.
  preprocess_new_static_water_layers = "02_preprocess/19_new_static_reference_layers.r",
  preprocess_rwqcb_regions           = "02_preprocess/27_rwqcb_regions.r",
  preprocess_reservoir_station_index = "02_preprocess/28_reservoir_station_index.r",
  export_scan_live_inputs          = "02_preprocess/33_export_scan_live_inputs.r",
  export_snow_pillow_live_inputs   = "02_preprocess/38_export_snow_pillow_live_inputs.r",
  build_snow_pillow_history_context = "02_preprocess/39_build_snow_pillow_history_context.r",
  
  # ---- Cache builders -------------------------------------------------------
  build_core_cache              = "05_map_build/02_build_core_map_cache.r",
  build_label_cache             = "05_map_build/05_build_label_cache.r",
  
  # ---- Final HTML builder ---------------------------------------------------
  build_final_map               = "05_map_build/04_build_portatreasure2_core_map.r",

  # ---- Development sandboxes ------------------------------------------------
  build_snow_pillow_popup_sandbox = "05_map_build/dev_sandbox/build_snow_pillow_live_popup_sandbox.r",

  # ---- QA/QC ----------------------------------------------------------------
  qa_local_layer_cache_check = "qa/qa_local_layer_cache_check.r",
  qa_local_layer_payload_size_check = "qa/qa_local_layer_payload_size_check.r",
  qa_calsim3_network_audit = "qa/qa_calsim3_network_audit.r",
  qa_cnrfc_product_intelligence_audit = "qa/qa_cnrfc_product_intelligence_audit.r",
  qa_cnrfc_weather_gov_wrh_audit = "qa/qa_cnrfc_weather_gov_wrh_audit.r",
  qa_cnrfc_basin_product_availability_audit = "qa/qa_cnrfc_basin_product_availability_audit.r",
  qa_cnrfc_water_resources_update_diagnostic = "qa/qa_cnrfc_water_resources_update_diagnostic.r",
  qa_cnrfc_water_resources_update_kml_audit = "qa/qa_cnrfc_water_resources_update_kml_audit.r",
  qa_cnrfc_active_forecast_point_audit = "qa/qa_cnrfc_active_forecast_point_audit.r",
  qa_cnrfc_active_forecast_source_diagnostic = "qa/qa_cnrfc_active_forecast_source_diagnostic.r",
  qa_cnrfc_active_forecast_xml_parse_audit = "qa/qa_cnrfc_active_forecast_xml_parse_audit.r"
)

# ==== 4. Core reusable workflow functions ====================================
##
## These are the most common grouped jobs.
## Each one is intended to be run with a single click.

# ---- 4.1 Build final HTML only ----------------------------------------------

build_final_map_only <- function() {
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML only"
  )
}

# ---- 4.2 Rebuild core cache, then final HTML --------------------------------

rebuild_core_cache_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 4.3 Rebuild labels, then final HTML ------------------------------------

rebuild_label_cache_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$build_label_cache,
    "Rebuild label cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 4.4 Rebuild core cache, labels, then final HTML ------------------------

rebuild_everything_from_cache_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_label_cache,
    "Rebuild label cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ==== 5. Raw-data refresh workflow functions =================================
##
## These functions rerun one specific preprocessor and then the downstream steps
## that usually matter for that layer.

# ---- 5.1 Refresh CNRFC stream gages, then map -------------------------------

refresh_cnrfc_stream_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_cnrfc_stream,
    "Preprocess CNRFC stream gages"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 5.2 Refresh CNRFC precip gages, then map -------------------------------

refresh_cnrfc_precip_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_cnrfc_precip,
    "Preprocess CNRFC precip gages"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 5.2A Refresh CNRFC Local BLM distances, then map -----------------------
##
## Run this after refreshing BLM managed lands or CNRFC Local catalog points.
## It precomputes CNRFC point-to-BLM distance fields so build_final_map_only()
## can stay fast and only join cached values.

refresh_cnrfc_local_blm_distances_and_map <- function() {

  run_step(
    SCRIPT_PATHS$preprocess_cnrfc_local_blm_distance,
    "Precompute CNRFC Local BLM distances"
  )

  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

update_cnrfc_local_blm_distances <- function() {

  run_step(
    SCRIPT_PATHS$preprocess_cnrfc_local_blm_distance,
    "Precompute CNRFC Local BLM distances"
  )
}

# ---- 5.2B Refresh USGS GW Local BLM distances, then map --------------------
##
## Run this after refreshing BLM managed lands or the Local USGS Wells catalog.
## This is intentionally separate from 48_update_usgs_gw_blm_distance_fields.R,
## which enriches the Ops Live groundwater candidate index.

refresh_usgs_gw_local_blm_distances_and_map <- function() {

  run_step(
    SCRIPT_PATHS$preprocess_usgs_gw_local_blm_distance,
    "Precompute USGS GW Local BLM distances"
  )

  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

update_usgs_gw_local_blm_distances <- function() {

  run_step(
    SCRIPT_PATHS$preprocess_usgs_gw_local_blm_distance,
    "Precompute USGS GW Local BLM distances"
  )
}

# ---- 5.2C Refresh SWRCB POD BLM distances, then map ------------------------
##
## Run this after refreshing BLM managed lands or the SWRCB / CalWATRS POD
## map-ready cache. Distance calculations are intentionally kept out of
## build_final_map_only(); the map build only joins the compact CSV sidecar.

refresh_swrcb_pod_blm_distances_and_map <- function() {

  run_step(
    SCRIPT_PATHS$preprocess_swrcb_pod_blm_distance,
    "Precompute SWRCB POD BLM distances"
  )

  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

update_swrcb_pod_blm_distances <- function() {

  run_step(
    SCRIPT_PATHS$preprocess_swrcb_pod_blm_distance,
    "Precompute SWRCB POD BLM distances"
  )
}

# ---- 5.2D Refresh Springs BLM distances, then map --------------------------
##
## Run this after refreshing BLM managed lands or the normalized Springs source
## object.  The distance script writes a compact CSV sidecar keyed by spring_id;
## final map code can then join cached distance fields without doing spatial
## calculations during a normal HTML build.

refresh_springs_blm_distances_and_map <- function() {

  run_step_clean(
    SCRIPT_PATHS$preprocess_springs_blm_distance,
    "Precompute Springs BLM distances"
  )

  run_step_clean(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

update_springs_blm_distances <- function() {

  run_step_clean(
    SCRIPT_PATHS$preprocess_springs_blm_distance,
    "Precompute Springs BLM distances"
  )
}

# ---- 5.2E Refresh Springs source, distances, core cache, then map ----------
##
## Use this after changing the underlying Springs source data or after adding a
## new source inventory to 17_springs.r.  This deliberately reruns the source
## normalizer first, then updates BLM distances from the normalized output, then
## rebuilds the core map cache and final HTML.

refresh_springs_source_and_map <- function() {

  run_step_clean(
    SCRIPT_PATHS$preprocess_springs,
    "Preprocess normalized Springs source"
  )

  run_step_clean(
    SCRIPT_PATHS$preprocess_springs_blm_distance,
    "Precompute Springs BLM distances"
  )

  run_step_clean(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )

  run_step_clean(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 5.2F Refresh BLM GW well inventory source/distances, then map ----------
##
## Use these helpers for the two small BLM groundwater-well Local layers:
##   - BLM-drilled wells | NOC database
##   - GW wells | 2025 Mojave-BLM field check
##
## The source normalizer reads CSVs from 01_raw_data/blm and writes normalized
## WGS84 RDS files.  The distance preprocessor compares those normalized points
## against BRIM's current BLM managed-lands RDS and writes a compact CSV sidecar.
## Both use run_step_clean() so sf objects do not linger before HTML builds.

update_blm_gw_well_inventory_sources <- function() {

  run_step_clean(
    SCRIPT_PATHS$preprocess_blm_gw_well_inventory,
    "Preprocess BLM GW well inventory sources"
  )
}

update_blm_gw_well_inventory_blm_distances <- function() {

  run_step_clean(
    SCRIPT_PATHS$preprocess_blm_gw_well_inventory_blm_distance,
    "Precompute BLM GW well inventory BLM distances"
  )
}

refresh_blm_gw_well_inventory_and_map <- function() {

  run_step_clean(
    SCRIPT_PATHS$preprocess_blm_gw_well_inventory,
    "Preprocess BLM GW well inventory sources"
  )

  run_step_clean(
    SCRIPT_PATHS$preprocess_blm_gw_well_inventory_blm_distance,
    "Precompute BLM GW well inventory BLM distances"
  )

  run_step_clean(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )

  run_step_clean(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 5.3 Refresh CNRFC basins, labels, then map -----------------------------

refresh_cnrfc_basins_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_cnrfc_basins,
    "Preprocess CNRFC basins"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_label_cache,
    "Rebuild label cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 5.4 Refresh BLM offices, then map --------------------------------------

refresh_blm_offices_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_blm_offices,
    "Preprocess BLM offices"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 5.5 Refresh field-office outer boundaries, labels, then map ------------

refresh_field_office_outer_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_field_office_outer,
    "Preprocess field-office outer boundaries"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_label_cache,
    "Rebuild label cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 5.6 Refresh CalSim3 arcs, then map -------------------------------------

refresh_calsim3_arcs_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_calsim3_arcs,
    "Preprocess CalSim3 arcs"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 5.7 Refresh HUC / GW / county, labels, then map ------------------------

refresh_huc_gw_county_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_huc_gw_county,
    "Preprocess HUC / GW / county layers"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_label_cache,
    "Rebuild label cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 5.8 Refresh BLM managed / held lands, then map -------------------------

refresh_blm_main_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_blm_main,
    "Preprocess BLM managed / held lands"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 5.9 Refresh project areas, labels, then map ----------------------------

refresh_project_areas_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_project_areas,
    "Preprocess project areas placeholder"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_label_cache,
    "Rebuild label cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 5.10 Refresh static water / Delta layers, then map --------------------

refresh_new_static_water_layers_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_new_static_water_layers,
    "Preprocess static water / Delta layers"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 5.11 Refresh RWQCB regions, then map ----------------------------------

refresh_rwqcb_regions_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_rwqcb_regions,
    "Preprocess RWQCB regions"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 5.12 Build CDEC reservoir station index only --------------------------
##
## This is intentionally a backend metadata/crosswalk job. It fetches current
## CDEC reservoir station metadata, compares/enriches with local legacy CDEC /
## CNRFC / USGS crosswalk files, and writes station-index RDS/GPKG/QA outputs.
## It does not rebuild the map because the reservoir index is not yet wired into
## the visible BRIM layer stack.

build_reservoir_station_index <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_reservoir_station_index,
    "Build CDEC reservoir station index"
  )
}

# ---- 5.13 Export SCAN live-feed inputs only --------------------------------
##
## RF041:
##   Export the stable SCAN station-reference inventory to the live-data feed
##   repository input folder. This does not rebuild the map and does not fetch
##   current soil-moisture observations.

export_scan_live_inputs <- function() {
  
  run_step(
    SCRIPT_PATHS$export_scan_live_inputs,
    "Export SCAN live-feed station index"
  )
}

# ---- 5.13A Export Snow Pillow / SWE live-feed inputs only -------------------
##
## SWE003:
##   Export the stable snow-pillow / SNOTEL / CDEC snow-sensor station-reference
##   inventory to the live-data feed repository input folder. This does not
##   rebuild the map and does not fetch current SWE observations.

export_snow_pillow_live_inputs <- function() {
  
  run_step(
    SCRIPT_PATHS$export_snow_pillow_live_inputs,
    "Export snow pillow / SWE live-feed station index"
  )
}

# ---- 5.13B Build Snow Pillow / SWE latest live-feed files locally -----------
##
## SWE004:
##   Build the browser-facing snow-pillow / SWE latest GeoJSON, summary JSON,
##   and current-water-year trace CSV in the local brim-live-data-feeds repo.
##   This does not rebuild the BRIM HTML. After a successful local run, manually
##   upload/replace the docs/data outputs in GitHub if you want to publish them.

build_snow_pillow_latest_feed_local <- function() {
  
  if (!dir.exists("brim-live-data-feeds")) {
    stop(
      "Missing brim-live-data-feeds/ folder. Run this from the PortaTreasure2 / BRIM project root."
    )
  }
  
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  
  setwd("brim-live-data-feeds")
  
  run_step(
    "scripts/build_snow_pillow_latest.R",
    "Build snow pillow / SWE latest live-feed files"
  )
}

# ---- 5.13C Build Snow Pillow / SWE historical-context products -------------
##
## SWE007:
##   Build local/source and browser-facing historical SWE context files for the
##   future snow-pillow / SWE Ops Live popup plots. This is a local/occasional
##   maintenance step, not a daily/latest refresh.

build_snow_pillow_history_context <- function() {
  
  run_step(
    SCRIPT_PATHS$build_snow_pillow_history_context,
    "Build snow pillow / SWE historical context products"
  )
}

# ---- 5.13D Build Snow Pillow / SWE popup sandbox ---------------------------
##
## SWE005:
##   Build a small standalone Leaflet sandbox for iterating on the future
##   snow-pillow / SWE Ops Live hover, popup, marker styling, and current-WY
##   trace plot. This does not rebuild the full BRIM HTML.

build_snow_pillow_popup_sandbox <- function() {
  
  run_step(
    SCRIPT_PATHS$build_snow_pillow_popup_sandbox,
    "Build snow pillow / SWE popup sandbox"
  )
}

# ---- 5.14 Refresh USGS data, then map ---------------------------------------

refresh_usgs_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_usgs,
    "Preprocess USGS streamgages / wells"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ==== 6. Custom combo workflows ==============================================

# ---- 6.1 Refresh both CNRFC point layers, then map --------------------------

refresh_cnrfc_points_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_cnrfc_stream,
    "Preprocess CNRFC stream gages"
  )
  
  run_step(
    SCRIPT_PATHS$preprocess_cnrfc_precip,
    "Preprocess CNRFC precip gages"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}

# ---- 6.2 Refresh CNRFC basins + field-office outers, then map --------------

refresh_cnrfc_basins_and_fo_outer_and_map <- function() {
  
  run_step(
    SCRIPT_PATHS$preprocess_cnrfc_basins,
    "Preprocess CNRFC basins"
  )
  
  run_step(
    SCRIPT_PATHS$preprocess_field_office_outer,
    "Preprocess field-office outer boundaries"
  )
  
  run_step(
    SCRIPT_PATHS$build_core_cache,
    "Rebuild core cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_label_cache,
    "Rebuild label cache"
  )
  
  run_step(
    SCRIPT_PATHS$build_final_map,
    "Build final HTML"
  )
}


# ==== 5.14 CNRFC product-intelligence preprocessors ==========================
##
## These promote the tested CNRFC QA/audit products into stable RDS outputs
## under 04_processed_data/rds. They are still intentionally conservative:
## the QA scripts remain the proving ground while the logic continues to mature.

preprocess_cnrfc_feature_inventory <- function(rebuild_qa = FALSE) {
  old_options <- options(
    BRIM_CNRFC_FEATURE_REBUILD_QA = rebuild_qa
  )
  on.exit(options(old_options), add = TRUE)
  
  run_step(
    SCRIPT_PATHS$preprocess_cnrfc_feature_inventory,
    "Preprocess CNRFC feature inventory"
  )
}

preprocess_cnrfc_basin_product_availability <- function(max_basin_ids = Inf,
                                                        basin_ids = c("PITC1", "NWMC1", "ORDC1", "HLEC1", "NMSC1", "SCSC1", "SHDC1", "TMDC1"),
                                                        sample_strategy = c("all", "wru_mixed", "priority", "alphabetical", "random"),
                                                        sample_seed = 20260624L,
                                                        product_mode = c("high_value", "full", "wy_focus", "minimal"),
                                                        product_ids = NULL,
                                                        check_weather_gov = FALSE,
                                                        check_water_resources_update = TRUE,
                                                        use_wru_xml_products = TRUE,
                                                        request_delay_sec = 0.10,
                                                        resume_from_cache = TRUE,
                                                        force_refresh = FALSE,
                                                        force_refresh_ids = NULL,
                                                        cache_max_age_days = 30,
                                                        refresh_wru_xml = FALSE) {
  sample_strategy <- match.arg(sample_strategy)
  product_mode <- match.arg(product_mode)
  
  old_options <- options(
    BRIM_CNRFC_BASIN_MAX_IDS = max_basin_ids,
    BRIM_CNRFC_BASIN_IDS = basin_ids,
    BRIM_CNRFC_BASIN_SAMPLE_STRATEGY = sample_strategy,
    BRIM_CNRFC_BASIN_SAMPLE_SEED = sample_seed,
    BRIM_CNRFC_BASIN_PRODUCT_MODE = product_mode,
    BRIM_CNRFC_BASIN_PRODUCT_IDS = product_ids,
    BRIM_CNRFC_BASIN_CHECK_WEATHER_GOV = check_weather_gov,
    BRIM_CNRFC_BASIN_CHECK_WATER_RESOURCES_UPDATE = check_water_resources_update,
    BRIM_CNRFC_BASIN_USE_WRU_XML_PRODUCTS = use_wru_xml_products,
    BRIM_CNRFC_BASIN_REQUEST_DELAY_SEC = request_delay_sec,
    BRIM_CNRFC_BASIN_RESUME_FROM_CACHE = resume_from_cache,
    BRIM_CNRFC_BASIN_FORCE_REFRESH = force_refresh,
    BRIM_CNRFC_BASIN_FORCE_REFRESH_IDS = force_refresh_ids,
    BRIM_CNRFC_BASIN_CACHE_MAX_AGE_DAYS = cache_max_age_days,
    BRIM_CNRFC_PREPROCESS_REFRESH_WRU_XML = refresh_wru_xml
  )
  on.exit(options(old_options), add = TRUE)
  
  run_step(
    SCRIPT_PATHS$preprocess_cnrfc_basin_product_availability,
    "Preprocess CNRFC basin product availability"
  )
}


preprocess_cnrfc_forecast_point_product_availability <- function() {
  run_step(
    SCRIPT_PATHS$preprocess_cnrfc_forecast_point_product_availability,
    "Preprocess CNRFC forecast-point product availability"
  )
}


preprocess_cnrfc_precip_weather_link_availability <- function(max_live_checks = 30L,
                                                               force_refresh = FALSE,
                                                               force_refresh_ids = character(0),
                                                               diagnostic_ids = c(
                                                                 "BKSC1", "BDMC1", "LBIC1", "SFXC1",
                                                                 "PVF", "SMF", "SFO",
                                                                 "KPVF", "KSMF", "KSFO"
                                                               ),
                                                               cache_max_age_days = 14L,
                                                               request_delay_sec = 0.05,
                                                               timeout_sec = 20L,
                                                               include_api_latest = TRUE,
                                                               use_current_onehourP_kml = TRUE,
                                                               use_current_onehourP_kml_as_authority = FALSE,
                                                               include_current_snapshot_extra_ids = FALSE,
                                                               include_class_coded_historical_extra_ids = FALSE) {
  old_options <- options(
    BRIM_CNRFC_NOAA_LINK_MAX_LIVE_CHECKS = max_live_checks,
    BRIM_CNRFC_NOAA_LINK_FORCE_REFRESH = force_refresh,
    BRIM_CNRFC_NOAA_LINK_FORCE_REFRESH_IDS = force_refresh_ids,
    BRIM_CNRFC_NOAA_LINK_DIAGNOSTIC_IDS = diagnostic_ids,
    BRIM_CNRFC_NOAA_LINK_CACHE_MAX_AGE_DAYS = cache_max_age_days,
    BRIM_CNRFC_NOAA_LINK_REQUEST_DELAY_SEC = request_delay_sec,
    BRIM_CNRFC_NOAA_LINK_TIMEOUT_SEC = timeout_sec,
    BRIM_CNRFC_NOAA_LINK_INCLUDE_API_LATEST = include_api_latest,
    BRIM_CNRFC_NOAA_LINK_USE_CURRENT_ONEHOURP_KML = use_current_onehourP_kml,
    BRIM_CNRFC_NOAA_LINK_USE_CURRENT_ONEHOURP_KML_AS_AUTHORITY = use_current_onehourP_kml_as_authority,
    BRIM_CNRFC_NOAA_LINK_INCLUDE_CURRENT_SNAPSHOT_EXTRA_IDS = include_current_snapshot_extra_ids,
    BRIM_CNRFC_NOAA_LINK_INCLUDE_CLASS_CODED_HISTORICAL_EXTRA_IDS = include_class_coded_historical_extra_ids
  )
  on.exit(options(old_options), add = TRUE)

  run_step(
    SCRIPT_PATHS$preprocess_cnrfc_precip_weather_link_availability,
    "Preprocess CNRFC precip/weather NOAA-NWS link availability"
  )
}


# ==== 6. QA/QC workflow functions ============================================

qa_local_layer_cache_check <- function() {
  run_step(
    SCRIPT_PATHS$qa_local_layer_cache_check,
    "QA local layer cache products"
  )
}

qa_local_layer_payload_size_check <- function() {
  run_step(
    SCRIPT_PATHS$qa_local_layer_payload_size_check,
    "QA local layer HTML payload-size estimates"
  )
}

qa_calsim3_network_audit <- function() {
  run_step(
    SCRIPT_PATHS$qa_calsim3_network_audit,
    "QA CalSim3 network types and fields"
  )
}

qa_cnrfc_product_intelligence_audit <- function() {
  run_step(
    SCRIPT_PATHS$qa_cnrfc_product_intelligence_audit,
    "QA CNRFC product intelligence audit"
  )
}

qa_cnrfc_weather_gov_wrh_audit <- function(rebuild_cnrfc_base = FALSE,
                                           run_live_checks = TRUE,
                                           max_live_ids = 75L,
                                           live_ids = c("ADRC1", "PITC1", "NWMC1", "ORDC1"),
                                           request_delay_sec = 0.15) {
  old_options <- options(
    BRIM_CNRFC_WRH_REBUILD_BASE = rebuild_cnrfc_base,
    BRIM_CNRFC_WRH_RUN_LIVE_CHECKS = run_live_checks,
    BRIM_CNRFC_WRH_MAX_LIVE_IDS = max_live_ids,
    BRIM_CNRFC_WRH_LIVE_IDS = live_ids,
    BRIM_CNRFC_WRH_REQUEST_DELAY_SEC = request_delay_sec
  )
  on.exit(options(old_options), add = TRUE)

  run_step(
    SCRIPT_PATHS$qa_cnrfc_weather_gov_wrh_audit,
    "QA CNRFC + Weather.gov WRH time-series audit"
  )
}

qa_cnrfc_basin_product_availability_audit <- function(max_basin_ids = 25L,
                                                     basin_ids = c("PITC1", "NWMC1", "ORDC1", "HLEC1", "NMSC1", "SCSC1", "SHDC1", "TMDC1"),
                                                     sample_strategy = c("priority", "wru_mixed", "alphabetical", "random", "all"),
                                                     sample_seed = 20260624L,
                                                     product_mode = c("full", "high_value", "wy_focus", "minimal"),
                                                     product_ids = NULL,
                                                     check_weather_gov = TRUE,
                                                     check_water_resources_update = TRUE,
                                                     use_wru_xml_products = TRUE,
                                                     request_delay_sec = 0.15,
                                                     resume_from_cache = TRUE,
                                                     force_refresh = FALSE,
                                                     force_refresh_ids = NULL,
                                                     cache_max_age_days = 30) {
  sample_strategy <- match.arg(sample_strategy)
  product_mode <- match.arg(product_mode)

  old_options <- options(
    BRIM_CNRFC_BASIN_MAX_IDS = max_basin_ids,
    BRIM_CNRFC_BASIN_IDS = basin_ids,
    BRIM_CNRFC_BASIN_SAMPLE_STRATEGY = sample_strategy,
    BRIM_CNRFC_BASIN_SAMPLE_SEED = sample_seed,
    BRIM_CNRFC_BASIN_PRODUCT_MODE = product_mode,
    BRIM_CNRFC_BASIN_PRODUCT_IDS = product_ids,
    BRIM_CNRFC_BASIN_CHECK_WEATHER_GOV = check_weather_gov,
    BRIM_CNRFC_BASIN_CHECK_WATER_RESOURCES_UPDATE = check_water_resources_update,
    BRIM_CNRFC_BASIN_USE_WRU_XML_PRODUCTS = use_wru_xml_products,
    BRIM_CNRFC_BASIN_REQUEST_DELAY_SEC = request_delay_sec,
    BRIM_CNRFC_BASIN_RESUME_FROM_CACHE = resume_from_cache,
    BRIM_CNRFC_BASIN_FORCE_REFRESH = force_refresh,
    BRIM_CNRFC_BASIN_FORCE_REFRESH_IDS = force_refresh_ids,
    BRIM_CNRFC_BASIN_CACHE_MAX_AGE_DAYS = cache_max_age_days
  )
  on.exit(options(old_options), add = TRUE)

  run_step(
    SCRIPT_PATHS$qa_cnrfc_basin_product_availability_audit,
    "QA CNRFC basin product availability matrix"
  )
}


qa_cnrfc_water_resources_update_diagnostic <- function(max_context_ids = 60L,
                                                       context_ids = c("PITC1", "NWMC1", "ORDC1", "HLEC1", "NMSC1", "FOLC1", "SHDC1", "AKYC1"),
                                                       request_delay_sec = 0.0) {
  old_options <- options(
    BRIM_CNRFC_WRU_MAX_CONTEXT_IDS = max_context_ids,
    BRIM_CNRFC_WRU_CONTEXT_IDS = context_ids,
    BRIM_CNRFC_WRU_REQUEST_DELAY_SEC = request_delay_sec
  )
  on.exit(options(old_options), add = TRUE)

  run_step(
    SCRIPT_PATHS$qa_cnrfc_water_resources_update_diagnostic,
    "QA CNRFC Water Resources Update diagnostic"
  )
}


qa_cnrfc_water_resources_update_kml_audit <- function(max_kml_files = 100L,
                                                   fetch_kml = TRUE,
                                                   request_delay_sec = 0.10,
                                                   max_context_ids = 120L) {
  old_options <- options(
    BRIM_CNRFC_WRU_KML_MAX_FILES = max_kml_files,
    BRIM_CNRFC_WRU_KML_FETCH = fetch_kml,
    BRIM_CNRFC_WRU_KML_REQUEST_DELAY_SEC = request_delay_sec,
    BRIM_CNRFC_WRU_KML_MAX_CONTEXT_IDS = max_context_ids
  )
  on.exit(options(old_options), add = TRUE)

  run_step(
    SCRIPT_PATHS$qa_cnrfc_water_resources_update_kml_audit,
    "QA CNRFC Water Resources Update KML/control audit"
  )
}


qa_cnrfc_active_forecast_point_audit <- function(fetch_pages = TRUE,
                                                request_delay_sec = 0.10,
                                                max_context_chars = 180L) {
  old_options <- options(
    BRIM_CNRFC_ACTIVE_FETCH_PAGES = fetch_pages,
    BRIM_CNRFC_ACTIVE_REQUEST_DELAY_SEC = request_delay_sec,
    BRIM_CNRFC_ACTIVE_MAX_CONTEXT_CHARS = max_context_chars
  )
  on.exit(options(old_options), add = TRUE)

  run_step(
    SCRIPT_PATHS$qa_cnrfc_active_forecast_point_audit,
    "QA CNRFC active homepage forecast-point audit"
  )
}



qa_cnrfc_active_forecast_source_diagnostic <- function(fetch_external_scripts = TRUE,
                                                        max_external_scripts = 30L,
                                                        fetch_candidate_endpoints = TRUE,
                                                        request_delay_sec = 0.10,
                                                        max_context_chars = 220L) {
  old_options <- options(
    BRIM_CNRFC_ACTIVE_SRC_FETCH_EXTERNAL_SCRIPTS = fetch_external_scripts,
    BRIM_CNRFC_ACTIVE_SRC_MAX_EXTERNAL_SCRIPTS = max_external_scripts,
    BRIM_CNRFC_ACTIVE_SRC_FETCH_CANDIDATE_ENDPOINTS = fetch_candidate_endpoints,
    BRIM_CNRFC_ACTIVE_SRC_REQUEST_DELAY_SEC = request_delay_sec,
    BRIM_CNRFC_ACTIVE_SRC_MAX_CONTEXT_CHARS = max_context_chars
  )
  on.exit(options(old_options), add = TRUE)

  run_step(
    SCRIPT_PATHS$qa_cnrfc_active_forecast_source_diagnostic,
    "QA CNRFC active forecast source diagnostic"
  )
}



qa_cnrfc_active_forecast_xml_parse_audit <- function(request_delay_sec = 0.10,
                                                     endpoint_timeout_sec = 60L,
                                                     max_record_text_chars = 1800L) {
  old_options <- options(
    BRIM_CNRFC_ACTIVE_XML_REQUEST_DELAY_SEC = request_delay_sec,
    BRIM_CNRFC_ACTIVE_XML_ENDPOINT_TIMEOUT_SEC = endpoint_timeout_sec,
    BRIM_CNRFC_ACTIVE_XML_MAX_RECORD_TEXT_CHARS = max_record_text_chars
  )
  on.exit(options(old_options), add = TRUE)

  run_step(
    SCRIPT_PATHS$qa_cnrfc_active_forecast_xml_parse_audit,
    "QA CNRFC active forecast XML parse audit"
  )
}

# ==== 7. Ready-to-click commands =============================================
##
## Put your cursor on ONE of the lines below and press Cmd/Ctrl + Enter.
##
## COMMON CHOICES:
##   build_final_map_only()
##   rebuild_core_cache_and_map()
##   rebuild_label_cache_and_map()
##   rebuild_everything_from_cache_and_map()
##   refresh_calsim3_arcs_and_map()
##   refresh_cnrfc_basins_and_map()
##   qa_local_layer_cache_check()
##   qa_local_layer_payload_size_check()
##   qa_calsim3_network_audit()
##   qa_cnrfc_product_intelligence_audit()
##   qa_cnrfc_weather_gov_wrh_audit()
##   qa_cnrfc_basin_product_availability_audit()
##   qa_cnrfc_water_resources_update_diagnostic()
##   qa_cnrfc_water_resources_update_kml_audit()
##   qa_cnrfc_active_forecast_point_audit()
##   qa_cnrfc_active_forecast_source_diagnostic()
##   qa_cnrfc_active_forecast_xml_parse_audit()
##   preprocess_cnrfc_feature_inventory()
##   preprocess_cnrfc_basin_product_availability()
##   preprocess_cnrfc_forecast_point_product_availability()
##   preprocess_cnrfc_precip_weather_link_availability()
##   update_usgs_gw_local_blm_distances()
##   refresh_usgs_gw_local_blm_distances_and_map()
##   update_swrcb_pod_blm_distances()
##   refresh_swrcb_pod_blm_distances_and_map()

######################

#build_final_map_only()
# rebuild_core_cache_and_map()
# rebuild_label_cache_and_map()
# rebuild_everything_from_cache_and_map()

# refresh_cnrfc_stream_and_map()
# refresh_cnrfc_precip_and_map()
# refresh_cnrfc_basins_and_map()
# refresh_blm_offices_and_map()
# refresh_field_office_outer_and_map()
# refresh_calsim3_arcs_and_map()
# refresh_huc_gw_county_and_map()
# refresh_blm_main_and_map()
# refresh_project_areas_and_map()
# refresh_usgs_and_map()
# refresh_new_static_water_layers_and_map()
# refresh_rwqcb_regions_and_map()
# build_reservoir_station_index()
# export_scan_live_inputs()
# export_snow_pillow_live_inputs()
# build_snow_pillow_latest_feed_local()
# build_snow_pillow_history_context()
# build_snow_pillow_popup_sandbox()

# qa_local_layer_cache_check()
# qa_local_layer_payload_size_check()
# qa_calsim3_network_audit()
# qa_cnrfc_product_intelligence_audit()
# qa_cnrfc_weather_gov_wrh_audit()
# qa_cnrfc_basin_product_availability_audit()
# qa_cnrfc_water_resources_update_diagnostic()
# qa_cnrfc_water_resources_update_kml_audit()
# qa_cnrfc_active_forecast_point_audit()
# qa_cnrfc_active_forecast_source_diagnostic()
# qa_cnrfc_active_forecast_xml_parse_audit()
# preprocess_cnrfc_feature_inventory()
# preprocess_cnrfc_basin_product_availability()
# preprocess_cnrfc_forecast_point_product_availability()
# preprocess_cnrfc_precip_weather_link_availability()

# update_usgs_gw_local_blm_distances()
# refresh_usgs_gw_local_blm_distances_and_map()
# update_swrcb_pod_blm_distances()
# refresh_swrcb_pod_blm_distances_and_map()

# refresh_cnrfc_points_and_map()
# refresh_cnrfc_basins_and_fo_outer_and_map()

# ==== 8. End note ============================================================

message("\nRunner loaded.")
message("Click one function call in Section 7 and run just that line.")