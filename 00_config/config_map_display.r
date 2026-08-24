# ==== config_map_display.r ===================================================
##
## PURPOSE:
##   Central display settings for PortaTreasure2 Leaflet map builds.
##
## WHY THIS FILE EXISTS:
##   This file controls map-building behavior only:
##
##     - default map center and zoom
##     - half-zoom behavior
##     - which cached layers are added to the map
##     - whether labels are added
##     - which overlay layers are visible when the map first opens
##     - HTML output behavior
##
## IMPORTANT:
##   This file does NOT control preprocessing.
##   This file does NOT control label text fields or label zoom thresholds.
##
##   Label-specific settings live in:
##
##     00_config/config_labels.r
##


# ---- Hosted Ops Live feed base URL ------------------------------------------
##
## Keep the GitHub Pages feed root in one place so a future hosting move only
## requires changing this value, not every Ops Live feed URL below.
##
BRIM_LIVE_FEED_BASE_URL <- "https://dbo99.github.io/brim-live-data-feeds"

brim_live_feed_url <- function(path) {
  paste0(
    sub("/+$", "", BRIM_LIVE_FEED_BASE_URL),
    "/",
    sub("^/+", "", path)
  )
}

# ==== 1. Main map-display settings ===========================================

MAP_DISPLAY <- list(
  
  # ---- Default map view -----------------------------------------------------
  ##
  ## Default startup view selected from the polished PT2 statewide/local-layer
  ## screenshot view. This centers the startup map on California / western
  ## Nevada with enough context to see the BLM-CA managed lands, field-office
  ## boundaries, and BLM office points.
  default_lng  = -118.73584,
  default_lat  = 38.30517,
  default_zoom = 6,
  
  # ---- Radar and basemaps ---------------------------------------------------
  ##
  add_ops_live_layers = TRUE,
  # Delta Ops Daily Summary / CVP-SWP snapshot
  add_ops_delta_ops_daily_summary = TRUE,
  ops_delta_ops_daily_summary_url = brim_live_feed_url("data/delta_ops_daily_summary_features.geojson"),
  ops_delta_ops_daily_summary_summary_url = brim_live_feed_url("data/delta_ops_daily_summary_summary.json"),
  ops_delta_ops_x2_reference_url = brim_live_feed_url("data/delta_ops_x2_reference.geojson"),

  add_ops_conus_radar = TRUE,
  add_ops_cdec_reservoir_storage = TRUE,
  ops_cdec_reservoir_storage_url = brim_live_feed_url("data/cdec_reservoir_latest.geojson"),
  ops_cdec_reservoir_storage_summary_url = brim_live_feed_url("data/cdec_reservoir_latest_summary.json"),

  ## CNRFC active river/reservoir forecast points are a compact local
  ## preprocessor output (53_) added to Ops Live.  They are not fetched from
  ## GitHub Pages yet because the active XML endpoints are parsed by the local
  ## BRIM preprocessor.
  add_ops_cnrfc_river_reservoir_forecast_points = TRUE,

  ## Static forecast geometry is embedded at final-map build time; current
  ## CNRFC/CBRFC values are fetched and validated independently in the browser.
  add_ops_major_water_supply_basin_forecasts = TRUE,
  ops_major_water_supply_cnrfc_url = brim_live_feed_url("data/major_water_supply_basin_forecasts.json"),
  ops_major_water_supply_cbrfc_url = brim_live_feed_url("data/cbrfc_major_water_supply_forecasts.json"),

  ## CNRFC precip/weather stations are built by 54_ after the NOAA/NWS audit.
  ## Ops Live uses the verified ASOS/airport + RAWS/GOES NWS/WRH time-series
  ## subset; the broader station-authority catalog is exposed as a Local layer.
  add_ops_cnrfc_precip_weather_stations = TRUE,

  ## Realtime wind feeds are produced by the separate brim-live-data-feeds
  ## repository and fetched only when their Ops Live rows are enabled. GFS is
  ## an animated model field selected from an hourly time-set manifest. The
  ## METAR/ASOS layer is a compact observed-station GeoJSON with a summary file.
  add_ops_gfs_surface_wind = TRUE,
  ops_gfs_surface_wind_manifest_url = brim_live_feed_url("data/wind/gfs_surface_wind_feed_manifest.json"),

  add_ops_hrrr_surface_wind = TRUE,
  ops_hrrr_surface_wind_manifest_url = brim_live_feed_url("data/wind/hrrr_surface_wind_feed_manifest.json"),

  add_ops_nbm_wind_guidance = TRUE,
  ops_nbm_wind_guidance_manifest_url = brim_live_feed_url("data/wind/nbm_wind_guidance_feed_manifest.json"),

  ## NOAA/NBM modeled Snow Levels, six-hour QPF, and consumer-computed
  ## accumulated QPF are independent Ops rows sharing one exact cycle
  ## controller. Accumulated QPF uses only the existing numeric six-hour
  ## sidecars and creates no public product or stored forecast asset.
  add_ops_nbm_snow_levels = TRUE,
  add_ops_nbm_qpf = TRUE,
  add_ops_nbm_accumulated_qpf = TRUE,
  ops_nbm_snow_levels_manifest_url = brim_live_feed_url("data/winter-storm-levels/winter_storm_levels_manifest.json"),
  ops_nbm_qpf_manifest_url = brim_live_feed_url("data/nbm-qpf/nbm_qpf_manifest.json"),

  add_ops_asos_awos_wind = TRUE,
  ops_asos_awos_wind_url = brim_live_feed_url("data/wind/asos_awos_wind_latest.geojson"),
  ops_asos_awos_wind_summary_url = brim_live_feed_url("data/wind/asos_awos_wind_latest_summary.json"),
  ops_asos_awos_wind_manifest_url = brim_live_feed_url("data/wind/asos_awos_wind_feed_manifest.json"),

  ## CoCoRaHS is fetched by a separate GitHub Action into static GeoJSON.
  ## The browser loads only the hosted feed so local/file:// CORS restrictions
  ## do not block the layer.
  add_ops_cocorahs_daily_precip = TRUE,
  ops_cocorahs_ca_daily_precip_url = brim_live_feed_url("data/cocorahs_daily_precip_ca_latest.geojson"),
  ops_cocorahs_ca_daily_precip_summary_url = brim_live_feed_url("data/cocorahs_daily_precip_ca_latest_summary.json"),
  ops_cocorahs_conus_daily_precip_url = brim_live_feed_url("data/cocorahs_daily_precip_conus_latest.geojson"),
  ops_cocorahs_conus_daily_precip_summary_url = brim_live_feed_url("data/cocorahs_daily_precip_conus_latest_summary.json"),

  ## USGS streamflow latest values are retrieved by the brim-live-data-feeds
  ## GitHub workflow and loaded by Ops Live as a hosted GeoJSON. The Ops layer
  ## intentionally draws only sites with latest continuous discharge or stage;
  ## the static USGS streamgage layer remains the full reference backbone.
  add_ops_usgs_streamflow_latest = TRUE,
  ops_usgs_streamflow_latest_url = brim_live_feed_url("data/usgs_streamflow_latest_ca.geojson"),
  ops_usgs_streamflow_latest_summary_url = brim_live_feed_url("data/usgs_streamflow_latest_ca_summary.json"),

  ## USGS groundwater latest levels are retrieved by the brim-live-data-feeds
  ## GitHub workflow and loaded by Ops Live as a hosted GeoJSON. This is a
  ## focused active/recent candidate subset, not the full static USGS wells
  ## backbone. Screen/perforation intervals are shown only if the feed contains
  ## them; current public USGS API outputs usually provide well/hole depth and
  ## aquifer codes, but not screened/open-interval detail.
  add_ops_usgs_groundwater_latest = TRUE,
  ops_usgs_groundwater_latest_url = brim_live_feed_url("data/usgs_groundwater_latest_ca.geojson"),
  ops_usgs_groundwater_latest_summary_url = brim_live_feed_url("data/usgs_groundwater_latest_ca_summary.json"),

  ## NRCS SCAN soil moisture latest/context feed. The daily latest GeoJSON and
  ## current-WY trace are built by the brim-live-data-feeds GitHub workflow.
  ## Historical/context CSVs are compact local-preprocessor outputs copied to
  ## the GitHub Pages /data folder for browser-side popup plots.
  add_ops_scan_soil_moisture_latest = TRUE,
  ops_scan_soil_moisture_latest_url = brim_live_feed_url("data/scan_soil_moisture_latest.geojson"),
  ops_scan_soil_moisture_summary_url = brim_live_feed_url("data/scan_soil_moisture_latest_summary.json"),
  ops_scan_soil_moisture_trace_url = brim_live_feed_url("data/scan_soil_moisture_current_wy_trace.csv"),
  ops_scan_depth_style_url = brim_live_feed_url("data/scan_depth_style.csv"),
  ops_scan_waterday_percentiles_url = brim_live_feed_url("data/scan_sms_waterday_percentiles.csv"),
  ops_scan_monthly_context_url = brim_live_feed_url("data/scan_sms_monthly_context.csv"),
  ops_scan_prior_wy_fallback_traces_url = brim_live_feed_url("data/scan_sms_prior_wy_fallback_traces.csv"),

  ## Snow-pillow / SWE latest/context feed. The daily latest GeoJSON and
  ## current-WY trace are built by the brim-live-data-feeds GitHub workflow.
  ## Historical/context CSVs are compact local-preprocessor outputs copied to
  ## GitHub Pages /data for browser-side popup plots.
  add_ops_snow_pillow_latest = TRUE,
  ops_snow_pillow_latest_url = brim_live_feed_url("data/snow_pillow_latest.geojson"),
  ops_snow_pillow_summary_url = brim_live_feed_url("data/snow_pillow_latest_summary.json"),
  ops_snow_pillow_trace_url = brim_live_feed_url("data/snow_pillow_current_wy_trace.csv"),
  ops_snow_pillow_waterday_percentiles_url = brim_live_feed_url("data/snow_pillow_swe_waterday_percentiles.csv"),
  ops_snow_pillow_monthly_context_url = brim_live_feed_url("data/snow_pillow_swe_monthly_context.csv"),
  ops_snow_pillow_prior_wy_fallback_traces_url = brim_live_feed_url("data/snow_pillow_swe_prior_wy_fallback_traces.csv"),

  default_base_group = "USGS Hydrography",
  
  # ---- Zoom behavior --------------------------------------------------------
  ##
  ## Half-step zooms make a dense statewide map feel smoother.
  ##
  ## wheel_px_per_zoom_level controls mouse-wheel sensitivity. A larger value
  ## requires more wheel movement per zoom level, making it easier to land on
  ## half-step zooms such as 6.5 or 7.5 instead of jumping a full level.
  zoom_snap  = 0.5,
  zoom_delta = 0.5,
  wheel_px_per_zoom_level = 160,
  
  # ---- Output behavior ------------------------------------------------------
  ##
  ## FALSE creates a smaller HTML plus a dependency folder.
  ## TRUE creates a single standalone HTML file, but it will be much larger.
  self_contained_html = TRUE,
  
  ## Whether to open the saved HTML automatically after building.
  open_after_save = TRUE,
  
  # ---- Label behavior -------------------------------------------------------
  ##
  ## Set FALSE when troubleshooting slow map loading.
  ## Labels can be expensive because they create many browser-side text objects.
  add_labels = TRUE,
  
  # ---- Core polygon / line / point layer switches ---------------------------
  add_project_areas      = FALSE,  # hidden from Local/Core; users can add project areas through Upload
  add_blm_offices        = TRUE,
  add_field_office_outer = TRUE,
  
  add_huc10 = TRUE,
  add_huc12 = TRUE,
  
  ## Local CNRFC river/reservoir catalog/review layer. This is broader than the
  ## Ops Live CNRFC forecast-point layer and may include fragmented or limited
  ## CNRFC product/time-series availability.
  add_cnrfc_stream = TRUE,
  ## Legacy local CNRFC precip-gage layer remains in the project, but is hidden
  ## from the main Local panel by default now that 54_ builds the broader
  ## station-authority catalog below.
  add_cnrfc_precip = FALSE,
  add_cnrfc_precip_weather_station_catalog = TRUE,

  ## Local CDEC reservoir-station points are hidden by default.  The Ops Live
  ## reservoir layer is the authoritative reservoir screening layer now, and
  ## reservoirs do not need a separate Local catalog layer in normal use.
  ## 04_build_portatreasure2_core_map.r also avoids reading this RDS when FALSE
  ## so the stations are not embedded in the output HTML.
  add_cdec_reservoir_stations = FALSE,
  
  add_usgs_streamgages = TRUE,
  add_usgs_wells       = TRUE,

  ## Small BLM groundwater-well inventory layers normalized by 18_ and
  ## prepared for Local display by 048c. Keep these as separate source layers
  ## so NOC database records and the 2025 Mojave-BLM field check remain
  ## transparent for field-staff review.
  add_blm_noc_drilled_wells = TRUE,
  add_mojave_2025_gw_well_inventory = TRUE,
  
  add_swrcb_pod_wr_blm = TRUE,
  
  add_springs = TRUE,
  
  ## Local SCAN and snow-pillow points are hidden by default.  These station
  ## networks are now represented by the richer Ops Live SCAN/SWE layers; keep
  ## the old Local caches in the project, but do not embed them in the output
  ## HTML unless explicitly re-enabled for debugging.
  add_scan_stations = FALSE,
  add_snow_pillows  = FALSE,
  
  add_cnrfc_basins = FALSE,
  add_cnrfc_fnf_delta = TRUE,
  add_cnrfc_basin_product_availability = TRUE,
  add_calsim3_arcs = TRUE,
  add_calsim3_nodes = TRUE,
  add_reference_layers = TRUE,
  
  ## Consolidated curated conveyance layer produced by the reproducible 66_
  ## pipeline. The two legacy source layers remain fully retained in BRIM, but
  ## are not read or embedded in the normal HTML while their switches are FALSE.
  add_brim_mapped_conveyance = TRUE,
  add_major_conveyance = FALSE,
  add_deltamapr_canals = FALSE,

  add_water_districts = TRUE,
  add_rwqcb_regions = TRUE,

  # ---- Left-side tools / external-data panel --------------------------------
  ##
  ## This panel is browser-side only. It does not change caches or saved data.
  ## It provides:
  ##   - simple distance / area measurement tools
  ##   - up to three temporary user-added GIS service layers
  ##   - curated source links for finding public GIS services
  ##   - a built-in BLM CA Surface Management Agency context overlay
  add_tools_adddata_panel = TRUE,
  add_blm_sma_context_overlay = TRUE,

  # ---- Default visible overlay layers --------------------------------------
  ##
  ## Startup layers are intentionally limited to a small, useful local-layer
  ## context for first-time users:
  ##   - BLM offices
  ##   - BLM field-office boundaries
  ##   - BLM-CA managed lands
  ##
  ## IMPORTANT:
  ##   These names use the categorized Leaflet group names created by
  ##   pt_layer_group_name() / pt_order_overlay_groups().  If a default layer
  ##   does not turn on at startup, check for a mismatch between this vector and
  ##   the group names in leaflet_layer_helpers.r.
  default_visible_overlays = c(
    "Core – BLM Offices",
    "Core – BLM Field Office Boundaries",
    "Core – BLM-CA Managed"
  )
)

# ==== 2. Console confirmation ================================================

message("PortaTreasure2 map-display configuration loaded.")
message("Labels enabled for map build: ", MAP_DISPLAY$add_labels)
message("Self-contained HTML: ", MAP_DISPLAY$self_contained_html)
