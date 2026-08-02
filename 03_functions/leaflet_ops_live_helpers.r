# ==== leaflet_ops_live_helpers.r ============================================
##
## PURPOSE:
##   Public entry point for BRIM Ops Live layers.
##
## DESIGN:
##   The final map builder should continue to source only this file.  This file
##   now sources shorter feature-specific modules and injects their browser-side
##   JavaScript into one htmlwidgets/onRender function.
##
## NOTE:
##   This helper does not require cache rebuilding. It is used only by the final
##   HTML map builder.
## ============================================================================

# ==== 1. Source Ops Live modules =============================================
##
## Keep this file as the stable public entry point because the final map builder
## sources it directly. Shorter modules below keep feature code isolated and
## easier to debug.

pt_ops_live_source_module <- function(filename, label) {
  path <- file.path("03_functions", filename)

  if (!file.exists(path)) {
    stop("Missing ", label, ": ", path)
  }

  source(path, local = FALSE)
}

pt_ops_live_source_module("leaflet_ops_live_shared_helpers.r", "shared Ops Live helper")
pt_ops_live_source_module("leaflet_ops_live_weather_hazards_helpers.r", "Ops Live weather-hazards helper")
pt_ops_live_source_module("leaflet_ops_live_wpc_qpf_hover_helpers.r", "Ops Live WPC QPF hover helper")
pt_ops_live_source_module("leaflet_ops_live_legend_helpers.r", "Ops Live legend helper")
pt_ops_live_source_module("leaflet_ops_live_reservoir_helpers.r", "Ops Live reservoir helper")
pt_ops_live_source_module("leaflet_ops_live_cocorahs_helpers.r", "Ops Live CoCoRaHS helper")
pt_ops_live_source_module("leaflet_ops_live_usgs_streamflow_helpers.r", "Ops Live USGS streamflow helper")
pt_ops_live_source_module("leaflet_ops_live_usgs_groundwater_helpers.r", "Ops Live USGS groundwater helper")
pt_ops_live_source_module("leaflet_ops_live_scan_helpers.r", "Ops Live SCAN soil moisture helper")
pt_ops_live_source_module("leaflet_ops_live_snow_pillow_helpers.r", "Ops Live snow-pillow / SWE helper")
pt_ops_live_source_module("leaflet_ops_live_delta_ops_helpers.r", "Ops Live Delta operations helper")
pt_ops_live_source_module("leaflet_ops_live_cnrfc_forecast_points_helpers.r", "Ops Live CNRFC forecast-points helper")
pt_ops_live_source_module("leaflet_ops_live_cnrfc_precip_weather_helpers.r", "Ops Live CNRFC precip/weather helper")
pt_ops_live_source_module("leaflet_ops_live_major_water_supply_basin_helpers.r", "Ops Live major water-supply basin helper")
pt_ops_live_source_module("leaflet_ops_live_arcgis_export_helpers.r", "Ops Live ArcGIS export helper")
pt_ops_live_source_module("leaflet_ops_live_service_helpers.r", "Ops Live service/status helper")
pt_ops_live_source_module("leaflet_ops_live_layer_definition_helpers.r", "Ops Live layer-definition helper")
pt_ops_live_source_module("leaflet_ops_live_wind_helpers.r", "Ops Live GFS wind/vector-field helper")
pt_ops_live_source_module("leaflet_ops_live_hrrr_wind_helpers.r", "Ops Live HRRR wind/vector-field helper")
pt_ops_live_source_module("leaflet_ops_live_nbm_wind_helpers.r", "Ops Live NBM blended wind-guidance helper")
pt_ops_live_source_module("leaflet_ops_live_observed_wind_helpers.r", "Ops Live observed-wind helper")
pt_ops_live_source_module("leaflet_ops_live_panel_helpers.r", "Ops Live panel helper")

# ==== 2. Small internal text helper ==========================================
##
## Avoid base::sub()/gsub() for JavaScript injection because replacement strings
## can contain backslashes and other characters that regex replacement can treat
## specially. This plain fixed-token replacement is safer for browser JS text.

pt_ops_live_replace_js_token <- function(js, token, replacement) {

  pos <- regexpr(token, js, fixed = TRUE)[[1]]

  if (is.na(pos) || pos < 0) {
    stop("Ops Live JavaScript token not found: ", token)
  }

  paste0(
    substr(js, 1, pos - 1),
    replacement,
    substr(js, pos + nchar(token), nchar(js))
  )
}

# ==== 3. Catalog visibility helper ==========================================
##
## Ops Live has a few rows that are launched from the External Layers catalog
## so the catalog remains the shared source of truth for URL, hover/popup fields,
## aliases, and symbology.  Read the catalog's primary_panel column here so
## rows set to primary_panel = disabled can be suppressed from Ops Live at build
## time without deleting their catalog metadata.

pt_ops_live_catalog_primary_panel_map <- function(catalog_path = file.path("00_config", "external_service_catalog.csv")) {

  if (!file.exists(catalog_path)) {
    return(list())
  }

  catalog_df <- utils::read.csv(
    catalog_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!"display_name" %in% names(catalog_df)) {
    return(list())
  }

  if (!"primary_panel" %in% names(catalog_df)) {
    catalog_df$primary_panel <- "external"
  }

  normalize_panel <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x <- gsub("[_[:space:]-]+", "_", x)
    x[x == "ops"] <- "ops_live"
    x[!x %in% c("external", "ops_live", "both", "disabled")] <- "external"
    x
  }

  display_name <- trimws(as.character(catalog_df$display_name))
  primary_panel <- normalize_panel(catalog_df$primary_panel)

  keep <- !is.na(display_name) & display_name != ""
  out <- as.list(primary_panel[keep])
  names(out) <- display_name[keep]

  out
}

# ==== 4. Add Ops Live panel and layers =======================================

pt_ops_live_records_as_row_list <- function(x, cols = NULL) {
  if (!is.data.frame(x) || nrow(x) == 0) {
    return(list())
  }

  if (inherits(x, "sf")) {
    x <- sf::st_drop_geometry(x)
  }

  if (!is.null(cols)) {
    keep <- intersect(cols, names(x))
    x <- x[, keep, drop = FALSE]
  }

  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (nm in names(x)) {
    if (inherits(x[[nm]], "Date") || inherits(x[[nm]], "POSIXt")) {
      x[[nm]] <- as.character(x[[nm]])
    }
    if (is.factor(x[[nm]])) {
      x[[nm]] <- as.character(x[[nm]])
    }
    if (is.list(x[[nm]])) {
      x[[nm]] <- vapply(x[[nm]], function(v) paste(as.character(v), collapse = "; "), character(1))
    }
  }

  lapply(seq_len(nrow(x)), function(i) {
    as.list(x[i, , drop = FALSE])
  })
}

pt_ops_live_cnrfc_forecast_record_cols <- function() {
  c(
    "cnrfc_id",
    "display_name",
    "lat",
    "lon",
    "elev_ft",
    "forecast_point_hydro_type",
    "map_point_role",
    "map_popup_subtitle",
    "is_active_river_forecast",
    "is_active_reservoir_inflow",
    "is_active_reservoir_release",
    "has_active_ensemble_forecast",
    "has_water_supply_index",
    "water_supply_index_area",
    "water_supply_index_url",
    "cnrfc_source_url",
    "cnrfc_ensemble_url",
    "map_popup_note",
    "run_timestamp"
  )
}

pt_ops_live_cnrfc_precip_weather_record_cols <- function() {
  c(
    "cnrfc_id",
    "display_name",
    "lat",
    "lon",
    "elev_ft",
    "source_class",
    "raw_precip_source_code",
    "precip_weather_station_type_hint",
    "datatransmission_types",
    "station_authority_basis",
    "station_authority_note",
    "station_probe_scope",
    "wrh_site_id",
    "wrh_timeseries_url",
    "wrh_timeseries_tabular_url",
    "nws_api_latest_url",
    "nws_api_recent_url",
    "api_availability_class",
    "api_recent_availability_class",
    "noaa_station_data_availability_class",
    "noaa_timeseries_map_candidate",
    "availability_confidence",
    "noaa_api_verified",
    "noaa_api_checked_not_verified",
    "noaa_api_not_checked",
    "is_ops_timeseries_candidate",
    "is_ops_timeseries_verified",
    "is_ops_timeseries_checked_candidate",
    "is_ops_timeseries_unchecked_candidate",
    "has_current_wru_precip_product",
    "also_active_river_reservoir_forecast_point",
    "in_current_nwsid_snapshot",
    "in_cnrfc_river_reservoir_catalog",
    "map_station_role",
    "map_symbol_fill_class",
    "cnrfc_source_url",
    "cnrfc_observed_precip_map_url",
    "local_catalog_note",
    "ops_live_note",
    "map_popup_note",
    "run_timestamp"
  )
}


# ---- 4A. Hosted live-feed URL helper ----------------------------------------
##
## Most live Ops feeds use BRIM_LIVE_FEED_BASE_URL / brim_live_feed_url() from
## config_map_display.r. Keep a small fallback here so a missing config helper
## does not break the final map build while still honoring the centralized base
## URL whenever it exists.

pt_ops_live_default_feed_url <- function(path) {
  if (exists("brim_live_feed_url", mode = "function")) {
    return(brim_live_feed_url(path))
  }

  base <- if (exists("BRIM_LIVE_FEED_BASE_URL")) {
    get("BRIM_LIVE_FEED_BASE_URL")
  } else {
    "https://dbo99.github.io/brim-live-data-feeds"
  }

  paste0(
    sub("/+$", "", base),
    "/",
    sub("^/+", "", path)
  )
}

pt_add_ops_live_layers <- function(m, map_display, cnrfc_river_reservoir_forecast_points = NULL, cnrfc_precip_weather_stations = NULL, major_water_supply_basin_geometry = NULL) {

  if (!isTRUE(map_display$add_ops_live_layers)) {
    return(m)
  }

  js <- r"---(
function(el, x, data) {
  
  var map = this;
  var includeRadar = !!(data && data.includeRadar);
  var includeCdecReservoirStorage = !!(data && data.includeCdecReservoirStorage);
  var CDEC_RESERVOIR_STORAGE_URL = data && data.cdecReservoirStorageUrl ? String(data.cdecReservoirStorageUrl) : '';
  var CDEC_RESERVOIR_STORAGE_SUMMARY_URL = data && data.cdecReservoirSummaryUrl ? String(data.cdecReservoirSummaryUrl) : '';
  var includeCocorahsDailyPrecip = !!(data && data.includeCocorahsDailyPrecip);
  var COCORAHS_CA_DAILY_PRECIP_URL = data && data.cocorahsCaDailyPrecipUrl ? String(data.cocorahsCaDailyPrecipUrl) : '';
  var COCORAHS_CA_DAILY_PRECIP_SUMMARY_URL = data && data.cocorahsCaDailyPrecipSummaryUrl ? String(data.cocorahsCaDailyPrecipSummaryUrl) : '';
  var COCORAHS_CONUS_DAILY_PRECIP_URL = data && data.cocorahsConusDailyPrecipUrl ? String(data.cocorahsConusDailyPrecipUrl) : '';
  var COCORAHS_CONUS_DAILY_PRECIP_SUMMARY_URL = data && data.cocorahsConusDailyPrecipSummaryUrl ? String(data.cocorahsConusDailyPrecipSummaryUrl) : '';
  var includeUsgsStreamflowLatest = !!(data && data.includeUsgsStreamflowLatest);
  var USGS_STREAMFLOW_LATEST_URL = data && data.usgsStreamflowLatestUrl ? String(data.usgsStreamflowLatestUrl) : '';
  var USGS_STREAMFLOW_LATEST_SUMMARY_URL = data && data.usgsStreamflowLatestSummaryUrl ? String(data.usgsStreamflowLatestSummaryUrl) : '';
  var includeUsgsGroundwaterLatest = !!(data && data.includeUsgsGroundwaterLatest);
  var USGS_GROUNDWATER_LATEST_URL = data && data.usgsGroundwaterLatestUrl ? String(data.usgsGroundwaterLatestUrl) : '';
  var USGS_GROUNDWATER_LATEST_SUMMARY_URL = data && data.usgsGroundwaterLatestSummaryUrl ? String(data.usgsGroundwaterLatestSummaryUrl) : '';
  var includeScanSoilMoistureLatest = !!(data && data.includeScanSoilMoistureLatest);
  var SCAN_SOIL_MOISTURE_LATEST_URL = data && data.scanSoilMoistureLatestUrl ? String(data.scanSoilMoistureLatestUrl) : '';
  var SCAN_SOIL_MOISTURE_SUMMARY_URL = data && data.scanSoilMoistureSummaryUrl ? String(data.scanSoilMoistureSummaryUrl) : '';
  var SCAN_SOIL_MOISTURE_TRACE_URL = data && data.scanSoilMoistureTraceUrl ? String(data.scanSoilMoistureTraceUrl) : '';
  var SCAN_DEPTH_STYLE_URL = data && data.scanDepthStyleUrl ? String(data.scanDepthStyleUrl) : '';
  var SCAN_WATERDAY_PERCENTILES_URL = data && data.scanWaterdayPercentilesUrl ? String(data.scanWaterdayPercentilesUrl) : '';
  var SCAN_MONTHLY_CONTEXT_URL = data && data.scanMonthlyContextUrl ? String(data.scanMonthlyContextUrl) : '';
  var SCAN_PRIOR_WY_FALLBACK_TRACES_URL = data && data.scanPriorWyFallbackTracesUrl ? String(data.scanPriorWyFallbackTracesUrl) : '';
  var includeSnowPillowLatest = !!(data && data.includeSnowPillowLatest);
  var SNOW_PILLOW_LATEST_URL = data && data.snowPillowLatestUrl ? String(data.snowPillowLatestUrl) : '';
  var SNOW_PILLOW_SUMMARY_URL = data && data.snowPillowSummaryUrl ? String(data.snowPillowSummaryUrl) : '';
  var SNOW_PILLOW_TRACE_URL = data && data.snowPillowTraceUrl ? String(data.snowPillowTraceUrl) : '';
  var SNOW_PILLOW_WATERDAY_PERCENTILES_URL = data && data.snowPillowWaterdayPercentilesUrl ? String(data.snowPillowWaterdayPercentilesUrl) : '';
  var SNOW_PILLOW_MONTHLY_CONTEXT_URL = data && data.snowPillowMonthlyContextUrl ? String(data.snowPillowMonthlyContextUrl) : '';
  var SNOW_PILLOW_PRIOR_WY_FALLBACK_TRACES_URL = data && data.snowPillowPriorWyFallbackTracesUrl ? String(data.snowPillowPriorWyFallbackTracesUrl) : '';
  var includeDeltaOpsDailySummary = !!(data && data.includeDeltaOpsDailySummary);
  var DELTA_OPS_DAILY_SUMMARY_URL = data && data.deltaOpsDailySummaryUrl ? String(data.deltaOpsDailySummaryUrl) : '';
  var DELTA_OPS_DAILY_SUMMARY_SUMMARY_URL = data && data.deltaOpsDailySummarySummaryUrl ? String(data.deltaOpsDailySummarySummaryUrl) : '';
  var DELTA_OPS_X2_REFERENCE_URL = data && data.deltaOpsX2ReferenceUrl ? String(data.deltaOpsX2ReferenceUrl) : '';
  var includeCnrfcRiverReservoirForecastPoints = !!(data && data.includeCnrfcRiverReservoirForecastPoints);
  var CNRFC_RIVER_RESERVOIR_FORECAST_POINTS = data && data.cnrfcRiverReservoirForecastPoints ? data.cnrfcRiverReservoirForecastPoints : [];
  var includeCnrfcPrecipWeatherStations = !!(data && data.includeCnrfcPrecipWeatherStations);
  var CNRFC_PRECIP_WEATHER_STATIONS = data && data.cnrfcPrecipWeatherStations ? data.cnrfcPrecipWeatherStations : [];
  var includeMajorWaterSupplyBasinForecasts = !!(data && data.includeMajorWaterSupplyBasinForecasts);
  var MAJOR_WATER_SUPPLY_CNRFC_URL = data && data.majorWaterSupplyCnrfcUrl ? String(data.majorWaterSupplyCnrfcUrl) : '';
  var MAJOR_WATER_SUPPLY_CBRFC_URL = data && data.majorWaterSupplyCbrfcUrl ? String(data.majorWaterSupplyCbrfcUrl) : '';
  var MAJOR_WATER_SUPPLY_GEOMETRY = data && data.majorWaterSupplyGeometry ? data.majorWaterSupplyGeometry : {type:'FeatureCollection',features:[]};
  var MAJOR_WATER_SUPPLY_PRODUCT_MAPPING = data && data.majorWaterSupplyProductMapping ? data.majorWaterSupplyProductMapping : [];
  var MAJOR_WATER_SUPPLY_GEOMETRY_CATALOG = data && data.majorWaterSupplyGeometryCatalog ? data.majorWaterSupplyGeometryCatalog : [];
  var MAJOR_WATER_SUPPLY_COMPONENT_MANIFEST = data && data.majorWaterSupplyComponentManifest ? data.majorWaterSupplyComponentManifest : [];
  var MAJOR_WATER_SUPPLY_RESERVOIR_CROSSWALK = data && data.majorWaterSupplyReservoirCrosswalk ? data.majorWaterSupplyReservoirCrosswalk : [];
  var MAJOR_WATER_SUPPLY_RELATED_LINKS = data && data.majorWaterSupplyRelatedLinks ? data.majorWaterSupplyRelatedLinks : [];
  var OPS_CATALOG_PRIMARY_PANEL = data && data.opsCatalogPrimaryPanel ? data.opsCatalogPrimaryPanel : {};
  var includeGfsSurfaceWind = !!(data && data.includeGfsSurfaceWind);
  var GFS_SURFACE_WIND_MANIFEST_URL = data && data.gfsSurfaceWindManifestUrl ? String(data.gfsSurfaceWindManifestUrl) : '';
  var includeHrrrSurfaceWind = !!(data && data.includeHrrrSurfaceWind);
  var HRRR_SURFACE_WIND_MANIFEST_URL = data && data.hrrrSurfaceWindManifestUrl ? String(data.hrrrSurfaceWindManifestUrl) : '';
  var includeNbmWindGuidance = !!(data && data.includeNbmWindGuidance);
  var NBM_WIND_GUIDANCE_MANIFEST_URL = data && data.nbmWindGuidanceManifestUrl ? String(data.nbmWindGuidanceManifestUrl) : '';
  var includeAsosAwosWind = !!(data && data.includeAsosAwosWind);
  var ASOS_AWOS_WIND_URL = data && data.asosAwosWindUrl ? String(data.asosAwosWindUrl) : '';
  var ASOS_AWOS_WIND_SUMMARY_URL = data && data.asosAwosWindSummaryUrl ? String(data.asosAwosWindSummaryUrl) : '';
  var ASOS_AWOS_WIND_MANIFEST_URL = data && data.asosAwosWindManifestUrl ? String(data.asosAwosWindManifestUrl) : '';
  
__PT_OPS_LIVE_SHARED_HELPERS_JS____PT_OPS_LIVE_WEATHER_HAZARDS_HELPERS_JS____PT_OPS_LIVE_WPC_QPF_HOVER_HELPERS_JS____PT_OPS_LIVE_LEGEND_HELPERS_JS____PT_OPS_LIVE_RESERVOIR_HELPERS_JS__

__PT_OPS_LIVE_COCORAHS_HELPERS_JS__

__PT_OPS_LIVE_USGS_STREAMFLOW_HELPERS_JS__

__PT_OPS_LIVE_USGS_GROUNDWATER_HELPERS_JS__

__PT_OPS_LIVE_SCAN_HELPERS_JS__

__PT_OPS_LIVE_SNOW_PILLOW_HELPERS_JS__

__PT_OPS_LIVE_DELTA_OPS_HELPERS_JS__

__PT_OPS_LIVE_CNRFC_FORECAST_POINTS_HELPERS_JS__

__PT_OPS_LIVE_CNRFC_PRECIP_WEATHER_HELPERS_JS__

__PT_OPS_LIVE_MAJOR_WATER_SUPPLY_BASIN_HELPERS_JS__

__PT_OPS_LIVE_ARCGIS_EXPORT_HELPERS_JS____PT_OPS_LIVE_SERVICE_HELPERS_JS____PT_OPS_LIVE_LAYER_DEFINITION_HELPERS_JS__

__PT_OPS_LIVE_WIND_HELPERS_JS__

__PT_OPS_LIVE_HRRR_WIND_HELPERS_JS__

__PT_OPS_LIVE_NBM_WIND_HELPERS_JS__

__PT_OPS_LIVE_OBSERVED_WIND_HELPERS_JS__

__PT_OPS_LIVE_PANEL_HELPERS_JS__
}
)---"

  ops_live_js_helpers <- list(
    "__PT_OPS_LIVE_SHARED_HELPERS_JS__" = "pt_ops_live_shared_helpers_js",
    "__PT_OPS_LIVE_WEATHER_HAZARDS_HELPERS_JS__" = "pt_ops_live_weather_hazards_js",
    "__PT_OPS_LIVE_WPC_QPF_HOVER_HELPERS_JS__" = "pt_ops_live_wpc_qpf_hover_js",
    "__PT_OPS_LIVE_LEGEND_HELPERS_JS__" = "pt_ops_live_legend_helpers_js",
    "__PT_OPS_LIVE_RESERVOIR_HELPERS_JS__" = "pt_ops_live_cdec_reservoir_js",
    "__PT_OPS_LIVE_COCORAHS_HELPERS_JS__" = "pt_ops_live_cocorahs_js",
    "__PT_OPS_LIVE_USGS_STREAMFLOW_HELPERS_JS__" = "pt_ops_live_usgs_streamflow_js",
    "__PT_OPS_LIVE_USGS_GROUNDWATER_HELPERS_JS__" = "pt_ops_live_usgs_groundwater_js",
    "__PT_OPS_LIVE_SCAN_HELPERS_JS__" = "pt_ops_live_scan_js",
    "__PT_OPS_LIVE_SNOW_PILLOW_HELPERS_JS__" = "pt_ops_live_snow_pillow_js",
    "__PT_OPS_LIVE_DELTA_OPS_HELPERS_JS__" = "pt_ops_live_delta_ops_js",
    "__PT_OPS_LIVE_CNRFC_FORECAST_POINTS_HELPERS_JS__" = "pt_ops_live_cnrfc_forecast_points_js",
    "__PT_OPS_LIVE_CNRFC_PRECIP_WEATHER_HELPERS_JS__" = "pt_ops_live_cnrfc_precip_weather_js",
    "__PT_OPS_LIVE_MAJOR_WATER_SUPPLY_BASIN_HELPERS_JS__" = "pt_ops_live_major_water_supply_basin_js",
    "__PT_OPS_LIVE_ARCGIS_EXPORT_HELPERS_JS__" = "pt_ops_live_arcgis_export_js",
    "__PT_OPS_LIVE_SERVICE_HELPERS_JS__" = "pt_ops_live_service_helpers_js",
    "__PT_OPS_LIVE_LAYER_DEFINITION_HELPERS_JS__" = "pt_ops_live_layer_definition_js",
    "__PT_OPS_LIVE_WIND_HELPERS_JS__" = "pt_ops_live_wind_js",
    "__PT_OPS_LIVE_HRRR_WIND_HELPERS_JS__" = "pt_ops_live_hrrr_wind_js",
    "__PT_OPS_LIVE_NBM_WIND_HELPERS_JS__" = "pt_ops_live_nbm_wind_js",
    "__PT_OPS_LIVE_OBSERVED_WIND_HELPERS_JS__" = "pt_ops_live_observed_wind_js",
    "__PT_OPS_LIVE_PANEL_HELPERS_JS__" = "pt_ops_live_panel_helpers_js"
  )

  missing_js_helpers <- unname(ops_live_js_helpers[
    !vapply(
      ops_live_js_helpers,
      exists,
      logical(1),
      mode = "function"
    )
  ])

  if (length(missing_js_helpers) > 0) {
    stop(
      "Missing Ops Live JavaScript helper function(s): ",
      paste(missing_js_helpers, collapse = ", ")
    )
  }

  for (token in names(ops_live_js_helpers)) {
    js <- pt_ops_live_replace_js_token(
      js = js,
      token = token,
      replacement = do.call(ops_live_js_helpers[[token]], list())
    )
  }

  major_basin_payload <- if (
    isTRUE(map_display$add_ops_major_water_supply_basin_forecasts) &&
    inherits(major_water_supply_basin_geometry, "sf")
  ) {
    pt_ops_live_major_basin_payload(major_water_supply_basin_geometry)
  } else {
    list(
      geometry = list(type = "FeatureCollection", features = list()),
      productMapping = list(), geometryCatalog = list(), componentManifest = list(),
      reservoirCrosswalk = list(), relatedLinks = list()
    )
  }

  htmlwidgets::onRender(
    m,
    js,
    data = list(
      includeRadar = if (!is.null(map_display$add_ops_conus_radar)) {
        isTRUE(map_display$add_ops_conus_radar)
      } else {
        TRUE
      },
      includeCdecReservoirStorage = if (!is.null(map_display$add_ops_cdec_reservoir_storage)) {
        isTRUE(map_display$add_ops_cdec_reservoir_storage)
      } else {
        FALSE
      },
      cdecReservoirStorageUrl = if (!is.null(map_display$ops_cdec_reservoir_storage_url)) {
        map_display$ops_cdec_reservoir_storage_url
      } else {
        ""
      },
      cdecReservoirSummaryUrl = if (!is.null(map_display$ops_cdec_reservoir_storage_summary_url)) {
        map_display$ops_cdec_reservoir_storage_summary_url
      } else {
        ""
      },
      includeCocorahsDailyPrecip = if (!is.null(map_display$add_ops_cocorahs_daily_precip)) {
        isTRUE(map_display$add_ops_cocorahs_daily_precip)
      } else {
        FALSE
      },
      cocorahsCaDailyPrecipUrl = if (!is.null(map_display$ops_cocorahs_ca_daily_precip_url)) {
        map_display$ops_cocorahs_ca_daily_precip_url
      } else {
        ""
      },
      cocorahsCaDailyPrecipSummaryUrl = if (!is.null(map_display$ops_cocorahs_ca_daily_precip_summary_url)) {
        map_display$ops_cocorahs_ca_daily_precip_summary_url
      } else {
        ""
      },
      cocorahsConusDailyPrecipUrl = if (!is.null(map_display$ops_cocorahs_conus_daily_precip_url)) {
        map_display$ops_cocorahs_conus_daily_precip_url
      } else {
        ""
      },
      cocorahsConusDailyPrecipSummaryUrl = if (!is.null(map_display$ops_cocorahs_conus_daily_precip_summary_url)) {
        map_display$ops_cocorahs_conus_daily_precip_summary_url
      } else {
        ""
      },
      includeUsgsStreamflowLatest = if (!is.null(map_display$add_ops_usgs_streamflow_latest)) {
        isTRUE(map_display$add_ops_usgs_streamflow_latest)
      } else {
        FALSE
      },
      usgsStreamflowLatestUrl = if (!is.null(map_display$ops_usgs_streamflow_latest_url)) {
        map_display$ops_usgs_streamflow_latest_url
      } else {
        ""
      },
      usgsStreamflowLatestSummaryUrl = if (!is.null(map_display$ops_usgs_streamflow_latest_summary_url)) {
        map_display$ops_usgs_streamflow_latest_summary_url
      } else {
        ""
      },
      includeUsgsGroundwaterLatest = if (!is.null(map_display$add_ops_usgs_groundwater_latest)) {
        isTRUE(map_display$add_ops_usgs_groundwater_latest)
      } else {
        FALSE
      },
      usgsGroundwaterLatestUrl = if (!is.null(map_display$ops_usgs_groundwater_latest_url)) {
        map_display$ops_usgs_groundwater_latest_url
      } else {
        ""
      },
      usgsGroundwaterLatestSummaryUrl = if (!is.null(map_display$ops_usgs_groundwater_latest_summary_url)) {
        map_display$ops_usgs_groundwater_latest_summary_url
      } else {
        ""
      },
      includeScanSoilMoistureLatest = if (!is.null(map_display$add_ops_scan_soil_moisture_latest)) {
        isTRUE(map_display$add_ops_scan_soil_moisture_latest)
      } else {
        FALSE
      },
      scanSoilMoistureLatestUrl = if (!is.null(map_display$ops_scan_soil_moisture_latest_url)) {
        map_display$ops_scan_soil_moisture_latest_url
      } else {
        ""
      },
      scanSoilMoistureSummaryUrl = if (!is.null(map_display$ops_scan_soil_moisture_summary_url)) {
        map_display$ops_scan_soil_moisture_summary_url
      } else {
        ""
      },
      scanSoilMoistureTraceUrl = if (!is.null(map_display$ops_scan_soil_moisture_trace_url)) {
        map_display$ops_scan_soil_moisture_trace_url
      } else {
        ""
      },
      scanDepthStyleUrl = if (!is.null(map_display$ops_scan_depth_style_url)) {
        map_display$ops_scan_depth_style_url
      } else {
        ""
      },
      scanWaterdayPercentilesUrl = if (!is.null(map_display$ops_scan_waterday_percentiles_url)) {
        map_display$ops_scan_waterday_percentiles_url
      } else {
        ""
      },
      scanMonthlyContextUrl = if (!is.null(map_display$ops_scan_monthly_context_url)) {
        map_display$ops_scan_monthly_context_url
      } else {
        ""
      },
      scanPriorWyFallbackTracesUrl = if (!is.null(map_display$ops_scan_prior_wy_fallback_traces_url)) {
        map_display$ops_scan_prior_wy_fallback_traces_url
      } else {
        ""
      },
      includeSnowPillowLatest = if (!is.null(map_display$add_ops_snow_pillow_latest)) {
        isTRUE(map_display$add_ops_snow_pillow_latest)
      } else {
        FALSE
      },
      snowPillowLatestUrl = if (!is.null(map_display$ops_snow_pillow_latest_url)) {
        map_display$ops_snow_pillow_latest_url
      } else {
        ""
      },
      snowPillowSummaryUrl = if (!is.null(map_display$ops_snow_pillow_summary_url)) {
        map_display$ops_snow_pillow_summary_url
      } else {
        ""
      },
      snowPillowTraceUrl = if (!is.null(map_display$ops_snow_pillow_trace_url)) {
        map_display$ops_snow_pillow_trace_url
      } else {
        ""
      },
      snowPillowWaterdayPercentilesUrl = if (!is.null(map_display$ops_snow_pillow_waterday_percentiles_url)) {
        map_display$ops_snow_pillow_waterday_percentiles_url
      } else {
        ""
      },
      snowPillowMonthlyContextUrl = if (!is.null(map_display$ops_snow_pillow_monthly_context_url)) {
        map_display$ops_snow_pillow_monthly_context_url
      } else {
        ""
      },
      snowPillowPriorWyFallbackTracesUrl = if (!is.null(map_display$ops_snow_pillow_prior_wy_fallback_traces_url)) {
        map_display$ops_snow_pillow_prior_wy_fallback_traces_url
      } else {
        ""
      },
      includeDeltaOpsDailySummary = if (!is.null(map_display$add_ops_delta_ops_daily_summary)) {
        isTRUE(map_display$add_ops_delta_ops_daily_summary)
      } else {
        FALSE
      },
      deltaOpsDailySummaryUrl = if (!is.null(map_display$ops_delta_ops_daily_summary_url)) {
        map_display$ops_delta_ops_daily_summary_url
      } else {
        ""
      },
      deltaOpsDailySummarySummaryUrl = if (!is.null(map_display$ops_delta_ops_daily_summary_summary_url)) {
        map_display$ops_delta_ops_daily_summary_summary_url
      } else {
        ""
      },
      deltaOpsX2ReferenceUrl = if (!is.null(map_display$ops_delta_ops_x2_reference_url)) {
        map_display$ops_delta_ops_x2_reference_url
      } else {
        ""
      },
      includeCnrfcRiverReservoirForecastPoints = if (!is.null(map_display$add_ops_cnrfc_river_reservoir_forecast_points)) {
        isTRUE(map_display$add_ops_cnrfc_river_reservoir_forecast_points)
      } else {
        FALSE
      },
      cnrfcRiverReservoirForecastPoints = pt_ops_live_records_as_row_list(
        cnrfc_river_reservoir_forecast_points,
        pt_ops_live_cnrfc_forecast_record_cols()
      ),
      includeCnrfcPrecipWeatherStations = if (!is.null(map_display$add_ops_cnrfc_precip_weather_stations)) {
        isTRUE(map_display$add_ops_cnrfc_precip_weather_stations)
      } else {
        FALSE
      },
      cnrfcPrecipWeatherStations = pt_ops_live_records_as_row_list(
        cnrfc_precip_weather_stations,
        pt_ops_live_cnrfc_precip_weather_record_cols()
      ),
      includeMajorWaterSupplyBasinForecasts = isTRUE(
        map_display$add_ops_major_water_supply_basin_forecasts
      ) && length(major_basin_payload$geometry$features) == 23L,
      majorWaterSupplyCnrfcUrl = if (!is.null(map_display$ops_major_water_supply_cnrfc_url)) {
        map_display$ops_major_water_supply_cnrfc_url
      } else {
        pt_ops_live_default_feed_url("data/major_water_supply_basin_forecasts.json")
      },
      majorWaterSupplyCbrfcUrl = if (!is.null(map_display$ops_major_water_supply_cbrfc_url)) {
        map_display$ops_major_water_supply_cbrfc_url
      } else {
        pt_ops_live_default_feed_url("data/cbrfc_major_water_supply_forecasts.json")
      },
      majorWaterSupplyGeometry = major_basin_payload$geometry,
      majorWaterSupplyProductMapping = major_basin_payload$productMapping,
      majorWaterSupplyGeometryCatalog = major_basin_payload$geometryCatalog,
      majorWaterSupplyComponentManifest = major_basin_payload$componentManifest,
      majorWaterSupplyReservoirCrosswalk = major_basin_payload$reservoirCrosswalk,
      majorWaterSupplyRelatedLinks = major_basin_payload$relatedLinks,
      includeGfsSurfaceWind = if (!is.null(map_display$add_ops_gfs_surface_wind)) {
        isTRUE(map_display$add_ops_gfs_surface_wind)
      } else {
        TRUE
      },
      gfsSurfaceWindManifestUrl = if (!is.null(map_display$ops_gfs_surface_wind_manifest_url)) {
        map_display$ops_gfs_surface_wind_manifest_url
      } else {
        pt_ops_live_default_feed_url("data/wind/gfs_surface_wind_feed_manifest.json")
      },
      includeHrrrSurfaceWind = if (!is.null(map_display$add_ops_hrrr_surface_wind)) {
        isTRUE(map_display$add_ops_hrrr_surface_wind)
      } else {
        TRUE
      },
      hrrrSurfaceWindManifestUrl = if (!is.null(map_display$ops_hrrr_surface_wind_manifest_url)) {
        map_display$ops_hrrr_surface_wind_manifest_url
      } else {
        pt_ops_live_default_feed_url("data/wind/hrrr_surface_wind_feed_manifest.json")
      },
      includeNbmWindGuidance = if (!is.null(map_display$add_ops_nbm_wind_guidance)) {
        isTRUE(map_display$add_ops_nbm_wind_guidance)
      } else {
        TRUE
      },
      nbmWindGuidanceManifestUrl = if (!is.null(map_display$ops_nbm_wind_guidance_manifest_url)) {
        map_display$ops_nbm_wind_guidance_manifest_url
      } else {
        pt_ops_live_default_feed_url("data/wind/nbm_wind_guidance_feed_manifest.json")
      },
      includeAsosAwosWind = if (!is.null(map_display$add_ops_asos_awos_wind)) {
        isTRUE(map_display$add_ops_asos_awos_wind)
      } else {
        TRUE
      },
      asosAwosWindUrl = if (!is.null(map_display$ops_asos_awos_wind_url)) {
        map_display$ops_asos_awos_wind_url
      } else {
        pt_ops_live_default_feed_url("data/wind/asos_awos_wind_latest.geojson")
      },
      asosAwosWindSummaryUrl = if (!is.null(map_display$ops_asos_awos_wind_summary_url)) {
        map_display$ops_asos_awos_wind_summary_url
      } else {
        pt_ops_live_default_feed_url("data/wind/asos_awos_wind_latest_summary.json")
      },
      asosAwosWindManifestUrl = if (!is.null(map_display$ops_asos_awos_wind_manifest_url)) {
        map_display$ops_asos_awos_wind_manifest_url
      } else {
        pt_ops_live_default_feed_url("data/wind/asos_awos_wind_feed_manifest.json")
      },
      opsCatalogPrimaryPanel = pt_ops_live_catalog_primary_panel_map()
    )
  )
}
