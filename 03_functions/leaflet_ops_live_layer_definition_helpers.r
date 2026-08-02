# ==== leaflet_ops_live_layer_definition_helpers.r ==========================
##
## PURPOSE:
##   Ops Live layer definitions and panel-row registration.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r`.
##   It returns browser-side JavaScript as text for injection into the Ops Live
##   htmlwidgets/onRender function. Keep edits narrow and feature-specific.
## ============================================================================

pt_ops_live_layer_definition_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Layer definitions
  // --------------------------------------------------------------------------
  var opsLayers = [];
  
  function ptOpsNormalizePrimaryPanel(value) {
    var panel = String(value || '')
      .trim()
      .toLowerCase()
      .replace(/[_\s-]+/g, '_');

    if (panel === 'ops') return 'ops_live';
    if (panel === 'external' || panel === 'ops_live' || panel === 'both' || panel === 'disabled') return panel;
    return 'external';
  }

  function ptOpsCatalogPrimaryPanel(sourceDisplayName) {
    sourceDisplayName = String(sourceDisplayName || '').trim();
    if (!sourceDisplayName || !OPS_CATALOG_PRIMARY_PANEL) return '';

    if (Object.prototype.hasOwnProperty.call(OPS_CATALOG_PRIMARY_PANEL, sourceDisplayName)) {
      return ptOpsNormalizePrimaryPanel(OPS_CATALOG_PRIMARY_PANEL[sourceDisplayName]);
    }

    return '';
  }

  function ptOpsCatalogSourceDisabled(sourceDisplayName) {
    return ptOpsCatalogPrimaryPanel(sourceDisplayName) === 'disabled';
  }

  function addOpsLayer(def) {
    if (def && def.catalogSourceDisplayName && ptOpsCatalogSourceDisabled(def.catalogSourceDisplayName)) {
      return;
    }
    opsLayers.push(def);
  }

  // --------------------------------------------------------------------------
  // Ops Live wrapper for selected curated External Layers catalog rows
  // --------------------------------------------------------------------------
  var ptOpsPromotedSlowTimers = {};

  function ptOpsClearPromotedSlowTimers(name) {
    name = String(name || '');
    var timers = ptOpsPromotedSlowTimers[name] || [];
    timers.forEach(function(timerId) {
      try { window.clearTimeout(timerId); } catch(e) {}
    });
    ptOpsPromotedSlowTimers[name] = [];
  }

  function ptOpsSchedulePromotedSlowStatus(name, message, delayMs) {
    name = String(name || '');
    if (!name || !delayMs) return;

    if (!ptOpsPromotedSlowTimers[name]) ptOpsPromotedSlowTimers[name] = [];

    var timerId = window.setTimeout(function() {
      // Do not resurrect warnings after the layer has loaded or has been
      // turned off.  The row-level loading class is the source of truth here.
      if (typeof isOpsLayerLoading === 'function' && !isOpsLayerLoading(name)) return;
      recordStatus(name, message, 'pt-ops-warn');
    }, delayMs);

    ptOpsPromotedSlowTimers[name].push(timerId);
  }

  var CatalogPromotedExternalLayer = L.Layer.extend({
    initialize: function(options) {
      this.options = options || {};
      this._map = null;
      this._isRemoved = true;
    },
    onAdd: function(mapObj) {
      this._map = mapObj;
      this._isRemoved = false;

      var name = this.options.name || 'Promoted External Layer';
      var sourceName = this.options.sourceDisplayName || name;
      var opsKey = this.options.opsKey || name;

      if (ptOpsCatalogSourceDisabled(sourceName)) {
        recordStatus(name, 'Catalog row is disabled for this map build: ' + sourceName, 'pt-ops-muted');
        setOpsLayerLoading(name, false);
        return;
      }

      activeLegendDefs[name] = {
        note: this.options.note || '',
        legendType: this.options.legendType || null,
        sourceUrl: this.options.sourceUrl || '',
        legendUrl: this.options.legendUrl || '',
        infoUrl: this.options.infoUrl || '',
        infoLabel: this.options.infoLabel || ''
      };
      redrawLegend();
      setOpsLayerLoading(name, true);
      recordStatus(name, 'Requesting curated External Layers catalog overlay…', 'pt-ops-warn');
      ptOpsClearPromotedSlowTimers(name);
      ptOpsSchedulePromotedSlowStatus(
        name,
        'Still querying current map view. Large public FeatureServer layers can take 30–60 seconds; zoom in and retry if no layer appears.',
        18000
      );
      ptOpsSchedulePromotedSlowStatus(
        name,
        'Still waiting on the public service. If this remains blank, use Clear ops, zoom farther in, and try again.',
        45000
      );

      var self = this;
      function tryAdd(attempt) {
        if (self._isRemoved) return;

        var bridge = window.ptOpsExternalCatalogBridge;
        if (!bridge || typeof bridge.addLayer !== 'function') {
          if (attempt < 30) {
            window.setTimeout(function() { tryAdd(attempt + 1); }, 200);
          } else {
            recordStatus(
              name,
              'External Layers catalog bridge was not available. Confirm the Tools / Add Data panel is enabled.',
              'pt-ops-warn'
            );
            setOpsLayerLoading(name, false);
          }
          return;
        }

        var result = bridge.addLayer({
          sourceDisplayName: sourceName,
          opsDisplayName: name,
          opsKey: opsKey,
          color: self.options.color || '#2C7FB8'
        });

        if (result && result.ok) {
          recordStatus(name, result.message || 'Requested curated catalog overlay; waiting for current-view query…', 'pt-ops-warn');
          setOpsLayerLoading(name, true);
        } else {
          recordStatus(name, (result && result.message) ? result.message : 'Could not request curated catalog overlay.', 'pt-ops-warn');
          setOpsLayerLoading(name, false);
        }
      }

      tryAdd(0);
    },
    onRemove: function(mapObj) {
      this._isRemoved = true;
      var name = this.options.name || 'Promoted External Layer';
      var opsKey = this.options.opsKey || name;

      var bridge = window.ptOpsExternalCatalogBridge;
      if (bridge && typeof bridge.removeLayer === 'function') {
        try {
          bridge.removeLayer({ opsKey: opsKey, opsDisplayName: name });
        } catch(e) {}
      }

      delete activeLegendDefs[name];
      setOpsLayerLoading(name, false);
      ptOpsClearPromotedSlowTimers(name);
      redrawLegend();
      recordStatus(name, 'Layer turned off.', 'pt-ops-muted');
      this._map = null;
    },
    forceRemove: function(mapObj) {
      this.onRemove(mapObj || this._map);
    }
  });

  // Receive async status from the External Layers loader for Ops-promoted
  // current-view catalog rows.  The bridge request returns immediately, but
  // FeatureServer/MapServer queries can take many seconds; keep the Ops row
  // spinner active until the real query reports a terminal success/failure.
  if (!window.ptOpsCatalogLayerStatusListenerInstalled) {
    window.ptOpsCatalogLayerStatusListenerInstalled = true;
    window.addEventListener('ptOpsCatalogLayerStatus', function(evt) {
      var detail = evt && evt.detail ? evt.detail : {};
      var name = detail.opsDisplayName || detail.name || detail.opsName || '';
      if (!name) return;

      if (detail.msg) {
        recordStatus(
          name,
          detail.msg,
          detail.isError ? 'pt-ops-warn' : (detail.terminal ? 'pt-ops-ok' : 'pt-ops-warn')
        );
      }

      if (detail.terminal) {
        ptOpsClearPromotedSlowTimers(name);
      }

      setOpsLayerLoading(name, !detail.terminal);
    });
  }
  

  // --------------------------------------------------------------------------
  // AirNow and smoke layers moved to External Layers.
  // --------------------------------------------------------------------------
  // These direct public FeatureServer layers are still cataloged, refreshable,
  // and styled in External Layers under Air / Smoke > Smoke / air quality.
  // Keeping them out of Ops Live helps the Ops panel stay focused on hydro
  // operations while preserving current-view refresh behavior in External.

  // --------------------------------------------------------------------------
  // Ops Live wrappers for operational camera/fire catalog rows
  // --------------------------------------------------------------------------
  addOpsLayer({
    category: 'Cameras',
    subgroup: '',
    name: 'ALERTCalifornia Cameras',
    sourceUrl: 'https://services8.arcgis.com/X84q166Srnyl4JMV/ArcGIS/rest/services/ALERTCalifornia_Camera_Feed/FeatureServer/0',
    layer: new CatalogPromotedExternalLayer({
      name: 'ALERTCalifornia Cameras',
      catalogSourceDisplayName: 'ALERTCalifornia Cameras',
      sourceDisplayName: 'ALERTCalifornia Cameras',
      opsKey: 'ops_alertcalifornia_cameras',
      color: '#2C7FB8',
      sourceUrl: 'https://services8.arcgis.com/X84q166Srnyl4JMV/ArcGIS/rest/services/ALERTCalifornia_Camera_Feed/FeatureServer/0',
      note: 'One-click Ops Live view of the curated External Layers row "ALERTCalifornia Cameras". Reuses the existing BRIM current-view query, directional camera-arrow styling, online/offline status, hover fields, popup fields, image thumbnails when available, and ALERTCalifornia viewer links. Useful visual-observation context for snowlines, smoke, clouds, visibility, weather, and fire operations.'
    })
  });

  addOpsLayer({
    category: 'Cameras',
    subgroup: '',
    name: 'ALERTCalifornia Camera Viewsheds',
    sourceUrl: 'https://services8.arcgis.com/X84q166Srnyl4JMV/ArcGIS/rest/services/ALERTCalifornia_Camera_Feed/FeatureServer/1',
    layer: new CatalogPromotedExternalLayer({
      name: 'ALERTCalifornia Camera Viewsheds',
      catalogSourceDisplayName: 'ALERTCalifornia Camera Viewsheds',
      sourceDisplayName: 'ALERTCalifornia Camera Viewsheds',
      opsKey: 'ops_alertcalifornia_camera_viewsheds',
      color: '#31B6C9',
      sourceUrl: 'https://services8.arcgis.com/X84q166Srnyl4JMV/ArcGIS/rest/services/ALERTCalifornia_Camera_Feed/FeatureServer/1',
      note: 'One-click Ops Live view of the curated External Layers row "ALERTCalifornia Camera Viewsheds". Reuses the existing BRIM current-view query, translucent viewshed styling, hover fields, popup fields, and ALERTCalifornia viewer links. Pair with camera points for quick camera-coverage and line-of-sight context; viewsheds are operational screening polygons, not exact field visibility limits.'
    })
  });

  addOpsExternalLinks({
    category: 'Cameras',
    subgroup: '',
    title: 'USGS real-time river cameras',
    note: 'External camera viewer; not a BRIM map layer.',
    links: [
      {label: 'USGS HIVIS', url: 'https://apps.usgs.gov/hivis', title: 'Open USGS HIVIS real-time river camera viewer'}
    ]
  });


  if (includeRadar) {
    addOpsLayer({
      category: 'Hydro Observations',
      subgroup: 'Radar',
      name: 'Radar | IEM NEXRAD',
      legendUrl: 'https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetLegendGraphic&LAYER=nexrad-n0q-900913&FORMAT=image/png',
      sourceUrl: 'https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi',
      layer: makeRadarLayer({
        name: 'Radar | IEM NEXRAD',
        opacity: 0.70,
        note: 'Live NEXRAD radar tile overlay from the Iowa Environmental Mesonet WMS. Useful as a fast alternate radar source.',
        sourceUrl: 'https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi',
        legendUrl: 'https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetLegendGraphic&LAYER=nexrad-n0q-900913&FORMAT=image/png',
      })
    });

    addOpsLayer({
      category: 'Hydro Observations',
      subgroup: 'Radar',
      name: 'Radar | NOAA MRMS',
      layer: new ArcGISExportLayer({
        name: 'Radar | NOAA MRMS',
        serviceType: 'MapServer',
        url: NOAA_RADAR,
        layers: [3],
        opacity: 0.70,
        note: 'Official NOAA/NWS MRMS radar base-reflectivity image. Coverage includes CONUS, Alaska, Hawaii, Guam, the Caribbean, and nearby Canada.',
        legendUrl: mapServerLegendUrl(NOAA_RADAR),
        checkFreshness: checkNoaaRadar
      })
    });
  }

  addOpsLayer({
    category: 'Satellite / Imagery',
    name: 'NOAA GOES GeoColor',
    layer: new ArcGISExportLayer({
      name: 'NOAA GOES GeoColor',
      serviceType: 'ImageServer',
      url: NOAA_GOES_GEOCOLOR,
      opacity: 0.82,
      note: 'Merged GOES East/West GeoColor imagery. Good quick-look layer for cloud cover, smoke/dust/haze, snow, and broad situational awareness; source notes display use, not operational decision support. Works best with few other overlays on.',
      infoUrl: INFO_NOAA_SATELLITE_MAPS,
      infoLabel: 'guide',
      legendNote: 'GeoColor is a visual satellite composite; it does not have a simple quantitative color legend.',
      checkFreshness: checkNoaaSatellite('NOAA GOES GeoColor')
    })
  });

  addOpsLayer({
    category: 'Satellite / Imagery',
    name: 'NOAA GOES Infrared',
    layer: new ArcGISExportLayer({
      name: 'NOAA GOES Infrared',
      serviceType: 'ImageServer',
      url: NOAA_GOES_IR,
      opacity: 0.82,
      note: 'GOES ABI Band 13 infrared imagery. Useful day or night for cloud-top temperature patterns and active weather systems; source notes display use, not operational decision support.',
      infoUrl: INFO_GOES_IR_BAND13,
      infoLabel: 'guide',
      legendNote: 'Enhanced infrared colors are brightness-temperature/cloud-top context, not surface air temperature.',
      checkFreshness: checkNoaaSatellite('NOAA GOES Infrared')
    })
  });

  addOpsLayer({
    category: 'Satellite / Imagery',
    name: 'NOAA GOES Water Vapor',
    layer: new ArcGISExportLayer({
      name: 'NOAA GOES Water Vapor',
      serviceType: 'ImageServer',
      url: NOAA_GOES_WV,
      opacity: 0.82,
      note: 'GOES ABI Band 10 water-vapor imagery. Useful for atmospheric-river context, moisture plumes, dry slots, and upper-level circulation patterns; source notes display use, not operational decision support.',
      infoUrl: INFO_GOES_WV_BAND10,
      infoLabel: 'guide',
      legendNote: 'Water-vapor colors show enhanced brightness-temperature/moisture-structure patterns aloft; the green/brown ramp is not a direct surface humidity scale.',
      checkFreshness: checkNoaaSatellite('NOAA GOES Water Vapor')
    })
  });

  addOpsLayer({
    category: 'Satellite / Imagery',
    name: 'NASA MODIS Terra True Color',
    layer: makeGibsWmtsLayer({
      name: 'NASA MODIS Terra True Color',
      layerId: 'MODIS_Terra_CorrectedReflectance_TrueColor',
      opacity: 0.78,
      sourceUrl: 'https://gibs.earthdata.nasa.gov/wmts/epsg3857/best/MODIS_Terra_CorrectedReflectance_TrueColor/',
      infoUrl: INFO_NASA_GIBS,
      infoLabel: 'guide',
      legendNote: 'Daily true-color imagery; black or missing tiles usually mean no current/default tile was available for that location/zoom/date.',
      note: 'Daily NASA GIBS MODIS Terra corrected-reflectance true-color imagery. Useful for daytime snow, smoke, dust, clouds, and landscape context; not a same-minute live product.'
    })
  });


  addOpsLayer({
    category: 'Hydro Observations',
    subgroup: 'Flows / levels / snow / moisture / etc',
    name: 'Streamflow | multi-agency | Nat\'l',
    sourceUrl: 'https://services9.arcgis.com/RHVPKKiFTONKtxq3/arcgis/rest/services/Live_Stream_Gauges_v1/FeatureServer/0',
    infoUrl: 'https://services9.arcgis.com/RHVPKKiFTONKtxq3/arcgis/rest/services/Live_Stream_Gauges_v1/FeatureServer/0',
    infoLabel: 'service',
    layer: new CatalogPromotedExternalLayer({
      name: 'Streamflow | multi-agency | Nat\'l',
    catalogSourceDisplayName: 'Live Stream Gages / Flow',
      sourceDisplayName: 'Live Stream Gages / Flow',
      opsKey: 'ops_live_agency_streamflow_gages',
      color: '#2C7FB8',
      legendType: 'multi_agency_streamflow',
      sourceUrl: 'https://services9.arcgis.com/RHVPKKiFTONKtxq3/arcgis/rest/services/Live_Stream_Gauges_v1/FeatureServer/0',
      note: 'One-click Ops Live view of the curated External Layers row "Live Stream Gages / Flow". This reuses the existing BRIM current-view query, updated stream/reservoir/questionable symbology, hover fields, popup fields, and hydrograph links. Zoom/pan to the area of interest before turning it on; use External Layers for SQL filtering or manual refresh controls.'
    })
  });
  



  if (includeDeltaOpsDailySummary && DELTA_OPS_DAILY_SUMMARY_URL) {
    addOpsLayer({
      category: 'Hydro Observations',
      subgroup: 'Delta operations',
      name: 'Delta ops snapshot | CVP/SWP',
      sourceUrl: 'https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/State-Water-Project/Operations-And-Maintenance/Files/Operations-Control-Office/Delta-Status-And-Operations/Delta-Operations-Daily-Summary.pdf',
      infoUrl: DELTA_OPS_DAILY_SUMMARY_SUMMARY_URL || DELTA_OPS_DAILY_SUMMARY_URL,
      infoLabel: DELTA_OPS_DAILY_SUMMARY_SUMMARY_URL ? 'summary' : 'GeoJSON',
      refreshable: true,
      extraRowHtml: '<label class=\"pt-ops-row-mini-toggle\" title=\"Show/hide Delta Ops labels\"><input type=\"checkbox\" data-pt-ops-action=\"delta-labels\" checked>lbl</label><a href=\"#\" class=\"pt-ops-row-mini-action\" data-pt-ops-action=\"delta-ops-zoom\" title=\"Zoom to Delta Ops snapshot extent\">z</a>',
      helperText: 'Daily DWR Delta Ops snapshot: exports, gates, outflow, OMR, X2, Delta status/control, East Side Streams estimate, and San Luis split. Values are preliminary.',
      layer: new DeltaOpsDailySummaryLayer({
        name: 'Delta ops snapshot | CVP/SWP',
        url: DELTA_OPS_DAILY_SUMMARY_URL,
        summaryUrl: DELTA_OPS_DAILY_SUMMARY_SUMMARY_URL,
        x2ReferenceUrl: DELTA_OPS_X2_REFERENCE_URL,
        sourceUrl: 'https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/State-Water-Project/Operations-And-Maintenance/Files/Operations-Control-Office/Delta-Status-And-Operations/Delta-Operations-Daily-Summary.pdf',
        note: 'DWR Delta Operations Daily Summary, parsed daily by the BRIM live-feed workflow. Displays a presentation-style Delta/CVP/SWP operating snapshot. Preliminary data; subject to revision without notice.',
        zoomOnAdd: true
      })
    });
  }
  if (includeUsgsStreamflowLatest && USGS_STREAMFLOW_LATEST_URL) {
    addOpsLayer({
      category: 'Hydro Observations',
      subgroup: 'Flows / levels / snow / moisture / etc',
      name: 'Streamflow | USGS | Ca',
      sourceUrl: USGS_STREAMFLOW_LATEST_URL,
      infoUrl: USGS_STREAMFLOW_LATEST_SUMMARY_URL || USGS_STREAMFLOW_LATEST_URL,
      infoLabel: USGS_STREAMFLOW_LATEST_SUMMARY_URL ? 'summary' : 'GeoJSON',
      layer: makeUsgsStreamflowLatestLayer({
        name: 'Streamflow | USGS | Ca',
        url: USGS_STREAMFLOW_LATEST_URL,
        summaryUrl: USGS_STREAMFLOW_LATEST_SUMMARY_URL,
        sourceUrl: USGS_STREAMFLOW_LATEST_URL,
        note: 'BRIM-hosted latest USGS streamflow layer for California. The scheduled BRIM feed retrieves latest continuous discharge and stage from the USGS Water Data API. This Ops layer draws only sites with latest discharge or stage; no-current/historical gages remain available in the static USGS streamgage layer.'
      })
    });
  }

  if (includeUsgsGroundwaterLatest && USGS_GROUNDWATER_LATEST_URL) {
    addOpsLayer({
      category: 'Hydro Observations',
      subgroup: 'Flows / levels / snow / moisture / etc',
      name: 'Groundwater | USGS | Ca/wrnNv/srnOr',
      sourceUrl: USGS_GROUNDWATER_LATEST_URL,
      infoUrl: USGS_GROUNDWATER_LATEST_SUMMARY_URL || USGS_GROUNDWATER_LATEST_URL,
      infoLabel: USGS_GROUNDWATER_LATEST_SUMMARY_URL ? 'summary' : 'GeoJSON',
      layer: makeUsgsGroundwaterLatestLayer({
        name: 'Groundwater | USGS | Ca/wrnNv/srnOr',
        url: USGS_GROUNDWATER_LATEST_URL,
        summaryUrl: USGS_GROUNDWATER_LATEST_SUMMARY_URL,
        sourceUrl: USGS_GROUNDWATER_LATEST_URL,
        note: 'BRIM-hosted latest USGS groundwater-level layer for California active/recent candidate wells. The scheduled BRIM feed retrieves field-measurement depth-to-water values from the USGS Water Data API and joins them to the BRIM well metadata index. Values are screening context only and should be interpreted with well construction, aquifer, datum, and measurement frequency.'
      })
    });
  }


  if (includeScanSoilMoistureLatest && SCAN_SOIL_MOISTURE_LATEST_URL) {
    addOpsLayer({
      category: 'Hydro Observations',
      subgroup: 'Flows / levels / snow / moisture / etc',
      name: 'Soil moisture | USDA NRCS SCAN | Ca/Nv',
      sourceUrl: SCAN_SOIL_MOISTURE_LATEST_URL,
      infoUrl: SCAN_SOIL_MOISTURE_SUMMARY_URL || SCAN_SOIL_MOISTURE_LATEST_URL,
      infoLabel: SCAN_SOIL_MOISTURE_SUMMARY_URL ? 'summary' : 'GeoJSON',
      layer: makeScanSoilMoistureLayer({
        name: 'Soil moisture | USDA NRCS SCAN | Ca/Nv',
        latestUrl: SCAN_SOIL_MOISTURE_LATEST_URL,
        summaryUrl: SCAN_SOIL_MOISTURE_SUMMARY_URL,
        traceUrl: SCAN_SOIL_MOISTURE_TRACE_URL,
        depthStyleUrl: SCAN_DEPTH_STYLE_URL,
        waterdayPercentilesUrl: SCAN_WATERDAY_PERCENTILES_URL,
        monthlyContextUrl: SCAN_MONTHLY_CONTEXT_URL,
        priorWyFallbackTracesUrl: SCAN_PRIOR_WY_FALLBACK_TRACES_URL,
        sourceUrl: SCAN_SOIL_MOISTURE_LATEST_URL,
        note: 'BRIM-hosted NRCS SCAN soil-moisture layer. Point colors show latest soil moisture at the selected depth relative to each station/depth historical context. Popups include depth tabs, current-water-year daily context, and recent monthly context. Values are screening context and should be checked against the official NRCS station page for decisions.'
      })
    });
  }


  if (includeSnowPillowLatest && SNOW_PILLOW_LATEST_URL) {
    addOpsLayer({
      category: 'Hydro Observations',
      subgroup: 'Flows / levels / snow / moisture / etc',
      name: 'Snow pillow SWE | CDEC / USDA NRCS | Ca/Nv/Or',
      sourceUrl: SNOW_PILLOW_LATEST_URL,
      infoUrl: SNOW_PILLOW_SUMMARY_URL || SNOW_PILLOW_LATEST_URL,
      infoLabel: SNOW_PILLOW_SUMMARY_URL ? 'summary' : 'GeoJSON',
      layer: makeSnowPillowLatestLayer({
        name: 'Snow pillow SWE | CDEC / USDA NRCS | Ca/Nv/Or',
        latestUrl: SNOW_PILLOW_LATEST_URL,
        summaryUrl: SNOW_PILLOW_SUMMARY_URL,
        traceUrl: SNOW_PILLOW_TRACE_URL,
        waterdayPercentilesUrl: SNOW_PILLOW_WATERDAY_PERCENTILES_URL,
        monthlyContextUrl: SNOW_PILLOW_MONTHLY_CONTEXT_URL,
        sourceUrl: SNOW_PILLOW_LATEST_URL,
        note: 'BRIM-hosted snow-pillow / SWE layer. Point colors show current SWE relative to each station historical context only when the latest SWE value is fresh/current. Popups include latest SWE, data-status notes, current-water-year daily context, and recent monthly context. Values are screening context and should be checked against official NRCS/CDEC source pages for decisions.'
      })
    });
  }

  // Temporarily hide NWS Surface Wind Barbs from Ops Live. The current
  // public MapServer export is a poor performer; keep the definition nearby
  // for possible re-enable or replacement when a better wind service is chosen.
  var includeSurfaceWindBarbs = false;
  if (includeSurfaceWindBarbs) {
    addOpsLayer({
      category: 'Hydro Observations',
      subgroup: 'Flows / levels / snow / moisture / etc',
      name: 'NWS Surface Wind Barbs',
      layer: new ArcGISExportLayer({
        name: 'NWS Surface Wind Barbs',
        serviceType: 'MapServer',
        url: SURFACE_OBS,
        layers: [210,220,230,240,250,260],
        opacity: 0.88,
        note: 'Near-real-time NWS/MADIS wind-barb observations. The source service uses scale bands so additional observations appear as users zoom in.',
        legendUrl: mapServerLegendUrl(SURFACE_OBS),
        checkFreshness: checkSurfaceObs
      })
    });
  }




  if (includeCnrfcRiverReservoirForecastPoints && CNRFC_RIVER_RESERVOIR_FORECAST_POINTS && CNRFC_RIVER_RESERVOIR_FORECAST_POINTS.length) {
    addOpsLayer({
      category: 'Forecasts / Outlooks',
      subgroup: 'NWS / CNRFC / WRH sites & product links',
      name: 'CNRFC forecast points | river/reservoir',
      sourceUrl: 'https://www.cnrfc.noaa.gov/',
      infoUrl: 'https://www.cnrfc.noaa.gov/',
      infoLabel: 'CNRFC',
      layer: new CnrfcRiverReservoirForecastLayer({
        name: 'CNRFC forecast points | river/reservoir',
        records: CNRFC_RIVER_RESERVOIR_FORECAST_POINTS,
        sourceUrl: 'https://www.cnrfc.noaa.gov/',
        note: 'Active CNRFC river/reservoir forecast points from 53_ map-ready output. Symbols show active river forecast, reservoir inflow/release product availability, ensemble availability, and three manually placed water-supply index points.'
      })
    });
  }

  if (includeMajorWaterSupplyBasinForecasts &&
      MAJOR_WATER_SUPPLY_GEOMETRY &&
      MAJOR_WATER_SUPPLY_GEOMETRY.features &&
      MAJOR_WATER_SUPPLY_GEOMETRY.features.length === 23) {
    addOpsLayer({
      category: 'Forecasts / Outlooks',
      subgroup: 'River / Reservoir Forecasts',
      name: 'Major Water-Supply Basin Forecasts',
      panelLabel: 'Water-Supply Basin Forecasts | CNRFC / CBRFC',
      refreshable: true,
      sourceUrl: MAJOR_WATER_SUPPLY_CNRFC_URL,
      infoUrl: MAJOR_WATER_SUPPLY_CBRFC_URL,
      infoLabel: 'CBRFC feed',
      layer: makeMajorWaterSupplyBasinForecastLayer({
        name: 'Major Water-Supply Basin Forecasts',
        cnrfcUrl: MAJOR_WATER_SUPPLY_CNRFC_URL,
        cbrfcUrl: MAJOR_WATER_SUPPLY_CBRFC_URL,
        geometry: MAJOR_WATER_SUPPLY_GEOMETRY
      })
    });
  }

  if (includeCdecReservoirStorage && CDEC_RESERVOIR_STORAGE_URL) {
    addOpsLayer({
      category: 'Forecasts / Outlooks',
      subgroup: 'River / Reservoir Forecasts',
      name: 'Reservoirs | storage-centric | CDEC / CNRFC / USACE',
      sourceUrl: CDEC_RESERVOIR_STORAGE_URL,
      infoUrl: CDEC_RESERVOIR_STORAGE_SUMMARY_URL,
      infoLabel: 'summary',
      layer: makeCdecReservoirStorageLayer({
        name: 'Reservoirs | storage-centric | CDEC / CNRFC / USACE',
        url: CDEC_RESERVOIR_STORAGE_URL,
        summaryUrl: CDEC_RESERVOIR_STORAGE_SUMMARY_URL,
        sourceUrl: CDEC_RESERVOIR_STORAGE_URL,
        note: 'Near-live CDEC storage where available, with CNRFC forecast/release links and USACE reservoir-plot links where available. Values are provisional and fetched when this layer is turned on.'
      })
    });
  }


  if (includeCnrfcPrecipWeatherStations && CNRFC_PRECIP_WEATHER_STATIONS && CNRFC_PRECIP_WEATHER_STATIONS.length) {
    addOpsLayer({
      category: 'Hydro Observations',
      subgroup: 'NWS / CNRFC / WRH sites & product links',
      name: 'NWS weather stations | NWS/WRH time series',
      sourceUrl: 'https://www.cnrfc.noaa.gov/rainfall_data.php',
      infoUrl: 'https://www.cnrfc.noaa.gov/rainfall_data.php',
      infoLabel: 'CNRFC',
      layer: new CnrfcPrecipWeatherStationsLayer({
        name: 'NWS weather stations | NWS/WRH time series',
        records: CNRFC_PRECIP_WEATHER_STATIONS,
        sourceUrl: 'https://www.cnrfc.noaa.gov/rainfall_data.php',
        note: 'Clustered NWS weather stations from the CNRFC station catalog with NWS/WRH station time-series links. ASOS/METAR-style 3-character station IDs use the standard K-prefixed WRH site ID where needed; RAWS/GOES labels group fire-weather/remote stations with GOES-telemetered station records.'
      })
    });
  }

  addOpsLayer({category: 'Hydro Observations', subgroup: 'Precip / QPE', name: 'QPE | NWS MRMS 1-hr', layer: new ArcGISExportLayer({name: 'QPE | NWS MRMS 1-hr', serviceType: 'ImageServer', url: MRMS, rasterFunction: 'rft_1hr', opacity: 0.58, legendType: 'qpe', note: 'NWS MRMS 1-hour QPE accumulation. Mostly automated sensor/radar-based precipitation estimate; useful for quick storm context.', legendUrl: mapServerLegendUrl(MRMS), checkFreshness: checkMrmsLatest})});
  addOpsLayer({category: 'Hydro Observations', subgroup: 'Precip / QPE', name: 'QPE | NWS MRMS 1-day', layer: new ArcGISExportLayer({name: 'QPE | NWS MRMS 1-day', serviceType: 'ImageServer', url: MRMS, rasterFunction: 'rft_24hr', opacity: 0.60, legendType: 'qpe', note: 'NWS MRMS 1-day QPE accumulation. Mostly automated sensor/radar-based precipitation estimate; useful for quick storm context.', legendUrl: mapServerLegendUrl(MRMS), checkFreshness: checkMrmsLatest})});
  addOpsLayer({category: 'Hydro Observations', subgroup: 'Precip / QPE', name: 'QPE | NWS MRMS 3-day', layer: new ArcGISExportLayer({name: 'QPE | NWS MRMS 3-day', serviceType: 'ImageServer', url: MRMS, rasterFunction: 'rft_72hr', opacity: 0.60, legendType: 'qpe', note: 'NWS MRMS 3-day QPE accumulation. Mostly automated sensor/radar-based precipitation estimate; useful for quick storm context.', legendUrl: mapServerLegendUrl(MRMS), checkFreshness: checkMrmsLatest})});
  addOpsLayer({category: 'Hydro Observations', subgroup: 'Precip / QPE', name: 'QPE | NWS RFC mosaic 1-day', layer: new ArcGISExportLayer({name: 'QPE | NWS RFC mosaic 1-day', serviceType: 'MapServer', url: RFC_QPE, layers: [32], opacity: 0.64, legendType: 'qpe', note: 'RFC multisensor QPE mosaic daily analysis: 1-day total ending near 12Z. This uses the Today\'s Analysis image sublayer, which should be more complete for western RFC coverage than the rolling Last 24 Hours image.', legendUrl: mapServerLegendUrl(RFC_QPE), checkFreshness: checkRfcQpe})});
  addOpsLayer({category: 'Hydro Observations', subgroup: 'Precip / QPE', name: 'QPE | NWS RFC mosaic 7-day', layer: new ArcGISExportLayer({name: 'QPE | NWS RFC mosaic 7-day', serviceType: 'MapServer', url: RFC_QPE, layers: [56], opacity: 0.64, legendType: 'qpe', note: 'RFC multisensor QPE mosaic, 7-day accumulation ending near 12Z.', legendUrl: mapServerLegendUrl(RFC_QPE), checkFreshness: checkRfcQpe})});

  if (includeCocorahsDailyPrecip && COCORAHS_CA_DAILY_PRECIP_URL) {
    addOpsLayer({
      category: 'Hydro Observations',
      subgroup: 'Precip / QPE',
      name: 'CoCoRaHS | CA daily',
      sourceUrl: 'https://www.cocorahs.org/',
      infoUrl: '',
      infoLabel: '',
      layer: makeCocorahsDailyPrecipLayer({
        name: 'CoCoRaHS | CA daily',
        scopeLabel: 'California',
        url: COCORAHS_CA_DAILY_PRECIP_URL,
        sourceUrl: 'https://www.cocorahs.org/',
        summaryUrl: COCORAHS_CA_DAILY_PRECIP_SUMMARY_URL,
        note: 'Volunteer daily precipitation reports for California, useful as a fast/snappy storm-verification and QPE/QPF context layer. Each amount is an approximate prior-24-hour station report ending at the listed observation time; reporting times vary by station. Data are fetched by a scheduled BRIM feed and loaded as static GeoJSON when the layer is turned on; treat as supplemental screening information.'
      })
    });
  }

  if (includeCocorahsDailyPrecip && COCORAHS_CONUS_DAILY_PRECIP_URL) {
    addOpsLayer({
      category: 'Hydro Observations',
      subgroup: 'Precip / QPE',
      name: 'CoCoRaHS | 50-state daily',
      sourceUrl: 'https://www.cocorahs.org/',
      infoUrl: '',
      infoLabel: '',
      layer: makeCocorahsDailyPrecipLayer({
        name: 'CoCoRaHS | 50-state daily',
        scopeLabel: '50 states',
        url: COCORAHS_CONUS_DAILY_PRECIP_URL,
        sourceUrl: 'https://www.cocorahs.org/',
        summaryUrl: COCORAHS_CONUS_DAILY_PRECIP_SUMMARY_URL,
        note: 'Volunteer daily precipitation reports for all 50 states, useful for broader storm-context and curiosity browsing. Each amount is an approximate prior-24-hour station report ending at the listed observation time; reporting times vary by station. Data are fetched by a scheduled BRIM feed and loaded as static GeoJSON when the layer is turned on; treat as supplemental screening information.'
      })
    });
  }

  

  addOpsExternalLinks({
    category: 'Hydro Observations',
    subgroup: 'Precip / QPE',
    title: 'CNRFC QPE graphics',
    note: 'External links, not map layers.',
    links: [
      {label: 'grid: 1-day', url: 'https://www.cnrfc.noaa.gov/?product=QPE24hr&time=24hr&PNGtypeID=QPE', title: 'Open CNRFC 1-day gridded QPE graphic'},
      {label: '2-day', url: 'https://www.cnrfc.noaa.gov/?product=QPE48hr&time=48hr&PNGtypeID=QPE', title: 'Open CNRFC 2-day gridded QPE graphic'},
      {label: '3-day', url: 'https://www.cnrfc.noaa.gov/?product=QPE72hr&time=72hr&PNGtypeID=QPE', title: 'Open CNRFC 3-day gridded QPE graphic'},
      {label: '7-day', url: 'https://www.cnrfc.noaa.gov/?product=QPE7day&time=7day&PNGtypeID=QPE', title: 'Open CNRFC 7-day gridded QPE graphic'},
      {label: 'point: 1-day', url: 'https://www.cnrfc.noaa.gov/?product=pp24', title: 'Open CNRFC 1-day point precipitation page'}
    ]
  });

  addOpsLayer({
    category: 'Forecasts / Outlooks',
    subgroup: 'Weather Forecasts / Outlooks',
    name: 'WPC QPF Day 1',
    refreshable: true,
    layer: new ArcGISExportLayer({name: 'WPC QPF Day 1', serviceType: 'MapServer', url: WPC_QPF, layers: [1], opacity: 0.64, refreshable: true, legendType: 'qpe', note: 'WPC 24-hour Day 1 QPF. Hover over the active layer for forecast precipitation and valid/issued times in Los Angeles time. Use rfrsh after pan/zoom to force a fresh image request for the current map view.', legendUrl: mapServerLegendUrl(WPC_QPF), sourceUrl: 'https://www.wpc.ncep.noaa.gov/qpf/d1qpfall.html', checkFreshness: checkWpcQpf}),
    onActivate: function() { activateWpcQpfHover('WPC QPF Day 1', 1); },
    onDeactivate: function() { deactivateWpcQpfHover('WPC QPF Day 1'); }
  });
  addOpsLayer({
    category: 'Forecasts / Outlooks',
    subgroup: 'Weather Forecasts / Outlooks',
    name: 'WPC QPF Day 2',
    refreshable: true,
    layer: new ArcGISExportLayer({name: 'WPC QPF Day 2', serviceType: 'MapServer', url: WPC_QPF, layers: [2], opacity: 0.64, refreshable: true, legendType: 'qpe', note: 'WPC 24-hour Day 2 QPF. Hover over the active layer for forecast precipitation and valid/issued times in Los Angeles time. Use rfrsh after pan/zoom to force a fresh image request for the current map view.', legendUrl: mapServerLegendUrl(WPC_QPF), sourceUrl: 'https://www.wpc.ncep.noaa.gov/qpf/day2.shtml', checkFreshness: checkWpcQpf}),
    onActivate: function() { activateWpcQpfHover('WPC QPF Day 2', 2); },
    onDeactivate: function() { deactivateWpcQpfHover('WPC QPF Day 2'); }
  });
  addOpsLayer({
    category: 'Forecasts / Outlooks',
    subgroup: 'Weather Forecasts / Outlooks',
    name: 'WPC QPF Day 3',
    refreshable: true,
    layer: new ArcGISExportLayer({name: 'WPC QPF Day 3', serviceType: 'MapServer', url: WPC_QPF, layers: [3], opacity: 0.64, refreshable: true, legendType: 'qpe', note: 'WPC 24-hour Day 3 QPF. Hover over the active layer for forecast precipitation and valid/issued times in Los Angeles time. Use rfrsh after pan/zoom to force a fresh image request for the current map view.', legendUrl: mapServerLegendUrl(WPC_QPF), sourceUrl: 'https://www.wpc.ncep.noaa.gov/qpf/day3.shtml', checkFreshness: checkWpcQpf}),
    onActivate: function() { activateWpcQpfHover('WPC QPF Day 3', 3); },
    onDeactivate: function() { deactivateWpcQpfHover('WPC QPF Day 3'); }
  });
  addOpsLayer({
    category: 'Forecasts / Outlooks',
    subgroup: 'Weather Forecasts / Outlooks',
    name: 'WPC QPF 3-day',
    refreshable: true,
    layer: new ArcGISExportLayer({name: 'WPC QPF 3-day', serviceType: 'MapServer', url: WPC_QPF, layers: [9], opacity: 0.64, refreshable: true, legendType: 'qpe', note: 'WPC 3-day QPF for Days 1-3. Hover over the active layer for forecast precipitation and valid/issued times in Los Angeles time. Use rfrsh after pan/zoom to force a fresh image request for the current map view.', legendUrl: mapServerLegendUrl(WPC_QPF), sourceUrl: 'https://www.wpc.ncep.noaa.gov/qpf/day1-3.shtml', checkFreshness: checkWpcQpf}),
    onActivate: function() { activateWpcQpfHover('WPC QPF 3-day', 9); },
    onDeactivate: function() { deactivateWpcQpfHover('WPC QPF 3-day'); }
  });
  addOpsLayer({
    category: 'Forecasts / Outlooks',
    subgroup: 'Weather Forecasts / Outlooks',
    name: 'WPC QPF 7-day',
    refreshable: true,
    layer: new ArcGISExportLayer({name: 'WPC QPF 7-day', serviceType: 'MapServer', url: WPC_QPF, layers: [11], opacity: 0.64, refreshable: true, legendType: 'qpe', note: 'WPC 7-day QPF for Days 1-7. Hover over the active layer for forecast precipitation and valid/issued times in Los Angeles time. Use rfrsh after pan/zoom to force a fresh image request for the current map view.', legendUrl: mapServerLegendUrl(WPC_QPF), sourceUrl: 'https://www.wpc.ncep.noaa.gov/qpf/day1-7.shtml', checkFreshness: checkWpcQpf}),
    onActivate: function() { activateWpcQpfHover('WPC QPF 7-day', 11); },
    onDeactivate: function() { deactivateWpcQpfHover('WPC QPF 7-day'); }
  });

  addOpsExternalLinks({
    category: 'Forecasts / Outlooks',
    subgroup: 'Weather Forecasts / Outlooks',
    title: 'CNRFC QPF graphics',
    note: 'External links, not map layers.',
    links: [
      {label: '1-day total', url: 'https://www.cnrfc.noaa.gov/?product=QPF24hr&time=24hr&PNGtypeID=QPF', title: 'Open CNRFC 1-day total QPF graphic'},
      {label: '3-day total', url: 'https://www.cnrfc.noaa.gov/?product=QPF72hr&time=72hr&PNGtypeID=QPF', title: 'Open CNRFC 3-day total QPF graphic'},
      {label: '6-day total', url: 'https://www.cnrfc.noaa.gov/?product=QPF6day&time=6day&PNGtypeID=QPF', title: 'Open CNRFC 6-day total QPF graphic'}
    ]
  });

  addOpsExternalLinks({
    category: 'Forecasts / Outlooks',
    subgroup: 'Weather Forecasts / Outlooks',
    title: 'CW3E QPF comparison',
    note: 'External multi-model comparison page, not a map layer.',
    links: [
      {label: 'multi-model', url: 'https://cw3e.ucsd.edu/Projects/QPF/QPF.html', title: 'Open CW3E multi-model QPF comparison tool'}
    ]
  });
  
  addOpsLayer({
    category: 'Hazards',
    name: 'NWS Watches / Warnings / Advisories',
    layer: new ArcGISExportLayer({
      name: 'NWS Watches / Warnings / Advisories',
      serviceType: 'MapServer',
      url: NWS_WWA,
      layers: [0,1],
      opacity: 0.52,
      note: 'Current National Weather Service watches, warnings, and advisories. Hover over colored polygons for event name, expiration, and linked forecast office.',
      legendUrl: mapServerLegendUrl(NWS_WWA),
      checkFreshness: checkNwsWwa
    }),
    onActivate: activateWwaHover,
    onDeactivate: deactivateWwaHover
  });
  addOpsLayer({
    category: 'Hazards',
    name: 'WPC ERO Day 1',
    refreshable: true,
    sourceUrl: 'https://www.wpc.ncep.noaa.gov/qpf/ero.php?day=1&opt=curr',
    legendUrl: mapServerLegendUrl(WPC_ERO),
    layer: new WpcEroCurrentViewLayer({
      name: 'WPC ERO Day 1',
      url: WPC_ERO,
      layerId: 0,
      refreshable: true,
      legendType: 'ero',
      note: 'Excessive Rainfall Outlook Day 1. Current-view BRIM polygon snapshot with hover and popup summaries; pan/zoom, then use rfrsh to requery.',
      legendUrl: mapServerLegendUrl(WPC_ERO),
      sourceUrl: 'https://www.wpc.ncep.noaa.gov/qpf/ero.php?day=1&opt=curr',
      checkFreshness: checkWpcEro
    })
  });

  addOpsLayer({
    category: 'Hazards',
    name: 'WPC ERO Day 2',
    refreshable: true,
    sourceUrl: 'https://www.wpc.ncep.noaa.gov/qpf/ero.php?day=2&opt=curr',
    legendUrl: mapServerLegendUrl(WPC_ERO),
    layer: new WpcEroCurrentViewLayer({
      name: 'WPC ERO Day 2',
      url: WPC_ERO,
      layerId: 1,
      refreshable: true,
      legendType: 'ero',
      note: 'Excessive Rainfall Outlook Day 2. Current-view BRIM polygon snapshot with hover and popup summaries; pan/zoom, then use rfrsh to requery.',
      legendUrl: mapServerLegendUrl(WPC_ERO),
      sourceUrl: 'https://www.wpc.ncep.noaa.gov/qpf/ero.php?day=2&opt=curr',
      checkFreshness: checkWpcEro
    })
  });

  addOpsLayer({
    category: 'Hazards',
    name: 'WPC ERO Day 3',
    refreshable: true,
    sourceUrl: 'https://www.wpc.ncep.noaa.gov/qpf/ero.php?day=3&opt=curr',
    legendUrl: mapServerLegendUrl(WPC_ERO),
    layer: new WpcEroCurrentViewLayer({
      name: 'WPC ERO Day 3',
      url: WPC_ERO,
      layerId: 2,
      refreshable: true,
      legendType: 'ero',
      note: 'Excessive Rainfall Outlook Day 3. Current-view BRIM polygon snapshot with hover and popup summaries; pan/zoom, then use rfrsh to requery.',
      legendUrl: mapServerLegendUrl(WPC_ERO),
      sourceUrl: 'https://www.wpc.ncep.noaa.gov/qpf/ero.php?day=3&opt=curr',
      checkFreshness: checkWpcEro
    })
  });


  // Current/recent fire perimeter layers moved to External Layers.
  // Use External Layers > Fire / Burn Areas > Fire perimeters for refreshable
  // current-view NIFC and CAL FIRE perimeter overlays.


  addOpsLayer({
    category: 'Weather Offices / Boundaries',
    name: 'NWS WFO Boundaries',
    sourceUrl: 'https://mapservices.weather.noaa.gov/static/rest/services/nws_reference_maps/nws_reference_map/FeatureServer/1',
    infoUrl: 'https://mapservices.weather.noaa.gov/static/rest/services/nws_reference_maps/nws_reference_map/FeatureServer/1',
    infoLabel: 'service',
    layer: new CatalogPromotedExternalLayer({
      name: 'NWS WFO Boundaries',
    catalogSourceDisplayName: 'NWS WFOs',
      sourceDisplayName: 'NWS WFOs',
      opsKey: 'ops_nws_wfo_boundaries',
      color: '#756BB1',
      sourceUrl: 'https://mapservices.weather.noaa.gov/static/rest/services/nws_reference_maps/nws_reference_map/FeatureServer/1',
      note: 'One-click Ops Live view of the curated External Layers row "NWS WFOs". Shows NWS Weather Forecast Office / County Warning Area responsibility boundaries and reuses the existing BRIM hover fields, popup fields, and WFO homepage links. Use External Layers if manual catalog controls or filtering are needed.'
    })
  });


  addOpsLayer({
    category: 'Drought',
    name: 'U.S. Drought Monitor',
    sourceUrl: 'https://services5.arcgis.com/0OTVzJS4K09zlixn/arcgis/rest/services/USDM_current/FeatureServer/0',
    infoUrl: 'https://services5.arcgis.com/0OTVzJS4K09zlixn/arcgis/rest/services/USDM_current/FeatureServer/0',
    infoLabel: 'service',
    layer: new CatalogPromotedExternalLayer({
      name: 'U.S. Drought Monitor',
    catalogSourceDisplayName: 'U.S. Drought Monitor (current)',
      sourceDisplayName: 'U.S. Drought Monitor (current)',
      opsKey: 'ops_us_drought_monitor_current',
      color: '#8C510A',
      sourceUrl: 'https://services5.arcgis.com/0OTVzJS4K09zlixn/arcgis/rest/services/USDM_current/FeatureServer/0',
      note: 'One-click Ops Live view of the curated External Layers row "U.S. Drought Monitor (current)". Reuses the existing BRIM drought-category styling, hover fields, popup fields, and source metadata. The catalog row remains the shared source of truth but is hidden from the External Layers browser.'
    })
  });


  addOpsExternalLinks({
    category: 'Drought',
    subgroup: '',
    title: 'Drought context links',
    note: 'External drought tools; not map layers.',
    links: [
      {label: 'DM-CA', url: 'https://droughtmonitor.unl.edu/CurrentMap/StateDroughtMonitor.aspx?CA', title: 'Open California U.S. Drought Monitor map'},
      {label: 'CE rprts', url: 'https://reports.climateengine.org/droughtv2', title: 'Open Climate Engine drought reports'},
      {label: 'D³', url: 'https://d3drought.org/', title: 'Open D3 drought dashboard'}
    ]
  });


  // CPC outlooks are promoted from the External Layers catalog so their
  // curated hover/popup aliases and CPC categorical symbology remain in one
  // shared place.
  addOpsLayer({
    category: 'Forecasts / Outlooks',
    subgroup: 'Weather Forecasts / Outlooks',
    name: 'CPC 6-10 Day Temperature Outlook',
    sourceUrl: 'https://www.cpc.ncep.noaa.gov/products/predictions/610day/610temp.new.gif',
    layer: new CatalogPromotedExternalLayer({
      name: 'CPC 6-10 Day Temperature Outlook',
    catalogSourceDisplayName: 'CPC 6-10 Day Temperature Outlook',
      sourceDisplayName: 'CPC 6-10 Day Temperature Outlook',
      opsKey: 'ops_cpc_6_10_day_temperature_outlook',
      color: '#D95F02',
      sourceUrl: 'https://www.cpc.ncep.noaa.gov/products/predictions/610day/610temp.new.gif',
      note: 'One-click Ops Live view of the curated External Layers row "CPC 6-10 Day Temperature Outlook". Reuses the existing CPC outlook styling, hover fields, popup fields, and source metadata. Use External Layers if manual catalog controls are needed.'
    })
  });

  addOpsLayer({
    category: 'Forecasts / Outlooks',
    subgroup: 'Weather Forecasts / Outlooks',
    name: 'CPC 6-10 Day Precipitation Outlook',
    sourceUrl: 'https://www.cpc.ncep.noaa.gov/products/predictions/610day/610prcp.new.gif',
    layer: new CatalogPromotedExternalLayer({
      name: 'CPC 6-10 Day Precipitation Outlook',
    catalogSourceDisplayName: 'CPC 6-10 Day Precipitation Outlook',
      sourceDisplayName: 'CPC 6-10 Day Precipitation Outlook',
      opsKey: 'ops_cpc_6_10_day_precipitation_outlook',
      color: '#2C7FB8',
      sourceUrl: 'https://www.cpc.ncep.noaa.gov/products/predictions/610day/610prcp.new.gif',
      note: 'One-click Ops Live view of the curated External Layers row "CPC 6-10 Day Precipitation Outlook". Reuses the existing CPC outlook styling, hover fields, popup fields, and source metadata. Use External Layers if manual catalog controls are needed.'
    })
  });

  addOpsLayer({
    category: 'Forecasts / Outlooks',
    subgroup: 'Weather Forecasts / Outlooks',
    name: 'CPC 8-14 Day Temperature Outlook',
    sourceUrl: 'https://www.cpc.ncep.noaa.gov/products/predictions/814day/814temp.new.gif',
    layer: new CatalogPromotedExternalLayer({
      name: 'CPC 8-14 Day Temperature Outlook',
    catalogSourceDisplayName: 'CPC 8-14 Day Temperature Outlook',
      sourceDisplayName: 'CPC 8-14 Day Temperature Outlook',
      opsKey: 'ops_cpc_8_14_day_temperature_outlook',
      color: '#A6611A',
      sourceUrl: 'https://www.cpc.ncep.noaa.gov/products/predictions/814day/814temp.new.gif',
      note: 'One-click Ops Live view of the curated External Layers row "CPC 8-14 Day Temperature Outlook". Reuses the existing CPC outlook styling, hover fields, popup fields, and source metadata. Use External Layers if manual catalog controls are needed.'
    })
  });

  addOpsLayer({
    category: 'Forecasts / Outlooks',
    subgroup: 'Weather Forecasts / Outlooks',
    name: 'CPC 8-14 Day Precipitation Outlook',
    sourceUrl: 'https://www.cpc.ncep.noaa.gov/products/predictions/814day/814prcp.new.gif',
    layer: new CatalogPromotedExternalLayer({
      name: 'CPC 8-14 Day Precipitation Outlook',
    catalogSourceDisplayName: 'CPC 8-14 Day Precipitation Outlook',
      sourceDisplayName: 'CPC 8-14 Day Precipitation Outlook',
      opsKey: 'ops_cpc_8_14_day_precipitation_outlook',
      color: '#1F78B4',
      sourceUrl: 'https://www.cpc.ncep.noaa.gov/products/predictions/814day/814prcp.new.gif',
      note: 'One-click Ops Live view of the curated External Layers row "CPC 8-14 Day Precipitation Outlook". Reuses the existing CPC outlook styling, hover fields, popup fields, and source metadata. Use External Layers if manual catalog controls are needed.'
    })
  });

  

)---"
}
