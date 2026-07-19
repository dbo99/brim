# ==== leaflet_ops_live_service_helpers.r ===================================
##
## PURPOSE:
##   Ops Live service endpoint constants and freshness/status check helpers.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r`.
##   It returns browser-side JavaScript as text for injection into the Ops Live
##   htmlwidgets/onRender function. Keep edits narrow and feature-specific.
## ============================================================================

pt_ops_live_service_helpers_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Service endpoints and freshness checks
  // --------------------------------------------------------------------------
  var MRMS = 'https://mapservices.weather.noaa.gov/raster/rest/services/obs/mrms_qpe/ImageServer';
  var RFC_QPE = 'https://mapservices.weather.noaa.gov/raster/rest/services/obs/rfc_qpe/MapServer';
  var WPC_QPF = 'https://mapservices.weather.noaa.gov/vector/rest/services/precip/wpc_qpf/MapServer';
  var WPC_ERO = 'https://mapservices.weather.noaa.gov/vector/rest/services/hazards/wpc_precip_hazards/MapServer';
  var NOAA_RADAR = 'https://mapservices.weather.noaa.gov/eventdriven/rest/services/radar/radar_base_reflectivity/MapServer';
  var NWS_WWA = 'https://mapservices.weather.noaa.gov/eventdriven/rest/services/WWA/watch_warn_adv/MapServer';
  var SURFACE_OBS = 'https://mapservices.weather.noaa.gov/vector/rest/services/obs/surface_obs/MapServer';
  var NOAA_GOES_GEOCOLOR = 'https://satellitemaps.nesdis.noaa.gov/arcgis/rest/services/MERGEDGC_current/ImageServer';
  var NOAA_GOES_IR = 'https://satellitemaps.nesdis.noaa.gov/arcgis/rest/services/ABI13_current/ImageServer';
  var NOAA_GOES_WV = 'https://satellitemaps.nesdis.noaa.gov/arcgis/rest/services/ABI10_current/ImageServer';

  var INFO_NOAA_SATELLITE_MAPS = 'https://www.nesdis.noaa.gov/imagery/interactive-maps/how-use-the-interactive-satellite-maps';
  var INFO_GOES_IR_BAND13 = 'https://www.goes.noaa.gov/documents/ABIQuickGuide_Band13.pdf';
  var INFO_GOES_WV_BAND10 = 'https://www.goes.noaa.gov/documents/ABIQuickGuide_Band10.pdf';
  var INFO_NASA_GIBS = 'https://nasa-gibs.github.io/gibs-api-docs/';

  function mapServerLegendUrl(baseUrl) {
    return safeUrlBase(baseUrl) + '/legend?f=html';
  }
  
  function checkReturnUpdates(name, baseUrl, expected) {
    var url = safeUrlBase(baseUrl) + '/returnUpdates?f=pjson&_=' + Date.now();
    fetch(url)
      .then(function(resp) { return resp.json(); })
      .then(function(json) {
        var text = '';
        if (json && json.fullUpdate) text += 'fullUpdate: ' + json.fullUpdate + '; ';
        if (json && json.layers && json.layers.length) text += 'layers reported: ' + json.layers.length + '; ';
        if (!text) text = 'metadata returned; ';
        recordStatus(name + ' metadata', text + 'expected: ' + expected, 'pt-ops-ok');
      })
      .catch(function() {
        recordStatus(name + ' metadata', 'Freshness request failed; expected: ' + expected, 'pt-ops-warn');
      });
  }
  
  function checkMrmsLatest() {
    var url = safeUrlBase(MRMS) + '/query?f=json&where=1%3D1&outFields=name,idp_validendtime,idp_filedate,idp_ingestdate&returnGeometry=false&orderByFields=idp_validendtime%20DESC&resultRecordCount=1&_=' + Date.now();
    fetch(url)
      .then(function(resp) { return resp.json(); })
      .then(function(json) {
        var f = json && json.features && json.features[0] && json.features[0].attributes;
        if (!f) throw new Error('No feature attributes returned');
        var valid = f.idp_validendtime ? fmtTime(new Date(f.idp_validendtime)) : 'unknown valid/end time';
        recordStatus('MRMS metadata', 'Latest valid/end time: ' + valid + '; expected update near :04 after hour.', 'pt-ops-ok');
      })
      .catch(function() {
        recordStatus('MRMS metadata', 'Freshness request failed; expected update near :04 after hour.', 'pt-ops-warn');
      });
  }
  
  function checkRfcQpe() {
    checkReturnUpdates('NWS QPE Mosaic', RFC_QPE, 'hourly near :55; daily products can update several times 12Z-21Z');
  }
  
  function checkWpcQpf() {
    checkReturnUpdates('WPC QPF', WPC_QPF, 'twice daily at 06Z and 18Z');
  }
  
  function checkWpcEro() {
    checkReturnUpdates('WPC ERO', WPC_ERO, 'Day 1 required 0100Z/0830Z/1500Z; updates possible anytime');
  }

  function checkNoaaRadar() {
    checkReturnUpdates('NOAA radar', NOAA_RADAR, 'roughly every 5-10 minutes from the MRMS radar base-reflectivity service');
  }

  function checkNwsWwa() {
    checkReturnUpdates('NWS watches/warnings/advisories', NWS_WWA, 'roughly every 5 minutes');
  }

  function checkSurfaceObs() {
    checkReturnUpdates('NWS surface observations', SURFACE_OBS, 'roughly every 10 minutes; individual stations may report less often');
  }

  function checkNoaaSatellite(name) {
    return function() {
      recordStatus(name + ' metadata', 'NOAA/NESDIS latest-image service requested; expected refresh is near real time, commonly about every 10-15 minutes for GOES imagery.', 'pt-ops-warn');
    };
  }
  

)---"
}
