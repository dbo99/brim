# ================================================================
# Operational QPE / QPF Leaflet Layer Test
# ================================================================
# Purpose:
#   Create a standalone, shareable HTML file that tests a long list of
#   operational hydrology / precipitation layers using live public services.
#
# Design goals:
#   - Keep the HTML lightweight: live layers are pulled from NOAA/CNRFC when opened.
#   - Avoid pseudo-tiling MapServer services, which can hammer services.
#   - Use ONE dynamic image request per active layer per map view.
#   - Put most layers OFF by default so the user can test them one at a time.
#
# Notes:
#   - The output HTML is a single file, but it is not offline. It needs internet.
#   - The NOAA services below are official ArcGIS REST services.
#   - The CNRFC overlays are EXPERIMENTAL direct PNG overlays derived from KML
#     products. They are useful to test, but less robust than NOAA MapServer layers.
#   - If too many layers are enabled at once, the map will look messy and will make
#     multiple live requests after each pan/zoom.
#
# Required R packages: leaflet, htmlwidgets, htmltools
# ================================================================

needed <- c("leaflet", "htmlwidgets", "htmltools")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  install.packages(missing)
}

library(leaflet)
library(htmlwidgets)
library(htmltools)

OUT_DIR <- "output"
OUT_HTML <- file.path(OUT_DIR, "operational_qpe_qpf_layer_test_v2.html")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# ----------------------------------------------------------------
# Basic Leaflet map.
# ----------------------------------------------------------------
m <- leaflet(options = leafletOptions(preferCanvas = TRUE, zoomControl = TRUE))
m <- addProviderTiles(m, providers$CartoDB.Positron, group = "CartoDB Positron")
m <- setView(m, lng = -119.5, lat = 37.2, zoom = 6)
m <- addScaleBar(m, position = "bottomleft")

# ----------------------------------------------------------------
# CSS for the title, status, source, and legend panels.
# ----------------------------------------------------------------
css_code <- r"(
.hydro-title {
  background: rgba(255, 255, 255, 0.94);
  padding: 10px 12px;
  border-radius: 8px;
  box-shadow: 0 1px 8px rgba(0,0,0,0.25);
  font-family: Arial, Helvetica, sans-serif;
  max-width: 520px;
  line-height: 1.25;
}
.hydro-title .big {
  font-size: 17px;
  font-weight: 700;
  margin-bottom: 2px;
}
.hydro-title .small {
  font-size: 12px;
  color: #333;
}
.hydro-panel {
  background: rgba(255, 255, 255, 0.95);
  padding: 8px 10px;
  border-radius: 8px;
  box-shadow: 0 1px 8px rgba(0,0,0,0.25);
  font-family: Arial, Helvetica, sans-serif;
  font-size: 12px;
  line-height: 1.25;
  max-width: 370px;
  max-height: 46vh;
  overflow-y: auto;
}
.hydro-panel h4 {
  margin: 0 0 6px 0;
  font-size: 13px;
}
.hydro-status-row {
  border-top: 1px solid #ddd;
  padding-top: 4px;
  margin-top: 4px;
}
.hydro-ok { color: #106b21; }
.hydro-warn { color: #9a5a00; }
.hydro-bad { color: #9c1c1c; }
.hydro-muted { color: #666; }
.leaflet-control-layers {
  max-height: 70vh;
  overflow-y: auto;
  font-family: Arial, Helvetica, sans-serif;
  font-size: 12px;
}
.legend-swatch {
  display: inline-block;
  width: 14px;
  height: 10px;
  margin-right: 5px;
  border: 1px solid rgba(0,0,0,0.25);
  vertical-align: middle;
}
.legend-line {
  white-space: nowrap;
}
.legend-section {
  border-top: 1px solid #ddd;
  margin-top: 6px;
  padding-top: 6px;
}
.legend-img-line {
  display: flex;
  align-items: center;
  gap: 4px;
  margin: 2px 0;
}
.legend-img-line img {
  max-width: 22px;
  max-height: 22px;
}
)"

m <- htmlwidgets::prependContent(
  m,
  htmltools::tags$style(htmltools::HTML(css_code))
)

# ----------------------------------------------------------------
# JavaScript: dynamic export overlays + layer control.
# ----------------------------------------------------------------
js_code <- r"(
function(el, x) {
  var map = this;

  // --------------------------------------------------------------
  // Controls: title, status, source links, and legend.
  // --------------------------------------------------------------
  var title = L.control({position: 'topleft'});
  title.onAdd = function() {
    var div = L.DomUtil.create('div', 'hydro-title');
    div.innerHTML =
      '<div class="big">Operational QPE / QPF Layer Test v2</div>' +
      '<div class="small">Turn layers on one at a time. NOAA layers use one dynamic image request per active layer per map view. CNRFC layers are experimental image overlays.</div>';
    L.DomEvent.disableClickPropagation(div);
    return div;
  };
  title.addTo(map);

  var statusCtl = L.control({position: 'bottomright'});
  var statusDiv;
  var statusRows = {};
  statusCtl.onAdd = function() {
    statusDiv = L.DomUtil.create('div', 'hydro-panel');
    statusDiv.innerHTML = '<h4>Live-layer status</h4><div class="hydro-muted">No live overlays loaded yet.</div>';
    L.DomEvent.disableClickPropagation(statusDiv);
    return statusDiv;
  };
  statusCtl.addTo(map);

  var legendCtl = L.control({position: 'bottomleft'});
  var legendDiv;
  legendCtl.onAdd = function() {
    legendDiv = L.DomUtil.create('div', 'hydro-panel');
    legendDiv.innerHTML = '<h4>Legend</h4><div class="hydro-muted">Enable a layer to show legend notes.</div>';
    L.DomEvent.disableClickPropagation(legendDiv);
    return legendDiv;
  };
  legendCtl.addTo(map);

  var linksCtl = L.control({position: 'topright'});
  linksCtl.onAdd = function() {
    var div = L.DomUtil.create('div', 'hydro-panel');
    div.innerHTML =
      '<h4>Source endpoints</h4>' +
      '<div><a href="https://mapservices.weather.noaa.gov/raster/rest/services/obs/mrms_qpe/ImageServer" target="_blank">MRMS QPE ImageServer</a></div>' +
      '<div><a href="https://mapservices.weather.noaa.gov/raster/rest/services/obs/rfc_qpe/MapServer" target="_blank">RFC QPE MapServer</a></div>' +
      '<div><a href="https://mapservices.weather.noaa.gov/vector/rest/services/precip/wpc_qpf/MapServer" target="_blank">WPC QPF MapServer</a></div>' +
      '<div><a href="https://mapservices.weather.noaa.gov/vector/rest/services/hazards/wpc_precip_hazards/MapServer" target="_blank">WPC ERO MapServer</a></div>' +
      '<div><a href="https://www.cnrfc.noaa.gov/qpf.php" target="_blank">CNRFC QPF page</a></div>';
    L.DomEvent.disableClickPropagation(div);
    return div;
  };

  // Add source box after the layer control so it appears below it.

  function fmtTime(d) {
    try { return d.toLocaleString(); } catch(e) { return String(d); }
  }

  function nowLocal() {
    return fmtTime(new Date());
  }

  function recordStatus(name, msg, cssClass) {
    statusRows[name] = {
      msg: msg,
      cssClass: cssClass || 'hydro-muted',
      time: nowLocal()
    };
    redrawStatus();
  }

  function redrawStatus() {
    var keys = Object.keys(statusRows).sort();
    if (!keys.length) {
      statusDiv.innerHTML = '<h4>Live-layer status</h4><div class="hydro-muted">No live overlays loaded yet.</div>';
      return;
    }
    var html = '<h4>Live-layer status</h4>';
    keys.forEach(function(k) {
      var r = statusRows[k];
      html += '<div class="hydro-status-row"><b>' + k + '</b><br>' +
              '<span class="' + r.cssClass + '">' + r.msg + '</span><br>' +
              '<span class="hydro-muted">Checked: ' + r.time + '</span></div>';
    });
    statusDiv.innerHTML = html;
  }

  var activeLegendDefs = {};

  function qpeLegendHtml(label) {
    return '<div class="legend-section"><b>' + label + '</b><br>' +
      '<div class="legend-line"><span class="legend-swatch" style="background:#e8f6ff"></span>Very light</div>' +
      '<div class="legend-line"><span class="legend-swatch" style="background:#9bd1ff"></span>Light</div>' +
      '<div class="legend-line"><span class="legend-swatch" style="background:#55b65a"></span>Moderate</div>' +
      '<div class="legend-line"><span class="legend-swatch" style="background:#ffd94a"></span>Heavier</div>' +
      '<div class="legend-line"><span class="legend-swatch" style="background:#ff8b2c"></span>Heavy</div>' +
      '<div class="legend-line"><span class="legend-swatch" style="background:#c82424"></span>Very heavy</div>' +
      '<div class="hydro-muted">Schematic only; use the source legend for exact breaks.</div></div>';
  }

  function eroLegendHtml() {
    return '<div class="legend-section"><b>WPC Excessive Rainfall Outlook</b><br>' +
      '<div class="legend-line"><span class="legend-swatch" style="background:#38a800"></span>Marginal</div>' +
      '<div class="legend-line"><span class="legend-swatch" style="background:#ffff00"></span>Slight</div>' +
      '<div class="legend-line"><span class="legend-swatch" style="background:#ff0000"></span>Moderate</div>' +
      '<div class="legend-line"><span class="legend-swatch" style="background:#ff00ff"></span>High</div></div>';
  }

  function redrawLegend() {
    var keys = Object.keys(activeLegendDefs).sort();
    if (!keys.length) {
      legendDiv.innerHTML = '<h4>Legend</h4><div class="hydro-muted">Enable a layer to show legend notes.</div>';
      return;
    }
    var seen = {};
    var html = '<h4>Legend / active overlays</h4>';
    keys.forEach(function(k) {
      var def = activeLegendDefs[k];
      html += '<div class="legend-section"><b>' + k + '</b><br><span class="hydro-muted">' + def.note + '</span></div>';
      if (def.legendType && !seen[def.legendType]) {
        if (def.legendType === 'qpe') html += qpeLegendHtml('QPE / QPF colors');
        if (def.legendType === 'ero') html += eroLegendHtml();
        seen[def.legendType] = true;
      }
    });
    legendDiv.innerHTML = html;
  }

  function getMapSizeForExport() {
    var s = map.getSize();
    // Keep image requests bounded and service-friendly.
    var w = Math.max(400, Math.min(1800, Math.round(s.x)));
    var h = Math.max(300, Math.min(1200, Math.round(s.y)));
    return {w: w, h: h};
  }

  function getBbox3857() {
    // Leaflet default CRS is Web Mercator (EPSG:3857).
    // Request ArcGIS exports in the same projection so the images line up.
    var b = map.getBounds();
    var sw = map.options.crs.project(b.getSouthWest());
    var ne = map.options.crs.project(b.getNorthEast());
    return [sw.x, sw.y, ne.x, ne.y].join(',');
  }

  function safeUrlBase(url) {
    return url.replace(/\/$/, '');
  }

  // --------------------------------------------------------------
  // Dynamic ArcGIS export layer.
  // One PNG image per active layer per map view.
  // --------------------------------------------------------------
  var ArcGISExportLayer = L.Layer.extend({
    initialize: function(options) {
      this.options = options || {};
      this._overlay = null;
      this._timer = null;
      this._pendingOverlay = null;
    },

    onAdd: function(mapObj) {
      this._map = mapObj;
      activeLegendDefs[this.options.name] = {
        note: this.options.note || '',
        legendType: this.options.legendType || null
      };
      redrawLegend();
      this._scheduleUpdate();
      mapObj.on('moveend zoomend resize', this._scheduleUpdate, this);
      recordStatus(this.options.name, 'Requested live image.', 'hydro-warn');
    },

    onRemove: function(mapObj) {
      mapObj.off('moveend zoomend resize', this._scheduleUpdate, this);
      if (this._timer) window.clearTimeout(this._timer);
      if (this._overlay) {
        mapObj.removeLayer(this._overlay);
        this._overlay = null;
      }
      if (this._pendingOverlay) {
        mapObj.removeLayer(this._pendingOverlay);
        this._pendingOverlay = null;
      }
      delete activeLegendDefs[this.options.name];
      redrawLegend();
      recordStatus(this.options.name, 'Layer turned off.', 'hydro-muted');
    },

    _scheduleUpdate: function() {
      var self = this;
      if (this._timer) window.clearTimeout(this._timer);
      this._timer = window.setTimeout(function() { self._update(); }, 650);
    },

    _buildUrl: function() {
      var opts = this.options;
      var size = getMapSizeForExport();
      var bbox = getBbox3857();
      var base = safeUrlBase(opts.url);
      var common = 'bbox=' + encodeURIComponent(bbox) +
        '&bboxSR=3857&imageSR=3857' +
        '&size=' + size.w + ',' + size.h +
        '&transparent=true&f=image';

      if (opts.serviceType === 'ImageServer') {
        // ImageServer exportImage is pickier about format names than MapServer.
        // Plain png is the safest choice here.
        var rr = encodeURIComponent(JSON.stringify({rasterFunction: opts.rasterFunction}));
        return base + '/exportImage?' + common + '&format=png&renderingRule=' + rr + '&_=' + Date.now();
      }

      // MapServer export. Layer IDs are passed as a comma-separated list.
      var layerText = Array.isArray(opts.layers) ? opts.layers.join(',') : String(opts.layers);
      return base + '/export?' + common + '&format=png32&dpi=96&layers=show:' + encodeURIComponent(layerText) + '&_=' + Date.now();
    },

    _update: function() {
      if (!this._map) return;
      var self = this;
      var b = this._map.getBounds();
      var url = this._buildUrl();
      var img = L.imageOverlay(url, b, {
        opacity: this.options.opacity || 0.65,
        interactive: false,
        crossOrigin: false
      });
      this._pendingOverlay = img;
      img.on('load', function() {
        if (self._overlay && self._overlay !== img) {
          self._map.removeLayer(self._overlay);
        }
        self._overlay = img;
        self._pendingOverlay = null;
        recordStatus(self.options.name, 'Loaded live image successfully.', 'hydro-ok');
      });
      img.on('error', function() {
        try { self._map.removeLayer(img); } catch(e) {}
        if (self._pendingOverlay === img) self._pendingOverlay = null;
        recordStatus(self.options.name, 'Image request failed. Try zooming in, waiting, or toggling fewer layers.', 'hydro-bad');
      });
      img.addTo(this._map);
    }
  });

  // --------------------------------------------------------------
  // Fixed image overlay layer for CNRFC experimental overlays.
  // --------------------------------------------------------------
  function makeFixedImageLayer(opts) {
    var lyr = L.imageOverlay(opts.url, [[opts.south, opts.west], [opts.north, opts.east]], {
      opacity: opts.opacity || 0.65,
      interactive: false,
      crossOrigin: false
    });
    lyr.on('add', function() {
      activeLegendDefs[opts.name] = {note: opts.note || '', legendType: opts.legendType || 'qpe'};
      redrawLegend();
      recordStatus(opts.name, 'Requested fixed image overlay. Browser will fetch directly from CNRFC.', 'hydro-warn');
    });
    lyr.on('load', function() {
      recordStatus(opts.name, 'Loaded CNRFC image overlay.', 'hydro-ok');
    });
    lyr.on('error', function() {
      recordStatus(opts.name, 'CNRFC image failed to load. This confirms it is less robust than NOAA MapServer layers.', 'hydro-bad');
    });
    lyr.on('remove', function() {
      delete activeLegendDefs[opts.name];
      redrawLegend();
      recordStatus(opts.name, 'Layer turned off.', 'hydro-muted');
    });
    return lyr;
  }

  // --------------------------------------------------------------
  // Service endpoint definitions.
  // --------------------------------------------------------------
  var MRMS = 'https://mapservices.weather.noaa.gov/raster/rest/services/obs/mrms_qpe/ImageServer';
  var RFC_QPE = 'https://mapservices.weather.noaa.gov/raster/rest/services/obs/rfc_qpe/MapServer';
  var WPC_QPF = 'https://mapservices.weather.noaa.gov/vector/rest/services/precip/wpc_qpf/MapServer';
  var WPC_ERO = 'https://mapservices.weather.noaa.gov/vector/rest/services/hazards/wpc_precip_hazards/MapServer';
  var NWM = 'https://mapservices.weather.noaa.gov/vector/rest/services/obs/NWM_Stream_Analysis/MapServer';

  var overlays = {};

  function addOverlay(name, layer) {
    overlays[name] = layer;
  }

  // --------------------------------------------------------------
  // MRMS QPE raster-function layers.
  // --------------------------------------------------------------
  addOverlay('QPE | MRMS 1 hr', new ArcGISExportLayer({
    name: 'QPE | MRMS 1 hr',
    serviceType: 'ImageServer',
    url: MRMS,
    rasterFunction: 'rft_1hr',
    opacity: 0.58,
    legendType: 'qpe',
    note: 'Radar-only MRMS QPE; usually updates near :04 after the hour.'
  }));

  addOverlay('QPE | MRMS 6 hr', new ArcGISExportLayer({
    name: 'QPE | MRMS 6 hr',
    serviceType: 'ImageServer',
    url: MRMS,
    rasterFunction: 'rft_6hr',
    opacity: 0.58,
    legendType: 'qpe',
    note: 'Radar-only MRMS QPE; usually updates near :04 after the hour.'
  }));

  addOverlay('QPE | MRMS 24 hr', new ArcGISExportLayer({
    name: 'QPE | MRMS 24 hr',
    serviceType: 'ImageServer',
    url: MRMS,
    rasterFunction: 'rft_24hr',
    opacity: 0.60,
    legendType: 'qpe',
    note: 'Radar-only MRMS QPE; useful for recent storm totals.'
  }));

  addOverlay('QPE | MRMS 72 hr', new ArcGISExportLayer({
    name: 'QPE | MRMS 72 hr',
    serviceType: 'ImageServer',
    url: MRMS,
    rasterFunction: 'rft_72hr',
    opacity: 0.60,
    legendType: 'qpe',
    note: 'Radar-only MRMS QPE; 72-hour accumulation.'
  }));

  // --------------------------------------------------------------
  // RFC / NWPS QPE layers. The Image child layer IDs are used.
  // --------------------------------------------------------------
  addOverlay('QPE | RFC Since 12Z', new ArcGISExportLayer({
    name: 'QPE | RFC Since 12Z',
    serviceType: 'MapServer',
    url: RFC_QPE,
    layers: [4],
    opacity: 0.62,
    legendType: 'qpe',
    note: 'RFC multisensor QPE mosaic; hourly products update near :55.'
  }));

  addOverlay('QPE | RFC Last 1 hr', new ArcGISExportLayer({
    name: 'QPE | RFC Last 1 hr',
    serviceType: 'MapServer',
    url: RFC_QPE,
    layers: [8],
    opacity: 0.62,
    legendType: 'qpe',
    note: 'RFC multisensor QPE mosaic; last 1 hour.'
  }));

  addOverlay('QPE | RFC Last 6 hr', new ArcGISExportLayer({
    name: 'QPE | RFC Last 6 hr',
    serviceType: 'MapServer',
    url: RFC_QPE,
    layers: [20],
    opacity: 0.62,
    legendType: 'qpe',
    note: 'RFC multisensor QPE mosaic; last 6 hours.'
  }));

  addOverlay('QPE | RFC Last 24 hr', new ArcGISExportLayer({
    name: 'QPE | RFC Last 24 hr',
    serviceType: 'MapServer',
    url: RFC_QPE,
    layers: [28],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'RFC multisensor QPE mosaic; last 24 hours.'
  }));

  addOverlay('QPE | RFC Last 7 days', new ArcGISExportLayer({
    name: 'QPE | RFC Last 7 days',
    serviceType: 'MapServer',
    url: RFC_QPE,
    layers: [56],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'RFC multisensor QPE mosaic; 7-day accumulation ending 12Z.'
  }));

  addOverlay('QPE | RFC Water Year to Date', new ArcGISExportLayer({
    name: 'QPE | RFC Water Year to Date',
    serviceType: 'MapServer',
    url: RFC_QPE,
    layers: [100],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'RFC QPE water-year-to-date accumulation.'
  }));

  addOverlay('QPE | RFC Water Year % Normal', new ArcGISExportLayer({
    name: 'QPE | RFC Water Year % Normal',
    serviceType: 'MapServer',
    url: RFC_QPE,
    layers: [259],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'RFC QPE compared with PRISM normals; useful but less event-oriented.'
  }));

  // --------------------------------------------------------------
  // WPC QPF layers.
  // --------------------------------------------------------------
  addOverlay('QPF | WPC Day 1', new ArcGISExportLayer({
    name: 'QPF | WPC Day 1',
    serviceType: 'MapServer',
    url: WPC_QPF,
    layers: [1],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'WPC 24-hour Day 1 QPF; service updates twice daily at 06Z and 18Z.'
  }));

  addOverlay('QPF | WPC Day 2', new ArcGISExportLayer({
    name: 'QPF | WPC Day 2',
    serviceType: 'MapServer',
    url: WPC_QPF,
    layers: [2],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'WPC 24-hour Day 2 QPF.'
  }));

  addOverlay('QPF | WPC Day 3', new ArcGISExportLayer({
    name: 'QPF | WPC Day 3',
    serviceType: 'MapServer',
    url: WPC_QPF,
    layers: [3],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'WPC 24-hour Day 3 QPF.'
  }));

  addOverlay('QPF | WPC Days 1-3', new ArcGISExportLayer({
    name: 'QPF | WPC Days 1-3',
    serviceType: 'MapServer',
    url: WPC_QPF,
    layers: [9],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'WPC 72-hour cumulative QPF for Days 1-3.'
  }));

  addOverlay('QPF | WPC Days 1-5', new ArcGISExportLayer({
    name: 'QPF | WPC Days 1-5',
    serviceType: 'MapServer',
    url: WPC_QPF,
    layers: [10],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'WPC 120-hour cumulative QPF for Days 1-5.'
  }));

  addOverlay('QPF | WPC Days 1-7', new ArcGISExportLayer({
    name: 'QPF | WPC Days 1-7',
    serviceType: 'MapServer',
    url: WPC_QPF,
    layers: [11],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'WPC 168-hour cumulative QPF for Days 1-7.'
  }));

  addOverlay('QPF | WPC 00-06 hr', new ArcGISExportLayer({
    name: 'QPF | WPC 00-06 hr',
    serviceType: 'MapServer',
    url: WPC_QPF,
    layers: [13],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'WPC 6-hour interval QPF.'
  }));

  addOverlay('QPF | WPC 06-12 hr', new ArcGISExportLayer({
    name: 'QPF | WPC 06-12 hr',
    serviceType: 'MapServer',
    url: WPC_QPF,
    layers: [14],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'WPC 6-hour interval QPF.'
  }));

  addOverlay('QPF | WPC 12-18 hr', new ArcGISExportLayer({
    name: 'QPF | WPC 12-18 hr',
    serviceType: 'MapServer',
    url: WPC_QPF,
    layers: [15],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'WPC 6-hour interval QPF.'
  }));

  addOverlay('QPF | WPC 18-24 hr', new ArcGISExportLayer({
    name: 'QPF | WPC 18-24 hr',
    serviceType: 'MapServer',
    url: WPC_QPF,
    layers: [16],
    opacity: 0.64,
    legendType: 'qpe',
    note: 'WPC 6-hour interval QPF.'
  }));

  // --------------------------------------------------------------
  // WPC Excessive Rainfall Outlook hazard polygons.
  // --------------------------------------------------------------
  addOverlay('Hazard | WPC ERO Day 1', new ArcGISExportLayer({
    name: 'Hazard | WPC ERO Day 1',
    serviceType: 'MapServer',
    url: WPC_ERO,
    layers: [0],
    opacity: 0.55,
    legendType: 'ero',
    note: 'Excessive Rainfall Outlook Day 1; risk of flash-flood guidance exceedance.'
  }));

  addOverlay('Hazard | WPC ERO Day 2', new ArcGISExportLayer({
    name: 'Hazard | WPC ERO Day 2',
    serviceType: 'MapServer',
    url: WPC_ERO,
    layers: [1],
    opacity: 0.55,
    legendType: 'ero',
    note: 'Excessive Rainfall Outlook Day 2.'
  }));

  addOverlay('Hazard | WPC ERO Day 3', new ArcGISExportLayer({
    name: 'Hazard | WPC ERO Day 3',
    serviceType: 'MapServer',
    url: WPC_ERO,
    layers: [2],
    opacity: 0.55,
    legendType: 'ero',
    note: 'Excessive Rainfall Outlook Day 3.'
  }));

  addOverlay('Hazard | WPC ERO Day 4', new ArcGISExportLayer({
    name: 'Hazard | WPC ERO Day 4',
    serviceType: 'MapServer',
    url: WPC_ERO,
    layers: [3],
    opacity: 0.55,
    legendType: 'ero',
    note: 'Excessive Rainfall Outlook Day 4.'
  }));

  addOverlay('Hazard | WPC ERO Day 5', new ArcGISExportLayer({
    name: 'Hazard | WPC ERO Day 5',
    serviceType: 'MapServer',
    url: WPC_ERO,
    layers: [4],
    opacity: 0.55,
    legendType: 'ero',
    note: 'Excessive Rainfall Outlook Day 5.'
  }));

  // --------------------------------------------------------------
  // National Water Model streamflow, kept here as a useful comparator.
  // It is off by default. If it is denied, wait and retry later.
  // --------------------------------------------------------------
  addOverlay('Flow | NWM Streamflow Analysis', new ArcGISExportLayer({
    name: 'Flow | NWM Streamflow Analysis',
    serviceType: 'MapServer',
    url: NWM,
    // The NWM streamflow service has zoom/size-specific streamflow sublayers.
    // Showing the broader streamflow group often works, but if it fails, test
    // specific child layers after inspecting the service metadata.
    layers: [0,1,2,3,4,5,6,7,8,9],
    opacity: 0.70,
    legendType: null,
    note: 'Modeled NWM streamflow analysis, cfs. Potentially request-protected; test gently.'
  }));

  // --------------------------------------------------------------
  // Experimental CNRFC QPF PNG overlays.
  // These are not MapServer layers, but they are simple and lightweight.
  // Bounds are from the associated CNRFC KML GroundOverlay metadata.
  // --------------------------------------------------------------
  addOverlay('EXP | CNRFC QPF 24 hr PNG', makeFixedImageLayer({
    name: 'EXP | CNRFC QPF 24 hr PNG',
    url: 'https://www.cnrfc.noaa.gov/data/kml/24hr_QPF_1.png',
    north: 43.375,
    south: 32.5,
    east: -114.105,
    west: -124.48,
    opacity: 0.65,
    legendType: 'qpe',
    note: 'Experimental direct CNRFC PNG from KML product. Good to test, less robust than NOAA MapServer.'
  }));

  addOverlay('EXP | CNRFC QPF 72 hr PNG', makeFixedImageLayer({
    name: 'EXP | CNRFC QPF 72 hr PNG',
    url: 'https://www.cnrfc.noaa.gov/data/kml/72hr_QPF_1.png',
    north: 43.375,
    south: 32.5,
    east: -114.105,
    west: -124.48,
    opacity: 0.65,
    legendType: 'qpe',
    note: 'Experimental direct CNRFC PNG from KML product. Good to test, less robust than NOAA MapServer.'
  }));

  addOverlay('EXP | CNRFC QPF 6-day PNG', makeFixedImageLayer({
    name: 'EXP | CNRFC QPF 6-day PNG',
    url: 'https://www.cnrfc.noaa.gov/data/kml/6day_QPF_1.png',
    north: 43.375,
    south: 32.5,
    east: -114.105,
    west: -124.48,
    opacity: 0.65,
    legendType: 'qpe',
    note: 'Experimental direct CNRFC PNG from KML product. Good to test, less robust than NOAA MapServer.'
  }));

  // --------------------------------------------------------------
  // Add the layer control.
  // --------------------------------------------------------------
  var layerControl = L.control.layers(null, overlays, {
    collapsed: false,
    position: 'topright'
  }).addTo(map);

  linksCtl.addTo(map);

  // Optional: turn one layer on by default so the user knows the live layers work.
  // Comment this line out if you want the map to open with only the basemap.
  overlays['QPE | RFC Last 24 hr'].addTo(map);

  // --------------------------------------------------------------
  // Best-effort service freshness / metadata checks.
  // These do not control layer rendering. They only populate the status box.
  // --------------------------------------------------------------
  function checkReturnUpdates(name, baseUrl, expected) {
    var url = safeUrlBase(baseUrl) + '/returnUpdates?f=pjson&_=' + Date.now();
    fetch(url)
      .then(function(resp) { return resp.json(); })
      .then(function(json) {
        var text = '';
        if (json && json.fullUpdate) text += 'fullUpdate: ' + json.fullUpdate + '; ';
        if (json && json.layers && json.layers.length) {
          text += 'layers reported: ' + json.layers.length + '; ';
        }
        if (!text) text = 'metadata returned; ';
        recordStatus(name + ' metadata', text + 'expected: ' + expected, 'hydro-ok');
      })
      .catch(function() {
        recordStatus(name + ' metadata', 'Freshness request failed; expected: ' + expected, 'hydro-warn');
      });
  }

  function checkMrmsLatest() {
    var url = safeUrlBase(MRMS) + '/query?f=json&where=1%3D1&outFields=name,idp_validendtime,idp_filedate,idp_ingestdate&returnGeometry=false&orderByFields=idp_validendtime%20DESC&resultRecordCount=1&_=' + Date.now();
    fetch(url)
      .then(function(resp) { return resp.json(); })
      .then(function(json) {
        var f = json && json.features && json.features[0] && json.features[0].attributes;
        if (!f) throw new Error('No feature attributes returned');
        var valid = f.idp_validendtime ? fmtTime(new Date(f.idp_validendtime)) : 'unknown valid time';
        recordStatus('MRMS metadata', 'Latest valid/end time: ' + valid + '; expected update near :04.', 'hydro-ok');
      })
      .catch(function() {
        recordStatus('MRMS metadata', 'Freshness request failed; expected update near :04 after hour.', 'hydro-warn');
      });
  }

  // Run metadata checks once at page load. These are lightweight and best-effort.
  checkMrmsLatest();
  checkReturnUpdates('RFC QPE', RFC_QPE, 'hourly near :55; daily products can update several times 12Z-21Z');
  checkReturnUpdates('WPC QPF', WPC_QPF, 'twice daily at 06Z and 18Z');
  checkReturnUpdates('WPC ERO', WPC_ERO, 'Day 1 required 0100Z/0830Z/1500Z; updates possible anytime');
}
)"

m <- htmlwidgets::onRender(m, js_code)

# ----------------------------------------------------------------
# Save the standalone HTML.
# ----------------------------------------------------------------
htmlwidgets::saveWidget(widget = m, file = OUT_HTML, selfcontained = TRUE)
message("Wrote: ", normalizePath(OUT_HTML))
