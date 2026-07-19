# pilot_operational_hydro_leaflet_v5.R
#
# Purpose:
#   Create a standalone/shareable R-made Leaflet HTML test map with live
#   operational weather + hydrology image layers.
#
# v5 changes from v4:
#   1) Uses ONE dynamic image request per map view for NOAA ArcGIS services,
#      instead of pseudo-tiling /export requests. This is much gentler on
#      mapservices.weather.noaa.gov and should reduce "request denied" behavior.
#   2) Adds a legend panel. WPC and NWM try to read the official ArcGIS legend
#      endpoint. MRMS has a schematic precipitation legend because the ImageServer
#      rendering-rule legend is less dependable in browser-only use.
#   3) Removes NWPS/USGS points from the default pilot because those were
#      redundant with your existing point layers and the v4 NWPS layer was an
#      image, not clickable.
#
# What this produces:
#   output/pilot_operational_hydro_leaflet_v5.html
#
# Important note:
#   The HTML is one shareable file, but it is not offline. When opened, it
#   still fetches live NOAA services and basemap tiles from the internet.

# ==== 1: Packages ====

## Install these once if needed:
## install.packages(c("leaflet", "htmlwidgets", "htmltools", "jsonlite"))

library(leaflet)
library(htmlwidgets)
library(htmltools)
library(jsonlite)

# ==== 2: Output path ====

OUTPUT_DIR <- "output"
OUT_HTML   <- file.path(OUTPUT_DIR, "pilot_operational_hydro_leaflet_v5.html")

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# ==== 3: Live service endpoints ====

## NOAA/NWS MRMS QPE ImageServer.
MRMS_QPE_URL <- "https://mapservices.weather.noaa.gov/raster/rest/services/obs/mrms_qpe/ImageServer"

## NOAA/NWS National Water Model Stream Analysis MapServer.
NWM_STREAM_URL <- "https://mapservices.weather.noaa.gov/vector/rest/services/obs/NWM_Stream_Analysis/MapServer"

## NOAA/NWS Weather Prediction Center QPF MapServer.
WPC_QPF_URL <- "https://mapservices.weather.noaa.gov/vector/rest/services/precip/wpc_qpf/MapServer"

# ==== 4: Map extent and title ====

## California-ish starting extent. Adjust as needed.
start_lng  <- -119.5
start_lat  <-   37.3
start_zoom <-    6

map_title <- tags$div(
  style = paste(
    "position:absolute; z-index:9999; top:10px; left:55px;",
    "background:rgba(255,255,255,0.92); padding:10px 12px;",
    "border-radius:8px; box-shadow:0 1px 5px rgba(0,0,0,0.35);",
    "font-family:Arial, sans-serif; font-size:13px; line-height:1.25; max-width:460px;"
  ),
  tags$b("Operational Hydrology Pilot v5"), tags$br(),
  "Live NOAA layers: MRMS 24-hr QPE, NWM streamflow analysis, WPC QPF", tags$br(),
  tags$span(style = "font-size:11px; color:#555;", "v5 uses one dynamic image per map view, not pseudo-tiles.")
)

map_css <- tags$style(HTML("
  .leaflet-control-layers {
    font-family: Arial, sans-serif;
    font-size: 13px;
    max-width: 380px;
  }
  .leaflet-control-layers-overlays label {
    margin-bottom: 4px;
  }
  .hydro-status {
    background: rgba(255,255,255,0.92);
    padding: 6px 8px;
    border-radius: 6px;
    font: 11px Arial, sans-serif;
    box-shadow: 0 1px 4px rgba(0,0,0,0.25);
    max-width: 380px;
    max-height: 120px;
    overflow-y: auto;
  }
  .hydro-status .ok { color: #146c2e; }
  .hydro-status .warn { color: #8a4b00; }
  .hydro-status .bad { color: #9b1c1c; }
  .hydro-legend {
    background: rgba(255,255,255,0.94);
    padding: 8px 10px;
    border-radius: 6px;
    font: 11px Arial, sans-serif;
    box-shadow: 0 1px 4px rgba(0,0,0,0.25);
    max-width: 310px;
    max-height: 360px;
    overflow-y: auto;
  }
  .hydro-legend h4 {
    margin: 0 0 4px 0;
    font-size: 12px;
  }
  .hydro-legend .section {
    margin-bottom: 8px;
    padding-bottom: 6px;
    border-bottom: 1px solid #ddd;
  }
  .hydro-legend .legend-row {
    display: flex;
    align-items: center;
    gap: 5px;
    margin: 1px 0;
    white-space: nowrap;
  }
  .hydro-legend .swatch {
    width: 18px;
    height: 11px;
    border: 1px solid rgba(0,0,0,0.25);
    display: inline-block;
    flex: 0 0 18px;
  }
  .hydro-legend img {
    vertical-align: middle;
  }
"))

# ==== 5: Base Leaflet map ====

m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") |>
  setView(lng = start_lng, lat = start_lat, zoom = start_zoom) |>
  addScaleBar(position = "bottomleft")

m <- prependContent(m, map_css, map_title)

# ==== 6: Browser-side JavaScript live layers ====

## This JavaScript runs after Leaflet initializes in the browser.
## It builds ArcGIS REST export/exportImage URLs directly, but uses a single
## image overlay per current map view. That avoids hammering NOAA services with
## many tile-style export requests.

live_layer_js <- paste0(
"function(el, x) {\n",
"  var map = this;\n",
"\n",
"  // ---- Service endpoints injected from R ----\n",
"  var MRMS_QPE_URL   = ", jsonlite::toJSON(MRMS_QPE_URL, auto_unbox = TRUE), ";\n",
"  var NWM_STREAM_URL = ", jsonlite::toJSON(NWM_STREAM_URL, auto_unbox = TRUE), ";\n",
"  var WPC_QPF_URL    = ", jsonlite::toJSON(WPC_QPF_URL, auto_unbox = TRUE), ";\n",
"\n",
"  // ---- Layer control ----\n",
"  var overlays = {};\n",
"  var layerControl = L.control.layers(null, overlays, {\n",
"    collapsed: false,\n",
"    position: 'topright'\n",
"  }).addTo(map);\n",
"\n",
"  // ---- Status/debug box ----\n",
"  var statusDiv = null;\n",
"  var status = L.control({ position: 'bottomright' });\n",
"  status.onAdd = function() {\n",
"    statusDiv = L.DomUtil.create('div', 'hydro-status');\n",
"    statusDiv.innerHTML = '<b>Live-layer status</b><br>';\n",
"    return statusDiv;\n",
"  };\n",
"  status.addTo(map);\n",
"\n",
"  function logStatus(msg, cls) {\n",
"    cls = cls || 'ok';\n",
"    if (statusDiv) {\n",
"      statusDiv.innerHTML += '<span class=\"' + cls + '\">' + msg + '</span><br>';\n",
"    }\n",
"    if (cls === 'bad') console.error(msg);\n",
"    else if (cls === 'warn') console.warn(msg);\n",
"    else console.log(msg);\n",
"  }\n",
"\n",
"  function addOverlay(name, layer, addByDefault) {\n",
"    overlays[name] = layer;\n",
"    layerControl.addOverlay(layer, name);\n",
"    if (addByDefault) { layer.addTo(map); }\n",
"    logStatus('Added control: ' + name, 'ok');\n",
"  }\n",
"\n",
"  function debounce(fn, wait) {\n",
"    var t = null;\n",
"    return function() {\n",
"      var ctx = this, args = arguments;\n",
"      clearTimeout(t);\n",
"      t = setTimeout(function() { fn.apply(ctx, args); }, wait);\n",
"    };\n",
"  }\n",
"\n",
"  // ---- Web Mercator helpers ----\n",
"  function clampLat(lat) { return Math.max(Math.min(lat, 85.05112878), -85.05112878); }\n",
"  function mercatorX(lon) { return lon * 20037508.342789244 / 180.0; }\n",
"  function mercatorY(lat) {\n",
"    lat = clampLat(lat);\n",
"    var rad = lat * Math.PI / 180.0;\n",
"    return Math.log(Math.tan(Math.PI / 4.0 + rad / 2.0)) * 6378137.0;\n",
"  }\n",
"  function mercatorBbox(bounds) {\n",
"    var west = bounds.getWest(), east = bounds.getEast();\n",
"    var south = bounds.getSouth(), north = bounds.getNorth();\n",
"    return [mercatorX(west), mercatorY(south), mercatorX(east), mercatorY(north)].join(',');\n",
"  }\n",
"\n",
"  // ---- Single-image ArcGIS REST layer ----\n",
"  var SingleArcGISImageLayer = L.Layer.extend({\n",
"    initialize: function(opts) {\n",
"      this.opts = opts || {};\n",
"      this._overlay = null;\n",
"      this._requestId = 0;\n",
"      this._loadedOnce = false;\n",
"      this._scheduleUpdate = debounce(this._update.bind(this), 350);\n",
"    },\n",
"    onAdd: function(mapRef) {\n",
"      this._map = mapRef;\n",
"      mapRef.on('moveend zoomend resize', this._scheduleUpdate, this);\n",
"      this._update();\n",
"    },\n",
"    onRemove: function(mapRef) {\n",
"      mapRef.off('moveend zoomend resize', this._scheduleUpdate, this);\n",
"      this._requestId++;\n",
"      if (this._overlay) {\n",
"        mapRef.removeLayer(this._overlay);\n",
"        this._overlay = null;\n",
"      }\n",
"    },\n",
"    setOpacity: function(opacity) {\n",
"      this.opts.opacity = opacity;\n",
"      if (this._overlay) this._overlay.setOpacity(opacity);\n",
"    },\n",
"    _buildUrl: function(bounds) {\n",
"      var opts = this.opts;\n",
"      var base = opts.url.replace(/\\/$/, '');\n",
"      var size = this._map.getSize();\n",
"      var w = Math.max(256, Math.min(4096, Math.round(size.x)));\n",
"      var h = Math.max(256, Math.min(4096, Math.round(size.y)));\n",
"      var params = [\n",
"        'bbox=' + mercatorBbox(bounds),\n",
"        'bboxSR=102100',\n",
"        'imageSR=102100',\n",
"        'size=' + w + ',' + h,\n",
"        'dpi=96',\n",
"        'format=png32',\n",
"        'transparent=true',\n",
"        'f=image'\n",
"      ];\n",
"\n",
"      if (opts.type === 'map') {\n",
"        params.push('layers=show:' + opts.layers.join(','));\n",
"        return base + '/export?' + params.join('&');\n",
"      }\n",
"\n",
"      if (opts.type === 'image') {\n",
"        if (opts.renderingRule) {\n",
"          params.push('renderingRule=' + encodeURIComponent(JSON.stringify(opts.renderingRule)));\n",
"        }\n",
"        return base + '/exportImage?' + params.join('&');\n",
"      }\n",
"\n",
"      throw new Error('Unknown ArcGIS export type: ' + opts.type);\n",
"    },\n",
"    _update: function() {\n",
"      if (!this._map || !this._map.hasLayer(this)) return;\n",
"      var bounds = this._map.getBounds().pad(0.03);\n",
"      var url = this._buildUrl(bounds);\n",
"      var id = ++this._requestId;\n",
"      var self = this;\n",
"      var preloader = new Image();\n",
"\n",
"      preloader.onload = function() {\n",
"        if (!self._map || !self._map.hasLayer(self) || id !== self._requestId) return;\n",
"        var newOverlay = L.imageOverlay(url, bounds, {\n",
"          opacity: self.opts.opacity || 1.0,\n",
"          interactive: false,\n",
"          attribution: self.opts.attribution || ''\n",
"        });\n",
"        newOverlay.addTo(self._map);\n",
"        if (self._overlay) self._map.removeLayer(self._overlay);\n",
"        self._overlay = newOverlay;\n",
"        if (!self._loadedOnce) {\n",
"          logStatus('Loaded image: ' + (self.opts.name || 'ArcGIS layer'), 'ok');\n",
"          self._loadedOnce = true;\n",
"        }\n",
"      };\n",
"\n",
"      preloader.onerror = function() {\n",
"        logStatus('Image request failed: ' + (self.opts.name || 'ArcGIS layer'), 'warn');\n",
"        console.warn('Failed ArcGIS image URL:', url);\n",
"      };\n",
"\n",
"      preloader.src = url;\n",
"    }\n",
"  });\n",
"\n",
"  function singleArcGISImageLayer(opts) {\n",
"    return new SingleArcGISImageLayer(opts);\n",
"  }\n",
"\n",
"  // ---- 1. MRMS QPE: 24-hour observed precipitation estimate ----\n",
"  var mrms24 = singleArcGISImageLayer({\n",
"    name: 'MRMS QPE - 24 hr',\n",
"    type: 'image',\n",
"    url: MRMS_QPE_URL,\n",
"    renderingRule: { rasterFunction: 'rft_24hr' },\n",
"    opacity: 0.58,\n",
"    attribution: 'NOAA/NWS MRMS'\n",
"  });\n",
"  addOverlay('MRMS QPE - 24 hr observed precip', mrms24, true);\n",
"\n",
"  // ---- 2. NWM Stream Analysis: streamflow in cfs ----\n",
"  // Sublayers 1-6 are the streamflow group with scale-dependent simplification.\n",
"  var nwmStreamflow = singleArcGISImageLayer({\n",
"    name: 'NWM streamflow analysis',\n",
"    type: 'map',\n",
"    url: NWM_STREAM_URL,\n",
"    layers: [1, 2, 3, 4, 5, 6],\n",
"    opacity: 0.78,\n",
"    attribution: 'NOAA/NWS NWM'\n",
"  });\n",
"  addOverlay('NWM streamflow analysis - cfs', nwmStreamflow, true);\n",
"\n",
"  // ---- 3. WPC QPF forecast precipitation ----\n",
"  var wpcDay1 = singleArcGISImageLayer({\n",
"    name: 'WPC QPF Day 1',\n",
"    type: 'map',\n",
"    url: WPC_QPF_URL,\n",
"    layers: [1],\n",
"    opacity: 0.48,\n",
"    attribution: 'NOAA/NWS WPC'\n",
"  });\n",
"  addOverlay('WPC QPF - Day 1', wpcDay1, false);\n",
"\n",
"  var wpcDays13 = singleArcGISImageLayer({\n",
"    name: 'WPC QPF Days 1-3',\n",
"    type: 'map',\n",
"    url: WPC_QPF_URL,\n",
"    layers: [9],\n",
"    opacity: 0.43,\n",
"    attribution: 'NOAA/NWS WPC'\n",
"  });\n",
"  addOverlay('WPC QPF - Days 1-3', wpcDays13, false);\n",
"\n",
"  var wpcDays17 = singleArcGISImageLayer({\n",
"    name: 'WPC QPF Days 1-7',\n",
"    type: 'map',\n",
"    url: WPC_QPF_URL,\n",
"    layers: [11],\n",
"    opacity: 0.38,\n",
"    attribution: 'NOAA/NWS WPC'\n",
"  });\n",
"  addOverlay('WPC QPF - Days 1-7', wpcDays17, false);\n",
"\n",
"  // ---- Legend control ----\n",
"  var legendDiv = null;\n",
"  var legend = L.control({ position: 'bottomleft' });\n",
"  legend.onAdd = function() {\n",
"    legendDiv = L.DomUtil.create('div', 'hydro-legend');\n",
"    legendDiv.innerHTML = '<h4>Legends</h4>';\n",
"    return legendDiv;\n",
"  };\n",
"  legend.addTo(map);\n",
"\n",
"  function addSection(title, id) {\n",
"    var sec = document.createElement('div');\n",
"    sec.className = 'section';\n",
"    sec.id = id;\n",
"    sec.innerHTML = '<b>' + title + '</b><br>';\n",
"    legendDiv.appendChild(sec);\n",
"    return sec;\n",
"  }\n",
"\n",
"  function addManualMrmsLegend() {\n",
"    var sec = addSection('MRMS 24-hr QPE, inches (schematic)', 'legend-mrms');\n",
"    var rows = [\n",
"      ['#d7f5ff', '0.01'], ['#8ee6ff', '0.10'], ['#48b9ff', '0.25'],\n",
"      ['#2486ff', '0.50'], ['#00c853', '1.0'], ['#ffe600', '2.0'],\n",
"      ['#ff9e00', '3.0'], ['#ff0000', '5.0+']\n",
"    ];\n",
"    rows.forEach(function(r) {\n",
"      var row = document.createElement('div');\n",
"      row.className = 'legend-row';\n",
"      row.innerHTML = '<span class=\"swatch\" style=\"background:' + r[0] + '\"></span><span>' + r[1] + '</span>';\n",
"      sec.appendChild(row);\n",
"    });\n",
"  }\n",
"\n",
"  function addArcGISLegend(serviceUrl, layerIds, title, fallbackText) {\n",
"    var sec = addSection(title, 'legend-' + title.toLowerCase().replace(/[^a-z0-9]+/g, '-'));\n",
"    sec.innerHTML += '<span style=\"color:#666\">Loading official legend...</span>';\n",
"\n",
"    fetch(serviceUrl.replace(/\\/$/, '') + '/legend?f=pjson')\n",
"      .then(function(resp) {\n",
"        if (!resp.ok) throw new Error('HTTP ' + resp.status);\n",
"        return resp.json();\n",
"      })\n",
"      .then(function(data) {\n",
"        sec.innerHTML = '<b>' + title + '</b><br>';\n",
"        var layers = ((data || {}).layers || []).filter(function(ly) {\n",
"          return layerIds.indexOf(ly.layerId) >= 0;\n",
"        });\n",
"        var n = 0;\n",
"        layers.forEach(function(ly) {\n",
"          (ly.legend || []).forEach(function(item) {\n",
"            if (!item.imageData) return;\n",
"            var row = document.createElement('div');\n",
"            row.className = 'legend-row';\n",
"            var mime = item.contentType || 'image/png';\n",
"            var label = item.label || '';\n",
"            row.innerHTML = '<img width=\"' + (item.width || 20) + '\" height=\"' + (item.height || 12) + '\" src=\"data:' + mime + ';base64,' + item.imageData + '\">' +\n",
"                            '<span>' + label + '</span>';\n",
"            sec.appendChild(row);\n",
"            n++;\n",
"          });\n",
"        });\n",
"        if (n === 0) throw new Error('No legend items returned');\n",
"      })\n",
"      .catch(function(err) {\n",
"        sec.innerHTML = '<b>' + title + '</b><br><span style=\"color:#8a4b00\">' + fallbackText + '</span>';\n",
"        logStatus('Legend fetch failed for ' + title + ': ' + err.message, 'warn');\n",
"      });\n",
"  }\n",
"\n",
"  addManualMrmsLegend();\n",
"  addArcGISLegend(NWM_STREAM_URL, [1], 'NWM streamflow, cfs', 'Official legend unavailable; use layer symbology as visual guide.');\n",
"  addArcGISLegend(WPC_QPF_URL, [1], 'WPC QPF Day 1, inches', 'Official legend unavailable; WPC colors indicate forecast precip depth.');\n",
"\n",
"  logStatus('Layer-control setup complete. Toggle layers at upper right.', 'ok');\n",
"}\n"
)

m <- htmlwidgets::onRender(m, live_layer_js)

# ==== 7: Save HTML ====

## selfcontained = TRUE usually works in RStudio because it includes Pandoc.
## The resulting file is easier to share, but it still fetches live services
## from the internet when opened.
htmlwidgets::saveWidget(widget = m, file = OUT_HTML, selfcontained = TRUE)
message("Wrote: ", normalizePath(OUT_HTML))

# ==== 8: Quick troubleshooting notes ====

## 1. If NWM stops loading, wait a few minutes and retry with this v5 script.
##    v4 used pseudo-tiles, which can create many export requests quickly.
##
## 2. If the legends do not load, the map itself may still be fine. The WPC/NWM
##    legends are fetched live from /legend?f=pjson. Some networks may block
##    those JSON requests while still allowing image requests.
##
## 3. The NOAA layers are image overlays. They are intentionally not clickable.
##    Use your own gage/station point layers for popups and site-specific data.
