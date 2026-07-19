# ==== leaflet_ops_live_arcgis_export_helpers.r =============================
##
## PURPOSE:
##   Dynamic ArcGIS export/image overlay helpers and non-ArcGIS raster tile helpers.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r`.
##   It returns browser-side JavaScript as text for injection into the Ops Live
##   htmlwidgets/onRender function. Keep edits narrow and feature-specific.
## ============================================================================

pt_ops_live_arcgis_export_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Dynamic ArcGIS export layer.
  // --------------------------------------------------------------------------
  var ArcGISExportLayer = L.Layer.extend({
    initialize: function(options) {
      this.options = options || {};
      this._overlay = null;
      this._timer = null;
      this._pendingOverlay = null;
      this._isRemoved = true;

      // Only expose the public Ops-row rfrsh hook when a layer explicitly
      // opts in.  ArcGISExportLayer is also used by many auto-updating raster
      // and image-heavy Ops products; those should not all sprout rfrsh links
      // just because this class knows how to rebuild an export image.
      if (this.options.refreshable === true) {
        this.refreshCurrentView = function() {
          if (this._isRemoved || !this._map) return;

          if (this._timer) {
            window.clearTimeout(this._timer);
            this._timer = null;
          }

          if (this._pendingOverlay && this._map) {
            try { this._map.removeLayer(this._pendingOverlay); } catch(e) {}
          }
          this._pendingOverlay = null;

          recordStatus(
            this.options.name,
            'Refreshing live image for the current map view…',
            'pt-ops-warn'
          );
          this._update();
        };
      }
    },
    onAdd: function(mapObj) {
      this._map = mapObj;
      this._isRemoved = false;
      activeLegendDefs[this.options.name] = {
        note: this.options.note || '',
        legendType: this.options.legendType || null,
        sourceUrl: this.options.sourceUrl || this.options.url || '',
        legendUrl: this.options.legendUrl || '',
        infoUrl: this.options.infoUrl || '',
        infoLabel: this.options.infoLabel || '',
        legendNote: this.options.legendNote || ''
      };
      redrawLegend();
      this._scheduleUpdate();
      mapObj.on('moveend zoomend resize', this._scheduleUpdate, this);
      recordStatus(this.options.name, 'Requested live image.', 'pt-ops-warn');
      if (typeof this.options.checkFreshness === 'function') {
        this.options.checkFreshness();
      }
    },
    _removeInternalOverlays: function(mapObj) {
      var m = mapObj || this._map;

      if (this._timer) {
        window.clearTimeout(this._timer);
        this._timer = null;
      }

      if (m && this._overlay) {
        try { m.removeLayer(this._overlay); } catch(e) {}
      }
      this._overlay = null;

      if (m && this._pendingOverlay) {
        try { m.removeLayer(this._pendingOverlay); } catch(e) {}
      }
      this._pendingOverlay = null;
    },
    forceRemove: function(mapObj) {
      this._isRemoved = true;
      this._removeInternalOverlays(mapObj || this._map);
      this._map = null;
    },
    onRemove: function(mapObj) {
      this._isRemoved = true;
      mapObj.off('moveend zoomend resize', this._scheduleUpdate, this);
      this._removeInternalOverlays(mapObj);
      this._map = null;
      delete activeLegendDefs[this.options.name];
      setOpsLayerLoading(this.options.name, false);
      redrawLegend();
      recordStatus(this.options.name, 'Layer turned off.', 'pt-ops-muted');
    },
    _scheduleUpdate: function() {
      if (this._isRemoved || !this._map) return;
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
        var imageUrl = base + '/exportImage?' + common + '&format=png';

        // Some ImageServer products need a named raster function (for example
        // MRMS QPE accumulations), while NOAA/NESDIS satellite ImageServers are
        // already styled as display images.  Only send renderingRule when the
        // layer explicitly asks for it; an empty renderingRule can make some
        // image services fail.
        if (opts.rasterFunction) {
          var rr = encodeURIComponent(JSON.stringify({rasterFunction: opts.rasterFunction}));
          imageUrl += '&renderingRule=' + rr;
        }

        return imageUrl + '&_=' + Date.now();
      }
      var layerText = Array.isArray(opts.layers) ? opts.layers.join(',') : String(opts.layers);
      return base + '/export?' + common + '&format=png32&dpi=96&layers=show:' + encodeURIComponent(layerText) + '&_=' + Date.now();
    },
    _update: function() {
      if (!this._map || this._isRemoved) return;
      var self = this;
      setOpsLayerLoading(this.options.name, true);
      var b = this._map.getBounds();
      var url = this._buildUrl();
      var img = L.imageOverlay(url, b, {
        opacity: this.options.opacity || 0.65,
        interactive: false,
        crossOrigin: false,
        pane: 'pane_ops'
      });
      this._pendingOverlay = img;
      img.on('load', function() {
        if (self._isRemoved || !self._map || !self._map.hasLayer(self)) {
          try { if (self._map) self._map.removeLayer(img); } catch(e) {}
          if (self._pendingOverlay === img) self._pendingOverlay = null;
          return;
        }
        if (self._overlay && self._overlay !== img) {
          try { self._map.removeLayer(self._overlay); } catch(e) {}
        }
        self._overlay = img;
        self._pendingOverlay = null;
        setOpsLayerLoading(self.options.name, false);
        recordStatus(self.options.name, 'Loaded live image successfully.', 'pt-ops-ok');
      });
      img.on('error', function() {
        try { if (self._map) self._map.removeLayer(img); } catch(e) {}
        if (self._pendingOverlay === img) self._pendingOverlay = null;
        if (self._isRemoved || !self._map || !self._map.hasLayer(self)) return;
        setOpsLayerLoading(self.options.name, false);
        recordStatus(self.options.name, 'Image request failed. Try zooming in, waiting, or toggling fewer Ops layers.', 'pt-ops-bad');
      });
      img.addTo(this._map);
    }
  });
  
  // --------------------------------------------------------------------------
  // WPC Excessive Rainfall Outlook current-view polygon layer.
  // --------------------------------------------------------------------------
  // The previous Ops ERO layer used ArcGIS export-image overlays.  Those draw
  // reliably enough, but they do not expose native Leaflet feature hover/click
  // behavior.  This current-view layer queries polygons intersecting the map
  // extent, lets BRIM draw them as vectors, and provides concise hover/popup
  // summaries similar to the External current-view layers.

  function ptEroEscape(value) {
    return escapeHtml(value === null || value === undefined ? '' : String(value));
  }

  function ptEroParseUtcDate(value) {
    if (value === null || value === undefined || String(value).trim() === '') return null;

    if (typeof value === 'number' || /^\d+$/.test(String(value).trim())) {
      var n = Number(value);
      if (isFinite(n)) return new Date(n);
    }

    var raw = String(value).trim();
    if (/^\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}(:\d{2})?$/.test(raw)) {
      raw = raw.replace(/\s+/, 'T');
      if (/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}$/.test(raw)) raw += ':00';
      raw += 'Z';
    } else if (/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}(:\d{2})?$/.test(raw) && !/[zZ]|[+-]\d{2}:?\d{2}$/.test(raw)) {
      if (/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}$/.test(raw)) raw += ':00';
      raw += 'Z';
    }

    var d = new Date(raw);
    return isNaN(d.getTime()) ? null : d;
  }

  function ptEroFormatPacific(value) {
    var d = ptEroParseUtcDate(value);
    if (!d) return '';
    try {
      return new Intl.DateTimeFormat('en-US', {
        timeZone: 'America/Los_Angeles',
        month: 'short',
        day: 'numeric',
        hour: 'numeric',
        minute: '2-digit',
        timeZoneName: 'short',
        hour12: true
      }).format(d);
    } catch(e) {
      return d.toLocaleString();
    }
  }

  function ptEroFormatUtc(value) {
    var d = ptEroParseUtcDate(value);
    if (!d) return value ? String(value) : '';
    function pad(n) { return String(n).padStart(2, '0'); }
    return d.getUTCFullYear() + '-' + pad(d.getUTCMonth() + 1) + '-' + pad(d.getUTCDate()) + ' ' +
      pad(d.getUTCHours()) + ':' + pad(d.getUTCMinutes()) + ' UTC';
  }

  function ptEroCategory(attrs) {
    attrs = attrs || {};
    var raw = attrs.outlook || attrs.Outlook || attrs['Excessive Rain Outlook'] || attrs.dn || attrs.DN || '';
    var txt = String(raw || '').trim();
    var lower = txt.toLowerCase();
    var code = Number(txt);

    if (lower.indexOf('marginal') >= 0 || code === 1) return 'Marginal excessive rainfall risk';
    if (lower.indexOf('slight') >= 0 || code === 2) return 'Slight excessive rainfall risk';
    if (lower.indexOf('moderate') >= 0 || code === 3) return 'Moderate excessive rainfall risk';
    if (lower.indexOf('high') >= 0 || code === 4) return 'High excessive rainfall risk';

    return txt || 'Excessive rainfall outlook';
  }

  function ptEroInterpretation(category) {
    var c = String(category || '').toLowerCase();
    if (c.indexOf('marginal') >= 0) return 'At least 5% risk of rainfall exceeding flash-flood guidance.';
    if (c.indexOf('slight') >= 0) return 'At least 15% risk of rainfall exceeding flash-flood guidance.';
    if (c.indexOf('moderate') >= 0) return 'At least 40% risk of rainfall exceeding flash-flood guidance.';
    if (c.indexOf('high') >= 0) return 'At least 70% risk of rainfall exceeding flash-flood guidance.';
    return '';
  }

  function ptEroStyle(attrs) {
    var c = ptEroCategory(attrs).toLowerCase();
    var fill = '#38a800';
    var stroke = '#00734c';
    var weight = 2;

    if (c.indexOf('slight') >= 0) { fill = '#fffe00'; stroke = '#e69800'; }
    if (c.indexOf('moderate') >= 0) { fill = '#f50000'; stroke = '#8a0000'; weight = 2.5; }
    if (c.indexOf('high') >= 0) { fill = '#ff69c5'; stroke = '#ff00ff'; weight = 3; }

    return {
      color: stroke,
      weight: weight,
      opacity: 0.85,
      fillColor: fill,
      fillOpacity: 0.28
    };
  }

  function ptEroRawAttributesTable(attrs) {
    attrs = attrs || {};
    var keys = Object.keys(attrs).filter(function(k) {
      return k !== 'shape' && k !== 'st_area(shape)' && k !== 'st_perimeter(shape)';
    });
    if (!keys.length) return '';

    var rows = keys.map(function(k) {
      return '<tr><th style="text-align:left;border:1px solid #ddd;padding:3px 5px;background:#f7f7f7;">' + ptEroEscape(k) + '</th>' +
        '<td style="border:1px solid #ddd;padding:3px 5px;">' + ptEroEscape(attrs[k]) + '</td></tr>';
    }).join('');

    return '<details style="margin-top:8px;"><summary>All returned attributes</summary>' +
      '<table style="border-collapse:collapse;width:100%;font-size:11px;margin-top:5px;">' + rows + '</table></details>';
  }

  function ptEroHoverHtml(layerName, attrs) {
    attrs = attrs || {};
    var cat = ptEroCategory(attrs);
    var interp = ptEroInterpretation(cat);
    var start = ptEroFormatPacific(attrs.start_time || attrs.valid || attrs.valid_time || attrs['Start Time'] || attrs['Valid Time']);
    var end = ptEroFormatPacific(attrs.end_time || attrs.expire || attrs['End Time']);

    var html = '<div><strong>' + ptEroEscape(cat) + '</strong>';
    if (interp) html += '<br><span>' + ptEroEscape(interp) + '</span>';
    if (start || end) html += '<br><span>Valid: ' + ptEroEscape(start || 'n/a') + ' – ' + ptEroEscape(end || 'n/a') + '</span>';
    html += '</div>';
    return html;
  }

  function ptEroPopupHtml(layerName, attrs, sourceUrl) {
    attrs = attrs || {};
    var cat = ptEroCategory(attrs);
    var interp = ptEroInterpretation(cat);
    var startRaw = attrs.start_time || attrs.valid || attrs.valid_time || attrs['Start Time'] || attrs['Valid Time'];
    var endRaw = attrs.end_time || attrs.expire || attrs['End Time'];
    var issueRaw = attrs.issue_time || attrs['Issue Time'] || attrs.idp_filedate || attrs['GIS File Date'];
    var product = attrs.product || attrs.Product || '';

    var html = '<div class="pt-ops-ero-popup"><h4 style="margin:0 0 6px 0;">' + ptEroEscape(layerName) + '</h4>' +
      '<div><b>BRIM summary</b></div>' +
      '<p style="margin:6px 0 8px 0;">' +
      '<b>Category:</b> ' + ptEroEscape(cat) + '<br>' +
      (interp ? '<b>Interpretation:</b> ' + ptEroEscape(interp) + '<br>' : '') +
      (product ? '<b>Product:</b> ' + ptEroEscape(product) + '<br>' : '') +
      '<b>Valid window:</b> ' + ptEroEscape(ptEroFormatPacific(startRaw) || 'n/a') + ' – ' + ptEroEscape(ptEroFormatPacific(endRaw) || 'n/a') + '<br>' +
      '<b>Provider/UTC window:</b> ' + ptEroEscape(ptEroFormatUtc(startRaw) || 'n/a') + ' – ' + ptEroEscape(ptEroFormatUtc(endRaw) || 'n/a') +
      (issueRaw ? '<br><b>Issued/file date:</b> ' + ptEroEscape(ptEroFormatPacific(issueRaw) || String(issueRaw)) : '') +
      '</p>';

    if (sourceUrl) {
      html += '<p style="margin:6px 0;"><a href="' + ptEroEscape(sourceUrl) + '" target="_blank" rel="noopener">Open WPC ERO product page</a></p>';
    }

    html += '<div style="font-size:11px;color:#666;margin-top:6px;">Current-view BRIM snapshot from the public WPC ERO MapServer. Pan/zoom, then use rfrsh to requery.</div>';
    html += ptEroRawAttributesTable(attrs);
    html += '</div>';
    return html;
  }

  var WpcEroCurrentViewLayer = L.Layer.extend({
    initialize: function(options) {
      this.options = options || {};
      this._map = null;
      this._layer = null;
      this._isRemoved = true;
      this._seq = 0;
    },
    onAdd: function(mapObj) {
      this._map = mapObj;
      this._isRemoved = false;
      activeLegendDefs[this.options.name] = {
        note: this.options.note || '',
        legendType: this.options.legendType || 'ero',
        sourceUrl: this.options.sourceUrl || '',
        legendUrl: this.options.legendUrl || '',
        infoUrl: this.options.infoUrl || '',
        infoLabel: this.options.infoLabel || ''
      };
      redrawLegend();
      this.refreshCurrentView();
      if (typeof this.options.checkFreshness === 'function') {
        this.options.checkFreshness();
      }
    },
    _queryUrl: function() {
      var b = this._map.getBounds();
      var geometry = JSON.stringify({
        xmin: b.getWest(),
        ymin: b.getSouth(),
        xmax: b.getEast(),
        ymax: b.getNorth(),
        spatialReference: {wkid: 4326}
      });
      var fields = 'product,valid_time,outlook,issue_time,start_time,end_time,idp_source,idp_filedate,idp_ingestdate,dn,snippet';
      return safeUrlBase(this.options.url) + '/' + this.options.layerId + '/query?' +
        'f=geojson&where=1%3D1&returnGeometry=true&geometryType=esriGeometryEnvelope' +
        '&spatialRel=esriSpatialRelIntersects&inSR=4326&outSR=4326' +
        '&outFields=' + encodeURIComponent(fields) +
        '&resultRecordCount=2000&geometry=' + encodeURIComponent(geometry) +
        '&_=' + Date.now();
    },
    refreshCurrentView: function() {
      if (this._isRemoved || !this._map) return;
      var self = this;
      var seq = ++this._seq;
      var name = this.options.name || 'WPC ERO';
      setOpsLayerLoading(name, true);
      recordStatus(name, 'Querying WPC ERO polygons for current map view…', 'pt-ops-warn');

      fetch(this._queryUrl(), {cache: 'no-store'})
        .then(function(resp) {
          if (!resp.ok) throw new Error('HTTP ' + resp.status);
          return resp.json();
        })
        .then(function(fc) {
          if (self._isRemoved || !self._map || seq !== self._seq) return;
          fc = fc || {type: 'FeatureCollection', features: []};
          fc.features = Array.isArray(fc.features) ? fc.features : [];

          var newLayer = L.geoJSON(fc, {
            pane: 'pane_ops',
            style: function(feature) {
              return ptEroStyle(feature && feature.properties ? feature.properties : {});
            },
            onEachFeature: function(feature, layer) {
              var attrs = feature && feature.properties ? feature.properties : {};
              layer.bindTooltip(ptEroHoverHtml(name, attrs), {direction: 'auto', opacity: 0.95, sticky: true});
              layer.bindPopup(ptEroPopupHtml(name, attrs, self.options.sourceUrl || ''));
              layer.on('mouseover', function() {
                if (self._map && self._map.getContainer()) self._map.getContainer().style.cursor = 'crosshair';
              });
              layer.on('mouseout', function() {
                if (self._map && self._map.getContainer()) self._map.getContainer().style.cursor = '';
              });
            }
          });

          if (self._layer && self._map.hasLayer(self._layer)) {
            try { self._map.removeLayer(self._layer); } catch(e) {}
          }
          self._layer = newLayer.addTo(self._map);
          setOpsLayerLoading(name, false);
          recordStatus(name, 'Loaded WPC ERO current-view snapshot: ' + fc.features.length.toLocaleString() + ' polygon(s). Pan/zoom, then use rfrsh to requery.', fc.features.length === 0 ? 'pt-ops-warn' : 'pt-ops-ok');
        })
        .catch(function(err) {
          console.error(err);
          if (self._isRemoved || !self._map || seq !== self._seq) return;
          setOpsLayerLoading(name, false);
          recordStatus(name, 'WPC ERO query failed. Try rfrsh after panning/zooming, or use the source product page.', 'pt-ops-warn');
        });
    },
    onRemove: function(mapObj) {
      this._isRemoved = true;
      this._seq += 1;
      var m = mapObj || this._map;
      if (m && this._layer) {
        try { m.removeLayer(this._layer); } catch(e) {}
      }
      this._layer = null;
      this._map = null;
      delete activeLegendDefs[this.options.name];
      setOpsLayerLoading(this.options.name, false);
      redrawLegend();
      recordStatus(this.options.name, 'Layer turned off.', 'pt-ops-muted');
    },
    forceRemove: function(mapObj) {
      this.onRemove(mapObj || this._map);
    }
  });

  function makeRadarLayer(opts) {
    var lyr = L.tileLayer.wms('https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?', {
      layers: 'nexrad-n0q-900913',
      format: 'image/png',
      transparent: true,
      opacity: opts.opacity || 0.70,
      attribution: 'Weather radar © Iowa Environmental Mesonet',
      pane: 'pane_ops',
      sourceUrl: opts.sourceUrl || 'https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi',
      legendUrl: opts.legendUrl || '',
      infoUrl: opts.infoUrl || '',
      infoLabel: opts.infoLabel || '',
      legendNote: opts.legendNote || ''
    });
    lyr.on('add', function() {
      activeLegendDefs[opts.name] = {
        note: opts.note || '',
        legendType: opts.legendType || null,
        sourceUrl: opts.sourceUrl || 'https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi',
        legendUrl: opts.legendUrl || '',
        infoUrl: opts.infoUrl || '',
        infoLabel: opts.infoLabel || '',
        legendNote: opts.legendNote || ''
      };
      setOpsLayerLoading(opts.name, true);
      redrawLegend();
      recordStatus(opts.name, 'Radar tile overlay requested. Time is controlled by the live IEM NEXRAD WMS service.', 'pt-ops-warn');
    });
    lyr.on('loading', function() {
      setOpsLayerLoading(opts.name, true);
    });
    lyr.on('load', function() {
      setOpsLayerLoading(opts.name, false);
      recordStatus(opts.name, 'Radar tiles loaded.', 'pt-ops-ok');
    });
    lyr.on('tileerror', function() {
      setOpsLayerLoading(opts.name, false);
      recordStatus(opts.name, 'One or more radar tiles failed to load. Try waiting, panning slightly, or toggling the layer.', 'pt-ops-warn');
    });
    lyr.on('remove', function() {
      delete activeLegendDefs[opts.name];
      setOpsLayerLoading(opts.name, false);
      redrawLegend();
      recordStatus(opts.name, 'Layer turned off.', 'pt-ops-muted');
    });
    return lyr;
  }

  function makeGibsWmtsLayer(opts) {
    // NASA GIBS WMTS tends to behave more reliably in Leaflet than the WMS
    // endpoint for daily true-color imagery.  The "default/default" path lets
    // GIBS select the current/default date for the chosen layer.  The native
    // Web-Mercator tile matrix is Level9, so higher Leaflet zooms are scaled.
    var layerId = opts.layerId || opts.layers || '';
    var styleName = opts.styleName || 'default';
    var timeName = opts.timeName || 'default';
    var matrixSet = opts.matrixSet || 'GoogleMapsCompatible_Level9';
    var ext = opts.ext || 'jpg';
    var url = 'https://gibs.earthdata.nasa.gov/wmts/epsg3857/best/' +
      encodeURIComponent(layerId) + '/' +
      encodeURIComponent(styleName) + '/' +
      encodeURIComponent(timeName) + '/' +
      encodeURIComponent(matrixSet) + '/{z}/{y}/{x}.' + ext;

    var lyr = L.tileLayer(url, {
      opacity: opts.opacity || 0.82,
      attribution: 'NASA GIBS / EOSDIS',
      pane: 'pane_ops',
      maxNativeZoom: opts.maxNativeZoom || 9,
      maxZoom: 20,
      noWrap: true
    });

    lyr.on('add', function() {
      activeLegendDefs[opts.name] = {
        note: opts.note || '',
        legendType: opts.legendType || null,
        sourceUrl: opts.sourceUrl || 'https://gibs.earthdata.nasa.gov/wmts/epsg3857/best/' + encodeURIComponent(layerId) + '/',
        legendUrl: opts.legendUrl || '',
        infoUrl: opts.infoUrl || 'https://nasa-gibs.github.io/gibs-api-docs/',
        legendNote: opts.legendNote || ''
      };
      setOpsLayerLoading(opts.name, true);
      redrawLegend();
      recordStatus(opts.name, 'NASA GIBS WMTS tiles requested. The default/latest available date is used by the source service.', 'pt-ops-warn');
    });

    lyr.on('loading', function() {
      setOpsLayerLoading(opts.name, true);
    });

    lyr.on('load', function() {
      setOpsLayerLoading(opts.name, false);
      recordStatus(opts.name, 'NASA GIBS tiles loaded.', 'pt-ops-ok');
    });

    lyr.on('tileerror', function() {
      setOpsLayerLoading(opts.name, false);
      recordStatus(opts.name, 'One or more NASA GIBS WMTS tiles failed to load. Daily true-color imagery can lag, have coverage gaps, or be missing near the latest date.', 'pt-ops-warn');
    });

    lyr.on('remove', function() {
      delete activeLegendDefs[opts.name];
      setOpsLayerLoading(opts.name, false);
      redrawLegend();
      recordStatus(opts.name, 'Layer turned off.', 'pt-ops-muted');
    });

    return lyr;
  }
  

)---"
}
