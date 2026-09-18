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
  // Exact Ops forecast selection group; family-specific requests remain separate.
  var ptOpsForecastOwners = [];
  var ptOpsForecastProductIds = [
    'ops_wpc_qpf_day_1','ops_wpc_qpf_day_2','ops_wpc_qpf_day_3','ops_wpc_qpf_3day','ops_wpc_qpf_7day',
    'ops_wpc_ero_day_1','ops_wpc_ero_day_2','ops_wpc_ero_day_3',
    'ops_cpc_6_10_temperature','ops_cpc_6_10_precipitation','ops_cpc_8_14_temperature','ops_cpc_8_14_precipitation'
  ];
  function ptOpsRegisterForecastOwner(owner) {
    if (ptOpsForecastProductIds.indexOf(owner.options.forecastProductId) >= 0 && ptOpsForecastOwners.indexOf(owner) < 0) ptOpsForecastOwners.push(owner);
  }
  function ptOpsSelectForecastOwner(owner, mapObj) {
    if (ptOpsForecastProductIds.indexOf(owner.options.forecastProductId) < 0) return;
    ptOpsForecastOwners.forEach(function(other) {
      if (other === owner || other._isRemoved) return;
      // The ordinary Ops path clears checkbox/count and invokes the real owner teardown.
      ptOpsDeactivateLayerByName(other.options.name);
      // Programmatic wrapper activation may not have a checked Ops row.
      if (mapObj.hasLayer(other)) mapObj.removeLayer(other);
    });
  }
  function ptIsWpcQpfOwner(layer) {
    return ['ops_wpc_qpf_day_1','ops_wpc_qpf_day_2','ops_wpc_qpf_day_3','ops_wpc_qpf_3day','ops_wpc_qpf_7day'].indexOf(layer.options.forecastProductId) >= 0;
  }

  // QPE has its own exact selection group; forecast and satellite owners stay independent.
  var ptOpsQpeOwners = [];
  var ptOpsQpeProductIds = [
    'ops_qpe_mrms_1hr','ops_qpe_mrms_1day','ops_qpe_mrms_3day',
    'ops_qpe_rfc_1day','ops_qpe_rfc_3day','ops_qpe_rfc_7day'
  ];
  function ptIsQpeOwner(owner) {
    return ptOpsQpeProductIds.indexOf(owner.options.qpeProductId) >= 0;
  }
  function ptOpsSelectQpeOwner(owner, mapObj) {
    if (!ptIsQpeOwner(owner)) return;
    ptOpsQpeOwners.forEach(function(other) {
      if (other === owner || other._isRemoved) return;
      ptOpsDeactivateLayerByName(other.options.name);
      if (mapObj.hasLayer(other)) mapObj.removeLayer(other);
    });
    // Direct activation of a registered wrapper also belongs to Ops OFF/Clear.
    var name = owner.options.name;
    if (opsDefByName[name] && opsDefByName[name].layer === owner) {
      activeLayers[name] = owner;
      if (checkboxByName[name]) checkboxByName[name].checked = true;
      updateOpsHeaderCount();
    }
  }

  // Radar coordinates its two existing controllers without joining other families.
  var ptOpsRadarOwners = [];
  var ptOpsRadarProductIds = ['ops_radar_iem_nexrad','ops_radar_noaa_mrms'];
  function ptIsRadarOwner(owner) {
    return ptOpsRadarProductIds.indexOf(owner.options.radarProductId) >= 0;
  }
  function ptOpsSelectRadarOwner(owner, mapObj) {
    if (!ptIsRadarOwner(owner)) return;
    ptOpsRadarOwners.forEach(function(other) {
      if (other === owner || other._isRemoved) return;
      ptOpsDeactivateLayerByName(other.options.name);
      if (mapObj.hasLayer(other)) mapObj.removeLayer(other);
    });
    var name = owner.options.name;
    if (opsDefByName[name] && opsDefByName[name].layer === owner) {
      activeLayers[name] = owner;
      if (checkboxByName[name]) checkboxByName[name].checked = true;
      updateOpsHeaderCount();
    }
  }

  var ArcGISExportLayer = L.Layer.extend({
    initialize: function(options) {
      this.options = options || {};
      this._overlay = null;
      this._timer = null;
      this._pendingOverlay = null;
      this._isRemoved = true;
      this._forecastSeq = 0;
      this._radarSeq = 0;
      if (ptIsRadarOwner(this)) ptOpsRadarOwners.push(this);
      ptOpsRegisterForecastOwner(this);
      if (ptIsQpeOwner(this)) ptOpsQpeOwners.push(this);

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
          if (ptIsWpcQpfOwner(this)) { this._invalidateForecast(); ptWpcQpfMetadata(this); }
          this._update();
        };
      }
    },
    onAdd: function(mapObj) {
      this._map = mapObj;
      this._isRemoved = false;
      ptOpsSelectForecastOwner(this, mapObj);
      ptOpsSelectQpeOwner(this, mapObj);
      if (ptIsRadarOwner(this)) { this._radarSeq += 1; ptOpsSelectRadarOwner(this, mapObj); }
      if (ptIsWpcQpfOwner(this)) {
        this._invalidateForecast();
        activateWpcQpfHover(this.options.name, this.options.layers[0]);
        mapObj.on('movestart zoomstart', this._invalidateForecast, this);
      }
      activeLegendDefs[this.options.name] = {
        note: this.options.note || '',
        legendType: this.options.legendType || null,
        rfcQpeProductId: this.options.rfcQpeProductId || null,
        forecastProductId: this.options.forecastProductId || null,
        sourceUrl: this.options.sourceUrl || this.options.url || '',
        legendUrl: this.options.legendUrl || '',
        infoUrl: this.options.infoUrl || '',
        infoLabel: this.options.infoLabel || '',
        legendNote: this.options.legendNote || ''
      };
      redrawLegend();
      this._scheduleUpdate();
      if (ptIsWpcQpfOwner(this)) ptWpcQpfMetadata(this);
      mapObj.on('moveend zoomend resize', this._scheduleUpdate, this);
      recordStatus(this.options.name, this.options.satelliteImagery ?
        'Satellite image requested. Displayed frame time: Unverified. BRIM check time is not image time.' :
        'Requested live image.', 'pt-ops-warn');
      // QPF uses its selected-layer, generation-bound metadata request above.
      // Do not also launch an unowned whole-service callback for these products.
      if (!ptIsWpcQpfOwner(this) && typeof this.options.checkFreshness === 'function') {
        if (ptIsRadarOwner(this)) {
          var self = this, radarSeq = this._radarSeq;
          this.options.checkFreshness(function() {
            return !self._isRemoved && self._map === mapObj && self._radarSeq === radarSeq;
          });
        } else this.options.checkFreshness();
      }
    },
    _invalidateForecast: function() {
      if (!ptIsWpcQpfOwner(this)) return;
      this._forecastSeq += 1;
      invalidateWpcQpfHover();
      if (this._map && this._pendingOverlay) this._map.removeLayer(this._pendingOverlay);
      this._pendingOverlay = null;
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
      if (ptIsWpcQpfOwner(this) || ptIsQpeOwner(this) || ptIsRadarOwner(this)) { this.onRemove(mapObj || this._map); return; }
      this._isRemoved = true;
      this._removeInternalOverlays(mapObj || this._map);
      this._map = null;
    },
    onRemove: function(mapObj) {
      this._isRemoved = true;
      if (ptIsRadarOwner(this)) {
        this._radarSeq += 1;
        delete statusRows['NOAA radar metadata'];
        redrawStatus();
      }
      if (ptIsWpcQpfOwner(this)) {
        this._invalidateForecast(); deactivateWpcQpfHover(this.options.name);
        if (mapObj) mapObj.off('movestart zoomstart', this._invalidateForecast, this);
      }
      if (!mapObj) return;
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
      this._invalidateForecast();
      // Satellite, QPE and Radar requests lose ownership as soon as the view changes.
      if ((this.options.satelliteImagery || ptIsQpeOwner(this) || ptIsRadarOwner(this)) && this._pendingOverlay) {
        this._map.removeLayer(this._pendingOverlay);
        this._pendingOverlay = null;
      }
      if (this._timer) window.clearTimeout(this._timer);
      var radarSeq = this._radarSeq;
      this._timer = window.setTimeout(function() {
        if (ptIsRadarOwner(self) && self._radarSeq !== radarSeq) return;
        self._update();
      }, 650);
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
      var requestMap = this._map;
      if ((ptIsWpcQpfOwner(this) || ptIsQpeOwner(this) || ptIsRadarOwner(this)) && this._pendingOverlay) {
        requestMap.removeLayer(this._pendingOverlay); this._pendingOverlay = null;
      }
      if (this.options.satelliteImagery) {
        if (this._pendingOverlay) requestMap.removeLayer(this._pendingOverlay);
        this._pendingOverlay = null;
        recordStatus(this.options.name, 'Satellite image requested. Displayed frame time: Unverified. BRIM check time is not image time.', 'pt-ops-warn');
      }
      setOpsLayerLoading(this.options.name, true);
      var b = this._map.getBounds();
      var url = this._buildUrl();
      var img = L.imageOverlay(url, b, {
        opacity: this.options.satelliteImagery ? 0 : (this.options.opacity || 0.65),
        interactive: false,
        crossOrigin: false,
        pane: 'pane_ops'
      });
      this._pendingOverlay = img;
      img.on('load', function() {
        if ((self.options.satelliteImagery || ptIsWpcQpfOwner(self) || ptIsQpeOwner(self) || ptIsRadarOwner(self)) && self._pendingOverlay !== img) {
          requestMap.removeLayer(img);
          return;
        }
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
        if (self.options.satelliteImagery) img.setOpacity(self.options.opacity || 0.65);
        setOpsLayerLoading(self.options.name, false);
        recordStatus(self.options.name, self.options.satelliteImagery ?
          'Image received. Displayed frame time: Unverified. BRIM check time is not image time.' :
          'Loaded live image successfully.', self.options.satelliteImagery ? 'pt-ops-warn' : 'pt-ops-ok');
      });
      img.on('error', function() {
        if ((self.options.satelliteImagery || ptIsWpcQpfOwner(self) || ptIsQpeOwner(self) || ptIsRadarOwner(self)) && self._pendingOverlay !== img) {
          requestMap.removeLayer(img);
          return;
        }
        try { if (self._map) self._map.removeLayer(img); } catch(e) {}
        if (self._pendingOverlay === img) self._pendingOverlay = null;
        if (self._isRemoved || !self._map || !self._map.hasLayer(self)) return;
        setOpsLayerLoading(self.options.name, false);
        recordStatus(self.options.name, self.options.satelliteImagery ?
          'Satellite image request failed. ' + (self._overlay ? 'Last received image retained; its age is Unverified. ' : 'No image loaded. ') +
          'Displayed frame time: Unverified. BRIM check time is not image time.' :
          'Image request failed. Try zooming in, waiting, or toggling fewer Ops layers.', 'pt-ops-bad');
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
      ptOpsRegisterForecastOwner(this);
    },
    onAdd: function(mapObj) {
      this._map = mapObj;
      this._isRemoved = false;
      ptOpsSelectForecastOwner(this, mapObj);
      activeLegendDefs[this.options.name] = {
        note: this.options.note || '',
        legendType: this.options.legendType || 'ero',
        forecastProductId: this.options.forecastProductId || null,
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
          var legend = activeLegendDefs[name];
          if (legend) {
            legend.forecastMetadata = ptForecastMetadata('ero', fc.features, !!fc.exceededTransferLimit);
            redrawLegend();
          }
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
    // Stable Ops owner; each activation gets a fresh native WMS tile controller.
    // An old tile callback cannot acquire a later activation of the same row.
    var wmsOptions = {
      layers: 'nexrad-n0q-900913', format: 'image/png', transparent: true,
      opacity: opts.opacity || 0.70, attribution: 'Weather radar © Iowa Environmental Mesonet',
      pane: 'pane_ops', sourceUrl: opts.sourceUrl || 'https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi',
      legendUrl: opts.legendUrl || '', infoUrl: opts.infoUrl || '',
      infoLabel: opts.infoLabel || '', legendNote: opts.legendNote || ''
    };
    var RadarLayer = L.Layer.extend({
      initialize: function(options) {
        this.options = Object.assign({}, options, wmsOptions);
        this._isRemoved = true;
        this._tiles = null;
        this._radarSeq = 0;
        if (ptIsRadarOwner(this)) ptOpsRadarOwners.push(this);
      },
      onAdd: function(mapObj) {
        this._map = mapObj;
        this._isRemoved = false;
        var self = this, radarSeq = ++this._radarSeq;
        ptOpsSelectRadarOwner(this, mapObj);
        // Project the same exposed WMS contract; owner-only metadata is not a
        // provider parameter. Leaflet receives its own per-activation options.
        var tileOptions = {};
        Object.keys(wmsOptions).forEach(function(key) {
          tileOptions[key] = self.options[key];
        });
        var tile = L.tileLayer.wms('https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?', tileOptions);
        this._tiles = tile;
        function current() {
          return !self._isRemoved && self._map === mapObj && self._radarSeq === radarSeq &&
            self._tiles === tile && mapObj.hasLayer(self);
        }
        this._tileHandlers = {
          loading: function() { if (current()) setOpsLayerLoading(opts.name, true); },
          load: function() {
            if (!current()) return;
            setOpsLayerLoading(opts.name, false);
            recordStatus(opts.name, 'Radar tiles loaded.', 'pt-ops-ok');
          },
          tileerror: function() {
            if (!current()) return;
            setOpsLayerLoading(opts.name, false);
            recordStatus(opts.name, 'One or more radar tiles failed to load. Try waiting, panning slightly, or toggling the layer.', 'pt-ops-warn');
          }
        };
        Object.keys(this._tileHandlers).forEach(function(type) { tile.on(type,self._tileHandlers[type]); });
        activeLegendDefs[opts.name] = {
          note: opts.note || '', legendType: opts.legendType || null,
          sourceUrl: opts.sourceUrl || 'https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi',
          legendUrl: opts.legendUrl || '', infoUrl: opts.infoUrl || '',
          infoLabel: opts.infoLabel || '', legendNote: opts.legendNote || ''
        };
        setOpsLayerLoading(opts.name, true);
        redrawLegend();
        recordStatus(opts.name, 'Radar tile overlay requested. Time is controlled by the live IEM NEXRAD WMS service.', 'pt-ops-warn');
        tile.addTo(mapObj);
      },
      onRemove: function(mapObj) {
        this._isRemoved = true;
        this._radarSeq += 1;
        var tile = this._tiles, handlers = this._tileHandlers;
        this._tiles = null;
        this._tileHandlers = null;
        if (tile) {
          Object.keys(handlers).forEach(function(type) { tile.off(type,handlers[type]); });
          (mapObj || this._map).removeLayer(tile);
        }
        this._map = null;
        delete activeLegendDefs[opts.name];
        setOpsLayerLoading(opts.name, false);
        redrawLegend();
        recordStatus(opts.name, 'Layer turned off.', 'pt-ops-muted');
      },
      forceRemove: function(mapObj) { this.onRemove(mapObj || this._map); }
    });
    return new RadarLayer(opts);
  }


)---"
}
