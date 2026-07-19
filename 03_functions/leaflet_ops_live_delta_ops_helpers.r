# ==== leaflet_ops_live_delta_ops_helpers.r ==================================
##
## PURPOSE:
##   Browser-side Ops Live helper for BRIM Delta Ops Daily Summary snapshot.
##   Uses static GeoJSON/JSON files from brim-live-data-feeds on GitHub Pages.
## ============================================================================

pt_ops_live_delta_ops_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Delta Ops Daily Summary snapshot layer
  // --------------------------------------------------------------------------

  function ptDeltaOpsEnsureStyle() {
    if (document.getElementById('pt-delta-ops-style')) return;
    var style = document.createElement('style');
    style.id = 'pt-delta-ops-style';
    style.innerHTML = `
      .pt-delta-ops-label,
      .pt-delta-ops-status-label,
      .pt-delta-ops-x2-label {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
        padding: 0 !important;
        pointer-events: none !important;
      }
      .pt-delta-ops-label-text {
        font-family: Arial, sans-serif;
        font-size: 11px;
        font-weight: 700;
        color: #111;
        white-space: nowrap;
        text-shadow:
          -1px -1px 0 rgba(255,255,255,0.96),
           1px -1px 0 rgba(255,255,255,0.96),
          -1px  1px 0 rgba(255,255,255,0.96),
           1px  1px 0 rgba(255,255,255,0.96);
      }
      .pt-delta-ops-inflow { color: #238b45; font-style: italic; font-weight: 700; }
      .pt-delta-ops-export { color: #7a0019; font-weight: 700; }
      .pt-delta-ops-outflow { color: #b2182b; font-weight: 700; }
      .pt-delta-ops-omr { color: #5e3c99; font-weight: 700; }
      .pt-delta-ops-x2 { color: #444444; font-weight: 700; }
      .pt-delta-ops-gates { color: #222222; font-weight: 700; line-height: 1.05; }
      .pt-delta-ops-sanluis { color: #6b4c1f; font-weight: 700; }
      .pt-delta-ops-status { color: #222222; font-weight: 800; font-size: 12px; line-height: 1.12; }
      .pt-delta-ops-date-header { color: #222222; font-weight: 800; font-size: 11px; line-height: 1.15; }
      .pt-delta-ops-x2-ref-text {
        color: #666;
        font-family: Arial, sans-serif;
        font-size: 8px;
        font-weight: 600;
        text-shadow:
          -1px -1px 0 rgba(255,255,255,0.82),
           1px -1px 0 rgba(255,255,255,0.82),
          -1px  1px 0 rgba(255,255,255,0.82),
           1px  1px 0 rgba(255,255,255,0.82);
      }
      .pt-delta-empty-icon { display: none !important; }
      .pt-ops-row-mini-toggle {
        display: inline-flex;
        align-items: center;
        gap: 1px;
        margin-left: 5px;
        font-size: 10px;
        line-height: 1;
        color: #555;
        white-space: nowrap;
      }
      .pt-ops-row-mini-toggle input {
        width: 10px;
        height: 10px;
        margin: 0 1px 0 0;
        vertical-align: -1px;
      }
      .pt-ops-row-mini-action {
        margin-left: 4px;
        font-size: 10px;
        color: #555;
        text-decoration: none;
      }
      .pt-ops-row-mini-action:hover { text-decoration: underline; color: #222; }
    `;
    document.head.appendChild(style);
  }

  function ptDeltaOpsEscape(x) {
    return String(x == null ? '' : x)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }

  function ptDeltaOpsNum(x) {
    var s = String(x == null ? '' : x);
    var m = s.match(/-?[0-9]+(?:,[0-9]{3})*(?:\.[0-9]+)?/);
    if (!m) return NaN;
    return Number(m[0].replace(/,/g, ''));
  }

  function ptDeltaOpsFormatCfs(x) {
    var n = Number(x);
    if (!isFinite(n)) return 'NA cfs';
    return Math.round(n).toLocaleString('en-US') + ' cfs';
  }

  function ptDeltaOpsFormatKafDay(x) {
    var n = Number(x);
    if (!isFinite(n)) return 'NA kaf/day';
    var digits = Math.abs(n) < 10 ? 1 : 0;
    return n.toLocaleString('en-US', {
      minimumFractionDigits: digits,
      maximumFractionDigits: digits
    }) + ' kaf/day';
  }


  function ptDeltaOpsEnsurePercentText(x) {
    var s = String(x == null ? '' : x).trim();
    if (!s || s.indexOf('%') >= 0 || /^NA$/i.test(s)) return s;
    var suffix = '';
    var suffixMatch = s.match(/\s*(\([^)]*\))\s*$/);
    if (suffixMatch) {
      suffix = ' ' + suffixMatch[1];
      s = s.replace(/\s*\([^)]*\)\s*$/, '').trim();
    }
    var numMatch = s.match(/-?[0-9]+(?:,[0-9]{3})*(?:\.[0-9]+)?/);
    if (!numMatch) return String(x == null ? '' : x);
    return numMatch[0] + '%' + suffix;
  }

  function ptDeltaOpsShortDate(dateStr) {
    var d = new Date(String(dateStr || '') + 'T00:00:00');
    if (isNaN(d.getTime())) return String(dateStr || '');
    return (d.getMonth() + 1) + '/' + d.getDate();
  }

  function ptDeltaOpsHeaderDate(dateStr) {
    var s = String(dateStr || '');
    var m = s.match(/^(\d{4})-(\d{1,2})-(\d{1,2})/);
    if (m) {
      return Number(m[2]) + '/' + Number(m[3]) + '/' + String(m[1]).slice(-2);
    }
    var d = new Date(s + 'T00:00:00');
    if (isNaN(d.getTime())) return s || 'latest';
    return (d.getMonth() + 1) + '/' + d.getDate() + '/' + String(d.getFullYear()).slice(-2);
  }

  function ptDeltaOpsDateMinusDays(dateStr, days) {
    var s = String(dateStr || '');
    var m = s.match(/^(\d{4})-(\d{1,2})-(\d{1,2})/);
    if (!m) return '';
    var d = new Date(Number(m[1]), Number(m[2]) - 1, Number(m[3]));
    if (isNaN(d.getTime())) return '';
    d.setDate(d.getDate() - Number(days || 0));
    var mm = String(d.getMonth() + 1).padStart(2, '0');
    var dd = String(d.getDate()).padStart(2, '0');
    return d.getFullYear() + '-' + mm + '-' + dd;
  }

  function ptDeltaOpsX2PositionDate(feature, summary) {
    var p = ptDeltaOpsProps(feature);
    if (p.x2_position_date) return String(p.x2_position_date);
    if (summary && summary.x2_position_date) return String(summary.x2_position_date);
    var reportDate = (summary && summary.report_date) || p.report_date || '';
    return ptDeltaOpsDateMinusDays(reportDate, 1);
  }

  function ptDeltaOpsProps(feature) {
    return feature && feature.properties ? feature.properties : {};
  }

  function ptDeltaOpsLatLng(feature) {
    var c = feature && feature.geometry && feature.geometry.coordinates ? feature.geometry.coordinates : null;
    if (!c || c.length < 2) return null;
    var lon = Number(c[0]);
    var lat = Number(c[1]);
    if (!isFinite(lon) || !isFinite(lat)) return null;
    return [lat, lon];
  }

  function ptDeltaOpsLabelClass(key) {
    if (['sacramento_freeport','san_joaquin_vernalis','total_delta_inflow'].indexOf(key) >= 0) return 'pt-delta-ops-inflow';
    if (['jones_cvp_exports','banks_swp_exports'].indexOf(key) >= 0) return 'pt-delta-ops-export';
    if (key === 'delta_outflow_index') return 'pt-delta-ops-outflow';
    if (key === 'omr_index') return 'pt-delta-ops-omr';
    if (key === 'x2_position_current') return 'pt-delta-ops-x2';
    if (key === 'cross_channel_gates') return 'pt-delta-ops-gates';
    if (key === 'san_luis_reservoir') return 'pt-delta-ops-sanluis';
    return 'pt-delta-ops-label-default';
  }

  function ptDeltaOpsColor(key) {
    if (['sacramento_freeport','san_joaquin_vernalis','total_delta_inflow'].indexOf(key) >= 0) return '#238b45';
    if (['jones_cvp_exports','banks_swp_exports'].indexOf(key) >= 0) return '#7a0019';
    if (key === 'delta_outflow_index') return '#b2182b';
    if (key === 'omr_index') return '#5e3c99';
    if (key === 'x2_position_current') return '#444444';
    if (key === 'cross_channel_gates') return '#222222';
    if (key === 'san_luis_reservoir') return '#6b4c1f';
    return '#222222';
  }

  function ptDeltaOpsScaleCfsCircle(key) {
    // Scale only the primary through-Delta water-volume cues.
    // Inputs: Sacramento, San Joaquin, East Side Streams estimate.
    // Outputs: Delta outflow, Jones/CVP exports, Banks/SWP exports.
    return [
      'sacramento_freeport',
      'san_joaquin_vernalis',
      'total_delta_inflow',
      'delta_outflow_index',
      'jones_cvp_exports',
      'banks_swp_exports'
    ].indexOf(key) >= 0;
  }

  function ptDeltaOpsFeatureCfs(feature) {
    var p = ptDeltaOpsProps(feature);
    var n = ptDeltaOpsNum(p.value_numeric);
    if (!isFinite(n)) n = ptDeltaOpsNum(p.value_raw);
    return Math.abs(n);
  }

  function ptDeltaOpsCircleRadius(key, feature, cfsMax) {
    if (!ptDeltaOpsScaleCfsCircle(key)) return 4;
    var n = ptDeltaOpsFeatureCfs(feature);
    if (!isFinite(n) || n <= 0 || !isFinite(cfsMax) || cfsMax <= 0) return 4;

    // Restrained, area-like scaling: radius follows sqrt(value/max).
    // Keeps the visual cue useful without giant circles.
    var minR = 3.2;
    var maxR = 9.2;
    return minR + Math.sqrt(n / cfsMax) * (maxR - minR);
  }

  function ptDeltaOpsLabelDirection(key) {
    if (['cross_channel_gates','x2_position_current','jones_cvp_exports','banks_swp_exports','san_joaquin_vernalis','san_luis_reservoir'].indexOf(key) >= 0) return 'right';
    if (['sacramento_freeport','delta_outflow_index'].indexOf(key) >= 0) return 'left';
    if (key === 'omr_index') return 'bottom';
    if (key === 'total_delta_inflow') return 'top';
    return 'right';
  }

  function ptDeltaOpsLabelOffset(key, radius) {
    var dir = ptDeltaOpsLabelDirection(key);
    var pad = (isFinite(radius) ? Math.max(8, radius + 5) : 8);
    if (dir === 'left') return L.point(-pad, 0);
    if (dir === 'right') return L.point(pad, 0);
    if (dir === 'top') return L.point(0, -pad);
    if (dir === 'bottom') return L.point(0, pad);
    return L.point(pad, 0);
  }

  function ptDeltaOpsPrettyLabel(feature, summary, featureByKey) {
    var p = ptDeltaOpsProps(feature);
    var key = String(p.feature_key || '');
    var raw = p.value_raw;
    var n = ptDeltaOpsNum(raw);
    var reportDate = summary && summary.report_date ? summary.report_date : p.report_date;

    if (key === 'cross_channel_gates') {
      return 'Delta X-Channel Gates<br>' + (isFinite(n) ? n : 'NA') + '% open';
    }
    if (key === 'jones_cvp_exports') return 'Jones/CVP: ' + ptDeltaOpsFormatCfs(n);
    if (key === 'banks_swp_exports') return 'Banks/SWP: ' + ptDeltaOpsFormatCfs(n);
    if (key === 'delta_outflow_index') return 'Outflow: ' + ptDeltaOpsFormatCfs(n);
    if (key === 'omr_index') return 'OMR: ' + ptDeltaOpsFormatCfs(n);
    if (key === 'sacramento_freeport') return 'Sac: ' + ptDeltaOpsFormatCfs(n);
    if (key === 'san_joaquin_vernalis') return 'SJ: ' + ptDeltaOpsFormatCfs(n);
    if (key === 'total_delta_inflow') return 'East side streams: ' + ptDeltaOpsFormatCfs(n);
    if (key === 'san_luis_reservoir') return String(p.label_text || raw || '').replace(/^San Luis:\s*/, 'San Luis: ');
    if (key === 'x2_position_current') return 'X2 ' + ptDeltaOpsShortDate(ptDeltaOpsX2PositionDate(feature, summary)) + ': ' + (isFinite(n) ? Math.round(n) : 'NA') + ' km';
    return String(p.label_text || raw || '');
  }

  function ptDeltaOpsExportKafDay(featureByKey, summary) {
    if (summary && isFinite(Number(summary.export_total_kaf_day))) {
      return Number(summary.export_total_kaf_day);
    }

    var jones = featureByKey.jones_cvp_exports ? ptDeltaOpsNum(ptDeltaOpsProps(featureByKey.jones_cvp_exports).value_numeric) : NaN;
    var banks = featureByKey.banks_swp_exports ? ptDeltaOpsNum(ptDeltaOpsProps(featureByKey.banks_swp_exports).value_numeric) : NaN;

    if (isFinite(jones) && isFinite(banks)) {
      return (jones + banks) * 1.98347 / 1000;
    }

    return NaN;
  }

  function ptDeltaOpsOutflowKafDay(featureByKey, summary) {
    if (summary && isFinite(Number(summary.outflow_kaf_day))) {
      return Number(summary.outflow_kaf_day);
    }

    var outflow = featureByKey.delta_outflow_index ? ptDeltaOpsNum(ptDeltaOpsProps(featureByKey.delta_outflow_index).value_numeric) : NaN;
    if (isFinite(outflow)) {
      return outflow * 1.98347 / 1000;
    }

    return NaN;
  }

  function ptDeltaOpsStatusLabel(featureByKey, summary) {
    var statusFeature = featureByKey.delta_conditions || null;
    var statusProps = statusFeature ? ptDeltaOpsProps(statusFeature) : {};
    var status = statusFeature ? String(statusProps.label_text || '').replace(/^Delta:\s*/, '') : 'NA';
    var control = featureByKey.controlling_factors ? String(ptDeltaOpsProps(featureByKey.controlling_factors).label_text || '').replace(/^Export control:\s*/, '').replace(/^Control:\s*/, '') : 'NA';
    var diverted = featureByKey.percent_inflow_diverted ? String(ptDeltaOpsProps(featureByKey.percent_inflow_diverted).label_text || '').replace(/^Diverted:\s*/, '') : 'NA';
    var reportDate = (summary && summary.report_date) || statusProps.report_date || '';
    var header = '<span class="pt-delta-ops-date-header">Report date: ' + ptDeltaOpsEscape(ptDeltaOpsHeaderDate(reportDate)) + '</span>';
    var outflowKafDay = ptDeltaOpsOutflowKafDay(featureByKey, summary);
    var outflowLine = isFinite(outflowKafDay) ? '<br>Outflow: ~' + ptDeltaOpsEscape(ptDeltaOpsFormatKafDay(outflowKafDay)) : '';
    var exportKafDay = ptDeltaOpsExportKafDay(featureByKey, summary);
    var exportLine = isFinite(exportKafDay) ? '<br>CVP/SWP exports: ~' + ptDeltaOpsEscape(ptDeltaOpsFormatKafDay(exportKafDay)) : '';
    diverted = ptDeltaOpsEnsurePercentText(diverted);
    return header + '<br>Delta: ' + ptDeltaOpsEscape(status) + '<br>Export control: ' + ptDeltaOpsEscape(control) + outflowLine + exportLine + '<br>Diverted: ' + ptDeltaOpsEscape(diverted);
  }

  function ptDeltaOpsPopup(feature, summary) {
    var p = ptDeltaOpsProps(feature);
    return '<b>' + ptDeltaOpsEscape(p.metric_name || p.display_name || p.feature_key || 'Delta Ops') + '</b><br>' +
      ptDeltaOpsEscape(String(p.label_text || p.value_raw || '')).replace(/\|/g, '<br>') +
      '<br><br>Report date: ' + ptDeltaOpsEscape(p.report_date || (summary && summary.report_date) || '') +
      '<br><em>' + ptDeltaOpsEscape(p.preliminary_notice || (summary && summary.preliminary_notice) || 'PRELIMINARY DATA; SUBJECT TO REVISION WITHOUT NOTICE') + '</em>' +
      '<br><br><a href="' + ptDeltaOpsEscape(p.source_url || (summary && summary.source_url) || '') + '" target="_blank">DWR source PDF</a>';
  }

  function ptDeltaOpsFetchJson(url) {
    return fetch(url, {cache: 'no-store'}).then(function(resp) {
      if (!resp.ok) throw new Error('HTTP ' + resp.status + ' for ' + url);
      return resp.json();
    });
  }

  var ptDeltaOpsActiveLayer = null;
  window.ptDeltaOpsLabelsVisible = true;

  window.ptDeltaOpsSetLabelsVisible = function(show) {
    window.ptDeltaOpsLabelsVisible = !!show;
    if (ptDeltaOpsActiveLayer && typeof ptDeltaOpsActiveLayer.setLabelsVisible === 'function') {
      ptDeltaOpsActiveLayer.setLabelsVisible(!!show);
    }
  };

  window.ptDeltaOpsZoomToDefault = function() {
    if (ptDeltaOpsActiveLayer && typeof ptDeltaOpsActiveLayer.zoomToDefault === 'function') {
      ptDeltaOpsActiveLayer.zoomToDefault();
      return true;
    }
    if (map && typeof map.setView === 'function') {
      map.setView([38.11002, -121.47775], 10);
      return true;
    }
    return false;
  };

  var DeltaOpsDailySummaryLayer = L.Layer.extend({
    initialize: function(options) {
      this.options = options || {};
      this._map = null;
      this._points = L.layerGroup();
      this._labels = L.layerGroup();
      this._x2Refs = L.layerGroup();
      this._x2Labels = L.layerGroup();
      this._statusLabel = L.layerGroup();
      this._isRemoved = true;
    },

    onAdd: function(mapObj) {
      this._map = mapObj;
      this._isRemoved = false;
      ptDeltaOpsActiveLayer = this;
      ptDeltaOpsEnsureStyle();

      this._x2Refs.addTo(mapObj);
      this._points.addTo(mapObj);
      this._labels.addTo(mapObj);
      this._x2Labels.addTo(mapObj);
      this._statusLabel.addTo(mapObj);

      activeLegendDefs[this.options.name] = {
        note: this.options.note || '',
        sourceUrl: this.options.sourceUrl || this.options.url || '',
        infoUrl: this.options.summaryUrl || '',
        infoLabel: this.options.summaryUrl ? 'summary' : ''
      };
      redrawLegend();
      setOpsLayerLoading(this.options.name, true);
      recordStatus(this.options.name, 'Fetching DWR Delta Ops daily summary…', 'pt-ops-warn');

      if (this.options.zoomOnAdd !== false) this.zoomToDefault();
      this.refreshCurrentView();
    },

    onRemove: function(mapObj) {
      this._isRemoved = true;
      this._points.clearLayers();
      this._labels.clearLayers();
      this._x2Refs.clearLayers();
      this._x2Labels.clearLayers();
      this._statusLabel.clearLayers();
      try { mapObj.removeLayer(this._points); } catch(e) {}
      try { mapObj.removeLayer(this._labels); } catch(e) {}
      try { mapObj.removeLayer(this._x2Refs); } catch(e) {}
      try { mapObj.removeLayer(this._x2Labels); } catch(e) {}
      try { mapObj.removeLayer(this._statusLabel); } catch(e) {}
      delete activeLegendDefs[this.options.name];
      redrawLegend();
      setOpsLayerLoading(this.options.name, false);
      recordStatus(this.options.name, 'Layer turned off.', 'pt-ops-muted');
      if (ptDeltaOpsActiveLayer === this) ptDeltaOpsActiveLayer = null;
      this._map = null;
    },

    forceRemove: function(mapObj) { this.onRemove(mapObj || this._map); },

    setLabelsVisible: function(show) {
      if (!this._map) return;
      var groups = [this._labels, this._x2Labels, this._statusLabel];
      for (var i = 0; i < groups.length; i++) {
        if (show) {
          if (!this._map.hasLayer(groups[i])) groups[i].addTo(this._map);
        } else {
          if (this._map.hasLayer(groups[i])) this._map.removeLayer(groups[i]);
        }
      }
    },

    zoomToDefault: function() {
      if (!this._map) return;
      this._map.setView([38.11002, -121.47775], 10);
    },

    refreshCurrentView: function() {
      var self = this;
      if (!self.options.url) {
        recordStatus(self.options.name, 'Missing Delta Ops GeoJSON URL.', 'pt-ops-warn');
        setOpsLayerLoading(self.options.name, false);
        return;
      }

      setOpsLayerLoading(self.options.name, true);
      Promise.all([
        ptDeltaOpsFetchJson(self.options.url),
        self.options.summaryUrl ? ptDeltaOpsFetchJson(self.options.summaryUrl).catch(function(){ return {}; }) : Promise.resolve({}),
        self.options.x2ReferenceUrl ? ptDeltaOpsFetchJson(self.options.x2ReferenceUrl).catch(function(){ return null; }) : Promise.resolve(null)
      ]).then(function(results) {
        if (self._isRemoved) return;
        self.draw(results[0], results[1] || {}, results[2]);
      }).catch(function(err) {
        console.error(err);
        recordStatus(self.options.name, 'Could not load Delta Ops daily summary: ' + err.message, 'pt-ops-warn');
        setOpsLayerLoading(self.options.name, false);
      });
    },

    draw: function(geojson, summary, x2Geojson) {
      var self = this;
      self._points.clearLayers();
      self._labels.clearLayers();
      self._x2Refs.clearLayers();
      self._x2Labels.clearLayers();
      self._statusLabel.clearLayers();

      var features = geojson && Array.isArray(geojson.features) ? geojson.features : [];
      var featureByKey = {};
      features.forEach(function(f) {
        var key = String(ptDeltaOpsProps(f).feature_key || '');
        if (key) featureByKey[key] = f;
      });

      var cfsScaleMax = 0;
      features.forEach(function(f) {
        var key = String(ptDeltaOpsProps(f).feature_key || '');
        if (!ptDeltaOpsScaleCfsCircle(key)) return;
        var n = ptDeltaOpsFeatureCfs(f);
        if (isFinite(n) && n > cfsScaleMax) cfsScaleMax = n;
      });

      if (x2Geojson && Array.isArray(x2Geojson.features)) {
        x2Geojson.features.forEach(function(f) {
          var ll = ptDeltaOpsLatLng(f);
          if (!ll) return;
          var p = ptDeltaOpsProps(f);
          var km = Number(p.river_km);
          L.circleMarker(ll, {
            radius: 2,
            color: '#999999',
            fillColor: '#999999',
            fillOpacity: 0.48,
            opacity: 0.48,
            weight: 0.5,
            interactive: false
          }).addTo(self._x2Refs);
          if (isFinite(km) && km % 5 === 0) {
            L.marker(ll, {
              interactive: false,
              icon: L.divIcon({className: 'pt-delta-empty-icon', html: '', iconSize: [0, 0]})
            }).bindTooltip('<span class="pt-delta-ops-x2-ref-text">' + String(km) + '</span>', {
              permanent: true,
              direction: 'center',
              className: 'pt-delta-ops-x2-label',
              opacity: 1
            }).addTo(self._x2Labels);
          }
        });
      }

      features.forEach(function(f) {
        var p = ptDeltaOpsProps(f);
        var key = String(p.feature_key || '');
        var ll = ptDeltaOpsLatLng(f);
        if (!ll) return;
        if (['controlling_factors', 'percent_inflow_diverted'].indexOf(key) >= 0) return;

        if (key === 'delta_conditions') {
          L.marker(ll, {
            interactive: false,
            icon: L.divIcon({className: 'pt-delta-empty-icon', html: '', iconSize: [0, 0]})
          }).bindTooltip('<span class="pt-delta-ops-label-text pt-delta-ops-status">' + ptDeltaOpsStatusLabel(featureByKey, summary) + '</span>', {
            permanent: true,
            direction: 'top',
            className: 'pt-delta-ops-status-label',
            opacity: 1
          }).addTo(self._statusLabel);
          return;
        }

        var color = ptDeltaOpsColor(key);
        var radius = ptDeltaOpsCircleRadius(key, f, cfsScaleMax);
        var labelHtml = '<span class="pt-delta-ops-label-text ' + ptDeltaOpsLabelClass(key) + '">' + ptDeltaOpsPrettyLabel(f, summary, featureByKey) + '</span>';
        L.circleMarker(ll, {
          radius: radius,
          color: color,
          fillColor: color,
          fillOpacity: 0.82,
          opacity: 0.92,
          weight: ptDeltaOpsScaleCfsCircle(key) ? 1.2 : 1
        }).bindPopup(ptDeltaOpsPopup(f, summary)).addTo(self._points);

        L.marker(ll, {
          interactive: false,
          icon: L.divIcon({className: 'pt-delta-empty-icon', html: '', iconSize: [0, 0]})
        }).bindTooltip(labelHtml, {
          permanent: true,
          direction: ptDeltaOpsLabelDirection(key),
          offset: ptDeltaOpsLabelOffset(key, radius),
          className: 'pt-delta-ops-label',
          opacity: 1
        }).addTo(self._labels);
      });

      self.setLabelsVisible(window.ptDeltaOpsLabelsVisible !== false);
      setOpsLayerLoading(self.options.name, false);
      recordStatus(
        self.options.name,
        'Loaded DWR Delta Ops snapshot for ' + (summary.report_date || 'latest available date') +
          '. Features: ' + features.length + '. Values are preliminary.',
        'pt-ops-ok'
      );
    }
  });

)---"
}
