# ==== leaflet_ops_live_cnrfc_precip_weather_helpers.r =======================
##
## PURPOSE:
##   Browser-side Ops Live helper for CNRFC precip/weather stations with
##   NWS/WRH time-series links.
##
## DESIGN:
##   The 54_ NOAA/NWS audit writes a slim map-ready RDS for Ops Live. The
##   user-facing map intentionally hides audit mechanics: symbols and filters
##   focus on station/source type, while QA fields remain available in the RDS.
## ============================================================================

pt_ops_live_cnrfc_precip_weather_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // NWS/WRH precip/weather station time-series candidates from the CNRFC station catalog.
  // --------------------------------------------------------------------------

  function ptCnrfcPwTrim(value) {
    if (value === null || value === undefined) return '';
    return String(value).trim();
  }

  function ptCnrfcPwBool(value) {
    if (value === true) return true;
    if (value === false || value === null || value === undefined) return false;
    var txt = String(value).trim().toLowerCase();
    return txt === 'true' || txt === 't' || txt === '1' || txt === 'yes' || txt === 'y';
  }

  function ptCnrfcPwNumber(value) {
    if (value === null || value === undefined || value === false || value === true) return null;
    if (typeof value === 'string') {
      value = value.trim();
      if (!value || ['NA', 'NULL', 'NAN'].indexOf(value.toUpperCase()) >= 0) return null;
    }
    var n = Number(value);
    return isFinite(n) ? n : null;
  }

  function ptCnrfcPwUrl(label, url) {
    url = ptCnrfcPwTrim(url);
    if (!url) return '';
    return '<div style="margin:1px 0;"><a href="' + escapeHtml(url) + '" target="_blank" rel="noopener">' + escapeHtml(label) + '</a></div>';
  }

  function ptCnrfcPwStationId(p) {
    return ptCnrfcPwTrim((p || {}).cnrfc_id).toUpperCase();
  }

  function ptCnrfcPwIsAirportId(id) {
    id = ptCnrfcPwTrim(id).toUpperCase();
    return /^[A-Z0-9]{3}$/.test(id) || /^K[A-Z0-9]{3}$/.test(id);
  }

  function ptCnrfcPwSourceClass(p) {
    p = p || {};
    var explicit = ptCnrfcPwTrim(p.source_class || p.precip_weather_station_type_hint).toLowerCase();
    var id = ptCnrfcPwStationId(p);
    var txt = [explicit, ptCnrfcPwTrim(p.datatransmission_types), ptCnrfcPwTrim(p.raw_precip_source_code)].join(';').toUpperCase();

    if (explicit.indexOf('asos') >= 0 || explicit.indexOf('airport') >= 0) return 'asos';
    if (explicit.indexOf('goes') >= 0) return 'goes';
    if (explicit.indexOf('alert') >= 0 || explicit.indexOf('event') >= 0) return 'alert';
    if (/\bZ\b/.test(txt) && ptCnrfcPwIsAirportId(id)) return 'asos';
    if (/\bG\b/.test(txt) || txt.indexOf('GOES') >= 0) return 'goes';
    if (/\bR\b/.test(txt) || txt.indexOf('ALERT') >= 0) return 'alert';
    return 'other';
  }

  function ptCnrfcPwSourceLabel(p) {
    var cls = ptCnrfcPwSourceClass(p);
    var raw = ptCnrfcPwTrim((p || {}).raw_precip_source_code || (p || {}).datatransmission_types);
    if (cls === 'asos') return raw ? 'ASOS/airport (' + raw + ')' : 'ASOS/airport';
    if (cls === 'goes') return raw ? 'RAWS/GOES (' + raw + ')' : 'RAWS/GOES';
    if (cls === 'alert') return raw ? 'ALERT/event only (' + raw + ')' : 'ALERT/event only';
    return raw ? 'Other / not parsed (' + raw + ')' : 'Other / not parsed';
  }

  function ptCnrfcPwWrhSiteId(id, sourceClass) {
    id = ptCnrfcPwTrim(id).toUpperCase();
    if (!id) return '';
    if (sourceClass === 'asos' && /^[A-Z0-9]{3}$/.test(id)) return 'K' + id;
    return id;
  }

  function ptCnrfcPwWrhTimeseriesUrl(id, sourceClass, p) {
    var fromData = ptCnrfcPwTrim((p || {}).wrh_timeseries_url);
    if (fromData) return fromData;
    var site = ptCnrfcPwWrhSiteId(id, sourceClass);
    if (!site) return '';
    return 'https://www.weather.gov/wrh/timeseries?site=' + encodeURIComponent(site);
  }

  function ptCnrfcPwIsVerified(p) {
    p = p || {};
    if (ptCnrfcPwBool(p.noaa_api_verified) || ptCnrfcPwBool(p.is_ops_timeseries_verified)) return true;
    var cls = ptCnrfcPwTrim(p.noaa_station_data_availability_class);
    return cls === 'verified_api_latest_observation' || cls === 'verified_api_recent_observation' || cls === 'verified_wrh_static_station_page';
  }

  function ptCnrfcPwCheckedCandidate(p) {
    p = p || {};
    if (ptCnrfcPwBool(p.noaa_api_checked_not_verified) || ptCnrfcPwBool(p.is_ops_timeseries_checked_candidate)) return true;
    return ptCnrfcPwTrim(p.noaa_station_data_availability_class) === 'candidate_wrh_user_link_api_not_verified';
  }

  function ptCnrfcPwUncheckedCandidate(p) {
    p = p || {};
    return ptCnrfcPwBool(p.noaa_api_not_checked) || ptCnrfcPwBool(p.is_ops_timeseries_unchecked_candidate);
  }

  function ptCnrfcPwVerificationLabel(p) {
    if (ptCnrfcPwIsVerified(p)) return 'verified by NWS API latest/recent observation';
    if (ptCnrfcPwCheckedCandidate(p)) return 'WRH candidate; API not verified in audit';
    if (ptCnrfcPwUncheckedCandidate(p)) return 'candidate; not checked in current audit cache';
    return 'not verified';
  }

  function ptCnrfcPwHasWrhCandidate(p) {
    p = p || {};
    if (ptCnrfcPwTrim(p.wrh_timeseries_url)) return true;
    var id = ptCnrfcPwStationId(p);
    var cls = ptCnrfcPwSourceClass(p);
    var site = ptCnrfcPwWrhSiteId(id, cls);
    return /^[A-Z0-9]{4,6}$/.test(site);
  }

  function ptCnrfcPwIsPriority(p) {
    var cls = ptCnrfcPwSourceClass(p);
    return ptCnrfcPwIsVerified(p) || cls === 'asos' || cls === 'goes';
  }

  function ptCnrfcPwCnrfcObservedPrecipUrl(p) {
    return ptCnrfcPwTrim((p || {}).cnrfc_observed_precip_map_url || (p || {}).cnrfc_source_url) || 'https://www.cnrfc.noaa.gov/rainfall_data.php';
  }

  function ptCnrfcPwEnsureStyle() {
    if (document.getElementById('pt-ops-cnrfc-pw-style')) return;

    var style = document.createElement('style');
    style.id = 'pt-ops-cnrfc-pw-style';
    style.innerHTML = `
      .pt-cnrfc-pw-divicon {
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .pt-cnrfc-pw-marker {
        position: relative;
        display: block;
        box-sizing: border-box;
        transform: none;
        margin: 0;
        width: 9px;
        height: 9px;
        border-radius: 50%;
        border: 1px solid #4d4d4d;
        background: #bdbdbd;
        opacity: 0.76;
      }
      .pt-cnrfc-pw-goes { background:#2ca25f; border-color:#005a32; }
      .pt-cnrfc-pw-asos { background:#fdae61; border-color:#8c510a; }
      .pt-cnrfc-pw-alert { background:#9e9ac8; border-color:#54278f; }
      .pt-cnrfc-pw-other { background:#bdbdbd; border-color:#4d4d4d; }
      .pt-cnrfc-pw-marker {
        will-change: opacity;
      }
      .pt-cnrfc-pw-filter-row {
        display:flex;
        flex-wrap:wrap;
        gap:3px 8px;
        margin-top:5px;
        font-size:11px;
        line-height:1.15;
      }
      .pt-cnrfc-pw-filter-row label {
        white-space:nowrap;
        display:inline-flex;
        align-items:center;
        gap:2px;
      }
      .pt-cnrfc-pw-filter-row input { margin:0 2px 0 0; }
      .leaflet-tooltip.pt-ops-cnrfc-pw-tooltip {
        background: rgba(255,255,255,0.96);
        border: 1px solid rgba(0,0,0,0.42);
        border-radius: 4px;
        box-shadow: 0 2px 7px rgba(0,0,0,0.22);
        color: #111;
        padding: 5px 7px;
        font: 12px/1.25 Arial, Helvetica, sans-serif;
        white-space: pre;
      }
      .pt-ops-cnrfc-pw-popup {
        font: 12px/1.3 Arial, Helvetica, sans-serif;
        min-width: 225px;
        max-width: 350px;
      }
      .pt-ops-cnrfc-pw-popup .pt-muted { color:#555; font-size:11px; }
      .pt-ops-cnrfc-pw-popup a { color: #1f5e9c; text-decoration: none; }
      .pt-ops-cnrfc-pw-popup a:hover { text-decoration: underline; }
    `;
    document.head.appendChild(style);
  }

  function ptCnrfcPwIcon(p) {
    p = p || {};
    var sourceClass = ptCnrfcPwSourceClass(p);
    var cls = ['pt-cnrfc-pw-marker', 'pt-cnrfc-pw-' + sourceClass];
    var size = 12;
    return L.divIcon({
      className: 'pt-cnrfc-pw-divicon',
      html: '<span class="' + cls.join(' ') + '"></span>',
      iconSize: [size, size],
      iconAnchor: [size / 2, size / 2],
      popupAnchor: [0, -size / 2]
    });
  }

  function ptCnrfcPwTooltipText(p) {
    p = p || {};
    var id = ptCnrfcPwTrim(p.cnrfc_id);
    var name = ptCnrfcPwTrim(p.display_name);
    var src = ptCnrfcPwSourceLabel(p).replace(/ \(.+\)$/, '');
    return [id || 'CNRFC', name || '', src].filter(function(x) { return !!x; }).join('\n');
  }

  function ptCnrfcPwPopupHtml(p) {
    p = p || {};
    var id = ptCnrfcPwTrim(p.cnrfc_id);
    var name = ptCnrfcPwTrim(p.display_name) || id || 'CNRFC precip/weather station';
    var sourceLabel = ptCnrfcPwSourceLabel(p);
    var alsoForecast = ptCnrfcPwBool(p.also_active_river_reservoir_forecast_point || p.in_cnrfc_river_reservoir_catalog);
    var sourceClass = ptCnrfcPwSourceClass(p);
    var wrhUrl = ptCnrfcPwWrhTimeseriesUrl(id, sourceClass, p);

    var html = '<div class="pt-ops-cnrfc-pw-popup">' +
      '<div><b>' + escapeHtml(id || name) + '</b></div>' +
      (name && name !== id ? '<div>' + escapeHtml(name) + '</div>' : '') +
      '<div><b>Source:</b> ' + escapeHtml(sourceLabel) + '</div>';

    if (alsoForecast) {
      html += '<div class="pt-muted">Also appears in CNRFC river/reservoir catalog</div>';
    }

    html += '<div style="margin-top:5px;">' +
      ptCnrfcPwUrl('NWS WRH time series', wrhUrl) +
      '</div>';

    html += '<div class="pt-muted" style="margin-top:4px;">Station time-series availability may vary by station and period.</div>';
    html += '</div>';
    return html;
  }

  function ptCnrfcPwDefaultFilters() {
    return { goes: true, asos: true, alert: false, other: false }; 
  }

  function ptCnrfcPwFilters() {
    if (!window.ptCnrfcPwFilters) window.ptCnrfcPwFilters = ptCnrfcPwDefaultFilters();
    return window.ptCnrfcPwFilters;
  }

  function ptCnrfcPwRecordVisible(p, filters) {
    filters = filters || ptCnrfcPwFilters();
    var cls = ptCnrfcPwSourceClass(p);
    return !!filters[cls];
  }

  function ptCnrfcPwAvailableSourceClasses() {
    var counts = { goes: 0, asos: 0, alert: 0, other: 0 };
    var records = [];
    if (window.ptCnrfcPwActiveLayer && window.ptCnrfcPwActiveLayer._records) {
      records = window.ptCnrfcPwActiveLayer._records;
    } else if (Array.isArray(window.CNRFC_PRECIP_WEATHER_STATIONS)) {
      records = window.CNRFC_PRECIP_WEATHER_STATIONS;
    }
    records.forEach(function(p) {
      var cls = ptCnrfcPwSourceClass(p);
      counts[cls] = (counts[cls] || 0) + 1;
    });
    return counts;
  }

  function ptCnrfcPwLegendHtml() {
    var f = ptCnrfcPwFilters();
    var counts = ptCnrfcPwAvailableSourceClasses();
    function checked(key) { return f[key] ? ' checked' : ''; }
    function filterBox(key, label) {
      if (!counts[key]) return '';
      return '<label><input type="checkbox" class="pt-cnrfc-pw-filter" data-filter="' + key + '"' + checked(key) + '>' + label + '</label>';
    }
    function legendLine(key, label, fill, border) {
      if (!counts[key]) return '';
      return '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="background:' + fill + ';border-color:' + border + ';"></span>' + label + '</div>';
    }
    return '<div class="pt-ops-map-legend-section">' +
      '<h4>NWS weather stations | NWS/WRH time series</h4>' +
      '<div class="pt-ops-legend-small">NWS weather stations from the CNRFC station catalog with NWS/WRH time-series links.</div>' +
      '<div class="pt-cnrfc-pw-filter-row">' +
        filterBox('goes', 'RAWS/GOES') +
        filterBox('asos', 'ASOS') +
        filterBox('alert', 'ALERT') +
        filterBox('other', 'Other') +
      '</div>' +
      '<div class="pt-ops-legend-grid-2" style="margin-top:4px;">' +
        legendLine('goes', 'RAWS/GOES', '#2ca25f', '#005a32') +
        legendLine('asos', 'ASOS/airport', '#fdae61', '#8c510a') +
        legendLine('alert', 'ALERT/event', '#9e9ac8', '#54278f') +
        legendLine('other', 'Other', '#bdbdbd', '#4d4d4d') +
      '</div>' +
      '<div class="pt-ops-legend-small" id="pt-cnrfc-pw-counts" style="margin-top:5px;border-top:1px solid rgba(0,0,0,0.12);padding-top:4px;"></div>' +
      '</div>';
  }

  function ptCnrfcPwRefreshLegendCounts(counts) {
    var el = document.getElementById('pt-cnrfc-pw-counts');
    if (!el || !counts) return;
    el.textContent = 'Showing ' + (counts.visible || 0).toLocaleString() + ' / ' + (counts.total || 0).toLocaleString() + ' stations';
  }

  function ptCnrfcPwWireFilterEvents() {
    if (window.ptCnrfcPwFilterEventsWired) return;
    window.ptCnrfcPwFilterEventsWired = true;
    document.addEventListener('change', function(e) {
      var t = e.target;
      if (!t || !t.classList || !t.classList.contains('pt-cnrfc-pw-filter')) return;
      var key = t.getAttribute('data-filter');
      if (!key) return;
      var f = ptCnrfcPwFilters();
      f[key] = !!t.checked;
      if (window.ptCnrfcPwActiveLayer && typeof window.ptCnrfcPwActiveLayer.applyFilters === 'function') {
        window.ptCnrfcPwActiveLayer.applyFilters();
      }
    });
  }

  var CnrfcPrecipWeatherStationsLayer = L.Layer.extend({
    initialize: function(options) {
      this.options = options || {};
      this._layerGroup = null;
      this._priorityGroup = null;
      this._backgroundGroup = null;
      this._map = null;
      this._records = this.options.records || [];
      this._markers = [];
    },
    _makeGroups: function() {
      this._layerGroup = L.layerGroup();
      var clusterOptions = {
        chunkedLoading: true,
        chunkInterval: 120,
        chunkDelay: 40,
        maxClusterRadius: function(zoom) {
          if (zoom < 7) return 85;
          if (zoom < 10) return 65;
          return 45;
        },
        // Match the CNRFC catalog point layers: keep regional clustering, but
        // split at project scale so Local-vs-Ops coincidences are readable.
        disableClusteringAtZoom: 9,
        spiderfyOnMaxZoom: true,
        showCoverageOnHover: false,
        removeOutsideVisibleBounds: true,
        animate: false
      };
      this._priorityGroup = (L.markerClusterGroup ? L.markerClusterGroup(clusterOptions) : L.layerGroup());
      this._backgroundGroup = L.layerGroup();
      this._layerGroup.addLayer(this._backgroundGroup);
      this._layerGroup.addLayer(this._priorityGroup);
    },
    _markerForRecord: function(p) {
      var lat = ptCnrfcPwNumber(p.lat);
      var lon = ptCnrfcPwNumber(p.lon);
      if (lat === null || lon === null || Math.abs(lat) > 90 || Math.abs(lon) > 180) return null;
      var marker = L.marker([lat, lon], {
        icon: ptCnrfcPwIcon(p),
        keyboard: false,
        riseOnHover: true
      });
      marker._ptCnrfcPwRecord = p;
      marker.bindTooltip(ptCnrfcPwTooltipText(p), {
        direction: 'top',
        sticky: false,
        offset: [0, -8],
        opacity: 0.92,
        className: 'pt-ops-cnrfc-pw-tooltip'
      });
      marker.on('mouseout', function() {
        try { marker.closeTooltip(); } catch(e) {}
      });
      marker.bindPopup(ptCnrfcPwPopupHtml(p), {
        maxWidth: 390,
        maxHeight: 520,
        autoPan: true,
        keepInView: true
      });
      return marker;
    },
    _buildMarkers: function() {
      var self = this;
      this._markers = [];
      this._records.forEach(function(p) {
        var marker = self._markerForRecord(p);
        if (marker) self._markers.push(marker);
      });
    },
    _clearClusterGroups: function() {
      try { if (this._priorityGroup && this._priorityGroup.clearLayers) this._priorityGroup.clearLayers(); } catch(e) {}
      try { if (this._backgroundGroup && this._backgroundGroup.clearLayers) this._backgroundGroup.clearLayers(); } catch(e) {}
    },
    _addMarkerBatch: function(group, markers) {
      if (!group || !markers || !markers.length) return;
      if (typeof group.addLayers === 'function') {
        group.addLayers(markers);
      } else {
        markers.forEach(function(marker) { group.addLayer(marker); });
      }
    },
    _renderFilteredMarkers: function() {
      if (!this._priorityGroup || !this._backgroundGroup) return;
      this._clearClusterGroups();
      var visible = 0;
      var priorityMarkers = [];
      var backgroundMarkers = [];
      var filters = ptCnrfcPwFilters();
      this._markers.forEach(function(marker) {
        var p = marker._ptCnrfcPwRecord || {};
        if (!ptCnrfcPwRecordVisible(p, filters)) return;
        if (ptCnrfcPwIsPriority(p)) { priorityMarkers.push(marker); }
        else { backgroundMarkers.push(marker); }
        visible += 1;
      }, this);
      this._addMarkerBatch(this._priorityGroup, priorityMarkers);
      this._addMarkerBatch(this._backgroundGroup, backgroundMarkers);
      this._visibleCount = visible;
      this._priorityVisibleCount = priorityMarkers.length;
      ptCnrfcPwRefreshLegendCounts({ visible: visible, total: this._markers.length });
      var name = this.options.name || 'NWS weather stations | NWS/WRH time series';
      recordStatus(name, 'Showing ' + visible.toLocaleString() + ' of ' + this._markers.length.toLocaleString() + ' NWS/WRH station time-series points.', 'pt-ops-ok');
    },
    applyFilters: function() {
      this._renderFilteredMarkers();
    },
    onAdd: function(mapObj) {
      ptCnrfcPwEnsureStyle();
      ptCnrfcPwWireFilterEvents();
      window.ptCnrfcPwActiveLayer = this;
      var name = this.options.name || 'NWS weather stations | NWS/WRH time series';
      var sourceUrl = this.options.sourceUrl || 'https://www.cnrfc.noaa.gov/';

      activeLegendDefs[name] = {
        note: this.options.note || '',
        legendType: 'cnrfc_precip_weather_stations',
        sourceUrl: sourceUrl,
        infoUrl: 'https://www.cnrfc.noaa.gov/rainfall_data.php',
        infoLabel: 'CNRFC'
      };
      redrawLegend();

      this._map = mapObj;
      this._makeGroups();
      this._buildMarkers();
      this._layerGroup.addTo(mapObj);
      this._renderFilteredMarkers();
      setOpsLayerLoading(name, false);
      setTimeout(function() {
        if (window.ptCnrfcPwActiveLayer) window.ptCnrfcPwActiveLayer.applyFilters();
      }, 0);
    },
    onRemove: function(mapObj) {
      var name = this.options.name || 'NWS weather stations | NWS/WRH time series';
      if (window.ptCnrfcPwActiveLayer === this) window.ptCnrfcPwActiveLayer = null;
      if (this._layerGroup) {
        try { mapObj.removeLayer(this._layerGroup); } catch(e) {}
        this._layerGroup = null;
      }
      this._priorityGroup = null;
      this._backgroundGroup = null;
      this._map = null;
      delete activeLegendDefs[name];
      redrawLegend();
      setOpsLayerLoading(name, false);
    },
    forceRemove: function(mapObj) {
      this.onRemove(mapObj);
    }
  });


)---"
}
