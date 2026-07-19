# ==== leaflet_ops_live_nbm_wind_helpers.r ====================================
##
## PURPOSE:
##   Browser-side Ops Live NOAA NBM blended wind-guidance overlay.
##
## DISPLAY:
##   - Wind barb = NBM median sustained wind and core direction.
##   - Circle color = NBM median gust.
##   - Hover/popup = 10th/50th/90th percentile sustained and gust guidance.
##   - Time choices = +6, +12, +24, +48 hours relative to browser time.
## ============================================================================

pt_ops_live_nbm_wind_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Ops Live NBM blended wind guidance
  // --------------------------------------------------------------------------

  function ptNbmNum(x) {
    x = Number(x);
    return isFinite(x) ? x : null;
  }

  function ptNbmEsc(x) {
    return escapeHtml(String(x == null ? '' : x));
  }

  function ptNbmCardinal(deg) {
    deg = ptNbmNum(deg);
    if (deg == null) return '';
    var dirs = ['N','NNE','NE','ENE','E','ESE','SE','SSE','S','SSW','SW','WSW','W','WNW','NW','NNW'];
    deg = ((deg % 360) + 360) % 360;
    return dirs[Math.floor((deg + 11.25) / 22.5) % 16];
  }

  function ptNbmLeadLabel(hours) {
    hours = Number(hours);
    return isFinite(hours) ? '+' + Math.round(hours) + ' hr' : 'selected';
  }

  function ptNbmGustScale(summary) {
    summary = summary || {};
    var x = ptNbmNum(summary.recommended_gust_scale_mph);
    if (x == null && summary.gust_p50_mph) x = ptNbmNum(summary.gust_p50_mph.p95);
    return x == null || x <= 0 ? 45 : x;
  }

  function ptNbmColor(value, scale) {
    value = ptNbmNum(value);
    scale = ptNbmNum(scale);
    if (value == null) return '#bdbdbd';
    if (scale == null || scale <= 0) scale = 45;
    var t = Math.max(0, Math.min(1, value / scale));
    var stops = [
      [0.00, '#ffffcc'], [0.18, '#c2e699'], [0.36, '#78c679'],
      [0.54, '#31a354'], [0.70, '#2b8cbe'], [0.84, '#fdae61'],
      [0.94, '#e31a1c'], [1.00, '#7a0177']
    ];
    for (var i = 1; i < stops.length; i += 1) {
      if (t <= stops[i][0]) return stops[i][1];
    }
    return stops[stops.length - 1][1];
  }

  function ptNbmRangeText(p, prefix) {
    var lo = ptNbmNum(p[prefix + '_p10_mph']);
    var mid = ptNbmNum(p[prefix + '_p50_mph']);
    var hi = ptNbmNum(p[prefix + '_p90_mph']);
    if (mid == null) return 'not available';
    var text = mid.toFixed(0) + ' mph median';
    if (lo != null && hi != null) text += ' · p10–p90 ' + lo.toFixed(0) + '–' + hi.toFixed(0) + ' mph';
    return text;
  }

  function ptNbmBarbSvg(p, scale) {
    p = p || {};
    var mph = ptNbmNum(p.wind_p50_mph);
    var speedKt = mph == null ? 0 : mph / 1.150779448;
    var direction = ptNbmNum(p.wind_dir_degrees);
    var fill = ptNbmColor(p.gust_p50_mph, scale);
    var circle = '<circle cx="22" cy="22" r="5.2" fill="' + fill + '" stroke="#fff" stroke-width="4"/>' +
      '<circle cx="22" cy="22" r="5.2" fill="none" stroke="#333" stroke-width="1.2"/>';

    if (speedKt < 2.5 || direction == null) {
      return '<svg viewBox="0 0 44 44" width="44" height="44" aria-hidden="true">' + circle + '</svg>';
    }

    direction = ((direction % 360) + 360) % 360;
    var rounded = Math.round(speedKt / 5) * 5;
    var flags = Math.floor(rounded / 50);
    var remainder = rounded - flags * 50;
    var full = Math.floor(remainder / 10);
    var half = (remainder - full * 10) >= 5 ? 1 : 0;
    var y = 5;
    var lines = [{x1:22, y1:22, x2:22, y2:3}];
    var polygons = [];
    for (var i = 0; i < flags; i += 1) {
      polygons.push('22,' + y + ' 32,' + (y + 4) + ' 22,' + (y + 8));
      y += 8;
    }
    for (var j = 0; j < full; j += 1) {
      lines.push({x1:22, y1:y, x2:32, y2:y + 5});
      y += 4;
    }
    if (half) lines.push({x1:22, y1:y, x2:28, y2:y + 3});

    function lineHtml(stroke, width) {
      return lines.map(function(line) {
        return '<line x1="' + line.x1 + '" y1="' + line.y1 + '" x2="' + line.x2 + '" y2="' + line.y2 + '" stroke="' + stroke + '" stroke-width="' + width + '" stroke-linecap="round"/>';
      }).join('');
    }
    function polyHtml(fillColor, stroke, width) {
      return polygons.map(function(points) {
        return '<polygon points="' + points + '" fill="' + fillColor + '" stroke="' + stroke + '" stroke-width="' + width + '"/>';
      }).join('');
    }

    return '<svg viewBox="0 0 44 44" width="44" height="44" aria-hidden="true">' +
      circle + '<g transform="rotate(' + direction.toFixed(1) + ' 22 22)">' +
      lineHtml('#fff', 4.5) + polyHtml('#fff', '#fff', 3.5) +
      lineHtml('#111', 1.8) + polyHtml('#111', '#111', 1.0) +
      '</g></svg>';
  }

  function ptNbmIcon(p, scale) {
    return L.divIcon({
      className: 'pt-nbm-wind-icon',
      html: ptNbmBarbSvg(p, scale),
      iconSize: [44, 44],
      iconAnchor: [22, 22],
      popupAnchor: [0, -16],
      tooltipAnchor: [0, -16]
    });
  }

  function ptNbmTooltip(p, leadHours) {
    var dir = ptNbmNum(p.wind_dir_degrees);
    var dirText = dir == null ? 'direction unavailable' : Math.round(dir) + '° ' + (p.wind_dir_cardinal || ptNbmCardinal(dir)) + ' (from)';
    return '<b>NBM ' + ptNbmEsc(ptNbmLeadLabel(leadHours)) + ' wind guidance</b><br>' +
      'Sustained: ' + ptNbmEsc(ptNbmRangeText(p, 'wind')) + '<br>' +
      'Gust: ' + ptNbmEsc(ptNbmRangeText(p, 'gust')) + '<br>' +
      ptNbmEsc(dirText);
  }

  function ptNbmPopup(p, summary, leadHours) {
    var dir = ptNbmNum(p.wind_dir_degrees);
    var dirText = dir == null ? 'not available' : Math.round(dir) + '° ' + (p.wind_dir_cardinal || ptNbmCardinal(dir)) + ' (from)';
    var valid = ptOpsWindLocalTimeText(summary || {}, 'valid_time_utc', 'valid_time_local') || 'not available';
    var cycle = ptOpsWindCycleText(summary || {}) || 'not available';
    var fh = ptOpsWindForecastHour(summary || {});
    return '<div style="min-width:285px;max-width:380px;">' +
      '<div style="font-size:15px;font-weight:700;margin-bottom:2px;">NOAA NBM wind guidance</div>' +
      '<div style="font-size:11px;color:#555;margin-bottom:7px;">' + ptNbmEsc(ptNbmLeadLabel(leadHours)) + ' outlook · calibrated multi-model blend</div>' +
      '<div style="display:grid;grid-template-columns:88px 1fr;gap:3px 8px;font-size:12px;">' +
        '<div style="color:#666;">Sustained</div><div>' + ptNbmEsc(ptNbmRangeText(p, 'wind')) + '</div>' +
        '<div style="color:#666;">Gust</div><div>' + ptNbmEsc(ptNbmRangeText(p, 'gust')) + '</div>' +
        '<div style="color:#666;">Direction</div><div>' + ptNbmEsc(dirText) + '</div>' +
        '<div style="color:#666;">Valid</div><div>' + ptNbmEsc(valid) + '</div>' +
        '<div style="color:#666;">NBM run</div><div>' + ptNbmEsc(cycle + (fh ? ' · ' + fh : '')) + '</div>' +
      '</div>' +
      '<div style="margin-top:8px;padding-top:6px;border-top:1px solid #ddd;font-size:10px;color:#666;">The displayed ranges are NBM 10th–90th percentiles, not formal confidence limits. Use official NWS forecasts and warnings for decisions.</div>' +
      '</div>';
  }

  function ptNbmLegendLines(summary) {
    summary = summary || {};
    var lines = ['Barb speed = median sustained wind · orientation = central direction · circle color = median gust.'];
    var valid = ptOpsWindLocalTimeText(summary, 'valid_time_utc', 'valid_time_local');
    if (valid) lines.push('Valid: ' + valid);
    var cycle = ptOpsWindCycleText(summary);
    var fh = ptOpsWindForecastHour(summary);
    if (cycle || fh) lines.push('NBM run: ' + (cycle || 'unknown') + (fh ? ' · ' + fh : ''));
    return lines;
  }

  function ptNbmUpdateLegend(name, summary, leadHours, loading, note) {
    if (!activeLegendDefs || !activeLegendDefs[name]) return;
    var def = activeLegendDefs[name];
    def.metricLines = ptNbmLegendLines(summary);
    def.nbmSummary = summary || {};
    def.windSummary = summary || {};
    def.windLeadHours = Number(leadHours);
    def.windLeadLoading = loading === true;
    def.legendNote = note || 'NBM blended guidance; p10–p90 ranges appear in hover and popup.';
    redrawLegend();
  }

  var PtOpsNbmWindGuidanceLayer = L.Layer.extend({
    initialize: function(options) {
      this.options = options || {};
      this._map = null;
      this._manifest = null;
      this._summary = null;
      this._features = [];
      this._group = null;
      this._removed = true;
      this._loadToken = 0;
      this._fieldToken = 0;
      this._leadHours = 6;
      this._modelKey = String(this.options.modelKey || 'nbm');
      this._leadOptions = Array.isArray(this.options.leadOptions) ? this.options.leadOptions : [
        {hours: 6, label: '+6 hr'}, {hours: 12, label: '+12 hr'},
        {hours: 24, label: '+24 hr'}, {hours: 48, label: '+48 hr'}
      ];
      this._drawHandler = null;
    },

    onAdd: function(mapObj) {
      this._map = mapObj;
      this._removed = false;
      this._loadToken += 1;
      var token = this._loadToken;
      var self = this;
      var name = this.options.name || 'Wind outlook | NOAA NBM guidance';
      var manifestUrl = String(this.options.manifestUrl || '');
      this._group = L.layerGroup().addTo(mapObj);
      this._drawHandler = function() { self._draw(); };
      mapObj.on('zoomend moveend', this._drawHandler);
      ptOpsWindRegisterLeadController(this._modelKey, this);

      activeLegendDefs[name] = {
        note: 'NOAA National Blend of Models calibrated wind guidance with uncertainty ranges.',
        legendType: 'wind_guidance_nbm',
        sourceUrl: this.options.sourceUrl || '',
        infoUrl: manifestUrl,
        infoLabel: 'feed manifest',
        metricLines: [],
        legendNote: 'Fetching NOAA NBM guidance manifest…',
        windModelKey: this._modelKey,
        windLeadHours: this._leadHours,
        windLeadOptions: this._leadOptions,
        windLeadLoading: true,
        windSummary: {},
        nbmSummary: {}
      };
      redrawLegend();
      setOpsLayerLoading(name, true);
      recordStatus(name, 'Fetching NOAA NBM wind-guidance manifest…', 'pt-ops-warn');

      ptOpsWindFetchJson(manifestUrl).then(function(manifest) {
        if (self._removed || token !== self._loadToken) return;
        self._manifest = manifest || {};
        self._loadSelectedLead();
      }).catch(function(err) {
        if (self._removed || token !== self._loadToken) return;
        console.error(err);
        setOpsLayerLoading(name, false);
        recordStatus(name, 'NBM guidance unavailable: ' + (err && err.message ? err.message : err), 'pt-ops-bad');
        ptNbmUpdateLegend(name, {}, self._leadHours, false, 'Could not load the NBM guidance manifest.');
      });
    },

    setLeadHours: function(hours) {
      hours = Number(hours);
      var allowed = this._leadOptions.some(function(x) { return Math.abs(Number(x.hours) - hours) < 0.01; });
      if (!allowed || this._removed) return;
      if (Math.abs(hours - this._leadHours) < 0.01 && this._summary) return;
      this._leadHours = hours;
      if (this._manifest) this._loadSelectedLead();
    },

    _loadSelectedLead: function() {
      if (this._removed || !this._manifest) return;
      this._fieldToken += 1;
      var fieldToken = this._fieldToken;
      var self = this;
      var name = this.options.name || 'Wind outlook | NOAA NBM guidance';
      var manifestUrl = String(this.options.manifestUrl || '');
      var targetMs = Date.now() + this._leadHours * 36e5;
      var selected;
      try {
        selected = ptOpsWindSelectManifestEntry(this._manifest, targetMs);
        var distanceHours = ptOpsWindTargetDistanceHours(selected, targetMs);
        if (distanceHours == null || distanceHours > Number(this.options.maxTargetErrorHours || 3.1)) {
          throw new Error('No NBM field is close enough to the requested target time.');
        }
      } catch(err) {
        this._handleError(err);
        return;
      }

      var url;
      try {
        url = ptOpsWindResolveEntryUrl(manifestUrl, selected.relative_url || selected.url);
      } catch(err) {
        this._handleError(err);
        return;
      }

      ptNbmUpdateLegend(name, selected, this._leadHours, true, 'Fetching ' + ptNbmLeadLabel(this._leadHours) + ' NBM guidance…');
      setOpsLayerLoading(name, true);
      recordStatus(name, 'Fetching ' + ptNbmLeadLabel(this._leadHours) + ' NBM wind guidance…', 'pt-ops-warn');

      ptOpsWindFetchJson(url).then(function(data) {
        if (self._removed || fieldToken !== self._fieldToken) return;
        if (!data || !Array.isArray(data.features)) throw new Error('NBM GeoJSON did not contain a feature array.');
        self._features = data.features;
        self._summary = selected;
        self._draw();
        ptNbmUpdateLegend(name, selected, self._leadHours, false);
        setOpsLayerLoading(name, false);
        recordStatus(
          name,
          ptNbmLeadLabel(self._leadHours) + ' NBM guidance loaded: ' +
            (selected.feature_count || self._features.length) + ' grid points. Model guidance, not observations.',
          'pt-ops-ok'
        );
      }).catch(function(err) {
        if (self._removed || fieldToken !== self._fieldToken) return;
        self._handleError(err);
      });
    },

    _handleError: function(err) {
      var name = this.options.name || 'Wind outlook | NOAA NBM guidance';
      console.error(err);
      setOpsLayerLoading(name, false);
      recordStatus(name, 'NBM guidance unavailable: ' + (err && err.message ? err.message : err), 'pt-ops-bad');
      ptNbmUpdateLegend(name, this._summary || {}, this._leadHours, false, 'Could not load the selected NBM outlook; any previously displayed field was retained.');
    },

    _draw: function() {
      if (!this._map || !this._group || !this._features.length) return;
      this._group.clearLayers();
      var z = this._map.getZoom();
      var step = z <= 5 ? 5 : (z === 6 ? 3 : (z === 7 ? 2 : 1));
      var bounds = this._map.getBounds().pad(0.18);
      var scale = ptNbmGustScale(this._summary || {});
      var lead = this._leadHours;
      var summary = this._summary || {};
      var group = this._group;

      this._features.forEach(function(feature) {
        if (!feature || !feature.geometry || !Array.isArray(feature.geometry.coordinates)) return;
        var lon = Number(feature.geometry.coordinates[0]);
        var lat = Number(feature.geometry.coordinates[1]);
        if (!isFinite(lat) || !isFinite(lon) || !bounds.contains([lat, lon])) return;
        var p = feature.properties || {};
        var gi = Number(p.grid_i), gj = Number(p.grid_j);
        if (step > 1 && ((isFinite(gi) ? gi : 0) % step !== 0 || (isFinite(gj) ? gj : 0) % step !== 0)) return;
        var marker = L.marker([lat, lon], {
          icon: ptNbmIcon(p, scale),
          interactive: true,
          keyboard: false,
          riseOnHover: true
        });
        marker.bindTooltip(ptNbmTooltip(p, lead), {sticky: true, direction: 'top', opacity: 0.96});
        marker.bindPopup(ptNbmPopup(p, summary, lead), {maxWidth: 410});
        marker.addTo(group);
      });
    },

    onRemove: function(mapObj) {
      var name = this.options.name || 'Wind outlook | NOAA NBM guidance';
      this._removed = true;
      this._loadToken += 1;
      this._fieldToken += 1;
      if (this._drawHandler && mapObj) {
        try { mapObj.off('zoomend moveend', this._drawHandler); } catch(e) {}
      }
      if (this._group && mapObj) {
        try { mapObj.removeLayer(this._group); } catch(e) {}
      }
      this._group = null;
      this._features = [];
      this._manifest = null;
      this._summary = null;
      this._drawHandler = null;
      ptOpsWindUnregisterLeadController(this._modelKey, this);
      delete activeLegendDefs[name];
      setOpsLayerLoading(name, false);
      redrawLegend();
      recordStatus(name, 'Layer turned off.', 'pt-ops-muted');
      this._map = null;
    },

    forceRemove: function(mapObj) {
      this.onRemove(mapObj || this._map);
    }
  });

  addOpsExternalLinks({
    category: 'Atmosphere / Wind',
    subgroup: 'Wind',
    title: 'NBM guidance links',
    note: 'Calibrated multi-model NOAA/NWS wind guidance and uncertainty information.',
    links: [
      {label: 'NOAA NBM', url: 'https://vlab.noaa.gov/web/mdl/nbm', title: 'Open NOAA National Blend of Models information'},
      {label: 'NCEP NBM products', url: 'https://www.nco.ncep.noaa.gov/pmb/products/blend/', title: 'Open NCEP NBM products'},
      {label: 'NBM dashboard', url: 'https://blend.mdl.nws.noaa.gov/nbm-dashboard', title: 'Open NOAA NBM dashboard'}
    ]
  });

  if (includeNbmWindGuidance && NBM_WIND_GUIDANCE_MANIFEST_URL) {
    addOpsLayer({
      category: 'Atmosphere / Wind',
      subgroup: 'Wind',
      name: 'Wind outlook | NOAA NBM guidance',
      sourceUrl: 'https://vlab.noaa.gov/web/mdl/nbm',
      infoUrl: NBM_WIND_GUIDANCE_MANIFEST_URL,
      infoLabel: 'feed manifest',
      legendType: 'wind_guidance_nbm',
      helperText: 'Calibrated multi-model wind guidance at +6, +12, +24, and +48 hr. Barb = median sustained wind; circle color = median gust; hover/popup = p10–p90 range.',
      layer: new PtOpsNbmWindGuidanceLayer({
        name: 'Wind outlook | NOAA NBM guidance',
        manifestUrl: NBM_WIND_GUIDANCE_MANIFEST_URL,
        sourceUrl: 'https://vlab.noaa.gov/web/mdl/nbm',
        modelKey: 'nbm',
        leadOptions: [
          {hours: 6, label: '+6 hr'},
          {hours: 12, label: '+12 hr'},
          {hours: 24, label: '+24 hr'},
          {hours: 48, label: '+48 hr'}
        ],
        maxTargetErrorHours: 3.1
      })
    });
  }

  )---"
}
