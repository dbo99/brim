# ==== leaflet_ops_live_wind_helpers.r =======================================
##
## PURPOSE:
##   Browser-side Ops Live wind/vector-field overlay helpers.
##
## DESIGN:
##   This module adds the BRIM real-time NOAA GFS 10-m wind-field layer. It
##   lazy-loads the small GFS time-set manifest only when the Ops Live row is
##   turned on, selects the entry whose UTC valid time is closest to browser
##   time, fetches only that entry's U/V JSON, and renders it with a
##   leaflet-velocity canvas overlay. Particle color scaling uses metadata in
##   the selected manifest entry; BRIM does not scan the full U/V arrays to
##   calculate the scale. The feed is model output, not observed wind.
## ============================================================================

pt_ops_live_wind_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Ops Live wind/vector-field overlay: NOAA GFS 10-m surface wind
  // --------------------------------------------------------------------------

  function ptOpsWindFetchJson(url) {
    url = String(url || '');
    if (!url) return Promise.reject(new Error('Missing wind feed URL.'));

    return fetch(url, { cache: 'no-store' }).then(function(resp) {
      if (!resp.ok) {
        throw new Error('HTTP ' + resp.status + ' for ' + url);
      }
      return resp.json();
    });
  }

  function ptOpsWindParseDate(value) {
    if (value == null || value === '') return null;
    var d = new Date(String(value));
    if (isNaN(d.getTime())) return null;
    return d;
  }

  function ptOpsWindNumber(value) {
    if (value == null || value === '') return null;
    var n = Number(value);
    return isFinite(n) ? n : null;
  }

  function ptOpsWindFirstNumber(values) {
    values = Array.isArray(values) ? values : [];
    for (var i = 0; i < values.length; i += 1) {
      var n = ptOpsWindNumber(values[i]);
      if (n != null) return n;
    }
    return null;
  }

  var ptOpsWindLeadControllers = {};

  function ptOpsWindLeadLabel(hours) {
    hours = Number(hours);
    if (!isFinite(hours) || Math.abs(hours) < 0.01) return 'Current';
    return '+' + Math.round(hours) + ' hr';
  }

  function ptOpsWindRegisterLeadController(modelKey, layer) {
    modelKey = String(modelKey || '');
    if (!modelKey || !layer) return;
    ptOpsWindLeadControllers[modelKey] = layer;
  }

  function ptOpsWindUnregisterLeadController(modelKey, layer) {
    modelKey = String(modelKey || '');
    if (!modelKey) return;
    if (!layer || ptOpsWindLeadControllers[modelKey] === layer) {
      delete ptOpsWindLeadControllers[modelKey];
    }
  }

  function ptOpsWindSetLead(modelKey, hours) {
    modelKey = String(modelKey || '');
    var layer = ptOpsWindLeadControllers[modelKey];
    if (!layer || typeof layer.setLeadHours !== 'function') return false;
    layer.setLeadHours(Number(hours));
    return true;
  }

  function ptOpsWindTargetDistanceHours(entry, targetMs) {
    entry = entry || {};
    var valid = ptOpsWindParseDate(entry.valid_time_utc || entry.valid_time || entry.validTimeUtc);
    if (!valid) return null;
    return Math.abs(valid.getTime() - Number(targetMs)) / 36e5;
  }

  function ptOpsWindFormatPacific(value) {
    var d = ptOpsWindParseDate(value);
    if (!d) return '';
    try {
      return new Intl.DateTimeFormat('en-US', {
        timeZone: 'America/Los_Angeles',
        month: 'short',
        day: 'numeric',
        hour: 'numeric',
        minute: '2-digit',
        timeZoneName: 'short'
      }).format(d);
    } catch(e) {
      return d.toLocaleString();
    }
  }

  function ptOpsWindFormatPacificHourRange(value) {
    var start = ptOpsWindParseDate(value);
    if (!start) return '';
    var end = new Date(start.getTime() + 60 * 60 * 1000);

    try {
      var dateFmt = new Intl.DateTimeFormat('en-US', {
        timeZone: 'America/Los_Angeles',
        month: 'short',
        day: 'numeric'
      });
      var timeFmt = new Intl.DateTimeFormat('en-US', {
        timeZone: 'America/Los_Angeles',
        hour: 'numeric',
        minute: '2-digit'
      });
      var zoneFmt = new Intl.DateTimeFormat('en-US', {
        timeZone: 'America/Los_Angeles',
        timeZoneName: 'short'
      });
      var zoneParts = zoneFmt.formatToParts(end);
      var zone = '';
      for (var i = 0; i < zoneParts.length; i += 1) {
        if (zoneParts[i].type === 'timeZoneName') {
          zone = zoneParts[i].value;
          break;
        }
      }

      if (dateFmt.format(start) === dateFmt.format(end)) {
        return dateFmt.format(start) + ', ' + timeFmt.format(start) + '–' +
          timeFmt.format(end) + (zone ? ' ' + zone : '');
      }

      return ptOpsWindFormatPacific(start) + '–' + ptOpsWindFormatPacific(end);
    } catch(e) {
      return ptOpsWindFormatPacific(start);
    }
  }

  function ptOpsWindLocalTimeText(summary, utcField, localField) {
    summary = summary || {};
    var formatted = ptOpsWindFormatPacific(summary[utcField]);
    if (formatted) return formatted;
    var localText = summary[localField];
    return localText == null ? '' : String(localText);
  }

  function ptOpsWindCycleText(summary) {
    summary = summary || {};
    var raw = summary.model_cycle_utc || summary.model_cycle || summary.refTime || summary.ref_time_utc;
    var formatted = ptOpsWindFormatPacific(raw);
    if (formatted) return formatted;
    return summary.model_cycle_local == null ? '' : String(summary.model_cycle_local);
  }

  function ptOpsWindForecastHour(summary) {
    summary = summary || {};
    if (summary.forecast_hour_label != null && summary.forecast_hour_label !== '') {
      return String(summary.forecast_hour_label);
    }
    var fh = summary.forecast_hour;
    if (fh == null || fh === '') fh = summary.forecastHour;
    if (fh == null || fh === '') fh = summary.fhour;
    if (fh == null || fh === '') return '';
    var n = Number(fh);
    if (isFinite(n)) return 'f' + String(Math.round(n)).padStart(3, '0');
    return String(fh);
  }

  function ptOpsWindGridText(summary) {
    summary = summary || {};
    var grid = summary.grid || {};
    var domain = summary.domain || {};
    var bits = [];

    var nx = grid.nx || summary.nx;
    var ny = grid.ny || summary.ny;
    if (nx && ny) bits.push(String(nx) + '×' + String(ny));

    var dx = grid.dx || summary.dx;
    var dy = grid.dy || summary.dy;
    if (dx && dy) bits.push(String(dx) + '°');
    else if (summary.grid_spacing_degrees) bits.push(String(summary.grid_spacing_degrees) + '°');

    if (domain && domain.west != null && domain.east != null && domain.south != null && domain.north != null) {
      var west = Number(domain.west);
      var east = Number(domain.east);
      var south = Number(domain.south);
      var north = Number(domain.north);
      if (isFinite(west) && isFinite(east) && isFinite(south) && isFinite(north) &&
          west <= -170 && east >= -75 && south <= 5 && north >= 70) {
        bits.push('broad North Pacific / North America domain');
      } else {
        bits.push('regional model domain');
      }
    } else if (summary.domain_label) {
      bits.push(String(summary.domain_label));
    }

    return bits.join(' · ');
  }

  function ptOpsWindSpeedMph(summary, key) {
    summary = summary || {};
    var mph = summary.speed_mph || {};
    var ms = summary.speed_ms || {};
    var mphValue = ptOpsWindFirstNumber([
      mph[key],
      summary['speed_mph_' + key]
    ]);
    if (mphValue != null) return mphValue;

    var msValue = ptOpsWindFirstNumber([
      ms[key],
      summary['speed_ms_' + key]
    ]);
    return msValue == null ? null : msValue * 2.2369362920544;
  }

  function ptOpsWindRecommendedMaxVelocity(summary) {
    summary = summary || {};
    var recommended = ptOpsWindFirstNumber([
      summary.recommended_velocity_scale_ms,
      summary.max_velocity_ms
    ]);
    if (recommended != null && recommended > 0) return recommended;

    var speedMs = summary.speed_ms || {};
    var p95 = ptOpsWindFirstNumber([
      speedMs.p95,
      summary.speed_ms_p95
    ]);
    if (p95 != null && p95 > 0) return p95;

    // Last-resort renderer safeguard only. Normal RTW015 manifests provide
    // recommended_velocity_scale_ms and p95, so this should not be reached.
    return 22;
  }

  function ptOpsWindResolveEntryUrl(manifestUrl, relativeUrl) {
    manifestUrl = String(manifestUrl || '');
    relativeUrl = String(relativeUrl || '');
    if (!manifestUrl) throw new Error('Missing wind manifest URL.');
    if (!relativeUrl) throw new Error('Selected wind manifest entry has no relative_url.');

    try {
      return new URL(relativeUrl, manifestUrl).toString();
    } catch(e) {
      var cleanManifest = manifestUrl.split('#')[0].split('?')[0];
      var slash = cleanManifest.lastIndexOf('/');
      if (slash < 0) throw new Error('Could not resolve selected wind URL.');
      return cleanManifest.slice(0, slash + 1) + relativeUrl.replace(/^\/+/, '');
    }
  }

  function ptOpsWindSelectManifestEntry(manifest, targetMs) {
    manifest = manifest || {};
    var entries = Array.isArray(manifest.entries) ? manifest.entries : [];
    targetMs = isFinite(Number(targetMs)) ? Number(targetMs) : Date.now();

    var candidates = entries.map(function(entry, index) {
      entry = entry || {};
      var validDate = ptOpsWindParseDate(entry.valid_time_utc || entry.valid_time || entry.validTimeUtc);
      var relativeUrl = entry.relative_url || entry.url || entry.wind_url;
      if (!validDate || !relativeUrl) return null;
      var deltaMs = validDate.getTime() - targetMs;
      return {
        entry: entry,
        index: index,
        deltaMs: deltaMs,
        absoluteDeltaMs: Math.abs(deltaMs),
        isFuture: deltaMs > 0
      };
    }).filter(function(x) { return x != null; });

    if (!candidates.length) {
      throw new Error('Wind manifest has no usable entries with valid_time_utc and relative_url.');
    }

    candidates.sort(function(a, b) {
      var distanceDifference = a.absoluteDeltaMs - b.absoluteDeltaMs;
      if (Math.abs(distanceDifference) > 1) return distanceDifference;

      // For an equal-distance tie, prefer the recent past over the future.
      if (a.isFuture !== b.isFuture) return a.isFuture ? 1 : -1;

      // Keep selection deterministic if the manifest contains duplicate times.
      var aFh = ptOpsWindNumber(a.entry.forecast_hour);
      var bFh = ptOpsWindNumber(b.entry.forecast_hour);
      if (aFh != null && bFh != null && aFh !== bFh) return aFh - bFh;
      return a.index - b.index;
    });

    return candidates[0].entry;
  }

  function ptOpsWindStatusText(summary) {
    summary = summary || {};
    var valid = ptOpsWindLocalTimeText(summary, 'valid_time_utc', 'valid_time_local');
    var cycle = ptOpsWindCycleText(summary);
    var fh = ptOpsWindForecastHour(summary);
    var parts = [];

    if (valid) parts.push('Valid ' + valid);
    if (cycle || fh) parts.push('GFS run ' + (cycle || 'unknown') + (fh ? ' · ' + fh : ''));

    return parts.length ? parts.join('; ') : 'NOAA GFS wind feed loaded.';
  }

  function ptOpsWindStatusClass(summary) {
    summary = summary || {};
    var d = ptOpsWindParseDate(summary.valid_time_utc || summary.valid_time || summary.validTimeUtc);
    if (!d) return 'pt-ops-warn';
    var ageHours = (Date.now() - d.getTime()) / 36e5;

    if (ageHours <= 6) return 'pt-ops-ok';
    if (ageHours <= 12) return 'pt-ops-warn';
    return 'pt-ops-bad';
  }

  function ptOpsWindStaleNote(summary) {
    summary = summary || {};
    var d = ptOpsWindParseDate(summary.valid_time_utc || summary.valid_time || summary.validTimeUtc);
    if (!d) return 'Valid time not available; verify the source feed if using this layer operationally.';
    var ageHours = (Date.now() - d.getTime()) / 36e5;
    if (ageHours <= 6) return 'Fresh/broad-pattern context.';
    if (ageHours <= 12) return 'Caution: valid time is older than roughly 6 hours.';
    return 'Stale: valid time is older than roughly 12 hours.';
  }

  function ptOpsWindLegendLines(summary) {
    summary = summary || {};
    var lines = ['Particle color scaled to current domain winds.'];
    var p95Mph = ptOpsWindSpeedMph(summary, 'p95');
    var maxMph = ptOpsWindSpeedMph(summary, 'max');
    var speedBits = [];
    if (p95Mph != null) speedBits.push('p95: ' + p95Mph.toFixed(1) + ' mph');
    if (maxMph != null) speedBits.push('domain max: ' + maxMph.toFixed(1) + ' mph');
    if (speedBits.length) lines.push(speedBits.join(' · '));

    var valid = ptOpsWindLocalTimeText(summary, 'valid_time_utc', 'valid_time_local');
    if (valid) lines.push('Valid: ' + valid + ' · hourly model snapshot');

    var cycle = ptOpsWindCycleText(summary);
    var fh = ptOpsWindForecastHour(summary);
    if (cycle || fh) lines.push('GFS run: ' + (cycle || 'unknown') + (fh ? ' · ' + fh : ''));

    return lines;
  }

  function ptOpsWindSetCanvasPointerEventsNone(mapObj) {
    try {
      var pane = mapObj && mapObj.getPanes ? mapObj.getPanes().overlayPane : null;
      if (!pane) return;
      Array.prototype.slice.call(pane.querySelectorAll('canvas')).forEach(function(canvas) {
        if (canvas && canvas.style) canvas.style.pointerEvents = 'none';
      });
    } catch(e) {}
  }

  function ptOpsLoadScriptOnce(id, urls) {
    if (document.getElementById(id)) {
      return Promise.resolve();
    }

    urls = Array.isArray(urls) ? urls.slice() : [String(urls || '')];

    return new Promise(function(resolve, reject) {
      function tryNext() {
        if (!urls.length) {
          reject(new Error('Could not load wind renderer library.'));
          return;
        }

        var url = urls.shift();
        if (!url) {
          tryNext();
          return;
        }

        var script = document.createElement('script');
        script.id = id;
        script.async = true;
        script.onload = function() { resolve(); };
        script.onerror = function() {
          try { script.remove(); } catch(e) {}
          tryNext();
        };
        script.src = url;
        document.head.appendChild(script);
      }

      tryNext();
    });
  }

  function ptOpsEnsureVelocityRenderer() {
    if (L && typeof L.velocityLayer === 'function') return Promise.resolve();

    return ptOpsLoadScriptOnce('pt-leaflet-velocity-js', [
      'https://cdn.jsdelivr.net/npm/leaflet-velocity@2.1.4/dist/leaflet-velocity.min.js',
      'https://unpkg.com/leaflet-velocity@2.1.4/dist/leaflet-velocity.min.js'
    ]).then(function() {
      if (!L || typeof L.velocityLayer !== 'function') {
        throw new Error('Wind renderer loaded, but L.velocityLayer is not available.');
      }
    });
  }

  function ptOpsWindUpdateLegend(name, summary, leadHours, isLoading, overrideNote) {
    if (!activeLegendDefs || !activeLegendDefs[name]) return;
    leadHours = isFinite(Number(leadHours)) ? Number(leadHours) : 0;

    activeLegendDefs[name].metricLines = ptOpsWindLegendLines(summary);
    activeLegendDefs[name].metricText = '';
    activeLegendDefs[name].windSummary = summary || {};
    activeLegendDefs[name].windLeadHours = leadHours;
    activeLegendDefs[name].windLeadLoading = isLoading === true;

    if (overrideNote) {
      activeLegendDefs[name].legendNote = String(overrideNote);
    } else if (leadHours > 0) {
      activeLegendDefs[name].legendNote =
        ptOpsWindLeadLabel(leadHours) +
        ' broad-pattern outlook selected relative to browser time. Model guidance, not observed wind.';
    } else {
      activeLegendDefs[name].legendNote = ptOpsWindStaleNote(summary);
    }
    redrawLegend();
  }

  var PtOpsGfsSurfaceWindLayer = L.Layer.extend({
    initialize: function(options) {
      this.options = options || {};
      this._map = null;
      this._velocityLayer = null;
      this._removed = true;
      this._loadToken = 0;
      this._fieldToken = 0;
      this._summary = null;
      this._manifest = null;
      this._selectedWindUrl = '';
      this._leadHours = 0;
      this._loadedLeadHours = 0;
      this._modelKey = String(this.options.modelKey || 'gfs');
      this._leadOptions = Array.isArray(this.options.leadOptions) ?
        this.options.leadOptions : [
          {hours: 0, label: 'Current'},
          {hours: 24, label: '+24 hr'}
        ];
    },

    onAdd: function(mapObj) {
      this._map = mapObj;
      this._removed = false;
      this._loadToken += 1;
      var token = this._loadToken;
      var self = this;
      var name = this.options.name || 'Wind flow | NOAA GFS surface';
      var manifestUrl = String(this.options.manifestUrl || '');

      ptOpsWindRegisterLeadController(this._modelKey, this);

      activeLegendDefs[name] = {
        note: 'NOAA/NCEP GFS 10-m model wind flow. Model-based situational-awareness overlay; not observed wind.',
        legendType: 'wind_flow_gfs',
        sourceUrl: this.options.sourceUrl || '',
        infoUrl: manifestUrl,
        infoLabel: 'feed manifest',
        metricLines: [],
        legendNote: 'Fetching NOAA GFS wind manifest…',
        windModelKey: this._modelKey,
        windLeadHours: this._leadHours,
        windLeadOptions: this._leadOptions,
        windLeadLoading: true,
        windSummary: {}
      };
      redrawLegend();
      setOpsLayerLoading(name, true);
      recordStatus(name, 'Fetching NOAA GFS wind time-set manifest…', 'pt-ops-warn');

      Promise.all([
        ptOpsEnsureVelocityRenderer(),
        ptOpsWindFetchJson(manifestUrl)
      ]).then(function(results) {
        if (self._removed || token !== self._loadToken) return;
        self._manifest = results[1] || {};
        self._loadSelectedLead();
      }).catch(function(err) {
        if (self._removed || token !== self._loadToken) return;
        console.error(err);
        setOpsLayerLoading(name, false);
        recordStatus(name, 'Wind layer unavailable: ' + (err && err.message ? err.message : err), 'pt-ops-bad');
        if (activeLegendDefs && activeLegendDefs[name]) {
          activeLegendDefs[name].metricLines = ['Wind layer unavailable.'];
          activeLegendDefs[name].windLeadLoading = false;
          activeLegendDefs[name].legendNote =
            'Could not load the GFS time-set manifest or wind renderer. The rest of BRIM is unaffected.';
          redrawLegend();
        }
      });
    },

    setLeadHours: function(hours) {
      hours = Number(hours);
      var allowed = this._leadOptions.some(function(x) {
        return Math.abs(Number(x.hours) - hours) < 0.01;
      });
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
      var name = this.options.name || 'Wind flow | NOAA GFS surface';
      var manifestUrl = String(this.options.manifestUrl || '');
      var requestedLead = this._leadHours;
      var targetMs = Date.now() + requestedLead * 36e5;
      var selected;

      try {
        selected = ptOpsWindSelectManifestEntry(this._manifest, targetMs);
        var distanceHours = ptOpsWindTargetDistanceHours(selected, targetMs);
        var tolerance = Number(this.options.maxTargetErrorHours || 2.1);
        if (distanceHours == null || distanceHours > tolerance) {
          throw new Error(
            'No GFS field is close enough to the requested ' +
            ptOpsWindLeadLabel(requestedLead) + ' target.'
          );
        }
      } catch(err) {
        this._handleFieldError(err, requestedLead);
        return;
      }

      var selectedWindUrl;
      try {
        selectedWindUrl = ptOpsWindResolveEntryUrl(
          manifestUrl,
          selected.relative_url || selected.url || selected.wind_url
        );
      } catch(err) {
        this._handleFieldError(err, requestedLead);
        return;
      }

      this._selectedWindUrl = selectedWindUrl;
      if (activeLegendDefs && activeLegendDefs[name]) {
        activeLegendDefs[name].windLeadHours = requestedLead;
        activeLegendDefs[name].windLeadLoading = true;
        activeLegendDefs[name].legendNote =
          'Fetching ' + ptOpsWindLeadLabel(requestedLead) + ' GFS wind field…';
        redrawLegend();
      }
      setOpsLayerLoading(name, true);
      recordStatus(
        name,
        'Fetching ' + ptOpsWindLeadLabel(requestedLead) + ' GFS wind field: ' +
          ptOpsWindStatusText(selected) + '…',
        'pt-ops-warn'
      );

      ptOpsWindFetchJson(selectedWindUrl).then(function(windData) {
        if (self._removed || fieldToken !== self._fieldToken) return;
        if (!Array.isArray(windData) || windData.length < 2) {
          throw new Error('Selected GFS wind JSON did not contain the expected U/V component records.');
        }

        var maxVelocity = ptOpsWindRecommendedMaxVelocity(selected);
        self._summary = selected;
        self._loadedLeadHours = requestedLead;
        self._addVelocityLayer(windData, maxVelocity);
        ptOpsWindUpdateLegend(name, selected, requestedLead, false);

        var status = ptOpsWindStatusText(selected);
        var context = requestedLead > 0 ?
          ptOpsWindLeadLabel(requestedLead) + ' broad-pattern outlook.' :
          ptOpsWindStaleNote(selected);
        recordStatus(
          name,
          status + '. ' + context + ' Model data, not observed wind.',
          ptOpsWindStatusClass(selected)
        );
        setOpsLayerLoading(name, false);
      }).catch(function(err) {
        if (self._removed || fieldToken !== self._fieldToken) return;
        self._handleFieldError(err, requestedLead);
      });
    },

    _handleFieldError: function(err, requestedLead) {
      var name = this.options.name || 'Wind flow | NOAA GFS surface';
      console.error(err);
      setOpsLayerLoading(name, false);

      var hasPrior = !!this._summary;
      if (hasPrior) this._leadHours = this._loadedLeadHours;
      recordStatus(
        name,
        'Could not load ' + ptOpsWindLeadLabel(requestedLead) + ' GFS field: ' +
          (err && err.message ? err.message : err) +
          (hasPrior ? ' Keeping the previously loaded field.' : ''),
        'pt-ops-bad'
      );

      if (activeLegendDefs && activeLegendDefs[name]) {
        activeLegendDefs[name].windLeadHours = this._leadHours;
        activeLegendDefs[name].windLeadLoading = false;
        activeLegendDefs[name].legendNote =
          'Requested GFS view was unavailable.' +
          (hasPrior ? ' The previously loaded field remains displayed.' : '');
        redrawLegend();
      }
    },

    _addVelocityLayer: function(windData, maxVelocity) {
      this._removeVelocityLayer();

      this._velocityLayer = L.velocityLayer({
        data: windData,
        displayValues: false,
        // velocityScale controls particle motion; maxVelocity controls the
        // color ramp. Keep motion stable while scaling colors from manifest metadata.
        velocityScale: 0.007,
        particleAge: 60,
        particleMultiplier: 1 / 430,
        lineWidth: 1.18,
        opacity: 0.74,
        maxVelocity: maxVelocity,
        colorScale: [
          '#b2e2e2', '#66c2a4', '#2ca25f', '#238b45',
          '#2b8cbe', '#0868ac', '#253494',
          '#fed976', '#feb24c', '#fd8d3c', '#e31a1c'
        ]
      });

      this._velocityLayer.addTo(this._map);
      try { this._velocityLayer.bringToFront(); } catch(e) {}
      var mapObj = this._map;
      window.setTimeout(function() { ptOpsWindSetCanvasPointerEventsNone(mapObj); }, 80);
      window.setTimeout(function() { ptOpsWindSetCanvasPointerEventsNone(mapObj); }, 500);
    },

    _removeVelocityLayer: function() {
      if (this._velocityLayer && this._map) {
        try {
          if (this._velocityLayer._windy && typeof this._velocityLayer._windy.stop === 'function') {
            this._velocityLayer._windy.stop();
          }
        } catch(e) {}
        try { this._map.removeLayer(this._velocityLayer); } catch(e) {}
        try {
          if (typeof this._velocityLayer.remove === 'function') this._velocityLayer.remove();
        } catch(e) {}
      }
      this._velocityLayer = null;
    },

    onRemove: function(mapObj) {
      var name = this.options.name || 'Wind flow | NOAA GFS surface';
      this._removed = true;
      this._loadToken += 1;
      this._fieldToken += 1;
      this._removeVelocityLayer();
      ptOpsWindUnregisterLeadController(this._modelKey, this);
      delete activeLegendDefs[name];
      setOpsLayerLoading(name, false);
      redrawLegend();
      recordStatus(name, 'Layer turned off.', 'pt-ops-muted');
      this._summary = null;
      this._manifest = null;
      this._map = null;
    },

    forceRemove: function(mapObj) {
      this.onRemove(mapObj || this._map);
    }
  });

  addOpsExternalLinks({
    category: 'Atmosphere / Wind',
    subgroup: 'Wind',
    title: 'GFS model links',
    note: 'NOAA/NCEP GFS 10-m U/V model wind.',
    links: [
      {label: 'NCEP GFS', url: 'https://www.nco.ncep.noaa.gov/pmb/products/gfs/', title: 'Open NOAA/NCEP GFS product page'}
    ]
  });

  if (includeGfsSurfaceWind) {
    addOpsLayer({
      category: 'Atmosphere / Wind',
      subgroup: 'Wind',
      name: 'Wind flow | NOAA GFS surface',
      sourceUrl: 'https://www.nco.ncep.noaa.gov/pmb/products/gfs/',
      legendType: 'wind_flow_gfs',
      helperText: 'Animated NOAA GFS 10-m model wind field with Current and +24 hr broad-pattern views; not observed station wind.',
      layer: new PtOpsGfsSurfaceWindLayer({
        name: 'Wind flow | NOAA GFS surface',
        manifestUrl: GFS_SURFACE_WIND_MANIFEST_URL,
        sourceUrl: 'https://www.nco.ncep.noaa.gov/pmb/products/gfs/',
        modelKey: 'gfs',
        leadOptions: [
          {hours: 0, label: 'Current'},
          {hours: 24, label: '+24 hr'}
        ],
        maxTargetErrorHours: 2.1
      })
    });
  }

)---"
}
