# ==== leaflet_ops_live_hrrr_wind_helpers.r ==================================
##
## PURPOSE:
##   Browser-side Ops Live NOAA HRRR 10-m regional wind overlay.
##
## DESIGN:
##   Lazy-loads the hosted rolling HRRR manifest, selects the entry whose UTC
##   valid time is closest to browser time, fetches only that field, and renders
##   it with leaflet-velocity. The feed is regridded by the live-feed workflow
##   with wgrib2 -new_grid_winds earth before publication.
## ============================================================================

pt_ops_live_hrrr_wind_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Ops Live HRRR regional wind/vector-field overlay
  // --------------------------------------------------------------------------

  function ptOpsHrrrWindStatusText(summary) {
    summary = summary || {};
    var valid = ptOpsWindLocalTimeText(summary, 'valid_time_utc', 'valid_time_local');
    var cycle = ptOpsWindCycleText(summary);
    var fh = ptOpsWindForecastHour(summary);
    var parts = [];
    if (valid) parts.push('Valid ' + valid);
    if (cycle || fh) parts.push('HRRR run ' + (cycle || 'unknown') + (fh ? ' · ' + fh : ''));
    return parts.length ? parts.join('; ') : 'NOAA HRRR wind feed loaded.';
  }

  function ptOpsHrrrWindStatusClass(summary) {
    summary = summary || {};
    var d = ptOpsWindParseDate(summary.valid_time_utc || summary.valid_time);
    if (!d) return 'pt-ops-warn';
    var ageHours = (Date.now() - d.getTime()) / 36e5;
    if (ageHours <= 3) return 'pt-ops-ok';
    if (ageHours <= 6) return 'pt-ops-warn';
    return 'pt-ops-bad';
  }

  function ptOpsHrrrWindStaleNote(summary) {
    summary = summary || {};
    var d = ptOpsWindParseDate(summary.valid_time_utc || summary.valid_time);
    if (!d) return 'Valid time not available; verify the feed before relying on this layer.';
    var ageHours = (Date.now() - d.getTime()) / 36e5;
    if (ageHours <= 3) return 'Fresh regional-detail context.';
    if (ageHours <= 6) return 'Caution: valid time is older than roughly 3 hours.';
    return 'Stale: valid time is older than roughly 6 hours.';
  }

  function ptOpsHrrrWindLegendLines(summary) {
    summary = summary || {};
    var lines = ['Particle color scaled to current regional-domain winds.'];
    var p95Mph = ptOpsWindSpeedMph(summary, 'p95');
    var maxMph = ptOpsWindSpeedMph(summary, 'max');
    var bits = [];
    if (p95Mph != null) bits.push('p95: ' + p95Mph.toFixed(1) + ' mph');
    if (maxMph != null) bits.push('domain max: ' + maxMph.toFixed(1) + ' mph');
    if (bits.length) lines.push(bits.join(' · '));

    var valid = ptOpsWindLocalTimeText(summary, 'valid_time_utc', 'valid_time_local');
    if (valid) lines.push('Valid: ' + valid + ' · hourly model snapshot');

    var cycle = ptOpsWindCycleText(summary);
    var fh = ptOpsWindForecastHour(summary);
    if (cycle || fh) lines.push('HRRR run: ' + (cycle || 'unknown') + (fh ? ' · ' + fh : ''));
    return lines;
  }

  function ptOpsHrrrWindUpdateLegend(name, summary, leadHours, isLoading, overrideNote) {
    if (!activeLegendDefs || !activeLegendDefs[name]) return;
    leadHours = isFinite(Number(leadHours)) ? Number(leadHours) : 0;
    activeLegendDefs[name].metricLines = ptOpsHrrrWindLegendLines(summary);
    activeLegendDefs[name].metricText = '';
    activeLegendDefs[name].windSummary = summary || {};
    activeLegendDefs[name].windLeadHours = leadHours;
    activeLegendDefs[name].windLeadLoading = isLoading === true;

    if (overrideNote) {
      activeLegendDefs[name].legendNote = String(overrideNote);
    } else if (leadHours > 0) {
      activeLegendDefs[name].legendNote =
        ptOpsWindLeadLabel(leadHours) +
        ' regional-detail outlook selected relative to browser time. Model guidance, not observed wind.';
    } else {
      activeLegendDefs[name].legendNote = ptOpsHrrrWindStaleNote(summary);
    }
    redrawLegend();
  }

  var PtOpsHrrrSurfaceWindLayer = L.Layer.extend({
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
      this._modelKey = String(this.options.modelKey || 'hrrr');
      this._leadOptions = Array.isArray(this.options.leadOptions) ?
        this.options.leadOptions : [
          {hours: 0, label: 'Current'},
          {hours: 6, label: '+6 hr'},
          {hours: 12, label: '+12 hr'}
        ];
    },

    onAdd: function(mapObj) {
      this._map = mapObj;
      this._removed = false;
      this._loadToken += 1;
      var token = this._loadToken;
      var self = this;
      var name = this.options.name || 'Wind flow | NOAA HRRR surface detail';
      var manifestUrl = String(this.options.manifestUrl || '');

      ptOpsWindRegisterLeadController(this._modelKey, this);

      activeLegendDefs[name] = {
        note: 'NOAA/NCEP HRRR 10-m regional model wind. Higher-resolution situational-awareness overlay; not observed wind.',
        legendType: 'wind_flow_hrrr',
        sourceUrl: this.options.sourceUrl || '',
        infoUrl: manifestUrl,
        infoLabel: 'feed manifest',
        metricLines: [],
        legendNote: 'Fetching NOAA HRRR wind manifest…',
        windModelKey: this._modelKey,
        windLeadHours: this._leadHours,
        windLeadOptions: this._leadOptions,
        windLeadLoading: true,
        windSummary: {}
      };
      redrawLegend();
      setOpsLayerLoading(name, true);
      recordStatus(name, 'Fetching NOAA HRRR rolling time-set manifest…', 'pt-ops-warn');

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
        recordStatus(name, 'HRRR wind layer unavailable: ' + (err && err.message ? err.message : err), 'pt-ops-bad');
        if (activeLegendDefs && activeLegendDefs[name]) {
          activeLegendDefs[name].metricLines = ['HRRR wind layer unavailable.'];
          activeLegendDefs[name].windLeadLoading = false;
          activeLegendDefs[name].legendNote =
            'Could not load the HRRR manifest or wind renderer. The rest of BRIM is unaffected.';
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
      var name = this.options.name || 'Wind flow | NOAA HRRR surface detail';
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
            'No HRRR field is close enough to the requested ' +
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
          'Fetching ' + ptOpsWindLeadLabel(requestedLead) + ' HRRR wind field…';
        redrawLegend();
      }
      setOpsLayerLoading(name, true);
      recordStatus(
        name,
        'Fetching ' + ptOpsWindLeadLabel(requestedLead) + ' HRRR wind field: ' +
          ptOpsHrrrWindStatusText(selected) + '…',
        'pt-ops-warn'
      );

      ptOpsWindFetchJson(selectedWindUrl).then(function(windData) {
        if (self._removed || fieldToken !== self._fieldToken) return;
        if (!Array.isArray(windData) || windData.length < 2) {
          throw new Error('Selected HRRR wind JSON did not contain the expected U/V component records.');
        }

        var maxVelocity = ptOpsWindRecommendedMaxVelocity(selected);
        self._summary = selected;
        self._loadedLeadHours = requestedLead;
        self._addVelocityLayer(windData, maxVelocity);
        ptOpsHrrrWindUpdateLegend(name, selected, requestedLead, false);

        var context = requestedLead > 0 ?
          ptOpsWindLeadLabel(requestedLead) + ' regional-detail outlook.' :
          ptOpsHrrrWindStaleNote(selected);
        recordStatus(
          name,
          ptOpsHrrrWindStatusText(selected) + '. ' + context +
            ' Model data, not observed wind.',
          ptOpsHrrrWindStatusClass(selected)
        );
        setOpsLayerLoading(name, false);
      }).catch(function(err) {
        if (self._removed || fieldToken !== self._fieldToken) return;
        self._handleFieldError(err, requestedLead);
      });
    },

    _handleFieldError: function(err, requestedLead) {
      var name = this.options.name || 'Wind flow | NOAA HRRR surface detail';
      console.error(err);
      setOpsLayerLoading(name, false);

      var hasPrior = !!this._summary;
      if (hasPrior) this._leadHours = this._loadedLeadHours;
      recordStatus(
        name,
        'Could not load ' + ptOpsWindLeadLabel(requestedLead) + ' HRRR field: ' +
          (err && err.message ? err.message : err) +
          (hasPrior ? ' Keeping the previously loaded field.' : ''),
        'pt-ops-bad'
      );

      if (activeLegendDefs && activeLegendDefs[name]) {
        activeLegendDefs[name].windLeadHours = this._leadHours;
        activeLegendDefs[name].windLeadLoading = false;
        activeLegendDefs[name].legendNote =
          'Requested HRRR view was unavailable.' +
          (hasPrior ? ' The previously loaded field remains displayed.' : '');
        redrawLegend();
      }
    },

    _addVelocityLayer: function(windData, maxVelocity) {
      this._removeVelocityLayer();
      this._velocityLayer = L.velocityLayer({
        data: windData,
        displayValues: false,
        velocityScale: 0.0065,
        particleAge: 75,
        particleMultiplier: 1 / 260,
        lineWidth: 1.15,
        opacity: 0.76,
        maxVelocity: maxVelocity,
        colorScale: [
          '#7bb6ff', '#b7dcff', '#d8f5e0', '#fff0a6',
          '#ffc16e', '#ff7d62', '#d83b62'
        ]
      });
      this._velocityLayer.addTo(this._map);
      try { this._velocityLayer.bringToFront(); } catch(e) {}
      var mapObj = this._map;
      window.setTimeout(function() { ptOpsWindSetCanvasPointerEventsNone(mapObj); }, 80);
      window.setTimeout(function() { ptOpsWindSetCanvasPointerEventsNone(mapObj); }, 350);
    },

    _removeVelocityLayer: function() {
      if (!this._velocityLayer) return;
      try {
        if (this._velocityLayer._windy && typeof this._velocityLayer._windy.stop === 'function') {
          this._velocityLayer._windy.stop();
        }
      } catch(e) {}
      try {
        if (this._map && this._map.hasLayer(this._velocityLayer)) this._map.removeLayer(this._velocityLayer);
      } catch(e) {}
      try {
        if (typeof this._velocityLayer.remove === 'function') this._velocityLayer.remove();
      } catch(e) {}
      this._velocityLayer = null;
    },

    onRemove: function(mapObj) {
      var name = this.options.name || 'Wind flow | NOAA HRRR surface detail';
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
      this._selectedWindUrl = '';
      this._map = null;
    },

    forceRemove: function(mapObj) {
      this.onRemove(mapObj || this._map);
    }
  });

  addOpsExternalLinks({
    category: 'Atmosphere / Wind',
    subgroup: 'Wind',
    title: 'HRRR wind links',
    note: 'Higher-resolution NOAA regional model wind over Hydrologic California and adjacent basins.',
    links: [
      {label: 'NOAA HRRR', url: 'https://rapidrefresh.noaa.gov/hrrr/', title: 'Open NOAA HRRR information'},
      {label: 'NCEP HRRR products', url: 'https://www.nco.ncep.noaa.gov/pmb/products/hrrr/', title: 'Open NCEP HRRR products'}
    ]
  });

  if (includeHrrrSurfaceWind && HRRR_SURFACE_WIND_MANIFEST_URL) {
    addOpsLayer({
      category: 'Atmosphere / Wind',
      subgroup: 'Wind',
      name: 'Wind flow | NOAA HRRR surface detail',
      sourceUrl: 'https://rapidrefresh.noaa.gov/hrrr/',
      infoUrl: HRRR_SURFACE_WIND_MANIFEST_URL,
      infoLabel: 'feed manifest',
      legendType: 'wind_flow_hrrr',
      helperText: 'Higher-resolution NOAA HRRR 10-m model wind for Hydrologic California + adjacent basins with Current, +6 hr, and +12 hr views; not observed station wind.',
      layer: new PtOpsHrrrSurfaceWindLayer({
        name: 'Wind flow | NOAA HRRR surface detail',
        manifestUrl: HRRR_SURFACE_WIND_MANIFEST_URL,
        sourceUrl: 'https://rapidrefresh.noaa.gov/hrrr/',
        modelKey: 'hrrr',
        leadOptions: [
          {hours: 0, label: 'Current'},
          {hours: 6, label: '+6 hr'},
          {hours: 12, label: '+12 hr'}
        ],
        maxTargetErrorHours: 2.1
      })
    });
  }

)---"
}
