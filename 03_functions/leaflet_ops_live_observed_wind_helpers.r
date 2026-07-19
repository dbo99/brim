# ==== leaflet_ops_live_observed_wind_helpers.r ==============================
##
## PURPOSE:
##   Browser-side Ops Live observed-wind layer using the hosted RTW020
##   METAR/ASOS GeoJSON and summary feed.
##
## DESIGN:
##   - lazy-loads only when the Ops Live row is enabled;
##   - renders standard meteorological wind barbs from sustained speed in knots
##     and wind-from direction;
##   - keeps gusts separate in labels/popups rather than encoding gust speed in
##     the barb itself;
##   - offers optional compact station/speed/gust labels at zoom 7+;
##   - computes observation ages against browser time so stale feeds remain
##     obvious even if the hosted summary has stopped updating.
## ============================================================================

pt_ops_live_observed_wind_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Ops Live observed wind: NOAA/NWS METAR/ASOS speed + gusts
  // --------------------------------------------------------------------------

  var PT_ASOS_WIND_LAYER_NAME = 'Observed wind | METAR/ASOS speed + gusts';
  if (typeof window.ptAsosWindLabelsVisible !== 'boolean') {
    window.ptAsosWindLabelsVisible = false;
  }

  function ptAsosWindEnsureStyle() {
    if (document.getElementById('pt-asos-wind-style')) return;
    var style = document.createElement('style');
    style.id = 'pt-asos-wind-style';
    style.textContent = `
      .pt-asos-wind-barb-icon,
      .pt-asos-wind-label-icon {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
      }
      .pt-asos-wind-barb-icon svg {
        display: block;
        overflow: visible;
      }
      .pt-asos-wind-label-text {
        position: absolute;
        left: 0;
        top: -22px;
        transform: translate(-50%, -100%);
        white-space: nowrap;
        pointer-events: none;
        font: 700 10px/1.05 Arial, Helvetica, sans-serif;
        color: #111;
        background: rgba(255,255,255,0.83);
        border: 1px solid rgba(70,70,70,0.50);
        border-radius: 2px;
        padding: 1px 3px;
        box-shadow: 0 1px 2px rgba(0,0,0,0.25);
      }
      .pt-asos-wind-tooltip {
        font: 11px/1.25 Arial, Helvetica, sans-serif;
      }
      .pt-asos-wind-popup-title {
        font-size: 14px;
        font-weight: 800;
        margin-bottom: 1px;
      }
      .pt-asos-wind-popup-subtitle {
        color: #666;
        font-size: 11px;
        line-height: 1.2;
        margin-bottom: 6px;
      }
      .pt-asos-wind-popup-grid {
        display: grid;
        grid-template-columns: max-content 1fr;
        column-gap: 8px;
        row-gap: 3px;
        font-size: 12px;
        line-height: 1.25;
      }
      .pt-asos-wind-popup-key {
        color: #666;
        font-weight: 700;
      }
      .pt-asos-wind-popup-links {
        margin-top: 7px;
        padding-top: 6px;
        border-top: 1px solid #ddd;
        font-size: 11px;
        line-height: 1.3;
      }
      .pt-asos-wind-popup-links a {
        white-space: nowrap;
      }
      .pt-asos-wind-popup-raw {
        margin-top: 7px;
        padding-top: 6px;
        border-top: 1px solid #ddd;
        color: #555;
        font: 10px/1.25 Menlo, Consolas, monospace;
        overflow-wrap: anywhere;
      }
    `;
    document.head.appendChild(style);
  }

  function ptAsosWindEscape(value) {
    return String(value == null ? '' : value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }

  function ptAsosWindNumber(value) {
    if (value == null || value === '') return null;
    var n = Number(value);
    return isFinite(n) ? n : null;
  }

  function ptAsosWindAgeMinutes(p) {
    p = p || {};
    var d = ptOpsWindParseDate(p.observation_time_utc);
    if (d) {
      return Math.max(0, (Date.now() - d.getTime()) / 60000);
    }
    return ptAsosWindNumber(p.age_minutes);
  }

  function ptAsosWindAgeDisplay(minutes) {
    var n = ptAsosWindNumber(minutes);
    if (n == null) return 'age unavailable';
    n = Math.max(0, Math.round(n));
    if (n < 60) return n + ' min old';
    var h = Math.floor(n / 60);
    var m = n % 60;
    if (!m) return h + ' hr old';
    return h + ' hr ' + m + ' min old';
  }

  function ptAsosWindDirectionText(p) {
    p = p || {};
    var deg = ptAsosWindNumber(p.wind_from_degrees);
    if (deg == null) deg = ptAsosWindNumber(p.wind_dir_degrees);
    var card = p.wind_dir_cardinal ? String(p.wind_dir_cardinal) : '';
    if (deg == null) return card || 'variable / not reported';
    var rounded = Math.round(((deg % 360) + 360) % 360);
    return rounded + '°' + (card ? ' ' + card : '') + ' (from)';
  }

  function ptAsosWindGustText(p) {
    p = p || {};
    var gust = ptAsosWindNumber(p.wind_gust_mph);
    if (p.has_gust === true && gust != null) return Math.round(gust) + ' mph';
    return 'not reported';
  }

  function ptAsosWindSafeHttpsUrl(value, fallback) {
    var url = value == null ? '' : String(value).trim();
    if (!/^https:\/\//i.test(url)) url = fallback || '';
    return /^https:\/\//i.test(url) ? url : '';
  }

  function ptAsosWindStationLocationText(p) {
    p = p || {};
    var bits = [];
    if (p.station_state) bits.push(String(p.station_state));
    if (p.station_country && String(p.station_country).toUpperCase() !== 'US') bits.push(String(p.station_country));
    return bits.join(', ');
  }

  function ptAsosWindLabelStationId(value) {
    var station = String(value || 'station').trim();
    // Common U.S. airport/METAR labels are colloquially shown with the
    // three-letter identifier (SAC rather than ICAO KSAC). Keep the full
    // station_id in hover and popup content for source fidelity.
    if (/^K[A-Z0-9]{3}$/i.test(station)) return station.slice(1);
    return station;
  }

  function ptAsosWindLabelText(p) {
    p = p || {};
    var feedLabel = p.label_short == null ? '' : String(p.label_short).trim();
    if (feedLabel) {
      // RTW019 publishes labels such as "KSAC 12G20". Shorten only the
      // leading station token; do not alter a K elsewhere in the label.
      return feedLabel.replace(/^K([A-Z0-9]{3})(\s|$)/i, '$1$2');
    }
    var station = ptAsosWindLabelStationId(p.station_id || 'station');
    var speed = ptAsosWindNumber(p.wind_speed_mph);
    var gust = ptAsosWindNumber(p.wind_gust_mph);
    if (speed == null) return station;
    if (p.has_gust === true && gust != null) return station + ' ' + Math.round(speed) + 'G' + Math.round(gust);
    return station + ' ' + Math.round(speed) + ' G–';
  }

  function ptAsosWindTooltipHtml(p) {
    p = p || {};
    var stationId = String(p.station_id || 'METAR station');
    var stationName = p.station_name == null ? '' : String(p.station_name).trim();
    var stationLine = '<b>' + ptAsosWindEscape(stationId) + '</b>';
    if (stationName && stationName.toUpperCase() !== stationId.toUpperCase()) {
      stationLine += ' · ' + ptAsosWindEscape(stationName);
    }
    var speed = ptAsosWindNumber(p.wind_speed_mph);
    var speedText = speed == null ? 'speed unavailable' : Math.round(speed) + ' mph';
    var gustText = ptAsosWindGustText(p);
    return stationLine + '<br>' +
      ptAsosWindEscape(speedText) + ' sustained · gust ' + ptAsosWindEscape(gustText) + '<br>' +
      ptAsosWindEscape(ptAsosWindDirectionText(p)) + ' · ' +
      ptAsosWindEscape(ptAsosWindAgeDisplay(ptAsosWindAgeMinutes(p)));
  }

  function ptAsosWindPopupHtml(p) {
    p = p || {};
    var stationIdRaw = String(p.station_id || 'METAR station').trim();
    var stationNameRaw = p.station_name == null ? '' : String(p.station_name).trim();
    var locationRaw = ptAsosWindStationLocationText(p);
    var title = stationNameRaw || stationIdRaw;
    var subtitleBits = [];
    if (stationNameRaw) subtitleBits.push(stationIdRaw);
    if (locationRaw) subtitleBits.push(locationRaw);

    var stationEncoded = encodeURIComponent(stationIdRaw);
    var nwsFallback = 'https://www.weather.gov/wrh/timeseries?site=' + stationEncoded;
    var awcFallback = 'https://aviationweather.gov/data/metar/?decoded=yes&hours=6&id=' + stationEncoded;
    var nwsUrl = ptAsosWindSafeHttpsUrl(p.nws_station_url, nwsFallback);
    var awcUrl = ptAsosWindSafeHttpsUrl(p.awc_metar_url, awcFallback);

    var speed = ptAsosWindNumber(p.wind_speed_mph);
    var speedText = speed == null ? 'not available' : speed.toFixed(1).replace(/\.0$/, '') + ' mph';
    var observed = ptOpsWindFormatPacific(p.observation_time_utc) || String(p.observation_time_local || 'not available');
    var raw = p.raw_text == null ? '' : String(p.raw_text);

    var html = '<div class="pt-asos-wind-popup-title">' + ptAsosWindEscape(title) + '</div>';
    if (subtitleBits.length) {
      html += '<div class="pt-asos-wind-popup-subtitle">' + ptAsosWindEscape(subtitleBits.join(' · ')) + '</div>';
    }
    html += '<div class="pt-asos-wind-popup-grid">' +
        '<div class="pt-asos-wind-popup-key">Wind</div><div>' + ptAsosWindEscape(speedText) + ' sustained</div>' +
        '<div class="pt-asos-wind-popup-key">Gust</div><div>' + ptAsosWindEscape(ptAsosWindGustText(p)) + '</div>' +
        '<div class="pt-asos-wind-popup-key">Direction</div><div>' + ptAsosWindEscape(ptAsosWindDirectionText(p)) + '</div>' +
        '<div class="pt-asos-wind-popup-key">Observed</div><div>' + ptAsosWindEscape(observed) + '</div>' +
        '<div class="pt-asos-wind-popup-key">Age</div><div>' + ptAsosWindEscape(ptAsosWindAgeDisplay(ptAsosWindAgeMinutes(p))) + '</div>' +
      '</div>';

    if (nwsUrl || awcUrl) {
      var linkBits = [];
      if (nwsUrl) linkBits.push('<a href="' + ptAsosWindEscape(nwsUrl) + '" target="_blank" rel="noopener noreferrer">NWS station observations</a>');
      if (awcUrl) linkBits.push('<a href="' + ptAsosWindEscape(awcUrl) + '" target="_blank" rel="noopener noreferrer">AWC decoded METAR</a>');
      html += '<div class="pt-asos-wind-popup-links">' + linkBits.join(' · ') + '</div>';
    }

    if (raw) {
      html += '<div class="pt-asos-wind-popup-raw"><b>Raw METAR</b><br>' + ptAsosWindEscape(raw) + '</div>';
    }

    html += '<div style="margin-top:6px;font-size:10px;color:#666;">NOAA/NWS Aviation Weather Center METAR cache. Gust not reported is not treated as zero.</div>';
    return html;
  }

  function ptAsosWindBarbSvg(p) {
    p = p || {};
    var speed = ptAsosWindNumber(p.wind_barb_speed_kt);
    if (speed == null) speed = ptAsosWindNumber(p.wind_speed_kt);
    speed = speed == null ? 0 : Math.max(0, speed);

    var direction = ptAsosWindNumber(p.wind_from_degrees);
    if (direction == null) direction = ptAsosWindNumber(p.wind_dir_degrees);
    var age = ptAsosWindAgeMinutes(p);
    var staleRing = age != null && age > 120
      ? '<circle cx="24" cy="24" r="5.6" fill="rgba(255,255,255,0.65)" stroke="#d95f0e" stroke-width="2.2"/>'
      : '';

    if (speed < 2.5 || p.calm === true) {
      return '<svg viewBox="0 0 48 48" width="48" height="48" aria-hidden="true">' +
        '<circle cx="24" cy="24" r="6" fill="rgba(255,255,255,0.86)" stroke="#fff" stroke-width="5"/>' +
        '<circle cx="24" cy="24" r="6" fill="none" stroke="#111" stroke-width="2"/>' +
        staleRing + '</svg>';
    }

    if (direction == null) {
      return '<svg viewBox="0 0 48 48" width="48" height="48" aria-hidden="true">' +
        '<circle cx="24" cy="24" r="7" fill="rgba(255,255,255,0.88)" stroke="#fff" stroke-width="5"/>' +
        '<circle cx="24" cy="24" r="7" fill="none" stroke="#111" stroke-width="2"/>' +
        '<text x="24" y="27" text-anchor="middle" font-family="Arial" font-size="8" font-weight="700" fill="#111">VRB</text>' +
        staleRing + '</svg>';
    }

    direction = ((direction % 360) + 360) % 360;
    var rounded = Math.round(speed / 5) * 5;
    var flags = Math.floor(rounded / 50);
    var remainder = rounded - flags * 50;
    var full = Math.floor(remainder / 10);
    var half = (remainder - full * 10) >= 5 ? 1 : 0;
    var y = 6;
    var lines = [{x1:24, y1:24, x2:24, y2:4}];
    var polygons = [];

    for (var i = 0; i < flags; i += 1) {
      polygons.push('24,' + y + ' 35,' + (y + 4) + ' 24,' + (y + 8));
      y += 8;
    }
    for (var j = 0; j < full; j += 1) {
      lines.push({x1:24, y1:y, x2:35, y2:y + 5});
      y += 4;
    }
    if (half) {
      lines.push({x1:24, y1:y, x2:30, y2:y + 3});
    }

    function lineHtml(stroke, width) {
      return lines.map(function(line) {
        return '<line x1="' + line.x1 + '" y1="' + line.y1 + '" x2="' + line.x2 + '" y2="' + line.y2 + '" stroke="' + stroke + '" stroke-width="' + width + '" stroke-linecap="round" stroke-linejoin="round"/>';
      }).join('');
    }
    function polygonHtml(fill, stroke, width) {
      return polygons.map(function(points) {
        return '<polygon points="' + points + '" fill="' + fill + '" stroke="' + stroke + '" stroke-width="' + width + '" stroke-linejoin="round"/>';
      }).join('');
    }

    return '<svg viewBox="0 0 48 48" width="48" height="48" aria-hidden="true">' +
      '<g transform="rotate(' + direction.toFixed(1) + ' 24 24)">' +
        lineHtml('#fff', 5) + polygonHtml('#fff', '#fff', 4) +
        lineHtml('#111', 2) + polygonHtml('#111', '#111', 1.2) +
      '</g>' + staleRing + '</svg>';
  }

  function ptAsosWindBarbIcon(p) {
    return L.divIcon({
      className: 'pt-asos-wind-barb-icon',
      html: ptAsosWindBarbSvg(p),
      iconSize: [48, 48],
      iconAnchor: [24, 24],
      // Keep hover and popup close to the represented station. Earlier
      // versions shifted both upward, and the tooltip also received a second
      // negative offset when it was bound below.
      popupAnchor: [0, 6],
      tooltipAnchor: [0, -4]
    });
  }

  function ptAsosWindLabelIcon(p) {
    return L.divIcon({
      className: 'pt-asos-wind-label-icon',
      html: '<span class="pt-asos-wind-label-text">' + ptAsosWindEscape(ptAsosWindLabelText(p)) + '</span>',
      iconSize: [1, 1],
      iconAnchor: [0, 0]
    });
  }

  function ptAsosWindStats(features) {
    features = Array.isArray(features) ? features : [];
    var ages = [];
    var gustCount = 0;
    var maxSpeed = null;
    var maxGust = null;
    var newestTime = null;

    features.forEach(function(feature) {
      var p = feature && feature.properties ? feature.properties : {};
      var age = ptAsosWindAgeMinutes(p);
      if (age != null) ages.push(age);
      if (p.has_gust === true && ptAsosWindNumber(p.wind_gust_mph) != null) gustCount += 1;
      var speed = ptAsosWindNumber(p.wind_speed_mph);
      var gust = ptAsosWindNumber(p.wind_gust_mph);
      if (speed != null && (maxSpeed == null || speed > maxSpeed)) maxSpeed = speed;
      if (gust != null && (maxGust == null || gust > maxGust)) maxGust = gust;
      var d = ptOpsWindParseDate(p.observation_time_utc);
      if (d && (!newestTime || d.getTime() > newestTime.getTime())) newestTime = d;
    });

    ages.sort(function(a, b) { return a - b; });
    var median = null;
    if (ages.length) {
      var mid = Math.floor(ages.length / 2);
      median = ages.length % 2 ? ages[mid] : (ages[mid - 1] + ages[mid]) / 2;
    }

    return {
      total: features.length,
      gustCount: gustCount,
      newestAge: ages.length ? ages[0] : null,
      medianAge: median,
      oldestAge: ages.length ? ages[ages.length - 1] : null,
      newestTime: newestTime,
      maxSpeed: maxSpeed,
      maxGust: maxGust
    };
  }

  function ptAsosWindLegendLines(stats) {
    stats = stats || {};
    var lines = [];
    var total = Number(stats.total || 0);
    var gusts = Number(stats.gustCount || 0);
    lines.push(total.toLocaleString() + ' stations · ' + gusts.toLocaleString() + ' reporting gusts');

    if (stats.newestTime) {
      var newest = ptOpsWindFormatPacific(stats.newestTime);
      lines.push('Newest: ' + newest + ' · ' + ptAsosWindAgeDisplay(stats.newestAge));
    }
    if (stats.medianAge != null || stats.oldestAge != null) {
      var ageBits = [];
      if (stats.medianAge != null) ageBits.push('median age ' + Math.round(stats.medianAge) + ' min');
      if (stats.oldestAge != null) ageBits.push('oldest ' + Math.round(stats.oldestAge) + ' min');
      lines.push(ageBits.join(' · '));
    }
    if (stats.maxSpeed != null || stats.maxGust != null) {
      var speedBits = [];
      if (stats.maxSpeed != null) speedBits.push('max sustained ' + Math.round(stats.maxSpeed) + ' mph');
      if (stats.maxGust != null) speedBits.push('max gust ' + Math.round(stats.maxGust) + ' mph');
      lines.push(speedBits.join(' · '));
    }
    return lines;
  }

  function ptAsosWindStatusClass(stats) {
    var age = stats && stats.newestAge != null ? Number(stats.newestAge) : NaN;
    if (!isFinite(age)) return 'pt-ops-warn';
    if (age <= 120) return 'pt-ops-ok';
    if (age <= 180) return 'pt-ops-warn';
    return 'pt-ops-bad';
  }

  function ptAsosWindLegendNote(stats) {
    var age = stats && stats.newestAge != null ? Number(stats.newestAge) : NaN;
    if (!isFinite(age)) return 'Observation freshness could not be calculated; verify timestamps in station popups.';
    if (age <= 120) return 'Recent station observations. Gust not reported is distinct from gust = 0.';
    if (age <= 180) return 'Caution: the newest station observation is more than 2 hours old.';
    return 'Stale feed: the newest station observation is more than 3 hours old.';
  }

  function ptAsosWindUpdateLegend(name, stats) {
    if (!activeLegendDefs || !activeLegendDefs[name]) return;
    activeLegendDefs[name].metricLines = ptAsosWindLegendLines(stats);
    activeLegendDefs[name].metricText = '';
    activeLegendDefs[name].legendNote = ptAsosWindLegendNote(stats);
    activeLegendDefs[name].observedWindStats = stats || {};
    redrawLegend();
  }

  function ptAsosWindWireLabelToggle() {
    if (window.ptAsosWindLabelEventsWired) return;
    window.ptAsosWindLabelEventsWired = true;
    document.addEventListener('change', function(e) {
      var toggle = e.target && e.target.closest ? e.target.closest('[data-pt-ops-action="asos-wind-labels"]') : null;
      if (!toggle) return;
      window.ptAsosWindLabelsVisible = !!toggle.checked;
      if (window.ptAsosWindActiveLayer && typeof window.ptAsosWindActiveLayer.syncLabels === 'function') {
        window.ptAsosWindActiveLayer.syncLabels();
      }
    });
  }

  function ptAsosWindResetLabelToggle() {
    window.ptAsosWindLabelsVisible = false;
    Array.prototype.forEach.call(
      document.querySelectorAll('[data-pt-ops-action="asos-wind-labels"]'),
      function(toggle) { toggle.checked = false; }
    );
  }

  ptAsosWindWireLabelToggle();

  var PtOpsAsosAwosWindLayer = L.Layer.extend({
    initialize: function(options) {
      this.options = options || {};
      this._map = null;
      this._barbGroup = null;
      this._labelGroup = null;
      this._removed = true;
      this._loadToken = 0;
      this._zoomHandler = null;
      this._features = [];
    },

    onAdd: function(mapObj) {
      this._map = mapObj;
      this._removed = false;
      ptAsosWindEnsureStyle();
      window.ptAsosWindActiveLayer = this;

      var name = this.options.name || PT_ASOS_WIND_LAYER_NAME;
      activeLegendDefs[name] = {
        note: 'Recent airport/surface observations from the NOAA/NWS Aviation Weather Center METAR cache. Wind barbs use sustained speed; gusts remain separate label/popup values.',
        legendType: 'observed_wind_metar',
        sourceUrl: this.options.sourceUrl || '',
        infoUrl: this.options.manifestUrl || this.options.summaryUrl || '',
        infoLabel: this.options.manifestUrl ? 'feed manifest' : 'summary',
        metricLines: [],
        legendNote: 'Fetching observed-wind station feed…'
      };
      redrawLegend();

      var self = this;
      this._zoomHandler = function() { self.syncLabels(); };
      mapObj.on('zoomend', this._zoomHandler);
      this._load(false);
    },

    _load: function(isRefresh) {
      var self = this;
      var name = this.options.name || PT_ASOS_WIND_LAYER_NAME;
      this._loadToken += 1;
      var token = this._loadToken;
      setOpsLayerLoading(name, true);
      recordStatus(name, isRefresh ? 'Refreshing METAR/ASOS observed wind…' : 'Fetching METAR/ASOS observed wind…', 'pt-ops-warn');

      var summaryPromise = this.options.summaryUrl
        ? ptOpsWindFetchJson(this.options.summaryUrl).catch(function(err) {
            console.warn('Observed-wind summary unavailable; continuing with GeoJSON.', err);
            return {};
          })
        : Promise.resolve({});

      Promise.all([
        ptOpsWindFetchJson(this.options.url),
        summaryPromise
      ]).then(function(results) {
        if (self._removed || token !== self._loadToken) return;
        var geojson = results[0] || {};
        var summary = results[1] || {};
        var features = Array.isArray(geojson.features) ? geojson.features : [];
        if (geojson.type !== 'FeatureCollection' || !features.length) {
          throw new Error('Observed-wind GeoJSON did not contain station features.');
        }

        var rendered = self._renderFeatures(features);
        self._features = features;
        var stats = ptAsosWindStats(features);
        stats.rendered = rendered;
        stats.domainLabel = summary.domain_label || geojson.domain_label || '';
        ptAsosWindUpdateLegend(name, stats);

        var status = 'Showing ' + rendered.toLocaleString() + ' METAR/ASOS wind stations';
        if (stats.gustCount != null) status += '; ' + Number(stats.gustCount).toLocaleString() + ' report gusts';
        if (stats.newestAge != null) status += '. Newest observation ' + ptAsosWindAgeDisplay(stats.newestAge);
        recordStatus(name, status + '.', ptAsosWindStatusClass(stats));
        setOpsLayerLoading(name, false);
      }).catch(function(err) {
        if (self._removed || token !== self._loadToken) return;
        console.error(err);
        setOpsLayerLoading(name, false);
        recordStatus(name, 'Observed-wind layer unavailable: ' + (err && err.message ? err.message : err), 'pt-ops-bad');
        if (activeLegendDefs && activeLegendDefs[name]) {
          activeLegendDefs[name].metricLines = ['Observed-wind layer unavailable.'];
          activeLegendDefs[name].legendNote = 'Could not load the hosted METAR/ASOS GeoJSON. The rest of BRIM is unaffected.';
          redrawLegend();
        }
      });
    },

    _renderFeatures: function(features) {
      var self = this;
      var newBarbGroup = L.layerGroup();
      var newLabelGroup = L.layerGroup();
      var rendered = 0;

      features.forEach(function(feature) {
        var geometry = feature && feature.geometry ? feature.geometry : {};
        var coords = Array.isArray(geometry.coordinates) ? geometry.coordinates : [];
        var lon = ptAsosWindNumber(coords[0]);
        var lat = ptAsosWindNumber(coords[1]);
        if (geometry.type !== 'Point' || lon == null || lat == null || Math.abs(lat) > 90 || Math.abs(lon) > 180) return;

        var p = feature.properties || {};
        var latlng = [lat, lon];
        var marker = L.marker(latlng, {
          icon: ptAsosWindBarbIcon(p),
          pane: 'pane_ops',
          keyboard: false,
          riseOnHover: true,
          // Do not set marker.title here. Leaflet writes it to the icon as a
          // native browser tooltip, which can appear after the richer Leaflet
          // tooltip and create a delayed duplicate station-ID box.
          alt: String(p.station_id || 'METAR wind station') + ' wind barb'
        });

        var popupIsOpen = false;

        marker.bindTooltip(ptAsosWindTooltipHtml(p), {
          direction: 'top',
          sticky: false,
          offset: [0, 0],
          opacity: 0.96,
          className: 'pt-asos-wind-tooltip'
        });

        marker.on('click', function() {
          try { marker.closeTooltip(); } catch(e) {}
        });
        marker.on('popupopen', function() {
          popupIsOpen = true;
          try { marker.closeTooltip(); } catch(e) {}
        });
        marker.on('popupclose', function() {
          popupIsOpen = false;
        });
        marker.on('tooltipopen', function() {
          var leafletPopupOpen =
            typeof marker.isPopupOpen === 'function' &&
            marker.isPopupOpen();
          if (popupIsOpen || leafletPopupOpen) {
            try { marker.closeTooltip(); } catch(e) {}
          }
        });
        marker.on('mouseout', function() {
          try { marker.closeTooltip(); } catch(e) {}
        });

        marker.bindPopup(ptAsosWindPopupHtml(p), {
          maxWidth: 430,
          maxHeight: 520,
          autoPan: true,
          keepInView: true
        });
        newBarbGroup.addLayer(marker);

        var labelMarker = L.marker(latlng, {
          icon: ptAsosWindLabelIcon(p),
          pane: 'tooltipPane',
          keyboard: false,
          interactive: false,
          zIndexOffset: 1000
        });
        newLabelGroup.addLayer(labelMarker);
        rendered += 1;
      });

      if (this._barbGroup && this._map) {
        try { this._map.removeLayer(this._barbGroup); } catch(e) {}
      }
      if (this._labelGroup && this._map) {
        try { this._map.removeLayer(this._labelGroup); } catch(e) {}
      }

      this._barbGroup = newBarbGroup;
      this._labelGroup = newLabelGroup;
      newBarbGroup.addTo(this._map);
      this.syncLabels();
      return rendered;
    },

    syncLabels: function() {
      if (!this._map || !this._labelGroup) return;
      var minZoom = ptAsosWindNumber(this.options.labelMinZoom);
      if (minZoom == null) minZoom = 7;
      var show = window.ptAsosWindLabelsVisible === true && this._map.getZoom() >= minZoom;
      var showing = this._map.hasLayer(this._labelGroup);
      if (show && !showing) {
        this._labelGroup.addTo(this._map);
      } else if (!show && showing) {
        try { this._map.removeLayer(this._labelGroup); } catch(e) {}
      }
    },

    refreshCurrentView: function() {
      if (this._removed || !this._map) return;
      this._load(true);
    },

    onRemove: function(mapObj) {
      var name = this.options.name || PT_ASOS_WIND_LAYER_NAME;
      this._removed = true;
      ptAsosWindResetLabelToggle();
      this._loadToken += 1;
      if (this._zoomHandler && mapObj) {
        try { mapObj.off('zoomend', this._zoomHandler); } catch(e) {}
      }
      this._zoomHandler = null;
      if (this._barbGroup && mapObj) {
        try { mapObj.removeLayer(this._barbGroup); } catch(e) {}
      }
      if (this._labelGroup && mapObj) {
        try { mapObj.removeLayer(this._labelGroup); } catch(e) {}
      }
      this._barbGroup = null;
      this._labelGroup = null;
      this._features = [];
      if (window.ptAsosWindActiveLayer === this) window.ptAsosWindActiveLayer = null;
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
    title: 'Observed wind links',
    note: 'Current airport/surface observations in the NOAA/NWS Aviation Weather Center METAR stream.',
    links: [
      {label: 'AWC METAR data', url: 'https://aviationweather.gov/data/metar/', title: 'Open NOAA/NWS Aviation Weather Center METAR data'}
    ]
  });

  if (includeAsosAwosWind && ASOS_AWOS_WIND_URL) {
    addOpsLayer({
      category: 'Atmosphere / Wind',
      subgroup: 'Wind',
      name: PT_ASOS_WIND_LAYER_NAME,
      sourceUrl: 'https://aviationweather.gov/data/metar/',
      infoUrl: ASOS_AWOS_WIND_MANIFEST_URL || ASOS_AWOS_WIND_SUMMARY_URL || ASOS_AWOS_WIND_URL,
      infoLabel: ASOS_AWOS_WIND_MANIFEST_URL ? 'feed manifest' : 'summary',
      refreshable: true,
      legendType: 'observed_wind_metar',
      extraRowHtml: '<label class="pt-ops-row-mini-toggle" style="display:inline-flex;align-items:center;gap:1px;margin-left:5px;font-size:10px;line-height:1;color:#555;white-space:nowrap;" title="Show station speed/gust labels at zoom 7 and closer"><input type="checkbox" data-pt-ops-action="asos-wind-labels" style="width:10px;height:10px;margin:0 1px 0 0;vertical-align:-1px;">lbl</label>',
      helperText: 'Observed station wind barbs from METAR/ASOS reports. Barbs use sustained knots; optional labels and popups show sustained/gust mph. Gust not reported is not zero.',
      layer: new PtOpsAsosAwosWindLayer({
        name: PT_ASOS_WIND_LAYER_NAME,
        url: ASOS_AWOS_WIND_URL,
        summaryUrl: ASOS_AWOS_WIND_SUMMARY_URL,
        manifestUrl: ASOS_AWOS_WIND_MANIFEST_URL,
        sourceUrl: 'https://aviationweather.gov/data/metar/',
        labelMinZoom: 7
      })
    });
  }

)---"
}
