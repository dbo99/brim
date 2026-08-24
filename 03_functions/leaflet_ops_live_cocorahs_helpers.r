# ==== leaflet_ops_live_cocorahs_helpers.r ====================================
##
## PURPOSE:
##   CoCoRaHS-specific JavaScript helpers for the BRIM Ops Live panel.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r` and returns browser-side
##   JavaScript text for a GitHub-hosted CoCoRaHS Daily Precip Reports layer.
##
##   RF011 replaces the earlier direct browser fetch with the same static-feed
##   pattern used by Reservoir Ops because the CoCoRaHS API blocks local/file
##   browser fetches through CORS.
##
##   V2 replaces the original thousands-of-circleMarker implementation with a
##   browser-managed canvas layer. The nationwide feed is too large for one SVG
##   path + tooltip + popup + event set per observation. The canvas design keeps
##   the national precipitation pattern visible while providing:
##     - chunked record preparation so the browser can continue painting;
##     - viewport-only canvas redraws after pan/zoom;
##     - one shared hover tooltip and one on-demand popup;
##     - fetch cancellation and stale-request guards;
##     - one-element teardown when the layer is switched off.
## ============================================================================

# ==== 1. CoCoRaHS Daily Precip hosted-feed browser helpers ===============================

pt_ops_live_cocorahs_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // CoCoRaHS Daily Precip Reports layer.
  // --------------------------------------------------------------------------

  var ptCocoLegendCtl = null;
  var ptCocoLegendDiv = null;
  var ptCocoLegendMap = null;
  var ptCocoLegendActiveLayers = {};
  var ptCocoLegendUserHidden = false;

  function ptCocoLegendRow(color, label, className) {
    var cls = 'pt-ops-cocorahs-legend-swatch' + (className ? ' ' + className : '');
    return '<div class="pt-ops-cocorahs-legend-row"><span class="' + cls + '" style="background:' + color + ';"></span>' + label + '</div>';
  }

  function ptCocoLegendHtml() {
    return '<div class="pt-ops-cocorahs-legend-head">' +
      '<div class="pt-ops-cocorahs-legend-title">CoCoRaHS daily precipitation</div>' +
      '<button type="button" class="pt-ops-cocorahs-legend-close" title="Hide CoCoRaHS legend" aria-label="Hide CoCoRaHS legend">×</button>' +
      '</div>' +
      '<div class="pt-ops-cocorahs-legend-subtitle">Fixed-size stations; color = human-measured 24-hr total (in).</div>' +
      '<div class="pt-ops-cocorahs-legend-grid">' +
      ptCocoLegendRow('#BDBDBD', 'NA', 'missing') +
      ptCocoLegendRow('transparent', 'Zero', 'hollow') +
      ptCocoLegendRow('#7B3294', 'Trace', '') +
      ptCocoLegendRow('#5E4FA2', '0.01–0.10 in', '') +
      ptCocoLegendRow('#3288BD', '0.10–0.25', '') +
      ptCocoLegendRow('#66C2A5', '0.25–0.50', '') +
      ptCocoLegendRow('#ABDDA4', '0.50–1.00', '') +
      ptCocoLegendRow('#FEE08B', '1.00–2.00', '') +
      ptCocoLegendRow('#FDAE61', '2.00–3.00', '') +
      ptCocoLegendRow('#F46D43', '3.00–5.00', '') +
      ptCocoLegendRow('#D53E4F', '&gt;5.00', '') +
      '</div>' +
      '<div class="pt-ops-cocorahs-legend-note">Purple = trace/very light. Amounts are prior-24-hr station reports; reporting times vary.</div>';
  }

  function ptCocoActiveLegendCount() {
    return Object.keys(ptCocoLegendActiveLayers).filter(function(k) { return ptCocoLegendActiveLayers[k]; }).length;
  }

  function ptCocoDropLegendControl() {
    if (ptCocoLegendCtl && ptCocoLegendMap) {
      try { ptCocoLegendMap.removeControl(ptCocoLegendCtl); } catch(e) {}
    }
    ptCocoLegendCtl = null;
    ptCocoLegendDiv = null;
    ptCocoLegendMap = null;
  }

  function ptCocoEnsureLegend(mapObj) {
    if (!mapObj || ptCocoLegendUserHidden) return;
    if (ptCocoLegendCtl && ptCocoLegendMap === mapObj) {
      if (ptCocoLegendDiv) ptCocoLegendDiv.innerHTML = ptCocoLegendHtml();
      return;
    }

    ptCocoDropLegendControl();

    ptCocoLegendMap = mapObj;
    ptCocoLegendCtl = L.control({position: 'topleft'});
    ptCocoLegendCtl.onAdd = function() {
      var div = L.DomUtil.create('div', 'pt-ops-cocorahs-legend-control leaflet-control');
      ptCocoLegendDiv = div;
      div.innerHTML = ptCocoLegendHtml();
      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);
      div.addEventListener('click', function(evt) {
        var closeBtn = evt.target && evt.target.closest ? evt.target.closest('.pt-ops-cocorahs-legend-close') : null;
        if (!closeBtn) return;
        evt.preventDefault();
        evt.stopPropagation();
        ptCocoLegendUserHidden = true;
        ptCocoDropLegendControl();
      });
      return div;
    };
    ptCocoLegendCtl.addTo(mapObj);
  }

  function ptCocoRemoveLegendIfUnused() {
    if (ptCocoActiveLegendCount() > 0) return;
    ptCocoLegendUserHidden = false;
    ptCocoDropLegendControl();
  }

  function ptCocoEnsureStyle() {
    if (document.getElementById('pt-ops-cocorahs-style')) return;

    var style = document.createElement('style');
    style.id = 'pt-ops-cocorahs-style';
    style.innerHTML = `
      .leaflet-tooltip.pt-ops-cocorahs-tooltip {
        background: rgba(255, 255, 255, 0.96);
        border: 1px solid rgba(0,0,0,0.42);
        border-radius: 4px;
        box-shadow: 0 2px 7px rgba(0,0,0,0.22);
        color: #111;
        padding: 5px 7px;
        font: 12px/1.25 Arial, Helvetica, sans-serif;
        white-space: nowrap;
      }

      .pt-ops-cocorahs-popup {
        font: 12px/1.35 Arial, Helvetica, sans-serif;
        min-width: 235px;
        max-width: 350px;
      }

      .pt-ops-cocorahs-popup a {
        color: #1f5e9c;
        text-decoration: none;
      }

      .pt-ops-cocorahs-popup a:hover {
        text-decoration: underline;
      }

      .pt-ops-cocorahs-legend-control {
        background: rgba(226, 238, 235, 0.96);
        border: 1px solid rgba(54, 84, 86, 0.45);
        border-radius: 5px;
        padding: 6px 8px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.25);
        font: 12px/1.2 Arial, Helvetica, sans-serif;
        color: #1f2526;
        min-width: 214px;
        max-width: 242px;
        position: absolute !important;
        left: 96px !important;
        top: 6px !important;
        z-index: 10010;
      }
      .pt-ops-cocorahs-legend-head {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 8px;
        margin-bottom: 2px;
      }
      .pt-ops-cocorahs-legend-title {
        font-weight: 700;
        margin-bottom: 2px;
        padding-right: 16px;
      }
      .pt-ops-cocorahs-legend-close {
        appearance: none;
        -webkit-appearance: none;
        border: 0;
        background: transparent;
        color: rgba(37, 52, 55, 0.72);
        cursor: pointer;
        font: 700 16px/1 Arial, Helvetica, sans-serif;
        padding: 0 1px;
        margin: -2px -2px 0 4px;
      }
      .pt-ops-cocorahs-legend-close:hover {
        color: #111;
      }
      .pt-ops-cocorahs-legend-subtitle,
      .pt-ops-cocorahs-legend-note {
        font-size: 10.5px;
        color: #4f5d5d;
        line-height: 1.18;
      }
      .pt-ops-cocorahs-legend-subtitle {
        margin-bottom: 5px;
      }
      .pt-ops-cocorahs-legend-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        column-gap: 8px;
        row-gap: 2px;
        margin-top: 3px;
      }
      .pt-ops-cocorahs-legend-row {
        white-space: nowrap;
        font-size: 11px;
      }
      .pt-ops-cocorahs-legend-swatch {
        display: inline-block;
        width: 10px;
        height: 10px;
        border: 1.2px solid rgba(0,0,0,0.42);
        border-radius: 50%;
        margin-right: 4px;
        vertical-align: -1px;
        box-sizing: border-box;
      }
      .pt-ops-cocorahs-legend-swatch.hollow {
        background: transparent !important;
        border: 1.6px solid #777;
      }
      .pt-ops-cocorahs-legend-swatch.missing {
        border-style: dashed;
        opacity: 0.72;
      }
      .pt-ops-cocorahs-legend-note {
        border-top: 1px solid rgba(54, 84, 86, 0.18);
        padding-top: 4px;
        margin-top: 5px;
      }

      .pt-ops-cocorahs-canvas {
        position: absolute;
        pointer-events: none;
        opacity: 1;
        transition: opacity 0.06s linear;
      }
    `;
    document.head.appendChild(style);
  }

  function ptCocoNumber(value) {
    if (value === null || value === undefined) return null;
    if (typeof value === 'boolean') return null;

    if (typeof value === 'string') {
      value = value.trim();
      if (!value || ['NA', 'NULL', 'NAN'].indexOf(value.toUpperCase()) >= 0) return null;
    }

    var n = Number(value);
    return isFinite(n) ? n : null;
  }

  function ptCocoBool(value) {
    if (value === true) return true;
    if (value === false || value === null || value === undefined) return false;
    var txt = String(value).trim().toLowerCase();
    return txt === 'true' || txt === 't' || txt === '1' || txt === 'yes' || txt === 'y';
  }

  function ptCocoTrim(value) {
    if (value === null || value === undefined) return '';
    return String(value).trim();
  }

  function ptCocoDatePartLosAngeles(offsetDays) {
    offsetDays = Number(offsetDays || 0);

    var d = new Date();
    d.setDate(d.getDate() + offsetDays);

    try {
      var parts = new Intl.DateTimeFormat('en-CA', {
        timeZone: 'America/Los_Angeles',
        year: 'numeric',
        month: '2-digit',
        day: '2-digit'
      }).formatToParts(d);

      var y = '';
      var m = '';
      var day = '';

      parts.forEach(function(part) {
        if (part.type === 'year') y = part.value;
        if (part.type === 'month') m = part.value;
        if (part.type === 'day') day = part.value;
      });

      if (y && m && day) return y + '-' + m + '-' + day;
    } catch(e) {
      // Fall through to browser-local date below.
    }

    return d.toISOString().slice(0, 10);
  }

  function ptCocoReportedLocalParts(value) {
    var raw = ptCocoTrim(value);
    if (!raw) return null;

    var s = raw.replace('T', ' ').trim();

    // CoCoRaHS API date-time strings often carry +00:00 / Z even though the
    // public report pages display the observation clock time as station-local.
    // For observation/submission fields, treat the clock as reported by
    // CoCoRaHS rather than converting the value to browser/Pacific time.
    s = s.replace(/(\d{1,2}:\d{2}(?::\d{2}(?:\.\d+)?)?)\s*(?:Z|[+-]\d{2}:?\d{2})$/i, '$1');
    s = s.replace(/\.\d+$/, '');

    var m = s.match(/^(\d{4})-(\d{2})-(\d{2})(?:\s+(\d{1,2}):(\d{2})(?::(\d{2}))?)?/);
    if (!m) return null;

    return {
      year: Number(m[1]),
      month: Number(m[2]),
      day: Number(m[3]),
      hour: m[4] === undefined ? null : Number(m[4]),
      minute: m[5] === undefined ? null : Number(m[5]),
      second: m[6] === undefined ? null : Number(m[6])
    };
  }

  function ptCocoFmtReportedLocalDateTime(value, suffix) {
    var raw = ptCocoTrim(value);
    if (!raw) return 'time not reported';

    var parts = ptCocoReportedLocalParts(raw);
    if (!parts) return raw.replace('T', ' ');

    var monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
    var dayNames = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
    var d = new Date(Date.UTC(parts.year, parts.month - 1, parts.day));

    var out = dayNames[d.getUTCDay()] + ', ' + monthNames[parts.month - 1] + ' ' + parts.day;

    if (parts.hour !== null && parts.minute !== null) {
      var h = parts.hour;
      var ampm = h >= 12 ? 'PM' : 'AM';
      var h12 = h % 12;
      if (h12 === 0) h12 = 12;
      out += ', ' + h12 + ':' + String(parts.minute).padStart(2, '0') + ' ' + ampm;
    }

    if (suffix) out += ' ' + suffix;
    return out;
  }

  function ptCocoQueryDateWindow(obs) {
    obs = obs || {};

    var startDate = ptCocoTrim(obs.sourceWindowStartDate || obs.start_date || obs.startDate);
    var endDate = ptCocoTrim(obs.sourceWindowEndDate || obs.end_date || obs.endDate);

    if (startDate && endDate) {
      if (startDate === endDate) return startDate;
      return startDate + ' to ' + endDate;
    }

    return '';
  }

  function ptCocoGaugeWindowText(obs) {
    obs = obs || {};

    var obsTime = ptCocoTrim(obs.obsDateTime);
    if (obsTime) {
      return 'Approx. prior 24 hr ending at reported obs time';
    }

    return 'Approx. prior 24 hr; station reporting times vary';
  }

  function ptCocoFmtPrecip(value, isTrace) {
    if (ptCocoBool(isTrace)) return 'Trace';
    var n = ptCocoNumber(value);
    if (n === null) return 'Not reported';
    return n.toLocaleString(undefined, {
      minimumFractionDigits: 2,
      maximumFractionDigits: 2
    }) + ' in';
  }

  function ptCocoAmountForStyle(obs) {
    obs = obs || {};
    if (ptCocoBool(obs.precipIsTrace) || ptCocoBool(obs.gaugeCatchIsTrace)) return 0.001;

    var n = ptCocoNumber(obs.precip);
    if (n !== null) return n;

    n = ptCocoNumber(obs.gaugeCatch);
    if (n !== null) return n;

    return null;
  }

  function ptCocoClassKey(obs) {
    obs = obs || {};
    if (ptCocoBool(obs.precipIsTrace) || ptCocoBool(obs.gaugeCatchIsTrace)) return 'trace';

    var n = ptCocoAmountForStyle(obs);
    if (n === null) return 'missing';
    if (n <= 0) return 'zero';
    if (n < 0.10) return 'p001_010';
    if (n < 0.25) return 'p010_025';
    if (n < 0.50) return 'p025_050';
    if (n < 1.00) return 'p050_100';
    if (n < 2.00) return 'p100_200';
    if (n < 3.00) return 'p200_300';
    if (n < 5.00) return 'p300_500';
    return 'p500_plus';
  }

  function ptCocoStyleForObs(obs) {
    var key = ptCocoClassKey(obs);
    var base = {
      radius: 5,
      stroke: '#4D4D4D',
      weight: 1.1,
      opacity: 0.80,
      fill: '#BDBDBD',
      fillOpacity: 0.76,
      dashArray: null
    };

    var colors = {
      trace: '#7B3294',
      p001_010: '#5E4FA2',
      p010_025: '#3288BD',
      p025_050: '#66C2A5',
      p050_100: '#ABDDA4',
      p100_200: '#FEE08B',
      p200_300: '#FDAE61',
      p300_500: '#F46D43',
      p500_plus: '#D53E4F'
    };

    if (key === 'missing') {
      base.fill = '#BDBDBD';
      base.fillOpacity = 0.34;
      base.stroke = '#777777';
      base.opacity = 0.70;
      base.dashArray = '3 3';
      return base;
    }

    if (key === 'zero') {
      base.fill = '#FFFFFF';
      base.fillOpacity = 0;
      base.stroke = '#777777';
      base.weight = 1.35;
      base.opacity = 0.72;
      return base;
    }

    base.fill = colors[key] || '#BDBDBD';
    base.stroke = key === 'trace' ? '#542788' : '#3F3F3F';
    base.weight = key === 'trace' ? 1.25 : 1.05;
    base.fillOpacity = 0.76;
    return base;
  }

  function ptCocoPopupLink(label, url) {
    url = ptCocoTrim(url);
    if (!url) return '';
    return '<div style="margin:1px 0;"><a href="' + escapeHtml(url) + '" target="_blank" rel="noopener">' + escapeHtml(label) + '</a></div>';
  }

  function ptCocoStationUrl(obs) {
    var stationNumber = ptCocoTrim(obs.stationNumber);
    if (!stationNumber) return '';
    return 'https://www.cocorahs.org/ViewData/ViewStationPrecipSummary.aspx?StationNumber=' + encodeURIComponent(stationNumber);
  }


  function ptCocoDailyReportUrl(obs) {
    obs = obs || {};
    var reportId = ptCocoTrim(obs.dailyPrecipReportID || obs.dailyPrecipReportId || obs.id);
    if (!reportId) return '';
    return 'https://www.cocorahs.org/ViewData/ViewDailyPrecipReport.aspx?DailyPrecipReportID=' + encodeURIComponent(reportId);
  }

  function ptCocoTooltip(obs) {
    obs = obs || {};

    var station = ptCocoTrim(obs.stationNumber) || ptCocoTrim(obs.stationName) || 'CoCoRaHS station';
    var precip = ptCocoFmtPrecip(obs.precip, obs.precipIsTrace);
    var obsTime = ptCocoFmtReportedLocalDateTime(obs.obsDateTime, 'station local');
    var notes = ptCocoTrim(obs.notes) ? ' · notes' : '';
    var flooding = ptCocoTrim(obs.flooding) ? ' · flooding: ' + ptCocoTrim(obs.flooding) : '';

    return '<b>' + escapeHtml(station) + '</b>' +
      (obs.stationName ? ' – ' + escapeHtml(obs.stationName) : '') +
      '<br>Precip: ' + escapeHtml(precip) +
      '<br>Obs: ' + escapeHtml(obsTime) +
      '<br><span style="color:#555;">Window: prior ~24 hr ending at obs time</span>' +
      (notes || flooding ? '<br><span style="color:#555;">' + escapeHtml((notes + flooding).replace(/^\s*·\s*/, '')) + '</span>' : '');
  }

  function ptCocoPopup(obs) {
    obs = obs || {};

    var stationNumber = ptCocoTrim(obs.stationNumber);
    var stationName = ptCocoTrim(obs.stationName);
    var precip = ptCocoFmtPrecip(obs.precip, obs.precipIsTrace);
    var gaugeCatch = ptCocoFmtPrecip(obs.gaugeCatch, obs.gaugeCatchIsTrace);
    var obsTime = ptCocoFmtReportedLocalDateTime(obs.obsDateTime, 'station local');
    var entryTime = ptCocoFmtReportedLocalDateTime(obs.entryDateTime, 'station local');
    var stampTime = ptCocoFmtReportedLocalDateTime(obs.dateTimeStamp, 'station local');
    var gaugeWindow = ptCocoGaugeWindowText(obs);
    var queryDateWindow = ptCocoQueryDateWindow(obs);
    var stationUrl = ptCocoTrim(obs.stationUrl) || ptCocoStationUrl(obs);
    var dailyReportUrl = ptCocoDailyReportUrl(obs);

    var html = '<div class="pt-ops-cocorahs-popup">' +
      '<div style="font-weight:700;font-size:13px;margin-bottom:2px;">' +
      escapeHtml(stationNumber || 'CoCoRaHS') +
      (stationName ? ' – ' + escapeHtml(stationName) : '') +
      '</div>' +
      '<div style="color:#555;font-size:11px;margin-bottom:5px;">Volunteer daily precipitation report; station-specific reporting window</div>';

    html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:4px;">' +
      '<b>Daily precipitation</b>' +
      (stationName ? '<br><b>Station name:</b> ' + escapeHtml(stationName) : '') +
      '<br><b>Precip:</b> ' + escapeHtml(precip) +
      '<br><b>Gauge catch:</b> ' + escapeHtml(gaugeCatch) +
      '<br><b>Observation date/time:</b> ' + escapeHtml(obsTime) +
      (entryTime !== 'time not reported' ? '<br><b>Submitted:</b> ' + escapeHtml(entryTime) : '') +
      '<br><b>Gauge window:</b> ' + escapeHtml(gaugeWindow) +
      (queryDateWindow ? '<br><b>Feed query dates:</b> ' + escapeHtml(queryDateWindow) : '') +
      '<br><span style="color:#555;">Observation/submission times are displayed as CoCoRaHS-reported station-local clock times, not converted to Pacific/browser time. This layer is not a uniform midnight-to-midnight, 4 AM-to-4 AM, or 8 AM-to-8 AM product; reporting times vary by station.</span>' +
      '</div>';

    var hasSnow = ptCocoNumber(obs.depthOfSnowfall) !== null || ptCocoBool(obs.depthOfSnowfallIsTrace) ||
      ptCocoNumber(obs.waterContentOfSnowfall) !== null || ptCocoBool(obs.waterContentOfSnowfallIsTrace) ||
      ptCocoNumber(obs.depthOfSnowOnGround) !== null || ptCocoBool(obs.depthOfSnowOnGroundIsTrace) ||
      ptCocoNumber(obs.waterContentOfSnowOnGround) !== null || ptCocoBool(obs.waterContentOfSnowOnGroundIsTrace);

    if (hasSnow) {
      html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
        '<b>Snow fields</b>' +
        '<br><b>New snowfall depth:</b> ' + escapeHtml(ptCocoFmtPrecip(obs.depthOfSnowfall, obs.depthOfSnowfallIsTrace).replace(' in', ' in')) +
        '<br><b>New snow water content:</b> ' + escapeHtml(ptCocoFmtPrecip(obs.waterContentOfSnowfall, obs.waterContentOfSnowfallIsTrace)) +
        '<br><b>Snow on ground:</b> ' + escapeHtml(ptCocoFmtPrecip(obs.depthOfSnowOnGround, obs.depthOfSnowOnGroundIsTrace).replace(' in', ' in')) +
        '<br><b>Snowpack water content:</b> ' + escapeHtml(ptCocoFmtPrecip(obs.waterContentOfSnowOnGround, obs.waterContentOfSnowOnGroundIsTrace)) +
        '</div>';
    }

    var flooding = ptCocoTrim(obs.flooding);
    var notes = ptCocoTrim(obs.notes);

    if (flooding || notes) {
      html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
        '<b>Notes</b>';
      if (flooding) html += '<br><b>Flooding:</b> ' + escapeHtml(flooding);
      if (notes) html += '<br><span style="color:#333;">' + escapeHtml(notes.length > 500 ? notes.substring(0, 497) + '…' : notes) + '</span>';
      html += '</div>';
    }

    html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
      '<b>Source / caveat</b>' +
      '<br>Source: CoCoRaHS — Community Collaborative Rain, Hail & Snow Network.' +
      '<br><span style="color:#555;">Volunteer-reported daily observations; use as supplemental screening and storm-verification context, especially for checking localized convective precipitation. Each amount is best read as an approximate prior-24-hour station report ending at the listed obs time.</span>' +
      '<br><span style="color:#555;">CoCoRaHS API timestamp: ' + escapeHtml(stampTime) + '</span>' +
      '</div>';

    var linkLines =
      ptCocoPopupLink('CoCoRaHS daily report', dailyReportUrl) +
      ptCocoPopupLink('CoCoRaHS station page', stationUrl) +
      ptCocoPopupLink('CoCoRaHS API help', 'https://api2.cocorahs.org/Help');

    if (linkLines) {
      html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
        '<b>Links</b>' + linkLines + '</div>';
    }

    html += '</div>';
    return html;
  }

  function ptCocoObsFromGeoJsonFeature(feature) {
    feature = feature || {};

    var obs = feature.properties || {};
    var geom = feature.geometry || {};
    var coords = Array.isArray(geom.coordinates) ? geom.coordinates : null;

    if ((obs.latitude === null || obs.latitude === undefined || obs.latitude === '') &&
        coords && coords.length >= 2) {
      obs.latitude = coords[1];
    }

    if ((obs.longitude === null || obs.longitude === undefined || obs.longitude === '') &&
        coords && coords.length >= 2) {
      obs.longitude = coords[0];
    }

    return obs;
  }



  function ptCocoParseMillis(value) {
    var raw = ptCocoTrim(value);
    if (!raw) return -Infinity;

    var d = new Date(raw);
    if (!isNaN(d.getTime())) return d.getTime();

    return -Infinity;
  }

  function ptCocoFeatureStationKey(feature) {
    var obs = ptCocoObsFromGeoJsonFeature(feature || {});
    var station = ptCocoTrim(obs.stationNumber);

    if (station) {
      return 'station:' + station.toUpperCase();
    }

    var lat = ptCocoNumber(obs.latitude);
    var lng = ptCocoNumber(obs.longitude);

    if (lat !== null && lng !== null) {
      return 'xy:' + lat.toFixed(5) + ',' + lng.toFixed(5);
    }

    return '';
  }

  function ptCocoFeatureTimeScore(feature) {
    var obs = ptCocoObsFromGeoJsonFeature(feature || {});

    return {
      obs: ptCocoParseMillis(obs.obsDateTime),
      entry: ptCocoParseMillis(obs.entryDateTime),
      stamp: ptCocoParseMillis(obs.dateTimeStamp)
    };
  }

  function ptCocoIsFeatureNewer(a, b) {
    if (!b) return true;

    var ta = ptCocoFeatureTimeScore(a);
    var tb = ptCocoFeatureTimeScore(b);

    if (ta.obs !== tb.obs) return ta.obs > tb.obs;
    if (ta.entry !== tb.entry) return ta.entry > tb.entry;
    if (ta.stamp !== tb.stamp) return ta.stamp > tb.stamp;

    return false;
  }

  function ptCocoDedupLatestByStation(features) {
    features = Array.isArray(features) ? features : [];

    var keepByKey = {};
    var orderByKey = {};
    var passthrough = [];
    var duplicateCount = 0;

    features.forEach(function(feature, idx) {
      var key = ptCocoFeatureStationKey(feature);

      if (!key) {
        passthrough.push(feature);
        return;
      }

      if (!Object.prototype.hasOwnProperty.call(keepByKey, key)) {
        keepByKey[key] = feature;
        orderByKey[key] = idx;
        return;
      }

      duplicateCount += 1;

      if (ptCocoIsFeatureNewer(feature, keepByKey[key])) {
        keepByKey[key] = feature;
        orderByKey[key] = idx;
      }
    });

    var keyed = Object.keys(keepByKey).map(function(key) {
      return {
        key: key,
        feature: keepByKey[key],
        order: orderByKey[key]
      };
    });

    keyed.sort(function(a, b) { return a.order - b.order; });

    return {
      features: keyed.map(function(x) { return x.feature; }).concat(passthrough),
      duplicateCount: duplicateCount
    };
  }

  function ptCocoFmtFeedAge(value) {
    var raw = ptCocoTrim(value);
    if (!raw) return '';

    var d = new Date(raw);
    if (isNaN(d.getTime())) return raw;

    var minutes = Math.round((Date.now() - d.getTime()) / 60000);

    if (!isFinite(minutes)) return raw;
    if (minutes < 0) minutes = 0;
    if (minutes < 60) return minutes.toLocaleString() + ' min old';

    var hours = minutes / 60;
    if (hours < 48) {
      return hours.toLocaleString(undefined, {maximumFractionDigits: 1}) + ' hr old';
    }

    return (hours / 24).toLocaleString(undefined, {maximumFractionDigits: 1}) + ' days old';
  }

  function ptCocoMetaValue(meta, names) {
    meta = meta || {};
    names = Array.isArray(names) ? names : [names];

    for (var i = 0; i < names.length; i++) {
      var nm = names[i];
      if (Object.prototype.hasOwnProperty.call(meta, nm) &&
          meta[nm] !== null && meta[nm] !== undefined && String(meta[nm]).trim() !== '') {
        return meta[nm];
      }
    }

    return '';
  }

  function ptCocoAbortError(message) {
    var err = new Error(message || 'CoCoRaHS operation cancelled.');
    err.name = 'AbortError';
    return err;
  }

  function ptCocoYield(callback) {
    if (typeof window.requestIdleCallback === 'function') {
      window.requestIdleCallback(callback, {timeout: 60});
    } else {
      window.setTimeout(function() { callback(null); }, 0);
    }
  }

  function ptCocoDrawRank(obs) {
    var key = ptCocoClassKey(obs);
    var amount = ptCocoAmountForStyle(obs);

    if (key === 'missing') return 0;
    if (key === 'zero') return 1;
    if (key === 'trace') return 2;

    return 3 + Math.max(0, amount === null ? 0 : amount);
  }

  function ptCocoPrepareCanvasRecords(features, isCancelled, onProgress) {
    features = Array.isArray(features) ? features : [];
    isCancelled = typeof isCancelled === 'function' ? isCancelled : function() { return false; };
    onProgress = typeof onProgress === 'function' ? onProgress : function() {};

    return new Promise(function(resolve, reject) {
      var records = [];
      var idx = 0;
      var stats = {
        validCount: 0,
        traceCount: 0,
        zeroCount: 0,
        measurableCount: 0,
        missingCount: 0
      };

      function finish() {
        records.sort(function(a, b) {
          if (a.drawRank !== b.drawRank) return a.drawRank - b.drawRank;
          return a.sourceOrder - b.sourceOrder;
        });

        records.forEach(function(record, order) {
          record.drawOrder = order;
        });

        resolve({records: records, stats: stats});
      }

      function step(deadline) {
        if (isCancelled()) {
          reject(ptCocoAbortError('CoCoRaHS record preparation cancelled.'));
          return;
        }

        var started = Date.now();
        var processed = 0;

        while (idx < features.length) {
          if (processed >= 750 || Date.now() - started >= 12) break;
          if (deadline && typeof deadline.timeRemaining === 'function' &&
              deadline.timeRemaining() <= 1 && processed > 50) break;

          var feature = features[idx];
          var obs = ptCocoObsFromGeoJsonFeature(feature);
          var lat = ptCocoNumber(obs.latitude);
          var lng = ptCocoNumber(obs.longitude);
          var sourceOrder = idx;
          idx += 1;
          processed += 1;

          if (lat === null || lng === null) continue;
          if (lat < -90 || lat > 90 || lng < -180 || lng > 180) continue;

          var amount = ptCocoAmountForStyle(obs);
          if (ptCocoBool(obs.precipIsTrace) || ptCocoBool(obs.gaugeCatchIsTrace)) {
            stats.traceCount += 1;
          } else if (amount !== null && amount === 0) {
            stats.zeroCount += 1;
          } else if (amount !== null && amount > 0) {
            stats.measurableCount += 1;
          } else {
            stats.missingCount += 1;
          }

          stats.validCount += 1;
          records.push({
            lat: lat,
            lng: lng,
            latlng: L.latLng(lat, lng),
            obs: obs,
            style: ptCocoStyleForObs(obs),
            drawRank: ptCocoDrawRank(obs),
            drawOrder: 0,
            sourceOrder: sourceOrder
          });
        }

        onProgress(idx, features.length);

        if (idx >= features.length) {
          finish();
        } else {
          ptCocoYield(step);
        }
      }

      ptCocoYield(step);
    });
  }

  var PtCocorahsCanvasLayer = L.Layer.extend({
    initialize: function(opts) {
      L.setOptions(this, opts || {});
      this._name = this.options.name || 'CoCoRaHS Daily Precip Reports';
      this._scopeLabel = this.options.scopeLabel || 'configured area';
      this._active = false;
      this._map = null;
      this._canvas = null;
      this._ctx = null;
      this._records = null;
      this._stats = null;
      this._meta = null;
      this._dedupCount = 0;
      this._statusMessage = '';
      this._requestSeq = 0;
      this._fetchController = null;
      this._redrawTimer = null;
      this._redrawFrame = null;
      this._hitGrid = Object.create(null);
      this._hitCellSize = 18;
      this._hoverRecord = null;
      this._tooltip = null;
      this._popup = null;
      this._visibleCount = 0;
    },

    onAdd: function(mapObj) {
      this._map = mapObj;
      this._active = true;

      ptCocoEnsureStyle();
      ptCocoLegendUserHidden = false;
      ptCocoLegendActiveLayers[this._name] = true;
      ptCocoEnsureLegend(mapObj);

      activeLegendDefs[this._name] = {
        note: this.options.note || (
          'Volunteer daily precipitation reports from CoCoRaHS for ' +
          this._scopeLabel +
          ', fetched by a scheduled BRIM feed and rendered as a browser-managed canvas. ' +
          'Useful for storm verification and QPE/QPF context; each station amount is an ' +
          'approximate prior-24-hour report ending at that station\'s reported observation time.'
        ),
        legendType: null,
        sourceUrl: this.options.sourceUrl || 'https://www.cocorahs.org/',
        legendUrl: '',
        infoUrl: this.options.infoUrl || '',
        infoLabel: this.options.infoLabel || '',
        legendNote: 'Fixed-size points: hollow = zero, gray/dashed = missing, purple = trace/very light, and color bins show human-measured 24-hr total precipitation (in). Reporting times vary by station; this is not a uniform layer-wide clock window.'
      };
      redrawLegend();

      this._ensureCanvas();
      this._bindMapEvents();

      if (this._records && this._records.length) {
        setOpsLayerLoading(this._name, false);
        if (this._statusMessage) recordStatus(this._name, this._statusMessage, 'pt-ops-ok');
        this._queueRedraw(20);
      } else {
        this._startFetch();
      }
    },

    onRemove: function() {
      this._active = false;
      this._requestSeq += 1;
      this._abortFetch();
      this._cancelRedraw();
      this._unbindMapEvents();
      this._closeTooltip();
      this._closePopup();
      this._dropCanvas();

      delete ptCocoLegendActiveLayers[this._name];
      ptCocoRemoveLegendIfUnused();
      delete activeLegendDefs[this._name];
      setOpsLayerLoading(this._name, false);
      redrawLegend();
      recordStatus(this._name, 'Layer turned off.', 'pt-ops-muted');
      this._map = null;
    },

    _ensureCanvas: function() {
      if (!this._map || this._canvas) return;

      var pane = this._map.getPane('pane_ops') || this._map.getPane('overlayPane');
      var canvas = L.DomUtil.create('canvas', 'pt-ops-cocorahs-canvas');
      canvas.setAttribute('aria-hidden', 'true');
      canvas.style.visibility = 'hidden';
      pane.appendChild(canvas);

      this._canvas = canvas;
      this._ctx = canvas.getContext('2d', {alpha: true});
    },

    _dropCanvas: function() {
      if (!this._canvas) return;
      try { L.DomUtil.remove(this._canvas); } catch(e) {
        try {
          if (this._canvas.parentNode) this._canvas.parentNode.removeChild(this._canvas);
        } catch(ignore) {}
      }
      this._canvas = null;
      this._ctx = null;
      this._hitGrid = Object.create(null);
      this._hoverRecord = null;
      this._visibleCount = 0;
    },

    _bindMapEvents: function() {
      if (!this._map) return;
      this._map.on('movestart zoomstart', this._suspendForMove, this);
      this._map.on('moveend zoomend resize viewreset', this._afterViewChange, this);
      this._map.on('mousemove', this._onMouseMove, this);
      this._map.on('mouseout', this._onMouseOut, this);
      this._map.on('click', this._onClick, this);
    },

    _unbindMapEvents: function() {
      if (!this._map) return;
      this._map.off('movestart zoomstart', this._suspendForMove, this);
      this._map.off('moveend zoomend resize viewreset', this._afterViewChange, this);
      this._map.off('mousemove', this._onMouseMove, this);
      this._map.off('mouseout', this._onMouseOut, this);
      this._map.off('click', this._onClick, this);
    },

    _suspendForMove: function() {
      this._cancelRedraw();
      this._closeTooltip();
      if (this._canvas) {
        this._canvas.style.opacity = '0';
        this._canvas.style.visibility = 'hidden';
      }
    },

    _afterViewChange: function() {
      // Let Leaflet paint newly requested basemap tiles before redrawing the
      // volunteer-observation overlay. This is especially important after a
      // large national-to-local zoom jump.
      this._queueRedraw(45);
    },

    _cancelRedraw: function() {
      if (this._redrawTimer !== null) {
        window.clearTimeout(this._redrawTimer);
        this._redrawTimer = null;
      }
      if (this._redrawFrame !== null && typeof window.cancelAnimationFrame === 'function') {
        window.cancelAnimationFrame(this._redrawFrame);
        this._redrawFrame = null;
      }
    },

    _queueRedraw: function(delayMs) {
      if (!this._active || !this._map || !this._canvas || !this._records) return;
      this._cancelRedraw();

      var self = this;
      this._redrawTimer = window.setTimeout(function() {
        self._redrawTimer = null;
        var draw = function() {
          self._redrawFrame = null;
          self._redraw();
        };
        if (typeof window.requestAnimationFrame === 'function') {
          self._redrawFrame = window.requestAnimationFrame(draw);
        } else {
          draw();
        }
      }, Math.max(0, Number(delayMs || 0)));
    },

    _drawRecord: function(ctx, record, point) {
      var style = record.style;
      var radius = Number(style.radius || 5);

      ctx.beginPath();
      ctx.arc(point.x, point.y, radius, 0, Math.PI * 2, false);

      if (Number(style.fillOpacity || 0) > 0) {
        ctx.globalAlpha = Number(style.fillOpacity || 0);
        ctx.fillStyle = style.fill || '#BDBDBD';
        ctx.fill();
      }

      ctx.globalAlpha = Number(style.opacity === undefined ? 1 : style.opacity);
      ctx.strokeStyle = style.stroke || '#4D4D4D';
      ctx.lineWidth = Number(style.weight || 1);
      if (style.dashArray) ctx.setLineDash([3, 3]);
      else ctx.setLineDash([]);
      ctx.stroke();
      ctx.setLineDash([]);
      ctx.globalAlpha = 1;
    },

    _addHit: function(record, point) {
      var cell = this._hitCellSize;
      var key = Math.floor(point.x / cell) + '|' + Math.floor(point.y / cell);
      if (!this._hitGrid[key]) this._hitGrid[key] = [];
      this._hitGrid[key].push({
        record: record,
        x: point.x,
        y: point.y,
        radius: Math.max(7, Number(record.style.radius || 5) + 3),
        order: record.drawOrder
      });
    },

    _redraw: function() {
      if (!this._active || !this._map || !this._canvas || !this._ctx || !this._records) return;

      var size = this._map.getSize();
      if (!size || size.x <= 0 || size.y <= 0) return;

      var dpr = Math.max(1, Math.min(2, Number(window.devicePixelRatio || 1)));
      var pixelWidth = Math.max(1, Math.round(size.x * dpr));
      var pixelHeight = Math.max(1, Math.round(size.y * dpr));

      if (this._canvas.width !== pixelWidth || this._canvas.height !== pixelHeight) {
        this._canvas.width = pixelWidth;
        this._canvas.height = pixelHeight;
        this._canvas.style.width = size.x + 'px';
        this._canvas.style.height = size.y + 'px';
      }

      var topLeft = this._map.containerPointToLayerPoint([0, 0]);
      L.DomUtil.setPosition(this._canvas, topLeft);

      var ctx = this._ctx;
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
      ctx.clearRect(0, 0, size.x, size.y);

      this._hitGrid = Object.create(null);
      this._hoverRecord = null;

      var bounds = this._map.getBounds().pad(0.06);
      var margin = 10;
      var visibleCount = 0;

      for (var i = 0; i < this._records.length; i++) {
        var record = this._records[i];
        if (!bounds.contains(record.latlng)) continue;

        var point = this._map.latLngToContainerPoint(record.latlng);
        if (point.x < -margin || point.y < -margin ||
            point.x > size.x + margin || point.y > size.y + margin) continue;

        this._drawRecord(ctx, record, point);
        this._addHit(record, point);
        visibleCount += 1;
      }

      this._visibleCount = visibleCount;
      this._canvas.style.visibility = 'visible';
      this._canvas.style.opacity = '1';
    },

    _findAt: function(point) {
      if (!point || !this._hitGrid) return null;

      var cell = this._hitCellSize;
      var cx = Math.floor(point.x / cell);
      var cy = Math.floor(point.y / cell);
      var best = null;
      var bestDistance = Infinity;

      for (var dx = -1; dx <= 1; dx++) {
        for (var dy = -1; dy <= 1; dy++) {
          var bucket = this._hitGrid[(cx + dx) + '|' + (cy + dy)] || [];
          for (var i = 0; i < bucket.length; i++) {
            var item = bucket[i];
            var px = point.x - item.x;
            var py = point.y - item.y;
            var distance = px * px + py * py;
            var limit = item.radius * item.radius;
            if (distance > limit) continue;

            if (distance < bestDistance - 0.01 ||
                (Math.abs(distance - bestDistance) <= 0.01 &&
                 best && item.order > best.order)) {
              best = item;
              bestDistance = distance;
            }
          }
        }
      }

      return best;
    },

    _ensureTooltip: function() {
      if (!this._tooltip) {
        this._tooltip = L.tooltip({
          direction: 'top',
          offset: [0, -7],
          opacity: 0.97,
          className: 'pt-ops-cocorahs-tooltip'
        });
      }
      return this._tooltip;
    },

    _closeTooltip: function() {
      this._hoverRecord = null;
      if (!this._map || !this._tooltip) return;
      try {
        if (this._map.hasLayer(this._tooltip)) this._map.removeLayer(this._tooltip);
      } catch(e) {}
    },

    _closePopup: function() {
      if (!this._map || !this._popup) return;
      try {
        if (this._map.hasLayer(this._popup)) this._map.removeLayer(this._popup);
      } catch(e) {}
    },

    _onMouseMove: function(evt) {
      if (!this._active || !this._map || !this._canvas ||
          this._canvas.style.visibility === 'hidden') return;

      var hit = this._findAt(evt.containerPoint);
      if (!hit) {
        this._closeTooltip();
        return;
      }

      if (this._hoverRecord === hit.record) return;
      this._hoverRecord = hit.record;

      var tooltip = this._ensureTooltip();
      tooltip.setLatLng(hit.record.latlng);
      tooltip.setContent(ptCocoTooltip(hit.record.obs));
      if (!this._map.hasLayer(tooltip)) tooltip.addTo(this._map);
    },

    _onMouseOut: function() {
      this._closeTooltip();
    },

    _onClick: function(evt) {
      if (!this._active || !this._map) return;
      var hit = this._findAt(evt.containerPoint);
      if (!hit) return;

      this._closeTooltip();
      if (!this._popup) {
        this._popup = L.popup({
          maxWidth: 430,
          maxHeight: 520,
          autoPan: true,
          keepInView: true,
          offset: [0, -4]
        });
      }

      this._popup
        .setLatLng(hit.record.latlng)
        .setContent(ptCocoPopup(hit.record.obs))
        .openOn(this._map);
    },

    _abortFetch: function() {
      if (this._fetchController) {
        try { this._fetchController.abort(); } catch(e) {}
      }
      this._fetchController = null;
    },

    _startFetch: function() {
      var self = this;
      var url = ptCocoTrim(this.options.url || '');

      if (!url) {
        recordStatus(this._name, 'No CoCoRaHS hosted GeoJSON URL is configured.', 'pt-ops-bad');
        return;
      }

      this._abortFetch();
      this._requestSeq += 1;
      var seq = this._requestSeq;
      var controller = typeof window.AbortController === 'function' ? new AbortController() : null;
      this._fetchController = controller;

      setOpsLayerLoading(this._name, true);
      recordStatus(
        this._name,
        'Fetching hosted CoCoRaHS daily precipitation GeoJSON for ' + this._scopeLabel + '…',
        'pt-ops-warn'
      );

      var fetchOptions = {
        cache: 'no-store',
        headers: {'Accept': 'application/geo+json, application/json'}
      };
      if (controller) fetchOptions.signal = controller.signal;

      fetch(url, fetchOptions)
        .then(function(resp) {
          if (!resp.ok) throw new Error('HTTP ' + resp.status);
          return resp.json();
        })
        .then(function(json) {
          if (!self._active || seq !== self._requestSeq) {
            throw ptCocoAbortError('Stale CoCoRaHS response ignored.');
          }

          var rawFeatures = Array.isArray(json.features) ? json.features : [];
          var meta = json.metadata || json.properties || {};
          var dedup = ptCocoDedupLatestByStation(rawFeatures);

          recordStatus(
            self._name,
            'Preparing ' + dedup.features.length.toLocaleString() +
              ' CoCoRaHS reports for efficient canvas display…',
            'pt-ops-warn'
          );

          return ptCocoPrepareCanvasRecords(
            dedup.features,
            function() { return !self._active || seq !== self._requestSeq; },
            function(done, total) {
              if (!self._active || seq !== self._requestSeq) return;
              if (done > 0 && done < total && done % 3000 < 750) {
                recordStatus(
                  self._name,
                  'Preparing CoCoRaHS canvas records: ' +
                    done.toLocaleString() + ' of ' + total.toLocaleString() + '…',
                  'pt-ops-warn'
                );
              }
            }
          ).then(function(prepared) {
            return {
              prepared: prepared,
              meta: meta,
              dedupCount: dedup.duplicateCount,
              rawCount: rawFeatures.length
            };
          });
        })
        .then(function(result) {
          if (!self._active || seq !== self._requestSeq) {
            throw ptCocoAbortError('Stale CoCoRaHS prepared records ignored.');
          }

          self._records = result.prepared.records;
          self._stats = result.prepared.stats;
          self._meta = result.meta;
          self._dedupCount = result.dedupCount;
          self._fetchController = null;
          setOpsLayerLoading(self._name, false);

          self._statusMessage = self._buildStatusMessage(
            result.meta,
            result.prepared.stats,
            result.dedupCount,
            result.rawCount
          );
          recordStatus(self._name, self._statusMessage, 'pt-ops-ok');
          self._queueRedraw(20);
        })
        .catch(function(err) {
          if (seq !== self._requestSeq) return;
          self._fetchController = null;
          setOpsLayerLoading(self._name, false);

          if (err && err.name === 'AbortError') return;
          recordStatus(
            self._name,
            'Could not fetch or prepare hosted CoCoRaHS GeoJSON: ' +
              (err && err.message ? err.message : err) + '.',
            'pt-ops-bad'
          );
        });
    },

    _buildStatusMessage: function(meta, stats, duplicateCount, rawCount) {
      var startDate = ptCocoMetaValue(meta, ['startDate', 'start_date', 'source_window_start_date']);
      var endDate = ptCocoMetaValue(meta, ['endDate', 'end_date', 'source_window_end_date']);
      var buildTime = ptCocoMetaValue(meta, ['feedBuildTimeUtc', 'feed_build_time_utc']);
      var totalApi = ptCocoMetaValue(meta, ['apiTotalCount', 'api_total_count', 'totalCount']);
      var fetched = ptCocoMetaValue(meta, ['apiRowsFetched', 'api_rows_fetched', 'rowsFetched']);
      var dateWindow = startDate && endDate ? (startDate + ' to ' + endDate) : 'latest configured date window';

      if (!stats.validCount) {
        return 'No mappable CoCoRaHS daily reports in hosted feed for ' +
          this._scopeLabel + ' for ' + dateWindow + '.';
      }

      var msg = stats.validCount.toLocaleString() + ' mappable ' +
        this._scopeLabel + ' daily reports with obs dates ' + dateWindow + '. ' +
        stats.measurableCount.toLocaleString() + ' measurable, ' +
        stats.zeroCount.toLocaleString() + ' zero, ' +
        stats.traceCount.toLocaleString() + ' trace';

      if (stats.missingCount > 0) {
        msg += ', ' + stats.missingCount.toLocaleString() + ' missing amount';
      }
      msg += '. Canvas rendering keeps pan/zoom and layer teardown responsive.';

      if (totalApi !== '' && Number(totalApi) > Number(fetched || rawCount || stats.validCount)) {
        msg += ' Feed includes ' + Number(fetched || rawCount || stats.validCount).toLocaleString() +
          ' of ' + Number(totalApi).toLocaleString() + ' API records.';
      }

      if (duplicateCount > 0) {
        msg += ' Suppressed ' + duplicateCount.toLocaleString() +
          ' older duplicate station report' + (duplicateCount === 1 ? '' : 's') + '.';
      }

      msg += ' Amounts are approximate prior-24-hour station reports ending at the listed observation times; reporting times vary by station.';
      if (buildTime) msg += ' Feed built ' + ptCocoFmtFeedAge(buildTime) + '.';
      return msg;
    }
  });

  function makeCocorahsDailyPrecipLayer(opts) {
    return new PtCocorahsCanvasLayer(opts || {});
  }


)---"
}

