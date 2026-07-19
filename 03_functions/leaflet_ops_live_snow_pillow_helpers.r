# ==== leaflet_ops_live_snow_pillow_helpers.r ================================
## SWE028:
##   - Clarify popup Historical context row so map-bin class is explicitly tied
##     to station/day POA percentile bins, separate from fixed/rolling median rows.
##   - Remove the separate context-bin footnote when the row carries the same
##     information.
## SWE025:
##   - Add compact popup % median row using fixed WY1991-WY2020 and rolling
##     30 complete-WY median SWE fields from the latest GeoJSON.
##   - Keep map colors and Plot B ribbons tied to POA percentile context.
## SWE020b:
##   - Update CDEC source notes for #82-preferred latest feed, with #3 used only
##     as a limited raw-tail/fallback source where #82 is not available/current.
##   - Clarify that CDEC historical context uses #82 while current-year display
##     may include a limited recent #3 tail; stale recent-change deltas are hidden.
## SWE017a:
##   - Harmonize SWE popup Plot A/B title and note wording with the polished
##     SCAN soil-moisture layer: clearer POA labels, water-day ribbon-support
##     wording, compact context-bin wording, and explicit retained-month line
##     break language. No data logic changed.
## SWE016b:
##   - Move prior-WY fallback trace labels from late-season endpoints to an
##     April-1/peak-season anchor, with nearest-valid and peak fallback.
## SWE016:
##   - Add compact WY endpoint labels to Plot B prior-WY fallback traces.
## SWE015c:
##   - Earlier CDEC source note superseded by SWE020b/SWE021 #82-preferred
##     source wording.
## SWE015a:
##   - Clarify Plot B notes/titles for prior-water-year fallback traces.
##   - Draw compact prior-water-year fallback traces in Plot B when daily
##     percentile ribbons are unavailable because the station has <10 years.
##   - Fetch the compact fallback trace CSV produced by the local SWE history
##     context builder.
##
## SWE014c:
##   - Add popup-only explanation when latest-day historical context is unavailable
##     because the station/day does not meet the daily-context threshold.
##
## SWE014b:
##   - Clean up SWE display mode buttons/hover labels, use delta notation,
##     distinguish reported 0 in from no-current symbols, and keep no-current
##     markers truly hollow/transparent.
## SWE014:
##   - Add compact on-hover mini daily-envelope SVG in Context mode.
##   - Mini plot is generated on hover and cached per station so the layer stays
##     responsive; selector modes other than Context keep text-only hovers.
##
## SWE013:
##   - Add map display-mode selector for SWE context, current daily SWE,
##     and 1-/3-/7-day SWE change.
##   - Recolor existing CDEC/NRCS symbols without refetching/reloading the
##     layer; popup Plot A/B behavior is unchanged.
##
## SWE012b:
##   - Keep latest labels explicitly daily, use provider-specific marker
##     shapes, and slightly darken Plot A monthly-reference ribbon.
##
## SWE012a:
##   - Harden current-context coloring, show provider name + station code,
##     filter current-WY traces to fetch_start_date, and place the latest dot
##     by latest observation date instead of max water-day.
##   - Add compact context-bin criteria to popup notes and prevent no-context
##     plot-title overlap.
##
## SWE012:
##   - Add first integrated BRIM Ops Live snow-pillow / SWE helper.
##   - Loads hosted latest GeoJSON, current-WY trace, daily percentile context,
##     and monthly context CSVs only when the layer is turned on.
##   - Draws point colors by current/fresh historical SWE context.
##   - Builds SCAN-style popup plots in the browser, including repeated monthly
##     reference climatology for Plot A and daily envelope for Plot B.
##
## PURPOSE:
##   Snow-pillow / SWE-specific JavaScript helpers for the BRIM Ops Live panel.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r` and returns browser-side
##   JavaScript text for the BRIM-hosted snow-pillow / SWE live feed.
##
##   GitHub Actions supply current/latest values and current-water-year traces.
##   Local preprocessing supplies compact historical context CSVs:
##     - daily water-day percentile ribbons for Plot B
##     - monthly recent-record context for Plot A
##
##   The browser downloads those static files only when the SWE Ops Live layer
##   is turned on.  The full daily-history RDS remains local and is not embedded
##   in the base BRIM HTML.
## ============================================================================

pt_ops_live_snow_pillow_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Snow pillow / SWE latest/context layer from BRIM-hosted GitHub files.
  // --------------------------------------------------------------------------

  function ptSnowEnsureStyle() {
    if (document.getElementById('pt-ops-snow-style')) return;

    var style = document.createElement('style');
    style.id = 'pt-ops-snow-style';
    style.innerHTML = `
      .leaflet-tooltip.pt-ops-snow-tooltip {
        background: rgba(255, 255, 255, 0.97);
        border: 1px solid rgba(0,0,0,0.42);
        border-radius: 4px;
        box-shadow: 0 2px 7px rgba(0,0,0,0.22);
        color: #111;
        padding: 6px 8px;
        font: 12px/1.25 Arial, Helvetica, sans-serif;
        white-space: nowrap;
        max-width: none;
      }

      .pt-ops-snow-hover-mini {
        margin-top: 5px;
        width: 260px;
      }
      .pt-ops-snow-hover-mini svg {
        display: block;
        width: 260px;
        height: 92px;
      }

      .pt-ops-snow-popup {
        width: 560px;
        max-width: 560px;
        font: 12px/1.35 Arial, Helvetica, sans-serif;
      }

      .pt-ops-snow-popup a {
        color: #1f5e9c;
        text-decoration: none;
      }

      .pt-ops-snow-popup a:hover { text-decoration: underline; }

      .pt-ops-snow-plot-shell { margin-top: 5px; }
      .pt-ops-snow-plot-note {
        font-size: 10.5px;
        color: #555;
        line-height: 1.25;
        margin: 2px 2px 0 2px;
      }

      .pt-ops-snow-legend-control {
        background: rgba(226, 238, 235, 0.96);
        border: 1px solid #AAA;
        border-radius: 5px;
        padding: 6px 8px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.25);
        font: 12px/1.2 Arial, Helvetica, sans-serif;
        min-width: 190px;
        position: absolute !important;
        left: 96px !important;
        top: 6px !important;
        z-index: 10010;
      }
      .pt-ops-snow-control-title { font-weight: bold; margin-bottom: 4px; }
      .pt-ops-snow-titlebar { display:flex; align-items:flex-start; justify-content:space-between; gap:8px; margin-bottom:4px; }
      .pt-ops-snow-titlebar .pt-ops-snow-control-title { margin-bottom:0; }
      .pt-ops-snow-legend-close { border:0; background:transparent; color:#777; font:bold 16px/1 Arial, Helvetica, sans-serif; padding:0 1px; cursor:pointer; }
      .pt-ops-snow-legend-close:hover { color:#222; }
      .pt-ops-snow-mode-row { display:flex; flex-wrap:wrap; gap:3px; margin:4px 0 5px 0; max-width:230px; }
      .pt-ops-snow-provider-row { display:flex; flex-wrap:wrap; align-items:center; gap:3px; margin:2px 0 6px 0; max-width:230px; }
      .pt-ops-snow-provider-label { color:#555; font-size:10.5px; margin-right:2px; }
      .pt-ops-snow-mode-btn,
      .pt-ops-snow-provider-btn { border:1px solid #999; background:#F7F7F7; border-radius:3px; padding:2px 5px; font-size:11px; line-height:1.15; cursor:pointer; }
      .pt-ops-snow-mode-btn.active,
      .pt-ops-snow-provider-btn.active { background:#FFFFFF; border-color:#111; font-weight:bold; box-shadow:0 1px 2px rgba(0,0,0,0.22); }
      .pt-ops-snow-map-legend-title { font-weight:bold; margin-bottom:3px; font-size:11px; max-width:230px; }
      .pt-ops-snow-legend-note { font-size:10.5px; color:#555; line-height:1.2; margin-top:4px; max-width:230px; }
      .pt-ops-snow-legend-grid { display: grid; grid-template-columns:1fr 1fr; column-gap:8px; row-gap:2px; }
      .pt-ops-snow-legend-row { white-space: nowrap; font-size: 11px; }
      .pt-ops-snow-legend-row span {
        display: inline-block;
        width: 10px;
        height: 10px;
        border: 1px solid rgba(0,0,0,0.25);
        margin-right: 3px;
        vertical-align: -1px;
      }
      .pt-ops-snow-status-table {
        border-collapse: collapse;
        font-size: 12px;
        margin: 4px 0 6px 0;
      }
      .pt-ops-snow-status-table td {
        padding: 1px 8px 1px 0;
        vertical-align: top;
      }
      .pt-ops-snow-status-table td:first-child {
        color: #555;
        white-space: nowrap;
      }
      .pt-ops-snow-marker-icon {
        background: transparent;
        border: none;
      }
      .pt-ops-snow-provider-symbol {
        display: block;
        width: 14px;
        height: 14px;
        box-sizing: border-box;
        box-shadow: 0 1px 3px rgba(0,0,0,0.25);
      }
    `;
    document.head.appendChild(style);
  }

  function ptSnowTrim(value) {
    if (value === null || value === undefined) return '';
    var txt = String(value).trim();
    if (!txt || ['NA', 'NULL', 'NAN'].indexOf(txt.toUpperCase()) >= 0) return '';
    return txt;
  }

  function ptSnowNum(value) {
    if (value === null || value === undefined || typeof value === 'boolean') return null;
    if (typeof value === 'string') {
      value = value.trim();
      if (!value || ['NA', 'NULL', 'NAN'].indexOf(value.toUpperCase()) >= 0) return null;
    }
    var n = Number(String(value).replace(/,/g, ''));
    return isFinite(n) ? n : null;
  }

  function ptSnowInt(value) {
    var n = ptSnowNum(value);
    return n === null ? null : Math.round(n);
  }

  function ptSnowBool(value) {
    if (value === true) return true;
    if (value === false || value === null || value === undefined) return false;
    var txt = String(value).trim().toLowerCase();
    return txt === 'true' || txt === 't' || txt === '1' || txt === 'yes' || txt === 'y';
  }

  function ptSnowFmt(value, digits) {
    var n = ptSnowNum(value);
    if (n === null) return 'NA';
    return n.toLocaleString(undefined, {
      minimumFractionDigits: digits || 0,
      maximumFractionDigits: digits || 0
    });
  }

  function ptSnowEscape(value) {
    return escapeHtml(value === null || value === undefined ? '' : value);
  }

  function ptSnowShortDate(value) {
    var txt = ptSnowTrim(value);
    if (!txt) return 'date NA';
    var d = new Date(txt + 'T00:00:00Z');
    if (isNaN(d.getTime())) return txt;
    return (d.getUTCMonth() + 1) + '/' + d.getUTCDate() + '/' + String(d.getUTCFullYear()).slice(-2);
  }

  function ptSnowMonYear(value) {
    var txt = ptSnowTrim(value);
    if (!txt) return 'NA';
    var d = new Date(txt + 'T00:00:00Z');
    if (isNaN(d.getTime())) return txt;
    return d.toLocaleString(undefined, {month: 'short', year: 'numeric', timeZone: 'UTC'});
  }

  function ptSnowCsvParse(text) {
    text = text || '';
    text = text.replace(/^\uFEFF/, '');
    if (!text.trim()) return [];

    var rows = [];
    var row = [];
    var cell = '';
    var inQuotes = false;

    for (var i = 0; i < text.length; i++) {
      var ch = text[i];
      if (inQuotes) {
        if (ch === '"') {
          if (text[i + 1] === '"') {
            cell += '"';
            i++;
          } else {
            inQuotes = false;
          }
        } else {
          cell += ch;
        }
      } else {
        if (ch === '"') {
          inQuotes = true;
        } else if (ch === ',') {
          row.push(cell);
          cell = '';
        } else if (ch === '\n') {
          row.push(cell);
          rows.push(row);
          row = [];
          cell = '';
        } else if (ch === '\r') {
          // skip CR in CRLF files
        } else {
          cell += ch;
        }
      }
    }
    row.push(cell);
    rows.push(row);

    if (!rows.length) return [];
    var header = rows.shift().map(function(x) { return String(x || '').trim(); });
    return rows.filter(function(r) {
      return r.some(function(v) { return String(v || '').trim() !== ''; });
    }).map(function(r) {
      var obj = {};
      header.forEach(function(h, idx) { obj[h] = r[idx] === undefined ? '' : r[idx]; });
      return obj;
    });
  }

  function ptSnowGetFeatureProps(feature) {
    return feature && feature.properties ? feature.properties : {};
  }

  function ptSnowCoords(feature) {
    var c = feature && feature.geometry && feature.geometry.coordinates ? feature.geometry.coordinates : [];
    return {lng: ptSnowNum(c[0]), lat: ptSnowNum(c[1])};
  }

  function ptSnowIndexRows(rows, keyCol) {
    var out = {};
    (rows || []).forEach(function(r) {
      var key = ptSnowTrim(r[keyCol || 'station_uid']);
      if (!key) return;
      if (!out[key]) out[key] = [];
      out[key].push(r);
    });
    return out;
  }

  function ptSnowContextFill(label) {
    label = ptSnowTrim(label);
    if (label === 'Much below normal') return '#8C510A';
    if (label === 'Below normal') return '#D8B365';
    if (label === 'Near normal') return '#7F7F7F';
    if (label === 'Above normal') return '#92C5DE';
    if (label === 'Much above normal') return '#2166AC';
    return '#FFFFFF';
  }

  function ptSnowContextFillOpacity(label) {
    label = ptSnowTrim(label);
    return (!label || label === 'No context') ? 0.18 : 0.88;
  }

  function ptSnowProviderSymbolKind(props) {
    var key = ptSnowTrim(props.live_provider_key).toLowerCase();
    var provider = ptSnowTrim(props.provider).toLowerCase();
    var source = ptSnowTrim(props.source_system).toLowerCase();
    var txt = key + ' ' + provider + ' ' + source;
    return (txt.indexOf('cdec') >= 0 || txt.indexOf('ccss') >= 0 || txt.indexOf('dwr') >= 0) ? 'square' : 'circle';
  }

  function ptSnowProviderFilterKey(props) {
    return ptSnowProviderSymbolKind(props) === 'square' ? 'cdec' : 'snotel';
  }

  function ptSnowProviderFilterLabel(filter) {
    if (filter === 'cdec') return 'CDEC/CCSS only';
    if (filter === 'snotel') return 'NRCS/SNOTEL only';
    return 'All providers';
  }

  function ptSnowProviderSymbolLabel(props) {
    return ptSnowProviderSymbolKind(props) === 'square' ? 'square = CDEC/CCSS' : 'circle = NRCS/SNOTEL';
  }

  function ptSnowProviderDataSourceNote(props) {
    if (ptSnowProviderSymbolKind(props) !== 'square') return '';
    var label = ptSnowTrim(props.latest_swe_source_label);
    var cls = ptSnowTrim(props.latest_swe_source_class).toLowerCase();
    var staleDeltaNote = ' Recent-change values are hidden when the latest SWE value is stale.';
    var baseNote = 'CDEC/CCSS: historical context uses SNO ADJ (#82). Current-year display uses #82 where available; SNOW WC (#3) may fill only a limited recent tail when #82 has not caught up.' + staleDeltaNote;
    if (label) {
      if (cls.indexOf('recent_tail') >= 0) {
        return 'CDEC/CCSS SWE source: ' + label + '. Historical context uses #82; the current-year display for this station includes only the limited #3 tail after the latest valid #82 value.' + staleDeltaNote;
      }
      if (cls.indexOf('fallback') >= 0) {
        return 'CDEC/CCSS SWE source: ' + label + '. #82 was unavailable in the current fetch window for this station; #3 is used as the current-year fallback. Historical context uses #82 where available.' + staleDeltaNote;
      }
      if (cls.indexOf('sno_adj') >= 0 || cls.indexOf('82') >= 0) {
        return baseNote;
      }
      return 'CDEC/CCSS SWE source: ' + label + '. ' + baseNote;
    }
    return baseNote;
  }

  function ptSnowMedianPart(props, prefix) {
    var pct = ptSnowNum(props[prefix + '_pct_median_swe']);
    var label = ptSnowTrim(props[prefix + '_label']);
    var nYears = ptSnowInt(props[prefix + '_n_years']);
    var fullYears = ptSnowInt(props[prefix + '_full_years']);

    if (pct === null || !label) return '';

    var out = Math.round(pct).toLocaleString() + '% vs ' + label;
    if (nYears !== null && (fullYears === null || nYears < fullYears)) {
      out += ' (n=' + nYears + ')';
    }
    return out;
  }

  function ptSnowMedianPopupRow(props) {
    var fixed = ptSnowMedianPart(props, 'normal_fixed');
    var rolling = ptSnowMedianPart(props, 'normal_rolling');
    var parts = [];
    if (fixed) parts.push(fixed);
    if (rolling) parts.push(rolling);
    if (!parts.length) return '';
    return '<tr><td>% median</td><td>' + ptSnowEscape(parts.join('; ')) + '</td></tr>';
  }

  function ptSnowMarkerIcon(props, fill, fillOpacity, isCurrent, size) {
    var kind = ptSnowProviderSymbolKind(props);
    var radius = kind === 'circle' ? '50%' : '2px';
    var borderColor = isCurrent ? '#222222' : '#777777';
    var borderStyle = isCurrent ? 'solid' : 'dashed';
    size = ptSnowInt(size);
    if (size === null || size < 10) size = 16;
    if (size > 26) size = 26;
    var opacityStyle = (fill === 'transparent') ? '' : ('opacity:' + fillOpacity + ';');
    var html = '<span class="pt-ops-snow-provider-symbol" style="' +
      'width:' + size + 'px;' +
      'height:' + size + 'px;' +
      'border-radius:' + radius + ';' +
      'background:' + fill + ';' +
      opacityStyle +
      'border:1.4px ' + borderStyle + ' ' + borderColor + ';' +
      '"></span>';
    return L.divIcon({
      className: 'pt-ops-snow-marker-icon',
      html: html,
      iconSize: [size + 2, size + 2],
      iconAnchor: [(size + 2) / 2, (size + 2) / 2],
      popupAnchor: [0, -Math.round(size / 2)],
      tooltipAnchor: [Math.round(size / 2), 0]
    });
  }

  function ptSnowStatusLabel(reportStatus, stalenessClass) {
    reportStatus = ptSnowTrim(reportStatus);
    stalenessClass = ptSnowTrim(stalenessClass);

    if (reportStatus === 'stale_last_value' || stalenessClass === 'very_stale_gt_21_days') return 'Stale measurement';
    if (reportStatus === 'missing_recent_value' || stalenessClass === 'no_valid_current_wy_swe') return 'No valid SWE reported';
    if (reportStatus === 'reported_zero' && (stalenessClass === 'fresh_0_2_days' || stalenessClass === 'recent_3_7_days')) return 'Current zero SWE';
    if (reportStatus === 'reported_positive' && (stalenessClass === 'fresh_0_2_days' || stalenessClass === 'recent_3_7_days')) return 'Current SWE reported';
    if ((reportStatus === 'reported_zero' || reportStatus === 'reported_positive') && stalenessClass === 'stale_8_21_days') return 'Older measurement';
    return 'Check source';
  }

  function ptSnowReportIsCurrent(props) {
    var age = ptSnowInt(props.latest_swe_age_days);
    var status = ptSnowTrim(props.latest_swe_report_status);
    var staleClass = ptSnowTrim(props.latest_swe_staleness_class);
    return age !== null && age <= 2 && staleClass === 'fresh_0_2_days' &&
      (status === 'reported_positive' || status === 'reported_zero');
  }

  function ptSnowWaterYearFromDate(dateTxt) {
    var txt = ptSnowTrim(dateTxt);
    if (!txt) return null;
    var d = new Date(txt + 'T00:00:00Z');
    if (isNaN(d.getTime())) return null;
    var yr = d.getUTCFullYear();
    var mo = d.getUTCMonth() + 1;
    return mo >= 10 ? yr + 1 : yr;
  }

  function ptSnowWaterDayFromDate(dateTxt) {
    var txt = ptSnowTrim(dateTxt);
    if (!txt) return null;
    var d = new Date(txt + 'T00:00:00Z');
    if (isNaN(d.getTime())) return null;
    var wy = ptSnowWaterYearFromDate(txt);
    if (wy === null) return null;
    var start = new Date(Date.UTC(wy - 1, 9, 1));
    return Math.floor((d.getTime() - start.getTime()) / 86400000) + 1;
  }

  function ptSnowWaterMonthFromDateObj(d) {
    var mo = d.getUTCMonth() + 1;
    return mo >= 10 ? mo - 9 : mo + 3;
  }

  function ptSnowMonthDate(waterYear, waterMonth) {
    waterYear = ptSnowInt(waterYear);
    waterMonth = ptSnowInt(waterMonth);
    if (waterYear === null || waterMonth === null) return null;
    var calMonth = [10,11,12,1,2,3,4,5,6,7,8,9][Math.max(0, Math.min(11, waterMonth - 1))];
    var calYear = waterMonth <= 3 ? waterYear - 1 : waterYear;
    return new Date(Date.UTC(calYear, calMonth - 1, 1));
  }

  function ptSnowMonthSequence(startDate, endDate) {
    var out = [];
    if (!startDate || !endDate || isNaN(startDate.getTime()) || isNaN(endDate.getTime())) return out;
    var d = new Date(Date.UTC(startDate.getUTCFullYear(), startDate.getUTCMonth(), 1));
    var end = new Date(Date.UTC(endDate.getUTCFullYear(), endDate.getUTCMonth(), 1));
    while (d.getTime() <= end.getTime()) {
      out.push(new Date(d.getTime()));
      d.setUTCMonth(d.getUTCMonth() + 1);
    }
    return out;
  }

  function ptSnowCurrentWyFromTrace(rows) {
    var wy = null;
    (rows || []).forEach(function(r) {
      var n = ptSnowInt(r.water_year);
      if (n !== null && (wy === null || n > wy)) wy = n;
    });
    if (wy !== null) return wy;
    var latest = '';
    (rows || []).forEach(function(r) {
      var d = ptSnowTrim(r.obs_date_local);
      if (d && (!latest || d > latest)) latest = d;
    });
    return latest ? ptSnowWaterYearFromDate(latest) : null;
  }

  function ptSnowContextLabel(sweIn, p10, p30, p70, p90, contextOk) {
    sweIn = ptSnowNum(sweIn);
    p10 = ptSnowNum(p10);
    p30 = ptSnowNum(p30);
    p70 = ptSnowNum(p70);
    p90 = ptSnowNum(p90);
    if (!contextOk || sweIn === null || p10 === null || p30 === null || p70 === null || p90 === null) return 'No context';
    if (sweIn <= p10) return 'Much below normal';
    if (sweIn <= p30) return 'Below normal';
    if (sweIn <= p70) return 'Near normal';
    if (sweIn <= p90) return 'Above normal';
    return 'Much above normal';
  }

  function ptSnowContextBinNote() {
    return 'Context bins: latest current SWE vs station/day historical percentiles; ≤10th = much below; >10th–30th = below; >30th–70th = near normal; >70th–90th = above; >90th = much above.';
  }

  function ptSnowContextBinShort(label) {
    label = ptSnowTrim(label);
    if (label === 'Much below normal') return '≤10th';
    if (label === 'Below normal') return '10–30th';
    if (label === 'Near normal') return '30–70th';
    if (label === 'Above normal') return '70–90th';
    if (label === 'Much above normal') return '>90th';
    return '';
  }

  function ptSnowContextPopupText(contextObj, fallbackText) {
    var out = ptSnowTrim(fallbackText) || 'No current context';
    if (!contextObj || !contextObj.isCurrent || !contextObj.label || contextObj.label === 'No context') return out;

    var bin = ptSnowContextBinShort(contextObj.label);
    if (!bin) return out;

    var row = contextObj.row || {};
    var refLabel = ptSnowTrim(row.daily_ref_label);
    var basis = refLabel ? ('station/day percentile, POA ' + refLabel) : 'station/day POA percentile';

    return out + ' — ' + basis + ' (' + bin + ')';
  }

  function ptSnowStatsPoaText(label, initialCap) {
    label = ptSnowTrim(label);
    if (!label || label.toLowerCase() === 'poa unavailable') {
      return (initialCap ? 'Stats' : 'stats') + ' POA unavailable';
    }
    return (initialCap ? 'Stats' : 'stats') + ' POA ' + label;
  }

  function ptSnowContextUnavailableDetail(props, match, reportCurrent, latestSwe) {
    if (!reportCurrent) {
      return 'latest daily SWE is not fresh/current for map context.';
    }
    if (latestSwe === null) {
      return 'latest daily SWE is unavailable.';
    }
    if (!match) {
      return 'no historical percentile row is available for the latest station/day.';
    }

    var threshold = ptSnowInt(match.min_years_for_daily_context);
    if (threshold === null) threshold = 10;

    var nYears = ptSnowInt(match.n_years);
    var contextOk = ptSnowBool(match.context_ok);

    if (!contextOk && nYears !== null && nYears < threshold) {
      return 'latest-day history has ' + nYears + ' years; threshold is ' + threshold + ' years.';
    }
    if (!contextOk) {
      return 'latest-day historical context does not meet the context-quality threshold.';
    }

    var missingPercentiles = ['p10_swe_in', 'p30_swe_in', 'p70_swe_in', 'p90_swe_in'].some(function(k) {
      return ptSnowNum(match[k]) === null;
    });
    if (missingPercentiles) {
      return 'latest-day percentile values are unavailable.';
    }

    return '';
  }

  function ptSnowContextForFeature(props, pctRows) {
    var latestSwe = ptSnowNum(props.latest_swe_in);
    var wd = ptSnowWaterDayFromDate(props.latest_swe_date_local);
    var reportCurrent = ptSnowReportIsCurrent(props);
    var match = null;
    (pctRows || []).forEach(function(r) {
      if (match) return;
      if (ptSnowInt(r.water_day) === wd) match = r;
    });
    var rawContext = match ? ptSnowContextLabel(latestSwe, match.p10_swe_in, match.p30_swe_in, match.p70_swe_in, match.p90_swe_in, ptSnowBool(match.context_ok)) : 'No context';
    var label = reportCurrent ? rawContext : 'No context';
    var unavailableDetail = label === 'No context' ? ptSnowContextUnavailableDetail(props, match, reportCurrent, latestSwe) : '';
    return {
      label: label,
      rawLabel: rawContext,
      isCurrent: reportCurrent,
      row: match,
      waterDay: wd,
      unavailableDetail: unavailableDetail
    };
  }

  function ptSnowCurrentSweStyle(props) {
    var swe = ptSnowNum(props.latest_swe_in);
    var current = ptSnowReportIsCurrent(props);
    if (!current || swe === null) {
      return {fill:'transparent', fillOpacity:1, isCurrent:false, label:'No current daily SWE', legendClass:'none', size:16};
    }
    if (swe <= 0) return {fill:'#DEEBF7', fillOpacity:0.92, isCurrent:true, label:'0 in', legendClass:'zero', size:16};
    if (swe < 2) return {fill:'#C6DBEF', fillOpacity:0.92, isCurrent:true, label:'>0–2 in', legendClass:'low', size:16};
    if (swe < 10) return {fill:'#6BAED6', fillOpacity:0.92, isCurrent:true, label:'2–10 in', legendClass:'moderate', size:16};
    if (swe < 30) return {fill:'#2171B5', fillOpacity:0.92, isCurrent:true, label:'10–30 in', legendClass:'high', size:16};
    return {fill:'#08306B', fillOpacity:0.92, isCurrent:true, label:'≥30 in', legendClass:'very_high', size:16};
  }

  function ptSnowDeltaFieldForMode(mode) {
    if (mode === 'delta1') return 'swe_delta_1day_in';
    if (mode === 'delta3') return 'swe_delta_3day_in';
    if (mode === 'delta7') return 'swe_delta_7day_in';
    return '';
  }

  function ptSnowDeltaLabelForMode(mode) {
    if (mode === 'delta1') return '1-day \u0394SWE';
    if (mode === 'delta3') return '3-day \u0394SWE';
    if (mode === 'delta7') return '7-day \u0394SWE';
    return '\u0394SWE';
  }

  function ptSnowDeltaStyle(props, mode) {
    var field = ptSnowDeltaFieldForMode(mode);
    var delta = field ? ptSnowNum(props[field]) : null;
    var current = ptSnowReportIsCurrent(props);
    if (!current || delta === null) {
      return {fill:'transparent', fillOpacity:1, isCurrent:false, label:'No current \u0394SWE', legendClass:'none', size:16};
    }
    if (delta <= -3) return {fill:'#8C510A', fillOpacity:0.9, isCurrent:true, label:'≤−3 in', legendClass:'large_loss', size:16};
    if (delta <= -1) return {fill:'#D8B365', fillOpacity:0.9, isCurrent:true, label:'−3 to −1 in', legendClass:'loss', size:16};
    if (delta < -0.1) return {fill:'#F6E8C3', fillOpacity:0.9, isCurrent:true, label:'−1 to −0.1 in', legendClass:'small_loss', size:16};
    if (delta <= 0.1) return {fill:'#F7F7F7', fillOpacity:0.88, isCurrent:true, label:'±0.1 in', legendClass:'flat', size:16};
    if (delta < 1) return {fill:'#D1E5F0', fillOpacity:0.9, isCurrent:true, label:'+0.1 to +1 in', legendClass:'small_gain', size:16};
    if (delta < 3) return {fill:'#67A9CF', fillOpacity:0.9, isCurrent:true, label:'+1 to +3 in', legendClass:'gain', size:16};
    return {fill:'#2166AC', fillOpacity:0.9, isCurrent:true, label:'≥+3 in', legendClass:'large_gain', size:16};
  }

  function ptSnowStyleForMode(props, contextObj, mode) {
    mode = mode || 'context';
    if (mode === 'current') return ptSnowCurrentSweStyle(props);
    if (mode === 'delta1' || mode === 'delta3' || mode === 'delta7') return ptSnowDeltaStyle(props, mode);
    var label = contextObj && contextObj.label ? contextObj.label : 'No context';
    return {
      fill: ptSnowContextFill(label),
      fillOpacity: ptSnowContextFillOpacity(label),
      isCurrent: contextObj ? contextObj.isCurrent : false,
      label: label || 'No context',
      legendClass: label || 'No context',
      size: 16
    };
  }

  function ptSnowModeHoverLine(props, contextLabel, mode, styleObj) {
    mode = mode || 'context';
    if (mode === 'current') return '';
    if (mode === 'delta1' || mode === 'delta3' || mode === 'delta7') {
      var field = ptSnowDeltaFieldForMode(mode);
      var delta = ptSnowNum(props[field]);
      var label = ptSnowDeltaLabelForMode(mode);
      return label + ': ' + (delta === null || !ptSnowReportIsCurrent(props) ? 'no current \u0394SWE' : ((delta > 0 ? '+' : '') + ptSnowFmt(delta, 1) + ' in'));
    }
    return 'Context: ' + (contextLabel || 'No context');
  }

  function ptSnowHoverHtml(stationName, props, contextLabel, mode, styleObj, miniPlotHtml) {
    mode = mode || 'context';
    var swe = ptSnowNum(props.latest_swe_in);
    var latestLine = swe === null ?
      'Latest daily SWE: no valid current-WY SWE reported' :
      'Latest daily SWE: ' + ptSnowFmt(swe, 1) + '" - ' + ptSnowShortDate(props.latest_swe_date_local);
    if (ptSnowTrim(props.latest_swe_report_status) === 'stale_last_value') latestLine += ' (stale)';

    var statusLine = 'Status: ' + ptSnowEscape(ptSnowStatusLabel(props.latest_swe_report_status, props.latest_swe_staleness_class));
    var modeLine = ptSnowModeHoverLine(props, contextLabel, mode, styleObj);
    var html = '<div class="pt-ops-snow-hover-wrap"><strong>' + ptSnowEscape(stationName) + '</strong><br/>';

    if (mode === 'delta1' || mode === 'delta3' || mode === 'delta7') {
      html += ptSnowEscape(modeLine) + '<br/>' + latestLine + '<br/>' + statusLine;
    } else if (mode === 'current') {
      html += latestLine + '<br/>' + statusLine;
    } else {
      html += latestLine + '<br/>' + ptSnowEscape(modeLine) + '<br/>' + statusLine + (miniPlotHtml || '');
    }

    return html + '</div>';
  }

  function ptSnowSvgLine(rows, xName, yName, xf, yf) {
    var pts = [];
    (rows || []).forEach(function(r) {
      var x = ptSnowNum(r[xName]);
      var y = ptSnowNum(r[yName]);
      if (x === null || y === null) return;
      pts.push(Math.round(xf(x) * 10) / 10 + ',' + Math.round(yf(y) * 10) / 10);
    });
    return pts.join(' ');
  }

  function ptSnowSvgDateLine(rows, xName, yName, xf, yf) {
    var pts = [];
    (rows || []).forEach(function(r) {
      var d = r[xName] instanceof Date ? r[xName].getTime() : Date.parse(r[xName]);
      var y = ptSnowNum(r[yName]);
      if (isNaN(d) || y === null) return;
      pts.push(Math.round(xf(d) * 10) / 10 + ',' + Math.round(yf(y) * 10) / 10);
    });
    return pts.join(' ');
  }

  function ptSnowSvgDatePolylineBreaks(rows, xName, yName, xf, yf, attrs, maxGapDays) {
    var clean = (rows || []).filter(function(r) {
      var d = r[xName] instanceof Date ? r[xName].getTime() : Date.parse(r[xName]);
      return !isNaN(d) && ptSnowNum(r[yName]) !== null;
    }).sort(function(a, b) {
      var da = a[xName] instanceof Date ? a[xName].getTime() : Date.parse(a[xName]);
      var db = b[xName] instanceof Date ? b[xName].getTime() : Date.parse(b[xName]);
      return da - db;
    });
    if (clean.length < 2) return '';
    var pieces = [];
    var seg = [];
    var prev = null;
    clean.forEach(function(r) {
      var d = r[xName] instanceof Date ? r[xName].getTime() : Date.parse(r[xName]);
      if (prev !== null && (d - prev) / 86400000 > maxGapDays && seg.length) {
        pieces.push(seg);
        seg = [];
      }
      seg.push(r);
      prev = d;
    });
    if (seg.length) pieces.push(seg);
    return pieces.map(function(s) {
      if (s.length < 2) return '';
      var pts = ptSnowSvgDateLine(s, xName, yName, xf, yf);
      return pts ? '<polyline points="' + pts + '" ' + attrs + '/>' : '';
    }).join('');
  }

  function ptSnowRibbonPolygon(rows, lo, hi, xf, yf) {
    var clean = (rows || []).filter(function(r) {
      return ptSnowNum(r.water_day) !== null && ptSnowNum(r[lo]) !== null && ptSnowNum(r[hi]) !== null;
    }).sort(function(a, b) { return ptSnowNum(a.water_day) - ptSnowNum(b.water_day); });
    if (clean.length < 2) return '';
    var pts = [];
    clean.forEach(function(r) { pts.push(Math.round(xf(ptSnowNum(r.water_day)) * 10) / 10 + ',' + Math.round(yf(ptSnowNum(r[lo])) * 10) / 10); });
    clean.slice().reverse().forEach(function(r) { pts.push(Math.round(xf(ptSnowNum(r.water_day)) * 10) / 10 + ',' + Math.round(yf(ptSnowNum(r[hi])) * 10) / 10); });
    return pts.join(' ');
  }

  function ptSnowRibbonPolygonDate(rows, xName, lo, hi, xf, yf) {
    var clean = (rows || []).filter(function(r) {
      var d = r[xName] instanceof Date ? r[xName].getTime() : Date.parse(r[xName]);
      return !isNaN(d) && ptSnowNum(r[lo]) !== null && ptSnowNum(r[hi]) !== null;
    }).sort(function(a, b) {
      var da = a[xName] instanceof Date ? a[xName].getTime() : Date.parse(a[xName]);
      var db = b[xName] instanceof Date ? b[xName].getTime() : Date.parse(b[xName]);
      return da - db;
    });
    if (clean.length < 2) return '';
    var pts = [];
    clean.forEach(function(r) {
      var d = r[xName] instanceof Date ? r[xName].getTime() : Date.parse(r[xName]);
      pts.push(Math.round(xf(d) * 10) / 10 + ',' + Math.round(yf(ptSnowNum(r[lo])) * 10) / 10);
    });
    clean.slice().reverse().forEach(function(r) {
      var d = r[xName] instanceof Date ? r[xName].getTime() : Date.parse(r[xName]);
      pts.push(Math.round(xf(d) * 10) / 10 + ',' + Math.round(yf(ptSnowNum(r[hi])) * 10) / 10);
    });
    return pts.join(' ');
  }

  function ptSnowSmoothRows(rows, cols, k) {
    rows = (rows || []).slice();
    cols = cols || [];
    k = k || 7;
    var half = Math.floor(k / 2);
    cols.forEach(function(col) {
      var vals = rows.map(function(r) { return ptSnowNum(r[col]); });
      var sm = vals.map(function(v, idx) {
        var acc = [];
        for (var j = Math.max(0, idx - half); j <= Math.min(vals.length - 1, idx + half); j++) {
          if (vals[j] !== null) acc.push(vals[j]);
        }
        if (!acc.length) return null;
        return acc.reduce(function(a,b) { return a + b; }, 0) / acc.length;
      });
      rows.forEach(function(r, idx) { r[col] = sm[idx]; });
    });
    return rows;
  }

  function ptSnowPrepareTraceRows(props, traceRows) {
    var fetchStartTxt = ptSnowTrim(props.fetch_start_date);
    var fetchStartMs = fetchStartTxt ? Date.parse(fetchStartTxt + 'T00:00:00Z') : NaN;

    return (traceRows || []).map(function(r) {
      var obsTxt = ptSnowTrim(r.obs_date_local);
      var obsMs = obsTxt ? Date.parse(obsTxt + 'T00:00:00Z') : NaN;
      return {
        obs_date_local: obsTxt,
        obs_ms: obsMs,
        water_year: ptSnowInt(r.water_year),
        water_day: ptSnowInt(r.water_day) || ptSnowWaterDayFromDate(obsTxt),
        swe_in: ptSnowNum(r.swe_in)
      };
    }).filter(function(r) {
      if (!r.obs_date_local || isNaN(r.obs_ms) || r.water_day === null || r.swe_in === null || r.swe_in < 0) return false;
      if (!isNaN(fetchStartMs) && r.obs_ms < fetchStartMs) return false;
      return true;
    }).sort(function(a, b) { return a.water_day - b.water_day; });
  }

  function ptSnowPrepareDailyPercentileRows(pctRows) {
    var p = (pctRows || []).map(function(r) {
      return {
        water_day: ptSnowInt(r.water_day),
        p00_swe_in: ptSnowNum(r.p00_swe_in !== undefined ? r.p00_swe_in : r.p10_swe_in),
        p10_swe_in: ptSnowNum(r.p10_swe_in),
        p30_swe_in: ptSnowNum(r.p30_swe_in),
        p50_swe_in: ptSnowNum(r.p50_swe_in),
        p70_swe_in: ptSnowNum(r.p70_swe_in),
        p90_swe_in: ptSnowNum(r.p90_swe_in),
        p100_swe_in: ptSnowNum(r.p100_swe_in !== undefined ? r.p100_swe_in : r.p90_swe_in),
        n_years: ptSnowInt(r.n_years),
        context_ok: ptSnowBool(r.context_ok),
        daily_ref_label: ptSnowTrim(r.daily_ref_label),
        daily_ref_wy_label: ptSnowTrim(r.daily_ref_wy_label),
        years_min: ptSnowInt(r.years_min),
        years_max: ptSnowInt(r.years_max)
      };
    }).filter(function(r) { return r.water_day !== null; }).sort(function(a, b) { return a.water_day - b.water_day; });

    var pRibbon = p.filter(function(r) { return r.context_ok && r.n_years !== null && r.n_years >= 10; });
    pRibbon = ptSnowSmoothRows(pRibbon, ['p00_swe_in','p10_swe_in','p30_swe_in','p50_swe_in','p70_swe_in','p90_swe_in','p100_swe_in'], 7);

    return {
      all: p,
      ribbon: pRibbon,
      dailyContextOk: pRibbon.length >= 30
    };
  }

  function ptSnowPreparePriorWyFallbackRows(rows) {
    return (rows || []).map(function(r) {
      return {
        water_year: ptSnowInt(r.water_year),
        water_day: ptSnowInt(r.water_day),
        swe_in: ptSnowNum(r.swe_in),
        fallback_wy_rank: ptSnowInt(r.fallback_wy_rank)
      };
    }).filter(function(r) {
      return r.water_year !== null && r.water_day !== null && r.swe_in !== null && r.swe_in >= 0;
    }).sort(function(a, b) {
      if (a.water_year !== b.water_year) return a.water_year - b.water_year;
      return a.water_day - b.water_day;
    });
  }

  function ptSnowPriorWyTraceLines(rows, xf, yf, attrs) {
    if (!rows || !rows.length) return '';
    var byWy = {};
    rows.forEach(function(r) {
      var wy = String(r.water_year);
      if (!byWy[wy]) byWy[wy] = [];
      byWy[wy].push(r);
    });
    return Object.keys(byWy).sort().map(function(wy) {
      var seg = byWy[wy].sort(function(a, b) { return a.water_day - b.water_day; });
      if (seg.length < 2) return '';
      var pts = seg.map(function(r) {
        return Math.round(xf(r.water_day) * 10) / 10 + ',' + Math.round(yf(r.swe_in) * 10) / 10;
      }).join(' ');
      return '<polyline points="' + pts + '" ' + attrs + '/>';
    }).join('');
  }

  function ptSnowPriorWyApr1WaterDay(wy) {
    wy = ptSnowInt(wy);
    if (wy === null) return 183;
    var wyStart = Date.UTC(wy - 1, 9, 1); // Oct 1 of prior calendar year
    var apr1 = Date.UTC(wy, 3, 1);        // Apr 1 of water year calendar year
    return Math.round((apr1 - wyStart) / 86400000) + 1;
  }

  function ptSnowPickPriorWyLabelAnchor(seg, wy) {
    if (!seg || !seg.length) return null;
    var apr1Day = ptSnowPriorWyApr1WaterDay(wy);
    var near = seg.filter(function(r) {
      return r.water_day !== null && r.swe_in !== null && Math.abs(r.water_day - apr1Day) <= 15;
    }).sort(function(a, b) {
      var da = Math.abs(a.water_day - apr1Day);
      var db = Math.abs(b.water_day - apr1Day);
      if (da !== db) return da - db;
      return a.water_day - b.water_day;
    });
    if (near.length) return near[0];

    // Sparse traces may not bracket Apr 1.  If so, label the seasonal peak
    // rather than the late-season zero endpoint.
    return seg.slice().sort(function(a, b) {
      if (b.swe_in !== a.swe_in) return b.swe_in - a.swe_in;
      return a.water_day - b.water_day;
    })[0];
  }

  function ptSnowPriorWyLabelColor(wy, idx) {
    // Muted categorical palette for the fallback prior-WY label key.  Green
    // is intentionally reserved for the latest/current observation marker.
    // Traces remain gray; only the anchor dot and WY label use color.
    var palette = ['#D95F02', '#7570B3', '#E7298A', '#A6761D', '#1F78B4', '#B15928', '#E6AB02', '#6A3D9A'];
    var n = palette.length;
    var v = ptSnowInt(wy);
    var i = v === null ? idx : Math.abs(v);
    return palette[((i % n) + n) % n];
  }

  function ptSnowPriorWyAnchorLabels(rows, xf, yf, plotLeft, plotTop, plotW, plotH) {
    if (!rows || !rows.length) return '';
    var byWy = {};
    rows.forEach(function(r) {
      var wy = String(r.water_year);
      if (!byWy[wy]) byWy[wy] = [];
      byWy[wy].push(r);
    });

    function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }

    var right = plotLeft + plotW;
    var bottom = plotTop + plotH;
    var labels = Object.keys(byWy).sort().map(function(wy, idx) {
      var seg = byWy[wy].filter(function(r) {
        return r.water_day !== null && r.swe_in !== null;
      }).sort(function(a, b) { return a.water_day - b.water_day; });
      if (!seg.length) return null;
      var anchorPt = ptSnowPickPriorWyLabelAnchor(seg, wy);
      if (!anchorPt) return null;
      var x0 = Math.round(xf(anchorPt.water_day) * 10) / 10;
      var y0 = Math.round(yf(anchorPt.swe_in) * 10) / 10;
      var placeLeft = x0 > (right - 52);
      return {
        wy: ptSnowInt(wy),
        label: "WY'" + String(wy).slice(-2),
        color: ptSnowPriorWyLabelColor(wy, idx),
        x0: x0,
        y0: y0,
        x: placeLeft ? Math.max(plotLeft + 5, x0 - 7) : Math.min(right - 5, x0 + 7),
        y: clamp(y0 + 3, plotTop + 9, bottom - 4),
        anchor: placeLeft ? 'end' : 'start'
      };
    }).filter(function(d) { return d !== null; });

    if (!labels.length) return '';

    labels.sort(function(a, b) { return a.y - b.y; });
    var minGap = 10;
    for (var i = 1; i < labels.length; i++) {
      if (labels[i].y - labels[i - 1].y < minGap) labels[i].y = labels[i - 1].y + minGap;
    }
    var overflow = labels.length ? labels[labels.length - 1].y - (bottom - 4) : 0;
    if (overflow > 0) labels.forEach(function(d) { d.y = d.y - overflow; });
    labels.forEach(function(d) { d.y = clamp(d.y, plotTop + 9, bottom - 4); });

    return labels.map(function(d) {
      var labelX = Math.round(d.x * 10) / 10;
      var labelY = Math.round(d.y * 10) / 10;
      var leader = (Math.abs(labelY - d.y0) > 4 || Math.abs(labelX - d.x0) > 8) ?
        '<line x1="' + d.x0 + '" y1="' + d.y0 + '" x2="' + labelX + '" y2="' + (labelY - 3) + '" stroke="' + d.color + '" stroke-width="0.6" opacity="0.55"/>' : '';
      var dot = '<circle cx="' + d.x0 + '" cy="' + d.y0 + '" r="2.7" fill="' + d.color + '" stroke="#FFFFFF" stroke-width="1.1" opacity="0.96"/>';
      var text = '<text x="' + labelX + '" y="' + labelY + '" text-anchor="' + d.anchor + '" font-size="9" font-weight="650" stroke="#FAFAFA" stroke-width="2.3" paint-order="stroke" fill="' + d.color + '">' + d.label + '</text>';
      return dot + leader + text;
    }).join('');
  }

  function ptSnowPriorWyYears(rows) {
    var years = [];
    (rows || []).forEach(function(r) {
      if (r.water_year !== null && years.indexOf(r.water_year) < 0) years.push(r.water_year);
    });
    years.sort(function(a, b) { return a - b; });
    return years;
  }

  function ptSnowPriorWyLabel(rows) {
    var years = ptSnowPriorWyYears(rows);
    if (!years.length) return '';
    return 'WY' + years[0] + (years.length > 1 ? '–WY' + years[years.length - 1] : '');
  }

  function ptSnowPriorWyCount(rows) {
    return ptSnowPriorWyYears(rows).length;
  }

  function ptSnowBuildMiniDailyEnvelope(props, traceRows, pctRows, fallbackRows) {
    traceRows = ptSnowPrepareTraceRows(props, traceRows);
    if (!traceRows.length) return '';

    var daily = ptSnowPrepareDailyPercentileRows(pctRows);
    var pRibbon = daily.ribbon;
    var dailyContextOk = daily.dailyContextOk;
    var priorWyRows = dailyContextOk ? [] : ptSnowPreparePriorWyFallbackRows(fallbackRows);

    var latestDateTxt = ptSnowTrim(props.latest_swe_date_local);
    var latestDateMs = latestDateTxt ? Date.parse(latestDateTxt + 'T00:00:00Z') : NaN;

    var w = 260, h = 92, ml = 24, mr = 6, mt = 5, mb = 17;
    var plotW = w - ml - mr;
    var plotH = h - mt - mb;

    var vals = [];
    if (dailyContextOk) {
      pRibbon.forEach(function(r) {
        ['p00_swe_in','p10_swe_in','p30_swe_in','p50_swe_in','p70_swe_in','p90_swe_in','p100_swe_in'].forEach(function(c) {
          var n = ptSnowNum(r[c]);
          if (n !== null) vals.push(n);
        });
      });
    }
    if (!dailyContextOk) priorWyRows.forEach(function(r) { if (r.swe_in !== null) vals.push(r.swe_in); });
    traceRows.forEach(function(r) { if (r.swe_in !== null) vals.push(r.swe_in); });
    if (!vals.length) return '';

    var yMin = 0;
    var yMax = Math.ceil(Math.max.apply(null, vals) / 5) * 5;
    if (!isFinite(yMax) || yMax <= 0) yMax = 5;

    function xf(x) { return ml + ((x - 1) / 365) * plotW; }
    function yf(y) { return mt + plotH - ((y - yMin) / (yMax - yMin)) * plotH; }

    var ribbonDefs = [
      ['p00_swe_in','p10_swe_in','#EAD6B8'],
      ['p10_swe_in','p30_swe_in','#F4EAD8'],
      ['p30_swe_in','p70_swe_in','#ECECEC'],
      ['p70_swe_in','p90_swe_in','#DDEEF7'],
      ['p90_swe_in','p100_swe_in','#BFD7EA']
    ];
    var ribbons = '';
    if (dailyContextOk) {
      ribbonDefs.forEach(function(d) {
        var pts = ptSnowRibbonPolygon(pRibbon, d[0], d[1], xf, yf);
        if (pts) ribbons += '<polygon points="' + pts + '" fill="' + d[2] + '" opacity="0.9"/>';
      });
    }

    var priorWyLines = (!dailyContextOk && priorWyRows.length) ? ptSnowPriorWyTraceLines(priorWyRows, xf, yf, 'fill="none" stroke="#B8B8B8" stroke-width="0.8" opacity="0.65"') : '';
    var medianPts = dailyContextOk ? ptSnowSvgLine(pRibbon, 'water_day', 'p50_swe_in', xf, yf) : '';
    var medianLine = medianPts ? '<polyline points="' + medianPts + '" fill="none" stroke="#666666" stroke-width="0.9" stroke-dasharray="3 3"/>' : '';
    var tracePts = ptSnowSvgLine(traceRows, 'water_day', 'swe_in', xf, yf);
    var traceLine = tracePts ? '<polyline points="' + tracePts + '" fill="none" stroke="#111111" stroke-width="1.5"/>' : '';
    var latestCandidates = traceRows.slice().filter(function(r) {
      return isNaN(latestDateMs) || r.obs_ms <= latestDateMs;
    }).sort(function(a, b) { return a.obs_ms - b.obs_ms; });
    var latestPt = latestCandidates.length ? latestCandidates[latestCandidates.length - 1] : traceRows[traceRows.length - 1];
    var latestDot = latestPt ? '<circle cx="' + Math.round(xf(latestPt.water_day) * 10) / 10 + '" cy="' + Math.round(yf(latestPt.swe_in) * 10) / 10 + '" r="2.7" fill="#2CA25F" stroke="white" stroke-width="0.9"/>' : '';

    var tickVals = [0, yMax];
    var yText = tickVals.map(function(yy) {
      return '<text x="' + (ml - 4) + '" y="' + Math.round(yf(yy) + 3) + '" text-anchor="end" font-size="8" fill="#555">' + yy + '</text>';
    }).join('');
    var xTicks = [[1,'O'], [93,'J'], [183,'A'], [274,'J']].map(function(t) {
      var x = Math.round(xf(t[0]) * 10) / 10;
      return '<line x1="' + x + '" x2="' + x + '" y1="' + (mt + plotH) + '" y2="' + (mt + plotH + 3) + '" stroke="#555"/>' +
        '<text x="' + x + '" y="' + (mt + plotH + 12) + '" text-anchor="middle" font-size="8" fill="#555">' + t[1] + '</text>';
    }).join('');

    return '<div class="pt-ops-snow-hover-mini"><svg width="' + w + '" height="' + h + '" viewBox="0 0 ' + w + ' ' + h + '" xmlns="http://www.w3.org/2000/svg">' +
      '<rect x="0" y="0" width="' + w + '" height="' + h + '" fill="#FAFAFA" stroke="#D0D0D0" stroke-width="1"/>' +
      '<rect x="' + ml + '" y="' + mt + '" width="' + plotW + '" height="' + plotH + '" fill="#F7F7F7" stroke="#D0D0D0" stroke-width="1"/>' +
      ribbons + priorWyLines + yText + medianLine + traceLine + latestDot +
      '<line x1="' + ml + '" x2="' + (ml + plotW) + '" y1="' + (mt + plotH) + '" y2="' + (mt + plotH) + '" stroke="#555"/>' +
      xTicks +
      '</svg></div>';
  }

  function ptSnowBuildPlot(stationUid, stationName, props, traceRows, pctRows, monthlyRows, fallbackRows) {
    var fetchStartTxt = ptSnowTrim(props.fetch_start_date);
    var fetchStartMs = fetchStartTxt ? Date.parse(fetchStartTxt + 'T00:00:00Z') : NaN;
    var latestDateTxt = ptSnowTrim(props.latest_swe_date_local);
    var latestDateMs = latestDateTxt ? Date.parse(latestDateTxt + 'T00:00:00Z') : NaN;

    traceRows = (traceRows || []).map(function(r) {
      var obsTxt = ptSnowTrim(r.obs_date_local);
      var obsMs = obsTxt ? Date.parse(obsTxt + 'T00:00:00Z') : NaN;
      return {
        obs_date_local: obsTxt,
        obs_ms: obsMs,
        water_year: ptSnowInt(r.water_year),
        water_day: ptSnowInt(r.water_day) || ptSnowWaterDayFromDate(obsTxt),
        swe_in: ptSnowNum(r.swe_in)
      };
    }).filter(function(r) {
      if (!r.obs_date_local || isNaN(r.obs_ms) || r.water_day === null || r.swe_in === null || r.swe_in < 0) return false;
      // Some CDEC daily records can leak in from the day before the WY fetch
      // window and carry a water-day near Sep.  Drop anything before the
      // latest-feed fetch_start_date so the trace and green latest dot stay
      // in the current water year.
      if (!isNaN(fetchStartMs) && r.obs_ms < fetchStartMs) return false;
      return true;
    }).sort(function(a, b) { return a.water_day - b.water_day; });

    if (!traceRows.length) {
      return '<div class="pt-ops-snow-plot-note">No current-water-year SWE trace available for this station.</div>';
    }

    var currentWy = ptSnowCurrentWyFromTrace(traceRows);
    var currentWyLabel = currentWy ? ('WY ' + currentWy) : 'current WY';

    var p = (pctRows || []).map(function(r) {
      return {
        water_day: ptSnowInt(r.water_day),
        p00_swe_in: ptSnowNum(r.p00_swe_in !== undefined ? r.p00_swe_in : r.p10_swe_in),
        p10_swe_in: ptSnowNum(r.p10_swe_in),
        p30_swe_in: ptSnowNum(r.p30_swe_in),
        p50_swe_in: ptSnowNum(r.p50_swe_in),
        p70_swe_in: ptSnowNum(r.p70_swe_in),
        p90_swe_in: ptSnowNum(r.p90_swe_in),
        p100_swe_in: ptSnowNum(r.p100_swe_in !== undefined ? r.p100_swe_in : r.p90_swe_in),
        n_years: ptSnowInt(r.n_years),
        context_ok: ptSnowBool(r.context_ok),
        daily_ref_label: ptSnowTrim(r.daily_ref_label),
        daily_ref_wy_label: ptSnowTrim(r.daily_ref_wy_label),
        years_min: ptSnowInt(r.years_min),
        years_max: ptSnowInt(r.years_max)
      };
    }).filter(function(r) { return r.water_day !== null; }).sort(function(a, b) { return a.water_day - b.water_day; });

    var dailyContextMinYears = 10;
    var pRibbon = p.filter(function(r) { return r.context_ok && r.n_years !== null && r.n_years >= dailyContextMinYears; });
    var dailyContextOk = pRibbon.length >= 30;
    pRibbon = ptSnowSmoothRows(pRibbon, ['p00_swe_in','p10_swe_in','p30_swe_in','p50_swe_in','p70_swe_in','p90_swe_in','p100_swe_in'], 7);
    var priorWyRows = dailyContextOk ? [] : ptSnowPreparePriorWyFallbackRows(fallbackRows);
    var priorWyLabel = ptSnowPriorWyLabel(priorWyRows);
    var priorWyCount = ptSnowPriorWyCount(priorWyRows);

    var dailyYearsMax = null;
    var dailyYears = [];
    p.forEach(function(r) {
      if (r.n_years !== null) dailyYearsMax = dailyYearsMax === null ? r.n_years : Math.max(dailyYearsMax, r.n_years);
      if (r.years_min !== null) dailyYears.push(r.years_min);
      if (r.years_max !== null) dailyYears.push(r.years_max);
    });
    var dailyPoa = 'POA unavailable';
    if (p.length && p[0].daily_ref_label) {
      dailyPoa = p[0].daily_ref_label;
    } else if (dailyYears.length) {
      dailyPoa = 'WY' + Math.min.apply(null, dailyYears) + '–WY' + Math.max.apply(null, dailyYears);
    }

    var monthly = (monthlyRows || []).map(function(r) {
      var d = ptSnowMonthDate(r.water_year, r.water_month);
      return {
        month_date: d,
        water_year: ptSnowInt(r.water_year),
        water_month: ptSnowInt(r.water_month),
        n_daily_obs: ptSnowInt(r.n_daily_obs),
        monthly_min_daily_obs: ptSnowInt(r.monthly_min_daily_obs),
        monthly_mean_ok: ptSnowBool(r.monthly_mean_ok),
        swe_month_mean_in: ptSnowNum(r.swe_month_mean_in),
        monthly_ref_ok: ptSnowBool(r.monthly_ref_ok),
        monthly_ref_n_years: ptSnowInt(r.monthly_ref_n_years),
        min_years_for_monthly_context: ptSnowInt(r.min_years_for_monthly_context),
        monthly_ref_p30_swe_in: ptSnowNum(r.monthly_ref_p30_swe_in),
        monthly_ref_p50_swe_in: ptSnowNum(r.monthly_ref_p50_swe_in),
        monthly_ref_p70_swe_in: ptSnowNum(r.monthly_ref_p70_swe_in),
        monthly_ref_label: ptSnowTrim(r.monthly_ref_label)
      };
    }).filter(function(r) { return r.month_date && !isNaN(r.month_date.getTime()); }).sort(function(a, b) { return a.month_date - b.month_date; });

    var actualMonthly = monthly.filter(function(r) { return r.monthly_mean_ok && r.swe_month_mean_in !== null; });
    var monthlyMeanMinDays = 10;
    monthly.forEach(function(r) { if (r.monthly_min_daily_obs !== null) monthlyMeanMinDays = r.monthly_min_daily_obs; });

    var w = 500, h = 370, ml = 48, mr = 14, mt = 22;
    var contextH = actualMonthly.length >= 3 ? 92 : 0;
    var gap = contextH > 0 ? 16 : 0;
    var plotTop = contextH > 0 ? (mt + contextH + gap) : 42;
    var mb = 34;
    var plotH = h - plotTop - mb;
    var plotW = w - ml - mr;

    var vals = [];
    if (dailyContextOk) {
      pRibbon.forEach(function(r) { ['p00_swe_in','p10_swe_in','p30_swe_in','p50_swe_in','p70_swe_in','p90_swe_in','p100_swe_in'].forEach(function(c) { var n = ptSnowNum(r[c]); if (n !== null) vals.push(n); }); });
    }
    if (!dailyContextOk) priorWyRows.forEach(function(r) { if (r.swe_in !== null) vals.push(r.swe_in); });
    traceRows.forEach(function(r) { if (r.swe_in !== null) vals.push(r.swe_in); });
    if (!vals.length) vals = [0, 50];
    var yMin = Math.max(0, Math.floor(Math.min.apply(null, vals) / 5) * 5);
    var yMax = Math.ceil(Math.max.apply(null, vals) / 5) * 5;
    if (!isFinite(yMin) || !isFinite(yMax) || yMin === yMax) { yMin = 0; yMax = 50; }

    function xf(x) { return ml + ((x - 1) / 365) * plotW; }
    function yf(y) { return plotTop + plotH - ((y - yMin) / (yMax - yMin)) * plotH; }

    var ribbonDefs = [
      ['p00_swe_in','p10_swe_in','#EAD6B8'], ['p10_swe_in','p30_swe_in','#F4EAD8'], ['p30_swe_in','p70_swe_in','#ECECEC'], ['p70_swe_in','p90_swe_in','#DDEEF7'], ['p90_swe_in','p100_swe_in','#BFD7EA']
    ];
    var ribbons = '';
    if (dailyContextOk) {
      ribbonDefs.forEach(function(d) {
        var pts = ptSnowRibbonPolygon(pRibbon, d[0], d[1], xf, yf);
        if (pts) ribbons += '<polygon points="' + pts + '" fill="' + d[2] + '" opacity="0.95"/>';
      });
    }
    var priorWyLines = (!dailyContextOk && priorWyRows.length) ? ptSnowPriorWyTraceLines(priorWyRows, xf, yf, 'fill="none" stroke="#B8B8B8" stroke-width="1.1" opacity="0.72"') : '';
    var priorWyAnchorLabels = (!dailyContextOk && priorWyRows.length) ? ptSnowPriorWyAnchorLabels(priorWyRows, xf, yf, ml, plotTop, plotW, plotH) : '';
    var medianPts = dailyContextOk ? ptSnowSvgLine(pRibbon, 'water_day', 'p50_swe_in', xf, yf) : '';
    var medianLine = medianPts ? '<polyline points="' + medianPts + '" fill="none" stroke="#666666" stroke-width="1.2" stroke-dasharray="3 3"/>' : '';
    var tracePts = ptSnowSvgLine(traceRows, 'water_day', 'swe_in', xf, yf);
    var traceLine = tracePts ? '<polyline points="' + tracePts + '" fill="none" stroke="#111111" stroke-width="2.1"/>' : '';
    var latestCandidates = traceRows.slice().filter(function(r) {
      return isNaN(latestDateMs) || r.obs_ms <= latestDateMs;
    }).sort(function(a, b) { return a.obs_ms - b.obs_ms; });
    var latestPt = latestCandidates.length ? latestCandidates[latestCandidates.length - 1] : (traceRows.length ? traceRows[traceRows.length - 1] : null);
    var latestDot = latestPt ? '<circle cx="' + Math.round(xf(latestPt.water_day) * 10) / 10 + '" cy="' + Math.round(yf(latestPt.swe_in) * 10) / 10 + '" r="3.8" fill="#2CA25F" stroke="white" stroke-width="1"/>' : '';

    var yTicks = [];
    var step = Math.max(5, Math.ceil((yMax - yMin) / 4 / 5) * 5);
    for (var yy = yMin; yy <= yMax + 0.001; yy += step) yTicks.push(yy);
    var yAxis = yTicks.map(function(yy) { return '<text x="' + (ml - 6) + '" y="' + (Math.round(yf(yy) + 4)) + '" text-anchor="end" font-size="10" fill="#555">' + yy + '</text>'; }).join('');
    var monthTicks = [
      [1,'Oct'], [32,'Nov'], [62,'Dec'], [93,'Jan'], [124,'Feb'], [152,'Mar'], [183,'Apr'], [213,'May'], [244,'Jun'], [274,'Jul'], [305,'Aug'], [336,'Sep']
    ];
    var xAxis = monthTicks.map(function(t) { return '<line x1="' + Math.round(xf(t[0]) * 10) / 10 + '" x2="' + Math.round(xf(t[0]) * 10) / 10 + '" y1="' + (plotTop + plotH) + '" y2="' + (plotTop + plotH + 4) + '" stroke="#555"/>' + '<text x="' + Math.round(xf(t[0]) * 10) / 10 + '" y="' + (plotTop + plotH + 17) + '" text-anchor="middle" font-size="10" fill="#555">' + t[1] + '</text>'; }).join('');

    var contextSvg = '';
    var monthlyNote = 'actual monthly values shown; monthly reference unavailable.';
    var monthlyPoa = 'POA unavailable';
    if (actualMonthly.length >= 3) {
      var cx0 = ml, cy0 = mt + 16, ch = contextH - 34, cw = plotW;
      var minDate = actualMonthly[0].month_date;
      var maxDate = actualMonthly[actualMonthly.length - 1].month_date;
      var monthSeq = ptSnowMonthSequence(minDate, maxDate);
      var refByMonth = {};
      monthly.forEach(function(r) {
        if (!r.monthly_ref_ok || r.monthly_ref_p30_swe_in === null || r.monthly_ref_p50_swe_in === null || r.monthly_ref_p70_swe_in === null) return;
        var key = String(r.water_month);
        if (!refByMonth[key]) refByMonth[key] = r;
        if (r.monthly_ref_label) monthlyPoa = r.monthly_ref_label;
      });
      var refPlot = monthSeq.map(function(d) {
        var wm = ptSnowWaterMonthFromDateObj(d);
        var r = refByMonth[String(wm)];
        if (!r) return null;
        return {
          month_date: d,
          ref_p30: r.monthly_ref_p30_swe_in,
          ref_p50: r.monthly_ref_p50_swe_in,
          ref_p70: r.monthly_ref_p70_swe_in,
          ref_years: r.monthly_ref_n_years,
          min_years: r.min_years_for_monthly_context
        };
      }).filter(function(r) { return r !== null; });
      var expectedMonths = Math.max(1, monthSeq.length);
      var density = actualMonthly.length / expectedMonths;
      var densityThreshold = 0.60;
      var refOk = refPlot.length >= 3 && density >= densityThreshold;
      var cVals = [];
      actualMonthly.forEach(function(r) { if (r.swe_month_mean_in !== null) cVals.push(r.swe_month_mean_in); });
      if (refOk) refPlot.forEach(function(r) { [r.ref_p30, r.ref_p50, r.ref_p70].forEach(function(v) { if (v !== null) cVals.push(v); }); });
      if (!cVals.length) cVals = [0, yMax];
      var cYMin = Math.max(0, Math.floor(Math.min.apply(null, cVals) / 5) * 5);
      var cYMax = Math.ceil(Math.max.apply(null, cVals) / 5) * 5;
      if (!isFinite(cYMin) || !isFinite(cYMax) || cYMin === cYMax) { cYMin = 0; cYMax = yMax; }
      function cxf(d) { var t = d instanceof Date ? d.getTime() : Number(d); return cx0 + ((t - minDate.getTime()) / Math.max(1, maxDate.getTime() - minDate.getTime())) * cw; }
      function cyf(y) { return cy0 + ch - ((y - cYMin) / (cYMax - cYMin)) * ch; }
      var refBand = '', refLine = '';
      if (refOk) {
        var bandPts = ptSnowRibbonPolygonDate(refPlot, 'month_date', 'ref_p30', 'ref_p70', cxf, cyf);
        if (bandPts) refBand = '<polygon points="' + bandPts + '" fill="#CFCFCF" opacity="0.55"/>';
        var refPts = ptSnowSvgDateLine(refPlot, 'month_date', 'ref_p50', cxf, cyf);
        if (refPts) refLine = '<polyline points="' + refPts + '" fill="none" stroke="#555555" stroke-width="1.2" stroke-dasharray="4 3"/>';
      }
      var actualLine = ptSnowSvgDatePolylineBreaks(actualMonthly, 'month_date', 'swe_month_mean_in', cxf, cyf, 'fill="none" stroke="#111111" stroke-width="1.6"', 45);
      var actualPoints = actualMonthly.map(function(r) {
        if (!r.month_date || r.swe_month_mean_in === null) return '';
        return '<circle cx="' + Math.round(cxf(r.month_date) * 10) / 10 + '" cy="' + Math.round(cyf(r.swe_month_mean_in) * 10) / 10 + '" r="1.7" fill="#111111" opacity="0.72"/>';
      }).join('');
      var years = [];
      actualMonthly.forEach(function(r) { var yr = r.month_date.getUTCFullYear(); if (years.indexOf(yr) < 0) years.push(yr); });
      var firstYear = years.length ? Math.min.apply(null, years) : null;
      var lastYear = years.length ? Math.max.apply(null, years) : null;
      var contextTitle = 'A. Monthly context';
      if (firstYear !== null && lastYear !== null) contextTitle += ' — showing ' + firstYear + '–' + lastYear;
      if (monthlyPoa && monthlyPoa !== 'POA unavailable') contextTitle += ' | POA ' + monthlyPoa;
      var labels = years.filter(function(yr) { return yr % 2 === 0; }).map(function(yr) {
        var xd = new Date(Date.UTC(yr, 0, 1));
        if (xd < minDate || xd > maxDate) return '';
        return '<text x="' + Math.round(cxf(xd) * 10) / 10 + '" y="' + (cy0 + ch + 11) + '" text-anchor="middle" font-size="9" fill="#666">' + yr + '</text>';
      }).join('');
      monthlyNote = (refOk ?
        'repeated monthly reference shown; retained monthly coverage is ' + Math.round(density * 100) + '% of months (threshold 60%).' :
        'monthly reference ribbon hidden because retained monthly coverage is sparse (' + Math.round(density * 100) + '% of months; threshold 60%).');
      contextSvg = '<text x="' + ml + '" y="' + (mt + 7) + '" font-size="10" fill="#333">' + ptSnowEscape(contextTitle) + '</text>' +
        '<rect x="' + cx0 + '" y="' + cy0 + '" width="' + cw + '" height="' + ch + '" fill="#F7F7F7" stroke="#D8D8D8" stroke-width="1"/>' +
        refBand + refLine + actualLine + actualPoints + labels +
        '<text x="' + (ml - 6) + '" y="' + Math.round(cyf(cYMin) + 3) + '" text-anchor="end" font-size="8" fill="#666">' + cYMin + '</text>' +
        '<text x="' + (ml - 6) + '" y="' + Math.round(cyf(cYMax) + 3) + '" text-anchor="end" font-size="8" fill="#666">' + cYMax + '</text>';
    }

    var dailyPoaTitle = (dailyPoa && dailyPoa !== 'POA unavailable') ? (' | POA ' + dailyPoa) : '';
    var dailyPlotTitle = dailyContextOk ?
      ('B. Daily envelope — ' + currentWyLabel + ' vs daily percentiles' + dailyPoaTitle) :
      (priorWyRows.length ?
        ('B. Daily context — ' + currentWyLabel + ' vs usable prior WY traces') :
        ('B. Daily trace — ' + currentWyLabel + ' (historical context limited)'));

    var dailyNote = dailyContextOk ?
      'ribbons = prior-year daily SWE percentile bands (min–10th, 10th–30th, 30th–70th, 70th–90th, 90th–max) drawn when ≥' + dailyContextMinYears + ' years support the water day; dashed=smoothed median; ribbon support varies by water day; max = ' + (dailyYearsMax === null ? 'NA' : dailyYearsMax + ' prior yrs') + '; ' + ptSnowStatsPoaText(dailyPoa, false) + '. Black=current WY; green dot=latest plotted observation. This is station-specific POA context, not a formal climatology.' :
      (priorWyRows.length ?
        'gray lines with colored Apr-1/peak markers/labels = ' + priorWyCount + ' usable prior WY trace' + (priorWyCount === 1 ? '' : 's') + (priorWyLabel ? ' (' + priorWyLabel + ')' : '') + '; shown instead of percentile ribbons because daily percentile ribbons require ≥' + dailyContextMinYears + ' years supporting the water day. ' + ptSnowStatsPoaText(dailyPoa, true) + '. Black=current WY; green dot=latest plotted observation.' :
        'current WY shown only; daily percentile envelope hidden because the station lacks daily history meeting the ≥' + dailyContextMinYears + '-year water-day threshold and no compact usable prior-WY fallback traces are available; ' + ptSnowStatsPoaText(dailyPoa, false) + '.');

    var monthlyMeanTxt = 'black = actual monthly mean SWE (months with ≥' + monthlyMeanMinDays + ' daily observations); points = retained months; line breaks indicate months not retained; ';
    var monthlyRefTxt = 'gray band/line = repeated historical monthly 30th–70th percentile and median when ≥10 years of data are available and retained monthly coverage meets threshold; ';
    var monthlyPoaTxt = ptSnowStatsPoaText(monthlyPoa, false) + '; ';

    return '<div class="pt-ops-snow-plot-shell">' +
      '<svg width="' + w + '" height="' + h + '" viewBox="0 0 ' + w + ' ' + h + '" xmlns="http://www.w3.org/2000/svg" style="max-width:100%; height:auto; background:#FAFAFA; border:1px solid #DDD;">' +
      '<rect x="0" y="0" width="' + w + '" height="' + h + '" fill="#FAFAFA"/>' +
      '<text x="10" y="14" font-size="12" font-weight="bold" fill="#222">' + ptSnowEscape(stationName) + ' — SWE</text>' +
      contextSvg +
      '<text x="' + ml + '" y="' + (plotTop - 6) + '" font-size="10" fill="#333">' + dailyPlotTitle + '</text>' +
      '<rect x="' + ml + '" y="' + plotTop + '" width="' + plotW + '" height="' + plotH + '" fill="#F7F7F7" stroke="#D0D0D0" stroke-width="1"/>' +
      ribbons + priorWyLines + priorWyAnchorLabels + yAxis + medianLine + traceLine + latestDot +
      '<line x1="' + ml + '" x2="' + (ml + plotW) + '" y1="' + (plotTop + plotH) + '" y2="' + (plotTop + plotH) + '" stroke="#555"/>' +
      xAxis +
      '<text transform="translate(13,' + (plotTop + plotH / 2) + ') rotate(-90)" text-anchor="middle" font-size="10" fill="#555">SWE (in)</text>' +
      '</svg>' +
      '<div class="pt-ops-snow-plot-note"><b>A. Monthly context:</b> ' + monthlyMeanTxt + monthlyRefTxt + monthlyPoaTxt + monthlyNote + '</div>' +
      '<div class="pt-ops-snow-plot-note"><b>' + (dailyContextOk ? 'B. Daily envelope — current WY vs daily percentiles:' : (priorWyRows.length ? 'B. Daily context — current WY vs usable prior WY traces:' : 'B. Daily trace — current WY:')) + '</b> ' + dailyNote + '</div>' +
      '<div class="pt-ops-snow-plot-note"><b>POA:</b> period of analysis used for reference statistics.</div>' +
      '</div>';
  }

  function ptSnowMakePopup(feature, traceRows, pctRows, monthlyRows, fallbackRows, contextObj) {
    var props = ptSnowGetFeatureProps(feature);
    var coords = ptSnowCoords(feature);
    var stationUid = ptSnowTrim(props.station_uid);
    var stationName = ptSnowTrim(props.station_name) || stationUid || 'Snow station';
    var providerId = ptSnowTrim(props.provider_station_id) || stationUid;
    var providerName = ptSnowTrim(props.provider) || ptSnowTrim(props.source_system) || 'Provider';
    var providerLine = providerName + ': ' + providerId;
    var elev = ptSnowNum(props.elevation_ft);
    var huc = ptSnowTrim(props.river_basin) || ptSnowTrim(props.huc) || 'HUC8 not assigned';
    var latestSwe = ptSnowNum(props.latest_swe_in);
    var latestText = latestSwe === null ? 'Not available' : (ptSnowFmt(latestSwe, 1) + '"');
    var status = ptSnowStatusLabel(props.latest_swe_report_status, props.latest_swe_staleness_class);
    var contextText = contextObj && contextObj.isCurrent ? contextObj.label : 'No current context';
    if (contextObj && !contextObj.isCurrent && contextObj.rawLabel && contextObj.rawLabel !== 'No context') {
      contextText += ' (latest valid context would be ' + contextObj.rawLabel + ')';
    }
    if (contextObj && contextObj.unavailableDetail && (contextText === 'No context' || contextText.indexOf('No current context') === 0)) {
      contextText += ' (' + contextObj.unavailableDetail + ')';
    }
    var contextDisplayText = ptSnowContextPopupText(contextObj, contextText);
    var providerDataSourceNote = ptSnowProviderDataSourceNote(props);
    var sourceLabel = ptSnowTrim(props.latest_swe_source_label);
    var medianRow = ptSnowMedianPopupRow(props);
    var officialUrl = ptSnowTrim(props.official_station_url) || ptSnowTrim(props.official_data_url) || ptSnowTrim(props.source_metadata_url);
    var plot = ptSnowBuildPlot(stationUid, stationName, props, traceRows, pctRows, monthlyRows, fallbackRows);

    return '<div class="pt-ops-snow-popup">' +
      '<div style="font-size:14px; font-weight:bold; margin-bottom:2px;">' + ptSnowEscape(stationName) + '</div>' +
      '<div style="font-size:12px; color:#555; margin-bottom:4px;">' + ptSnowEscape(providerLine) + (elev === null ? '' : ' · elev. ' + ptSnowFmt(elev, 0) + ' ft') + ' · ' + ptSnowEscape(huc) + '</div>' +
      '<table class="pt-ops-snow-status-table"><tbody>' +
      '<tr><td>Latest daily SWE</td><td><b>' + ptSnowEscape(latestText) + '</b> — ' + ptSnowEscape(ptSnowShortDate(props.latest_swe_date_local)) + '</td></tr>' +
      (sourceLabel ? '<tr><td>SWE source</td><td>' + ptSnowEscape(sourceLabel) + '</td></tr>' : '') +
      '<tr><td>Status</td><td>' + ptSnowEscape(status) + '</td></tr>' +
      '<tr><td>Historical context</td><td>' + ptSnowEscape(contextDisplayText) + '</td></tr>' +
      medianRow +
      '<tr><td>Period of record</td><td>' + ptSnowEscape(ptSnowTrim(props.period_of_record) || 'Not available') + '</td></tr>' +
      '</tbody></table>' +
      plot +
      '<div style="font-size:11px; color:#666; margin-top:4px;">SWE values are provider-reported daily observations. For BRIM display and context products, negative SWE and implausibly high SWE values are treated as invalid; current display values should be checked against the official source for decisions. User-facing dates use local date labels where provided.</div>' +
      (providerDataSourceNote ? '<div style="font-size:11px; color:#666; margin-top:3px;">' + ptSnowEscape(providerDataSourceNote) + '</div>' : '') +
      (officialUrl ? '<div style="font-size:12px; margin-top:5px;"><a href="' + ptSnowEscape(officialUrl) + '" target="_blank" rel="noopener noreferrer">Open official station/source page</a></div>' : '') +
      '</div>';
  }

  function makeSnowPillowLatestLayer(options) {
    options = options || {};
    var layer = L.layerGroup();
    layer.options = options;
    layer._ptSnowLayerName = options.name || 'Snow pillows / SWE Latest';
    layer._ptSnowMarkers = [];
    layer._ptSnowLegendCtl = null;
    layer._ptSnowLegendDiv = null;
    layer._ptSnowMode = 'context';
    layer._ptSnowProviderFilter = 'all';
    layer._ptSnowProviderCounts = {all: 0, cdec: 0, snotel: 0};
    layer._ptSnowIsRemoved = true;
    layer._ptSnowMap = null;

    layer.onAdd = function(mapObj) {
      L.LayerGroup.prototype.onAdd.call(this, mapObj);
      this._ptSnowMap = mapObj;
      this._ptSnowIsRemoved = false;
      this.clearLayers();
      this._ptSnowMarkers = [];
      ptSnowEnsureStyle();

      var name = this._ptSnowLayerName;
      setOpsLayerLoading(name, true);
      recordStatus(name, 'Loading BRIM-hosted snow-pillow / SWE feed…', 'pt-ops-warn');
      activeLegendDefs[name] = {
        note: options.note || 'BRIM-hosted snow-pillow/SWE latest values and station-specific historical context. Point colors show current SWE relative to station historical context only when the latest SWE value is fresh/current.',
        sourceUrl: options.sourceUrl || options.latestUrl || '',
        infoUrl: options.summaryUrl || options.sourceUrl || options.latestUrl || '',
        infoLabel: options.summaryUrl ? 'summary' : 'GeoJSON'
      };
      redrawLegend();

      var self = this;
      var latestPromise = fetch(options.latestUrl, {cache: 'no-cache'}).then(function(resp) {
        if (!resp.ok) throw new Error('HTTP ' + resp.status + ' for latest GeoJSON');
        return resp.json();
      });
      function optionalText(url) {
        if (!url) return Promise.resolve('');
        return fetch(url, {cache: 'no-cache'}).then(function(resp) { return resp.ok ? resp.text() : ''; }).catch(function() { return ''; });
      }

      Promise.all([
        latestPromise,
        optionalText(options.traceUrl),
        optionalText(options.waterdayPercentilesUrl),
        optionalText(options.monthlyContextUrl),
        optionalText(options.priorWyFallbackTracesUrl),
        optionalText(options.summaryUrl)
      ]).then(function(parts) {
        if (self._ptSnowIsRemoved) return;
        self._ptSnowRender(parts[0], parts[1], parts[2], parts[3], parts[4], parts[5]);
      }).catch(function(err) {
        setOpsLayerLoading(name, false);
        recordStatus(name, 'Could not load snow-pillow / SWE feed: ' + err.message, 'pt-ops-warn');
      });
    };

    layer._ptSnowRender = function(geojson, traceCsv, pctCsv, monthlyCsv, fallbackCsv, summaryText) {
      var name = this._ptSnowLayerName;
      var features = geojson && geojson.features ? geojson.features : [];
      var traceRows = ptSnowCsvParse(traceCsv);
      var pctRows = ptSnowCsvParse(pctCsv);
      var monthlyRows = ptSnowCsvParse(monthlyCsv);
      var fallbackRows = ptSnowCsvParse(fallbackCsv);
      var traceByStation = ptSnowIndexRows(traceRows, 'station_uid');
      var pctByStation = ptSnowIndexRows(pctRows, 'station_uid');
      var monthlyByStation = ptSnowIndexRows(monthlyRows, 'station_uid');
      var fallbackByStation = ptSnowIndexRows(fallbackRows, 'station_uid');
      var self = this;
      this._ptSnowProviderCounts = {all: 0, cdec: 0, snotel: 0};

      features.forEach(function(feature) {
        var props = ptSnowGetFeatureProps(feature);
        var stationUid = ptSnowTrim(props.station_uid);
        var coords = ptSnowCoords(feature);
        if (!stationUid || coords.lng === null || coords.lat === null) return;
        var stationName = ptSnowTrim(props.station_name) || stationUid;
        var tr = traceByStation[stationUid] || [];
        var pct = pctByStation[stationUid] || [];
        var monthly = monthlyByStation[stationUid] || [];
        var fallback = fallbackByStation[stationUid] || [];
        var contextObj = ptSnowContextForFeature(props, pct);
        var styleObj = ptSnowStyleForMode(props, contextObj, self._ptSnowMode);
        var hover = ptSnowHoverHtml(stationName, props, contextObj.label, self._ptSnowMode, styleObj);
        var popup = ptSnowMakePopup(feature, tr, pct, monthly, fallback, contextObj);

        var marker = L.marker([coords.lat, coords.lng], {
          icon: ptSnowMarkerIcon(props, styleObj.fill, styleObj.fillOpacity, styleObj.isCurrent, styleObj.size),
          keyboard: false,
          riseOnHover: true
        });
        marker._ptSnowProps = props;
        marker._ptSnowStationName = stationName;
        marker._ptSnowContextObj = contextObj;
        marker._ptSnowTraceRows = tr;
        marker._ptSnowPctRows = pct;
        marker._ptSnowFallbackRows = fallback;
        marker._ptSnowProviderFilterKey = ptSnowProviderFilterKey(props);
        marker._ptSnowMiniPlotHtml = null;
        self._ptSnowProviderCounts.all += 1;
        if (marker._ptSnowProviderFilterKey === 'cdec') self._ptSnowProviderCounts.cdec += 1;
        if (marker._ptSnowProviderFilterKey === 'snotel') self._ptSnowProviderCounts.snotel += 1;
        marker.bindTooltip(hover, {className: 'pt-ops-snow-tooltip', direction: 'auto', opacity: 0.95});
        marker.on('mouseover', function() {
          var modeNow = self._ptSnowMode || 'context';
          var propsNow = marker._ptSnowProps || {};
          var ctxNow = marker._ptSnowContextObj || {label:'No context', isCurrent:false};
          var styleNow = ptSnowStyleForMode(propsNow, ctxNow, modeNow);
          var miniNow = '';
          if (modeNow === 'context') {
            if (marker._ptSnowMiniPlotHtml === null) {
              marker._ptSnowMiniPlotHtml = ptSnowBuildMiniDailyEnvelope(propsNow, marker._ptSnowTraceRows || [], marker._ptSnowPctRows || [], marker._ptSnowFallbackRows || []);
            }
            miniNow = marker._ptSnowMiniPlotHtml || '';
          }
          marker.setTooltipContent(ptSnowHoverHtml(marker._ptSnowStationName || stationName, propsNow, ctxNow.label, modeNow, styleNow, miniNow));
        });
        marker.bindPopup(popup, {maxWidth: 600, minWidth: 560, keepInView: true});
        self._ptSnowMarkers.push(marker);
        if (self._ptSnowProviderFilter === 'all' || self._ptSnowProviderFilter === marker._ptSnowProviderFilterKey) {
          marker.addTo(self);
        }
      });

      this._ptSnowAddLegend();
      setOpsLayerLoading(name, false);
      recordStatus(name, 'Loaded ' + features.length + ' snow-pillow/SWE stations; context rows: daily ' + pctRows.length.toLocaleString() + ', monthly ' + monthlyRows.length.toLocaleString() + ', fallback traces ' + fallbackRows.length.toLocaleString() + '.', 'pt-ops-ok');
    };

    layer._ptSnowModeLabel = function(mode) {
      if (mode === 'current') return 'Current daily SWE';
      if (mode === 'delta1') return '1-day \u0394';
      if (mode === 'delta3') return '3-day \u0394';
      if (mode === 'delta7') return '7-day \u0394';
      return 'Historical context';
    };

    layer._ptSnowLegendBodyHtml = function() {
      var mode = this._ptSnowMode || 'context';
      function modeButton(pair) {
        var active = pair[0] === mode ? ' active' : '';
        return '<button type="button" class="pt-ops-snow-mode-btn' + active + '" data-snow-mode="' + pair[0] + '">' + pair[1] + '</button>';
      }
      function providerButton(pair) {
        var filterNow = this._ptSnowProviderFilter || 'all';
        var active = pair[0] === filterNow ? ' active' : '';
        var counts = this._ptSnowProviderCounts || {all: 0, cdec: 0, snotel: 0};
        var n = counts[pair[0]] || 0;
        var label = pair[1] + (n ? ' (' + n.toLocaleString() + ')' : '');
        return '<button type="button" class="pt-ops-snow-provider-btn' + active + '" data-snow-provider="' + pair[0] + '">' + label + '</button>';
      }
      var primaryButtons = [
        ['context', 'Context'],
        ['current', 'Current']
      ].map(modeButton).join('');
      var deltaButtons = [
        ['delta1', '1-day &Delta;'],
        ['delta3', '3-day &Delta;'],
        ['delta7', '7-day &Delta;']
      ].map(modeButton).join('');
      var providerButtons = [
        ['all', 'Both'],
        ['cdec', 'CDEC'],
        ['snotel', 'SNOTEL']
      ].map(providerButton, this).join('');

      var legend = '';
      var note = '';
      if (mode === 'current') {
        legend = '<div class="pt-ops-snow-map-legend-title">Current daily SWE</div>' +
          '<div class="pt-ops-snow-legend-grid">' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#DEEBF7"></span>0 in</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#C6DBEF"></span>&gt;0–2 in</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#6BAED6"></span>2–10 in</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#2171B5"></span>10–30 in</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#08306B"></span>&ge;30 in</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:transparent; border:2px solid #777; box-sizing:border-box;"></span>No current</div>' +
          '</div>';
        note = 'Colors use latest fresh/current daily SWE. CDEC/CCSS = squares; NRCS/SNOTEL = circles.';
      } else if (mode === 'delta1' || mode === 'delta3' || mode === 'delta7') {
        legend = '<div class="pt-ops-snow-map-legend-title">' + this._ptSnowModeLabel(mode) + '</div>' +
          '<div class="pt-ops-snow-legend-grid">' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#8C510A"></span>&le;−3 in</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#D8B365"></span>−3 to −1</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#F6E8C3"></span>−1 to −0.1</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#F7F7F7"></span>&plusmn;0.1</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#D1E5F0"></span>+0.1 to +1</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#67A9CF"></span>+1 to +3</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#2166AC"></span>&ge;+3 in</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:transparent; border:2px solid #777; box-sizing:border-box;"></span>No current</div>' +
          '</div>';
        note = '\u0394 is from the latest daily SWE feed; stations without fresh/current data are hollow/dashed. CDEC/CCSS = squares; NRCS/SNOTEL = circles.';
      } else {
        legend = '<div class="pt-ops-snow-map-legend-title">Current SWE vs historical context</div>' +
          '<div class="pt-ops-snow-legend-grid">' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#8C510A"></span>Much below</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#D8B365"></span>Below</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#7F7F7F"></span>Near normal</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#92C5DE"></span>Above</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:#2166AC"></span>Much above</div>' +
          '<div class="pt-ops-snow-legend-row"><span style="background:transparent; border:2px solid #777; box-sizing:border-box;"></span>No current context</div>' +
          '</div>';
        note = 'Context bins use station/day historical percentiles. CDEC/CCSS = squares; NRCS/SNOTEL = circles.';
      }

      return '<div class="pt-ops-snow-titlebar"><div class="pt-ops-snow-control-title">SWE display</div><button type="button" class="pt-ops-snow-legend-close" data-snow-close="1" title="Hide this legend/control">&times;</button></div>' +
        '<div class="pt-ops-snow-mode-row">' + primaryButtons + '</div>' +
        '<div class="pt-ops-snow-mode-row">' + deltaButtons + '</div>' +
        '<div class="pt-ops-snow-provider-row"><span class="pt-ops-snow-provider-label">Provider</span>' + providerButtons + '</div>' +
        legend + '<div class="pt-ops-snow-legend-note">' + note + '</div>';
    };

    layer._ptSnowAttachLegendEvents = function() {
      var div = this._ptSnowLegendDiv;
      if (!div) return;
      var self = this;
      var closeBtn = div.querySelector('[data-snow-close="1"]');
      if (closeBtn) {
        closeBtn.addEventListener('click', function(e) {
          if (typeof L !== 'undefined' && L.DomEvent) L.DomEvent.stop(e);
          else if (e && e.preventDefault) { e.preventDefault(); e.stopPropagation(); }
          if (div) div.style.display = 'none';
        });
      }
      Array.prototype.forEach.call(div.querySelectorAll('.pt-ops-snow-mode-btn'), function(btn) {
        btn.addEventListener('click', function(e) {
          L.DomEvent.stop(e);
          var mode = btn.getAttribute('data-snow-mode') || 'context';
          self._ptSnowSetMode(mode);
        });
      });
      Array.prototype.forEach.call(div.querySelectorAll('.pt-ops-snow-provider-btn'), function(btn) {
        btn.addEventListener('click', function(e) {
          L.DomEvent.stop(e);
          var filter = btn.getAttribute('data-snow-provider') || 'all';
          self._ptSnowSetProviderFilter(filter);
        });
      });
    };

    layer._ptSnowUpdateLegendHtml = function() {
      if (!this._ptSnowLegendDiv) return;
      this._ptSnowLegendDiv.innerHTML = this._ptSnowLegendBodyHtml();
      this._ptSnowAttachLegendEvents();
    };

    layer._ptSnowUpdateMarkersForMode = function() {
      var mode = this._ptSnowMode || 'context';
      (this._ptSnowMarkers || []).forEach(function(marker) {
        var props = marker._ptSnowProps || {};
        var contextObj = marker._ptSnowContextObj || {label:'No context', isCurrent:false};
        var stationName = marker._ptSnowStationName || ptSnowTrim(props.station_name) || 'Snow station';
        var styleObj = ptSnowStyleForMode(props, contextObj, mode);
        marker.setIcon(ptSnowMarkerIcon(props, styleObj.fill, styleObj.fillOpacity, styleObj.isCurrent, styleObj.size));
        var hover = ptSnowHoverHtml(stationName, props, contextObj.label, mode, styleObj);
        if (marker.getTooltip && marker.getTooltip()) {
          marker.setTooltipContent(hover);
        } else {
          marker.bindTooltip(hover, {className:'pt-ops-snow-tooltip', direction:'auto', opacity:0.95});
        }
      });
    };

    layer._ptSnowApplyProviderFilter = function() {
      var filter = this._ptSnowProviderFilter || 'all';
      var self = this;
      (this._ptSnowMarkers || []).forEach(function(marker) {
        var keep = filter === 'all' || marker._ptSnowProviderFilterKey === filter;
        var onLayer = self.hasLayer && self.hasLayer(marker);
        if (keep && !onLayer) {
          try { marker.addTo(self); } catch(e) {}
        } else if (!keep && onLayer) {
          try { self.removeLayer(marker); } catch(e) {}
        }
      });
    };

    layer._ptSnowSetProviderFilter = function(filter) {
      if (['all','cdec','snotel'].indexOf(filter) < 0) filter = 'all';
      this._ptSnowProviderFilter = filter;
      this._ptSnowApplyProviderFilter();
      this._ptSnowUpdateLegendHtml();
      var counts = this._ptSnowProviderCounts || {all: 0, cdec: 0, snotel: 0};
      var visible = filter === 'all' ? counts.all : counts[filter];
      recordStatus(this._ptSnowLayerName, 'Snow-pillow/SWE provider filter: ' + ptSnowProviderFilterLabel(filter) + ' (' + (visible || 0).toLocaleString() + ' stations shown).', 'pt-ops-muted');
    };

    layer._ptSnowSetMode = function(mode) {
      if (['context','current','delta1','delta3','delta7'].indexOf(mode) < 0) mode = 'context';
      this._ptSnowMode = mode;
      this._ptSnowUpdateMarkersForMode();
      this._ptSnowApplyProviderFilter();
      this._ptSnowUpdateLegendHtml();
      recordStatus(this._ptSnowLayerName, 'Snow-pillow/SWE display mode: ' + this._ptSnowModeLabel(mode) + '.', 'pt-ops-muted');
    };

    layer._ptSnowAddLegend = function() {
      var mapObj = this._ptSnowMap;
      if (!mapObj) return;
      var layerRef = this;
      this._ptSnowLegendCtl = L.control({position: 'topleft'});
      this._ptSnowLegendCtl.onAdd = function() {
        var div = L.DomUtil.create('div', 'pt-ops-snow-legend-control leaflet-control');
        layerRef._ptSnowLegendDiv = div;
        layerRef._ptSnowUpdateLegendHtml();
        L.DomEvent.disableClickPropagation(div);
        L.DomEvent.disableScrollPropagation(div);
        return div;
      };
      this._ptSnowLegendCtl.addTo(mapObj);
    };

    layer.onRemove = function(mapObj) {
      this._ptSnowIsRemoved = true;
      try { this.clearLayers(); } catch(e) {}
      if (this._ptSnowLegendCtl && this._ptSnowMap) {
        try { this._ptSnowMap.removeControl(this._ptSnowLegendCtl); } catch(e) {}
      }
      this._ptSnowLegendCtl = null;
      this._ptSnowMarkers = [];
      this._ptSnowProviderCounts = {all: 0, cdec: 0, snotel: 0};
      delete activeLegendDefs[this._ptSnowLayerName];
      setOpsLayerLoading(this._ptSnowLayerName, false);
      redrawLegend();
      recordStatus(this._ptSnowLayerName, 'Layer turned off.', 'pt-ops-muted');
      L.LayerGroup.prototype.onRemove.call(this, mapObj);
      this._ptSnowMap = null;
    };

    layer.forceRemove = function(mapObj) {
      try { this.onRemove(mapObj || this._ptSnowMap); } catch(e) {}
    };

    return layer;
  }
)---"
}
