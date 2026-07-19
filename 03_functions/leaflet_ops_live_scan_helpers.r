# ==== leaflet_ops_live_scan_helpers.r ========================================
## RF067:
##   - Break Plot A actual monthly median lines across missing/unretained months
##     so sparse monthly records show gaps instead of long misleading connector
##     segments. Monthly points are still drawn for all retained months.
##
## RF065b:
##   - Normalize SCAN mature-reference thresholds to 7 years for Plot A monthly
##     reference bands and Plot B daily percentile envelopes.
##   - Use Plot B prior-WY fallback traces below that threshold when available,
##     and label percentile envelopes as station/depth reference periods rather
##     than formal climatology.
##   - Keep context-bin labels tied to p10/p30/p70/p90 while Plot B ribbons
##     continue to draw the full min/10/30/median/70/90/max envelope.
## RF066:
##   - Tighten Plot B daily-ribbon eligibility to require 200 supported
##     water-days at the 7-year threshold before drawing percentile ribbons.
##     Otherwise use usable prior-WY fallback traces when available.
##   - Replace terse "ref" title wording with "POA" and define POA in popup
##     notes as the period of analysis used for reference statistics.
##
## RF063b:
##   - Add Plot B usable prior-WY fallback traces for station/depth tabs where
##     daily percentile ribbons are hidden because the historical record is too
##     short or sparse.
##   - Keep fallback traces gray, but add non-green colored anchor markers and
##     compact WY labels so users can match labels to traces without confusing
##     prior years with the latest/current observation marker.
##   - Use a SCAN-specific label strategy: label near the current/latest water
##     day when possible, otherwise choose a nearby day with better trace
##     coverage/separation. Keep labels compact (WY'25).
##   - Make Plot A/B notes more dynamic and add percentile-bin criteria only
##     where real daily percentile context is being shown.
##
## RF062:
##   - Add compact Plot B-style hover mini plot for the currently selected
##     SCAN depth.  The hover plot is generated on hover and cached per
##     station/depth, keeping popup tabs and map-depth selector behavior intact.
##
## RF058:
##   - Plot A monthly reference band/median now repeats the monthly climatology
##     pattern across the display window instead of following sparse actual
##     monthly rows.
##
## RF057b:
##   - Move SCAN context legend to the upper-left legend slot rather than the
##     normal Leaflet control stack, so it does not collide with HUC fill and
##     other small left-side controls.
##   - Make popup depth tabs fall back to the first available station depth
##     when the currently selected map depth is not available at that station.
##
## PURPOSE:
##   SCAN soil-moisture-specific JavaScript helpers for the BRIM Ops Live panel.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r` and returns browser-side
##   JavaScript text for the BRIM-hosted SCAN soil-moisture live feed.
##
##   The daily GitHub feed supplies current/latest values and current-water-year
##   traces. Local preprocessing supplies compact historical context CSVs:
##     - daily water-day percentile ribbons for Plot B
##     - monthly recent-record context for Plot A
##
##   The browser downloads those static files only when the SCAN Ops Live layer
##   is turned on. The full daily-history RDS remains local and is not embedded
##   in the base BRIM HTML.
## ============================================================================

pt_ops_live_scan_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // SCAN soil moisture latest/context layer from BRIM-hosted GitHub files.
  // --------------------------------------------------------------------------

  function ptScanEnsureStyle() {
    if (document.getElementById('pt-ops-scan-style')) return;

    var style = document.createElement('style');
    style.id = 'pt-ops-scan-style';
    style.innerHTML = `
      .leaflet-tooltip.pt-ops-scan-tooltip {
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

      .pt-ops-scan-hover-mini {
        margin-top: 5px;
        width: 260px;
      }

      .pt-ops-scan-hover-mini svg {
        display: block;
        width: 260px;
        height: 92px;
      }

      .pt-ops-scan-popup {
        width: 630px;
        max-width: 630px;
        font: 12px/1.35 Arial, Helvetica, sans-serif;
      }

      .pt-ops-scan-popup a {
        color: #1f5e9c;
        text-decoration: none;
      }

      .pt-ops-scan-popup a:hover {
        text-decoration: underline;
      }

      .pt-ops-scan-depth-tab-row {
        display: flex;
        flex-wrap: wrap;
        gap: 4px;
        margin: 5px 0 6px 0;
      }

      .pt-ops-scan-depth-tab {
        border: 1px solid #B8B8B8;
        background: #F6F6F6;
        border-radius: 4px;
        padding: 3px 7px;
        font-size: 12px;
        cursor: pointer;
      }

      .pt-ops-scan-depth-tab.active {
        background: #FFFFFF;
        border-color: #333333;
        font-weight: bold;
        box-shadow: 0 1px 3px rgba(0,0,0,0.18);
      }

      .pt-ops-scan-depth-dot {
        display: inline-block;
        width: 9px;
        height: 9px;
        border-radius: 50%;
        margin-right: 4px;
        vertical-align: -1px;
      }

      .pt-ops-scan-depth-panel { display: none; }
      .pt-ops-scan-depth-panel.active { display: block; }

      .pt-ops-scan-plot-shell { margin-top: 4px; }
      .pt-ops-scan-plot-note {
        font-size: 10.5px;
        color: #555;
        line-height: 1.25;
        margin: 2px 2px 0 2px;
      }

      .pt-ops-scan-depth-control,
      .pt-ops-scan-legend-control {
        background: rgba(226, 238, 235, 0.96);
        border: 1px solid #AAA;
        border-radius: 5px;
        padding: 6px 8px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.25);
        font: 12px/1.2 Arial, Helvetica, sans-serif;
      }

      .pt-ops-scan-depth-control {
        min-width: 118px;
        /* Keep the SCAN depth chooser aligned with the SCAN map legend and
         * out of the left-side Draw / Measure control stack. */
        position: absolute !important;
        left: 96px !important;
        top: 116px !important;
        z-index: 10010;
      }
      .pt-ops-scan-legend-control {
        min-width: 190px;
        /* RF057b: keep the live SCAN context legend in the same
         * approximate upper-left map slot as the HUC legend.  It is
         * intentionally allowed to overlap the HUC legend if both are on;
         * that is visually busy but harmless and avoids pushing the small
         * controls down into the HUC fill / measure / external panels. */
        position: absolute !important;
        left: 96px !important;
        top: 6px !important;
        z-index: 10010;
      }
      .pt-ops-scan-control-title { font-weight: bold; margin-bottom: 4px; }
      .pt-ops-scan-legend-titlebar { display:flex; align-items:flex-start; justify-content:space-between; gap:8px; margin-bottom:4px; }
      .pt-ops-scan-legend-titlebar .pt-ops-scan-control-title { margin-bottom:0; }
      .pt-ops-scan-legend-close { border:0; background:transparent; color:#777; font:bold 16px/1 Arial, Helvetica, sans-serif; padding:0 1px; cursor:pointer; }
      .pt-ops-scan-legend-close:hover { color:#222; }
      .pt-ops-scan-depth-button-row { display: flex; align-items: center; gap: 3px; white-space: nowrap; }

      .pt-ops-scan-map-depth-btn {
        border: 1px solid #999;
        background: #F7F7F7;
        border-radius: 3px;
        padding: 1px 4px;
        font-size: 11px;
        line-height: 1.2;
        cursor: pointer;
      }

      .pt-ops-scan-map-depth-btn.active {
        background: #FFFFFF;
        border-color: #111;
        font-weight: bold;
        box-shadow: 0 1px 2px rgba(0,0,0,0.2);
      }

      .pt-ops-scan-map-legend-title { font-weight: bold; margin-bottom: 3px; font-size: 11px; }
      .pt-ops-scan-legend-grid { display: grid; grid-template-columns: 1fr 1fr; column-gap: 8px; row-gap: 2px; }
      .pt-ops-scan-legend-row { white-space: nowrap; font-size: 11px; }
      .pt-ops-scan-legend-row span {
        display: inline-block;
        width: 10px;
        height: 10px;
        border: 1px solid rgba(0,0,0,0.25);
        margin-right: 3px;
        vertical-align: -1px;
      }
    `;
    document.head.appendChild(style);
  }

  function ptScanTrim(value) {
    if (value === null || value === undefined) return '';
    var txt = String(value).trim();
    if (!txt || ['NA', 'NULL', 'NAN'].indexOf(txt.toUpperCase()) >= 0) return '';
    return txt;
  }

  function ptScanNum(value) {
    if (value === null || value === undefined || typeof value === 'boolean') return null;
    if (typeof value === 'string') {
      value = value.trim();
      if (!value || ['NA', 'NULL', 'NAN'].indexOf(value.toUpperCase()) >= 0) return null;
    }
    var n = Number(String(value).replace(/,/g, ''));
    return isFinite(n) ? n : null;
  }

  function ptScanInt(value) {
    var n = ptScanNum(value);
    return n === null ? null : Math.round(n);
  }

  function ptScanBool(value) {
    if (value === true) return true;
    if (value === false || value === null || value === undefined) return false;
    var txt = String(value).trim().toLowerCase();
    return txt === 'true' || txt === 't' || txt === '1' || txt === 'yes' || txt === 'y';
  }

  function ptScanFmt(value, digits) {
    var n = ptScanNum(value);
    if (n === null) return 'NA';
    return n.toLocaleString(undefined, {
      minimumFractionDigits: digits || 0,
      maximumFractionDigits: digits || 0
    });
  }

  function ptScanEscape(value) {
    return escapeHtml(value === null || value === undefined ? '' : value);
  }

  function ptScanContextFill(label) {
    label = ptScanTrim(label);
    if (label === 'Much below normal') return '#8C510A';
    if (label === 'Below normal') return '#D8B365';
    if (label === 'Near normal') return '#7F7F7F';
    if (label === 'Above normal') return '#92C5DE';
    if (label === 'Much above normal') return '#2166AC';
    return '#FFFFFF';
  }

  function ptScanContextFillOpacity(label) {
    label = ptScanTrim(label);
    return (!label || label === 'No context') ? 0 : 0.88;
  }

  function ptScanDepthColor(depth, depthStyleByDepth) {
    var key = String(Math.round(ptScanNum(depth) || 0));
    return (depthStyleByDepth[key] && depthStyleByDepth[key].depth_color_hex) ? depthStyleByDepth[key].depth_color_hex : '#333333';
  }

  function ptScanDepthLabel(depth, depthStyleByDepth) {
    var key = String(Math.round(ptScanNum(depth) || 0));
    return (depthStyleByDepth[key] && depthStyleByDepth[key].depth_label) ? depthStyleByDepth[key].depth_label : key + ' in';
  }

  function ptScanHoverHtml(stationName, depthLabel, smsPct, contextLabel, ageLabel, miniPlotHtml) {
    return '<div class="pt-ops-scan-hover-wrap"><strong>' + ptScanEscape(stationName) + '</strong><br/>' +
      ptScanEscape(depthLabel) + ': ' + ptScanEscape(ptScanFmt(smsPct, 1)) + '%<br/>' +
      'Context: ' + ptScanEscape(contextLabel || 'No context') + '<br/>' +
      'Age: ' + ptScanEscape(ageLabel || 'not available') +
      (miniPlotHtml || '') +
      '</div>';
  }

  function ptScanCsvParse(text) {
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

  function ptScanKey(siteCode, depth) {
    return String(siteCode) + '|' + String(Math.round(ptScanNum(depth) || 0));
  }

  function ptScanIndexRows(rows, siteCol, depthCol) {
    var out = {};
    (rows || []).forEach(function(r) {
      var site = ptScanTrim(r[siteCol || 'site_code']);
      var depth = ptScanNum(r[depthCol || 'depth_in']);
      if (!site || depth === null) return;
      var key = ptScanKey(site, depth);
      if (!out[key]) out[key] = [];
      out[key].push(r);
    });
    return out;
  }

  function ptScanGetFeatureProps(feature) {
    return feature && feature.properties ? feature.properties : {};
  }

  function ptScanCoords(feature) {
    var c = feature && feature.geometry && feature.geometry.coordinates ? feature.geometry.coordinates : [];
    return {lng: ptScanNum(c[0]), lat: ptScanNum(c[1])};
  }

  function ptScanSafeId() {
    return Array.prototype.slice.call(arguments).join('-').replace(/[^A-Za-z0-9_-]+/g, '-').replace(/-+/g, '-');
  }

  function ptScanWaterYearFromTrace(rows) {
    var wy = null;
    (rows || []).forEach(function(r) {
      var n = ptScanInt(r.water_year);
      if (n !== null && (wy === null || n > wy)) wy = n;
    });
    if (wy !== null) return wy;

    var latest = null;
    (rows || []).forEach(function(r) {
      var d = Date.parse(r.obs_date);
      if (!isNaN(d) && (latest === null || d > latest)) latest = d;
    });
    if (latest !== null) {
      var dt = new Date(latest);
      var yr = dt.getUTCFullYear();
      var mo = dt.getUTCMonth() + 1;
      return mo >= 10 ? yr + 1 : yr;
    }
    return null;
  }

  function ptScanBuildDepthLookup(features, traceRows, pctByKey, depthStyleByDepth) {
    var lookup = {};

    (features || []).forEach(function(feature) {
      var props = ptScanGetFeatureProps(feature);
      var site = ptScanTrim(props.site_code);
      if (!site) return;
      var stationName = ptScanTrim(props.station_name) || ptScanTrim(props.station_name_display) || ('Site ' + site);
      var vals = [];

      if (props.depth_values_json) {
        try {
          vals = JSON.parse(props.depth_values_json) || [];
        } catch(e) {
          vals = [];
        }
      }

      if (!Array.isArray(vals) || !vals.length) {
        var d = ptScanNum(props.display_depth_in);
        if (d !== null) {
          vals = [{
            depth_in: d,
            depth_label: props.display_depth_label || ptScanDepthLabel(d, depthStyleByDepth),
            sms_pct: props.display_sms_pct,
            context_label: props.display_context_label || 'No context',
            obs_age_days: props.display_obs_age_days,
            obs_age_label: props.display_status_label || '',
            obs_date: props.display_obs_date || '',
            vs_median_pct_points: props.display_vs_median_pct_points,
            n_years: props.display_context_n_years
          }];
        }
      }

      vals.forEach(function(v) {
        var depth = ptScanNum(v.depth_in);
        if (depth === null) return;
        var depthKey = String(Math.round(depth));
        var context = ptScanTrim(v.context_label) || 'No context';
        var ageDays = ptScanInt(v.obs_age_days);
        var ageLabel = ptScanTrim(v.obs_age_label) || (ageDays === null ? 'not available' : (ageDays <= 0 ? 'today' : (ageDays === 1 ? '1 day' : ageDays + ' days')));
        var depthLabel = ptScanTrim(v.depth_label) || ptScanDepthLabel(depth, depthStyleByDepth);
        var rec = {
          site_code: site,
          station_name: stationName,
          depth_in: depth,
          depth_key: depthKey,
          depth_label: depthLabel,
          sms_pct: ptScanNum(v.sms_pct),
          obs_date: ptScanTrim(v.obs_date),
          obs_age_label: ageLabel,
          context_label: context,
          marker_fill: ptScanContextFill(context),
          marker_fill_opacity: ptScanContextFillOpacity(context),
          vs_median_pct_points: ptScanNum(v.vs_median_pct_points),
          n_years: ptScanInt(v.n_years),
          depth_color_hex: ptScanDepthColor(depth, depthStyleByDepth)
        };
        if (!lookup[site]) lookup[site] = {};
        lookup[site][depthKey] = rec;
      });
    });

    return lookup;
  }

  function ptScanDepthsForSite(site, depthLookup, traceByKey, depthStyleRows) {
    var seen = {};
    if (depthLookup[site]) {
      Object.keys(depthLookup[site]).forEach(function(d) { seen[d] = true; });
    }
    Object.keys(traceByKey || {}).forEach(function(key) {
      var parts = key.split('|');
      if (parts[0] === String(site)) seen[String(Math.round(ptScanNum(parts[1]) || 0))] = true;
    });

    var order = {};
    (depthStyleRows || []).forEach(function(r, idx) {
      var d = ptScanNum(r.depth_in);
      if (d !== null) order[String(Math.round(d))] = ptScanInt(r.depth_order) || idx + 1;
    });

    return Object.keys(seen).sort(function(a, b) {
      var oa = order[a] || 999;
      var ob = order[b] || 999;
      if (oa !== ob) return oa - ob;
      return Number(a) - Number(b);
    });
  }

  function ptScanSvgLine(rows, xName, yName, xf, yf) {
    var pts = [];
    (rows || []).forEach(function(r) {
      var x = ptScanNum(r[xName]);
      var y = ptScanNum(r[yName]);
      if (x === null || y === null) return;
      pts.push(Math.round(xf(x) * 10) / 10 + ',' + Math.round(yf(y) * 10) / 10);
    });
    return pts.join(' ');
  }

  function ptScanSvgDateLine(rows, xName, yName, xf, yf) {
    var pts = [];
    (rows || []).forEach(function(r) {
      var d = Date.parse(r[xName]);
      var y = ptScanNum(r[yName]);
      if (isNaN(d) || y === null) return;
      pts.push(Math.round(xf(d) * 10) / 10 + ',' + Math.round(yf(y) * 10) / 10);
    });
    return pts.join(' ');
  }

  function ptScanSvgDateLineSegments(rows, xName, yName, xf, yf, maxGapDays) {
    var clean = [];
    (rows || []).forEach(function(r) {
      var d = Date.parse(r[xName]);
      var y = ptScanNum(r[yName]);
      if (isNaN(d) || y === null) return;
      clean.push({d: d, y: y});
    });

    clean.sort(function(a, b) { return a.d - b.d; });

    var maxGapMs = (maxGapDays || 45) * 24 * 60 * 60 * 1000;
    var segments = [];
    var current = [];
    var prevD = null;

    clean.forEach(function(r) {
      if (prevD !== null && (r.d - prevD) > maxGapMs) {
        if (current.length >= 2) segments.push(current);
        current = [];
      }

      current.push(Math.round(xf(r.d) * 10) / 10 + ',' + Math.round(yf(r.y) * 10) / 10);
      prevD = r.d;
    });

    if (current.length >= 2) segments.push(current);
    return segments.map(function(seg) { return seg.join(' '); });
  }

  function ptScanRibbonPolygon(rows, lo, hi, xf, yf) {
    var clean = (rows || []).filter(function(r) {
      return ptScanNum(r.water_day) !== null && ptScanNum(r[lo]) !== null && ptScanNum(r[hi]) !== null;
    }).sort(function(a, b) { return ptScanNum(a.water_day) - ptScanNum(b.water_day); });

    if (clean.length < 2) return '';

    var pts = [];
    clean.forEach(function(r) { pts.push(Math.round(xf(ptScanNum(r.water_day)) * 10) / 10 + ',' + Math.round(yf(ptScanNum(r[lo])) * 10) / 10); });
    clean.slice().reverse().forEach(function(r) { pts.push(Math.round(xf(ptScanNum(r.water_day)) * 10) / 10 + ',' + Math.round(yf(ptScanNum(r[hi])) * 10) / 10); });
    return pts.join(' ');
  }

  function ptScanFallbackLabelPalette(idx) {
    // Non-green palette: green/teal are reserved for latest/current cues and
    // the 8-inch SCAN depth color.  Prior-year fallback traces stay gray;
    // only anchor dots/labels use these colors.
    var colors = ['#D55E00', '#0072B2', '#CC79A7', '#E69F00', '#7B3294', '#A6761D', '#666666'];
    return colors[Math.abs(idx || 0) % colors.length];
  }

  function ptScanGroupFallbackTraces(rows, maxGroups) {
    var groups = {};
    (rows || []).forEach(function(r) {
      var wy = ptScanInt(r.water_year);
      var wd = ptScanInt(r.water_day);
      var v = ptScanNum(r.sms_pct);
      if (wy === null || wd === null || v === null) return;
      if (!groups[wy]) groups[wy] = [];
      groups[wy].push({
        water_year: wy,
        water_day: wd,
        sms_pct: v,
        obs_date: ptScanTrim(r.obs_date),
        trace_label: ptScanTrim(r.trace_label) || ("WY'" + String(wy).slice(-2)),
        trace_order: ptScanInt(r.trace_order),
        n_days_in_trace: ptScanInt(r.n_days_in_trace),
        water_day_span: ptScanInt(r.water_day_span),
        min_daily_rows_per_prior_wy: ptScanInt(r.min_daily_rows_per_prior_wy),
        min_waterday_span_per_prior_wy: ptScanInt(r.min_waterday_span_per_prior_wy),
        fallback_reason: ptScanTrim(r.fallback_reason)
      });
    });

    var out = Object.keys(groups).map(function(wy) {
      var pts = groups[wy].sort(function(a, b) { return a.water_day - b.water_day; });
      return {
        water_year: ptScanInt(wy),
        label: pts[0] && pts[0].trace_label ? pts[0].trace_label : ("WY'" + String(wy).slice(-2)),
        trace_order: pts[0] ? pts[0].trace_order : null,
        n_days_in_trace: pts[0] ? pts[0].n_days_in_trace : null,
        water_day_span: pts[0] ? pts[0].water_day_span : null,
        min_daily_rows_per_prior_wy: pts[0] ? pts[0].min_daily_rows_per_prior_wy : null,
        min_waterday_span_per_prior_wy: pts[0] ? pts[0].min_waterday_span_per_prior_wy : null,
        fallback_reason: pts[0] ? pts[0].fallback_reason : '',
        rows: pts
      };
    }).sort(function(a, b) {
      var ao = ptScanInt(a.trace_order), bo = ptScanInt(b.trace_order);
      if (ao !== null && bo !== null && ao !== bo) return ao - bo;
      return b.water_year - a.water_year;
    });

    maxGroups = maxGroups || 7;
    return out.slice(0, maxGroups);
  }

  function ptScanFallbackWyRange(groups) {
    var yrs = [];
    (groups || []).forEach(function(g) {
      var wy = ptScanInt(g.water_year);
      if (wy !== null) yrs.push(wy);
    });
    if (!yrs.length) return 'WY range unavailable';
    var lo = Math.min.apply(null, yrs);
    var hi = Math.max.apply(null, yrs);
    return lo === hi ? ('WY' + lo) : ('WY' + lo + '–WY' + hi);
  }

  function ptScanFallbackUsabilityText(groups) {
    var minDays = null;
    var minSpan = null;
    (groups || []).forEach(function(g) {
      if (minDays === null && g.min_daily_rows_per_prior_wy !== null) minDays = g.min_daily_rows_per_prior_wy;
      if (minSpan === null && g.min_waterday_span_per_prior_wy !== null) minSpan = g.min_waterday_span_per_prior_wy;
    });
    if (minDays !== null && minSpan !== null) {
      return 'usable = ≥' + minDays + ' daily observations and ≥' + minSpan + '-day water-year span';
    }
    if (minDays !== null) return 'usable = ≥' + minDays + ' daily observations';
    if (minSpan !== null) return 'usable = ≥' + minSpan + '-day water-year span';
    return 'usable = fragment-filtered prior WY traces';
  }

  function ptScanNearestTracePoint(rows, targetDay, windowDays) {
    var best = null;
    (rows || []).forEach(function(r) {
      var wd = ptScanInt(r.water_day);
      var v = ptScanNum(r.sms_pct);
      if (wd === null || v === null) return;
      var dist = Math.abs(wd - targetDay);
      if (windowDays !== null && windowDays !== undefined && dist > windowDays) return;
      if (!best || dist < best.dist) best = {row: r, dist: dist};
    });
    return best ? best.row : null;
  }

  function ptScanBestFallbackLabelDay(groups, preferredDay) {
    preferredDay = ptScanInt(preferredDay);
    if (preferredDay === null) preferredDay = 183;

    var candidates = [preferredDay, preferredDay - 21, preferredDay + 21, 62, 93, 124, 152, 183, 213, 244, 274, 305, 336]
      .filter(function(x) { return x >= 1 && x <= 365; });

    var seen = {};
    candidates = candidates.filter(function(x) {
      var key = String(Math.round(x));
      if (seen[key]) return false;
      seen[key] = true;
      return true;
    });

    var best = {day: preferredDay, score: -Infinity};
    candidates.forEach(function(day) {
      var vals = [];
      groups.forEach(function(g) {
        var p = ptScanNearestTracePoint(g.rows, day, 21);
        if (p) vals.push(ptScanNum(p.sms_pct));
      });
      if (!vals.length) return;
      var minVal = Math.min.apply(null, vals);
      var maxVal = Math.max.apply(null, vals);
      var sep = maxVal - minVal;
      var score = vals.length * 1000 + sep * 10 - Math.abs(day - preferredDay);
      if (score > best.score) best = {day: day, score: score};
    });
    return best.day;
  }

  function ptScanFallbackTraceSvg(groups, xf, yf, plotLeft, plotRight, yMin, yMax, preferredDay) {
    if (!groups || !groups.length) return '';

    var labelDay = ptScanBestFallbackLabelDay(groups, preferredDay);
    var lineSvg = '';
    var labels = [];

    groups.forEach(function(g, idx) {
      var pts = ptScanSvgLine(g.rows, 'water_day', 'sms_pct', xf, yf);
      if (pts) {
        lineSvg += '<polyline points="' + pts + '" fill="none" stroke="#8A8A8A" stroke-width="1.15" stroke-opacity="0.45"/>';
      }

      var lp = ptScanNearestTracePoint(g.rows, labelDay, 24) || ptScanNearestTracePoint(g.rows, labelDay, null);
      if (!lp) return;
      var x0 = xf(ptScanNum(lp.water_day));
      var y0 = yf(ptScanNum(lp.sms_pct));
      if (!isFinite(x0) || !isFinite(y0)) return;
      labels.push({
        label: g.label || ("WY'" + String(g.water_year).slice(-2)),
        color: ptScanFallbackLabelPalette(idx),
        x0: x0,
        y0: y0
      });
    });

    if (!labels.length) return lineSvg;

    labels.sort(function(a, b) { return a.y0 - b.y0; });

    var minY = yf(yMax) + 6;
    var maxY = yf(yMin) - 6;
    var minSep = 9;
    labels.forEach(function(l, idx) {
      var y = l.y0;
      if (idx > 0 && y < labels[idx - 1].yt + minSep) y = labels[idx - 1].yt + minSep;
      l.yt = Math.max(minY, Math.min(maxY, y));
    });
    for (var i = labels.length - 2; i >= 0; i--) {
      if (labels[i].yt > labels[i + 1].yt - minSep) {
        labels[i].yt = Math.max(minY, labels[i + 1].yt - minSep);
      }
    }

    var labelX = Math.min(plotRight - 31, Math.max(plotLeft + 8, xf(labelDay) + 8));
    var labelSvg = '';
    labels.forEach(function(l) {
      var cx = Math.round(l.x0 * 10) / 10;
      var cy = Math.round(l.y0 * 10) / 10;
      var tx = Math.round(labelX * 10) / 10;
      var ty = Math.round((l.yt + 3) * 10) / 10;
      labelSvg += '<circle cx="' + cx + '" cy="' + cy + '" r="2.7" fill="' + l.color + '" stroke="white" stroke-width="0.8"/>';
      var leader = Math.abs(l.yt - l.y0) > 3 || Math.abs(labelX - l.x0) > 10;
      if (leader) {
        labelSvg += '<line x1="' + cx + '" y1="' + cy + '" x2="' + Math.round((labelX - 2) * 10) / 10 + '" y2="' + Math.round(l.yt * 10) / 10 + '" stroke="' + l.color + '" stroke-width="0.7" stroke-opacity="0.62"/>';
      }
      labelSvg += '<text x="' + tx + '" y="' + ty + '" font-size="8.5" font-weight="bold" fill="' + l.color + '">' + ptScanEscape(l.label) + '</text>';
    });

    return lineSvg + labelSvg;
  }

  function ptScanBuildMiniDailyEnvelope(site, depth, traceByKey, pctByKey, depthStyleByDepth) {
    var depthKey = String(Math.round(ptScanNum(depth) || 0));
    var key = ptScanKey(site, depthKey);
    var depthColor = ptScanDepthColor(depthKey, depthStyleByDepth);

    var tr = (traceByKey[key] || []).map(function(r) {
      var obsTxt = ptScanTrim(r.obs_date);
      var obsMs = obsTxt ? Date.parse(obsTxt + 'T00:00:00Z') : NaN;
      return {
        obs_date: obsTxt,
        obs_ms: obsMs,
        water_year: ptScanInt(r.water_year),
        water_day: ptScanInt(r.water_day),
        sms_pct: ptScanNum(r.sms_pct)
      };
    }).filter(function(r) {
      return r.obs_date && !isNaN(r.obs_ms) && r.water_day !== null && r.sms_pct !== null;
    }).sort(function(a, b) { return a.water_day - b.water_day; });

    if (!tr.length) return '';

    var pctRowsForMini = pctByKey[key] || [];
    var dailyMinYears = 7;
    if (pctRowsForMini.length && ptScanInt(pctRowsForMini[0].min_years_for_context) !== null) {
      dailyMinYears = ptScanInt(pctRowsForMini[0].min_years_for_context);
    }
    var pRibbon = pctRowsForMini.map(function(r) {
      return {
        water_day: ptScanInt(r.water_day),
        p00: ptScanNum(r.p00 !== undefined ? r.p00 : r.p10),
        p10: ptScanNum(r.p10),
        p30: ptScanNum(r.p30),
        p50: ptScanNum(r.p50),
        p70: ptScanNum(r.p70),
        p90: ptScanNum(r.p90),
        p100: ptScanNum(r.p100 !== undefined ? r.p100 : r.p90),
        n_years: ptScanInt(r.n_years)
      };
    }).filter(function(r) {
      return r.water_day !== null && r.n_years !== null && r.n_years >= dailyMinYears;
    }).sort(function(a, b) { return a.water_day - b.water_day; });

    var dailyRibbonMinSupportedDays = 200;
    var dailyContextOk = pRibbon.length >= dailyRibbonMinSupportedDays;

    var w = 260, h = 92, ml = 24, mr = 6, mt = 5, mb = 17;
    var plotW = w - ml - mr;
    var plotH = h - mt - mb;

    var vals = [];
    if (dailyContextOk) {
      pRibbon.forEach(function(r) {
        ['p00','p10','p30','p50','p70','p90','p100'].forEach(function(c) {
          var n = ptScanNum(r[c]);
          if (n !== null) vals.push(n);
        });
      });
    }
    tr.forEach(function(r) { if (r.sms_pct !== null) vals.push(r.sms_pct); });
    if (!vals.length) return '';

    var yMin = Math.max(0, Math.floor(Math.min.apply(null, vals) / 5) * 5);
    var yMax = Math.ceil(Math.max.apply(null, vals) / 5) * 5;
    if (!isFinite(yMin) || !isFinite(yMax) || yMin === yMax) {
      yMin = 0;
      yMax = 50;
    }

    function xf(x) { return ml + ((x - 1) / 365) * plotW; }
    function yf(y) { return mt + plotH - ((y - yMin) / (yMax - yMin)) * plotH; }

    var ribbonDefs = [
      ['p00','p10','#EAD6B8'],
      ['p10','p30','#F4EAD8'],
      ['p30','p70','#ECECEC'],
      ['p70','p90','#DDEEF7'],
      ['p90','p100','#BFD7EA']
    ];

    var ribbons = '';
    if (dailyContextOk) {
      ribbonDefs.forEach(function(d) {
        var pts = ptScanRibbonPolygon(pRibbon, d[0], d[1], xf, yf);
        if (pts) ribbons += '<polygon points="' + pts + '" fill="' + d[2] + '" opacity="0.9"/>';
      });
    }

    var medianPts = dailyContextOk ? ptScanSvgLine(pRibbon, 'water_day', 'p50', xf, yf) : '';
    var medianLine = medianPts ? '<polyline points="' + medianPts + '" fill="none" stroke="#666666" stroke-width="0.9" stroke-dasharray="3 3"/>' : '';
    var tracePts = ptScanSvgLine(tr, 'water_day', 'sms_pct', xf, yf);
    var traceLine = tracePts ? '<polyline points="' + tracePts + '" fill="none" stroke="#111111" stroke-width="1.5"/>' : '';

    var latestPt = tr.slice().sort(function(a, b) { return a.obs_ms - b.obs_ms; }).slice(-1)[0];
    var latestDot = latestPt ? '<circle cx="' + Math.round(xf(latestPt.water_day) * 10) / 10 + '" cy="' + Math.round(yf(latestPt.sms_pct) * 10) / 10 + '" r="2.7" fill="' + depthColor + '" stroke="white" stroke-width="0.9"/>' : '';

    var tickVals = [yMin, yMax];
    var yText = tickVals.map(function(yy) {
      return '<text x="' + (ml - 4) + '" y="' + Math.round(yf(yy) + 3) + '" text-anchor="end" font-size="8" fill="#555">' + yy + '</text>';
    }).join('');

    var xTicks = [[1,'O'], [93,'J'], [183,'A'], [274,'J']].map(function(t) {
      var x = Math.round(xf(t[0]) * 10) / 10;
      return '<line x1="' + x + '" x2="' + x + '" y1="' + (mt + plotH) + '" y2="' + (mt + plotH + 3) + '" stroke="#555"/>' +
        '<text x="' + x + '" y="' + (mt + plotH + 12) + '" text-anchor="middle" font-size="8" fill="#555">' + t[1] + '</text>';
    }).join('');

    return '<div class="pt-ops-scan-hover-mini"><svg width="' + w + '" height="' + h + '" viewBox="0 0 ' + w + ' ' + h + '" xmlns="http://www.w3.org/2000/svg">' +
      '<rect x="0" y="0" width="' + w + '" height="' + h + '" fill="#FAFAFA" stroke="#D0D0D0" stroke-width="1"/>' +
      '<rect x="' + ml + '" y="' + mt + '" width="' + plotW + '" height="' + plotH + '" fill="#F7F7F7" stroke="#D0D0D0" stroke-width="1"/>' +
      ribbons + yText + medianLine + traceLine + latestDot +
      '<line x1="' + ml + '" x2="' + (ml + plotW) + '" y1="' + (mt + plotH) + '" y2="' + (mt + plotH) + '" stroke="#555"/>' +
      xTicks +
      '</svg></div>';
  }

  function ptScanBuildMarkerHoverHtml(marker, depth, depthLookup, traceByKey, pctByKey, depthStyleByDepth, includeMiniPlot) {
    depth = String(depth);
    var site = marker && marker._ptScanSite ? marker._ptScanSite : '';
    var stationName = marker && marker._ptScanStationName ? marker._ptScanStationName : (site ? ('Site ' + site) : 'SCAN station');
    var rec = site && depthLookup && depthLookup[site] ? depthLookup[site][depth] : null;

    if (!rec) {
      return ptScanHoverHtml(stationName, ptScanDepthLabel(depth, depthStyleByDepth), null, 'No context', 'not available', '');
    }

    var miniPlotHtml = '';
    if (includeMiniPlot) {
      if (!marker._ptScanMiniPlotCache) marker._ptScanMiniPlotCache = {};
      if (marker._ptScanMiniPlotCache[depth] === undefined) {
        marker._ptScanMiniPlotCache[depth] = ptScanBuildMiniDailyEnvelope(site, depth, traceByKey || {}, pctByKey || {}, depthStyleByDepth || {});
      }
      miniPlotHtml = marker._ptScanMiniPlotCache[depth] || '';
    }

    return ptScanHoverHtml(rec.station_name || stationName, rec.depth_label, rec.sms_pct, rec.context_label, rec.obs_age_label, miniPlotHtml);
  }

  function ptScanMonYear(dateTxt) {
    var d = new Date(dateTxt + 'T00:00:00Z');
    if (isNaN(d.getTime())) return 'NA';
    return d.toLocaleString(undefined, {month: 'short', year: 'numeric', timeZone: 'UTC'});
  }

  function ptScanMakePlot(site, stationName, depth, traceByKey, pctByKey, monthlyByKey, fallbackByKey, depthStyleByDepth) {
    var depthKey = String(Math.round(ptScanNum(depth) || 0));
    var key = ptScanKey(site, depthKey);
    var tr = (traceByKey[key] || []).slice().sort(function(a, b) { return (ptScanNum(a.water_day) || 0) - (ptScanNum(b.water_day) || 0); });
    var p = (pctByKey[key] || []).slice().sort(function(a, b) { return (ptScanNum(a.water_day) || 0) - (ptScanNum(b.water_day) || 0); });
    var monthly = (monthlyByKey[key] || []).slice().sort(function(a, b) { return Date.parse(a.month_date) - Date.parse(b.month_date); });
    var fallbackGroups = ptScanGroupFallbackTraces(fallbackByKey[key] || [], 7);

    var depthLabel = ptScanDepthLabel(depthKey, depthStyleByDepth);
    var depthColor = ptScanDepthColor(depthKey, depthStyleByDepth);

    if (!tr.length) {
      return '<div style="font-size:12px; color:#666; border-left:4px solid ' + depthColor + '; padding-left:6px;">No current-water-year plot data available for ' + ptScanEscape(depthLabel) + ' at this station.</div>';
    }

    var currentWy = ptScanWaterYearFromTrace(tr);
    var currentWyLabel = currentWy ? ('WY ' + currentWy) : 'current WY';

    var dailyMinYears = 7;
    if (p.length && ptScanInt(p[0].min_years_for_context) !== null) {
      dailyMinYears = ptScanInt(p[0].min_years_for_context);
    }
    var pRibbon = p.filter(function(r) { var n = ptScanInt(r.n_years); return n !== null && n >= dailyMinYears; });
    var dailyRibbonMinSupportedDays = 200;
    var dailyContextOk = pRibbon.length >= dailyRibbonMinSupportedDays;

    var dailyYearsMax = null;
    var dailyYearsMin = null;
    p.forEach(function(r) {
      var n = ptScanInt(r.n_years);
      if (n === null) return;
      if (dailyYearsMax === null || n > dailyYearsMax) dailyYearsMax = n;
      if (dailyYearsMin === null || n < dailyYearsMin) dailyYearsMin = n;
    });

    var analysisYears = [];
    p.forEach(function(r) {
      var a = ptScanInt(r.years_min);
      var b = ptScanInt(r.years_max);
      if (a !== null) analysisYears.push(a);
      if (b !== null) analysisYears.push(b);
    });
    var dailyPoa = analysisYears.length ? ('WY' + Math.min.apply(null, analysisYears) + '–WY' + Math.max.apply(null, analysisYears)) : 'POA unavailable';

    var w = 600, h = 350, ml = 46, mr = 14, mt = 22;
    var contextH = monthly.length >= 3 ? 86 : 0;
    var gap = contextH > 0 ? 14 : 0;
    var plotTop = mt + contextH + gap;
    var mb = 34;
    var plotH = h - plotTop - mb;
    var plotW = w - ml - mr;

    var fallbackMode = !dailyContextOk && fallbackGroups.length > 0;

    var vals = [];
    if (dailyContextOk) {
      pRibbon.forEach(function(r) { ['p00','p10','p30','p50','p70','p90','p100'].forEach(function(c) { var n = ptScanNum(r[c]); if (n !== null) vals.push(n); }); });
    } else if (fallbackMode) {
      fallbackGroups.forEach(function(g) { g.rows.forEach(function(r) { var n = ptScanNum(r.sms_pct); if (n !== null) vals.push(n); }); });
    }
    tr.forEach(function(r) { var n = ptScanNum(r.sms_pct); if (n !== null) vals.push(n); });
    if (!vals.length) vals = [0, 50];
    var yMin = Math.max(0, Math.floor(Math.min.apply(null, vals) / 5) * 5);
    var yMax = Math.ceil(Math.max.apply(null, vals) / 5) * 5;
    if (!isFinite(yMin) || !isFinite(yMax) || yMin === yMax) { yMin = 0; yMax = 50; }

    function xf(x) { return ml + ((x - 1) / 365) * plotW; }
    function yf(y) { return plotTop + plotH - ((y - yMin) / (yMax - yMin)) * plotH; }

    var ribbonDefs = [
      ['p00','p10','#EAD6B8'], ['p10','p30','#F4EAD8'], ['p30','p70','#ECECEC'], ['p70','p90','#DDEEF7'], ['p90','p100','#BFD7EA']
    ];
    var ribbons = '';
    if (dailyContextOk) {
      ribbonDefs.forEach(function(d) {
        var pts = ptScanRibbonPolygon(pRibbon, d[0], d[1], xf, yf);
        if (pts) ribbons += '<polygon points="' + pts + '" fill="' + d[2] + '" opacity="0.95"/>';
      });
    }

    var medianPts = dailyContextOk ? ptScanSvgLine(pRibbon, 'water_day', 'p50', xf, yf) : '';
    var medianLine = medianPts ? '<polyline points="' + medianPts + '" fill="none" stroke="#666666" stroke-width="1.2" stroke-dasharray="3 3"/>' : '';
    var tracePts = ptScanSvgLine(tr, 'water_day', 'sms_pct', xf, yf);
    var traceLine = tracePts ? '<polyline points="' + tracePts + '" fill="none" stroke="#111111" stroke-width="2.2"/>' : '';

    var latest = null;
    tr.forEach(function(r) {
      var d = Date.parse(r.obs_date);
      if (isNaN(d)) return;
      if (!latest || d > latest._d) { latest = r; latest._d = d; }
    });
    var latestDot = '';
    if (latest) {
      latestDot = '<circle cx="' + (Math.round(xf(ptScanNum(latest.water_day)) * 10) / 10) + '" cy="' + (Math.round(yf(ptScanNum(latest.sms_pct)) * 10) / 10) + '" r="3.5" fill="' + depthColor + '" stroke="white" stroke-width="1"/>';
    }

    var fallbackSvg = fallbackMode ?
      ptScanFallbackTraceSvg(fallbackGroups, xf, yf, ml, ml + plotW, yMin, yMax, latest ? ptScanNum(latest.water_day) : null) :
      '';

    var yTicks = [yMin, Math.round((yMin + yMax) / 2), yMax];
    var grid = yTicks.map(function(yy) {
      return '<text x="' + (ml - 6) + '" y="' + (Math.round((yf(yy) + 4) * 10) / 10) + '" text-anchor="end" font-size="10" fill="#555">' + yy + '</text>';
    }).join('');

    var months = [
      [1,'Oct'],[32,'Nov'],[62,'Dec'],[93,'Jan'],[124,'Feb'],[152,'Mar'],[183,'Apr'],[213,'May'],[244,'Jun'],[274,'Jul'],[305,'Aug'],[336,'Sep']
    ];
    var xAxis = months.map(function(mo) {
      var x = Math.round(xf(mo[0]) * 10) / 10;
      return '<line x1="' + x + '" x2="' + x + '" y1="' + (plotTop + plotH) + '" y2="' + (plotTop + plotH + 4) + '" stroke="#555"/>' +
        '<text x="' + x + '" y="' + (plotTop + plotH + 17) + '" text-anchor="middle" font-size="10" fill="#555">' + mo[1] + '</text>';
    }).join('');

    var contextSvg = '';
    var monthlyPoaNote = 'POA unavailable';
    var monthlyMinDays = null;
    var monthlyStatMinYears = 7;
    var monthlyRefShown = false;
    if (monthly.length >= 3) {
      monthlyMinDays = ptScanInt(monthly[0].min_days_per_month);
      monthlyStatMinYears = ptScanInt(monthly[0].context_stat_min_years) || monthlyStatMinYears;
      var cx0 = ml, cy0 = mt + 16, ch = contextH - 34, cw = plotW;
      var dates = monthly.map(function(r) { return Date.parse(r.month_date); }).filter(function(d) { return !isNaN(d); });
      var minDate = Math.min.apply(null, dates), maxDate = Math.max.apply(null, dates);

      function ptScanMonthStartMs(d) {
        var dt = new Date(d);
        return Date.UTC(dt.getUTCFullYear(), dt.getUTCMonth(), 1);
      }

      function ptScanAddMonthsMs(d, n) {
        var dt = new Date(d);
        return Date.UTC(dt.getUTCFullYear(), dt.getUTCMonth() + n, 1);
      }

      function ptScanCalendarMonthKey(d) {
        var dt = new Date(d);
        return String(dt.getUTCMonth() + 1);
      }

      var monthlyRefByMonth = {};
      monthly.forEach(function(r) {
        var cm = ptScanTrim(r.calendar_month);
        if (!cm) {
          var md = Date.parse(r.month_date);
          if (!isNaN(md)) cm = ptScanCalendarMonthKey(md);
        }
        var cmNum = ptScanNum(cm);
        if (cmNum === null) return;
        var cmKey = String(Math.round(cmNum));
        if (!ptScanBool(r.monthly_ref_ok) || ptScanNum(r.ref_p50) === null) return;
        if (!monthlyRefByMonth[cmKey]) {
          monthlyRefByMonth[cmKey] = {
            calendar_month: cmKey,
            ref_p30: r.ref_p30,
            ref_p50: r.ref_p50,
            ref_p70: r.ref_p70,
            ref_label: r.ref_label || ''
          };
        }
      });

      var monthlyRefPlot = [];
      var refStart = ptScanMonthStartMs(minDate);
      var refEnd = ptScanMonthStartMs(maxDate);
      for (var refDate = refStart; refDate <= refEnd; refDate = ptScanAddMonthsMs(refDate, 1)) {
        var ref = monthlyRefByMonth[ptScanCalendarMonthKey(refDate)];
        if (!ref) continue;
        monthlyRefPlot.push({
          month_date_ms: refDate,
          ref_p30: ref.ref_p30,
          ref_p50: ref.ref_p50,
          ref_p70: ref.ref_p70
        });
      }

      var cVals = [];
      monthly.forEach(function(r) {
        var n = ptScanNum(r.actual_sms_pct);
        if (n !== null) cVals.push(n);
      });
      monthlyRefPlot.forEach(function(r) {
        ['ref_p30','ref_p50','ref_p70'].forEach(function(c) { var n = ptScanNum(r[c]); if (n !== null) cVals.push(n); });
      });
      var cYMin = Math.max(0, Math.floor(Math.min.apply(null, cVals) / 5) * 5);
      var cYMax = Math.ceil(Math.max.apply(null, cVals) / 5) * 5;
      if (!isFinite(cYMin) || !isFinite(cYMax) || cYMin === cYMax) { cYMin = yMin; cYMax = yMax; }
      function cxf(d) { return cx0 + ((d - minDate) / Math.max(1, maxDate - minDate)) * cw; }
      function cyf(y) { return cy0 + ch - ((y - cYMin) / (cYMax - cYMin)) * ch; }

      var refBand = '', refLine = '';
      monthlyRefShown = monthlyRefPlot.length >= 3;
      if (monthlyRefShown) {
        var bandPts = [];
        monthlyRefPlot.forEach(function(r) { bandPts.push(Math.round(cxf(r.month_date_ms) * 10) / 10 + ',' + Math.round(cyf(ptScanNum(r.ref_p30)) * 10) / 10); });
        monthlyRefPlot.slice().reverse().forEach(function(r) { bandPts.push(Math.round(cxf(r.month_date_ms) * 10) / 10 + ',' + Math.round(cyf(ptScanNum(r.ref_p70)) * 10) / 10); });
        refBand = '<polygon points="' + bandPts.join(' ') + '" fill="#D9D9D9" opacity="0.42"/>';
        var refPts = monthlyRefPlot.map(function(r) {
          return Math.round(cxf(r.month_date_ms) * 10) / 10 + ',' + Math.round(cyf(ptScanNum(r.ref_p50)) * 10) / 10;
        }).join(' ');
        if (refPts) refLine = '<polyline points="' + refPts + '" fill="none" stroke="#666666" stroke-width="1.1" stroke-dasharray="4 3"/>';
      }

      var actualSegments = ptScanSvgDateLineSegments(monthly, 'month_date', 'actual_sms_pct', cxf, cyf, 45);
      var actualLine = actualSegments.map(function(pts) {
        return '<polyline points="' + pts + '" fill="none" stroke="#111111" stroke-width="1.7"/>';
      }).join('');
      var actualDots = monthly.map(function(r) {
        var d = Date.parse(r.month_date);
        var v = ptScanNum(r.actual_sms_pct);
        if (isNaN(d) || v === null) return '';
        return '<circle cx="' + (Math.round(cxf(d) * 10) / 10) + '" cy="' + (Math.round(cyf(v) * 10) / 10) + '" r="1.9" fill="#111111" opacity="0.82"/>';
      }).join('');

      var cLatest = null;
      monthly.forEach(function(r) {
        var d = Date.parse(r.month_date);
        if (isNaN(d)) return;
        if (!cLatest || d > cLatest._d) { cLatest = r; cLatest._d = d; }
      });
      var cLatestDot = '';
      if (cLatest) {
        cLatestDot = '<circle cx="' + (Math.round(cxf(Date.parse(cLatest.month_date)) * 10) / 10) + '" cy="' + (Math.round(cyf(ptScanNum(cLatest.actual_sms_pct)) * 10) / 10) + '" r="3" fill="' + depthColor + '" stroke="white" stroke-width="1"/>';
      }

      var firstYear = new Date(minDate).getUTCFullYear();
      var lastYear = new Date(maxDate).getUTCFullYear();
      var yearLabels = '';
      for (var yr = firstYear; yr <= lastYear; yr++) {
        if (yr % 2 !== 0) continue;
        var yd = Date.parse(yr + '-01-01T00:00:00Z');
        if (yd < minDate || yd > maxDate) continue;
        yearLabels += '<text x="' + (Math.round(cxf(yd) * 10) / 10) + '" y="' + (cy0 + ch + 11) + '" text-anchor="middle" font-size="9" fill="#666">' + yr + '</text>';
      }

      monthlyPoaNote = (monthly[0] && monthly[0].ref_label) ? monthly[0].ref_label : 'POA unavailable';
      var contextLabel = 'A. Monthly context — showing ' + firstYear + '–' + lastYear;
      if (monthlyRefShown && monthlyPoaNote !== 'POA unavailable') {
        contextLabel += ' | POA ' + monthlyPoaNote;
      }

      contextSvg = '<text x="' + ml + '" y="' + (mt + 7) + '" font-size="10" fill="#333">' + ptScanEscape(contextLabel) + '</text>' +
        '<rect x="' + cx0 + '" y="' + cy0 + '" width="' + cw + '" height="' + ch + '" fill="#F7F7F7" stroke="#D8D8D8" stroke-width="1"/>' +
        refBand + refLine + actualLine + actualDots + cLatestDot +
        '<text x="' + (ml - 6) + '" y="' + (Math.round((cyf(cYMin) + 3) * 10) / 10) + '" text-anchor="end" font-size="8" fill="#666">' + cYMin + '</text>' +
        '<text x="' + (ml - 6) + '" y="' + (Math.round((cyf(cYMax) + 3) * 10) / 10) + '" text-anchor="end" font-size="8" fill="#666">' + cYMax + '</text>' +
        yearLabels;
    }

    var lowerTitle = dailyContextOk ?
      ('B. Daily envelope — ' + currentWyLabel + ' vs daily percentiles | POA ' + dailyPoa) :
      (fallbackMode ?
        ('B. Daily context — ' + currentWyLabel + ' vs usable prior WY traces') :
        ('B. Daily trace — ' + currentWyLabel + ' (historical context limited)'));

    var contextBinNote = dailyContextOk ?
      ' Context bins: ≤10th = much below; >10th–30th = below; >30th–70th = near normal; >70th–90th = above; >90th = much above.' :
      '';

    var dailyNote = dailyContextOk ?
      ('ribbons = prior-year daily percentile bands (min–10th, 10th–30th, 30th–70th, 70th–90th, 90th–max) drawn when ≥' + dailyMinYears + ' years support the water day and ≥' + dailyRibbonMinSupportedDays + ' water-days are supported for this station/depth; dashed=median; ribbon support varies by water day; max = ' + (dailyYearsMax === null ? 'NA' : dailyYearsMax + ' prior yrs') + '. Black=current WY; depth-colored dot=latest plotted observation. This is a station/depth POA, not a formal climatology.' + contextBinNote) :
      (fallbackMode ?
        ('gray lines with colored anchor markers/labels = ' + fallbackGroups.length + ' usable prior WY traces (' + ptScanFallbackWyRange(fallbackGroups) + '; ' + ptScanFallbackUsabilityText(fallbackGroups) + '), shown instead of percentile ribbons because this station/depth has <' + dailyRibbonMinSupportedDays + ' water-days with ≥' + dailyMinYears + ' years of support. Black=current WY; depth-colored dot=latest plotted observation.') :
        ('current WY shown only; daily percentile envelope hidden because this station/depth has <' + dailyRibbonMinSupportedDays + ' water-days with ≥' + dailyMinYears + ' years of support and no compact usable prior-WY fallback traces were available; POA ' + dailyPoa + '.'));

    var monthlyNote = 'black = retained actual monthly median' +
      (monthlyMinDays === null ? '' : ' (months with ≥' + monthlyMinDays + ' daily observations)') +
      '; points = retained months; line breaks indicate months not retained; ' +
      (monthlyRefShown ?
        ('gray band/line = repeated historical monthly 30th–70th percentile and median when ≥' + monthlyStatMinYears + ' years of data are available') :
        ('gray monthly reference hidden because retained coverage does not meet the ≥' + monthlyStatMinYears + '-year threshold')) +
      '; POA ' + monthlyPoaNote + '; plot shows recent/available record for space.';

    var contextSummary = 'Monthly ref ' + (monthlyRefShown ? 'shown' : 'hidden') +
      '; daily ' + (dailyContextOk ? 'percentile envelope shown' : (fallbackMode ? ('uses ' + fallbackGroups.length + ' usable prior WY traces') : 'current WY only')) + '.';

    return '<div class="pt-ops-scan-plot-shell">' +
      '<svg width="' + w + '" height="' + h + '" viewBox="0 0 ' + w + ' ' + h + '" xmlns="http://www.w3.org/2000/svg" style="max-width:100%; height:auto; background:#FAFAFA; border:1px solid #DDD;">' +
      '<rect x="0" y="0" width="' + w + '" height="' + h + '" fill="#FAFAFA"/>' +
      '<text x="10" y="14" font-size="12" font-weight="bold" fill="#222">' + ptScanEscape(stationName) + ' — ' + ptScanEscape(depthLabel) + ' soil moisture</text>' +
      contextSvg +
      '<text x="' + ml + '" y="' + (plotTop - 6) + '" font-size="10" fill="#333">' + ptScanEscape(lowerTitle) + '</text>' +
      '<rect x="' + ml + '" y="' + plotTop + '" width="' + plotW + '" height="' + plotH + '" fill="#F7F7F7" stroke="#D0D0D0" stroke-width="1"/>' +
      ribbons + fallbackSvg + grid + medianLine + traceLine + latestDot +
      '<line x1="' + ml + '" x2="' + (ml + plotW) + '" y1="' + (plotTop + plotH) + '" y2="' + (plotTop + plotH) + '" stroke="#555"/>' +
      xAxis +
      '<text transform="translate(12,' + (plotTop + plotH / 2) + ') rotate(-90)" text-anchor="middle" font-size="10" fill="#555">Volumetric water content (%)</text>' +
      '</svg>' +
      '<div class="pt-ops-scan-plot-note"><b>Context summary:</b> ' + ptScanEscape(contextSummary) + '</div>' +
      '<div class="pt-ops-scan-plot-note"><b>A. Monthly context:</b> ' + ptScanEscape(monthlyNote) + '</div>' +
      '<div class="pt-ops-scan-plot-note"><b>B. Daily context:</b> ' + ptScanEscape(dailyNote) + '</div>' +
      '<div class="pt-ops-scan-plot-note"><b>POA:</b> period of analysis used for reference statistics.</div>' +
      '</div>';
  }

  function ptScanMakeDepthTable(site, depthLookup, monthlyByKey) {
    var rows = [];
    if (depthLookup[site]) {
      Object.keys(depthLookup[site]).forEach(function(d) { rows.push(depthLookup[site][d]); });
    }
    rows.sort(function(a, b) { return Number(a.depth_key) - Number(b.depth_key); });
    if (!rows.length) return '<div style="font-size:12px; color:#666;">No latest depth values available.</div>';

    var html = '<table style="font-size:12px; border-collapse:collapse;">' +
      '<thead><tr>' +
      '<th style="text-align:left; padding-right:6px;">Depth</th>' +
      '<th style="text-align:right; padding-right:8px;">Latest</th>' +
      '<th style="text-align:left; padding-right:8px;">Date</th>' +
      '<th style="text-align:left; padding-right:8px;">Age</th>' +
      '<th style="text-align:left; padding-right:8px;">Historical context</th>' +
      '<th style="text-align:left;">Record</th>' +
      '</tr></thead><tbody>';

    rows.forEach(function(r) {
      var key = ptScanKey(site, r.depth_key);
      var m = monthlyByKey[key] && monthlyByKey[key][0] ? monthlyByKey[key][0] : null;
      var record = m && m.record_label ? m.record_label : 'NA';
      var latestTxt = r.sms_pct === null ? 'NA' : (ptScanFmt(r.sms_pct, 1) + '%');
      var dateTxt = r.obs_date || 'NA';
      var contextTxt = r.context_label || 'No context';
      if (r.vs_median_pct_points !== null && r.vs_median_pct_points !== undefined) {
        var diff = ptScanNum(r.vs_median_pct_points);
        if (diff !== null) contextTxt += ' (' + (diff > 0 ? '+' : '') + ptScanFmt(diff, 1) + ' pts vs median)';
      }
      if (r.n_years !== null && r.n_years !== undefined) contextTxt += '<br><span style="color:#666;">' + ptScanEscape(r.n_years) + ' yrs</span>';

      html += '<tr>' +
        '<td style="white-space:nowrap; padding:1px 6px 1px 0;"><span style="display:inline-block; width:8px; height:8px; border-radius:50%; background:' + ptScanEscape(r.depth_color_hex || '#555555') + '; margin-right:4px;"></span>' + ptScanEscape(r.depth_label) + '</td>' +
        '<td style="text-align:right; white-space:nowrap; padding:1px 8px 1px 0;">' + ptScanEscape(latestTxt) + '</td>' +
        '<td style="white-space:nowrap; padding:1px 8px 1px 0;">' + ptScanEscape(dateTxt) + '</td>' +
        '<td style="white-space:nowrap; padding:1px 8px 1px 0;">' + ptScanEscape(r.obs_age_label || '') + '</td>' +
        '<td style="padding:1px 8px 1px 0;">' + contextTxt + '</td>' +
        '<td style="white-space:nowrap; padding:1px 0;">' + ptScanEscape(record) + '</td>' +
        '</tr>';
    });
    html += '</tbody></table>';
    return html;
  }

  function makeScanSoilMoistureLayer(options) {
    options = options || {};
    var layer = L.layerGroup();
    layer.options = options;
    layer._ptScanLayerName = options.name || 'SCAN Soil Moisture';
    layer._ptScanSelectedDepth = '8';
    layer._ptScanMarkers = [];
    layer._ptScanMarkerLookup = {};
    layer._ptScanDepthLookup = {};
    layer._ptScanTraceByKey = {};
    layer._ptScanPctByKey = {};
    layer._ptScanFallbackByKey = {};
    layer._ptScanDepthStyleByDepth = {};
    layer._ptScanLegendCtl = null;
    layer._ptScanDepthCtl = null;
    layer._ptScanIsRemoved = true;
    layer._ptScanMap = null;

    layer.onAdd = function(mapObj) {
      L.LayerGroup.prototype.onAdd.call(this, mapObj);
      this._ptScanMap = mapObj;
      this._ptScanIsRemoved = false;
      this.clearLayers();
      this._ptScanMarkers = [];
      this._ptScanMarkerLookup = {};
      ptScanEnsureStyle();

      var name = this._ptScanLayerName;
      setOpsLayerLoading(name, true);
      recordStatus(name, 'Loading BRIM-hosted SCAN soil-moisture feed…', 'pt-ops-warn');
      activeLegendDefs[name] = {
        note: options.note || 'BRIM-hosted SCAN soil-moisture latest values and station-specific historical context. Point colors show the selected soil-depth latest value relative to that station/depth historical context.',
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
        return fetch(url, {cache: 'no-cache'}).then(function(resp) {
          return resp.ok ? resp.text() : '';
        }).catch(function() { return ''; });
      }

      Promise.all([
        latestPromise,
        optionalText(options.traceUrl),
        optionalText(options.depthStyleUrl),
        optionalText(options.waterdayPercentilesUrl),
        optionalText(options.monthlyContextUrl),
        optionalText(options.priorWyFallbackTracesUrl),
        optionalText(options.summaryUrl)
      ]).then(function(parts) {
        if (self._ptScanIsRemoved) return;
        self._ptScanRender(parts[0], parts[1], parts[2], parts[3], parts[4], parts[5], parts[6]);
      }).catch(function(err) {
        setOpsLayerLoading(name, false);
        recordStatus(name, 'Could not load SCAN feed: ' + err.message, 'pt-ops-warn');
      });
    };

    layer._ptScanRender = function(geojson, traceCsv, depthStyleCsv, pctCsv, monthlyCsv, fallbackCsv, summaryText) {
      var name = this._ptScanLayerName;
      var features = geojson && geojson.features ? geojson.features : [];
      var traceRows = ptScanCsvParse(traceCsv);
      var depthStyleRows = ptScanCsvParse(depthStyleCsv);
      var pctRows = ptScanCsvParse(pctCsv);
      var monthlyRows = ptScanCsvParse(monthlyCsv);
      var fallbackRows = ptScanCsvParse(fallbackCsv);
      var depthStyleByDepth = {};
      depthStyleRows.forEach(function(r) {
        var d = ptScanNum(r.depth_in);
        if (d === null) return;
        depthStyleByDepth[String(Math.round(d))] = r;
      });

      var traceByKey = ptScanIndexRows(traceRows, 'site_code', 'depth_in');
      var pctByKey = ptScanIndexRows(pctRows, 'site_code', 'depth_in');
      var monthlyByKey = ptScanIndexRows(monthlyRows, 'site_code', 'depth_in');
      var fallbackByKey = ptScanIndexRows(fallbackRows, 'site_code', 'depth_in');
      var depthLookup = ptScanBuildDepthLookup(features, traceRows, pctByKey, depthStyleByDepth);
      this._ptScanDepthLookup = depthLookup;
      this._ptScanTraceByKey = traceByKey;
      this._ptScanPctByKey = pctByKey;
      this._ptScanFallbackByKey = fallbackByKey;
      this._ptScanDepthStyleByDepth = depthStyleByDepth;

      var availableDepths = {};
      Object.keys(depthLookup).forEach(function(site) { Object.keys(depthLookup[site]).forEach(function(d) { availableDepths[d] = true; }); });
      var depthOrder = {};
      depthStyleRows.forEach(function(r, idx) { var d = ptScanNum(r.depth_in); if (d !== null) depthOrder[String(Math.round(d))] = ptScanInt(r.depth_order) || idx + 1; });
      var depths = Object.keys(availableDepths).sort(function(a, b) {
        var oa = depthOrder[a] || 999, ob = depthOrder[b] || 999;
        if (oa !== ob) return oa - ob;
        return Number(a) - Number(b);
      });
      if (!depths.length) depths = ['8'];
      if (depths.indexOf(this._ptScanSelectedDepth) < 0) this._ptScanSelectedDepth = depths.indexOf('8') >= 0 ? '8' : depths[0];

      var self = this;
      features.forEach(function(feature) {
        var props = ptScanGetFeatureProps(feature);
        var site = ptScanTrim(props.site_code);
        var coords = ptScanCoords(feature);
        if (!site || coords.lng === null || coords.lat === null) return;
        var stationName = ptScanTrim(props.station_name) || ptScanTrim(props.station_name_display) || ('Site ' + site);
        var rec = depthLookup[site] && depthLookup[site][self._ptScanSelectedDepth] ? depthLookup[site][self._ptScanSelectedDepth] : null;
        var fill = rec ? rec.marker_fill : '#FFFFFF';
        var fillOpacity = rec ? rec.marker_fill_opacity : 0;
        var hover = rec ? ptScanHoverHtml(stationName, rec.depth_label, rec.sms_pct, rec.context_label, rec.obs_age_label) : ptScanHoverHtml(stationName, self._ptScanSelectedDepth + ' in', null, 'No context', 'not available');

        var depthTabs = ptScanDepthsForSite(site, depthLookup, traceByKey, depthStyleRows);
        // RF057b: if the selected map depth is not available at this station,
        // open the first available station depth instead.  Without this, the
        // popup can contain valid tab panels but none are active, which makes
        // it look like the plots disappeared.
        var siteActiveDepth = depthTabs.indexOf(self._ptScanSelectedDepth) >= 0 ?
          self._ptScanSelectedDepth :
          (depthTabs.length ? depthTabs[0] : self._ptScanSelectedDepth);

        var tabButtons = depthTabs.map(function(d) {
          var depthColor = ptScanDepthColor(d, depthStyleByDepth);
          var active = d === siteActiveDepth ? ' active' : '';
          var panelId = ptScanSafeId('pt-ops-scan-panel', site, d);
          return '<button type="button" class="pt-ops-scan-depth-tab' + active + '" data-depth="' + d + '" data-target="' + panelId + '"><span class="pt-ops-scan-depth-dot" style="background:' + depthColor + '"></span>' + ptScanEscape(ptScanDepthLabel(d, depthStyleByDepth)) + '</button>';
        }).join('');

        var tabPanels = depthTabs.map(function(d) {
          var active = d === siteActiveDepth ? ' active' : '';
          var panelId = ptScanSafeId('pt-ops-scan-panel', site, d);
          return '<div id="' + panelId + '" class="pt-ops-scan-depth-panel' + active + '" data-depth="' + d + '">' +
            ptScanMakePlot(site, stationName, d, traceByKey, pctByKey, monthlyByKey, fallbackByKey, depthStyleByDepth) + '</div>';
        }).join('');

        var countyState = [ptScanTrim(props.county), ptScanTrim(props.state)].filter(Boolean).join(', ');
        var elev = ptScanNum(props.elevation_ft);
        var siteUrl = ptScanTrim(props.site_page_url) || ptScanTrim(props.data_page_url) || ptScanTrim(props.source_url);
        var popup = '<div class="pt-ops-scan-popup">' +
          '<div style="font-size:14px; font-weight:bold; margin-bottom:2px;">' + ptScanEscape(stationName) + '</div>' +
          '<div style="font-size:12px; color:#555; margin-bottom:4px;">Station ' + ptScanEscape(site) + (countyState ? ' · ' + ptScanEscape(countyState) : '') + (elev === null ? '' : ' · elev. ' + ptScanFmt(elev, 0) + ' ft') + '</div>' +
          '<div style="font-size:12px; margin-bottom:5px;">Depth tabs below show latest value, recent monthly context, and current-water-year daily envelope for each available depth.</div>' +
          '<div style="margin-bottom:6px; font-size:12px;"><b>Latest by depth:</b><br>' + ptScanMakeDepthTable(site, depthLookup, monthlyByKey) + '</div>' +
          '<div class="pt-ops-scan-depth-tabs"><div class="pt-ops-scan-depth-tab-row">' + tabButtons + '</div>' + tabPanels + '</div>' +
          '<div style="font-size:11px; color:#666; margin-top:4px;">Depth tabs show available soil sensors. Where duplicate same-depth sensors exist, BRIM plots use the primary NRCS sensor rather than averaging duplicates. User-facing dates/times use America/Los_Angeles where provided.</div>' +
          (siteUrl ? '<div style="font-size:12px; margin-top:5px;"><a href="' + ptScanEscape(siteUrl) + '" target="_blank" rel="noopener noreferrer">Open official NRCS station page</a></div>' : '') +
          '</div>';

        var marker = L.circleMarker([coords.lat, coords.lng], {
          radius: 7,
          stroke: true,
          color: '#222222',
          weight: 1,
          fillColor: fill,
          fillOpacity: fillOpacity
        });
        marker._ptScanSite = site;
        marker._ptScanStationName = stationName;
        marker._ptScanMiniPlotCache = {};
        marker.bindTooltip(hover, {className: 'pt-ops-scan-tooltip', direction: 'auto', opacity: 0.95});
        marker.on('mouseover', function() {
          var depthNow = self._ptScanSelectedDepth || siteActiveDepth;
          marker.setTooltipContent(ptScanBuildMarkerHoverHtml(
            marker,
            depthNow,
            self._ptScanDepthLookup || {},
            self._ptScanTraceByKey || {},
            self._ptScanPctByKey || {},
            self._ptScanDepthStyleByDepth || {},
            true
          ));
        });
        marker.bindPopup(popup, {maxWidth: 660, minWidth: 630, keepInView: true});
        marker.addTo(self);
        self._ptScanMarkers.push(marker);
        self._ptScanMarkerLookup[site] = marker;
      });

      this._ptScanAddControls(depths);
      this._ptScanApplyDepth(this._ptScanSelectedDepth);
      setOpsLayerLoading(name, false);
      recordStatus(name, 'Loaded ' + features.length + ' SCAN stations; context rows: daily ' + pctRows.length.toLocaleString() + ', monthly ' + monthlyRows.length.toLocaleString() + ', fallback traces ' + fallbackRows.length.toLocaleString() + '.', 'pt-ops-ok');
    };

    layer._ptScanAddControls = function(depths) {
      var self = this;
      var mapObj = this._ptScanMap;
      if (!mapObj) return;

      this._ptScanDepthCtl = L.control({position: 'topleft'});
      this._ptScanDepthCtl.onAdd = function() {
        var div = L.DomUtil.create('div', 'pt-ops-scan-depth-control leaflet-control');
        var html = '<div class="pt-ops-scan-control-title">SCAN depth</div><div class="pt-ops-scan-depth-button-row">';
        depths.forEach(function(d) {
          var active = d === self._ptScanSelectedDepth ? ' active' : '';
          html += '<button type="button" class="pt-ops-scan-map-depth-btn' + active + '" data-depth="' + d + '">' + d + '</button>';
        });
        html += '</div>';
        div.innerHTML = html;
        L.DomEvent.disableClickPropagation(div);
        L.DomEvent.disableScrollPropagation(div);
        div.addEventListener('click', function(e) {
          var btn = e.target.closest ? e.target.closest('.pt-ops-scan-map-depth-btn') : null;
          if (!btn) return;
          self._ptScanApplyDepth(btn.getAttribute('data-depth'));
        });
        return div;
      };
      this._ptScanDepthCtl.addTo(mapObj);

      this._ptScanLegendCtl = L.control({position: 'topleft'});
      this._ptScanLegendCtl.onAdd = function() {
        var div = L.DomUtil.create('div', 'pt-ops-scan-legend-control leaflet-control');
        div.innerHTML = '<div class="pt-ops-scan-legend-titlebar"><div class="pt-ops-scan-control-title">SCAN context</div><button type="button" class="pt-ops-scan-legend-close" data-pt-scan-legend-close="1" title="Hide this legend">&times;</button></div>' +
          '<div class="pt-ops-scan-map-legend-title">Latest <span class="pt-ops-scan-selected-depth-label">' + self._ptScanSelectedDepth + '</span>-in context</div>' +
          '<div class="pt-ops-scan-legend-grid">' +
          '<div class="pt-ops-scan-legend-row"><span style="background:#8C510A"></span>Much below</div>' +
          '<div class="pt-ops-scan-legend-row"><span style="background:#D8B365"></span>Below</div>' +
          '<div class="pt-ops-scan-legend-row"><span style="background:#7F7F7F"></span>Near normal</div>' +
          '<div class="pt-ops-scan-legend-row"><span style="background:#92C5DE"></span>Above</div>' +
          '<div class="pt-ops-scan-legend-row"><span style="background:#2166AC"></span>Much above</div>' +
          '<div class="pt-ops-scan-legend-row"><span style="background:transparent; border:2px solid #777; box-sizing:border-box;"></span>No context</div>' +
          '</div>';
        L.DomEvent.disableClickPropagation(div);
        L.DomEvent.disableScrollPropagation(div);
        div.addEventListener('click', function(e) {
          var btn = e.target && e.target.closest ? e.target.closest('[data-pt-scan-legend-close="1"]') : null;
          if (!btn) return;
          if (typeof L !== 'undefined' && L.DomEvent) L.DomEvent.stop(e);
          else if (e && e.preventDefault) { e.preventDefault(); e.stopPropagation(); }
          div.style.display = 'none';
        });
        return div;
      };
      this._ptScanLegendCtl.addTo(mapObj);

      mapObj.on('popupopen', function(e) {
        var root = e.popup && e.popup.getElement ? e.popup.getElement() : null;
        if (root) self._ptScanSetPopupDepth(root, self._ptScanSelectedDepth);
      });
      var container = mapObj.getContainer();
      if (container && !container._ptScanTabsBound) {
        container.addEventListener('click', function(e) {
          var btn = e.target.closest ? e.target.closest('.pt-ops-scan-depth-tab') : null;
          if (!btn) return;
          var wrap = btn.closest('.pt-ops-scan-depth-tabs');
          if (!wrap) return;
          var target = btn.getAttribute('data-target');
          Array.prototype.forEach.call(wrap.querySelectorAll('.pt-ops-scan-depth-tab'), function(b) { b.classList.remove('active'); });
          Array.prototype.forEach.call(wrap.querySelectorAll('.pt-ops-scan-depth-panel'), function(p) { p.classList.remove('active'); });
          btn.classList.add('active');
          var panel = wrap.querySelector('#' + target);
          if (panel) panel.classList.add('active');
        });
        container._ptScanTabsBound = true;
      }
    };

    layer._ptScanSetPopupDepth = function(root, depth) {
      if (!root) return;
      var btn = root.querySelector('.pt-ops-scan-depth-tab[data-depth="' + depth + '"]');
      // RF057b: fall back to first available station depth if the map-selected
      // depth does not exist for this station.
      if (!btn) btn = root.querySelector('.pt-ops-scan-depth-tab');
      if (!btn) return;
      var wrap = btn.closest('.pt-ops-scan-depth-tabs');
      if (!wrap) return;
      var target = btn.getAttribute('data-target');
      Array.prototype.forEach.call(wrap.querySelectorAll('.pt-ops-scan-depth-tab'), function(b) { b.classList.remove('active'); });
      Array.prototype.forEach.call(wrap.querySelectorAll('.pt-ops-scan-depth-panel'), function(p) { p.classList.remove('active'); });
      btn.classList.add('active');
      var panel = wrap.querySelector('#' + target);
      if (panel) panel.classList.add('active');
    };

    layer._ptScanApplyDepth = function(depth) {
      depth = String(depth);
      this._ptScanSelectedDepth = depth;
      var mapObj = this._ptScanMap;
      if (!mapObj) return;
      Array.prototype.forEach.call(document.querySelectorAll('.pt-ops-scan-map-depth-btn'), function(btn) {
        btn.classList.toggle('active', btn.getAttribute('data-depth') === depth);
      });
      Array.prototype.forEach.call(document.querySelectorAll('.pt-ops-scan-selected-depth-label'), function(span) { span.textContent = depth; });
      this._ptScanMarkers.forEach(function(marker) {
        var site = marker._ptScanSite;
        var rec = marker && site && this._ptScanDepthLookup[site] ? this._ptScanDepthLookup[site][depth] : null;
        if (!rec) {
          marker.setStyle({fillColor: '#FFFFFF', fillOpacity: 0});
        } else {
          marker.setStyle({fillColor: rec.marker_fill || '#FFFFFF', fillOpacity: rec.marker_fill_opacity === undefined || rec.marker_fill_opacity === null ? 0.88 : rec.marker_fill_opacity});
        }
        marker.unbindTooltip();
        marker.bindTooltip(ptScanBuildMarkerHoverHtml(
          marker,
          depth,
          this._ptScanDepthLookup || {},
          this._ptScanTraceByKey || {},
          this._ptScanPctByKey || {},
          this._ptScanDepthStyleByDepth || {},
          false
        ), {className: 'pt-ops-scan-tooltip', direction: 'auto', opacity: 0.95});
      }, this);
      var popup = mapObj._popup;
      if (popup && popup.getElement) this._ptScanSetPopupDepth(popup.getElement(), depth);
    };

    layer.onRemove = function(mapObj) {
      this._ptScanIsRemoved = true;
      try { this.clearLayers(); } catch(e) {}
      if (this._ptScanDepthCtl && this._ptScanMap) {
        try { this._ptScanMap.removeControl(this._ptScanDepthCtl); } catch(e) {}
      }
      if (this._ptScanLegendCtl && this._ptScanMap) {
        try { this._ptScanMap.removeControl(this._ptScanLegendCtl); } catch(e) {}
      }
      this._ptScanDepthCtl = null;
      this._ptScanLegendCtl = null;
      this._ptScanMarkers = [];
      this._ptScanMarkerLookup = {};
      this._ptScanDepthLookup = {};
      this._ptScanTraceByKey = {};
      this._ptScanPctByKey = {};
      this._ptScanFallbackByKey = {};
      this._ptScanDepthStyleByDepth = {};
      delete activeLegendDefs[this._ptScanLayerName];
      setOpsLayerLoading(this._ptScanLayerName, false);
      redrawLegend();
      recordStatus(this._ptScanLayerName, 'Layer turned off.', 'pt-ops-muted');
      L.LayerGroup.prototype.onRemove.call(this, mapObj);
      this._ptScanMap = null;
    };

    layer.forceRemove = function(mapObj) {
      try { this.onRemove(mapObj || this._ptScanMap); } catch(e) {}
    };

    return layer;
  }
)---"
}
