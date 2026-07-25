# ==== leaflet_ops_live_usgs_groundwater_helpers.r ===========================
##
## PURPOSE:
##   USGS groundwater-specific JavaScript helpers for the BRIM Ops Live panel.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r` and returns browser-side
##   JavaScript for the GitHub-hosted USGS latest groundwater-level GeoJSON feed.
##
##   The scheduled GitHub feed is responsible for talking to USGS. The browser
##   only downloads a static GeoJSON + summary JSON from GitHub Pages. This keeps
##   BRIM users from directly hitting USGS when they open the map.
##
## DISPLAY V2 / RF030:
##   - Draw the active/recent candidate groundwater sites from the BRIM feed.
##   - Symbolize by latest depth to water in feet below ground/land surface.
##   - Preserve well/hole depth and aquifer-code context in popups.
##   - Add RF029 history context when the hosted feed includes it:
##       * period-of-record and seasonal deeper-percentile text;
##       * latest-vs-median context;
##       * compact inline SVG of water-year mean depth to water.
##   - Do not imply screened/perforated interval where the USGS API does not
##     provide it; show screen/perforation as not available through the API when absent.
##
## RF032:
##   - Popup text clarifies that the seasonal percentile uses a +/- water-day
##     window around the latest measurement date, not meteorological seasons.
##   - Water-year mean plot text clarifies that each point is the mean of
##     available measurements in that water year; n = 1 is therefore a single
##     measurement plotted as that year's mean.
##
## RF033:
##   - Adds a compact hover sparkline for quick area scanning without clicking.
##   - Uses the same RF029 water-year-mean history series in both hover and
##     popup plots.
##   - Shows the latest instantaneous/field measurement as a distinct marker so
##     users do not confuse it with a water-year mean.
##
## RF034:
##   - Detects exact co-located/nested USGS groundwater sites in the browser.
##   - Draws a single multi-well marker for duplicate coordinates so hidden
##     stacked points are not silently inaccessible.
##   - Hover summarizes the co-located wells; popup lists individual well records
##     with compact context, links, and mini history plots where available.
##
## RF035:
##   - Constrains grouped/nested well popups so large co-located completions do
##     not run off the screen.
##   - Keeps the location summary visible at the top and puts individual well
##     cards in a scrollable section.
##   - Tightens nested-well cards without changing single-well popup behavior.
##
## GW_NESTED_001:
##   - Converts nested/co-located popups from a long scroll list to a
##     shallow-to-deep button/card selector.
##
## RF041:
##   - Refines groundwater history plots with smaller plot text, explicit
##     year/value labels, and a water-year mean change note.
##   - Updates USGS groundwater links so the primary plot link opens the
##     parameter-72019 period-of-record view.
##   - Uses month/day/two-digit-year date-time formatting for latest measured
##     field-measurement timestamps.
## ============================================================================

pt_ops_live_usgs_groundwater_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // USGS latest groundwater-level layer from BRIM-hosted GitHub GeoJSON.
  // --------------------------------------------------------------------------

  function ptUsgwEnsureStyle() {
    if (document.getElementById('pt-ops-usgs-groundwater-style')) return;

    var style = document.createElement('style');
    style.id = 'pt-ops-usgs-groundwater-style';
    style.innerHTML = `
      .leaflet-tooltip.pt-ops-usgs-groundwater-tooltip {
        background: rgba(255, 255, 255, 0.97);
        border: 1px solid rgba(0,0,0,0.42);
        border-radius: 4px;
        box-shadow: 0 2px 7px rgba(0,0,0,0.22);
        color: #111;
        padding: 6px 8px;
        font: 12px/1.25 Arial, Helvetica, sans-serif;
        white-space: normal;
        width: 215px;
        max-width: 235px;
      }

      .pt-ops-usgs-groundwater-hover-plot {
        margin-top: 4px;
        border: 1px solid #e2e2dd;
        border-radius: 3px;
        background: #fbfbf8;
        padding: 2px;
      }

      .pt-ops-usgs-groundwater-hover-plot svg {
        display: block;
        width: 100%;
        height: auto;
      }

      /* RF041: nested cards use the compact hover sparkline, but a 200px
       * viewBox can scale too large inside wider popups. Keep it readable
       * without letting SVG text dominate the popup. */
      .pt-ops-usgs-groundwater-nested-card .pt-ops-usgs-groundwater-hover-plot {
        max-width: 285px;
      }

      .pt-ops-usgs-groundwater-cluster {
        border-radius: 50%;
        background: rgba(42, 111, 151, 0.24);
        border: 2px solid rgba(31, 94, 140, 0.95);
        box-shadow: 0 1px 6px rgba(0,0,0,0.35);
        color: #102f47;
        font: 700 12px/1 Arial, Helvetica, sans-serif;
        text-align: center;
      }

      .pt-ops-usgs-groundwater-cluster div {
        width: 100%;
        height: 100%;
        border-radius: 50%;
        background: rgba(255, 255, 255, 0.80);
        display: flex;
        align-items: center;
        justify-content: center;
        box-sizing: border-box;
      }

      .pt-ops-usgs-groundwater-cluster-small {
        border-color: rgba(31, 94, 140, 0.95);
      }

      .pt-ops-usgs-groundwater-cluster-medium {
        border-color: rgba(177, 96, 0, 0.98);
        color: #7a3f00;
      }

      .pt-ops-usgs-groundwater-cluster-large {
        border-color: rgba(128, 24, 24, 0.98);
        color: #6b1616;
      }

      .pt-ops-usgs-groundwater-multi-divicon {
        background: transparent;
        border: none;
      }

      .pt-ops-usgs-groundwater-multi-marker {
        position: relative;
        width: 25px;
        height: 25px;
        border-radius: 7px;
        border: 2px solid #7B241C;
        background: rgba(246, 190, 103, 0.94);
        box-shadow: 0 1px 5px rgba(0,0,0,0.32);
      }

      .pt-ops-usgs-groundwater-multi-marker::before,
      .pt-ops-usgs-groundwater-multi-marker::after {
        content: '';
        position: absolute;
        width: 5px;
        height: 5px;
        border-radius: 50%;
        background: #111;
        opacity: 0.84;
      }

      .pt-ops-usgs-groundwater-multi-marker::before {
        left: 6px;
        top: 6px;
        box-shadow: 9px 0 0 #111, 0 9px 0 #111, 9px 9px 0 #111;
      }

      .pt-ops-usgs-groundwater-multi-marker span {
        position: absolute;
        right: -7px;
        top: -8px;
        min-width: 15px;
        height: 15px;
        padding: 0 2px;
        border-radius: 10px;
        background: #7B241C;
        color: #fff;
        border: 1px solid #fff;
        font: 10px/14px Arial, Helvetica, sans-serif;
        text-align: center;
        box-shadow: 0 1px 3px rgba(0,0,0,0.28);
      }

      .pt-ops-usgs-groundwater-popup {
        font: 12px/1.35 Arial, Helvetica, sans-serif;
        min-width: 300px;
        max-width: 540px;
      }

      .pt-ops-usgs-groundwater-nested-popup {
        min-width: 320px;
        max-width: 520px;
      }

      .pt-ops-usgs-groundwater-nested-summary {
        border-top: 1px solid #ddd;
        padding-top: 4px;
        margin-top: 4px;
        margin-bottom: 5px;
      }

      .pt-ops-usgs-groundwater-nested-overview {
        margin: 5px 0 5px 0;
        padding-top: 4px;
        border-top: 1px solid #ecece6;
      }

      .pt-ops-usgs-groundwater-nested-overview-title {
        font-weight: 700;
        margin-bottom: 3px;
      }

      .pt-ops-usgs-groundwater-nested-table-wrap {
        max-height: 132px;
        overflow-y: auto;
        border: 1px solid #ddd;
        border-radius: 4px;
        background: #fff;
      }

      .pt-ops-usgs-groundwater-nested-table {
        width: 100%;
        border-collapse: collapse;
        font-size: 10.8px;
        line-height: 1.18;
      }

      .pt-ops-usgs-groundwater-nested-table th {
        position: sticky;
        top: 0;
        background: #f3f3ec;
        color: #333;
        text-align: left;
        border-bottom: 1px solid #d6d6cc;
        padding: 3px 4px;
        white-space: nowrap;
      }

      .pt-ops-usgs-groundwater-nested-table td {
        border-bottom: 1px solid #eee;
        padding: 3px 4px;
        vertical-align: top;
        white-space: nowrap;
      }

      .pt-ops-usgs-groundwater-nested-table tbody tr {
        cursor: pointer;
      }

      .pt-ops-usgs-groundwater-nested-table tbody tr:hover,
      .pt-ops-usgs-groundwater-nested-table tbody tr.pt-usgw-nested-active {
        background: #eef5ff;
      }

      .pt-ops-usgs-groundwater-nested-table .pt-usgw-muted {
        color: #666;
      }

      .pt-ops-usgs-groundwater-nested-tabbar {
        display: flex;
        flex-wrap: wrap;
        gap: 4px;
        max-height: 92px;
        overflow-y: auto;
        padding: 5px 0 4px 0;
        margin: 3px 0 5px 0;
        border-top: 1px solid #ecece6;
        border-bottom: 1px solid #ecece6;
      }

      .pt-ops-usgs-groundwater-nested-tabbar button {
        border: 1px solid #b9b9ad;
        border-radius: 999px;
        background: #f6f6ef;
        color: #222;
        padding: 3px 7px;
        font: 11px/1.2 Arial, Helvetica, sans-serif;
        cursor: pointer;
      }

      .pt-ops-usgs-groundwater-nested-tabbar button:hover,
      .pt-ops-usgs-groundwater-nested-tabbar button.pt-usgw-nested-active {
        background: #2B6CB0;
        border-color: #1f4f82;
        color: #fff;
      }

      .pt-ops-usgs-groundwater-nested-card-panel {
        display: none;
      }

      .pt-ops-usgs-groundwater-nested-card-panel.pt-usgw-nested-active {
        display: block;
      }

      .pt-ops-usgs-groundwater-nested-card {
        border: 1px solid #d6d6cc;
        border-radius: 5px;
        background: #fbfbf8;
        margin: 5px 0;
        padding: 6px 7px;
        font-size: 11.5px;
        line-height: 1.28;
      }

      .pt-ops-usgs-groundwater-nested-card .pt-ops-usgs-groundwater-hover-plot {
        margin-top: 3px;
      }

      .pt-ops-usgs-groundwater-nested-card a {
        display: inline-block;
        margin-right: 8px;
      }

      .pt-ops-usgs-groundwater-popup a {
        color: #1f5e9c;
        text-decoration: none;
      }

      .pt-ops-usgs-groundwater-popup a:hover {
        text-decoration: underline;
      }

      .pt-ops-usgs-groundwater-card {
        background: rgba(226, 241, 238, 0.96);
        border: 1px solid rgba(90,120,116,0.55);
        border-radius: 6px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.22);
        padding: 6px 7px;
        width: 360px;
        max-width: calc(100vw - 24px);
        box-sizing: border-box;
        font: 11.5px/1.25 Arial, Helvetica, sans-serif;
        color: #222;
        pointer-events: auto;
      }

      .pt-ops-usgs-groundwater-card * {
        pointer-events: auto;
      }


      .pt-ops-map-legend {
        background: rgba(226, 238, 235, 0.98) !important;
        border-color: rgba(90, 120, 116, 0.55) !important;
      }

      .pt-ops-usgs-groundwater-filter-title {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 8px;
        font-weight: 700;
        font-size: 12.5px;
        margin-bottom: 3px;
      }

      .pt-ops-usgs-groundwater-filter-close {
        border: 0;
        background: transparent;
        color: #777;
        font: bold 16px/1 Arial, Helvetica, sans-serif;
        padding: 0 1px;
        cursor: pointer;
      }

      .pt-ops-usgs-groundwater-filter-close:hover { color: #222; }

      .pt-ops-usgs-groundwater-filter-subtitle {
        color: #555;
        font-size: 10.5px;
        margin: 4px 0 5px 0;
        padding-top: 4px;
        border-top: 1px solid rgba(90,120,116,0.25);
      }

      .pt-ops-usgs-groundwater-filter-row {
        display: grid;
        grid-template-columns: 92px 1fr 1fr;
        column-gap: 4px;
        align-items: center;
        margin: 3px 0;
      }

      .pt-ops-usgs-groundwater-filter-row label {
        font-weight: 700;
      }

      .pt-ops-usgs-groundwater-card input[type="number"] {
        width: 100%;
        box-sizing: border-box;
        border: 1px solid #bfbfb6;
        border-radius: 3px;
        padding: 2px 3px;
        font: 11px/1.1 Arial, Helvetica, sans-serif;
      }

      .pt-ops-usgs-groundwater-card .pt-usgw-filter-hint {
        color: #666;
        font-size: 10.5px;
        margin-top: 1px;
      }

      .pt-ops-usgs-groundwater-card .pt-usgw-filter-presets {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 3px;
        margin: 5px 0 4px 0;
      }

      .pt-ops-usgs-groundwater-card .pt-usgw-filter-nested-inline {
        display: inline-flex;
        align-items: center;
        gap: 3px;
        white-space: nowrap;
        margin-left: 4px;
        font: 10.5px/1.2 Arial, Helvetica, sans-serif;
        cursor: pointer;
      }

      .pt-ops-usgs-groundwater-card .pt-usgw-filter-nested-inline input {
        margin: 0;
        cursor: pointer;
      }

      .pt-ops-usgs-groundwater-card button {
        border: 1px solid #aaa;
        border-radius: 4px;
        background: #f7f7f2;
        padding: 2px 5px;
        font: 10.5px/1.2 Arial, Helvetica, sans-serif;
        cursor: pointer !important;
        position: relative;
        z-index: 10002;
      }

      .pt-ops-usgs-groundwater-card button:hover {
        background: #eef5ff;
        border-color: #6b8fbd;
      }

      .pt-ops-usgs-groundwater-filter-actions {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 5px;
        margin-top: 4px;
      }

      .pt-ops-usgs-groundwater-filter-actions button {
        flex: 1 1 auto;
        font-weight: 700;
      }

      .pt-ops-usgs-groundwater-filter-count {
        margin-top: 4px;
        color: #555;
        font-size: 10.5px;
      }

      @media (max-width: 760px) {
        .pt-ops-usgs-groundwater-card {
          width: min(360px, calc(100vw - 20px));
        }
      }

      .pt-ops-usgs-groundwater-history-plot {
        margin-top: 5px;
        border: 1px solid #ddd;
        border-radius: 4px;
        background: #fbfbf8;
        padding: 3px;
      }

      .pt-ops-usgs-groundwater-history-plot svg {
        display: block;
        width: 100%;
        height: auto;
      }
    `;
    document.head.appendChild(style);
  }

  function ptUsgwTrim(value) {
    if (value === null || value === undefined) return '';
    var txt = String(value).trim();
    if (!txt || ['NA', 'NULL', 'NAN'].indexOf(txt.toUpperCase()) >= 0) return '';
    return txt;
  }

  function ptUsgwNum(value) {
    if (value === null || value === undefined || typeof value === 'boolean') return null;
    if (typeof value === 'string') {
      value = value.trim();
      if (!value || ['NA', 'NULL', 'NAN'].indexOf(value.toUpperCase()) >= 0) return null;
    }
    var n = Number(value);
    return isFinite(n) ? n : null;
  }

  function ptUsgwBool(value) {
    if (value === true) return true;
    if (value === false || value === null || value === undefined) return false;
    var txt = String(value).trim().toLowerCase();
    return txt === 'true' || txt === 't' || txt === '1' || txt === 'yes' || txt === 'y';
  }

  function ptUsgwFmtNumber(value, digits) {
    var n = ptUsgwNum(value);
    if (n === null) return 'not available';
    return n.toLocaleString(undefined, {
      minimumFractionDigits: digits || 0,
      maximumFractionDigits: digits || 0
    });
  }

  function ptUsgwFmtDepth(value) {
    var n = ptUsgwNum(value);
    if (n === null) return 'not available';
    var digits = Math.abs(n) < 10 && n !== 0 ? 2 : 1;
    return n.toLocaleString(undefined, {
      minimumFractionDigits: digits,
      maximumFractionDigits: digits
    }) + ' ft bgs';
  }

  function ptUsgwFmtPositiveFt(value) {
    var n = ptUsgwNum(value);
    if (n === null) return 'not available';
    return n.toLocaleString(undefined, {
      minimumFractionDigits: 0,
      maximumFractionDigits: 1
    }) + ' ft';
  }

  function ptUsgwFmtAgeDays(value) {
    var n = ptUsgwNum(value);
    if (n === null) return 'age unknown';
    if (n < 1) return 'same day';
    if (n < 90) return Math.round(n) + ' days old';
    return Math.round(n) + ' days old';
  }

  function ptUsgwStatusLabel(value) {
    var status = ptUsgwTrim(value);
    if (!status) return 'latest status not reported';

    var labels = {
      latest_groundwater_level_90d: 'Latest field measurement ≤90 days old',
      latest_groundwater_level_1y: 'Latest field measurement 91–365 days old',
      latest_groundwater_level_2y: 'Latest field measurement 1–2 years old',
      latest_groundwater_level_query_window: 'Latest field measurement within 800-day query window',
      stale_or_index_fallback_groundwater_level: 'Index fallback / stale measurement',
      no_recent_groundwater_level: 'No recent groundwater level'
    };

    if (Object.prototype.hasOwnProperty.call(labels, status)) return labels[status];
    return status.replace(/_/g, ' ');
  }

  function ptUsgwFmtPacific(value) {
    var raw = ptUsgwTrim(value);
    if (!raw) return 'time not available';
    var d = new Date(raw);
    if (isNaN(d.getTime())) return raw.replace('T', ' ');

    try {
      return d.toLocaleString('en-US', {
        timeZone: 'America/Los_Angeles',
        weekday: 'short',
        year: 'numeric',
        month: 'numeric',
        day: 'numeric',
        hour: 'numeric',
        minute: '2-digit',
        timeZoneName: 'short'
      });
    } catch(e) {
      return d.toLocaleString();
    }
  }

  function ptUsgwFmtPacificShort(value) {
    var raw = ptUsgwTrim(value);
    if (!raw) return 'time not available';
    var d = new Date(raw);
    if (isNaN(d.getTime())) return raw.replace('T', ' ');

    try {
      return d.toLocaleString('en-US', {
        timeZone: 'America/Los_Angeles',
        weekday: 'short',
        month: 'numeric',
        day: 'numeric',
        hour: 'numeric',
        minute: '2-digit',
        timeZoneName: 'short'
      });
    } catch(e) {
      return d.toLocaleString();
    }
  }

  function ptUsgwFmtPacificMmDdYy(value) {
    var raw = ptUsgwTrim(value);
    if (!raw) return 'time not available';
    var d = new Date(raw);
    if (isNaN(d.getTime())) return raw.replace('T', ' ');

    try {
      return d.toLocaleString('en-US', {
        timeZone: 'America/Los_Angeles',
        year: '2-digit',
        month: '2-digit',
        day: '2-digit',
        hour: 'numeric',
        minute: '2-digit',
        timeZoneName: 'short'
      });
    } catch(e) {
      return d.toLocaleString();
    }
  }

  function ptUsgwFmtDateShort(value) {
    var raw = ptUsgwTrim(value);
    if (!raw) return 'date not available';
    var d = new Date(raw);
    if (isNaN(d.getTime())) return raw;

    try {
      return d.toLocaleDateString('en-US', {
        timeZone: 'America/Los_Angeles',
        year: 'numeric',
        month: 'short',
        day: 'numeric'
      });
    } catch(e) {
      return d.toLocaleDateString();
    }
  }

  function ptUsgwDepthRadius(depth) {
    // GW_GW_004: keep USGS groundwater Ops Live wells at a constant
    // circle size. Depth-to-water is already encoded by fill color;
    // varying radius made the layer read too much like streamflow CFS
    // symbology and implied a magnitude scaling that is not intended.
    return 7.0;
  }

  function ptUsgwDepthFill(depth) {
    var n = ptUsgwNum(depth);
    if (n === null) return '#F2F2F2';
    if (n < 0) return '#756BB1';
    if (n < 25) return '#2B8CBE';
    if (n < 100) return '#7BCCC4';
    if (n < 250) return '#A1D99B';
    if (n < 500) return '#FEE391';
    if (n < 1000) return '#FEC44F';
    return '#BD0026';
  }

  function ptUsgwIsStale(ageDays) {
    var a = ptUsgwNum(ageDays);
    return a !== null && a > 365;
  }

  function ptUsgwDepthStroke(depth, ageDays) {
    // Make stale/older measurements visually distinct from the fill class.
    // A dark charcoal outline keeps measurement age visually separate
    // from the orange/red deep-depth fill classes.
    if (ptUsgwIsStale(ageDays)) return '#333333';

    var n = ptUsgwNum(depth);
    if (n === null) return '#777777';
    if (n < 0) return '#4A1486';
    if (n < 100) return '#08589E';
    if (n < 250) return '#238B45';
    if (n < 500) return '#B8860B';
    if (n < 1000) return '#A63603';
    return '#7F0000';
  }

  function ptUsgwPopupLink(label, url) {
    url = ptUsgwTrim(url);
    if (!url) return '';
    return '<div style="margin:1px 0;"><a href="' + escapeHtml(url) + '" target="_blank" rel="noopener">' + escapeHtml(label) + '</a></div>';
  }

  function ptUsgwSiteNoForUrl(siteNo) {
    var s = ptUsgwTrim(siteNo);
    if (!s) return '';
    s = s.replace(/^USGS[-_]?/i, '').replace(/[^0-9]/g, '');
    return s;
  }

  function ptUsgwMonitoringLocationUrl(p) {
    p = p || {};
    var siteNo = ptUsgwSiteNoForUrl(p.site_no || p.monitoring_location_id);
    if (siteNo) return 'https://waterdata.usgs.gov/monitoring-location/USGS-' + siteNo + '/';
    return ptUsgwTrim(p.usgs_monitoring_location_url);
  }

  function ptUsgwGroundwaterPorPlotUrl(p) {
    p = p || {};
    var siteNo = ptUsgwSiteNoForUrl(p.site_no || p.monitoring_location_id);
    if (siteNo) {
      return 'https://waterdata.usgs.gov/monitoring-location/USGS-' + siteNo + '/#dataTypeId=measurements-72019-0&period=periodOfRecord';
    }
    return ptUsgwTrim(p.usgs_gw_levels_url || p.usgs_all_graphs_url || p.usgs_monitoring_location_url);
  }

  function ptUsgwStationName(p) {
    return ptUsgwTrim(p.station_nm) || ptUsgwTrim(p.name) || 'USGS groundwater site';
  }

  function ptUsgwScreenIntervalText(p) {
    p = p || {};
    var top = ptUsgwNum(p.screen_top_ft);
    var bottom = ptUsgwNum(p.screen_bottom_ft);
    var hasScreen = ptUsgwBool(p.has_screen_interval) || top !== null || bottom !== null;

    if (top !== null && bottom !== null) return top.toLocaleString() + '–' + bottom.toLocaleString() + ' ft bgs';
    if (top !== null) return 'top ' + top.toLocaleString() + ' ft bgs; bottom not available';
    if (bottom !== null) return 'bottom ' + bottom.toLocaleString() + ' ft bgs; top not available';
    if (hasScreen) return 'present in feed but interval fields are incomplete';
    return 'not available through current USGS API feed';
  }


  function ptUsgwBlmDistanceText(p) {
    p = p || {};
    var onBlm = ptUsgwBool(p.on_blm_ca);
    var dist = ptUsgwNum(p.dist_to_blm_mi);

    if (onBlm) return 'On BLM-CA managed lands';
    if (dist === null) return 'not available';
    if (dist < 0.01) return '<0.01 mi';

    var digits = dist < 1 ? 2 : (dist < 10 ? 1 : 0);
    return dist.toLocaleString(undefined, {
      minimumFractionDigits: digits,
      maximumFractionDigits: digits
    }) + ' mi';
  }

  function ptUsgwAquiferText(p) {
    p = p || {};
    var parts = [];
    if (ptUsgwTrim(p.aqfr_cd)) parts.push('AQFR ' + ptUsgwTrim(p.aqfr_cd));
    if (ptUsgwTrim(p.aqfr_type_cd)) parts.push('type ' + ptUsgwTrim(p.aqfr_type_cd));
    if (ptUsgwTrim(p.nat_aqfr_cd)) parts.push('national ' + ptUsgwTrim(p.nat_aqfr_cd));
    return parts.length ? parts.join('; ') : 'not available';
  }


  function ptUsgwFmtPct(value) {
    var n = ptUsgwNum(value);
    if (n === null) return 'not available';
    return n.toLocaleString(undefined, {
      minimumFractionDigits: 0,
      maximumFractionDigits: 1
    }) + '%';
  }

  function ptUsgwFmtDeltaDepth(value) {
    var n = ptUsgwNum(value);
    if (n === null) return 'not available';

    var absVal = Math.abs(n).toLocaleString(undefined, {
      minimumFractionDigits: Math.abs(n) < 10 ? 1 : 0,
      maximumFractionDigits: 1
    });

    if (Math.abs(n) < 0.05) return 'about equal to median';
    return absVal + ' ft ' + (n > 0 ? 'deeper than median' : 'shallower than median');
  }

  function ptUsgwFmtTrend(value) {
    var n = ptUsgwNum(value);
    if (n === null) return 'not available';
    var absVal = Math.abs(n).toLocaleString(undefined, {
      minimumFractionDigits: Math.abs(n) < 1 ? 2 : 1,
      maximumFractionDigits: 2
    });
    if (Math.abs(n) < 0.005) return 'near-flat linear trend';
    return absVal + ' ft/yr ' + (n > 0 ? 'deeper' : 'shallower') + ' in water-year means';
  }

  function ptUsgwFmtWyMeanChange(value) {
    var n = ptUsgwNum(value);
    if (n === null) return '';
    var absVal = Math.abs(n).toLocaleString(undefined, {
      minimumFractionDigits: Math.abs(n) < 10 ? 1 : 0,
      maximumFractionDigits: 1
    });
    if (Math.abs(n) < 0.05) return 'WY mean change: about 0 ft';
    return 'WY mean change: ' + absVal + ' ft ' + (n > 0 ? 'deeper' : 'shallower');
  }

  function ptUsgwDeeperPctileText(value, baselineN) {
    var pct = ptUsgwNum(value);
    if (pct === null) return 'not enough history';
    var n = ptUsgwNum(baselineN);
    var txt = ptUsgwFmtPct(pct) + ' — deeper than about ' + ptUsgwFmtPct(pct) + ' of prior measurements';
    if (n !== null) txt += '; n=' + n.toLocaleString();
    return txt;
  }

  function ptUsgwParseWyMeanJson(value) {
    var raw = ptUsgwTrim(value);
    if (!raw) return [];

    var parsed = null;
    try {
      parsed = JSON.parse(raw);
    } catch(e) {
      return [];
    }

    if (!Array.isArray(parsed)) return [];

    var out = [];
    parsed.forEach(function(d) {
      if (!d) return;
      var wy = ptUsgwNum(d.wy);
      var mean = ptUsgwNum(d.mean_ft_bgs);
      var n = ptUsgwNum(d.n);
      if (wy === null || mean === null) return;
      out.push({wy: Math.round(wy), mean: mean, n: n});
    });

    out.sort(function(a, b) { return a.wy - b.wy; });
    return out;
  }

  function ptUsgwMiniPlotSvg(p) {
    p = p || {};

    var ptsAll = ptUsgwParseWyMeanJson(p.hist_plot_wy_mean_json);
    if (ptsAll.length < 2) return '';

    var latestWy = ptUsgwNum(p.hist_latest_water_year);
    var latestVal = ptUsgwNum(p.latest_wl_ft_bgs);
    if (latestWy !== null) latestWy = Math.round(latestWy);

    // The current water year is still in progress, so do not draw it as part
    // of the blue WY-mean line.  Use the red latest-measurement point at that
    // WY x-position instead.  This avoids visually overplotting a provisional
    // current-WY mean and the latest field measurement on the same year.
    var pts = ptsAll;
    if (latestWy !== null) {
      var completedPts = ptsAll.filter(function(d) { return d.wy < latestWy; });
      if (completedPts.length >= 2) pts = completedPts;
    }

    if (pts.length < 2) return '';

    var years = pts.map(function(d) { return d.wy; });
    var vals = pts.map(function(d) { return d.mean; });

    if (latestWy !== null && latestVal !== null) {
      years.push(latestWy);
      vals.push(latestVal);
    }

    var minYear = Math.min.apply(null, years);
    var maxYear = Math.max.apply(null, years);
    var minVal = Math.min.apply(null, vals);
    var maxVal = Math.max.apply(null, vals);

    if (!isFinite(minYear) || !isFinite(maxYear) || !isFinite(minVal) || !isFinite(maxVal)) return '';
    if (maxYear === minYear) maxYear = minYear + 1;
    if (maxVal === minVal) {
      maxVal = minVal + 1;
      minVal = minVal - 1;
    }

    var w = 430;
    var h = 190;
    var left = 56;
    var right = 66;
    var top = 38;
    var bottom = 52;
    var plotW = w - left - right;
    var plotH = h - top - bottom;

    function xScale(year) {
      return left + ((year - minYear) / (maxYear - minYear)) * plotW;
    }

    function yScale(v) {
      // Depth to water: larger values are deeper. Plot deeper values lower.
      return top + ((v - minVal) / (maxVal - minVal)) * plotH;
    }

    function fmtCoord(n) {
      return Number(n).toFixed(1);
    }

    function fmtFtShort(v) {
      var n = ptUsgwNum(v);
      if (n === null) return 'NA';
      return n.toLocaleString(undefined, {maximumFractionDigits: Math.abs(n) < 10 ? 1 : 0}) + ' ft';
    }

    var poly = pts.map(function(d) {
      return fmtCoord(xScale(d.wy)) + ',' + fmtCoord(yScale(d.mean));
    }).join(' ');

    var first = pts[0];
    var last = pts[pts.length - 1];
    var firstYear = first.wy;
    var lastCompletedYear = last.wy;
    var midYear = Math.round((firstYear + maxYear) / 2);
    var change = last.mean - first.mean;
    var changeText = ptUsgwFmtWyMeanChange(change);
    var minLabel = fmtFtShort(minVal);
    var maxLabel = fmtFtShort(maxVal);

    function axisTick(year, label, anchor) {
      var x = xScale(year);
      return '<line x1="' + fmtCoord(x) + '" y1="' + (top + plotH) + '" x2="' + fmtCoord(x) + '" y2="' + (top + plotH + 4) + '" stroke="#bbb" stroke-width="1"></line>' +
        '<text x="' + fmtCoord(x) + '" y="' + (h - 20) + '" font-size="11" fill="#555" text-anchor="' + anchor + '">' + escapeHtml(label) + '</text>';
    }

    var svg = '';
    svg += '<div class="pt-ops-usgs-groundwater-history-plot">';
    svg += '<svg viewBox="0 0 ' + w + ' ' + h + '" role="img" aria-label="Water-year mean groundwater depth to water plot">';
    svg += '<rect x="0" y="0" width="' + w + '" height="' + h + '" fill="#fbfbf8"></rect>';
    svg += '<text x="' + left + '" y="16" font-size="12" fill="#333">Blue = completed WY means; red = latest field measurement</text>';
    svg += '<line x1="' + left + '" y1="' + top + '" x2="' + (w - right) + '" y2="' + top + '" stroke="#ddd" stroke-width="1"></line>';
    svg += '<line x1="' + left + '" y1="' + (top + plotH / 2) + '" x2="' + (w - right) + '" y2="' + (top + plotH / 2) + '" stroke="#eee" stroke-width="1"></line>';
    svg += '<line x1="' + left + '" y1="' + (top + plotH) + '" x2="' + (w - right) + '" y2="' + (top + plotH) + '" stroke="#ddd" stroke-width="1"></line>';
    svg += '<text x="4" y="' + (top + 4) + '" font-size="11" fill="#555">' + escapeHtml(minLabel) + '</text>';
    svg += '<text x="4" y="' + (top + plotH + 3) + '" font-size="11" fill="#555">' + escapeHtml(maxLabel) + '</text>';
    svg += '<polyline points="' + poly + '" fill="none" stroke="#2B6CB0" stroke-width="2" stroke-linejoin="round" stroke-linecap="round"></polyline>';

    pts.forEach(function(d) {
      svg += '<circle cx="' + fmtCoord(xScale(d.wy)) + '" cy="' + fmtCoord(yScale(d.mean)) + '" r="2.1" fill="#2B6CB0" opacity="0.82"></circle>';
    });

    svg += '<text x="' + Math.min(w - right - 62, xScale(firstYear) + 4) + '" y="' + Math.max(top + 10, yScale(first.mean) - 5) + '" font-size="10.5" fill="#2B4F7F">' + escapeHtml(fmtFtShort(first.mean)) + '</text>';
    svg += '<text x="' + Math.max(left + 4, Math.min(w - right - 62, xScale(lastCompletedYear) - 58)) + '" y="' + Math.max(top + 10, yScale(last.mean) - 5) + '" font-size="10.5" fill="#2B4F7F">' + escapeHtml(fmtFtShort(last.mean)) + '</text>';

    if (latestWy !== null && latestVal !== null && latestWy >= minYear && latestWy <= maxYear) {
      var latestX = xScale(latestWy);
      var latestY = yScale(latestVal);
      svg += '<circle cx="' + fmtCoord(latestX) + '" cy="' + fmtCoord(latestY) + '" r="4.8" fill="#C0392B" stroke="#fff" stroke-width="1.4"></circle>';
      svg += '<text x="' + Math.min(w - 43, latestX + 9) + '" y="' + Math.max(22, Math.min(h - 30, latestY + 4)) + '" font-size="11" fill="#7B241C">latest</text>';
    }

    svg += axisTick(firstYear, 'WY ' + firstYear, 'start');
    if (midYear > firstYear && midYear < maxYear) svg += axisTick(midYear, String(midYear), 'middle');
    svg += axisTick(maxYear, 'WY ' + maxYear, 'end');
    svg += '<text x="' + left + '" y="' + (h - 6) + '" font-size="11" fill="#555">' + escapeHtml(changeText) + '</text>';
    svg += '</svg>';
    svg += '</div>';

    return svg;
  }


  function ptUsgwHoverMiniPlotSvg(p) {
    p = p || {};

    var ptsAll = ptUsgwParseWyMeanJson(p.hist_plot_wy_mean_json);
    if (ptsAll.length < 2) return '';

    var pts = ptsAll;

    var latestWy = ptUsgwNum(p.hist_latest_water_year);
    var latestVal = ptUsgwNum(p.latest_wl_ft_bgs);
    if (latestWy !== null) latestWy = Math.round(latestWy);

    if (latestWy !== null) {
      var completedPts = ptsAll.filter(function(d) { return d.wy < latestWy; });
      if (completedPts.length >= 2) pts = completedPts;
    }

    if (pts.length < 2) return '';

    var years = pts.map(function(d) { return d.wy; });
    var vals = pts.map(function(d) { return d.mean; });

    if (latestWy !== null && latestVal !== null) {
      years.push(latestWy);
      vals.push(latestVal);
    }

    var minYear = Math.min.apply(null, years);
    var maxYear = Math.max.apply(null, years);
    var minVal = Math.min.apply(null, vals);
    var maxVal = Math.max.apply(null, vals);

    if (!isFinite(minYear) || !isFinite(maxYear) || !isFinite(minVal) || !isFinite(maxVal)) return '';
    if (maxYear === minYear) maxYear = minYear + 1;
    if (maxVal === minVal) {
      maxVal = minVal + 1;
      minVal = minVal - 1;
    }

    var w = 210;
    var h = 70;
    var left = 26;
    var right = 10;
    var top = 14;
    var bottom = 18;
    var plotW = w - left - right;
    var plotH = h - top - bottom;

    function xScale(year) {
      return left + ((year - minYear) / (maxYear - minYear)) * plotW;
    }

    function yScale(v) {
      // Depth to water: larger values are deeper. Plot deeper values lower.
      return top + ((v - minVal) / (maxVal - minVal)) * plotH;
    }

    function fmtCoord(n) { return Number(n).toFixed(1); }
    function fmtFtTiny(v) {
      var n = ptUsgwNum(v);
      if (n === null) return 'NA';
      return n.toLocaleString(undefined, {maximumFractionDigits: 0});
    }

    var poly = pts.map(function(d) {
      return fmtCoord(xScale(d.wy)) + ',' + fmtCoord(yScale(d.mean));
    }).join(' ');

    var first = pts[0];
    var last = pts[pts.length - 1];
    var firstYear = first.wy;
    var lastYear = last.wy;
    var changeText = ptUsgwFmtWyMeanChange(last.mean - first.mean).replace('WY mean change: ', 'Δ WY: ');

    var svg = '';
    svg += '<div class="pt-ops-usgs-groundwater-hover-plot">';
    svg += '<svg viewBox="0 0 ' + w + ' ' + h + '" role="img" aria-label="Compact water-year mean groundwater depth to water plot">';
    svg += '<rect x="0" y="0" width="' + w + '" height="' + h + '" fill="#fbfbf8"></rect>';
    svg += '<line x1="' + left + '" y1="' + top + '" x2="' + (w - right) + '" y2="' + top + '" stroke="#e6e6df" stroke-width="1"></line>';
    svg += '<line x1="' + left + '" y1="' + (top + plotH) + '" x2="' + (w - right) + '" y2="' + (top + plotH) + '" stroke="#e6e6df" stroke-width="1"></line>';
    svg += '<text x="3" y="' + (top + 3) + '" font-size="6.5" fill="#666">' + escapeHtml(fmtFtTiny(minVal)) + '</text>';
    svg += '<text x="3" y="' + (top + plotH + 3) + '" font-size="6.5" fill="#666">' + escapeHtml(fmtFtTiny(maxVal)) + '</text>';
    svg += '<polyline points="' + poly + '" fill="none" stroke="#2B6CB0" stroke-width="1.8" stroke-linejoin="round" stroke-linecap="round"></polyline>';

    pts.forEach(function(d) {
      svg += '<circle cx="' + fmtCoord(xScale(d.wy)) + '" cy="' + fmtCoord(yScale(d.mean)) + '" r="1.35" fill="#2B6CB0" opacity="0.75"></circle>';
    });

    if (latestWy !== null && latestVal !== null && latestWy >= minYear && latestWy <= maxYear) {
      svg += '<circle cx="' + fmtCoord(xScale(latestWy)) + '" cy="' + fmtCoord(yScale(latestVal)) + '" r="3.2" fill="#C0392B" stroke="#fff" stroke-width="1.0"></circle>';
    }

    svg += '<text x="' + left + '" y="8" font-size="6.8" fill="#555">Completed WY means; red = latest</text>';
    svg += '<text x="' + left + '" y="' + (h - 4) + '" font-size="6.8" fill="#666">' + escapeHtml(firstYear) + '</text>';
    svg += '<text x="' + (w - right - 28) + '" y="' + (h - 4) + '" font-size="6.8" fill="#666">' + escapeHtml(lastYear) + '</text>';
    svg += '<text x="' + (left + 44) + '" y="' + (h - 4) + '" font-size="6.5" fill="#666">' + escapeHtml(changeText) + '</text>';
    svg += '</svg>';
    svg += '</div>';

    return svg;
  }


  function ptUsgwHistoryHtml(p) {
    p = p || {};

    if (!ptUsgwBool(p.hist_has_history) && ptUsgwNum(p.hist_record_count) === null) {
      return '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
        '<b>Historical context</b><br><span style="color:#555;">No RF029 groundwater-history summary is available in this feed yet.</span>' +
        '</div>';
    }

    var recN = ptUsgwNum(p.hist_record_count);
    var porYears = ptUsgwNum(p.hist_por_years);
    var firstDate = ptUsgwTrim(p.hist_first_wl_date);
    var lastDate = ptUsgwTrim(p.hist_last_wl_date);
    var plot = ptUsgwMiniPlotSvg(p);

    var html = '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
      '<b>Historical context</b>';

    html += '<br><b>Record:</b> ' + (recN !== null ? recN.toLocaleString() + ' measurements' : 'measurement count not available');
    if (firstDate || lastDate) html += ' (' + escapeHtml(firstDate || '?') + ' to ' + escapeHtml(lastDate || '?') + ')';
    if (porYears !== null) html += '; ' + porYears.toLocaleString(undefined, {maximumFractionDigits: 1}) + ' yrs';

    html += '<br><b>POR percentile:</b> ' + escapeHtml(ptUsgwDeeperPctileText(p.hist_por_deeper_pctile, p.hist_por_baseline_n));
    html += '<br><b>Vs POR median:</b> ' + escapeHtml(ptUsgwFmtDeltaDepth(p.hist_latest_vs_por_median_ft));

    var seasonalWindow = ptUsgwNum(p.hist_seasonal_window_days);
    html += '<br><b>Seasonal percentile:</b> ' + escapeHtml(ptUsgwDeeperPctileText(p.hist_seasonal_deeper_pctile, p.hist_seasonal_baseline_n));
    if (seasonalWindow !== null) html += ' <span style="color:#555;">(±' + seasonalWindow + ' water-days)</span>';
    html += '<br><b>Vs seasonal median:</b> ' + escapeHtml(ptUsgwFmtDeltaDepth(p.hist_latest_vs_seasonal_median_ft));

    var latestWyMean = ptUsgwNum(p.hist_latest_water_year_mean_ft_bgs);
    var latestWyN = ptUsgwNum(p.hist_latest_water_year_n);
    if (latestWyMean !== null) {
      html += '<br><b>Latest WY mean:</b> ' + escapeHtml(ptUsgwFmtDepth(latestWyMean));
      if (latestWyN !== null) html += ' <span style="color:#555;">(n=' + latestWyN.toLocaleString() + ')</span>';
    }

    var trend = ptUsgwNum(p.hist_trend_wy_mean_ft_per_year);
    if (trend !== null) html += '<br><b>Simple WY trend:</b> ' + escapeHtml(ptUsgwFmtTrend(trend));

    if (plot) {
      html += plot;
    } else {
      html += '<br><span style="color:#555;">Mini plot not shown: not enough water-year summary points in the RF029 feed.</span>';
    }

    var seasonalNote = 'Seasonal comparison uses prior measurements within the +/- water-day window around the latest measurement date.';
    if (seasonalWindow !== null) {
      seasonalNote = 'Seasonal comparison uses prior measurements within +/-' + seasonalWindow + ' water-days of the latest measurement date.';
    }

    html += '<div style="color:#555;font-size:11px;margin-top:3px;">' +
      'Percentiles compare the latest depth-to-water with prior measurements; higher values mean the latest measurement is deeper than more of the historical record. ' +
      escapeHtml(seasonalNote) + ' Plot uses completed WY means in blue; red is the latest field measurement plotted at its water year. WY means average the available measurements in each water year.' +
      '</div>';
    html += '</div>';

    return html;
  }

  function ptUsgwCoordKey(coords) {
    if (!coords || coords.length < 2) return '';
    // Round to seven decimals (~1 cm at the equator). The USGS nested-well
    // problem is mostly exact-coordinate stacking, but this also avoids tiny
    // binary/serialization jitter in browser-parsed GeoJSON coordinates.
    return Number(coords[0]).toFixed(7) + '|' + Number(coords[1]).toFixed(7);
  }

  function ptUsgwSortGroupFeatures(a, b) {
    var pa = a && a.properties ? a.properties : {};
    var pb = b && b.properties ? b.properties : {};

    var da = ptUsgwNum(pa.well_depth_ft);
    var db = ptUsgwNum(pb.well_depth_ft);

    if (da !== null && db !== null && da !== db) return da - db;
    if (da !== null && db === null) return -1;
    if (da === null && db !== null) return 1;

    return String(ptUsgwTrim(pa.site_no) || ptUsgwTrim(pa.station_nm)).localeCompare(
      String(ptUsgwTrim(pb.site_no) || ptUsgwTrim(pb.station_nm))
    );
  }

  function ptUsgwGroupDtwRange(features) {
    var vals = [];
    (features || []).forEach(function(feature) {
      var p = feature && feature.properties ? feature.properties : {};
      var v = ptUsgwNum(p.latest_wl_ft_bgs);
      if (v !== null) vals.push(v);
    });
    if (!vals.length) return 'DTW values not available';
    var minVal = Math.min.apply(null, vals);
    var maxVal = Math.max.apply(null, vals);
    if (Math.abs(maxVal - minVal) < 0.05) return 'DTW: ' + ptUsgwFmtDepth(minVal);
    return 'DTW range: ' + ptUsgwFmtDepth(minVal) + ' to ' + ptUsgwFmtDepth(maxVal);
  }

  function ptUsgwGroupWellDepthRange(features) {
    var vals = [];
    (features || []).forEach(function(feature) {
      var p = feature && feature.properties ? feature.properties : {};
      var v = ptUsgwNum(p.well_depth_ft);
      if (v !== null) vals.push(v);
    });
    if (!vals.length) return 'Well depths not available';
    var minVal = Math.min.apply(null, vals);
    var maxVal = Math.max.apply(null, vals);
    if (Math.abs(maxVal - minVal) < 0.05) return 'Well depth: ' + ptUsgwFmtPositiveFt(minVal);
    return 'Well depths: ' + ptUsgwFmtPositiveFt(minVal) + ' to ' + ptUsgwFmtPositiveFt(maxVal);
  }

  function ptUsgwScreenIntervalTextCompact(p) {
    var txt = ptUsgwScreenIntervalText(p || {});
    if (!txt || txt === 'not available through current USGS API feed') return 'not available through USGS API';
    return txt;
  }


  function ptUsgwNestedGroupId(features) {
    var parts = [];
    (features || []).forEach(function(feature) {
      var p = feature && feature.properties ? feature.properties : {};
      var siteNo = ptUsgwTrim(p.site_no);
      if (siteNo) parts.push(siteNo);
    });
    var key = parts.slice(0, 6).join('-') || String(Date.now());
    return 'pt-usgw-nested-' + key.replace(/[^A-Za-z0-9_-]/g, '-');
  }

  function ptUsgwNestedTabLabel(p, idx) {
    p = p || {};
    var wellDepth = ptUsgwNum(p.well_depth_ft);
    var siteNo = ptUsgwTrim(p.site_no);
    var label = String(idx);
    if (wellDepth !== null) {
      label += ' | well ' + ptUsgwFmtPositiveFt(wellDepth);
    } else if (siteNo) {
      label += ' | ' + siteNo.slice(-4);
    }
    return label;
  }

  function ptUsgwNestedSiteShort(p) {
    p = p || {};
    var siteNo = ptUsgwTrim(p.site_no);
    if (!siteNo) return '—';
    if (siteNo.length <= 8) return siteNo;
    return '…' + siteNo.slice(-6);
  }

  function ptUsgwNestedHistShort(p) {
    p = p || {};
    var seasonalPct = ptUsgwNum(p.hist_seasonal_deeper_pctile);
    var porPct = ptUsgwNum(p.hist_por_deeper_pctile);
    if (seasonalPct !== null) return 'seasonal ' + ptUsgwFmtPct(seasonalPct);
    if (porPct !== null) return 'POR ' + ptUsgwFmtPct(porPct);
    return '—';
  }

  function ptUsgwNestedOverviewTable(features, groupId) {
    features = (features || []).slice().sort(ptUsgwSortGroupFeatures);
    if (!features.length) return '';

    var html = '<div class="pt-ops-usgs-groundwater-nested-overview">' +
      '<div class="pt-ops-usgs-groundwater-nested-overview-title">Wells at this coordinate</div>' +
      '<div class="pt-ops-usgs-groundwater-nested-table-wrap">' +
      '<table class="pt-ops-usgs-groundwater-nested-table" aria-label="Co-located USGS groundwater wells summary">' +
      '<thead><tr>' +
      '<th>#</th><th>USGS</th><th>Well depth</th><th>DTW</th><th>Age</th><th>Hist.</th>' +
      '</tr></thead><tbody>';

    features.forEach(function(feature, i) {
      var p = feature && feature.properties ? feature.properties : {};
      var active = i === 0 ? ' class="pt-usgw-nested-active"' : '';
      var onclick = ' onclick="return window.ptUsgwSelectNestedCard(\'' + groupId + '\',' + i + ');"';
      html += '<tr' + active + onclick + '>';
      html += '<td><b>' + escapeHtml(String(i + 1)) + '</b></td>';
      html += '<td class="pt-usgw-muted" title="USGS ' + escapeHtml(ptUsgwTrim(p.site_no) || '') + '">' + escapeHtml(ptUsgwNestedSiteShort(p)) + '</td>';
      html += '<td>' + escapeHtml(ptUsgwFmtPositiveFt(p.well_depth_ft)) + '</td>';
      html += '<td>' + escapeHtml(ptUsgwFmtDepth(p.latest_wl_ft_bgs)) + '</td>';
      html += '<td>' + escapeHtml(ptUsgwFmtAgeDays(p.latest_age_days).replace(' old', '')) + '</td>';
      html += '<td>' + escapeHtml(ptUsgwNestedHistShort(p)) + '</td>';
      html += '</tr>';
    });

    html += '</tbody></table></div>' +
      '<div style="color:#555;font-size:10.5px;margin-top:2px;">Rows are ordered shallowest to deepest where well depth is available. Click a row or button to inspect that well.</div>' +
      '</div>';

    return html;
  }

  window.ptUsgwSelectNestedCard = window.ptUsgwSelectNestedCard || function(groupId, idx) {
    var root = document.getElementById(groupId);
    if (!root) return false;

    var buttons = root.querySelectorAll('.pt-ops-usgs-groundwater-nested-tabbar button');
    var cards = root.querySelectorAll('.pt-ops-usgs-groundwater-nested-card-panel');

    buttons.forEach(function(btn, i) {
      if (i === idx) btn.classList.add('pt-usgw-nested-active');
      else btn.classList.remove('pt-usgw-nested-active');
    });

    cards.forEach(function(card, i) {
      if (i === idx) card.classList.add('pt-usgw-nested-active');
      else card.classList.remove('pt-usgw-nested-active');
    });

    var rows = root.querySelectorAll('.pt-ops-usgs-groundwater-nested-table tbody tr');
    rows.forEach(function(row, i) {
      if (i === idx) row.classList.add('pt-usgw-nested-active');
      else row.classList.remove('pt-usgw-nested-active');
    });

    return false;
  };

  function ptUsgwNestedTooltip(features) {
    features = (features || []).slice().sort(ptUsgwSortGroupFeatures);
    var n = features.length;

    var html = '<b>' + n.toLocaleString() + ' co-located USGS groundwater wells</b>';
    html += '<br><span style="color:#555;">These records share one mapped coordinate.</span>';
    html += '<br>' + escapeHtml(ptUsgwGroupDtwRange(features));
    html += '<br>' + escapeHtml(ptUsgwGroupWellDepthRange(features));

    var shown = 0;
    features.forEach(function(feature) {
      if (shown >= 5) return;
      var p = feature && feature.properties ? feature.properties : {};
      html += '<br><span style="color:#333;">' + escapeHtml(ptUsgwTrim(p.station_nm) || ptUsgwTrim(p.site_no) || 'USGS') +
        ': ' + escapeHtml(ptUsgwFmtDepth(p.latest_wl_ft_bgs)) + '</span>';
      shown += 1;
    });

    if (features.length > shown) {
      html += '<br><span style="color:#555;">+' + (features.length - shown).toLocaleString() + ' more; click for full list.</span>';
    } else {
      html += '<br><span style="color:#555;">Click for individual well records.</span>';
    }

    return html;
  }

  function ptUsgwNestedWellCard(p, idx) {
    p = p || {};
    var siteNo = ptUsgwTrim(p.site_no);
    var station = ptUsgwStationName(p);
    var measured = ptUsgwTrim(p.latest_wl_datetime_utc) || ptUsgwTrim(p.latest_wl_date);
    var plot = ptUsgwHoverMiniPlotSvg(p);

    var html = '<div class="pt-ops-usgs-groundwater-nested-card">';
    html += '<div style="font-weight:700;margin-bottom:1px;">' + escapeHtml(String(idx) + '. ' + (station || siteNo || 'USGS groundwater well')) + '</div>';
    if (siteNo && siteNo !== station) html += '<div style="color:#555;font-size:10.5px;margin-bottom:2px;">USGS ' + escapeHtml(siteNo) + '</div>';

    var apiFlag = ptUsgwBool(p.has_api_latest_wl);
    var latestSource = apiFlag ? 'USGS API field measurement' : 'BRIM index fallback';

    html += '<b>DTW:</b> ' + escapeHtml(ptUsgwFmtDepth(p.latest_wl_ft_bgs));
    html += ' <span style="color:#555;">| ' + escapeHtml(ptUsgwFmtAgeDays(p.latest_age_days)) + '</span>';
    html += '<br><b>Measured:</b> ' + escapeHtml(measured ? ptUsgwFmtPacificMmDdYy(measured) : 'date not available');
    html += '<br><b>Source:</b> ' + escapeHtml(latestSource);

    html += '<br><b>Depth:</b> well ' + escapeHtml(ptUsgwFmtPositiveFt(p.well_depth_ft));
    html += ' | hole ' + escapeHtml(ptUsgwFmtPositiveFt(p.hole_depth_ft));
    html += '<br><b>Screen/perf:</b> ' + escapeHtml(ptUsgwScreenIntervalTextCompact(p));
    html += '<br><b>Aquifer:</b> ' + escapeHtml(ptUsgwAquiferText(p));
    html += '<br><b>Approx. BLM distance:</b> ' + escapeHtml(ptUsgwBlmDistanceText(p));

    var porPct = ptUsgwNum(p.hist_por_deeper_pctile);
    var seasonalPct = ptUsgwNum(p.hist_seasonal_deeper_pctile);
    if (porPct !== null || seasonalPct !== null) {
      html += '<br><b>History:</b> ';
      if (porPct !== null) html += 'POR ' + escapeHtml(ptUsgwFmtPct(porPct)) + ' deeper';
      if (porPct !== null && seasonalPct !== null) html += ' | ';
      if (seasonalPct !== null) html += 'seasonal ' + escapeHtml(ptUsgwFmtPct(seasonalPct)) + ' deeper';
    }

    if (plot) html += plot;

    var links =
      ptUsgwPopupLink('USGS plot (POR)', ptUsgwGroundwaterPorPlotUrl(p)) +
      ptUsgwPopupLink('USGS site page', ptUsgwMonitoringLocationUrl(p));

    if (links) html += '<div style="margin-top:2px;">' + links + '</div>';
    html += '</div>';
    return html;
  }

  function ptUsgwNestedPopup(features) {
    features = (features || []).slice().sort(ptUsgwSortGroupFeatures);
    var n = features.length;
    var groupId = ptUsgwNestedGroupId(features);

    var html = '<div id="' + escapeHtml(groupId) + '" class="pt-ops-usgs-groundwater-popup pt-ops-usgs-groundwater-nested-popup">' +
      '<div style="font-weight:700;font-size:13px;margin-bottom:2px;">Co-located / nested USGS groundwater wells</div>' +
      '<div style="color:#555;font-size:11px;margin-bottom:5px;">' + n.toLocaleString() + ' USGS groundwater records share this mapped coordinate. Select a well below; buttons are ordered shallowest to deepest where well depth is available.</div>';

    html += '<div class="pt-ops-usgs-groundwater-nested-summary">' +
      '<b>Location summary</b>' +
      '<br>' + escapeHtml(ptUsgwGroupDtwRange(features)) +
      '<br>' + escapeHtml(ptUsgwGroupWellDepthRange(features)) +
      '<br><span style="color:#555;">Shared coordinates can represent nested/multiple monitoring wells, separate completions, or closely related site records.</span>' +
      '</div>';

    html += ptUsgwNestedOverviewTable(features, groupId);

    html += '<div class="pt-ops-usgs-groundwater-nested-tabbar" aria-label="Select co-located USGS groundwater record">';
    features.forEach(function(feature, i) {
      var p = feature && feature.properties ? feature.properties : {};
      var active = i === 0 ? ' pt-usgw-nested-active' : '';
      html += '<button type="button" class="' + active.trim() + '" onclick="return window.ptUsgwSelectNestedCard(\'' + groupId + '\',' + i + ');">' +
        escapeHtml(ptUsgwNestedTabLabel(p, i + 1)) +
        '</button>';
    });
    html += '</div>';

    features.forEach(function(feature, i) {
      var p = feature && feature.properties ? feature.properties : {};
      var active = i === 0 ? ' pt-usgw-nested-active' : '';
      html += '<div class="pt-ops-usgs-groundwater-nested-card-panel' + active + '">';
      html += ptUsgwNestedWellCard(p, i + 1);
      html += '</div>';
    });

    html += '<div style="padding-top:4px;margin-top:5px;color:#555;font-size:11px;">' +
      'Grouped marker added by BRIM because multiple USGS groundwater records share one coordinate. WY line = completed water-year means; red point = latest field measurement.' +
      '</div>';

    html += '</div>';
    return html;
  }

  function ptUsgwMakeMultiWellIcon(count) {
    var n = Math.max(2, Math.min(99, Number(count || 2)));
    return L.divIcon({
      className: 'pt-ops-usgs-groundwater-multi-divicon',
      html: '<div class="pt-ops-usgs-groundwater-multi-marker"><span>' + escapeHtml(n) + '</span></div>',
      iconSize: [32, 32],
      iconAnchor: [16, 16],
      popupAnchor: [0, -16],
      tooltipAnchor: [0, -18]
    });
  }

  function ptUsgwAddSingleWellMarker(markers, feature) {
    var p = feature && feature.properties ? feature.properties : {};
    var dtw = ptUsgwNum(p.latest_wl_ft_bgs);
    var age = ptUsgwNum(p.latest_age_days);
    var coords = ptUsgwFeatureCoords(feature);

    if (!coords || dtw === null) return false;

    var marker = L.circleMarker(coords, {
      radius: ptUsgwDepthRadius(dtw),
      color: ptUsgwDepthStroke(dtw, age),
      weight: ptUsgwIsStale(age) ? 2.65 : 1.35,
      opacity: 0.92,
      dashArray: null,
      fillColor: ptUsgwDepthFill(dtw),
      fillOpacity: 0.72,
      pane: 'pane_ops'
    });

    marker.bindTooltip(ptUsgwTooltip(p), {
      direction: 'top',
      sticky: true,
      opacity: 0.97,
      className: 'pt-ops-usgs-groundwater-tooltip'
    });
    marker.bindPopup(ptUsgwPopup(p));
    marker.addTo(markers);
    return true;
  }

  function ptUsgwAddNestedMarker(markers, features) {
    features = (features || []).slice().sort(ptUsgwSortGroupFeatures);
    if (!features.length) return false;

    var coords = ptUsgwFeatureCoords(features[0]);
    if (!coords) return false;

    var marker = L.marker(coords, {
      icon: ptUsgwMakeMultiWellIcon(features.length),
      pane: 'pane_ops',
      zIndexOffset: 650
    });

    marker.bindTooltip(ptUsgwNestedTooltip(features), {
      direction: 'top',
      sticky: true,
      opacity: 0.97,
      className: 'pt-ops-usgs-groundwater-tooltip'
    });
    marker.bindPopup(ptUsgwNestedPopup(features), {
      maxWidth: 560,
      maxHeight: 610
    });
    marker.addTo(markers);
    return true;
  }

  function ptUsgwTooltip(p) {
    p = p || {};

    var siteNo = ptUsgwTrim(p.site_no);
    var dtw = ptUsgwNum(p.latest_wl_ft_bgs);
    var age = ptUsgwNum(p.latest_age_days);
    var measured = ptUsgwTrim(p.latest_wl_datetime_utc) || ptUsgwTrim(p.latest_wl_date);

    var html = '<b>' + escapeHtml(siteNo || 'USGS') + '</b>';
    html += ' – ' + escapeHtml(ptUsgwStationName(p));
    html += '<br>DTW: <b>' + escapeHtml(ptUsgwFmtDepth(dtw)) + '</b>';
    if (dtw !== null && dtw < 0) html += '<br><span style="color:#5B2C83;">Reported above land-surface datum / artesian condition.</span>';
    html += '<br>Measured: ' + escapeHtml(measured ? ptUsgwFmtPacificMmDdYy(measured) : 'date not available');
    html += '<br>Age at feed: ' + escapeHtml(ptUsgwFmtAgeDays(age));

    var porPct = ptUsgwNum(p.hist_por_deeper_pctile);
    var seasonalPct = ptUsgwNum(p.hist_seasonal_deeper_pctile);
    if (porPct !== null) html += '<br>POR: ' + escapeHtml(ptUsgwFmtPct(porPct)) + ' deeper pctile';
    if (seasonalPct !== null) html += '<br>Seasonal: ' + escapeHtml(ptUsgwFmtPct(seasonalPct)) + ' deeper pctile';

    var hoverPlot = ptUsgwHoverMiniPlotSvg(p);
    if (hoverPlot) html += hoverPlot;

    return html;
  }

  function ptUsgwPopup(p) {
    p = p || {};

    var siteNo = ptUsgwTrim(p.site_no);
    var dtw = ptUsgwNum(p.latest_wl_ft_bgs);
    var measured = ptUsgwTrim(p.latest_wl_datetime_utc) || ptUsgwTrim(p.latest_wl_date);
    var apiFlag = ptUsgwBool(p.has_api_latest_wl);
    var latestSource = apiFlag ? 'USGS API field measurement' : 'BRIM index fallback';

    var html = '<div class="pt-ops-usgs-groundwater-popup">' +
      '<div style="font-weight:700;font-size:13px;margin-bottom:2px;">' +
      escapeHtml(siteNo || 'USGS') + ' – ' + escapeHtml(ptUsgwStationName(p)) +
      '</div>' +
      '<div style="color:#555;font-size:11px;margin-bottom:5px;">USGS Water Data field-measurement groundwater level</div>';

    html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:4px;">' +
      '<b>Latest groundwater level</b>' +
      '<br><b>Depth to water:</b> ' + escapeHtml(ptUsgwFmtDepth(dtw)) +
      '<br><b>Measured:</b> ' + escapeHtml(measured ? ptUsgwFmtPacificMmDdYy(measured) : 'date not available') +
      '<br><b>Age at feed:</b> ' + escapeHtml(ptUsgwFmtAgeDays(p.latest_age_days)) +
      '<br><b>Source:</b> ' + escapeHtml(latestSource) +
      '<br><b>Qualifier:</b> ' + escapeHtml(ptUsgwTrim(p.latest_wl_qualifier) || 'not listed') +
      '<br><b>Procedure:</b> ' + escapeHtml(ptUsgwTrim(p.latest_wl_procedure) || 'not listed') +
      '</div>';

    if (dtw !== null && dtw < 0) {
      html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;color:#5B2C83;">' +
        '<b>Negative depth note</b><br>USGS parameter 72019 is depth to water below land surface / ground surface. Negative values are retained as reported and may indicate a water level above the land-surface datum or related measurement/datum context.' +
        '</div>';
    }

    html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
      '<b>Well / aquifer context</b>' +
      '<br><b>Well depth:</b> ' + escapeHtml(ptUsgwFmtPositiveFt(p.well_depth_ft)) +
      '<br><b>Hole depth:</b> ' + escapeHtml(ptUsgwFmtPositiveFt(p.hole_depth_ft)) +
      '<br><b>Screen/perforation:</b> ' + escapeHtml(ptUsgwScreenIntervalText(p)) +
      '<br><b>Aquifer codes:</b> ' + escapeHtml(ptUsgwAquiferText(p)) +
      '<br><b>Approx. distance to BLM-CA managed lands:</b> ' + escapeHtml(ptUsgwBlmDistanceText(p)) +
      '</div>';

    html += ptUsgwHistoryHtml(p);

    html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
      '<b>Source / caveat</b>' +
      '<br>Latest values come from USGS Water Data field measurements and BRIM groundwater live-feed index records, refreshed by the BRIM GitHub workflow.' +
      '<br><span style="color:#555;">Groundwater field measurements are low-frequency site-visit data. Interpret values with well construction, datum, aquifer context, and measurement history; screen/perforation intervals are not available through the current USGS API feed when not shown above.</span>' +
      '<br><span style="color:#555;">Feed built: ' + escapeHtml(ptUsgwFmtPacificShort(p.feed_build_time_utc)) + '</span>' +
      '</div>';

    var links =
      ptUsgwPopupLink('USGS plot (period of record)', ptUsgwGroundwaterPorPlotUrl(p)) +
      ptUsgwPopupLink('USGS site page', ptUsgwMonitoringLocationUrl(p));

    if (links) {
      html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
        '<b>Links</b>' +
        links +
        '</div>';
    }

    html += '</div>';
    return html;
  }

  function ptUsgwFeatureCoords(feature) {
    if (!feature || !feature.geometry || feature.geometry.type !== 'Point') return null;
    var coords = feature.geometry.coordinates || [];
    if (coords.length < 2) return null;
    var lng = ptUsgwNum(coords[0]);
    var lat = ptUsgwNum(coords[1]);
    if (lat === null || lng === null) return null;
    if (lat < -90 || lat > 90 || lng < -180 || lng > 180) return null;
    return [lat, lng];
  }

  function ptUsgwAnnotateCoLocatedCounts(features) {
    var counts = {};

    (features || []).forEach(function(feature) {
      var coords = ptUsgwFeatureCoords(feature);
      if (!coords) return;
      var key = ptUsgwCoordKey(coords);
      counts[key] = (counts[key] || 0) + 1;
    });

    (features || []).forEach(function(feature) {
      var p = feature && feature.properties ? feature.properties : null;
      var coords = ptUsgwFeatureCoords(feature);
      if (!p || !coords) return;
      var key = ptUsgwCoordKey(coords);
      p._pt_usgw_coord_key = key;
      p._pt_usgw_colocated_count = counts[key] || 0;
    });
  }

  function ptUsgwFilterStateIsActive(state) {
    state = state || {};
    return ptUsgwNum(state.ageMin) !== null ||
      ptUsgwNum(state.ageMax) !== null ||
      ptUsgwNum(state.depthMin) !== null ||
      ptUsgwNum(state.depthMax) !== null ||
      ptUsgwNum(state.blmDistMax) !== null ||
      state.onBlmOnly === true ||
      state.nestedOnly === true;
  }

  function ptUsgwFeaturePassesFilters(feature, state) {
    state = state || {};
    var p = feature && feature.properties ? feature.properties : {};
    var dtw = ptUsgwNum(p.latest_wl_ft_bgs);
    var age = ptUsgwNum(p.latest_age_days);

    // The live layer only maps features with a usable latest DTW value.
    if (dtw === null) return false;

    var ageMin = ptUsgwNum(state.ageMin);
    var ageMax = ptUsgwNum(state.ageMax);
    var depthMin = ptUsgwNum(state.depthMin);
    var depthMax = ptUsgwNum(state.depthMax);
    var blmDistMax = ptUsgwNum(state.blmDistMax);
    var distToBlm = ptUsgwNum(p.dist_to_blm_mi);

    if (ageMin !== null && (age === null || age < ageMin)) return false;
    if (ageMax !== null && (age === null || age > ageMax)) return false;
    if (depthMin !== null && dtw < depthMin) return false;
    if (depthMax !== null && dtw > depthMax) return false;
    if (state.onBlmOnly === true && ptUsgwBool(p.on_blm_ca) !== true) return false;
    if (blmDistMax !== null && (distToBlm === null || distToBlm > blmDistMax)) return false;
    if (state.nestedOnly === true && ptUsgwNum(p._pt_usgw_colocated_count) <= 1) return false;

    return true;
  }

  function ptUsgwFilterFeatures(features, state) {
    return (features || []).filter(function(feature) {
      return ptUsgwFeaturePassesFilters(feature, state || {});
    });
  }

  function ptUsgwDefaultFilterState() {
    return {ageMin: '', ageMax: '', depthMin: '', depthMax: '', blmDistMax: '', onBlmOnly: false, nestedOnly: false};
  }

  function ptUsgwReadFilterState(root) {
    root = root || document;
    var ageMin = root.querySelector('[data-pt-usgw-filter="ageMin"]');
    var ageMax = root.querySelector('[data-pt-usgw-filter="ageMax"]');
    var depthMin = root.querySelector('[data-pt-usgw-filter="depthMin"]');
    var depthMax = root.querySelector('[data-pt-usgw-filter="depthMax"]');
    var blmDistMax = root.querySelector('[data-pt-usgw-filter="blmDistMax"]');
    var onBlmOnly = root.querySelector('[data-pt-usgw-filter="onBlmOnly"]');
    var nestedOnly = root.querySelector('[data-pt-usgw-filter="nestedOnly"]');

    return {
      ageMin: ageMin ? ageMin.value : '',
      ageMax: ageMax ? ageMax.value : '',
      depthMin: depthMin ? depthMin.value : '',
      depthMax: depthMax ? depthMax.value : '',
      blmDistMax: blmDistMax ? blmDistMax.value : '',
      onBlmOnly: onBlmOnly ? onBlmOnly.checked : false,
      nestedOnly: nestedOnly ? nestedOnly.checked : false
    };
  }

  function ptUsgwSetFilterState(root, state) {
    root = root || document;
    state = state || ptUsgwDefaultFilterState();
    var vals = {
      ageMin: state.ageMin === undefined || state.ageMin === null ? '' : String(state.ageMin),
      ageMax: state.ageMax === undefined || state.ageMax === null ? '' : String(state.ageMax),
      depthMin: state.depthMin === undefined || state.depthMin === null ? '' : String(state.depthMin),
      depthMax: state.depthMax === undefined || state.depthMax === null ? '' : String(state.depthMax),
      blmDistMax: state.blmDistMax === undefined || state.blmDistMax === null ? '' : String(state.blmDistMax)
    };
    Object.keys(vals).forEach(function(k) {
      var el = root.querySelector('[data-pt-usgw-filter="' + k + '"]');
      if (el) el.value = vals[k];
    });
    var onBlmOnly = root.querySelector('[data-pt-usgw-filter="onBlmOnly"]');
    if (onBlmOnly) onBlmOnly.checked = state.onBlmOnly === true;
    var nestedOnly = root.querySelector('[data-pt-usgw-filter="nestedOnly"]');
    if (nestedOnly) nestedOnly.checked = state.nestedOnly === true;
  }

  function ptUsgwDescribeFilterState(state) {
    state = state || {};
    var parts = [];
    var ageMin = ptUsgwNum(state.ageMin);
    var ageMax = ptUsgwNum(state.ageMax);
    var depthMin = ptUsgwNum(state.depthMin);
    var depthMax = ptUsgwNum(state.depthMax);
    var blmDistMax = ptUsgwNum(state.blmDistMax);

    if (ageMin !== null || ageMax !== null) {
      if (ageMin !== null && ageMax !== null) parts.push('age ' + ageMin + '–' + ageMax + ' days');
      else if (ageMin !== null) parts.push('age ≥' + ageMin + ' days');
      else parts.push('age ≤' + ageMax + ' days');
    }
    if (depthMin !== null || depthMax !== null) {
      if (depthMin !== null && depthMax !== null) parts.push('DTW ' + depthMin + '–' + depthMax + ' ft bgs');
      else if (depthMin !== null) parts.push('DTW ≥' + depthMin + ' ft bgs');
      else parts.push('DTW ≤' + depthMax + ' ft bgs');
    }
    if (state.onBlmOnly === true) parts.push('on BLM-CA lands');
    if (blmDistMax !== null) parts.push('within approx. ' + blmDistMax + ' mi of BLM-CA managed lands');
    if (state.nestedOnly === true) parts.push('nested/co-located only');

    return parts.length ? parts.join('; ') : 'none';
  }

  function ptUsgwUpdateFilterCount(root, filteredCount, totalCount, markerCount, visibleCount) {
    if (!root) return;
    var el = root.querySelector('.pt-ops-usgs-groundwater-filter-count');
    if (!el) return;
    var txt = 'Showing ' + Number(filteredCount || 0).toLocaleString() + ' of ' + Number(totalCount || 0).toLocaleString() + ' groundwater site records';
    if (markerCount !== undefined && markerCount !== null) {
      txt += ' (' + Number(markerCount || 0).toLocaleString() + ' map markers before low-zoom clustering; ' + Number(visibleCount || 0).toLocaleString() + ' records in current view)';
    }
    el.textContent = txt;
  }

  function ptUsgwCreateUnifiedCard(onApply, onReset) {
    var root = null;
    var control = L.control({position: 'bottomleft'});

    function stopMapEvent(e) {
      if (!e) return;
      if (e.stopPropagation) e.stopPropagation();
      if (typeof L !== 'undefined' && L.DomEvent && L.DomEvent.stopPropagation) {
        try { L.DomEvent.stopPropagation(e); } catch(err) {}
      }
    }

    function buildRoot() {
      root = L.DomUtil.create('div', 'leaflet-control pt-ops-usgs-groundwater-card pt-map-legend-card');
      var actions = (window.BRIM && window.BRIM.legendCloseout && window.BRIM.legendCloseout.actionsHtml) ?
        window.BRIM.legendCloseout.actionsHtml(
          'pt-ops-usgs-groundwater-card-dock',
          'pt-ops-usgs-groundwater-filter-close',
          'USGS groundwater Ops Live legend and filters'
        ) :
        '<span class="pt-map-card-actions"><button type="button" class="pt-map-card-dock pt-ops-usgs-groundwater-card-dock" title="Undock USGS groundwater Ops Live legend and filters">&#x2197;</button><button type="button" class="pt-map-legend-close pt-ops-usgs-groundwater-filter-close" title="Hide USGS groundwater Ops Live legend and filters">&times;</button></span>';
      var legendBody = typeof opsGroundwaterLegendBodyHtml === 'function' ?
        opsGroundwaterLegendBodyHtml() : '';
      root.innerHTML = '' +
        '<div class="pt-ops-usgs-groundwater-filter-title pt-map-card-handle"><span>USGS GW monitoring wells measured in last 800 days</span>' + actions + '</div>' +
        '<div class="pt-ops-legend-small pt-ops-usgs-groundwater-card-metric">Loading groundwater site records…</div>' +
        '<div class="pt-ops-usgs-groundwater-card-legend">' + legendBody + '</div>' +
        '<div class="pt-ops-usgs-groundwater-filter-subtitle">Filters current Ops Live subset (≤800 days).</div>' +
        '<div class="pt-ops-usgs-groundwater-filter-row"><label>Age</label><input type="number" min="0" step="1" placeholder="min days" data-pt-usgw-filter="ageMin"><input type="number" min="0" step="1" placeholder="max days" data-pt-usgw-filter="ageMax"></div>' +
        '<div class="pt-ops-usgs-groundwater-filter-row"><label>DTW</label><input type="number" step="1" placeholder="min ft" data-pt-usgw-filter="depthMin"><input type="number" step="1" placeholder="max ft" data-pt-usgw-filter="depthMax"></div>' +
        '<div class="pt-ops-usgs-groundwater-filter-row"><label>BLM</label><label class="pt-usgw-filter-nested-inline"><input type="checkbox" data-pt-usgw-filter="onBlmOnly"> on BLM</label><input type="number" min="0" step="0.1" placeholder="max mi" data-pt-usgw-filter="blmDistMax"></div>' +
        '<div class="pt-usgw-filter-hint">DTW is ft bgs; negative values are reported artesian/above land surface. Approx. distance to BLM-CA managed lands.</div>' +
        '<div class="pt-usgw-filter-presets" aria-label="USGS groundwater filter presets">' +
          '<button type="button" data-pt-usgw-preset="90d">Last 90d</button>' +
          '<button type="button" data-pt-usgw-preset="365d">Last 1y</button>' +
          '<button type="button" data-pt-usgw-preset="deep500">DTW ≥500</button>' +
          '<button type="button" data-pt-usgw-preset="shallow25">DTW ≤25</button>' +
          '<button type="button" data-pt-usgw-preset="onblm">On BLM</button>' +
          '<button type="button" data-pt-usgw-preset="blm1">≤1 mi BLM</button>' +
          '<button type="button" data-pt-usgw-preset="blm5">≤5 mi BLM</button>' +
          '<label class="pt-usgw-filter-nested-inline"><input type="checkbox" data-pt-usgw-filter="nestedOnly"> nested/co-located only</label>' +
        '</div>' +
        '<div class="pt-ops-usgs-groundwater-filter-actions"><button type="button" data-pt-usgw-apply="1">Apply</button><button type="button" data-pt-usgw-reset="1">Reset</button></div>' +
        '<div class="pt-ops-usgs-groundwater-filter-count">Filter ready.</div>';

      L.DomEvent.disableClickPropagation(root);
      L.DomEvent.disableScrollPropagation(root);
      ['mousedown', 'mouseup', 'click', 'dblclick', 'contextmenu', 'wheel'].forEach(function(evt) {
        root.addEventListener(evt, stopMapEvent, false);
      });

      var closeButton = root.querySelector('.pt-ops-usgs-groundwater-filter-close');
      if (closeButton) {
        closeButton.addEventListener('click', function(e) {
          if (typeof L !== 'undefined' && L.DomEvent) L.DomEvent.stop(e);
          else if (e && e.preventDefault) { e.preventDefault(); e.stopPropagation(); }
          if (root && root.__brimDetachableState && root.__brimDetachableState.floating) {
            root.__brimDetachableState.dock();
          }
          if (root) root.style.display = 'none';
          if (window.BRIM && window.BRIM.legendCloseout && window.BRIM.legendCloseout.scheduleLayout) {
            window.BRIM.legendCloseout.scheduleLayout();
          }
        });
      }

      var applyButton = root.querySelector('[data-pt-usgw-apply="1"]');
      var resetButton = root.querySelector('[data-pt-usgw-reset="1"]');
      applyButton.style.cursor = 'pointer';
      resetButton.style.cursor = 'pointer';

      applyButton.addEventListener('click', function(e) {
        stopMapEvent(e);
        if (typeof onApply === 'function') onApply(ptUsgwReadFilterState(root), root);
      });

      resetButton.addEventListener('click', function(e) {
        stopMapEvent(e);
        var state = ptUsgwDefaultFilterState();
        ptUsgwSetFilterState(root, state);
        if (typeof onReset === 'function') onReset(state, root);
      });

      var inputs = root.querySelectorAll('[data-pt-usgw-filter]');
      Array.prototype.forEach.call(inputs, function(input) {
        input.addEventListener('keydown', function(e) {
          if (e && e.key === 'Enter') {
            stopMapEvent(e);
            if (typeof onApply === 'function') onApply(ptUsgwReadFilterState(root), root);
          }
        });
      });

      function applyPresetButton(btn, e) {
        stopMapEvent(e);
        if (e && e.preventDefault) e.preventDefault();
        var preset = btn.getAttribute('data-pt-usgw-preset');
        var state = ptUsgwReadFilterState(root);
        if (preset === '90d') {
          state.ageMin = '';
          state.ageMax = '90';
        } else if (preset === '365d') {
          state.ageMin = '';
          state.ageMax = '365';
        } else if (preset === 'deep500') {
          state.depthMin = '500';
          state.depthMax = '';
        } else if (preset === 'shallow25') {
          state.depthMin = '';
          state.depthMax = '25';
        } else if (preset === 'onblm') {
          state.onBlmOnly = true;
          state.blmDistMax = '';
        } else if (preset === 'blm1') {
          state.onBlmOnly = false;
          state.blmDistMax = '1';
        } else if (preset === 'blm5') {
          state.onBlmOnly = false;
          state.blmDistMax = '5';
        }
        ptUsgwSetFilterState(root, state);
        if (typeof onApply === 'function') onApply(state, root);
      }

      var presetButtons = root.querySelectorAll('[data-pt-usgw-preset]');
      Array.prototype.forEach.call(presetButtons, function(btn) {
        btn.style.cursor = 'pointer';
        btn.style.pointerEvents = 'auto';
        btn.addEventListener('click', function(e) {
          applyPresetButton(btn, e);
        }, true);
      });

      return root;
    }

    control.onAdd = function() {
      if (!root) root = buildRoot();
      return root;
    };
    control.ptUsgwRoot = function() { return root; };
    control.ptUsgwDestroy = function() {
      if (root && root.__brimDetachableState) {
        root.__brimDetachableState.destroy(false, false);
      }
      control.remove();
      root = null;
    };
    return control;
  }

  function ptUsgwBuildLayer(markers, features) {
    markers.clearLayers();

    var stats = {
      featureCount: 0,
      drawnCount: 0,
      apiCount: 0,
      fallbackCount: 0,
      latest90dCount: 0,
      latest1yCount: 0,
      latest2yCount: 0,
      artesianOrNegativeCount: 0,
      screenIntervalCount: 0,
      historyPlotCount: 0,
      historyPorPctCount: 0,
      historySeasonalPctCount: 0,
      skippedNoLatestCount: 0,
      coLocatedGroupCount: 0,
      coLocatedWellCount: 0
    };

    var groups = {};
    var order = [];

    (features || []).forEach(function(feature) {
      stats.featureCount += 1;
      var p = feature && feature.properties ? feature.properties : {};
      var dtw = ptUsgwNum(p.latest_wl_ft_bgs);
      var age = ptUsgwNum(p.latest_age_days);
      var coords = ptUsgwFeatureCoords(feature);

      if (!coords) return;

      if (dtw === null) {
        stats.skippedNoLatestCount += 1;
        return;
      }

      var key = ptUsgwCoordKey(coords);
      if (!groups[key]) {
        groups[key] = [];
        order.push(key);
      }
      groups[key].push(feature);

      if (ptUsgwBool(p.has_api_latest_wl)) stats.apiCount += 1;
      else stats.fallbackCount += 1;
      if (age !== null && age <= 90) stats.latest90dCount += 1;
      if (age !== null && age <= 365) stats.latest1yCount += 1;
      if (age !== null && age <= 365 * 2) stats.latest2yCount += 1;
      if (dtw < 0 || ptUsgwBool(p.is_artesian_or_above_land_surface)) stats.artesianOrNegativeCount += 1;
      if (ptUsgwBool(p.has_screen_interval)) stats.screenIntervalCount += 1;
      if (ptUsgwTrim(p.hist_plot_wy_mean_json)) stats.historyPlotCount += 1;
      if (ptUsgwNum(p.hist_por_deeper_pctile) !== null) stats.historyPorPctCount += 1;
      if (ptUsgwNum(p.hist_seasonal_deeper_pctile) !== null) stats.historySeasonalPctCount += 1;
    });

    order.forEach(function(key) {
      var g = groups[key] || [];
      if (g.length === 1) {
        if (ptUsgwAddSingleWellMarker(markers, g[0])) stats.drawnCount += 1;
      } else if (g.length > 1) {
        if (ptUsgwAddNestedMarker(markers, g)) {
          stats.drawnCount += 1;
          stats.coLocatedGroupCount += 1;
          stats.coLocatedWellCount += g.length;
        }
      }
    });

    return stats;
  }

  function ptUsgwMakeMarkerContainer(opts) {
    opts = opts || {};

    if (typeof L !== 'undefined' && typeof L.markerClusterGroup === 'function') {
      return L.markerClusterGroup({
        disableClusteringAtZoom: opts.disableClusteringAtZoom || 9,
        spiderfyOnMaxZoom: false,
        showCoverageOnHover: false,
        chunkedLoading: true,
        removeOutsideVisibleBounds: true,
        maxClusterRadius: opts.maxClusterRadius || 48,
        iconCreateFunction: function(cluster) {
          var count = cluster.getChildCount();
          var size = count < 10 ? 34 : (count < 100 ? 40 : 46);
          var cls = count < 10 ? 'small' : (count < 100 ? 'medium' : 'large');
          return L.divIcon({
            html: '<div><span>' + count.toLocaleString() + '</span></div>',
            className: 'pt-ops-usgs-groundwater-cluster pt-ops-usgs-groundwater-cluster-' + cls,
            iconSize: L.point(size, size)
          });
        }
      });
    }

    return L.layerGroup();
  }

  function makeUsgsGroundwaterLatestLayer(opts) {
    opts = opts || {};

    var layerGroup = L.layerGroup();
    var markers = ptUsgwMakeMarkerContainer(opts);
    var name = opts.name || 'Groundwater: USGS Latest Levels';
    var url = opts.url || '';
    var summaryUrl = opts.summaryUrl || '';
    var allFeatures = [];
    var filteredFeatures = [];
    var currentSummary = null;
    var filterState = ptUsgwDefaultFilterState();
    var filterControl = null;
    var filterControlRoot = null;
    var lastStats = null;
    var layerIsActive = false;
    var activationGeneration = 0;

    function removeFilterControl() {
      if (filterControl) {
        try {
          if (typeof filterControl.ptUsgwDestroy === 'function') {
            filterControl.ptUsgwDestroy();
          } else if (typeof filterControl.remove === 'function') {
            filterControl.remove();
          } else if (layerGroup._map && typeof layerGroup._map.removeControl === 'function') {
            layerGroup._map.removeControl(filterControl);
          }
        } catch(e) {}
      }
      filterControl = null;
      filterControlRoot = null;
    }

    function ensureFilterControl() {
      if (filterControl || !layerGroup._map) return;
      filterControl = ptUsgwCreateUnifiedCard(applyGroundwaterFilter, resetGroundwaterFilter);
      filterControl.addTo(layerGroup._map);
      filterControlRoot = filterControl.ptUsgwRoot ? filterControl.ptUsgwRoot() : null;
      if (window.BRIM && window.BRIM.legendCloseout && window.BRIM.legendCloseout.scheduleLayout) {
        window.BRIM.legendCloseout.scheduleLayout(filterControlRoot);
      }
    }

    function countFeaturesInCurrentView(features) {
      if (!layerGroup._map || !layerGroup._map.getBounds) return Number((features || []).length || 0);
      var bounds = layerGroup._map.getBounds();
      var n = 0;
      (features || []).forEach(function(feature) {
        var coords = ptUsgwFeatureCoords(feature);
        if (!coords) return;
        if (bounds.contains(L.latLng(coords[0], coords[1]))) n += 1;
      });
      return n;
    }

    function updateGroundwaterCardMetric() {
      if (!activeLegendDefs[name]) return;
      var visibleCount = countFeaturesInCurrentView(filteredFeatures);
      var filteredCount = Number((filteredFeatures || []).length || 0);
      var totalCount = Number((allFeatures || []).length || 0);
      var txt = filteredCount === totalCount ?
        ('Current view: ' + visibleCount.toLocaleString() + ' / ' + totalCount.toLocaleString() + ' feed site records.') :
        ('Current view: ' + visibleCount.toLocaleString() + ' / ' + filteredCount.toLocaleString() + ' filtered site records (' + totalCount.toLocaleString() + ' feed site records).');
      activeLegendDefs[name].metricText = txt;
      if (filterControlRoot) {
        var metric = filterControlRoot.querySelector('.pt-ops-usgs-groundwater-card-metric');
        if (metric) metric.textContent = txt;
        ptUsgwUpdateFilterCount(
          filterControlRoot,
          filteredCount,
          totalCount,
          lastStats ? lastStats.drawnCount : filteredCount,
          visibleCount
        );
      }
      if (window.BRIM && window.BRIM.legendCloseout && window.BRIM.legendCloseout.scheduleLayout) {
        window.BRIM.legendCloseout.scheduleLayout(filterControlRoot);
      }
    }

    function renderGroundwaterFeatures(features, reason) {
      filteredFeatures = features || [];
      var stats = ptUsgwBuildLayer(markers, filteredFeatures);
      lastStats = stats;
      if (!layerGroup.hasLayer(markers)) markers.addTo(layerGroup);
      updateGroundwaterCardMetric();

      if (!stats.drawnCount) {
        recordStatus(name, 'USGS groundwater filter returned no mappable points. Active filters: ' + ptUsgwDescribeFilterState(filterState) + '.', 'pt-ops-warn');
        return stats;
      }

      if (reason === 'filter') {
        var msg = 'USGS groundwater filter applied: ' + Number((features || []).length).toLocaleString() + ' of ' + Number(allFeatures.length || 0).toLocaleString() + ' groundwater site records shown as ' + Number(stats.drawnCount || 0).toLocaleString() + ' displayed marker' + (stats.drawnCount === 1 ? '' : 's') + ' before low-zoom clustering. Filters: ' + ptUsgwDescribeFilterState(filterState) + '.';
        if (stats.coLocatedGroupCount) {
          msg += ' ' + stats.coLocatedWellCount.toLocaleString() + ' records grouped into ' + stats.coLocatedGroupCount.toLocaleString() + ' co-located/nested markers.';
        }
        recordStatus(name, msg, 'pt-ops-ok');
      }

      return stats;
    }

    function applyGroundwaterFilter(state, root) {
      filterState = state || ptUsgwDefaultFilterState();
      filterControlRoot = root || filterControlRoot;
      var filtered = ptUsgwFilterFeatures(allFeatures, filterState);
      renderGroundwaterFeatures(filtered, 'filter');
    }

    function resetGroundwaterFilter(state, root) {
      filterState = state || ptUsgwDefaultFilterState();
      filterControlRoot = root || filterControlRoot;
      var filtered = ptUsgwFilterFeatures(allFeatures, filterState);
      renderGroundwaterFeatures(filtered, 'filter');
    }

    layerGroup.on('add', function() {
      layerIsActive = true;
      var requestGeneration = ++activationGeneration;
      // Start each layer activation with a clean filter state.  Otherwise a
      // previously applied filter remains active after layer toggle even though
      // the rebuilt filter panel appears blank.
      filterState = ptUsgwDefaultFilterState();
      ptUsgwEnsureStyle();

      activeLegendDefs[name] = {
        note: opts.note || 'BRIM-hosted GeoJSON of latest USGS groundwater field measurements for active/recent candidate wells in the California-centered query area, including nearby border-basin context where present. Values are depth to water in feet below ground/land surface from parameter 72019 where available; RF029 history fields are shown in popups when present.',
        legendType: 'usgs_groundwater',
        unifiedCard: true,
        sourceUrl: url,
        legendUrl: '',
        infoUrl: summaryUrl || url,
        infoLabel: summaryUrl ? 'summary' : 'GeoJSON',
        legendNote: 'Circle fill color reflects latest depth to water. Dark outline marks older/stale measurements. Popup history percentiles and mini plots are historical context, not a groundwater-storage calculation.'
      };
      redrawLegend();
      ensureFilterControl();

      if (!url) {
        recordStatus(name, 'USGS groundwater GeoJSON URL is not configured.', 'pt-ops-bad');
        return;
      }

      setOpsLayerLoading(name, true);
      recordStatus(name, 'Fetching BRIM-hosted USGS latest groundwater GeoJSON…', 'pt-ops-warn');

      var geoPromise = fetch(url, {cache: 'no-store', headers: {'Accept': 'application/json'}})
        .then(function(resp) {
          if (!resp.ok) throw new Error('GeoJSON HTTP ' + resp.status);
          return resp.json();
        });

      var summaryPromise = summaryUrl ? fetch(summaryUrl, {cache: 'no-store', headers: {'Accept': 'application/json'}})
        .then(function(resp) {
          if (!resp.ok) throw new Error('summary HTTP ' + resp.status);
          return resp.json();
        })
        .catch(function() { return null; }) : Promise.resolve(null);

      Promise.all([geoPromise, summaryPromise])
        .then(function(results) {
          if (!layerIsActive || requestGeneration !== activationGeneration) return;

          var geojson = results[0] || {};
          var summary = results[1] || null;
          var features = Array.isArray(geojson.features) ? geojson.features : [];
          allFeatures = features;
          currentSummary = summary;
          ptUsgwAnnotateCoLocatedCounts(allFeatures);

          var stats = renderGroundwaterFeatures(ptUsgwFilterFeatures(allFeatures, filterState), 'initial');
          setOpsLayerLoading(name, false);
          if (layerGroup._map && typeof layerGroup._map.on === 'function') {
            layerGroup._map.on('moveend zoomend', updateGroundwaterCardMetric);
          }

          if (!stats.drawnCount) {
            recordStatus(name, 'USGS groundwater feed loaded, but no latest groundwater-level points were mappable.', 'pt-ops-warn');
            return;
          }

          var latest2y = (summary && summary.latest_2y_count !== undefined) ? Number(summary.latest_2y_count || 0) : stats.latest2yCount;

          var recordCount = stats.apiCount + stats.fallbackCount;
          var msg = recordCount.toLocaleString() + ' USGS groundwater latest-level site records loaded as ' +
            stats.drawnCount.toLocaleString() + ' displayed marker' + (stats.drawnCount === 1 ? '' : 's') +
            ' before low-zoom clustering. ' +
            stats.latest90dCount.toLocaleString() + ' measured within 90 days; ' +
            stats.latest1yCount.toLocaleString() + ' within 1 year; ' +
            latest2y.toLocaleString() + ' within 2 years.';

          if (stats.fallbackCount) {
            msg += ' ' + stats.fallbackCount.toLocaleString() + ' index-fallback values.';
          }

          if (stats.artesianOrNegativeCount) {
            msg += ' ' + stats.artesianOrNegativeCount.toLocaleString() + ' reported above-land-surface/artesian values.';
          }

          if (stats.coLocatedGroupCount) {
            msg += ' ' + stats.coLocatedWellCount.toLocaleString() + ' records grouped into ' + stats.coLocatedGroupCount.toLocaleString() + ' co-located/nested markers.';
          }

          if (summary && summary.screen_interval_count !== undefined) {
            msg += ' Screen/perf intervals in feed: ' + Number(summary.screen_interval_count || 0).toLocaleString() + '.';
          }

          var plotCount = (summary && summary.history_sites_with_plot_json !== undefined) ? Number(summary.history_sites_with_plot_json || 0) : stats.historyPlotCount;
          var porCount = (summary && summary.history_sites_with_por_percentile !== undefined) ? Number(summary.history_sites_with_por_percentile || 0) : stats.historyPorPctCount;
          var seasonalCount = (summary && summary.history_sites_with_seasonal_percentile !== undefined) ? Number(summary.history_sites_with_seasonal_percentile || 0) : stats.historySeasonalPctCount;

          if (plotCount || porCount || seasonalCount) {
            msg += ' History: ' + plotCount.toLocaleString() + ' mini-plot sites; ' +
              porCount.toLocaleString() + ' POR-percentile sites; ' +
              seasonalCount.toLocaleString() + ' seasonal-percentile sites.';
          }

          if (summary && summary.feed_build_time_utc) {
            msg += ' Feed built ' + ptUsgwFmtPacific(summary.feed_build_time_utc) + '.';
          }

          recordStatus(name, msg, 'pt-ops-ok');
        })
        .catch(function(err) {
          if (!layerIsActive || requestGeneration !== activationGeneration) return;
          setOpsLayerLoading(name, false);
          recordStatus(
            name,
            'Could not fetch BRIM-hosted USGS groundwater feed: ' + (err && err.message ? err.message : err),
            'pt-ops-bad'
          );
        });
    });

    layerGroup.on('remove', function() {
      layerIsActive = false;
      activationGeneration += 1;
      try {
        if (layerGroup._map && typeof layerGroup._map.off === 'function') {
          layerGroup._map.off('moveend zoomend', updateGroundwaterCardMetric);
        }
      } catch(e) {}
      try { markers.clearLayers(); } catch(e) {}
      try { layerGroup.removeLayer(markers); } catch(e) {}
      removeFilterControl();
      filterState = ptUsgwDefaultFilterState();
      allFeatures = [];
      filteredFeatures = [];
      lastStats = null;
      currentSummary = null;
      delete activeLegendDefs[name];
      setOpsLayerLoading(name, false);
      redrawLegend();
      recordStatus(name, 'Layer turned off.', 'pt-ops-muted');
    });

    return layerGroup;
  }

)---"
}
