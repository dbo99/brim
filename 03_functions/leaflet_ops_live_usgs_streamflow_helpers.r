# ==== leaflet_ops_live_usgs_streamflow_helpers.r =============================
##
## PURPOSE:
##   USGS streamflow-specific JavaScript helpers for the BRIM Ops Live panel.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r` and returns browser-side
##   JavaScript for the GitHub-hosted USGS latest streamflow GeoJSON feed.
##
##   The scheduled GitHub feed is responsible for talking to USGS.  The browser
##   only downloads a static GeoJSON + summary JSON from GitHub Pages.  This
##   keeps BRIM users from directly hitting USGS when they open the map.
##
## DISPLAY V1:
##   - Draw only sites with latest continuous discharge or stage.
##   - Skip historical/no-current sites because the static USGS gage layer already
##     provides the full reference backbone.
##   - Scale/color circles by discharge magnitude only; this is NOT flood-status
##     or percentile styling.
## ============================================================================

pt_ops_live_usgs_streamflow_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // USGS latest streamflow layer from BRIM-hosted GitHub GeoJSON.
  // --------------------------------------------------------------------------

  function ptUsgsEnsureStyle() {
    if (document.getElementById('pt-ops-usgs-streamflow-style')) return;

    var style = document.createElement('style');
    style.id = 'pt-ops-usgs-streamflow-style';
    style.innerHTML = `
      .leaflet-tooltip.pt-ops-usgs-streamflow-tooltip {
        background: rgba(255, 255, 255, 0.97);
        border: 1px solid rgba(0,0,0,0.42);
        border-radius: 4px;
        box-shadow: 0 2px 7px rgba(0,0,0,0.22);
        color: #111;
        padding: 5px 7px;
        font: 12px/1.25 Arial, Helvetica, sans-serif;
        white-space: nowrap;
      }

      .pt-ops-usgs-streamflow-popup {
        font: 12px/1.35 Arial, Helvetica, sans-serif;
        min-width: 275px;
        max-width: 430px;
      }

      .pt-ops-usgs-streamflow-popup a {
        color: #1f5e9c;
        text-decoration: none;
      }

      .pt-ops-usgs-streamflow-popup a:hover {
        text-decoration: underline;
      }

      .pt-ops-usgs-streamflow-filter {
        position: absolute !important;
        left: 225px !important;
        top: 186px !important;
        clear: none !important;
        float: none !important;
        z-index: 10050;
        isolation: isolate;
        background: rgba(226, 241, 238, 0.96);
        border: 1px solid rgba(90,120,116,0.55);
        border-radius: 6px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.22);
        padding: 6px 7px;
        width: 230px;
        margin-left: 0 !important;
        margin-top: 0 !important;
        font: 11.5px/1.25 Arial, Helvetica, sans-serif;
        color: #222;
        pointer-events: auto;
      }

      .pt-ops-usgs-streamflow-filter * {
        pointer-events: auto;
      }

      .pt-ops-usgs-streamflow-filter,
      .pt-ops-usgs-streamflow-filter label {
        cursor: default !important;
        user-select: none;
      }

      .pt-ops-usgs-streamflow-filter input[type="number"] {
        cursor: text !important;
        user-select: text;
      }

      .pt-ops-usgs-streamflow-filter input[type="checkbox"],
      .pt-ops-usgs-streamflow-filter .pt-usgsf-filter-inline {
        cursor: pointer !important;
        position: relative;
        z-index: 10060;
        touch-action: manipulation;
      }

      .pt-ops-usgs-streamflow-filter-title {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 8px;
        font-weight: 700;
        font-size: 12px;
        margin-bottom: 1px;
      }

      .pt-ops-usgs-streamflow-filter-close {
        border: 0;
        background: transparent;
        color: #777;
        font: bold 16px/1 Arial, Helvetica, sans-serif;
        padding: 0 1px;
        cursor: pointer;
      }

      .pt-ops-usgs-streamflow-filter-close:hover { color: #222; }

      .pt-ops-usgs-streamflow-filter-subtitle {
        color: #555;
        font-size: 10.5px;
        margin: 0 0 5px 0;
      }

      .pt-ops-usgs-streamflow-filter-row {
        display: grid;
        grid-template-columns: 70px 1fr 1fr;
        column-gap: 4px;
        align-items: center;
        margin: 3px 0;
      }

      .pt-ops-usgs-streamflow-filter-row label {
        font-weight: 700;
      }

      .pt-ops-usgs-streamflow-filter input[type="number"] {
        width: 100%;
        box-sizing: border-box;
        border: 1px solid #bfbfb6;
        border-radius: 3px;
        padding: 2px 3px;
        font: 11px/1.1 Arial, Helvetica, sans-serif;
      }

      .pt-ops-usgs-streamflow-filter .pt-usgsf-filter-hint {
        color: #666;
        font-size: 10.5px;
        margin-top: 1px;
      }

      .pt-ops-usgs-streamflow-filter .pt-usgsf-filter-presets {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 3px;
        margin: 5px 0 4px 0;
      }

      .pt-ops-usgs-streamflow-filter .pt-usgsf-filter-inline {
        display: inline-flex;
        align-items: center;
        gap: 3px;
        white-space: nowrap;
        margin-left: 4px;
        font: 10.5px/1.2 Arial, Helvetica, sans-serif;
        cursor: pointer;
      }

      .pt-ops-usgs-streamflow-filter .pt-usgsf-filter-inline input {
        margin: 0;
        cursor: pointer;
      }

      .pt-ops-usgs-streamflow-filter button {
        border: 1px solid #aaa;
        border-radius: 4px;
        background: #f7f7f2;
        padding: 2px 5px;
        font: 10.5px/1.2 Arial, Helvetica, sans-serif;
        line-height: 1.2;
        cursor: pointer !important;
        position: relative;
        z-index: 10060;
        pointer-events: auto !important;
        touch-action: manipulation;
      }

      .pt-ops-usgs-streamflow-filter button:hover {
        background: #eef5ff;
        border-color: #6b8fbd;
      }

      .pt-ops-usgs-streamflow-filter button:disabled,
      .pt-ops-usgs-streamflow-filter input:disabled {
        opacity: 0.52;
        cursor: not-allowed !important;
      }

      .pt-ops-usgs-streamflow-filter-actions {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 5px;
        margin-top: 4px;
      }

      .pt-ops-usgs-streamflow-filter-actions button {
        flex: 1 1 auto;
        font-weight: 700;
      }

      .pt-ops-usgs-streamflow-filter-count {
        margin-top: 4px;
        color: #555;
        font-size: 10.5px;
      }

      @media (max-width: 760px) {
        .pt-ops-usgs-streamflow-filter {
          left: 10px !important;
          top: 186px !important;
          width: 218px;
        }
      }
    `;
    document.head.appendChild(style);
  }

  function ptUsgsTrim(value) {
    if (value === null || value === undefined) return '';
    var txt = String(value).trim();
    if (!txt || ['NA', 'NULL', 'NAN'].indexOf(txt.toUpperCase()) >= 0) return '';
    return txt;
  }

  function ptUsgsNum(value) {
    if (value === null || value === undefined || typeof value === 'boolean') return null;
    if (typeof value === 'string') {
      value = value.trim();
      if (!value || ['NA', 'NULL', 'NAN'].indexOf(value.toUpperCase()) >= 0) return null;
    }
    var n = Number(value);
    return isFinite(n) ? n : null;
  }

  function ptUsgsBool(value) {
    if (value === true) return true;
    if (value === false || value === null || value === undefined) return false;
    var txt = String(value).trim().toLowerCase();
    return txt === 'true' || txt === 't' || txt === '1' || txt === 'yes' || txt === 'y';
  }

  function ptUsgsFmtNumber(value, digits) {
    var n = ptUsgsNum(value);
    if (n === null) return 'not available';
    return n.toLocaleString(undefined, {
      minimumFractionDigits: digits || 0,
      maximumFractionDigits: digits || 0
    });
  }

  function ptUsgsFmtCfs(value) {
    var n = ptUsgsNum(value);
    if (n === null) return 'not available';
    var digits = Math.abs(n) < 10 && n !== 0 ? 2 : (Math.abs(n) < 100 ? 1 : 0);
    return n.toLocaleString(undefined, {
      minimumFractionDigits: digits,
      maximumFractionDigits: digits
    }) + ' cfs';
  }

  function ptUsgsFmtFt(value) {
    var n = ptUsgsNum(value);
    if (n === null) return 'not available';
    return n.toLocaleString(undefined, {
      minimumFractionDigits: 2,
      maximumFractionDigits: 2
    }) + ' ft';
  }


  function ptUsgsFmtAge(value) {
    var n = ptUsgsNum(value);
    if (n === null) return 'age unknown';
    if (n < 1) return Math.round(n * 60) + ' min old';
    if (n < 48) return n.toFixed(n < 10 ? 1 : 0) + ' hr old';
    return (n / 24).toFixed(1) + ' days old';
  }

  function ptUsgsFmtPacific(value) {
    var raw = ptUsgsTrim(value);
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

  function ptUsgsFmtPacificShort(value) {
    var raw = ptUsgsTrim(value);
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

  function ptUsgsDischargeRadius(q) {
    q = ptUsgsNum(q);
    if (q === null) return 4.8;
    if (q < 0) return 5.0;
    if (q === 0) return 3.8;
    if (q < 1) return 4.6;
    if (q < 10) return 5.4;
    if (q < 100) return 6.4;
    if (q < 1000) return 7.8;
    if (q < 10000) return 9.6;
    if (q < 50000) return 12.0;
    return 14.5;
  }

  function ptUsgsDischargeFill(q, hasStage) {
    q = ptUsgsNum(q);
    if (q === null) return hasStage ? '#D9EAF7' : '#F2F2F2';
    if (q < 0) return '#9E9AC8';
    if (q === 0) return '#F7F7F7';
    if (q < 1) return '#DEEBF7';
    if (q < 10) return '#9ECAE1';
    if (q < 100) return '#4292C6';
    if (q < 1000) return '#08519C';
    if (q < 10000) return '#31A354';
    if (q < 50000) return '#FDAE61';
    return '#D73027';
  }

  function ptUsgsDischargeStroke(q, hasStage) {
    q = ptUsgsNum(q);
    if (q === null) return hasStage ? '#3182BD' : '#777777';
    if (q < 0) return '#54278F';
    if (q === 0) return '#666666';
    if (q < 100) return '#08519C';
    if (q < 1000) return '#084081';
    if (q < 10000) return '#006D2C';
    if (q < 50000) return '#A63603';
    return '#7F0000';
  }

  function ptUsgsPopupLink(label, url) {
    url = ptUsgsTrim(url);
    if (!url) return '';
    return '<div style="margin:1px 0;"><a href="' + escapeHtml(url) + '" target="_blank" rel="noopener">' + escapeHtml(label) + '</a></div>';
  }

  function ptUsgsStationName(p) {
    return ptUsgsTrim(p.station_nm) || ptUsgsTrim(p.name) || 'USGS streamgage';
  }

  function ptUsgsTooltip(p) {
    p = p || {};

    var q = ptUsgsNum(p.q_cfs);
    var stage = ptUsgsNum(p.stage_ft);
    var siteNo = ptUsgsTrim(p.site_no);
    var nwsli = ptUsgsTrim(p.nwsli);
    var qTime = ptUsgsTrim(p.q_datetime_utc);
    var stageTime = ptUsgsTrim(p.stage_datetime_utc);

    var html = '<b>' + escapeHtml(siteNo || 'USGS') + '</b>';
    html += ' – ' + escapeHtml(ptUsgsStationName(p));

    if (q !== null) {
      html += '<br>Q / QR: <b>' + escapeHtml(ptUsgsFmtCfs(q)) + '</b>';
      if (qTime) html += '<br>Obs: ' + escapeHtml(ptUsgsFmtPacificShort(qTime));
      html += '<br>Age at feed: ' + escapeHtml(ptUsgsFmtAge(p.q_obs_age_hours));
    } else if (stage !== null) {
      html += '<br>HG only: <b>' + escapeHtml(ptUsgsFmtFt(stage)) + '</b>';
      if (stageTime) html += '<br>Obs: ' + escapeHtml(ptUsgsFmtPacificShort(stageTime));
      html += '<br>Age at feed: ' + escapeHtml(ptUsgsFmtAge(p.stage_obs_age_hours));
    }

    if (q !== null && stage !== null) html += '<br>HG: ' + escapeHtml(ptUsgsFmtFt(stage));
    if (nwsli) html += '<br>NWSLI: ' + escapeHtml(nwsli);

    return html;
  }


  function ptUsgsCnrfcAvailabilitySummary(p) {
    p = p || {};
    var parts = [];
    if (ptUsgsBool(p.has_cnrfc_obs_page)) parts.push('obs');
    if (ptUsgsBool(p.has_cnrfc_deterministic_forecast)) parts.push('det');
    if (ptUsgsBool(p.has_cnrfc_ensemble_forecast)) parts.push('ens');
    if (parts.length) return parts.join(', ');
    if (ptUsgsTrim(p.nwsli)) return 'none confirmed';
    return 'not crosswalked';
  }

  function ptUsgsHistoryHtml(p) {
    var n = ptUsgsNum(p.q_hist_n);
    if (n === null || n <= 0) return '';

    return '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
      '<b>Recent daily-flow context</b>' +
      '<br><b>Source:</b> ' + escapeHtml(ptUsgsTrim(p.history_source) || 'daily values') +
      '<br><b>Window:</b> ' + escapeHtml(ptUsgsTrim(p.q_hist_start_date) || '?') + ' to ' + escapeHtml(ptUsgsTrim(p.q_hist_end_date) || '?') +
      '<br><b>Daily mean min/max:</b> ' + escapeHtml(ptUsgsFmtCfs(p.q_3day_min_cfs)) + ' / ' + escapeHtml(ptUsgsFmtCfs(p.q_3day_max_cfs)) +
      '<br><b>Daily mean change:</b> ' + escapeHtml(ptUsgsFmtCfs(p.q_3day_change_cfs)) +
      '</div>';
  }

  function ptUsgsBlmDistanceHtml(p) {
    p = p || {};
    var hasDistField = Object.prototype.hasOwnProperty.call(p, 'dist_to_blm_mi') || Object.prototype.hasOwnProperty.call(p, 'dist_to_blm_ft') || Object.prototype.hasOwnProperty.call(p, 'on_blm_ca');
    if (!hasDistField) return '';

    var onBlm = ptUsgsBool(p.on_blm_ca);
    var distMi = ptUsgsNum(p.dist_to_blm_mi);
    var distFt = ptUsgsNum(p.dist_to_blm_ft);
    var distTxt = 'not available';

    if (onBlm) {
      distTxt = 'on BLM-CA managed lands';
    } else if (distMi !== null) {
      distTxt = distMi.toLocaleString(undefined, {maximumFractionDigits: 3}) + ' mi';
      if (distMi < 0.2 && distFt !== null) {
        distTxt += ' (' + Math.round(distFt).toLocaleString() + ' ft)';
      }
    }

    return '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
      '<b>BLM-CA lands screening</b>' +
      '<br><b>Approx. distance to BLM-CA managed lands:</b> ' + escapeHtml(distTxt) +
      '<br><span style="color:#555;">Distance calculated from projected CA Albers geometry for screening only; interpret with source boundary date and gage-coordinate precision.</span>' +
      '</div>';
  }

  function ptUsgsPopup(p) {
    p = p || {};

    var siteNo = ptUsgsTrim(p.site_no);
    var q = ptUsgsNum(p.q_cfs);
    var stage = ptUsgsNum(p.stage_ft);
    var nwsli = ptUsgsTrim(p.nwsli);
    var cdecId = ptUsgsTrim(p.cdec_id);
    var status = ptUsgsTrim(p.latest_status) || 'latest status not reported';

    var html = '<div class="pt-ops-usgs-streamflow-popup">' +
      '<div style="font-weight:700;font-size:13px;margin-bottom:2px;">' +
      escapeHtml(siteNo || 'USGS') + ' – ' + escapeHtml(ptUsgsStationName(p)) +
      '</div>' +
      '<div style="color:#555;font-size:11px;margin-bottom:5px;">USGS Water Data latest continuous values</div>';

    html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:4px;">' +
      '<b>Latest observation</b>' +
      '<br><b>Q / QR discharge:</b> ' + escapeHtml(ptUsgsFmtCfs(q)) +
      '<br><b>Q obs time:</b> ' + escapeHtml(ptUsgsFmtPacificShort(p.q_datetime_utc)) +
      '<br><b>Q age at feed:</b> ' + escapeHtml(ptUsgsFmtAge(p.q_obs_age_hours)) +
      '<br><b>HG stage:</b> ' + escapeHtml(ptUsgsFmtFt(stage)) +
      '<br><b>HG obs time:</b> ' + escapeHtml(ptUsgsFmtPacificShort(p.stage_datetime_utc)) +
      '<br><b>Status:</b> ' + escapeHtml(status.replace(/_/g, ' ')) +
      '</div>';

    html += ptUsgsHistoryHtml(p);

    html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
      '<b>Station IDs</b>' +
      '<br><b>USGS:</b> ' + escapeHtml(siteNo || 'not listed') +
      '<br><b>NWSLI:</b> ' + escapeHtml(nwsli || 'not crosswalked') +
      '<br><b>CDEC:</b> ' + escapeHtml(cdecId || 'not crosswalked') +
      '<br><b>CNRFC links:</b> ' + escapeHtml(ptUsgsCnrfcAvailabilitySummary(p)) +
      '<br><span style="color:#555;">Other USGS parameters may be available at this site.</span>' +
      '</div>';

    html += ptUsgsBlmDistanceHtml(p);

    html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
      '<b>Source / caveat</b>' +
      '<br>Latest values are from the USGS Water Data API latest-continuous feed, refreshed by the BRIM GitHub workflow.' +
      '<br><span style="color:#555;">Circle size/color reflects discharge magnitude only, not flood status, percentile, or site-specific normal.</span>' +
      '<br><span style="color:#555;">Feed built: ' + escapeHtml(ptUsgsFmtPacificShort(p.feed_build_time_utc)) + '</span>' +
      '</div>';

    var links =
      ptUsgsPopupLink('Open USGS 7-day flow plot', p.usgs_7day_flow_plot_url || p.usgs_hydrograph_url) +
      ptUsgsPopupLink('Open USGS monitoring-location page', p.usgs_monitoring_location_url) +
      ptUsgsPopupLink('Open USGS official rating table', p.usgs_rating_stac_url || p.usgs_rating_depot_url) +
      ptUsgsPopupLink('Open legacy USGS Rating Depot', p.usgs_rating_depot_url) +
      ptUsgsPopupLink('Open CNRFC observed page', p.cnrfc_obs_url) +
      ptUsgsPopupLink('Open CNRFC deterministic forecast', p.cnrfc_forecast_url) +
      ptUsgsPopupLink('Open CNRFC ensemble products', p.cnrfc_ensemble_url) +
      ptUsgsPopupLink('Open HADS metadata', p.hads_metadata_url) +
      ptUsgsPopupLink('Open APRFC gage analysis tool', p.aprfc_gage_analysis_url);

    if (links) {
      html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
        '<b>Links</b>' +
        '<div style="color:#555;font-size:11px;margin:1px 0 3px 0;">CNRFC obs/forecast links are shown only when the crosswalked NWSLI appears on checked CNRFC product pages. HADS/APRFC links are NWSLI-based templates. Rating-table links are generated from USGS templates and may be unavailable for sites without traditional stage-discharge ratings.</div>' +
        links +
        '</div>';
    }

    html += '</div>';
    return html;
  }

  function ptUsgsFeatureCoords(feature) {
    if (!feature || !feature.geometry || feature.geometry.type !== 'Point') return null;
    var coords = feature.geometry.coordinates || [];
    if (coords.length < 2) return null;
    var lng = ptUsgsNum(coords[0]);
    var lat = ptUsgsNum(coords[1]);
    if (lat === null || lng === null) return null;
    if (lat < -90 || lat > 90 || lng < -180 || lng > 180) return null;
    return [lat, lng];
  }

  function ptUsgsDefaultFilterState() {
    return {flowMin: '', flowMax: '', blmDistMax: '', onBlmOnly: false};
  }

  function ptUsgsFeaturePassesFilters(feature, state) {
    state = state || {};
    var p = feature && feature.properties ? feature.properties : {};
    var q = ptUsgsNum(p.q_cfs);
    var stage = ptUsgsNum(p.stage_ft);

    // The live layer maps sites with either discharge or stage.
    if (q === null && stage === null) return false;

    var flowMin = ptUsgsNum(state.flowMin);
    var flowMax = ptUsgsNum(state.flowMax);
    var blmDistMax = ptUsgsNum(state.blmDistMax);
    var distToBlm = ptUsgsNum(p.dist_to_blm_mi);

    if (flowMin !== null && (q === null || q < flowMin)) return false;
    if (flowMax !== null && (q === null || q > flowMax)) return false;
    if (state.onBlmOnly === true && ptUsgsBool(p.on_blm_ca) !== true) return false;
    if (blmDistMax !== null && (distToBlm === null || distToBlm > blmDistMax)) return false;

    return true;
  }

  function ptUsgsFilterFeatures(features, state) {
    return (features || []).filter(function(feature) {
      return ptUsgsFeaturePassesFilters(feature, state || {});
    });
  }

  function ptUsgsBlmDistanceFieldStatus(features) {
    features = features || [];
    var status = {
      featureCount: Number(features.length || 0),
      keysPresent: false,
      populated: false,
      sparse: false,
      distMiCount: 0,
      distFtCount: 0,
      onBlmCount: 0,
      coveragePct: 0
    };

    for (var i = 0; i < features.length; i++) {
      var p = features[i] && features[i].properties ? features[i].properties : {};
      var hasMiKey = Object.prototype.hasOwnProperty.call(p, 'dist_to_blm_mi');
      var hasFtKey = Object.prototype.hasOwnProperty.call(p, 'dist_to_blm_ft');
      var hasOnKey = Object.prototype.hasOwnProperty.call(p, 'on_blm_ca');

      if (hasMiKey || hasFtKey || hasOnKey) status.keysPresent = true;

      if (hasMiKey && ptUsgsNum(p.dist_to_blm_mi) !== null) {
        status.distMiCount += 1;
      }
      if (hasFtKey && ptUsgsNum(p.dist_to_blm_ft) !== null) {
        status.distFtCount += 1;
      }
      if (hasOnKey && ptUsgsBool(p.on_blm_ca) !== null) {
        status.onBlmCount += 1;
      }
    }

    // Require broad distance coverage, not just one populated row. Sparse
    // partial fields can otherwise make BLM controls look enabled while most
    // gages still report "not available" in their popups.
    if (status.featureCount > 0) {
      status.coveragePct = Math.round(1000 * status.distMiCount / status.featureCount) / 10;
      var minNeeded = Math.max(1, Math.floor(status.featureCount * 0.95));
      status.populated = status.distMiCount >= minNeeded;
      status.sparse = status.keysPresent && status.distMiCount > 0 && !status.populated;
    }

    return status;
  }

  function ptUsgsHasBlmDistanceFields(features) {
    return ptUsgsBlmDistanceFieldStatus(features).populated === true;
  }

  function ptUsgsBlmFilterRequested(state) {
    state = state || {};
    return state.onBlmOnly === true || ptUsgsNum(state.blmDistMax) !== null;
  }

  function ptUsgsClearBlmFilterState(state) {
    state = state || ptUsgsDefaultFilterState();
    state.onBlmOnly = false;
    state.blmDistMax = '';
    return state;
  }

  function ptUsgsSetBlmControlsAvailable(root, available, fieldStatus) {
    if (!root) return;
    fieldStatus = fieldStatus || {featureCount: 0, keysPresent: false, populated: false, sparse: false, distMiCount: 0, distFtCount: 0, onBlmCount: 0, coveragePct: 0};

    var els = root.querySelectorAll('[data-pt-usgsf-blm-control="1"]');
    Array.prototype.forEach.call(els, function(el) {
      el.disabled = !available;
      if (!available && el.type === 'checkbox') el.checked = false;
      if (!available && el.tagName === 'INPUT' && el.type !== 'checkbox') el.value = '';
    });

    var hint = root.querySelector('[data-pt-usgsf-blm-hint="1"]');
    if (hint) {
      if (available) {
        hint.textContent = 'Flow is raw latest discharge magnitude. BLM distance uses projected CA Albers screening geometry.';
      } else if (fieldStatus.sparse) {
        hint.textContent = 'Flow filter is available now. BLM fields are sparse in the hosted feed (' + Number(fieldStatus.distMiCount || 0).toLocaleString() + ' / ' + Number(fieldStatus.featureCount || 0).toLocaleString() + ' distance values). Rerun updater/feed with the enriched station index.';
      } else if (fieldStatus.keysPresent) {
        hint.textContent = 'Flow filter is available now. BLM distance fields are present in the hosted feed but have no populated values.';
      } else {
        hint.textContent = 'Flow filter is available now. BLM filters need refreshed streamflow distance fields in the hosted feed.';
      }
    }
  }

  function ptUsgsReadFilterState(root) {
    root = root || document;
    var flowMin = root.querySelector('[data-pt-usgsf-filter="flowMin"]');
    var flowMax = root.querySelector('[data-pt-usgsf-filter="flowMax"]');
    var blmDistMax = root.querySelector('[data-pt-usgsf-filter="blmDistMax"]');
    var onBlmOnly = root.querySelector('[data-pt-usgsf-filter="onBlmOnly"]');

    return {
      flowMin: flowMin ? flowMin.value : '',
      flowMax: flowMax ? flowMax.value : '',
      blmDistMax: blmDistMax ? blmDistMax.value : '',
      onBlmOnly: onBlmOnly ? onBlmOnly.checked : false
    };
  }

  function ptUsgsSetFilterState(root, state) {
    root = root || document;
    state = state || ptUsgsDefaultFilterState();
    var vals = {
      flowMin: state.flowMin === undefined || state.flowMin === null ? '' : String(state.flowMin),
      flowMax: state.flowMax === undefined || state.flowMax === null ? '' : String(state.flowMax),
      blmDistMax: state.blmDistMax === undefined || state.blmDistMax === null ? '' : String(state.blmDistMax)
    };
    Object.keys(vals).forEach(function(k) {
      var el = root.querySelector('[data-pt-usgsf-filter="' + k + '"]');
      if (el) el.value = vals[k];
    });
    var onBlmOnly = root.querySelector('[data-pt-usgsf-filter="onBlmOnly"]');
    if (onBlmOnly) onBlmOnly.checked = state.onBlmOnly === true;
  }

  function ptUsgsDescribeFilterState(state) {
    state = state || {};
    var parts = [];
    var flowMin = ptUsgsNum(state.flowMin);
    var flowMax = ptUsgsNum(state.flowMax);
    var blmDistMax = ptUsgsNum(state.blmDistMax);

    if (flowMin !== null || flowMax !== null) {
      if (flowMin !== null && flowMax !== null) parts.push('flow ' + flowMin.toLocaleString() + '–' + flowMax.toLocaleString() + ' cfs');
      else if (flowMin !== null) parts.push('flow ≥' + flowMin.toLocaleString() + ' cfs');
      else parts.push('flow ≤' + flowMax.toLocaleString() + ' cfs');
    }
    if (state.onBlmOnly === true) parts.push('on BLM-CA lands');
    if (blmDistMax !== null) parts.push('within approx. ' + blmDistMax + ' mi of BLM-CA managed lands');

    return parts.length ? parts.join('; ') : 'none';
  }

  function ptUsgsUpdateFilterCount(root, filteredCount, totalCount, markerCount, visibleCount) {
    if (!root) return;
    var el = root.querySelector('.pt-ops-usgs-streamflow-filter-count');
    if (!el) return;
    var txt = 'Showing ' + Number(filteredCount || 0).toLocaleString() + ' of ' + Number(totalCount || 0).toLocaleString() + ' streamgage site records';
    txt += ' (' + Number(markerCount || 0).toLocaleString() + ' map markers; ' + Number(visibleCount || 0).toLocaleString() + ' in current view)';
    el.textContent = txt;
  }

  function ptUsgsCreateFilterControl(onApply, onReset) {
    var root = null;
    var mapRef = null;

    function stopMapPropagation(e) {
      if (!e) return;
      if (e.stopPropagation) e.stopPropagation();
      if (typeof L !== 'undefined' && L.DomEvent && L.DomEvent.stopPropagation) {
        try { L.DomEvent.stopPropagation(e); } catch(err) {}
      }
      e.cancelBubble = true;
    }

    function stopMapButtonEvent(e) {
      stopMapPropagation(e);
      if (e && e.preventDefault) e.preventDefault();
      return false;
    }

    function buildRoot() {
      root = L.DomUtil.create('div', 'pt-ops-usgs-streamflow-filter');
      // Match the USGS groundwater filter pattern: append the filter directly
      // to the map container, not inside Leaflet's top-left control stack.
      // The normal control stack can leave transparent overlap over the upper
      // preset buttons, which makes only the lower strip of some buttons click.
      root.style.position = 'absolute';
      root.style.left = '225px';
      root.style.top = '186px';
      root.style.marginLeft = '0';
      root.style.marginTop = '0';
      root.style.zIndex = '10050';
      root.style.pointerEvents = 'auto';
      root.style.background = 'rgba(226, 241, 238, 0.96)';
      root.style.width = '230px';
      root.innerHTML = '' +
        '<div class="pt-ops-usgs-streamflow-filter-title"><span>USGS streamflow Ops Live filters</span><button type="button" class="pt-ops-usgs-streamflow-filter-close" data-pt-usgsf-filter-close="1" title="Hide these filters">&times;</button></div>' +
        '<div class="pt-ops-usgs-streamflow-filter-subtitle">Filters current CA Ops Live subset.</div>' +
        '<div class="pt-ops-usgs-streamflow-filter-row"><label>Flow</label><input type="number" min="0" step="1" placeholder="min cfs" data-pt-usgsf-filter="flowMin"><input type="number" min="0" step="1" placeholder="max cfs" data-pt-usgsf-filter="flowMax"></div>' +
        '<div class="pt-ops-usgs-streamflow-filter-row"><label>BLM</label><label class="pt-usgsf-filter-inline" data-pt-usgsf-onblm-label="1"><input type="checkbox" data-pt-usgsf-filter="onBlmOnly" data-pt-usgsf-blm-control="1"> on BLM</label><input type="number" min="0" step="0.1" placeholder="max mi" data-pt-usgsf-filter="blmDistMax" data-pt-usgsf-blm-control="1"></div>' +
        '<div class="pt-usgsf-filter-hint" data-pt-usgsf-blm-hint="1">Flow is raw latest discharge magnitude. BLM distance uses projected CA Albers screening geometry when feed fields are present.</div>' +
        '<div class="pt-usgsf-filter-presets" aria-label="USGS streamflow filter presets">' +
          '<button type="button" data-pt-usgsf-preset="q100">Q ≥100</button>' +
          '<button type="button" data-pt-usgsf-preset="q1000">Q ≥1k</button>' +
          '<button type="button" data-pt-usgsf-preset="q10000">Q ≥10k</button>' +
          '<button type="button" data-pt-usgsf-preset="onblm" data-pt-usgsf-blm-control="1">On BLM</button>' +
          '<button type="button" data-pt-usgsf-preset="blm1" data-pt-usgsf-blm-control="1">≤1 mi BLM</button>' +
          '<button type="button" data-pt-usgsf-preset="blm5" data-pt-usgsf-blm-control="1">≤5 mi BLM</button>' +
        '</div>' +
        '<div class="pt-ops-usgs-streamflow-filter-actions"><button type="button" data-pt-usgsf-apply="1">Apply</button><button type="button" data-pt-usgsf-reset="1">Reset</button></div>' +
        '<div class="pt-ops-usgs-streamflow-filter-count">Filter ready.</div>';

      L.DomEvent.disableClickPropagation(root);
      L.DomEvent.disableScrollPropagation(root);
      ['mousedown', 'mouseup', 'click', 'dblclick', 'contextmenu', 'wheel'].forEach(function(evt) {
        root.addEventListener(evt, stopMapPropagation, false);
      });

      var applyButton = root.querySelector('[data-pt-usgsf-apply="1"]');
      var resetButton = root.querySelector('[data-pt-usgsf-reset="1"]');
      applyButton.style.cursor = 'pointer';
      resetButton.style.cursor = 'pointer';

      applyButton.addEventListener('click', function(e) {
        stopMapButtonEvent(e);
        if (typeof onApply === 'function') onApply(ptUsgsReadFilterState(root), root);
      });

      resetButton.addEventListener('click', function(e) {
        stopMapButtonEvent(e);
        var state = ptUsgsDefaultFilterState();
        ptUsgsSetFilterState(root, state);
        if (typeof onReset === 'function') onReset(state, root);
      });

      var closeButton = root.querySelector('[data-pt-usgsf-filter-close="1"]');
      if (closeButton) {
        closeButton.addEventListener('click', function(e) {
          if (typeof L !== 'undefined' && L.DomEvent) L.DomEvent.stop(e);
          else if (e && e.preventDefault) { e.preventDefault(); e.stopPropagation(); }
          if (root) root.style.display = 'none';
        });
      }

      var inputs = root.querySelectorAll('[data-pt-usgsf-filter]');
      Array.prototype.forEach.call(inputs, function(input) {
        input.addEventListener('keydown', function(e) {
          if (e && e.key === 'Enter') {
            stopMapButtonEvent(e);
            if (typeof onApply === 'function') onApply(ptUsgsReadFilterState(root), root);
          }
        });
      });

      var onBlmCheckbox = root.querySelector('[data-pt-usgsf-filter="onBlmOnly"]');
      var onBlmLabel = root.querySelector('[data-pt-usgsf-onblm-label="1"]');
      if (onBlmCheckbox) {
        onBlmCheckbox.style.cursor = 'pointer';
        ['pointerdown', 'mousedown', 'mouseup', 'dblclick'].forEach(function(evt) {
          onBlmCheckbox.addEventListener(evt, stopMapPropagation, true);
        });
      }
      if (onBlmLabel) {
        onBlmLabel.style.cursor = 'pointer';
        ['pointerdown', 'mousedown', 'mouseup', 'dblclick'].forEach(function(evt) {
          onBlmLabel.addEventListener(evt, stopMapPropagation, true);
        });
        onBlmLabel.addEventListener('click', function(e) {
          stopMapPropagation(e);
          if (!onBlmCheckbox || onBlmCheckbox.disabled) return;
          if (e && e.target !== onBlmCheckbox) {
            if (e.preventDefault) e.preventDefault();
            onBlmCheckbox.checked = !onBlmCheckbox.checked;
          }
        }, true);
      }

      function applyPresetButton(btn, e) {
        stopMapButtonEvent(e);
        var preset = btn.getAttribute('data-pt-usgsf-preset');
        var state = ptUsgsReadFilterState(root);
        if (preset === 'q100') {
          state.flowMin = '100';
          state.flowMax = '';
        } else if (preset === 'q1000') {
          state.flowMin = '1000';
          state.flowMax = '';
        } else if (preset === 'q10000') {
          state.flowMin = '10000';
          state.flowMax = '';
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
        ptUsgsSetFilterState(root, state);
        if (typeof onApply === 'function') onApply(state, root);
      }

      var presetButtons = root.querySelectorAll('[data-pt-usgsf-preset]');
      Array.prototype.forEach.call(presetButtons, function(btn) {
        btn.style.cursor = 'pointer';
        btn.style.pointerEvents = 'auto';
        btn.addEventListener('click', function(e) {
          applyPresetButton(btn, e);
        }, true);
      });

      return root;
    }

    return {
      addTo: function(map) {
        mapRef = map;
        if (!root) root = buildRoot();
        var container = map && map.getContainer ? map.getContainer() : null;
        if (container && root.parentNode !== container) container.appendChild(root);
        return this;
      },
      remove: function() {
        if (root && root.parentNode) root.parentNode.removeChild(root);
        root = null;
        mapRef = null;
      },
      ptUsgsRoot: function() {
        return root;
      }
    };
  }

  function ptUsgsBuildLayer(markers, features) {
    markers.clearLayers();

    var stats = {
      featureCount: 0,
      drawnCount: 0,
      dischargeCount: 0,
      stageOnlyCount: 0,
      skippedNoLatestCount: 0,
      stale6hCount: 0,
      maxQ: null
    };

    (features || []).forEach(function(feature) {
      stats.featureCount += 1;
      var p = feature && feature.properties ? feature.properties : {};
      var q = ptUsgsNum(p.q_cfs);
      var stage = ptUsgsNum(p.stage_ft);
      var coords = ptUsgsFeatureCoords(feature);

      if (!coords) return;

      if (q === null && stage === null) {
        stats.skippedNoLatestCount += 1;
        return;
      }

      var hasStage = stage !== null;
      var marker = L.circleMarker(coords, {
        radius: ptUsgsDischargeRadius(q),
        color: ptUsgsDischargeStroke(q, hasStage),
        weight: q === null ? 1.1 : 1.45,
        opacity: 0.90,
        fillColor: ptUsgsDischargeFill(q, hasStage),
        fillOpacity: q === null ? 0.42 : 0.76,
        dashArray: q === null ? '3 3' : null,
        pane: 'pane_ops'
      });

      marker.bindTooltip(ptUsgsTooltip(p), {
        direction: 'top',
        sticky: true,
        opacity: 0.97,
        className: 'pt-ops-usgs-streamflow-tooltip'
      });
      marker.bindPopup(ptUsgsPopup(p));
      marker.addTo(markers);

      stats.drawnCount += 1;
      if (q !== null) {
        stats.dischargeCount += 1;
        if (stats.maxQ === null || q > stats.maxQ) stats.maxQ = q;
      } else if (stage !== null) {
        stats.stageOnlyCount += 1;
      }

      if (ptUsgsBool(p.q_stale_6h)) stats.stale6hCount += 1;
    });

    return stats;
  }

  function makeUsgsStreamflowLatestLayer(opts) {
    opts = opts || {};

    var layerGroup = L.layerGroup();
    var markers = L.layerGroup();
    var name = opts.name || 'Streamflow: USGS NWIS Latest';
    var url = opts.url || '';
    var summaryUrl = opts.summaryUrl || '';
    var allFeatures = [];
    var filteredFeatures = [];
    var filterState = ptUsgsDefaultFilterState();
    var filterControl = null;
    var filterControlRoot = null;
    var lastStats = null;
    var blmFieldsAvailable = false;
    var blmFieldStatus = {featureCount: 0, keysPresent: false, populated: false, sparse: false, distMiCount: 0, distFtCount: 0, onBlmCount: 0, coveragePct: 0};

    function removeFilterControl() {
      if (filterControl) {
        try {
          if (typeof filterControl.remove === 'function') {
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
      filterControl = ptUsgsCreateFilterControl(applyStreamflowFilter, resetStreamflowFilter);
      filterControl.addTo(layerGroup._map);
      filterControlRoot = filterControl.ptUsgsRoot ? filterControl.ptUsgsRoot() : null;
      ptUsgsSetBlmControlsAvailable(filterControlRoot, blmFieldsAvailable, blmFieldStatus);
    }

    function countFeaturesInCurrentView(features) {
      if (!layerGroup._map || !layerGroup._map.getBounds) return Number((features || []).length || 0);
      var bounds = layerGroup._map.getBounds();
      var n = 0;
      (features || []).forEach(function(feature) {
        var coords = ptUsgsFeatureCoords(feature);
        if (!coords) return;
        if (bounds.contains(L.latLng(coords[0], coords[1]))) n += 1;
      });
      return n;
    }

    function updateStreamflowLegendMetric() {
      if (!activeLegendDefs[name] || typeof ptOpsSetLegendMetric !== 'function') return;
      var visibleCount = countFeaturesInCurrentView(filteredFeatures);
      var filteredCount = Number((filteredFeatures || []).length || 0);
      var totalCount = Number((allFeatures || []).length || 0);
      var txt = '';
      if (filteredCount === totalCount) {
        txt = 'Current view: ' + visibleCount.toLocaleString() + ' / ' + totalCount.toLocaleString() + ' feed site records.';
      } else {
        txt = 'Current view: ' + visibleCount.toLocaleString() + ' / ' + filteredCount.toLocaleString() + ' filtered site records (' + totalCount.toLocaleString() + ' feed site records).';
      }
      ptOpsSetLegendMetric(name, txt);
      if (filterControlRoot) {
        ptUsgsUpdateFilterCount(filterControlRoot, filteredCount, totalCount, lastStats ? lastStats.drawnCount : filteredCount, visibleCount);
      }
    }

    function renderStreamflowFeatures(features, reason) {
      filteredFeatures = features || [];
      lastStats = ptUsgsBuildLayer(markers, filteredFeatures);
      if (!layerGroup.hasLayer(markers)) markers.addTo(layerGroup);
      updateStreamflowLegendMetric();

      if (!lastStats.drawnCount) {
        recordStatus(name, 'USGS streamflow filter returned no mappable points. Active filters: ' + ptUsgsDescribeFilterState(filterState) + '.', 'pt-ops-warn');
        return lastStats;
      }

      if (reason === 'filter') {
        var msg = 'USGS streamflow filter applied: ' + Number(filteredFeatures.length || 0).toLocaleString() + ' of ' + Number(allFeatures.length || 0).toLocaleString() + ' streamgage site records shown as ' + Number(lastStats.drawnCount || 0).toLocaleString() + ' displayed marker' + (lastStats.drawnCount === 1 ? '' : 's') + '. Filters: ' + ptUsgsDescribeFilterState(filterState) + '.';
        recordStatus(name, msg, 'pt-ops-ok');
      }

      return lastStats;
    }

    function applyStreamflowFilter(state, root) {
      filterState = state || ptUsgsDefaultFilterState();
      filterControlRoot = root || filterControlRoot;
      if (!blmFieldsAvailable && ptUsgsBlmFilterRequested(filterState)) {
        filterState = ptUsgsClearBlmFilterState(filterState);
        ptUsgsSetFilterState(filterControlRoot, filterState);
        recordStatus(name, 'BLM-distance filters need refreshed streamflow distance fields in the hosted feed. Flow filter still applied.', 'pt-ops-warn');
      }
      renderStreamflowFeatures(ptUsgsFilterFeatures(allFeatures, filterState), 'filter');
    }

    function resetStreamflowFilter(state, root) {
      filterState = state || ptUsgsDefaultFilterState();
      filterControlRoot = root || filterControlRoot;
      renderStreamflowFeatures(ptUsgsFilterFeatures(allFeatures, filterState), 'filter');
    }

    layerGroup.on('add', function() {
      // Start each layer activation with a clean filter state.  The previous
      // state lives in this closure after a layer is turned off, but the new
      // filter control opens blank; keeping the old state made filters appear
      // inactive while still being applied.
      filterState = ptUsgsDefaultFilterState();
      ptUsgsEnsureStyle();

      activeLegendDefs[name] = {
        note: opts.note || 'BRIM-hosted GeoJSON of latest USGS Water Data continuous streamflow values for California. Draws only sites with recent latest discharge or stage; static/no-current gages are intentionally skipped in this Ops layer.',
        legendType: 'usgs_streamflow',
        sourceUrl: url,
        legendUrl: '',
        infoUrl: summaryUrl || url,
        infoLabel: summaryUrl ? 'summary' : 'GeoJSON',
        legendNote: 'Circle size and fill color increase with latest discharge magnitude only. Stage-only sites are small dashed blue-gray circles. This is not flood-stage, percentile, or anomaly styling.'
      };
      redrawLegend();

      if (!url) {
        recordStatus(name, 'USGS streamflow GeoJSON URL is not configured.', 'pt-ops-bad');
        return;
      }

      setOpsLayerLoading(name, true);
      recordStatus(name, 'Fetching BRIM-hosted USGS latest streamflow GeoJSON…', 'pt-ops-warn');

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
          var geojson = results[0] || {};
          var summary = results[1] || null;
          allFeatures = Array.isArray(geojson.features) ? geojson.features : [];
          blmFieldStatus = ptUsgsBlmDistanceFieldStatus(allFeatures);
          blmFieldsAvailable = blmFieldStatus.populated === true;
          if (!blmFieldsAvailable && ptUsgsBlmFilterRequested(filterState)) {
            filterState = ptUsgsClearBlmFilterState(filterState);
          }
          filteredFeatures = ptUsgsFilterFeatures(allFeatures, filterState);

          ensureFilterControl();
          ptUsgsSetBlmControlsAvailable(filterControlRoot, blmFieldsAvailable, blmFieldStatus);
          var stats = renderStreamflowFeatures(filteredFeatures, 'initial');
          setOpsLayerLoading(name, false);

          if (layerGroup._map && typeof layerGroup._map.on === 'function') {
            layerGroup._map.on('moveend zoomend', updateStreamflowLegendMetric);
          }

          if (!stats.drawnCount) {
            recordStatus(name, 'USGS streamflow feed loaded, but no latest discharge/stage points were mappable.', 'pt-ops-warn');
            return;
          }

          var msg = stats.drawnCount.toLocaleString() + ' USGS latest-value streamgages displayed. ' +
            stats.dischargeCount.toLocaleString() + ' with discharge; ' +
            stats.stageOnlyCount.toLocaleString() + ' stage-only.';

          if (stats.skippedNoLatestCount) {
            msg += ' Skipped ' + stats.skippedNoLatestCount.toLocaleString() + ' static/no-current sites.';
          }

          if (summary && summary.feed_build_time_utc) {
            msg += ' Feed built ' + ptUsgsFmtPacific(summary.feed_build_time_utc) + '.';
          }

          if (summary && summary.history_mode) {
            msg += ' History mode: ' + summary.history_mode + '.';
          }

          if (!blmFieldsAvailable) {
            if (blmFieldStatus.sparse) {
              msg += ' BLM-distance fields are sparse in the hosted feed (' + Number(blmFieldStatus.distMiCount || 0).toLocaleString() + ' / ' + Number(blmFieldStatus.featureCount || 0).toLocaleString() + ' distance values); BLM filters disabled until the enriched station index is used by the feed build.';
            } else if (blmFieldStatus.keysPresent) {
              msg += ' BLM-distance fields were found in the hosted feed but had no populated values.';
            } else {
              msg += ' BLM-distance filters disabled until streamflow distance fields are refreshed in the hosted feed.';
            }
          }

          recordStatus(name, msg, 'pt-ops-ok');
        })
        .catch(function(err) {
          setOpsLayerLoading(name, false);
          recordStatus(
            name,
            'Could not fetch BRIM-hosted USGS streamflow feed: ' + (err && err.message ? err.message : err),
            'pt-ops-bad'
          );
        });
    });

    layerGroup.on('remove', function() {
      try {
        if (layerGroup._map && typeof layerGroup._map.off === 'function') {
          layerGroup._map.off('moveend zoomend', updateStreamflowLegendMetric);
        }
      } catch(e) {}
      removeFilterControl();
      try { markers.clearLayers(); } catch(e) {}
      try { layerGroup.removeLayer(markers); } catch(e) {}
      allFeatures = [];
      filteredFeatures = [];
      filterState = ptUsgsDefaultFilterState();
      lastStats = null;
      blmFieldsAvailable = false;
      delete activeLegendDefs[name];
      setOpsLayerLoading(name, false);
      redrawLegend();
      recordStatus(name, 'Layer turned off.', 'pt-ops-muted');
    });

    return layerGroup;
  }

)---"
}
