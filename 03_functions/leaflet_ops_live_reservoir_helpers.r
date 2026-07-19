# ==== leaflet_ops_live_reservoir_helpers.r ===================================
##
## PURPOSE:
##   Reservoir-specific JavaScript helpers for the BRIM Ops Live panel.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r`.
##   It intentionally returns JavaScript as text because the Ops Live panel is
##   browser-side htmlwidgets/onRender code.
##
##   The first refactor goal is boring/no behavior change:
##     - keep `pt_add_ops_live_layers()` as the public entry point
##     - keep the final map builder sourcing `leaflet_ops_live_helpers.r`
##     - move the existing Reservoir Ops implementation out of the long shared
##       Ops Live helper so future USGS/SNOTEL/SCAN modules can be added cleanly
##

# ==== 1. Reservoir Ops browser helpers ==============================
##
## Returns the JavaScript functions used by the Reservoir Ops
## GeoJSON layer. These functions depend on shared Ops Live helpers defined in
## `leaflet_ops_live_helpers.r`, such as:
##   - escapeHtml()
##   - setOpsLayerLoading()
##   - recordStatus()
##   - redrawLegend()
##   - activeLegendDefs
##
## The dependency direction is intentional: the shared Ops Live helper owns the
## panel plumbing, while this module owns the reservoir feature behavior.

pt_ops_live_cdec_reservoir_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Reservoir Ops GeoJSON layer.
  // --------------------------------------------------------------------------

  function ptResNumber(value) {
    // JavaScript's Number(null), Number(''), and Number(false) all return 0.
    // That is dangerous for reservoir popups because GeoJSON null/blank values
    // mean "not reported", not "zero acre-feet". Treat explicit missing values
    // as missing before numeric conversion so CNRFC-only points such as
    // Twitchell/Lexington do not display phantom 0 AF / 0% values.
    if (value === null || value === undefined) return null;
    if (typeof value === 'boolean') return null;

    if (typeof value === 'string') {
      value = value.trim();
      if (!value || ['NA', 'NULL', 'NAN'].indexOf(value.toUpperCase()) >= 0) {
        return null;
      }
    }

    var n = Number(value);
    return isFinite(n) ? n : null;
  }

  function ptResFmtNumber(value, digits) {
    var n = ptResNumber(value);
    if (n === null) return 'Not available';
    return n.toLocaleString(undefined, {
      minimumFractionDigits: digits || 0,
      maximumFractionDigits: digits || 0
    });
  }

  function ptResFmtStorageAf(value) {
    var n = ptResNumber(value);
    if (n === null) return 'Not available';
    if (Math.abs(n) >= 1000000) {
      return ptResFmtNumber(n / 1000000, 2) + ' MAF';
    }
    return ptResFmtNumber(n, 0) + ' AF';
  }

  function ptResFmtStorageLong(value) {
    var n = ptResNumber(value);
    if (n === null) return 'Not available';
    if (Math.abs(n) >= 1000000) {
      return ptResFmtNumber(n / 1000000, 2) + ' MAF (' + ptResFmtNumber(n, 0) + ' AF)';
    }
    return ptResFmtNumber(n, 0) + ' AF';
  }

  function ptResFmtCfs(value) {
    var n = ptResNumber(value);
    if (n === null) return 'Not available';
    return ptResFmtNumber(n, 0) + ' cfs';
  }

  function ptResFmtSignedAf(value) {
    var n = ptResNumber(value);
    if (n === null) return 'Not available';
    var sign = n > 0 ? '+' : '';
    return sign + ptResFmtNumber(n, 0) + ' AF';
  }

  function ptResBestStorageAf(props) {
    props = props || {};
    var n = ptResNumber(props.display_storage_af);
    if (n !== null) return n;
    n = ptResNumber(props.storage_af);
    if (n !== null) return n;
    n = ptResNumber(props.midnight_storage_af);
    if (n !== null) return n;
    return ptResNumber(props.cnrfc_fallback_storage_af);
  }

  function ptResHasStorageValue(props) {
    props = props || {};
    return (
      ptResNumber(props.display_storage_af) !== null ||
      ptResNumber(props.storage_af) !== null ||
      ptResNumber(props.midnight_storage_af) !== null ||
      ptResNumber(props.cnrfc_fallback_storage_af) !== null
    );
  }

  function ptResFmtPct(value) {
    var n = ptResNumber(value);
    if (n === null) return 'Not available';
    return n.toLocaleString(undefined, {
      minimumFractionDigits: 1,
      maximumFractionDigits: 1
    }) + '%';
  }

  function ptResStorageRadius(storageAf) {
    var n = ptResNumber(storageAf);
    if (n === null || n <= 0) return 4.5;

    // Semi-proportional scaling: radius increases approximately with the
    // square root of storage.  This preserves small reservoirs while making
    // very large reservoirs such as Shasta/Oroville/Folsom visibly larger
    // than mid-size reservoirs.  The cap keeps symbols from overwhelming
    // the Ops pane.
    return Math.max(4.5, Math.min(24, 4 + 8 * Math.sqrt(n / 1000000)));
  }

  function ptResColorInterpolate(hexA, hexB, t) {
    t = Math.max(0, Math.min(1, Number(t) || 0));

    function h2rgb(hex) {
      hex = String(hex || '').replace('#', '');
      return {
        r: parseInt(hex.slice(0, 2), 16),
        g: parseInt(hex.slice(2, 4), 16),
        b: parseInt(hex.slice(4, 6), 16)
      };
    }

    function c2hex(c) {
      c = Math.max(0, Math.min(255, Math.round(c)));
      var h = c.toString(16);
      return h.length === 1 ? '0' + h : h;
    }

    var a = h2rgb(hexA);
    var b = h2rgb(hexB);

    return '#' +
      c2hex(a.r + (b.r - a.r) * t) +
      c2hex(a.g + (b.g - a.g) * t) +
      c2hex(a.b + (b.b - a.b) * t);
  }

  function ptResPctCapacityFillColor(pct) {
    var p = ptResNumber(pct);
    if (p === null) return '#2C7FB8';
    p = Math.max(0, Math.min(100, p));

    // Continuous version of the former 0/25/50/75/100 percent-capacity ramp.
    // This preserves the familiar reservoir blue palette but avoids abrupt
    // class breaks for a naturally continuous value.
    var stops = [
      {p: 0,   c: '#C6DBEF'},
      {p: 25,  c: '#9ECAE1'},
      {p: 50,  c: '#6BAED6'},
      {p: 75,  c: '#2171B5'},
      {p: 100, c: '#08306B'}
    ];

    for (var i = 1; i < stops.length; i++) {
      if (p <= stops[i].p) {
        var prev = stops[i - 1];
        var next = stops[i];
        return ptResColorInterpolate(prev.c, next.c, (p - prev.p) / (next.p - prev.p));
      }
    }

    return stops[stops.length - 1].c;
  }

  function ptResFillColor(props) {
    props = props || {};

    // CNRFC-only / no-storage points should not look like low-percent-capacity
    // reservoirs.  Use a pale neutral fill so they read as "location with links"
    // rather than a reported storage value.
    if (!ptResHasStorageValue(props)) return '#F7F7F7';

    var pct = ptResNumber(props.pct_capacity);
    if (pct === null) pct = ptResNumber(props.midnight_pct_capacity);

    if (pct !== null) return ptResPctCapacityFillColor(pct);
    return '#2C7FB8';
  }

  function ptResStrokeColor(props) {
    props = props || {};

    if (!ptResHasStorageValue(props)) return '#777777';

    var age = ptResAgeForStatus(ptResDisplayAgeHours(props));

    if (age !== null && age > 24) return '#99000D';
    if (age !== null && age > 12) return '#B35806';

    return '#08306B';
  }

  function ptResFmtAge(hours) {
    var h = ptResNumber(hours);

    if (h === null) return 'age unknown';

    // A tiny negative age can occur from clock skew or source time rounding.
    // The feed builder should normally prevent this by parsing CDEC times as
    // Pacific local time, but the map should still avoid showing awkward
    // values such as -0.2 hr old.
    if (h < 0) {
      if (h > -0.25) return 'near real-time';
      return 'time check needed';
    }

    var minutes = Math.round(h * 60);

    if (minutes < 1) return '<1 min old';
    if (minutes < 60) return minutes.toLocaleString() + ' min old';

    var wholeHours = Math.floor(minutes / 60);
    var remMinutes = minutes % 60;

    if (wholeHours < 48) {
      if (remMinutes === 0) return wholeHours.toLocaleString() + ' hr old';
      return wholeHours.toLocaleString() + ' hr ' + remMinutes + ' min old';
    }

    return (minutes / 1440).toLocaleString(undefined, {
      maximumFractionDigits: 1
    }) + ' days old';
  }

  function ptResAgeForStatus(hours) {
    var h = ptResNumber(hours);
    if (h === null) return null;
    return Math.max(0, h);
  }

  function ptResDisplayAgeHours(props) {
    props = props || {};

    var h = ptResNumber(props.display_obs_age_hours);
    if (h !== null) return h;

    h = ptResNumber(props.obs_age_hours);
    if (h !== null) return h;

    return ptResNumber(props.cnrfc_fallback_obs_age_hours);
  }

  function ptResDataStatus(props) {
    props = props || {};

    if (ptResNumber(props.storage_af) === null &&
        ptResNumber(props.midnight_storage_af) === null &&
        ptResNumber(props.cnrfc_fallback_storage_af) !== null) {
      return 'CNRFC fallback';
    }

    if (ptResNumber(props.storage_af) === null &&
        ptResNumber(props.midnight_storage_af) === null) {
      return 'No CDEC/CNRFC storage value';
    }

    if (ptResNumber(props.storage_af) === null && ptResNumber(props.midnight_storage_af) !== null) {
      return 'Daily midnight only';
    }

    var age = ptResAgeForStatus(ptResDisplayAgeHours(props));

    if (age === null) return 'Age unknown';
    if (age > 24) return 'Stale >24 hr';
    if (age > 12) return 'Stale >12 hr';
    return 'Current-ish';
  }

  function ptResLink(label, url) {
    url = String(url || '').trim();
    if (!url) return '';
    return '<br><a href="' + escapeHtml(url) + '" target="_blank">' + escapeHtml(label) + '</a>';
  }

  function ptResLinkItem(label, url) {
    url = String(url || '').trim();
    if (!url) return '';
    return '<div style="margin:1px 0;"><a href="' + escapeHtml(url) + '" target="_blank">' + escapeHtml(label) + '</a></div>';
  }

  function ptResBool(value) {
    if (value === true) return true;
    if (value === false || value === null || value === undefined) return false;

    var txt = String(value).trim().toLowerCase();
    return txt === 'true' || txt === 't' || txt === '1' || txt === 'yes' || txt === 'y';
  }

  function ptResCurrentWaterYearLosAngeles() {
    var now = new Date();

    try {
      var parts = new Intl.DateTimeFormat('en-US', {
        timeZone: 'America/Los_Angeles',
        year: 'numeric',
        month: 'numeric'
      }).formatToParts(now);

      var year = null;
      var month = null;

      parts.forEach(function(part) {
        if (part.type === 'year') year = Number(part.value);
        if (part.type === 'month') month = Number(part.value);
      });

      if (isFinite(year) && isFinite(month)) {
        return month >= 10 ? year + 1 : year;
      }
    } catch(e) {
      // Fall through to browser-local time below.
    }

    var localYear = now.getFullYear();
    var localMonth = now.getMonth() + 1;
    return localMonth >= 10 ? localYear + 1 : localYear;
  }

  function ptResUsacePlotUrl(props, interval) {
    props = props || {};
    interval = String(interval || '').toLowerCase();

    var isDaily = interval === 'd';
    var isHourly = interval === 'h';

    if (!isDaily && !isHourly) return '';
    if (isDaily && !ptResBool(props.usace_has_daily_plot)) return '';
    if (isHourly && !ptResBool(props.usace_has_hourly_plot)) return '';

    var plotId = String(props.usace_plot_id || props.cdec_id || '').trim();
    if (!plotId) return '';

    var base = String(props.usace_california_plots_url || 'https://www.spk-wc.usace.army.mil/plots/california.html').trim();
    if (!base) return '';

    // Use the clean page URL as a base even if a querystring somehow arrives.
    base = base.split('?')[0];

    var wy = ptResCurrentWaterYearLosAngeles();
    var url = base +
      '?name=' + encodeURIComponent(plotId.toLowerCase()) +
      '&year=' + encodeURIComponent(String(wy)) +
      '&interval=' + encodeURIComponent(interval) +
      '&tab=plot&window=wy';

    // The daily plot examples from USACE include gl=true; keep it only on the
    // daily one-click link so the URL mirrors the observed daily page pattern.
    if (isDaily) {
      url += '&gl=true';
    }

    return url;
  }


  function ptResCnrfcIdFromUrl(url) {
    url = String(url || '').trim();
    if (!url) return '';

    var match = url.match(/[?&]id=([A-Za-z0-9_-]+)/);
    return match && match[1] ? String(match[1]).toUpperCase() : '';
  }

  function ptResAddRow(label, value) {
    value = String(value === null || value === undefined ? '' : value).trim();
    if (!value || value === 'Not available') return '';
    return '<br><b>' + escapeHtml(label) + ':</b> ' + escapeHtml(value);
  }

  function ptResPopup(props) {
    props = props || {};

    var cdecId = props.cdec_id || props.station_id || '';
    var name = props.reservoir_name || props.cdec_station_name || props.cdec_station_name_latest || props.midnight_station_name || 'CDEC reservoir';
    var obsTime = props.obs_datetime_cdec_display || props.obs_datetime || props.obs_datetime_pst || '';
    var fallbackObsTime = props.cnrfc_fallback_obs_datetime_display || '';
    var ageText = ptResFmtAge(ptResDisplayAgeHours(props));
    var status = ptResDataStatus(props);
    var latestAf = ptResNumber(props.storage_af);
    var midnightAf = ptResNumber(props.midnight_storage_af);
    var fallbackAf = ptResNumber(props.cnrfc_fallback_storage_af);
    var hasDailyContext = (
      midnightAf !== null ||
      ptResNumber(props.midnight_storage_change_af) !== null ||
      ptResNumber(props.midnight_pct_capacity) !== null ||
      ptResNumber(props.midnight_pct_average) !== null ||
      ptResNumber(props.midnight_inflow_cfs) !== null ||
      ptResNumber(props.midnight_outflow_cfs) !== null
    );

    var html = '<div class="pt-ops-res-popup" style="min-width:250px;max-width:340px;">' +
      '<div style="font-weight:700;font-size:13px;margin-bottom:2px;">' +
      escapeHtml(cdecId || 'CDEC') + ' – ' + escapeHtml(name) +
      '</div>';

    html += '<div style="color:#555;font-size:11px;margin-bottom:5px;">';
    html += 'CDEC ID: ' + escapeHtml(cdecId || 'not available');

    // The CNRFC/NWS crosswalk ID is useful QA/context, but some reservoirs
    // use different role-specific CNRFC product IDs for observed/current,
    // inflow, ensemble, or release products. Avoid showing the match
    // confidence here because it can be misleading when role overrides drive
    // the actual CNRFC links used below.
    if (props.cnrfc_nws_id) {
      html += ' | CNRFC/NWS crosswalk: ' + escapeHtml(props.cnrfc_nws_id);
    }

    var roleIds = [];
    var obsProductId = props.cnrfc_obs_id_effective || ptResCnrfcIdFromUrl(props.cnrfc_obs_url);
    var inflowProductId = props.cnrfc_inflow_id_effective || ptResCnrfcIdFromUrl(props.cnrfc_inflow_url);
    var ensembleProductId = props.cnrfc_ensemble_id_effective || ptResCnrfcIdFromUrl(props.cnrfc_ensemble_url);
    var releaseProductId = props.cnrfc_release_id_effective || ptResCnrfcIdFromUrl(props.cnrfc_release_url);

    if (obsProductId && obsProductId !== props.cnrfc_nws_id) {
      roleIds.push('observed/current ' + obsProductId);
    }
    if (inflowProductId && inflowProductId !== props.cnrfc_nws_id) {
      roleIds.push('inflow ' + inflowProductId);
    }
    if (ensembleProductId && ensembleProductId !== props.cnrfc_nws_id) {
      roleIds.push('ensemble ' + ensembleProductId);
    }
    if (releaseProductId && releaseProductId !== props.cnrfc_nws_id) {
      roleIds.push('release ' + releaseProductId);
    }
    if (roleIds.length > 0) {
      html += '<br>CNRFC product IDs: ' + escapeHtml(roleIds.join(', '));
    }

    html += '</div>';

    html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:4px;">' +
      '<b>Storage</b>';

    if (latestAf !== null) {
      html += '<br><b>Latest:</b> ' + escapeHtml(ptResFmtStorageLong(latestAf));
      if (obsTime) {
        html += '<br><b>Observed:</b> ' + escapeHtml(obsTime);
      }
      html += '<br><b>Age:</b> ' + escapeHtml(ageText);
      if (ptResNumber(props.pct_capacity) !== null && ptResNumber(props.capacity_af) !== null && ptResNumber(props.capacity_af) > 0) {
        html += '<br><b>Latest % capacity:</b> ' + escapeHtml(ptResFmtPct(props.pct_capacity));
      }
    } else if (fallbackAf !== null) {
      html += '<br><b>Latest:</b> ' + escapeHtml(ptResFmtStorageLong(fallbackAf));
      html += '<br><span style="color:#555;">Source: CNRFC observed/current fallback</span>';
      if (fallbackObsTime) {
        html += '<br><b>Observed:</b> ' + escapeHtml(fallbackObsTime);
      }
      html += '<br><b>Age:</b> ' + escapeHtml(ageText);
      if (ptResNumber(props.cnrfc_fallback_elevation_ft) !== null) {
        html += '<br><b>Elevation:</b> ' + escapeHtml(ptResFmtNumber(props.cnrfc_fallback_elevation_ft, 2)) + ' ft';
      }
      if (ptResNumber(props.pct_capacity) !== null) {
        html += '<br><b>Latest % capacity:</b> ' + escapeHtml(ptResFmtPct(props.pct_capacity));
      }
    } else {
      html += '<br><b>Latest:</b> Not reported in current CDEC latest table';
    }

    if (midnightAf !== null) {
      html += '<br><b>Midnight:</b> ' + escapeHtml(ptResFmtStorageLong(midnightAf));
    }

    if (ptResNumber(props.midnight_storage_change_af) !== null) {
      html += '<br><b>Daily change:</b> ' + escapeHtml(ptResFmtSignedAf(props.midnight_storage_change_af));
    }

    if (ptResNumber(props.midnight_pct_capacity) !== null) {
      html += '<br><b>Midnight % capacity:</b> ' + escapeHtml(ptResFmtPct(props.midnight_pct_capacity));
    }

    if (ptResNumber(props.midnight_pct_average) !== null) {
      html += '<br><b>Midnight % average:</b> ' + escapeHtml(ptResFmtPct(props.midnight_pct_average));
    }

    if (ptResNumber(props.capacity_af) !== null && ptResNumber(props.capacity_af) > 0) {
      html += '<br><b>Capacity:</b> ' + escapeHtml(ptResFmtNumber(props.capacity_af, 0)) + ' AF';
      if (props.capacity_source_display) {
        html += '<br><span style="color:#555;">Capacity source: ' + escapeHtml(props.capacity_source_display) + '</span>';
      }
    }

    html += '</div>';

    if (hasDailyContext && (ptResNumber(props.midnight_inflow_cfs) !== null || ptResNumber(props.midnight_outflow_cfs) !== null || ptResNumber(props.midnight_avg_storage_af) !== null || ptResNumber(props.midnight_storage_year_ago_af) !== null)) {
      html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
        '<b>Daily RES context</b>';

      if (ptResNumber(props.midnight_avg_storage_af) !== null) {
        html += '<br><b>Average storage:</b> ' + escapeHtml(ptResFmtStorageLong(props.midnight_avg_storage_af));
      }
      if (ptResNumber(props.midnight_storage_year_ago_af) !== null) {
        html += '<br><b>Storage 1 yr ago:</b> ' + escapeHtml(ptResFmtStorageLong(props.midnight_storage_year_ago_af));
      }
      if (ptResNumber(props.midnight_inflow_cfs) !== null) {
        html += '<br><b>Daily inflow:</b> ' + escapeHtml(ptResFmtCfs(props.midnight_inflow_cfs));
      }
      if (ptResNumber(props.midnight_outflow_cfs) !== null) {
        html += '<br><b>Daily outflow:</b> ' + escapeHtml(ptResFmtCfs(props.midnight_outflow_cfs));
      }
      html += '</div>';
    }

    html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
      '<b>Status</b>' +
      '<br><b>Feed status:</b> ' + escapeHtml(status);

    if (props.data_quality_note_live) {
      html += '<br><span style="color:#555;">' + escapeHtml(props.data_quality_note_live) + '</span>';
    }
    if (props.cnrfc_fallback_status && fallbackAf !== null) {
      html += '<br><span style="color:#555;">' + escapeHtml(props.cnrfc_fallback_status) + '</span>';
    }
    if (props.cnrfc_fallback_note && fallbackAf !== null) {
      html += '<br><span style="color:#555;">' + escapeHtml(props.cnrfc_fallback_note) + '</span>';
    }
    if (props.midnight_data_status && midnightAf !== null) {
      html += '<br><span style="color:#555;">' + escapeHtml(props.midnight_data_status) + '</span>';
    }
    if (props.cnrfc_role_override_note) {
      html += '<br><span style="color:#555;">CNRFC note: ' + escapeHtml(props.cnrfc_role_override_note) + '</span>';
    }
    if (props.cnrfc_obs_availability_note) {
      html += '<br><span style="color:#555;">CNRFC observed/current note: ' + escapeHtml(props.cnrfc_obs_availability_note) + '</span>';
    }
    if (props.county) {
      html += '<br><b>County:</b> ' + escapeHtml(props.county);
    }
    if (props.operator_agency) {
      html += '<br><b>Operator:</b> ' + escapeHtml(props.operator_agency);
    }
    if (props.usace_plot_availability_note) {
      html += '<br><span style="color:#555;">USACE note: ' + escapeHtml(props.usace_plot_availability_note) + '</span>';
    }
    html += '</div>';

    var linkLines =
      ptResLinkItem('CDEC station metadata', props.cdec_station_url) +
      ptResLinkItem('CDEC latest storage table', props.cdec_latest_storage_table_url) +
      ptResLinkItem('CDEC daily midnight table', props.midnight_report_source_url) +
      ptResLinkItem('CDEC sensor 15 hourly CSV', props.cdec_sensor15_hourly_url) +
      ptResLinkItem('CNRFC observed/current reservoir page', props.cnrfc_obs_url) +
      ptResLinkItem('CNRFC reservoir inflow forecast', props.cnrfc_inflow_url) +
      ptResLinkItem('CNRFC reservoir ensemble forecast', props.cnrfc_ensemble_url) +
      ptResLinkItem('CNRFC reservoir release schedule', props.cnrfc_release_url) +
      ptResLinkItem('USACE daily plot (current WY)', ptResUsacePlotUrl(props, 'd')) +
      ptResLinkItem('USACE hourly plot (current WY)', ptResUsacePlotUrl(props, 'h')) +
      ptResLinkItem('USACE California plots (main page)', props.usace_california_plots_url) +
      ptResLinkItem('Capacity source', props.capacity_source_url);

    if (linkLines) {
      html += '<div style="border-top:1px solid #ddd;padding-top:4px;margin-top:5px;">' +
        '<b>Links</b>' +
        linkLines +
        '</div>';
    }

    html += '</div>';
    return html;
  }

  function ptResTooltip(props) {
    props = props || {};

    var cdecId = props.cdec_id || '';
    var name = props.reservoir_name || props.cdec_station_name || props.cdec_station_name_latest || props.midnight_station_name || 'CDEC reservoir';
    var latestAf = ptResNumber(props.storage_af);
    var midnightAf = ptResNumber(props.midnight_storage_af);
    var fallbackAf = ptResNumber(props.cnrfc_fallback_storage_af);
    var storageAf = ptResBestStorageAf(props);

    var ageText = '';
    var label = 'Storage';

    if (latestAf !== null) {
      label = 'Latest CDEC storage';
      ageText = ptResFmtAge(ptResDisplayAgeHours(props));
    } else if (midnightAf !== null) {
      label = 'CDEC midnight storage';
      ageText = 'daily midnight only';
    } else if (fallbackAf !== null) {
      label = 'CNRFC observed storage';
      ageText = ptResFmtAge(ptResDisplayAgeHours(props));
    } else {
      ageText = 'No current CDEC/CNRFC storage value';
    }

    return '<b>' + escapeHtml(cdecId || 'CDEC') + '</b> – ' + escapeHtml(name) +
      '<br>' + escapeHtml(label) + ': ' + escapeHtml(ptResFmtStorageAf(storageAf)) +
      '<br>' + escapeHtml(ageText);
  }

  function makeCdecReservoirStorageLayer(opts) {
    opts = opts || {};

    var layerGroup = L.layerGroup();
    var geojsonLayer = null;

    layerGroup.on('add', function() {
      var name = opts.name || 'Reservoir Ops: CDEC / CNRFC / USACE';

      if (!opts.url) {
        setOpsLayerLoading(name, false);
        recordStatus(name, 'No reservoir-ops GeoJSON URL configured.', 'pt-ops-bad');
        return;
      }

      activeLegendDefs[name] = {
        note: opts.note || '',
        legendType: 'reservoir_capacity',
        sourceUrl: opts.sourceUrl || opts.url || '',
        legendUrl: '',
        infoUrl: opts.summaryUrl || '',
        infoLabel: opts.summaryUrl ? 'summary' : '',
        legendNote: 'Circle size scales semi-proportionally with displayed storage: CDEC latest first, then CDEC daily midnight, then selective CNRFC observed/current fallback. Darker blue means a reservoir is more full by percent capacity where capacity is available. Neutral blue means storage is available but percent capacity is unavailable. Hollow gray/dashed points have no current CDEC/CNRFC storage value and are shown for operational links only. Orange/red outlines indicate stale latest observations.'
      };

      redrawLegend();
      setOpsLayerLoading(name, true);
      recordStatus(name, 'Fetching latest reservoir-ops GeoJSON…', 'pt-ops-warn');

      fetch(opts.url + (opts.url.indexOf('?') >= 0 ? '&' : '?') + '_=' + Date.now(), { cache: 'no-store' })
        .then(function(resp) {
          if (!resp.ok) throw new Error('HTTP ' + resp.status + ' ' + resp.statusText);
          return resp.json();
        })
        .then(function(geojson) {
          var features = geojson && Array.isArray(geojson.features) ? geojson.features : [];

          geojsonLayer = L.geoJSON(geojson, {
            pane: 'pane_ops',
            pointToLayer: function(feature, latlng) {
              var p = feature && feature.properties ? feature.properties : {};
              var hasStorageValue = ptResHasStorageValue(p);

              return L.circleMarker(latlng, {
                radius: ptResStorageRadius(ptResBestStorageAf(p)),
                color: ptResStrokeColor(p),
                weight: hasStorageValue ? 1.8 : 1.6,
                opacity: hasStorageValue ? 0.95 : 0.9,
                fillColor: ptResFillColor(p),
                fillOpacity: hasStorageValue ? 0.78 : 0.18,
                dashArray: hasStorageValue ? null : '3,3',
                pane: 'pane_ops'
              });
            },
            onEachFeature: function(feature, layer) {
              var p = feature && feature.properties ? feature.properties : {};
              layer.bindTooltip(ptResTooltip(p), {
                sticky: true,
                className: 'pt-ops-res-tooltip'
              });
              layer.bindPopup(ptResPopup(p));
            }
          });

          geojsonLayer.addTo(layerGroup);

          var maxAge = null;
          var stale12 = 0;
          var stale24 = 0;

          features.forEach(function(f) {
            var p = f && f.properties ? f.properties : {};
            var age = ptResAgeForStatus(ptResDisplayAgeHours(p));

            if (age !== null) {
              if (maxAge === null || age > maxAge) maxAge = age;
              if (age > 12) stale12 += 1;
              if (age > 24) stale24 += 1;
            }
          });

          var latestCount = 0;
          var midnightOnly = 0;
          var cnrfcFallbackCount = 0;

          features.forEach(function(f) {
            var p = f && f.properties ? f.properties : {};
            if (ptResNumber(p.storage_af) !== null) latestCount += 1;
            if (ptResNumber(p.storage_af) === null &&
                ptResNumber(p.midnight_storage_af) !== null &&
                ptResNumber(p.cnrfc_fallback_storage_af) === null) midnightOnly += 1;
            if (ptResNumber(p.storage_af) === null &&
                ptResNumber(p.midnight_storage_af) === null &&
                ptResNumber(p.cnrfc_fallback_storage_af) !== null) cnrfcFallbackCount += 1;
          });

          var coverageStatus = geojson && geojson.coverage_status ? String(geojson.coverage_status) : '';
          var coverageNote = geojson && geojson.coverage_note ? String(geojson.coverage_note) : '';

          var msg = 'Loaded ' + features.length + ' reservoir-ops features';
          msg += ' (' + latestCount + ' latest';
          if (midnightOnly > 0) msg += ', ' + midnightOnly + ' midnight-only';
          if (cnrfcFallbackCount > 0) msg += ', ' + cnrfcFallbackCount + ' CNRFC fallback';
          msg += ').';

          if (maxAge !== null) {
            msg += ' Max latest age: ' + ptResFmtAge(maxAge) + '.';
          }
          if (coverageStatus && coverageStatus !== 'normal') {
            msg += ' Source coverage: ' + coverageStatus + '.';
          }
          if (stale24 > 0) {
            msg += ' Stale >24 hr: ' + stale24 + '.';
          } else if (stale12 > 0) {
            msg += ' Stale >12 hr: ' + stale12 + '.';
          }
          if (coverageNote) {
            msg += ' ' + coverageNote;
          }

          var statusClass = stale24 > 0 ? 'pt-ops-bad' :
            ((stale12 > 0 || (coverageStatus && coverageStatus !== 'normal')) ? 'pt-ops-warn' : 'pt-ops-ok');

          setOpsLayerLoading(name, false);
          recordStatus(name, msg, statusClass);
          redrawLegend();
        })
        .catch(function(err) {
          setOpsLayerLoading(name, false);
          recordStatus(name, 'Reservoir Ops GeoJSON request failed: ' + (err && err.message ? err.message : err), 'pt-ops-bad');
        });
    });

    layerGroup.on('remove', function() {
      var name = opts.name || 'Reservoir Ops: CDEC / CNRFC / USACE';

      if (geojsonLayer) {
        try { layerGroup.removeLayer(geojsonLayer); } catch(e) {}
        geojsonLayer = null;
      }

      delete activeLegendDefs[name];
      setOpsLayerLoading(name, false);
      redrawLegend();
      recordStatus(name, 'Layer turned off.', 'pt-ops-muted');
    });

    return layerGroup;
  }

)---"
}
