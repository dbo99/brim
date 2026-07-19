# ==== leaflet_ops_live_weather_hazards_helpers.r ===========================
##
## PURPOSE:
##   NWS watches/warnings/advisories hover/query helpers.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r`.
##   It returns browser-side JavaScript as text for injection into the Ops Live
##   htmlwidgets/onRender function. Keep edits narrow and feature-specific.
## ============================================================================

pt_ops_live_weather_hazards_js <- function() {

  r"---(
  function formatWwaTime(value) {
    if (value === null || value === undefined || String(value).trim() === '') {
      return '';
    }

    var raw = String(value).trim();
    var d = new Date(raw);

    if (!isNaN(d.getTime())) {
      try {
        return d.toLocaleString([], {
          weekday: 'short',
          month: 'short',
          day: 'numeric',
          hour: 'numeric',
          minute: '2-digit',
          timeZoneName: 'short'
        });
      } catch(e) {
        return d.toLocaleString();
      }
    }

    return raw.replace('T', ' ').replace(/:00(\s|$)/, '$1');
  }

  function wwaSeverityClass(attrs) {
    attrs = attrs || {};
    var prod = String(attrs.prod_type || attrs.event || '').toLowerCase();
    var sig = String(attrs.sig || '').toUpperCase();

    if (sig === 'W' || prod.indexOf('warning') >= 0) return 'pt-ops-wwa-warning';
    if (sig === 'A' || prod.indexOf('watch') >= 0) return 'pt-ops-wwa-watch';
    if (sig === 'Y' || prod.indexOf('advisory') >= 0) return 'pt-ops-wwa-advisory';

    return '';
  }

  function wwaPriority(attrs) {
    attrs = attrs || {};
    var cls = wwaSeverityClass(attrs);
    if (cls.indexOf('warning') >= 0) return 1;
    if (cls.indexOf('watch') >= 0) return 2;
    if (cls.indexOf('advisory') >= 0) return 3;
    return 4;
  }

  function wwaAttr(feature) {
    return feature && feature.attributes ? feature.attributes : {};
  }

  function wwaDedupeAndSort(features) {
    var seen = {};
    var out = [];

    (features || []).forEach(function(feature) {
      var a = wwaAttr(feature);
      var key = String(a.cap_id || '') + '|' + String(a.prod_type || '') + '|' +
        String(a.expiration || '') + '|' + String(a.wfo || '');

      if (seen[key]) return;
      seen[key] = true;
      out.push(a);
    });

    out.sort(function(a, b) {
      var pa = wwaPriority(a);
      var pb = wwaPriority(b);
      if (pa !== pb) return pa - pb;
      return String(a.prod_type || '').localeCompare(String(b.prod_type || ''));
    });

    return out.slice(0, 6);
  }

  function normalizeWwaWfo(value) {
    var code = String(value || '').trim().toUpperCase();

    // NWS offices commonly appear as KSTO/KREV in service attributes.
    // For user-facing display and weather.gov office pages, drop the leading K.
    if (/^K[A-Z]{3}$/.test(code)) {
      code = code.substring(1);
    }

    return code;
  }

  function wwaForecastOfficeHtml(value) {
    var code = normalizeWwaWfo(value);

    if (!code) {
      return 'NWS hazard product';
    }

    if (/^[A-Z]{3}$/.test(code)) {
      var url = 'https://www.weather.gov/' + code.toLowerCase() + '/';
      return 'Forecast office: <a href="' + escapeHtml(url) +
        '" target="_blank" rel="noopener">' + escapeHtml(code) + '</a>';
    }

    return 'Forecast office: ' + escapeHtml(code);
  }

  function wwaHoverHtml(items) {
    var html = '<div class="pt-ops-wwa-card">';

    items.forEach(function(attrs) {
      var title = attrs.prod_type || attrs.event || 'NWS hazard';
      var expiration = formatWwaTime(attrs.expiration || attrs.ends || '');
      var wfo = attrs.wfo ? String(attrs.wfo) : '';
      var cls = wwaSeverityClass(attrs);

      html += '<div class="pt-ops-wwa-item ' + escapeHtml(cls) + '">' +
        '<div class="pt-ops-wwa-title">' + escapeHtml(title) + '</div>';

      if (expiration) {
        html += '<div class="pt-ops-wwa-expires">Expires: ' + escapeHtml(expiration) + '</div>';
      }

      html += '<div class="pt-ops-wwa-meta">' +
        wwaForecastOfficeHtml(wfo) +
        '</div></div>';
    });

    html += '</div>';
    return html;
  }

  function ptWwaSetHoverCursor(isHot) {
    var container = map && map.getContainer ? map.getContainer() : null;
    if (!container) return;

    if (isHot) {
      container.classList.add('pt-ops-wwa-hot');
      container.style.cursor = 'pointer';
    } else {
      container.classList.remove('pt-ops-wwa-hot');
      if (container.style.cursor === 'pointer') {
        container.style.cursor = '';
      }
    }
  }

  function closeWwaHoverTooltip() {
    if (wwaHoverTooltip) {
      try { map.removeLayer(wwaHoverTooltip); } catch(e) {}
      wwaHoverTooltip = null;
    }
    ptWwaSetHoverCursor(false);
  }

  function showWwaHoverTooltip(latlng, items) {
    if (!items || !items.length) {
      closeWwaHoverTooltip();
      return;
    }

    ptWwaSetHoverCursor(true);

    if (!wwaHoverTooltip) {
      wwaHoverTooltip = L.tooltip({
        permanent: true,
        direction: 'top',
        offset: [8, -10],
        opacity: 0.97,
        interactive: true,
        className: 'pt-ops-wwa-tooltip'
      });
    }

    wwaHoverTooltip
      .setLatLng(latlng)
      .setContent(wwaHoverHtml(items))
      .addTo(map);
  }

  function wwaQueryUrl(layerId, latlng) {
    var geometry = JSON.stringify({
      x: latlng.lng,
      y: latlng.lat,
      spatialReference: {wkid: 4326}
    });

    var fields = [
      'prod_type',
      'expiration',
      'ends',
      'issuance',
      'wfo',
      'phenom',
      'sig',
      'event',
      'cap_id'
    ].join(',');

    return safeUrlBase(NWS_WWA) + '/' + layerId + '/query?' +
      'f=json' +
      '&returnGeometry=false' +
      '&geometryType=esriGeometryPoint' +
      '&spatialRel=esriSpatialRelIntersects' +
      '&inSR=4326' +
      '&outFields=' + encodeURIComponent(fields) +
      '&resultRecordCount=8' +
      '&geometry=' + encodeURIComponent(geometry);
  }

  function queryWwaAtLatLng(latlng) {
    if (!wwaHoverActive || !latlng) return;

    var seq = ++wwaHoverSeq;
    var urls = [wwaQueryUrl(0, latlng), wwaQueryUrl(1, latlng)];

    Promise.all(urls.map(function(url) {
      return fetch(url, {cache: 'no-store'})
        .then(function(resp) {
          if (!resp.ok) throw new Error('HTTP ' + resp.status);
          return resp.json();
        })
        .then(function(json) {
          return Array.isArray(json.features) ? json.features : [];
        })
        .catch(function() {
          return [];
        });
    })).then(function(results) {
      if (!wwaHoverActive || seq !== wwaHoverSeq) return;

      var features = [];
      results.forEach(function(arr) {
        features = features.concat(arr || []);
      });

      var items = wwaDedupeAndSort(features);
      showWwaHoverTooltip(latlng, items);
    });
  }

  function scheduleWwaHoverQuery(e) {
    if (!wwaHoverActive || !e || !e.latlng) return;

    wwaLastMouseLatLng = e.latlng;

    if (wwaHoverTimer) {
      window.clearTimeout(wwaHoverTimer);
    }

    wwaHoverTimer = window.setTimeout(function() {
      queryWwaAtLatLng(wwaLastMouseLatLng);
    }, 325);
  }

  function activateWwaHover() {
    if (wwaHoverActive) return;

    wwaHoverActive = true;
    map.on('mousemove', scheduleWwaHoverQuery);
    map.on('mouseout zoomstart movestart', closeWwaHoverTooltip);
    recordStatus('NWS WWA hover', 'Hover over colored hazard polygons for event name, expiration, and linked forecast office.', 'pt-ops-ok');
  }

  function deactivateWwaHover() {
    if (!wwaHoverActive) return;

    wwaHoverActive = false;
    wwaHoverSeq += 1;

    if (wwaHoverTimer) {
      window.clearTimeout(wwaHoverTimer);
      wwaHoverTimer = null;
    }

    map.off('mousemove', scheduleWwaHoverQuery);
    map.off('mouseout zoomstart movestart', closeWwaHoverTooltip);
    closeWwaHoverTooltip();
  }



)---"
}
