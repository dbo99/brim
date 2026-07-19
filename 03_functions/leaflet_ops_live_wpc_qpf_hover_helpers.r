# ==== leaflet_ops_live_wpc_qpf_hover_helpers.r =============================
##
## PURPOSE:
##   WPC QPF polygon identify/hover helpers.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r`.
##   It returns browser-side JavaScript as text for injection into the Ops Live
##   htmlwidgets/onRender function. Keep edits narrow and feature-specific.
## ============================================================================

pt_ops_live_wpc_qpf_hover_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // WPC QPF hover identify.
  // --------------------------------------------------------------------------
  //
  // WPC QPF layers are image/polygon overlays, so normal Leaflet hover events
  // do not expose feature attributes.  Instead, when one or more WPC QPF layers
  // are active, query the WPC QPF MapServer at the cursor location and display
  // the returned qpf/valid-time attributes in a compact hover card.
  var wpcQpfHoverLayers = {};
  var wpcQpfHoverActive = false;
  var wpcQpfHoverTimer = null;
  var wpcQpfHoverSeq = 0;
  var wpcQpfHoverTooltip = null;
  var wpcQpfLastMouseLatLng = null;

  function ptWpcQpfLayerOrder(name) {
    var order = {
      'WPC QPF Day 1': 1,
      'WPC QPF Day 2': 2,
      'WPC QPF Day 3': 3,
      'WPC QPF 3-day total': 4,
      'WPC QPF 7-day total': 5
    };

    return order[name] || 99;
  }

  function activeWpcQpfLayerDefs() {
    return Object.keys(wpcQpfHoverLayers)
      .map(function(name) {
        var def = wpcQpfHoverLayers[name] || {};
        def.name = name;
        return def;
      })
      .sort(function(a, b) {
        return ptWpcQpfLayerOrder(a.name) - ptWpcQpfLayerOrder(b.name);
      });
  }

  function parseArcgisUtcDate(value) {
    if (value === null || value === undefined || String(value).trim() === '') {
      return null;
    }

    if (typeof value === 'number' || /^[0-9]+$/.test(String(value).trim())) {
      var n = Number(value);
      if (isFinite(n)) return new Date(n);
    }

    var raw = String(value).trim();

    // ArcGIS attributes commonly arrive as "YYYY-MM-DD HH:MM:SS" without a
    // timezone suffix.  WPC QPF timestamps are UTC/Z times, so make that
    // explicit before JavaScript parses the value.
    if (/^\d{4}-\d{2}-\d{2}\s+\d{2}:\d{2}(:\d{2})?$/.test(raw)) {
      raw = raw.replace(/\s+/, 'T');
      if (/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}$/.test(raw)) raw += ':00';
      raw += 'Z';
    } else if (/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}(:\d{2})?$/.test(raw) && !/[zZ]|[+-]\d{2}:?\d{2}$/.test(raw)) {
      if (/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}$/.test(raw)) raw += ':00';
      raw += 'Z';
    }

    var d = new Date(raw);
    return isNaN(d.getTime()) ? null : d;
  }

  function formatLosAngelesCompactParts(value) {
    var d = value instanceof Date ? value : parseArcgisUtcDate(value);

    if (!d || isNaN(d.getTime())) {
      return null;
    }

    try {
      var parts = new Intl.DateTimeFormat('en-US', {
        timeZone: 'America/Los_Angeles',
        weekday: 'short',
        year: 'numeric',
        month: 'numeric',
        day: 'numeric',
        hour: 'numeric',
        minute: '2-digit',
        timeZoneName: 'short',
        hour12: true
      }).formatToParts(d);

      var out = {};
      parts.forEach(function(part) {
        if (part.type !== 'literal') out[part.type] = part.value;
      });

      if (out.weekday && out.month && out.day && out.year && out.hour && out.minute) {
        return {
          text: out.weekday + ' ' +
            out.month + '/' + out.day + '/' + out.year + ' ' +
            out.hour + ':' + out.minute + ' ' +
            (out.dayPeriod || '').trim(),
          tz: (out.timeZoneName || '').trim()
        };
      }
    } catch(e) {
      // Fall through to generic local string.
    }

    return {text: d.toLocaleString(), tz: ''};
  }

  function formatLosAngelesCompact(value) {
    var p = formatLosAngelesCompactParts(value);
    if (!p) return '';
    return p.text + (p.tz ? ' (' + p.tz + ')' : '');
  }

  function formatLosAngelesRangeCompact(startValue, endValue) {
    var start = formatLosAngelesCompactParts(startValue);
    var end = formatLosAngelesCompactParts(endValue);

    if (!start || !end) return '';

    if (start.tz && end.tz && start.tz === end.tz) {
      return start.text + ' - ' + end.text + ' (' + start.tz + ')';
    }

    return start.text + (start.tz ? ' ' + start.tz : '') +
      ' - ' + end.text + (end.tz ? ' ' + end.tz : '');
  }

  function formatWpcQpfAmount(value, units) {
    var n = Number(value);
    if (!isFinite(n)) return '';

    var digits = n < 1 ? 2 : (n < 10 ? 2 : 1);
    var txt = n.toLocaleString(undefined, {
      minimumFractionDigits: 0,
      maximumFractionDigits: digits
    });

    var u = String(units || '').trim();
    if (!u || /^inches?$/i.test(u)) u = 'in';

    return txt + ' ' + u;
  }

  function wpcQpfQueryUrl(layerId, latlng, useEnvelope) {
    var geometry;
    var geometryType = 'esriGeometryPoint';

    if (useEnvelope) {
      // The WPC cumulative-total layers can be harder to hit with a single
      // point query.  Use a tiny cursor-centered envelope as a fallback so the
      // hover is more forgiving without turning it into a broad area sample.
      var centerPt = map.latLngToContainerPoint(latlng);
      var tolPx = 12;
      var sw = map.containerPointToLatLng(L.point(centerPt.x - tolPx, centerPt.y + tolPx));
      var ne = map.containerPointToLatLng(L.point(centerPt.x + tolPx, centerPt.y - tolPx));

      geometry = JSON.stringify({
        xmin: Math.min(sw.lng, ne.lng),
        ymin: Math.min(sw.lat, ne.lat),
        xmax: Math.max(sw.lng, ne.lng),
        ymax: Math.max(sw.lat, ne.lat),
        spatialReference: {wkid: 4326}
      });
      geometryType = 'esriGeometryEnvelope';
    } else {
      geometry = JSON.stringify({
        x: latlng.lng,
        y: latlng.lat,
        spatialReference: {wkid: 4326}
      });
    }

    var fields = [
      'product',
      'valid_time',
      'qpf',
      'units',
      'issue_time',
      'start_time',
      'end_time'
    ].join(',');

    return safeUrlBase(WPC_QPF) + '/' + layerId + '/query?' +
      'f=json' +
      '&returnGeometry=false' +
      '&geometryType=' + encodeURIComponent(geometryType) +
      '&spatialRel=esriSpatialRelIntersects' +
      '&inSR=4326' +
      '&outFields=' + encodeURIComponent(fields) +
      // High-QPF bands can be nested inside lower-QPF polygons. Request enough
      // intersecting records for the browser to choose the maximum qpf value
      // under/near the cursor instead of accidentally missing >1 inch bands.
      '&resultRecordCount=100' +
      '&geometry=' + encodeURIComponent(geometry);
  }

  function wpcQpfBestFeature(features) {
    features = Array.isArray(features) ? features : [];

    if (!features.length) return null;

    // WPC QPF polygons are often nested: a 2-inch polygon can sit inside 1-inch,
    // 0.75-inch, and lower-value polygons.  ArcGIS query order is not a reliable
    // proxy for the visible band.  For a point or tiny cursor envelope, the most
    // useful hover value is the highest intersecting qpf value.
    var best = null;
    var bestQpf = -Infinity;

    for (var i = 0; i < features.length; i++) {
      var attrs = features[i] && features[i].attributes ? features[i].attributes : {};
      var qpf = Number(attrs.qpf);

      if (!isFinite(qpf)) continue;

      if (qpf > bestQpf) {
        bestQpf = qpf;
        best = features[i];
      }
    }

    return best || features[0];
  }

  function wpcQpfFeatureHtml(layerName, attrs) {
    attrs = attrs || {};

    var qpf = formatWpcQpfAmount(attrs.qpf, attrs.units);
    if (!qpf) return '';

    var valid = formatLosAngelesRangeCompact(attrs.start_time, attrs.end_time);
    var issued = formatLosAngelesCompact(attrs.issue_time);

    if (!valid && attrs.valid_time) {
      valid = String(attrs.valid_time);
    }

    var html = '<div class="pt-ops-wpc-qpf-card">' +
      '<div class="pt-ops-wpc-qpf-title">' + escapeHtml(layerName) + '</div>' +
      '<div class="pt-ops-wpc-qpf-row"><b>Forecast:</b> ' + escapeHtml(qpf) + '</div>';

    if (valid) {
      html += '<div class="pt-ops-wpc-qpf-row"><b>Valid:</b> ' + escapeHtml(valid) + '</div>';
    }

    if (issued) {
      html += '<div class="pt-ops-wpc-qpf-row"><b>Issued:</b> ' + escapeHtml(issued) + '</div>';
    }

    if (attrs.product) {
      html += '<div class="pt-ops-wpc-qpf-muted">' + escapeHtml(attrs.product) + '</div>';
    }

    html += '</div>';
    return html;
  }

  function closeWpcQpfHoverTooltip() {
    if (wpcQpfHoverTooltip) {
      try { map.removeLayer(wpcQpfHoverTooltip); } catch(e) {}
      wpcQpfHoverTooltip = null;
    }
  }

  function showWpcQpfHoverTooltip(latlng, htmlParts) {
    htmlParts = (htmlParts || []).filter(function(x) { return !!x; });

    if (!htmlParts.length) {
      closeWpcQpfHoverTooltip();
      return;
    }

    if (!wpcQpfHoverTooltip) {
      wpcQpfHoverTooltip = L.tooltip({
        permanent: true,
        direction: 'top',
        offset: [8, -10],
        opacity: 0.97,
        interactive: false,
        className: 'pt-ops-wpc-qpf-tooltip'
      });
    }

    wpcQpfHoverTooltip
      .setLatLng(latlng)
      .setContent(htmlParts.join('<div style="border-top:1px solid #ddd;margin:4px 0 3px 0;"></div>'))
      .addTo(map);
  }

  function queryWpcQpfAtLatLng(latlng) {
    if (!wpcQpfHoverActive || !latlng) return;

    var activeDefs = activeWpcQpfLayerDefs();

    if (!activeDefs.length) {
      closeWpcQpfHoverTooltip();
      return;
    }

    var seq = ++wpcQpfHoverSeq;

    Promise.all(activeDefs.map(function(def) {
      function fetchWpcQpf(useEnvelope) {
        return fetch(wpcQpfQueryUrl(def.layerId, latlng, useEnvelope), {cache: 'no-store'})
          .then(function(resp) {
            if (!resp.ok) throw new Error('HTTP ' + resp.status);
            return resp.json();
          });
      }

      return fetchWpcQpf(false)
        .then(function(json) {
          var features = Array.isArray(json.features) ? json.features : [];

          if (features.length) return json;

          // Cumulative WPC total layers sometimes miss a precise point query.
          // Retry once with a very small envelope around the cursor.
          return fetchWpcQpf(true);
        })
        .then(function(json) {
          var feature = wpcQpfBestFeature(json.features);
          var attrs = feature && feature.attributes ? feature.attributes : {};
          return wpcQpfFeatureHtml(def.name, attrs);
        })
        .catch(function() {
          return '';
        });
    })).then(function(htmlParts) {
      if (!wpcQpfHoverActive || seq !== wpcQpfHoverSeq) return;
      showWpcQpfHoverTooltip(latlng, htmlParts);
    });
  }

  function scheduleWpcQpfHoverQuery(e) {
    if (!wpcQpfHoverActive || !e || !e.latlng) return;

    wpcQpfLastMouseLatLng = e.latlng;

    if (wpcQpfHoverTimer) {
      window.clearTimeout(wpcQpfHoverTimer);
    }

    wpcQpfHoverTimer = window.setTimeout(function() {
      queryWpcQpfAtLatLng(wpcQpfLastMouseLatLng);
    }, 325);
  }

  function activateWpcQpfHover(name, layerId) {
    if (name && layerId !== null && layerId !== undefined) {
      wpcQpfHoverLayers[name] = {
        layerId: layerId
      };
    }

    if (wpcQpfHoverActive) return;

    wpcQpfHoverActive = true;
    if (map.getContainer()) {
      map.getContainer().classList.add('pt-ops-wpc-qpf-hover-on');
    }
    map.on('mousemove', scheduleWpcQpfHoverQuery);
    map.on('mouseout zoomstart movestart', closeWpcQpfHoverTooltip);
    recordStatus('WPC QPF hover', 'Hover over active WPC QPF polygons to show forecast precipitation and valid/issued times in Los Angeles time.', 'pt-ops-ok');
  }

  function deactivateWpcQpfHover(name) {
    if (name && wpcQpfHoverLayers[name]) {
      delete wpcQpfHoverLayers[name];
    }

    if (activeWpcQpfLayerDefs().length > 0) return;

    wpcQpfHoverActive = false;
    wpcQpfHoverSeq += 1;

    if (wpcQpfHoverTimer) {
      window.clearTimeout(wpcQpfHoverTimer);
      wpcQpfHoverTimer = null;
    }

    map.off('mousemove', scheduleWpcQpfHoverQuery);
    map.off('mouseout zoomstart movestart', closeWpcQpfHoverTooltip);
    if (map.getContainer()) {
      map.getContainer().classList.remove('pt-ops-wpc-qpf-hover-on');
    }
    closeWpcQpfHoverTooltip();

    recordStatus('WPC QPF hover', 'No WPC QPF hover layers active.', 'pt-ops-muted');
  }


)---"
}
