# ==== leaflet_ops_live_panel_helpers.r =====================================
##
## PURPOSE:
##   Fixed Ops Live panel rendering, checkbox wiring, and clear-ops behavior.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r`.
##   It returns browser-side JavaScript as text for injection into the Ops Live
##   htmlwidgets/onRender function. Keep edits narrow and feature-specific.
## ============================================================================

pt_ops_live_panel_helpers_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Fixed Ops panel, status, legend, and source links
  // --------------------------------------------------------------------------
  var statusDiv;
  var legendDiv;
  var opsDiv;

  function ptOpsOrderIndex(order, value) {
    var key = String(value || '');
    var idx = order.indexOf(key);
    return idx >= 0 ? idx : 999;
  }

  function ptOpsSubgroupOrder(category, subgroup) {
    var subgroupOrder = {
      'Hydro Observations': [
        'Radar',
        'Precip / WRU stations',
        'Precip / QPE',
        'Flows / levels / snow / moisture / etc'
      ],
      'Forecasts / Outlooks': [
        'River / Reservoir Forecasts',
        'Weather Forecasts / Outlooks'
      ],
      'Fire / Smoke / Air Quality': [
        'Current / recent fire',
        'Smoke / air quality'
      ],
      'Atmosphere / Wind': [
        'Wind'
      ]
    };

    return ptOpsOrderIndex(subgroupOrder[category] || [], subgroup || '');
  }

  function ptOpsPanelEntries() {
    var categoryOrder = [
      'Hydro Observations',
      'Forecasts / Outlooks',
      'Hazards',
      'Fire / Smoke / Air Quality',
      'Drought',
      'Weather Offices / Boundaries',
      'Satellite / Imagery',
      'Cameras',
      'Atmosphere / Wind'
    ];

    var entries = opsLayers.map(function(def, idx) {
      return {def: def, idx: idx};
    });

    entries.sort(function(a, b) {
      var ac = ptOpsOrderIndex(categoryOrder, a.def.category);
      var bc = ptOpsOrderIndex(categoryOrder, b.def.category);
      if (ac !== bc) return ac - bc;

      var asg = ptOpsSubgroupOrder(a.def.category, a.def.subgroup || '');
      var bsg = ptOpsSubgroupOrder(b.def.category, b.def.subgroup || '');
      if (asg !== bsg) return asg - bsg;

      return a.idx - b.idx;
    });

    return entries;
  }

  function buildOpsPanelHtml() {
    var html = '<div class="pt-ops-small">Live weather, precipitation, satellite, observation, and hazard overlays are fetched from public services when checked. Turn on only one or two image-heavy layers at a time when possible. Use Clear ops in the ribbon to remove active Ops layers.</div>';

    var entries = ptOpsPanelEntries();
    var lastCat = null;
    var lastSubgroup = null;

    entries.forEach(function(entry, panelPos) {
      var def = entry.def;
      var idx = entry.idx;
      var subgroup = def.subgroup || '';

      if (def.category !== lastCat) {
        if (lastCat !== null) {
          html += opsExternalLinksForCategory(lastCat, '');
        }

        html += '<div class="pt-ops-section"><b>' + escapeHtml(def.category) + '</b></div>';
        lastCat = def.category;
        lastSubgroup = null;
      }

      if (subgroup && subgroup !== lastSubgroup) {
        html += '<div class="pt-ops-subgroup">' + escapeHtml(subgroup) + '</div>';
        lastSubgroup = subgroup;
      }

      html += '<div class="pt-ops-layer-row" data-pt-ops-row-index="' + idx + '">' +
        '<label class="pt-ops-layer-label">' +
        '<input type="checkbox" data-pt-ops-index="' + idx + '"> ' +
        '<span class="pt-ops-layer-name">' + escapeHtml(def.name) + '</span>' +
        '<span class="pt-ops-layer-spinner" aria-hidden="true">loading…</span>' +
        '</label>' +
        layerRowLinksHtml(def) +
        (def.extraRowHtml ? String(def.extraRowHtml) : '') +
        '</div>';

      if (def.helperText) {
        html += '<div class="pt-ops-layer-help">' + escapeHtml(def.helperText) + '</div>';
      }

      var nextEntry = entries[panelPos + 1];
      var nextDef = nextEntry ? nextEntry.def : null;
      var nextSubgroup = nextDef ? (nextDef.subgroup || '') : '';

      if (subgroup && (!nextDef || nextDef.category !== def.category || nextSubgroup !== subgroup)) {
        html += opsExternalLinksForCategory(def.category, subgroup);
      }
    });

    if (lastCat !== null) {
      html += opsExternalLinksForCategory(lastCat, '');
    }

    html += '<div id="pt-ops-status-block" class="pt-ops-section">' +
      '<h4>Ops status / freshness</h4>' +
      '<div class="pt-ops-muted">No Ops overlays loaded yet.</div>' +
      '</div>';

    html += '<div id="pt-ops-legend-block" class="pt-ops-section">' +
      '<h4>Active overlay notes</h4>' +
      '<div class="pt-ops-muted">Turn on an Ops overlay to show notes here.</div>' +
      '</div>';

    html += '<div class="pt-ops-section pt-ops-links">' +
      '<h4>Ops source links</h4>' +
      '<a href="https://gispub.epa.gov/airnow/" target="_blank">AirNow interactive map</a>' +
      '<a href="https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/Air%20Now%20Current%20Monitor%20Data%20Public/FeatureServer/0" target="_blank">AirNow current monitor FeatureServer</a>' +
      '<a href="https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/AirNowLatestContoursPM25/FeatureServer/0" target="_blank">AirNow PM2.5 contour FeatureServer</a>' +
      '<a href="https://fire.airnow.gov/" target="_blank">AirNow Fire &amp; Smoke Map</a>' +
      '<a href="https://portal.airfire.org/" target="_blank">AirFire portal</a>' +
      '<a href="https://map.purpleair.com/air-quality-standards-us-epa-aqi?opt=%2F1%2Flp%2Fa10%2Fp604800%2FcC0#5.38/36.993/-117.891" target="_blank">PurpleAir map</a>' +
      '<a href="https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi" target="_blank">IEM NEXRAD Radar WMS</a>' +
      '<a href="https://mapservices.weather.noaa.gov/eventdriven/rest/services/radar/radar_base_reflectivity/MapServer" target="_blank">NOAA MRMS Radar Reflectivity MapServer</a>' +
      '<a href="https://satellitemaps.nesdis.noaa.gov/arcgis/rest/services/MERGEDGC_current/ImageServer" target="_blank">NOAA GOES GeoColor ImageServer</a>' +
      '<a href="https://satellitemaps.nesdis.noaa.gov/arcgis/rest/services/ABI13_current/ImageServer" target="_blank">NOAA GOES Infrared ImageServer</a>' +
      '<a href="https://satellitemaps.nesdis.noaa.gov/arcgis/rest/services/ABI10_current/ImageServer" target="_blank">NOAA GOES Water Vapor ImageServer</a>' +
      '<a href="https://gibs.earthdata.nasa.gov/wmts/epsg3857/best/MODIS_Terra_CorrectedReflectance_TrueColor/" target="_blank">NASA GIBS MODIS Terra WMTS</a>' +
      '<a href="https://mapservices.weather.noaa.gov/vector/rest/services/obs/surface_obs/MapServer" target="_blank">NWS Surface Observations MapServer</a>' +
      '<a href="https://mapservices.weather.noaa.gov/raster/rest/services/obs/mrms_qpe/ImageServer" target="_blank">MRMS QPE ImageServer</a>' +
      '<a href="https://mapservices.weather.noaa.gov/raster/rest/services/obs/rfc_qpe/MapServer" target="_blank">NWS QPE Mosaic MapServer</a>' +
      '<a href="https://www.cocorahs.org/" target="_blank">CoCoRaHS home</a>' +
      '<a href="https://api2.cocorahs.org/Help" target="_blank">CoCoRaHS Web API</a>' +
      '<a href="https://mapservices.weather.noaa.gov/vector/rest/services/precip/wpc_qpf/MapServer" target="_blank">WPC QPF MapServer</a>' +
      '<a href="https://mapservices.weather.noaa.gov/eventdriven/rest/services/WWA/watch_warn_adv/MapServer" target="_blank">NWS Watches / Warnings / Advisories MapServer</a>' +
      '<a href="https://mapservices.weather.noaa.gov/vector/rest/services/hazards/wpc_precip_hazards/MapServer" target="_blank">WPC ERO MapServer</a>' +
      '</div>';

    return html;
  }

  function ptClearOpsPaneDomArtifacts() {
    // Clear Ops should leave the Ops pane visually empty. Dynamic image layers
    // can occasionally leave a stale <img> behind if the remote image finishes
    // loading during the same moment the layer is being removed. This final DOM
    // sweep runs only during Clear Ops, when no Ops overlay should remain.
    var pane = map.getPane && map.getPane('pane_ops');
    if (!pane) return;

    Array.prototype.slice.call(
      pane.querySelectorAll('.leaflet-image-layer, .leaflet-tile-container, .leaflet-tile')
    ).forEach(function(el) {
      try {
        if (el && el.parentNode) el.parentNode.removeChild(el);
      } catch(e) {}
    });
  }


  function ptOpsActivateLayerByName(layerName) {
    layerName = String(layerName || '');
    if (!layerName) return {ok: false, message: 'Missing Ops layer name.'};

    var chk = checkboxByName[layerName];
    var def = opsDefByName[layerName];

    if (!chk || !def) {
      return {ok: false, message: 'Ops layer not found.'};
    }

    if (chk.checked && activeLayers[layerName]) {
      return {ok: true, alreadyActive: true, message: 'Layer already active.'};
    }

    chk.checked = true;
    try {
      chk.dispatchEvent(new Event('change', {bubbles: true}));
    } catch(e) {
      var evt = document.createEvent('HTMLEvents');
      evt.initEvent('change', true, false);
      chk.dispatchEvent(evt);
    }

    return {ok: true, alreadyActive: false, message: 'Layer activated.'};
  }

  function ptOpsIsLayerActiveByName(layerName) {
    layerName = String(layerName || '');
    if (!layerName) return false;

    var chk = checkboxByName[layerName];
    return !!(chk && chk.checked && activeLayers[layerName]);
  }

  function ptOpsDeactivateLayerByName(layerName) {
    layerName = String(layerName || '');
    if (!layerName) return {ok: false, active: false, message: 'Missing Ops layer name.'};

    var chk = checkboxByName[layerName];
    var def = opsDefByName[layerName];

    if (!chk || !def) {
      return {ok: false, active: false, message: 'Ops layer not found.'};
    }

    if (!chk.checked && !activeLayers[layerName]) {
      return {ok: true, alreadyInactive: true, active: false, message: 'Layer already inactive.'};
    }

    chk.checked = false;
    try {
      chk.dispatchEvent(new Event('change', {bubbles: true}));
    } catch(e) {
      var evt = document.createEvent('HTMLEvents');
      evt.initEvent('change', true, false);
      chk.dispatchEvent(evt);
    }

    return {ok: true, alreadyInactive: false, active: false, message: 'Layer deactivated.'};
  }

  function ptOpsToggleLayerByName(layerName) {
    layerName = String(layerName || '');
    if (!layerName) return {ok: false, active: false, message: 'Missing Ops layer name.'};

    if (ptOpsIsLayerActiveByName(layerName)) {
      return ptOpsDeactivateLayerByName(layerName);
    }

    var result = ptOpsActivateLayerByName(layerName);
    if (result && result.ok) result.active = true;
    return result;
  }

  window.ptOpsActivateLayerByName = ptOpsActivateLayerByName;
  window.ptOpsDeactivateLayerByName = ptOpsDeactivateLayerByName;
  window.ptOpsToggleLayerByName = ptOpsToggleLayerByName;
  window.ptOpsIsLayerActiveByName = ptOpsIsLayerActiveByName;

  function ptClearOpsLayers() {
    Object.keys(activeLayers).forEach(function(name) {
      var def = opsDefByName[name];
      if (def && typeof def.onDeactivate === 'function') {
        try { def.onDeactivate(); } catch(e) {}
      }

      var layer = activeLayers[name];
      if (layer && typeof layer.forceRemove === 'function') {
        try { layer.forceRemove(map); } catch(e) {}
      }
      try { map.removeLayer(layer); } catch(e) {}
      if (layer && typeof layer.forceRemove === 'function') {
        try { layer.forceRemove(map); } catch(e) {}
      }

      delete activeLayers[name];
    });

    ptClearOpsPaneDomArtifacts();

    Object.keys(checkboxByName).forEach(function(name) {
      checkboxByName[name].checked = false;
    });

    activeLegendDefs = {};
    statusRows = {};
    clearAllOpsLayerLoading();
    redrawLegend();
    redrawStatus();
    updateOpsHeaderCount();
  }

  function addFixedOpsPanel() {
    var old = document.getElementById('pt-ops-live-wrap');
    if (old) old.remove();

    var wrap = document.createElement('div');
    wrap.id = 'pt-ops-live-wrap';
    
    // Start collapsed so the initial map view is less crowded.
    // Users can expand the Ops panel from the fixed bottom-right header.
    wrap.className = 'pt-ops-live-wrap pt-ops-collapsed';

    var header = document.createElement('div');
    header.className = 'pt-ops-live-header';
    header.innerHTML =
      '<span class="pt-ops-live-title">Ops Live Layers ' +
      '<span id="pt-ops-active-count" class="pt-ops-live-active-count">(none active)</span> ' +
      '<span class="pt-ops-live-caret">▸</span></span>' +
      '<span class="pt-ops-ribbon-clear-shell">' +
      '<button type="button" id="pt-ops-clear-ribbon-btn" class="pt-ops-ribbon-clear" title="Clear active Ops live layers">Clear ops</button>' +
      '</span>';

    var body = document.createElement('div');
    body.className = 'pt-ops-live-body pt-ops-panel';
    body.innerHTML = buildOpsPanelHtml();

    wrap.appendChild(header);
    wrap.appendChild(body);
    document.body.appendChild(wrap);

    L.DomEvent.disableClickPropagation(wrap);
    L.DomEvent.disableScrollPropagation(wrap);

    header.addEventListener('click', function(e) {
      e.preventDefault();
      e.stopPropagation();
      wrap.classList.toggle('pt-ops-collapsed');
      var caret = header.querySelector('.pt-ops-live-caret');
      if (caret) {
        caret.textContent = wrap.classList.contains('pt-ops-collapsed') ? '▸' : '▾';
      }
    });

    var clearRibbonBtn = document.getElementById('pt-ops-clear-ribbon-btn');
    if (clearRibbonBtn) {
      // Give the Clear ops button explicit interactive states.  In some
      // browsers / Leaflet-control contexts the CSS :hover feedback can be
      // visually inconsistent, so mirror hover/press states with classes and
      // direct inline styles.  This makes the button feel like the other map
      // controls even though it lives inside the custom Ops header.
      function ptClearBtnApplyState(state) {
        clearRibbonBtn.classList.toggle('pt-ops-ribbon-clear-hover', state === 'hover' || state === 'pressed');
        clearRibbonBtn.classList.toggle('pt-ops-ribbon-clear-pressed', state === 'pressed');

        if (state === 'pressed') {
          clearRibbonBtn.style.setProperty('background', '#c8d9b8', 'important');
          clearRibbonBtn.style.setProperty('border-color', '#43572d', 'important');
          clearRibbonBtn.style.setProperty('box-shadow', 'inset 0 1px 5px rgba(0,0,0,0.45)', 'important');
          clearRibbonBtn.style.setProperty('transform', 'translateY(1px)', 'important');
          return;
        }

        if (state === 'hover') {
          clearRibbonBtn.style.setProperty('background', '#dfead1', 'important');
          clearRibbonBtn.style.setProperty('border-color', '#546b38', 'important');
          clearRibbonBtn.style.setProperty('box-shadow', '0 1px 7px rgba(0,0,0,0.38)', 'important');
          clearRibbonBtn.style.removeProperty('transform');
          return;
        }

        clearRibbonBtn.style.setProperty('background', '#f7f7f7', 'important');
        clearRibbonBtn.style.setProperty('border-color', '#777', 'important');
        clearRibbonBtn.style.setProperty('box-shadow', '0 1px 2px rgba(0,0,0,0.25)', 'important');
        clearRibbonBtn.style.removeProperty('transform');
      }

      function ptClearBtnHoverOn() { ptClearBtnApplyState('hover'); }
      function ptClearBtnHoverOff() { ptClearBtnApplyState('base'); }
      function ptClearBtnPressOn() { ptClearBtnApplyState('pressed'); }
      function ptClearBtnPressOff() { ptClearBtnApplyState('hover'); }

      ptClearBtnApplyState('base');

      clearRibbonBtn.addEventListener('mouseover', ptClearBtnHoverOn);
      clearRibbonBtn.addEventListener('mouseout', ptClearBtnHoverOff);
      clearRibbonBtn.addEventListener('mouseenter', ptClearBtnHoverOn);
      clearRibbonBtn.addEventListener('mouseleave', ptClearBtnHoverOff);
      clearRibbonBtn.addEventListener('focus', ptClearBtnHoverOn);
      clearRibbonBtn.addEventListener('blur', ptClearBtnHoverOff);
      clearRibbonBtn.addEventListener('mousedown', ptClearBtnPressOn);
      clearRibbonBtn.addEventListener('mouseup', ptClearBtnPressOff);
      clearRibbonBtn.addEventListener('touchstart', ptClearBtnPressOn, {passive: true});
      clearRibbonBtn.addEventListener('touchend', ptClearBtnHoverOff);
      clearRibbonBtn.addEventListener('touchcancel', ptClearBtnHoverOff);

      clearRibbonBtn.addEventListener('click', function(e) {
        e.preventDefault();
        e.stopPropagation();
        ptClearBtnPressOff();
        ptClearOpsLayers();
      });
    }

    opsDiv = body;
    statusDiv = body.querySelector('#pt-ops-status-block');
    legendDiv = body.querySelector('#pt-ops-legend-block');


    body.addEventListener('click', function(e) {
      var deltaZoomLink = e.target && e.target.closest ? e.target.closest('[data-pt-ops-action="delta-ops-zoom"]') : null;
      if (deltaZoomLink && body.contains(deltaZoomLink)) {
        e.preventDefault();
        e.stopPropagation();
        if (window.ptDeltaOpsZoomToDefault && typeof window.ptDeltaOpsZoomToDefault === 'function') {
          window.ptDeltaOpsZoomToDefault();
        }
        return;
      }

      var refreshLink = e.target && e.target.closest ? e.target.closest('[data-pt-ops-action="refresh"]') : null;
      if (!refreshLink || !body.contains(refreshLink)) return;

      e.preventDefault();
      e.stopPropagation();

      var name = refreshLink.getAttribute('data-pt-ops-name') || '';
      var def = opsDefByName[name];
      var layer = activeLayers[name];

      if (!def) {
        recordStatus('Ops refresh', 'Could not find that Ops row to refresh.', 'pt-ops-warn');
        return;
      }

      if (!layer) {
        recordStatus(name, 'Turn this Ops layer on before using rfrsh.', 'pt-ops-warn');
        return;
      }

      setOpsLayerLoading(name, true);

      if (layer && typeof layer.refreshCurrentView === 'function') {
        try {
          layer.refreshCurrentView();
        } catch(err) {
          console.error(err);
          setOpsLayerLoading(name, false);
          recordStatus(name, 'Could not refresh this Ops layer.', 'pt-ops-warn');
        }
        return;
      }

      var opts = layer && layer.options ? layer.options : {};
      var opsKey = opts.opsKey || '';
      var bridge = window.ptOpsExternalCatalogBridge;

      if (opsKey && bridge && typeof bridge.refreshLayer === 'function') {
        try {
          var result = bridge.refreshLayer({ opsKey: opsKey, opsDisplayName: name });
          if (!result || !result.ok) {
            setOpsLayerLoading(name, false);
            recordStatus(name, (result && result.message) ? result.message : 'Could not refresh this promoted catalog layer.', 'pt-ops-warn');
          } else {
            recordStatus(name, result.message || 'Refreshing current-view snapshot…', 'pt-ops-warn');
          }
        } catch(err2) {
          console.error(err2);
          setOpsLayerLoading(name, false);
          recordStatus(name, 'Could not refresh this promoted catalog layer.', 'pt-ops-warn');
        }
        return;
      }

      setOpsLayerLoading(name, false);
      recordStatus(name, 'This Ops layer does not expose a current-view refresh method.', 'pt-ops-muted');
    });

    body.addEventListener('change', function(e) {
      var deltaLabelToggle = e.target && e.target.closest ? e.target.closest('[data-pt-ops-action="delta-labels"]') : null;
      if (!deltaLabelToggle || !body.contains(deltaLabelToggle)) return;
      e.preventDefault();
      e.stopPropagation();
      var show = !!deltaLabelToggle.checked;
      window.ptDeltaOpsLabelsVisible = show;
      if (window.ptDeltaOpsSetLabelsVisible && typeof window.ptDeltaOpsSetLabelsVisible === 'function') {
        window.ptDeltaOpsSetLabelsVisible(show);
      }
    });

    var checks = body.querySelectorAll('input[data-pt-ops-index]');
    Array.prototype.forEach.call(checks, function(chk) {
      var idx = parseInt(chk.getAttribute('data-pt-ops-index'), 10);
      var def = opsLayers[idx];
      checkboxByName[def.name] = chk;
      rowByName[def.name] = chk.closest ? chk.closest('.pt-ops-layer-row') : chk.parentNode;
      opsDefByName[def.name] = def;
      chk.addEventListener('change', function() {
        if (chk.checked) {
          setOpsLayerLoading(def.name, true);
          activeLayers[def.name] = def.layer;
          updateOpsHeaderCount();
          def.layer.addTo(map);
          if (typeof def.onActivate === 'function') {
            try { def.onActivate(); } catch(e) {}
          }
        } else {
          if (activeLayers[def.name]) {
            if (typeof def.onDeactivate === 'function') {
              try { def.onDeactivate(); } catch(e) {}
            }
            var layer = activeLayers[def.name];
            if (layer && typeof layer.forceRemove === 'function') {
              try { layer.forceRemove(map); } catch(e) {}
            }
            try { map.removeLayer(layer); } catch(e) {}
            if (layer && typeof layer.forceRemove === 'function') {
              try { layer.forceRemove(map); } catch(e) {}
            }
            delete activeLayers[def.name];
          }
          setOpsLayerLoading(def.name, false);
          updateOpsHeaderCount();
        }
      });
    });

    updateOpsHeaderCount();

    var clearBtn = document.getElementById('pt-ops-clear-all');
    if (clearBtn) {
      clearBtn.addEventListener('click', function(e) {
        e.preventDefault();
        e.stopPropagation();
        ptClearOpsLayers();
      });
    }
  }

  addFixedOpsPanel();
)---"
}
