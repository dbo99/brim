function(el, x, hucThemeData) {

  if (
    window.BRIM_HUC_LOCAL &&
    typeof window.BRIM_HUC_LOCAL.destroy === 'function'
  ) {
    try {
      window.BRIM_HUC_LOCAL.destroy();
    } catch (oldControllerError) {
      console.warn(
        'BRIM HUC controller: prior controller cleanup failed.',
        oldControllerError
      );
    }
  }

  var map = this;
  var hucTheme = 'none';
  var hucLegendUserHidden = false;
  var visibleHucLayers = {};
  var styleGeneration = 0;
  var scheduledStyleFrame = null;
  var activeStyleJob = null;
  var listenerRecords = [];
  var destroyed = false;
  var currentHucTooltip = null;
  var currentHucTooltipLayer = null;
  var documentClearClickHandler = null;
  var documentPopupResetHandler = null;
  var hucPopupSuppressed = false;
  var hucPopupSuppressionGeneration = 0;
  var hucPopupReleaseTimer = null;

  hucThemeData = hucThemeData || {};

  /*
   * R sends JSON-safe record arrays. Rebuild browser lookups once, then use
   * Leaflet's group index directly for every activation and theme operation.
   * No map-wide layer scan is needed.
   */
  var hucThemeLookup = {};
  var hucThemeLookupRows = hucThemeData.lookup || [];

  if (Array.isArray(hucThemeLookupRows)) {
    hucThemeLookupRows.forEach(function(row) {
      if (row && row.layer_id !== undefined && row.layer_id !== null) {
        hucThemeLookup[String(row.layer_id)] = row;
      }
    });
  } else {
    hucThemeLookup = hucThemeLookupRows || {};
  }

  var hucThemeLegends = {};
  var hucThemeLegendRows = hucThemeData.legends || [];

  if (Array.isArray(hucThemeLegendRows)) {
    hucThemeLegendRows.forEach(function(row) {
      if (!row || !row.huc_layer || !row.theme) return;
      if (!hucThemeLegends[row.huc_layer]) {
        hucThemeLegends[row.huc_layer] = {};
      }
      hucThemeLegends[row.huc_layer][row.theme] = row.legend || null;
    });
  } else {
    hucThemeLegends = hucThemeLegendRows || {};
  }

  var hucLevelRows = hucThemeData.levels || [];
  var hucLevels = {};
  var hucGroupToLevel = {};
  var sharedHucRenderer = (
    typeof L !== 'undefined' &&
    typeof L.canvas === 'function'
  ) ? L.canvas({
    pane: 'pane_huc'
  }) : null;
  if (sharedHucRenderer) {
    sharedHucRenderer._brimHucLevel = 'huc-family';
  }

  var diagnostics = {
    installedAt: Date.now(),
    registryBuildMs: 0,
    mapScanCount: 0,
    styleOperationCount: 0,
    optionPrimeCount: 0,
    styleJobCount: 0,
    canceledStyleJobs: 0,
    staleCallbacksPrevented: 0,
    activationCount: 0,
    deactivationCount: 0,
    ignoredOverlayEvents: 0,
    sameThemeNoops: 0,
    tooltipOpenCount: 0,
    tooltipCloseCount: 0,
    tooltipReplacementCount: 0,
    lastTooltipCloseReason: null,
    popupSuppressionStartCount: 0,
    popupSuppressionReleaseCount: 0,
    popupSuppressedOpenCount: 0,
    lastPopupSuppressionReason: null,
    lastApplyMs: null,
    lastApplyReason: null,
    lastApplyOperations: 0,
    longTasks: []
  };

  function nowMs() {
    return (
      window.performance &&
      typeof window.performance.now === 'function'
    ) ? window.performance.now() : Date.now();
  }

  function listen(target, eventNames, handler) {
    if (!target || typeof target.on !== 'function') return;
    target.on(eventNames, handler);
    listenerRecords.push({
      target: target,
      eventNames: eventNames,
      handler: handler
    });
  }

  function getLayerId(layer) {
    if (!layer || !layer.options) return null;

    if (layer.options.layerId !== undefined && layer.options.layerId !== null) {
      return String(layer.options.layerId);
    }

    if (layer.options.layer_id !== undefined && layer.options.layer_id !== null) {
      return String(layer.options.layer_id);
    }

    return null;
  }

  function rendererName(renderer) {
    if (!renderer) return 'unassigned';
    if (typeof L !== 'undefined' && L.Canvas && renderer instanceof L.Canvas) {
      return 'Canvas';
    }
    if (typeof L !== 'undefined' && L.SVG && renderer instanceof L.SVG) {
      return 'SVG';
    }
    if (renderer._brimHucLevel) return 'Canvas-compatible';
    return renderer.constructor && renderer.constructor.name ?
      renderer.constructor.name :
      'unknown';
  }

  function buildHucRegistry() {
    var started = nowMs();
    var layerManager = map.layerManager || {};
    var byGroup = layerManager._byGroup || {};

    if (!Array.isArray(hucLevelRows) || hucLevelRows.length === 0) {
      /*
       * Compatibility with an older theme payload. Group names are not safely
       * inferable here, so retain an explicit warning instead of scanning the
       * entire map and restoring the old hot path.
       */
      console.warn('BRIM HUC controller: no HUC level records were supplied.');
      return;
    }

    /*
     * A controller rebuild can occur while a HUC root is already mounted.
     * Adopt its existing Canvas so the rebuilt controller and every still-
     * hidden level continue to share exactly one renderer.
     */
    hucLevelRows.some(function(row) {
      if (!row || !row.group_name) return false;
      var groupName = String(row.group_name);
      var root = layerManager._groupContainers ?
        layerManager._groupContainers[groupName] :
        null;
      if (!root || !map.hasLayer || !map.hasLayer(root)) return false;

      var table = byGroup[groupName] || {};
      var stamps = Object.keys(table);
      for (var i = 0; i < stamps.length; i++) {
        var layer = table[stamps[i]];
        var renderer = layer && layer.options ?
          (layer.options.renderer || layer._renderer) :
          null;
        if (renderer) {
          sharedHucRenderer = renderer;
          sharedHucRenderer._brimHucLevel = 'huc-family';
          return true;
        }
      }
      return false;
    });

    hucLevelRows.forEach(function(row) {
      if (!row || !row.huc_layer || !row.group_name) return;

      var levelName = String(row.huc_layer);
      var groupName = String(row.group_name);
      var table = byGroup[groupName] || {};
      var layers = [];
      var renderer = null;
      var root = layerManager._groupContainers ?
        layerManager._groupContainers[groupName] :
        null;
      var rootIsActive = !!(root && map.hasLayer && map.hasLayer(root));

      Object.keys(table).forEach(function(stamp) {
        var layer = table[stamp];
        var layerId = getLayerId(layer);
        var record = layerId === null ? null : hucThemeLookup[layerId];

        if (
          !layer ||
          typeof layer.setStyle !== 'function' ||
          !record ||
          String(record.huc_layer) !== levelName
        ) {
          return;
        }

        layers.push(layer);
        if (layer.options && sharedHucRenderer && !rootIsActive) {
          /*
           * Hidden-before-add guarantees the current BRIM HUC roots have never
           * mounted, so their bootstrap R renderers can be safely consolidated
           * onto one map-local Canvas. A single canvas preserves pointer
           * interaction when multiple HUC levels are active.
           */
          layer.options.renderer = sharedHucRenderer;
        }
        if (!renderer && layer.options) {
          renderer = layer.options.renderer || layer._renderer || null;
        }
      });

      hucLevels[levelName] = {
        name: levelName,
        label: row.huc_label ? String(row.huc_label) : levelName.toUpperCase(),
        groupName: groupName,
        expectedCount: Number(row.expected_count) || 0,
        layers: layers,
        root: root || null,
        renderer: renderer,
        active: rootIsActive,
        appliedTheme: 'none',
        dirty: false
      };
      hucGroupToLevel[groupName] = levelName;
    });

    diagnostics.registryBuildMs = nowMs() - started;
  }

  buildHucRegistry();

  function isHucFeatureLayer(layer) {
    var layerId = getLayerId(layer);
    return layerId !== null && !!hucThemeLookup[layerId];
  }

  function tooltipSource(tooltip) {
    return tooltip && tooltip._source ? tooltip._source : null;
  }

  /*
   * Leaflet tooltips are per-feature overlays, not a map-wide singleton.
   * Keep ownership explicit so a missed Canvas mouseout cannot leave an old
   * HUC summary behind when the next polygon opens its own tooltip.
   */
  function closeCurrentHucTooltip(reason) {
    var tooltip = currentHucTooltip;
    var source = currentHucTooltipLayer || tooltipSource(tooltip);
    if (!tooltip && !source) return false;

    currentHucTooltip = null;
    currentHucTooltipLayer = null;
    diagnostics.tooltipCloseCount += 1;
    diagnostics.lastTooltipCloseReason = reason || 'unspecified';

    try {
      if (source && typeof source.closeTooltip === 'function') {
        source.closeTooltip();
      }
    } catch (sourceCloseError) {}

    try {
      if (
        tooltip &&
        tooltip._map &&
        map &&
        typeof map.closeTooltip === 'function'
      ) {
        map.closeTooltip(tooltip);
      }
    } catch (mapCloseError) {}

    return true;
  }

  function handleTooltipOpen(event) {
    var tooltip = event && event.tooltip;
    var source = tooltipSource(tooltip);
    if (!tooltip || !isHucFeatureLayer(source)) return;

    if (currentHucTooltip && currentHucTooltip !== tooltip) {
      diagnostics.tooltipReplacementCount += 1;
      closeCurrentHucTooltip('HUC hover replacement');
    }

    currentHucTooltip = tooltip;
    currentHucTooltipLayer = source;
    diagnostics.tooltipOpenCount += 1;
  }

  function handleTooltipClose(event) {
    if (event && event.tooltip === currentHucTooltip) {
      currentHucTooltip = null;
      currentHucTooltipLayer = null;
      diagnostics.tooltipCloseCount += 1;
      diagnostics.lastTooltipCloseReason = 'Leaflet tooltipclose';
    }
  }

  function handleHucMouseout(event) {
    var source = event && (
      event.layer ||
      event.propagatedFrom ||
      event.sourceTarget
    );
    if (
      currentHucTooltipLayer &&
      source === currentHucTooltipLayer
    ) {
      closeCurrentHucTooltip('HUC polygon mouseout');
    }
  }

  /*
   * BRIM's custom marquee fires its start event during pointerup, immediately
   * before fitBounds. A browser click synthesized from that same release runs
   * before the marquee's zero-task end event. Native Leaflet BoxZoom fires its
   * end event inside mouseup, before any following click. Hold suppression
   * through one zero-delay task so both event orders cross the same safe
   * boundary without imposing a user-visible cooldown.
   */
  function beginHucPopupSuppression(reason) {
    hucPopupSuppressionGeneration += 1;
    if (hucPopupReleaseTimer !== null) {
      window.clearTimeout(hucPopupReleaseTimer);
      hucPopupReleaseTimer = null;
    }
    hucPopupSuppressed = true;
    diagnostics.popupSuppressionStartCount += 1;
    diagnostics.lastPopupSuppressionReason = reason || 'zoom start';
    closeCurrentHucTooltip('marquee or box zoom start');
  }

  function releaseHucPopupSuppression(reason) {
    hucPopupSuppressionGeneration += 1;
    if (hucPopupReleaseTimer !== null) {
      window.clearTimeout(hucPopupReleaseTimer);
      hucPopupReleaseTimer = null;
    }
    if (hucPopupSuppressed) {
      diagnostics.popupSuppressionReleaseCount += 1;
    }
    hucPopupSuppressed = false;
    diagnostics.lastPopupSuppressionReason = reason || 'released';
  }

  function scheduleHucPopupRelease(reason) {
    var generation = hucPopupSuppressionGeneration;
    if (hucPopupReleaseTimer !== null) {
      window.clearTimeout(hucPopupReleaseTimer);
    }
    hucPopupReleaseTimer = window.setTimeout(function() {
      hucPopupReleaseTimer = null;
      if (destroyed || generation !== hucPopupSuppressionGeneration) return;
      releaseHucPopupSuppression(reason || 'zoom end');
    }, 0);
  }

  function handlePopupOpen(event) {
    var popup = event && event.popup;
    if (popup && isHucFeatureLayer(popup._source)) {
      closeCurrentHucTooltip('HUC popup open');
      if (hucPopupSuppressed) {
        diagnostics.popupSuppressedOpenCount += 1;
        try {
          if (map && typeof map.closePopup === 'function') {
            map.closePopup(popup);
          }
        } catch (popupCloseError) {}
      }
    }
  }

  listen(map, 'tooltipopen', handleTooltipOpen);
  listen(map, 'tooltipclose', handleTooltipClose);
  listen(map, 'movestart zoomstart', function() {
    closeCurrentHucTooltip('map movement');
  });
  listen(map, 'popupopen', handlePopupOpen);
  listen(map, 'boxzoomstart pt:marqueezoomstart', function(event) {
    beginHucPopupSuppression(event ? event.type : 'zoom start');
  });
  listen(map, 'boxzoomend pt:marqueezoomend', function(event) {
    scheduleHucPopupRelease(event ? event.type : 'zoom end');
  });
  listen(map, 'pt:measureinteractionchange', function(event) {
    if (event && event.active) {
      closeCurrentHucTooltip('measure mode activation');
    }
  });

  Object.keys(hucLevels).forEach(function(levelName) {
    listen(hucLevels[levelName].root, 'mouseout', handleHucMouseout);
  });

  if (
    typeof document !== 'undefined' &&
    typeof document.addEventListener === 'function'
  ) {
    documentClearClickHandler = function(event) {
      var clearButton = (
        event &&
        event.target &&
        typeof event.target.closest === 'function'
      ) ? event.target.closest(
        '.pt-main-layer-clear-btn,#pt-clear-all-btn'
      ) : null;
      if (clearButton) closeCurrentHucTooltip('clear action');
    };
    document.addEventListener('click', documentClearClickHandler, true);

    documentPopupResetHandler = function(event) {
      if (!hucPopupSuppressed || !event) return;
      if (event.type === 'keydown' && event.key !== 'Escape') return;
      releaseHucPopupSuppression(
        event.type === 'keydown' ?
          'box zoom canceled' :
          'next pointer gesture'
      );
    };
    document.addEventListener(
      'pointerdown',
      documentPopupResetHandler,
      true
    );
    document.addEventListener(
      'mousedown',
      documentPopupResetHandler,
      true
    );
    document.addEventListener(
      'keydown',
      documentPopupResetHandler,
      true
    );
  }

  function refreshVisibleHucLayers() {
    visibleHucLayers = {};
    Object.keys(hucLevels).forEach(function(levelName) {
      if (hucLevels[levelName].active) {
        visibleHucLayers[levelName] = true;
      }
    });
  }

  refreshVisibleHucLayers();

  function getMapContainerForHucControls() {
    return map.getContainer ? map.getContainer() : el.querySelector('.leaflet-container');
  }

  function addFloatingHucThemeControl() {
    var mapContainer = getMapContainerForHucControls();

    if (!mapContainer) {
      console.warn('BRIM HUC fill control: map container not found.');
      return;
    }

    var oldControl = mapContainer.querySelector('.pt-huc-theme-control');
    if (oldControl) oldControl.remove();

    var div = document.createElement('div');
    div.className = 'pt-huc-theme-control leaflet-control';
    div.style.position = 'absolute';
    div.style.top = '198px';
    div.style.left = '8px';
    div.style.zIndex = '10010';
    div.style.background = 'rgba(255, 255, 255, 0.96)';
    div.style.padding = '6px';
    div.style.border = '1px solid #777';
    div.style.borderRadius = '4px';
    div.style.boxShadow = '0 1px 4px rgba(0,0,0,0.35)';
    div.style.font = '12px Arial, sans-serif';
    div.style.boxSizing = 'border-box';
    div.style.display = 'none';
    div.innerHTML =
      "<label style='font-weight:bold;display:block;margin-bottom:3px;'>HUC fill</label>" +
      "<select id='pt-huc-theme-select' style='font-size:12px;max-width:210px;'>" +
        "<option value='none'>None / boundaries only</option>" +
        "<option value='ppt_in'>PRISM precip - in/yr</option>" +
        "<option value='ppt_kaf'>PRISM precip - kaf/yr</option>" +
        "<option value='rech_in'>BCMv8 recharge - in/yr</option>" +
        "<option value='rech_kaf'>BCMv8 recharge - kaf/yr</option>" +
      "</select>" +
      "<div id='pt-huc-theme-status' aria-live='polite' " +
        "style='display:none;margin-top:3px;font-size:10px;line-height:1.15;color:#24527a;'>" +
      "</div>";

    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    mapContainer.appendChild(div);
  }

  function addFloatingHucLegend() {
    var mapContainer = getMapContainerForHucControls();

    if (!mapContainer) {
      console.warn('BRIM HUC legend: map container not found.');
      return;
    }

    var oldLegend = mapContainer.querySelector('.pt-huc-theme-legend');
    if (oldLegend) oldLegend.remove();

    var div = document.createElement('div');
    div.className = 'pt-huc-theme-legend leaflet-control';
    div.style.position = 'absolute';
    div.style.top = '12px';
    div.style.left = '126px';
    div.style.zIndex = '10010';
    div.style.background = 'rgba(255, 255, 255, 0.94)';
    div.style.padding = '6px';
    div.style.border = '1px solid #777';
    div.style.borderRadius = '4px';
    div.style.boxShadow = '0 1px 4px rgba(0,0,0,0.35)';
    div.style.font = '12px Arial, sans-serif';
    div.style.width = '300px';
    div.style.maxWidth = '300px';
    div.style.maxHeight = '220px';
    div.style.overflowY = 'auto';
    div.style.display = 'none';
    div.style.boxSizing = 'border-box';

    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    mapContainer.appendChild(div);
  }

  addFloatingHucThemeControl();
  addFloatingHucLegend();

  function layerSortValue(layerName) {
    var n = parseInt(String(layerName).replace('huc', ''), 10);
    return isNaN(n) ? 999 : n;
  }

  function legendRows(legend) {
    if (!legend) return '';

    var rows = legend.rows || [];
    var html = '<b>' + (legend.title || 'HUC legend') + '</b><br/>';

    if (rows.length === 0) {
      html += '<div>No mapped values for this HUC level.</div>';
    }

    var useTwoCols = rows.length >= 9;
    if (useTwoCols) {
      html += "<div style='display:grid;grid-template-columns:1fr 1fr;" +
        "column-gap:8px;row-gap:1px;margin-top:2px;'>";
    }

    for (var i = 0; i < rows.length; i++) {
      html +=
        "<div style='white-space:nowrap;min-width:0;'>" +
        "<span style='display:inline-block;width:12px;height:12px;margin-right:4px;" +
        "border:1px solid #777;background:" + rows[i].color +
        ";vertical-align:-1px;'></span>" +
        "<span style='font-size:11px;'>" + rows[i].label + '</span>' +
        '</div>';
    }

    if (useTwoCols) html += '</div>';
    if (legend.note) {
      html += "<div style='font-size:10px;margin-top:3px;color:#444;'>" +
        legend.note + '</div>';
    }
    return html;
  }

  function legendHtml(theme) {
    if (theme === 'none') return '';

    var levels = Object.keys(visibleHucLayers).sort(function(a, b) {
      return layerSortValue(a) - layerSortValue(b);
    });

    if (levels.length === 0) {
      return '<b>HUC fill</b><br/>Turn on a HUC layer to see the legend.';
    }

    var html = '';
    if (levels.length > 1) {
      html += "<div style='font-size:10px;margin-bottom:4px;color:#444;'>" +
        'Multiple HUC levels are visible. Colors are scaled separately by HUC level.' +
        '</div>';
    }

    for (var i = 0; i < levels.length; i++) {
      var levelName = levels[i];
      var legend = hucThemeLegends[levelName] ?
        hucThemeLegends[levelName][theme] :
        null;
      if (i > 0) {
        html += "<hr style='border:none;border-top:1px solid #ccc;margin:6px 0;'/>";
      }
      html += legendRows(legend);
    }
    return html;
  }

  function updateHucThemeControlDisplay() {
    var control = document.querySelector('.pt-huc-theme-control');
    if (!control) return;
    control.style.display = Object.keys(visibleHucLayers).length > 0 ? 'block' : 'none';
  }

  function updateLegend() {
    var div = document.querySelector('.pt-huc-theme-legend');
    updateHucThemeControlDisplay();
    if (!div) return;

    var hasVisibleHuc = Object.keys(visibleHucLayers).length > 0;
    if (!hasVisibleHuc || hucTheme === 'none') hucLegendUserHidden = false;
    var html = legendHtml(hucTheme);
    div.style.display = (
      !hasVisibleHuc ||
      hucTheme === 'none' ||
      html === '' ||
      hucLegendUserHidden
    ) ? 'none' : 'block';
    div.innerHTML =
      '<div style="display:flex;justify-content:space-between;align-items:flex-start;' +
      'gap:8px;margin-bottom:3px;"><span style="font-weight:700;">' +
      'HUC thematic fill</span><button type="button" ' +
      'class="pt-huc-theme-legend-close" aria-label="Hide HUC thematic-fill legend" ' +
      'title="Hide HUC thematic-fill legend" style="border:0;background:transparent;' +
      'color:#666;font-size:17px;line-height:1;cursor:pointer;padding:0 2px;">' +
      '&times;</button></div>' + html;

    var close = div.querySelector('.pt-huc-theme-legend-close');
    if (close) {
      close.addEventListener('click', function(event) {
        event.preventDefault();
        event.stopPropagation();
        hucLegendUserHidden = true;
        div.style.display = 'none';
      }, false);
    }
  }

  var hucThemeStatusTimer = null;

  function setHucThemeStatus(message, autoHideMs) {
    var status = document.getElementById('pt-huc-theme-status');
    if (!status) return;

    if (hucThemeStatusTimer !== null) {
      window.clearTimeout(hucThemeStatusTimer);
      hucThemeStatusTimer = null;
    }

    if (!message) {
      status.textContent = '';
      status.style.display = 'none';
      return;
    }

    status.textContent = message;
    status.style.display = 'block';

    if (autoHideMs && autoHideMs > 0) {
      hucThemeStatusTimer = window.setTimeout(function() {
        status.textContent = '';
        status.style.display = 'none';
        hucThemeStatusTimer = null;
      }, autoHideMs);
    }
  }

  function fillStyle(layer, theme) {
    var id = getLayerId(layer);
    var record = id === null ? null : hucThemeLookup[id];
    var fillColor = '#FFFFFF';

    if (record && theme !== 'none' && record[theme]) {
      fillColor = record[theme];
    }

    return {
      fillColor: fillColor,
      fillOpacity: theme === 'none' ? 0 : 0.58
    };
  }

  function cancelStyleWork(reason) {
    styleGeneration += 1;

    if (scheduledStyleFrame !== null) {
      if (window.cancelAnimationFrame) {
        window.cancelAnimationFrame(scheduledStyleFrame);
      } else {
        window.clearTimeout(scheduledStyleFrame);
      }
      scheduledStyleFrame = null;
    }

    if (activeStyleJob) {
      diagnostics.canceledStyleJobs += 1;
      diagnostics.lastCancelReason = reason || 'unspecified';
      activeStyleJob = null;
    }
  }

  function requestFrame(callback) {
    if (window.requestAnimationFrame) {
      return window.requestAnimationFrame(callback);
    }
    return window.setTimeout(callback, 0);
  }

  /*
   * A newly activated Canvas group already has a draw queued by Leaflet.
   * Updating its retained options synchronously before that frame prevents an
   * old inactive theme from flashing, without issuing thousands of redraws.
   */
  function primeActivatedLevel(levelState, theme) {
    for (var i = 0; i < levelState.layers.length; i++) {
      var layer = levelState.layers[i];
      var style = fillStyle(layer, theme);
      layer.options.fillColor = style.fillColor;
      layer.options.fillOpacity = style.fillOpacity;
    }
    diagnostics.optionPrimeCount += levelState.layers.length;
    levelState.appliedTheme = theme;
    levelState.dirty = false;
  }

  function requestThemeApply(levelNames, reason) {
    cancelStyleWork('superseded by ' + reason);

    var generation = styleGeneration;
    var targetTheme = hucTheme;
    var targets = [];

    levelNames.forEach(function(levelName) {
      var levelState = hucLevels[levelName];
      if (
        levelState &&
        levelState.active &&
        (levelState.dirty || levelState.appliedTheme !== targetTheme)
      ) {
        levelState.dirty = true;
        targets.push(levelState);
      }
    });

    if (targets.length === 0) {
      diagnostics.sameThemeNoops += 1;
      updateLegend();
      setHucThemeStatus('', 0);
      return;
    }

    diagnostics.styleJobCount += 1;
    var started = nowMs();
    var operations = 0;
    var targetIndex = 0;
    var layerIndex = 0;

    activeStyleJob = {
      generation: generation,
      theme: targetTheme,
      reason: reason,
      levels: targets.map(function(levelState) { return levelState.name; })
    };

    function runStyleFrame() {
      scheduledStyleFrame = null;

      if (generation !== styleGeneration || !activeStyleJob) {
        diagnostics.staleCallbacksPrevented += 1;
        return;
      }

      var deadline = nowMs() + 8;
      var frameOperations = 0;

      while (targetIndex < targets.length) {
        var levelState = targets[targetIndex];

        if (!levelState.active) {
          levelState.dirty = true;
          targetIndex += 1;
          layerIndex = 0;
          continue;
        }

        while (layerIndex < levelState.layers.length) {
          if (generation !== styleGeneration || !levelState.active) {
            diagnostics.staleCallbacksPrevented += 1;
            return;
          }

          var layer = levelState.layers[layerIndex];
          layer.setStyle(fillStyle(layer, targetTheme));
          layerIndex += 1;
          operations += 1;
          frameOperations += 1;
          diagnostics.styleOperationCount += 1;

          if (frameOperations >= 100 && nowMs() >= deadline) {
            scheduledStyleFrame = requestFrame(runStyleFrame);
            return;
          }
        }

        levelState.appliedTheme = targetTheme;
        levelState.dirty = false;
        targetIndex += 1;
        layerIndex = 0;
      }

      diagnostics.lastApplyMs = nowMs() - started;
      diagnostics.lastApplyReason = reason;
      diagnostics.lastApplyOperations = operations;
      activeStyleJob = null;
      updateLegend();
      setHucThemeStatus('HUC fill applied.', 900);
    }

    scheduledStyleFrame = requestFrame(runStyleFrame);
  }

  function activeLevelNames() {
    return Object.keys(hucLevels).filter(function(levelName) {
      return hucLevels[levelName].active;
    });
  }

  function resetHucThemeToBoundariesOnly() {
    hucTheme = 'none';
    var controlSelect = document.getElementById('pt-huc-theme-select');
    if (controlSelect && controlSelect.value !== 'none') {
      controlSelect.value = 'none';
    }
  }

  function setTheme(nextTheme, reason) {
    var allowed = {
      none: true,
      ppt_in: true,
      ppt_kaf: true,
      rech_in: true,
      rech_kaf: true
    };
    if (!allowed[nextTheme]) return false;

    if (nextTheme === hucTheme) {
      diagnostics.sameThemeNoops += 1;
      return false;
    }

    hucTheme = nextTheme;
    var select = document.getElementById('pt-huc-theme-select');
    if (select && select.value !== nextTheme) select.value = nextTheme;
    setHucThemeStatus(
      nextTheme === 'none' ?
        'Returning to boundaries only…' :
        'Applying HUC fill…'
    );
    requestThemeApply(activeLevelNames(), reason || 'theme change');
    return true;
  }

  var select = document.getElementById('pt-huc-theme-select');
  if (select) {
    select.addEventListener('mousedown', function() {
      setHucThemeStatus('Selecting HUC fill…');
    });
    select.addEventListener('focus', function() {
      setHucThemeStatus('Selecting HUC fill…');
    });
    select.addEventListener('keydown', function(event) {
      if (!event || event.key !== 'Escape') {
        setHucThemeStatus('Selecting HUC fill…');
      }
    });
    select.addEventListener('blur', function() {
      window.setTimeout(function() {
        var status = document.getElementById('pt-huc-theme-status');
        if (status && status.textContent === 'Selecting HUC fill…') {
          setHucThemeStatus('', 0);
        }
      }, 250);
    });
    select.addEventListener('change', function(event) {
      setTheme(event.target.value, 'dropdown change');
    });
  } else {
    console.warn('BRIM HUC controller: theme select was not found.');
  }

  function eventLevel(event) {
    if (event && event.name && hucGroupToLevel[String(event.name)]) {
      return hucGroupToLevel[String(event.name)];
    }

    if (event && event.layer) {
      var names = Object.keys(hucLevels);
      for (var i = 0; i < names.length; i++) {
        if (hucLevels[names[i]].root === event.layer) return names[i];
      }
    }
    return null;
  }

  listen(map, 'overlayadd', function(event) {
    var levelName = eventLevel(event);
    if (!levelName) {
      diagnostics.ignoredOverlayEvents += 1;
      return;
    }

    closeCurrentHucTooltip('HUC overlay add or level switch');
    var levelState = hucLevels[levelName];
    var hadVisibleHuc = activeLevelNames().length > 0;
    cancelStyleWork('HUC overlay add');
    levelState.active = true;
    diagnostics.activationCount += 1;

    if (!hadVisibleHuc) {
      resetHucThemeToBoundariesOnly();
    }

    /*
     * Leaflet has queued this Canvas renderer's first draw but has not painted
     * it yet. Prime all options now so activation never shows stale fill.
     */
    primeActivatedLevel(levelState, hucTheme);
    refreshVisibleHucLayers();
    updateLegend();

    /*
     * If another level was in a canceled, partial theme job, finish it with the
     * current selection. The newly activated level is already complete.
     */
    requestThemeApply(activeLevelNames(), 'HUC overlay add reconciliation');
  });

  listen(map, 'overlayremove', function(event) {
    var levelName = eventLevel(event);
    if (!levelName) {
      diagnostics.ignoredOverlayEvents += 1;
      return;
    }

    closeCurrentHucTooltip('HUC overlay remove');
    cancelStyleWork('HUC overlay remove');
    hucLevels[levelName].active = false;
    diagnostics.deactivationCount += 1;
    if (map.closePopup) map.closePopup();
    refreshVisibleHucLayers();

    var remaining = activeLevelNames();
    if (remaining.length === 0) {
      resetHucThemeToBoundariesOnly();
      if (
        sharedHucRenderer &&
        map.hasLayer &&
        map.hasLayer(sharedHucRenderer) &&
        map.removeLayer
      ) {
        map.removeLayer(sharedHucRenderer);
      }
      updateLegend();
      setHucThemeStatus('', 0);
    } else {
      requestThemeApply(remaining, 'HUC overlay remove reconciliation');
    }
  });

  function domSnapshot() {
    var container = getMapContainerForHucControls();
    var count = function(selector) {
      return container && container.querySelectorAll ?
        container.querySelectorAll(selector).length :
        null;
    };
    return {
      totalNodes: count('*'),
      svgPaths: count('svg path'),
      canvasElements: count('canvas'),
      hucFeatureSvgPaths: count('svg path.pt-huc-feature'),
      leafletLayers: map._layers ? Object.keys(map._layers).length : null
    };
  }

  function heapSnapshot() {
    var memory = window.performance && window.performance.memory;
    if (!memory) return null;
    return {
      usedJSHeapSize: memory.usedJSHeapSize,
      totalJSHeapSize: memory.totalJSHeapSize,
      jsHeapSizeLimit: memory.jsHeapSizeLimit
    };
  }

  function stats() {
    var levels = {};
    Object.keys(hucLevels).sort(function(a, b) {
      return layerSortValue(a) - layerSortValue(b);
    }).forEach(function(levelName) {
      var state = hucLevels[levelName];
      levels[levelName] = {
        groupName: state.groupName,
        registeredCount: state.layers.length,
        expectedCount: state.expectedCount,
        active: state.active,
        appliedTheme: state.appliedTheme,
        dirty: state.dirty,
        renderer: rendererName(state.renderer),
        rendererMounted: !!(
          state.renderer &&
          map.hasLayer &&
          map.hasLayer(state.renderer)
        )
      };
    });

    return {
      currentTheme: hucTheme,
      activeLevels: activeLevelNames(),
      generation: styleGeneration,
      applying: activeStyleJob ? {
        theme: activeStyleJob.theme,
        reason: activeStyleJob.reason,
        levels: activeStyleJob.levels
      } : null,
      levels: levels,
      retainedFeatureObjects: Object.keys(hucThemeLookup).length,
      diagnostics: {
        registryBuildMs: diagnostics.registryBuildMs,
        mapScanCount: diagnostics.mapScanCount,
        styleOperationCount: diagnostics.styleOperationCount,
        optionPrimeCount: diagnostics.optionPrimeCount,
        styleJobCount: diagnostics.styleJobCount,
        canceledStyleJobs: diagnostics.canceledStyleJobs,
        staleCallbacksPrevented: diagnostics.staleCallbacksPrevented,
        activationCount: diagnostics.activationCount,
        deactivationCount: diagnostics.deactivationCount,
        ignoredOverlayEvents: diagnostics.ignoredOverlayEvents,
        sameThemeNoops: diagnostics.sameThemeNoops,
        tooltipOpenCount: diagnostics.tooltipOpenCount,
        tooltipCloseCount: diagnostics.tooltipCloseCount,
        tooltipReplacementCount: diagnostics.tooltipReplacementCount,
        tooltipOpen: !!currentHucTooltip,
        tooltipLayerId: getLayerId(currentHucTooltipLayer),
        lastTooltipCloseReason: diagnostics.lastTooltipCloseReason,
        popupSuppressed: hucPopupSuppressed,
        popupSuppressionStartCount: diagnostics.popupSuppressionStartCount,
        popupSuppressionReleaseCount:
          diagnostics.popupSuppressionReleaseCount,
        popupSuppressedOpenCount: diagnostics.popupSuppressedOpenCount,
        lastPopupSuppressionReason:
          diagnostics.lastPopupSuppressionReason,
        listenerCount: listenerRecords.length,
        destroyed: destroyed,
        lastApplyMs: diagnostics.lastApplyMs,
        lastApplyReason: diagnostics.lastApplyReason,
        lastApplyOperations: diagnostics.lastApplyOperations
      },
      dom: domSnapshot(),
      heap: heapSnapshot(),
      longTasks: diagnostics.longTasks.slice()
    };
  }

  var profileEnabled = false;
  var longTaskObserver = null;
  var profileScenario = null;
  var profileReports = [];

  try {
    profileEnabled = (
      new window.URLSearchParams(window.location.search).get('brimProfile') === '1'
    );
  } catch (error) {
    profileEnabled = false;
  }
  if (window.BRIM_ENABLE_HUC_PROFILE === true) profileEnabled = true;

  function enableProfile() {
    profileEnabled = true;
    if (
      !longTaskObserver &&
      window.PerformanceObserver &&
      window.PerformanceObserver.supportedEntryTypes &&
      window.PerformanceObserver.supportedEntryTypes.indexOf('longtask') >= 0
    ) {
      longTaskObserver = new window.PerformanceObserver(function(list) {
        list.getEntries().forEach(function(entry) {
          diagnostics.longTasks.push({
            startTime: entry.startTime,
            duration: entry.duration,
            applyingTheme: activeStyleJob ? activeStyleJob.theme : null,
            applyReason: activeStyleJob ? activeStyleJob.reason : null
          });
        });
      });
      longTaskObserver.observe({entryTypes: ['longtask']});
    }
    return stats();
  }

  function disableProfile() {
    profileEnabled = false;
    if (longTaskObserver) {
      longTaskObserver.disconnect();
      longTaskObserver = null;
    }
  }

  function startScenario(name) {
    if (!profileEnabled) enableProfile();
    profileScenario = {
      name: name || 'HUC scenario',
      startedAt: new Date().toISOString(),
      startedMs: nowMs(),
      longTaskStart: diagnostics.longTasks.length,
      start: stats()
    };
    return profileScenario;
  }

  function endScenario() {
    if (!profileScenario) return null;
    var endedMs = nowMs();
    var report = {
      name: profileScenario.name,
      startedAt: profileScenario.startedAt,
      endedAt: new Date().toISOString(),
      durationMs: endedMs - profileScenario.startedMs,
      start: profileScenario.start,
      end: stats(),
      longTasks: diagnostics.longTasks.slice(profileScenario.longTaskStart)
    };
    report.longTaskCount = report.longTasks.length;
    report.longTaskAtLeast200ms = report.longTasks.filter(function(entry) {
      return entry.duration >= 200;
    }).length;
    report.maxLongTaskMs = report.longTasks.reduce(function(maximum, entry) {
      return Math.max(maximum, entry.duration);
    }, 0);
    profileReports.push(report);
    profileScenario = null;
    return report;
  }

  function downloadProfileJson() {
    var payload = JSON.stringify({
      generatedAt: new Date().toISOString(),
      current: stats(),
      reports: profileReports
    }, null, 2);
    var blob = new Blob([payload], {type: 'application/json'});
    var url = window.URL.createObjectURL(blob);
    var link = document.createElement('a');
    link.href = url;
    link.download = 'brim_huc_profile.json';
    document.body.appendChild(link);
    link.click();
    link.remove();
    window.URL.revokeObjectURL(url);
  }

  function destroy() {
    if (destroyed) return;
    destroyed = true;
    releaseHucPopupSuppression('controller destruction');
    closeCurrentHucTooltip('controller destruction');
    cancelStyleWork('controller destruction');

    if (hucThemeStatusTimer !== null) {
      window.clearTimeout(hucThemeStatusTimer);
      hucThemeStatusTimer = null;
    }

    if (longTaskObserver) {
      longTaskObserver.disconnect();
      longTaskObserver = null;
    }

    listenerRecords.forEach(function(record) {
      try {
        if (record.target && typeof record.target.off === 'function') {
          record.target.off(
            record.eventNames,
            record.handler
          );
        }
      } catch (listenerError) {}
    });
    listenerRecords = [];

    if (
      documentClearClickHandler &&
      typeof document !== 'undefined' &&
      typeof document.removeEventListener === 'function'
    ) {
      document.removeEventListener(
        'click',
        documentClearClickHandler,
        true
      );
      documentClearClickHandler = null;
    }

    if (
      documentPopupResetHandler &&
      typeof document !== 'undefined' &&
      typeof document.removeEventListener === 'function'
    ) {
      document.removeEventListener(
        'pointerdown',
        documentPopupResetHandler,
        true
      );
      document.removeEventListener(
        'mousedown',
        documentPopupResetHandler,
        true
      );
      document.removeEventListener(
        'keydown',
        documentPopupResetHandler,
        true
      );
      documentPopupResetHandler = null;
    }

    var mapContainer = getMapContainerForHucControls();
    if (mapContainer && mapContainer.querySelector) {
      var control = mapContainer.querySelector('.pt-huc-theme-control');
      var legend = mapContainer.querySelector('.pt-huc-theme-legend');
      if (control && typeof control.remove === 'function') control.remove();
      if (legend && typeof legend.remove === 'function') legend.remove();
    }
  }

  listen(map, 'unload', destroy);

  var localApi = {
    stats: stats,
    setTheme: function(theme) {
      return setTheme(theme, 'console');
    },
    closeTooltip: function() {
      return closeCurrentHucTooltip('console');
    },
    destroy: destroy
  };
  window.BRIM_HUC_LOCAL = localApi;
  window.BRIM_HUC_PROFILE = {
    enable: enableProfile,
    disable: disableProfile,
    startScenario: startScenario,
    endScenario: endScenario,
    downloadJson: downloadProfileJson,
    reports: function() { return profileReports.slice(); }
  };

  if (profileEnabled) enableProfile();
  updateLegend();
  console.log(
    'BRIM HUC controller loaded:',
    Object.keys(hucThemeLookup).length,
    'features across',
    Object.keys(hucLevels).length,
    'levels'
  );
}
