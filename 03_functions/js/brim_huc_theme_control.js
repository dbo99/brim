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
  var defaultHucTheme = 'none';
  var focusedHucLevel = null;
  var activationSequence = 0;
  var hucCardUserHidden = false;
  var visibleHucLayers = {};
  var styleGeneration = 0;
  var scheduledStyleFrame = null;
  var activeStyleJob = null;
  var listenerRecords = [];
  var domListenerRecords = [];
  var destroyed = false;
  var card = null;
  var cardControl = null;
  var detachableState = null;
  var select = null;
  var minimumInput = null;
  var minimumValue = null;
  var visibleCount = null;
  var legendBody = null;
  var activeContext = null;
  var statusNode = null;
  var generalizationNode = null;
  var hucThemeStatusMessage = '';
  var currentHucTooltip = null;
  var currentHucTooltipLayer = null;
  var documentClearClickHandler = null;
  var documentPopupResetHandler = null;
  var hucPopupSuppressed = false;
  var hucPopupSuppressionGeneration = 0;
  var hucPopupReleaseTimer = null;

  hucThemeData = hucThemeData || {};

  var hucThemeRows = Array.isArray(hucThemeData.themes) ?
    hucThemeData.themes :
    [];
  var allowedThemes = {};
  hucThemeRows = hucThemeRows.filter(function(row) {
    if (!row || row.id === undefined || row.id === null) return false;
    var id = String(row.id);
    if (!id || allowedThemes[id]) return false;
    allowedThemes[id] = true;
    row.id = id;
    row.label = String(row.label || id);
    return true;
  });
  var requestedDefaultTheme = String(hucThemeData.default_theme || '');
  defaultHucTheme = allowedThemes[requestedDefaultTheme] ?
    requestedDefaultTheme :
    (hucThemeRows.length ? hucThemeRows[0].id : 'none');

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
    membershipOperationCount: 0,
    membershipResetOperationCount: 0,
    membershipResetWhileMountedCount: 0,
    deferredMembershipResetCount: 0,
    canceledMembershipResetCount: 0,
    forcedInactivePathRemovalCount: 0,
    inactiveInvariantFailureCount: 0,
    featureOperationCount: 0,
    optionPrimeCount: 0,
    styleJobCount: 0,
    canceledStyleJobs: 0,
    staleCallbacksPrevented: 0,
    activationCount: 0,
    deactivationCount: 0,
    ignoredOverlayEvents: 0,
    sameThemeNoops: 0,
    sameThresholdNoops: 0,
    sameLevelNoops: 0,
    legendUpdateCount: 0,
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

  function listenDom(target, eventName, handler) {
    if (!target || typeof target.addEventListener !== 'function') return;
    target.addEventListener(eventName, handler, false);
    domListenerRecords.push({
      target: target,
      eventName: eventName,
      handler: handler
    });
  }

  function escapeHtml(value) {
    return String(value === undefined || value === null ? '' : value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
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

  function leafletStamp(layer) {
    if (!layer) return null;
    if (typeof L !== 'undefined' && typeof L.stamp === 'function') {
      return String(L.stamp(layer));
    }
    return layer._leaflet_id === undefined ||
      layer._leaflet_id === null ?
      null :
      String(layer._leaflet_id);
  }

  function rendererHasLayer(renderer, layer) {
    if (!renderer || !renderer._layers || !layer) return null;
    var stamp = leafletStamp(layer);
    if (stamp !== null) return renderer._layers[stamp] === layer;

    var rendererStamps = Object.keys(renderer._layers);
    for (var i = 0; i < rendererStamps.length; i++) {
      if (renderer._layers[rendererStamps[i]] === layer) return true;
    }
    return false;
  }

  function mountedLayerCount(levelState) {
    return levelState.layers.reduce(function(total, layer) {
      return total + (layer && layer._map ? 1 : 0);
    }, 0);
  }

  function rendererPathCount(levelState) {
    if (!levelState.renderer || !levelState.renderer._layers) return null;
    return levelState.layers.reduce(function(total, layer) {
      return total + (rendererHasLayer(levelState.renderer, layer) ? 1 : 0);
    }, 0);
  }

  function layerControlChecked(levelState) {
    var container = getMapContainerForHucControls();
    if (
      !container ||
      !container.querySelectorAll ||
      !levelState.root
    ) {
      return null;
    }

    var rootStamp = leafletStamp(levelState.root);
    if (rootStamp === null) return null;
    var selectors = container.querySelectorAll(
      '.leaflet-control-layers-selector'
    );
    for (var i = 0; i < selectors.length; i++) {
      if (String(selectors[i].layerId) === rootStamp) {
        return !!selectors[i].checked;
      }
    }
    return null;
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

      var members = layers.map(function(layer) {
        return !root || typeof root.hasLayer !== 'function' ?
          true :
          root.hasLayer(layer);
      });

      hucLevels[levelName] = {
        name: levelName,
        label: row.huc_label ? String(row.huc_label) : levelName.toUpperCase(),
        groupName: groupName,
        generalizationDisclosure: String(row.generalization_disclosure || ''),
        expectedCount: Number(row.expected_count) || 0,
        layers: layers,
        members: members,
        root: root || null,
        renderer: renderer,
        active: rootIsActive,
        selectedTheme: defaultHucTheme,
        minimumBlmPct: 0,
        lastActivated: rootIsActive ? ++activationSequence : 0,
        appliedTheme: 'none',
        appliedMinimumBlm: members.every(function(member) {
          return member;
        }) ? 0 : null,
        eligibleCount: members.filter(function(member) {
          return member;
        }).length,
        lastMembershipResetMs: null,
        lastMembershipResetOperations: 0,
        lastMembershipResetDetached: null,
        lastMembershipResetRendererPaths: null,
        lastApplyReason: null,
        lastResetReason: null,
        lastResetCancelReason: null,
        lifecycleGeneration: 0,
        resetPending: false,
        pendingResetReason: null,
        pendingResetFrame: null,
        activationPending: false,
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
      if (clearButton) {
        closeCurrentHucTooltip('clear action');
        cancelStyleWork('clear action');
        setHucThemeStatus('', 0);
      }
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

  function addCardCss() {
    if (document.getElementById('pt-huc-theme-card-css')) return;
    var style = document.createElement('style');
    style.id = 'pt-huc-theme-card-css';
    style.textContent =
      '.pt-huc-theme-card{width:300px;max-width:calc(100vw - 24px);' +
        'box-sizing:border-box;padding:8px;border:1px solid rgba(111,89,52,.58);' +
        'border-radius:6px;box-shadow:0 1px 5px rgba(0,0,0,.30);' +
        'font:12px/1.25 Arial,sans-serif;color:#222;}' +
      '.pt-huc-theme-head{display:flex;align-items:flex-start;' +
        'justify-content:space-between;gap:8px;margin-bottom:7px;}' +
      '.pt-huc-theme-title{font-weight:700;font-size:13px;line-height:1.2;' +
        'color:#3f2918;}' +
      '.pt-huc-theme-display{display:grid;grid-template-columns:auto 1fr;' +
        'align-items:center;gap:5px;margin-bottom:5px;}' +
      '.pt-huc-theme-display label{font-weight:700;}' +
      '.pt-huc-theme-select{min-width:0;width:100%;font-size:12px;}' +
      '.pt-huc-theme-filter{border-top:1px solid rgba(111,89,52,.23);' +
        'padding-top:5px;margin-top:5px;}' +
      '.pt-huc-theme-filter-head{display:flex;align-items:baseline;' +
        'justify-content:space-between;gap:8px;font-weight:700;}' +
      '.pt-huc-theme-filter-value{font-variant-numeric:tabular-nums;' +
        'white-space:nowrap;}' +
      '.pt-huc-theme-minimum{display:block;width:100%;margin:3px 0 1px;}' +
      '.pt-huc-theme-visible-count{font-size:10.5px;color:#555;' +
        'font-variant-numeric:tabular-nums;}' +
      '.pt-huc-theme-context{border-top:1px solid rgba(111,89,52,.23);' +
        'font-size:10.5px;color:#555;margin-top:5px;padding-top:5px;}' +
      '.pt-huc-theme-legend-body{padding-top:4px;}' +
      '.pt-huc-theme-legend-body[hidden]{display:none;}' +
      '.pt-huc-theme-legend-level+.pt-huc-theme-legend-level{' +
        'border-top:1px solid rgba(111,89,52,.23);margin-top:6px;padding-top:6px;}' +
      '.pt-huc-theme-legend-title{font-weight:700;margin-bottom:4px;}' +
      '.pt-huc-theme-rows.is-two-column{display:grid;' +
        'grid-template-columns:1fr 1fr;column-gap:8px;}' +
      '.pt-huc-theme-row{display:flex;align-items:center;gap:5px;margin:2px 0;' +
        'min-width:0;}' +
      '.pt-huc-theme-swatch{width:13px;height:13px;flex:0 0 13px;' +
        'border:1px solid #777;box-sizing:border-box;}' +
      '.pt-huc-theme-row-label{flex:1 1 auto;min-width:0;}' +
      '.pt-huc-theme-row-count{flex:0 0 auto;color:#555;' +
        'font-variant-numeric:tabular-nums;}' +
      '.pt-huc-theme-status{display:none;margin-top:5px;color:#24527a;' +
        'font-size:10.5px;line-height:12px;align-items:center;gap:4px;}' +
      '.pt-huc-theme-generalization-note{margin-top:5px;padding-top:4px;' +
        'border-top:1px solid rgba(111,89,52,.23);color:#555;' +
        'font-size:10px;line-height:1.25;}' +
      '.pt-huc-theme-status.is-loading{display:inline-flex;}' +
      '.pt-huc-theme-spinner-slot{display:inline-flex;align-items:center;' +
        'justify-content:center;width:12px;height:12px;flex:0 0 12px;}' +
      '.pt-huc-theme-spinner{display:block;width:10px;height:10px;' +
        'box-sizing:border-box;border:2px solid rgba(36,82,122,.28);' +
        'border-top-color:#24527a;border-radius:50%;' +
        'animation:pt-huc-theme-spin .72s linear infinite;}' +
      '@keyframes pt-huc-theme-spin{to{transform:rotate(360deg);}}';
    document.head.appendChild(style);
  }

  function actionsHtml() {
    if (
      window.BRIM &&
      window.BRIM.legendCloseout &&
      typeof window.BRIM.legendCloseout.actionsHtml === 'function'
    ) {
      return window.BRIM.legendCloseout.actionsHtml(
        'pt-huc-theme-dock',
        'pt-huc-theme-close',
        'HUC thematic card'
      );
    }
    return '<span class="pt-map-card-actions">' +
      '<button type="button" class="pt-map-card-dock pt-huc-theme-dock" ' +
        'title="Undock HUC thematic card">&#x2197;</button>' +
      '<button type="button" class="pt-map-legend-close pt-huc-theme-close" ' +
        'title="Hide HUC thematic card">&times;</button></span>';
  }

  function scheduleSharedLayout() {
    if (
      window.BRIM &&
      window.BRIM.legendCloseout &&
      typeof window.BRIM.legendCloseout.scheduleLayout === 'function'
    ) {
      window.BRIM.legendCloseout.scheduleLayout(card);
    }
  }

  function hideCard() {
    if (!card) return;
    if (detachableState && detachableState.floating && detachableState.dock) {
      detachableState.dock();
    }
    if (card.style.display !== 'none') card.style.display = 'none';
    scheduleSharedLayout();
  }

  function showCard() {
    if (
      !card ||
      Object.keys(visibleHucLayers).length === 0 ||
      hucCardUserHidden
    ) {
      return;
    }
    if (card.style.display !== 'block') card.style.display = 'block';
    scheduleSharedLayout();
  }

  function layerSortValue(layerName) {
    var n = parseInt(String(layerName).replace('huc', ''), 10);
    return isNaN(n) ? 999 : n;
  }

  function legendRows(legend) {
    if (!legend) return '';

    var rows = legend.rows || [];
    if (!rows.length) return '';
    var html =
      '<div class="pt-huc-theme-legend-title">' +
      escapeHtml(legend.title || 'HUC legend') +
      '</div>';

    var useTwoCols = rows.length >= 9;
    html += '<div class="pt-huc-theme-rows' +
      (useTwoCols ? ' is-two-column' : '') + '">';

    for (var i = 0; i < rows.length; i++) {
      html +=
        '<div class="pt-huc-theme-row">' +
        '<span class="pt-huc-theme-swatch" style="background:' +
          escapeHtml(rows[i].color || '#9E9E9E') + ';"></span>' +
        '<span class="pt-huc-theme-row-label">' +
          escapeHtml(rows[i].label || '') + '</span>' +
        '<span class="pt-huc-theme-row-count">' +
          escapeHtml(rows[i].count === undefined ? '' : rows[i].count) +
          '</span>' +
        '</div>';
    }

    return html + '</div>';
  }

  function legendHtml() {
    var focused = focusedLevelState();
    if (!focused || focused.selectedTheme === 'none') return '';

    var legend = hucThemeLegends[focused.name] ?
      hucThemeLegends[focused.name][focused.selectedTheme] :
      null;
    var rowsHtml = legendRows(legend);
    return rowsHtml ?
      '<div class="pt-huc-theme-legend-level">' + rowsHtml + '</div>' :
      '';
  }

  function focusedLevelState() {
    var focused = focusedHucLevel ? hucLevels[focusedHucLevel] : null;
    if (focused && focused.active) return focused;

    var active = activeLevelNames().sort(function(a, b) {
      return hucLevels[b].lastActivated - hucLevels[a].lastActivated;
    });
    if (!active.length) return null;
    focusedHucLevel = active[0];
    return hucLevels[focusedHucLevel];
  }

  function currentFocusedTheme() {
    var focused = focusedHucLevel ? hucLevels[focusedHucLevel] : null;
    return focused ? focused.selectedTheme : defaultHucTheme;
  }

  function currentFocusedMinimumBlm() {
    var focused = focusedHucLevel ? hucLevels[focusedHucLevel] : null;
    return focused ? focused.minimumBlmPct : 0;
  }

  function updateVisibleCount() {
    if (!visibleCount) return;
    var focused = focusedLevelState();
    if (!focused) {
      visibleCount.textContent = '';
      return;
    }

    if (
      focused.dirty ||
      focused.appliedMinimumBlm !== focused.minimumBlmPct
    ) {
      visibleCount.textContent = 'Updating active HUC membership\u2026';
      return;
    }

    visibleCount.textContent =
      focused.eligibleCount.toLocaleString() + ' of ' +
      focused.expectedCount.toLocaleString() + ' ' +
      focused.label + ' features shown';
  }

  function updateLegend() {
    if (!card) return;
    var levels = Object.keys(visibleHucLayers).sort(function(a, b) {
      return layerSortValue(a) - layerSortValue(b);
    });
    var focused = focusedLevelState();
    var selectedTheme = focused ?
      focused.selectedTheme :
      currentFocusedTheme();
    if (select && select.value !== selectedTheme) {
      select.value = selectedTheme;
    }
    var focusedMinimum = focused ? focused.minimumBlmPct : 0;
    if (minimumInput && Number(minimumInput.value) !== focusedMinimum) {
      minimumInput.value = String(focusedMinimum);
    }
    if (minimumValue) minimumValue.textContent = focusedMinimum + '%';
    if (activeContext) {
      var activeLabels = levels.map(function(levelName) {
          return hucLevels[levelName].label;
        });
      activeContext.textContent = levels.length > 1 && focused ?
        'Selected: ' + focused.label + '; active: ' +
          activeLabels.join(', ') :
        (levels.length ? 'Active: ' + activeLabels.join(', ') : '');
    }
    if (legendBody) {
      var html = legendHtml();
      legendBody.innerHTML = html;
      legendBody.hidden = !html;
    }
    updateVisibleCount();
    renderHucThemeStatus();
    if (generalizationNode) {
      generalizationNode.textContent = focused ?
        focused.generalizationDisclosure : '';
    }
    diagnostics.legendUpdateCount += 1;
    if (levels.length) showCard();
    else hideCard();
    scheduleSharedLayout();
  }

  function createCard() {
    addCardCss();
    cardControl = L.control({position: 'bottomleft'});
    cardControl.onAdd = function() {
      var div = L.DomUtil.create(
        'div',
        'leaflet-control pt-map-legend-card pt-map-legend-local ' +
        'pt-huc-theme-card'
      );
      var optionHtml = hucThemeRows.map(function(row) {
        return '<option value="' + escapeHtml(row.id) + '">' +
          escapeHtml(row.label) + '</option>';
      }).join('');
      div.innerHTML =
        '<div class="pt-huc-theme-head pt-map-card-handle">' +
          '<span class="pt-huc-theme-title">HUC thematic display</span>' +
          actionsHtml() +
        '</div>' +
        '<div class="pt-huc-theme-display">' +
          '<label for="pt-huc-theme-select">Display</label>' +
          '<select id="pt-huc-theme-select" class="pt-huc-theme-select">' +
            optionHtml +
          '</select>' +
        '</div>' +
        '<div class="pt-huc-theme-filter">' +
          '<div class="pt-huc-theme-filter-head">' +
            '<label for="pt-huc-theme-minimum">' +
              'Minimum BLM-managed land</label>' +
            '<span class="pt-huc-theme-filter-value">0%</span>' +
          '</div>' +
          '<input id="pt-huc-theme-minimum" ' +
            'class="pt-huc-theme-minimum" type="range" ' +
            'min="0" max="100" step="1" value="0">' +
          '<div class="pt-huc-theme-visible-count" aria-live="polite"></div>' +
        '</div>' +
        '<div class="pt-huc-theme-context"></div>' +
        '<div class="pt-huc-theme-legend-body"></div>' +
        '<div class="pt-huc-theme-status" aria-live="polite"></div>' +
        '<div class="pt-huc-theme-generalization-note"></div>';
      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);
      return div;
    };
    cardControl.addTo(map);
    card = getMapContainerForHucControls().querySelector('.pt-huc-theme-card');
    if (!card) return;

    select = card.querySelector('.pt-huc-theme-select');
    minimumInput = card.querySelector('.pt-huc-theme-minimum');
    minimumValue = card.querySelector('.pt-huc-theme-filter-value');
    visibleCount = card.querySelector('.pt-huc-theme-visible-count');
    legendBody = card.querySelector('.pt-huc-theme-legend-body');
    activeContext = card.querySelector('.pt-huc-theme-context');
    statusNode = card.querySelector('.pt-huc-theme-status');
    generalizationNode = card.querySelector(
      '.pt-huc-theme-generalization-note'
    );
    if (select) {
      select.value = currentFocusedTheme();
      listenDom(select, 'change', function(event) {
        setTheme(event.target.value, 'dropdown change');
      });
    }
    if (minimumInput) {
      minimumInput.value = String(currentFocusedMinimumBlm());
      listenDom(minimumInput, 'input', function(event) {
        setMinimumBlmPct(event.target.value, 'minimum %BLM input');
      });
    }

    var close = card.querySelector('.pt-huc-theme-close');
    if (close) {
      listenDom(close, 'click', function(event) {
        event.preventDefault();
        event.stopPropagation();
        hucCardUserHidden = true;
        hideCard();
      });
    }

    if (
      window.BRIM &&
      window.BRIM.legendCloseout &&
      typeof window.BRIM.legendCloseout.makeDetachable === 'function'
    ) {
      detachableState = window.BRIM.legendCloseout.makeDetachable({
        card: card,
        map: map,
        handleSelector: '.pt-huc-theme-head',
        dockSelector: '.pt-huc-theme-dock',
        label: 'HUC thematic card'
      });
    }

    updateLegend();
  }

  var hucThemeStatusTimer = null;

  function focusedLargeLevelHasPendingWork() {
    var focused = focusedLevelState();
    if (
      !focused ||
      !focused.active ||
      (focused.name !== 'huc10' && focused.name !== 'huc12') ||
      !activeStyleJob
    ) {
      return false;
    }
    return activeStyleJob.levels.indexOf(focused.name) >= 0;
  }

  function renderHucThemeStatus() {
    var status = statusNode;
    if (!status) return;

    if (focusedLargeLevelHasPendingWork()) {
      status.classList.add('is-loading');
      status.innerHTML =
        '<span class="pt-huc-theme-spinner-slot" aria-hidden="true">' +
          '<span class="pt-huc-theme-spinner"></span>' +
        '</span><span>Loading\u2026</span>';
      status.style.display = 'inline-flex';
      return;
    }

    status.classList.remove('is-loading');
    if (!hucThemeStatusMessage) {
      status.innerHTML = '';
      status.style.display = 'none';
      return;
    }

    status.innerHTML = escapeHtml(hucThemeStatusMessage);
    status.style.display = 'block';
  }

  function setHucThemeStatus(message, autoHideMs) {
    if (hucThemeStatusTimer !== null) {
      window.clearTimeout(hucThemeStatusTimer);
      hucThemeStatusTimer = null;
    }

    hucThemeStatusMessage = message ? String(message) : '';
    renderHucThemeStatus();

    if (autoHideMs && autoHideMs > 0) {
      hucThemeStatusTimer = window.setTimeout(function() {
        hucThemeStatusMessage = '';
        hucThemeStatusTimer = null;
        renderHucThemeStatus();
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

  function layerPercentBlm(layer) {
    var id = getLayerId(layer);
    var record = id === null ? null : hucThemeLookup[id];
    if (
      !record ||
      record.percent_blm === undefined ||
      record.percent_blm === null ||
      record.percent_blm === ''
    ) {
      return null;
    }
    var value = Number(record.percent_blm);
    return isFinite(value) ? value : null;
  }

  function layerIsEligible(layer, threshold) {
    if (threshold === 0) return true;
    var value = layerPercentBlm(layer);
    return value !== null && value >= threshold;
  }

  function closeLayerInteraction(layer, reason) {
    if (!layer) return;
    if (currentHucTooltipLayer === layer) {
      closeCurrentHucTooltip(reason);
    } else {
      try {
        if (
          typeof layer.isTooltipOpen === 'function' &&
          layer.isTooltipOpen() &&
          typeof layer.closeTooltip === 'function'
        ) {
          layer.closeTooltip();
        }
      } catch (tooltipCloseError) {}
    }

    try {
      var popup = typeof layer.getPopup === 'function' ?
        layer.getPopup() :
        layer._popup;
      if (popup && popup._map && map.closePopup) map.closePopup(popup);
    } catch (popupCloseError) {}
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
    renderHucThemeStatus();
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

  function requestHucApply(levelNames, reason) {
    cancelStyleWork('superseded by ' + reason);

    var generation = styleGeneration;
    var targets = [];

    levelNames.forEach(function(levelName) {
      var levelState = hucLevels[levelName];
      var targetTheme = levelState ? levelState.selectedTheme : null;
      var targetMinimumBlm = levelState ?
        levelState.minimumBlmPct :
        null;
      if (
        levelState &&
        levelState.active &&
        (
          levelState.activationPending ||
          levelState.dirty ||
          levelState.appliedTheme !== targetTheme ||
          levelState.appliedMinimumBlm !== targetMinimumBlm
        )
      ) {
        var applyStyle = (
          levelState.dirty ||
          levelState.appliedTheme !== targetTheme
        );
        var activationOnly = (
          levelState.activationPending &&
          !levelState.dirty &&
          levelState.appliedTheme === targetTheme &&
          levelState.appliedMinimumBlm === targetMinimumBlm
        );
        if (!activationOnly) levelState.dirty = true;
        targets.push({
          state: levelState,
          theme: targetTheme,
          minimumBlmPct: targetMinimumBlm,
          applyStyle: applyStyle,
          activationOnly: activationOnly
        });
      }
    });

    if (targets.length === 0) {
      setHucThemeStatus('', 0);
      updateVisibleCount();
      return;
    }

    diagnostics.styleJobCount += 1;
    var started = nowMs();
    var operations = 0;
    var targetIndex = 0;
    var layerIndex = 0;
    var currentEligibleCount = 0;

    activeStyleJob = {
      generation: generation,
      theme: targets.length === 1 ? targets[0].theme : null,
      themes: targets.reduce(function(result, target) {
        result[target.state.name] = target.theme;
        return result;
      }, {}),
      minimumBlmPct: targets.length === 1 ?
        targets[0].minimumBlmPct :
        null,
      minimumBlmPcts: targets.reduce(function(result, target) {
        result[target.state.name] = target.minimumBlmPct;
        return result;
      }, {}),
      reason: reason,
      levels: targets.map(function(target) { return target.state.name; })
    };
    updateVisibleCount();
    renderHucThemeStatus();

    function runStyleFrame() {
      scheduledStyleFrame = null;

      if (generation !== styleGeneration || !activeStyleJob) {
        diagnostics.staleCallbacksPrevented += 1;
        return;
      }

      var deadline = nowMs() + 8;
      var frameOperations = 0;

      while (targetIndex < targets.length) {
        var target = targets[targetIndex];
        var levelState = target.state;
        var targetTheme = target.theme;
        var targetMinimumBlm = target.minimumBlmPct;

        if (!levelState.active) {
          levelState.activationPending = false;
          levelState.dirty = true;
          targetIndex += 1;
          layerIndex = 0;
          currentEligibleCount = 0;
          continue;
        }

        if (target.activationOnly) {
          levelState.activationPending = false;
          levelState.lastApplyReason = reason;
          targetIndex += 1;
          continue;
        }

        while (layerIndex < levelState.layers.length) {
          if (generation !== styleGeneration || !levelState.active) {
            diagnostics.staleCallbacksPrevented += 1;
            return;
          }

          var layer = levelState.layers[layerIndex];
          var eligible = layerIsEligible(layer, targetMinimumBlm);
          var member = levelState.members[layerIndex];

          if (eligible) {
            currentEligibleCount += 1;
            if (!member) {
              layer.setStyle(fillStyle(layer, targetTheme));
              diagnostics.styleOperationCount += 1;
              if (
                levelState.root &&
                typeof levelState.root.addLayer === 'function'
              ) {
                levelState.root.addLayer(layer);
                levelState.members[layerIndex] = true;
                diagnostics.membershipOperationCount += 1;
              }
            } else if (target.applyStyle) {
              layer.setStyle(fillStyle(layer, targetTheme));
              diagnostics.styleOperationCount += 1;
            }
          } else if (member) {
            closeLayerInteraction(layer, 'HUC minimum %BLM filter');
            if (
              levelState.root &&
              typeof levelState.root.removeLayer === 'function'
            ) {
              levelState.root.removeLayer(layer);
              levelState.members[layerIndex] = false;
              diagnostics.membershipOperationCount += 1;
            }
          }

          layerIndex += 1;
          operations += 1;
          frameOperations += 1;
          diagnostics.featureOperationCount += 1;

          if (frameOperations >= 100 && nowMs() >= deadline) {
            scheduledStyleFrame = requestFrame(runStyleFrame);
            return;
          }
        }

        levelState.appliedTheme = targetTheme;
        levelState.appliedMinimumBlm = targetMinimumBlm;
        levelState.eligibleCount = currentEligibleCount;
        levelState.activationPending = false;
        levelState.lastApplyReason = reason;
        levelState.dirty = false;
        targetIndex += 1;
        layerIndex = 0;
        currentEligibleCount = 0;
      }

      diagnostics.lastApplyMs = nowMs() - started;
      diagnostics.lastApplyReason = reason;
      diagnostics.lastApplyOperations = operations;
      activeStyleJob = null;
      updateLegend();
      setHucThemeStatus('HUC display updated.', 900);
    }

    scheduledStyleFrame = requestFrame(runStyleFrame);
  }

  function activeLevelNames() {
    return Object.keys(hucLevels).filter(function(levelName) {
      return hucLevels[levelName].active;
    });
  }

  function setTheme(nextTheme, reason) {
    nextTheme = String(nextTheme || '');
    if (!allowedThemes[nextTheme]) return false;

    var focused = focusedLevelState();
    if (!focused) return false;

    if (nextTheme === focused.selectedTheme) {
      diagnostics.sameThemeNoops += 1;
      return false;
    }

    focused.selectedTheme = nextTheme;
    if (select && select.value !== nextTheme) select.value = nextTheme;
    setHucThemeStatus(
      nextTheme === 'none' ?
        'Returning to boundaries only…' :
        'Applying HUC fill…'
    );
    requestHucApply(activeLevelNames(), reason || 'theme change');
    return true;
  }

  function focusLevel(levelName) {
    levelName = String(levelName || '');
    var levelState = hucLevels[levelName];
    if (!levelState || !levelState.active) return false;

    if (focusedHucLevel === levelName) {
      diagnostics.sameLevelNoops += 1;
      return false;
    }

    focusedHucLevel = levelName;
    updateLegend();
    return true;
  }

  function inactiveLevelDetached(levelState) {
    var mapHasGroup = !!(
      levelState.root &&
      map.hasLayer &&
      map.hasLayer(levelState.root)
    );
    var groupHasMap = !!(levelState.root && levelState.root._map);
    var mountedCount = mountedLayerCount(levelState);
    var pathCount = rendererPathCount(levelState);
    return (
      !mapHasGroup &&
      !groupHasMap &&
      mountedCount === 0 &&
      (!levelState.renderer || pathCount === 0)
    );
  }

  function cancelInactiveLevelReset(levelState, reason) {
    levelState.lifecycleGeneration += 1;
    if (levelState.pendingResetFrame !== null) {
      if (window.cancelAnimationFrame) {
        window.cancelAnimationFrame(levelState.pendingResetFrame);
      } else {
        window.clearTimeout(levelState.pendingResetFrame);
      }
      levelState.pendingResetFrame = null;
    }
    if (levelState.resetPending) {
      diagnostics.canceledMembershipResetCount += 1;
      levelState.lastResetCancelReason = reason || null;
    }
    levelState.resetPending = false;
    levelState.pendingResetReason = null;
  }

  function removeLingeringInactivePaths(levelState) {
    var removed = 0;
    for (var i = 0; i < levelState.layers.length; i++) {
      var layer = levelState.layers[i];
      if (!layer || !layer._map) continue;

      closeLayerInteraction(layer, 'inactive HUC path cleanup');
      if (
        levelState.root &&
        typeof levelState.root.hasLayer === 'function' &&
        levelState.root.hasLayer(layer) &&
        typeof levelState.root.removeLayer === 'function'
      ) {
        levelState.root.removeLayer(layer);
        levelState.members[i] = false;
        diagnostics.membershipOperationCount += 1;
      }
      if (layer._map && map.removeLayer) {
        map.removeLayer(layer);
      }
      removed += 1;
    }
    diagnostics.forcedInactivePathRemovalCount += removed;
    return removed;
  }

  function restoreDetachedLevelMembership(
    levelState,
    generation,
    reason,
    started
  ) {
    if (
      destroyed ||
      levelState.active ||
      generation !== levelState.lifecycleGeneration
    ) {
      diagnostics.staleCallbacksPrevented += 1;
      return false;
    }

    if (!inactiveLevelDetached(levelState)) return false;

    var restored = 0;
    for (var i = 0; i < levelState.layers.length; i++) {
      if (
        !levelState.members[i] &&
        levelState.root &&
        typeof levelState.root.addLayer === 'function'
      ) {
        levelState.root.addLayer(levelState.layers[i]);
        levelState.members[i] = true;
        restored += 1;
      }
    }

    diagnostics.membershipOperationCount += restored;
    diagnostics.membershipResetOperationCount += restored;
    levelState.eligibleCount = levelState.members.filter(function(member) {
      return member;
    }).length;
    levelState.appliedMinimumBlm = (
      levelState.eligibleCount === levelState.layers.length
    ) ? 0 : null;
    levelState.lastMembershipResetMs = nowMs() - started;
    levelState.lastMembershipResetOperations = restored;
    levelState.lastMembershipResetDetached = true;
    levelState.lastMembershipResetRendererPaths =
      rendererPathCount(levelState);
    levelState.lastResetReason = reason;
    levelState.resetPending = false;
    levelState.pendingResetReason = null;
    levelState.pendingResetFrame = null;
    levelState.dirty = (
      levelState.dirty ||
      levelState.appliedTheme !== defaultHucTheme
    );
    return true;
  }

  function resetInactiveLevelSession(levelState, reason) {
    cancelInactiveLevelReset(levelState, 'superseded inactive reset');
    var generation = levelState.lifecycleGeneration;
    var started = nowMs();

    levelState.selectedTheme = defaultHucTheme;
    levelState.minimumBlmPct = 0;
    levelState.activationPending = false;
    levelState.resetPending = true;
    levelState.pendingResetReason = reason;
    levelState.lastResetReason = reason;
    levelState.lastMembershipResetDetached = null;
    levelState.lastMembershipResetRendererPaths =
      rendererPathCount(levelState);
    levelState.dirty = (
      levelState.dirty ||
      levelState.appliedTheme !== defaultHucTheme
    );

    /*
     * Some layer-control paths publish overlayremove while FeatureGroup
     * removal is still unwinding. Wait only until the current task completes,
     * then require the group, every retained child, and every Canvas path to
     * be detached before restoring filtered members to the inactive group.
     */
    var finalizeAfterRemoval = function() {
      if (
        destroyed ||
        levelState.active ||
        generation !== levelState.lifecycleGeneration
      ) {
        diagnostics.staleCallbacksPrevented += 1;
        return;
      }

      if (restoreDetachedLevelMembership(
        levelState,
        generation,
        reason,
        started
      )) {
        return;
      }

      diagnostics.membershipResetWhileMountedCount += 1;
      removeLingeringInactivePaths(levelState);
      levelState.pendingResetFrame = requestFrame(function() {
        levelState.pendingResetFrame = null;
        if (restoreDetachedLevelMembership(
          levelState,
          generation,
          reason,
          started
        )) {
          return;
        }
        if (
          !destroyed &&
          !levelState.active &&
          generation === levelState.lifecycleGeneration
        ) {
          diagnostics.inactiveInvariantFailureCount += 1;
          levelState.lastMembershipResetMs = nowMs() - started;
          levelState.lastMembershipResetOperations = 0;
          levelState.lastMembershipResetDetached = false;
          levelState.lastMembershipResetRendererPaths =
            rendererPathCount(levelState);
          levelState.resetPending = false;
          levelState.pendingResetReason = null;
        }
      });
    };

    if (restoreDetachedLevelMembership(
      levelState,
      generation,
      reason,
      started
    )) {
      return;
    }

    diagnostics.deferredMembershipResetCount += 1;
    if (typeof Promise !== 'undefined' && Promise.resolve) {
      Promise.resolve().then(finalizeAfterRemoval);
    } else {
      window.setTimeout(finalizeAfterRemoval, 0);
    }
  }

  function setMinimumBlmPct(nextMinimum, reason) {
    var parsed = Math.round(Number(nextMinimum));
    if (!isFinite(parsed)) return false;
    parsed = Math.max(0, Math.min(100, parsed));

    var focused = focusedLevelState();
    if (!focused) return false;

    if (parsed === focused.minimumBlmPct) {
      diagnostics.sameThresholdNoops += 1;
      return false;
    }

    focused.minimumBlmPct = parsed;
    if (minimumInput && Number(minimumInput.value) !== parsed) {
      minimumInput.value = String(parsed);
    }
    if (minimumValue) minimumValue.textContent = parsed + '%';
    setHucThemeStatus('Updating HUC membership\u2026');
    requestHucApply(
      activeLevelNames(),
      reason || 'minimum %BLM change'
    );
    return true;
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

    var levelState = hucLevels[levelName];
    if (levelState.active) {
      diagnostics.sameLevelNoops += 1;
      return;
    }
    closeCurrentHucTooltip('HUC overlay add or level switch');
    var hadVisibleHuc = activeLevelNames().length > 0;
    cancelInactiveLevelReset(levelState, 'HUC overlay add');
    cancelStyleWork('HUC overlay add');
    levelState.active = true;
    levelState.activationPending = (
      levelName === 'huc10' ||
      levelName === 'huc12'
    );
    levelState.lastActivated = ++activationSequence;
    focusedHucLevel = levelName;
    diagnostics.activationCount += 1;

    if (!hadVisibleHuc) {
      hucCardUserHidden = false;
    }

    /*
     * Leaflet has queued this Canvas renderer's first draw but has not painted
     * it yet. Prime all options now so activation never shows stale fill.
     */
    primeActivatedLevel(levelState, levelState.selectedTheme);
    refreshVisibleHucLayers();
    updateLegend();

    /*
     * If another level was in a canceled, partial theme job, finish it with the
     * current selection. The newly activated level is already complete.
     */
    requestHucApply(activeLevelNames(), 'HUC overlay add reconciliation');
  });

  listen(map, 'overlayremove', function(event) {
    var levelName = eventLevel(event);
    if (!levelName) {
      diagnostics.ignoredOverlayEvents += 1;
      return;
    }

    var levelState = hucLevels[levelName];
    if (!levelState.active) {
      diagnostics.sameLevelNoops += 1;
      return;
    }
    closeCurrentHucTooltip('HUC overlay remove');
    cancelStyleWork('HUC overlay remove');
    levelState.active = false;
    levelState.activationPending = false;
    resetInactiveLevelSession(levelState, 'HUC overlay remove');
    diagnostics.deactivationCount += 1;
    if (map.closePopup) map.closePopup();
    refreshVisibleHucLayers();

    var remaining = activeLevelNames();
    if (remaining.length === 0) {
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
      if (focusedHucLevel === levelName) focusedHucLevel = null;
      updateLegend();
      requestHucApply(remaining, 'HUC overlay remove reconciliation');
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
        checked: layerControlChecked(state),
        active: state.active,
        focused: focusedHucLevel === levelName,
        mapHasGroup: !!(
          state.root &&
          map.hasLayer &&
          map.hasLayer(state.root)
        ),
        groupMapAttached: !!(state.root && state.root._map),
        mountedCount: mountedLayerCount(state),
        rendererPathCount: rendererPathCount(state),
        selectedTheme: state.selectedTheme,
        minimumBlmPct: state.minimumBlmPct,
        appliedTheme: state.appliedTheme,
        appliedMinimumBlm: state.appliedMinimumBlm,
        eligibleCount: state.eligibleCount,
        memberCount: state.members.filter(function(member) {
          return member;
        }).length,
        lastMembershipResetMs: state.lastMembershipResetMs,
        lastMembershipResetOperations:
          state.lastMembershipResetOperations,
        lastMembershipResetDetached:
          state.lastMembershipResetDetached,
        lastMembershipResetRendererPaths:
          state.lastMembershipResetRendererPaths,
        lastApplyReason: state.lastApplyReason,
        lastResetReason: state.lastResetReason,
        lastResetCancelReason: state.lastResetCancelReason,
        lifecycleGeneration: state.lifecycleGeneration,
        resetPending: state.resetPending,
        pendingResetReason: state.pendingResetReason,
        activationPending: state.activationPending,
        stylePending: !!(
          activeStyleJob &&
          activeStyleJob.levels.indexOf(levelName) >= 0
        ),
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
      currentTheme: currentFocusedTheme(),
      focusedLevel: focusedHucLevel,
      currentMinimumBlm: currentFocusedMinimumBlm(),
      eligibleCount: activeLevelNames().reduce(function(total, levelName) {
        return total + hucLevels[levelName].eligibleCount;
      }, 0),
      activeLevels: activeLevelNames(),
      generation: styleGeneration,
      applying: activeStyleJob ? {
        theme: activeStyleJob.theme,
        themes: activeStyleJob.themes,
        minimumBlmPct: activeStyleJob.minimumBlmPct,
        minimumBlmPcts: activeStyleJob.minimumBlmPcts,
        reason: activeStyleJob.reason,
        levels: activeStyleJob.levels
      } : null,
      levels: levels,
      retainedFeatureObjects: Object.keys(hucThemeLookup).length,
      cardVisible: !!(
        card &&
        card.style.display !== 'none' &&
        !hucCardUserHidden
      ),
      cardUserHidden: hucCardUserHidden,
      stylePending: scheduledStyleFrame !== null,
      loadingIndicatorVisible: focusedLargeLevelHasPendingWork(),
      pendingResetLevels: Object.keys(hucLevels).filter(function(levelName) {
        return hucLevels[levelName].resetPending;
      }),
      diagnostics: {
        registryBuildMs: diagnostics.registryBuildMs,
        mapScanCount: diagnostics.mapScanCount,
        styleOperationCount: diagnostics.styleOperationCount,
        membershipOperationCount: diagnostics.membershipOperationCount,
        membershipResetOperationCount:
          diagnostics.membershipResetOperationCount,
        membershipResetWhileMountedCount:
          diagnostics.membershipResetWhileMountedCount,
        deferredMembershipResetCount:
          diagnostics.deferredMembershipResetCount,
        canceledMembershipResetCount:
          diagnostics.canceledMembershipResetCount,
        forcedInactivePathRemovalCount:
          diagnostics.forcedInactivePathRemovalCount,
        inactiveInvariantFailureCount:
          diagnostics.inactiveInvariantFailureCount,
        featureOperationCount: diagnostics.featureOperationCount,
        optionPrimeCount: diagnostics.optionPrimeCount,
        styleJobCount: diagnostics.styleJobCount,
        styleFilterJobCount: diagnostics.styleJobCount,
        canceledStyleJobs: diagnostics.canceledStyleJobs,
        staleCallbacksPrevented: diagnostics.staleCallbacksPrevented,
        activationCount: diagnostics.activationCount,
        deactivationCount: diagnostics.deactivationCount,
        ignoredOverlayEvents: diagnostics.ignoredOverlayEvents,
        sameThemeNoops: diagnostics.sameThemeNoops,
        sameThresholdNoops: diagnostics.sameThresholdNoops,
        sameLevelNoops: diagnostics.sameLevelNoops,
        legendUpdateCount: diagnostics.legendUpdateCount,
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
        domListenerCount: domListenerRecords.length,
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
    Object.keys(hucLevels).forEach(function(levelName) {
      cancelInactiveLevelReset(
        hucLevels[levelName],
        'controller destruction'
      );
      hucLevels[levelName].activationPending = false;
    });
    hucThemeStatusMessage = '';
    renderHucThemeStatus();

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
    domListenerRecords.forEach(function(record) {
      try {
        record.target.removeEventListener(
          record.eventName,
          record.handler,
          false
        );
      } catch (domListenerError) {}
    });
    domListenerRecords = [];

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

    if (detachableState && typeof detachableState.destroy === 'function') {
      detachableState.destroy(true, true);
      detachableState = null;
    } else if (card && card.parentNode) {
      card.parentNode.removeChild(card);
    }
    if (cardControl && typeof cardControl.remove === 'function') {
      try { cardControl.remove(); } catch (controlError) {}
    }
    card = null;
    select = null;
    minimumInput = null;
    minimumValue = null;
    visibleCount = null;
    legendBody = null;
    activeContext = null;
    statusNode = null;
  }

  createCard();
  var installedActiveLevels = activeLevelNames();
  if (installedActiveLevels.length) {
    focusedHucLevel = installedActiveLevels.sort(function(a, b) {
      return hucLevels[b].lastActivated - hucLevels[a].lastActivated;
    })[0];
    installedActiveLevels.forEach(function(levelName) {
      var levelState = hucLevels[levelName];
      primeActivatedLevel(levelState, levelState.selectedTheme);
      levelState.activationPending = (
        levelName === 'huc10' ||
        levelName === 'huc12'
      );
      levelState.dirty = !levelState.activationPending;
    });
    updateLegend();
    requestHucApply(
      installedActiveLevels,
      'active controller installation'
    );
  }

  listen(map, 'unload', destroy);

  var localApi = {
    stats: stats,
    setTheme: function(theme) {
      return setTheme(theme, 'console');
    },
    focusLevel: focusLevel,
    setMinimumBlmPct: function(minimum) {
      return setMinimumBlmPct(minimum, 'console');
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
  console.log(
    'BRIM HUC controller loaded:',
    Object.keys(hucThemeLookup).length,
    'features across',
    Object.keys(hucLevels).length,
    'levels'
  );
}
