function(el, x, data) {
  var map = this;
  var groupName = data && data.groupName ?
    String(data.groupName) :
    'Points – USGS monitoring wells';
  var columns = data && data.records ? data.records : {};
  var locations = data && data.locations ? data.locations : {};
  var metadata = data && data.metadata ? data.metadata : {};

  if (
    window.BRIM_USGS_GW_LOCAL &&
    typeof window.BRIM_USGS_GW_LOCAL.destroy === 'function'
  ) {
    try { window.BRIM_USGS_GW_LOCAL.destroy(); } catch (oldControllerErr) {}
  }

  var siteCount = Number(metadata.siteCount || columnLength(columns.site_no));
  var locationCount = Number(
    metadata.uniqueCoordinateCount || columnLength(locations.site_start)
  );
  var clusterToExactZoom = Number(metadata.clusterToExactZoom || 11);
  var spatialCellDegrees = 0.25;
  var frameBudgetMs = 8;
  var ordinaryWellRadius = 4.8;
  var ordinaryWellStrokeWeight = 0.95;
  var displayPaneName = 'pane_usgs_gw_display';
  var displayPaneZIndex = 584;
  var interactivePaneName = 'pane_usgs_gw_interactive';
  var interactivePaneZIndex = 585;
  var generation = 0;
  var scheduledFrame = null;
  var scheduledTimer = null;
  var statusClearTimer = null;
  var deferredCleanupCount = 0;
  var gridIndex = null;
  var filterMask = null;
  var filterMaskReady = true;
  var filterVersion = 0;
  var filterSignature = '';
  var currentFilters = defaultFilters();
  var layerActive = false;
  var loading = false;
  var builtOnce = false;
  var currentRoot = null;
  var currentRootComplete = false;
  var currentRenderKey = '';
  var lastFiltered = siteCount;
  var lastDrawn = 0;
  var lastVisibleSites = 0;
  var lastVisibleLocations = 0;
  var lastClusterObjects = 0;
  var lastExactObjects = 0;
  var lastQueryLocations = 0;
  var renderStatusText = '';
  var renderStatusBusy = false;
  var pointRenderer = typeof L.canvas === 'function' ?
    L.canvas({
      pane: displayPaneName,
      padding: 0.45,
      tolerance: 4
    }) :
    null;
  var homeButton = null;
  var homeButtonHandler = null;
  var destroyed = false;
  var listenerRecords = [];
  var mapMoving = false;
  var mapZooming = false;
  var debugEnabled = false;
  var debugLastPointer = null;
  var debugLastNestedEvent = null;
  var debugApi = null;
  var interactivePaneObserver = null;
  var containerListenerRecords = [];
  var rendererForwardedMoveCount = 0;
  var rendererForwardedClickCount = 0;
  var rendererHitCount = 0;

  function columnLength(value) {
    return Array.isArray(value) ? value.length : 0;
  }

  function valueAt(name, index) {
    var values = columns[name];
    if (!Array.isArray(values)) return null;
    return values[index];
  }

  function locationValue(name, index) {
    var values = locations[name];
    if (!Array.isArray(values)) return null;
    return values[index];
  }

  function locationLat(index) {
    var start = Number(locationValue('site_start', index) || 0);
    return Number(valueAt('pt_lat', start));
  }

  function locationLng(index) {
    var start = Number(locationValue('site_start', index) || 0);
    return Number(valueAt('pt_lng', start));
  }

  function has(value) {
    if (value === null || value === undefined) return false;
    var text = String(value).trim();
    return (
      text !== '' &&
      text !== 'NA' &&
      text !== 'NaN' &&
      text !== 'null' &&
      text !== 'undefined'
    );
  }

  function esc(value) {
    if (!has(value)) return 'NA';
    return String(value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function escLoose(value) {
    return String(value == null ? '' : value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function bool(value) {
    if (value === true) return true;
    if (value === false || value === null || value === undefined) return false;
    return ['true', 't', '1', 'yes', 'y'].indexOf(
      String(value).trim().toLowerCase()
    ) >= 0;
  }

  function num(value) {
    if (value === null || value === undefined || value === '') return null;
    var text = String(value).replace(/,/g, '').trim();
    if (
      text === '' ||
      text === 'NA' ||
      text === 'NaN' ||
      text === 'null'
    ) return null;
    var parsed = Number(text);
    return isNaN(parsed) ? null : parsed;
  }

  function fmt(value) {
    var parsed = Number(value);
    return isFinite(parsed) ? parsed.toLocaleString() : '0';
  }

  function fmtNum(value, digits) {
    var parsed = num(value);
    if (parsed === null) return 'NA';
    return parsed.toLocaleString(undefined, {
      minimumFractionDigits: digits,
      maximumFractionDigits: digits
    });
  }

  function compactCount(value) {
    var parsed = Number(value || 0);
    if (parsed >= 1000000) return (parsed / 1000000).toFixed(1) + 'm';
    if (parsed >= 10000) return Math.round(parsed / 1000) + 'k';
    if (parsed >= 1000) return (parsed / 1000).toFixed(1) + 'k';
    return String(parsed);
  }

  function cssColor(value, fallback) {
    if (!has(value)) return fallback;
    var text = String(value).trim();
    if (
      /^#[0-9a-fA-F]{3,8}$/.test(text) ||
      /^rgba?\([0-9.,\s]+\)$/.test(text)
    ) return text;
    return fallback;
  }

  function ensureDisplayPane() {
    var pane = map.getPane ? map.getPane(displayPaneName) : null;
    if (!pane && map.createPane) pane = map.createPane(displayPaneName);
    if (!pane) return null;
    pane.style.zIndex = String(displayPaneZIndex);
    /*
     * A Leaflet Canvas covers its full pane, not only painted circles. Keep
     * this USGS-only display surface out of DOM hit testing; the bounded
     * virtualized circle-marker hits are forwarded explicitly below.
     */
    pane.style.pointerEvents = 'none';
    return pane;
  }

  function ensureInteractivePane() {
    var pane = map.getPane ? map.getPane(interactivePaneName) : null;
    if (!pane && map.createPane) {
      pane = map.createPane(interactivePaneName);
    }
    if (!pane) return null;
    pane.style.zIndex = String(interactivePaneZIndex);
    syncInteractivePanePointerState(pane);
    return pane;
  }

  function syncInteractivePanePointerState(existingPane) {
    var pane = existingPane ||
      (map.getPane ? map.getPane(interactivePaneName) : null);
    if (!pane) return;
    var measureActive = !!(
      map._ptMeasureInteractionActive ||
      pane.getAttribute('data-pt-measure-suspended') === 'true'
    );
    var expected = measureActive || !layerActive ? 'none' : 'auto';
    if (pane.style.pointerEvents !== expected) {
      pane.style.pointerEvents = expected;
    }
  }

  function installInteractivePaneObserver() {
    var pane = ensureInteractivePane();
    if (!pane || interactivePaneObserver || !window.MutationObserver) return;
    interactivePaneObserver = new window.MutationObserver(function() {
      syncInteractivePanePointerState(pane);
    });
    interactivePaneObserver.observe(pane, {
      attributes: true,
      attributeFilter: [
        'style',
        'class',
        'data-pt-measure-suspended'
      ]
    });
  }

  function mapContainer() {
    return map && map.getContainer ? map.getContainer() : null;
  }

  function eventTargetsUsgsDomMarker(event) {
    var target = event && event.target;
    if (!target || typeof target.closest !== 'function') return false;
    return !!target.closest(
      '.pt-usgs-gw-virtual-nested-divicon,' +
      '.pt-usgs-gw-grid-cluster-divicon'
    );
  }

  function eventTargetsMapUi(event) {
    var target = event && event.target;
    if (!target || typeof target.closest !== 'function') return false;
    return !!target.closest(
      '.leaflet-control,.leaflet-popup,.leaflet-tooltip'
    );
  }

  function rendererInteractionSuppressed() {
    return !!(
      destroyed ||
      !layerActive ||
      mapMoving ||
      mapZooming ||
      map._ptMeasureInteractionActive ||
      !currentRoot ||
      !map.hasLayer ||
      !map.hasLayer(currentRoot)
    );
  }

  function rendererHit(event) {
    if (
      !pointRenderer ||
      !pointRenderer._drawFirst ||
      !map.mouseEventToLayerPoint
    ) return null;

    var point;
    try {
      point = map.mouseEventToLayerPoint(event);
    } catch (pointError) {
      return null;
    }

    var hit = null;
    for (
      var order = pointRenderer._drawFirst;
      order;
      order = order.next
    ) {
      var layer = order.layer;
      if (
        layer &&
        layer.options &&
        layer.options.interactive &&
        typeof layer._containsPoint === 'function' &&
        layer._containsPoint(point)
      ) {
        hit = layer;
      }
    }
    return hit;
  }

  function setRendererCursor(active) {
    var container = mapContainer();
    if (
      container &&
      container.classList &&
      typeof container.classList.toggle === 'function'
    ) {
      container.classList.toggle('pt-usgs-gw-manual-hit', !!active);
    }
  }

  function clearRendererHover(event) {
    setRendererCursor(false);
    if (!pointRenderer || !pointRenderer._hoveredLayer) return;
    if (event && typeof pointRenderer._handleMouseOut === 'function') {
      try {
        pointRenderer._handleMouseOut(event);
        return;
      } catch (mouseOutError) {}
    }
    try {
      if (
        pointRenderer._hoveredLayer.closeTooltip &&
        pointRenderer._hoveredLayer.isTooltipOpen &&
        pointRenderer._hoveredLayer.isTooltipOpen()
      ) {
        pointRenderer._hoveredLayer.closeTooltip();
      }
    } catch (tooltipCloseError) {}
    pointRenderer._hoveredLayer = null;
  }

  function stopAtWell(event) {
    if (event.stopImmediatePropagation) event.stopImmediatePropagation();
    if (event.stopPropagation) event.stopPropagation();
  }

  function forwardRendererMove(event) {
    if (
      eventTargetsUsgsDomMarker(event) ||
      eventTargetsMapUi(event)
    ) {
      setRendererCursor(false);
      return;
    }
    if (rendererInteractionSuppressed()) {
      clearRendererHover(event);
      return;
    }
    if (
      !pointRenderer ||
      !pointRenderer._drawFirst ||
      !map.hasLayer(pointRenderer) ||
      typeof pointRenderer._onMouseMove !== 'function'
    ) {
      setRendererCursor(false);
      return;
    }

    pointRenderer._onMouseMove(event);
    rendererForwardedMoveCount += 1;
    var hit = pointRenderer._hoveredLayer;
    setRendererCursor(!!hit);
    if (hit) {
      rendererHitCount += 1;
      stopAtWell(event);
    }
  }

  function forwardRendererClick(event) {
    if (
      eventTargetsUsgsDomMarker(event) ||
      eventTargetsMapUi(event) ||
      rendererInteractionSuppressed()
    ) return;
    if (
      !pointRenderer ||
      typeof pointRenderer._onClick !== 'function' ||
      !rendererHit(event)
    ) return;

    pointRenderer._onClick(event);
    rendererForwardedClickCount += 1;
    rendererHitCount += 1;
    stopAtWell(event);
  }

  function installRendererForwarding() {
    var container = mapContainer();
    if (!container || !container.addEventListener) return;
    [
      {eventName: 'mousemove', handler: forwardRendererMove},
      {eventName: 'click', handler: forwardRendererClick},
      {
        eventName: 'mouseout',
        handler: function(event) {
          if (
            !event.relatedTarget ||
            !container.contains ||
            !container.contains(event.relatedTarget)
          ) clearRendererHover(event);
        }
      }
    ].forEach(function(record) {
      container.addEventListener(
        record.eventName,
        record.handler,
        true
      );
      containerListenerRecords.push({
        target: container,
        eventName: record.eventName,
        handler: record.handler
      });
    });
  }

  function latestWl(index) {
    return num(valueAt('gwpop_latest_wl', index));
  }

  function wellDepth(index) {
    return num(valueAt('gwpop_well_depth', index));
  }

  function elevFt(index) {
    return num(valueAt('gwpop_elev_ft', index));
  }

  function inOps(index) {
    return (
      bool(valueAt('well_recent_feed_ring', index)) ||
      bool(valueAt('gwpop_in_ops', index))
    );
  }

  function onBlm(index) {
    return (
      bool(valueAt('on_blm_ca', index)) ||
      bool(valueAt('on_blm', index)) ||
      bool(valueAt('gw_on_blm_ca', index))
    );
  }

  function distMi(index) {
    var value = num(valueAt('dist_to_blm_mi', index));
    if (value !== null) return value;
    value = num(valueAt('distance_to_blm_mi', index));
    if (value !== null) return value;
    return num(valueAt('gw_dist_to_blm_mi', index));
  }

  function firstYear(index, fields) {
    for (var i = 0; i < fields.length; i++) {
      var raw = valueAt(fields[i], index);
      if (raw === null || raw === undefined) continue;
      var match = String(raw).trim().match(/((18|19|20)\d{2})(?:[-\/](\d{1,2}))?/);
      if (!match) continue;
      var year = Number(match[1]);
      var month = match[3] == null ? null : Number(match[3]);
      if (month !== null && !isNaN(month) && month >= 10) return year + 1;
      return year;
    }
    return null;
  }

  function porYears(index) {
    var start = firstYear(index, ['gwpop_start_date', 'start_date', 'begin_date']);
    var end = firstYear(index, ['gwpop_end_date', 'end_date']);
    if (start === null || end === null) return null;
    return end - start + 1;
  }

  function latestAgeDays(index) {
    var raw = valueAt('gwpop_latest_date', index);
    if (!has(raw)) return null;
    var date = new Date(String(raw));
    if (isNaN(date.getTime())) return null;
    return Math.floor((Date.now() - date.getTime()) / 86400000);
  }

  function defaultFilters() {
    return {
      opsOnly: false,
      nestedOnly: false,
      wlMode: 'all',
      wlAgeMax: null,
      dtwMin: null,
      dtwMax: null,
      wellDepthMin: null,
      wellDepthMax: null,
      minPor: null,
      startMax: null,
      endMin: null,
      blmMode: 'any',
      blmMax: null,
      elevMin: null,
      elevMax: null
    };
  }

  function normalizedFilters(filters) {
    var source = filters || {};
    var out = defaultFilters();
    Object.keys(out).forEach(function(key) {
      if (source[key] !== undefined) out[key] = source[key];
    });
    return out;
  }

  function filtersAreDefault(filters) {
    var baseline = defaultFilters();
    return Object.keys(baseline).every(function(key) {
      return filters[key] === baseline[key];
    });
  }

  function recordPasses(index, filters) {
    var wl = latestWl(index);
    var depth = wellDepth(index);
    var elevation = elevFt(index);
    var age = latestAgeDays(index);
    var periodYears = porYears(index);
    var startYear = firstYear(
      index,
      ['gwpop_start_date', 'start_date', 'begin_date']
    );
    var endYear = firstYear(index, ['gwpop_end_date', 'end_date']);
    var distance = distMi(index);
    var nestedCount = Number(valueAt('gwpop_nested_n', index) || 1);
    var nested = bool(valueAt('well_is_nested', index)) || nestedCount > 1;

    if (filters.opsOnly && !inOps(index)) return false;
    if (filters.nestedOnly && !nested) return false;
    if (filters.wlMode === 'has' && wl === null) return false;
    if (filters.wlMode === 'none' && wl !== null) return false;
    if (filters.wlMode === 'artesian' && !(wl !== null && wl < 0)) return false;
    if (
      filters.wlAgeMax != null &&
      (age === null || age > Number(filters.wlAgeMax))
    ) return false;
    if (
      filters.dtwMin != null &&
      (wl === null || wl < Number(filters.dtwMin))
    ) return false;
    if (
      filters.dtwMax != null &&
      (wl === null || wl > Number(filters.dtwMax))
    ) return false;
    if (
      filters.wellDepthMin != null &&
      (depth === null || depth < Number(filters.wellDepthMin))
    ) return false;
    if (
      filters.wellDepthMax != null &&
      (depth === null || depth > Number(filters.wellDepthMax))
    ) return false;
    if (
      filters.minPor != null &&
      (periodYears === null || periodYears < Number(filters.minPor))
    ) return false;
    if (
      filters.startMax != null &&
      (startYear === null || startYear > Number(filters.startMax))
    ) return false;
    if (
      filters.endMin != null &&
      (endYear === null || endYear < Number(filters.endMin))
    ) return false;
    if (filters.blmMode === 'on' && !onBlm(index)) return false;
    if (
      filters.blmMode === 'distance' &&
      (distance === null || distance > Number(filters.blmMax))
    ) return false;
    if (
      filters.elevMin != null &&
      (elevation === null || elevation < Number(filters.elevMin))
    ) return false;
    if (
      filters.elevMax != null &&
      (elevation === null || elevation > Number(filters.elevMax))
    ) return false;
    return true;
  }

  function norm(text) {
    return String(text == null ? '' : text)
      .replace(/&amp;/g, '&')
      .replace(/[–—]/g, '-')
      .toLowerCase()
      .replace(/points\s*-\s*/g, '')
      .replace(/monitoring sites\s*\/\s*records\s*-\s*/g, '')
      .replace(/\s*\([^)]*\)\s*$/g, '')
      .replace(/\s+/g, ' ')
      .trim();
  }

  function isUsgsWellText(text) {
    var normalized = norm(text);
    return (
      normalized.indexOf('usgs monitoring wells') >= 0 ||
      normalized.indexOf('usgs wells') >= 0
    );
  }

  function safeControlScan() {
    if (typeof document === 'undefined' || !document.querySelectorAll) return null;
    var labels = document.querySelectorAll(
      '.leaflet-control-layers-overlays label'
    );
    for (var i = 0; i < labels.length; i++) {
      var label = labels[i];
      var text = label.textContent || label.innerText || '';
      if (!isUsgsWellText(text)) continue;
      var input = label.querySelector ?
        label.querySelector('input[type="checkbox"]') :
        null;
      if (input) return !!input.checked;
    }
    return null;
  }

  function eventMatches(event) {
    if (!event) return false;
    if (isUsgsWellText(event.name)) return true;
    if (event.layer && event.layer.options) {
      var bits = [
        event.layer.options.group,
        event.layer.options.name,
        event.layer.options.layerId
      ].join(' ');
      return isUsgsWellText(bits);
    }
    return false;
  }

  function now() {
    return window.performance && performance.now ?
      performance.now() :
      Date.now();
  }

  function profileEnabledFromPage() {
    if (window.BRIM_ENABLE_USGS_GW_PROFILE === true) return true;
    try {
      return new URLSearchParams(window.location.search).get('brimProfile') === '1';
    } catch (queryErr) {
      return false;
    }
  }

  var profileEnabled = profileEnabledFromPage();
  var profileEvents = [];
  var longTasks = [];
  var scenarioName = '';
  var scenarioStart = 0;
  var activePhase = '';
  var phaseStarts = {};
  var longTaskObserver = null;
  var tileLayerRecords = [];
  var profilerInstalled = false;

  function profileRecord(type, details) {
    if (!profileEnabled) return;
    profileEvents.push({
      time: now(),
      type: type,
      phase: activePhase || '',
      details: details || {}
    });
  }

  function profilePhaseStart(name, details) {
    activePhase = name;
    phaseStarts[name] = now();
    profileRecord('phase-start', {
      name: name,
      details: details || {}
    });
  }

  function profilePhaseEnd(name, details) {
    var start = phaseStarts[name];
    profileRecord('phase-end', {
      name: name,
      durationMs: start == null ? null : now() - start,
      details: details || {}
    });
    delete phaseStarts[name];
    if (activePhase === name) activePhase = '';
  }

  function memorySnapshot() {
    var memory = window.performance && window.performance.memory ?
      window.performance.memory :
      null;
    return memory ? {
      usedJSHeapSize: memory.usedJSHeapSize,
      totalJSHeapSize: memory.totalJSHeapSize,
      jsHeapSizeLimit: memory.jsHeapSizeLimit
    } : null;
  }

  function domSnapshot(label) {
    var container = map && map.getContainer ? map.getContainer() : null;
    var layerCount = 0;
    try { map.eachLayer(function() { layerCount += 1; }); } catch (layerScanErr) {}
    var snapshot = {
      label: label || '',
      time: now(),
      domElements: document.getElementsByTagName('*').length,
      mapDomElements: container ? container.getElementsByTagName('*').length : 0,
      markerIcons: container ?
        container.querySelectorAll('.leaflet-marker-icon').length :
        0,
      clusterIcons: container ?
        container.querySelectorAll('.pt-usgs-gw-grid-cluster').length :
        0,
      nestedIcons: container ?
        container.querySelectorAll('.pt-usgs-gw-virtual-nested-marker').length :
        0,
      svgPaths: container ?
        container.querySelectorAll('svg path').length :
        0,
      canvases: container ?
        container.querySelectorAll('canvas').length :
        0,
      mapLayerCount: layerCount,
      groundwater: controllerStats(),
      memory: memorySnapshot()
    };
    profileRecord('snapshot', snapshot);
    return snapshot;
  }

  function summarizeLongTasks(startTime) {
    var relevant = longTasks.filter(function(task) {
      return task.startTime >= Number(startTime || 0);
    });
    var maximum = 0;
    var over50 = 0;
    var over200 = 0;
    var byPhase = {};
    relevant.forEach(function(task) {
      maximum = Math.max(maximum, task.duration);
      if (task.duration >= 50) over50 += 1;
      if (task.duration >= 200) over200 += 1;
      var key = task.phase || 'unattributed';
      if (!byPhase[key]) byPhase[key] = {count: 0, maxMs: 0, totalMs: 0};
      byPhase[key].count += 1;
      byPhase[key].maxMs = Math.max(byPhase[key].maxMs, task.duration);
      byPhase[key].totalMs += task.duration;
    });
    return {
      total: relevant.length,
      maximumMs: maximum,
      over50ms: over50,
      over200ms: over200,
      byPhase: byPhase
    };
  }

  function profileReport() {
    return {
      enabled: profileEnabled,
      scenario: scenarioName,
      scenarioStart: scenarioStart,
      scenarioDurationMs: scenarioStart > 0 ? now() - scenarioStart : null,
      metadata: metadata,
      stats: controllerStats(),
      longTasks: summarizeLongTasks(scenarioStart),
      events: profileEvents.slice(),
      memory: memorySnapshot()
    };
  }

  function downloadProfileJson() {
    var report = profileReport();
    var blob = new Blob(
      [JSON.stringify(report, null, 2)],
      {type: 'application/json'}
    );
    var url = URL.createObjectURL(blob);
    var anchor = document.createElement('a');
    anchor.href = url;
    anchor.download = 'brim-usgs-groundwater-profile-' +
      (scenarioName || 'session').replace(/[^a-z0-9_-]+/gi, '-') +
      '.json';
    document.body.appendChild(anchor);
    anchor.click();
    anchor.remove();
    setTimeout(function() { URL.revokeObjectURL(url); }, 0);
  }

  function startScenario(name) {
    scenarioName = String(name || 'manual');
    scenarioStart = now();
    profileEvents = [];
    longTasks = [];
    profileRecord('scenario-start', {name: scenarioName});
    return domSnapshot('scenario-start');
  }

  function endScenario() {
    domSnapshot('scenario-end');
    profileRecord('scenario-end', {name: scenarioName});
    return profileReport();
  }

  function hookTileLayer(layer) {
    if (!profileEnabled || !layer || typeof layer.on !== 'function') return;
    if (!(L.TileLayer && layer instanceof L.TileLayer)) return;
    if (layer.__ptUsgsGwProfileHooked) return;
    layer.__ptUsgsGwProfileHooked = true;
    var loadingAt = null;
    var loadingHandler = function() {
      loadingAt = now();
      profileRecord('basemap-loading', {
        url: layer._url || '',
        zoom: map.getZoom()
      });
    };
    var loadHandler = function() {
      profileRecord('basemap-settled', {
        url: layer._url || '',
        zoom: map.getZoom(),
        durationMs: loadingAt == null ? null : now() - loadingAt
      });
      loadingAt = null;
    };
    layer.on('loading', loadingHandler);
    layer.on('load', loadHandler);
    tileLayerRecords.push({
      layer: layer,
      loadingHandler: loadingHandler,
      loadHandler: loadHandler
    });
  }

  function installProfiler() {
    if (!profileEnabled || profilerInstalled) return;
    profilerInstalled = true;
    try {
      if (
        window.PerformanceObserver &&
        PerformanceObserver.supportedEntryTypes &&
        PerformanceObserver.supportedEntryTypes.indexOf('longtask') >= 0
      ) {
        longTaskObserver = new PerformanceObserver(function(list) {
          if (!profileEnabled) return;
          list.getEntries().forEach(function(entry) {
            longTasks.push({
              startTime: entry.startTime,
              duration: entry.duration,
              phase: activePhase || 'unattributed'
            });
          });
        });
        longTaskObserver.observe({entryTypes: ['longtask']});
      }
    } catch (observerErr) {
      profileRecord('profile-warning', {
        message: 'PerformanceObserver longtask unavailable',
        error: String(observerErr)
      });
    }
    try { map.eachLayer(hookTileLayer); } catch (tileScanErr) {}
  }

  function notifyLegendStatus() {
    try { if (map && map.fire) map.fire('pt:usgsgwlocalstatus'); } catch (eventErr) {}
    try {
      if (
        window.BRIM_USGS_GW_LOCAL_REFRESH_LEGEND &&
        typeof window.BRIM_USGS_GW_LOCAL_REFRESH_LEGEND === 'function'
      ) {
        window.BRIM_USGS_GW_LOCAL_REFRESH_LEGEND();
      }
    } catch (refreshErr) {}
  }

  function setRenderStatus(text, busy, clearAfterMs) {
    renderStatusText = text || '';
    renderStatusBusy = !!busy;
    loading = !!busy;
    if (statusClearTimer) {
      clearTimeout(statusClearTimer);
      statusClearTimer = null;
    }
    notifyLegendStatus();
    var delay = Number(clearAfterMs || 0);
    if (delay > 0) {
      var expected = renderStatusText;
      statusClearTimer = setTimeout(function() {
        if (renderStatusText !== expected || destroyed) return;
        renderStatusText = '';
        renderStatusBusy = false;
        loading = false;
        notifyLegendStatus();
      }, delay);
    }
  }

  function requestFrame(callback) {
    var raf = window.requestAnimationFrame || function(fn) {
      return window.setTimeout(fn, 16);
    };
    scheduledFrame = raf(function(timestamp) {
      scheduledFrame = null;
      callback(timestamp);
    });
  }

  function cancelScheduled() {
    if (scheduledFrame !== null) {
      if (window.cancelAnimationFrame) {
        window.cancelAnimationFrame(scheduledFrame);
      } else {
        clearTimeout(scheduledFrame);
      }
      scheduledFrame = null;
    }
    if (scheduledTimer !== null) {
      clearTimeout(scheduledTimer);
      scheduledTimer = null;
    }
  }

  function nextGeneration(reason) {
    generation += 1;
    cancelScheduled();
    profileRecord('generation', {
      generation: generation,
      reason: reason || ''
    });
    return generation;
  }

  function tokenCurrent(token) {
    return !destroyed && token === generation;
  }

  function closeInteractiveState() {
    clearRendererHover();
    try { map.closePopup(); } catch (popupErr) {}
    try {
      var container = map.getContainer();
      var tooltip = container.querySelector('.leaflet-tooltip.pt-usgs-gw-local-tooltip');
      if (tooltip && tooltip.parentNode) tooltip.parentNode.removeChild(tooltip);
    } catch (tooltipErr) {}
  }

  function detachPointRenderer() {
    clearRendererHover();
    if (
      !pointRenderer ||
      !map.hasLayer ||
      !map.removeLayer ||
      !map.hasLayer(pointRenderer)
    ) return;
    try { map.removeLayer(pointRenderer); } catch (rendererRemoveError) {}
  }

  function disposeRootLater(root) {
    if (!root) return;
    deferredCleanupCount += 1;
    var cleanup = function() {
      markRootMounted(root, false);
      try { if (root.clearLayers) root.clearLayers(); } catch (clearErr) {}
      deferredCleanupCount = Math.max(0, deferredCleanupCount - 1);
    };
    if (window.requestIdleCallback) {
      window.requestIdleCallback(cleanup, {timeout: 600});
    } else {
      setTimeout(cleanup, 40);
    }
  }

  function detachCurrent(preserveComplete) {
    closeInteractiveState();
    if (!currentRoot) {
      detachPointRenderer();
      return;
    }
    markRootMounted(currentRoot, false);
    try {
      if (map.hasLayer(currentRoot)) map.removeLayer(currentRoot);
    } catch (removeErr) {}
    detachPointRenderer();
    if (!preserveComplete || !currentRootComplete) {
      var old = currentRoot;
      currentRoot = null;
      currentRootComplete = false;
      currentRenderKey = '';
      disposeRootLater(old);
    }
  }

  function hardClearDisplay(dropDisplayCache) {
    nextGeneration('clear');
    setRenderStatus('', false);
    detachCurrent(!dropDisplayCache);
    if (dropDisplayCache && currentRoot) {
      var old = currentRoot;
      currentRoot = null;
      currentRootComplete = false;
      currentRenderKey = '';
      disposeRootLater(old);
    }
    lastDrawn = currentRoot && currentRootComplete ? lastDrawn : 0;
    lastVisibleSites = currentRoot && currentRootComplete ? lastVisibleSites : 0;
    lastVisibleLocations =
      currentRoot && currentRootComplete ? lastVisibleLocations : 0;
    lastClusterObjects =
      currentRoot && currentRootComplete ? lastClusterObjects : 0;
    lastExactObjects = currentRoot && currentRootComplete ? lastExactObjects : 0;
    profileRecord('display-cleared', {
      dropDisplayCache: !!dropDisplayCache
    });
  }

  function spatialKey(lng, lat) {
    return (
      Math.floor(Number(lng) / spatialCellDegrees) +
      ':' +
      Math.floor(Number(lat) / spatialCellDegrees)
    );
  }

  function ensureSpatialIndex(token, done) {
    if (gridIndex) {
      done();
      return;
    }

    profilePhaseStart('index-build', {locations: locationCount});
    setRenderStatus('Indexing groundwater locations…', true);
    var work = {};
    var index = 0;

    function step() {
      if (!tokenCurrent(token)) {
        profilePhaseEnd('index-build', {cancelled: true});
        return;
      }
      var started = now();
      while (index < locationCount && now() - started < frameBudgetMs) {
        var lng = locationLng(index);
        var lat = locationLat(index);
        var key = spatialKey(lng, lat);
        if (!work[key]) work[key] = [];
        work[key].push(index);
        index += 1;
      }
      if (index < locationCount) {
        requestFrame(step);
        return;
      }
      gridIndex = work;
      profilePhaseEnd('index-build', {
        cancelled: false,
        cells: Object.keys(work).length
      });
      done();
    }

    requestFrame(step);
  }

  function buildFilterMask(token, done) {
    var signature = JSON.stringify(currentFilters);
    if (
      filterMaskReady &&
      signature === filterSignature
    ) {
      done();
      return;
    }

    filterSignature = signature;
    if (filtersAreDefault(currentFilters)) {
      filterMask = null;
      filterMaskReady = true;
      lastFiltered = siteCount;
      done();
      return;
    }

    profilePhaseStart('filter-index', {sites: siteCount});
    setRenderStatus('Filtering groundwater sites…', true);
    var mask = new Uint8Array(siteCount);
    var index = 0;
    var matched = 0;

    function step() {
      if (!tokenCurrent(token)) {
        profilePhaseEnd('filter-index', {cancelled: true});
        return;
      }
      var started = now();
      while (index < siteCount && now() - started < frameBudgetMs) {
        if (recordPasses(index, currentFilters)) {
          mask[index] = 1;
          matched += 1;
        }
        index += 1;
      }
      if (index < siteCount) {
        requestFrame(step);
        return;
      }
      filterMask = mask;
      filterMaskReady = true;
      lastFiltered = matched;
      profilePhaseEnd('filter-index', {
        cancelled: false,
        matched: matched
      });
      done();
    }

    requestFrame(step);
  }

  function bufferedBounds() {
    var bounds = map.getBounds();
    var zoom = Math.round(map.getZoom());
    var ratio = zoom >= clusterToExactZoom ? 0.15 : (zoom >= 8 ? 0.18 : 0.10);
    var west = bounds.getWest();
    var east = bounds.getEast();
    var south = bounds.getSouth();
    var north = bounds.getNorth();
    var lngPad = Math.max(0.04, (east - west) * ratio);
    var latPad = Math.max(0.04, (north - south) * ratio);
    return {
      west: Math.max(-180, west - lngPad),
      east: Math.min(180, east + lngPad),
      south: Math.max(-90, south - latPad),
      north: Math.min(90, north + latPad)
    };
  }

  function viewportKey() {
    var bounds = map.getBounds();
    function round(value) { return Number(value).toFixed(5); }
    return [
      Math.round(map.getZoom()),
      round(bounds.getWest()),
      round(bounds.getSouth()),
      round(bounds.getEast()),
      round(bounds.getNorth()),
      filterVersion
    ].join('|');
  }

  function queryLocations() {
    var bounds = bufferedBounds();
    var minX = Math.floor(bounds.west / spatialCellDegrees);
    var maxX = Math.floor(bounds.east / spatialCellDegrees);
    var minY = Math.floor(bounds.south / spatialCellDegrees);
    var maxY = Math.floor(bounds.north / spatialCellDegrees);
    var result = [];

    for (var y = minY; y <= maxY; y++) {
      for (var xIndex = minX; xIndex <= maxX; xIndex++) {
        var bucket = gridIndex[xIndex + ':' + y];
        if (!bucket) continue;
        for (var i = 0; i < bucket.length; i++) {
          var locationIndex = bucket[i];
          var lng = locationLng(locationIndex);
          var lat = locationLat(locationIndex);
          if (
            lng >= bounds.west &&
            lng <= bounds.east &&
            lat >= bounds.south &&
            lat <= bounds.north
          ) result.push(locationIndex);
        }
      }
    }
    return result;
  }

  function locationRange(locationIndex) {
    return {
      start: Number(locationValue('site_start', locationIndex) || 0),
      count: Number(locationValue('site_count', locationIndex) || 0)
    };
  }

  function matchingCountForLocation(locationIndex) {
    var range = locationRange(locationIndex);
    if (!filterMask) return range.count;
    var matched = 0;
    for (var i = range.start; i < range.start + range.count; i++) {
      if (filterMask[i]) matched += 1;
    }
    return matched;
  }

  function matchingIndicesForLocation(locationIndex) {
    var range = locationRange(locationIndex);
    var result = [];
    for (var i = range.start; i < range.start + range.count; i++) {
      if (!filterMask || filterMask[i]) result.push(i);
    }
    return result;
  }

  function clusterRadiusForZoom(zoom) {
    if (zoom <= 6) return 175;
    if (zoom <= 8) return 145;
    if (zoom <= 9) return 115;
    if (zoom <= 10) return 75;
    return 35;
  }

  function buildDescriptors(token, locationIds, done) {
    var zoom = Math.round(map.getZoom());
    var exactMode = zoom >= clusterToExactZoom;
    var descriptors = [];
    var position = 0;
    var visibleSites = 0;
    var visibleLocations = 0;
    var aggregates = exactMode ? null : {};
    var cellSize = clusterRadiusForZoom(zoom);

    profilePhaseStart('viewport-build', {
      zoom: zoom,
      queriedLocations: locationIds.length,
      exactMode: exactMode
    });
    setRenderStatus(
      exactMode ?
        'Preparing visible groundwater locations…' :
        'Aggregating visible groundwater locations…',
      true
    );

    function buildStep() {
      if (!tokenCurrent(token)) {
        profilePhaseEnd('viewport-build', {cancelled: true});
        return;
      }

      var started = now();
      while (
        position < locationIds.length &&
        now() - started < frameBudgetMs
      ) {
        var locationIndex = locationIds[position];
        position += 1;
        var matched = matchingCountForLocation(locationIndex);
        if (matched <= 0) continue;

        visibleSites += matched;
        visibleLocations += 1;

        if (exactMode) {
          descriptors.push({
            type: 'exact',
            locationIndex: locationIndex
          });
          continue;
        }

        var lat = locationLat(locationIndex);
        var lng = locationLng(locationIndex);
        var pixel = map.project([lat, lng], zoom);
        var key = (
          Math.floor(pixel.x / cellSize) +
          ':' +
          Math.floor(pixel.y / cellSize)
        );
        var aggregate = aggregates[key];
        if (!aggregate) {
          aggregate = aggregates[key] = {
            type: 'cluster',
            siteCount: 0,
            locationCount: 0,
            sumX: 0,
            sumY: 0,
            west: lng,
            east: lng,
            south: lat,
            north: lat,
            onlyLocation: locationIndex
          };
        }
        aggregate.siteCount += matched;
        aggregate.locationCount += 1;
        aggregate.sumX += pixel.x;
        aggregate.sumY += pixel.y;
        aggregate.west = Math.min(aggregate.west, lng);
        aggregate.east = Math.max(aggregate.east, lng);
        aggregate.south = Math.min(aggregate.south, lat);
        aggregate.north = Math.max(aggregate.north, lat);
        if (aggregate.locationCount > 1) aggregate.onlyLocation = null;
      }

      if (position < locationIds.length) {
        requestFrame(buildStep);
        return;
      }

      if (!exactMode) {
        Object.keys(aggregates).forEach(function(key) {
          var aggregate = aggregates[key];
          if (
            aggregate.locationCount === 1 &&
            aggregate.onlyLocation !== null
          ) {
            descriptors.push({
              type: 'exact',
              locationIndex: aggregate.onlyLocation
            });
            return;
          }
          var centerPixel = L.point(
            aggregate.sumX / aggregate.locationCount,
            aggregate.sumY / aggregate.locationCount
          );
          var center = map.unproject(centerPixel, zoom);
          aggregate.lat = center.lat;
          aggregate.lng = center.lng;
          descriptors.push(aggregate);
        });
      }

      profilePhaseEnd('viewport-build', {
        cancelled: false,
        descriptors: descriptors.length,
        visibleSites: visibleSites,
        visibleLocations: visibleLocations
      });
      done({
        descriptors: descriptors,
        visibleSites: visibleSites,
        visibleLocations: visibleLocations,
        exactMode: exactMode
      });
    }

    requestFrame(buildStep);
  }

  function makePopup(index) {
    var parts = [];
    var siteId = has(valueAt('gwpop_site_no', index)) ?
      valueAt('gwpop_site_no', index) :
      valueAt('site_no', index);
    var nestedCount = parseInt(valueAt('gwpop_nested_n', index), 10);
    var wl = latestWl(index);

    parts.push('<b>' + esc(siteId) + '</b> – ' + esc(valueAt('gwpop_name', index)));
    parts.push('<b>Elevation:</b> ' + esc(valueAt('gwpop_elev_ft', index)) + ' ft');
    parts.push('<b>Type:</b> ' + esc(valueAt('gwpop_site_type', index)));
    parts.push('<b>USGS source status:</b> ' + esc(valueAt('gwpop_status', index)));
    parts.push('<b>Record count:</b> ' + esc(valueAt('gwpop_count_nu', index)));
    if (isFinite(nestedCount) && nestedCount > 1) {
      parts.push(
        '<b>Co-located/nested group:</b> ' +
        nestedCount +
        ' USGS well records share this mapped coordinate.'
      );
    }
    parts.push(
      '<b>Recent feed:</b> ' +
      (
        inOps(index) ?
          'included in Ops Live groundwater layer; pink ring indicates Ops Live membership.' :
          'not currently included in Ops Live groundwater layer.'
      )
    );
    if (wl !== null) {
      var sourceNote = bool(valueAt('gwpop_synced_ops', index)) ?
        'Source: Ops Live groundwater feed (USGS field measurements).' :
        'Source: static cached USGS lookup' +
          (
            has(valueAt('gwpop_latest_run', index)) ?
              '; lookup run ' + esc(valueAt('gwpop_latest_run', index)) :
              ''
          ) +
          '.';
      parts.push(
        '<b>Most recent groundwater level:</b> ' +
        fmtNum(wl, 1) +
        ' ft bgs' +
        (
          has(valueAt('gwpop_latest_date', index)) ?
            ' on ' + esc(valueAt('gwpop_latest_date', index)) :
            ''
        ) +
        '<br/><span style="font-size:11px;color:#555;">' +
        sourceNote +
        '</span>'
      );
    }
    parts.push('<b>Well depth:</b> ' + esc(valueAt('gwpop_well_depth', index)) + ' ft');
    parts.push('<b>Hole depth:</b> ' + esc(valueAt('gwpop_hole_depth', index)) + ' ft');
    parts.push('<b>Aquifer:</b> ' + esc(valueAt('gwpop_aquifer', index)));
    parts.push('<b>Aquifer type:</b> ' + esc(valueAt('gwpop_aquifer_type', index)));
    parts.push('<b>National aquifer:</b> ' + esc(valueAt('gwpop_nat_aqfr', index)));
    parts.push(
      '<b>Period:</b> ' +
      esc(valueAt('gwpop_start_date', index)) +
      ' to ' +
      esc(valueAt('gwpop_end_date', index))
    );
    parts.push('<b>Common data:</b> ' + esc(valueAt('gwpop_common_data', index)));
    parts.push(
      '<span style="font-size:11px;color:#555;">' +
      'Additional parameters may be available from USGS.</span>'
    );
    if (has(siteId)) {
      parts.push(
        '<a href="https://waterdata.usgs.gov/monitoring-location/' +
        encodeURIComponent(String(siteId)) +
        '" target="_blank">USGS Site</a>'
      );
    }
    return parts.join('<br/>');
  }

  function tooltipHtml(index) {
    var text = has(valueAt('hover_text', index)) ?
      String(valueAt('hover_text', index)) :
      'USGS ' + (valueAt('site_no', index) || 'well');
    return escLoose(text).replace(/\n/g, '<br/>');
  }

  function sortedIndices(indices) {
    return indices.slice().sort(function(a, b) {
      var depthA = wellDepth(a);
      var depthB = wellDepth(b);
      if (depthA === null && depthB !== null) return 1;
      if (depthB === null && depthA !== null) return -1;
      if (depthA !== null && depthB !== null && depthA !== depthB) {
        return depthA - depthB;
      }
      return String(valueAt('site_no', a)).localeCompare(
        String(valueAt('site_no', b))
      );
    });
  }

  function makeNestedTooltip(indices) {
    var ordered = sortedIndices(indices);
    var lines = [
      'Nested / co-located USGS wells (' + ordered.length + ')'
    ];
    var shown = Math.min(ordered.length, 5);
    for (var i = 0; i < shown; i++) {
      var index = ordered[i];
      var depth = wellDepth(index);
      var wl = latestWl(index);
      lines.push(
        (
          depth === null ?
            'well depth NA' :
            fmt(Math.round(depth)) + ' ft well'
        ) +
        ': ' +
        (
          wl === null ?
            'no cached WL' :
            fmtNum(wl, 1) + ' ft bgs'
        ) +
        (
          has(valueAt('gwpop_latest_date', index)) ?
            ' (' + valueAt('gwpop_latest_date', index) + ')' :
            ''
        )
      );
    }
    if (ordered.length > shown) {
      lines.push('+' + (ordered.length - shown) + ' more; click for details');
    }
    return escLoose(lines.join('\n')).replace(/\n/g, '<br/>');
  }

  function makeNestedPopup(indices) {
    var ordered = sortedIndices(indices);
    var rows = ordered.map(function(index) {
      var siteId = valueAt('site_no', index);
      var siteCell = esc(siteId);
      if (has(siteId)) {
        siteCell =
          '<a href="https://waterdata.usgs.gov/monitoring-location/' +
          encodeURIComponent(String(siteId)) +
          '" target="_blank" rel="noopener">' +
          esc(siteId) +
          '</a>';
      }
      return (
        '<tr><td>' +
        siteCell +
        '</td><td>' +
        (
          wellDepth(index) === null ?
            'NA' :
            fmt(Math.round(wellDepth(index))) + ' ft'
        ) +
        '</td><td>' +
        (
          latestWl(index) === null ?
            'NA' :
            fmtNum(latestWl(index), 1) + ' ft bgs'
        ) +
        '</td><td>' +
        esc(valueAt('gwpop_latest_date', index)) +
        '</td></tr>'
      );
    }).join('');
    return (
      '<div style="font:12px/1.35 Arial,Helvetica,sans-serif;max-width:420px;">' +
      '<div style="font-weight:700;font-size:13px;margin-bottom:3px;">' +
      'Co-located / nested USGS groundwater wells</div>' +
      '<div style="color:#555;font-size:11px;margin-bottom:5px;">' +
      ordered.length +
      ' catalog records at this mapped coordinate.</div>' +
      '<table style="border-collapse:collapse;width:100%;font-size:11.5px;">' +
      '<thead><tr><th style="text-align:left;border-bottom:1px solid #ddd;">USGS</th>' +
      '<th style="text-align:left;border-bottom:1px solid #ddd;">Well depth</th>' +
      '<th style="text-align:left;border-bottom:1px solid #ddd;">Cached WL</th>' +
      '<th style="text-align:left;border-bottom:1px solid #ddd;">Date</th></tr></thead>' +
      '<tbody>' + rows + '</tbody></table>' +
      '<div style="color:#555;font-size:11px;margin-top:5px;">' +
      'Each USGS ID opens its individual source record. Grouped symbol added ' +
      'by BRIM because multiple records share coordinates.' +
      '</div></div>'
    );
  }

  function locationDiagnosticId(locationIndex) {
    return (
      Number(locationLng(locationIndex)).toFixed(7) +
      '_' +
      Number(locationLat(locationIndex)).toFixed(7)
    );
  }

  function markNestedMounted(layer, mounted, token) {
    var details = layer && layer._ptUsgsGwNested;
    if (!details) return;
    details.mounted = !!mounted;
    if (mounted) details.mountedGeneration = Number(token || generation);
    if (layer._icon && layer._icon.setAttribute) {
      layer._icon.setAttribute(
        'data-pt-usgs-gw-location-id',
        details.locationId
      );
      layer._icon.setAttribute(
        'data-pt-usgs-gw-generation',
        String(details.mountedGeneration)
      );
    }
  }

  function markRootMounted(root, mounted, token) {
    if (!root || typeof root.eachLayer !== 'function') return;
    try {
      root.eachLayer(function(layer) {
        markNestedMounted(layer, mounted, token);
      });
    } catch (markErr) {}
  }

  function currentNestedLayers() {
    var result = [];
    if (!currentRoot || typeof currentRoot.eachLayer !== 'function') return result;
    try {
      currentRoot.eachLayer(function(layer) {
        if (layer && layer._ptUsgsGwNested) result.push(layer);
      });
    } catch (scanErr) {}
    return result;
  }

  function handlerCount(layer, eventName) {
    var handlers = layer && layer._events ? layer._events[eventName] : null;
    if (!handlers) return 0;
    if (Array.isArray(handlers)) return handlers.length;
    return 1;
  }

  function computedValue(element, propertyName) {
    if (!element) return null;
    try {
      if (window.getComputedStyle) {
        return window.getComputedStyle(element)[propertyName] || null;
      }
    } catch (styleErr) {}
    return element.style ? element.style[propertyName] || null : null;
  }

  function elementDescription(element) {
    if (!element) return null;
    return {
      tag: String(element.tagName || '').toLowerCase(),
      id: element.id || '',
      className: typeof element.className === 'string' ?
        element.className :
        (
          element.className && element.className.baseVal ?
            element.className.baseVal :
            ''
        )
    };
  }

  function iconContainsPoint(icon, x, y) {
    if (!icon || !icon.getBoundingClientRect) return false;
    var rect = icon.getBoundingClientRect();
    return (
      x >= rect.left &&
      x <= rect.right &&
      y >= rect.top &&
      y <= rect.bottom
    );
  }

  function inspectNestedAtCursor(x, y) {
    if (isFinite(Number(x)) && isFinite(Number(y))) {
      debugLastPointer = {x: Number(x), y: Number(y)};
    }
    var pointer = debugLastPointer;
    var layers = currentNestedLayers();
    var target = null;
    if (pointer) {
      for (var i = layers.length - 1; i >= 0; i--) {
        if (iconContainsPoint(layers[i]._icon, pointer.x, pointer.y)) {
          target = layers[i];
          break;
        }
      }
    }
    var hit = null;
    if (pointer && document.elementFromPoint) {
      try { hit = document.elementFromPoint(pointer.x, pointer.y); } catch (hitErr) {}
    }
    var pane = map.getPane ? map.getPane(interactivePaneName) : null;
    var base = {
      found: !!target,
      cursor: pointer ? {x: pointer.x, y: pointer.y} : null,
      visibleNestedCount: layers.length,
      generation: generation,
      pane: {
        name: interactivePaneName,
        zIndex: computedValue(pane, 'zIndex'),
        pointerEvents: computedValue(pane, 'pointerEvents'),
        measureSuspended: !!(
          pane &&
          pane.getAttribute &&
          pane.getAttribute('data-pt-measure-suspended') === 'true'
        )
      },
      measureActive: !!map._ptMeasureInteractionActive,
      elementFromPoint: elementDescription(hit),
      map: {
        zoom: map.getZoom(),
        moving: mapMoving,
        zooming: mapZooming,
        rendering: loading || !currentRootComplete,
        layerActive: layerActive,
        renderKey: currentRenderKey
      },
      lastNestedEvent: debugLastNestedEvent
    };
    if (!target) return base;

    var details = target._ptUsgsGwNested;
    var icon = target._icon || null;
    var hitInsideIcon = !!(
      icon &&
      hit &&
      (hit === icon || (icon.contains && icon.contains(hit)))
    );
    base.locationId = details.locationId;
    base.locationIndex = details.locationIndex;
    base.siteCount = details.siteCount;
    base.createdGeneration = details.createdGeneration;
    base.mountedGeneration = details.mountedGeneration;
    base.mounted = details.mounted;
    base.interactionReady = details.interactionReady;
    base.handlers = {
      mouseover: handlerCount(target, 'mouseover'),
      click: handlerCount(target, 'click')
    };
    base.icon = {
      pointerEvents: computedValue(icon, 'pointerEvents'),
      hitByElementFromPoint: hitInsideIcon
    };
    return base;
  }

  function debugPointerHandler(event) {
    debugLastPointer = {
      x: Number(event.clientX),
      y: Number(event.clientY)
    };
  }

  function debugNestedEventHandler(event) {
    var target = event && event.target;
    var icon = target && target.closest ?
      target.closest('.pt-usgs-gw-virtual-nested-divicon') :
      null;
    if (!icon) return;
    debugLastNestedEvent = {
      type: event.type,
      time: now(),
      locationId: icon.getAttribute ?
        icon.getAttribute('data-pt-usgs-gw-location-id') :
        null
    };
  }

  function enableDebug() {
    if (debugEnabled) return true;
    debugEnabled = true;
    document.addEventListener('pointermove', debugPointerHandler, true);
    document.addEventListener('mouseover', debugNestedEventHandler, true);
    document.addEventListener('click', debugNestedEventHandler, true);
    return true;
  }

  function disableDebug() {
    if (!debugEnabled) return false;
    debugEnabled = false;
    document.removeEventListener('pointermove', debugPointerHandler, true);
    document.removeEventListener('mouseover', debugNestedEventHandler, true);
    document.removeEventListener('click', debugNestedEventHandler, true);
    return false;
  }

  function bindLazyTooltip(layer, contentFunction) {
    layer.on('mouseover', function() {
      if (map._ptMeasureInteractionActive) return;
      try {
        if (!layer.getTooltip || !layer.getTooltip()) {
          layer.bindTooltip(contentFunction(), {
            direction: 'auto',
            opacity: 0.9,
            sticky: true,
            className: 'pt-usgs-gw-local-tooltip'
          });
        }
        if (layer.openTooltip) layer.openTooltip();
      } catch (tooltipErr) {}
    });
  }

  function makeNestedIcon(count) {
    var label = String(Math.max(2, Math.min(99, Number(count) || 2)));
    return L.divIcon({
      className: 'pt-usgs-gw-virtual-nested-divicon',
      html:
        '<div class="pt-usgs-gw-virtual-nested-marker">' +
        '<span>' + escLoose(label) + '</span></div>',
      iconSize: L.point(24, 24),
      iconAnchor: L.point(12, 12),
      popupAnchor: L.point(0, -12)
    });
  }

  function makeExactLayer(locationIndex, token) {
    var indices = matchingIndicesForLocation(locationIndex);
    if (!indices.length) return null;

    if (indices.length > 1) {
      var nestedLat = Number(valueAt('pt_lat', indices[0]));
      var nestedLng = Number(valueAt('pt_lng', indices[0]));
      var nested = L.marker([nestedLat, nestedLng], {
        icon: makeNestedIcon(indices.length),
        pane: interactivePaneName,
        keyboard: false,
        interactive: true,
        bubblingMouseEvents: false,
        riseOnHover: true,
        zIndexOffset: 90
      });
      nested._ptUsgsGwNested = {
        locationId: locationDiagnosticId(locationIndex),
        locationIndex: locationIndex,
        siteCount: indices.length,
        createdGeneration: Number(token || generation),
        mountedGeneration: null,
        interactionReady: false,
        mounted: false
      };
      bindLazyTooltip(nested, function() {
        return makeNestedTooltip(indices);
      });
      nested.bindPopup(function() {
        return makeNestedPopup(indices);
      }, {maxWidth: 430});
      nested._ptUsgsGwNested.interactionReady = (
        handlerCount(nested, 'mouseover') > 0 &&
        handlerCount(nested, 'click') > 0
      );
      return nested;
    }

    var index = indices[0];
    var lat = Number(valueAt('pt_lat', index));
    var lng = Number(valueAt('pt_lng', index));
    var fill = cssColor(valueAt('well_fill_col', index), '#F2F2F2');
    var stroke = cssColor(
      has(valueAt('well_stroke_col_display', index)) ?
        valueAt('well_stroke_col_display', index) :
        (
          inOps(index) ?
            '#ff00cc' :
            valueAt('well_stroke_col', index)
        ),
      inOps(index) ? '#ff00cc' : '#4D4D4D'
    );
    var options = {
      pane: displayPaneName,
      radius: ordinaryWellRadius,
      stroke: true,
      color: stroke,
      weight: ordinaryWellStrokeWeight,
      opacity: 0.96,
      fill: true,
      fillColor: fill,
      fillOpacity: 0.86,
      interactive: true,
      bubblingMouseEvents: false
    };
    if (pointRenderer) options.renderer = pointRenderer;

    var marker = L.circleMarker([lat, lng], options);
    bindLazyTooltip(marker, function() {
      return tooltipHtml(index);
    });
    marker.bindPopup(function() {
      return makePopup(index);
    }, {maxWidth: 380});
    return marker;
  }

  function makeClusterIcon(descriptor) {
    var size = descriptor.siteCount >= 1000 ? 44 :
      (descriptor.siteCount >= 100 ? 38 : 33);
    return L.divIcon({
      className: 'pt-usgs-gw-grid-cluster-divicon',
      html:
        '<div class="pt-usgs-gw-grid-cluster" data-site-count="' +
        descriptor.siteCount +
        '"><span>' +
        compactCount(descriptor.siteCount) +
        '</span></div>',
      iconSize: L.point(size, size),
      iconAnchor: L.point(size / 2, size / 2)
    });
  }

  function makeClusterLayer(descriptor) {
    var marker = L.marker([descriptor.lat, descriptor.lng], {
      icon: makeClusterIcon(descriptor),
      pane: interactivePaneName,
      keyboard: false,
      interactive: true,
      bubblingMouseEvents: false,
      zIndexOffset: 40
    });
    marker.bindTooltip(
      '<b>' + fmt(descriptor.siteCount) + ' individual USGS sites</b><br/>' +
      fmt(descriptor.locationCount) + ' mapped coordinate locations<br/>' +
      'Click to zoom in.',
      {
        direction: 'auto',
        opacity: 0.92,
        className: 'pt-usgs-gw-local-tooltip'
      }
    );
    marker.on('click', function() {
      var bounds = L.latLngBounds(
        [descriptor.south, descriptor.west],
        [descriptor.north, descriptor.east]
      );
      if (
        descriptor.west === descriptor.east &&
        descriptor.south === descriptor.north
      ) {
        map.setView(
          [descriptor.lat, descriptor.lng],
          Math.min(map.getMaxZoom(), map.getZoom() + 2),
          {animate: false}
        );
      } else {
        map.fitBounds(bounds, {padding: [24, 24], animate: false});
      }
    });
    return marker;
  }

  function createLayer(descriptor, token) {
    if (descriptor.type === 'cluster') return makeClusterLayer(descriptor);
    return makeExactLayer(descriptor.locationIndex, token);
  }

  function mountDescriptors(token, renderKey, built) {
    if (!tokenCurrent(token) || !layerActive) return;
    ensureDisplayPane();
    ensureInteractivePane();

    profilePhaseStart('layer-mount', {
      descriptors: built.descriptors.length,
      exactMode: built.exactMode
    });
    setRenderStatus('Drawing groundwater locations…', true);

    var previousRoot = currentRoot;
    if (previousRoot) {
      markRootMounted(previousRoot, false);
      try {
        if (map.hasLayer(previousRoot)) map.removeLayer(previousRoot);
      } catch (removePreviousErr) {}
      detachPointRenderer();
    }

    var root = L.layerGroup();
    currentRoot = root;
    currentRootComplete = false;
    currentRenderKey = renderKey;
    root.addTo(map);
    if (previousRoot && previousRoot !== root) disposeRootLater(previousRoot);

    var position = 0;
    var mounted = 0;
    var clusterObjects = 0;
    var exactObjects = 0;

    function mountStep() {
      if (!tokenCurrent(token) || !layerActive || currentRoot !== root) {
        markRootMounted(root, false);
        try { if (map.hasLayer(root)) map.removeLayer(root); } catch (removeErr) {}
        detachPointRenderer();
        if (currentRoot === root) {
          currentRoot = null;
          currentRootComplete = false;
          currentRenderKey = '';
        }
        disposeRootLater(root);
        profilePhaseEnd('layer-mount', {cancelled: true, mounted: mounted});
        return;
      }

      var started = now();
      var batch = 0;
      while (
        position < built.descriptors.length &&
        batch < 220 &&
        now() - started < frameBudgetMs
      ) {
        var descriptor = built.descriptors[position];
        position += 1;
        var layer = createLayer(descriptor, token);
        if (!layer) continue;
        if (
          layer._ptUsgsGwNested &&
          !layer._ptUsgsGwNested.interactionReady
        ) {
          profileRecord('nested-interaction-not-ready', {
            locationId: layer._ptUsgsGwNested.locationId,
            generation: token
          });
          continue;
        }
        markNestedMounted(layer, true, token);
        root.addLayer(layer);
        markNestedMounted(layer, true, token);
        mounted += 1;
        batch += 1;
        if (descriptor.type === 'cluster') clusterObjects += 1;
        else exactObjects += 1;
      }

      if (position < built.descriptors.length) {
        requestFrame(mountStep);
        return;
      }

      currentRootComplete = true;
      builtOnce = true;
      lastDrawn = mounted;
      lastVisibleSites = built.visibleSites;
      lastVisibleLocations = built.visibleLocations;
      lastClusterObjects = clusterObjects;
      lastExactObjects = exactObjects;
      loading = false;
      profilePhaseEnd('layer-mount', {
        cancelled: false,
        mounted: mounted,
        clusters: clusterObjects,
        exact: exactObjects
      });
      setRenderStatus('Groundwater wells drawn.', false, 900);
      profileRecord('render-complete', {
        renderKey: renderKey,
        stats: controllerStats()
      });
      if (profileEnabled) domSnapshot('render-complete');
    }

    requestFrame(mountStep);
  }

  function beginRender(reason) {
    if (!layerActive || destroyed) return;
    var key = viewportKey();

    if (currentRootComplete && currentRoot && currentRenderKey === key) {
      if (!map.hasLayer(currentRoot)) {
        setRenderStatus('Restoring groundwater wells…', true);
        currentRoot.addTo(map);
        markRootMounted(currentRoot, true, generation);
      }
      loading = false;
      setRenderStatus('Groundwater wells restored.', false, 700);
      profileRecord('display-cache-restored', {renderKey: key});
      return;
    }

    var token = nextGeneration('render:' + reason);
    loading = true;
    setRenderStatus('Preparing groundwater wells…', true);
    profileRecord('render-request', {
      reason: reason,
      renderKey: key,
      zoom: map.getZoom()
    });

    ensureSpatialIndex(token, function() {
      if (!tokenCurrent(token) || !layerActive) return;
      buildFilterMask(token, function() {
        if (!tokenCurrent(token) || !layerActive) return;
        profilePhaseStart('viewport-query', {zoom: map.getZoom()});
        var locationIds = queryLocations();
        lastQueryLocations = locationIds.length;
        profilePhaseEnd('viewport-query', {
          queriedLocations: locationIds.length
        });
        buildDescriptors(token, locationIds, function(built) {
          if (!tokenCurrent(token) || !layerActive) return;
          mountDescriptors(token, key, built);
        });
      });
    });
  }

  function scheduleRender(reason, delayMs) {
    if (!layerActive || destroyed) return;
    if (scheduledTimer !== null) clearTimeout(scheduledTimer);
    scheduledTimer = setTimeout(function() {
      scheduledTimer = null;
      beginRender(reason);
    }, Number(delayMs == null ? 34 : delayMs));
  }

  function suspendForMovement(reason) {
    if (!layerActive) return;
    nextGeneration('movement:' + reason);
    setRenderStatus('Waiting for basemap; updating groundwater wells…', true);
    detachCurrent(currentRootComplete);
    profileRecord('movement-suspend', {
      reason: reason,
      zoom: map.getZoom()
    });
  }

  function activate(reason) {
    layerActive = true;
    ensureDisplayPane();
    syncInteractivePanePointerState();
    scheduleRender(reason || 'activate', 0);
  }

  function deactivate(dropDisplayCache) {
    layerActive = false;
    syncInteractivePanePointerState();
    hardClearDisplay(!!dropDisplayCache);
    profileRecord('deactivate', {
      dropDisplayCache: !!dropDisplayCache
    });
  }

  function syncActive() {
    var checked = safeControlScan();
    var next = checked === null ? layerActive : checked;
    if (next && !layerActive) {
      activate('checkbox');
    } else if (!next && layerActive) {
      deactivate(false);
    } else if (
      next &&
      layerActive &&
      (!currentRoot || !map.hasLayer(currentRoot))
    ) {
      scheduleRender('sync', 0);
    }
  }

  function applyFilters(filters) {
    currentFilters = normalizedFilters(filters);
    filterVersion += 1;
    filterMaskReady = false;
    nextGeneration('filter-change');
    detachCurrent(false);
    if (layerActive) scheduleRender('filter', 0);
    return controllerStats();
  }

  function resetFilters() {
    return applyFilters(defaultFilters());
  }

  function controllerStats() {
    var container = map && map.getContainer ? map.getContainer() : null;
    return {
      filtered: lastFiltered,
      total: siteCount,
      uniqueCoordinates: locationCount,
      nestedLocations: Number(metadata.nestedLocationCount || 0),
      nestedSiteRecords: Number(metadata.nestedSiteRecordCount || 0),
      drawn: lastDrawn,
      visibleSites: lastVisibleSites,
      visibleLocations: lastVisibleLocations,
      queriedLocations: lastQueryLocations,
      clusterObjects: lastClusterObjects,
      exactObjects: lastExactObjects,
      leafletDisplayObjects: lastDrawn,
      markerDomElements: container ?
        container.querySelectorAll(
          '.pt-usgs-gw-grid-cluster,.pt-usgs-gw-virtual-nested-marker'
        ).length :
        0,
      canvasElements: container ?
        container.querySelectorAll('canvas').length :
        0,
      active: layerActive,
      loading: loading || renderStatusBusy,
      status: renderStatusText,
      indexReady: !!gridIndex,
      filterIndexReady: !!filterMaskReady,
      generation: generation,
      displayCacheReady: !!(currentRoot && currentRootComplete),
      deferredCleanupCount: deferredCleanupCount,
      clusterToExactZoom: clusterToExactZoom,
      ordinaryWellRadius: ordinaryWellRadius,
      ordinaryWellStrokeWeight: ordinaryWellStrokeWeight,
      ordinaryWellOuterDiameter: Number(
        (2 * ordinaryWellRadius + ordinaryWellStrokeWeight).toFixed(2)
      ),
      displayPane: displayPaneName,
      displayPaneZIndex: displayPaneZIndex,
      displayPanePassThrough: !!(
        map.getPane &&
        map.getPane(displayPaneName) &&
        map.getPane(displayPaneName).style.pointerEvents === 'none'
      ),
      displayRendererMounted: !!(
        pointRenderer &&
        map.hasLayer &&
        map.hasLayer(pointRenderer)
      ),
      displayCanvasConnected: !!(
        pointRenderer &&
        pointRenderer._container &&
        pointRenderer._container.isConnected
      ),
      interactivePane: interactivePaneName,
      interactivePaneZIndex: interactivePaneZIndex,
      interactionForwardingActive: !!(
        layerActive &&
        currentRoot &&
        map.hasLayer &&
        map.hasLayer(currentRoot)
      ),
      rendererForwardedMoveCount: rendererForwardedMoveCount,
      rendererForwardedClickCount: rendererForwardedClickCount,
      rendererHitCount: rendererHitCount,
      mapMoving: mapMoving,
      mapZooming: mapZooming
    };
  }

  function onMap(eventName, handler) {
    map.on(eventName, handler);
    listenerRecords.push({
      target: map,
      eventName: eventName,
      handler: handler
    });
  }

  function installStyles() {
    if (document.getElementById('pt-usgs-gw-virtualized-style')) return;
    var style = document.createElement('style');
    style.id = 'pt-usgs-gw-virtualized-style';
    style.textContent =
      '.leaflet-tooltip.pt-usgs-gw-local-tooltip{' +
      'font:12px/1.25 Arial,sans-serif;white-space:normal;' +
      'min-width:170px;max-width:360px;}' +
      '.leaflet-container.pt-usgs-gw-manual-hit{cursor:pointer!important;}' +
      '.pt-usgs-gw-virtual-nested-divicon,' +
      '.pt-usgs-gw-grid-cluster-divicon{' +
      'background:transparent;border:0;pointer-events:auto;}' +
      '.pt-usgs-gw-virtual-nested-marker{' +
      'position:relative;width:22px;height:22px;border-radius:5px;' +
      'border:1.4px solid #7B241C;background:rgba(246,190,103,.94);' +
      'box-shadow:0 1px 4px rgba(0,0,0,.25);}' +
      '.pt-usgs-gw-virtual-nested-marker:before{' +
      'content:"";position:absolute;left:5px;top:6px;width:3px;height:3px;' +
      'border-radius:50%;background:#111;box-shadow:7px 0 0 #111,' +
      '0 7px 0 #111,7px 7px 0 #111;opacity:.82;}' +
      '.pt-usgs-gw-virtual-nested-marker span{' +
      'position:absolute;right:-6px;top:-7px;min-width:13px;height:13px;' +
      'border-radius:7px;background:#7B241C;border:1px solid #fff;' +
      'color:#fff;font:700 8.5px/13px Arial,sans-serif;text-align:center;' +
      'padding:0 2px;box-sizing:border-box;}' +
      '.pt-usgs-gw-grid-cluster{' +
      'position:relative;width:100%;height:100%;border-radius:50%;' +
      'box-sizing:border-box;' +
      'display:flex;align-items:center;justify-content:center;' +
      'border:2px solid rgba(76,38,16,.78);' +
      'background:rgba(246,190,103,.88);' +
      'box-shadow:0 1px 5px rgba(0,0,0,.30);color:#3a1d0f;' +
      'font:700 11px/1 Arial,sans-serif;}' +
      '.pt-usgs-gw-grid-cluster:after{' +
      'content:"";position:absolute;inset:4px;border-radius:50%;' +
      'border:1px solid rgba(255,255,255,.72);pointer-events:none;}';
    document.head.appendChild(style);
  }

  function installHomeGuard() {
    homeButton = document.getElementById('pt-reset-zoom-btn');
    if (!homeButton || homeButton.__ptUsgsGwVirtualHomeGuard) return;
    homeButton.__ptUsgsGwVirtualHomeGuard = true;
    homeButtonHandler = function() {
      if (!layerActive) return;
      // Preserve the earlier basemap-recovery fix: detach the bounded display
      // before the normal home handler changes view. The ordinary handler is
      // not prevented, and the viewport layer is rebuilt after movement.
      suspendForMovement('home');
    };
    homeButton.addEventListener('click', homeButtonHandler, true);
  }

  function documentChangeHandler(event) {
    var target = event && event.target;
    if (
      target &&
      target.matches &&
      target.matches(
        '.leaflet-control-layers-overlays input[type="checkbox"]'
      )
    ) setTimeout(syncActive, 0);
  }

  function documentClickProfileHandler(event) {
    if (!profileEnabled) return;
    var target = event && event.target && event.target.closest ?
      event.target.closest(
        '.pt-main-layer-clear-btn,#pt-clear-all-btn,' +
        '#pt-reset-zoom-btn,#pt-marquee-zoom-btn'
      ) :
      null;
    if (!target) return;
    profileRecord('ui-click', {
      id: target.id || '',
      className: target.className || '',
      text: String(target.textContent || '').trim()
    });
    requestFrame(function() {
      if (!destroyed) domSnapshot('after-ui-click');
    });
  }

  function destroy() {
    if (destroyed) return;
    destroyed = true;
    nextGeneration('destroy');
    detachCurrent(false);
    listenerRecords.forEach(function(record) {
      try {
        record.target.off(
          record.eventName,
          record.handler
        );
      } catch (listenerErr) {}
    });
    listenerRecords = [];
    document.removeEventListener('change', documentChangeHandler, true);
    document.removeEventListener('click', documentClickProfileHandler, true);
    disableDebug();
    if (homeButton && homeButtonHandler) {
      homeButton.removeEventListener('click', homeButtonHandler, true);
      delete homeButton.__ptUsgsGwVirtualHomeGuard;
    }
    tileLayerRecords.forEach(function(record) {
      try {
        record.layer.off('loading', record.loadingHandler);
        record.layer.off('load', record.loadHandler);
      } catch (tileOffErr) {}
    });
    tileLayerRecords = [];
    if (longTaskObserver) {
      try { longTaskObserver.disconnect(); } catch (disconnectErr) {}
    }
    if (interactivePaneObserver) {
      try { interactivePaneObserver.disconnect(); } catch (paneObserverErr) {}
      interactivePaneObserver = null;
    }
    containerListenerRecords.forEach(function(record) {
      try {
        record.target.removeEventListener(
          record.eventName,
          record.handler,
          true
        );
      } catch (containerListenerError) {}
    });
    containerListenerRecords = [];
    if (window.BRIM_USGS_GW_DEBUG === debugApi) {
      delete window.BRIM_USGS_GW_DEBUG;
    }
  }

  installStyles();
  ensureDisplayPane();
  installInteractivePaneObserver();
  installRendererForwarding();
  installProfiler();

  window.BRIM_USGS_GW_PROFILE = {
    enabled: function() { return profileEnabled; },
    enable: function() {
      profileEnabled = true;
      installProfiler();
      return true;
    },
    disable: function() {
      profileEnabled = false;
      return false;
    },
    startScenario: startScenario,
    snapshot: domSnapshot,
    endScenario: endScenario,
    report: profileReport,
    downloadJson: downloadProfileJson,
    metadata: function() { return metadata; }
  };

  debugApi = {
    enable: enableDebug,
    disable: disableDebug,
    enabled: function() { return debugEnabled; },
    inspectNestedAtCursor: inspectNestedAtCursor,
    nestedLayers: function() {
      return currentNestedLayers().map(function(layer) {
        var details = layer._ptUsgsGwNested;
        return {
          locationId: details.locationId,
          locationIndex: details.locationIndex,
          siteCount: details.siteCount,
          createdGeneration: details.createdGeneration,
          mountedGeneration: details.mountedGeneration,
          mounted: details.mounted,
          interactionReady: details.interactionReady,
          handlers: {
            mouseover: handlerCount(layer, 'mouseover'),
            click: handlerCount(layer, 'click')
          }
        };
      });
    }
  };
  window.BRIM_USGS_GW_DEBUG = debugApi;

  window.BRIM_USGS_GW_LOCAL = {
    applyFilters: applyFilters,
    setActive: function(active) {
      if (active) activate('api');
      else deactivate(false);
    },
    stats: controllerStats,
    resetFilters: resetFilters,
    clear: function() {
      deactivate(true);
    },
    refresh: function() {
      if (layerActive) {
        nextGeneration('api-refresh');
        detachCurrent(false);
        scheduleRender('api-refresh', 0);
      }
    },
    destroy: destroy
  };

  onMap('overlayadd', function(event) {
    if (!eventMatches(event)) return;
    profileRecord('overlayadd', {name: event.name || ''});
    activate('overlayadd');
  });

  onMap('overlayremove', function(event) {
    if (!eventMatches(event)) return;
    profileRecord('overlayremove', {name: event.name || ''});
    deactivate(false);
  });

  onMap('zoomstart', function() {
    mapZooming = true;
    profileRecord('map-event', {name: 'zoomstart', zoom: map.getZoom()});
    suspendForMovement('zoomstart');
  });

  onMap('zoom', function() {
    profileRecord('map-event', {name: 'zoom', zoom: map.getZoom()});
  });

  onMap('zoomend', function() {
    mapZooming = false;
    profileRecord('map-event', {name: 'zoomend', zoom: map.getZoom()});
    scheduleRender('zoomend', 52);
  });

  onMap('movestart', function() {
    mapMoving = true;
    profileRecord('map-event', {name: 'movestart', zoom: map.getZoom()});
    if (!mapZooming) suspendForMovement('movestart');
  });

  onMap('moveend', function() {
    mapMoving = false;
    profileRecord('map-event', {name: 'moveend', zoom: map.getZoom()});
    scheduleRender('moveend', 52);
  });

  onMap('pt:marqueezoomstart', function() {
    mapMoving = true;
    mapZooming = true;
    profileRecord('map-event', {
      name: 'pt:marqueezoomstart',
      zoom: map.getZoom()
    });
    suspendForMovement('marquee');
  });

  onMap('pt:marqueezoomend', function() {
    mapMoving = false;
    mapZooming = false;
    profileRecord('map-event', {
      name: 'pt:marqueezoomend',
      zoom: map.getZoom()
    });
    scheduleRender('marquee-end', 52);
  });

  onMap('layeradd', function(event) {
    if (event && event.layer) hookTileLayer(event.layer);
  });

  onMap('pt:measureinteractionchange', function() {
    syncInteractivePanePointerState();
  });

  document.addEventListener('change', documentChangeHandler, true);
  document.addEventListener('click', documentClickProfileHandler, true);

  setTimeout(syncActive, 0);
  setTimeout(syncActive, 500);
  setTimeout(installHomeGuard, 0);
  setTimeout(installHomeGuard, 800);
  setTimeout(installHomeGuard, 1800);
}
