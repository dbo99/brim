function(el, x, data) {
  var map = this;
  var groupName = data && data.groupName ?
    String(data.groupName) :
    'Points – Springs';
  var labelGroupName = data && data.labelGroupName ?
    String(data.labelGroupName) :
    'Labels – Springs';
  var columns = data && data.records ? data.records : {};
  var locations = data && data.locations ? data.locations : {};
  var metadata = data && data.metadata ? data.metadata : {};

  if (
    window.BRIM_SPRINGS_LOCAL &&
    typeof window.BRIM_SPRINGS_LOCAL.destroy === 'function'
  ) {
    try { window.BRIM_SPRINGS_LOCAL.destroy(); } catch (oldControllerErr) {}
  }

  var recordCount = Number(
    metadata.recordCount || columnLength(columns.spring_id)
  );
  var locationCount = Number(
    metadata.uniqueCoordinateCount || columnLength(locations.record_start)
  );
  var clusterToExactZoom = Number(metadata.clusterToExactZoom || 11);
  var spatialCellDegrees = Number(metadata.spatialCellDegrees || 0.25);
  var frameBudgetMs = 8;
  var interactivePaneName = 'pane_springs_interactive';
  var interactivePaneZIndex = 584;
  var generation = 0;
  var scheduledFrame = null;
  var scheduledTimer = null;
  var labelGeneration = 0;
  var labelFrame = null;
  var statusClearTimer = null;
  var gridIndex = null;
  var filterMask = null;
  var filterMaskReady = true;
  var currentFilters = defaultFilters();
  var layerActive = false;
  var labelsActive = false;
  var loading = false;
  var destroyed = false;
  var mapMoving = false;
  var mapZooming = false;
  var currentRoot = null;
  var currentRootComplete = false;
  var currentRenderKey = '';
  var labelRoot = null;
  var deferredCleanupCount = 0;
  var lastFiltered = recordCount;
  var lastDrawn = 0;
  var lastVisibleRecords = 0;
  var lastVisibleLocations = 0;
  var lastQueriedLocations = 0;
  var lastAggregateObjects = 0;
  var lastExactObjects = 0;
  var lastLabelObjects = 0;
  var lastVisibleRecordIndices = [];
  var renderStatusText = '';
  var renderStatusBusy = false;
  var legendUserHidden = false;
  var legend = null;
  var listenerRecords = [];
  var homeButton = null;
  var homeButtonHandler = null;
  var interactivePaneObserver = null;
  var pointRenderer = typeof L.canvas === 'function' ?
    L.canvas({pane: 'pane_points', padding: 0.45, tolerance: 4}) :
    null;

  var profileEnabled = profileInitiallyEnabled();
  var profileScenario = null;
  var profileEvents = [];
  var profileLongTasks = [];
  var longTaskObserver = null;

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

  function locationRange(locationIndex) {
    return {
      start: Number(locationValue('record_start', locationIndex) || 0),
      count: Number(locationValue('record_count', locationIndex) || 0)
    };
  }

  function locationLat(locationIndex) {
    return Number(valueAt(
      'pt_lat',
      Number(locationValue('record_start', locationIndex) || 0)
    ));
  }

  function locationLng(locationIndex) {
    return Number(valueAt(
      'pt_lng',
      Number(locationValue('record_start', locationIndex) || 0)
    ));
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

  function boolValue(value) {
    if (value === true) return true;
    if (value === false) return false;
    var text = String(value == null ? '' : value).trim().toLowerCase();
    if (['true', 't', '1', 'yes', 'y'].indexOf(text) >= 0) return true;
    if (['false', 'f', '0', 'no', 'n'].indexOf(text) >= 0) return false;
    return null;
  }

  function fmt(value) {
    var parsed = Number(value);
    return isFinite(parsed) ? parsed.toLocaleString() : '0';
  }

  function compactCount(value) {
    var parsed = Number(value || 0);
    if (parsed >= 1000000) return (parsed / 1000000).toFixed(1) + 'm';
    if (parsed >= 10000) return Math.round(parsed / 1000) + 'k';
    if (parsed >= 1000) return (parsed / 1000).toFixed(1) + 'k';
    return String(parsed);
  }

  function now() {
    return (
      window.performance &&
      typeof window.performance.now === 'function'
    ) ? window.performance.now() : Date.now();
  }

  function normalizedText(value) {
    return String(value == null ? '' : value)
      .replace(/&amp;/g, '&')
      .replace(/[–—]/g, '-')
      .toLowerCase()
      .replace(/points\s*-\s*/g, '')
      .replace(/monitoring sites\s*\/\s*records\s*-\s*/g, '')
      .replace(/\s*\([^)]*\)\s*$/g, '')
      .replace(/\s+/g, ' ')
      .trim();
  }

  function isSpringsText(value) {
    var text = normalizedText(value);
    return text === 'springs';
  }

  function isLabelText(value) {
    var raw = String(value == null ? '' : value);
    return /^\s*Labels\s*[–-]\s*/i.test(raw) &&
      normalizedText(raw.replace(/^\s*Labels\s*[–-]\s*/i, '')) === 'springs';
  }

  function eventOptionValues(event) {
    if (!event || !event.layer || !event.layer.options) return [];
    return [
      event.layer.options.group,
      event.layer.options.name,
      event.layer.options.layerId
    ];
  }

  function eventMatches(event) {
    if (!event || labelEventMatches(event)) return false;
    if (isSpringsText(event.name)) return true;
    var values = eventOptionValues(event);
    for (var index = 0; index < values.length; index += 1) {
      if (isSpringsText(values[index])) return true;
    }
    return false;
  }

  function labelEventMatches(event) {
    if (!event) return false;
    if (isLabelText(event.name)) return true;
    var values = eventOptionValues(event);
    for (var index = 0; index < values.length; index += 1) {
      if (isLabelText(values[index])) return true;
    }
    return false;
  }

  function safeControlScan(labelOnly) {
    if (typeof document === 'undefined' || !document.querySelectorAll) {
      return null;
    }
    var labels = document.querySelectorAll(
      '.leaflet-control-layers-overlays label'
    );
    for (var index = 0; index < labels.length; index += 1) {
      var label = labels[index];
      var fullName = label.getAttribute ?
        (label.getAttribute('data-pt-layer-full-name') || '') :
        '';
      var text = fullName || label.textContent || label.innerText || '';
      var matches = labelOnly ? isLabelText(text) : isSpringsText(text);
      if (!matches) continue;
      var input = label.querySelector ?
        label.querySelector('input[type="checkbox"]') :
        null;
      if (input) return !!input.checked;
    }
    return null;
  }

  function defaultFilters() {
    return {source: 'all', blmMode: 'any', blmMax: null};
  }

  function normalizedFilters(filters) {
    var next = filters || {};
    var source = ['all', 'nhd', 'survey'].indexOf(next.source) >= 0 ?
      next.source :
      'all';
    var blmMode = ['any', 'on', 'off', 'distance'].indexOf(next.blmMode) >= 0 ?
      next.blmMode :
      'any';
    var blmMax = num(next.blmMax);
    if (blmMode !== 'distance' || blmMax === null || blmMax < 0) {
      blmMax = null;
      if (blmMode === 'distance') blmMode = 'any';
    }
    return {source: source, blmMode: blmMode, blmMax: blmMax};
  }

  function filterSignature() {
    return [
      currentFilters.source,
      currentFilters.blmMode,
      currentFilters.blmMax === null ? '' : currentFilters.blmMax
    ].join('|');
  }

  function isNhd(index) {
    return String(valueAt('spring_source_key', index) || '') === 'nhd';
  }

  function isSurvey(index) {
    return String(valueAt('spring_source_key', index) || '') ===
      'survey_2015_16';
  }

  function recordPassesFilters(index) {
    if (currentFilters.source === 'nhd' && !isNhd(index)) return false;
    if (currentFilters.source === 'survey' && !isSurvey(index)) return false;

    var onBlm = boolValue(valueAt('on_blm_ca', index));
    var distance = num(valueAt('dist_to_blm_mi', index));
    if (currentFilters.blmMode === 'on') return onBlm === true;
    if (currentFilters.blmMode === 'off') return onBlm === false;
    if (currentFilters.blmMode === 'distance') {
      return (
        distance !== null &&
        currentFilters.blmMax !== null &&
        distance <= currentFilters.blmMax
      );
    }
    return true;
  }

  function filtersAreDefault() {
    return (
      currentFilters.source === 'all' &&
      currentFilters.blmMode === 'any' &&
      currentFilters.blmMax === null
    );
  }

  function ensureInteractivePane() {
    var pane = map.getPane ? map.getPane(interactivePaneName) : null;
    if (!pane && map.createPane) pane = map.createPane(interactivePaneName);
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
    pane.style.pointerEvents = measureActive ? 'none' : 'auto';
  }

  function installInteractivePaneObserver() {
    var pane = ensureInteractivePane();
    if (!pane || interactivePaneObserver || !window.MutationObserver) return;
    interactivePaneObserver = new window.MutationObserver(function() {
      syncInteractivePanePointerState(pane);
    });
    interactivePaneObserver.observe(pane, {
      attributes: true,
      attributeFilter: ['style', 'class', 'data-pt-measure-suspended']
    });
  }

  function requestFrame(callback) {
    var request = window.requestAnimationFrame || function(fn) {
      return window.setTimeout(fn, 16);
    };
    scheduledFrame = request(function(timestamp) {
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

  function setRenderStatus(text, busy, clearAfterMs) {
    renderStatusText = text || '';
    renderStatusBusy = !!busy;
    loading = !!busy;
    if (statusClearTimer) {
      clearTimeout(statusClearTimer);
      statusClearTimer = null;
    }
    updateLegend();
    if (Number(clearAfterMs || 0) > 0) {
      var expected = renderStatusText;
      statusClearTimer = setTimeout(function() {
        if (destroyed || renderStatusText !== expected) return;
        renderStatusText = '';
        renderStatusBusy = false;
        loading = false;
        updateLegend();
      }, Number(clearAfterMs));
    }
  }

  function closeInteractiveState() {
    try { map.closePopup(); } catch (popupErr) {}
    try {
      var container = map.getContainer();
      var tooltips = container.querySelectorAll(
        '.leaflet-tooltip.pt-springs-local-tooltip'
      );
      Array.prototype.forEach.call(tooltips, function(tooltip) {
        if (tooltip && tooltip.parentNode) tooltip.parentNode.removeChild(tooltip);
      });
    } catch (tooltipErr) {}
  }

  function disposeRootLater(root) {
    if (!root) return;
    deferredCleanupCount += 1;
    var cleanup = function() {
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
    if (!currentRoot) return;
    try {
      if (map.hasLayer(currentRoot)) map.removeLayer(currentRoot);
    } catch (removeErr) {}
    if (!preserveComplete || !currentRootComplete) {
      var oldRoot = currentRoot;
      currentRoot = null;
      currentRootComplete = false;
      currentRenderKey = '';
      disposeRootLater(oldRoot);
    }
  }

  function cancelLabelWork() {
    labelGeneration += 1;
    if (labelFrame !== null) {
      if (window.cancelAnimationFrame) {
        window.cancelAnimationFrame(labelFrame);
      } else {
        clearTimeout(labelFrame);
      }
      labelFrame = null;
    }
  }

  function clearLabels() {
    cancelLabelWork();
    lastLabelObjects = 0;
    if (!labelRoot) return;
    var oldRoot = labelRoot;
    labelRoot = null;
    try {
      if (map.hasLayer(oldRoot)) map.removeLayer(oldRoot);
    } catch (removeErr) {}
    disposeRootLater(oldRoot);
  }

  function hardClearDisplay(dropDisplayCache) {
    nextGeneration('clear');
    setRenderStatus('', false);
    clearLabels();
    detachCurrent(!dropDisplayCache);
    if (dropDisplayCache && currentRoot) {
      var oldRoot = currentRoot;
      currentRoot = null;
      currentRootComplete = false;
      currentRenderKey = '';
      disposeRootLater(oldRoot);
    }
    if (!currentRoot || !currentRootComplete) {
      lastDrawn = 0;
      lastVisibleRecords = 0;
      lastVisibleLocations = 0;
      lastQueriedLocations = 0;
      lastAggregateObjects = 0;
      lastExactObjects = 0;
      lastVisibleRecordIndices = [];
    }
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

    setRenderStatus('Indexing spring locations…', true);
    profileRecord('index-start', {locations: locationCount});
    var work = {};
    var position = 0;

    function step() {
      if (!tokenCurrent(token)) return;
      var started = now();
      while (
        position < locationCount &&
        now() - started < frameBudgetMs
      ) {
        var key = spatialKey(
          locationLng(position),
          locationLat(position)
        );
        if (!work[key]) work[key] = [];
        work[key].push(position);
        position += 1;
      }
      if (position < locationCount) {
        requestFrame(step);
        return;
      }
      gridIndex = work;
      profileRecord('index-complete', {locations: locationCount});
      done();
    }

    requestFrame(step);
  }

  function buildFilterMask(token, done) {
    if (filtersAreDefault()) {
      filterMask = null;
      filterMaskReady = true;
      lastFiltered = recordCount;
      done();
      return;
    }

    setRenderStatus('Filtering spring records…', true);
    filterMaskReady = false;
    var work = new Array(recordCount);
    var position = 0;
    var matching = 0;

    function step() {
      if (!tokenCurrent(token)) return;
      var started = now();
      while (
        position < recordCount &&
        now() - started < frameBudgetMs
      ) {
        var passes = recordPassesFilters(position);
        work[position] = passes;
        if (passes) matching += 1;
        position += 1;
      }
      if (position < recordCount) {
        requestFrame(step);
        return;
      }
      filterMask = work;
      filterMaskReady = true;
      lastFiltered = matching;
      updateLegend();
      done();
    }

    requestFrame(step);
  }

  function bufferedBounds() {
    var bounds = map.getBounds();
    var zoom = Number(map.getZoom());
    var west = Number(bounds.getWest());
    var south = Number(bounds.getSouth());
    var east = Number(bounds.getEast());
    var north = Number(bounds.getNorth());
    var ratio = zoom >= 11 ? 0.15 : (zoom >= 8 ? 0.18 : 0.10);
    var lngPad = Math.max(0.04, (east - west) * ratio);
    var latPad = Math.max(0.04, (north - south) * ratio);
    return {
      west: Math.max(-180, west - lngPad),
      south: Math.max(-90, south - latPad),
      east: Math.min(180, east + lngPad),
      north: Math.min(90, north + latPad)
    };
  }

  function queryLocations() {
    var bounds = bufferedBounds();
    var minX = Math.floor(bounds.west / spatialCellDegrees);
    var maxX = Math.floor(bounds.east / spatialCellDegrees);
    var minY = Math.floor(bounds.south / spatialCellDegrees);
    var maxY = Math.floor(bounds.north / spatialCellDegrees);
    var result = [];

    for (var xIndex = minX; xIndex <= maxX; xIndex += 1) {
      for (var yIndex = minY; yIndex <= maxY; yIndex += 1) {
        var bucket = gridIndex[xIndex + ':' + yIndex];
        if (!bucket) continue;
        for (var index = 0; index < bucket.length; index += 1) {
          var locationIndex = bucket[index];
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

  function matchingIndicesForLocation(locationIndex) {
    var range = locationRange(locationIndex);
    var indices = [];
    for (
      var index = range.start;
      index < range.start + range.count;
      index += 1
    ) {
      if (!filterMask || filterMask[index]) indices.push(index);
    }
    return indices;
  }

  function clusterRadius(zoom) {
    if (zoom <= 6) return 175;
    if (zoom <= 8) return 145;
    if (zoom <= 9) return 115;
    if (zoom <= 10) return 75;
    return 35;
  }

  function buildDescriptors(token, locationIds, done) {
    var zoom = Number(map.getZoom());
    var exactMode = zoom >= clusterToExactZoom;
    var cellSize = clusterRadius(zoom);
    var aggregates = exactMode ? null : {};
    var descriptors = [];
    var visibleRecords = 0;
    var visibleLocations = 0;
    var visibleRecordIndices = [];
    var position = 0;

    setRenderStatus(
      exactMode ?
        'Preparing visible spring locations…' :
        'Aggregating visible spring locations…',
      true
    );

    function step() {
      if (!tokenCurrent(token) || !layerActive) return;
      var started = now();
      while (
        position < locationIds.length &&
        now() - started < frameBudgetMs
      ) {
        var locationIndex = locationIds[position];
        position += 1;
        var matchedIndices = matchingIndicesForLocation(locationIndex);
        if (!matchedIndices.length) continue;

        visibleRecords += matchedIndices.length;
        visibleLocations += 1;
        if (exactMode) {
          descriptors.push({
            type: 'exact',
            locationIndex: locationIndex,
            recordIndices: matchedIndices
          });
          for (var matchIndex = 0; matchIndex < matchedIndices.length; matchIndex += 1) {
            visibleRecordIndices.push(matchedIndices[matchIndex]);
          }
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
            type: 'aggregate',
            recordCount: 0,
            locationCount: 0,
            sumX: 0,
            sumY: 0,
            west: lng,
            east: lng,
            south: lat,
            north: lat,
            onlyLocation: locationIndex,
            onlyRecordIndices: matchedIndices
          };
        }
        aggregate.recordCount += matchedIndices.length;
        aggregate.locationCount += 1;
        aggregate.sumX += pixel.x;
        aggregate.sumY += pixel.y;
        aggregate.west = Math.min(aggregate.west, lng);
        aggregate.east = Math.max(aggregate.east, lng);
        aggregate.south = Math.min(aggregate.south, lat);
        aggregate.north = Math.max(aggregate.north, lat);
        if (aggregate.locationCount > 1) {
          aggregate.onlyLocation = null;
          aggregate.onlyRecordIndices = null;
        }
      }

      if (position < locationIds.length) {
        requestFrame(step);
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
              locationIndex: aggregate.onlyLocation,
              recordIndices: aggregate.onlyRecordIndices
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

      done({
        descriptors: descriptors,
        visibleRecords: visibleRecords,
        visibleLocations: visibleLocations,
        visibleRecordIndices: visibleRecordIndices,
        exactMode: exactMode
      });
    }

    requestFrame(step);
  }

  function sourceDisplay(index) {
    return has(valueAt('spring_source_display', index)) ?
      valueAt('spring_source_display', index) :
      'Unknown source';
  }

  function springName(index) {
    return has(valueAt('spring_name_display', index)) ?
      valueAt('spring_name_display', index) :
      'Unnamed spring';
  }

  function tooltipHtml(index) {
    return (
      esc(springName(index)) +
      '<br/>Source: ' +
      esc(sourceDisplay(index))
    );
  }

  function optionalPopupField(parts, index, fieldName, label) {
    var value = valueAt(fieldName, index);
    if (has(value)) {
      parts.push('<div><b>' + esc(label) + ':</b> ' + esc(value) + '</div>');
    }
  }

  function popupHtml(index, compact) {
    var parts = [];
    parts.push(
      '<div class="pt-springs-popup-record' +
      (compact ? ' compact' : '') +
      '">'
    );
    parts.push(
      '<div class="pt-springs-popup-title">Spring: ' +
      esc(springName(index)) +
      '</div>'
    );
    parts.push('<div><b>Source:</b> ' + esc(sourceDisplay(index)) + '</div>');
    parts.push(
      '<div><b>Record ID:</b> ' +
      esc(valueAt('spring_id', index)) +
      '</div>'
    );

    var onBlm = boolValue(valueAt('on_blm_ca', index));
    var blmDistance = num(valueAt('dist_to_blm_mi', index));
    if (onBlm === true) {
      parts.push('<div><b>BLM distance:</b> on BLM</div>');
    } else if (blmDistance !== null) {
      parts.push(
        '<div><b>BLM distance:</b> ' +
        esc(
          blmDistance
            .toFixed(blmDistance < 10 ? 2 : 1)
            .replace(/\.00$/, '')
        ) +
        ' mi</div>'
      );
    }

    if (has(valueAt('elevation_display', index))) {
      optionalPopupField(parts, index, 'elevation_display', 'Elevation');
    } else if (num(valueAt('elevation_ft', index)) !== null) {
      parts.push(
        '<div><b>Elevation:</b> ' +
        esc(num(valueAt('elevation_ft', index))) +
        ' ft</div>'
      );
    }
    optionalPopupField(parts, index, 'gnis_id', 'GNIS ID');
    optionalPopupField(parts, index, 'spring_type', 'Type');
    optionalPopupField(parts, index, 'spring_classification', 'Classification');
    optionalPopupField(parts, index, 'spring_condition', 'Condition');
    optionalPopupField(parts, index, 'spring_status', 'Status');
    optionalPopupField(parts, index, 'flow_condition', 'Flow / condition');
    if (has(valueAt('flow_rate', index))) {
      parts.push(
        '<div><b>Flow:</b> ' +
        esc(valueAt('flow_rate', index)) +
        (has(valueAt('flow_unit', index)) ?
          ' ' + esc(valueAt('flow_unit', index)) :
          '') +
        '</div>'
      );
    }
    optionalPopupField(parts, index, 'provider', 'Provider');

    var links = [];
    var googleUrl = valueAt('google_search_url', index);
    var reportUrl = valueAt('source_report_url', index);
    var reportLabel = has(valueAt('source_report_label', index)) ?
      valueAt('source_report_label', index) :
      'Source report';
    if (has(googleUrl)) {
      links.push(
        '<a href="' + esc(googleUrl) +
        '" target="_blank" rel="noopener">Google search</a>'
      );
    }
    if (has(reportUrl)) {
      links.push(
        '<a href="' + esc(reportUrl) +
        '" target="_blank" rel="noopener">' +
        esc(reportLabel) +
        '</a>'
      );
    }
    if (links.length) {
      parts.push('<div class="pt-springs-popup-links">' + links.join(' · ') + '</div>');
    }
    parts.push('</div>');
    return parts.join('');
  }

  function multiRecordTooltip(indices) {
    var names = [];
    for (var index = 0; index < indices.length && index < 3; index += 1) {
      names.push(esc(springName(indices[index])));
    }
    return (
      '<b>' + fmt(indices.length) +
      ' spring records at one mapped location</b><br/>' +
      names.join('<br/>') +
      (indices.length > 3 ? '<br/>…' : '')
    );
  }

  function multiRecordPopup(indices) {
    var parts = [
      '<div class="pt-springs-multi-popup">',
      '<div class="pt-springs-multi-title">' +
        fmt(indices.length) +
        ' spring records at one mapped location</div>',
      '<div class="pt-springs-multi-note">' +
        'Each analytical record is retained separately.</div>',
      '<div class="pt-springs-multi-list">'
    ];
    for (var index = 0; index < indices.length; index += 1) {
      parts.push(popupHtml(indices[index], true));
    }
    parts.push('</div></div>');
    return parts.join('');
  }

  function bindLazyTooltip(layer, htmlBuilder) {
    layer.on('mouseover', function() {
      if (map._ptMeasureInteractionActive) return;
      try {
        if (!layer.getTooltip || !layer.getTooltip()) {
          layer.bindTooltip(htmlBuilder(), {
            direction: 'auto',
            opacity: 0.9,
            sticky: true,
            className: 'pt-springs-local-tooltip'
          });
        }
        if (layer.openTooltip) layer.openTooltip();
      } catch (tooltipErr) {}
    });
  }

  function makeMultiRecordIcon(indices) {
    var count = indices.length;
    var allNhd = indices.every(function(index) { return isNhd(index); });
    var allSurvey = indices.every(function(index) { return isSurvey(index); });
    var sourceClass = allNhd ? ' nhd' : (allSurvey ? ' survey' : ' mixed');
    var label = String(Math.max(2, Math.min(99, Number(count) || 2)));
    return L.divIcon({
      className: 'pt-springs-multi-divicon',
      html:
        '<div class="pt-springs-multi-marker' + sourceClass + '">' +
        '<span>' + escLoose(label) + '</span></div>',
      iconSize: L.point(24, 24),
      iconAnchor: L.point(12, 12),
      popupAnchor: L.point(0, -12)
    });
  }

  function exactStyle(index) {
    if (isSurvey(index)) {
      return {
        radius: 7,
        color: '#E6550D',
        weight: 2,
        opacity: 0.96,
        fillColor: '#FFFFFF',
        fillOpacity: 0
      };
    }
    if (isNhd(index)) {
      return {
        radius: 4,
        color: '#FFFFFF',
        weight: 0.7,
        opacity: 0.88,
        fillColor: '#2B6CB0',
        fillOpacity: 0.80
      };
    }
    return {
      radius: 4,
      color: '#333333',
      weight: 1,
      opacity: 0.88,
      fillColor: '#777777',
      fillOpacity: 0.70
    };
  }

  function makeExactLayer(descriptor) {
    var indices = descriptor.recordIndices || [];
    if (!indices.length) return null;

    if (indices.length > 1) {
      var locationIndex = descriptor.locationIndex;
      var marker = L.marker(
        [locationLat(locationIndex), locationLng(locationIndex)],
        {
          icon: makeMultiRecordIcon(indices),
          pane: interactivePaneName,
          keyboard: false,
          interactive: true,
          bubblingMouseEvents: false,
          riseOnHover: true,
          zIndexOffset: 90
        }
      );
      marker._ptSpringsMulti = {
        locationIndex: locationIndex,
        recordCount: indices.length,
        recordIds: indices.map(function(index) {
          return String(valueAt('spring_id', index));
        })
      };
      bindLazyTooltip(marker, function() {
        return multiRecordTooltip(indices);
      });
      marker.bindPopup(function() {
        return multiRecordPopup(indices);
      }, {maxWidth: 430, maxHeight: 520});
      return marker;
    }

    var index = indices[0];
    var style = exactStyle(index);
    var options = {
      pane: 'pane_points',
      radius: style.radius,
      stroke: true,
      color: style.color,
      weight: style.weight,
      opacity: style.opacity,
      fill: true,
      fillColor: style.fillColor,
      fillOpacity: style.fillOpacity,
      interactive: true,
      bubblingMouseEvents: false
    };
    if (pointRenderer) options.renderer = pointRenderer;
    var point = L.circleMarker(
      [Number(valueAt('pt_lat', index)), Number(valueAt('pt_lng', index))],
      options
    );
    point._ptSpringRecordId = String(valueAt('spring_id', index));
    bindLazyTooltip(point, function() {
      return tooltipHtml(index);
    });
    point.bindPopup(function() {
      return popupHtml(index, false);
    }, {maxWidth: 420, maxHeight: 520});
    return point;
  }

  function makeAggregateIcon(descriptor) {
    var size = descriptor.recordCount >= 1000 ? 44 :
      (descriptor.recordCount >= 100 ? 38 : 33);
    return L.divIcon({
      className: 'pt-springs-aggregate-divicon',
      html:
        '<div class="pt-springs-aggregate-marker" data-record-count="' +
        descriptor.recordCount +
        '"><span>' +
        compactCount(descriptor.recordCount) +
        '</span></div>',
      iconSize: L.point(size, size),
      iconAnchor: L.point(size / 2, size / 2)
    });
  }

  function makeAggregateLayer(descriptor) {
    var marker = L.marker([descriptor.lat, descriptor.lng], {
      icon: makeAggregateIcon(descriptor),
      pane: interactivePaneName,
      keyboard: false,
      interactive: true,
      bubblingMouseEvents: false,
      zIndexOffset: 40
    });
    bindLazyTooltip(marker, function() {
      return (
        '<b>' + fmt(descriptor.recordCount) +
        ' individual spring records</b><br/>' +
        fmt(descriptor.locationCount) +
        ' mapped coordinate locations<br/>' +
        'Click to zoom in.'
      );
    });
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

  function createLayer(descriptor) {
    return descriptor.type === 'aggregate' ?
      makeAggregateLayer(descriptor) :
      makeExactLayer(descriptor);
  }

  function labelText(index) {
    var text = has(valueAt('spring_label_text', index)) ?
      String(valueAt('spring_label_text', index)).trim() :
      String(springName(index)).trim();
    if (!has(text) || /^unnamed\b/i.test(text)) return '';
    return text;
  }

  function makeLabelLayer(index) {
    var text = labelText(index);
    if (!text) return null;
    return L.marker(
      [Number(valueAt('pt_lat', index)), Number(valueAt('pt_lng', index))],
      {
        interactive: false,
        keyboard: false,
        pane: 'pane_labels_pts',
        icon: L.divIcon({
          className: 'pt-springs-local-label-divicon',
          html: '<span>' + esc(text) + '</span>',
          iconSize: L.point(1, 1),
          iconAnchor: L.point(0, 0)
        })
      }
    );
  }

  function rebuildLabels() {
    clearLabels();
    if (
      !labelsActive ||
      !layerActive ||
      Number(map.getZoom()) < 12 ||
      !lastVisibleRecordIndices.length
    ) return;

    var token = labelGeneration;
    var root = L.layerGroup();
    labelRoot = root;
    root.addTo(map);
    var position = 0;
    var mounted = 0;
    var maxLabels = 700;
    var request = window.requestAnimationFrame || function(fn) {
      return window.setTimeout(fn, 16);
    };

    function step() {
      labelFrame = null;
      if (
        destroyed ||
        token !== labelGeneration ||
        !labelsActive ||
        !layerActive ||
        labelRoot !== root
      ) return;
      var started = now();
      var batch = 0;
      while (
        position < lastVisibleRecordIndices.length &&
        mounted < maxLabels &&
        batch < 100 &&
        now() - started < frameBudgetMs
      ) {
        var layer = makeLabelLayer(lastVisibleRecordIndices[position]);
        position += 1;
        if (!layer) continue;
        root.addLayer(layer);
        mounted += 1;
        batch += 1;
      }
      lastLabelObjects = mounted;
      if (
        position < lastVisibleRecordIndices.length &&
        mounted < maxLabels
      ) {
        labelFrame = request(step);
      }
    }

    labelFrame = request(step);
  }

  function mountDescriptors(token, renderKey, built) {
    if (!tokenCurrent(token) || !layerActive) return;
    ensureInteractivePane();
    setRenderStatus('Drawing visible spring locations…', true);

    var previousRoot = currentRoot;
    if (previousRoot) {
      try {
        if (map.hasLayer(previousRoot)) map.removeLayer(previousRoot);
      } catch (removePreviousErr) {}
    }

    var root = L.layerGroup();
    currentRoot = root;
    currentRootComplete = false;
    currentRenderKey = renderKey;
    root.addTo(map);
    if (previousRoot && previousRoot !== root) disposeRootLater(previousRoot);

    var position = 0;
    var mounted = 0;
    var aggregateObjects = 0;
    var exactObjects = 0;

    function step() {
      if (!tokenCurrent(token) || !layerActive || currentRoot !== root) {
        try { if (map.hasLayer(root)) map.removeLayer(root); } catch (removeErr) {}
        if (currentRoot === root) {
          currentRoot = null;
          currentRootComplete = false;
          currentRenderKey = '';
        }
        disposeRootLater(root);
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
        var layer = createLayer(descriptor);
        if (!layer) continue;
        root.addLayer(layer);
        mounted += 1;
        batch += 1;
        if (descriptor.type === 'aggregate') aggregateObjects += 1;
        else exactObjects += 1;
      }

      if (position < built.descriptors.length) {
        requestFrame(step);
        return;
      }

      currentRootComplete = true;
      lastDrawn = mounted;
      lastVisibleRecords = built.visibleRecords;
      lastVisibleLocations = built.visibleLocations;
      lastVisibleRecordIndices = built.visibleRecordIndices;
      lastAggregateObjects = aggregateObjects;
      lastExactObjects = exactObjects;
      loading = false;
      rebuildLabels();
      setRenderStatus('Springs drawn.', false, 900);
      profileRecord('render-complete', {
        renderKey: renderKey,
        stats: controllerStats()
      });
    }

    requestFrame(step);
  }

  function viewportKey() {
    var bounds = map.getBounds();
    return [
      Number(map.getZoom()),
      Number(bounds.getWest()).toFixed(4),
      Number(bounds.getSouth()).toFixed(4),
      Number(bounds.getEast()).toFixed(4),
      Number(bounds.getNorth()).toFixed(4),
      filterSignature()
    ].join('|');
  }

  function beginRender(reason) {
    if (!layerActive || destroyed) return;
    var renderKey = viewportKey();
    if (
      currentRootComplete &&
      currentRoot &&
      currentRenderKey === renderKey
    ) {
      if (!map.hasLayer(currentRoot)) currentRoot.addTo(map);
      loading = false;
      rebuildLabels();
      setRenderStatus('Springs restored.', false, 700);
      profileRecord('display-cache-restored', {renderKey: renderKey});
      return;
    }

    var token = nextGeneration('render:' + reason);
    loading = true;
    setRenderStatus('Preparing spring records…', true);
    ensureSpatialIndex(token, function() {
      if (!tokenCurrent(token) || !layerActive) return;
      buildFilterMask(token, function() {
        if (!tokenCurrent(token) || !layerActive) return;
        var locationIds = queryLocations();
        lastQueriedLocations = locationIds.length;
        buildDescriptors(token, locationIds, function(built) {
          if (!tokenCurrent(token) || !layerActive) return;
          mountDescriptors(token, renderKey, built);
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
    clearLabels();
    setRenderStatus('Waiting for basemap; updating springs…', true);
    detachCurrent(currentRootComplete);
  }

  function activate(reason) {
    if (destroyed) return;
    layerActive = true;
    legendUserHidden = false;
    scheduleRender(reason || 'activate', 0);
    updateLegend();
  }

  function deactivate(dropDisplayCache) {
    layerActive = false;
    hardClearDisplay(!!dropDisplayCache);
    updateLegend();
  }

  function syncActive() {
    if (destroyed) return;
    var checked = safeControlScan(false);
    var nextActive = checked === null ? layerActive : checked;
    var labelChecked = safeControlScan(true);
    if (labelChecked !== null) labelsActive = labelChecked;

    if (nextActive && !layerActive) {
      activate('checkbox');
    } else if (!nextActive && layerActive) {
      deactivate(false);
    } else if (
      nextActive &&
      layerActive &&
      (!currentRoot || !map.hasLayer(currentRoot))
    ) {
      scheduleRender('sync', 0);
    }

    if (!labelsActive || !layerActive) clearLabels();
    else rebuildLabels();
    updateLegend();
  }

  function applyFilters(filters) {
    currentFilters = normalizedFilters(filters);
    filterMaskReady = false;
    nextGeneration('filter-change');
    clearLabels();
    detachCurrent(false);
    if (layerActive) scheduleRender('filter', 0);
    updateLegend();
    return controllerStats();
  }

  function resetFilters() {
    return applyFilters(defaultFilters());
  }

  function sourceCount(key) {
    var counts = metadata.sourceCounts || {};
    if (key === 'nhd') return Number(counts.nhd || 0);
    if (key === 'survey') return Number(counts.survey_2015_16 || 0);
    var known = Number(counts.nhd || 0) +
      Number(counts.survey_2015_16 || 0);
    return Math.max(0, recordCount - known);
  }

  function buttonHtml(kind, value, label) {
    var active = false;
    if (kind === 'source') active = currentFilters.source === value;
    if (kind === 'blm') {
      if (value === 'any') active = currentFilters.blmMode === 'any';
      else if (value === 'on') active = currentFilters.blmMode === 'on';
      else if (value === 'off') active = currentFilters.blmMode === 'off';
      else {
        active = (
          currentFilters.blmMode === 'distance' &&
          String(currentFilters.blmMax) === String(value)
        );
      }
    }
    return (
      '<button type="button" class="pt-springs-filter-btn' +
      (active ? ' active' : '') +
      '" data-kind="' + esc(kind) +
      '" data-value="' + esc(value) + '">' +
      esc(label) +
      '</button>'
    );
  }

  function symbolRow(symbol, label, count) {
    return (
      '<div class="pt-springs-local-row">' +
      symbol +
      '<span class="pt-springs-local-label">' + esc(label) + '</span>' +
      '<span class="pt-springs-local-count">' + fmt(count) + '</span>' +
      '</div>'
    );
  }

  function legendHtml() {
    var html = '';
    html += '<button type="button" class="pt-springs-local-close" title="Hide legend">&times;</button>';
    html += '<div class="pt-springs-local-title">Springs</div>';
    html += symbolRow(
      '<span class="pt-springs-local-dot"></span>',
      'NHD spring point',
      sourceCount('nhd')
    );
    html += symbolRow(
      '<span class="pt-springs-local-ring"></span>',
      '2015–16 Mojave survey',
      sourceCount('survey')
    );
    if (sourceCount('other') > 0) {
      html += symbolRow(
        '<span class="pt-springs-local-dot other"></span>',
        'Other source',
        sourceCount('other')
      );
    }
    html += (
      '<div class="pt-springs-local-showing">Showing ' +
      fmt(lastFiltered) +
      ' / ' +
      fmt(recordCount) +
      ' spring record(s).</div>'
    );
    if (renderStatusText) {
      html += (
        '<div class="pt-springs-local-status' +
        (renderStatusBusy ? ' busy' : '') +
        '"><span class="pt-springs-local-spinner"></span>' +
        esc(renderStatusText) +
        '</div>'
      );
    }
    html += '<div class="pt-springs-filter-box">';
    html += (
      '<div class="pt-springs-filter-line">Source: ' +
      buttonHtml('source', 'all', 'all') +
      buttonHtml('source', 'nhd', 'NHD') +
      buttonHtml('source', 'survey', '2015–16') +
      '</div>'
    );
    html += (
      '<div class="pt-springs-filter-line">BLM max mi: ' +
      buttonHtml('blm', 'any', 'any') +
      buttonHtml('blm', 'on', 'on') +
      buttonHtml('blm', 'off', 'off') +
      buttonHtml('blm', '1', '≤1') +
      buttonHtml('blm', '5', '≤5') +
      '</div>'
    );
    html += (
      '<div class="pt-springs-local-note">' +
      'BLM distance is screening-only; verify points near boundaries. ' +
      'Spring name labels (lbl) display at zoom 12+.</div>'
    );
    html += '</div>';
    return html;
  }

  function legendDiv() {
    var container = map && map.getContainer ? map.getContainer() : el;
    var div = container && container.querySelector ?
      container.querySelector('.pt-springs-local-legend') :
      null;
    if (!div && el && el.querySelector) {
      div = el.querySelector('.pt-springs-local-legend');
    }
    return div;
  }

  function attachLegendEvents(div) {
    if (!div || !div.querySelectorAll) return;
    if (window.BRIM && window.BRIM.legendCloseout) {
      window.BRIM.legendCloseout.wire(
        div,
        '.pt-springs-local-close',
        function() { legendUserHidden = true; }
      );
    } else {
      var closeButton = div.querySelector('.pt-springs-local-close');
      if (closeButton) {
        closeButton.onclick = function(event) {
          if (event) {
            event.preventDefault();
            event.stopPropagation();
          }
          legendUserHidden = true;
          div.style.display = 'none';
        };
      }
    }

    var buttons = div.querySelectorAll('.pt-springs-filter-btn');
    for (var index = 0; index < buttons.length; index += 1) {
      buttons[index].onclick = function(event) {
        if (event) {
          event.preventDefault();
          event.stopPropagation();
        }
        var kind = this.getAttribute('data-kind');
        var value = this.getAttribute('data-value');
        var next = normalizedFilters(currentFilters);
        if (kind === 'source') next.source = value || 'all';
        if (kind === 'blm') {
          if (value === 'any') {
            next.blmMode = 'any';
            next.blmMax = null;
          } else if (value === 'on' || value === 'off') {
            next.blmMode = value;
            next.blmMax = null;
          } else {
            next.blmMode = 'distance';
            next.blmMax = Number(value);
          }
        }
        applyFilters(next);
      };
    }
  }

  function updateLegend() {
    var div = legendDiv();
    if (!div) return;
    if (!layerActive) {
      div.style.display = 'none';
      legendUserHidden = false;
      return;
    }
    div.innerHTML = legendHtml();
    div.style.display = legendUserHidden ? 'none' : 'block';
    attachLegendEvents(div);
  }

  function installLegend() {
    if (!L.control || !L.DomUtil) return;
    legend = L.control({position: 'bottomleft'});
    legend.onAdd = function() {
      var div = L.DomUtil.create(
        'div',
        'leaflet-control pt-springs-local-legend'
      );
      div.style.display = 'none';
      div.style.marginBottom = '74px';
      if (L.DomEvent) {
        L.DomEvent.disableClickPropagation(div);
        L.DomEvent.disableScrollPropagation(div);
      }
      div.innerHTML = legendHtml();
      attachLegendEvents(div);
      return div;
    };
    legend.addTo(map);
  }

  function controllerStats() {
    var container = map && map.getContainer ? map.getContainer() : null;
    return {
      analyticalRecords: Number(
        metadata.analyticalRecordCount || recordCount
      ),
      validCoordinates: Number(
        metadata.validCoordinateCount || recordCount
      ),
      total: recordCount,
      uniqueCoordinates: locationCount,
      duplicateLocations: Number(metadata.duplicateLocationCount || 0),
      recordsAtDuplicateLocations: Number(
        metadata.recordsAtDuplicateLocations || 0
      ),
      maxRecordsAtLocation: Number(metadata.maxRecordsAtLocation || 1),
      filtered: lastFiltered,
      drawn: lastDrawn,
      visibleRecords: lastVisibleRecords,
      visibleLocations: lastVisibleLocations,
      queriedLocations: lastQueriedLocations,
      aggregateObjects: lastAggregateObjects,
      exactObjects: lastExactObjects,
      labelObjects: lastLabelObjects,
      leafletDisplayObjects: lastDrawn,
      markerDomElements: container && container.querySelectorAll ?
        container.querySelectorAll(
          '.pt-springs-aggregate-marker,.pt-springs-multi-marker'
        ).length :
        0,
      active: layerActive,
      labelsActive: labelsActive,
      loading: loading || renderStatusBusy,
      status: renderStatusText,
      indexReady: !!gridIndex,
      filterIndexReady: !!filterMaskReady,
      generation: generation,
      displayCacheReady: !!(currentRoot && currentRootComplete),
      deferredCleanupCount: deferredCleanupCount,
      clusterToExactZoom: clusterToExactZoom,
      interactivePane: interactivePaneName,
      interactivePaneZIndex: interactivePaneZIndex,
      mapMoving: mapMoving,
      mapZooming: mapZooming,
      filters: normalizedFilters(currentFilters),
      approximatePayloadBytes: Number(metadata.approximatePayloadBytes || 0)
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
    if (
      typeof document === 'undefined' ||
      !document.head ||
      document.getElementById('pt-springs-virtualized-style')
    ) return;
    var style = document.createElement('style');
    style.id = 'pt-springs-virtualized-style';
    style.textContent =
      '.leaflet-tooltip.pt-springs-local-tooltip{font:12px/1.25 Arial,sans-serif;white-space:normal;min-width:150px;max-width:340px;}' +
      '.pt-springs-aggregate-divicon,.pt-springs-multi-divicon{background:transparent;border:0;pointer-events:auto;}' +
      '.pt-springs-aggregate-marker{position:relative;width:100%;height:100%;border-radius:50%;box-sizing:border-box;display:flex;align-items:center;justify-content:center;border:2px solid rgba(90,58,15,.82);background:rgba(221,211,173,.94);box-shadow:0 1px 5px rgba(0,0,0,.30);color:#3a240d;font:700 11px/1 Arial,sans-serif;}' +
      '.pt-springs-aggregate-marker:after{content:"";position:absolute;inset:4px;border-radius:50%;border:1px solid rgba(255,255,255,.78);pointer-events:none;}' +
      '.pt-springs-multi-marker{position:relative;width:22px;height:22px;border-radius:50%;box-shadow:0 1px 4px rgba(0,0,0,.28);box-sizing:border-box;}' +
      '.pt-springs-multi-marker.nhd{border:1px solid #fff;background:#2B6CB0;box-shadow:0 1px 4px rgba(0,0,0,.28),inset 0 0 0 1px rgba(255,255,255,.42);}' +
      '.pt-springs-multi-marker.survey{border:3px solid #E6550D;background:rgba(255,255,255,.88);}' +
      '.pt-springs-multi-marker.mixed{border:2px solid #E6550D;background:#2B6CB0;box-shadow:0 1px 4px rgba(0,0,0,.28),inset 0 0 0 2px #fff;}' +
      '.pt-springs-multi-marker span{position:absolute;right:-7px;top:-8px;min-width:14px;height:14px;border-radius:8px;background:#5a3a0f;border:1px solid #fff;color:#fff;font:700 8.5px/13px Arial,sans-serif;text-align:center;padding:0 2px;box-sizing:border-box;}' +
      '.pt-springs-local-label-divicon{background:transparent;border:0;white-space:nowrap;pointer-events:none;}' +
      '.pt-springs-local-label-divicon span{display:inline-block;transform:translate(-50%,-13px);font:700 11px/1.1 Arial,sans-serif;color:#5a3a0f;background:rgba(255,255,255,.64);border:1px solid rgba(90,58,15,.22);border-radius:3px;padding:1px 3px;text-shadow:0 1px 2px #fff,1px 0 2px #fff,-1px 0 2px #fff,0 -1px 2px #fff;box-shadow:0 1px 2px rgba(0,0,0,.10);max-width:190px;overflow:hidden;text-overflow:ellipsis;}' +
      '.pt-springs-local-legend{background:rgba(246,239,222,.96);border:1px solid rgba(112,103,83,.55);border-radius:6px;box-shadow:0 1px 5px rgba(0,0,0,.25);padding:7px 9px 8px 9px;width:255px;max-width:255px;font:11.5px/1.22 Arial,sans-serif;color:#222;position:relative;margin-bottom:74px;}' +
      '.pt-springs-local-title{font-weight:700;font-size:12px;margin:0 18px 4px 0;}' +
      '.pt-springs-local-close{position:absolute;top:3px;right:5px;border:0;background:transparent;color:#555;font-size:16px;line-height:16px;cursor:pointer;padding:0 2px;}' +
      '.pt-springs-local-row{display:flex;align-items:center;gap:5px;margin:2px 0;}' +
      '.pt-springs-local-label{flex:1;min-width:0;}' +
      '.pt-springs-local-count{font-variant-numeric:tabular-nums;color:#555;}' +
      '.pt-springs-local-dot{display:inline-block;width:9px;height:9px;border-radius:50%;border:1px solid #fff;background:#2B6CB0;box-sizing:border-box;}' +
      '.pt-springs-local-dot.other{background:#777;border-color:#333;}' +
      '.pt-springs-local-ring{display:inline-block;width:12px;height:12px;border-radius:50%;border:2px solid #E6550D;background:transparent;box-sizing:border-box;}' +
      '.pt-springs-filter-box{border-top:1px solid rgba(112,103,83,.25);margin-top:5px;padding-top:5px;}' +
      '.pt-springs-filter-line{margin:3px 0;}' +
      '.pt-springs-filter-btn{border:1px solid rgba(112,103,83,.65);background:rgba(255,255,255,.92);border-radius:5px;padding:2px 5px;margin:1px 2px 1px 0;font:11px/1.1 Arial,sans-serif;cursor:pointer;}' +
      '.pt-springs-filter-btn.active{background:rgba(221,211,173,.98);color:#222;border-color:rgba(112,103,83,.75);font-weight:700;}' +
      '.pt-springs-local-showing{margin-top:5px;color:#444;font-size:10.5px;}' +
      '.pt-springs-local-note{margin-top:4px;color:#666;font-size:10px;}' +
      '.pt-springs-local-status{display:flex;align-items:center;gap:5px;font-size:10px;color:#5a4a24;background:rgba(255,248,220,.78);border:1px solid rgba(155,134,74,.42);border-radius:5px;padding:2px 5px;margin:3px 0 2px 0;}' +
      '.pt-springs-local-spinner{width:9px;height:9px;border:2px solid rgba(90,58,15,.22);border-top-color:#5a3a0f;border-radius:50%;display:none;box-sizing:border-box;animation:pt-springs-spin .8s linear infinite;}' +
      '.pt-springs-local-status.busy .pt-springs-local-spinner{display:inline-block;flex:0 0 auto;}' +
      '@keyframes pt-springs-spin{to{transform:rotate(360deg);}}' +
      '.pt-springs-popup-record{font:12px/1.35 Arial,Helvetica,sans-serif;max-width:380px;}' +
      '.pt-springs-popup-record.compact{padding:6px 0;border-top:1px solid #ddd;}' +
      '.pt-springs-popup-title{font-weight:700;font-size:13px;margin-bottom:3px;}' +
      '.pt-springs-popup-links{margin-top:5px;}' +
      '.pt-springs-multi-title{font:700 13px/1.25 Arial,sans-serif;margin-bottom:2px;}' +
      '.pt-springs-multi-note{font:11px/1.25 Arial,sans-serif;color:#555;margin-bottom:4px;}' +
      '.pt-springs-multi-list{max-height:390px;overflow:auto;padding-right:4px;}';
    document.head.appendChild(style);
  }

  function installHomeGuard() {
    if (destroyed) return;
    if (typeof document === 'undefined' || !document.getElementById) return;
    homeButton = document.getElementById('pt-reset-zoom-btn');
    if (!homeButton || homeButton.__ptSpringsVirtualHomeGuard) return;
    homeButton.__ptSpringsVirtualHomeGuard = true;
    homeButtonHandler = function() {
      if (layerActive) suspendForMovement('home');
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

  function documentClickHandler(event) {
    var target = event && event.target && event.target.closest ?
      event.target.closest('.pt-main-layer-clear-btn,#pt-clear-all-btn') :
      null;
    if (!target) return;
    currentFilters = defaultFilters();
    filterMask = null;
    filterMaskReady = true;
    lastFiltered = recordCount;
    deactivate(true);
    profileRecord('explicit-clear-click', {
      id: target.id || '',
      className: target.className || ''
    });
  }

  function profileInitiallyEnabled() {
    if (window.BRIM_ENABLE_SPRINGS_PROFILE) return true;
    try {
      return new URLSearchParams(window.location.search)
        .get('brimProfile') === '1';
    } catch (queryErr) {
      return false;
    }
  }

  function profileRecord(type, details) {
    if (!profileEnabled) return;
    profileEvents.push({
      type: type,
      atMs: now(),
      details: details || {}
    });
  }

  function installProfiler() {
    if (
      !profileEnabled ||
      longTaskObserver ||
      !window.PerformanceObserver
    ) return;
    try {
      longTaskObserver = new window.PerformanceObserver(function(list) {
        list.getEntries().forEach(function(entry) {
          profileLongTasks.push({
            startTime: entry.startTime,
            duration: entry.duration
          });
        });
      });
      longTaskObserver.observe({entryTypes: ['longtask']});
    } catch (observerErr) {
      longTaskObserver = null;
    }
  }

  function profileSnapshot(label) {
    var snapshot = {
      label: label || '',
      atMs: now(),
      stats: controllerStats()
    };
    if (window.performance && window.performance.memory) {
      snapshot.memory = {
        usedJSHeapSize: window.performance.memory.usedJSHeapSize,
        totalJSHeapSize: window.performance.memory.totalJSHeapSize,
        jsHeapSizeLimit: window.performance.memory.jsHeapSizeLimit
      };
    }
    profileRecord('snapshot', snapshot);
    return snapshot;
  }

  function startScenario(name) {
    profileScenario = {
      name: String(name || 'springs-scenario'),
      startedAtMs: now(),
      start: profileSnapshot('start')
    };
    return profileScenario;
  }

  function endScenario() {
    if (!profileScenario) return null;
    profileScenario.endedAtMs = now();
    profileScenario.durationMs =
      profileScenario.endedAtMs - profileScenario.startedAtMs;
    profileScenario.end = profileSnapshot('end');
    profileScenario.longTasks = profileLongTasks.slice();
    profileScenario.events = profileEvents.slice();
    return profileScenario;
  }

  function profileReport() {
    return {
      metadata: metadata,
      scenario: profileScenario,
      events: profileEvents.slice(),
      longTasks: profileLongTasks.slice(),
      current: profileSnapshot('report')
    };
  }

  function downloadProfileJson() {
    var report = JSON.stringify(profileReport(), null, 2);
    if (!window.Blob || !window.URL || !document.createElement) return report;
    var blob = new Blob([report], {type: 'application/json'});
    var url = window.URL.createObjectURL(blob);
    var link = document.createElement('a');
    link.href = url;
    link.download = 'brim_springs_profile.json';
    document.body.appendChild(link);
    link.click();
    link.remove();
    setTimeout(function() { window.URL.revokeObjectURL(url); }, 0);
    return report;
  }

  function destroy() {
    if (destroyed) return;
    destroyed = true;
    nextGeneration('destroy');
    clearLabels();
    detachCurrent(false);
    listenerRecords.forEach(function(record) {
      try {
        record.target.off(record.eventName, record.handler);
      } catch (listenerErr) {}
    });
    listenerRecords = [];
    document.removeEventListener('change', documentChangeHandler, true);
    document.removeEventListener('click', documentClickHandler, true);
    if (homeButton && homeButtonHandler) {
      homeButton.removeEventListener('click', homeButtonHandler, true);
      delete homeButton.__ptSpringsVirtualHomeGuard;
    }
    if (interactivePaneObserver) {
      try { interactivePaneObserver.disconnect(); } catch (observerErr) {}
      interactivePaneObserver = null;
    }
    if (longTaskObserver) {
      try { longTaskObserver.disconnect(); } catch (profileObserverErr) {}
      longTaskObserver = null;
    }
    if (legend && map.removeControl) {
      try { map.removeControl(legend); } catch (legendErr) {}
    }
    if (window.BRIM_SPRINGS_LOCAL === controllerApi) {
      delete window.BRIM_SPRINGS_LOCAL;
    }
    if (window.BRIM_SPRINGS_PROFILE === profileApi) {
      delete window.BRIM_SPRINGS_PROFILE;
    }
  }

  installStyles();
  installInteractivePaneObserver();
  installLegend();
  installProfiler();

  var profileApi = {
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
    snapshot: profileSnapshot,
    endScenario: endScenario,
    report: profileReport,
    downloadJson: downloadProfileJson,
    metadata: function() { return metadata; }
  };
  window.BRIM_SPRINGS_PROFILE = profileApi;

  var controllerApi = {
    applyFilters: applyFilters,
    resetFilters: resetFilters,
    setActive: function(active) {
      if (active) activate('api');
      else deactivate(false);
    },
    clear: function() {
      currentFilters = defaultFilters();
      filterMask = null;
      filterMaskReady = true;
      lastFiltered = recordCount;
      deactivate(true);
    },
    refresh: function() {
      if (!layerActive) return;
      nextGeneration('api-refresh');
      clearLabels();
      detachCurrent(false);
      scheduleRender('api-refresh', 0);
    },
    stats: controllerStats,
    multiRecordLocations: function() {
      if (!currentRoot || !currentRoot.eachLayer) return [];
      var result = [];
      currentRoot.eachLayer(function(layer) {
        if (layer._ptSpringsMulti) result.push(layer._ptSpringsMulti);
      });
      return result;
    },
    destroy: destroy
  };
  window.BRIM_SPRINGS_LOCAL = controllerApi;

  onMap('overlayadd', function(event) {
    if (eventMatches(event)) {
      currentFilters = defaultFilters();
      filterMask = null;
      filterMaskReady = true;
      lastFiltered = recordCount;
      activate('overlayadd');
    }
    if (labelEventMatches(event)) {
      labelsActive = true;
      rebuildLabels();
    }
  });

  onMap('overlayremove', function(event) {
    if (eventMatches(event)) {
      currentFilters = defaultFilters();
      filterMask = null;
      filterMaskReady = true;
      lastFiltered = recordCount;
      deactivate(false);
    }
    if (labelEventMatches(event)) {
      labelsActive = false;
      clearLabels();
    }
  });

  onMap('zoomstart', function() {
    mapZooming = true;
    suspendForMovement('zoomstart');
  });

  onMap('zoomend', function() {
    mapZooming = false;
    scheduleRender('zoomend', 52);
  });

  onMap('movestart', function() {
    mapMoving = true;
    if (!mapZooming) suspendForMovement('movestart');
  });

  onMap('moveend', function() {
    mapMoving = false;
    scheduleRender('moveend', 52);
  });

  onMap('pt:marqueezoomstart', function() {
    mapMoving = true;
    mapZooming = true;
    suspendForMovement('marquee');
  });

  onMap('pt:marqueezoomend', function() {
    mapMoving = false;
    mapZooming = false;
    scheduleRender('marquee-end', 52);
  });

  onMap('pt:measureinteractionchange', function() {
    syncInteractivePanePointerState();
  });

  document.addEventListener('change', documentChangeHandler, true);
  document.addEventListener('click', documentClickHandler, true);

  setTimeout(syncActive, 0);
  setTimeout(syncActive, 500);
  setTimeout(installHomeGuard, 0);
  setTimeout(installHomeGuard, 800);
  setTimeout(installHomeGuard, 1800);
}
