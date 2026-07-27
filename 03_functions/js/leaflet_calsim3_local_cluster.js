function(el, x, calsimData) {
  if (
    window.BRIM_CALSIM3_LOCAL &&
    typeof window.BRIM_CALSIM3_LOCAL.destroy === 'function'
  ) {
    try {
      window.BRIM_CALSIM3_LOCAL.destroy(true);
    } catch (oldControllerError) {
      console.warn(
        'BRIM CalSim3 controller: prior controller cleanup failed.',
        oldControllerError
      );
    }
  }

  var map = this;
  calsimData = calsimData || {};

  var groupName = String(calsimData.groupName || 'Channels – CalSim3.0');
  var labelGroupName = Object.prototype.hasOwnProperty.call(
    calsimData,
    'labelGroupName'
  ) ? String(calsimData.labelGroupName || '') : 'Labels – CalSim3.0';
  var clusterId = String(
    calsimData.clusterId || 'pt_calsim3_nodes_cluster'
  );
  var calsimPaneName = String(calsimData.paneName || 'pane_calsim3');
  var canvasTolerance = Number(calsimData.canvasTolerance || 6);
  var transitionZoom = Number(calsimData.transitionZoom || 9);
  var labelMinZoom = Number(calsimData.labelMinZoom || 11);
  var labelCap = Number(calsimData.labelCap || 160);
  var labelNodeCap = Number(calsimData.labelNodeCap || 96);
  var labelArcCap = Number(calsimData.labelArcCap || 64);
  var labelGridDegrees = Number(calsimData.labelGridDegrees || 0.25);
  var labelViewportPadRatio = Number(
    calsimData.labelViewportPadRatio || 0.15
  );
  labelCap = Math.max(1, Math.floor(labelCap));
  labelNodeCap = Math.max(0, Math.floor(labelNodeCap));
  labelArcCap = Math.max(0, Math.floor(labelArcCap));
  if (!(labelGridDegrees > 0)) labelGridDegrees = 0.25;
  if (!(labelViewportPadRatio >= 0)) labelViewportPadRatio = 0.15;
  var finderInitialLimit = 12;
  var finderBatchSize = 12;
  var finderShowAllMaximum = 200;
  var hasExactDuplicateLocations =
    Number(calsimData.exactDuplicateLocationCount || 0) > 0;
  var listenerRecords = [];
  var initialTimers = [];
  var destroyed = false;
  var currentTooltip = null;
  var currentPopup = null;
  var observedTooltip = null;
  var observedPopup = null;
  var currentHighlight = null;
  var focusTimer = null;
  var focusGeneration = 0;
  var card = null;
  var cardControl = null;
  var cardHiddenByUser = false;
  var lastFinderResults = [];
  var lastFinderQuery = '';
  var finderVisibleLimit = finderInitialLimit;
  var autoApply = true;
  var uiDirty = false;
  var applyFrame = null;
  var scheduledOptions = null;
  var applyScheduleGeneration = 0;
  var longTaskObserver = null;
  var profileEnabled = false;
  var activeScenario = null;
  var profileReports = [];
  var observedNetworkActive = null;
  var sharedPaneRenderer = null;
  var lastNodeRadiusTierKey = null;
  var appliedNodeRadiusTier = null;
  var labelsRequested = false;
  var labelDescriptors = [];
  var labelGrid = Object.create(null);
  var labelGridCellCount = 0;
  var labelLayerRoot = null;
  var visibleLabelDescriptors = [];
  var labelTimer = null;
  var labelFrame = null;
  var labelGeneration = 0;
  var labelMapMoving = false;
  var forwardingSharedRendererEvent = false;
  var lastForwardedPointerEvent = null;
  var lastUnrelatedFeatureMoveEvent = null;
  var lastUnrelatedFeatureClickEvent = null;

  var diagnostics = {
    installedAt: Date.now(),
    activationCount: 0,
    deactivationCount: 0,
    filterApplyCount: 0,
    filterResetCount: 0,
    finderQueryCount: 0,
    finderFocusCount: 0,
    finderFilteredCount: 0,
    lowZoomClusterClicks: 0,
    exactLocationSpiderfyClicks: 0,
    ignoredClusterClicks: 0,
    tooltipOpenCount: 0,
    tooltipCloseCount: 0,
    popupOpenCount: 0,
    popupCloseCount: 0,
    unspiderfyCount: 0,
    profileLongTaskCount: 0,
    profileLongTaskDurationMs: 0,
    unresolvedArcObjects: 0,
    unresolvedNodeObjects: 0,
    applyGeneration: 0,
    autoApplyScheduleCount: 0,
    autoApplyRunCount: 0,
    cancelledApplyCount: 0,
    lastApplyDurationMs: 0,
    lastArcAddCount: 0,
    lastArcRemoveCount: 0,
    lastNodeAddCount: 0,
    lastNodeRemoveCount: 0,
    totalArcAddCount: 0,
    totalArcRemoveCount: 0,
    totalNodeAddCount: 0,
    totalNodeRemoveCount: 0,
    sharedRendererConfigureCount: 0,
    sharedRendererForwardedMoveCount: 0,
    sharedRendererForwardedClickCount: 0,
    sharedRendererBackgroundSkipCount: 0,
    sharedRendererSuppressedSkipCount: 0,
    nodeRadiusTierChangeCount: 0,
    nodeRadiusMarkerUpdateCount: 0,
    nodeRadiusRedundantSkipCount: 0,
    nodeRadiusInactiveSkipCount: 0,
    labelIndexBuildCount: 0,
    labelIndexDurationMs: 0,
    labelDescriptorCount: 0,
    labelScheduleCount: 0,
    labelRenderCount: 0,
    labelCancelCount: 0,
    labelRemoveCount: 0,
    labelControlChangeCount: 0,
    labelViewportCandidateCount: 0,
    labelEligibleCandidateCount: 0,
    labelVisibleCount: 0,
    labelCappedCount: 0,
    labelDuplicateNameSkipCount: 0
  };

  function rowsToArray(rows) {
    if (!rows) return [];
    if (Array.isArray(rows)) return rows.slice();
    if (typeof rows !== 'object') return [];
    var keys = Object.keys(rows);
    var n = 0;
    keys.forEach(function(key) {
      if (Array.isArray(rows[key])) {
        n = Math.max(n, rows[key].length);
      }
    });
    if (!n && keys.length) n = 1;
    var out = [];
    for (var i = 0; i < n; i += 1) {
      var row = {};
      keys.forEach(function(key) {
        row[key] = Array.isArray(rows[key]) ? rows[key][i] : rows[key];
      });
      out.push(row);
    }
    return out;
  }

  function valuesToArray(values) {
    if (Array.isArray(values)) return values.map(String);
    if (values === null || values === undefined || values === '') return [];
    if (typeof values === 'object') {
      return Object.keys(values).map(function(key) {
        return String(values[key]);
      });
    }
    return [String(values)];
  }

  function clean(value) {
    if (value === null || value === undefined) return '';
    var out = String(value).trim();
    return (
      out === 'NA' ||
      out === 'NaN' ||
      out === 'null' ||
      out === 'undefined'
    ) ? '' : out;
  }

  function esc(value) {
    return clean(value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function normalSearch(parts) {
    return parts.map(clean).join(' ').toLocaleLowerCase();
  }

  function asNumber(value, fallback) {
    var out = Number(value);
    return isFinite(out) ? out : fallback;
  }

  function countLabel(value) {
    return Number(value || 0).toLocaleString();
  }

  function setFrom(values) {
    var out = Object.create(null);
    (values || []).forEach(function(value) {
      out[String(value)] = true;
    });
    return out;
  }

  function selectedValues(selection, order) {
    return (order || []).filter(function(value) {
      return selection[value] === true;
    });
  }

  function copySelection(selection) {
    return setFrom(Object.keys(selection || {}).filter(function(value) {
      return selection[value] === true;
    }));
  }

  function usefulArcName(value) {
    var text = clean(value);
    var normalized = text.toLocaleLowerCase();
    return (
      text.length > 0 &&
      text.length <= 32 &&
      normalized !== '<null>' &&
      normalized !== 'unlabeled return flow'
    ) ? text : '';
  }

  function usefulEndpoint(value) {
    var text = clean(value);
    var normalized = text.toLocaleLowerCase();
    return (
      text &&
      normalized !== 'n/a' &&
      normalized !== 'na'
    ) ? text : '';
  }

  function arcLabelContent(record) {
    var name = usefulArcName(record && record.name);
    if (name) {
      return {
        text: name,
        rule: 'name',
        priority: 0
      };
    }
    var fromNode = usefulEndpoint(record && record.fromNode);
    var toNode = usefulEndpoint(record && record.toNode);
    var endpoints = fromNode && toNode ?
      fromNode + ' → ' + toNode : '';
    if (endpoints && endpoints.length <= 24) {
      return {
        text: endpoints,
        rule: 'explicit-endpoints',
        priority: 1
      };
    }
    var id = clean(record && record.id);
    if (id && id.length <= 20) {
      return {
        text: id,
        rule: 'arc-id',
        priority: 2
      };
    }
    return null;
  }

  function nodeLabelContent(record) {
    var id = clean(record && record.id);
    if (id && id.length <= 18) {
      return {
        text: id,
        rule: 'node-id',
        priority: 0
      };
    }
    return null;
  }

  var groupRoot = (
    map.layerManager &&
    typeof map.layerManager.getLayerGroup === 'function'
  ) ? map.layerManager.getLayerGroup(groupName, false) : null;

  var clusterGroup = (
    map.layerManager &&
    typeof map.layerManager.getLayer === 'function'
  ) ? map.layerManager.getLayer('cluster', clusterId) : null;

  var arcRecords = rowsToArray(calsimData.arcRecords);
  var nodeRecords = rowsToArray(calsimData.nodeRecords);
  var arcTypeOrder = valuesToArray(calsimData.arcTypeOrder);
  var nodeGroupOrder = valuesToArray(calsimData.nodeGroupOrder);
  var recordsByKey = Object.create(null);

  arcRecords.forEach(function(record, recordIndex) {
    record.kind = 'arc';
    record.lid = clean(record.lid);
    record.id = clean(record.id);
    record.fromNode = clean(record.fromNode);
    record.toNode = clean(record.toNode);
    record.name = clean(record.name);
    record.type = clean(record.type);
    record.color = clean(record.color) || '#777777';
    record.weight = asNumber(record.weight, 1.2);
    record.searchText = normalSearch([
      record.id,
      record.fromNode,
      record.toNode,
      record.name,
      record.type
    ]);
    record.idSearch = normalSearch([record.id]);
    record.primarySearchFields = [
      record.fromNode,
      record.toNode,
      record.name
    ].map(function(value) {
      return normalSearch([value]);
    }).filter(Boolean);
    record.secondarySearchFields = [
      normalSearch([record.type])
    ].filter(Boolean);
    record.searchOrder = recordIndex;
    record.key = 'arc:' + record.lid;
    record.labelContent = arcLabelContent(record);
    record.layer = (
      map.layerManager &&
      typeof map.layerManager.getLayer === 'function'
    ) ? map.layerManager.getLayer('shape', record.lid) : null;
    if (record.layer) {
      record.layer._brimCalsim3Component = 'arcs';
      record.layer._brimCalsim3RecordKey = record.key;
    } else {
      diagnostics.unresolvedArcObjects += 1;
    }
    recordsByKey[record.key] = record;
  });

  var nodeStore = (
    clusterGroup &&
    clusterGroup.clusterLayerStore &&
    clusterGroup.clusterLayerStore._layers
  ) ? clusterGroup.clusterLayerStore._layers : {};

  nodeRecords.forEach(function(record, recordIndex) {
    record.kind = 'node';
    record.lid = clean(record.lid);
    record.id = clean(record.id);
    record.description = clean(record.description);
    record.river = clean(record.river);
    record.comment = clean(record.comment);
    record.group = clean(record.group);
    record.fill = clean(record.fill) || '#BDBDBD';
    record.stroke = clean(record.stroke) || '#737373';
    record.radius = asNumber(record.radius, 4.7);
    record.searchText = normalSearch([
      record.id,
      record.description,
      record.river,
      record.comment,
      record.group
    ]);
    record.idSearch = normalSearch([record.id]);
    record.primarySearchFields = [
      record.description,
      record.river
    ].map(function(value) {
      return normalSearch([value]);
    }).filter(Boolean);
    record.secondarySearchFields = [
      record.comment,
      record.group
    ].map(function(value) {
      return normalSearch([value]);
    }).filter(Boolean);
    record.searchOrder = arcRecords.length + recordIndex;
    record.key = 'node:' + record.lid;
    record.labelContent = nodeLabelContent(record);
    record.layer = nodeStore[record.lid] || null;
    if (record.layer) {
      record.layer._brimCalsim3Component = 'nodes';
      record.layer._brimCalsim3RecordKey = record.key;
    } else {
      diagnostics.unresolvedNodeObjects += 1;
    }
    recordsByKey[record.key] = record;
  });

  var state = {
    showArcs: true,
    showNodes: true,
    arcTypes: setFrom(arcTypeOrder),
    nodeGroups: setFrom(nodeGroupOrder),
    hoverArcs: true,
    hoverNodes: true
  };

  function listen(target, type, handler) {
    if (!target || typeof target.on !== 'function') return;
    target.on(type, handler);
    listenerRecords.push({
      target: target,
      type: type,
      handler: handler
    });
  }

  function nowMs() {
    return (
      typeof performance !== 'undefined' &&
      typeof performance.now === 'function'
    ) ? performance.now() : Date.now();
  }

  function currentMapZoom() {
    return (
      map &&
      typeof map.getZoom === 'function'
    ) ? asNumber(map.getZoom(), transitionZoom) : transitionZoom;
  }

  function nodeRadiusTierForZoom(zoom) {
    var value = asNumber(zoom, transitionZoom);
    if (value < 10) {
      return {
        key: 'compact',
        scale: 0.58,
        minimumZoom: null,
        maximumZoom: 9.5
      };
    }
    if (value < 11) {
      return {
        key: 'medium',
        scale: 0.76,
        minimumZoom: 10,
        maximumZoom: 10.5
      };
    }
    return {
      key: 'full',
      scale: 1,
      minimumZoom: 11,
      maximumZoom: null
    };
  }

  function nodeRadiusForRecord(record, tier) {
    tier = tier || nodeRadiusTierForZoom(currentMapZoom());
    return asNumber(record && record.radius, 4.7) * tier.scale;
  }

  function setRetainedNodeRadius(record, radius) {
    if (!record || !record.layer) return false;
    var current = NaN;
    if (typeof record.layer.getRadius === 'function') {
      try {
        current = Number(record.layer.getRadius());
      } catch (nodeRadiusReadError) {}
    }
    record.displayRadius = radius;
    if (isFinite(current) && Math.abs(current - radius) < 0.0001) {
      return false;
    }
    if (typeof record.layer.setRadius === 'function') {
      record.layer.setRadius(radius);
      return true;
    }
    return false;
  }

  function applyNodeRadiusTier(source) {
    var zoom = currentMapZoom();
    var tier = nodeRadiusTierForZoom(zoom);
    diagnostics.lastNodeRadiusRequestedSource = source || null;
    diagnostics.lastNodeRadiusRequestedZoom = zoom;
    if (!networkActive()) {
      lastNodeRadiusTierKey = null;
      appliedNodeRadiusTier = null;
      diagnostics.nodeRadiusInactiveSkipCount += 1;
      return {
        changed: false,
        reason: 'inactive',
        tier: tier
      };
    }
    if (lastNodeRadiusTierKey === tier.key) {
      diagnostics.nodeRadiusRedundantSkipCount += 1;
      return {
        changed: false,
        reason: 'same-tier',
        tier: tier
      };
    }
    var updates = 0;
    nodeRecords.forEach(function(record) {
      if (setRetainedNodeRadius(
        record,
        nodeRadiusForRecord(record, tier)
      )) {
        updates += 1;
      }
    });
    lastNodeRadiusTierKey = tier.key;
    appliedNodeRadiusTier = tier;
    diagnostics.nodeRadiusTierChangeCount += 1;
    diagnostics.nodeRadiusMarkerUpdateCount += updates;
    diagnostics.lastNodeRadiusMarkerUpdateCount = updates;
    diagnostics.lastNodeRadiusAppliedSource = source || null;
    diagnostics.lastNodeRadiusAppliedZoom = zoom;
    diagnostics.lastNodeRadiusTierKey = tier.key;
    diagnostics.lastNodeRadiusScale = tier.scale;
    return {
      changed: true,
      reason: 'tier-change',
      updatedMarkers: updates,
      tier: tier
    };
  }

  function invalidateNodeRadiusTier(reason) {
    lastNodeRadiusTierKey = null;
    appliedNodeRadiusTier = null;
    diagnostics.lastNodeRadiusInvalidationReason = reason || null;
  }

  function nodeHighlightRadius(record) {
    var displayRadius = nodeRadiusForRecord(
      record,
      nodeRadiusTierForZoom(currentMapZoom())
    );
    return Math.max(displayRadius + 2.2, displayRadius * 1.5);
  }

  function requestFrame(callback) {
    if (
      typeof window !== 'undefined' &&
      typeof window.requestAnimationFrame === 'function'
    ) {
      return window.requestAnimationFrame(callback);
    }
    return setTimeout(callback, 0);
  }

  function cancelFrame(handle) {
    if (handle === null) return;
    if (
      typeof window !== 'undefined' &&
      typeof window.cancelAnimationFrame === 'function'
    ) {
      window.cancelAnimationFrame(handle);
    } else {
      clearTimeout(handle);
    }
  }

  function normalizedControlGroup(value) {
    return clean(value)
      .replace(/\s+/g, ' ')
      .replace(
        /^(Ops|Core|Basins|Points|Channels|Reference|Labels)\s+[–-]\s+/i,
        ''
      )
      .replace(/\s*\([^)]*\)\s*$/g, '')
      .replace(/\s+lbl\s*$/i, '')
      .toLocaleLowerCase();
  }

  function controlInputForGroup(group, wantLabelRow) {
    if (
      !group ||
      typeof document === 'undefined' ||
      typeof document.querySelectorAll !== 'function'
    ) {
      return null;
    }
    var target = normalizedControlGroup(group);
    var labels = document.querySelectorAll(
      '.leaflet-control-layers-overlays label'
    );
    for (var i = 0; i < labels.length; i += 1) {
      var label = labels[i];
      var full = label.getAttribute ?
        clean(label.getAttribute('data-pt-layer-full-name')) : '';
      var text = full || clean(label.textContent || label.innerText);
      var isLabelRow = /^Labels\s+[–-]\s+/i.test(text);
      if (wantLabelRow === true && !isLabelRow) continue;
      if (wantLabelRow === false && isLabelRow) continue;
      if (normalizedControlGroup(text) !== target) continue;
      return label.querySelector ?
        label.querySelector('input[type="checkbox"]') : null;
    }
    return null;
  }

  function sharedLabelControlInput() {
    return controlInputForGroup(labelGroupName, true);
  }

  function updateExplorerLabelControl() {
    if (!card) return;
    var input = card.querySelector('.pt-calsim3-labels');
    if (!input) return;
    input.checked = labelsRequested;
    input.disabled = !labelGroupName || !networkActive();
  }

  function removeVisibleLabels(reason) {
    var hadLabels = visibleLabelDescriptors.length > 0;
    if (
      labelLayerRoot &&
      map &&
      typeof map.hasLayer === 'function' &&
      map.hasLayer(labelLayerRoot) &&
      typeof map.removeLayer === 'function'
    ) {
      map.removeLayer(labelLayerRoot);
    }
    if (
      labelLayerRoot &&
      typeof labelLayerRoot.clearLayers === 'function'
    ) {
      labelLayerRoot.clearLayers();
    }
    visibleLabelDescriptors = [];
    diagnostics.labelVisibleCount = 0;
    diagnostics.lastLabelRemoveReason = reason || null;
    if (hadLabels) diagnostics.labelRemoveCount += 1;
  }

  function cancelLabelWork(reason, removeLabels) {
    labelGeneration += 1;
    var cancelled = false;
    if (labelTimer !== null) {
      clearTimeout(labelTimer);
      labelTimer = null;
      cancelled = true;
    }
    if (labelFrame !== null) {
      cancelFrame(labelFrame);
      labelFrame = null;
      cancelled = true;
    }
    if (cancelled) diagnostics.labelCancelCount += 1;
    diagnostics.lastLabelCancelReason = reason || null;
    if (removeLabels === true) removeVisibleLabels(reason);
  }

  function setLabelsRequestedState(enabled, reason) {
    enabled = enabled === true && !!labelGroupName;
    var changed = labelsRequested !== enabled;
    if (changed) {
      diagnostics.labelControlChangeCount += 1;
    }
    labelsRequested = enabled;
    diagnostics.lastLabelControlReason = reason || null;
    updateExplorerLabelControl();
    if (enabled) {
      if (
        changed ||
        (
          !visibleLabelDescriptors.length &&
          labelTimer === null &&
          labelFrame === null &&
          labelsEligible()
        )
      ) {
        scheduleLabelRender(reason || 'labels-on', 0);
      }
    } else if (
      changed ||
      visibleLabelDescriptors.length ||
      labelTimer !== null ||
      labelFrame !== null
    ) {
      cancelLabelWork(reason || 'labels-off', true);
    }
    return labelsRequested;
  }

  function syncLabelsFromSharedControl(reason, fallback) {
    var input = sharedLabelControlInput();
    return setLabelsRequestedState(
      input ? !!input.checked : fallback === true,
      reason
    );
  }

  function setLabelsRequested(enabled, reason) {
    enabled = enabled === true;
    if (enabled && !networkActive()) enabled = false;
    var input = sharedLabelControlInput();
    if (
      input &&
      !!input.checked !== enabled &&
      typeof input.click === 'function'
    ) {
      input.click();
    }
    return syncLabelsFromSharedControl(
      reason || 'label-control',
      input ? !!input.checked : enabled
    );
  }

  function labelAnchorForRecord(record) {
    if (!record || !record.layer) return null;
    try {
      if (
        record.kind === 'node' &&
        typeof record.layer.getLatLng === 'function'
      ) {
        var nodeLatLng = record.layer.getLatLng();
        if (
          nodeLatLng &&
          isFinite(Number(nodeLatLng.lat)) &&
          isFinite(Number(nodeLatLng.lng))
        ) {
          return {
            lat: Number(nodeLatLng.lat),
            lng: Number(nodeLatLng.lng)
          };
        }
      }
      if (record.kind === 'arc') {
        if (typeof record.layer.getCenter === 'function') {
          var lineCenter = record.layer.getCenter();
          if (
            lineCenter &&
            isFinite(Number(lineCenter.lat)) &&
            isFinite(Number(lineCenter.lng))
          ) {
            return {
              lat: Number(lineCenter.lat),
              lng: Number(lineCenter.lng)
            };
          }
        }
        if (typeof record.layer.getBounds === 'function') {
          var bounds = record.layer.getBounds();
          var center = bounds && typeof bounds.getCenter === 'function' ?
            bounds.getCenter() : null;
          if (
            center &&
            isFinite(Number(center.lat)) &&
            isFinite(Number(center.lng))
          ) {
            return {
              lat: Number(center.lat),
              lng: Number(center.lng)
            };
          }
        }
      }
    } catch (labelAnchorError) {}
    return null;
  }

  function labelGridKey(lat, lng) {
    return (
      Math.floor(Number(lng) / labelGridDegrees) + ':' +
      Math.floor(Number(lat) / labelGridDegrees)
    );
  }

  function buildLabelIndex() {
    var startedAt = nowMs();
    labelDescriptors = [];
    labelGrid = Object.create(null);
    labelGridCellCount = 0;
    arcRecords.concat(nodeRecords).forEach(function(record) {
      if (!record.labelContent) return;
      var anchor = labelAnchorForRecord(record);
      if (!anchor) return;
      var descriptor = {
        key: record.key,
        kind: record.kind,
        text: record.labelContent.text,
        rule: record.labelContent.rule,
        priority: record.labelContent.priority,
        sourceOrder: record.searchOrder,
        lat: anchor.lat,
        lng: anchor.lng,
        record: record
      };
      var key = labelGridKey(anchor.lat, anchor.lng);
      if (!labelGrid[key]) {
        labelGrid[key] = [];
        labelGridCellCount += 1;
      }
      labelGrid[key].push(descriptor);
      labelDescriptors.push(descriptor);
    });
    diagnostics.labelIndexBuildCount += 1;
    diagnostics.labelIndexDurationMs = nowMs() - startedAt;
    diagnostics.labelDescriptorCount = labelDescriptors.length;
    diagnostics.labelGridCellCount = labelGridCellCount;
  }

  function mapBoundsNumbers() {
    if (!map || typeof map.getBounds !== 'function') return null;
    try {
      var bounds = map.getBounds();
      var west = Number(bounds.getWest());
      var south = Number(bounds.getSouth());
      var east = Number(bounds.getEast());
      var north = Number(bounds.getNorth());
      if (
        !isFinite(west) ||
        !isFinite(south) ||
        !isFinite(east) ||
        !isFinite(north)
      ) {
        return null;
      }
      var lngPad = Math.max(0, east - west) * labelViewportPadRatio;
      var latPad = Math.max(0, north - south) * labelViewportPadRatio;
      return {
        west: west - lngPad,
        south: south - latPad,
        east: east + lngPad,
        north: north + latPad,
        centerLat: (south + north) / 2,
        centerLng: (west + east) / 2
      };
    } catch (labelBoundsError) {
      return null;
    }
  }

  function viewportLabelCandidates(viewport) {
    if (!viewport) return [];
    var minX = Math.floor(viewport.west / labelGridDegrees);
    var maxX = Math.floor(viewport.east / labelGridDegrees);
    var minY = Math.floor(viewport.south / labelGridDegrees);
    var maxY = Math.floor(viewport.north / labelGridDegrees);
    if (
      maxX < minX ||
      maxY < minY ||
      (maxX - minX + 1) * (maxY - minY + 1) > 5000
    ) {
      return [];
    }
    var out = [];
    for (var xIndex = minX; xIndex <= maxX; xIndex += 1) {
      for (var yIndex = minY; yIndex <= maxY; yIndex += 1) {
        var cell = labelGrid[xIndex + ':' + yIndex] || [];
        cell.forEach(function(descriptor) {
          if (
            descriptor.lng >= viewport.west &&
            descriptor.lng <= viewport.east &&
            descriptor.lat >= viewport.south &&
            descriptor.lat <= viewport.north
          ) {
            out.push(descriptor);
          }
        });
      }
    }
    return out;
  }

  function labelDistance(descriptor, viewport) {
    var latDistance = descriptor.lat - viewport.centerLat;
    var lngDistance = descriptor.lng - viewport.centerLng;
    return latDistance * latDistance + lngDistance * lngDistance;
  }

  function sortLabelCandidates(candidates, viewport) {
    return candidates.slice().sort(function(left, right) {
      var priorityDifference = left.priority - right.priority;
      if (priorityDifference) return priorityDifference;
      var distanceDifference =
        labelDistance(left, viewport) - labelDistance(right, viewport);
      if (distanceDifference) return distanceDifference;
      return left.sourceOrder - right.sourceOrder;
    });
  }

  function selectLabelDescriptors(candidates, viewport) {
    var nodeCandidates = [];
    var arcCandidates = [];
    candidates.forEach(function(descriptor) {
      if (!recordVisible(descriptor.record)) return;
      if (descriptor.kind === 'node') nodeCandidates.push(descriptor);
      else arcCandidates.push(descriptor);
    });
    nodeCandidates = sortLabelCandidates(nodeCandidates, viewport);
    arcCandidates = sortLabelCandidates(arcCandidates, viewport);

    var seenArcNames = Object.create(null);
    arcCandidates = arcCandidates.filter(function(descriptor) {
      if (descriptor.rule !== 'name') return true;
      var key = descriptor.text.toLocaleLowerCase();
      if (seenArcNames[key]) {
        diagnostics.labelDuplicateNameSkipCount += 1;
        return false;
      }
      seenArcNames[key] = true;
      return true;
    });

    var selected = nodeCandidates.slice(
      0,
      Math.min(labelNodeCap, labelCap)
    ).concat(
      arcCandidates.slice(0, Math.min(labelArcCap, labelCap))
    );
    if (selected.length > labelCap) selected = selected.slice(0, labelCap);

    if (selected.length < labelCap) {
      var selectedKeys = Object.create(null);
      selected.forEach(function(descriptor) {
        selectedKeys[descriptor.key] = true;
      });
      var remainder = sortLabelCandidates(
        nodeCandidates.concat(arcCandidates).filter(function(descriptor) {
          return !selectedKeys[descriptor.key];
        }),
        viewport
      );
      selected = selected.concat(
        remainder.slice(0, labelCap - selected.length)
      );
    }

    diagnostics.labelEligibleCandidateCount =
      nodeCandidates.length + arcCandidates.length;
    diagnostics.labelCappedCount = Math.max(
      0,
      diagnostics.labelEligibleCandidateCount - selected.length
    );
    return selected;
  }

  function ensureLabelLayerRoot() {
    if (labelLayerRoot || typeof L === 'undefined' || !L.layerGroup) {
      return labelLayerRoot;
    }
    labelLayerRoot = L.layerGroup([]);
    labelLayerRoot.options = labelLayerRoot.options || {};
    labelLayerRoot.options.group = labelGroupName;
    labelLayerRoot._brimCalsim3LabelRoot = true;
    return labelLayerRoot;
  }

  function labelMarker(descriptor) {
    if (
      typeof L === 'undefined' ||
      !L.marker ||
      !L.divIcon
    ) {
      return null;
    }
    var isNode = descriptor.kind === 'node';
    var iconAnchor = isNode ? [-5, 8] : [0, 0];
    if (L.point) iconAnchor = L.point(iconAnchor[0], iconAnchor[1]);
    return L.marker(
      [descriptor.lat, descriptor.lng],
      {
        icon: L.divIcon({
          className:
            'pt-label pt-calsim3-label-icon pt-calsim3-label-' +
            descriptor.kind,
          html: '<span>' + esc(descriptor.text) + '</span>',
          iconSize: null,
          iconAnchor: iconAnchor
        }),
        pane: isNode ? 'pane_labels_pts' : 'pane_labels_poly',
        interactive: false,
        keyboard: false,
        bubblingMouseEvents: false
      }
    );
  }

  function renderLabels(generation, reason) {
    if (
      destroyed ||
      generation !== labelGeneration ||
      !labelsEligible()
    ) {
      return;
    }
    var viewport = mapBoundsNumbers();
    var viewportCandidates = viewportLabelCandidates(viewport);
    diagnostics.labelViewportCandidateCount = viewportCandidates.length;
    var selected = selectLabelDescriptors(viewportCandidates, viewport);
    var root = ensureLabelLayerRoot();
    if (!root || typeof root.clearLayers !== 'function') return;
    if (
      map &&
      typeof map.hasLayer === 'function' &&
      map.hasLayer(root) &&
      typeof map.removeLayer === 'function'
    ) {
      map.removeLayer(root);
    }
    root.clearLayers();
    visibleLabelDescriptors = [];
    selected.forEach(function(descriptor) {
      if (generation !== labelGeneration) return;
      var marker = labelMarker(descriptor);
      if (!marker || typeof root.addLayer !== 'function') return;
      root.addLayer(marker);
      visibleLabelDescriptors.push(descriptor);
    });
    if (
      visibleLabelDescriptors.length &&
      typeof root.addTo === 'function'
    ) {
      root.addTo(map);
    }
    diagnostics.labelRenderCount += 1;
    diagnostics.labelVisibleCount = visibleLabelDescriptors.length;
    diagnostics.lastLabelRenderReason = reason || null;
    diagnostics.lastLabelRenderZoom = currentMapZoom();
  }

  function labelsEligible() {
    return !!(
      labelsRequested &&
      networkActive() &&
      currentMapZoom() >= labelMinZoom &&
      !labelMapMoving &&
      !map._ptMeasureInteractionActive
    );
  }

  function scheduleLabelRender(reason, delayMs) {
    cancelLabelWork(reason || 'label-reschedule', false);
    if (!labelsEligible()) {
      removeVisibleLabels(reason || 'label-ineligible');
      return false;
    }
    var generation = labelGeneration;
    diagnostics.labelScheduleCount += 1;
    diagnostics.lastLabelScheduleReason = reason || null;
    labelTimer = setTimeout(function() {
      labelTimer = null;
      if (
        destroyed ||
        generation !== labelGeneration ||
        !labelsEligible()
      ) {
        return;
      }
      labelFrame = requestFrame(function() {
        labelFrame = null;
        renderLabels(generation, reason);
      });
    }, Math.max(0, Number(delayMs || 0)));
    return true;
  }

  function configureCalSimRenderer(layerOrRenderer) {
    if (!layerOrRenderer) return null;
    var renderer = layerOrRenderer._renderer || layerOrRenderer;
    if (
      !renderer ||
      !renderer.options ||
      String(renderer.options.pane || '') !== calsimPaneName
    ) {
      return null;
    }
    if (
      typeof L !== 'undefined' &&
      L.Canvas &&
      !(renderer instanceof L.Canvas)
    ) {
      return null;
    }
    if (
      renderer.options.tolerance !== canvasTolerance ||
      renderer._brimCalsim3Component !== 'shared-arcs-nodes'
    ) {
      renderer.options.tolerance = canvasTolerance;
      renderer._brimCalsim3Component = 'shared-arcs-nodes';
      diagnostics.sharedRendererConfigureCount += 1;
    }
    return renderer;
  }

  function primeSharedCanvasRenderer() {
    if (
      !map ||
      typeof L === 'undefined' ||
      typeof L.canvas !== 'function'
    ) {
      diagnostics.sharedRendererPrimeFailure = 'canvas-factory-unavailable';
      return null;
    }
    if (!map._paneRenderers) map._paneRenderers = {};
    var existing = map._paneRenderers[calsimPaneName];
    if (existing) {
      var configuredExisting = configureCalSimRenderer(existing);
      if (configuredExisting) return configuredExisting;
      diagnostics.sharedRendererPrimeFailure =
        'non-canvas-pane-renderer-already-present';
      return null;
    }
    var renderer = L.canvas({
      pane: calsimPaneName,
      tolerance: canvasTolerance
    });
    renderer._brimCalsim3Component = 'shared-arcs-nodes';
    map._paneRenderers[calsimPaneName] = renderer;
    diagnostics.sharedRendererPrimeCount = 1;
    return renderer;
  }

  function syncSharedRendererInteraction() {
    if (!sharedPaneRenderer || !sharedPaneRenderer._container) return;
    // A Canvas renderer covers its whole pane, not just the painted paths.
    // Keeping this high-z surface out of DOM hit testing prevents CalSim from
    // shielding unrelated point, line, and polygon panes. Background map
    // events are forwarded into Leaflet's existing Canvas hit loop below.
    sharedPaneRenderer._container.style.pointerEvents = 'none';
    diagnostics.sharedRendererHasDrawnPath =
      !!sharedPaneRenderer._drawFirst;
    diagnostics.sharedRendererPassThrough = true;
  }

  function mapContainer() {
    if (map && typeof map.getContainer === 'function') {
      return map.getContainer();
    }
    return el || null;
  }

  function containerHasClass(name) {
    var container = mapContainer();
    return !!(
      container &&
      container.classList &&
      typeof container.classList.contains === 'function' &&
      container.classList.contains(name)
    );
  }

  function setSharedRendererCursor(active) {
    var container = mapContainer();
    if (
      !container ||
      !container.classList ||
      typeof container.classList.toggle !== 'function'
    ) {
      return;
    }
    container.classList.toggle('pt-calsim3-path-hover', !!active);
  }

  function clearSharedRendererHover(reason, originalEvent) {
    if (!sharedPaneRenderer) {
      setSharedRendererCursor(false);
      return;
    }
    var eventForMouseOut = originalEvent || lastForwardedPointerEvent;
    if (
      sharedPaneRenderer._hoveredLayer &&
      eventForMouseOut &&
      typeof sharedPaneRenderer._handleMouseOut === 'function'
    ) {
      try {
        forwardingSharedRendererEvent = true;
        sharedPaneRenderer._handleMouseOut(eventForMouseOut);
      } catch (rendererMouseOutError) {
        sharedPaneRenderer._hoveredLayer = null;
      } finally {
        forwardingSharedRendererEvent = false;
      }
    } else if (sharedPaneRenderer._hoveredLayer) {
      sharedPaneRenderer._hoveredLayer = null;
    }
    lastForwardedPointerEvent = null;
    setSharedRendererCursor(false);
    diagnostics.lastSharedRendererHoverClearReason = reason || null;
  }

  function mapEventOrigin(event) {
    if (!event) return null;
    return (
      event.propagatedFrom ||
      event.layer ||
      event.sourceTarget ||
      event.target ||
      null
    );
  }

  function mapEventIsBackground(event) {
    return mapEventOrigin(event) === map;
  }

  function sharedRendererInteractionSuppressed() {
    var dragging = map && map.dragging;
    return !!(
      destroyed ||
      !networkActive() ||
      map._ptMeasureInteractionActive ||
      map._animatingZoom ||
      (
        dragging &&
        typeof dragging.moving === 'function' &&
        dragging.moving()
      ) ||
      containerHasClass('pt-measure-active') ||
      containerHasClass('pt-marquee-zoom-mode') ||
      containerHasClass('pt-teaching-markup-active') ||
      containerHasClass('pt-teaching-label-active')
    );
  }

  function sharedRendererCanReceive(event) {
    return !!(
      !forwardingSharedRendererEvent &&
      mapEventIsBackground(event) &&
      event.originalEvent &&
      sharedPaneRenderer &&
      sharedPaneRenderer._drawFirst &&
      map &&
      typeof map.hasLayer === 'function' &&
      map.hasLayer(sharedPaneRenderer)
    );
  }

  function onMapMouseMove(event) {
    if (forwardingSharedRendererEvent) return;
    var origin = mapEventOrigin(event);
    if (origin !== map) {
      if (event && event.originalEvent && !sourceIsCalSim(origin)) {
        lastUnrelatedFeatureMoveEvent = event.originalEvent;
      }
      diagnostics.sharedRendererBackgroundSkipCount += 1;
      clearSharedRendererHover(
        'unrelated-feature-priority',
        event && event.originalEvent
      );
      return;
    }
    if (
      event &&
      event.originalEvent &&
      event.originalEvent === lastUnrelatedFeatureMoveEvent
    ) {
      lastUnrelatedFeatureMoveEvent = null;
      diagnostics.sharedRendererBackgroundSkipCount += 1;
      clearSharedRendererHover(
        'unrelated-canvas-hit-priority',
        event.originalEvent
      );
      return;
    }
    lastUnrelatedFeatureMoveEvent = null;
    if (sharedRendererInteractionSuppressed()) {
      diagnostics.sharedRendererSuppressedSkipCount += 1;
      clearSharedRendererHover(
        'shared-interaction-suppressed',
        event && event.originalEvent
      );
      return;
    }
    if (!sharedRendererCanReceive(event)) {
      diagnostics.sharedRendererBackgroundSkipCount += 1;
      clearSharedRendererHover(
        'unrelated-feature-priority',
        event && event.originalEvent
      );
      return;
    }
    if (typeof sharedPaneRenderer._onMouseMove !== 'function') return;
    lastForwardedPointerEvent = event.originalEvent;
    try {
      forwardingSharedRendererEvent = true;
      sharedPaneRenderer._onMouseMove(event.originalEvent);
      diagnostics.sharedRendererForwardedMoveCount += 1;
    } finally {
      forwardingSharedRendererEvent = false;
    }
    setSharedRendererCursor(!!sharedPaneRenderer._hoveredLayer);
  }

  function onMapClick(event) {
    var origin = mapEventOrigin(event);
    if (origin !== map) {
      if (event && event.originalEvent && !sourceIsCalSim(origin)) {
        lastUnrelatedFeatureClickEvent = event.originalEvent;
      }
      return;
    }
    if (
      event &&
      event.originalEvent &&
      event.originalEvent === lastUnrelatedFeatureClickEvent
    ) {
      lastUnrelatedFeatureClickEvent = null;
      return;
    }
    lastUnrelatedFeatureClickEvent = null;
    if (
      forwardingSharedRendererEvent ||
      sharedRendererInteractionSuppressed() ||
      !sharedRendererCanReceive(event) ||
      typeof sharedPaneRenderer._onClick !== 'function'
    ) {
      return;
    }
    try {
      forwardingSharedRendererEvent = true;
      sharedPaneRenderer._onClick(event.originalEvent);
      diagnostics.sharedRendererForwardedClickCount += 1;
    } finally {
      forwardingSharedRendererEvent = false;
    }
  }

  function onMapMouseOut(event) {
    clearSharedRendererHover(
      'map-mouseout',
      event && event.originalEvent
    );
  }

  function detachSharedRenderer(reason) {
    clearSharedRendererHover(reason || 'renderer-detach');
    if (
      !sharedPaneRenderer ||
      !map ||
      typeof map.hasLayer !== 'function' ||
      !map.hasLayer(sharedPaneRenderer) ||
      typeof map.removeLayer !== 'function'
    ) {
      return false;
    }
    try {
      map.removeLayer(sharedPaneRenderer);
      diagnostics.sharedRendererDetachCount =
        Number(diagnostics.sharedRendererDetachCount || 0) + 1;
      diagnostics.lastSharedRendererDetachReason = reason || null;
      return true;
    } catch (rendererDetachError) {
      return false;
    }
  }

  function rendererState() {
    var arcRenderer = null;
    var nodeRenderer = null;
    arcRecords.some(function(record) {
      arcRenderer = configureCalSimRenderer(record.layer);
      return !!arcRenderer;
    });
    nodeRecords.some(function(record) {
      nodeRenderer = configureCalSimRenderer(record.layer);
      return !!nodeRenderer;
    });
    return {
      pane: calsimPaneName,
      tolerance: canvasTolerance,
      primed: !!sharedPaneRenderer,
      arcRendererMounted: !!arcRenderer,
      nodeRendererMounted: !!nodeRenderer,
      canvasOnMap: !!(
        sharedPaneRenderer &&
        map &&
        typeof map.hasLayer === 'function' &&
        map.hasLayer(sharedPaneRenderer)
      ),
      pointerEvents: (
        sharedPaneRenderer &&
        sharedPaneRenderer._container &&
        sharedPaneRenderer._container.style
      ) ? sharedPaneRenderer._container.style.pointerEvents || '' : null,
      shared: !!(
        arcRenderer &&
        nodeRenderer &&
        arcRenderer === nodeRenderer
      ),
      passThrough: !!(
        sharedPaneRenderer &&
        sharedPaneRenderer._container &&
        sharedPaneRenderer._container.style &&
        sharedPaneRenderer._container.style.pointerEvents === 'none'
      ),
      hoveredComponent: componentForSource(
        sharedPaneRenderer && sharedPaneRenderer._hoveredLayer
      ) || null
    };
  }

  sharedPaneRenderer = primeSharedCanvasRenderer();

  function groupEventIsTarget(event) {
    if (!event) return false;
    if (event.name === groupName) return true;
    return !!(groupRoot && event.layer === groupRoot);
  }

  function labelGroupEventIsTarget(event) {
    return !!(
      event &&
      labelGroupName &&
      event.name === labelGroupName
    );
  }

  function componentForSource(source) {
    return source ? source._brimCalsim3Component || '' : '';
  }

  function sourceIsCalSim(source) {
    var component = componentForSource(source);
    return component === 'arcs' || component === 'nodes';
  }

  function closeCurrentTooltip(reason, component) {
    if (!currentTooltip) return false;
    var source = currentTooltip._source || null;
    if (component && componentForSource(source) !== component) return false;
    var tooltip = currentTooltip;
    currentTooltip = null;
    try {
      if (typeof map.closeTooltip === 'function') {
        map.closeTooltip(tooltip);
      } else if (typeof map.removeLayer === 'function') {
        map.removeLayer(tooltip);
      }
    } catch (tooltipCloseError) {}
    diagnostics.tooltipCloseCount += 1;
    diagnostics.lastTooltipCloseReason = reason || null;
    return true;
  }

  function closeCurrentPopup(reason) {
    if (!currentPopup) return false;
    var popup = currentPopup;
    currentPopup = null;
    try {
      if (typeof map.closePopup === 'function') {
        map.closePopup(popup);
      } else if (typeof map.removeLayer === 'function') {
        map.removeLayer(popup);
      }
    } catch (popupCloseError) {}
    diagnostics.popupCloseCount += 1;
    diagnostics.lastPopupCloseReason = reason || null;
    return true;
  }

  function unspiderfy(reason) {
    if (
      !clusterGroup ||
      !clusterGroup._spiderfied ||
      typeof clusterGroup.unspiderfy !== 'function'
    ) {
      return false;
    }
    try {
      clusterGroup.unspiderfy();
      diagnostics.unspiderfyCount += 1;
      diagnostics.lastUnspiderfyReason = reason || null;
      return true;
    } catch (unspiderfyError) {
      return false;
    }
  }

  function clearTransientInteraction(reason) {
    closeCurrentTooltip(reason);
    closeCurrentPopup(reason);
    unspiderfy(reason);
  }

  function cancelPendingFocus(reason) {
    focusGeneration += 1;
    if (focusTimer !== null) {
      clearTimeout(focusTimer);
      focusTimer = null;
    }
    diagnostics.lastFocusCancelReason = reason || null;
  }

  function restoreHighlight() {
    if (!currentHighlight) return;
    var record = currentHighlight;
    currentHighlight = null;
    if (!record.layer || typeof record.layer.setStyle !== 'function') return;
    try {
      if (record.kind === 'arc') {
        record.layer.setStyle({
          color: record.color,
          weight: record.weight,
          opacity: 0.85
        });
      } else {
        setRetainedNodeRadius(
          record,
          nodeRadiusForRecord(
            record,
            appliedNodeRadiusTier ||
              nodeRadiusTierForZoom(currentMapZoom())
          )
        );
        record.layer.setStyle({
          color: record.stroke,
          weight: 1,
          fillColor: record.fill,
          fillOpacity: 0.82
        });
      }
    } catch (highlightRestoreError) {}
  }

  function clearFinderTransient(reason) {
    cancelPendingFocus(reason);
    restoreHighlight();
    lastFinderResults = [];
    lastFinderQuery = '';
    finderVisibleLimit = finderInitialLimit;
    if (card) {
      var results = card.querySelector('.pt-calsim3-finder-results');
      if (results) results.innerHTML = '';
      var input = card.querySelector('.pt-calsim3-find');
      if (input) input.value = '';
    }
  }

  function networkActive() {
    return !!(
      groupRoot &&
      typeof map.hasLayer === 'function' &&
      map.hasLayer(groupRoot)
    );
  }

  function arcShouldBePresent(record) {
    return !!(
      state.showArcs &&
      state.arcTypes[record.type] === true
    );
  }

  function nodeShouldBeRegistered(record) {
    return state.nodeGroups[record.group] === true;
  }

  function recordVisible(record) {
    if (!record || !record.layer || !networkActive()) return false;
    if (record.kind === 'arc') {
      return !!(
        arcShouldBePresent(record) &&
        groupRoot &&
        typeof groupRoot.hasLayer === 'function' &&
        groupRoot.hasLayer(record.layer)
      );
    }
    return !!(
      state.showNodes &&
      nodeShouldBeRegistered(record) &&
      groupRoot &&
      typeof groupRoot.hasLayer === 'function' &&
      groupRoot.hasLayer(clusterGroup) &&
      clusterGroup &&
      typeof clusterGroup.hasLayer === 'function' &&
      clusterGroup.hasLayer(record.layer)
    );
  }

  function countSelected(records, field, selection) {
    return records.reduce(function(total, record) {
      return total + (selection[record[field]] === true ? 1 : 0);
    }, 0);
  }

  function shownCounts() {
    return {
      arcs: state.showArcs ?
        countSelected(arcRecords, 'type', state.arcTypes) : 0,
      nodes: state.showNodes ?
        countSelected(nodeRecords, 'group', state.nodeGroups) : 0
    };
  }

  function setStatus(message, tone) {
    if (!card) return;
    var status = card.querySelector('.pt-calsim3-status');
    if (!status) return;
    status.textContent = message || '';
    status.setAttribute('data-tone', tone || 'normal');
  }

  function updateCounts() {
    if (!card) return;
    var counts = shownCounts();
    var summary = card.querySelector('.pt-calsim3-counts');
    if (summary) {
      summary.textContent =
        countLabel(counts.arcs) + ' / ' + countLabel(arcRecords.length) +
        ' arcs · ' +
        countLabel(counts.nodes) + ' / ' + countLabel(nodeRecords.length) +
        ' nodes shown';
    }
  }

  function cancelPendingApply(reason) {
    applyScheduleGeneration += 1;
    if (applyFrame !== null) {
      cancelFrame(applyFrame);
      applyFrame = null;
      diagnostics.cancelledApplyCount += 1;
    }
    diagnostics.lastApplyCancelReason = reason || null;
    updateApplyUi();
  }

  function reconcileObjects(source) {
    var startedAt = nowMs();
    var arcAdds = 0;
    var arcRemoves = 0;
    var nodeAdds = 0;
    var nodeRemoves = 0;
    diagnostics.applyGeneration += 1;
    clearSharedRendererHover('filter-apply');
    clearTransientInteraction('filter-apply');
    cancelPendingFocus('filter-apply');
    restoreHighlight();

    if (groupRoot) {
      arcRecords.forEach(function(record) {
        if (!record.layer) return;
        var present = (
          typeof groupRoot.hasLayer === 'function'
        ) ? groupRoot.hasLayer(record.layer) : false;
        var shouldBePresent = arcShouldBePresent(record);
        if (shouldBePresent && !present && groupRoot.addLayer) {
          groupRoot.addLayer(record.layer);
          arcAdds += 1;
        } else if (!shouldBePresent && present && groupRoot.removeLayer) {
          groupRoot.removeLayer(record.layer);
          arcRemoves += 1;
        }
      });
    }

    if (clusterGroup) {
      nodeRecords.forEach(function(record) {
        if (!record.layer) return;
        var present = (
          typeof clusterGroup.hasLayer === 'function'
        ) ? clusterGroup.hasLayer(record.layer) : false;
        var shouldBePresent = nodeShouldBeRegistered(record);
        if (shouldBePresent && !present && clusterGroup.addLayer) {
          clusterGroup.addLayer(record.layer);
          nodeAdds += 1;
        } else if (!shouldBePresent && present && clusterGroup.removeLayer) {
          clusterGroup.removeLayer(record.layer);
          nodeRemoves += 1;
        }
      });

      if (groupRoot && typeof groupRoot.hasLayer === 'function') {
        var clusterPresent = groupRoot.hasLayer(clusterGroup);
        if (state.showNodes && !clusterPresent && groupRoot.addLayer) {
          groupRoot.addLayer(clusterGroup);
        } else if (
          !state.showNodes &&
          clusterPresent &&
          groupRoot.removeLayer
        ) {
          groupRoot.removeLayer(clusterGroup);
        }
      }
    }

    rendererState();
    syncSharedRendererInteraction();
    if (
      sharedPaneRenderer &&
      !sharedPaneRenderer._drawFirst &&
      networkActive()
    ) {
      detachSharedRenderer('empty-filter-result');
    }
    diagnostics.filterApplyCount += 1;
    diagnostics.lastApplyDurationMs = nowMs() - startedAt;
    diagnostics.lastApplySource = source || 'manual';
    diagnostics.lastArcAddCount = arcAdds;
    diagnostics.lastArcRemoveCount = arcRemoves;
    diagnostics.lastNodeAddCount = nodeAdds;
    diagnostics.lastNodeRemoveCount = nodeRemoves;
    diagnostics.totalArcAddCount += arcAdds;
    diagnostics.totalArcRemoveCount += arcRemoves;
    diagnostics.totalNodeAddCount += nodeAdds;
    diagnostics.totalNodeRemoveCount += nodeRemoves;
    uiDirty = false;
    updateCounts();
    updateApplyUi();
    setStatus(
      source === 'auto' ? 'Live applied.' : 'Applied.',
      'ok'
    );
    removeVisibleLabels('filter-' + (source || 'manual'));
    scheduleLabelRender('filter-' + (source || 'manual'), 0);
  }

  function normalizeRequestedSelection(values, allowed) {
    var allowedSet = setFrom(allowed);
    return setFrom((values || []).filter(function(value) {
      return allowedSet[String(value)] === true;
    }).map(String));
  }

  function applyOptions(options, source) {
    options = options || {};
    cancelPendingApply('direct-apply');
    scheduledOptions = null;
    if (Object.prototype.hasOwnProperty.call(options, 'showArcs')) {
      state.showArcs = options.showArcs === true;
    }
    if (Object.prototype.hasOwnProperty.call(options, 'showNodes')) {
      state.showNodes = options.showNodes === true;
    }
    if (Object.prototype.hasOwnProperty.call(options, 'arcTypes')) {
      state.arcTypes = normalizeRequestedSelection(
        options.arcTypes,
        arcTypeOrder
      );
    }
    if (Object.prototype.hasOwnProperty.call(options, 'nodeGroups')) {
      state.nodeGroups = normalizeRequestedSelection(
        options.nodeGroups,
        nodeGroupOrder
      );
    }
    reconcileObjects(source || 'manual');
    writeUiState();
    return stats();
  }

  function resetState() {
    cancelPendingApply('reset');
    state.showArcs = true;
    state.showNodes = true;
    state.arcTypes = setFrom(arcTypeOrder);
    state.nodeGroups = setFrom(nodeGroupOrder);
    state.hoverArcs = true;
    state.hoverNodes = true;
    clearFinderTransient('reset');
    reconcileObjects('reset');
    diagnostics.filterResetCount += 1;
    writeUiState();
    setStatus('Reset to all arcs and nodes.', 'ok');
    return stats();
  }

  function setHover(component, enabled) {
    enabled = enabled === true;
    if (component === 'arcs') {
      state.hoverArcs = enabled;
      if (!enabled) {
        closeCurrentTooltip('arc-hover-disabled', 'arcs');
        if (
          componentForSource(
            sharedPaneRenderer && sharedPaneRenderer._hoveredLayer
          ) === 'arcs'
        ) {
          clearSharedRendererHover('arc-hover-disabled');
        }
      }
    } else if (component === 'nodes') {
      state.hoverNodes = enabled;
      if (!enabled) {
        closeCurrentTooltip('node-hover-disabled', 'nodes');
        if (
          componentForSource(
            sharedPaneRenderer && sharedPaneRenderer._hoveredLayer
          ) === 'nodes'
        ) {
          clearSharedRendererHover('node-hover-disabled');
        }
      }
    }
    if (card) {
      var input = card.querySelector(
        component === 'arcs' ?
          '.pt-calsim3-hover-arcs' :
          '.pt-calsim3-hover-nodes'
      );
      if (input) input.checked = enabled;
    }
    return stats();
  }

  function updateApplyUi() {
    if (!card) return;
    var autoInput = card.querySelector('.pt-calsim3-auto');
    var applyButton = card.querySelector('.pt-calsim3-apply');
    if (autoInput) autoInput.checked = autoApply;
    if (applyButton) {
      applyButton.disabled = autoApply || !uiDirty;
      applyButton.title = autoApply ?
        'Live apply updates filter changes automatically' :
        (uiDirty ? 'Apply pending filter changes' : 'No pending changes');
    }
    card.setAttribute('data-filter-pending', uiDirty ? 'true' : 'false');
    card.setAttribute('data-auto-apply', autoApply ? 'true' : 'false');
    card.setAttribute('data-live-apply', autoApply ? 'true' : 'false');
  }

  function scheduleUiApply(reason, requestedOptions) {
    uiDirty = true;
    scheduledOptions = requestedOptions || null;
    updateApplyUi();
    if (!autoApply) {
      setStatus('Changes pending — Apply.', 'normal');
      return false;
    }

    applyScheduleGeneration += 1;
    var generation = applyScheduleGeneration;
    if (applyFrame !== null) {
      cancelFrame(applyFrame);
      diagnostics.cancelledApplyCount += 1;
    }
    diagnostics.autoApplyScheduleCount += 1;
    applyFrame = requestFrame(function() {
      applyFrame = null;
      if (
        destroyed ||
        generation !== applyScheduleGeneration ||
        (!card && !scheduledOptions)
      ) {
        return;
      }
      diagnostics.autoApplyRunCount += 1;
      var options = scheduledOptions || readUiOptions();
      scheduledOptions = null;
      applyOptions(options, 'auto');
    });
    diagnostics.lastApplyScheduleReason = reason || null;
    setStatus('Updating filters…', 'normal');
    return true;
  }

  function setAutoApply(enabled) {
    enabled = enabled === true;
    if (autoApply === enabled) {
      updateApplyUi();
      return stats();
    }
    autoApply = enabled;
    if (!enabled) {
      cancelPendingApply('auto-disabled');
      if (uiDirty) setStatus('Changes pending — Apply.', 'normal');
      else setStatus('Live apply off. Changes require Apply.', 'normal');
    } else if (uiDirty) {
      scheduleUiApply('auto-enabled');
    } else {
      setStatus('Live apply on.', 'normal');
      updateApplyUi();
    }
    return stats();
  }

  function startsWithText(value, needle) {
    return !!value && value.indexOf(needle) === 0;
  }

  function hasWholeTerm(value, needle) {
    if (!value || !needle) return false;
    var from = 0;
    while (from <= value.length - needle.length) {
      var at = value.indexOf(needle, from);
      if (at < 0) return false;
      var before = at === 0 ? '' : value.charAt(at - 1);
      var afterAt = at + needle.length;
      var after = afterAt >= value.length ? '' : value.charAt(afterAt);
      if (
        (!before || !/[a-z0-9]/i.test(before)) &&
        (!after || !/[a-z0-9]/i.test(after))
      ) {
        return true;
      }
      from = at + 1;
    }
    return false;
  }

  function finderRank(record, needle) {
    if (record.idSearch === needle) return 0;
    if (startsWithText(record.idSearch, needle)) return 1;
    if (record.primarySearchFields.some(function(value) {
      return value === needle;
    })) return 2;
    if (record.primarySearchFields.some(function(value) {
      return startsWithText(value, needle);
    })) return 3;
    if (hasWholeTerm(record.searchText, needle)) return 4;
    if (
      record.idSearch.indexOf(needle) >= 0 ||
      record.primarySearchFields.some(function(value) {
        return value.indexOf(needle) >= 0;
      })
    ) return 5;
    if (record.secondarySearchFields.some(function(value) {
      return value.indexOf(needle) >= 0;
    })) return 6;
    return Infinity;
  }

  function searchRecords(query) {
    var needle = clean(query).toLocaleLowerCase();
    diagnostics.finderQueryCount += 1;
    if (!needle) {
      lastFinderResults = [];
      return [];
    }
    lastFinderResults = arcRecords.concat(nodeRecords).map(function(record) {
      return {
        record: record,
        rank: finderRank(record, needle)
      };
    }).filter(function(candidate) {
      return isFinite(candidate.rank);
    }).sort(function(left, right) {
      return (
        left.rank - right.rank ||
        left.record.searchOrder - right.record.searchOrder
      );
    }).map(function(candidate) {
      return candidate.record;
    });
    return lastFinderResults;
  }

  function publicFinderRecord(record) {
    return {
      key: record.key,
      kind: record.kind,
      id: record.id,
      type: record.kind === 'arc' ? record.type : record.group,
      fromNode: record.kind === 'arc' ? record.fromNode : '',
      toNode: record.kind === 'arc' ? record.toNode : '',
      name: record.kind === 'arc' ? record.name : record.description,
      river: record.kind === 'node' ? record.river : '',
      visible: recordVisible(record)
    };
  }

  function resultPrimary(record) {
    if (record.kind === 'arc') {
      return (
        (record.fromNode || '?') + ' → ' + (record.toNode || '?')
      );
    }
    return record.id || 'CalSim3 node';
  }

  function resultSecondary(record) {
    if (record.kind === 'arc') {
      var arcBits = ['Arc ' + (record.id || 'unlabeled'), record.type];
      if (record.name) arcBits.push(record.name);
      return arcBits.filter(Boolean).join(' · ');
    }
    return [
      record.group,
      record.description,
      record.river ? 'River: ' + record.river : ''
    ].filter(Boolean).join(' · ');
  }

  function finderPage(query, visibleLimit) {
    var matches = searchRecords(query);
    var limit = Math.max(
      0,
      Math.min(
        matches.length,
        asNumber(visibleLimit, finderInitialLimit)
      )
    );
    return {
      query: clean(query),
      total: matches.length,
      showing: limit,
      canShowAll: matches.length <= finderShowAllMaximum,
      results: matches.slice(0, limit).map(publicFinderRecord)
    };
  }

  function renderFinderResults(query, expansion) {
    var normalizedQuery = clean(query);
    var isNewQuery = normalizedQuery.toLocaleLowerCase() !==
      lastFinderQuery.toLocaleLowerCase();
    if (isNewQuery || expansion === 'reset') {
      finderVisibleLimit = finderInitialLimit;
    } else if (expansion === 'more') {
      finderVisibleLimit += finderBatchSize;
    } else if (expansion === 'all') {
      finderVisibleLimit = finderShowAllMaximum;
    } else if (expansion === 'fewer') {
      finderVisibleLimit = finderInitialLimit;
    }
    lastFinderQuery = normalizedQuery;
    var matches = searchRecords(normalizedQuery);
    if (
      expansion === 'all' &&
      matches.length <= finderShowAllMaximum
    ) {
      finderVisibleLimit = matches.length;
    }
    finderVisibleLimit = Math.min(
      Math.max(finderInitialLimit, finderVisibleLimit),
      Math.max(finderInitialLimit, matches.length)
    );
    var limited = matches.slice(0, finderVisibleLimit);
    if (!card) return limited.map(publicFinderRecord);
    var holder = card.querySelector('.pt-calsim3-finder-results');
    if (!holder) return limited.map(publicFinderRecord);
    var previousScrollTop = holder.scrollTop || 0;
    var html = limited.map(function(record) {
      var visible = recordVisible(record);
      return '<div class="pt-calsim3-result">' +
        '<button type="button" class="pt-calsim3-result-main" ' +
        'data-action="focus-result" data-record-key="' + esc(record.key) + '">' +
        '<span class="pt-calsim3-result-kind">' +
        (record.kind === 'arc' ? 'Arc' : 'Node') + '</span>' +
        '<span class="pt-calsim3-result-text"><b>' +
        esc(resultPrimary(record)) + '</b><small>' +
        esc(resultSecondary(record)) + '</small></span></button>' +
        (visible ? '' :
          '<button type="button" class="pt-calsim3-show-result" ' +
          'data-action="show-result" data-record-key="' +
          esc(record.key) + '">Show</button>') +
        '</div>';
    }).join('');
    if (matches.length) {
      html += '<div class="pt-calsim3-finder-nav">' +
        '<span>Showing ' + countLabel(limited.length) + ' of ' +
        countLabel(matches.length) + '</span><span class="pt-calsim3-mini-actions">';
      if (limited.length < matches.length) {
        html += '<button type="button" data-action="finder-more">Show more</button>';
        if (matches.length <= finderShowAllMaximum) {
          html += '<button type="button" data-action="finder-all">Show all</button>';
        }
      }
      if (limited.length > finderInitialLimit) {
        html += '<button type="button" data-action="finder-fewer">Show fewer</button>';
      }
      html += '</span></div>';
    }
    holder.innerHTML = html;
    holder.scrollTop = (
      isNewQuery ||
      expansion === 'fewer'
    ) ? 0 : previousScrollTop;

    if (!normalizedQuery) {
      setStatus('Type to search retained CalSim arcs and nodes.', 'normal');
    } else if (!matches.length) {
      setStatus('No retained CalSim records match.', 'warn');
    } else {
      setStatus(
        'Showing ' + countLabel(limited.length) + ' of ' +
        countLabel(matches.length) + ' match' +
        (matches.length === 1 ? '.' : 'es.'),
        'normal'
      );
    }
    return limited.map(publicFinderRecord);
  }

  function applyFinderHighlight(record) {
    restoreHighlight();
    if (!record || !record.layer) return;
    currentHighlight = record;
    if (typeof record.layer.setStyle === 'function') {
      if (record.kind === 'arc') {
        record.layer.setStyle({
          color: '#FF00A8',
          weight: Math.max(5, record.weight + 3),
          opacity: 1
        });
        if (typeof record.layer.bringToFront === 'function') {
          record.layer.bringToFront();
        }
      } else {
        setRetainedNodeRadius(record, nodeHighlightRadius(record));
        record.layer.setStyle({
          color: '#111111',
          weight: 3,
          fillColor: record.fill,
          fillOpacity: 1
        });
      }
    }
    try {
      if (typeof record.layer.openPopup === 'function') {
        record.layer.openPopup();
      }
    } catch (finderPopupError) {}
  }

  function focusRecord(recordOrKey) {
    var record = typeof recordOrKey === 'string' ?
      recordsByKey[recordOrKey] : recordOrKey;
    if (!record || !record.layer) {
      setStatus('Result object is unavailable; no map change made.', 'warn');
      return {focused: false, reason: 'unresolved'};
    }
    if (!recordVisible(record)) {
      diagnostics.finderFilteredCount += 1;
      setStatus(
        'Result is filtered out. Use Show to reveal it explicitly.',
        'warn'
      );
      return {focused: false, reason: 'filtered'};
    }

    clearTransientInteraction('finder-focus');
    cancelPendingFocus('finder-replacement');
    restoreHighlight();
    diagnostics.finderFocusCount += 1;

    try {
      if (
        record.kind === 'arc' &&
        typeof record.layer.getBounds === 'function' &&
        typeof map.fitBounds === 'function'
      ) {
        map.fitBounds(record.layer.getBounds(), {
          padding: [36, 36],
          maxZoom: 11,
          animate: false
        });
      } else if (
        record.kind === 'node' &&
        typeof record.layer.getLatLng === 'function' &&
        typeof map.setView === 'function'
      ) {
        var currentZoom = typeof map.getZoom === 'function' ?
          asNumber(map.getZoom(), transitionZoom) : transitionZoom;
        map.setView(
          record.layer.getLatLng(),
          Math.max(transitionZoom, currentZoom),
          {animate: false}
        );
      }
    } catch (finderMoveError) {}

    var generation = focusGeneration;
    focusTimer = setTimeout(function() {
      focusTimer = null;
      if (
        destroyed ||
        generation !== focusGeneration ||
        !recordVisible(record)
      ) {
        return;
      }
      applyFinderHighlight(record);
    }, 80);
    setStatus(
      (record.kind === 'arc' ? 'Arc ' : 'Node ') +
      (record.id || resultPrimary(record)) + ' selected.',
      'ok'
    );
    return {focused: true, key: record.key};
  }

  function showFinderRecord(recordOrKey) {
    var record = typeof recordOrKey === 'string' ?
      recordsByKey[recordOrKey] : recordOrKey;
    if (!record) return {focused: false, reason: 'missing'};

    if (record.kind === 'arc') {
      state.showArcs = true;
      state.arcTypes[record.type] = true;
    } else {
      state.showNodes = true;
      state.nodeGroups[record.group] = true;
    }
    cancelPendingApply('finder-show');
    reconcileObjects('finder-show');
    writeUiState();
    return focusRecord(record);
  }

  function onClusterClick(event) {
    if (!hasExactDuplicateLocations || !event || !event.layer) {
      diagnostics.ignoredClusterClicks += 1;
      return;
    }
    var zoom = (
      map && typeof map.getZoom === 'function'
    ) ? Number(map.getZoom()) : 0;

    if (
      zoom >= transitionZoom &&
      typeof event.layer.spiderfy === 'function'
    ) {
      event.layer.spiderfy();
      diagnostics.exactLocationSpiderfyClicks += 1;
      return;
    }
    if (typeof event.layer.zoomToBounds === 'function') {
      event.layer.zoomToBounds();
      diagnostics.lowZoomClusterClicks += 1;
      return;
    }
    diagnostics.ignoredClusterClicks += 1;
  }

  function onTooltipOpen(event) {
    var tooltip = event && event.tooltip ? event.tooltip : null;
    var source = tooltip && tooltip._source ? tooltip._source : null;
    if (tooltip) observedTooltip = tooltip;
    if (!tooltip || !sourceIsCalSim(source)) return;
    var component = componentForSource(source);
    var allowed = (
      !map._ptMeasureInteractionActive &&
      (
        (component === 'arcs' && state.hoverArcs) ||
        (component === 'nodes' && state.hoverNodes)
      )
    );
    if (!allowed) {
      currentTooltip = tooltip;
      closeCurrentTooltip('hover-suppressed');
      return;
    }
    if (currentTooltip && currentTooltip !== tooltip) {
      closeCurrentTooltip('replacement');
    }
    currentTooltip = tooltip;
    diagnostics.tooltipOpenCount += 1;
  }

  function onTooltipClose(event) {
    if (event && event.tooltip === observedTooltip) {
      observedTooltip = null;
    }
    if (event && event.tooltip === currentTooltip) {
      currentTooltip = null;
    }
  }

  function onPopupOpen(event) {
    var popup = event && event.popup ? event.popup : null;
    var source = popup && popup._source ? popup._source : null;
    if (popup) observedPopup = popup;
    if (!popup || !sourceIsCalSim(source)) return;
    closeCurrentTooltip('popup-open');
    currentPopup = popup;
    diagnostics.popupOpenCount += 1;
  }

  function onPopupClose(event) {
    if (event && event.popup === observedPopup) {
      observedPopup = null;
    }
    if (event && event.popup === currentPopup) {
      currentPopup = null;
    }
  }

  function dockAndHideCard() {
    if (!card) return;
    var detachable = card.__brimDetachableState;
    if (detachable && detachable.floating && detachable.dock) {
      detachable.dock();
    }
    card.style.display = 'none';
  }

  function updateCardVisibility() {
    if (!card) return;
    if (!networkActive() || cardHiddenByUser) {
      dockAndHideCard();
    } else {
      card.style.display = 'block';
    }
  }

  function onOverlayAdd(event) {
    if (labelGroupEventIsTarget(event)) {
      syncLabelsFromSharedControl('label-overlay-add', true);
      return;
    }
    if (!groupEventIsTarget(event)) return;
    if (observedNetworkActive !== true) {
      diagnostics.activationCount += 1;
    }
    observedNetworkActive = true;
    cardHiddenByUser = false;
    rendererState();
    syncSharedRendererInteraction();
    applyNodeRadiusTier('overlay-add');
    updateCardVisibility();
    updateCounts();
    syncLabelsFromSharedControl('network-overlay-add', false);
  }

  function onOverlayRemove(event) {
    if (labelGroupEventIsTarget(event)) {
      syncLabelsFromSharedControl('label-overlay-remove', false);
      return;
    }
    if (!groupEventIsTarget(event)) return;
    if (observedNetworkActive !== false) {
      diagnostics.deactivationCount += 1;
    }
    observedNetworkActive = false;
    cancelPendingApply('overlay-remove');
    scheduledOptions = null;
    uiDirty = false;
    writeUiState();
    clearTransientInteraction('overlay-remove');
    clearFinderTransient('overlay-remove');
    // Update the browser-managed label state immediately, but do not click the
    // companion layer-control input from inside Leaflet's main-checkbox
    // transaction. The shared inline-label adapter owns that main/companion
    // lifecycle and reconciles its canonical checkbox on the native `change`
    // event after Leaflet has finished the outer click.
    setLabelsRequestedState(false, 'network-overlay-remove');
    cardHiddenByUser = false;
    dockAndHideCard();
    detachSharedRenderer('overlay-remove');
    invalidateNodeRadiusTier('overlay-remove');
  }

  function onLayerAdd(event) {
    if (event && event.layer) configureCalSimRenderer(event.layer);
    syncSharedRendererInteraction();
    if (groupRoot && event && event.layer === groupRoot) {
      onOverlayAdd(event);
    }
  }

  function onLayerRemove(event) {
    syncSharedRendererInteraction();
    if (groupRoot && event && event.layer === groupRoot) {
      onOverlayRemove(event);
    }
  }

  function onMeasureInteractionChange(event) {
    if (event && event.active) {
      clearSharedRendererHover('measure-active');
      clearTransientInteraction('measure-active');
      cancelPendingFocus('measure-active');
      restoreHighlight();
      cancelLabelWork('measure-active', true);
    } else {
      scheduleLabelRender('measure-inactive', 40);
    }
  }

  function typeCounts(records, field, order) {
    var counts = Object.create(null);
    order.forEach(function(value) {
      counts[value] = 0;
    });
    records.forEach(function(record) {
      counts[record[field]] = (counts[record[field]] || 0) + 1;
    });
    return counts;
  }

  function representative(records, field, value) {
    for (var i = 0; i < records.length; i += 1) {
      if (records[i][field] === value) return records[i];
    }
    return {};
  }

  function nodeGroupDisplayLabel(value) {
    var labels = {
      'Conveyance': 'Conveyance',
      'Storage / Reservoir': 'Storage',
      'Project demand – urban': 'P — Urban',
      'Project demand – ag': 'P — Ag',
      'Project demand – refuge': 'P — Refuge',
      'Non-project demand – urban': 'NP — Urban',
      'Non-project demand – ag': 'NP — Ag',
      'Non-project demand – refuge': 'NP — Refuge',
      'Settlement demand – urban': 'Settlement — Urban',
      'Settlement demand – ag': 'Settlement — Ag',
      'Demand – other': 'Other demand',
      'Treatment plant': 'Treatment',
      'Return flow': 'Return flow',
      'External Unit': 'External unit',
      'Major Feature': 'Major feature',
      'Unknown / Other': 'Unknown / other'
    };
    return labels[value] || value;
  }

  function checkboxHtml(kind, value, label, count, symbol, title) {
    return '<label class="pt-calsim3-option">' +
      '<input type="checkbox" data-filter-kind="' + esc(kind) +
      '" value="' + esc(value) + '" checked title="' +
      esc(title || label) + '">' +
      symbol + '<span class="pt-calsim3-option-label">' +
      '<span title="' + esc(title || label) + '">' + esc(label) +
      '</span></span><span class="pt-calsim3-option-count">' +
      countLabel(count) + '</span></label>';
  }

  function lineSymbol(color, weight) {
    return '<span class="pt-calsim3-line-symbol" style="border-top-color:' +
      esc(color || '#777777') + ';border-top-width:' +
      Math.max(2, asNumber(weight, 1.2) + 1) + 'px"></span>';
  }

  function nodeSymbol(fill, stroke) {
    return '<span class="pt-calsim3-node-symbol" style="background:' +
      esc(fill || '#BDBDBD') + ';border-color:' +
      esc(stroke || '#737373') + '"></span>';
  }

  function ensureCss() {
    if (
      typeof document === 'undefined' ||
      !document.getElementById ||
      !document.createElement ||
      !document.head
    ) {
      return;
    }
    if (document.getElementById('pt-calsim3-explorer-css')) return;
    var style = document.createElement('style');
    style.id = 'pt-calsim3-explorer-css';
    style.textContent =
      '.pt-calsim3-explorer{width:348px;max-width:calc(100vw - 18px);max-height:calc(100vh - 154px);overflow:auto;box-sizing:border-box;background:rgba(246,239,222,.97);border:1px solid rgba(112,103,83,.55);border-radius:6px;box-shadow:0 1px 5px rgba(0,0,0,.25);padding:7px 8px;font:11px/1.2 Arial,sans-serif;color:#222;}' +
      '.pt-calsim3-head{display:flex;align-items:flex-start;justify-content:space-between;gap:7px;font-size:13px;font-weight:700;margin-bottom:2px;}' +
      '.pt-calsim3-head-title{min-width:0;white-space:nowrap;}' +
      '.pt-calsim3-head-actions{display:inline-flex;align-items:center;gap:3px;white-space:nowrap;}' +
      '.pt-calsim3-label-toggle{display:inline-flex;align-items:center;gap:2px;font-size:9px;font-weight:400;color:#5d574c;white-space:nowrap;}' +
      '.pt-calsim3-label-toggle input{width:10px;height:10px;margin:0;}' +
      '.pt-calsim3-label-zoom{font-size:8.5px;color:#6a6254;}' +
      '.pt-calsim3-counts{font-size:10px;color:#514b40;margin-bottom:1px;}' +
      '.pt-calsim3-zoom-note{font-size:9.2px;color:#6a6254;}' +
      '.pt-calsim3-section{border-top:1px solid rgba(112,103,83,.27);margin-top:4px;padding-top:4px;}' +
      '.pt-calsim3-section-title{display:flex;justify-content:space-between;align-items:center;font-weight:700;margin-bottom:2px;}' +
      '.pt-calsim3-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:1px 7px;}' +
      '.pt-calsim3-node-section>summary{list-style:none;cursor:pointer;}' +
      '.pt-calsim3-node-section>summary::-webkit-details-marker{display:none;}' +
      '.pt-calsim3-node-section>summary:before{content:"▸";font-size:9px;margin-right:3px;}' +
      '.pt-calsim3-node-section[open]>summary:before{content:"▾";}' +
      '.pt-calsim3-node-heading{min-width:0;display:flex;align-items:baseline;gap:5px;}' +
      '.pt-calsim3-node-key{font-size:9px;font-weight:400;color:#6a6254;white-space:nowrap;}' +
      '.pt-calsim3-node-grid{max-height:128px;overflow:auto;padding-right:2px;}' +
      '.pt-calsim3-option{display:grid;grid-template-columns:auto auto minmax(0,1fr) auto;align-items:center;gap:3px;min-width:0;min-height:18px;}' +
      '.pt-calsim3-option input{margin:0;}' +
      '.pt-calsim3-option-label{min-width:0;white-space:nowrap;}' +
      '.pt-calsim3-option-count{font-size:9px;color:#6a6254;text-align:right;font-variant-numeric:tabular-nums;}' +
      '.pt-calsim3-line-symbol{display:inline-block;width:18px;height:0;border-top-style:solid;flex:0 0 18px;}' +
      '.pt-calsim3-node-symbol{display:inline-block;width:9px;height:9px;border:1.4px solid;border-radius:50%;box-sizing:border-box;}' +
      '.pt-calsim3-display-row{display:flex;align-items:center;flex-wrap:wrap;gap:3px 10px;}' +
      '.pt-calsim3-display-row label{display:inline-flex;align-items:center;gap:3px;white-space:nowrap;}' +
      '.pt-calsim3-live-row{display:flex;align-items:center;gap:6px;margin-top:3px;padding:2px 4px;border:1px solid #b8ad95;border-radius:3px;background:#f8f5ec;white-space:nowrap;}' +
      '.pt-calsim3-live-row label{display:inline-flex;align-items:center;gap:3px;font-size:10.5px;font-weight:700;white-space:nowrap;}' +
      '.pt-calsim3-live-help{min-width:0;font-size:9px;color:#6a6254;white-space:nowrap;}' +
      '.pt-calsim3-mini-actions{display:inline-flex;gap:3px;}' +
      '.pt-calsim3-explorer button{border:1px solid #b8ad95;border-radius:3px;background:#f8f5ec;color:#222;font:10px/1.2 Arial,sans-serif;padding:2px 5px;cursor:pointer;}' +
      '.pt-calsim3-explorer button:hover{background:#fff;}' +
      '.pt-calsim3-explorer button:disabled{cursor:default;opacity:.5;background:#eee9dc;}' +
      '.pt-calsim3-head .pt-map-card-actions button{border:0!important;background:transparent!important;padding:3px 4px!important;}' +
      '.pt-calsim3-apply-row{display:flex;align-items:center;gap:5px;margin-top:4px;}' +
      '.pt-calsim3-apply{font-weight:700!important;}' +
      '.pt-calsim3-find-row{display:grid;grid-template-columns:minmax(0,1fr) auto;gap:4px;}' +
      '.pt-calsim3-find{width:100%;min-width:0;height:24px;box-sizing:border-box;border:1px solid #b8ad95;border-radius:3px;padding:2px 5px;font-size:11px;}' +
      '.pt-calsim3-finder-results{max-height:190px;overflow:auto;margin-top:3px;display:grid;gap:2px;}' +
      '.pt-calsim3-result{display:grid;grid-template-columns:minmax(0,1fr) auto;gap:3px;align-items:stretch;}' +
      '.pt-calsim3-result-main{display:grid!important;grid-template-columns:34px minmax(0,1fr);gap:4px;text-align:left!important;white-space:normal!important;}' +
      '.pt-calsim3-result-kind{font-size:9px;font-weight:700;color:#6b5a42;text-transform:uppercase;}' +
      '.pt-calsim3-result-text{min-width:0;}' +
      '.pt-calsim3-result-text b,.pt-calsim3-result-text small{display:block;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}' +
      '.pt-calsim3-result-text small{font-size:9px;color:#5f594e;margin-top:1px;}' +
      '.pt-calsim3-show-result{align-self:stretch;}' +
      '.pt-calsim3-finder-nav{display:flex;justify-content:space-between;align-items:center;gap:4px;padding-top:2px;font-size:9.5px;color:#5f594e;}' +
      '.pt-calsim3-status{min-height:13px;margin-top:4px;font-size:10px;color:#514b40;}' +
      '.pt-calsim3-status[data-tone="warn"]{color:#8a3b12;}' +
      '.pt-calsim3-status[data-tone="ok"]{color:#2f6331;}' +
      '.pt-calsim3-note{font-size:9.3px;color:#5d574c;margin-top:3px;}' +
      '.pt-calsim3-label-icon{background:transparent!important;border:0!important;box-shadow:none!important;pointer-events:none!important;white-space:nowrap;}' +
      '.pt-calsim3-label-icon span{display:inline-block;background:rgba(255,255,255,.76);color:#222;font-size:9.5px;font-weight:600;line-height:1.05;padding:1px 2px;text-shadow:0 0 2px #fff,0 0 2px #fff;pointer-events:none!important;white-space:nowrap;}' +
      '.pt-calsim3-label-node span{position:relative;left:4px;top:-7px;}' +
      '.pt-calsim3-label-arc span{position:relative;transform:translate(-50%,-50%);font-size:9px;font-weight:500;}' +
      '.leaflet-container.pt-calsim3-path-hover:not(.pt-measure-active):not(.pt-marquee-zoom-mode){cursor:pointer;}' +
      '@media(max-width:390px){.pt-calsim3-explorer{width:calc(100vw - 18px);font-size:10.5px;padding:6px}.pt-calsim3-grid{column-gap:4px}.pt-calsim3-option{min-height:17px}.pt-calsim3-head{font-size:12px}.pt-calsim3-node-key{font-size:8.7px}}';
    document.head.appendChild(style);
  }

  function writeUiState() {
    if (!card) return;
    var showArcs = card.querySelector('.pt-calsim3-show-arcs');
    var showNodes = card.querySelector('.pt-calsim3-show-nodes');
    var hoverArcs = card.querySelector('.pt-calsim3-hover-arcs');
    var hoverNodes = card.querySelector('.pt-calsim3-hover-nodes');
    if (showArcs) showArcs.checked = state.showArcs;
    if (showNodes) showNodes.checked = state.showNodes;
    if (hoverArcs) hoverArcs.checked = state.hoverArcs;
    if (hoverNodes) hoverNodes.checked = state.hoverNodes;
    updateExplorerLabelControl();
    Array.prototype.forEach.call(
      card.querySelectorAll('input[data-filter-kind="arc"]'),
      function(input) {
        input.checked = state.arcTypes[input.value] === true;
      }
    );
    Array.prototype.forEach.call(
      card.querySelectorAll('input[data-filter-kind="node"]'),
      function(input) {
        input.checked = state.nodeGroups[input.value] === true;
      }
    );
    updateCounts();
    updateApplyUi();
  }

  function readUiOptions() {
    var arcTypes = [];
    var nodeGroups = [];
    Array.prototype.forEach.call(
      card.querySelectorAll('input[data-filter-kind="arc"]:checked'),
      function(input) {
        arcTypes.push(input.value);
      }
    );
    Array.prototype.forEach.call(
      card.querySelectorAll('input[data-filter-kind="node"]:checked'),
      function(input) {
        nodeGroups.push(input.value);
      }
    );
    return {
      showArcs: !!card.querySelector('.pt-calsim3-show-arcs').checked,
      showNodes: !!card.querySelector('.pt-calsim3-show-nodes').checked,
      arcTypes: arcTypes,
      nodeGroups: nodeGroups
    };
  }

  function setUiFacet(kind, selected) {
    Array.prototype.forEach.call(
      card.querySelectorAll(
        'input[data-filter-kind="' + kind + '"]'
      ),
      function(input) {
        input.checked = selected;
      }
    );
    scheduleUiApply(kind + (selected ? '-all' : '-none'));
  }

  function wireCard() {
    if (!card) return;
    var close = card.querySelector('.pt-calsim3-close');
    if (close) {
      close.__brimLegendCloseoutWired = true;
      close.addEventListener('click', function(event) {
        event.preventDefault();
        event.stopPropagation();
        cardHiddenByUser = true;
        dockAndHideCard();
      }, false);
    }

    card.addEventListener('change', function(event) {
      var target = event.target;
      if (!target) return;
      if (target.classList.contains('pt-calsim3-hover-arcs')) {
        setHover('arcs', target.checked);
        setStatus('Arc hover ' + (target.checked ? 'on.' : 'off.'), 'normal');
      } else if (target.classList.contains('pt-calsim3-hover-nodes')) {
        setHover('nodes', target.checked);
        setStatus('Node hover ' + (target.checked ? 'on.' : 'off.'), 'normal');
      } else if (target.classList.contains('pt-calsim3-auto')) {
        setAutoApply(target.checked);
      } else if (target.classList.contains('pt-calsim3-labels')) {
        setLabelsRequested(target.checked, 'explorer-label-control');
      } else if (
        target.classList.contains('pt-calsim3-show-arcs') ||
        target.classList.contains('pt-calsim3-show-nodes') ||
        target.getAttribute('data-filter-kind')
      ) {
        scheduleUiApply('selection-change');
      }
    }, false);

    card.addEventListener('input', function(event) {
      var target = event.target;
      if (
        target &&
        target.classList &&
        target.classList.contains('pt-calsim3-find')
      ) {
        renderFinderResults(target.value);
      }
    }, false);

    card.addEventListener('click', function(event) {
      var button = (
        event.target &&
        event.target.closest
      ) ? event.target.closest('button[data-action]') : null;
      if (!button) return;
      event.preventDefault();
      event.stopPropagation();
      var action = button.getAttribute('data-action');
      var key = button.getAttribute('data-record-key');
      if (action === 'apply') applyOptions(readUiOptions(), 'manual');
      if (action === 'reset') resetState();
      if (action === 'arc-all') setUiFacet('arc', true);
      if (action === 'arc-none') setUiFacet('arc', false);
      if (action === 'node-all') setUiFacet('node', true);
      if (action === 'node-none') setUiFacet('node', false);
      if (action === 'focus-result') focusRecord(key);
      if (action === 'show-result') showFinderRecord(key);
      if (action === 'finder-more') {
        renderFinderResults(lastFinderQuery, 'more');
      }
      if (action === 'finder-all') {
        renderFinderResults(lastFinderQuery, 'all');
      }
      if (action === 'finder-fewer') {
        renderFinderResults(lastFinderQuery, 'fewer');
      }
      if (action === 'clear-finder') {
        clearFinderTransient('finder-clear');
        setStatus('Finder cleared.', 'normal');
      }
    }, false);
  }

  function buildCard() {
    if (
      typeof document === 'undefined' ||
      !document.createElement ||
      typeof L === 'undefined' ||
      !L.control ||
      !L.DomUtil ||
      !el ||
      typeof el.querySelector !== 'function'
    ) {
      return;
    }
    ensureCss();
    var arcCounts = typeCounts(arcRecords, 'type', arcTypeOrder);
    var nodeCounts = typeCounts(nodeRecords, 'group', nodeGroupOrder);

    cardControl = L.control({position: 'bottomleft'});
    cardControl.onAdd = function() {
      var div = L.DomUtil.create(
        'div',
        'leaflet-control pt-map-legend-card pt-map-legend-local ' +
        'pt-calsim3-explorer'
      );
      div.style.display = 'none';
      var html = '';
      html += '<div class="pt-calsim3-head pt-map-card-handle">';
      html += '<span class="pt-calsim3-head-title">CalSim3.0 Network Explorer</span>';
      html += '<span class="pt-map-card-actions pt-calsim3-head-actions">';
      html += '<label class="pt-calsim3-label-toggle" title="Show CalSim3 labels at zoom 11 and deeper"><input type="checkbox" class="pt-calsim3-labels"> lbl <span class="pt-calsim3-label-zoom">z11+</span></label>';
      html += '<button type="button" class="pt-map-card-dock pt-calsim3-dock" aria-label="Undock CalSim3.0 Network Explorer" title="Undock CalSim3.0 Network Explorer">&#x2197;</button>';
      html += '<button type="button" class="pt-map-legend-close pt-calsim3-close" aria-label="Hide CalSim3.0 Network Explorer" title="Hide CalSim3.0 Network Explorer">&times;</button>';
      html += '</span></div>';
      html += '<div class="pt-calsim3-counts"></div>';
      html += '<div class="pt-calsim3-zoom-note">Nodes decluster at zoom 9; exact same-coordinate records spiderfy.</div>';
      html += '<div class="pt-calsim3-section"><div class="pt-calsim3-section-title">Display</div>';
      html += '<div class="pt-calsim3-display-row">';
      html += '<label><input type="checkbox" class="pt-calsim3-show-arcs" checked> Arcs</label>';
      html += '<label><input type="checkbox" class="pt-calsim3-show-nodes" checked> Nodes</label>';
      html += '<label><input type="checkbox" class="pt-calsim3-hover-arcs" checked> Hover arcs</label>';
      html += '<label><input type="checkbox" class="pt-calsim3-hover-nodes" checked> Hover nodes</label>';
      html += '</div></div>';
      html += '<div class="pt-calsim3-live-row" title="When enabled, filter selections update automatically. Turn off to use Apply.">';
      html += '<label><input type="checkbox" class="pt-calsim3-auto pt-calsim3-live-apply" checked> Live apply</label>';
      html += '<span class="pt-calsim3-live-help">Updates filters as selections change.</span></div>';
      html += '<div class="pt-calsim3-section"><div class="pt-calsim3-section-title"><span>Arc types</span><span class="pt-calsim3-mini-actions"><button type="button" data-action="arc-all">All</button><button type="button" data-action="arc-none">None</button></span></div>';
      html += '<div class="pt-calsim3-grid">';
      arcTypeOrder.forEach(function(value) {
        var sample = representative(arcRecords, 'type', value);
        html += checkboxHtml(
          'arc',
          value,
          value,
          arcCounts[value],
          lineSymbol(sample.color, sample.weight),
          value
        );
      });
      html += '</div></div>';
      html += '<details class="pt-calsim3-section pt-calsim3-node-section" open><summary class="pt-calsim3-section-title"><span class="pt-calsim3-node-heading"><span>Node groups</span><span class="pt-calsim3-node-key">P = project · NP = non-project</span></span><span class="pt-calsim3-mini-actions"><button type="button" data-action="node-all">All</button><button type="button" data-action="node-none">None</button></span></summary>';
      html += '<div class="pt-calsim3-grid pt-calsim3-node-grid">';
      nodeGroupOrder.forEach(function(value) {
        var sample = representative(nodeRecords, 'group', value);
        html += checkboxHtml(
          'node',
          value,
          nodeGroupDisplayLabel(value),
          nodeCounts[value],
          nodeSymbol(sample.fill, sample.stroke),
          value
        );
      });
      html += '</div></details>';
      html += '<div class="pt-calsim3-apply-row"><button type="button" class="pt-calsim3-apply" data-action="apply" disabled>Apply</button><button type="button" data-action="reset">Reset</button></div>';
      html += '<div class="pt-calsim3-status" aria-live="polite">Live apply on.</div>';
      html += '<div class="pt-calsim3-section"><div class="pt-calsim3-section-title">Finder</div>';
      html += '<div class="pt-calsim3-find-row"><input type="search" class="pt-calsim3-find" placeholder="Arc ID, endpoint, node, river…"><button type="button" data-action="clear-finder">Clear</button></div>';
      html += '<div class="pt-calsim3-finder-results"></div></div>';
      html += '<div class="pt-calsim3-note">Hover controls tooltips only; click popups remain available.</div>';
      div.innerHTML = html;
      if (L.DomEvent.disableClickPropagation) {
        L.DomEvent.disableClickPropagation(div);
      }
      if (L.DomEvent.disableScrollPropagation) {
        L.DomEvent.disableScrollPropagation(div);
      }
      card = div;
      return div;
    };
    cardControl.addTo(map);
    wireCard();
    writeUiState();
    updateCardVisibility();
  }

  function domCounts() {
    if (!el || typeof el.querySelectorAll !== 'function') {
      return {
        markerClusters: null,
        svgPaths: null,
        canvasElements: null,
        labelIcons: null
      };
    }
    return {
      markerClusters: el.querySelectorAll('.marker-cluster').length,
      svgPaths: el.querySelectorAll('path.leaflet-interactive').length,
      canvasElements: el.querySelectorAll('canvas.leaflet-zoom-animated').length,
      labelIcons: el.querySelectorAll('.pt-calsim3-label-icon').length
    };
  }

  function handlerState(handler) {
    if (!handler) return null;
    var enabled = null;
    var moving = null;
    try {
      if (typeof handler.enabled === 'function') {
        enabled = !!handler.enabled();
      }
    } catch (handlerEnabledError) {}
    try {
      if (typeof handler.moving === 'function') {
        moving = !!handler.moving();
      }
    } catch (handlerMovingError) {}
    return {
      enabled: enabled,
      moving: moving
    };
  }

  function nodeStyleValue(node, property) {
    if (!node) return '';
    if (
      node.style &&
      node.style[property] !== undefined &&
      node.style[property] !== ''
    ) {
      return String(node.style[property]);
    }
    try {
      if (
        typeof window !== 'undefined' &&
        typeof window.getComputedStyle === 'function'
      ) {
        return String(window.getComputedStyle(node)[property] || '');
      }
    } catch (computedStyleError) {}
    return '';
  }

  function paneInteractionStates() {
    var panes = null;
    try {
      panes = map && typeof map.getPanes === 'function' ?
        map.getPanes() : map._panes;
    } catch (paneStateError) {
      panes = null;
    }
    if (!panes) return [];
    return Object.keys(panes).sort().map(function(name) {
      var pane = panes[name];
      return {
        name: name,
        zIndex: nodeStyleValue(pane, 'zIndex'),
        pointerEvents: nodeStyleValue(pane, 'pointerEvents'),
        measureSuspended: !!(
          pane &&
          typeof pane.getAttribute === 'function' &&
          pane.getAttribute('data-pt-measure-suspended') === 'true'
        )
      };
    });
  }

  function elementDescriptor(node) {
    if (!node) return null;
    var pane = null;
    var rect = null;
    var mapRect = null;
    try {
      pane = node.closest ? node.closest('.leaflet-pane') : node.parentNode;
    } catch (closestPaneError) {}
    try {
      if (typeof node.getBoundingClientRect === 'function') {
        rect = node.getBoundingClientRect();
      }
      var container = mapContainer();
      if (
        container &&
        typeof container.getBoundingClientRect === 'function'
      ) {
        mapRect = container.getBoundingClientRect();
      }
    } catch (elementBoundsError) {}
    return {
      tag: clean(node.tagName).toLocaleLowerCase(),
      id: clean(node.id),
      className: clean(
        typeof node.className === 'string' ?
          node.className :
          (node.getAttribute ? node.getAttribute('class') : '')
      ),
      pointerEvents: nodeStyleValue(node, 'pointerEvents'),
      zIndex: nodeStyleValue(node, 'zIndex'),
      paneId: clean(pane && pane.id),
      paneZIndex: nodeStyleValue(pane, 'zIndex'),
      panePointerEvents: nodeStyleValue(pane, 'pointerEvents'),
      paneClassName: clean(
        pane && (
          typeof pane.className === 'string' ?
            pane.className :
            (pane.getAttribute ? pane.getAttribute('class') : '')
        )
      ),
      width: rect ? Number(rect.width || 0) : null,
      height: rect ? Number(rect.height || 0) : null,
      coversViewport: !!(
        rect &&
        mapRect &&
        Number(rect.width || 0) >= Number(mapRect.width || 0) - 1 &&
        Number(rect.height || 0) >= Number(mapRect.height || 0) - 1
      )
    };
  }

  function boundedElements(selector, maximum) {
    if (!el || typeof el.querySelectorAll !== 'function') return [];
    var nodes = [];
    try {
      nodes = Array.prototype.slice.call(el.querySelectorAll(selector));
    } catch (selectorError) {
      return [];
    }
    return nodes.slice(0, maximum).map(elementDescriptor);
  }

  function currentOwner(source) {
    if (!source) return null;
    var component = componentForSource(source);
    if (component) return 'calsim3-' + component;
    if (source === map) return 'map';
    return 'unrelated-layer';
  }

  function globalInteractionState() {
    var container = mapContainer();
    var renderers = rendererState();
    var popup = observedPopup || currentPopup || (map && map._popup) || null;
    return {
      measure: {
        mapFlag: !!(map && map._ptMeasureInteractionActive),
        containerClass: containerHasClass('pt-measure-active')
      },
      marquee: {
        activeClass: containerHasClass('pt-marquee-zoom-mode'),
        rectangleCount: boundedElements(
          '.pt-marquee-zoom-rect',
          4
        ).length
      },
      teachingMarkup: {
        activeClass: containerHasClass('pt-teaching-markup-active'),
        labelPlacementClass: containerHasClass(
          'pt-teaching-label-active'
        )
      },
      dragging: handlerState(map && map.dragging),
      boxZoom: handlerState(map && map.boxZoom),
      panes: paneInteractionStates(),
      renderer: renderers,
      viewportCanvases: boundedElements('canvas', 40),
      knownFullMapInteractionElements: boundedElements(
        '.pt-marquee-zoom-rect,.leaflet-zoom-box',
        12
      ),
      popupOwner: currentOwner(popup && popup._source),
      tooltipOwner: currentOwner(
        (observedTooltip || currentTooltip) &&
        (observedTooltip || currentTooltip)._source
      ),
      calSim: {
        hoverArcs: state.hoverArcs,
        hoverNodes: state.hoverNodes,
        labelsRequested: labelsRequested,
        visibleLabels: visibleLabelDescriptors.length,
        labelWorkPending: labelTimer !== null || labelFrame !== null,
        cursorClass: containerHasClass('pt-calsim3-path-hover'),
        finderFocusPending: focusTimer !== null,
        finderHighlightKey: currentHighlight ?
          currentHighlight.key : null,
        mapListenerCount: listenerRecords.length,
        forwardingRendererEvent: forwardingSharedRendererEvent
      }
    };
  }

  function hitTargetAt(xCoordinate, yCoordinate) {
    var xNumber = Number(xCoordinate);
    var yNumber = Number(yCoordinate);
    if (
      !isFinite(xNumber) ||
      !isFinite(yNumber) ||
      typeof document === 'undefined' ||
      typeof document.elementFromPoint !== 'function'
    ) {
      return null;
    }
    try {
      return elementDescriptor(
        document.elementFromPoint(xNumber, yNumber)
      );
    } catch (hitTargetError) {
      return null;
    }
  }

  function visibleClusterCounts() {
    var out = {
      visibleClusterIcons: 0,
      visibleIndividualNodes: 0
    };
    var visibleRoot = clusterGroup && clusterGroup._featureGroup;
    if (!visibleRoot || typeof visibleRoot.eachLayer !== 'function') {
      return out;
    }
    visibleRoot.eachLayer(function(layer) {
      if (
        typeof L !== 'undefined' &&
        L.MarkerCluster &&
        layer instanceof L.MarkerCluster
      ) {
        out.visibleClusterIcons += 1;
      } else {
        out.visibleIndividualNodes += 1;
      }
    });
    return out;
  }

  function actualArcCount() {
    if (!groupRoot || typeof groupRoot.hasLayer !== 'function') return null;
    return arcRecords.reduce(function(total, record) {
      return total + (
        record.layer && groupRoot.hasLayer(record.layer) ? 1 : 0
      );
    }, 0);
  }

  function stats() {
    var visibleCounts = visibleClusterCounts();
    var renderers = rendererState();
    var radiusTier = nodeRadiusTierForZoom(currentMapZoom());
    var baselineRadii = nodeRecords.map(function(record) {
      return asNumber(record.radius, 4.7);
    });
    var effectiveRadii = nodeRecords.map(function(record) {
      if (
        record.layer &&
        typeof record.layer.getRadius === 'function'
      ) {
        try {
          var effectiveRadius = Number(record.layer.getRadius());
          if (isFinite(effectiveRadius)) return effectiveRadius;
        } catch (effectiveRadiusReadError) {}
      }
      return asNumber(record.displayRadius, asNumber(record.radius, 4.7));
    });
    var baselineRadiusMin = baselineRadii.length ?
      Math.min.apply(null, baselineRadii) : null;
    var baselineRadiusMax = baselineRadii.length ?
      Math.max.apply(null, baselineRadii) : null;
    var effectiveRadiusMin = effectiveRadii.length ?
      Math.min.apply(null, effectiveRadii) : null;
    var effectiveRadiusMax = effectiveRadii.length ?
      Math.max.apply(null, effectiveRadii) : null;
    var registeredNodes = (
      clusterGroup && typeof clusterGroup.getLayers === 'function'
    ) ? clusterGroup.getLayers().length : 0;
    var heap = (
      typeof performance !== 'undefined' &&
      performance.memory
    ) ? {
      usedJSHeapSize: performance.memory.usedJSHeapSize,
      totalJSHeapSize: performance.memory.totalJSHeapSize,
      jsHeapSizeLimit: performance.memory.jsHeapSizeLimit
    } : null;
    var counts = shownCounts();
    var visibleNodeLabels = visibleLabelDescriptors.filter(
      function(descriptor) {
        return descriptor.kind === 'node';
      }
    ).length;

    return {
      groupName: groupName,
      clusterId: clusterId,
      active: networkActive(),
      zoom: (
        map && typeof map.getZoom === 'function'
      ) ? map.getZoom() : null,
      transitionZoom: transitionZoom,
      sameLocationMode: calsimData.sameLocationMode || null,
      analyticalArcRecords: Number(
        calsimData.arcRecordCount || arcRecords.length
      ),
      analyticalNodeRecords: Number(
        calsimData.analyticalRecordCount || nodeRecords.length
      ),
      resolvedArcObjects: arcRecords.length -
        diagnostics.unresolvedArcObjects,
      resolvedNodeObjects: nodeRecords.length -
        diagnostics.unresolvedNodeObjects,
      actualArcObjectsInGroup: actualArcCount(),
      registeredNodeMarkers: registeredNodes,
      shownArcRecords: counts.arcs,
      shownNodeRecords: counts.nodes,
      showArcs: state.showArcs,
      showNodes: state.showNodes,
      selectedArcTypes: selectedValues(state.arcTypes, arcTypeOrder),
      selectedNodeGroups: selectedValues(
        state.nodeGroups,
        nodeGroupOrder
      ),
      hoverArcs: state.hoverArcs,
      hoverNodes: state.hoverNodes,
      autoApply: autoApply,
      liveApply: autoApply,
      uiDirty: uiDirty,
      labels: {
        groupName: labelGroupName,
        requested: labelsRequested,
        eligible: labelsEligible(),
        minimumZoom: labelMinZoom,
        cap: labelCap,
        nodeCap: labelNodeCap,
        arcCap: labelArcCap,
        viewportPadRatio: labelViewportPadRatio,
        gridDegrees: labelGridDegrees,
        descriptorCount: labelDescriptors.length,
        gridCellCount: labelGridCellCount,
        visibleCount: visibleLabelDescriptors.length,
        visibleNodeCount: visibleNodeLabels,
        visibleArcCount:
          visibleLabelDescriptors.length - visibleNodeLabels,
        pending: labelTimer !== null || labelFrame !== null,
        mapMoving: labelMapMoving,
        pointerIntercept: false,
        nodeRule: 'retained node ID',
        arcRules: [
          'meaningful retained Name',
          'explicit FromNode → ToNode',
          'retained Arc_ID'
        ]
      },
      nodeRadius: {
        requestedTier: radiusTier.key,
        requestedScale: radiusTier.scale,
        appliedTier: appliedNodeRadiusTier ?
          appliedNodeRadiusTier.key : null,
        appliedScale: appliedNodeRadiusTier ?
          appliedNodeRadiusTier.scale : null,
        baselineMinimum: baselineRadiusMin,
        baselineMaximum: baselineRadiusMax,
        targetMinimum: baselineRadiusMin === null ?
          null : baselineRadiusMin * radiusTier.scale,
        targetMaximum: baselineRadiusMax === null ?
          null : baselineRadiusMax * radiusTier.scale,
        effectiveMinimum: effectiveRadiusMin,
        effectiveMaximum: effectiveRadiusMax
      },
      validNodeCoordinates: Number(
        calsimData.validCoordinateCount || 0
      ),
      uniqueExactCoordinateLocations: Number(
        calsimData.uniqueExactCoordinateCount || 0
      ),
      exactDuplicateLocationCount: Number(
        calsimData.exactDuplicateLocationCount || 0
      ),
      recordsAtExactDuplicateLocations: Number(
        calsimData.recordsAtExactDuplicateLocations || 0
      ),
      maxRecordsAtExactLocation: Number(
        calsimData.maxRecordsAtExactLocation || 1
      ),
      visibleClusterIcons: visibleCounts.visibleClusterIcons,
      visibleIndividualNodes: visibleCounts.visibleIndividualNodes,
      spiderfied: !!(clusterGroup && clusterGroup._spiderfied),
      measureActive: !!map._ptMeasureInteractionActive,
      listenerCount: listenerRecords.length,
      finderResultCount: lastFinderResults.length,
      finderVisibleLimit: finderVisibleLimit,
      finderQuery: lastFinderQuery,
      cardVisible: !!(
        card &&
        card.style &&
        card.style.display !== 'none'
      ),
      cardDetached: !!(
        card &&
        card.__brimDetachableState &&
        card.__brimDetachableState.floating
      ),
      pendingAsyncFilterWork: applyFrame !== null,
      renderer: renderers,
      interaction: globalInteractionState(),
      diagnostics: Object.assign({}, diagnostics),
      dom: domCounts(),
      heap: heap
    };
  }

  function profileRequestedByUrl() {
    try {
      return /(?:^|[?&])brimProfile=1(?:&|$)/.test(
        window.location.search || ''
      );
    } catch (profileUrlError) {
      return false;
    }
  }

  function enableProfile() {
    profileEnabled = true;
    if (
      longTaskObserver ||
      typeof PerformanceObserver === 'undefined'
    ) {
      return stats();
    }
    try {
      longTaskObserver = new PerformanceObserver(function(list) {
        list.getEntries().forEach(function(entry) {
          diagnostics.profileLongTaskCount += 1;
          diagnostics.profileLongTaskDurationMs += Number(
            entry.duration || 0
          );
        });
      });
      longTaskObserver.observe({entryTypes: ['longtask']});
    } catch (longTaskError) {
      longTaskObserver = null;
    }
    return stats();
  }

  function disableProfile() {
    profileEnabled = false;
    if (longTaskObserver) {
      try {
        longTaskObserver.disconnect();
      } catch (observerDisconnectError) {}
      longTaskObserver = null;
    }
    return stats();
  }

  function startScenario(name) {
    if (!profileEnabled) enableProfile();
    activeScenario = {
      name: String(name || 'calsim3-scenario'),
      startedAt: (
        typeof performance !== 'undefined' &&
        typeof performance.now === 'function'
      ) ? performance.now() : Date.now(),
      before: stats()
    };
    return activeScenario.before;
  }

  function endScenario() {
    if (!activeScenario) return null;
    var endedAt = (
      typeof performance !== 'undefined' &&
      typeof performance.now === 'function'
    ) ? performance.now() : Date.now();
    var report = {
      name: activeScenario.name,
      durationMs: endedAt - activeScenario.startedAt,
      before: activeScenario.before,
      after: stats()
    };
    profileReports.push(report);
    activeScenario = null;
    return report;
  }

  function downloadProfileJson() {
    var json = JSON.stringify(profileReports, null, 2);
    if (
      typeof Blob === 'undefined' ||
      typeof URL === 'undefined' ||
      typeof document === 'undefined'
    ) {
      return json;
    }
    var blob = new Blob([json], {type: 'application/json'});
    var url = URL.createObjectURL(blob);
    var link = document.createElement('a');
    link.href = url;
    link.download = 'brim-calsim3-profile.json';
    document.body.appendChild(link);
    link.click();
    link.remove();
    URL.revokeObjectURL(url);
    return json;
  }

  function restoreAllObjects() {
    if (groupRoot) {
      arcRecords.forEach(function(record) {
        if (
          record.layer &&
          groupRoot.addLayer &&
          (
            !groupRoot.hasLayer ||
            !groupRoot.hasLayer(record.layer)
          )
        ) {
          groupRoot.addLayer(record.layer);
        }
      });
    }
    if (clusterGroup) {
      nodeRecords.forEach(function(record) {
        if (
          record.layer &&
          clusterGroup.addLayer &&
          (
            !clusterGroup.hasLayer ||
            !clusterGroup.hasLayer(record.layer)
          )
        ) {
          clusterGroup.addLayer(record.layer);
        }
      });
      if (
        groupRoot &&
        groupRoot.addLayer &&
        (
          !groupRoot.hasLayer ||
          !groupRoot.hasLayer(clusterGroup)
        )
      ) {
        groupRoot.addLayer(clusterGroup);
      }
    }
  }

  var api = null;

  function destroy(restoreObjects) {
    if (destroyed) return;
    destroyed = true;
    cancelPendingApply('destroy');
    clearSharedRendererHover('destroy');
    clearTransientInteraction('destroy');
    clearFinderTransient('destroy');
    cancelLabelWork('destroy', true);
    initialTimers.forEach(clearTimeout);
    initialTimers = [];

    listenerRecords.forEach(function(record) {
      if (
        record.target &&
        typeof record.target.off === 'function'
      ) {
        record.target.off(record.type, record.handler);
      }
    });
    listenerRecords = [];

    if (restoreObjects === true) restoreAllObjects();
    if (
      card &&
      card.__brimDetachableState &&
      card.__brimDetachableState.destroy
    ) {
      card.__brimDetachableState.destroy(false);
    }
    if (cardControl && typeof cardControl.remove === 'function') {
      cardControl.remove();
    } else if (
      cardControl &&
      map &&
      typeof map.removeControl === 'function'
    ) {
      map.removeControl(cardControl);
    } else if (card && card.parentNode) {
      card.parentNode.removeChild(card);
    }
    card = null;
    cardControl = null;
    invalidateNodeRadiusTier('destroy');
    disableProfile();
    if (window.BRIM_CALSIM3_LOCAL === api) {
      delete window.BRIM_CALSIM3_LOCAL;
    }
  }

  buildLabelIndex();
  buildCard();
  observedNetworkActive = networkActive();
  applyNodeRadiusTier('controller-install');
  syncLabelsFromSharedControl('controller-install', false);

  if (hasExactDuplicateLocations && clusterGroup) {
    listen(clusterGroup, 'clusterclick', onClusterClick);
  }
  listen(map, 'mousemove', onMapMouseMove);
  listen(map, 'click', onMapClick);
  listen(map, 'mouseout', onMapMouseOut);
  listen(map, 'tooltipopen', onTooltipOpen);
  listen(map, 'tooltipclose', onTooltipClose);
  listen(map, 'popupopen', onPopupOpen);
  listen(map, 'popupclose', onPopupClose);
  listen(map, 'overlayadd', onOverlayAdd);
  listen(map, 'overlayremove', onOverlayRemove);
  listen(map, 'layeradd', onLayerAdd);
  listen(map, 'layerremove', onLayerRemove);
  listen(map, 'movestart', function() {
    labelMapMoving = true;
    cancelLabelWork('move-start', true);
    clearSharedRendererHover('move-start');
    clearTransientInteraction('move-start');
    cancelPendingFocus('move-start');
    restoreHighlight();
  });
  listen(map, 'zoomstart', function() {
    labelMapMoving = true;
    cancelLabelWork('zoom-start', true);
    clearSharedRendererHover('zoom-start');
    clearTransientInteraction('zoom-start');
    cancelPendingFocus('zoom-start');
    restoreHighlight();
  });
  listen(map, 'zoomend', function() {
    labelMapMoving = false;
    applyNodeRadiusTier('zoom-end');
    scheduleLabelRender('zoom-end', 45);
  });
  listen(map, 'moveend', function() {
    labelMapMoving = false;
    scheduleLabelRender('move-end', 45);
  });
  listen(map, 'pt:measureinteractionchange', onMeasureInteractionChange);
  listen(map, 'unload', function() {
    destroy(false);
  });

  api = {
    stats: stats,
    apply: applyOptions,
    scheduleApply: function(options) {
      scheduleUiApply('api', options || {});
      return stats();
    },
    setAutoApply: setAutoApply,
    setLiveApply: setAutoApply,
    setLabels: function(enabled) {
      setLabelsRequested(enabled, 'api-label-control');
      return stats();
    },
    reset: resetState,
    setHover: setHover,
    search: function(query) {
      return searchRecords(query).map(publicFinderRecord);
    },
    finderPage: finderPage,
    nodeGroupDisplayLabel: nodeGroupDisplayLabel,
    nodeRadiusTier: function(zoom) {
      return Object.assign(
        {},
        nodeRadiusTierForZoom(
          zoom === undefined ? currentMapZoom() : zoom
        )
      );
    },
    labelRules: function() {
      return {
        minimumZoom: labelMinZoom,
        cap: labelCap,
        nodeCap: labelNodeCap,
        arcCap: labelArcCap,
        node: 'retained node ID',
        arc: [
          'meaningful retained Name',
          'explicit FromNode → ToNode',
          'retained Arc_ID'
        ]
      };
    },
    interactionState: globalInteractionState,
    hitTargetAt: hitTargetAt,
    focus: focusRecord,
    showResult: showFinderRecord,
    hideCard: function() {
      cardHiddenByUser = true;
      dockAndHideCard();
      return stats();
    },
    destroy: destroy,
    closeTransientInteraction: function() {
      clearTransientInteraction('console');
    }
  };
  window.BRIM_CALSIM3_LOCAL = api;
  window.BRIM_CALSIM3_PROFILE = {
    enable: enableProfile,
    disable: disableProfile,
    startScenario: startScenario,
    endScenario: endScenario,
    downloadJson: downloadProfileJson,
    reports: function() {
      return profileReports.slice();
    }
  };

  [0, 300, 1000].forEach(function(delay) {
    initialTimers.push(setTimeout(function() {
      syncLabelsFromSharedControl('initial-control-sync', false);
      updateCardVisibility();
      updateCounts();
    }, delay));
  });
  if (
    diagnostics.unresolvedArcObjects ||
    diagnostics.unresolvedNodeObjects
  ) {
    setStatus(
      'CalSim object reconciliation failed; see browser console.',
      'warn'
    );
    console.warn(
      'BRIM CalSim3 controller: retained records did not reconcile with ' +
      'browser objects.',
      {
        unresolvedArcs: diagnostics.unresolvedArcObjects,
        unresolvedNodes: diagnostics.unresolvedNodeObjects
      }
    );
  }
  if (profileRequestedByUrl()) enableProfile();
}
