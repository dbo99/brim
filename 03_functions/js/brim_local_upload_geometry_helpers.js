function ptInitBrimLocalUploadGeometryHelpers(el, x) {
  // Phase 1: pure geometry diagnostics for browser-session Local GIS uploads.
  //
  // Vertex convention: one stored coordinate position is one vertex. Polygon
  // ring-closing positions count because they are present in the source. Extra
  // coordinate dimensions and JavaScript array containers never count.
  window.BRIM = window.BRIM || {};

  var BASE_TYPE_ORDER = ['Point', 'Line', 'Polygon', 'Unknown'];
  var PERFORMANCE_THRESHOLDS = {
    detailedFeatureCount: 5000,
    detailedVertexCount: 100000,
    pointFeatureCount: 10000
  };

  function isCoordinatePosition(value) {
    return Array.isArray(value) &&
      value.length >= 2 &&
      typeof value[0] === 'number' &&
      typeof value[1] === 'number';
  }

  function walkCoordinates(value, callback) {
    if (!Array.isArray(value)) return true;
    if (isCoordinatePosition(value)) {
      return callback(value) !== false;
    }
    for (var i = 0; i < value.length; i++) {
      if (!walkCoordinates(value[i], callback)) return false;
    }
    return true;
  }

  function walkGeometry(geometry, callbacks) {
    callbacks = callbacks || {};
    if (!geometry || typeof geometry !== 'object') {
      if (callbacks.onNull) callbacks.onNull(geometry);
      return true;
    }

    var type = String(geometry.type || '');
    if (callbacks.onGeometry) callbacks.onGeometry(geometry, type);

    if (type === 'GeometryCollection') {
      var geometries = Array.isArray(geometry.geometries) ? geometry.geometries : [];
      for (var i = 0; i < geometries.length; i++) {
        if (!walkGeometry(geometries[i], callbacks)) return false;
      }
      return true;
    }

    return walkCoordinates(geometry.coordinates, function(position) {
      if (callbacks.onCoordinate) return callbacks.onCoordinate(position, geometry, type);
      return true;
    });
  }

  function baseGeometryType(type) {
    type = String(type || '');
    if (type === 'Point' || type === 'MultiPoint') return 'Point';
    if (type === 'LineString' || type === 'MultiLineString') return 'Line';
    if (type === 'Polygon' || type === 'MultiPolygon') return 'Polygon';
    if (type === 'GeometryCollection') return '';
    return type ? 'Unknown' : '';
  }

  function orderedBaseTypes(typeSet) {
    return BASE_TYPE_ORDER.filter(function(type) {
      return !!typeSet[type];
    });
  }

  function geometryLabel(baseTypes, geometryCollectionCount) {
    baseTypes = Array.isArray(baseTypes) ? baseTypes : [];
    if (baseTypes.length === 1) return baseTypes[0];
    if (baseTypes.length > 1) return 'Mixed (' + baseTypes.join(' + ') + ')';
    if (geometryCollectionCount > 0) return 'Geometry collection';
    return 'Unknown';
  }

  function featureCollectionMetrics(featureCollection) {
    var features = featureCollection && Array.isArray(featureCollection.features) ?
      featureCollection.features : [];
    var typeSet = Object.create(null);
    var vertexCount = 0;
    var eligibleVertexCount = 0;
    var pointVertexCount = 0;
    var nullGeometryCount = 0;
    var emptyGeometryCount = 0;
    var geometryCollectionCount = 0;

    features.forEach(function(feature) {
      var geometry = feature && Object.prototype.hasOwnProperty.call(feature, 'geometry') ?
        feature.geometry : null;
      if (!geometry) {
        nullGeometryCount += 1;
        return;
      }

      var featureVertexCount = 0;
      walkGeometry(geometry, {
        onGeometry: function(_member, type) {
          if (type === 'GeometryCollection') geometryCollectionCount += 1;
          var baseType = baseGeometryType(type);
          if (baseType) typeSet[baseType] = true;
        },
        onCoordinate: function(_position, _geometry, type) {
          vertexCount += 1;
          featureVertexCount += 1;
          var baseType = baseGeometryType(type);
          if (baseType === 'Line' || baseType === 'Polygon') eligibleVertexCount += 1;
          if (baseType === 'Point') pointVertexCount += 1;
        }
      });

      if (featureVertexCount === 0) emptyGeometryCount += 1;
    });

    var baseTypes = orderedBaseTypes(typeSet);
    return {
      featureCount: features.length,
      vertexCount: vertexCount,
      eligibleVertexCount: eligibleVertexCount,
      pointVertexCount: pointVertexCount,
      baseTypes: baseTypes,
      geometryLabel: geometryLabel(baseTypes, geometryCollectionCount),
      pointOnly: baseTypes.length === 1 && baseTypes[0] === 'Point',
      hasSimplifiableGeometry: baseTypes.indexOf('Line') >= 0 || baseTypes.indexOf('Polygon') >= 0,
      nullGeometryCount: nullGeometryCount,
      emptyGeometryCount: emptyGeometryCount,
      geometryCollectionCount: geometryCollectionCount
    };
  }

  function performanceWarning(metrics) {
    metrics = metrics || {};
    if (metrics.pointOnly && (
      Number(metrics.featureCount || 0) >= PERFORMANCE_THRESHOLDS.pointFeatureCount ||
      Number(metrics.vertexCount || 0) >= PERFORMANCE_THRESHOLDS.pointFeatureCount
    )) {
      return 'Large point layers may slow map interaction. Vertex reduction does not reduce point count.';
    }

    if (metrics.hasSimplifiableGeometry && (
      Number(metrics.featureCount || 0) >= PERFORMANCE_THRESHOLDS.detailedFeatureCount ||
      Number(metrics.vertexCount || 0) >= PERFORMANCE_THRESHOLDS.detailedVertexCount
    )) {
      return 'Highly detailed geometry may slow map interaction. Try a higher vertex reduction setting.';
    }

    return '';
  }

  function forEachCoordinate(featureCollection, callback) {
    var features = featureCollection && Array.isArray(featureCollection.features) ?
      featureCollection.features : [];
    for (var i = 0; i < features.length; i++) {
      if (!walkGeometry(features[i] ? features[i].geometry : null, {
        onCoordinate: callback
      })) return;
    }
  }

  // Phase 2: reversible display simplification. Each requested vertex-reduction
  // percentage is converted to an upload-specific tolerance by a deterministic,
  // bounded log-space search. Tolerances use GeoJSON's expected WGS84 angular
  // units. Leaflet's bundled line simplifier is applied independently to each
  // line/ring; this does not preserve shared topology.
  var ADAPTIVE_SEARCH_CONFIG = {
    minToleranceDegrees: 1e-12,
    maxToleranceDegrees: 5,
    maxBinaryIterations: 26,
    toleranceRatioStop: 1.000001,
    minimumReduction: 0,
    maximumReduction: 98
  };

  function normalizeReductionTarget(value) {
    var numeric = Number(value);
    if (!isFinite(numeric)) numeric = 0;
    return Math.max(
      ADAPTIVE_SEARCH_CONFIG.minimumReduction,
      Math.min(ADAPTIVE_SEARCH_CONFIG.maximumReduction, Math.round(numeric))
    );
  }

  function geometryVertexCounts(geometry) {
    var counts = {vertexCount: 0, eligibleVertexCount: 0, pointVertexCount: 0};
    walkGeometry(geometry, {
      onCoordinate: function(_position, _member, type) {
        counts.vertexCount += 1;
        var baseType = baseGeometryType(type);
        if (baseType === 'Line' || baseType === 'Polygon') counts.eligibleVertexCount += 1;
        if (baseType === 'Point') counts.pointVertexCount += 1;
      }
    });
    return counts;
  }

  function actualReductionPercent(sourceVertexCount, displayedVertexCount, eligibleVertexCount) {
    var source = Number(sourceVertexCount || 0);
    var displayed = Number(displayedVertexCount || 0);
    if (Number(eligibleVertexCount || 0) <= 0 || source <= 0) return null;
    var reduction = Math.max(0, Math.min(100, (source - displayed) * 100 / source));
    return Math.round(reduction * 10) / 10;
  }

  function createAdaptiveToleranceSearch(sourceEligibleVertexCount, requestedReduction) {
    var sourceCount = Math.max(0, Math.floor(Number(sourceEligibleVertexCount || 0)));
    var target = normalizeReductionTarget(requestedReduction);
    var targetCount = Math.max(0, Math.round(sourceCount * (1 - target / 100)));
    return {
      sourceEligibleVertexCount: sourceCount,
      requestedReduction: target,
      targetEligibleVertexCount: targetCount,
      stage: target === 0 || sourceCount === 0 ? 'complete' : 'minimum',
      complete: target === 0 || sourceCount === 0,
      lowerTolerance: ADAPTIVE_SEARCH_CONFIG.minToleranceDegrees,
      upperTolerance: ADAPTIVE_SEARCH_CONFIG.maxToleranceDegrees,
      binaryIterations: 0,
      evaluationCount: 0,
      bestTolerance: 0,
      bestEligibleVertexCount: sourceCount,
      bestDifference: Math.abs(sourceCount - targetCount),
      termination: target === 0 ? 'original' : (sourceCount === 0 ? 'no-eligible-vertices' : '')
    };
  }

  function nextAdaptiveTolerance(search) {
    if (!search || search.complete) return null;
    if (search.stage === 'minimum') return ADAPTIVE_SEARCH_CONFIG.minToleranceDegrees;
    if (search.stage === 'maximum') return ADAPTIVE_SEARCH_CONFIG.maxToleranceDegrees;
    return Math.exp((Math.log(search.lowerTolerance) + Math.log(search.upperTolerance)) / 2);
  }

  function adaptiveCandidateIsBetter(search, tolerance, displayedEligibleVertexCount) {
    var count = Math.max(0, Math.floor(Number(displayedEligibleVertexCount || 0)));
    var difference = Math.abs(count - search.targetEligibleVertexCount);
    if (difference < search.bestDifference) return true;
    if (difference > search.bestDifference) return false;
    // Equal-distance results retain more source detail, then the smaller
    // tolerance. This deterministic tie-break is the safer visual choice.
    if (count > search.bestEligibleVertexCount) return true;
    if (count < search.bestEligibleVertexCount) return false;
    return tolerance < search.bestTolerance;
  }

  function recordAdaptiveToleranceEvaluation(search, tolerance, displayedEligibleVertexCount) {
    if (!search || search.complete) return false;
    var count = Math.max(0, Math.min(
      search.sourceEligibleVertexCount,
      Math.floor(Number(displayedEligibleVertexCount || 0))
    ));
    search.evaluationCount += 1;
    var becameBest = adaptiveCandidateIsBetter(search, tolerance, count);
    if (becameBest) {
      search.bestTolerance = tolerance;
      search.bestEligibleVertexCount = count;
      search.bestDifference = Math.abs(count - search.targetEligibleVertexCount);
    }

    if (search.stage === 'minimum') {
      if (count <= search.targetEligibleVertexCount) {
        search.complete = true;
        search.termination = count === search.targetEligibleVertexCount ? 'exact-target' : 'minimum-tolerance';
      } else {
        search.lowerTolerance = tolerance;
        search.stage = 'maximum';
      }
      return becameBest;
    }

    if (search.stage === 'maximum') {
      if (count > search.targetEligibleVertexCount) {
        search.complete = true;
        search.termination = 'maximum-tolerance';
      } else if (count === search.targetEligibleVertexCount) {
        search.complete = true;
        search.termination = 'exact-target';
      } else {
        search.upperTolerance = tolerance;
        search.stage = 'binary';
      }
      return becameBest;
    }

    search.binaryIterations += 1;
    if (count > search.targetEligibleVertexCount) {
      search.lowerTolerance = tolerance;
    } else {
      search.upperTolerance = tolerance;
    }

    if (count === search.targetEligibleVertexCount) {
      search.complete = true;
      search.termination = 'exact-target';
    } else if (search.binaryIterations >= ADAPTIVE_SEARCH_CONFIG.maxBinaryIterations) {
      search.complete = true;
      search.termination = 'iteration-limit';
    } else if (
      search.upperTolerance / search.lowerTolerance <= ADAPTIVE_SEARCH_CONFIG.toleranceRatioStop
    ) {
      search.complete = true;
      search.termination = 'tolerance-bracket';
    }
    return becameBest;
  }

  function copyCoordinate(coordinate) {
    return Array.isArray(coordinate) ? coordinate.slice() : coordinate;
  }

  function samePosition2d(a, b) {
    return isCoordinatePosition(a) && isCoordinatePosition(b) &&
      a[0] === b[0] && a[1] === b[1];
  }

  function sameCoordinate(a, b) {
    if (!Array.isArray(a) || !Array.isArray(b) || a.length !== b.length) return false;
    for (var i = 0; i < a.length; i++) {
      if (a[i] !== b[i]) return false;
    }
    return true;
  }

  function distinctPositionCount(coordinates) {
    var seen = Object.create(null);
    var count = 0;
    (Array.isArray(coordinates) ? coordinates : []).forEach(function(coordinate) {
      if (!isCoordinatePosition(coordinate)) return;
      var key = String(coordinate[0]) + '\u0000' + String(coordinate[1]);
      if (!seen[key]) {
        seen[key] = true;
        count += 1;
      }
    });
    return count;
  }

  function pointSegmentDistanceSquared(point, start, end) {
    var x = start[0];
    var y = start[1];
    var dx = end[0] - x;
    var dy = end[1] - y;

    if (dx !== 0 || dy !== 0) {
      var t = ((point[0] - x) * dx + (point[1] - y) * dy) / (dx * dx + dy * dy);
      if (t > 1) {
        x = end[0];
        y = end[1];
      } else if (t > 0) {
        x += dx * t;
        y += dy * t;
      }
    }

    dx = point[0] - x;
    dy = point[1] - y;
    return dx * dx + dy * dy;
  }

  function leafletLineSimplifier(coordinates, toleranceDegrees) {
    var leaflet = window.L;
    if (!leaflet || !leaflet.LineUtil || typeof leaflet.LineUtil.simplify !== 'function' ||
        typeof leaflet.point !== 'function') {
      throw new Error('Leaflet line simplification is unavailable.');
    }

    var points = coordinates.map(function(coordinate) {
      var point = leaflet.point(coordinate[0], coordinate[1]);
      point._ptSourceCoordinate = coordinate;
      return point;
    });
    var simplifiedPoints = leaflet.LineUtil.simplify(points, toleranceDegrees);
    return simplifiedPoints.map(function(point) {
      if (!point || !point._ptSourceCoordinate) {
        throw new Error('Leaflet simplification lost a source-coordinate association.');
      }
      return copyCoordinate(point._ptSourceCoordinate);
    });
  }

  function simplifyOpenCoordinates(coordinates, toleranceDegrees, lineSimplifier) {
    if (!Array.isArray(coordinates)) return {ok: false, coordinates: coordinates};
    if (coordinates.length < 2 || coordinates.some(function(coordinate) {
      return !isCoordinatePosition(coordinate);
    })) {
      return {ok: false, coordinates: coordinates};
    }
    if (distinctPositionCount(coordinates) < 2) {
      return {ok: true, coordinates: coordinates, unchanged: true};
    }

    var simplifyLine = lineSimplifier || leafletLineSimplifier;
    var simplified = simplifyLine(coordinates, toleranceDegrees);
    if (!Array.isArray(simplified) || simplified.length < 2 ||
        simplified.some(function(coordinate) { return !isCoordinatePosition(coordinate); })) {
      return {ok: false, coordinates: coordinates};
    }
    return {ok: true, coordinates: simplified};
  }

  function ensureMinimumRingPositions(sourceOpenRing, simplifiedOpenRing) {
    if (distinctPositionCount(simplifiedOpenRing) >= 3) return simplifiedOpenRing;

    var first = sourceOpenRing[0];
    var last = sourceOpenRing[sourceOpenRing.length - 1];
    var bestIndex = -1;
    var bestDistance = -1;

    for (var i = 1; i < sourceOpenRing.length - 1; i++) {
      var candidate = sourceOpenRing[i];
      if (samePosition2d(candidate, first) || samePosition2d(candidate, last)) continue;
      var distance = pointSegmentDistanceSquared(candidate, first, last);
      if (distance > bestDistance) {
        bestDistance = distance;
        bestIndex = i;
      }
    }

    if (bestIndex < 0) return null;
    return [copyCoordinate(first), copyCoordinate(sourceOpenRing[bestIndex]), copyCoordinate(last)];
  }

  function simplifyRing(coordinates, toleranceDegrees, lineSimplifier) {
    if (!Array.isArray(coordinates) || coordinates.length < 4 ||
        !sameCoordinate(coordinates[0], coordinates[coordinates.length - 1])) {
      return {ok: false, coordinates: coordinates};
    }

    var openRing = coordinates.slice(0, -1);
    if (distinctPositionCount(openRing) < 3) {
      return {ok: true, coordinates: coordinates, unchanged: true};
    }

    var simplifiedResult = simplifyOpenCoordinates(openRing, toleranceDegrees, lineSimplifier);
    if (!simplifiedResult.ok) return {ok: false, coordinates: coordinates};

    var safeOpenRing = ensureMinimumRingPositions(openRing, simplifiedResult.coordinates);
    if (!safeOpenRing || distinctPositionCount(safeOpenRing) < 3) {
      return {ok: false, coordinates: coordinates};
    }

    var closedRing = safeOpenRing.map(copyCoordinate);
    closedRing.push(copyCoordinate(closedRing[0]));
    return {ok: true, coordinates: closedRing};
  }

  function copyGeometryMember(geometry, key, value) {
    var copy = {};
    Object.keys(geometry || {}).forEach(function(member) {
      copy[member] = geometry[member];
    });
    copy[key] = value;
    return copy;
  }

  function simplifyGeometry(geometry, toleranceDegrees, lineSimplifier) {
    if (!geometry) return {ok: true, geometry: geometry};
    var type = String(geometry.type || '');
    var coordinates = geometry.coordinates;
    var result;
    var i;
    var j;
    var simplified;

    if (type === 'Point' || type === 'MultiPoint') {
      return {ok: true, geometry: geometry};
    }

    if (type === 'LineString') {
      result = simplifyOpenCoordinates(coordinates, toleranceDegrees, lineSimplifier);
      if (!result.ok) return {ok: false, geometry: geometry};
      return {ok: true, geometry: copyGeometryMember(geometry, 'coordinates', result.coordinates)};
    }

    if (type === 'MultiLineString') {
      if (!Array.isArray(coordinates)) return {ok: false, geometry: geometry};
      simplified = [];
      for (i = 0; i < coordinates.length; i++) {
        result = simplifyOpenCoordinates(coordinates[i], toleranceDegrees, lineSimplifier);
        if (!result.ok) return {ok: false, geometry: geometry};
        simplified.push(result.coordinates);
      }
      return {ok: true, geometry: copyGeometryMember(geometry, 'coordinates', simplified)};
    }

    if (type === 'Polygon') {
      if (!Array.isArray(coordinates)) return {ok: false, geometry: geometry};
      simplified = [];
      for (i = 0; i < coordinates.length; i++) {
        result = simplifyRing(coordinates[i], toleranceDegrees, lineSimplifier);
        if (!result.ok) return {ok: false, geometry: geometry};
        simplified.push(result.coordinates);
      }
      return {ok: true, geometry: copyGeometryMember(geometry, 'coordinates', simplified)};
    }

    if (type === 'MultiPolygon') {
      if (!Array.isArray(coordinates)) return {ok: false, geometry: geometry};
      simplified = [];
      for (i = 0; i < coordinates.length; i++) {
        if (!Array.isArray(coordinates[i])) return {ok: false, geometry: geometry};
        var polygon = [];
        for (j = 0; j < coordinates[i].length; j++) {
          result = simplifyRing(coordinates[i][j], toleranceDegrees, lineSimplifier);
          if (!result.ok) return {ok: false, geometry: geometry};
          polygon.push(result.coordinates);
        }
        simplified.push(polygon);
      }
      return {ok: true, geometry: copyGeometryMember(geometry, 'coordinates', simplified)};
    }

    if (type === 'GeometryCollection') {
      if (!Array.isArray(geometry.geometries)) return {ok: false, geometry: geometry};
      var geometries = [];
      for (i = 0; i < geometry.geometries.length; i++) {
        result = simplifyGeometry(geometry.geometries[i], toleranceDegrees, lineSimplifier);
        if (!result.ok) return {ok: false, geometry: geometry};
        geometries.push(result.geometry);
      }
      return {ok: true, geometry: copyGeometryMember(geometry, 'geometries', geometries)};
    }

    return {ok: true, geometry: geometry};
  }

  function copyFeatureWithGeometry(feature, geometry) {
    if (!feature || typeof feature !== 'object') return feature;
    var copy = {};
    Object.keys(feature).forEach(function(key) {
      copy[key] = feature[key];
    });
    copy.geometry = geometry;
    return copy;
  }

  function simplifyFeature(feature, toleranceDegrees, lineSimplifier) {
    toleranceDegrees = Number(toleranceDegrees || 0);
    if (!(toleranceDegrees > 0)) {
      return {feature: feature, fallback: false};
    }

    try {
      var sourceVertexCount = 0;
      walkGeometry(feature ? feature.geometry : null, {
        onCoordinate: function() { sourceVertexCount += 1; }
      });
      var result = simplifyGeometry(
        feature ? feature.geometry : null,
        toleranceDegrees,
        lineSimplifier
      );
      if (!result.ok) return {feature: feature, fallback: true};

      var displayedVertexCount = 0;
      walkGeometry(result.geometry, {
        onCoordinate: function() { displayedVertexCount += 1; }
      });
      if (displayedVertexCount > sourceVertexCount) {
        return {feature: feature, fallback: true};
      }

      return {
        feature: copyFeatureWithGeometry(feature, result.geometry),
        fallback: false
      };
    } catch (err) {
      return {feature: feature, fallback: true, error: err};
    }
  }

  function simplifyFeatureCollection(featureCollection, toleranceDegrees, lineSimplifier) {
    var features = featureCollection && Array.isArray(featureCollection.features) ?
      featureCollection.features : [];
    var outputFeatures = [];
    var fallbackCount = 0;

    features.forEach(function(feature) {
      var result = simplifyFeature(feature, toleranceDegrees, lineSimplifier);
      outputFeatures.push(result.feature);
      if (result.fallback) fallbackCount += 1;
    });

    var output = {};
    Object.keys(featureCollection || {}).forEach(function(key) {
      output[key] = featureCollection[key];
    });
    output.type = 'FeatureCollection';
    output.features = outputFeatures;
    return {
      featureCollection: output,
      fallbackCount: fallbackCount,
      metrics: featureCollectionMetrics(output)
    };
  }

  function simplifyFeatureCollectionToReduction(featureCollection, requestedReduction, lineSimplifier) {
    var sourceMetrics = featureCollectionMetrics(featureCollection);
    var target = normalizeReductionTarget(requestedReduction);
    if (target === 0 || sourceMetrics.eligibleVertexCount === 0) {
      return {
        featureCollection: featureCollection,
        fallbackCount: 0,
        metrics: sourceMetrics,
        requestedReduction: target,
        toleranceDegrees: 0,
        searchEvaluationCount: 0,
        searchIterations: 0,
        searchTermination: target === 0 ? 'original' : 'no-eligible-vertices'
      };
    }

    var search = createAdaptiveToleranceSearch(sourceMetrics.eligibleVertexCount, target);
    var best = {
      featureCollection: featureCollection,
      fallbackCount: 0,
      metrics: sourceMetrics,
      toleranceDegrees: 0
    };
    var maximumFallbackCount = 0;
    var tolerance;
    while ((tolerance = nextAdaptiveTolerance(search)) !== null) {
      var candidate = simplifyFeatureCollection(featureCollection, tolerance, lineSimplifier);
      maximumFallbackCount = Math.max(maximumFallbackCount, candidate.fallbackCount);
      if (recordAdaptiveToleranceEvaluation(
        search,
        tolerance,
        candidate.metrics.eligibleVertexCount
      )) {
        best = {
          featureCollection: candidate.featureCollection,
          fallbackCount: candidate.fallbackCount,
          metrics: candidate.metrics,
          toleranceDegrees: tolerance
        };
      }
    }
    best.fallbackCount = Math.max(best.fallbackCount, maximumFallbackCount);
    best.requestedReduction = target;
    best.searchEvaluationCount = search.evaluationCount;
    best.searchIterations = search.binaryIterations;
    best.searchTermination = search.termination;
    return best;
  }

  window.BRIM.localUploadGeometry = {
    vertexConvention: 'Each stored coordinate position counts once, including polygon ring-closing positions; coordinate dimensions and array containers do not count.',
    isCoordinatePosition: isCoordinatePosition,
    walkCoordinates: walkCoordinates,
    walkGeometry: walkGeometry,
    baseGeometryType: baseGeometryType,
    featureCollectionMetrics: featureCollectionMetrics,
    performanceThresholds: PERFORMANCE_THRESHOLDS,
    performanceWarning: performanceWarning,
    forEachCoordinate: forEachCoordinate,
    adaptiveSearchConfig: ADAPTIVE_SEARCH_CONFIG,
    normalizeReductionTarget: normalizeReductionTarget,
    geometryVertexCounts: geometryVertexCounts,
    actualReductionPercent: actualReductionPercent,
    createAdaptiveToleranceSearch: createAdaptiveToleranceSearch,
    nextAdaptiveTolerance: nextAdaptiveTolerance,
    recordAdaptiveToleranceEvaluation: recordAdaptiveToleranceEvaluation,
    simplifyFeature: simplifyFeature,
    simplifyFeatureCollection: simplifyFeatureCollection,
    simplifyFeatureCollectionToReduction: simplifyFeatureCollectionToReduction
  };
}
