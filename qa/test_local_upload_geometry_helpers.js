'use strict';

const assert = require('assert');
const fs = require('fs');
const vm = require('vm');

const helperPath = '03_functions/js/brim_local_upload_geometry_helpers.js';
const context = {
  window: {},
  console: console
};
vm.createContext(context);
vm.runInContext(fs.readFileSync(helperPath, 'utf8'), context, {filename: helperPath});
context.ptInitBrimLocalUploadGeometryHelpers(null, null);

const geometry = context.window.BRIM.localUploadGeometry;

function feature(type, coordinates) {
  return {
    type: 'Feature',
    properties: {fixture: type},
    geometry: type === null ? null : {type: type, coordinates: coordinates}
  };
}

const fixtures = [
  feature('Point', [1, 2, 30]),
  feature('MultiPoint', [[1, 2], [3, 4], [5, 6]]),
  feature('LineString', [[0, 0], [1, 1], [1, 1], [2, 2]]),
  feature('MultiLineString', [
    [[0, 0], [1, 1]],
    [[2, 2], [3, 3], [4, 4]]
  ]),
  feature('Polygon', [
    [[0, 0], [5, 0], [5, 5], [0, 5], [0, 0]],
    [[1, 1], [2, 1], [2, 2], [1, 2], [1, 1]]
  ]),
  feature('MultiPolygon', [
    [[[10, 10], [12, 10], [12, 12], [10, 12], [10, 10]]],
    [[[20, 20], [22, 20], [22, 22], [20, 22], [20, 20]]]
  ]),
  {
    type: 'Feature',
    properties: {fixture: 'GeometryCollection'},
    geometry: {
      type: 'GeometryCollection',
      geometries: [
        {type: 'Point', coordinates: [30, 30]},
        {type: 'LineString', coordinates: [[30, 30], [31, 31], [32, 32]]},
        {type: 'Polygon', coordinates: [[[40, 40], [42, 40], [42, 42], [40, 42], [40, 40]]]},
        null
      ]
    }
  },
  feature(null, null),
  feature('LineString', []),
  feature('LineString', [[50, 50], [50, 50], [51, 51], [51, 51]])
];

const metrics = geometry.featureCollectionMetrics({
  type: 'FeatureCollection',
  features: fixtures
});

assert.strictEqual(metrics.featureCount, 10, 'counts source features, not attribute rows');
assert.strictEqual(metrics.vertexCount, 46, 'counts stored coordinate positions once');
assert.strictEqual(metrics.eligibleVertexCount, 41, 'counts line and polygon positions as simplification-eligible');
assert.strictEqual(metrics.pointVertexCount, 5, 'counts point positions separately');
assert.deepStrictEqual(Array.from(metrics.baseTypes), ['Point', 'Line', 'Polygon']);
assert.strictEqual(metrics.geometryLabel, 'Mixed (Point + Line + Polygon)');
assert.strictEqual(metrics.pointOnly, false);
assert.strictEqual(metrics.hasSimplifiableGeometry, true);
assert.strictEqual(metrics.nullGeometryCount, 1);
assert.strictEqual(metrics.emptyGeometryCount, 1);
assert.strictEqual(metrics.geometryCollectionCount, 1);

const pointMetrics = geometry.featureCollectionMetrics({
  type: 'FeatureCollection',
  features: [feature('Point', [0, 0]), feature('MultiPoint', [[1, 1], [2, 2]])]
});
assert.strictEqual(pointMetrics.vertexCount, 3);
assert.strictEqual(pointMetrics.eligibleVertexCount, 0);
assert.strictEqual(pointMetrics.pointVertexCount, 3);
assert.strictEqual(pointMetrics.geometryLabel, 'Point');
assert.strictEqual(pointMetrics.pointOnly, true);
assert.strictEqual(pointMetrics.hasSimplifiableGeometry, false);

assert.deepStrictEqual(JSON.parse(JSON.stringify(geometry.performanceThresholds)), {
  detailedFeatureCount: 5000,
  detailedVertexCount: 100000,
  pointFeatureCount: 10000
});
assert.strictEqual(geometry.performanceWarning({
  featureCount: 4999,
  vertexCount: 99999,
  pointOnly: false,
  hasSimplifiableGeometry: true
}), '');
assert.strictEqual(geometry.performanceWarning({
  featureCount: 5000,
  vertexCount: 10000,
  pointOnly: false,
  hasSimplifiableGeometry: true
}), 'Highly detailed geometry may slow map interaction. Try a higher vertex reduction setting.');
assert.strictEqual(geometry.performanceWarning({
  featureCount: 1,
  vertexCount: 100000,
  pointOnly: false,
  hasSimplifiableGeometry: true
}), 'Highly detailed geometry may slow map interaction. Try a higher vertex reduction setting.');
assert.strictEqual(geometry.performanceWarning({
  featureCount: 10000,
  vertexCount: 10000,
  pointOnly: true,
  hasSimplifiableGeometry: false
}), 'Large point layers may slow map interaction. Vertex reduction does not reduce point count.');
assert.strictEqual(geometry.performanceWarning({
  featureCount: 9999,
  vertexCount: 9999,
  pointOnly: true,
  hasSimplifiableGeometry: false
}), '');

let visited = 0;
geometry.forEachCoordinate({
  type: 'FeatureCollection',
  features: fixtures
}, function() {
  visited += 1;
  return visited < 7;
});
assert.strictEqual(visited, 7, 'coordinate walking can stop early for bounded diagnostics');

assert.strictEqual(
  geometry.vertexConvention,
  'Each stored coordinate position counts once, including polygon ring-closing positions; coordinate dimensions and array containers do not count.'
);

console.log('Local-upload Phase 1 geometry diagnostics: all fixture assertions passed.');

function squaredDistance(a, b) {
  const dx = a[0] - b[0];
  const dy = a[1] - b[1];
  return dx * dx + dy * dy;
}

function squaredSegmentDistance(point, start, end) {
  let x = start[0];
  let y = start[1];
  let dx = end[0] - x;
  let dy = end[1] - y;
  if (dx !== 0 || dy !== 0) {
    const ratio = ((point[0] - x) * dx + (point[1] - y) * dy) / (dx * dx + dy * dy);
    if (ratio > 1) {
      x = end[0];
      y = end[1];
    } else if (ratio > 0) {
      x += dx * ratio;
      y += dy * ratio;
    }
  }
  dx = point[0] - x;
  dy = point[1] - y;
  return dx * dx + dy * dy;
}

// Faithful coordinate-array equivalent of Leaflet LineUtil.simplify's radial
// distance pre-pass followed by Douglas-Peucker simplification.
function leafletEquivalentSimplifier(points, tolerance) {
  if (!tolerance || !points.length) return points.slice();
  const squaredTolerance = tolerance * tolerance;
  const reduced = [points[0]];
  let previousIndex = 0;
  for (let index = 1; index < points.length; index += 1) {
    if (squaredDistance(points[index], points[previousIndex]) > squaredTolerance) {
      reduced.push(points[index]);
      previousIndex = index;
    }
  }
  if (previousIndex < points.length - 1) reduced.push(points[points.length - 1]);

  const markers = new Uint8Array(reduced.length);
  markers[0] = 1;
  markers[reduced.length - 1] = 1;
  function markDouglasPeucker(first, last) {
    let bestIndex = -1;
    let bestDistance = 0;
    for (let index = first + 1; index < last; index += 1) {
      const distance = squaredSegmentDistance(reduced[index], reduced[first], reduced[last]);
      if (distance > bestDistance) {
        bestDistance = distance;
        bestIndex = index;
      }
    }
    if (bestDistance > squaredTolerance) {
      markers[bestIndex] = 1;
      markDouglasPeucker(first, bestIndex);
      markDouglasPeucker(bestIndex, last);
    }
  }
  markDouglasPeucker(0, reduced.length - 1);
  return reduced.filter(function(_point, index) { return markers[index]; });
}

function denseSquare(x, y, size, steps) {
  const ring = [];
  for (let index = 0; index < steps; index += 1) ring.push([x + size * index / steps, y]);
  for (let index = 0; index < steps; index += 1) ring.push([x + size, y + size * index / steps]);
  for (let index = 0; index < steps; index += 1) ring.push([x + size - size * index / steps, y + size]);
  for (let index = 0; index < steps; index += 1) ring.push([x, y + size - size * index / steps]);
  ring.push(ring[0].slice());
  return ring;
}

const wigglyLine = [];
for (let index = 0; index <= 120; index += 1) {
  wigglyLine.push([
    -122 + index * 0.00001,
    38 + Math.sin(index / 3) * 0.000035
  ]);
}

const simplificationSource = {
  type: 'FeatureCollection',
  features: [
    {
      type: 'Feature',
      id: 'line',
      properties: {name: 'detailed line', value: 10},
      geometry: {type: 'LineString', coordinates: wigglyLine}
    },
    {
      type: 'Feature',
      id: 'polygon-hole',
      properties: {name: 'polygon with hole'},
      geometry: {
        type: 'Polygon',
        coordinates: [denseSquare(-121, 37, 0.004, 20), denseSquare(-120.999, 37.001, 0.001, 8)]
      }
    },
    {
      type: 'Feature',
      id: 'multipolygon',
      properties: {name: 'multipart'},
      geometry: {
        type: 'MultiPolygon',
        coordinates: [
          [denseSquare(-120, 36, 0.002, 12)],
          [denseSquare(-119.99, 36.01, 0.002, 12)]
        ]
      }
    },
    {
      type: 'Feature',
      id: 'collection',
      properties: {name: 'mixed collection'},
      geometry: {
        type: 'GeometryCollection',
        geometries: [
          {type: 'Point', coordinates: [-118, 35, 900]},
          {type: 'LineString', coordinates: wigglyLine.slice(0, 30)}
        ]
      }
    },
    {
      type: 'Feature',
      id: 'constant',
      properties: {name: 'constant line'},
      geometry: {
        type: 'LineString',
        coordinates: [[-117, 34], [-117, 34], [-117, 34], [-117, 34]]
      }
    },
    {
      type: 'Feature',
      id: 'unsafe',
      properties: {name: 'unclosed source ring'},
      geometry: {
        type: 'Polygon',
        coordinates: [[[-116, 33], [-115.9, 33], [-115.9, 33.1], [-116, 33.1]]]
      }
    }
  ]
};

const sourceSnapshot = JSON.stringify(simplificationSource);
const originalResult = geometry.simplifyFeatureCollectionToReduction(
  simplificationSource,
  0,
  leafletEquivalentSimplifier
);
assert.strictEqual(JSON.stringify(originalResult.featureCollection), sourceSnapshot);
assert.strictEqual(originalResult.fallbackCount, 0);
assert.strictEqual(originalResult.toleranceDegrees, 0);
assert.strictEqual(originalResult.searchTermination, 'original');

for (let requested = 1; requested <= 98; requested += 1) {
  assert.strictEqual(geometry.normalizeReductionTarget(requested), requested, 'accepts every integer target from 1 through 98');
}
assert.strictEqual(geometry.normalizeReductionTarget(-10), 0);
assert.strictEqual(geometry.normalizeReductionTarget(120), 98);

const representativeTargets = [0, 25, 50, 75, 90, 95, 98];
const representativeResults = representativeTargets.map(function(target) {
  return geometry.simplifyFeatureCollectionToReduction(
    simplificationSource,
    target,
    leafletEquivalentSimplifier
  );
});

assert.strictEqual(JSON.stringify(simplificationSource), sourceSnapshot, 'source object is never mutated');
representativeResults.forEach(function(result, resultIndex) {
  const target = representativeTargets[resultIndex];
  assert.strictEqual(result.requestedReduction, target);
  assert.ok(result.metrics.vertexCount <= geometry.featureCollectionMetrics(simplificationSource).vertexCount);
  assert.ok(result.searchEvaluationCount <= 28, 'bounded search evaluates at most min, max, and 26 binary candidates');
  assert.ok([
    'original', 'exact-target', 'minimum-tolerance', 'maximum-tolerance',
    'iteration-limit', 'tolerance-bracket'
  ].indexOf(result.searchTermination) >= 0, 'search reports a bounded stopping reason');
  if (target > 0) assert.strictEqual(result.fallbackCount, 1, 'unsafe feature fallback remains reported');
});

for (let index = 1; index < representativeResults.length; index += 1) {
  assert.ok(
    representativeResults[index].metrics.vertexCount <= representativeResults[index - 1].metrics.vertexCount,
    'representative target counts are monotonically non-increasing'
  );
}

let previousDisplayedCount = Infinity;
for (let requested = 1; requested <= 98; requested += 1) {
  const result = geometry.simplifyFeatureCollectionToReduction(
    simplificationSource,
    requested,
    leafletEquivalentSimplifier
  );
  assert.ok(result.metrics.vertexCount <= previousDisplayedCount, 'all slider targets are monotonic at ' + requested + '%');
  previousDisplayedCount = result.metrics.vertexCount;
}

const repeated95 = geometry.simplifyFeatureCollectionToReduction(
  simplificationSource,
  95,
  leafletEquivalentSimplifier
);
assert.strictEqual(
  JSON.stringify(repeated95.featureCollection),
  JSON.stringify(representativeResults[5].featureCollection),
  'repeated identical requests produce deterministic coordinates'
);
assert.strictEqual(repeated95.toleranceDegrees, representativeResults[5].toleranceDegrees);

representativeResults.slice(1).forEach(function(result) {
  result.featureCollection.features.forEach(function(outputFeature, index) {
    assert.strictEqual(outputFeature.properties, simplificationSource.features[index].properties, 'properties remain linked and unchanged');
    assert.strictEqual(outputFeature.geometry.type, simplificationSource.features[index].geometry.type, 'geometry type is preserved');
  });

  const polygon = result.featureCollection.features[1].geometry.coordinates;
  assert.strictEqual(polygon.length, 2, 'polygon hole remains represented');
  polygon.forEach(function(ring) {
    assert.ok(ring.length >= 4, 'polygon rings retain minimum coordinate count');
    assert.strictEqual(JSON.stringify(ring[0]), JSON.stringify(ring[ring.length - 1]), 'polygon rings remain closed');
  });

  const multipolygon = result.featureCollection.features[2].geometry.coordinates;
  assert.strictEqual(multipolygon.length, 2, 'multipart polygon structure remains represented');
  multipolygon.forEach(function(part) {
    assert.ok(part.length >= 1);
    part.forEach(function(ring) {
      assert.ok(ring.length >= 4);
      assert.strictEqual(JSON.stringify(ring[0]), JSON.stringify(ring[ring.length - 1]));
    });
  });

  const collection = result.featureCollection.features[3].geometry;
  assert.strictEqual(collection.geometries.length, 2);
  assert.strictEqual(JSON.stringify(collection.geometries[0]), JSON.stringify(simplificationSource.features[3].geometry.geometries[0]), 'points remain unchanged');
  assert.strictEqual(
    JSON.stringify(result.featureCollection.features[0].geometry.coordinates[0]),
    JSON.stringify(simplificationSource.features[0].geometry.coordinates[0]),
    'line start endpoint remains exact'
  );
  assert.strictEqual(
    JSON.stringify(result.featureCollection.features[0].geometry.coordinates.slice(-1)[0]),
    JSON.stringify(simplificationSource.features[0].geometry.coordinates.slice(-1)[0]),
    'line end endpoint remains exact'
  );
  assert.strictEqual(JSON.stringify(result.featureCollection.features[4].geometry), JSON.stringify(simplificationSource.features[4].geometry), 'constant geometry safely remains original');
  assert.strictEqual(result.featureCollection.features[5], simplificationSource.features[5], 'unsafe feature falls back to exact source feature');
});

assert.deepStrictEqual(
  JSON.parse(JSON.stringify(geometry.adaptiveSearchConfig)),
  {
    minToleranceDegrees: 1e-12,
    maxToleranceDegrees: 5,
    maxBinaryIterations: 26,
    toleranceRatioStop: 1.000001,
    minimumReduction: 0,
    maximumReduction: 98
  }
);

assert.strictEqual(geometry.actualReductionPercent(286430, 24180, 286430), 91.6);
assert.strictEqual(geometry.actualReductionPercent(10, 7, 0), null, 'no percentage for point-only geometry');

const pointOnlySource = {
  type: 'FeatureCollection',
  features: [feature('Point', [1, 2, 3]), feature('MultiPoint', [[4, 5], [6, 7]])]
};
const pointOnlyResult = geometry.simplifyFeatureCollectionToReduction(
  pointOnlySource,
  98,
  leafletEquivalentSimplifier
);
assert.strictEqual(JSON.stringify(pointOnlyResult.featureCollection), JSON.stringify(pointOnlySource));
assert.strictEqual(pointOnlyResult.searchTermination, 'no-eligible-vertices');

const tinyUnsafeSource = {
  type: 'FeatureCollection',
  features: [
    feature('LineString', [[0, 0], [1, 1]]),
    feature('Polygon', [[[0, 0], [1, 0], [1, 1], [0, 1]]])
  ]
};
const tinyUnsafeResult = geometry.simplifyFeatureCollectionToReduction(
  tinyUnsafeSource,
  98,
  leafletEquivalentSimplifier
);
assert.strictEqual(JSON.stringify(tinyUnsafeResult.featureCollection.features[0].geometry), JSON.stringify(tinyUnsafeSource.features[0].geometry));
assert.strictEqual(tinyUnsafeResult.featureCollection.features[1], tinyUnsafeSource.features[1]);
assert.ok(tinyUnsafeResult.metrics.vertexCount <= geometry.featureCollectionMetrics(tinyUnsafeSource).vertexCount);

console.log('Local-upload Phase 2 adaptive simplification: all deterministic safety assertions passed.');
