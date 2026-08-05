'use strict';

const assert = require('assert');
const fs = require('fs');
const engineApi = require('../03_functions/js/brim_local_reference_filter_engine.js');

const payloadPath = process.argv[2];
if (!payloadPath) throw new Error('Expected a generated Local Reference payload JSON path.');
const payload = JSON.parse(fs.readFileSync(payloadPath, 'utf8'));

const expectedGeometryKeys = [
  'wsa:globalid:eab306b9-62de-406c-9e79-fa7692fd729d:geometry:1',
  'wsa:globalid:f713da90-b15b-4cdb-9a29-cb1c158626d7:geometry:1'
];
const expectedFeatureKeys = expectedGeometryKeys.map(key => key.replace(/:geometry:1$/, ''));

const notStatedRecords = payload.records.filter(record => record.category_key === 'unknown');
const notStatedFeatures = payload.features.filter(feature => feature.category_keys.includes('unknown'));
assert.strictEqual(notStatedRecords.length, 2);
assert.strictEqual(notStatedFeatures.length, 2);
assert.deepStrictEqual(notStatedRecords.map(record => record.geometry_key), expectedGeometryKeys);
assert.deepStrictEqual(notStatedRecords.map(record => record.semantic_feature_key), expectedFeatureKeys);
assert.deepStrictEqual(notStatedRecords.map(record => record.geometry_component_count), [1, 1]);
assert.deepStrictEqual(notStatedFeatures.map(feature => feature.semantic_feature_key), expectedFeatureKeys);
assert.deepStrictEqual(
  notStatedFeatures.map(feature => feature.display_name),
  ['Red Mountain', 'Trinity Alps (Subunit 4)']
);
notStatedFeatures.forEach(feature => {
  assert.strictEqual(feature.semantic_feature_bounds.length, 4);
  assert.ok(feature.semantic_feature_bounds.every(Number.isFinite));
});

// Autocomplete searches names and approved source identifiers without changing
// the applied feature set or map-filter counts merely from typing.
const searchEngine = engineApi.create(payload);
const initialSearchState = searchEngine.snapshot();
assert.deepStrictEqual(
  searchEngine.featureSearch('Red Mountain').map(feature => feature.semantic_feature_key),
  [expectedFeatureKeys[0]]
);
assert.deepStrictEqual(
  searchEngine.featureSearch('F713DA90-B15B-4CDB-9A29-CB1C158626D7')
    .map(feature => feature.semantic_feature_key),
  [expectedFeatureKeys[1]]
);
assert.deepStrictEqual(searchEngine.snapshot(), initialSearchState);

let selected = searchEngine.addFeature(expectedFeatureKeys[0]);
assert.deepStrictEqual(selected.applied_feature_keys, [expectedFeatureKeys[0]]);
assert.deepStrictEqual(selected.applied_selected.includes('unknown'), true);
assert.strictEqual(selected.counts.currently_showing.semantic_feature_count, 1);
selected = searchEngine.addFeature(expectedFeatureKeys[1]);
assert.deepStrictEqual(selected.applied_feature_keys, expectedFeatureKeys);
assert.strictEqual(selected.counts.currently_showing.semantic_feature_count, 2);
assert.deepStrictEqual(
  selected.visible_semantic_feature_bounds,
  [
    Math.min(...notStatedFeatures.map(feature => feature.semantic_feature_bounds[0])),
    Math.min(...notStatedFeatures.map(feature => feature.semantic_feature_bounds[1])),
    Math.max(...notStatedFeatures.map(feature => feature.semantic_feature_bounds[2])),
    Math.max(...notStatedFeatures.map(feature => feature.semantic_feature_bounds[3]))
  ]
);
selected = searchEngine.addFeature(expectedFeatureKeys[1]);
assert.deepStrictEqual(selected.applied_feature_keys, expectedFeatureKeys, 'duplicate selection must be ignored');
selected = searchEngine.removeFeature(expectedFeatureKeys[0]);
assert.deepStrictEqual(selected.applied_feature_keys, [expectedFeatureKeys[1]]);
selected = searchEngine.removeFeature(expectedFeatureKeys[1]);
assert.deepStrictEqual(selected.applied_feature_keys, []);
assert.strictEqual(selected.counts.currently_showing.semantic_feature_count, 63);

const engine = engineApi.create(payload);
engine.none();
const notStatedOnly = engine.setCategory('unknown', true);

assert.deepStrictEqual(notStatedOnly.applied_selected, ['unknown']);
assert.deepStrictEqual(notStatedOnly.visible_geometry_keys, expectedGeometryKeys);
assert.deepStrictEqual(notStatedOnly.counts.currently_showing, {
  record_count: 2,
  semantic_feature_count: 2,
  geometry_component_count: 2
});
assert.deepStrictEqual(notStatedOnly.category_counts.unknown.total, {
  record_count: 2,
  semantic_feature_count: 2,
  geometry_component_count: 2
});
assert.deepStrictEqual(
  notStatedOnly.category_counts.unknown.currently_showing,
  notStatedOnly.category_counts.unknown.total
);
Object.keys(notStatedOnly.category_counts)
  .filter(key => key !== 'unknown')
  .forEach(key => {
    assert.strictEqual(
      notStatedOnly.category_counts[key].currently_showing.semantic_feature_count,
      0,
      'No non-Unknown category may enter the Not-stated filter result.'
    );
  });

const reset = engine.reset();
assert.strictEqual(reset.counts.currently_showing.record_count, 63);
assert.strictEqual(reset.counts.currently_showing.semantic_feature_count, 63);
assert.strictEqual(reset.counts.currently_showing.geometry_component_count, 104);

console.log('Actual WSA Not-stated filter-engine assertions passed.');
