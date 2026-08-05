'use strict';

const assert = require('assert');
const fs = require('fs');
const engineApi = require('../03_functions/js/brim_local_reference_filter_engine.js');

const identities = [
  ['nlcs000280', 'California National Historic Trail', 'california trail cali nlcs000280', [32, -124, 42, -114], 114],
  ['nlcs000281', 'Pony Express National Historic Trail', 'pony express trail poex nlcs000281', [38, -121, 42, -114], 6],
  ['nlcs000282', 'Old Spanish National Historic Trail', 'old spanish trail osnht olsp nlcs000282', [34, -120, 38, -114], 28],
  ['nlcs000283', 'Juan Bautista de Anza National Historic Trail', 'anza juan bautista de anza juba nlcs000283', [32, -118, 38, -114], 7],
  ['nlcs000284', 'Pacific Crest National Scenic Trail', 'pct pacific crest pcnst nlcs000284', [32, -122, 42, -116], 11],
  ['nlcs000285', 'Butterfield Overland National Historic Trail', 'butterfield butterfield trail buov nlcs000285', [32, -120, 35, -114], 11]
];

const fixture = {
  auto_supported: true,
  auto_default: true,
  auto_zoom_supported: true,
  auto_zoom_default: true,
  feature_selection_supported: true,
  feature_selection_mode: 'semantic_feature_multi',
  categories: identities.map(row => ({category_key: row[0]})),
  features: identities.map(row => ({
    semantic_feature_key: 'trail:nlcs_id:' + row[0],
    feature_key: 'trail:nlcs_id:' + row[0],
    display_name: row[1],
    category_keys: [row[0]],
    search_text: (row[1] + ' ' + row[2]).toLowerCase(),
    semantic_feature_bounds: row[3],
    geometry_component_count: row[4]
  })),
  records: identities.map((row, index) => ({
    geometry_key: 'trail:globalid:g' + index,
    feature_key: 'trail:nlcs_id:' + row[0],
    semantic_feature_key: 'trail:nlcs_id:' + row[0],
    category_key: row[0],
    geometry_component_count: row[4]
  }))
};

const engine = engineApi.create(fixture);
let state = engine.snapshot();
assert.deepStrictEqual(state.counts.total, {
  record_count: 6,
  semantic_feature_count: 6,
  geometry_component_count: 177
});
assert.deepStrictEqual(state.counts.currently_showing, state.counts.total);

const beforeTyping = engine.snapshot();
[
  ['pct', 'nlcs000284'],
  ['pacific crest', 'nlcs000284'],
  ['anza', 'nlcs000283'],
  ['juan bautista de anza', 'nlcs000283'],
  ['pony express', 'nlcs000281'],
  ['butterfield', 'nlcs000285'],
  ['old spanish', 'nlcs000282'],
  ['california trail', 'nlcs000280']
].forEach(([query, id]) => {
  const result = engine.featureSearch(query);
  assert.ok(result.some(feature => feature.semantic_feature_key.endsWith(id)), query);
});
assert.deepStrictEqual(engine.snapshot(), beforeTyping, 'typing/search must not alter visibility');
assert.deepStrictEqual(engine.featureSearch('no matching trail'), []);

state = engine.setCategory('nlcs000284', false);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 5);
state = engine.addFeature('trail:nlcs_id:nlcs000284');
assert.ok(state.applied_selected.includes('nlcs000284'));
assert.deepStrictEqual(state.applied_feature_keys, ['trail:nlcs_id:nlcs000284']);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 1);
assert.strictEqual(state.counts.currently_showing.geometry_component_count, 11);
assert.deepStrictEqual(state.visible_semantic_feature_bounds, [32, -122, 42, -116]);
assert.deepStrictEqual(
  engine.addFeature('trail:nlcs_id:nlcs000284').applied_feature_keys,
  ['trail:nlcs_id:nlcs000284'],
  'duplicate chips are prohibited'
);

state = engine.addFeature('trail:nlcs_id:nlcs000285');
assert.deepStrictEqual(state.applied_feature_keys, [
  'trail:nlcs_id:nlcs000284',
  'trail:nlcs_id:nlcs000285'
]);
assert.deepStrictEqual(state.visible_semantic_feature_bounds, [32, -122, 42, -114]);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 2);
assert.strictEqual(state.counts.currently_showing.geometry_component_count, 22);

state = engine.removeFeature('trail:nlcs_id:nlcs000284');
assert.deepStrictEqual(state.applied_feature_keys, ['trail:nlcs_id:nlcs000285']);
state = engine.removeFeature('trail:nlcs_id:nlcs000285');
assert.deepStrictEqual(state.applied_feature_keys, []);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 6);

state = engine.reset();
state = engine.setAuto(false);
state = engine.addFeature('trail:nlcs_id:nlcs000280');
assert.deepStrictEqual(state.draft_feature_keys, ['trail:nlcs_id:nlcs000280']);
assert.deepStrictEqual(state.applied_feature_keys, []);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 6);
assert.strictEqual(state.has_pending_changes, true);
state = engine.apply();
assert.deepStrictEqual(state.applied_feature_keys, ['trail:nlcs_id:nlcs000280']);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 1);
assert.strictEqual(state.counts.currently_showing.geometry_component_count, 114);

state = engine.setAutoZoom(false);
assert.strictEqual(state.auto_zoom, false);
state = engine.none();
state = engine.apply();
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 0);
assert.strictEqual(state.visible_semantic_feature_bounds, null);
state = engine.reset();
assert.strictEqual(state.auto, true);
assert.strictEqual(state.auto_zoom, true);
assert.deepStrictEqual(state.applied_feature_keys, []);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 6);

const controllerSource = fs.readFileSync(
  require.resolve('../03_functions/js/brim_local_reference_controller.js'),
  'utf8'
);
assert.ok(controllerSource.includes('layerData.show_category_count'));
assert.ok(controllerSource.includes('Zoom to results'));
assert.ok(controllerSource.includes('Auto-zoom'));
assert.ok(controllerSource.includes('pt-lr-chip-remove'));
assert.ok(controllerSource.includes('pt-trails-hover-tooltip'));
assert.ok(controllerSource.includes('@media (pointer:coarse)'));
assert.ok(controllerSource.includes('pt-local-reference-tabbed-popup'));
assert.ok(controllerSource.includes('function activatePopupTab'));
assert.ok(controllerSource.includes('function resetTabbedPopup'));
assert.ok(controllerSource.includes('function closeLayerPopup'));
assert.ok(controllerSource.includes("layerData.popup_layout || '') !== 'tabbed_card'"));
assert.ok(controllerSource.includes("event.key === 'ArrowRight'"));
assert.ok(controllerSource.includes("event.key === 'Home'"));
assert.ok(controllerSource.includes("event.key === 'Enter'"));
assert.ok(controllerSource.includes('preventScroll: true'));
assert.ok(controllerSource.includes('grid-template-columns:repeat(2,minmax(0,1fr))'));
assert.ok(controllerSource.includes('max-height:min(72vh,620px)'));
assert.ok(controllerSource.includes('height:min(54vh,450px)'));
assert.ok(controllerSource.includes('height:min(50vh,390px)'));
assert.ok(!controllerSource.includes("' rec · '"));
assert.ok(!controllerSource.includes("' sem · '"));
assert.ok(!controllerSource.includes("' geom'"));

console.log('Local Reference Phase 2 Trails controller/filter contract tests passed.');
