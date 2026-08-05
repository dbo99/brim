'use strict';

const assert = require('assert');
const engineApi = require('../03_functions/js/brim_local_reference_filter_engine.js');

const fixture = {
  auto_supported: true,
  auto_default: true,
  auto_zoom_supported: true,
  auto_zoom_default: true,
  feature_selection_supported: true,
  feature_selection_mode: 'semantic_feature_multi',
  categories: [
    {category_key: 'suitable'},
    {category_key: 'non_suitable'},
    {category_key: 'unknown'}
  ],
  features: [
    {semantic_feature_key: 'f1', feature_key: 'f1', display_name: 'Alpha Ridge', category_keys: ['suitable'], search_text: 'alpha ridge nlcs-001 case-a', semantic_feature_bounds: [10, 10, 11, 11], geometry_component_count: 2},
    {semantic_feature_key: 'f2', feature_key: 'f2', display_name: 'Beta Canyon', category_keys: ['suitable'], search_text: 'beta canyon nlcs-002 case-b', semantic_feature_bounds: [20, 20, 20.02, 20.02], geometry_component_count: 1},
    {semantic_feature_key: 'f3', feature_key: 'f3', display_name: 'Gamma Mountain', category_keys: ['non_suitable'], search_text: 'gamma mountain nlcs-003 case-c', semantic_feature_bounds: [30, 30, 31, 31], geometry_component_count: 3},
    {semantic_feature_key: 'f4', feature_key: 'f4', display_name: 'Delta Subunit', category_keys: ['unknown'], search_text: 'delta subunit nlcs-004 case-d', semantic_feature_bounds: [40, 40, 42, 44], geometry_component_count: 3}
  ],
  records: [
    {geometry_key: 'g1', feature_key: 'f1', semantic_feature_key: 'f1', feature_display_name: 'Alpha Ridge', category_key: 'suitable', search_text: 'alpha ridge', semantic_feature_bounds: [10, 10, 11, 11], geometry_component_count: 2},
    {geometry_key: 'g2', feature_key: 'f2', semantic_feature_key: 'f2', feature_display_name: 'Beta Canyon', category_key: 'suitable', search_text: 'beta canyon', semantic_feature_bounds: [20, 20, 20.02, 20.02], geometry_component_count: 1},
    {geometry_key: 'g3', feature_key: 'f3', semantic_feature_key: 'f3', feature_display_name: 'Gamma Mountain', category_key: 'non_suitable', search_text: 'gamma mountain', semantic_feature_bounds: [30, 30, 31, 31], geometry_component_count: 3},
    {geometry_key: 'g4a', feature_key: 'f4', semantic_feature_key: 'f4', feature_display_name: 'Delta Subunit', category_key: 'unknown', search_text: 'delta subunit', semantic_feature_bounds: [40, 40, 42, 44], geometry_component_count: 1},
    {geometry_key: 'g4b', feature_key: 'f4', semantic_feature_key: 'f4', feature_display_name: 'Delta Subunit', category_key: 'unknown', search_text: 'delta subunit', semantic_feature_bounds: [40, 40, 42, 44], geometry_component_count: 2}
  ]
};

const engine = engineApi.create(fixture);
let state = engine.snapshot();
assert.strictEqual(state.auto, true);
assert.strictEqual(state.auto_zoom, true);
assert.deepStrictEqual(state.counts.total, {
  record_count: 5,
  semantic_feature_count: 4,
  geometry_component_count: 9
});
assert.deepStrictEqual(state.counts.currently_showing, state.counts.total);
assert.deepStrictEqual(state.visible_semantic_feature_bounds, [10, 10, 42, 44]);

// Typing is search-only: it never mutates the applied filter.
assert.deepStrictEqual(engine.featureSearch('nlcs-001').map(row => row.semantic_feature_key), ['f1']);
assert.deepStrictEqual(engine.featureSearch('canyon').map(row => row.semantic_feature_key), ['f2']);
assert.deepStrictEqual(engine.snapshot(), state);
assert.deepStrictEqual(engine.featureSearch('no match'), []);

// A selected semantic feature becomes the filter, auto-enables its category,
// and cannot be selected twice.
state = engine.setCategory('suitable', false);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 2);
state = engine.addFeature('f1');
assert.ok(state.draft_selected.includes('suitable'));
assert.ok(state.applied_selected.includes('suitable'));
assert.deepStrictEqual(state.draft_feature_keys, ['f1']);
assert.deepStrictEqual(state.applied_feature_keys, ['f1']);
assert.deepStrictEqual(state.visible_geometry_keys, ['g1']);
assert.deepStrictEqual(state.visible_semantic_feature_bounds, [10, 10, 11, 11]);
const duplicateState = engine.addFeature('f1');
assert.deepStrictEqual(duplicateState.draft_feature_keys, ['f1']);
assert.deepStrictEqual(engine.featureSearch('Alpha Ridge'), []);

state = engine.addFeature('f4');
assert.deepStrictEqual(state.applied_feature_keys, ['f1', 'f4']);
assert.deepStrictEqual(state.visible_geometry_keys, ['g1', 'g4a', 'g4b']);
assert.deepStrictEqual(state.counts.currently_showing, {
  record_count: 3,
  semantic_feature_count: 2,
  geometry_component_count: 5
});
assert.deepStrictEqual(state.visible_semantic_feature_bounds, [10, 10, 42, 44]);
state = engine.removeFeature('f1');
assert.deepStrictEqual(state.applied_feature_keys, ['f4']);
assert.deepStrictEqual(state.visible_semantic_feature_bounds, [40, 40, 42, 44]);
state = engine.removeFeature('f4');
assert.deepStrictEqual(state.applied_feature_keys, []);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 4);

// Auto off preserves separate staged and applied category/feature state.
state = engine.reset();
state = engine.setAuto(false);
state = engine.setCategory('suitable', false);
state = engine.addFeature('f1');
assert.deepStrictEqual(state.draft_feature_keys, ['f1']);
assert.deepStrictEqual(state.applied_feature_keys, []);
assert.ok(state.draft_selected.includes('suitable'), 'selection must auto-enable its category');
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 4);
assert.strictEqual(state.has_pending_changes, true);
state = engine.apply();
assert.deepStrictEqual(state.applied_feature_keys, ['f1']);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 1);
state = engine.addFeature('f4');
assert.deepStrictEqual(state.draft_feature_keys, ['f1', 'f4']);
assert.deepStrictEqual(state.applied_feature_keys, ['f1']);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 1);
state = engine.apply();
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 2);
state = engine.removeFeature('f1');
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 2);
state = engine.apply();
assert.deepStrictEqual(state.applied_feature_keys, ['f4']);
assert.deepStrictEqual(state.visible_semantic_feature_bounds, [40, 40, 42, 44]);

// Category controls stay authoritative even while a feature chip exists.
state = engine.none();
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 1);
state = engine.apply();
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 0);
assert.strictEqual(state.visible_semantic_feature_bounds, null);
state = engine.all();
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 0);
state = engine.apply();
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 1);

state = engine.setAutoZoom(false);
assert.strictEqual(state.auto_zoom, false);
state = engine.reset();
assert.strictEqual(state.auto, true);
assert.strictEqual(state.auto_zoom, true);
assert.deepStrictEqual(state.draft_feature_keys, []);
assert.deepStrictEqual(state.applied_feature_keys, []);
assert.deepStrictEqual(state.category_counts.unknown.total, {
  record_count: 2,
  semantic_feature_count: 1,
  geometry_component_count: 3
});

const noSelection = engineApi.create(Object.assign({}, fixture, {
  auto_supported: false,
  auto_default: true,
  auto_zoom_supported: false,
  auto_zoom_default: true,
  feature_selection_supported: false,
  feature_selection_mode: 'none'
}));
assert.strictEqual(noSelection.snapshot().auto, false);
assert.strictEqual(noSelection.snapshot().auto_zoom, false);
assert.deepStrictEqual(noSelection.featureSearch('alpha'), []);
assert.throws(() => noSelection.addFeature('f1'), /unsupported/);

console.log('Local Reference synthetic filter-engine selection/count tests passed.');
