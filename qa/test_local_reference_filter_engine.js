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

const facetFixture = Object.assign({}, fixture, {
  facets: [
    {
      facet_key: 'management_pattern',
      values: [
        {value_key: 'single_agency'},
        {value_key: 'shared_multi_agency'}
      ]
    },
    {
      facet_key: 'geographic_context',
      values: [
        {value_key: 'california'},
        {value_key: 'western_nevada_context'}
      ]
    }
  ],
  features: fixture.features.map(feature => Object.assign({}, feature, {
    display_name: feature.semantic_feature_key === 'f1' ?
      'Alpha/Ridge–Wilderness' : feature.display_name
  })),
  records: fixture.records.map(record => Object.assign({}, record, {
    facet_values: {
      management_pattern: record.semantic_feature_key === 'f4' ?
        'shared_multi_agency' : 'single_agency',
      geographic_context: record.semantic_feature_key === 'f4' ?
        'western_nevada_context' : 'california'
    }
  }))
});
const facetEngine = engineApi.create(facetFixture);
state = facetEngine.snapshot();
assert.deepStrictEqual(state.applied_facets.management_pattern, [
  'single_agency', 'shared_multi_agency'
]);
assert.strictEqual(
  state.facet_counts.geographic_context.western_nevada_context.total.record_count,
  2
);
assert.deepStrictEqual(
  facetEngine.featureSearch('alpha ridge wilderness').map(row => row.semantic_feature_key),
  ['f1'],
  'slash, dash, and punctuation must normalize for named-feature search'
);
state = facetEngine.setFacetValue('management_pattern', 'shared_multi_agency', false);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 3);
assert.strictEqual(state.counts.currently_showing.geometry_component_count, 6);
state = facetEngine.facetNone('geographic_context');
assert.strictEqual(state.counts.currently_showing.record_count, 0);
state = facetEngine.facetAll('geographic_context');
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 3);
state = facetEngine.setAuto(false);
state = facetEngine.setFacetValue('management_pattern', 'shared_multi_agency', true);
assert.strictEqual(state.has_pending_changes, true);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 3);
state = facetEngine.apply();
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 4);
state = facetEngine.reset();
assert.deepStrictEqual(state.draft_facets.geographic_context, [
  'california', 'western_nevada_context'
]);

const multivalueFixture = {
  auto_supported: true,
  auto_default: true,
  feature_selection_supported: false,
  feature_selection_mode: 'none',
  categories: [{category_key: 'acec'}],
  facets: [{
    facet_key: 'relevant_value_family',
    values: [
      {value_key: 'water_aquatic'},
      {value_key: 'wildlife_and_habitat'},
      {value_key: 'scenic'}
    ]
  }],
  records: [
    {geometry_key: 'a', semantic_feature_key: 'a', category_key: 'acec', geometry_component_count: 2,
      facet_values: {relevant_value_family: ['water_aquatic', 'wildlife_and_habitat']}},
    {geometry_key: 'b', semantic_feature_key: 'b', category_key: 'acec', geometry_component_count: 1,
      facet_values: {relevant_value_family: 'scenic'}}
  ]
};
const multivalueEngine = engineApi.create(multivalueFixture);
state = multivalueEngine.snapshot();
assert.strictEqual(
  state.facet_counts.relevant_value_family.water_aquatic.total.semantic_feature_count,
  1
);
state = multivalueEngine.setFacetValue('relevant_value_family', 'water_aquatic', false);
assert.strictEqual(
  state.counts.currently_showing.semantic_feature_count,
  2,
  'a multi-valued ACEC remains visible while any selected value matches'
);
state = multivalueEngine.setFacetValue('relevant_value_family', 'wildlife_and_habitat', false);
assert.deepStrictEqual(state.visible_geometry_keys, ['b']);
state = multivalueEngine.facetNone('relevant_value_family');
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 0);

const fieldOfficeContextFixture = {
  auto_supported: true,
  auto_default: true,
  feature_selection_supported: false,
  feature_selection_mode: 'none',
  categories: [{category_key: 'acec'}],
  facets: [
    {
      facet_key: 'planning_framework',
      values: [
        {value_key: 'drecp'},
        {value_key: 'other'}
      ]
    },
    {
      facet_key: 'field_office_context',
      values: [
        {value_key: 'barstow_field_office'},
        {value_key: 'needles_field_office'},
        {value_key: 'ridgecrest_field_office'},
        {value_key: 'central_coast_field_office'}
      ]
    }
  ],
  records: [
    {geometry_key: 'cross-office', semantic_feature_key: 'cross-office', category_key: 'acec', geometry_component_count: 3,
      facet_values: {planning_framework: 'drecp', field_office_context: ['barstow_field_office', 'needles_field_office']}},
    {geometry_key: 'ridgecrest', semantic_feature_key: 'ridgecrest', category_key: 'acec', geometry_component_count: 1,
      facet_values: {planning_framework: 'drecp', field_office_context: 'ridgecrest_field_office'}},
    {geometry_key: 'needles', semantic_feature_key: 'needles', category_key: 'acec', geometry_component_count: 2,
      facet_values: {planning_framework: 'other', field_office_context: 'needles_field_office'}},
    {geometry_key: 'central-coast', semantic_feature_key: 'central-coast', category_key: 'acec', geometry_component_count: 1,
      facet_values: {planning_framework: 'other', field_office_context: 'central_coast_field_office'}}
  ]
};
const fieldOfficeContextEngine = engineApi.create(fieldOfficeContextFixture);
state = fieldOfficeContextEngine.snapshot();
assert.strictEqual(
  state.facet_counts.field_office_context.needles_field_office.total.semantic_feature_count,
  2,
  'a cross-office ACEC contributes once to each matching office count'
);
state = fieldOfficeContextEngine.facetNone('field_office_context');
state = fieldOfficeContextEngine.setFacetValue('field_office_context', 'barstow_field_office', true);
assert.deepStrictEqual(state.visible_geometry_keys, ['cross-office']);
state = fieldOfficeContextEngine.setFacetValue('field_office_context', 'needles_field_office', true);
assert.strictEqual(
  state.counts.currently_showing.semantic_feature_count,
  2,
  'field-office values use OR semantics without duplicating a cross-office ACEC'
);
assert.deepStrictEqual(state.visible_geometry_keys, ['cross-office', 'needles']);
state = fieldOfficeContextEngine.facetNone('planning_framework');
state = fieldOfficeContextEngine.setFacetValue('planning_framework', 'drecp', true);
assert.deepStrictEqual(
  state.visible_geometry_keys,
  ['cross-office'],
  'field-office context combines with other facets using AND semantics'
);
assert.deepStrictEqual(state.counts.currently_showing, {
  record_count: 1,
  semantic_feature_count: 1,
  geometry_component_count: 3
});

const numericEngine = engineApi.create({
  auto_supported: true,
  auto_default: true,
  feature_selection_supported: false,
  feature_selection_mode: 'none',
  categories: [{category_key: 'context'}],
  numeric_filter: {min: 0, max: 100, step: 1, default: 0},
  records: [
    {geometry_key: 'county-a', semantic_feature_key: 'county-a',
      category_key: 'context', numeric_value: 12.4},
    {geometry_key: 'county-b', semantic_feature_key: 'county-b',
      category_key: 'context', numeric_value: 64.8},
    {geometry_key: 'county-missing', semantic_feature_key: 'county-missing',
      category_key: 'context', numeric_value: null}
  ]
});
state = numericEngine.snapshot();
assert.strictEqual(state.numeric_filter_supported, true);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 3);
state = numericEngine.setNumericMinimum(40);
assert.strictEqual(state.applied_numeric_minimum, 40);
assert.deepStrictEqual(state.visible_geometry_keys, ['county-b']);
state = numericEngine.setAuto(false);
state = numericEngine.setNumericMinimum(70);
assert.strictEqual(state.draft_numeric_minimum, 70);
assert.strictEqual(state.applied_numeric_minimum, 40);
assert.strictEqual(state.has_pending_changes, true);
state = numericEngine.apply();
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 0);
state = numericEngine.reset();
assert.strictEqual(state.draft_numeric_minimum, 0);
assert.strictEqual(state.counts.currently_showing.semantic_feature_count, 3);
assert.throws(() => noSelection.setNumericMinimum(10), /unsupported/);

console.log('Local Reference synthetic filter-engine selection/count tests passed.');
