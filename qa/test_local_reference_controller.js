'use strict';

const assert = require('assert');
const fs = require('fs');

class FakeClassList {
  constructor(initial) { this.values = new Set(String(initial || '').split(/\s+/).filter(Boolean)); }
  add(value) { this.values.add(value); }
  remove(value) { this.values.delete(value); }
  contains(value) { return this.values.has(value); }
}

class FakeElement {
  constructor(className) {
    this.className = className || '';
    this.classList = new FakeClassList(className);
    this.listeners = Object.create(null);
    this.childrenBySelector = Object.create(null);
    this.childrenBySelectorAll = Object.create(null);
    this.children = [];
    this.style = {
      display: '',
      setProperty: function(name, value) { this[name] = value; },
      removeProperty: function(name) { delete this[name]; }
    };
    this.attributes = Object.create(null);
    this.value = '';
    this.checked = false;
    this.disabled = false;
    this.textContent = '';
    this._innerHTML = '';
    this.hidden = false;
    this.parentNode = null;
  }
  set innerHTML(value) {
    this._innerHTML = String(value || '');
    this.childrenBySelectorAll['[data-pt-lr-quick-view]'] = [];
    const quickButtonPattern = /<button\b([^>]*data-pt-lr-quick-view="[^"]+"[^>]*)>([^<]*)<\/button>/g;
    let match;
    while ((match = quickButtonPattern.exec(this._innerHTML))) {
      const button = new FakeElement();
      const attributePattern = /([:\w-]+)="([^"]*)"/g;
      let attribute;
      while ((attribute = attributePattern.exec(match[1]))) {
        button.setAttribute(attribute[1], attribute[2]);
      }
      button.textContent = match[2];
      this.childrenBySelectorAll['[data-pt-lr-quick-view]'].push(button);
    }
  }
  get innerHTML() { return this._innerHTML; }
  setAttribute(name, value) { this.attributes[name] = String(value); }
  getAttribute(name) {
    return Object.prototype.hasOwnProperty.call(this.attributes, name) ? this.attributes[name] : null;
  }
  removeAttribute(name) { delete this.attributes[name]; }
  addEventListener(name, handler) {
    this.listeners[name] = this.listeners[name] || [];
    this.listeners[name].push(handler);
  }
  removeEventListener(name, handler) {
    this.listeners[name] = (this.listeners[name] || []).filter(item => item !== handler);
  }
  dispatch(name, target, extra) {
    const event = Object.assign({
      target: target || this,
      preventDefault: function() { this.defaultPrevented = true; },
      stopPropagation: function() {}
    }, extra || {});
    (this.listeners[name] || []).slice().forEach(handler => handler(event));
    return event;
  }
  querySelector(selector) {
    if (!this.childrenBySelector[selector]) {
      const child = new FakeElement(selector.replace(/^\./, ''));
      const category = selector.match(/data-pt-lr-category="([^"]+)"/);
      if (category) child.setAttribute('data-pt-lr-category', category[1]);
      const facet = selector.match(/data-pt-lr-facet="([^"]+)"/);
      const facetValue = selector.match(/data-pt-lr-facet-value="([^"]+)"/);
      if (facet) child.setAttribute('data-pt-lr-facet', facet[1]);
      if (facetValue) child.setAttribute('data-pt-lr-facet-value', facetValue[1]);
      this.childrenBySelector[selector] = child;
    }
    return this.childrenBySelector[selector];
  }
  querySelectorAll(selector) { return this.childrenBySelectorAll[selector] || []; }
  appendChild(child) { child.parentNode = this; this.children.push(child); return child; }
  closest() { return null; }
}

const documentHead = new FakeElement('head');
global.document = {
  head: documentHead,
  activeElement: null,
  getElementById: function() { return null; },
  createElement: function() { return new FakeElement(); }
};
global.window = {
  BRIM: {},
  setTimeout: setTimeout,
  clearTimeout: clearTimeout
};
window.BRIM.localReferenceFilterEngine = require('../03_functions/js/brim_local_reference_filter_engine.js');

const controls = [];
global.L = {
  control: function() {
    return {
      onAdd: null,
      card: null,
      addTo: function(map) {
        this.removed = false;
        this.card = this.onAdd(map);
        if (!controls.includes(this)) controls.push(this);
        return this;
      },
      remove: function() { this.removed = true; }
    };
  },
  DomUtil: {create: function(tag, className) { return new FakeElement(className); }},
  DomEvent: {
    disableClickPropagation: function() {},
    disableScrollPropagation: function() {}
  },
  point: function(x, y) { return {x, y}; }
};

function fakeLayer(id) {
  return {
    options: {layerId: id},
    closeTooltipCalls: 0,
    closePopupCalls: 0,
    tooltipOpen: false,
    styleCalls: [],
    bringToFrontCalls: 0,
    listeners: Object.create(null),
    on: function(names, handler) {
      names.split(/\s+/).forEach(name => {
        this.listeners[name] = this.listeners[name] || [];
        this.listeners[name].push(handler);
      });
    },
    off: function(names, handler) {
      names.split(/\s+/).forEach(name => {
        this.listeners[name] = (this.listeners[name] || []).filter(item => item !== handler);
      });
    },
    fire: function(name, event) {
      (this.listeners[name] || []).slice().forEach(handler => handler(event || {}));
    },
    bindPopup: function(html, options) {
      this.popupHtml = html;
      this.popupOptions = options;
      return this;
    },
    openPopup: function(latlng) { this.openPopupLatLng = latlng; return this; },
    setStyle: function(style) { this.styleCalls.push(style); this.style = style; },
    bringToFront: function() { this.bringToFrontCalls += 1; return this; },
    closeTooltip: function() { this.tooltipOpen = false; this.closeTooltipCalls += 1; },
    closePopup: function() { this.closePopupCalls += 1; },
    isTooltipOpen: function() { return this.tooltipOpen; }
  };
}

const layers = {
  g1: fakeLayer('g1'),
  g2: fakeLayer('g2'),
  g3: fakeLayer('g3'),
  g4: fakeLayer('g4')
};
const labelLayers = {
  l1: fakeLayer('synthetic::f1::g1'),
  l2: fakeLayer('synthetic::f2::g2'),
  l3g3: fakeLayer('synthetic::f3::g3'),
  l3g4: fakeLayer('synthetic::f3::g4')
};
const rootMembers = new Set(Object.values(layers));
const labelRootMembers = new Set(Object.values(labelLayers));
const directMembers = new Set();
const groupRoot = {
  hasLayer: layer => rootMembers.has(layer),
  addLayer: layer => rootMembers.add(layer),
  removeLayer: layer => rootMembers.delete(layer),
  clearLayers: () => rootMembers.clear(),
  getLayers: () => Array.from(rootMembers)
};
const labelGroupRoot = {
  hasLayer: layer => labelRootMembers.has(layer),
  addLayer: layer => labelRootMembers.add(layer),
  removeLayer: layer => labelRootMembers.delete(layer),
  clearLayers: () => labelRootMembers.clear(),
  getLayers: () => Array.from(labelRootMembers)
};
const mapListeners = Object.create(null);
const map = {
  rootActive: true,
  labelRootActive: true,
  currentZoom: 8,
  fitBoundsCalls: [],
  closePopupCalls: 0,
  layerManager: {
    _byGroup: {
      'Reference – Synthetic': layers,
      'Labels – Synthetic': labelLayers
    },
    _groupContainers: {
      'Reference – Synthetic': groupRoot,
      'Labels – Synthetic': labelGroupRoot
    }
  },
  hasLayer: function(layer) {
    if (layer === groupRoot) return this.rootActive;
    if (layer === labelGroupRoot) return this.labelRootActive;
    return directMembers.has(layer) ||
      (this.rootActive && rootMembers.has(layer)) ||
      (this.labelRootActive && labelRootMembers.has(layer));
  },
  removeLayer: function(layer) {
    if (layer === groupRoot) this.rootActive = false;
    else if (layer === labelGroupRoot) this.labelRootActive = false;
    else directMembers.delete(layer);
    return this;
  },
  addLayer: function(layer) {
    if (layer === groupRoot) this.rootActive = true;
    else if (layer === labelGroupRoot) this.labelRootActive = true;
    else directMembers.add(layer);
    return this;
  },
  getZoom: function() { return this.currentZoom; },
  fitBounds: function(bounds, options) { this.fitBoundsCalls.push({bounds, options}); },
  closePopup: function() { this.closePopupCalls += 1; },
  on: function(names, handler) {
    names.split(/\s+/).forEach(name => {
      mapListeners[name] = mapListeners[name] || [];
      mapListeners[name].push(handler);
    });
  },
  off: function(names, handler) {
    names.split(/\s+/).forEach(name => {
      mapListeners[name] = (mapListeners[name] || []).filter(item => item !== handler);
    });
  },
  fire: function(name, event) {
    (mapListeners[name] || []).slice().forEach(handler => handler(event || {}));
  }
};

const payload = [{
  layer_id: 'synthetic',
  display_name: 'Synthetic',
  group_name: 'Reference – Synthetic',
  auto_supported: true,
  auto_default: true,
  feature_selection_supported: true,
  feature_selection_mode: 'semantic_feature_multi',
  feature_search_fields: ['name', 'identifier'],
  feature_display_field: 'name',
  auto_zoom_supported: true,
  auto_zoom_default: true,
  zoom_padding: 36,
  zoom_max: 12,
  preserve_view_on_reset: true,
  popup_layout: 'tabbed_card',
  primary_count_mode: 'semantic_feature',
  primary_count_label: 'Synthetic features',
  show_component_count: false,
  show_category_count: true,
  category_count_mode: 'semantic_feature',
  distinguish_units_supported: true,
  runtime_presentation: {
    restore_input_order: true,
    bring_selected_to_front: true,
    restore_style_on_mouseout: true,
    distinguish_fill_opacity: 0.14,
    selected_color: '#163E5A',
    selected_weight: 3.2,
    selected_fill_opacity: 0.14
  },
  legend_lbl_available: true,
  legend_lbl_zoom_visible: true,
  facets: [],
  component_count_label: 'mapped components',
  category_heading: 'BLM recommendation for wilderness designation',
  caution: 'Historical recommendation, not current WSA status. Management continues under the applicable FLPMA authority; verify current plans, closures, and field-office direction.',
  categories: [
    {category_key: 'a', label: 'Recommended suitable', fill_color: '#112233', stroke_color: '#010203', fill_opacity: 0.2, stroke_weight: 1, dash_array: '', legend_swatch_style: 'polygon'},
    {category_key: 'b', label: 'Recommended non-suitable', fill_color: '#445566', stroke_color: '#040506', fill_opacity: 0.2, stroke_weight: 1, dash_array: '2,3', legend_swatch_style: 'dotted_polygon'},
    {category_key: 'c', label: 'No recommendation', fill_color: '#778899', stroke_color: '#070809', fill_opacity: 0.2, stroke_weight: 1, dash_array: '', legend_swatch_style: 'polygon'},
    {category_key: 'd', label: 'Not stated', fill_color: '#AABBCC', stroke_color: '#0A0B0C', fill_opacity: 0.2, stroke_weight: 1, dash_array: '2,3', legend_swatch_style: 'dotted_polygon'}
  ],
  features: [
    {semantic_feature_key: 'f1', feature_key: 'f1', display_name: 'Alpha Ridge', category_keys: ['a'], search_text: 'alpha ridge nlcs-001', semantic_feature_bounds: [10, 10, 11, 11], geometry_component_count: 2},
    {semantic_feature_key: 'f2', feature_key: 'f2', display_name: 'Beta <script>alert(1)</script> Canyon', category_keys: ['a'], search_text: 'beta canyon nlcs-002', semantic_feature_bounds: [20, 20, 20.02, 20.02], geometry_component_count: 1},
    {semantic_feature_key: 'f3', feature_key: 'f3', display_name: 'Gamma Mountain', category_keys: ['b', 'c'], search_text: 'gamma mountain nlcs-003', semantic_feature_bounds: [30, 30, 32, 34], geometry_component_count: 5}
  ],
  records: [
    {geometry_key: 'g1', feature_key: 'f1', semantic_feature_key: 'f1', feature_display_name: 'Alpha Ridge', category_key: 'a', search_text: 'alpha ridge', semantic_feature_bounds: [10, 10, 11, 11], geometry_component_count: 2},
    {geometry_key: 'g2', feature_key: 'f2', semantic_feature_key: 'f2', feature_display_name: 'Beta Canyon', category_key: 'a', search_text: 'beta canyon', semantic_feature_bounds: [20, 20, 20.02, 20.02], geometry_component_count: 1},
    {geometry_key: 'g3', feature_key: 'f3', semantic_feature_key: 'f3', feature_display_name: 'Gamma Mountain', category_key: 'b', search_text: 'gamma mountain', semantic_feature_bounds: [30, 30, 32, 34], geometry_component_count: 3},
    {geometry_key: 'g4', feature_key: 'f3', semantic_feature_key: 'f3', feature_display_name: 'Gamma Mountain', category_key: 'c', search_text: 'gamma mountain', semantic_feature_bounds: [30, 30, 32, 34], geometry_component_count: 2}
  ],
  semantic_labels: {
    available: true,
    label_id: 'synthetic',
    label_group: 'Labels – Synthetic',
    anchor_strategy: 'polygon_visible_component_point_on_surface',
    visible_component_aware: true,
    min_zoom: 8,
    max_zoom: null,
    semantic_feature_count: 3,
    anchor_count: 4,
    records: [
      {label_record_key: 'synthetic::f1::g1', semantic_feature_key: 'f1', geometry_key: 'g1', label_text: 'Alpha Ridge', anchor_priority: 1, lng: 10, lat: 10},
      {label_record_key: 'synthetic::f2::g2', semantic_feature_key: 'f2', geometry_key: 'g2', label_text: 'Beta Canyon', anchor_priority: 1, lng: 20, lat: 20},
      {label_record_key: 'synthetic::f3::g4', semantic_feature_key: 'f3', geometry_key: 'g4', label_text: 'Gamma Mountain', anchor_priority: 1, lng: 31, lat: 31},
      {label_record_key: 'synthetic::f3::g3', semantic_feature_key: 'f3', geometry_key: 'g3', label_text: 'Gamma Mountain', anchor_priority: 2, lng: 30, lat: 30}
    ]
  }
}];

const controllerSource = fs.readFileSync(
  require.resolve('../03_functions/js/brim_local_reference_controller.js'),
  'utf8'
);
const controller = eval('(' + controllerSource + '\n)');
const mapRoot = new FakeElement('map');
controller.call(map, mapRoot, null, payload);

assert.strictEqual(controls.length, 1);
const card = controls[0].card;
assert.ok(card);
assert.strictEqual((card.innerHTML.match(/class="pt-lr-category-heading"/g) || []).length, 1);
assert.ok(card.innerHTML.includes('role="combobox"'));
assert.ok(card.innerHTML.includes('aria-autocomplete="list"'));
assert.ok(card.innerHTML.includes('role="listbox"'));
assert.ok(card.innerHTML.includes('Auto-zoom'));
assert.ok(card.innerHTML.includes('pt-lr-label-toggle'));
assert.ok(card.innerHTML.includes('pt-lr-label-toggle"> lbl (z8+)</label>'));
assert.ok(!card.innerHTML.includes('pt-lr-label-toggle"> LBL</label>'));
assert.ok(card.innerHTML.includes('Zoom to results'));
assert.ok(card.innerHTML.includes('Select named Synthetic features'));
assert.ok(!card.innerHTML.includes('Select named named'));
['Recommended suitable', 'Recommended non-suitable', 'No recommendation', 'Not stated'].forEach(label => {
  assert.ok(card.innerHTML.includes('<span>' + label + '</span>'), 'missing category label: ' + label);
});
assert.ok(!/<span>Suitable<\/span>|<span>Non-suitable<\/span>/.test(card.innerHTML));
assert.ok(card.innerHTML.includes(payload[0].caution));
assert.ok(!card.innerHTML.includes('protections continue pending Congressional action'));
assert.strictEqual(rootMembers.size, 4);
let stats = window.BRIM.localReferenceController.stats()[0];
assert.deepStrictEqual(stats.counts.total, {
  record_count: 4,
  semantic_feature_count: 3,
  geometry_component_count: 8
});
assert.deepStrictEqual(stats.counts.currently_showing, stats.counts.total);
assert.strictEqual(stats.resolved_leaflet_layers, 4);
assert.strictEqual(mapRoot.getAttribute('data-pt-lr-synthetic-semantic-count'), '3');
assert.strictEqual(mapRoot.getAttribute('data-pt-lr-synthetic-component-count'), '8');
assert.strictEqual(labelRootMembers.size, 3);
assert.deepStrictEqual(
  Array.from(labelRootMembers).map(layer => layer.options.layerId).sort(),
  ['synthetic::f1::g1', 'synthetic::f2::g2', 'synthetic::f3::g4']
);
assert.strictEqual(stats.labels_enabled, true);
assert.strictEqual(stats.visible_label_count, 3);
assert.strictEqual(stats.label_anchor_count, 4);
assert.strictEqual(stats.unresolved_visible_label_count, 0);

const legendLabelToggle = card.querySelector('.pt-lr-label-toggle');
legendLabelToggle.checked = false;
card.dispatch('change', legendLabelToggle);
assert.strictEqual(map.labelRootActive, false, 'legend LBL turns off shared label group');
legendLabelToggle.checked = true;
card.dispatch('change', legendLabelToggle);
assert.strictEqual(map.labelRootActive, true, 'legend LBL turns on shared label group');

// Distinguish mode is available for exactly one selected category, uses a
// deterministic semantic fill, retains category stroke, and turns itself off
// (restoring accepted category colors) when a second category is selected.
const categoryBForDistinguish = card.querySelector('[data-pt-lr-category="b"]');
card.querySelector('.pt-lr-none').dispatch('click');
const categoryAForDistinguish = card.querySelector('[data-pt-lr-category="a"]');
categoryAForDistinguish.checked = true;
card.dispatch('change', categoryAForDistinguish);
const distinguish = card.querySelector('.pt-lr-distinguish');
assert.strictEqual(distinguish.disabled, false);
distinguish.checked = true;
card.dispatch('change', distinguish);
assert.ok(/^hsl\(/.test(layers.g1.style.fillColor));
assert.notStrictEqual(layers.g1.style.fillColor, layers.g2.style.fillColor);
assert.strictEqual(layers.g1.style.color, '#010203');
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].distinguish_units, true);
categoryBForDistinguish.checked = true;
card.dispatch('change', categoryBForDistinguish);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].distinguish_units, false);
assert.strictEqual(layers.g1.style.fillColor, '#112233');
assert.strictEqual(layers.g3.style.fillColor, '#445566');
card.querySelector('.pt-lr-reset').dispatch('click');
map.fitBoundsCalls = [];

const countA = card.querySelector('[data-pt-lr-count="a"]');
const countB = card.querySelector('[data-pt-lr-count="b"]');
const categoryA = card.querySelector('[data-pt-lr-category="a"]');
const summary = card.querySelector('.pt-lr-summary');
assert.strictEqual(countA.textContent, '2');
assert.strictEqual(countA.getAttribute('aria-label'), '2 of 2 Synthetic features');
assert.strictEqual(countB.textContent, '1');
assert.strictEqual(summary.textContent, 'Showing 3 of 3 Synthetic features');
assert.ok(!/\b(rec|sem|geom)\b/.test([countA.textContent, countB.textContent, summary.textContent].join(' ')));

const search = card.querySelector('.pt-lr-search');
const suggestions = card.querySelector('.pt-lr-suggestions');
const searchStatus = card.querySelector('.pt-lr-search-status');
const chips = card.querySelector('.pt-lr-chips');

// Typing and no-match searches never alter visibility or map view.
search.value = 'a';
search.dispatch('input');
assert.strictEqual(rootMembers.size, 4);
assert.strictEqual(map.fitBoundsCalls.length, 0);
assert.strictEqual(search.getAttribute('aria-expanded'), 'true');
assert.ok(suggestions.innerHTML.includes('Alpha Ridge'));
search.dispatch('keydown', search, {key: 'ArrowDown'});
assert.strictEqual(search.getAttribute('aria-activedescendant'), 'pt-lr-listbox-synthetic-option-1');
search.dispatch('keydown', search, {key: 'ArrowUp'});
assert.strictEqual(search.getAttribute('aria-activedescendant'), 'pt-lr-listbox-synthetic-option-0');
search.value = 'does not exist';
search.dispatch('input');
assert.strictEqual(searchStatus.textContent, 'No matching Synthetic features.');
assert.strictEqual(rootMembers.size, 4);
assert.strictEqual(map.fitBoundsCalls.length, 0);

// Keyboard selection creates a safe, accessible chip and fits a small feature.
Object.values(layers).forEach(layer => { layer.bringToFrontCalls = 0; });
search.value = 'nlcs-002';
search.dispatch('input');
search.dispatch('keydown', search, {key: 'Enter'});
stats = window.BRIM.localReferenceController.stats()[0];
assert.deepStrictEqual(stats.applied_feature_keys, ['f2']);
assert.deepStrictEqual(Array.from(rootMembers), [layers.g2]);
assert.ok(layers.g2.bringToFrontCalls > 0, 'selected existing path is brought to front');
assert.strictEqual(layers.g2.style.color, '#163E5A');
assert.strictEqual(layers.g2.style.weight, 3.2);
assert.strictEqual(layers.g2.style.fillOpacity, 0.14);
const frontCallsBeforeMouseout = layers.g2.bringToFrontCalls;
layers.g2.fire('mouseout');
assert.ok(layers.g2.bringToFrontCalls > frontCallsBeforeMouseout);
assert.strictEqual(layers.g2.style.color, '#163E5A');
assert.strictEqual(search.value, '');
assert.strictEqual(map.fitBoundsCalls.length, 1);
assert.deepStrictEqual(map.fitBoundsCalls[0], {
  bounds: [[20, 20], [20.02, 20.02]],
  options: {padding: [36, 36], maxZoom: 12, animate: false}
});
assert.ok(chips.innerHTML.includes('Beta &lt;script&gt;alert(1)&lt;/script&gt; Canyon'));
assert.ok(!chips.innerHTML.includes('<script>'));
assert.ok(chips.innerHTML.includes('class="pt-lr-chip-remove"'));
assert.ok(chips.innerHTML.includes('aria-label="Remove Beta &lt;script&gt;alert(1)&lt;/script&gt; Canyon from selected Synthetic features"'));

// A second chip combines semantic-feature bounds; duplicates are unavailable.
search.value = 'alpha';
search.dispatch('input');
search.dispatch('keydown', search, {key: 'Enter'});
stats = window.BRIM.localReferenceController.stats()[0];
assert.deepStrictEqual(stats.applied_feature_keys, ['f1', 'f2']);
assert.deepStrictEqual(stats.visible_semantic_feature_bounds, [10, 10, 20.02, 20.02]);
assert.strictEqual(map.fitBoundsCalls.length, 2);
assert.deepStrictEqual(map.fitBoundsCalls[1].bounds, [[10, 10], [20.02, 20.02]]);
search.value = 'alpha';
search.dispatch('input');
assert.strictEqual(searchStatus.textContent, 'No matching Synthetic features.');
search.dispatch('keydown', search, {key: 'Enter'});
assert.deepStrictEqual(window.BRIM.localReferenceController.stats()[0].applied_feature_keys, ['f1', 'f2']);

function removeChip(key) {
  const button = new FakeElement('pt-lr-chip-remove');
  button.setAttribute('data-pt-lr-remove-feature', key);
  button.closest = selector => selector === '[data-pt-lr-remove-feature]' ? button : null;
  chips.dispatch('click', button);
}
removeChip('f2');
stats = window.BRIM.localReferenceController.stats()[0];
assert.deepStrictEqual(stats.applied_feature_keys, ['f1']);
assert.deepStrictEqual(stats.visible_semantic_feature_bounds, [10, 10, 11, 11]);
assert.strictEqual(map.fitBoundsCalls.length, 3);
removeChip('f1');
assert.deepStrictEqual(window.BRIM.localReferenceController.stats()[0].applied_feature_keys, []);
assert.strictEqual(rootMembers.size, 4, 'removing the final chip returns to category-filter view');
assert.strictEqual(map.fitBoundsCalls.length, 3, 'final-chip removal must not zoom to the full layer');

// Selecting a feature automatically enables its authoritative category.
categoryA.checked = false;
card.dispatch('change', categoryA);
assert.deepStrictEqual(Array.from(rootMembers), [layers.g3, layers.g4]);
const beforeAutoEnableZoom = map.fitBoundsCalls.length;
search.value = 'nlcs-001';
search.dispatch('input');
search.dispatch('keydown', search, {key: 'Enter'});
assert.strictEqual(categoryA.checked, true);
assert.deepStrictEqual(Array.from(rootMembers), [layers.g1]);
assert.strictEqual(map.fitBoundsCalls.length, beforeAutoEnableZoom + 1);

// Auto off stages both categories and chips; Apply reconciles and zooms.
const auto = card.querySelector('.pt-lr-auto');
auto.checked = false;
card.dispatch('change', auto);
const beforeStagedSelection = map.fitBoundsCalls.length;
search.value = 'nlcs-002';
search.dispatch('input');
search.dispatch('keydown', search, {key: 'Enter'});
stats = window.BRIM.localReferenceController.stats()[0];
assert.deepStrictEqual(stats.draft_feature_keys, ['f1', 'f2']);
assert.deepStrictEqual(stats.applied_feature_keys, ['f1']);
assert.deepStrictEqual(Array.from(rootMembers), [layers.g1]);
assert.deepStrictEqual(Array.from(labelRootMembers), [labelLayers.l1]);
assert.strictEqual(map.fitBoundsCalls.length, beforeStagedSelection);
card.querySelector('.pt-lr-apply').dispatch('click');
assert.deepStrictEqual(Array.from(rootMembers), [layers.g1, layers.g2]);
assert.deepStrictEqual(
  Array.from(labelRootMembers).map(layer => layer.options.layerId).sort(),
  ['synthetic::f1::g1', 'synthetic::f2::g2']
);
assert.strictEqual(map.fitBoundsCalls.length, beforeStagedSelection + 1);

// Auto-zoom is independent; explicit Zoom to results always fits applied data.
const autoZoom = card.querySelector('.pt-lr-auto-zoom');
autoZoom.checked = false;
card.dispatch('change', autoZoom);
removeChip('f2');
card.querySelector('.pt-lr-apply').dispatch('click');
assert.strictEqual(map.fitBoundsCalls.length, beforeStagedSelection + 1);
card.querySelector('.pt-lr-zoom-results').dispatch('click');
assert.strictEqual(map.fitBoundsCalls.length, beforeStagedSelection + 2);
assert.deepStrictEqual(map.fitBoundsCalls.at(-1).bounds, [[10, 10], [11, 11]]);

// None/no-results, Reset, and All do not trigger automatic map moves.
const beforeNoZoomActions = map.fitBoundsCalls.length;
card.querySelector('.pt-lr-none').dispatch('click');
card.querySelector('.pt-lr-apply').dispatch('click');
assert.strictEqual(rootMembers.size, 0);
assert.strictEqual(labelRootMembers.size, 0);
assert.strictEqual(map.labelRootActive, true, 'zero results preserve logical LBL on');
assert.strictEqual(card.querySelector('.pt-lr-zoom-results').disabled, true);
assert.strictEqual(map.fitBoundsCalls.length, beforeNoZoomActions);
card.querySelector('.pt-lr-reset').dispatch('click');
assert.strictEqual(rootMembers.size, 4);
assert.strictEqual(labelRootMembers.size, 3);
assert.strictEqual(map.fitBoundsCalls.length, beforeNoZoomActions);
assert.strictEqual(search.value, '');
assert.strictEqual(chips.innerHTML, '');
card.querySelector('.pt-lr-all').dispatch('click');
assert.strictEqual(map.fitBoundsCalls.length, beforeNoZoomActions);

// A semantic feature with two geometry records uses the highest-priority
// currently visible component anchor, never an anchor in a filtered component.
const categoryC = card.querySelector('[data-pt-lr-category="c"]');
categoryC.checked = false;
card.dispatch('change', categoryC);
assert.ok(rootMembers.has(layers.g3));
assert.ok(!rootMembers.has(layers.g4));
assert.ok(labelRootMembers.has(labelLayers.l3g3));
assert.ok(!labelRootMembers.has(labelLayers.l3g4));
card.querySelector('.pt-lr-reset').dispatch('click');
assert.ok(labelRootMembers.has(labelLayers.l3g4));
assert.ok(!labelRootMembers.has(labelLayers.l3g3));

// Zoom gating empties/restores the class-owned label membership without
// changing the LBL preference or the applied semantic result set.
map.currentZoom = 7;
map.fire('zoomend');
assert.strictEqual(labelRootMembers.size, 0);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].labels_enabled, true);
map.currentZoom = 8;
map.fire('zoomend');
assert.strictEqual(labelRootMembers.size, 3);

// Layer teardown is idempotent and layer-on returns to one default controller.
search.value = 'alpha';
search.dispatch('input');
search.dispatch('keydown', search, {key: 'Enter'});
search.dispatch('blur');
layers.g1.tooltipOpen = true;
directMembers.add(layers.g1); // Simulate an unmanaged/direct rendering residue.
map.rootActive = false;
map.fire('overlayremove', {name: 'Reference – Synthetic'});
map.fire('overlayremove', {name: 'Reference – Synthetic'});
assert.strictEqual(rootMembers.size, 0);
assert.strictEqual(labelRootMembers.size, 0);
assert.strictEqual(map.labelRootActive, false);
assert.strictEqual(directMembers.size, 0);
stats = window.BRIM.localReferenceController.stats()[0];
assert.deepStrictEqual(stats.draft_feature_keys, []);
assert.strictEqual(stats.active, false);
assert.strictEqual(stats.group_root_attached, false);
assert.strictEqual(stats.group_member_layer_count, 0);
assert.strictEqual(stats.attached_owned_layer_count, 0);
assert.strictEqual(stats.open_owned_tooltip_count, 0);
assert.strictEqual(stats.card_count, 0);
assert.strictEqual(stats.pending_controller_callback_count, 0);
assert.strictEqual(mapRoot.getAttribute('data-pt-lr-synthetic-semantic-count'), '0');
assert.strictEqual(mapRoot.getAttribute('data-pt-lr-synthetic-component-count'), '0');
map.rootActive = true;
map.fire('overlayadd', {name: 'Reference – Synthetic'});
assert.strictEqual(rootMembers.size, 4);
assert.strictEqual(labelRootMembers.size, 3);
stats = window.BRIM.localReferenceController.stats()[0];
assert.strictEqual(stats.active, true);
assert.strictEqual(stats.group_root_attached, true);
assert.strictEqual(stats.group_member_layer_count, 4);
assert.strictEqual(stats.attached_owned_layer_count, 4);
assert.strictEqual(stats.card_count, 1);
assert.strictEqual(stats.labels_enabled, false);
assert.strictEqual(stats.visible_label_count, 0);
assert.strictEqual(stats.label_group_member_count, 3);
assert.deepStrictEqual(stats.draft_feature_keys, []);
assert.strictEqual(mapRoot.getAttribute('data-pt-lr-synthetic-semantic-count'), '3');
assert.strictEqual(mapRoot.getAttribute('data-pt-lr-synthetic-component-count'), '8');
assert.strictEqual(controls.length, 1);

// LBL remains an independent display preference after layer reactivation.
map.labelRootActive = true;
map.fire('overlayadd', {name: 'Labels – Synthetic'});
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].visible_label_count, 3);

// A normal off/on cycle performs only the single full-record presentation pass
// needed by activation; unchanged Distinguish/ACEC resets do not repeat it.
Object.values(layers).forEach(layer => {
  layer.styleCalls = [];
  layer.bringToFrontCalls = 0;
});
map.rootActive = false;
map.fire('overlayremove', {name: 'Reference – Synthetic'});
map.rootActive = true;
map.fire('overlayadd', {name: 'Reference – Synthetic'});
Object.values(layers).forEach(layer => {
  assert.strictEqual(layer.styleCalls.length, 1, 'one activation style pass for ' + layer.options.layerId);
  assert.strictEqual(layer.bringToFrontCalls, 1, 'one activation order pass for ' + layer.options.layerId);
});

// Ten full off/on cycles remain exact, default, and duplicate-free.
for (let cycle = 0; cycle < 10; cycle += 1) {
  map.rootActive = false;
  map.fire('overlayremove', {name: 'Reference – Synthetic'});
  stats = window.BRIM.localReferenceController.stats()[0];
  assert.strictEqual(stats.group_member_layer_count, 0, 'off cycle ' + cycle);
  assert.strictEqual(stats.attached_owned_layer_count, 0, 'off cycle ' + cycle);
  assert.strictEqual(stats.card_count, 0, 'off cycle ' + cycle);
  assert.strictEqual(stats.label_group_member_count, 0, 'label off cycle ' + cycle);
  map.rootActive = true;
  map.fire('overlayadd', {name: 'Reference – Synthetic'});
  stats = window.BRIM.localReferenceController.stats()[0];
  assert.strictEqual(stats.group_member_layer_count, 4, 'on cycle ' + cycle);
  assert.strictEqual(stats.attached_owned_layer_count, 4, 'on cycle ' + cycle);
  assert.strictEqual(stats.card_count, 1, 'on cycle ' + cycle);
  assert.strictEqual(stats.label_group_member_count, 3, 'label on cycle ' + cycle);
  assert.strictEqual(stats.labels_enabled, false, 'LBL stays off after layer cycle ' + cycle);
  assert.deepStrictEqual(stats.draft_feature_keys, [], 'default chips cycle ' + cycle);
}

const tooltipCloseCountBeforePopup = layers.g1.closeTooltipCalls;
map.fire('popupopen', {popup: {_source: layers.g1}});
assert.strictEqual(layers.g1.closeTooltipCalls, tooltipCloseCountBeforePopup + 1);

assert.ok(!/zoomstart|movestart|moveend/.test(controllerSource));
assert.ok(controllerSource.includes("listen(map, 'zoomend'"));
assert.ok(controllerSource.includes("action === 'typing'"));
assert.ok(controllerSource.includes("action === 'none'"));
assert.ok(controllerSource.includes("action === 'reset'"));
assert.ok(controllerSource.includes("action === 'all'"));
assert.ok(controllerSource.includes('.pt-lr-chip{display:inline-flex;max-width:100%;min-width:0'));
assert.ok(controllerSource.includes('.pt-local-reference-card{box-sizing:border-box;width:330px'));
assert.ok(controllerSource.includes('.pt-lr-head-controls'));
assert.ok(controllerSource.includes('.pt-lr-facet-value-swatch'));
assert.ok(controllerSource.includes('.pt-lr-map-details>summary::before{content:"▸";position:absolute;left:7px'));
assert.ok(controllerSource.includes('[data-pt-local-reference-layer="ca_desert_ncl"]{width:330px;max-height:none;overflow:visible'));
assert.ok(controllerSource.includes('.pt-local-reference-card[data-pt-local-reference-layer="acec"] .pt-lr-search,.pt-local-reference-card[data-pt-local-reference-layer="acec"] .pt-lr-suggestions{width:250px'));
assert.ok(controllerSource.includes('.pt-local-reference-card[data-pt-local-reference-layer="acec"] .pt-lr-quick-views{display:grid;grid-template-columns:repeat(3,minmax(0,1fr))'));
assert.ok(controllerSource.includes('.pt-local-reference-card[data-pt-local-reference-layer="acec"] .pt-lr-quick-views button{width:100%;min-height:20px'));
assert.ok(controllerSource.includes('.pt-lr-quick-views button[aria-pressed=true]::before{content:"\\\\2713"'));
assert.ok(controllerSource.includes('grid-template-columns:15px minmax(0,1fr) 14px 46px'));
assert.ok(controllerSource.includes("acecValueTheme(snapshot, 'applied_facets')"));
assert.ok(controllerSource.includes('.pt-local-reference-card[data-pt-local-reference-layer="acec"] .pt-lr-facet-collapsible>summary::before'));
assert.ok(controllerSource.includes('WSA name context:'));
assert.ok(controllerSource.includes('WSA-name inventory comparison'));
assert.ok(controllerSource.includes('@media (max-width:420px)'));
assert.ok(/@media \(pointer:coarse\)/.test(controllerSource));
assert.ok(controllerSource.includes('.leaflet-tooltip.pt-wsa-hover-tooltip'));
assert.ok(controllerSource.includes('.leaflet-container.pt-lr-tabbed-popup-open .leaflet-popup-pane{z-index:1100}'));
assert.ok(controllerSource.includes("listen(map, 'popupclose', onAnyPopupClose)"));
assert.ok(controllerSource.includes('height:var(--pt-lr-popup-panel-height,auto)'));
assert.ok(controllerSource.includes('max-height:min(54vh,450px)'));
assert.ok(controllerSource.includes('max-height:min(50vh,390px)'));
assert.ok(!controllerSource.includes('.pt-lr-popup-panel-scroll{height:min('));
assert.ok(controllerSource.includes('tallestNaturalPanelHeight'));
assert.ok(controllerSource.includes('var floor = Math.min(112, cap)'));
assert.ok(controllerSource.includes('naturalHeight + 1'));
assert.ok(controllerSource.includes('window.ResizeObserver'));
assert.ok(controllerSource.includes('document.fonts.ready'));
assert.ok(controllerSource.includes('pt-lr-popup-measuring'));
assert.ok(controllerSource.includes('state.popup._updateLayout'));
assert.ok(controllerSource.includes("listenDom(el, 'toggle', onTabbedPopupDetailsToggle, true)"));
assert.ok(controllerSource.includes('teardownInactiveLayer'));
assert.ok(controllerSource.includes('detachOwnedGeometry'));
assert.ok(controllerSource.includes('detachOwnedSemanticLabels'));
assert.ok(controllerSource.includes('reconcileSemanticLabels'));
assert.ok(!controllerSource.includes('cloneNode(true)'));
assert.ok(controllerSource.includes('overflow-wrap:break-word!important'));
assert.ok(!controllerSource.includes('overflow-wrap:anywhere'));
assert.ok(controllerSource.includes('.leaflet-tooltip.pt-wsa-hover-tooltip,.leaflet-tooltip.pt-trails-hover-tooltip{display:none!important}'));
assert.ok(!controllerSource.includes("' rec · '"));
assert.ok(!controllerSource.includes("' sem · '"));
assert.ok(!controllerSource.includes("' geom'"));

window.BRIM.localReferenceController.destroy();
assert.strictEqual(controls[0].removed, true);
assert.strictEqual(rootMembers.size, 0);
assert.strictEqual(labelRootMembers.size, 0);
assert.strictEqual(map.rootActive, false);
assert.strictEqual(map.labelRootActive, false);
assert.strictEqual(mapRoot.getAttribute('data-pt-lr-synthetic-active'), null);
assert.strictEqual((mapListeners.overlayadd || []).length, 0);
assert.strictEqual((mapListeners.overlayremove || []).length, 0);
assert.strictEqual((mapListeners.popupopen || []).length, 0);
assert.strictEqual((mapListeners.zoomend || []).length, 0);

// An initially inactive source group is normalized to the same zero-owned
// invariant before any layer-on event.
Object.values(layers).forEach(layer => rootMembers.add(layer));
Object.values(labelLayers).forEach(layer => labelRootMembers.add(layer));
map.rootActive = false;
map.labelRootActive = false;
const inactiveMapRoot = new FakeElement('map');
controller.call(map, inactiveMapRoot, null, payload);
stats = window.BRIM.localReferenceController.stats()[0];
assert.strictEqual(rootMembers.size, 0);
assert.strictEqual(labelRootMembers.size, 0);
assert.strictEqual(stats.active, false);
assert.strictEqual(stats.group_member_layer_count, 0);
assert.strictEqual(stats.attached_owned_layer_count, 0);
assert.strictEqual(stats.card_count, 0);
assert.strictEqual(inactiveMapRoot.getAttribute('data-pt-lr-synthetic-component-count'), '0');
window.BRIM.localReferenceController.destroy();

// Federal Wilderness uses normalized lookup records and creates its three-tab
// popup only when a mapped component is selected.
rootMembers.add(layers.g1);
map.rootActive = true;
map.layerManager = {
  _byGroup: {'Reference – Federal Wilderness': {g1: layers.g1}},
  _groupContainers: {'Reference – Federal Wilderness': groupRoot}
};
const federalPayload = [{
  layer_id: 'federal_wilderness',
  display_name: 'Federal Wilderness',
  group_name: 'Reference – Federal Wilderness',
  auto_supported: true,
  auto_default: true,
  feature_selection_supported: true,
  feature_selection_mode: 'semantic_feature_multi',
  auto_zoom_supported: true,
  auto_zoom_default: true,
  zoom_padding: 36,
  zoom_max: 11,
  preserve_view_on_reset: true,
  popup_layout: 'tabbed_card',
  primary_count_mode: 'semantic_feature',
  primary_count_label: 'named wildernesses',
  show_component_count: true,
  show_category_count: true,
  category_count_mode: 'geometry_component',
  distinguish_units_supported: true,
  facets: [],
  component_count_label: 'mapped components',
  category_heading: 'Managing agency',
  caution: 'Verify current access and agency direction.',
  categories: [
    {category_key: 'usfs', label: 'U.S. Forest Service', fill_color: '#228B22', stroke_color: '#228B22', fill_opacity: 0.18, stroke_weight: 1.6, dash_array: '', legend_swatch_style: 'polygon'}
  ],
  features: [
    {semantic_feature_key: 'fw-95', feature_key: 'fw-95', display_name: 'Ansel Adams Wilderness', category_keys: ['usfs'], search_text: 'ansel adams wilderness', semantic_feature_bounds: [37, -120, 38, -119], geometry_component_count: 1}
  ],
  records: [
    {geometry_key: 'g1', feature_key: 'fw-95', semantic_feature_key: 'fw-95', category_key: 'usfs', geometry_component_count: 1, facet_values: {}}
  ],
  federal_wilderness: {
    semantics: [{
      wilderness_id: 'fw-95', official_name: 'Ansel Adams Wilderness',
      alternate_names: 'N/A', states: 'CA', designation_date: '1964-09-03',
      designation_year: 1964, original_public_law: 'Public Law 88-577',
      subsequent_public_laws: 'Public Law 98-425', official_reference_acres: 230872,
      source_component_count: 1, managing_agencies: 'USFS', shared_management: false,
      summary_short: 'A named federal wilderness.',
      management_access_summary: 'Check current Forest Service information.',
      wilderness_connect_url: 'https://example.test/wilderness',
      congress_search_url: 'https://example.test/congress',
      acreage_source: 'Official published interagency reference acreage.',
      validation_status: 'PASS WITH DOCUMENTED EXPLANATION',
      explanatory_note: 'Public Law 98-425 renamed the original Minarets Wilderness.',
      evidence_source: 'Direct GovInfo public law.',
      evidence_url: 'https://www.govinfo.gov/content/pkg/PLAW-88publ577/html/PLAW-88publ577.htm'
    }],
    components: [{
      component_id: 'g1', wilderness_id: 'fw-95', agency_key: 'USFS',
      office_key: 'office-001', geographic_state: 'CA', source_gis_acres: 231457,
      calculated_acres: 231457.2, component_description: 'USFS-managed component.',
      geometry_caveat: 'Preserve component geometry.'
    }],
    agencies: [{agency_key: 'USFS', name: 'U.S. Forest Service'}],
    offices: [{office_key: 'office-001', local_managing_unit: 'Inyo National Forest', local_unit_url: 'https://example.test/inyo', blm_office: 'undefined', blm_office_url: ''}],
    documents: [
      {wilderness_id: 'fw-95', title: 'Public Law 88-577 — Wilderness Act', type: 'public law', publication_date: '1964-09-03', url: 'https://www.govinfo.gov/content/pkg/PLAW-88publ577/html/PLAW-88publ577.htm', authority_level: '1 - public law'},
      {wilderness_id: 'fw-95', title: 'Official management page', type: 'agency page', url: 'https://example.test/management', authority_level: '3 - official managing-agency page'},
      {wilderness_id: 'fw-95', title: 'Secondary profile', type: 'profile', url: 'https://example.test/profile', authority_level: '9 - interagency reference'}
    ],
    policy: [
      {topic: 'Designation', language: 'Federal Wilderness is a congressional designation.'},
      {topic: 'Management identity', language: 'Named wilderness and selected component management are distinct.'},
      {topic: 'BLM stewardship', language: 'Publication does not establish BLM management.'},
      {topic: 'Access', language: 'Designation does not guarantee public access.'},
      {topic: 'Rules', language: 'Rules vary by managing agency.'},
      {topic: 'Motorized/mechanized use', language: 'Motorized use is generally prohibited.'},
      {topic: 'Acreage', language: 'GIS and published acreage can differ.'},
      {topic: 'Boundary meaning', language: 'The polygon is not an access route.'},
      {topic: 'Litigation', language: 'Research links are discovery tools.'},
      {topic: 'Current conditions', language: 'Check current alerts.'}
    ],
    sources: [
      {title: 'Official GIS source', agency: 'BLM', url: 'https://example.test/source', type: 'Official GIS service'},
      {title: 'Unrelated Field Office', agency: 'BLM', url: 'https://example.test/unrelated-office', type: 'Official local office page'}
    ],
    templates: {govinfo_public_law: 'https://www.govinfo.gov/content/pkg/PLAW-{congress}publ{number}/html/PLAW-{congress}publ{number}.htm'}
  }
}];
const federalMapRoot = new FakeElement('map');
controller.call(map, federalMapRoot, null, federalPayload);
const federalCard = controls.at(-1).card;
assert.strictEqual(federalCard.getAttribute('data-pt-local-reference-layer'), 'federal_wilderness');
assert.ok(federalCard.innerHTML.includes('<details class="pt-lr-map-details"><summary>Map / layer note</summary>'));
assert.ok(!federalCard.innerHTML.includes('Filter by category or select named features'));
assert.strictEqual(federalCard.querySelector('.pt-lr-summary').textContent, '1/1 wildernesses · 1/1 components');
const federalTooltipCloseBefore = layers.g1.closeTooltipCalls;
layers.g1.fire('click', {latlng: {lat: 37.5, lng: -119.5}});
assert.strictEqual(layers.g1.closeTooltipCalls, federalTooltipCloseBefore + 1);
assert.strictEqual((layers.g1.popupHtml.match(/role="tab"/g) || []).length, 3);
assert.ok(layers.g1.popupHtml.includes('Original designation'));
assert.ok(layers.g1.popupHtml.includes('1964-09-03 · Public Law 88-577'));
assert.ok(layers.g1.popupHtml.includes('Official published wilderness acreage'));
assert.ok(layers.g1.popupHtml.includes('230,872 acres'));
assert.ok(layers.g1.popupHtml.includes('Selected mapped-component source GIS acreage'));
assert.ok(layers.g1.popupHtml.includes('231,457.0 acres'));
assert.ok(layers.g1.popupHtml.includes('PASS WITH DOCUMENTED EXPLANATION'));
assert.ok(layers.g1.popupHtml.includes('PLAW-98publ425'));
assert.ok(layers.g1.popupHtml.includes('Official GIS source'));
assert.ok(!layers.g1.popupHtml.includes('Unrelated Field Office'));
assert.ok(!/>\s*(?:NA|N\/A|null|undefined)\s*</i.test(layers.g1.popupHtml));
assert.strictEqual(layers.g1.popupOptions.className, 'pt-local-reference-tabbed-popup');
assert.deepStrictEqual(layers.g1.openPopupLatLng, {lat: 37.5, lng: -119.5});
assert.ok(controllerSource.includes('[data-pt-local-reference-layer="federal_wilderness"]{width:330px;max-height:none;overflow:visible'));
assert.ok(controllerSource.includes('[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-categories{display:block}'));
assert.ok(controllerSource.includes('[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-facet-values{display:block}'));
assert.ok(controllerSource.includes('[data-pt-local-reference-layer="federal_wilderness"].pt-map-card-undocked{max-height:calc(100vh - 8px);overflow-y:auto'));
assert.ok(controllerSource.includes('class="pt-nm-shared-boundary-row"'));
assert.ok(controllerSource.includes('counts: matching / total'));
assert.ok(controllerSource.includes("setAttribute('title', countDescription)"));
assert.ok(!controllerSource.includes('Map colors'));
assert.ok(!controllerSource.includes('pt-nm-map-key'));
window.BRIM.localReferenceController.destroy();
assert.strictEqual((layers.g1.listeners.click || []).length, 0);

// ACEC value-family quick views derive their state from the facet engine and
// apply the centralized family color only for an applied single-value state.
Object.values(layers).forEach(layer => rootMembers.add(layer));
map.rootActive = true;
map.layerManager = {
  _byGroup: {'Reference – ACECs': layers},
  _groupContainers: {'Reference – ACECs': groupRoot}
};
const acecValues = [
  {value_key: 'water_aquatic', label: 'Fish or aquatic resources', swatch_color: '#3B82A0', sort_order: 1},
  {value_key: 'wildlife_and_habitat', label: 'Wildlife and habitat', swatch_color: '#7A9A4A', sort_order: 2},
  {value_key: 'botanical_or_ecological', label: 'Natural systems or processes', swatch_color: '#4F8C68', sort_order: 3},
  {value_key: 'cultural_archaeological_historic', label: 'Cultural or historic', swatch_color: '#A66A43', sort_order: 4},
  {value_key: 'scenic', label: 'Scenic', swatch_color: '#8A6DAA', sort_order: 5},
  {value_key: 'other_or_unresolved', label: 'Natural hazard or other', swatch_color: '#B58A3D', sort_order: 6}
];
const acecPayload = [{
  layer_id: 'acec',
  display_name: 'ACECs',
  group_name: 'Reference – ACECs',
  auto_supported: true,
  auto_default: true,
  feature_selection_supported: false,
  feature_selection_mode: 'none',
  auto_zoom_supported: true,
  auto_zoom_default: false,
  zoom_padding: 36,
  zoom_max: 11,
  preserve_view_on_reset: true,
  popup_layout: 'tabbed_card',
  primary_count_mode: 'semantic_feature',
  primary_count_label: 'ACECs',
  show_component_count: true,
  show_category_count: false,
  category_count_mode: 'semantic_feature',
  distinguish_units_supported: false,
  category_filter_visible: false,
  component_count_label: 'parts',
  category_heading: 'Designation',
  caution: 'Planning-designation boundary; verify current direction.',
  categories: [{
    category_key: 'acec', label: 'Area of Critical Environmental Concern',
    fill_color: '#B86F52', stroke_color: '#7A3F2E', fill_opacity: 0.12,
    stroke_weight: 1.6, dash_array: '', legend_swatch_style: 'polygon'
  }],
  facets: [{
    facet_key: 'relevant_value_family',
    label: 'Relevant and important values',
    count_mode: 'semantic_feature',
    collapsible: true,
    open_default: true,
    thematic_style: {
      multiple_selection: 'neutral', fill_opacity: 0.22,
      stroke_weight: 2, stroke_darken: 0.28
    },
    values: acecValues
  }, {
    facet_key: 'planning_framework',
    label: 'Planning framework',
    count_mode: 'semantic_feature',
    collapsible: true,
    open_default: false,
    values: [
      {value_key: 'drecp', label: 'DRECP', sort_order: 1},
      {value_key: 'other_plan', label: 'Other plan', sort_order: 2}
    ]
  }, {
    facet_key: 'field_office_context',
    label: 'Field office context',
    count_mode: 'semantic_feature',
    collapsible: true,
    open_default: false,
    values: [
      {value_key: 'barstow_field_office', label: 'Barstow Field Office', sort_order: 1},
      {value_key: 'central_coast_field_office', label: 'Central Coast Field Office', sort_order: 2},
      {value_key: 'needles_field_office', label: 'Needles Field Office', sort_order: 3},
      {value_key: 'ridgecrest_field_office', label: 'Ridgecrest Field Office', sort_order: 4}
    ]
  }],
  quick_views: [
    {quick_view_key: 'fish_aquatic', label: 'Fish / aquatic values', facet_key: 'relevant_value_family', value_key: 'water_aquatic'},
    {quick_view_key: 'drecp', label: 'DRECP ACECs', facet_key: 'planning_framework', value_key: 'drecp'},
    {quick_view_key: 'wildlife_habitat', label: 'Wildlife / habitat', facet_key: 'relevant_value_family', value_key: 'wildlife_and_habitat'},
    {quick_view_key: 'cultural_historic', label: 'Cultural / historic', facet_key: 'relevant_value_family', value_key: 'cultural_archaeological_historic'},
    {quick_view_key: 'scenic', label: 'Scenic', facet_key: 'relevant_value_family', value_key: 'scenic'},
    {quick_view_key: 'natural_systems', label: 'Natural systems', facet_key: 'relevant_value_family', value_key: 'botanical_or_ecological'}
  ],
  features: [],
  records: [
    {geometry_key: 'g1', semantic_feature_key: 'acec-1', category_key: 'acec', geometry_component_count: 1, facet_values: {relevant_value_family: ['water_aquatic'], planning_framework: ['drecp'], field_office_context: ['barstow_field_office', 'needles_field_office', 'ridgecrest_field_office']}},
    {geometry_key: 'g2', semantic_feature_key: 'acec-2', category_key: 'acec', geometry_component_count: 1, facet_values: {relevant_value_family: ['wildlife_and_habitat'], planning_framework: ['other_plan'], field_office_context: ['needles_field_office']}},
    {geometry_key: 'g3', semantic_feature_key: 'acec-3', category_key: 'acec', geometry_component_count: 1, facet_values: {relevant_value_family: ['water_aquatic', 'wildlife_and_habitat'], planning_framework: ['other_plan'], field_office_context: ['ridgecrest_field_office']}},
    {geometry_key: 'g4', semantic_feature_key: 'acec-4', category_key: 'acec', geometry_component_count: 1, facet_values: {relevant_value_family: ['scenic'], planning_framework: ['drecp'], field_office_context: ['central_coast_field_office']}}
  ],
  acec: {
    semantics: [
      {acec_id: 'acec-1', official_acec_name: 'Afton Canyon', source_administrative_unit: 'California Desert District', current_gis_acres: 81579.4, calculated_source_geometry_acres: 81579.4, unit_summary_short: 'Generated QA summary that must not be displayed.'},
      {acec_id: 'acec-2', official_acec_name: 'Second ACEC', source_administrative_unit: 'Needles Field Office', current_gis_acres: 100, calculated_source_geometry_acres: 80},
      {acec_id: 'acec-3', official_acec_name: 'Third ACEC', source_administrative_unit: 'California Desert District'},
      {acec_id: 'acec-4', official_acec_name: 'Fourth ACEC', source_administrative_unit: 'Central Coast Field Office'}
    ],
    components: [
      {component_id: 'g1', acec_id: 'acec-1'},
      {component_id: 'g2', acec_id: 'acec-2'},
      {component_id: 'g3', acec_id: 'acec-3'},
      {component_id: 'g4', acec_id: 'acec-4'}
    ],
    values: [
      {acec_id: 'acec-1', value_family: 'botanical_or_ecological', value_name: 'Natural processes', value_type: 'natural_process', value_description: 'Official statewide source codes natural processes as relevant (YES).', value_source_document: 'BLM CA ACEC FeatureServer', value_source_url: 'https://example.com/acec'},
      {acec_id: 'acec-1', value_family: 'water_aquatic', value_name: 'Fish or aquatic resources', value_type: 'fish_resource', value_description: 'Official statewide source codes fish or aquatic resources as relevant (YES).', value_source_document: 'BLM CA ACEC FeatureServer', value_source_url: 'https://example.com/acec'}
    ], documents: [], management: [], planning: [], relationships: [
      {acec_id: 'acec-1', related_feature_name: 'California Desert District', relationship_type: 'administrative_relationship', relationship_context_class: 'documented_management_relationship'}
    ],
    offices: [{acec_id: 'acec-1', source_admin_unit_code: 'CAD00000', responsible_blm_district: 'California Desert District'}],
    access: [{acec_id: 'acec-1', public_access_status: 'unresolved', public_access_scope: 'Do not infer access from polygon or roads.', non_blm_land_summary: 'No surface-management overlay was supplied or executed.'}],
    current_field_offices: [
      {office_key: 'barstow_field_office', current_official_name: 'Barstow Field Office', official_office_url: 'https://www.blm.gov/office/barstow-field-office'},
      {office_key: 'needles_field_office', current_official_name: 'Needles Field Office', official_office_url: 'https://www.blm.gov/office/needles-field-office'},
      {office_key: 'ridgecrest_field_office', current_official_name: 'Ridgecrest Field Office', official_office_url: 'https://www.blm.gov/office/ridgecrest-field-office'},
      {office_key: 'central_coast_field_office', current_official_name: 'Central Coast Field Office', official_office_url: 'https://www.blm.gov/office/central-coast-field-office'}
    ],
    field_office_context: [
      {acec_id: 'acec-1', current_field_office_key: 'barstow_field_office', current_field_office_name: 'Barstow Field Office', percent_of_acec_area: 70, acec_context_class: 'crosses_field_office_boundaries'},
      {acec_id: 'acec-1', current_field_office_key: 'needles_field_office', current_field_office_name: 'Needles Field Office', percent_of_acec_area: 30, acec_context_class: 'crosses_field_office_boundaries'},
      {acec_id: 'acec-1', current_field_office_key: 'ridgecrest_field_office', current_field_office_name: 'Ridgecrest Field Office', intersection_area_acres: 73.6, percent_of_acec_area: 0.0345, acec_context_class: 'crosses_field_office_boundaries'},
      {acec_id: 'acec-2', current_field_office_key: 'needles_field_office', current_field_office_name: 'Needles Field Office', percent_of_acec_area: 100, acec_context_class: 'wholly_within_one_field_office'},
      {acec_id: 'acec-3', current_field_office_key: 'ridgecrest_field_office', current_field_office_name: 'Ridgecrest Field Office', percent_of_acec_area: 100, acec_context_class: 'wholly_within_one_field_office'},
      {acec_id: 'acec-4', current_field_office_key: 'central_coast_field_office', current_field_office_name: 'Central Coast Field Office', percent_of_acec_area: 100, acec_context_class: 'wholly_within_one_field_office'}
    ],
    overlap_pairs: [
      {acec_id_a: 'acec-1', acec_id_b: 'acec-2', overlap_area_m2: 100},
      {acec_id_a: 'acec-2', acec_id_b: 'acec-3', overlap_area_m2: 50}
    ],
    overlap_style: {
      palette: ['#0072B2', '#E69F00', '#009E73', '#CC79A7', '#56B4E9', '#D55E00'],
      fill_opacity: 0.27, stroke_weight: 2.2, stroke_darken: 0.30,
      minimum_overlap_area_m2: 1
    },
    field_office_presentation: {additional_office_minimum_percent: 1},
    area_presentation: {
      material_difference_minimum_acres: 10,
      material_difference_minimum_percent: 0.5
    },
    caveats: {
      field_office_context: 'Derived from spatial intersection with verified current BLM field-office boundaries; this does not by itself establish administrative responsibility.'
    }
  }
}];
const acecMapRoot = new FakeElement('map');
controller.call(map, acecMapRoot, null, acecPayload);
const acecCard = controls.at(-1).card;
const quickButtons = acecCard.querySelectorAll('[data-pt-lr-quick-view]');
const quickButton = key => quickButtons.find(button =>
  button.getAttribute('data-pt-lr-quick-view') === key
);
const rootLayerIds = () => Array.from(rootMembers)
  .map(layer => layer.options.layerId).sort();
assert.strictEqual(quickButtons.length, 6);
assert.ok(acecCard.innerHTML.includes('Field office context'));
assert.ok(acecCard.innerHTML.includes('Barstow Field Office'));
assert.ok(acecCard.innerHTML.includes('Needles Field Office'));
assert.ok(acecCard.innerHTML.includes('Ridgecrest Field Office'));
assert.ok(acecCard.innerHTML.includes('Central Coast Field Office'));
assert.ok(
  acecCard.innerHTML.indexOf('Barstow Field Office') <
    acecCard.innerHTML.indexOf('Central Coast Field Office') &&
  acecCard.innerHTML.indexOf('Central Coast Field Office') <
    acecCard.innerHTML.indexOf('Needles Field Office') &&
  acecCard.innerHTML.indexOf('Needles Field Office') <
    acecCard.innerHTML.indexOf('Ridgecrest Field Office')
);
assert.ok(!acecCard.innerHTML.includes('Hollister'));
layers.g1.fire('click', {latlng: {lat: 35, lng: -116}});
assert.ok(layers.g1.popupHtml.includes('Source administrative unit'));
assert.ok(layers.g1.popupHtml.includes('California Desert District'));
assert.ok(layers.g1.popupHtml.includes('Field office context'));
assert.ok(layers.g1.popupHtml.includes('Barstow Field Office · Needles Field Office (derived spatially)'));
assert.ok(layers.g1.popupHtml.includes('does not by itself establish administrative responsibility'));
const g1PrimaryPopup = layers.g1.popupHtml.split('<details class="pt-popup-technical">')[0];
const g1TechnicalPopup = layers.g1.popupHtml.split('<details class="pt-popup-technical">')[1];
assert.ok(g1PrimaryPopup.includes('Current BLM source GIS acreage'));
assert.ok(layers.g1.popupHtml.includes('81,579.4 acres (127 mi²)'));
assert.ok(g1PrimaryPopup.includes('Identified as relevant values in the current BLM California ACEC source.'));
assert.ok(g1PrimaryPopup.includes(
  'The statewide BLM ACEC source identifies fish or aquatic resources as a relevant value for this ACEC.'
));
assert.ok(g1PrimaryPopup.includes('Barstow Field Office</a></strong> — 70.0%'));
assert.ok(g1PrimaryPopup.includes('Needles Field Office</a></strong> — 30.0%'));
assert.ok(!g1PrimaryPopup.includes('Ridgecrest Field Office'));
assert.strictEqual((layers.g1.popupHtml.match(/https:\/\/example\.com\/acec/g) || []).length, 1);
assert.ok(!g1PrimaryPopup.includes('(YES)'));
assert.ok(!g1PrimaryPopup.includes('(NO)'));
assert.ok(!g1PrimaryPopup.includes('natural_process'));
assert.ok(!g1PrimaryPopup.includes('administrative_relationship'));
assert.ok(!g1PrimaryPopup.includes('documented_management_relationship'));
assert.ok(!g1PrimaryPopup.includes('Generated QA summary'));
assert.ok(!g1PrimaryPopup.includes('BRIM calculated source-geometry acreage'));
assert.ok(!g1PrimaryPopup.includes('polygon part'));
assert.ok(g1TechnicalPopup.includes('BRIM calculated source-geometry acreage'));
assert.ok(g1TechnicalPopup.includes('Ridgecrest Field Office'));
assert.ok(g1TechnicalPopup.includes('0.0345% of mapped ACEC area'));
assert.ok(g1TechnicalPopup.includes('administrative_relationship'));
assert.ok(g1TechnicalPopup.includes('documented_management_relationship'));
layers.g2.fire('click', {latlng: {lat: 35, lng: -116}});
const g2PrimaryPopup = layers.g2.popupHtml.split('<details class="pt-popup-technical">')[0];
assert.ok(g2PrimaryPopup.includes('Current BLM source GIS acreage'));
assert.ok(g2PrimaryPopup.includes('BRIM calculated source-geometry acreage'));
assert.ok(g2PrimaryPopup.includes('differ by 20.0 acres'));
assert.deepStrictEqual(
  quickButtons.map(button => button.textContent),
  ['Fish / aquatic values', 'DRECP ACECs', 'Wildlife / habitat', 'Cultural / historic', 'Scenic', 'Natural systems']
);
assert.strictEqual(rootMembers.size, 4);
assert.strictEqual(layers.g1.style.fillColor, '#B86F52');
assert.strictEqual(layers.g1.style.color, '#7A3F2E');
assert.strictEqual(layers.g1.style.fillOpacity, 0.12);
assert.strictEqual(layers.g1.style.weight, 1.6);
assert.strictEqual(acecMapRoot.getAttribute('data-pt-lr-acec-value-style-mode'), 'default');
assert.ok(acecCard.innerHTML.indexOf('<summary>Map / display</summary>') >= 0);
assert.ok(
  acecCard.innerHTML.indexOf('<summary>Map / display</summary>') <
    acecCard.innerHTML.indexOf('<summary>Boundary / use note</summary>')
);
assert.ok(!/<details class="pt-lr-map-details pt-acec-map-display"[^>]*\sopen/.test(acecCard.innerHTML));
assert.ok(acecCard.innerHTML.includes(
  'title="Give simultaneously visible overlapping ACECs contrasting colors."'
));
assert.ok(acecCard.innerHTML.includes('> Distinguish overlaps</label>'));
const overlapToggle = acecCard.querySelector('.pt-acec-distinguish-overlaps');
assert.strictEqual(overlapToggle.checked, false);
const countsBeforeOverlapMode = JSON.parse(JSON.stringify(
  window.BRIM.localReferenceController.stats()[0].counts
));
overlapToggle.checked = true;
acecCard.dispatch('change', overlapToggle);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].distinguish_overlaps, true);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].active_overlap_pair_count, 2);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].active_overlap_participant_count, 3);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].overlap_color_conflict_count, 0);
assert.deepStrictEqual(
  window.BRIM.localReferenceController.stats()[0].counts,
  countsBeforeOverlapMode,
  'overlap display mode must not change semantic or geometry counts'
);
assert.notStrictEqual(layers.g1.style.fillColor, layers.g2.style.fillColor);
assert.notStrictEqual(layers.g2.style.fillColor, layers.g3.style.fillColor);
const initialOverlapColors = {
  'acec-1': layers.g1.style.fillColor,
  'acec-2': layers.g2.style.fillColor,
  'acec-3': layers.g3.style.fillColor
};
assert.ok(acecPayload[0].acec.overlap_style.palette.includes(layers.g1.style.fillColor));
assert.ok(acecPayload[0].acec.overlap_style.palette.includes(layers.g2.style.fillColor));
assert.ok(acecPayload[0].acec.overlap_style.palette.includes(layers.g3.style.fillColor));
assert.strictEqual(layers.g4.style.fillColor, '#B86F52');
assert.strictEqual(layers.g1.style.fillOpacity, 0.27);
assert.strictEqual(layers.g1.style.weight, 2.2);
const g2OverlapStyle = Object.assign({}, layers.g2.style);
layers.g2.style = {fillColor: '#FFFFFF', color: '#111111', weight: 4};
layers.g2.fire('mouseout');
assert.deepStrictEqual(layers.g2.style, g2OverlapStyle, 'mouseout must restore overlap style');
window.BRIM.localReferenceController.reset();
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].distinguish_overlaps, false);
assert.strictEqual(layers.g1.style.fillColor, '#B86F52');
assert.strictEqual(layers.g2.style.fillColor, '#B86F52');
assert.strictEqual(overlapToggle.checked, false);
assert.strictEqual(quickButton('cultural_historic').getAttribute('style').includes('#A66A43'), true);
assert.strictEqual(quickButton('natural_systems').getAttribute('style').includes('#4F8C68'), true);
assert.strictEqual(quickButton('scenic').getAttribute('style').includes('#8A6DAA'), true);
assert.strictEqual(quickButton('other_or_unresolved'), undefined);

quickButton('cultural_historic').dispatch('click');
assert.strictEqual(layers.g1.style.fillColor, '#A66A43');
assert.strictEqual(layers.g1.style.color, '#784C30');
assert.strictEqual(quickButton('cultural_historic').getAttribute('aria-pressed'), 'true');
quickButton('natural_systems').dispatch('click');
assert.strictEqual(layers.g1.style.fillColor, '#4F8C68');
assert.strictEqual(layers.g1.style.color, '#39654B');
assert.strictEqual(quickButton('natural_systems').getAttribute('aria-pressed'), 'true');
quickButton('scenic').dispatch('click');
assert.strictEqual(layers.g4.style.fillColor, '#8A6DAA');
assert.strictEqual(layers.g4.style.color, '#634E7A');
assert.strictEqual(quickButton('scenic').getAttribute('aria-pressed'), 'true');

quickButton('fish_aquatic').dispatch('click');
assert.deepStrictEqual(rootLayerIds(), ['g1', 'g3']);
assert.strictEqual(layers.g1.style.fillColor, '#3B82A0');
assert.strictEqual(layers.g1.style.color, '#2A5E73');
assert.strictEqual(layers.g1.style.fillOpacity, 0.22);
assert.strictEqual(layers.g1.style.weight, 2);
assert.strictEqual(quickButton('fish_aquatic').getAttribute('aria-pressed'), 'true');
assert.strictEqual(quickButton('wildlife_habitat').getAttribute('aria-pressed'), 'false');
assert.strictEqual(acecMapRoot.getAttribute('data-pt-lr-acec-value-style-mode'), 'single');
assert.strictEqual(acecMapRoot.getAttribute('data-pt-lr-acec-value-style-key'), 'water_aquatic');
assert.strictEqual(acecMapRoot.getAttribute('data-pt-lr-acec-value-style-color'), '#3B82A0');
assert.ok(acecCard.querySelector('.pt-lr-thematic-state').innerHTML.includes('Fish or aquatic resources'));
layers.g1.style = {fillColor: '#B86F52', color: '#7A3F2E'};
layers.g1.fire('mouseout');
assert.strictEqual(layers.g1.style.fillColor, '#3B82A0');
assert.strictEqual(layers.g1.style.color, '#2A5E73');

overlapToggle.checked = true;
acecCard.dispatch('change', overlapToggle);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].distinguish_overlaps, true);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].active_overlap_pair_count, 0);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].active_overlap_participant_count, 0);
assert.strictEqual(layers.g1.style.fillColor, '#3B82A0');
assert.strictEqual(layers.g3.style.fillColor, '#3B82A0');

quickButton('wildlife_habitat').dispatch('click');
assert.deepStrictEqual(rootLayerIds(), ['g2', 'g3']);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].active_overlap_pair_count, 1);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].active_overlap_participant_count, 2);
assert.notStrictEqual(layers.g2.style.fillColor, layers.g3.style.fillColor);
assert.strictEqual(layers.g2.style.fillColor, initialOverlapColors['acec-2']);
assert.strictEqual(layers.g3.style.fillColor, initialOverlapColors['acec-3']);
assert.ok(acecPayload[0].acec.overlap_style.palette.includes(layers.g2.style.fillColor));
assert.ok(acecPayload[0].acec.overlap_style.palette.includes(layers.g3.style.fillColor));
overlapToggle.checked = false;
acecCard.dispatch('change', overlapToggle);
assert.strictEqual(layers.g2.style.fillColor, '#7A9A4A');
assert.strictEqual(layers.g2.style.color, '#586F35');
assert.strictEqual(quickButton('fish_aquatic').getAttribute('aria-pressed'), 'false');
assert.strictEqual(quickButton('wildlife_habitat').getAttribute('aria-pressed'), 'true');

const fishFacetInput = acecCard.querySelector(
  '[data-pt-lr-facet="relevant_value_family"][data-pt-lr-facet-value="water_aquatic"]'
);
fishFacetInput.checked = true;
acecCard.dispatch('change', fishFacetInput);
assert.deepStrictEqual(rootLayerIds(), ['g1', 'g2', 'g3']);
assert.strictEqual(layers.g1.style.fillColor, '#B86F52');
assert.strictEqual(layers.g1.style.color, '#7A3F2E');
assert.strictEqual(quickButton('fish_aquatic').getAttribute('aria-pressed'), 'false');
assert.strictEqual(quickButton('wildlife_habitat').getAttribute('aria-pressed'), 'false');
assert.strictEqual(acecMapRoot.getAttribute('data-pt-lr-acec-value-style-mode'), 'multiple');
assert.ok(acecCard.querySelector('.pt-lr-thematic-state').innerHTML.includes('neutral (multiple values selected)'));
overlapToggle.checked = true;
acecCard.dispatch('change', overlapToggle);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].active_overlap_pair_count, 2);
assert.notStrictEqual(layers.g1.style.fillColor, '#B86F52');
assert.notStrictEqual(layers.g2.style.fillColor, '#B86F52');
overlapToggle.checked = false;
acecCard.dispatch('change', overlapToggle);
assert.strictEqual(layers.g1.style.fillColor, '#B86F52');
assert.strictEqual(layers.g2.style.fillColor, '#B86F52');
overlapToggle.checked = true;
acecCard.dispatch('change', overlapToggle);
acecValues.forEach(value => {
  const input = acecCard.querySelector(
    '[data-pt-lr-facet="relevant_value_family"]' +
    '[data-pt-lr-facet-value="' + value.value_key + '"]'
  );
  input.checked = false;
  acecCard.dispatch('change', input);
});
assert.strictEqual(rootMembers.size, 0);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].distinguish_overlaps, true);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].active_overlap_pair_count, 0);
assert.strictEqual(
  window.BRIM.localReferenceController.stats()[0].counts.currently_showing.semantic_feature_count,
  0
);

acecCard.querySelector('.pt-lr-reset').dispatch('click');
assert.strictEqual(rootMembers.size, 4);
assert.strictEqual(layers.g1.style.fillColor, '#B86F52');
assert.strictEqual(acecMapRoot.getAttribute('data-pt-lr-acec-value-style-mode'), 'default');
overlapToggle.checked = true;
acecCard.dispatch('change', overlapToggle);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].active_overlap_pair_count, 2);
quickButton('drecp').dispatch('click');
assert.deepStrictEqual(rootLayerIds(), ['g1', 'g4']);
assert.strictEqual(layers.g1.style.fillColor, '#B86F52');
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].distinguish_overlaps, true);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].active_overlap_pair_count, 0);
assert.strictEqual(quickButton('drecp').getAttribute('aria-pressed'), 'true');
assert.strictEqual(acecMapRoot.getAttribute('data-pt-lr-acec-value-style-mode'), 'default');

const acecAuto = acecCard.querySelector('.pt-lr-auto');
acecAuto.checked = false;
acecCard.dispatch('change', acecAuto);
quickButton('scenic').dispatch('click');
assert.deepStrictEqual(rootLayerIds(), ['g1', 'g4']);
assert.strictEqual(layers.g4.style.fillColor, '#B86F52');
assert.strictEqual(quickButton('scenic').getAttribute('aria-pressed'), 'true');
assert.strictEqual(acecMapRoot.getAttribute('data-pt-lr-acec-value-style-mode'), 'default');
acecCard.querySelector('.pt-lr-apply').dispatch('click');
assert.deepStrictEqual(rootLayerIds(), ['g4']);
assert.strictEqual(layers.g4.style.fillColor, '#8A6DAA');
assert.strictEqual(layers.g4.style.color, '#634E7A');
assert.strictEqual(acecMapRoot.getAttribute('data-pt-lr-acec-value-style-mode'), 'single');
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].distinguish_overlaps, true);
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].active_overlap_pair_count, 0);

map.rootActive = false;
map.fire('overlayremove', {name: 'Reference – ACECs'});
assert.strictEqual(rootMembers.size, 0);
assert.strictEqual(layers.g4.style.fillColor, '#B86F52');
assert.strictEqual(acecMapRoot.getAttribute('data-pt-lr-acec-value-style-mode'), 'default');
assert.strictEqual(acecMapRoot.getAttribute('data-pt-lr-acec-distinguish-overlaps'), 'false');
map.rootActive = true;
map.fire('overlayadd', {name: 'Reference – ACECs'});
assert.strictEqual(rootMembers.size, 4);
assert.strictEqual(layers.g4.style.fillColor, '#B86F52');
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].value_style_mode, 'default');
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].distinguish_overlaps, false);
const acecCardAfterReadd = controls.at(-1).card;
assert.strictEqual((acecCardAfterReadd.listeners.change || []).length, 1);
assert.strictEqual(
  acecCardAfterReadd.querySelector('.pt-acec-distinguish-overlaps').checked,
  false
);
window.BRIM.localReferenceController.destroy();
assert.strictEqual(rootMembers.size, 0);

// California Desert NCL uses the shared lifecycle/filter engine plus a compact
// class-specific card and a normalized three-tab popup assembled only on click.
rootMembers.clear();
labelRootMembers.clear();
[layers.g1, layers.g2].forEach(layer => rootMembers.add(layer));
[labelLayers.l1, labelLayers.l2].forEach(layer => labelRootMembers.add(layer));
map.rootActive = true;
map.labelRootActive = true;
map.currentZoom = 8;
map.layerManager = {
  _byGroup: {
    'Reference – CA Desert National Conservation Lands': {g1: layers.g1, g2: layers.g2},
    'Labels – CA Desert National Conservation Lands': {l1: labelLayers.l1, l2: labelLayers.l2}
  },
  _groupContainers: {
    'Reference – CA Desert National Conservation Lands': groupRoot,
    'Labels – CA Desert National Conservation Lands': labelGroupRoot
  }
};
const desertPayload = [{
  layer_id: 'ca_desert_ncl',
  display_name: 'CA Desert National Conservation Lands',
  group_name: 'Reference – CA Desert National Conservation Lands',
  auto_supported: true,
  auto_default: true,
  feature_selection_supported: true,
  feature_selection_mode: 'semantic_feature_multi',
  feature_display_field: 'pt_cdncl_display_name',
  auto_zoom_supported: true,
  auto_zoom_default: true,
  zoom_padding: 36,
  zoom_max: 10,
  preserve_view_on_reset: true,
  popup_layout: 'tabbed_card',
  primary_count_mode: 'semantic_feature',
  primary_count_label: 'mapped units',
  show_component_count: false,
  show_category_count: false,
  category_filter_visible: false,
  category_count_mode: 'semantic_feature',
  distinguish_units_supported: true,
  component_count_label: 'source components',
  category_heading: '',
  dashboard_summary: '11 mapped units · 10 DRECP subareas + Desert Lily Preserve',
  caution: 'Broad mapped units do not establish ownership or public access.',
  categories: [{
    category_key: 'ca_desert_ncl', label: 'California Desert NCL mapped unit',
    fill_color: '#B89C6A', stroke_color: '#6F5632', fill_opacity: 0.10,
    stroke_weight: 1.7, dash_array: '', legend_swatch_style: 'polygon'
  }],
  facets: [{
    facet_key: 'field_office_context', label: 'BLM Field Office context',
    count_mode: 'semantic_feature', collapsible: false, open_default: true,
    layout_columns: 2, context_cue: 'spatial context only',
    context_title: 'Spatial intersection context; not a responsible-office or management assignment.',
    values: [
      {value_key: 'cad08000', label: 'Barstow Field Office', sort_order: 1},
      {value_key: 'cad06000', label: 'Palm Springs/S. Coast Field Office', sort_order: 2}
    ]
  }, {
    facet_key: 'related_designation_overlap', label: 'Related designation overlap',
    count_mode: 'semantic_feature', collapsible: false, open_default: true,
    layout_columns: 2, context_cue: 'spatial context only',
    context_title: 'Current spatial overlap context; it does not transfer a related designation.',
    values: [
      {value_key: 'acec', label: 'ACEC', sort_order: 1},
      {value_key: 'federal_wilderness', label: 'Federal Wilderness', sort_order: 2},
      {value_key: 'national_monuments', label: 'National Monument', sort_order: 3},
      {value_key: 'wilderness_study_areas', label: 'WSA', sort_order: 4},
      {value_key: 'national_trails', label: 'Scenic/Historic Trail', sort_order: 5},
      {value_key: 'wild_scenic_river', label: 'Wild & Scenic River', sort_order: 6}
    ]
  }],
  quick_views: [],
  features: [
    {semantic_feature_key: 'NLCS002009', feature_key: 'NLCS002009', display_name: 'Basin and Range', category_keys: ['ca_desert_ncl'], search_text: 'basin and range nlcs002009 barstow', semantic_feature_bounds: [34, -118, 36, -116], geometry_component_count: 21},
    {semantic_feature_key: 'NLCS002012', feature_key: 'NLCS002012', display_name: 'Desert Lily Preserve', category_keys: ['ca_desert_ncl'], search_text: 'desert lily preserve nlcs002012 palm springs', semantic_feature_bounds: [32, -116, 33, -115], geometry_component_count: 1}
  ],
  records: [
    {geometry_key: 'g1', semantic_feature_key: 'NLCS002009', category_key: 'ca_desert_ncl', geometry_component_count: 21, facet_values: {field_office_context: ['cad08000'], related_designation_overlap: ['national_monuments', 'national_trails']}},
    {geometry_key: 'g2', semantic_feature_key: 'NLCS002012', category_key: 'ca_desert_ncl', geometry_component_count: 1, facet_values: {field_office_context: ['cad06000'], related_designation_overlap: ['acec']}}
  ],
  semantic_labels: {
    available: true, label_id: 'cadesert_ncl',
    label_group: 'Labels – CA Desert National Conservation Lands',
    anchor_strategy: 'polygon_semantic_point_on_surface',
    visible_component_aware: false, min_zoom: 7, max_zoom: null,
    semantic_feature_count: 2, anchor_count: 2,
    records: [
      {label_record_key: 'synthetic::f1::g1', semantic_feature_key: 'NLCS002009', geometry_key: 'g1', label_text: 'Basin and Range', anchor_priority: 1, lng: -117, lat: 35},
      {label_record_key: 'synthetic::f2::g2', semantic_feature_key: 'NLCS002012', geometry_key: 'g2', label_text: 'Desert Lily Preserve', anchor_priority: 1, lng: -115.5, lat: 32.5}
    ]
  },
  desert_ncl: {
    semantics: [
      {nlcs_id: 'NLCS002009', standardized_display_name: 'Basin and Range', mapped_unit_interpretation: 'A DRECP mapped geographic and planning subarea.', planning_context: 'DRECP allocation context.', geographic_description: 'Northern California Desert context.', directly_supported_conservation_values: 'Landscape connectivity.', directly_supported_management_objectives: 'Conserve applicable BLM-administered lands and interests.', blm_role_summary: 'Program authority applies within scope.', boundary_caveat: 'Not an ownership or access boundary.', access_and_route_caveat: 'Access must be verified.', land_status_caveat: 'Land status varies.', source_limitations: 'Broad planning geometry.'},
      {nlcs_id: 'NLCS002012', standardized_display_name: 'Desert Lily Preserve', mapped_unit_interpretation: 'A distinct BLM source-layer record requiring identity caution.', planning_context: 'California Desert NCL source context.', geographic_description: 'Imperial County desert context.', directly_supported_conservation_values: 'Desert lily resources.', directly_supported_management_objectives: 'Consult governing authorities.', blm_role_summary: 'Do not infer management from publication.', boundary_caveat: 'Not proven identical to the ACEC or statutory Sanctuary.', access_and_route_caveat: 'Access is not established.', land_status_caveat: 'Underlying status varies.', source_limitations: 'Identity remains qualified.'}
    ],
    components: [
      {component_id: 'g1', nlcs_id: 'NLCS002009', pt_cdncl_unit_type_label: 'DRECP mapped subarea', pt_cdncl_raw_name: 'Basin and Range', pt_cdncl_calculated_raw_area_acres: 640000, pt_cdncl_official_reported_acres: 620000, pt_cdncl_global_id: '{GLOBAL-BASIN}', pt_cdncl_source_objectid: 1, pt_cdncl_last_verified: '2026-08-10'},
      {component_id: 'g2', nlcs_id: 'NLCS002012', pt_cdncl_unit_type_label: 'Desert Lily source-layer record', pt_cdncl_raw_name: 'Desert Lily Preserve', pt_cdncl_calculated_raw_area_acres: 2300, pt_cdncl_official_reported_acres: '', pt_cdncl_global_id: '{GLOBAL-LILY}', pt_cdncl_source_objectid: 4, pt_cdncl_last_verified: '2026-08-10'}
    ],
    aliases: [{nlcs_id: 'NLCS002012', alias: 'Desert Lily NCL source record'}],
    policy: [{policy_id: 'blm_authority_scope', recommended_language: 'The broad polygon does not establish BLM ownership or management of every parcel.'}],
    documents: [{document_id: 'doc-1', exact_title: 'DRECP Record of Decision', direct_document_url: 'https://example.test/drecp'}],
    unit_documents: [{nlcs_id: 'NLCS002012', document_id: 'doc-1'}],
    offices: [{office_key: 'cad06000', office_name: 'Palm Springs/S. Coast Field Office'}],
    field_office_context: [{nlcs_id: 'NLCS002012', office_key: 'cad06000', percent_of_unit_area: 99.7, display_context: true}],
    related_context: [
      {nlcs_id: 'NLCS002009', related_layer_key: 'national_monuments', related_layer_label: 'National Monuments', related_feature_name: 'Chuckwalla National Monument', normal_popup_suitable: true},
      {nlcs_id: 'NLCS002012', related_layer_key: 'acec', related_layer_label: 'Areas of Critical Environmental Concern', related_feature_name: 'Desert Lily Preserve ACEC', normal_popup_suitable: true}
    ],
    research_relationships: [{nlcs_id: 'NLCS002012', related_brim_layer_family: 'Desert Lily Sanctuary', related_feature_name: 'Desert Lily Sanctuary statutory evidence', evidence_url: 'https://example.test/sanctuary'}],
    sources: [{source_title: 'BLM California Desert NCL FeatureServer', authority_level: 'official primary GIS', url: 'https://example.test/featureserver'}],
    caveats: {
      office: 'Spatial context does not establish administrative responsibility.',
      relationships: 'Spatial overlap does not establish legal identity or management.',
      desert_lily: 'The NCL record, Desert Lily Preserve ACEC, and statutory Desert Lily Sanctuary are not assumed identical.'
    }
  }
}];
const desertMapRoot = new FakeElement('map');
controller.call(map, desertMapRoot, null, desertPayload);
const desertCard = controls.at(-1).card;
assert.strictEqual(desertCard.getAttribute('data-pt-local-reference-layer'), 'ca_desert_ncl');
assert.ok(!desertCard.innerHTML.includes('Mapped-unit type'));
assert.ok(desertCard.innerHTML.includes('BLM Field Office context'));
assert.ok(desertCard.innerHTML.includes('Related designation overlap'));
assert.ok(desertCard.innerHTML.includes('11 mapped units · 10 DRECP subareas + Desert Lily Preserve'));
assert.ok(desertCard.innerHTML.includes('Distinguish mapped units'));
assert.ok(!desertCard.innerHTML.includes('off by default'));
assert.ok(desertCard.innerHTML.includes('spatial context only'));
assert.strictEqual((desertCard.innerHTML.match(/pt-lr-facet-two-column/g) || []).length, 2);
assert.strictEqual((desertCard.innerHTML.match(/pt-lr-facet-collapsible/g) || []).length, 0);
assert.strictEqual((desertCard.innerHTML.match(/<details class="pt-lr-map-details/g) || []).length, 1);
assert.ok(desertCard.innerHTML.includes('<summary>Boundary / use note</summary>'));
assert.ok(
  desertCard.innerHTML.indexOf('class="pt-lr-search"') <
    desertCard.innerHTML.indexOf('class="pt-lr-auto"') &&
  desertCard.innerHTML.indexOf('class="pt-lr-auto"') <
    desertCard.innerHTML.indexOf('Distinguish mapped units') &&
  desertCard.innerHTML.indexOf('Distinguish mapped units') <
    desertCard.innerHTML.indexOf('class="pt-lr-dashboard-summary"') &&
  desertCard.innerHTML.indexOf('class="pt-lr-dashboard-summary"') <
    desertCard.innerHTML.indexOf('BLM Field Office context') &&
  desertCard.innerHTML.indexOf('BLM Field Office context') <
    desertCard.innerHTML.indexOf('Related designation overlap') &&
  desertCard.innerHTML.indexOf('Related designation overlap') <
    desertCard.innerHTML.indexOf('class="pt-lr-actions"')
);
assert.ok(!desertCard.innerHTML.includes('Monument overlap'));
assert.strictEqual(desertCard.querySelectorAll('[data-pt-lr-quick-view]').length, 0);
assert.ok(!desertCard.innerHTML.includes('data-pt-lr-category='));
assert.strictEqual(desertCard.querySelector('.pt-lr-distinguish').checked, false);
assert.strictEqual(
  desertCard.querySelector('[data-pt-lr-facet-count="related_designation_overlap"][data-pt-lr-facet-count-value="national_monuments"]').textContent,
  '1'
);
assert.strictEqual(
  desertCard.querySelector('[data-pt-lr-facet-count="related_designation_overlap"][data-pt-lr-facet-count-value="federal_wilderness"]').textContent,
  '0'
);

const setDesertFacet = (facetKey, allValues, selectedValues) => {
  allValues.forEach(valueKey => {
    const input = desertCard.querySelector(
      '[data-pt-lr-facet="' + facetKey + '"]' +
      '[data-pt-lr-facet-value="' + valueKey + '"]'
    );
    input.checked = selectedValues.includes(valueKey);
    desertCard.dispatch('change', input);
  });
};
const relatedFacetValues = [
  'acec', 'federal_wilderness', 'national_monuments',
  'wilderness_study_areas', 'national_trails', 'wild_scenic_river'
];
setDesertFacet('related_designation_overlap', relatedFacetValues, ['national_monuments']);
assert.deepStrictEqual(Array.from(rootMembers), [layers.g1]);
assert.deepStrictEqual(Array.from(labelRootMembers), [labelLayers.l1]);
setDesertFacet(
  'related_designation_overlap', relatedFacetValues,
  ['national_monuments', 'acec']
);
assert.strictEqual(rootMembers.size, 2);
assert.strictEqual(labelRootMembers.size, 2);
setDesertFacet('field_office_context', ['cad08000', 'cad06000'], ['cad06000']);
assert.deepStrictEqual(Array.from(rootMembers), [layers.g2]);
assert.deepStrictEqual(Array.from(labelRootMembers), [labelLayers.l2]);
desertCard.querySelector('.pt-lr-reset').dispatch('click');
assert.strictEqual(rootMembers.size, 2);
assert.strictEqual(labelRootMembers.size, 2);

const desertSearch = desertCard.querySelector('.pt-lr-search');
desertSearch.value = 'Desert Lily';
desertSearch.dispatch('input');
desertSearch.dispatch('keydown', desertSearch, {key: 'Enter'});
assert.deepStrictEqual(Array.from(rootMembers), [layers.g2]);
assert.deepStrictEqual(Array.from(labelRootMembers), [labelLayers.l2]);
desertCard.querySelector('.pt-lr-reset').dispatch('click');
assert.strictEqual(rootMembers.size, 2);
layers.g2.fire('click', {latlng: {lat: 32.5, lng: -115.5}});
assert.strictEqual((layers.g2.popupHtml.match(/role="tab"/g) || []).length, 3);
assert.ok(layers.g2.popupHtml.includes('Conservation &amp; planning'));
assert.ok(layers.g2.popupHtml.includes('Related designations &amp; sources'));
assert.ok(layers.g2.popupHtml.includes('Identity caution:'));
assert.ok(layers.g2.popupHtml.includes('Desert Lily Preserve ACEC'));
assert.ok(layers.g2.popupHtml.includes('pt-cdncl-related-group'));
assert.ok(layers.g2.popupHtml.includes('Areas of Critical Environmental Concern</strong> — 1 related mapped feature'));
assert.ok(!layers.g2.popupHtml.includes('<details class="pt-cdncl-related-group" open'));
assert.ok(layers.g2.popupHtml.includes('99.7% of mapped unit area'));
assert.ok(layers.g2.popupHtml.includes('Spatial overlap does not establish legal identity or management.'));
const desertPrimaryPopup = layers.g2.popupHtml.split('<details class="pt-popup-technical">')[0];
const desertTechnicalPopup = layers.g2.popupHtml.split('<details class="pt-popup-technical">')[1];
assert.ok(!desertPrimaryPopup.includes('GLOBAL-LILY'));
assert.ok(!desertPrimaryPopup.includes('Source OBJECTID'));
assert.ok(desertTechnicalPopup.includes('GLOBAL-LILY'));
assert.ok(desertTechnicalPopup.includes('Source OBJECTID (diagnostic only)'));
layers.g1.fire('click', {latlng: {lat: 35, lng: -117}});
assert.ok(layers.g1.popupHtml.includes('National Monuments</strong> — 1 related mapped feature'));
assert.ok(layers.g1.popupHtml.includes('Chuckwalla National Monument'));
const desertDistinguish = desertCard.querySelector('.pt-lr-distinguish');
desertDistinguish.checked = true;
desertCard.dispatch('change', desertDistinguish);
assert.notStrictEqual(layers.g1.style.fillColor, layers.g2.style.fillColor);
desertCard.querySelector('.pt-lr-reset').dispatch('click');
assert.strictEqual(window.BRIM.localReferenceController.stats()[0].distinguish_units, false);
assert.strictEqual(layers.g1.style.fillColor, '#B89C6A');
map.rootActive = false;
map.fire('overlayremove', {name: 'Reference – CA Desert National Conservation Lands'});
map.fire('overlayremove', {name: 'Reference – CA Desert National Conservation Lands'});
assert.strictEqual(rootMembers.size, 0);
assert.strictEqual(labelRootMembers.size, 0);
map.rootActive = true;
map.fire('overlayadd', {name: 'Reference – CA Desert National Conservation Lands'});
assert.strictEqual(rootMembers.size, 2);
assert.strictEqual(controls.at(-1).card.querySelector('.pt-lr-distinguish').checked, false);
assert.strictEqual((controls.at(-1).card.listeners.change || []).length, 1);
window.BRIM.localReferenceController.destroy();
assert.strictEqual(rootMembers.size, 0);
assert.strictEqual(labelRootMembers.size, 0);

console.log('Local Reference synthetic controller selection/zoom/lifecycle tests passed.');
