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
    this.children = [];
    this.style = {display: '', setProperty: function(name, value) { this[name] = value; }};
    this.attributes = Object.create(null);
    this.value = '';
    this.checked = false;
    this.disabled = false;
    this.textContent = '';
    this.innerHTML = '';
    this.hidden = false;
    this.parentNode = null;
  }
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
      this.childrenBySelector[selector] = child;
    }
    return this.childrenBySelector[selector];
  }
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
const rootMembers = new Set(Object.values(layers));
const directMembers = new Set();
const groupRoot = {
  hasLayer: layer => rootMembers.has(layer),
  addLayer: layer => rootMembers.add(layer),
  removeLayer: layer => rootMembers.delete(layer),
  clearLayers: () => rootMembers.clear(),
  getLayers: () => Array.from(rootMembers)
};
const mapListeners = Object.create(null);
const map = {
  rootActive: true,
  fitBoundsCalls: [],
  closePopupCalls: 0,
  layerManager: {
    _byGroup: {'Reference – Synthetic': layers},
    _groupContainers: {'Reference – Synthetic': groupRoot}
  },
  hasLayer: function(layer) {
    if (layer === groupRoot) return this.rootActive;
    return directMembers.has(layer) || (this.rootActive && rootMembers.has(layer));
  },
  removeLayer: function(layer) {
    if (layer === groupRoot) this.rootActive = false;
    else directMembers.delete(layer);
    return this;
  },
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
    {semantic_feature_key: 'f3', feature_key: 'f3', display_name: 'Gamma Mountain', category_keys: ['b'], search_text: 'gamma mountain nlcs-003', semantic_feature_bounds: [30, 30, 32, 34], geometry_component_count: 5}
  ],
  records: [
    {geometry_key: 'g1', feature_key: 'f1', semantic_feature_key: 'f1', feature_display_name: 'Alpha Ridge', category_key: 'a', search_text: 'alpha ridge', semantic_feature_bounds: [10, 10, 11, 11], geometry_component_count: 2},
    {geometry_key: 'g2', feature_key: 'f2', semantic_feature_key: 'f2', feature_display_name: 'Beta Canyon', category_key: 'a', search_text: 'beta canyon', semantic_feature_bounds: [20, 20, 20.02, 20.02], geometry_component_count: 1},
    {geometry_key: 'g3', feature_key: 'f3', semantic_feature_key: 'f3', feature_display_name: 'Gamma Mountain', category_key: 'b', search_text: 'gamma mountain', semantic_feature_bounds: [30, 30, 32, 34], geometry_component_count: 3},
    {geometry_key: 'g4', feature_key: 'f3', semantic_feature_key: 'f3', feature_display_name: 'Gamma Mountain', category_key: 'b', search_text: 'gamma mountain', semantic_feature_bounds: [30, 30, 32, 34], geometry_component_count: 2}
  ]
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
search.value = 'nlcs-002';
search.dispatch('input');
search.dispatch('keydown', search, {key: 'Enter'});
stats = window.BRIM.localReferenceController.stats()[0];
assert.deepStrictEqual(stats.applied_feature_keys, ['f2']);
assert.deepStrictEqual(Array.from(rootMembers), [layers.g2]);
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
assert.strictEqual(map.fitBoundsCalls.length, beforeStagedSelection);
card.querySelector('.pt-lr-apply').dispatch('click');
assert.deepStrictEqual(Array.from(rootMembers), [layers.g1, layers.g2]);
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
assert.strictEqual(card.querySelector('.pt-lr-zoom-results').disabled, true);
assert.strictEqual(map.fitBoundsCalls.length, beforeNoZoomActions);
card.querySelector('.pt-lr-reset').dispatch('click');
assert.strictEqual(rootMembers.size, 4);
assert.strictEqual(map.fitBoundsCalls.length, beforeNoZoomActions);
assert.strictEqual(search.value, '');
assert.strictEqual(chips.innerHTML, '');
card.querySelector('.pt-lr-all').dispatch('click');
assert.strictEqual(map.fitBoundsCalls.length, beforeNoZoomActions);

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
stats = window.BRIM.localReferenceController.stats()[0];
assert.strictEqual(stats.active, true);
assert.strictEqual(stats.group_root_attached, true);
assert.strictEqual(stats.group_member_layer_count, 4);
assert.strictEqual(stats.attached_owned_layer_count, 4);
assert.strictEqual(stats.card_count, 1);
assert.deepStrictEqual(stats.draft_feature_keys, []);
assert.strictEqual(mapRoot.getAttribute('data-pt-lr-synthetic-semantic-count'), '3');
assert.strictEqual(mapRoot.getAttribute('data-pt-lr-synthetic-component-count'), '8');
assert.strictEqual(controls.length, 1);

// Ten full off/on cycles remain exact, default, and duplicate-free.
for (let cycle = 0; cycle < 10; cycle += 1) {
  map.rootActive = false;
  map.fire('overlayremove', {name: 'Reference – Synthetic'});
  stats = window.BRIM.localReferenceController.stats()[0];
  assert.strictEqual(stats.group_member_layer_count, 0, 'off cycle ' + cycle);
  assert.strictEqual(stats.attached_owned_layer_count, 0, 'off cycle ' + cycle);
  assert.strictEqual(stats.card_count, 0, 'off cycle ' + cycle);
  map.rootActive = true;
  map.fire('overlayadd', {name: 'Reference – Synthetic'});
  stats = window.BRIM.localReferenceController.stats()[0];
  assert.strictEqual(stats.group_member_layer_count, 4, 'on cycle ' + cycle);
  assert.strictEqual(stats.attached_owned_layer_count, 4, 'on cycle ' + cycle);
  assert.strictEqual(stats.card_count, 1, 'on cycle ' + cycle);
  assert.deepStrictEqual(stats.draft_feature_keys, [], 'default chips cycle ' + cycle);
}

const tooltipCloseCountBeforePopup = layers.g1.closeTooltipCalls;
map.fire('popupopen', {popup: {_source: layers.g1}});
assert.strictEqual(layers.g1.closeTooltipCalls, tooltipCloseCountBeforePopup + 1);

assert.ok(!/zoomstart|zoomend|movestart|moveend/.test(controllerSource));
assert.ok(controllerSource.includes("action === 'typing'"));
assert.ok(controllerSource.includes("action === 'none'"));
assert.ok(controllerSource.includes("action === 'reset'"));
assert.ok(controllerSource.includes("action === 'all'"));
assert.ok(controllerSource.includes('.pt-lr-chip{display:inline-flex;max-width:100%;min-width:0'));
assert.ok(controllerSource.includes('.pt-local-reference-card{box-sizing:border-box;width:330px'));
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
assert.strictEqual(map.rootActive, false);
assert.strictEqual(mapRoot.getAttribute('data-pt-lr-synthetic-active'), null);
assert.strictEqual((mapListeners.overlayadd || []).length, 0);
assert.strictEqual((mapListeners.overlayremove || []).length, 0);
assert.strictEqual((mapListeners.popupopen || []).length, 0);

// An initially inactive source group is normalized to the same zero-owned
// invariant before any layer-on event.
Object.values(layers).forEach(layer => rootMembers.add(layer));
map.rootActive = false;
const inactiveMapRoot = new FakeElement('map');
controller.call(map, inactiveMapRoot, null, payload);
stats = window.BRIM.localReferenceController.stats()[0];
assert.strictEqual(rootMembers.size, 0);
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
window.BRIM.localReferenceController.destroy();
assert.strictEqual((layers.g1.listeners.click || []).length, 0);

console.log('Local Reference synthetic controller selection/zoom/lifecycle tests passed.');
