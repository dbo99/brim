#!/usr/bin/env node

const fs = require('fs');

function assert(condition, message) {
  if (!condition) throw new Error(message);
}

function extractFunction(source, name) {
  const start = source.indexOf(`function ${name}(`);
  assert(start >= 0, `Function ${name} is missing.`);
  const bodyStart = source.indexOf('{', start);
  let depth = 0;
  for (let i = bodyStart; i < source.length; i += 1) {
    if (source[i] === '{') depth += 1;
    if (source[i] === '}') depth -= 1;
    if (depth === 0) return source.slice(start, i + 1);
  }
  throw new Error(`Could not extract function ${name}.`);
}

const externalPath = '03_functions/js/leaflet_tools_adddata_panel.js';
const uicPath = '03_functions/js/brim_uic_explorer.js';
const external = fs.readFileSync(externalPath, 'utf8');
const uic = fs.readFileSync(uicPath, 'utf8');

// htmlwidgets onRender files are anonymous function expressions, not programs.
new Function(`return (${external});`);
new Function(`return (${uic});`);

const adapters = [
  'drought_monitor', 'cpc_outlook', 'stream_gauge_flow',
  'wcr_completed_depth', 'fire_year', 'mlrs_mineral_cases',
  'sgma_prioritization', 'nifc_current_fire', 'aml_status',
  'calipc_ramp', 'swrcb_ir_status', 'subsidence_observation',
  'dwr_tre_insar_points', 'generic_categorical', 'alert_camera',
  'alert_camera_viewshed'
];

assert(external.includes('function ptRegisteredLegendHtml(options, context)'), 'Central legend dispatch is missing.');
assert(external.includes("ptRegisteredLegendHtml(rec, 'active_layer')"), 'Active-layer details do not use central legend dispatch.');
assert(external.includes("ptRegisteredLegendHtml(styleInfoOptions, 'catalog_detail')"), 'Catalog details do not use central legend dispatch.');
assert(external.includes("catalogExtId: ptCatalogField(rec, 'external_layer_id')"), 'Catalog detail dispatch lacks stable catalog identity.');
for (const adapter of adapters) {
  assert(new RegExp(`\\b${adapter}: function\\(`).test(external), `Renderer dispatch is missing adapter ${adapter}.`);
}
assert(uic.includes('function sourceRowsHtml()'), 'UIC dynamic source-card renderer is missing.');
assert(external.includes('generic_categorical: function() { return ptGenericCategoricalLegendHtml(options); }'), 'Generic categorical style-note dispatch was removed.');
assert(external.includes('Provider legend — opens external page'), 'Provider link label is ambiguous.');
assert(external.includes('Source page — opens external page'), 'Catalog source-page link label is ambiguous.');

const adapterKeysFactory = new Function(
  'ptCleanText', 'ptLegacyLegendAdapterKeys',
  `${extractFunction(external, 'ptLegendAdapterKeys')}; return ptLegendAdapterKeys;`
);
let fallbackCalls = 0;
const adapterKeys = adapterKeysFactory(
  value => String(value || '').trim(),
  () => { fallbackCalls += 1; return ['legacy']; }
);
assert(JSON.stringify(adapterKeys({catalogExtId: 'EXT001', legendAdapter: 'cpc_outlook'})) === '["cpc_outlook"]', 'Catalog row did not use its build-resolved adapter.');
assert(adapterKeys({catalogExtId: 'EXT001'}).length === 0 && fallbackCalls === 0, 'Catalog row fell through to independent legacy predicates.');
assert(adapterKeys({layerName: 'manual'}).join(',') === 'legacy' && fallbackCalls === 1, 'Manual/noncatalog legacy fallback was removed.');

const popupContract = new Function(
  `${extractFunction(external, 'ptHasValue')};` +
  `${extractFunction(external, 'ptIsExternalNoiseField')};` +
  `${extractFunction(external, 'ptExternalAttributeKeys')};` +
  `${extractFunction(external, 'ptHasMeaningfulPopupProperties')};` +
  'return ptHasMeaningfulPopupProperties;'
)();
assert(!popupContract({}), 'Empty feature properties were treated as meaningful.');
assert(!popupContract({OBJECTID: 1, Shape_Area: 10, GlobalID: 'x'}), 'System-only properties were treated as meaningful.');
assert(popupContract({OBJECTID: 1, NAME: 'District'}), 'A meaningful returned property was rejected.');
assert(/if \(clickable\)[\s\S]{0,500}ptPopupFromProperties\(props, options\)/.test(external), 'Generic popup runtime behavior is no longer available independently of capability classification.');
const badgeContract = extractFunction(external, 'ptCatalogLayerCapabilityBadgesHtml');
assert(!badgeContract.includes('ptLegendAdapterKeys') && !badgeContract.includes('ptHasMeaningfulPopupProperties'), 'Visible badges reclassify capability metadata in the browser.');

console.log('Layer capability adapter contract tests passed.');
