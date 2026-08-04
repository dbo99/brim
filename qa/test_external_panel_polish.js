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

const path = '03_functions/js/leaflet_tools_adddata_panel.js';
const source = fs.readFileSync(path, 'utf8');
new Function(`return (${source});`);

const cleanText = value => String(value == null ? '' : value).trim();
const escapeHtml = value => cleanText(value).replace(/[&<>"']/g, character => ({
  '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'
})[character]);

// Expanded catalog information: reference links are last, ordered, optional,
// escaped, and source/service duplicates remain suppressed.
const detailHtml = new Function(
  'ptCatalogField', 'ptDeriveLegendUrl', 'ptServiceTypeLabel', 'ptEscapeHtml',
  'ptExternalFriendlyLoadNote', 'ptExternalFriendlyLoadBasis',
  'ptExternalLoadBadgeOverrideNote', 'ptExternalLoadBadgeInlineHtml',
  'ptLoadModeHumanLabel', 'ptRegisteredLegendHtml', 'ptNormalizeUrl',
  'ptShortUrl',
  `${extractFunction(source, 'ptCatalogDetailHtml')}; return ptCatalogDetailHtml;`
)(
  (record, field) => cleanText(record && record[field]),
  () => '',
  type => type || 'Unknown',
  escapeHtml,
  () => '',
  () => '',
  () => '',
  () => '',
  mode => mode,
  () => '',
  value => cleanText(value).replace(/\/+$/, ''),
  () => 'short service URL'
);

const detailRecord = {
  service_type: 'map',
  service_url: 'https://example.test/MapServer/2?a=1&b=2',
  source_page: 'https://example.test/source?a=1&b=2',
  legend_url: 'https://example.test/legend?a=1&b=2',
  legend_note: 'Source colors are approximated for map display.',
  notes: 'Bottom links follow this note.'
};
const detail = detailHtml(detailRecord);
const providerIndex = detail.indexOf('Provider legend — opens external page');
const sourceIndex = detail.indexOf('Source page — opens external page');
const serviceIndex = detail.indexOf('<b>Service URL</b> —');
assert(providerIndex > detail.indexOf('Bottom links follow this note.'), 'Provider legend did not move below catalog notes.');
assert(providerIndex >= 0 && providerIndex < sourceIndex && sourceIndex < serviceIndex, 'Bottom reference links are not provider, source, service.');
assert(detail.includes('<b>Map display notes:</b> Source colors are approximated for map display.'), 'legend_note content is not labeled as Map display notes.');
assert(!detail.includes('Legend note'), 'legend_note prose is still labeled as a legend note.');
assert(!source.includes('<b>Legend note:</b>'), 'A rendered External UI path still labels legend_note prose as a legend note.');
assert(detail.includes('a=1&amp;b=2'), 'Reference-link URLs are not escaped.');
assert(!detailHtml({service_type: 'geojson'}).includes('opens external page'), 'Missing reference links were rendered.');
const duplicateSource = detailHtml({
  service_type: 'feature',
  service_url: 'https://example.test/FeatureServer/0',
  source_page: 'https://example.test/FeatureServer/0'
});
assert(!duplicateSource.includes('Source page — opens external page'), 'Duplicate source/service link suppression changed.');
assert(duplicateSource.includes('<b>Service URL</b> —'), 'Service URL disappeared when duplicate source was suppressed.');

// The approved visible legacy strings are gone while internal PT2 identifiers
// remain legal and intact.
[
  'PT2 identify-enabled hover/click',
  'If PT2 cannot draw a service legend directly',
  'PT2 will try to auto-select'
].forEach(text => assert(!source.includes(text), `Rendered legacy wording remains: ${text}`));
assert(source.includes('BRIM identify-enabled hover/click'), 'BRIM identify wording is missing.');
assert(source.includes('If BRIM cannot draw a service legend directly'), 'BRIM provider-legend guidance is missing.');
assert(source.includes('BRIM will try to auto-select'), 'BRIM sublayer guidance is missing.');
assert(source.includes('PT2_CAPABILITY_DEFINITIONS') && source.includes('PT2_CATALOG'), 'Permitted internal PT2 identifiers were renamed.');

// Loading a catalog row populates the existing fields but does not invoke the
// add loader. Revealing the destination scrolls only the panel and focuses the
// first editable field on the next animation frame.
const fieldIds = [
  'pt-custom-name', 'pt-custom-url', 'pt-custom-type', 'pt-custom-clickable',
  'pt-custom-current-view', 'pt-custom-where', 'pt-custom-legend-url',
  'pt-custom-legend-note', 'pt-custom-min-zoom-live',
  'pt-custom-min-zoom-current-view', 'pt-custom-popup-fields',
  'pt-custom-popup-aliases', 'pt-custom-popup-link-template',
  'pt-custom-popup-link-label', 'pt-custom-hover-fields',
  'pt-custom-hover-aliases', 'pt-custom-hover-bold-fields',
  'pt-custom-hover-no-label-fields', 'pt-custom-hover-round-fields',
  'pt-custom-hover-show-native-field-names', 'pt-custom-default-label-field',
  'pt-custom-out-fields', 'pt-custom-style-field-candidates',
  'pt-custom-default-style-field', 'pt-custom-field-curation-notes',
  'pt-custom-show-native-field-names'
];
const formElements = Object.fromEntries(fieldIds.map(id => [id, {value: '', checked: false}]));
let addCalls = 0;
let actionLabelCalls = 0;
const applyCatalogRecord = new Function(
  'document', 'ptGetSelectedCatalogRecord', 'ptCatalogField', 'ptTruth',
  'ptUpdatePopupSupportUI', 'ptUpdateLoadModeUI', 'ptSetStatus',
  'ptHandleAddCustomLayer', 'ptUpdateManualAddActionLabel',
  `${extractFunction(source, 'ptApplyCatalogRecord')}; return ptApplyCatalogRecord;`
)(
  {getElementById: id => formElements[id] || null},
  () => ({
    display_name: 'Configured test layer',
    service_url: 'https://example.test/FeatureServer/0',
    service_type: 'feature',
    default_clickable: 'true',
    supports_popups: 'true',
    default_load_mode: 'current_view',
    where_clause: "STATE = 'CA'",
    popup_fields: 'NAME',
    out_fields: 'NAME,STATE'
  }),
  (record, field) => cleanText(record && record[field]),
  value => /^(true|1|yes)$/i.test(cleanText(value)),
  () => {},
  () => {},
  () => {},
  () => { addCalls += 1; },
  () => { actionLabelCalls += 1; return 'Add filtered overlay'; }
);
applyCatalogRecord(false);
assert(formElements['pt-custom-name'].value === 'Configured test layer', 'Catalog layer name was not populated.');
assert(formElements['pt-custom-url'].value.endsWith('/FeatureServer/0'), 'Catalog service URL was not populated.');
assert(formElements['pt-custom-type'].value === 'feature', 'Catalog service type was not populated.');
assert(formElements['pt-custom-current-view'].checked, 'Catalog load mode was not populated.');
assert(formElements['pt-custom-where'].value === "STATE = 'CA'", 'Catalog SQL was not populated.');
assert(formElements['pt-custom-popup-fields'].value === 'NAME', 'Catalog hidden field metadata was not populated.');
assert(addCalls === 0 && actionLabelCalls === 1, 'Load into advanced form invoked an add loader or skipped action-label refresh.');

const mapState = {center: [37, -120], zoom: 7, activeLayers: 2};
const mapStateBefore = JSON.stringify(mapState);
const panelBody = {
  scrollTop: 20,
  getBoundingClientRect: () => ({top: 100})
};
const advanced = {
  open: false,
  closest: selector => selector === '.pt-tools-body' ? panelBody : null,
  getBoundingClientRect: () => ({top: 500})
};
let focused = false;
let focusOptions = null;
const nameInput = {
  focus: options => { focused = true; focusOptions = options; }
};
const revealAdvancedForm = new Function(
  'document', 'window',
  `${extractFunction(source, 'ptRevealAdvancedManualForm')}; return ptRevealAdvancedManualForm;`
)(
  {getElementById: id => ({
    'pt-advanced-manual-details': advanced,
    'pt-custom-name': nameInput
  })[id] || null},
  {requestAnimationFrame: callback => callback(), setTimeout: callback => callback()}
);
revealAdvancedForm();
assert(advanced.open, 'Advanced form disclosure did not open.');
assert(panelBody.scrollTop === 412, 'Panel scroll position did not move toward the advanced form.');
assert(focused && focusOptions && focusOptions.preventScroll === true, 'Layer-name field did not receive preventScroll focus.');
assert(JSON.stringify(mapState) === mapStateBefore, 'Advanced-form reveal changed map or active-layer state.');
const loadIntoFormContract = extractFunction(source, 'ptLoadCatalogQuickIndexIntoForm');
assert(
  loadIntoFormContract.indexOf('ptApplyCatalogRecord(false)') <
    loadIntoFormContract.indexOf('ptRevealAdvancedManualForm()'),
  'Load into advanced form does not populate fields before revealing the destination.'
);
assert(!/ptHandleAddCustomLayer|ptAddCustomRecord|\.addTo\(|map\./.test(loadIntoFormContract), 'Load into advanced form can activate or move a map layer.');

// One normalized SQL decision drives both truthful action wording and the
// value that may be serialized into a custom-layer record.
const sqlState = new Function(
  'ptDetectServiceType', 'ptCleanText', 'ptIsParentMapServerUrl',
  `${extractFunction(source, 'ptManualSqlState')}; return ptManualSqlState;`
)(
  (url, selectedType) => {
    if (selectedType && selectedType !== 'auto') return selectedType;
    if (/FeatureServer/i.test(url)) return 'feature';
    if (/MapServer/i.test(url)) return 'map';
    if (/ImageServer/i.test(url)) return 'image';
    if (/geojson/i.test(url)) return 'geojson';
    return 'unknown';
  },
  cleanText,
  url => /\/MapServer\/?(?:[?#].*)?$/i.test(cleanText(url))
);
const featureFiltered = sqlState('https://x/FeatureServer/0', 'auto', false, 'live', "STATE = 'CA'");
assert(featureFiltered.actionLabel === 'Add filtered overlay' && featureFiltered.appliedWhereClause === "STATE = 'CA'", 'FeatureServer SQL is not classified as applied.');
assert(sqlState('https://x/FeatureServer/0', 'feature', false, 'live', '').actionLabel === 'Add configured overlay', 'Blank SQL does not show configured wording.');
assert(sqlState('https://x/MapServer/2', 'map', false, 'live', 'YEAR = 2025').actionLabel === 'Add filtered overlay', 'Visual MapServer sublayer SQL is not supported.');
assert(sqlState('https://x/MapServer', 'map', true, 'current_view', 'YEAR = 2025').actionLabel === 'Add filtered overlay', 'Current-view parent MapServer resolution lost SQL support.');
const parentVisual = sqlState('https://x/MapServer', 'map', false, 'live', 'YEAR = 2025');
assert(parentVisual.actionLabel === 'Add configured overlay' && !parentVisual.appliedWhereClause && parentVisual.validationWhereClause === 'YEAR = 2025', 'Parent visual MapServer validation state is incorrect.');
['geojson', 'image'].forEach(type => {
  const state = sqlState(`https://x/data/${type}`, type, false, 'live', 'A = 1');
  assert(state.actionLabel === 'Add configured overlay' && !state.appliedWhereClause && !state.allowsInput, `${type} retained unsupported SQL.`);
});
const tiled = sqlState('https://x/MapServer/2', 'map', false, 'tiled', 'A = 1');
assert(tiled.actionLabel === 'Add configured overlay' && !tiled.appliedWhereClause && !tiled.validationWhereClause && !tiled.allowsInput, 'Tiled MapServer retained unsupported SQL.');

const featureLiveContract = extractFunction(source, 'ptAddArcgisFeatureLayer');
const featureCurrentContract = extractFunction(source, 'ptAddArcgisFeatureLayerCurrentView');
const mapCurrentContract = extractFunction(source, 'ptAddArcgisMapLayerCurrentView');
const mapVisualContract = extractFunction(source, 'ptAddArcgisMapLayer');
assert(featureLiveContract.includes('layerOptions.where = whereText'), 'Live FeatureServer loader does not receive SQL.');
assert(featureCurrentContract.includes('ptRunArcgisCurrentViewFeatureQuery(url, bounds, whereText'), 'Current-view FeatureServer loader does not receive SQL.');
assert(mapCurrentContract.includes('.where(whereText)'), 'Current-view MapServer loader does not receive SQL.');
assert(mapVisualContract.includes('opts.layerDefs[parsed.layerId] = whereText'), 'Visual MapServer sublayer does not receive layerDefs SQL.');
['ptAddGeoJsonLayer', 'ptAddArcgisImageLayer', 'ptAddArcgisTiledMapLayer'].forEach(name => {
  assert(!/whereClause|layerDefs|\.where\(/.test(extractFunction(source, name)), `${name} claims unsupported SQL.`);
});

const existingRecord = {id: 'existing', name: 'Existing layer'};
const existingBefore = JSON.stringify(existingRecord);
let layerAdded = false;
const recordHarness = new Function(
  'seedRecords', 'window', 'map', 'ptCleanText',
  'ptCatalogLoadWasCancelled', 'ptDeriveLegendUrl',
  'ptRenderCustomLayerList', 'ptRenderQuickCatalog',
  'ptUpdateWcrCompletedDepthMapLegend', 'ptUpdateMlrsMineralCasesMapLegend',
  'ptUpdateSgmaPrioritizationMapLegend', 'ptUpdateSubsidenceObservationMapLegend',
  `let ptCustomLayerSeq = 0;
   const ptCustomLayers = seedRecords;
   const ptOpsPromotedGeneration = {};
   const ptOpsPromotedCancelled = {};
   ${extractFunction(source, 'ptAddCustomRecord')}
   return {add: ptAddCustomRecord, records: ptCustomLayers};`
)(
  [existingRecord],
  {},
  {},
  cleanText,
  () => false,
  () => '',
  () => {}, () => {}, () => {}, () => {}, () => {}, () => {}
);
recordHarness.add(
  'New filtered layer',
  {addTo: () => { layerAdded = true; }},
  '#123456',
  'ArcGIS FeatureServer',
  'https://x/FeatureServer/0',
  {whereClause: "STATE = 'CA'"}
);
assert(recordHarness.records.length === 2 && layerAdded, 'Configured add did not create and add a new layer record.');
assert(JSON.stringify(existingRecord) === existingBefore, 'Configured add mutated an existing layer record.');
assert(recordHarness.records[1].whereClause === "STATE = 'CA'", 'Supported SQL was not retained on the new layer record.');

const loadingButton = {
  textContent: 'Add configured overlay', disabled: false,
  setAttribute() {}, removeAttribute() {},
  classList: {add() {}, remove() {}}
};
const loadingSpinner = {style: {}};
const labelRef = {value: 'Add filtered overlay'};
const loadingHarness = new Function(
  'document', 'window', 'Date', 'labelRef',
  `let ptManualAddClearTimer = null;
   let ptManualAddStartedAt = 0;
   let ptManualAddInProgress = false;
   const PT2_MANUAL_ADD_MIN_SPINNER_MS = 0;
   function ptSetInlineNote() {}
   function ptUpdateManualAddActionLabel() {
     const btn = document.getElementById('pt-custom-add-btn');
     if (btn) btn.textContent = labelRef.value;
     return labelRef.value;
   }
   ${extractFunction(source, 'ptSetManualAddLoading')}
   return ptSetManualAddLoading;`
)(
  {getElementById: id => ({
    'pt-custom-add-btn': loadingButton,
    'pt-custom-add-spinner': loadingSpinner
  })[id] || null},
  {clearTimeout() {}, setTimeout: callback => callback()},
  Date,
  labelRef
);
loadingHarness(true, 'Adding manual overlay…');
assert(loadingButton.textContent === 'Adding…', 'Loading-state button wording changed.');
loadingHarness(false);
assert(loadingButton.textContent === 'Add filtered overlay', 'Filtered action label was not restored after loading.');
labelRef.value = 'Add configured overlay';
loadingHarness(true, 'Adding manual overlay…');
loadingHarness(false);
assert(loadingButton.textContent === 'Add configured overlay', 'Configured action label was not restored after loading.');

console.log('External panel polish tests passed.');
