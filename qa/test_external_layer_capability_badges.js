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

const helperNames = [
  'ptCatalogField', 'ptCatalogNumericField', 'ptCapabilityDefinition',
  'ptCatalogCapabilityEnabled', 'ptCapabilityBadgeHtml',
  'ptCatalogLayerCapabilityBadgesHtml',
  'ptCatalogHierarchyCapabilityBadgesHtml', 'ptCatalogHierarchyCountHtml',
  'ptCapabilityDefinitionKeyHtml'
];
const helperSource = helperNames.map(name => extractFunction(source, name)).join('\n');
const definitions = {
  LGND: {code: 'LGND', label: 'Map legend available', capability: 'has_legend'},
  INFO: {
    code: 'INFO',
    label: 'Feature details available by hover or click',
    capability: 'has_feature_info'
  }
};
const cleanText = value => String(value == null ? '' : value).trim();
const escapeHtml = value => cleanText(value).replace(/[&<>"']/g, character => ({
  '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'
})[character]);
const helpers = new Function(
  'PT2_CAPABILITY_DEFINITIONS', 'ptCleanText', 'ptEscapeHtml',
  `${helperSource}; return {${helperNames.join(',')}};`
)(definitions, cleanText, escapeHtml);

const legendOnly = helpers.ptCatalogLayerCapabilityBadgesHtml({
  has_legend: true,
  has_feature_info: false
});
assert(legendOnly.includes('data-capability="legend"'), 'Layer LGND badge did not render for has_legend=true.');
assert(!legendOnly.includes('data-capability="info"'), 'Layer INFO badge rendered for has_feature_info=false.');

const infoOnly = helpers.ptCatalogLayerCapabilityBadgesHtml({
  has_legend: false,
  has_feature_info: true
});
assert(!infoOnly.includes('data-capability="legend"'), 'Layer LGND badge rendered for has_legend=false.');
assert(infoOnly.includes('data-capability="info"'), 'Layer INFO badge did not render for has_feature_info=true.');

const both = helpers.ptCatalogLayerCapabilityBadgesHtml({
  has_legend: true,
  has_feature_info: true
});
assert(both.includes('>LGND</span>') && both.includes('>INFO</span>'), 'LGND and INFO did not render together.');
assert(!helpers.ptCatalogLayerCapabilityBadgesHtml({}), 'Catalog rows without capability fields did not degrade safely.');
assert(!helpers.ptCatalogLayerCapabilityBadgesHtml({has_legend: false, has_feature_info: false}), 'Neither-capability layer rendered a badge.');
assert(!legendOnly.includes('data-capability-count'), 'Layer-level badges display numeric counts.');
assert(legendOnly.includes('aria-label="Map legend available"'), 'Layer LGND lacks its shared accessible definition.');
assert(infoOnly.includes('aria-label="Feature details available by hover or click"'), 'Layer INFO lacks its shared accessible definition.');

const parentRecord = {
  group_legend_count: 2,
  group_info_count: 9,
  subgroup_legend_count: 1,
  subgroup_info_count: 0
};
const groupBadges = helpers.ptCatalogHierarchyCapabilityBadgesHtml(parentRecord, 'group', 10);
assert(groupBadges.includes('data-capability-count="2"') && groupBadges.includes('data-capability-count="9"'), 'Group badges do not use the precomputed nonzero totals.');
assert(groupBadges.includes('aria-label="2 of 10 layers — Map legend available"'), 'Parent accessible label lacks numerator, denominator, or shared definition.');
const subgroupBadges = helpers.ptCatalogHierarchyCapabilityBadgesHtml(parentRecord, 'subgroup', 4);
assert(subgroupBadges.includes('data-capability-count="1"'), 'Subgroup LGND total did not render.');
assert(!subgroupBadges.includes('data-capability="info"'), 'Zero-count subgroup INFO badge was not omitted.');

assert(helpers.ptCatalogHierarchyCountHtml(3, 10) === '(3 of 10)', 'Search-reduced ordinary count is unclear.');
assert(helpers.ptCatalogHierarchyCountHtml(10, 10) === '(10)', 'Unfiltered ordinary count lost its compact form.');
assert(
  helpers.ptCatalogHierarchyCapabilityBadgesHtml(parentRecord, 'group', 10) === groupBadges,
  'Capability totals changed with search-visible row counts.'
);

const keyHtml = helpers.ptCapabilityDefinitionKeyHtml();
assert((source.match(/ptCapabilityDefinitionKeyHtml\(\)/g) || []).length === 2, 'Shared definition key is not invoked exactly once.');
assert((source.match(/id="pt-catalog-capability-key"/g) || []).length === 1, 'Capability key markup is duplicated.');
assert(keyHtml.includes(definitions.LGND.label) && keyHtml.includes(definitions.INFO.label), 'Capability key does not consume shared definition metadata.');
assert((keyHtml.match(/pt-capability-key-item/g) || []).length === 2, 'Capability key does not render both shared definitions.');

const layerBadgeContract = extractFunction(source, 'ptCatalogLayerCapabilityBadgesHtml');
assert(layerBadgeContract.includes("'has_legend'") && layerBadgeContract.includes("'has_feature_info'"), 'Layer badges do not consume build-resolved booleans.');
assert(!/legend_adapter|popup|hover|identify|renderer/i.test(layerBadgeContract), 'Browser-side capability reclassification was introduced.');
const hierarchyContract = extractFunction(source, 'ptCatalogHierarchyCapabilityBadgesHtml');
assert(hierarchyContract.includes("hierarchy + '_legend_count'") && hierarchyContract.includes("hierarchy + '_info_count'"), 'Parent badges do not consume precomputed hierarchy totals.');
assert(!/reduce\(|filter\(/.test(hierarchyContract), 'Parent capability totals are aggregated during rendering.');

const renderContract = extractFunction(source, 'ptRenderQuickCatalog');
assert(renderContract.includes('ptCatalogHierarchyCountHtml(groupRows.length, groupTotal)'), 'Group search count clarity is not wired into rendering.');
assert(renderContract.includes('ptCatalogHierarchyCountHtml(subgroupRows.length, subgroupTotal)'), 'Subgroup search count clarity is not wired into rendering.');
assert(renderContract.includes('ptCatalogLayerCapabilityBadgesHtml(rec)'), 'Layer capability badges are not rendered in the catalog.');
assert(renderContract.includes('pt-catalog-row-text') && renderContract.includes('pt-catalog-info-btn') && renderContract.includes('pt-catalog-add-small'), 'Capability markup replaced an existing row control region.');
assert(renderContract.includes('pt-catalog-active-badge') && renderContract.includes('ptCatalogNumberBadgeHtml(rec)'), 'Capability markup displaced Active or load-hint badges.');

const badgeHtml = legendOnly + infoOnly + groupBadges + subgroupBadges;
assert(!/pt-catalog-load-(green|yellow|orange|red|gray)/.test(badgeHtml), 'Capability badges reuse load-hint semantic classes.');
assert(!/<button|tabindex=|data-pt-|onclick=/i.test(badgeHtml), 'Capability badges are interactive or focusable.');
assert(/\.pt-capability-badges\s*\{[\s\S]*?flex-wrap:\s*wrap/.test(source), 'Capability badge containers do not wrap.');
assert(/\.pt-catalog-layer-name\s*\{[\s\S]*?flex-wrap:\s*wrap/.test(source), 'Layer flexible text region does not wrap badges.');
assert(/\.pt-catalog-row-main\s*\{[\s\S]*?grid-template-columns:\s*28px 1fr auto/.test(source), 'Add/text/info row layout changed unexpectedly.');

console.log('External layer capability badge source-contract tests passed.');
