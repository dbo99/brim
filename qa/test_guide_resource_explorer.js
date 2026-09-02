#!/usr/bin/env node

"use strict";

// Source/model contracts for the GUIDE-I2B-R12B Resource Explorer.
// Uses Node built-ins only; no browser, network, or generated artifact is required.

const fs = require("fs");
const path = require("path");
const assert = require("assert");

const root = path.resolve(__dirname, "..");
const source = fs.readFileSync(
  path.join(root, "03_functions", "js", "leaflet_brim_guide.js"), "utf8"
);
const css = fs.readFileSync(
  path.join(root, "03_functions", "css", "leaflet_brim_guide.css"), "utf8"
);
const helperSource = fs.readFileSync(
  path.join(root, "03_functions", "leaflet_guide_helpers.r"), "utf8"
);
const registry = JSON.parse(fs.readFileSync(
  path.join(root, "00_config", "guide_resources.json"), "utf8"
));
const relationships = JSON.parse(fs.readFileSync(
  path.join(root, "00_config", "guide_product_resource_relationships.json"), "utf8"
));

new Function(`return (${source})`)();

function extractFunction(text, functionName) {
  const start = text.indexOf(`function ${functionName}`);
  assert(start >= 0, `Could not locate ${functionName}`);
  const firstBrace = text.indexOf("{", start);
  let depth = 0;
  let quote = null;
  let escaped = false;
  let lineComment = false;
  let blockComment = false;
  for (let index = firstBrace; index < text.length; index += 1) {
    const char = text[index];
    const next = text[index + 1];
    if (lineComment) {
      if (char === "\n") lineComment = false;
      continue;
    }
    if (blockComment) {
      if (char === "*" && next === "/") {
        blockComment = false;
        index += 1;
      }
      continue;
    }
    if (quote) {
      if (escaped) escaped = false;
      else if (char === "\\") escaped = true;
      else if (char === quote) quote = null;
      continue;
    }
    if (char === "/" && next === "/") {
      lineComment = true;
      index += 1;
      continue;
    }
    if (char === "/" && next === "*") {
      blockComment = true;
      index += 1;
      continue;
    }
    if (char === "\"" || char === "'" || char === "`") {
      quote = char;
      continue;
    }
    if (char === "{") depth += 1;
    if (char === "}") {
      depth -= 1;
      if (depth === 0) return text.slice(start, index + 1);
    }
  }
  throw new Error(`Could not close ${functionName}`);
}

const createModel = new Function(
  `return (${extractFunction(source, "ptCreateResourceExplorerModel")})`
)();

assert.strictEqual(relationships.schema_version, 2,
  "Product-Resource relationship authority must use schema version 2");
assert.strictEqual(relationships.products.length, 270,
  "Relationship authority Product count changed");
assert.strictEqual(relationships.resources.length, 72,
  "Relationship authority Resource review count changed");
assert.strictEqual(registry.schema_version, 3, "Resource registry schema changed");
assert.strictEqual(registry.resources.length, 72, "Canonical Resource count changed");

const canonicalResourceIds = registry.resources.map(resource => resource.id);
const relationshipResourceIds = relationships.resources.map(resource => resource.resource_id);
assert.deepStrictEqual(relationshipResourceIds, canonicalResourceIds,
  "Relationship authority Resource universe/order changed");
assert.strictEqual(new Set(relationships.products.map(product => product.product_id)).size, 270,
  "Relationship Product IDs are not unique");

const canonicalLinks = relationships.products.flatMap(product =>
  product.resource_links.map(link => ({ productId: product.product_id, ...link }))
);
assert.strictEqual(canonicalLinks.length, 86, "Canonical relationship link count changed");
assert.deepStrictEqual(Object.fromEntries([
  "direct_match_in_brim", "selected_product_from_broader_resource", "source_reference"
].map(role => [role, canonicalLinks.filter(link => link.relationship_role === role).length])), {
  direct_match_in_brim: 13,
  selected_product_from_broader_resource: 57,
  source_reference: 16
}, "Canonical relationship-role counts changed");

const authorityText = JSON.stringify(relationships);
[
  "temporary_r12a_legacy_public_projection", "relationshipFlags", "brimLinked",
  "beyondBrim", "displayedInBrim", "usedByBrim", "relatedExternalResource"
].forEach(field => assert(!authorityText.includes(field),
  `Retired relationship field remains in schema-v2 authority: ${field}`));
assert(!helperSource.includes("pt_guide_r12a_temporary_legacy_public_relationship_projection"),
  "Temporary R12A compiler adapter remains");

const published = registry.resources.filter(resource => resource.publication_state === "published");
const staged = registry.resources.filter(resource => resource.publication_state === "staged");
assert.strictEqual(published.length, 67, "Published Resource count changed");
assert.strictEqual(staged.length, 5, "Staged Resource count changed");
const publishedIds = published.map(resource => resource.id);
const productById = new Map(relationships.products.map(product => [product.product_id, product]));
const representationById = new Map(
  relationships.resources.map(resource => [resource.resource_id, resource])
);
const publishedRepresentations = published.map(resource => representationById.get(resource.id));
assert(publishedRepresentations.every(record => record.map_review_state === "reviewed"),
  "Every published Resource must be reviewed");
assert.deepStrictEqual(Object.fromEntries([
  "direct_match_in_brim", "selected_products_in_brim", "not_currently_mapped_in_brim"
].map(value => [value, publishedRepresentations.filter(
  record => record.map_representation === value
).length])), {
  direct_match_in_brim: 3,
  selected_products_in_brim: 20,
  not_currently_mapped_in_brim: 44
}, "Published map-representation truth changed");
assert(staged.every(resource => {
  const record = representationById.get(resource.id);
  return record.map_review_state === "not_yet_reviewed" &&
    record.map_representation === null && record.evidence_refs.length === 0;
}), "Staged Resources did not remain explicitly deferred");

const metadataLabels = {
  resourceType: {
    program_or_mission: "Program or mission",
    dataset_or_collection: "Dataset or collection",
    data_portal_or_catalog: "Data portal or catalog",
    viewer_or_explorer: "Viewer or explorer",
    dashboard: "Dashboard",
    analysis_tool: "Analysis tool",
    data_service_or_api: "Data service or API",
    documentation_or_guide: "Documentation or guide",
    organization_homepage: "Organization homepage",
    report_or_publication: "Report or publication"
  },
  temporal: {
    current_or_near_real_time: "Current or near-real-time",
    forecast: "Forecast",
    historical_archive: "Historical archive",
    climatology_or_normals: "Climatology or normals",
    static_reference: "Static reference",
    mixed: "Mixed",
    unknown: "Unknown"
  },
  geography: {
    global: "Global", multinational: "Multinational", national: "National",
    multi_state: "Multi-state", state: "State", regional: "Regional",
    local: "Local", unknown: "Unknown"
  }
};

function publicResource(record) {
  const representation = representationById.get(record.id);
  const representedProducts = [];
  for (const product of relationships.products) {
    const sourceResourceIds = publishedIds.filter(resourceId =>
      product.resource_links.some(link => link.resource_id === resourceId)
    );
    product.resource_links.filter(link => link.resource_id === record.id).forEach(link => {
      representedProducts.push({
        productId: product.product_id,
        title: `Product ${product.product_id}`,
        deliveryClass: product.delivery_class,
        coverageDisposition: product.coverage_disposition,
        relationshipRole: link.relationship_role,
        sourceResourceIds
      });
    });
  }
  const providers = record.providers.map(provider => ({
    name: provider.name, role: provider.role
  }));
  const accessPoints = record.access_points.map(point => ({
    role: point.role,
    label: point.role === "canonical" ? "Official Resource" : point.label,
    url: point.url
  }));
  const geographicScope = {
    scopeType: record.geographic_scope.scope_type,
    scopeLabel: metadataLabels.geography[record.geographic_scope.scope_type],
    names: record.geographic_scope.names
  };
  const result = {
    kind: "Resource", id: record.id, title: record.title,
    aliases: record.search_aliases,
    provider: providers.find(provider => provider.role === "display_provider").name,
    providers, summary: record.summary, canonicalUrl: record.canonical_url,
    accessPoints, resourceType: record.resource_type,
    resourceTypeLabel: metadataLabels.resourceType[record.resource_type],
    temporalCharacter: record.temporal_character,
    temporalCharacterLabel: metadataLabels.temporal[record.temporal_character],
    resourceGranularity: record.resource_granularity,
    subjectTags: record.subject_tags,
    informationTypeTags: record.information_type_tags,
    variables: record.variables, useScopes: record.use_scopes, geographicScope,
    mapReviewState: representation.map_review_state,
    mapRepresentation: representation.map_representation,
    representedProducts, searchText: ""
  };
  result.searchText = [
    result.title, result.aliases, result.provider,
    result.providers.map(provider => provider.name), result.summary,
    result.accessPoints.map(point => point.label), result.resourceTypeLabel,
    result.resourceGranularity, result.subjectTags, result.informationTypeTags,
    result.variables, result.useScopes, geographicScope.scopeLabel,
    geographicScope.names, representedProducts.map(product => product.title)
  ].flat(Infinity).join(" ");
  return result;
}

const resources = published.map(publicResource);
const model = createModel(resources);
assert.deepStrictEqual(model.presets, [
  { id: "in_brim_map", label: "In BRIM map" },
  { id: "beyond_the_map", label: "Beyond the map" },
  { id: "all_resources", label: "All Resources" }
], "Primary Resource views changed");
assert.deepStrictEqual(model.facetCounts(model.createState()).presets, {
  in_brim_map: 23, beyond_the_map: 44, all_resources: 67
}, "Primary Resource view counts changed");
assert.strictEqual(model.results(model.createState({ preset: "in_brim_map" })).length, 23,
  "In BRIM map membership changed");
assert.strictEqual(model.results(model.createState({ preset: "beyond_the_map" })).length, 44,
  "Beyond the map membership changed");
assert.strictEqual(model.results(model.createState({ preset: "all_resources" })).length, 67,
  "All Resources membership changed");

let state = model.createState({
  preset: "all_resources", resultsScrollTop: 417, returnResultsScrollTop: 417
});
state = model.setQuery(state, "GOES");
assert(model.results(state).some(resource => resource.id === "resource_noaa_goes_image_viewer"),
  "Search no longer resolves the GOES Resource");
assert.strictEqual(state.sortMode, "relevance", "Search did not select relevance sort");
state = model.setQuery(state, "");
assert.strictEqual(state.sortMode, "title", "Cleared search did not restore title sort");
state = model.toggleProvider(state, resources[0].provider);
assert(state.providers.includes(resources[0].provider), "Provider filter was not selected");
assert(model.chips(state).some(chip => chip.key === "provider"),
  "Provider filter did not create a removable chip");
state = model.clearProviders(state);
assert.deepStrictEqual(state.providers, [], "Provider filters did not clear");
state = model.showMore(state);
assert.strictEqual(state.renderLimit, 50, "Show-more page size changed");

const goes = resources.find(resource => resource.id === "resource_noaa_goes_image_viewer");
const accessGroups = model.accessPointGroups(goes);
assert.strictEqual(accessGroups.official.url, "https://www.star.nesdis.noaa.gov/GOES/",
  "GOES canonical Official Resource action changed");
assert.strictEqual(accessGroups.additional.length, 4,
  "GOES additional access-point count changed");

const detailState = model.selectResource(model.createState({
  resultsScrollTop: 417, returnResultsScrollTop: 417
}), goes.id);
assert.strictEqual(detailState.pane, "detail", "Selection did not open detail");
assert.strictEqual(detailState.detailScrollTop, 0, "Selected detail did not reset to top");
assert.strictEqual(detailState.selectedFocusKey, `resource-result-${goes.id}`,
  "Selected-row focus key changed");
const escaped = model.escape(detailState);
assert.strictEqual(escaped.action, "nested", "Detail Escape skipped nested handling");
assert.strictEqual(escaped.state.pane, "results", "Detail Escape did not restore results");
assert.strictEqual(escaped.state.resultsScrollTop, 417,
  "Detail Escape did not restore prior results position");
assert.strictEqual(escaped.state.focusKey, `resource-result-${goes.id}`,
  "Detail Escape did not restore selected-row focus");
assert.strictEqual(model.escape(model.createState()).action, "close-guide",
  "Explorer-root Escape no longer closes the Guide");
assert.deepStrictEqual(model.restore(model.snapshot(detailState)), detailState,
  "Resource Explorer state round-trip changed");

const multipleSource = relationships.products.find(product =>
  product.coverage_disposition === "multiple_source_resources"
);
assert(multipleSource && multipleSource.resource_links.length >= 2,
  "Multiple-source Product authority disappeared");
const multipleProjection = resources.flatMap(resource => resource.representedProducts)
  .find(product => product.productId === multipleSource.product_id);
assert.deepStrictEqual(multipleProjection.sourceResourceIds,
  publishedIds.filter(resourceId => multipleSource.resource_links.some(
    link => link.resource_id === resourceId
  )), "Multiple-source exact Resource list changed");

const dwrFamilyIds = [
  "DWR_TRE_ALTAMIRA_ANNUAL_RATE_MOSAIC",
  "DWR_TRE_ALTAMIRA_TOTAL_SINCE_2015_MOSAIC",
  "DWR_TRE_ALTAMIRA_POINT_LOCATIONS_2026Q1"
];
assert(dwrFamilyIds.every(id => productById.get(id).coverage_review_state === "not_yet_reviewed"),
  "DWR/TRE Altamira family decision changed");
assert(productById.get("EXT033").coverage_review_state === "not_yet_reviewed",
  "EXT033 multiple-source composite decision changed");

const lifecycle = model.createLifecycle();
assert.strictEqual(lifecycle.attach(), true, "First listener attach failed");
assert.strictEqual(lifecycle.attach(), false, "Listener attach is not idempotent");
assert.strictEqual(lifecycle.detach(), true, "First teardown failed");
assert.strictEqual(lifecycle.detach(), false, "Teardown is not idempotent");

assert.strictEqual((source.match(/function ptCreateResourceExplorerModel/g) || []).length, 1,
  "Resource Explorer has more than one model authority");
assert(!/fetch\s*\(|XMLHttpRequest|localStorage|sessionStorage|indexedDB/.test(source),
  "Resource Explorer introduced runtime fetch or browser storage");
assert(!/\b(addLayer|removeLayer|show_on_map|configure_on_map)\b/.test(source),
  "Resource Explorer introduced a map/layer action");
[
  "BRIM-linked", "Beyond BRIM", "Available in BRIM", "Used by BRIM",
  "Related resource", "resource-relationship-choice", "relationshipFlags",
  "brimLinked", "beyondBrim", "displayedInBrim", "usedByBrim",
  "relatedExternalResource"
].forEach(value => assert(!source.includes(value),
  `Retired public relationship presentation remains: ${value}`));
assert(source.includes("'In BRIM map'") && source.includes("'Beyond the map'") &&
  source.includes("'All Resources'"), "Exact primary map-presence labels are missing");
assert(source.includes("This Resource is represented directly in the BRIM map.") &&
  source.includes("BRIM includes selected products from this broader Resource.") &&
  source.includes("This Resource is not currently represented in the BRIM map."),
"Exact Resource representation statements are missing");
assert(source.includes("'Delivery: ' + deliveryLabel(relationship.deliveryClass)") &&
  source.includes("BRIM combines this Product from multiple sources: ") &&
  source.includes("sourceTitles.join('; ') + '.'"),
"Secondary delivery or exact multiple-source explanation is missing");
assert(!source.includes("delivery-filter") && !source.includes("multiple-source-filter"),
  "Delivery or multiple-source status became a primary filter");
assert(source.includes("presets.setAttribute('role', 'radiogroup')") &&
  source.includes("presetButton.setAttribute('role', 'radio')") &&
  source.includes("presetButton.setAttribute('aria-checked'") &&
  source.includes("['ArrowLeft', 'ArrowRight', 'Home', 'End']"),
"Primary map-presence views lost radio or keyboard semantics");

assert(source.includes("var resultsScroll = node('div', 'brim-guide__resource-results-scroll')") &&
  source.includes("resultsScroll.setAttribute('aria-label', 'Scrollable Resource results')") &&
  source.includes("var detail = existing.querySelector('.brim-guide__resource-detail')") &&
  source.includes("if (results) state.resourceExplorer.resultsScrollTop = results.scrollTop") &&
  source.includes("if (detail) state.resourceExplorer.detailScrollTop = detail.scrollTop") &&
  source.includes("state.resourceExplorer.returnResultsScrollTop = currentResultsScrollTop") &&
  source.includes("state.resourceExplorer.detailScrollTop = 0") &&
  source.includes("render(false)") && source.includes("focus({ preventScroll: true })"),
"Independent scroll capture, detail reset, or focus restoration is incomplete");
assert(source.includes("brim-guide__resource-result-alignment-spacer") &&
  source.includes("var desiredScrollTop = Math.max(0, results.scrollTop + rowTop - contentTop)") &&
  source.includes("desiredScrollTop - Math.max(0, results.scrollHeight - results.clientHeight)"),
"Selected-row top alignment lost its near-end scroll-range guarantee");
assert(!source.includes("detail.style.marginTop") && !source.includes("scrollIntoView"),
  "Shared-scroll offset or scrollIntoView workaround remains");
assert(css.includes(".brim-guide__resource-content {\n    min-height: 0; overflow: hidden;") &&
  css.includes(".brim-guide__resource-results-scroll,\n  .brim-guide__resource-detail {") &&
  css.includes("overflow-x: hidden; overflow-y: auto") &&
  css.includes("overscroll-behavior: contain") && css.includes("scrollbar-gutter: stable"),
"Desktop results/detail do not own independent native scroll regions");
assert(css.includes("@media (max-width: 700px)") &&
  css.includes("[data-resource-pane=\"results\"] .brim-guide__resource-detail") &&
  css.includes("[data-resource-pane=\"detail\"] .brim-guide__resource-results"),
"Narrow one-pane visibility contract changed");
assert(css.includes("data-resource-preset=\"in_brim_map\"") &&
  css.includes("data-resource-preset=\"beyond_the_map\"") &&
  css.includes(".brim-guide__resource-representation--beyond"),
"Restrained map-presence color treatment is missing");

console.log("GUIDE-I2B-R12B Resource Explorer source/model contracts passed.");
console.log("CANONICAL_RESOURCES=72");
console.log("PUBLISHED_RESOURCES=67");
console.log("STAGED_RESOURCES=5");
console.log("PRESET_COUNTS=23,44,67");
console.log("MAP_REPRESENTATION_COUNTS=3,20,44");
console.log("CANONICAL_RESOURCE_LINKS=86");
console.log("LEGACY_PUBLIC_PROJECTIONS=0");
console.log("DESKTOP_SCROLL_OWNERS=PROVIDERS,RESULTS,DETAIL");
console.log("NARROW_ONE_PANE=PASS");
console.log("LIFECYCLE_IDEMPOTENCE=PASS");
