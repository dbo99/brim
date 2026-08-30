#!/usr/bin/env node

"use strict";

// Dedicated source/model contracts for the GUIDE-I2B-R8 Resource Explorer.
// Uses only Node built-ins and executes the pure model used by the browser.

const fs = require("fs");
const path = require("path");
const assert = require("assert");

const root = path.resolve(__dirname, "..");
const guidePath = path.join(root, "03_functions", "js", "leaflet_brim_guide.js");
const cssPath = path.join(root, "03_functions", "css", "leaflet_brim_guide.css");
const registryPath = path.join(root, "00_config", "guide_resources.json");
const enrichmentPath = path.join(root, "00_config", "guide_product_enrichment.json");
const source = fs.readFileSync(guidePath, "utf8");
const css = fs.readFileSync(cssPath, "utf8");
const registry = JSON.parse(fs.readFileSync(registryPath, "utf8"));
const enrichment = JSON.parse(fs.readFileSync(enrichmentPath, "utf8"));
const metadataVocabularies = {
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
  temporalCharacter: {
    current_or_near_real_time: "Current or near-real-time",
    forecast: "Forecast",
    historical_archive: "Historical archive",
    climatology_or_normals: "Climatology or normals",
    static_reference: "Static reference",
    mixed: "Mixed",
    unknown: "Unknown"
  },
  geographicScope: {
    global: "Global",
    multinational: "Multinational",
    national: "National",
    multi_state: "Multi-state",
    state: "State",
    regional: "Regional",
    local: "Local",
    unknown: "Unknown"
  }
};

const callback = new Function(`return (${source})`)();
assert.strictEqual(typeof callback, "function", "Guide onRender callback did not compile");

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

const factorySource = extractFunction(source, "ptCreateResourceExplorerModel");
const createModel = new Function(`return (${factorySource})`)();

function exactRelationshipRows() {
  return enrichment.products.flatMap(product =>
    (product.resource_relationships || []).map(relationship => ({
      productId: product.stable_id,
      ...relationship
    }))
  );
}

const relationshipRows = exactRelationshipRows();
assert.strictEqual(relationshipRows.length, 17, "Enrichment must contain 17 exact rows");
assert.deepStrictEqual(
  Object.fromEntries(["displayed_in_brim", "used_by_brim", "related_external_resource"]
    .map(type => [type, relationshipRows.filter(row => row.relationship_type === type).length])),
  { displayed_in_brim: 0, used_by_brim: 7, related_external_resource: 10 },
  "Exact relationship type counts changed"
);
assert.strictEqual(registry.resources.length, 33, "Registry Resource count changed");
assert.strictEqual(registry.schema_version, 3, "Registry schema version changed");
assert(registry.resources.every(resource => resource.publication_state === "published"),
  "Every R6 Resource must be published");
const resourceIdsByRelationshipType = Object.fromEntries(
  ["displayed_in_brim", "used_by_brim", "related_external_resource"].map(type => [
    type,
    [...new Set(relationshipRows.filter(row => row.relationship_type === type)
      .map(row => row.id))]
  ])
);
assert.deepStrictEqual(
  Object.fromEntries(Object.entries(resourceIdsByRelationshipType)
    .map(([type, ids]) => [type, ids.length])),
  { displayed_in_brim: 0, used_by_brim: 6, related_external_resource: 3 },
  "Exact unique Resource relationship-subtype counts changed"
);
const brimLinkedIds = new Set([
  ...resourceIdsByRelationshipType.displayed_in_brim,
  ...resourceIdsByRelationshipType.used_by_brim,
  ...resourceIdsByRelationshipType.related_external_resource
]);
const beyondIds = new Set(registry.resources.map(resource => resource.id)
  .filter(id => !brimLinkedIds.has(id)));
assert.strictEqual(brimLinkedIds.size, 9, "BRIM-linked unique Resource count changed");
assert.strictEqual(beyondIds.size, 24, "Beyond-BRIM complement count changed");
assert([...brimLinkedIds].every(id => !beyondIds.has(id)),
  "BRIM-linked and Beyond BRIM are not disjoint");
assert.strictEqual(brimLinkedIds.size + beyondIds.size, 33,
  "BRIM-linked and Beyond BRIM are not exhaustive");

function projectedResource(record) {
  const relatedProducts = relationshipRows
    .filter(row => row.id === record.id)
    .map(row => ({
      id: row.productId,
      title: `Product ${row.productId}`,
      entityType: "Layer",
      relationshipType: row.relationship_type,
      role: row.role,
      useScope: row.use_scope
    }));
  const relationshipTypes = relatedProducts.map(product => product.relationshipType);
  const displayed = relationshipTypes.includes("displayed_in_brim");
  const used = relationshipTypes.includes("used_by_brim");
  const related = relationshipTypes.includes("related_external_resource");
  const brimLinked = displayed || used || related;
  const providers = record.providers.map(provider => ({
    name: provider.name,
    role: provider.role
  }));
  const provider = providers.find(value => value.role === "display_provider").name;
  const accessPoints = record.access_points.map(point => ({
    role: point.role,
    label: point.role === "canonical" ? "Official Resource" : point.label,
    url: point.url
  }));
  const result = {
    kind: "Resource",
    id: record.id,
    title: record.title,
    aliases: record.search_aliases,
    provider,
    providers,
    summary: record.summary,
    canonicalUrl: record.canonical_url,
    accessPoints,
    resourceType: record.resource_type,
    resourceTypeLabel: metadataVocabularies.resourceType[record.resource_type],
    temporalCharacter: record.temporal_character,
    temporalCharacterLabel: metadataVocabularies.temporalCharacter[
      record.temporal_character
    ],
    resourceGranularity: record.resource_granularity,
    subjectTags: record.subject_tags,
    informationTypes: record.information_type_tags,
    variables: record.variables,
    useScopes: record.use_scopes,
    geographicScope: {
      scopeType: record.geographic_scope.scope_type,
      scopeLabel: metadataVocabularies.geographicScope[record.geographic_scope.scope_type],
      names: record.geographic_scope.names
    },
    relatedProducts,
    relationshipFlags: {
      brimLinked,
      beyondBrim: !brimLinked,
      displayedInBrim: displayed,
      usedByBrim: used,
      relatedExternalResource: related
    },
    searchText: ""
  };
  result.searchText = [
    result.title, result.aliases, result.provider,
    result.providers.map(value => value.name), result.summary,
    result.accessPoints.map(value => value.label), result.resourceTypeLabel,
    result.resourceGranularity, result.subjectTags, result.informationTypes,
    result.variables, result.useScopes, result.geographicScope.scopeLabel,
    result.geographicScope.names
  ].flat(Infinity).join(" ");
  return result;
}

const fullResources = registry.resources.map(projectedResource);
const model = createModel(fullResources);

assert(fullResources.every(resource =>
  resource.resourceTypeLabel === metadataVocabularies.resourceType[resource.resourceType] &&
  resource.temporalCharacterLabel ===
    metadataVocabularies.temporalCharacter[resource.temporalCharacter] &&
  resource.geographicScope.scopeLabel ===
    metadataVocabularies.geographicScope[resource.geographicScope.scopeType]
), "Build-derived controlled metadata labels changed");
assert.deepStrictEqual(
  fullResources.filter(resource => resource.temporalCharacter === "unknown")
    .map(resource => resource.id),
  [
    "resource_blm_california", "resource_usgs_bcmv8",
    "resource_nidis_soil_moisture_dashboard",
    "resource_nidis_grace_groundwater_soil_moisture"
  ],
  "Temporal unknown Resource IDs changed"
);
assert.deepStrictEqual(
  fullResources.filter(resource => resource.geographicScope.scopeType === "unknown")
    .map(resource => resource.id),
  ["resource_prism_normals"],
  "Geographic-scope unknown Resource ID changed"
);
const doiResource = fullResources.find(resource => resource.id === "resource_doi");
assert(!model.normalize(doiResource.searchText).includes(
  model.normalize(doiResource.temporalCharacterLabel)
), "Temporal character entered the R8 search corpus");

assert.strictEqual(model.normalize("  Café—Water & Forecasts  "),
  "cafe water forecasts", "NFKD/punctuation/whitespace normalization changed");
const fullCounts = model.facetCounts(model.createState()).presets;
assert.deepStrictEqual(fullCounts, {
  brim_linked: 9,
  beyond_brim: 24,
  all_resources: 33
}, "Relationship preset truth table changed");
assert.deepStrictEqual(model.presets.map(preset => preset.id),
  ["brim_linked", "beyond_brim", "all_resources"],
  "Resource preset model order changed");
const subtypeCounts = model.facetCounts(model.createState()).relationshipTypes;
assert.deepStrictEqual({
  displayed_in_brim: subtypeCounts.displayed_in_brim || 0,
  used_by_brim: subtypeCounts.used_by_brim || 0,
  related_external_resource: subtypeCounts.related_external_resource || 0
}, { displayed_in_brim: 0, used_by_brim: 6, related_external_resource: 3 },
"Relationship subtype facet counts changed");
let presetState = model.setPreset(model.createState(), "brim_linked");
assert.strictEqual(presetState.preset, "brim_linked",
  "BRIM-linked preset was not selected");
presetState = model.setPreset(presetState, "beyond_brim");
assert.strictEqual(presetState.preset, "beyond_brim",
  "A later relationship preset did not replace the prior preset");
assert.strictEqual(model.results(presetState).length, 24,
  "Preset switching became additive instead of mutually exclusive");
assert.strictEqual(model.chips(presetState).filter(chip => chip.key === "preset").length, 0,
  "A primary relationship view incorrectly created a removable chip");

function fixture(overrides = {}) {
  const value = {
    kind: "Resource",
    id: "resource_fixture",
    title: "River Atlas",
    aliases: ["Watershed Book"],
    provider: "NOAA",
    providers: [{ name: "NOAA", role: "display_provider" }],
    summary: "Daily observations for California basins.",
    canonicalUrl: "https://example.gov/resource",
    accessPoints: [{
      role: "canonical", label: "Official Resource", url: "https://example.gov/resource"
    }],
    resourceType: "dashboard",
    resourceTypeLabel: "Dashboard",
    temporalCharacter: "current_or_near_real_time",
    temporalCharacterLabel: "Current or near-real-time",
    resourceGranularity: "viewer",
    subjectTags: ["Surface Water"],
    informationTypes: ["Live Observation"],
    variables: ["streamflow"],
    useScopes: ["screening"],
    geographicScope: {
      scopeType: "national", scopeLabel: "National", names: ["California"]
    },
    relatedProducts: [],
    relationshipFlags: {
      brimLinked: false, beyondBrim: true, displayedInBrim: false,
      usedByBrim: false, relatedExternalResource: false
    },
    searchText: "River Atlas Watershed Book NOAA Daily observations California basins Dashboard viewer Surface Water Live Observation streamflow screening National California Official Resource"
  };
  return Object.assign(value, overrides);
}

const weightedResources = [
  fixture({ id: "resource_title", title: "Alpha", searchText: "Alpha" }),
  fixture({ id: "resource_alias", title: "Zulu alias", aliases: ["Alpha"], searchText: "Zulu alias Alpha" }),
  fixture({ id: "resource_provider", title: "Zulu provider", provider: "Alpha", providers: [{ name: "Alpha", role: "display_provider" }], searchText: "Zulu provider Alpha" }),
  fixture({ id: "resource_subject", title: "Zulu subject", subjectTags: ["Alpha"], searchText: "Zulu subject Alpha" }),
  fixture({ id: "resource_summary", title: "Zulu summary", summary: "Alpha", searchText: "Zulu summary Alpha" })
];
const weighted = createModel(weightedResources);
const weightedItems = weighted.resultItems(weighted.setQuery(weighted.createState(), "alpha"));
assert.deepStrictEqual(weightedItems.map(item => item.resource.id), [
  "resource_title", "resource_alias", "resource_provider", "resource_subject", "resource_summary"
], "Fixed search-weight order changed");
assert.strictEqual(weightedItems[0].score, 1200 + 341,
  "Title exact weight or all-token bonus changed");

const andState = model.setQuery(model.createState(), "soil moisture nasa");
assert(model.results(andState).length > 0, "Cross-field all-token query returned no records");
assert(model.results(andState).every(resource =>
  ["soil", "moisture", "nasa"].every(token => model.normalize(resource.searchText).includes(token))
), "All-token matching allowed a missing token");
assert.strictEqual(model.results(model.setQuery(model.createState(), "soil impossibletoken")).length, 0,
  "All-token matching degraded to OR");

const typoModel = createModel([
  fixture({ id: "resource_ordinary", title: "Climate", searchText: "Climate" }),
  fixture({ id: "resource_typo", title: "Climatic", aliases: ["Climate"], searchText: "Climatic Climate" })
]);
const ordinaryScore = typoModel.resultItems(typoModel.setQuery(typoModel.createState(), "climate"))[0].score;
const typoItems = typoModel.resultItems(typoModel.setQuery(typoModel.createState(), "climare"));
assert(typoItems.length > 0 && typoItems.every(item => item.score === 70),
  "One-edit title/alias recovery changed");
assert(ordinaryScore > typoItems[0].score, "Typo recovery does not score below ordinary matches");
assert.strictEqual(typoModel.results(typoModel.setQuery(typoModel.createState(), "clmr")).length, 0,
  "Four-character typo recovery was incorrectly enabled");

const contextId = relationshipRows[0].productId;
const contextState = model.createState({ productContextId: contextId });
const contextResults = model.resultItems(contextState);
assert(contextResults.length > 0, "Product-context filter returned no exact relationships");
assert(contextResults.every(item => item.resource.relatedProducts.some(product => product.id === contextId)),
  "Product context did not remain an exact relationship filter");
assert(contextResults.every(item => item.score >= 2000), "Exact Product-context boost changed");

const tieModel = createModel([
  fixture({ id: "resource_b", title: "Same Title", searchText: "Same Title" }),
  fixture({ id: "resource_a", title: "Same Title", searchText: "Same Title" })
]);
assert.deepStrictEqual(
  tieModel.results(tieModel.setQuery(tieModel.createState(), "same" )).map(value => value.id),
  ["resource_a", "resource_b"],
  "Normalized-title/stable-ID tie break changed"
);

const providerValues = [...new Set(fullResources.map(resource => resource.provider))];
assert(providerValues.length >= 2, "Fixture needs at least two display providers");
let providerState = model.createState({ providers: providerValues.slice(0, 2) });
assert(model.results(providerState).every(resource => providerValues.slice(0, 2).includes(resource.provider)),
  "Provider multi-select is not OR within provider");
const subjectValue = fullResources.find(resource => resource.subjectTags.length).subjectTags[0];
providerState = model.patchState(providerState, { subject: subjectValue });
assert(model.results(providerState).every(resource =>
  providerValues.slice(0, 2).includes(resource.provider) && resource.subjectTags.includes(subjectValue)
), "Provider and Subject are not AND across dimensions");
const providerCounts = model.facetCounts(model.createState()).providers;
assert.strictEqual(Object.values(providerCounts).reduce((sum, count) => sum + count, 0), 33,
  "Display-provider facet counts changed");
const currentTypeCounts = model.facetCounts(model.createState()).resourceTypes;
assert.deepStrictEqual(Object.keys(currentTypeCounts).sort(), [
  "analysis_tool", "dashboard", "data_portal_or_catalog", "dataset_or_collection",
  "organization_homepage", "program_or_mission", "viewer_or_explorer"
], "Current projected Resource-type machine IDs changed");
assert(Object.keys(currentTypeCounts).every(value =>
  model.resourceTypeLabel(value) === metadataVocabularies.resourceType[value]
), "Current Resource Type labels are not controlled projections");
const deepCounts = model.facetCounts(model.createState());
assert(!Object.prototype.hasOwnProperty.call(deepCounts, "resourceGranularities") &&
  !Object.prototype.hasOwnProperty.call(deepCounts, "geographies") &&
  !Object.prototype.hasOwnProperty.call(deepCounts, "temporalCharacters"),
  "Granularity, geography, or temporal character became a facet-count dimension");
const typeValue = fullResources.find(resource =>
  resource.resourceType && resource.resourceType !== "unknown"
).resourceType;
const typeState = model.patchState(model.createState(), {
  resourceType: typeValue, moreFiltersOpen: true
});
assert(model.results(typeState).length > 0 &&
  model.results(typeState).every(resource => resource.resourceType === typeValue),
  "Resource Type filtering is not deterministic");
const typeChip = model.chips(typeState).find(chip => chip.key === "resourceType");
assert.deepStrictEqual(typeChip, {
  key: "resourceType", value: typeValue,
  label: `Resource type: ${metadataVocabularies.resourceType[typeValue]}`
}, "Resource Type chip lost machine identity or controlled display label");
const granularityProbe = fullResources.find(resource =>
  resource.resourceGranularity && resource.resourceGranularity !== "unknown"
);
assert(model.results(model.setQuery(
  model.createState(), granularityProbe.resourceGranularity
)).some(resource => resource.id === granularityProbe.id),
"Projected granularity stopped contributing to deterministic search");
const geographyProbe = fullResources.find(resource =>
  resource.geographicScope.names.length > 0 &&
  resource.geographicScope.names[0] !== "unknown"
);
assert(model.results(model.setQuery(
  model.createState(), geographyProbe.geographicScope.names[0]
)).some(resource => resource.id === geographyProbe.id),
"Projected geography stopped contributing to deterministic search");

let relationshipState = model.createState();
relationshipState = model.toggleRelationshipType(relationshipState, "used_by_brim");
relationshipState = model.toggleRelationshipType(
  relationshipState, "related_external_resource"
);
assert.deepStrictEqual(relationshipState.relationshipTypes,
  ["used_by_brim", "related_external_resource"],
  "Relationship subtype selections did not remain additive");
assert.strictEqual(model.results(relationshipState).length, 9,
  "Relationship subtypes are not OR within their dimension");
assert(model.results(relationshipState).every(resource =>
  resource.relatedProducts.some(product =>
    relationshipState.relationshipTypes.includes(product.relationshipType)
  )), "Relationship subtype OR filter admitted a non-matching Resource");
let narrowRelationshipState = model.patchState(model.createState(), {
  pane: "facets", facetsOpen: true, renderLimit: 25
});
narrowRelationshipState = model.toggleRelationshipType(
  narrowRelationshipState, "used_by_brim"
);
assert.strictEqual(narrowRelationshipState.pane, "results",
  "Narrow relationship selection did not return to results");
assert.strictEqual(narrowRelationshipState.facetsOpen, false,
  "Narrow relationship selection left the facet disclosure open");
relationshipState = model.patchState(relationshipState, { subject: subjectValue });
assert(model.results(relationshipState).every(resource =>
  resource.subjectTags.includes(subjectValue) && resource.relatedProducts.some(product =>
    relationshipState.relationshipTypes.includes(product.relationshipType)
  )), "Relationship subtype and Subject are not AND across dimensions");
const preservedRelationshipTypes = relationshipState.relationshipTypes.slice();
relationshipState = model.setPreset(relationshipState, "brim_linked");
relationshipState = model.setPreset(relationshipState, "beyond_brim");
assert.deepStrictEqual(relationshipState.relationshipTypes, preservedRelationshipTypes,
  "Primary view switching cleared secondary relationship refinements");
assert.strictEqual(relationshipState.subject, subjectValue,
  "Primary view switching cleared another secondary facet");

let populated = model.createState({
  query: "water", preset: "beyond_brim", providers: providerValues.slice(0, 2),
  providerQuery: "agency", subject: subjectValue, informationType: "Live Observation",
  resourceType: "dashboard", moreFiltersOpen: true,
  relationshipTypes: ["related_external_resource"],
  sortMode: "provider", selectedId: fullResources[0].id, renderLimit: 50,
  facetScrollTop: 37, resultsScrollTop: 211, detailScrollTop: 19
});
const chipKeys = model.chips(populated).map(chip => chip.key);
assert.deepStrictEqual(chipKeys.slice(0, 2), ["provider", "provider"],
  "Provider chip ordering changed");
assert(!chipKeys.includes("preset") && chipKeys.includes("relationshipType"),
  "Primary view or relationship-subtype chip contract changed");
const providerRemoved = model.removeChip(populated, "provider", providerValues[0]);
assert.deepStrictEqual(providerRemoved.providers, [providerValues[1]],
  "Provider chip removal changed more than its owned value");
const providersCleared = model.clearProviders(populated);
assert.deepStrictEqual(providersCleared.providers, [], "Provider-only Clear retained selections");
assert.strictEqual(providersCleared.providerQuery, "", "Provider-only Clear retained provider text");
assert.strictEqual(providersCleared.query, "water", "Provider-only Clear changed query");
assert.strictEqual(providersCleared.subject, subjectValue, "Provider-only Clear changed Subject");
const relationshipRemoved = model.removeChip(
  populated, "relationshipType", "related_external_resource"
);
assert.deepStrictEqual(relationshipRemoved.relationshipTypes, [],
  "Relationship subtype chip removal changed");
const reset = model.reset(populated);
assert.strictEqual(reset.query, "", "Reset retained query");
assert.strictEqual(reset.preset, "all_resources", "Reset did not restore All Resources");
assert.deepStrictEqual(reset.providers, [], "Reset retained providers");
assert.deepStrictEqual(reset.relationshipTypes, [], "Reset retained relationship subtypes");
assert.strictEqual(reset.sortMode, "title", "Reset did not restore Title sort");
assert.strictEqual(reset.renderLimit, 25, "Reset did not restore 25-row limit");
assert.strictEqual(reset.focusKey, "resource-search", "Reset focus target changed");
assert.strictEqual(reset.resourceType, "", "Reset retained Resource Type");
assert(!Object.prototype.hasOwnProperty.call(reset, "resourceGranularity") &&
  !Object.prototype.hasOwnProperty.call(reset, "geography"),
  "Removed metadata facets remain in Resource filter state");
assert.strictEqual(reset.moreFiltersOpen, false, "Reset retained More filters disclosure");
assert.deepStrictEqual(
  [reset.facetScrollTop, reset.resultsScrollTop, reset.detailScrollTop],
  [0, 0, 0],
  "Reset did not restore all Resource scroll regions to their initial positions"
);

const scrollPreserved = model.setPreset(populated, "brim_linked");
assert.deepStrictEqual(
  [scrollPreserved.facetScrollTop, scrollPreserved.resultsScrollTop,
    scrollPreserved.detailScrollTop],
  [37, 211, 19],
  "Primary view switching unexpectedly reset Resource scroll state"
);
assert.strictEqual(scrollPreserved.moreFiltersOpen, true,
  "Primary view switching closed More filters");
assert.strictEqual(scrollPreserved.resourceType, "dashboard",
  "Primary view switching cleared Resource Type");

assert.strictEqual(model.createState().sortMode, "title", "No-query default sort changed");
assert.strictEqual(model.setQuery(model.createState(), "water").sortMode, "relevance",
  "Query default sort did not become Relevance");
assert.deepStrictEqual(model.sortOptions(model.createState()), ["title", "provider"],
  "No-query sort options changed");
assert.deepStrictEqual(model.sortOptions(model.createState({ productContextId: contextId })),
  ["relevance", "title", "provider"], "Context sort options changed");

let pageState = model.createState();
assert.strictEqual(pageState.renderLimit, 25, "Initial result limit changed");
assert.strictEqual(model.results(pageState).slice(0, pageState.renderLimit).length, 25,
  "Initial rendered result count changed");
pageState = model.showMore(pageState);
assert.strictEqual(pageState.renderLimit, 50, "Show more increment changed");
assert.strictEqual(model.results(pageState).slice(0, pageState.renderLimit).length, 33,
  "Show more did not reveal all 33 embedded records");

let detailState = model.createState();
assert.strictEqual(model.detail(detailState), null, "Detail exists before selection");
detailState = model.selectResource(detailState, "resource_noaa_goes_image_viewer");
const goesDetail = model.detail(detailState);
assert(goesDetail, "Selected-only detail was not created");
assert.strictEqual(goesDetail.temporalCharacter, "current_or_near_real_time",
  "Representative non-unknown temporal detail assignment changed");
assert.strictEqual(goesDetail.temporalCharacterLabel, "Current or near-real-time",
  "Representative temporal detail label changed");
const unknownTemporalDetail = fullResources.find(
  resource => resource.id === "resource_blm_california"
);
assert.strictEqual(unknownTemporalDetail.temporalCharacter, "unknown",
  "Approved unknown temporal detail fixture changed");
const unknownGeographyDetail = fullResources.find(
  resource => resource.id === "resource_prism_normals"
);
assert.strictEqual(unknownGeographyDetail.geographicScope.scopeType, "unknown",
  "Approved unknown geography detail fixture changed");
assert.strictEqual(goesDetail.accessPoints.length, 5, "GOES access-point family changed");
assert.deepStrictEqual(goesDetail.accessPoints[0], {
  role: "canonical", label: "Official Resource", url: "https://www.star.nesdis.noaa.gov/GOES/"
}, "Canonical access-point rendering contract changed");
assert.strictEqual(goesDetail.accessPoints.filter(point => point.url.includes("sector=psw")).length, 2,
  "GOES Pacific Southwest configured-view count changed");
const goesAccessGroups = model.accessPointGroups(goesDetail);
assert.deepStrictEqual(goesAccessGroups.official, goesDetail.accessPoints[0],
  "Exact canonical access point was not promoted as the official action");
assert.deepStrictEqual(goesAccessGroups.additional, goesDetail.accessPoints.slice(1),
  "Additional access-point labels, URLs, or order changed");

const roundTrip = model.restore(model.snapshot(populated));
assert.deepStrictEqual(roundTrip, model.createState(populated),
  "Compact-to-wide Resource state snapshot/restore changed");
let facetsState = model.patchState(model.createState(), {
  pane: "facets", facetsOpen: true, renderLimit: 25
});
let escaped = model.escape(facetsState);
assert.strictEqual(escaped.action, "nested", "Facet Escape skipped nested priority");
assert.strictEqual(escaped.state.pane, "results", "Facet Escape did not return to results");
assert.strictEqual(escaped.state.focusKey, "resource-facets-toggle",
  "Facet Escape focus restoration changed");
escaped = model.escape(detailState);
assert.strictEqual(escaped.action, "nested", "Detail Escape skipped nested priority");
assert.strictEqual(escaped.state.pane, "results", "Detail Escape did not return to results");
assert.strictEqual(escaped.state.focusKey, "resource-result-resource_noaa_goes_image_viewer",
  "Detail Escape focus restoration changed");
assert.strictEqual(model.escape(model.createState()).action, "close-guide",
  "Explorer-root Escape no longer closes the Guide");

const lifecycle = model.createLifecycle();
assert.strictEqual(lifecycle.attach(), true, "First listener attach failed");
assert.strictEqual(lifecycle.attach(), false, "Repeated listener attach was not idempotent");
assert.deepStrictEqual(lifecycle.state(), { attached: true, destroyed: false },
  "Attached lifecycle state changed");
assert.strictEqual(lifecycle.detach(), true, "First teardown failed");
assert.strictEqual(lifecycle.detach(), false, "Repeated teardown was not idempotent");
assert.deepStrictEqual(lifecycle.state(), { attached: false, destroyed: true },
  "Destroyed lifecycle state changed");

assert.strictEqual((source.match(/function ptCreateResourceExplorerModel/g) || []).length, 1,
  "Resource Explorer has more than one model/controller authority");
assert(!/fetch\s*\(|XMLHttpRequest|localStorage|sessionStorage|indexedDB/.test(source),
  "Resource Explorer introduced runtime fetch or browser storage");
assert(!/\b(addLayer|removeLayer|show_on_map|configure_on_map)\b/.test(source),
  "Resource Explorer introduced a map/layer action");
assert(!/favorites|recents|verification badge|\bcore\b|priority ranking/i.test(factorySource),
  "Deferred or rejected prototype state entered the Resource model");
assert(source.includes("oldRoot.__brimGuideTeardown()") &&
  source.includes("if (!lifecycle.detach()) return") &&
  source.includes("root.removeEventListener('click', handleRootClick)"),
  "Crash-safe replacement or teardown contract is incomplete");
assert(source.includes("target = '_blank'") && source.includes("rel = 'noopener noreferrer'"),
  "Exact access actions lost safe external-link attributes");
assert(source.includes("function accessPointGroups(resource)") &&
  source.includes("point.role === 'canonical' && point.url === canonicalUrl") &&
  source.includes("'Open official resource ↗'") &&
  source.includes("'Additional access points'") &&
  source.includes("accessGroups.additional.forEach"),
  "Official canonical action or exact additional access-point rendering changed");
assert(source.includes("No Resources currently have an exact reviewed displayed-in-BRIM relationship."),
  "Available-in-BRIM zero state message changed");
assert(source.includes("presets.setAttribute('role', 'radiogroup')") &&
  source.includes("presetButton.setAttribute('role', 'radio')") &&
  source.includes("presetButton.setAttribute('aria-checked'") &&
  source.includes("presetButton.tabIndex = resourceState.preset === preset.id ? 0 : -1") &&
  source.includes("['ArrowLeft', 'ArrowRight', 'Home', 'End']"),
  "Relationship presets lost single-select radio semantics");
assert(source.indexOf("['brim_linked', 'BRIM-linked'") <
  source.indexOf("['beyond_brim', 'Explore beyond BRIM'") &&
  source.indexOf("['beyond_brim', 'Explore beyond BRIM'") <
  source.indexOf("['all_resources', 'Search all Resources'") &&
  source.includes("var enabledPresets = resourceExplorerModel.presets.filter"),
  "Gateway or arrow-key preset order changed");
assert(!source.includes("connected_to_brim") && !source.includes("mapped_in_brim") &&
  source.includes("brim_linked") && source.includes("beyond_brim"),
  "Superseded primary relationship views remain in the controller");
assert(!source.includes("Connected to BRIM") &&
  source.includes("Resources with at least one exact reviewed BRIM Product relationship."),
  "BRIM-linked wording or exact supporting definition changed");
assert(source.includes("Available in BRIM") && source.includes("Used by BRIM") &&
  source.includes("Related resource") && source.includes("Exact BRIM relationships"),
  "Exact relationship subtype controls, badges, or detail explanation are missing");
assert(source.includes("Use A · Explore for BRIM layers, live products, and tools."),
  "Explore-versus-Resources orientation copy is missing");
assert(source.includes("brim-guide__eyebrow brim-guide__masthead") &&
  source.includes("brim-guide__masthead-acronym") &&
  source.includes("word.charAt(0)") && source.includes("word.slice(1)"),
  "Guide masthead no longer preserves the expanded name with B/R/I/M emphasis");
assert(source.includes("function resourceFacetChoices") &&
  source.includes("function resourceFacetSelect") &&
  source.includes("'Subject', 'subject'") &&
  source.includes("'Information Type', 'informationType'") &&
  source.includes("'Resource type', 'resourceType'") &&
  !source.includes("'Resource granularity', 'resourceGranularity'") &&
  !source.includes("'Geography', 'geography'") &&
  !source.includes("'Temporal character', 'temporalCharacter'") &&
  source.includes("'brim-guide__resource-more-toggle', 'More filters'") &&
  !source.includes("'More Filters'"),
  "Visible primary facets or scalable More filters controls changed");
assert(source.includes("option.value = optionValue") &&
  source.includes("valueLabel ? valueLabel(optionValue) : optionValue") &&
  source.includes("resourceExplorerModel.resourceTypeLabel") &&
  source.includes("resource.resourceTypeLabel") &&
  source.includes("resource.resourceGranularity") &&
  source.includes("geographyValues(resource)") &&
  source.includes("appendResourceDetailRow(list, 'Granularity'") &&
  source.includes("appendResourceDetailRow(\n      list,\n      'Geography'"),
  "Controlled Resource Type labels or approved granularity/geography metadata disappeared");
assert(source.includes("resource.temporalCharacter !== 'unknown'") &&
  source.includes("list, 'Temporal character', resource.temporalCharacterLabel") &&
  source.includes("resource.geographicScope.scopeType !== 'unknown'") &&
  source.includes("? resource.geographicScope.scopeLabel : ''"),
  "Temporal detail or unknown temporal/geography omission contract changed");
assert(!factorySource.includes("temporalCharacter") &&
  !extractFunction(source, "resourceBadges").includes("temporalCharacter"),
  "Temporal character entered Resource search/filter state or badges");
assert(source.includes("moreToggle.setAttribute('aria-expanded'") &&
  source.includes("moreToggle.setAttribute('aria-controls'") &&
  source.includes("morePanel.hidden = !resourceState.moreFiltersOpen") &&
  source.includes("action === 'resource-more-filters'"),
  "More filters disclosure semantics or state-preserving controller action is missing");
assert(!/\b(variable|variables|useScope|useScopes):\s*String\(/.test(factorySource) &&
  !source.includes("'Time mode'") && !source.includes("'Temporal class'") &&
  !source.includes("'Update frequency'"),
  "Descriptive variables/use scopes or deferred temporal metadata became R8 facets");
assert(source.includes("candidates[index].offsetParent !== null") &&
  source.includes("if (state.view === 'resource-explorer')") &&
  source.includes("searchInput.focus()"),
  "Hidden Resource focus targets do not fall back to visible search");
assert(source.includes("function captureResourceScrollPositions()") &&
  source.includes("function restoreResourceScrollPositions()") &&
  source.includes("function prepareResourceResultAlignment(resourceId)") &&
  source.includes("function alignPendingResourceResult()") &&
  source.includes("pendingResourceResultAlignment = ''") &&
  source.includes("detail.style.marginTop = Math.max(") &&
  source.includes("content.scrollTop + selectedRow.getBoundingClientRect().top") &&
  source.includes("content.scrollTop + rowTop - contentTop") &&
  source.includes("state.resourceExplorer.resultsScrollTop = content.scrollTop") &&
  source.includes("var content = node('div', 'brim-guide__resource-content')") &&
  source.includes("focus({ preventScroll: true })") &&
  source.includes("function restoreResourceScrollAfterFocus()") &&
  source.includes("window.requestAnimationFrame(function()") &&
  (source.match(/window\.requestAnimationFrame\(function\(\)/g) || []).length >= 2 &&
  source.includes("restoreResourceScrollAfterFocus();") &&
  source.includes("render(false)"),
  "Resource selection alignment, scroll restoration, non-scrolling focus, or reset behavior is missing");
assert(source.includes("select:not([disabled])") &&
  source.includes("if (event.shiftKey && document.activeElement === first)") &&
  source.includes("document.activeElement === last") &&
  source.includes("first.focus()") && source.includes("last.focus()"),
  "Dialog Tab order no longer includes native sort or wraps in both directions");
assert(css.includes("grid-template-columns: minmax(300px, 0.82fr) minmax(430px, 1.18fr)") &&
  css.includes("grid-template-columns: minmax(280px, 1fr) minmax(572px, 2fr)") &&
  css.includes("grid-template-columns: minmax(280px, 1fr) minmax(280px, 1fr)"),
  "Desktop refinement width is not balanced for results and selected detail");
assert(css.includes("@media (min-width: 1101px)") &&
  css.includes(".brim-guide--resource-explorer .brim-guide__main {\n    display: grid; min-height: 0; overflow: hidden;") &&
  css.includes(".brim-guide__resource-explorer-header,\n  .brim-guide__resource-presets,\n  .brim-guide__resource-chips {\n    flex: 0 0 auto;") &&
  css.includes(".brim-guide__resource-presets {\n    min-height: 33px; overflow: hidden;") &&
  css.includes(".brim-guide__resource-facets,\n  .brim-guide__resource-content") &&
  css.includes("overflow-x: hidden; overflow-y: auto;") &&
  css.includes("overflow-anchor: none") &&
  css.includes("overscroll-behavior: contain") &&
  css.includes("scrollbar-gutter: stable"),
  "Wide Resource Explorer header stack or bounded independent content regions changed");
assert(css.includes(".brim-guide--resource-explorer .brim-guide__main {\n  padding: 10px 14px 18px; overflow-anchor: none;"),
  "Resource pane transitions no longer suppress browser scroll-anchor jumps");
assert(source.indexOf("explorer.appendChild(header)") <
  source.indexOf("explorer.appendChild(presets)") &&
  source.indexOf("explorer.appendChild(presets)") <
  source.indexOf("explorer.appendChild(layout)"),
  "Primary preset ribbon entered a scrollable Resource pane");
assert(!/\.brim-guide__resource-results\s*\{[^}]*overflow-y:\s*auto/s.test(css) &&
  !/\.brim-guide__resource-detail\s*\{[^}]*overflow-y:\s*auto/s.test(css),
  "Results or detail introduced a duplicate nested vertical scrollbar");
assert(css.includes("@media (max-width: 700px)") &&
  css.includes("position: sticky; z-index: 2; top: 73px;") &&
  css.includes("[data-resource-pane=\"results\"] .brim-guide__resource-facets") &&
  css.includes("[data-resource-pane=\"facets\"] .brim-guide__resource-content") &&
  css.includes("[data-resource-pane=\"detail\"] .brim-guide__resource-facets"),
  "Narrow one-pane Resource visibility, hit-target isolation, or header offset is missing");
assert(css.includes(".brim-guide__resource-choices") &&
  css.includes(".brim-guide__resource-choice.is-selected") &&
  css.includes(".brim-guide__resource-choice:disabled") &&
  css.includes("@media (max-width: 700px)"),
  "Visible Resource facet choices lack responsive active-state styling");
assert(css.includes("data-resource-preset=\"brim_linked\"") &&
  css.includes("data-resource-preset=\"beyond_brim\"") &&
  css.includes(".brim-guide__resource-choice--brim.is-selected:not(:disabled)") &&
  css.includes(".brim-guide__resource-choice--external.is-selected:not(:disabled)") &&
  css.includes(".brim-guide__resource-badge--brim") &&
  css.includes(".brim-guide__resource-badge--external") &&
  css.includes("color: var(--guide-external-dark)"),
  "Restrained BRIM-green or external-blue semantic treatment is missing");
assert(css.includes(".brim-guide__resource-more-panel[hidden]") &&
  css.includes(".brim-guide__resource-filter-label") &&
  css.includes(".brim-guide__resource-more-toggle[aria-expanded=\"true\"]::after"),
  "More filters disclosure styling or visible expanded state is missing");
assert(css.includes("grid-template-columns: 16px minmax(0, 1fr) auto") &&
  css.includes("align-items: center") &&
  css.includes(".brim-guide__resource-provider-option input[type=\"checkbox\"]") &&
  css.includes("width: 14px; height: 14px; margin: 0; align-self: center"),
  "Provider checkbox size, vertical alignment, or count column changed");
assert(css.includes(".brim-guide__masthead-acronym") &&
  css.includes("font-weight: 800; letter-spacing: inherit"),
  "Subtle masthead acronym weight or inherited tracking changed");
assert(css.includes(".brim-guide__resource-selected-label") &&
  css.includes(".brim-guide__resource-result.is-selected .brim-guide__resource-result-header strong") &&
  css.includes(".brim-guide__resource-access--primary") &&
  css.includes(".brim-guide__resource-access--secondary") &&
  css.includes("background: var(--guide-external-dark); color: #fff") &&
  source.includes("row.setAttribute('aria-current', 'true')"),
  "Selected-row or primary official-access treatment is incomplete");

console.log("GUIDE-I2B-R8 Resource Explorer source/model contracts passed.");
console.log("PUBLISHED_RESOURCES=33");
console.log("RESOURCE_TYPE_MACHINE_ID_LABELS=PASS");
console.log("TEMPORAL_DETAIL_NONUNKNOWN_ONLY=PASS");
console.log("GEOGRAPHY_UNKNOWN_DETAIL_OMISSION=PASS");
console.log("DEFERRED_RESOURCE_FACETS=ABSENT");
console.log("PRESET_COUNTS=33,9,24");
console.log("RELATIONSHIP_SUBTYPE_UNIQUE_COUNTS=0,6,3");
console.log("INITIAL_LIMIT=25");
console.log("SHOW_MORE_VISIBLE=33");
console.log("SELECTED_ONLY_DETAIL=PASS");
console.log("ROOT_AND_NESTED_ESCAPE=PASS");
console.log("LIFECYCLE_IDEMPOTENCE=PASS");
console.log("RUNTIME_RESOURCE_FETCH=NONE");
console.log("RESOURCE_STORAGE_KEYS=NONE");
console.log("RESOURCE_LAYER_ACTIONS=NONE");
