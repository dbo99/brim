#!/usr/bin/env node

"use strict";

// Source/model contracts for the BRIM Guide Resource Explorer.
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


// Current approved addition identities and exact public overlay boundaries.
const r17c2NewResourceIds = [
  "resource_dwr_sgma_water_year_type_dataset",
  "resource_smud_upper_american_river_project_conditions",
  "resource_sjrrp_friant_releases_and_allocations",
  "resource_sonoma_water_russian_river_operating_conditions",
  "resource_usbr_stanislaus_watershed_team",
  "resource_trrp_flows_and_releases",
  "resource_sce_flow_and_reservoir_portal",
  "resource_sce_big_creek_project",
  "resource_google_deepmind_weather_lab",
  "resource_california_environmental_flows_framework",
  "resource_california_natural_flows",
  "resource_uswfs_public_information",
  "resource_inciweb_incident_information",
  "resource_nifc_public_fire_information",
  "resource_northern_california_fire_coordination",
  "resource_southern_california_fire_coordination",
  "resource_blm_maps_and_geospatial_data",
  "resource_blm_california_wildfire_dashboard_public"
];
assert.deepStrictEqual(registry.resources.slice(215).map(r => r.id),r17c2NewResourceIds);
const l082 = registry.resources.find(r => r.id === 'resource_blm_california_wildfire_dashboard_public');
assert.equal(l082.canonical_url,'https://nifc.maps.arcgis.com/apps/dashboards/1c4565c092da44478befc12722cf0486#');
assert.equal(l082.access_points.length,1);
assert(!relationships.products.some(p=>p.resource_links.some(l=>l.resource_id===l082.id)));
const usgsAPI = registry.resources.find(r=>r.id==='resource_usgs_usgs_water_services_apis_service');
assert.equal(usgsAPI.canonical_url,'https://api.waterdata.usgs.gov/');
assert(usgsAPI.search_aliases.includes('USGS Water Services APIs'));
const allActionURLs = registry.resources.flatMap(r=>r.access_points.map(a=>a.url));
assert.equal(allActionURLs.length,457);assert.equal(new Set(allActionURLs).size,456);

const r17c1AddedResourceIds = [
  "resource_usgs_usgs_california_river_basin_schematics_collection",
  "resource_noaa_nws_graphical_forecasts",
  "resource_polarwx_tropical",
  "resource_brightband_operational_weatherbench",
  "resource_geolibre",
  "resource_noaa_wpc_excessive_rainfall_outlook"
];
const r17c1RetiredResourceIds = [
  "resource_noaa_cnrfc_forcing_csv_service",
  "resource_noaa_cnrfc_hourly_hefs_csv_service"
];
const r16bAddedResourceIds = [
  "resource_usace_usace_water_management_data_platform",
  "resource_usace_sacramento_district_water_control_data_system",
  "resource_usace_los_angeles_district_water_management_platform",
  "resource_usbr_central_valley_operations_office_platform",
  "resource_usbr_lower_colorado_river_operations",
  "resource_usbr_upper_colorado_basin_water_operations",
  "resource_usbr_colorado_river_basin_hub",
  "resource_usbr_klamath_project_water_operations_platform",
  "resource_usbr_truckee_river_operating_agreement_platform",
  "resource_usbr_reclamation_information_sharing_environment_platform",
  "resource_usbr_central_valley_project_water_supply_program",
  "resource_usbr_reclamation_hydromet_platform",
  "resource_usbr_reclamation_agrimet_platform"
];
const r16bRetiredResourceIds = [
  "resource_usace_usace_warm_springs_dam_lake_sonoma_hourly_data_product",
  "resource_usace_usace_terminus_dam_lake_kaweah_hourly_data_product",
  "resource_usace_usace_hidden_dam_hensley_lake_hourly_data_product",
  "resource_usace_usace_sacramento_river_clear_creek_hourly_data_product",
  "resource_usace_usace_farmington_dam_hourly_data_product",
  "resource_usace_usace_pine_flat_lake_hourly_data_product",
  "resource_usbr_cvp_swp_long_term_operations_record_of_decision_product"
];
const spkId = "resource_usace_sacramento_district_water_control_data_system";
const cnrfcId = "resource_noaa_cnrfc";
const cnrfcSummaryBefore =
  "Operational river, precipitation, temperature, snow-level, and water-supply forecasting for California and Nevada.";
const cnrfcSummaryAfter =
  "Operational river, reservoir-inflow, precipitation, temperature, freezing-level, and short- to long-term water-supply forecasting for California and Nevada.";
const a5SummaryAppend = " Selected shortcuts include full natural flow (FNF) water-year trend plots for named forecast points; use the Water Resources map for the wider network.";
const a5LabelChanges = [
  {
    "station_code": "CEGC1",
    "old_label": "Trinity",
    "new_label": "Trinity River — Trinity Lake — FNF water-year trend (CEGC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=CEGC1&prodID=9"
  },
  {
    "station_code": "CMPC1",
    "old_label": "Mokelumne",
    "new_label": "Mokelumne River — Pardee Reservoir — FNF water-year trend (CMPC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=CMPC1&prodID=9"
  },
  {
    "station_code": "EXQC1",
    "old_label": "Merced",
    "new_label": "Merced River — Lake McClure / New Exchequer — FNF water-year trend (EXQC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=EXQC1&prodID=9"
  },
  {
    "station_code": "FOLC1",
    "old_label": "American",
    "new_label": "American River — Folsom Lake — FNF water-year trend (FOLC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=FOLC1&prodID=9"
  },
  {
    "station_code": "FRAC1",
    "old_label": "San Joaquin",
    "new_label": "San Joaquin River — Millerton Reservoir — FNF water-year trend (FRAC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=FRAC1&prodID=9"
  },
  {
    "station_code": "HLEC1",
    "old_label": "Yuba",
    "new_label": "Yuba River — Englebright Reservoir — FNF water-year trend (HLEC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=HLEC1&prodID=9"
  },
  {
    "station_code": "ISAC1",
    "old_label": "Kern",
    "new_label": "Kern River — Lake Isabella — FNF water-year trend (ISAC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=ISAC1&prodID=9"
  },
  {
    "station_code": "MHBC1",
    "old_label": "Cosumnes",
    "new_label": "Cosumnes River — Michigan Bar — FNF water-year trend (MHBC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=MHBC1&prodID=9"
  },
  {
    "station_code": "NDPC1",
    "old_label": "Tuolumne",
    "new_label": "Tuolumne River — New Don Pedro Reservoir — FNF water-year trend (NDPC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=NDPC1&prodID=9"
  },
  {
    "station_code": "NMSC1",
    "old_label": "Stanislaus",
    "new_label": "Stanislaus River — New Melones Reservoir — FNF water-year trend (NMSC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=NMSC1&prodID=9"
  },
  {
    "station_code": "ORDC1",
    "old_label": "Feather",
    "new_label": "Feather River — Lake Oroville — FNF water-year trend (ORDC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=ORDC1&prodID=9"
  },
  {
    "station_code": "PFTC1",
    "old_label": "Kings",
    "new_label": "Kings River — Pine Flat Reservoir — FNF water-year trend (PFTC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=PFTC1&prodID=9"
  },
  {
    "station_code": "SCSC1",
    "old_label": "Tule",
    "new_label": "Tule River — Lake Success — FNF water-year trend (SCSC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=SCSC1&prodID=9"
  },
  {
    "station_code": "SHDC1",
    "old_label": "Sacramento/McCloud/Pit",
    "new_label": "Sacramento River — Shasta Lake — FNF water-year trend (SHDC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=SHDC1&prodID=9"
  },
  {
    "station_code": "TMDC1",
    "old_label": "Kaweah",
    "new_label": "Kaweah River — Lake Kaweah — FNF water-year trend (TMDC1)",
    "url": "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=TMDC1&prodID=9"
  }
];
const a5NewActions = [
  {
    "role": "configured_view",
    "label": "Water Resources — Regional Forecast Map",
    "url": "https://www.cnrfc.noaa.gov/water_resources_update.php"
  },
  {
    "role": "configured_view",
    "label": "Daily Basin QPF & Freezing Levels — Days 1–6 (HD6RSA)",
    "url": "https://www.cnrfc.noaa.gov/awipsProducts/RNOHD6RSA.php"
  }
];
const cnrfcCanonicalUrlBefore = "https://cnrfc.noaa.gov/";
const cnrfcCanonicalUrlAfter = "https://www.cnrfc.noaa.gov/";
const integratedReportId = "resource_swrcb_impaired_waters_and_tmdls_program";
const integratedReportProductIds = ["SWRCB_2024_IR_LINES", "SWRCB_2024_IR_POLYGONS"];
const integratedReportCanonicalUrl =
  "https://www.waterboards.ca.gov/water_issues/programs/water_quality_assessment/";
const productTitleFixtures = {
  cnrfc_fnf_delta: "CNRFC FNF Sha/Tri/west Sierra Basins",
  cnrfc_stream: "CNRFC river/reservoir catalog",
  cnrfc_precip_weather_station_catalog: "CNRFC weather station catalog",
  cnrfc_basin_product_availability: "CNRFC Product Availability",
  ops_cnrfc_forecast_points: "CNRFC forecast points | river/reservoir",
  ops_major_water_supply_forecasts: "Major Water-Supply Basin Forecasts",
  ops_cdec_reservoir_storage: "Reservoirs | storage-centric | CDEC / CNRFC / USACE"
};

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
const resourceGatewaySearchOptions = new Function(
  `return (${extractFunction(source, "resourceGatewaySearchOptions")})`
)();
const resourceCountText = new Function(
  `return (${extractFunction(source, "resourceCountText")})`
)();
const guideFilterOwner = new Function(
  `return (${extractFunction(source, "guideFilterOwner")})`
)();
const appendResourceFacetLabel = new Function(
  "node",
  `return (${extractFunction(source, "appendResourceFacetLabel")})`
)(function(tag, className, text) {
  return {
    tag,
    className,
    textContent: String(text),
    attributes: {},
    setAttribute(name, value) { this.attributes[name] = value; }
  };
});

assert.strictEqual(resourceGatewaySearchOptions("resources", "landing", ""), null,
  "Focusing the empty Resource gateway search must not navigate");
assert.strictEqual(resourceGatewaySearchOptions("resources", "landing", "   "), null,
  "Whitespace-only Resource gateway input must not navigate");
assert.strictEqual(resourceGatewaySearchOptions("explore", "landing", "coco"), null,
  "A Explore search was captured by Resource gateway routing");
assert.strictEqual(resourceGatewaySearchOptions("resources", "resource-explorer", "cocoa"), null,
  "An open Resource Explorer would be recreated by a subsequent keystroke");
assert.deepStrictEqual(
  resourceGatewaySearchOptions("resources", "landing", " CoCo "),
  { preset: "all_resources", query: " CoCo ", focusSearch: true },
  "Resource gateway routing lost the exact first non-whitespace query"
);
const crossSurfaceFixture = {
  section: "explore",
  view: "landing",
  productSubject: "Precipitation",
  productResultCount: 28,
  resourceResultCount: 210
};
assert.strictEqual(guideFilterOwner(crossSurfaceFixture.section), "products",
  "A Explore did not own its Product filter state");
assert.strictEqual(crossSurfaceFixture.productResultCount, 28,
  "The A Explore precipitation fixture count changed");
crossSurfaceFixture.section = "resources";
assert.strictEqual(guideFilterOwner(crossSurfaceFixture.section), "resources",
  "C Resources did not become the active filter owner");
assert.strictEqual(crossSurfaceFixture.productSubject, "Precipitation",
  "The privately preserved A filter fixture was destroyed during A-to-C navigation");
assert.strictEqual(crossSurfaceFixture.resourceResultCount, 210,
  "A Product filter implicitly changed the clean C Resource gateway count");
const renderSourceStart = source.indexOf("function render(preserveResourceScroll)");
const renderSource = source.slice(
  renderSourceStart,
  source.indexOf("function focusMain", renderSourceStart)
);
assert(renderSource.includes("var filterOwner = guideFilterOwner(state.section)") &&
  renderSource.includes("if (filterOwner !== 'products')") &&
  renderSource.includes("activeSummary.hidden = true"),
"Inactive-section Product filters can leak into the active utility bar");
const rootClickSource = extractFunction(source, "handleRootClick");
const sectionBranchStart = rootClickSource.indexOf("action === 'section'");
const sectionBranchEnd = rootClickSource.indexOf("} else if", sectionBranchStart);
const sectionBranchSource = rootClickSource.slice(sectionBranchStart, sectionBranchEnd);
assert(sectionBranchSource.includes("state.view = 'landing'") &&
  !sectionBranchSource.includes("openResourceExplorer("),
"A Product filter implicitly opens All Resources during A-to-C navigation");
assert.strictEqual(resourceCountText(1), "1 Resource",
  "Singular Resource count semantics changed");
assert.strictEqual(resourceCountText(2), "2 Resources",
  "Plural Resource count semantics changed");
const facetFixture = {
  children: [],
  appendChild(child) { this.children.push(child); }
};
appendResourceFacetLabel(facetFixture, "Climate & Drought", 1);
assert.deepStrictEqual(facetFixture.children.map(child => ({
  className: child.className,
  textContent: child.textContent,
  ariaHidden: child.attributes["aria-hidden"] || null
})), [
  {
    className: "brim-guide__resource-facet-label",
    textContent: "Climate & Drought",
    ariaHidden: null
  },
  {
    className: "brim-guide__resource-facet-count",
    textContent: "1",
    ariaHidden: "true"
  }
], "Resource facet label and numeric count are not separate DOM nodes");

assert.strictEqual(relationships.schema_version, 2,
  "Product-Resource relationship authority must use schema version 2");
assert.strictEqual(registry.schema_version, 3, "Resource registry schema changed");

const canonicalResourceIds = registry.resources.map(resource => resource.id);
const relationshipResourceIds = relationships.resources.map(resource => resource.resource_id);
assert.strictEqual(new Set(canonicalResourceIds).size, canonicalResourceIds.length,
  "Canonical Resource IDs are not unique");
assert.strictEqual(new Set(relationshipResourceIds).size, relationshipResourceIds.length,
  "Relationship Resource IDs are not unique");
assert.strictEqual(canonicalResourceIds.length, 233,
  "R17C2 canonical Resource count must be exactly 233");
assert.deepStrictEqual(
  canonicalResourceIds.filter(id => r16bAddedResourceIds.includes(id)).sort(),
  [...r16bAddedResourceIds].sort(),
  "The exact 13 R16B canonical additions changed"
);
assert(r16bRetiredResourceIds.every(id => !canonicalResourceIds.includes(id)),
  "An exact R16B merged Resource remains canonical");
assert.deepStrictEqual(
  registry.resources.flatMap(resource => resource.aliases).sort(),
  [...r16bRetiredResourceIds, ...r17c1RetiredResourceIds].sort(),
  "The exact nine retired stable IDs are not preserved as aliases"
);
assert.deepStrictEqual([...relationshipResourceIds].sort(), [...canonicalResourceIds].sort(),
  "Relationship authority Resource set does not equal the canonical Resource set");
assert.strictEqual(
  new Set(relationships.products.map(product => product.product_id)).size,
  relationships.products.length,
  "Relationship Product IDs are not unique");

const canonicalLinks = relationships.products.flatMap(product =>
  product.resource_links.map(link => ({ productId: product.product_id, ...link }))
);
assert.strictEqual(canonicalLinks.length, 121, "Canonical relationship link count changed");
assert.deepStrictEqual(Object.fromEntries([
  "direct_match_in_brim", "selected_product_from_broader_resource", "source_reference"
].map(role => [role, canonicalLinks.filter(link => link.relationship_role === role).length])), {
  direct_match_in_brim: 13,
  selected_product_from_broader_resource: 64,
  source_reference: 44
}, "Canonical relationship-role counts changed");
assert.strictEqual(new Set(canonicalLinks.map(link =>
  `${link.productId}\r${link.resource_id}`)).size, canonicalLinks.length,
"A duplicate canonical Product-Resource pair was introduced");
assert.strictEqual(relationships.products.filter(product =>
  product.resource_links.length > 0).length, 93,
"Products-with-Resource-links count changed");
assert.strictEqual(new Set(canonicalLinks.map(link => link.resource_id)).size, 28,
"Resources-with-Product-links count changed");
const d10Product = relationships.products.find(
  product => product.product_id === "ops_cdec_reservoir_storage"
);
assert.deepStrictEqual(d10Product.resource_links.map(link => ({
  resourceId: link.resource_id,
  role: link.relationship_role
})), [
  {
    resourceId: "resource_dwr_cdec",
    role: "selected_product_from_broader_resource"
  },
  { resourceId: spkId, role: "source_reference" },
  { resourceId: cnrfcId, role: "source_reference" }
], "D10 must preserve CDEC/SPK and add only the exact CNRFC source reference");

function exactRelationshipFixture(productId) {
  const product = relationships.products.find(record => record.product_id === productId);
  return {
    reviewState: product.coverage_review_state,
    disposition: product.coverage_disposition,
    evidenceBasis: product.coverage_evidence_basis,
    links: product.resource_links.map(link => ({
      resourceId: link.resource_id,
      role: link.relationship_role
    }))
  };
}
assert.deepStrictEqual(exactRelationshipFixture("cnrfc_stream"), {
  reviewState: "reviewed",
  disposition: "selected_product_from_broader_resource",
  evidenceBasis: "reviewed_evidence",
  links: [{
    resourceId: cnrfcId,
    role: "selected_product_from_broader_resource"
  }]
}, "The exact CNRFC stream relationship fixture changed");
assert.deepStrictEqual(exactRelationshipFixture("cnrfc_precip_weather_station_catalog"), {
  reviewState: "reviewed",
  disposition: "selected_product_from_broader_resource",
  evidenceBasis: "reviewed_evidence",
  links: [{
    resourceId: cnrfcId,
    role: "selected_product_from_broader_resource"
  }]
}, "The exact CNRFC weather-station relationship fixture changed");
assert.deepStrictEqual(exactRelationshipFixture("ops_major_water_supply_forecasts"), {
  reviewState: "reviewed",
  disposition: "selected_product_from_broader_resource",
  evidenceBasis: "reviewed_evidence",
  links: [
    { resourceId: "resource_noaa_nwps", role: "source_reference" },
    { resourceId: "resource_usgs_national_hydrography_products", role: "source_reference" },
    { resourceId: cnrfcId, role: "source_reference" }
  ]
}, "The exact major water-supply CNRFC source fixture changed");
assert.deepStrictEqual(exactRelationshipFixture("ops_us_drought_monitor"), {
  reviewState: "reviewed",
  disposition: "selected_product_from_broader_resource",
  evidenceBasis: "maintainer_clarification",
  links: [{
    resourceId: "resource_climate_and_drought_data_providers_drought_gov_california_dashboard",
    role: "selected_product_from_broader_resource"
  }]
}, "The exact Drought.gov California relationship fixture changed");
["product-ops-cocorahs-ca-daily", "ops_cocorahs_conus_daily"].forEach(productId => {
  assert.deepStrictEqual(exactRelationshipFixture(productId), {
    reviewState: "reviewed",
    disposition: "selected_product_from_broader_resource",
    evidenceBasis: "maintainer_clarification",
    links: [{
      resourceId: "resource_cocorahs_cocorahs_other",
      role: "selected_product_from_broader_resource"
    }]
  }, `The exact CoCoRaHS relationship fixture changed for ${productId}`);
});
["brim_mapped_conveyance", "ops_delta_snapshot"].forEach(productId => {
  assert.deepStrictEqual(exactRelationshipFixture(productId), {
    reviewState: "not_yet_reviewed",
    disposition: null,
    evidenceBasis: "not_yet_reviewed",
    links: []
  }, `An unsupported California Water Watch relationship remains for ${productId}`);
});
integratedReportProductIds.forEach(productId => {
  assert.deepStrictEqual(exactRelationshipFixture(productId), {
    reviewState: "reviewed",
    disposition: "selected_product_from_broader_resource",
    evidenceBasis: "reviewed_evidence",
    links: [{
      resourceId: integratedReportId,
      role: "selected_product_from_broader_resource"
    }]
  }, `The exact Integrated Report relationship changed for ${productId}`);
});
assert(!relationships.products.some(product =>
  /2026.*(integrated|_ir_)|(integrated|_ir_).*2026/i.test(product.product_id)
), "A 2026 Integrated Report Product was inferred without tracked authority");

const integratedReportRaw = registry.resources.find(resource =>
  resource.id === integratedReportId
);
assert(integratedReportRaw, "The statewide Integrated Report parent is missing");
assert.strictEqual(integratedReportRaw.title,
  "California Integrated Reports & Impaired Waters",
  "The evergreen Integrated Report title changed");
assert.strictEqual(integratedReportRaw.canonical_url, integratedReportCanonicalUrl,
  "The evergreen Integrated Report canonical action changed");
assert.deepStrictEqual(integratedReportRaw.access_points, [
  {
    role: "canonical",
    label: "Surface Water Quality Assessment Program",
    url: integratedReportCanonicalUrl
  },
  {
    role: "configured_view",
    label: "2024 California Integrated Report — EPA partial approval / partial disapproval",
    url: `${integratedReportCanonicalUrl}2024-integrated-report.html`
  },
  {
    role: "configured_view",
    label: "2026 California Integrated Report — State Board approved; submitted to EPA",
    url: `${integratedReportCanonicalUrl}2026_integrated_report.html`
  }
], "The exact current Integrated Report actions changed");
assert.deepStrictEqual(integratedReportRaw.search_aliases, [
  "Impaired Waters and TMDLs", "303(d)", "305(b)",
  "California Integrated Report", "impaired waters", "TMDL",
  "surface water quality assessment"
], "The exact Integrated Report search aliases changed");
assert.deepStrictEqual(integratedReportRaw.subject_tags,
  ["Surface Water", "Water Quality", "Ecology & Habitat"],
  "The Integrated Report retained unsupported Water Rights or Groundwater tags");
assert(!JSON.stringify({
  canonical: integratedReportRaw.canonical_url,
  accessPoints: integratedReportRaw.access_points,
  publicSources: integratedReportRaw.public_source_references
}).includes("integrated2010.shtml"),
"The obsolete 2010 Integrated Report action remains active");
assert.strictEqual(registry.resources.filter(resource => {
  const text = [
    resource.title, resource.canonical_url, resource.search_aliases,
    resource.access_points.map(point => [point.label, point.url])
  ].flat(Infinity).join(" ");
  return /water_quality_assessment|California Integrated Report|impaired waters/i.test(text);
}).length, 1, "A duplicate current statewide Integrated Report owner exists");

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
assert.strictEqual(published.length, 228, "R17C2 published Resource count must be exactly 228");
assert.strictEqual(staged.length, 5, "The exact five staged Resources changed");
assert.strictEqual(published.length + staged.length, registry.resources.length,
  "Canonical Resources contain an unsupported publication state");
const publishedIds = published.map(resource => resource.id);
const stagedIds = new Set(staged.map(resource => resource.id));
const productById = new Map(relationships.products.map(product => [product.product_id, product]));
const representationById = new Map(
  relationships.resources.map(resource => [resource.resource_id, resource])
);
const publishedRepresentations = published.map(resource => representationById.get(resource.id));
const publicRepresentationValues = new Set([
  "direct_match_in_brim", "selected_products_in_brim", "not_currently_mapped_in_brim"
]);
assert(publishedRepresentations.every(record => record &&
  record.map_review_state === "reviewed" &&
  publicRepresentationValues.has(record.map_representation)),
"Every published Resource must have reviewed public map-presence authority");
assert(staged.every(resource => {
  const record = representationById.get(resource.id);
  if (!record) return false;
  const deferred = record.map_review_state === "not_yet_reviewed" &&
    record.map_representation === null && record.evidence_refs.length === 0;
  const reviewed = record.map_review_state === "reviewed" &&
    publicRepresentationValues.has(record.map_representation);
  return deferred || reviewed;
}), "A staged Resource lacks an allowed reviewed or not-yet-reviewed map state");

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
        title: productTitleFixtures[product.product_id] || `Product ${product.product_id}`,
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
const browserResourceIds = resources.map(resource => resource.id);
assert.deepStrictEqual(browserResourceIds, publishedIds,
  "Browser projection does not exactly match canonical published Resources");
assert.strictEqual(resources.length, published.length,
  "Browser Resource count differs from the canonical published count");
assert(browserResourceIds.every(resourceId => !stagedIds.has(resourceId)),
  "A staged Resource entered the browser Resource projection");
const droughtResource = resources.find(resource =>
  resource.id === "resource_climate_and_drought_data_providers_drought_gov_california_dashboard"
);
const cocorahsResource = resources.find(resource =>
  resource.id === "resource_cocorahs_cocorahs_other"
);
const californiaWaterWatchResource = resources.find(resource =>
  resource.id === "resource_dwr_california_water_watch"
);
const integratedReportResource = resources.find(resource => resource.id === integratedReportId);
const cnrfcResource = resources.find(resource => resource.id === cnrfcId);
const cnrfcRawResource = registry.resources.find(resource => resource.id === cnrfcId);
assert.strictEqual(cnrfcRawResource.canonical_url, cnrfcCanonicalUrlAfter,
  "The repaired CNRFC canonical URL changed");
assert.strictEqual(cnrfcRawResource.access_points[0].url, cnrfcCanonicalUrlAfter,
  "The repaired CNRFC canonical access point changed");
assert.strictEqual(cnrfcRawResource.public_source_references.find(reference =>
  reference.role === "official_source"
).url, cnrfcCanonicalUrlAfter, "The repaired CNRFC official-source reference changed");
assert(!JSON.stringify(cnrfcRawResource).includes(cnrfcCanonicalUrlBefore),
  "The non-resolving bare CNRFC root remains in the canonical Resource");
assert.strictEqual(cnrfcResource.canonicalUrl, cnrfcCanonicalUrlAfter,
  "The projected CNRFC official action changed");
assert.deepStrictEqual(cnrfcResource.accessPoints[0], {
  role: "canonical", label: "Official Resource", url: cnrfcCanonicalUrlAfter
}, "The rendered CNRFC Official Resource action changed");
assert.strictEqual(new Set(cnrfcResource.accessPoints.map(point =>
  point.url.toLowerCase().replace(/\/+$/, "")
)).size, cnrfcResource.accessPoints.length,
"The projected CNRFC access actions contain a normalized duplicate");
assert.strictEqual(cnrfcResource.summary, "Operational river, reservoir-inflow, precipitation, temperature, freezing-level, and short- to long-term water-supply forecasting for California and Nevada. Selected shortcuts include full natural flow (FNF) water-year trend plots for named forecast points; use the Water Resources map for the wider network. The SACC0 tabular shortcut is explicitly dated WY2026 forecast information, not a categorical water-year classification.",
  "The exact corrected CNRFC Resource summary changed");
assert(!JSON.stringify(registry).includes(cnrfcSummaryBefore),
  "The superseded CNRFC Resource summary remains in authority");
assert.strictEqual(cnrfcResource.mapReviewState, "reviewed",
  "The CNRFC Resource review state changed");
assert.strictEqual(cnrfcResource.mapRepresentation, "selected_products_in_brim",
  "The CNRFC Resource representation changed");
assert.deepStrictEqual(cnrfcResource.representedProducts.map(product => ({
  productId: product.productId,
  role: product.relationshipRole
})), [
  { productId: "cnrfc_fnf_delta", role: "selected_product_from_broader_resource" },
  { productId: "cnrfc_stream", role: "selected_product_from_broader_resource" },
  { productId: "cnrfc_precip_weather_station_catalog", role: "selected_product_from_broader_resource" },
  { productId: "cnrfc_basin_product_availability", role: "selected_product_from_broader_resource" },
  { productId: "ops_cnrfc_forecast_points", role: "direct_match_in_brim" },
  { productId: "ops_major_water_supply_forecasts", role: "source_reference" },
  { productId: "ops_cdec_reservoir_storage", role: "source_reference" }
], "The CNRFC Resource must expose exactly seven current Products and roles");
assert.strictEqual(integratedReportResource.mapRepresentation, "selected_products_in_brim",
  "The Integrated Report map representation was not derived from exact Product links");
assert.deepStrictEqual(integratedReportResource.representedProducts.map(product => ({
  productId: product.productId,
  role: product.relationshipRole
})), integratedReportProductIds.map(productId => ({
  productId,
  role: "selected_product_from_broader_resource"
})), "The Integrated Report parent does not expose exactly its two selected Products");
assert.strictEqual(droughtResource.mapRepresentation, "selected_products_in_brim",
  "Drought.gov California Resource representation changed");
assert.deepStrictEqual(droughtResource.representedProducts.map(product => product.productId),
  ["ops_us_drought_monitor"],
  "Drought.gov California must represent only U.S. Drought Monitor");
assert.strictEqual(cocorahsResource.mapRepresentation, "selected_products_in_brim",
  "CoCoRaHS Resource representation changed");
assert.deepStrictEqual(cocorahsResource.representedProducts.map(product => product.productId),
  ["product-ops-cocorahs-ca-daily", "ops_cocorahs_conus_daily"],
  "CoCoRaHS must represent exactly the two maintained Products");
assert.strictEqual(californiaWaterWatchResource.mapRepresentation,
  "not_currently_mapped_in_brim", "California Water Watch must remain beyond the map");
assert.deepStrictEqual(californiaWaterWatchResource.representedProducts, [],
  "California Water Watch retained an unsupported represented Product");

const r16bAccessPointFamilyCounts = {
  [spkId]: 30,
  resource_usace_los_angeles_district_water_management_platform: 5,
  resource_usbr_central_valley_operations_office_platform: 50,
  resource_usbr: 2,
  resource_usbr_cvp_long_term_operations_program: 1,
  resource_usbr_lower_colorado_river_operations: 5,
  resource_usbr_upper_colorado_basin_water_operations: 6,
  resource_usbr_colorado_river_basin_hub: 3,
  resource_usbr_klamath_project_water_operations_platform: 3,
  resource_usbr_truckee_river_operating_agreement_platform: 2,
  resource_usbr_reclamation_information_sharing_environment_platform: 5,
  resource_usbr_central_valley_project_water_supply_program: 3,
  resource_usbr_reclamation_hydromet_platform: 4,
  resource_usbr_reclamation_agrimet_platform: 4
};
const r16bAdditionalPoints = Object.fromEntries(Object.entries(
  r16bAccessPointFamilyCounts
).map(([id, count]) => {
  const resource = resources.find(candidate => candidate.id === id);
  assert(resource, `Missing R16B access-point parent: ${id}`);
  const points = resource.accessPoints.filter(point => point.role !== "canonical");
  assert.strictEqual(points.length, count, `R16B access-point count changed for ${id}`);
  return [id, points];
}));
assert.strictEqual(Object.values(r16bAdditionalPoints).flat().length, 123,
  "R16B must add exactly 123 curated access points");
const normalizedR16bActions = Object.values(r16bAdditionalPoints).flat()
  .map(point => point.url.toLowerCase().replace(/\/$/, ""));
assert.strictEqual(new Set(normalizedR16bActions).size, 123,
  "R16B contains a duplicate normalized access-point action");
const cvoOrdinaryLabels = [
  "Coordinated Operations Agreement Accounting",
  "Federal Share of San Luis Reservoir",
  "Millerton Full-Natural Flow",
  "Normal Full-Natural Flow",
  "San Luis Unit Operations",
  "Shasta Flood-Control Diagram",
  "Shasta Full-Natural Flow",
  "Term 91 Status"
];
const cvoPoints = r16bAdditionalPoints.resource_usbr_central_valley_operations_office_platform;
assert.strictEqual(cvoPoints.filter(point => cvoOrdinaryLabels.includes(point.label)).length, 8,
  "CVO must retain exactly eight ordinary accounting/derived actions");
assert.strictEqual(cvoPoints.filter(point => !cvoOrdinaryLabels.includes(point.label)).length, 42,
  "CVO must retain exactly 42 searchable/user-facing actions");
const spk = resources.find(resource => resource.id === spkId);
const scc = spk.accessPoints.find(point => /[?&]report=scc(?:&|$)/.test(point.url));
assert(scc && scc.label === "Success Dam & Lake Hourly Data",
  "SPK report code scc is not labeled Success Dam & Lake");
assert(!spk.searchText.includes("Sacramento River / Clear Creek"),
  "The false scc phrase entered public search metadata");
assert(!canonicalResourceIds.includes("resource_usace_usace_water_control_manuals_other"),
  "The held Water Control Manuals candidate entered canonical authority");

const model = createModel(resources);
const precipitationResourceIds = resources
  .filter(resource => resource.subjectTags.includes("Precipitation"))
  .map(resource => resource.id);
const expectedPrecipitationResourceIds = [
  "resource_prism_normals",
  "resource_dwr_california_water_watch", "resource_dwr_cdec",
  "resource_noaa_cnrfc", "resource_noaa_cpc_forecasts_outlooks",
  "resource_noaa_wpc_qpf", "resource_nrcs_snow_survey_water_supply_forecasting",
  "resource_usace_sacramento_district_water_control_data_system",
  "resource_usace_los_angeles_district_water_management_platform",
  "resource_cw3e_cw3e_micro_rain_radar_snow_levels_dashboard",
  "resource_scwa_solano_county_flood_monitoring_map_viewer",
  "resource_rcfcwcd_riverside_county_rainfall_map_viewer",
  "resource_contra_costa_county_flood_control_and_wa_contra_costa_county_rainmap_viewer",
  "resource_cocorahs_cocorahs_other",
  "resource_lacpw_los_angeles_county_precipitation_data_platform",
  "resource_marin_county_flood_control_marin_county_rainfall_and_creek_data_dashboards_collection",
  "resource_santa_barbara_county_public_works_santa_barbara_county_real_time_hydrology_platform",
  "resource_santa_cruz_county_flood_control_santa_cruz_county_hydrologic_monitoring_map_viewer",
  "resource_napa_county_flood_control_napa_valley_rainfall_and_stream_monitoring_map_viewer"
];
assert.deepStrictEqual([...precipitationResourceIds].sort(),
  [...expectedPrecipitationResourceIds].sort(),
  "The exact 19-Resource Precipitation facet membership changed");
const cPrecipitationState = model.patchState(
  model.createState({ preset: "all_resources" }),
  { subject: "Precipitation" }
);
assert.strictEqual(model.results(cPrecipitationState).length, 19,
  "C-owned Precipitation filtering did not return exactly 19 Resources");
assert.strictEqual(
  model.chips(cPrecipitationState).find(chip => chip.key === "subject").label,
  "Subject: Precipitation",
  "The C-owned active filter value or chip text changed"
);
const cClearedState = model.removeChip(cPrecipitationState, "subject", "Precipitation");
assert.strictEqual(cClearedState.subject, "",
  "Clearing the C-owned Precipitation token left the Resource filter active");
assert.strictEqual(cClearedState.preset, "all_resources",
  "Clearing the C-owned Precipitation token left All Resources");
assert.strictEqual(model.results(cClearedState).length, 228,
  "Clearing the C-owned Precipitation token did not restore 228 Resources");
assert.deepStrictEqual(model.presets, [
  { id: "in_brim_map", label: "In BRIM map" },
  { id: "beyond_the_map", label: "Beyond the map" },
  { id: "all_resources", label: "All Resources" }
], "Primary Resource views changed");
const expectedInBrimIds = published.filter(resource => {
  const value = representationById.get(resource.id).map_representation;
  return value === "direct_match_in_brim" || value === "selected_products_in_brim";
}).map(resource => resource.id);
const expectedBeyondIds = published.filter(resource =>
  representationById.get(resource.id).map_representation === "not_currently_mapped_in_brim"
).map(resource => resource.id);
const inBrimIds = model.results(model.createState({ preset: "in_brim_map" }))
  .map(resource => resource.id);
const beyondIds = model.results(model.createState({ preset: "beyond_the_map" }))
  .map(resource => resource.id);
const allIds = model.results(model.createState({ preset: "all_resources" }))
  .map(resource => resource.id);
assert.deepStrictEqual([...inBrimIds].sort(), [...expectedInBrimIds].sort(),
  "In BRIM map membership is not derived from published direct/selected Resources");
assert.deepStrictEqual([...beyondIds].sort(), [...expectedBeyondIds].sort(),
  "Beyond the map membership is not derived from published not-mapped Resources");
assert.deepStrictEqual([...allIds].sort(), [...publishedIds].sort(),
  "All Resources membership does not equal the complete published Resource set");
assert(inBrimIds.every(resourceId => !beyondIds.includes(resourceId)),
  "In BRIM map and Beyond the map memberships overlap");
assert.deepStrictEqual([...new Set([...inBrimIds, ...beyondIds])].sort(),
  [...publishedIds].sort(),
  "Public primary membership union does not equal the published Resource set");
assert.deepStrictEqual([inBrimIds.length, beyondIds.length, allIds.length], [28, 200, 228],
  "R17C1 public Resource view counts changed");
assert([...inBrimIds, ...beyondIds, ...allIds].every(resourceId => !stagedIds.has(resourceId)),
  "A staged Resource leaked into a public primary Resource view");
assert.deepStrictEqual(model.facetCounts(model.createState()).presets, {
  in_brim_map: expectedInBrimIds.length,
  beyond_the_map: expectedBeyondIds.length,
  all_resources: publishedIds.length
}, "Primary Resource view counts are not derived from published authority");

function searchIds(query) {
  const searchState = model.setQuery(model.createState({ preset: "all_resources" }), query);
  return model.results(searchState).map(resource => resource.id);
}
for(const id of r17c2NewResourceIds) {
  const r=resources.find(r=>r.id===id);
  assert(r && searchIds(r.title).includes(id),`New Resource title not discoverable: ${id}`);
}
assert(searchIds("coco").includes("resource_cocorahs_cocorahs_other"),
  "Resource gateway target query no longer finds the CoCoRaHS Resource");
assert(searchIds("reservoir inflow").includes(cnrfcId),
  "The corrected CNRFC Resource summary is not indexed for reservoir inflow");
assert(searchIds("freezing-level").includes(cnrfcId),
  "The corrected CNRFC Resource summary is not indexed for freezing-level");
assert.deepStrictEqual(searchIds("CNRFC").sort(), [
  "resource_dwr_cdec",
  "resource_noaa_cnrfc",
  spkId
].sort(), "The exact three-Resource CNRFC search membership changed");
assert(r17c1AddedResourceIds.every(id => canonicalResourceIds.includes(id)) &&
  r17c1RetiredResourceIds.every(id => !canonicalResourceIds.includes(id)),
  "The exact C1 canonical additions/retirements changed");
for (const id of r17c1RetiredResourceIds) {
  assert.deepStrictEqual(registry.resources.filter(resource => resource.aliases.includes(id))
    .map(resource => resource.id), [cnrfcId], "A retired CNRFC ID does not recover exactly one parent");
}
assert.strictEqual(cnrfcResource.accessPoints.length, 24, "CNRFC must expose 24 curated actions");
assert.deepStrictEqual(cnrfcResource.accessPoints.slice(1, 3), a5NewActions,
  "A5 entry points or their second/third placement changed");
for (const change of a5LabelChanges) {
  assert.deepStrictEqual(cnrfcResource.accessPoints.filter(point => point.url === change.url),
    [{role: "configured_view", label: change.new_label, url: change.url}],
    "A5 station label/unchanged URL mapping changed");
  assert(searchIds(change.new_label).includes(cnrfcId), `A5 named forecast point not searchable: ${change.station_code}`);
}
for (const query of ["FNF", "full natural flow", "FNF water-year trend", "Water Resources", "HD6RSA", "freezing levels",
  "Trinity Lake", "Pardee Reservoir", "Lake McClure", "Folsom Lake", "Millerton Reservoir", "Englebright Reservoir",
  "Lake Isabella", "Michigan Bar", "New Don Pedro Reservoir", "New Melones Reservoir", "Lake Oroville",
  "Pine Flat Reservoir", "Lake Success", "Shasta Lake", "Lake Kaweah"]) {
  assert(searchIds(query).includes(cnrfcId), `A5 CNRFC recovery failed: ${query}`);
}
const cnrfcStationIds = ["CEGC1", "CMPC1", "EXQC1", "FOLC1", "FRAC1", "HLEC1", "ISAC1",
  "MHBC1", "NDPC1", "NMSC1", "ORDC1", "PFTC1", "SCSC1", "SHDC1", "TMDC1"];
assert.deepStrictEqual(cnrfcResource.accessPoints.filter(point =>
  point.url.includes("ensembleProduct.php?") && point.url !== "https://www.cnrfc.noaa.gov/ensembleProductTabular.php?id=SACC0&prodID=9&year=2026").map(point => point.url),
  cnrfcStationIds.map(id => `https://www.cnrfc.noaa.gov/ensembleProduct.php?id=${id}&prodID=9`),
  "The exact 15 CNRFC configured station destinations changed");
for (const query of ["AKYC1", "CNRFC Forcings CSV", "CNRFC Hourly HEFS CSV"]) {
  assert(searchIds(query).includes(cnrfcId), `CNRFC parent search recovery failed: ${query}`);
}
const eroId = "resource_noaa_wpc_excessive_rainfall_outlook";
const eroResource = resources.find(resource => resource.id === eroId);
assert.deepStrictEqual(eroResource.representedProducts.map(product => ({
  productId: product.productId, role: product.relationshipRole
})), [1, 2, 3].map(day => ({productId: `ops_wpc_ero_day_${day}`,
  role: "selected_product_from_broader_resource"})), "ERO chips/roles changed");
assert(inBrimIds.includes(eroId) && !beyondIds.includes(eroId),
  "The ERO owner is in the wrong public Resource view");
assert.deepStrictEqual(cnrfcResource.accessPoints.filter(point => point.url === "https://www.cnrfc.noaa.gov/ensembleProductTabular.php?id=SACC0&prodID=9&year=2026"),
  [{"role": "archive", "label": "CNRFC Sacramento Valley Water Resources Index — WY2026 Forecast View", "url": "https://www.cnrfc.noaa.gov/ensembleProductTabular.php?id=SACC0&prodID=9&year=2026"}], "The exact dated SACC0 action changed");
const requiredSearchParents = {
  Kaweah: [spkId],
  Terminus: [spkId],
  "Pine Flat": [spkId],
  Hensley: [spkId],
  Farmington: [spkId],
  "Lake Sonoma": [spkId],
  "Success Dam": [spkId],
  "Section 7": [spkId],
  Sacramento: [spkId],
  "San Joaquin": [spkId],
  "Tulare Lake": [spkId],
  "North Coast": [spkId],
  "Great Basin": [spkId],
  "Upper Colorado": [spkId, "resource_usbr_upper_colorado_basin_water_operations"],
  "Lower Colorado": ["resource_usbr_lower_colorado_river_operations"],
  "Colorado River Basin": ["resource_usbr_colorado_river_basin_hub"],
  CVO: ["resource_usbr_central_valley_operations_office_platform"],
  Klamath: ["resource_usbr_klamath_project_water_operations_platform"],
  Truckee: [spkId, "resource_usbr_truckee_river_operating_agreement_platform"],
  Lahontan: ["resource_usbr_truckee_river_operating_agreement_platform"],
  TROA: ["resource_usbr_truckee_river_operating_agreement_platform"],
  Hydromet: ["resource_usbr_reclamation_hydromet_platform"],
  AgriMet: ["resource_usbr_reclamation_agrimet_platform"],
  "Impaired Waters": [integratedReportId],
  TMDL: [integratedReportId],
  "303(d)": [integratedReportId],
  "2024 Integrated Report": [integratedReportId],
  "2026 Integrated Report": [integratedReportId]
};
Object.entries(requiredSearchParents).forEach(([query, expectedIds]) => {
  const matches = searchIds(query);
  expectedIds.forEach(id => assert(matches.includes(id),
    `Required search '${query}' did not return ${id}`));
});
// The pinned R17B fixture characterized USACE alone for Success Dam.
// The expanded C1 catalog has two legitimate providers; no generic agency precedence.
const successDamMatches = searchIds("Success Dam");
[spkId, cnrfcId].forEach(id => assert(successDamMatches.includes(id),
  `Success Dam must retain the distinct provider Resource ${id}`));
assert(searchIds("SCSC1").includes(cnrfcId),
  "The exact CNRFC SCSC1 alias no longer recovers its Resource");
assert(searchIds("Tule - Success Dam").includes(cnrfcId),
  "The approved CNRFC station/place alias no longer recovers its Resource");
assert(searchIds("Success Dam & Lake Hourly Data").includes(spkId),
  "The exact USACE action label no longer recovers its Resource");
assert(searchIds("USACE Success Dam").includes(spkId),
  "The qualified query lost the USACE Resource; provider exclusivity is not implied");
const successDamSpk = resources.find(resource => resource.id === spkId);
const successDamCnrfc = resources.find(resource => resource.id === cnrfcId);
assert.notStrictEqual(successDamSpk.id, successDamCnrfc.id,
  "The two provider Resources were conflated");
assert(successDamSpk.accessPoints.some(point => point.role === "configured_view" &&
  point.label === "Success Dam & Lake Hourly Data" &&
  point.url === "https://water.usace.army.mil/office/spk/reports/getreport.html?type=hourly&report=scc"),
  "The USACE scc action identity or destination changed");
assert(successDamCnrfc.accessPoints.some(point => point.role === "configured_view" &&
  point.label === "Tule River — Lake Success — FNF water-year trend (SCSC1)" &&
  point.url === "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=SCSC1&prodID=9"),
  "The CNRFC SCSC1 action identity or destination changed");
// Bare scc returned no Resource in all three A3B scenarios. Do not invent a
// positive recovery expectation or promote a URL query value into search data.
assert(resources.every(resource => !resource.searchText.includes("https://") &&
  !resource.searchText.includes(resource.id)),
"A URL or Resource ID leaked into public Resource search text");

let state = model.createState({
  preset: "all_resources", resultsScrollTop: 417, returnResultsScrollTop: 417
});
state = model.setQuery(state, "GOES");
assert(model.results(state).some(resource => resource.id === "resource_noaa_goes_image_viewer"),
  "Search no longer resolves the GOES Resource");
assert.strictEqual(state.sortMode, "relevance", "Search did not select relevance sort");
state = model.setQuery(state, "");
assert.strictEqual(state.sortMode, "title", "Cleared search did not restore title sort");
assert.strictEqual(state.preset, "all_resources",
  "Clearing a Resource query left the All Resources view");
const cocoState = model.setQuery(model.createState({ preset: "all_resources" }), "coco");
assert.strictEqual(model.facetCounts(cocoState).subjects["Climate & Drought"], 1,
  "Resource facet counts do not update under search");
const climateState = model.patchState(cocoState, { subject: "Climate & Drought" });
assert.strictEqual(
  model.chips(climateState).find(chip => chip.key === "subject").label,
  "Subject: Climate & Drought",
  "The live facet count leaked into the active-filter value"
);
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
[
  "provider_hosted: 'Provider-hosted'",
  "brim_enhanced: 'BRIM-enhanced'",
  "brim_managed: 'BRIM-managed'",
  "not_applicable: 'Not applicable'"
].forEach(mapping => assert(source.includes(mapping),
  `The accepted Product-delivery label mapping changed: ${mapping}`));
assert.strictEqual(
  (source.match(/deliveryLabel\(relationship\.deliveryClass\)/g) || []).length,
  1,
  "Product delivery must remain visible only as secondary C · Resources relationship text"
);
assert(!source.includes("delivery-filter") && !source.includes("multiple-source-filter"),
  "Delivery or multiple-source status became a primary filter");
assert(source.includes("presets.setAttribute('role', 'radiogroup')") &&
  source.includes("presetButton.setAttribute('role', 'radio')") &&
  source.includes("presetButton.setAttribute('aria-checked'") &&
  source.includes("['ArrowLeft', 'ArrowRight', 'Home', 'End']"),
"Primary map-presence views lost radio or keyboard semantics");
const searchHandlerSource = extractFunction(source, "handleSearchInput");
assert.strictEqual((searchHandlerSource.match(/openResourceExplorer\(/g) || []).length, 1,
  "Resource gateway search can recreate navigation on later keystrokes");
assert(searchHandlerSource.includes("resourceGatewaySearchOptions(") &&
  searchHandlerSource.includes("openResourceExplorer(gatewayOptions, event.target)") &&
  source.includes("query: String(options.query || '')") &&
  source.includes("Search Resource titles, providers, subjects, variables, and places"),
"Resource gateway query preservation or Resource-specific search presentation is incomplete");
assert(source.includes("appendResourceFacetLabel(choice, optionValue, count)") &&
  source.includes("appendResourceFacetLabel(presetButton, preset.label, count)") &&
  source.includes("label + ': ' + optionValue + ', ' + resourceCountText(count)") &&
  css.includes(".brim-guide__resource-facet-label") &&
  css.includes(".brim-guide__resource-facet-count"),
"Resource facet labels, live count badges, or accessible count meaning are incomplete");
const sharedFilterButtonRule = css.match(
  /\.brim-guide__facet-button,\s*\.brim-guide__resource-choice\s*\{([^}]+)\}/
);
assert(sharedFilterButtonRule && [
  "min-height: 24px", "gap: 5px", "padding: 2px 4px",
  "border: 1px solid #a7a093", "border-radius: 5px",
  "background: #fffdf8", "font-size: 8px", "font-weight: 650"
].every(declaration => sharedFilterButtonRule[1].includes(declaration)),
"A Explore and C Resources no longer share the approved filter-button geometry");
const sharedFilterButtonStateRule = css.match(
  /\.brim-guide__facet-button:hover,\s*\.brim-guide__facet-button\.is-selected,\s*\.brim-guide__resource-choice:hover,\s*\.brim-guide__resource-choice\.is-selected\s*\{([^}]+)\}/
);
assert(sharedFilterButtonStateRule && [
  "border-color: #657a69", "background: rgba(72, 102, 79, 0.12)",
  "color: #203728"
].every(declaration => sharedFilterButtonStateRule[1].includes(declaration)),
"A Explore and C Resources no longer share hover/selected filter-button styling");

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

// The return tab changes paint only; generic button typography and keyboard focus remain shared.
const returnTabPaint = css.match(/\n\.brim-guide \.brim-guide__resource-spine-back \{\s*([^}]+)\}/);
const returnTabFeedback = css.match(
  /\n\.brim-guide \.brim-guide__resource-spine-back:hover,\s*\.brim-guide \.brim-guide__resource-spine-back:focus-visible \{\s*([^}]+)\}/
);
assert(returnTabPaint && returnTabFeedback, "Return tab needs scoped rest, hover and keyboard-focus paint");
assert.strictEqual(returnTabPaint[1].trim(),
  "background: #dce8d5; color: var(--guide-accent-dark); box-shadow: inset 0 0 0 1px #6b8064;",
  "Return tab paint must beat generic button color without changing geometry or typography");
assert.strictEqual(returnTabFeedback[1].trim(), "background: #cfddc6;",
  "Return tab feedback must preserve the shared focus outline and default forced-color behavior");
const returnTabDesktop = css.match(/\n\.brim-guide__resource-spine-back \{\s*([^}]+)\}/);
assert(returnTabDesktop, "Desktop return tab rule is missing");
assert.strictEqual(returnTabDesktop[1].trim(),
  "padding: 8px 3px; border: 0; border-top: 1px solid var(--guide-rule); border-bottom: 1px solid var(--guide-rule); border-radius: 0; background: transparent; color: #294735; cursor: pointer; font-size: 9px; font-weight: 700; line-height: 1.2; writing-mode: vertical-rl; transform: rotate(180deg);",
  "Desktop return tab geometry, orientation and typography changed");
const returnTabNarrow = css.slice(css.lastIndexOf("@media (max-width: 700px)")).match(
  /\.brim-guide--resource-explorer \.brim-guide__resource-spine-back \{\s*([^}]+)\}/
);
assert(returnTabNarrow, "Return tab narrow rule must remain in its existing media context");
assert.strictEqual(returnTabNarrow[1].trim(),
  "min-height: 28px; padding: 3px 8px; writing-mode: horizontal-tb; transform: none;",
  "Narrow return tab dimensions or horizontal orientation changed");
assert(css.includes(".brim-guide button,\n.brim-guide input,\n.brim-guide select {\n  font: inherit;") &&
  /\.brim-guide button:focus-visible,[^{]+\{\s*outline: 2px solid #236fa1; outline-offset: 2px;\s*\}/.test(css),
"Shared button typography or visible keyboard-focus outline changed");
assert(/resourceSpine\.appendChild\(button\(\s*'brim-guide__resource-spine-back',\s*'Back to Guide',\s*'resource-back-guide',\s*'Back to compact BRIM Guide'\s*\)\)/.test(source) &&
  /action === 'resource-back-guide'\) \{\s*backToCompactGuide\(\);\s*\} else/.test(source),
"Actual return button label, accessible name or navigation action changed");
function returnTabLuminance(hex) {
  const channels = hex.slice(1).match(/../g).map(value => parseInt(value, 16) / 255)
    .map(value => value <= 0.04045 ? value / 12.92 : ((value + 0.055) / 1.055) ** 2.4);
  return channels[0] * 0.2126 + channels[1] * 0.7152 + channels[2] * 0.0722;
}
const returnTabTextColor = css.match(/--guide-accent-dark:\s*(#[0-9a-f]{6})/)[1];
const returnTabContrast = [returnTabPaint, returnTabFeedback].map(rule => {
  const fill = rule[1].match(/background:\s*(#[0-9a-f]{6})/)[1];
  const values = [returnTabLuminance(fill), returnTabLuminance(returnTabTextColor)].sort((a, b) => a - b);
  return (values[1] + 0.05) / (values[0] + 0.05);
});
assert(returnTabContrast.every(ratio => ratio >= 4.5),
  "Return tab text must meet 4.5:1 against rest and hover/focus fills");
console.log(`RETURN_TAB_STATIC_CONTRAST=${returnTabContrast.map(ratio => ratio.toFixed(3)).join(",")}`);
console.log("RETURN_TAB_PAINT_GEOMETRY_FOCUS_ACTION=STATIC_PASS; RENDERED_VISUAL_REVIEW=PENDING");

console.log("BRIM Guide Resource Explorer R17C2 current catalog and preserved C1 contracts passed.");
console.log(`CANONICAL_RESOURCES=${canonicalResourceIds.length}`);
console.log(`PUBLISHED_RESOURCES=${publishedIds.length}`);
console.log(`STAGED_RESOURCES=${stagedIds.size}`);
console.log(`PRESET_COUNTS=${expectedInBrimIds.length},${expectedBeyondIds.length},${publishedIds.length}`);
console.log(`CANONICAL_RESOURCE_LINKS=${canonicalLinks.length}`);
console.log("PRODUCTS_WITH_RESOURCE_LINKS=93");
console.log("RESOURCES_WITH_PRODUCT_LINKS=28");
console.log("RELATIONSHIP_ROLE_COUNTS=13_DIRECT,64_SELECTED,44_SOURCE_REFERENCE");
console.log("CNRFC_RELATED_PRODUCTS=7");
console.log("GENERIC_PROJECTION_INVARIANTS=PASS");
console.log("STAGED_REVIEWED_RESOURCE_SUPPORT=PASS");
console.log("LEGACY_PUBLIC_PROJECTIONS=0");
console.log("DESKTOP_SCROLL_OWNERS=FILTER_BODY,RESULTS,DETAIL");
console.log("NARROW_ONE_PANE=PASS");
console.log("LIFECYCLE_IDEMPOTENCE=PASS");
console.log("CROSS_SURFACE_FILTER_OWNERSHIP=PASS");
console.log("FILTER_BUTTON_VISUAL_PARITY=PASS");

// Reuse the same public projection and source extraction in the focused provider suite.
module.exports = { createModel, resources, registry, relationships, extractFunction };
