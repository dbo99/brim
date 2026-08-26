#!/usr/bin/env node

"use strict";

// Focused source-only contract tests for M04 Layer Explorer. This test reads
// authored source and the descriptive catalog only. It does not build BRIM,
// access a network, execute a preprocessor, or create map layers.

const fs = require("fs");
const path = require("path");
const assert = require("assert");

const root = path.resolve(__dirname, "..");
const panelPath = path.join(root, "03_functions", "js", "leaflet_tools_adddata_panel.js");
const helperPath = path.join(root, "03_functions", "leaflet_tools_adddata_helpers.r");
const catalogPath = path.join(root, "08_docs", "catalog", "BRIM_LAYER_CATALOG.csv");

const panelSource = fs.readFileSync(panelPath, "utf8");
const helperSource = fs.readFileSync(helperPath, "utf8");
const catalogText = fs.readFileSync(catalogPath, "utf8");

// Confirm the complete htmlwidgets callback remains valid JavaScript in its
// intended function-expression context before extracting its pure model.
const panelCallback = new Function(`return (${panelSource})`)();
assert.strictEqual(typeof panelCallback, "function", "Tools onRender callback did not compile");

function extractFunction(source, functionName) {
  const start = source.indexOf(`function ${functionName}`);
  assert(start >= 0, `Could not locate ${functionName}`);

  const firstBrace = source.indexOf("{", start);
  assert(firstBrace >= 0, `Could not locate ${functionName} body`);

  let depth = 0;
  let quote = null;
  let escaped = false;
  let lineComment = false;
  let blockComment = false;

  for (let index = firstBrace; index < source.length; index += 1) {
    const char = source[index];
    const next = source[index + 1];

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
      if (escaped) {
        escaped = false;
      } else if (char === "\\") {
        escaped = true;
      } else if (char === quote) {
        quote = null;
      }
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
      if (depth === 0) return source.slice(start, index + 1);
    }
  }

  throw new Error(`Could not close ${functionName}`);
}

function parseCsv(text) {
  const rows = [];
  let row = [];
  let field = "";
  let quoted = false;

  for (let index = 0; index < text.length; index += 1) {
    const char = text[index];
    const next = text[index + 1];

    if (quoted) {
      if (char === "\"" && next === "\"") {
        field += "\"";
        index += 1;
      } else if (char === "\"") {
        quoted = false;
      } else {
        field += char;
      }
      continue;
    }

    if (char === "\"") {
      quoted = true;
    } else if (char === ",") {
      row.push(field);
      field = "";
    } else if (char === "\n") {
      row.push(field);
      rows.push(row);
      row = [];
      field = "";
    } else if (char !== "\r") {
      field += char;
    }
  }

  if (field.length || row.length) {
    row.push(field);
    rows.push(row);
  }
  return rows;
}

const csvRows = parseCsv(catalogText);
assert.strictEqual(csvRows.length, 27, "Catalog must contain one header and 26 records");
csvRows.forEach((row, index) => {
  assert.strictEqual(row.length, 15, `Catalog CSV row ${index + 1} does not have 15 fields`);
});

const header = csvRows[0];
const catalog = csvRows.slice(1).map(row => Object.fromEntries(
  header.map((name, index) => [name, row[index]])
));
const expectedIds = catalog.map(record => record.candidate_stable_id);
assert.strictEqual(new Set(expectedIds).size, 26, "Catalog stable IDs are duplicated");

function projectRecord(record) {
  const tokens = record.architecture.split(";");
  const architecture = tokens.includes("LOCAL") ? "Local" :
    tokens.includes("EXTERNAL") ? "External" :
      tokens.includes("OPS_LIVE") ? "Ops Live" : "UNKNOWN";
  const implementation = tokens.includes("CUSTOM_CONTROLLER") ? "Custom controller" :
    tokens.includes("CUSTOM_LOADER") ? "Custom loader" :
      tokens.includes("SHARED_CONTROLLER") ? "Shared controller" : "Ordinary Leaflet";
  const rendererToken = tokens.find(token => token.endsWith("_RENDERER")) || "UNKNOWN";

  return {
    stableId: record.candidate_stable_id,
    displayName: record.runtime_display_name,
    architecture,
    catalogPath: record.catalog_path,
    implementation,
    renderer: rendererToken,
    domainReview: record.domain_review_fields === "NOT_APPLICABLE" ?
      "NOT_APPLICABLE" : "DOMAIN_REVIEW_REQUIRED",
    confidence: record.confidence
  };
}

const modelFactorySource = extractFunction(panelSource, "ptCreateLayerExplorerReadOnlyModel");
const createModel = new Function(`return (${modelFactorySource})`)();
const payload = {
  coverage: "PARTIAL",
  catalogAuthority: "DESCRIPTIVE_ONLY",
  uiConsumption: "READ_ONLY_METADATA",
  runtimeControl: "NONE",
  runtimeAuthority: "UNCHANGED",
  records: catalog.map(projectRecord)
};
const model = createModel(payload);

assert(Object.isFrozen(model), "Layer Explorer model is not immutable");
assert(Object.isFrozen(model.records), "Layer Explorer record collection is not immutable");
assert.strictEqual(model.coverage, "PARTIAL", "Partial-catalog state was lost");
assert.strictEqual(model.catalogAuthority, "DESCRIPTIVE_ONLY", "Catalog authority changed");
assert.strictEqual(model.uiConsumption, "READ_ONLY_METADATA", "UI consumption changed");
assert.strictEqual(model.runtimeControl, "NONE", "Runtime control is not NONE");
assert.strictEqual(model.runtimeAuthority, "UNCHANGED", "Runtime authority changed");
assert.strictEqual(model.records.length, 26, "Exactly 26 records were not embedded");
assert.deepStrictEqual(model.records.map(record => record.stableId), expectedIds,
  "Stable IDs or deterministic source order changed");

assert.deepStrictEqual(
  model.filterRecords("huc12", "ALL").map(record => record.stableId),
  ["huc12"],
  "Stable-ID search failed"
);
assert.deepStrictEqual(
  model.filterRecords("drought monitor", "ALL").map(record => record.stableId),
  ["ops_u_s_drought_monitor"],
  "Display-name search failed"
);

for (const architecture of ["Local", "External", "Ops Live"]) {
  const expected = payload.records
    .filter(record => record.architecture === architecture)
    .map(record => record.stableId);
  const actual = model.filterRecords("", architecture).map(record => record.stableId);
  assert.deepStrictEqual(actual, expected, `${architecture} filtering changed deterministic order`);
}
assert.deepStrictEqual(
  model.filterRecords("water", "Local").map(record => record.stableId),
  payload.records
    .filter(record => record.architecture === "Local")
    .filter(record => `${record.displayName} ${record.stableId}`.toLowerCase().includes("water"))
    .map(record => record.stableId),
  "Combined search and architecture filter failed"
);

const modalSource = extractFunction(panelSource, "ptOpenLayerExplorer");
assert(!modalSource.includes("window.BRIM_GUIDE"),
  "Retained descriptive modal must not delegate to BRIM Guide");
assert(modalSource.includes("Partial catalog: all 26 currently cataloged records are shown"),
  "User-visible partial-catalog notice is missing");
assert(modalSource.includes("data-pt-layer-explorer-id"), "Record selection control is missing");
assert(modalSource.includes("pt-layer-explorer-search"), "Search control is missing");
assert(modalSource.includes("pt-layer-explorer-architecture"), "Architecture filter is missing");
assert(modalSource.includes("renderDetail(selectedRecord)"), "Selected-record detail rendering is missing");
assert(!modalSource.includes("innerHTML"), "Catalog metadata must not be rendered with innerHTML");
const fallbackSource = modalSource.slice(modalSource.indexOf("var old ="));
assert(!/(map\.|addLayer|removeLayer|dispatchEvent|fetch\(|XMLHttpRequest|ptClear|ptToggle|BRIM_)/.test(fallbackSource),
  "Layer Explorer modal contains a map, controller, clear/reset, or network hook");

assert(!panelSource.includes('id="pt-layer-explorer-btn"'),
  "External Layers retains the retired Guide entry button");
assert(!panelSource.includes("ptBind('pt-layer-explorer-btn'"),
  "External Layers retains the retired Guide entry handler");
assert(!panelSource.includes('>BRIM Guide</button>'),
  "External Layers retains a duplicate primary Guide entry");
assert(helperSource.includes('layer_explorer = layer_explorer_metadata'),
  "Build-time metadata is not injected through the existing Tools data payload");
assert.strictEqual((helperSource.match(/BRIM_LAYER_CATALOG\.csv/g) || []).length, 1,
  "Catalog filename must occur only in the approved build-time adapter");
assert(!/fetch\([^)]*BRIM_LAYER_CATALOG|XMLHttpRequest[^]*BRIM_LAYER_CATALOG/.test(panelSource),
  "Browser source attempts to fetch the descriptive catalog CSV");

// Shared legend/resource fields intentionally never enter the UI projection;
// the explorer imposes no one-resource-per-layer uniqueness constraint.
assert(!modelFactorySource.includes("legendAuthority"),
  "Layer Explorer model unexpectedly clones legend authority metadata");
assert(!modelFactorySource.includes("notesOrResources"),
  "Layer Explorer model unexpectedly clones shared resource metadata");

console.log("Layer Explorer source/model contract tests passed.");
console.log("EMBEDDED_RECORDS=26");
console.log("PARTIAL_CATALOG=EXPLICIT");
console.log("CATALOG_UI_CONSUMPTION=READ_ONLY_METADATA");
console.log("CATALOG_RUNTIME_CONTROL=NONE");
console.log("CATALOG_NETWORK_FETCH=NONE");
