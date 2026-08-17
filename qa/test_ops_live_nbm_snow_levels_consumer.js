const assert = require("assert");
const crypto = require("crypto");
const fs = require("fs");
const path = require("path");

const repoRoot = path.join(__dirname, "..");
const helperSource = fs.readFileSync(
  path.join(repoRoot, "03_functions", "leaflet_ops_live_nbm_snow_levels_helpers.r"),
  "utf8"
);
const panelSource = fs.readFileSync(
  path.join(repoRoot, "03_functions", "leaflet_ops_live_panel_helpers.r"),
  "utf8"
);
const configSource = fs.readFileSync(
  path.join(repoRoot, "00_config", "config_map_display.r"),
  "utf8"
);

function embeddedRawJs(source) {
  const startToken = 'r"---(';
  const endToken = ')---"';
  const start = source.indexOf(startToken);
  const end = source.lastIndexOf(endToken);
  assert(start >= 0 && end > start, "embedded R raw JavaScript was not found");
  return source.slice(start + startToken.length, end);
}

function includesAll(source, values, label) {
  values.forEach((value) => {
    assert(source.includes(value), `${label} is missing: ${value}`);
  });
}

includesAll(
  helperSource,
  [
    "var PT_SNOW_TARGET_CACHE_LIMIT = 4",
    "cache: 'no-store'",
    "cache: 'force-cache'",
    "window.crypto.subtle.digest('SHA-256'",
    "cycle_utc: entry.cycle_time_utc",
    "valid_time_utc: entry.valid_time_utc",
    "lead_hours: entry.lead_hours",
    "getSelectionState: function()",
    "stepValidTime: function(delta)",
    "brim:nbm-time-selection",
    "panelOrder: -100",
    'data-pt-ops-action="nbm-snow-labels"',
    'data-pt-ops-action="nbm-snow-card"',
    "Snow level (ft MSL)",
    "Fixed scale",
    "America/Los_Angeles",
    "unifiedCard: true"
  ],
  "NBM Snow Levels source contract"
);
includesAll(
  panelSource,
  [
    "Number(a.def.panelOrder)",
    'data-pt-ops-action="nbm-snow-labels"',
    'data-pt-ops-action="nbm-snow-card"'
  ],
  "Ops panel integration"
);
assert(
  panelSource.indexOf("panelOrder") < panelSource.indexOf("return a.idx - b.idx"),
  "explicit subgroup placement must be evaluated before insertion order"
);
includesAll(
  configSource,
  [
    "add_ops_nbm_snow_levels = TRUE",
    'data/winter-storm-levels/winter_storm_levels_manifest.json'
  ],
  "Snow Levels configuration"
);

const documentListeners = new Map();
const documentStub = {
  visibilityState: "visible",
  head: {appendChild() {}},
  createElement() {
    return {id: "", textContent: "", style: {}};
  },
  getElementById() {
    return null;
  },
  querySelectorAll() {
    return [];
  },
  addEventListener(name, handler) {
    documentListeners.set(name, handler);
  },
  removeEventListener(name, handler) {
    if (documentListeners.get(name) === handler) documentListeners.delete(name);
  }
};

function extendLayer(methods) {
  function Layer(options) {
    if (typeof methods.initialize === "function") methods.initialize.call(this, options);
  }
  Layer.prototype = Object.assign({constructor: Layer}, methods);
  return Layer;
}

function emptyLayerGroup() {
  return {
    addTo() {
      return this;
    },
    clearLayers() {},
    addLayer() {}
  };
}

const windowEvents = [];
const windowStub = {
  BRIM: {},
  CustomEvent: class CustomEvent {
    constructor(type, options) {
      this.type = type;
      this.detail = options && options.detail;
    }
  },
  dispatchEvent(event) {
    windowEvents.push(event);
    return true;
  },
  performance: {now: () => 0},
  setInterval: () => 101,
  clearInterval() {},
  requestAnimationFrame(callback) {
    callback();
  }
};
const LStub = {
  Layer: {extend: extendLayer},
  layerGroup: emptyLayerGroup
};

const api = new Function(
  "window",
  "document",
  "L",
  `
    var includeNbmSnowLevels = false;
    var NBM_SNOW_LEVELS_MANIFEST_URL = '';
    var activeLegendDefs = {};
    function redrawLegend() {}
    function setOpsLayerLoading() {}
    function recordStatus() {}
    function escapeHtml(value) {
      return String(value).replace(/[&<>"']/g, function(character) {
        return ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'})[character];
      });
    }
    ${embeddedRawJs(helperSource)}
    return {
      validateManifest: ptSnowValidateManifest,
      validateGeoJson: ptSnowValidateGeoJson,
      freshness: ptSnowFreshness,
      defaultTargetIndex: ptSnowDefaultTargetIndex,
      closestTargetIndex: ptSnowClosestTargetIndex,
      formatPacific: ptSnowFormatPacific,
      formatPacificCompact: function(value) { return ptSnowFormatPacific(value, true); },
      utcHour: ptSnowUtcHour,
      color: ptSnowLevelColor,
      gradient: ptSnowLegendGradient,
      featureHtml: ptSnowFeatureHtml,
      cacheGet: ptSnowCacheGet,
      cacheSet: ptSnowCacheSet,
      cacheClear: ptSnowCacheClear,
      cacheSize: function() { return ptSnowTargetCache.size; },
      Layer: PtOpsNbmSnowLevelsLayer,
      activeLegendDefs: activeLegendDefs
    };
  `
)(windowStub, documentStub, LStub);

const leads = [1, 6, 12, 18, 24, 30, 36, 42, 48, 60, 72];
const currentCycle = "2026-01-15T12:00:00Z";
const previousCycle = "2026-01-15T00:00:00Z";

function iso(milliseconds) {
  return new Date(milliseconds).toISOString().replace(".000Z", "Z");
}

function target(cycleUtc, lead) {
  const cycleMs = Date.parse(cycleUtc);
  const validMs = cycleMs + lead * 3600000;
  const cycleCode = cycleUtc.replace(/[-:T]/g, "").slice(0, 10);
  return {
    source_id: "nbm_snow_level",
    cycle_time_utc: cycleUtc,
    valid_time_utc: iso(validMs),
    valid_from_utc: iso(validMs - 3 * 3600000),
    valid_through_utc: iso(validMs + 3 * 3600000),
    lead_hours: lead,
    retrieval_url: "https://example.test/source.grib2",
    inventory_url: "https://example.test/source.grib2.idx",
    inventory_record: "1:0:d=2026011512:SNOWLVL:surface",
    path: `nbm/snow-level/winter_storm_levels_nbm_snow_level_${cycleCode}_f${String(lead).padStart(3, "0")}_aaaaaaaaaaaa.geojson`,
    media_type: "application/geo+json",
    sha256: "a".repeat(64),
    bytes: 100,
    feature_count: 1,
    contour_levels_ft_msl: [6000],
    source_grid: {
      rows: 10,
      columns: 20,
      resolution_m: 2500,
      finite_coverage: 0.95,
      min_m: 0,
      max_m: 5000
    },
    output_bbox_wgs84: [-129, 31, -113, 44]
  };
}

function manifestFixture() {
  return {
    product_id: "winter_storm_levels",
    schema_version: "1.0.0",
    contract_version: "1.0.0",
    status: "current",
    source: {
      id: "nbm_snow_level",
      name: "National Blend of Models snow level",
      agency: "NOAA/NWS/NCEP/MDL",
      parameter: "SNOWLVL",
      definition: "Elevation where wet-bulb temperature reaches 0.5 degrees C",
      source_unit: "m above mean sea level",
      output_unit: "ft above mean sea level",
      product_url: "https://example.test/product",
      retrieval_base_url: "https://example.test/primary",
      alternate_retrieval_base_url: "https://example.test/alternate"
    },
    domain: {
      id: "winter_storm_levels_west_v1",
      label: "Western United States",
      bbox_wgs84: [-130, 30, -112, 44.5]
    },
    contour: {
      geometry: "LineString",
      datum: "mean_sea_level",
      unit: "ft_msl",
      minimum_ft: 0,
      maximum_ft: 20000,
      interval_ft: 1000,
      simplify_tolerance_m: 0
    },
    freshness: {
      current_after_hours: 9,
      delayed_after_hours: 15,
      expire_after_hours: 24,
      valid_tolerance_hours: 3
    },
    cycle_time_utc: currentCycle,
    retrieval_time_utc: "2026-01-15T12:30:00Z",
    publication_time_utc: null,
    target_count: 22,
    diagnostics: {
      expected_current_cycle_target_count: 11,
      actual_current_cycle_target_count: 11,
      retained_cycle_count: 2,
      complete_bundle_validated: true
    },
    targets: [
      ...leads.map((lead) => target(previousCycle, lead)),
      ...leads.map((lead) => target(currentCycle, lead))
    ]
  };
}

const manifest = api.validateManifest(manifestFixture());
assert.strictEqual(manifest._ptSnowCycles.length, 2);
assert.strictEqual(manifest._ptSnowCycles[0].cycle_time_utc, currentCycle);
assert.strictEqual(manifest._ptSnowCycles[1].cycle_time_utc, previousCycle);
assert.deepStrictEqual(
  manifest._ptSnowCycles[0].targets.map((entry) => entry.lead_hours),
  leads,
  "the current run must expose the exact discrete lead set"
);

const currentTargets = manifest._ptSnowCycles[0].targets;
assert.strictEqual(
  api.defaultTargetIndex(currentTargets, Date.parse(currentCycle) + 6 * 3600000),
  1,
  "the active f006 target must be selected at its valid time"
);
assert.strictEqual(
  api.closestTargetIndex(
    manifest._ptSnowCycles[1].targets,
    target(previousCycle, 24).valid_time_utc,
    Date.parse(currentCycle)
  ),
  4,
  "previous-run target selection must preserve an exact valid time when available"
);

assert.strictEqual(api.freshness(manifest, Date.parse(currentCycle) + 6 * 3600000), "current");
assert.strictEqual(api.freshness(manifest, Date.parse(currentCycle) + 10 * 3600000), "delayed_but_usable");
assert.strictEqual(api.freshness(manifest, Date.parse(currentCycle) + 16 * 3600000), "stale_last_known_good");
assert.strictEqual(api.freshness(manifest, Date.parse(currentCycle) + 25 * 3600000), "expired");

assert(api.formatPacific("2026-07-15T12:00:00Z").includes("5:00 AM PDT"));
assert(api.formatPacific("2026-01-15T12:00:00Z").includes("4:00 AM PST"));
assert(api.formatPacificCompact("2026-07-15T12:00:00Z").includes("5 AM PDT"));
assert(!api.formatPacificCompact("2026-07-15T12:00:00Z").includes("5:00 AM PDT"));
assert(api.formatPacificCompact("2026-07-15T12:30:00Z").includes("5:30 AM PDT"));
assert.strictEqual(api.utcHour("2026-07-15T12:00:00Z"), "12Z");
assert.strictEqual(api.color(0), "#2c5aa0");
assert.strictEqual(api.color(10000), "#ece08b");
assert.strictEqual(api.color(20000), "#a52347");
assert(api.gradient().includes("#2c5aa0 0.0%"));
assert(api.gradient().includes("#a52347 100.0%"));

const entry = target(currentCycle, 6);
const feature = {
  type: "Feature",
  id: "2026011512_f006_06000_001",
  properties: {
    product_id: "winter_storm_levels",
    source_id: "nbm_snow_level",
    parameter: "snow_level",
    definition: "height of the wet-bulb 0.5 degree C surface",
    level_ft_msl: 6000,
    label: "6,000 ft",
    unit: "ft_msl",
    cycle_time_utc: entry.cycle_time_utc,
    valid_time_utc: entry.valid_time_utc,
    lead_hours: entry.lead_hours,
    segment: 1,
    length_m: 100000
  },
  geometry: {
    type: "LineString",
    coordinates: [[-125, 40], [-124, 41]]
  }
};
assert.deepStrictEqual(
  api.validateGeoJson(
    {type: "FeatureCollection", contract_version: "1.0.0", bbox: entry.output_bbox_wgs84, features: [feature]},
    entry
  ),
  {featureCount: 1, vertexCount: 2}
);
const tooltip = api.featureHtml(feature, entry, false);
assert(tooltip.includes("6,000 ft MSL"));
assert(tooltip.includes('pt-nbm-snow-tooltip-label">Valid</span>'));
assert(tooltip.includes('pt-nbm-snow-tooltip-label">Run</span>'));
assert(tooltip.includes("10 AM PST"));
assert(tooltip.includes("4 AM PST · +6 h"));
assert(!tooltip.includes("10:00 AM PST"));
assert(!tooltip.includes("National Blend of Models"));
const popup = api.featureHtml(feature, entry, true);
assert(popup.includes("Modeled snow level"));
assert(popup.includes("Forecast lead"));
assert(popup.includes("NOAA / National Blend of Models"));
assert(popup.includes("10:00 AM PST"));
assert(helperSource.includes(".leaflet-tooltip.pt-nbm-snow-tooltip"));
assert(helperSource.includes("width:max-content;min-width:236px;max-width:300px"));
assert(helperSource.includes(".pt-nbm-snow-tooltip-row"));
assert(helperSource.includes("white-space:nowrap"));

assert.throws(
  () => api.validateManifest({...manifestFixture(), product_id: "wrong_product"}),
  /product_id/
);
assert.throws(
  () => api.validateManifest({...manifestFixture(), contract_version: "2.0.0"}),
  /contract version/
);
assert.throws(
  () => api.validateManifest({...manifestFixture(), targets: manifestFixture().targets.slice(1)}),
  /exactly 22 retained targets/
);
assert.throws(
  () => api.validateGeoJson(
    {type: "FeatureCollection", contract_version: "1.0.0", bbox: entry.output_bbox_wgs84, features: [{...feature, id: "bad"}]},
    entry
  ),
  /feature ID/
);

api.cacheClear();
for (let index = 1; index <= 4; index += 1) api.cacheSet(`target-${index}`, {index});
assert.deepStrictEqual(api.cacheGet("target-1"), {index: 1});
api.cacheSet("target-5", {index: 5});
assert.strictEqual(api.cacheSize(), 4);
assert.strictEqual(api.cacheGet("target-2"), null, "least-recently-used target must be evicted");
assert.deepStrictEqual(api.cacheGet("target-1"), {index: 1}, "recently touched immutable target must remain cached");

const lifecycleLayer = new api.Layer({manifestUrl: "https://example.test/manifest.json"});
lifecycleLayer._createCard = function() {};
lifecycleLayer.refreshCurrentView = function() {};
const mapEvents = new Map();
let removedLayerCount = 0;
let removedControlCount = 0;
let destroyedCardCount = 0;
const fakeMap = {
  on(name, handler) {
    mapEvents.set(name, handler);
  },
  off(name, handler) {
    if (mapEvents.get(name) === handler) mapEvents.delete(name);
  },
  getZoom() {
    return 6;
  },
  removeLayer() {
    removedLayerCount += 1;
  },
  removeControl() {
    removedControlCount += 1;
  }
};
lifecycleLayer.onAdd(fakeMap);
lifecycleLayer.onAdd(fakeMap);
assert.strictEqual(lifecycleLayer.getDiagnostics().activations, 1, "repeated ON must be idempotent");
assert.strictEqual(windowStub.BRIM.opsLiveTimeControllers.nbmSnowLevels, lifecycleLayer);
lifecycleLayer._cardControl = {};
lifecycleLayer._card = {};
lifecycleLayer._detachable = {destroy() { destroyedCardCount += 1; }};
lifecycleLayer.forceRemove(fakeMap);
lifecycleLayer.forceRemove(fakeMap);
assert.strictEqual(lifecycleLayer.getDiagnostics().removals, 1, "repeated OFF must be idempotent");
assert.strictEqual(windowStub.BRIM.opsLiveTimeControllers.nbmSnowLevels, undefined);
assert.strictEqual(documentListeners.size, 0, "OFF must remove document listeners");
assert.strictEqual(removedLayerCount, 1, "OFF must remove the owned label group exactly once");
assert.strictEqual(removedControlCount, 1, "OFF must remove the owned card control exactly once");
assert.strictEqual(destroyedCardCount, 1, "OFF must destroy detachable-card state exactly once");

const seamLayer = new api.Layer({manifestUrl: "https://example.test/manifest.json"});
seamLayer._removed = false;
seamLayer._cycles = manifest._ptSnowCycles;
seamLayer._cycleIndex = 0;
seamLayer._targetIndex = 1;
seamLayer._timeState = {
  cycle_utc: currentCycle,
  valid_time_utc: currentTargets[1].valid_time_utc,
  lead_hours: currentTargets[1].lead_hours
};
const seamRequests = [];
seamLayer._requestTarget = function(cycleIndex, targetIndex, reason) {
  seamRequests.push({cycleIndex, targetIndex, reason});
  return true;
};
assert.deepStrictEqual(seamLayer.getSelectionState(), {
  cycle_utc: currentCycle,
  valid_time_utc: currentTargets[1].valid_time_utc,
  lead_hours: currentTargets[1].lead_hours,
  cycle_index: 0,
  target_index: 1,
  cycle_role: "current"
});
assert.strictEqual(seamLayer.stepValidTime(1), true);
assert.deepStrictEqual(seamRequests.pop(), {cycleIndex: 0, targetIndex: 2, reason: "step valid time"});
assert.strictEqual(seamLayer.stepValidTime(-2), false, "stepping before the first target must be rejected");
assert.strictEqual(seamLayer.selectTarget(1, 4, "paired product action"), true);
assert.deepStrictEqual(seamRequests.pop(), {cycleIndex: 1, targetIndex: 4, reason: "paired product action"});
seamLayer._displayedEntry = currentTargets[1];
assert.strictEqual(seamLayer.selectCycle(1), true);
assert.strictEqual(seamRequests.pop().cycleIndex, 1, "cycle selection must use the public target seam");
seamLayer._emitTimeSelection("fixture selection");
assert.strictEqual(windowEvents.at(-1).type, "brim:nbm-time-selection");
assert.strictEqual(windowEvents.at(-1).detail.valid_time_utc, currentTargets[1].valid_time_utc);
assert.strictEqual(windowEvents.at(-1).detail.product_id, "winter_storm_levels");

const inFlightLayer = new api.Layer({manifestUrl: "https://example.test/manifest.json"});
inFlightLayer._createCard = function() {};
inFlightLayer.refreshCurrentView = function() {};
inFlightLayer.onAdd(fakeMap);
inFlightLayer._cycles = manifest._ptSnowCycles;
inFlightLayer._fetchTarget = function() {
  return Promise.resolve({
    payload: {type: "FeatureCollection", features: []},
    cacheHit: false,
    bytes: 100,
    fetchMs: 1,
    hashMs: 1,
    parseMs: 1,
    featureCount: 0,
    vertexCount: 0
  });
};
const inFlightRequest = inFlightLayer.selectTarget(0, 1, "toggle-off fixture");
inFlightLayer.forceRemove(fakeMap);

const targetErrorLayer = new api.Layer({manifestUrl: "https://example.test/manifest.json"});
targetErrorLayer._removed = false;
targetErrorLayer._map = fakeMap;
targetErrorLayer._cycles = manifest._ptSnowCycles;
targetErrorLayer._displayedEntry = currentTargets[1];
targetErrorLayer._timeState = {
  cycle_utc: currentCycle,
  valid_time_utc: currentTargets[1].valid_time_utc,
  lead_hours: currentTargets[1].lead_hours
};
targetErrorLayer._ui.banner = {className: "", textContent: ""};
targetErrorLayer._fetchTarget = function() { return Promise.reject(new Error("fixture target failure")); };

const manifestErrorLayer = new api.Layer({manifestUrl: "https://example.test/manifest.json"});
manifestErrorLayer._removed = false;
manifestErrorLayer._ui.banner = {className: "", textContent: ""};
manifestErrorLayer._fetchManifest = function() { return Promise.reject(new Error("fixture manifest failure")); };

if (process.env.BRIM_SNOW_LIVE_MANIFEST) {
  const liveManifestBytes = fs.readFileSync(process.env.BRIM_SNOW_LIVE_MANIFEST);
  const liveManifest = api.validateManifest(JSON.parse(liveManifestBytes.toString("utf8")));
  const currentIndex = api.defaultTargetIndex(liveManifest._ptSnowCycles[0].targets, Date.now());
  assert(currentIndex >= 0, "the live manifest must expose a currently active target");
  const currentEntry = liveManifest._ptSnowCycles[0].targets[currentIndex];
  const previousIndex = api.closestTargetIndex(
    liveManifest._ptSnowCycles[1].targets,
    currentEntry.valid_time_utc,
    Date.now()
  );
  const previousEntry = liveManifest._ptSnowCycles[1].targets[previousIndex];

  function validateLiveTarget(filename, liveEntry) {
    const started = performance.now();
    const bytes = fs.readFileSync(filename);
    assert.strictEqual(bytes.byteLength, liveEntry.bytes, "live target bytes must match the manifest");
    assert.strictEqual(
      crypto.createHash("sha256").update(bytes).digest("hex"),
      liveEntry.sha256,
      "live target SHA-256 must match the manifest"
    );
    const parsed = JSON.parse(bytes.toString("utf8"));
    const geometry = api.validateGeoJson(parsed, liveEntry);
    return {
      path: liveEntry.path,
      bytes: bytes.byteLength,
      featureCount: geometry.featureCount,
      vertexCount: geometry.vertexCount,
      parseValidateMs: Number((performance.now() - started).toFixed(2))
    };
  }

  assert(process.env.BRIM_SNOW_LIVE_CURRENT, "current live target file is required");
  assert(process.env.BRIM_SNOW_LIVE_PREVIOUS, "previous live target file is required");
  const liveSummary = {
    manifestBytes: liveManifestBytes.byteLength,
    currentCycleUtc: liveManifest._ptSnowCycles[0].cycle_time_utc,
    previousCycleUtc: liveManifest._ptSnowCycles[1].cycle_time_utc,
    currentValidPacific: api.formatPacific(currentEntry.valid_time_utc),
    currentValidUtc: api.utcHour(currentEntry.valid_time_utc),
    currentTarget: validateLiveTarget(process.env.BRIM_SNOW_LIVE_CURRENT, currentEntry),
    previousTarget: validateLiveTarget(process.env.BRIM_SNOW_LIVE_PREVIOUS, previousEntry)
  };
  console.log(`LIVE_SMOKE ${JSON.stringify(liveSummary)}`);
}

Promise.resolve(inFlightRequest)
  .then(() => {
    assert.deepStrictEqual(inFlightLayer.getTimeState(), {
      cycle_utc: null,
      valid_time_utc: null,
      lead_hours: null
    }, "toggle OFF during target load must prevent a late state commit");
    assert.strictEqual(inFlightLayer._displayGroup, null, "toggle OFF during target load must not add late geometry");
    return targetErrorLayer.selectTarget(0, 2, "target error fixture");
  })
  .then(() => {
    assert.strictEqual(targetErrorLayer.getDiagnostics().errors.length, 1);
    assert(targetErrorLayer._ui.banner.textContent.includes("fixture target failure"));
    assert.strictEqual(targetErrorLayer._displayedEntry, currentTargets[1], "target failure must retain prior validated geometry state");
    return manifestErrorLayer.refreshCurrentView();
  })
  .then(() => {
    assert.strictEqual(manifestErrorLayer.getDiagnostics().errors.length, 1);
    assert(manifestErrorLayer._ui.banner.textContent.includes("fixture manifest failure"));
    console.log("NBM Snow Levels deterministic consumer tests passed.");
  })
  .catch((error) => {
    console.error(error);
    process.exitCode = 1;
  });
