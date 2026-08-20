
const assert = require("assert");
const crypto = require("crypto");
const fs = require("fs");
const path = require("path");
const zlib = require("zlib");

const repoRoot = path.join(__dirname, "..");
const helperSource = fs.readFileSync(path.join(repoRoot, "03_functions", "leaflet_ops_live_nbm_snow_levels_helpers.r"), "utf8");
const panelSource = fs.readFileSync(path.join(repoRoot, "03_functions", "leaflet_ops_live_panel_helpers.r"), "utf8");
const sharedSource = fs.readFileSync(path.join(repoRoot, "03_functions", "leaflet_ops_live_shared_helpers.r"), "utf8");
const configSource = fs.readFileSync(path.join(repoRoot, "00_config", "config_map_display.r"), "utf8");
const coreSource = fs.readFileSync(path.join(repoRoot, "03_functions", "leaflet_core_helpers.r"), "utf8");

function embeddedRawJs(source) {
  const startToken = 'r"---(';
  const endToken = ')---"';
  const start = source.indexOf(startToken);
  const end = source.lastIndexOf(endToken);
  assert(start >= 0 && end > start);
  return source.slice(start + startToken.length, end);
}
function includesAll(source, values) {
  values.forEach((value) => assert(source.includes(value), `missing source contract: ${value}`));
}
function opsLayerRegistration(source, nameToken) {
  const nameIndex = source.indexOf(`name: ${nameToken},`);
  assert(nameIndex >= 0, `missing Ops registration: ${nameToken}`);
  const start = source.lastIndexOf("addOpsLayer({", nameIndex);
  const end = source.indexOf("\n    });", nameIndex);
  assert(start >= 0 && end > nameIndex, `incomplete Ops registration: ${nameToken}`);
  return source.slice(start, end);
}

includesAll(helperSource, [
  "var PtOpsNbmForecastController", "var PtOpsNbmProductLayer",
  "NBM Forecast Guidance", "NBM Snow Levels", "NBM 6-Hour QPF",
  "panelOrder: -100", "panelOrder: -99",
  "window.BRIM.opsLiveTimeControllers.nbmForecast = this",
  "window.BRIM.opsLiveTimeControllers.nbmSnowLevels = this",
  "brim:nbm-time-selection", "ptNbmBuildInventory", "ptNbmHorizonLabel",
  "interactive: false", "attachSelectedFrame",
  "palette colors are never reverse-mapped", "u16le", "forecast_state_id",
  "new window.DecompressionStream('gzip')", "ptQpfLngLatToCell",
  "pt-ops-nbm-qpf-hover-tooltip",
  "No exact QPF target exists for this cycle + lead + valid time. No substitute is used."
]);
assert(!helperSource.includes("var PT_QPF_LEADS"));
assert(!helperSource.includes("var PT_SNOW_LEADS"));
assert(!helperSource.includes('data-pt-ops-action="nbm-qpf"'));
assert(!helperSource.includes("pt-ops-nbm-qpf-toggle"));
assert(!panelSource.includes('data-pt-ops-action="nbm-qpf"'));
includesAll(panelSource, ["Number(a.def.panelOrder)", 'data-pt-ops-action="nbm-snow-labels"', 'data-pt-ops-action="nbm-snow-card"']);
includesAll(panelSource, ["!activeLayers['NBM Snow Levels'] && !activeLayers['NBM 6-Hour QPF']"]);
includesAll(configSource, ["add_ops_nbm_snow_levels = TRUE", "add_ops_nbm_qpf = TRUE"]);
assert(coreSource.indexOf('addMapPane("pane_ops_qpf"') < coreSource.indexOf('addMapPane("pane_ops"'));

const snowRowSource = opsLayerRegistration(helperSource, "PT_SNOW_PRODUCT_NAME");
const qpfRowSource = opsLayerRegistration(helperSource, "PT_QPF_PRODUCT_NAME");
includesAll(snowRowSource, [
  "refreshable: true",
  "sourceUrl: 'https://vlab.noaa.gov/web/mdl/nbm-weather-elements'",
  'data-pt-ops-action="nbm-snow-labels"',
  "NOAA/NBM forecast snow-level elevation. +1 h is Snow-only."
]);
includesAll(qpfRowSource, [
  "refreshable: true",
  "sourceUrl: 'https://vlab.noaa.gov/web/mdl/nbm-weather-elements'",
  "NOAA/NBM precipitation forecast for the preceding six hours."
]);
assert(!snowRowSource.includes("nbm-snow-card"), "Snow row must not expose lgnd");
assert(!snowRowSource.includes(">lgnd</a>"), "Snow row must not render lgnd");
assert(!qpfRowSource.includes("extraRowHtml"), "QPF row must not append controls after srce");
assert(!qpfRowSource.includes(">lgnd</a>"), "QPF row must not render lgnd");
includesAll(sharedSource, [
  "if (def.refreshable === true || opts.refreshable === true) return true",
  "pushUnique('srce', sourceUrl, 'Open source')",
  "pushUnique('lgnd', legendUrl, 'Open legend')"
]);

const documentListeners = new Map();
const documentStub = {
  visibilityState: "visible",
  head: {appendChild() {}},
  createElement() { return {id: "", textContent: "", style: {}, appendChild() {}}; },
  getElementById() { return null; },
  querySelectorAll() { return []; },
  addEventListener(name, handler) { documentListeners.set(name, handler); },
  removeEventListener(name, handler) {
    if (documentListeners.get(name) === handler) documentListeners.delete(name);
  }
};
function extend(methods) {
  function Klass(...args) {
    if (typeof methods.initialize === "function") methods.initialize.apply(this, args);
  }
  Klass.prototype = Object.assign({constructor: Klass}, methods);
  return Klass;
}
const windowEvents = [];
const windowStub = {
  BRIM: {},
  CustomEvent: class CustomEvent {
    constructor(type, options) { this.type = type; this.detail = options && options.detail; }
  },
  dispatchEvent(event) { windowEvents.push(event); return true; },
  performance: {now: () => 0},
  setInterval: () => 101,
  clearInterval() {},
  setTimeout,
  clearTimeout,
  requestAnimationFrame(callback) { callback(); },
  URL,
  Blob,
  Response,
  DecompressionStream,
  crypto: crypto.webcrypto
};
const LStub = {
  Class: {extend},
  Layer: {extend},
  layerGroup() { return {addTo() { return this; }, clearLayers() {}, addLayer() {}}; }
};
const api = new Function("window", "document", "L", `
  var includeNbmSnowLevels = false;
  var includeNbmQpf = false;
  var NBM_SNOW_LEVELS_MANIFEST_URL = '';
  var NBM_QPF_MANIFEST_URL = '';
  var activeLegendDefs = {};
  function addOpsLayer() {}
  function redrawLegend() {}
  function setOpsLayerLoading() {}
  function recordStatus() {}
  function escapeHtml(value) { return String(value); }
  ${embeddedRawJs(helperSource)}
  return {
    validateManifest: ptSnowValidateManifest,
    validateGeoJson: ptSnowValidateGeoJson,
    freshness: ptSnowFreshness,
    defaultTargetIndex: ptSnowDefaultTargetIndex,
    formatPacific: ptSnowFormatPacific,
    color: ptSnowLevelColor,
    cacheGet: ptSnowCacheGet, cacheSet: ptSnowCacheSet,
    cacheClear: ptSnowCacheClear, cacheSize: function() { return ptSnowTargetCache.size; },
    validateQpfManifest: ptQpfValidateManifest,
    validateQpfTarget: ptQpfValidateTarget,
    findQpfPair: ptQpfFindPair,
    qpfTargetUrl: ptQpfTargetUrl,
    qpfNumericUrl: ptQpfNumericUrl,
    forecastStateText: ptQpfForecastStateText,
    verifyForecastState: ptQpfVerifyForecastState,
    decodeNumeric: ptQpfDecodeNumericPayload,
    fetchNumericFrame: ptQpfFetchNumericFrame,
    numericAcquire: ptQpfNumericAcquire,
    numericCacheClear: ptQpfNumericCacheClear,
    numericCacheSize: function() { return ptQpfNumericCache.size; },
    numericInflightSize: function() { return ptQpfNumericInflight.size; },
    lngLatToCell: ptQpfLngLatToCell,
    numericValueAt: ptQpfNumericValueAt,
    createNumericProvider: ptQpfCreateDefaultNumericHoverProvider,
    buildInventory: ptNbmBuildInventory,
    horizonLabel: ptNbmHorizonLabel,
    horizonCue: ptNbmHorizonCue,
    Controller: PtOpsNbmForecastController,
    ProductLayer: PtOpsNbmProductLayer,
    activeLegendDefs
  };
`)(windowStub, documentStub, LStub);

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

function manifestFixture(snowLeads = leads, retainedCycles = 2) {
  const cycleTargets = retainedCycles === 1
    ? snowLeads.map((lead) => target(currentCycle, lead))
    : [
        ...snowLeads.map((lead) => target(previousCycle, lead)),
        ...snowLeads.map((lead) => target(currentCycle, lead))
      ];
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
    target_count: cycleTargets.length,
    diagnostics: {
      expected_current_cycle_target_count: snowLeads.length,
      actual_current_cycle_target_count: snowLeads.length,
      retained_cycle_count: retainedCycles,
      complete_bundle_validated: true
    },
    targets: cycleTargets
  };
}

const qpfLeads = [6, 12, 18, 24, 30, 36, 42, 48, 60, 72];
const qpfClassTriples = [
  [0.01, 0.1, "#D9F0D3"], [0.1, 0.25, "#A6DBA0"],
  [0.25, 0.5, "#62BD73"], [0.5, 1, "#2F9E55"],
  [1, 1.5, "#146B38"], [1.5, 2, "#FFF59D"],
  [2, 2.5, "#FFE066"], [2.5, 3, "#FDBE55"],
  [3, 3.5, "#F79441"], [3.5, 4, "#F05A3C"],
  [4, 4.5, "#D73027"], [4.5, 5, "#BD1F2D"],
  [5, 5.5, "#9E1737"], [5.5, 6, "#7A123D"],
  [6, 7, "#5B0B55"], [7, 8, "#480A6A"],
  [8, 10, "#5F0A87"], [10, 12, "#7D1A9A"],
  [12, 15, "#A542B0"], [15, 20, "#CD86CF"],
  [20, null, "#F1C6E7"]
];

function qpfTarget(cycleUtc, lead) {
  const cycleMs = Date.parse(cycleUtc);
  const validMs = cycleMs + lead * 3600000;
  const cycleToken = cycleUtc.replace(/[-:]/g, "");
  const imagePath = `docs/data/nbm-qpf/nbm/qpf/nbm_qpf_${cycleToken}_f${String(lead).padStart(3, "0")}_bbbbbbbbbbbb.webp`;
  const numericPath = `docs/data/nbm-qpf/nbm/qpf/nbm_qpf_${cycleToken}_f${String(lead).padStart(3, "0")}_cccccccccccc.u16le.gz`;
  const entry = {
    product_id: "nbm_qpf",
    forecast_state_id: "",
    source_id: "noaa_nbm_core_conus_apcp",
    parameter: "APCP",
    level: "surface",
    cycle_utc: cycleUtc,
    lead_hours: lead,
    lead_end_hours: lead,
    accumulation_start_utc: iso(validMs - 6 * 3600000),
    accumulation_end_utc: iso(validMs),
    valid_time_utc: iso(validMs),
    accumulation_hours: 6,
    source_parameter: "APCP",
    source_level: "surface",
    source_inventory_semantics: `${lead - 6}-${lead} hour acc fcst`,
    native_units: "kg/m^2",
    normalized_units: "mm",
    stored_numeric_units: "in",
    display_units: "in",
    grid_contract_id: "nbm_qpf_lossless_webp_v1",
    columns: 720,
    rows: 733,
    crs: "EPSG:3857",
    extent_m: [-14471533.8031256, 3503549.84350437, -12467782.9688466, 5543147.2038618],
    row_order: "north_to_south",
    column_order: "west_to_east",
    pixel_is_area: true,
    image_path: imagePath,
    image_media_type: "image/webp",
    image_encoding: "lossless_vp8l_rgba8",
    image_width: 720,
    image_height: 733,
    bounds_wgs84: [-130, 30, -112, 44.5],
    bytes: 100 + lead,
    sha256: "b".repeat(64),
    image: {
      path: imagePath,
      media_type: "image/webp",
      encoding: "lossless_vp8l_rgba8",
      bytes: 100 + lead,
      sha256: "b".repeat(64)
    },
    numeric: {
      path: numericPath,
      media_type: "application/octet-stream",
      encoding: "uint16_le",
      compression: "gzip",
      stored_units: "in",
      scale: 0.001,
      offset: 0,
      nodata: 65535,
      compressed_bytes: 200 + lead,
      uncompressed_bytes: 1055520,
      sha256: "c".repeat(64)
    },
    palette_id: "brim_nbm_qpf_6h_west_v1",
    palette_version: 1
  };
  entry.forecast_state_id = crypto.createHash("sha256")
    .update(api.forecastStateText(entry), "utf8").digest("hex");
  return entry;
}

function qpfCycle(cycleUtc, maximum = 0.4, cycleLeads = qpfLeads) {
  return {
    cycle_utc: cycleUtc,
    cycle_status: "complete",
    cycle_max_qpf_in: maximum,
    legend_cap_in: maximum <= 3 ? 3 : maximum <= 4 ? 4 : maximum <= 6 ? 6 :
      maximum <= 8 ? 8 : maximum <= 10 ? 10 : maximum <= 12 ? 12 : maximum <= 15 ? 15 : 20,
    legend_overflow: maximum > 20,
    target_count: cycleLeads.length,
    complete_required_leads_hours: cycleLeads.slice(),
    targets: cycleLeads.map((lead) => qpfTarget(cycleUtc, lead))
  };
}

function qpfManifestFixture(cycles = [qpfCycle(currentCycle)]) {
  return {
    schema_version: "1.0.0",
    product_id: "nbm_qpf",
    generated_at_utc: "2026-01-15T12:45:00Z",
    source: {
      source_id: "noaa_nbm_core_conus_apcp",
      agency: "NOAA/NWS/NCEP/MDL",
      dataset: "National Blend of Models",
      family: "core",
      domain: "conus",
      parameter: "APCP",
      level: "surface",
      field_kind: "deterministic accumulated precipitation",
      native_units: "kg/m^2",
      normalized_units: "mm",
      display_units: "in"
    },
    palette: {
      palette_id: "brim_nbm_qpf_6h_west_v1",
      palette_version: 1,
      display_units: "in",
      class_interval: "lower-inclusive upper-exclusive",
      below_0_01_in: "transparent",
      nodata: "transparent",
      overflow: ">=20 in uses the final fixed class",
      classes: qpfClassTriples.map(([lower, upper, color]) => ({
        lower_inclusive_in: lower,
        upper_exclusive_in: upper,
        color_hex: color,
        alpha_u8: 255
      }))
    },
    spatial_representation: {
      contract_id: "nbm_qpf_lossless_webp_v1",
      media_type: "image/webp",
      encoding: "lossless VP8L RGBA8 WebP",
      crs: "EPSG:3857",
      bounds_wgs84: [-130, 30, -112, 44.5],
      extent_m: [-14471533.8031256, 3503549.84350437, -12467782.9688466, 5543147.2038618],
      image_width: 720,
      image_height: 733,
      pixel_size_m: [2782.98726983229, 2782.53391590372],
      row_order: "north_to_south",
      column_order: "west_to_east",
      pixel_is_area: true,
      leaflet_bounds: [[30, -130], [44.5, -112]],
      default_leaflet_opacity: 0.55
    },
    numeric_representation: {
      contract_id: "nbm_qpf_uint16_le_gzip_v1",
      media_type: "application/octet-stream",
      encoding: "uint16_le",
      compression: "gzip",
      stored_units: "in",
      scale: 0.001,
      offset: 0,
      nodata: 65535,
      valid_stored_min: 0,
      valid_stored_max: 65534,
      represented_min: 0,
      represented_max: 65.534,
      uncompressed_bytes: 1055520,
      grid_contract_id: "nbm_qpf_lossless_webp_v1",
      columns: 720,
      rows: 733,
      crs: "EPSG:3857",
      bounds_wgs84: [-130, 30, -112, 44.5],
      extent_m: [-14471533.8031256, 3503549.84350437, -12467782.9688466, 5543147.2038618],
      row_order: "north_to_south",
      column_order: "west_to_east",
      pixel_is_area: true
    },
    forecast_state_binding: {
      algorithm: "nbm_qpf_forecast_state_sha256_v1",
      digest: "sha256",
      canonicalization: "ordered UTF-8 key=value lines",
      binds: ["forecast metadata", "image path and SHA-256", "numeric path and SHA-256"]
    },
    freshness: {
      basis: "source_cycle_age",
      current_through_hours: 9,
      delayed_through_hours: 15,
      stale_through_hours: 24,
      expired_after_hours: 24,
      product_status_independent_from_snow: true
    },
    retention_mode: cycles.length === 1 ? "bootstrap" : "steady",
    current_cycle_utc: cycles[0].cycle_utc,
    previous_cycle_utc: cycles.length === 1 ? null : cycles[1].cycle_utc,
    cycles
  };
}


const longSnowLeads = [1, ...Array.from({length: 40}, (_, index) => (index + 1) * 6)];
const longQpfLeads = longSnowLeads.filter((lead) => lead !== 1);

function clone(value) {
  return JSON.parse(JSON.stringify(value));
}

const numericDependencies = {
  digestHex(buffer) {
    return crypto.createHash("sha256").update(Buffer.from(buffer)).digest("hex");
  },
  gunzip(buffer) {
    return zlib.gunzipSync(Buffer.from(buffer));
  }
};

function numericPayloadFixture(lead = 54) {
  const raw = Buffer.alloc(1055520);
  const values = new Uint16Array(raw.buffer, raw.byteOffset, raw.byteLength / 2);
  values[0] = 123;
  values[366 * 720 + 360] = 456;
  values[100 * 720 + 200] = 65535;
  values[732 * 720 + 719] = 789;
  const gzip = zlib.gzipSync(raw, {level: 9, mtime: 0});
  const entry = qpfTarget(currentCycle, lead);
  const sha = crypto.createHash("sha256").update(gzip).digest("hex");
  const cycleToken = currentCycle.replace(/[-:]/g, "");
  entry.numeric.compressed_bytes = gzip.byteLength;
  entry.numeric.sha256 = sha;
  entry.numeric.path = `docs/data/nbm-qpf/nbm/qpf/nbm_qpf_${cycleToken}_f${String(lead).padStart(3, "0")}_${sha.slice(0, 12)}.u16le.gz`;
  entry.forecast_state_id = crypto.createHash("sha256")
    .update(api.forecastStateText(entry), "utf8").digest("hex");
  return {entry, gzip};
}

function arrayBuffer(buffer) {
  return buffer.buffer.slice(buffer.byteOffset, buffer.byteOffset + buffer.byteLength);
}

function cellCenterLatLng(row, column) {
  const extent = [-14471533.8031256, 3503549.84350437, -12467782.9688466, 5543147.2038618];
  const x = extent[0] + (column + 0.5) * (extent[2] - extent[0]) / 720;
  const y = extent[3] - (row + 0.5) * (extent[3] - extent[1]) / 733;
  const radius = 6378137;
  return {
    lng: x / radius * 180 / Math.PI,
    lat: (2 * Math.atan(Math.exp(y / radius)) - Math.PI / 2) * 180 / Math.PI
  };
}

async function main() {
  const snowShort = api.validateManifest(manifestFixture());
  const qpfShort = api.validateQpfManifest(qpfManifestFixture());
  assert.deepStrictEqual(snowShort._ptSnowCycles[0].targets.map((entry) => entry.lead_hours), leads);
  assert.deepStrictEqual(qpfShort._ptQpfCycles[0].targets.map((entry) => entry.lead_hours), qpfLeads);

  const snowLong = api.validateManifest(manifestFixture(longSnowLeads));
  const snowBootstrap = api.validateManifest(manifestFixture(longSnowLeads, 1));
  const qpfLong = api.validateQpfManifest(qpfManifestFixture([qpfCycle(currentCycle, 0.4, longQpfLeads)]));
  assert.strictEqual(snowLong._ptSnowCycles[0].targets.length, 41);
  assert.strictEqual(snowLong._ptSnowCycles[0].targets.at(-1).lead_hours, 240);
  assert.strictEqual(snowBootstrap._ptSnowCycles.length, 1);
  assert.deepStrictEqual(
    snowBootstrap._ptSnowCycles[0].targets.filter((entry) => [54, 66, 240].includes(entry.lead_hours))
      .map((entry) => entry.lead_hours),
    [54, 66, 240]
  );
  assert.strictEqual(qpfLong._ptQpfCycles[0].targets.length, 40);
  assert.strictEqual(qpfLong._ptQpfCycles[0].targets.at(-1).lead_hours, 240);
  assert.strictEqual(api.horizonLabel(240), "Day 10 · +240 h");
  assert(api.horizonCue(240).includes("lower confidence"));
  assert.deepStrictEqual(
    qpfLong._ptQpfCycles[0].targets.filter((entry) => [54, 66, 240].includes(entry.lead_hours))
      .map((entry) => entry.lead_hours),
    [54, 66, 240]
  );

  const snowOnly = api.buildInventory(snowLong._ptSnowCycles, [], true, false);
  const qpfOnly = api.buildInventory([], qpfLong._ptQpfCycles, false, true);
  const combined = api.buildInventory(snowLong._ptSnowCycles, qpfLong._ptQpfCycles, true, true);
  assert.strictEqual(snowOnly[0].targets.length, 41);
  assert.strictEqual(qpfOnly[0].targets.length, 40);
  assert.strictEqual(combined[0].targets.length, 41);
  assert(combined[0].targets[0].snowTarget && !combined[0].targets[0].qpfTarget);
  assert(combined[0].targets[1].snowTarget && combined[0].targets[1].qpfTarget);

  const pair = api.findQpfPair(qpfShort, {
    cycle_utc: currentCycle,
    valid_time_utc: iso(Date.parse(currentCycle) + 6 * 3600000),
    lead_hours: 6
  }, Date.parse(currentCycle) + 8 * 3600000);
  assert.strictEqual(pair.status, "paired");
  assert.strictEqual(api.findQpfPair(qpfShort, {
    cycle_utc: previousCycle,
    valid_time_utc: iso(Date.parse(previousCycle) + 6 * 3600000),
    lead_hours: 6
  }, Date.parse(currentCycle)).status, "no_same_cycle");
  assert.strictEqual(api.findQpfPair(qpfShort, {
    cycle_utc: currentCycle,
    valid_time_utc: iso(Date.parse(currentCycle) + 3600000),
    lead_hours: 1
  }, Date.parse(currentCycle)).status, "snow_only");

  const duplicateQpf = qpfManifestFixture([qpfCycle(currentCycle, 0.4, longQpfLeads)]);
  duplicateQpf.cycles[0].complete_required_leads_hours[2] = 12;
  assert.throws(() => api.validateQpfManifest(duplicateQpf), /unique ordered/);
  const badSnow = manifestFixture(longSnowLeads);
  badSnow.targets.pop();
  assert.throws(() => api.validateManifest(badSnow), /target_count|coherent|retained/);

  const {entry: numericEntry, gzip: numericGzip} = numericPayloadFixture(54);
  await api.verifyForecastState(numericEntry, numericDependencies);
  const numericFrame = await api.decodeNumeric(arrayBuffer(numericGzip), numericEntry, numericDependencies);
  assert.strictEqual(numericFrame.values.length, 720 * 733);
  assert.deepStrictEqual(api.lngLatToCell(cellCenterLatLng(0, 0), numericFrame), {
    row: 0, column: 0, index: 0
  });
  assert.deepStrictEqual(api.lngLatToCell(cellCenterLatLng(366, 360), numericFrame), {
    row: 366, column: 360, index: 366 * 720 + 360
  });
  assert.deepStrictEqual(api.lngLatToCell({lat: 44.5, lng: -130}, numericFrame), {
    row: 0, column: 0, index: 0
  });
  assert.deepStrictEqual(api.lngLatToCell({lat: 30, lng: -112}, numericFrame), {
    row: 732, column: 719, index: 732 * 720 + 719
  });
  assert.strictEqual(api.lngLatToCell({lat: 29.99, lng: -120}, numericFrame), null);
  assert.strictEqual(api.lngLatToCell({lat: 35, lng: -130.01}, numericFrame), null);
  assert.strictEqual(api.numericValueAt(numericFrame, cellCenterLatLng(0, 0)).value_in, 0.123);
  assert.strictEqual(api.numericValueAt(numericFrame, cellCenterLatLng(366, 360)).value_in, 0.456);
  assert.strictEqual(api.numericValueAt(numericFrame, cellCenterLatLng(100, 200)), null);
  assert.strictEqual(api.numericValueAt(numericFrame, cellCenterLatLng(732, 719)).value_in, 0.789);

  const wrongCompressedBytes = clone(numericEntry);
  wrongCompressedBytes.numeric.compressed_bytes += 1;
  await assert.rejects(
    api.decodeNumeric(arrayBuffer(numericGzip), wrongCompressedBytes, numericDependencies),
    /compressed byte count/
  );

  const wrongSha = clone(numericEntry);
  wrongSha.numeric.sha256 = "d".repeat(64);
  wrongSha.numeric.path = wrongSha.numeric.path.replace(/_[0-9a-f]{12}\.u16le\.gz$/, "_dddddddddddd.u16le.gz");
  wrongSha.forecast_state_id = crypto.createHash("sha256")
    .update(api.forecastStateText(wrongSha), "utf8").digest("hex");
  await assert.rejects(
    api.decodeNumeric(arrayBuffer(numericGzip), wrongSha, numericDependencies),
    /numeric SHA-256/
  );

  const corruptBytes = Buffer.from([0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00]);
  const corrupt = clone(numericEntry);
  corrupt.numeric.compressed_bytes = corruptBytes.byteLength;
  corrupt.numeric.sha256 = crypto.createHash("sha256").update(corruptBytes).digest("hex");
  corrupt.numeric.path = corrupt.numeric.path.replace(
    /_[0-9a-f]{12}\.u16le\.gz$/,
    `_${corrupt.numeric.sha256.slice(0, 12)}.u16le.gz`
  );
  corrupt.forecast_state_id = crypto.createHash("sha256")
    .update(api.forecastStateText(corrupt), "utf8").digest("hex");
  await assert.rejects(
    api.decodeNumeric(arrayBuffer(corruptBytes), corrupt, numericDependencies),
    /gzip payload is corrupt/
  );

  const shortGzip = zlib.gzipSync(Buffer.alloc(4), {mtime: 0});
  const wrongUncompressed = clone(numericEntry);
  wrongUncompressed.numeric.compressed_bytes = shortGzip.byteLength;
  wrongUncompressed.numeric.sha256 = crypto.createHash("sha256").update(shortGzip).digest("hex");
  wrongUncompressed.numeric.path = wrongUncompressed.numeric.path.replace(
    /_[0-9a-f]{12}\.u16le\.gz$/,
    `_${wrongUncompressed.numeric.sha256.slice(0, 12)}.u16le.gz`
  );
  wrongUncompressed.forecast_state_id = crypto.createHash("sha256")
    .update(api.forecastStateText(wrongUncompressed), "utf8").digest("hex");
  await assert.rejects(
    api.decodeNumeric(arrayBuffer(shortGzip), wrongUncompressed, numericDependencies),
    /uncompressed byte count/
  );

  const wrongState = clone(numericEntry);
  wrongState.forecast_state_id = "e".repeat(64);
  await assert.rejects(
    api.decodeNumeric(arrayBuffer(numericGzip), wrongState, numericDependencies),
    /forecast_state_id/
  );
  const swappedLead = clone(numericEntry);
  swappedLead.lead_hours = 60;
  assert.throws(() => api.validateQpfTarget(swappedLead, swappedLead.cycle_utc, 0), /identity|preceding-six-hour|path/);
  const swappedCycle = clone(numericEntry);
  swappedCycle.cycle_utc = previousCycle;
  assert.throws(() => api.validateQpfTarget(swappedCycle, swappedCycle.cycle_utc, 0), /identity|preceding-six-hour|path/);
  const wrongGrid = clone(numericEntry);
  wrongGrid.columns = 721;
  assert.throws(() => api.validateQpfTarget(wrongGrid, wrongGrid.cycle_utc, 0), /grid/);
  const wrongDimensions = clone(numericEntry);
  wrongDimensions.image_width = 721;
  assert.throws(() => api.validateQpfTarget(wrongDimensions, wrongDimensions.cycle_utc, 0), /grid/);

  const manifestUrl = "https://example.test/data/nbm-qpf/nbm_qpf_manifest.json";
  const originalFetch = global.fetch;
  global.fetch = async () => ({ok: false, status: 503, headers: {get() { return null; }}});
  await assert.rejects(api.fetchNumericFrame(numericEntry, manifestUrl, null), /HTTP 503/);

  api.numericCacheClear();
  let numericFetches = 0;
  global.fetch = async () => ({
    ok: true,
    status: 200,
    headers: {get() { return null; }},
    async arrayBuffer() { numericFetches += 1; return arrayBuffer(numericGzip); }
  });
  const firstAcquire = api.numericAcquire(numericEntry, manifestUrl);
  const secondAcquire = api.numericAcquire(numericEntry, manifestUrl);
  assert.strictEqual(firstAcquire.promise, secondAcquire.promise);
  await Promise.all([firstAcquire.promise, secondAcquire.promise]);
  assert.strictEqual(numericFetches, 1);
  assert.strictEqual(api.numericCacheSize(), 1);
  firstAcquire.release();
  secondAcquire.release();

  api.numericCacheClear();
  let abortedSignal = null;
  global.fetch = (url, options) => new Promise((resolve, reject) => {
    abortedSignal = options.signal;
    options.signal.addEventListener("abort", () => {
      const error = new Error("aborted");
      error.name = "AbortError";
      reject(error);
    });
  });
  const abortAcquire = api.numericAcquire(numericEntry, manifestUrl);
  const abortedPromise = abortAcquire.promise.catch((error) => error);
  abortAcquire.release();
  const abortError = await abortedPromise;
  assert.strictEqual(abortError.name, "AbortError");
  assert.strictEqual(abortedSignal.aborted, true);
  assert.strictEqual(api.numericInflightSize(), 0);

  global.fetch = async () => ({
    ok: true,
    status: 200,
    headers: {get() { return null; }},
    async arrayBuffer() { return arrayBuffer(numericGzip); }
  });
  const primeAcquire = api.numericAcquire(numericEntry, manifestUrl);
  await primeAcquire.promise;
  primeAcquire.release();
  const numericCycle = qpfCycle(currentCycle, 0.4, [54]);
  numericCycle.targets = [numericEntry];
  const numericManifest = api.validateQpfManifest(qpfManifestFixture([numericCycle]));
  const providerEvents = new Map();
  const providerClasses = new Set();
  const providerStatuses = [];
  const providerMap = {
    on(name, handler) { providerEvents.set(name, handler); },
    off(name, handler) { if (providerEvents.get(name) === handler) providerEvents.delete(name); },
    removeLayer() {},
    getContainer() {
      return {classList: {
        add(name) { providerClasses.add(name); },
        remove(name) { providerClasses.delete(name); }
      }};
    }
  };
  const provider = api.createNumericProvider();
  const providerHandle = provider.attachSelectedFrame({
    map: providerMap,
    manifest: numericManifest,
    manifest_url: manifestUrl,
    target: numericEntry,
    target_url: api.qpfTargetUrl(numericEntry, manifestUrl),
    exact_identity: {
      cycle_utc: numericEntry.cycle_utc,
      valid_time_utc: numericEntry.valid_time_utc,
      lead_hours: numericEntry.lead_hours
    },
    onStatus(kind, text) { providerStatuses.push({kind, text}); }
  });
  await new Promise((resolve) => setImmediate(resolve));
  assert.strictEqual(providerHandle.isReady(), true);
  assert.strictEqual(providerStatuses.at(-1).kind, "ready");
  assert(providerEvents.has("mousemove"));
  assert(providerClasses.has("pt-ops-nbm-qpf-numeric-hover-on"));
  providerHandle.setSuppressed(true);
  providerHandle.setSuppressed(false);
  providerHandle.detach();
  providerHandle.detach();
  assert.strictEqual(providerHandle.isReady(), false);
  assert.strictEqual(providerEvents.size, 0);
  assert.strictEqual(providerClasses.size, 0);
  provider.clearCache();
  global.fetch = originalFetch;

  assert.strictEqual(api.freshness(snowShort, Date.parse(currentCycle) + 6 * 3600000), "current");
  assert.strictEqual(api.freshness(snowShort, Date.parse(currentCycle) + 25 * 3600000), "expired");
  assert(api.formatPacific("2026-07-15T12:00:00Z").includes("5:00 AM PDT"));
  assert.strictEqual(api.color(0), "#2c5aa0");
  assert.strictEqual(api.color(20000), "#a52347");

  api.cacheClear();
  for (let index = 1; index <= 4; index += 1) api.cacheSet(`target-${index}`, {index});
  api.cacheGet("target-1");
  api.cacheSet("target-5", {index: 5});
  assert.strictEqual(api.cacheSize(), 4);
  assert.strictEqual(api.cacheGet("target-2"), null);

  const controller = new api.Controller({
    qpfManifestUrl: "https://example.test/data/nbm-qpf/nbm_qpf_manifest.json"
  });
  controller._removed = false;
  controller._snowActive = true;
  controller._qpfActive = true;
  controller._inventoryCycles = combined;
  const loaded = [];
  controller._loadSnowSelection = async (state) => loaded.push(["snow", state.lead_hours]);
  controller._loadQpfSelection = async (state) => loaded.push(["qpf", state.lead_hours]);
  await controller.selectTarget(0, 1, "fixture selection");
  assert.deepStrictEqual(loaded, [["snow", 6], ["qpf", 6]]);
  assert.strictEqual(windowEvents.at(-1).type, "brim:nbm-time-selection");
  assert.strictEqual(windowEvents.at(-1).detail.product_id, "nbm_forecast_guidance");
  assert.strictEqual(windowEvents.at(-1).detail.snow_available, true);
  assert.strictEqual(windowEvents.at(-1).detail.qpf_available, true);

  let attached = 0;
  let detached = 0;
  let suppressed = null;
  controller._qpfManifest = qpfShort;
  controller._qpfDisplayedEntry = qpfShort.cycles[0].targets[0];
  controller.setQpfNumericHoverProvider({
    attachSelectedFrame(context) {
      attached += 1;
      assert.deepStrictEqual(context.exact_identity, {
        cycle_utc: currentCycle,
        valid_time_utc: iso(Date.parse(currentCycle) + 6 * 3600000),
        lead_hours: 6
      });
      return {detach() { detached += 1; }, setSuppressed(value) { suppressed = value; }};
    }
  });
  controller._setSnowHoverOwned(true);
  assert.strictEqual(attached, 1);
  assert.strictEqual(suppressed, true);
  controller.setQpfNumericHoverProvider(null);
  assert.strictEqual(detached, 1);

  const mapEvents = new Map();
  let removedLayers = 0;
  const fakeMap = {
    on(name, handler) { mapEvents.set(name, handler); },
    off(name, handler) { if (mapEvents.get(name) === handler) mapEvents.delete(name); },
    removeLayer() { removedLayers += 1; },
    removeControl() {},
    getZoom() { return 6; }
  };
  const lifecycle = new api.Controller({});
  lifecycle._createCard = function() {};
  lifecycle.refreshProduct = function() { return Promise.resolve(); };
  lifecycle.activateProduct("snow", fakeMap);
  lifecycle.activateProduct("snow", fakeMap);
  lifecycle.activateProduct("qpf", fakeMap);
  assert.strictEqual(lifecycle.getDiagnostics().activations.snow, 1);
  assert.strictEqual(lifecycle.getDiagnostics().activations.qpf, 1);
  assert.strictEqual(windowStub.BRIM.opsLiveTimeControllers.nbmForecast, lifecycle);
  assert.strictEqual(windowStub.BRIM.opsLiveTimeControllers.nbmSnowLevels, lifecycle);
  lifecycle.deactivateProduct("snow", fakeMap);
  assert.strictEqual(lifecycle._qpfActive, true);
  assert.strictEqual(windowStub.BRIM.opsLiveTimeControllers.nbmForecast, lifecycle);
  lifecycle.deactivateProduct("snow", fakeMap);
  lifecycle.deactivateProduct("qpf", fakeMap);
  lifecycle.deactivateProduct("qpf", fakeMap);
  assert.strictEqual(lifecycle.getDiagnostics().removals.snow, 1);
  assert.strictEqual(lifecycle.getDiagnostics().removals.qpf, 1);
  assert.strictEqual(windowStub.BRIM.opsLiveTimeControllers.nbmForecast, undefined);
  assert.strictEqual(documentListeners.size, 0);
  assert.strictEqual(mapEvents.size, 0);
  assert(removedLayers >= 1);

  let activations = 0;
  let removals = 0;
  const proxy = new api.ProductLayer({
    activateProduct() { activations += 1; },
    deactivateProduct() { removals += 1; },
    refreshProduct() {}
  }, "qpf");
  proxy.onAdd(fakeMap);
  proxy.onAdd(fakeMap);
  proxy.forceRemove(fakeMap);
  proxy.forceRemove(fakeMap);
  assert.strictEqual(activations, 1);
  assert.strictEqual(removals, 1);

  if (process.env.BRIM_SNOW_LIVE_MANIFEST) {
    const bytes = fs.readFileSync(process.env.BRIM_SNOW_LIVE_MANIFEST);
    const live = api.validateManifest(JSON.parse(bytes.toString("utf8")));
    assert.strictEqual(live._ptSnowCycles.length, 1);
    assert.deepStrictEqual(
      live._ptSnowCycles[0].targets.map((entry) => entry.lead_hours),
      longSnowLeads
    );
    const index = api.defaultTargetIndex(live._ptSnowCycles[0].targets, Date.now());
    assert(index >= 0 && process.env.BRIM_SNOW_LIVE_CURRENT);
    const entry = live._ptSnowCycles[0].targets[index];
    const targetBytes = fs.readFileSync(process.env.BRIM_SNOW_LIVE_CURRENT);
    assert.strictEqual(targetBytes.byteLength, entry.bytes);
    assert.strictEqual(crypto.createHash("sha256").update(targetBytes).digest("hex"), entry.sha256);
    const geometry = api.validateGeoJson(JSON.parse(targetBytes.toString("utf8")), entry);
    console.log(`LIVE_SMOKE ${JSON.stringify({
      manifestBytes: bytes.byteLength, currentCycleUtc: live._ptSnowCycles[0].cycle_time_utc,
      leadCount: live._ptSnowCycles[0].targets.length, currentLeadHours: entry.lead_hours,
      featureCount: geometry.featureCount,
      vertexCount: geometry.vertexCount
    })}`);
  }

  if (process.env.BRIM_QPF_LIVE_MANIFEST) {
    const bytes = fs.readFileSync(process.env.BRIM_QPF_LIVE_MANIFEST);
    const live = api.validateQpfManifest(JSON.parse(bytes.toString("utf8")));
    assert.strictEqual(live.retention_mode, "bootstrap");
    assert.strictEqual(live.cycles.length, 1);
    assert.deepStrictEqual(live.cycles[0].targets.map((entry) => entry.lead_hours), longQpfLeads);
    live.cycles[0].targets.forEach((target) => {
      assert(target.forecast_state_id && target.numeric.path.endsWith(".u16le.gz"));
      assert.strictEqual(target.numeric.uncompressed_bytes, 1055520);
      assert.strictEqual(target.numeric.encoding, "uint16_le");
      assert.strictEqual(target.numeric.compression, "gzip");
    });
    const entry = live.cycles[0].targets[0];
    assert(process.env.BRIM_QPF_LIVE_TARGET);
    const targetBytes = fs.readFileSync(process.env.BRIM_QPF_LIVE_TARGET);
    assert.strictEqual(targetBytes.byteLength, entry.bytes);
    assert.strictEqual(crypto.createHash("sha256").update(targetBytes).digest("hex"), entry.sha256);
    assert.strictEqual(targetBytes.subarray(0, 4).toString("ascii"), "RIFF");
    assert.strictEqual(targetBytes.subarray(8, 12).toString("ascii"), "WEBP");
    const numericResults = [];
    for (const lead of [6, 54, 66, 240]) {
      const envName = `BRIM_QPF_LIVE_NUMERIC_F${String(lead).padStart(3, "0")}`;
      assert(process.env[envName], `${envName} is required for the f240 live smoke`);
      const numericEntry = live.cycles[0].targets.find((target) => target.lead_hours === lead);
      const numericBytes = fs.readFileSync(process.env[envName]);
      const frame = await api.decodeNumeric(arrayBuffer(numericBytes), numericEntry, numericDependencies);
      numericResults.push({
        leadHours: lead,
        compressedBytes: numericBytes.byteLength,
        cells: frame.values.length,
        forecastStateId: frame.forecast_state_id
      });
    }
    console.log(`QPF_LIVE_SMOKE ${JSON.stringify({
      manifestBytes: bytes.byteLength, retentionMode: live.retention_mode,
      currentCycleUtc: live.current_cycle_utc, leadCount: live.cycles[0].targets.length,
      targetPath: entry.image_path, targetBytes: targetBytes.byteLength,
      numericResults
    })}`);
  }

  console.log("NBM Snow Levels + QPF peer-layer deterministic consumer tests passed.");
}
main().catch((error) => { console.error(error); process.exitCode = 1; });
