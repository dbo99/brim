const assert = require("assert");
const fs = require("fs");
const path = require("path");

const repoRoot = path.join(__dirname, "..");
const read = (...parts) => fs.readFileSync(path.join(repoRoot, ...parts), "utf8");

const legend = read("03_functions", "leaflet_ops_live_legend_helpers.r");
const streamflow = read(
  "03_functions",
  "leaflet_ops_live_usgs_streamflow_helpers.r"
);
const groundwater = read(
  "03_functions",
  "leaflet_ops_live_usgs_groundwater_helpers.r"
);
const scan = read("03_functions", "leaflet_ops_live_scan_helpers.r");
const opsPanel = read(
  "03_functions",
  "leaflet_ops_live_panel_helpers.r"
);
const closeout = read("03_functions", "js", "brim_legend_closeout_helpers.js");
const measure = read(
  "03_functions",
  "js",
  "leaflet_tools_adddata_panel.js"
);
const springs = read(
  "03_functions",
  "js",
  "leaflet_springs_local_virtualized.js"
);

global.window = {};

function includesAll(source, values, label) {
  values.forEach((value) => {
    assert(
      source.includes(value),
      `${label} is missing required source contract: ${value}`
    );
  });
}

function embeddedRawJs(source) {
  const startToken = 'r"---(';
  const endToken = ')---"';
  const start = source.indexOf(startToken);
  const end = source.lastIndexOf(endToken);
  assert(start >= 0 && end > start, "embedded R raw JavaScript was not found");
  return source.slice(start + startToken.length, end);
}

includesAll(
  legend,
  [
    "function opsGroundwaterLegendBodyHtml()",
    "function opsUsgsStreamflowLegendBodyHtml()",
    "if (def.unifiedCard === true) return;"
  ],
  "shared legend renderer"
);

includesAll(
  streamflow,
  [
    "function ptUsgsCreateUnifiedCard",
    "L.control({position: 'bottomleft'})",
    "pt-ops-usgs-streamflow-card pt-map-legend-card",
    "opsUsgsStreamflowLegendBodyHtml()",
    "unifiedCard: true",
    "Q ≥100",
    "Q ≥1k",
    "Q ≥10k",
    "On BLM",
    "≤1 mi BLM",
    "≤5 mi BLM",
    "data-pt-usgsf-apply",
    "data-pt-usgsf-reset",
    "raw latest discharge magnitude",
    "projected CA Albers screening geometry",
    "not flood-stage",
    "in current view",
    "layerIsActive = false",
    "requestGeneration !== activationGeneration",
    "ptUsgsDestroy"
  ],
  "USGS streamflow unified card"
);
assert(
  !streamflow.includes("function ptUsgsCreateFilterControl"),
  "the retired separate streamflow filter control must not return"
);
assert(
  !streamflow.includes("root.style.left = '225px'") &&
    !streamflow.includes("root.style.top = '186px'"),
  "streamflow must not return to fixed top-left filter coordinates"
);
const streamCloseBlock = streamflow.slice(
  streamflow.indexOf(
    "var closeButton = root.querySelector('.pt-ops-usgs-streamflow-filter-close')"
  ),
  streamflow.indexOf(
    "var closeButton = root.querySelector('.pt-ops-usgs-streamflow-filter-close')"
  ) + 900
);
assert(streamCloseBlock.includes("root.style.display = 'none'"));
assert(!streamCloseBlock.includes("removeLayer"));
const streamApi = new Function(
  `${embeddedRawJs(streamflow)}
  return {
    filter: ptUsgsFilterFeatures,
    updateCount: ptUsgsUpdateFilterCount
  };`
)();
const streamFeatures = [
  {properties: {q_cfs: 50, stage_ft: 2, on_blm_ca: false, dist_to_blm_mi: 2}},
  {properties: {q_cfs: 500, stage_ft: 3, on_blm_ca: true, dist_to_blm_mi: 0}},
  {properties: {q_cfs: 2000, stage_ft: 4, on_blm_ca: false, dist_to_blm_mi: 4}},
  {properties: {q_cfs: null, stage_ft: 5, on_blm_ca: false, dist_to_blm_mi: 8}}
];
assert.strictEqual(streamApi.filter(streamFeatures, {}).length, 4);
assert.strictEqual(
  streamApi.filter(streamFeatures, {flowMin: "100"}).length,
  2,
  "streamflow minimum cfs must exclude stage-only and lower-flow records"
);
assert.strictEqual(
  streamApi.filter(streamFeatures, {flowMin: "100", onBlmOnly: true}).length,
  1,
  "streamflow filters must combine across fields with AND semantics"
);
assert.strictEqual(
  streamApi.filter(streamFeatures, {blmDistMax: "1"}).length,
  1,
  "streamflow BLM-distance filtering must retain only nearby records"
);
const streamCountNode = {textContent: ""};
streamApi.updateCount(
  {querySelector: () => streamCountNode},
  2,
  4,
  2,
  1
);
assert(streamCountNode.textContent.includes("Showing 2 of 4"));
assert(streamCountNode.textContent.includes("1 in current view"));

includesAll(
  groundwater,
  [
    "function ptUsgwCreateUnifiedCard",
    "L.control({position: 'bottomleft'})",
    "pt-ops-usgs-groundwater-card pt-map-legend-card",
    "opsGroundwaterLegendBodyHtml()",
    "unifiedCard: true",
    "Last 90d",
    "Last 1y",
    "DTW ≥500",
    "DTW ≤25",
    "On BLM",
    "≤1 mi BLM",
    "≤5 mi BLM",
    "nested/co-located only",
    "data-pt-usgw-apply",
    "data-pt-usgw-reset",
    "negative values are reported artesian",
    "records in current view",
    "layerIsActive = false",
    "requestGeneration !== activationGeneration",
    "ptUsgwDestroy"
  ],
  "USGS groundwater unified card"
);
assert(
  !groundwater.includes("function ptUsgwCreateFilterControl"),
  "the retired separate groundwater filter control must not return"
);
assert(
  !groundwater.includes("root.style.left = '225px'") &&
    !groundwater.includes("root.style.top = '172px'"),
  "groundwater must not return to fixed top-left filter coordinates"
);
const groundwaterCloseBlock = groundwater.slice(
  groundwater.indexOf(
    "var closeButton = root.querySelector('.pt-ops-usgs-groundwater-filter-close')"
  ),
  groundwater.indexOf(
    "var closeButton = root.querySelector('.pt-ops-usgs-groundwater-filter-close')"
  ) + 900
);
assert(groundwaterCloseBlock.includes("root.style.display = 'none'"));
assert(!groundwaterCloseBlock.includes("removeLayer"));
const groundwaterApi = new Function(
  `${embeddedRawJs(groundwater)}
  return {
    filter: ptUsgwFilterFeatures,
    updateCount: ptUsgwUpdateFilterCount
  };`
)();
const groundwaterFeatures = [
  {
    properties: {
      latest_wl_ft_bgs: -5,
      latest_age_days: 30,
      on_blm_ca: true,
      dist_to_blm_mi: 0.2,
      _pt_usgw_colocated_count: 2
    }
  },
  {
    properties: {
      latest_wl_ft_bgs: 600,
      latest_age_days: 200,
      on_blm_ca: false,
      dist_to_blm_mi: 2,
      _pt_usgw_colocated_count: 1
    }
  },
  {
    properties: {
      latest_wl_ft_bgs: 100,
      latest_age_days: 500,
      on_blm_ca: true,
      dist_to_blm_mi: 5,
      _pt_usgw_colocated_count: 2
    }
  }
];
assert.strictEqual(groundwaterApi.filter(groundwaterFeatures, {}).length, 3);
assert.strictEqual(
  groundwaterApi.filter(groundwaterFeatures, {ageMax: "90"}).length,
  1
);
assert.strictEqual(
  groundwaterApi.filter(groundwaterFeatures, {depthMin: "500"}).length,
  1
);
assert.strictEqual(
  groundwaterApi.filter(groundwaterFeatures, {
    onBlmOnly: true,
    blmDistMax: "1",
    nestedOnly: true
  }).length,
  1,
  "groundwater filters must combine across fields with AND semantics"
);
const groundwaterCountNode = {textContent: ""};
groundwaterApi.updateCount(
  {querySelector: () => groundwaterCountNode},
  1,
  3,
  1,
  1
);
assert(groundwaterCountNode.textContent.includes("Showing 1 of 3"));
assert(groundwaterCountNode.textContent.includes("1 records in current view"));

includesAll(
  closeout,
  [
    "pt-ops-usgs-streamflow-card",
    "pt-ops-usgs-groundwater-card",
    "pt-tools-adddata-wrap",
    "pt-local-upload-wrap",
    "((availableHeight - stackHeight) / 2)",
    "pt-map-legend-corner-overflow",
    "window.BRIM.legendCloseout.scheduleLayout",
    "new window.ResizeObserver"
  ],
  "shared detachable-card layout"
);
const actionsStart = closeout.indexOf(
  "window.BRIM.legendCloseout.actionsHtml"
);
const actionsEnd = closeout.indexOf(
  "window.BRIM.legendCloseout.makeDetachable",
  actionsStart
);
const actionsBlock = closeout.slice(actionsStart, actionsEnd);
assert(
  actionsBlock.indexOf("pt-map-card-dock") <
    actionsBlock.indexOf("window.BRIM.legendCloseout.buttonHtml"),
  "the dock action must remain immediately before the close action"
);

const scanMarkerStart = scan.indexOf(
  "var marker = L.circleMarker([coords.lat, coords.lng]"
);
assert(scanMarkerStart >= 0, "SCAN marker construction was not found");
const scanMarkerBlock = scan.slice(scanMarkerStart, scanMarkerStart + 1800);
includesAll(
  scanMarkerBlock,
  [
    "pane: 'pane_ops'",
    "interactive: true",
    "marker.bindTooltip",
    "marker.on('mouseover'",
    "marker.bindPopup(popup"
  ],
  "SCAN marker interaction"
);
includesAll(
  scan,
  [
    "Open official NRCS station page",
    "Depth tabs below show latest value",
    "_ptScanIsRemoved",
    "requestGeneration !== self._ptScanActivationGeneration",
    "layer.onRemove"
  ],
  "SCAN rich-popup and lifecycle"
);

includesAll(
  springs,
  ["L.canvas({", "pane: 'pane_points'"],
  "regression fixture for the higher springs canvas"
);
includesAll(
  measure,
  [
    "data-pt-measure-suspended",
    "pane.style.pointerEvents = 'none'",
    "state.pane.style.pointerEvents = state.pointerEvents"
  ],
  "Measure pane suspend/restore lifecycle"
);
includesAll(
  opsPanel,
  [
    "function ptClearOpsLayers",
    "map.removeLayer(layer)",
    "activeLegendDefs = {}",
    "layer.forceRemove(map)"
  ],
  "Clear Ops shared removal lifecycle"
);

console.log("Ops Live unified-card and SCAN source fixtures passed.");
