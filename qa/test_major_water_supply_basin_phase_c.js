#!/usr/bin/env node

"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");

const root = path.resolve(__dirname, "..");
const helperSource = fs.readFileSync(path.join(root, "03_functions", "leaflet_ops_live_major_water_supply_basin_helpers.r"), "utf8");
const layerDefinitionSource = fs.readFileSync(path.join(root, "03_functions", "leaflet_ops_live_layer_definition_helpers.r"), "utf8");
const panelSource = fs.readFileSync(path.join(root, "03_functions", "leaflet_ops_live_panel_helpers.r"), "utf8");

function parseCsv(text) {
  const rows = [];
  let row = [], field = "", quoted = false;
  for (let i = 0; i < text.length; i += 1) {
    const char = text[i];
    if (quoted) {
      if (char === '"' && text[i + 1] === '"') { field += '"'; i += 1; }
      else if (char === '"') quoted = false;
      else field += char;
    } else if (char === '"') quoted = true;
    else if (char === ',') { row.push(field); field = ""; }
    else if (char === '\n') { row.push(field.replace(/\r$/, "")); rows.push(row); row = []; field = ""; }
    else field += char;
  }
  if (field || row.length) { row.push(field); rows.push(row); }
  const header = rows.shift();
  return rows.filter((values) => values.length === header.length).map((values) =>
    Object.fromEntries(header.map((name, index) => [name, values[index]]))
  );
}

const mapping = parseCsv(fs.readFileSync(path.join(root, "00_config", "major_water_supply_basin_product_mapping.csv"), "utf8"));
const catalog = parseCsv(fs.readFileSync(path.join(root, "00_config", "major_water_supply_basin_geometry_catalog.csv"), "utf8"));
const components = parseCsv(fs.readFileSync(path.join(root, "00_config", "major_water_supply_basin_component_manifest.csv"), "utf8"));
const crosswalk = parseCsv(fs.readFileSync(path.join(root, "00_config", "major_water_supply_basin_reservoir_crosswalk.csv"), "utf8"));
const links = parseCsv(fs.readFileSync(path.join(root, "00_config", "major_water_supply_basin_related_links.csv"), "utf8"));

assert.strictEqual(mapping.length, 54);
assert.strictEqual(new Set(mapping.map((row) => row.forecast_key)).size, 54);
assert.strictEqual(catalog.length, 23);
assert.strictEqual(components.length, 18);
assert(components.every((row) => row.operation === "union" && row.required === "TRUE"));
assert(components.every((row) => catalog.some((item) => item.geometry_id === row.derived_geometry_id && item.source_url)));
assert.strictEqual(crosswalk.length, 14);
assert.strictEqual(links.length, 21);
assert.strictEqual(mapping.filter((row) => row.geometry_id === "BDBC1_FNF").length, 0);
assert.strictEqual(mapping.filter((row) => /^HUC2_/.test(row.geometry_id)).length, 0);
const forecastRowOffset = layerDefinitionSource.indexOf("panelLabel: 'Water-Supply Basin Forecasts | CNRFC / CBRFC'");
const reservoirRowOffset = layerDefinitionSource.indexOf("name: 'Reservoirs | storage-centric | CDEC / CNRFC / USACE'");
assert(forecastRowOffset >= 0 && forecastRowOffset < reservoirRowOffset, "forecast catalog row must immediately precede the reservoir registration");
assert.match(panelSource, /escapeHtml\(def\.panelLabel \|\| def\.name\)/);

const rawStart = helperSource.indexOf('r"---(');
const rawEnd = helperSource.lastIndexOf(')---"');
assert(rawStart >= 0 && rawEnd > rawStart);
const browserSource = helperSource.slice(rawStart + 'r"---('.length, rawEnd);

let capturedLayerProto = null;
function layerConstructor(proto) {
  capturedLayerProto = proto;
  function Layer(options) { if (proto.initialize) proto.initialize.call(this, options); }
  Layer.prototype = proto;
  return Layer;
}
const windowObject = {BRIM: {}};
const documentObject = {getElementById() { return null; }, createElement() { return {}; }, head: {appendChild() {}}};
const L = {Layer: {extend: layerConstructor}, layerGroup() { return {}; }, geoJSON() { return {}; }};
const api = new Function(
  "window", "document", "L", "MAJOR_WATER_SUPPLY_PRODUCT_MAPPING",
  "MAJOR_WATER_SUPPLY_GEOMETRY_CATALOG", "MAJOR_WATER_SUPPLY_COMPONENT_MANIFEST", "MAJOR_WATER_SUPPLY_RESERVOIR_CROSSWALK",
  "MAJOR_WATER_SUPPLY_RELATED_LINKS", "escapeHtml", "setOpsLayerLoading", "recordStatus",
  browserSource + "\nreturn window.BRIM.majorWaterSupplyForecast;"
)(windowObject, documentObject, L, mapping, catalog, components, crosswalk, links, String, () => {}, () => {});
assert(capturedLayerProto, "major-basin controller prototype was not captured");

function state(status, mapEligible, popupEligible = true, missingReason = null) {
  return {status, value_origin: status === "unavailable" ? "none" : "current_source", source_issue_at: "2026-07-31T07:40:00-07:00", valid_through: "2026-08-07T14:40:00Z", stale_since: null, map_eligible: mapEligible, popup_eligible: popupEligible, missing_reason: missingReason};
}

function cnrfcFixture() {
  const rows = mapping.filter((row) => row.source_family === "CNRFC");
  return {
    schema_version: "1.0", product_id: "major_water_supply_basin_forecasts",
    roster_version: "cnrfc-major-water-supply-v1.1.0", publication_mode: "steady_state",
    generated_at: "2026-08-01T15:42:06Z",
    expected_record_count: 51, actual_record_count: 51,
    family_health: {
      water_year_fnf: {health: "healthy"}, water_year_index: {health: "healthy"},
      ten_day_streamflow_volume_accumulation: {health: "healthy"},
      april_july_streamflow_volume_forecast: {health: "healthy"}
    },
    records: rows.map((row) => {
      const short = row.product_family === "ten_day_accumulation";
      const april = row.product_family === "april_july";
      const unavailable = row.forecast_key === "CNRFC:MHBC1:10D_VOLUME_ACCUM";
      const status = unavailable ? "unavailable" : (april ? "expired" : "current");
      const eligible = status === "current";
      const metrics = short ? [
        "day_3_median_volume", "day_5_median_volume", "day_10_median_volume",
        "day_3_deterministic_volume", "day_5_deterministic_volume"
      ] : (april ? ["forecast_volume", "normal_average_volume", "percent_average"] : ["forecast_volume", "percent_mean", "percent_median"]);
      const record = {
        forecast_key: row.forecast_key, product_type: row.product_type,
        display_name: row.forecast_key.split(":")[1],
        water_year: 2026,
        forecast_period: april ? "April-July" : (short ? "Short range" : "2026 Water Year"),
        forecast_statistic: april ? "50_percent_exceedance" : (short ? "ten_day_streamflow_volume_accumulation" : "median"),
        forecast_issued_at: "2026-07-31T07:40:00-07:00",
        source_url: "https://example.test/official/" + encodeURIComponent(row.forecast_key),
        retrieval_url: "https://example.test/data/" + encodeURIComponent(row.forecast_key),
        summary_url: "https://example.test/summary/" + encodeURIComponent(row.forecast_key),
        archive_url: "https://example.test/archive/" + encodeURIComponent(row.forecast_key),
        status, metric_state: {}
      };
      metrics.forEach((metric, index) => {
        record[metric] = unavailable ? null : (metric.startsWith("percent_") ? 90 + index : 80 + index);
        record.metric_state[metric] = state(status, eligible, !unavailable, unavailable ? "CNRFC does not publish this short-range product at this location." : null);
      });
      return record;
    })
  };
}

function cnrfcFixtureWithProduct7Median() {
  const payload = cnrfcFixture();
  let index = 0;
  payload.records.filter((record) => record.product_type === "april_july_streamflow_volume_forecast").forEach((record) => {
    record.percent_median = index === 0 ? 0 : (index === 1 ? 124 : 70 + index);
    record.metric_state.percent_median = state(record.status, record.status === "current", true);
    index += 1;
  });
  return payload;
}

const months = ["2026-07","2026-08","2026-09","2026-10","2026-11","2026-12","2027-01","2027-02","2027-03","2027-04","2027-05","2027-06"];
function cbrfcFixture() {
  const glApril = {forecast_key: api.cbrfcKeys[0], product_type: "april_july_water_supply_forecast", display_name: "Lake Powell April–July", forecast_period: "Apr 1-Jul 31", forecast_volume: 1080, percent_average: 17, percent_median: 18, status: "expired", metric_state: {forecast_volume: state("expired", false), percent_average: state("expired", false), percent_median: state("expired", false)}};
  const glWy = {forecast_key: api.cbrfcKeys[1], product_type: "water_year_unregulated_inflow_forecast", display_name: "Lake Powell water year", forecast_period: "Water Year", forecast_volume: 3481, percent_average: 36, status: "current", metric_state: {forecast_volume: state("current", true), percent_average: state("current", true)}, diagnostic: {observed_to_date_policy: "available_on_summary_not_published"}};
  const local = {
    forecast_key: api.cbrfcKeys[2], product_type: "lake_mead_local_intervening_monthly_forecast", display_name: "Lake Mead Local", forecast_period: "MONTHLY OUTLOOKS", status: "current_partial",
    forecast_issue_date: "2026-07-01", source_url: "https://example.test/lake-mead", retrieval_url: "https://example.test/lake-mead", summary_url: "https://example.test/lake-mead-summary", archive_url: "https://example.test/lake-mead-archive",
    monthly_forecasts: months.map((month, index) => {
      const current = month === "2026-08", corrected = month === "2027-01";
      return {
        raw_forecast_month_label: corrected ? "January 2026" : month, forecast_month: month,
        source_date_override_applied: corrected,
        source_date_override_id: corrected ? "CBRFC_LKSA3_LOCAL_JANUARY_ROLLOVER_2026" : null,
        source_date_override_reason: corrected ? "Reviewed rollover correction." : null,
        source_date_override_evidence_url: corrected ? "https://example.test/evidence" : null,
        forecast_volume: current ? 72 : 60 + index, percent_median: current ? 83 : 90,
        status: current ? "current" : (index === 0 ? "expired" : "not_yet_valid"),
        metric_state: {forecast_volume: state(current ? "current" : (index === 0 ? "expired" : "not_yet_valid"), current), percent_median: state(current ? "current" : (index === 0 ? "expired" : "not_yet_valid"), current)}
      };
    })
  };
  return {schema_version: "1.0", product_id: "cbrfc_major_water_supply_forecasts", roster_version: "cbrfc-colorado-river-v1.3.0", generated_at: "2026-08-01T15:30:01Z", publication_mode: "bootstrap", expected_record_count: 3, actual_record_count: 3, family_health: {april_july_water_supply_forecast: {health: "healthy"}, water_year_unregulated_inflow_forecast: {health: "healthy"}, lake_mead_local_intervening_monthly_forecast: {health: "healthy"}}, records: [glApril, glWy, local]};
}

const cnrfc = cnrfcFixture(), cbrfc = cbrfcFixture();
assert.strictEqual(api.validateCnrfc(cnrfc), cnrfc);
assert.strictEqual(api.validateCbrfc(cbrfc), cbrfc);
const cnrfcWithProduct7Median = cnrfcFixtureWithProduct7Median();
assert.strictEqual(api.validateCnrfc(cnrfcWithProduct7Median), cnrfcWithProduct7Median);
assert.deepStrictEqual(api.viewCapabilities(cnrfc, cbrfc), {cnrfcAprilJulyPercentMedian: false, comparisonAprilJulyPercentMedian: false});
assert.deepStrictEqual(api.viewCapabilities(cnrfcWithProduct7Median, cbrfc), {cnrfcAprilJulyPercentMedian: true, comparisonAprilJulyPercentMedian: true});
assert.strictEqual(api.cnrfcKeys.length, 51);
assert.deepStrictEqual(api.cbrfcKeys, ["CBRFC:GLDA3:APR_JUL_WSUP", "CBRFC:GLDA3:WATER_YEAR_INFLOW", "CBRFC:LKSA3:LOCAL_INTERVENING_MONTHLY"]);

function rejected(mutator, validator, pattern) {
  const fixture = validator === api.validateCnrfc ? cnrfcFixture() : cbrfcFixture();
  mutator(fixture);
  assert.throws(() => validator(fixture), pattern);
}
rejected((p) => { p.schema_version = "2"; }, api.validateCnrfc, /schema/);
rejected((p) => { p.product_id = "wrong"; }, api.validateCnrfc, /product/);
rejected((p) => { p.roster_version = "wrong"; }, api.validateCnrfc, /roster/);
rejected((p) => { p.records[1].forecast_key = p.records[0].forecast_key; }, api.validateCnrfc, /Duplicate/);
rejected((p) => { p.records.reverse(); }, api.validateCnrfc, /order\/key/);
rejected((p) => { p.records[0].forecast_volume = NaN; }, api.validateCnrfc, /non-finite/);
rejected((p) => { p.records[0].geometry = {}; }, api.validateCnrfc, /spatial/);
rejected((p) => { delete p.records[0].metric_state.percent_mean; }, api.validateCnrfc, /missing metric state/);
rejected((p) => { p.records[0].forecast_statistic = "deterministic"; }, api.validateCnrfc, /forecast_statistic=median/);
rejected((p) => { p.records.find((record) => record.product_type === "april_july_streamflow_volume_forecast").forecast_statistic = "deterministic"; }, api.validateCnrfc, /forecast_statistic=50_percent_exceedance/);
rejected((p) => { const record = p.records.find((item) => item.product_type === "april_july_streamflow_volume_forecast"); record.percent_median = 59; }, api.validateCnrfc, /value\/state pair is incomplete/);
rejected((p) => { const record = p.records.find((item) => item.product_type === "april_july_streamflow_volume_forecast"); record.percent_median = 59; record.metric_state.percent_median = state("expired", false); }, api.validateCnrfc, /all 18/);
assert.throws(() => { const p = cnrfcFixtureWithProduct7Median(); p.records.find((record) => record.product_type === "april_july_streamflow_volume_forecast").percent_median = Infinity; api.validateCnrfc(p); }, /non-finite/);
assert.throws(() => { const p = cnrfcFixtureWithProduct7Median(); const record = p.records.find((item) => item.product_type === "april_july_streamflow_volume_forecast"); delete record.metric_state.percent_median.valid_through; api.validateCnrfc(p); }, /missing valid_through/);
assert.doesNotThrow(() => {
  const p = cnrfcFixtureWithProduct7Median();
  const record = p.records.find((item) => item.product_type === "april_july_streamflow_volume_forecast");
  record.percent_median = null;
  record.metric_state.percent_median = state("unavailable", false, false, "Official Product 7 headline did not include percent of median.");
  api.validateCnrfc(p);
});
rejected((p) => { p.records[1].metric_state.percent_median = state("current", true); p.records[1].percent_median = 99; }, api.validateCbrfc, /must not publish/);
rejected((p) => { p.records[2].monthly_forecasts.pop(); }, api.validateCbrfc, /exactly 12/);
rejected((p) => { p.records[2].forecast_volume = 800; }, api.validateCbrfc, /aggregate/);
rejected((p) => { p.records[1].observed_to_date = 123; }, api.validateCbrfc, /observed-to-date/);

assert.strictEqual(api.metricEligible({status: "current", map_eligible: false}), false);
assert.strictEqual(api.metricEligible({status: "expired", map_eligible: true}), true);
const selected = api.selectedMonth(cbrfc.records[2], "forecast_volume");
assert.strictEqual(selected.forecast_month, "2026-08");
assert.strictEqual(selected.forecast_volume, 72);
assert.strictEqual(selected.percent_median, 83);
assert.strictEqual(cbrfc.records[2].monthly_forecasts.length, 12);
assert.strictEqual(cbrfc.records[2].monthly_forecasts[6].raw_forecast_month_label, "January 2026");
assert.strictEqual(cbrfc.records[2].monthly_forecasts[6].forecast_month, "2027-01");

assert.strictEqual(api.formatKaf(999), "999 kaf");
assert.match(api.formatKaf(3481), /3\.481 MAF/);
assert.strictEqual(api.legendUnit(999), "kaf");
assert.strictEqual(api.legendUnit(1000), "MAF");
assert.strictEqual(api.percentClasses.length, 7);
assert.strictEqual(api.percentClass(36).label, "< 50%");
assert.strictEqual(api.volumeDomain([10,20,30,40], {minimumSample: 5}).mode, "insufficient");
assert.strictEqual(api.volumeDomain([72], {minimumSample: 1, singleMidpoint: true}).mode, "midpoint");
assert.strictEqual(api.volumeDomain([3481,16000], {minimumSample: 2}).mode, "range");
assert.strictEqual(api.volumeColor(72, {mode: "midpoint", min: 72, max: 72}), "#6baed6");

let config = api.viewConfig("ca_major", "water_year", "forecast_volume", "day_3");
assert.deepStrictEqual([config.viewKey, config.periodKey, config.measure], ["ca_major", "water_year", "forecast_volume"]);
assert.strictEqual(config.view.label, "California forecast basins");
assert.strictEqual(api.viewConfig("ca_indices", "water_year", "forecast_volume", "").view.label, "California forecast indices");
assert.deepStrictEqual(api.indexOptions, [
  ["SACC0_FNF", "Sacramento Valley"],
  ["VNSC0_FNF", "San Joaquin Valley"],
  ["MLIC0_FNF", "Central Valley"]
]);
const expectedIndexKeys = [
  "CNRFC:SACC0:WY_INDEX", "CNRFC:VNSC0:WY_INDEX", "CNRFC:MLIC0:WY_INDEX",
  "CNRFC:SACC0:APR_JUL_VOLUME", "CNRFC:VNSC0:APR_JUL_VOLUME", "CNRFC:MLIC0:APR_JUL_VOLUME"
];
assert.deepStrictEqual(mapping.filter((row) => row.view_applicability === "california_indexes").map((row) => row.forecast_key), expectedIndexKeys);
["SACC0_FNF", "VNSC0_FNF", "MLIC0_FNF"].forEach((geometryId) => {
  ["water_year", "april_july"].forEach((period) => {
    const indexConfig = api.viewConfig("ca_indices", period, "forecast_volume", "");
    const rows = api.selectionRows(indexConfig, {indexGeometryId: geometryId});
    assert.strictEqual(rows.length, 1, `${geometryId}/${period} must resolve one literal index record`);
    assert.strictEqual(rows[0].geometry_id, geometryId);
  });
});
assert.doesNotMatch(browserSource, /California major basins|label: 'California indices'/);
config = api.viewConfig("ca_major", "short_range", "deterministic", "day_10");
assert.strictEqual(config.horizon, "day_3");
assert.strictEqual(config.horizons.some((row) => row[0] === "day_10"), false);
assert.deepStrictEqual(config.horizons.map((row) => row[1]), ["3-day total", "5-day total"]);
assert.strictEqual(api.accumulationLabel("day_10", true), "10-day total volume");
assert.deepStrictEqual(config.period.measures.map((row) => row[0]), ["median", "deterministic"]);
[
  ["ca_major", "water_year"], ["ca_major", "april_july"],
  ["ca_indices", "water_year"], ["ca_indices", "april_july"],
  ["colorado", "glda_april_july"], ["colorado", "glda_water_year"], ["colorado", "lksa_current_month"],
  ["comparison", "water_year"], ["comparison", "april_july"]
].forEach(([view, period]) => {
  const longRange = api.viewConfig(view, period, "deterministic", "day_5");
  assert.strictEqual(longRange.period.measures.some((row) => row[0] === "deterministic"), false, `${view}/${period} exposed deterministic`);
  assert.notStrictEqual(longRange.measure, "deterministic", `${view}/${period} retained deterministic state`);
  assert.strictEqual(longRange.horizon, "", `${view}/${period} retained a short-range accumulation period`);
});
assert.strictEqual(api.viewConfig("ca_major", "water_year", "deterministic", "day_5").measure, "forecast_volume");
assert.strictEqual(api.viewConfig("ca_major", "april_july", "deterministic", "day_5").measure, "forecast_volume");
const futureCapabilities = api.viewCapabilities(cnrfcWithProduct7Median, cbrfc);
const currentAprilConfig = api.viewConfig("ca_major", "april_july", "percent_median", "", api.viewCapabilities(cnrfc, cbrfc));
assert.strictEqual(currentAprilConfig.measure, "forecast_volume", "current feed must not expose an absent Product 7 metric");
const futureBasinAprilConfig = api.viewConfig("ca_major", "april_july", "percent_median", "", futureCapabilities);
const futureIndexAprilConfig = api.viewConfig("ca_indices", "april_july", "percent_median", "", futureCapabilities);
const futureComparisonAprilConfig = api.viewConfig("comparison", "april_july", "percent_median", "", futureCapabilities);
[futureBasinAprilConfig, futureIndexAprilConfig, futureComparisonAprilConfig].forEach((futureConfig) => {
  assert.strictEqual(futureConfig.measure, "percent_median");
  assert.strictEqual(futureConfig.period.measures[2][1], "Percent of median");
});
const noCbrfcMedian = cbrfcFixture();
delete noCbrfcMedian.records[0].percent_median;
delete noCbrfcMedian.records[0].metric_state.percent_median;
const noComparisonCapability = api.viewCapabilities(cnrfcWithProduct7Median, noCbrfcMedian);
assert.strictEqual(noComparisonCapability.cnrfcAprilJulyPercentMedian, true);
assert.strictEqual(noComparisonCapability.comparisonAprilJulyPercentMedian, false);
assert.strictEqual(api.viewConfig("comparison", "april_july", "percent_median", "", noComparisonCapability).measure, "forecast_volume");
config = api.viewConfig("comparison", "short_range", "percent_median", "");
assert.strictEqual(config.periodKey, "april_july");
assert.strictEqual(config.measure, "forecast_volume");

function visibleText(html) {
  return html.replace(/<[^>]*>/g, " ").replace(/&amp;/g, "&").replace(/\s+/g, " ").trim();
}

const waterYearRow = mapping.find((row) => row.source_family === "CNRFC" && row.product_family === "water_year");
const waterYearRecord = cnrfc.records.find((record) => record.forecast_key === waterYearRow.forecast_key);
const waterYearParts = api.recordForecastParts(waterYearRecord, waterYearRow, {selectedProduct: true, selectedMetric: "forecast_volume"});
const waterYearVisible = visibleText(waterYearParts.summary);
assert.match(waterYearParts.summary, /pt-major-product-card/);
assert.match(waterYearParts.summary, /Forecast volume/);
assert.match(waterYearParts.summary, /Percent of mean/);
assert.match(waterYearParts.summary, /Percent of median/);
assert.match(waterYearParts.summary, /Median ensemble forecast/);
assert.doesNotMatch(waterYearParts.summary, /Deterministic/i);
assert.match(waterYearParts.summary, /is-selected/);
assert.match(waterYearParts.summary, /pt-major-metric-forecast-volume is-selected/);
assert(waterYearParts.summary.indexOf("Forecast volume") < waterYearParts.summary.indexOf("Percent of mean"));
assert(waterYearParts.summary.indexOf("Percent of mean") < waterYearParts.summary.indexOf("Percent of median"));
assert.strictEqual((waterYearVisible.match(/Issued/g) || []).length, 1, "shared issue time should appear once in a product summary");
assert.strictEqual((waterYearVisible.match(/valid through/g) || []).length, 1, "shared validity should appear once in a product summary");
assert.doesNotMatch(waterYearVisible, /2026-07-31T/, "raw ISO timestamps must not dominate the visible summary");
assert.match(waterYearVisible, /Jul 31, 2026, 7:40 AM PDT/);
assert.doesNotMatch(waterYearParts.summary, /Geometry role|Product identifier/);
assert.match(waterYearParts.provenance, /Product identifier/);
assert.match(waterYearParts.provenance, /Official forecast/);
assert.match(waterYearParts.provenance, /Structured data/);
assert.match(waterYearParts.provenance, /Summary dashboard/);
assert.match(waterYearParts.provenance, /Forecast archive/);
assert.match(waterYearParts.provenance, /Source terminology[\s\S]*Percent mean; Percent median/);
assert.match(waterYearParts.provenance, /Forecast statistic[\s\S]*Median ensemble forecast/);
assert.doesNotMatch(waterYearParts.provenance, /producer source|>retrieval<|>summary<|>archive</i);

const waterYearMeanParts = api.recordForecastParts(waterYearRecord, waterYearRow, {selectedProduct: true, selectedMetric: "percent_mean"});
const waterYearMedianParts = api.recordForecastParts(waterYearRecord, waterYearRow, {selectedProduct: true, selectedMetric: "percent_median"});
assert.match(waterYearMeanParts.summary, /pt-major-metric-percent-mean is-selected/);
assert.match(waterYearMedianParts.summary, /pt-major-metric-percent-median is-selected/);

const aprilRow = mapping.find((row) => row.source_family === "CNRFC" && row.product_family === "april_july");
const aprilRecord = cnrfc.records.find((record) => record.forecast_key === aprilRow.forecast_key);
const aprilParts = api.recordForecastParts(aprilRecord, aprilRow, {selectedProduct: true, selectedMetric: "forecast_volume"});
assert.match(aprilParts.summary, /April–July 2026/);
assert.match(aprilParts.summary, /Forecast period ended/);
assert.match(aprilParts.summary, /50% exceedance forecast/);
assert.doesNotMatch(aprilParts.summary, /Deterministic/i);
assert.match(aprilParts.summary, /Forecast volume/);
assert.match(aprilParts.summary, /Percent of mean/);
assert.match(aprilParts.summary, /Percent of median/);
assert.match(aprilParts.summary, /Percent of median: Not included in accepted feed/);
assert.match(aprilParts.summary, /Mean reference volume: <strong>81 kaf<\/strong>/);
assert(aprilParts.summary.indexOf("Forecast volume") < aprilParts.summary.indexOf("Percent of mean"));
assert(aprilParts.summary.indexOf("Percent of mean") < aprilParts.summary.indexOf("Percent of median"));
assert(aprilParts.summary.indexOf("Percent of median") < aprilParts.summary.indexOf("Mean reference volume"));
assert.doesNotMatch(aprilParts.summary, /Percent average|Percent of average|Percent mean\b|Percent median\b|Normal-average volume/);
assert.match(aprilParts.provenance, /Source terminology[\s\S]*Median Forecast; Normal-average volume; Percent of Mean/);
assert.match(aprilParts.provenance, /Forecast statistic[\s\S]*50% Exceedance/);

const aprilMeanParts = api.recordForecastParts(aprilRecord, aprilRow, {selectedProduct: true, selectedMetric: "percent_average"});
const aprilMissingMedianParts = api.recordForecastParts(aprilRecord, aprilRow, {selectedProduct: true, selectedMetric: "percent_median"});
assert.match(aprilMeanParts.summary, /pt-major-metric-percent-mean is-selected/);
assert.doesNotMatch(aprilMissingMedianParts.summary, /pt-major-metric-percent-median is-selected/);
assert.doesNotMatch(aprilMissingMedianParts.summary, />0(?:\.0)?%<\/strong><span>Percent of median/);
const futureAprilRecord = cnrfcWithProduct7Median.records.find((record) => record.forecast_key === aprilRow.forecast_key);
const futureAprilParts = api.recordForecastParts(futureAprilRecord, aprilRow, {selectedProduct: true, selectedMetric: "percent_median"});
assert.match(futureAprilParts.summary, /pt-major-metric-percent-median is-selected/);
assert.match(futureAprilParts.summary, /<strong>0%<\/strong><span>Percent of median<\/span>/, "direct zero percent must remain valid");
assert.match(futureAprilParts.provenance, /Median Forecast; Normal-average volume; Percent of Mean; Percent of Median/);

const powellWaterYearRow = mapping.find((row) => row.forecast_key === api.cbrfcKeys[1]);
const powellWaterYearParts = api.recordForecastParts(cbrfc.records[1], powellWaterYearRow, {selectedProduct: true, selectedMetric: "forecast_volume"});
assert.match(powellWaterYearParts.summary, /Percent of mean/);
assert.match(powellWaterYearParts.summary, /Percent of median: Not included in accepted feed/);
assert.doesNotMatch(powellWaterYearParts.summary, />0(?:\.0)?%<\/strong><span>Percent of median/);
assert.match(powellWaterYearParts.provenance, /Source terminology[\s\S]*Percent average/);

const shortRow = mapping.find((row) => row.source_family === "CNRFC" && row.product_family === "ten_day_accumulation" && row.forecast_key !== "CNRFC:MHBC1:10D_VOLUME_ACCUM");
const shortRecord = cnrfc.records.find((record) => record.forecast_key === shortRow.forecast_key);
const shortParts = api.recordForecastParts(shortRecord, shortRow, {selectedProduct: true, selectedMetric: "day_5_deterministic_volume"});
assert.match(shortParts.summary, /Accumulation period/);
assert.match(shortParts.summary, /Median ensemble/);
assert.match(shortParts.summary, /Deterministic/);
["3-day total volume", "5-day total volume", "10-day total volume"].forEach((label) => assert.match(shortParts.summary, new RegExp(label)));
assert.match(shortParts.summary, /10-day total volume[\s\S]*Not included in accepted feed/);
assert.match(shortParts.summary, /Selected map metric/);
assert.doesNotMatch(shortParts.summary, />0(?:\.0)? kaf</);
const equalShortRecord = JSON.parse(JSON.stringify(shortRecord));
equalShortRecord.day_3_median_volume = 25;
equalShortRecord.day_3_deterministic_volume = 25;
const equalShortParts = api.recordForecastParts(equalShortRecord, shortRow, {selectedProduct: true, selectedMetric: "day_3_deterministic_volume"});
assert.match(equalShortParts.summary, /3-day total volume[\s\S]*25 kaf[\s\S]*25 kaf/);
assert.match(equalShortParts.summary, /Median ensemble[\s\S]*Deterministic/);

const unavailableRow = mapping.find((row) => row.forecast_key === "CNRFC:MHBC1:10D_VOLUME_ACCUM");
const unavailableRecord = cnrfc.records.find((record) => record.forecast_key === unavailableRow.forecast_key);
const unavailableParts = api.recordForecastParts(unavailableRecord, unavailableRow, {selectedProduct: true, selectedMetric: "day_3_median_volume"});
assert.match(unavailableParts.summary, /Unavailable/);
assert.match(unavailableParts.summary, /does not publish this short-range product/);
assert.match(unavailableParts.summary, /Not published/);
assert.doesNotMatch(unavailableParts.summary, />0(?:\.0)? kaf</);

const localRow = mapping.find((row) => row.forecast_key === api.cbrfcKeys[2]);
const localParts = api.recordForecastParts(cbrfc.records[2], localRow, {selectedProduct: true, selectedMetric: "forecast_volume"});
assert(localParts.summary.indexOf("August 2026") < localParts.summary.indexOf("Full 12-month outlook"), "current month must precede the full outlook disclosure");
assert.match(localParts.summary, /72 kaf · Percent of median: 83%/);
assert.doesNotMatch(localParts.summary, /Percent of mean/);
assert.match(localParts.summary, /<details class="pt-major-monthly-disclosure"><summary>Full 12-month outlook<\/summary>/);
assert.strictEqual((localParts.summary.match(/pt-major-month-/g) || []).length, 12);
const monthlyTableHtml = localParts.summary.slice(localParts.summary.indexOf("<table>"));
let previousMonthOffset = -1;
["July 2026","August 2026","September 2026","October 2026","November 2026","December 2026","January 2027","February 2027","March 2027","April 2027","May 2027","June 2027"].forEach((label) => {
  const offset = monthlyTableHtml.indexOf(label);
  assert(offset > previousMonthOffset, `${label} must remain in forecast order`);
  previousMonthOffset = offset;
});
assert.doesNotMatch(localParts.summary, /January 2026|Correction evidence|Reviewed rollover correction/);
assert.match(localParts.provenance, /Correction provenance/);
assert.match(localParts.provenance, /January 2026/);
assert.match(localParts.provenance, /Correction evidence/);
assert.match(localParts.summary, /not summed; no total-Lake-Mead value is inferred/);

assert.match(browserSource, /<details class="pt-major-provenance"><summary>Details &amp; provenance<\/summary>/);
assert.doesNotMatch(browserSource, /<details class="pt-major-provenance"\s+open/);
assert(browserSource.indexOf("Forecast summary") < browserSource.indexOf("Geometry &amp; methodology"), "forecast summary must precede technical geometry provenance");
assert.doesNotMatch(browserSource, /<h4>Identity<\/h4>/);
const popupSource = browserSource.slice(browserSource.indexOf("_popupHtml: function"), browserSource.indexOf("_contextPopup: function"));
assert.strictEqual((popupSource.match(/props\.display_name/g) || []).length, 1, "basin identity should be rendered once in the popup header");
assert.doesNotMatch(popupSource, /addEventListener|popupopen|role="tab"/);
assert.doesNotMatch(popupSource, /<button|<select/);
assert.match(browserSource, /scope="col"/);
assert.match(browserSource, /scope="row"/);
assert.match(browserSource, /aria-label="Selected map metric/);
assert.match(browserSource, /grid-template-columns:repeat\(3,minmax\(0,1fr\)\)/);
assert.doesNotMatch(browserSource, />Horizon<|\['day_3','Day 3'\]|\['day_5','Day 5'\]|\['day_10','Day 10'\]/);
assert.strictEqual(api.selectionRows(config).some((row) => /LKSA3/.test(row.forecast_key)), false);
config = api.viewConfig("colorado", "glda_water_year", "percent_median", "");
assert.strictEqual(config.measure, "forecast_volume");
assert.strictEqual(config.period.measures[1][1], "Percent of mean");
config = api.viewConfig("comparison", "water_year", "percent_average", "");
assert.strictEqual(config.period.measures[1][1], "Percent of mean");
const cnrfcIndex = Object.fromEntries(cnrfc.records.map((record) => [record.forecast_key, record]));
const cbrfcIndex = Object.fromEntries(cbrfc.records.map((record) => [record.forecast_key, record]));
const comparisonMeasurements = api.selectionRows(config).map((row) => api.measurement(row, config, cnrfcIndex, cbrfcIndex));
assert.deepStrictEqual(comparisonMeasurements.map((measurement) => measurement.metric), ["percent_mean", "percent_average"]);
assert(comparisonMeasurements.every((measurement) => measurement.state.map_eligible), "comparison must retain each producer's direct mean statistic eligibility");
const futureCnrfcIndex = Object.fromEntries(cnrfcWithProduct7Median.records.map((record) => [record.forecast_key, record]));
const futureComparisonMeasurements = api.selectionRows(futureComparisonAprilConfig).map((row) => api.measurement(row, futureComparisonAprilConfig, futureCnrfcIndex, cbrfcIndex));
assert.deepStrictEqual(futureComparisonMeasurements.map((measurement) => measurement.metric), ["percent_median", "percent_median"]);
assert.deepStrictEqual(futureComparisonMeasurements.map((measurement) => measurement.value), [87, 18]);

const evidenceConfig = api.viewConfig("ca_major", "april_july", "forecast_volume", "");
const evidenceMeasurement = {
  row: aprilRow,
  record: {...aprilRecord, source_url: "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=NDPC1&prodID=7"}
};
const evidenceSlot = {
  id: "CNRFC", url: "https://dbo99.github.io/brim-live-data-feeds/data/major_water_supply_basin_forecasts.json",
  accepted: cnrfc, acceptedAt: new Date("2026-08-01T16:00:00Z"), lastAttemptAt: new Date("2026-08-01T16:05:00Z"),
  state: "error", lastError: new Error("offline")
};
const evidenceContext = {
  _state: {focusedGeometryId: aprilRow.geometry_id},
  _contextualOfficialUrl: capturedLayerProto._contextualOfficialUrl,
  _familyHealthHtml: capturedLayerProto._familyHealthHtml
};
const compactHealthHtml = capturedLayerProto._compactHealthHtml.call(evidenceContext, evidenceSlot, false);
assert.match(compactHealthHtml, /^<button type="button"/);
assert.match(compactHealthHtml, /CNRFC retained/);
assert.match(compactHealthHtml, /aria-expanded="false"/);
assert.match(compactHealthHtml, /aria-controls="pt-major-status-CNRFC"/);
assert.match(compactHealthHtml, /CNRFC feed retained after refresh failure — activate for details and source links/);
assert.doesNotMatch(compactHealthHtml, /cnrfc-major-water-supply-v1\.1\.0/);
const sourceEvidenceHtml = capturedLayerProto._sourceEvidenceHtml.call(evidenceContext, evidenceSlot, evidenceConfig, [evidenceMeasurement]);
assert.match(sourceEvidenceHtml, /CNRFC source evidence/);
assert.match(sourceEvidenceHtml, /51\/51/);
assert.match(sourceEvidenceHtml, /cnrfc-major-water-supply-v1\.1\.0/);
assert.match(sourceEvidenceHtml, /water year fnf: healthy/);
assert.match(sourceEvidenceHtml, /Latest refresh failed; BRIM is continuing with the last accepted snapshot/);
assert.match(sourceEvidenceHtml, /BRIM Live canonical CNRFC feed/);
assert.match(sourceEvidenceHtml, /Official NOAA\/NWS CNRFC source/);
assert.match(sourceEvidenceHtml, /Official CNRFC forecast for selected product/);
assert.match(sourceEvidenceHtml, /ensembleProduct\.php\?id=NDPC1&prodID=7/);

const powellAprilConfig = api.viewConfig("colorado", "glda_april_july", "forecast_volume", "");
const powellAprilMeasurement = api.measurement(api.selectionRows(powellAprilConfig)[0], powellAprilConfig, cnrfcIndex, cbrfcIndex);
const originalAprilEligibility = powellAprilMeasurement.state.map_eligible;
assert.deepStrictEqual(api.displayState(powellAprilMeasurement, false), {eligible: false, current: false, reference: false});
assert.deepStrictEqual(api.displayState(powellAprilMeasurement, true), {eligible: true, current: false, reference: true});
assert.strictEqual(powellAprilMeasurement.state.map_eligible, originalAprilEligibility, "reference display must not mutate producer eligibility");

const powellWaterConfig = api.viewConfig("colorado", "glda_water_year", "forecast_volume", "");
const powellMeasurement = api.measurement(api.selectionRows(powellWaterConfig)[0], powellWaterConfig, cnrfcIndex, cbrfcIndex);
assert.deepStrictEqual(api.displayState(powellMeasurement, false), {eligible: true, current: true, reference: false});
assert.strictEqual(powellMeasurement.row.geometry_id, "GLDA3_CBRFC_MODELED_UPSTREAM");

const meadConfig = api.viewConfig("colorado", "lksa_current_month", "selected_month_forecast_volume", "");
const meadMeasurement = api.measurement(api.selectionRows(meadConfig)[0], meadConfig, cnrfcIndex, cbrfcIndex);
assert.deepStrictEqual(api.displayState(meadMeasurement, false), {eligible: true, current: true, reference: false});
assert.strictEqual(meadMeasurement.row.geometry_id, "LKSA3_CBRFC_LOCAL_INTERVENING");
assert.strictEqual(meadMeasurement.value, 72);

[powellAprilConfig, powellWaterConfig, meadConfig, powellWaterConfig].forEach((transitionConfig, index) => {
  const rows = api.selectionRows(transitionConfig);
  assert.strictEqual(rows.length, 1, `Colorado transition ${index} must retain one selected product`);
  const measurement = api.measurement(rows[0], transitionConfig, cnrfcIndex, cbrfcIndex);
  const display = api.displayState(measurement, transitionConfig.periodKey === "glda_april_july");
  assert.strictEqual(display.eligible, true, `Colorado transition ${index} must settle to one display value`);
});

assert.strictEqual(api.basinOptions.length, 15);
assert.deepStrictEqual(api.basinOptions.map((row) => row.label), api.basinOptions.map((row) => row.label).slice().sort());
assert.strictEqual(api.basinOptions.some((row) => row.id === "BDBC1_FNF"), false);
const viewChoices = api.choiceHtml("view", "View", [
  ["ca_major", "California forecast basins"], ["ca_indices", "California forecast indices"]
], "ca_major", "pt-major-choice-view");
assert.match(viewChoices, /<fieldset/);
assert.match(viewChoices, /type="radio"/);
assert.match(viewChoices, /role="radiogroup"/);
assert.match(viewChoices, /data-state-key="view"/);
assert.match(viewChoices, /value="ca_major"[^>]* checked/);
assert.match(api.highlightedText("Sacramento Valley", "mento"), /Sacra<mark>mento<\/mark> Valley/);

const centralComposition = api.compositionHtml("MLIC0_FNF");
assert.match(centralComposition, /Basin \/ index composition/);
assert.match(centralComposition, /BRIM geometry assembly/);
assert.match(centralComposition, /MLIC0 = SACC0 \+ VNSC0/);
assert.match(centralComposition, /MWSP_MLIC0_FNF_V1/);
assert.match(centralComposition, /CNRFC source page/);
assert.match(centralComposition, /Forecast values and percentages always come directly from accepted producer records/);
assert.strictEqual(api.compositionHtml("SHDC1_FNF"), "", "unverified official forecast formulas must be omitted");
assert.doesNotMatch(browserSource, /MLIC0 = SACC0|SHDC1 = PITC1/, "composition formulas must not be browser literals");
assert.strictEqual(mapping.some((row) => row.geometry_id === "BDBC1_FNF"), false);

assert.match(browserSource, /bindPopup\(function\(\)/);
assert.doesNotMatch(browserSource, /cdec_reservoir_latest|reservoirStorageUrl|fetch\([^)]*reservoir/i);
assert.match(browserSource, /HUC2_14_UPPER_COLORADO_CONTEXT/);
assert.match(browserSource, /context_only/);
assert.match(browserSource, /Select Lake Mead Local to display the current monthly forecast/);
assert.match(browserSource, /Show ended-period values for reference/);
assert.match(browserSource, /source map eligibility remains expired/);
assert.match(browserSource, /fillOpacity: displayState\.reference \? 0\.34/);
assert.match(browserSource, /indexGeometryId/);
assert.match(browserSource, /getState: function/);
assert.match(browserSource, /pt-major-basin-close/);
assert.match(browserSource, /pt-major-basin-dock/);
assert.match(browserSource, /ptMajorChoiceHtml\('measure', shortRange \? 'Forecast type' : 'Measure'/);
assert.match(browserSource, /ptMajorChoiceHtml\('horizon', 'Accumulation period'/);
assert.doesNotMatch(browserSource, /<select class="pt-major-basin-(?:view|period|measure|horizon|index)/);
assert.match(browserSource, /Find basin or river…/);
assert.match(browserSource, /Auto-zoom to selected basin/);
assert.match(browserSource, /pt-major-basin-all/);
assert.match(browserSource, /pt-major-basin-none/);
assert.match(browserSource, /Data status &amp; sources/);
assert.match(browserSource, /id="pt-major-basin-source-status"/);
assert.match(browserSource, /<button type="button" class="pt-major-health-badge/);
assert.match(browserSource, /aria-expanded=/);
assert.match(browserSource, /aria-controls="pt-major-status-/);
assert.match(browserSource, /feed ' \+ tooltipState \+ ' — activate for details and source links/);
assert.match(browserSource, /\.pt-major-health-badge:focus-visible/);
assert.match(browserSource, /tabindex="-1" aria-label=/);
assert.match(browserSource, /section\.focus\(\{preventScroll: true\}\)/);
assert.match(browserSource, /BRIM Live canonical ' \+ slot\.id \+ ' feed/);
assert.match(browserSource, /Official NOAA\/NWS ' \+ slot\.id \+ ' source/);
assert.match(browserSource, /Official ' \+ slot\.id \+ ' forecast for selected product/);
assert.match(browserSource, /ptMajorHtmlLink\(url, 'Official ' \+ sourceId \+ ' forecast'\)/);
assert.match(browserSource, /Payload generated/);
assert.match(browserSource, /Family health:/);
assert.match(browserSource, /Latest refresh failed; BRIM is continuing with the last accepted snapshot/);
const compactHealthSource = browserSource.slice(browserSource.indexOf("_compactHealthHtml: function"), browserSource.indexOf("_contextualOfficialUrl: function"));
assert.doesNotMatch(compactHealthSource, /roster_version/, "compact badges must not expose roster IDs");
assert.match(browserSource, /Forecast data:<\/b>.*NOAA\/NWS CNRFC/s);
assert.match(browserSource, /Basin geometry:<\/b>.*USGS WBD context/s);
assert.match(browserSource, /pt-major-legend-scale pt-major-legend-volume/);
assert.match(browserSource, /pt-major-legend-scale pt-major-legend-percent/);
assert.match(browserSource, /role="list"/);
assert.match(browserSource, /fitBounds\(layer\.getBounds\(\), \{padding:\[32,32\], maxZoom:8\}\)/);
assert.match(browserSource, /config\.viewKey !== 'ca_major' \|\| self\._state\.selectedBasins\.indexOf\(row\.geometry_id\) >= 0/);
const wireSource = browserSource.slice(browserSource.indexOf("_wireCard: function"), browserSource.indexOf("_config: function"));
assert.strictEqual((wireSource.match(/addEventListener/g) || []).length, 5, "the persistent card must own one delegated listener set, dock-state listener, and status-disclosure listener");
assert.strictEqual((browserSource.match(/this\._geoJson = L\.geoJSON/g) || []).length, 1, "geometry must be populated once");
const renderSource = browserSource.slice(browserSource.indexOf("_render: function"), browserSource.indexOf("_syncSharedCardState: function"));
assert.doesNotMatch(renderSource, /fetch\(|\.refresh\(/, "selection rendering must not refetch sources");
assert.doesNotMatch(browserSource, /requestAnimationFrame\([^)]*requestAnimationFrame|setInterval\(/, "the controller must not create an idle animation loop");
assert.match(browserSource, /actionsHtml/);
assert.match(browserSource, /makeDetachable/);
assert.match(browserSource, /visibility = 'hidden'/);
assert.match(browserSource, /scheduleLayout\(this\._card\)/);
assert.match(browserSource, /setOpsLayerLoading\(PT_MAJOR_BASIN_LAYER_NAME, false\);\s*this\._recordCombinedStatus\(\);/);
assert.match(browserSource, /\.pt-major-basin-popup\{box-sizing:border-box;width:500px;max-width:calc\(100vw - 82px\);max-height:66vh;overflow-x:hidden;overflow-y:auto/);
assert.match(browserSource, /\.pt-major-basin-popup details/);

async function sourceSlotTests() {
  let calls = 0, release;
  const deferred = new Promise((resolve) => { release = resolve; });
  const slot = api.sourceSlot({id: "CNRFC", url: "https://example.test/cnrfc", validate: api.validateCnrfc, fetchImpl: () => { calls += 1; return deferred; }});
  const first = slot.refresh(), duplicate = slot.refresh();
  assert.strictEqual(first, duplicate);
  assert.strictEqual(calls, 1);
  release({ok: true, json: async () => cnrfcFixture()});
  await first;
  assert.strictEqual(slot.accepted.records.length, 51);

  let shouldFail = false;
  const retained = api.sourceSlot({id: "CBRFC", url: "https://example.test/cbrfc", validate: api.validateCbrfc, fetchImpl: async () => {
    if (shouldFail) throw new Error("offline");
    return {ok: true, json: async () => cbrfcFixture()};
  }});
  await retained.refresh();
  const accepted = retained.accepted;
  shouldFail = true;
  await assert.rejects(retained.refresh(), /offline/);
  assert.strictEqual(retained.accepted, accepted);
  assert(retained.lastError);

  const independent = api.sourceSlot({id: "CNRFC", url: "https://example.test/cnrfc", validate: api.validateCnrfc, fetchImpl: async () => ({ok: true, json: async () => cnrfcFixture()})});
  await independent.refresh();
  assert.strictEqual(independent.accepted.records.length, 51, "one source may advance independently of another source failure");
}

async function optionalLiveSmoke() {
  if (process.env.BRIM_MAJOR_BASIN_LIVE_SMOKE !== "1") return;
  const urls = [
    "https://dbo99.github.io/brim-live-data-feeds/data/major_water_supply_basin_forecasts.json",
    "https://dbo99.github.io/brim-live-data-feeds/data/cbrfc_major_water_supply_forecasts.json"
  ];
  const payloads = await Promise.all(urls.map(async (url) => {
    const response = await fetch(url, {cache: "no-store"});
    assert(response.ok, `live smoke HTTP ${response.status}: ${url}`);
    return response.json();
  }));
  api.validateCnrfc(payloads[0]);
  api.validateCbrfc(payloads[1]);
  console.log("Canonical CNRFC/CBRFC browser-validator smoke test passed.");
}

sourceSlotTests().then(optionalLiveSmoke).then(() => {
  const sharedSource = fs.readFileSync(path.join(root, "03_functions", "js", "brim_legend_closeout_helpers.js"), "utf8");
  assert.match(sharedSource, /card: '\.pt-major-basin-card'[^\n]*close: '\.pt-major-basin-close'[^\n]*handle: '\.pt-major-basin-title'[^\n]*dock: '\.pt-major-basin-dock'/);
  assert.match(sharedSource, /state\.floating \? '&#x21A9;' : '&#x2197;'/);
  console.log("Major water-supply basin Phase C browser/controller tests passed.");
}).catch((error) => { console.error(error); process.exitCode = 1; });
