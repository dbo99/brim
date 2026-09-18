'use strict';

// Offline lifecycle regression. --baseline reads pinned blobs into memory;
// it never checks out/stages files. No project initialization or provider calls.
// Extraction executes exact top-level production functions in separate Tools
// and Ops scopes, plus the actual Ops bridge, registration, checkbox, refresh,
// Clear Ops ribbon and Clear All callers. Only rendering/DOM/Esri dependencies
// are controlled. Missing/ambiguous extraction or unexpected networking fails.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const crypto = require('node:crypto');
const {execFileSync} = require('node:child_process');
const root = path.resolve(__dirname, '..');
const base = '6832e14bd0144eb2e488890ccc8edfb8783436d8';
const baseline = process.argv.includes('--baseline');
assert(process.argv.slice(2).every(x => ['--baseline', '--syntax-only'].includes(x)));
const gitArgs = ['--no-pager', '--no-optional-locks', '--no-replace-objects',
  '-c', 'core.fsmonitor=false', '-c', 'core.untrackedCache=false',
  '-c', 'gc.auto=0', '-c', 'maintenance.auto=false'];
const gitEnv = {...process.env, GIT_OPTIONAL_LOCKS: '0', GIT_NO_LAZY_FETCH: '1',
  GIT_ALLOW_PROTOCOL: '', GIT_TERMINAL_PROMPT: '0', LC_ALL: 'C'};
const read = p => baseline
  ? execFileSync('git', [...gitArgs, 'show', base + ':' + p],
      {cwd: root, env: gitEnv, encoding: 'utf8', maxBuffer: 2000000})
  : fs.readFileSync(path.join(root, p), 'utf8');
const sha = s => crypto.createHash('sha256').update(s).digest('hex');
const toolsPath = '03_functions/js/leaflet_tools_adddata_panel.js';
const defsPath = '03_functions/leaflet_ops_live_layer_definition_helpers.r';
const tools = read(toolsPath), defs = read(defsPath);
const panel = read('03_functions/leaflet_ops_live_panel_helpers.r');
function unique(s, token) {
  const i = s.indexOf(token);
  assert(i >= 0 && s.indexOf(token, i + token.length) < 0, 'Unique region: ' + token);
  return i;
}
function region(s, start, end) {
  const a = unique(s, start), b = unique(s, end);
  assert(b > a, 'Ordered region: ' + start);
  return s.slice(a, b);
}
function raw(s) {
  return region(s, 'r"---(', ')---"').slice('r"---('.length);
}
function fn(s, name) {
  const start = '  function ' + name + '(';
  const a = unique(s, start);
  // Production top-level declarations close at exactly two-space indentation.
  const end = /^  }$/m.exec(s.slice(a));
  assert(end, 'Complete function: ' + name);
  const result = s.slice(a, a + end.index + end[0].length);
  new vm.Script('(' + result.trim() + ')');
  return result;
}
function handler(s, start, end) {
  const block = region(s, start, end);
  const close = block.lastIndexOf('});');
  assert(close >= 0 && !block.slice(close + 3).trim(), 'Complete event handler');
  return '(' + block.slice(block.indexOf('function('), close + 1) + ')';
}
const dj = raw(defs), pj = raw(panel);
new vm.Script('(' + tools + ')');
new vm.Script('(function(){' + dj + '\n})');
new vm.Script('(function(){' + pj + '\n})');
console.log(JSON.stringify({mode: baseline ? 'baseline' : 'working',
  harness: sha(fs.readFileSync(__filename)), tools: sha(tools), definitions: sha(defs),
  panel: sha(panel)}));
if (process.argv.includes('--syntax-only')) {
  console.log('PASS full Tools hook and R raw JavaScript boundaries');
  process.exit(0);
}
const functions = [
  'ptCpcForecastResponse', 'ptCpcForecastDescriptor', 'ptCleanText', 'ptTruth', 'ptNormalizeUrl', 'ptLooksLikeArcgisHubPage',
  'ptDetectServiceType', 'ptExplainUrlProblem', 'ptCatalogField',
  'ptCatalogPrimaryPanel', 'ptCatalogRecordKey', 'ptOutFieldsArray',
  'ptIsOpsPromotedRecord', 'ptExternalPanelLayerRecords', 'ptExternalPanelLayerCount',
  'ptStatusKeepsCatalogSpinner', 'ptNotifyOpsPromotedStatus', 'ptSetExternalStatus',
  'ptAddCustomRecord', 'ptOptionsFromCustomRecord', 'ptRunArcgisCurrentViewFeatureQuery',
  'ptParseArcgisLayerUrl', 'ptAddArcgisMapLayerCurrentView', 'ptIsRefreshableCurrentViewRecord', 'ptIsRefreshableImageServerRecord',
  'ptIsRefreshableVisualMapServerRecord', 'ptIsRefreshableVisualRasterRecord',
  'ptIsRefreshableLiveGeoJsonRecord', 'ptIsRefreshableUicSnapshotRecord',
  'ptIsRefreshableExternalRecord', 'ptRefreshCustomLayer', 'ptAddArcgisFeatureLayerCurrentView',
  'ptRemoveCustomLayer', 'ptClearCustomLayers', 'ptClearAllPt2SessionLayers',
  'ptFindCatalogRecordIndexByDisplayName', 'ptRemoveOpsPromotedLayerByKey',
  'ptOptionsFromCatalogRecordForOps', 'ptOpsAddCatalogLayer'
];
const toolsCode = region(tools, '  var ptCustomLayers = [];',
  '  function ptIsOpsPromotedRecord(') +
  functions.map(n => fn(tools, n)).join('\n') +
  region(tools, '  window.ptOpsExternalCatalogBridge = {',
    '\n  ptPopulateCatalogControls();\n  ptRenderQuickCatalog();\n  ptRenderCustomLayerList();\n  ptUpdatePopupSupportUI();');
const opsCode = region(dj, '  var ptOpsPromotedSlowTimers = {};',
  '  // AirNow and smoke layers moved to External Layers.');
// Exact real registrations; only the two catalog fixtures below are exercised.
const nationalKey = 'ops_live_agency_streamflow_gages';
const viewshedKey = 'ops_alertcalifornia_camera_viewsheds';
const promotedKeys = [
  'ops_alertcalifornia_cameras', viewshedKey, nationalKey,
  'ops_nws_wfo_boundaries', 'ops_us_drought_monitor_current',
  'ops_cpc_6_10_day_temperature_outlook', 'ops_cpc_6_10_day_precipitation_outlook',
  'ops_cpc_8_14_day_temperature_outlook', 'ops_cpc_8_14_day_precipitation_outlook'
];
assert.equal([...dj.matchAll(/layer: new CatalogPromotedExternalLayer\(\{/g)].length,
  promotedKeys.length, 'Complete shared promoted registration set');
function registrationFor(key) {
  const at = unique(dj, "      opsKey: '" + key + "',");
  const start = dj.lastIndexOf('  addOpsLayer({', at);
  const end = dj.indexOf('\n  });', at);
  assert(start >= 0 && end > at, 'Complete actual registration: ' + key);
  return dj.slice(start, end + '\n  });'.length);
}
const registration = promotedKeys.map(registrationFor).join('\n');
const refreshHandler = handler(pj, "    body.addEventListener('click', function(e) {",
  "    body.addEventListener('change', function(e) {");
const clearHandler = handler(pj, "      clearRibbonBtn.addEventListener('click', function(e) {",
  '    }\n\n    opsDiv = body;');
const toggleHandler = handler(pj, "      chk.addEventListener('change', function() {",
  '    });\n\n    updateOpsHeaderCount();');
let unexpectedCalls = 0;
const deny = () => {
  unexpectedCalls++;
  throw new Error('Unexpected network or uncontrolled dependency');
};
const noop = () => {};
function fixture() {
  const pending = [], ensures = [], errors = [], external = [], events = [], timers = new Map();
  const listeners = new Map(), geometry = new Set(), allLayers = new Set();
  let timerId = 0, delayEnsure = false, queryThrow = false, immediate = null, ops;
  const operations = [], identities = new Map();
  let objectId = 0, actionOwner = 'fixture';
  const inOwner = (owner, run) => {
    const previous = actionOwner; actionOwner = owner;
    try { return run(); } finally { actionOwner = previous; }
  };
  function identity(object, owner = actionOwner, kind = 'geometry') {
    if (!identities.has(object)) identities.set(object, {id: ++objectId, owner, kind});
    return identities.get(object);
  }
  const log = (op, object, extra = {}) =>
    operations.push({op, ...identity(object), ...extra});
  const sentinel = owner => {
    const layer = {addTo(m) { m.addLayer(this); return this; }};
    identity(layer, owner, 'sentinel'); return layer;
  };
  const basemap = sentinel('basemap'), local = sentinel('local');
  let localChecked = true;
  const window = {
    addEventListener(type, cb) {
      if (!listeners.has(type)) listeners.set(type, []);
      listeners.get(type).push(cb);
    },
    dispatchEvent(evt) {
      events.push(evt.detail);
      for (const cb of listeners.get(evt.type) || []) cb(evt);
    },
    setTimeout(cb, delay) {
      const owner = actionOwner, token = {};
      identity(token, owner, 'timer');
      if (delay === 200) log('retry-schedule', token);
      timers.set(++timerId, () => inOwner(owner, () => {
        if (delay === 200) log('retry-fire', token);
        return cb();
      }));
      return timerId;
    },
    clearTimeout(id) { timers.delete(id); }
  };
  function CustomEvent(type, init) { this.type = type; this.detail = init.detail; }
  const event = {preventDefault: noop, stopPropagation: noop};
  const document = {
    getElementById(id) {
      return id === 'pt-ops-clear-ribbon-btn' ? {click: () => ops.clear(event)} : null;
    },
    // Clear All's Local button boundary is mocked; no native Local controller.
    querySelector(selector) {
      return selector === '.pt-main-layer-clear-btn' ? {click() {
        if (localChecked) { localChecked = false; map.removeLayer(local); }
      }} : null;
    },
    querySelectorAll() { return []; },
    createElement: deny
  };
  const map = {
    addLayer(layer) {
      const effective = !allLayers.has(layer);
      log('add', layer, {effective});
      if (!effective) return this;
      allLayers.add(layer);
      if (layer.onAdd) layer.onAdd(this);
      else if (identity(layer).kind !== 'sentinel') geometry.add(layer);
      return this;
    },
    removeLayer(layer) {
      const effective = allLayers.delete(layer);
      log('remove', layer, {effective});
      if (!effective) return this;
      geometry.delete(layer);
      if (layer.onRemove) layer.onRemove(this);
      return this;
    },
    hasLayer: layer => allLayers.has(layer),
    getBounds: () => ({fixtureBounds: true})
  };
  const L = {
    Layer: {extend(methods) {
      function Layer(options) {
        identity(this, options.opsKey, 'wrapper'); this.initialize(options);
      }
      Object.assign(Layer.prototype, methods, {addTo(m) { m.addLayer(this); return this; }});
      return Layer;
    }},
    esri: {query({url}) {
      const owner = url.includes('/viewsheds/') ? viewshedKey :
        url === 'https://example.invalid/FeatureServer/0' ? nationalKey : 'external';
      const token = {}; identity(token, owner, 'query'); log('query-create', token);
      if (queryThrow) throw new Error('Controlled query construction failure');
      const request = {url, owner, where(v) { this.whereText = v; return this; },
        intersects(v) { this.bounds = v; return this; },
        fields(v) { this.outFields = v; return this; },
        run(cb) {
          log('query-run', token);
          this.complete = (error, count = 1) => inOwner(owner, () => cb(error,
            {type: 'FeatureCollection', features: Array.from({length: count},
              (_, id) => ({type: 'Feature', properties: {id}, geometry:
                owner === viewshedKey ? {type: 'Polygon', coordinates:
                  [[[-120, 38], [-120, 39], [-119, 38], [-120, 38]]]} :
                  {type: 'Point', coordinates: [-120, 38]}}))}, {}));
          this.completeRaw = (error, fc, response = {}) => cb(error, fc, response);
          pending.push(this);
          if (immediate) this.complete(immediate.error, immediate.count);
        }};
      return request;
    }}
  };
  const render = (fc, color, opacity, options) => {
    const layer = {count: fc.features.length, addTo(m) { m.addLayer(this); return this; }};
    identity(layer, options.opsPromotedKey || 'external');
    log('construct', layer); return layer;
  };
  // HYPOTHETICAL catalog/query payload: identity matches the real registration;
  // this harness tests lifecycle, not provider attributes or scientific styling.
  const catalog = {display_name: 'Live Stream Gages / Flow',
    service_url: 'https://example.invalid/FeatureServer/0', service_type: 'feature',
    default_load_mode: 'current_view', where_clause: 'flow_cfs IS NOT NULL',
    primary_panel: 'ops_live', external_layer_id: 'EXT056',
    supports_popups: 'TRUE', default_clickable: 'TRUE'};
  // Real viewshed identity/mode; hypothetical URL and polygon payload, never captured data.
  const viewshedCatalog = {display_name: 'ALERTCalifornia Camera Viewsheds',
    service_url: 'https://example.invalid/viewsheds/FeatureServer/1', service_type: 'feature',
    default_load_mode: 'current_view', where_clause: '', primary_panel: 'ops_live',
    external_layer_id: 'EXT012', default_style_method: 'alert_camera_viewshed',
    min_zoom_current_view: '6.0', supports_popups: 'TRUE', default_clickable: 'TRUE'};
  const t = {window, document, map, L, CustomEvent, fetch: deny,
    XMLHttpRequest: deny, WebSocket: deny, EventSource: deny,
    console: {error: e => errors.push(e), warn: deny, info: noop},
    PT2_CATALOG: [catalog, viewshedCatalog], ptCatalogLoadingIdx: null,
    ptLastManualCurrentViewLayerId: null,
    ptSetStatus: (msg, error) => external.push({msg, error}),
    ptEnsureEsriLeaflet: cb => {
      const token = {}, owner = actionOwner;
      identity(token, owner, 'dependency'); log('dependency', token);
      const complete = ok => inOwner(owner, () => cb(ok));
      return delayEnsure ? ensures.push(complete) : complete(true);
    },
    ptExternalFeatureCollectionLayer: render,
    ptSortFeatureCollectionForDrawing: (fc, options) => {
      identity(fc, options.opsPromotedKey || 'external', 'collection');
      log('prepare', fc); return fc;
    },
    ptDeriveLegendUrl: () => '', ptCatalogLoadWasCancelled: () => false,
    ptZoomCheck: () => true
  };
  for (const n of ['ptIsCpcOutlookStyle', 'ptIsDroughtMonitorStyle',
    'ptIsSubsidenceObservationStyle', 'ptIsUicAquiferExemptionStyle']) t[n] = () => false;
  for (const n of ['ptRenderCustomLayerList', 'ptRenderQuickCatalog',
    'ptUpdateManualRefreshControl', 'ptUpdateWcrCompletedDepthMapLegend',
    'ptUpdateMlrsMineralCasesMapLegend', 'ptUpdateSgmaPrioritizationMapLegend',
    'ptUpdateSubsidenceObservationMapLegend', 'ptUpdateOpsPromotedStreamflowMetric',
    'ptRestoreExternalBaseAttribution', 'ptHideVisualIdentifyTooltip',
    'ptSetManualCurrentViewRecord', 'ptClearBlmSma', 'ptResetManualExternalAddState',
    'ptClearCatalogLoading', 'ptClearCatalogRowNotes', 'ptClearActionNotes',
    'ptSetTeachingLabelPlacementActive', 'ptSetTeachingMarkupMode',
    'ptClearMeasurements']) t[n] = noop;
  vm.createContext(t); vm.runInContext(toolsCode, t);
  ops = {window, document, map, L, console: t.console, Event: function(type) { this.type=type; },
    activeLayers: {}, activeLegendDefs: {}, checkboxByName: {}, opsDefByName: {}, ptGibsLayers: [],
    opsLayers: [], statusRows: {}, loading: {}, body: {contains: () => true},
    ptOpsCatalogSourceDisabled: () => false,
    recordStatus(name, msg, cssClass) { ops.statusRows[name] = {msg, cssClass}; },
    setOpsLayerLoading(name, value) { ops.loading[name] = value; },
    isOpsLayerLoading: name => !!ops.loading[name],
    clearAllOpsLayerLoading() { ops.loading = {}; },
    redrawLegend: noop, redrawStatus: noop, updateOpsHeaderCount: noop,
    ptClearOpsPaneDomArtifacts: noop, ptClearBtnPressOff: noop};
  ops.addOpsLayer = def => { ops.opsLayers.push(def); ops.opsDefByName[def.name] = def; };
  vm.createContext(ops);
  const forecastArc = raw(read('03_functions/leaflet_ops_live_arcgis_export_helpers.r'));
  if (forecastArc.includes('  var ptOpsForecastOwners =')) vm.runInContext(region(forecastArc, '  var ptOpsForecastOwners =', '  function ptIsWpcQpfOwner('), ops);
  vm.runInContext(opsCode + '\n' + registration + '\n' + fn(pj, 'ptClearOpsLayers') + '\n' + fn(pj, 'ptOpsDeactivateLayerByName'), ops);
  assert.equal(ops.opsLayers.length, promotedKeys.length, 'Exact real registration set');
  const owners = new Map();
  ops.clear = vm.runInContext(clearHandler, ops);
  ops.refresh = vm.runInContext(refreshHandler, ops);
  for (const def of ops.opsLayers) {
    const key = def.layer.options.opsKey, chk = {checked: false};
    assert(promotedKeys.includes(key) && !owners.has(key), 'Unique real Ops key');
    ops.checkboxByName[def.name] = chk;
    // Each actual panel handler has its own def/chk in the same Ops VM.
    const toggle = vm.runInContext('(function(def, chk) { return ' +
      toggleHandler + '; })', ops)(def, chk);
    chk.dispatchEvent = () => inOwner(key, () => toggle());
    owners.set(key, {def, chk, toggle});
  }
  const {def, chk} = owners.get(nationalKey), name = def.name, key = nationalKey;
  ops.def = def; ops.chk = chk; ops.toggle = owners.get(key).toggle;
  function on(owner = key) {
    const o = owners.get(owner); o.chk.checked = true;
    inOwner(owner, () => o.toggle());
  }
  function off(owner = key) {
    const o = owners.get(owner); o.chk.checked = false;
    inOwner(owner, () => o.toggle());
  }
  function refresh(owner = key) {
    inOwner(owner, () => ops.refresh({...event, target: {closest(selector) {
      return selector === '[data-pt-ops-action="refresh"]'
        ? {getAttribute: () => owners.get(owner).def.name} : null;
    }}}));
  }
  const record = (owner = key) => t.ptCustomLayers.find(r => r.opsPromotedKey === owner);
  const ownerState = owner => {
    const o = owners.get(owner), rec = record(owner);
    return JSON.stringify({status: ops.statusRows[o.def.name], loading: ops.loading[o.def.name],
      active: ops.activeLayers[o.def.name] === o.def.layer,
      layer: rec && identity(rec.layer).id, count: rec && rec.featureCount,
      refreshing: rec && rec.refreshing});
  };
  basemap.addTo(map); local.addTo(map);
  function externalSentinel() {
    const layer = sentinel('external-sentinel');
    const rec = t.ptAddCustomRecord('External sentinel', layer, '#000',
      'ArcGIS MapServer — current view', 'https://example.invalid/MapServer/0',
      {loadMode: 'current_view', whereClause: '1=1'});
    return {layer, rec};
  }
  const state = () => JSON.stringify({rows: ops.statusRows, loading: ops.loading,
    count: geometry.size, records: t.ptCustomLayers.map(r =>
      ({id: r.id, count: r.featureCount, refreshing: r.refreshing}))});
  function loaded() {
    on(); assert.equal(pending.length, 1, 'Initial query reached controlled Esri service');
    pending[0].complete(null, 2);
    assert(record(), 'Initial real loader registered a record');
    return record();
  }
  const mark = () => operations.length;
  const since = (at, object) => operations.slice(at).filter(op =>
    object === undefined || op.id === identity(object).id);
  return {t, ops, name, key, pending, ensures, errors, external, events, timers, geometry,
    map, owners, ownerState, operations, mark, since, basemap, local, externalSentinel,
    on, off, refresh, record, state, loaded, clear: () => ops.clear(event),
    clearAll: () => t.ptClearAllPt2SessionLayers(),
    delayEnsure: value => { delayEnsure = value; },
    queryThrow: value => { queryThrow = value; },
    immediate: value => { immediate = value; }};
}
let passed = 0, failed = 0;
function test(name, run) {
  if (require.main !== module) return;
  try {
    const before = unexpectedCalls, f = fixture(); run(f);
    assert.equal(unexpectedCalls, before, 'Unexpected networking must fail even if caught');
    for (const error of f.errors) {
      assert.notEqual(error.name, 'ReferenceError', 'Missing harness dependency');
      assert.notEqual(error.name, 'TypeError', 'Invalid harness dependency');
    }
    passed++; console.log('PASS ' + name);
  }
  catch (e) { failed++; console.log('FAIL ' + name + ': ' + e.message); }
}
test('initial promoted success reaches terminal Ops state', f => {
  f.loaded(); assert.equal(f.ops.loading[f.name], false);
  assert.match(f.ops.statusRows[f.name].msg, /layer added/);
  assert.equal(f.external.length, 0); assert.equal(f.geometry.size, 1);
});
test('promoted refresh preserves ownership and reaches terminal success', f => {
  const rec = f.loaded(); f.refresh(); f.pending[1].complete(null, 3);
  assert.equal(f.record(), rec); assert.equal(rec.featureCount, 3);
  assert.equal(f.ops.loading[f.name], false, 'Refresh must finish the Ops spinner');
  assert.match(f.ops.statusRows[f.name].msg, /Refreshed current-view/);
  assert.equal(f.external.length, 0, 'Promoted refresh must not report External status');
});
test('current failure retains data and ends Ops loading', f => {
  const rec = f.loaded(), old = rec.layer; f.refresh();
  f.pending[1].complete(new Error('Controlled service failure'));
  assert.equal(rec.layer, old); assert.equal(rec.refreshing, false);
  assert.equal(f.ops.loading[f.name], false, 'Failed refresh must finish Ops loading');
  assert.match(f.ops.statusRows[f.name].msg, /previous snapshot was left unchanged/);
});
for (const oldError of [false, true]) {
  for (const newerPending of [false, true]) {
    test('old ' + (oldError ? 'failure' : 'success') + ' ignored with newer ' +
      (newerPending ? 'pending' : 'complete'), f => {
      f.loaded(); f.refresh(); f.refresh();
      if (!newerPending) f.pending[2].complete(null, 4);
      const before = f.state(), externalCount = f.external.length;
      f.pending[1].complete(oldError ? new Error('Old failure') : null, 9);
      assert.equal(f.state(), before, 'Old completion must not mutate current state');
      assert.equal(f.external.length, externalCount);
      if (newerPending) f.pending[2].complete(null, 4);
      assert.equal(f.record().featureCount, 4);
      assert.equal(f.ops.loading[f.name], false);
    });
  }
}
for (const action of ['off', 'clear', 'clearAll']) {
  for (const error of [false, true]) {
    test(action + ' rejects late refresh ' + (error ? 'failure' : 'success'), f => {
      f.loaded(); f.refresh(); f[action](); const before = f.state();
      const externalCount = f.external.length;
      f.pending[1].complete(error ? new Error('Late failure') : null, 8);
      assert.equal(f.state(), before, 'Removed owner must reject completion');
      assert.equal(f.external.length, externalCount, 'Late completion must not report External status');
      assert.equal(f.geometry.size, 0);
    });
  }
}
for (const error of [false, true]) {
  test('off/re-enable rejects old refresh ' + (error ? 'failure' : 'success'), f => {
    const old = f.loaded(); f.refresh(); f.off(); f.on(); const before = f.state();
    const externalCount = f.external.length;
    f.pending[1].complete(error ? new Error('Old activation') : null, 8);
    assert.equal(f.state(), before, 'Old activation must not affect re-enabled owner');
    assert.equal(f.external.length, externalCount, 'Old activation must not report External status');
    f.pending[2].complete(null, 3); assert.notEqual(f.record(), old);
    assert.equal(f.geometry.size, 1); assert.equal(f.ops.loading[f.name], false);
  });
  test('old initial ' + (error ? 'failure' : 'empty result') + ' cannot finish re-enable', f => {
    f.on(); f.off(); f.on(); const before = f.state();
    f.pending[0].complete(error ? new Error('Old initial failure') : null, 0);
    assert.equal(f.state(), before, 'Old initial terminal status must be ignored');
    f.pending[1].complete(null, 2); assert.equal(f.ops.loading[f.name], false);
  });
}
test('repeated removal and duplicate terminal completion are harmless', f => {
  f.loaded(); f.refresh(); f.pending[1].complete(null, 4); const before = f.state();
  f.pending[1].complete(null, 7); assert.equal(f.state(), before);
  f.off(); f.off(); assert.equal(f.geometry.size, 0);
  assert.equal(f.ops.loading[f.name], false);
});
test('name-only status cannot acquire ownership', f => {
  f.loaded(); f.refresh(); const before = f.state();
  f.t.window.dispatchEvent({type: 'ptOpsCatalogLayerStatus',
    detail: {opsDisplayName: f.name, terminal: true, msg: 'Unowned completion'}});
  assert.equal(f.state(), before);
});
test('delayed dependency completion cannot restart removed refresh', f => {
  f.loaded(); f.delayEnsure(true); f.refresh(); f.off(); const before = f.state();
  f.ensures[0](true); assert.equal(f.pending.length, 1);
  assert.equal(f.state(), before);
});
for (const ok of [false, true]) {
  test('older dependency callback ' + ok + ' cannot finish newer refresh', f => {
    f.loaded(); f.delayEnsure(true); f.refresh(); f.refresh(); const before = f.state();
    f.ensures[0](ok); assert.equal(f.pending.length, 1);
    assert.equal(f.state(), before); f.ensures[1](true);
    f.pending[1].complete(null, 3); assert.equal(f.ops.loading[f.name], false);
  });
}
test('current dependency failure terminates and retains data', f => {
  const old = f.loaded().layer; f.delayEnsure(true); f.refresh(); f.ensures[0](false);
  assert.equal(f.record().layer, old); assert.equal(f.record().refreshing, false);
  assert.equal(f.ops.loading[f.name], false);
  assert.match(f.ops.statusRows[f.name].msg, /Could not load Esri Leaflet/);
});
test('synchronous query construction failure remains terminal', f => {
  const old = f.loaded().layer; f.queryThrow(true); f.refresh();
  assert.equal(f.record().layer, old); assert.equal(f.ops.loading[f.name], false);
  assert.match(f.ops.statusRows[f.name].msg, /Could not refresh this current-view layer/);
});
test('prior request status event cannot overwrite pending request', f => {
  f.loaded(); f.refresh(); f.pending[1].complete(null, 3);
  const old = f.events.at(-1); f.refresh(); const before = f.state();
  f.t.window.dispatchEvent({type: 'ptOpsCatalogLayerStatus', detail: old});
  assert.equal(f.state(), before); f.pending[2].complete(null, 4);
  assert.equal(f.ops.loading[f.name], false);
});
test('wrong key and prior activation status events cannot acquire ownership', f => {
  f.loaded(); const old = f.events.at(-1); f.off(); f.on(); const before = f.state();
  for (const detail of [old, {...old, opsKey: 'wrong-key'}]) {
    f.t.window.dispatchEvent({type: 'ptOpsCatalogLayerStatus', detail});
    assert.equal(f.state(), before);
  }
  f.pending[1].complete(null, 2); assert.equal(f.ops.loading[f.name], false);
});
test('old bridge retry cannot acquire re-enabled activation', f => {
  const bridge = f.t.window.ptOpsExternalCatalogBridge;
  f.t.window.ptOpsExternalCatalogBridge = null;
  f.on(); const oldRetry = [...f.timers.values()].at(-1);
  f.off(); f.on(); const newRetry = [...f.timers.values()].at(-1);
  f.t.window.ptOpsExternalCatalogBridge = bridge;
  oldRetry(); assert.equal(f.pending.length, 0, 'Old retry must be cancelled by activation');
  newRetry(); assert.equal(f.pending.length, 1);
  f.pending[0].complete(null, 2); assert.equal(f.ops.loading[f.name], false);
});
for (const outcome of ['success', 'empty', 'failure']) {
  test('synchronous initial ' + outcome + ' remains terminal', f => {
    f.immediate({error: outcome === 'failure' ? new Error('Immediate initial failure') : null,
      count: outcome === 'success' ? 2 : 0});
    f.on(); assert.equal(f.ops.loading[f.name], false);
    assert.match(f.ops.statusRows[f.name].msg,
      outcome === 'success' ? /layer added/ : outcome === 'empty' ? /No features/ : /query failed/);
  });
}
for (const error of [false, true]) {
  test('ordinary External refresh ' + (error ? 'failure' : 'success'), f => {
    const old = {addTo() {}};
    const rec = f.t.ptAddCustomRecord('External', old, '#000',
      'ArcGIS MapServer — current view', 'https://example.invalid/MapServer/0',
      {loadMode: 'current_view', whereClause: '1=1'});
    f.t.ptRefreshCustomLayer(rec.id);
    f.pending[0].complete(error ? new Error('Ordinary failure') : null, 3);
    assert.equal(rec.refreshing, false);
    if (error) assert.equal(rec.layer, old); else assert.equal(rec.featureCount, 3);
    assert.match(f.external.at(-1).msg, error ? /previous snapshot/ : /Refreshed current-view/);
    assert.equal(f.events.length, 0, 'Ordinary External needs no Ops ownership');
  });
}
test('Clear External preserves promoted ownership', f => {
  const rec = f.loaded(); f.t.ptClearCustomLayers();
  assert.equal(f.record(), rec); assert.equal(f.geometry.size, 1);
});
for (const error of [false, true]) {
  test('synchronous refresh ' + (error ? 'failure' : 'success') + ' stays terminal', f => {
    f.loaded(); f.immediate({error: error ? new Error('Immediate failure') : null, count: 3});
    f.refresh(); assert.equal(f.ops.loading[f.name], false);
    assert.match(f.ops.statusRows[f.name].msg,
      error ? /previous snapshot was left unchanged/ : /Refreshed current-view/);
  });
}
// Boundary accounting only: these stubs do not measure native rendering or tile latency.
function countOps(f, at, op, effective) {
  return f.since(at).filter(e => e.op === op &&
    (effective === undefined || e.effective === effective)).length;
}
function untouched(f, at, layer) {
  assert.equal(f.map.hasLayer(layer), true, 'Unrelated object remains mounted');
  assert.equal(f.since(at, layer).length, 0, 'No operation attempted on unrelated object');
}
function noNewWork(f, at, run) {
  const before = f.state();
  run();
  assert.equal(f.state(), before, 'Stale completion preserves all current owner states');
  // Invoking a controlled old timer is the input; launching more work is forbidden.
  const work = f.since(at).filter(e => e.op !== 'retry-fire');
  assert.equal(work.length, 0, 'Stale callback performed: ' +
    work.map(e => e.op + ':' + e.owner).join(', '));
}
function pair(f) {
  f.loaded();
  f.on(viewshedKey); f.pending.at(-1).complete(null, 5);
  const ext = f.externalSentinel();
  return {national: f.record().layer, viewshed: f.record(viewshedKey).layer,
    external: ext.layer, externalRecord: ext.rec};
}
test('accounting: valid refresh constructs and inserts exactly one replacement', f => {
  const p = pair(f), other = f.ownerState(viewshedKey), at = f.mark();
  f.refresh(); f.pending.at(-1).complete(null, 3);
  for (const op of ['dependency', 'query-create', 'query-run', 'prepare', 'construct', 'add', 'remove']) {
    assert.equal(countOps(f, at, op), 1, 'One valid refresh operation: ' + op);
  }
  assert.equal(countOps(f, at, 'add', true), 1);
  assert.equal(countOps(f, at, 'remove', true), 1);
  assert.equal(f.since(at, p.national).filter(e => e.op === 'remove' && e.effective).length, 1);
  assert.equal(f.since(at, f.record().layer).filter(e => e.op === 'add').length, 1);
  assert(f.since(at).every(e => e.owner === nationalKey), 'All work belongs to national owner');
  assert.equal(f.ownerState(viewshedKey), other);
  for (const layer of [p.viewshed, p.external, f.local, f.basemap]) untouched(f, at, layer);
});
test('accounting: failed refresh retains object without preparation or replacement', f => {
  const p = pair(f), at = f.mark();
  f.refresh(); f.pending.at(-1).complete(new Error('Accounted failure'));
  for (const op of ['dependency', 'query-create', 'query-run']) assert.equal(countOps(f, at, op), 1);
  for (const op of ['prepare', 'construct', 'add', 'remove']) assert.equal(countOps(f, at, op), 0);
  assert.equal(f.record().layer, p.national); assert.equal(f.ops.loading[f.name], false);
  for (const layer of [p.national, p.viewshed, p.external, f.local, f.basemap]) untouched(f, at, layer);
});
for (const action of ['superseded-pending', 'superseded-complete', 'off', 'remove', 'clear', 'clearAll', 're-enable']) {
  for (const error of [false, true]) {
    test('accounting: ' + action + ' rejects stale refresh ' + (error ? 'failure' : 'success'), f => {
      pair(f); f.refresh(); const obsolete = f.pending.at(-1);
      if (action.startsWith('superseded')) {
        f.refresh();
        if (action === 'superseded-complete') f.pending.at(-1).complete(null, 4);
      } else if (action === 'remove') {
        f.map.removeLayer(f.owners.get(nationalKey).def.layer);
      } else if (action === 're-enable') {
        f.off(); f.on(); f.pending.at(-1).complete(null, 4);
      } else {
        f[action]();
      }
      const at = f.mark();
      noNewWork(f, at, () => obsolete.complete(error ? new Error('Obsolete completion') : null, 9));
      untouched(f, at, f.basemap);
    });
  }
}
test('accounting: duplicate terminal callback makes no replacement attempt', f => {
  pair(f); f.refresh(); const request = f.pending.at(-1); request.complete(null, 4);
  noNewWork(f, f.mark(), () => request.complete(null, 9));
});
test('accounting: repeated direct removal has one effective disposal per object', f => {
  const p = pair(f), wrapper = f.owners.get(nationalKey).def.layer, at = f.mark();
  f.map.removeLayer(wrapper); f.map.removeLayer(wrapper); wrapper.forceRemove(f.map);
  assert.equal(f.since(at, wrapper).filter(e => e.op === 'remove').length, 2);
  assert.equal(f.since(at, wrapper).filter(e => e.op === 'remove' && e.effective).length, 1);
  assert.equal(f.since(at, p.national).filter(e => e.op === 'remove' && e.effective).length, 1);
  assert.equal(countOps(f, at, 'construct'), 0);
  assert.equal(countOps(f, at, 'add'), 0);
  for (const layer of [p.viewshed, p.external, f.local, f.basemap]) untouched(f, at, layer);
});
for (const action of ['off', 'superseded']) {
  for (const ok of [false, true]) {
    test('accounting: obsolete dependency ' + action + '/' + ok + ' launches no work', f => {
      pair(f); f.delayEnsure(true); f.refresh();
      if (action === 'off') f.off(); else f.refresh();
      noNewWork(f, f.mark(), () => f.ensures[0](ok));
    });
  }
}
test('accounting: obsolete bridge retry cannot launch work or affect another owner', f => {
  f.on(viewshedKey); f.pending[0].complete(null, 5);
  const other = f.ownerState(viewshedKey), bridge = f.t.window.ptOpsExternalCatalogBridge;
  f.t.window.ptOpsExternalCatalogBridge = null;
  f.on(); const oldRetry = [...f.timers.values()].at(-1);
  f.off(); f.on(); const currentRetry = [...f.timers.values()].at(-1);
  f.t.window.ptOpsExternalCatalogBridge = bridge;
  const at = f.mark();
  noNewWork(f, at, oldRetry);
  assert.equal(countOps(f, at, 'retry-fire'), 1);
  assert.equal(f.ownerState(viewshedKey), other);
  currentRetry(); assert.equal(f.pending.length, 2);
  f.pending[1].complete(null, 2); assert.equal(f.ops.loading[f.name], false);
});
// Expectations follow ptClearOpsLayers, ptClearCustomLayers and
// ptClearAllPt2SessionLayers; Local checkbox effects remain mocked above.
for (const action of ['clear', 'clearExternal', 'clearAll']) {
  test('sentinels: ' + action + ' removes exactly its owned layers', f => {
    const p = pair(f), at = f.mark();
    if (action === 'clearExternal') f.t.ptClearCustomLayers(); else f[action]();
    const removeOps = action !== 'clearExternal';
    const removeExternal = action !== 'clear';
    for (const [layer, removed] of [[p.national, removeOps], [p.viewshed, removeOps],
      [p.external, removeExternal], [f.local, action === 'clearAll']]) {
      assert.equal(f.map.hasLayer(layer), !removed);
      const removals = f.since(at, layer).filter(e => e.op === 'remove');
      assert.equal(removals.length, removed ? 1 : 0, 'Expected per-object removal attempts');
      assert.equal(removals.filter(e => e.effective).length, removed ? 1 : 0);
    }
    for (const key of [nationalKey, viewshedKey]) {
      const wrapper = f.owners.get(key).def.layer;
      assert.equal(f.map.hasLayer(wrapper), !removeOps);
      assert.equal(f.since(at, wrapper).filter(e => e.op === 'remove' && e.effective).length,
        removeOps ? 1 : 0);
    }
    assert.equal(f.t.ptCustomLayers.includes(p.externalRecord), !removeExternal);
    assert.equal(countOps(f, at, 'construct'), 0);
    assert.equal(countOps(f, at, 'add'), 0);
    untouched(f, at, f.basemap);
  });
}
test('owners: real viewsheds and national requests complete independently on one map', f => {
  f.on(); f.on(viewshedKey);
  assert.equal(f.pending[0].whereText, 'flow_cfs IS NOT NULL');
  assert.equal(f.pending[1].whereText, '1=1');
  const nationalPending = f.ownerState(nationalKey);
  f.pending[1].complete(null, 5);
  assert.equal(f.ownerState(nationalKey), nationalPending);
  const viewshedLoaded = f.ownerState(viewshedKey);
  f.pending[0].complete(null, 2);
  assert.equal(f.ownerState(viewshedKey), viewshedLoaded);
  f.refresh(); f.refresh(viewshedKey);
  const nationalRefreshing = f.ownerState(nationalKey), oldView = f.record(viewshedKey).layer;
  f.pending[3].complete(new Error('Viewshed refresh failure'));
  assert.equal(f.ownerState(nationalKey), nationalRefreshing);
  assert.equal(f.record(viewshedKey).layer, oldView);
  assert.equal(f.ops.loading[f.owners.get(viewshedKey).def.name], false);
  const failedView = f.ownerState(viewshedKey);
  f.pending[2].complete(null, 3);
  assert.equal(f.ownerState(viewshedKey), failedView);
  f.refresh(viewshedKey); const completedNational = f.ownerState(nationalKey);
  f.pending[4].complete(null, 6);
  assert.equal(f.ownerState(nationalKey), completedNational);
  assert.equal(f.record(viewshedKey).featureCount, 6);
  assert.equal(f.geometry.size, 2);
});
for (const owner of [nationalKey, viewshedKey]) {
  for (const error of [false, true]) {
    test('owners: ' + owner + ' removal/re-enable isolates late ' + (error ? 'failure' : 'success'), f => {
      const p = pair(f), other = owner === nationalKey ? viewshedKey : nationalKey;
      f.refresh(owner); const obsolete = f.pending.at(-1);
      f.refresh(other); const otherRequest = f.pending.at(-1);
      const otherPending = f.ownerState(other), at = f.mark();
      f.off(owner); f.on(owner); f.pending.at(-1).complete(null, 7);
      assert.equal(f.ownerState(other), otherPending);
      untouched(f, at, owner === nationalKey ? p.viewshed : p.national);
      untouched(f, at, p.external); untouched(f, at, f.local); untouched(f, at, f.basemap);
      noNewWork(f, f.mark(), () => obsolete.complete(error ? new Error('Old owner request') : null, 9));
      const reenabled = f.ownerState(owner);
      otherRequest.complete(null, 8);
      assert.equal(f.ownerState(owner), reenabled);
      assert.equal(f.record(other).featureCount, 8);
    });
  }
}
test('owners: status key wins over another owners display name', f => {
  pair(f); const viewEvent = f.events.at(-1); f.refresh();
  const before = f.ownerState(nationalKey), at = f.mark();
  f.t.window.dispatchEvent({type: 'ptOpsCatalogLayerStatus',
    detail: {...viewEvent, opsDisplayName: f.name}});
  assert.equal(f.ownerState(nationalKey), before);
  assert.equal(f.since(at).length, 0);
});
test('accounting: obsolete initial success after re-enable constructs nothing', f => {
  f.on(); const obsolete = f.pending[0];
  f.off(); f.on(); f.pending[1].complete(null, 2);
  f.on(viewshedKey); f.pending[2].complete(null, 5);
  noNewWork(f, f.mark(), () => obsolete.complete(null, 7));
});
for (const action of ['off', 're-enable-pending', 'clear', 'clearAll']) {
  test('accounting: ' + action + ' rejects obsolete initial success before construction', f => {
    f.on(); const obsolete = f.pending[0];
    if (action === 're-enable-pending') { f.off(); f.on(); } else f[action]();
    const pending = action === 're-enable-pending';
    assert.equal(f.record(), undefined, 'No initial snapshot is registered');
    assert.equal(!!f.ops.loading[f.name], pending);
    const at = f.mark(), eventCount = f.events.length, externalCount = f.external.length;
    noNewWork(f, at, () => obsolete.complete(null, 7));
    assert.equal(f.events.length, eventCount, 'Obsolete initial response emits no Ops status');
    assert.equal(f.external.length, externalCount, 'Obsolete initial response emits no External status');
    untouched(f, at, f.basemap);
    assert.equal(f.map.hasLayer(f.local), action !== 'clearAll');
    assert.equal(f.since(at, f.local).length, 0);
    if (pending) {
      assert.equal(f.pending.length, 2);
      f.pending[1].complete(null, 2);
      assert.equal(f.record().featureCount, 2);
      assert.equal(f.ops.loading[f.name], false);
    }
  });
}
test('ordinary External initial success needs no Ops metadata', f => {
  const options = {}, at = f.mark();
  f.t.ptAddArcgisFeatureLayerCurrentView('External initial',
    'https://example.invalid/FeatureServer/9', true, '#000', '1=1', options);
  assert.equal(f.pending.length, 1);
  f.pending[0].complete(null, 3);
  assert.equal(f.t.ptCustomLayers.length, 1);
  const rec = f.t.ptCustomLayers[0];
  assert.equal(rec.opsPromotedKey, '');
  assert.equal(rec.opsPromotedGeneration, null);
  assert.equal(options.opsPromotedGeneration, undefined);
  assert.equal(rec.featureCount, 3);
  assert.equal(f.map.hasLayer(rec.layer), true);
  for (const op of ['dependency', 'query-create', 'query-run', 'prepare', 'construct', 'add']) {
    assert.equal(countOps(f, at, op), 1);
  }
  assert.equal(countOps(f, at, 'add', true), 1);
  assert.equal(countOps(f, at, 'remove'), 0);
  assert.match(f.external.at(-1).msg, /Current-view FeatureServer layer added: 3/);
  assert.equal(f.events.length, 0);
  untouched(f, at, f.local); untouched(f, at, f.basemap);
});
module.exports = {fixture};
if (require.main === module) { console.log('RESULT passed=' + passed + ' failed=' + failed); process.exitCode = failed ? 1 : 0; }
