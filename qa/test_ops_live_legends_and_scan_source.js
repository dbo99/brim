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

// Descriptive BRIM marker and compact reservoir rendering use the real modules.
// Controlled stubs exercise HTML/state contracts; rendered fit remains a human check.
const definitions = read('03_functions', 'leaflet_ops_live_layer_definition_helpers.r');
const shared = read('03_functions', 'leaflet_ops_live_shared_helpers.r');
const definitionJs = embeddedRawJs(definitions);
const panelJs = embeddedRawJs(opsPanel);
const sharedJs = embeddedRawJs(shared);
[definitionJs, panelJs, sharedJs].forEach(js => new Function(js));
// Read the actual R projection; no Guide/browser/network dependency is involved.
const projectionRun = require('child_process').spawnSync(process.env.RSCRIPT_BINARY || 'Rscript',
  ['--vanilla', '-e', 'source("03_functions/leaflet_ops_live_helpers.r"); cat(jsonlite::toJSON(list(projection=pt_ops_live_delivery_projection(),identities=pt_ops_live_guide_identity_registry()),auto_unbox=TRUE,na="null"))'],
  {cwd:repoRoot,encoding:'utf8'});
assert.equal(projectionRun.status,0,projectionRun.stderr);
const projected = JSON.parse(projectionRun.stdout);
const projection = projected.projection;
const authored = JSON.parse(read('00_config','guide_product_resource_relationships.json')).products;
const classById = new Map(authored.map(p=>[p.product_id,p.delivery_class]));
assert.equal(classById.size,270);assert.equal(projected.identities.length,48);assert.equal(projection.length,47);
const constantTokens = projected.identities.map(r=>r.source_token).filter(t=>t.startsWith('PT_'));
assert.equal(constantTokens.length,4);
const opsModuleText = fs.readdirSync(path.join(repoRoot,'03_functions')).filter(n=>/^leaflet_ops_live_.*\.r$/.test(n)).map(n=>read('03_functions',n)).join('\n');
const constantValues = constantTokens.map(token=>{
  const matches=[...opsModuleText.matchAll(new RegExp('var '+token+" = ['\"]([^'\"]+)['\"];",'g'))];
  assert.equal(matches.length,1,'Actual constant definition '+token);return matches[0][1];
});
const runtimeName = token => constantTokens.includes(token)?constantValues[constantTokens.indexOf(token)]:token;
const registrationFactory = new Function('OPS_CATALOG_PRIMARY_PANEL','OPS_DELIVERY_PROJECTION',...constantTokens,
  definitionJs.slice(0, definitionJs.indexOf('  var ptOpsPromotedSlowTimers')) +
  '\nreturn {add:addOpsLayer, rows:opsLayers, names:ptOpsBrimPreparedNames, lookup:ptOpsDeliveryForName};');
const makeRegistration = value => registrationFactory({Disabled:'disabled'},value,...constantValues);
const registration = makeRegistration(projection);
for(const row of projection)assert.equal(row.delivery_class,classById.get(row.stable_id));
const reservoirName = 'Reservoirs | storage-centric | CDEC / CNRFC / USACE';
const preparedNames = [reservoirName,'CoCoRaHS | CA daily','CoCoRaHS | 50-state daily',
  'Delta ops snapshot | CVP/SWP','Streamflow | USGS | Ca','Groundwater | USGS | Ca/wrnNv/srnOr',
  'Soil moisture | USDA NRCS SCAN | Ca/Nv','Snow pillow SWE | CDEC / USDA NRCS | Ca/Nv/Or',
  'Major Water-Supply Basin Forecasts','Wind flow | NOAA GFS surface','Wind flow | NOAA HRRR surface detail',
  'Wind outlook | NOAA NBM guidance','Observed wind | METAR/ASOS speed + gusts',
  'NBM Snow Levels','NBM 6-Hour QPF','NBM Accumulated QPF (0–10 d)'];
assert.deepStrictEqual(Object.keys(registration.names).sort(),preparedNames.slice().sort());
for (const name of preparedNames) {
  const layer={onAdd(){throw Error('Presentation activated a layer');}};
  const row={name,layer};registration.add(row);
  assert.strictEqual(row.brimPrepared,true);assert.strictEqual(row.layer,layer);
}
for(const name of ['QPE | NWS MRMS 1-day','QPE | NWS RFC mosaic 7-day','WPC QPF Day 1',
  'NOAA GOES GeoColor',"Streamflow | multi-agency | Nat'l",'CNRFC forecast points | river/reservoir',
  'NWS weather stations | NWS/WRH time series','Unknown GitHub data','toString']) {
  const row={name};registration.add(row);assert(!row.brimPrepared,name);
}
const prior=registration.rows.length;
registration.add({name:reservoirName,catalogSourceDisplayName:'Disabled'});
assert.equal(registration.rows.length,prior,'Marker changed catalog availability');
if(process.env.BRIM_OPS_CONSUMER_MAP) {
  const audit=JSON.parse(fs.readFileSync(process.env.BRIM_OPS_CONSUMER_MAP));
  assert.deepStrictEqual(preparedNames.slice().sort(),audit.rows.filter(r=>r.disposition==='POSITIVE_VERIFIED_BRIM_PREPARED').map(r=>r.internal_name).sort());
}
let constructorArgs;
const reservoirLayer={identity:'existing layer'};
let reservoir;
const reservoirBlock=definitionJs.slice(definitionJs.indexOf('  if (includeCdecReservoirStorage'),
  definitionJs.indexOf('  if (includeCnrfcPrecipWeatherStations'));
new Function('includeCdecReservoirStorage','CDEC_RESERVOIR_STORAGE_URL','CDEC_RESERVOIR_STORAGE_SUMMARY_URL',
  'makeCdecReservoirStorageLayer','addOpsLayer',reservoirBlock)(true,'https://example.test/storage','https://example.test/summary',
  options=>{constructorArgs=options;return reservoirLayer;},row=>{registration.add(row);reservoir=row;});
assert.equal(reservoir.name,reservoirName);
assert.equal(reservoir.panelLabel,'Reservoirs | storage + forecast links');
assert.equal(reservoir.helperText,'CDEC / CNRFC / USACE');
assert.equal(reservoir.category,'Forecasts / Outlooks');
assert.equal(reservoir.subgroup,'River / Reservoir Forecasts');
assert.strictEqual(reservoir.layer,reservoirLayer);
assert.equal(constructorArgs.name,reservoirName);
assert.equal(constructorArgs.url,reservoir.sourceUrl);
assert.equal(constructorArgs.summaryUrl,reservoir.infoUrl);
assert(constructorArgs.note.includes('Near-live CDEC storage') && constructorArgs.note.includes('CNRFC forecast/release links') && constructorArgs.note.includes('USACE reservoir-plot links'));
const waterName = 'Major Water-Supply Basin Forecasts';
let water, waterArgs;
const waterLayer={identity:'water supply'};
const waterBlock=definitionJs.slice(definitionJs.indexOf('  if (includeMajorWaterSupplyBasinForecasts'),definitionJs.indexOf('  if (includeCdecReservoirStorage'));
const geometry={features:Array.from({length:23},()=>({}))};
new Function('includeMajorWaterSupplyBasinForecasts','MAJOR_WATER_SUPPLY_GEOMETRY','MAJOR_WATER_SUPPLY_CNRFC_URL',
  'MAJOR_WATER_SUPPLY_CBRFC_URL','makeMajorWaterSupplyBasinForecastLayer','addOpsLayer',waterBlock)
  (true,geometry,'https://example.test/cnrfc','https://example.test/cbrfc',options=>{waterArgs=options;return waterLayer;},row=>{registration.add(row);water=row;});
assert.equal(water.panelLabel,'Water-Supply Basin Forecasts');assert.equal(water.helperText,'CNRFC / CBRFC');
assert.equal(water.name,waterName);assert.equal(water.category,reservoir.category);assert.equal(water.subgroup,reservoir.subgroup);
assert.strictEqual(water.layer,waterLayer);assert.strictEqual(waterArgs.geometry,geometry);
assert.equal(waterArgs.name,waterName);assert.equal(waterArgs.cnrfcUrl,water.sourceUrl);assert.equal(waterArgs.cbrfcUrl,water.infoUrl);
if(process.env.BRIM_OPS_CONSUMER_MAP) {
  const audit=JSON.parse(fs.readFileSync(process.env.BRIM_OPS_CONSUMER_MAP));
  assert.equal(audit.rows.length,48);assert.equal(new Set(audit.rows.map(r=>r.stable_id)).size,48);
  let positives=0;
  for(const record of audit.rows) {
    const row={name:record.internal_name};registration.add(row);
    const positive=record.disposition==='POSITIVE_VERIFIED_BRIM_PREPARED';
    assert.equal(row.brimPrepared===true,positive,record.stable_id);if(positive)positives++;
  }
  assert.equal(positives,16);assert.equal(audit.rows.length-positives,32);
}
const direct={name:'Direct <agency> & service',panelLabel:'Direct <agency> & service',helperText:'A < B & C',category:'Hydro Observations',layer:{}};
const escape=value=>String(value).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;').replace(/'/g,'&#39;');
// Real link/refresh builders, with only URL-title shortening stubbed.
const rowLinks=new Function('escapeHtml','compactUrlForTitle',sharedJs.slice(sharedJs.indexOf('  function ptOpsCompactLinkLabel'),sharedJs.indexOf('  function addOpsExternalLinks'))+'\nreturn layerRowLinksHtml;')(escape,String);
const deltaExtra=new Function('return '+definitionJs.match(/extraRowHtml: ('[^\n]+'),/)[1])();
const short={name:'Delta ops snapshot | CVP/SWP',panelLabel:'Delta',category:'Hydro Observations',brimPrepared:true,refreshable:true,sourceUrl:'https://example.test/source?a=1&b=2',extraRowHtml:deltaExtra,layer:{}};
const long={name:'Wind flow | NOAA HRRR surface detail',category:'Forecasts / Outlooks',brimPrepared:true,layer:{}};
const snow={name:'NBM Snow Levels',category:'Forecasts / Outlooks',brimPrepared:true,extraRowHtml:'<a href="#" data-pt-ops-action="nbm-snow-card">lgnd</a><label><input type="checkbox" data-pt-ops-action="nbm-snow-labels" checked>lbl</label>',layer:{}};
[direct,short,long,snow].forEach(row=>registration.add(row));
const rows=[reservoir,direct,water,short,long,snow];
let networkCalls=0, removals=0, events=0, deactivations=0;
const checkbox={checked:false,dispatchEvent(){events++;}};
const activeLayers={},defs={[reservoirName]:reservoir},checkboxes={[reservoirName]:checkbox};
reservoir.onDeactivate=()=>deactivations++;
const api=new Function('opsLayers','escapeHtml','layerRowLinksHtml','opsExternalLinksForCategory',
  'checkboxByName','opsDefByName','activeLayers','map','window','clearAllOpsLayerLoading','redrawLegend','redrawStatus','updateOpsHeaderCount','fetch',
  'var activeLegendDefs={}, statusRows={};\n'+panelJs.slice(0,panelJs.indexOf('  function addFixedOpsPanel()'))+
  '\nreturn {render:buildOpsPanelHtml,activate:ptOpsActivateLayerByName,clear:ptClearOpsLayers,kind:ptOpsDeliveryKind,badge:ptOpsDeliveryBadgeHtml,primary:ptOpsPrimaryTitleHtml};')
  (rows,escape,rowLinks,()=>'',checkboxes,defs,activeLayers,{removeLayer(){removals++;},getPane(){return null;}},{},()=>{},()=>{},()=>{},()=>{},()=>{networkCalls++;throw Error('Unexpected fetch');});
const html=api.render();assert.equal(api.render(),html,'Repeated render duplicated content');
assert.equal(networkCalls,0);assert.equal(events,0);assert.equal(removals,0);
// Minimal controlled HTML/event model: structural and delegated-callback evidence,
// never a browser, native-layout, visual-fit or accessibility-tree certification.
const decode=text=>text.replace(/&(?:amp|lt|gt|quot|#39);/g,x=>({'&amp;':'&','&lt;':'<','&gt;':'>','&quot;':'"','&#39;':"'"})[x]);
function parseFixture(markup) {
  const root={tag:'root',attrs:{},children:[],listeners:{}};const stack=[root];
  for(const token of markup.match(/<[^>]+>|[^<]+/g)||[]) {
    if(token.startsWith('</')) {assert.equal(stack.pop().tag,token.slice(2,-1));continue;}
    if(!token.startsWith('<')) {stack.at(-1).children.push(decode(token));continue;}
    const tag=token.match(/^<([\w-]+)/)[1],attrs={};
    for(const m of token.slice(tag.length+1,-1).matchAll(/([\w-]+)(?:="([^"]*)")?/g))attrs[m[1]]=decode(m[2]||'');
    const node={tag,attrs,children:[],listeners:{},parent:stack.at(-1),checked:'checked' in attrs};stack.at(-1).children.push(node);
    if(!['input','br','hr'].includes(tag))stack.push(node);
  }
  assert.equal(stack.length,1);
  function visit(node) {
    node.getAttribute=k=>node.attrs[k]??null;node.setAttribute=(k,v)=>node.attrs[k]=v;node.removeAttribute=k=>delete node.attrs[k];
    node.matches=selector=>selector.startsWith('.')?(node.attrs.class||'').split(' ').includes(selector.slice(1)):
      selector.startsWith('#')?node.attrs.id===selector.slice(1):(()=>{const m=selector.match(/^(\w+)?(?:\[([\w-]+)(?:="([^"]*)")?\])?$/);return !!m&&(!m[1]||node.tag===m[1])&&(!m[2]||(m[2] in node.attrs&&(m[3]===undefined||node.attrs[m[2]]===m[3])));})();
    node.closest=selector=>node.matches(selector)?node:node.parent?node.parent.closest(selector):null;
    node.querySelectorAll=selector=>node.children.filter(x=>typeof x!=='string').flatMap(x=>[...(x.matches(selector)?[x]:[]),...x.querySelectorAll(selector)]);
    node.querySelector=selector=>node.querySelectorAll(selector)[0]||null;
    node.contains=child=>child===node||!!(child.parent&&node.contains(child.parent));
    node.addEventListener=(type,fn)=>(node.listeners[type]||(node.listeners[type]=[])).push(fn);
    node.classList={contains:c=>(node.attrs.class||'').split(' ').includes(c),toggle(c,on){const set=new Set((node.attrs.class||'').split(' ').filter(Boolean));if(on)set.add(c);else set.delete(c);node.attrs.class=[...set].join(' ');}};
    node.children.filter(x=>typeof x!=='string').forEach(visit);
  }
  visit(root);return root;
}
const textOf=n=>typeof n==='string'?n:n.children.map(textOf).join('');
const body=parseFixture(html),allIds=body.querySelectorAll('[id]').map(n=>n.attrs.id);
assert.equal(new Set(allIds).size,allIds.length,'Duplicate panel IDs');
assert.equal(body.querySelectorAll('.pt-ops-delivery-key').length,1);
assert.equal(textOf(body.querySelector('#pt-ops-delivery-key')),'BRIM-M — Managed: BRIM-prepared data or curated collections.BRIM-E — Enhanced: External services with BRIM-added features.Original sources remain credited. Features and refresh schedules vary by layer.');
assert(html.indexOf('class="pt-ops-delivery-key"')>html.indexOf('Use Clear ops in the ribbon'));
assert.equal(textOf(body.querySelector('#pt-ops-reservoir-description')),'Symbols show observed storage; popups link to forecasts and reservoir operations.');
for(let i=0;i<rows.length;i++) {
  const row=body.querySelector('[data-pt-ops-row-index="'+i+'"]'),def=rows[i],label=row.querySelector('.pt-ops-layer-label');
  const primary=label.querySelector('.pt-ops-primary-title'),input=row.querySelector('input[data-pt-ops-index]');
  assert.equal(label.attrs.for,input.attrs.id);assert.strictEqual(input.parent,row);assert.strictEqual(input.closest('label'),null);
  assert.equal(textOf(primary),(def.panelLabel||def.name)+(api.kind(def)==='managed'?' BRIM-M':api.kind(def)==='enhanced'?' BRIM-E':''));
  const markers=row.querySelectorAll('.pt-ops-delivery-badge');assert.equal(markers.length,api.kind(def)?1:0);
  if(api.kind(def)) {
    assert(primary.contains(markers[0]));assert.strictEqual(markers[0].parent,primary.querySelector('.pt-ops-primary-tail'));
    assert.equal(markers[0].attrs.role,'img');assert.equal(markers[0].attrs['aria-label'],'BRIM-'+api.kind(def));
    assert(!('tabindex' in markers[0].attrs));assert.equal(markers[0].tag,'span');
  } else assert(!row.matches('.pt-ops-layer-managed'));
  for(const id of (input.attrs['aria-describedby']||'').split(' ').filter(Boolean))assert(body.querySelector('#'+id));
  const agencies=row.querySelectorAll('.pt-ops-layer-agencies');const paired=i===0||i===2;
  assert.equal(agencies.length,paired?1:0);
  if(paired) {assert.equal(textOf(agencies[0]),def.helperText);assert(!primary.contains(agencies[0]));assert.equal((html.split(def.helperText).length-1),1);}
  const tools=row.querySelector('.pt-ops-row-actions');assert.strictEqual(tools.parent,label.parent);
  assert(!label.contains(tools));assert.equal(label.querySelectorAll('a').length,0);
  assert.equal(label.querySelectorAll('[data-pt-ops-action]').length,0);
  const originalLinks=parseFixture(rowLinks(def));
  assert.deepStrictEqual(tools.querySelectorAll('a').slice(0,originalLinks.querySelectorAll('a').length).map(n=>n.attrs),originalLinks.querySelectorAll('a').map(n=>n.attrs));
  if(def.extraRowHtml)assert(html.includes(def.extraRowHtml),'Rich extraRowHtml changed');
}
assert(html.includes('Direct &lt;agency&gt; &amp; service') && html.includes('A &lt; B &amp; C'));
reservoir.panelLabel='<img src=x>';reservoir.helperText='A < B & C';
const escapedHtml=api.render();assert.equal(textOf(parseFixture(escapedHtml).querySelector('[data-pt-ops-row-index="0"]').querySelector('.pt-ops-primary-title')),'<img src=x> BRIM-M');
assert(escapedHtml.includes('A &lt; B &amp; C'));assert(!escapedHtml.includes('<img src=x>'));
reservoir.panelLabel='Reservoirs | storage + forecast links';reservoir.helperText='CDEC / CNRFC / USACE';
assert.equal(api.activate(reservoir.panelLabel).ok,false,'Display alias changed activation identity');
assert.equal(api.activate(reservoirName).ok,true);assert.equal(events,1);assert(checkbox.checked);
activeLayers[reservoirName]=reservoirLayer;
api.clear();api.clear();assert.equal(removals,1);assert.equal(deactivations,1);assert(!checkbox.checked);
assert.equal(Object.keys(activeLayers).length,0);assert.equal(networkCalls,0);
// Bind the exact existing delegated action and per-checkbox callbacks once.
const bindStart=panelJs.indexOf("    body.addEventListener('click'");
const bindEnd=panelJs.indexOf('\n    updateOpsHeaderCount();',panelJs.indexOf("    var checks = body.querySelectorAll",bindStart));
const handlerSource=panelJs.slice(bindStart,bindEnd);assert(bindStart>=0&&bindEnd>bindStart);
let adds=0,refreshes=0,zooms=0,cards=0;const changes=[],statuses=[],loading=[],rowByName={};
rows.forEach(def=>{def.layer.addTo=()=>adds++;def.layer.refreshCurrentView=()=>refreshes++;});
new Function('body','opsLayers','checkboxByName','rowByName','opsDefByName','activeLayers','map','window','recordStatus','setOpsLayerLoading','updateOpsHeaderCount',handlerSource)
  (body,rows,checkboxes,rowByName,defs,activeLayers,{removeLayer(){removals++;}},
    {ptDeltaOpsZoomToDefault(){zooms++;},ptNbmSnowLevelsShowCard(){cards++;},ptDeltaOpsSetLabelsVisible(v){changes.push(['delta',v]);},ptNbmSnowLevelsSetLabelsVisible(v){changes.push(['snow',v]);}},
    (...args)=>statuses.push(args),(...args)=>loading.push(args),()=>{});
assert.equal(body.listeners.click.length,1);assert.equal(body.listeners.change.length,1);
for(const input of body.querySelectorAll('input[data-pt-ops-index]'))assert.equal(input.listeners.change.length,1);
function dispatch(target,type) {
  const e={target,stopped:false,prevented:false,preventDefault(){this.prevented=true;},stopPropagation(){this.stopped=true;}};
  for(let n=target;n;n=n.parent){for(const fn of n.listeners[type]||[])fn(e);if(e.stopped)break;}return e;
}
function clickModel(target) {
  const event=dispatch(target,'click');if(event.prevented)return;
  const label=target.closest('label');const control=target.tag==='input'?target:label?(label.attrs.for?body.querySelector('#'+label.attrs.for):label.querySelector('input')):null;
  if(control){control.checked=!control.checked;dispatch(control,'change');}
}
const deltaRow=body.querySelector('[data-pt-ops-row-index="3"]'),deltaToggle=deltaRow.querySelector('input[data-pt-ops-index]');
clickModel(deltaRow.querySelector('[data-pt-ops-action="refresh"]'));assert.equal(adds,0);assert.equal(refreshes,0);assert(statuses.at(-1)[1].includes('Turn this Ops layer on'));
clickModel(deltaRow.querySelector('.pt-ops-primary-title'));assert(deltaToggle.checked);assert.equal(adds,1);assert.strictEqual(activeLayers[short.name],short.layer);
clickModel(deltaRow.querySelector('[data-pt-ops-action="refresh"]'));assert.equal(refreshes,1);assert.equal(adds,1);assert(deltaToggle.checked);
clickModel(deltaRow.querySelector('[data-pt-ops-action="delta-ops-zoom"]'));assert.equal(zooms,1);assert.equal(adds,1);
clickModel(deltaRow.querySelector('[data-pt-ops-action="delta-labels"]'));assert.deepStrictEqual(changes,[['delta',false]]);assert(deltaToggle.checked);
clickModel(deltaRow.querySelector('a[href="https://example.test/source?a=1&b=2"]'));assert.equal(adds,1);assert(deltaToggle.checked);
const snowRow=body.querySelector('[data-pt-ops-row-index="5"]');
clickModel(snowRow.querySelector('[data-pt-ops-action="nbm-snow-labels"]'));assert.deepStrictEqual(changes.at(-1),['snow',false]);assert.equal(adds,1);
clickModel(snowRow.querySelector('[data-pt-ops-action="nbm-snow-card"]'));assert.equal(cards,0);assert(statuses.at(-1)[1].includes('Turn on an NBM forecast layer'));
clickModel(snowRow.querySelector('.pt-ops-layer-label'));assert.equal(adds,2);
clickModel(snowRow.querySelector('[data-pt-ops-action="nbm-snow-card"]'));assert.equal(cards,1);assert.equal(adds,2);
clickModel(deltaToggle);assert(!deltaToggle.checked);assert(!activeLayers[short.name]);assert.equal(networkCalls,0);
const loadingSource=sharedJs.slice(sharedJs.indexOf('  function setOpsLayerLoading'),sharedJs.indexOf('  function isOpsLayerLoading'));
const setLoading=new Function('rowByName',loadingSource+'\nreturn setOpsLayerLoading;')(rowByName);
setLoading(short.name,true);assert(deltaRow.matches('.pt-ops-layer-loading'));assert.equal(deltaRow.attrs['aria-busy'],'true');
setLoading(short.name,false);assert(!deltaRow.matches('.pt-ops-layer-loading'));assert(!('aria-busy' in deltaRow.attrs));
const rowCss=shared.slice(shared.indexOf('      .pt-ops-layer-row {'),shared.indexOf('      .pt-ops-row-links a:hover'));
assert(rowCss.includes('grid-template-columns: max-content minmax(0, 1fr);') && rowCss.includes('align-items: start;'));
assert(/\.pt-ops-row-content\s*\{[^}]*flex-wrap: wrap;[^}]*min-width: 0;/s.test(rowCss));
assert(rowCss.includes(':not(.pt-ops-layer-loading):not(.pt-ops-warn):not(.pt-ops-bad) > input:not(:disabled)'));
assert(rowCss.includes('color: #244B6B;') && rowCss.includes('font-size: 0.83em;'));
assert(/\.pt-ops-primary-tail\s*\{\s*white-space: nowrap;/.test(rowCss));
assert(/\.pt-ops-layer-name\s*\{\s*white-space: normal;/.test(rowCss));
assert(!/\.pt-ops-(?:primary-title|layer-label)\s*\{[^}]*white-space:\s*nowrap/s.test(rowCss));
assert(!/overflow(?:-x)?:\s*(?:hidden|auto|scroll)|text-overflow|(?:^|[;{])\s*height:/m.test(rowCss));
assert(!/\.pt-ops-delivery-badge\s*\{[^}]*(?:display|flex|position|width|margin-left|cursor|box-shadow):/s.test(rowCss));
assert(/\.pt-ops-delivery-badge\s*\{[^}]*padding: 0 2px;[^}]*border: 1px solid #89959d;[^}]*border-radius: 2px;/s.test(rowCss));
assert(!shared.includes('pt-ops-prepared-marker')&&!opsPanel.includes('· BRIM'));
assert(rowCss.includes('background: #e8f2f8;')&&rowCss.includes('color: #45545f;'));
assert(/\.pt-ops-layer-agencies\s*\{[^}]*display: block;[^}]*font-size: 0.9em;[^}]*font-weight: 400;/s.test(rowCss));
assert(shared.includes('.pt-ops-layer-row.pt-ops-layer-loading .pt-ops-layer-spinner'));
assert(shared.includes('.pt-ops-warn { color: #9a5a00; }') && shared.includes('.pt-ops-bad { color: #9c1c1c; }'));
// The earlier !important tint overrides the later white body background.
assert(shared.includes('background: rgba(226, 238, 235, 0.96) !important;'));
const luminance=rgb=>rgb.map(c=>{c/=255;return c<=.04045?c/12.92:((c+.055)/1.055)**2.4;}).reduce((sum,c,i)=>sum+c*[.2126,.7152,.0722][i],0);
const contrast=(a,b)=>{const x=luminance(a),y=luminance(b);return (Math.max(x,y)+.05)/(Math.min(x,y)+.05);};
const ratios=[];
for(const underneath of [0,255]) {
  const bg=[226,238,235].map(c=>.96*c+.04*underneath);
  for(const [name,fg] of [['managed-title',[36,75,107]],['enhanced-badge',[69,84,95]],['agencies',[89,101,108]],['key',[69,92,87]]]) {
    const ratio=contrast(fg,bg);assert(ratio>=4.5,name+' contrast '+ratio);ratios.push({name,underneath,ratio});
  }
}
const managedBadgeContrast=contrast([36,75,107],[232,242,248]);assert(managedBadgeContrast>=4.5);
ratios.push({name:'managed-badge',ratio:managedBadgeContrast});
// Exact CNRFC display alias, internal identity, layer and independent homepage link.
let cnrfc,cnrfcArgs;
const cnrfcBlock=definitionJs.slice(definitionJs.indexOf('  if (includeCnrfcRiverReservoirForecastPoints'),definitionJs.indexOf('  if (includeMajorWaterSupplyBasinForecasts'));
new Function('includeCnrfcRiverReservoirForecastPoints','CNRFC_RIVER_RESERVOIR_FORECAST_POINTS','CnrfcRiverReservoirForecastLayer','addOpsLayer',cnrfcBlock)
  (true,[{id:'fixture'}],function(options){cnrfcArgs=options;},row=>{registration.add(row);cnrfc=row;});
assert.equal(cnrfc.name,'CNRFC forecast points | river/reservoir');
assert.equal(cnrfc.panelLabel,'CNRFC river/reservoir forecast points');
assert.equal(cnrfc.guideProductId,'ops_cnrfc_forecast_points');assert.equal(cnrfc.deliveryClass,'brim_managed');
assert.equal(cnrfcArgs.name,cnrfc.name);assert.equal(cnrfc.sourceUrl,'https://www.cnrfc.noaa.gov/');
assert.equal(cnrfc.infoUrl,cnrfc.sourceUrl);assert.equal(cnrfc.infoLabel,'CNRFC');
assert(rowLinks(cnrfc).includes('href="https://www.cnrfc.noaa.gov/"'));
// Actual projection -> actual registration -> actual title/badge render for all 48.
const allRegistration=makeRegistration(projection);
for(const identity of projected.identities)allRegistration.add({name:runtimeName(identity.source_token),category:'Fixture',layer:{}});
assert.equal(allRegistration.rows.length,48);
const allResults=allRegistration.rows.map((row,i)=>{
  const identity=projected.identities[i],eligible=identity.included_by_default;
  assert.equal(row.guideProductId,eligible?identity.stable_id:null);
  assert.equal(row.deliveryClass,eligible?classById.get(identity.stable_id):null);
  const markup=api.primary(row),parsed=parseFixture(markup),badges=parsed.querySelectorAll('.pt-ops-delivery-badge');
  const expected=row.deliveryClass==='brim_managed'?'BRIM-M':row.deliveryClass==='brim_enhanced'?'BRIM-E':'';
  assert.equal(badges.length,expected?1:0);if(expected)assert.equal(textOf(badges[0]),expected);
  return {stable_id:identity.stable_id,delivery_class:row.deliveryClass,badge:expected,prepared:row.brimPrepared===true};
});
assert.equal(allResults.filter(r=>r.prepared).length,16);
assert.equal(allResults.filter(r=>r.badge==='BRIM-M').length,18);
assert.equal(allResults.find(r=>r.stable_id==='ops_radar_iem_nexrad').badge,'');
// Exact authored six-ID correction must reach the existing registration/renderer.
const providerRadarQpe=['ops_radar_noaa_mrms','ops_qpe_mrms_1hr','ops_qpe_mrms_1day',
  'ops_qpe_mrms_3day','ops_qpe_rfc_1day','ops_qpe_rfc_7day'];
for(const id of providerRadarQpe) {
  assert.equal(classById.get(id),'provider_hosted');
  const row=allResults.find(r=>r.stable_id===id);assert(row,id);
  assert.equal(row.delivery_class,'provider_hosted');assert.equal(row.badge,'');
  assert.equal(row.prepared,false);
}
assert.equal(allResults.filter(r=>r.badge==='BRIM-E').length,22);
assert.equal(allResults.filter(r=>r.delivery_class==='provider_hosted').length,7);
for(const id of ['ops_nws_weather_stations','ops_cnrfc_forecast_points']) {
  const row=allResults.find(r=>r.stable_id===id);assert.equal(row.badge,'BRIM-M');assert.equal(row.prepared,false);
}
// Unknown, inherited/prototype metadata, missing, malformed and collision fixtures.
for(const metadata of [undefined,null,{},[],[{stable_id:'x',source_token:'Unknown',delivery_class:'invalid'}],
  [{stable_id:'__proto__',source_token:'Unknown',delivery_class:'brim_managed'}],
  [Object.create({stable_id:'x',source_token:'Unknown',delivery_class:'brim_managed'})],
  [projection[0],projection[0]],
  [projection[0],{...projection[1],source_token:projection[0].source_token}]]) {
  const reg=makeRegistration(metadata);const row={name:'Unknown',guideProductId:'x',deliveryClass:'brim_enhanced',layer:{}};
  reg.add(row);assert.equal(reg.rows.length,1);assert.equal(api.kind(row),'');assert(!api.primary(row).includes('BRIM-'));
}
for(const name of ['Unknown','__proto__','constructor','toString']) {
  const row={name,layer:{}};registration.add(row);assert.equal(api.kind(row),'');
}
for(const cls of ['provider_hosted','not_applicable']) {
  const reg=makeRegistration([{stable_id:'fixture',source_token:'Fixture',delivery_class:cls}]);
  const row={name:'Fixture'};reg.add(row);assert.equal(api.kind(row),'');assert.equal(api.primary(row),'Fixture');
}
const constantRow=projection.find(r=>r.source_token===constantTokens[0]);
const collision=makeRegistration([constantRow,{stable_id:'collision',source_token:constantValues[0],delivery_class:'brim_enhanced'}]);
const collisionRow={name:constantValues[0]};collision.add(collisionRow);assert.equal(api.kind(collisionRow),'');
// Prove late constant resolution from the real declaration, not copied display text.
const late=new Function('OPS_CATALOG_PRIMARY_PANEL','OPS_DELIVERY_PROJECTION',
  definitionJs.slice(0,definitionJs.indexOf('  var ptOpsPromotedSlowTimers'))+
  '\nvar early=ptOpsDeliveryForName('+JSON.stringify(constantValues[0])+'); var '+constantTokens[0]+'='+JSON.stringify(constantValues[0])+';'+
  'return {early:early,late:ptOpsDeliveryForName('+constantTokens[0]+')};')({},[constantRow]);
assert.equal(late.early,null);assert.equal(late.late.stable_id,constantRow.stable_id);
const guideJs=read('03_functions','js','leaflet_brim_guide.js');
const deliveryLabel=new Function(guideJs.slice(guideJs.indexOf('  function deliveryLabel('),guideJs.indexOf('  function relationshipRoleLabel('))+'return deliveryLabel;')();
assert.deepStrictEqual(['brim_managed','brim_enhanced','provider_hosted','not_applicable'].map(deliveryLabel),['BRIM-managed','BRIM-enhanced','Provider-hosted','Not applicable']);
if(process.env.BRIM_PROVIDER_TEST_BUNDLE) {
  const guide=JSON.parse(fs.readFileSync(process.env.BRIM_PROVIDER_TEST_BUNDLE));
  let occurrences=0;
  for(const product of guide.products)for(const link of product.relatedResources) {
    assert.equal(link.deliveryClass,classById.get(product.id));assert(deliveryLabel(link.deliveryClass));occurrences++;
  }
  for(const resource of guide.resources)for(const product of resource.representedProducts) {
    assert.equal(product.deliveryClass,classById.get(product.productId));assert(deliveryLabel(product.deliveryClass));occurrences++;
  }
  assert.equal(occurrences,242);
}
if(process.env.BRIM_OPS_BADGE_RESULT)fs.writeFileSync(process.env.BRIM_OPS_BADGE_RESULT,JSON.stringify({status:'PASS',rows:allResults,contrast:ratios,browser:'UNAVAILABLE_POLICY'},null,2)+'\n');
console.log('OPS_DELIVERY_BADGES=PASS; 48 ACTUAL_IDENTITY_RENDERINGS=PASS; PREPARED_16_UNPREPARED_32=PASS; PAIRED_TITLE_BADGE_ESCAPE_ASSOCIATION=PASS; ACTION_ROUTING_CLEAR_STATUS=PASS; CONTRAST='+JSON.stringify(ratios)+'; RENDERED_FIT=PENDING_HUMAN_REVIEW');
