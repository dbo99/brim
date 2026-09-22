// Actual emitted well controller plus actual Clear Local/Clear All handlers.
// DOM/Leaflet are small offline harnesses; this is not mounted browser acceptance.
const assert = require('assert');
const fs = require('fs');
const path = require('path');
const vm = require('vm');
const {spawnSync} = require('child_process');
const root = path.resolve(__dirname, '..');
const source = fs.readFileSync(path.join(root, '03_functions/leaflet_layer_local_well_spring_helpers.r'), 'utf8');
const match = source.match(/js <- r"---\(([\s\S]*?)\)---"/);
assert(match && match[1].trim().startsWith('function(el, x, data)'));
new vm.Script(`(${match[1]})`); // parse in its intended htmlwidgets function scope
let passed = 0;
function test(name, fn) { fn(); passed++; console.log(`PASS ${name}`); }
function element(attrs = {}) {
  const handlers = {};
  const classes = new Set();
  return {classList:{add(c){classes.add(c);},contains(c){return classes.has(c);},toggle(c,on){on?classes.add(c):classes.delete(c);}},
    appendChild(c){this.children.push(c);return c;},
    dispatch(name){(handlers[name] || []).forEach(fn=>fn.call(this,{preventDefault(){},stopPropagation(){}}));},
    style: {}, attrs, children: [], value: attrs.value || '',
    addEventListener(name, fn) { (handlers[name] ||= []).push(fn); },
    getAttribute(name) { return attrs[name] || ''; },
    setAttribute(name, value) { attrs[name] = value; },
    click() { (handlers.click || []).forEach(fn => fn.call(this, {preventDefault(){},stopPropagation(){}})); },
    keydown(key) {
      (handlers.keydown || []).forEach(fn => fn.call(this,{key,preventDefault(){},stopPropagation(){}}));
      // Model native button keyboard activation; no custom controller key handler.
      if (attrs.type === 'button' && (key === 'Enter' || key === ' ')) this.click();
    },
    set innerHTML(html) {
      this.html = html;
      this.children = [...html.matchAll(/<(button|input)\b([^>]*)>/g)].map(m => {
        const a = {}; for (const v of m[2].matchAll(/([\w-]+)="([^"]*)"/g)) a[v[1]] = v[2];
        return element(a);
      });
    },
    get innerHTML() { return this.html || ''; },
    querySelectorAll(selector) { return this.children.filter(c => selector.startsWith('input') ? c.attrs.type==='checkbox' : (c.className || c.attrs.class || '').split(' ').includes(selector.replace(/^\./, ''))); },
    querySelector(selector) { return this.querySelectorAll(selector)[0] || null; }
  };
}
const controls = [];
const state = new Map();
const style = new Map();
const docHandlers = {};
const timers = [];
const layers = new Set();
const events = {};
const fits = [];
const map = {
  fitBounds(bounds,options) { fits.push(JSON.parse(JSON.stringify({bounds,options}))); return this; },
  zoom: 12,
  getZoom() { return this.zoom; },
  hasLayer(l) { return layers.has(l); },
  addLayer(l) { layers.add(l); return this; },
  removeLayer(l) { layers.delete(l); return this; },
  on(n, fn) { (events[n] ||= []).push(fn); return this; },
  fire(n, evt={}) { (events[n] || []).forEach(fn => fn(evt)); }
};
function group(type) { return {type, members: [],
  addTo(m) { m.addLayer(this); return this; },
  addLayer(l) { this.members.push(l); return this; },
  addLayers(ls) { this.members.push(...ls); return this; },
  clearLayers() { this.members = []; return this; }
}; }
function register(name) {
  const input = element({type:'checkbox'});
  input.checked=false;
  input.click=function() {this.checked=!this.checked;map.fire(this.checked?'overlayadd':'overlayremove',{name});this.dispatch('change');};
  state.set(name,input);
  return input;
}
let mainClear;
const document = {
  head: {appendChild(e) { style.set(e.id,e); }},
  createElement() { return element(); },
  getElementById(id) { return style.get(id) || null; },
  addEventListener(n, fn) { (docHandlers[n] ||= []).push(fn); },
  querySelector(selector) { return selector === '.pt-main-layer-clear-btn' ? mainClear : null; },
  querySelectorAll(selector) {
    if (selector.includes('input')) return [...state.values()].filter(x => !selector.includes(':checked') || x.checked);
    if (selector === '.leaflet-control-layers-overlays label') return [...state].map(([name,input]) => ({textContent:name,getAttribute(){return name;},querySelector(){return input;}}));
    return [];
  }
};
const context = vm.createContext({console,document,window:{},setTimeout(fn) {timers.push(fn);return timers.length;},
  L: {
    point:(x,y)=>({x,y}), divIcon:o=>o,
    marker(coords,options) { return {coords,options,bindTooltip(html){this.tooltip=html;return this;},bindPopup(html){this.popup=html;return this;}}; },
    markerClusterGroup:()=>group('markers'), layerGroup:()=>group('labels'),
    control() { return {addTo(m){this.container=this.onAdd(m);controls.push(this);return this;},getContainer(){return this.container;}}; },
    DomUtil:{create:()=>element()},DomEvent:{disableClickPropagation(){},disableScrollPropagation(){}}
  }
});
const install = new vm.Script(`(${match[1]})`).runInContext(context);
const names = {mojave:'GW sites | 2025 Mojave limited field inventory',noc:'GW wells | BLM NOC inventory'};
const legendCode=`p<-parse(commandArgs(TRUE)[1]);d<-Filter(function(x)is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name('pt_add_blm_gw_well_inventory_browser_layer')),as.list(p));stopifnot(length(d)==1L);b<-as.list(d[[1]][[3]][[3]])[-1];a<-Filter(function(x)is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name('legend_title')),b);stopifnot(length(a)==1L);v<-lapply(c('noc','mojave_2025'),function(k){e<-new.env(parent=baseenv());e$layer_kind<-k;eval(a[[1]],e);e$legend_title});names(v)<-c('noc','mojave');cat(jsonlite::toJSON(v,auto_unbox=TRUE))`;
const legendResult=spawnSync('Rscript',['--vanilla','-e',legendCode,path.join(root,'03_functions/leaflet_layer_local_well_spring_helpers.r')],{encoding:'utf8'});
assert.strictEqual(legendResult.status,0,legendResult.stderr);const titles=JSON.parse(legendResult.stdout);
assert.deepStrictEqual(titles,{noc:'BLM NOC well inventory',mojave:'Mojave limited field inventory (2025)'});
for (const name of Object.values(names)) {register(name);register(`Labels: ${name}`);}
function project(records) {
  // Actual R browser projection and JSON serializer, using synthetic records only.
  const code = `e<-new.env(parent=globalenv()); for(x in parse(commandArgs(TRUE)[1])) if(is.call(x) && identical(x[[1]],as.name('<-')) && identical(x[[2]],as.name('pt_prepare_blm_gw_well_inventory_records'))) eval(x,e); d<-jsonlite::fromJSON(file('stdin')); d$longitude<-d$pt_lng; d$latitude<-d$pt_lat; cat(jsonlite::toJSON(e$pt_prepare_blm_gw_well_inventory_records(d),dataframe='rows',na='null',auto_unbox=TRUE))`;
  const result=spawnSync('Rscript',['--vanilla','-e',code,path.join(root,'03_functions/leaflet_layer_local_well_spring_helpers.r')],{input:JSON.stringify(records),encoding:'utf8'});
  assert.strictEqual(result.status,0,result.stderr || String(result.error));
  return JSON.parse(result.stdout);
}
const records = project(['present','not_found','unknown','spring_present'].map((status,i)=>({record_uid:`s${i}`,pt_lat:[34,35,32.8,36][i],pt_lng:[-115,-116,-114.6,-117.2][i],
  source_key:'mojave_2025_blm_field_check',water_level_recorded:i===0 || i===2,lab_sample_documented:i===1 || i===2,
  well_name_display:i===0?null:`Site & ${i}`,hover_line1:i===0?'Site 901':`Site & ${i}`,hover_line2:i===0?'Depth to water: production':null,
  well_present_key:status,popup_html:'<b>escaped &lt;source&gt;</b>',on_blm_ca:i===0,dist_to_blm_mi:i,
  blm_distance_label:i===0?'on BLM':`${i} mi`})));
const nocRecord = project([{...records[0],source_key:'noc_blm_drilled',record_uid:'noc_1',pt_lat:50,pt_lng:-125,well_name_display:'NOC name',hover_line1:'NOC name',hover_line2:'2020-10-03',well_completion_date:'2020-10-03'}])[0];
install.call(map,null,null,{groupName:names.mojave,labelGroupName:`Labels: ${names.mojave}`,layerKind:'mojave_2025',legendTitle:titles.mojave,records});
install.call(map,null,null,{groupName:names.noc,labelGroupName:`Labels: ${names.noc}`,layerKind:'noc',legendTitle:titles.noc,records:[nocRecord]});
function flush() { while(timers.length) timers.shift()(); }
flush();
const mojave = context.window.BRIM_MOJAVE_2025_WELLS_LOCAL;
const noc = context.window.BRIM_BLM_NOC_WELLS_LOCAL;
const legend = controls[0].container;
const nocLegend = controls[1].container;
const markers = () => [...layers].filter(l=>l.type==='markers').flatMap(g=>g.members);
function statusButton(value) {return legend.querySelectorAll('.pt-blm-gw-filter-btn').find(b=>b.getAttribute('data-kind')==='status'&&b.getAttribute('data-value')===value);}
function filterButton(kind,value) {return legend.querySelectorAll('.pt-blm-gw-filter-btn').find(b=>b.getAttribute('data-kind')===kind&&b.getAttribute('data-value')===value);}
function observation(value) {return filterButton('observation',value);}
function turnOn(name) {
  if(!state.get(name).checked) {
    const count=fits.length;state.get(name).click();
    assert.equal(fits.length,count+(name===names.mojave?1:0));
  }
}
const expectedFit={bounds:[[32.8,-117.2],[36,-114.6]],options:{padding:[38,38],animate:false}};

test('actual controller starts off without markers; emits syntactically valid JavaScript',()=>{assert.equal(layers.size,0);assert.equal(mojave.stats().active,false);assert.equal(fits.length,0);});
test('four statuses have distinct icons, legend counts, site total and status control',()=>{
  turnOn(names.mojave);assert.equal(mojave.stats().drawn,4);
  assert(legend.innerHTML.includes(titles.mojave));assert(!names.mojave.includes('('));
  assert.deepStrictEqual(fits,[expectedFit]);
  map.fire('overlayadd',{name:names.mojave});map.fire('overlayadd',{name:`Points – ${names.mojave} (138)`});flush();
  assert.equal(fits.length,1);assert.equal(new Set(markers().map(m=>m.options.icon.html)).size,4);
  assert(legend.innerHTML.includes('4 / 4 sites.'));assert(legend.innerHTML.includes('Spring / spring box present'));
  assert(statusButton('spring_present'));assert(markers().some(m=>m.options.icon.html.includes('pt-mojave-spring-present')));
});
test('blank names remain unlabeled; neutral hover and qualifiers are escaped',()=>{
  turnOn(`Labels: ${names.mojave}`);
  const labels=[...layers].filter(l=>l.type==='labels').flatMap(g=>g.members);
  assert.equal(labels.length,3);assert(!labels.some(m=>m.options.icon.html.includes('901')));
  assert(markers().some(m=>m.tooltip==='Site 901<br/>Depth to water: production'));
  assert(markers().some(m=>m.tooltip==='Site &amp; 1'));
  assert(markers().every(m=>m.popup.includes('escaped &lt;source&gt;')));
});
test('spring status filters and reset operate through actual DOM handlers',()=>{
  statusButton('spring_present').click();assert.equal(mojave.stats().drawn,1);
  assert(markers()[0].options.icon.html.includes('pt-mojave-spring-present'));
  legend.querySelector('.pt-blm-gw-reset-btn').click();assert.equal(mojave.stats().drawn,4);
  const input=legend.querySelector('.pt-blm-gw-blm-manual');input.value='1';input.keydown('Enter');assert.equal(mojave.stats().drawn,2);
  legend.querySelector('.pt-blm-gw-reset-btn').click();
});
test('actual R serialization retains false booleans and fails on missing or string observation fields',()=>{
  assert.strictEqual(records[0].lab_sample_documented,false);assert.strictEqual(records[2].water_level_recorded,true);
  assert(!('water_level_recorded' in nocRecord));assert(!('lab_sample_documented' in nocRecord));
  for (const value of [undefined,null,'False',0]) {
    const bad={...records[0],lab_sample_documented:value};
    assert.throws(()=>project([bad]));
    assert.throws(()=>install.call(map,null,null,{layerKind:'mojave_2025',records:[bad]}),/boolean observation fields/);
  }
});
test('four observation DOM controls use the same filtered marker, cluster, label and showing counts',()=>{
  for (const [mode,count,labelCount] of [['water_level',2,1],['lab_sample',2,2],['both',1,1],['all',4,3]]) {
    observation(mode).click();assert.strictEqual(mojave.stats().filters.observation,mode);
    assert.equal(mojave.stats().drawn,count);assert.equal(markers().length,count);
    assert.equal([...layers].filter(g=>g.type==='markers').length,1);
    assert.equal([...layers].filter(g=>g.type==='labels').flatMap(g=>g.members).length,labelCount);
    assert(legend.innerHTML.includes(`${count} / 4 sites.`));
    const buttons=legend.querySelectorAll('.pt-blm-gw-filter-btn').filter(b=>b.getAttribute('data-kind')==='observation');
    assert.equal(buttons.length,4);assert.equal(buttons.filter(b=>b.getAttribute('aria-pressed')==='true').length,1);
    assert.equal(observation(mode).getAttribute('aria-pressed'),'true');
  }
  assert(legend.innerHTML.includes('numeric reading in the approved monitoring table'));
  assert(legend.innerHTML.includes('documented laboratory sample collection'));
});
test('observations intersect status and BLM; All clears only observations; Reset clears all established filters',()=>{
  observation('both').click();statusButton('unknown').click();assert.equal(mojave.stats().drawn,1);
  filterButton('blm','on').click();assert.equal(mojave.stats().drawn,0);
  observation('all').click();assert.equal(mojave.stats().drawn,0);
  assert.equal(mojave.stats().filters.status,'unknown');assert.equal(mojave.stats().filters.blm,'on');
  filterButton('blm','any').click();assert.equal(mojave.stats().drawn,1);
  legend.querySelector('.pt-blm-gw-reset-btn').click();assert.equal(mojave.stats().drawn,4);
  assert.equal(mojave.stats().filters.observation,'all');assert.equal(mojave.stats().filters.status,'all');
});
test('observation buttons have native keyboard activation, selected semantics and scoped wrapping',()=>{
  observation('lab_sample').keydown('Enter');assert.equal(mojave.stats().drawn,2);
  observation('both').keydown(' ');assert.equal(mojave.stats().drawn,1);
  assert.equal(observation('both').getAttribute('type'),'button');
  assert.equal(observation('both').getAttribute('aria-pressed'),'true');
  assert(legend.innerHTML.includes('role="group" aria-label="Observations"'));
  assert(style.get('pt-blm-gw-local-style').textContent.includes('.pt-blm-gw-observations{display:flex;flex-wrap:wrap;'));
  legend.querySelector('.pt-blm-gw-reset-btn').click();
});
test('NOC retains well total, installation filter, symbol and independent lifecycle',()=>{
  assert.equal(fits.length,1); // All preceding filters, labels and resets were non-navigating.
  turnOn(names.noc);assert.equal(fits.length,1);assert(nocLegend.innerHTML.includes('1 / 1 well record(s).'));assert(nocLegend.innerHTML.includes('Install WY:'));
  assert(nocLegend.innerHTML.includes('NOC well record'));assert(nocLegend.innerHTML.includes('BLM NOC well inventory'));
  assert(!nocLegend.innerHTML.includes('BLM-drilled'));
  assert(!nocLegend.innerHTML.includes('Spring / spring box'));assert(markers().some(m=>m.options.icon.html.includes('pt-blm-noc-well-dot')));
  assert(!nocLegend.innerHTML.includes('Observations:'));assert(!('observation' in noc.stats().filters));
  observation('both').click();assert.equal(noc.stats().drawn,1);
  state.get(names.mojave).click();assert.equal(noc.stats().drawn,1);assert.equal(markers().length,1);
  turnOn(names.mojave);assert.equal(mojave.stats().drawn,4);
});
test('zoom and repeated overlay removal are idempotent without listener growth',()=>{
  const listeners=Object.values(events).reduce((n,a)=>n+a.length,0);
  map.zoom=8;map.fire('zoomend');assert(![...layers].some(g=>g.type==='labels'));
  map.zoom=12;map.fire('zoomend');
  for(let i=0;i<3;i++){observation('both').click();state.get(names.mojave).click();map.fire('overlayremove',{name:names.mojave});turnOn(names.mojave);assert.equal(mojave.stats().drawn,4);assert.equal(mojave.stats().filters.observation,'all');}
  assert.equal(Object.values(events).reduce((n,a)=>n+a.length,0),listeners);assert.equal(markers().length,5);
});
// Execute the existing Clear Local callback itself, not a copied implementation.
const core=fs.readFileSync(path.join(root,'03_functions/leaflet_core_helpers.r'),'utf8');
const clearMatch=core.match(/clearBtn\.addEventListener\('click', (function\(e\) \{[\s\S]*?\n        \})\);/);
assert(clearMatch);
context.root=document;
const clearLocal=new vm.Script(`(${clearMatch[1]})`).runInContext(context);
mainClear={click(){clearLocal({preventDefault(){},stopPropagation(){}});}};
test('actual Clear Local callback clears both controllers and labels',()=>{
  observation('both').click();
  const fitCount=fits.length;mainClear.click();flush();assert.equal(fits.length,fitCount);assert.equal(layers.size,0);assert(!mojave.stats().active);assert(!noc.stats().active);
  mainClear.click();assert.equal(layers.size,0);assert.equal(mojave.stats().filters.status,'all');assert.equal(mojave.stats().filters.observation,'all');
});
const tools=fs.readFileSync(path.join(root,'03_functions/js/leaflet_tools_adddata_panel.js'),'utf8');
const all=tools.slice(tools.indexOf('  function ptClearAllPt2SessionLayers()'),tools.indexOf('  function ptToggleCustomLayer(',tools.indexOf('  function ptClearAllPt2SessionLayers()')));
assert(all.startsWith('  function ptClearAllPt2SessionLayers()'));
for(const name of ['ptSetTeachingLabelPlacementActive','ptSetTeachingMarkupMode','ptClearMeasurements','ptClearCustomLayers','ptSetStatus'])context[name]=()=>{};
new vm.Script(all).runInContext(context);
test('actual Clear All handler dispatches to Clear Local without changing map extent',()=>{
  turnOn(names.mojave);turnOn(names.noc);turnOn(`Labels: ${names.mojave}`);
  observation('lab_sample').click();
  const fitCount=fits.length;context.ptClearAllPt2SessionLayers();flush();assert.equal(fits.length,fitCount);assert.equal(layers.size,0);assert.equal(map.zoom,12);
  assert.equal(mojave.stats().filters.observation,'all');
  context.ptClearAllPt2SessionLayers();assert.equal(layers.size,0);
});
test('re-enable after clear restores exact totals and blank label suppression',()=>{
  turnOn(names.mojave);assert.equal(mojave.stats().drawn,4);assert.equal(mojave.stats().labels,false);
  turnOn(`Labels: ${names.mojave}`);assert.equal([...layers].filter(l=>l.type==='labels')[0].members.length,3);
  mainClear.click();assert.equal(layers.size,0);
});
// Deliver the actual full configuration through the frozen R header projection.
// This includes the runtime min_zoom override assignments, not selected declarations.
function configuredPairs(directory) {
  const rCode=`e<-new.env(parent=baseenv());sys.source(commandArgs(TRUE)[1],e); x<-readLines(commandArgs(TRUE)[2],warn=FALSE); start<-which(startsWith(x,'  inline_label_pairs <- if')); stopifnot(length(start)==1L); end<-which(seq_along(x)>start & startsWith(x,'  htmlwidgets::onRender('))[1]; expr<-parse(text=x[start:(end-1L)]);stopifnot(length(expr)==1L);eval(expr,e);cat(jsonlite::toJSON(e$inline_label_pairs,auto_unbox=TRUE,null='null'))`;
  const r=spawnSync('Rscript',['--vanilla','-e',rCode,path.join(directory,'00_config/config_labels.r'),path.join(root,'03_functions/leaflet_core_helpers.r')],{encoding:'utf8'});
  assert.strictEqual(r.status,0,r.stderr || String(r.error));return JSON.parse(r.stdout);
}
const pairs=configuredPairs(root);
const pair=pairs.find(p=>p.main==='GW wells | BLM NOC inventory');
const beforeDir=process.env.BRIM_WELL_QA_BEFORE_DIR;
const oldNames={[names.noc]:'GW wells | NOC',[names.mojave]:'GW wells | 2025 Mojave-BLM limited field check'};
const oldPairs=beforeDir?configuredPairs(beforeDir):pairs.map(p=>oldNames[p.main]?{...p,main:oldNames[p.main],label:oldNames[p.label]}:p);
const oldPair=oldPairs.find(p=>p.main==='GW wells | NOC');
const matcherStart=core.indexOf('    function normLayerName(txt) {');
const matcherEnd=core.indexOf('    var labels = Array.prototype.slice.call(',matcherStart);
assert(matcherStart>=0 && matcherEnd>matcherStart);
new vm.Script(core.slice(matcherStart,matcherEnd)).runInContext(context);
function layerRow(fullName,input) {
  const row=element({'data-pt-layer-full-name':fullName,'data-pt-layer-short-name':fullName.replace(/^(Points|Labels) – /,'')});
  row.textContent=fullName;row.children=[input || element({type:'checkbox'})];return row;
}
const mainRow=layerRow('Points – GW wells | BLM NOC inventory (287)',state.get(names.noc));
const labelRow=layerRow('Labels – GW wells | BLM NOC inventory',state.get(`Labels: ${names.noc}`));
const wrongMain=layerRow(`Points – ${names.mojave} (138)`,state.get(names.mojave));
const wrongLabel=layerRow(`Labels – ${names.mojave}`,state.get(`Labels: ${names.mojave}`));
const rows=[wrongMain,wrongLabel,labelRow,mainRow];
test('full final configuration: NOC minZoom 9, Albion 10 and unchanged unrelated pairs',()=>{
  assert.deepStrictEqual(pair,{main:'GW wells | BLM NOC inventory',label:'GW wells | BLM NOC inventory',minZoom:9});
  assert.equal(pairs.find(p=>p.main===names.mojave).minZoom,10);
  assert.deepStrictEqual(pairs.filter(p=>!Object.values(names).includes(p.main)),oldPairs.filter(p=>!Object.values(oldNames).includes(p.main)));
  if(beforeDir) {
    const before=fs.readFileSync(path.join(beforeDir,'00_config/config_labels.r'),'utf8');
    const after=fs.readFileSync(path.join(root,'00_config/config_labels.r'),'utf8');
    assert.equal(after.split('GW wells | BLM NOC inventory').length-1,3);
    assert.equal(after.split(names.mojave).length-1,3);
    assert.equal(after.replaceAll(names.noc,oldNames[names.noc]).replaceAll(names.mojave,oldNames[names.mojave]),before);
  }
});
test('actual generic matcher: old/old works, old/new fails, corrected counted Points and Labels pair uniquely',()=>{
  const oldRows=[layerRow('Points – GW wells | NOC (287)'),layerRow('Labels – GW wells | NOC')];
  assert.strictEqual(context.findLayerRow(oldRows,oldPair.main,false),oldRows[0]);
  assert.strictEqual(context.findLayerRow(oldRows,oldPair.label,true),oldRows[1]);
  assert.strictEqual(context.findLayerRow(rows,oldPair.main,false),null);
  assert.strictEqual(context.findLayerRow(rows,oldPair.label,true),null);
  assert.strictEqual(context.findLayerRow(rows,pair.main,false),mainRow);
  assert.strictEqual(context.findLayerRow(rows,pair.label,true),labelRow);
  assert.strictEqual(context.findLayerRow(rows,names.mojave,false),wrongMain);
  assert.strictEqual(context.findLayerRow(rows,names.mojave,true),wrongLabel);
  context.data={inlineLabelPairs:pairs};
  context.installInlineLabelToggles(document,rows);flush();
  context.installInlineLabelToggles(document,rows);flush();
  for(const row of [mainRow,wrongMain])assert.equal(row.querySelectorAll('.pt-inline-lbl-toggle').length,1);
  for(const row of [labelRow,wrongLabel]) {
    assert(row.classList.contains('pt-label-companion-hidden'));
    assert.equal(row.getAttribute('data-pt-inline-companion-label'),'true');
    assert.equal(row.querySelectorAll('.pt-inline-lbl-toggle').length,0);
  }
  assert(mainRow.querySelector('.pt-inline-lbl-toggle').title.includes('(z9+)'));
  assert(wrongMain.querySelector('.pt-inline-lbl-toggle').title.includes('(z10+)'));
  const oldAlbionRows=[layerRow(`Points – ${oldNames[names.mojave]} (138)`),layerRow(`Labels – ${oldNames[names.mojave]}`)];
  assert.strictEqual(context.findLayerRow(oldAlbionRows,oldNames[names.mojave],false),oldAlbionRows[0]);
  assert.strictEqual(context.findLayerRow(rows,oldNames[names.mojave],false),null);
});
test('actual inline binding enables once and preserves NOC zoom, year/BLM filtering, Clear and re-enable',()=>{
  assert.equal(fits.length,7);assert(fits.every(f=>JSON.stringify(f)===JSON.stringify(expectedFit)));
  const start=fits.length;
  const inline=mainRow.querySelector('.pt-inline-lbl-toggle').querySelector('input');
  assert(inline.disabled);turnOn(names.noc);assert(!inline.disabled);
  inline.checked=true;inline.dispatch('change');assert(state.get(`Labels: ${names.noc}`).checked);
  map.zoom=8;map.fire('zoomend');assert(![...layers].some(g=>g.type==='labels'));
  map.zoom=9;map.fire('zoomend');assert.equal([...layers].filter(g=>g.type==='labels').flatMap(g=>g.members).length,1);
  const from=nocLegend.querySelector('.pt-blm-gw-wy-from');
  from.value='2022';from.keydown('Enter');assert.equal(noc.stats().drawn,0);
  nocLegend.querySelector('.pt-blm-gw-reset-btn').click();assert.equal(noc.stats().drawn,1);
  const off=nocLegend.querySelectorAll('.pt-blm-gw-filter-btn').find(b=>b.getAttribute('data-kind')==='blm'&&b.getAttribute('data-value')==='off');
  off.click();assert.equal(noc.stats().drawn,0);nocLegend.querySelector('.pt-blm-gw-reset-btn').click();
  mainClear.click();flush();assert(inline.disabled);assert(!inline.checked);assert.equal(layers.size,0);
  turnOn(names.noc);inline.checked=true;inline.dispatch('change');assert.equal(noc.stats().drawn,1);
  assert.equal(mainRow.querySelectorAll('.pt-inline-lbl-toggle').length,1);
  assert.equal(fits.length,start);mainClear.click();map.zoom=12;
});
test('actual Albion activation ignores empty/invalid bounds and never includes another layer or scaffold',()=>{
  const start=fits.length;
  for(const [i,invalid] of [[],[{...records[0],pt_lat:91}],[{...records[0],pt_lng:Infinity}],[{...records[0],pt_lat:null}]].entries()) {
    const name=`Empty or invalid fixture ${i}`;const input=register(name);
    install.call(map,null,null,{groupName:name,labelGroupName:`Labels: ${name}`,layerKind:'mojave_2025',records:invalid});
    flush();input.click();map.fire('overlayadd',{name});input.click();flush();
    assert.equal(fits.length,start);
  }
});
// Exercise the real shared shell against the actual well-card HTML. The prior
// harness omitted shell enhancement, so it could not detect duplicate titles.
function shellNode(tag='div', attrs={}) {
  const listeners={},node={tagName:tag.toUpperCase(),attrs:{...attrs},children:[],parentNode:null,text:'',
    style:{cssText:'',setProperty(k,v){this[k]=String(v);},getPropertyValue(k){return this[k]||'';}},
    get className(){return this.attrs.class||'';},set className(v){this.attrs.class=v;},
    get parentElement(){return this.parentNode;},get childNodes(){return this.children;},
    get firstChild(){return this.children[0]||null;},
    get nextSibling(){return this.parentNode?.children[this.parentNode.children.indexOf(this)+1]||null;},
    get previousSibling(){return this.parentNode?.children[this.parentNode.children.indexOf(this)-1]||null;},
    get textContent(){return this.text+this.children.map(c=>c.textContent).join('');},set textContent(v){this.text=String(v);this.children=[];},
    set innerHTML(v){this.text=String(v);this.children=[];},
    getAttribute(k){return this.attrs[k]||null;},setAttribute(k,v){this.attrs[k]=String(v);},
    appendChild(c){return this.insertBefore(c,null);},
    insertBefore(c,b){if(c===b)return c;if(c.parentNode)c.parentNode.removeChild(c);const i=b?this.children.indexOf(b):this.children.length;assert(i>=0);this.children.splice(i,0,c);c.parentNode=this;return c;},
    removeChild(c){const i=this.children.indexOf(c);assert(i>=0);this.children.splice(i,1);c.parentNode=null;return c;},
    contains(c){return c===this||this.children.some(x=>x.contains(c));},
    matches(s){return s.startsWith('.')?s.slice(1).split('.').every(c=>this.classList.contains(c)):s===tag;},
    closest(s){return this.matches(s)?this:this.parentNode?.closest(s)||null;},
    querySelectorAll(s){return this.children.flatMap(c=>[...(c.matches(s)?[c]:[]),...c.querySelectorAll(s)]);},
    querySelector(s){return this.querySelectorAll(s)[0]||null;},
    addEventListener(k,fn){(listeners[k]||=[]).push(fn);},
    removeEventListener(k,fn){listeners[k]=(listeners[k]||[]).filter(x=>x!==fn);},
    click(){(listeners.click||[]).slice().forEach(fn=>fn({preventDefault(){},stopPropagation(){},target:this}));},
    getBoundingClientRect(){return {left:20,top:20,right:278,bottom:350,width:258,height:330};}
  };
  node.classList={contains(c){return node.className.split(/\s+/).includes(c);},add(c){if(!this.contains(c))node.className=(node.className+' '+c).trim();},remove(c){node.className=node.className.split(/\s+/).filter(x=>x!==c).join(' ');}};
  return node;
}
function shellCard(html,className) {
  const card=shellNode('div',{class:className}),stack=[card];
  // Parse the emitted small HTML fragment, preserving nested header/actions.
  for(const token of html.matchAll(/<[^>]+>|[^<]+/g)) {
    const t=token[0];if(t.startsWith('</')){stack.pop();continue;}
    if(t.startsWith('<')) {
      const m=t.match(/^<(\w+)/);assert(m);const attrs={};for(const a of t.matchAll(/([\w-]+)="([^"]*)"/g))attrs[a[1]]=a[2];
      const n=shellNode(m[1],attrs);stack.at(-1).appendChild(n);if(!['input','br','hr'].includes(m[1]))stack.push(n);
    } else stack.at(-1).text+=t;
  }
  assert.equal(stack.length,1);return card;
}
const shellSource=fs.readFileSync(path.join(root,'03_functions/js/brim_legend_closeout_helpers.js'),'utf8');
const shellFunctions=shellSource.slice(shellSource.indexOf('  function autoHeader('),shellSource.indexOf('  function enhanceAllLegendCards('));
const definitions=shellSource.slice(shellSource.indexOf('  var legendCards = ['),shellSource.indexOf('\n  ];',shellSource.indexOf('  var legendCards = ['))+5);
assert(shellFunctions.includes('function enhanceLegendCard('));
function shellHarness(cards) {
  const container=shellNode(),corner=shellNode('div',{class:'leaflet-bottom'});container.appendChild(corner);cards.forEach(c=>corner.appendChild(c));
  const shared={window:{BRIM:{legendCloseout:{}}},document:{createElement:shellNode,addEventListener(){},removeEventListener(){}},mapContainer:container,map:{getContainer:()=>container,on(){},off(){}},scheduleResponsiveLegendOverflow(){}};
  const ctx=vm.createContext(shared);
  new vm.Script(shellSource.slice(shellSource.indexOf('  window.BRIM.legendCloseout.wire ='),shellSource.indexOf('  var map = this;'))+definitions+shellFunctions).runInContext(ctx);
  return {ctx,container,corner,enhance(){for(const def of ctx.legendCards)ctx.enhanceLegendCard(def);}};
}
for(const [family,html,title] of [['NOC',nocLegend.innerHTML,titles.noc],['Albion',legend.innerHTML,titles.mojave]]) {
  test(`${family} actual shared shell has one dataset title, pin/close and stable repeated enhancement`,()=>{
    const card=shellCard(html,'pt-blm-gw-legend'),h=shellHarness([card]);
    const bodyBefore=card.children.filter(c=>!c.matches('.pt-map-card-auto-header'));
    let hidden=0;h.ctx.window.BRIM.legendCloseout.wire(card,'.pt-blm-gw-legend-close',()=>{hidden++;card.style.display='none';},{hide:false});
    h.enhance();h.enhance();
    const header=card.querySelector('.pt-map-card-auto-header'),dock=card.querySelector('.pt-map-card-dock'),close=card.querySelector('.pt-blm-gw-legend-close');
    assert.equal(card.querySelectorAll('.pt-map-card-auto-title').length,1);assert.equal(card.querySelector('.pt-map-card-auto-title').textContent,title);
    assert.equal(card.querySelectorAll('.pt-blm-gw-legend-title').length,0);assert(!card.textContent.includes('BLM groundwater-well legend'));
    assert.equal(card.querySelectorAll('.pt-map-card-auto-header').length,1);assert(header.contains(dock)&&header.contains(close));
    assert.equal(card.querySelectorAll('.pt-map-card-dock').length,1);assert.equal(card.querySelectorAll('.pt-blm-gw-legend-close').length,1);
    assert.strictEqual(dock.nextSibling,close);assert.deepStrictEqual(card.children.filter(c=>c!==header),bodyBefore);
    const state=card.__brimDetachableState;dock.click();assert(state.floating);assert.strictEqual(card.parentNode,h.container);
    dock.click();assert(!state.floating);assert.strictEqual(card.parentNode,h.corner);
    close.click();assert.equal(hidden,1);assert.equal(card.style.display,'none');
    // Model the existing owner's innerHTML replacement, then actual re-enhancement.
    const redraw=shellCard(html,'pt-blm-gw-legend');card.children.slice().forEach(c=>card.removeChild(c));redraw.children.slice().forEach(c=>card.appendChild(c));
    h.enhance();h.enhance();assert.equal(card.querySelectorAll('.pt-map-card-auto-title').length,1);assert.equal(card.querySelector('.pt-map-card-auto-title').textContent,title);
    assert.equal(card.querySelectorAll('.pt-map-card-dock').length,1);assert.equal(card.querySelectorAll('.pt-blm-gw-legend-close').length,1);
  });
}
test('shared shell control preserves unrelated default title and local fix has no global heading effect',()=>{
  const well=shellCard(legend.innerHTML,'pt-blm-gw-legend');
  const unrelated=shellCard('<button type="button" class="pt-springs-local-close">x</button><div>Unchanged spring controls</div>','pt-springs-local-legend');
  const h=shellHarness([well,unrelated]);h.enhance();h.enhance();
  assert.equal(unrelated.querySelector('.pt-map-card-auto-title').textContent,'Springs legend');
  assert.equal(unrelated.querySelectorAll('.pt-map-card-auto-title').length,1);assert(unrelated.textContent.includes('Unchanged spring controls'));
  assert(unrelated.querySelector('.pt-map-card-dock'));assert(unrelated.querySelector('.pt-springs-local-close'));
  assert.equal(well.querySelector('.pt-map-card-auto-title').textContent,titles.mojave);
});

console.log(`RESULT passed=${passed} failed=0; mounted_browser=NOT_RUN; network_calls=0`);
