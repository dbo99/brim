#!/usr/bin/env node
'use strict';
// Current compiler + actual Explorer/facet renderer, with no browser or network.
const assert = require('assert');
const fs = require('fs');
const path = require('path');
const {execFileSync} = require('child_process');
const root = path.resolve(__dirname, '..');
const {createModel, extractFunction} = require('./test_guide_resource_explorer.js');
const source = fs.readFileSync(path.join(root, '03_functions/js/leaflet_brim_guide.js'), 'utf8');
const registry = JSON.parse(fs.readFileSync(path.join(root, '00_config/guide_resources.json')));
// Reuse the established foundation's source-only runtime group setup, then call
// the production compiler. An optional measured build bundle is checked as well.
const foundation = fs.readFileSync(path.join(__dirname, 'test_guide_foundation.R'), 'utf8');
const setup = foundation.slice(foundation.indexOf('suppressPackageStartupMessages(library(dplyr))'),
  foundation.indexOf('initial_map <-'));
assert(setup.includes('runtime_groups <-') && !setup.includes('build_final_map_only'));
const output = execFileSync(process.env.RSCRIPT || '/usr/local/bin/Rscript', ['--vanilla', '-e',
  setup + '\nif (nzchar(Sys.getenv("BRIM_INFORMATION_RUNTIME_GROUPS"))) runtime_groups <- as.character(unlist(jsonlite::fromJSON(Sys.getenv("BRIM_INFORMATION_RUNTIME_GROUPS"))$runtime_groups, use.names=FALSE))' +
  '\ncat("\\nBRIM_INFORMATION_BUNDLE\\n"); cat(jsonlite::toJSON(pt_build_guide_bundle(runtime_groups, MAP_DISPLAY, "default"), auto_unbox=TRUE, null="null", na="null"))'],
  {cwd: root, encoding: 'utf8', maxBuffer: 8 * 1024 * 1024, env: {...process.env, GIT_OPTIONAL_LOCKS: '0'}});
const bundle = JSON.parse(output.split('\nBRIM_INFORMATION_BUNDLE\n')[1]);
const resources = bundle.resources;
const list = x => x == null ? [] : Array.isArray(x) ? x : [x];
const ids = rs => rs.map(r => r.id).sort();
const vocabulary = ['External On-Demand Service','Forecast / Outlook','Historical Context',
  'Live Observation','Model / Simulation','Screening / Derived','Static Reference','Tool / Workflow'];
const publicRecords = registry.resources.filter(r => r.publication_state === 'published');
const staged = registry.resources.filter(r => r.publication_state === 'staged');
function validate(records, field) {
  assert.equal(new Set(records.map(r => r.id)).size, records.length, 'Duplicate Resource ID');
  records.forEach(r => {
    const tags = list(r[field]);
    assert.equal(new Set(tags).size, tags.length, 'Duplicate Information Type');
    assert(tags.every(t => vocabulary.includes(t)), 'Unknown Information Type');
  });
}
validate(publicRecords, 'information_type_tags'); validate(resources, 'informationTypeTags');
assert.throws(() => validate([resources[0], resources[0]], 'informationTypeTags'), /Duplicate Resource ID/);
assert.throws(() => validate([{id:'fixture', informationTypeTags:['Live Observation','Live Observation']}], 'informationTypeTags'), /Duplicate Information Type/);
assert.throws(() => validate([{id:'fixture', informationTypeTags:['Instantaneous']}], 'informationTypeTags'), /Unknown Information Type/);
assert.equal(resources.length, 228); assert.equal(staged.length, 5);
assert.deepStrictEqual(ids(resources), ids(publicRecords));
assert(staged.every(r => !resources.some(p => p.id === r.id)));
for (const r of resources) assert.deepStrictEqual(list(r.informationTypeTags),
  publicRecords.find(p => p.id === r.id).information_type_tags, r.id);
if (process.env.BRIM_INFORMATION_TEST_BUNDLE) {
  const measured = JSON.parse(fs.readFileSync(process.env.BRIM_INFORMATION_TEST_BUNDLE));
  assert.deepStrictEqual(measured.resources, resources, 'Measured normal compiler Resources differ');
}
const examples = {
  resource_pivotal_weather_pivotal_weather_model_maps_platform: ['Forecast / Outlook','Model / Simulation'],
  resource_pivotal_weather_pivotal_weather_soundings_product: ['Forecast / Outlook','Model / Simulation','Tool / Workflow'],
  resource_noaa_cnrfc: ['Forecast / Outlook','Model / Simulation','Historical Context'],
  resource_noaa_wpc_qpf: ['Forecast / Outlook'],
  resource_noaa_wpc_excessive_rainfall_outlook: ['Forecast / Outlook','Screening / Derived'],
  resource_noaa_nws_graphical_forecasts: ['Forecast / Outlook','Tool / Workflow'],
  resource_google_deepmind_weather_lab: ['Forecast / Outlook','Model / Simulation','Tool / Workflow'],
  resource_dwr_cdec: ['Live Observation','Forecast / Outlook','Historical Context','Screening / Derived'],
  resource_nasa_grace_groundwater_soil_moisture: ['Forecast / Outlook','Model / Simulation','Historical Context','Screening / Derived'],
  resource_noaa_cpc_soil_moisture: ['Forecast / Outlook','Model / Simulation','Historical Context','Screening / Derived'],
  resource_usgs_water_data_nation: ['Live Observation','Historical Context','Tool / Workflow','External On-Demand Service'],
  resource_nasa_smap_l3_enhanced_soil_moisture: ['Historical Context','Screening / Derived']
};
for (const [id,tags] of Object.entries(examples)) {
  const r = resources.find(r => r.id === id); assert(r, id);
  tags.forEach(t => assert(list(r.informationTypeTags).includes(t), `${id}: ${t}`));
}
for (const id of ['resource_doi','resource_blm_california']) {
  const r=resources.find(r=>r.id===id); assert(r,id); assert.deepStrictEqual(list(r.informationTypeTags),[]);
}
for (const id of ['resource_noaa_cpc_soil_moisture','resource_nasa_grace_groundwater_soil_moisture','resource_nasa_smap_l3_enhanced_soil_moisture','resource_usgs_quickdri','resource_usgs_vegdri']) {
  assert(!list(resources.find(r=>r.id===id).informationTypeTags).includes('Live Observation'),id);
}
let audit;
if (process.env.BRIM_INFORMATION_TYPE_AUDIT) {
  audit=JSON.parse(fs.readFileSync(process.env.BRIM_INFORMATION_TYPE_AUDIT));
  assert.deepStrictEqual(ids(audit.rows),ids(resources));
  const before=JSON.parse(fs.readFileSync(process.env.BRIM_INFORMATION_R2_REGISTRY));
  assert.equal(before.resources.length,registry.resources.length);
  for(let i=0;i<registry.resources.length;i++) {
    const a=before.resources[i],b=registry.resources[i]; assert.equal(a.id,b.id);
    const reviewed=audit.rows.find(r=>r.id===b.id);
    if(reviewed) {
      assert.deepStrictEqual(reviewed.old_tags,a.information_type_tags);
      assert.deepStrictEqual(reviewed.final_tags,b.information_type_tags);
      assert.deepStrictEqual({...a,information_type_tags:b.information_type_tags},b,'Non-tag field changed: '+b.id);
    } else assert.deepStrictEqual(a,b,'Staged record changed');
  }
  const delta=JSON.parse(fs.readFileSync(process.env.BRIM_INFORMATION_FIELD_DELTA));
  assert.deepStrictEqual(delta.changes,audit.rows.filter(r=>JSON.stringify(r.old_tags)!==JSON.stringify(r.final_tags)).map(r=>({id:r.id,field:'information_type_tags',before:r.old_tags,after:r.final_tags})));
}
const model=createModel(resources), original=JSON.stringify(resources);
assert.deepStrictEqual(model.facetCounts(model.createState()).presets,{in_brim_map:28,beyond_the_map:200,all_resources:228});
const dimensions=[{}, {providers:['family:blm']},{providers:['family:noaa']},
  {providers:['family:private']},{providers:['family:other']},{providers:['family:noaa','family:private']},
  {query:'forecast'},{query:'GRACE'},{query:'soil moisture'},{query:'zzzzzzzzzzzz'},
  {subject:'Weather & Forecasts'},{resourceType:'viewer_or_explorer'},
  {productContextId:'ops_cdec_reservoir_storage'},
  {providers:['family:noaa'],subject:'Weather & Forecasts',query:'forecast'}];
let cases=0;
for(const preset of ['all_resources','in_brim_map','beyond_the_map']) for(const options of dimensions) {
  const base=model.createState({...options,preset,informationType:''});
  const eligible=model.results(base), counts=model.facetCounts(base).informationTypes;
  for(const tag of vocabulary) {
    const expected=ids(eligible.filter(r=>list(r.informationTypeTags).includes(tag)));
    assert.equal(counts[tag]||0,expected.length,JSON.stringify({options,preset,tag}));
    const selected=model.patchState(base,{informationType:tag});
    assert.deepStrictEqual(ids(model.results(selected)),expected);
    assert.deepStrictEqual(model.facetCounts(selected).informationTypes,counts,'Own facet selection affected counts');
    assert.deepStrictEqual(ids(model.results(model.showMore(selected))),expected,'Pagination capped result membership');
    assert.deepStrictEqual(ids(model.results(model.patchState(selected,{informationType:''}))),ids(eligible));
    assert.equal(model.results(model.reset(selected)).length,228);
    cases++;
  }
}
// Counts are unique Resource sets even where source JSON is a scalar and tags overlap.
const forecast=model.results(model.createState({informationType:'Forecast / Outlook'}));
assert(forecast.length>model.pageSize);
assert(forecast.some(r=>list(r.informationTypeTags).includes('Model / Simulation')));
const scalar=resources.find(r=>typeof r.informationTypeTags==='string'); assert(scalar);
assert(model.results(model.createState({informationType:scalar.informationTypeTags})).some(r=>r.id===scalar.id));
if(process.env.BRIM_INFORMATION_BADGE_SETS) {
  const saved=JSON.parse(fs.readFileSync(process.env.BRIM_INFORMATION_BADGE_SETS));
  for(const tag of vocabulary) assert.deepStrictEqual(ids(model.results(model.createState({informationType:tag}))),saved.sets[tag].after_ids);
}
// Run the actual facet DOM builder and actual click branch using a tiny DOM stub.
function node(tag, className='', text='') {
  return {tag, className, textContent:String(text), children:[], attributes:{},
    classList:{toggle(){}},appendChild(child){this.children.push(child);return child;},
    setAttribute(k,v){this.attributes[k]=String(v);},getAttribute(k){return this.attributes[k]??null;},
    closest(){return this;}};
}
function button(cls,text,action,label){const n=node('button',cls,text);n.setAttribute('data-guide-action',action);if(label)n.setAttribute('aria-label',label);return n;}
const renderer=new Function('node','button','resourceExplorerModel','resourceFocusKey',
  ['sortedCountKeys','resourceCountText','appendResourceFacetLabel','resourceFacetChoices'].map(n=>extractFunction(source,n)).join('\n')+'\nreturn resourceFacetChoices;')
  (node,button,model,(n,k)=>n.setAttribute('data-guide-focus-key',k));
const ui={resourceExplorer:model.createState()};let renders=0;
const click=new Function('root','state','resourceExplorerModel','render','focusResourceTarget',
  'return ('+extractFunction(source,'handleRootClick')+')')({contains:()=>true},ui,model,()=>renders++,()=>{});
const facet=renderer('Information Type','informationType','',model.facetCounts(ui.resourceExplorer).informationTypes);
const choices=facet.children[1].children;
assert.equal(choices.length,8);
for(const choice of choices){
  const tag=choice.getAttribute('data-resource-value');
  const count=model.facetCounts(ui.resourceExplorer).informationTypes[tag];
  assert(choice.getAttribute('aria-label').includes(String(count)));
  assert(choice.children.some(n=>n.textContent===String(count)),'Visible badge mismatch');
  click({target:choice});assert.equal(ui.resourceExplorer.informationType,tag);
  assert.equal(model.results(ui.resourceExplorer).length,count);
  click({target:choice});assert.equal(ui.resourceExplorer.informationType,'');
}
click({target:choices[0]});click({target:choices[1]});
assert.equal(ui.resourceExplorer.informationType,choices[1].getAttribute('data-resource-value'));
assert(renders>=18); assert.equal(JSON.stringify(resources),original,'Runtime mutated type authority');
console.log(JSON.stringify({status:'PASS',publicResources:228,stagedExcluded:5,facetCases:cases,counts:model.facetCounts(model.createState()).informationTypes,auditBound:Boolean(audit),browser:'UNAVAILABLE_POLICY — source/model/DOM only'}));
