'use strict';
// Explicit local inputs only. Actual source adapters + existing offline DOM doubles;
// no server, browser, provider request, fixture rewrite or generated cache.
const assert=require('node:assert/strict'),fs=require('node:fs'),path=require('node:path'),crypto=require('node:crypto');
const T=require('../03_functions/js/soil_moisture_transport.js'),C=require('../03_functions/js/soil_moisture_charts.js');
const {environment,until}=require('./test_soil_popup_presentation.js');
const root=path.resolve(__dirname,'..'),pkgRoot=path.resolve(process.argv[2]||''),saved=path.resolve(process.argv[3]||'');
assert(process.argv[2]&&process.argv[3],'Usage: node qa/test_snotel_ca_static_candidate.js <verified-package> <saved-SM1-fixture> [canonical-root]');
const sha=b=>crypto.createHash('sha256').update(b).digest('hex'),json=p=>JSON.parse(fs.readFileSync(p));
const checks=[],cases=[];let packageBytes=0;
function pinned(d,parse=true){const b=fs.readFileSync(path.join(pkgRoot,d.path));assert.equal(b.length,d.bytes,d.path);assert.equal(sha(b),d.sha256,d.path);return parse?JSON.parse(b):b;}
const pkg=pinned(T.snotelStatic.package),allowed=new Map(pkg.files.map(d=>[d.path,d]));
allowed.set(T.snotelStatic.package.path,T.snotelStatic.package);
for(const d of allowed.values()){pinned(d,false);packageBytes+=d.bytes;}
assert.equal(allowed.size,24);assert.equal(packageBytes,10265680);
const fixture=pinned(allowed.get('package/REVIEW_FIXTURE.json'));
const schema=pinned(allowed.get('package/CANDIDATE_SCHEMA.json'));
assert.deepEqual(schema.history_columns,T.snotelStatic.columns);
checks.push('24 immutable package files; exact wire size/hash; frozen schema');
const reads=[];
const reader=new T.SnotelStaticReader(async(d,signal)=>{signal?.throwIfAborted();reads.push(d.path);return pinned(d);});
const copy=x=>JSON.parse(JSON.stringify(x));
const counts={boundaries:0,numeric:0,gaps:0,zeros:0,revision_boundaries:0,non_V_numeric:0};

async function main(){
 const context=await reader.load();assert.equal(context.stations.stations.length,1);
 assert(!reads.some(p=>p.includes('/history/')||p.includes('/provenance/')));
 const st=context.stations.stations[0],models=[];
 for(const sensor of st.sensors){
  const original=pinned(allowed.get('package/sample/'+sensor.history.path)),before=JSON.stringify(original);
  const decoded=await reader.history(st.stationTriplet,sensor.sensor_identity),model=C.snotelStatic(decoded);models.push(model);
  assert.equal(JSON.stringify(original),before);assert.deepEqual(decoded.raw,original);
  assert.equal(decoded.raw.sensor_identity,`${st.stationTriplet}|SMS:${sensor.signed_depth}:${sensor.ordinal}`);
  assert.equal(model.context,decoded);assert.equal(model.history.length,7700);assert.equal(model.history,model.seasonal);
  for(let i=0;i<model.history.length;i++){
   const row=model.history[i],source=Object.fromEntries(original.columns.map((k,j)=>[k,original.rows[i][j]]));
   assert.deepEqual(row.raw,source);assert.equal(row.date,source.provider_date);assert.equal(row.v,source.display_value);
   assert.equal(row.ok,source.display_status==='PLOT_NUMERIC');
   assert.equal(row.wy,+source.provider_date.slice(0,4)+(source.provider_date.slice(5,7)>='10'?1:0));
   assert.equal(row.raw.source_timestamp_utc,new Date(Date.parse(source.provider_date)+32*3600000).toISOString().replace('.000Z','Z'));
   counts.boundaries++;counts.numeric+=row.ok;counts.gaps+=source.display_status==='DISPLAY_GAP';
   counts.zeros+=row.ok&&row.v===0;counts.revision_boundaries+=source.revision_count>1;
   counts.non_V_numeric+=row.ok&&source.qc_flag.value!=='V';
   assert.deepEqual(Object.keys(row).sort(),['date','v','wy','dowy','x','raw','ok'].sort());
  }
  assert(C.segments(model.history).length>1);assert.equal(model.reference.length,0);
  assert(C.readout(model.history.find(C.good),model).includes('Source timestamp'));
  assert(C.readout(model.history.find(C.good),model).includes('Instantaneous DAILY/END'));
  assert(model.sourceNote.includes('not a daily mean'));
 }
 assert.deepEqual(counts,{boundaries:23100,numeric:23070,gaps:30,zeros:4287,revision_boundaries:17,non_V_numeric:258});
 checks.push('all 23100 Blue Lakes records: exact identity/value/provider date/END timestamp/WY/raw metadata; unchanged seven-field chart rows');

 // history_point is the real frozen candidate record, not constructed from the
 // expected class/value. Assertions additionally compare native source records.
 for(const test of fixture.cases){
  const input=copy(test.history_point),before=JSON.stringify(input);
  const admitted=T.admitSnotelStaticRecord(input,test.archive_ids);
  const model=C.snotelStatic({contract:T.snotelStatic.id,records:[admitted],identity:test.sensor_identity});
  const actual=model.history[0],expected=test.history_point;
  assert.equal(JSON.stringify(input),before);assert.deepEqual(actual.raw,expected);
  assert.equal(actual.v,expected.display_value);assert.equal(actual.ok,expected.display_status==='PLOT_NUMERIC');
  assert.equal(actual.date,expected.provider_date);assert.equal(model.context.identity,test.sensor_identity);
  assert.equal(actual.raw.source_timestamp_utc,new Date(Date.parse(actual.date)+32*3600000).toISOString().replace('.000Z','Z'));
  assert.equal(actual.raw.revision_count,test.source_observations.length);
  if(actual.ok)for(const {observation:o} of test.source_observations){
   assert.equal(actual.v,o.value_native);assert.equal(actual.v,o.provider_record.value);
   assert.equal(o.sensor_identity,test.sensor_identity);assert.equal(o.provider_date,actual.date);
   assert(['V','E','K','N'].includes(o.qc_flag));
  }
  else {assert.equal(actual.v,null);assert.equal(C.segments([actual]).length,0);}
  cases.push({case:test.case,sensor:test.sensor_identity,date:actual.date,value:actual.v,status:actual.raw.display_status,result:'PASS'});
 }
 assert.equal(cases.length,14);
 const hold=cases.find(c=>c.case==='SUSPECT_NO_NUMERIC_INSUFFICIENT_EVIDENCE');
 assert.equal(hold.status,'UNRESOLVED_HOLD');assert.equal(hold.sensor,'518:CA:SNTL|SMS:-2:1');
 const zero=cases.find(c=>c.case==='ELIGIBLE_ZERO');assert.equal(zero.value,0);assert.equal(zero.status,'PLOT_NUMERIC');
 checks.push('all 14 real fixture cases through admission/model; source-native comparisons; explicit hold versus gap; QA does not veto');

 const revisionCase=fixture.cases.find(c=>c.case==='FLAG_ONLY_REVISION_SAFE_COLLAPSE');
 const history=await reader.history('356:CA:SNTL',revisionCase.sensor_identity);
 const record=history.records.find(r=>r.provider_date===revisionCase.history_point.provider_date);
 const evidence=await reader.evidence(history,record);
 assert.deepEqual(evidence.observations,revisionCase.source_observations.map(r=>r.observation));
 assert.equal(evidence.queries.length,2);assert.equal(record.revision_count,2);
 assert.deepEqual(record.qa_flag,{representations:[{present:true,value:'P'},{present:true,value:'R'}]});
 assert(evidence.observations.some(o=>o.qa_flag==='P')&&evidence.observations.some(o=>o.qa_flag==='R'));
 // Resolve every sample boundary's references, not just the revision example.
 let resolved=0;
 for(const model of models)for(const r of model.context.records){
  const e=await reader.evidence(model.context,r);assert.equal(e.observations.length,r.revision_count);
  for(const o of e.observations){assert.equal(o.sensor_identity,model.context.raw.sensor_identity);assert.equal(o.provider_date,r.provider_date);if(r.display_status==='PLOT_NUMERIC')assert.equal(o.value_native,r.display_value);resolved++;}
  assert.equal(e.queries.length,r.source_query_refs.length);
 }
 assert.equal(resolved,23087);
 checks.push('all 23087 referenced sample observations reconstructed losslessly by field dictionaries; revision variants/query lineage retained; no latest-wins');

 const raw=history.raw;
 for(const mutate of [b=>b.candidate_id='other',b=>b.sensor_identity='356:CA:SNTL|SMS:-8:2',b=>b.ordinal=2,
  b=>b.unit_native='fraction',b=>b.period_ref='BEGIN',b=>b.columns.reverse(),b=>b.rows[0].pop(),
  b=>b.rows[0][1]='2005-08-22T08:00:00Z',b=>b.rows[0][5]=null,b=>b.rows[0][7]={present:true,value:'S'},
  b=>b.rows[0][10]=[[999,0]],b=>b.rows.reverse(),b=>b.rows[0][4]='UNKNOWN']){
  const b=copy(raw);mutate(b);assert.throws(()=>T.decodeSnotelStatic(b,raw.sensor_identity));
 }
 // Synthetic guard probes are separate from the 14 producer cases.
 const unbounded=copy(fixture.cases.find(c=>c.case==='QC_V_PUBLIC_PRESENT').history_point);
 unbounded.display_value=101;unbounded.archive_current.value=101;
 const accepted=T.admitSnotelStaticRecord(unbounded,fixture.cases.find(c=>c.case==='QC_V_PUBLIC_PRESENT').archive_ids);
 assert.equal(C.snotelStatic({contract:T.snotelStatic.id,records:[accepted]}).history[0].v,101);
 assert.throws(()=>T.descriptor({path:'history/a.json',bytes:1,sha256:'a'.repeat(64)}));
 for(const p of ['../a.json','https://example.invalid/a.json','a/../../b.json','/a.json','a%2fb.json'])assert.throws(()=>T.staticDescriptor({path:p,bytes:1,sha256:'a'.repeat(64)}));
 const beforeRead=reads.length;await assert.rejects(reader.history('356:CA:SNTL','356:CA:SNTL|SMS:-8:2'));assert.equal(reads.length,beforeRead);
 checks.push('malformed identity/unit/clock/shape/status/reference/order rejection; no magnitude veto; unchanged pilot descriptor guard');

 // Compare the existing pilot output exactly with its preexisting predicate.
 const pilot=json(path.join(saved,'snotel-356.json'));assert.equal(T.decode(pilot,'snotel','356:CA:SNTL'),pilot);
 for(const b of pilot.data.data){const m=C.snotel(b);assert.deepEqual(m.history,b.values.map(r=>C.row(r.date,r.value,r,r.qcFlag==='V'&&Number.isFinite(r.value)&&r.value>=0&&r.value<=100)));}
 assert.equal(C.wy('2023-09-30'),2023);assert.equal(C.wy('2023-10-01'),2024);
 checks.push('SM1 pilot decode/predicate retained; provider date, not next-day UTC date, determines WY');

 const source=fs.readFileSync(path.join(root,'03_functions/js/soil_moisture_controller.js'),'utf8');
 function component(hook){
  const env=environment(saved),requests=[];
  env.context.fetch=async(url,options)=>{
   const u=new URL(url);assert.equal(u.origin,'http://127.0.0.1');assert(u.pathname.startsWith('/package/'));
   const rel=u.pathname.slice('/package/'.length);assert(allowed.has(rel),'Only compact package member reads');requests.push(rel);
   assert.equal(options.redirect,'error');
   const replacement=hook?await hook(rel,options):undefined;
   return new Response(replacement??fs.readFileSync(path.join(pkgRoot,rel)));
  };
  env.run(source);const api=env.run('createSoilMoistureController()');
  const layer=api.registerSnotelStatic('SNOTEL static component','http://127.0.0.1/package/PACKAGE_MANIFEST.json');
  return {env,api,layer,requests};
 }
 const a=component();a.layer.addTo(a.env.map);await until(()=>a.api.sources.snotel.index);
 assert(!a.requests.some(p=>p.includes('/history/')));assert.equal(a.api.sources.snotel.index.stations[0].primary_sensor,null);
 await a.api.select('snotel:356:CA:SNTL');assert(!a.requests.some(p=>p.includes('/history/')));
 let box=a.env.map.popup.content;assert(box.textContent.includes('Choose an exact sensor'));assert(!box.querySelector('[data-csv]'));
 box.querySelector('[data-sensor-choice="SMS:-8:1"]').click();await until(()=>box.querySelector('[data-static-chart] svg'));
 assert(box.textContent.includes('instantaneous source boundary'));assert(!box.textContent.includes('SNOTEL pilot'));
 assert.equal(a.requests.filter(p=>p.includes('/history/')).length,1);
 assert.equal(a.requests.filter(p=>p.includes('/observations/')).length,0);
 box.querySelector('[data-sensor-choice="SMS:-20:1"]').click();await until(()=>a.requests.filter(p=>p.includes('/history/')).length===2&&box.querySelector('svg'));
 box.querySelector('[data-sensor-choice="SMS:-8:1"]').click();await until(()=>box.querySelector('svg'));assert.equal(a.requests.filter(p=>p.includes('/history/')).length,2);
 assert.throws(()=>a.api.register('snotel','pilot','/snotel.json'),/already registered/);
 a.env.map.closePopup();assert.equal(a.env.map.popup,null);a.layer.forceRemove(a.env.map);a.layer.forceRemove(a.env.map);
 assert.equal(a.api.stats().card,false);assert.equal(a.api.stats().markers,0);
 a.layer.addTo(a.env.map);await until(()=>a.api.sources.snotel.status.startsWith('1 stations'));a.api.reset();a.api.reset();a.env.map.emit('unload',{});
 assert.equal(a.api.stats().card,false);assert.equal(a.api.stats().sources.snotel.enabled,false);
 checks.push('actual controller: catalog-only activation; explicit -8/-20 selection; lazy per-sensor read/cache; no provenance UI/CSV expansion; normal close/off/reset/reactivation');

 for(const close of ['off','popupclose','reset','new-selection']){
  let release,held=false,aborted=false;
  const b=component(async(rel,options)=>{if(rel.endsWith('_SMS_-8_1.json')){held=true;options.signal.addEventListener('abort',()=>aborted=true);await new Promise(r=>release=r);}});
  b.layer.addTo(b.env.map);await until(()=>b.api.sources.snotel.index);await b.api.select('snotel:356:CA:SNTL');
  const old=b.env.map.popup.content;old.querySelector('[data-sensor-choice="SMS:-8:1"]').click();await until(()=>held);
  if(close==='off')b.layer.forceRemove(b.env.map);else if(close==='popupclose')b.env.map.closePopup();else if(close==='reset')b.api.reset();
  else {old.querySelector('[data-sensor-choice="SMS:-2:1"]').click();await until(()=>old.querySelector('svg'));}
  release();await new Promise(r=>setTimeout(r,20));assert(aborted);
  if(close!=='new-selection'){assert(!old.querySelector('svg'));assert.equal(b.api.stats().historyCache,0);}
  else assert.equal(old.querySelector('[data-sensor-choice="SMS:-2:1"]').getAttribute('aria-pressed'),'true');
  b.api.reset();b.env.map.emit('unload',{});
 }
 for(const failure of ['hash','size']){
  const b=component(async rel=>{if(rel==='PACKAGE_MANIFEST.json'){
   const bytes=fs.readFileSync(path.join(pkgRoot,rel));if(failure==='hash'){bytes[10]^=1;return bytes;}return bytes.subarray(0,bytes.length-1);
  }});
  b.layer.addTo(b.env.map);await until(()=>b.api.sources.snotel.status.startsWith('unavailable:'));
  assert.equal(b.api.sources.snotel.index,null);assert.equal(b.api.sources.snotel.staticReader.context,null);
  assert.equal(b.requests.length,1);b.api.reset();b.env.map.emit('unload',{});
 }
 const bad=component();assert.throws(()=>bad.api.registerSnotelStatic('bad','https://example.invalid/PACKAGE_MANIFEST.json'),/Local SNOTEL/);
 const c=environment(saved);c.run(source);const ca=c.run('createSoilMoistureController()');ca.register('snotel','pilot','/snotel.json');
 assert.throws(()=>ca.registerSnotelStatic('duplicate','http://127.0.0.1/package/PACKAGE_MANIFEST.json'),/already registered/);ca.reset();c.map.emit('unload',{});
 checks.push('late completion cannot revive cleared/closed/reset/reselected popup or cache; hash/byte corruption, redirects, hosted URL and duplicate Product registration guarded');
 const canonical=process.argv[4]?await canonicalChecks(path.resolve(process.argv[4])):null;
 console.log(JSON.stringify({result:'PASS',scope:'offline source/component only',package_files:allowed.size,package_bytes:packageBytes,blue_lakes:counts,real_fixture_cases_total:cases.length,real_fixture_cases_pass:cases.length,real_fixture_cases_fail:0,cases,checks,source_observations_resolved:resolved,canonical,builds:0,browsers:0,provider_requests:0},null,2));
}

async function canonicalChecks(canonicalRoot){
 const rootDescriptor={path:'MANIFEST.json',bytes:19511,sha256:'653d674b2bd1f008ddb4ff66f741962607cd9bd1194669d5f4c573026893df03'};
 const verified=new Map(),readPaths=[];
 function disk(d){
  T.staticDescriptor(d);const bytes=fs.readFileSync(path.join(canonicalRoot,d.path));
  assert.equal(bytes.length,d.bytes,d.path);assert.equal(sha(bytes),d.sha256,d.path);
  verified.set(d.path,d);return JSON.parse(bytes);
 }
 const manifest=disk(rootDescriptor),descriptors=new Map(manifest.files.map(d=>[d.path,d]));
 descriptors.set(rootDescriptor.path,rootDescriptor);
 const read=async(d,signal)=>{signal?.throwIfAborted();readPaths.push(d.path);return disk(d);};
 const full=new T.SnotelStaticReader(read,new T.BundleCache(),'canonical');
 const context=await full.load();assert.equal(context.files.size,134);
 assert.deepEqual(readPaths,['MANIFEST.json','SCHEMA.json','DISPLAY_POLICY.json','STATIONS.json']);
 const identities=context.stations.stations.flatMap(s=>s.sensors.map(x=>x.sensor_identity));
 assert.equal(context.stations.stations.length,32);assert.equal(identities.length,98);assert.equal(new Set(identities).size,98);
 assert.equal(Math.max(...manifest.files.map(d=>d.bytes)),1676087);
 assert.equal(Math.max(...context.stations.stations.flatMap(s=>s.sensors.map(x=>x.counts.boundaries))),8799);
 assert.deepEqual(T.limits,{index:262144,bundle:6000000,expanded_scan:12000000,cache_compact_bytes:12000000,cache_entries:8,table_cells:1000000,table_rows:50000,table_columns:64});
 const history=await full.history('463:CA:SNTL','463:CA:SNTL|SMS:-8:1');
 assert.equal(history.records.length,8799);assert.equal(history.records.length*history.raw.columns.length,105588);
 assert.deepEqual(readPaths.slice(4),['history/463_CA_SNTL_SMS_-8_1.json','provenance/463_CA_SNTL.json']);
 assert.equal(C.snotelStatic(history).history.length,8799);
 for(const r of history.records){
  for(const [slot,index] of r.source_query_refs)assert(history.context.provenance.archives[history.raw.archive_ids[slot]].query_ledger[index]);
  assert.equal(r.source_timestamp_utc,new Date(Date.parse(r.provider_date)+32*3600000).toISOString().replace('.000Z','Z'));
 }
 const n=readPaths.length;await full.history('463:CA:SNTL','463:CA:SNTL|SMS:-8:1');assert.equal(readPaths.length,n);
 await assert.rejects(full.history('463:CA:SNTL','463:CA:SNTL|SMS:-8:2'),/Unknown exact/);assert.equal(readPaths.length,n);
 await assert.rejects(full.evidence(history,history.records[0]),/original observation recovery unavailable/);assert.equal(readPaths.length,n);
 const canonicalFixture=disk(descriptors.get('provenance/REVIEW_FIXTURE.json'));
 assert.deepEqual(canonicalFixture.cases,fixture.cases);
 // The same frozen real producer cases cover V/E/K/N, zero, QC-S, gaps, hold,
 // revisions and exact raw pointers; no second science predicate is introduced.
 for(const test of canonicalFixture.cases){
  const admitted=T.admitSnotelStaticRecord(copy(test.history_point),test.archive_ids),row=C.snotelStatic({contract:T.snotelStatic.id,records:[admitted]}).history[0];
  assert.deepEqual(row.raw,test.history_point);assert.equal(row.v,test.history_point.display_value);
  assert.equal(row.ok,test.history_point.display_status==='PLOT_NUMERIC');
 }
 const negatives=[];
 async function malformed(label,file,mutate){
  const reader=new T.SnotelStaticReader(async(d,signal)=>{signal?.throwIfAborted();const b=disk(d);if(d.path===file)mutate(b);return b;},new T.BundleCache(),'canonical');
  await assert.rejects(reader.load());assert.equal(reader.context,null);negatives.push(label);
 }
 // Deliberately synthetic parsed-object probes isolate structural validation;
 // actual wire hash/byte corruption is tested through the controller below.
 for(const [label,file,mutate] of [
  ['candidate identity','MANIFEST.json',b=>b.candidate_id='other'],
  ['contract','MANIFEST.json',b=>b.source_contract='other'],
  ['adapter','MANIFEST.json',b=>b.source_adapter_version='other'],
  ['not local','MANIFEST.json',b=>b.candidate_status='LIVE'],
  ['descriptor count','MANIFEST.json',b=>b.files.pop()],
  ['duplicate path','MANIFEST.json',b=>b.files[1]=b.files[0]],
  ['unsafe traversal','MANIFEST.json',b=>b.files[0].path='../x.json'],
  ['absolute member','MANIFEST.json',b=>b.files[0].path='/x.json'],
  ['encoded member','MANIFEST.json',b=>b.files[0].path='x%2fy.json'],
  ['invalid bytes','MANIFEST.json',b=>b.files[0].bytes=6000001],
  ['invalid sha','MANIFEST.json',b=>b.files[0].sha256='bad'],
  ['missing schema descriptor','MANIFEST.json',b=>b.files.find(x=>x.path==='SCHEMA.json').path='absent.json'],
  ['schema hash mismatch','MANIFEST.json',b=>b.schema_sha256='a'.repeat(64)],
  ['policy hash mismatch','MANIFEST.json',b=>b.display_policy_sha256='a'.repeat(64)],
  ['schema column','SCHEMA.json',b=>b.history_columns.reverse()],
  ['schema identity','SCHEMA.json',b=>b.candidate_id='other'],
  ['schema enum','SCHEMA.json',b=>b.display_status_enum.pop()],
  ['policy identity','DISPLAY_POLICY.json',b=>b.candidate_id='other'],
  ['policy science','DISPLAY_POLICY.json',b=>b.interpolation=true],
  ['catalog identity','STATIONS.json',b=>b.candidate_id='other'],
  ['station count','STATIONS.json',b=>b.stations.pop()],
  ['duplicate station','STATIONS.json',b=>b.stations[1]=b.stations[0]],
  ['duplicate sensor','STATIONS.json',b=>b.stations[0].sensors[1]=b.stations[0].sensors[0]],
  ['sensor identity','STATIONS.json',b=>b.stations[0].sensors[0].ordinal=2],
  ['history missing','STATIONS.json',b=>b.stations[0].sensors[0].history.path='history/absent.json'],
  ['history hash','STATIONS.json',b=>b.stations[0].sensors[0].history.sha256='a'.repeat(64)],
  ['history bytes','STATIONS.json',b=>b.stations[0].sensors[0].history.bytes++],
  ['provenance missing','STATIONS.json',b=>b.stations[0].native_metadata_provenance.path='provenance/absent.json']
 ])await malformed(label,file,mutate);
 const source=fs.readFileSync(path.join(root,'03_functions/js/soil_moisture_controller.js'),'utf8');
 function component(hook){
  const env=environment(saved),requests=[];
  env.context.fetch=async(url,options)=>{
   const u=new URL(url);assert.equal(u.origin,'http://127.0.0.1');assert(u.pathname.startsWith('/canonical/'));
   const rel=u.pathname.slice('/canonical/'.length),d=descriptors.get(rel);assert(d);disk(d);
   assert.equal(options.redirect,'error');requests.push(rel);
   const replacement=hook?await hook(rel,options):undefined;
   return replacement instanceof Response?replacement:new Response(replacement??fs.readFileSync(path.join(canonicalRoot,rel)));
  };
  env.run(source);
  const layer=env.run("makeSoilMoistureLayer('snotel','Soil moisture | SNOTEL | pilot','http://127.0.0.1/canonical/MANIFEST.json','canonical')");
  return{env,layer,api:env.window.BRIM.soilMoisture,requests};
 }
 const a=component();a.layer.addTo(a.env.map);await until(()=>a.api.sources.snotel.index);
 assert.deepEqual(a.requests,['MANIFEST.json','SCHEMA.json','DISPLAY_POLICY.json','STATIONS.json']);
 assert.equal(a.api.stats().results.length,32);assert.equal(a.api.stats().markers,32); // Offline model, not rendered-marker acceptance.
 for(const st of a.api.sources.snotel.index.stations){assert.equal(st.primary_sensor,null);for(const s of st.sensors){assert.equal(s.observation,null);assert.equal(s.capabilities.latest_vwc,false);}}
 a.api.edit({min:0},true);assert.equal(a.api.stats().results.length,0);a.api.edit({min:null},true);
 a.api.edit({query:'463:CA:SNTL'},true);assert.deepEqual([...a.api.stats().results],['snotel:463:CA:SNTL']);
 await a.api.select('snotel:463:CA:SNTL');assert.equal(a.requests.length,4);
 let box=a.env.map.popup.content;box.querySelector('[data-sensor-choice="SMS:-8:1"]').click();await until(()=>box.querySelector('[data-static-chart] svg'));
 assert.deepEqual(a.requests.slice(4),['history/463_CA_SNTL_SMS_-8_1.json','provenance/463_CA_SNTL.json']);
 box.querySelector('[data-sensor-choice="SMS:-8:1"]').click();await until(()=>box.querySelector('svg'));assert.equal(a.requests.length,6);
 assert.throws(()=>a.api.register('snotel','pilot','/snotel.json'),/already registered/);
 assert.throws(()=>a.api.registerSnotelStatic('compact','http://127.0.0.1/package/PACKAGE_MANIFEST.json'),/already registered/);
 // Synthetic independent ordinal and null-coordinate catalog specimens. Only
 // the in-memory projection changes; frozen bytes and hash expectations do not.
 const st=a.api.sources.snotel.index.stations.find(s=>s.id==='463:CA:SNTL'),extra=copy(st.sensors.find(s=>s.id==='SMS:-8:1'));
 extra.id='SMS:-8:2';extra.raw.ordinal=2;extra.raw.sensor_identity='463:CA:SNTL|SMS:-8:2';st.sensors.push(extra);a.api.redraw();
 const exactChoices=[];a.api.sources.snotel.staticReader.history=async(station,sensor)=>{exactChoices.push([station,sensor]);throw Error('Synthetic exact-choice probe only');};
 await a.api.select(st.key);box=a.env.map.popup.content;
 for(const id of ['SMS:-8:1','SMS:-8:2']){box.querySelector(`[data-sensor-choice="${id}"]`).click();await until(()=>box.textContent.includes('Synthetic exact-choice'));}
 assert.deepEqual(exactChoices,[['463:CA:SNTL','463:CA:SNTL|SMS:-8:1'],['463:CA:SNTL','463:CA:SNTL|SMS:-8:2']]);assert.equal(a.requests.length,6);
 st.coordinates=null;a.api.redraw();assert.equal(a.api.stats().results.length,1);assert.equal(a.api.stats().markers,0);
 await a.api.select(st.key);assert(a.env.document.querySelector('[data-message]').textContent.includes('public coordinates unavailable'));assert.equal(a.requests.length,6);
 a.layer.forceRemove(a.env.map);a.layer.forceRemove(a.env.map);a.api.reset();a.env.map.emit('unload',{});
 for(const mode of ['pilot','compact']){
  const e=environment(saved);e.run(source);const api=e.run('createSoilMoistureController()');
  if(mode==='pilot')api.register('snotel','pilot','/snotel.json');else api.registerSnotelStatic('compact','http://127.0.0.1/package/PACKAGE_MANIFEST.json');
  assert.throws(()=>api.registerSnotelStatic('full','http://127.0.0.1/canonical/MANIFEST.json','canonical'),/already registered/);api.reset();
 }
 const lifecycle=[];
 for(const close of ['off','popupclose','reset','new-selection','unload']){
  let release,held=false,aborted=false;
  const b=component(async(rel,options)=>{if(rel==='history/463_CA_SNTL_SMS_-8_1.json'){held=true;options.signal.addEventListener('abort',()=>aborted=true);await new Promise(r=>release=r);}});
  b.layer.addTo(b.env.map);await until(()=>b.api.sources.snotel.index);await b.api.select('snotel:463:CA:SNTL');
  const old=b.env.map.popup.content;old.querySelector('[data-sensor-choice="SMS:-8:1"]').click();await until(()=>held);
  if(close==='off')b.layer.forceRemove(b.env.map);else if(close==='popupclose')b.env.map.closePopup();else if(close==='reset')b.api.reset();else if(close==='unload')b.env.map.emit('unload',{});
  else{old.querySelector('[data-sensor-choice="SMS:-2:1"]').click();await until(()=>old.querySelector('svg'));}
  release();await new Promise(r=>setTimeout(r,20));assert(aborted);
  if(close!=='new-selection'){assert(!old.querySelector('svg'));assert.equal(b.api.stats().historyCache,0);}else assert.equal(old.querySelector('[data-sensor-choice="SMS:-2:1"]').getAttribute('aria-pressed'),'true');
  b.api.reset();b.layer.addTo(b.env.map);await until(()=>b.api.sources.snotel.status.startsWith('32 stations'));b.api.reset();b.api.reset();b.env.map.emit('unload',{});assert.equal(b.api.stats().card,false);assert.equal(b.api.stats().markers,0);lifecycle.push(close);
 }
 for(const failure of ['hash','size','missing','redirect']){
  const b=component(async rel=>{if(rel==='history/463_CA_SNTL_SMS_-8_1.json'){
   if(failure==='missing')return new Response('',{status:404});if(failure==='redirect')throw new TypeError('Redirect refused');
   const bytes=fs.readFileSync(path.join(canonicalRoot,rel));if(failure==='hash'){bytes[10]^=1;return bytes;}return bytes.subarray(0,bytes.length-1);
  }});
  b.layer.addTo(b.env.map);await until(()=>b.api.sources.snotel.index);await b.api.select('snotel:463:CA:SNTL');const box=b.env.map.popup.content;
  box.querySelector('[data-sensor-choice="SMS:-8:1"]').click();await until(()=>box.textContent.includes('Selected history unavailable'));
  assert.equal(b.requests.length,5);assert.equal(b.api.stats().historyCache,0);b.api.reset();b.env.map.emit('unload',{});negatives.push('selected wire '+failure);
 }
 const e=environment(saved);e.run(source);const api=e.run('createSoilMoistureController()');
 for(const url of ['https://example.invalid/MANIFEST.json','http://127.1/MANIFEST.json','http://user@localhost/MANIFEST.json','http://localhost/a/../MANIFEST.json','http://localhost/a%2fb/MANIFEST.json','http://localhost/MANIFEST.json?q=1','http://localhost/MANIFEST.json#x','http://localhost/PACKAGE_MANIFEST.json','http://localhost:99999/MANIFEST.json'])assert.throws(()=>api.registerSnotelStatic('bad',url,'canonical'));
 for(const host of ['localhost','127.0.0.1','[::1]']){const x=environment(saved);x.run(source);const c=x.run('createSoilMoistureController()');c.registerSnotelStatic('local',`http://${host}:8080/canonical/MANIFEST.json`,'canonical');c.reset();}
 let actualRRegistration='NOT_SUPPLIED';
 if(process.env.BRIM_SNOTEL_WRAPPER_RESULT){
  const captured=json(process.env.BRIM_SNOTEL_WRAPPER_RESULT);assert.equal(captured.result,'PASS');
  for(const mode of ['full','pilot']){
   const hook=captured[mode],x=environment(saved),rows=[];
   assert(hook.code.includes(source.trim()),'R wrapper embeds the current actual controller');
   x.run(source);const start=hook.code.indexOf('  if (soilMoistureSnotelStaticManifestUrl &&');
   const end=hook.code.indexOf('  if (includeSnowPillowLatest &&',start);assert(start>=0&&end>start);
   Object.assign(x.context,{soilMoistureShared:hook.data.soilMoistureShared,
    soilMoistureSnotelPilot:hook.data.soilMoistureSnotelPilot,
    soilMoistureIndexes:hook.data.soilMoistureIndexes,
    soilMoistureSnotelStaticManifestUrl:hook.data.soilMoistureSnotelStaticManifestUrl,
    addOpsLayer:row=>rows.push(row)});
   x.run(hook.code.slice(start,end));assert.equal(rows.length,1);
   assert.equal(rows[0].name,'Soil moisture | SNOTEL | pilot');
   const api=x.window.BRIM.soilMoisture;
   assert.equal(api.sources.snotel.staticReader?.mode??null,mode==='full'?'canonical':null);
   assert.equal(x.requests.length,0);api.reset();x.map.emit('unload',{});
  }
  actualRRegistration='PASS';
 }
 return{result:'PASS',descriptors:134,stations:32,exact_sensors:98,identities,activation_metadata_requests:4,activation_history_requests:0,activation_provenance_requests:0,selected_cache_miss_requests:2,selected_cache_hit_requests:0,max_history_rows:8799,limits_unchanged:true,real_fixture_cases:canonicalFixture.cases.length,raw_observation_recovery:'EXPLICIT_UNAVAILABLE',query_provenance:'PASS',synthetic_cases:['same-depth independent ordinals','null public coordinates',...negatives],lifecycle,actual_r_registration:actualRRegistration,verified_inputs:[...verified.values()],mounted_acceptance:'NOT_RUN'};
}
main().catch(e=>{console.error(e.stack);process.exitCode=1;});
