/* Offline application adapter gate. Oracle is read only after actual histories.
 * No sockets, producer invocation, generated fixture, or expected/ record input. */
'use strict';
const fs=require('node:fs'),path=require('node:path'),assert=require('node:assert/strict'),crypto=require('node:crypto'),{spawnSync}=require('node:child_process');
const Reader=require('../03_functions/js/dendra_reader.js'),Core=require('../03_functions/js/dendra_core.js'),Soil=require('../03_functions/js/soil_moisture_engine.js'),Charts=require('../03_functions/js/soil_moisture_charts.js');
const args=Object.fromEntries(process.argv.slice(2).reduce((a,x,i,v)=>i%2?a:[...a,[x,v[i+1]]],[]));
const required=['--fixture-root','--fixture-manifest-sha256','--assertions-sha256','--saved-native-root','--saved-native-manifest'];required.forEach(k=>assert(args[k],k+' required'));
const root=fs.realpathSync(args['--fixture-root']),hash=b=>crypto.createHash('sha256').update(b).digest('hex'),clone=x=>JSON.parse(JSON.stringify(x));
const checks=[],results=[],snapshots=[],commands=[];let fixture;
function check(name,fn){fn();checks.push({name,status:'PASS'});}
async function asyncCheck(name,fn){await fn();checks.push({name,status:'PASS'});}
function bytes(p){const f=path.resolve(root,p);assert(f.startsWith(root+path.sep),'Outside fixture');return fs.readFileSync(f);}
function json(p){return JSON.parse(bytes(p));}
const fixtureBytes=bytes('FIXTURE_MANIFEST.json');assert.equal(hash(fixtureBytes),args['--fixture-manifest-sha256']);fixture=JSON.parse(fixtureBytes);
assert.equal(hash(bytes('EXPECTED_CONSUMER_ASSERTIONS.json')),args['--assertions-sha256']);assert.equal(fixture.expected_assertions_sha256,args['--assertions-sha256']);
const files=new Map();for(const pin of fixture.files){assert(!files.has(pin.path));const b=bytes(pin.path);assert.equal(b.length,pin.bytes);assert.equal(hash(b),pin.sha256);files.set(pin.path,pin);}
check('immutable fixture pins and complete declared closure',()=>assert.equal(files.size,454));
const selection=json('SELECTION.json'),manifest=json('baseline/original-manifest.json');
assert.equal(selection.source_manifest.sha256,hash(bytes('baseline/original-manifest.json')));
const registry=new Map();
function register(locator,relative){const pin=files.get(relative);assert(pin,relative);assert(!registry.has(locator));registry.set(locator,{...pin,path:locator,relative});}
register(selection.source_manifest.path,'baseline/original-manifest.json');
for(const [p] of files){if(p.startsWith('baseline/')&&p!=='baseline/original-manifest.json')register(p.slice(9),p);else if(p.startsWith('routine/')&&(/\/(bridge|generation|daily-output)\.json$/.test(p)||p.includes('/prepared/lineage/')))register(path.join(root,p),p);}
function options(item,{transform,readHook,omit}={}){
 const log=[],components=[...registry.values()].filter(d=>d.path!==omit).map(({relative,audience,...d})=>d);
 const o={manifest:selection.source_manifest,bridge:item.final_bridge,selection:selection.streams.map(s=>s.identity.stream_id),components,crypto:crypto.webcrypto,
  readBytes:async(d,signal,max)=>{assert(!signal.aborted);assert(d.bytes<=max);const pin=registry.get(d.path);assert(pin);log.push(pin.relative);if(readHook)await readHook(d,signal);const b=bytes(pin.relative);return transform?transform(b,pin):b;}};
 return {o,log};
}
// Python is used only for exact producer JSON record digests: JSON number tokens
// distinguish 1.0 from 1, which a JS Number round-trip cannot preserve. It reads
// independently hash-verified source components, never expected/ or assertions.
function sourceDigests(paths){
 const script="import json,hashlib,sys\nfrom pathlib import Path\nr=Path(sys.argv[1]);out={}\nfor p in json.load(sys.stdin):\n j=json.loads((r/p).read_text()); rows=j.get('records',j.get('rows'));  groups=rows if isinstance(rows,dict) else {j['stream_id']:rows}\n out[p]={sid:{v['date']:hashlib.sha256((json.dumps(v,sort_keys=True,separators=(',',':'),ensure_ascii=True,allow_nan=False)+'\\n').encode()).hexdigest() for v in vals} for sid,vals in groups.items()}\nprint(json.dumps(out))";
 const run=spawnSync('python3',['-c',script,root],{input:JSON.stringify(paths),encoding:'utf8',maxBuffer:4*1024*1024});commands.push({command:'python3 exact source-record JSON digest helper',exit:run.status});assert.equal(run.status,0,run.stderr);return JSON.parse(run.stdout);
}
function sourceRecord(component,sid,date){const p=registry.get(component.path).relative,j=json(p);const r=(j.records||j.rows[sid]).find(r=>r.date===date);assert(r);return {p,r};}
function portable(component){if(!component)return null;return {path:registry.get(component.path).relative,bytes:component.bytes,sha256:component.sha256};}
async function actualFixture(){
 for(const item of selection.streams){const {o,log}=options(item),r=Reader.createReader(Reader.DELIVERY,o);assert.equal(log.length,0);const catalog=await r.loadCatalog();assert(log.every(p=>!p.includes('/history/')&&!p.includes('daily-output')&&!p.includes('/lineage/')));
  assert.equal(catalog.series.length,8);assert.equal(catalog.stations.length,6);const sensor=catalog.series.find(s=>s.stream_id===item.identity.stream_id);
  const years=[...files.keys()].filter(p=>p.startsWith('baseline/history/'+sensor.station_id+'/'+sensor.stream_id+'/')).map(p=>Number(/wy-(\d+)/.exec(p)[1])).sort((a,b)=>a-b);
  const rows=await r.history(sensor.id,years),state=r.getState(sensor.id,years),n=log.length;
  assert.deepEqual(await r.history(sensor.id,[...years].reverse()),rows);assert.equal(log.length,n);assert(r.cacheInfo.entries<=24&&r.cacheInfo.bytes<=12000000);
  assert(log.filter(p=>p.includes('/history/')).every(p=>p.includes('/'+sensor.stream_id+'/')));
  rows.forEach(row=>assert.deepEqual(Object.keys(row).sort(),'date v ok wy dowy x n expected coverage span flags'.split(' ').sort()));
  assert.equal(new Set(rows.map(x=>x.date)).size,rows.length);assert.equal(state.records.length,rows.length);assert.equal(state.latest_instantaneous,null);assert.equal(state.publication_eligible,false);
  snapshots.push({scenario:item.scenario,stream_id:item.identity.stream_id,years,rows,state,catalog,log,cache:r.cacheInfo});
  check(item.scenario+' independent real adapter, metadata-only catalog, selected-WY lazy history and cache',()=>{assert(Core.segments(rows).flat().every(Core.good));assert.deepEqual(Charts.segments(rows),Core.segments(rows));});
 }
 // Only now inspect the oracle. It never enters reader options or transport.
 const oracle=json('EXPECTED_CONSUMER_ASSERTIONS.json'),paths=[...new Set(snapshots.flatMap(s=>s.state.records.flatMap(r=>[r.bulk?.component,r.api?.component].filter(Boolean).map(d=>registry.get(d.path).relative))))],digests=sourceDigests(paths);
 for(const os of oracle.streams){const actual=snapshots.find(s=>s.scenario===os.scenario);assert(actual);for(const e of os.dates){const mismatches=[];let observed=null;try{
   const record=actual.state.records.find(r=>r.record.date===e.date),row=actual.rows.find(r=>r.date===e.date);assert(record&&row,'Missing exact date');const s=actual.state,i=s.identity,r=record.record;
   for(const k of ['station_id','stream_id','depth_cm','native_unit','conversion_multiplier'])assert.deepEqual(i[k],e[k],k);
   assert.equal(s.depth_status,e.depth_status);assert.equal(s.scale_state,e.scale_state);
   assert.equal(record.authority,e.authority);assert.equal(record.state,e.expected_state);assert.equal(record.valid_empty,e.valid_empty);
   assert.equal(r.mean_native,e.mean_native);assert.equal(r.mean_percent,e.mean_percent);assert.equal(row.ok,e.presentation_eligible);assert.equal(row.v,e.presentation_eligible?e.mean_percent:null);
   assert.equal(row.wy,e.water_year);assert.equal(row.dowy,e.dowy);assert.equal(row.x,e.water_day_aligned);assert.equal(r.mean_percent===0,e.true_zero);
   assert.equal(!!record.bulk,e.bulk_evidence_present);assert.equal(!!record.api,e.api_evidence_present);assert.equal(record.authority==='SEALED_API'?r.query_complete:null,e.query_complete_api);
   assert.equal(!!s.update?.material_changed_dates.includes(e.date),e.materially_changed);
   const src=sourceRecord(record.api?.component||record.bulk.component,os.stream_id,e.date);assert.deepEqual(r,src.r);assert.equal(digests[src.p][os.stream_id][e.date],e.record_sha256);
   observed={identity:i,depth_status:s.depth_status,scale_state:s.scale_state,date:r.date,authority:record.authority,state:record.state,mean_native:r.mean_native,mean_percent:r.mean_percent,display:row,true_zero:r.mean_percent===0,valid_empty:record.valid_empty,query_complete_api:record.authority==='SEALED_API'?r.query_complete:null,materially_changed:!!s.update?.material_changed_dates.includes(r.date),record_sha256:digests[src.p][os.stream_id][e.date],original_bulk_component:portable(record.bulk?.component),prepared_api_output:portable(record.api?.component),original_bulk_record_sha256:record.bulk?digests[registry.get(record.bulk.component.path).relative][os.stream_id][e.date]:null};
   assert.deepEqual(portable(record.bulk?.component),e.original_bulk_component);assert.deepEqual(portable(record.api?.component),e.prepared_api_output);
   if(record.bulk){const bulk=sourceRecord(record.bulk.component,os.stream_id,e.date);assert.deepEqual(record.bulk.record,bulk.r);assert.equal(digests[bulk.p][os.stream_id][e.date],e.original_bulk_record_sha256);}else assert.equal(e.original_bulk_record_sha256,null);
  }catch(error){mismatches.push(error.message);}results.push({scenario:os.scenario,stream_id:os.stream_id,date:e.date,status:mismatches.length?'FAIL':'PASS',observed,mismatches});}}
 check('exact immutable assertion scope',()=>assert.equal(results.length,60));
 check('successful no-op/source check remains distinct from material change',()=>{const s=snapshots.find(s=>s.scenario==='overlap_no_change').state;assert.equal(s.update.outcome,'STREAM_UPDATE_SUCCESS');assert(s.update.last_attempted_source_check);assert.deepEqual(s.update.material_changed_dates,[]);});
 check('unadmitted replacement keeps prior and does not invent a provider attempt',()=>{const s=snapshots.find(s=>s.scenario==='failed_replacement').state;assert.equal(s.update.outcome,'KEEP_PRIOR_ACKNOWLEDGED_HISTORY');assert.equal(s.admission.status,'REPLACEMENT_NOT_ADMITTED');assert.equal(s.update.attempt_evidence,null);assert.equal(s.update.last_attempted_source_check,null);assert(s.coverage.gaps.length);assert.equal(s.coverage.queried_empty.length,0);});
 check('unqueried is not failed or empty',()=>{const s=snapshots.find(s=>s.scenario==='unqueried').state;assert.equal(s.update,null);assert.equal(s.coverage,null);assert(s.records.every(r=>r.authority==='CANDIDATE3_BULK'));});
 check('exact UNKNOWN choices and existing component predicates',()=>{const c=snapshots[0].catalog,quail=c.stations.find(s=>s.sensors.filter(x=>x.depth_cm===null).length===2),by=Object.fromEntries(c.series.map(s=>[s.id,s]));assert(quail);assert.equal(Core.resolve(quail,'site',by).stream,null);assert.equal(Core.resolve(quail,'unknown',by).stream,null);assert.notEqual(Core.instrumentLabel(quail.sensors[0],c.series),Core.instrumentLabel(quail.sensors[1],c.series));for(const s of quail.sensors)assert.equal(Core.resolve(quail,'unknown',by,{[quail.id]:s.id}).stream.id,s.id);assert.equal(Soil.primary(quail,'site'),null);});
}
// Re-signed in-memory specimens probe schema validation beyond hash rejection.
// They are explicitly synthetic and never replace the immutable producer fixture.
function specimen(mutate=()=>{}){
 const g=clone(manifest),item=selection.streams[0],entry=json('baseline/'+g.station_index.path).stations.find(st=>st.station_id===item.identity.station_id).streams.find(s=>s.stream_id===item.identity.stream_id),d=json('baseline/'+entry.descriptor.path),h=d.history.find(h=>files.has('baseline/'+h.file.path)),p=json('baseline/'+h.file.path),binding=json('baseline/'+g.source_binding.path);
 p.records=p.records.filter(r=>r.daily_status==='ACCEPTED').slice(0,3);d.history=[{...h,rows:p.records.length}];d.rows=p.records.length;mutate({g,d,p,binding});
 const memory=new Map(),pins=[];
 const put=(name,obj)=>{const b=Buffer.from(JSON.stringify(obj)),pin={path:name,bytes:b.length,sha256:hash(b)};memory.set(name,b);pins.push(pin);return pin;};
 const part=put('part',p);part.path=`history/${d.identity.station_id}/${d.identity.stream_id}/wy-${p.water_year}.${part.sha256}.json`;memory.set(part.path,memory.get('part'));memory.delete('part');d.history[0].file=part;
 const desc=put('descriptor',d);desc.path=`streams/${d.identity.station_id}/${d.identity.stream_id}.${desc.sha256}.json`;memory.set(desc.path,memory.get('descriptor'));memory.delete('descriptor');
 g.source_binding=put('source-binding.json',binding);g.station_index=put('stations.json',{schema_version:Reader.DELIVERY,kind:'station_index',stations:[{station_id:d.identity.station_id,streams:[{stream_id:d.identity.stream_id,descriptor:desc}]}]});g.census={...g.census,streams:1,stations:1};g.files=clone(pins);const manifestPin=put('manifest.json',g);
 return {d,p,memory,o:{manifest:manifestPin,selection:[d.identity.stream_id],components:pins,crypto:crypto.webcrypto,readBytes:async pin=>memory.get(pin.path)}};
}
async function runSpec(s){const r=Reader.createReader(Reader.DELIVERY,s.o),c=await r.loadCatalog(),sensor=c.series[0],years=[s.p.water_year],rows=await r.history(sensor.id,years);return {r,c,sensor,rows,state:r.getState(sensor.id,years)};}
function apiSpec(scenario,mutate){
 const item=selection.streams.find(s=>s.scenario===scenario),{o}=options(item),memory=new Map([...registry].map(([p,d])=>[p,bytes(d.relative)])),get=p=>JSON.parse(memory.get(p));
 const bridge=get(item.final_bridge.path),a=get(bridge.api.path),sid=item.identity.stream_id,s=a.streams[sid],daily=a.daily,outputPath=daily.root+'/daily-output.json',lineagePath=daily.root+'/lineage/'+sid+'.json',output=get(outputPath),lineage=get(lineagePath),routine=get(s.last_routine_generation.path);
 mutate({a,s,bridge,output,lineage,routine,sid});
 const pins=new Map(o.components.map(d=>[d.path,d]));
 const put=(p,obj)=>{const b=Buffer.from(JSON.stringify(obj)),d={path:p,bytes:b.length,sha256:hash(b)};memory.set(p,b);pins.set(p,d);return d;};
 // Re-sign only this synthetic control chain. No producer file is written.
 const routinePin=put(s.last_routine_generation.path,routine);s.last_routine_generation={path:routinePin.path,sha256:routinePin.sha256};lineage.generation_sha256=routinePin.sha256;
 const lineagePin=put(lineagePath,lineage),outputPin=put(outputPath,output);
 for(const [name,pin] of [['daily-output.json',outputPin],['lineage/'+sid+'.json',lineagePin]]){const d=daily.pins.find(p=>p.path===name);d.sha256=pin.sha256;d.bytes=pin.bytes;}
 s.last_prepared_candidate=clone(daily);const apiPin=put(bridge.api.path,a);bridge.api={path:apiPin.path,sha256:apiPin.sha256};const bridgePin=put(item.final_bridge.path,bridge);
 o.components=[...pins.values()];o.bridge=bridgePin;o.readBytes=async d=>memory.get(d.path);
 return {o,item};
}
async function runApiSpec(s){const r=Reader.createReader(Reader.DELIVERY,s.o),c=await r.loadCatalog(),sensor=c.series.find(x=>x.stream_id===s.item.identity.stream_id),rows=await r.history(sensor.id,[2024]);return {rows,state:r.getState(sensor.id,[2024])};}
async function safety(){
 for(const [name,mutate] of [
  ['crossed generation schema',x=>x.g.schema_version='dendra-daily-2.0.0'],
  ['candidate binding mismatch',x=>x.binding.candidate_manifest.sha256='0'.repeat(64)],
  ['wrong station row',x=>x.p.records[0].station_id='0'.repeat(24)],
  ['wrong exact depth',x=>x.p.records[0].depth_cm=7],
  ['wrong multiplier',x=>x.d.identity.conversion_multiplier=1],
  ['wrong WY',x=>x.p.records[0].water_year++],
  ['wrong DOWY',x=>x.p.records[0].dowy++],
  ['wrong leap coordinate',x=>x.p.records[0].water_day_aligned++],
  ['duplicate date',x=>x.p.records[1]=clone(x.p.records[0])],
  ['fractional actual sample count',x=>x.p.records[0].n_valid=1.5],
  ['quality alias',x=>{x.p.records[0].processing_version='dendra-bulk-daily-3';x.p.records[0].source_quality_status='RESOLVED_CLEAR';}],
  ['unsupported processing provenance',x=>x.p.records[0].processing_version='unknown'],
  ['unsupported native unit',x=>x.d.identity.native_unit='Unknown'],
  ['null is not an accepted zero',x=>x.p.records[0].daily_mean_vwc_percent=null]
 ])await asyncCheck('reject '+name,async()=>assert.rejects(runSpec(specimen(mutate))));
 await asyncCheck('positive fractional expected_samples unchanged',async()=>{const x=await runSpec(specimen(({p})=>{p.records[0].expected_samples=p.records[0].expected_sample_count=86400/17400;p.records[0].cadence_seconds=17400;}));assert.equal(x.rows[0].expected,86400/17400);});
 await asyncCheck('unresolved Dimensionless retains native only',async()=>{const x=await runSpec(specimen(({d,p})=>{d.identity.native_unit='Dimensionless';d.identity.conversion_multiplier=1;for(const r of p.records){r.native_unit='Dimensionless';r.conversion_multiplier=1;r.daily_status='UNRESOLVED_SEMANTICS';r.daily_mean_vwc_percent=r.mean_percent=r.mean_value=null;r.plot_eligible=false;}}));assert(x.rows.every(r=>r.v===null&&!r.ok));assert(x.state.records.some(r=>r.record.mean_native!==null));assert.equal(x.sensor.unit,'Dimensionless');assert.equal(Soil.percentVwc({...x.sensor,observation:{eligible:true,value:.25}}),null);});
 for(const [name,config] of [
  ['hash mismatch',{transform:b=>{const c=Buffer.from(b);c[0]^=1;return c;}}],
  ['oversize bytes',{transform:b=>Buffer.concat([b,Buffer.from('x')])}],
  ['missing requested component',{omit:manifest.station_index.path}]
 ])await asyncCheck('reject '+name,async()=>{const x=options(selection.streams[0],config);await assert.rejects(Reader.createReader(Reader.DELIVERY,x.o).loadCatalog());});
 await asyncCheck('missing partial WY is unavailable, never synthesized',async()=>{const x=options(selection.streams[0]),r=Reader.createReader(Reader.DELIVERY,x.o),c=await r.loadCatalog(),s=c.series[0];const entry=json('baseline/'+manifest.station_index.path).stations.flatMap(x=>x.streams).find(x=>x.stream_id===s.stream_id),d=json('baseline/'+entry.descriptor.path),missing=d.history.find(h=>!files.has('baseline/'+h.file.path));assert(missing);await assert.rejects(r.history(s.id,[missing.water_year]));assert.throws(()=>r.getState(s.id,[missing.water_year]));});
 await asyncCheck('abort before activation performs no read',async()=>{const x=options(selection.streams[0]),r=Reader.createReader(Reader.DELIVERY,x.o),a=new AbortController();a.abort();await assert.rejects(r.loadCatalog(a.signal));assert.equal(x.log.length,0);});
 await asyncCheck('cancelled delayed history cannot commit state',async()=>{let release,entered;const gate=new Promise(r=>release=r),arrived=new Promise(r=>entered=r);const x=options(selection.streams[0],{readHook:async d=>{if(d.path.startsWith('history/')){entered();await gate;}}}),r=Reader.createReader(Reader.DELIVERY,x.o),c=await r.loadCatalog(),s=c.series[0],y=snapshots[0].years[0];const pending=r.history(s.id,[y]);await arrived;r.cancel();release();await assert.rejects(pending);assert.throws(()=>r.getState(s.id,[y]));assert.equal(r.cacheInfo.entries,0);});
 await asyncCheck('external abort cancels the selection and hides its cached state',async()=>{const x=specimen();let delay=false,release,entered;const gate=new Promise(r=>release=r),arrived=new Promise(r=>entered=r),read=x.o.readBytes;x.o.readBytes=async d=>{if(delay){entered();await gate;}return read(d);};const r=Reader.createReader(Reader.DELIVERY,x.o),c=await r.loadCatalog(),s=c.series[0],year=x.p.water_year;await r.history(s.id,[year]);delay=true;const controller=new AbortController(),pending=r.history(s.id,[year,year+1],controller.signal);await arrived;controller.abort();release();await assert.rejects(pending);assert.throws(()=>r.getState(s.id,[year]));});
 await asyncCheck('catalog reload never reuses a prior snapshot cache',async()=>{const x=specimen(),r=Reader.createReader(Reader.DELIVERY,x.o),c=await r.loadCatalog(),s=c.series[0];await r.history(s.id,[x.p.water_year]);assert.equal(r.cacheInfo.entries,1);await r.loadCatalog();assert.equal(r.cacheInfo.entries,0);assert.throws(()=>r.getState(s.id,[x.p.water_year]));});
 for(const [name,mutate] of [
  ['API unsupported schema',x=>x.output.schema_version='legacy'],
  ['API numerical-policy mismatch',x=>x.output.numerical_policy='changed'],
  ['API station mismatch',x=>x.output.rows[x.sid][0].identity.station_id='0'.repeat(24)],
  ['API exact depth mismatch',x=>x.output.rows[x.sid][0].identity.depth_cm=1],
  ['API fractional actual count',x=>x.output.rows[x.sid][0].n_total=1.5],
  ['API zero expected count',x=>x.output.rows[x.sid][0].expected_samples=0],
  ['API incomplete query',x=>x.output.rows[x.sid][0].query_complete=false],
  ['API unbound seal',x=>x.output.rows[x.sid][0].source_intervals[0].seal_record_sha256='0'.repeat(64)],
  ['API mixed lineage',x=>x.lineage.identity.stream_id='0'.repeat(24)],
  ['API duplicate date',x=>x.output.rows[x.sid].push(clone(x.output.rows[x.sid][0]))],
  ['API conflicting admitted views',x=>x.s.views.push(clone(x.s.views[0]))],
  ['API missing covered day',x=>x.output.rows[x.sid].splice(0,1)],
  ['API unrecognized quality disposition',x=>x.output.quality_disposition[x.sid][0].state='ACCEPTED'],
  ['API quality-withheld promotion',x=>{const r=x.output.rows[x.sid][1];r.presentation_eligible=r.plot_eligible=true;r.mean_percent=r.mean_value=25;r.n_valid=144;}],
  ['API crossed bridge outcome',x=>x.bridge.changes[x.sid].outcome='KEEP_PRIOR_ACKNOWLEDGED_HISTORY']
 ])await asyncCheck('reject '+name,async()=>assert.rejects(runApiSpec(apiSpec('quality_veto',mutate))));
 await asyncCheck('sealed API fractional expectation preserved without science changes',async()=>{const x=await runApiSpec(apiSpec('quality_veto',({output,sid})=>{output.rows[sid][0].expected_samples=86400/17400;output.rows[sid][0].cadence_seconds=17400;}));assert.equal(x.rows.find(r=>r.date==='2024-02-28').expected,86400/17400);});
 await asyncCheck('valid-empty cannot become zero',async()=>assert.rejects(runApiSpec(apiSpec('valid_empty',({output,sid})=>{output.rows[sid][0].mean_native=0;}))));
 await asyncCheck('failed replacement requires producer non-admission',async()=>assert.rejects(runApiSpec(apiSpec('failed_replacement',({routine,sid})=>{routine.changes[sid].status='ADMITTED';}))));
 await asyncCheck('missing original bulk prevents API from concealing a missing component',async()=>{const item=selection.streams.find(s=>s.scenario==='quality_veto'),component=[...registry.keys()].find(p=>p.startsWith('history/'+item.identity.station_id+'/'+item.identity.stream_id+'/wy-2024.'));const {o}=options(item,{omit:component});const r=Reader.createReader(Reader.DELIVERY,o),c=await r.loadCatalog(),s=c.series.find(s=>s.stream_id===item.identity.stream_id);await assert.rejects(r.history(s.id,[2024]));assert.throws(()=>r.getState(s.id,[2024]));});
 await asyncCheck('declared metadata cap rejects before transport',async()=>{const x=specimen(),pin=x.o.components.find(d=>d.path==='manifest.json');pin.bytes=Reader.DELIVERY_BOUND.generation_bytes+1;assert.throws(()=>Reader.createReader(Reader.DELIVERY,x.o),/bound/);});
 await asyncCheck('returned catalog and state cannot mutate reader-owned identity or cache',async()=>{const x=specimen(),a=await runSpec(x);a.c.series[0].depth_cm=999;a.state.records[0].record.mean_percent=999;a.rows[0].v=999;const again=await a.r.history(a.sensor.id,[x.p.water_year]);assert.notEqual(again[0].v,999);assert.notEqual(a.r.getState(a.sensor.id,[x.p.water_year]).records[0].record.mean_percent,999);});
 await asyncCheck('bounded snapshot cache evicts old exact selections',async()=>{const {o}=options(selection.streams[0]),r=Reader.createReader(Reader.DELIVERY,o),c=await r.loadCatalog(),s=c.series[0],years=snapshots[0].years;assert(years.length>=5);for(let mask=1;mask<=25;mask++){await r.history(s.id,years.filter((y,n)=>mask&(1<<n)));assert(r.cacheInfo.entries<=24&&r.cacheInfo.bytes<=12000000);}assert.throws(()=>r.getState(s.id,[years[0]]));});
 check('browser UMD export remains additive without activation',()=>{const vm=require('node:vm'),context={};vm.runInNewContext(fs.readFileSync(path.join(__dirname,'../03_functions/js/dendra_reader.js'),'utf8'),context,{timeout:1000});assert.equal(typeof context.BrimDendraReader.Reader,'function');assert.equal(typeof context.BrimDendraReader.createReader,'function');assert.equal(context.BrimDendraReader.DELIVERY,Reader.DELIVERY);});
 check('unknown dispatch and raw provider URL rejected',()=>{assert.throws(()=>Reader.createReader('unsupported',{}));assert.throws(()=>Reader.createReader('dendra-daily-1.1.0',{indexUrl:'https://api.dendra.science/v2'}));});
}
async function nativeCompatibility(){
 const base=fs.realpathSync(args['--saved-native-root']),pins=JSON.parse(fs.readFileSync(args['--saved-native-manifest'])),requests=[];
 const fetcher=async url=>{const p=new URL(url).pathname.replace(/^\/docs\/data\//,'');assert(p.startsWith('dendra/')&&!p.includes('..'));const pin=pins[p];assert(pin);const b=fs.readFileSync(path.join(base,p));assert.equal(b.length,pin.bytes);assert.equal(hash(b),pin.sha256);requests.push(p);return new Response(b);};
 const r=Reader.createReader('dendra-daily-2.0.0',{indexUrl:'http://offline.invalid/docs/data/dendra/index.json',fetcher,crypto:crypto.webcrypto});assert(r instanceof Reader.Reader);const index=await r.loadIndex();assert.equal(requests.length,1+index.catalogs.length);
 let total=0;for(const s of index.streams){const rows=await r.history(index,s,undefined,'all');assert.equal(rows.length,s.summary.calendar_row_count);total+=rows.length;}const s=index.streams[0];await r.history(index,s);const n=requests.length;await r.history(index,s);assert.equal(requests.length,n);
 checks.push({name:'saved native v2 actual Reader, lazy catalog and all selected histories',status:'PASS',streams:index.streams.length,rows:total,requests:requests.length});
 // Explicit synthetic native v1 specimen; no fixture or native contract changes.
 const sid='a'.repeat(24),station='b'.repeat(24),row={date:'2024-02-29',v:0,ok:true,wy:2024,dowy:152,x:152,n:144,flags:[]};
 const p={schema_version:'dendra-daily-1.0.0',policy_version:'dendra-daily-1.0.0-frozen-cadence',safeguard_version:'dendra-safeguards-1.1.0',datastream_id:sid,station_id:station,parameter:'soil_moisture',depth_cm:null,unit:'% volumetric water content',water_year:2024,rows:[row]},b=Buffer.from(JSON.stringify(p)),h={path:`docs/data/dendra/history/${sid}/2024-${hash(b)}.json`,bytes:b.length,sha256:hash(b),water_year:2024,rows:1};
 const i={schema_version:'dendra-daily-1.1.0',integration_version:'dendra-integration-1',policy_version:p.policy_version,safeguard_version:p.safeguard_version,product_id:'dendra-daily',completeness:'complete_selected_catalog',mode:'replay',generation:'20240229T000000-1',as_of_utc:'2024-03-01T08:00:00Z',generated_at_utc:'2024-03-01T08:00:00Z',complete_through_date:row.date,retention:{soil_moisture:{water_years:10},soil_temperature:{completed_days:90}},files:[h],stations:[{station_id:station,public_level:3,source_is_hidden:false}],streams:[{datastream_id:sid,station_id:station,parameter:'soil_moisture',public_level:3,source_is_hidden:false,depth_cm:null,unit_normalization:{target_unit:p.unit,status:'verified_percent_conversion'},histories:[h],recent_rows:[row],summary:{calendar_row_count:1},csv_path:h.path,diagnostics_path:h.path,change:Object.fromEntries([7,14,30].map(n=>[n,{state:'insufficient',span:n,required:Math.ceil(.8*n),delta:null}]))}]};
 const old=Reader.createReader(i.schema_version,{indexUrl:'http://offline.invalid/docs/data/dendra/index.json',fetcher:async u=>new Response(u.endsWith('index.json')?JSON.stringify(i):b),crypto:crypto.webcrypto}),loaded=await old.loadIndex();assert(old instanceof Reader.Reader);assert.deepEqual(await old.history(loaded,loaded.streams[0]),[row]);checks.push({name:'synthetic native v1 dispatch, zero, UNKNOWN and leap day unchanged',status:'PASS'});
}
(async()=>{await actualFixture();await safety();await nativeCompatibility();const failed=results.filter(r=>r.status==='FAIL');console.log(JSON.stringify({status:failed.length?'FAIL':'PASS',assertions_total:results.length,assertions_pass:results.length-failed.length,assertions_fail:failed.length,assertion_results:results,checks,commands,fixture_pins:{manifest:hash(fixtureBytes),assertions:args['--assertions-sha256'],files:files.size},requests:snapshots.map(s=>({scenario:s.scenario,years:s.years,paths:s.log,cache:s.cache})),scope:'Source/component only. No network, browser, producer execution or fixture writes.'},null,2));if(failed.length)process.exitCode=1;})().catch(error=>{console.log(JSON.stringify({status:'FAIL',error:error.stack,assertions_total:results.length,assertions_pass:results.filter(r=>r.status==='PASS').length,assertions_fail:results.filter(r=>r.status==='FAIL').length,assertion_results:results,checks,commands},null,2));process.exitCode=1;});
