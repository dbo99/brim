/* Prepared Dendra contract reader. Daily arithmetic remains in the feed's R producer. */
(function(root,factory){const api=factory();if(typeof module==='object'&&module.exports)module.exports=api;else root.BrimDendraReader=api;})(typeof globalThis!=='undefined'?globalThis:this,function(){
'use strict';
const SCHEMA='dendra-daily-1.1.0', DAILY='dendra-daily-1.0.0', POLICY='dendra-daily-1.0.0-frozen-cadence', SAFE='dendra-safeguards-1.1.0';
const need=(ok,message)=>{if(!ok)throw Error(message);};
const id=x=>typeof x==='string'&&/^[a-f0-9]{24}$/.test(x);
const finite=x=>typeof x==='number'&&Number.isFinite(x);
const unit=s=>s.parameter==='soil_temperature'?'degree Celsius':'% volumetric water content';
const utc=x=>typeof x==='string'&&/^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(?:\.\d+)?Z$/.test(x)&&Number.isFinite(Date.parse(x));
function validateIndex(i){
 if(i?.schema_version==='dendra-daily-2.0.0')return validateArchiveRoot(i);
 need(i&&i.schema_version===SCHEMA&&i.integration_version==='dendra-integration-1'&&i.policy_version===POLICY&&i.safeguard_version===SAFE&&i.product_id==='dendra-daily','Unsupported Dendra schema/policy');
 need(i.completeness==='complete_selected_catalog'&&['live','replay'].includes(i.mode)&&/^\d{8}T\d{6}-\d+$/.test(i.generation),'Incomplete/unidentified Dendra generation');
 need(utc(i.as_of_utc)&&utc(i.generated_at_utc)&&/^\d{4}-\d\d-\d\d$/.test(i.complete_through_date),'Invalid feed clock');
 need(i.mode==='replay'||utc(i.expires_at_utc),'Missing live expiration');
 need(i.retention?.soil_moisture?.water_years===10&&i.retention?.soil_temperature?.completed_days===90,'Unsupported retention');
 need(Array.isArray(i.files)&&i.files.length<2000&&Array.isArray(i.streams)&&i.streams.length>0&&Array.isArray(i.stations),'Invalid index inventory');
 const paths=new Set(),ids=new Set(),stations=new Map();
 for(const f of i.files){need(typeof f.path==='string'&&/^docs\/data\/dendra\/(history|companion|diagnostics|state)\/[a-zA-Z0-9_./-]+$/.test(f.path)&&!f.path.split('/').includes('..')&&!paths.has(f.path)&&/^[a-f0-9]{64}$/.test(f.sha256)&&Number.isInteger(f.bytes)&&f.bytes>0&&f.bytes<12e6,'Unsafe file inventory');paths.add(f.path);}
 for(const s of i.stations){need(id(s.station_id)&&!stations.has(s.station_id)&&s.public_level===3&&s.source_is_hidden===false,'Unsafe station identity');if(s.source_is_geo_protected)need(!s.geometry,'Protected coordinates');if(s.geometry){const c=s.geometry.coordinates;need(s.geometry.type==='Point'&&Array.isArray(c)&&c.length===2&&c.every(finite)&&Math.abs(c[0])<=180&&Math.abs(c[1])<=90,'Invalid station geometry');}stations.set(s.station_id,s);}
 for(const s of i.streams){
  need(id(s.datastream_id)&&!ids.has(s.datastream_id)&&stations.has(s.station_id),'Duplicate/missing sensor identity');ids.add(s.datastream_id);
  need(['soil_moisture','soil_temperature'].includes(s.parameter)&&s.public_level===3&&s.source_is_hidden===false&&(s.depth_cm===null||finite(s.depth_cm)&&s.depth_cm>=0),'Unsafe sensor metadata');
  need(s.unit_normalization?.target_unit===unit(s)&&s.unit_normalization?.status===(s.parameter==='soil_temperature'?'verified_temperature_conversion':'verified_percent_conversion'),'Unverified sensor units');
  need(Array.isArray(s.histories)&&Array.isArray(s.recent_rows)&&s.recent_rows.length<=60&&s.summary,'Missing selected-history contract');
  need(paths.has(s.csv_path)&&paths.has(s.diagnostics_path),'Missing diagnostic inventory');
  for(const h of s.histories){const f=i.files.find(f=>f.path===h.path);need(f&&f.sha256===h.sha256&&Number.isInteger(h.water_year)&&Number.isInteger(h.rows)&&h.rows>0,'History inventory mismatch');const kind=s.parameter==='soil_temperature'?'companion':'history';need(h.path===`docs/data/dendra/${kind}/${s.datastream_id}/${h.water_year}-${h.sha256}.json`,'Wrong history path/identity');}
  validateRows(s.recent_rows,s,i.complete_through_date);
  if(s.parameter==='soil_moisture')for(const n of [7,14,30]){const c=s.change?.[String(n)];need(c&&['wetting','drying','little','stale','insufficient','no-data'].includes(c.state)&&c.span===n&&c.required===Math.ceil(.8*n)&&(c.delta===null||finite(c.delta)),'Missing authoritative recent-change summary');if(c.last)validateRows([c.last],s,i.complete_through_date);}
  need(i.mode==='replay'||utc(s.expires_at_utc),'Missing parameter expiration');
 }
 return i;
}
function validateRows(rows,s,cutoff){
 let prev='';for(const r of rows){need(r&&/^\d{4}-\d\d-\d\d$/.test(r.date)&&r.date>prev&&r.date<=cutoff&&typeof r.ok==='boolean'&&Number.isInteger(r.wy)&&Number.isInteger(r.dowy)&&Number.isInteger(r.x),'Malformed daily row');prev=r.date;const day=Date.parse(r.date+'T00:00:00Z'),yr=+r.date.slice(0,4),month=+r.date.slice(5,7),wy=yr+(month>=10?1:0),aligned=Date.UTC(month>=10?1999:2000,month-1,+r.date.slice(8,10));need(Number.isFinite(day)&&new Date(day).toISOString().slice(0,10)===r.date&&r.wy===wy&&r.dowy===(day-Date.UTC(wy-1,9,1))/86400000+1&&r.x===(aligned-Date.UTC(1999,9,1))/86400000+1,'Daily calendar mismatch');need(!r.ok||finite(r.v)&&(s.parameter==='soil_temperature'||r.v>=0&&r.v<=100),'Invalid accepted daily value');need(Array.isArray(r.flags)&&finite(r.n)&&r.n>=0,'Invalid daily diagnostics');}
}
function freshness(index,s,now=Date.now()){
 if(index.mode==='replay')return 'FROZEN saved observations · not live';
 const lag=s.summary.last_plottable_date?Math.floor((now-Date.parse(s.summary.last_plottable_date+'T08:00:00Z'))/86400000):Infinity;
 return now>Date.parse(s.expires_at_utc)||lag>4?'STALE parameter/feed · last accepted '+(s.summary.last_plottable_date||'unavailable'):'Current completed daily context';
}
class Reader{
 constructor(indexUrl,fetcher=globalThis.fetch.bind(globalThis),crypto=globalThis.crypto){this.url=new URL(indexUrl,globalThis.location?.href);need(['http:','https:'].includes(this.url.protocol),'Static HTTP(S) index required');need(!/(^|\.)dendra\.science$/i.test(this.url.hostname),'Use prepared static data, never raw Dendra');this.fetcher=fetcher;this.crypto=crypto;this.index=null;this.cache=new Map();this.cacheBytes=0;this.loadEpoch=0;this.metrics={requests:0,bytes:0,cacheHits:0};}
 path(p){need(p.startsWith('docs/data/dendra/')&&!p.split('/').includes('..'),'Unsafe product path');return new URL(p.slice('docs/data/dendra/'.length),new URL('.',this.url)).href;}
 async bytes(url,signal,max=12e6){
  this.metrics.requests++;const r=await this.fetcher(url,{signal,mode:'cors',credentials:'omit',cache:'no-store'});need(r.ok,'Static feed HTTP '+r.status);
  const length=Number(r.headers?.get('content-length'));need(!length||length<=max,'Static feed exceeds bound');
  need(r.body?.getReader,'Bounded streaming response required');const stream=r.body.getReader(),chunks=[];let total=0;
  try{for(;;){if(signal?.aborted)throw new DOMException('Cancelled','AbortError');const {done,value}=await stream.read();if(done)break;total+=value.byteLength;need(total<=max,'Static feed exceeds bound');chunks.push(value);}}catch(e){await stream.cancel();throw e;}finally{stream.releaseLock();}
  const result=new Uint8Array(total);let offset=0;for(const chunk of chunks){result.set(chunk,offset);offset+=chunk.byteLength;}this.metrics.bytes+=total;return result.buffer;
 }
 async file(descriptor,signal,max=12e6){
  archiveDescriptor(descriptor,max);const b=await this.bytes(this.path(descriptor.path),signal,Math.min(max,descriptor.bytes));need(b.byteLength===descriptor.bytes,'Static file byte count mismatch');need(this.crypto?.subtle,'SHA-256 verification unavailable');
  const hash=Array.from(new Uint8Array(await this.crypto.subtle.digest('SHA-256',b))).map(x=>x.toString(16).padStart(2,'0')).join('');need(hash===descriptor.sha256,'Static file hash mismatch');return new TextDecoder().decode(b);
 }
 cachePut(key,text){if(this.cache.has(key)){this.cacheBytes-=new TextEncoder().encode(this.cache.get(key)).length;this.cache.delete(key);}this.cache.set(key,text);this.cacheBytes+=new TextEncoder().encode(text).length;while(this.cache.size>24||this.cacheBytes>12000000){const first=this.cache.keys().next().value;this.cacheBytes-=new TextEncoder().encode(this.cache.get(first)).length;this.cache.delete(first);}}
 async loadIndex(signal){
  const epoch=++this.loadEpoch,b=await this.bytes(this.url.href,signal,256000);let i=validateIndex(JSON.parse(new TextDecoder().decode(b)));
  if(i.schema_version===ARCHIVE){const streams=[],stations=new Map();for(const d of i.catalogs){const c=JSON.parse(await this.file(d,signal,262144));need(c.schema_version==='dendra-map-catalog-2'&&Array.isArray(c.streams)&&Array.isArray(c.stations),'Invalid archive catalog shard');streams.push(...c.streams);for(const st of c.stations){need(!stations.has(st.station_id)||same(stations.get(st.station_id),st),'Mixed station shards');stations.set(st.station_id,st);}}i={...i,streams,stations:[...stations.values()]};validateArchiveCatalog(i);}
  if(signal?.aborted||epoch!==this.loadEpoch)throw new DOMException('Cancelled','AbortError');this.index=i;if(i.files){const hashes=new Set(i.files.map(f=>f.sha256));for(const key of this.cache.keys())if(!hashes.has(key.split(':').pop()))this.cache.delete(key);}this.cacheBytes=[...this.cache.values()].reduce((n,t)=>n+new TextEncoder().encode(t).length,0);return i;
 }
 async verified(i,s,descriptor,signal){
  need(this.index?.generation===i.generation,'Generation changed before request');const key=s.datastream_id+':'+s.parameter+':'+descriptor.sha256;
  if(this.cache.has(key)){this.metrics.cacheHits++;const text=this.cache.get(key);this.cache.delete(key);this.cache.set(key,text);return text;}
  const text=await this.file(descriptor,signal,i.schema_version===ARCHIVE?(descriptor.path.includes('/state/')?262144:descriptor.path.endsWith('.csv')?512000:1000000):12e6);
  need(!signal?.aborted&&this.index?.generation===i.generation,'Late/mixed generation response');this.cachePut(key,text);return text;
 }
 async manifest(i,s,signal){const m=JSON.parse(await this.verified(i,s,s.manifest,signal));validateManifest(m,i,s);return m;}
 async history(i,s,signal,selection="default"){
  if(i.schema_version===ARCHIVE)return archiveHistory(this,i,s,signal,selection);
  const parts=await Promise.all(s.histories.map(async h=>{const d=i.files.find(f=>f.path===h.path),p=JSON.parse(await this.verified(i,s,d,signal));
   need(p.schema_version===DAILY&&p.policy_version===POLICY&&p.safeguard_version===SAFE&&p.datastream_id===s.datastream_id&&p.station_id===s.station_id&&p.parameter===s.parameter&&p.depth_cm===s.depth_cm&&p.unit===unit(s)&&p.water_year===h.water_year&&Array.isArray(p.rows)&&p.rows.length===h.rows,'History schema/identity/units mismatch');validateRows(p.rows,s,i.complete_through_date);need(p.rows.every(r=>r.wy===h.water_year),'Mixed water-year partition');return p.rows;}));
  const rows=parts.flat().sort((a,b)=>a.date.localeCompare(b.date));validateRows(rows,s,i.complete_through_date);need(rows.length===s.summary.calendar_row_count&&rows.at(-1)?.date===i.complete_through_date,'History calendar closure mismatch');need(s.parameter!=='soil_temperature'||rows.length===90,'Temperature is not 90 completed days');return rows;
 }
 async csv(i,s,signal){if(i.schema_version===ARCHIVE)return archiveCsv(this,i,s,signal);const f=i.files.find(f=>f.path===s.csv_path),text=await this.verified(i,s,f,signal);need(text.startsWith('datastream_id,date,')&&text.trim().split('\n').slice(1).every(row=>row.startsWith(s.datastream_id+',')),'CSV sensor identity mismatch');return text;}
}

const ARCHIVE='dendra-daily-2.0.0',BOUND={index_bytes:256000,shard_bytes:262144,partition_bytes:1000000,csv_bytes:512000,files:20000,inventory_page_files:500,catalog_shards:128,streams:1024,stations:1500,years_per_stream:256,cache_entries:24,cache_bytes:12000000,selected_rows:93696,csv_export_bytes:32000000};
const same=(a,b)=>{const canonical=x=>x&&typeof x==='object'?(Array.isArray(x)?x.map(canonical):Object.fromEntries(Object.keys(x).sort().map(k=>[k,canonical(x[k])]))):x;return JSON.stringify(canonical(a))===JSON.stringify(canonical(b));};
function archiveDescriptor(d,max){need(d&&typeof d.path==='string'&&/^docs\/data\/dendra\/(history|companion|diagnostics|state)\/[a-zA-Z0-9_./-]+$/.test(d.path)&&!d.path.split('/').includes('..')&&/^[a-f0-9]{64}$/.test(d.sha256)&&Number.isInteger(d.bytes)&&d.bytes>0&&d.bytes<=max,'Unsafe/bounded descriptor');}
function validateArchiveRoot(i){
 need(i.integration_version==='dendra-integration-2'&&i.policy_version===POLICY&&i.safeguard_version===SAFE&&i.product_id==='dendra-daily'&&same(i.limits,BOUND),'Unsupported archive policy/bounds');
 need(i.completeness==='complete_selected_catalog'&&['live','replay'].includes(i.mode)&&/^\d{8}T\d{6}-\d+$/.test(i.generation)&&utc(i.as_of_utc)&&utc(i.generated_at_utc)&&/^\d{4}-\d\d-\d\d$/.test(i.complete_through_date),'Incomplete archive/clock');
 need(i.mode==='replay'||utc(i.expires_at_utc),'Missing live archive expiry');
 const w=i.windows,t=w?.soil_temperature;need(w?.version==='dendra-windows-2'&&same(w.soil_moisture,{archive:'all_acquired_daily_no_automatic_deletion',display_water_years:10,reference:'not_computed'})&&Number.isInteger(t?.archive_completed_days)&&t.archive_completed_days>=1&&t.archive_completed_days<=3660&&t.display_completed_days===t.archive_completed_days&&t.reference==='not_computed','Unsupported archive windows');
 need(Number.isInteger(i.stream_count)&&i.stream_count>0&&i.stream_count<=1024&&Number.isInteger(i.station_count)&&i.station_count>0&&i.station_count<=1500&&Number.isInteger(i.file_count)&&i.file_count<=20000,'Archive count bound');
 for(const [key,cap,prefix] of [['catalogs',128,'catalog'],['file_pages',40,'files']]){need(Array.isArray(i[key])&&i[key].length>0&&i[key].length<=cap,'Shard count bound');const paths=new Set();for(const d of i[key]){archiveDescriptor(d,262144);need(new RegExp('^docs/data/dendra/state/'+prefix+'/[0-9]{3}-'+d.sha256+'\\.json$').test(d.path)&&!paths.has(d.path),'Shard identity/path');paths.add(d.path);}}
 need(Array.isArray(i.selection_ids)&&i.selection_ids.length===i.stream_count&&i.selection_ids.every(id)&&new Set(i.selection_ids).size===i.stream_count,'Archive selection identity');return i;
}
function validateArchiveCatalog(i){
 need(i.streams.length===i.stream_count&&i.stations.length===i.station_count,'Partial archive catalog');const stations=new Set(),ids=new Set();
 for(const st of i.stations){need(id(st.station_id)&&!stations.has(st.station_id)&&st.public_level===3&&st.source_is_hidden===false&&(!st.source_is_geo_protected||!st.geometry),'Unsafe archive station');stations.add(st.station_id);if(st.geometry){const c=st.geometry.coordinates;need(st.geometry.type==='Point'&&Array.isArray(c)&&c.length===2&&c.every(finite)&&Math.abs(c[0])<=180&&Math.abs(c[1])<=90,'Archive geometry');}}
 for(const s of i.streams){need(id(s.datastream_id)&&!ids.has(s.datastream_id)&&stations.has(s.station_id)&&s.public_level===3&&s.source_is_hidden===false&&['soil_moisture','soil_temperature'].includes(s.parameter)&&(s.depth_cm===null||finite(s.depth_cm)&&s.depth_cm>=0),'Archive sensor identity');ids.add(s.datastream_id);need(s.unit_normalization?.target_unit===unit(s)&&s.unit_normalization.status===(s.parameter==='soil_temperature'?'verified_temperature_conversion':'verified_percent_conversion'),'Archive units');need(/^\d{4}-\d\d-\d\d$/.test(s.acquired_through_date)&&s.acquired_through_date<=i.complete_through_date&&Array.isArray(s.recent_rows)&&s.recent_rows.length<=60&&Number.isInteger(s.summary?.calendar_row_count)&&s.summary.calendar_row_count>0&&s.summary.calendar_row_count<=93696,'Archive stream coverage');archiveDescriptor(s.manifest,262144);need(s.manifest.path===`docs/data/dendra/state/streams/${s.datastream_id}-${s.manifest.sha256}.json`,'Manifest identity');validateRows(s.recent_rows,s,s.acquired_through_date);if(s.latest_accepted)validateRows([s.latest_accepted],s,s.acquired_through_date);if(s.parameter==='soil_moisture')for(const n of [7,14,30]){const c=s.change?.[n];need(c&&c.span===n&&c.required===Math.ceil(.8*n)&&['wetting','drying','little','stale','insufficient','no-data'].includes(c.state)&&(c.delta===null||finite(c.delta)),'Archive change summary');}need(i.mode==='replay'||utc(s.expires_at_utc),'Archive stream expiry');}
 need(same([...ids].sort(),i.selection_ids),'Archive selection/catalog mismatch');return i;
}
function validateManifest(m,i,s){
 const {manifest,...summary}=s;need(m.schema_version==='dendra-stream-manifest-2'&&same(m.stream,summary)&&same(m.windows,i.windows[s.parameter])&&Array.isArray(m.partitions)&&m.partitions.length>0&&m.partitions.length<=256,'Archive manifest identity/window');let year=0,count=0;
 for(const p of m.partitions){need(Number.isInteger(p.water_year)&&p.water_year>year&&Number.isInteger(p.rows)&&p.rows>0&&p.rows<=366&&p.start_date<=p.end_date&&p.end_date<=s.acquired_through_date,'Archive partition coverage');year=p.water_year;count+=p.rows;for(const kind of ['history','diagnostics','csv']){const d=p[kind];archiveDescriptor(d,kind==='csv'?512000:1000000);const base=kind==='history'?(s.parameter==='soil_temperature'?'companion':'history'):'diagnostics';need(d.path===`docs/data/dendra/${base}/${s.datastream_id}/${p.water_year}-${d.sha256}.${kind==='csv'?'csv':'json'}`,'Archive partition path');}}
 need(count===s.summary.calendar_row_count&&m.partitions.at(-1).end_date===s.acquired_through_date,'Archive manifest row closure');
}
async function archiveHistory(reader,i,s,signal,selection){
 need(['default','all'].includes(selection),'Unknown display selection');const m=await reader.manifest(i,s,signal),wy=Number(i.complete_through_date.slice(0,4))+(i.complete_through_date.slice(5,7)>='10'?1:0),parts=m.partitions.filter(p=>selection==='all'||s.parameter==='soil_temperature'||p.water_year>=wy-9);const rows=[];
 for(const h of parts){const p=JSON.parse(await reader.verified(i,s,h.history,signal));need(p.schema_version===DAILY&&p.policy_version===POLICY&&p.safeguard_version===SAFE&&['datastream_id','station_id','parameter','depth_cm','orientation','native_unit_name','unit_normalization'].every(k=>same(p[k],s[k]))&&p.unit===unit(s)&&p.water_year===h.water_year&&Array.isArray(p.rows)&&p.rows.length===h.rows,'Archive history identity');validateRows(p.rows,s,s.acquired_through_date);need(p.rows[0].date===h.start_date&&p.rows.at(-1).date===h.end_date&&p.rows.every(r=>r.wy===h.water_year),'Archive history calendar');rows.push(...p.rows);need(rows.length<=93696,'Selected history bound');}
 validateRows(rows,s,s.acquired_through_date);return rows;
}
async function archiveCsv(reader,i,s,signal){const m=await reader.manifest(i,s,signal);let out='',header=null,total=0,rows=0;for(const p of m.partitions){const text=await reader.verified(i,s,p.csv,signal),lines=text.trimEnd().split(/\r?\n/);need(lines[0].startsWith('datastream_id,date,')&&(!header||header===lines[0])&&lines.length===p.rows+1&&lines.slice(1).every(l=>l.startsWith(s.datastream_id+',')),'Archive CSV identity');if(!header){header=lines[0];out=header+'\r\n';}const body=lines.slice(1).join('\r\n')+'\r\n';total+=new TextEncoder().encode(body).length;need(total+header.length+2<=32000000,'CSV export bound');out+=body;rows+=p.rows;}need(rows===s.summary.calendar_row_count,'CSV archive closure');return out;}

// Explicit in-memory admission for the unpublished local candidate. This is not
// a hosted index, a Parquet reader, or permission to expose station coordinates.
const LOCAL_CANDIDATE='dendra-00g-local-candidate-2';
function exactFields(value,fields){need(value&&typeof value==='object'&&!Array.isArray(value)&&Object.keys(value).length===fields.length&&fields.every(k=>Object.prototype.hasOwnProperty.call(value,k)),'Local candidate fields mismatch');}
const localStreamFields='station station_id stream_id export_local_series_key depth_status native_unit provider subprovider source_file_sha256 native_asset daily_asset timestamp_semantics depth_cm conversion_multiplier orientation equipment_type_id records'.split(' ');
const localCounts='n_valid n_total n_null n_invalid n_missing n_duplicate_conflicts n_duplicate_rows n_out_of_range observation_count nominal_range_observations valid_sample_count'.split(' ');
const localNumbers='mean_native mean_percent mean_value daily_mean_vwc_percent expected_samples cadence_seconds coverage_fraction temporal_span_fraction depth_cm expected_sample_count sample_count_fraction first_last_span_fraction'.split(' ');
const localStrings='date cadence_source daily_status source_quality_status source_quality_reason stream_id export_local_series_key station_id station_name depth_status native_unit processing_version date_pst_fixed'.split(' ');
const localCalendar='water_year dowy water_day water_year_days water_day_aligned plot_day_aligned'.split(' ');
const localBooleans='plot_eligible query_complete source_empty'.split(' ');
const localRowFields=[...localCounts,...localNumbers,...localStrings,...localCalendar,...localBooleans,'accepted_stream_id','latest_source_timestamp_utc','flags','conversion_multiplier'];
const localStates=['ACCEPTED','WITHHELD_BY_EXISTING_SCREEN','MISSING','UNRESOLVED_SEMANTICS'];
const localAcceptedQuality=Object.freeze({'dendra-bulk-daily-2':'RESOLVED_CLEAR','dendra-bulk-daily-3':'PROVIDER_READY_TO_USE'});
function localDepth(s){need(s.depth_cm===null?s.depth_status==='UNKNOWN':finite(s.depth_cm)&&s.depth_cm>=0&&s.depth_status==='ACCEPTED_EXACT_STREAM_REVIEW','Local candidate depth/status mismatch');}
function localBulkRow(r,s){
   exactFields(r,localRowFields);
   for(const k of localCounts)need(Number.isInteger(r[k])&&r[k]>=0,'Local sample count must be a nonnegative integer');
   for(const k of localNumbers)need(r[k]===null||finite(r[k]),'Local numeric diagnostic');
   for(const k of localStrings)need(typeof r[k]==='string','Local row text');
   for(const k of localCalendar)need(Number.isInteger(r[k]),'Local calendar integer');
   for(const k of localBooleans)need(typeof r[k]==='boolean','Local row boolean');
   need((r.accepted_stream_id===null||r.accepted_stream_id===s.stream_id)&&(r.latest_source_timestamp_utc===null||utc(r.latest_source_timestamp_utc)),'Local witness identity/time');
   need(Array.isArray(r.flags)&&r.flags.every(v=>typeof v==='string'),'Local quality flags');
   for(const k of ['station_id','stream_id','export_local_series_key','native_unit','conversion_multiplier','depth_cm','depth_status'])need(r[k]===s[k],'Mixed local row/series identity');
   need(r.station_name===s.station&&Object.prototype.hasOwnProperty.call(localAcceptedQuality,r.processing_version)&&r.date_pst_fixed===r.date,'Local row provenance');
   localDepth(r);need(localStates.includes(r.daily_status),'Unsupported local daily state');
   const accepted=r.daily_status==='ACCEPTED';
   need(accepted?finite(r.daily_mean_vwc_percent)&&r.plot_eligible&&r.n_valid>0&&r.source_quality_status===localAcceptedQuality[r.processing_version]&&!r.source_empty:r.daily_mean_vwc_percent===null&&!r.plot_eligible,'Local accepted/withheld value mismatch');
   if(accepted)need(r.mean_percent===r.daily_mean_vwc_percent&&r.mean_value===r.daily_mean_vwc_percent,'Local authoritative value mismatch');
   need(r.expected_samples===null||r.expected_samples>0,'Local positive sample expectation');
   need(r.cadence_seconds===null||r.cadence_seconds>0,'Local positive cadence');
   need(r.valid_sample_count===r.n_valid&&r.observation_count===r.n_total&&r.expected_sample_count===r.expected_samples&&r.sample_count_fraction===r.coverage_fraction&&r.first_last_span_fraction===r.temporal_span_fraction,'Local diagnostic alias mismatch');
   const days=(Date.UTC(r.water_year,9,1)-Date.UTC(r.water_year-1,9,1))/86400000;
   need(r.water_year_days===days&&r.water_day===r.dowy&&r.water_day_aligned===r.plot_day_aligned,'Local calendar alias mismatch');
   // Values already are percentage points. Never multiply, round or fill here.
   return {...r,flags:r.flags.slice(),v:r.daily_mean_vwc_percent,ok:accepted,wy:r.water_year,x:r.plot_day_aligned,n:r.n_valid,expected:r.expected_samples,coverage:r.coverage_fraction,span:r.temporal_span_fraction};
}
function parseLocalCandidateSeries(input){
 need(Array.isArray(input)&&input.length>0&&input.length<=BOUND.streams,'Local candidate stream bound');
 const seen=new Set(),stations=new Map(),series=[];
 for(const original of input){
  exactFields(original,localStreamFields);
  const s=original;
  for(const k of localStreamFields.filter(k=>!['records','depth_cm','conversion_multiplier','orientation','equipment_type_id'].includes(k)))need(typeof s[k]==='string'&&s[k].length>0&&s[k].length<=512,'Local candidate metadata type');
  need(id(s.station_id)&&id(s.stream_id)&&/^[a-f0-9]{64}$/.test(s.source_file_sha256),'Local candidate identity');
  need(new RegExp('^csv-sha256:'+s.source_file_sha256+':column:[1-9][0-9]*$').test(s.export_local_series_key),'Local export identity');
  need(s.provider==='Dendra'&&s.timestamp_semantics==='fixed UTC-08 completed days','Local source/time semantics');
  need(['Percent','VolumetricWaterContent'].includes(s.native_unit)&&s.conversion_multiplier===(s.native_unit==='Percent'?1:100),'Unresolved local unit/normalization');
  need([s.orientation,s.equipment_type_id].every(v=>v===null||typeof v==='string'&&v.length<=512),'Local distinguishing metadata');
  localDepth(s);
  const key=s.export_local_series_key+'|stream:'+s.stream_id;
  need(!seen.has(key),'Duplicate exact local series');seen.add(key);
  need(Array.isArray(s.records)&&s.records.length>0&&s.records.length<=BOUND.selected_rows,'Local selected row bound');
  const rows=s.records.map(r=>{
   return localBulkRow(r,s);
  });
  validateRows(rows,{parameter:'soil_moisture'},'9999-12-31');
  const copy=JSON.parse(JSON.stringify(s)),last=rows.filter(r=>r.ok).at(-1);
  const sensor={...copy,id:key,parameter:'soil_moisture',depth:s.depth_cm,depth_mm:s.depth_cm===null?null:s.depth_cm*10,depth_native:s.depth_cm,depth_unit:'cm',unit:'percent VWC',rows,
   capabilities:{latest_vwc:true,recent_change:false,source_context:false,common_reference:false,companion:false},
   observation:last?{eligible:true,value:last.v,date:last.date,day_offset:'-08:00',statistic:'Daily mean'}:{eligible:false,value:null,date:null,missing_reason:'No accepted completed daily mean'},history:{available:!!last}};
  series.push(sensor);
  let st=stations.get(s.station_id);
  if(!st){st={source:'dendra',id:s.station_id,key:'dendra:'+s.station_id,name:s.station,provider:s.provider,coordinates:null,primary_sensor:null,requires_explicit_sensor:true,streams:[],sensors:[]};stations.set(s.station_id,st);}
  need(st.name===s.station,'Mixed local station identity');st.streams.push(key);st.sensors.push(sensor);
 }
 return {version:LOCAL_CANDIDATE,installed:false,publication_eligible:false,series,stations:[...stations.values()]};
}
function parseLocalCandidate(input){
 exactFields(input,'version installed publication_eligible identity_key date_field value_field depth_nullable daily_states policy examples'.split(' '));
 need(input.version===LOCAL_CANDIDATE&&input.installed===false&&input.publication_eligible===false&&input.depth_nullable===true&&input.policy===POLICY,'Unsupported local candidate contract');
 need(input.identity_key==='export_local_series_key + exact stream_id; NULL depths never merge'&&input.date_field==='date'&&input.value_field==='daily_mean_vwc_percent'&&same(input.daily_states,localStates),'Local candidate field semantics');
 const e=input.examples;need(e&&typeof e==='object'&&!Array.isArray(e)&&e.known_percent&&e.unknown_depth&&Object.keys(e).every(k=>['known_percent','known_fraction','unknown_depth'].includes(k)),'Local fixture examples');
 need(Object.values(e).every(s=>Array.isArray(s.records)&&s.records.length>=1&&s.records.length<=3),'Local fixture row bound');
 return parseLocalCandidateSeries(Object.values(e));
}
// Candidate-3 is an explicit, unpublished component route, not a native index.
// Locators are opaque keys in a caller-pinned finite mirror, never filesystem or
// network permissions. All producer science and reconciliation remain upstream.
const DELIVERY='dendra-candidate3-delivery-1';
const DELIVERY_BOUND=Object.freeze({generation_bytes:1048576,metadata_bytes:262144,partition_bytes:1048576});
const copy=x=>JSON.parse(JSON.stringify(x));
const sha=x=>typeof x==='string'&&/^[a-f0-9]{64}$/.test(x);
const relative=p=>typeof p==='string'&&/^[a-zA-Z0-9_.\/-]+$/.test(p)&&!p.startsWith('/')&&!p.split('/').some(x=>x==='..'||x==='.'||!x);
const descriptor=(d,max)=>need(d&&typeof d.path==='string'&&d.path.length>0&&d.path.length<=4096&&sha(d.sha256)&&Number.isInteger(d.bytes)&&d.bytes>0&&d.bytes<=max,'Invalid delivery descriptor/bound');
const projection=(r,v,ok)=>({date:r.date,v,ok,wy:r.water_year,dowy:r.dowy,x:r.water_day_aligned,n:r.n_valid,expected:r.expected_samples,coverage:r.coverage_fraction,span:r.temporal_span_fraction,flags:[]});
function deliveryUnit(i){
 need(['Percent','VolumetricWaterContent','Dimensionless'].includes(i.native_unit),'Unsupported delivery unit');
 need(i.conversion_multiplier===(i.native_unit==='VolumetricWaterContent'?100:1),'Delivery normalization mismatch');
 return i.native_unit!=='Dimensionless';
}
function deliveryIdentity(i){
 need(i&&id(i.station_id)&&id(i.stream_id)&&typeof i.station_name==='string'&&i.station_name.length>0&&i.station_name.length<=512,'Delivery identity');
 need(/^csv-sha256:[a-f0-9]{64}:column:[1-9][0-9]*$/.test(i.export_local_series_key),'Delivery export identity');
 need(i.accepted_stream_id===null||i.accepted_stream_id===i.stream_id,'Delivery accepted identity');
 need(i.depth_cm===null||finite(i.depth_cm)&&i.depth_cm>=0,'Delivery depth');deliveryUnit(i);
 return i.export_local_series_key+'|stream:'+i.stream_id;
}
function deliveryCalendar(r){
 need(r.water_day===r.dowy&&r.water_year_days===(Date.UTC(r.water_year,9,1)-Date.UTC(r.water_year-1,9,1))/86400000,'Delivery calendar aliases');
 validateRows([projection(r,null,false)],{parameter:'soil_moisture'},'9999-12-31');
}
function interval(x){need(x&&utc(x.start)&&utc(x.end)&&Date.parse(x.start)<Date.parse(x.end),'Invalid query interval');}
function apiIdentity(actual,bulk){
 need(actual&&['station_id','stream_id','depth_cm','native_unit'].every(k=>actual[k]===bulk[k]),'API/bulk identity mismatch');
 need(actual.orientation===null||typeof actual.orientation==='string','API orientation');
 need(actual.unit_status===(deliveryUnit(bulk)?'verified_percent_conversion':'unresolved'),'API scale status mismatch');
}
const apiCounts=localCounts.filter(k=>k.startsWith('n_'));
const apiNumbers='mean_native mean_percent mean_value expected_samples cadence_seconds coverage_fraction temporal_span_fraction'.split(' ');
const apiFields=[...apiCounts,...apiNumbers,'date','water_year','dowy','water_day','water_year_days','water_day_aligned','cadence_source','plot_eligible','flags','representation','identity','query_complete','presentation_eligible','source_intervals'];
function apiRow(r,identity,views,disposition,cutoff){
 exactFields(r,apiFields);need(same(r.identity,identity)&&r.representation==='completed_daily'&&r.query_complete===true,'API row identity/completion');
 for(const k of apiCounts)need(Number.isInteger(r[k])&&r[k]>=0,'API sample count integer');
 for(const k of apiNumbers)need(r[k]===null||finite(r[k]),'API numeric diagnostic');
 need((r.expected_samples===null||r.expected_samples>0)&&(r.cadence_seconds===null||r.cadence_seconds>0)&&typeof r.cadence_source==='string','API cadence/expectation');
 need(typeof r.plot_eligible==='boolean'&&typeof r.presentation_eligible==='boolean'&&Array.isArray(r.flags)&&r.flags.every(x=>typeof x==='string'),'API eligibility/flags');
 deliveryCalendar(r);need(r.date<=cutoff,'API incomplete day');
 const start=Date.parse(r.date+'T08:00:00Z'),end=start+86400000;
 const covering=views.filter(v=>Date.parse(v.start)<end&&Date.parse(v.end)>start);
 let cursor=start;for(const v of covering){need(Date.parse(v.start)<=cursor,'Incomplete API day coverage');cursor=Math.max(cursor,Date.parse(v.end));}need(cursor>=end,'Incomplete API day coverage');
 const intervals=covering.map(v=>Object.fromEntries(['task_id','query_state','seal_record_sha256','content_sha256','parsed_sha256'].map(k=>[k,v.original[k]])));
 need(same(r.source_intervals,intervals),'API row/seal lineage mismatch');
 const empty=covering.every(v=>v.original.query_state==='COVERED_EMPTY');
 if(disposition)need(disposition.state==='DAILY_VALUE_WITHHELD'&&typeof disposition.reason==='string'&&!r.plot_eligible&&!r.presentation_eligible&&r.mean_percent===null,'Unsupported API quality disposition');
 const percent=identity.unit_status==='verified_percent_conversion';
 if(!percent)need(!r.presentation_eligible&&r.mean_percent===null,'Unresolved unit cannot present percent');
 need(!r.presentation_eligible||r.plot_eligible&&finite(r.mean_percent)&&r.mean_percent>=0&&r.mean_percent<=100&&r.n_valid>0&&!empty&&!disposition,'API accepted value mismatch');
 need(r.presentation_eligible?r.mean_value===r.mean_percent:r.mean_percent===null,'API authoritative percent mismatch');
 if(empty)need(r.n_total===0&&r.n_valid===0&&r.mean_native===null&&!r.presentation_eligible,'Empty is not a numeric value');
 return {state:disposition?disposition.state:r.presentation_eligible?'ACCEPTED':'MISSING_OR_REJECTED',valid_empty:empty,display:projection(r,r.presentation_eligible?r.mean_percent:null,r.presentation_eligible)};
}
class Candidate3Reader{
 #components; #options; #read; #crypto; #snapshot=null; #cache={cache:new Map(),cacheBytes:0}; #epoch=0; #abort=new AbortController();
 constructor(options){
  need(options&&typeof options.readBytes==='function'&&Array.isArray(options.components)&&options.components.length>0&&options.components.length<=BOUND.files,'Finite pinned delivery resolver required');
  this.#options=copy({manifest:options.manifest,bridge:options.bridge||null,selection:options.selection});
  need(Array.isArray(options.selection)&&options.selection.length>0&&options.selection.length<=BOUND.streams&&options.selection.every(id)&&new Set(options.selection).size===options.selection.length,'Exact delivery selection required');
  this.#components=new Map();for(const d of options.components){descriptor(d,DELIVERY_BOUND.generation_bytes);need(!this.#components.has(d.path),'Duplicate resolver locator');this.#components.set(d.path,copy(d));}
  this.#read=options.readBytes;this.#crypto=options.crypto||globalThis.crypto;need(this.#crypto?.subtle,'SHA-256 verification unavailable');
  this.metrics={requests:0,bytes:0,cacheHits:0};
 }
 cancel(){this.#abort.abort();this.#abort=new AbortController();this.#epoch++;this.#snapshot=null;this.#cache={cache:new Map(),cacheBytes:0};}
 #guard(epoch,signal){if(epoch!==this.#epoch||signal?.aborted||this.#abort.signal.aborted)throw new DOMException('Cancelled or stale delivery selection','AbortError');}
 async #json(ref,max,epoch,signal){
  this.#guard(epoch,signal);const pin=this.#components.get(ref?.path);descriptor(pin,max);
  need(pin.sha256===ref.sha256&&(ref.bytes===undefined||ref.bytes===pin.bytes),'Unbound/missing delivery component');
  const abort=this.#abort,external=()=>abort.abort();signal?.addEventListener('abort',external,{once:true});
  let b;try{this.metrics.requests++;b=await this.#read(copy(pin),abort.signal,Math.min(max,pin.bytes));}finally{signal?.removeEventListener('abort',external);}
  this.#guard(epoch,signal);need(b instanceof Uint8Array||b instanceof ArrayBuffer,'Delivery byte response required');
  const bytes=b instanceof Uint8Array?b:new Uint8Array(b);need(bytes.byteLength===pin.bytes,'Delivery byte count mismatch');
  const hash=Array.from(new Uint8Array(await this.#crypto.subtle.digest('SHA-256',bytes))).map(x=>x.toString(16).padStart(2,'0')).join('');
  this.#guard(epoch,signal);need(hash===pin.sha256,'Delivery hash mismatch');this.metrics.bytes+=bytes.byteLength;
  return JSON.parse(new TextDecoder('utf-8',{fatal:true}).decode(bytes));
 }
 async loadCatalog(signal){
  this.cancel();const epoch=this.#epoch,o=this.#options,g=await this.#json(o.manifest,DELIVERY_BOUND.generation_bytes,epoch,signal);
  need(g.schema_version===DELIVERY&&g.kind==='generation'&&g.candidate_id==='dendra-00g-local-candidate-3'&&g.publication_eligible===false&&g.publication_state==='offline_export_only'&&sha(g.generation_sha256)&&sha(g.candidate_manifest_sha256),'Unsupported Candidate-3 generation');
  need(Array.isArray(g.files)&&g.files.length<=BOUND.files,'Delivery file inventory bound');const files=new Map();for(const d of g.files){need(relative(d.path)&&!files.has(d.path),'Unsafe/duplicate delivery inventory');descriptor(d,DELIVERY_BOUND.generation_bytes);files.set(d.path,d);}
  const member=d=>{need(d&&same(files.get(d.path),d),'Delivery inventory membership mismatch');return d;};
  const binding=await this.#json(member(g.source_binding),DELIVERY_BOUND.generation_bytes,epoch,signal);
  need(binding.schema_version===DELIVERY&&binding.kind==='source_binding'&&binding.candidate_manifest?.sha256===g.candidate_manifest_sha256&&binding.daily_policy===POLICY&&binding.timestamp_semantics==='fixed UTC-08 completed daily; ending-year WY; true DOWY; separate leap alignment'&&binding.row_schema?.additionalProperties===false&&same(Object.keys(binding.row_schema.properties).sort(),[...localRowFields].sort()),'Delivery source binding mismatch');
  const index=await this.#json(member(g.station_index),DELIVERY_BOUND.metadata_bytes,epoch,signal);
  need(index.schema_version===DELIVERY&&index.kind==='station_index'&&Array.isArray(index.stations)&&index.stations.length<=BOUND.stations,'Delivery station index');
  const inventory=new Map(),stationIds=new Set();for(const st of index.stations){need(id(st.station_id)&&!stationIds.has(st.station_id)&&Array.isArray(st.streams),'Duplicate delivery station');stationIds.add(st.station_id);for(const s of st.streams){need(id(s.stream_id)&&!inventory.has(s.stream_id),'Duplicate delivery stream');member(s.descriptor);inventory.set(s.stream_id,{...s,station_id:st.station_id});}}
  need(inventory.size<=BOUND.streams&&g.census?.streams===inventory.size&&g.census.stations===stationIds.size,'Delivery census mismatch');
  const series=[],stations=new Map(),descriptors=new Map();
  for(const sid of o.selection){const entry=inventory.get(sid);need(entry,'Selected delivery stream absent');const d=await this.#json(entry.descriptor,DELIVERY_BOUND.metadata_bytes,epoch,signal),i=d.identity;
   need(d.schema_version===DELIVERY&&d.kind==='stream'&&i?.stream_id===sid&&i.station_id===entry.station_id,'Delivery stream identity');const key=deliveryIdentity(i),percent=deliveryUnit(i);
   localDepth({...i,depth_status:d.catalog_depth_status});need(Array.isArray(d.history)&&d.history.length>0&&d.history.length<=BOUND.years_per_stream,'Delivery WY bound');let year=0,count=0;
   for(const h of d.history){need(Number.isInteger(h.water_year)&&h.water_year>year&&Number.isInteger(h.rows)&&h.rows>0&&h.rows<=366,'Delivery WY identity');member(h.file);need(h.file.path===`history/${i.station_id}/${sid}/wy-${h.water_year}.${h.file.sha256}.json`,'Delivery WY path mismatch');year=h.water_year;count+=h.rows;}
   need(count===d.rows&&count<=BOUND.selected_rows&&entry.descriptor.path===`streams/${i.station_id}/${sid}.${entry.descriptor.sha256}.json`,'Delivery row/descriptor closure');descriptors.set(key,d);
   const s={...i,id:key,parameter:'soil_moisture',depth_status:d.catalog_depth_status,depth:i.depth_cm,depth_mm:i.depth_cm===null?null:i.depth_cm*10,depth_native:i.depth_cm,depth_unit:'cm',unit:percent?'percent VWC':i.native_unit,orientation:null,rows:[],history:{available:true},capabilities:{latest_vwc:percent,recent_change:false,source_context:false,common_reference:false,companion:false}};
   series.push(s);let st=stations.get(i.station_id);if(!st){st={source:'dendra',id:i.station_id,key:'dendra:'+i.station_id,name:i.station_name,provider:'Dendra',coordinates:null,primary_sensor:null,requires_explicit_sensor:true,streams:[],sensors:[]};stations.set(i.station_id,st);}need(st.name===i.station_name,'Delivery station name mismatch');st.streams.push(key);st.sensors.push(s);
  }
  let bridge=null;if(o.bridge){bridge=await this.#json(o.bridge,DELIVERY_BOUND.metadata_bytes,epoch,signal);need(bridge.schema_version==='dendra-candidate3-routine-1'&&bridge.publication_eligible===false&&bridge.baseline?.path===o.manifest.path&&bridge.baseline.sha256===o.manifest.sha256&&bridge.candidate_manifest_sha256===g.candidate_manifest_sha256&&sha(bridge.bridge_id)&&bridge.changes&&typeof bridge.changes==='object','Mixed/unsupported delivery bridge');}
  const catalog={version:DELIVERY,generation:g.generation_sha256,installed:false,publication_eligible:false,partial_scope:true,series,stations:[...stations.values()]};
  this.#guard(epoch,signal);this.#snapshot={catalog,g,binding,descriptors,bridge};return copy(catalog);
 }
 async #api(snapshot,d,epoch,signal){
  const bridge=snapshot.bridge,sid=d.identity.stream_id,change=bridge?.changes[sid]||null;
  const empty={rows:[],update:change,coverage:null,api_generation:null,prepared:null,lineage:null,admission:null,views:[],quality:[]};
  if(!bridge?.api){need(!change,'Update without admitted API snapshot');return empty;}
  const a=await this.#json(bridge.api,DELIVERY_BOUND.metadata_bytes,epoch,signal);need(a.schema_version==='dendra-routine-generation-1'&&a.publication_eligible===false&&sha(a.generation_id),'Unsupported API generation');
  const s=a.streams?.[sid];if(!s){need(!change,'Update without API stream');return empty;}apiIdentity(s.identity,d.identity);
  need(change&&same(change.coverage,s.coverage)&&change.outcome===s.outcome&&same(change.last_successful_complete_query,s.last_successful_complete_query)&&change.last_attempted_source_check===s.last_attempted_source_check,'Bridge/API update mismatch');
  need(['STREAM_UPDATE_SUCCESS','KEEP_PRIOR_ACKNOWLEDGED_HISTORY'].includes(s.outcome)&&Array.isArray(change.material_changed_dates)&&Array.isArray(change.routine_recomputed_dates),'Unsupported API outcome');
  need(change.last_attempted_source_check===null||utc(change.last_attempted_source_check),'Invalid source check');
  need(s.coverage&&Array.isArray(s.coverage.gaps)&&Array.isArray(s.coverage.queried_empty)&&Array.isArray(s.views)&&s.views.length<=BOUND.selected_rows,'API query coverage');
  [...s.coverage.gaps,...s.coverage.queried_empty].forEach(interval);let previous=-Infinity;
  for(const v of s.views){interval(v);const original=v.original;need(Date.parse(v.start)>=previous&&original&&['COMPLETE_NONEMPTY','COVERED_EMPTY'].includes(original.query_state)&&Date.parse(v.start)>=Date.parse(original.start)&&Date.parse(v.end)<=Date.parse(original.end),'Conflicting/incomplete admitted API views');previous=Date.parse(v.end);
   need(['content_sha256','parsed_sha256','seal_record_sha256','task_id'].every(k=>sha(original[k]))&&v.task_id===original.task_id&&a.sources?.[v.source]?.sha256===v.source,'Unbound API seal/source');
   need(!s.coverage.gaps.some(x=>Date.parse(x.start)<Date.parse(v.end)&&Date.parse(x.end)>Date.parse(v.start)),'API coverage gap overlaps admitted view');
  }
  need(same(s.coverage.queried_empty,s.views.filter(v=>v.original.query_state==='COVERED_EMPTY').map(v=>({start:v.start,end:v.end}))),'API empty coverage mismatch');
  const daily=a.daily;need(daily&&sha(daily.fingerprint)&&same(s.last_prepared_candidate,daily)&&Array.isArray(daily.pins)&&daily.pins.length<=BOUND.files,'API prepared binding');
  const pins=new Map();for(const pin of daily.pins){need(relative(pin.path)&&!pins.has(pin.path),'Prepared component path');descriptor(pin,DELIVERY_BOUND.generation_bytes);pins.set(pin.path,pin);}
  const preparedRef=name=>{const pin=pins.get(name);need(pin,'Missing prepared component');return {...pin,path:daily.root+'/'+pin.path};};
  const prepared=preparedRef('daily-output.json'),lineageRef=preparedRef('lineage/'+sid+'.json');
  const output=await this.#json(prepared,DELIVERY_BOUND.generation_bytes,epoch,signal),lineage=await this.#json(lineageRef,DELIVERY_BOUND.metadata_bytes,epoch,signal);
  need(output.schema_version==='dendra-sealed-daily-handoff-4'&&output.daily_schema===DAILY&&output.numerical_policy===POLICY&&output.publication_eligible===false&&output.science_binding?.collector_fingerprint===daily.fingerprint,'Unsupported prepared daily policy');
  need(lineage.schema_version==='dendra-routine-view-lineage-1'&&lineage.generation_sha256===s.last_routine_generation?.sha256&&same(lineage.identity,s.identity)&&same(lineage.views,s.views),'Prepared API lineage mismatch');
  const routine=await this.#json(s.last_routine_generation,DELIVERY_BOUND.metadata_bytes,epoch,signal);
  need(routine.schema_version==='dendra-routine-generation-1'&&routine.publication_eligible===false&&same(routine.streams?.[sid]?.views,s.views)&&same(routine.streams[sid].identity,s.identity),'Routine admission mismatch');
  const admission=routine.changes?.[sid]||null;
  if(s.outcome==='KEEP_PRIOR_ACKNOWLEDGED_HISTORY')need(admission?.status==='REPLACEMENT_NOT_ADMITTED'&&change.material_changed_dates.length===0,'Unadmitted replacement state mismatch');
  need(Array.isArray(output.rows?.[sid])&&output.rows[sid].length<=BOUND.selected_rows&&Array.isArray(output.quality_disposition?.[sid]),'Prepared rows/disposition missing');
  const quality=output.quality_disposition[sid],dispositions=new Map();for(const q of quality){need(!dispositions.has(q.date),'Duplicate quality disposition');dispositions.set(q.date,q);}
  let date='';const rows=output.rows[sid].map(record=>{need(record.date>date,'Duplicate/unsorted API date');date=record.date;const result=apiRow(record,s.identity,s.views,dispositions.get(record.date),output.completed_fixed_pst_cutoff);return {record,...result};});
  need(quality.every(q=>rows.some(r=>r.record.date===q.date)),'Orphan quality disposition');
  // A missing prepared day inside admitted complete coverage is unavailable,
  // not permission to fall back to a superseded bulk value or manufacture null.
  const dates=new Set(rows.map(r=>r.record.date));let covered=0;
  for(const v of s.views){const start=Date.parse(v.start),end=Date.parse(v.end);for(let day=Math.ceil((start-28800000)/86400000)*86400000+28800000;day+86400000<=end;day+=86400000){need(++covered<=BOUND.selected_rows,'API covered-day bound');need(dates.has(new Date(day-28800000).toISOString().slice(0,10)),'Missing prepared covered day');}}
  for(const dates of [change.material_changed_dates,change.routine_recomputed_dates])need(new Set(dates).size===dates.length&&dates.every(date=>rows.some(r=>r.record.date===date)),'Update dates lack prepared evidence');
  return {rows,update:change,coverage:s.coverage,api_generation:bridge.api,prepared,lineage:lineageRef,admission,views:s.views,quality,identity:s.identity};
 }
 #key(key,years){need(typeof key==='string'&&Array.isArray(years)&&years.length>0&&years.length<=BOUND.years_per_stream&&years.every(Number.isInteger)&&new Set(years).size===years.length,'Explicit exact series/WYs required');return key+':'+[...years].sort((a,b)=>a-b).join(',');}
 async history(key,years,signal){
  const cacheKey=this.#key(key,years),epoch=this.#epoch,snapshot=this.#snapshot;need(snapshot,'Load delivery catalog first');this.#guard(epoch,signal);
  if(this.#cache.cache.has(cacheKey)){this.metrics.cacheHits++;return JSON.parse(this.#cache.cache.get(cacheKey)).rows;}
  const d=snapshot.descriptors.get(key);need(d,'Unknown exact delivery series');const i=d.identity,records=new Map(),percent=deliveryUnit(i);
  for(const h of d.history.filter(h=>years.includes(h.water_year))){const p=await this.#json(h.file,DELIVERY_BOUND.partition_bytes,epoch,signal);
   need(p.schema_version===DELIVERY&&p.kind==='history'&&p.station_id===i.station_id&&p.stream_id===i.stream_id&&p.water_year===h.water_year&&Array.isArray(p.records)&&p.records.length===h.rows,'Bulk partition identity/count');
   const checked=p.records.map((r,n)=>{const row=localBulkRow(r,{...i,station:i.station_name,depth_status:d.catalog_depth_status});need(r.water_year===h.water_year,'Mixed bulk WY');if(!percent)need(r.daily_status==='UNRESOLVED_SEMANTICS'&&r.mean_percent===null&&!r.plot_eligible,'Unresolved bulk percent');
    const display=projection(r,percent?row.v:null,percent&&row.ok);records.set(r.date,{authority:'CANDIDATE3_BULK',record:r,state:r.daily_status,valid_empty:false,bulk:{record:r,component:h.file,row_index:n,manifest:this.#options.manifest},api:null,display});return display;});
   validateRows(checked,{parameter:'soil_moisture'},'9999-12-31');
  }
  const api=await this.#api(snapshot,d,epoch,signal);
  for(const item of api.rows.filter(x=>years.includes(x.record.water_year))){const r=item.record,prior=records.get(r.date);records.set(r.date,{authority:'SEALED_API',record:r,state:item.state,valid_empty:item.valid_empty,bulk:prior?.bulk||null,api:{component:api.prepared,lineage:api.lineage,source_intervals:r.source_intervals},display:item.display});}
  for(const year of years)need(d.history.some(h=>h.water_year===year)||api.rows.some(r=>r.record.water_year===year),'Requested WY unavailable');
  need(records.size<=BOUND.selected_rows,'Selected delivery row bound');const selected=[...records.values()].sort((a,b)=>a.record.date.localeCompare(b.record.date)),rows=selected.map(x=>x.display);
  validateRows(rows,{parameter:'soil_moisture'},'9999-12-31');const state={schema_version:DELIVERY,generation:snapshot.g.generation_sha256,candidate_manifest_sha256:snapshot.g.candidate_manifest_sha256,manifest:this.#options.manifest,bridge:this.#options.bridge,identity:i,depth_status:d.catalog_depth_status,scale_state:percent?'verified_percent_conversion':'unresolved',api_generation:api.api_generation,update:api.update,coverage:api.coverage,admission:api.admission,query_views:api.views,quality_disposition:api.quality,latest_instantaneous:null,publication_eligible:false,records:selected.map(({display,...rest})=>rest)};
  const text=JSON.stringify({rows,state});need(new TextEncoder().encode(text).length<=BOUND.cache_bytes,'Delivery snapshot cache bound');this.#guard(epoch,signal);Reader.prototype.cachePut.call(this.#cache,cacheKey,text);return copy(rows);
 }
 getState(key,years){this.#guard(this.#epoch);const text=this.#cache.cache.get(this.#key(key,years));need(text,'Requested delivery state unavailable/evicted');return JSON.parse(text).state;}
 get cacheInfo(){return {entries:this.#cache.cache.size,bytes:this.#cache.cacheBytes};}
}
function createReader(contract,options){
 if(contract===DELIVERY)return new Candidate3Reader(options);
 need([SCHEMA,ARCHIVE].includes(contract),'Unsupported Dendra reader contract');
 return new Reader(options.indexUrl,options.fetcher,options.crypto);
}
return {Reader,validateIndex,validateRows,freshness,unit,BOUND,parseLocalCandidate,parseLocalCandidateSeries,createReader,Candidate3Reader,DELIVERY,DELIVERY_BOUND};
});
