/* Actual consumer validation for every advertised local prepared file.
 * Disk-backed fetch only; never opens a provider or static-server connection. */
const fs=require('node:fs'),path=require('node:path'),vm=require('node:vm');
const assert=require('node:assert/strict'),crypto=require('node:crypto').webcrypto;
const root=path.resolve(process.argv[2]);
const T=require('../03_functions/js/soil_moisture_transport.js');
const {Reader}=require('../03_functions/js/dendra_reader.js');
const {environment}=require('./test_soil_popup_presentation.js');
// Reuse the established offline document/map context and real production UI.
// The controller keeps its normal popup-priority binding; product I/O below
// still uses the existing disk-backed readers and verification checks.
const {context}=environment(root);
vm.runInContext(fs.readFileSync(path.join(__dirname,'../03_functions/js/soil_moisture_controller.js'),'utf8'),context);
const api=vm.runInContext('createSoilMoistureController()',context),checked=[];
async function verified(rel,max,descriptor){
 const p=path.resolve(root,rel);assert(p.startsWith(root+path.sep));
 const size=fs.statSync(p).size;assert(size<=max);const b=fs.readFileSync(p);assert(b.length>0&&b.length<=max,rel+' byte bound');
 const sha=Buffer.from(await crypto.subtle.digest('SHA-256',b)).toString('hex');
 if(descriptor){assert.equal(b.length,descriptor.bytes);assert.equal(sha,descriptor.sha256);}
 checked.push({path:rel,bytes:b.length,sha256:sha});return b;
}
(async()=>{
 let expectedGeneration;
 for(const source of ['scan','dendra','snotel']){
  const i=await api.expandIndex(JSON.parse(await verified(source+'.json',T.limits.index)),source,async d=>JSON.parse(await verified(d.path,T.limits.index,d)));
  if(source==='dendra'){expectedGeneration=i.generation;continue;}
  for(const station of i.stations){const d=T.descriptor(station.bundle),b=await verified(d.path,T.limits.bundle,d);T.decode(JSON.parse(b),source,station.id);}
 }
 const fetcher=async url=>{const u=new URL(url);assert.equal(u.origin,'http://saved.invalid');const rel=u.pathname.slice(1),b=fs.readFileSync(path.join(root,rel));return new Response(b);};
 const reader=new Reader('http://saved.invalid/dendra/index.json',fetcher,crypto),index=await reader.loadIndex();
 await verified('dendra/index.json',256000);assert.equal(index.generation,expectedGeneration);
 let rows=0;
 for(const stream of index.streams){rows+=(await reader.history(index,stream,undefined,'all')).length;await reader.csv(index,stream);}
 let files=index.files;if(index.schema_version==='dendra-daily-2.0.0'){files=[];for(const d of index.file_pages){const p=JSON.parse(await verified('dendra/'+d.path.slice('docs/data/dendra/'.length),262144,d));files.push(...p.files);}}
 for(const file of files){const rel='dendra/'+file.path.slice('docs/data/dendra/'.length),b=await verified(rel,11999999,file);if(rel.endsWith('.json'))JSON.parse(b);}
 assert.equal(rows,index.streams.reduce((n,s)=>n+s.summary.calendar_row_count,0));
 console.log(JSON.stringify({passed:true,scope:'Saved common/native consumer preflight',provider_requests:0,files:checked,Dendra_rows:rows,limits:T.limits}));
})().catch(error=>{console.error(error);process.exitCode=1;});
