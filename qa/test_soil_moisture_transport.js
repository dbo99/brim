const assert=require('node:assert/strict'),fs=require('node:fs'),path=require('node:path');
const T=require('../03_functions/js/soil_moisture_transport.js');
const dir=path.resolve(process.argv[2]),baseline=path.resolve(process.argv[3]),metrics=[];
const index=JSON.parse(fs.readFileSync(path.join(dir,'scan.json')));
for(const station of index.stations){
 const d=T.descriptor(station.bundle),p=path.join(dir,d.path),b=fs.readFileSync(p),old=fs.readFileSync(path.join(baseline,d.path));
 const beforeStart=performance.now(),beforeParsed=JSON.parse(old),beforeParse=performance.now()-beforeStart;
 const a=performance.now(),body=JSON.parse(b),parsed=performance.now(),decoded=T.decodeScan(body,station.id),end=performance.now();
 assert.deepEqual(decoded,beforeParsed,station.id+' complete lossless feature/table equality');
 metrics.push({station:station.id,name:station.name,before_bytes:old.length,after_bytes:b.length,before_parse_ms:beforeParse,parse_ms:parsed-a,decode_ms:end-parsed});
}
assert.equal(metrics.length,28);
const original=JSON.parse(fs.readFileSync(path.join(dir,'scan-2229.json')));
for(const mutate of [b=>b.schema='wrong',b=>b.feature.properties.station_triplet='other',
 b=>b.tables['scan_soil_moisture_current_wy_trace.csv'].columns[0]='unexpected_field',
 b=>b.tables['scan_soil_moisture_current_wy_trace.csv'].columns.pop(),
 b=>b.tables.extra={},b=>b.tables['scan_depth_style.csv'].columns.push('depth_in'),
 b=>b.tables['scan_depth_style.csv'].rows[0].pop(),b=>b.tables['scan_depth_style.csv'].rows[0][0]=null,
 b=>b.tables['scan_depth_style.csv'].columns[0]='__proto__']){
 const b=structuredClone(original);mutate(b);assert.throws(()=>T.decodeScan(b,'2229:NV:SCAN'));
}
for(const d of [{path:'../bad.json',bytes:3,sha256:'a'.repeat(64)},
 {path:'a.json',bytes:6000001,sha256:'a'.repeat(64)},
 {path:'a.json',bytes:0,sha256:'a'.repeat(64)},
 {path:'a.json',bytes:3,sha256:'bad'}])assert.throws(()=>T.descriptor(d));
const cache=new T.BundleCache();for(let n=0;n<10;n++)cache.set('x'+n,{n},2000000);
assert.equal(cache.size,6);assert.equal(cache.bytes,12000000);assert.equal(cache.get('x0'),undefined);
cache.get('x4');cache.set('new',{},3000000);assert.equal(cache.get('x5'),undefined);assert(cache.get('x4'));
cache.clear();for(let n=0;n<10;n++)cache.set('s'+n,{},10);assert.equal(cache.size,8);cache.clear();assert.equal(cache.bytes,0);
console.log(JSON.stringify({passed:true,stations:28,complete_table_fidelity:true,malformed_and_bound_cases:13,cache_byte_and_entry_lru:true,limits:T.limits,station_costs:metrics},null,2));
