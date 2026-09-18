"use strict";
// Installed-library source/offline contract; geometry and transport are modeled.
// Usage: node qa/test_ops_radar_leaflet_contract.js /path/to/installed/leaflet.js
// No provider requests, browser, dependency installation or library vendoring.
const fs=require('node:fs'),vm=require('node:vm'),assert=require('node:assert/strict'),crypto=require('node:crypto');
function main() {
  if (process.argv.length !== 3 || !process.argv[2]) {
    throw new Error('Usage: node qa/test_ops_radar_leaflet_contract.js <installed-leaflet.js>');
  }
  const leafletPath=process.argv[2];
  if (!fs.statSync(leafletPath).isFile()) throw new Error('Installed Leaflet input must be a regular JavaScript file');
  // Imported QA validates its own CLI flags during require. Isolate this
  // entry point's library argument only during those synchronous imports,
  // and restore the original argv even if an import fails.
  const savedArgv=process.argv;
  let fixture,mockElement;
  try {
    process.argv=savedArgv.slice(0,2);
    ({fixture}=require('./test_ops_radar_qpe_contracts.js'));
    ({mockElement}=require('./test_springs_virtualized.js'));
  } finally { process.argv=savedArgv; }
const lib=fs.readFileSync(leafletPath),sha=b=>crypto.createHash('sha256').update(b).digest('hex');
const el=tag=>Object.assign(mockElement(tag),{getContext:()=>({})});
const context={window:{screen:{deviceXDPI:1,logicalXDPI:1},devicePixelRatio:1,addEventListener(){},setTimeout,clearTimeout},document:{documentElement:el('html'),createElement:el,createElementNS:(_ns,tag)=>el(tag),addEventListener(){}},navigator:{userAgent:'offline contract fixture',platform:'Mac'},setTimeout,clearTimeout,console};
vm.createContext(context);vm.runInContext(lib.toString(),context);const L=context.L||context.window.L;
assert(L && typeof L.version==='string' && L.Layer && L.tileLayer && L.tileLayer.wms,'Input must expose the installed Leaflet Layer and WMS contracts');
console.log('INSTALLED_LEAFLET='+JSON.stringify({version:L.version,bytes:lib.length,sha256:sha(lib)}));
const f=fixture(),name='Radar | IEM NEXRAD',owner=f.context.opsDefByName[name].layer;
const realOwner=new (L.Layer.extend({}))();assert.equal(typeof realOwner.fire,'function');
assert.equal(typeof owner.fire,'function','Fixture L.Layer must inherit the relevant Leaflet Evented surface');
for(const layer of [realOwner,owner]){
 const calls=[],a=function(e){assert.equal(this,layer);assert.equal(e.target,layer);calls.push('a');},b=()=>calls.push('b');
 assert.equal(layer.on('contract',a),layer);layer.on('contract',b);assert.equal(layer.fire('contract'),layer);
 assert.deepEqual(calls,['a','b']);assert.equal(layer.off('contract',a),layer);layer.fire('contract');assert.deepEqual(calls,['a','b','b']);layer.off('contract',b);
}
console.log('PASS installed Layer Evented and modeled relevant on/off/fire contract agree');
let native,removed=0;
f.context.L.tileLayer.wms=(url,options)=>{
 native=L.tileLayer.wms(url,options);
 // Geometry/transport are suppressed; real installed WMS construction and Evented remain.
 native.onAdd=function(){};native.onRemove=function(){removed++;};return native;
};
f.context.ptOpsActivateLayerByName(name);assert.equal(owner._tiles,native);
assert.equal(native._url,'https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?');
for(const key of ['layers','format','transparent','opacity','pane','attribution','sourceUrl','legendUrl','infoUrl','infoLabel','legendNote'])assert.equal(native.options[key],owner.options[key],key);
assert.equal(native.wmsParams.layers,'nexrad-n0q-900913');assert.equal(native.wmsParams.format,'image/png');assert.equal(native.wmsParams.transparent,true);
for(const key of ['name','radarProductId','legendType','note'])assert(!Object.hasOwn(native.wmsParams,key),key+' leaked into WMS');
// Build one URL without image creation or fetch using installed WMS projection code.
native._crs=L.CRS.EPSG3857;native._wmsVersion=1.1;native._tileZoom=5;native.wmsParams.srs='EPSG:3857';native._globalTileRange={max:{y:2**native._tileZoom-1}};native._map={options:{crs:L.CRS.EPSG3857},unproject:(p,z)=>L.CRS.EPSG3857.pointToLatLng(p,z)};
const url=native.getTileUrl(Object.assign(L.point(5,12),{z:5}));const params=new URL(url).searchParams;
assert.equal(params.get('layers'),'nexrad-n0q-900913');assert.equal(params.get('format'),'image/png');assert.equal(params.get('transparent'),'true');assert.equal(params.get('srs'),'EPSG:3857');assert(params.get('bbox'));
console.log('PASS installed native WMS exposed/request agreement and no owner-only parameters');
console.log('OFFLINE_REQUEST='+url);
native.fire('loading');assert(f.context.isOpsLayerLoading(name));native.fire('load');assert(!f.context.isOpsLayerLoading(name));assert.equal(f.context.statusRows[name].msg,'Radar tiles loaded.');
native.fire('loading');native.fire('tileerror');assert(!f.context.isOpsLayerLoading(name));assert.match(f.context.statusRows[name].msg,/failed/);
const old=native,callbacks={...owner._tileHandlers};let removeEvent=0,zoomSentinel=0,otherLoad=0;
old.on('remove',()=>removeEvent++);old.on('zoom',()=>zoomSentinel++);old.on('load',()=>otherLoad++);
f.context.ptOpsDeactivateLayerByName(name);assert.equal(removed,1);assert.equal(removeEvent,1);old.fire('zoom');old.fire('load');assert.equal(zoomSentinel,1);assert.equal(otherLoad,1);
f.context.ptOpsActivateLayerByName(name);assert.notEqual(native,old);const before=JSON.stringify(f.context.statusRows);Object.values(callbacks).forEach(cb=>cb());old.fire('loading');old.fire('load');old.fire('tileerror');assert.equal(JSON.stringify(f.context.statusRows),before);assert(f.context.isOpsLayerLoading(name));
f.context.ptOpsDeactivateLayerByName(name);assert.equal(removed,2);
console.log('PASS installed native Evented callbacks, selective teardown and obsolete closure rejection');
console.log('RESULT passed=3 failed=0; browser=NOT_RUN; geometry_transport=MODELED_NO_NETWORK');

}
try { main(); } catch (error) {
  console.error('LEAFLET_CONTRACT_ERROR: '+error.message);
  process.exitCode=1;
}
