#!/usr/bin/env node
'use strict';
// E31 source/offline contract. No provider requests, browser, or native build.
const fs = require('fs'), path = require('path'), assert = require('assert'), vm = require('vm');
const root = path.resolve(__dirname, '..');
const read = name => fs.readFileSync(path.join(root, name), 'utf8');
let passed = 0, failed = 0;
const tests=[]; function test(name, fn) { tests.push([name,fn]); }
const definitions = read('03_functions/leaflet_ops_live_layer_definition_helpers.r');
test('five approved GIBS registrations', () => assert.equal((definitions.match(/layer: makeGibsImageryLayer\(/g) || []).length, 5));
test('Water Vapor retired from active registration and identity', () => assert(!definitions.includes('ops_goes_water_vapor')));
test('two unique new VIIRS identities', () => ['ops_viirs_noaa20_true_color','ops_viirs_noaa21_true_color'].forEach(id => assert.equal(definitions.split('"' + id + '"').length - 1, 1)));
test('focused family helper assembled', () => assert(read('03_functions/leaflet_ops_live_helpers.r').includes('pt_ops_live_gibs_js')));
test('satellite matrix above choices', () => assert(read('03_functions/leaflet_ops_live_panel_helpers.r').includes('GOES-West imagery matrix')));

const crypto=require("crypto");
const hash=x=>crypto.createHash("sha256").update(x).digest("hex");
const bridge=String.raw`import sys,json,xml.etree.ElementTree as E
def node(e):
 ns,_,local=e.tag[1:].partition('}') if e.tag.startswith('{') else ('','',e.tag)
 return dict(localName=local,namespaceURI=ns,textContent=''.join(e.itertext()),attrs=e.attrib,children=[node(c) for c in e])
try: root=E.fromstring(sys.stdin.read())
except E.ParseError as e:
 print(str(e),file=sys.stderr);sys.exit(2)
print(json.dumps(node(root)))`;
function run(code,input,cap){
 const r=require('child_process').spawnSync('/usr/bin/python3',['-B','-c',code],{input,encoding:'utf8',maxBuffer:cap});
 if(r.error)throw Error('Offline bridge process/buffer failure: '+r.error.message);
 if(r.status!==0)throw Error((r.status===2?'Malformed XML: ':'Offline bridge failure: ')+r.stderr);
 return r.stdout;
}
function parseXML(xml){
 const raw=run(bridge,xml,64*1024*1024);
 function wrap(x){return {...x,nodeType:1,childNodes:x.children.map(wrap),getAttribute:k=>x.attrs[k] ?? null};}
 return {documentElement:wrap(JSON.parse(raw)),getElementsByTagName:()=>[]};
}

function event(type, values = {}) {
  return {type, preventDefault() { this.defaultPrevented = true; },
    stopPropagation() { this.stopped = true; }, ...values};
}
function events(target) {
  const listeners = [];
  target.addEventListener = (type, f, capture = false) => {
    if (!listeners.some(x => x.type === type && x.f === f && x.capture === capture))
      listeners.push({type, f, capture});
  };
  target.removeEventListener = (type, f, capture = false) => {
    const i = listeners.findIndex(x => x.type === type && x.f === f && x.capture === capture);
    if (i >= 0) listeners.splice(i, 1);
  };
  target.dispatchEvent = e => {
    e.target = e.target || target;
    listeners.filter(x => x.type === e.type).slice().forEach(x => x.f.call(target, e));
    if (!e.stopped && e.bubbles && target.parentNode) target.parentNode.dispatchEvent(e);
    return !e.defaultPrevented;
  };
  target.listenerCount = () => listeners.length;
  return target;
}
function style() {
  const values = {};
  const api = {
    getPropertyValue: k => values[k] || "",
    setProperty: (k, v) => { values[k] = String(v); },
    removeProperty: k => { delete values[k]; }
  };
  Object.defineProperty(api, "cssText", {get: () => Object.entries(values).map(x => x.join(":")).join(";"),
    set: text => {
      Object.keys(values).forEach(k => delete values[k]);
      String(text).split(";").forEach(v => {
        const i = v.indexOf(":"); if (i > 0) values[v.slice(0, i).trim()] = v.slice(i + 1).trim();
      });
    }});
  return new Proxy(api, {get: (o, k) => k in o ? o[k] : values[k] || "",
    set: (o, k, v) => { if (k in o) o[k] = v; else values[k] = String(v); return true; }});
}
const decodeText = s => s.replace(/&(?:amp|lt|gt|quot|#039);/g,
  v => ({"&amp;": "&", "&lt;": "<", "&gt;": ">", "&quot;": '"', "&#039;": "'"}[v]));
function node(tag = "div") {
  const attrs = {}, n = events({nodeType: 1, tagName: tag.toUpperCase(), childNodes: [],
    parentNode: null, style: style(), scrollTop: 0});
  Object.defineProperties(n, {
    parentElement: {get: () => n.parentNode},
    children: {get: () => n.childNodes.filter(c => c.nodeType === 1)},
    nextSibling: {get: () => n.parentNode?.childNodes[n.parentNode.childNodes.indexOf(n) + 1] || null},
    previousSibling: {get: () => n.parentNode?.childNodes[n.parentNode.childNodes.indexOf(n) - 1] || null},
    className: {get: () => attrs.class || "", set: v => { attrs.class = v; }},
    id: {get: () => attrs.id || "", set: v => { attrs.id = v; }},
    textContent: {get: () => n.childNodes.map(c => c.textContent).join(""),
      set: v => { n.childNodes.forEach(c => { c.parentNode = null; });
        n.childNodes = [{nodeType: 3, textContent: String(v), parentNode: n}]; }},
    innerHTML: {get: () => n._html || "", set: html => {
      n._html = String(html); n.childNodes.forEach(c => { c.parentNode = null; }); n.childNodes = [];
      const stack = [n];
      for (const m of String(html).matchAll(/<\/?[^>]+>|[^<]+/g)) {
        const t = m[0], parent = stack[stack.length - 1];
        if (t.startsWith("</")) { assert(stack.length > 1, "HTML close mismatch"); stack.pop(); }
        else if (t.startsWith("<")) {
          const tagMatch = /^<([\w-]+)/.exec(t); assert(tagMatch, "Unsupported HTML");
          const el = node(tagMatch[1]);
          for (const a of t.matchAll(/([\w-]+)="([^"]*)"/g)) el.setAttribute(a[1], decodeText(a[2]));
          parent.appendChild(el);
          if (!/^(BR|INPUT|IMG|HR)$/.test(el.tagName) && !t.endsWith("/>")) stack.push(el);
        } else parent.childNodes.push({nodeType: 3, textContent: decodeText(t), parentNode: parent});
      }
      assert.strictEqual(stack.length, 1, "HTML unclosed element");
    }}
  });
  n.classList = {
    contains: c => n.className.split(/\s+/).includes(c),
    add: (...cs) => { n.className = [...new Set(n.className.split(/\s+/).filter(Boolean).concat(cs))].join(" "); },
    remove: (...cs) => { n.className = n.className.split(/\s+/).filter(c => !cs.includes(c)).join(" "); },
    toggle: (c, on) => { if (on === undefined) on = !n.classList.contains(c); n.classList[on ? "add" : "remove"](c); }
  };
  n.setAttribute = (k, v) => { attrs[k] = String(v); if (k === "style") n.style.cssText = v; };
  n.getAttribute = k => attrs[k] === undefined ? null : attrs[k];
  n.removeAttribute = k => { delete attrs[k]; };
  n.appendChild = c => n.insertBefore(c, null);
  n.insertBefore = (c, before) => {
    if (c.parentNode) c.parentNode.removeChild(c);
    const i = before ? n.childNodes.indexOf(before) : n.childNodes.length;
    assert(i >= 0, "Missing insertBefore sibling");
    n.childNodes.splice(i, 0, c); c.parentNode = n; return c;
  };
  n.removeChild = c => { const i = n.childNodes.indexOf(c); assert(i >= 0, "Missing child");
    n.childNodes.splice(i, 1); c.parentNode = null; return c; };
  n.contains = c => c === n || n.children.some(x => x.contains(c));
  n.matches = selector => selector.split(",").some(part => {
    part = part.trim();
    const rel = /^(.*?)\s*(>)\s*([^>]+)$/.exec(part);
    if (rel) return n.matches(rel[3]) && !!n.parentElement?.matches(rel[1]);
    const tagMatch = /^[\w-]+/.exec(part);
    if (tagMatch && n.tagName !== tagMatch[0].toUpperCase()) return false;
    if ([...part.matchAll(/\.([\w-]+)/g)].some(m => !n.classList.contains(m[1]))) return false;
    if ([...part.matchAll(/#([\w-]+)/g)].some(m => n.id !== m[1])) return false;
    if ([...part.matchAll(/\[([\w-]+)(?:="([^"]*)")?\]/g)]
      .some(m => m[2] === undefined ? n.getAttribute(m[1]) === null : n.getAttribute(m[1]) !== m[2])) return false;
    return true;
  });
  n.closest = s => n.matches(s) ? n : n.parentElement?.closest(s) || null;
  n.querySelectorAll = s => n.children.flatMap(c => [...(c.matches(s) ? [c] : []), ...c.querySelectorAll(s)]);
  n.querySelector = s => n.querySelectorAll(s)[0] || null;
  n.click = () => n.dispatchEvent(event("click", {bubbles: true}));
  n.getBoundingClientRect = () => {
    const r = n.rect || {left: 8, top: 240, width: 330, height: 260};
    const left = n.classList.contains("pt-map-card-undocked") ? parseFloat(n.style.left) || 0 : r.left;
    const top = n.classList.contains("pt-map-card-undocked") ? parseFloat(n.style.top) || 0 : r.top;
    return {...r, left, top, right: left + r.width, bottom: top + r.height};
  };
  return n;
}
const raw = s => s.slice(s.indexOf('r"---(')+7,s.lastIndexOf(')---"'));
const family=raw(read('03_functions/leaflet_ops_live_gibs_helpers.r'));
const panel=raw(read('03_functions/leaflet_ops_live_panel_helpers.r'));
const shared=raw(read('03_functions/leaflet_ops_live_shared_helpers.r'));
const defs=raw(definitions);
function region(s,a,b) { const i=s.indexOf(a),j=s.indexOf(b,i+1);assert(i>=0&&j>i,a);return s.slice(i,j); }
function fn(s,name) {const tail=s.slice(s.indexOf('  function '+name+'('));assert(tail.startsWith('  function'));return tail.slice(0,tail.indexOf('\n  }')+4);}
new vm.Script('(function(){'+family+panel+'})');
const pure=family.slice(0,family.indexOf('  // One lazy metadata owner'));
const pureContext=vm.createContext({AbortController,TextDecoder,setTimeout,clearTimeout,Date,console});
vm.runInContext(pure,pureContext);const Q=pureContext.ptGibs;
const fixture=path.join(root,'qa/fixtures/gibs_wmts/BRIM_E30R4D1_PROVIDER_METADATA.zip');
assert.equal(hash(fs.readFileSync(fixture)),'67527b1e64ec1e63b70bb629cebc1f5ab42a9990a439fe97b6cc840a7a38f452');
const xml=run('import zipfile,sys\nwith zipfile.ZipFile('+JSON.stringify(fixture)+') as z:\n assert z.testzip() is None\n sys.stdout.buffer.write(z.read("WMTSCapabilities.xml"))',undefined,10*1024*1024);
const doc=parseXML(xml),frozen=Date.parse('2026-09-16T04:34:06.764403Z');
const bindings=Q.parseCapabilities(xml,()=>doc);
const kids=(n,k,ns=Q.WMTS)=>n.childNodes.filter(x=>x.localName===k&&x.namespaceURI===ns);
const one=(n,k,ns=Q.WMTS)=>{const x=kids(n,k,ns);assert.equal(x.length,1,k);return x[0];};
const id=n=>one(n,'Identifier',Q.OWS).textContent.trim();
const contents=one(doc.documentElement,'Contents'),layers=kids(contents,'Layer'),matrices=kids(contents,'TileMatrixSet');
function clone(n){const x={...n,childNodes:n.childNodes.map(clone),attrs:{...n.attrs}};x.getAttribute=k=>x.attrs[k]??null;return x;}
function mutated(edit){
 const c={...contents,childNodes:[...Q.PRODUCTS.map(p=>layers.find(n=>id(n)===p.id)),...matrices].map(clone)};
 const r={...doc.documentElement,attrs:{...doc.documentElement.attrs},childNodes:[c]};r.getAttribute=k=>r.attrs[k]??null;
 edit(kids(c,'Layer'),kids(c,'TileMatrixSet'),c,r);
 return {documentElement:r,getElementsByTagName:()=>[]};
}
const mutation=edit=>Q.parseCapabilities(xml,()=>mutated(edit));
const dim=l=>kids(l,'Dimension').find(n=>id(n)==='Time');
const limits=l=>one(one(l,'TileMatrixSetLink'),'TileMatrixSetLimits');
const first=Q.PRODUCTS[0],daily=Q.PRODUCTS[2];
function response(text=xml){let done=false;const b=Buffer.from(text);return {ok:true,status:200,headers:{get:()=>String(b.length)},body:{getReader:()=>({read:async()=>done?{done:true}:(done=true,{done:false,value:b}),cancel:async()=>{}})}};}
const tick=async()=>{for(let i=0;i<15;i++)await Promise.resolve();};
// Actual installed Leaflet viewport queue; the DOM/map/transport remain doubles.
const leafletPath='/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/leaflet/htmlwidgets/lib/leaflet/leaflet.js';
const lib=fs.readFileSync(leafletPath,'utf8');
assert.equal(hash(lib),'7d64e8a8b6beb191bd8f7e3b7832f6d4283a8f5a3d5367ed78db2dd25606f16b');
const tail=lib.slice(lib.indexOf('_getTiledPixelBounds:function'));
const queue=tail.slice(tail.indexOf('_update:function')+'_update:'.length,tail.indexOf(',_isValidTile:function'));
class P {constructor(x,y){this.x=x;this.y=y;}subtract(a){return new P(this.x-(a.x??a[0]),this.y-(a.y??a[1]));}add(a){return new P(this.x+(a.x??a[0]),this.y+(a.y??a[1]));}distanceTo(a){return Math.hypot(this.x-a.x,this.y-a.y);}}
class B {constructor(a,b){this.min=new P(Math.min(a.x,b.x),Math.min(a.y,b.y));this.max=new P(Math.max(a.x,b.x),Math.max(a.y,b.y));}getCenter(){return new P((this.min.x+this.max.x)/2,(this.min.y+this.max.y)/2);}getBottomLeft(){return new P(this.min.x,this.max.y);}getTopRight(){return new P(this.max.x,this.min.y);}contains(a){return a.x>=this.min.x&&a.x<=this.max.x&&a.y>=this.min.y&&a.y<=this.max.y;}}
function harness({documentOverride=doc,delay=false,realLeaflet=false}={}){
 const body=node('body'); const document={body,querySelector:q=>body.querySelector(q),querySelectorAll:q=>body.querySelectorAll(q),createElement:node,getElementById:id=>body.querySelector('#'+id),createDocumentFragment:()=>node()};
 const active=new Set(),tiles=[],requests=[],fetches=[],waiting=[],mapEvents={},loading={};let clock=frozen;
 let map={center:[37.4,-119.5],zoom:5,on(k,f){mapEvents[k]=f;},fire(k){mapEvents[k]?.();},getPane:()=>null,getCenter(){return this.center;},getZoom(){return this.zoom;},
 hasLayer:l=>active.has(l),addLayer(l){if(!active.has(l)){active.add(l);l.onAdd?.(map);}return map;},removeLayer(l){if(active.delete(l))l.onRemove?.(map);return map;},
 setView(center,z){this.center=center;this.zoom=z;for(const l of active)if(l._update){l._tileZoom=Math.min(z,l.options.maxNativeZoom);l._update();}return map;}};
 const update=vm.runInNewContext('('+queue+')',{x:P,P:B,document,isFinite,Math,Error});
 let L={DomEvent:{disableClickPropagation(){},disableScrollPropagation(){}},Layer:{extend(methods){return class {constructor(){Object.assign(this,methods);}addTo(m){m.addLayer(this);return this;}};}},tileLayer(url,options){
  const l={url,options,handlers:{},_map:map,_tiles:{},_level:{el:{appendChild(){}}},on(k,f){this.handlers[k]=f;return this;},off(){this.handlers={};return this;},fire(k){this.handlers[k]?.();},_clampZoom:z=>Math.min(z,options.maxNativeZoom),_isValidTile:()=>true,
   _tileCoordsToKey:t=>t.x+':'+t.y+':'+t.z,
   _getTiledPixelBounds(){const n=2**this._tileZoom,lat=map.center[0]*Math.PI/180,cx=(map.center[1]+180)/360*n,cy=(1-Math.log(Math.tan(lat)+1/Math.cos(lat))/Math.PI)/2*n;return new B(new P(Math.floor(cx-.7),Math.floor(cy-.5)),new P(Math.floor(cx+.7),Math.floor(cy+.5)));},
   _pxBoundsToTileRange:b=>b,_update:update,
   _addTile(t){const key=this._tileCoordsToKey(t);this._tiles[key]={coords:t,current:true};requests.push({layer:this,coords:t,url:url.replace('{z}',t.z).replace('{y}',t.y).replace('{x}',t.x)});},
   onAdd(){this._tileZoom=Math.min(map.zoom,options.maxNativeZoom);this._update();},onRemove(){this._tiles={};},addTo(m){m.addLayer(this);return this;}};tiles.push(l);return l;
 }};
 // E33R1: full installed Leaflet owns event registration/removal and tile levels.
 // DOM, image transport and painting remain offline doubles; no browser claims.
 if(realLeaflet){
  document.documentElement=node('html');document.defaultView={getComputedStyle:e=>e.style};events(document);
  Object.defineProperty(document.documentElement.style,'transform',{value:'',writable:true});
  Object.defineProperty(document.documentElement.style,'transition',{value:'',writable:true});
  const win=events({screen:{deviceXDPI:1,logicalXDPI:1},devicePixelRatio:1,setTimeout,clearTimeout,
    WebKitCSSMatrix:function(){this.m11=1;}});
  const ctx=vm.createContext({window:win,document,navigator:{userAgent:'offline fixture',platform:'Mac'},setTimeout,clearTimeout,console});
  vm.runInContext(lib,ctx);L=ctx.L||ctx.window.L;assert.equal(L.version,'1.3.1+HEAD.ba6f97f');
  const container=node();Object.assign(container,{clientWidth:1000,clientHeight:700,clientLeft:0,clientTop:0});
  container.getElementsByClassName=c=>container.querySelectorAll('.'+c);body.appendChild(container);
  map=L.map(container,{zoomAnimation:false,fadeAnimation:false,zoomSnap:0.5,zoomDelta:0.5,
    attributionControl:false,zoomControl:false}).setView([37.36,-118.73584],6.5);
  map.createPane('pane_ops');
  const make=L.tileLayer;
  L.tileLayer=(url,options)=>{const t=make(url,options);tiles.push(t);
    t.on('tileloadstart',e=>requests.push({layer:t,coords:e.coords,url:e.tile.src}));return t;};
 }
 class Clock extends Date{constructor(...a){super(...(a.length?a:[clock]));}static now(){return clock;}}
 const context=vm.createContext({window:{},map,L,document,Event:function(k,v){return event(k,v);},AbortController,TextDecoder,setTimeout,clearTimeout,Date:Clock,console,
 DOMParser:class{parseFromString(){return documentOverride;}},
 fetch:(url,opts)=>{fetches.push({url,opts});return delay?new Promise((resolve,reject)=>waiting.push({resolve,reject})):Promise.resolve(response());},
 escapeHtml:s=>String(s).replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#039;'}[c])),
 activeLayers:{},activeLegendDefs:{},statusRows:{},checkboxByName:{},rowByName:{},opsDefByName:{},opsLinkBlocks:[],
 recordStatus(name,msg){context.statusRows[name]=msg;},redrawLegend(){},redrawStatus(){},updateOpsHeaderCount(){},setOpsLayerLoading(n,v){loading[n]=v;},clearAllOpsLayerLoading(){}});
 const execute=s=>vm.runInContext(s,context,{timeout:3000});
 ['ptOpsShouldDisplayLink','compactUrlForTitle','ptOpsCompactLinkLabel','linkHtml','infoLabelForDef','layerLinkItems','ptOpsDefIsRefreshable','ptOpsRefreshActionHtml','layerRowLinksHtml','opsExternalLinkBlockHtml','opsExternalLinksForCategory'].forEach(n=>execute(fn(shared,n)));
 const timing=raw(read('03_functions/leaflet_ops_live_wpc_qpf_hover_helpers.r'));
 ['parseArcgisUtcDate','formatLosAngelesCompactParts'].forEach(n=>execute(fn(timing,n)));
 execute(family);
 const sat=region(defs,'  // Five satellite products,','  addOpsLayer({\n    category: \'Hydro Observations\',');
 context.opsLayers=[];context.addOpsLayer=def=>{def.guideProductId=def.layer.gibsProduct.stableId;def.deliveryClass='brim_enhanced';context.opsLayers.push(def);};execute(sat);execute(panel);
 // Real Tools Clear All owner invokes the Ops clear bridge.
 context.ptSetTeachingLabelPlacementActive=()=>{};context.ptSetTeachingMarkupMode=()=>{};context.ptSetStatus=()=>{};context.ptClearMeasurements=()=>{};context.ptClearCustomLayers=()=>{};
 execute(fn(read('03_functions/js/leaflet_tools_adddata_panel.js'),'ptClearAllPt2SessionLayers'));
 const checks=context.opsLayers.map(d=>context.checkboxByName[d.name]);
 function choose(i,on=true){checks[i].checked=on;checks[i].dispatchEvent(event('change',{bubbles:true}));}
 return {context,document,body,map,active,tiles,requests,fetches,waiting,loading,checks,choose,layers:context.ptGibsLayers,
 clock:n=>{clock=n;},resolve:()=>waiting.shift().resolve(response()),reject:()=>waiting.shift().reject(new Error('offline simulated CORS rejection')),
 date(i,value){const s=document.getElementById('pt-gibs-'+Q.PRODUCTS[i].stableId+'-date');s.value=value;s.dispatchEvent(event('change',{bubbles:true}));return s;},
 text:i=>document.getElementById('pt-gibs-'+Q.PRODUCTS[i].stableId+'-status').textContent};
}
test('full authenticated XML, implemented BRIM parser, no positive subset',()=>{assert.equal(Buffer.byteLength(xml),5786627);assert.equal(hash(xml),'b7c97c269fb83114a239560acad7b2cddc0431cca34ba78472fdf07105d7ad6e');assert.equal(Object.keys(bindings).length,5);});
for(const [i,p] of Q.PRODUCTS.entries())test('full capture exact binding '+p.stableId,()=>{
 const b=bindings[p.id];assert(b.ok,b.reason);assert.equal(b.coverageLimits.coverageProven,false);
 if(p.kind==='instant'){assert.equal(new Date(Q.latestInstant(b.ranges,frozen)).toISOString(),['2026-09-16T03:10:00.000Z','2026-09-16T03:30:00.000Z'][i]);assert.equal(b.coverageLimits.status,'KNOWN_INCONSISTENT_GOES_LIMITS');assert.deepEqual(Array.from(b.coverageLimits.records.find(r=>r[0]===5&&r[3]===35)),[5,0,18,35,39]);}
 else{assert.equal(b.coverageLimits.status,'NOT_ADVERTISED');assert.deepEqual(Array.from(Q.dailyWindow(b.ranges,frozen)),['2026-09-16','2026-09-15','2026-09-14','2026-09-13','2026-09-12','2026-09-11','2026-09-10','2026-09-09']);}
 assert.equal(p.zoom,i===0?7:i===1?6:9);assert.equal(p.mime,i<2?'image/png':'image/jpeg');
});
const rejectCases=[
 ['duplicate product',(ls,ms,c)=>c.childNodes.push(clone(ls[0]))],
 ['missing product',(ls,ms,c)=>c.childNodes.splice(c.childNodes.indexOf(ls[0]),1)],
 ['Time alias duplicate',ls=>{const d=clone(dim(ls[0]));one(d,'Identifier',Q.OWS).textContent='time';ls[0].childNodes.push(d);}],
 ['uppercase TIME',ls=>one(dim(ls[0]),'Identifier',Q.OWS).textContent='TIME'],
 ['missing Time',ls=>ls[0].childNodes=ls[0].childNodes.filter(n=>n.localName!=='Dimension')],
 ['wrong UOM namespace',ls=>one(dim(ls[0]),'UOM',Q.OWS).namespaceURI=Q.WMTS],
 ['duplicate UOM',ls=>dim(ls[0]).childNodes.push(clone(one(dim(ls[0]),'UOM',Q.OWS)))],
 ['wrong UOM value',ls=>one(dim(ls[0]),'UOM',Q.OWS).textContent='seconds'],
 ['malformed default date',ls=>one(dim(ls[0]),'Default').textContent='2026-02-30T03:00:00Z'],
 ['bad time period',ls=>kids(dim(ls[0]),'Value')[0].textContent='2026-09-16T03:00:00Z/2026-09-16T03:10:00Z/PT0S'],
 ['wrong format',ls=>kids(ls[0],'Format').forEach(n=>n.textContent='image/jpeg')],
 ['wrong style',ls=>one(kids(ls[0],'Style')[0],'Identifier',Q.OWS).textContent='other'],
 ['matrix link mismatch',ls=>one(one(ls[0],'TileMatrixSetLink'),'TileMatrixSet').textContent='GoogleMapsCompatible_Level6'],
 ['core CRS',(ls,ms)=>one(ms.find(n=>id(n)===first.matrix),'SupportedCRS',Q.OWS).textContent='EPSG:4326'],
 ['core matrix duplicate',(ls,ms)=>{const m=ms.find(n=>id(n)===first.matrix);m.childNodes.push(clone(kids(m,'TileMatrix')[0]));}],
 ['wrong origin',(ls,ms)=>one(kids(ms.find(n=>id(n)===first.matrix),'TileMatrix')[0],'TopLeftCorner').textContent='0 0'],
 ['wrong scale',(ls,ms)=>one(kids(ms.find(n=>id(n)===first.matrix),'TileMatrix')[0],'ScaleDenominator').textContent='NaN'],
 ['wrong tile size',(ls,ms)=>one(kids(ms.find(n=>id(n)===first.matrix),'TileMatrix')[0],'TileWidth').textContent='512'],
 ['noncanonical matrix width',(ls,ms)=>one(kids(ms.find(n=>id(n)===first.matrix),'TileMatrix')[0],'MatrixWidth').textContent='1.0'],
 ['unknown coverage field',ls=>{const n=clone(limits(ls[0]).childNodes[0]);n.localName='Unexpected';limits(ls[0]).childNodes.push(n);}],
 ['new unresolved limits',ls=>one(kids(limits(ls[0]),'TileMatrixLimits')[0],'MaxTileCol').textContent='99'],
 ['absent GOES limits',ls=>{const l=one(ls[0],'TileMatrixSetLink');l.childNodes=l.childNodes.filter(n=>n.localName!=='TileMatrixSetLimits');}],
 ['reversed range',ls=>one(kids(limits(ls[0]),'TileMatrixLimits')[0],'MinTileRow').textContent='2']
];
for(const [name,mutate] of rejectCases)test('per-product rejection: '+name,()=>{const b=mutation(mutate);assert.equal(b[first.id].ok,false,b[first.id].reason);assert.equal(b[Q.PRODUCTS[2].id].ok,true,'unrelated daily binding survives');});
test('lowercase time alias accepted',()=>assert(mutation(ls=>one(dim(ls[0]),'Identifier',Q.OWS).textContent='time')[first.id].ok));
test('unexpected daily coverage limits rejected',()=>assert.equal(mutation(ls=>one(ls[2],'TileMatrixSetLink').childNodes.push(clone(limits(ls[0]))))[daily.id].ok,false));
for(const [name,change] of [['root namespace',r=>r.namespaceURI='wrong'],['WMTS version',r=>r.attrs.version='2.0.0']])test(name+' fails closed',()=>assert.throws(()=>mutation((ls,ms,c,r)=>change(r))));
test('malformed XML and forbidden entity declarations rejected',()=>{assert.throws(()=>Q.parseCapabilities('<broken',parseXML));assert.throws(()=>Q.parseCapabilities('<!DOCTYPE x>'+xml,()=>doc));});
test('valid unique complete limits are honored',()=>{
 const b=mutation(ls=>{const c=limits(ls[0]),template=kids(c,'TileMatrixLimits')[0];c.childNodes=Array.from({length:8},(_,z)=>{const n=clone(template);['TileMatrix','MinTileRow','MaxTileRow','MinTileCol','MaxTileCol'].forEach((f,i)=>one(n,f).textContent=String(i===0?z:0));return n;});})[first.id];
 assert(b.ok,b.reason);assert.equal(b.coverageLimits.status,'DECLARED_LIMITS_VALID');assert(Q.tileAllowed(first,b,{z:5,x:0,y:0}));assert(!Q.tileAllowed(first,b,{z:5,x:1,y:0}));
});
test('valid core bounds never normalize columns; no regional mask',()=>{const b=bindings[first.id];for(const c of [{z:5,x:35,y:12},{z:5,x:-1,y:12},{z:5,x:5,y:32},{z:8,x:1,y:1},{z:5,x:.5,y:0}])assert(!Q.tileAllowed(first,b,c));for(const c of [{z:5,x:20,y:15},{z:5,x:0,y:0},{z:5,x:31,y:31}])assert(Q.tileAllowed(first,b,c));assert(!/REGION|regionalTileAllowed|Air_Mass/.test(family));});
test('latest instant uses advertised anchored step, not future or default',()=>{const r=Q.domain(['2026-09-16T03:03:00Z/2026-09-16T05:03:00Z/PT10M'],'instant');assert.equal(new Date(Q.latestInstant(r,frozen)).toISOString(),'2026-09-16T04:33:00.000Z');assert.equal(Q.latestInstant(r,0),null);});
test('daily gaps and eight UTC dates without 48h rejection',()=>{const r=Q.domain(['2026-09-09/2026-09-15/P2D'],'daily');assert.deepEqual(Array.from(Q.dailyWindow(r,frozen)),['2026-09-15','2026-09-13','2026-09-11','2026-09-09']);assert.throws(()=>Q.tileURL(daily,{...bindings[daily.id],ranges:r},'2026-09-14'));});
test('exact dated URL, no default time',()=>assert.equal(Q.tileURL(first,bindings[first.id],'2026-09-16T03:10:00Z').replace('{z}','5').replace('{y}','12').replace('{x}','5'),Q.ROOT+first.id+'/default/2026-09-16T03%3A10%3A00Z/GoogleMapsCompatible_Level7/5/12/5.png'));
test('shared owner lazy, coalesced, parsed cache, explicit refresh',async()=>{let calls=0;const o=Q.createOwner({fetcher:async(u,opts)=>{calls++;assert.equal(u,Q.CAPABILITIES);assert.equal(opts.mode,'cors');assert.equal(opts.credentials,'omit');return response();},parseXML:()=>doc,now:()=>frozen});assert.equal(calls,0);const a=o.check(),b=o.check(true);assert.equal(a,b);assert.equal((await a).status,'PARSED');await o.check();assert.equal(calls,1);await o.check(true);assert.equal(calls,2);o.destroy();assert.equal((await o.check()).status,'DESTROYED');});
test('failed metadata cached UNKNOWN, no implicit retries or provider retirement',async()=>{let calls=0;const o=Q.createOwner({fetcher:async()=>{calls++;throw Error('CORS blocked');}});assert.equal((await o.check()).status,'UNKNOWN');await o.check();assert.equal(calls,1);await o.check(true);assert.equal(calls,2);o.destroy();});
test('timeout aborts request without retries',async()=>{let signal;await assert.rejects(Q.fetchMetadata(async(u,o)=>{signal=o.signal;return new Promise(()=>{});},{timeoutMs:2}),/timeout/);assert(signal.aborted);});
test('size guards both declared and streamed bytes',async()=>{const r=response('abcdef');await assert.rejects(Q.fetchMetadata(async()=>r,{cap:3}),/cap/);const s=response('abcdef');s.headers.get=()=>null;await assert.rejects(Q.fetchMetadata(async()=>s,{cap:3}),/cap/);});
test('HTTP, missing stream and invalid UTF8 fail without fallback',async()=>{await assert.rejects(Q.fetchMetadata(async()=>({ok:false,status:403})),/403/);await assert.rejects(Q.fetchMetadata(async()=>({ok:true,headers:{get:()=>null}})),/streaming/);const r=response('ok');r.body.getReader=()=>({read:async()=>({done:false,value:Buffer.from([0xff])}),cancel:async()=>{}});await assert.rejects(Q.fetchMetadata(async()=>r));});
test('map/panel startup stays lazy; actual satellite checkboxes select one',async()=>{const h=harness();assert.equal(h.fetches.length,0);assert.equal(h.tiles.length,0);assert(h.checks.every(c=>!c.checked));h.choose(0);h.choose(1);await tick();assert.equal(h.fetches.length,1);assert.equal(h.tiles.length,1);assert(!h.checks[0].checked&&h.checks[1].checked);assert(!h.layers[0].gibsState.active&&h.layers[1].gibsState.active);});
test('all five labels, dates and native zoom limits use actual BRIM wiring',async()=>{const h=harness();for(let i=0;i<5;i++){h.choose(i);await tick();assert(h.text(i).includes(i<2?'Requested advertised time:':'Requested UTC data date: 2026-09-15'));assert(h.text(i).includes('Unverified'));}assert.equal(h.tiles.length,5);assert.equal(h.fetches.length,1);assert.deepEqual(h.tiles.map(t=>t.options.maxNativeZoom),[7,6,9,9,9]);assert.equal(h.body.querySelectorAll('input[type="radio"]').length,0);});
test('actual Leaflet queue requests only selected viewport and remains unclipped elsewhere',async()=>{const h=harness();h.choose(0);await tick();assert(h.requests.length>0&&h.requests.length<=9);assert(h.requests.every(r=>Q.tileAllowed(first,bindings[first.id],r.coords)));h.map.setView([0,45],5);assert(h.requests.some(r=>r.coords.x>17));assert.equal(h.fetches.length,1);h.map.setView([37,-119],12);assert(h.requests.every(r=>r.coords.z<=7));assert.equal(h.fetches.length,1);});
test('off during shared metadata does not cancel another selected layer',async()=>{const h=harness({delay:true});h.choose(0);h.choose(1);h.choose(0,false);h.resolve();await tick();assert.equal(h.tiles.length,1);assert.equal(h.layers[0].gibsState.request,null);assert(h.layers[1].gibsState.request);assert.equal(h.fetches.length,1);});
test('rapid off/re-add ignores older selection completion',async()=>{const h=harness({delay:true});h.choose(0);h.choose(0,false);h.choose(0);h.resolve();await tick();assert.equal(h.tiles.length,1);assert(h.layers[0].gibsState.active);});
test('Clear Ops cancels selected and explicit off-row checks',async()=>{const h=harness({delay:true});h.choose(0);h.layers[1].refreshCurrentView();h.context.ptClearOpsLayers();h.resolve();await tick();assert.equal(h.tiles.length,0);assert.equal(Object.keys(h.context.activeLayers).length,0);assert(h.checks.every(c=>!c.checked));assert.equal(h.layers[1].gibsState.metadata,'NOT_CHECKED');});
test('map destroy cancels metadata; late completion cannot mount',async()=>{const h=harness({delay:true});h.choose(0);h.map.fire('unload');h.resolve();await tick();assert.equal(h.tiles.length,0);assert(!h.layers[0].canActivate());assert(h.fetches[0].opts.signal.aborted);});
test('date change detaches old tiles, preserves select focus, ignores old events',async()=>{const h=harness();h.choose(2);await tick();const old=h.tiles[0],late=old.handlers.tileerror;const select=h.date(2,'2026-09-12');assert(!h.active.has(old));assert.equal(h.tiles.length,2);assert.equal(h.layers[2].gibsState.request,'2026-09-12');late();assert.equal(h.layers[2].gibsState.tileErrors,0);assert.equal(h.date(2,'2026-09-13'),select);assert.equal(h.fetches.length,1);});
test('unadvertised date cannot draw; yesterday missing requires manual choice',async()=>{const d=mutated(ls=>kids(dim(ls[2]),'Value').forEach(n=>n.textContent='2026-09-14'));const h=harness({documentOverride:d});h.choose(2);await tick();assert.equal(h.tiles.length,0);assert(h.text(2).includes('Choose'));h.date(2,'2026-09-15');assert.equal(h.tiles.length,0);h.date(2,'2026-09-14');assert.equal(h.tiles.length,1);});
test('daily choice survives refresh, rollover expiration requires manual choice',async()=>{const h=harness();h.choose(2);await tick();h.date(2,'2026-09-09');await h.layers[2].refreshCurrentView();assert.equal(h.layers[2].gibsState.request,'2026-09-09');h.clock(frozen+Q.DAY);await h.layers[2].refreshCurrentView();assert.equal(h.layers[2].gibsState.request,null);assert(h.text(2).includes('Choose'));assert.equal(h.layers[2].gibsState.chosen,'2026-09-09');});
test('errors survive load and subsequent loading batches without date fallback',async()=>{const h=harness();h.choose(2);await tick();const t=h.tiles[0];t.fire('tileerror');t.fire('load');t.fire('loading');t.fire('tileload');t.fire('load');assert(h.text(2).includes('transport errors: 1'));assert.equal(h.tiles.length,1);assert.equal(h.layers[2].gibsState.request,'2026-09-15');});
test('refresh failure retains honest old dated imagery; no other layer status overwritten',async()=>{const h=harness({delay:true});h.choose(1);h.resolve();await tick();h.choose(0);await tick();const before=h.text(1),old=h.tiles.at(-1),date=h.layers[0].gibsState.request;const recheck=h.layers[0].refreshCurrentView();h.reject();await recheck;assert(h.active.has(old));assert.equal(h.layers[0].gibsState.request,date);assert(h.text(0).includes('Previous dated imagery retained'));assert.equal(h.text(1),before);});
test('per-product invalid binding disables only its add path, leaves info/recheck',async()=>{const d=mutated(ls=>one(ls[0],'Identifier',Q.OWS).textContent='missing');const h=harness({documentOverride:d});h.choose(0);await tick();h.choose(1);await tick();assert(h.checks[0].disabled);assert(!h.checks[1].disabled);assert(!h.checks[0].checked);assert.equal(h.tiles.length,1);assert.equal(h.context.ptOpsActivateLayerByName(first.label).ok,false);assert(h.body.querySelectorAll('[data-pt-ops-action="refresh"]').length===5);await h.layers[0].refreshCurrentView();assert.equal(h.fetches.length,2);});
test('actual rfrsh click works on an off row and does not select imagery',async()=>{const h=harness();const links=h.body.querySelectorAll('[data-pt-ops-action="refresh"]');links[0].click();await tick();assert.equal(h.fetches.length,1);assert.equal(h.tiles.length,0);assert(!h.checks[0].checked);});
test('matrix placement and satellite info/srce accessibility; no panel legend',()=>{const h=harness();const html=h.context.buildOpsPanelHtml();assert(html.indexOf('Satellite / Imagery')<html.indexOf('GOES-West imagery matrix'));assert(html.indexOf('GOES-West imagery matrix')<html.indexOf('pt-ops-layer-toggle-0'));assert(!/<img|<canvas|<iframe/i.test(html));for(const p of Q.PRODUCTS){const d=h.context.opsLayers.find(d=>d.name===p.label);const links=h.context.layerLinkItems(d).join('');assert(links.includes(p.infoUrl.replace(/&/g,'&amp;')));assert(links.includes(p.sourceUrl));assert(links.includes('aria-label="Product information — external"'));assert(links.includes('aria-label="GIS source and service metadata — external"'));assert(links.includes('rel="noopener"'));assert(!links.includes('>guide<'));}assert.equal(h.context.infoLabelForDef({category:'Hydro Observations'},{infoLabel:'guide'}),'guide');});
test('old active endpoints/Water Vapor retired; no live fixture/time/storage fallback',()=>{const src=['layer_definition','service','panel','gibs'].map(n=>read('03_functions/leaflet_ops_live_'+n+'_helpers.r')).join('\n');assert(!/ABI10_current|ABI13_current|MERGEDGC_current|ops_goes_water_vapor|Air_Mass/.test(src));assert(!/2026-09-16|WMTSCapabilities\.xml[^\n]*5786627|localStorage|sessionStorage|setInterval|no-cors/.test(family));});
test('Clear All uses real Tools to Ops bridge; reactivation restores single selection',async()=>{const h=harness();h.choose(0);await tick();h.choose(2);await tick();h.context.ptClearAllPt2SessionLayers();assert.equal(h.active.size,0);assert(h.checks.every(c=>!c.checked));h.choose(0);await tick();assert(h.layers[0].gibsState.request);h.choose(2);await tick();assert(!h.layers[0].gibsState.request);assert(h.layers[2].gibsState.request);assert.equal(h.fetches.length,1);});
test('off-row recheck clears loading without enabling tiles',async()=>{const h=harness({delay:true});const links=h.body.querySelectorAll('[data-pt-ops-action="refresh"]');links[0].click();assert(h.loading[first.label]);h.resolve();await tick();assert.equal(h.loading[first.label],false);assert.equal(h.tiles.length,0);});
test('explicit coalesced refresh updates GOES advertised instant only',async()=>{const d=mutated(ls=>kids(dim(ls[0]),'Value').forEach(n=>n.textContent='2026-09-16T03:00:00Z/2026-09-16T05:00:00Z/PT10M'));const h=harness({documentOverride:d});h.choose(1);await tick();h.choose(0);await tick();const other=h.layers[1].gibsState.request;h.clock(frozen+600000);await Promise.all([h.layers[0].refreshCurrentView(),h.layers[0].refreshCurrentView()]);assert.equal(h.fetches.length,2);assert.equal(h.layers[0].gibsState.request,'2026-09-16T04:40:00Z');assert.equal(h.layers[1].gibsState.request,other);assert.equal(h.active.size,2);});
test('off/readd stale tile completion never overwrites new transport state',async()=>{const h=harness();h.choose(0);await tick();const old=h.tiles[0].handlers.tileerror;h.choose(0,false);h.choose(0);await tick();old();assert.equal(h.layers[0].gibsState.tileErrors,0);assert.equal(h.tiles.length,2);assert(h.active.has(h.tiles[1]));assert(!h.active.has(h.tiles[0]));});
test('GOES requested time pairs exact Z with existing Pacific formatter; daily remains UTC',async()=>{const h=harness();h.choose(0);await tick();assert(h.text(0).includes('2026-09-16T03:10:00Z'));assert(h.text(0).includes('PDT'));assert(!h.text(0).includes('Z UTC'));assert(h.text(0).includes('precise coverage UNKNOWN'));h.choose(2);await tick();assert(!h.text(2).includes('PDT'));h.choose(0,false);assert(!Object.hasOwn(h.context.statusRows,first.label));});
test('retained dated tiles continue truthful transport reporting after failed recheck',async()=>{const h=harness({delay:true});h.choose(0);h.resolve();await tick();const tile=h.tiles[0];const check=h.layers[0].refreshCurrentView();h.reject();await check;tile.fire('loading');tile.fire('tileerror');tile.fire('load');assert(h.text(0).includes('Previous dated imagery retained'));assert(h.text(0).includes('transport errors: 1'));assert(h.active.has(tile));});
// These cases catch the leaked Leaflet remove listener omitted by the queue double.
for(let i=0;i<5;i++)for(const action of ['initial','refresh','off/on','Clear Ops','Clear All',...(i>=2?['date']:[])])
 test('E33 zoom '+Q.PRODUCTS[i].stableId+' after '+action,async()=>{
  const h=harness({realLeaflet:true});h.choose(i);await tick();const old=h.tiles.at(-1);
  if(action==='refresh')await h.layers[i].refreshCurrentView();
  if(action==='date')h.date(i,'2026-09-12');
  if(action==='off/on'){h.choose(i,false);h.choose(i);await tick();}
  if(action==='Clear Ops'||action==='Clear All'){
    if(action==='Clear Ops')h.context.ptClearOpsLayers();else h.context.ptClearAllPt2SessionLayers();
    assert(!h.map.hasLayer(old));h.choose(i);await tick();
  }
  const tile=h.tiles.at(-1),state=h.layers[i].gibsState,binding=state.binding,requested=state.request,chosen=state.chosen,n=h.fetches.length;
  try {
    for(const [center,z] of [[[37,-119],7],[[39,-116],8.5],[[38,-121],5.5],[[36,-118],6.5]]){
      const before=h.requests.length;
      h.map.setView(center,z,{animate:false});
      assert.equal(h.map.getZoom(),z);assert(h.map.hasLayer(tile));assert(h.map.hasLayer(h.layers[i]));
      assert(h.checks[i].checked);assert.equal(state.active,true);assert.equal(state.request,requested);
      assert.equal(state.chosen,chosen);assert.strictEqual(state.binding,binding);assert.equal(h.fetches.length,n);
      assert(h.requests.length>before,'new viewport must enqueue new owned tiles');
      assert(Object.values(tile._tiles).some(t=>t.current),'current tile ownership missing');
    }
    // Native zoom-animation callback uses the same retained tile owner. CSS painting is not modeled.
    tile._animateZoom({center:h.map.getCenter(),zoom:7,noUpdate:false});
    assert.equal(h.fetches.length,n);assert(h.checks[i].checked);
    tile.fire('tileerror');tile.fire('load');assert(h.text(i).includes('transport errors: 1'));
    assert(h.checks[i].checked);assert.equal(state.request,requested);
    h.choose(i,false);assert(!h.map.hasLayer(tile));assert(!h.map.hasLayer(h.layers[i]));
    assert(!h.checks[i].checked);assert(!Object.hasOwn(h.context.statusRows,Q.PRODUCTS[i].label));
    h.map.setView([35,-117],7.5,{animate:false});assert.equal(h.fetches.length,n);
  } finally {h.map.remove();}
 });
test('exact shorter IR label and display-only subgroup headings',()=>{
 const h=harness();assert.equal(Q.PRODUCTS[1].label,'GOES-West Clean IR (Band 13)');
 assert.equal(Q.PRODUCTS[1].id,'GOES-West_ABI_Band13_Clean_Infrared');
 assert.deepEqual(h.body.querySelectorAll('.pt-ops-subgroup').map(n=>n.textContent),
  ['GOES-West (geostationary)','Daily true color (polar orbiters)']);
 assert.deepEqual(Array.from(h.context.opsLayers,d=>d.guideProductId),
  ['ops_goes_geocolor','ops_goes_infrared','ops_viirs_noaa20_true_color','ops_viirs_noaa21_true_color','ops_modis_terra_true_color']);
});
test('one native collapsed local meta disclosure with five exact three-field entries',()=>{
 const h=harness(),cards=h.body.querySelectorAll('#pt-gibs-meta');assert.equal(cards.length,1);
 const card=cards[0];assert.equal(card.tagName,'DETAILS');assert.equal(card.getAttribute('open'),null);
 const summary=card.querySelector('summary');assert.equal(summary.textContent,'meta');assert(summary.getAttribute('aria-label'));
 const entries=card.querySelectorAll('[data-gibs-meta-id]');assert.equal(entries.length,5);
 const expected=[['Geostationary','GOES-18','ABI — GeoColor'],['Geostationary','GOES-18','ABI — Band 13 Clean Infrared'],
  ['Polar-orbiting','NOAA-20','VIIRS — Corrected Reflectance True Color'],['Polar-orbiting','NOAA-21','VIIRS — Corrected Reflectance True Color'],
  ['Polar-orbiting','Terra','MODIS — Corrected Reflectance True Color']];
 entries.forEach((n,i)=>{assert.equal(n.getAttribute('data-gibs-meta-id'),Q.PRODUCTS[i].stableId);
  assert.deepEqual(n.querySelectorAll('dt').map(n=>n.textContent),['Orbit','Satellite','Instrument / product']);
  assert.deepEqual(n.querySelectorAll('dd').map(n=>n.textContent),expected[i]);});
 summary.click();summary.click();assert.equal(h.fetches.length,0);assert(h.checks.every(c=>!c.checked));
 assert.equal(card.querySelectorAll('a').length,0); // Native details interaction/fit still needs browser acceptance.
 assert(h.body.querySelectorAll('.pt-ops-layer-row').every(row=>!row.textContent.includes('meta')));
});
test('satellite switch preserves non-satellite owner and cleans real Leaflet listeners',async()=>{
 const h=harness({realLeaflet:true}),L=h.context.L,other=L.layerGroup().addTo(h.map);
 h.context.activeLayers['Non-satellite sentinel']=other;
 h.choose(0);await tick();const old=h.tiles.at(-1);h.choose(2);await tick();
 assert(!h.map.hasLayer(old));assert(!h.map.hasLayer(h.layers[0]));assert(!h.checks[0].checked);
 assert(h.map.hasLayer(h.layers[2]));assert(h.checks[2].checked);assert(h.map.hasLayer(other));
 assert.equal(h.context.activeLayers['Non-satellite sentinel'],other);assert.equal(h.fetches.length,1);
 h.map.setView([36,-120],7.5,{animate:false});assert.equal(h.fetches.length,1);
 h.choose(2,false);assert(h.checks.every(c=>!c.checked));assert(h.map.hasLayer(other));
 h.map.remove();assert(h.layers.every(l=>!l.canActivate()));
});
(async()=>{for(const [name,f] of tests.filter(([n])=>!process.argv.includes("--zoom-only")||n.startsWith("E33 zoom"))){try{await f();passed++;console.log('PASS '+name);}catch(e){failed++;console.log('FAIL '+name+'\n'+e.stack);}}console.log('RESULT passed='+passed+' failed='+failed);process.exitCode=failed?1:0;})();
