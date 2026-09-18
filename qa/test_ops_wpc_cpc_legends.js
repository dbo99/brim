'use strict';
// Offline source ownership tests; controlled transport and DOM, no provider calls.
const assert = require('node:assert/strict');
const vm = require('node:vm');
const fs = require('node:fs');
const path = require('node:path');
const {fixture, fn, raw, source} = require('./test_ops_radar_qpe_contracts.js');
const hover = raw(source.qpfTime);
const cases = [];
const test = (name, run) => cases.push({name, run});
function hoverContext() {
  const c = vm.createContext({escapeHtml: s => String(s).replace(/</g,'&lt;')});
  const service=raw(source.service); vm.runInContext(service.slice(service.indexOf('  function ptForecastDateParts(')),c);
  vm.runInContext(hover, c); return c;
}
// E42-H1 intentionally supersedes E38 amount-only/no-title/no-timing UX.
test('ordinary QPF hover retains amount with compact product and honest unavailable time', () => {
  const c=hoverContext();
  assert.equal(c.wpcQpfFeatureHtml('WPC QPF Day 1', {qpf:0.72,units:'Inches',product:'24-hour QPF',issue_time:'2026-09-16 17:42:26'}).replace(/<[^>]*>/g,''), 'WPC QPF Day 10.72 inValid time unverifiedWPC polygon');
});
test('absence is not a zero forecast', () => {
  const c=hoverContext();
  for(const v of [null,undefined,'',' ',false,true,NaN,Infinity,-9999,'garbage']) assert.equal(c.formatWpcQpfAmount(v,'Inches'),'');
  assert.equal(c.formatWpcQpfAmount(0,'Inches'),'0.00 in');
});
test('QPF selection replaces previous QPF at the actual owner', () => {
  const f=fixture();
  f.context.ptOpsActivateLayerByName('WPC QPF Day 1');
  f.context.ptOpsActivateLayerByName('WPC QPF Day 2');
  assert(!f.map.hasLayer(f.context.opsDefByName['WPC QPF Day 1'].layer));
  assert.equal(f.context.checkboxByName['WPC QPF Day 1'].checked,false); f.unchanged();
});
test('QPF legend belongs on map with cycle uncertainty, not panel swatches', () => {
  const f=fixture();f.context.ptOpsActivateLayerByName('WPC QPF Day 1');
  const card=f.container.querySelector('[data-forecast-product="ops_wpc_qpf_day_1"]');
  assert(card); assert(card.querySelector('details').textContent.includes('Image and metadata cycle alignment is not independently confirmed'));
  assert(!f.notes.textContent.includes('QPE / QPF colors'));
});
test('CPC hover and popup implementation remains available', () => {
  assert(fn(source.tools,'ptTooltipFromProperties'));assert(fn(source.tools,'ptPopupFromProperties'));
});
const crypto=require('node:crypto'),zlib=require('node:zlib'),{execFileSync}=require('node:child_process');
const plain=x=>JSON.parse(JSON.stringify(x));
const sha=b=>crypto.createHash('sha256').update(b).digest('hex');
const fixturePath=path.join(__dirname,'fixtures/wpc_cpc/BRIM_E37R1_PROVIDER_CAPTURE.zip');
assert.equal(sha(fs.readFileSync(fixturePath)),'a9a3ed2ee97bed85e0f01459b4a5ccdb7d60f098ab476684713a2536168c2d50');
const captured=JSON.parse(execFileSync('python3',['-I','-B','-S','-c',String.raw`
import sys,zipfile,io,json,hashlib,csv
z=zipfile.ZipFile(sys.argv[1]);o=zipfile.ZipFile(io.BytesIO(z.read('prior/BRIM_00E_E37_PROVIDER_CAPTURE.zip')))
def pairs(ps):
 d={}
 for k,v in ps:
  if k in d: raise ValueError('duplicate JSON key')
  d[k]=v
 return d
out={}
for prefix,archive in [('current',z),('prior',o)]:
 assert archive.testzip() is None
 names=archive.namelist();assert len(names)==len(set(names))
 assert all(not n.startswith('/') and '..' not in n.split('/') for n in names)
 for n in names:
  if n.endswith('.body') or n=='CAPTURE.json':out[prefix+'/'+n]=json.loads(archive.read(n),object_pairs_hook=pairs)
with open(sys.argv[2],newline='') as f:out['catalog']=[r for r in csv.DictReader(f) if r['external_layer_id'] in ['EXT062','EXT063','EXT064','EXT065']]
print(json.dumps(out))
`,fixturePath,path.join(__dirname,'../00_config/external_service_catalog.csv')],{encoding:'utf8',maxBuffer:4000000}));
const snap=JSON.parse(fs.readFileSync(path.join(__dirname,'fixtures/wpc_cpc/cpc_hover_popup_snapshots.json'))).cpc;
const data=n=>captured['current/responses/'+n+'.body'];
const fullMeta=captured['prior/responses/qpf_layers.body'],fullLegend=captured['prior/responses/qpf_legend.body'];
const qpfNames=['WPC QPF Day 1','WPC QPF Day 2','WPC QPF Day 3','WPC QPF 3-day','WPC QPF 7-day'];
const qpfIds=['ops_wpc_qpf_day_1','ops_wpc_qpf_day_2','ops_wpc_qpf_day_3','ops_wpc_qpf_3day','ops_wpc_qpf_7day'];
const selectors=[1,2,3,9,11];
const tick=()=>new Promise(r=>setImmediate(r));
function centerPng(text){
 const b=Buffer.from(text,'base64');assert.equal(b.subarray(0,8).toString('hex'),'89504e470d0a1a0a');
 let at=8,w,h,parts=[];
 while(at<b.length){const n=b.readUInt32BE(at),type=b.toString('ascii',at+4,at+8),d=b.subarray(at+8,at+8+n);assert(at+12+n<=b.length);
  if(type==='IHDR'){w=d.readUInt32BE(0);h=d.readUInt32BE(4);assert.deepEqual([...d.subarray(8)],[8,6,0,0,0]);}
  if(type==='IDAT')parts.push(d);at+=12+n;
 }
 const x=zlib.inflateSync(Buffer.concat(parts)),stride=w*4;assert.equal(x.length,h*(stride+1));let rows=[];
 for(let y=0;y<h;y++){let row=Buffer.alloc(stride),f=x[y*(stride+1)];for(let i=0;i<stride;i++){let a=i>=4?row[i-4]:0,c=y&&i>=4?rows[y-1][i-4]:0,up=y?rows[y-1][i]:0,p=a+up-c,pa=Math.abs(p-a),pb=Math.abs(p-up),pc=Math.abs(p-c);let predict=[0,a,up,Math.floor((a+up)/2),pa<=pb&&pa<=pc?a:pb<=pc?up:c][f];assert(predict!==undefined);row[i]=(x[y*(stride+1)+1+i]+predict)&255;}rows.push(row);}
 return [...rows[Math.floor(h/2)].subarray(Math.floor(w/2)*4,Math.floor(w/2)*4+4)];
}
function selectedScale(meta,legend,id,expectedPng){
 assert(meta&&!meta.error&&Array.isArray(meta.layers));assert(legend&&!legend.error&&Array.isArray(legend.layers));
 const ls=meta.layers.filter(l=>l.id===id),keys=legend.layers.filter(l=>l.layerId===id);assert.equal(ls.length,1);assert.equal(keys.length,1);
 const renderer=ls[0].drawingInfo.renderer,groups=renderer.uniqueValueGroups.flatMap(g=>g.classes).map(c=>({value:Number(c.values[0][0]),label:c.label,rgba:c.symbol.color})).sort((a,b)=>a.value-b.value);
 const infos=renderer.uniqueValueInfos?.map(c=>({value:Number(c.value),label:c.label,rgba:c.symbol.color})).sort((a,b)=>a.value-b.value);
 if(infos)assert.deepEqual(infos,groups);
 assert.equal(new Set(groups.map(x=>x.value)).size,groups.length);assert.equal(keys[0].legend.length,groups.length);
 groups.forEach(row=>{const found=keys[0].legend.filter(l=>l.label===row.label);assert.equal(found.length,1);assert.deepEqual(centerPng(found[0].imageData),expectedPng ? expectedPng[row.value-1] : row.rgba);});
 return groups;
}
function toolsContext(){
 const c=vm.createContext({window:{BRIM:{}},console,ptOpsPromotedGeneration:{}});
 for(const m of source.tools.matchAll(/^  function (\w+)\([^\n]*\) \{[\s\S]*?^  \}/gm))vm.runInContext(m[0],c);
 return c;
}
function qpfFixture(){
 const f=fixture();f.run(hover);
 f.context.L.tooltip=()=>({setLatLng(ll){this.ll=ll;return this;},setContent(s){this.html=s;return this;},addTo(m){m.addLayer(this);return this;}});
 f.context.L.point=(x,y)=>({x,y});f.map.latLngToContainerPoint=()=>({x:1,y:1});f.map.containerPointToLatLng=p=>({lat:p.y,lng:p.x});
 const original=f.context.fetch;f.context.fetch=url=>{
  if(/geometryType=/.test(url)){let resolve;const p=new Promise(r=>{resolve=r;});f.requests.push({url,resolve});return p;}
  return original(url);
 };return f;
}
function metaRequests(f){return f.requests.filter(r=>r.url.includes('outFields=product,valid_time'));}
function answer(r,json){r.resolve({ok:true,json:()=>Promise.resolve(json)});}
function card(f,id=qpfIds[0]){const c=f.container.querySelector('[data-forecast-product="'+id+'"]');assert(c);return c;}
for(let i=0;i<5;i++){
 test(qpfIds[i]+' complete captured 19-entry renderer/legend agreement',()=>{
  const f=fixture(),rows=selectedScale(fullMeta,fullLegend,selectors[i]);assert.equal(rows.length,19);assert.deepEqual(plain(f.context.ptWpcQpfScale),rows);assert.deepEqual(rows.map(r=>r.value),[0,.01,.1,.25,.5,.75,1,1.25,1.5,1.75,2,2.5,3,4,5,7,10,15,20]);
  assert.deepEqual(rows[0].rgba,[255,255,255,255]);
 });
 test(qpfIds[i]+' positive contour ticks preserve captured colors; zero uses an explanatory note',()=>{
  const f=fixture(),rows=selectedScale(fullMeta,fullLegend,selectors[i]).slice(1);f.context.ptOpsActivateLayerByName(qpfNames[i]);
  const scale=card(f,qpfIds[i]).querySelector('.pt-forecast-scale'),boundaries=scale.querySelectorAll('[data-qpf-boundary]');
  assert.equal(boundaries.length,18);
  boundaries.forEach((node,j)=>{
   assert.equal(Number(node.getAttribute('data-qpf-boundary')),rows[j].value);
   assert.equal(node.querySelector('.pt-ops-swatch').style.background,'rgba('+rows[j].rgba.slice(0,3).join(',')+','+(rows[j].rgba[3]/255)+')');
   assert.equal(node.getAttribute('role'),'listitem');assert.match(node.getAttribute('aria-label'),/inches/);
   // E45 changes visible high ticks only; captured labels/values/colors stay exact above.
   assert.equal(node.querySelector('.pt-qpf-boundary-tick').textContent,['0.01','0.10','0.25','0.50','0.75','1.00','1.25','1.50','1.75','2','2.50','3','4','5','7','10','15','20'][j]);
  });
  assert.equal(scale.querySelectorAll('.pt-qpf-contour-strip').length,2);
  assert.equal(scale.querySelectorAll('.pt-qpf-contour-band').length,17);
  assert.equal(scale.querySelectorAll('.pt-qpf-contour-cap').length,1);
  assert.equal(scale.querySelectorAll('.pt-qpf-contour-zero').length,0);
  assert(!scale.textContent.includes('0 — real zero'));
  assert.deepEqual(Array.from(scale.querySelectorAll('.pt-qpf-contour-note'),n=>n.textContent),['No color = no forecast precipitation']);
  assert.equal(f.notes.querySelectorAll('.pt-qpf-contour-scale').length,0);
 });
 test(qpfIds[i]+' captured timing cross-check without issue-zone invention',()=>{
  const c=fixture().context,attrs=data('qpf_attributes_'+selectors[i]).features;
  const m=c.ptForecastMetadata('qpf',attrs,false);assert(m.valid.includes('PDT'));assert(m.issued.includes('time zone Unverified'));assert(m.scope.includes('image-cycle match Unverified'));assert(m.scope.includes('bounded attribute sample'));
 });
}
for(let i=0;i<3;i++)test('ERO '+i+' selected four mapped fills and real interval',()=>{
 const f=fixture(),rows=selectedScale(data('ero_layers'),data('ero_legend'),i,[[56,168,0,255],[255,255,0,255],[245,0,0,255],[255,105,197,255]]);assert.equal(rows.length,4);assert.deepEqual(plain(f.context.ptWpcEroScale),rows);
 const attrs=data('ero_attributes_'+i).features;const m=f.context.ptForecastMetadata('ero',attrs,false);assert(m.issued.includes('time zone Unverified'));assert(m.valid.includes('PDT'));
 if(i===0){const interval=f.context.ptWpcForecastInterval(attrs[0].attributes);assert.equal(Date.parse(interval.end)-Date.parse(interval.start),20*3600000);}
});
for(const mutation of ['selector','color','label','missing','error'])test('full-capture scale rejects '+mutation,()=>{
 const m=plain(fullMeta),l=plain(fullLegend);
 if(mutation==='color')m.layers.find(x=>x.id===1).drawingInfo.renderer.uniqueValueInfos[0].symbol.color[0]=1;
 if(mutation==='label')l.layers.find(x=>x.layerId===1).legend[0].label='wrong';
 if(mutation==='missing')m.layers=[];if(mutation==='error')m.error={code:500};
 assert.throws(()=>selectedScale(m,l,mutation==='selector'?999:1),{code:'ERR_ASSERTION'});
});
for(const s of snap){
 test(s.product_label+' exact unchanged hover popup and style snapshots',()=>{
  const c=toolsContext();assert.equal(c.ptTooltipFromProperties(s.props,s.options),s.hover);assert.equal(c.ptPopupFromProperties(s.props,s.options),s.popup);
  assert.deepEqual(plain(c.ptStyleForExternalFeature({properties:s.props},'#000',.14,s.options)),s.style);
  s.ramps.forEach(r=>r.colors.forEach(v=>assert.equal(c.ptCpcOutlookColor(r.cat,v.prob,s.options),v.color)));
 });
 test(s.product_label+' descriptor from actual palette across every boundary',()=>{
  const c=toolsContext(),d=c.ptCpcForecastDescriptor(s.options);assert.equal(d.entries.length,17);
  for(const [group,cat] of ['A','B'].entries())for(let i=0;i<7;i++){
   const low=[0,40,50,60,70,80,90][i],high=[39.999,49.999,59.999,69.999,79.999,89.999,100][i];
   for(const v of [low,high])assert.equal(d.entries[group*7+i].color,c.ptCpcOutlookColor(cat,v,s.options));
  }
  assert.deepEqual(Array.from(d.entries.slice(14),x=>x.label),['Near Normal','Equal Chances','Unknown']);
 });
}
for(const n of ['cpc610_attributes_0','cpc610_attributes_1','cpc814_attributes_0','cpc814_attributes_1'])test(n+' UTC calendar dates remain dates',()=>{
 const m=fixture().context.ptForecastMetadata('cpc',data(n).features,false);assert.match(m.issued,/^\d{4}-\d{2}-\d{2}$/);assert.match(m.valid,/^\d{4}-\d{2}-\d{2} – \d{4}-\d{2}-\d{2}$/);assert(!/PDT|PST|GIS/.test(m.valid));
});
test('WPC explicit Z, full-year, invalid-calendar and DST evidence',()=>{
 const c=fixture().context;
 const a={valid_time:'00Z 09/17/26 - 00Z 09/18/26',start_time:'2026-09-17 00:00:00',end_time:'2026-09-18 00:00:00'};
 const x=c.ptWpcForecastInterval(a);assert(x.text.includes('5:00 PM PDT'));assert.equal(x.start,'2026-09-17T00:00:00.000Z');
 for(const b of [{...a,start_time:'2027-09-17 00:00:00'},{...a,valid_time:a.valid_time.replaceAll('Z','')},{...a,end_time:'2026-09-17 00:00:00'},{...a,start_time:'2026-02-30 00:00:00'}])assert.equal(c.ptWpcForecastInterval(b),null);
 const dst=c.ptWpcForecastInterval({valid_time:'08Z 11/01/26 - 12Z 11/01/26',start_time:'2026-11-01 08:00:00',end_time:'2026-11-01 12:00:00'});assert(dst.text.includes('PDT')&&dst.text.includes('PST'));
});
test('date parser rejects normalization and epoch-unit guesses',()=>{
 const c=fixture().context;
 for(const v of [null,undefined,true,false,NaN,Infinity,1789516800,'1789516800000','2026-02-30','2026-09-16T00:00:00','2026-9-16',1789516800001])assert.equal(c.ptForecastCalendarDate(v),null);
 assert.equal(c.ptForecastCalendarDate('2024-02-29'),'2024-02-29');assert.equal(c.ptForecastCalendarDate(1789516800000),'2026-09-16');
});
for(const family of ['qpf','ero','cpc'])test(family+' mixed incomplete empty invalid metadata is qualified',()=>{
 const c=fixture().context,features=plain(data(family==='qpf'?'qpf_attributes_1':family==='ero'?'ero_attributes_0':'cpc610_attributes_0').features);
 assert.equal(c.ptForecastMetadata(family,[],false).valid,'Unverified');assert.equal(c.ptForecastMetadata(family,features,true).valid,'Unverified');
 const mixed=[features[0],plain(features[0])];mixed[1].attributes[family==='cpc'?'fcst_date':'issue_time']=family==='cpc'?1789430400000:'2026-09-15 12:00:00';assert(c.ptForecastMetadata(family,mixed,false).scope.includes('mixed'));
 const invalid=[{attributes:{}}];assert(c.ptForecastMetadata(family,invalid,false).scope.includes('missing/invalid'));
});
test('threshold range units and nested legitimate numeric best-feature semantics',()=>{
 const c=hoverContext();assert.equal(c.formatWpcQpfAmount('<0.01','in'),'<0.01 in');assert.equal(c.formatWpcQpfAmount('0.25–0.50','Inches'),'0.25–0.50 in');
 for(const v of ['mm','feet','',null])assert.equal(c.formatWpcQpfAmount(.72,v),'');
 for(const v of ['>1-2','2-1','-999','0x10','1e3'])assert.equal(c.formatWpcQpfAmount(v,'in'),'');
 const fs=[null,'',false,.25,2,.75].map(qpf=>({attributes:{qpf,units:'Inches'}}));assert.equal(c.wpcQpfBestFeature(fs).attributes.qpf,2);assert.equal(c.wpcQpfBestFeature(fs.slice(0,3)),null);
});
test('QPF A-B-A rejects old images metadata and hover; preserves non-QPF',async()=>{
 const f=qpfFixture(),c=f.context,a=c.opsDefByName[qpfNames[0]].layer,b=c.opsDefByName[qpfNames[1]].layer;
 c.ptOpsActivateLayerByName('QPE | NWS MRMS 1-hr');c.ptOpsActivateLayerByName(qpfNames[0]);a._update();const old=f.images.at(-1),oldMeta=metaRequests(f).at(-1);c.queryWpcQpfAtLatLng({lat:38,lng:-120});const h=f.requests.at(-1);
 c.ptOpsActivateLayerByName(qpfNames[1]);b._update();const second=f.images.at(-1);c.ptOpsActivateLayerByName(qpfNames[0]);a._update();const current=f.images.at(-1);current.fire('load');
 old.fire('load');second.fire('error');answer(oldMeta,data('qpf_attributes_1'));answer(h,{features:[{attributes:{qpf:9,units:'in'}}]});await tick();
 assert.equal(a._overlay,current);assert(!f.map.hasLayer(old)&&!f.map.hasLayer(second));assert.equal(c.wpcQpfHoverTooltip,null);assert(card(f).textContent.includes('no loaded metadata'));assert.equal(f.container.querySelectorAll('.pt-ops-forecast-card').length,1);assert(f.map.hasLayer(c.opsDefByName['QPE | NWS MRMS 1-hr'].layer));f.unchanged();
});
for(const action of ['off','clear','clearAll','refresh','pan','zoom'])test('QPF pending response invalidated by '+action,async()=>{
 const f=qpfFixture(),c=f.context,layer=c.opsDefByName[qpfNames[0]].layer;c.ptOpsActivateLayerByName(qpfNames[0]);layer._update();const img=f.images.at(-1),meta=metaRequests(f).at(-1);c.queryWpcQpfAtLatLng({lat:38,lng:-120});const h=f.requests.at(-1),n=f.requests.length;
 if(action==='off')c.ptOpsDeactivateLayerByName(qpfNames[0]);if(action==='clear'||action==='clearAll')f.clear(action==='clearAll');if(action==='refresh')layer.refreshCurrentView();if(action==='pan'||action==='zoom'){f.map.fire(action==='pan'?'movestart':'zoomstart');f.map.fire(action==='pan'?'moveend':'zoomend');assert.equal(f.requests.length,n);}
 img.fire('load');answer(meta,data('qpf_attributes_1'));answer(h,{features:[{attributes:{qpf:9,units:'in'}}]});await tick();assert.equal(c.wpcQpfHoverTooltip,null);assert(!f.map.hasLayer(img));
 if(['off','clear','clearAll'].includes(action)){assert.equal(f.container.querySelectorAll('.pt-ops-forecast-card').length,0);c.ptOpsActivateLayerByName(qpfNames[0]);assert(card(f));}else{assert.equal(c.checkboxByName[qpfNames[0]].checked,true);assert(card(f).textContent.includes('no loaded metadata'));}
 f.unchanged();
});
test('cursor movement immediately clears old amount and cancels fallback ownership',async()=>{
 const f=qpfFixture(),c=f.context;c.ptOpsActivateLayerByName(qpfNames[0]);c.queryWpcQpfAtLatLng({lat:38,lng:-120});answer(f.requests.at(-1),{features:[{attributes:{qpf:0,units:'in'}}]});await tick();assert(c.wpcQpfHoverTooltip.html.includes('0.00 in'));
 c.scheduleWpcQpfHoverQuery({latlng:{lat:39,lng:-120}});assert.equal(c.wpcQpfHoverTooltip,null);c.queryWpcQpfAtLatLng({lat:39,lng:-120});const h=f.requests.at(-1),count=f.requests.length;f.map.fire('zoomstart');answer(h,{features:[]});await tick();assert.equal(f.requests.length,count);assert.equal(c.wpcQpfHoverTooltip,null);
});
test('map card metadata updates preserve DOM disclosure close docking and re-add',async()=>{
 const f=qpfFixture(),c=f.context;c.ptOpsActivateLayerByName(qpfNames[0]);const el=card(f),view=c.ptOpsForecastCards[qpfIds[0]],detail=el.querySelector('details');detail.setAttribute('open','');
 el.querySelector('.pt-forecast-close').click();const hidden=el.style.display;assert.equal(hidden,'none');answer(metaRequests(f).at(-1),data('qpf_attributes_1'));await tick();
 assert.equal(card(f),el);assert.equal(el.style.display,hidden);assert.notEqual(detail.getAttribute('open'),null);assert(el.textContent.includes('5:00 PM PDT'));assert(el.textContent.includes('time zone Unverified'));assert.equal(c.ptOpsForecastCards[qpfIds[0]],view);
 assert.equal(el.querySelector('.pt-forecast-close').getAttribute('type'),'button');assert.equal(el.querySelector('.pt-forecast-dock').getAttribute('type'),'button');assert(el.classList.contains('pt-map-legend-external'));
 c.ptOpsDeactivateLayerByName(qpfNames[0]);assert(!f.container.contains(el));c.ptOpsActivateLayerByName(qpfNames[0]);assert.notEqual(card(f),el);assert.notEqual(card(f).style.display,'none');
});
test('QPF metadata is bounded, no geometry/distinct query, not cursor assigned',async()=>{
 const f=qpfFixture();f.context.ptOpsActivateLayerByName(qpfNames[2]);const r=metaRequests(f)[0];assert.equal(f.requests.length,1, 'No unowned whole-service metadata request');assert(r.url.includes('/3/query'));const u=new URL(r.url);assert.equal(u.searchParams.get('returnGeometry'),'false');assert.equal(u.searchParams.get('returnDistinctValues'),'false');assert.equal(u.searchParams.get('resultRecordCount'),'100');assert.equal(u.searchParams.get('geometry'),null);
 answer(r,data('qpf_attributes_3'));await tick();assert(card(f,qpfIds[2]).textContent.includes('Provider forecast metadata'));assert.equal(f.context.wpcQpfHoverTooltip,null);
});
// Existing promoted-layer harness supplies actual registration, bridge, loader,
// generation/request guards and normal removal. Only provider/DOM rendering is controlled.
const promotedFixture=require('./test_ops_promoted_streamflow.js').fixture;
function cpcFixture(){
 const f=promotedFixture();f.t.PT2_CATALOG.push(...captured.catalog);
 for(const n of ['ptIsCpcOutlookStyle','ptCpcOutlookKind','ptCpcCategoryKey','ptCpcOutlookColor'])vm.runInContext(fn(source.tools,n),f.t);
 const service=raw(source.service);vm.runInContext(service.slice(service.indexOf('  function ptForecastDateParts(')),f.ops);return f;
}
for(let i=0;i<4;i++)test(snap[i].product_label+' real bridge/accepted collection dates and stale response rejection',()=>{
 const f=cpcFixture(),key=snap[i].options.opsPromotedKey,name=snap[i].product_label,attrs=data(['cpc610_attributes_0','cpc610_attributes_1','cpc814_attributes_0','cpc814_attributes_1'][i]).features;
 f.on(key);const old=f.pending.at(-1);f.off(key);f.on(key);const current=f.pending.at(-1);const fc={type:'FeatureCollection',features:attrs.map(x=>({type:'Feature',properties:x.attributes,geometry:{type:'Polygon',coordinates:[]}}))};
 current.completeRaw(null,fc);const metadata=plain(f.ops.activeLegendDefs[name].forecastMetadata);assert.match(metadata.issued,/^\d{4}-\d{2}-\d{2}$/);old.completeRaw(null,{features:[]});assert.deepEqual(plain(f.ops.activeLegendDefs[name].forecastMetadata),metadata);assert.equal(f.t.ptCustomLayers.filter(r=>r.opsPromotedKey===key).length,1);assert(f.ops.activeLegendDefs[name].forecastScale.entries.length===17);
 assert.equal(f.errors.filter(e=>e instanceof TypeError||e instanceof ReferenceError).length,0);
});
test('CPC refresh partial category responses qualify dates and stale refresh cannot take over',()=>{
 const f=cpcFixture(),key=snap[0].options.opsPromotedKey,name=snap[0].product_label,fc={features:data('cpc610_attributes_0').features.map(x=>({properties:x.attributes}))};f.on(key);f.pending.at(-1).completeRaw(null,fc);f.refresh(key);
 for(let i=0;i<4;i++)f.pending.at(-1).completeRaw(i===1?new Error('controlled missing category'):null,i===1?{features:[]}:fc);
 assert(f.ops.activeLegendDefs[name].forecastMetadata.scope.includes('incomplete'));assert.equal(f.ops.activeLegendDefs[name].forecastMetadata.valid,'Unverified');
 f.refresh(key);const old=f.pending.at(-1);f.off(key);old.completeRaw(null,fc);for(let i=1;i<4;i++)f.pending.at(-1).completeRaw(null,fc);assert.equal(f.ops.activeLegendDefs[name],undefined);
});
test('twelve exact identities/selectors and canonical cards do not collapse products',()=>{
 const c=fixture().context;
 const ids=[...qpfIds,...[1,2,3].map(x=>'ops_wpc_ero_day_'+x),...['6_10','8_14'].flatMap(p=>['temperature','precipitation'].map(k=>'ops_cpc_'+p+'_'+k))];assert.equal(new Set(ids).size,12);
 for(const id of ids){assert(c.ptOpsForecastFamily(id));assert(source.defs.includes("forecastProductId: '"+id+"'"));}
 assert.equal(c.ptOpsForecastFamily('ops_wpc_qpf_other'),null);assert.equal(c.ptOpsForecastFamily('ops_qpe_rfc_1day'),null);
 const cpc=toolsContext();ids.forEach((id,i)=>{const def={forecastProductId:id,note:'',sourceUrl:'https://example.invalid'};if(i>=8)def.forecastScale=cpc.ptCpcForecastDescriptor(snap[i-8].options);c.activeLegendDefs['product '+i]=def;});c.redrawLegend();assert.equal(Object.keys(c.ptOpsForecastCards).length,12);
 const owner=c.activeLegendDefs['product 8'];c.activeLegendDefs['alias']=owner;c.redrawLegend();assert.equal(Object.keys(c.ptOpsForecastCards).length,12);
 assert.equal(fixture().notes.querySelectorAll('.pt-ops-swatch').length,0);
});

function eroFixture(i){
 const f=fixture(),c=f.context,ar=raw(source.arc);c.console.error=()=>{};
 const bbox=f.map.getBounds();Object.assign(bbox,{getWest:()=>-125,getEast:()=>-115,getSouth:()=>32,getNorth:()=>42});f.map.getBounds=()=>bbox;
 c.fetch=url=>{let resolve;const p=new Promise(r=>resolve=r);f.requests.push({url,resolve});return p;};
 c.L.geoJSON=(fc,options)=>({fc,options,addTo(m){m.addLayer(this);return this;}});
 f.run(ar.slice(ar.indexOf('  function ptEroEscape('),ar.indexOf('  function makeRadarLayer(')));
 const owner=new c.WpcEroCurrentViewLayer({name:'WPC ERO Day '+(i+1),forecastProductId:'ops_wpc_ero_day_'+(i+1),url:'https://mapservices.weather.noaa.gov/vector/rest/services/hazards/wpc_precip_hazards/MapServer',layerId:i,sourceUrl:'https://www.wpc.ncep.noaa.gov/qpf/ero.php'});
 f.map.addLayer(owner);return {...f,owner};
}
for(let i=0;i<3;i++)test('ERO '+i+' actual accepted collection supplies card metadata; old generation rejected',async()=>{
 const f=eroFixture(i),c=f.context,attrs=data('ero_attributes_'+i).features,fc={features:attrs.map(f=>({properties:f.attributes}))},id='ops_wpc_ero_day_'+(i+1);
 answer(f.requests.at(-1),fc);await tick();const el=card(f,id);assert(el.textContent.includes('time zone Unverified'));assert(!el.textContent.includes('no loaded metadata'));assert.equal(f.owner._layer.fc.features.length,attrs.length);
 f.owner.refreshCurrentView();const old=f.requests.at(-1);f.owner.refreshCurrentView();answer(f.requests.at(-1),{features:[]});await tick();assert(el.textContent.includes('no loaded metadata'));answer(old,fc);await tick();assert(el.textContent.includes('no loaded metadata'));
 assert.equal(card(f,id),el);f.map.removeLayer(f.owner);assert(!f.container.contains(el));
});

// Actual owner classes and registered options, with controlled transport only.
function forecastFixture(){
 const f=qpfFixture(),c=f.context,ar=raw(source.arc),dj=raw(source.defs);c.console.error=()=>{};
 const bounds=f.map.getBounds();Object.assign(bounds,{getWest:()=>-125,getEast:()=>-115,getSouth:()=>32,getNorth:()=>42});f.map.getBounds=()=>bounds;
 c.fetch=url=>{let resolve;const promise=new Promise(r=>resolve=r);f.requests.push({url,resolve});return promise;};
 c.L.geoJSON=(fc,options)=>({fc,options,addTo(m){m.addLayer(this);return this;}});
 f.run(ar.slice(ar.indexOf('  function ptEroEscape('),ar.indexOf('  function makeRadarLayer(')));
 f.run(dj.slice(dj.indexOf('  var ptOpsPromotedSlowTimers = {};'),dj.indexOf('  // AirNow and smoke layers moved to External Layers.')));
 c.WPC_ERO='https://mapservices.weather.noaa.gov/vector/rest/services/hazards/wpc_precip_hazards/MapServer';
 c.checkWpcEro=()=>{};
 const pendingBridge=[],bridgeRemoved=[],tools=toolsContext();
 f.window.ptOpsExternalCatalogBridge={
  cpcLegendDescriptor(name){const row=snap.find(x=>x.product_label===name);return tools.ptCpcForecastDescriptor(row.options);},
  addLayer(o){pendingBridge.push(o);o.onOwnership(pendingBridge.length);return {ok:true};},
  removeLayer(o){bridgeRemoved.push(o);return {ok:true};}, refreshLayer(){return {ok:true};}
 };
 const more=['ops_wpc_ero_day_1','ops_wpc_ero_day_2','ops_wpc_ero_day_3','ops_cpc_6_10_temperature','ops_cpc_6_10_precipitation','ops_cpc_8_14_temperature','ops_cpc_8_14_precipitation'];
 for(const id of more){const at=dj.indexOf("forecastProductId: '"+id+"'"),start=dj.lastIndexOf('  addOpsLayer({',at),end=dj.indexOf('\n  });',at)+6;assert(start>=0&&end>at);f.run(dj.slice(start,end));f.bindOpsDefinition(c.opsLayers.at(-1));}
 const defs=c.opsLayers.filter(d=>c.ptOpsForecastFamily(d.layer.options.forecastProductId));assert.equal(defs.length,12);
 return {...f,defs,pendingBridge,bridgeRemoved};
}
const forecastIds=[...qpfIds,...[1,2,3].map(x=>'ops_wpc_ero_day_'+x),...['6_10','8_14'].flatMap(p=>['temperature','precipitation'].map(k=>'ops_cpc_'+p+'_'+k))];
for(const from of forecastIds)for(const to of forecastIds)if(from!==to)test('exact forecast transition '+from+' → '+to,async()=>{
 const f=forecastFixture(),c=f.context,a=f.defs.find(d=>d.layer.options.forecastProductId===from),b=f.defs.find(d=>d.layer.options.forecastProductId===to);
 c.ptOpsActivateLayerByName('QPE | NWS MRMS 1-hr');c.ptOpsActivateLayerByName(a.name);
 if(a.layer._update)a.layer._update();
 const oldRequests=[...f.requests],oldImages=[...f.images],oldBridge=[...f.pendingBridge];
 if(c.ptIsWpcQpfOwner(a.layer)){c.queryWpcQpfAtLatLng({lat:38,lng:-120});oldRequests.push(f.requests.at(-1));}
 const oldCard=card(f,from);oldCard.querySelector('.pt-forecast-dock').click();
 c.ptOpsActivateLayerByName(b.name);
 assert(!f.map.hasLayer(a.layer));assert.equal(a.layer._isRemoved,true);assert.equal(c.checkboxByName[a.name].checked,false);assert.equal(c.activeLayers[a.name],undefined);assert(!f.container.contains(oldCard));
 assert(f.map.hasLayer(b.layer));assert.equal(c.checkboxByName[b.name].checked,true);assert.equal(f.container.querySelectorAll('.pt-ops-forecast-card').length,1);
 const before=plain(c.activeLegendDefs[b.name]);
 oldImages.forEach(x=>x.fire('load'));oldRequests.forEach(r=>answer(r,{features:[]}));oldBridge.forEach(o=>o.onOwnership(99999));await tick();
 assert.deepEqual(plain(c.activeLegendDefs[b.name]),before);assert.equal(c.activeLegendDefs[a.name],undefined);assert.equal(c.wpcQpfHoverTooltip,null);assert.equal(f.container.querySelectorAll('.pt-ops-forecast-card').length,1);
 if(from.startsWith('ops_cpc_')){assert.equal(a.layer._opsPromotedGeneration,null);assert(f.bridgeRemoved.length>0);}
 assert(f.map.hasLayer(c.opsDefByName['QPE | NWS MRMS 1-hr'].layer));f.unchanged();
 c.ptOpsDeactivateLayerByName(b.name);assert.equal(f.container.querySelectorAll('.pt-ops-forecast-card').length,0);assert(!f.defs.some(d=>f.map.hasLayer(d.layer)));
});
for(const family of ['qpf','ero','cpc'])for(const action of ['off','clear','clearAll'])test('forecast '+family+' '+action+' removes floating card and re-enables once',()=>{
 const f=forecastFixture(),c=f.context,d=f.defs.find(d=>c.ptOpsForecastFamily(d.layer.options.forecastProductId)===family);
 c.ptOpsActivateLayerByName(d.name);const el=card(f,d.layer.options.forecastProductId);el.querySelector('.pt-forecast-dock').click();
 if(action==='off')c.ptOpsDeactivateLayerByName(d.name);else f.clear(action==='clearAll');assert(!f.container.contains(el));assert(!f.defs.some(x=>f.map.hasLayer(x.layer)));
 c.ptOpsActivateLayerByName(d.name);assert.equal(f.container.querySelectorAll('.pt-ops-forecast-card').length,1);f.unchanged();
});
test('programmatic Ops wrapper activation uses exact forecast exclusion; QPF predicate remains five-only',()=>{
 const f=forecastFixture(),c=f.context;for(const d of f.defs){f.map.addLayer(d.layer);assert.equal(f.defs.filter(x=>f.map.hasLayer(x.layer)).length,1);assert.equal(c.ptIsWpcQpfOwner(d.layer),qpfIds.includes(d.layer.options.forecastProductId));}f.unchanged();
});
test('H1 same query selected maximum owns timing; mixed/partial timing stays unverified',async()=>{
 const f=qpfFixture(),c=f.context;c.ptOpsActivateLayerByName(qpfNames[0]);const attrs=data('qpf_attributes_1').features[0].attributes;
 for(const mode of ['coherent','mixed','missing','partial']){
  c.queryWpcQpfAtLatLng({lat:38,lng:-120});const a={...attrs,qpf:.25,units:'in'},b={...attrs,qpf:2,units:'in'};
  if(mode==='mixed')a.start_time='2027-09-17 00:00:00';if(mode==='missing')delete b.valid_time;
  answer(f.requests.at(-1),{features:[{attributes:a},{attributes:b}],exceededTransferLimit:mode==='partial'});await tick();const html=c.wpcQpfHoverTooltip.html;
  assert(html.includes('2.00 in'));assert.equal(html.includes('Valid time unverified'),mode!=='coherent');assert.equal(html.includes(' hr'),mode==='coherent');
 }
});
test('H1 existing single envelope fallback is labelled nearby; query unchanged',async()=>{
 const f=qpfFixture(),c=f.context;c.ptOpsActivateLayerByName(qpfNames[0]);const initial=f.requests.length;c.queryWpcQpfAtLatLng({lat:38,lng:-120});const point=f.requests.at(-1);answer(point,{features:[]});await tick();const envelope=f.requests.at(-1);
 assert.equal(new URL(point.url).searchParams.get('geometryType'),'esriGeometryPoint');assert.equal(new URL(envelope.url).searchParams.get('geometryType'),'esriGeometryEnvelope');
 answer(envelope,{features:[{attributes:{...data('qpf_attributes_1').features[0].attributes,qpf:.72,units:'in'}}]});await tick();assert(c.wpcQpfHoverTooltip.html.includes('Nearby WPC polygon'));assert.equal(f.requests.length,initial+2);
});

for(let i=0;i<4;i++)for(let j=0;j<4;j++)if(i!==j)test('CPC real bridge transition '+i+' → '+j+' rejects obsolete initial construction',()=>{
 const f=cpcFixture(),from=snap[i].options.opsPromotedKey,to=snap[j].options.opsPromotedKey;
 f.loaded();const retained=f.record();f.on(from);const old=f.pending.at(-1);f.on(to);const current=f.pending.at(-1);
 const fc={features:data(j<2?'cpc610_attributes_0':'cpc814_attributes_0').features.map(x=>({properties:x.attributes}))};
 current.completeRaw(null,fc);const state=f.ownerState(to);old.completeRaw(null,fc);
 assert.equal(f.ownerState(to),state);assert(!f.record(from));assert(f.record(to));assert.equal(f.record(),retained);
 assert.equal(f.owners.get(from).chk.checked,false);assert.equal(f.ops.activeLayers[snap[i].product_label],undefined);
 assert.equal(f.t.ptCustomLayers.filter(r=>[from,to].includes(r.opsPromotedKey)).length,1);
 f.off(to);assert(!f.record(to));assert.equal(f.record(),retained);
});

// E45 menu fixture: actual authored row metadata and actual panel sorter/HTML.
// All 50 configured rows participate; transport/layer construction is excluded.
let panelProjection;
function panelInventory(definitions=source.defs) {
 const f=forecastFixture(),c=f.context;
 const owners=new Map(c.opsLayers.map(d=>[d.name,d.layer]));
 if(!panelProjection)panelProjection=JSON.parse(execFileSync(process.env.RSCRIPT_BINARY || 'Rscript',['--vanilla','-e','source("03_functions/leaflet_ops_live_helpers.r");cat(jsonlite::toJSON(pt_ops_live_delivery_projection(),auto_unbox=TRUE,na="null"))'],{cwd:path.join(__dirname,'..'),encoding:'utf8'}));
 c.ptOpsDeliveryProjection=c.ptOpsDeliveryRows(panelProjection);assert.equal(c.ptOpsDeliveryProjection.length,49);
 const moduleNames=['layer_definition','wind','hrrr_wind','nbm_wind','nbm_snow_levels','nbm_accumulated_qpf','observed_wind'];
 const texts=moduleNames.map(n=>n==='layer_definition'?definitions:fs.readFileSync(path.join(__dirname,'../03_functions/leaflet_ops_live_'+n+'_helpers.r'),'utf8'));
 const all=texts.join('\n')+'\n'+source.service;
 for(const [_,name,value] of all.matchAll(/var (PT_\w+) = ['"]([^'"]+)['"];?/g))c[name]=value;
 for(const m of raw(source.service).matchAll(/^  var [A-Z_]+ = '[^']+';/gm))f.run(m[0]);
 const metadata=[];
 // Each registration authors its panel fields before layer:. No lifecycle code
 // is evaluated. Unrelated URL globals are inert, named offline sentinels.
 for(const text of texts)for(const m of raw(text).matchAll(/^[ \t]*addOpsLayer\(\{([\s\S]*?)\blayer:\s*/gm)){
  const head=m[1];for(const token of head.match(/\b[A-Z][A-Z_0-9]+\b/g)||[])if(!(token in c))c[token]='offline:'+token;
  const d=f.run('({'+head+'layer:{options:{}}})');if(owners.has(d.name))d.layer=owners.get(d.name);metadata.push(d);
 }
 assert.equal(metadata.length,50,'Complete configured row inventory');
 c.opsLayers=[];metadata.forEach(d=>c.addOpsLayer(d));
 for(const n of ['ptOpsOrderIndex','ptOpsSubgroupOrder','ptOpsPanelEntries','buildOpsPanelHtml'])f.run(fn(raw(source.panel),n));
 for(const n of ['infoLabelForDef','layerLinkItems','ptOpsDefIsRefreshable','ptOpsRefreshActionHtml','layerRowLinksHtml'])f.run(fn(raw(source.shared),n));
 c.opsExternalLinksForCategory=()=>'';c.ptGibsMetadataHtml=()=>'';c.ptGibsRowHtml=()=>'';
 const entries=c.ptOpsPanelEntries(),html=c.buildOpsPanelHtml();
 const indexes=[...html.matchAll(/data-pt-ops-row-index="(\d+)"/g)].map(m=>Number(m[1]));
 assert.deepEqual(indexes,Array.from(entries,e=>e.idx));
 return {f,entries,html,rows:metadata,indexes};
}
test('ERO rows follow the complete five-QPF block in actual sorted panel HTML',()=>{
 const p=panelInventory(),names=Array.from(p.entries,e=>e.def.name),start=names.indexOf('WPC QPF Day 1');
 assert.deepEqual(names.slice(start,start+8),[...qpfNames,'WPC ERO Day 1','WPC ERO Day 2','WPC ERO Day 3']);
 for(let day=1;day<=3;day++){
  const e=p.entries.filter(e=>e.def.name==='WPC ERO Day '+day);assert.equal(e.length,1);
  assert.equal(e[0].def.category,'Forecasts / Outlooks');assert.equal(e[0].def.subgroup,'Weather Forecasts / Outlooks');
  assert.equal(e[0].def.guideProductId,'ops_wpc_ero_day_'+day);
  assert.equal((p.html.match(new RegExp('data-pt-ops-row-index="'+e[0].idx+'"','g'))||[]).length,1);
 }
});
test('all rendered row checkboxes default OFF and ERO source links/badges remain owned',()=>{
 const p=panelInventory();assert.equal(p.indexes.length,50);
 const checkboxes=[...p.html.matchAll(/<input[^>]*data-pt-ops-index="\d+"[^>]*>/g)];assert.equal(checkboxes.length,50);assert(checkboxes.every(m=>!m[0].includes(' checked')));
 for(let day=1;day<=3;day++){
  const e=p.entries.find(e=>e.def.name==='WPC ERO Day '+day),start=p.html.indexOf('data-pt-ops-row-index="'+e.idx+'"'),tail=p.html.slice(start),row=tail.slice(0,tail.indexOf('</div></div>')+12);
  assert(row.includes('https://www.wpc.ncep.noaa.gov/qpf/ero.php?day='+day+'&amp;opt=curr'));assert.equal((row.match(/role="img"/g)||[]).length,e.def.deliveryClass==='brim_enhanced'||e.def.deliveryClass==='brim_managed'?1:0);
 }
});



test('E48 QPE, forecast and actual satellite owner groups coexist and stay independent',()=>{
 const f=forecastFixture(),c=f.context;c.AbortController=AbortController;
 f.run(raw(fs.readFileSync(path.join(__dirname,'../03_functions/leaflet_ops_live_gibs_helpers.r'),'utf8')));
 // Hold metadata pending without transport: this case isolates owner selection.
 // Full captured binding/drawing/date/zoom regressions remain in the GIBS suite.
 c.ptGibsOwner.check=()=>new Promise(()=>{});
 const satIds=['ops_goes_geocolor','ops_goes_infrared'];
 const satellites=satIds.map(id=>{const layer=c.makeGibsImageryLayer(id),d={name:layer.options.name,layer};f.bindOpsDefinition(d);return d;});
 const otherFamilies=['NBM retained fixture','CoCoRaHS retained fixture','Local retained fixture','External retained fixture'];
 const others=otherFamilies.map(name=>{const layer={options:{name},addTo(m){m.addLayer(this);return this;}};f.bindOpsDefinition({name,layer});c.ptOpsActivateLayerByName(name);return layer;});
 c.ptOpsActivateLayerByName('Radar | NOAA MRMS');c.ptOpsActivateLayerByName(satellites[0].name);
 for(const qpe of ['QPE | NWS MRMS 1-hr','QPE | NWS RFC mosaic 7-day']) {
  c.ptOpsActivateLayerByName(qpe);
  for(const forecast of ['WPC QPF Day 1','WPC ERO Day 1','CPC 6-10 Day Temperature Outlook']) {
   c.ptOpsActivateLayerByName(forecast);
   assert(f.map.hasLayer(c.opsDefByName[qpe].layer));assert(f.map.hasLayer(c.opsDefByName[forecast].layer));
   assert(f.map.hasLayer(satellites[0].layer));assert.equal(f.defs.filter(d=>f.map.hasLayer(d.layer)).length,1);
   assert(f.map.hasLayer(c.opsDefByName['Radar | NOAA MRMS'].layer));others.forEach(l=>assert(f.map.hasLayer(l)));
  }
 }
 for (const name of ['Radar | IEM NEXRAD','Radar | NOAA MRMS','Radar | IEM NEXRAD']) {
  c.ptOpsActivateLayerByName(name);
  for (const other of ['Radar | IEM NEXRAD','Radar | NOAA MRMS']) assert.equal(f.map.hasLayer(c.opsDefByName[other].layer),other===name);
  assert(f.map.hasLayer(c.opsDefByName['QPE | NWS RFC mosaic 7-day'].layer));
  assert(f.map.hasLayer(c.opsDefByName['CPC 6-10 Day Temperature Outlook'].layer));
  assert(f.map.hasLayer(satellites[0].layer));others.forEach(l=>assert(f.map.hasLayer(l)));
 }
 c.ptOpsActivateLayerByName(satellites[1].name);assert(!f.map.hasLayer(satellites[0].layer));assert(f.map.hasLayer(satellites[1].layer));
 assert(f.map.hasLayer(c.opsDefByName['QPE | NWS RFC mosaic 7-day'].layer));assert(f.map.hasLayer(c.opsDefByName['CPC 6-10 Day Temperature Outlook'].layer));
 f.unchanged();
});

const cpcPanelIds=['ops_cpc_6_10_temperature','ops_cpc_6_10_precipitation','ops_cpc_8_14_temperature','ops_cpc_8_14_precipitation'];
const cpcPanelLabels=['CPC 6-10 Day Temp. Outlook','CPC 6-10 Day Precip. Outlook','CPC 8-14 Day Temp. Outlook','CPC 8-14 Day Precip. Outlook'];
for(let i=0;i<4;i++)test('E48 CPC panel-only short label '+i+' retains canonical owner and actions',()=>{
 const p=panelInventory(),full=snap[i].product_label,e=p.entries.find(e=>e.def.name===full);assert(e);
 assert.equal(e.def.panelLabel,cpcPanelLabels[i]);
 const start=p.html.indexOf('data-pt-ops-row-index="'+e.idx+'"'),tail=p.html.slice(start),row=tail.slice(0,tail.indexOf('</div></div>')+12);
 assert(row.replace(/<[^>]*>/g,'').includes(cpcPanelLabels[i]));assert(row.includes('data-pt-ops-index="'+e.idx+'"'));
 for(const action of ['rfrsh','lgnd','srce'])assert(row.includes('>'+action+'</'),action);
 assert.equal((row.match(/role="img"/g)||[]).length,1);assert(row.includes(e.def.sourceUrl.replace(/&/g,'&amp;')));
 const f=forecastFixture(),d=f.defs.find(d=>d.name===full);assert(d);
 assert.equal(d.layer.options.name,full);assert.equal(d.layer.options.catalogSourceDisplayName,full);assert.equal(d.layer.options.sourceDisplayName,full);
 assert.equal(d.layer.options.opsKey,snap[i].options.opsPromotedKey);
 assert.equal(d.layer.options.forecastProductId,cpcPanelIds[i]);
 f.context.ptOpsActivateLayerByName(full);assert(card(f,cpcPanelIds[i]).textContent.includes(full));
});

module.exports={panelInventory,qpfFixture,cpcFixture,toolsContext,forecastFixture,data,snap,card,answer,tick};
if(require.main===module)(async()=>{let passed=0,failed=0;for(const t of cases){try{await t.run();passed++;console.log('PASS '+t.name);}catch(e){if(e.code!=='ERR_ASSERTION')throw e;failed++;console.log('FAIL '+t.name+': '+e.message);}}console.log('RESULT passed='+passed+' failed='+failed);process.exitCode=failed?1:0;})().catch(e=>{console.error('HARNESS_ERROR',e.stack);process.exitCode=2;});
