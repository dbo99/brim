/* Shared Ops ownership. Uses ordinary Ops switches, Local Reference predicates,
 * BRIM legendCloseout docking, and accepted source popup renderers. */
var ptSoilController = null;
function makeSoilMoistureLayer(source,name,indexUrl) {
  if(!ptSoilController)ptSoilController=createSoilMoistureController();
  return ptSoilController.register(source,name,indexUrl);
}
function createSoilMoistureController(){
'use strict';
const E=window.BRIM.soilMoistureEngine, T=window.BRIM.soilMoistureTransport, D=window.BRIM.soilMoistureDepths, UI=window.BRIM.soilMoistureUI, Charts=window.BRIM.soilMoistureCharts;
const engine=E.create(), sources={}, markers=new Map(), bundles=new T.BundleCache();
const bundleMetrics=[];
let hoverOwner=null;
function clearHover(){const previous=hoverOwner;hoverOwner=null;if(previous)previous.closeTooltip();}
function releaseMarker(marker){if(hoverOwner===marker)clearHover();marker.unbindTooltip();marker.off();}
let control=null,card=null,reopen=null,detachable=null,timer=null,clockTimer=null,results=[],pageSize=20,popup=null,popupAbort=null,popupEpoch=0,popupLayoutTimer=null,transactionMs=[],listeners=[];
const esc=s=>String(s??'').replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const label=k=>({scan:'SCAN',dendra:'dendra',snotel:sources.snotel?.staticReader?'SNOTEL (static review)':'SNOTEL (pilot)'})[k];
function listen(target,event,fn,dom=false){if(dom)target.addEventListener(event,fn);else target.on(event,fn);listeners.push({target,event,fn,dom});}
function cancelPopup(){clearHover();clearTimeout(popupLayoutTimer);popupLayoutTimer=null;for(const p of Object.values(sources))if(p.renderer?.dendra.popupOpen)map.closePopup();popupEpoch++;popupAbort?.abort();popupAbort=null;if(popup)map.closePopup(popup);popup=null;}
function settlePopup(){if(!popup)return;popup.update();clearTimeout(popupLayoutTimer);const target=popup;popupLayoutTimer=setTimeout(()=>{if(popup===target)target.update();},300);}
function popupLayout(){const wide=window.innerWidth>=900;return {maxWidth:Math.max(220,Math.min(660,window.innerWidth-(wide?400:80))),maxHeight:Math.max(230,window.innerHeight-210),autoPanPaddingTopLeft:[wide?330:12,85],autoPanPaddingBottomRight:[25,75]};}
async function bounded(url, limit, signal, hash, expectedBytes, metrics, redirect='follow') {
  const started = performance.now();
  const response = await fetch(url, {signal, cache:'no-store', redirect});
  if (!response.ok) throw Error('HTTP ' + response.status);
  const reader = response.body.getReader(), chunks = [];
  let size = 0;
  try {
    for (;;) {
      const {value, done} = await reader.read();
      if (done) break;
      size += value.length;
      if (size > limit) throw Error('Prepared file exceeds bound');
      chunks.push(value);
    }
  } finally { await reader.cancel(); }
  if (expectedBytes !== undefined && size !== expectedBytes) throw Error('Prepared byte count mismatch');
  const bytes = new Uint8Array(size);
  let offset = 0;
  chunks.forEach(chunk => { bytes.set(chunk, offset); offset += chunk.length; });
  if (hash) {
    const actual = Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256', bytes)))
      .map(v => v.toString(16).padStart(2, '0')).join('');
    if (actual !== hash) throw Error('Prepared generation hash mismatch');
  }
  const received = performance.now();
  const body = JSON.parse(new TextDecoder().decode(bytes));
  if (metrics) Object.assign(metrics, {bytes:size, requests:1,
    fetch_verify_ms:received-started, parse_ms:performance.now()-received});
  return body;
}
async function expandIndex(index,key,readShard){
 if(index.schema==='brim-soil-moisture-1')return validateIndex(index,key);
 if(index.schema!=='brim-soil-moisture-2'||key!=='dendra'||index.source!==key||index.stations||!Array.isArray(index.shards)||!index.shards.length||index.shards.length>128)throw Error('Invalid sharded source index');
 const stations=[],paths=new Set();
 for(const d of index.shards){if(!d||!Number.isInteger(d.bytes)||d.bytes<=0||d.bytes>262144||!/^[a-f0-9]{64}$/.test(d.sha256)||d.path!==`dendra-index-${String(paths.size).padStart(3,'0')}-${d.sha256}.json`||paths.has(d.path))throw Error('Invalid common shard descriptor');paths.add(d.path);const b=await readShard(d);if(b.schema!=='brim-soil-moisture-shard-2'||b.source!==key||b.generation!==index.generation||!Array.isArray(b.stations))throw Error('Mixed/invalid source shard');stations.push(...b.stations);if(stations.length>1500)throw Error('Source station bound');}
 return validateIndex({...index,schema:'brim-soil-moisture-1',stations},key);
}
// Component opt-in only: no Product, URL default, primary or current value is installed.
function staticCatalog(context){
 const stations=context.stations.stations.map(st=>({source:'snotel',id:st.stationTriplet,key:'snotel:'+st.stationTriplet,
  name:st.name,provider:'NRCS AWDB',primary_sensor:null,requires_explicit_sensor:true,
  coordinates:st.coordinates? [st.coordinates.longitude,st.coordinates.latitude]:null,raw:st,
  sensors:st.sensors.map(s=>({id:`SMS:${s.signed_depth}:${s.ordinal}`,parameter:'soil_moisture',
   depth_native:s.signed_depth,depth_unit:'in',depth_mm:Math.abs(s.signed_depth)*25.4,unit:'percent VWC',
   history:{available:true},observation:null,raw:s,
   capabilities:{latest_vwc:false,recent_change:false,source_context:false,common_reference:false,companion:true}}))}));
 return validateIndex({schema:'brim-soil-moisture-1',source:'snotel',generation:T.snotelStatic.manifest,
  note:'Local static candidate · not live',stations,counts:{stations:stations.length,
   sensors:context.stations.sensor_count,imported_histories:context.stations.sensor_count}},'snotel');
}
function registerSnotelStatic(name,packageUrl){
 const url=new URL(packageUrl,location.href);
 if(url.protocol!=='http:'||!['localhost','127.0.0.1','[::1]'].includes(url.hostname)||url.username||url.password||url.search||url.hash||!url.pathname.endsWith('/PACKAGE_MANIFEST.json'))throw Error('Local SNOTEL package URL required');
 const reader=new T.SnotelStaticReader((d,signal)=>bounded(new URL(d.path,url).href,
  d.bytes,signal,d.sha256,d.bytes,undefined,'error'),bundles);
 return register('snotel',name,url.href,reader);
}
function validateIndex(index,key){if(index.schema!=='brim-soil-moisture-1'||index.source!==key||typeof index.generation!=='string'||!Array.isArray(index.stations)||index.stations.length>1500)throw Error('Invalid source index');const seen=new Set();let n=0;for(const st of index.stations){if(st.source!==key||st.key!==key+':'+st.id||seen.has(st.key)||typeof st.name!=='string'||!Array.isArray(st.sensors))throw Error('Invalid station identity');if(st.source_url&&!/^https:\/\//.test(st.source_url))throw Error('Invalid public source URL');seen.add(st.key);st.sensors=st.sensors.map(s=>Object.assign({},index.sensor_defaults,s));n+=st.sensors.length;if(n>10000)throw Error('Sensor bound');if(st.coordinates!==null&&(!Array.isArray(st.coordinates)||st.coordinates.length!==2||!st.coordinates.every(Number.isFinite)||Math.abs(st.coordinates[0])>180||Math.abs(st.coordinates[1])>90))throw Error('Invalid coordinates');const ids=new Set();for(const s of st.sensors){if(typeof s.id!=='string'||ids.has(s.id)||s.depth_mm!==null&&(!Number.isFinite(s.depth_mm)||s.depth_mm<0))throw Error('Invalid sensor identity/depth');if(typeof s.capabilities!=='object'||['latest_vwc','recent_change','source_context','common_reference','companion'].some(k=>typeof s.capabilities[k]!=='boolean'))throw Error('Invalid capability declaration');const o=s.observation;if(o&&typeof o.eligible!=='boolean')throw Error('Invalid eligibility');if(s.capabilities.recent_change){for(const w of ['7','14','30']){const c=s.change?.[w];if(!c||!['wetting','drying','little','stale','insufficient','no-data'].includes(c.state)||c.delta!==null&&!Number.isFinite(c.delta)||['wetting','drying','little'].includes(c.state)&&c.delta===null)throw Error('Invalid change summary');}}if(o&&o.eligible&&(!Number.isFinite(o.value)||o.value<0||o.value>100||s.unit!=='percent VWC'||!/^\d{4}-\d{2}-\d{2}$/.test(o.date)||!Number.isFinite(Date.parse(o.date+'T00:00:00Z'))||typeof o.statistic!=='string'))throw Error('Invalid eligible observation/unit');ids.add(s.id);}if(st.primary_sensor!==null&&!ids.has(st.primary_sensor))throw Error('Invalid primary sensor');}if(index.counts?.stations!==index.stations.length||index.counts?.sensors!==n||index.counts?.imported_histories!==index.stations.reduce((n,s)=>n+s.sensors.filter(v=>v.history?.available).length,0))throw Error('Invalid counts');return index;}
function all(){return Object.values(sources).flatMap(p=>p.index?.stations||[]);}
function depthLabel(s){if(!s)return'No displayed sensor';if(s.depth_mm===null)return s.depth_status==='UNKNOWN'?'Unknown depth':'Unspecified depth';if(engine.state.unit==='mm')return s.depth_mm+' mm';return Math.abs(s.depth_native)+' '+s.depth_unit;}
function valueText(s){const o=s?.observation;return o?.eligible?Number(o.value.toFixed(2))+'% · '+o.date+' · '+E.age(s,Date.now())+' d old':'No imported eligible value';}
function symbol(s){const v=E.modeValue(s,engine.state.mode,Date.now(),engine.state.window);if(v.value===null)return{color:'#aab3b5',text:'?',reason:v.reason};if(engine.state.mode==='availability')return{color:v.value?'#328575':'#fff',text:v.value?'•':'○',reason:v.reason};if(engine.state.mode==='change')return{color:v.value>.5?'#267eaa':v.value<-.5?'#a86930':'#72838a',text:v.value>.5?'↑':v.value<-.5?'↓':'↔',reason:v.reason};return{color:v.value<10?'#c99852':v.value<25?'#92b8a1':v.value<50?'#438e95':'#315c94',text:'•',reason:v.reason};}
// Both map and list explain the same applied match, never the pending draft.
function admissionText(result) {
  const other = result.matches.filter(sensor => sensor.id !== result.display?.id);
  return other.length ? 'Included via ' + other.map(sensor =>
    depthLabel(sensor) + ': ' + valueText(sensor)).join('; ') : '';
}
function admissionHtml(result) {
  const text = admissionText(result);
  return text ? '<span class="sm-match">' + esc(text) + '</span>' : '';
}
function redraw(user=false) {
  const start = performance.now();
  // Icon/tooltip refresh can replace the hovered DOM before mouseout arrives.
  // Close through the owning Leaflet layer before touching that DOM.
  clearHover();
  results = engine.results(all());
  const keep = new Set();
  for (const result of results) {
    const station = result.station;
    if (!station.coordinates) continue;
    keep.add(station.key);
    const appearance = symbol(result.display);
    const icon=L.divIcon({className:'sm-provider-marker',html:UI.providerSymbol(station.source,appearance.color,appearance.text==='?'),iconSize:[20,20],iconAnchor:[10,10]});
    let marker = markers.get(station.key);
    if (!marker) {
      marker = L.marker([station.coordinates[1], station.coordinates[0]], {
        pane:'pane_ops', icon, keyboard:true, alt:station.name
      });
      marker.on('click', () => select(station.key));
      // Leaflet forwards keypress, but its Enter opener is installed only by
      // bindPopup. These popups belong to the shared selection controller.
      marker.on('keypress', e => {if(e.originalEvent.key==='Enter'||e.originalEvent.keyCode===13){e.originalEvent.preventDefault();select(station.key);}});
      marker.on('tooltipopen',()=>{if(hoverOwner!==marker)clearHover();hoverOwner=marker;});
      marker.on('tooltipclose',()=>{if(hoverOwner===marker)hoverOwner=null;});
      marker.on('mouseout remove',()=>{if(hoverOwner===marker)clearHover();});
      markers.set(station.key, marker);
      marker.addTo(sources[station.source].layer);
    }
    marker.setIcon(icon);
    // Accessible marker name without the native title's duplicate hover.
    marker.getElement().setAttribute('aria-label',station.name+' · '+label(station.source));
    const admission = admissionHtml(result);
    const hoverHtml='<b class="sm-hover-name">' + esc(station.name) + '</b><div class="sm-hover-source">' +
      esc(label(station.source) + ' · ' + depthLabel(result.display)) + '</div><div class="sm-hover-measure">' +
      esc(valueText(result.display).replace('% ·','% VWC ·')) + '</div>' + admission +
      (engine.state.mode==='change'?'<div>'+esc(appearance.text==='?'?'Change unavailable for this observation':Number(E.modeValue(result.display,'change',Date.now(),engine.state.window).value.toFixed(2))+' pp over '+engine.state.window+'-day windows')+'</div>':'');
    // Passing options to bindTooltip again replaces the vendor tooltip object;
    // update the existing one so an open predecessor cannot be orphaned.
    if(marker.getTooltip())marker.setTooltipContent(hoverHtml);
    else marker.bindTooltip(hoverHtml,{className:'sm-station-tooltip'});
  }
  for (const [key, marker] of markers) {
    if (!keep.has(key)) {
      releaseMarker(marker);
      for (const source of Object.values(sources)) {
        if (source.layer.hasLayer(marker)) source.layer.removeLayer(marker);
      }
      markers.delete(key);
    }
  }
  renderResults();
  renderStatus();
  if (user && engine.state.autoZoom) fit();
  transactionMs.push(performance.now() - start);
}
function fit(rows=results){const xy=rows.map(r=>r.station.coordinates).filter(Boolean);if(!xy.length){message('No mapped results in the applied scope.');return false;}map.fitBounds(xy.map(p=>[p[1],p[0]]),{paddingTopLeft:[window.innerWidth<900?12:card?.style.display==='none'?30:320,55],paddingBottomRight:[65,80],maxZoom:13,animate:false});return true;}
function message(text){if(card)card.querySelector('[data-message]').textContent=text;}
function apply(){clearTimeout(timer);timer=null;try{engine.apply();cancelPopup();pageSize=20;message('');redraw(true);}catch(e){message(e.message);}syncPending();}
function edit(p,discrete=false){engine.edit(p);syncPending();clearTimeout(timer);timer=null;if(engine.state.auto){if(discrete)apply();else timer=setTimeout(apply,200);}}
function syncPending(){if(card){card.querySelector('[data-pending]').textContent=engine.pending()?'Pending edits; Apply or Enter':'';card.querySelector('[data-apply]').disabled=!engine.pending();}}
function depthScope(c=engine.state.applied){return ['range','nominal'].includes(c.selection)?(c.selection==='nominal'?'Approximate depth · ':'Depth range · ')+D.describe(c.lower,c.upper)+(c.includeUnknown?' + unknown depth':''):c.selection==='exact'?'Exact mode · '+(c.depth==='site'?"Station’s default sensor":c.depth==='unknown'?'Unspecified depth':c.depth+' mm')+(c.rule==='display'?' · map sensor filter':c.rule==='any'?' · at least one selected depth':' · all selected depths'):c.depth==='site'?"Station's default sensor":c.depth==='unknown'?'Unspecified depth':c.depth+' mm exact';}
function renderStatus(){if(!card)return;for(const [key,p] of Object.entries(sources)){const b=card.querySelector('[data-source="'+key+'"]');if(b){b.setAttribute('aria-pressed',String(engine.state.enabled[key]));b.textContent=label(key)+' '+(p.index?.counts.stations??'—');}}card.querySelector('[data-status]').textContent=Object.entries(sources).map(([k,p])=>label(k)+': '+(engine.state.enabled[k]?p.status:'off')).join(' · ');card.querySelector('[data-legend]').innerHTML=UI.key(engine.state,depthScope());const enabled=Object.values(sources).filter(p=>engine.state.enabled[p.source]&&p.index);card.querySelector('[data-vintage]').textContent=enabled.map(p=>label(p.source)+': '+p.index.note).join(' ');card.querySelectorAll('[data-mode-button]').forEach(b=>{b.setAttribute('aria-pressed',String(b.dataset.modeButton===engine.state.mode));b.disabled=!E.modeCapability(b.dataset.modeButton).available;});card.querySelector('[data-context-note]').textContent=E.modeCapability('context').reason;card.querySelector('[data-change-windows]').hidden=engine.state.mode!=='change';const blmKnown=all().some(s=>s.blm?.on_blm_ca!==null&&s.blm?.on_blm_ca!==undefined);card.querySelector('[data-filter="onBLM"]').disabled=!blmKnown;card.querySelector('[data-filter="distance"]').disabled=!all().some(s=>Number.isFinite(s.blm?.dist_to_blm_mi));card.querySelector('[data-blm-note]').textContent=blmKnown?'Distance to BLM-California lands, not road distance. Unknown geography fails an active filter.':'BLM geography unavailable in the loaded products; filters disabled. This is not a known off-BLM result.';}
function renderResults() {
  if (!card) return;
  const counts=UI.coverage(results,E,Date.now(),engine.state.mode,engine.state.window);
  card.querySelector('[data-count]').textContent=markers.size+' mapped / '+counts.stations+' matching stations · '+counts.eligible+' with eligible map observation';
  card.querySelector('[data-coverage]').textContent='Of '+counts.stations+' stations: '+counts.catalogOnly+' catalog only; '+counts.stale+' older than 3 days; '+counts.missingDepth+' missing selected sensor; '+counts.unresolvedUnits+' unresolved units; '+counts.unsupported+' unavailable in this map mode. Categories can overlap; each station counted once per category, not once per sensor. Not a land-area statistic.';
  card.querySelector('[data-results]').innerHTML = results.slice(0,pageSize).map(result => {
    const station = result.station;
    return '<button type="button" class="sm-result" data-key="' + esc(station.key) + '">' +
      '<b>' + esc(station.name) + '</b><span>' +
      esc(label(station.source) + ' · ' + station.provider + ' · ' + station.id) +
      '</span><span>Map ' + esc(depthLabel(result.display) + ' · ' + valueText(result.display)) +
      '</span>' + admissionHtml(result) + '</button>';
  }).join('') || '<p>No results in enabled networks and applied criteria. Reset filters or enable a source.</p>';
  card.querySelector('[data-more]').hidden = results.length <= pageSize;
}
let snapNodes=[];
function rangeFeedback(){if(!card)return;const c=engine.state.draft;card.querySelector('[data-depth-summary]').textContent=depthScope(c);for(const b of card.querySelectorAll('[data-nominal]')){const n=D.bands[+b.dataset.nominal];b.setAttribute('aria-pressed',String(c.selection==='nominal'&&c.lower===n.lo&&c.upper===n.hi));}card.querySelector('[data-default-depth]').setAttribute('aria-pressed',String(c.selection==='default'&&c.depth==='site'));if(!snapNodes.length)return;const low=Math.max(0,snapNodes.findIndex(n=>n.lo===c.lower)),upper=c.upper===null?snapNodes.length-1:Math.max(low,snapNodes.findIndex(n=>n.hi===c.upper));for(const edge of ['from','to']){const index=edge==='from'?low:upper;card.querySelector('[data-range="'+edge+'"]').value=index;card.querySelector('[data-range-select="'+edge+'"]').value=index;card.querySelector('[data-range="'+edge+'"]').setAttribute('aria-valuetext',D.label(snapNodes[index],engine.state.unit)+'; '+D.describe(c.lower,c.upper));}card.querySelector('[data-range-bounds]').textContent=D.describe(c.lower,c.upper);}
function depthOptions(){if(!card)return;const depths=[...new Set(all().flatMap(s=>s.sensors.map(v=>v.depth_mm)).filter(v=>v!==null))].sort((a,b)=>a-b);snapNodes=D.nodes(depths);card.querySelectorAll('[data-nominal]').forEach(b=>b.textContent=D.label(D.bands[+b.dataset.nominal],engine.state.unit));card.querySelector('[data-range-ticks]').innerHTML=snapNodes.map((n,i)=>({n,i})).filter(({n,i})=>n.nominal||n.lo===400||n.lo===600||i===snapNodes.length-2).map(({n,i})=>'<span style="left:'+100*i/(snapNodes.length-1)+'%" title="'+esc(D.label(n,engine.state.unit))+'">'+(n.nominal?'≈':'')+(engine.state.unit==='mm'?n.lo:n.lo/10)+'</span>').join('')+'<span style="left:100%" title="Open-ended deep bound">∞</span>';const select=card.querySelector('[data-filter="depth"]'),current=engine.state.draft.depth;select.innerHTML='<option value="site">Station’s default sensor</option><option value="unknown">Unspecified depth</option>'+depths.map(d=>'<option value="'+d+'">'+d+' mm exact</option>').join('');select.value=current;card.querySelector('[data-depth-chips]').innerHTML=[...depths,null].map(d=>'<label><input type="checkbox" data-exact="'+(d===null?'unknown':d)+'" '+(engine.state.draft.depths.includes(d)?'checked':'')+'>'+esc(d===null?'Unknown depth':d+' mm')+'</label>').join('');for(const edge of ['from','to']){const el=card.querySelector('[data-range="'+edge+'"]');el.max=snapNodes.length-(edge==='from'?2:1);card.querySelector('[data-range-select="'+edge+'"]').innerHTML=snapNodes.filter(n=>edge==='to'||!n.open).map((n,i)=>'<option value="'+i+'">'+esc(D.label(n,engine.state.unit))+'</option>').join('');}rangeFeedback();}
function syncControls(){if(!card)return;for(const [k,v] of Object.entries(engine.state.draft)){const el=card.querySelector('[data-filter="'+k+'"]');if(el){if(el.type==='checkbox')el.checked=v;else el.value=v??'';}}card.querySelector('[data-auto]').checked=engine.state.auto;card.querySelector('[data-autozoom]').checked=engine.state.autoZoom;depthOptions();syncPending();}
// Native propagation only on this owned surface. No Leaflet fakeStop/private skip flag.
function containEvents(el){for(const type of ['pointerdown','mousedown','touchstart','dblclick','click','wheel'])listen(el,type,e=>e.stopPropagation(),true);}
function rangeEdit(edge,index,commit){let from=+card.querySelector('[data-range="from"]').value,to=+card.querySelector('[data-range="to"]').value;if(edge==='from')from=Math.min(index,to,snapNodes.length-2);else to=Math.max(index,from);const next=D.bounds(snapNodes,from,to);clearTimeout(timer);timer=null;engine.edit({...next,selection:'range',depth:'site',rule:'display',depths:[]});rangeFeedback();syncPending();if(commit&&engine.state.auto)apply();}
function ensureCard(){if(card){card.style.display='flex';return;}if(!document.getElementById('sm1-style')){const style=document.createElement('style');style.id='sm1-style';style.textContent=UI.style;document.head.appendChild(style);}
 control=L.control({position:'bottomleft'});control.onAdd=()=>{card=L.DomUtil.create('div','sm-card pt-map-legend-card');card.innerHTML='<div class="pt-map-card-handle"><b>Soil moisture</b>'+window.BRIM.legendCloseout.actionsHtml('sm-dock','sm-close','Soil moisture')+'</div><div class="sm-sources"><b>Show sources</b><div>'+Object.keys(sources).map(k=>'<button type="button" data-source="'+k+'"></button>').join('')+'</div></div><div class="sm-key" data-legend role="region" aria-label="Soil moisture map key"></div><div class="sm-scroll"><section class="sm-section" aria-label="Map display"><h4>Map display</h4><div>'+UI.buttons([['value','Moisture'],['change','Wetting / Drying'],['context','Context']],engine.state.mode,'data-mode-button')+'</div><small data-context-note></small><details data-advanced-mode><summary>Advanced / data availability</summary>'+UI.buttons([['availability','Coverage']],engine.state.mode,'data-mode-button')+'</details><div data-change-windows>'+UI.buttons([[7,'7 days'],[14,'14 days'],[30,'30 days']],engine.state.window,'data-window-button')+'</div></section><section class="sm-section" aria-label="Depth"><h4>Depth</h4><div><button data-default-depth aria-pressed="true">Station’s default sensor</button>'+D.bands.map((n,i)=>'<button data-nominal="'+i+'" aria-pressed="false">'+esc(D.label(n))+'</button>').join('')+'</div><small data-depth-summary></small><label>Labels <select data-unit><option value="native">cm / in</option><option value="mm">mm</option></select></label><details data-range-panel><summary>Range…</summary><small>Depth steps—not to scale. Equal handles select the full nominal band.</small><div class="sm-range-track"><input type="range" min="0" step="1" data-range="from" aria-label="Lower depth"><input type="range" min="0" step="1" data-range="to" aria-label="Upper depth"></div><div class="sm-range-ticks" data-range-ticks><span>≈5</span><span>≈10</span><span>≈20</span><span>40</span><span>≈50</span><span>60</span><span>≈100</span><span>Deep</span></div><label>From <select data-range-select="from"></select></label><label>To <select data-range-select="to"></select></label><small data-range-bounds aria-live="polite"></small><label><input type="checkbox" data-filter="includeUnknown"> Include unknown depth</label></details></section><section class="sm-section" aria-label="Station filters"><h4>Station filters</h4><input data-filter="query" aria-label="Search soil moisture stations" placeholder="Station name, ID or alias"><div class="sm-vwc-bounds"><label>Min VWC % <input type="number" step="any" min="0" max="100" data-filter="min"></label><label>Max VWC % <input type="number" step="any" min="0" max="100" data-filter="max"></label></div><small>Blank = no bound. Resolved % VWC only.</small><label>Age ≤ <input type="number" min="0" data-filter="age"> days</label><div><label><input type="checkbox" data-auto checked> Apply filters automatically</label><button data-apply>Apply</button><button data-reset>Reset filters</button></div><small data-pending aria-live="polite"></small><details><summary>More criteria</summary><details><summary>Exact depths and matching</summary><label>Map sensor <select data-filter="depth"></select></label><select data-filter="rule" aria-label="Exact depth rule"><option value="display">Filter using the map sensor</option><option value="any">At least one selected depth must match</option><option value="every">All selected depths must match</option></select><div data-depth-chips></div><small>Editing these choices enters Exact mode. One sensor must meet both moisture and age. Missing a required depth excludes the station. The map/admitting sensor difference is labeled. 203.2 mm remains distinct from 200 mm.</small></details><label><input type="checkbox" data-filter="onBLM"> On BLM-CA</label><label>Within mi <input type="number" min="0" data-filter="distance"></label><small data-blm-note></small></details><div><label><input type="checkbox" data-autozoom> Auto-zoom</label><button data-fit>Zoom to results</button></div><small data-message role="status"></small></section><section class="sm-section" aria-label="Results"><h4>Results</h4><small data-count></small><div data-results></div><button data-more>More results</button></section><details><summary>Coverage, methods and source status</summary><p>Seasonal comparison unavailable: comparable statistics, era and support are not established across networks.</p><p data-coverage></p><p data-status></p><p data-vintage></p><p>No query-time observation requests. Range picks an eligible explicit choice, else in-range declared primary, else primary rank and stable ID; never the wettest value. Source context is available in SCAN popups; common reference is not established.</p></details></div>';containEvents(card);return card;};control.addTo(map);
 const depthEdit=patch=>{edit({depth:'site',rule:'display',depths:[],includeUnknown:false,...patch},true);syncControls();};
 const click=e=>{const b=e.target.closest('button');if(!b)return;if(b.dataset.source){const p=sources[b.dataset.source];(engine.state.enabled[p.source]?window.ptOpsDeactivateLayerByName:window.ptOpsActivateLayerByName)(p.name);}else if(b.hasAttribute('data-apply'))apply();else if(b.hasAttribute('data-reset')){clearTimeout(timer);timer=null;cancelPopup();engine.reset();syncControls();redraw(true);}else if(b.hasAttribute('data-default-depth'))depthEdit({selection:'default',lower:0,upper:null});else if(b.hasAttribute('data-nominal')){const n=D.bands[+b.dataset.nominal];depthEdit({selection:'nominal',lower:n.lo,upper:n.hi});}else if(b.hasAttribute('data-mode-button')){if(engine.setMode(b.dataset.modeButton))redraw();}else if(b.hasAttribute('data-window-button')){engine.state.window=+b.dataset.windowButton;card.querySelectorAll('[data-window-button]').forEach(x=>x.setAttribute('aria-pressed',String(+x.dataset.windowButton===engine.state.window)));for(const p of Object.values(sources))if(p.renderer)p.renderer.dendra.state.window=engine.state.window;cancelPopup();redraw();}else if(b.hasAttribute('data-fit'))fit();else if(b.dataset.key)select(b.dataset.key);else if(b.hasAttribute('data-more')){pageSize+=20;renderResults();}};
 listen(card,'click',click,true);listen(card,'input',e=>{const t=e.target,k=t.dataset.filter;if(t.dataset.range){rangeEdit(t.dataset.range,+t.value,false);return;}if(k&&t.tagName==='INPUT'&&t.type!=='checkbox')edit({[k]:t.validity.badInput?NaN:t.value});},true);
 listen(card,'change',e=>{const t=e.target,k=t.dataset.filter;if(t.dataset.range||t.dataset.rangeSelect){rangeEdit(t.dataset.range||t.dataset.rangeSelect,+t.value,true);return;}if(t.hasAttribute('data-exact')){const v=t.dataset.exact==='unknown'?null:+t.dataset.exact,a=engine.state.draft.depths.slice();edit({selection:'exact',depth:'site',includeUnknown:false,depths:t.checked?[...new Set([...a,v])]:a.filter(x=>x!==v)},true);rangeFeedback();}else if(k&&(t.tagName==='SELECT'||t.type==='checkbox')){edit({[k]:t.type==='checkbox'?t.checked:t.value,...(['depth','rule'].includes(k)?{selection:'exact',includeUnknown:false}:{})},true);rangeFeedback();}else if(t.hasAttribute('data-auto')){clearTimeout(timer);timer=null;engine.state.auto=t.checked;if(t.checked&&engine.pending())apply();}else if(t.hasAttribute('data-autozoom'))engine.state.autoZoom=t.checked;else if(t.hasAttribute('data-unit')){engine.state.unit=t.value;depthOptions();redraw();}},true);
 listen(card,'keydown',e=>{if(e.key==='Enter'&&e.target.dataset.filter){e.preventDefault();apply();}if(e.key==='ArrowDown'&&e.target.classList.contains('sm-result')){e.preventDefault();e.target.nextElementSibling?.focus();}if(e.key==='ArrowUp'&&e.target.classList.contains('sm-result')){e.preventDefault();e.target.previousElementSibling?.focus();}},true);
 window.BRIM.legendCloseout.wire(card,'.sm-close',()=>{reopen.getContainer().style.display='';});detachable=window.BRIM.legendCloseout.makeDetachable({card,map,label:'Soil moisture',dockSelector:'.sm-dock',handleSelector:'.pt-map-card-handle'});
 reopen=L.control({position:'bottomleft'});reopen.onAdd=()=>{const el=L.DomUtil.create('button','');el.textContent='Soil moisture';el.title='Reopen soil controls and key';containEvents(el);el.onclick=()=>{card.style.display='flex';el.style.display='none';window.BRIM.legendCloseout.scheduleLayout(card);};el.style.display='none';return el;};reopen.addTo(map);syncControls();renderStatus();clockTimer=setInterval(()=>redraw(),60000);
}
async function select(key) {
  const result = results.find(r => r.station.key === key);
  if (!result || !engine.state.enabled[result.station.source]) return;
  const station = result.station, source = sources[station.source];
  cancelPopup();
  if (!station.coordinates) {
    message(station.name + ': public coordinates unavailable. No map location or history request.');
    return;
  }
  fit([result]);
  if (station.source === 'dendra' && result.display?.history) {
    const dendra = source.renderer?.dendra;
    if (dendra) {
      dendra.state.window = engine.state.window;
      dendra.state.depth = 'site';
      dendra.state.choices[station.id] = result.display.id;
      dendra.openStation(station.id);
    }
    return;
  }
  const epoch = popupEpoch, box = L.DomUtil.create('div','sm-popup');
  popupAbort = new AbortController();
  box.innerHTML=UI.heading(station,result.display,depthLabel(result.display),E.age(result.display,Date.now()),null,sourceBadge(station.source))+'<div class="sm-operational">'+esc(operationalNote(station.source))+'</div>';
  popup = L.popup(popupLayout()).setLatLng([station.coordinates[1],station.coordinates[0]])
    .setContent(box).openOn(map);
  if(source.staticReader){staticSnotelPopup(box,station,result.display,source);return;}
  if (!station.bundle) {
    box.innerHTML += '<p>Metadata-only catalog station. No history imported; no source request is made.</p>';
    return;
  }
  const metric = {station:station.key, requests:0, bytes:0, parse_ms:0};
  try {
    const descriptor = T.descriptor(station.bundle);
    const cacheKey = source.index.generation + ':' + station.key + ':' + descriptor.sha256;
    let body = bundles.get(cacheKey);
    metric.cache_hit = !!body;
    if (!body) body = await bounded(new URL(descriptor.path,source.url).href,
      T.limits.bundle,popupAbort.signal,descriptor.sha256,descriptor.bytes,metric);
    if (epoch !== popupEpoch || !engine.state.enabled[station.source]) return;
    const startDecode = performance.now();
    const decoded = T.decode(body,station.source,station.id);
    metric.decode_ms = performance.now() - startDecode;
    if (!metric.cache_hit) bundles.set(cacheKey,body,descriptor.bytes);
    if (station.source === 'scan') scanPopup(box,station,result.display,decoded);
    else snotelPopup(box,station,result.display,decoded);
    metric.status = 'ready';
    settlePopup();
  } catch (error) {
    metric.status = 'unavailable'; metric.error = error.message;
    if (epoch === popupEpoch && engine.state.enabled[station.source]) {
      box.innerHTML += '<p>Selected history unavailable: ' + esc(error.message) + '</p>';
      settlePopup();
    }
  } finally {
    bundleMetrics.push(metric);
    if (bundleMetrics.length > 128) bundleMetrics.shift();
  }
}
function sourceBadge(source){const delivery=typeof ptOpsDeliveryForName==='function'?ptOpsDeliveryForName(sources[source].name):null;return delivery&&typeof ptOpsDeliveryBadgeHtml==='function'?ptOpsDeliveryBadgeHtml(delivery.delivery_class==='brim_managed'?'managed':'',false):'';}
function operationalNote(source){const note=sources[source].index.note||'';return sources[source].staticReader?'Local static candidate · not live':source==='snotel'?'Pilot · not live':/frozen|snapshot|not.live|saved|replay/i.test(note)?'Saved preview · not live':'Prepared daily data';}
function staticSnotelPopup(box,st,selected,source){
 const owner=popup,states={};
 const choices=st.sensors.map(s=>[s.id,depthLabel(s),s.id]);
 box.innerHTML='<h3>'+esc(st.name)+'</h3><p>NRCS AWDB · SNOTEL · Local static candidate · not live</p>'+UI.sensorPicker(choices,selected?.id)+'<div data-static-chart>Choose an exact sensor to load its source-boundary history.</div>';
 const chart=box.querySelector('[data-static-chart]');
 async function choose(id){
  const sensor=st.sensors.find(s=>s.id===id);if(!sensor||popup!==owner||!engine.state.enabled.snotel)return;
  popupAbort?.abort();popupAbort=new AbortController();const signal=popupAbort.signal,token=++popupEpoch;
  chart.textContent='Loading selected history…';
  box.querySelectorAll('[data-sensor-choice]').forEach(b=>{if(b.tagName==='SELECT')b.value=id;else b.setAttribute('aria-pressed',String(b.dataset.sensorChoice===id));});
  try{
   const decoded=await source.staticReader.history(st.id,sensor.raw.sensor_identity,signal);
   if(token!==popupEpoch||popup!==owner||!engine.state.enabled.snotel)return;
   Charts.render(chart,Charts.snotelStatic(decoded),states[id]||(states[id]={}));settlePopup();
  }catch(error){if(token===popupEpoch&&popup===owner&&!signal.aborted){chart.textContent='Selected history unavailable: '+error.message;settlePopup();}}
 }
 box.querySelectorAll('[data-sensor-choice]').forEach(b=>{if(b.tagName==='SELECT')b.onchange=e=>choose(e.target.value);else b.onclick=()=>choose(b.dataset.sensorChoice);});
 if(selected)choose(selected.id);
 settlePopup();
}
function sourcePopup(box,st,selected,modelFor,detailsFor,csvFor){
 const mapDepth=depthLabel(selected),owner=popup,states={};let chartState={};
 // Native details can cross Leaflet's maxHeight boundary after initial layout.
 // Capture their non-bubbling toggle and reuse this popup's normal settlement.
 box.addEventListener('toggle',e=>{if(e.target.tagName==='DETAILS'&&popup===owner)settlePopup();},true);
 function draw(sensor){const model=modelFor(sensor);chartState=states[sensor.id]||(states[sensor.id]={view:chartState.view||(model.reference.length||model.priorTraces?'years':'history'),scale:chartState.scale||'record'});box.innerHTML='<div class="sm-station-context">'+UI.heading(st,sensor,depthLabel(sensor),E.age(sensor,Date.now()),mapDepth,sourceBadge(st.source))+'</div>'+'<div class="sm-operational">'+esc(operationalNote(st.source))+'</div>'+UI.sensorPicker(st.sensors.map(s=>[s.id,depthLabel(s)+(st.sensors.filter(x=>x.depth_mm===s.depth_mm).length>1?' · sensor '+(st.sensors.filter(x=>x.depth_mm===s.depth_mm).indexOf(s)+1):'')]),sensor.id)+'<div data-source-chart></div><p><a target="_blank" rel="noopener noreferrer" href="'+esc(st.source_url)+'">Official source</a> · <a data-csv download="'+esc(st.source+'-'+st.id+'-'+sensor.id+'.csv')+'">'+(st.source==='scan'?'Selected sensor current-WY CSV':'Selected daily CSV')+'</a></p><details><summary>Technical source details</summary><p>'+esc(sources[st.source].index.note||'')+'</p>'+detailsFor(sensor)+'</details>';Charts.render(box.querySelector('[data-source-chart]'),model,chartState);box.querySelector('[data-csv]').href='data:text/csv;charset=utf-8,'+encodeURIComponent(csvFor(sensor));box.querySelectorAll('[data-sensor-choice]').forEach(b=>{const choose=id=>draw(st.sensors.find(s=>s.id===id));if(b.tagName==='SELECT')b.onchange=e=>choose(e.target.value);else b.onclick=()=>choose(b.dataset.sensorChoice);});settlePopup();}
 draw(selected||st.sensors[0]);
}
function scanPopup(box,st,selected,b){const site=String(b.feature.properties.site_code);const rows=(name,s)=>b.tables[name].filter(r=>String(r.site_code)===site&&+r.depth_in===Math.abs(s.depth_native));sourcePopup(box,st,selected,s=>ptScanPresentationData(site,Math.abs(s.depth_native),b.tables),s=>'<p>Stored age labels are snapshot provenance at publication; the heading uses current reader age.</p><pre>'+esc(JSON.stringify({sensor:s,source:b.feature.properties},null,2))+'</pre>',s=>Charts.csv(rows('scan_soil_moisture_current_wy_trace.csv',s)));}
function snotelPopup(box,st,selected,b){const block=s=>b.data.data.find(x=>`SMS:${x.stationElement.heightDepth}:${x.stationElement.ordinal}`===s.id);sourcePopup(box,st,selected,s=>Charts.snotel(block(s)),s=>'<pre>'+esc(JSON.stringify(block(s).stationElement,null,2))+'</pre><p>'+esc([...new Set(block(s).values.map(v=>'QC '+v.qcFlag+' / QA '+v.qaFlag))].join('; '))+'</p>',s=>Charts.csv(block(s).values));}
function register(source,name,url,staticReader=null){if(sources[source]&&(staticReader||sources[source].staticReader))throw Error('SNOTEL source already registered');const layer=L.layerGroup(),p={source,name,url:new URL(url,location.href).href,layer,index:null,status:'not loaded',epoch:0,abort:null,renderer:null,staticReader};sources[source]=p;
layer.onAdd=function(m){L.LayerGroup.prototype.onAdd.call(this,m);engine.enable(source,true);ensureCard();p.epoch++;const token=p.epoch;p.abort=new AbortController();p.status='loading';redraw();const signal=p.abort.signal;(p.index?Promise.resolve(p.index):p.staticReader?p.staticReader.load(signal).then(staticCatalog):bounded(p.url,262144,signal).then(i=>expandIndex(i,source,d=>bounded(new URL(d.path,p.url).href,262144,signal,d.sha256,d.bytes)))).then(async i=>{if(token!==p.epoch||!engine.state.enabled[source])return;if(source==='dendra'){if(!p.reader)p.reader=new window.BrimDendraReader.Reader(DENDRA_DAILY_INDEX_URL);if(!p.reader.index)await p.reader.loadIndex(signal);if(token!==p.epoch||!engine.state.enabled[source])return;if(p.reader.index.generation!==i.generation)throw Error('Dendra adapter/reader generation mismatch');p.renderer=makeDendraLayer({name,indexUrl:DENDRA_DAILY_INDEX_URL,reader:p.reader,popupOnly:true});p.renderer.addTo(layer);}p.index=i;p.status=i.counts.stations+' stations / '+i.counts.imported_histories+' moisture histories';setOpsLayerLoading(name,false);recordStatus(name,p.status,'pt-ops-muted');depthOptions();redraw();}).catch(e=>{if(token!==p.epoch||!engine.state.enabled[source])return;p.index=null;p.status='unavailable: '+e.message;setOpsLayerLoading(name,false);recordStatus(name,p.status,'pt-ops-warn');redraw();});};
function deactivate(){if(!engine.state.enabled[source])return;engine.enable(source,false);p.epoch++;p.abort?.abort();cancelPopup();if(p.renderer){p.renderer.forceRemove(map);layer.removeLayer(p.renderer);p.renderer=null;}for(const [k,marker] of markers)if(k.startsWith(source+':')){releaseMarker(marker);markers.delete(k);}layer.clearLayers();p.status='off';redraw();if(!Object.values(engine.state.enabled).some(Boolean))destroyCard();}
layer.onRemove=function(m){deactivate();L.LayerGroup.prototype.onRemove.call(this,m);};layer.forceRemove=function(m){if(m.hasLayer(layer))m.removeLayer(layer);else deactivate();};return layer;}
function destroyCard(){clearTimeout(timer);timer=null;clearInterval(clockTimer);clockTimer=null;listeners.forEach(({target,event,fn,dom})=>dom?target.removeEventListener(event,fn):target.off(event,fn));listeners=[];detachable?.destroy(true,true);detachable=null;if(control)map.removeControl(control);if(reopen)map.removeControl(reopen);control=null;card=null;reopen=null;snapNodes=[];}
function reset(){clearTimeout(timer);timer=null;clearInterval(clockTimer);clockTimer=null;cancelPopup();for(const p of Object.values(sources)){p.epoch++;p.abort?.abort();p.renderer?.forceRemove(map);p.renderer=null;for(const [key,marker] of markers)if(key.startsWith(p.source+':'))releaseMarker(marker);p.layer.clearLayers();engine.enable(p.source,false);}markers.clear();destroyCard();engine.state.auto=true;engine.state.autoZoom=false;engine.state.mode='value';engine.state.window=7;engine.state.unit='native';engine.reset();results=[];pageSize=20;}
UI.bindPopupPriority(map,document);
map.on('popupopen popupclose movestart',clearHover);
map.on('popupclose',function(e){if(e.popup===popup){clearTimeout(popupLayoutTimer);popupLayoutTimer=null;popupEpoch++;popupAbort?.abort();popup=null;}});map.on('unload',reset);
map.on('resize',function(){if(popup){Object.assign(popup.options,popupLayout());settlePopup();}});
const api={register,registerSnotelStatic,reset,engine,select,apply,edit,redraw,fit,validateIndex,expandIndex,admissionText,stats:()=>({sources:Object.fromEntries(Object.entries(sources).map(([k,p])=>[k,{enabled:engine.state.enabled[k],status:p.status,counts:p.index?.counts}])),results:results.map(r=>r.station.key),markers:markers.size,card:!!card,pending:engine.pending(),transaction:engine.state.transaction,transactionMs,historyCache:bundles.size,historyCacheBytes:bundles.bytes,bundleMetrics:bundleMetrics.slice(),bundleLimits:T.limits,listeners:listeners.length}),get sources(){return sources;}};window.BRIM.soilMoisture=api;return api;
}
