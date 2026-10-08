/* Shared soil presentation only; one controller owns map/filter state. */
(function(root,factory){const api=factory();if(typeof module==='object')module.exports=api;else root.BRIM.soilMoistureUI=api;})(typeof window==='object'?window:this,function(){
  'use strict';
  const esc=s=>String(s??'').replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
  const buttons=(items,selected,attr)=>items.map(([id,label])=>'<button type="button" '+attr+'="'+esc(id)+'" aria-pressed="'+(String(id)===String(selected))+'">'+esc(label)+'</button>').join('');
  function sensorPicker(items,selected) {
    return items.length<=5?'<div class="sm-sensors">'+items.map(([id,label,title])=>'<button type="button" data-sensor-choice="'+esc(id)+'" aria-pressed="'+(id===selected)+'" title="'+esc(title||label)+'" aria-label="'+esc(title||label)+'">'+esc(label)+'</button>').join('')+'</div>':'<label>Sensor <select data-sensor-choice>'+items.map(([id,label])=>'<option value="'+esc(id)+'"'+(id===selected?' selected':'')+'>'+esc(label)+'</option>').join('')+'</select></label>';
  }
  function heading(st,s,depth,clockAge,mapDepth,badge='') {
    const o=s?.observation,good=o?.eligible&&Number.isFinite(o.value);
    return '<h3>'+esc(st.name)+'</h3><div class="sm-subline">'+esc(st.source==='dendra'?'dendra':st.source==='snotel'?'USDA NRCS · SNOTEL pilot':'USDA NRCS · SCAN')+' '+badge+'</div><div class="sm-headline"><strong>'+(good?esc(Number(o.value.toFixed(2)))+'%':'Unavailable')+'</strong> · '+esc(depth)+'<br><span>'+esc(good?o.date+' · '+clockAge+' d old · '+(st.source==='dendra'?'Daily mean':'Daily soil moisture'):o?.missing_reason||'No eligible selected sensor')+'</span></div>'+(mapDepth&&mapDepth!==depth?'<div class="sm-subline">Map: '+esc(mapDepth)+' · Viewing: '+esc(depth)+'</div>':'');
  }
  // Standard Leaflet divIcon content: provider shape and metric color are independent.
  const providers={dendra:['circle','dendra'],scan:['square','SCAN'],snotel:['triangle','SNOTEL']};
  function providerSymbol(source,color='#526c75',unavailable=false) {
    const shape=providers[source][0],geometry={circle:'<circle cx="10" cy="10" r="6.5"',square:'<rect x="4" y="4" width="12" height="12"',triangle:'<path d="M10 2 L18 17 H2 Z"'}[shape];
    return '<svg viewBox="0 0 20 20" width="20" height="20" aria-hidden="true" data-provider-shape="'+shape+'">'+geometry+' fill="'+(unavailable?'white':color)+'" fill-opacity="'+(unavailable?'.5':'.9')+'" stroke="#294951" stroke-width="1.3"'+(unavailable?' stroke-dasharray="2 2"':'')+'/></svg>';
  }
  // Display only; native rows and exported values are never converted or rounded.
  const fahrenheit=v=>Number.isFinite(v)?v*9/5+32:null;
  function key(state,scope) {
    const mode=state.mode;
    const items=mode==='value'?[['#c99852','<10%'],['#92b8a1','10–<25%'],['#438e95','25–<50%'],['#315c94','≥50%']]:mode==='change'?[['#267eaa','↑ >+0.50 pp'],['#a86930','↓ <−0.50 pp'],['#72838a','↔ −0.50 to +0.50 pp']]:mode==='availability'?[['#328575','Eligible observation']]:[];
    const title={value:'Moisture · % VWC',change:'Wetting / drying · percentage points',availability:'Data coverage',context:'Context · unavailable',reference:'Context · unavailable'}[mode];
    return '<b>'+esc(title)+'</b><div>'+esc(scope)+'</div><div class="sm-key-items">'+items.map(([color,text])=>'<span><i style="background:'+color+'"></i>'+esc(text)+'</span>').join('')+'<span><i class="sm-unavailable"></i>'+(['context','reference'].includes(mode)?'No context':'Unavailable / unsupported')+'</span></div><small>'+(mode==='change'?'Wetting / drying = change in mean moisture: '+state.window+' completed daily means vs preceding '+state.window+'. Choose 7, 14 or 30 days under Map display. Stale/unsupported values are unavailable.':['context','reference'].includes(mode)?'Validated source reference capability is not connected. No seasonal classes are assigned.':mode==='value'?'VWC = volumetric water content. Numeric bins, not hazard or normality. Source daily statistics differ.':'Eligibility and age describe observations, not land area.')+'</small>'+ (Object.keys(providers).filter(k=>state.enabled?.[k]).length>1?'<div class="sm-provider-key">'+Object.keys(providers).filter(k=>state.enabled[k]).map(k=>'<span>'+providerSymbol(k)+' '+esc(providers[k][1])+'</span>').join('')+'<span>Shape = provider</span></div>':'');
  }
  function coverage(results,E,now,mode,windowDays) {
    const counts={stations:results.length,eligible:0,stale:0,catalogOnly:0,missingDepth:0,unresolvedUnits:0,unsupported:0};
    for(const r of results){const s=r.display;if(!s)counts.missingDepth++;if(!r.station.sensors.some(x=>x.history?.available))counts.catalogOnly++;if(s?.observation?.eligible)counts.eligible++;if(s&&E.age(s,now)>3)counts.stale++;if(s&&s.unit!=='percent VWC')counts.unresolvedUnits++;if(s&&E.modeValue(s,mode,now,windowDays).value===null)counts.unsupported++;}
    return counts;
  }
  // The shared controller installs this once per map. Track the actual popup,
  // not its fading DOM: popupclose must restore tools before Leaflet removes it.
  // Only presentation state changes; the popup and every tool keep their owner.
  function bindPopupPriority(map,doc) {
    const container=map.getContainer(),flag='sm-popup-priority';
    let active=null;
    function sync(){for(const node of [container,doc.body])node.classList.toggle(flag,!!active);}
    function opened(e){
      const el=e.popup.getElement();
      active=el&&(el.classList.contains('pt-dendra-shared')||el.querySelector('.sm-popup'))?e.popup:null;
      sync();
    }
    function closed(e){if(e.popup===active){active=null;sync();}}
    function dispose(){
      active=null;sync();
      map.off('popupopen',opened);map.off('popupclose',closed);map.off('unload',dispose);
    }
    map.on('popupopen',opened);map.on('popupclose',closed);map.on('unload',dispose);
    return dispose;
  }
  // At the existing phone breakpoint, the open station popup has interaction
  // priority. Map tools are siblings of mapPane; Ops is a direct body child.
  // Visibility preserves layout/expanded state but removes these background
  // tools from pointer and keyboard interaction. Descendants cannot opt back in.
  // No geometry, z-index, pointer-events or generic Leaflet ownership changes.
  const popupPriorityStyle=`@media(max-width:600px){
    .leaflet-container.sm-popup-priority > :not(.leaflet-map-pane),
    .leaflet-container.sm-popup-priority > :not(.leaflet-map-pane) *,
    body.sm-popup-priority > #pt-ops-live-wrap,
    body.sm-popup-priority > #pt-ops-live-wrap *{visibility:hidden!important}
  }`;
  // The corner manager measures the gap between External Layers and Upload.
  // Keep the whole soil card inside that gap; only its middle region scrolls.
  // Shared popup context stays in document flow: Leaflet only adds a local
  // scroll container above maxHeight, so sticky would use a different ancestor
  // in short popups. Dendra owns its separate station-context styling.
  const style=`.sm-card{box-sizing:border-box;font:11px/1.35 Arial;color:#203e46;width:286px;background:rgba(226,238,235,.96);border:1px solid #9db3bb;border-radius:6px;max-height:min(calc(100vh - 150px),calc(var(--pt-map-legend-max-height,100vh) - 8px));display:flex;flex-direction:column;overflow:hidden}.sm-card *{box-sizing:border-box}.sm-card .pt-map-card-handle{display:flex;align-items:center;gap:6px;padding:6px 9px;flex-shrink:0}.sm-card .pt-map-card-handle>b{margin-right:auto}.sm-scroll{flex:1 1 auto;overflow:auto;overscroll-behavior:contain;padding:0 9px 8px;min-height:0}.sm-key{padding:6px 9px;background:#f5faf8;border-top:1px solid #b9ccc6;border-bottom:1px solid #b9ccc6;flex-shrink:0;font-size:10px}.sm-key-items{display:flex;gap:4px 9px;flex-wrap:wrap;margin:4px 0}.sm-key i{display:inline-block;width:11px;height:11px;border:1px solid #405b62;border-radius:50%;vertical-align:middle;margin-right:3px}.sm-key .sm-unavailable{background:white;border:2px dashed #849295}.sm-card button,.sm-card select,.sm-card input,.sm-popup button,.sm-popup select{font:inherit;color:inherit;max-width:100%}.sm-card input[type=number]{width:68px}.sm-card button,.sm-popup button{cursor:pointer;border:1px solid #a7bcc2;border-radius:4px;background:white;padding:3px 5px;margin:2px}.sm-card button[aria-pressed=true],.sm-popup button[aria-pressed=true]{background:#cbe5df;border-color:#367a71;font-weight:bold}.sm-card label{display:inline-block;margin:3px}.sm-card :focus-visible,.sm-popup :focus-visible{outline:3px solid #287da8;outline-offset:1px}.sm-card [data-filter=query]{width:100%;padding:4px;margin:4px 0}.sm-card .sm-result{display:block;width:100%;text-align:left;padding:6px;overflow-wrap:anywhere}.sm-result span,.sm-card small{display:block;font-size:10px;color:#526c75}.sm-card [data-results]{max-height:165px;overflow:auto}.sm-match{color:#805319}.sm-card summary,.sm-popup summary{cursor:pointer}.sm-popup{width:min(600px,calc(100vw - 100px));font:12px/1.4 Arial;color:#203e46;min-width:210px}.sm-popup h3{font-size:17px;margin:0 0 2px}.sm-popup svg{width:100%;height:auto;display:block}.sm-popup table{width:100%;border-collapse:collapse}.sm-popup th,.sm-popup td{padding:4px;text-align:left;border-bottom:1px solid #ddd}.sm-popup .sm-station-context{background:white;padding:3px 0;border-bottom:1px solid #dce5e2}.sm-subline{font-size:10px;color:#526c75;overflow-wrap:anywhere}.sm-headline{margin:6px 0}.sm-headline strong{font-size:21px}.sm-headline span{font-size:11px}.sm-popup details{margin:8px 0}.sm-popup pre{white-space:pre-wrap;overflow-wrap:anywhere;font-size:10px}.sm-popup [data-chart-readout]{min-height:34px;font-size:11px}.sm-range-track{position:relative;height:32px;margin:0 8px}.sm-range-track input{position:absolute;left:0;width:100%;margin:0;pointer-events:none;background:transparent;appearance:none;height:4px;top:14px}.sm-range-track input:focus{z-index:2}.sm-range-track input::-webkit-slider-runnable-track{height:4px;background:#a7bcc266}.sm-range-track input::-webkit-slider-thumb{appearance:none;width:16px;height:16px;border-radius:50%;background:#287d82;border:2px solid white;margin-top:-6px}.sm-range-track input::-moz-range-thumb{width:14px;height:14px;border-radius:50%;background:#287d82}.sm-range-track input::-webkit-slider-thumb{pointer-events:auto}.sm-range-track input::-moz-range-thumb{pointer-events:auto}.sm-range-ticks{position:relative;height:15px;font-size:9px;margin:0 8px}.sm-range-ticks span{position:absolute;transform:translateX(-50%)}.sm-popup [hidden],.sm-card [hidden]{display:none!important}@media(max-width:600px){.sm-card{width:min(286px,calc(100vw - 80px));max-height:min(45vh,calc(var(--pt-map-legend-max-height,100vh) - 8px))}.sm-key{font-size:9px;padding:5px}.sm-key small{font-size:9px}.sm-card [data-results]{max-height:95px}.sm-popup h3{font-size:15px}}`;
  const polishStyle=`.sm-sources{padding:0 9px 5px;flex-shrink:0}.sm-section{border:0;border-top:1px solid #b9ccc6;margin:6px 0 0;padding:5px 0}.sm-section h4{font-size:11px;margin:0 0 3px}.sm-section details{margin-top:4px}.sm-card .sm-section:first-child{border-top:0}.sm-provider-key{display:flex;flex-wrap:wrap;gap:2px 8px;align-items:center;margin-top:3px}.sm-provider-key span{display:inline-flex;align-items:center}.sm-provider-key svg{width:14px;height:14px}.sm-provider-marker{background:none;border:0}.sm-provider-marker:focus-visible{outline:3px solid #287da8;border-radius:3px}.sm-provider-marker svg{display:block}.leaflet-tooltip.sm-station-tooltip{box-sizing:border-box;white-space:normal;width:240px;min-width:180px;max-width:240px;font:11px/1.4 Arial;color:#203e46;overflow-wrap:break-word}.sm-hover-name{display:block;font-size:12px;line-height:1.35;margin-bottom:2px}.sm-hover-source{color:#526c75}.sm-hover-measure{margin-top:2px}.sm-vwc-bounds{display:flex;gap:6px}.sm-vwc-bounds label{flex:1;min-width:0;margin:3px 0}.sm-card .sm-vwc-bounds input{width:100%}.sm-card button:disabled{cursor:default;opacity:.65}.sm-station-tooltip .sm-match{display:block;font-size:10px}.sm-popup .sm-operational{font-size:10px;color:#526c75;margin:2px 0}.sm-popup .sm-sensors{margin:3px 0}.sm-popup .sm-headline{margin:4px 0}.sm-popup [data-chart-readout]{min-height:28px}`;
  return {esc,providers,providerSymbol,fahrenheit,buttons,sensorPicker,heading,key,coverage,bindPopupPriority,style:style+polishStyle+popupPriorityStyle};
});
