/* Source-aware display of retained rows only. No aggregation, interpolation or reference computation. */
(function(root,factory){const api=factory();if(typeof module==='object')module.exports=api;else root.BRIM.soilMoistureCharts=api;})(typeof window==='object'?window:this,function(){
  'use strict';
  const day=s=>Date.parse(s+'T00:00:00Z')/86400000;
  const wy=s=>Number(s.slice(0,4))+(s.slice(5,7)>='10'?1:0);
  const dowy=s=>day(s)-day((wy(s)-1)+'-10-01')+1;
  // Date-only NRCS observations stay date-only. A display coordinate is not an observation hour.
  const plotDay=s=>day((s.slice(5,7)>='10'?'1999':'2000')+s.slice(4))-day('1999-10-01')+1;
  const currentDate=(now=Date.now(),offsetHours=-8)=>new Date(now+offsetHours*3600000).toISOString().slice(0,10);
  const currentWY=(now=Date.now(),offsetHours=-8)=>wy(currentDate(now,offsetHours));
  const esc=s=>String(s??'').replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
  const num=v=>v===null||v===undefined||v===''?null:Number.isFinite(Number(v))?Number(v):null;
  const row=(date,v,raw,ok=true)=>({date,v:num(v),wy:wy(date),dowy:dowy(date),x:plotDay(date),raw,ok:ok&&num(v)!==null});
  const good=r=>r.ok&&Number.isFinite(r.v);
  function segments(rows,gap=1){const out=[];let part=[],previous=null;for(const r of rows){if(!good(r)||previous&&day(r.date)-day(previous.date)>gap){if(part.length)out.push(part);part=[];}if(good(r))part.push(r);previous=good(r)?r:null;}if(part.length)out.push(part);return out;}
  function scale(values,absolute=false){if(absolute)return [0,100,20];if(!values.length)return [0,10,2];const min=Math.min(...values),max=Math.max(...values),span=Math.max(max-min,2),raw=span/4,pow=10**Math.floor(Math.log10(raw)),ratio=raw/pow,step=(ratio<=1?1:ratio<=2?2:ratio<=5?5:10)*pow;return [Math.floor((min-span*.04)/step)*step,Math.ceil((max+span*.04)/step)*step,step];}
  function yearSelection(years,shortcut,now=Date.now()){return new Set(shortcut==='current'?years.filter(y=>y===currentWY(now)):shortcut==='recent'?years.slice(-3):years);}
  function dateTicks(first,last){
    const start=day(first),end=day(last);if(!Number.isFinite(start)||!Number.isFinite(end))return [];
    const days=end-start,offsets=days===0?[0]:[0,.25,.5,.75,1].map(f=>Math.round(days*f));
    return [...new Set(offsets)].map(n=>{const date=new Date((start+n)*86400000).toISOString().slice(0,10);return {date,fraction:days?n/days:0,label:days>730?date.slice(0,7):new Date(date+'T12:00:00Z').toLocaleDateString('en-US',{month:'short',day:'numeric',...(days>365?{year:'2-digit'}:{}),timeZone:'UTC'})};});
  }
  function snotel(block){const rows=block.values.map(r=>row(r.date,r.value,r,r.qcFlag==='V'&&Number.isFinite(r.value)&&r.value>=0&&r.value<=100));return {history:rows,seasonal:rows,historyGap:1,historyLabel:'Daily record',statistic:'Daily soil moisture',units:'% VWC',reference:[],monthlyReference:[],referenceReason:'No historical reference supplied. Only the acquired daily interval is available.',sourceNote:'NRCS reports daily soil moisture; its within-day reduction and sample support are not established in this product. Source date only (metadata UTC−08). Original values and QA/QC remain in the CSV and technical details.'};}
  function snotelStatic(decoded){
    if(decoded?.contract!=='SNOTEL_CA_STATIC_CANDIDATE_1')throw Error('Explicit SNOTEL static contract required');
    const rows=decoded.records.map(r=>row(r.provider_date,r.display_value,r,r.display_status==='PLOT_NUMERIC'));
    return {history:rows,seasonal:rows,historyGap:1,historyLabel:'Source-boundary history',
      statistic:'Instantaneous DAILY/END soil moisture',units:'% VWC',reference:[],monthlyReference:[],
      referenceReason:'No historical reference supplied.',context:decoded,
      sourceNote:'NRCS AWDB SMS DAILY/END: instantaneous source boundary, not a daily mean. Provider date D maps to (D+1) at 08:00 UTC; fixed GMT−08 year-round. Dates and water years use the provider date. Values are unconverted REST current/native pct; producer display decisions and all revision evidence are retained.'};
  }
  function readout(r,model){return r.date+' · WY '+r.wy+' · DOWY '+r.dowy+' · '+Number(r.v.toFixed(2))+' '+model.units+' · '+model.statistic+(model.context?.contract==='SNOTEL_CA_STATIC_CANDIDATE_1'?' · Source timestamp '+r.raw.source_timestamp_utc+' · fixed GMT−08':(model.historyGap===45?' · '+(r.raw.actual_n_days||'unknown')+' retained daily observations in month':' · '+'Source-accepted observation')+'; sampling coverage unknown');}
  function csv(rows){if(!rows.length)return '';const keys=Object.keys(rows[0]);return keys.join(',')+'\n'+rows.map(r=>keys.map(k=>'"'+String(r[k]??'').replace(/"/g,'""')+'"').join(',')).join('\n');}
  function render(container,model,state={}) {
    state.view=state.view||'history';state.scale=state.scale||'record';state.presentation=state.presentation||'both';
    const years=[...new Set(model.seasonal.filter(good).map(r=>r.wy))].sort((a,b)=>a-b);
    if(!state.years)state.years=new Set(years);else state.years=new Set([...state.years].filter(y=>years.includes(y)));
    const buttons=(items,value,attr)=>items.map(([v,t])=>'<button type="button" '+attr+'="'+v+'" aria-pressed="'+(v===value)+'">'+esc(t)+'</button>').join('');
    container.innerHTML='<div>'+buttons([['history','History'],['years','Water years']],state.view,'data-chart-view')+'<label>Axis <select data-chart-scale><option value="record">Sensor range</option><option value="absolute">0–100% VWC</option></select></label></div><div data-year-tools>'+buttons([['all','All available'],['recent','Last 3'],['current','Current WY']],null,'data-chart-years')+'<div data-year-list></div></div><div data-band-tools>'+buttons([['traces','Traces'],['band','Band'],['both','Both']],state.presentation,'data-chart-band')+'</div><div data-chart-caption class="sm-subline"></div><div data-chart-svg></div><div data-chart-readout role="status" aria-live="polite"></div><div data-source-support class="sm-subline"></div><details><summary>Data, methods and historical context</summary><p>'+esc(model.sourceNote)+'</p><p>'+esc(model.referenceReason)+'</p><p>'+esc(model.referenceNote||'')+'</p><p>Current reader water year: WY '+currentWY()+'. Selection of displayed years does not change the saved reference cohort. Missing or screened dates break traces. Latest point marks the latest retained accepted observation.</p></details>';
    container.querySelector('[data-chart-scale]').value=state.scale;
    function draw(){
      const seasonal=state.view==='years',base=seasonal?model.seasonal:model.history,shown=seasonal?base.filter(r=>state.years.has(r.wy)):base;
      const refs=seasonal?model.reference:model.monthlyReference,band=refs.length&&state.presentation!=='traces',traces=!refs.length||state.presentation!=='band';
      container.querySelector('[data-year-tools]').hidden=!seasonal||!traces;
      container.querySelector('[data-band-tools]').hidden=!refs.length;
      container.querySelectorAll('[data-chart-view]').forEach(b=>b.setAttribute('aria-pressed',String(b.dataset.chartView===state.view)));
      container.querySelectorAll('[data-chart-band]').forEach(b=>b.setAttribute('aria-pressed',String(b.dataset.chartBand===state.presentation)));
      container.querySelector('[data-year-list]').innerHTML=years.map(y=>'<button type="button" data-chart-year="'+y+'" aria-pressed="'+state.years.has(y)+'">WY '+y+'</button>').join('');
      container.querySelectorAll('[data-chart-year]').forEach(b=>b.onclick=()=>{const y=+b.dataset.chartYear;state.years.has(y)?state.years.delete(y):state.years.add(y);draw();});
      container.querySelector('[data-source-support]').textContent=shown.filter(good).length+' valid / '+shown.length+' retained '+(!seasonal&&model.historyGap===45?'monthly':'daily')+' rows in this view; sampling coverage unknown.'+(band?' Reference: '+refs.length+' supported '+(seasonal?'water days':'months')+'; original cohort unchanged.':'');
      const dates=base.map(r=>r.date).sort(),start=day(dates[0]),end=day(dates.at(-1));
      container.querySelector('[data-chart-caption]').textContent=(seasonal?'October–September · acquired WY traces':model.historyLabel+' · '+(dates[0]||'none')+' → '+(dates.at(-1)||'none'))+' · '+model.units+(seasonal&&years.length===1?' · only WY '+years[0]+' supplied':'')+(seasonal&&!model.priorTraces&&model.reference.length?' · underlying reference-year traces not included in this saved product':'');
      const referenceKeys=seasonal?['p00','p10','p30','p50','p70','p90','p100']:['p30','p50','p70'];
      const vals=base.filter(good).map(r=>r.v).concat(refs.flatMap(r=>referenceKeys.map(k=>r[k]).filter(Number.isFinite)));
      const [lo,hi,step]=scale(vals,state.scale==='absolute'),W=650,H=215,M={l:44,r:16,t:16,b:29},pw=W-M.l-M.r,ph=H-M.t-M.b;
      const x=r=>M.l+(seasonal?(r.x-1)/365:(day(r.date)-start)/Math.max(1,end-start))*pw,y=v=>M.t+(hi-v)/(hi-lo)*ph,xy=r=>x(r).toFixed(2)+','+y(r.v).toFixed(2);
      let svg='<svg viewBox="0 0 '+W+' '+H+'" role="img" aria-label="'+esc(model.historyLabel+'; '+(seasonal?'water years':'chronological')+'; '+state.scale+' scale')+'">';
      for(let v=lo;v<=hi+step*.01;v+=step)svg+='<line x1="44" x2="634" y1="'+y(v)+'" y2="'+y(v)+'" stroke="#dce5e2"/><text x="38" y="'+(y(v)+3)+'" text-anchor="end" font-size="10">'+Number(v.toFixed(2))+'</text>';
      const ticks=seasonal?[[1,'Oct'],[62,'Dec'],[124,'Feb'],[model.sourceWaterDay?183:184,'Apr'],[model.sourceWaterDay?244:245,'Jun'],[model.sourceWaterDay?305:306,'Aug'],[366,'Sep']]:dates.length?dateTicks(dates[0],dates.at(-1)).map(t=>[t.fraction,t.label]):[];
      ticks.forEach(([v,t],i)=>{const px=M.l+(seasonal?(v-1)/365:v)*pw;svg+='<line x1="'+px+'" x2="'+px+'" y1="16" y2="186" stroke="#e2eae6" stroke-width=".6"/><text x="'+px+'" y="206" text-anchor="'+(i===0?'start':i===ticks.length-1?'end':'middle')+'" font-size="10">'+esc(t)+'</text>';});
      if(band){const pairs=seasonal?[['p00','p10','#ead6b8'],['p10','p30','#f4ead8'],['p30','p70','#ececec'],['p70','p90','#ddeef7'],['p90','p100','#bfd7ea']]:[['p30','p70','#d9d9d9']];
        for(const [low,high,color] of pairs){let parts=[],part=[];for(const r of refs){const prev=part.at(-1),gap=prev&&(seasonal?r.x-prev.x>1:day(r.date)-day(prev.date)>45);if(gap||!Number.isFinite(r[low])||!Number.isFinite(r[high])){if(part.length)parts.push(part);part=[];}if(Number.isFinite(r[low])&&Number.isFinite(r[high]))part.push(r);}if(part.length)parts.push(part);for(const rs of parts)svg+='<polygon data-band="'+low+'-'+high+'" points="'+rs.map(r=>x(r)+','+y(r[low])).concat(rs.slice().reverse().map(r=>x(r)+','+y(r[high]))).join(' ')+'" fill="'+color+'"/>';}
        const median=refs.map(r=>({...r,v:r.p50,ok:Number.isFinite(r.p50)}));let prev=null;svg+='<path data-reference-median d="'+median.filter(good).map(r=>{const move=!prev||(seasonal?r.x-prev.x>1:day(r.date)-day(prev.date)>45);prev=r;return(move?'M':'L')+xy(r);}).join(' ')+'" fill="none" stroke="#66757a" stroke-dasharray="3 3"/>';
      }
      if(traces){const groups=seasonal?years.map(yr=>shown.filter(r=>r.wy===yr)):[shown];groups.forEach((rs,i)=>segments(rs,seasonal?1:model.historyGap).forEach(part=>{const color=seasonal&&part[0].wy!==currentWY()?['#719e8c','#bd8758','#837eaf','#588dad'][i%4]:'#234f58';svg+='<path data-wy="'+part[0].wy+'" d="'+part.map((r,i)=>(i?'L':'M')+xy(r)).join(' ')+'" fill="none" stroke="'+color+'" stroke-width="1.7"/>';if(part.length===1)svg+='<circle cx="'+x(part[0])+'" cy="'+y(part[0].v)+'" r="2" fill="'+color+'"/>';}));}
      const valid=traces?shown.filter(good):[],last=base.filter(good).slice().sort((a,b)=>a.date.localeCompare(b.date)).at(-1);
      if(traces&&last&&valid.includes(last))svg+='<circle class="latest-point" data-date="'+last.date+'" data-value="'+last.v+'" cx="'+x(last)+'" cy="'+y(last.v)+'" r="4" fill="'+(day(currentDate())-day(last.date)>3?'#bc852c':'#159571')+'" stroke="white"/>';
      const inspect=valid.length?valid:band?refs.filter(r=>Number.isFinite(r.p50)).map(r=>({...r,v:r.p50,reference:true})):[];
      svg+='<circle data-focus-point visibility="hidden" r="4" fill="white" stroke="#245960"/><rect data-chart-hit tabindex="0" role="slider" aria-label="Observation readout; Left and Right move through retained observations" aria-valuemin="0" aria-valuemax="'+Math.max(0,inspect.length-1)+'" aria-valuenow="0" x="44" y="16" width="590" height="170" fill="transparent"/></svg>';
      const holder=container.querySelector('[data-chart-svg]');holder.innerHTML=svg;
      const root=holder.querySelector('svg'),hit=root.querySelector('[data-chart-hit]'),point=root.querySelector('[data-focus-point]'),out=container.querySelector('[data-chart-readout]');let cursor=Math.max(0,inspect.length-1);
      const show=i=>{const r=inspect[i];if(!r){out.textContent=band?'Saved reference shown; select Traces or Both for observation readout.':'No accepted observations in selected water years.';return;}cursor=i;out.textContent=r.reference?'Saved reference · '+(seasonal?'source water day '+r.x:r.date)+' · '+referenceKeys.map(k=>k+': '+r[k]).join(' · ')+' '+model.units+' · support '+(r.raw.n_years||r.raw.ref_n_years||'declared')+' years; original cohort unchanged':readout(r,{...model,statistic:!seasonal&&model.historyStatistic||model.statistic,historyGap:seasonal?1:model.historyGap});hit.setAttribute('aria-valuenow',i);hit.setAttribute('aria-valuetext',out.textContent);point.setAttribute('cx',x(r));point.setAttribute('cy',y(r.v));point.setAttribute('visibility','visible');};
      hit.addEventListener('focus',()=>show(cursor));hit.addEventListener('keydown',e=>{if(['ArrowLeft','ArrowRight','Home','End'].includes(e.key)){e.preventDefault();show(e.key==='Home'?0:e.key==='End'?inspect.length-1:Math.max(0,Math.min(inspect.length-1,cursor+(e.key==='ArrowLeft'?-1:1))));}});
      hit.addEventListener('mousemove',e=>{const b=root.getBoundingClientRect();if(!b.width)return;const px=(e.clientX-b.left)/b.width*W,py=(e.clientY-b.top)/b.height*H;let best=-1,dist=Infinity;inspect.forEach((r,i)=>{const d=(x(r)-px)**2+(y(r.v)-py)**2*.3;if(d<dist){dist=d;best=i;}});if(best>=0)show(best);});
      out.textContent=valid.length?'Hover or focus the plot; arrow keys inspect retained observations.':band?'Saved reference; hover or focus the plot to inspect its retained quantiles and support.':'No accepted observations in selected water years.';
    }
    container.querySelectorAll('[data-chart-view]').forEach(b=>b.onclick=()=>{state.view=b.dataset.chartView;draw();});container.querySelectorAll('[data-chart-band]').forEach(b=>b.onclick=()=>{state.presentation=b.dataset.chartBand;draw();});container.querySelectorAll('[data-chart-years]').forEach(b=>b.onclick=()=>{state.years=yearSelection(years,b.dataset.chartYears);draw();});container.querySelector('[data-chart-scale]').onchange=e=>{state.scale=e.target.value;draw();};draw();
    return state;
  }
  return {dateTicks,day,wy,dowy,plotDay,currentDate,currentWY,num,row,good,segments,scale,yearSelection,snotel,snotelStatic,readout,csv,render};
});
