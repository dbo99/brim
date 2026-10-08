/* Pure, dependency-free display calculations. Node + browser share this code. */
(function (root, factory) {
  if (typeof module === 'object' && module.exports) module.exports = factory();
  else root.DendraCore = factory();
})(typeof globalThis !== 'undefined' ? globalThis : this, function () {
  'use strict';
  const DAY = 86400000;
  const day = s => Date.parse(s + 'T00:00:00Z') / DAY;
  const iso = n => new Date(n * DAY).toISOString().slice(0,10);
  const add = (s,n) => iso(day(s)+n);
  const wy = s => +s.slice(0,4) + (s.slice(5,7) >= '10' ? 1 : 0);
  const good = r => r && r.ok === true && typeof r.v === 'number' && Number.isFinite(r.v) && r.v >= 0 && r.v <= 100;
  const usable = (rows,end) => rows.filter(r => good(r) && (!end || r.date <= end));
  function latest(rows,end) { const a=usable(rows,end); return a.length ? a[a.length-1] : null; }
  function maxGap(rows,start,end) {
    const dates = new Set(rows.filter(good).map(r=>r.date)); let run=0,longest=0;
    for(let t=day(start); t<=day(end); t++) {run = dates.has(iso(t)) ? 0 : run+1; longest=Math.max(longest,run);}
    return longest;
  }
  function windowStats(rows,start,end) {
    const a=rows.filter(r=>good(r) && r.date>=start && r.date<=end), n=day(end)-day(start)+1;
    return {start,end,n:a.length,expected:n,mean:a.length?a.reduce((s,r)=>s+r.v,0)/a.length:null,maxGap:maxGap(rows,start,end)};
  }
  function change(rows,end,span=7,deadband=0.5,staleDays=3) {
    if(![7,14,30].includes(span)) throw new Error('Unsupported comparison window');
    if(!Number.isFinite(deadband)||deadband<0) throw new Error('Invalid display deadband');
    const last=latest(rows,end), recent=windowStats(rows,add(end,-span+1),end), prior=windowStats(rows,add(end,-2*span+1),add(end,-span));
    const lag=last?day(end)-day(last.date):null;
    const base={last,lag,recent,prior,span,deadband,delta:null,required:Math.ceil(span*0.8)};
    if(!last) return {...base,state:'no-data',symbol:'?',label:'No accepted daily value'};
    if(lag>staleDays) return {...base,state:'stale',symbol:'◷',label:'Stale daily record'};
    if(recent.n<base.required || prior.n<base.required || recent.maxGap>2 || prior.maxGap>2)
      return {...base,state:'insufficient',symbol:'?',label:'Insufficient recent coverage'};
    const delta=recent.mean-prior.mean;
    return {...base,delta,state:delta>deadband?'wetting':delta<-deadband?'drying':'little',
      symbol:delta>deadband?'↑':delta<-deadband?'↓':'↔',
      label:delta>deadband?'Recent wetting':delta<-deadband?'Recent drying':'Little net change'};
  }
  function coverage(rows,end) {
    const first=rows.length?wy(rows[0].date):wy(end), last=wy(end), out=[];
    for(let y=first;y<=last;y++) {
      const start=(y-1)+'-10-01', stop=end<y+'-09-30'?end:y+'-09-30';
      const a=rows.filter(r=>r.date>=start&&r.date<=stop), valid=a.filter(good);
      out.push({wy:y,start,end:stop,n:valid.length,expected:day(stop)-day(start)+1,
        first:valid.length?valid[0].date:null,last:valid.length?valid[valid.length-1].date:null,
        excluded:a.filter(r=>!good(r)&&r.n>0).length,longestGap:maxGap(a,start,stop)});
    }
    return out;
  }
  function segments(rows) {
    const result=[];let part=[],prev=null;
    for(const r of rows){
      if(!good(r)){if(part.length)result.push(part);part=[];prev=null;continue;}
      if(prev && day(r.date)-day(prev.date)!==1){if(part.length)result.push(part);part=[];}
      part.push(r);prev=r;
    }
    if(part.length)result.push(part);return result;
  }
  function resolve(station,depth,streams,choices={}) {
    const imported=station.streams.map(id=>streams[id]).filter(Boolean);
    let candidates;
    if(depth==='site') {
      if(choices[station.id] && imported.some(s=>s.id===choices[station.id])) return {stream:streams[choices[station.id]],reason:'explicit site choice'};
      if(station.requires_explicit_sensor) return {stream:null,reason:'Choose an exact sensor; no producer primary supplied'};
      candidates=imported.slice().sort((a,b)=>(a.depth??Infinity)-(b.depth??Infinity)||a.id.localeCompare(b.id));
      if(candidates.length===1) return {stream:candidates[0],reason:'only imported sensor'};
      const known=candidates.filter(s=>s.depth!==null);
      if(known.length && known.filter(s=>s.depth===known[0].depth).length===1) return {stream:known[0],reason:'shallowest explicitly reported imported depth'};
      return {stream:null,reason:candidates.length?'Choose a site sensor; depth is ambiguous':'Daily history not imported'};
    }
    candidates=imported.filter(s=>depth==='unknown'?s.depth===null:s.depth===Number(depth));
    const chosen=station.requires_explicit_sensor&&candidates.find(s=>s.id===choices[station.id]);
    return chosen ? {stream:chosen,reason:'explicit exact sensor choice'} : candidates.length===1 ? {stream:candidates[0],reason:'exact depth selection'} : {stream:null,reason:candidates.length?'Multiple sensors at this depth; not averaged':'No imported sensor at this depth'};
  }
  function sensorLabel(s) { return (s.depth_cm===null?(s.depth_status==='UNKNOWN'?'Unknown depth':'Depth unspecified'):s.depth_cm+' cm')+(s.orientation?' · '+s.orientation:''); }
  function uniqueSuffix(value,values) {
    let n=Math.min(6,value.length);
    while(n<value.length&&values.some(v=>v!==value&&v.endsWith(value.slice(-n))))n++;
    return (n<value.length?'…':'')+value.slice(-n);
  }
  function instrumentLabel(s,streams) {
    if(s.export_local_series_key&&s.depth_cm===null&&s.depth_status==='UNKNOWN') {
      const label=sensorLabel(s),peers=streams.filter(x=>x.station_id===s.station_id&&x.export_local_series_key&&x.depth_status==='UNKNOWN'&&sensorLabel(x)===label);
      if(new Set(peers.map(x=>x.export_local_series_key+'|stream:'+x.stream_id)).size<2)return label;
      let token='stream '+uniqueSuffix(s.stream_id,peers.map(x=>x.stream_id));
      // If an exact provider stream occurs in distinct exports, retain that
      // distinction with supplied export identity fragments, never an ordinal.
      const exports=peers.filter(x=>x.stream_id===s.stream_id).map(x=>x.export_local_series_key.split(':'));
      if(new Set(exports.map(x=>x.join(':'))).size>1){
        const [,hash,,column]=s.export_local_series_key.split(':'),sameColumn=exports.filter(x=>x[3]===column);
        token+=' · export column '+column;
        if(new Set(sameColumn.map(x=>x[1])).size>1)token+=' / '+uniqueSuffix(hash,sameColumn.map(x=>x[1]));
      }
      return label+' · '+token;
    }
    const peers=streams.filter(x=>x.depth_cm===s.depth_cm&&x.orientation===s.orientation);
    // Local series identity includes the export key, even when provider IDs or
    // human-readable metadata coincide. Never substitute an ordinal or depth.
    if(s.export_local_series_key)return sensorLabel(s)+' · '+s.stream_id+' · '+s.export_local_series_key;
    return sensorLabel(s)+(peers.length>1?' · sensor '+(peers.indexOf(s)+1):'');
  }
  return {DAY,day,iso,add,wy,good,usable,latest,maxGap,windowStats,change,coverage,segments,resolve,sensorLabel,instrumentLabel};
});
