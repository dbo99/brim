/* Presentation intervals, in canonical mm. Never rewrite a retained sensor depth. */
(function(root, factory) {
  const api = factory();
  if (typeof module === 'object') module.exports = api;
  else root.BRIM.soilMoistureDepths = api;
})(typeof window === 'object' ? window : this, function() {
  'use strict';
  const bands = Object.freeze([[50,50.8],[100,101.6],[200,203.2],[500,508],[1000,1016]].map(
    ([lo,hi]) => Object.freeze({lo,hi,nominal:true})));
  function nodes(depths=[]) {
    // Stable presentation steps cover the verified catalog inventory even before
    // a network is enabled. A step is a filter interval, never a claim of data.
    const out = [...bands, ...[0,150,250,400,600,750,800,1200].map(d=>({lo:d,hi:d}))];
    for (const d of depths) if (Number.isFinite(d) && d>=0 && !out.some(n=>d>=n.lo&&d<=n.hi)) out.push({lo:d,hi:d});
    out.sort((a,b)=>a.lo-b.lo);
    return [...out, {lo:null,hi:null,open:true}];
  }
  function label(n, unit='native') {
    if (n.open) return 'Any deeper depth';
    if (n.lo===0 && n.hi===0) return 'Surface';
    if (unit==='mm') return (n.nominal?'≈':'')+n.lo+' mm';
    return n.nominal?'≈'+n.lo/10+' cm / '+Math.round(n.hi/25.4)+' in':n.lo/10+' cm';
  }
  function bounds(list, from, to) {
    if (!Number.isInteger(from)||!Number.isInteger(to)||from<0||to<from||to>=list.length||list[from].open) throw Error('Choose ordered depth steps');
    return {lower:list[from].lo,upper:list[to].open?null:list[to].hi};
  }
  function describe(lower,upper) {
    return upper===null?lower+' mm and deeper':lower===0?'Surface to '+upper+' mm':lower===upper?lower+' mm exact':lower+'–'+upper+' mm inclusive';
  }
  function contains(sensor, c) {
    return sensor.depth_mm===null?!!c.includeUnknown:Number.isFinite(sensor.depth_mm)&&sensor.depth_mm>=c.lower&&(c.upper===null||sensor.depth_mm<=c.upper);
  }
  return {bands,nodes,label,bounds,describe,contains};
});
