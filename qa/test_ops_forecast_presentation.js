'use strict';
// Source/offline fixtures only. No provider, browser or filesystem writes.
const assert = require('node:assert/strict');
const {fixture, raw, source} = require('./test_ops_radar_qpe_contracts.js');
const cases=[];
const test=(name,run)=>cases.push({name,run});
const text=s=>s.replace(/<[^>]*>/g,'').replace(/&amp;/g,'&');
function context(){const f=fixture();f.run(raw(source.qpfTime));return f.context;}
function required(c,n){assert.equal(typeof c[n],'function',n+' must exist');return c[n];}
const interval={valid_time:'00Z 09/17/26 - 00Z 09/18/26',start_time:'2026-09-17 00:00:00',end_time:'2026-09-18 00:00:00'};
test('causal layout: actual QPF tooltip CSS has no 420px floor',()=>{
 const css=raw(source.shared).match(/\.leaflet-tooltip\.pt-ops-wpc-qpf-tooltip\s*\{([^}]+)\}/)[1];
 assert(!/min-width:\s*420px/.test(css));assert.match(css,/box-sizing:\s*border-box/);assert.match(css,/320px/);
});
test('causal layout: actual forecast shell has scoped padding and rounded border',()=>{
 assert.match(raw(source.shared),/\.pt-ops-forecast-card\s*\{[^}]*padding:\s*(10|11|12)px/);
 assert.match(raw(source.shared),/\.pt-ops-forecast-card\s*\{[^}]*border-radius:\s*8px/);
});
test('forecast header is a scoped row; chronology precedes scale',()=>{
 const html=context().ptOpsForecastCardHtml('WPC QPF Day 1',{forecastProductId:'ops_wpc_qpf_day_1'});
 assert(html.includes('pt-forecast-head'));assert(html.indexOf('data-forecast-field="valid"')<html.indexOf('pt-forecast-scale'));
});
test('compact validated Pacific interval retains clock and weekday',()=>{
 const c=context();assert.equal(required(c,'ptForecastCompactInterval')('2026-09-17T00:00:00.000Z','2026-09-18T00:00:00.000Z'),'Wed 9/16 5 PM → Thu 9/17 5 PM PDT');
});
test('compact interval retains nonzero minutes and endpoint DST',()=>{
 const c=context(),fn=required(c,'ptForecastCompactInterval');assert.match(fn('2026-11-01T08:15:00Z','2026-11-01T12:30:00Z'),/1:15 AM PDT → Sun 11\/1 4:30 AM PST/);
});
test('cross-year local interval shows both years',()=>{
 const c=context(),s=required(c,'ptForecastCompactInterval')('2026-12-31T20:00:00Z','2027-01-02T00:00:00Z');assert(s.includes('/2026')&&s.includes('/2027'));
});
test('CPC calendar days do not acquire a timezone',()=>{
 const c=context(),p=required(c,'ptForecastPresentation')('cpc',{issued:'2026-09-16',valid:'2026-09-22 – 2026-09-26',utc:'No timezone conversion',scope:'Loaded-feature metadata only; not a global census'});
 assert.equal(p.valid,'Tue 9/22 – Sat 9/26');assert.equal(p.issued,'Wed 9/16');assert(!/PDT|PST/.test(p.valid));
});
test('unqualified WPC issue clock stays explicitly unverified',()=>{
 const c=context(),p=required(c,'ptForecastPresentation')('qpf',{issued:'2026-09-16 22:12:18 (time zone Unverified)',valid:'Unverified',utc:'Unverified',scope:'Mixed/incomplete — Unverified'});
 assert(p.issued.includes('10:12:18 PM'));assert(p.issued.includes('zone unverified'));assert.equal(p.valid,'Unverified');assert(/Mixed\/incomplete/.test(p.scope));
});
test('H1 supersedes amount-only: product amount duration and same-feature interval',()=>{
 const c=context(),s=text(c.wpcQpfFeatureHtml('WPC QPF Day 1',{...interval,qpf:1.25,units:'in'}));
 assert(s.includes('WPC QPF Day 1'));assert(s.includes('1.25 in · 24 hr'));assert(s.includes('Wed 9/16 5 PM'));assert(s.includes('17/00Z → 18/00Z'));assert(s.includes('WPC polygon'));assert(!s.includes('image match unverified'));assert(!s.includes('exact grid'));
});
for(const [name,attrs,hours] of [
 ['short actual day', {valid_time:'04Z 09/17/26 - 00Z 09/18/26',start_time:'2026-09-17 04:00:00',end_time:'2026-09-18 00:00:00'},20],
 ['DST crossing',{valid_time:'08Z 11/01/26 - 12Z 11/01/26',start_time:'2026-11-01 08:00:00',end_time:'2026-11-01 12:00:00'},4],
 ['three days',{valid_time:'00Z 09/17/26 - 00Z 09/20/26',start_time:'2026-09-17 00:00:00',end_time:'2026-09-20 00:00:00'},72],
 ['seven days',{valid_time:'00Z 09/17/26 - 00Z 09/24/26',start_time:'2026-09-17 00:00:00',end_time:'2026-09-24 00:00:00'},168]
])test('H1 '+name+' uses elapsed validated duration, not product label',()=>{
 assert(text(context().wpcQpfFeatureHtml('WPC QPF Day 1',{...attrs,qpf:0,units:'in'})).includes('0.00 in · '+hours+' hr'));
});
for(const attrs of [{},{...interval,valid_time:interval.valid_time.replaceAll('Z','')},{...interval,start_time:'2027-09-17 00:00:00'}])test('H1 missing/conflicting interval keeps amount, omits invented hours',()=>{
 const s=text(context().wpcQpfFeatureHtml('WPC QPF 7-day',{...attrs,qpf:2,units:'in'}));assert(s.includes('2.00 in'));assert(s.includes('Valid time unverified'));assert(!/\d+ hr|\d+Z/.test(s));
});
test('H1 month/year crossing UTC shorthand is unambiguous',()=>{
 const c=context(),fn=required(c,'ptForecastCompactUtc');assert.equal(fn('2026-12-31T00:00:00Z','2027-01-02T00:00:00Z'),'12/31/2026/00Z → 1/2/2027/00Z');
 assert.equal(fn('2026-09-30T00:15:00Z','2026-10-01T01:30:00Z'),'9/30/0015Z → 10/1/0130Z');
});
test('H1 zero thresholds ranges no-data and escaping remain truthful',()=>{
 const c=context();for(const v of ['<0.01','0.25–0.50',0])assert(c.wpcQpfFeatureHtml('<img onerror=x>',{...interval,qpf:v,units:'in'}).includes('&lt;img'));
 for(const v of [null,'',false,-1,NaN,'bad'])assert.equal(c.wpcQpfFeatureHtml('QPF',{...interval,qpf:v,units:'in'}),'');
});
test('H1 nearby-envelope wording does not claim exact cursor or grid',()=>{
 const s=text(context().wpcQpfFeatureHtml('QPF',{...interval,qpf:1,units:'in'},true));assert(s.includes('Nearby WPC polygon'));assert(!s.includes('exact'));
});
test('QPF contour scale uses two contiguous strips with boundary ticks',()=>{
 const c=context(),html=c.ptOpsForecastScaleHtml({forecastProductId:'ops_wpc_qpf_day_1'});
 assert(html.includes('pt-qpf-contour-scale'));assert(!html.includes('pt-forecast-steps'));
 assert.equal((html.match(/class="pt-qpf-contour-strip"/g)||[]).length,2);
 assert.equal((html.match(/data-qpf-boundary=/g)||[]).length,18);
 assert.equal((html.match(/pt-ops-swatch/g)||[]).length,18);
 assert(html.includes('pt-qpf-boundary-tick'));assert(!html.includes('pt-qpf-contour-zero'));
 assert(!html.includes('0 — real zero'));
 assert(html.includes('<div class="pt-qpf-contour-note">No color = no forecast precipitation</div>'));
 assert(!html.includes('Missing/no-data is not zero.'));
});
test('QPF positive bands fill their strip without swatch gaps or card expansion',()=>{
 const css=raw(source.shared),rule=n=>{const m=css.match(new RegExp('\\.pt-ops-forecast-card \\.'+n+'\\s*\\{([^}]+)\\}'));assert(m,'Missing scoped '+n);return m[1];};
 assert.match(rule('pt-qpf-contour-strip'),/display:\s*flex/);assert.match(rule('pt-qpf-contour-strip'),/gap:\s*0/);
 assert.match(rule('pt-qpf-contour-band'),/flex:\s*1 1 0/);assert.match(rule('pt-qpf-contour-band'),/min-width:\s*0/);
 assert.match(css,/\.pt-qpf-contour-strip \.pt-ops-swatch\s*\{[^}]*width:\s*100%/);
 assert.match(rule('pt-qpf-boundary-tick'),/white-space:\s*nowrap/);
 // Edge labels use the upper track; they cannot run into adjacent lower-track labels.
 assert.match(css,/\.pt-qpf-contour-band:first-child \.pt-qpf-boundary-tick\s*\{[^}]*top:\s*-18px/);
 assert.match(css,/\.pt-qpf-contour-cap \.pt-qpf-boundary-tick\s*\{[^}]*top:\s*-18px/);
 assert.match(css,/\.pt-ops-forecast-card\s*\{[^}]*width:\s*340px;[^}]*max-width:\s*min\(360px/);
});
test('QPF legend discloses endpoint uncertainty without emitting invented extreme rules',()=>{
 const c=context(),def={forecastProductId:'ops_wpc_qpf_day_1'},html=c.ptOpsForecastScaleHtml(def)+c.ptOpsForecastCardHtml('QPF',def),s=text(html).replaceAll('&lt;','<').replaceAll('&gt;','>');
 assert.match(s,/contour/i);assert(s.includes("Legend uses provider boundary values; endpoint inclusion is not separately specified"));assert.match(s,/does not separately establish top-bin semantics/i);
 assert.match(s,/unique-value renderer/i);assert(!/<\s*0\.01|0\s*[–−-]\s*0\.01|(?:>=|≥)\s*20|20\s*\+/.test(s));
 assert(!/read down the left column/i.test(s));
});
test('CPC grouping places neutral between Below and Above; unknown distinct',()=>{
 const c=context(),entries=['Above normal <40%','Below normal <40%','Near Normal','Equal Chances','Unknown'].map(label=>({label,color:'#888'}));
 const html=c.ptOpsForecastScaleHtml({forecastProductId:'ops_cpc_6_10_temperature',forecastScale:{entries}});
 assert(html.indexOf('<h5>Below normal</h5>')>=0);assert(html.indexOf('<h5>Below normal</h5>')<html.indexOf('<h5>Near normal</h5>'));assert(html.indexOf('<h5>Near normal</h5>')<html.indexOf('<h5>Above normal</h5>'));assert(html.includes('Unknown'));assert(html.includes('Equal Chances'));
});
// E45 presentation-only regressions, through emitted scale and card fields.
test('QPF high ticks are compact integers; join repeats 2 and 2.50 stays exact',()=>{
 const c=context(),html=c.ptOpsForecastScaleHtml({forecastProductId:'ops_wpc_qpf_day_1'});
 const ticks=[...html.matchAll(/class="pt-qpf-boundary-tick"[^>]*>([^<]+)</g)].map(m=>m[1]);
 assert.deepEqual(ticks,['0.01','0.10','0.25','0.50','0.75','1.00','1.25','1.50','1.75','2','2','2.50','3','4','5','7','10','15','20']);
 assert.deepEqual(Array.from(c.ptWpcQpfScale,r=>r.value),[0,.01,.1,.25,.5,.75,1,1.25,1.5,1.75,2,2.5,3,4,5,7,10,15,20]);
 assert.equal(c.wpcQpfFeatureHtml('QPF',{...interval,qpf:2,units:'in'}).includes('2.00 in'),true);
});
test('QPF strip tracks have explicit noncollapsing spacing without wider card',()=>{
 const css=raw(source.shared),rule=css.match(/\.pt-ops-forecast-card \.pt-qpf-contour-strips\s*\{([^}]+)\}/);
 assert(rule,'Separate flex stack owns noncollapsing gap');assert.match(rule[1],/display:\s*flex/);assert.match(rule[1],/flex-direction:\s*column/);
 const gap=Number(rule[1].match(/gap:\s*(\d+)px/)[1]);assert(gap>=44,'14px lower text + 18px upper offset + breathing room');
 assert.match(css,/\.pt-qpf-contour-strips > \.pt-qpf-contour-strip\s*\{[^}]*margin:\s*0/);
 const html=context().ptOpsForecastScaleHtml({forecastProductId:'ops_wpc_qpf_day_1'});assert(html.includes('class="pt-qpf-contour-strips"'));
 assert.match(css,/\.pt-ops-forecast-card\s*\{[^}]*width:\s*340px;[^}]*max-width:\s*min\(360px/);
});
test('normal visible QPF qualification omits permanent warning; exact details retained',()=>{
 const f=fixture(),c=f.context;c.ptOpsActivateLayerByName('WPC QPF Day 1');
 const def=c.activeLegendDefs['WPC QPF Day 1'];def.forecastMetadata=c.ptForecastMetadata('qpf',[{attributes:{...interval,issue_time:'2026-09-16 22:00:00'}}],false);c.redrawLegend();
 const card=f.container.querySelector('[data-forecast-product="ops_wpc_qpf_day_1"]');
 const scope=card.querySelector('[data-forecast-field="scope"]').textContent;
 assert.equal(scope,'');
 assert(card.querySelector('details').textContent.includes('Image and metadata cycle alignment is not independently confirmed'));assert(!card.querySelector('[data-forecast-field="fullscope"]').textContent.includes('image-cycle match Unverified'));
});
test('QPF exceptional visible qualifiers survive warning deduplication',()=>{
 const c=context();for(const warning of ['no loaded metadata','mixed metadata — dates Unverified','missing/invalid metadata — dates Unverified','incomplete response — dates Unverified','metadata request failed']){
  const p=c.ptForecastPresentation('qpf',{issued:'Unverified',valid:'Unverified',utc:'Unverified',scope:'Provider forecast metadata; image-cycle match Unverified; '+warning});
  assert(p.scope.includes(warning));assert.equal((p.scope.match(/image-cycle match unverified/gi)||[]).length,0);assert.equal(p.valid,'Unverified');
 }
});

for (const [id,name] of [['ops_wpc_qpf_day_1','WPC QPF Day 1'],['ops_wpc_qpf_day_2','WPC QPF Day 2'],['ops_wpc_qpf_day_3','WPC QPF Day 3'],['ops_wpc_qpf_3day','WPC QPF 3-day'],['ops_wpc_qpf_7day','WPC QPF 7-day']]) {
 test('E53 '+id+' healthy card is concise and exact qualifications collapsed',()=>{
  const f=fixture(),c=f.context;c.ptOpsActivateLayerByName(name);
  c.activeLegendDefs[name].forecastMetadata=c.ptForecastMetadata('qpf',[{attributes:{...interval,issue_time:'2026-09-16 22:00:00'}}],false);c.redrawLegend();
  const card=f.container.querySelector('[data-forecast-product="'+id+'"]'),details=card.querySelector('details');
  assert.equal(details.getAttribute('open'),null);
  const visible=card.children.filter(n=>n!==details).map(n=>n.textContent).join('');
  assert.equal((visible.match(/No color = no forecast precipitation/g)||[]).length,1);
  assert(!/Missing\/no-data is not zero|Exact ranges unverified|Image-cycle match unverified/i.test(visible));
  for(const line of ['Legend uses provider boundary values; endpoint inclusion is not separately specified','Image and metadata cycle alignment is not independently confirmed']) assert.equal(details.children.filter(n=>n.textContent===line).length,1);
  assert.equal(card.querySelectorAll('[data-qpf-boundary]').length,18);
  for(const warning of ['no loaded metadata','mixed metadata — dates Unverified','missing/invalid metadata — dates Unverified','incomplete response — dates Unverified','metadata request failed']) {
   c.activeLegendDefs[name].forecastMetadata={issued:'Unverified',valid:'Unverified',utc:'Unverified',scope:'Provider forecast metadata; image-cycle match Unverified; '+warning};c.redrawLegend();
   assert(card.querySelector('[data-forecast-field="scope"]').textContent.includes(warning));
  }
 });
}

async function main(){let passed=0,failed=0;for(const t of cases){try{await t.run();passed++;console.log('PASS '+t.name);}catch(e){if(e.code!=='ERR_ASSERTION')throw e;failed++;console.log('FAIL '+t.name+': '+e.message);}}console.log('RESULT passed='+passed+' failed='+failed);process.exitCode=failed?1:0;}
module.exports={context,interval,cases};
if(require.main===module)main().catch(e=>{console.error('HARNESS_ERROR',e.stack);process.exitCode=2;});
