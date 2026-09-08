#!/usr/bin/env node
'use strict';

// No-install model and small DOM harness. This is not mounted-browser evidence.
// The existing suite supplies its public projection and preserves its full assertions.
const { createModel, resources: fixtureResources, registry, relationships, extractFunction } =
  require('./test_guide_resource_explorer.js');
const fs = require('fs');
const path = require('path');
const assert = require('assert');
const crypto = require('crypto');
const rootPath = path.resolve(__dirname, '..');
const source = fs.readFileSync(path.join(rootPath, '03_functions/js/leaflet_brim_guide.js'), 'utf8');
const css = fs.readFileSync(path.join(rootPath, '03_functions/css/leaflet_brim_guide.css'), 'utf8');
const bundle = process.env.BRIM_PROVIDER_TEST_BUNDLE
  ? JSON.parse(fs.readFileSync(process.env.BRIM_PROVIDER_TEST_BUNDLE, 'utf8'))
  : { resources: fixtureResources };
const before = JSON.stringify({ bundle, registry, relationships });
const resources = bundle.resources;
const model = createModel(resources);
const policy = model.providerPolicy();
assert.deepStrictEqual(policy.groups, [
  { id: 'federal', label: 'Federal' }, { id: 'state', label: 'State' }
]);
// Independent approved roster: never generated from the implementation or catalog counts.
const expectedRoster = [
  ['blm', 'BLM', 'federal'], ['epa', 'EPA', 'federal'], ['fema', 'FEMA', 'federal'],
  ['nasa', 'NASA', 'federal'], ['noaa', 'NOAA', 'federal'], ['usace', 'USACE', 'federal'],
  ['usbr', 'USBR', 'federal'], ['usda', 'USDA', 'federal'], ['usgs', 'USGS', 'federal'],
  ['dwr', 'DWR', 'state'], ['waterboards', 'Water Boards', 'state']
];
const expectedLabels = Object.fromEntries(expectedRoster.map(([id, label]) => [id, label]));
assert.deepStrictEqual(policy.families.map(f => [f.id, f.label, f.group]), expectedRoster);
assert.deepStrictEqual(policy.provider_roles, ['display_provider', 'publisher', 'maintainer', 'partner']);
const allFamilies = policy.families.concat(policy.legacy_families);
assert.equal(new Set(allFamilies.map(f => f.id)).size, 24);
assert.deepStrictEqual(policy.groups.map(g => policy.families.filter(f => f.group === g.id).length), [9, 2]);
assert.deepStrictEqual(policy.legacy_families.map(f => f.id).sort(),
  ['aso','calfire','caloes','cdfa','climateengine','cvfpb','cw3e','nifc','nsidc','pivotal','prism','synoptic','windy']);
const initial = model.createState();
const initialIds = model.results(initial).map(r => r.id);
assert.equal(initialIds.length, 210);
assert.equal(new Set(initialIds).size, 210);
assert.deepStrictEqual(new Set(initialIds), new Set(resources.map(r => r.id)));
assert.deepStrictEqual(model.facetCounts(initial).presets,
  { in_brim_map: 27, beyond_the_map: 183, all_resources: 210 });
assert.deepStrictEqual(initial.providers, []);
assert(!('providerGroupsOpen' in initial));
assert(!('narrow' in initial));
assert.deepStrictEqual(model.createState({ providerGroupsOpen: ['other'], narrow: true }), initial);
const expectedCounts = { blm: 1, usbr: 13, epa: 4, fema: 2, nasa: 19, nifc: 1,
  noaa: 24, usace: 4, usda: 10, usgs: 14, calfire: 1, caloes: 1, cdfa: 1,
  cvfpb: 1, dwr: 13, waterboards: 5, aso: 1, cw3e: 3, climateengine: 1,
  nsidc: 3, pivotal: 2, prism: 1, synoptic: 3, windy: 1 };
assert.deepStrictEqual(model.facetCounts(initial).providers,
  Object.fromEntries(Object.entries(expectedCounts).map(([id, count]) => ['family:' + id, count])));
const classification = registry.resources.map(r => ({ id: r.id,
  public: r.publication_state === 'published',
  families: model.providerMembership({ id: r.id, title: r.title,
    provider: r.providers.find(p => p.role === 'display_provider').name,
    providers: r.providers, canonicalUrl: r.canonical_url }) }));
assert.equal(classification.length, 215);
assert.equal(classification.filter(r => r.public && r.families.length).length, 127);
assert.equal(classification.filter(r => r.public && !r.families.length).length, 83);
assert.equal(classification.filter(r => r.public).reduce((n, r) => n + r.families.length, 0), 129);
assert.equal(new Set(resources.map(r => r.provider)).size, 110);
assert(classification.filter(r => !r.public).every(r => !initialIds.includes(r.id)));
// Exercise every explicit spelling through the single exposed mapping; no fuzzy fallback.
for (const family of allFamilies) for (const name of family.exact_provider_names) {
  for (const role of policy.provider_roles) {
    assert(model.providerMembership({ id: 'fixture', provider: 'Uncurated source',
      providers: [{ role, name: '  ' + name.toUpperCase().replace(/ /g, '  ') + '  ' }]
    }).includes('family:' + family.id), `${family.id}: ${name} / ${role}`);
  }
  assert(!model.providerMembership({ id: 'fixture', provider: 'Uncurated source',
    providers: [{ role: 'data_owner', name }] }).includes('family:' + family.id));
  assert(!model.providerMembership({ id: 'fixture', provider: 'Prefix ' + name,
    providers: [] }).includes('family:' + family.id));
}
assert.deepStrictEqual(model.providerMembership({ id: 'fixture', provider: 'Uncurated source',
  providers: [], title: 'NASA NOAA USGS', searchText: 'NASA',
  canonicalUrl: 'https://www.nasa.gov', representedProducts: [{ title: 'NASA' }] }), []);
assert.deepStrictEqual(model.providerMembership({ id: 'fixture', provider: 'NIFC / WFIGS' }), ['family:nifc']);
assert(!model.providerMembership({ id: 'fixture', provider: 'U.S. Department of Agriculture' }).includes('family:usbr'));
assert(!model.providerMembership({ id: 'fixture', provider: 'USDA-NRCS' }).includes('family:usda'));
assert.equal(policy.exact_resource_rules.length, 3);
for (const rule of policy.exact_resource_rules) {
  const r = resources.find(r => r.id === rule.resource_id);
  assert(r);
  assert(rule.families.every(id => model.providerMembership(r).includes('family:' + id)));
  for (const field of ['provider', 'title', 'canonicalUrl']) {
    assert.throws(() => createModel([{ ...r, [field]: r[field] + ' changed' }]), /guard mismatch/);
  }
  assert.deepStrictEqual(model.providerMembership({ ...r, id: 'similar-but-not-reviewed' }), []);
}
for (const id of ['resource_noaa_cnrfc', 'resource_noaa_nws_graphical_forecasts',
  'resource_noaa_wpc_excessive_rainfall_outlook']) {
  assert(model.results(model.createState({ providers: ['family:noaa'] })).some(r => r.id === id));
}
const gsas = resources.find(r => /All Groundwater Sustainability Agencies/i.test(r.title));
assert(gsas && model.providerMembership(gsas).includes('family:dwr'));
for (const pair of [['family:nasa', 'family:nsidc'], ['family:nasa', 'family:usda']]) {
  const joint = resources.filter(r => pair.every(f => model.providerMembership(r).includes(f)));
  assert.equal(joint.length, 1);
  const results = model.results(model.createState({ providers: pair }));
  assert.equal(results.filter(r => r.id === joint[0].id).length, 1);
  const union = new Set(pair.flatMap(f => model.results(model.createState({ providers: [f] })).map(r => r.id)));
  assert.deepStrictEqual(new Set(results.map(r => r.id)), union);
}
// Independent count oracle: exact Set intersections for all dimensions, ignoring providers.
for (const input of [ {}, { query: 'NASA' }, { query: 'water', preset: 'in_brim_map' },
  { subject: 'Climate & Drought', informationType: 'Forecast / Outlook' },
  { resourceType: 'organization_homepage' }, { productContextId: 'ops_cdec_reservoir_storage' } ]) {
  const state = model.createState({ ...input, providers: ['family:noaa', 'family:nasa'] });
  const otherMatches = model.results(model.clearProviders(state));
  for (const option of model.providerOptions(state)) {
    const ids = new Set(otherMatches.filter(r => model.providerMembership(r).includes(option.value)).map(r => r.id));
    assert.equal(option.count, ids.size, JSON.stringify(input) + option.value);
  }
  const expected = otherMatches.filter(r => state.providers.some(f => model.providerMembership(r).includes(f)));
  assert.deepStrictEqual(model.results(state).map(r => r.id), expected.map(r => r.id));
}
for (const group of policy.groups) {
  const options = model.providerOptions(initial).filter(o => o.group === group.id);
  assert.deepStrictEqual(options.map(o => o.label), options.map(o => o.label).sort((a,b) => a.toLowerCase().localeCompare(b.toLowerCase())));
}
assert.deepStrictEqual(model.providerOptions(model.createState({ providerQuery: 'impossible' })), model.providerOptions(initial));
const zero = model.createState({ query: 'zzzzzzzzzz', providers: ['family:noaa'], subject: 'Climate & Drought' });
assert.equal(model.results(zero).length, 0);
assert.equal(model.providerOptions(zero).length, 11);
assert(model.providerOptions(zero).every(o => o.count === 0));
assert.equal(model.chips(zero).filter(c => c.key === 'provider').length, 1);
const cleared = model.clearProviders(zero);
assert.equal(cleared.query, zero.query); assert.equal(cleared.subject, zero.subject);
assert.deepStrictEqual(cleared.providers, []);
assert.equal(cleared.focusKey, 'resource-provider-family%3Ablm');
assert.deepStrictEqual(model.removeChip(zero, 'provider', 'family:noaa').providers, []);
for (const nameQuery of ['Westlands', 'Chino Basin']) {
  const r = resources.find(r => r.provider.includes(nameQuery)); assert(r);
  const legacy = model.createState({ providers: [r.provider], query: '' });
  assert(model.results(legacy).every(item => item.provider === r.provider));
  assert(model.results(legacy).some(item => item.id === r.id));
  assert(model.chips(legacy)[0].label.startsWith('Exact provider: '));
  assert(!model.providerOptions(legacy).some(o => o.label.includes(nameQuery)));
}
const stale = model.createState({ query: 'water', providers: ['family:noaa', 'Unknown old provider', 'family:retired'] });
assert.equal(model.chips(stale).filter(c => c.label.startsWith('Unavailable provider: ')).length, 2);
assert.equal(model.removeChip(stale, 'provider', 'Unknown old provider').query, 'water');
assert(model.removeChip(stale, 'provider', 'Unknown old provider').providers.includes('family:noaa'));
assert.equal(model.results(model.createState({ providers: ['Unknown old provider'] })).length, 0);
let state = model.createState({ providers: ['family:noaa'], query: 'water',
  productContextId: 'ops_cdec_reservoir_storage', resultsScrollTop: 300, facetScrollTop: 120,
  returnResultsScrollTop: 300 });
assert.deepStrictEqual(model.providerGroups(state), policy.groups);
const selected = model.selectResource(state, 'resource_noaa_cnrfc');
const saved = model.snapshot(selected);
const restored = model.restore(saved);
assert.deepStrictEqual(restored, selected);
assert(!('providerGroupsOpen' in model.escape(restored).state));
assert.equal(model.escape(restored).state.productContextId, state.productContextId);
assert.equal(model.escape(restored).state.resultsScrollTop, 300);
assert.equal(model.setQuery(state, 'forecast').facetScrollTop, 120);
assert(!('providerGroupsOpen' in model.setQuery(state, 'forecast')));
assert.equal(model.reset(state).query, '');
assert.deepStrictEqual(model.reset(state).providers, []);
assert.equal(model.reset(state).productContextId, '');
assert.equal(model.reset(state).facetScrollTop, 0);
assert(!('providerGroupsOpen' in model.reset(state)));
// Exact A5E parity in the working evidence area, or unchanged Git model for portable runs.
let parityCases;
if (process.env.BRIM_PROVIDER_PARITY_BASELINE) {
  parityCases = JSON.parse(fs.readFileSync(process.env.BRIM_PROVIDER_PARITY_BASELINE)).cases;
} else {
  const { execFileSync } = require('child_process');
  const baseline = execFileSync('git', ['--no-optional-locks', '-C', rootPath, 'show',
    '3cdfc886da1aadcd447bb8c2b52d6d185fda0a97:03_functions/js/leaflet_brim_guide.js'],
  { encoding: 'utf8', env: { ...process.env, GIT_OPTIONAL_LOCKS: '0' } });
  assert.equal(crypto.createHash('sha256').update(baseline).digest('hex'),
    'c893c8a750842400203215e24a2b9a7be82e93828fa0a41530e36af1685dfcc5');
  const oldModel = new Function('return (' + extractFunction(baseline, 'ptCreateResourceExplorerModel') + ')')()(resources);
  parityCases = ['all_resources', 'in_brim_map', 'beyond_the_map'].flatMap(preset =>
    ['', 'Westlands', 'Chino Basin', 'GSA', 'BLM', 'ACEC', 'Federal Wilderness', 'CadNSDI',
      'CAL FIRE', 'NIFC', 'WFIGS', 'CW3E', 'Pivotal', 'Windy', 'Success Dam', 'SCSC1', 'NASA', 'NOAA'].flatMap(query =>
      ['', 'ops_cdec_reservoir_storage'].map(productContextId => {
        const input = { preset, query, productContextId };
        return { input, ids: oldModel.results(oldModel.createState(input)).map(r => r.id) };
      })));
}
assert.equal(parityCases.length, 108);
for (const { input, ids } of parityCases) {
  assert.deepStrictEqual(model.results(model.createState(input)).map(r => r.id), ids, JSON.stringify(input));
}

// A synthetic catalog may grow or shrink without changing any checkbox choice or order.
const expectedOptions = expectedRoster.map(([id,label,group]) => ({ value: 'family:'+id, label, group }));
for (const count of [0,1,35]) {
  const synthetic = allFamilies.flatMap(f => Array.from({ length: count }, (_,i) => ({
    ...resources[0], id: 'synthetic_'+f.id+'_'+i, title: f.id+' '+i,
    provider: f.exact_provider_names[0], providers: [], representedProducts: []
  })));
  const sm = createModel(synthetic);
  for (const input of [{}, { query: 'zzzzzzzzzz', providers: ['family:blm'] },
    { subject: 'nonexistent' }, { preset: 'beyond_the_map' }, { providers: ['family:nifc'] }]) {
    const options = sm.providerOptions(sm.createState(input));
    assert.deepStrictEqual(options.map(({count,...option}) => option), expectedOptions);
    assert(options.some(o=>o.value==='family:blm'));
    assert(!options.some(o=>o.value==='family:nifc'));
    if (input.query) assert(options.every(o=>o.count===0));
  }
  assert.equal(sm.providerOptions(sm.createState()).find(o=>o.value==='family:blm').count,count);
}
const optionSource = extractFunction(source,'providerOptions');
assert(!/\.filter\(|\.sort\(|\.slice\(/.test(optionSource), 'Roster must not be filtered, promoted or sorted from counts');
assert(!/providerGroupsOpen|toggleProviderGroup|providerMinCount|minimumProviderCount|providerEligibility/.test(source));
const nifc = resources.find(r=>r.provider==='NIFC / WFIGS'); assert(nifc);
for (const query of ['NIFC','WFIGS']) assert(model.results(model.createState({query})).some(r=>r.id===nifc.id));
const oldNifc = model.createState({providers:['family:nifc']});
assert.deepStrictEqual(model.results(oldNifc).map(r=>r.id),[nifc.id]);
assert(model.chips(oldNifc)[0].label.includes('NIFC'));
assert(!model.chips(oldNifc)[0].label.includes('BLM'));
assert.deepStrictEqual(model.removeChip(oldNifc,'provider','family:nifc').providers,[]);
for (const family of policy.legacy_families) {
  const old = model.createState({providers:['family:'+family.id]});
  assert.equal(model.chips(old)[0].label,family.label);
  assert.deepStrictEqual(model.removeChip(old,'provider','family:'+family.id).providers,[]);
  assert(!model.providerOptions(old).some(o=>o.value==='family:'+family.id));
}

// Current canonical links must agree with the public BLM projection, including source_reference.
const blm = resources.find(r=>r.id==='resource_blm_california'); assert(blm);
const canonicalBlm = relationships.products.filter(p=>p.resource_links.some(l=>l.resource_id===blm.id));
assert.equal(blm.representedProducts.length,25);
assert.deepStrictEqual(new Set(blm.representedProducts.map(p=>p.productId)),new Set(canonicalBlm.map(p=>p.product_id)));
const requiredBlm = ['acec','federal_wilderness','wilderness_study_areas','EXT103','EXT104','tool_blm_sma_context'];
for (const id of requiredBlm) assert(blm.representedProducts.some(p=>p.productId===id));
assert.deepStrictEqual(model.results(model.createState({providers:['family:blm']})).map(r=>r.id),[blm.id]);
for (const link of blm.representedProducts) {
  const canonical = canonicalBlm.find(p=>p.product_id===link.productId);
  assert.equal(link.relationshipRole,canonical.resource_links.find(l=>l.resource_id===blm.id).relationship_role);
  if (bundle.products) {
    const product = bundle.products.find(p=>p.id===link.productId); assert(product);
    assert.equal(product.relatedResources.find(r=>r.id===blm.id).relationshipRole,link.relationshipRole);
  }
}
for (const id of ['resource_usgs_bcmv8','resource_prism_normals','resource_dwr_bulletin118_sgma_2019']) {
  const r = resources.find(r=>r.id===id); assert(r, id);
  assert(!model.providerMembership(r).includes('family:blm'), 'Shared Products do not assign BLM ownership');
}

// Minimal DOM: real renderers and handlers; layout and native activation are simulated.
const document = { activeElement: null };
class Element {
  constructor(tag, className = '', text = '') {
    this.tagName = tag.toUpperCase(); this.className = className; this.textContent = text;
    this.children = []; this.attributes = {}; this.parentNode = null; this.hidden = false;
    this.disabled = false; this.scrollTop = 0;
    this.classList = { toggle: (name,on) => { const c=new Set(this.className.split(' '));
      if(on)c.add(name);else c.delete(name);this.className=[...c].join(' '); } };
  }
  appendChild(child) { child.parentNode = this; this.children.push(child); return child; }
  setAttribute(k,v) { this.attributes[k] = String(v); }
  getAttribute(k) { return this.attributes[k] ?? null; }
  hasAttribute(k) { return k in this.attributes; }
  get offsetParent() { return this.hidden || (this.parentNode && this.parentNode.offsetParent === null) ? null : {}; }
  focus(options) { assert(this.offsetParent !== null && !this.disabled, 'Focus target is hidden/disabled');
    this.focusOptions=options; document.activeElement = this; }
  contains(other) { return this === other || this.children.some(c => c.contains(other)); }
  closest() { return this.hasAttribute('data-guide-action') ? this : this.parentNode && this.parentNode.closest(); }
  all() { return this.children.flatMap(c => [c, ...c.all()]); }
  querySelectorAll(selector) {
    if (selector === '[data-guide-focus-key]') return this.all().filter(e => e.hasAttribute('data-guide-focus-key'));
    if (selector.startsWith('button:not')) return this.all().filter(e => !e.disabled && ['BUTTON','INPUT','SELECT'].includes(e.tagName));
    throw new Error('Unhandled DOM selector: ' + selector);
  }
}
const node = (tag, cls, text) => new Element(tag, cls, text);
const button = (cls,text,action,label) => {
  const e = node('button',cls,text); e.type='button';
  if (action) e.setAttribute('data-guide-action',action);
  if (label) e.setAttribute('aria-label',label);
  return e;
};
const helpers = {node, button, resourceExplorerModel:model,
  asArray:x=>Array.isArray(x)?x:x?[x]:[], resourcesById:Object.fromEntries(resources.map(r=>[r.id,r]))};
for (const name of ['resourceFocusKey','resourceCountText','appendResourceFacetLabel','sortedCountKeys',
  'resourceFacetChoices','resourceFacetSelect','renderResourceProviders','renderResourceFacets',
  'resourceRepresentation','deliveryLabel','relationshipRoleLabel','appendResourceDetailRow','renderResourceDetail']) {
  helpers[name]=new Function(...Object.keys(helpers),'return ('+extractFunction(source,name)+')')(...Object.values(helpers));
}
const focusKey = helpers.resourceFocusKey;
const hasClass = (e,name)=>e.className.split(' ').includes(name);
for (const width of [1440,1280,390]) {
  const ui = { view:'resource-explorer', resourceExplorer:model.createState({pane:'facets',facetsOpen:true}) };
  const root = node('div'); const search=node('input');search.type='search';focusKey(search,'resource-search');
  const filterToggle=button('','Filters','resource-facets-toggle');focusKey(filterToggle,'resource-facets-toggle');
  const render = () => { root.children=[];root.appendChild(search);root.appendChild(filterToggle);
    root.appendChild(helpers.renderResourceFacets(ui.resourceExplorer,model.facetCounts(ui.resourceExplorer))); };
  const findAction = a=>root.all().find(e=>e.getAttribute('data-guide-action')===a);
  const findClass = c=>root.all().find(e=>hasClass(e,c));
  const checkbox = id=>root.all().find(e=>e.getAttribute('data-resource-provider')==='family:'+id);
  const revealed=[];
  const focus = (key,reveal)=>{ const e=root.all().find(e=>e.getAttribute('data-guide-focus-key')===key);
    assert(e,'Missing focus target '+key);e.focus({preventScroll:!reveal}); if(reveal)revealed.push(key); };
  const click = new Function('root','state','resourceExplorerModel','render','focusResourceTarget',
    'return ('+extractFunction(source,'handleRootClick')+')')(root,ui,model,render,focus);
  const change = new Function('state','resourceExplorerModel','render','focusResourceTarget',
    'return ('+extractFunction(source,'handleResourceChange')+')')(ui,model,render,focus);
  const keydown = new Function('root','state','document','resourceExplorerModel','render','focusResourceTarget',
    'return ('+extractFunction(source,'handleRootKeydown')+')')(root,ui,document,model,render,focus);
  const activate=(e,key)=>{let prevented=false;keydown({target:e,key,preventDefault(){prevented=true;}});
    assert(!prevented,'Native activation intercepted');if(e.tagName==='BUTTON')click({target:e});
    else {assert.equal(e.type,'checkbox');e.checked=!e.checked;change({target:e});}};
  render();
  const providers=findClass('brim-guide__resource-provider');
  assert.equal(providers.children[0].children[0].textContent,'Selected providers');
  assert.equal(providers.children[1].textContent,'Other providers remain in results and searchable above.');
  assert.deepStrictEqual(providers.all().filter(e=>e.tagName==='LEGEND').map(e=>e.textContent),['Federal','State']);
  assert.equal(providers.all().filter(e=>e.tagName==='FIELDSET').length,2);
  assert.equal(providers.all().filter(e=>e.type==='checkbox').length,11);
  assert(!providers.all().some(e=>['SELECT','DETAILS','SUMMARY'].includes(e.tagName)||e.hasAttribute('aria-expanded')));
  const allIds=root.all().filter(e=>e.id).map(e=>e.id);assert.equal(new Set(allIds).size,allIds.length);
  for(const cb of providers.all().filter(e=>e.type==='checkbox')) {
    assert.equal(cb.parentNode.tagName,'LABEL');assert.equal(cb.parentNode.getAttribute('for'),cb.id);
    assert(cb.getAttribute('aria-label').includes('Resource'));assert(!cb.disabled && cb.offsetParent!==null);
  }
  activate(checkbox('noaa'),' ');assert.equal(document.activeElement,checkbox('noaa'));
  assert.equal(model.chips(ui.resourceExplorer)[0].label,'NOAA');
  ui.resourceExplorer=model.setQuery(ui.resourceExplorer,'zzzzzzzzzz');render();
  assert(checkbox('noaa').checked&&!checkbox('noaa').disabled);
  click({target:findAction('resource-provider-clear')});assert.equal(document.activeElement,checkbox('blm'));
  assert.equal(ui.resourceExplorer.query,'zzzzzzzzzz');
  ui.resourceExplorer=model.createState({pane:'facets',facetsOpen:true,providers:['family:blm']});render();
  const body=findClass('brim-guide__resource-filter-body'),footer=findClass('brim-guide__resource-filter-actions');
  assert.equal(body.parentNode,footer.parentNode);assert(!body.contains(footer));
  assert.equal(footer.parentNode.children.at(-1),footer);assert(footer.contains(findAction('resource-reset')));
  assert.equal(findAction('resource-more-filters').getAttribute('aria-controls'),'brim-guide-resource-more-panel');
  activate(findAction('resource-more-filters'),'Enter');
  const select=root.all().find(e=>e.tagName==='SELECT');assert(select);
  assert.equal(document.activeElement,select);assert.equal(revealed.at(-1),'resource-more-resourceType');
  assert(findClass('brim-guide__resource-filter-body').contains(select));
  assert.equal(findClass('brim-guide__resource-more-panel').hidden,false);
  const types=Object.keys(model.facetCounts(ui.resourceExplorer).resourceTypes);assert(types.includes('organization_homepage'));
  select.value='organization_homepage';change({target:select});
  assert.equal(ui.resourceExplorer.resourceType,'organization_homepage');
  assert.deepStrictEqual(model.results(ui.resourceExplorer).map(r=>r.id),[blm.id]);
  assert(ui.resourceExplorer.moreFiltersOpen);
  activate(findAction('resource-more-filters'),' ');assert(!ui.resourceExplorer.moreFiltersOpen);
  assert.equal(document.activeElement,findAction('resource-more-filters'));
  assert.equal(ui.resourceExplorer.resourceType,'organization_homepage');
  // Simulate native sequential Tab between real rendered controls; verify the root trap's ends.
  activate(findAction('resource-more-filters'),'Enter');
  const focusable=root.querySelectorAll('button:not([disabled])').filter(e=>e.offsetParent!==null);
  for(let i=0;i<focusable.length;i++) {
    focusable[i].focus();let prevented=false;
    keydown({target:focusable[i],key:'Tab',shiftKey:false,preventDefault(){prevented=true;}});
    if(i===focusable.length-1){assert(prevented);assert.equal(document.activeElement,search);}
    else {assert(!prevented);focusable[i+1].focus();}
  }
  assert.equal(focusable.at(-1),findAction('resource-reset'));
  search.focus();let wrapped=false;keydown({target:search,key:'Tab',shiftKey:true,preventDefault(){wrapped=true;}});
  assert(wrapped);assert.equal(document.activeElement,findAction('resource-reset'));
  click({target:findAction('resource-reset')});assert.equal(document.activeElement,search);
  assert.deepStrictEqual(ui.resourceExplorer.providers,[]);assert.equal(ui.resourceExplorer.resourceType,'');
}
// Actual scroll-state functions select the body on wide layouts and main on narrow layouts.
// Mock scroll values exercise ownership and restoration, not CSS geometry.
for (const width of [1440,1280,390]) {
  const ui={resourceExplorer:model.createState()};
  const body={scrollTop:121},results={scrollTop:242},detail={scrollTop:363};
  const pane={value:'facets'};
  const explorer={getAttribute:()=>pane.value,querySelector:selector=>({
    '.brim-guide__resource-filter-body':body,'.brim-guide__resource-results-scroll':results,
    '.brim-guide__resource-detail':detail
  })[selector]};
  const main={scrollTop:484,querySelector:()=>explorer};
  const win={matchMedia:()=>({matches:width>=1101})};
  const capture=new Function('main','state','window','return ('+extractFunction(source,'captureResourceScrollPositions')+')')(main,ui,win);
  const restore=new Function('main','state','window','return ('+extractFunction(source,'restoreResourceScrollPositions')+')')(main,ui,win);
  capture();assert.equal(ui.resourceExplorer.facetScrollTop,width>=1101?121:484);
  if(width>=1101){assert.equal(ui.resourceExplorer.resultsScrollTop,242);assert.equal(ui.resourceExplorer.detailScrollTop,363);}
  body.scrollTop=results.scrollTop=detail.scrollTop=main.scrollTop=0;restore();
  assert.equal(width>=1101?body.scrollTop:main.scrollTop,width>=1101?121:484);
  if(width<1101){pane.value='results';main.scrollTop=55;capture();assert.equal(ui.resourceExplorer.resultsScrollTop,55);}
}
// Explicit reveal uses native focus scrolling then captures it before scheduled restoration.
{
  const target=node('select');focusKey(target,'resource-more-resourceType');
  const root=node('div');root.appendChild(target);const events=[];
  const win={setTimeout:fn=>fn()};
  const focus=new Function('window','root','state','searchInput','captureResourceScrollPositions',
    'restoreResourceScrollAfterFocus','focusMain','return ('+extractFunction(source,'focusResourceTarget')+')')(
    win,root,{view:'resource-explorer'},node('input'),()=>events.push('capture'),()=>events.push('restore'),()=>{});
  focus('resource-more-resourceType',true);assert.equal(target.focusOptions.preventScroll,false);
  assert.deepStrictEqual(events,['capture','restore']);events.length=0;
  focus('resource-more-resourceType');assert.equal(target.focusOptions.preventScroll,true);assert.deepStrictEqual(events,['restore']);
}
// Run the actual detail renderer and shared Product handler for every existing BLM relationship.
const blmDetail=helpers.renderResourceDetail(blm);
const productButtons=blmDetail.all().filter(e=>e.getAttribute('data-guide-action')==='record');
assert.deepStrictEqual(productButtons.map(e=>e.getAttribute('data-guide-record')),blm.representedProducts.map(p=>p.productId));
const products=bundle.products || blm.representedProducts.map(p=>({kind:'Product',id:p.productId,title:p.title}));
const recordsById=Object.fromEntries(products.map(p=>[p.id,p]));
for(const target of productButtons) {
  const ui={view:'resource-explorer',resourceExplorer:model.selectResource(model.createState({providers:['family:blm']}),blm.id)};
  const saved=model.snapshot(ui.resourceExplorer);const history=[];
  const open=new Function('recordsById','state','pushHistory','render','focusMain',
    'return ('+extractFunction(source,'openRecord')+')')(recordsById,ui,()=>history.push(model.snapshot(ui.resourceExplorer)),()=>{},()=>{});
  const click=new Function('root','openRecord','return ('+extractFunction(source,'handleRootClick')+')')(blmDetail,open);
  click({target});assert.equal(ui.view,'detail');assert.equal(ui.selectedId,target.getAttribute('data-guide-record'));
  assert.deepStrictEqual(model.restore(history.pop()),saved);
}
assert(blmDetail.all().some(e=>e.textContent==='Bureau of Land Management'));
assert.equal(JSON.stringify(model.detail(model.selectResource(model.createState({providers:['family:nifc']}),nifc.id)).accessPoints),JSON.stringify(nifc.accessPoints));
assert(!source.includes('resource-provider-search') && !source.includes('handleResourceInput'));
assert(!source.includes("action === 'resource-provider-group'"));
assert(!css.includes('.brim-guide__resource-provider-toggle'));
assert(css.includes('.brim-guide input:focus-visible'));
for(const match of css.matchAll(/\.brim-guide__resource-provider-options\s*\{([^}]+)\}/g)) {
  assert(!/overflow|max-height/.test(match[1]),'Nested provider scroll returned');
  assert(match[1].includes('repeat(auto-fit, minmax(min(100%, 7rem), 1fr))'));
}
assert(css.includes('min-height: max(24px, 1.5rem)'));
assert(css.includes('font-size: 0.8125rem; font-weight: 400'));
assert(css.includes('scroll-padding-block: 0.75rem'));
assert(css.includes('.brim-guide__resource-filter-body {\n    flex: 1 1 auto; min-height: 0; overflow-x: hidden; overflow-y: auto'));
assert(css.includes('.brim-guide__resource-filter-actions {\n    flex: 0 0 auto;'));
const actionRules=[...css.matchAll(/\.brim-guide__resource-filter-actions\s*\{([^}]+)\}/g)];
assert(actionRules.every(m=>!/position:\s*(fixed|absolute|sticky)/.test(m[1])));
assert(source.includes("existing.querySelector('.brim-guide__resource-filter-body')"));
assert(source.includes('candidates[index].focus({ preventScroll: !reveal })'));
assert(source.includes('if (reveal) captureResourceScrollPositions()'));
assert(css.slice(css.lastIndexOf('@media (max-width: 700px)')).includes('[data-resource-pane="facets"] .brim-guide__resource-presets {\n    position: static;'));
assert.equal(JSON.stringify({bundle,registry,relationships}),before,'Input bundle/attribution/URLs/relationships mutated');
console.log('SELECTED_PROVIDERS=11; GROUPS=2; BLM_PRESENT; NIFC_SEARCHABLE_LEGACY_REMOVABLE');
console.log('STATIC_ROSTER_0_1_MANY_AND_FILTERED_ZERO=PASS; COUNT_ELIGIBILITY=ABSENT');
console.log('PROVIDER_MAPPING_COUNTS_OR_AND_COMPATIBILITY=PASS; BLM_25_LINKS_AND_PRODUCT_NAVIGATION=PASS');
console.log('NO_PROVIDER_SEARCH_ORDER_PARITY=108_PASS');
console.log('MORE_FILTERS_STRUCTURE_NATIVE_ACTIVATION_FOCUS_AND_RESET=PASS');
console.log('DOM_STRUCTURE_AND_SCROLL_OWNERSHIP=PASS_1440_1280_390; GEOMETRY_NOT_TESTED; NATIVE_ACTIVATION_SIMULATED; MOUNTED_BROWSER=NOT_RUN');
console.log('INPUT_BUNDLE_AND_ATTRIBUTION_IMMUTABLE=PASS');
