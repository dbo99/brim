// Source/offline reproduction: no browser, HTML build or provider access.
// Pass the installed Leaflet JavaScript file as the sole argument. Its identity
// is reported; no machine-specific path is embedded in this test.
// Real Leaflet methods own Canvas registration, path removal and hit testing.
// Painting and DOM targeting are modeled. Comparison polygons are event
// sentinels, not full Bulletin 118/Federal Wilderness controllers.
const assert = require('assert');
const fs = require('fs');
const path = require('path');
const vm = require('vm');
const crypto = require('crypto');
const fixture = require('./test_springs_virtualized');
const {installLayer, mockElement, mockLayer, makeMap, waitFor,
  checkedState, documentListeners} = fixture;
const read = (p) => fs.readFileSync(path.join(__dirname, '..', p), 'utf8');
const leafletBytes = fs.readFileSync(process.argv[2]);
const domElement = (tag) => Object.assign(mockElement(tag), {getContext: () => ({})});
const leafletContext = {
  window: {screen: {deviceXDPI: 1, logicalXDPI: 1}, devicePixelRatio: 1,
    addEventListener() {}, setTimeout, clearTimeout},
  document: {documentElement: domElement('html'), createElement: domElement,
    createElementNS: (_ns, tag) => domElement(tag), addEventListener() {}},
  navigator: {userAgent: 'node offline fixture', platform: 'Mac'},
  setTimeout, clearTimeout, console
};
vm.createContext(leafletContext);
vm.runInContext(leafletBytes.toString('utf8'), leafletContext);
const realL = leafletContext.L || leafletContext.window.L;
console.log('LEAFLET=' + JSON.stringify({version: realL.version,
  bytes: leafletBytes.length,
  sha256: crypto.createHash('sha256').update(leafletBytes).digest('hex')}));

const rSource = read('03_functions/leaflet_layer_local_well_spring_helpers.r');
const transition = rSource.match(/clusterToExactZoom\s*=\s*(\d+)L/);
assert.ok(transition, 'Current Springs R payload must declare the transition');
const exactZoom = Number(transition[1]);
const paneSource = read('03_functions/leaflet_core_helpers.r');
const paneZ = {};
for (const match of paneSource.matchAll(/addMapPane\("([^"]+)",\s*zIndex\s*=\s*(\d+)/g)) {
  paneZ[match[1]] = Number(match[2]);
}
assert.ok(paneZ.pane_points > paneZ.pane_gw);
assert.ok(paneZ.pane_points > paneZ.pane_lines);

// Preserve the actual renderer lifecycle omitted by the older small fixture.
L.canvas = (options) => {
  const renderer = realL.canvas(options);
  assert.ok(renderer, 'Installed Leaflet Canvas support required');
  renderer._requestRedraw = () => {}; // no painting; keep actual path list
  renderer.onAdd = function(map) {
    this._map = map;
    this._layers = {};
    this._initContainer(); // real Canvas DOM event registration
  };
  renderer.onRemove = function() {
    this._destroyContainer();
    this._map = null;
  };
  return renderer;
};
L.circleMarker = (latlng, options) => {
  const layer = mockLayer(options);
  layer._latlng = latlng;
  layer.beforeAdd = realL.Path.prototype.beforeAdd;
  layer.onAdd = function(map) {
    this._map = map;
    this.beforeAdd(map);
    this._renderer._initPath(this);
  };
  layer.onRemove = realL.Path.prototype.onRemove;
  layer._containsPoint = (point) => point.springId === layer._ptSpringRecordId;
  return layer;
};

const rows = [];
function check(name, condition, observed) {
  rows.push({name, pass: !!condition, observed});
  console.log((condition ? 'PASS ' : 'FAIL ') + name +
    (observed === undefined ? '' : ' ' + JSON.stringify(observed)));
}

async function scenario(order) {
  const map = makeMap();
  const container = mockElement('div');
  const captures = [];
  container.classList.toggle = (name, enabled) => {
    container.classList[enabled ? 'add' : 'remove'](name);
  };
  container.addEventListener = (name, handler, capture) => {
    captures.push({name, handler, capture});
  };
  container.removeEventListener = (name, handler, capture) => {
    const index = captures.findIndex((r) => r.name === name &&
      r.handler === handler && r.capture === capture);
    if (index >= 0) captures.splice(index, 1);
  };
  container.contains = (target) => target === container;
  map.getContainer = () => container;
  const addLayer = map.addLayer;
  map.addLayer = function(layer) {
    const present = this.hasLayer(layer);
    addLayer.call(this, layer);
    // Leaflet fires add after onAdd; renderer containers exist at this point.
    if (!present && layer.fire) layer.fire('add');
    return this;
  };
  map.getRenderer = realL.Map.prototype.getRenderer;
  map._draggableMoved = () => false;
  map.dragging = {moving: () => false};
  map.mouseEventToLayerPoint = (event) => event.point;
  map._fireDOMEvent = (event, type, targets) => {
    targets.forEach((layer) => layer.fire(type, {originalEvent: event}));
  };
  const data = {
    groupName: 'Points – Springs',
    records: {spring_id: ['near_1', 'near_2'],
      pt_lat: [35, 35.00001], pt_lng: [-120, -120.00001],
      spring_name_display: ['Near one', 'Near two'],
      spring_source_key: ['nhd', 'nhd']},
    locations: {record_start: [0, 1], record_count: [1, 1]},
    metadata: {recordCount: 2, uniqueCoordinateCount: 2,
      clusterToExactZoom: exactZoom}
  };
  map._zoom = 6; // selected coarse fixture view; exact transition comes from R
  const foreign = {};
  const paneIdentities = {};
  for (const [name, pane] of [['Bulletin 118', 'pane_gw'], ['Federal Wilderness', 'pane_lines']]) {
    const layer = mockLayer({pane});
    layer.hovers = 0;
    layer.clicks = 0;
    layer.on('mouseover', () => { layer.hovers += 1; });
    layer.on('click', () => { layer.clicks += 1; });
    layer.savedHandler = layer._events.mouseover[0];
    layer.savedOptions = JSON.stringify(layer.options);
    foreign[name] = layer;
    paneIdentities[pane] = map.createPane(pane);
    paneIdentities[pane].style.zIndex = String(paneZ[pane]);
  }
  const foreignOn = () => Object.values(foreign).forEach((layer) => map.addLayer(layer));
  const springsOn = () => {
    checkedState.checked = true;
    installLayer.call(map, mockElement('div'), {}, data);
    map.fire('overlayadd', {name: data.groupName});
  };
  if (order === 'comparison-first') { foreignOn(); springsOn(); }
  else { springsOn(); foreignOn(); }
  const api = window.BRIM_SPRINGS_LOCAL;
  const settled = () => !api.stats().loading && api.stats().drawn > 0;
  await waitFor(settled);
  check(order + ': coarse fixture is aggregate-only',
    api.stats().exactObjects === 0 && api.stats().aggregateObjects === 1,
    {exact: api.stats().exactObjects, aggregate: api.stats().aggregateObjects});
  // Model ordinary rectangular Canvas DOM targeting. Transparent pixels do
  // not retarget an event to a lower sibling pane. This is NOT a browser test.
  function routeMove(name, springId, type = 'mousemove', target = container) {
    const event = {type, target, point: {springId}, stopped: false,
      stopPropagation() { this.stopped = true; },
      stopImmediatePropagation() { this.stopped = true; }};
    for (const record of captures.slice()) {
      if (record.capture && record.name === type) record.handler(event);
      if (event.stopped) return event;
    }
    if (target !== container) return event; // UI / DOM marker native target
    const targetZ = name ? paneZ[foreign[name].options.pane] : -Infinity;
    const above = Array.from(map._layers).filter((layer) =>
      layer instanceof realL.Canvas && paneZ[layer.options.pane] > targetZ &&
      layer._container && layer._container.style.pointerEvents !== 'none' &&
      map.getPane(layer.options.pane).style.pointerEvents !== 'none');
    above.sort((a, b) => paneZ[b.options.pane] - paneZ[a.options.pane]);
    if (above.length) {
      above[0][type === 'click' ? '_onClick' : '_onMouseMove'](event);
    } else if (name) {
      foreign[name].fire(type === 'click' ? 'click' : 'mouseover', {originalEvent: event});
    }
    return event;
  }
  function foreignHover(stage) {
    for (const name of Object.keys(foreign)) {
      const before = foreign[name].hovers;
      routeMove(name);
      check(order + ': ' + stage + ' / ' + name,
        foreign[name].hovers === before + 1);
    }
  }
  foreignHover('hover before declustering');
  map.fire('zoomstart');
  map._zoom = exactZoom;
  map.fire('zoomend');
  await waitFor(() => settled() && api.stats().exactObjects === 2);
  console.log('TRANSITION=' + JSON.stringify({order, from: 6, to: exactZoom,
    exact: api.stats().exactObjects, aggregate: api.stats().aggregateObjects}));
  const points = Array.from(map._layers).filter((layer) => layer._ptSpringRecordId);
  check(order + ': real Canvas mounts on individual features', points.length === 2 &&
    map.hasLayer(points[0]._renderer));
  const renderer = points[0]._renderer;
  let springClicks = 0;
  points.forEach((p) => p.on('click', () => { springClicks += 1; }));
  routeMove(null, 'near_1');
  check(order + ': Springs hover after declustering', !!points.find((p) =>
    p._ptSpringRecordId === 'near_1').getTooltip());
  foreignHover('hover after Springs individual hover');
  check(order + ': only private Canvas bypasses rectangular DOM targeting',
    renderer._container.style.pointerEvents === 'none' &&
    map.getPane('pane_points').style.pointerEvents === undefined);
  for (const name of Object.keys(foreign)) {
    const hovers = foreign[name].hovers;
    const clicks = foreign[name].clicks;
    const ownClicks = springClicks;
    const move = routeMove(name, 'near_1');
    const click = routeMove(name, 'near_1', 'click');
    check(order + ': true Springs hit owns hover/click without lower duplication / ' + name,
      move.stopped && click.stopped && foreign[name].hovers === hovers &&
      foreign[name].clicks === clicks && springClicks === ownClicks + 1);
    routeMove(name, undefined, 'click');
    check(order + ': transparent pixel delivers lower click once / ' + name,
      foreign[name].clicks === clicks + 1 && springClicks === ownClicks + 1);
  }
  routeMove(null, 'near_1');
  check(order + ': Springs hit cursor', container.classList.contains('pt-springs-canvas-hit'));
  routeMove('Bulletin 118');
  check(order + ': transparent pixel clears Springs hover/cursor',
    !renderer._hoveredLayer && !container.classList.contains('pt-springs-canvas-hit'));
  for (const selector of ['.leaflet-control', '.leaflet-popup',
    '.leaflet-tooltip', '.pt-springs-aggregate-divicon', '.pt-springs-multi-divicon']) {
    routeMove(null, 'near_1');
    const clicks = springClicks;
    const target = {closest: (selectors) => selectors.includes(selector) ? {} : null};
    routeMove(null, 'near_1', 'mousemove', target);
    const event = routeMove(null, 'near_1', 'click', target);
    check(order + ': preserves native UI/DOM marker ownership / ' + selector,
      !event.stopped && !renderer._hoveredLayer && springClicks === clicks);
  }
  routeMove(null, 'near_1');
  map._ptMeasureInteractionActive = true;
  const clicksBeforeMeasure = springClicks;
  routeMove(null, 'near_1');
  const measureClick = routeMove(null, 'near_1', 'click');
  check(order + ': measurement suppresses Springs forwarding',
    !renderer._hoveredLayer && !measureClick.stopped && springClicks === clicksBeforeMeasure);
  map._ptMeasureInteractionActive = false;
  routeMove(null, 'near_1');
  check(order + ': hover restores after measurement', !!renderer._hoveredLayer);
  for (const r of captures.filter((r) => r.name === 'mouseout')) {
    r.handler({type: 'mouseout', relatedTarget: null});
  }
  check(order + ': leaving map clears own hover', !renderer._hoveredLayer);
  for (const name of Object.keys(foreign)) {
    const layer = foreign[name];
    check(order + ': handler and style unchanged / ' + name,
      layer._events.mouseover.length === 1 &&
      layer._events.mouseover[0] === layer.savedHandler &&
      JSON.stringify(layer.options) === layer.savedOptions);
    check(order + ': pane unchanged / ' + name,
      map.getPane(layer.options.pane) === paneIdentities[layer.options.pane] &&
      paneIdentities[layer.options.pane].style.pointerEvents === undefined);
  }
  checkedState.checked = false;
  map.fire('overlayremove', {name: data.groupName});
  check(order + ': layer-off removes own renderer', !map.hasLayer(renderer),
    {rendererMounted: map.hasLayer(renderer), remainingPaths: Object.keys(renderer._layers).length});
  foreignHover('hover after layer-off');
  checkedState.checked = true;
  api.setActive(true);
  await waitFor(() => settled() && renderer._drawFirst);
  routeMove(null, 'near_1');
  check(order + ': own hover survives reactivation', !!renderer._hoveredLayer);
  map.fire('zoomstart');
  check(order + ': zoom suspension detaches Canvas', !map.hasLayer(renderer));
  map._zoom = 6;
  map.fire('zoomend');
  await waitFor(() => settled() && api.stats().aggregateObjects === 1);
  check(order + ': return to aggregate-only state leaves no Canvas', !map.hasLayer(renderer));
  foreignHover('hover after return to aggregate-only state');
  map.fire('zoomstart');
  map._zoom = exactZoom;
  map.fire('zoomend');
  await waitFor(() => settled() && renderer._drawFirst);
  routeMove(null, 'near_1');
  check(order + ': repeated declustering restores styled renderer and hover',
    renderer._container.style.pointerEvents === 'none' && !!renderer._hoveredLayer);
  check(order + ': transitions do not duplicate capture listeners', captures.length === 3);
  for (const target of [
    {id: '', className: 'pt-main-layer-clear-btn'},
    {id: 'pt-clear-all-btn', className: ''}
  ]) {
    api.setActive(true);
    await waitFor(() => settled() && renderer._drawFirst);
    target.closest = () => target;
    (documentListeners.click || []).slice().forEach((fn) => fn({target}));
    check(order + ': Springs clear contribution removes own renderer / ' +
      (target.id || 'Clear Local'), !map.hasLayer(renderer));
  }
  api.destroy();
  check(order + ': destroy removes own renderer', !map.hasLayer(renderer));
  // Unlike the diagnostic RED, the repaired controller must restore foreign
  // delivery itself: no test-side renderer removal is performed.
  foreignHover('hover after destroy without test-side cleanup');
  check(order + ': destroy removes capture and renderer-add listeners',
    captures.length === 0 && !renderer.listens('add'));
  check(order + ': destroy preserves foreign layers', Object.values(foreign).every(
    (layer) => map.hasLayer(layer) && layer._events.mouseover[0] === layer.savedHandler));
}

(async () => {
  await scenario('comparison-first');
  await scenario('springs-first');
  const passed = rows.filter((row) => row.pass).length;
  const failed = rows.length - passed;
  console.log('OWNERSHIP_RESULT=' + JSON.stringify({passed, failed,
    limits: 'Real Leaflet path/Canvas lifecycle and hit methods; modeled DOM targeting; no full comparison controller/browser'}));
  process.exitCode = failed ? 1 : 0;
})().catch((error) => { console.error(error); process.exitCode = 2; });
