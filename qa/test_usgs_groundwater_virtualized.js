const assert = require("assert");
const fs = require("fs");
const path = require("path");

const sourcePath = path.join(
  __dirname,
  "..",
  "03_functions",
  "js",
  "leaflet_usgs_groundwater_local_virtualized.js"
);
const source = fs.readFileSync(sourcePath, "utf8");
const installLayer = new Function(`return (${source}\n);`)();

function mockElement(tagName = "div", className = "") {
  const classes = new Set(String(className).split(/\s+/).filter(Boolean));
  const attributes = {};
  const element = {
    tagName: tagName.toUpperCase(),
    id: "",
    className,
    style: {},
    nodeType: 1,
    classList: {
      add: (...names) => names.forEach((name) => classes.add(name)),
      remove: (...names) => names.forEach((name) => classes.delete(name)),
      contains: (name) => classes.has(name),
      toggle: (name, active) => {
        if (active) classes.add(name);
        else classes.delete(name);
      }
    },
    setAttribute: (name, value) => {
      attributes[name] = String(value);
      if (element._mutationCallback) element._mutationCallback([]);
    },
    getAttribute: (name) => attributes[name] || null,
    removeAttribute: (name) => {
      delete attributes[name];
      if (element._mutationCallback) element._mutationCallback([]);
    },
    contains: (node) => node === element,
    closest: (selector) => (
      selector === ".pt-usgs-gw-virtual-nested-divicon" &&
      classes.has("pt-usgs-gw-virtual-nested-divicon") ?
        element :
        null
    ),
    getBoundingClientRect: () => ({
      left: 10,
      top: 10,
      right: 34,
      bottom: 34
    }),
    querySelector: () => null,
    querySelectorAll: () => [],
    getElementsByTagName: () => [],
    appendChild: () => {},
    remove: () => {},
    click: () => {}
  };
  return element;
}

function evented(target = {}) {
  target._events = {};
  target.on = function(name, handler) {
    this._events[name] = this._events[name] || [];
    this._events[name].push(handler);
    return this;
  };
  target.off = function(name, handler) {
    if (!this._events[name]) return this;
    this._events[name] = this._events[name].filter((fn) => fn !== handler);
    return this;
  };
  target.fire = function(name, details = {}) {
    const event = Object.assign({type: name, target: this}, details);
    (this._events[name] || []).slice().forEach((handler) => handler(event));
    return this;
  };
  return target;
}

function mockLayer(options = {}) {
  const layer = evented({options});
  layer.bindPopup = function(content) {
    this._popupContent = content;
    this.on("click", function() {});
    return this;
  };
  layer.bindTooltip = function(content) {
    this._tooltip = {content};
    return this;
  };
  layer.getTooltip = function() {
    return this._tooltip || null;
  };
  layer.openTooltip = function() {
    return this;
  };
  return layer;
}

function mockLayerGroup(initial = []) {
  const group = mockLayer();
  group._layers = initial.slice();
  group.addTo = function(map) {
    map.addLayer(this);
    return this;
  };
  group.addLayer = function(layer) {
    this._layers.push(layer);
    if (this._map) this._map.addLayer(layer);
    return this;
  };
  group.clearLayers = function() {
    if (this._map) {
      this._layers.slice().forEach((layer) => this._map.removeLayer(layer));
    }
    this._layers = [];
    return this;
  };
  group.eachLayer = function(callback) {
    this._layers.slice().forEach(callback);
    return this;
  };
  group.onAdd = function(map) {
    this._map = map;
    this._layers.forEach((layer) => map.addLayer(layer));
  };
  group.onRemove = function(map) {
    this._layers.slice().forEach((layer) => map.removeLayer(layer));
    this._map = null;
  };
  return group;
}

class MockTileLayer {}

global.L = {
  TileLayer: MockTileLayer,
  canvas: (options) => ({options}),
  layerGroup: (layers) => mockLayerGroup(layers),
  circleMarker: (latlng, options) => {
    const layer = mockLayer(options);
    layer._latlng = latlng;
    return layer;
  },
  marker: (latlng, options) => {
    const layer = mockLayer(options);
    layer._latlng = latlng;
    layer.onAdd = function() {
      const iconClass = (
        options &&
        options.icon &&
        options.icon.className
      ) || "";
      this._icon = mockElement(
        "div",
        `leaflet-marker-icon ${iconClass}`.trim()
      );
    };
    layer.onRemove = function() {
      this._icon = null;
    };
    return layer;
  },
  divIcon: (options) => options,
  point: (x, y) => ({x, y}),
  latLngBounds: (southWest, northEast) => ({
    southWest,
    northEast
  })
};

const styleElements = {};
const documentListeners = {};
let elementAtPoint = null;
const checkedState = {checked: false};
const controlLabel = {
  textContent: "USGS monitoring wells",
  innerText: "USGS monitoring wells",
  querySelector: () => checkedState
};

global.document = {
  head: {
    appendChild: (node) => {
      if (node.id) styleElements[node.id] = node;
    }
  },
  body: {
    appendChild: () => {}
  },
  createElement: (tag) => ({
    ...mockElement(tag)
  }),
  getElementById: (id) => styleElements[id] || null,
  querySelectorAll: (selector) => (
    selector.indexOf("leaflet-control-layers-overlays") >= 0 ?
      [controlLabel] :
      []
  ),
  getElementsByTagName: () => [],
  elementFromPoint: () => elementAtPoint,
  addEventListener: (name, handler) => {
    documentListeners[name] = documentListeners[name] || [];
    documentListeners[name].push(handler);
  },
  removeEventListener: (name, handler) => {
    documentListeners[name] = (documentListeners[name] || [])
      .filter((fn) => fn !== handler);
  }
};

class MockMutationObserver {
  constructor(callback) {
    this.callback = callback;
    this.target = null;
  }
  observe(target) {
    this.target = target;
    target._mutationCallback = this.callback;
  }
  disconnect() {
    if (this.target) this.target._mutationCallback = null;
    this.target = null;
  }
}

global.window = {
  location: {search: ""},
  performance: {
    now: () => Date.now()
  },
  requestAnimationFrame: (fn) => setTimeout(() => fn(Date.now()), 0),
  cancelAnimationFrame: (id) => clearTimeout(id),
  setTimeout,
  clearTimeout,
  URLSearchParams,
  MutationObserver: MockMutationObserver,
  getComputedStyle: (element) => element.style || {},
  BRIM_ENABLE_USGS_GW_PROFILE: false
};
global.performance = global.window.performance;

function makeBounds(west, south, east, north) {
  return {
    getWest: () => west,
    getSouth: () => south,
    getEast: () => east,
    getNorth: () => north
  };
}

function makeMap() {
  const panes = {
    pane_points: mockElement("div", "leaflet-pane leaflet-pane-points")
  };
  const map = evented({
    _layers: new Set(),
    _zoom: 11,
    _bounds: makeBounds(-121, 34, -118, 38),
    _ptMeasureInteractionActive: false
  });
  map.addLayer = function(layer) {
    if (this._layers.has(layer)) return this;
    this._layers.add(layer);
    if (layer.onAdd) layer.onAdd(this);
    this.fire("layeradd", {layer});
    return this;
  };
  map.removeLayer = function(layer) {
    if (!this._layers.has(layer)) return this;
    if (layer.onRemove) layer.onRemove(this);
    this._layers.delete(layer);
    this.fire("layerremove", {layer});
    return this;
  };
  map.hasLayer = function(layer) {
    return !!layer && this._layers.has(layer);
  };
  map.eachLayer = function(callback) {
    Array.from(this._layers).forEach(callback);
  };
  map.getZoom = function() {
    return this._zoom;
  };
  map.getMaxZoom = () => 18;
  map.getBounds = function() {
    return this._bounds;
  };
  map.project = function(latlng, zoom) {
    const scale = 256 * Math.pow(2, zoom);
    return {
      x: (latlng[1] + 180) * scale / 360,
      y: (90 - latlng[0]) * scale / 180
    };
  };
  map.unproject = function(point, zoom) {
    const scale = 256 * Math.pow(2, zoom);
    return {
      lng: point.x * 360 / scale - 180,
      lat: 90 - point.y * 180 / scale
    };
  };
  map.getContainer = () => ({
    querySelector: () => null,
    querySelectorAll: () => [],
    getElementsByTagName: () => []
  });
  map.closePopup = () => {};
  map.getPane = (name) => panes[name] || null;
  map.getPanes = () => panes;
  map.createPane = (name) => {
    panes[name] = mockElement("div", "leaflet-pane");
    return panes[name];
  };
  map.setView = function(_center, zoom) {
    this._zoom = zoom;
  };
  map.fitBounds = () => {};
  return map;
}

function waitFor(predicate, timeoutMs = 2000) {
  const started = Date.now();
  return new Promise((resolve, reject) => {
    function poll() {
      if (predicate()) {
        resolve();
        return;
      }
      if (Date.now() - started > timeoutMs) {
        reject(new Error("Timed out waiting for virtualized render"));
        return;
      }
      setTimeout(poll, 5);
    }
    poll();
  });
}

async function run() {
  const map = makeMap();
  const data = {
    groupName: "Points – USGS monitoring wells",
    records: {
      site_no: ["1", "2", "3", "4"],
      pt_lat: [35, 35, 36, 37],
      pt_lng: [-120, -120, -119, -118],
      well_radius: [3, 3, 3, 3],
      well_fill_col: ["#fff", "#fff", "#fff", "#fff"],
      well_stroke_col: ["#333", "#333", "#333", "#333"],
      well_recent_feed_ring: [false, false, true, false],
      well_is_nested: [true, true, false, false],
      hover_text: ["one", "two", "three", "four"],
      gwpop_site_no: ["1", "2", "3", "4"],
      gwpop_name: ["one", "two", "three", "four"],
      gwpop_nested_n: [2, 2, 1, 1],
      gwpop_in_ops: [false, false, true, false]
    },
    locations: {
      site_start: [0, 2, 3],
      site_count: [2, 1, 1]
    },
    metadata: {
      siteCount: 4,
      uniqueCoordinateCount: 3,
      nestedLocationCount: 1,
      nestedSiteRecordCount: 2,
      clusterToExactZoom: 11
    }
  };

  installLayer.call(map, {}, {}, data);
  checkedState.checked = true;
  map.fire("overlayadd", {name: data.groupName});

  await waitFor(() => {
    const stats = window.BRIM_USGS_GW_LOCAL.stats();
    return stats.active && !stats.loading && stats.drawn === 3;
  });

  let stats = window.BRIM_USGS_GW_LOCAL.stats();
  assert.strictEqual(stats.total, 4);
  assert.strictEqual(stats.uniqueCoordinates, 3);
  assert.strictEqual(stats.nestedLocations, 1);
  assert.strictEqual(stats.visibleSites, 4);
  assert.strictEqual(stats.exactObjects, 3);
  assert.strictEqual(stats.clusterObjects, 0);
  assert.strictEqual(stats.ordinaryWellRadius, 4.8);
  assert.strictEqual(stats.ordinaryWellStrokeWeight, 0.95);
  assert.strictEqual(stats.ordinaryWellOuterDiameter, 10.55);
  assert.strictEqual(stats.interactivePane, "pane_usgs_gw_interactive");
  assert.strictEqual(stats.interactivePaneZIndex, 585);

  const exactLayers = Array.from(map._layers).filter(
    (layer) => layer && layer._latlng
  );
  const nestedLayer = exactLayers.find(
    (layer) => (
      layer.options &&
      layer.options.icon &&
      layer.options.icon.className === "pt-usgs-gw-virtual-nested-divicon"
    )
  );
  const singleLayer = exactLayers.find(
    (layer) => layer.options && layer.options.renderer
  );
  assert.ok(nestedLayer, "nested/co-located display marker was not created");
  assert.ok(singleLayer, "Canvas exact-point layer was not created");
  assert.strictEqual(nestedLayer.options.pane, "pane_usgs_gw_interactive");
  assert.strictEqual(
    map.getPane("pane_usgs_gw_interactive").style.zIndex,
    "585"
  );
  assert.strictEqual(
    map.getPane("pane_usgs_gw_interactive").style.pointerEvents,
    "auto"
  );
  assert.strictEqual(
    nestedLayer.getTooltip(),
    null,
    "nested tooltip should be lazy"
  );
  assert.strictEqual(
    singleLayer.getTooltip(),
    null,
    "single-site tooltip should be lazy"
  );
  map._ptMeasureInteractionActive = true;
  singleLayer.fire("mouseover");
  assert.strictEqual(
    singleLayer.getTooltip(),
    null,
    "Measure mode should suppress groundwater hover"
  );
  map._ptMeasureInteractionActive = false;
  singleLayer.fire("mouseover");
  assert.ok(singleLayer.getTooltip(), "single-site tooltip did not bind on hover");
  assert.ok(
    String(singleLayer._popupContent()).includes(
      "https://waterdata.usgs.gov/monitoring-location/"
    ),
    "single-site popup lost the USGS source URL"
  );
  const nestedPopup = String(nestedLayer._popupContent());
  assert.ok(
    nestedPopup.includes("/monitoring-location/1") &&
      nestedPopup.includes("/monitoring-location/2"),
    "nested popup lost access to individual USGS records"
  );

  const debugApi = window.BRIM_USGS_GW_DEBUG;
  assert.ok(debugApi, "quiet groundwater diagnostic was not installed");
  const initialNestedDiagnostics = debugApi.nestedLayers();
  assert.strictEqual(initialNestedDiagnostics.length, 1);
  assert.strictEqual(initialNestedDiagnostics[0].siteCount, 2);
  assert.strictEqual(initialNestedDiagnostics[0].mounted, true);
  assert.strictEqual(initialNestedDiagnostics[0].interactionReady, true);
  assert.strictEqual(initialNestedDiagnostics[0].handlers.mouseover, 1);
  assert.strictEqual(initialNestedDiagnostics[0].handlers.click, 1);
  elementAtPoint = nestedLayer._icon;
  const cursorDiagnostic = debugApi.inspectNestedAtCursor(20, 20);
  assert.strictEqual(cursorDiagnostic.found, true);
  assert.strictEqual(cursorDiagnostic.locationId, "-120.0000000_35.0000000");
  assert.strictEqual(cursorDiagnostic.icon.hitByElementFromPoint, true);
  assert.strictEqual(cursorDiagnostic.pane.pointerEvents, "auto");
  assert.strictEqual(cursorDiagnostic.handlers.mouseover, 1);
  assert.strictEqual(cursorDiagnostic.handlers.click, 1);
  elementAtPoint = mockElement("canvas", "leaflet-zoom-animated");
  const blockedCursorDiagnostic = debugApi.inspectNestedAtCursor(20, 20);
  assert.strictEqual(blockedCursorDiagnostic.found, true);
  assert.strictEqual(blockedCursorDiagnostic.icon.hitByElementFromPoint, false);
  assert.strictEqual(
    blockedCursorDiagnostic.elementFromPoint.tag,
    "canvas",
    "diagnostic did not identify an element intercepting the nested marker"
  );
  elementAtPoint = nestedLayer._icon;

  map._ptMeasureInteractionActive = true;
  map.getPane("pane_usgs_gw_interactive").setAttribute(
    "data-pt-measure-suspended",
    "true"
  );
  assert.strictEqual(
    map.getPane("pane_usgs_gw_interactive").style.pointerEvents,
    "none",
    "Measure activation did not suspend the dedicated interactive pane"
  );
  map._ptMeasureInteractionActive = false;
  map.getPane("pane_usgs_gw_interactive").removeAttribute(
    "data-pt-measure-suspended"
  );
  assert.strictEqual(
    map.getPane("pane_usgs_gw_interactive").style.pointerEvents,
    "auto",
    "Measure deactivation did not restore the dedicated interactive pane"
  );

  window.BRIM_USGS_GW_LOCAL.applyFilters({opsOnly: true});
  await waitFor(() => {
    const current = window.BRIM_USGS_GW_LOCAL.stats();
    return !current.loading && current.filtered === 1 && current.drawn === 1;
  });
  stats = window.BRIM_USGS_GW_LOCAL.stats();
  assert.strictEqual(stats.visibleSites, 1);
  assert.strictEqual(stats.visibleLocations, 1);

  window.BRIM_USGS_GW_LOCAL.resetFilters();
  await waitFor(() => {
    const current = window.BRIM_USGS_GW_LOCAL.stats();
    return !current.loading && current.filtered === 4 && current.drawn === 3;
  });

  checkedState.checked = false;
  map.fire("overlayremove", {name: data.groupName});
  stats = window.BRIM_USGS_GW_LOCAL.stats();
  assert.strictEqual(stats.active, false);
  assert.strictEqual(stats.displayCacheReady, true);

  checkedState.checked = true;
  map.fire("overlayadd", {name: data.groupName});
  await waitFor(() => {
    const nested = debugApi.nestedLayers();
    return (
      !window.BRIM_USGS_GW_LOCAL.stats().loading &&
      nested.length === 1 &&
      nested[0].mounted
    );
  });
  assert.strictEqual(
    window.BRIM_USGS_GW_LOCAL.stats().displayCacheReady,
    true
  );
  const restoredNestedDiagnostics = debugApi.nestedLayers();
  assert.strictEqual(restoredNestedDiagnostics.length, 1);
  assert.strictEqual(restoredNestedDiagnostics[0].mounted, true);
  assert.strictEqual(
    restoredNestedDiagnostics[0].handlers.mouseover,
    initialNestedDiagnostics[0].handlers.mouseover,
    "off/on duplicated the nested hover handler"
  );
  assert.strictEqual(
    restoredNestedDiagnostics[0].handlers.click,
    initialNestedDiagnostics[0].handlers.click,
    "off/on duplicated the nested click handler"
  );

  window.BRIM_USGS_GW_LOCAL.clear();
  assert.strictEqual(debugApi.nestedLayers().length, 0);
  checkedState.checked = true;
  map.fire("overlayadd", {name: data.groupName});
  await waitFor(() => {
    const nested = debugApi.nestedLayers();
    return (
      !window.BRIM_USGS_GW_LOCAL.stats().loading &&
      nested.length === 1 &&
      nested[0].mounted
    );
  });
  const clearRestoredNested = debugApi.nestedLayers()[0];
  assert.strictEqual(clearRestoredNested.interactionReady, true);
  assert.strictEqual(clearRestoredNested.handlers.mouseover, 1);
  assert.strictEqual(clearRestoredNested.handlers.click, 1);

  map._zoom = 6;
  map.fire("zoomstart");
  map.fire("movestart");
  map.fire("zoomend");
  map.fire("moveend");
  await waitFor(() => {
    const current = window.BRIM_USGS_GW_LOCAL.stats();
    return !current.loading && current.clusterObjects > 0;
  });
  stats = window.BRIM_USGS_GW_LOCAL.stats();
  assert.ok(stats.leafletDisplayObjects < stats.uniqueCoordinates);
  assert.strictEqual(stats.visibleSites, 4);
  const clusterLayer = Array.from(map._layers).find(
    (layer) => (
      layer &&
      layer.options &&
      layer.options.icon &&
      layer.options.icon.className === "pt-usgs-gw-grid-cluster-divicon"
    )
  );
  assert.ok(clusterLayer, "low-zoom aggregate marker was not created");
  assert.strictEqual(clusterLayer.options.pane, "pane_usgs_gw_interactive");

  map._zoom = 11;
  map.fire("zoomstart");
  map.fire("movestart");
  map.fire("zoomend");
  map.fire("moveend");
  await waitFor(() => {
    const current = window.BRIM_USGS_GW_LOCAL.stats();
    return !current.loading && current.exactObjects === 3;
  });
  assert.strictEqual(debugApi.nestedLayers().length, 1);
  assert.strictEqual(debugApi.nestedLayers()[0].interactionReady, true);

  map.fire("movestart");
  assert.strictEqual(
    debugApi.nestedLayers()[0].mounted,
    false,
    "plain pan did not detach the exact display before movement"
  );
  map.fire("moveend");
  await waitFor(() => (
    debugApi.nestedLayers().length === 1 &&
    debugApi.nestedLayers()[0].mounted
  ));

  map._zoom = 6;
  map.fire("zoomstart");
  map.fire("movestart");
  map.fire("zoomend");
  map.fire("moveend");
  await waitFor(() => (
    !window.BRIM_USGS_GW_LOCAL.stats().loading &&
    window.BRIM_USGS_GW_LOCAL.stats().clusterObjects > 0
  ));
  map.fire("pt:marqueezoomstart");
  map._zoom = 11;
  map.fire("movestart");
  map.fire("pt:marqueezoomend");
  map.fire("moveend");
  await waitFor(() => (
    !window.BRIM_USGS_GW_LOCAL.stats().loading &&
    window.BRIM_USGS_GW_LOCAL.stats().exactObjects === 3
  ));
  assert.strictEqual(debugApi.nestedLayers()[0].interactionReady, true);

  window.BRIM_USGS_GW_LOCAL.clear();
  stats = window.BRIM_USGS_GW_LOCAL.stats();
  assert.strictEqual(stats.active, false);
  assert.strictEqual(stats.displayCacheReady, false);

  checkedState.checked = true;
  map.fire("overlayadd", {name: data.groupName});
  window.BRIM_USGS_GW_LOCAL.clear();
  await new Promise((resolve) => setTimeout(resolve, 100));
  stats = window.BRIM_USGS_GW_LOCAL.stats();
  assert.strictEqual(stats.active, false);
  assert.strictEqual(stats.drawn, 0);
  assert.strictEqual(
    Array.from(map._layers).filter((layer) => layer && layer._latlng).length,
    0,
    "a cancelled render recreated stale groundwater geometry"
  );

  window.BRIM_USGS_GW_LOCAL.destroy();
  assert.strictEqual(window.BRIM_USGS_GW_DEBUG, undefined);
  assert.ok(
    Object.values(map._events).every((handlers) => handlers.length === 0),
    "destroy left duplicate-able map listeners attached"
  );

  const classMap = makeMap();
  classMap._bounds = makeBounds(-121, 34, -118, 38);
  const classValues = [-1, 10, 50, 150, 350, 750, 1200, null];
  const classFills = [
    "#756BB1",
    "#2B8CBE",
    "#7BCCC4",
    "#A1D99B",
    "#FEE391",
    "#FEC44F",
    "#BD0026",
    "#F2F2F2"
  ];
  const classData = {
    groupName: data.groupName,
    records: {
      site_no: classValues.map((_, index) => String(index + 10)),
      pt_lat: classValues.map((_, index) => 35 + index * 0.1),
      pt_lng: classValues.map((_, index) => -120 + index * 0.1),
      gwpop_latest_wl: classValues,
      well_radius: [4.8, 4.8, 4.8, 5.2, 5.8, 6.3, 2.6, 3],
      well_stroke_weight_display: [0.8, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.2],
      well_fill_col: classFills,
      well_stroke_col: classFills.map(() => "#333333"),
      well_recent_feed_ring: classValues.map((_, index) => index === 5),
      gwpop_in_ops: classValues.map((_, index) => index === 5),
      hover_text: classValues.map((_, index) => `class ${index}`)
    },
    locations: {
      site_start: classValues.map((_, index) => index),
      site_count: classValues.map(() => 1)
    },
    metadata: {
      siteCount: 8,
      uniqueCoordinateCount: 8,
      nestedLocationCount: 0,
      nestedSiteRecordCount: 0,
      clusterToExactZoom: 11
    }
  };

  installLayer.call(classMap, {}, {}, classData);
  checkedState.checked = true;
  classMap.fire("overlayadd", {name: classData.groupName});
  await waitFor(() => {
    const current = window.BRIM_USGS_GW_LOCAL.stats();
    return !current.loading && current.drawn === 8;
  });
  const classLayers = Array.from(classMap._layers).filter(
    (layer) => layer && layer.options && layer.options.renderer
  );
  assert.strictEqual(classLayers.length, 8);
  assert.deepStrictEqual(
    classLayers.map((layer) => layer.options.radius),
    classValues.map(() => 4.8),
    "ordinary well radius varied by water-level class"
  );
  assert.deepStrictEqual(
    classLayers.map((layer) => layer.options.weight),
    classValues.map(() => 0.95),
    "ordinary well stroke varied by water-level class or Ops membership"
  );
  assert.deepStrictEqual(
    classLayers.map((layer) => layer.options.fillColor),
    classFills,
    "water-level class colors changed while standardizing geometry"
  );
  assert.strictEqual(
    classLayers[5].options.color,
    "#ff00cc",
    "Ops Live color cue was lost"
  );
  window.BRIM_USGS_GW_LOCAL.destroy();

  const helperSource = fs.readFileSync(
    path.join(
      __dirname,
      "..",
      "03_functions",
      "leaflet_layer_local_usgs_helpers.r"
    ),
    "utf8"
  );
  const legendStart = helperSource.indexOf(
    "Most recent cached groundwater level"
  );
  const legendEnd = helperSource.indexOf("Catalog cues", legendStart);
  const legendBlock = helperSource.slice(legendStart, legendEnd);
  assert.strictEqual(
    (legendBlock.match(/row\(dot\(/g) || []).length,
    8,
    "the eight ordinary groundwater legend classes do not share dot()"
  );
  assert.ok(
    /\.pt-usgs-gw-local-dot\{width:9px;height:9px;border:1\.3px/.test(
      helperSource
    ),
    "legend dots no longer share one explicit outer geometry"
  );
  console.log("USGS groundwater virtualized fixture: OK");
}

run().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
