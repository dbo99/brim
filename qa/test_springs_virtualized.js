const assert = require("assert");
const fs = require("fs");
const path = require("path");

const sourcePath = path.join(
  __dirname,
  "..",
  "03_functions",
  "js",
  "leaflet_springs_local_virtualized.js"
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
      contains: (name) => classes.has(name)
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
    querySelector: () => null,
    querySelectorAll: () => [],
    appendChild: () => {},
    remove: () => {},
    addEventListener: () => {},
    removeEventListener: () => {}
  };
  return element;
}

function evented(target = {}) {
  target._events = {};
  target.on = function(name, handler) {
    String(name).split(/\s+/).forEach((eventName) => {
      this._events[eventName] = this._events[eventName] || [];
      this._events[eventName].push(handler);
    });
    return this;
  };
  target.off = function(name, handler) {
    String(name).split(/\s+/).forEach((eventName) => {
      this._events[eventName] = (this._events[eventName] || [])
        .filter((fn) => fn !== handler);
    });
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

global.L = {
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
    return layer;
  },
  divIcon: (options) => options,
  point: (x, y) => ({x, y}),
  latLngBounds: (southWest, northEast) => ({southWest, northEast})
};

const styleElements = {};
const documentListeners = {};
const checkedState = {checked: false};
const labelCheckedState = {checked: false};

function controlLabel(text, checked) {
  return {
    textContent: text,
    innerText: text,
    getAttribute: () => "",
    querySelector: () => checked
  };
}

const controlLabels = [
  controlLabel("Springs", checkedState),
  controlLabel("Labels – Springs", labelCheckedState)
];

global.document = {
  head: {
    appendChild: (node) => {
      if (node.id) styleElements[node.id] = node;
    }
  },
  body: {
    appendChild: () => {}
  },
  createElement: (tag) => mockElement(tag),
  getElementById: (id) => styleElements[id] || null,
  querySelectorAll: (selector) => (
    selector.includes("leaflet-control-layers-overlays") ?
      controlLabels :
      []
  ),
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
  performance: {now: () => Date.now()},
  requestAnimationFrame: (fn) => setTimeout(() => fn(Date.now()), 0),
  cancelAnimationFrame: (id) => clearTimeout(id),
  setTimeout,
  clearTimeout,
  URLSearchParams,
  MutationObserver: MockMutationObserver,
  BRIM_ENABLE_SPRINGS_PROFILE: false
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
    pane_points: mockElement("div", "leaflet-pane"),
    pane_labels_pts: mockElement("div", "leaflet-pane")
  };
  const map = evented({
    _layers: new Set(),
    _zoom: 11,
    _bounds: makeBounds(-121, 34, -117, 38),
    _ptMeasureInteractionActive: false
  });
  map.addLayer = function(layer) {
    if (this._layers.has(layer)) return this;
    this._layers.add(layer);
    if (layer.onAdd) layer.onAdd(this);
    return this;
  };
  map.removeLayer = function(layer) {
    if (!this._layers.has(layer)) return this;
    if (layer.onRemove) layer.onRemove(this);
    this._layers.delete(layer);
    return this;
  };
  map.hasLayer = function(layer) {
    return !!layer && this._layers.has(layer);
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
    querySelectorAll: () => []
  });
  map.closePopup = () => {};
  map.getPane = (name) => panes[name] || null;
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

function waitFor(predicate, timeoutMs = 2500) {
  const started = Date.now();
  return new Promise((resolve, reject) => {
    function poll() {
      if (predicate()) {
        resolve();
        return;
      }
      if (Date.now() - started > timeoutMs) {
        reject(new Error("Timed out waiting for Springs virtualized render"));
        return;
      }
      setTimeout(poll, 5);
    }
    poll();
  });
}

function visibleFeatureLayers(map) {
  return Array.from(map._layers).filter((layer) => layer && layer._latlng);
}

async function run() {
  assert.ok(
    !source.includes("markerClusterGroup"),
    "Springs controller still uses the global MarkerCluster architecture"
  );
  assert.ok(
    !source.includes("rowsToArray"),
    "Springs controller still expands the columnar payload into row objects"
  );

  const map = makeMap();
  const data = {
    groupName: "Points – Springs",
    labelGroupName: "Labels – Springs",
    records: {
      spring_id: ["spring_1", "spring_2", "spring_3", "spring_4", "spring_5"],
      pt_lat: [35, 35, 35.00000001, 36, 37],
      pt_lng: [-120, -120, -120, -119, -118],
      spring_name_display: [
        "Alpha Spring",
        "Alternate Alpha",
        "Near Alpha",
        "Survey Spring",
        "Far Spring"
      ],
      spring_source_key: [
        "nhd",
        "nhd",
        "nhd",
        "survey_2015_16",
        "nhd"
      ],
      spring_source_display: [
        "NHD",
        "NHD",
        "NHD",
        "2015–16 Mojave survey",
        "NHD"
      ],
      on_blm_ca: [true, true, false, false, false],
      dist_to_blm_mi: [0, 0, 2, 4, 9],
      google_search_url: [
        "https://example.test/1",
        "https://example.test/2",
        "https://example.test/3",
        "https://example.test/4",
        "https://example.test/5"
      ],
      source_report_url: [
        null,
        null,
        null,
        "https://example.test/report",
        null
      ],
      source_report_label: [
        null,
        null,
        null,
        "Survey report",
        null
      ]
    },
    locations: {
      record_start: [0, 2, 3, 4],
      record_count: [2, 1, 1, 1]
    },
    metadata: {
      analyticalRecordCount: 5,
      validCoordinateCount: 5,
      recordCount: 5,
      uniqueCoordinateCount: 4,
      duplicateLocationCount: 1,
      recordsAtDuplicateLocations: 2,
      maxRecordsAtLocation: 2,
      sourceCounts: {nhd: 4, survey_2015_16: 1},
      clusterToExactZoom: 11,
      spatialCellDegrees: 0.25,
      approximatePayloadBytes: 1000
    }
  };

  installLayer.call(map, {}, {}, data);
  map.fire("overlayadd", {
    layer: {options: {group: "Points – Hot Springs Resorts"}}
  });
  assert.strictEqual(
    window.BRIM_SPRINGS_LOCAL.stats().active,
    false,
    "a similarly named overlay incorrectly activated Springs"
  );
  checkedState.checked = true;
  map.fire("overlayadd", {name: data.groupName});

  await waitFor(() => {
    const stats = window.BRIM_SPRINGS_LOCAL.stats();
    return stats.active && !stats.loading && stats.drawn === 4;
  });

  let stats = window.BRIM_SPRINGS_LOCAL.stats();
  assert.strictEqual(stats.total, 5);
  assert.strictEqual(stats.analyticalRecords, 5);
  assert.strictEqual(stats.validCoordinates, 5);
  assert.strictEqual(stats.uniqueCoordinates, 4);
  assert.strictEqual(stats.duplicateLocations, 1);
  assert.strictEqual(stats.recordsAtDuplicateLocations, 2);
  assert.strictEqual(stats.maxRecordsAtLocation, 2);
  assert.strictEqual(stats.visibleRecords, 5);
  assert.strictEqual(stats.visibleLocations, 4);
  assert.strictEqual(stats.exactObjects, 4);
  assert.strictEqual(stats.aggregateObjects, 0);
  assert.strictEqual(stats.interactivePane, "pane_springs_interactive");
  assert.strictEqual(
    map.getPane("pane_springs_interactive").style.pointerEvents,
    "auto"
  );

  const multi = visibleFeatureLayers(map).find(
    (layer) => layer._ptSpringsMulti
  );
  const survey = visibleFeatureLayers(map).find(
    (layer) => (
      layer.options &&
      layer.options.renderer &&
      layer.options.color === "#E6550D"
    )
  );
  const ordinary = visibleFeatureLayers(map).find(
    (layer) => (
      layer.options &&
      layer.options.renderer &&
      layer.options.fillColor === "#2B6CB0"
    )
  );
  assert.ok(multi, "multi-record spring location marker was not created");
  assert.deepStrictEqual(
    multi._ptSpringsMulti.recordIds,
    ["spring_1", "spring_2"]
  );
  assert.ok(
    multi.options.icon.html.includes("pt-springs-multi-marker nhd"),
    "same-source multi-record marker lost NHD visual meaning"
  );
  assert.ok(survey, "survey open-ring Canvas point was not created");
  assert.strictEqual(survey.options.fillOpacity, 0);
  assert.ok(ordinary, "ordinary NHD Canvas point was not created");
  assert.strictEqual(ordinary.getTooltip(), null);

  map._ptMeasureInteractionActive = true;
  ordinary.fire("mouseover");
  assert.strictEqual(
    ordinary.getTooltip(),
    null,
    "Measure mode did not suppress spring hover"
  );
  map._ptMeasureInteractionActive = false;
  ordinary.fire("mouseover");
  assert.ok(ordinary.getTooltip(), "spring tooltip was not built lazily");

  const multiPopup = String(multi._popupContent());
  assert.ok(
    multiPopup.includes("spring_1") &&
      multiPopup.includes("spring_2") &&
      multiPopup.includes("https://example.test/1") &&
      multiPopup.includes("https://example.test/2"),
    "multi-record popup lost identifiers or record links"
  );

  window.BRIM_SPRINGS_LOCAL.applyFilters({
    source: "survey",
    blmMode: "any",
    blmMax: null
  });
  await waitFor(() => {
    const current = window.BRIM_SPRINGS_LOCAL.stats();
    return !current.loading && current.filtered === 1 && current.drawn === 1;
  });
  stats = window.BRIM_SPRINGS_LOCAL.stats();
  assert.strictEqual(stats.visibleRecords, 1);
  assert.strictEqual(visibleFeatureLayers(map)[0].options.color, "#E6550D");

  window.BRIM_SPRINGS_LOCAL.applyFilters({
    source: "all",
    blmMode: "on",
    blmMax: null
  });
  await waitFor(() => {
    const current = window.BRIM_SPRINGS_LOCAL.stats();
    return !current.loading && current.filtered === 2 && current.drawn === 1;
  });
  assert.deepStrictEqual(
    window.BRIM_SPRINGS_LOCAL.multiRecordLocations()[0].recordIds,
    ["spring_1", "spring_2"],
    "filtering lost a member of the retained same-coordinate group"
  );

  window.BRIM_SPRINGS_LOCAL.resetFilters();
  await waitFor(() => {
    const current = window.BRIM_SPRINGS_LOCAL.stats();
    return !current.loading && current.filtered === 5 && current.drawn === 4;
  });

  map._zoom = 12;
  map.fire("zoomstart");
  map.fire("movestart");
  map.fire("zoomend");
  map.fire("moveend");
  await waitFor(() => !window.BRIM_SPRINGS_LOCAL.stats().loading);
  labelCheckedState.checked = true;
  map.fire("overlayadd", {name: data.labelGroupName});
  await waitFor(() => window.BRIM_SPRINGS_LOCAL.stats().labelObjects === 5);
  assert.strictEqual(window.BRIM_SPRINGS_LOCAL.stats().labelObjects, 5);

  checkedState.checked = false;
  map.fire("overlayremove", {name: data.groupName});
  stats = window.BRIM_SPRINGS_LOCAL.stats();
  assert.strictEqual(stats.active, false);
  assert.strictEqual(stats.displayCacheReady, true);
  assert.strictEqual(stats.labelObjects, 0);
  assert.strictEqual(visibleFeatureLayers(map).length, 0);

  checkedState.checked = true;
  map.fire("overlayadd", {name: data.groupName});
  await waitFor(() => {
    const current = window.BRIM_SPRINGS_LOCAL.stats();
    return !current.loading && current.drawn === 4;
  });
  assert.strictEqual(
    window.BRIM_SPRINGS_LOCAL.stats().displayCacheReady,
    true,
    "same-view ordinary reactivation did not reuse the bounded display root"
  );

  window.BRIM_SPRINGS_LOCAL.refresh();
  checkedState.checked = false;
  map.fire("overlayremove", {name: data.groupName});
  await new Promise((resolve) => setTimeout(resolve, 100));
  assert.strictEqual(window.BRIM_SPRINGS_LOCAL.stats().active, false);
  assert.strictEqual(
    visibleFeatureLayers(map).length,
    0,
    "layer-off allowed pending Springs work to recreate geometry"
  );
  checkedState.checked = true;
  map.fire("overlayadd", {name: data.groupName});
  await waitFor(() => (
    !window.BRIM_SPRINGS_LOCAL.stats().loading &&
    window.BRIM_SPRINGS_LOCAL.stats().drawn === 4
  ));

  map.fire("movestart");
  assert.strictEqual(
    visibleFeatureLayers(map).length,
    0,
    "movement did not detach Springs before basemap work"
  );
  map.fire("moveend");
  await waitFor(() => (
    !window.BRIM_SPRINGS_LOCAL.stats().loading &&
    window.BRIM_SPRINGS_LOCAL.stats().drawn === 4
  ));

  map._zoom = 6;
  map.fire("zoomstart");
  map.fire("movestart");
  map.fire("zoomend");
  map.fire("moveend");
  await waitFor(() => {
    const current = window.BRIM_SPRINGS_LOCAL.stats();
    return !current.loading && current.aggregateObjects > 0;
  });
  stats = window.BRIM_SPRINGS_LOCAL.stats();
  assert.ok(stats.leafletDisplayObjects < stats.uniqueCoordinates);
  assert.strictEqual(stats.visibleRecords, 5);
  const aggregate = visibleFeatureLayers(map).find(
    (layer) => (
      layer.options &&
      layer.options.icon &&
      String(layer.options.icon.className).includes("aggregate")
    )
  );
  assert.ok(aggregate, "low-zoom aggregate marker was not created");
  assert.strictEqual(
    aggregate.getTooltip(),
    null,
    "aggregate hover content was constructed eagerly"
  );
  aggregate.fire("mouseover");
  assert.ok(aggregate.getTooltip(), "aggregate tooltip was not built lazily");

  map.fire("pt:marqueezoomstart");
  map._zoom = 11;
  map.fire("movestart");
  map.fire("pt:marqueezoomend");
  map.fire("moveend");
  await waitFor(() => {
    const current = window.BRIM_SPRINGS_LOCAL.stats();
    return !current.loading && current.exactObjects === 4;
  });
  assert.deepStrictEqual(
    window.BRIM_SPRINGS_LOCAL.multiRecordLocations()[0].recordIds,
    ["spring_1", "spring_2"]
  );

  window.BRIM_SPRINGS_LOCAL.refresh();
  window.BRIM_SPRINGS_LOCAL.clear();
  await new Promise((resolve) => setTimeout(resolve, 100));
  stats = window.BRIM_SPRINGS_LOCAL.stats();
  assert.strictEqual(stats.active, false);
  assert.strictEqual(stats.displayCacheReady, false);
  assert.strictEqual(stats.drawn, 0);
  assert.strictEqual(
    visibleFeatureLayers(map).length,
    0,
    "cancelled Springs work recreated stale geometry"
  );

  checkedState.checked = true;
  map.fire("overlayadd", {name: data.groupName});
  await waitFor(() => !window.BRIM_SPRINGS_LOCAL.stats().loading);
  const clearTarget = {
    id: "pt-clear-all-btn",
    className: "",
    closest: () => clearTarget
  };
  (documentListeners.click || []).slice().forEach((handler) => {
    handler({target: clearTarget});
  });
  stats = window.BRIM_SPRINGS_LOCAL.stats();
  assert.strictEqual(stats.active, false);
  assert.strictEqual(stats.displayCacheReady, false);
  assert.strictEqual(visibleFeatureLayers(map).length, 0);

  checkedState.checked = false;
  map.fire("overlayremove", {name: data.groupName});
  window.BRIM_SPRINGS_LOCAL.destroy();
  assert.ok(
    Object.values(map._events).every((handlers) => handlers.length === 0),
    "destroy left duplicate-able Springs map listeners attached"
  );
  assert.strictEqual(window.BRIM_SPRINGS_LOCAL, undefined);

  console.log("Springs virtualized fixture: OK");
}

run().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
