const assert = require("assert");
const fs = require("fs");
const path = require("path");

const root = path.join(__dirname, "..");
const sourcePath = path.join(
  root,
  "03_functions",
  "js",
  "leaflet_calsim3_local_cluster.js"
);
const source = fs.readFileSync(sourcePath, "utf8");
const installController = new Function(`return (${source}\n);`)();

const toolsSource = fs.readFileSync(
  path.join(root, "03_functions", "js", "leaflet_tools_adddata_panel.js"),
  "utf8"
);
const coreSource = fs.readFileSync(
  path.join(root, "03_functions", "leaflet_core_helpers.r"),
  "utf8"
);
const helperSource = fs.readFileSync(
  path.join(
    root,
    "03_functions",
    "leaflet_layer_local_reference_helpers.r"
  ),
  "utf8"
);
const polygonSource = fs.readFileSync(
  path.join(root, "03_functions", "leaflet_layer_local_polygon_helpers.r"),
  "utf8"
);
const swrcbSource = fs.readFileSync(
  path.join(root, "03_functions", "leaflet_layer_local_swrcb_helpers.r"),
  "utf8"
);
const legendSource = fs.readFileSync(
  path.join(
    root,
    "03_functions",
    "js",
    "brim_legend_closeout_helpers.js"
  ),
  "utf8"
);
const mapBuildSource = fs.readFileSync(
  path.join(root, "05_map_build", "04_build_portatreasure2_core_map.r"),
  "utf8"
);
const labelConfigSource = fs.readFileSync(
  path.join(root, "00_config", "config_labels.r"),
  "utf8"
);

function evented(target = {}) {
  target._events = {};
  target.on = function(names, handler) {
    String(names).split(/\s+/).filter(Boolean).forEach((name) => {
      this._events[name] = this._events[name] || [];
      this._events[name].push(handler);
    });
    return this;
  };
  target.off = function(names, handler) {
    String(names).split(/\s+/).filter(Boolean).forEach((name) => {
      if (!this._events[name]) return;
      this._events[name] = this._events[name].filter(
        (candidate) => candidate !== handler
      );
    });
    return this;
  };
  target.fire = function(name, details = {}) {
    const event = Object.assign({type: name, target: this}, details);
    (this._events[name] || []).slice().forEach((handler) => handler(event));
    return this;
  };
  target.listenerCount = function(name) {
    return (this._events[name] || []).length;
  };
  target.totalListenerCount = function() {
    return Object.values(this._events).reduce(
      (total, handlers) => total + handlers.length,
      0
    );
  };
  return target;
}

function makePath(kind, index) {
  return {
    kind,
    index,
    styles: [],
    popupCalls: 0,
    radius: null,
    setRadiusCalls: 0,
    setStyle(style) {
      this.styles.push(Object.assign({}, style));
      return this;
    },
    setRadius(value) {
      this.radius = value;
      this.setRadiusCalls += 1;
      return this;
    },
    getRadius() {
      return this.radius;
    },
    bringToFront() {
      this.broughtToFront = true;
      return this;
    },
    openPopup() {
      this.popupCalls += 1;
      return this;
    },
    getBounds() {
      return {
        kind: "bounds",
        index,
        getCenter() {
          return {lat: 35 + index / 100, lng: -120 - index / 100};
        }
      };
    },
    getLatLng() {
      return {lat: 35 + index / 100, lng: -120 - index / 100};
    }
  };
}

function makeClassList(initial = []) {
  const values = new Set(initial);
  return {
    add(name) {
      values.add(name);
    },
    remove(name) {
      values.delete(name);
    },
    toggle(name, active) {
      if (active === undefined) {
        if (values.has(name)) values.delete(name);
        else values.add(name);
      } else if (active) {
        values.add(name);
      } else {
        values.delete(name);
      }
      return values.has(name);
    },
    contains(name) {
      return values.has(name);
    },
    toString() {
      return Array.from(values).join(" ");
    }
  };
}

class MockMarkerCluster {}
class MockCanvas {
  constructor() {
    this.options = {pane: "pane_calsim3", tolerance: 0};
    this._container = {
      tagName: "CANVAS",
      className: "leaflet-zoom-animated",
      style: {pointerEvents: ""}
    };
    this._drawFirst = null;
    this._hoveredLayer = null;
    this.hoverCandidate = null;
    this.mouseMoveCalls = 0;
    this.clickCalls = 0;
    this.mouseOutCalls = 0;
  }
  _onMouseMove() {
    this.mouseMoveCalls += 1;
    this._hoveredLayer = this.hoverCandidate;
  }
  _onClick() {
    this.clickCalls += 1;
  }
  _handleMouseOut() {
    this.mouseOutCalls += 1;
    this._hoveredLayer = null;
  }
}

global.L = {
  MarkerCluster: MockMarkerCluster,
  Canvas: MockCanvas,
  point: (x, y) => ({x, y}),
  divIcon: (options) => options,
  marker: (latlng, options) => ({
    kind: "label",
    _latlng: {lat: Number(latlng[0]), lng: Number(latlng[1])},
    options
  }),
  layerGroup: () => {
    const layers = [];
    return {
      options: {},
      _layers: layers,
      addLayer(layer) {
        layers.push(layer);
        return this;
      },
      clearLayers() {
        layers.length = 0;
        return this;
      },
      getLayers() {
        return layers.slice();
      },
      addTo(map) {
        this._map = map;
        map.addLayer(this);
        return this;
      }
    };
  }
};

global.document = {
  body: {
    appendChild() {}
  },
  elementFromPoint() {
    return null;
  },
  createElement() {
    return {
      click() {},
      remove() {}
    };
  }
};

global.window = {
  location: {search: ""},
  URL: {
    createObjectURL: () => "blob:fixture",
    revokeObjectURL() {}
  }
};

function makeFixture(options = {}) {
  const arcCount = options.arcCount || 10;
  const americanCount = options.americanCount || 0;
  const sharedRenderer = new MockCanvas();
  global.L.canvas = () => sharedRenderer;
  const arcRecords = [];
  const arcLayers = {};
  for (let i = 1; i <= arcCount; i += 1) {
    const lid = `pt_calsim3_arc_${String(i).padStart(5, "0")}`;
    const type = i % 2 ? "Channel" : "Return";
    const american = i <= americanCount;
    const record = {
      lid,
      id: american && i === 1 ? "AMERICAN" :
        (american && i === 2 ? "AMERICAN_PREFIX_2" :
          (i === 1 ? "C_ALPHA" : `ARC_${i}`)),
      fromNode: american && i === 3 ? "AMERICAN" :
        (american && i === 4 ? "AMERICAN_ENDPOINT_4" :
          (i === 1 ? "FROM_ALPHA" : `FROM_${i}`)),
      toNode: i === 1 ? "TO_ALPHA" : `TO_${i}`,
      name: american ?
        (i === 5 ? "American" : `Central American fixture ${i}`) :
        (i === 1 ? "Alpha Canal" : `Fixture arc ${i}`),
      type,
      color: type === "Channel" ? "#1F78B4" : "#33A02C",
      weight: type === "Channel" ? 1.4 : 1
    };
    arcRecords.push(record);
    arcLayers[lid] = makePath("arc", i);
    arcLayers[lid]._renderer = sharedRenderer;
  }

  const nodeRecords = [
    {
      lid: "pt_calsim3_node_00001",
      id: "NODE_ALPHA",
      description: "Conveyance junction",
      river: "Alpha River",
      comment: "Finder comment alpha",
      group: "Conveyance",
      fill: "#1F78B4",
      stroke: "#08519C",
      radius: 4.5
    },
    {
      lid: "pt_calsim3_node_00002",
      id: "NODE_STORAGE",
      description: "Storage reservoir",
      river: "Beta River",
      comment: "",
      group: "Storage / Reservoir",
      fill: "#6A3D9A",
      stroke: "#3F007D",
      radius: 5.5
    },
    {
      lid: "pt_calsim3_node_00003",
      id: "NODE_RETURN",
      description: "Return flow",
      river: "",
      comment: "Return fixture",
      group: "Return flow",
      fill: "#33A02C",
      stroke: "#1B7837",
      radius: 4.5
    }
  ];
  const nodeLayers = {};
  nodeRecords.forEach((record, index) => {
    nodeLayers[record.lid] = makePath("node", index + 1);
    nodeLayers[record.lid].radius = record.radius;
    nodeLayers[record.lid]._renderer = sharedRenderer;
  });

  const clusterMembers = new Set(Object.values(nodeLayers));
  const visibleChildren = [];
  const clusterGroup = evented({
    _spiderfied: null,
    clusterLayerStore: {_layers: nodeLayers},
    _featureGroup: {
      eachLayer(callback) {
        visibleChildren.forEach(callback);
      }
    },
    getLayers() {
      return Array.from(clusterMembers);
    },
    hasLayer(layer) {
      return clusterMembers.has(layer);
    },
    addLayer(layer) {
      this.addLayerCalls += 1;
      clusterMembers.add(layer);
      return this;
    },
    removeLayer(layer) {
      this.removeLayerCalls += 1;
      clusterMembers.delete(layer);
      return this;
    },
    addLayerCalls: 0,
    removeLayerCalls: 0,
    unspiderfyCalls: 0,
    unspiderfy() {
      this.unspiderfyCalls += 1;
      this._spiderfied = null;
    }
  });

  const groupMembers = new Set([
    ...Object.values(arcLayers),
    clusterGroup
  ]);
  const groupRoot = {
    addLayerCalls: 0,
    removeLayerCalls: 0,
    hasLayer(layer) {
      return groupMembers.has(layer);
    },
    addLayer(layer) {
      this.addLayerCalls += 1;
      groupMembers.add(layer);
      return this;
    },
    removeLayer(layer) {
      this.removeLayerCalls += 1;
      groupMembers.delete(layer);
      return this;
    }
  };

  const activeLayers = new Set(
    options.initialActive === false ? [] : [groupRoot]
  );
  const containerClassList = makeClassList(["leaflet-container"]);
  const container = {
    tagName: "DIV",
    id: "fixture-map",
    className: "leaflet-container",
    classList: containerClassList,
    style: {},
    querySelectorAll() {
      return [];
    }
  };
  const paneStates = {
    pane_polygons: {
      style: {zIndex: "410", pointerEvents: ""},
      getAttribute() {
        return null;
      }
    },
    pane_lines: {
      style: {zIndex: "480", pointerEvents: ""},
      getAttribute() {
        return null;
      }
    },
    pane_points: {
      style: {zIndex: "520", pointerEvents: ""},
      getAttribute() {
        return null;
      }
    },
    pane_calsim3: {
      style: {zIndex: "525", pointerEvents: ""},
      getAttribute() {
        return null;
      }
    }
  };
  let draggingEnabled = true;
  let draggingMoving = false;
  let boxZoomEnabled = true;
  let zoom = 8;
  let viewportBounds = {
    west: -123,
    south: 34,
    east: -118,
    north: 38
  };
  const map = evented({
    _paneRenderers: {},
    _ptMeasureInteractionActive: false,
    closedTooltips: [],
    closedPopups: [],
    fitBoundsCalls: [],
    setViewCalls: [],
    layerManager: {
      getLayerGroup(groupName) {
        return groupName === "Channels – CalSim3.0" ? groupRoot : null;
      },
      getLayer(category, id) {
        if (category === "cluster" && id === "pt_calsim3_nodes_cluster") {
          return clusterGroup;
        }
        if (category === "shape") return arcLayers[id] || null;
        return null;
      }
    },
    getZoom() {
      return zoom;
    },
    getBounds() {
      return {
        getWest: () => viewportBounds.west,
        getSouth: () => viewportBounds.south,
        getEast: () => viewportBounds.east,
        getNorth: () => viewportBounds.north
      };
    },
    getContainer() {
      return container;
    },
    getPanes() {
      return paneStates;
    },
    dragging: {
      enabled() {
        return draggingEnabled;
      },
      moving() {
        return draggingMoving;
      },
      enable() {
        draggingEnabled = true;
      },
      disable() {
        draggingEnabled = false;
      }
    },
    boxZoom: {
      enabled() {
        return boxZoomEnabled;
      },
      enable() {
        boxZoomEnabled = true;
      },
      disable() {
        boxZoomEnabled = false;
      }
    },
    hasLayer(layer) {
      return activeLayers.has(layer);
    },
    addLayer(layer) {
      activeLayers.add(layer);
      this.fire("layeradd", {layer});
      return this;
    },
    removeLayer(layer) {
      activeLayers.delete(layer);
      this.fire("layerremove", {layer});
      return this;
    },
    fitBounds(bounds, options) {
      this.fitBoundsCalls.push({bounds, options});
      return this;
    },
    setView(latlng, targetZoom, options) {
      this.setViewCalls.push({latlng, zoom: targetZoom, options});
      zoom = targetZoom;
      return this;
    },
    closeTooltip(tooltip) {
      this.closedTooltips.push(tooltip);
      this.fire("tooltipclose", {tooltip});
    },
    closePopup(popup) {
      this.closedPopups.push(popup);
      this.fire("popupclose", {popup});
    }
  });

  const labelInput = {
    checked: false,
    click() {
      this.checked = !this.checked;
      map.fire(this.checked ? "overlayadd" : "overlayremove", {
        name: "Labels – CalSim3.0"
      });
    }
  };
  const labelControlRow = {
    textContent: "Labels – CalSim3.0",
    innerText: "Labels – CalSim3.0",
    getAttribute(name) {
      return name === "data-pt-layer-full-name" ?
        "Labels – CalSim3.0" : "";
    },
    querySelector() {
      return labelInput;
    }
  };
  global.document.querySelectorAll = (selector) => (
    selector === ".leaflet-control-layers-overlays label" ?
      [labelControlRow] :
      []
  );

  return {
    arcRecords,
    nodeRecords,
    arcLayers,
    nodeLayers,
    sharedRenderer,
    clusterGroup,
    clusterMembers,
    groupRoot,
    groupMembers,
    activeLayers,
    container,
    paneStates,
    map,
    labelInput,
    labelControlRow,
    setZoom(value) {
      zoom = value;
    },
    setBounds(west, south, east, north) {
      viewportBounds = {west, south, east, north};
    },
    visibleLabelRoots() {
      return Array.from(activeLayers).filter(
        (layer) => layer && layer._brimCalsim3LabelRoot
      );
    },
    setDraggingMoving(value) {
      draggingMoving = !!value;
    }
  };
}

function makeClusterClickLayer(fixture) {
  return {
    zoomToBoundsCalls: 0,
    spiderfyCalls: 0,
    zoomToBounds() {
      this.zoomToBoundsCalls += 1;
    },
    spiderfy() {
      this.spiderfyCalls += 1;
      fixture.clusterGroup._spiderfied = this;
    }
  };
}

function install(fixture, duplicateCount = 1, dataOverrides = {}) {
  const element = {
    tagName: "DIV",
    className: "leaflet-container",
    classList: fixture.container.classList,
    style: {},
    querySelectorAll(selector) {
      if (selector === ".marker-cluster") return [];
      if (selector === "path.leaflet-interactive") return [];
      if (selector === "canvas.leaflet-zoom-animated") return [{}, {}];
      return [];
    }
  };
  const data = {
    groupName: "Channels – CalSim3.0",
    labelGroupName: "Labels – CalSim3.0",
    labelMinZoom: 11,
    labelCap: 160,
    labelNodeCap: 96,
    labelArcCap: 64,
    labelGridDegrees: 0.25,
    labelViewportPadRatio: 0.15,
    clusterId: "pt_calsim3_nodes_cluster",
    transitionZoom: 9,
    exactClusterRadiusPx: 0.000001,
    arcRecordCount: fixture.arcRecords.length,
    analyticalRecordCount: fixture.nodeRecords.length,
    validCoordinateCount: fixture.nodeRecords.length,
    uniqueExactCoordinateCount: duplicateCount ?
      fixture.nodeRecords.length - 1 :
      fixture.nodeRecords.length,
    exactDuplicateLocationCount: duplicateCount,
    recordsAtExactDuplicateLocations: duplicateCount ? 2 : 0,
    maxRecordsAtExactLocation: duplicateCount ? 2 : 1,
    sameLocationMode: duplicateCount ?
      "exact-coordinate-spiderfy" :
      "not-required-current-cache",
    arcRecords: asColumns(fixture.arcRecords),
    nodeRecords: asColumns(fixture.nodeRecords),
    arcTypeOrder: ["Channel", "Return"],
    nodeGroupOrder: ["Conveyance", "Storage / Reservoir", "Return flow"]
  };
  Object.assign(data, dataOverrides);
  installController.call(fixture.map, element, {}, data);
}

function delay(milliseconds) {
  return new Promise((resolve) => setTimeout(resolve, milliseconds));
}

function assertClose(actual, expected, message) {
  assert.ok(
    Math.abs(actual - expected) < 0.0001,
    `${message}: expected ${expected}, received ${actual}`
  );
}

function asColumns(records) {
  const out = {};
  Object.keys(records[0] || {}).forEach((field) => {
    out[field] = records.map((record) => record[field]);
  });
  return out;
}

async function run() {
  assert.match(
    toolsSource,
    /map\.fire\('pt:measureinteractionchange', \{active: active\}\)/,
    "Measure state must continue publishing the CalSim lifecycle event"
  );
  assert.match(
    toolsSource,
    /ptMeasureSuspendedPanes\.push\(\{[\s\S]*pointerEvents: pane\.style\.pointerEvents[\s\S]*pane\.style\.pointerEvents = 'none'/,
    "Measure must save each pane's prior pointer-event state before suspension"
  );
  assert.match(
    toolsSource,
    /state\.pane\.style\.pointerEvents = state\.pointerEvents[\s\S]*ptMeasureSuspendedPanes = \[\]/,
    "Measure off must restore every saved pane pointer-event state"
  );
  assert.match(
    coreSource,
    /wasDragging = map\.dragging[\s\S]*map\.dragging\.disable\(\)[\s\S]*map\.dragging && wasDragging[\s\S]*map\.dragging\.enable\(\)/,
    "marquee completion must restore the pre-existing dragging state"
  );
  assert.match(
    coreSource,
    /addEventListener\('pointercancel'[\s\S]*if \(active\) setActive\(false\)/,
    "a canceled marquee pointer must release capture state and dragging"
  );
  assert.match(
    coreSource,
    /addEventListener\('blur'[\s\S]*if \(active\) setActive\(false\)/,
    "window blur must release an unfinished marquee interaction"
  );
  assert.match(
    coreSource,
    /isControlTarget\(e\.target\)[\s\S]*!isOwnZoomUtilityTarget\(e\.target\)[\s\S]*setActive\(false\)/,
    "using Finder or another map control must cancel marquee mode first"
  );
  assert.match(
    coreSource,
    /pt-main-layer-clear-btn[\s\S]*input\.click\(\)/,
    "Clear Local must continue using normal overlay checkbox removal"
  );
  assert.match(
    toolsSource,
    /ptClearAllPt2SessionLayers[\s\S]*mainClear\.click\(\)/,
    "Clear All must continue delegating to Clear Local"
  );
  assert.match(
    coreSource,
    /addMapPane\("pane_calsim3",\s+zIndex = 525\)/,
    "CalSim arcs and nodes need one dedicated pane above normal points"
  );
  assert.match(
    coreSource,
    /preferCanvas\s*=\s*TRUE/,
    "the shared CalSim pane must resolve to Canvas in the production map"
  );
  assert.match(
    helperSource,
    /pane = PT_CALSIM3_PANE,\s*interactive = TRUE/g,
    "CalSim arcs and nodes must be interactive in the same pane"
  );
  assert.doesNotMatch(
    helperSource,
    /pt_calsim3_canvas_renderer/,
    "split explicit CalSim canvases would reintroduce event interception"
  );
  assert.match(
    source,
    /renderer\.options\.tolerance = canvasTolerance/,
    "the shared renderer must retain the serialized arc hit tolerance"
  );
  assert.match(
    source,
    /L\.canvas\(\{[\s\S]*pane: calsimPaneName[\s\S]*map\._paneRenderers\[calsimPaneName\] = renderer/,
    "the controller must prime exactly one deferred pane-scoped Canvas"
  );
  assert.match(
    source,
    /sharedPaneRenderer\._container\.style\.pointerEvents = 'none'/,
    "the full-map CalSim Canvas must remain outside DOM hit targeting"
  );
  assert.match(
    source,
    /mapEventIsBackground\(event\)[\s\S]*sharedPaneRenderer\._onMouseMove\(event\.originalEvent\)/,
    "only background map events may enter the existing CalSim Canvas hit loop"
  );
  assert.match(
    source,
    /interactionState: globalInteractionState[\s\S]*hitTargetAt: hitTargetAt/,
    "optional shared-interaction diagnostics are missing"
  );
  assert.match(
    helperSource,
    /layerId = ~pt_calsim3_browser_id/g,
    "arcs and nodes must receive browser-only unique layer keys"
  );
  assert.match(
    source,
    /CalSim3\.0 Network Explorer/,
    "the unified card title is missing"
  );
  assert.match(
    source,
    /pt-calsim3-dock[\s\S]*pt-calsim3-close/,
    "the dock action must be immediately before the close action"
  );
  assert.match(
    source,
    /@media\(max-width:390px\)/,
    "the explorer lacks the requested narrow-viewport layout"
  );
  assert.match(
    source,
    /max-height:calc\(100vh - 154px\);overflow:auto/,
    "the explorer lacks short-viewport scrolling"
  );
  assert.match(
    source,
    /width:348px[\s\S]*max-height:calc\(100vh - 154px\)/,
    "the rendered card dimensions must not increase"
  );
  assert.match(
    source,
    /P = project · NP = non-project/,
    "the compact node-group family key is missing"
  );
  assert.match(
    labelConfigSource,
    /"CalSim3\.0"[\s\S]*"CalSim3\.0"/,
    "CalSim is missing from the shared inline-label pair configuration"
  );
  assert.match(
    coreSource,
    /\{main: 'CalSim3\.0', label: 'CalSim3\.0'\}/,
    "the Local layer-row inline lbl registry is missing CalSim"
  );
  assert.match(
    mapBuildSource,
    /"Labels: CalSim3\.0"[\s\S]*pt_add_calsim3_label_companion\([\s\S]*pt_add_calsim3_cluster_controller\(/,
    "the CalSim Labels companion row must register before the controller"
  );
  assert.match(
    helperSource,
    /pt_add_calsim3_label_companion[\s\S]*pt_calsim3_label_dummy[\s\S]*pane = "pane_labels_pts"[\s\S]*interactive = FALSE/,
    "the shared CalSim label companion is not a hidden noninteractive label row"
  );
  assert.match(
    source,
    /pt-calsim3-labels[^]*?lbl[^]*?z11\+[^]*?pt-calsim3-dock[^]*?pt-calsim3-close/,
    "the Explorer header lbl control must precede dock and close"
  );
  assert.match(
    source,
    /labelMinZoom = Number\(calsimData\.labelMinZoom \|\| 11\)[\s\S]*labelCap = Number\(calsimData\.labelCap \|\| 160\)/,
    "CalSim labels must use the exact zoom-11 threshold and bounded default cap"
  );
  assert.match(
    source,
    /fromNode \+ ' → ' \+ toNode[\s\S]*rule: 'explicit-endpoints'/,
    "arc label fallback must use explicit retained endpoints"
  );
  assert.match(
    source,
    /node: 'retained node ID'[\s\S]*'meaningful retained Name'[\s\S]*'explicit FromNode → ToNode'[\s\S]*'retained Arc_ID'/,
    "the audited label content hierarchy is missing from diagnostics"
  );
  assert.match(
    source,
    /buildLabelIndex\(\);[\s\S]*listen\(map, 'mousemove', onMapMouseMove\)/,
    "the lightweight spatial label index must build once before pointer listeners"
  );
  assert.match(
    source,
    /pt-calsim3-label-icon[\s\S]*pointer-events:none!important/,
    "CalSim label icons must never intercept map pointer interaction"
  );
  assert.match(
    source,
    /'Project demand – urban': 'P — Urban'[\s\S]*'Non-project demand – ag': 'NP — Ag'/,
    "node-group labels are not compact display-only mappings"
  );
  assert.match(
    source,
    /value="' \+ esc\(value\)[\s\S]*title="' \+\s*esc\(title \|\| label\)/,
    "compact labels must preserve original filter values and titles"
  );
  assert.match(
    source,
    /class="pt-calsim3-auto pt-calsim3-live-apply" checked> Live apply/,
    "Live apply must be distinct, default on, and retain Auto compatibility"
  );
  const displayMarkupStart = source.indexOf(
    "html += '<div class=\"pt-calsim3-display-row\">';"
  );
  const displayMarkupEnd = source.indexOf(
    "html += '</div></div>';",
    displayMarkupStart
  );
  assert.ok(displayMarkupStart >= 0 && displayMarkupEnd > displayMarkupStart);
  assert.doesNotMatch(
    source.slice(displayMarkupStart, displayMarkupEnd),
    /pt-calsim3-auto|Live apply/,
    "Live apply must not remain inside the Display checkbox group"
  );
  assert.match(
    source,
    /pt-calsim3-live-row[\s\S]*border:1px solid #b8ad95[\s\S]*background:#f8f5ec/,
    "Live apply must reuse the established neutral BRIM control styling"
  );
  assert.match(
    source,
    /Hover controls tooltips only; click popups remain available\./,
    "the Explorer must state that hover switches do not disable click popups"
  );
  assert.match(
    source,
    /value < 10[\s\S]*scale: 0\.58[\s\S]*value < 11[\s\S]*scale: 0\.76[\s\S]*scale: 1/,
    "the exact compact, medium, and full node-radius tiers are missing"
  );
  assert.match(
    source,
    /record\.layer\.setRadius\(radius\)/,
    "zoom-responsive sizing must mutate the retained CircleMarker radius"
  );
  assert.match(
    source,
    /max-height:190px;overflow:auto/,
    "Finder results must be independently bounded"
  );
  assert.match(
    legendSource,
    /card: '\.pt-calsim3-explorer'[\s\S]*dock: '\.pt-calsim3-dock'/,
    "the explorer is not registered with the shared detachable-card helper"
  );
  assert.ok(
    mapBuildSource.indexOf("pt_add_calsim3_arc_layer(") <
      mapBuildSource.indexOf("pt_add_calsim3_node_layer(") &&
      mapBuildSource.indexOf("pt_add_calsim3_node_layer(") <
      mapBuildSource.indexOf("pt_add_calsim3_cluster_controller("),
    "CalSim arcs must register before exact nodes and the controller"
  );
  [
    "arc-all",
    "arc-none",
    "node-all",
    "node-none",
    "finder-more",
    "finder-all",
    "finder-fewer"
  ].forEach((action) => {
    assert.ok(
      source.includes(`data-action="${action}"`) ||
        source.includes(`action === '${action}'`),
      `Explorer action is missing: ${action}`
    );
  });
  assert.strictEqual(
    (source.match(/cardControl = L\.control/g) || []).length,
    1,
    "CalSim must create exactly one Explorer card"
  );
  assert.doesNotMatch(
    source,
    /\bL\.(?:polyline|circleMarker)\s*\(/,
    "filtering must reuse retained objects rather than reconstruct geometry"
  );
  assert.doesNotMatch(
    source,
    /map\.dragging\.(?:disable|enable)\(/,
    "CalSim filtering and Finder must not mutate shared dragging state"
  );
  const referencePointBlock = helperSource.slice(
    helperSource.indexOf("add_station_layer <- function"),
    helperSource.indexOf("# ==== 5. Office")
  );
  assert.match(
    referencePointBlock,
    /addCircleMarkers\([\s\S]*popup = ~popup_html[\s\S]*label = ~hover_text[\s\S]*pane = "pane_points"/,
    "representative Reference points lost hover or popup bindings"
  );
  const referenceLineBlock = helperSource.slice(
    helperSource.indexOf("pt_add_major_conveyance_layer <- function"),
    helperSource.indexOf("# ==== 10. CNRFC")
  );
  assert.match(
    referenceLineBlock,
    /addPolylines\([\s\S]*popup = ~popup_html[\s\S]*pane = "pane_lines"[\s\S]*highlightOptions/,
    "Major Conveyance lost popup or hover-highlight behavior"
  );
  assert.match(
    polygonSource,
    /group = pt_layer_group_name\("GW – Bull\. 118"\)[\s\S]*popup = ~popup_html[\s\S]*label = ~pt_gw_hover_html[\s\S]*interactive = TRUE/,
    "representative Reference polygons lost hover or popup bindings"
  );
  assert.match(
    swrcbSource,
    /L\.circleMarker\([\s\S]*marker\.bindTooltip\([\s\S]*marker\.bindPopup\(/,
    "Water Rights lost its retained hover or popup bindings"
  );
  [
    "record.id",
    "record.fromNode",
    "record.toNode",
    "record.name",
    "record.type",
    "record.description",
    "record.river",
    "record.comment",
    "record.group"
  ].forEach((field) => {
    assert.ok(
      source.includes(field),
      `Finder source is missing ${field}`
    );
  });

  const fixture = makeFixture();
  fixture.arcRecords[1].name = "";
  fixture.arcRecords[4].name = "<Null>";
  fixture.arcRecords[4].fromNode = "N/A";
  install(fixture, 1);
  const api = window.BRIM_CALSIM3_LOCAL;
  const auditedNodeGroupLabels = {
    "Conveyance": "Conveyance",
    "Storage / Reservoir": "Storage",
    "Project demand – urban": "P — Urban",
    "Project demand – ag": "P — Ag",
    "Project demand – refuge": "P — Refuge",
    "Non-project demand – urban": "NP — Urban",
    "Non-project demand – ag": "NP — Ag",
    "Non-project demand – refuge": "NP — Refuge",
    "Settlement demand – urban": "Settlement — Urban",
    "Settlement demand – ag": "Settlement — Ag",
    "Demand – other": "Other demand",
    "Treatment plant": "Treatment",
    "Return flow": "Return flow",
    "External Unit": "External unit",
    "Major Feature": "Major feature",
    "Unknown / Other": "Unknown / other"
  };
  Object.entries(auditedNodeGroupLabels).forEach(([retained, display]) => {
    assert.strictEqual(
      api.nodeGroupDisplayLabel(retained),
      display,
      `unexpected compact node-group label for ${retained}`
    );
  });
  assert.strictEqual(
    api.nodeGroupDisplayLabel("Future retained group"),
    "Future retained group",
    "unreviewed node groups must remain literal rather than invent terminology"
  );

  assert.strictEqual(
    fixture.clusterGroup.listenerCount("clusterclick"),
    1,
    "same-location mode must install one cluster-click handler"
  );

  let stats = api.stats();
  assert.strictEqual(stats.active, true);
  assert.strictEqual(stats.transitionZoom, 9);
  assert.strictEqual(stats.analyticalArcRecords, 10);
  assert.strictEqual(stats.analyticalNodeRecords, 3);
  assert.strictEqual(stats.resolvedArcObjects, 10);
  assert.strictEqual(stats.resolvedNodeObjects, 3);
  assert.strictEqual(stats.actualArcObjectsInGroup, 10);
  assert.strictEqual(stats.registeredNodeMarkers, 3);
  assert.strictEqual(stats.shownArcRecords, 10);
  assert.strictEqual(stats.shownNodeRecords, 3);
  assert.strictEqual(stats.listenerCount, 18);
  assert.strictEqual(stats.pendingAsyncFilterWork, false);
  assert.strictEqual(stats.autoApply, true);
  assert.strictEqual(stats.liveApply, true);
  assert.strictEqual(stats.uiDirty, false);
  assert.strictEqual(stats.labels.requested, false);
  assert.strictEqual(stats.labels.minimumZoom, 11);
  assert.strictEqual(stats.labels.cap, 160);
  assert.strictEqual(stats.labels.nodeCap, 96);
  assert.strictEqual(stats.labels.arcCap, 64);
  assert.strictEqual(stats.labels.descriptorCount, 13);
  assert.strictEqual(stats.labels.visibleCount, 0);
  assert.strictEqual(stats.labels.pointerIntercept, false);
  assert.strictEqual(fixture.labelInput.checked, false);
  assert.strictEqual(fixture.visibleLabelRoots().length, 0);
  assert.deepStrictEqual(api.labelRules(), {
    minimumZoom: 11,
    cap: 160,
    nodeCap: 96,
    arcCap: 64,
    node: "retained node ID",
    arc: [
      "meaningful retained Name",
      "explicit FromNode → ToNode",
      "retained Arc_ID"
    ]
  });
  assert.deepStrictEqual(stats.nodeRadius, {
    requestedTier: "compact",
    requestedScale: 0.58,
    appliedTier: "compact",
    appliedScale: 0.58,
    baselineMinimum: 4.5,
    baselineMaximum: 5.5,
    targetMinimum: 2.61,
    targetMaximum: 3.19,
    effectiveMinimum: 2.61,
    effectiveMaximum: 3.19
  });
  const retainedNodeReferences = Object.assign({}, fixture.nodeLayers);
  assertClose(
    fixture.nodeLayers.pt_calsim3_node_00001.radius,
    4.5 * 0.58,
    "zoom-8 NODE_ALPHA radius"
  );
  assertClose(
    fixture.nodeLayers.pt_calsim3_node_00002.radius,
    5.5 * 0.58,
    "zoom-8 NODE_STORAGE radius"
  );
  assert.strictEqual(
    Object.values(fixture.nodeLayers).reduce(
      (total, layer) => total + layer.setRadiusCalls,
      0
    ),
    3,
    "controller install must resize each retained node exactly once"
  );

  fixture.setZoom(9);
  fixture.map.fire("zoomend");
  let radiusCallCount = Object.values(fixture.nodeLayers).reduce(
    (total, layer) => total + layer.setRadiusCalls,
    0
  );
  assert.strictEqual(
    radiusCallCount,
    3,
    "zoom changes inside one radius tier must not resize markers"
  );
  fixture.setZoom(10);
  fixture.map.fire("zoomend");
  assertClose(
    fixture.nodeLayers.pt_calsim3_node_00001.radius,
    4.5 * 0.76,
    "zoom-10 NODE_ALPHA radius"
  );
  assertClose(
    fixture.nodeLayers.pt_calsim3_node_00002.radius,
    5.5 * 0.76,
    "zoom-10 NODE_STORAGE radius"
  );
  stats = api.stats();
  assert.strictEqual(stats.nodeRadius.requestedTier, "medium");
  assert.strictEqual(stats.nodeRadius.appliedTier, "medium");
  fixture.setZoom(10.5);
  fixture.map.fire("zoomend");
  assert.strictEqual(
    Object.values(fixture.nodeLayers).reduce(
      (total, layer) => total + layer.setRadiusCalls,
      0
    ),
    6,
    "zoom 10 to 10.5 must remain one medium tier"
  );
  fixture.setZoom(11);
  fixture.map.fire("zoomend");
  assertClose(
    fixture.nodeLayers.pt_calsim3_node_00001.radius,
    4.5,
    "zoom-11 NODE_ALPHA baseline radius"
  );
  assertClose(
    fixture.nodeLayers.pt_calsim3_node_00002.radius,
    5.5,
    "zoom-11 NODE_STORAGE baseline radius"
  );
  fixture.setZoom(12);
  fixture.map.fire("zoomend");
  assert.strictEqual(
    Object.values(fixture.nodeLayers).reduce(
      (total, layer) => total + layer.setRadiusCalls,
      0
    ),
    9,
    "zoom 11 to 12 must remain one full-size tier"
  );
  fixture.setZoom(10);
  fixture.map.fire("zoomend");
  radiusCallCount = Object.values(fixture.nodeLayers).reduce(
    (total, layer) => total + layer.setRadiusCalls,
    0
  );
  assert.strictEqual(radiusCallCount, 12);
  fixture.setZoom(9);
  fixture.map.fire("zoomend");
  radiusCallCount = Object.values(fixture.nodeLayers).reduce(
    (total, layer) => total + layer.setRadiusCalls,
    0
  );
  assert.strictEqual(radiusCallCount, 15);
  fixture.setZoom(8);
  fixture.map.fire("zoomend");
  assert.strictEqual(
    Object.values(fixture.nodeLayers).reduce(
      (total, layer) => total + layer.setRadiusCalls,
      0
    ),
    radiusCallCount,
    "the reverse zoom 9 to 8 must remain in the compact tier"
  );
  fixture.setZoom(9);
  fixture.map.fire("zoomend");
  fixture.map.fire("zoomend");
  assert.strictEqual(
    Object.values(fixture.nodeLayers).reduce(
      (total, layer) => total + layer.setRadiusCalls,
      0
    ),
    radiusCallCount,
    "repeated zoomend events in the compact tier must perform no updates"
  );
  Object.keys(retainedNodeReferences).forEach((key) => {
    assert.strictEqual(
      fixture.nodeLayers[key],
      retainedNodeReferences[key],
      `radius tiers replaced retained marker ${key}`
    );
  });
  stats = api.stats();
  assert.strictEqual(stats.nodeRadius.requestedTier, "compact");
  assert.strictEqual(stats.nodeRadius.appliedTier, "compact");
  assert.strictEqual(stats.diagnostics.nodeRadiusTierChangeCount, 5);
  assert.strictEqual(stats.diagnostics.nodeRadiusMarkerUpdateCount, 15);
  assert.strictEqual(stats.diagnostics.nodeRadiusRedundantSkipCount, 6);

  api.setLabels(true);
  stats = api.stats();
  assert.strictEqual(fixture.labelInput.checked, true);
  assert.strictEqual(stats.labels.requested, true);
  assert.strictEqual(stats.labels.visibleCount, 0);
  assert.strictEqual(
    fixture.map.fitBoundsCalls.length + fixture.map.setViewCalls.length,
    0,
    "checking lbl below zoom 11 must not move the map"
  );

  fixture.setZoom(11);
  fixture.map.fire("zoomstart");
  fixture.map.fire("zoomend");
  await delay(80);
  stats = api.stats();
  assert.strictEqual(stats.labels.eligible, true);
  assert.strictEqual(stats.labels.visibleCount, 13);
  assert.strictEqual(stats.labels.visibleNodeCount, 3);
  assert.strictEqual(stats.labels.visibleArcCount, 10);
  assert.strictEqual(fixture.visibleLabelRoots().length, 1);
  let visibleLabelMarkers = fixture.visibleLabelRoots()[0].getLayers();
  assert.strictEqual(visibleLabelMarkers.length, 13);
  assert.ok(
    visibleLabelMarkers.every(
      (marker) => marker.options.interactive === false
    ),
    "visible labels must remain noninteractive"
  );
  assert.ok(
    visibleLabelMarkers.every(
      (marker) => (
        marker.options.pane === "pane_labels_pts" ||
        marker.options.pane === "pane_labels_poly"
      )
    ),
    "labels must use established BRIM label panes"
  );
  assert.ok(
    visibleLabelMarkers.some(
      (marker) => marker.options.icon.html.includes("NODE_ALPHA")
    ),
    "node labels must use the concise retained node ID"
  );
  assert.ok(
    visibleLabelMarkers.some(
      (marker) => marker.options.icon.html.includes("Alpha Canal")
    ),
    "arc labels must prefer a meaningful retained name"
  );
  assert.ok(
    visibleLabelMarkers.some(
      (marker) => marker.options.icon.html.includes("FROM_2 → TO_2")
    ),
    "arc labels must fall back to explicit retained endpoints"
  );
  assert.ok(
    visibleLabelMarkers.some(
      (marker) => marker.options.icon.html.includes("ARC_5")
    ),
    "arc labels must fall back to retained Arc_ID when endpoints are unusable"
  );

  fixture.setZoom(10);
  fixture.map.fire("zoomstart");
  assert.strictEqual(api.stats().labels.visibleCount, 0);
  fixture.map.fire("zoomend");
  await delay(80);
  stats = api.stats();
  assert.strictEqual(stats.labels.requested, true);
  assert.strictEqual(stats.labels.visibleCount, 0);
  assert.strictEqual(fixture.labelInput.checked, true);

  fixture.setZoom(11);
  fixture.map.fire("zoomstart");
  fixture.map.fire("zoomend");
  await delay(80);
  assert.strictEqual(api.stats().labels.visibleCount, 13);

  api.apply({
    showArcs: false,
    showNodes: true,
    arcTypes: [],
    nodeGroups: ["Conveyance"]
  });
  await delay(20);
  stats = api.stats();
  assert.strictEqual(stats.labels.visibleCount, 1);
  assert.strictEqual(stats.labels.visibleNodeCount, 1);
  assert.strictEqual(stats.labels.visibleArcCount, 0);

  api.reset();
  await delay(20);
  assert.strictEqual(api.stats().labels.visibleCount, 13);
  api.setLiveApply(false);
  api.scheduleApply({
    showArcs: false,
    showNodes: false,
    arcTypes: [],
    nodeGroups: []
  });
  await delay(20);
  assert.strictEqual(
    api.stats().labels.visibleCount,
    13,
    "manual pending filters must not update labels before Apply"
  );
  api.apply({
    showArcs: false,
    showNodes: false,
    arcTypes: [],
    nodeGroups: []
  });
  await delay(20);
  assert.strictEqual(api.stats().labels.visibleCount, 0);
  api.setLiveApply(true);
  api.reset();
  await delay(20);
  assert.strictEqual(api.stats().labels.visibleCount, 13);

  fixture.map.fire("movestart");
  assert.strictEqual(api.stats().labels.visibleCount, 0);
  fixture.setBounds(-130, 20, -129, 21);
  fixture.map.fire("moveend");
  fixture.setBounds(-123, 34, -118, 38);
  fixture.map.fire("moveend");
  await delay(80);
  stats = api.stats();
  assert.strictEqual(stats.labels.visibleCount, 13);
  assert.ok(
    stats.diagnostics.labelCancelCount > 0,
    "a superseded viewport label job must be canceled"
  );

  fixture.map._ptMeasureInteractionActive = true;
  fixture.map.fire("pt:measureinteractionchange", {active: true});
  assert.strictEqual(api.stats().labels.visibleCount, 0);
  assert.strictEqual(fixture.labelInput.checked, true);
  fixture.map._ptMeasureInteractionActive = false;
  fixture.map.fire("pt:measureinteractionchange", {active: false});
  await delay(80);
  assert.strictEqual(api.stats().labels.visibleCount, 13);
  api.hideCard();
  assert.strictEqual(
    api.stats().labels.visibleCount,
    13,
    "hiding only the Explorer card must not change visible labels"
  );
  assert.strictEqual(
    fixture.labelInput.checked,
    true,
    "hiding only the Explorer card must not change shared label state"
  );

  fixture.setZoom(9);
  fixture.map.fire("zoomstart");
  fixture.map.fire("zoomend");
  await delay(60);
  assert.strictEqual(api.stats().labels.visibleCount, 0);
  assert.strictEqual(api.stats().labels.requested, true);
  api.setLabels(false);
  assert.strictEqual(fixture.labelInput.checked, false);

  assert.deepStrictEqual(stats.renderer, {
    pane: "pane_calsim3",
    tolerance: 6,
    primed: true,
    arcRendererMounted: true,
    nodeRendererMounted: true,
    canvasOnMap: false,
    pointerEvents: "none",
    shared: true,
    passThrough: true,
    hoveredComponent: null
  });
  assert.strictEqual(
    fixture.sharedRenderer.options.tolerance,
    6,
    "shared arc/node Canvas did not receive the serialized tolerance"
  );

  fixture.sharedRenderer._drawFirst = {
    layer: fixture.arcLayers.pt_calsim3_arc_00001
  };
  fixture.activeLayers.add(fixture.sharedRenderer);
  fixture.map.fire("layeradd", {layer: fixture.sharedRenderer});
  stats = api.stats();
  assert.strictEqual(stats.renderer.canvasOnMap, true);
  assert.strictEqual(stats.renderer.pointerEvents, "none");
  assert.strictEqual(stats.renderer.passThrough, true);
  assert.strictEqual(
    stats.interaction.panes.find((pane) => pane.name === "pane_calsim3")
      .pointerEvents,
    "",
    "CalSim containment must not rewrite the pane's prior pointer-event value"
  );

  const backgroundEvent = {
    sourceTarget: fixture.map,
    originalEvent: {
      type: "mousemove",
      clientX: 140,
      clientY: 180,
      target: fixture.container
    }
  };
  fixture.sharedRenderer.hoverCandidate =
    fixture.arcLayers.pt_calsim3_arc_00001;
  fixture.map.fire("mousemove", backgroundEvent);
  assert.strictEqual(
    fixture.sharedRenderer.mouseMoveCalls,
    1,
    "background map movement must reach the shared CalSim hit loop"
  );
  assert.strictEqual(
    fixture.container.classList.contains("pt-calsim3-path-hover"),
    true,
    "a CalSim hit must expose its cursor state on the existing map container"
  );
  fixture.map.fire("click", {
    sourceTarget: fixture.map,
    originalEvent: {
      type: "click",
      clientX: 140,
      clientY: 180,
      target: fixture.container
    }
  });
  assert.strictEqual(
    fixture.sharedRenderer.clickCalls,
    1,
    "background map clicks must reach the shared CalSim hit loop"
  );

  const unrelatedPointLayer = {kind: "reference-point"};
  const unrelatedTooltip = {_source: unrelatedPointLayer};
  const unrelatedPopup = {_source: unrelatedPointLayer};
  const unrelatedTooltipCloseCount = fixture.map.closedTooltips.length;
  const unrelatedPopupCloseCount = fixture.map.closedPopups.length;
  const unrelatedMoveDomEvent = {
    type: "mousemove",
    clientX: 140,
    clientY: 180,
    target: {className: "leaflet-interactive"}
  };
  const unrelatedClickDomEvent = {
    type: "click",
    clientX: 140,
    clientY: 180,
    target: {className: "leaflet-interactive"}
  };
  fixture.map.fire("tooltipopen", {tooltip: unrelatedTooltip});
  fixture.map.fire("popupopen", {popup: unrelatedPopup});
  assert.strictEqual(
    api.interactionState().tooltipOwner,
    "unrelated-layer"
  );
  assert.strictEqual(
    api.interactionState().popupOwner,
    "unrelated-layer"
  );
  fixture.map.fire("mousemove", {
    propagatedFrom: unrelatedPointLayer,
    originalEvent: unrelatedMoveDomEvent
  });
  fixture.map.fire("mousemove", {
    sourceTarget: fixture.map,
    originalEvent: unrelatedMoveDomEvent
  });
  fixture.map.fire("click", {
    propagatedFrom: unrelatedPointLayer,
    originalEvent: unrelatedClickDomEvent
  });
  fixture.map.fire("click", {
    sourceTarget: fixture.map,
    originalEvent: unrelatedClickDomEvent
  });
  assert.strictEqual(
    fixture.sharedRenderer.mouseMoveCalls,
    1,
    "an unrelated feature hover must retain priority over CalSim"
  );
  assert.strictEqual(
    fixture.sharedRenderer.clickCalls,
    1,
    "an unrelated feature click must retain priority over CalSim"
  );
  assert.strictEqual(
    fixture.map.closedTooltips.length,
    unrelatedTooltipCloseCount,
    "CalSim event routing must not close unrelated tooltips"
  );
  assert.strictEqual(
    fixture.map.closedPopups.length,
    unrelatedPopupCloseCount,
    "CalSim event routing must not close unrelated popups"
  );
  assert.strictEqual(
    fixture.container.classList.contains("pt-calsim3-path-hover"),
    false,
    "leaving a CalSim hit must clear the map cursor class"
  );

  fixture.sharedRenderer.hoverCandidate =
    fixture.nodeLayers.pt_calsim3_node_00001;
  fixture.map._ptMeasureInteractionActive = true;
  fixture.container.classList.add("pt-measure-active");
  fixture.map.fire("mousemove", backgroundEvent);
  assert.strictEqual(
    fixture.sharedRenderer.mouseMoveCalls,
    1,
    "Measure mode must suppress forwarded CalSim hit testing"
  );
  fixture.map._ptMeasureInteractionActive = false;
  fixture.container.classList.remove("pt-measure-active");
  assert.strictEqual(api.interactionState().measure.mapFlag, false);
  assert.strictEqual(api.interactionState().measure.containerClass, false);

  const clickLayer = makeClusterClickLayer(fixture);
  fixture.setZoom(8);
  fixture.clusterGroup.fire("clusterclick", {layer: clickLayer});
  assert.strictEqual(clickLayer.zoomToBoundsCalls, 1);
  assert.strictEqual(clickLayer.spiderfyCalls, 0);

  fixture.setZoom(9);
  fixture.clusterGroup.fire("clusterclick", {layer: clickLayer});
  assert.strictEqual(clickLayer.zoomToBoundsCalls, 1);
  assert.strictEqual(clickLayer.spiderfyCalls, 1);
  assert.strictEqual(api.stats().spiderfied, true);

  api.apply({
    showArcs: true,
    showNodes: true,
    arcTypes: ["Return"],
    nodeGroups: ["Storage / Reservoir"]
  });
  stats = api.stats();
  assert.strictEqual(stats.shownArcRecords, 5);
  assert.strictEqual(stats.actualArcObjectsInGroup, 5);
  assert.strictEqual(stats.shownNodeRecords, 1);
  assert.strictEqual(stats.registeredNodeMarkers, 1);
  assert.deepStrictEqual(stats.selectedArcTypes, ["Return"]);
  assert.deepStrictEqual(stats.selectedNodeGroups, ["Storage / Reservoir"]);
  assert.ok(
    !fixture.groupMembers.has(fixture.arcLayers.pt_calsim3_arc_00001),
    "filtered arc retained an interactive path"
  );
  assert.ok(
    fixture.groupMembers.has(fixture.arcLayers.pt_calsim3_arc_00002),
    "selected arc type was removed"
  );
  assert.ok(
    !fixture.clusterMembers.has(fixture.nodeLayers.pt_calsim3_node_00001),
    "filtered node retained cluster membership"
  );
  assert.ok(
    fixture.clusterMembers.has(fixture.nodeLayers.pt_calsim3_node_00002),
    "selected node group was removed"
  );

  api.apply({
    showArcs: false,
    showNodes: false,
    arcTypes: [],
    nodeGroups: []
  });
  stats = api.stats();
  assert.strictEqual(stats.shownArcRecords, 0);
  assert.strictEqual(stats.shownNodeRecords, 0);
  assert.strictEqual(stats.actualArcObjectsInGroup, 0);
  assert.strictEqual(stats.registeredNodeMarkers, 0);
  assert.ok(
    !fixture.groupMembers.has(fixture.clusterGroup),
    "Show Nodes off retained the cluster root"
  );

  api.reset();
  stats = api.stats();
  assert.strictEqual(stats.shownArcRecords, 10);
  assert.strictEqual(stats.shownNodeRecords, 3);
  assert.strictEqual(stats.actualArcObjectsInGroup, 10);
  assert.strictEqual(stats.registeredNodeMarkers, 3);
  assert.strictEqual(stats.showArcs, true);
  assert.strictEqual(stats.showNodes, true);
  assert.strictEqual(stats.hoverArcs, true);
  assert.strictEqual(stats.hoverNodes, true);

  const autoRunsBefore = stats.diagnostics.autoApplyRunCount;
  const filterRunsBefore = stats.diagnostics.filterApplyCount;
  const moveCallsBeforeAuto = fixture.map.fitBoundsCalls.length +
    fixture.map.setViewCalls.length;
  api.scheduleApply({
    showArcs: true,
    showNodes: true,
    arcTypes: ["Channel"],
    nodeGroups: ["Conveyance"]
  });
  api.scheduleApply({
    showArcs: true,
    showNodes: true,
    arcTypes: ["Return"],
    nodeGroups: ["Return flow"]
  });
  assert.strictEqual(api.stats().pendingAsyncFilterWork, true);
  await delay(20);
  stats = api.stats();
  assert.deepStrictEqual(stats.selectedArcTypes, ["Return"]);
  assert.deepStrictEqual(stats.selectedNodeGroups, ["Return flow"]);
  assert.strictEqual(
    stats.diagnostics.autoApplyRunCount,
    autoRunsBefore + 1,
    "one rapid gesture batch must produce one Live apply"
  );
  assert.strictEqual(
    stats.diagnostics.filterApplyCount,
    filterRunsBefore + 1,
    "the superseded Live selection must never reconcile"
  );
  assert.strictEqual(stats.pendingAsyncFilterWork, false);
  assert.strictEqual(
    fixture.map.fitBoundsCalls.length + fixture.map.setViewCalls.length,
    moveCallsBeforeAuto,
    "Live filtering must not move the map"
  );
  assert.ok(stats.diagnostics.lastApplyDurationMs >= 0);
  assert.ok(
    stats.diagnostics.lastArcAddCount +
      stats.diagnostics.lastArcRemoveCount > 0
  );

  api.setLiveApply(false);
  const manualStateBefore = api.stats();
  api.scheduleApply({
    showArcs: false,
    showNodes: false,
    arcTypes: [],
    nodeGroups: []
  });
  await delay(20);
  stats = api.stats();
  assert.strictEqual(stats.autoApply, false);
  assert.strictEqual(stats.uiDirty, true);
  assert.strictEqual(stats.pendingAsyncFilterWork, false);
  assert.deepStrictEqual(
    stats.selectedArcTypes,
    manualStateBefore.selectedArcTypes,
    "Live apply off must leave selection pending"
  );
  api.apply({
    showArcs: false,
    showNodes: false,
    arcTypes: [],
    nodeGroups: []
  });
  stats = api.stats();
  assert.strictEqual(stats.uiDirty, false);
  assert.strictEqual(stats.shownArcRecords, 0);
  assert.strictEqual(stats.shownNodeRecords, 0);
  api.setLiveApply(true);
  api.reset();

  const arcTooltip = {
    _source: fixture.arcLayers.pt_calsim3_arc_00001
  };
  fixture.map.fire("tooltipopen", {tooltip: arcTooltip});
  api.setHover("arcs", false);
  assert.strictEqual(
    fixture.map.closedTooltips.at(-1),
    arcTooltip,
    "disabling arc hover must close the current arc tooltip"
  );

  const suppressedTooltip = {
    _source: fixture.arcLayers.pt_calsim3_arc_00002
  };
  fixture.map.fire("tooltipopen", {tooltip: suppressedTooltip});
  assert.strictEqual(
    fixture.map.closedTooltips.at(-1),
    suppressedTooltip,
    "disabled arc hover must suppress later tooltips"
  );

  const popup = {
    _source: fixture.arcLayers.pt_calsim3_arc_00001
  };
  const popupCloseCount = fixture.map.closedPopups.length;
  fixture.map.fire("popupopen", {popup});
  assert.strictEqual(
    fixture.map.closedPopups.length,
    popupCloseCount,
    "popup click must remain available while arc hover is off"
  );

  api.setHover("nodes", false);
  const suppressedNodeTooltip = {
    _source: fixture.nodeLayers.pt_calsim3_node_00002
  };
  fixture.map.fire("tooltipopen", {tooltip: suppressedNodeTooltip});
  assert.strictEqual(
    fixture.map.closedTooltips.at(-1),
    suppressedNodeTooltip,
    "disabled node hover must suppress later tooltips"
  );
  const nodePopup = {
    _source: fixture.nodeLayers.pt_calsim3_node_00002
  };
  const nodePopupCloseCount = fixture.map.closedPopups.length;
  fixture.map.fire("popupopen", {popup: nodePopup});
  assert.strictEqual(
    fixture.map.closedPopups.length,
    nodePopupCloseCount,
    "popup click must remain available while node hover is off"
  );
  api.setHover("nodes", true);
  fixture.map.fire("popupopen", {popup});

  const nodeTooltip = {
    _source: fixture.nodeLayers.pt_calsim3_node_00001
  };
  fixture.map.fire("tooltipopen", {tooltip: nodeTooltip});
  fixture.clusterGroup._spiderfied = clickLayer;
  fixture.map._ptMeasureInteractionActive = true;
  fixture.map.fire("pt:measureinteractionchange", {active: true});
  assert.strictEqual(fixture.map.closedTooltips.at(-1), nodeTooltip);
  assert.strictEqual(fixture.map.closedPopups.at(-1), popup);
  assert.strictEqual(fixture.clusterGroup.unspiderfyCalls > 0, true);
  fixture.map._ptMeasureInteractionActive = false;

  assert.strictEqual(api.search("c_alpha")[0].id, "C_ALPHA");
  assert.strictEqual(api.search("from_alpha")[0].id, "C_ALPHA");
  assert.strictEqual(api.search("to_alpha")[0].id, "C_ALPHA");
  assert.strictEqual(api.search("alpha canal")[0].kind, "arc");
  assert.strictEqual(api.search("node_alpha")[0].kind, "node");
  assert.strictEqual(api.search("conveyance junction")[0].id, "NODE_ALPHA");
  assert.strictEqual(api.search("alpha river")[0].id, "NODE_ALPHA");
  assert.strictEqual(api.search("finder comment alpha")[0].id, "NODE_ALPHA");
  assert.strictEqual(
    api.search("arc").length,
    9,
    "Finder must retain every match internally"
  );
  assert.strictEqual(api.finderPage("arc", 12).showing, 9);

  api.apply({
    showArcs: true,
    showNodes: true,
    arcTypes: ["Return"],
    nodeGroups: ["Storage / Reservoir"]
  });
  const alpha = api.search("c_alpha")[0];
  const fitCountBeforeFilteredFocus = fixture.map.fitBoundsCalls.length;
  assert.deepStrictEqual(
    api.focus(alpha.key),
    {focused: false, reason: "filtered"},
    "filtered Finder selection must not change the map"
  );
  assert.strictEqual(
    fixture.map.fitBoundsCalls.length,
    fitCountBeforeFilteredFocus
  );
  const shown = api.showResult(alpha.key);
  assert.strictEqual(shown.focused, true);
  assert.deepStrictEqual(
    api.stats().selectedArcTypes,
    ["Channel", "Return"],
    "Show result must explicitly add, not replace, the filtered arc type"
  );
  await delay(100);
  assert.strictEqual(fixture.map.fitBoundsCalls.length > 0, true);
  assert.strictEqual(
    fixture.arcLayers.pt_calsim3_arc_00001.broughtToFront,
    true
  );
  assert.strictEqual(
    fixture.arcLayers.pt_calsim3_arc_00001.popupCalls,
    1,
    "Finder selection should open the retained popup"
  );
  const cancellableArcKey = "arc:pt_calsim3_arc_00002";
  assert.strictEqual(api.focus(cancellableArcKey).focused, true);
  fixture.map.fire("movestart");
  await delay(100);
  assert.strictEqual(
    fixture.arcLayers.pt_calsim3_arc_00002.popupCalls,
    0,
    "movement must invalidate pending Finder focus work"
  );

  const node = api.search("node_alpha")[0];
  assert.deepStrictEqual(
    api.focus(node.key),
    {focused: false, reason: "filtered"}
  );
  api.showResult(node.key);
  await delay(100);
  assert.strictEqual(fixture.map.setViewCalls.at(-1).zoom >= 9, true);
  assert.strictEqual(
    fixture.nodeLayers.pt_calsim3_node_00001.popupCalls,
    1
  );
  assertClose(
    fixture.nodeLayers.pt_calsim3_node_00001.radius,
    (4.5 * 0.58) + 2.2,
    "Finder highlight must scale from the compact displayed radius"
  );
  fixture.map.fire("movestart");
  assertClose(
    fixture.nodeLayers.pt_calsim3_node_00001.radius,
    4.5 * 0.58,
    "movement must restore the compact displayed radius"
  );

  const preservedBeforeOff = api.stats();
  const retainedNodeRadiusCallsBeforeOff = Object.values(
    fixture.nodeLayers
  ).reduce((total, layer) => total + layer.setRadiusCalls, 0);
  fixture.activeLayers.delete(fixture.groupRoot);
  fixture.map.fire("overlayremove", {
    name: "Channels – CalSim3.0",
    layer: fixture.groupRoot
  });
  stats = api.stats();
  assert.strictEqual(stats.active, false);
  assert.strictEqual(stats.renderer.canvasOnMap, false);
  assert.strictEqual(
    fixture.container.classList.contains("pt-calsim3-path-hover"),
    false,
    "CalSim off must clear its existing-container cursor state"
  );
  assert.strictEqual(stats.interaction.dragging.enabled, true);
  assert.strictEqual(stats.interaction.boxZoom.enabled, true);
  assert.strictEqual(
    stats.interaction.panes.every(
      (pane) => pane.pointerEvents === ""
    ),
    true,
    "CalSim off must not leave any shared pane pointer state dirty"
  );
  fixture.activeLayers.add(fixture.groupRoot);
  fixture.activeLayers.add(fixture.sharedRenderer);
  fixture.map.fire("layeradd", {layer: fixture.sharedRenderer});
  fixture.map.fire("overlayadd", {
    name: "Channels – CalSim3.0",
    layer: fixture.groupRoot
  });
  stats = api.stats();
  assert.strictEqual(stats.active, true);
  assert.deepStrictEqual(
    stats.selectedArcTypes,
    preservedBeforeOff.selectedArcTypes,
    "ordinary off/on must preserve applied arc filters"
  );
  assert.deepStrictEqual(
    stats.selectedNodeGroups,
    preservedBeforeOff.selectedNodeGroups,
    "ordinary off/on must preserve applied node filters"
  );
  assert.strictEqual(
    Object.values(fixture.nodeLayers).reduce(
      (total, layer) => total + layer.setRadiusCalls,
      0
    ),
    retainedNodeRadiusCallsBeforeOff,
    "ordinary off/on at one zoom tier must not redundantly resize markers"
  );
  Object.keys(retainedNodeReferences).forEach((key) => {
    assert.strictEqual(
      fixture.nodeLayers[key],
      retainedNodeReferences[key],
      `ordinary off/on replaced retained marker ${key}`
    );
  });
  assert.strictEqual(stats.nodeRadius.appliedTier, "compact");

  const mapListenerCount = fixture.map.totalListenerCount();
  const clusterListenerCount = fixture.clusterGroup.totalListenerCount();
  for (let cycle = 0; cycle < 4; cycle += 1) {
    fixture.activeLayers.delete(fixture.groupRoot);
    fixture.map.fire("overlayremove", {
      name: "Channels – CalSim3.0",
      layer: fixture.groupRoot
    });
    fixture.activeLayers.add(fixture.groupRoot);
    fixture.activeLayers.add(fixture.sharedRenderer);
    fixture.map.fire("layeradd", {layer: fixture.sharedRenderer});
    fixture.map.fire("overlayadd", {
      name: "Channels – CalSim3.0",
      layer: fixture.groupRoot
    });
  }
  assert.strictEqual(
    fixture.map.totalListenerCount(),
    mapListenerCount,
    "ordinary CalSim off/on must not accumulate map listeners"
  );
  assert.strictEqual(
    fixture.clusterGroup.totalListenerCount(),
    clusterListenerCount,
    "ordinary CalSim off/on must not accumulate cluster listeners"
  );

  const firstController = window.BRIM_CALSIM3_LOCAL;
  install(fixture, 1);
  assert.strictEqual(
    fixture.map.totalListenerCount(),
    mapListenerCount,
    "controller reinstall must replace rather than duplicate map listeners"
  );
  assert.strictEqual(
    fixture.clusterGroup.listenerCount("clusterclick"),
    1,
    "controller reinstall must replace rather than duplicate cluster listeners"
  );
  assert.strictEqual(
    firstController.stats().listenerCount,
    0,
    "replaced controller must release its listeners"
  );
  assert.strictEqual(
    window.BRIM_CALSIM3_LOCAL.stats().actualArcObjectsInGroup,
    10,
    "replacement must restore the complete retained arc object set"
  );
  assert.strictEqual(
    window.BRIM_CALSIM3_LOCAL.stats().registeredNodeMarkers,
    3,
    "replacement must restore the complete retained node object set"
  );

  window.BRIM_CALSIM3_LOCAL.destroy(true);
  assert.strictEqual(fixture.map.totalListenerCount(), 0);
  assert.strictEqual(fixture.clusterGroup.totalListenerCount(), 0);

  const noDuplicateFixture = makeFixture();
  install(noDuplicateFixture, 0);
  assert.strictEqual(
    noDuplicateFixture.clusterGroup.listenerCount("clusterclick"),
    0,
    "current no-duplicate cache must use native zoom-9 declustering"
  );
  stats = window.BRIM_CALSIM3_LOCAL.stats();
  assert.strictEqual(stats.uniqueExactCoordinateLocations, 3);
  assert.strictEqual(stats.exactDuplicateLocationCount, 0);
  assert.strictEqual(stats.registeredNodeMarkers, 3);
  window.BRIM_CALSIM3_LOCAL.destroy(true);

  const activationFixture = makeFixture({initialActive: false});
  activationFixture.setZoom(10);
  const activationNodeReferences = Object.assign(
    {},
    activationFixture.nodeLayers
  );
  install(activationFixture, 0);
  const activationApi = window.BRIM_CALSIM3_LOCAL;
  stats = activationApi.stats();
  assert.strictEqual(stats.active, false);
  assert.strictEqual(stats.nodeRadius.requestedTier, "medium");
  assert.strictEqual(stats.nodeRadius.appliedTier, null);
  assert.strictEqual(stats.nodeRadius.targetMinimum, 4.5 * 0.76);
  assert.strictEqual(stats.nodeRadius.effectiveMinimum, 4.5);
  assert.strictEqual(stats.diagnostics.nodeRadiusInactiveSkipCount, 1);
  assert.strictEqual(
    Object.values(activationFixture.nodeLayers).reduce(
      (total, layer) => total + layer.setRadiusCalls,
      0
    ),
    0,
    "inactive install must not resize retained node markers"
  );
  activationFixture.activeLayers.add(activationFixture.groupRoot);
  activationFixture.map.fire("overlayadd", {
    name: "Channels – CalSim3.0",
    layer: activationFixture.groupRoot
  });
  stats = activationApi.stats();
  assert.strictEqual(stats.active, true);
  assert.strictEqual(stats.nodeRadius.appliedTier, "medium");
  assert.strictEqual(stats.shownArcRecords, 10);
  assert.strictEqual(stats.shownNodeRecords, 3);
  assertClose(
    activationFixture.nodeLayers.pt_calsim3_node_00001.radius,
    4.5 * 0.76,
    "first activation must use the current zoom-10 tier"
  );
  activationFixture.activeLayers.delete(activationFixture.groupRoot);
  activationFixture.map.fire("overlayremove", {
    name: "Channels – CalSim3.0",
    layer: activationFixture.groupRoot
  });
  const activationCallsBeforeInactiveZoom = Object.values(
    activationFixture.nodeLayers
  ).reduce((total, layer) => total + layer.setRadiusCalls, 0);
  activationFixture.setZoom(11);
  activationFixture.map.fire("zoomend");
  stats = activationApi.stats();
  assert.strictEqual(stats.nodeRadius.appliedTier, null);
  assert.strictEqual(stats.nodeRadius.targetMinimum, 4.5);
  assert.strictEqual(stats.nodeRadius.effectiveMinimum, 4.5 * 0.76);
  assert.strictEqual(
    Object.values(activationFixture.nodeLayers).reduce(
      (total, layer) => total + layer.setRadiusCalls,
      0
    ),
    activationCallsBeforeInactiveZoom,
    "zoom while off must not leave radius work on hidden markers"
  );
  activationFixture.activeLayers.add(activationFixture.groupRoot);
  activationFixture.map.fire("overlayadd", {
    name: "Channels – CalSim3.0",
    layer: activationFixture.groupRoot
  });
  stats = activationApi.stats();
  assert.strictEqual(stats.nodeRadius.appliedTier, "full");
  assertClose(
    activationFixture.nodeLayers.pt_calsim3_node_00001.radius,
    4.5,
    "repeat activation must use the current zoom-11 tier"
  );
  Object.keys(activationNodeReferences).forEach((key) => {
    assert.strictEqual(
      activationFixture.nodeLayers[key],
      activationNodeReferences[key],
      `activation lifecycle replaced retained marker ${key}`
    );
  });
  activationApi.destroy(true);

  const labelCapFixture = makeFixture({arcCount: 210});
  labelCapFixture.setZoom(11);
  install(labelCapFixture, 0, {
    labelCap: 20,
    labelNodeCap: 12,
    labelArcCap: 8
  });
  let labelCapApi = window.BRIM_CALSIM3_LOCAL;
  labelCapApi.setLabels(true);
  await delay(30);
  stats = labelCapApi.stats();
  assert.strictEqual(stats.labels.visibleCount, 20);
  assert.strictEqual(stats.labels.cap, 20);
  assert.ok(stats.diagnostics.labelCappedCount > 0);
  assert.strictEqual(labelCapFixture.visibleLabelRoots().length, 1);
  const firstLabelRoot = labelCapFixture.visibleLabelRoots()[0];
  const labelMapListenerCount = labelCapFixture.map.totalListenerCount();
  install(labelCapFixture, 0, {
    labelCap: 20,
    labelNodeCap: 12,
    labelArcCap: 8
  });
  await delay(30);
  assert.strictEqual(
    labelCapApi.stats().listenerCount,
    0,
    "replaced label controller must release its listeners"
  );
  labelCapApi = window.BRIM_CALSIM3_LOCAL;
  stats = labelCapApi.stats();
  assert.strictEqual(stats.labels.requested, true);
  assert.strictEqual(stats.labels.visibleCount, 20);
  assert.strictEqual(labelCapFixture.visibleLabelRoots().length, 1);
  assert.notStrictEqual(
    labelCapFixture.visibleLabelRoots()[0],
    firstLabelRoot,
    "controller replacement retained a stale label root"
  );
  assert.strictEqual(
    labelCapFixture.map.totalListenerCount(),
    labelMapListenerCount,
    "controller replacement accumulated label/map listeners"
  );

  labelCapFixture.activeLayers.delete(labelCapFixture.groupRoot);
  labelCapFixture.map.fire("overlayremove", {
    name: "Channels – CalSim3.0",
    layer: labelCapFixture.groupRoot
  });
  stats = labelCapApi.stats();
  assert.strictEqual(stats.active, false);
  assert.strictEqual(stats.labels.requested, false);
  assert.strictEqual(stats.labels.visibleCount, 0);
  assert.strictEqual(
    labelCapFixture.labelInput.checked,
    true,
    "controller must not synchronously click the companion during main removal"
  );
  labelCapFixture.labelInput.click();
  assert.strictEqual(labelCapFixture.labelInput.checked, false);
  assert.strictEqual(labelCapFixture.visibleLabelRoots().length, 0);
  labelCapFixture.activeLayers.add(labelCapFixture.groupRoot);
  labelCapFixture.map.fire("overlayadd", {
    name: "Channels – CalSim3.0",
    layer: labelCapFixture.groupRoot
  });
  assert.strictEqual(labelCapApi.stats().labels.requested, false);
  assert.strictEqual(labelCapFixture.visibleLabelRoots().length, 0);
  labelCapApi.destroy(true);

  const broadFixture = makeFixture({arcCount: 130, americanCount: 118});
  install(broadFixture, 0);
  const broadApi = window.BRIM_CALSIM3_LOCAL;
  const mapMutationCountBeforeFinder =
    broadFixture.groupRoot.addLayerCalls +
    broadFixture.groupRoot.removeLayerCalls +
    broadFixture.clusterGroup.addLayerCalls +
    broadFixture.clusterGroup.removeLayerCalls;
  const americanMatches = broadApi.search("American");
  assert.strictEqual(
    americanMatches.length,
    118,
    "broad Finder query must retain every internal match"
  );
  assert.deepStrictEqual(
    americanMatches.slice(0, 5).map((record) => record.id),
    ["AMERICAN", "AMERICAN_PREFIX_2", "ARC_3", "ARC_5", "ARC_4"],
    "Finder ranking must be exact ID, ID prefix, exact endpoint/name, then prefix"
  );
  assert.deepStrictEqual(
    broadApi.search("American").slice(0, 5).map((record) => record.key),
    americanMatches.slice(0, 5).map((record) => record.key),
    "Finder ranking must be stable across repeated queries"
  );
  let finderPage = broadApi.finderPage("American", 12);
  assert.strictEqual(finderPage.total, 118);
  assert.strictEqual(finderPage.showing, 12);
  assert.strictEqual(finderPage.canShowAll, true);
  finderPage = broadApi.finderPage("American", 24);
  assert.strictEqual(finderPage.showing, 24, "Show more must add one batch");
  finderPage = broadApi.finderPage("American", 200);
  assert.strictEqual(finderPage.showing, 118, "Show all must reach every match");
  assert.strictEqual(
    broadFixture.groupRoot.addLayerCalls +
      broadFixture.groupRoot.removeLayerCalls +
      broadFixture.clusterGroup.addLayerCalls +
      broadFixture.clusterGroup.removeLayerCalls,
    mapMutationCountBeforeFinder,
    "Finder expansion must be DOM/index-only and never touch map membership"
  );
  broadApi.destroy(true);

  console.log("CalSim3 Network Explorer controller fixture: PASS");
}

run().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
