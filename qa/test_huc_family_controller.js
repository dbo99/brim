const assert = require("assert");
const fs = require("fs");
const path = require("path");

const sourcePath = path.join(
  __dirname,
  "..",
  "03_functions",
  "js",
  "brim_huc_theme_control.js"
);
const source = fs.readFileSync(sourcePath, "utf8");
const installController = new Function(`return (${source}\n);`)();
const toolsSource = fs.readFileSync(
  path.join(
    __dirname,
    "..",
    "03_functions",
    "js",
    "leaflet_tools_adddata_panel.js"
  ),
  "utf8"
);
const coreHelpersSource = fs.readFileSync(
  path.join(
    __dirname,
    "..",
    "03_functions",
    "leaflet_core_helpers.r"
  ),
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
    return Object.keys(this._events).reduce(
      (total, name) => total + this._events[name].length,
      0
    );
  };
  return target;
}

const elementsById = {};
const elementsByClass = {};
const documentListeners = {};

function mockElement(tagName = "div") {
  const listeners = {};
  const element = {
    tagName: tagName.toUpperCase(),
    id: "",
    className: "",
    style: {},
    textContent: "",
    value: "",
    innerHTML: "",
    addEventListener(name, handler) {
      listeners[name] = listeners[name] || [];
      listeners[name].push(handler);
    },
    dispatchEvent(event) {
      event.target = event.target || this;
      (listeners[event.type] || []).slice().forEach((handler) => handler(event));
    },
    querySelector(selector) {
      if (selector === ".pt-huc-theme-legend-close") return null;
      return elementsByClass[selector.replace(/^\./, "")] || null;
    },
    querySelectorAll() {
      return [];
    },
    appendChild(child) {
      if (child.id) elementsById[child.id] = child;
      String(child.className).split(/\s+/).filter(Boolean).forEach((className) => {
        elementsByClass[className] = child;
      });

      if (String(child.className).includes("pt-huc-theme-control")) {
        const select = mockElement("select");
        select.id = "pt-huc-theme-select";
        select.value = "none";
        elementsById[select.id] = select;
        const status = mockElement("div");
        status.id = "pt-huc-theme-status";
        elementsById[status.id] = status;
      }
    },
    remove() {
      if (this.id && elementsById[this.id] === this) {
        delete elementsById[this.id];
      }
      Object.keys(elementsByClass).forEach((className) => {
        if (elementsByClass[className] === this) {
          delete elementsByClass[className];
        }
      });
    },
    click() {}
  };
  return element;
}

const mapContainer = mockElement("div");

global.document = {
  body: {appendChild() {}},
  createElement: (tagName) => mockElement(tagName),
  getElementById: (id) => elementsById[id] || null,
  querySelector: (selector) => (
    elementsByClass[selector.replace(/^\./, "")] || null
  ),
  addEventListener(name, handler) {
    documentListeners[name] = documentListeners[name] || [];
    documentListeners[name].push(handler);
  },
  removeEventListener(name, handler) {
    documentListeners[name] = (documentListeners[name] || []).filter(
      (candidate) => candidate !== handler
    );
  },
  dispatchEvent(event) {
    (documentListeners[event.type] || []).slice().forEach(
      (handler) => handler(event)
    );
  }
};

class MockCanvas {
  constructor(level) {
    this._brimHucLevel = level;
  }
}
class MockSvg {}

global.L = {
  Canvas: MockCanvas,
  SVG: MockSvg,
  canvas: () => new MockCanvas("huc-family"),
  DomEvent: {
    disableClickPropagation() {},
    disableScrollPropagation() {}
  }
};

global.window = {
  location: {search: ""},
  performance: {now: () => Date.now()},
  requestAnimationFrame: (callback) => setTimeout(() => callback(Date.now()), 0),
  cancelAnimationFrame: (id) => clearTimeout(id),
  setTimeout,
  clearTimeout,
  URLSearchParams,
  BRIM_ENABLE_HUC_PROFILE: false
};

function makeLayer(level, code, colors) {
  const layer = evented({
    options: {
      layerId: `${level}_${code}`,
      renderer: new MockCanvas(level),
      fillColor: "#FFFFFF",
      fillOpacity: 0
    },
    styleCalls: [],
    setStyle(style) {
      this.styleCalls.push(Object.assign({}, style));
      Object.assign(this.options, style);
    }
  });
  layer._tooltip = {
    _source: layer,
    _map: null,
    content: `<b>${level.toUpperCase()} ${code}</b><br/>rich fixture summary`
  };
  layer._popup = {
    _source: layer,
    _map: null,
    content: `<b>${level.toUpperCase()} ${code}</b><br/>rich fixture popup`
  };
  layer.getTooltip = () => layer._tooltip;
  layer.getPopup = () => layer._popup;
  layer.isTooltipOpen = () => !!layer._tooltip._map;
  layer.isPopupOpen = () => !!layer._popup._map;
  layer.openTooltip = function() {
    if (!this._map || this._tooltip._map) return this;
    this._tooltip._map = this._map;
    this._map._visibleTooltips.add(this._tooltip);
    this._map.fire("tooltipopen", {tooltip: this._tooltip});
    return this;
  };
  layer.closeTooltip = function() {
    if (!this._map || !this._tooltip._map) return this;
    this._tooltip._map = null;
    this._map._visibleTooltips.delete(this._tooltip);
    this._map.fire("tooltipclose", {tooltip: this._tooltip});
    return this;
  };
  return layer;
}

function makeFixture() {
  const groups = {
    huc10: "Basins – HUC10 – PRISM/BCMv8 summaries",
    huc12: "Basins – HUC12 – PRISM/BCMv8 summaries"
  };
  const layers = {
    huc10: [
      makeLayer("huc10", "a"),
      makeLayer("huc10", "b"),
      makeLayer("huc10", "c")
    ],
    huc12: [
      makeLayer("huc12", "a"),
      makeLayer("huc12", "b"),
      makeLayer("huc12", "c"),
      makeLayer("huc12", "d")
    ]
  };
  const roots = {huc10: evented({}), huc12: evented({})};
  const unrelated = makeLayer("unrelated", "popup");
  const byGroup = {};
  const groupContainers = {};
  const lookup = [];
  const levels = [];
  const legends = [];

  Object.keys(layers).forEach((levelName) => {
    byGroup[groups[levelName]] = {};
    groupContainers[groups[levelName]] = roots[levelName];
    layers[levelName].forEach((layer, index) => {
      byGroup[groups[levelName]][String(index + 1)] = layer;
      lookup.push({
        layer_id: layer.options.layerId,
        huc_layer: levelName,
        huc_label: levelName.toUpperCase(),
        ppt_in: index % 2 ? "#2255AA" : "#77AADD",
        ppt_kaf: index % 2 ? "#332288" : "#9988CC",
        rech_in: index % 2 ? "#228833" : "#88CC88",
        rech_kaf: index % 2 ? "#117744" : "#66AA66"
      });
    });
    levels.push({
      huc_layer: levelName,
      huc_label: levelName.toUpperCase(),
      group_name: groups[levelName],
      expected_count: layers[levelName].length
    });
    ["ppt_in", "ppt_kaf", "rech_in", "rech_kaf"].forEach((theme) => {
      legends.push({
        huc_layer: levelName,
        theme,
        legend: {
          title: `${levelName.toUpperCase()} ${theme}`,
          rows: [{color: "#2255AA", label: "fixture bin"}],
          note: "fixture"
        }
      });
    });
  });

  return {
    groups,
    layers,
    roots,
    unrelated,
    data: {lookup, levels, legends},
    layerManager: {
      _byGroup: byGroup,
      _groupContainers: groupContainers
    }
  };
}

function makeMap(fixture) {
  const visible = new Set();
  const map = evented({
    layerManager: fixture.layerManager,
    _layers: {},
    _visibleTooltips: new Set(),
    _visiblePopups: new Set(),
    _currentPopup: null,
    popupOpenCount: 0
  });

  function attachPopupBinding(layer) {
    layer._map = map;
    layer.on("click", () => {
      if (map._currentPopup && map._currentPopup !== layer._popup) {
        map.closePopup(map._currentPopup);
      }
      layer._popup._map = map;
      map._currentPopup = layer._popup;
      map._visiblePopups.add(layer._popup);
      map.popupOpenCount += 1;
      map.fire("popupopen", {popup: layer._popup});
    });
  }

  Object.keys(fixture.layers).forEach((levelName) => {
    fixture.layers[levelName].forEach(attachPopupBinding);
  });
  attachPopupBinding(fixture.unrelated);
  map.getContainer = () => mapContainer;
  map.hasLayer = (layer) => visible.has(layer);
  map.removeLayer = (layer) => {
    visible.delete(layer);
    return map;
  };
  map.closePopup = (popup) => {
    const target = popup || map._currentPopup;
    if (!target || !target._map) return map;
    target._map = null;
    map._visiblePopups.delete(target);
    if (map._currentPopup === target) map._currentPopup = null;
    map.fire("popupclose", {popup: target});
    return map;
  };
  map.closeTooltip = (tooltip) => {
    if (tooltip && tooltip._source) tooltip._source.closeTooltip();
  };
  map.activate = function(levelName) {
    visible.add(fixture.roots[levelName]);
    visible.add(fixture.layers[levelName][0].options.renderer);
    this.fire("overlayadd", {
      name: fixture.groups[levelName],
      layer: fixture.roots[levelName]
    });
  };
  map.deactivate = function(levelName) {
    visible.delete(fixture.roots[levelName]);
    this.fire("overlayremove", {
      name: fixture.groups[levelName],
      layer: fixture.roots[levelName]
    });
  };
  return map;
}

function waitFor(predicate, timeoutMs = 1500) {
  const started = Date.now();
  return new Promise((resolve, reject) => {
    function poll() {
      if (predicate()) return resolve();
      if (Date.now() - started > timeoutMs) {
        return reject(new Error("Timed out waiting for HUC controller"));
      }
      setTimeout(poll, 5);
    }
    poll();
  });
}

async function run() {
  assert.ok(
    !source.includes("map.eachLayer"),
    "HUC controller must not restore a map-wide layer scan"
  );
  assert.match(
    toolsSource,
    /map\.fire\('pt:measureinteractionchange', \{active: active\}\)/,
    "Measure state changes must publish the tooltip lifecycle event"
  );
  assert.match(
    coreHelpersSource,
    /window\.addEventListener\('pointerup'[\s\S]*map\.fire\('pt:marqueezoomstart'[\s\S]*map\.once\('moveend'[\s\S]*setTimeout\(function\(\)[\s\S]*map\.fire\('pt:marqueezoomend'[\s\S]*map\.fitBounds/,
    "fixture ordering must match BRIM's pointerup/start/fitBounds/deferred-end lifecycle"
  );

  const fixture = makeFixture();
  const map = makeMap(fixture);
  installController.call(map, mapContainer, {}, fixture.data);

  const installedMapListenerCount = map.totalListenerCount();
  const installedDocumentListenerCounts = {};
  ["click", "pointerdown", "mousedown", "keydown"].forEach((eventName) => {
    installedDocumentListenerCounts[eventName] =
      (documentListeners[eventName] || []).length;
  });
  assert.strictEqual(fixture.roots.huc10.listenerCount("mouseout"), 1);
  map.activate("huc10");
  fixture.layers.huc10[0].openTooltip();
  assert.strictEqual(map._visibleTooltips.size, 1);
  map.fire("boxzoomstart");
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().diagnostics.popupSuppressed,
    true
  );

  // A rebuild must destroy the old lifecycle owner before installing another.
  installController.call(map, mapContainer, {}, fixture.data);
  assert.strictEqual(map._visibleTooltips.size, 0);
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().diagnostics.popupSuppressed,
    false,
    "controller rebuild must clear prior popup suppression"
  );
  assert.strictEqual(map.totalListenerCount(), installedMapListenerCount);
  Object.keys(installedDocumentListenerCounts).forEach((eventName) => {
    assert.strictEqual(
      (documentListeners[eventName] || []).length,
      installedDocumentListenerCounts[eventName]
    );
  });
  assert.strictEqual(fixture.roots.huc10.listenerCount("mouseout"), 1);
  assert.strictEqual(
    fixture.layers.huc10[0].options.renderer,
    fixture.layers.huc12[0].options.renderer,
    "controller rebuild should adopt the already-mounted shared Canvas"
  );
  map.deactivate("huc10");

  let stats = window.BRIM_HUC_LOCAL.stats();
  assert.deepStrictEqual(stats.activeLevels, []);
  assert.strictEqual(stats.retainedFeatureObjects, 7);
  assert.strictEqual(stats.levels.huc10.registeredCount, 3);
  assert.strictEqual(stats.levels.huc12.registeredCount, 4);
  assert.strictEqual(stats.levels.huc10.renderer, "Canvas");
  assert.strictEqual(
    fixture.layers.huc10[0].options.renderer,
    fixture.layers.huc12[0].options.renderer,
    "all HUC levels should share one map-local Canvas renderer"
  );
  assert.strictEqual(stats.diagnostics.mapScanCount, 0);

  map.fire("overlayadd", {name: "Unrelated layer", layer: {}});
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.diagnostics.ignoredOverlayEvents, 1);
  assert.strictEqual(stats.diagnostics.styleOperationCount, 0);

  map.activate("huc10");
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.deepStrictEqual(stats.activeLevels, ["huc10"]);
  assert.strictEqual(stats.currentTheme, "none");
  assert.strictEqual(stats.diagnostics.optionPrimeCount, 3);

  const huc10A = fixture.layers.huc10[0];
  const huc10B = fixture.layers.huc10[1];

  huc10A.openTooltip();
  huc10B.openTooltip();
  assert.strictEqual(
    map._visibleTooltips.size,
    1,
    "hover A then B must leave only one visible tooltip"
  );
  assert.strictEqual(huc10A.isTooltipOpen(), false);
  assert.strictEqual(huc10B.isTooltipOpen(), true);
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().diagnostics.tooltipReplacementCount,
    1
  );
  assert.ok(
    huc10B.getTooltip().content.includes("rich fixture summary"),
    "the lifecycle guard must preserve rich tooltip content"
  );

  fixture.roots.huc10.fire("mouseout", {layer: huc10B});
  assert.strictEqual(
    map._visibleTooltips.size,
    0,
    "polygon mouseout must promptly close the current tooltip"
  );

  huc10A.openTooltip();
  map.fire("movestart");
  assert.strictEqual(map._visibleTooltips.size, 0);
  huc10A.openTooltip();
  map.fire("zoomstart");
  assert.strictEqual(map._visibleTooltips.size, 0);

  ["pt-main-layer-clear-btn", "pt-clear-all-btn"].forEach((clearClassOrId) => {
    huc10A.openTooltip();
    document.dispatchEvent({
      type: "click",
      target: {
        closest(selector) {
          if (!selector.includes(clearClassOrId)) return null;
          return clearClassOrId.startsWith("pt-clear") ?
            {id: clearClassOrId, className: ""} :
            {id: "", className: clearClassOrId};
        }
      }
    });
    assert.strictEqual(
      map._visibleTooltips.size,
      0,
      `${clearClassOrId} must close the current tooltip`
    );
  });

  huc10A.openTooltip();
  map.fire("pt:measureinteractionchange", {active: true});
  assert.strictEqual(
    map._visibleTooltips.size,
    0,
    "measure-mode activation must close the current tooltip"
  );

  huc10A.openTooltip();
  const popupCountBeforeClick = map.popupOpenCount;
  huc10A.fire("click");
  assert.strictEqual(map.popupOpenCount, popupCountBeforeClick + 1);
  assert.strictEqual(
    huc10A.isPopupOpen(),
    true,
    "an ordinary HUC click must open the rich bound popup"
  );
  assert.ok(huc10A.getPopup().content.includes("rich fixture popup"));
  assert.strictEqual(
    map._visibleTooltips.size,
    0,
    "opening the HUC popup should close hover without blocking the click"
  );
  map.closePopup(huc10A.getPopup());

  // BRIM custom-marquee order: pointerup fires pt:start, the browser click
  // reaches Leaflet (preclick then click), and pt:end arrives on a zero task.
  huc10A.openTooltip();
  map.fire("pt:marqueezoomstart");
  assert.strictEqual(map._visibleTooltips.size, 0);
  huc10A.fire("mouseup", {originalEvent: {type: "pointerup"}});
  huc10A.fire("preclick", {originalEvent: {type: "click"}});
  huc10A.fire("click", {originalEvent: {type: "click"}});
  assert.strictEqual(
    huc10A.isPopupOpen(),
    false,
    "the release click during BRIM marquee zoom must not leave a HUC popup"
  );
  map.fire("pt:marqueezoomend");
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().diagnostics.popupSuppressed,
    true,
    "suppression must survive through the marquee end event"
  );
  await new Promise((resolve) => setTimeout(resolve, 5));
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().diagnostics.popupSuppressed,
    false
  );

  // Native Leaflet 1.3.1 order: boxzoomend fires inside mouseup before the
  // browser dispatches its subsequent preclick/click pair.
  map.fire("boxzoomstart");
  huc10A.fire("mouseup", {originalEvent: {type: "mouseup"}});
  map.fire("boxzoomend");
  huc10A.fire("preclick", {originalEvent: {type: "click"}});
  huc10A.fire("click", {originalEvent: {type: "click"}});
  assert.strictEqual(
    huc10A.isPopupOpen(),
    false,
    "native box-zoom release click must remain suppressed after boxzoomend"
  );
  await new Promise((resolve) => setTimeout(resolve, 5));

  // Also tolerate a plugin/event ordering that emits the click before its end.
  map.fire("boxzoomstart");
  huc10A.fire("mouseup", {originalEvent: {type: "mouseup"}});
  huc10A.fire("preclick", {originalEvent: {type: "click"}});
  huc10A.fire("click", {originalEvent: {type: "click"}});
  map.fire("boxzoomend");
  assert.strictEqual(huc10A.isPopupOpen(), false);
  await new Promise((resolve) => setTimeout(resolve, 5));

  for (let marquee = 0; marquee < 3; marquee += 1) {
    map.fire("pt:marqueezoomstart");
    huc10A.fire("preclick", {originalEvent: {type: "click"}});
    huc10A.fire("click", {originalEvent: {type: "click"}});
    map.fire("pt:marqueezoomend");
    await new Promise((resolve) => setTimeout(resolve, 5));
    assert.strictEqual(
      window.BRIM_HUC_LOCAL.stats().diagnostics.popupSuppressed,
      false,
      "repeated marquee zoom must not leave popup interaction disabled"
    );
    assert.strictEqual(huc10A.isPopupOpen(), false);
  }

  map.fire("boxzoomstart");
  document.dispatchEvent({type: "keydown", key: "Escape"});
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().diagnostics.popupSuppressed,
    false,
    "Escape must clear suppression when native box zoom is canceled"
  );
  map.fire("boxzoomstart");
  document.dispatchEvent({type: "pointerdown"});
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().diagnostics.popupSuppressed,
    false,
    "a later pointer gesture must recover from a missing zoom-end event"
  );

  map.fire("pt:marqueezoomstart");
  fixture.unrelated.fire("preclick", {originalEvent: {type: "click"}});
  fixture.unrelated.fire("click", {originalEvent: {type: "click"}});
  assert.strictEqual(
    fixture.unrelated.isPopupOpen(),
    true,
    "unrelated layer popups must not be suppressed"
  );
  map.fire("pt:marqueezoomend");
  await new Promise((resolve) => setTimeout(resolve, 5));
  map.closePopup(fixture.unrelated.getPopup());

  huc10A.fire("click", {originalEvent: {type: "click"}});
  assert.strictEqual(
    huc10A.isPopupOpen(),
    true,
    "the first deliberate HUC click after marquee zoom must work"
  );
  map.closePopup(huc10A.getPopup());

  huc10A.openTooltip();
  assert.strictEqual(
    huc10A.isTooltipOpen(),
    true,
    "HUC hover must remain functional after popup suppression"
  );
  fixture.roots.huc10.fire("mouseout", {layer: huc10A});
  assert.strictEqual(huc10A.isTooltipOpen(), false);

  assert.strictEqual(window.BRIM_HUC_LOCAL.setTheme("ppt_in"), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  fixture.layers.huc10.forEach((layer) => {
    assert.strictEqual(layer.styleCalls.length, 1);
    assert.strictEqual(layer.options.fillOpacity, 0.58);
  });
  fixture.layers.huc12.forEach((layer) => {
    assert.strictEqual(layer.styleCalls.length, 0);
  });

  assert.strictEqual(window.BRIM_HUC_LOCAL.setTheme("ppt_in"), false);
  fixture.layers.huc10.forEach((layer) => {
    assert.strictEqual(layer.styleCalls.length, 1);
  });

  huc10A.openTooltip();
  map.activate("huc12");
  assert.strictEqual(
    map._visibleTooltips.size,
    0,
    "switching HUC levels must dismiss the previous hover"
  );
  fixture.layers.huc12.forEach((layer) => {
    assert.strictEqual(layer.styleCalls.length, 0);
    assert.strictEqual(layer.options.fillOpacity, 0.58);
  });
  assert.ok(
    elementsByClass["pt-huc-theme-legend"].innerHTML.includes(
      "Multiple HUC levels are visible"
    ),
    "multi-level legend note should remain available"
  );

  window.BRIM_HUC_LOCAL.setTheme("rech_in");
  window.BRIM_HUC_LOCAL.setTheme("ppt_kaf");
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  fixture.layers.huc10.concat(fixture.layers.huc12).forEach((layer) => {
    const record = fixture.data.lookup.find(
      (row) => row.layer_id === layer.options.layerId
    );
    assert.strictEqual(layer.options.fillColor, record.ppt_kaf);
    assert.strictEqual(layer.options.fillOpacity, 0.58);
  });
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.ok(stats.diagnostics.canceledStyleJobs >= 1);
  assert.strictEqual(stats.diagnostics.mapScanCount, 0);

  huc10A.openTooltip();
  map.deactivate("huc10");
  assert.strictEqual(
    map._visibleTooltips.size,
    0,
    "HUC overlayremove must close the current tooltip"
  );
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().levels.huc12.rendererMounted,
    true,
    "shared Canvas must remain while another HUC level is active"
  );
  fixture.layers.huc12[0].openTooltip();
  map.deactivate("huc12");
  assert.strictEqual(map._visibleTooltips.size, 0);
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.deepStrictEqual(stats.activeLevels, []);
  assert.strictEqual(stats.currentTheme, "none");
  assert.strictEqual(
    stats.levels.huc12.rendererMounted,
    false,
    "last HUC off must remove the empty shared Canvas"
  );

  map.activate("huc12");
  fixture.layers.huc12.forEach((layer) => {
    assert.strictEqual(layer.options.fillColor, "#FFFFFF");
    assert.strictEqual(layer.options.fillOpacity, 0);
  });

  const listenerCountBeforeCycles = map.totalListenerCount();
  const rootListenerCountBeforeCycles =
    fixture.roots.huc12.totalListenerCount();
  const documentListenerCountBeforeCycles = Object.keys(
    documentListeners
  ).reduce(
    (total, eventName) => total + documentListeners[eventName].length,
    0
  );
  for (let cycle = 0; cycle < 3; cycle += 1) {
    map.deactivate("huc12");
    map.activate("huc12");
  }
  assert.strictEqual(
    map.totalListenerCount(),
    listenerCountBeforeCycles,
    "HUC off/on cycles must not accumulate map listeners"
  );
  assert.strictEqual(
    fixture.roots.huc12.totalListenerCount(),
    rootListenerCountBeforeCycles,
    "HUC off/on cycles must not accumulate root listeners"
  );
  assert.strictEqual(
    Object.keys(documentListeners).reduce(
      (total, eventName) => total + documentListeners[eventName].length,
      0
    ),
    documentListenerCountBeforeCycles,
    "HUC off/on cycles must not accumulate document listeners"
  );

  fixture.layers.huc12[0].openTooltip();
  const finalController = window.BRIM_HUC_LOCAL;
  finalController.destroy();
  assert.strictEqual(
    map._visibleTooltips.size,
    0,
    "controller destruction must close the current tooltip"
  );
  assert.strictEqual(map.totalListenerCount(), 0);
  assert.strictEqual(fixture.roots.huc10.totalListenerCount(), 0);
  assert.strictEqual(fixture.roots.huc12.totalListenerCount(), 0);
  assert.strictEqual(documentListeners.click.length, 0);
  assert.strictEqual(documentListeners.pointerdown.length, 0);
  assert.strictEqual(documentListeners.mousedown.length, 0);
  assert.strictEqual(documentListeners.keydown.length, 0);
  assert.strictEqual(finalController.stats().diagnostics.destroyed, true);

  console.log("HUC family controller fixture: PASS");
}

run().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
