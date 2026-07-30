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

const documentListeners = {};

function classListFor(element) {
  return {
    add(...names) {
      const current = new Set(String(element.className).split(/\s+/).filter(Boolean));
      names.forEach((name) => current.add(name));
      element.className = Array.from(current).join(" ");
    },
    remove(...names) {
      const remove = new Set(names);
      element.className = String(element.className)
        .split(/\s+/)
        .filter((name) => name && !remove.has(name))
        .join(" ");
    },
    contains(name) {
      return String(element.className).split(/\s+/).includes(name);
    }
  };
}

function matches(element, selector) {
  if (!element || !selector) return false;
  if (selector.startsWith(".")) {
    return element.classList.contains(selector.slice(1));
  }
  if (selector.startsWith("#")) return element.id === selector.slice(1);
  return element.tagName.toLowerCase() === selector.toLowerCase();
}

function mockElement(tagName = "div", className = "") {
  const listeners = {};
  const children = [];
  const selectorMap = {};
  let html = "";
  const element = {
    tagName: tagName.toUpperCase(),
    id: "",
    className,
    style: {},
    hidden: false,
    parentNode: null,
    children,
    classList: null,
    textContent: "",
    value: "",
    addEventListener(name, handler) {
      listeners[name] = listeners[name] || [];
      listeners[name].push(handler);
    },
    removeEventListener(name, handler) {
      listeners[name] = (listeners[name] || []).filter(
        (candidate) => candidate !== handler
      );
    },
    dispatchEvent(event) {
      event.target = event.target || this;
      (listeners[event.type] || []).slice().forEach((handler) => handler(event));
    },
    querySelector(selector) {
      if (selectorMap[selector]) return selectorMap[selector];
      for (const child of children) {
        if (matches(child, selector)) return child;
        const nested = child.querySelector(selector);
        if (nested) return nested;
      }
      return null;
    },
    querySelectorAll(selector) {
      let found = [];
      children.forEach((child) => {
        if (selector === "*" || matches(child, selector)) found.push(child);
        found = found.concat(child.querySelectorAll(selector));
      });
      return found;
    },
    appendChild(child) {
      if (child.parentNode) child.parentNode.removeChild(child);
      children.push(child);
      child.parentNode = this;
      return child;
    },
    removeChild(child) {
      const index = children.indexOf(child);
      if (index >= 0) children.splice(index, 1);
      child.parentNode = null;
      return child;
    },
    remove() {
      if (this.parentNode) this.parentNode.removeChild(this);
    },
    contains(candidate) {
      if (candidate === this) return true;
      return children.some((child) => child.contains(candidate));
    },
    closest(selector) {
      let node = this;
      while (node) {
        if (matches(node, selector)) return node;
        node = node.parentNode;
      }
      return null;
    },
    setAttribute(name, value) {
      this[name] = String(value);
    },
    getAttribute(name) {
      return this[name] === undefined ? null : this[name];
    },
    click() {}
  };
  element.classList = classListFor(element);
  Object.defineProperty(element, "innerHTML", {
    get() {
      return html;
    },
    set(value) {
      html = String(value);
      while (children.length) element.removeChild(children[0]);
      if (
        element.classList.contains("pt-huc-theme-card") &&
        !selectorMap[".pt-huc-theme-select"]
      ) {
        const head = mockElement(
          "div",
          "pt-huc-theme-head pt-map-card-handle"
        );
        const dock = mockElement(
          "button",
          "pt-huc-theme-dock pt-map-card-dock"
        );
        const close = mockElement(
          "button",
          "pt-huc-theme-close pt-map-legend-close"
        );
        const themeSelect = mockElement("select", "pt-huc-theme-select");
        themeSelect.id = "pt-huc-theme-select";
        const minimum = mockElement("input", "pt-huc-theme-minimum");
        minimum.id = "pt-huc-theme-minimum";
        minimum.value = "0";
        const minimumValue = mockElement(
          "span",
          "pt-huc-theme-filter-value"
        );
        minimumValue.textContent = "0%";
        const visibleCount = mockElement(
          "div",
          "pt-huc-theme-visible-count"
        );
        const context = mockElement("div", "pt-huc-theme-context");
        const legend = mockElement("div", "pt-huc-theme-legend-body");
        const status = mockElement("div", "pt-huc-theme-status");
        head.appendChild(dock);
        head.appendChild(close);
        element.appendChild(head);
        element.appendChild(themeSelect);
        element.appendChild(minimumValue);
        element.appendChild(minimum);
        element.appendChild(visibleCount);
        element.appendChild(context);
        element.appendChild(legend);
        element.appendChild(status);
        selectorMap[".pt-huc-theme-head"] = head;
        selectorMap[".pt-huc-theme-dock"] = dock;
        selectorMap[".pt-huc-theme-close"] = close;
        selectorMap[".pt-huc-theme-select"] = themeSelect;
        selectorMap[".pt-huc-theme-minimum"] = minimum;
        selectorMap[".pt-huc-theme-filter-value"] = minimumValue;
        selectorMap[".pt-huc-theme-visible-count"] = visibleCount;
        selectorMap[".pt-huc-theme-context"] = context;
        selectorMap[".pt-huc-theme-legend-body"] = legend;
        selectorMap[".pt-huc-theme-status"] = status;
      }
    }
  });
  return element;
}

const documentById = {};
const mapContainer = mockElement("div", "leaflet-container");
const documentHead = mockElement("head");
const originalHeadAppend = documentHead.appendChild.bind(documentHead);
documentHead.appendChild = function(child) {
  originalHeadAppend(child);
  if (child.id) documentById[child.id] = child;
  return child;
};

global.document = {
  head: documentHead,
  body: mockElement("body"),
  createElement: (tagName) => mockElement(tagName),
  getElementById: (id) => documentById[id] || null,
  querySelector: (selector) => mapContainer.querySelector(selector),
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
    this._layers = {};
  }
}
class MockSvg {}

const detachableStates = [];
let leafletStampSequence = 0;
global.L = {
  Canvas: MockCanvas,
  SVG: MockSvg,
  canvas: () => new MockCanvas("huc-family"),
  stamp(layer) {
    if (!layer._leaflet_id) layer._leaflet_id = ++leafletStampSequence;
    return layer._leaflet_id;
  },
  DomUtil: {
    create(tagName, className) {
      return mockElement(tagName, className);
    }
  },
  DomEvent: {
    disableClickPropagation() {},
    disableScrollPropagation() {}
  },
  control(options) {
    return {
      options,
      onAdd: null,
      _container: null,
      addTo(map) {
        this._container = this.onAdd(map);
        map.getContainer().appendChild(this._container);
        return this;
      },
      remove() {
        if (this._container) this._container.remove();
        this._container = null;
      }
    };
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
  BRIM_ENABLE_HUC_PROFILE: false,
  BRIM: {
    legendCloseout: {
      actionsHtml() {
        return '<span class="pt-map-card-actions"></span>';
      },
      makeDetachable(options) {
        const state = {
          floating: false,
          dockCalls: 0,
          destroyed: false,
          dock() {
            this.floating = false;
            this.dockCalls += 1;
          },
          destroy(removeCard) {
            this.destroyed = true;
            if (removeCard && options.card) options.card.remove();
          }
        };
        options.card._detachableState = state;
        detachableStates.push(state);
        return state;
      },
      scheduleLayout() {}
    }
  }
};

function makeLayer(level, code) {
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

function registerRendererLayer(layer) {
  const renderer = layer && layer.options && layer.options.renderer;
  if (renderer && renderer._layers) {
    renderer._layers[String(L.stamp(layer))] = layer;
  }
}

function unregisterRendererLayer(layer) {
  const renderer = layer && layer.options && layer.options.renderer;
  if (renderer && renderer._layers) {
    delete renderer._layers[String(L.stamp(layer))];
  }
}

function makeFixture(options = {}) {
  const percentValues = options.percentValues || {
    huc8: [0, 70, 100],
    huc12: [null, 50, 75, 90]
  };
  const groups = Object.keys(percentValues).reduce((result, levelName) => {
    result[levelName] =
      `Basins – ${levelName.toUpperCase()} – PRISM/BCMv8 summaries`;
    return result;
  }, {});
  const codeForIndex = (index) => (
    index < 26 ? String.fromCharCode(97 + index) : String(index + 1)
  );
  const layers = Object.keys(percentValues).reduce((result, levelName) => {
    result[levelName] = percentValues[levelName].map(
      (value, index) => makeLayer(levelName, codeForIndex(index))
    );
    return result;
  }, {});
  function makeRoot() {
    const members = new Set();
    return evented({
      _map: null,
      addLayer(layer) {
        members.add(layer);
        if (this._map) {
          layer._map = this._map;
          registerRendererLayer(layer);
        }
        return this;
      },
      removeLayer(layer) {
        members.delete(layer);
        if (layer._map === this._map) {
          unregisterRendererLayer(layer);
          layer._map = null;
        }
        return this;
      },
      hasLayer(layer) {
        return members.has(layer);
      },
      memberCount() {
        return members.size;
      },
      members() {
        return Array.from(members);
      }
    });
  }
  const roots = Object.keys(layers).reduce((result, levelName) => {
    result[levelName] = makeRoot();
    return result;
  }, {});
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
      roots[levelName].addLayer(layer);
      byGroup[groups[levelName]][String(index + 1)] = layer;
      lookup.push({
        layer_id: layer.options.layerId,
        huc_layer: levelName,
        huc_label: levelName.toUpperCase(),
        percent_blm: percentValues[levelName][index],
        blm_pct: index % 2 ? "#673A7B" : "#DFC7E5",
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
    ["blm_pct", "ppt_in", "ppt_kaf", "rech_in", "rech_kaf"].forEach((theme) => {
      legends.push({
        huc_layer: levelName,
        theme,
        legend: {
          title: `${levelName.toUpperCase()} ${theme}`,
          rows: [{
            color: theme === "blm_pct" ? "#DFC7E5" : "#2255AA",
            label: "fixture bin",
            count: layers[levelName].length
          }]
        }
      });
    });
  });

  return {
    groups,
    layers,
    roots,
    unrelated,
    data: {
      default_theme: "none",
      themes: [
        {id: "none", label: "Boundaries only (no fill)"},
        {id: "blm_pct", label: "BLM-managed land — %"},
        {id: "ppt_in", label: "PRISM precip - in/yr"},
        {id: "ppt_kaf", label: "PRISM precip - kaf/yr"},
        {id: "rech_in", label: "BCMv8 recharge - in/yr"},
        {id: "rech_kaf", label: "BCMv8 recharge - kaf/yr"}
      ],
      lookup,
      levels,
      legends
    },
    layerManager: {
      _byGroup: byGroup,
      _groupContainers: groupContainers
    }
  };
}

function makeMap(fixture, options = {}) {
  const visible = new Set();
  const layerControlInputs = {};
  const map = evented({
    layerManager: fixture.layerManager,
    _layers: {},
    _visibleTooltips: new Set(),
    _visiblePopups: new Set(),
    _currentPopup: null,
    popupOpenCount: 0
  });

  function attachPopupBinding(layer) {
    layer.on("click", () => {
      if (layer._map !== map) return;
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
  fixture.unrelated._map = map;
  map.getContainer = () => mapContainer;
  map.hasLayer = (layer) => visible.has(layer);
  map.removeLayer = (layer) => {
    visible.delete(layer);
    if (layer && layer.options && layer.options.renderer && layer._map) {
      unregisterRendererLayer(layer);
      layer._map = null;
    }
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
    const root = fixture.roots[levelName];
    root._map = map;
    root.members().forEach((layer) => {
      layer._map = map;
      registerRendererLayer(layer);
    });
    visible.add(root);
    visible.add(fixture.layers[levelName][0].options.renderer);
    layerControlInputs[levelName].checked = true;
    this.fire("overlayadd", {
      name: fixture.groups[levelName],
      layer: root
    });
  };
  map.deactivate = function(levelName) {
    const root = fixture.roots[levelName];
    const removalSnapshot = root.members().filter(
      (layer) => layer._map === map
    );
    const finishChildRemoval = () => {
      removalSnapshot.forEach((layer) => {
        if (layer._tooltip && layer._tooltip._map) {
          layer._tooltip._map = null;
          map._visibleTooltips.delete(layer._tooltip);
          map.fire("tooltipclose", {tooltip: layer._tooltip});
        }
        if (layer._popup && layer._popup._map) {
          map.closePopup(layer._popup);
        }
        unregisterRendererLayer(layer);
        layer._map = null;
      });
    };
    visible.delete(root);
    layerControlInputs[levelName].checked = false;
    if (options.earlyOverlayRemove) {
      finishChildRemoval();
      this.fire("overlayremove", {
        name: fixture.groups[levelName],
        layer: root
      });
      root._map = null;
    } else {
      finishChildRemoval();
      root._map = null;
      this.fire("overlayremove", {
        name: fixture.groups[levelName],
        layer: root
      });
    }
  };

  Object.keys(fixture.roots).forEach((levelName) => {
    const input = mockElement(
      "input",
      "leaflet-control-layers-selector"
    );
    input.layerId = L.stamp(fixture.roots[levelName]);
    input.checked = false;
    layerControlInputs[levelName] = input;
    mapContainer.appendChild(input);
  });
  map._layerControlInputs = layerControlInputs;
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

async function waitForHucSettled(timeoutMs = 5000) {
  await waitFor(() => {
    const stats = window.BRIM_HUC_LOCAL.stats();
    return (
      stats.applying === null &&
      stats.stylePending === false &&
      stats.pendingResetLevels.length === 0
    );
  }, timeoutMs);
}

function assertRendererInvariant(fixture, expectedActiveLevels) {
  const stats = window.BRIM_HUC_LOCAL.stats();
  const expected = new Set(expectedActiveLevels);
  let expectedRendererPaths = 0;
  let renderer = null;

  Object.keys(fixture.layers).forEach((levelName) => {
    const levelStats = stats.levels[levelName];
    const active = expected.has(levelName);
    assert.strictEqual(levelStats.checked, active, `${levelName} checked state`);
    assert.strictEqual(levelStats.active, active, `${levelName} controller state`);
    assert.strictEqual(levelStats.mapHasGroup, active, `${levelName} map group`);
    assert.strictEqual(
      levelStats.groupMapAttached,
      active,
      `${levelName} FeatureGroup _map`
    );
    assert.strictEqual(
      levelStats.mountedCount,
      active ? levelStats.memberCount : 0,
      `${levelName} mounted paths`
    );
    assert.strictEqual(
      levelStats.rendererPathCount,
      active ? levelStats.memberCount : 0,
      `${levelName} renderer paths`
    );
    assert.strictEqual(
      fixture.roots[levelName].memberCount(),
      levelStats.memberCount,
      `${levelName} retained membership`
    );
    if (!active) {
      assert.strictEqual(levelStats.stylePending, false);
      assert.strictEqual(levelStats.activationPending, false);
    }
    expectedRendererPaths += active ? levelStats.memberCount : 0;
    const levelRenderer = fixture.layers[levelName][0].options.renderer;
    if (!renderer) renderer = levelRenderer;
    assert.strictEqual(
      levelRenderer,
      renderer,
      "all HUC levels must retain one shared Canvas"
    );
  });

  assert.deepStrictEqual(
    stats.activeLevels.slice().sort(),
    expectedActiveLevels.slice().sort()
  );
  assert.strictEqual(
    Object.keys(renderer._layers).length,
    expectedRendererPaths,
    "shared Canvas path registry must contain only checked HUC levels"
  );
  return stats;
}

async function run() {
  assert.ok(
    !source.includes("map.eachLayer"),
    "HUC controller must not restore a map-wide layer scan"
  );
  assert.ok(
    !source.includes("BRIM_HUC_BLM"),
    "HUC %BLM must not introduce a second controller"
  );
  assert.ok(
    !source.includes("pt-huc-theme-note"),
    "HUC explanatory-note rendering must be removed"
  );
  assert.ok(
    !source.includes("Boundaries only; no thematic legend."),
    "boundaries-only explanatory footnote must be removed"
  );
  assert.ok(
    !source.includes("pt-huc-theme-control"),
    "legacy standalone HUC selector must be removed"
  );
  assert.match(source, /pt-huc-theme-card/);
  assert.match(source, /makeDetachable/);
  assert.match(source, /var deadline = nowMs\(\) \+ 8/);
  assert.match(source, /frameOperations >= 100/);
  assert.match(source, /generation !== styleGeneration/);
  assert.match(source, /levelState\.root\.removeLayer\(layer\)/);
  assert.match(source, /levelState\.root\.addLayer\(layer\)/);
  assert.match(source, /value >= threshold/);
  assert.match(source, /min="0" max="100" step="1" value="0"/);
  assert.match(source, /Loading\\u2026/);
  assert.match(source, /pt-huc-theme-spinner-slot/);
  assert.match(source, /animation:pt-huc-theme-spin/);
  assert.match(source, /rendererPathCount/);
  assert.match(source, /inactiveLevelDetached/);
  assert.match(source, /Promise\.resolve\(\)\.then\(finalizeAfterRemoval\)/);
  assert.ok(
    !source.includes("L.polygon(") && !source.includes("new L.Polygon"),
    "HUC filter must not construct a second polygon population"
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
  let card = mapContainer.querySelector(".pt-huc-theme-card");
  assert.ok(card, "controller must create one unified HUC thematic card");
  assert.strictEqual(
    mapContainer.querySelectorAll(".pt-huc-theme-card").length,
    1
  );
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentTheme, "none");
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentMinimumBlm, 0);
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().cardVisible, false);
  assert.ok(
    card.innerHTML.indexOf('value="none"') <
      card.innerHTML.indexOf('value="blm_pct"'),
    "boundaries-only must be the first HUC selector option"
  );
  assert.ok(card.innerHTML.includes("Boundaries only (no fill)"));
  assert.ok(card.innerHTML.includes("Minimum BLM-managed land"));
  assert.strictEqual(
    mapContainer.querySelectorAll(".pt-huc-theme-control").length,
    0,
    "legacy standalone selector must not remain"
  );
  assert.strictEqual(detachableStates.length, 1);

  const installedMapListenerCount = map.totalListenerCount();
  const installedDocumentListenerCounts = {};
  ["click", "pointerdown", "mousedown", "keydown"].forEach((eventName) => {
    installedDocumentListenerCounts[eventName] =
      (documentListeners[eventName] || []).length;
  });
  assert.strictEqual(fixture.roots.huc8.listenerCount("mouseout"), 1);
  map.activate("huc8");
  fixture.layers.huc8[0].openTooltip();
  assert.strictEqual(map._visibleTooltips.size, 1);
  map.fire("boxzoomstart");
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().diagnostics.popupSuppressed,
    true
  );

  // A rebuild must destroy the old lifecycle owner before installing another.
  const firstDetachableState = detachableStates[0];
  const firstController = window.BRIM_HUC_LOCAL;
  assert.strictEqual(firstController.setMinimumBlmPct(70), true);
  assert.ok(
    firstController.stats().applying,
    "fixture must replace the controller during scheduled filter work"
  );
  installController.call(map, mapContainer, {}, fixture.data);
  assert.strictEqual(firstDetachableState.destroyed, true);
  assert.strictEqual(firstController.stats().diagnostics.destroyed, true);
  assert.strictEqual(
    mapContainer.querySelectorAll(".pt-huc-theme-card").length,
    1,
    "controller replacement must leave exactly one HUC card"
  );
  assert.strictEqual(map._visibleTooltips.size, 0);
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().diagnostics.popupSuppressed,
    false,
    "controller rebuild must clear prior popup suppression"
  );
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentMinimumBlm, 0);
  assert.strictEqual(fixture.roots.huc8.memberCount(), 3);
  assert.strictEqual(map.totalListenerCount(), installedMapListenerCount);
  Object.keys(installedDocumentListenerCounts).forEach((eventName) => {
    assert.strictEqual(
      (documentListeners[eventName] || []).length,
      installedDocumentListenerCounts[eventName]
    );
  });
  assert.strictEqual(fixture.roots.huc8.listenerCount("mouseout"), 1);
  assert.strictEqual(
    fixture.layers.huc8[0].options.renderer,
    fixture.layers.huc12[0].options.renderer,
    "controller rebuild should adopt the already-mounted shared Canvas"
  );
  map.deactivate("huc8");

  let stats = window.BRIM_HUC_LOCAL.stats();
  assert.deepStrictEqual(stats.activeLevels, []);
  assert.strictEqual(stats.retainedFeatureObjects, 7);
  assert.strictEqual(stats.levels.huc8.registeredCount, 3);
  assert.strictEqual(stats.levels.huc12.registeredCount, 4);
  assert.strictEqual(stats.levels.huc8.renderer, "Canvas");
  assert.strictEqual(
    fixture.layers.huc8[0].options.renderer,
    fixture.layers.huc12[0].options.renderer,
    "all HUC levels should share one map-local Canvas renderer"
  );
  assert.strictEqual(stats.diagnostics.mapScanCount, 0);

  map.fire("overlayadd", {name: "Unrelated layer", layer: {}});
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.diagnostics.ignoredOverlayEvents, 1);
  assert.strictEqual(stats.diagnostics.styleOperationCount, 0);

  map.activate("huc8");
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.deepStrictEqual(stats.activeLevels, ["huc8"]);
  assert.strictEqual(stats.currentTheme, "none");
  assert.strictEqual(stats.currentMinimumBlm, 0);
  assert.strictEqual(stats.eligibleCount, 3);
  assert.strictEqual(stats.levels.huc8.memberCount, 3);
  assert.ok(stats.diagnostics.optionPrimeCount >= 3);
  assert.strictEqual(stats.cardVisible, true);
  fixture.layers.huc8.forEach((layer) => {
    assert.strictEqual(layer.options.fillColor, "#FFFFFF");
    assert.strictEqual(layer.options.fillOpacity, 0);
  });
  card = mapContainer.querySelector(".pt-huc-theme-card");
  assert.strictEqual(
    card.querySelector(".pt-huc-theme-select").value,
    "none"
  );
  assert.strictEqual(
    card.querySelector(".pt-huc-theme-minimum").value,
    "0"
  );
  assert.strictEqual(
    card.querySelector(".pt-huc-theme-filter-value").textContent,
    "0%"
  );
  assert.strictEqual(
    card.querySelector(".pt-huc-theme-visible-count").textContent,
    "3 of 3 HUC8 features shown"
  );
  assert.strictEqual(
    card.querySelector(".pt-huc-theme-legend-body").hidden,
    true,
    "boundaries-only must not leave a blank legend/footer"
  );
  assert.ok(
    card.querySelector(".pt-huc-theme-context").textContent.includes("HUC8")
  );
  const primeCountBeforeSameLevel =
    window.BRIM_HUC_LOCAL.stats().diagnostics.optionPrimeCount;
  map.activate("huc8");
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.diagnostics.sameLevelNoops, 1);
  assert.strictEqual(
    stats.diagnostics.optionPrimeCount,
    primeCountBeforeSameLevel,
    "same-level activation must not repeat style priming"
  );

  const huc8A = fixture.layers.huc8[0];
  const huc8B = fixture.layers.huc8[1];

  huc8A.openTooltip();
  huc8B.openTooltip();
  assert.strictEqual(
    map._visibleTooltips.size,
    1,
    "hover A then B must leave only one visible tooltip"
  );
  assert.strictEqual(huc8A.isTooltipOpen(), false);
  assert.strictEqual(huc8B.isTooltipOpen(), true);
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().diagnostics.tooltipReplacementCount,
    1
  );
  assert.ok(
    huc8B.getTooltip().content.includes("rich fixture summary"),
    "the lifecycle guard must preserve rich tooltip content"
  );

  fixture.roots.huc8.fire("mouseout", {layer: huc8B});
  assert.strictEqual(
    map._visibleTooltips.size,
    0,
    "polygon mouseout must promptly close the current tooltip"
  );

  huc8A.openTooltip();
  map.fire("movestart");
  assert.strictEqual(map._visibleTooltips.size, 0);
  huc8A.openTooltip();
  map.fire("zoomstart");
  assert.strictEqual(map._visibleTooltips.size, 0);

  ["pt-main-layer-clear-btn", "pt-clear-all-btn"].forEach((clearClassOrId) => {
    huc8A.openTooltip();
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

  huc8A.openTooltip();
  map.fire("pt:measureinteractionchange", {active: true});
  assert.strictEqual(
    map._visibleTooltips.size,
    0,
    "measure-mode activation must close the current tooltip"
  );

  huc8A.openTooltip();
  const popupCountBeforeClick = map.popupOpenCount;
  huc8A.fire("click");
  assert.strictEqual(map.popupOpenCount, popupCountBeforeClick + 1);
  assert.strictEqual(
    huc8A.isPopupOpen(),
    true,
    "an ordinary HUC click must open the rich bound popup"
  );
  assert.ok(huc8A.getPopup().content.includes("rich fixture popup"));
  assert.strictEqual(
    map._visibleTooltips.size,
    0,
    "opening the HUC popup should close hover without blocking the click"
  );
  map.closePopup(huc8A.getPopup());

  // BRIM custom-marquee order: pointerup fires pt:start, the browser click
  // reaches Leaflet (preclick then click), and pt:end arrives on a zero task.
  huc8A.openTooltip();
  map.fire("pt:marqueezoomstart");
  assert.strictEqual(map._visibleTooltips.size, 0);
  huc8A.fire("mouseup", {originalEvent: {type: "pointerup"}});
  huc8A.fire("preclick", {originalEvent: {type: "click"}});
  huc8A.fire("click", {originalEvent: {type: "click"}});
  assert.strictEqual(
    huc8A.isPopupOpen(),
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
  huc8A.fire("mouseup", {originalEvent: {type: "mouseup"}});
  map.fire("boxzoomend");
  huc8A.fire("preclick", {originalEvent: {type: "click"}});
  huc8A.fire("click", {originalEvent: {type: "click"}});
  assert.strictEqual(
    huc8A.isPopupOpen(),
    false,
    "native box-zoom release click must remain suppressed after boxzoomend"
  );
  await new Promise((resolve) => setTimeout(resolve, 5));

  // Also tolerate a plugin/event ordering that emits the click before its end.
  map.fire("boxzoomstart");
  huc8A.fire("mouseup", {originalEvent: {type: "mouseup"}});
  huc8A.fire("preclick", {originalEvent: {type: "click"}});
  huc8A.fire("click", {originalEvent: {type: "click"}});
  map.fire("boxzoomend");
  assert.strictEqual(huc8A.isPopupOpen(), false);
  await new Promise((resolve) => setTimeout(resolve, 5));

  for (let marquee = 0; marquee < 3; marquee += 1) {
    map.fire("pt:marqueezoomstart");
    huc8A.fire("preclick", {originalEvent: {type: "click"}});
    huc8A.fire("click", {originalEvent: {type: "click"}});
    map.fire("pt:marqueezoomend");
    await new Promise((resolve) => setTimeout(resolve, 5));
    assert.strictEqual(
      window.BRIM_HUC_LOCAL.stats().diagnostics.popupSuppressed,
      false,
      "repeated marquee zoom must not leave popup interaction disabled"
    );
    assert.strictEqual(huc8A.isPopupOpen(), false);
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

  huc8A.fire("click", {originalEvent: {type: "click"}});
  assert.strictEqual(
    huc8A.isPopupOpen(),
    true,
    "the first deliberate HUC click after marquee zoom must work"
  );
  map.closePopup(huc8A.getPopup());

  huc8A.openTooltip();
  assert.strictEqual(
    huc8A.isTooltipOpen(),
    true,
    "HUC hover must remain functional after popup suppression"
  );
  fixture.roots.huc8.fire("mouseout", {layer: huc8A});
  assert.strictEqual(huc8A.isTooltipOpen(), false);

  const sameThresholdNoopsBefore =
    window.BRIM_HUC_LOCAL.stats().diagnostics.sameThresholdNoops;
  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(0), false);
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().diagnostics.sameThresholdNoops,
    sameThresholdNoopsBefore + 1
  );

  huc8A.openTooltip();
  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(1), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(huc8A.isTooltipOpen(), false);
  assert.strictEqual(fixture.roots.huc8.hasLayer(huc8A), false);
  assert.strictEqual(huc8A._map, null);
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().eligibleCount, 2);
  assert.strictEqual(
    card.querySelector(".pt-huc-theme-visible-count").textContent,
    "2 of 3 HUC8 features shown"
  );
  fixture.layers.huc12.forEach((layer) => {
    assert.strictEqual(layer.styleCalls.length, 0);
    assert.strictEqual(fixture.roots.huc12.hasLayer(layer), true);
  });

  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(0), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(fixture.roots.huc8.hasLayer(huc8A), true);
  assert.strictEqual(huc8A._map, map);
  assert.strictEqual(huc8A.options.fillOpacity, 0);

  huc8A.fire("click");
  assert.strictEqual(huc8A.isPopupOpen(), true);
  const minimumSlider = card.querySelector(".pt-huc-theme-minimum");
  minimumSlider.value = "70";
  minimumSlider.dispatchEvent({type: "input"});
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(huc8A.isPopupOpen(), false);
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentTheme, "none");
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentMinimumBlm, 70);
  assert.strictEqual(fixture.roots.huc8.hasLayer(huc8A), false);
  assert.strictEqual(fixture.roots.huc8.hasLayer(huc8B), true);
  assert.strictEqual(fixture.roots.huc8.hasLayer(fixture.layers.huc8[2]), true);
  const popupCountWhileFiltered = map.popupOpenCount;
  huc8A.fire("click");
  assert.strictEqual(
    map.popupOpenCount,
    popupCountWhileFiltered,
    "removed HUC polygon must not remain interactive"
  );
  assert.strictEqual(
    card.querySelector(".pt-huc-theme-filter-value").textContent,
    "70%"
  );

  const canceledBeforeRapidThreshold =
    window.BRIM_HUC_LOCAL.stats().diagnostics.canceledStyleJobs;
  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(50), true);
  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(75), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.currentMinimumBlm, 75);
  assert.strictEqual(stats.eligibleCount, 1);
  assert.ok(
    stats.diagnostics.canceledStyleJobs > canceledBeforeRapidThreshold,
    "rapid threshold input must cancel the obsolete membership job"
  );

  const styleCallsBeforeTheme = fixture.layers.huc8.map(
    (layer) => layer.styleCalls.length
  );
  assert.strictEqual(window.BRIM_HUC_LOCAL.setTheme("ppt_in"), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(
    fixture.layers.huc8[0].styleCalls.length,
    styleCallsBeforeTheme[0]
  );
  assert.strictEqual(
    fixture.layers.huc8[1].styleCalls.length,
    styleCallsBeforeTheme[1]
  );
  assert.strictEqual(
    fixture.layers.huc8[2].styleCalls.length,
    styleCallsBeforeTheme[2] + 1
  );
  assert.strictEqual(fixture.layers.huc8[2].options.fillOpacity, 0.58);

  assert.strictEqual(window.BRIM_HUC_LOCAL.setTheme("ppt_in"), false);

  fixture.layers.huc8[2].openTooltip();
  map.activate("huc12");
  assert.strictEqual(
    map._visibleTooltips.size,
    0,
    "switching HUC levels must dismiss the previous hover"
  );
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentMinimumBlm, 0);
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().levels.huc12.memberCount, 4);
  assert.strictEqual(fixture.roots.huc12.hasLayer(fixture.layers.huc12[0]), true);
  assert.strictEqual(fixture.roots.huc12.hasLayer(fixture.layers.huc12[1]), true);
  assert.strictEqual(fixture.roots.huc12.hasLayer(fixture.layers.huc12[2]), true);
  assert.strictEqual(fixture.roots.huc12.hasLayer(fixture.layers.huc12[3]), true);
  assert.ok(
    !mapContainer.querySelector(".pt-huc-theme-legend-body").innerHTML.includes(
      "Multiple HUC levels are visible"
    ),
    "HUC multi-level explanatory note must be removed"
  );

  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(100), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().eligibleCount, 1);
  assert.strictEqual(fixture.roots.huc8.memberCount(), 1);
  assert.strictEqual(fixture.roots.huc12.memberCount(), 0);

  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(0), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(fixture.roots.huc8.memberCount(), 1);
  assert.strictEqual(fixture.roots.huc12.memberCount(), 4);

  window.BRIM_HUC_LOCAL.setTheme("rech_in");
  map.deactivate("huc12");
  map.activate("huc12");
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentTheme, "none");
  fixture.layers.huc12.forEach((layer) => {
    assert.strictEqual(layer.options.fillColor, "#FFFFFF");
    assert.strictEqual(layer.options.fillOpacity, 0);
  });
  window.BRIM_HUC_LOCAL.setTheme("ppt_kaf");
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  fixture.layers.huc8.forEach((layer) => {
    const record = fixture.data.lookup.find(
      (row) => row.layer_id === layer.options.layerId
    );
    if (fixture.roots.huc8.hasLayer(layer)) {
      assert.strictEqual(layer.options.fillColor, record.ppt_in);
      assert.strictEqual(layer.options.fillOpacity, 0.58);
    } else {
      assert.strictEqual(layer.options.fillOpacity, 0);
    }
  });
  fixture.layers.huc12.forEach((layer) => {
    const record = fixture.data.lookup.find(
      (row) => row.layer_id === layer.options.layerId
    );
    assert.strictEqual(layer.options.fillColor, record.ppt_kaf);
    assert.strictEqual(layer.options.fillOpacity, 0.58);
  });
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.ok(stats.diagnostics.canceledStyleJobs >= 1);
  assert.strictEqual(stats.diagnostics.mapScanCount, 0);

  huc8A.openTooltip();
  map.deactivate("huc8");
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
  assert.strictEqual(stats.levels.huc8.selectedTheme, "none");
  assert.strictEqual(stats.levels.huc12.selectedTheme, "none");
  assert.strictEqual(stats.cardVisible, false);
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
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().cardVisible, true);
  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(70), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentTheme, "none");
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentMinimumBlm, 70);
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().eligibleCount, 2);
  assert.strictEqual(fixture.roots.huc12.memberCount(), 2);

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
    assert.strictEqual(
      window.BRIM_HUC_LOCAL.stats().currentTheme,
      "none",
      "every activation after an actual off must start with boundaries"
    );
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

  assert.strictEqual(window.BRIM_HUC_LOCAL.setTheme("ppt_kaf"), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(70), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  card = mapContainer.querySelector(".pt-huc-theme-card");
  const finalDetachableState = detachableStates[detachableStates.length - 1];
  finalDetachableState.floating = true;
  card.querySelector(".pt-huc-theme-close").dispatchEvent({
    type: "click",
    preventDefault() {},
    stopPropagation() {}
  });
  assert.strictEqual(finalDetachableState.floating, false);
  assert.strictEqual(finalDetachableState.dockCalls, 1);
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().cardVisible, false);
  assert.deepStrictEqual(
    window.BRIM_HUC_LOCAL.stats().activeLevels,
    ["huc12"],
    "card X must not turn off active HUC geometry"
  );
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().currentTheme,
    "ppt_kaf",
    "card X must preserve the active session theme"
  );
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().currentMinimumBlm,
    70,
    "card X must preserve the active session threshold"
  );
  map.deactivate("huc12");
  map.activate("huc12");
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().cardVisible,
    true,
    "ordinary off/on must restore one card"
  );
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().currentTheme,
    "none",
    "ordinary off/on must reset the selected HUC theme"
  );
  assert.strictEqual(
    window.BRIM_HUC_LOCAL.stats().currentMinimumBlm,
    0,
    "ordinary off/on must reset the selected minimum %BLM"
  );
  assert.strictEqual(fixture.roots.huc12.memberCount(), 4);

  fixture.layers.huc12[2].openTooltip();
  const finalController = window.BRIM_HUC_LOCAL;
  finalController.destroy();
  assert.strictEqual(
    map._visibleTooltips.size,
    0,
    "controller destruction must close the current tooltip"
  );
  assert.strictEqual(map.totalListenerCount(), 0);
  assert.strictEqual(fixture.roots.huc8.totalListenerCount(), 0);
  assert.strictEqual(fixture.roots.huc12.totalListenerCount(), 0);
  assert.strictEqual(documentListeners.click.length, 0);
  assert.strictEqual(documentListeners.pointerdown.length, 0);
  assert.strictEqual(documentListeners.mousedown.length, 0);
  assert.strictEqual(documentListeners.keydown.length, 0);
  assert.strictEqual(finalController.stats().diagnostics.destroyed, true);
  assert.strictEqual(finalDetachableState.destroyed, true);
  assert.strictEqual(
    mapContainer.querySelectorAll(".pt-huc-theme-card").length,
    0
  );

  // A/B. Exercise the retained HUC8/HUC12 counts used by the browser gate.
  const restorationFixture = makeFixture({
    percentValues: {
      huc8: Array.from(
        {length: 140},
        (unused, index) => index === 0 ? 70 : 10
      ),
      huc12: Array.from(
        {length: 5065},
        (unused, index) => index < 390 ? 75 : 50
      )
    }
  });
  const restorationMap = makeMap(restorationFixture);
  installController.call(
    restorationMap,
    mapContainer,
    {},
    restorationFixture.data
  );

  restorationMap.activate("huc8");
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.currentTheme, "none");
  assert.strictEqual(stats.currentMinimumBlm, 0);
  assert.strictEqual(stats.levels.huc8.memberCount, 140);
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-visible-count").textContent,
    "140 of 140 HUC8 features shown"
  );
  assert.strictEqual(window.BRIM_HUC_LOCAL.setTheme("blm_pct"), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(70), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(restorationFixture.roots.huc8.memberCount(), 1);

  restorationMap.deactivate("huc8");
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.levels.huc8.selectedTheme, "none");
  assert.strictEqual(stats.levels.huc8.minimumBlmPct, 0);
  assert.strictEqual(stats.levels.huc8.memberCount, 140);
  assert.strictEqual(stats.levels.huc8.eligibleCount, 140);
  assert.strictEqual(stats.levels.huc8.appliedMinimumBlm, 0);
  assert.strictEqual(stats.levels.huc8.lastMembershipResetOperations, 139);
  assert.strictEqual(stats.applying, null);
  assert.strictEqual(stats.stylePending, false);

  restorationMap.activate("huc8");
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.focusedLevel, "huc8");
  assert.strictEqual(stats.currentTheme, "none");
  assert.strictEqual(stats.currentMinimumBlm, 0);
  assert.strictEqual(stats.levels.huc8.memberCount, 140);
  assert.strictEqual(stats.applying, null);
  assert.strictEqual(stats.stylePending, false);
  restorationFixture.layers.huc8.forEach((layer) => {
    assert.strictEqual(layer.options.fillColor, "#FFFFFF");
    assert.strictEqual(layer.options.fillOpacity, 0);
  });
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-select").value,
    "none"
  );
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-minimum").value,
    "0"
  );
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-filter-value").textContent,
    "0%"
  );
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-visible-count").textContent,
    "140 of 140 HUC8 features shown"
  );

  const restoredHuc8 = restorationFixture.layers.huc8[139];
  restoredHuc8.openTooltip();
  assert.strictEqual(restoredHuc8.isTooltipOpen(), true);
  restorationFixture.roots.huc8.fire("mouseout", {layer: restoredHuc8});
  const restoredHuc8PopupCount = restorationMap.popupOpenCount;
  restoredHuc8.fire("click");
  assert.strictEqual(
    restorationMap.popupOpenCount,
    restoredHuc8PopupCount + 1,
    "restored HUC8 polygon must retain its popup binding"
  );
  restorationMap.closePopup(restoredHuc8.getPopup());
  restorationMap.deactivate("huc8");

  restorationMap.activate("huc12");
  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(75), true);
  await waitFor(
    () => window.BRIM_HUC_LOCAL.stats().applying === null,
    5000
  );
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.levels.huc12.memberCount, 390);
  assert.strictEqual(stats.levels.huc12.eligibleCount, 390);
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-visible-count").textContent,
    "390 of 5,065 HUC12 features shown"
  );

  restorationMap.deactivate("huc12");
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.levels.huc12.selectedTheme, "none");
  assert.strictEqual(stats.levels.huc12.minimumBlmPct, 0);
  assert.strictEqual(stats.levels.huc12.memberCount, 5065);
  assert.strictEqual(stats.levels.huc12.eligibleCount, 5065);
  assert.strictEqual(
    stats.levels.huc12.lastMembershipResetOperations,
    4675
  );
  assert.strictEqual(
    stats.diagnostics.membershipResetWhileMountedCount,
    0,
    "membership reset must occur only after the HUC root is detached"
  );

  restorationMap.activate("huc12");
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.currentTheme, "none");
  assert.strictEqual(stats.currentMinimumBlm, 0);
  assert.strictEqual(stats.levels.huc12.memberCount, 5065);
  assert.ok(stats.applying);
  assert.strictEqual(stats.loadingIndicatorVisible, true);
  assert.ok(
    mapContainer.querySelector(".pt-huc-theme-status")
      .innerHTML.includes("Loading\u2026")
  );
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.stylePending, false);
  assert.strictEqual(stats.loadingIndicatorVisible, false);
  assert.ok(
    !mapContainer.querySelector(".pt-huc-theme-status")
      .innerHTML.includes("pt-huc-theme-spinner")
  );
  restorationFixture.layers.huc12.forEach((layer) => {
    assert.strictEqual(layer.options.fillColor, "#FFFFFF");
    assert.strictEqual(layer.options.fillOpacity, 0);
  });
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-minimum").value,
    "0"
  );
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-filter-value").textContent,
    "0%"
  );
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-visible-count").textContent,
    "5,065 of 5,065 HUC12 features shown"
  );
  window.BRIM_HUC_LOCAL.destroy();

  // C/D. Concurrent sessions are isolated; actual off resets only one level.
  const isolationFixture = makeFixture();
  const isolationMap = makeMap(isolationFixture);
  installController.call(
    isolationMap,
    mapContainer,
    {},
    isolationFixture.data
  );
  isolationMap.activate("huc8");
  assert.strictEqual(window.BRIM_HUC_LOCAL.setTheme("blm_pct"), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(70), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  isolationMap.activate("huc12");
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.currentTheme, "none");
  assert.strictEqual(stats.currentMinimumBlm, 0);
  assert.strictEqual(stats.levels.huc12.memberCount, 4);
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-minimum").value,
    "0"
  );
  assert.strictEqual(window.BRIM_HUC_LOCAL.setTheme("ppt_in"), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(50), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.deepStrictEqual(stats.activeLevels, ["huc8", "huc12"]);
  assert.strictEqual(stats.levels.huc8.selectedTheme, "blm_pct");
  assert.strictEqual(stats.levels.huc8.minimumBlmPct, 70);
  assert.strictEqual(stats.levels.huc12.selectedTheme, "ppt_in");
  assert.strictEqual(stats.levels.huc12.minimumBlmPct, 50);

  assert.strictEqual(window.BRIM_HUC_LOCAL.focusLevel("huc8"), true);
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentTheme, "blm_pct");
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentMinimumBlm, 70);
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-select").value,
    "blm_pct"
  );
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-minimum").value,
    "70"
  );
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-filter-value").textContent,
    "70%"
  );
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-visible-count").textContent,
    "2 of 3 HUC8 features shown"
  );
  assert.ok(
    mapContainer.querySelector(".pt-huc-theme-legend-body")
      .innerHTML.includes("HUC8 blm_pct")
  );
  assert.strictEqual(window.BRIM_HUC_LOCAL.focusLevel("huc12"), true);
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentTheme, "ppt_in");
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentMinimumBlm, 50);
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-select").value,
    "ppt_in"
  );
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-minimum").value,
    "50"
  );
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-filter-value").textContent,
    "50%"
  );
  assert.strictEqual(
    mapContainer.querySelector(".pt-huc-theme-visible-count").textContent,
    "3 of 4 HUC12 features shown"
  );
  assert.ok(
    mapContainer.querySelector(".pt-huc-theme-legend-body")
      .innerHTML.includes("HUC12 ppt_in")
  );
  assert.strictEqual(window.BRIM_HUC_LOCAL.focusLevel("huc12"), false);

  assert.strictEqual(window.BRIM_HUC_LOCAL.focusLevel("huc8"), true);
  isolationMap.deactivate("huc8");
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.deepStrictEqual(stats.activeLevels, ["huc12"]);
  assert.strictEqual(stats.focusedLevel, "huc12");
  assert.strictEqual(stats.currentTheme, "ppt_in");
  assert.strictEqual(stats.levels.huc8.selectedTheme, "none");
  assert.strictEqual(stats.levels.huc8.minimumBlmPct, 0);
  assert.strictEqual(stats.levels.huc8.memberCount, 3);
  assert.strictEqual(stats.levels.huc12.selectedTheme, "ppt_in");
  assert.strictEqual(stats.levels.huc12.minimumBlmPct, 50);
  assert.strictEqual(stats.levels.huc12.memberCount, 3);
  isolationFixture.roots.huc12.members().forEach((layer) => {
    const record = isolationFixture.data.lookup.find(
      (row) => row.layer_id === layer.options.layerId
    );
    assert.strictEqual(layer.options.fillColor, record.ppt_in);
  });

  isolationMap.activate("huc8");
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.focusedLevel, "huc8");
  assert.strictEqual(stats.currentTheme, "none");
  assert.strictEqual(stats.currentMinimumBlm, 0);
  assert.strictEqual(stats.levels.huc8.selectedTheme, "none");
  assert.strictEqual(stats.levels.huc8.minimumBlmPct, 0);
  assert.strictEqual(stats.levels.huc8.memberCount, 3);
  assert.strictEqual(stats.levels.huc12.selectedTheme, "ppt_in");
  assert.strictEqual(stats.levels.huc12.minimumBlmPct, 50);
  isolationFixture.layers.huc8.forEach((layer) => {
    assert.strictEqual(layer.options.fillColor, "#FFFFFF");
    assert.strictEqual(layer.options.fillOpacity, 0);
  });

  assert.strictEqual(window.BRIM_HUC_LOCAL.setTheme("blm_pct"), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  assert.strictEqual(window.BRIM_HUC_LOCAL.setMinimumBlmPct(70), true);
  await waitFor(() => window.BRIM_HUC_LOCAL.stats().applying === null);
  isolationMap.deactivate("huc8");
  assert.strictEqual(window.BRIM_HUC_LOCAL.stats().currentMinimumBlm, 50);
  isolationMap.deactivate("huc12");

  isolationMap.activate("huc8");
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.currentTheme, "none");
  assert.strictEqual(stats.currentMinimumBlm, 0);
  assert.strictEqual(stats.levels.huc8.memberCount, 3);
  isolationMap.deactivate("huc8");
  isolationMap.activate("huc12");
  stats = window.BRIM_HUC_LOCAL.stats();
  assert.strictEqual(stats.currentTheme, "none");
  assert.strictEqual(stats.currentMinimumBlm, 0);
  assert.strictEqual(stats.levels.huc12.memberCount, 4);
  assert.strictEqual(
    mapContainer.querySelectorAll(".pt-huc-theme-card").length,
    1
  );
  assert.strictEqual(
    mapContainer.querySelectorAll(".pt-huc-theme-select").length,
    1
  );
  window.BRIM_HUC_LOCAL.destroy();

  /*
   * Renderer-aware regression: model an overlayremove published while the
   * FeatureGroup removal pass still owns its original member snapshot. Adding
   * filtered members in that handler would escape the snapshot and remain in
   * the shared Canvas with their last thematic fill.
   */
  const ghostFixture = makeFixture({
    percentValues: {
      huc2: [0, 100],
      huc4: [0, 50, 100],
      huc6: [0, 25, 75, 100],
      huc8: Array.from({length: 140}, (unused, index) => index),
      huc10: Array.from({length: 60}, (unused, index) => index * 2),
      huc12: Array.from(
        {length: 5065},
        (unused, index) => index < 390 ? 75 : 50
      )
    }
  });
  const ghostMap = makeMap(ghostFixture, {earlyOverlayRemove: true});
  installController.call(
    ghostMap,
    mapContainer,
    {},
    ghostFixture.data
  );
  const ghostController = window.BRIM_HUC_LOCAL;
  const ghostMapListenerCount = ghostMap.totalListenerCount();
  const ghostRootListenerCounts = Object.keys(ghostFixture.roots).reduce(
    (result, levelName) => {
      result[levelName] = ghostFixture.roots[levelName].totalListenerCount();
      return result;
    },
    {}
  );

  ghostMap.activate("huc12");
  stats = assertRendererInvariant(ghostFixture, ["huc12"]);
  assert.strictEqual(stats.loadingIndicatorVisible, true);
  assert.ok(
    mapContainer.querySelector(".pt-huc-theme-status")
      .innerHTML.includes("Loading\u2026")
  );
  await waitForHucSettled();
  assert.strictEqual(
    ghostController.stats().diagnostics.lastApplyOperations,
    0,
    "large-level activation indicator must not create a feature scan"
  );
  assert.strictEqual(ghostController.setMinimumBlmPct(75), true);
  assert.strictEqual(ghostController.setTheme("blm_pct"), true);
  stats = ghostController.stats();
  assert.strictEqual(stats.loadingIndicatorVisible, true);
  await waitForHucSettled();
  stats = assertRendererInvariant(ghostFixture, ["huc12"]);
  assert.strictEqual(stats.levels.huc12.memberCount, 390);
  assert.strictEqual(stats.levels.huc12.rendererPathCount, 390);

  ghostMap.deactivate("huc12");
  stats = assertRendererInvariant(ghostFixture, []);
  assert.strictEqual(stats.levels.huc12.resetPending, true);
  assert.strictEqual(stats.levels.huc12.mountedCount, 0);
  assert.strictEqual(stats.levels.huc12.rendererPathCount, 0);
  assert.strictEqual(stats.loadingIndicatorVisible, false);
  await waitForHucSettled();
  stats = assertRendererInvariant(ghostFixture, []);
  assert.strictEqual(stats.levels.huc12.memberCount, 5065);
  assert.strictEqual(stats.levels.huc12.lastMembershipResetOperations, 4675);
  assert.strictEqual(stats.levels.huc12.lastMembershipResetDetached, true);
  assert.strictEqual(
    stats.levels.huc12.lastMembershipResetRendererPaths,
    0
  );
  console.log(
    "HUC12 detached membership reset:",
    stats.levels.huc12.lastMembershipResetOperations,
    "operations in",
    stats.levels.huc12.lastMembershipResetMs,
    "ms; renderer paths",
    stats.levels.huc12.lastMembershipResetRendererPaths
  );

  ghostMap.activate("huc8");
  await waitForHucSettled();
  stats = assertRendererInvariant(ghostFixture, ["huc8"]);
  assert.strictEqual(stats.levels.huc8.rendererPathCount, 140);
  assert.strictEqual(stats.levels.huc12.rendererPathCount, 0);

  // HUC10 -> HUC8 with a prior PRISM fill.
  ghostMap.deactivate("huc8");
  await waitForHucSettled();
  ghostMap.activate("huc10");
  await waitForHucSettled();
  assert.strictEqual(ghostController.setTheme("ppt_in"), true);
  await waitForHucSettled();
  ghostMap.deactivate("huc10");
  assertRendererInvariant(ghostFixture, []);
  await waitForHucSettled();
  ghostMap.activate("huc8");
  await waitForHucSettled();
  assertRendererInvariant(ghostFixture, ["huc8"]);

  // HUC8 -> HUC12 with a prior BCMv8 fill.
  assert.strictEqual(ghostController.setTheme("rech_in"), true);
  await waitForHucSettled();
  ghostMap.deactivate("huc8");
  assertRendererInvariant(ghostFixture, []);
  await waitForHucSettled();
  ghostMap.activate("huc12");
  assert.strictEqual(ghostController.stats().currentTheme, "none");
  assert.strictEqual(ghostController.stats().currentMinimumBlm, 0);
  await waitForHucSettled();
  assertRendererInvariant(ghostFixture, ["huc12"]);

  // HUC6 -> HUC10.
  ghostMap.deactivate("huc12");
  await waitForHucSettled();
  ghostMap.activate("huc6");
  assert.strictEqual(ghostController.setTheme("blm_pct"), true);
  await waitForHucSettled();
  ghostMap.deactivate("huc6");
  assertRendererInvariant(ghostFixture, []);
  await waitForHucSettled();
  ghostMap.activate("huc10");
  await waitForHucSettled();
  assertRendererInvariant(ghostFixture, ["huc10"]);
  ghostMap.deactivate("huc10");
  await waitForHucSettled();
  for (const levelName of ["huc2", "huc4"]) {
    ghostMap.activate(levelName);
    await waitForHucSettled();
    assertRendererInvariant(ghostFixture, [levelName]);
    ghostMap.deactivate(levelName);
    await waitForHucSettled();
    assertRendererInvariant(ghostFixture, []);
  }

  // Rapid HUC12 -> HUC10 -> HUC8 cancellation in one task.
  ghostMap.activate("huc12");
  assert.strictEqual(ghostController.setTheme("ppt_kaf"), true);
  assert.strictEqual(ghostController.setMinimumBlmPct(75), true);
  ghostMap.deactivate("huc12");
  assertRendererInvariant(ghostFixture, []);
  ghostMap.activate("huc10");
  ghostMap.deactivate("huc10");
  ghostMap.activate("huc8");
  assertRendererInvariant(ghostFixture, ["huc8"]);
  await waitForHucSettled();
  stats = assertRendererInvariant(ghostFixture, ["huc8"]);
  assert.strictEqual(stats.levels.huc12.resetPending, false);
  assert.strictEqual(stats.levels.huc10.resetPending, false);
  assert.strictEqual(stats.levels.huc12.rendererPathCount, 0);
  assert.strictEqual(stats.levels.huc10.rendererPathCount, 0);

  // Concurrent HUC8 + HUC12; removing HUC12 must leave HUC8 intact.
  ghostMap.activate("huc12");
  await waitForHucSettled();
  assertRendererInvariant(ghostFixture, ["huc8", "huc12"]);
  ghostMap.deactivate("huc12");
  stats = assertRendererInvariant(ghostFixture, ["huc8"]);
  assert.strictEqual(stats.levels.huc8.rendererPathCount, 140);
  assert.strictEqual(stats.levels.huc12.rendererPathCount, 0);
  await waitForHucSettled();
  ghostMap.activate("huc12");
  assert.strictEqual(ghostController.stats().currentTheme, "none");
  assert.strictEqual(ghostController.stats().currentMinimumBlm, 0);
  await waitForHucSettled();
  assertRendererInvariant(ghostFixture, ["huc8", "huc12"]);
  ghostMap.deactivate("huc12");
  ghostMap.deactivate("huc8");
  await waitForHucSettled();
  assertRendererInvariant(ghostFixture, []);

  // Five cycles across three levels and all three theme families.
  const cycleLevels = ["huc12", "huc8", "huc10", "huc12", "huc6"];
  const cycleThemes = ["blm_pct", "ppt_in", "rech_in", "ppt_kaf", "rech_kaf"];
  const cycleThresholds = [75, 50, 25, 50, 25];
  for (let cycle = 0; cycle < cycleLevels.length; cycle += 1) {
    const levelName = cycleLevels[cycle];
    ghostMap.activate(levelName);
    await waitForHucSettled();
    assert.strictEqual(ghostController.setTheme(cycleThemes[cycle]), true);
    assert.strictEqual(
      ghostController.setMinimumBlmPct(cycleThresholds[cycle]),
      true
    );
    await waitForHucSettled();
    assertRendererInvariant(ghostFixture, [levelName]);
    ghostMap.deactivate(levelName);
    assertRendererInvariant(ghostFixture, []);
    await waitForHucSettled();
    stats = assertRendererInvariant(ghostFixture, []);
    assert.strictEqual(
      stats.levels[levelName].memberCount,
      stats.levels[levelName].expectedCount
    );
    assert.strictEqual(stats.levels[levelName].selectedTheme, "none");
    assert.strictEqual(stats.levels[levelName].minimumBlmPct, 0);
  }
  assert.strictEqual(ghostMap.totalListenerCount(), ghostMapListenerCount);
  Object.keys(ghostRootListenerCounts).forEach((levelName) => {
    assert.strictEqual(
      ghostFixture.roots[levelName].totalListenerCount(),
      ghostRootListenerCounts[levelName]
    );
  });
  stats = ghostController.stats();
  assert.strictEqual(stats.diagnostics.inactiveInvariantFailureCount, 0);
  assert.ok(stats.diagnostics.deferredMembershipResetCount > 0);
  assert.strictEqual(stats.diagnostics.forcedInactivePathRemovalCount, 0);
  assert.strictEqual(
    mapContainer.querySelectorAll(".pt-huc-theme-card").length,
    1
  );

  // Loading follows the existing scheduler and disappears on all exits.
  ghostMap.activate("huc10");
  stats = ghostController.stats();
  assert.strictEqual(stats.loadingIndicatorVisible, true);
  await waitForHucSettled();
  assert.strictEqual(ghostController.setTheme("ppt_in"), true);
  assert.strictEqual(ghostController.stats().loadingIndicatorVisible, true);
  await waitForHucSettled();
  assert.strictEqual(ghostController.stats().loadingIndicatorVisible, false);
  assert.ok(
    !mapContainer.querySelector(".pt-huc-theme-status")
      .innerHTML.includes("pt-huc-theme-spinner")
  );

  assert.strictEqual(ghostController.setTheme("rech_in"), true);
  assert.strictEqual(ghostController.stats().loadingIndicatorVisible, true);
  ghostMap.deactivate("huc10");
  assert.strictEqual(ghostController.stats().loadingIndicatorVisible, false);
  await waitForHucSettled();

  ["pt-main-layer-clear-btn", "pt-clear-all-btn"].forEach(
    (clearClassOrId) => {
      ghostMap.activate("huc12");
      assert.strictEqual(
        ghostController.stats().loadingIndicatorVisible,
        true
      );
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
        ghostController.stats().loadingIndicatorVisible,
        false
      );
      ghostMap.deactivate("huc12");
    }
  );
  await waitForHucSettled();
  assertRendererInvariant(ghostFixture, []);

  ghostMap.activate("huc12");
  await waitForHucSettled();
  ghostMap.activate("huc8");
  await waitForHucSettled();
  assert.strictEqual(ghostController.focusLevel("huc12"), true);
  assert.strictEqual(ghostController.setTheme("blm_pct"), true);
  assert.strictEqual(ghostController.stats().loadingIndicatorVisible, true);
  assert.strictEqual(ghostController.focusLevel("huc8"), true);
  assert.strictEqual(ghostController.stats().loadingIndicatorVisible, false);
  await waitForHucSettled();
  ghostMap.deactivate("huc12");
  ghostMap.deactivate("huc8");
  await waitForHucSettled();
  ghostController.destroy();

  const replacementFixture = makeFixture({
    percentValues: {huc12: [0, 50, 75, 100]}
  });
  const replacementMap = makeMap(
    replacementFixture,
    {earlyOverlayRemove: true}
  );
  installController.call(
    replacementMap,
    mapContainer,
    {},
    replacementFixture.data
  );
  replacementMap.activate("huc12");
  const oldLoadingStatus = mapContainer.querySelector(
    ".pt-huc-theme-status"
  );
  assert.ok(oldLoadingStatus.innerHTML.includes("Loading\u2026"));
  installController.call(
    replacementMap,
    mapContainer,
    {},
    replacementFixture.data
  );
  assert.ok(!oldLoadingStatus.innerHTML.includes("pt-huc-theme-spinner"));
  window.BRIM_HUC_LOCAL.destroy();
  assert.strictEqual(
    mapContainer.querySelectorAll(".pt-huc-theme-card").length,
    0
  );

  console.log("HUC family controller fixture: PASS");
}

run().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
