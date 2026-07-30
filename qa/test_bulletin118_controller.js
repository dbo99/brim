const assert = require("assert");
const fs = require("fs");
const path = require("path");

const sourcePath = path.join(
  __dirname,
  "..",
  "03_functions",
  "js",
  "brim_bulletin118_theme_control.js"
);
const source = fs.readFileSync(sourcePath, "utf8");
const installController = new Function(`return (${source}\n);`)();

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
      this._events[name] = (this._events[name] || []).filter(
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
  target.totalListenerCount = function() {
    return Object.keys(this._events).reduce(
      (sum, name) => sum + this._events[name].length,
      0
    );
  };
  return target;
}

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
    },
    toggle(name, force) {
      const shouldAdd = force === undefined ? !this.contains(name) : !!force;
      if (shouldAdd) this.add(name);
      else this.remove(name);
      return shouldAdd;
    }
  };
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
    parentNode: null,
    children,
    classList: null,
    textContent: "",
    value: "",
    hidden: false,
    focusCount: 0,
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
    focus() {
      this.focusCount += 1;
    },
    scrollIntoView() {},
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
        if (matches(child, selector)) found.push(child);
        found = found.concat(child.querySelectorAll(selector));
      });
      return found;
    },
    setAttribute(name, value) {
      this[name] = String(value);
    },
    getAttribute(name) {
      return this[name] === undefined ? null : this[name];
    }
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
        element.classList.contains("pt-bulletin118-theme-card") &&
        !selectorMap[".pt-bulletin118-theme-select"]
      ) {
        const head = mockElement("div", "pt-bulletin118-theme-head pt-map-card-handle");
        const dock = mockElement("button", "pt-bulletin118-theme-dock pt-map-card-dock");
        const close = mockElement("button", "pt-bulletin118-theme-close pt-map-legend-close");
        const themeSelect = mockElement("select", "pt-bulletin118-theme-select");
        themeSelect.id = "pt-bulletin118-theme-select";
        themeSelect.value = "basins";
        const legend = mockElement("div", "pt-bulletin118-theme-legend");
        const filterValue = mockElement("span", "pt-bulletin118-filter-value");
        filterValue.textContent = "0%";
        const minimum = mockElement("input", "pt-bulletin118-minimum");
        minimum.value = "0";
        const visibleCount = mockElement("div", "pt-bulletin118-visible-count");
        visibleCount.textContent = "4 of 4 basins shown";
        const searchInput = mockElement("input", "pt-bulletin118-search-input");
        const searchClear = mockElement("button", "pt-bulletin118-search-clear");
        const searchResults = mockElement("div", "pt-bulletin118-search-results");
        searchResults.hidden = true;
        const status = mockElement("div", "pt-bulletin118-theme-status");
        head.appendChild(dock);
        head.appendChild(close);
        element.appendChild(head);
        element.appendChild(themeSelect);
        element.appendChild(filterValue);
        element.appendChild(minimum);
        element.appendChild(visibleCount);
        element.appendChild(searchInput);
        element.appendChild(searchClear);
        element.appendChild(searchResults);
        element.appendChild(legend);
        element.appendChild(status);
        selectorMap[".pt-bulletin118-theme-head"] = head;
        selectorMap[".pt-bulletin118-theme-dock"] = dock;
        selectorMap[".pt-bulletin118-theme-close"] = close;
        selectorMap[".pt-bulletin118-theme-select"] = themeSelect;
        selectorMap[".pt-bulletin118-filter-value"] = filterValue;
        selectorMap[".pt-bulletin118-minimum"] = minimum;
        selectorMap[".pt-bulletin118-visible-count"] = visibleCount;
        selectorMap[".pt-bulletin118-search-input"] = searchInput;
        selectorMap[".pt-bulletin118-search-clear"] = searchClear;
        selectorMap[".pt-bulletin118-search-results"] = searchResults;
        selectorMap[".pt-bulletin118-theme-legend"] = legend;
        selectorMap[".pt-bulletin118-theme-status"] = status;
      }
    }
  });
  return element;
}

function matches(element, selector) {
  if (!element || !selector) return false;
  if (selector.startsWith(".")) {
    return element.classList.contains(selector.slice(1));
  }
  if (selector.startsWith("#")) return element.id === selector.slice(1);
  return element.tagName.toLowerCase() === selector.toLowerCase();
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
  createElement: (tagName) => mockElement(tagName),
  getElementById: (id) => documentById[id] || null
};

let nextFrame = 1;
const frames = new Map();
function requestAnimationFrame(callback) {
  const id = nextFrame++;
  frames.set(id, callback);
  return id;
}
function cancelAnimationFrame(id) {
  frames.delete(id);
}
function flushFrames() {
  const pending = Array.from(frames.entries());
  frames.clear();
  pending.forEach(([, callback]) => callback(Date.now()));
}

const detachableStates = [];
global.window = {
  performance: {now: () => Date.now()},
  requestAnimationFrame,
  cancelAnimationFrame,
  setTimeout,
  clearTimeout,
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

global.L = {
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

function makeLayer(id, sgmaColor, blmColor, renderer) {
  return evented({
    options: {
      layerId: id,
      fillColor: "#8B5A2B",
      fillOpacity: 0.2,
      renderer
    },
    styleCalls: [],
    tooltipCloseCount: 0,
    popupOpenCount: 0,
    setStyle(style) {
      this.styleCalls.push(Object.assign({}, style));
      Object.assign(this.options, style);
    },
    closeTooltip() {
      this.tooltipCloseCount += 1;
    },
    getBounds() {
      return {
        id,
        isValid: () => true
      };
    },
    openPopup() {
      this.popupOpenCount += 1;
    },
    _fixtureRecord: {sgmaColor, blmColor}
  });
}

function makeFixture() {
  const groupName = "Basins – GW Basins, Bulletin 118 (515)";
  const renderer = {};
  const root = evented({});
  const layers = [
    makeLayer("gw_bull118_1", "#FF0000", "#F5F5F5", renderer),
    makeLayer("gw_bull118_2", "#FFFF00", "#FEE391", renderer),
    makeLayer("gw_bull118_3", "#55FF00", "#FE9929", renderer),
    makeLayer("gw_bull118_4", "#0070FF", "#993404", renderer)
  ];
  const groupLayers = new Set(layers);
  root.addCalls = 0;
  root.removeCalls = 0;
  root.hasLayer = (layer) => groupLayers.has(layer);
  root.addLayer = function(layer) {
    if (!groupLayers.has(layer)) {
      groupLayers.add(layer);
      this.addCalls += 1;
    }
    return this;
  };
  root.removeLayer = function(layer) {
    if (groupLayers.delete(layer)) this.removeCalls += 1;
    return this;
  };
  const byGroup = {[groupName]: {}};
  layers.forEach((layer, index) => {
    byGroup[groupName][String(index + 1)] = layer;
  });
  const activeLayers = new Set();
  const map = evented({
    layerManager: {
      _byGroup: byGroup,
      _groupContainers: {[groupName]: root}
    },
    removedLayers: [],
    popupCloseCount: 0,
    fitBoundsCalls: [],
    getContainer: () => mapContainer,
    hasLayer(layer) {
      return activeLayers.has(layer);
    },
    removeLayer(layer) {
      activeLayers.delete(layer);
      this.removedLayers.push(layer);
    },
    closePopup() {
      this.popupCloseCount += 1;
    },
    fitBounds(bounds, options) {
      this.fitBoundsCalls.push({bounds, options});
      return this;
    },
    closeTooltip() {}
  });

  const blmValues = [0, null, 70, 90];
  const names = [
    "Indian Wells Valley",
    "Owens Valley",
    "Mojave River Valley",
    "Death Valley"
  ];
  const records = layers.map((layer, index) => ({
    layer_id: layer.options.layerId,
    percent_blm: blmValues[index],
    sgma_color: layer._fixtureRecord.sgmaColor,
    blm_color: layer._fixtureRecord.blmColor,
    display_label: `${index + 1}-00${index + 1} - ${names[index]}`,
    basin_name: names[index],
    subbasin_name: names[index],
    subbasin_num: `${index + 1}-00${index + 1}`,
    basin_num: String(index + 1)
  }));
  const data = {
    records,
    group_name: groupName,
    expected_count: layers.length,
    default_theme: "basins",
    themes: {
      basins: {
        title: "Basins only",
        rows: [{color: "#8B5A2B", label: "Bulletin 118 basins", count: 4}]
      },
      sgma_2019: {
        title: "DWR SGMA 2019 Basin Prioritization",
        rows: [
          {color: "#FF0000", label: "High", count: 1},
          {color: "#FFFF00", label: "Medium", count: 1},
          {color: "#55FF00", label: "Low", count: 1},
          {color: "#0070FF", label: "Very Low", count: 1}
        ],
        note: "Final DWR categories."
      },
      blm_pct: {
        title: "BLM-managed land — %",
        rows: [{color: "#993404", label: ">50%", count: 1}]
      }
    },
    styles: {
      basins: {fill_color: "#8B5A2B", fill_opacity: 0.2},
      sgma_2019: {fill_opacity: 0.48},
      blm_pct: {fill_opacity: 0.58}
    },
    source_url: "https://example.test/dwr"
  };
  return {
    map,
    root,
    renderer,
    layers,
    data,
    activeLayers,
    groupLayers,
    groupName
  };
}

function click(element) {
  element.dispatchEvent({
    type: "click",
    preventDefault() {},
    stopPropagation() {}
  });
}

function removeAllCards() {
  mapContainer.querySelectorAll(".pt-bulletin118-theme-card").forEach(
    (card) => card.remove()
  );
}

removeAllCards();
const fixture = makeFixture();
installController.call(fixture.map, mapContainer, {}, fixture.data);

let api = window.BRIM_BULLETIN118_LOCAL;
let stats = api.stats();
assert.strictEqual(stats.active, false);
assert.strictEqual(stats.currentTheme, "basins");
assert.strictEqual(stats.currentMinimumBlm, 0);
assert.strictEqual(stats.visibleBasinCount, fixture.layers.length);
assert.strictEqual(stats.registeredCount, fixture.layers.length);
assert.strictEqual(stats.retainedRecordCount, fixture.layers.length);
assert.strictEqual(stats.diagnostics.mapScanCount, 0);
assert.strictEqual(stats.cardVisible, false);
assert.strictEqual(
  mapContainer.querySelectorAll(".pt-bulletin118-theme-card").length,
  1
);
const card = mapContainer.querySelector(".pt-bulletin118-theme-card");

fixture.activeLayers.add(fixture.root);
fixture.activeLayers.add(fixture.renderer);
fixture.map.fire("overlayadd", {
  name: fixture.groupName,
  layer: fixture.root
});
flushFrames();
stats = api.stats();
assert.strictEqual(stats.active, true);
assert.strictEqual(stats.cardVisible, true);
assert.strictEqual(stats.appliedTheme, "basins");
assert.strictEqual(
  card.querySelector(".pt-bulletin118-theme-legend").innerHTML
    .includes("pt-bulletin118-theme-note"),
  false
);
fixture.layers.forEach((layer) => {
  assert.strictEqual(layer.options.fillColor, "#8B5A2B");
  assert.strictEqual(layer.options.fillOpacity, 0.2);
});

const operationsBeforePriorityTheme =
  stats.diagnostics.styleOperationCount;
assert.strictEqual(api.setTheme("sgma_2019"), true);
assert.strictEqual(api.stats().stylePending, true);
flushFrames();
fixture.layers.forEach((layer) => {
  assert.strictEqual(layer.options.fillColor, layer._fixtureRecord.sgmaColor);
  assert.strictEqual(layer.options.fillOpacity, 0.48);
});
stats = api.stats();
assert.strictEqual(stats.appliedTheme, "sgma_2019");
assert.strictEqual(
  stats.diagnostics.styleOperationCount,
  operationsBeforePriorityTheme + fixture.layers.length
);
assert.strictEqual(
  mapContainer.querySelector(".pt-bulletin118-theme-legend").innerHTML
    .includes("DWR SGMA 2019 Basin Prioritization"),
  true
);
assert.strictEqual(
  mapContainer.querySelector(".pt-bulletin118-theme-legend").innerHTML
    .includes("Final DWR categories."),
  true
);

const styleJobsBeforeNoop = stats.diagnostics.styleJobCount;
assert.strictEqual(api.setTheme("sgma_2019"), false);
assert.strictEqual(
  api.stats().diagnostics.styleJobCount,
  styleJobsBeforeNoop
);
assert.strictEqual(api.stats().diagnostics.sameThemeNoops, 1);

const callsBeforeRapid = fixture.layers.map((layer) => layer.styleCalls.length);
assert.strictEqual(api.setTheme("blm_pct"), true);
assert.strictEqual(api.setTheme("basins"), true);
flushFrames();
fixture.layers.forEach((layer, index) => {
  assert.strictEqual(layer.options.fillColor, "#8B5A2B");
  assert.strictEqual(layer.styleCalls.length, callsBeforeRapid[index] + 1);
});
assert.strictEqual(api.stats().currentTheme, "basins");
assert.strictEqual(api.stats().appliedTheme, "basins");
assert.ok(api.stats().diagnostics.canceledStyleJobs >= 1);

const minimumSlider = card.querySelector(".pt-bulletin118-minimum");
minimumSlider.value = "70";
minimumSlider.dispatchEvent({type: "input"});
assert.strictEqual(api.stats().filterPending, true);
flushFrames();
stats = api.stats();
assert.strictEqual(stats.currentMinimumBlm, 70);
assert.strictEqual(stats.visibleBasinCount, 2);
assert.strictEqual(fixture.groupLayers.size, 2);
assert.strictEqual(fixture.groupLayers.has(fixture.layers[0]), false);
assert.strictEqual(fixture.groupLayers.has(fixture.layers[1]), false);
assert.strictEqual(fixture.groupLayers.has(fixture.layers[2]), true);
assert.strictEqual(fixture.groupLayers.has(fixture.layers[3]), true);
assert.strictEqual(
  card.querySelector(".pt-bulletin118-filter-value").textContent,
  "70%"
);
assert.strictEqual(
  card.querySelector(".pt-bulletin118-visible-count").textContent,
  "2 of 4 basins shown"
);

const filterJobsBeforeNoop = stats.diagnostics.filterJobCount;
assert.strictEqual(api.setMinimumBlm(70), false);
assert.strictEqual(api.stats().diagnostics.filterJobCount, filterJobsBeforeNoop);
assert.strictEqual(api.stats().diagnostics.sameThresholdNoops, 1);

assert.strictEqual(api.setTheme("sgma_2019"), true);
flushFrames();
assert.strictEqual(api.stats().currentMinimumBlm, 70);
assert.strictEqual(api.stats().visibleBasinCount, 2);
assert.strictEqual(api.stats().currentTheme, "sgma_2019");
assert.strictEqual(fixture.layers[0].options.fillColor, "#FF0000");
assert.strictEqual(fixture.layers[2].options.fillColor, "#55FF00");

assert.strictEqual(api.setMinimumBlm(90), true);
assert.strictEqual(api.setMinimumBlm(25), true);
flushFrames();
stats = api.stats();
assert.strictEqual(stats.currentMinimumBlm, 25);
assert.strictEqual(stats.visibleBasinCount, 2);
assert.strictEqual(fixture.groupLayers.size, 2);
assert.strictEqual(fixture.groupLayers.has(fixture.layers[1]), false);
assert.ok(stats.diagnostics.canceledFilterJobs >= 1);

assert.strictEqual(api.setMinimumBlm(0), true);
flushFrames();
assert.strictEqual(api.stats().visibleBasinCount, 4);
assert.strictEqual(fixture.groupLayers.size, 4);

assert.strictEqual(api.setTheme("blm_pct"), true);
flushFrames();
assert.strictEqual(
  card.querySelector(".pt-bulletin118-theme-legend").innerHTML
    .includes("pt-bulletin118-theme-note"),
  false
);
assert.strictEqual(api.setMinimumBlm(90), true);
flushFrames();
assert.strictEqual(fixture.groupLayers.size, 1);

const indianMatches = api.search("Indian");
assert.strictEqual(indianMatches.length, 1);
assert.strictEqual(indianMatches[0].label.includes("Indian Wells Valley"), true);
const searchInput = card.querySelector(".pt-bulletin118-search-input");
searchInput.value = "Indian";
searchInput.dispatchEvent({
  type: "keydown",
  key: "ArrowDown",
  preventDefault() {}
});
searchInput.dispatchEvent({
  type: "keydown",
  key: "Enter",
  preventDefault() {}
});
stats = api.stats();
assert.strictEqual(stats.currentMinimumBlm, 0);
assert.strictEqual(stats.visibleBasinCount, 4);
assert.strictEqual(stats.currentTheme, "blm_pct");
assert.strictEqual(fixture.groupLayers.size, 4);
assert.strictEqual(fixture.map.fitBoundsCalls.length, 1);
assert.strictEqual(fixture.layers[0].popupOpenCount, 1);
assert.deepStrictEqual(
  fixture.map.fitBoundsCalls[0].options.padding,
  [24, 24]
);
assert.strictEqual(fixture.map.fitBoundsCalls[0].options.maxZoom, 11);

assert.strictEqual(api.setMinimumBlm(1), true);
flushFrames();
assert.strictEqual(fixture.groupLayers.has(fixture.layers[1]), false);
searchInput.value = "Owens";
searchInput.dispatchEvent({type: "input"});
const searchResults = card.querySelector(
  ".pt-bulletin118-search-results"
);
const mouseResult = searchResults.querySelector(
  ".pt-bulletin118-search-result"
);
searchResults.dispatchEvent({
  type: "click",
  target: mouseResult,
  preventDefault() {},
  stopPropagation() {}
});
assert.strictEqual(fixture.map.fitBoundsCalls.length, 2);
assert.strictEqual(fixture.layers[1].popupOpenCount, 1);
assert.strictEqual(api.stats().currentMinimumBlm, 0);
assert.strictEqual(fixture.groupLayers.size, 4);

searchInput.value = "Valley";
searchInput.dispatchEvent({type: "input"});
assert.ok(api.stats().searchResultCount > 0);
searchInput.dispatchEvent({
  type: "keydown",
  key: "Escape",
  preventDefault() {}
});
assert.strictEqual(api.stats().searchResultCount, 0);
assert.strictEqual(searchResults.hidden, true);

const popupBeforeFiltering = {_source: fixture.layers[0]};
fixture.map.fire("popupopen", {popup: popupBeforeFiltering});
const popupCloseBeforeFilter = fixture.map.popupCloseCount;
assert.strictEqual(api.setMinimumBlm(70), true);
flushFrames();
assert.strictEqual(fixture.map.popupCloseCount, popupCloseBeforeFilter + 1);
assert.strictEqual(api.setMinimumBlm(0), true);
flushFrames();

assert.strictEqual(api.setTheme("basins"), true);
flushFrames();
assert.strictEqual(api.setTheme("blm_pct"), true);
assert.strictEqual(api.stats().stylePending, true);
searchInput.value = "Valley";
searchInput.dispatchEvent({type: "input"});
assert.ok(api.stats().searchResultCount > 0);
const operationsBeforeOff = api.stats().diagnostics.styleOperationCount;
card._detachableState.floating = true;
fixture.activeLayers.delete(fixture.root);
fixture.map.fire("overlayremove", {
  name: fixture.groupName,
  layer: fixture.root
});
flushFrames();
stats = api.stats();
assert.strictEqual(stats.active, false);
assert.strictEqual(stats.cardVisible, false);
assert.strictEqual(stats.stylePending, false);
assert.strictEqual(stats.filterPending, false);
assert.strictEqual(stats.searchResultCount, 0);
assert.strictEqual(searchInput.value, "");
assert.strictEqual(stats.diagnostics.styleOperationCount, operationsBeforeOff);
assert.strictEqual(card._detachableState.floating, false);
assert.strictEqual(card._detachableState.dockCalls, 1);
assert.strictEqual(fixture.map.removedLayers.includes(fixture.renderer), true);

fixture.activeLayers.add(fixture.root);
fixture.activeLayers.add(fixture.renderer);
fixture.map.fire("overlayadd", {
  name: fixture.groupName,
  layer: fixture.root
});
flushFrames();
stats = api.stats();
assert.strictEqual(stats.currentTheme, "blm_pct");
assert.strictEqual(stats.appliedTheme, "blm_pct");
assert.strictEqual(stats.cardVisible, true);
fixture.layers.forEach((layer) => {
  assert.strictEqual(layer.options.fillColor, layer._fixtureRecord.blmColor);
  assert.strictEqual(layer.options.fillOpacity, 0.58);
});

click(card.querySelector(".pt-bulletin118-theme-close"));
stats = api.stats();
assert.strictEqual(stats.active, true);
assert.strictEqual(stats.cardVisible, false);
assert.strictEqual(stats.cardUserHidden, true);

fixture.activeLayers.delete(fixture.root);
fixture.map.fire("overlayremove", {
  name: fixture.groupName,
  layer: fixture.root
});
fixture.activeLayers.add(fixture.root);
fixture.activeLayers.add(fixture.renderer);
fixture.map.fire("overlayadd", {
  name: fixture.groupName,
  layer: fixture.root
});
flushFrames();
assert.strictEqual(api.stats().cardVisible, true);
assert.strictEqual(api.stats().cardUserHidden, false);

const tooltip = {_source: fixture.layers[0]};
fixture.map.fire("tooltipopen", {tooltip});
fixture.map.fire("pt:measureinteractionchange", {active: true});
assert.strictEqual(fixture.layers[0].tooltipCloseCount > 0, true);

const popup = {_source: fixture.layers[1]};
fixture.map.fire("popupopen", {popup});
const popupCloseBefore = fixture.map.popupCloseCount;
fixture.map.fire("pt:measureinteractionchange", {active: true});
assert.strictEqual(fixture.map.popupCloseCount, popupCloseBefore + 1);

const listenerCountBeforeReplacement = fixture.map.totalListenerCount();
installController.call(fixture.map, mapContainer, {}, fixture.data);
api = window.BRIM_BULLETIN118_LOCAL;
flushFrames();
assert.strictEqual(
  mapContainer.querySelectorAll(".pt-bulletin118-theme-card").length,
  1
);
assert.strictEqual(
  fixture.map.totalListenerCount(),
  listenerCountBeforeReplacement
);
assert.strictEqual(api.stats().currentTheme, "basins");
assert.strictEqual(api.stats().appliedTheme, "basins");
assert.strictEqual(api.stats().diagnostics.mapScanCount, 0);
assert.strictEqual(api.stats().currentMinimumBlm, 0);
assert.strictEqual(fixture.groupLayers.size, fixture.layers.length);

api.destroy();
assert.strictEqual(
  mapContainer.querySelectorAll(".pt-bulletin118-theme-card").length,
  0
);
assert.strictEqual(fixture.map.totalListenerCount(), 0);

assert.strictEqual(source.includes("map.eachLayer"), false);
assert.strictEqual(source.includes("returnGeometry"), false);
assert.strictEqual(source.includes("DWR SGMA 2019 Basin Prioritization"), true);
assert.strictEqual(source.includes("BLM-managed land \\u2014 %"), true);
assert.strictEqual(source.includes("groupRoot.addLayer"), true);
assert.strictEqual(source.includes("groupRoot.removeLayer"), true);
assert.strictEqual(source.includes("Find basin or subbasin\\u2026"), true);
assert.strictEqual(/fillOpacity\\s*:\\s*0\\s*[,}]/.test(source), false);

console.log("Bulletin 118 controller fixture: PASS");
