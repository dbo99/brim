#!/usr/bin/env node

"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");

const repositoryRoot = path.resolve(__dirname, "..");
const referenceSource = fs.readFileSync(
  path.join(
    repositoryRoot,
    "03_functions",
    "leaflet_layer_local_reference_helpers.r"
  ),
  "utf8"
);
const sharedSource = fs.readFileSync(
  path.join(
    repositoryRoot,
    "03_functions",
    "js",
    "brim_legend_closeout_helpers.js"
  ),
  "utf8"
);

function embeddedController(source) {
  const functionStart = source.indexOf(
    "pt_add_cnrfc_basin_product_availability_control <- function"
  );
  const functionEnd = source.indexOf(
    "\n\npt_add_field_office_hover_cleanup <- function",
    functionStart
  );
  assert(functionStart >= 0 && functionEnd > functionStart);
  const block = source.slice(functionStart, functionEnd);
  const rawStartToken = 'r"---(';
  const rawEndToken = ')---"';
  const rawStart = block.indexOf(rawStartToken);
  const rawEnd = block.lastIndexOf(rawEndToken);
  assert(rawStart >= 0 && rawEnd > rawStart, "embedded controller not found");
  return block.slice(rawStart + rawStartToken.length, rawEnd);
}

const targetGroup = "Basins – CNRFC Product Availability";
const records = [
  {
    cnrfc_id: "BDBC1",
    product_availability_label: "Forecast products confirmed",
    product_availability_color: "#4daf4a",
    forecast_group_label: "Sacramento",
    forecast_group_color: "#377eb8",
    water_supply_label: "Water supply forecast available",
    water_supply_color: "#984ea3",
    ensemble_label: "Any ensemble product available",
    ensemble_color: "#ff7f00",
    qpf_snow_level_label: "6-day daily QPF/FrzingLvl available",
    qpf_snow_level_color: "#ffff33",
    temperature_label: "Basin mean temp forecast available",
    temperature_color: "#a65628",
    stroke_color: "#333333"
  }
];
const controllerSource = embeddedController(referenceSource)
  .replace("__TARGET_GROUP__", targetGroup)
  .replace("__CNRFC_RECORDS__", JSON.stringify(records))
  .replace(
    "__GENERALIZATION_DISCLOSURE__",
    JSON.stringify(
      "Forecast-basin boundaries are generalized for statewide screening. " +
      "Check authoritative source for boundary-sensitive use."
    )
  );
const installController = new Function(`return (${controllerSource}\n);`)();

assert.match(controllerSource, /L\.control\(\{position: 'bottomleft'\}\)/);
assert.match(controllerSource, /legendCloseout\.actionsHtml/);
assert.match(controllerSource, /shared\.makeDetachable/);
assert.match(controllerSource, /cardUserHidden = true/);
assert.match(controllerSource, /map\.__ptCnrfcBasinAvailabilityController/);
assert.doesNotMatch(
  controllerSource,
  /mapContainer\.appendChild\(div\)/,
  "the retired free-floating map-container panel returned"
);
assert.match(
  sharedSource,
  /card: '\.pt-cnrfc-basin-panel'[^\n]*close: '\.pt-cnrfc-basin-close'[^\n]*handle: '\.pt-cnrfc-basin-title'[^\n]*dock: '\.pt-cnrfc-basin-dock'/,
  "the CNRFC card is not registered with the shared enhancer"
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
    return Object.values(this._events).reduce(
      (count, handlers) => count + handlers.length,
      0
    );
  };
  return target;
}

function classListFor(element) {
  return {
    add(...names) {
      const current = new Set(
        String(element.className).split(/\s+/).filter(Boolean)
      );
      names.forEach((name) => current.add(name));
      element.className = Array.from(current).join(" ");
    },
    remove(...names) {
      const removed = new Set(names);
      element.className = String(element.className)
        .split(/\s+/)
        .filter((name) => name && !removed.has(name))
        .join(" ");
    },
    contains(name) {
      return String(element.className).split(/\s+/).includes(name);
    }
  };
}

function styleObject() {
  const values = {};
  return {
    display: "",
    get cssText() {
      return Object.entries(values)
        .map(([name, value]) => `${name}:${value}`)
        .join(";");
    },
    set cssText(value) {
      Object.keys(values).forEach((name) => delete values[name]);
      if (value) values.__raw = String(value);
    },
    setProperty(name, value) {
      values[name] = String(value);
    },
    getPropertyValue(name) {
      return values[name] || "";
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

function descendants(element) {
  return element.children.reduce(
    (all, child) => all.concat(child, descendants(child)),
    []
  );
}

function mockElement(tagName = "div", className = "") {
  const listeners = {};
  let html = "";
  const element = {
    nodeType: 1,
    tagName: tagName.toUpperCase(),
    id: "",
    className,
    classList: null,
    style: styleObject(),
    parentNode: null,
    parentElement: null,
    children: [],
    childNodes: [],
    textContent: "",
    value: "",
    attributes: {},
    appendChild(child) {
      if (child.parentNode) child.parentNode.removeChild(child);
      this.children.push(child);
      this.childNodes.push(child);
      child.parentNode = this;
      child.parentElement = this;
      return child;
    },
    insertBefore(child, before) {
      if (!before || !this.children.includes(before)) return this.appendChild(child);
      if (child.parentNode) child.parentNode.removeChild(child);
      const index = this.children.indexOf(before);
      this.children.splice(index, 0, child);
      this.childNodes.splice(index, 0, child);
      child.parentNode = this;
      child.parentElement = this;
      return child;
    },
    removeChild(child) {
      this.children = this.children.filter((candidate) => candidate !== child);
      this.childNodes = this.childNodes.filter((candidate) => candidate !== child);
      child.parentNode = null;
      child.parentElement = null;
      return child;
    },
    remove() {
      if (this.parentNode) this.parentNode.removeChild(this);
    },
    contains(candidate) {
      return candidate === this || this.children.some(
        (child) => child.contains(candidate)
      );
    },
    querySelector(selector) {
      return descendants(this).find((candidate) => matches(candidate, selector)) || null;
    },
    querySelectorAll(selector) {
      return descendants(this).filter((candidate) => matches(candidate, selector));
    },
    closest(selector) {
      let current = this;
      while (current) {
        if (matches(current, selector)) return current;
        current = current.parentNode;
      }
      return null;
    },
    setAttribute(name, value) {
      this.attributes[name] = String(value);
    },
    getAttribute(name) {
      return Object.prototype.hasOwnProperty.call(this.attributes, name)
        ? this.attributes[name]
        : null;
    },
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
    listenerCount(name) {
      return (listeners[name] || []).length;
    },
    getBoundingClientRect() {
      return {left: 8, top: 350, right: 293, bottom: 600, width: 285, height: 250};
    }
  };
  element.classList = classListFor(element);

  Object.defineProperty(element, "innerHTML", {
    get() {
      return html;
    },
    set(value) {
      html = String(value);
      if (!html.includes("pt-cnrfc-basin-title")) return;
      element.children.slice().forEach((child) => element.removeChild(child));

      const header = mockElement(
        "div",
        "pt-cnrfc-basin-title pt-map-card-handle"
      );
      const title = mockElement("span", "pt-cnrfc-basin-title-text");
      title.textContent = "CNRFC basin catalog availability";
      const actions = mockElement("span", "pt-map-card-actions");
      const dock = mockElement(
        "button",
        "pt-map-card-dock pt-cnrfc-basin-dock"
      );
      dock.setAttribute("aria-label", "Undock CNRFC basin catalog availability");
      const close = mockElement(
        "button",
        "pt-map-legend-close pt-cnrfc-basin-close"
      );
      close.setAttribute("aria-label", "Hide CNRFC basin catalog availability");
      actions.appendChild(dock);
      actions.appendChild(close);
      header.appendChild(title);
      header.appendChild(actions);
      element.appendChild(header);
      element.appendChild(mockElement("label", "pt-cnrfc-basin-label"));
      const select = mockElement("select", "pt-cnrfc-basin-select");
      select.value = "product_availability";
      element.appendChild(select);
      element.appendChild(mockElement("div", "pt-cnrfc-basin-legend"));
    }
  });

  return element;
}

function click(element) {
  element.dispatchEvent({
    type: "click",
    target: element,
    preventDefault() {},
    stopPropagation() {}
  });
}

const elementsById = {};
const documentHead = mockElement("head");
const headAppendChild = documentHead.appendChild.bind(documentHead);
documentHead.appendChild = function(child) {
  headAppendChild(child);
  if (child.id) elementsById[child.id] = child;
  return child;
};

global.document = {
  head: documentHead,
  createElement: (tagName) => mockElement(tagName),
  getElementById: (id) => elementsById[id] || null,
  addEventListener() {},
  removeEventListener() {}
};

const sharedCalls = {
  actions: 0,
  wire: 0,
  detachable: 0,
  layout: 0,
  states: []
};

global.window = {
  BRIM: {
    legendCloseout: {
      actionsHtml() {
        sharedCalls.actions += 1;
        return '<span class="pt-map-card-actions">' +
          '<button type="button" class="pt-map-card-dock pt-cnrfc-basin-dock">&#x2197;</button>' +
          '<button type="button" class="pt-map-legend-close pt-cnrfc-basin-close">&times;</button>' +
          '</span>';
      },
      wire(div, selector, onClose) {
        const button = div.querySelector(selector);
        if (!button || button.__brimLegendCloseoutWired) return button;
        button.__brimLegendCloseoutWired = true;
        sharedCalls.wire += 1;
        button.addEventListener("click", (event) => {
          event.preventDefault();
          event.stopPropagation();
          onClose(event, button, div);
          div.style.display = "none";
        });
        return button;
      },
      makeDetachable(options) {
        if (options.card.__brimDetachableState) {
          return options.card.__brimDetachableState;
        }
        sharedCalls.detachable += 1;
        const card = options.card;
        const dockButton = card.querySelector(options.dockSelector);
        const dockParent = card.parentNode;
        const state = {
          floating: false,
          destroyed: false,
          undock() {
            if (this.floating || this.destroyed) return;
            card.ownerDockParent = card.parentNode || dockParent;
            card.ownerDockParent.removeChild(card);
            mapContainer.appendChild(card);
            card.classList.add("pt-map-card-undocked");
            this.floating = true;
          },
          dock() {
            if (!this.floating || this.destroyed) return;
            mapContainer.removeChild(card);
            (card.ownerDockParent || dockParent).appendChild(card);
            card.classList.remove("pt-map-card-undocked");
            this.floating = false;
          },
          destroy(removeCard) {
            if (this.destroyed) return;
            if (this.floating) this.dock();
            dockButton.removeEventListener("click", this.dockClick);
            this.destroyed = true;
            if (removeCard) card.remove();
            delete card.__brimDetachableState;
          }
        };
        state.dockClick = () => {
          if (state.floating) state.dock();
          else state.undock();
        };
        dockButton.addEventListener("click", state.dockClick);
        card.__brimDetachableState = state;
        sharedCalls.states.push(state);
        return state;
      },
      scheduleLayout() {
        sharedCalls.layout += 1;
      }
    }
  }
};

let nextStamp = 1;
global.L = {
  stamp(layer) {
    if (!layer.__stamp) layer.__stamp = nextStamp++;
    return layer.__stamp;
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
      _map: null,
      addTo(map) {
        this._map = map;
        this._container = this.onAdd(map);
        map.lowerLeft.appendChild(this._container);
        map.controls.add(this);
        return this;
      },
      getContainer() {
        return this._container;
      },
      remove() {
        if (this._container) this._container.remove();
        if (this._map) this._map.controls.delete(this);
        this._container = null;
      }
    };
  }
};

const mapContainer = mockElement("div", "leaflet-container");
mapContainer.getBoundingClientRect = () => ({
  left: 0,
  top: 0,
  right: 800,
  bottom: 600,
  width: 800,
  height: 600
});
const controlContainer = mockElement("div", "leaflet-control-container");
const lowerLeft = mockElement("div", "leaflet-bottom leaflet-left");
controlContainer.appendChild(lowerLeft);
mapContainer.appendChild(controlContainer);

const layer = evented({
  options: {group: targetGroup, layerId: "BDBC1"},
  styleCalls: [],
  tooltipCloseCount: 0,
  setStyle(style) {
    this.styleCalls.push(Object.assign({}, style));
  },
  closeTooltip() {
    this.tooltipCloseCount += 1;
  }
});
const activeLayers = new Set();
const map = evented({
  lowerLeft,
  controls: new Set(),
  layerManager: {_byGroup: {[targetGroup]: {BDBC1: layer}}},
  getContainer: () => mapContainer,
  eachLayer(callback) {
    activeLayers.forEach(callback);
  },
  hasLayer(candidate) {
    return activeLayers.has(candidate);
  },
  removeControl(control) {
    control.remove();
  }
});

global.setTimeout = (callback) => {
  callback();
  return 1;
};
global.clearTimeout = () => {};

function cards() {
  return mapContainer.querySelectorAll(".pt-cnrfc-basin-panel");
}

function activate() {
  activeLayers.add(layer);
  map.fire("overlayadd", {name: targetGroup});
}

function clearLocal() {
  activeLayers.delete(layer);
  map.fire("overlayremove", {name: targetGroup});
}

function clearAll() {
  clearLocal();
}

installController.call(map, mapContainer, {});
assert.strictEqual(cards().length, 0, "an inactive layer created an orphan card");
assert.strictEqual(map.controls.size, 0);
assert.strictEqual(map.totalListenerCount(), 7);

activate();
assert.strictEqual(cards().length, 1, "activation did not create exactly one card");
assert.strictEqual(map.controls.size, 1);
let card = cards()[0];
assert.strictEqual(card.parentNode, lowerLeft, "card is not in the lower-left stack");
assert.strictEqual(card.style.display, "block");
assert.ok(card.querySelector(".pt-cnrfc-basin-select"));
assert.match(
  card.querySelector(".pt-cnrfc-basin-legend").innerHTML,
  /Forecast products confirmed/,
  "existing legend body was not retained"
);

let actions = card.querySelector(".pt-map-card-actions");
let dockButton = card.querySelector(".pt-cnrfc-basin-dock");
let closeButton = card.querySelector(".pt-cnrfc-basin-close");
assert.ok(dockButton, "detach/dock control is absent");
assert.ok(closeButton, "close control is absent");
assert.ok(
  actions.children.indexOf(dockButton) < actions.children.indexOf(closeButton),
  "detach/dock arrow does not immediately precede close"
);
assert.strictEqual(dockButton.listenerCount("click"), 1);
assert.strictEqual(closeButton.listenerCount("click"), 1);
assert.strictEqual(sharedCalls.actions, 1);
assert.strictEqual(sharedCalls.wire, 1);
assert.strictEqual(sharedCalls.detachable, 1);

click(dockButton);
assert.strictEqual(card.__brimDetachableState.floating, true);
assert.strictEqual(card.parentNode, mapContainer);
click(dockButton);
assert.strictEqual(card.__brimDetachableState.floating, false);
assert.strictEqual(card.parentNode, lowerLeft);

const stylesBeforeClose = layer.styleCalls.length;
click(closeButton);
assert.strictEqual(card.style.display, "none");
assert.strictEqual(activeLayers.has(layer), true, "close disabled the layer");
assert.strictEqual(
  map.__ptCnrfcBasinAvailabilityController.isUserHidden(),
  true
);
map.fire("moveend");
assert.strictEqual(
  card.style.display,
  "none",
  "same-session map activity reopened an intentionally closed card"
);
assert.ok(layer.styleCalls.length >= stylesBeforeClose);

clearLocal();
assert.strictEqual(cards().length, 0, "Clear Local left an orphan card");
assert.strictEqual(map.controls.size, 0);
assert.strictEqual(sharedCalls.states[0].destroyed, true);

activate();
assert.strictEqual(cards().length, 1);
card = cards()[0];
assert.strictEqual(card.style.display, "block", "fresh activation did not restore card");
const select = card.querySelector(".pt-cnrfc-basin-select");
const expectedModeColors = {
  product_availability: "#4daf4a",
  forecast_group: "#377eb8",
  water_supply: "#984ea3",
  ensemble: "#ff7f00",
  qpf_snow_level: "#ffff33",
  temperature: "#a65628"
};
Object.entries(expectedModeColors).forEach(([mode, color]) => {
  select.value = mode;
  select.dispatchEvent({type: "change", target: select});
  assert.strictEqual(
    layer.styleCalls[layer.styleCalls.length - 1].fillColor,
    color,
    `${mode} no longer controls Product Availability styling`
  );
});
assert.strictEqual(layer.totalListenerCount(), 4, "hover cleanup listeners duplicated");

const replacedState = card.__brimDetachableState;
installController.call(map, mapContainer, {});
assert.strictEqual(replacedState.destroyed, true, "replacement did not clean old card");
assert.strictEqual(cards().length, 1, "replacement created duplicate cards");
assert.strictEqual(map.controls.size, 1, "replacement left duplicate controls");
assert.strictEqual(map.totalListenerCount(), 7, "replacement duplicated map listeners");
assert.strictEqual(
  Object.prototype.hasOwnProperty.call(
    map,
    "__ptCnrfcBasinAvailabilityController"
  ),
  true
);

clearAll();
assert.strictEqual(cards().length, 0, "Clear All left an orphan card");
assert.strictEqual(map.controls.size, 0);
activate();
assert.strictEqual(cards().length, 1);
map.fire("unload");
assert.strictEqual(cards().length, 0, "unload left an orphan card");
assert.strictEqual(map.controls.size, 0);
assert.strictEqual(map.totalListenerCount(), 0, "unload left controller listeners");
assert.strictEqual(
  Object.prototype.hasOwnProperty.call(
    map,
    "__ptCnrfcBasinAvailabilityController"
  ),
  false,
  "unload left a controller reference"
);

console.log("CNRFC Product Availability shared-card lifecycle fixture: PASS");
