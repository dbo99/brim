#!/usr/bin/env node

"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");

const source = fs.readFileSync(
  path.join(
    __dirname, "..", "03_functions", "leaflet_layer_local_reference_helpers.r"
  ),
  "utf8"
);
const start = source.indexOf(
  "pt_add_polygon_generalization_disclosure_control <- function"
);
const end = source.indexOf(
  "\n\npt_add_wsr_reference_browser_layers <- function", start
);
assert(start >= 0 && end > start, "disclosure helper block not found");
const block = source.slice(start, end);
const rawStart = block.indexOf('r"---(');
const rawEnd = block.lastIndexOf(')---"');
assert(rawStart >= 0 && rawEnd > rawStart, "embedded disclosure controller missing");
const controllerSource = block.slice(rawStart + 'r"---('.length, rawEnd);
const install = new Function(`return (${controllerSource}\n);`)();

function evented(target) {
  target._events = {};
  target.on = function(names, handler) {
    String(names).split(/\s+/).forEach((name) => {
      this._events[name] = this._events[name] || [];
      this._events[name].push(handler);
    });
    return this;
  };
  target.off = function(names, handler) {
    String(names).split(/\s+/).forEach((name) => {
      this._events[name] = (this._events[name] || []).filter(
        (candidate) => candidate !== handler
      );
    });
    return this;
  };
  target.fire = function(name, event) {
    (this._events[name] || []).slice().forEach((handler) => handler(event || {}));
  };
  return target;
}

function button() {
  return {
    listeners: {},
    addEventListener(name, handler) { this.listeners[name] = handler; },
    click() {
      this.listeners.click({preventDefault() {}, stopPropagation() {}});
    }
  };
}

const cards = [];
const styles = {};
global.document = {
  getElementById(id) { return styles[id] || null; },
  createElement() {
    return {
      id: "",
      textContent: ""
    };
  },
  head: {
    appendChild(node) { styles[node.id] = node; }
  }
};
global.window = {
  setTimeout(handler) { handler(); },
  BRIM_SCHEDULE_CARD_LAYOUT() {}
};
global.L = {
  control() {
    return {
      onAdd: null,
      addTo(map) {
        this._map = map;
        this._container = this.onAdd(map);
        map._controls.push(this);
        cards.push(this._container);
      }
    };
  },
  DomUtil: {
    create(tagName, className) {
      const close = button();
      const node = {
        tagName,
        className,
        style: {},
        _html: "",
        set innerHTML(value) { this._html = String(value); },
        get innerHTML() { return this._html; },
        querySelector(selector) {
          return selector === ".pt-polygon-generalization-close" ? close : null;
        },
        close
      };
      return node;
    }
  },
  DomEvent: {
    disableClickPropagation() {},
    disableScrollPropagation() {}
  }
};

const root = {};
const map = evented({
  _controls: [],
  _visible: new Set(),
  layerManager: {_groupContainers: {"Reference – Fixture": root}},
  hasLayer(layer) { return this._visible.has(layer); },
  removeControl(control) {
    this._controls = this._controls.filter((candidate) => candidate !== control);
  }
});
const data = {
  layer_id: "fixture",
  group_name: "Reference – Fixture",
  title: "Fixture polygons",
  note: "Fixture polygons are generalized for screening. " +
    "Check authoritative source for boundary-sensitive use."
};

install.call(map, {}, {}, data);
assert.strictEqual(map._controls.length, 1);
assert.strictEqual(cards[0].style.display, "none");
assert.match(cards[0].innerHTML, /Fixture polygons are generalized/);

map._visible.add(root);
map.fire("overlayadd", {name: data.group_name});
assert.strictEqual(cards[0].style.display, "block");
cards[0].close.click();
assert.strictEqual(cards[0].style.display, "none");

// Clear Local / Clear All remove the root and fire overlayremove. The card is
// hidden and its user-close state resets for the next ordinary activation.
map._visible.delete(root);
map.fire("overlayremove", {name: data.group_name});
assert.strictEqual(cards[0].style.display, "none");
map._visible.add(root);
map.fire("overlayadd", {name: data.group_name});
assert.strictEqual(cards[0].style.display, "block");

// Reinstalling the same layer owner destroys the prior controller: one control
// and one overlay listener remain, so no duplicate card/listener accumulates.
install.call(map, {}, {}, data);
assert.strictEqual(map._controls.length, 1);
assert.strictEqual(map._events.overlayadd.length, 1);
assert.strictEqual(map._events.overlayremove.length, 1);
assert.strictEqual(
  Object.keys(map.__brimPolygonDisclosureControllers).length,
  1
);

map.fire("unload", {});
assert.strictEqual(map._controls.length, 0);
assert.strictEqual(map._events.overlayadd.length, 0);
assert.strictEqual(map._events.overlayremove.length, 0);

console.log(
  "Polygon disclosure layer-owned lifecycle/clear/idempotence fixture: PASS"
);
