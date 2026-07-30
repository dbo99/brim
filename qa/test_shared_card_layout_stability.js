const assert = require("assert");
const fs = require("fs");
const path = require("path");

const repositoryRoot = path.join(__dirname, "..");
const helperSource = fs.readFileSync(
  path.join(
    repositoryRoot,
    "03_functions",
    "js",
    "brim_legend_closeout_helpers.js"
  ),
  "utf8"
);
const installSharedCards = new Function(`return (${helperSource}\n);`)();

function classList(initial = "") {
  const names = new Set(String(initial).split(/\s+/).filter(Boolean));
  return {
    add(...values) {
      values.forEach((value) => names.add(value));
    },
    remove(...values) {
      values.forEach((value) => names.delete(value));
    },
    contains(value) {
      return names.has(value);
    },
    toString() {
      return Array.from(names).join(" ");
    }
  };
}

let pendingMutationRecords = [];
let mutationCallback = null;

function makeStyle(onWrite) {
  const values = {};
  return {
    get cssText() {
      return Object.keys(values)
        .sort()
        .map((name) => `${name}:${values[name]}`)
        .join(";");
    },
    set cssText(value) {
      Object.keys(values).forEach((name) => delete values[name]);
      if (value) values.__raw = String(value);
      if (onWrite) onWrite();
    },
    getPropertyValue(name) {
      return values[name] || "";
    },
    setProperty(name, value) {
      if (values[name] === String(value)) return;
      values[name] = String(value);
      if (onWrite) onWrite();
    }
  };
}

function emptyNode(tagName = "div", initialClass = "") {
  const node = {
    nodeType: 1,
    tagName: tagName.toUpperCase(),
    id: "",
    parentNode: null,
    parentElement: null,
    childNodes: [],
    children: [],
    classList: classList(initialClass),
    style: makeStyle(),
    textContent: "",
    appendChild(child) {
      child.parentNode = this;
      child.parentElement = this;
      this.childNodes.push(child);
      this.children.push(child);
      return child;
    },
    insertBefore(child) {
      return this.appendChild(child);
    },
    removeChild(child) {
      this.childNodes = this.childNodes.filter((candidate) => candidate !== child);
      this.children = this.children.filter((candidate) => candidate !== child);
      child.parentNode = null;
      child.parentElement = null;
      return child;
    },
    setAttribute(name, value) {
      this[name] = String(value);
    },
    getAttribute(name) {
      return this[name] === undefined ? null : this[name];
    },
    addEventListener() {},
    removeEventListener() {},
    querySelector() {
      return null;
    },
    querySelectorAll() {
      return [];
    },
    contains(candidate) {
      return candidate === this || this.children.some(
        (child) => child.contains && child.contains(candidate)
      );
    },
    closest() {
      return null;
    },
    matches() {
      return false;
    }
  };
  return node;
}

const documentById = {};
const documentHead = emptyNode("head");
const originalHeadAppend = documentHead.appendChild.bind(documentHead);
documentHead.appendChild = function(child) {
  originalHeadAppend(child);
  if (child.id) documentById[child.id] = child;
  return child;
};

const mapContainer = emptyNode("div", "leaflet-container");
mapContainer.getBoundingClientRect = () => ({
  left: 0,
  top: 0,
  right: 800,
  bottom: 600,
  width: 800,
  height: 600
});

let cornerStyleWrites = 0;
const corner = emptyNode("div", "leaflet-bottom leaflet-left");
corner.scrollTop = 0;
corner.style = makeStyle(() => {
  cornerStyleWrites += 1;
  pendingMutationRecords.push({
    type: "attributes",
    attributeName: "style",
    target: corner,
    removedNodes: []
  });
});

const card = emptyNode("div", "pt-map-legend-card");
card.getBoundingClientRect = () => {
  const bottomCss = parseFloat(
    corner.style.getPropertyValue("--pt-map-legend-bottom")
  );
  const bottom = Number.isFinite(bottomCss) ? bottomCss : 4;
  const top = 484 - bottom;
  return {
    left: 8,
    right: 308,
    top,
    bottom: top + 100,
    width: 300,
    height: 100
  };
};

corner.querySelectorAll = (selector) => (
  selector === ".pt-map-legend-card" ? [card] : []
);
corner.contains = (candidate) => candidate === card;

const externalWrap = emptyNode("div");
externalWrap.id = "pt-tools-adddata-wrap";
externalWrap.getBoundingClientRect = () => ({
  left: 0,
  top: 0,
  right: 300,
  bottom: 200,
  width: 300,
  height: 200
});
documentById[externalWrap.id] = externalWrap;

mapContainer.contains = (candidate) => (
  candidate === corner ||
  candidate === card ||
  candidate === externalWrap
);
mapContainer.querySelectorAll = (selector) => {
  if (
    selector ===
    ".leaflet-control-container > .leaflet-top, .leaflet-control-container > .leaflet-bottom"
  ) {
    return [corner];
  }
  return [];
};

global.document = {
  head: documentHead,
  createElement: (tagName) => emptyNode(tagName),
  getElementById: (id) => documentById[id] || null,
  addEventListener() {},
  removeEventListener() {}
};

const frameCallbacks = new Map();
let nextFrameId = 1;
function requestAnimationFrame(callback) {
  const id = nextFrameId++;
  frameCallbacks.set(id, callback);
  return id;
}
function flushFrames() {
  const callbacks = Array.from(frameCallbacks.values());
  frameCallbacks.clear();
  callbacks.forEach((callback) => callback());
}
function flushMutations() {
  const records = pendingMutationRecords;
  pendingMutationRecords = [];
  if (records.length && mutationCallback) mutationCallback(records);
}

global.MutationObserver = class {
  constructor(callback) {
    mutationCallback = callback;
  }
  observe() {}
  disconnect() {}
};

class ResizeObserverMock {
  constructor(callback) {
    this.callback = callback;
  }
  observe() {}
  disconnect() {}
}

global.window = {
  BRIM: {},
  ResizeObserver: ResizeObserverMock,
  requestAnimationFrame,
  cancelAnimationFrame: (id) => frameCallbacks.delete(id),
  setTimeout: () => 1,
  clearTimeout() {},
  addEventListener() {},
  getComputedStyle(node) {
    if (node === corner) {
      return {
        display: "block",
        bottom:
          corner.style.getPropertyValue("--pt-map-legend-bottom") || "4px"
      };
    }
    return {display: "block", bottom: "0px"};
  }
};

const map = {
  getContainer: () => mapContainer,
  on() {},
  off() {}
};

installSharedCards.call(map, mapContainer, {});
assert.strictEqual(frameCallbacks.size, 1);
flushFrames();
assert.strictEqual(cornerStyleWrites, 2);
assert.strictEqual(
  corner.style.getPropertyValue("--pt-map-legend-bottom"),
  "134px"
);
assert.strictEqual(
  corner.style.getPropertyValue("--pt-map-legend-max-height"),
  "384px"
);

flushMutations();
assert.strictEqual(
  frameCallbacks.size,
  0,
  "shared layout reacted to its own stable style writes"
);

const firstStableWrites = cornerStyleWrites;
window.BRIM.legendCloseout.scheduleLayout();
assert.strictEqual(frameCallbacks.size, 1);
flushFrames();
assert.strictEqual(
  cornerStyleWrites,
  firstStableWrites,
  "an already-settled card rewrote its layout styles"
);
flushMutations();
assert.strictEqual(
  frameCallbacks.size,
  0,
  "an idle settled card left a recurring animation frame"
);

const layoutStats = window.BRIM.legendCloseout.layoutStats();
assert.strictEqual(layoutStats.framePending, false);
assert.ok(layoutStats.ignoredSelfMutationCount >= 2);
assert.strictEqual(layoutStats.styleWriteCount, 2);

assert.match(helperSource, /Math\.round\(value\)/);
assert.match(
  helperSource,
  /__brimLegendLayoutCssText[\s\S]*ignoredSelfMutationCount/
);
assert.match(helperSource, /scrollbar-gutter:stable/);
assert.match(
  helperSource,
  /card: '\.pt-huc-theme-card'[\s\S]*dock: '\.pt-huc-theme-dock'/
);
assert.strictEqual(
  helperSource.includes("card: '.pt-huc-theme-legend'"),
  false,
  "legacy separate HUC legend remains registered as a detachable card"
);

const calsimSource = fs.readFileSync(
  path.join(
    repositoryRoot,
    "03_functions",
    "js",
    "leaflet_calsim3_local_cluster.js"
  ),
  "utf8"
);
assert.match(
  calsimSource,
  /pt-calsim3-label-toggle[\s\S]*lbl[\s\S]*pt-calsim3-dock/
);

const referenceSource = fs.readFileSync(
  path.join(
    repositoryRoot,
    "03_functions",
    "leaflet_layer_local_reference_helpers.r"
  ),
  "utf8"
);
assert.match(referenceSource, /Central California District \(CCD\)/);
assert.match(
  referenceSource,
  /pt-blm-office-legend-row[^]*?align-items:center/
);

console.log("Shared detachable-card idle layout fixture: PASS");
