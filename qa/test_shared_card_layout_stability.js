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
  const listeners = {};
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
      if (child.parentNode && child.parentNode !== this) {
        child.parentNode.removeChild(child);
      }
      child.parentNode = this;
      child.parentElement = this;
      if (!this.childNodes.includes(child)) this.childNodes.push(child);
      if (!this.children.includes(child)) this.children.push(child);
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
      (listeners[event.type] || []).slice().forEach(
        (handler) => handler(event)
      );
    },
    listenerCount(name) {
      return (listeners[name] || []).length;
    },
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
mapContainer.appendChild(corner);
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
function cornerBottomPx() {
  const managedBottom = parseFloat(
    corner.style.getPropertyValue("--pt-map-legend-bottom")
  );
  if (Number.isFinite(managedBottom)) return managedBottom;
  return corner.classList.contains("pt-map-legend-gap-managed") ? 4 : 0;
}
card.getBoundingClientRect = () => {
  const bottom = cornerBottomPx();
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

const secondCard = emptyNode("div", "pt-map-legend-card");
secondCard.getBoundingClientRect = () => {
  const bottom = cornerBottomPx();
  const top = 396 - bottom;
  return {
    left: 8,
    right: 308,
    top,
    bottom: top + 80,
    width: 300,
    height: 80
  };
};
const visibleCards = [card];
corner.querySelectorAll = (selector) => (
  selector === ".pt-map-legend-card" ? visibleCards.slice() : []
);
corner.contains = (candidate) => visibleCards.includes(candidate);

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
  candidate === secondCard ||
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

const documentEvents = emptyNode("document");
global.document = {
  head: documentHead,
  createElement: (tagName) => emptyNode(tagName),
  getElementById: (id) => documentById[id] || null,
  addEventListener: documentEvents.addEventListener.bind(documentEvents),
  removeEventListener: documentEvents.removeEventListener.bind(documentEvents),
  dispatchEvent: documentEvents.dispatchEvent.bind(documentEvents)
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

const timeoutCallbacks = [];
function setTimeoutMock(callback, delay = 0) {
  timeoutCallbacks.push({callback, delay});
  return timeoutCallbacks.length;
}
function flushTimeouts() {
  const pending = timeoutCallbacks.splice(0).sort(
    (a, b) => a.delay - b.delay
  );
  pending.forEach(({callback}) => callback());
}

global.MutationObserver = class {
  constructor(callback) {
    mutationCallback = callback;
  }
  observe() {}
  disconnect() {}
};

const resizeObservers = [];
class ResizeObserverMock {
  constructor(callback) {
    this.callback = callback;
    resizeObservers.push(this);
  }
  observe() {}
  disconnect() {}
}

global.window = {
  BRIM: {},
  ResizeObserver: ResizeObserverMock,
  requestAnimationFrame,
  cancelAnimationFrame: (id) => frameCallbacks.delete(id),
  setTimeout: setTimeoutMock,
  clearTimeout() {},
  addEventListener() {},
  getComputedStyle(node) {
    if (node === corner) {
      return {
        display: "block",
        bottom:
          corner.style.getPropertyValue("--pt-map-legend-bottom") ||
          (corner.classList.contains("pt-map-legend-gap-managed")
            ? "4px"
            : "0px")
      };
    }
    return {display: "block", bottom: "0px"};
  }
};

global.setTimeout = setTimeoutMock;
global.clearTimeout = () => {};

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

const authoritativeInitialWrites = cornerStyleWrites;
flushTimeouts();
assert.strictEqual(frameCallbacks.size, 1);
flushFrames();
assert.strictEqual(
  corner.style.getPropertyValue("--pt-map-legend-bottom"),
  "134px",
  "a later startup pass corrected a mismatched initial bottom offset"
);
assert.strictEqual(
  cornerStyleWrites,
  authoritativeInitialWrites,
  "a later startup pass exposed a second initial stack position"
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
resizeObservers.forEach((observer) => observer.callback());
assert.strictEqual(frameCallbacks.size, 1);
flushFrames();
assert.strictEqual(
  cornerStyleWrites,
  firstStableWrites,
  "ResizeObserver activity rewrote a settled stack position"
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

visibleCards.push(secondCard);
window.BRIM.legendCloseout.scheduleLayout(secondCard);
assert.strictEqual(frameCallbacks.size, 1);
flushFrames();
assert.strictEqual(
  corner.style.getPropertyValue("--pt-map-legend-bottom"),
  "90px",
  "two lower-left cards did not settle as one centered stack"
);
assert.strictEqual(
  secondCard.getBoundingClientRect().top,
  306,
  "the two-card stack did not settle at the authoritative safe-gap top"
);
const twoCardStableWrites = cornerStyleWrites;
window.BRIM.legendCloseout.scheduleLayout();
flushFrames();
assert.strictEqual(
  cornerStyleWrites,
  twoCardStableWrites,
  "a settled two-card stack repeated an identical style write"
);
assert.strictEqual(
  /\./.test(corner.style.getPropertyValue("--pt-map-legend-bottom")),
  false,
  "lower-left positioning retained a fractional CSS pixel"
);
flushMutations();
assert.strictEqual(
  frameCallbacks.size,
  0,
  "the settled two-card stack left an observer-driven animation frame"
);
const finalLayoutStats = window.BRIM.legendCloseout.layoutStats();
assert.strictEqual(finalLayoutStats.framePending, false);
assert.strictEqual(finalLayoutStats.styleWriteCount, 3);

const detachableCard = emptyNode("div", "pt-map-legend-card");
detachableCard.style = makeStyle();
detachableCard.getBoundingClientRect = () => {
  const floating = detachableCard.classList.contains("pt-map-card-undocked");
  const left = floating
    ? parseFloat(detachableCard.style.getPropertyValue("left")) || 0
    : 8;
  const top = floating
    ? parseFloat(detachableCard.style.getPropertyValue("top")) || 0
    : 320;
  return {left, top, right: left + 220, bottom: top + 120, width: 220, height: 120};
};
const dockButton = emptyNode("button", "pt-map-card-dock test-dock");
const closeButton = emptyNode("button", "test-close");
const dragHandle = emptyNode("div", "pt-map-card-handle");
detachableCard.appendChild(dragHandle);
detachableCard.appendChild(dockButton);
detachableCard.appendChild(closeButton);
detachableCard.querySelector = (selector) => ({
  ".test-dock": dockButton,
  ".test-close": closeButton,
  ".pt-map-card-handle": dragHandle
}[selector] || null);
corner.appendChild(detachableCard);

const detachableState = window.BRIM.legendCloseout.makeDetachable({
  card: detachableCard,
  map,
  handleSelector: ".pt-map-card-handle",
  dockSelector: ".test-dock",
  label: "test card"
});
assert.ok(detachableState);
assert.strictEqual(dockButton.listenerCount("click"), 1);
assert.strictEqual(dragHandle.listenerCount("pointerdown"), 1);
dockButton.dispatchEvent({
  type: "click",
  preventDefault() {},
  stopPropagation() {}
});
assert.strictEqual(detachableState.floating, true);
assert.strictEqual(detachableCard.parentNode, mapContainer);
assert.strictEqual(
  detachableCard.classList.contains("pt-map-card-undocked"),
  true
);

dragHandle.dispatchEvent({
  type: "pointerdown",
  target: dragHandle,
  button: 0,
  clientX: 20,
  clientY: 330,
  preventDefault() {},
  stopPropagation() {}
});
document.dispatchEvent({
  type: "pointermove",
  clientX: 65,
  clientY: 365,
  preventDefault() {}
});
document.dispatchEvent({type: "pointerup"});
assert.strictEqual(detachableCard.style.getPropertyValue("left"), "53px");
assert.strictEqual(detachableCard.style.getPropertyValue("top"), "355px");

dockButton.dispatchEvent({
  type: "click",
  preventDefault() {},
  stopPropagation() {}
});
assert.strictEqual(detachableState.floating, false);
assert.strictEqual(detachableCard.parentNode, corner);
assert.strictEqual(
  detachableCard.classList.contains("pt-map-card-undocked"),
  false
);

let closeCallbackCount = 0;
window.BRIM.legendCloseout.wire(
  detachableCard,
  ".test-close",
  () => { closeCallbackCount += 1; }
);
closeButton.dispatchEvent({
  type: "click",
  preventDefault() {},
  stopPropagation() {}
});
assert.strictEqual(closeCallbackCount, 1);
assert.strictEqual(detachableCard.style.display, "none");
detachableState.destroy(false);
assert.strictEqual(dockButton.listenerCount("click"), 0);
assert.strictEqual(dragHandle.listenerCount("pointerdown"), 0);

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
