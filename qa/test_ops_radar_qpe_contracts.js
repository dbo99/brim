"use strict";
// Offline source fixture: no browser, network, image download, build, or file output.
// Metadata evidence accessed 2026-09-11 21:10:19–21:11:14 UTC:
// https://mapservices.weather.noaa.gov/raster/rest/services/obs/rfc_qpe/MapServer?f=pjson
// https://mapservices.weather.noaa.gov/raster/rest/services/obs/rfc_qpe/MapServer/legend?f=pjson
// Selected numeric IDs 32 (Today's Analysis), 40 (Last 3 days), 56 (Last 7 days).
// Each row below: original label, SHA256 of decoded embedded PNG bytes, RGBA.
// All 20x20 RGBA PNGs passed signature/chunk CRC/length/zlib/filter inspection.
// Solid swatches have a 16x16 opaque interior and 2px transparent margin;
// "Less than 0.01" is wholly transparent. No borders/patterns/antialiasing.
// Tuple SHA256 = UTF8 JSON.stringify(ordered rows.map(r => [r[0],r[1]])),
// no trailing newline. This is NOT a raw HTTP-response hash.
// Independent provider fixture; never populated from production legend data.
const assert = require("assert");
const fs = require("fs");
const path = require("path");
const vm = require("vm");
const crypto = require("crypto");
const {execFileSync} = require("child_process");
const BASE = "6832e14bd0144eb2e488890ccc8edfb8783436d8";
const a6Only = process.argv.includes("--a6-only");
const sourceRootArg = process.argv.slice(2).find(a => a.startsWith("--source-root="));
assert(process.argv.slice(2).every(a => a === "--baseline" || a === "--a6-only" || a === sourceRootArg), "Unknown option");
assert(!sourceRootArg || (a6Only && !process.argv.includes("--baseline")), "Source snapshots require --a6-only");
const baseline = process.argv.includes("--baseline");
const root = sourceRootArg ? path.resolve(sourceRootArg.slice("--source-root=".length)) : path.resolve(__dirname, "..");
const sha = value => crypto.createHash("sha256").update(value).digest("hex");
const gitArgs = ["--no-pager", "--no-optional-locks", "--no-replace-objects",
  "-c", "core.fsmonitor=false", "-c", "core.untrackedCache=false",
  "-c", "gc.auto=0", "-c", "maintenance.auto=false"];
function read(p) {
  return baseline ? execFileSync("git", [...gitArgs, "show", BASE + ":" + p], {
    cwd: root, encoding: "utf8", maxBuffer: 2000000,
    env: {...process.env, GIT_OPTIONAL_LOCKS: "0", GIT_NO_LAZY_FETCH: "1",
      GIT_ALLOW_PROTOCOL: "", GIT_TERMINAL_PROMPT: "0", LC_ALL: "C"}
  }) : fs.readFileSync(path.join(root, p), "utf8");
}
const files = {
  defs: "03_functions/leaflet_ops_live_layer_definition_helpers.r",
  arc: "03_functions/leaflet_ops_live_arcgis_export_helpers.r",
  legend: "03_functions/leaflet_ops_live_legend_helpers.r",
  shared: "03_functions/leaflet_ops_live_shared_helpers.r",
  service: "03_functions/leaflet_ops_live_service_helpers.r",
  panel: "03_functions/leaflet_ops_live_panel_helpers.r",
  closeout: "03_functions/js/brim_legend_closeout_helpers.js",
  tools: "03_functions/js/leaflet_tools_adddata_panel.js",
  delivery: "00_config/guide_product_resource_relationships.json",
  qpfTime: "03_functions/leaflet_ops_live_wpc_qpf_hover_helpers.r"
};
const source = Object.fromEntries(Object.entries(files).map(([k, p]) => [k, read(p)]));
console.log("MODE=" + (baseline ? "BASE_HEAD" : "WORKING") + " HARNESS_SHA256=" +
  sha(fs.readFileSync(__filename)));
Object.entries(files).forEach(([k, p]) => console.log("INPUT " + p + " " + sha(source[k])));
function unique(s, token) {
  const p = s.indexOf(token);
  assert(p >= 0 && p === s.lastIndexOf(token), "Missing/ambiguous extraction: " + token);
  return p;
}
function region(s, start, end) {
  const a = unique(s, start), b = unique(s, end);
  assert(b > a, "Reversed extraction");
  return s.slice(a, b);
}
function raw(s) {
  return region(s, 'r"---(', ')---"').slice('r"---('.length);
}
function fn(s, name) {
  const start = unique(s, "  function " + name + "(");
  const tail = s.slice(start), end = /\n  }\r?\n/.exec(tail);
  assert(end, "Missing function end: " + name);
  const result = tail.slice(0, end.index + end[0].length);
  new vm.Script("(" + result.trim() + ")");
  return result;
}
function handler(s, token, end) {
  const part = region(s, token, end);
  const a = part.indexOf("function("), b = part.indexOf("\n      });");
  assert(a >= 0 && b > a, "Missing listener body");
  const result = part.slice(a, b) + "\n      }";
  new vm.Script("(" + result + ")");
  return result;
}
const js = Object.fromEntries(["defs", "arc", "legend", "shared", "service", "panel"]
  .map(k => [k, raw(source[k])]));
// Parse the actual raw strings in their htmlwidgets function-body context.
Object.values(js).forEach(s => new vm.Script("(function(el,x){\n" + s + "\n})"));
const arc = region(js.arc, js.arc.includes("  var ptOpsForecastOwners =") ? "  var ptOpsForecastOwners =" : js.arc.includes("  var ptWpcQpfOwners =") ? "  var ptWpcQpfOwners =" : "  var ArcGISExportLayer =", "\n  // WPC Excessive Rainfall Outlook");
const registrationPrefix = region(js.defs, "  var opsLayers = [];", "  // Ops Live wrapper for selected curated External Layers catalog rows");
// E31 supersedes the 39 E26R1 satellite endpoint/default-date assertions.
// Their exact preimage and 119/0 baseline are retained in the E31 review evidence.
// GIBS full-capture and satellite lifecycle coverage lives in test_ops_gibs_imagery.js.
const radar = region(js.defs, "  if (includeRadar) {", baseline ? "\n  addOpsLayer({\n    category: 'Satellite / Imagery',\n    name: 'NOAA GOES GeoColor'" : "  // Five satellite products,");
const qpe = region(js.defs, "  addOpsLayer({category: 'Hydro Observations', subgroup: 'Precip / QPE', name: 'QPE | NWS MRMS 1-hr'",
  "\n  if (includeCocorahsDailyPrecip && COCORAHS_CA_DAILY_PRECIP_URL)");
const qpf = region(js.defs, "  addOpsLayer({\n    category: 'Forecasts / Outlooks',\n    subgroup: 'Weather Forecasts / Outlooks',\n    name: 'WPC QPF Day 1'",
  "\n  addOpsExternalLinks({\n    category: 'Forecasts / Outlooks',\n    subgroup: 'Weather Forecasts / Outlooks',\n    title: 'CNRFC QPF graphics'");
const toggle = handler(js.panel, "      chk.addEventListener('change', function() {",
  "\n    updateOpsHeaderCount();\n\n    var clearBtn");
const clearClick = handler(js.panel, "      clearRibbonBtn.addEventListener('click', function(e) {",
  "\n    opsDiv = body;");
function rStrings(name) {
  const section = region(source.defs, "  " + name + " <- c(", "\n  )\n\n  " +
    (name === "source_token" ? "stable_id" : "flag_by_token"));
  const values = [...section.matchAll(/^\s+("(?:[^"\\]|\\.)*"),?$/gm)].map(m => JSON.parse(m[1]));
  assert(a6Only ? [48, 49, 50].includes(values.length) : values.length === (baseline ? 48 : 50), "Authored identity vector changed");
  return values;
}
const names = rStrings("source_token"), ids = rStrings("stable_id");
const products = JSON.parse(source.delivery).products;
// Supply only the selected runtime registrations; unrelated Guide products have
// additional R compilation rules that this focused fixture does not emulate.
const projection = names.flatMap((source_token, i) => {
  if (!/^(Radar \||QPE \||WPC QPF )/.test(source_token)) return [];
  const matches = products.filter(p => p.product_id === ids[i]);
  assert.strictEqual(matches.length, 1, "Authored delivery join");
  return [{source_token, stable_id: ids[i], delivery_class: matches[0].delivery_class}];
});

const evidence = [
  {layerId: 32, tupleSha256: "32a95083a145906a45baac64dd0a554f00d366a9d687281ea1575c4f221158ae", rows: [
    ["Greater than or equal to 10","32bad36d15a7462d2ddf1a0ca6ea7c1d6c4423b5059ac987c5e2587b1f9a05ac",[220,220,220,255]],
    ["8 to 10","4f85bbd84403cb05e01ba7379826472feb70c53bff40a13d2c2c3cceb289a8e9",[125,75,225,255]],
    ["6 to 8","aa091dd781be8f82ef1a786178f10290a98b9c5b2b099fae919e82d61d297d78",[250,0,250,255]],
    ["5 to 6","a776d0d17f11d26da4f793f8ebbb7821853c8063a67880d831697eeb872d6877",[125,0,0,255]],
    ["4 to 5","26318283c2e39dfaa61f9c6d51db15c364e41d0a1af89335abfa64b79eae906a",[175,0,0,255]],
    ["3 to 4","0252216e297371a5c1ec2cfd6c1233bc4e05d466946eb356dc957ccaee9d9254",[250,0,0,255]],
    ["2.5 to 3","4f90408b7a603c16967505bde10e42082f6ac5e4eb02170724acc0871c6dbfc2",[250,150,0,255]],
    ["2 to 2.5","2ef32e98bd72e3ed776b16c59083b2b3c7ae65902f92f9d0291adfd728a24357",[255,217,102,255]],
    ["1.5 to 2","4f7f6b4de637453eb1bb2a4f2266441aa5c707453979eda96bc684ca0a769296",[250,250,0,255]],
    ["1 to 1.5","ab5b7ecc61f8f7608a7ac37287574c0853cc10d37472faeb0d88068c5c586fc9",[0,100,10,255]],
    ["0.75 to 1","ee4e7e1a4fcbb9d122261c341be9816e2891cf2e3bfc6a701b53ecbad7810e64",[0,160,15,255]],
    ["0.5 to 0.75","3eeb245158bd7bb4e4927b58b8ea3b4e5ee309336533ffc32a24d218cabf5f6c",[0,250,20,255]],
    ["0.25 to 0.5","b5524627dfad6b06cbdc5eb582eda24954ec9fc16987924ebf569e9ef5bfab6f",[0,20,50,255]],
    ["0.1  to  0.25","331eb64b9b0346ffd22941b225481a935c297741898eb4041996475c53e9c2f2",[61,133,198,255]],
    ["0.01 to 0.1","1ad692a389303255062ee8b338a7e7bd77a144597a9575958e859423c59f6c37",[20,200,250,255]],
    ["Less than 0.01","bef2d9be969bb3183116e5b0c0552e9c972ee0c8bbb2f26f5591b7dd33ec98d6",[0,0,0,0]],
    ["Missing data","6a201711347aba1d2cdd8edd591b99c822c265d93f361af70f092d03bf8a262b",[125,125,125,255]],
  ]},
  // Layer 40 independently retrieved/decoded 2026-09-14 UTC; its own tuples
  // happen to match layer 56. They were not inferred from another palette.
  {layerId: 40, tupleSha256: "05b6cbddb02de1dcc452973b91648e17009a3631a44893f7c926c32af6cc4047", rows: [
    ["Greater than or equal to 20","1ff6b3611e784879e14f54284d92c6b37e20f9f23e88e23cbcb8580651eda490",[219,219,219,255]],
    ["15 to 20","c563763131a64cc494e06f263ef59f1e2ad2b2ea7fe006d9fde97750733d4f4d",[124,74,224,255]],
    ["10 to 15","aa091dd781be8f82ef1a786178f10290a98b9c5b2b099fae919e82d61d297d78",[250,0,250,255]],
    ["8 to 10","a776d0d17f11d26da4f793f8ebbb7821853c8063a67880d831697eeb872d6877",[125,0,0,255]],
    ["6 to 8","03416498784d53f1b36b31ce829f44c6b2a14eb165972be2f977f02f2dd21833",[176,0,0,255]],
    ["5 to 6","0252216e297371a5c1ec2cfd6c1233bc4e05d466946eb356dc957ccaee9d9254",[250,0,0,255]],
    ["4 to 5","4f90408b7a603c16967505bde10e42082f6ac5e4eb02170724acc0871c6dbfc2",[250,150,0,255]],
    ["3 to 4","2ef32e98bd72e3ed776b16c59083b2b3c7ae65902f92f9d0291adfd728a24357",[255,217,102,255]],
    ["2 to 3","4f7f6b4de637453eb1bb2a4f2266441aa5c707453979eda96bc684ca0a769296",[250,250,0,255]],
    ["1.5 to 2","9f220fc8928dc0e083683a48d4c9affce6456889cc61027c1f2e4c75fa1bacb4",[0,99,10,255]],
    ["1 to 1.5","6b5cf688d026501bf1b85b8bb8cecc99c1dfb1743601f6cfcd70c68292f5eeb9",[0,161,16,255]],
    ["0.5 to 1","000cf707f9d5b8cad464211cb2df6785520f07c1262bee606190efd10c44ce19",[0,250,21,255]],
    ["0.25 to 0.5","8d16ce8ba96668e7c80635a3c7c84499443523b930087f9589fb5747ea7aebf0",[0,20,150,255]],
    ["0.1 to 0.25","a146f3f62fc8a144486d6e1c5a3c873b0214dfac1aa06d0cfa43e8fc0786db64",[62,135,199,255]],
    ["0.01 to 0.1","1ad692a389303255062ee8b338a7e7bd77a144597a9575958e859423c59f6c37",[20,200,250,255]],
    ["Less than 0.01","bef2d9be969bb3183116e5b0c0552e9c972ee0c8bbb2f26f5591b7dd33ec98d6",[0,0,0,0]],
    ["Missing Data","6a201711347aba1d2cdd8edd591b99c822c265d93f361af70f092d03bf8a262b",[125,125,125,255]]
  ]},
  {layerId: 56, tupleSha256: "05b6cbddb02de1dcc452973b91648e17009a3631a44893f7c926c32af6cc4047", rows: [
    ["Greater than or equal to 20","1ff6b3611e784879e14f54284d92c6b37e20f9f23e88e23cbcb8580651eda490",[219,219,219,255]],
    ["15 to 20","c563763131a64cc494e06f263ef59f1e2ad2b2ea7fe006d9fde97750733d4f4d",[124,74,224,255]],
    ["10 to 15","aa091dd781be8f82ef1a786178f10290a98b9c5b2b099fae919e82d61d297d78",[250,0,250,255]],
    ["8 to 10","a776d0d17f11d26da4f793f8ebbb7821853c8063a67880d831697eeb872d6877",[125,0,0,255]],
    ["6 to 8","03416498784d53f1b36b31ce829f44c6b2a14eb165972be2f977f02f2dd21833",[176,0,0,255]],
    ["5 to 6","0252216e297371a5c1ec2cfd6c1233bc4e05d466946eb356dc957ccaee9d9254",[250,0,0,255]],
    ["4 to 5","4f90408b7a603c16967505bde10e42082f6ac5e4eb02170724acc0871c6dbfc2",[250,150,0,255]],
    ["3 to 4","2ef32e98bd72e3ed776b16c59083b2b3c7ae65902f92f9d0291adfd728a24357",[255,217,102,255]],
    ["2 to 3","4f7f6b4de637453eb1bb2a4f2266441aa5c707453979eda96bc684ca0a769296",[250,250,0,255]],
    ["1.5 to 2","9f220fc8928dc0e083683a48d4c9affce6456889cc61027c1f2e4c75fa1bacb4",[0,99,10,255]],
    ["1 to 1.5","6b5cf688d026501bf1b85b8bb8cecc99c1dfb1743601f6cfcd70c68292f5eeb9",[0,161,16,255]],
    ["0.5 to 1","000cf707f9d5b8cad464211cb2df6785520f07c1262bee606190efd10c44ce19",[0,250,21,255]],
    ["0.25 to 0.5","8d16ce8ba96668e7c80635a3c7c84499443523b930087f9589fb5747ea7aebf0",[0,20,150,255]],
    ["0.1 to 0.25","a146f3f62fc8a144486d6e1c5a3c873b0214dfac1aa06d0cfa43e8fc0786db64",[62,135,199,255]],
    ["0.01 to 0.1","1ad692a389303255062ee8b338a7e7bd77a144597a9575958e859423c59f6c37",[20,200,250,255]],
    ["Less than 0.01","bef2d9be969bb3183116e5b0c0552e9c972ee0c8bbb2f26f5591b7dd33ec98d6",[0,0,0,0]],
    ["Missing Data","6a201711347aba1d2cdd8edd591b99c822c265d93f361af70f092d03bf8a262b",[125,125,125,255]],
  ]}
];

// Small generic DOM/Leaflet boundary: real parent/sibling movement, event identity,
// selectors, style storage and HTML parsing. Geometry is deterministic, not a browser.
function event(type, values = {}) {
  return {type, preventDefault() { this.defaultPrevented = true; },
    stopPropagation() { this.stopped = true; }, ...values};
}
function events(target) {
  const listeners = [];
  target.addEventListener = (type, f, capture = false) => {
    if (!listeners.some(x => x.type === type && x.f === f && x.capture === capture))
      listeners.push({type, f, capture});
  };
  target.removeEventListener = (type, f, capture = false) => {
    const i = listeners.findIndex(x => x.type === type && x.f === f && x.capture === capture);
    if (i >= 0) listeners.splice(i, 1);
  };
  target.dispatchEvent = e => {
    e.target = e.target || target;
    listeners.filter(x => x.type === e.type).slice().forEach(x => x.f.call(target, e));
    if (!e.stopped && e.bubbles && target.parentNode) target.parentNode.dispatchEvent(e);
    return !e.defaultPrevented;
  };
  target.listenerCount = () => listeners.length;
  return target;
}
function style() {
  const values = {};
  const api = {
    getPropertyValue: k => values[k] || "",
    setProperty: (k, v) => { values[k] = String(v); },
    removeProperty: k => { delete values[k]; }
  };
  Object.defineProperty(api, "cssText", {get: () => Object.entries(values).map(x => x.join(":")).join(";"),
    set: text => {
      Object.keys(values).forEach(k => delete values[k]);
      String(text).split(";").forEach(v => {
        const i = v.indexOf(":"); if (i > 0) values[v.slice(0, i).trim()] = v.slice(i + 1).trim();
      });
    }});
  return new Proxy(api, {get: (o, k) => k in o ? o[k] : values[k] || "",
    set: (o, k, v) => { if (k in o) o[k] = v; else values[k] = String(v); return true; }});
}
const decodeText = s => s.replace(/&(?:amp|lt|gt|quot|#039);/g,
  v => ({"&amp;": "&", "&lt;": "<", "&gt;": ">", "&quot;": '"', "&#039;": "'"}[v]));
function node(tag = "div") {
  const attrs = {}, n = events({nodeType: 1, tagName: tag.toUpperCase(), childNodes: [],
    parentNode: null, style: style(), scrollTop: 0});
  if (n.tagName === "INPUT") n.checked = false;
  Object.defineProperties(n, {
    parentElement: {get: () => n.parentNode},
    children: {get: () => n.childNodes.filter(c => c.nodeType === 1)},
    nextSibling: {get: () => n.parentNode?.childNodes[n.parentNode.childNodes.indexOf(n) + 1] || null},
    previousSibling: {get: () => n.parentNode?.childNodes[n.parentNode.childNodes.indexOf(n) - 1] || null},
    className: {get: () => attrs.class || "", set: v => { attrs.class = v; }},
    id: {get: () => attrs.id || "", set: v => { attrs.id = v; }},
    textContent: {get: () => n.childNodes.map(c => c.textContent).join(""),
      set: v => { n.childNodes.forEach(c => { c.parentNode = null; });
        n.childNodes = [{nodeType: 3, textContent: String(v), parentNode: n}]; }},
    innerHTML: {get: () => n._html || "", set: html => {
      n._html = String(html); n.childNodes.forEach(c => { c.parentNode = null; }); n.childNodes = [];
      const stack = [n];
      for (const m of String(html).matchAll(/<\/?[^>]+>|[^<]+/g)) {
        const t = m[0], parent = stack[stack.length - 1];
        if (t.startsWith("</")) { assert(stack.length > 1, "HTML close mismatch"); stack.pop(); }
        else if (t.startsWith("<")) {
          const tagMatch = /^<([\w-]+)/.exec(t); assert(tagMatch, "Unsupported HTML");
          const el = node(tagMatch[1]);
          for (const a of t.matchAll(/([\w-]+)="([^"]*)"/g)) el.setAttribute(a[1], decodeText(a[2]));
          parent.appendChild(el);
          if (!/^(BR|INPUT|IMG|HR)$/.test(el.tagName) && !t.endsWith("/>")) stack.push(el);
        } else parent.childNodes.push({nodeType: 3, textContent: decodeText(t), parentNode: parent});
      }
      assert.strictEqual(stack.length, 1, "HTML unclosed element");
    }}
  });
  n.classList = {
    contains: c => n.className.split(/\s+/).includes(c),
    add: (...cs) => { n.className = [...new Set(n.className.split(/\s+/).filter(Boolean).concat(cs))].join(" "); },
    remove: (...cs) => { n.className = n.className.split(/\s+/).filter(c => !cs.includes(c)).join(" "); },
    toggle: (c, on) => { if (on === undefined) on = !n.classList.contains(c); n.classList[on ? "add" : "remove"](c); }
  };
  n.setAttribute = (k, v) => { attrs[k] = String(v); if (k === "style") n.style.cssText = v; };
  n.getAttribute = k => attrs[k] === undefined ? null : attrs[k];
  n.removeAttribute = k => { delete attrs[k]; };
  n.appendChild = c => n.insertBefore(c, null);
  n.insertBefore = (c, before) => {
    if (c.parentNode) c.parentNode.removeChild(c);
    const i = before ? n.childNodes.indexOf(before) : n.childNodes.length;
    assert(i >= 0, "Missing insertBefore sibling");
    n.childNodes.splice(i, 0, c); c.parentNode = n; return c;
  };
  n.removeChild = c => { const i = n.childNodes.indexOf(c); assert(i >= 0, "Missing child");
    n.childNodes.splice(i, 1); c.parentNode = null; return c; };
  n.contains = c => c === n || n.children.some(x => x.contains(c));
  n.matches = selector => selector.split(",").some(part => {
    part = part.trim();
    const rel = /^(.*?)\s*(>)\s*([^>]+)$/.exec(part);
    if (rel) return n.matches(rel[3]) && !!n.parentElement?.matches(rel[1]);
    const tagMatch = /^[\w-]+/.exec(part);
    if (tagMatch && n.tagName !== tagMatch[0].toUpperCase()) return false;
    if ([...part.matchAll(/\.([\w-]+)/g)].some(m => !n.classList.contains(m[1]))) return false;
    if ([...part.matchAll(/#([\w-]+)/g)].some(m => n.id !== m[1])) return false;
    if ([...part.matchAll(/\[([\w-]+)(?:="([^"]*)")?\]/g)]
      .some(m => m[2] === undefined ? n.getAttribute(m[1]) === null : n.getAttribute(m[1]) !== m[2])) return false;
    return true;
  });
  n.closest = s => n.matches(s) ? n : n.parentElement?.closest(s) || null;
  n.querySelectorAll = s => n.children.flatMap(c => [...(c.matches(s) ? [c] : []), ...c.querySelectorAll(s)]);
  n.querySelector = s => n.querySelectorAll(s)[0] || null;
  n.click = () => n.dispatchEvent(event("click", {bubbles: true}));
  n.getBoundingClientRect = () => {
    const r = n.rect || {left: 8, top: 240, width: 330, height: 260};
    const left = n.classList.contains("pt-map-card-undocked") ? parseFloat(n.style.left) || 0 : r.left;
    const top = n.classList.contains("pt-map-card-undocked") ? parseFloat(n.style.top) || 0 : r.top;
    return {...r, left, top, right: left + r.width, bottom: top + r.height};
  };
  return n;
}
function fixture() {
  const document = events({documentElement: node("html"), head: node("head")});
  const container = node(); container.rect = {left: 0, top: 0, width: 800, height: 700};
  document.documentElement.appendChild(document.head); document.documentElement.appendChild(container);
  document.createElement = node;
  document.getElementById = id => document.documentElement.querySelector("#" + id);
  document.querySelector = s => document.documentElement.querySelector(s);
  document.querySelectorAll = s => document.documentElement.querySelectorAll(s);
  const controls = container.appendChild(node()); controls.className = "leaflet-control-container";
  const corner = controls.appendChild(node()); corner.className = "leaflet-bottom leaflet-left";
  const notes = container.appendChild(node()), status = container.appendChild(node());
  const count = container.appendChild(node()); count.id = "pt-ops-active-count";
  const opsButton = container.appendChild(node("button")); opsButton.id = "pt-ops-clear-ribbon-btn";
  const obstacle = container.appendChild(node()); obstacle.id = "pt-tools-adddata-wrap";
  obstacle.rect = {left: 0, top: 0, width: 300, height: 200};
  const upload = container.appendChild(node()); upload.id = "pt-local-upload-wrap";
  upload.rect = {left: 0, top: 600, width: 300, height: 100};
  const layers = new Set(), removals = [], requests = [], images = [], mapListeners = [];
  const timers = new Map(), frames = new Map(), mutations = [];
  let seq = 0, adds = 0, controlRemoves = 0;
  const map = {
    options: {crs: {project: p => p}}, getContainer: () => container,
    getSize: () => ({x: 800, y: 700}),
    getBounds: () => ({getSouthWest: () => ({x: -2, y: -1}), getNorthEast: () => ({x: 2, y: 1})}),
    getPane: () => null,
    on(types, f, ctx) { types.split(" ").forEach(t => mapListeners.push({t, f, ctx})); return this; },
    off(types, f, ctx) { for (let i = mapListeners.length - 1; i >= 0; i--)
      if (types.split(" ").includes(mapListeners[i].t) && mapListeners[i].f === f &&
          mapListeners[i].ctx === ctx) mapListeners.splice(i, 1); return this; },
    fire(t) { mapListeners.filter(x => x.t === t).slice().forEach(x => x.f.call(x.ctx || map)); },
    hasLayer: l => layers.has(l),
    addLayer(l) { if (!layers.has(l)) { layers.add(l); l.onAdd?.(map); l.fire?.("add"); } return map; },
    removeLayer(l) { removals.push(l); if (layers.delete(l)) { l.onRemove?.(map); l.fire?.("remove"); } return map; }
  };
  function imageLayer(url, bounds, options) {
    const l = events({url, bounds, options, addTo(m) { m.addLayer(this); return this; },
      on(t, f) { this.addEventListener(t, f); return this; },
      off(t, f) { this.removeEventListener(t, f); return this; }, fire(t) { this.dispatchEvent(event(t)); },
      setOpacity(value) { this.options.opacity = value; return this; }});
    images.push(l); return l;
  }
  const L = {
    // Model the inherited Leaflet Evented surface used by these owners.
    // on/off use exact handler identity; fire returns the layer and binds this.
    Layer: {extend(def) { function Layer(options) { events(this); if (this.initialize) this.initialize(options); }
      Layer.prototype = {...def, addTo(m) { m.addLayer(this); return this; },
        on(t, f) { this.addEventListener(t, f); return this; },
        off(t, f) { this.removeEventListener(t, f); return this; },
        fire(t, data) { this.dispatchEvent(event(t, data)); return this; }}; return Layer; }},
    imageOverlay: imageLayer, tileLayer: Object.assign((url, options) => {
      const l = imageLayer(url, null, options);
      l.redraw = function() { this.redraws = (this.redraws || 0) + 1; this.fire("loading"); return this; };
      return l;
    }, {wms: (url, options) => imageLayer(url, null, options)}),
    control(options) { return {options, addTo(m) { this._map = m; this._container = this.onAdd(m);
      this._container.classList.add("leaflet-control"); corner.insertBefore(this._container, corner.children[0] || null);
      adds++; return this; }, remove() { if (this._map) {
        this._container.parentNode?.removeChild(this._container); this.onRemove?.(this._map);
        this._map = null; controlRemoves++; } return this; }}; },
    DomUtil: {create(tag, cls) { const n = node(tag); n.className = cls; return n; }},
    DomEvent: {disableClickPropagation(n) { n.clickPropagationDisabled = true; },
      disableScrollPropagation(n) { n.scrollPropagationDisabled = true; },
      stop(e) { e.preventDefault(); e.stopPropagation(); }}
  };
  const window = events({BRIM: {}, getComputedStyle: n => ({display: n.style.display || "block",
    bottom: n.style.getPropertyValue("--pt-map-legend-bottom") || "0px"}),
    setTimeout(f, delay) { const id = ++seq; timers.set(id, {f, delay}); return id; },
    clearTimeout: id => timers.delete(id),
    requestAnimationFrame(f) { const id = ++seq; frames.set(id, f); return id; },
    cancelAnimationFrame: id => frames.delete(id)});
  const clears = [];
  const context = vm.createContext({window, document, L, map, URL, console: {
    log() {}, warn(...a) { throw new Error("Unexpected swallowed runtime error: " + a.join(" ")); }},
    setTimeout: window.setTimeout, clearTimeout: window.clearTimeout,
    MutationObserver: class { constructor(f) { mutations.push(f); } observe() {} disconnect() {} },
    Event: function(type, values) { return event(type, values); },
    fetch(url) {
      assert(/\/query\?f=json&where=1%3D1&outFields=product,valid_time|\/returnUpdates\?f=pjson&_=|\/query\?f=json&where=1%3D1&outFields=name,idp_validendtime/.test(url),
        "Unexpected offline request: " + url);
      let resolve; const p = new Promise(r => { resolve = r; }); requests.push({url, resolve}); return p;
    },
    ptGibsLayers: [],
    invalidateWpcQpfHover: () => clears.push(["hover-invalidate"]),
    OPS_DELIVERY_PROJECTION: projection, OPS_CATALOG_PRIMARY_PANEL: {}, includeRadar: true,
    legendDiv: notes, statusDiv: status,
    activateWpcQpfHover: (...a) => clears.push(["hover-on", ...a]),
    deactivateWpcQpfHover: (...a) => clears.push(["hover-off", ...a]),
    ptClearBtnPressOff: () => clears.push(["button-release"]),
    ...Object.fromEntries(["ptSetTeachingLabelPlacementActive", "ptSetTeachingMarkupMode",
      "ptClearMeasurements", "ptClearCustomLayers"].map(k => [k, () => clears.push([k])]))
  });
  const run = s => vm.runInContext(s, context, {timeout: 1000});
  run("(" + source.closeout + ").call(map, map.getContainer(), {});");
  run(region(js.shared, "  function fmtTime(", "  function eroLegendHtml("));
  ["activeOverlayLinksHtml", "linkHtml", "ptOpsCompactLinkLabel", "compactUrlForTitle",
    "ptOpsShouldDisplayLink"].forEach(n => run(fn(js.shared, n)));
  for (const k of ["MRMS", "RFC_QPE", "WPC_QPF", "NOAA_RADAR"]) {
    const lines = js.service.split("\n").filter(l => l.startsWith("  var " + k + " = "));
    assert.strictEqual(lines.length, 1); run(lines[0]);
  }
  ["mapServerLegendUrl", "checkReturnUpdates", "checkRfcQpe", "checkMrmsLatest",
    "checkWpcQpf", "checkNoaaRadar"].forEach(n => run(fn(js.service, n)));
  if (js.service.includes('function ptOpsRfcQpeCheckTime(')) {
    ["parseArcgisUtcDate", "formatLosAngelesCompactParts"].forEach(n => run(fn(raw(source.qpfTime), n)));
    run(fn(js.service, "ptOpsRfcQpeCheckTime"));
    if (js.service.includes("function ptOpsRfcQpeMetadata(")) run(fn(js.service, "ptOpsRfcQpeMetadata"));
  }
  ["ptOpsRadarMrmsMetadata", "ptOpsMrmsMetadataDate", "ptOpsRadarMrmsStatus"].forEach(n => {
    if (js.service.includes("function " + n + "(")) run(fn(js.service, n));
  });
  if (js.service.includes('function ptForecastDateParts(')) run(js.service.slice(js.service.indexOf('  function ptForecastDateParts(')));
  run(arc); run(fn(js.arc, "makeRadarLayer")); run(js.legend);
  run(registrationPrefix + radar + qpe + qpf);
  ["ptOpsDeliveryKind", "ptOpsDeliveryBadgeHtml", "ptOpsPrimaryTitleHtml", "ptClearOpsPaneDomArtifacts",
    "ptClearOpsLayers", "ptOpsActivateLayerByName", "ptOpsDeactivateLayerByName"].forEach(n => run(fn(js.panel, n)));
  run(fn(source.tools, "ptSetStatus"));
  run(fn(source.tools, "ptClearAllPt2SessionLayers"));
  function bindOpsDefinition(def) {
    const chk = node("input"), row = node(); row.appendChild(chk);
    context.checkboxByName[def.name] = chk; context.rowByName[def.name] = row; context.opsDefByName[def.name] = def;
    const bind = run("(function(chk, def){ chk.addEventListener('change', " + toggle + "); })");
    bind(chk, def);
  }
  context.opsLayers.forEach(bindOpsDefinition);
  opsButton.addEventListener("click", run("(" + clearClick + ")"));
  const base = {tag: "basemap"}, unrelated = {tag: "unrelated-overlay"};
  map.addLayer(base); map.addLayer(unrelated);
  function settle() {
    const pending = [...frames.values()]; frames.clear(); pending.forEach(f => f());
  }
  function card(i) {
    const found = container.querySelectorAll('[data-rfc-qpe-product-id="' + idsRFC[i] + '"]');
    assert.strictEqual(found.length, 1, "Expected one active RFC card: " + idsRFC[i]); return found[0];
  }
  return {context, run, bindOpsDefinition, map, container, corner, notes, status, document, window, images, requests,
    timers, frames, clears, mutations, settle, card, base, unrelated, layers, removals,
    mapListeners, counts: () => ({adds, removes: controlRemoves}),
    on(i) { assert(context.ptOpsActivateLayerByName(namesRFC[i]).ok); },
    off(i) { assert(context.ptOpsDeactivateLayerByName(namesRFC[i]).ok); },
    clear(all = false) { if (all) context.ptClearAllPt2SessionLayers(); else opsButton.click(); },
    unchanged() { assert(layers.has(base) && layers.has(unrelated));
      assert(!removals.includes(base) && !removals.includes(unrelated)); }
  };
}

const idsRFC = ["ops_qpe_rfc_1day", "ops_qpe_rfc_3day", "ops_qpe_rfc_7day"];
const imageIdsRFC = [32, 40, 56];
const namesRFC = ["QPE | NWS RFC mosaic 1-day", "QPE | NWS RFC mosaic 3-day", "QPE | NWS RFC mosaic 7-day"];
const normalize = s => s.replace(/\s+/g, " ").trim();
const cssColor = rgba => rgba[3] === 0 ? "transparent" :
  "#" + rgba.slice(0, 3).map(v => v.toString(16).padStart(2, "0")).join("");
const cases = [];
const test = (name, body) => cases.push({name, body});
test("exact RFC identity, selectors, opacity, delivery and refresh eligibility", f => {
  namesRFC.forEach((name, i) => {
    const d = f.context.opsDefByName[name], o = d.layer.options;
    assert.strictEqual(d.guideProductId, idsRFC[i]);
    assert.strictEqual(d.deliveryClass, "brim_enhanced");
    assert.strictEqual(f.context.ptOpsDeliveryKind(d), "enhanced");
    assert.strictEqual(o.name, name); assert.deepStrictEqual(Array.from(o.layers), [imageIdsRFC[i]]);
    assert.strictEqual(o.url, f.context.RFC_QPE); assert.strictEqual(o.opacity, 0.64);
    assert.strictEqual(o.checkFreshness, f.context.checkRfcQpe);
    assert.strictEqual(d.layer.refreshCurrentView, undefined);
    assert(d.layer._buildUrl().includes("&layers=show:" + imageIdsRFC[i] + "&_="));
  });
});
test("independent provider tuple evidence retains all 51 original label/PNG pairs", () => {
  evidence.forEach(e => {
    assert.strictEqual(e.rows.length, 17);
    assert.strictEqual(sha(JSON.stringify(e.rows.map(r => r.slice(0, 2)))), e.tupleSha256);
    assert.deepStrictEqual(e.rows[15][2], [0, 0, 0, 0]);
    assert.deepStrictEqual(e.rows[16][2], [125, 125, 125, 255]);
  });
  assert.notStrictEqual(evidence[0].rows[12][2][2], evidence[1].rows[12][2][2]);
});
for (let i = 0; i < 3; i++) {
  test(idsRFC[i] + " ascending numeric labels preserve every published pair", f => {
    f.on(i); const c = f.card(i), rows = c.querySelector(".pt-ops-rfc-qpe-scale").querySelectorAll(".pt-ops-rfc-qpe-row");
    const expected = evidence[i].rows.slice(0, 15).reverse();
    assert.strictEqual(rows.length, 15);
    rows.forEach((row, j) => {
      assert.strictEqual(row.getAttribute("title"), expected[j][0], "original provider semantics " + j);
      assert.strictEqual(row.querySelector(".pt-ops-rfc-qpe-color").style.background, cssColor(expected[j][2]), "paired RGBA " + j);
    });
    assert(c.textContent.includes("inches"));
    assert(c.textContent.includes(["Daily QPE", "3-day QPE", "7-day QPE"][i]));
    assert(!f.notes.textContent.includes("QPE / QPF colors")); f.unchanged();
  });
  test(idsRFC[i] + " keeps displayed interval Unverified after recent metadata and image completion", async f => {
    f.on(i); const c = f.card(i), initial = c.textContent;
    f.requests.forEach(r => r.resolve({json: () => Promise.resolve({
      fullUpdate: "2099-12-31T23:59:59Z", layers: [{id: imageIdsRFC[i], lastEditDate: 4102444799000}]
    })}));
    await Promise.resolve(); await Promise.resolve(); await Promise.resolve(); await Promise.resolve();
    f.context.opsDefByName[namesRFC[i]].layer._update();
    f.images.at(-1).fire("load"); f.context.redrawLegend();
    assert(!f.status.textContent.includes("2099"));
    assert(f.status.textContent.includes("Unverified"));
    assert.strictEqual(c.textContent, initial);
    assert(c.textContent.includes("Displayed accumulation interval: Unverified"));
    assert(!c.textContent.includes("2099")); assert.strictEqual(f.card(i), c);
  });
  test(idsRFC[i] + " close is independent, persists through redraw/map update, off/on restores", f => {
    f.on(i); const c = f.card(i);
    f.context.ptOpsActivateLayerByName('Radar | NOAA MRMS'); const other = cardE18(f, 'Radar | NOAA MRMS');
    const state = c.__brimDetachableState, before = f.counts();
    const close = c.querySelector(".pt-ops-rfc-qpe-close");
    assert.strictEqual(close.tagName, "BUTTON"); assert(close.getAttribute("aria-label"));
    close.click(); assert.strictEqual(c.style.display, "none");
    assert(f.context.activeLayers[namesRFC[i]]); assert.notStrictEqual(other.style.display, "none");
    for (let n = 0; n < 4; n++) {
      f.context.redrawLegend(); f.map.fire("moveend"); f.map.fire("zoomend"); f.map.fire("resize");
    }
    assert.strictEqual(f.card(i), c); assert.strictEqual(c.__brimDetachableState, state);
    assert.strictEqual(c.style.display, "none"); assert.deepStrictEqual(f.counts(), before);
    assert.strictEqual(close.listenerCount(), 1);
    f.off(i); assert(!f.container.contains(c)); assert.strictEqual(cardE18(f, "Radar | NOAA MRMS"), other);
    f.on(i); assert.notStrictEqual(f.card(i), c); assert.notStrictEqual(f.card(i).style.display, "none");
    assert.strictEqual(cardE18(f, "Radar | NOAA MRMS"), other); f.unchanged();
  });
}
test("sequential QPE scale plus radar/forecast cards retain independent docking state", f => {
  f.on(0); f.on(1); f.on(2); f.on(0);
  f.context.ptOpsActivateLayerByName('Radar | NOAA MRMS');
  f.context.ptOpsActivateLayerByName('WPC QPF Day 1');
  const a = f.card(0), b = cardE18(f,'Radar | NOAA MRMS'), state = a.__brimDetachableState;
  const third = f.container.querySelector('.pt-ops-forecast-card');
  assert.notStrictEqual(a, b); assert.notStrictEqual(b, third);
  assert.notStrictEqual(state, third.__brimDetachableState); assert.notStrictEqual(state, b.__brimDetachableState);
  const order = f.corner.children.slice(), dock = a.querySelector(".pt-ops-rfc-qpe-dock");
  dock.click(); assert(state.floating); assert.strictEqual(a.parentNode, f.container);
  assert(dock.getAttribute("aria-label").startsWith("Dock "));
  const count = f.counts(); f.context.redrawLegend(); f.map.fire("resize");
  assert.strictEqual(f.card(0), a); assert(state.floating); assert.deepStrictEqual(f.counts(), count);
  assert.strictEqual(dock.listenerCount(), 1);
  dock.click(); assert(!state.floating); assert.strictEqual(a.parentNode, f.corner);
  assert.deepStrictEqual(f.corner.children, order); f.unchanged();
});
test("actual drag hooks clamp to viewport and teardown removes document/map listeners", f => {
  f.on(0); const c = f.card(0), state = c.__brimDetachableState;
  c.querySelector(".pt-ops-rfc-qpe-dock").click();
  const handle = c.querySelector(".pt-map-card-handle");
  handle.dispatchEvent(event("pointerdown", {button: 0, clientX: 10, clientY: 10}));
  assert.strictEqual(f.document.listenerCount(), 3);
  f.document.dispatchEvent(event("pointermove", {clientX: 5000, clientY: -5000}));
  assert.strictEqual(c.style.left, "466px"); assert.strictEqual(c.style.top, "4px");
  f.off(0); assert(state.destroyed); assert.strictEqual(state.drag, null);
  assert.strictEqual(f.document.listenerCount(), 0); assert.strictEqual(handle.listenerCount(), 0);
  assert(!c.__brimDetachableState); assert.strictEqual(f.mapListeners.length, 1);
});
test("shared safe-gap layout, overflow, scheduled work and control/focus conventions", f => {
  f.on(0); f.context.ptOpsActivateLayerByName('Radar | NOAA MRMS'); const a = f.card(0), b = cardE18(f,'Radar | NOAA MRMS');
  a.rect = {left: 8, top: 200, width: 330, height: 260};
  b.rect = {left: 8, top: 468, width: 330, height: 260};
  f.window.BRIM.legendCloseout.scheduleLayout(a); f.settle();
  assert(f.corner.classList.contains("pt-map-legend-gap-managed"));
  assert(f.corner.classList.contains("pt-map-legend-corner-overflow"));
  assert.strictEqual(f.corner.style.getPropertyValue("--pt-map-legend-max-height"), "384px");
  assert.strictEqual(f.frames.size, 0);
  assert(a.clickPropagationDisabled && a.scrollPropagationDisabled);
  assert.strictEqual(a.querySelector(".pt-ops-rfc-qpe-close").getAttribute("type"), "button");
  assert(source.closeout.includes(":focus-visible{outline:2px solid"));
  assert(source.shared.includes(".pt-ops-rfc-qpe-card"));
});
test("RFC/MRMS replace each other while QPF retains its independent map guide", f => {
  f.on(0); const old=f.card(0),qpfName='WPC QPF Day 1',mrms='QPE | NWS MRMS 1-hr';
  f.context.ptOpsActivateLayerByName(qpfName); const forecast=f.container.querySelector('.pt-ops-forecast-card');
  f.context.ptOpsActivateLayerByName(mrms); assert(!f.container.contains(old));
  assert.strictEqual(f.container.querySelector('.pt-ops-forecast-card'),forecast);
  const m=cardE18(f,mrms); f.on(1); assert(!f.container.contains(m)); const r=f.card(1);
  f.context.ptOpsDeactivateLayerByName(qpfName); assert.strictEqual(f.card(1),r);
  assert(!f.notes.textContent.includes('QPE / QPF colors'));
  assert(f.clears.some(x=>x[0]==='hover-on'));assert(f.clears.some(x=>x[0]==='hover-off'));f.unchanged();
});
for (const all of [false, true]) for (const mode of ["docked", "floating", "hidden"]) {
  test((all ? "Clear All" : "Clear Ops") + " disposes " + mode + " cards, repeat/late callbacks stay empty", async f => {
    const cards=[],states=[];
    for(let i=0;i<3;i++) {
      f.on(i);const c=f.card(i);cards.push(c);states.push(c.__brimDetachableState);
      if(mode!=='docked')c.querySelector('.pt-ops-rfc-qpe-dock').click();
      if(mode==='hidden')c.querySelector('.pt-ops-rfc-qpe-close').click();
      f.context.opsDefByName[namesRFC[i]].layer._update();
    }
    f.clear(all); f.clear(all);
    f.images.forEach(img => { img.fire("load"); img.fire("error"); });
    f.requests.forEach(r => r.resolve({json: () => Promise.resolve({fullUpdate: "2099 recent"})}));
    await Promise.resolve(); await Promise.resolve(); await Promise.resolve(); await Promise.resolve();
    f.context.redrawLegend(); f.map.fire("resize"); f.settle();
    assert.strictEqual(f.container.querySelectorAll(".pt-ops-rfc-qpe-card").length, 0);
    states.forEach(s => assert(s.destroyed && !s.drag));
    cards.forEach(c => assert(!f.container.contains(c) && !c.__brimDetachableState));
    assert.strictEqual(f.mapListeners.length, 1); assert.strictEqual(f.document.listenerCount(), 0);
    assert.deepStrictEqual(f.counts(), {adds: 3, removes: 3});
    if (all) assert(f.clears.some(x => x[0] === "ptClearCustomLayers"));
    f.unchanged();
  });
}
test("repeated activation/close/dock/clear cycles do not accumulate roots or owned listeners", f => {
  for (let cycle = 0; cycle < 6; cycle++) {
    const cards=[];
    for(let i=0;i<3;i++) {
      f.on(i);const c=f.card(i);cards.push(c);
      if(i===0)c.querySelector('.pt-ops-rfc-qpe-dock').click();
      if(i===1)c.querySelector('.pt-ops-rfc-qpe-close').click();
    }
    f.clear(cycle % 2 === 0); f.settle();
    assert.strictEqual(f.mapListeners.length, 1);
    assert.strictEqual(f.document.listenerCount(), 0);
    assert.strictEqual(f.container.querySelectorAll(".pt-ops-rfc-qpe-card").length, 0);
    cards.forEach(c => {
      assert(!c.__brimDetachableState);
      assert.strictEqual(c.querySelector(".pt-ops-rfc-qpe-dock").listenerCount(), 0);
      assert.strictEqual(c.querySelector(".pt-map-card-handle").listenerCount(), 0);
    });
  }
  assert.deepStrictEqual(f.counts(), {adds: 18, removes: 18}); f.unchanged();
});
test("status-only entries cannot recreate cards after off or clear", f => {
  f.on(0); f.card(0); f.off(0);
  f.context.recordStatus(namesRFC[0], "recent successful metadata", "pt-ops-ok");
  f.context.redrawLegend(); assert.strictEqual(f.container.querySelectorAll(".pt-ops-rfc-qpe-card").length, 0);
  f.clear(); f.context.redrawLegend(); f.unchanged();
});
test("other radar/MRMS/QPF selectors, authored badges, requests and generic presentation", f => {
  const spec = [
    ["Radar | NOAA MRMS", "ops_radar_noaa_mrms", [3], "", 0.70],
    ["QPE | NWS MRMS 1-hr", "ops_qpe_mrms_1hr", "rft_1hr", "enhanced", 0.58],
    ["QPE | NWS MRMS 1-day", "ops_qpe_mrms_1day", "rft_24hr", "enhanced", 0.60],
    ["QPE | NWS MRMS 3-day", "ops_qpe_mrms_3day", "rft_72hr", "enhanced", 0.60],
    ...[["Day 1", 1, "day_1"], ["Day 2", 2, "day_2"], ["Day 3", 3, "day_3"],
      ["3-day", 9, "3day"], ["7-day", 11, "7day"]]
      .map(([n, id, suffix]) => ["WPC QPF " + n, "ops_wpc_qpf_" + suffix, [id], "enhanced", 0.64])
  ];
  spec.forEach(([name, id, selector, badge, opacity]) => {
    const d = f.context.opsDefByName[name], o = d.layer.options;
    assert.strictEqual(d.guideProductId, id); assert.strictEqual(f.context.ptOpsDeliveryKind(d), badge);
    assert.strictEqual(o.opacity, opacity);
    if (Array.isArray(selector)) assert.deepStrictEqual(Array.from(o.layers), selector);
    else assert.strictEqual(o.rasterFunction, selector);
    f.context.ptOpsActivateLayerByName(name); d.layer._update();
    assert.strictEqual(f.images.at(-1).options.opacity, opacity);
    assert(!f.container.querySelector(".pt-ops-rfc-qpe-card"));
    if (o.legendType === "qpe") assert(f.container.querySelector(".pt-ops-forecast-card"));
    f.context.ptOpsDeactivateLayerByName(name);
  });
  const iem = f.context.opsDefByName["Radar | IEM NEXRAD"];
  assert.strictEqual(iem.layer.options.layers, "nexrad-n0q-900913");
  assert.strictEqual(f.context.ptOpsDeliveryKind(iem), "");
  f.context.ptOpsActivateLayerByName(iem.name); iem.layer.fire("load");
  f.context.ptOpsDeactivateLayerByName(iem.name); f.unchanged();
});
test("unrelated existing map legend survives RFC close/off/clear ownership changes", f => {
  f.context.activeLegendDefs["reservoir fixture"] = {legendType: "reservoir_capacity"};
  f.context.redrawLegend();
  assert(f.container.querySelector(".pt-ops-map-legend").textContent.includes("Reservoir observed"));
  f.on(0); f.card(0).querySelector(".pt-ops-rfc-qpe-close").click(); f.off(0);
  assert(f.container.querySelector(".pt-ops-map-legend").textContent.includes("Reservoir observed"));
  f.clear(); f.unchanged();
});

test("A6 exact one-ID addition, 1→3→7 order and default-off lifecycle", f => {
  assert.strictEqual(ids.length, 50);
  const registered = f.context.opsLayers.filter(d => d.name.startsWith("QPE | NWS RFC mosaic"));
  assert.deepStrictEqual(Array.from(registered, d => d.name), namesRFC);
  assert.deepStrictEqual(Array.from(registered, d => d.guideProductId), idsRFC);
  assert.strictEqual(Object.keys(f.context.activeLayers).length, 0);
  idsRFC.forEach((id, i) => {
    const d = registered[i];
    assert.strictEqual(d.deliveryClass, "brim_enhanced");
    assert.strictEqual(f.context.ptOpsDeliveryKind(d), "enhanced");
    assert.deepStrictEqual(Array.from(d.layer.options.layers), [imageIdsRFC[i]]);
    assert.strictEqual(d.layer.options.opacity, 0.64);
    assert.strictEqual(products.filter(p => p.product_id === id).length, 1);
    assert.deepStrictEqual(products.find(p => p.product_id === id).resource_links, []);
  });
});
for (let i = 0; i < 3; i++) {
  test("A6 " + idsRFC[i] + " ascending compact key, internal threshold and separate missing data", f => {
    assert(f.context.opsDefByName[namesRFC[i]], "Expected RFC duration registration");
    f.on(i); const c = f.card(i), scale = c.querySelector(".pt-ops-rfc-qpe-scale");
    assert(scale);
    const rows = scale.querySelectorAll(".pt-ops-rfc-qpe-row");
    const expected = evidence[i].rows.slice(0, 15).reverse();
    assert.strictEqual(rows.length, 15);
    expected.forEach((r, j) => {
      assert.strictEqual(rows[j].getAttribute("title"), r[0]);
      assert.strictEqual(rows[j].querySelector(".pt-ops-rfc-qpe-color").style.background, cssColor(r[2]));
      const display = normalize(r[0]).replace("Greater than or equal to ", "≥").replace(" to ", "–");
      assert.strictEqual(normalize(rows[j].textContent), display);
    });
    assert.strictEqual(f.context.ptOpsRfcQpeLegends[idsRFC[i]].rows.length, 17);
    assert.strictEqual(f.context.ptOpsRfcQpeLegends[idsRFC[i]].rows[15][0], "Less than 0.01");
    const missing = c.querySelector(".pt-ops-rfc-qpe-missing");
    assert(missing && !scale.contains(missing));
    assert.strictEqual(normalize(missing.textContent), "Missing data");
    assert.strictEqual(missing.querySelector(".pt-ops-rfc-qpe-color").style.background, "#7d7d7d");
    assert(c.textContent.includes("Below 0.01 in omitted from this key."));
    assert(c.textContent.includes("4 a.m. PST / 5 a.m. PDT (12Z)"));
    assert(c.textContent.includes("Displayed accumulation interval: Unverified"));
    assert(c.textContent.includes(["RFC mosaic · Daily QPE", "RFC mosaic · 3-day QPE", "RFC mosaic · 7-day QPE"][i]));
    assert(!/no color means|no precipitation/i.test(c.textContent));
    if (!i) assert(c.textContent.includes("not a rolling last-24-hour product"));
    const before = {counts: f.counts(), requests: f.requests.length, images: f.images.length, mutations: f.mutations.length};
    for (let n = 0; n < 4; n++) { f.context.redrawLegend(); f.map.fire("resize"); }
    assert.strictEqual(f.card(i), c);
    assert.deepStrictEqual({counts: f.counts(), requests: f.requests.length, images: f.images.length, mutations: f.mutations.length}, before);
    f.off(i); f.unchanged();
  });
}
test("A6 responsive CSS uses row-major auto-fit without global restyling", () => {
  const css = source.shared.slice(source.shared.indexOf("/* RFC QPE cards"), source.shared.indexOf("/* Top-left map legend"));
  assert(css.includes("repeat(auto-fit, minmax(min(100%, 125px), 1fr))"));
  assert(css.includes("grid-auto-flow: row"));
  assert(css.includes("font-variant-numeric: tabular-nums"));
  assert(!/grid-auto-flow:\s*(column|dense)|\border\s*:/.test(css));
  assert(css.includes(".pt-ops-rfc-qpe-card summary:focus-visible"));
});
test("A6 check is honest about validity and creates no metadata request or timer", async f => {
  f.on(0); const c = f.card(0), initialScale = c.querySelector(".pt-ops-rfc-qpe-scale").textContent, requests = f.requests.length;
  const timers = f.timers.size;
  f.context.checkRfcQpe();
  await Promise.resolve();
  assert.strictEqual(f.requests.length, requests, "An unbound service check must not imply new frame metadata");
  assert.strictEqual(f.timers.size, timers);
  assert(f.status.textContent.includes("BRIM check:"));
  assert(f.status.textContent.includes("Provider publication/update: Unverified"));
  assert(f.status.textContent.includes("Expected service refresh"));
  assert(f.status.textContent.includes("not a data guarantee"));
  assert(!f.status.innerHTML.includes("pt-ops-ok"));
  // E4 exposes the check; scientific scale and interval stay unchanged.
  assert.strictEqual(c.querySelector(".pt-ops-rfc-qpe-scale").textContent, initialScale);
  assert.strictEqual(f.card(0), c);
  assert(c.textContent.includes("Displayed accumulation interval: Unverified"));
});
for (const [utc, date, clock, zone] of [
  ["2026-01-01T12:00:00Z", "1/1/2026", "4:00 AM", "PST"],
  ["2026-07-01T12:00:00Z", "7/1/2026", "5:00 AM", "PDT"],
  ["2026-03-07T12:00:00Z", "3/7/2026", "4:00 AM", "PST"],
  ["2026-03-08T12:00:00Z", "3/8/2026", "5:00 AM", "PDT"],
  ["2026-10-31T12:00:00Z", "10/31/2026", "5:00 AM", "PDT"],
  ["2026-11-01T12:00:00Z", "11/1/2026", "4:00 AM", "PST"],
  ["2025-12-25T12:00:00Z", "12/25/2025", "4:00 AM", "PST"],
  ["2026-02-26T12:00:00Z", "2/26/2026", "4:00 AM", "PST"],
  ["2026-03-01T12:00:00Z", "3/1/2026", "4:00 AM", "PST"]
]) {
  // Actual shared formatter, reached through RFC check presentation; these are
  // explicit fixture instants, not asserted dates of any provider image.
  test("A6 Pacific endpoint formatting " + utc + " independent of host timezone", f => {
    assert.strictEqual(typeof f.context.ptOpsRfcQpeCheckTime, "function");
    const text = f.context.ptOpsRfcQpeCheckTime(utc);
    assert(text.includes(date) && text.includes(clock) && text.includes(zone), text);
    assert(text.indexOf(zone) < text.indexOf("(" + utc + ")"));
    assert(text.endsWith("(" + utc + ")"), text);
    assert(!/Z UTC| UTC\)/.test(text), text);
  });
}
test("E8 RFC check preserves the original uppercase-Z instant without a redundant suffix", f => {
  for (const value of ["2026-07-01T12:00:00Z", "2026-07-01T12:00:00.000Z",
    "2026-07-01T12:00:00.456Z"]) {
    const text = f.context.ptOpsRfcQpeCheckTime(value);
    assert(text.includes("7/1/2026") && text.includes("5:00 AM PDT"), text);
    assert(text.endsWith("(" + value + ")"), text);
    assert(!/Z UTC| UTC\)/.test(text), text);
  }
  for (const value of ["2026-07-01T12:00:00z", "2026-07-01T12:00:00+00:00"]) {
    assert.strictEqual(f.context.ptOpsRfcQpeCheckTime(value), "Unverified");
  }
});
test("A6 invalid or absent check instants cannot acquire a Pacific date", f => {
  assert.strictEqual(typeof f.context.ptOpsRfcQpeCheckTime, "function");
  for (const value of [undefined, null, "", "invalid", "2026-02-30T12:00:00Z",
    "2026-01-01", "2026-01-01T12:00:00", NaN, Infinity, {}, 0]) {
    assert.strictEqual(f.context.ptOpsRfcQpeCheckTime(value), "Unverified");
  }
});

// RFC metadata uses the real check/render owners; no provider or browser is run.
function mountRfcWithoutCheck(f, i) {
  f.context.activeLegendDefs[namesRFC[i]] = {rfcQpeProductId: idsRFC[i]};
  f.context.redrawLegend();
  return f.card(i);
}
function fixedCheckClock(f, iso) {
  f.run("Date = class extends Date { constructor(...a) { super(...(a.length ? a : [" +
    JSON.stringify(iso) + "])); } };");
}
function metadataValue(card, key) {
  const span = card.querySelector('[data-rfc-qpe-metadata="' + key + '"]');
  assert(span, "Missing metadata field: " + key);
  return span.textContent;
}
for (let i = 0; i < 3; i++) {
  test("E4 " + idsRFC[i] + " one closed disclosure, honest initial shared context", f => {
    const c = mountRfcWithoutCheck(f, i), details = c.querySelectorAll("details");
    assert.strictEqual(details.length, 1);
    assert.strictEqual(details[0].querySelector("summary").textContent, "Source, method & timing");
    assert.strictEqual(details[0].getAttribute("open"), null);
    assert(!details[0].contains(c.querySelector(".pt-ops-rfc-qpe-interval")));
    assert.strictEqual(metadataValue(c, "method"), f.context.ptOpsRfcQpeLegends[idsRFC[i]].period);
    assert(metadataValue(c, "cutoff").includes("4 a.m. PST / 5 a.m. PDT (12Z)"));
    assert(metadataValue(c, "refresh").includes("hourly near :55"));
    assert(metadataValue(c, "refresh").includes("4 a.m.–1 p.m. PST / 5 a.m.–2 p.m. PDT (12–21 UTC)"));
    assert.strictEqual(metadataValue(c, "check"), "No check recorded");
    assert.strictEqual(metadataValue(c, "provider"), "Unverified");
    const context = metadataValue(c, "context");
    assert(context.includes("shared RFC service context"));
    assert(context.includes("not a data guarantee"));
    assert(context.includes("no frame-date metadata requested"));
    assert(!f.status.textContent.includes("NWS QPE Mosaic metadata"));
    assert.strictEqual(f.requests.length, 0);
  });
}
test("E4 real check updates all mounted values, preserving independent disclosure and focus", f => {
  const cards = idsRFC.map((_, i) => mountRfcWithoutCheck(f, i));
  const details = cards.map(c => c.querySelector("details"));
  details[0].open = true; details[1].open = false; details[2].open = true;
  f.document.activeElement = details[2].querySelector("summary");
  const focus = f.document.activeElement;
  const before = {counts: f.counts(), requests: f.requests.length, timers: f.timers.size,
    listeners: f.document.listenerCount(), mutations: f.mutations.length};
  fixedCheckClock(f, "2026-07-01T12:00:00Z");
  f.context.checkRfcQpe();
  cards.forEach((c, i) => {
    assert.strictEqual(f.card(i), c);
    assert.strictEqual(c.querySelector("details"), details[i]);
    assert.strictEqual(details[i].open, i !== 1);
    c.querySelectorAll("[data-rfc-qpe-metadata]").forEach(s =>
      assert(f.context.statusRows["NWS QPE Mosaic metadata"].msg.includes(s.textContent),
        "Panel/legend metadata value differs: " + s.textContent));
    assert(metadataValue(c, "check").includes("7/1/2026 5:00 AM PDT"));
    assert(metadataValue(c, "check").endsWith("(2026-07-01T12:00:00.000Z)"));
    assert(!/Z UTC| UTC\)/.test(metadataValue(c, "check")));
  });
  assert.strictEqual(f.document.activeElement, focus);
  assert.deepStrictEqual({counts: f.counts(), requests: f.requests.length, timers: f.timers.size,
    listeners: f.document.listenerCount(), mutations: f.mutations.length}, before);
});
test("E4 a check before card creation is reused; disclosure/redraw/docking do not check", f => {
  fixedCheckClock(f, "2026-01-01T12:00:00Z"); f.context.checkRfcQpe();
  const row = f.context.statusRows["NWS QPE Mosaic metadata"], msg = row.msg;
  fixedCheckClock(f, "2099-07-01T12:00:00Z");
  const cards = idsRFC.map((_, i) => mountRfcWithoutCheck(f, i));
  cards.forEach((c, i) => {
    const d = c.querySelector("details"); d.open = true;
    c.querySelector(".pt-ops-rfc-qpe-dock").click();
    c.querySelector(".pt-ops-rfc-qpe-dock").click();
    f.context.redrawLegend();
    assert.strictEqual(f.card(i), c); assert.strictEqual(d.open, true);
    assert(metadataValue(c, "check").includes("1/1/2026 4:00 AM PST"));
    assert(!metadataValue(c, "check").includes("2099"));
  });
  assert.strictEqual(f.context.statusRows["NWS QPE Mosaic metadata"], row);
  assert.strictEqual(row.msg, msg);
  assert.strictEqual(f.requests.length, 0);
});
test("E4 status updates do not revive closed/removed cards; Clear resets shared check", f => {
  f.on(0); const hidden = f.card(0); hidden.querySelector(".pt-ops-rfc-qpe-close").click();
  f.context.checkRfcQpe();assert.strictEqual(hidden.style.display,'none');
  f.on(1);f.off(1);f.on(2);const count=f.counts();
  assert(!f.container.contains(hidden));
  const d = f.card(2).querySelector("details"); d.open = true;
  for (let n = 0; n < 3; n++) f.context.checkRfcQpe();
  assert.strictEqual(hidden.style.display, "none");
  assert(!f.container.querySelector('[data-rfc-qpe-product-id="' + idsRFC[1] + '"]'));
  assert.deepStrictEqual(f.counts(), count); assert.strictEqual(d.open, true);
  f.clear();
  assert.strictEqual(f.context.statusRows["NWS QPE Mosaic metadata"], undefined);
  const c = mountRfcWithoutCheck(f, 0);
  assert.strictEqual(metadataValue(c, "check"), "No check recorded");
  assert.strictEqual(c.querySelector("details").getAttribute("open"), null);
  f.clear(true); f.on(0); const readded = f.card(0);
  assert.notStrictEqual(readded, c);
  assert.strictEqual(readded.querySelectorAll("details").length, 1);
  assert.strictEqual(readded.querySelector("details").getAttribute("open"), null);
  f.unchanged();
});
test("E4 formatter failure stays Unverified in panel and legend without throwing", f => {
  const c = mountRfcWithoutCheck(f, 0);
  f.context.formatLosAngelesCompactParts = () => { throw new Error("fixture formatter unavailable"); };
  assert.doesNotThrow(() => f.context.checkRfcQpe());
  assert.strictEqual(metadataValue(c, "check"), "Unverified");
  assert(f.context.statusRows["NWS QPE Mosaic metadata"].msg.includes("BRIM check: Unverified"));
  for (const value of [null, {text: "local time", tz: "local"}, {text: "UTC time", tz: "UTC"}]) {
    f.context.formatLosAngelesCompactParts = () => value;
    f.context.checkRfcQpe();
    assert.strictEqual(metadataValue(c, "check"), "Unverified");
  }
});
test("E4 authored method and check text are escaped at creation and plain-text updates", f => {
  const payload = '<img src="x" onerror="bad()"> & <script>bad()</script>';
  f.context.ptOpsRfcQpeLegends[idsRFC[0]].period = payload;
  f.context.ptOpsRfcQpeCheckTime = () => payload;
  f.context.checkRfcQpe();
  const c = mountRfcWithoutCheck(f, 0);
  assert.strictEqual(metadataValue(c, "method"), payload);
  assert.strictEqual(metadataValue(c, "check"), payload);
  assert(!c.querySelector("img") && !c.querySelector("script"));
  assert(!f.status.querySelector("img") && !f.status.querySelector("script"));
  f.context.ptOpsRfcQpeCheckTime = () => payload + " updated";
  f.context.checkRfcQpe();
  assert.strictEqual(metadataValue(c, "check"), payload + " updated");
  assert(!c.querySelector("img") && !c.querySelector("script"));
});

// E18 extends the existing source/offline fixture. E22 supersedes only IEM's
// provider image and NOAA's no-guide/service-link assertions with a shared
// approximate BRIM guide; exact NOAA scale and all product times stay unknown.
const namesE18 = ["Radar | IEM NEXRAD", "Radar | NOAA MRMS",
  "QPE | NWS MRMS 1-hr", "QPE | NWS MRMS 1-day", "QPE | NWS MRMS 3-day"];
function cardE18(f, name) {
  const cards = f.container.querySelectorAll('[data-radar-mrms-name]');
  const found = cards.filter(c => c.getAttribute("data-radar-mrms-name") === name &&
    c.classList.contains("pt-ops-radar-mrms-card"));
  assert.strictEqual(found.length, 1, "Expected one active map card: " + name);
  return found[0];
}
function metadataE18(card, key) {
  const span = card.querySelector('[data-radar-mrms-field="' + key + '"]');
  assert(span, "Missing shared metadata field: " + key); return span.textContent;
}
for (const [i, name] of namesE18.entries()) {
  test("E18 " + name + " truthful map card and shared textual metadata", f => {
    assert(f.context.ptOpsActivateLayerByName(name).ok);
    const c = cardE18(f,name);
    assert.strictEqual(c.parentNode,f.corner);
    assert.strictEqual(metadataE18(c,"timing"),"Unverified");
    const notes=f.notes.querySelector('[data-radar-mrms-name="'+name+'"]');
    assert(notes, "Text-only panel metadata absent");
    c.querySelectorAll("[data-radar-mrms-field]").forEach(s=>
      assert.strictEqual(metadataE18(notes,s.getAttribute("data-radar-mrms-field")),s.textContent));
    assert.strictEqual(f.notes.querySelectorAll(".pt-ops-swatch, img").length,0);
    if(i===0) {
      assert(!c.querySelector("img"));
      assert.strictEqual(c.querySelectorAll(".pt-ops-swatch").length,5);
      assert(c.textContent.includes("Iowa Environmental Mesonet"));
    } else if(i===1) {
      assert(!c.querySelector("img"));
      assert.strictEqual(c.querySelectorAll(".pt-ops-swatch").length,5);
      assert(/Unverified.*not embedded/.test(metadataE18(c,"scale")));
      assert(!c.textContent.includes("whole-service"));
      assert.strictEqual(c.querySelector('a').getAttribute("href"),f.context.activeLegendDefs[name].sourceUrl);
    } else {
      assert.strictEqual(c.querySelectorAll(".pt-ops-swatch").length,7);
      assert(c.textContent.includes("Approximate color-family guide only"));
      assert(/Exact breaks.*units/.test(c.textContent));
      assert.strictEqual(metadataE18(c,"scale"),"Unverified in BRIM");
      assert(c.textContent.includes("Displayed accumulation dates"));
      assert.deepStrictEqual(c.querySelectorAll(".pt-ops-swatch").map(n=>n.style.background),
        ["#ccecff","#6ec6ff","#31d843","#fff04a","#ff9a26","#e31a1c","#d900ff"]);
    }
    assert(c.querySelector("a").getAttribute("rel").includes("noopener"));
    f.unchanged();
  });
  test("E18 " + name + " redraw, disclosure, close, off and reactivation", f => {
    f.context.ptOpsActivateLayerByName(name);const c=cardE18(f,name),n=f.requests.length;
    const details=c.querySelector("details");assert(details);assert.strictEqual(details.getAttribute("open"),null);
    details.setAttribute("open","");details.dispatchEvent(event("toggle"));
    f.context.redrawLegend();f.settle();
    assert.strictEqual(cardE18(f,name),c);assert.strictEqual(f.requests.length,n);
    c.querySelector(".pt-ops-radar-mrms-dock").click();
    f.context.redrawLegend();assert.strictEqual(cardE18(f,name),c);
    c.querySelector(".pt-ops-radar-mrms-close").click();
    assert.strictEqual(c.style.display,"none");
    f.context.redrawLegend();assert.strictEqual(c.style.display,"none");
    assert.strictEqual(f.requests.length,n);
    f.context.ptOpsDeactivateLayerByName(name);
    assert(!f.container.contains(c));assert(!f.context.ptOpsRadarMrmsCards[name]);
    f.context.ptOpsActivateLayerByName(name);assert.notStrictEqual(cardE18(f,name),c);
    assert.notStrictEqual(cardE18(f,name).style.display,"none");
    f.unchanged();
  });
}
for (const all of [false,true]) for(const mode of ["docked","floating","hidden"]) {
  test("E18 clear "+(all?"all":"Ops")+" destroys "+mode+" cards",f=>{
    const cards=[];
    for(const n of namesE18) {
      f.context.ptOpsActivateLayerByName(n);const c=cardE18(f,n);cards.push(c);
      if(mode!=='docked')c.querySelector('.pt-ops-radar-mrms-dock').click();
      if(mode==='hidden')c.querySelector('.pt-ops-radar-mrms-close').click();
    }
    f.clear(all);f.settle();f.context.redrawLegend();
    assert.strictEqual(f.container.querySelectorAll(".pt-ops-radar-mrms-card").length,0);
    assert.strictEqual(Object.keys(f.context.ptOpsRadarMrmsCards).length,0);
    cards.forEach(c=>assert(!f.container.contains(c)));
    f.unchanged();
  });
}
test("E18 MRMS product switching retains only current card content",f=>{
  let previous=null;
  for(const name of namesE18.slice(2)) {
    if(previous)f.context.ptOpsDeactivateLayerByName(previous);
    f.context.ptOpsActivateLayerByName(name);
    assert.strictEqual(f.container.querySelectorAll(".pt-ops-radar-mrms-card").length,1);
    assert(cardE18(f,name).textContent.includes(name));previous=name;
  }
});
test("E18 mixed MRMS and WPC retains MRMS cards alongside new WPC map guide",f=>{
  const qpf="WPC QPF Day 1";f.context.ptOpsActivateLayerByName(qpf);
  const original=f.context.qpeLegendHtml("QPE / QPF colors");
  assert.strictEqual(f.context.opsDefByName[qpf].layer.options.legendType,"qpe");
  namesE18.slice(2).forEach(n=>f.context.ptOpsActivateLayerByName(n));
  assert.strictEqual(f.notes.querySelectorAll(".pt-ops-swatch").length,0);
  assert(!f.notes.innerHTML.includes(original));
  const scale=f.container.querySelector(".pt-forecast-scale");
  assert.strictEqual(scale.querySelectorAll(".pt-ops-swatch").length,18);
  assert.deepStrictEqual(scale.querySelectorAll(".pt-qpf-contour-note").map(n=>n.textContent),
    ["No color = no forecast precipitation"]);
  f.context.ptOpsDeactivateLayerByName(qpf);
  assert.strictEqual(f.notes.querySelectorAll(".pt-ops-swatch").length,0);
  assert.strictEqual(f.container.querySelectorAll(".pt-ops-radar-mrms-card").length,1);
  assert.strictEqual(cardE18(f,namesE18.at(-1)).getAttribute('data-radar-mrms-name'),namesE18.at(-1));
});
test("E18 MRMS invalid and missing metadata dates remain unverified",async f=>{
  assert.strictEqual(typeof f.context.ptOpsMrmsMetadataDate,"function");
  for(const value of [undefined,null,"","not-a-date","2026-02-30T12:00:00Z",true,{},NaN,Infinity,"1750000000000"]) {
    assert.strictEqual(f.context.ptOpsMrmsMetadataDate(value),"Unverified");
  }
  f.context.ptOpsActivateLayerByName(namesE18[2]);
  f.requests[0].resolve({json:()=>Promise.resolve({features:[{attributes:{idp_validendtime:"not-a-date"}}]})});
  await new Promise(r=>setImmediate(r));
  const status=f.context.statusRows["MRMS metadata"];
  assert(status.msg.includes("Unbound latest service metadata row"));
  assert(status.msg.includes("Unverified"));assert(!status.msg.includes("Invalid Date"));
  assert(!status.msg.includes("Latest valid/end time:"));
});
for(const stamp of ["2026-09-15T12:00:00Z",1789473600000]) {
  test("E18 valid service date "+stamp+" stays unbound to all MRMS rasters",async f=>{
    const urls=namesE18.slice(2).map(n=>f.context.opsDefByName[n].layer._buildUrl().replace(/&_=\d+$/,""));
    for(const name of namesE18.slice(2)) {
      f.context.ptOpsActivateLayerByName(name);const c=cardE18(f,name);
      c.querySelector('details').setAttribute('open','');
      f.requests.at(-1).resolve({json:()=>Promise.resolve({features:[{attributes:{name:'unrelated service item',idp_validendtime:stamp}}]})});
      await new Promise(r=>setImmediate(r));
      const status=f.context.statusRows['MRMS metadata'];
      for(const text of ['Unbound latest service metadata row','Displayed-raster association: Unverified','Displayed accumulation dates: Unverified','Service update expectation'])assert(status.msg.includes(text));
      assert.strictEqual(cardE18(f,name),c);assert.strictEqual(metadataE18(c,'timing'),'Unverified');
      assert.strictEqual(metadataE18(c,'check'),status.time);
      const notes=f.notes.querySelector('[data-radar-mrms-name="'+name+'"]');
      assert.strictEqual(metadataE18(notes,'check'),status.time);
      assert.notStrictEqual(c.querySelector('details').getAttribute('open'),null);
    }
    assert.deepStrictEqual(namesE18.slice(2).map(n=>f.context.opsDefByName[n].layer._buildUrl().replace(/&_=\d+$/,"")),urls);
    urls.forEach(u=>assert(!/[?&](time|mosaicRule)=/.test(u)));
  });
}
test("E18 NOAA metadata success never verifies displayed frame",async f=>{
  f.context.ptOpsActivateLayerByName(namesE18[1]);const c=cardE18(f,namesE18[1]);
  f.requests[0].resolve({json:()=>Promise.resolve({fullUpdate:"2026-09-15T12:00:00Z",layers:[{id:3}]})});
  await new Promise(r=>setImmediate(r));
  const s=f.context.statusRows["NOAA radar metadata"];
  assert(s.msg.includes("Service metadata received"));assert(s.msg.includes("Displayed frame time: Unverified"));
  assert(s.msg.includes("Service update expectation"));
  assert.strictEqual(metadataE18(c,"timing"),"Unverified");assert.strictEqual(metadataE18(c,"check"),s.time);
});
for(const name of [namesE18[1],namesE18[2]]) {
  test("E18 "+name+" failed check preserves unknown timing",async f=>{
    f.context.ptOpsActivateLayerByName(name);const c=cardE18(f,name);
    f.requests[0].resolve({json:()=>Promise.reject(new Error("synthetic service failure"))});
    await new Promise(r=>setImmediate(r));
    const s=f.context.statusRows[name===namesE18[1]?"NOAA radar metadata":"MRMS metadata"];
    assert(/check failed/i.test(s.msg));assert(s.msg.includes("Unverified"));
    assert.strictEqual(metadataE18(c,"timing"),"Unverified");
  });
  test("E18 "+name+" pending response after Clear cannot restore state",async f=>{
    f.context.ptOpsActivateLayerByName(name);cardE18(f,name);f.clear();
    f.requests[0].resolve({json:()=>Promise.resolve({fullUpdate:true,features:[{attributes:{idp_validendtime:"2026-09-15T12:00:00Z"}}]})});
    await new Promise(r=>setImmediate(r));
    assert.strictEqual(Object.keys(f.context.statusRows).length,0);
    assert.strictEqual(Object.keys(f.context.ptOpsRadarMrmsCards).length,0);
    assert.strictEqual(f.container.querySelectorAll(".pt-ops-radar-mrms-card").length,0);
  });
}

const radarLabelsE22 = ["Light", "Moderate", "Heavy", "Very heavy", "Strongest echoes"];
const radarColorsE22 = ["#31d843", "#fff04a", "#ff9a26", "#e31a1c", "#d900ff"];
const radarNoteE22 = "General radar reflectivity guide — approximate. Provider palettes vary; this is not an exact dBZ or precipitation-rate scale.";
test("E22 one shared semantic guide supplies both radar cards", f => {
  const guide = f.context.ptOpsRadarGuide;
  assert(guide, "Shared BRIM radar guide is absent");
  assert.deepStrictEqual(Array.from(guide.entries, e => e.label), radarLabelsE22);
  assert.deepStrictEqual(Array.from(guide.entries, e => e.color), radarColorsE22);
  assert.strictEqual(guide.note, radarNoteE22);
  guide.entries.forEach(e => assert.deepStrictEqual(Object.keys(e).sort(), ["color", "label"]));
  // An alternate fixture value must reach both cards through the same definition.
  guide.entries[0].label = "Shared <fixture>"; guide.entries[0].color = "#123456";
  namesE18.slice(0, 2).forEach(name => {
    f.context.ptOpsActivateLayerByName(name);
    const c = cardE18(f, name), g = c.querySelector(".pt-ops-radar-guide");
    assert(g, "Map radar guide is absent");
    assert(g.textContent.includes("Shared <fixture>")); assert(!g.querySelector("fixture"));
    assert.strictEqual(g.querySelector(".pt-ops-swatch").style.background, "#123456");
  });
});
for (const [i, name] of namesE18.slice(0, 2).entries()) {
  test("E22 " + name + " usable approximate map guide, source link and unknown timing", f => {
    f.context.ptOpsActivateLayerByName(name);
    const c = cardE18(f, name), g = c.querySelector(".pt-ops-radar-guide");
    assert(g, "Five-category BRIM radar guide is absent");
    assert.deepStrictEqual(g.querySelectorAll(".pt-ops-legend-line").map(n => n.textContent), radarLabelsE22);
    assert.deepStrictEqual(g.querySelectorAll(".pt-ops-swatch").map(n => n.style.background), radarColorsE22);
    assert(g.textContent.includes(radarNoteE22));
    assert(!/\d|mm\s*\/\s*h|in\s*\/\s*h/.test(g.textContent), "No numeric scale or rate conversion");
    g.querySelectorAll(".pt-ops-swatch").forEach(n => assert.strictEqual(n.getAttribute("aria-hidden"), "true"));
    assert(!c.querySelector("img"));
    const def = f.context.activeLegendDefs[name];
    for (const node of [c, f.notes]) {
      const links = node.querySelectorAll("a").map(n => n.getAttribute("href"));
      assert(links.includes(def.sourceUrl), "Provider source link retained");
      assert(!links.includes(def.legendUrl), "Misleading provider legend link removed");
    }
    assert.strictEqual(metadataE18(c, "timing"), "Unverified");
    if (i === 0) {
      assert(!c.querySelector('[data-radar-mrms-field="scale"]'));
      assert(!/Provider legend image/.test(c.textContent + f.notes.textContent));
    } else assert(/Unverified.*not embedded/.test(metadataE18(c, "scale")));
    assert.strictEqual(f.notes.querySelectorAll(".pt-ops-radar-guide, .pt-ops-swatch, img").length, 0);
    f.unchanged();
  });
}
for (const name of namesE18.slice(0,2)) test("E22 " + name + " metadata refresh preserves guide and disclosure without extra requests", f => {
  f.context.ptOpsActivateLayerByName(name);
  const card=cardE18(f,name),guide=card.querySelector('.pt-ops-radar-guide'),n=f.requests.length;
  assert(guide);card.querySelector('details').setAttribute('open','');
  f.context.ptOpsUpdateRadarMrmsMetadata();f.context.redrawLegend();f.settle();
  assert.strictEqual(cardE18(f,name),card);assert.strictEqual(card.querySelector('.pt-ops-radar-guide'),guide);
  assert.notStrictEqual(card.querySelector('details').getAttribute('open'),null);
  assert.strictEqual(metadataE18(card,'timing'),'Unverified');
  assert(!/Provider legend image/.test(card.textContent+f.notes.textContent));assert.strictEqual(f.requests.length,n);f.unchanged();
});


// Exact E48 selection contract. These tests use real checkbox handlers and owners.
const qpeNames = ['QPE | NWS MRMS 1-hr','QPE | NWS MRMS 1-day','QPE | NWS MRMS 3-day',...namesRFC];
const qpeIds = ['ops_qpe_mrms_1hr','ops_qpe_mrms_1day','ops_qpe_mrms_3day',...idsRFC];
function qpeCard(f, name) {
  const i=qpeNames.indexOf(name);
  return i<3 ? cardE18(f,name) : f.card(i-3);
}
function assertQpeSelection(f, name) {
  assert.deepStrictEqual(qpeNames.filter(n=>f.map.hasLayer(f.context.opsDefByName[n].layer)),name?[name]:[],'exact active QPE wrapper');
  assert.deepStrictEqual(qpeNames.filter(n=>f.context.checkboxByName[n].checked),name?[name]:[],'exact checked QPE row');
  assert.deepStrictEqual(qpeNames.filter(n=>f.context.activeLayers[n]),name?[name]:[],'exact active QPE registry');
  assert.deepStrictEqual(qpeNames.filter(n=>f.context.activeLegendDefs[n]),name?[name]:[],'exact QPE legend owner');
  const cards=[...f.container.querySelectorAll('.pt-ops-rfc-qpe-card'),...f.container.querySelectorAll('.pt-ops-radar-mrms-card').filter(c=>qpeNames.some(n=>c.getAttribute('data-radar-mrms-name')===n))];
  assert.strictEqual(cards.length,name?1:0,'exact QPE map card');
  if(name)assert.strictEqual(cards[0],qpeCard(f,name));
}
function checkboxQpe(f,name) {
  const chk=f.context.checkboxByName[name];chk.checked=true;chk.dispatchEvent(event('change',{bubbles:true}));
}
function flushImageTimer(f,owner) {
  const id=owner._timer,t=f.timers.get(id);assert(t,'owned scheduled image request');f.timers.delete(id);t.f();
}
test('E48 exact six QPE identities default OFF; no heuristic forecast/radar membership',f=>{
  assert.deepStrictEqual(Array.from(f.context.opsLayers.filter(d=>d.layer.options.qpeProductId),d=>d.layer.options.qpeProductId),qpeIds);
  qpeNames.forEach((n,i)=>assert.strictEqual(f.context.opsDefByName[n].guideProductId,qpeIds[i]));
  assertQpeSelection(f,null);
  for(const n of ['Radar | NOAA MRMS','WPC QPF Day 1'])assert(!f.context.opsDefByName[n].layer.options.qpeProductId);
});
for(const from of qpeNames)for(const to of qpeNames)if(from!==to)test('E48 ordered QPE switch '+from+' → '+to,f=>{
  const a=f.context.opsDefByName[from].layer,b=f.context.opsDefByName[to].layer;
  checkboxQpe(f,from);flushImageTimer(f,a);const first=f.images.at(-1);first.fire('load');
  a._scheduleUpdate();const oldTimer=a._timer;const oldCard=qpeCard(f,from);
  assert(f.context.ptOpsActivateLayerByName(to).ok);assertQpeSelection(f,to);
  assert(!f.map.hasLayer(first));assert(!f.timers.has(oldTimer));assert.strictEqual(a._map,null);
  assert(!f.mapListeners.some(l=>l.ctx===a));assert(!f.container.contains(oldCard));
  assert(!oldCard.__brimDetachableState);assert(f.mapListeners.some(l=>l.ctx===b));
  assert.strictEqual(f.document.getElementById('pt-ops-active-count').textContent,'(1 active)');
  f.unchanged();
});
for(const name of qpeNames)test('E48 '+name+' rapid A-B-A rejects first-A load/error',f=>{
  const a=f.context.opsDefByName[name].layer,other=qpeNames[(qpeNames.indexOf(name)+1)%6];
  checkboxQpe(f,name);flushImageTimer(f,a);const old=f.images.at(-1);
  checkboxQpe(f,other);checkboxQpe(f,name);flushImageTimer(f,a);const current=f.images.at(-1);
  const status=JSON.stringify(f.context.statusRows[name]),card=qpeCard(f,name);
  old.fire('load');old.fire('error');
  assert.strictEqual(a._pendingOverlay,current);assert.strictEqual(a._overlay,null);
  assert.strictEqual(JSON.stringify(f.context.statusRows[name]),status);assert(f.context.isOpsLayerLoading(name));
  assertQpeSelection(f,name);assert.strictEqual(qpeCard(f,name),card);assert(!f.map.hasLayer(old));
  current.fire('load');assert.strictEqual(a._overlay,current);assert(!f.context.isOpsLayerLoading(name));
  const loaded=JSON.stringify(f.context.statusRows[name]);old.fire('error');old.fire('load');
  assert.strictEqual(a._overlay,current);assert.strictEqual(JSON.stringify(f.context.statusRows[name]),loaded);
});
for(const name of qpeNames)test('E48 '+name+' view updates, OFF/re-enable and direct owner cleanup',f=>{
  const a=f.context.opsDefByName[name].layer;checkboxQpe(f,name);flushImageTimer(f,a);const pending=f.images.at(-1),requests=f.requests.length;
  f.map.fire('moveend');f.map.fire('zoomend');const status=JSON.stringify(f.context.statusRows[name]);
  pending.fire('load');pending.fire('error');assert.strictEqual(JSON.stringify(f.context.statusRows[name]),status);assert.strictEqual(a._overlay,null);
  flushImageTimer(f,a);f.images.at(-1).fire('load');assert.strictEqual(f.requests.length,requests,'pan/zoom adds no service check');
  assert.strictEqual(a.refreshCurrentView,undefined,'QPE public refresh eligibility unchanged');
  f.context.ptOpsDeactivateLayerByName(name);assertQpeSelection(f,null);assert(!f.mapListeners.some(l=>l.ctx===a));
  checkboxQpe(f,qpeNames[(qpeNames.indexOf(name)+1)%6]);
  f.map.addLayer(a);assertQpeSelection(f,name);f.clear();assertQpeSelection(f,null);assert(!f.map.hasLayer(a));
  assert(!f.mapListeners.some(l=>l.ctx===a));assert(!f.timers.has(a._timer));f.unchanged();
});
for(const mode of ['docked','floating','hidden'])test('E48 switch disposes '+mode+' old card and failure has no fallback',f=>{
  const name=qpeNames[3],next=qpeNames[0],a=f.context.opsDefByName[name].layer,b=f.context.opsDefByName[next].layer;
  checkboxQpe(f,name);const card=qpeCard(f,name),state=card.__brimDetachableState;
  if(mode!=='docked')card.querySelector('.pt-ops-rfc-qpe-dock').click();
  if(mode==='hidden')card.querySelector('.pt-ops-rfc-qpe-close').click();
  flushImageTimer(f,a);const old=f.images.at(-1);checkboxQpe(f,next);flushImageTimer(f,b);f.images.at(-1).fire('error');
  const failure=f.context.statusRows[next].msg;assert(/failed/i.test(failure));old.fire('load');old.fire('error');
  f.context.redrawLegend();assertQpeSelection(f,next);assert(state.destroyed);assert(!f.container.contains(card));
  assert.strictEqual(f.context.statusRows[next].msg,failure);assert(!f.map.hasLayer(a));f.unchanged();
});

// E53: exact independent Radar ownership, real Ops selection and owner lifecycle.
const radarNames=['Radar | IEM NEXRAD','Radar | NOAA MRMS'];
function assertRadar(f,name) {
 for(const n of radarNames) {
  const on=n===name;assert.strictEqual(f.map.hasLayer(f.context.opsDefByName[n].layer),on,n+' wrapper');
  assert.strictEqual(f.context.checkboxByName[n].checked,on,n+' checkbox');
  assert.strictEqual(!!f.context.activeLayers[n],on,n+' registry');assert.strictEqual(!!f.context.activeLegendDefs[n],on,n+' legend');
  assert.strictEqual(!!f.context.ptOpsRadarMrmsCards[n],on,n+' card');
 }
}
function radarRequest(f,name) {const l=f.context.opsDefByName[name].layer;if(name===radarNames[1])flushImageTimer(f,l);return name===radarNames[0]?l._tiles:l._pendingOverlay;}
test('E53 exact two Radar IDs preserve checkbox UI and unbadged status',f=>{
 assert.deepStrictEqual(Array.from(f.context.opsLayers.filter(d=>d.subgroup==='Radar'),d=>d.guideProductId),['ops_radar_iem_nexrad','ops_radar_noaa_mrms']);
 assert.deepStrictEqual(Array.from(f.context.ptOpsRadarProductIds),['ops_radar_iem_nexrad','ops_radar_noaa_mrms']);
 for(const n of radarNames){const d=f.context.opsDefByName[n];assert.strictEqual(d.deliveryClass,'provider_hosted');assert(!f.context.ptOpsDeliveryBadgeHtml(d));assert.strictEqual(f.context.checkboxByName[n].tagName,'INPUT');}
 assertRadar(f,null);
});
for(const a of radarNames)for(const b of radarNames)if(a!==b)test('E53 '+a+' -> '+b+' -> '+a+' cleans wrappers/cards/count',f=>{
 f.context.ptOpsActivateLayerByName(a);const first=radarRequest(f,a),oldCard=cardE18(f,a);first.fire('load');
 f.context.ptOpsActivateLayerByName(b);assertRadar(f,b);assert(!f.map.hasLayer(first));assert(!f.container.contains(oldCard));
 assert(!f.mapListeners.some(x=>x.ctx===f.context.opsDefByName[a].layer));
 f.context.ptOpsActivateLayerByName(a);assertRadar(f,a);assert.strictEqual(f.document.getElementById('pt-ops-active-count').textContent,'(1 active)');
 f.context.ptOpsDeactivateLayerByName(a);assertRadar(f,null);assert.strictEqual(f.document.getElementById('pt-ops-active-count').textContent,'(none active)');f.unchanged();
});
for(const a of radarNames)test('E53 '+a+' rapid switch/off/re-enable rejects stale image/tile callbacks',f=>{
 const b=radarNames.find(n=>n!==a),owner=f.context.opsDefByName[a].layer;
 f.context.ptOpsActivateLayerByName(a);const old=radarRequest(f,a),callbacks=owner._tileHandlers?{...owner._tileHandlers}:null;
 f.context.ptOpsActivateLayerByName(b);f.context.ptOpsActivateLayerByName(a);const current=radarRequest(f,a),before=JSON.stringify(f.context.statusRows),card=cardE18(f,a);
 assert.notStrictEqual(current,old);old.fire('load');old.fire('error');old.fire('loading');old.fire('tileerror');
 if(callbacks){Object.values(callbacks).forEach(fn=>fn());assert.strictEqual(old.listenerCount(),0);}
 assert.strictEqual(JSON.stringify(f.context.statusRows),before);assert.strictEqual(cardE18(f,a),card);assert(!f.map.hasLayer(old));
 current.fire('load');assertRadar(f,a);assert(!f.context.isOpsLayerLoading(a));
 f.context.ptOpsDeactivateLayerByName(a);f.context.ptOpsActivateLayerByName(a);radarRequest(f,a);const after=JSON.stringify(f.context.statusRows);
 current.fire('load');current.fire('error');current.fire('tileerror');assert.strictEqual(JSON.stringify(f.context.statusRows),after);assertRadar(f,a);f.unchanged();
});
for(const name of radarNames)test('E53 '+name+' failed replacement never restores old Radar',f=>{
 const other=radarNames.find(n=>n!==name);f.context.ptOpsActivateLayerByName(other);f.context.ptOpsActivateLayerByName(name);
 radarRequest(f,name).fire(name===radarNames[0]?'tileerror':'error');assertRadar(f,name);assert(/failed/i.test(f.context.statusRows[name].msg));f.unchanged();
});
for(const name of radarNames)for(const all of [false,true])test('E53 '+name+' Clear '+(all?'All':'Ops')+' and direct re-enable clean every owner',f=>{
 f.context.ptOpsActivateLayerByName(name);const layer=f.context.opsDefByName[name].layer,old=radarRequest(f,name);f.clear(all);assertRadar(f,null);
 const before=JSON.stringify(f.context.statusRows);old.fire('load');old.fire('error');old.fire('tileerror');assert.strictEqual(JSON.stringify(f.context.statusRows),before);
 assert(!f.map.hasLayer(old));assert(!f.mapListeners.some(x=>x.ctx===layer));
 f.map.addLayer(layer);assertRadar(f,name);f.clear();assertRadar(f,null);f.unchanged();
});
test('E53 NOAA stale scheduled work and view response cannot retake ownership',f=>{
 const n=radarNames[1],o=f.context.opsDefByName[n].layer;f.context.ptOpsActivateLayerByName(n);const timer=f.timers.get(o._timer);
 f.context.ptOpsActivateLayerByName(radarNames[0]);f.context.ptOpsActivateLayerByName(n);const count=f.images.length;timer.f();assert.strictEqual(f.images.length,count);
 const old=radarRequest(f,n);f.map.fire('zoomend');const status=JSON.stringify(f.context.statusRows);old.fire('load');old.fire('error');assert.strictEqual(JSON.stringify(f.context.statusRows),status);assert.strictEqual(o._overlay,null);assertRadar(f,n);
});
for(const failure of [false,true])test('E53 NOAA obsolete metadata '+(failure?'failure':'success')+' rejected after A-B-A',async f=>{
 const n=radarNames[1];f.context.ptOpsActivateLayerByName(n);const old=f.requests.at(-1);
 f.context.ptOpsActivateLayerByName(radarNames[0]);f.context.ptOpsActivateLayerByName(n);const current=f.requests.at(-1);
 current.resolve({json:()=>Promise.resolve({fullUpdate:true})});await new Promise(r=>setImmediate(r));const status=JSON.stringify(f.context.statusRows);
 old.resolve({json:()=>failure?Promise.reject(new Error('obsolete')):Promise.resolve({fullUpdate:true})});await new Promise(r=>setImmediate(r));assert.strictEqual(JSON.stringify(f.context.statusRows),status);assertRadar(f,n);
});
for(const name of radarNames)test('E53 '+name+' coexistence leaves QPE and QPF untouched',f=>{
 const qpe=qpeNames[0],qpf='WPC QPF Day 1';f.context.ptOpsActivateLayerByName(qpe);f.context.ptOpsActivateLayerByName(qpf);
 for(const n of [name,radarNames.find(n=>n!==name),name]) {f.context.ptOpsActivateLayerByName(n);assertRadar(f,n);assertQpeSelection(f,qpe);assert(f.map.hasLayer(f.context.opsDefByName[qpf].layer));assert.strictEqual(f.document.getElementById('pt-ops-active-count').textContent,'(3 active)');}
 f.unchanged();
});

// E54: exposed/native WMS agreement and the actual tile event surface.
test('E54 IEM exposed WMS options and actual inner constructor agree without owner parameters',f=>{
 const name=radarNames[0],owner=f.context.opsDefByName[name].layer;
 const expected={layers:'nexrad-n0q-900913',format:'image/png',transparent:true,opacity:0.70,pane:'pane_ops',attribution:'Weather radar © Iowa Environmental Mesonet'};
 for(const [k,v] of Object.entries(expected))assert.strictEqual(owner.options[k],v,k+' exposed');
 f.context.ptOpsActivateLayerByName(name);const tile=owner._tiles;
 assert.strictEqual(tile.url,'https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?');
 for(const k of [...Object.keys(expected),'sourceUrl','legendUrl','infoUrl','infoLabel','legendNote'])assert.strictEqual(tile.options[k],owner.options[k],k+' inner');
 for(const k of ['name','radarProductId','legendType','note'])assert.strictEqual(Object.hasOwn(tile.options,k),false,k+' provider parameter');
 f.context.ptOpsDeactivateLayerByName(name);f.context.ptOpsActivateLayerByName(name);
 assert.notStrictEqual(owner._tiles,tile);assert.deepStrictEqual(owner._tiles.options,tile.options);assertRadar(f,name);f.unchanged();
});
test('E54 IEM current native loading/load/tileerror update only current status',f=>{
 const name=radarNames[0],owner=f.context.opsDefByName[name].layer;f.context.ptOpsActivateLayerByName(name);const tile=owner._tiles;
 tile.fire('load');assert.strictEqual(f.context.isOpsLayerLoading(name),false);assert.strictEqual(f.context.statusRows[name].msg,'Radar tiles loaded.');
 tile.fire('loading');assert.strictEqual(f.context.isOpsLayerLoading(name),true);
 tile.fire('tileerror');assert.strictEqual(f.context.isOpsLayerLoading(name),false);assert(/failed/.test(f.context.statusRows[name].msg));
 tile.fire('loading');tile.fire('load');assert.strictEqual(f.context.isOpsLayerLoading(name),false);assert.strictEqual(f.context.statusRows[name].cssClass,'pt-ops-ok');assertRadar(f,name);
});
test('E54 IEM selective teardown preserves unrelated native listeners and rejects retained old closures',f=>{
 const name=radarNames[0],owner=f.context.opsDefByName[name].layer;f.context.ptOpsActivateLayerByName(name);const old=owner._tiles,callbacks={...owner._tileHandlers};let removed=0,zoom=0,load=0;
 old.on('remove',()=>removed++);old.on('zoom',()=>zoom++);old.on('load',()=>load++);
 f.context.ptOpsDeactivateLayerByName(name);assert.strictEqual(removed,1);assert.strictEqual(old.listenerCount(),3);
 old.fire('zoom');old.fire('load');assert.strictEqual(zoom,1);assert.strictEqual(load,1);
 f.context.ptOpsActivateLayerByName(name);const state=JSON.stringify(f.context.statusRows);Object.values(callbacks).forEach(cb=>cb());old.fire('loading');old.fire('tileerror');assert.strictEqual(JSON.stringify(f.context.statusRows),state);assertRadar(f,name);f.unchanged();
});
for(const name of radarNames)test('E54 '+name+' direct add enforces peer ownership and repeated cleanup is safe',f=>{
 const other=radarNames.find(n=>n!==name),owner=f.context.opsDefByName[name].layer;
 f.context.ptOpsActivateLayerByName(other);owner.addTo(f.map);assertRadar(f,name);const native=radarRequest(f,name);
 owner.forceRemove(f.map);f.map.removeLayer(owner);owner.forceRemove(f.map);f.context.ptOpsDeactivateLayerByName(name);
 assertRadar(f,null);assert(!f.map.hasLayer(native));owner.addTo(f.map);assertRadar(f,name);f.clear();assertRadar(f,null);f.unchanged();
});

module.exports = {fixture, fn, raw, region, source, js};
if (require.main === module) (async () => {
  let passed = 0, failed = 0; const counts = {existing: {passed:0,failed:0}, E18: {passed:0,failed:0}, E22: {passed:0,failed:0}};
  for (const {name, body} of cases.filter(c => !a6Only || c.name.startsWith("A6 "))) {
    // Extraction and fixture setup errors are fatal, never credited as behavioral RED.
    const f = fixture();
    const group = name.startsWith("E22 ") ? "E22" : name.startsWith("E18 ") ? "E18" : "existing";
    try { await body(f); passed++; counts[group].passed++; console.log("PASS " + name); }
    catch (e) {
      if (e.code !== "ERR_ASSERTION") throw e;
      failed++; counts[group].failed++; console.log("FAIL " + name + ": " + e.message);
    }
  }
  console.log("RFC_AND_SELECTION_REGRESSION=" + JSON.stringify(counts.existing));
  console.log("E18_PRESENTATION=" + JSON.stringify(counts.E18));
  console.log("E22_RADAR_GUIDE=" + JSON.stringify(counts.E22));
  console.log("E26R1_SATELLITE=SUPERSEDED_BY_E31_FOCUSED_GIBS_SUITE; historical 39/0 retained");
  console.log("RESULT passed=" + passed + " failed=" + failed);
  process.exitCode = failed ? 1 : 0;
})().catch(e => { console.error("HARNESS_ERROR", e.stack); process.exitCode = 2; });
