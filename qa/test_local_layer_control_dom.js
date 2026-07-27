#!/usr/bin/env node

"use strict";

const assert = require("assert");
const fs = require("fs");
const path = require("path");

const root = path.resolve(__dirname, "..");
const coreSource = fs.readFileSync(
  path.join(root, "03_functions", "leaflet_core_helpers.r"),
  "utf8"
);
const controllerSource = fs.readFileSync(
  path.join(
    root,
    "03_functions",
    "js",
    "leaflet_calsim3_local_cluster.js"
  ),
  "utf8"
);
const referenceSource = fs.readFileSync(
  path.join(
    root,
    "03_functions",
    "leaflet_layer_local_reference_helpers.r"
  ),
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

assert.strictEqual(
  (coreSource.match(/\{main: 'CalSim3\.0', label: 'CalSim3\.0'\}/g) || [])
    .length,
  1,
  "CalSim must have exactly one shared inline-label pair"
);
assert.match(
  coreSource,
  /mainInput\.addEventListener\('change', syncInlineState\);[\s\S]*labelInput\.addEventListener\('change', syncInlineState\);/,
  "main and companion changes must reconcile inline state immediately"
);
assert.doesNotMatch(
  coreSource,
  /mainInput\.addEventListener\('change', function\(\) \{\s*setTimeout\(syncInlineState/,
  "CalSim activation must not depend on a delayed inline-label rescan"
);
assert.match(
  coreSource,
  /parsed\.category === 'Labels'[\s\S]*pt-label-companion-hidden/,
  "all companion label rows must retain the shared hidden-row contract"
);

const overlayRemoveBlock = controllerSource.match(
  /function onOverlayRemove\(event\) \{[\s\S]*?\n  \}\n\n  function onLayerAdd/
);
assert.ok(overlayRemoveBlock, "CalSim overlay-remove handler was not found");
assert.match(
  overlayRemoveBlock[0],
  /setLabelsRequestedState\(false, 'network-overlay-remove'\)/,
  "main-layer removal must clear browser label state without a nested click"
);
assert.doesNotMatch(
  overlayRemoveBlock[0],
  /setLabelsRequested\(false, 'network-overlay-remove'\)/,
  "main-layer removal must not click the canonical companion synchronously"
);
assert.match(
  legendSource,
  /card: '\.pt-calsim3-explorer'[\s\S]*close: '\.pt-calsim3-close'/,
  "the Explorer must retain shared detachable/close registration"
);
assert.match(
  mapBuildSource,
  /pt_add_calsim3_label_companion\([\s\S]*pt_add_calsim3_cluster_controller\(/,
  "the label companion/controller integration order changed"
);
assert.match(
  referenceSource,
  /pt-conv-lbl[\s\S]*controlInputFor\(labelGroup, true\)[\s\S]*input\.click\(\)/,
  "Water Conveyance lost its canonical shared-label adapter"
);

const prefixes =
  /^(Ops|Core|Basins|Points|Channels|Reference|Labels)\s+–\s+/;
const rawRows = [
  {key: "huc-main", raw: "Basins – HUC2 – PRISM/BCMv8", kind: "main"},
  {key: "gw-main", raw: "Points – USGS streamgages", kind: "main"},
  {
    key: "conv-main",
    raw: "Channels – Water conveyance | BRIM mapped",
    kind: "main"
  },
  {key: "calsim-main", raw: "Channels – CalSim3.0", kind: "main"},
  {key: "reference-main", raw: "Reference – Counties", kind: "main"},
  {key: "huc-label", raw: "Labels – HUC2", kind: "label"},
  {key: "gw-label", raw: "Labels – USGS streamgages", kind: "label"},
  {
    key: "conv-label",
    raw: "Labels – Water conveyance | BRIM mapped",
    kind: "label"
  },
  {key: "calsim-label", raw: "Labels – CalSim3.0", kind: "label"}
];
const pairs = [
  ["huc-main", "huc-label"],
  ["gw-main", "gw-label"],
  ["conv-main", "conv-label"],
  ["calsim-main", "calsim-label"]
];

function shortName(raw) {
  return raw.replace(prefixes, "");
}

class LayerControlFixture {
  constructor({legacyNestedMainOff = false} = {}) {
    this.legacyNestedMainOff = legacyNestedMainOff;
    this.rows = rawRows.map((row) => ({
      ...row,
      text: row.raw,
      hidden: false,
      checked: false,
      active: false,
      inlineCount: 0,
      inlineEnabled: false,
      inlineChecked: false,
      adapterListenerCount: 0
    }));
    this.rowsByKey = Object.fromEntries(
      this.rows.map((row) => [row.key, row])
    );
    this.handlingClick = false;
    this.controlRebuildCount = 0;
    this.canonicalLabelChangeCount = 0;
    this.browserLabelsRequested = false;
    this.explorerChecked = false;
    this.enhance();
  }

  enhance() {
    this.rows.forEach((row) => {
      row.text = shortName(row.raw);
      row.hidden = row.kind === "label";
    });
    pairs.forEach(([mainKey, labelKey]) => {
      const main = this.rowsByKey[mainKey];
      const label = this.rowsByKey[labelKey];
      if (main.inlineCount === 0) {
        main.inlineCount = 1;
        main.adapterListenerCount = 2;
      }
      label.hidden = true;
      this.syncInline(mainKey, labelKey);
    });
  }

  rebuildFromLeaflet() {
    this.controlRebuildCount += 1;
    this.rows.forEach((row) => {
      row.text = row.raw;
      row.hidden = false;
      row.inlineCount = 0;
      row.inlineEnabled = false;
      row.inlineChecked = false;
      row.adapterListenerCount = 0;
    });
  }

  syncInline(mainKey, labelKey) {
    const main = this.rowsByKey[mainKey];
    const label = this.rowsByKey[labelKey];
    if (!main.checked && label.checked) {
      this.clickLayer(labelKey, false, "shared-main-off");
    }
    main.inlineEnabled = main.checked;
    main.inlineChecked = main.checked && label.checked;
  }

  onRegisteredLayerChange(row, active) {
    if (!this.handlingClick) this.rebuildFromLeaflet();
    if (row.key === "calsim-label") {
      this.browserLabelsRequested = active && this.rowsByKey[
        "calsim-main"
      ].active;
      this.explorerChecked = this.browserLabelsRequested;
    }
    if (row.key === "calsim-main" && !active) {
      this.browserLabelsRequested = false;
      this.explorerChecked = false;
      if (
        this.legacyNestedMainOff &&
        this.rowsByKey["calsim-label"].checked
      ) {
        this.clickLayer(
          "calsim-label",
          false,
          "legacy-controller-main-off"
        );
      }
    }
  }

  clickLayer(key, checked, origin) {
    const row = this.rowsByKey[key];
    row.checked = checked;
    const wasHandling = this.handlingClick;
    this.handlingClick = true;

    const removed = this.rows.filter(
      (candidate) => candidate.active && !candidate.checked
    );
    // Leaflet snapshots every checked input before changing any map layer.
    // The outer transaction can therefore retain a companion in this list
    // even when a nested transaction later unchecks and removes it.
    const added = this.rows.filter((candidate) => candidate.checked);
    removed.forEach((candidate) => {
      candidate.active = false;
      this.onRegisteredLayerChange(candidate, false);
    });
    added.forEach((candidate) => {
      if (!candidate.active) {
        candidate.active = true;
        this.onRegisteredLayerChange(candidate, true);
      }
    });

    // Leaflet 1.3.1 does not stack this boolean. A nested input click sets it
    // false while its caller is still applying the outer transaction.
    this.handlingClick = false;
    if (row.kind === "label") this.canonicalLabelChangeCount += 1;

    // Native checkbox change fires after Leaflet's click handler returns.
    const pair = pairs.find(
      ([mainKey, labelKey]) => key === mainKey || key === labelKey
    );
    if (pair) this.syncInline(pair[0], pair[1]);

    void wasHandling;
    void origin;
  }

  clickExplorerLabel(checked) {
    this.clickLayer("calsim-label", checked, "explorer");
  }

  clickInlineLabel(mainKey, checked) {
    const pair = pairs.find(([key]) => key === mainKey);
    assert.ok(pair, `missing inline-label pair for ${mainKey}`);
    assert.strictEqual(
      this.rowsByKey[mainKey].inlineEnabled,
      true,
      `inline label control is disabled for ${mainKey}`
    );
    this.clickLayer(pair[1], checked, "inline");
  }

  clearLocal() {
    this.rows
      .filter((row) => row.checked)
      .slice()
      .forEach((row) => {
        if (row.checked) this.clickLayer(row.key, false, "clear-local");
      });
  }

  clearAll() {
    this.clearLocal();
  }

  snapshot() {
    return this.rows.map((row) => ({
      key: row.key,
      text: row.text,
      hidden: row.hidden,
      inlineCount: row.inlineCount,
      adapterListenerCount: row.adapterListenerCount
    }));
  }

  visibleText() {
    return this.rows.filter((row) => !row.hidden).map((row) => row.text);
  }
}

const fixture = new LayerControlFixture();
const initialSnapshot = fixture.snapshot();
const initialVisibleText = fixture.visibleText();

assert.strictEqual(
  fixture.rowsByKey["calsim-main"].inlineCount,
  1,
  "fresh page must contain exactly one CalSim inline label control"
);
assert.strictEqual(
  fixture.rowsByKey["calsim-label"].hidden,
  true,
  "fresh page exposed the CalSim companion row"
);
assert.strictEqual(fixture.rowsByKey["calsim-main"].inlineChecked, false);

fixture.clickLayer("calsim-main", true, "main");
assert.strictEqual(
  fixture.rowsByKey["calsim-main"].inlineEnabled,
  true,
  "first CalSim activation did not enable inline lbl immediately"
);
fixture.enhance();
fixture.enhance();
assert.strictEqual(
  fixture.rowsByKey["calsim-main"].inlineCount,
  1,
  "repeated enhancement created a duplicate CalSim inline control"
);
assert.strictEqual(
  fixture.rowsByKey["calsim-main"].adapterListenerCount,
  2,
  "repeated enhancement accumulated inline adapter listeners"
);

let canonicalChanges = fixture.canonicalLabelChangeCount;
fixture.clickExplorerLabel(true);
assert.strictEqual(
  fixture.canonicalLabelChangeCount,
  canonicalChanges + 1,
  "Explorer lbl changed canonical state more than once"
);
assert.strictEqual(fixture.rowsByKey["calsim-main"].inlineChecked, true);
assert.strictEqual(fixture.explorerChecked, true);

canonicalChanges = fixture.canonicalLabelChangeCount;
fixture.clickInlineLabel("calsim-main", false);
assert.strictEqual(
  fixture.canonicalLabelChangeCount,
  canonicalChanges + 1,
  "inline lbl changed canonical state more than once"
);
assert.strictEqual(fixture.explorerChecked, false);
fixture.clickInlineLabel("calsim-main", true);

fixture.clickLayer("calsim-main", false, "main");
assert.strictEqual(fixture.browserLabelsRequested, false);
assert.strictEqual(fixture.explorerChecked, false);
assert.strictEqual(fixture.rowsByKey["calsim-label"].checked, false);
assert.deepStrictEqual(
  fixture.snapshot(),
  initialSnapshot,
  "CalSim label lifecycle mutated Local-panel row text/order/classes"
);
assert.deepStrictEqual(
  fixture.visibleText(),
  initialVisibleText,
  "CalSim label lifecycle changed unrelated visible Local row text"
);
assert.ok(
  fixture.visibleText().every((text) => !prefixes.test(text)),
  "a raw Local group prefix leaked into visible row text"
);
assert.ok(
  fixture.rows.filter((row) => row.kind === "label").every((row) => row.hidden),
  "a companion label row became visible"
);
assert.strictEqual(
  fixture.controlRebuildCount,
  0,
  "the corrected lifecycle triggered a Leaflet control rewrite"
);

for (let cycle = 0; cycle < 3; cycle += 1) {
  fixture.clickLayer("calsim-main", true, "main");
  fixture.clickExplorerLabel(true);
  fixture.clickLayer("calsim-main", false, "main");
}
assert.deepStrictEqual(fixture.snapshot(), initialSnapshot);
assert.strictEqual(fixture.controlRebuildCount, 0);

["huc-main", "gw-main", "conv-main"].forEach((mainKey) => {
  fixture.clickLayer(mainKey, true, "representative-main");
  fixture.clickInlineLabel(mainKey, true);
  fixture.clickLayer(mainKey, false, "representative-main");
  assert.deepStrictEqual(
    fixture.visibleText(),
    initialVisibleText,
    `${mainKey} row text changed during shared-label lifecycle`
  );
});
fixture.clearLocal();
fixture.clearAll();
assert.deepStrictEqual(fixture.snapshot(), initialSnapshot);
assert.strictEqual(fixture.controlRebuildCount, 0);

const legacyFixture = new LayerControlFixture({
  legacyNestedMainOff: true
});
legacyFixture.clickLayer("calsim-main", true, "main");
legacyFixture.clickExplorerLabel(true);
legacyFixture.clickLayer("calsim-main", false, "main");
assert.ok(
  legacyFixture.controlRebuildCount > 0,
  "fixture did not reproduce the former nested-click control rebuild"
);
assert.ok(
  legacyFixture.rows.some(
    (row) => row.kind === "label" && row.hidden === false
  ),
  "fixture did not reproduce former companion-row leakage"
);

console.log("Local inline-label DOM lifecycle fixture: PASS");
