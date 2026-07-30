# Ops Live USGS Cards and SCAN Interaction

## Scope

This guide covers the browser-side integration for three Ops Live layers:

- USGS streamflow for California
- USGS groundwater latest measurements
- USDA NRCS SCAN soil moisture

The USGS work is a presentation/lifecycle change. The SCAN work restores point
interaction without changing the feed contract, popup content, depth styling,
or data preprocessing.

## USGS unified card architecture

USGS streamflow and groundwater each own one `bottomleft` Leaflet control. Each
control contains, in order:

1. a single draggable header with the shared undock/dock action immediately
   left of the close action;
2. current-view and feed/filter record counts;
3. the existing layer symbology;
4. the existing filters, presets, notes, Apply, Reset, and result count.

The active-layer registry entries set `unifiedCard: true`. The legacy aggregate
Ops map-legend renderer skips those entries, preventing a second copy of the
same symbology. Other Ops legend types continue to use that aggregate renderer.

The close action hides only the card. It does not turn the layer off or clear
filter state. Turning the layer off, Clear Ops, or Clear All removes the control
and its detachable state. A later layer activation starts from the established
clean filter state and creates one coherent card.

Streamflow retains minimum/maximum cfs, Q ≥100 / ≥1k / ≥10k, On BLM, and
approximately ≤1 / ≤5 mile BLM screening presets. Discharge is raw latest cfs;
BLM distance is projected CA Albers screening geometry. The styling is not a
flood-stage, percentile, or anomaly renderer.

Groundwater retains age, depth-to-water, On BLM, BLM-distance, and
nested/co-located filters and presets. Negative depth-to-water values remain
reported artesian/above-land-surface values.

## Shared lower-left placement

Docked legend cards use the shared detachable-card lifecycle. The layout helper
measures the live bounds of `#pt-tools-adddata-wrap` (External Layers) and
`#pt-local-upload-wrap` (Local GIS Uploads). It centers the visible lower-left
card stack in the vertical gap between them. This also corrects the lower
placement of the SWRCB water-rights card without adding a layer-specific
coordinate.

When the stack is taller than the measured gap, the corner becomes a bounded
vertical scroller. Undocked cards remain draggable and are clamped to the map
viewport; docking restores their Leaflet control position. The header is part
of the card and is not implemented as a sticky inner header.

### Windows fractional-layout stability

A Windows-only idle text jiggle in the CalSim `lbl` control and one BLM office
legend row exposed a shared layout feedback risk. The responsive-stack helper
formerly removed overflow before every measurement and observed its own corner
`style` writes. At fractional device-pixel ratios, scrollbar/reflow and
fractional bottom-offset measurements could alternate and schedule another
layout pass.

The shared helper now preserves settled overflow state while measuring, reserves
a stable scrollbar gutter, rounds clamp and lower-left offsets to CSS pixels,
compares before writing, and ignores MutationObserver records that exactly
match its own layout signature. A small overflow hysteresis prevents
single-pixel boundary toggling. Dock, detach, drag, viewport clamp, redock, and
responsive scrolling remain enabled.

`qa/test_shared_card_layout_stability.js` verifies that after a docked stack
settles, delivering its own style-mutation records leaves zero queued animation
frames and a second explicit layout pass performs zero new style writes.
Windows browser retesting remains required because the source environment
cannot reproduce Windows font rasterization, scrollbar metrics, display
scaling, or browser zoom.

## SCAN data-to-interaction path

The SCAN consumer path is:

1. URLs and layer metadata are registered in
   `leaflet_ops_live_layer_definition_helpers.r`.
2. `makeScanSoilMoistureLayer()` fetches the latest GeoJSON and optional
   current-water-year trace, depth style, daily percentile, monthly context,
   fallback trace, and summary products.
3. `_ptScanRender()` applies the selected-depth symbology and builds the rich
   station popup, official NRCS link, tooltip, and hover mini plot.
4. Circle markers render in the existing `pane_ops` pane and explicitly remain
   interactive.
5. layer removal clears markers, controls, detachable state, registry state,
   and invalidates late asynchronous work through `_ptScanIsRemoved` plus an
   activation generation token.

### Root-cause finding

The SCAN helper formerly omitted a pane, so its canvas markers used Leaflet's
default overlay pane. The springs performance implementation added a
viewport-sized interactive canvas in `pane_points`, whose z-index is above the
default overlay pane. Source inspection therefore supports the regression
finding that the newer springs canvas could receive pointer hit testing above
visible SCAN points.

SCAN now uses the already-established Ops vector pane (`pane_ops`, z-index 560),
above `pane_points` and below labels, controls, tooltips, and popups. No new
arbitrary pane or z-index was introduced.

Measure mode already suspends pointer events on non-measure Leaflet panes and
restores each pane's prior inline value. Because SCAN now uses `pane_ops`, it
participates in that existing suspend/restore lifecycle.

## Required validation

Source checks:

- parse every changed R file;
- syntax-check the extracted embedded JavaScript and the shared closeout helper;
- run `qa/test_ops_live_legends_and_scan_source.js`;
- run `tools/validate_source_repository.R`;
- run `git diff --check`.

The source fixture verifies code contracts; it does not prove browser behavior.
The production browser gate must cover:

- first activation, pan/zoom, off/on, and refresh for each affected Ops layer;
- all streamflow and groundwater filters, presets, Enter-to-apply, Apply, and
  Reset;
- dock, drag, viewport clamp, redock, close-only, responsive stacking, and
  overflow;
- placement between the collapsed and expanded External Layers and Local GIS
  Upload panels, including coexistence with the SWRCB card;
- SCAN hover mini plot and rich click popup across depth selections;
- SCAN off/on, refresh, Clear Ops, Clear All, and coexistence with springs and
  other point layers;
- Measure activation suppressing SCAN interaction and Measure exit restoring it.

## Build and data implications

No feed regeneration, remote preprocessing, cache rebuild, or producer-repo
publication is required. In a build-capable checkout with valid existing
caches, run only `build_final_map_only()` and complete the browser gate above.
