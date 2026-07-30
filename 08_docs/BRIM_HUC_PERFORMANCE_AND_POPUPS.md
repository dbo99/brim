# BRIM HUC performance and popup hierarchy

## Scope

This note covers the six Local HUC polygon levels as one maintained family:
HUC2, HUC4, HUC6, HUC8, HUC10, and HUC12. The maintained implementation
includes browser rendering, theme/card lifecycle, fixed `%BLM` visualization,
and the presentation order of existing parent rows. It does not change HUC
geometry, identifiers, hierarchy values, BLM percentages/areas, PRISM/BCMv8
calculations, labels, or retained analytical products.

Implementation:

- `03_functions/blm_pct_theme_helpers.r`
- `03_functions/leaflet_layer_local_polygon_helpers.r`
- `03_functions/leaflet_huc_theme_helpers.r`
- `03_functions/js/brim_huc_theme_control.js`
- `03_functions/js/brim_legend_closeout_helpers.js`
- `03_functions/js/leaflet_tools_adddata_panel.js`
- `03_functions/popup_helpers.r`

Focused validation and retained-product diagnostics:

- `qa/test_huc_family.R`
- `qa/test_huc_blm_theme.R`
- `qa/test_huc_family_controller.js`
- `qa/qa_huc_family_performance.R`

## Data and build path

The HUC path remains:

1. `02_preprocess/02_huc_gw_county_pct_blm.r` reads the source HUC geometries,
   calculates HUC and BLM-intersection areas in EPSG:3310, creates hierarchy
   fields by code prefix, and writes the analytical HUC family.
2. `02_preprocess/13_huc_climate_recharge_summary.r` derives HUC climate and
   recharge summaries. `02_preprocess/14_huc4_huc2_from_huc6_climate_rollup.r`
   supplies the coarser rollups.
3. `05_map_build/02_cache_blocks/01_prepare_core_polygons.r` makes the
   simplified, WGS84 map geometry. Canonical analytical geometry remains
   unsimplified.
4. `05_map_build/02_cache_blocks/02_cache_huc_climate_theme.r` joins the
   geometry-free climate/recharge tables, builds popup HTML, and precomputes
   each theme's bin, label, and color fields.
5. `05_map_build/02_build_core_map_cache.r` writes
   `04_processed_data/cache/latest/huc_all_map.rds`.
6. `05_map_build/04_build_portatreasure2_core_map.r` embeds that cache in the
   standalone widget, registers the six Local layer groups, installs the shared
   HUC controller, and adds the separate HUC label groups.

No browser fetch, sidecar, public-feed dependency, or scientific calculation
was introduced.

## Source baseline

The tracked June 23, 2026 cache QA reports the following exact retained
map-feature counts. `addPolygons()` creates one Leaflet polygon object per
retained row; a multipart row is still one Leaflet object.

| Level | Retained map polygons / Leaflet objects | R object MiB | Popup MiB | Estimated Leaflet text payload MiB |
|---|---:|---:|---:|---:|
| HUC2 | 4 | 0.306 | 0.003 | 0.355 |
| HUC4 | 16 | 0.456 | 0.013 | 0.519 |
| HUC6 | 24 | 0.475 | 0.022 | 0.526 |
| HUC8 | 140 | 0.999 | 0.149 | 0.992 |
| HUC10 | 1,128 | 4.406 | 1.224 | 3.598 |
| HUC12 | 5,065 | 16.201 | 6.041 | 11.386 |
| **Total** | **6,377** | **22.843** | **7.452** | **17.376** |

The same QA reports mixed `POLYGON`/`MULTIPOLYGON` geometry at every level,
zero empty geometry, zero invalid geometry, and EPSG:4326 map output. A prior
tracked HUC payload audit records 123,190 coordinate rows for HUC10 and 259,996
for HUC12. Exact multipart-feature, ring, and coordinate-row counts for all six
current production products require the retained products and are deliberately
left to `qa_huc_family_performance.R`; the lean source repository does not
contain those RDS files.

Exact analytical-to-map code-set reconciliation requires the external
analytical and map RDS products. The read-only audit described below used both
the codex-ship and production copies; the lean source fixtures remain
non-authoritative.

## Fixed BLM-managed-land theme

The retained field is `percentBLMland`. It is a numeric percentage in the
0–100 scale, calculated as `100 * blm_area_sqmi / total_area_sqmi` in
EPSG:3310 and rounded to two decimal places by
`02_preprocess/02_huc_gw_county_pct_blm.r`. The map cache retains the same
field; the final-map payload classifies it without spatial recalculation or a
core-cache rewrite.

A read-only 2026-07-30 audit of the codex-ship map-facing product confirmed all
6,377 rows, with no missing or nonfinite values:

| Layer | n | 0% | >0–1% | >1–5% | >5–15% | >15–30% | >30–50% | >50–75% | >75% | Missing |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| HUC2 | 4 | 0 | 1 | 2 | 1 | 0 | 0 | 0 | 0 | 0 |
| HUC4 | 16 | 1 | 1 | 9 | 2 | 1 | 2 | 0 | 0 | 0 |
| HUC6 | 24 | 2 | 3 | 11 | 1 | 5 | 1 | 1 | 0 | 0 |
| HUC8 | 140 | 19 | 34 | 41 | 17 | 13 | 13 | 2 | 1 | 0 |
| HUC10 | 1,128 | 407 | 219 | 147 | 116 | 67 | 71 | 55 | 46 | 0 |
| HUC12 | 5,065 | 2,743 | 519 | 384 | 339 | 262 | 210 | 218 | 390 | 0 |
| **Combined** | **6,377** | **3,172** | **777** | **594** | **476** | **348** | **297** | **276** | **437** | **0** |

The shared neutral helper `03_functions/blm_pct_theme_helpers.r` owns the
classification used by both HUC and Bulletin 118:

| Bin | Color |
|---|---:|
| 0% | `#F2F2F2` |
| >0–1% | `#F1E6F4` |
| >1–5% | `#DFC7E5` |
| >5–15% | `#C9A3D2` |
| >15–30% | `#AA78B7` |
| >30–50% | `#87539A` |
| >50–75% | `#673A7B` |
| >75% | `#452357` |
| Missing | `#9E9E9E` |

The bins are fixed and absolute. They are not recomputed by level, visible
extent, or browser state. `pt_build_huc_theme_data()` classifies each retained
value once while preparing the compact final-map payload and builds per-level
counted legend rows from the same classifier. Purple keeps the percentage
theme distinct from the yellow BLM-managed-land layer, blue precipitation, and
green recharge.

## Previous browser architecture and bottlenecks

The previous implementation:

- created all 6,377 `L.Polygon` feature objects, popup bindings, hover tooltip
  bindings, mouse events, and highlight events eagerly;
- added each feature to an initially visible group, then hid the group only
  when the layer control was added later;
- set `preferCanvas=TRUE` on the map but assigned every HUC path to the custom
  `pane_huc`;
- relied on Leaflet 1.3.1's custom-pane renderer selection, which chooses SVG
  before Canvas, so each active HUC feature became an SVG path;
- recursively scanned every visible map layer and nested group during every
  theme application and every overlay add/remove, including unrelated
  overlays;
- made a complete per-feature `setStyle()` pass even when the same theme was
  already applied;
- performed two full passes when the first HUC level was enabled after all HUC
  layers had been off; and
- let sequential Clear Local overlay removals repeatedly rescan and restyle the
  HUCs that remained active.

Popup and hover HTML were eager. Theme values were not calculated in the
browser; the browser looked up R-precomputed colors by stable layer ID. HUC
labels were and remain independent label-only MarkerCluster groups.

The dominant source-level causes were therefore SVG DOM creation/removal for
dense custom-pane polygons, an initially mounted-then-hidden lifecycle, and
repeated map-wide scans/style passes. Geometry and payload size remain relevant
costs, particularly on first activation, but they were not the only costs.

## Selected HUC-family architecture

The R layer helper now creates each non-default HUC group in Leaflet's hidden
state before registering its polygons. Feature objects and their retained
popup/hover bindings are still created, but the group does not mount or project
all children at widget startup.

Every HUC path receives an explicit `L.Canvas` bootstrap renderer in
`pane_huc`. This is required because `preferCanvas=TRUE` alone does not select
Canvas for a custom pane in the bundled Leaflet version. While all HUC roots
are still hidden, the controller consolidates those paths onto one map-local
family Canvas. A single Canvas preserves hit testing when multiple HUC levels
are active; separate canvases stacked in one pane could intercept one another.
The shared Canvas keeps individual feature hover, popup, and highlight behavior
without one SVG element per polygon. It is removed from the map when the last
HUC level turns off, so an empty Canvas cannot intercept other `pane_huc`
layers.

The browser controller builds a direct registry from Leaflet's existing
per-group index:

- one root, layer array, expected count, and renderer reference per HUC level;
- no `map.eachLayer()` traversal;
- ordinary unrelated overlay events are ignored;
- only the focused active HUC level is a theme or threshold target;
- same-theme selection is a no-op;
- same-threshold selection is a no-op;
- repeated same-level requests are no-ops;
- theme and membership work share one frame-budgeted scheduler with
  approximately 8 ms slices;
- a generation token cancels stale work after rapid selection, off, or clear;
- an activated Canvas level is synchronously primed before its already-queued
  first draw, preventing an inactive theme from flashing;
- the first registry option and fresh-controller default are
  `Boundaries only (no fill)`;
- each active level owns its current-session theme and minimum-%BLM threshold;
- ordinary off resets only that level to boundaries and 0%, then restores full
  retained membership only after group, child, and Canvas-path detachment; and
- the selector, slider, visible count, and legend follow focused-level state.

Leaflet still retains one feature object per polygon so feature identity and
interaction semantics remain unchanged. Activation and off still iterate the
children of one Leaflet FeatureGroup; Canvas removes the per-feature SVG DOM
cost, but real browser measurement must establish the remaining projection and
group-iteration cost.

### Unified thematic card

One `bottomleft` Local-brown card owns the selector, minimum-%BLM filter,
active HUC context, visible count, and dynamic legend. Its registry order is:

1. `Boundaries only (no fill)`
2. `BLM-managed land — %`
3. `PRISM precip - in/yr`
4. `PRISM precip - kaf/yr`
5. `BCMv8 recharge - in/yr`
6. `BCMv8 recharge - kaf/yr`

The four prior scientific themes retain their relative order and existing
level-specific classifications. The `%BLM` legend shows all fixed classes and
counts only for the focused active level. The selector and slider follow that
level and change only its current-session state. Multiple active HUC levels
share the same card, scheduler, and fixed `%BLM` colors while retaining
separate current-session themes, thresholds, memberships, and counts.

HUC legends contain no explanatory footer or note container. Boundaries-only
leaves the legend body absent rather than rendering a separator or placeholder.
For focused HUC10/HUC12 work, the operational status shows a compact spinner
and `Loading…` only while that level is present in the existing scheduler job.
It has no independent timer or animation-frame loop. Cancellation, off, focus
change, clear, completion, and controller destruction remove the spinner.

The card uses `BRIM.legendCloseout.makeDetachable()` for the shared lower-left
stack, undock/dock action, detached dragging, viewport clamp, redock, and
responsive overflow. The dock action is immediately left of X. X hides only
the card; it does not deactivate geometry or reset session state. The first HUC
activation after an all-off state restores exactly one card, and level
switching updates that same card. Controller destruction removes its DOM
listeners, detachable state, and control.

### Minimum `%BLM` membership

The slider ranges from 0 through 100 in one-point steps and uses the retained
predicate `percentBLMland >= selected threshold`. Zero includes every feature,
including a future missing value; a positive threshold excludes missing or
nonfinite values. Excluded paths are removed from the active level's existing
FeatureGroup, so they cannot intercept hover, click, or popup events. Eligible
paths are restyled before the same object is re-added. No geometry, popup,
tooltip, or polygon object is reconstructed.

The threshold belongs to the focused level's current active session. Ordinary
overlay removal cancels stale work and resets only that level to 0%. Membership
restoration now requires `map.hasLayer(root) === false`, `root._map === null`,
zero retained children with `_map`, and zero retained paths in the shared
Canvas registry. If `overlayremove` arrives while the FeatureGroup removal pass
is still unwinding, one generation-guarded event-loop finalizer performs those
checks before restoring missing retained members. A single frame verification
uses normal Leaflet removal only if a child is still mounted.

This guard matches Leaflet 1.3.1's removal order. `Map.removeLayer(root)` first
removes the FeatureGroup children, deletes the root from the map registry, and
fires root `layerremove` (which the layer control translates to
`overlayremove`); only after those handlers return does it clear `root._map`.
Therefore `map.hasLayer(root)` is already false inside `overlayremove` while
`root._map` still points at the map. Calling `root.addLayer()` in that interval
mounts the restored child as an independent path after the group's child
removal pass, which is the ghost-path failure.

Detached `addLayer()` calls do not project or draw Canvas paths, and work is
bounded by the level's direct retained registry. Immediate reactivation
invalidates the old reset generation; the normal active scheduler then owns any
needed membership reconciliation. The synchronous activation option prime
applies fresh boundaries before the next paint, so hidden polygons are not
restyled merely to perform an off reset.

The 2026-07-30 retained-data audit produced:

| Layer | 0% | 1% | 50% | 70% | 75% | 100% |
|---|---:|---:|---:|---:|---:|---:|
| HUC2 | 4 | 3 | 0 | 0 | 0 | 0 |
| HUC4 | 16 | 14 | 0 | 0 | 0 | 0 |
| HUC6 | 24 | 19 | 1 | 0 | 0 | 0 |
| HUC8 | 140 | 87 | 3 | 1 | 1 | 0 |
| HUC10 | 1,128 | 502 | 101 | 59 | 46 | 0 |
| HUC12 | 5,065 | 1,805 | 608 | 431 | 390 | 7 |
| **Combined** | **6,377** | **2,430** | **713** | **491** | **437** | **7** |

### HUC hover-tooltip lifecycle

Leaflet 1.3.1 treats bound tooltips as independent per-feature overlays; unlike
popups, opening one tooltip does not close another. The bundled
`map.closeTooltip()` also requires a specific tooltip argument, so the former
no-argument call in the HUC `overlayremove` path did not close anything. Normal
hover relied entirely on each path's Leaflet `mouseout` handler. If that
Canvas handoff was delayed or missed, a newly opened polygon tooltip could
accumulate beside the old one.

The HUC controller now owns one current HUC-family tooltip. A map-level
`tooltipopen` guard closes the prior owned tooltip before accepting the new
one, while propagated HUC-root `mouseout` handling closes the current polygon
promptly. The same helper runs on HUC overlay add/remove, map move/zoom start,
Clear Local, Clear All, Measure activation, popup opening, map unload, and
controller destruction or rebuild. Destruction removes every registered map,
HUC-root, and document listener, so layer off/on and controller rebuilds do not
accumulate handlers.

`sticky = TRUE` remains intentional. The rich summary follows the pointer
within a large watershed instead of being anchored at the polygon center.
Sticky affects tooltip position, not exclusivity; the lifecycle helper now
supplies exclusivity and deterministic closure without changing tooltip
interaction, popup behavior, thematic styling, Canvas rendering, or HUC
interactivity. Every level's hover now ends with
`BLM-managed land: 12.3%`, formatted to one decimal place from retained
`percentBLMland`, with `0.0%` and `Not available` fallbacks.

### Marquee/box-zoom popup suppression

BRIM's marquee control is a custom pointer handler rather than Leaflet's
native shift-drag `BoxZoom`. It prevents and stops the pointer events, disables
map dragging, fires `pt:marqueezoomstart` inside its `pointerup` handler, calls
`fitBounds()`, and emits `pt:marqueezoomend` from a zero-delay task after
`moveend`. A browser click synthesized from the same release can therefore
reach the Canvas polygon between those two BRIM events. Because neither
Leaflet map dragging nor native `BoxZoom` moved, Leaflet 1.3.1's
`_draggableMoved()` check does not reject that click; the polygon's direct
`bindPopup()` listener opens the HUC popup.

The HUC controller now suppresses only HUC popup opening from
`boxzoomstart`/`pt:marqueezoomstart` through the first zero-delay task after
the corresponding end event. Native Leaflet fires `boxzoomend` inside
`mouseup`, before a subsequent browser click, so the same task boundary also
covers native ordering. If the direct bound listener starts opening a HUC
popup first, the map-level `popupopen` guard closes that specific popup
synchronously, before browser paint. Non-HUC popup sources are ignored.

The release is a deterministic event-loop boundary, not a time-based cooldown:
the browser dispatches the release-derived click before it processes queued
tasks, and a human cannot issue a later deliberate click before that task
runs. A new pointer gesture or Escape also clears stale suppression, while a
generation token prevents an older release task from clearing a newer marquee
operation. Controller destruction cancels the task and clears the state.

### Alternatives not selected

- A custom browser geometry decoder/renderer could reduce eager feature-object
  and binding costs further, but would replace proven polygon hit testing,
  popups, hover, and highlight behavior across six scientific layers.
- Lazy popup reconstruction was not selected in this source gate. The existing
  cached HTML is scientifically complete and the main measured source defects
  were rendering and repeated styling, not popup string calculation.
- Additional geometry simplification was not selected because map-facing
  geometry is already simplified and no topology/appearance evidence justified
  a more aggressive tolerance.
- External HUC data, sidecars, WebGL, disabled HUC12, and duplicate per-level
  implementations were explicitly out of scope.

## Popup hierarchy convention

The current subject HUC summary remains first. Parent rows now proceed from the
nearest parent outward:

- HUC12: HUC10, HUC8, HUC6, HUC4, HUC2
- HUC10: HUC8, HUC6, HUC4, HUC2
- HUC8: HUC6, HUC4, HUC2
- HUC6: HUC4, HUC2
- HUC4: HUC2
- HUC2: no parent rows

Only iteration order changed. All existing names, codes, BLM percentages,
areas, PRISM/BCMv8 fields, valid-area fields, links, suppression rules, and
markup remain in the same popup builder.

## Clear, labels, and Measure

Clear Local and Clear All continue to click the normal BRIM layer-control
checkboxes. Their capture-phase HUC guard closes the current tooltip
and cancels its current scheduler job immediately; each HUC `overlayremove`
then marks the level inactive, closes popup/tooltip state, and updates or hides
the unified card. It does not restyle the level being removed. When another
level was partially styled by a canceled job, only active dirty levels are
reconciled.

HUC label groups, their declustering thresholds, and source/label checkbox
synchronization are unchanged. Measure continues to use BRIM's existing pane
pointer-event suppression/restoration, which includes `pane_huc`, and now
emits the existing `pt:measureinteractionchange` lifecycle event when its
active state changes. The HUC controller uses that activation event to close
its current hover.

## Build implications

The fixed `%BLM` lookup, registry order/default, slider, hover construction,
unified card, controller, diagnostics, and shared-card registration are
final-map code. HUC and Bulletin hover strings are created when their retained
map objects are passed to `addPolygons()`; they are not stored in those map
caches. Because both audited products already contain `percentBLMland`, this
change requires only:

```r
source("run_build_map.r")
build_final_map_only()
```

No core-cache, label-cache, raw HUC, `%BLM`, PRISM, or BCMv8 preprocessing is
required. Retained `popup_html` is unchanged; unlike the new hover rows, a
future retained-popup-content change could require a core-cache rebuild.

No production cache or HTML was regenerated during this source gate.

## Source QA

Run from the repository root:

```sh
Rscript qa/test_huc_family.R
Rscript qa/test_huc_blm_theme.R
Rscript qa/test_huc_blm_theme.R \
  "/path/to/huc_all_map.rds" "/path/to/gw_bull118_map.rds"
node qa/test_huc_family_controller.js
```

The R fixture checks all six popup orders with recognizable parent labels,
required scientific popup fields, final one-decimal `%BLM` hover rows and
fallbacks, unique theme IDs, complete five-theme legend records, a hidden-group
call before `addPolygons`, explicit JavaScript Canvas serialization, the HUC
pane, and retained popup/hover bindings.

The focused `%BLM` fixture checks the exact 6,377-row per-level and combined
distribution, representative `>=` threshold counts, shared
labels/colors/classifier, first/default registry state, one lookup row per
feature, unique level/code keys, retained numeric filter values, fixed lookup
colors, and active-level legend counts. With HUC and Bulletin RDS arguments it
performs the exact audits against both retained map caches read-only.

The JavaScript fixture checks direct registry counts, Canvas reporting,
active-level-only style/membership work, true FeatureGroup removal/re-add,
missing/zero/inclusive-threshold semantics, same-theme/threshold/level no-ops,
rapid threshold/theme cancellation, final-state consistency, note removal,
boundaries/0% defaults, per-level current-session theme/threshold isolation
through same-level, cross-level, and concurrent HUC8 `%BLM` / HUC12 PRISM
sequences, exact HUC8 140/140 and HUC12 390/5,065-to-5,065 restoration, one
unified detachable card, X behavior, and controller replacement. It also checks
single-tooltip A-to-B replacement, mouseout, overlay removal, movement, both
clear actions, Measure activation, popup click-through, destruction/rebuild,
listener stability through repeated off/on cycles, BRIM and native box-zoom
event ordering, repeated marquee release, post-marquee deliberate clicks,
continued hover, and unrelated-popup isolation, while asserting that the
controller source contains no `map.eachLayer` hot path. The renderer-aware
fixture models early `overlayremove` ordering and asserts checked/controller/
group agreement plus per-level `_map` and Canvas-path counts across HUC2/4/6/8/
10/12, HUC10→HUC8, HUC8→HUC12, HUC6→HUC10, rapid three-level cancellation,
concurrent removal, and five reuse cycles spanning `%BLM`, PRISM, and BCMv8.

Run the retained-product diagnostic after the analytical and map RDS files are
available:

```sh
Rscript qa/qa_huc_family_performance.R \
  "/path/to/huc_all_full.rds" \
  "/path/to/huc_all_map.rds" \
  "/path/to/output_directory"
```

It writes read-only metrics and reconciliation CSVs for feature/ID counts,
empty and invalid geometry, geometry types, multipart features, rings,
coordinate rows, object and popup bytes, parent completeness, theme triplets,
style-descriptor counts, popup order, and analytical-to-map code-set equality.

## Browser diagnostics and benchmark gate

Diagnostics are silent by default. Enable them with `?brimProfile=1` on the
standalone HTML URL or in the console:

```javascript
BRIM_HUC_PROFILE.enable()
BRIM_HUC_PROFILE.startScenario("huc12-first-on")
// Perform exactly one scenario and wait until the map visibly settles.
BRIM_HUC_PROFILE.endScenario()
BRIM_HUC_PROFILE.downloadJson()
BRIM_HUC_LOCAL.stats()
```

`stats()` reports layer-control checked state when accessible, controller and
focus state, group membership and `_map` state, retained and expected counts,
mounted-child and per-level shared-Canvas path counts, selected/applied theme
and threshold state, active and reset generations/reasons, reset timing and
operations, pending jobs, loading visibility, card visibility, canceled/stale
work, listener counts, Canvas/SVG/DOM snapshots, long tasks, and heap values
when Chrome exposes `performance.memory`. Map-scan count must remain zero. The
Long Tasks observer is created only when profiling is enabled.

Use the actual timestamped self-contained output in `BRIM_v0.38_codex_ship`.
Before/after runs must use the same Chrome version, machine, viewport, basemap,
cache state, and DevTools setup. Record HTML/cache sizes separately.

For every HUC level test the default boundaries-only state, `%BLM`, all four
climate/recharge choices, fixed colors/counts, slider thresholds 0/1/50/70/75/
100, first on, repeat on, off, popup, hover, labels, Measure
suppression/restoration, Clear Local, Clear All, reactivation, unified-card
detach/drag/clamp/redock/X, and diagnostic feature counts. For HUC10 and HUC12
additionally record:

1. Statewide first and repeated activation.
2. Dense and sparse pan, direct zoom, and incremental zoom.
3. Boundaries-only, `%BLM`, two climate/recharge themes, and return.
4. Rapid theme/threshold switching, switch-then-off, and switch-then-Clear
   Local.
5. Basemap responsiveness, long tasks, SVG/Canvas/DOM counts, object counts,
   heap behavior, and any stale or partially colored display.

After each off or level transition, verify that every unchecked level reports
zero mounted children and zero shared-renderer paths. Include HUC12→HUC8,
HUC10→HUC8, HUC8→HUC12, HUC6→HUC10, rapid HUC12→HUC10→HUC8, concurrent
HUC8+HUC12 with HUC12-only removal, and repeated three-level transitions after
`%BLM`, PRISM, and BCMv8 fills.

Open one popup at every level and verify the subject first, nearest parent
second, progression to HUC2, unchanged values, and no parent section for HUC2.
Also regress other Local polygons, Water Rights/shared legends, Springs, Local
USGS groundwater, Ops Live, Local labels, startup, both clear paths, console
errors, and final HTML size.

No rendered-browser timings are claimed by this source gate.

## Known limits

- First activation must still project and index every polygon in the selected
  level and FeatureGroup add/remove still visits each child.
- Popup/hover strings and Leaflet feature objects remain eager in the embedded
  widget, although hidden-start groups avoid mounting them at startup.
- Previous-theme style results are not stored as duplicate Leaflet layer
  states. R-precomputed color lookups are retained, same-theme is a no-op, and
  every real active-theme transition applies each active feature once.
- Exact current production ring/multipart counts, activation/off timing, long
  tasks, heap behavior, card rendering, Windows fractional-layout behavior,
  and HTML size require the codex-ship retained products and rendered Chrome
  gate.
- No public `brim-live-data-feeds` file or interface was changed.
