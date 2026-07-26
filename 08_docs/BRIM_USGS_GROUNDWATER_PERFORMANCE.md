# BRIM Local USGS groundwater performance

## Scope

This note documents the Local/static USGS monitoring-well performance design
introduced on `feature/usgs-groundwater-performance`. It does not change the
Ops Live groundwater layer, retained analytical data, cache production, or
standalone single-file delivery model.

The implementation lives in:

- `03_functions/leaflet_layer_local_usgs_helpers.r`
- `03_functions/js/leaflet_usgs_groundwater_local_virtualized.js`

Focused validation lives in:

- `qa/test_usgs_groundwater_virtualized.js`
- `qa/qa_usgs_groundwater_performance.R`

## Retained-product reconciliation

The July 24, 2026 retained `usgs_wells_map.rds` contains:

| Measure | Count |
|---|---:|
| Valid retained rows | 44,159 |
| Distinct USGS site numbers | 44,159 |
| Unique mapped coordinates at the established 7-decimal grouping precision | 41,361 |
| Nested/co-located mapped locations | 1,710 |
| Site records at nested locations | 4,508 |
| Non-nested locations | 39,651 |
| Maximum records at one mapped location | 11 |

Nested-location size distribution:

| Records at location | Location count |
|---:|---:|
| 2 | 1,111 |
| 3 | 320 |
| 4 | 152 |
| 5 | 79 |
| 6 | 34 |
| 7 | 6 |
| 8 | 1 |
| 9 | 2 |
| 10 | 4 |
| 11 | 1 |

Every source coordinate remains in the embedded record table. Coordinate
grouping affects display semantics only; it does not replace or deduplicate
site records.

## Previous browser architecture and bottleneck

The previous Local layer converted the complete columnar payload into 44,159
JavaScript row objects during page initialization. First activation then
created one `L.Marker`/`L.DivIcon` for each single or nested mapped location
and inserted all 41,361 objects into one MarkerCluster group.

The important previous options were:

- `disableClusteringAtZoom: 11`
- radius steps of 175, 145, 115, 75, and 35 pixels
- `animate: false`
- `animateAddingMarkers: false`
- `chunkedLoading: false`
- `removeOutsideVisibleBounds: true`

`removeOutsideVisibleBounds` reduced visible DOM but did not avoid constructing
or indexing every Leaflet marker. Direct marquee zoom to the unclustered
threshold forced a large synchronous cluster-to-marker transition. Ordinary
off and Clear Local removed and cleared the global group, so reactivation
recreated every marker. Popup functions were deferred, but marker objects,
icons, tooltip strings, grouping, and cluster membership were eager.

The earlier home-recovery guard temporarily detached the heavy cluster before
an unanimated home `setView`, then restored it later. The new layer preserves
that principle for home, zoom, and marquee transitions by detaching its bounded
display before movement and rebuilding only after movement settles.

## Selected architecture

The full analytical record set remains embedded as compact column arrays. R
sorts rows by the established coordinate key and adds a compact location table
containing only a zero-based site-row start and site count.

The browser then uses:

1. A lazy, immutable 0.25-degree grid index built in frame-budgeted slices.
2. A buffered current-viewport query.
3. Deterministic Web Mercator screen-grid aggregates below zoom 11.
4. Exact current-viewport mapped locations at zoom 11 and above.
5. A dedicated Leaflet Canvas renderer for single exact sites.
6. Bounded `DivIcon` markers for aggregates and nested locations.
7. Lazy hover and popup construction.
8. A generation token that invalidates stale index, filter, query, mount, zoom,
   pan, off, and clear work.
9. Immediate display-root detachment followed by deferred bounded cleanup.
10. Reuse of the immutable spatial index and completed display root when safe.

All ordinary individual sites use the retained catalog's majority normal
geometry: radius 4.8 pixels and stroke weight 0.95 pixels, for a resting outer
diameter of 10.55 pixels. Water-level and Ops membership continue to affect
colors, not ordinary marker size. Nested-location icons and aggregate icons
remain separately designed symbols. The eight water-level legend classes share
one 9-pixel dot geometry.

No library, CDN dependency, network request, sidecar, or WebGL dependency was
added.

Representative 1,200 by 800 pixel object-count estimates from the retained
cache are:

| Scenario | Zoom | Buffered locations queried | Display objects | Reduction from 41,361 |
|---|---:|---:|---:|---:|
| Statewide/home | 6 | 41,361 | 10 aggregates | 99.98% |
| Sacramento | 11 | 1,262 | 1,262 exact | 96.95% |
| Dense Fresno | 11 | 2,779 | 2,779 exact | 93.28% |
| Dense Kern/oil-field | 11 | 2,078 | 2,078 exact | 94.98% |
| Sparse northeast California | 11 | 4 | 4 exact | 99.99% |
| Dense Fresno | 10 | 6,971 | 228 mixed aggregate/exact | 99.45% |
| Dense Fresno | 12 | 779 | 779 exact | 98.12% |

These are deterministic structural measurements, not browser interaction
timings.

## Clear and movement lifecycle

Ordinary layer off:

- increments the generation token;
- cancels the scheduled frame and render timer;
- closes groundwater popups and tooltips;
- removes the one bounded display root;
- retains a completed same-viewport root for fast ordinary reactivation.

Explicit controller clear:

- performs the same immediate visible removal;
- drops the completed display-root cache;
- defers bounded child disposal;
- retains immutable source columns and the spatial index.

Clear Local and Clear All continue to use the normal BRIM checkbox cleanup, so
unrelated systems are not bypassed. The overlay-remove event performs
groundwater cancellation/removal synchronously. No clear path moves the map.

Zoom, pan, home, and marquee start invalidate pending work and detach the
bounded display before basemap movement. A delayed render after `zoomend` or
`moveend` lets tile work run first. A stale callback must pass both its
generation token and current active state before it can mount anything.

## Nested-location interaction lifecycle

Nested markers and low-zoom aggregate markers use the dedicated
`pane_usgs_gw_interactive` pane at z-index 585. This places bounded interactive
DivIcons above point and operations geometry but below label/office panes, and
prevents the transparent exact-site Canvas in `pane_points` from becoming the
hit-test target for a visible nested marker.

Nested hover and popup handlers are bound before the marker is eligible for
mount. Each marker records its coordinate ID, site count, creation generation,
mounted generation, mounted state, and interaction-ready state. A marker whose
hover/click handlers are incomplete is not added to the visible root. Root
detach, cached restoration, replacement, deferred disposal, filtering, and
stale-generation cancellation update the mounted state without rebinding
handlers.

Measure remains intentionally exclusive while active. A bounded
`MutationObserver` on the groundwater pane follows the existing Measure
suspension attribute/style lifecycle: `pointer-events:none` while active and
`pointer-events:auto` after Measure closes. This is self-contained in the
groundwater helper and also covers a pane created while Measure is already
active.

The reported intermittent state could not be browser-reproduced because the
required in-app browser connection was unavailable in this development
environment. Code and fixture evidence identify shared-pane Canvas/DivIcon hit
testing, plus stale Measure pane state, as the credible failure paths addressed
by the hardening.

The bounded diagnostic is silent by default:

```javascript
BRIM_USGS_GW_DEBUG.enable()
// Move the cursor over a nested marker, then:
BRIM_USGS_GW_DEBUG.inspectNestedAtCursor()
BRIM_USGS_GW_DEBUG.nestedLayers()
BRIM_USGS_GW_DEBUG.disable()
```

The cursor report includes location/site identity, handler counts, creation and
mounted generations, pane z-index and pointer state, Measure state,
`elementFromPoint`, zoom/move/render state, and whether the hit-tested element
belongs to the marker icon.

## Development profiler

Profiling is disabled by default. Enable it with either:

- `?brimProfile=1` on the standalone HTML URL; or
- `BRIM_USGS_GW_PROFILE.enable()` in the browser console.

Useful console calls:

```javascript
BRIM_USGS_GW_PROFILE.startScenario("home-first-on")
// Perform one scenario and wait until the map visibly settles.
BRIM_USGS_GW_PROFILE.endScenario()
BRIM_USGS_GW_PROFILE.downloadJson()
BRIM_USGS_GW_LOCAL.stats()
```

The report includes:

- scenario duration;
- phase durations for index, filter, viewport query/build, and mount;
- total long tasks, maximum duration, tasks at least 50 ms, and tasks at least
  200 ms;
- coarse long-task attribution to the active groundwater phase;
- marker, cluster, nested-icon, Canvas, SVG-path, DOM, and Leaflet layer
  snapshots;
- current bounded display-object counts;
- basemap tile loading and settled events;
- JavaScript heap values when `performance.memory` is available;
- generation, loading, display-cache, filter-index, and cleanup state.

Chrome may omit `performance.memory` unless precise memory reporting is
available. Record that result as unavailable; do not substitute an estimate.

## Manual full-application benchmark

Use the actual timestamped output of `build_final_map_only()` in
`BRIM_v0.38_codex_ship`. Do not substitute a synthetic point page.

For before/after comparability:

1. Use the same Chrome version, machine, display size, basemap, and network.
2. Close unrelated heavy tabs and DevTools panels.
3. Use a fresh reload for first activation and the same loaded page for repeat
   activation.
4. Run each scenario five times; report median and worst case.
5. Start a named profile immediately before the action.
6. End it only after groundwater status is idle and basemap tiles visibly
   settle.
7. Download each JSON report.
8. Capture console errors separately.

Required named scenarios:

| Scenario name | Action |
|---|---|
| `home_first_on` | At home, activate Local USGS monitoring wells. |
| `home_off` | At home after a completed draw, turn the layer off. |
| `home_repeat_on` | Reactivate without moving the map. |
| `home_clear_local` | Clear Local with the layer active. |
| `home_clear_all` | Clear All with the layer active. |
| `home_clear_during_load` | Clear Local immediately after activation. |
| `home_to_marquee_z11` | From home, marquee directly to a dense zoom-11 area. |
| `incremental_to_z11` | Reach the same area one zoom step at a time. |
| `dense_z11_pan` | Pan one viewport in a dense Central Valley/oil-field area. |
| `moderate_z11_pan` | Pan one viewport in a moderate-density area. |
| `sparse_z11_pan` | Pan one viewport in sparse northeast California. |
| `z10_to_z11_transition` | Cross from aggregate to exact display. |
| `z11_to_home` | Use the normal home control from zoom 11. |
| `z11_clear_local` | Clear Local with exact points drawn. |
| `z11_clear_all` | Clear All with exact points drawn. |
| `clear_during_marquee` | Clear while a marquee transition is pending. |
| `clear_nested_popup` | Open a nested popup, then Clear Local. |
| `clear_labels_state` | Exercise the existing label control state, then clear. |
| `measure_on_off` | Enable Measure, verify well hover suppression, then disable it. |
| `rapid_cycle_10x` | Repeat activate/off or activate/clear ten times. |

For each clear scenario, verify geometry, popup, tooltip, legend, and
interactive state disappear immediately; wait afterward only to confirm a stale
job does not recreate them.

## Focused automated checks

Run:

```bash
node qa/test_usgs_groundwater_virtualized.js
Rscript qa/qa_usgs_groundwater_performance.R \
  /path/to/04_processed_data/cache/latest/usgs_wells_map.rds \
  /temporary/output/directory
```

The Node fixture covers exact and aggregate display, nested semantics, dedicated
pane hit testing, pre-mount interaction readiness, stable handler counts across
cached off/on restoration, Measure pane suppression/restoration, Canvas
selection, lazy interactions, filtering, ordinary cached reactivation, explicit
clear, clear during pending render, stale-job rejection, listener teardown, and
equal radius/stroke across all eight water-level classes. It also checks the
common legend-dot geometry.

The R profiler writes count, nested-distribution, viewport-architecture, and
algorithm-timing CSV files plus a short Markdown summary. Its timings are
algorithm-only and must not be reported as browser/UI timings.

## Secondary audit

Large Local point families remain unchanged on this branch:

| Family | Retained features | Current strategy | Follow-up |
|---|---:|---|---|
| Springs | 27,278 | Viewport-virtualized on `feature/springs-performance`; compact columns, exact-coordinate locations, low-zoom aggregates, Canvas exact points, bounded labels, and generation cancellation. | See `BRIM_SPRINGS_PERFORMANCE.md`; rendered validation remains a separate gate. |
| CNRFC weather catalog | 3,797 | Global Canvas circle paths in MarkerCluster; rebuilds on activation/filter. | Moderate benefit, lower urgency. |
| SWRCB POD records | 3,510 | Three source-specific global MarkerCluster groups; source groups are reused until filters rebuild. | Useful if filter/clear cost becomes visible. |
| CNRFC legacy precip | 3,137 | Native clustered points; normally hidden by map configuration. | Low priority while hidden. |
| USGS streamgages | 2,387 | Global Canvas circle paths in MarkerCluster; objects reused on ordinary off. | Lower risk and lower payoff. |
| CNRFC river/reservoir | 2,047 | Global Canvas circle paths in MarkerCluster; rebuilds on activation/filter. | Lower priority. |
| CalSim3 nodes | 1,540 | Native point paths in the combined network layer. | Audit with its arc lifecycle, not as a groundwater copy. |

The groundwater JavaScript is kept in a separate, syntax-checkable file so its
columnar records, lazy immutable grid, viewport query, deterministic low-zoom
aggregation, Canvas singles, generation cancellation, bounded roots, movement
suspension, and profiler/diagnostic ideas can be factored into a shared
dense-point utility.

The subsequent `feature/springs-performance` work adapted the proven
viewport/index concepts without refactoring this groundwater controller. That
choice kept groundwater outside the implementation diff because its internals
did not yet expose a stable generic adapter boundary.

Spring-specific record fields, popup/tooltip content, label behavior,
symbology, filters, links, and duplicate-coordinate semantics remain in the
Springs controller. The retained Springs cache has 27,278 records and 27,265
unique seven-decimal coordinates: 13 duplicate-coordinate pairs, all within one
source family and none cross-source. The Springs redesign keeps every record
and uses spring-specific multi-record-location wording and interaction rather
than inheriting nested-well semantics.

HUC10 and HUC12 also remain unchanged:

| Layer | Features | Vertices | R object size | Existing likely embedded text estimate |
|---|---:|---:|---:|---:|
| HUC10 | 1,128 | 123,190 | 4,738,152 bytes | about 3.60 MiB |
| HUC12 | 5,065 | 259,996 | 17,346,696 bytes | about 11.39 MiB |

At this historical baseline both were configured under the map-wide Leaflet
Canvas preference, but the bundled Leaflet custom-pane lookup selected SVG for
`pane_huc`. Activation registered every polygon/path, and theme changes
recursively scanned map layers and called `setStyle` on every visible HUC
polygon. The later HUC-family correction and profiling gate are documented in
`BRIM_HUC_PERFORMANCE_AND_POPUPS.md`. Point viewport clustering does not apply
directly to polygon topology; geometry/simplification remains a separately
reviewed decision.

## Limits

Browser automation was unavailable in the Codex runtime used for this change.
No interactive time, long-task, tile-settle, warning, or heap result should be
claimed from the structural/profile fixtures. The manual full-application
protocol above is required before declaring the user-facing acceptance targets
met.
