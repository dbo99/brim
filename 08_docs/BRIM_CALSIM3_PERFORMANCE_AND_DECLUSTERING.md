# BRIM CalSim3 performance, declustering, and Network Explorer

## Scope and controlling rules

This note covers the Local `CalSim3.0` network on
`feature/calsim3-performance-decluster`. The implementation preserves the
standalone single-HTML architecture, retained analytical rows and geometry,
network fields, symbology, bound hover text, popup HTML and links, the single
combined Local checkbox, and zoom-9 declustering.

The controlling repository rules are:

- production data and generated products remain external;
- missing data may not be invented or silently omitted;
- map-facing optimization may not change canonical analytical geometry;
- only reviewed dependency stages may be run;
- UI work must cover ordinary off, Clear Local, Clear All, hover, popup,
  legends, Measure, filtering, Finder behavior, and browser performance; and
- source and retained-cache fixtures may not claim full-map Chrome
  performance.

Guidance and precedent inspected for this work includes `AGENTS.md`,
`README.md`, `BUILD.md`, `DATA.md`, `CODEX_HANDOFF.md`,
`08_docs/integrations/LIVE_DATA_FEEDS.md`, the current Local registry/build/helper source, shared
detachable-card and responsive-stack code, the BRIM-mapped conveyance
explorer, Springs/USGS groundwater/HUC performance notes and controllers,
shared Local clear controls, Measure lifecycle code, bundled Leaflet 1.3.1
and leaflet-binding 2.2.2 source, the CalSim popup builders, and the current
retained CalSim map products. Current source and retained schemas control
where historical notes differ.

## Complete data and build path

The current path is:

1. `00_config/config_source_files.r` registers the external
   `calsim3arcs.shp` and `calsim3nodes.shp`.
2. `02_preprocess/10_calsim3_arcs.r` reads both shapefiles. It filters arcs to
   `Channel`, `Diversion`, `Return`, and `Inflow`; retains arc identity and
   endpoint fields; retains every node with its CalSim ID, description, river
   name, comment, and exact point geometry; transforms to WGS84; and writes
   processed RDS products.
3. `05_map_build/02_build_core_map_cache.r` sources
   `05_map_build/02_cache_blocks/04_cache_admin_water_reference_layers.r`.
4. That cache block creates `calsim3_arcs_map` and `calsim3_nodes_map`, with
   retained popup, hover, class, color, line-width, radius, and stroke fields.
5. `00_config/config_local_layer_registry.r` registers both component rows
   under canonical group `Channels – CalSim3.0`; the catalog count sums both.
6. `00_config/config_labels.r` registers CalSim with the standard inline-label
   adapter used by the Local layer control.
7. `05_map_build/04_build_portatreasure2_core_map.r` registers one Local
   overlay row, one hidden `Labels – CalSim3.0` companion row, and calls the
   CalSim arc, node, label-companion, and browser-controller helpers.
8. `03_functions/leaflet_layer_local_reference_helpers.r` serializes one
   native Leaflet polyline per retained arc and one native Leaflet circle
   marker per retained node. It also adds the inert label companion used only
   for standard layer-control state and supplies the bounded label settings.
9. `03_functions/js/leaflet_calsim3_local_cluster.js` owns the unified
   Network Explorer, object-reuse filtering, Finder, independent hover
   controls, same-location cluster clicks, viewport-managed labels, transient
   cleanup, diagnostics, and optional profiling.
10. `03_functions/js/brim_legend_closeout_helpers.js` supplies the established
   dock/undock, drag, viewport clamp, lower-left responsive stack, and
   short-viewport overflow behavior.

There is no browser fetch, network sidecar, live-feed dependency, CalSim label
cache, alternate geometry encoder, or asynchronous geometry representation.
The default-on `Live apply` control owns only one cancellable animation-frame
request. Persistent CalSim labels are a separate browser-managed, high-zoom,
viewport-bounded display assembled from the retained Leaflet objects; hover
tooltips remain bound to those original objects.

## Retained-product baseline

The current July 2026 `codex_ship` retained map products were inspected
read-only. They were not copied, rewritten, or regenerated.

| Metric | Measured value |
|---|---:|
| Map-facing arcs | 2,166 |
| Map-facing nodes | 1,540 |
| Total Local catalog records | 3,706 |
| Valid node coordinates | 1,540 |
| Unique exact coordinate locations | 1,540 |
| Exact duplicate-coordinate groups | 0 |
| Minimum nearest-node distance | 5.709 m |
| Arc coordinate rows | 386,547 |
| Arc cache file | 4,958,897 bytes |
| Node cache file | 153,462 bytes |
| Distinct arc IDs | 2,156 |
| Repeated arc-ID values | 10 |
| Distinct node IDs | 1,536 |
| Repeated node-ID values | 4 |

Retained baseline node radii reconcile to:

| Baseline radius | Rows |
|---:|---:|
| 4.0 | 27 |
| 4.5 | 1,180 |
| 5.0 | 240 |
| 5.5 | 93 |

Arc classes reconcile to:

| Arc type | Rows |
|---|---:|
| Channel | 1,139 |
| Diversion | 544 |
| Return | 262 |
| Inflow | 221 |

Node grouped symbology reconciles to:

| Node group | Rows |
|---|---:|
| Conveyance | 1,092 |
| Storage / Reservoir | 84 |
| Non-project demand – ag | 68 |
| Treatment plant | 63 |
| Non-project demand – urban | 59 |
| Project demand – ag | 40 |
| External Unit | 27 |
| Settlement demand – ag | 23 |
| Project demand – urban | 22 |
| Project demand – refuge | 17 |
| Return flow | 17 |
| Major Feature | 9 |
| Demand – other | 8 |
| Unknown / Other | 8 |
| Settlement demand – urban | 2 |
| Non-project demand – refuge | 1 |

No retained arc or node ID is blank. Repeated IDs are reported and preserved,
not merged. The repeated node IDs are `DRM000`, `CSL005`, `SJR013`, and
`50_PA2`; each repeated row has a different coordinate.

## Retained fields and Arc_ID convention

The Explorer uses these compact, non-geometric retained indexes:

- arcs: browser key, `Arc_ID`, `FromNode`, `ToNode`, `Name`, `Type`,
  `line_col`, and `line_weight`;
- nodes: browser key, `node_id_display`/`CalSim3_ID`,
  `node_description`/`NodeDescri`, `riv_name_display`/`Riv_Name`,
  `comment_display`/`Comment`, `calsim3_node_group`, fill, stroke, and radius.

Popup HTML and hover text stay bound to the original Leaflet objects and are
not duplicated into the Explorer index.

`Arc_ID` is a retained source identifier, not a reliable endpoint parser.
Most IDs follow a type-prefixed convention:

- Channel: commonly `C_<FromNode>`;
- Diversion: commonly `D_<FromNode>_<ToNode>`;
- Return: commonly `R_<FromNode>_<ToNode>`;
- Inflow: commonly `I_<ToNode>`.

The read-only audit found 2,159 of 2,166 IDs with the expected type prefix and
2,108 matching the simple convention exactly. Fifty-eight retained rows do
not match the simple convention. Examples include branch suffixes such as
`C_SAC029A`, historical/mismatched source fields, missing endpoint attributes,
IDs whose prefix differs from retained `Type`, and `R-71_PA5_SJR091`.

Consequently, Finder endpoint queries use the explicit retained `FromNode`
and `ToNode` fields. They never split or reinterpret `Arc_ID`.

`Arc_ID` and node IDs are not unique enough to address browser objects. The
draw helpers therefore assign deterministic synthetic keys such as
`pt_calsim3_arc_00001` and `pt_calsim3_node_00001`. These keys exist only in
the serialized Leaflet layer manager. They do not replace, rewrite, display
as, or alter analytical IDs.

## Previous static legend and interaction architecture

Before the Explorer addition:

- arcs and nodes shared one Leaflet group and one Local checkbox;
- all retained popups and tooltips were bound directly to native objects;
- node markers were retained by one MarkerCluster group;
- a separate compact control displayed fixed, coarsened symbology;
- no component visibility controls, type filters, independent hover switches,
  or Finder existed;
- ordinary off removed the group root but retained its Leaflet objects for
  reuse; and
- the small CalSim controller owned zoom-9 exact-location behavior,
  transient tooltip/popup cleanup, Measure/off lifecycle, and diagnostics.

The obsolete static legend implementation was removed. The unified controller
creates the only CalSim card.

## Explorer design

The single lower-left Local-brown card is titled
`CalSim3.0 Network Explorer`. It contains:

- a compact `lbl` switch with a `z11+` note in the heading, immediately before
  the dock and close actions;
- exact shown/total analytical record counts;
- the zoom-9 transition note;
- one compact `Display` row for `Arcs`, `Nodes`, and both hover switches;
- one distinct, default-on `Live apply` control using the existing neutral
  BRIM control border and background;
- actual retained arc-type rows with line symbology and counts;
- collapsible, bounded retained node-group rows with point symbology and
  counts;
- `Apply` and `Reset`;
- independently bounded progressive Finder results;
- current status; and
- concise filter/interaction semantics.

Node filter values, ordering, counts, and title text remain the complete
retained strings. Only their visible labels are compacted:

| Retained filter value | Visible label |
|---|---|
| Conveyance | Conveyance |
| Storage / Reservoir | Storage |
| Project demand – urban | P — Urban |
| Project demand – ag | P — Ag |
| Project demand – refuge | P — Refuge |
| Non-project demand – urban | NP — Urban |
| Non-project demand – ag | NP — Ag |
| Non-project demand – refuge | NP — Refuge |
| Settlement demand – urban | Settlement — Urban |
| Settlement demand – ag | Settlement — Ag |
| Demand – other | Other demand |
| Treatment plant | Treatment |
| Return flow | Return flow |
| External Unit | External unit |
| Major Feature | Major feature |
| Unknown / Other | Unknown / other |

The node heading supplies the muted key
`P = project · NP = non-project`. All non-P/NP labels remain independently
understandable. The full retained value is also the row/input title and
continues unchanged in Finder, feature hover, and popup content.

The dock arrow is immediately left of the close action. The shared
detachable-card helper provides drag while detached, viewport clamping,
redocking, lower-left stacking, and corner overflow. The card has its own
short-viewport scroll. Its existing 348-pixel desktop width and existing
short-viewport maximum height are unchanged. The two-column node layout is
unchanged; its list scrolls within a bounded 128-pixel region.

The prior multi-clause footer was replaced by
`Hover controls tooltips only; click popups remain available.` The removed
Display checkbox and shortened footer offset the one-line Live apply control,
so neither the 348-pixel width nor the existing maximum card height changed.

The close action hides only the card. It does not turn off CalSim, change a
filter, change hover or label state, clear a popup, or zoom. A later ordinary
layer off/on shows the card again.

## Arc-hover root cause and solution

CalSim arcs retain visible line widths of roughly 1.0–1.4 pixels, but
`tolerance = 6` alone did not repair rendered hover. The rendered HTML proved
that CalSim created two explicit full-viewport Canvas elements:

- arcs in `pane_lines`, z-index 480; and
- exact nodes in `pane_points`, z-index 520.

Bundled Leaflet 1.3.1 attaches `mousemove` to each Canvas container. The
higher node canvas receives the browser event and searches only its own draw
list. Empty pixels on one Canvas do not fall through to a lower sibling
Canvas, so the arc renderer never reached its `_containsPoint` test and its
tolerance was irrelevant. This also explains the map-pan cursor seen over
arcs.

Arcs and exact nodes now both declare `pane_calsim3`, z-index 525. Leaflet
1.3.1's pane factory prefers SVG even on a `preferCanvas = TRUE` map, so while
CalSim is still off the controller primes the map-scoped pane-renderer cache
with one deferred `L.canvas({pane: "pane_calsim3", tolerance: 6})`. That
object has no DOM canvas until the first arc activation mounts it. Arcs
register before the MarkerCluster nodes, and later exact nodes resolve through
the same pane cache.

The cross-map audit found an additional constraint: any interactive Canvas in
that z-525 pane is a viewport-sized DOM hit target, even at unpainted pixels.
It could therefore block lower unrelated point, line, and polygon panes while
CalSim was active. This deterministic shielding risk is the strongest
explanation for the reported session-wide hover loss, although the exact
intermittent action sequence has not been reproduced from the source-only
gate.

The shared Canvas is now permanently `pointer-events: none`; its only job as a
DOM element is drawing. No new overlay or interaction surface was added.
Leaflet background map mouse events are forwarded into that same renderer's
native `_onMouseMove` and `_onClick` hit loop. Events propagated from an
unrelated Leaflet feature are not forwarded. A same-DOM-event guard also
preserves the unrelated feature's priority when another Canvas reports a hit
before the browser event bubbles to the map background. The one Canvas hit
loop still sees both retained CalSim populations: a node wins where its circle
is hit, while an arc remains reachable elsewhere.

Visible geometry, colors, widths, opacity, popup/tooltip bindings, and object
identity are unchanged. Individual-node radii now follow the documented
zoom tiers below; the retained baseline radii are unchanged. There is no
invisible interaction copy, duplicated path geometry, or second CalSim
renderer.

The optional `BRIM_CALSIM3_LOCAL.interactionState()` diagnostic reports the
Measure flag/class, marquee and teaching-tool classes, dragging and box-zoom
handler state, pane z-index and pointer-event values, mounted viewport
Canvases, known full-map interaction elements, CalSim renderer state, popup
and tooltip ownership, Finder focus/highlight state, and controller listener
count. `BRIM_CALSIM3_LOCAL.hitTargetAt(x, y)` reports the current top DOM hit
target without changing map behavior. Neither API logs by default.

The same audit found an independent cancellation gap in the existing
one-shot marquee control. Normal pointer-up and Escape already restored the
pre-marquee dragging state, but `pointercancel` and window blur did not. Those
two cancellation paths now call the same `setActive(false)` cleanup. Using
Finder or another map control also cancels marquee first, while the marquee
and reset buttons retain their own normal toggle behavior. Cleanup removes the
rectangle/mode class and restores dragging only when it had previously been
enabled. No Reference-layer behavior was changed.

## Filter and object-reuse semantics

The defaults are both components shown, every actual type/group selected,
both hover switches on, and `Live apply` on.

- Multiple selected arc types use OR.
- Multiple selected node groups use OR.
- Component visibility is ANDed with its selected classes.
- No selected class means no records shown for that component.
- With `Live apply` on, show/type/group/All/None changes reconcile on one
  animation frame. A newer selection cancels and supersedes pending work.
- With `Live apply` off, controls enter an explicit pending state and `Apply`
  executes the current UI selection.
- `Apply` is disabled while `Live apply` is on or when no manual change is
  pending.
- Hover switches act immediately because turning one off must close its
  current component tooltip.
- `Reset` acts immediately in either mode: it selects all actual classes,
  shows both components, restores both hover switches, clears Finder
  focus/highlight, and does not zoom.

Filtering reuses the original objects:

- arc paths are removed from or re-added to the existing combined group;
- existing node markers are removed from or re-added to the existing
  MarkerCluster group;
- hiding nodes removes the existing cluster root from the combined group;
- popup and tooltip bindings remain on the retained object; and
- class counts represent analytical records, never cluster-icon counts.

The 2,166 arc plus 1,540 node reconciliation remains one synchronous
object-reuse task inside its scheduled frame. Leaflet coalesces the Canvas
redraw, so no mixed partial state is intentionally presented. The pending
frame is bounded and cancellable; off, clear, Reset, or a newer UI selection
invalidates it. Diagnostics report apply duration, per-apply and cumulative
arc add/remove counts, node add/remove counts, and final shown counts. The
rendered browser gate must still measure whether the reconciliation itself
produces an unacceptable long task.

Applied show/type/group/hover state is preserved across ordinary off/on.
Clear Local and Clear All use that same normal overlay removal path, so they
also preserve applied settings for a later reactivation. `Reset` is the
explicit way to restore defaults.

## Hover, popup, Measure, and lifecycle

- Arc and node hover are independent and default on.
- Disabling one switch closes that component's current tooltip.
- Popup click remains available when hover is off.
- Only one current CalSim tooltip is retained at once.
- Opening a CalSim popup closes the current CalSim tooltip.
- Measure activation closes CalSim tooltip/popup/spiderfy state and cancels
  Finder highlight and renderer-hover state; the shared Measure pane
  suppression remains authoritative.
- Pan/zoom closes transient CalSim tooltip/popup/spiderfy state.
- Ordinary off, Clear Local, and Clear All synchronously remove the combined
  group, close transient interaction, clear Finder result/focus/highlight,
  remove the shared renderer, clear its cursor state, redock any detached
  card, and hide it.
- Controller replacement restores complete object membership before the new
  controller attaches, then removes old listeners, renderer-hover state, and
  card state.
- Unload cancels timers, removes listeners/card state, and leaves unrelated
  layers untouched.

## Finder fields and behavior

Finder precomputes one lowercase search string per retained record when the
controller installs. It searches:

- arcs: `Arc_ID`, explicit `FromNode`, explicit `ToNode`, `Name`, and `Type`;
- nodes: node ID, description, river name, comment, and node group.

Search is case-insensitive and stable. Matches rank in this order:

1. exact ID;
2. ID prefix;
3. exact endpoint/node name or description;
4. endpoint/node name or description prefix;
5. whole term;
6. primary-field substring; and
7. secondary description/class substring.

Ties retain source index order. Typing never zooms. The initial DOM contains
at most 12 results and reports `Showing 12 of N`. `Show more` adds batches of
12; `Show all` is offered when the total is at most 200; and `Show fewer`
collapses to 12. Expansion retains the query and result-list scroll state and
only rebuilds result DOM from the already indexed records. It does not add,
remove, or reconstruct map objects.

Arc results display `From → To`, Arc ID, Type, and Name when available. Node
results display the node ID plus group, description, and river when available.
Each result is explicitly labeled `Arc` or `Node`; raw JSON is never shown.

Selecting a visible result zooms without animation, highlights the existing
object, and opens its retained popup. Selecting a filtered-out result does not
zoom or silently change filters. It reports that the result is filtered and
offers `Show`. `Show` explicitly turns on that component, adds the one needed
type/group to the current applied selection without discarding other selected
classes, applies, then focuses the result.

Off, clear, reset, replacement, Measure activation, and a later Finder focus
invalidate the pending focus timer and restore any prior highlighted style.

## Persistent label rules and bounded rendering

CalSim uses the standard Local-row inline `lbl` switch and a matching Explorer
heading switch. Both are thin adapters over the one hidden
`Labels – CalSim3.0` companion checkbox, so there is one canonical requested
state rather than two independent settings. Label state defaults off. Either
visible switch updates the other through that canonical state. Turning the
main CalSim layer off also turns labels off, consistent with the existing
shared inline-label behavior; closing only the Explorer card does not.

The canonical internal groups are `Channels – CalSim3.0` and
`Labels – CalSim3.0`; `CalSim3.0` is the curated visible short name and
`Labels: CalSim3.0` is only the pre-normalization R spelling. The shared
inline-label adapter owns main-row/companion-checkbox lifecycle. The CalSim
controller updates its browser label root immediately when the main group is
removed, but deliberately does not click the companion checkbox from inside
Leaflet's main-checkbox transaction. The native main-input `change` event runs
after that transaction and synchronously reconciles the companion and inline
controls. This prevents a nested Leaflet 1.3.1 click from clearing its
non-stacked `_handlingClick` guard, re-adding the companion, and rebuilding
the entire layer-control DOM from raw group names.

Labels render only while CalSim is active, the canonical label state is on,
Measure is inactive, and map zoom is 11 or higher. Checking labels below zoom
11 does not zoom the map and does not discard the checked state. Labels
disappear below zoom 11 and return after zooming back to 11 or higher.

The retained fields were audited before choosing label text:

- all 1,540 node rows have a nonblank retained ID; 1,536 IDs are unique, with
  a median length of 6 and maximum length of 9;
- 1,532 node descriptions are nonblank, but they comprise only 22 repeated,
  generic technical classes such as `conveyance-channel`, so they are not
  useful persistent names;
- 1,594 of 2,166 arc names are nonblank and there are 356 distinct nonblank
  values; name length has a median of 16, 90th percentile of 26, and maximum
  of 59;
- 572 arc names are blank, 125 are `<Null>`, and 161 are the generic
  `Unlabeled Return Flow`;
- 1,931 arcs have both explicit retained endpoints available; and
- every arc has a nonblank retained `Arc_ID`, with maximum length 18.

The display-only rules are therefore:

1. Node label: the retained node ID. Generic node descriptions remain
   available in Finder, hover, and popup but are not used as persistent text.
2. Arc label: a meaningful retained `Name` no longer than 32 characters,
   excluding blank, `<Null>`, and `Unlabeled Return Flow`.
3. Arc fallback: explicit retained `FromNode → ToNode` when both endpoints
   exist and the result is no longer than 24 characters.
4. Final arc fallback: retained `Arc_ID` when no longer than 20 characters.

Arc endpoints are never inferred by parsing `Arc_ID`. Repeated meaningful arc
names are shown once within the current viewport selection. All text is
escaped before insertion.

The controller does not create one permanent label object per analytical
record. At install it derives lightweight label descriptors from the existing
retained layer references, locates their anchor coordinates, and indexes them
once in 0.25-degree cells. A settled pan, zoom, filter change, label-on action,
or Measure exit queries only cells intersecting the current viewport plus a
15% buffer. The result is capped at 160 labels, with 96 node and 64 arc slots
reserved first; unused reserved slots can be filled by the other family.
Candidates are deterministic: useful text first, then distance to viewport
center, then retained source order. This keeps both families represented
without duplicating the network or geometry.

Only the selected descriptors become markers in one reusable label root.
Node labels use `pane_labels_pts`; arc labels use `pane_labels_poly`.
All label markers are noninteractive and retain the established BRIM white
halo. The current label root is removed immediately at movement/zoom start,
filter reconciliation, Measure activation, label/main off, clear, or
controller replacement. A generation token cancels stale scheduled work, and
only the latest settled state can repopulate the root. Diagnostics report
requested/eligible/rendered state, family counts, index size, render totals,
and cancellation counts.

## Zoom-9 preservation

The node architecture from the performance/declustering work is unchanged:

- CalSim starts hidden before arc and node registration;
- arcs and individual nodes use the one shared `pane_calsim3` Canvas;
- arcs register before clustered nodes, preserving Canvas hit/draw order;
- MarkerCluster animation remains off;
- current retained nodes use `disableClusteringAtZoom = 9`;
- all 1,540 current nodes have unique exact coordinates;
- zoom 9+ therefore shows every current node as an individual Canvas path;
- a future exact-coordinate group alone uses the bounded one-millionth-pixel
  radius and click spiderfy fallback; and
- filtering adds/removes the same markers from that same cluster group, so it
  does not introduce a second cluster/individual population.

Individual nodes use three zoom-radius tiers:

| Map zoom | Baseline scale | Retained radii 4.0 / 4.5 / 5.0 / 5.5 |
|---|---:|---|
| below 10, including 9 and 9.5 | 58% | 2.32 / 2.61 / 2.90 / 3.19 |
| 10 and 10.5 | 76% | 3.04 / 3.42 / 3.80 / 4.18 |
| 11 and above | 100% | 4.00 / 4.50 / 5.00 / 5.50 |

The controller calls `setRadius()` on each existing retained CircleMarker
only when the tier changes. It neither creates replacement markers nor
changes cluster icons, coordinates, data, counts, popup/tooltip bindings,
Finder records, filters, or declustering. Repeated `zoomend` events within one
tier perform no marker update. While zoomed below 9, MarkerCluster icons remain
authoritative; resizing their retained child markers does not resize or
replace the cluster icons. The shared Canvas tolerance remains six pixels, so
the compact visible radius does not remove the established pointer hit
tolerance. Finder highlighting derives from the current displayed radius and
restores that tier radius after focus cleanup.

Radius diagnostics report the requested/applied tier and scale, baseline,
target, and currently effective ranges, tier transitions, actual marker
updates, same-tier skips, and inactive skips. Off invalidates the applied
tier; reactivation applies the current zoom tier to the same marker objects
and avoids a `setRadius()` call when their radii are already correct.

No geometry simplification, coordinate rounding, record aggregation, ID
deduplication, or alternate zoom threshold was introduced.

## Source and retained-product QA

Focused deterministic fixtures:

```sh
Rscript qa/test_calsim3_performance.R
node qa/test_calsim3_cluster_controller.js
node qa/test_local_layer_control_dom.js
```

The R fixture checks hidden-before-add ordering, unique browser keys, the
shared interactive pane, serialized Canvas tolerance, current and
exact-duplicate cluster options, popup/hover retention, controller indexes
and stable class order, explicit endpoint retention, retained-marker radius
tier wiring, the hidden noninteractive label companion, serialized label
threshold/caps/index settings, the shared inline-label registrations, distinct
default-on Live apply markup, the hover/popup note, and invalid-coordinate
refusal.

The Node fixture checks object resolution and counts, type/group filters,
independent show controls, zero-selection behavior, Reset, hover suppression,
popup availability with hover off, Measure cleanup, pass-through Canvas
state, background-only CalSim hit forwarding, unrelated feature priority,
same-event Canvas-hit suppression, all Finder matches, ranking,
initial/progressive caps, DOM-only expansion, all Finder field families,
Arc/Node distinction, filtered-result behavior, Live apply scheduling and
supersession, manual pending/apply, operation diagnostics, exact 58/76/100%
radius tiers, same-tier update suppression, retained marker identity,
zoom-aware highlight restoration, zoom/highlight/popup focus, off/on state
preservation, cluster clicks, synchronized default-off Local/Explorer label
controls, below-threshold state retention, zoom restoration, audited node/arc
label fallbacks, viewport filtering, family quotas and total cap, movement and
filter cancellation, noninteractive established panes, Measure label
cleanup/restoration, listener stability, replacement, and teardown. It also
source-checks Measure
pane restoration, marquee dragging restoration, the unified card, compact
node-label mapping and titles, unchanged dimensions, action order, responsive
scroll, shared detachable registration, one shared Canvas, lack of geometry
reconstruction, removal of the static-legend build call, and representative
Reference-point, Reference-polygon, Major Conveyance, and Water Rights
hover/popup binding contracts without editing those layers.

The Local layer-control DOM fixture models Leaflet 1.3.1's exact
main/companion input transaction, reproduces the former nested-click rebuild,
and proves the corrected CalSim sequence performs no rebuild. It snapshots
row order, curated visible text, hidden companion rows, inline-control count
and listener count across first activation, Explorer/inline synchronization,
main off, repeated off/on, Clear Local, Clear All, HUC, USGS streamgages, and
Water Conveyance.

Read-only retained-product diagnostics:

```sh
Rscript qa/qa_calsim3_performance.R \
  "/path/to/calsim3_arcs_map.rds" \
  "/path/to/calsim3_nodes_map.rds" \
  "/temporary/calsim3_qa"
```

That diagnostic writes counts, feature classes, near-coordinate distribution,
repeated-ID rows, required-field presence, Arc_ID convention rows/summary,
integrity checks, and structural size metrics. Its distance and object-size
measurements are R diagnostics, not browser performance.

The compact Explorer/controller metadata serialized from the current retained
products was measured at approximately 688,845 JSON bytes before final
self-contained HTML encoding. That is not the final HTML size delta and must
be measured in the rendered build.

## Build and cache implications

This change affects only final-map R/JavaScript serialization and browser
behavior. It does not change raw preprocessing, processed RDS products, core
cache schema/content, the shared label cache, analytical geometry, source ID
fields, or the Local catalog count. CalSim persistent labels are generated
from retained browser objects and require no label-cache rebuild.

With valid current caches, the required build is:

```r
source("run_build_map.r")
build_final_map_only()
```

Do not run `02_preprocess/10_calsim3_arcs.r`, rebuild the core cache, rebuild
labels, or run all preprocessors for this change.

## `codex_ship` source sync manifest

Build-required source files:

1. `00_config/config_labels.r`
2. `03_functions/leaflet_core_helpers.r`
3. `03_functions/leaflet_layer_local_reference_helpers.r`
4. `03_functions/js/leaflet_calsim3_local_cluster.js`
5. `03_functions/js/brim_legend_closeout_helpers.js`
6. `05_map_build/04_build_portatreasure2_core_map.r`

Source-parity documentation and QA:

7. `08_docs/BRIM_CALSIM3_PERFORMANCE_AND_DECLUSTERING.md`
8. `qa/qa_calsim3_performance.R`
9. `qa/test_calsim3_cluster_controller.js`
10. `qa/test_calsim3_performance.R`
11. `qa/test_local_layer_control_dom.js`

No raw data, processed products, caches, generated HTML, or temporary QA
outputs belong in the sync.

## Required rendered-browser gate

Use the current build-capable project, sync only the reviewed manifest, run
`build_final_map_only()`, and test the resulting self-contained HTML in Chrome.
Use the same machine, Chrome version, viewport, basemap, cache state, and
DevTools setup for before/after comparison.

Functional acceptance:

1. Start with CalSim off. Confirm no CalSim card, shared CalSim Canvas, or
   cluster icon is mounted.
2. Activate CalSim and reconcile 2,166 arcs, 1,540 nodes, four arc types, and
   sixteen node groups against displayed analytical counts.
3. Verify card lower-left stacking with Water Rights and other legends,
   dock/undock, drag, clamp, redock, close-only behavior, and scroll at short
   heights and the unchanged 348-pixel desktop width. Confirm compact node
   labels, full row titles, right-aligned counts, and the P/NP key.
4. Confirm both label switches default off and remain synchronized. Check at
   zoom 10 without changing zoom, then pan and zoom through 10→11→10→11;
   confirm checked-state retention, disappearance/restoration, both label
   families, full node-group text in Finder/hover/popup, noninteractive label
   hit behavior, and no stale label after rapid movement or filter changes.
   Close/reopen the Explorer and confirm label state is unchanged. Turn
   CalSim off and confirm both label switches turn off.
5. Apply each single arc type, multi-type OR, all, none, Arcs off, both
   components off, and Reset. Verify no stale hover/popup target.
6. Apply each single node group, multi-group OR, all, none, Nodes off, and
   Reset below zoom 9 and at zoom 9+.
7. Hover thin and dense arcs at multiple zooms. Confirm tolerance is reliable
   without a visibly wider stroke or duplicate popup target.
8. Toggle arc hover and node hover independently; verify current tooltip
   closure, popup click while hover is off, and correct restoration.
9. Exercise partial Arc ID, explicit from-node, explicit to-node, arc name,
   node ID, node description, river, comment, and group queries. Confirm
   ranked Arc/Node-labeled results and no zoom while typing. Query `American`
   and confirm all 118 matches are reachable through 12-result initial view,
   Show more, Show all, and Show fewer without map-object changes.
10. Select visible arc and node results; verify zoom, temporary highlight, and
   the existing popup. Select filtered results; verify no implicit zoom, clear
   status, explicit `Show`, and preservation of other selected classes.
11. With Live apply on, exercise None → Diversion, All → None, one node group,
    restore all types, and simultaneous arc/node changes. Confirm one final
    state, disabled Apply, and operation diagnostics. Turn Live apply off,
    confirm pending status and manual Apply, then verify immediate Reset in
    both modes.
12. Test repeated analytical IDs and the closest current node pair. Confirm
    separate objects remain independently interactive.
13. Test zoom 8→9, 9→10, 10→10.5, 10.5→11, 11→12, and each reverse
    transition. Confirm 58%, 76%, and baseline radii, no updates within a tier,
    unchanged cluster icons below zoom 9, dense/sparse zoom-9 pan, repeated
    filter application, and no cluster/individual duplication.
14. Test Measure over arcs, nodes, clusters, labels, and any spiderfied exact-location
    group, with each hover switch combination.
15. Test ordinary off/on, Clear Local/reactivation, and Clear
    All/reactivation with tooltip, popup, Finder focus, detached card, and
    filter state active.
16. With CalSim active, repeat the preceding lifecycle sequences while
    checking hover and popup behavior for a representative Reference point,
    Reference line, Reference polygon, HUC polygon, Spring, Local USGS
    groundwater point, Water Right, and Major Conveyance feature. Include
    Measure on/off and marquee completion. At any failure, capture
    `interactionState()` plus `hitTargetAt(clientX, clientY)` before reloading.

Performance acceptance:

1. Measure CalSim off at startup, first activation, repeat activation, dense
   arc selection, dense node selection, filter Apply, Reset, 8→9, 9→8,
   dense/sparse zoom-9 pan, off, Clear Local, and Clear All.
2. Record long tasks, non-responsive warnings, basemap responsiveness,
   cluster/Canvas/DOM counts, heap trend where available, and final HTML bytes.
3. Confirm repeated Apply/off/on does not accumulate listeners, objects,
   Canvas elements, cluster icons, label roots/markers, result handlers, or
   heap.
4. Confirm the one-frame, synchronous 3,706-object reconciliation remains
   acceptable and that newer selections cancel the prior pending frame.
5. At zoom 11+, pan dense and sparse extents with labels on. Confirm the
   rendered label total never exceeds 160, unused labels are removed before
   movement completes, only the latest viewport/filter state renders, and no
   permanent 3,706-label population appears.

Regression acceptance:

- Springs;
- Local USGS groundwater;
- HUC;
- Major Conveyance;
- Water Rights/shared lower-left stack;
- Local upload;
- Measure;
- Local clear controls;
- basemap responsiveness; and
- browser console.

Enable optional diagnostics with `?brimProfile=1` or:

```javascript
BRIM_CALSIM3_PROFILE.enable()
BRIM_CALSIM3_PROFILE.startScenario("calsim3-explorer-apply")
// Perform one scenario and wait for map/tile rendering to settle.
BRIM_CALSIM3_PROFILE.endScenario()
BRIM_CALSIM3_PROFILE.downloadJson()
BRIM_CALSIM3_LOCAL.stats()
BRIM_CALSIM3_LOCAL.labelRules()
BRIM_CALSIM3_LOCAL.interactionState()
BRIM_CALSIM3_LOCAL.hitTargetAt(clientX, clientY)
```

Source fixtures do not establish production Chrome latency, transition
smoothness, tile starvation, long-task acceptability, heap stability, rendered
card layout, Canvas hit accuracy, or final HTML size. Those remain explicit
rendered-browser uncertainties.
