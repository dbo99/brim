# BRIM Local Springs performance

## Scope and controlling boundary

This note documents the Local/static Springs redesign on
`feature/springs-performance`. It preserves the standalone single-HTML BRIM
delivery model and does not change source preprocessing, the retained
`springs_map.rds` cache schema, the separate live-feed repository, USGS
groundwater, or unrelated point layers.

The implementation lives in:

- `03_functions/leaflet_layer_local_well_spring_helpers.r`
- `03_functions/js/leaflet_springs_local_virtualized.js`

Focused validation lives in:

- `qa/test_springs_virtualization_prep.R`
- `qa/test_springs_virtualized.js`
- `qa/qa_springs_performance.R`

## Complete Springs path

The current source path is:

1. `00_config/config_local_layer_registry.r` registers layer ID `springs`,
   display name `Springs`, canonical group `Points – Springs`, cache
   `springs_map.rds`, and display switch `add_springs`.
2. `02_preprocess/17_springs.r` normalizes NHD and 2015–16 Mojave survey
   sources into `springs_combined_wgs84.rds`. It deliberately does not
   deduplicate coordinates and generates stable `spring_id` values.
3. `02_preprocess/62_update_springs_blm_distance_fields.R` optionally creates
   the BLM relationship/distance sidecar. It is not part of ordinary final-map
   rendering.
4. `05_map_build/02_build_core_map_cache.r` reads the combined source and
   sources
   `05_map_build/02_cache_blocks/04_cache_admin_water_reference_layers.r`.
5. That cache block builds `springs_map`, supplies source/name/symbol/link
   fields, optionally joins the BLM-distance sidecar, and writes
   `springs_map.rds`.
6. `05_map_build/04_build_portatreasure2_core_map.r` reads
   `springs_map.rds`, registers the Springs and optional Labels rows, and calls
   `pt_add_springs_layer()`.
7. `pt_add_springs_layer()` extracts exact WGS84 coordinates and a compact
   semantic record table. It refuses to invent missing IDs or omit invalid
   coordinates.
8. `pt_prepare_springs_virtualized_payload()` keeps the table columnar, sorts
   exact-coordinate members contiguously, and adds a location table containing
   only zero-based record starts and record counts.
9. `pt_add_springs_browser_layer()` embeds the compact records, locations,
   metadata, and the separate syntax-checkable JavaScript controller through
   `htmlwidgets::onRender()`.
10. `leaflet_springs_local_virtualized.js` handles lazy spatial indexing,
    viewport queries, aggregation, exact points, multi-record locations,
    filters, labels, hover/popup construction, movement, Measure, ordinary off,
    Clear Local, Clear All, cancellation, reuse, and profiling.

The visible Leaflet group rows remain dummy anchors so the established Local
catalog, brown Local treatment, inline `lbl` checkbox, and normal BRIM clear
machinery continue to own layer state.

## Source-repository baseline

The retained-cache measurements already tracked in this repository establish:

| Measure | Source-only evidence |
|---|---:|
| Analytical Springs records | 27,278 |
| Point coordinates | 27,278 |
| Unique locations at 7 decimal places | 27,265 |
| Duplicate-coordinate groups at 7 decimal places | 13 |
| Maximum records in those groups | 2 |
| Cross-source duplicate groups | 0 |
| Retained cache file size | about 0.643 MiB |
| Retained R object size | about 17.125 MiB |
| Estimated full cache geometry plus text payload | about 11.986 MiB |
| Estimated attribute JSON | about 11.315 MiB |
| Prebuilt cache popup text | about 1.964 MiB |
| Prebuilt cache hover text | about 0.771 MiB |

The 7-decimal duplicate result comes from the retained-cache audit recorded in
`BRIM_USGS_GROUNDWATER_PERFORMANCE.md`: 13 same-source pairs and no
cross-source group. The older general repository QA rounds to 6 decimal places
and reports 16 pairs. Those are different grouping precisions, not conflicting
record counts.

The payload estimates are structural cache measurements, not exact final-HTML
bytes. The source repository does not contain the retained production RDS or a
generated full BRIM HTML, so the new exact-coordinate counts, compact payload
bytes, and before/after HTML contribution must be rerun against
`BRIM_v0.38_codex_ship`.

## Previous architecture and verified bottlenecks

The previous on-render controller:

- converted the complete columnar payload into 27,278 JavaScript row objects
  while the visible layer was still off;
- rescanned all rows to construct the initially hidden legend;
- created one `L.Marker` and one `L.DivIcon` for every filtered analytical
  record on activation;
- eagerly created every icon and tooltip string;
- inserted all records into one global MarkerCluster index;
- used `disableClusteringAtZoom: 11`;
- kept all global marker and cluster-index objects even though only a small
  viewport subset was visible;
- synchronously cleared the full collection on ordinary off and rebuilt it on
  reactivation;
- forced the full index through the cluster-to-exact transition at high zoom.

Popup HTML was generated by a function on click, but its marker, icon, tooltip,
event binding, and cluster membership were eager. `removeOutsideVisibleBounds`
reduced visible DOM nodes; it did not avoid the complete marker population or
cluster index. Labels were already limited to the viewport and capped at 700,
but each update still scanned the complete filtered row-object array.

The old marker build and filtered rebuild were synchronous, so they had no
general generation token to cancel. The home/marquee guards could temporarily
remove the heavy root and used guarded timers, but they did not change the
global object population.

## Architecture decision

The selected approach adapts the proven groundwater virtualization design into
a separate Springs controller.

A generic shared-engine extraction was rejected for this branch because the
merged groundwater controller is a self-contained implementation with no
stable generic adapter boundary. Making it use a newly extracted engine would
turn a Springs performance change into a groundwater rewrite and expand the
rendered regression surface.

A thin Springs adapter around groundwater internals was rejected because those
internals are not exported. Importing or calling groundwater-specific state
would also risk transferring well terminology, filters, symbology, Ops cues,
and nested-well semantics into Springs.

The chosen controller reuses proven algorithms and lifecycle concepts without
modifying groundwater. A future shared-engine extraction should wait until
both controllers have passed rendered benchmarks and their common boundaries
are based on observed behavior rather than an untested abstraction.

## New browser architecture

All analytical records remain embedded as compact column arrays. R groups only
identical retained numeric longitude/latitude pairs, using each double's exact
hexadecimal representation as the grouping key. It never rounds, samples, or
deduplicates the record table.

The browser uses:

1. a lazy immutable 0.25-degree grid over coordinate locations;
2. a buffered current-viewport query;
3. deterministic screen-grid aggregates below zoom 11;
4. exact mapped coordinate locations at zoom 11 and above;
5. a dedicated Leaflet Canvas renderer for ordinary single-record points;
6. bounded `DivIcon` objects only for low-zoom aggregates and exact
   multi-record locations;
7. lazy tooltip and popup construction;
8. frame-budgeted index, filter, descriptor, marker, and label work;
9. generation-token cancellation for activation, filtering, movement, off,
   clear, refresh, and destruction;
10. immediate display-root detachment with bounded deferred child disposal;
11. safe same-view root reuse after ordinary off/on;
12. a disabled-by-default development profiler.

The NHD blue-dot and 2015–16 survey open-ring meanings remain separate and are
rendered on Canvas at exact zoom. Aggregate counts always mean individual
spring records. Aggregate hover also reports mapped coordinate locations.

## Analytical records versus coordinate locations

An analytical record is one retained source row identified by `spring_id`. A
coordinate location is one exact longitude/latitude pair used only to organize
display.

Multiple records at one exact coordinate are not called nested wells and are
not deduplicated. At exact zoom they use one bounded multi-record spring marker.
Its hover reports the record count, and its scrollable popup lists every record
separately with name, source, record ID, available scientific/detail fields,
BLM context, and links. If a filter leaves only one member, that member draws
as its normal source-specific point.

`qa/qa_springs_performance.R` classifies retained duplicate groups only from
available fields—for example, cross-source records, distinct GNIS records,
distinct/alternate names, or unresolved possible duplicate rows. The output is
an audit aid, not authority to merge or correct records.

## Filtering, labels, and lifecycle

The established Springs filters remain:

- source: all, NHD, or 2015–16 survey;
- BLM relationship/distance: any, on, off, within 1 mile, or within 5 miles.

`Any` is unconstrained. Source and BLM predicates are ANDed; each control offers
one selected value, so no hidden cross-filter OR behavior is introduced. Reset
changes filter state only and never changes map extent or turns the layer off.

Labels remain controlled by the standard `Labels – Springs` row, start at zoom
12, exclude explicit unnamed labels, use only visible filtered records, and
remain capped at 700. Label work has its own cancellation token and bounded
frame batches.

Ordinary off invalidates pending work, closes transient interaction, removes
the bounded display and labels immediately, and preserves a complete
same-view display root for fast reactivation. Clear Local and Clear All are
captured before checkbox cleanup, immediately remove the visible root, reject
pending generations, and drop the display-root cache. The immutable columns
and completed spatial index remain available because they are the embedded
source, not visible Leaflet geometry.

Zoom, pan, home, and marquee start detach visible Springs content before map
movement. Rendering resumes after movement settles so basemap tile work gets
the first opportunity to paint. Every scheduled callback checks its generation
and active state before it can mount anything.

Aggregate and multi-record `DivIcon` objects use the dedicated
`pane_springs_interactive` pane at z-index 584. Measure mode suppresses the
normal point pane and this dedicated pane, including a pane created while
Measure is already active, and restores interaction afterward.

The legend X hides only the legend. It does not clear geometry or change
filters.

## Profiling and focused QA

Profiling is off by default. Enable it with `?brimProfile=1` or:

```javascript
BRIM_SPRINGS_PROFILE.enable()
BRIM_SPRINGS_PROFILE.startScenario("home_first_on")
// Perform the scenario and wait for Springs/tile settling.
BRIM_SPRINGS_PROFILE.endScenario()
BRIM_SPRINGS_PROFILE.downloadJson()
BRIM_SPRINGS_LOCAL.stats()
```

The report includes scenario duration, generation/status events, long tasks
when the browser exposes them, current bounded object counts, filter/index
state, payload metadata, and JavaScript heap values when
`performance.memory` is available.

Source checks:

```bash
Rscript qa/test_springs_virtualization_prep.R
node qa/test_springs_virtualized.js
node qa/test_usgs_groundwater_virtualized.js
Rscript qa/qa_springs_performance.R \
  /path/to/04_processed_data/cache/latest/springs_map.rds \
  /temporary/output/directory
```

The R fixture checks exact grouping without rounding, record/field retention,
location totals, IDs, and refusal to lose invalid/duplicate-ID records. The
Node fixture checks exact and aggregate rendering, Canvas selection,
source-specific symbols, lazy hover, multi-record access, filters, labels,
Measure suppression, movement detach, ordinary reuse, explicit clear,
generation cancellation, marquee transition, and listener teardown. The
groundwater fixture is a required non-regression check.

The retained-cache QA writes counts, coordinate distribution, duplicate audit,
field retention, filter reconciliation, viewport object estimates, integrity
checks, R-only algorithm timing, and a short summary.

## Build implications

This feature changes only final-map R/JavaScript embedding and does not change
the retained Springs cache, label cache, core cache, source preprocessor, or
BLM-distance sidecar.

With a valid existing cache, the required build is:

```r
source("run_build_map.r")
build_final_map_only()
```

Do not rerun `02_preprocess/17_springs.r`,
`02_preprocess/62_update_springs_blm_distance_fields.R`, or the core-cache
build for this performance change.

## Rendered `codex_ship` validation

Use the actual timestamped output of `build_final_map_only()` in
`BRIM_v0.38_codex_ship`. Use one Chrome/machine/display/basemap setup, run five
iterations where timing is compared, and report median and worst case.

Required scenarios:

| Scenario | Required checks |
|---|---|
| Home first on | feedback, first activation, bounded aggregate objects, tiles |
| Home off / repeat on | immediate off and substantially faster same-view reuse |
| Home Clear Local / Clear All | immediate geometry, label, popup, tooltip, legend removal |
| Clear during first load | no stale aggregate or exact point reappears |
| Direct marquee to zoom 11 | no freeze, tiles paint, exact points reconcile |
| Incremental zoom to 11 | controlled aggregate-to-exact transition |
| Dense zoom-11 pan | usable pan, bounded exact objects, no stale points |
| Sparse zoom-11 pan | correct small query and no retained off-viewport points |
| Zoom 10 to 11 | record totals unchanged across transition |
| Zoom 11 to home | display detaches before movement and rebuilds after tiles |
| Source filters | all/NHD/survey counts and symbols reconcile |
| BLM filters | any/on/off/within-distance counts reconcile |
| Duplicate location | every record ID/name/source/link remains accessible |
| Labels | zoom threshold, filtering, 700 cap, layer-off and clear cleanup |
| Measure on/off | point and multi-record interaction suppresses and restores |
| Rapid cycle 10x | no listener, marker, DOM, heap, or stale-work growth |
| Groundwater regression | repeat its key activation, nested, Measure, and clear checks |

Also record console errors, Chrome non-responsive warnings, tile-settle
behavior, `BRIM_SPRINGS_LOCAL.stats()`, profiler JSON, DOM marker/canvas counts,
and heap availability. Measure the final HTML bytes and the Springs compact
payload bytes reported during build.

## Known limits and intentionally unchanged files

Source fixtures and R algorithms do not prove real BRIM browser latency,
long-task behavior, tile starvation, or heap stability. Those remain the
rendered gate.

No unrelated large point layer was migrated. Groundwater files, CNRFC point
families, SWRCB PODs, streamgages, CalSim nodes, source preprocessors, retained
data, caches, generated HTML, and the public live-feed repository are
intentionally unchanged.
