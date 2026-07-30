# BRIM Bulletin 118 thematic fills

## Scope and controlling guidance

This implementation is limited to the Local Bulletin 118 groundwater-basin
layer. The six HUC layers, their retained products, controller, selector, and
legend are unchanged; a later HUC-specific branch may reuse the fixed `%BLM`
scheme documented here.

The controlling repository documents are:

- `AGENTS.md`: keep production data external, preserve existing behavior, use
  the narrowest supported build, validate changed R/JavaScript, and state build
  implications.
- `README.md`, `BUILD.md`, and `run_build_map.r`: the core cache is built by
  `05_map_build/02_build_core_map_cache.r`; the final HTML is built by
  `05_map_build/04_build_portatreasure2_core_map.r`.
- `DATA.md`, `.gitignore`, and `EXTERNAL_DATA_MANIFEST.csv`: production
  geometry/RDS/cache/HTML products stay external. A small, authoritative,
  attribute-only source snapshot may be tracked when its refresh and validation
  are explicit.
- `CODEX_HANDOFF.md`: source, build-capable codex-ship, and full production
  workspaces have separate roles.
- `08_docs/BRIM_HUC_PERFORMANCE_AND_POPUPS.md`,
  `08_docs/BRIM_CALSIM3_PERFORMANCE_AND_DECLUSTERING.md`, and
  `08_docs/OPS_LIVE_USGS_CARDS_SCAN_INTERACTION.md`: direct retained-object
  references, cancellation, Measure cleanup, shared detachable cards,
  lower-left stacking, and responsive overflow are the current browser
  patterns.

Current source and retained schemas control where older prose differs.

## Existing Bulletin 118 path

The pre-change and retained-data path is:

1. `SRC$bull118_gw` in `00_config/config_source_files.r` points to the
   production shapefile `i08_B118_v6_2_calalb.shp`.
2. `safe_read_gw()` in `02_preprocess/02_huc_gw_county_pct_blm.r` creates:
   `basin_num` from `Basin_Numb`, `subbasin_num` from `Basin_Subb`, and the
   retained name fields from `Basin_Su_1`. It also creates `label`.
3. `compute_pct_blm()` transforms to EPSG:3310, calculates total and
   BLM-managed area from the dissolved BLM core, creates
   `percentBLMland`, and returns EPSG:4326.
4. The same preprocessor writes full-resolution
   `04_processed_data/rds/bull118gw_full.rds` and the combined GPKG/QA
   products. These analytical products are external.
5. `05_map_build/02_build_core_map_cache.r` reads `bull118gw_full.rds`.
   `05_map_build/02_cache_blocks/01_prepare_core_polygons.r` simplifies only
   the map-facing geometry with `keep = 0.05`.
6. `05_map_build/02_cache_blocks/02_cache_huc_climate_theme.r` calls
   `pt_make_gw_popups()`. The core-cache builder writes
   `04_processed_data/cache/latest/gw_bull118_map.rds`.
7. `00_config/config_local_layer_registry.r` registers layer ID
   `gw_bull118`, canonical group
   `Basins – GW Basins, Bulletin 118`, and cache file
   `gw_bull118_map.rds`.
8. `05_map_build/04_build_portatreasure2_core_map.r` reads the cache, registers
   the `(515)` feature count, and calls `pt_add_county_gw_layers()`.
9. `03_functions/leaflet_layer_local_polygon_helpers.r` creates the 515
   polygons, retains popup and sticky-hover bindings, and uses `pane_gw`
   (z-index 390). The uniform baseline is fill `#8B5A2B` at opacity `0.20`,
   boundary `#5A381E`, weight `1`, opacity `0.9`, with hover weight `2`.
10. The layer is off by default. This implementation hides its FeatureGroup
    before polygon registration and uses one explicit `pane_gw` Canvas, so the
    Canvas is first mounted when the layer is activated.
11. Ordinary off removes the existing group from the map. The Bulletin
    controller closes its popup/hover, hides/redocks the card, cancels pending
    styling, and removes the empty renderer; the original Leaflet polygon
    objects remain registered for reuse.
12. Clear Local clicks checked Local overlay controls. Clear All invokes that
    same Local clear path after ending Measure and clearing other session
    layers. Both therefore reach the Bulletin `overlayremove` cleanup.
13. Measure is authoritative: `leaflet_tools_adddata_panel.js` suppresses
    pointer events on `pane_gw`, closes transient feature UI, and emits
    `pt:measureinteractionchange`. The Bulletin controller closes any retained
    popup/hover on Measure activation; normal pane restoration handles exit.

The retained production and codex-ship `gw_bull118_map.rds` files audited on
2026-07-28 were identical:

- 515 rows and 515 unique `subbasin_num` values;
- 497 `POLYGON` and 18 `MULTIPOLYGON`;
- zero empty, 515 valid, EPSG:4326;
- fields: `basin_num`, `basin_name`, `subbasin_name`, `label`,
  `subbasin_num`, `total_area_sqmi`, `blm_area_sqmi`,
  `percentBLMland`, `geometry`, and `popup_html`;
- file size 534,680 bytes and R object size 1,426,192 bytes.

These are retained-product measurements, not source-fixture estimates.

## Authoritative 2019 SGMA source

The source is California DWR's final 2019 SGMA basin-prioritization service:

`https://gis.water.ca.gov/arcgis/rest/services/Geoscientific/i08_B118_SGMA_2019_Basin_Prioritization/MapServer`

BRIM queries only attribute table `MapServer/2`, requests only:

- `OBJECTID`
- `Basin_Subbasin_Number`
- `Priority`

The request uses `returnGeometry=false`. No source geometry, GeoJSON, or raw
JSON is retained. The service description identifies the 515-basin final 2019
results released December 6, 2019.

The service's feature layer now describes current 2025 basin geometry. That
geometry is intentionally not used: the service feature-layer metadata is read
only to validate the published renderer colors.

As verified on 2026-07-28, the renderer remains:

| Priority | DWR color |
|---|---:|
| High | `#FF0000` |
| Medium | `#FFFF00` |
| Low | `#55FF00` |
| Very Low | `#0070FF` |
| Defensive unmatched | `#9E9E9E` |

## Crosswalk refresh

`02_preprocess/68_refresh_bulletin118_sgma_2019_priority.R`:

1. validates that table 2 exposes exactly the three expected fields;
2. validates the layer-1 renderer colors;
3. submits an attribute-only query with `returnGeometry=false`;
4. rejects any geometry;
5. trims code whitespace without changing code content;
6. normalizes only the four expected priority strings;
7. requires 515 rows and 515 unique codes;
8. fails on missing/duplicate code or source OBJECTID;
9. requires exactly 46 High, 48 Medium, 11 Low, and 410 Very Low;
10. sorts by code using radix order; and
11. writes
    `00_config/bulletin118_sgma_2019_priority_crosswalk.csv`.

The tracked CSV contains normalized code/priority, source OBJECTID, table URL,
and access date. It contains 515 data rows and no geometry.

Run from the project root:

```r
source("02_preprocess/68_refresh_bulletin118_sgma_2019_priority.R")
```

## Exact join and cache QA

The core-cache builder reads the tracked CSV and calls
`pt_enrich_bulletin118_sgma_2019()` before map-facing simplification.

The join is:

`gw$subbasin_num` → `crosswalk$basin_subbasin_number`

It uses `match()` on normalized codes and assigns two new fields in place:

- `sgma_2019_priority`
- `sgma_2019_source_objectid`

It does not use basin names. Table 2 does not contain a name field, so no
attribute-only name comparison is possible; mismatch reports identify codes
only and names never participate in matching.

The join blocks the build unless it proves:

- 515 BRIM rows before and after;
- 515 unique BRIM and 515 unique source keys;
- 515 exact matches and zero unmatched in either direction;
- exact category totals;
- unchanged feature order;
- identical geometry;
- unchanged geometry-type, empty, and validity counts; and
- byte-for-byte R equality for every existing retained attribute.

The full-resolution analytical RDS is not overwritten. Enrichment is
materialized only in the rebuilt map-facing cache.

## Popup

`pt_make_gw_popups()` adds one row immediately after basin identification:

`DWR SGMA 2019 priority: High`

The category changes by basin. A future unmatched value renders
`No matched value`, although current cache QA requires zero unmatched values.
The popup includes a concise DWR SGMA 2019 source link and preserves the
existing label, basin name, `%BLM-CA`, BLM area, total area, Google search,
formatting, and units.

## Fixed percentage-BLM audit and palette

The audited field is the retained `percentBLMland`; no spatial recalculation is
performed. The following map-cache distribution was measured read-only on
2026-07-28:

| Layer | n | 0% | >0–1% | >1–5% | >5–15% | >15–30% | >30–50% | >50% | Missing |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| Bulletin 118 | 515 | 279 | 66 | 36 | 32 | 23 | 29 | 50 | 0 |
| HUC2 | 4 | 0 | 1 | 2 | 1 | 0 | 0 | 0 | 0 |
| HUC4 | 16 | 1 | 1 | 9 | 2 | 1 | 2 | 0 | 0 |
| HUC6 | 24 | 2 | 3 | 11 | 1 | 5 | 1 | 1 | 0 |
| HUC8 | 140 | 19 | 34 | 41 | 17 | 13 | 13 | 3 | 0 |
| HUC10 | 1,128 | 407 | 219 | 147 | 116 | 67 | 71 | 101 | 0 |
| HUC12 | 5,065 | 2,743 | 519 | 384 | 339 | 262 | 210 | 608 | 0 |

The candidate scheme remains useful across all levels: it isolates true zero,
resolves low overlaps, retains mid-range distinctions, and preserves meaningful
high-overlap classes without per-layer quantiles.

The fixed mapping is:

| Bin | Color |
|---|---:|
| 0% | `#F5F5F5` |
| >0–1% | `#FFF7BC` |
| >1–5% | `#FEE391` |
| >5–15% | `#FEC44F` |
| >15–30% | `#FE9929` |
| >30–50% | `#D95F0E` |
| >50% | `#993404` |
| Missing | `#9E9E9E` |

This is a colorblind-conscious yellow-to-brown sequential palette compatible
with BRIM's Local identity. The definition lives in
`03_functions/bulletin118_data_helpers.r` for later explicit HUC reuse.

## Unified thematic card and controller

The card title is `Bulletin 118 Groundwater Basins`. It contains exactly one
Display selector and one dynamic legend, with these choices:

1. Basins only
2. DWR SGMA 2019 Basin Prioritization
3. BLM-managed land — %

Basins only is the default and preserves the prior fill, boundary, opacity,
weight, hover, popup, and tooltip behavior.

The SGMA theme uses the DWR colors and retained category counts. The `%BLM`
theme uses the fixed bins above. Theme changes restyle the original 515 Leaflet
objects; there is no second polygon population, geometry reconstruction, popup
rebuild, runtime DWR request, or map-wide `eachLayer` scan.

Only the DWR theme has a bottom note: it identifies the final 2019 categories
and links to the DWR source. Basins only and `%BLM` end after their legend rows;
they do not allocate an empty note container.

### Minimum `%BLM` filter

The unified card includes `Minimum BLM-managed land`, a 0–100% range control
with one-percentage-point steps. Its default is 0%, and its live count reports
`n of 515 basins shown`.

Eligibility uses the retained `percentBLMland` value:

- 0% includes every retained basin, including a future missing value;
- a positive threshold includes only finite values greater than or equal to the
  selected threshold; and
- returning to 0% restores all 515 direct polygon references.

The controller does not make an invisible Canvas path. It removes an excluded
polygon from the existing Bulletin FeatureGroup, then adds that same object
back when it becomes eligible. The active theme is written to the polygon
options before re-add. Slider `input` events use one cancellable animation-frame
reconciliation, so rapid changes cannot leave mixed membership. Theme and
threshold state are independent.

### Local basin search

The same card includes `Find basin or subbasin…`. The final-map payload carries
only retained label, basin name, subbasin name, subbasin code, basin number, and
`percentBLMland` values. The controller builds one local lowercase search index
over all 515 records. It performs case-insensitive substring matching and
renders at most eight results with their basin/subbasin code.

Mouse selection, Up/Down, Enter, Escape, and the clear control are supported.
Selecting a result fits its existing polygon bounds with padding and opens its
existing popup. Search includes filtered-out records. If the chosen record is
below the active threshold, the controller deterministically resets the
threshold to 0%, reconciles membership, and only then fits and opens the basin.
The selected color theme is unchanged.

The controller builds one direct registry from Leaflet's Bulletin group. A
same-theme request is a no-op. A real change schedules one animation-frame
pass; a newer change, off, clear, or controller replacement cancels older work.
The legend updates after the completed pass. Activation primes retained layer
options and schedules one controlled redraw so first activation and controller
replacement are consistent; it does no style work while the layer is off.

The card is a `bottomleft` Local card using the shared detachable-card helper:

- the dock/undock arrow is immediately left of X;
- X hides only the card;
- detach enables drag and viewport clamp;
- dock returns it to the shared lower-left stack;
- shared obstacle-aware placement and overflow scrolling remain authoritative;
- there is no fixed screenshot coordinate or sticky inner header.

Ordinary off, Clear Local, and Clear All close Bulletin popup/hover and
autocomplete, cancel pending style/filter work, hide/redock the card, and
remove the empty Canvas renderer. Reactivation reuses the original objects,
reconciles the retained threshold, reapplies the selected theme, and shows one
card. X and detach/dock do not change theme or threshold. Measure uses the
shared pane suppression and closes retained Bulletin interaction on activation.

## Build implications

Changing only final-map/card/controller code would require:

```r
source("run_build_map.r")
build_final_map_only()
```

The SGMA join and popup row are stored in `gw_bull118_map.rds`. The current
runner has no Bulletin-118-only cache writer. The minimum supported rebuild is:

```r
source("run_build_map.r")
rebuild_core_cache_and_map()
```

That rebuilds all ordinary core-cache outputs and then the final HTML. It reads
the existing full-resolution processed products, including
`bull118gw_full.rds`; it does not rerun the HUC/GW/county spatial preprocessor.
No label-cache rebuild is required.

To refresh the remote attribute snapshot and then perform the supported
downstream build:

```r
source("run_build_map.r")
refresh_bulletin118_sgma_2019_and_map()
```

This performs the focused DWR refresh, the normal core-cache rebuild, and the
final-map build. It is not a Bulletin-only cache build.

## Source QA

Run:

```sh
Rscript qa/test_bulletin118_thematic.R
node --check 03_functions/js/brim_bulletin118_theme_control.js
node qa/test_bulletin118_controller.js
node qa/test_shared_card_layout_stability.js
Rscript tools/validate_source_repository.R
git diff --check
```

The R fixture checks the tracked source contract, exact join and preservation,
popup row/source/fallback, known priority example, fixed bin boundaries and
colors, three-theme payload, hidden-before-add registration, explicit Canvas,
pane, unique layer IDs, retained search/filter fields, note policy, threshold
counts, and embedded controller payload.

The JavaScript fixture checks direct references, no map scan, first activation,
all themes, same-theme no-op, rapid cancellation, off during pending work,
renderer/card cleanup, detached redock, X, off/on restoration, Measure popup/
tooltip cleanup, controller replacement, listener stability, threshold
membership, no invisible interactive paths, search mouse/keyboard selection,
Indian Wells Valley matching, fit/popup, and filtered-result reveal.

The shared-card fixture simulates fractional layout inputs, delivers the
helper's own MutationObserver records, and proves that a settled docked stack
causes no further style writes or queued animation frame.

## Codex-ship browser gate

In the build-capable checkout:

1. sync only the source files listed in the handoff manifest;
2. rerun the crosswalk refresh only if a new source access is intended;
3. run `rebuild_core_cache_and_map()`;
4. reconcile the rebuilt `gw_bull118_map.rds` to the old retained product:
   row/key/order, geometry binary/type/empty/validity, every old scientific
   field, new priority/object-ID fields, popup row, category totals, and size;
5. run `build_final_map_only()` only for later UI-only iterations.

Browser-test one unified Local card, selector/legend synchronization, exact
colors/counts, all three themes, same/rapid/return switching, hover, popup,
pan/zoom, Measure, ordinary off/on, X, detach/drag/clamp/redock, Clear Local,
Clear All, short-height overflow, card coexistence, reactivation, console, DOM/
Canvas counts, and final HTML size.

Regress HUC selector/legend without changing them, CalSim, Springs, Local USGS
groundwater, streamgages, Water Rights/shared legends, mapped conveyance,
Local upload, Ops/Measure, Local-panel rows, startup, and both clear paths.

## Known limits

- Rendered activation/theme timing, browser long tasks, visual color fidelity,
  final cache/HTML size, and cross-card layout require the codex-ship build and
  browser gate.
- The source service can change. Refresh fails rather than silently accepting a
  changed schema, renderer, row count, key set, or priority distribution.
- The current exact join is code-only. The source table has no basin-name field,
  so names cannot be compared without requesting a different source.
- HUC thematic implementation is explicitly deferred to a separate branch.
