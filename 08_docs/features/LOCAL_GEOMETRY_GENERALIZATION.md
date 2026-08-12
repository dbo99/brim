# Local geometry generalization and public disclosure

## Scope and result

This document inventories BRIM-side vertex reduction for the 34 polygon and
polyline layers currently rendered in the Local panel. It covers Core, Basins,
Channels, Reference, and the optional NPS context nested under National
Monuments. It excludes points, disabled/retired Local layers, External, Ops
Live, uploads, and QA/sandbox geometry.

As of the source state recorded below:

- **34** current Local polygon/polyline layers were audited;
- **23** use BRIM-generated generalized display geometry;
- **11** do not use BRIM-generated generalized display geometry; and
- **0** active paths remain at `REVIEW`.

The row-level machine-readable authority for this audit is
[`local_geometry_generalization_inventory.csv`](local_geometry_generalization_inventory.csv).

## Interpretation contract

Generalization is only vertex reduction performed while producing or rendering
a BRIM display product. It is not a claim about positional accuracy, legal
status, survey quality, or source authority. A `keep=0.50` setting means the
simplifier was asked to retain 50% of vertices, not that the result has 50%
positional accuracy. Actual observed vertex ratios can differ because shape
safeguards, multipart geometry, and endpoint retention apply.

The inventory keeps these operations distinct:

- **source geometry** — the acquired or accepted input geometry;
- **repair** — validity cleanup such as `st_make_valid()`;
- **aggregation** — dissolve, union, classification, or masking;
- **generalization** — genuine vertex reduction for display;
- **reprojection** — coordinate reference system transformation; and
- **clipping** — spatial cropping or intersection.

Repair, aggregation, reprojection, and clipping do not become generalization
merely because they can change an `sf` object. The curated Water conveyance
controller explicitly uses Leaflet `smoothFactor=1.5`; that is transient,
per-zoom screen-path rendering and does not reduce the cached or embedded
coordinate geometry. It is therefore recorded but not counted as BRIM cached
display-geometry generalization. Ordinary Leaflet renderer defaults are treated
the same way.

## Generalized layers

Counts below were either already recorded by accepted feature QA/metadata or
obtained by one read-only count of current source/display artifacts in the
isolated build workspace. “Input” means the immediately comparable source or
repaired geometry entering the active simplification step. Blank counts were
not rebuilt merely to complete the table.

| Public layer(s) | Geometry | Method and active parameter | CRS | Input → display vertices | Public disclosure |
|---|---|---|---|---:|---|
| BLM Field Office Boundaries | polygon | `ms_simplify`, `keep=0.20` | EPSG:4326 | 345,154 → 80,899 | Generalized display geometry. Check authoritative source for boundary-sensitive use. |
| Bulletin 118 Groundwater Basins | polygon | `ms_simplify`, `keep=0.05` | EPSG:4326 | 707,212 → 36,993 | Generalized display geometry. Check authoritative source for boundary-sensitive use. |
| CNRFC Product Availability | polygon | inherits CNRFC basin `ms_simplify`, `keep=0.12`; no second reduction | EPSG:4326 | not comparable → 35,059 | Generalized display geometry. Check authoritative source for boundary-sensitive use. |
| CNRFC FNF Sha/Tri/west Sierra Basins | polygon | `ms_simplify`, `keep=0.80` | EPSG:4326 | 18,510 → 15,470 | None: no existing natural legend/card, so no new card was created. |
| HUC2, HUC4, HUC6, HUC8, HUC10, HUC12 | polygon | `ms_simplify`, `keep=0.03` | EPSG:4326 | 641,429 → 19,878; 902,148 → 27,790; 883,018 → 27,584; 1,418,071 → 46,085; 3,611,843 → 123,190; 6,797,854 → 259,996 | Generalized display geometry. Check authoritative source for boundary-sensitive use. |
| Wild & Scenic Rivers: BLM-CA lines; USFS/interagency segments | polyline | `ms_simplify`, `keep=0.06` | EPSG:4326 | 135,402 → 8,812; 143,458 → 9,138 | Generalized display geometry. Check authoritative source for boundary-sensitive use. |
| WSR corridors: BLM-CA; USFS/LSRS areas; USFS/LSRS legal status | polygon | `ms_simplify`, `keep=0.06` | EPSG:4326 | 197,351 → 16,093; 137,169 → 8,764; 138,236 → 8,763 | Same shared WSR footer as the line layers. |
| National Monuments | polygon | `st_simplify`, 1 m tolerance | EPSG:3310 | 256,149 repaired → 175,540 | Generalized display geometry. Check authoritative source for boundary-sensitive use. |
| California Desert National Conservation Lands | polygon | `st_simplify`, 2 m tolerance | EPSG:3310 | 84,155 repaired → 32,168 | Generalized display geometry. Check authoritative source for boundary-sensitive use. |
| Federal Wilderness | polygon | `ms_simplify`, `keep=0.50` | EPSG:3310 | not recorded → 390,055 | Generalized display geometry. Check authoritative source for boundary-sensitive use. |
| Areas of Critical Environmental Concern | polygon | `st_simplify`, 1 m tolerance | EPSG:3310 | 217,760 → 61,066 | Generalized display geometry. Check authoritative source for boundary-sensitive use. |
| Counties | polygon | `ms_simplify`, `keep=0.05` | EPSG:4326 | 1,010,950 → 72,534 | Generalized display geometry. Check authoritative source for boundary-sensitive use. |
| Water Districts | polygon | `ms_simplify`, `keep=0.12` | EPSG:4326 | 2,255,585 → 389,000 | Generalized display geometry. Check authoritative source for boundary-sensitive use. |
| NPS National Parks context; NPS National Preserve context | polygon boundary and tract-based fill | `st_simplify`, 2 m boundary and 5 m land/interest tolerances | EPSG:3310 | combined 10-unit display: 46,433; source counts are not directly comparable after classification/masking/dissolve | Generalized display geometry. Check authoritative source for boundary-sensitive use. |

All `ms_simplify` paths above use `keep_shapes=TRUE` and `explode=FALSE`.
The shared helper performs validity cleanup before and after simplification.
The fixed-tolerance paths use `preserveTopology=TRUE`. National Monuments,
California Desert NCL, ACECs, and Federal Wilderness arrive in the shared
Reference cache with their focused display generalization already complete;
the shared cache does not reduce them a second time.

The NPS context has two genuine display reductions after tract-interest
classification, masking, dissolve, and validity repair: 2 m for legislative
boundaries and 5 m for land/interest fills. The technical settings remain in
this inventory; both the context note and main National Monument footer use the
common public disclosure sentence.

## Investigated and not generalized

| Public layer | Geometry | Active finding |
|---|---|---|
| BLM-CA Managed | polygon | `keep=1.00`; shared helper performs cleanup but bypasses simplification. |
| BLM Held/Managed Differences | polygon | `keep=1.00`; shared helper performs cleanup but bypasses simplification. |
| Groundwater Sustainability Plan Areas | polygon | Reference manifest `simplify_keep=1`; bypassed. |
| Adjudicated Groundwater Basins | polygon | Reference manifest `simplify_keep=1`; bypassed. |
| Water conveyance \| BRIM mapped | polyline | Cached/embedded coordinates are not reduced; explicit Leaflet smoothing is renderer-only. |
| CalSim3.0 arcs | polyline | `keep=1.00`; shared helper performs cleanup but bypasses simplification. |
| National Scenic/Historic Trails | polyline | Reference manifest and accepted cache both use `simplify_keep=1`; bypassed. |
| Wilderness Study Areas | polygon | Reference manifest and accepted cache both use `simplify_keep=1`; bypassed. |
| DRECP Planning Area Boundary | polygon | Reference manifest `simplify_keep=1`; bypassed. |
| Grazing Allotments | polygon | Reference manifest `simplify_keep=1`; bypassed. |
| RWQCB Regions | polygon | `keep=1.00`; shared helper performs cleanup but bypasses simplification. |

No disclosure was added to these layers. Disabled Project Areas, standalone
CNRFC Basins, Major Conveyance, and Deltamapr Conveyance were inspected only
far enough to confirm that they are not current Local overlays; they are not
inventory rows. The Ops Live Major Water Supply Basin product and all External
layers are outside this task.

## Active code ownership

The active generic fractional-retention helper is
`03_functions/spatial_helpers.r::simplify_sf_for_web()`. Its current callers and
settings are owned by `05_map_build/02_build_core_map_cache.r`, the sourced
`02_cache_blocks/01_prepare_core_polygons.r`,
`02_cache_blocks/04_cache_admin_water_reference_layers.r`, and
`02_cache_blocks/05_cache_final_point_tweaks.r`. The Reference per-layer
fractional settings originate in `00_config/reference_layers_manifest.csv`.

Focused fixed-tolerance or accepted-candidate ownership is:

- National Monuments and NPS context:
  `02_preprocess/70_national_monuments_pipeline/`;
- California Desert NCL: `02_preprocess/71_desert_ncl_pipeline/`;
- ACECs: `02_preprocess/69_acec_pipeline/`; and
- Federal Wilderness: `02_preprocess/68_federal_wilderness_pipeline/`.

The public Local Reference disclosure field is registry-owned in
`00_config/config_local_reference_interactions.r` and passed through the shared
controller payload. Family cards for field offices, CNRFC product availability,
HUCs, Bulletin 118, and WSR use their existing owning controllers. No new card
or accordion was introduced.

## Audit state

Audit baseline: branch `feature/local-geometry-generalization-disclosure`,
starting commit `c42320cd7675467efb8aea5797f5b2af92b3cde4`, inspected
2026-08-11. The inventory records current active code and the already available
isolated-build artifacts; it does not promote historical QA output or rebuild
source products. There are no unresolved active generalization paths at this
baseline.
