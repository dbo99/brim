# Polygon generalization portfolio V3

## Scope and authority

BRIM uses high-fidelity/source geometry and fit-for-purpose browser display
geometry as separate products. The authoritative source-owned V3 controls are:

- `00_config/polygon_generalization_portfolio_v3.csv` — the complete 29-family
  decision registry: 24 reviewed geometry replacements and 5 retained displays;
- `00_config/polygon_generalization_source_row_crosswalk_v3.csv` — the explicit
  8,971-row source/candidate identity crosswalk; and
- `08_docs/features/local_geometry_generalization_inventory.csv` — the public
  layer, method, parameter, weight, disclosure, and implementation inventory.

The current RWQCB runtime baseline is unsimplified/clean-only (`keep=1.00`
means 100%, not 1%) with 9 regions and 212,739 vertices. The final RWQCB
decision is the reviewed shared-coverage 100 m artifact with
SHA-256
`b7c263024b9c6cd296b063a9386d589b8d89e4fb4cc937c0e32aea9a37e3850e`:
9 regions (RB 1–9), 32,031 vertices, and 1,178,655 browser-geometry bytes. The
accepted comparison records a 6,646,188-byte browser-geometry reduction. The
earlier 15% candidate is research history and is not a production input.

This portfolio does not generalize BLM-CA Managed or BLM held/managed
difference geometry. It retains the currently accepted display products for
BLM Field Offices, Counties, and Water Districts. It does not force a single
algorithm across independent polygon families.

The inventory's `geometry_bytes` and `browser_geometry_bytes` are measurements
of the accepted geometry-only artifact and its browser serialization, not total
enriched cache size. A study-prepared retained object can serialize a few bytes
differently without representing a geometry change. Water Districts illustrate
that distinction: the retained display still has 3,483 records and 387,387
vertices; the accepted repackaged geometry-only measurements are 4,953,617 RDS
bytes and 13,882,769 browser bytes, while the current-runtime comparison was
4,953,391 and 13,882,602. The 226/167-byte differences are serialization
effects, not a new Water District treatment.

## Reviewed external bundle

Large reviewed RDS artifacts remain outside Git under:

`04_processed_data/rds/reviewed_polygon_geometry/portfolio_v3/`

`EXTERNAL_DATA_MANIFEST.csv` registers the bundle, all 24 geometry-only
candidates, and the four reconstructed high-fidelity parents that are not
otherwise available at stable source-repository-relative paths. Bundle files
are copied only at the later reviewed-bundle gate; source implementation must
not regenerate them. In particular, BRIM does not productionize or invoke the
sandbox GEOS bridge.

Every required artifact is fail-closed. The implementation checks its path,
SHA-256, schema (`study_id` plus geometry only), ordered study IDs, CRS,
geometry type, validity, empties, row count, and vertex count before an output
can be saved. Every high-fidelity parent is independently hash-pinned. Missing
or changed bundle/parent inputs stop the build or refresh.

## Identity and attribute preservation

`03_functions/polygon_generalization_helpers.r` owns
`pt_apply_reviewed_polygon_geometry()`. The helper maps the prepared runtime
object to the reviewed candidate through the tracked row crosswalk, then
replaces only its `sf` geometry column. It proves that retained attributes and
row order remain byte-for-byte equivalent as R values.

Stable business identifiers are used where they are unique. Ambiguous families
use this fingerprint:

`SHA256(layer_id + pinned parent SHA-256 + normalized retained business tuple + canonical source-geometry WKB)`

The fingerprint is geometry identity, not semantic identity. The crosswalk
retains both. It also contains a pinned baseline-display fingerprint only for
the three legitimate writers that may supply an already prepared current
display object (GSP Areas, BLM WSR corridors, and Grazing Allotments). Bare row
position is never an identity contract, and fuzzy joins are not permitted.

This geometry-only overlay preserves popup, hover, fill, classification,
filter, selection, result-count, and label-key attributes already owned by
BRIM. CNRFC Product Availability therefore retains `cnrfc_id` and all six fill
fields. Its `labels_all_map.rds::cnrfc_basins` child is rebuilt from the exact
reviewed Product display geometry while retaining `Basin = cnrfc_id`, the
existing parent/label groups, and minimum zoom 7.

NPS Park/Preserve context is applied independently to the reviewed 0 m
post-mask parent. The helper does not simplify an already simplified display
object as a new parent. It retains the 9-park/1-preserve semantic and tract
classification contract and records the final 25 m reviewed artifact hashes in
the cache metadata.

## Cache-writer ownership and reversion prevention

The normal core map-cache builder applies the reviewed overlay immediately
before saving these controlled outputs:

- `gw_bull118_map.rds`;
- `cnrfc_basin_product_availability_map.rds`;
- `cnrfc_fnf_delta_map.rds`;
- `huc_all_map.rds` (HUC2–HUC12);
- `reference_layers_all_map.rds` (the 12 controlled children);
- `rwqcb_regions_map.rds`; and
- `nps_park_preserve_context_map.rds` through its focused owner.

The WSA, Federal Wilderness, ACEC, National Monuments, NPS context, and Desert
NCL focused refresh scripts call the same reviewed overlay after their existing
acquisition/enrichment behavior and before saving. They therefore cannot
silently restore an older/default geometry.

`05_map_build/13_refresh_polygon_generalization_portfolio_caches.r` is the
narrow release refresh. It reads only existing map-ready caches, applies the
reviewed bundle, rebuilds only the CNRFC Product label child, proves protected
shared-cache and label siblings are unchanged, stages and verifies every
result, preserves a rollback bundle, and atomically replaces the eight affected
latest cache files. It has no remote-service behavior.

## Public disclosure ownership

Public disclosure text is registry-owned and layer-specific. Every disclosed
note ends exactly:

> Check authoritative source for boundary-sensitive use.

The Local Reference controller receives its notes through the existing central
registry payload. Bulletin 118, HUC, CNRFC Product, BLM field-office, WSR, and
NPS context notes are injected into their existing cards. FNF, GSP Areas, and
Adjudicated Groundwater Basins use one small shared lifecycle-aware disclosure
control because they have no existing natural card owner. No blanket geometry
footer is used. BLM-CA Managed and held/managed differences receive no note, as
required by the accepted disclosure portfolio.

## Acceptance boundary

Source-only tests validate the registry/crosswalk, reviewed hashes and counts,
fail-closed helper behavior, attribute/row-order retention, cache-writer guards,
custom disclosures, CNRFC label lineage, and protected sibling logic. The
external bundle and caches are intentionally absent from the lean source repo,
so artifact-level geometry and browser acceptance occur only after a reviewed
bundle is copied into `codex_ship` at a later gate.

Realistic acceptance must exercise default and every relevant fill/display
mode, filters and selected polygons, popup/hover, labels, repeated on/off,
Clear Local, Clear All, duplicate card/listener absence, unrelated Local
regression, and External/Ops Live smoke where shared lifecycle code is touched.
The actual final HTML size must be compared. Sandbox combined HTML is weight
evidence, not browser acceptance.
