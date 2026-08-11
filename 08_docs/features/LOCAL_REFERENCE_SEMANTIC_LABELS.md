# Local Reference semantic labels

## Purpose

The shared Local Reference label architecture keeps map geometry and labels on
one applied visibility truth. The Local Reference filter engine determines the
currently rendered semantic and geometry-component IDs; the label renderer
consumes that snapshot. It does not evaluate categories, facets, Quick views,
or search selections independently.

The initial registrations are:

| Layer | Semantic labels | Cached anchors | Anchor strategy |
|---|---:|---:|---|
| ACECs | 238 | 238 | point on surface of each semantic ACEC |
| Federal Wilderness | 158 | 197 | point on surface per source component; choose the largest currently visible component |
| Wilderness Study Areas | 63 | 63 | point on surface of each semantic WSA geometry |
| National Scenic/Historic Trails | 6 | 6 | midpoint of the longest line component for each semantic trail |
| National Monuments | 20 | 22 | point on surface per reviewed visible geometry; choose an anchor from currently visible geometry |
| California Desert NCL | 11 | 11 | point on surface of each semantic mapped unit |

Federal Wilderness deliberately caches more anchors than labels. A shared
named wilderness still receives exactly one label. When an agency filter hides
the preferred component, the controller chooses the next ranked anchor whose
`geometry_key` remains in the applied visible geometry set. It never falls
back to an anchor in hidden geometry.

## Registration contract

`LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY` in `00_config/config_labels.r` is the
single opt-in registry. A future layer supplies:

- controller `layer_id` and reference-cache `source_nickname`;
- label-cache `label_id`;
- semantic ID, geometry/component ID, and public label-text fields;
- an approved anchor strategy;
- whether anchor choice must be visible-component-aware; and
- whether the compact `LBL` control is available.

The current strategies are semantic polygon point-on-surface, visible-component
polygon point-on-surface, and semantic longest-line-component midpoint. A new
strategy belongs in the shared label helper and must retain the same output
schema; it must not introduce a layer-specific filter engine.

National Monuments uses this registration path. Its 20 semantic monuments
have 22 geometry-aware anchors because Sand to Snow and Tule Lake each have two
reviewed visible agency records; the controller still renders one canonical
label. California Desert NCL uses one anchor for each of its 11 stable
`NLCS_ID` mapped units; labels follow applied named-unit, office-context, and
related-designation filters and never represent the umbrella program as an
additional feature.

## Cache contract

`05_map_build/05_build_label_cache.r` is the canonical aggregate generator.
It reads the accepted map-ready caches listed in that script, generates
deterministically ordered point children, validates the registered semantic
counts and anchor-to-source distances, and writes the timestamped/latest
aggregate plus a child inventory containing source-cache hashes and serialized
child hashes.

Point-on-surface calculations operate on geometry-only `sf` objects. This
makes the constant-attribute assumption explicit instead of suppressing the
`st_point_on_surface assumes attributes are constant over geometries` warning.
Stable anchors are computed during cache generation; no polygon or line
analysis occurs in the browser.

The retired `major_conveyance` child is omitted and recorded as
`retired_omitted` in aggregate QA. Every other unrelated child is regenerated
from its currently accepted map-ready cache with deterministic row ordering.
Changing one Local Reference source child later should use its focused refresh
path and must prove all sibling hashes unchanged.

## Accepted canonical checkpoint

Human visual acceptance was completed on 2026-08-09 against the isolated
realistic build. Production remained untouched. The accepted aggregate-label
lineage is:

- prior production aggregate: SHA-256
  `682136e4a3c1dca3d7a8175b19faca7331b8fc0c4f7da95fa1e1a7c4bd1c6c96`;
- prior isolated aggregate: SHA-256
  `50e9233ecda2f073ceea8f572fcfc5cd9c981263a295bb5909cb60417576e43c`;
- accepted canonical aggregate: 437,433 bytes, SHA-256
  `a3fc3935b60a83e2cf35ba2c6c15d4fc50cba6bb3b35f94175a45ea0701fc9be`.

Two focused executions of `05_map_build/05_build_label_cache.r` produced
byte-identical canonical aggregates. The accepted Local Reference geometry
cache remained SHA-256
`4ce07b11b4e10efedaa362a60b527e333466b066168bdc51356edb8c3c9a0a86`.

The canonical aggregate has this exact child order and provenance:

| Child | Rows | Accepted input/provenance |
|---|---:|---|
| `huc2` | 4 | `huc_all_map.rds` |
| `huc4` | 16 | `huc_all_map.rds` |
| `huc6` | 24 | `huc_all_map.rds` |
| `huc8` | 140 | `huc_all_map.rds` |
| `huc10` | 1,128 | `huc_all_map.rds` |
| `huc12` | 5,065 | `huc_all_map.rds` |
| `gw_bull118` | 515 | `gw_bull118_map.rds` |
| `county` | 58 | `county_map.rds` |
| `project_areas` | 0 | accepted empty `project_areas_map.rds` |
| `cnrfc_basins` | 345 | `cnrfc_basins_map.rds` |
| `field_office_outer` | 14 | `field_office_outer_map.rds` |
| `acec` | 238 | registered semantic anchors from `reference_layers_all_map.rds` |
| `fedwilderness` | 197 | component-anchor candidates for 158 semantic wildernesses from `reference_layers_all_map.rds` |
| `wildernessstudyarea` | 63 | semantic anchors from `reference_layers_all_map.rds` |
| `trails` | 6 | semantic line anchors from `reference_layers_all_map.rds` |
| `water_districts` | 3,483 | `water_districts_map.rds` |
| `cnrfc_stream` | 2,047 | `cnrfc_stream_map.rds` |
| `cnrfc_precip` | 3,137 | `cnrfc_precip_map.rds` |

The stale 441-row `major_conveyance` child is deliberately absent. Protected
unrelated children retain their expected counts and production label-text
multisets while being regenerated from their current accepted inputs.

The visually accepted self-contained HTML is
`06_output/html/PortaTreasure2_core_20260809_012316.html`: 205,262,153 bytes,
SHA-256
`e5c622223d8ca198ecfcb9588259047d04840fe7102607dfe28b3d68ac299c04`.
Acceptance covered applied-result label membership, all four registered layer
families, visible-component Federal Wilderness anchors, compact WSA and Trails
`LBL` controls, lifecycle teardown, and standalone-HTML size.

The RDS cache, reconciliation CSVs, isolated QA outputs, rollback copy, and
realistic HTML are generated downstream evidence and are not tracked source.
Git tracks the generator, registration, controller, validation, and this
lineage record.

### National Monuments accepted extension

The current isolated National Monuments candidate adds the `monuments` label
child without changing any sibling. It contains 22 component-aware anchor rows
for 20 semantic monuments, with zero unresolved anchors and a 0.5-metre maximum
source-distance tolerance. Two focused executions were byte-identical:

- Local Reference aggregate SHA-256
  `f4b42087a87e6b9632ad88fbb582ead36ea229ecc18bdbf2404d5031dc2d0a64`;
- label aggregate SHA-256
  `439f60102df29138e2bbfeb46cf129984be2c566f5746a09e576bfad5e6778f0`.

Human visual acceptance passed on 2026-08-10 for the corrected realistic
National Monuments HTML. The accepted extension supersedes the earlier
candidate hashes above; production remains protected by the post-merge release
gates. The final release-gate rerun changed only aggregate list order so the
focused cache matches the canonical full builder; all child object hashes are
unchanged, and two focused executions produced the same aggregate hash.

The card-owned National Park/Preserve context deliberately has no label child.
Its optional legislative outlines and tract-derived land/interest fills are
context only and cannot add to, duplicate, or filter the 20 National Monument
semantic labels.

### California Desert NCL candidate extension

The focused Phase 6 path adds an 11-row `cadesert_ncl` semantic-label child.
Its anchors are built from the accepted two-metre display derivative, one per
stable `NLCS_ID`. The focused cache writer may add or replace only that child;
all previously accepted label-child object hashes must remain unchanged. The
accepted focused aggregates are:

- Local Reference aggregate: 19,573,762 bytes, SHA-256
  `f4f362a94d1a64ff052015ce2d261b3bb5e3d3fee58500adda098e1e9459c1cb`;
- label aggregate: 440,702 bytes, SHA-256
  `015943ace234f9f21f6d29b3a016e2aa5dd5aacedb2f6f82019baa9bbd7506d4`.

Human visual acceptance passed on 2026-08-10 against
`PortaTreasure2_core_20260810_214827.html`: 203,518,870 bytes, SHA-256
`b732119b9e519915cb9957fdcbe78e451126e3ead89101715197bfdac0ca6739`.
The normal public related-designation facet intentionally reports zero Federal
Wilderness units: all 93 positive-area technical rows are below the reviewed
display threshold and remain available only in the technical sidecar.

## Runtime and staged/applied behavior

Each registered cache child is added as one hidden Labels companion group with
individually addressable marker IDs. The controller keeps a compact lookup of
`label_record_key`, `semantic_feature_key`, optional `geometry_key`, and anchor
priority. It reconciles group membership only when applied geometry changes or
the map crosses the configured label zoom threshold.

- Staged checkbox, facet, Quick-view, or chip changes do not alter labels.
- Apply updates geometry and labels in the same render transaction.
- Auto mode applies both immediately.
- Styling-only modes do not affect label membership.
- Zero results leave `LBL` logically on but empty; restored results restore the
  matching labels.
- Reset restores the layer's normal applied filter and matching labels without
  changing `LBL`.
- Layer-off follows the accepted Local-panel convention: it turns the companion
  `LBL` preference off, removes all label members, and resets controller state.
- Layer-on repopulates default membership without turning `LBL` on.
- Clear Local and Clear All use the same overlay-removal lifecycle.

Repeated filtering, zooming, layer cycles, and `LBL` cycles must leave one
companion group, at most one marker per visible semantic feature, no unresolved
visible anchors, and no controller-owned listeners after map unload.
