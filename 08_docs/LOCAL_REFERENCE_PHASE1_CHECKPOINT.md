# Local Reference Layer Enrichment — Phase 1 checkpoint

Status: Phase 1 shared-framework and WSA checkpoint accepted. The isolated
WSA preprocessor/cache path and realistic HTML build were exercised outside
production, focused QA passed, and manual visual acceptance was recorded for
`PortaTreasure2_core_20260804_222116.html`. Production was not modified.

## Boundary and execution status

The registry contains exactly the 11 approved user-facing rows beneath Local >
Reference, in display order. Wilderness Study Areas is the only executable
Phase 1 interaction. The other ten entries are contracts only; no Trails or
other layer UI is enabled.

The confirmed enrichment tiers remain:

- Rich: Trails, National Monuments, CA Desert NCL, Wilderness Study Areas,
  Federal Wilderness, and ACECs.
- Moderate: DRECP and Grazing Allotments.
- Basic: Counties, RWQCB Regions, and Water Districts.

## Shared schemas

The layer registry fields are:

`layer_id`, `source_nickname`, `display_name`, `implementation_status`,
`enrichment_depth`, `color_basis`, `color_source_field`, `palette_key`,
`category_definition`, `unknown_style`, `shared_management_style`,
`legend_mode`, `filter_mode`, `auto_supported`, `auto_default`, `count_mode`,
`primary_count_mode`, `primary_count_label`, `show_component_count`,
`component_count_label`, `category_heading`, `feature_selection_supported`,
`feature_selection_mode`, `feature_search_fields`, `feature_display_field`,
`auto_zoom_supported`, `auto_zoom_default`, `zoom_padding`, `zoom_max`,
`preserve_view_on_reset`, `retention_enabled`, `search_fields`, and
`category_sort_order`.

Each category definition has one authoritative row for map, legend, filter,
order, count, and missing-value behavior:

`category_key`, `label`, `source_values`, `fill_color`, `stroke_color`,
`fill_opacity`, `stroke_weight`, `dash_array`, `legend_swatch_style`,
`sort_order`, `include_when_absent`, and `provisional`.

All colors are explicitly provisional pending realistic review on BRIM
basemaps. The agency palette is reusable only for layers whose approved
`color_basis` is verified managing agency; it is not a default palette.

The management-role schema contains:

`layer_id`, `semantic_feature_key`, `geometry_key`, `designation_authority`,
`administering_agency`, `local_managing_agency`, `co_managing_agencies`,
`blm_role`, `blm_role_summary`, `blm_office`, `blm_office_url`,
`management_role_source`, `management_role_source_url`,
`management_role_verified_on`, `management_role_confidence`,
`provenance_class`, and `limitations`.

The controlled `blm_role` values are `sole_manager`, `co_manager`,
`administering_partner`, `local_land_manager`,
`planning_authority_on_blm_lands`, `program_administrator`,
`data_steward_only`, `no_identified_management_role`, and `unknown`.

## Provisional WSA color tokens

| Category | Fill | Stroke | Fill opacity | Stroke weight | Dash | Legend swatch |
|---|---:|---:|---:|---:|---:|---|
| Recommended suitable | `#6EA990` | `#287461` | 0.22 | 1.5 | solid | polygon |
| Recommended non-suitable | `#D58A70` | `#A14E38` | 0.20 | 1.5 | solid | polygon |
| No recommendation | `#88A8BA` | `#4C708A` | 0.20 | 1.5 | solid | polygon |
| Not stated | `#B0B0B0` | `#6B6B6B` | 0.14 | 1.4 | `2,3` | dotted polygon |

The complete provisional token matrix, including the fixed six trail identity
colors with no initial line-pattern differences, is in
`qa/artifacts/local_reference_phase1_checkpoint/local_reference_category_tokens.csv`.

## WSA deterministic reconciliation

- Current source: 63 semantic Wilderness Study Areas represented by 104
  polygon components.
- Stable semantic keys: 61 `wsa:nlcs_id:<NLCS_ID>` keys and two normalized
  `wsa:globalid:<GlobalID>` fallbacks (Red Mountain and Trinity Alps Subunit 4).
- Source joins: 59 exact normalized aliases, two explicitly unmatched/manual
  review, and two source-only. There are no duplicates or unexpected matches.
- Normalization is applied only to the reviewed alias allowlist; it is never a
  stable feature key, and there is no fuzzy-match acceptance path.
- Seed coverage: 59 exact, two explicitly unmatched/manual review, and two
  seed-only.
- Manual review remains: San Benito WSA ↔ San Benito Mountain ISA; Trinity
  Alps (Subunit 4) ↔ Trinity Alps Subunit. Neither crosswalk supplies popup
  enrichment.
- Source-only remains: Wall Canyon and Sheldon Contiguous.
- Seed-only/non-mapped remains: Brushy Mountain / English Ridge Subunit and
  Carson Iceberg.
- Recommendation categories: Recommended suitable 4 records / 21 components;
  Recommended non-suitable 46 / 60; No recommendation 11 / 21;
  Not stated 2 / 2. Raw `WSA_RCMND` values remain unchanged.
- Raw `GIS_ACRES` values also remain unchanged throughout source, processed
  data, cache QA, provenance, and popup technical details.
- Red Mountain retains authoritative raw `GIS_ACRES = 0`. Its 317.9-acre
  geometry calculation appears only as a clearly labeled source anomaly.
- Feature-level managing-office/direct-page coverage remains eight records.
  The BLM local-land-manager role is supported at the WSA layer-family level;
  missing feature-office evidence is not guessed.

## Implemented WSA interaction

Hover is a precomputed two- or three-row standard Leaflet tooltip: source name;
one compact FLPMA/recommendation/approximate-area row; and an optional verified
BLM-office row. Positive source `GIS_ACRES` values are converted for hover only
using `GIS_ACRES / 640`, formatted to approximately three significant digits,
and displayed as compact square miles such as `~74.2 mi²`. Zero, null, blank,
and nonnumeric values are omitted from hover. Directly verified office names
use explicit hover-only abbreviations while the popup retains their full
official names. Responsive WSA-only tooltip CSS bounds width, wraps at normal
word boundaries, and suppresses hover on coarse pointers. No hover event
performs geometry work, joins, or network requests.

The focused preprocessor path retains source and joined structured fields but
does not persist rendered popup/hover strings. Those display strings are built
from the same shared definitions in the map-cache stage.

The escaped structured popup contains, in order:

1. designation and recommendation, native source GIS acreage in acres, and exact-join
   reference fields;
2. management and planning, including BLM's verified layer-family role,
   source attributes, and feature office only where verified;
3. official curated research notes only where present;
4. the recommendation/designation caution; and
5. a collapsed source-and-technical disclosure with stable keys, dates,
   geometry-component count, join status, and official source links.

All hover information is duplicated in the popup. Opening a WSA popup closes
its tooltip, giving coarse-pointer/touch users a stable detail path. The popup
retains authoritative native acreage, including Red Mountain's source-zero
value; the hover conversion never replaces or renames `GIS_ACRES`.

## Generic controller behavior

The generic engine operates on synthetic or registered records and is not
WSA-specific. WSA supplies its category definition, records, semantic-feature
catalog, and semantic bounds through the registry adapter. Named-feature
selection and zoom contracts remain disabled for the other ten layers in this
checkpoint.

- Recommendation checkboxes remain the authoritative category filter.
- The accessible autocomplete searches approved name/code/case/ID fields.
  Typing alone does not alter map visibility. Selecting results creates safe,
  removable, duplicate-protected chips and automatically enables each
  selected feature's recommendation category.
- With one or more chips, the applied result is the selected semantic features
  subject to applied category membership. Removing the final chip returns to
  the category-filter view.
- Auto is supported and defaults on for WSA. With Auto off, edits remain
  staged until Apply.
- Auto-zoom is a separate control and defaults on for WSA. It uses the combined
  bounds of complete semantic features, including all multipart components,
  with layer-specific padding and maximum zoom. Typing, no matches, None,
  Reset, All, hover, popup, pan, and zoom do not cause unwanted map movement.
- `Zoom to results` fits the applied visible feature set independently of the
  Auto-zoom setting and is disabled when no results are visible.
- All, None, Apply, and Reset are implemented.
- The visible card uses semantic WSA counts: plain category totals and
  `Showing N of 63 Wilderness Study Areas`. Record, semantic-feature, and
  geometry-component counts remain separately available to controller QA.
- Internal counts separately report total category records, currently showing
  after the applied filter/search, distinct semantic features, and geometry
  components. They are data-derived and do not react to pan or zoom.
- Turning the layer off resets filters and restores all child layers for the
  next activation. Clear Local and Clear All use the same checkbox-driven
  `overlayremove` lifecycle.
- The card uses BRIM's shared close/detach helper and destroys listeners on map
  unload. Coarse-pointer controls receive larger targets.

## QA artifacts and tests

Tracked checkpoint artifacts are under
`qa/artifacts/local_reference_phase1_checkpoint/`:

- exact 11-layer registry contract;
- complete category/style-token matrix;
- management-role schema and per-layer evidence/display recommendations;
- 63-row source join QA;
- 63-row seed coverage QA;
- WSA category/semantic/component counts; and
- payload-size estimate.

Focused tests:

- `Rscript qa/test_local_reference_phase1.R`
- `Rscript qa/test_local_reference_wsa_hover_layout.R`
- actual source/processed/cache integrity through
  `Rscript qa/test_local_reference_wsa_not_stated.R` with the three explicit
  isolated WSA input paths;
- `node qa/test_local_reference_filter_engine.js`
- `node qa/test_local_reference_controller.js`
- parse checks for every changed R file and syntax checks for both JavaScript
  files.

The tests cover exact scope and Auto contracts, field aliases, 61/2 stable-key
coverage, 59/2/2 joins on both sides, four recommendation categories, 104
components, one-definition styling, manual/source/seed-only protections, Red
Mountain anomaly wording, escaped popup content, payload construction,
mounted hover/popup rendering, native and converted area formatting,
autocomplete keyboard navigation, safe and accessible selection chips,
duplicate prevention, staged/applied state, semantic and multipart bounds,
Auto-zoom and explicit zoom behavior, synthetic search/filter/count semantics,
layer-off/Clear lifecycle, touch tooltip closure, map-view stability, and
listener cleanup.

## Payload estimate

The checkpoint's uncompressed interaction material is approximately 257,959
bytes: 164,791 popup HTML, 5,686 hover text, 42,549 controller JSON, and 44,933
shared controller/filter JavaScript. The focused retained attribute data
occupies about 29,224 bytes as an in-memory R object. This is a conservative
raw-size estimate and does not subtract generic WSA popup strings already
present in the existing map. Popups average 2,616 bytes; the largest current
popup is 3,341 bytes (Buffalo Hills Wilderness Study Area).

## Accepted realistic build

Manual visual acceptance passed for:

- Build: `PortaTreasure2_core_20260804_222116.html`
- Size: 205,977,672 bytes
- SHA-256:
  `fa5ae5a3ea001a44dfaceb8a2692e2c7b48d11b656e73dfd057a07b41de60d72`
- Accepted behavior: concise responsive hover, richer structured popup,
  recommendation legend and semantic counts, accessible search chips,
  category/feature filtering, independent Auto-zoom, explicit
  `Zoom to results`, narrow-card wrapping, touch behavior, and lifecycle
  teardown.
- Accepted data totals: 63 semantic WSAs, 104 mapped polygon components, and
  recommendation totals 4 / 46 / 11 / 2.
- Production remained untouched. The realistic HTML and isolated processed,
  cache, and QA outputs remain outside this source-repository checkpoint.

## Remaining research caveat and deferred work

Individual historical recommendation validation may still be supplemented as
additional authoritative evidence is reviewed. Any such supplemental research
must remain separate from, and must not modify, the preserved raw
`WSA_RCMND` source values. The two proposed manual crosswalks also remain
explicitly unmatched/manual-review.

Trails UI and all other layer implementations remain deferred.
