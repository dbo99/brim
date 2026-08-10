# National Monuments

## Purpose and semantic scope

`Local → Reference → National Monuments` is BRIM's current multi-agency
National Monument reference layer. Its production semantic universe is the 20
current National Monuments wholly or partly in California. A monument crossing
the state line remains one semantic monument and retains its complete
authoritative geometry; Cascade–Siskiyou therefore includes its California and
Oregon extent.

The layer distinguishes semantic monument identity from visible geometry
records, authoritative source records, polygon parts, administering agencies,
and legal-status transactions. The focused repaired-source candidate contains
20 semantic monuments, 22 visible agency-geometry records, and 24,432 polygon
parts. Sand to Snow and Tule Lake are each represented by two visible agency
geometry records. Each still has one search result, one filter identity, one
popup identity, and one semantic label. The Tule Lake source repair is not a
cache promotion or UI acceptance checkpoint.

## Authority and acquisition

The canonical acquisition is R-based:

`02_preprocess/70_national_monuments_pipeline/acquire_authoritative_sources.R`

It acquires current BLM, USFS designated-area, USFS legal-status, and NPS
official-boundary services directly. It preserves immutable service/layer
metadata, count and object-ID discovery, exact request definitions, response
headers, untouched response bytes, native geometry, EPSG:4326 geometry, hashes,
and a fail-closed `FAILED`/`COMPLETE` gate. Count/ID checks, deterministic
batching, recursive incomplete-batch splitting, ArcGIS-error detection, and
exact requested-versus-returned ID validation are mandatory.

Tule Lake has a focused canonical R addendum:

`02_preprocess/70_national_monuments_pipeline/acquire_tule_lake_authoritative_source.R`

It reuses the same fail-closed acquisition implementation for the authoritative
USFWS `FWSSpecialDesignation` record. The exact target is
`DESNAME = 'Tule Lake National Monument' AND DESTYPE = 'NATIONAL MONUMENT'`,
`OBJECTID=135`, with source
`GlobalID={2E9B5F58-4AD7-4467-B80D-C6111AAB66C2}` and authoritative view
identifier `GlobalID_2=bd04754c-21fd-4e14-bcca-72d1d63a563f`.
The NPS `TULE` record remains authoritative for the Segregation Center agency
component; it is not a complete Tule Lake monument boundary.

The completed 2026-08-09 R snapshot contains 10 BLM current, 9 USFS current,
10 USFS legal-status, and 7 NPS official-boundary records. No transfer limit or
adaptive split occurred. An earlier Python snapshot is retained as independent
QA evidence only; Python is not called by the BRIM build. All eight corresponding
native/EPSG:4326 geometry responses have identical raw SHA-256 hashes across R
and Python, and the tiered parsed comparison reports zero maximum numeric and
coordinate difference.

Raw snapshots and derived RDS/GPKG/cache/HTML products remain external. The
tracked pipeline, endpoint/query configuration, reconciliation roles, curated
tables, QA contracts, and feature documentation are source authority. See the
pipeline README for exact checkpoint paths and hashes.

## Multi-agency geometry reconciliation

Agency publication is not treated as proof of administration or a distinct
display component. All 37 relevant source-role rows are accounted for as
selected current boundary, selected agency component, duplicate/provisional
representation, legal-status context, out-of-scope record, or other reviewed
context. No national out-of-scope unit is pulled into the display derivative.

The three important shared BLM–USFS decisions are:

- **Berryessa Snow Mountain / Molok Luyuk** — the BLM service supplies the
  complete current boundary, including the current expansion. Two Forest
  Service records are provisional constituent/agency areas and omit part of the
  current whole. BRIM renders the one complete BLM boundary with the balanced
  shared olive/yellow-green style; the USFS records remain provenance/QA context.
- **Sand to Snow** — the BLM whole and the two current USFS records reconcile
  to the same monument. The two USFS records are final non-overlapping
  administering-agency components (`AREAID=013` USFS and `AREAID=BLM` BLM).
  BRIM renders those two components and does not also render the BLM whole.
- **Santa Rosa and San Jacinto Mountains** — BLM and USFS publish substantially
  duplicate complete boundaries, not distinct administering components. BRIM
  renders the reconciled BLM whole once with the balanced shared style and
  retains the USFS whole as duplicate-source context.

No agency split is inferred for Berryessa or Santa Rosa/San Jacinto. No duplicate
whole-monument boundaries are stacked to create artificial transparency.

Tule Lake is one semantic National Monument with three truthful constituent
areas and two administering-agency source records:

- **Tule Lake Segregation Center** — the NPS `TULE` record
  (`OBJECTID=2145`, `GlobalID=c7a22b4d-edb4-4afd-8c16-e48df7dad7b3`),
  approximately 37.396 calculated acres.
- **Peninsula / Castle Rock** — part 1 of the USFWS Tule Lake National
  Monument record, approximately 1,277.039 calculated acres.
- **Camp Tulelake** — part 2 of that same USFWS record, approximately 66.708
  calculated acres.

The complete repaired semantic geometry is approximately 1,381.143 calculated
acres. The USFWS multipart feature is preserved as one source/display record;
its two authoritative parts are named only through the reviewed constituent
crosswalk.

## Geometry QA and display derivative

The selected raw geometry has 22 records, 24,446 polygon parts, 256,723
vertices, and six invalid projected geometries. Untouched source responses are
never modified. On the derived copy, `sf::st_make_valid()` produces 24,432
valid parts and 256,149 vertices with zero invalid or empty records. Large
repair-area differences are retained and reported rather than hidden; they are
primarily nested-shell interpretation corrections in Mojave Trails, Cascade–
Siskiyou, and Berryessa.

The display candidate applies topology-preserving simplification at 1 metre in
EPSG:3310. It retains all 24,432 repaired parts exactly and contains 175,540
vertices. Total display-area change is -18.075281 acres; the largest absolute
per-geometry change is 18.726030 acres (0.209323650%). The 0, 0.25, 0.5, and
1 metre benchmark table is retained with the candidate QA products. Source,
calculated, repaired, and display acreage remain distinct.

Two positive-area semantic overlaps remain after source reconciliation:
approximately 62.007099 acres between Lava Beds and Sáttítla Highlands, and
0.016567 acres between Cabrillo and California Coastal. They are independent
valid semantic monuments with already contrasting agency styles. A
`Distinguish overlaps` control is deliberately omitted because duplicate
source representations have been removed, Sand to Snow's components do not
overlap, and the remaining overlaps do not create a material statewide
interpretation problem.

## Styling, filtering, and labels

Agency color is assigned only when the visible geometry is a reviewed agency
component or a complete single-agency boundary. It is never assigned merely
from source publisher. The exact accepted Federal Wilderness colors are reused:

- BLM `#B8860B`
- USFS `#228B22`
- NPS `#54278F`
- USFWS `#1F78B4`

Single-agency/component geometry uses 0.16 fill opacity and a 1.8-pixel
agency-colored outline. Complete shared BLM–USFS boundaries use a dedicated
olive/yellow-green fill `#92962A`, mustard/dark-olive outline `#766717`, 0.13
fill opacity, 1.8-pixel weight, and `6,3` dashes. This balanced shared treatment
is derived from the BLM mustard and USFS green visual families without making
either agency primary. The shared token is centralized beside the agency
tokens. It is a geometry display role, not a fourth administering agency. The
card reads the shared swatch from that runtime token and explains that this is
one complete shared boundary for which no agency split was inferred.

The repaired source model requires semantic agency counts of 9 BLM-involved, 7
USFS-involved, 7 NPS-involved, and 1 USFWS-involved monument; shared monuments
match every verified administering agency. This makes Tule Lake the fourth
shared-management monument and changes the exact management-pattern contract
to 16 single-agency/4 shared. Other exact semantic contracts are 17
presidential/3 congressional original designations and
management patterns, and 4 recent 2024–2025 designations or major changes.
Quick views are BLM involved, Shared BLM–USFS, and Recent 2024–2025.

The accepted source repair adds USFWS as the fourth agency facet using the
canonical `#1F78B4` token. Runtime, card, label, and lifecycle contracts require
22 geometry records and 20 semantic monuments. The reviewed external candidate
may be promoted only in the isolated integration workspace for the focused
cache refresh and realistic browser/human acceptance gate.

The tracked `qa/qa_local_reference_national_monument_agency_styles.R` audit
joins all 20 semantic records to management evidence and every relevant source
geometry role. It requires exact agency/filter membership, selected display
roles, centralized colors, and one expected visible semantic label. Berryessa
and Santa Rosa/San Jacinto must be shared whole boundaries; Sand to Snow must
remain the reviewed BLM/USFS component pair. A non-NPS semantic record may
never receive the NPS style.

The shared Local Reference semantic-label renderer caches 22 component-aware
anchors but displays at most one canonical label for each of the 20 visible
semantic monuments. Filters and named selection use the controller's applied
semantic/geometry snapshot; agency-component styling cannot create duplicate
labels. The inline `lbl` preference is removed cleanly with layer teardown.

## Optional NPS Park / Preserve context

The collapsed `NPS context` section owns two independent default-off toggles:
9 National Parks and 1 National Preserve. The exact units are Channel Islands,
Death Valley, Joshua Tree, Kings Canyon, Lassen Volcanic, Pinnacles, Redwood,
Sequoia, Yosemite, and Mojave National Preserve. Death Valley retains its full
California–Nevada extent. No other NPS unit type is included.

The context uses two distinct official NPS Land Resources Division roles:

- a restrained dashed legislative/authorized boundary outline, explicitly not
  represented as ownership; and
- a very light tract-derived NPS land/interest fill that preserves authoritative
  non-NPS inholding holes and disconnected holdings.

The current 6,207 tract records classify to 4,079 `Federal Land (Fee)`, 12
`Federal Land (Less than Fee)`, 6 `Other Federal Land`, 237 `Public`, and 1,873
`Private` records. The public fill uses conservative precedence: fee is masked
by overlapping non-NPS status and explicit less-than-fee remains an interest,
not fee ownership. Mojave's private/state/public pattern therefore remains
visible rather than receiving a solid outer-boundary fill.

The map derivative has 10 outline and 10 land/interest records, 167 parts, 741
holes, and 46,433 vertices. Boundary and fill simplification are respectively
2 and 5 metres after classification/dissolve. Context styling reuses the NPS
token only at 0.065 fill opacity and restrained line weights in a subordinate
pane; National Monument geometry remains visually primary. Hover/popup wording
keeps legislative boundary and land/interest meaning separate. Context labels
are deliberately omitted.

## Hover, popup, card, and lifecycle

Hover identifies the National Monument and distinguishes a selected agency
component from a complete shared or single-agency boundary. It includes shared
administering agencies, designation, mapped approximate area, and the full
California–Oregon cue for Cascade–Siskiyou where relevant. `Selected component`
appears only for genuine agency components.

Popup Overview distinguishes semantic monument, visible geometry role,
administering agencies, selected component when applicable, source/public
acreage, and mapped-boundary area. Values/resources, management, history,
documents, and technical provenance remain normalized payload tables rather
than repeated strings on every polygon.

The compact docked card is 330 pixels wide and uses the page's existing scroll;
only the detached card owns a `calc(100vh - 8px)` vertical overflow surface.
Search is capped at 250 pixels. Auto
and Auto-zoom are in the header; facets and source/display notes use the
National Monuments disclosure treatment: 29-pixel left padding, a 7-pixel
inset, 18-by-20-pixel indicator target, 14-pixel `▸`/`▾`, hover feedback, and
a focus-visible outline. Docked and detached states use the shared card
lifecycle.

Layer-off, repeated off/on, Clear Local, and Clear All remove owned paths,
labels, popup/tooltip state, callbacks, and the single card. Reactivation adds
exactly 22 visible geometry layers and never clones them.

Context toggles do not alter monument counts, filters, Quick views, search, or
labels. Reset turns both context groups off. Parent layer-off removes both group
roots and any open owned context popup without clearing their prebuilt members;
reactivation starts them off. Close/reopen and detach/dock preserve one card and
do not duplicate geometry or listeners.

## Focused isolated acceptance checkpoint

The accepted source-repair candidate
`national_monuments_candidate/20260810T060500Z_tule_repair/reference_monuments_wgs84_current_candidate.rds`
has SHA-256
`853798e215de9e9f1f83fdfda43c8be9454e20ef14d02782859c07b6b66fdf2c`.
It was promoted only into the isolated integration workspace. Two consecutive
focused cache refreshes produced byte-identical Local Reference and label-cache
artifacts; the latest file hashes are respectively
`f4b42087a87e6b9632ad88fbb582ead36ea229ecc18bdbf2404d5031dc2d0a64`
and `439f60102df29138e2bbfeb46cf129984be2c566f5746a09e576bfad5e6778f0`.
The final label hash reflects canonical semantic-child list order; every child
object hash remained unchanged and two focused release-gate runs were
byte-identical.
Every unrelated cache child remained unchanged.

The current UI-refined realistic final-map-only candidate is
`06_output/html/PortaTreasure2_core_20260810_105524.html`, 205,006,831 bytes,
SHA-256 `09fb5b9567a90a88431f9b960de18becfdd4e7ca5639e1a3789144e94baf65a1`.
Mounted browser QA confirmed:

- a 330-by-491.3-pixel normal card with four visible primary facets, no
  redundant Map colors block, and only NPS context and Boundary/use
  disclosures;
- the four canonical agency swatches beside their facet rows, followed by one
  compact olive/dashed `Shared whole boundary: BLM–USFS` explanatory row;
- one `counts: matching / total` cue plus precise count-cell titles describing
  current matching results and total category membership;
- exact semantic facet counts of 9 BLM, 7 USFS, 7 NPS, 1 USFWS, 17
  proclamation, 3 congressional, 16 single-agency, 4 shared/multi-agency, 4
  recent, and 16 earlier/no-major-change;
- exact visible geometry style counts of 7 BLM mustard, 5 USFS green, 7 NPS
  purple, 1 USFWS blue, and 2 shared olive whole boundaries;
- a Shared BLM–USFS Quick view with exactly three semantic monuments, rendering
  two shared olive whole boundaries plus Sand to Snow's real BLM and USFS
  components, and excluding Tule Lake;
- truthful, independently clickable NPS and USFWS Tule Lake component popups;
- Berryessa's shared olive style in normal, hover, popup-open, mouseout, and
  restored states, with no NPS runtime agency/style assignment;
- exactly one canonical label per semantic monument, including Sand to Snow and
  Tule Lake;
- independent default-off 9-park and 1-preserve context toggles that do not
  change monument counts or filtering;
- compact NPS-context hovers containing only the bold unit name and rounded
  legislative-boundary area in `mi²`; both land-fill and boundary-outline
  geometry use the same intrinsic-width two-line presentation with normal
  Leaflet padding and a 320-pixel maximum-width safeguard;
- no normal-card internal or horizontal scroll; one intentional outer Local
  stack fallback where the whole stack exceeds its corner; one scroll owner in
  short and detached layouts; exact layer-off, repeated off/on, Clear Local,
  Clear All, Reset, popup, label, and context cleanup; and no browser warnings
  or errors.

Human visual acceptance passed on 2026-08-10 for this artifact. It remains the
accepted pre-merge evidence; production stays protected until the normal
post-merge build, backup, deployment, and smoke-test gates complete.

## Build and QA gates

The focused execution order is acquisition, tiered acquisition parity,
candidate build/geometry QA, reviewed isolated promotion, focused cache refresh,
deterministic cache rerun, source tests, final-map-only realistic build, mounted
browser QA, and human visual acceptance. Every gate is fail-closed. Production
is not modified during implementation or realistic testing.

Focused contracts live in:

- `qa/test_local_reference_phase5_national_monuments.R`
- `qa/qa_local_reference_national_monument_agency_styles.R`
- `qa/qa_tule_lake_national_monument_source_repair.R`
- `qa/test_local_reference_filter_engine.js`
- `qa/test_local_reference_controller.js`
- `qa/test_local_reference_semantic_labels.R`
- `qa/qa_local_reference_label_cache_reconciliation.R`
- `02_preprocess/70_national_monuments_pipeline/qa_reconcile_acquisitions.R`
- `02_preprocess/70_national_monuments_pipeline/build_nps_park_preserve_context.R`
- `05_map_build/11_refresh_local_reference_nps_context_cache.r`

The current corrected isolated realistic HTML passed human visual acceptance
on 2026-08-10. Stage, commit, release, or deploy it only through the separately
authorized normal BRIM release gates.
