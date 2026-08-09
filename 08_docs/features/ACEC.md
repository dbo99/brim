# California Areas of Critical Environmental Concern (ACECs)

## Purpose and authority

`Local → Reference → ACECs` is BRIM’s interactive reference layer for the 238
currently mapped, designated ACECs in the BLM California planning service. An
ACEC boundary is a planning-designation boundary, not an ownership or
cadastral boundary and not proof of public access or site-specific allowable
uses.

The geometry authority is a timestamped raw snapshot of the current BLM layer:

`https://gis.blm.gov/caarcgis/rest/services/Planning/BLM_CA_ACEC/FeatureServer/0`

The accepted snapshot is
`01_raw_data/acec_authoritative_snapshots/20260806T222915Z/features_epsg4326.raw.geojson`
in the external build workspace, with SHA-256
`0e2658c269476fa629fa7da83b93097e76655042a56bc1cc9d96e10fa6193d00`.

The browser never queries that service. Snapshot acquisition, derived-candidate
generation, processed-RDS promotion, focused cache refresh, realistic build,
and visual acceptance are separate gates. The historical shapefile supplied in
the 2026-08-05 enrichment package is a QA/reconciliation baseline only and is
never substituted for current geometry.

## Identity and geometry

- Component identity is the normalized current source `GlobalID`, stored as
  `component_id` with the `blmca-` prefix.
- Semantic identity is the reviewed package `acec_id`.
- Each current source row maps to exactly one semantic ACEC. Equal names are
  not an identity join: the two “Black Mountain” designations remain separate
  because their plans, decision dates, administrative units, and acreages
  differ.
- All 27 source attributes remain unchanged in the focused candidate. Additive
  identity and build metadata do not overwrite source fields.
- The current source has 238 records, 613 polygon parts, 82 multipart records,
  and 14 invalid ring geometries. Validity repair is applied only to the
  derived candidate and retains all records, IDs, and parts.
- The display candidate uses one-metre, topology-preserving simplification in
  EPSG:3310. Source and processed vertex/area/validity metrics are written for
  every record. The current snapshot contains 217,760 source vertices; the
  accepted display candidate contains 61,066 vertices.

The fail-closed builder and promotion contract live in
`02_preprocess/69_acec_pipeline/`. The generic reference-layer batch rejects a
focused ACEC request and skips ACEC during broad runs.

## Enrichment and runtime shape

Tracked normalized tables keep the following grains separate:

- 238 component rows;
- 238 semantic ACEC reference rows;
- 867 value records;
- 32 document records;
- 246 management-prescription records;
- 238 planning-history records;
- 476 relationship records;
- 238 office-assignment rows;
- 14 verified current BLM California field-office rows;
- 276 retained semantic ACEC-to-current-field-office spatial relationships;
- 238 access/land-status rows;
- 11 ACEC-name/WSA-inventory context rows; and
- seven source-register rows.

A separate derived display table contains 27 semantic ACEC-to-ACEC overlap
pairs. It is computed from the repaired, unsimplified current BLM snapshot,
not from the enrichment package or the one-metre display geometry.

One-to-many records are not joined onto geometry. Map-ready geometry retains
only stable keys plus draw, hover, label, search, and filter fields. The browser
receives normalized lookup collections and constructs popup HTML only after a
feature click.

The three names containing RNA/Research Natural Area wording are research
candidates only. RNA status has not been verified; it is disclosed as a
research flag in affected popups and is not a category or filter.

Eleven official ACEC names contain “WSA.” A tracked ACEC-owned comparison
against BRIM's current 63-record BLM California Wilderness Study Area inventory
classifies six as current WSAs and five as historical-name-only, with no
unresolved cases. The official ACEC name remains unchanged. The Overview tab
states either “Also a current BLM Wilderness Study Area.” or that the ACEC
retains “WSA” in its official name but is not a current Wilderness Study Area.
This clarification does not change the WSA layer, geometry, or authority.

## Search, counts, and filters

The primary map count is semantic ACECs. The compact summary uses:

`238/238 ACECs · 613/613 parts`

Named search covers official and source/legacy names, aliases, governing plan,
planning framework, source administrative unit, current spatial field-office
context, `acec_id`, `component_id`, and current `GlobalID`. Selection, chips,
bounds, and Auto-zoom use exact semantic IDs.

No status or type filter is shown: every current record is a designated ACEC.
The approved facets are:

1. Relevant and important value family (multi-valued; OR within the facet):
   fish/aquatic, wildlife/habitat, natural systems/processes,
   cultural/historic, scenic, and natural hazard/other.
2. Planning framework: DRECP (128), Central California Plan (53), Northwest
   California Integrated Plan (25), northern California other plan (19), and
   California Desert other plan (13).
3. Field office context: the 14 verified current California field offices.
   Checkbox values are alphabetized by current official office name, use OR
   semantics within the facet, and are derived from positive-area polygon
   intersection. A cross-office ACEC can match multiple values without
   duplicating or multiplying its semantic geometry.

`Source administrative unit` remains the ACEC FeatureServer's explicit source
coding and is not a user-facing field-office facet. It is preserved unchanged
in the runtime payload and popup even where the source supplies only a district.
Spatial context never overwrites or reinterprets that source field and is not
presented as management responsibility.

### Current field-office geometry authority

The context derivation reuses BRIM's existing Field Office source instead of
adding a second administrative-boundary pipeline:

- raw source: `01_raw_data/blm/fo_outer.shp`;
- preprocessor: `02_preprocess/09_field_office_outer.r`;
- unsimplified WGS84 analysis derivative:
  `04_processed_data/rds/field_office_outer_wgs84.rds`; and
- upstream layer: BLM CA Administrative Unit Boundary Field Office Polygon,
  layer 1 of the BLM California Administrative Unit FeatureServer.

The raw metadata records a 2025-06-23 BLM export, projected on 2025-06-27. Its
14 names, office codes, GlobalIDs, and geometry were reconciled to the live BLM
layer and to BLM's verified current California office roster on 2026-08-08.
The roster contains Barstow, Needles, Ridgecrest, and Central Coast; it does not
contain Hollister, Alturas, or Susanville. Although the upstream layer's generic
description mentions historic administrative boundaries, this snapshot and the
live layer query each contain only the 14 current California field offices.
The separate 20%-simplified field-office map-cache geometry is display-only and
is not used for spatial QA.

The derivation repairs current ACEC geometry, transforms both inputs to
EPSG:3310, computes exact intersection areas, and retains intersections strictly
greater than 100 square metres. This excludes one reviewed 53.260128-square-
metre contact artifact. An ACEC is classified as wholly within one office when
one retained office covers at least 99.5% of its area, as crossing boundaries
when multiple retained offices intersect, or as partial/no-match review when
coverage is incomplete. Results are 202 wholly within one office, 35 crossing
office boundaries, one partial spatial match requiring review, and zero without
a retained match.

The popup applies a separate, measured presentation rule without changing this
relationship table or the OR facet: it shows the largest office relationship
and any additional office covering at least 1% of mapped ACEC area. The full
relationship list remains in collapsed technical details. This keeps 254
relationships in the normal presentation and relocates 22 sub-1% relationships
across 20 ACECs; 15 ACECs remain visibly multi-office. The relocated values
range from 0.00045571% to 0.57030772% and 0.33 to 175.40 intersecting acres;
none reaches 640 acres. Thus Mohave Ground Squirrel is presented primarily as
Ridgecrest context while its 0.0345% Bakersfield and 0.0009% Bishop
intersections remain auditable in technical details. Substantial cross-office
relationships retain all office names and use rounded one-decimal percentages.

Source-versus-spatial QA finds 106 exact single-office agreements, six source
offices included in genuine multi-office contexts, and 126 district-only source
records contextualized spatially, with no meaningful source-office disagreement.
The retained relationship table stores office name/code, intersection area,
percentage of ACEC area, classification, threshold, source comparison class,
snapshot date, method, and confidence for every relationship.

Quick views select fish/aquatic, DRECP, wildlife/habitat, cultural/historic,
scenic, or natural-systems membership. The six value-family colors come from
one tracked registry and drive facet swatches, active quick-view feedback, and
applied map styling. With all value families selected, ACECs use the established
neutral terracotta style. Exactly one selected value family uses its registry
color for the fill and a derived darker outline; two or more selected value
families return to neutral so the map does not imply a false primary value.
DRECP alone is also neutral. The multi-valued facet retains OR semantics.

### Overlap display mode

The collapsed **Map / display** section immediately above the boundary note
contains **Distinguish overlaps**, off by default. This is a display mode only:
it never changes the applied filter, visible result set, semantic count, part
count, search/chip selection, or popup/hover content.

The tracked overlap graph uses repaired current BLM geometry in EPSG:3310
before display simplification. Candidate pairs must have an intersection area
strictly greater than one square metre, which excludes boundary/point touches
and numerical slivers at or below the threshold. The reviewed graph has 27
pairs, 39 participants, 13 connected components with sizes
`7, 6, 5, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2`, maximum degree four, and two
containments. The historical package produces the same 27 semantic pairs as a
QA comparison only. Running the criterion on the one-metre display geometry
would produce 57 pairs, including 35 display-only threshold artifacts, so
display geometry is deliberately not the graph authority.

The controller colors a semantic ACEC only while at least one of its reviewed
overlap partners is also in the current applied visible set. Nonparticipants
retain the current single-value thematic color or neutral ACEC style. A stable
semantic-ID graph coloring uses the six-color UIC-derived contrast palette;
the current graph has no adjacent color conflicts and an ACEC keeps the same
overlap color as filters change. Colors are an identity aid and carry no value,
plan, management, status, or acreage meaning; no identity legend is created.

Style precedence is Leaflet selection/highlight and hover, then overlap color,
then single-value thematic color, then neutral ACEC style. The controller
reapplies the current overlap/thematic base style after mouseout. Turning the
mode off restores the correct current thematic or neutral style. Reset, layer
off/on, Clear Local, and Clear All return the mode to off; no geometry,
controller, popup, tooltip, label, or listener is duplicated.

Defaults include all values. Facet groups are collapsible with explicit
disclosure markers and 24-pixel summary targets. Auto and Auto-zoom sit in the
header, the name search is 250 pixels wide within the standard 330-pixel card,
and the six quick-view buttons use a compact three-column, two-row layout.

## Hover, popup, and lifecycle

Hover contains only the official name, governing plan, concise public-facing
value-family cues, and approximate mapped area in square miles. IDs, raw codes,
source administrative coding, acreage, geometry QA, and office-intersection
percentages are excluded. Hover is suppressed for coarse-pointer devices.

Click popups use the shared responsive and keyboard-accessible three-tab shell:

1. **Overview** — designation, governing plan, the unchanged source
   administrative unit, separately labeled spatial field-office context,
   acreage, counties, boundary caveat, and RNA research disclosure when
   applicable. The context is labeled “derived spatially” and carries the
   explicit caveat that intersection with current boundaries does not by itself
   establish administrative responsibility. Current BLM source GIS acreage is
   presented once as the primary public value. A material difference shows a
   separately labeled BRIM calculated source-geometry acreage; otherwise that
   comparison is available only in collapsed technical details. Repetitive
   generated summaries and
   polygon-part QA are not displayed in the Overview. For all 11 official ACEC names containing
   “WSA,” this tab also shows the current-versus-historical WSA-name
   clarification.
2. **Values & context** — a compact list of relevant and important values, one
   shared BLM source statement/link, public-facing water-resource wording, and
   current field-office context. Raw Boolean markers and internal value-type or
   relationship codes are retained in structured data but omitted from normal
   user-facing statements.
3. **Management & sources** — management and access summaries, normalized
   prescriptions, deduplicated official planning/source links, plus collapsed
   IDs, raw relationship codes, access/source fields, geometry provenance,
   both acreage measures, and complete field-office intersection detail.

The acreage presentation uses `current_gis_acres` from the BLM source as the
primary public value. The BRIM-calculated source-geometry area normally appears
only in collapsed technical details. A difference is promoted to Overview only
when it is at least 10 acres and 0.5% of the source acreage; the current dataset
has one such record, Massacre Rim.

Management text is qualified: travel, grazing, minerals, recreation, access,
closures, rights-of-way, development, exceptions, and similar direction are
plan-specific and may vary within an ACEC.

The layer uses the existing Local Reference group/controller. Filtering
removes and re-adds owned Leaflet paths; it creates no parallel geometry.
Layer-off, Clear Local, and Clear All close owned presentations, clear pending
state, remove the card and paths, and restore defaults. Repeated off/on cycles
must remain duplicate-free.

Focused source QA lives in `qa/test_local_reference_acec.R`,
`qa/qa_local_reference_acec_overlap.R`,
`qa/qa_local_reference_acec_field_office_context.R`,
`qa/test_local_reference_filter_engine.js`, and the shared controller tests.
The cache refresh in `05_map_build/09_refresh_local_reference_acec_cache.r`
hashes every shared-cache child and fails if anything other than `acec` changes.
Human visual acceptance of an isolated realistic HTML remains required before
any Git checkpoint or production release.

## Tracking, regeneration, and accepted build evidence

Git tracks the focused builders, normalized ACEC tables, compact derived
relationship inventories, runtime configuration/helpers/controllers, focused
QA, and this documentation. It does not track the raw FeatureServer response,
the enrichment ZIP, processed RDS, shared caches, realistic HTML, screenshots,
or browser logs. Those remain external products under the repository contracts
in `DATA.md` and `BUILD.md`.

The accepted implementation is reproducible by acquiring a new explicit raw
FeatureServer snapshot, running the fail-closed focused builder and QA in this
directory, promoting the reviewed candidate in the isolated build workspace,
refreshing only the ACEC cache child, and then running the final-map-only build.
Each new snapshot or build is a new candidate and requires the same hash,
sibling-cache, browser, and human-acceptance gates.

Human visual acceptance was completed on 2026-08-08 for these external
products:

- processed ACEC RDS, 610,924 bytes, SHA-256
  `89e4f6a185a0aed9f52b2dd103180f7d43685d4c18817015acad1939933f77e7`;
- final Local Reference cache, 20,800,778 bytes, SHA-256
  `4ce07b11b4e10efedaa362a60b527e333466b066168bdc51356edb8c3c9a0a86`;
- unchanged label product, 380,278 bytes, SHA-256
  `50e9233ecda2f073ceea8f572fcfc5cd9c981263a295bb5909cb60417576e43c`;
  and
- `06_output/html/PortaTreasure2_core_20260808_164823.html`, 205,103,456
  bytes, SHA-256
  `eaf1cdf56ca5bdfb2bfae25807096ecb9821e5c1a9b6cdbb76bc63cc5c1140ff`.

The accepted browser pass covered contrasting source, WSA-name, aquatic-value,
planning-document, district-only, multi-office, material-acreage-difference,
and overlap-display cases; all popup tabs; hover; labels; search; Quick views;
facets; thematic and overlap styling; staged and automatic filters; docking;
Clear Local; Clear All; narrow viewport behavior; and console output. Layer
teardown and shared-layer regression tests also passed. Production was not
modified.

## Shared follow-ups

The following work is intentionally outside this ACEC branch:

1. ACEC now registers with the shared Local Reference semantic-label system;
   `lbl` follows the applied ACEC semantic result set without reacting to
   staged filters or Distinguish overlaps.
2. A BRIM-wide geometry-generalization/thinning disclosure framework remains a
   separate shared architecture task.
3. Authoritative External companion boundary layers remain a later shared
   integration task.
