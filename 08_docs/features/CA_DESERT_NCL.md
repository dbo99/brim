# California Desert National Conservation Lands

## Scope and semantic model

This feature owns `Local → Reference → CA Desert National Conservation Lands`
and source nickname `cadesert_ncl`. It does not own DRECP, ACEC, National
Monuments, Federal Wilderness, Wilderness Study Areas, Trails, Wild and Scenic
Rivers, External, or production release operations.

The semantic key is the authoritative `NLCS_ID`; the source `GlobalID` is
geometry lineage, and `OBJECTID` is diagnostic only. The accepted universe is
`NLCS002009` through `NLCS002019`. Ten records are DRECP ecoregion subarea
allocations, not independently established legal conservation units.
`NLCS002012`, source name `Desert Lily Preserve`, is retained as its own BLM
source-layer record. It is not renamed or merged into the current Desert Lily
ACEC or the statutory Desert Lily Sanctuary.

## Source and geometry

The focused pipeline is `02_preprocess/71_desert_ncl_pipeline`. Acquisition
reads the BLM FeatureServer directly, preserves untouched timestamped response
bodies with request/response metadata and hashes, and fails if the 11-ID
universe changes. Browsers never query the FeatureServer.

Raw source geometry remains unsimplified in the immutable external snapshot:
173 parts, 25 holes, and 84,148 vertices. Two source records have ring
self-intersections. The reviewed unsimplified validity-repaired derivative has
173 parts, 32 holes, and 84,155 vertices; the validity repair changes no unit's
area by as much as 0.000001 acre. The reviewed display derivative is generated
in EPSG:3310 with a topology-preserving two-metre tolerance. Its gate requires
173 parts, 32 holes, zero invalid/empty records, exact part/hole retention, and
no more than 0.01 percent absolute area change for any unit. The selected
candidate has 32,168 display vertices.

## Derived context

Current field-office context is a positive-area EPSG:3310 intersection against
BRIM's accepted unsimplified `field_office_outer_wgs84.rds`. Every positive-area
relationship remains in the technical sidecar. The user-facing facet includes
offices covering at least one percent of a mapped unit (or the largest match if
none reaches that threshold). An office intersection is context, not a
management assignment.

Relationships to current accepted National Monuments, Federal Wilderness,
WSA, ACEC, Trails, and WSR data are stored as normalized keyed rows. Related
features retain identity and geometry in their owning layers. Overlap never
transfers authority or merges legal/program identities.

## Runtime and UI

The layer uses the shared Local Reference engine: 11-unit selector/search,
visible two-column BLM Field Office context and Related designation overlap
facets, Apply/Reset, Auto and Auto-zoom, result counts, chips, semantic labels,
and idempotent layer teardown. The mapped-unit-type distinction remains in
structured data and popups, while the card summarizes it noninteractively as
ten DRECP subareas plus Desert Lily Preserve. The related-designation facet
normalizes the existing display-suitable relationship rows into ACEC, Federal
Wilderness, National Monument, WSA, Scenic/Historic Trail, and Wild & Scenic
River screening values. It does not transfer a sibling designation or render
sibling geometry. There are no Quick views. Default cartography is a neutral
desert-gold program-context style. Optional deterministic unit colors use the
public label `Distinguish mapped units`, remain unchecked initially, and carry
no category meaning.

The three-tab popup is `Overview`, `Conservation & planning`, and `Related
designations & sources`. Related-designation categories use the same compact,
collapsed presentation and include only relationships that pass the reviewed
display threshold; the complete normalized relationship table remains in the
technical sidecar. The popup separates calculated source-geometry area from
published planning-document acreage, exposes GlobalID lineage and diagnostic
OBJECTID only in technical details, and carries explicit access, land-status,
boundary, office-context, relationship, and Desert Lily identity caveats.

The focused cache writer is
`05_map_build/12_refresh_local_reference_desert_ncl_cache.r`. It owns only the
`cadesert_ncl` children of `reference_layers_all_map.rds` and
`labels_all_map.rds`, hashes every sibling before and after replacement, and
must be run twice with identical final hashes before a realistic build.

## Acceptance state

Human visual acceptance passed on 2026-08-10 in the isolated `codex_ship`
workspace. The accepted HTML is
`PortaTreasure2_core_20260810_214827.html`: 203,518,870 bytes, SHA-256
`b732119b9e519915cb9957fdcbe78e451126e3ead89101715197bfdac0ca6739`.
The accepted Local Reference and label aggregate SHA-256 values are
`f4f362a94d1a64ff052015ce2d261b3bb5e3d3fee58500adda098e1e9459c1cb`
and `015943ace234f9f21f6d29b3a016e2aa5dd5aacedb2f6f82019baa9bbd7506d4`.
Production remains protected by the normal post-merge backup, manifest,
rollback-script, exact-artifact, post-copy hash, and smoke-test gates.
