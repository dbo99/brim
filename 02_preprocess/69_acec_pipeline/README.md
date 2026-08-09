# California ACEC focused pipeline

This directory owns the fail-closed California ACEC geometry candidate. The
generic reference-layer batch intentionally cannot build or overwrite ACEC.

Inputs are explicit:

- a timestamped, unmodified GeoJSON response from the current BLM California
  ACEC FeatureServer layer (all fields, all geometry, EPSG:4326);
- the reviewed `BRIM_CA_ACEC_Enrichment_2026-08-05_FINAL.zip`, whose historical
  shapefile is used only as a reconciliation baseline; and
- the tracked normalized ACEC lookup tables in `00_config/`.

`build_acec_current_candidate()` validates the package hash, exact source
schema, 238 unique GlobalIDs, lookup coverage, historical baseline counts, and
geometry invariants. It repairs the 14 invalid source polygons and applies a
reviewed one-metre topology-preserving simplification in EPSG:3310. The
candidate keeps all source attributes and all 613 polygon parts. It writes a
candidate RDS plus per-feature geometry QA, source reconciliation QA, and a
JSON run summary. It does not overwrite the current processed layer.

`build_acec_overlap_pairs()` is a narrower, offline derivation from the same
explicit current raw snapshot. It assigns the reviewed semantic `acec_id`
crosswalk, repairs geometry in EPSG:3310, performs no simplification, excludes
touches and intersections of one square metre or less, and writes only a
compact semantic pair table. The derived pair table is a browser styling
input; it is not geometry, a filter, or a management relationship. The
function refuses to overwrite an existing output and never changes the raw
snapshot, current processed RDS, shared cache, or any sibling layer artifact.

`build_acec_field_office_context()` is a separate offline enrichment step. It
reuses BRIM's existing unsimplified `field_office_outer_wgs84.rds` derivative,
whose 14 codes and GlobalIDs reconcile exactly to the verified current BLM
California office roster and the source `fo_outer` export dated 2025-06-23.
It intersects repaired ACEC and field-office derivatives in EPSG:3310, retains
areas strictly greater than 100 square metres, and writes normalized
many-to-many relationships plus a per-ACEC QA table. It never changes or
reinterprets the ACEC source administrative-unit field. Hollister, Alturas,
and Susanville are not current-office values and are rejected from the runtime
lookup. The 20%-simplified field-office map-cache geometry is not an analysis
input.

Promotion is a separate `promote_acec_candidate()` call. That call requires an
explicit candidate, final path, and unused archive path; it verifies hashes
after both archive and promotion. Promotion is appropriate only inside the
isolated `BRIM_v0.38_codex_ship` workspace after candidate QA has passed.

No function in this file contacts the network. Snapshot acquisition is a
separate, explicitly reviewed gate.
