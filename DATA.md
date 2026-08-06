# BRIM data governance

## Data classes

1. **Tracked authored source** — code, registries, compact configuration, controlled tokens, reviewed crosswalks, small curated inputs, tests, and documentation.
2. **External raw inputs** — downloaded services, shapefiles, GDBs, rasters, archives, observations, and source snapshots too large or inappropriate for normal Git.
3. **External processed products** — standardized RDS/GPKG/GeoJSON products, analysis outputs, and map-ready geometries.
4. **Caches** — assembled map/cache objects derived from retained processed products.
5. **Generated review/release artifacts** — realistic/final HTML, QA maps, screenshots, reports, logs, and rollback packages.
6. **Research packages** — workbooks/ZIPs/source registers used as evidence and curated-input candidates; not production geometry or code authority.

`EXTERNAL_DATA_MANIFEST.csv` records excluded external products. Absence from Git does not make a dataset optional.

## Source authority

For each layer, explicitly identify:

- authoritative geometry/source object;
- semantic feature key;
- geometry/record/audit key;
- raw source attributes;
- curated enrichment source;
- source date/version/service;
- processing lineage;
- current map/cache child.

BRIM local source geometry and raw fields outrank research-package geometry or prebuilt popup content unless a reviewed migration explicitly replaces the source.

## Identity and joins

- Use durable semantic identifiers when available.
- Keep semantic identity separate from geometry components, service OBJECTIDs, row numbers, and GlobalIDs that may change on republication.
- Preserve raw source names; store standardized display names and aliases separately.
- Exact ID joins are preferred.
- Name normalization may produce reviewed candidates but must not silently become a production join.
- Fuzzy joins require an explicit allowlist/manual review and QA evidence.
- Report completeness, uniqueness, duplicates, unmatched source rows, unmatched enrichment rows, and fallback keys.

## Raw versus curated values

Never overwrite a raw source field with curated text.

Curated values must retain:

- semantic key;
- target field;
- raw/source value;
- proposed display value;
- evidence title/URL;
- verification date;
- provenance class;
- confidence/review status;
- limitation note.

Unknown, blank, not applicable, not found, and unresolved must remain distinguishable.

## Management and jurisdiction

Do not infer management authority from:

- agency publication of a GIS service;
- a generic `MNG_AGCY` field without context;
- spatial intersection alone;
- a partner association;
- broad program participation.

Separate designation authority, administering agency, local managing agency, co-management, BLM role, responsible office, intersecting offices, and data stewardship. Claims need evidence and confidence.

## Geometry and area

For every spatial workflow record:

- source and working CRS;
- geometry type;
- empty/valid status;
- feature, geometry-record, multipart, part/ring, and semantic counts separately;
- repairs/simplification/generalization rules;
- before/after area/length and guardrails;
- sliver/hole/overlap treatment;
- source versus calculated area provenance.

Calculated area is not official acreage unless an authoritative source says so. Preserve native source values and label geometry-derived estimates clearly.

Canonical/full-resolution geometry must remain separate from display-optimized geometry. Simplification, clipping, dissolving, and repair must be deterministic, documented, and tested.

## Cache ownership

A shared cache is not a license to rewrite unrelated children.

- Identify the owned child.
- Hash all siblings before and after a focused refresh.
- Require unchanged siblings unless the task explicitly owns them.
- Treat an idempotent no-op as success when the resulting child is byte-identical.
- Do not copy broad cache directories into source or production.

## External services and live data

Record endpoint, layer ID, schema, query/filter, batching, timestamp, failure retention, fallback behavior, and user-visible currency. Do not make startup network requests for default-off External layers unless the architecture explicitly requires it.

The live-feed generator is a separate repository; see `08_docs/integrations/LIVE_DATA_FEEDS.md`.

## Git policy

Do not commit large raw/processed geospatial data, caches, realistic/final HTML, browser downloads, screenshots, logs, secrets, or machine-specific paths unless an explicit tracked contract and repository-size policy approve the file.

Small tracked fixtures must be labeled non-authoritative and must not be silently substituted for production inputs.

Feature-specific data inventories and pipeline details belong in focused feature/pipeline documentation, not this root policy.
