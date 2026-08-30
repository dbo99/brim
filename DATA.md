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

## Guide Resource metadata

`00_config/guide_resources.json` is the canonical tracked source for authored
BRIM Guide Resource metadata. Its schema version 3 contains one ordered
33-record dataset, and all 33 reviewed Resources are `published`. The registry
preserves immutable
`resource_*` IDs, separates final-ID aliases, build-time `res.*` migration
aliases, and future human search aliases, and stores only reviewed descriptive
fields. Every Resource has exactly one controlled Resource Type machine ID,
one controlled temporal-character machine ID, and one geographic-scope object
whose controlled scope-class ID is independent of its normalized named-place
array. `resource_granularity` remains canonical editorial metadata unchanged
from schema version 2; it is not a public facet. Temporal character describes
the Resource's temporal role and is separate from cadence, update frequency,
freshness, or runtime status, none of which the registry infers. The compiler
owns the single ID-to-label vocabulary for Resource Type, temporal character,
and geographic scope and projects deterministic labels at build time. The
registry does not contain Product relationships or profile, layer,
lifecycle, status, freshness, or runtime-control authority.

`00_config/guide_product_enrichment.json` is the sole authored authority for
Product-to-Resource relationships. Its 17 reviewed rows consist of zero
`displayed_in_brim`, seven `used_by_brim`, and ten
`related_external_resource` relationships. They are never inferred from
Resource titles, providers, summaries, URLs, publication, or geographic
intersection. Relationship Resource IDs validate against the complete
published registry and Product IDs validate against the current projected
Product universe; the compiler then derives the reverse Resource-to-Product
links from those exact surviving rows.

The Resource Explorer derives its primary views from those rows without adding
another relationship category. `BRIM-linked` is the union of Resources with at
least one `displayed_in_brim`, `used_by_brim`, or
`related_external_resource` row; it currently contains nine unique Resources.
`Beyond BRIM` is the exact 24-Resource complement within the 33 published
Resources. The visible relationship subtype refinements preserve the canonical
types and currently count zero Available in BRIM, six Used by BRIM, and three
Related resource records by unique Resource rather than relationship row.

The browser projection contains the 33 published records in registry order and
only the reviewed 22-field Resource shape: identity and descriptive metadata,
labeled access points, Resource Type and temporal machine IDs with build-derived
labels, normalized geographic scope with its build-derived scope label and
independent named places, derived related Products and relationship flags, and
normalized search text. Migration aliases, runtime authority, raw source
evidence, and unreviewed fields do not enter that shape.
The selected-detail primary external action is the one access point whose role
is `canonical` and whose URL exactly equals `canonicalUrl`; other access points
retain their projected labels and exact URLs as secondary actions. This display
priority does not add or infer Resource authority.
`default` remains the only current Guide profile, and the standalone HTML makes
no runtime Resource-data request or browser-storage copy.

Resource filtering uses only that projected authority. Provider, Subject,
Information Type, and Resource type may be filtered directly; Resource type is
the sole scalable native select under `More filters`; its state uses the machine
ID and its option text uses the controlled label. Temporal character is not a
facet or search field and appears in selected detail only when it is not
`unknown`. Geographic scope and named geography remain search/detail metadata,
not facets; an `unknown` scope is retained canonically but its label is omitted
from public detail. Resource granularity remains editorial-only detail/search
metadata rather than a facet. Variables and use scopes remain searchable
descriptive fields, not controlled facet vocabularies. Temporal, geographic,
named-geography, access-point-type, granularity, verification, and priority
facets are not current Guide authority.

Raw bookmark exports, intake workbooks, candidate records, unresolved notes,
and other Resource-inventory evidence remain External research/input material
and are not tracked wholesale or embedded in the Guide. Importing a future
inventory requires a separate reconciliation and approval; it cannot be
treated as an automatic registry or relationship expansion.

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

### National Monuments data contract

National Monuments uses four primary authoritative service layers: BLM current
National Conservation Lands geometry, USFS current designated-area geometry,
USFS designated-area legal status, and NPS official unit boundaries. Tule Lake
also requires a focused authoritative USFWS National Realty
`FWSSpecialDesignation` record because the NPS `TULE` boundary is only the
Segregation Center component. The canonical primary acquisition is
`02_preprocess/70_national_monuments_pipeline/acquire_authoritative_sources.R`.
The focused addendum is acquired by
`acquire_tule_lake_authoritative_source.R`. Both write only immutable external
snapshots and must pass count, complete object-ID, target-ID,
response-integrity, geometry, and hash gates before a candidate can be built.

The BRIM semantic universe is 20 current National Monuments wholly or partly
in California. Cross-state geometry is retained complete. Semantic monument
ID, visible geometry/component ID, source OBJECTID/durable ID, agency
relationship, and legal-status record are separate keys. Source publisher is
not management authority, and duplicate agency-published whole boundaries are
not independently rendered.

The focused source-repair derivative has 22 visible geometry records because
Sand to Snow and Tule Lake each use two verified administering-agency source
records. Tule Lake remains one semantic monument: its NPS record contains the
Segregation Center, while its USFWS two-part record contains Peninsula/Castle
Rock and Camp Tulelake. Berryessa Snow Mountain /
Molok Luyuk and Santa Rosa and San Jacinto Mountains use one reconciled shared
whole boundary each. USFS legal-status geometries and duplicate/provisional
source boundaries remain provenance/QA context. Exact current endpoints,
queries, snapshots, hashes, source-role decisions, repair/simplification
metrics, and external-product locations are documented in the focused pipeline
README and `08_docs/features/NATIONAL_MONUMENTS.md`.

National Parks and National Preserves are optional context, not National
Monument semantic records. Their separate canonical R acquisition targets the
official NPS Land Resources Division boundary layer 2 and tract layer 1 for the
exact reviewed California unit-code set. Preserve untouched boundaries and
tract responses externally. Never fill the legislative boundary as though it
were ownership: derive the display fill only after documented `Interest`
classification, conservative non-NPS masking, dissolve, hole retention, and
geometry QA. The standalone context cache has no authority to change monument
identity, filters, labels, search, or shared cache children.

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
