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
233-record dataset: 228 reviewed Resources are `published` and five Resources
are `staged`. GUIDE-I2B-R14 appended the exact 133-Resource R13-rebased
target-200 tranche as staged canonical candidates. GUIDE-I2B-R15B rehabilitates 25
R15A-reviewed same-identity endpoint actions, separately normalizes the USBR
homepage action to `https://www.usbr.gov/`, and replaces the two invalid
Sacramento County and Kern River target records with the accepted broad
SnowTrax and Santa Barbara County Real-Time Hydrology Resources. The rejected
CDEC Reservoir Conditions, Napa monitoring-platform, iSnobal, and Santa
Barbara map proposals are represented only as reviewed subordinate or
alternate access points on their canonical parent Resources, not as additional
canonical Resources. GUIDE-I2B-R15C performed fresh, bounded endpoint
verification for all exact 133 canonical URLs and then published that complete
tranche by changing only `publication_state`; this point-in-time publication
gate is not continuous endpoint monitoring.
GUIDE-I2B-R16B applies the approved balanced USACE/USBR catalog model: 13
canonical additions, seven exact canonical merges, and 123 curated child
access points. Six over-granular Sacramento District reports are retained as
truthfully labeled current or legacy/standby access points of the Sacramento
District Water Control Data System, and the 2024 Long-Term Operations Record
of Decision is retained under the existing Long-Term Operations program. The
retired stable IDs remain aliases of their parents. Report code `scc` is
publicly labeled `Success Dam & Lake`; the false Sacramento River / Clear
Creek phrase is not public search metadata. Water Control Manuals remains held
and absent because its reviewed collection endpoint was empty.
The final R16B visual-review currentness correction retains
`resource_swrcb_impaired_waters_and_tmdls_program` as the stable statewide
program identity, replaces its obsolete 2010 action with the evergreen Surface
Water Quality Assessment Program page, and adds accurately labeled 2024 and
2026 Integrated Report cycle pages. The current 2024 line and polygon Products
are exact selected Products of that parent; the 270-Product authority contains
no 2026 Integrated Report Product, so no 2026 Product relationship is inferred.
At C1, CNRFC CSV-child consolidation and six reviewed canonical additions brought
the authority to 215 Resources / 210 published / five staged. GUIDE-I2B-R10 published exactly 34
of the 39 identity-ready Wave-2 records by changing only
`publication_state`. The five earlier held IDs remain
`resource_nasa_giovanni`, `resource_usgs_earthexplorer`, and
`resource_usgs_water_data_apis` for subject review, plus
`resource_nrcs_web_soil_survey` and `resource_usda_cropland_data_layer` for
taxonomy resolution. The publication projection runs before
relationships, search text, counts, facets, adaptation, or browser embedding,
so only the 228 published Resources are browser-visible. The registry preserves immutable
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

R10 publication, R14 staging, and R15C publication introduce no Product
relationship, Guide profile, controlled vocabulary, or facet. An empty
`subject_tags` array is valid when exact evidence does not support a controlled
subject; R15C preserves that value for 15 of its newly published records rather
than inventing metadata.
`00_config/guide_product_resource_relationships.json` is the sole authored
Product–Resource and Resource map-representation authority. Its schema-version-2
`products` and `resources` arrays must equal the complete current compiled
Product set and canonical Resource set exactly: 270 Product records and 233
Resource review/representation records. Each Product owns a delivery
classification, an independent coverage-review state and disposition,
evidence, and zero or more exact canonical Resource links. Each Resource owns
exactly one map-review state, an optional reviewed map representation, and
evidence. Resource representation is validated from the reverse Product links
rather than maintained as a second relationship list.

The current authority contains 98 reviewed and 172 `not_yet_reviewed` Products,
121 exact links, and the exact supported relationship roles: 13 direct matches,
64 selected-product links, and 44 source references. Delivery class is
secondary descriptive context and does not imply Resource coverage or create a
primary public filter. Multiple-source Products expose the exact canonical
source Resource list in registry order. The three DWR/TRE Altamira Products
remain one source family, while `EXT033` remains the separate DWR/USGS/TRE
multiple-source composite. Only accepted apply actions are present. The staged
intake backlog remains deferred under its existing evidence packets.
Placeholder or inferred Resources are prohibited.

The full Resource relationship authority contains three
`direct_match_in_brim`, 25 `selected_products_in_brim`, 200
`not_currently_mapped_in_brim`, and five `not_yet_reviewed` records. All 133
Resources in the revised R15 target use the reviewed
`not_currently_mapped_in_brim` classification. R15B removes the two invalid
Resource review records and inserts matching reviewed records for the broad
SnowTrax and Santa Barbara County replacements; it applies no Product-link
action, so the Product corpus remained 270 and the canonical link count
remained 86 at R15B. The base R16B batch adds the exact crosswalk-proven
`source_reference` from `ops_cdec_reservoir_storage` to the Sacramento District
parent, preserving the existing CDEC link and the two existing USBR links. That
parent is the one new `selected_products_in_brim` Resource; the other 12
additions are reviewed as `not_currently_mapped_in_brim`. The consolidated
visual-review correction then adds selected-product links from Drought.gov
California to `ops_us_drought_monitor` and from CoCoRaHS to its exact California
and 50-state Products, while removing the unsupported provider-similarity links
from California Water Watch to BRIM-mapped conveyance and the Delta operations
snapshot. California Water Watch is therefore not currently mapped. The
final currentness correction then adds the two exact 2024 Integrated Report
line/polygon links to the evergreen statewide assessment-program parent. The
R16B authority was 270 Products, 211 Resource records, and 90 canonical links.
The subsequent CNRFC review preserved seven parent links and reached 94 links.
The C1 WPC ERO review reached 96 links across 72 Products and 27 Resources.
The current C2 source-reference additions bring this to 121 links across 93
Products and 28 Resources; all 270 Product identities are unchanged.

Product and Resource IDs, controlled values, set equality, link cardinality,
evidence paths, duplicates, unknown fields, and disposition/representation
rules validate fail closed. Resource links validate against all 233 canonical
Resources, while publication projection removes staged Resources before public
relationships are derived. Product relationships and Resource map presence are
never inferred from titles, providers, summaries, URLs, publication, or
geographic intersection. `00_config/guide_product_enrichment.json` owns
editorial Product content only and contains no Resource relationships. The
R12A compatibility objects and compiler adapter have been deleted; permanent
compatibility shadow, old/new dual authority, and fallback are prohibited.

All 228 published Resources have reviewed map-presence authority: three are
`direct_match_in_brim`, 25 are `selected_products_in_brim`, and 200 are
`not_currently_mapped_in_brim`. The public primary views derive only from those
Resource records and are exactly `In BRIM map` (28), `Beyond the map` (200), and
`All Resources` (228). Product relationships support detail and exact
Product-context navigation but do not create another primary Resource filter.

The browser projection contains the 228 published records in registry order and
only the reviewed 23-field Resource shape: identity and descriptive metadata,
labeled access points, Resource Type and temporal machine IDs with build-derived
labels, normalized geographic scope with its build-derived scope label and
independent named places, `mapReviewState`, `mapRepresentation`, exact
`representedProducts`, and normalized search text. Represented Product entries
carry only Product identity/title plus secondary delivery, coverage, role, and
exact source-Resource IDs. Migration aliases, legacy relationship flags and
subtypes, runtime authority, raw source evidence, and unreviewed fields do not
enter that shape.
The selected-detail primary external action is the one access point whose role
is `canonical` and whose URL exactly equals `canonicalUrl`; other access points
retain their projected labels and exact URLs as secondary actions. This display
priority does not add or infer Resource authority. Where both broad official
landing or sector pages and configured views are retained, the broad official
pages precede the configured views. The NOAA GOES Image Viewer retains its
official canonical action and uses California-relevant Pacific Southwest and
U.S. Pacific Coast sector access points before the two configured Pacific
Southwest views. The same parent-first rule keeps CDEC Reservoir Conditions
under `resource_dwr_cdec`, the Napa OneRain root under the existing Napa map
Resource, iSnobal under the broad SnowTrax Resource, and the Santa Barbara map
route under the broad county real-time hydrology Resource.
R16B extends that parent-first rule without changing the R or JavaScript search
runtime. It adds 30 Sacramento WCDS, five Los Angeles water-management, 50 CVO,
two national/regional directory, one LTO Record of Decision, five Lower
Colorado, six Upper Colorado, three Colorado Basin hub, three Klamath, two
Truckee/TROA, five RISE, three CVP Water Supply, four Hydromet, and four AgriMet
access points. Truthful labels and curated `search_aliases` preserve reservoir,
project, Section 7, regional, program, and legacy discovery; URLs and stable IDs
remain excluded from public search text. CVO retains exactly 42 searchable
user-facing actions and eight ordinary accounting/derived actions.
HTTPS remains the default requirement for every canonical external Resource
URL, access point, and public source reference. Three provider endpoints are
explicit exact-pair exceptions: TID WISKI at
`http://wiskiweb.tid.org/index.htm`, Kings River Water Association at
`http://kingsriverwater.org/`, and Orange County Hydrology at
`http://hydstra.ocpublicworks.com/web.htm`. One shared validator accepts those
values only for their exact reviewed Resource IDs and full URLs; both registry
loading and compiled Guide-bundle validation route through that policy. Host,
provider, suffix, and wildcard matching are prohibited. They remain user-initiated
external navigation actions, never runtime fetches, embedded mixed-content
subresources, or machine-facing dependencies. Any future HTTP exception
requires explicit review. This bounded approval does not establish continuous
endpoint monitoring.
`default` remains the only current Guide profile, and the standalone HTML makes
no runtime Resource-data request or browser-storage copy.

GUIDE-I2B-R15C publishes the exact revised 133-Resource target after fresh,
bounded endpoint verification of all 133 stored canonical URLs. It changes no
Guide UI/controller behavior, relationship authority, or URL policy; runs no
preprocessor; and regenerates no cache. The endpoint checks are point-in-time
publication evidence rather than monitoring.

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
Information Type is reviewed Resource metadata, never inferred at runtime from
provider, Product tags, hosting, or keywords. Multiple existing vocabulary tags
may describe a supported mixed collection. Forecasts are prospective guidance;
Live Observation describes recurrent observations at their stated cadence;
Historical Context covers archives, climatologies, and reconstructions; Model /
Simulation and Screening / Derived identify modeled outputs and derived
indicators. Static Reference requires a reference dataset, document, or method;
Tool / Workflow requires an actual exploration or processing function; External
On-Demand Service requires a documented requested service operation. A generic
homepage or uncertain record may remain untyped. Modeled or assimilated output
does not become a measurement because it describes current conditions.
Resource Information Type badges count distinct matching IDs, ignoring only the
currently selected Information Type while honoring all other filters and Product
context. Counts cover the full matching set regardless of how many result rows
are currently rendered. Classification edits preserve the remaining canonical
fields, staged records, and Product relationships.
The Explorer initially renders at most 25 result rows and exposes further rows
in deterministic 25-record increments, so all 228 published Resources remain
reachable without embedding hidden card copies or introducing pagination,
virtualization, runtime fetching, or browser-storage authority.

Raw bookmark exports, intake workbooks, candidate records, unresolved notes,
and other Resource-inventory evidence remain External research/input material
and are not tracked wholesale or embedded in the Guide. Importing a future
inventory requires a separate reconciliation and approval; it cannot be
treated as an automatic registry or relationship expansion.

Ordinary Product relationship onboarding is declarative. When the canonical
Resource already exists, add the new Product record to
`guide_product_resource_relationships.json`; when a new canonical Resource is
also required, add it to `guide_resources.json` and link it from the Product
record. Ordinary additions do not require compiler, JavaScript, CSS, QA-source,
or architecture-document changes unless the schema or controlled vocabulary
changes.

## Documentation-only Resource intake and ownership

`08_docs/catalog/BRIM_RESOURCE_INTAKE_LEDGER.json` is a schema-version-1
`DOCUMENTATION_ONLY` decision ledger. It retains 97 intake entries, 27 linked
water-year action children, and 278 station/name alias children for 139
established CNRFC station identities. It separates submitted and replacement
URLs, canonical identity, access points, aliases, and exact relationship
operations, with immutable packet/member hashes and row pointers. Original
proposals remain historical evidence; actual maintainer decisions are recorded
separately. The unchanged relationship validator checks that this tracked evidence file
exists; the builder does not parse it as a catalog or embed it. It creates no feed,
facet, current-year classification, automatic publication, or eligibility rule.

The CNRFC parent owns 24 actions: one canonical landing, the Water Resources
regional forecast map, the HD6RSA daily basin QPF/freezing-level page, 15 selected
station views, five CSV/archive actions, and the explicitly dated WY2026 SACC0
action. The two retired CSV-child stable IDs and
their migration/search identities resolve to that parent. Its seven Product
links are unchanged. The WPC ERO parent owns the Day 1–3 selected Products;
the source-backed distinction from QPF and the unchanged Product delivery
classifications are documented in the architecture's WPC ERO subsection.
Runtime authority has 457 action rows / 456 distinct URLs, with 452 public
actions, all unique. The one staged/public USGS URL overlap is deliberate.
There are 690 alias occurrences across the three existing namespaces. Alias text remains distinct from a
station join, feed, or generated menu.
The station labels name the river and forecast point for full natural flow
(FNF) water-year trends; URLs and default-year behavior are unchanged. They
make no equivalence claim about observed flows, regulated releases or storage.
Michigan Bar remains a river point; EXQC1 uses Lake McClure / New Exchequer.
Water Resources is the wider regional entry point. HD6RSA covers 24-hour basin
QPF and freezing levels for Days 1–6, not general temperature or a BRIM feed.
One documentation-only A5 amendment records 15 label changes and two new action
submissions; the original 97 entries and all 27/278 children remain unchanged.
The C2 amendment preserves all 23 C1 actions and labels, and adds only the dated
SACC0 action plus its approved metadata. Scope and execution approval remain
separate from candidate acceptance; no dependency hold is released.
The full default Guide JSON cap is 940000 bytes, with at most 626502 bytes
of growth over the unchanged 313498-byte baseline and 200000 bytes of combined
Guide JS/CSS. The pre-polish corpus measured 832232 bytes (518734 growth); this
historical measurement is not the size of the changed corpus. Candidate evidence
must record a fresh full compiler/serializer measurement without field exclusions.

The ledger preserves historical BRIM-R17C-G2-01 and BRIM-R17C1-EXEC-01 decisions.
Its appended C2 amendment records BRIM-R17C2-G2-02 scope and
BRIM-R17C2-EXEC-01 execution approval separately. The exact selected Weather Lab
Resource and L082 dashboard are included; all other individual exclusions and
holds and all 45 housekeeping/dependency holds remain. The duplicate CDEC
confirmation remains a parent action. `APPROVED_*` means
scope approval; it does not mean accepted implementation. PR, accepted SHA/tree,
and implemented date remain null until separately verified human acceptance.
The historical R17C-G1-V2-CTIME-01 exception is limited to its 24 enumerated
transitions, with cause `UNESTABLISHED`; it never excuses fresh drift or
releases a dependency for cleanup. Water-year values remain
`external_source_only_no_runtime_feed` and cross-scheme labels are not
scientific equivalence joins.

The selected combined wave adds eighteen Resources and 25 exact BLM source-reference
links. Current public views are 28 In BRIM map / 200 Beyond the map / 228 All;
121 links cover 93 Products and 28 Resources. The original 96 links, all five
staged records, and source attribution are preserved. The existing published
USGS service Resource uses `https://api.waterdata.usgs.gov/` and retains its
former name as an alias; operational API consumers are unchanged.

L082 is one BLM California Wildfire Dashboard Resource with one external action
and no Product link. Its archived 2022-06-29 official BLM notice refers through
`https://bit.ly/39IubK0` to ArcGIS item `1c4565c092da44478befc12722cf0486`.
The submitted trailing `#` is retained. This is exact referral evidence, not a
claim of current fire restrictions, update frequency, dataset ownership, or
interactive browser health. Weather Lab and OPERA DIST are external discovery
links, with no feed integration or new map layer.

The C2 ledger amendment accounts for all 129 named rows, 305 children, 22
metadata findings, eleven related-work items, and 270 Product dispositions.
Classification is independent of metadata completeness: 151 Products remain
deferred outside selected families; the other residual groups remain explicit.
The old 97 entries, 27/278 children, and A5 amendment are unchanged. Human visual
acceptance and release metadata remain pending.

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
