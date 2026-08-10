# National Monuments focused pipeline

This directory owns the fail-closed California-scope National Monuments source
refresh. It does not run from the broad reference-layer batch and it never
modifies production.

## Acquisition

`acquire_authoritative_sources.R` is the canonical BRIM entry point. It reads
`source_config.json` and requires an explicit external `--output-root`. Each run
creates a new immutable UTC-named
snapshot directory and refuses to overwrite an existing path.

For BLM and NPS, discovery is restricted to the reviewed durable target IDs.
For the two Forest Service layers, the run preserves the national-monument
attribute discovery response but downloads geometry only for the seven
reviewed California-scope names. It therefore avoids downloading unrelated
national geometry while retaining the pre-filter source evidence needed to
prove target selection.

For every source the downloader preserves:

- parent-service and layer metadata responses;
- count and complete object-ID responses;
- exact request definitions, response headers, timestamps, bytes, and hashes;
- unmodified attribute-discovery pages;
- native-CRS ArcGIS geometry pages;
- EPSG:4326 GeoJSON geometry pages; and
- deterministic combined derivatives plus a selected-attribute inventory.

Object-ID batching is the completeness authority. The count and unique ID set
must agree before attributes or geometry are accepted. Every geometry batch
must return exactly the requested IDs. Transfer-limit or incomplete batches
are split recursively; a one-ID incomplete response fails the run. HTTP 200
responses containing ArcGIS errors, malformed JSON, null/empty geometry,
unexpected geometry type, empty counts, missing target identifiers, or any
count/ID mismatch fail closed.

Example isolated invocation:

```bash
Rscript 02_preprocess/70_national_monuments_pipeline/acquire_authoritative_sources.R \
  --output-root /path/to/BRIM_v0.38_codex_ship/01_raw_data/national_monuments_authoritative_snapshots
```

Tule Lake has one additional focused R acquisition because the NPS LRD `TULE`
feature is only the approximately 37-acre Segregation Center component. The
USFWS National Realty `FWSSpecialDesignation` record supplies the two remaining
USFWS-administered monument parts without reopening the four primary sources:

```bash
Rscript 02_preprocess/70_national_monuments_pipeline/acquire_tule_lake_authoritative_source.R \
  --output-root /path/to/BRIM_v0.38_codex_ship/01_raw_data/national_monuments_tule_lake_authoritative_snapshots
```

The focused config requires exactly `OBJECTID=135` and authoritative view
identifier
`GlobalID_2=bd04754c-21fd-4e14-bcca-72d1d63a563f` from
`SpecialDesignation201903/FeatureServer/0`. The preserved source attributes
also retain the underlying special-designation
`GlobalID={2E9B5F58-4AD7-4467-B80D-C6111AAB66C2}`.

The R implementation uses BRIM's established `httr2` and `jsonlite` ArcGIS
request patterns plus `digest` for SHA-256 manifests. `sf` is intentionally not
needed to preserve source-native ArcGIS JSON and request the service-projected
EPSG:4326 GeoJSON derivative; it is used by the downstream geometry QA and
candidate builder.

The earlier Python implementation has been removed from the tracked production
source tree. Its retained 2026-08-09 raw snapshot remains immutable independent
QA evidence; no BRIM source, build, or production step depends on Python.

`qa_reconcile_acquisitions.R` performs a tiered, fail-closed comparison between
two completed snapshots. Semantically identical requests require byte-identical
untouched response hashes. Counts, IDs, categorical/date/null values, geometry
types, part/ring counts, and coordinate-array structure require exact agreement.
Floating numeric attributes use `1e-12` absolute and relative tolerances;
coordinates use `1e-6` metres for projected native data and `1e-10` degrees for
geographic data. EPSG:3310 area, perimeter, and bounds deltas are also checked
with documented metric tolerances. Derived JSON file hashes may differ because
R and Python are independent serializers; raw authoritative hashes are the
source-byte parity authority.

The external snapshot and later RDS/GPKG/QA products are not tracked Git
artifacts. The tracked configuration records the exact endpoints, discovery
queries, durable IDs/names, and expected polygon geometry contract.

Acquisition is only the first gate. No geometry may be appended or promoted
until current source records are assigned reviewed roles (complete boundary,
agency component, constituent area, legal-status transaction, or duplicate),
multi-source overlaps are reconciled, raw coordinate-level QA passes, and a
display candidate is separately reviewed.

## Authoritative 2026-08-09 acquisition checkpoint

The completed canonical R snapshot is retained externally at:

`01_raw_data/national_monuments_authoritative_snapshots/20260809T190401Z/`

Retrieval ran from `2026-08-09T19:04:01Z` through
`2026-08-09T20:03:06Z`. `COMPLETE.json` and
`acquisition_manifest.json` are present; the acquisition-manifest SHA-256 is
`f1454ffb23f8ef8bcc5d1fea6647bba4d9ee48ec45dce46a80db2c0e017256ac`.
No source encountered a transfer limit and no adaptive split was required.

| Source | Layer/query scope | Acquired records | Untouched native response SHA-256 | Untouched EPSG:4326 response SHA-256 |
|---|---|---:|---|---|
| BLM current | FeatureServer layer 0; reviewed `NLCS_ID` allowlist | 10 | `a7bb0ce5e764e48349b7039551b8470b4bf209a19d6349c6ab378cecf39f67fe` | `59016f24dffe2ab49794d1b86e7e95085f86391593f7cf95db951e5bb98b67b0` |
| USFS current | MapServer layer 0; `AREATYPE = 'National Monument'`, then reviewed California-scope names | 9 | `1276e1fe7241a6295dd46643b1a893b370da404edb82d5d8cef5447f016a418d` | `a2becab0ce55edfdef2d90889261c71f6b6f4070916e2259f4eb371dfcaaf970` |
| USFS legal status | MapServer layer 0; same designation/name scope | 10 | `bc236d0b2fb5f10050b24742a527973815983cb8a3581cd5d9397f7d66bcb574` | `747b06ceed4c63417b4404f828738b973e91b6c540c3e155c40fbe7dc86188fe` |
| NPS official boundary | FeatureServer layer 2; seven reviewed unit codes and `Status = 'Official'` | 7 | `afdf649c6e275c52dfde832e19031b01e9b680dfa04550ee177856f34a7d8385` | `be1a738ecb04802cf91dc1ab87394d1f456777634fb22391d47d1be450ba3043` |

The earlier independent Python QA snapshot is retained at
`01_raw_data/national_monuments_authoritative_snapshots/20260809T185104Z/`.
It is evidence only. The tiered parity report at
`04_processed_data/qa/national_monuments/acquisition_python_20260809T185104Z_vs_r_20260809T190401Z_tiered_v2.json`
passes: all eight native/EPSG:4326 geometry responses have exact raw SHA-256
parity; counts, durable IDs, categorical/date/null values, and geometry
structure match exactly; and the maximum parsed numeric, native-coordinate,
geographic-coordinate, area, perimeter, and bounds differences are all zero.
There is no service-data discrepancy and Python is not a production dependency.

## Candidate, promotion, and focused cache path

The current reviewed candidate is built only from a completed R snapshot:

```bash
Rscript 02_preprocess/70_national_monuments_pipeline/build_national_monuments_candidate.R \
  --snapshot /path/to/completed/snapshot \
  --tule-snapshot /path/to/completed/tule-lake-snapshot \
  --output-dir /path/to/external/candidate-directory \
  --existing-raw /path/to/existing/reference_monuments_wgs84.rds
```

The focused source-repair candidate contract is 20 semantic National Monuments
and 22 visible geometry records. Sand to Snow has one BLM and one USFS final
agency component. Tule Lake has one NPS Segregation Center record and one
two-part USFWS record for Peninsula/Castle Rock and Camp Tulelake. Berryessa Snow
Mountain / Molok Luyuk and Santa Rosa and San Jacinto Mountains use one
reconciled complete shared boundary each; duplicate/provisional agency-source
representations remain provenance and QA context rather than stacked map
geometry. Cascade–Siskiyou retains its complete California–Oregon geometry.

The selected display derivative uses topology-preserving simplification at
1 metre in EPSG:3310. It retains all 24,432 repaired polygon parts, reduces
256,149 repaired vertices to 175,540 display vertices, has zero invalid or
empty display geometries, and changes total displayed area by -18.075281 acres.
The maximum per-geometry absolute change is 18.726030 acres (0.209323650%).

The focused USFWS snapshot is
`national_monuments_tule_lake_authoritative_snapshots/20260810T054408Z/`.
It ran from `2026-08-10T05:44:08Z` through `2026-08-10T05:44:11Z`, returned
one exact record, encountered no transfer limit, required no adaptive split,
and has acquisition-manifest SHA-256
`98064e3ce7aeeca8acb74e9e043a14bd2f690f3819a01441c4f622454bb39427`.
The accepted source-repair candidate is retained externally at
`national_monuments_candidate/20260810T060500Z_tule_repair/`. Promotion is
permitted only to the isolated integration cache destination for the focused
USFWS-aware UI, realistic browser, and human visual-acceptance gates.

Promotion is a separate guarded operation and requires the explicit reviewed
candidate path and destination. It refuses an unreviewed/partial candidate and
backs up an existing isolated destination. The focused cache refresh is:

```bash
Rscript 05_map_build/10_refresh_local_reference_national_monuments_cache.r
```

It owns only the `monuments` children in the Local Reference and label caches,
requires every sibling hash to remain unchanged, and writes timestamped QA.
Two consecutive focused executions must be byte-identical before a realistic
final-map-only build is considered reproducible. Raw snapshots, candidate
RDS/GPKG files, cache products, HTML, and QA run outputs remain external.

## NPS National Park / National Preserve context

The optional card-owned context overlay has its own targeted acquisition entry
point:

```bash
Rscript 02_preprocess/70_national_monuments_pipeline/acquire_nps_park_preserve_context.R \
  --output-root /path/to/external/nps_park_preserve_context_authoritative_snapshots
```

It reads `nps_context_source_config.json` and reuses the same canonical R
acquisition engine and completeness contracts as the four National Monument
sources. It queries the NPS Land Resources Division Boundary and Tract Data
FeatureServer directly:

- layer 2 boundaries: `Status = 'Official' AND UNIT_TYPE IN ('National
  Parks','National Preserves') AND STATE LIKE '%CA%'`;
- layer 1 tracts: `ALPHA IN
  ('CHIS','DEVA','JOTR','KICA','LAVO','MOJA','PINN','REDW','SEQU','YOSE')`.

The reviewed immutable snapshot is
`01_raw_data/nps_park_preserve_context_authoritative_snapshots/20260810T013723Z/`.
It contains 10 official boundaries and 6,207 complete tract/interest records,
ran from `2026-08-10T01:37:42Z` through `2026-08-10T01:39:40Z`, encountered no
transfer limit and required no adaptive split. The boundary native and
EPSG:4326 combined hashes are respectively
`e2babd551093d167c267751a263e46d2adb168bb74579f47f2572893e3ac2c5c`
and `85c791fa6eb3278cc0fd53594c39dbd6f9ec12068f52f8cddaa871058308f0e6`;
the tract hashes are
`cd1c519f96406913c404302eea0cb32f9c2a19d4df940aa274dd43c0843b613f`
and `0191b2343dc021efdac726f018cc743d29117e9bd486777d0674742f9f7c9de4`.

`build_nps_park_preserve_context.R` keeps the official boundary as an outline,
classifies the authoritative `Interest` field, and builds a subordinate
land/interest fill. Fee ownership is masked by overlapping private, public,
other-federal, deferred, or no-information records; explicit less-than-fee
records remain interests rather than fee ownership. The derivative is clipped
to the legislative boundary, dissolved by unit/display role, retains inholding
holes and disconnected holdings, and keeps all of cross-border Death Valley.

The reviewed candidate has 20 display records (10 outlines plus 10
land/interest fills), 167 polygon parts, 741 holes, and 46,433 display vertices.
It uses 2-metre boundary and 5-metre land/interest simplification, exact
projected part/hole retention, zero empty/invalid display records, and a
maximum simplification area change of 0.00554%. Its RDS SHA-256 is
`34f8ada5608dcfc8b3c63dcb83c6d6411ca15fefa1b341d34dd44e31f0f2697c`.

Promote that reviewed external object into an isolated build cache only with
explicit path and hash environment variables:

```bash
BRIM_NPS_CONTEXT_CANDIDATE_RDS=/path/to/nps_park_preserve_context_candidate.rds \
BRIM_NPS_CONTEXT_CANDIDATE_SHA256=34f8ada5608dcfc8b3c63dcb83c6d6411ca15fefa1b341d34dd44e31f0f2697c \
Rscript 05_map_build/11_refresh_local_reference_nps_context_cache.r
```

This writes a standalone context cache and QA inventory; it does not mutate the
shared Local Reference or label caches. The overlay is context only and does
not alter the 20-monument semantic universe.

## 2026-08-10 isolated integration checkpoint

The reviewed Tule Lake source-repair candidate
`national_monuments_candidate/20260810T060500Z_tule_repair/reference_monuments_wgs84_current_candidate.rds`
has SHA-256
`853798e215de9e9f1f83fdfda43c8be9454e20ef14d02782859c07b6b66fdf2c`.
Its guarded isolated promotion produced a processed RDS with SHA-256
`f874a94a48babdf661dedb61494ddc32b9f4d289c72876b174fd3445369ed3a3`.
Focused cache refreshes at `20260810_075554` and `20260810_075618` were
byte-identical: the Local Reference aggregate hash is
`f4b42087a87e6b9632ad88fbb582ead36ea229ecc18bdbf2404d5031dc2d0a64`
and the label aggregate hash is
`439f60102df29138e2bbfeb46cf129984be2c566f5746a09e576bfad5e6778f0`.
The first run changed only the owned monument children and the second changed
no child. A final release-gate rerun canonicalized semantic-child list order;
it preserved every child object hash and produced this label aggregate hash
identically twice.

The current UI-refined realistic final-map-only build is
`06_output/html/PortaTreasure2_core_20260810_105524.html`, 205,006,831 bytes,
SHA-256 `09fb5b9567a90a88431f9b960de18becfdd4e7ca5639e1a3789144e94baf65a1`.
No cache or preprocessor was run for this presentation-only pass. Focused
mounted browser QA passed the agency/shared swatch, dynamic count, result-line,
normal/short/detached scroll, dock, label, layer lifecycle, and
zero-console-error contracts. The redundant Map colors block and Map/display
disclosure are absent; one compact shared-whole-boundary row remains directly
under the agency facet. Human visual acceptance passed on 2026-08-10;
production remains protected by the authorized post-merge release gates.

The external focused evidence is retained under
`04_processed_data/qa/national_monuments/ui_refinement_20260810_095616/`.
Its JSON report SHA-256 is
`a8511b3f4a378d060c79153a1b76eac9e34f341a91cd4e03202b8264ed4c38e6`.

The final NPS-context hover cleanup uses only the semantic unit name and the
existing `legislative_boundary_area_sq_mi` value. The focused evidence is at
`04_processed_data/qa/national_monuments/nps_context_hover_20260810_103546/`;
its JSON report SHA-256 is
`ea7fe674e7949207f817c956ef0fdefc1044b4c5c47d2a4b924cd3b17d7e386d`.

The final sizing refinement removes the context tooltip minimum width while
retaining intrinsic `max-content` sizing and a 320-pixel safeguard. The focused
evidence is at
`04_processed_data/qa/national_monuments/nps_context_intrinsic_hover_20260810_105524/`;
its JSON report SHA-256 is
`cfd05ae386258611a160b3839f69e96cb34ae0ed06c62d9dfe2bb81bf9ea5b16`.
