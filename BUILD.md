# BRIM build, test, and release guide

## Environment boundary

Author source in `BRIM_v0.38_source_repo`. Run realistic builds in `BRIM_v0.38_codex_ship`. Treat `BRIM_v0.38` as production and modify it only during an approved post-merge release.

The lean source repository does not contain every required processed product and cannot prove a complete production build by itself.

## Entry point

```r
source("run_build_map.r")
```

## Build decision tree

Choose the smallest valid path.

### 1. Source-only validation

Use when changing configuration, helpers, controllers, tests, or documentation and no rendered artifact is yet required.

- parse changed R files;
- validate JavaScript in its intended context;
- run focused R/JS/fixture tests;
- run diff, secret, and path scans.

### 2. Final map only

```r
build_final_map_only()
```

Use only when required current caches already exist and the change affects map assembly, UI, styling, popups, controls, or browser behavior without changing processed/cache data.

### 3. Rebuild retained cache and map

```r
rebuild_everything_from_cache_and_map()
```

Use only when existing retained processed products are authoritative and the core/label cache must be reconstructed. This is not permission to run preprocessors.

### 4. Focused child/cache refresh

Use a layer-owned refresh entry point when one cache child can be updated without rewriting siblings. Record before/after child hashes and require unchanged sibling hashes.

#### National Monuments focused reproduction

National Monuments is owned by
`02_preprocess/70_national_monuments_pipeline/`. Its canonical network
acquisition is R-based. The earlier Python acquisition implementation has been
removed from tracked source; only its immutable raw snapshot remains as
independent QA evidence and no BRIM build step depends on Python.

Run the following gates separately and only in the isolated build workspace:

1. acquire a new immutable snapshot only when source refresh is actually
   required:

   ```bash
   Rscript 02_preprocess/70_national_monuments_pipeline/acquire_authoritative_sources.R \
     --output-root /path/to/external/national_monuments_authoritative_snapshots
   ```

2. require `COMPLETE.json`, no `FAILED.json`, exact four-source counts/IDs, and
   the acquisition manifest before candidate work. Tule Lake additionally
   requires the focused USFWS addendum acquired with
   `acquire_tule_lake_authoritative_source.R`; require its exact one-record
   `OBJECTID=135` / GlobalID contract and separate `COMPLETE.json`;
3. if an independent snapshot is being reconciled, run
   `qa_reconcile_acquisitions.R --reference ... --candidate ... --output ...`
   and require the tiered report to pass;
4. build a new external candidate with
   `build_national_monuments_candidate.R --snapshot ... --tule-snapshot ...
   --output-dir ...`
   and review source roles, validity repair, simplification benchmarks, area
   changes, overlaps, migration, and exact 20-semantic/22-display counts;
5. promote only the explicitly reviewed candidate to the isolated processed
   RDS using `promote_reviewed_candidate.R` with its expected SHA-256;
6. run only
   `Rscript 05_map_build/10_refresh_local_reference_national_monuments_cache.r`,
   require every unrelated reference/label child hash to remain unchanged,
   and rerun once to prove deterministic aggregates;
7. run the focused R/JavaScript QA, then `build_final_map_only()` and mounted
   browser QA.

The optional National Park/Preserve context is a separate targeted child of
this feature. When a source refresh is justified, acquire it with
`acquire_nps_park_preserve_context.R`, build it with
`build_nps_park_preserve_context.R`, review the immutable candidate and hash,
then run `05_map_build/11_refresh_local_reference_nps_context_cache.r` with the
explicit `BRIM_NPS_CONTEXT_CANDIDATE_RDS` and
`BRIM_NPS_CONTEXT_CANDIDATE_SHA256` variables. This standalone cache must not
rewrite the shared Local Reference or label caches. Require exact 10-unit and
6,207-tract source counts, 20 display records, valid/nonempty geometry,
inholding-hole retention, and a deterministic second cache run before the
realistic build.

Do not reacquire to force derived R JSON text to match an independent
serializer. Corresponding untouched authoritative responses use exact hash
parity; parsed numeric attributes and coordinates use the documented tiered
tolerances. Raw snapshots, candidates, processed RDS/GPKG, caches, QA run
outputs, and realistic HTML stay external. Exact source/query/count/hash and
current candidate contracts are recorded in the pipeline README and
`08_docs/features/NATIONAL_MONUMENTS.md`.

### 5. Focused preprocessor

#### BLM well inventory focused regeneration

**NOT_RUN_IN_B4_I1.** The source review gate validates only curated table
invariants, extracted pure functions with synthetic data, and offline emitted
JavaScript. The commands below describe later, separately approved isolated
execution; they are not evidence that products have been regenerated.

From an approved isolated build root with exact reviewed source, external
NOC/field-list CSVs, current BLM managed-land geometry and accepted preview
inputs already bound:

```r
source("run_build_map.r")
update_blm_gw_well_inventory_sources()
update_blm_gw_well_inventory_blm_distances()
refresh_blm_gw_well_inventory_cache()
build_final_map_only()
```

Run each stage only after its own input/output authorization and checks. The
convenience `refresh_blm_gw_well_inventory_and_map()` runs those same four
stages and is unsuitable when only one stage is approved. The standalone
focused cache entry is
`Rscript 05_map_build/14_refresh_blm_gw_well_inventory_cache.r`; it requires
existing normalized combined inventory and a complete matching distance
sidecar. Missing/stale/duplicate keys, source/coordinate mismatches and
symlink/archive collisions fail before cache saves. It does not regenerate
prerequisites or invoke the full core-cache builder. Normal full-build stages
remain unchanged.

The source normalizer rewrites **both** independent inventories and their
combined product. Under `04_processed_data/rds/`, its exact outputs are:

- `blm_noc_drilled_wells_wgs84.rds`
- `mojave_2025_gw_well_inventory_wgs84.rds`
- `blm_gw_well_inventory_combined_wgs84.rds`

Under `04_processed_data/qa/`, it rewrites the six CSVs with prefix
`blm_gw_well_inventory_` and suffixes `source_summary_latest.csv`,
`coordinate_qa_latest.csv`, `status_counts_latest.csv`,
`field_summary_latest.csv`, `duplicate_exclusions_latest.csv` and
`popup_field_guide_latest.csv`.

Unchanged preprocessor 63 recalculates the **combined** distance sidecar
`04_processed_data/cache/latest/blm_gw_well_inventory_blm_distance_fields.csv`
against `04_processed_data/rds/blm_managed_core_3310.rds`. It also rewrites
four QA CSVs under `04_processed_data/qa/`, with prefix
`blm_gw_well_inventory_blm_distance_` and suffixes `summary_latest.csv`,
`by_source_latest.csv`, `bins_by_source_latest.csv` and `preview_latest.csv`.
This is not an Albion-only distance run. Normalization and distance calculation
use bound local inputs and require no provider acquisition.

Focused refresh writes only `blm_noc_drilled_wells_map.rds` and
`mojave_2025_gw_well_inventory_map.rds` under `04_processed_data/cache/latest/`,
plus their existing `<stem>_<YYYYMMDD_HHMMSS>.rds` counterparts under
`04_processed_data/cache/enriched/`. Existing latest files can be overwritten;
timestamp collisions fail. Save utilities are not a multi-file transaction:
an I/O failure after one save requires recovery from the authorized backup.
Standard path initialization may create missing project directories. Final
HTML is a separate last stage with its own output manifest and browser gate.

Before execution, bind and back up every existing overwrite target, reserve
noncolliding enriched/HTML names, hash all protected siblings and define
restore-and-hash recovery. Preserve the complete accepted preview-input
manifest, including both restored USGS streamgage and groundwater indexes,
all auxiliary data and installed tool/serializer bindings; no convenience
cache or provider refresh may replace them. Require exact NOC attributes,
geometry, popup and distance parity. Only these sidecar execution-provenance
columns may differ: `blm_distance_run_time`, `input_well_inventory_rds`,
`input_well_inventory_mtime`, `input_blm_lands_rds`, `input_blm_lands_mtime`.
That parity is not demonstrated by synthetic tests. Validate the 138-site
Albion contract and every unrelated cache hash, then separately review the
rendered layer, filters, labels, popups, teardown and narrow viewport.

Canonical source lineage and remaining uncertainty are in
[BLM well inventories](08_docs/features/BLM_WELL_INVENTORIES.md).

Run only after the preprocessor gate below passes.

### 6. Broad dependency orchestration

Exceptional. Requires an explicit dependency graph, backup plan, remote-service plan, overwrite inventory, runtime expectation, and user authorization. Never infer authorization from a request to “build” or “test.”

## Preprocessor gate

Before any preprocessor runs, document:

1. exact script/function;
2. why current processed products/caches are insufficient;
3. authoritative inputs and expected versions/hashes;
4. remote endpoints and whether network access is required;
5. all outputs, archives, QA products, and files that may be overwritten;
6. sibling products that must remain byte-identical;
7. expected row/feature/component counts and schema/CRS contracts;
8. failure behavior and rollback/recovery method;
9. exact subsequent build step;
10. focused tests required for acceptance.

Do not run preprocessors generically, speculatively, by loop, or as a convenience sweep. Do not let a build script install packages or silently download replacement data unless the reviewed pipeline explicitly owns that behavior.

## Isolated realistic build

A user-facing change is not accepted from source tests alone.

1. verify clean source branch/HEAD;
2. sync an exact source manifest to `codex_ship`;
3. run only the authorized processing/cache stage;
4. prove sibling products are unchanged;
5. build a new realistic HTML;
6. record path, size, SHA-256, source manifest, cache hashes, and test results;
7. perform mounted browser tests where possible;
8. obtain human visual acceptance.

Do not modify production during this stage.

## Commit and PR gate

After acceptance:

- stage only the coherent source/QA/documentation manifest;
- exclude research ZIPs, realistic HTML, generated caches, screenshots, logs, and sandbox reports unless an established tracked contract requires them;
- audit the staged diff;
- commit and push only with user authorization;
- merge through a human PR gate;
- update local `main` by fast-forward and verify clean state.

## Production release

Production release occurs only after merge.

1. audit the exact merged delta;
2. identify authored files and accepted generated artifacts separately;
3. back up every production target in one release folder;
4. write a rollback script before copying;
5. sync exact authored files from merged source;
6. deploy only the exact accepted HTML/cache artifacts by recorded hash;
7. verify every destination hash/content;
8. run a production smoke test, including at least one unrelated layer;
9. write a permanent closeout record;
10. delete merged branches only after production acceptance.

A new build after visual acceptance is a new candidate and requires review; do not silently substitute it for the accepted artifact.

## Build evidence

Every implementation report should state:

- repository/branch/base/head;
- exact changed-source manifest;
- exact processing/build commands;
- inputs/outputs and cache effects;
- test commands/results;
- realistic HTML path/size/hash;
- manual review targets;
- production status;
- staged/commit/PR status.

Feature-specific pipelines and commands belong beside the pipeline or in `08_docs/features/`, not in this root guide.
