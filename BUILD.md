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

### 5. Focused preprocessor

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
