# BRIM build guide

## Entry point

```r
setwd("/path/to/BRIM")
source("run_build_map.r")
```

## Build levels

### Final map only

```r
build_final_map_only()
```

Uses existing current core and label caches.

### Rebuild core cache, labels, and final map

```r
rebuild_everything_from_cache_and_map()
```

This is the preferred complete map-build validation after data cleanup.

### Preprocessors

Preprocessors under `02_preprocess/` are not one homogeneous batch.
Some download remote data, some generate production RDS/GPKG files, some
perform one-time audits, and some may overwrite outputs. Run only a reviewed
dependency chain for the dataset being updated.

## Important pipelines

- BLM core boundary: `02_preprocess/01_blm_managed_and_held.r`
- Consolidated conveyance: `02_preprocess/66_build_conveyance_pipeline.R`
- UIC aquifer exemptions:
  `02_preprocess/67_build_uic_aquifer_exemptions.R`
- Main map assembly: `05_map_build/` via `run_build_map.r`

### UIC build boundary

`run_uic_pipeline("check_only")`, candidate refresh, and the standalone UIC
sandbox map are a developer/research workflow separate from production BRIM.
Candidate promotion requires an explicit candidate ID and confirmation phrase,
but even promoted pipeline products are comparison baselines rather than Local
map inputs; see `UIC_AQUIFER_EXEMPTIONS.md`.

The ordinary `build_final_map_only()` path never reads UIC raw, candidate,
approved, map-ready, or label products. Production UIC rows appear only under
**External Layers → Energy / Minerals → Underground Injection Control (UIC)**
and contact their authoritative services only after a user enables them.

## Source-repository limitation

This GitHub-ready source directory does not include the production caches
and processed data needed to render the complete 200 MB HTML. Use the
validated `BRIM_v0.38_codex_ship` or restore external data listed in
`EXTERNAL_DATA_MANIFEST.csv` for production builds.

## Sample validation

A source-only validation should at minimum:

1. Parse every R file.
2. Confirm no tracked file exceeds repository size policy.
3. Inspect the fixtures under `sample_data/`.
4. Review path and credential scan reports.
