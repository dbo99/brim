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
- Static major water-supply basin geometry:
  `02_preprocess/69_build_major_water_supply_basin_geometry.R`
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

### Major water-supply basin geometry

This focused preprocessor depends on the retained California RDS inputs, the
reviewed six-file CBRFC basin/outlet source set, and original WBD HUC2 14/15
archives listed in the source manifest. Source acquisition is a separate,
reviewed step; the builder does not download data, calculate forecast values,
rebuild unrelated caches, or build HTML:

```r
source("run_build_map.r")
preprocess_major_water_supply_basin_geometry()
```

It writes 23 retained geometries: 19 preserved California records, two
generalized CBRFC operational unions, and two context-only HUC2 polygons. It
also writes the 54-row mapping audit, source/checksum and selector audits,
per-feature hashes, geometry/hole/part/outlet/HUC2 metrics, and five rendered
comparison maps. California originals use `keep = 0.20`; the four cleaned
derived unions use `keep = 0.10`; CBRFC unions use `keep = 0.05` under a 0.05%
area guardrail; HUC2 context uses visually reviewed `keep = 0.01`.

After one display simplification and any required validity repair, the four
derived displays receive a separate narrow normalization pass. It fills
validity-created interior rings and defensively removes only detached parts
strictly smaller than 0.01 square mile. This display-only step has its own
0.01% area-change guardrail and writes a per-artifact QA table. It does not run
on the original 15 or alter the unsimplified EPSG:3310 product.

The LKSA3 authoritative union retains its measured approximately 1.486-square-
mile source gap. Its reviewed one-time 0.05 simplification fills that gap as an
explicit geometry-specific display exception; no general hole-fill rule is
added. The original full-resolution FNF RDS remains authoritative, all 19
Phase B1 feature hashes must remain unchanged, and the old Local
`cnrfc_fnf_delta_map.rds` cache is not rewritten.

Run focused QA after preprocessing:

```r
source("qa/test_major_water_supply_basin_geometry.R")
```

See `08_docs/BRIM_MAJOR_WATER_SUPPLY_BASIN_FORECASTS.md` for the 23-object
inventory, literal 54-key producer mapping, generalized-union limitations,
source hashes, simplification results, supporting-link separation, and rendered
review. This preprocessor is intentionally not part of any broad default
rebuild.

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
