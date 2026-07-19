# BRIM — BLM-California Resources Information Mapper

BRIM is a standalone Leaflet web map built primarily in R for
BLM-California water-resource screening, reference-data review,
External Layers, and Ops Live situational-awareness feeds.

This directory is the **private-GitHub-ready source edition** of BRIM v0.38.
It preserves the complete source code and preprocessing logic while keeping
large production data, caches, and generated HTML outside normal Git history.

## Main entry point

```r
source("run_build_map.r")
```

Normal production commands:

```r
build_final_map_only()
rebuild_everything_from_cache_and_map()
```

A production build requires external processed data described in `DATA.md`.
The small `sample_data/` directory is for code and schema inspection only.

## Repository map

- `00_config/` — source registry, layer registries, and catalogs.
- `02_preprocess/` — all preprocessing and QA workflows.
- `03_functions/` — shared R/Leaflet helpers.
- `05_map_build/` — map assembly and browser-side controls.
- `08_docs/` — architecture, maintenance, and workflow notes.
- `qa/` — compact QA scripts and summaries.
- `sample_data/` — non-authoritative representative fixtures.

## Related public repository

Live feed production is maintained separately at:

`https://github.com/dbo99/brim-live-data-feeds`

See `LIVE_DATA_FEEDS.md`; the public repository is intentionally not
embedded in this private source repository.

## First files for Codex

1. `AGENTS.md`
2. `BUILD.md`
3. `DATA.md`
4. `run_build_map.r`
5. the relevant registry/helper/build file for the requested layer

This repository is private project source and has no public-use license.
