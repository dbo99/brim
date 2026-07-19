# AGENTS.md

## Project

BRIM is a self-contained Leaflet HTML map built mainly in R.
The authoritative production project remains outside this lean source repo.

## Before editing

- Read `README.md`, `BUILD.md`, and `DATA.md`.
- Inspect current code before relying on historical handoff notes.
- Preserve existing behavior before reorganizing or refactoring.
- Keep patches narrow and discuss large structural changes first.

## Data safety

- Do not delete or overwrite production data.
- Do not invent missing datasets or silently skip required layers.
- Large raw data, processed RDS/GPKG files, caches, and generated HTML are
  external by design; see `DATA.md` and the external-data manifest.
- Small files under `sample_data/` are non-authoritative fixtures.

## Build rules

- Main entry point: `source("run_build_map.r")`.
- Use `build_final_map_only()` when existing caches are valid.
- Use `rebuild_everything_from_cache_and_map()` to rebuild core cache,
  labels, and final HTML from retained processed products.
- Do not run all preprocessors blindly; many contact remote services or
  overwrite processed products.
- State exactly which preprocessing/build stage a change requires.

## Patch delivery

- ZIP patches contain changed files only and preserve project-relative paths.
- Provide one copy/paste-ready R block: set working directory, locate ZIP,
  unzip with overwrite, then run only required steps.
- Keep backups concentrated rather than scattering backup files.

## Conveyance

- Entry point: `02_preprocess/66_build_conveyance_pipeline.R`.
- Preserve facilities, segments, labels, reviewed decisions, aliases, and
  lineage as separate concepts.
- Do not delete legacy source layers merely because display switches are off.
- Canonical geometry must remain unsimplified; optimize only map-facing data.

## Live feeds

- The public feed generator is a separate repository:
  `https://github.com/dbo99/brim-live-data-feeds`.
- Do not duplicate or commit its published data here.
- Treat BRIM/feed integration as an interface contract documented in
  `LIVE_DATA_FEEDS.md`.

## Validation

- Parse all changed R files.
- Run the narrowest relevant build/test.
- For UI changes, test layer on/off, clear behavior, labels, legends,
  filtering, and browser performance.
- Report uncertainty and stop rather than stacking speculative fixes.
