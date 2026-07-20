# BRIM Codex handoff

## Baseline

- Version: BRIM v0.38
- Private repository: `dbo99/brim`
- Default branch: `main`
- Main entry point: `source("run_build_map.r")`

## Repository roles

- `BRIM_v0.38_source_repo`: lean Git source repository for normal Codex work.
- `BRIM_v0.38_codex_ship`: separate build-capable project with required processed products.
- `BRIM_v0.38`: full production working project with large data, caches, outputs, and working material.

Do not merge these roles or add large generated products to Git.

## Build and data limits

Use `build_final_map_only()` when valid caches exist.
Use `rebuild_everything_from_cache_and_map()` for the complete cache-and-map rebuild.

The lean source repository does not contain every production cache or dataset.
Do not claim a complete build succeeded unless the build-capable project or restored external data was actually used.

## Live feeds

The live-feed generator remains separate at `dbo99/brim-live-data-feeds`.
Treat the connection as an interface based on manifests, schemas, paths, timestamps, and fallback behavior.

## Working rules

Read `AGENTS.md`, `README.md`, `BUILD.md`, `DATA.md`, and `LIVE_DATA_FEEDS.md` before editing.
Make narrow, reviewable changes.
Do not run all preprocessors blindly.
Do not invent missing data or silently disable layers.
Parse changed R files and run the narrowest relevant validation.
State what was tested and what still requires the build-capable project.

## First Codex task

Begin with an architecture audit only. Do not modify files until the user gives a concrete development task.
