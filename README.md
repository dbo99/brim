# BRIM

BRIM is a standalone Leaflet web map built primarily in R for water-resource screening, land/reference review, external web services, and operational situational awareness.

This repository is the lean private source edition. It contains authored code, configuration, tests, curated small inputs, and documentation. Large raw/processed data, caches, realistic HTML builds, and production artifacts are external by design.

## Authority

Merged `main` in this repository is the authored source authority.

- `BRIM_v0.38_source_repo` — authoring and Git.
- `BRIM_v0.38_codex_ship` — isolated build-capable integration/testing.
- `BRIM_v0.38` — full production workspace and deployed artifacts.

See `AGENTS.md` for mandatory workflow and safety rules.

## Start here

1. `AGENTS.md` — mandatory agent/developer rules.
2. `BUILD.md` — build decision tree and preprocessor/release safety.
3. `DATA.md` — data authority, identifiers, provenance, geometry, and external products.
4. `08_docs/BRIM_DEVELOPMENT_ARCHITECTURE.md` — durable application/UI architecture.
5. `08_docs/DOCUMENTATION_INDEX.md` — feature, pipeline, checkpoint, audit, and historical documents.
6. `CODEX_HANDOFF.md` — reusable startup contract for a fresh Codex thread.

## Main entry point

```r
source("run_build_map.r")
```

Do not choose a build command until the decision and preprocessor gates in `BUILD.md` have been applied.

## Repository map

- `00_config/` — source registries, layer registries, controlled tokens, and compact curated inputs.
- `02_preprocess/` — focused data acquisition/processing/audit workflows; never a default batch.
- `03_functions/` — shared R, Leaflet, and browser helpers/controllers.
- `05_map_build/` — cache and map assembly.
- `08_docs/` — architecture, feature, integration, operations, checkpoint, and historical documents.
- `qa/` — focused tests, fixtures, and compact contract artifacts.
- `sample_data/` — non-authoritative fixtures only.

## Related repository

Live-feed generation is maintained separately at `dbo99/brim-live-data-feeds`. See `08_docs/integrations/LIVE_DATA_FEEDS.md`.

This repository is private project source and has no public-use license.
