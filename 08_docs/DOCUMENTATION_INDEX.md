# BRIM documentation index and authority

## Authority order

1. Current merged code, registries, schemas, and focused tests.
2. Root controlling documents: `AGENTS.md`, `README.md`, `BUILD.md`, `DATA.md`, `CODEX_HANDOFF.md`.
3. `BRIM_DEVELOPMENT_ARCHITECTURE.md` and current feature/pipeline documents.
4. Accepted checkpoint and release-closeout records.
5. Historical handoffs/status files, audit outputs, research packages, rollback folders, and manual-patch archives.

## Document classes

### Control

Mandatory startup rules. Root only.

### Architecture

Durable cross-feature contracts under `08_docs/`.

### Operations

Backup, recovery, release, and maintenance procedures under `08_docs/operations/`.

### Integrations

Cross-repository and external-interface contracts under `08_docs/integrations/`.

### Features

User-facing feature behavior and interpretation under `08_docs/features/`.

### Pipelines

Execution, inputs, outputs, and QA instructions beside the owning pipeline.

### Checkpoints/releases

Accepted implementation evidence. Valid for the recorded phase and useful for regression requirements, but not universal current instructions.

### Audit/research/history

Discovery and historical evidence only. Never automatically controlling.

## Current canonical locations

- Agent rules: `/AGENTS.md`
- Orientation: `/README.md`
- Build/release: `/BUILD.md`
- Data governance: `/DATA.md`
- Codex template: `/CODEX_HANDOFF.md`
- Architecture: `/08_docs/BRIM_DEVELOPMENT_ARCHITECTURE.md`
- Descriptive-only 26-record layer catalog (six HUC, four heterogeneous proof records, and 16 diversity-selected records):
  `/08_docs/catalog/BRIM_LAYER_CATALOG.csv`
- Descriptive catalog parity test: `/qa/test_descriptive_layer_catalog.R`
- BRIM Guide architecture and onboarding boundary:
  `/08_docs/BRIM_DEVELOPMENT_ARCHITECTURE.md` under **BRIM Guide foundation** (accepted V4.1 shell and typography roles, sole upper-left entry, complete 270-item post-basemap A–Z layer/tool browse with a live derived full/subset status, the one-controller 33-record Resource Explorer with mutually exclusive green BRIM-linked / muted-blue Beyond / neutral All primary views in consistent user-facing order, exact visible relationship-subtype refinements, searchable Provider controls, visible compact high-value facets plus controlled Resource Type alone as the scalable control under `More filters`, machine IDs with build-derived labels for Resource Type, temporal character, and geographic scope, temporal character shown only as known selected-detail metadata and excluded from search/facets, normalized geography retained for search/detail with unknown scope omitted from public detail, granularity retained as editorial-only search/detail metadata, and temporal/geographic/named-geography/access-point-type/granularity/verification/priority facets deferred, weighted search, balanced desktop refinement/results/detail regions with two bounded independent content scrollers beneath a stationary Explorer header, one-time selected-row alignment with close restoration, a promoted exact canonical official-Resource action, selected-only detail, responsive single-pane behavior, and crash-safe teardown, a radio-style single-select Where-in-BRIM Product dimension plus zero-or-one Subject and Information Type groups combined by AND with removable chips and contextual Clear all, explicit controlled multi-tag subject taxonomy without path/group fallback, generic Layer-path-or-purpose / Tool-action-summary / Collection-scope presentation, the SMA overlay correctly presented as an External Layer, `Model / Simulation` Information Type, semantic-field search plus separate exact-path lookup, 84 source-backed rich Product records including the 60-record Local/External/Ops Live Wave 1, and 11 typed Quick Access destinations including exact Fire/USGS collections and the current curated `Water conveyance | BRIM mapped` layer, timing boundaries, mailto contact, seven maintained Methods, three verified Updates, and current-build Legacy Notes retirement)
- BRIM Guide source-backed Product enrichment registry:
  `/00_config/guide_product_enrichment.json` (stable-ID-keyed editorial content only; it cannot create Products or control map runtime behavior)
- BRIM Guide canonical authored Resource metadata registry:
  `/00_config/guide_resources.json` (one ordered schema-v3 dataset with 72 canonical Resources: 33 published/browser-visible and 39 staged; publication projection removes staged records before relationships, search, counts, facets, adaptation, and embedding; 34 staged records are publication-ready within R7C evidence while three subject-review and two taxonomy-blocked records remain held; staging adds no Product relationship or profile; controlled Resource Type, temporal character, and geographic scope are required descriptive metadata; granularity remains editorial-only; broad official access pages precede configured views where both are retained, including the California-relevant NOAA GOES Pacific Southwest and U.S. Pacific Coast access points; exact Product relationships remain solely authored in Product enrichment, and runtime authority remains elsewhere)
- BRIM Guide build-time inventory/profile owner:
  `/03_functions/leaflet_guide_helpers.r`
- BRIM Guide focused foundation test: `/qa/test_guide_foundation.R`
- BRIM Guide Resource registry contract test: `/qa/test_guide_resource_registry.R`
- BRIM Guide Resource Explorer browser-model contract test: `/qa/test_guide_resource_explorer.js`
- Backup/recovery: `/08_docs/operations/BACKUP_AND_RECOVERY.md`
- Live feeds: `/08_docs/integrations/LIVE_DATA_FEEDS.md`
- UIC feature: `/08_docs/features/UIC_AQUIFER_EXEMPTIONS.md`
- UIC pipeline: `/02_preprocess/67_uic_aquifer_exemptions_pipeline/README.md`
- Federal Wilderness feature: `/08_docs/features/FEDERAL_WILDERNESS.md`
- Federal Wilderness pipeline: `/02_preprocess/68_federal_wilderness_pipeline/README.md`
- National Monuments feature: `/08_docs/features/NATIONAL_MONUMENTS.md`
- National Monuments pipeline:
  `/02_preprocess/70_national_monuments_pipeline/README.md`
- California Desert NCL feature: `/08_docs/features/CA_DESERT_NCL.md`
- California Desert NCL pipeline:
  `/02_preprocess/71_desert_ncl_pipeline/README.md`
- National Monuments 20-record agency/style audit:
  `/qa/qa_local_reference_national_monument_agency_styles.R`
- Local Reference semantic labels:
  `/08_docs/features/LOCAL_REFERENCE_SEMANTIC_LABELS.md`
- Local display-geometry generalization and disclosure:
  `/08_docs/features/LOCAL_GEOMETRY_GENERALIZATION.md`
- Local display-geometry machine-readable inventory:
  `/08_docs/features/local_geometry_generalization_inventory.csv`

Dated `PT2_STATUS_*`, `PortaTreasure2_handoff_*`, checkpoint, audit, and rollback documents must be read as historical/contextual unless current code explicitly still implements their contracts.
