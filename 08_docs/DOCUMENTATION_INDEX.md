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
  `/08_docs/BRIM_DEVELOPMENT_ARCHITECTURE.md` under **BRIM Guide foundation** (accepted V4.1 shell and typography roles, complete 270-item post-basemap A–Z layer/tool browse, and the one-controller 200-record Resource Explorer with exact `In BRIM map` 23 / `Beyond the map` 177 / `All Resources` 200 primary views derived only from reviewed Resource map representation; restrained direct/selected/not-mapped statements; delivery and exact multiple-source lists as secondary detail; searchable Provider and compact high-value facets; a stationary outer Guide and Refine framework; independent desktop Provider, middle-results, and right-detail native scroll owners with selected-row promotion, detail-top reset, and result/focus restoration; narrow one-pane behavior; exact canonical official-Resource actions; weighted search; 84 source-backed rich Product records; 11 typed Quick Access destinations; seven maintained Methods; three verified Updates; and current-build Legacy Notes retirement)
- BRIM Guide source-backed Product enrichment registry:
  `/00_config/guide_product_enrichment.json` (stable-ID-keyed editorial content only; it contains no Product–Resource relationship authority and cannot create Products or control map runtime behavior)
- BRIM Guide sole Product–Resource relationship registry:
  `/00_config/guide_product_resource_relationships.json` (schema version 2 with exactly one record for every one of 270 compiled Products and every one of 205 canonical Resources; sole Product-link and Resource map-representation authority; the full Resource set resolves to 3 direct, 20 selected-products, 177 not-currently-mapped, and 5 not-yet-reviewed records while the 200 published Resources retain the public 3/20/177 split; R15B substitutes the two invalid target review records with reviewed not-currently-mapped broad SnowTrax and Santa Barbara County Real-Time Hydrology records, and R15C publishes the exact repaired 133-Resource target without a Product-link action, preserving 86 canonical links; delivery remains secondary; temporary R12A objects and adapter are absent; ordinary relationship additions remain declarative data-only changes)
- BRIM Guide canonical authored Resource metadata registry:
  `/00_config/guide_resources.json` (one ordered schema-v3 dataset with 205 canonical Resources: 200 published/browser-visible and five staged; R15B repairs the 25 R15A-reviewed endpoint actions plus the separate USBR canonical homepage host, removes the two invalid target records, and inserts accepted broad SnowTrax and Santa Barbara County Real-Time Hydrology replacements; R15C performs fresh bounded endpoint verification for all exact 133 canonical URLs and publishes the complete repaired target by changing only publication state, with that point-in-time evidence not establishing continuous monitoring; the rejected CDEC Reservoir Conditions, Napa monitoring-platform, iSnobal, and Santa Barbara map proposals survive only as exact configured access points on their canonical parent Resources; HTTPS remains the external-URL default, with only three exact Resource-ID/full-URL HTTP exceptions for TID WISKI, Kings River Water Association, and Orange County Hydrology, all user-initiated links rather than runtime services; future exceptions require explicit review; R10 published exactly 34 Wave-2 records by changing only publication state; publication projection removes staged records before public relationships, search, counts, facets, adaptation, and embedding, preserving the 23/177/200 public views; `resource_nasa_giovanni`, `resource_usgs_earthexplorer`, and `resource_usgs_water_data_apis` remain held for subject review, while `resource_nrcs_web_soil_survey` and `resource_usda_cropland_data_layer` remain held for taxonomy resolution; controlled Resource Type, temporal character, and geographic scope are required descriptive metadata; granularity remains editorial-only; broad official access pages precede configured views where both are retained, including the California-relevant NOAA GOES Pacific Southwest and U.S. Pacific Coast access points; Product relationships remain solely authored in `guide_product_resource_relationships.json`, and runtime authority remains elsewhere)
- BRIM Guide build-time inventory/profile owner:
  `/03_functions/leaflet_guide_helpers.r` (owns the shared HTTPS-default Resource URL validator and the exact three-entry Resource-ID/full-URL HTTP-only exception contract used by both registry loading and compiled Guide-bundle validation)
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
