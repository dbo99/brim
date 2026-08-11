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

Dated `PT2_STATUS_*`, `PortaTreasure2_handoff_*`, checkpoint, audit, and rollback documents must be read as historical/contextual unless current code explicitly still implements their contracts.
