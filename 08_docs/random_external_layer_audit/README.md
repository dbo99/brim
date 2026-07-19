# EL_001 random external layer audit

This folder supports the EL_001 audit-only workstream for possible new BRIM External/Ops layers.

## Purpose

EL_001 reads the random-adds handoff inventory, compares candidate layers to the current BRIM External Layers catalog, optionally probes public ArcGIS REST endpoints, and writes ranked audit outputs.

It does **not** modify:

- `00_config/external_service_catalog.csv`
- BRIM HTML outputs
- core caches
- local/static layer products
- GitHub-hosted live-feed products

## Inputs

Default input folder:

`08_docs/random_external_layer_audit/input/`

Included handoff files:

- `BRIM_random_adds_master_inventory.csv`
- `BRIM_random_adds_master_inventory.json`
- `audit_record_template.json`
- `BRIM_random_adds_batch_plan.md`
- `HANDOFF_README.md`

The script also reads the current BRIM catalog:

`00_config/external_service_catalog.csv`

## Script

`02_preprocess/60_audit_random_external_layer_candidates.R`

## Outputs

The script writes timestamped and `latest` outputs to:

`08_docs/random_external_layer_audit/output/`

Expected products:

- `random_external_layer_audit_<timestamp>.csv`
- `random_external_layer_ranked_plan_<timestamp>.csv`
- `random_external_layer_audit_<timestamp>.json`
- `random_external_layer_audit_<timestamp>.html`
- matching `*_latest.*` convenience copies

## Optional environment variables

- `BRIM_RANDOM_ADDS_LIVE_PROBE=TRUE/FALSE`
- `BRIM_RANDOM_ADDS_TIMEOUT_SEC=12`
- `BRIM_RANDOM_ADDS_MAX_LAYER_PROBE=10`
- `BRIM_RANDOM_ADDS_INVENTORY=<path to alternate inventory CSV>`

## Notes

The audit is intentionally conservative. Candidate rows that already appear to overlap the existing BRIM catalog should be treated as existing-row upgrades, not new layer adds. Rows without stable service endpoints should be endpoint-discovery work or compact/link-only candidates until validated.
