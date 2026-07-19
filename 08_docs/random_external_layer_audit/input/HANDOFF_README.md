# BRIM Random Adds Handoff Package

Created: 2026-06-10

This package is for handing off the BRIM “random adds” layer inventory to another chat for service audit and patch planning.

## Files

- `BRIM_random_adds_ingestion_prompt.md` — copy/paste this into the receiving chat.
- `BRIM_random_adds_master_inventory.csv` — machine-readable master inventory.
- `BRIM_random_adds_master_inventory.json` — same inventory as JSON.
- `audit_record_template.json` — blank audit schema for each candidate after service inspection.
- `BRIM_random_adds_batch_plan.md` — suggested first-pass audit/build batches.

## Best identifier rule

For each row, the receiving chat should resolve the most actionable identifier in this order:

1. Exact ArcGIS REST layer URL, e.g. `/FeatureServer/0` or `/MapServer/3`.
2. Service root URL + layer IDs.
3. WMS/WMTS endpoint + layer name.
4. API endpoint.
5. Downloadable dataset URL.
6. Provider landing page.

Keep landing pages even when exact service URLs exist because they usually explain caveats, update cadence, and intended use.

BLM CA grazing allotments/pastures were intentionally removed because they are already handled elsewhere.
