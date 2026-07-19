# PT2 External Overlay Field-Curation QA v1

Generated: 2026-05-10 05:30

## Files included

```text
02_preprocess/21_build_pt2_external_overlay_field_curation_qa_sheet.r
02_preprocess/22_promote_pt2_external_overlay_catalog_approved_rows.r
04_processed_data/qa/pt2_external_overlay_field_curation_test_sheet_template.csv
08_docs/PT2_EXTERNAL_OVERLAY_FIELD_CURATION_QA_WORKFLOW.md
```

## What this adds

This replaces the simpler load/no-load QA concept with a richer field-curation workflow.

It helps decide:

```text
- Which fields belong in popups
- Which fields are useful for hovers/labels
- Which numeric fields are candidates for future styling
- Whether a service is only useful as a visual overlay
- Whether a better service is needed
```

## First run

```r
source("02_preprocess/21_build_pt2_external_overlay_field_curation_qa_sheet.r")
```

Then open:

```text
04_processed_data/qa/pt2_external_overlay_field_curation_test_sheet_latest.csv
```

## Note

This is a QA/catalog system update, not a map UI update. The current PT2 helper may
not yet consume the new popup/style fields. The point is to curate them now so the
future helper update can use them cleanly.
