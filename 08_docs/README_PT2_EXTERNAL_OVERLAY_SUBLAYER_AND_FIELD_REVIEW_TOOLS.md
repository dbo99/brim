# PT2 External Overlay Sublayer + Field Review Tools

Generated: 2026-05-11 17:02

## Files included

```text
02_preprocess/24_inventory_pt2_external_overlay_mapserver_sublayers.r
02_preprocess/25_normalize_pt2_external_overlay_catalog_agencies.r
02_preprocess/26_review_pt2_external_overlay_fields_helpers.r
08_docs/PT2_EXTERNAL_OVERLAY_SUBLAYER_AND_FIELD_REVIEW_WORKFLOW.md
```

## What this adds

- A MapServer sublayer inventory script
- A catalog agency/source normalization script
- Convenience functions for one-by-one field review

## Recommended order

```r
source("02_preprocess/25_normalize_pt2_external_overlay_catalog_agencies.r")
```

Review dry run, then set `DRY_RUN <- FALSE` and rerun if it looks right.

Then:

```r
source("02_preprocess/24_inventory_pt2_external_overlay_mapserver_sublayers.r")
```

Then:

```r
source("02_preprocess/26_review_pt2_external_overlay_fields_helpers.r")
pt2_review_queue()
pt2_show_overlay_fields(pattern = "groundwater")
```
