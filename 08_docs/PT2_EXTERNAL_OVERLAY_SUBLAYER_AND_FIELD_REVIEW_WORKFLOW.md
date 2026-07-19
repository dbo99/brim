# PT2 External Overlay Sublayer + Field Review Workflow

## Where we left off

The field-curation QA script now works and correctly sees unique service URLs.
The next issue is that many MapServer services are service-level URLs. Those are
fine for visual overlays, but they often do not expose useful attribute fields
until a specific sublayer is selected.

## New staged workflow

### 1. Normalize agency names

Run:

```r
source("02_preprocess/25_normalize_pt2_external_overlay_catalog_agencies.r")
```

Review the dry-run list. If it only shows `CNRA -> DWR / CNRA` changes, set:

```r
DRY_RUN <- FALSE
```

and rerun.

Then rebuild the map so the Agency/source dropdown collapses CNRA into DWR / CNRA:

```r
source("run_build_map.r")
build_final_map_only()
```

### 2. Inventory MapServer sublayers

Run:

```r
source("02_preprocess/24_inventory_pt2_external_overlay_mapserver_sublayers.r")
```

Review:

```text
04_processed_data/qa/pt2_external_overlay_mapserver_sublayer_inventory_latest.csv
04_processed_data/qa/pt2_external_overlay_mapserver_sublayer_candidates_latest.csv
```

The candidate file contains catalog-shaped rows for sublayers that returned metadata.

### 3. Review fields one by one

Run:

```r
source("02_preprocess/26_review_pt2_external_overlay_fields_helpers.r")
```

Examples:

```r
pt2_review_queue()
pt2_show_overlay_fields(pattern = "groundwater")
pt2_show_overlay_fields(qa_id = "PT2EXT-0012")
```

This is the best point for a ChatGPT/user back-and-forth:

1. Show fields for one row.
2. User picks popup fields and styling fields.
3. Update the QA sheet or catalog row.
4. Move to the next row.

## Why one-by-one review makes sense

For hydrology/resource overlays, field meaning matters. A groundwater station
inventory can have well metadata and surface elevation fields but still lack an
actual current depth-to-water value. A biodiversity grid can have many numeric
scores but only a few are useful as default styling choices.

A one-by-one review is slower but gives much better popups and future styling.
