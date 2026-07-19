# PT2 External Overlay Field Overrides System v1

Generated: 2026-05-11 19:18

## Files to copy into the project

```text
00_config/external_service_field_overrides.csv
02_preprocess/21_build_pt2_external_overlay_field_curation_qa_sheet.r
```

Optional documentation files are included under `08_docs/`.

## What this changes

This keeps field curation decisions in a small config file instead of manually
editing generated QA CSVs.

Source of truth:

```text
00_config/external_service_catalog.csv
00_config/external_service_field_overrides.csv
02_preprocess/21_build_pt2_external_overlay_field_curation_qa_sheet.r
```

Generated output:

```text
04_processed_data/qa/pt2_external_overlay_field_curation_test_sheet_latest.csv
```

## Current overrides included

This initial override file contains groundwater/well-related curation rows:

```text
DWR Well Completion Reports Index — inspectable features
DWR Groundwater Stations — inspectable features
DWR Recently Measured Groundwater Levels
Water Boards 2026 Aquifer Risk Map — Water Quality Risk
```

Main design decision:

```text
For point observation layers, hover/tooltips should include compact observation
vitals, such as well name/ID, latest measurement date, and key measurement values.
```

## How to run

```r
source("02_preprocess/21_build_pt2_external_overlay_field_curation_qa_sheet.r")
```

You should see messages like:

```text
Applied field-curation override rows: 4
```

## What not to do yet

Do not run the promotion script yet unless you explicitly decide which rows should
be approved into the production catalog.

This stage is about generating a better QA sheet and then pausing for map testing.
