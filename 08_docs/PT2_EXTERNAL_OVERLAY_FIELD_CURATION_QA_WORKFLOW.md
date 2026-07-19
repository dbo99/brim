# PT2 External Overlay Field-Curation QA Workflow

## Why this exists

Testing whether an external overlay loads is not enough. Many services contain dozens
of fields, and a default popup can become noisy or even misleading. For example, a
groundwater station inventory may include station metadata and ground-surface elevation,
but not the actual latest depth-to-water value you hoped to map.

This workflow adds field curation before rows are promoted into the production catalog.

## Main output

```text
04_processed_data/qa/pt2_external_overlay_field_curation_test_sheet_latest.csv
```

## Key manual decisions

For each service row, decide:

```text
best_use
useful_for_visualization
popup_fields
hover_fields
default_label_field
style_field_candidates
default_style_field
style_units
style_legend_title
field_curation_notes
approved_for_catalog
```

## Field meanings

```text
popup_fields
```

Semicolon-separated field names that should appear in PT2 popups. Keep this short.

```text
hover_fields
```

Fields that would make good hover labels/tooltips.

```text
default_label_field
```

The best single name/ID field.

```text
out_fields
```

Fields PT2 should request from the service when current-view loading is used.
Usually this should be popup_fields + style_field_candidates + default_label_field.

```text
style_field_candidates
```

Numeric fields that may be useful for future color/radius styling.

```text
default_style_field
```

The first field PT2 should use if attribute-based styling is later enabled.

```text
useful_for_visualization
```

Suggested values:

```text
yes
limited
service-rendered
no
```

## Recommended statuses

```text
field_review_status:
  needs_field_review
  curated
  needs_better_service
  not_useful_for_attributes
  mapserver_visual_only
```

```text
test_status:
  untested
  metadata_ok_needs_manual_pt2_test
  works
  works_with_zoom_limit
  works_as_mapserver_only
  too_slow
  broken_url
  wrong_layer
  duplicate
  defer
  reject
```

## Workflow

1. Run:

```r
source("02_preprocess/21_build_pt2_external_overlay_field_curation_qa_sheet.r")
```

2. Open:

```text
04_processed_data/qa/pt2_external_overlay_field_curation_test_sheet_latest.csv
```

3. Review metadata fields and suggested fields.

4. Test important rows in PT2.

5. Edit curation columns.

6. Mark good rows:

```text
approved_for_catalog = TRUE
```

7. Dry-run promotion:

```r
source("02_preprocess/22_promote_pt2_external_overlay_catalog_approved_rows.r")
```

8. If counts look right, set:

```r
DRY_RUN <- FALSE
```

and rerun.

## Future map-helper implementation

A later PT2 helper update should consume:

```text
popup_fields
hover_fields
out_fields
style_field_candidates
default_style_field
```

to create cleaner popups and layer-specific styling controls.
