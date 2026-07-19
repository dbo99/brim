# PT2 External Overlay Curated Hover/Popup v1

Generated: 2026-05-11 23:32

## Files included

```text
03_functions/leaflet_tools_adddata_helpers.r
00_config/external_service_field_overrides.csv
08_docs/PT2_EXTERNAL_OVERLAY_CURATED_HOVER_POPUP_V1.md
```

## What this implements

The Tools / External Overlays helper now consumes curated field settings.

It reads:

```text
00_config/external_service_catalog.csv
00_config/external_service_field_overrides.csv
```

and embeds these catalog/override fields into the standalone HTML:

```text
hover_fields
popup_fields
popup_aliases
default_label_field
out_fields
style_field_candidates
default_style_field
field_curation_notes
```

## Behavior

For FeatureServer current-view and GeoJSON overlays:

- `hover_fields` become Leaflet tooltips.
- `popup_fields` control popup rows.
- `default_label_field` is used as a tooltip fallback.
- `out_fields` are used in FeatureServer current-view queries when possible.
- Generic fallback popups still work when no curated fields exist.

For live FeatureServer overlays:

- `fields` are passed to Esri Leaflet when curated out fields exist.
- Tooltip/popup binding uses the same curated field logic.

For MapServer overlays:

- Behavior remains visual-overlay only.
- MapServer attribute popups are still not implemented.

## Groundwater test

After rebuilding, test:

```text
DWR Recently Measured Groundwater Levels
DWR Groundwater Stations — inspectable features
```

Zoom in, use current-view loading, and hover points.

Expected DWR Recently Measured hover fields:

```text
WELL_NAME; LAST_MSMT_DATE; LAST_GWE; LAST_GSE_GWE; GSE_GWE_BIN
```

## Rebuild

```r
source("03_functions/leaflet_tools_adddata_helpers.r")
source("run_build_map.r")
build_final_map_only()
```

No core-cache rebuild is needed.
