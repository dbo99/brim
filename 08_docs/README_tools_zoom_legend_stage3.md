# PT2 Tools / Add Data zoom + legend Stage 3

This bundle adds staged zoom-threshold and legend-link improvements.

## Files included

```text
03_functions/leaflet_tools_adddata_helpers.r
00_config/external_service_catalog.csv
02_preprocess/20_build_external_service_catalog.r
```

## Changes

- Renames catalog buttons: `Load into form` and `Add to map`.
- Keeps the FeatureServer SQL filter blank by default; examples remain as placeholder text only.
- Adds catalog fields for `min_zoom_live`, `min_zoom_current_view`, `legend_url`, and `legend_note`.
- Warns/blocks loading when a catalog layer is being loaded too far zoomed out.
- Adds legend links to the catalog preview and Active temporary layers list.
- Automatically derives MapServer legend URLs when possible.

## Rebuild

```r
source("run_build_map.r")
build_final_map_only()
```

No core-cache rebuild is needed.
