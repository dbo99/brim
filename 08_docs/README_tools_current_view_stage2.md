# PT2 Tools / Add Data current-view stage 2

This bundle adds a staged performance improvement for large FeatureServer layers.

## Files included

```text
03_functions/leaflet_tools_adddata_helpers.r
00_config/external_service_catalog.csv
02_preprocess/20_build_external_service_catalog.r
```

## What changed

- Adds a FeatureServer option: **load current map view only**.
- This is intended for large national FeatureServer layers such as BLM AIM, USFWS Critical Habitat, and CDFW ACE.
- Current-view loading:
  - uses the current Leaflet map extent as the spatial query box,
  - returns a static temporary GeoJSON-style layer,
  - respects optional SQL where clauses when provided,
  - reports the number of returned features.
- Keeps the original live FeatureServer mode available.
- Adds an optional FeatureServer SQL filter field.
- Adds catalog fields:
  - `default_load_mode`
  - `where_clause`
  - `large_layer_warning`
- Defaults likely large catalog FeatureServer layers to `current_view`.
- Makes the URL shown in the Active temporary layers list clickable.
- Leaves MapServer layers as visual overlays; MapServer popups remain out of scope for this stage.

## Recommended use

For large FeatureServer layers:

1. Zoom to the area of interest.
2. Choose the catalog layer.
3. Leave **load current map view only** checked.
4. Click **Add selected** or **Add layer**.
5. If you pan far away and need another area, remove the layer and load it again for the new view.

## Rebuild

```r
source("run_build_map.r")
build_final_map_only()
```

No core-cache rebuild is needed.
