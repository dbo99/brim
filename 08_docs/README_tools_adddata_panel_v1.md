# PT2 Tools / Add Data panel v1

This bundle adds the left-side **Tools / Add Data** drawer.

## Files included

- `00_config/config_map_display.r`
- `03_functions/leaflet_tools_adddata_helpers.r`
- `05_map_build/04_build_portatreasure2_core_map.r`

`leaflet_core_helpers.r` is not replaced by this bundle because the current file already contains the TOC flash fix and does not need changes for this feature.

## What it adds

- Distance measurement
- Area measurement
- Clear measurements
- Built-in BLM CA Land Status / Surface Management Agency overlay toggle
- Opacity slider for the BLM SMA overlay
- Temporary external GIS layers, max 3 at a time:
  - ArcGIS FeatureServer layer URL
  - ArcGIS MapServer URL or layer endpoint
  - GeoJSON URL
- Optional popups for user-added layers
- Custom layer list with show/hide and remove buttons
- Full-page GIS source-links modal

## Build command

Run from the project root:

```r
source("run_build_map.r")
build_final_map_only()
```

No cache rebuild should be needed.

## Notes

The external GIS-service functionality is browser-side. Some services may fail from
a standalone local HTML file because of CORS, authentication, service limits, or
agency computer restrictions. That is expected; the panel reports failures in
the status area.

The BLM SMA overlay and ArcGIS FeatureServer/MapServer layers require browser
access to the Esri Leaflet library from unpkg.com at runtime. Measurement tools
and the source-links modal do not require Esri Leaflet.
