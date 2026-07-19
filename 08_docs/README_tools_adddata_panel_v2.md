# PT2 Tools / Add Data Panel v2

This bundle replaces only:

```text
03_functions/leaflet_tools_adddata_helpers.r
```

## Changes

- Moves the collapsed Tools / Add Data tab lower on the left side so it does not overlap the Notes button or HUC fill control.
- Lowers the Tools panel z-index so the HUC dropdown can stay above it when controls are close together.
- Reorders the panel:
  1. Measure
  2. Add temporary GIS layer
  3. Active temporary layers
  4. Built-in context overlay
  5. Find service links
- Adds **Auto-detect from URL** as the default service type.
- Detects:
  - `/FeatureServer/0`, `/FeatureServer/1`, etc. as ArcGIS FeatureServer layers
  - `/MapServer` or `/MapServer/3` as ArcGIS MapServer layers
  - `.geojson` or `f=geojson` as GeoJSON
  - ArcGIS Hub / portal dataset pages as non-direct URLs with a clearer warning
- Adds clearer helper text explaining that `/datasets/...` pages are usually not direct service endpoints.
- Expands the source-links modal with a “How to recognize usable URLs” section.

## Rebuild

```r
source("run_build_map.r")
build_final_map_only()
```

No cache rebuild is needed.

## Test

After the HTML opens:

1. Confirm the Tools / Add Data tab is below Notes and HUC fill.
2. Confirm Notes and HUC fill still appear.
3. Try measurement tools.
4. Paste a known direct `/FeatureServer/0` or `/MapServer/0` endpoint and use Auto-detect.
5. Paste an ArcGIS Hub `/datasets/...` page and confirm the panel gives a useful warning rather than silently failing.
