# PortaTreasure2 water district labels v1

This bundle adds a separate high-zoom label layer for water districts.

## Files replaced

- `00_config/config_labels.r`
- `03_functions/leaflet_label_helpers.r`
- `05_map_build/05_build_label_cache.r`

## What changed

- Adds `Labels – Water Districts` to the Labels section of the main Leaflet TOC.
- Builds water district labels from the cached `water_districts_map.rds` layer.
- Uses the cleaned `agency_display` field for label text.
- Keeps the water district polygon layer unchanged.
- Uses clustered label-only markers for performance.
- Makes water district labels effectively high-zoom only by keeping them in visually hidden clusters until zoom 12.
- Styles water district labels slightly smaller than the default label text.

## Required existing cache

This expects the current new-static-layer work to have already created:

- `04_processed_data/cache/latest/water_districts_map.rds`

If that file does not exist yet, first run:

```r
source("run_build_map.r")
refresh_new_static_water_layers_and_map()
```

## Normal run after copying these files

Because this changes label configuration and the label cache builder, run:

```r
source("run_build_map.r")
rebuild_label_cache_and_map()
```

If the water-district core cache is stale or missing, run:

```r
source("run_build_map.r")
rebuild_core_cache_and_map()
rebuild_label_cache_and_map()
```

## Test

1. Open the new HTML.
2. Turn on `Reference – Water Districts` if desired.
3. Turn on `Labels – Water Districts`.
4. Zoom in to about zoom 12 or higher.
5. Confirm water district names appear and are useful where polygon hover/click is difficult.
