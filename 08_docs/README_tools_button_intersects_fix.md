# PT2 Tools / External Overlays button + intersects fix

This focused replacement changes only:

```text
03_functions/leaflet_tools_adddata_helpers.r
```

## Changes

- Renamed catalog button: Add to map -> Add selected overlay
- Updated catalog button helper text
- Renamed manual button: Add layer -> Add manual overlay
- Updated catalog-loaded status text
- Changed current-view query from within(bounds) to intersects(bounds)
- Updated current-view query start message
- Updated current-view query failure message
- Updated zero-feature current-view message
- Updated current-view catch-block message
- Updated max-layer limit status wording

## Why this matters

The catalog and manual buttons previously used very similar wording:

```text
Add to map
Add layer
```

The replacement makes their roles clearer:

```text
Load into form        = copy the selected catalog item into the editable/manual fields
Add selected overlay  = add the selected catalog item immediately
Add manual overlay    = add whatever is currently in the manual fields
```

The current-view FeatureServer query now uses `intersects(bounds)` instead of `within(bounds)`. This should work better for polygon services such as USFWS Critical Habitat because features often cross the current map extent without being fully contained by it.

## Rebuild

```r
source("run_build_map.r")
build_final_map_only()
```

No cache rebuild is needed.

## Quick test

1. Choose USFWS Critical Habitat or another large FeatureServer catalog row.
2. Click **Load into form** and confirm the manual fields populate.
3. With current-view mode checked, zoom into an area of interest and click **Add manual overlay**.
4. Also test **Add selected overlay** directly from the catalog.
5. Confirm Notes and HUC fill still appear.
