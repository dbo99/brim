# PT2 Tools / External Overlays local-status and title update

This focused bundle replaces:

```text
03_functions/leaflet_tools_adddata_helpers.r
03_functions/leaflet_core_helpers.r
```

## Changes in leaflet_tools_adddata_helpers.r

- Renames the left-side tab:
  - `Tools / Add Data` -> `Tools / External Overlays`
- Renames panel sections:
  - `Choose from starter catalog` -> `Choose from overlay catalog`
  - `Add temporary GIS layer manually` -> `Add external overlay manually`
  - `Active temporary layers` -> `Active external overlays`
- Keeps clearer buttons:
  - `Load into form`
  - `Add selected overlay`
  - `Add manual overlay`
- Adds local action-note boxes directly below:
  - the catalog Add selected overlay button
  - the manual Add manual overlay button
- Zoom-threshold warnings now appear beside the relevant button instead of only at the bottom of the panel.
- Adds a local measurement status box directly below the measurement buttons.
- Distance/area measurement results now appear near the measurement controls.
- Keeps the bottom status line for general messages.
- Preserves the current-view FeatureServer `intersects(bounds)` behavior.

## Changes in leaflet_core_helpers.r

- Renames the main right-side layer-control header:
  - `Layers` -> `CASO HydroPortal layers`

## Rebuild

```r
source("run_build_map.r")
build_final_map_only()
```

No core-cache rebuild is needed.

## Smoke test

After rebuilding, check:

1. Right-side TOC title says `CASO HydroPortal layers`.
2. Left-side tab says `Tools / External Overlays`.
3. Notes and HUC fill controls still appear.
4. Distance/area results appear below the measurement buttons.
5. A too-far-zoomed external overlay warning appears below the relevant Add button.
