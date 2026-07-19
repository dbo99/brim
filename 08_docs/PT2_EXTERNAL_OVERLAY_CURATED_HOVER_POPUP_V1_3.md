# PT2 External Overlay Curated Hover/Popup v1.3

Generated: 2026-05-12 07:08

## Files included

```text
03_functions/leaflet_tools_adddata_helpers.r
00_config/external_service_field_overrides.csv
08_docs/PT2_EXTERNAL_OVERLAY_CURATED_HOVER_POPUP_V1_3.md
```

## Changes

- Hover date formatting is hardened for ArcGIS epoch-millisecond values.
- Hover tooltip max width is increased to 420 px.
- `LAST_GSE_GWE` is labeled `GW depth`.
- For `DWR Recently Measured Groundwater Levels`, popup and hover labels show both alias and native field name, e.g.:
  `GW depth (LAST_GSE_GWE): 17.5`
- Native field names are shown in muted italics.

## Rebuild

```r
source("03_functions/leaflet_tools_adddata_helpers.r")
source("run_build_map.r")
build_final_map_only()
```

## Test

Start with:

```text
DWR Recently Measured Groundwater Levels
```

Expected:

```text
- Hover date appears as YYYY-MM-DD
- Hover is wider / less wrapped
- Label says GW depth, not Depth to GW from GSE
- Popup/hover shows native field name in italics next to alias for this layer
```
