# PT2 External Overlay Curated Hover/Popup v1.2

Generated: 2026-05-12 06:40

## Files included

```text
03_functions/leaflet_tools_adddata_helpers.r
00_config/external_service_field_overrides.csv
08_docs/PT2_EXTERNAL_OVERLAY_CURATED_HOVER_POPUP_V1_2.md
```

## What this patch fixes

### 1. Hover dates

The hover/tooltip display now formats ArcGIS millisecond date fields as:

```text
YYYY-MM-DD
```

v1.1 fixed popup dates, but hover dates could still show raw millisecond timestamps.

### 2. Groundwater field interpretation

For `DWR Recently Measured Groundwater Levels`, the hover now uses:

```text
WELL_NAME; LAST_MSMT_DATE; LAST_GWE; LAST_GSE_GWE; LAST_WLM_GSE; GSE_GWE_BIN
```

Readable aliases are now:

```text
LAST_GWE       -> GW elevation
LAST_GSE_GWE   -> Depth to GW from GSE
LAST_WLM_GSE   -> GSE elevation
LAST_WLM_RPE   -> Reference-point elevation
GSE_GWE_BIN    -> Depth bin
```

Based on the sample arithmetic shown in the map, `LAST_GSE_GWE` appears to be the
native depth-to-groundwater value (`GSE - GWE`). `LAST_WLM_GSE` appears to be the
ground-surface elevation value, not depth.

### 3. Native vs PT2-derived note

Curated popups now include a small italic note:

```text
Field labels are PT2-friendly aliases for native service fields unless explicitly marked as PT2-derived.
```

No PT2-derived groundwater-depth field is currently being calculated.

## Still deferred

```text
- Reload current view button
- Attribute color ramp / styling dropdown
- Formal confirmation of DWR field definitions
```

## Rebuild

```r
source("03_functions/leaflet_tools_adddata_helpers.r")
source("run_build_map.r")
build_final_map_only()
```
