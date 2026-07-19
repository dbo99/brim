# PT2 External Overlay Curated Hover/Popup v1.1

Generated: 2026-05-12 06:09

## Files included

```text
03_functions/leaflet_tools_adddata_helpers.r
00_config/external_service_field_overrides.csv
08_docs/PT2_EXTERNAL_OVERLAY_CURATED_HOVER_POPUP_V1_1.md
```

## What this patch changes

Small, low-risk polish patch after curated hover/popup v1.

### 1. ArcGIS date formatting

Date-looking fields are now formatted from ArcGIS millisecond timestamps to:

```text
YYYY-MM-DD
```

This applies to curated hover, curated popup, and fallback popup display.

### 2. Better groundwater aliases

The DWR groundwater overrides now use readable labels such as:

```text
Last measurement date
GW elevation
Depth from GSE
Depth from RPE
Depth bin
Well depth
Last ground surface elev.
Last reference-point elev.
```

### 3. Recent groundwater hover updated

For:

```text
DWR Recently Measured Groundwater Levels
```

hover fields are now:

```text
WELL_NAME; LAST_MSMT_DATE; LAST_GWE; LAST_WLM_GSE; GSE_GWE_BIN
```

The popup still retains vertical-reference fields such as `LAST_GSE_GWE`, `LAST_WLM_GSE`, `LAST_WLM_RPE`, and related metadata.

### 4. Current-view snapshot note

Active external overlays loaded in current-view mode now show:

```text
Snapshot: pan/zoom, then remove and re-add to refresh the area.
```

No reload-button lifecycle logic was added in this patch.

### 5. Text selection polish

Popup and Tools panel text should be easier to select/copy. Buttons/dropdowns remain clickable.

## Test

After copying files, rebuild:

```r
source("03_functions/leaflet_tools_adddata_helpers.r")
source("run_build_map.r")
build_final_map_only()
```

Then test:

```text
DWR Recently Measured Groundwater Levels
DWR Groundwater Stations — inspectable features
```

Expected improvements:

```text
- Date values look like YYYY-MM-DD
- Hover labels are human-readable
- Popup labels are human-readable
- Static snapshot note appears under current-view active overlays
- Text in popup/panel is easier to select
```

## Deferred / wishlist

```text
- Add Reload current view button for current-view overlays
- Add color-ramp / attribute styling controls for FeatureServer and GeoJSON overlays
- Confirm exact DWR field definitions for LAST_GWE, LAST_WLM_GSE, LAST_GSE_GWE, LAST_WLM_RPE
```
