# PT2 External Overlay Curated Hover/Popup v1.4

Generated: 2026-05-12 07:29

## Files included

```text
03_functions/leaflet_tools_adddata_helpers.r
00_config/external_service_field_overrides.csv
08_docs/PT2_EXTERNAL_OVERLAY_CURATED_HOVER_POPUP_V1_4.md
```

## What changed

This patch keeps the richer popup behavior from v1.3, but makes the **hover** for
`DWR Recently Measured Groundwater Levels` simpler and faster to read.

Expected hover:

```text
<well ID>
<date>
gw depth: <value>
gw elev: <value>
grnd elev: <value>
```

Specifically:

```text
hover_fields:
WELL_NAME; LAST_MSMT_DATE; LAST_GSE_GWE; LAST_GWE; LAST_WLM_GSE

hover_aliases:
LAST_GSE_GWE=gw depth; LAST_GWE=gw elev; LAST_WLM_GSE=grnd elev

hover_bold_fields:
WELL_NAME; LAST_MSMT_DATE

hover_no_label_fields:
WELL_NAME; LAST_MSMT_DATE

hover_show_native_field_names:
FALSE
```

The popup still shows alias + native field name for technical review, such as:

```text
GW depth (LAST_GSE_GWE): 17.5
```

## Rebuild

```r
source("03_functions/leaflet_tools_adddata_helpers.r")
source("run_build_map.r")
build_final_map_only()
```

## Test first

```text
DWR Recently Measured Groundwater Levels
```
