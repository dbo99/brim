# RO_001 — Ops Live UI sandbox

## Purpose

RO_001 is a narrow sandbox-only patch for the BRIM Ops Live reorganization workstream. It creates a standalone HTML mockup so the Ops panel grouping, subgroup labels, and compact row-link vocabulary can be reviewed before production helper files are changed.

This patch does **not** change production BRIM behavior.

## Files added

- `05_map_build/dev_sandbox/RO_001_ops_live_panel_ui_sandbox.R`

## What it previews

- Row-link vocabulary: `rfrsh · lgnd · srce`
- Observations near the top of Ops Live
- Observations subgroups:
  - `Visual / cameras`
  - `Precipitation / radar / QPE`
  - `Flows / levels / moisture`
  - `Wind`
- Reservoirs directly after Observations and before Forecasts / Outlooks
- Cameras in Observations, not Fire
- Fire limited to fire perimeter layers for now
- Drought separated from CPC outlooks
- A possible replacement heading for `Weather Reference`: `Weather Offices / Boundaries`
- Satellite / Imagery kept as a separate image-heavy group

## What it does not do

- Does not source the real BRIM build.
- Does not edit `03_functions/` production helpers.
- Does not edit `00_config/external_service_catalog.csv`.
- Does not move any External Layers into Ops.
- Does not query live services.
- Does not verify hover, popup, current-view refresh, or Clear Ops behavior.

## Run command

Run from any R session:

```r
local({
  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
  setwd("C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2")
  source("05_map_build/dev_sandbox/RO_001_ops_live_panel_ui_sandbox.R")
})
```

The script writes a timestamped standalone HTML file under:

```text
06_output/html/dev_sandbox/
```

## Satellite / imagery inspection note

Current code inspection found no hard-coded imagery date in the active Ops satellite calls.

- NOAA GOES layers use current ImageServer endpoints:
  - `MERGEDGC_current/ImageServer`
  - `ABI13_current/ImageServer`
  - `ABI10_current/ImageServer`
- The ArcGIS export helper builds `/exportImage` requests using the current map bbox and map size, and appends a cache-busting `Date.now()` value.
- NASA MODIS Terra True Color uses the GIBS WMTS path with `default/default`, meaning GIBS controls the default/latest available date. If this appears stuck, a later patch could explicitly diagnose or set the WMTS time dimension.

## Suggested next patch

If the sandbox layout looks good, RO_002 should be a production helper patch that adds the compact row-link labels and refresh bridge support for Ops-promoted current-view catalog layers. RO_003 can then promote the cameras and CAL FIRE Recent Large Fire Perimeters with `primary_panel = both` for first QA.
