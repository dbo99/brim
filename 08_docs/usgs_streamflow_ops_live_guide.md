# USGS Streamflow Ops Live Guide

BRIM's `Streamflow | USGS | Ca` Ops Live layer uses a BRIM-hosted static GeoJSON feed created by the `brim-live-data-feeds` workflow. The browser downloads that hosted feed; it does not query USGS directly when BRIM opens.

## Main files

```text
02_preprocess/29_export_usgs_streamflow_live_inputs.R
02_preprocess/49_update_usgs_streamflow_blm_distance_fields.R
brim-live-data-feeds/scripts/build_usgs_streamflow_latest_ca.R
03_functions/leaflet_ops_live_usgs_streamflow_helpers.r
03_functions/leaflet_ops_live_legend_helpers.r
```

## Normal maintenance sequence

From the PortaTreasure2 project root:

```r
local({
  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
  setwd("C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2")

  source("02_preprocess/29_export_usgs_streamflow_live_inputs.R")
  source("02_preprocess/49_update_usgs_streamflow_blm_distance_fields.R")
})
```

Then commit/publish the updated files in `brim-live-data-feeds/data/input/` and rerun the streamflow feed workflow, or let the scheduled GitHub Action rebuild the hosted feed.

## BLM-distance fields

The streamflow BLM-distance updater writes these fields into:

```text
brim-live-data-feeds/data/input/usgs_streamgages_index_ca.csv
```

Fields:

```text
on_blm_ca
dist_to_blm_mi
dist_to_blm_ft
```

The canonical distance source is:

```text
04_processed_data/rds/blm_managed_core_3310.rds
```

This is the dissolved projected BLM-CA managed-lands geometry used for screening distance math. The updater fails loudly if this RDS is missing. It does not fall back to a shapefile.

Popup/filter wording should use:

```text
Approx. distance to BLM-CA managed lands
```

because distances are projected CA Albers calculations but remain screening-level due to source geometry, boundary dates, and gage-coordinate precision.

## Browser filters

The USGS streamflow Ops Live filter panel supports:

- latest flow min/max in cfs;
- on-BLM filter;
- maximum approximate distance to BLM-CA managed lands.

The BLM controls are disabled when the hosted GeoJSON does not yet contain distance fields. Flow filtering still works without those fields.

## Count wording

Use “streamgage site records” or “feed site records,” not simply “records,” so users do not confuse the station-index/feed rows with time-series observations.
