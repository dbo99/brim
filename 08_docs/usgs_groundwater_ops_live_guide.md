# USGS Groundwater Ops Live Layer Guide

_Last updated: 2026-06-16_

This guide documents the BRIM Ops Live **Groundwater | USGS** layer: what each script does, when to run it, and the safest update sequence for routine maintenance, BLM-land updates, and water-year turnover.

## Layer purpose

The USGS groundwater Ops Live layer is a screening layer for recent USGS groundwater-level field measurements in and near California. It is not the full static USGS wells inventory. It is intended for quick operational awareness and resource-review triage.

The map loads a static GitHub-hosted GeoJSON feed so BRIM users do not hit USGS directly when they open the map.

## Main files

### Local BRIM project

```text
02_preprocess/30_export_usgs_groundwater_live_inputs.R
02_preprocess/48_update_usgs_gw_blm_distance_fields.R
04_processed_data/rds/blm_managed_core_3310.rds
brim-live-data-feeds/data/input/usgs_groundwater_latest_index_ca.csv
```

### Live-feed repository

```text
brim-live-data-feeds/scripts/build_usgs_groundwater_latest_ca.R
brim-live-data-feeds/docs/data/usgs_groundwater_latest_ca.geojson
brim-live-data-feeds/docs/data/usgs_groundwater_latest_ca_summary.json
```

### Map UI

```text
03_functions/leaflet_ops_live_usgs_groundwater_helpers.r
```

## Data pipeline mental model

```text
30_export_usgs_groundwater_live_inputs.R
  Refreshes the USGS groundwater candidate/index CSV.
  This may fetch USGS Water Data API field measurements and monitoring-location metadata.
  Run only when the candidate universe needs to be refreshed.

48_update_usgs_gw_blm_distance_fields.R
  Adds/refreshes BLM relationship fields on the existing candidate CSV.
  This does NOT fetch USGS data.
  Run when the candidate CSV changes or when BLM managed lands change.

GitHub Action / build_usgs_groundwater_latest_ca.R
  Refreshes latest groundwater measurements and publishes the hosted GeoJSON/summary.

build_final_map_only()
  Rebuilds the BRIM HTML shell/UI. It does not refresh the hosted GitHub data feed.
```

## Fields added by the BLM distance updater

```text
on_blm_ca
  TRUE when the well coordinate intersects BLM-CA managed lands.

dist_to_blm_mi
  Approximate screening distance, in miles, from the well coordinate to BLM-CA managed lands.
  Zero for points on BLM-CA managed lands.

dist_to_blm_ft
  Same distance in feet.
```

Distances are calculated in EPSG:3310 using the processed BLM managed-lands geometry. The map should describe the value as an approximate screening distance because it depends on source-boundary date, BLM geometry processing, and well-coordinate precision.

## Canonical BLM distance source

Use the processed/projected managed-lands RDS:

```text
04_processed_data/rds/blm_managed_core_3310.rds
```

This should be a single dissolved `MULTIPOLYGON` in EPSG:3310. The WGS84 map-display cache is separate:

```text
04_processed_data/cache/latest/blm_core_map.rds
```

Do not calculate distance from the WGS84 map object. Use the EPSG:3310 RDS.

## Routine daily/weekly update

Usually the GitHub Action is enough. It rebuilds the hosted GeoJSON/summary from the existing candidate CSV.

After the Action runs, refresh the BRIM map in the browser. Rebuild the final map only if UI/helper code changed.

## Full candidate refresh

Run this when you want to rebuild the candidate list from BRIM local well caches plus recent USGS API discovery.

```r
local({
  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
  setwd("C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2")

  source("02_preprocess/30_export_usgs_groundwater_live_inputs.R")
})
```

Then run the BLM distance updater:

```r
local({
  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
  setwd("C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2")

  source("02_preprocess/48_update_usgs_gw_blm_distance_fields.R")
})
```

Commit/push:

```text
brim-live-data-feeds/data/input/usgs_groundwater_latest_index_ca.csv
```

Then run the groundwater GitHub Action.

## BLM managed-lands update

When the BLM managed-lands source changes:

1. Update the raw BLM source declaration in `00_config/config_source_files.r`.
2. Rerun the BLM/core map preprocessing so the processed products update.
3. Confirm this file has been refreshed:

```text
04_processed_data/rds/blm_managed_core_3310.rds
```

4. Rerun:

```r
local({
  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
  setwd("C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2")

  source("02_preprocess/48_update_usgs_gw_blm_distance_fields.R")
})
```

5. Commit/push the updated groundwater candidate CSV.
6. Run the groundwater GitHub Action.

This keeps the groundwater BLM-distance fields tied to the same processed BLM-CA Managed layer family shown in the map.

## Water-year turnover checklist

At the turn of the water year, check the layer but do not automatically rebuild everything unless needed.

1. Confirm the hosted feed still builds and the latest summary has a current build time.
2. Confirm the candidate lookback is still intended. Current default is 800 days.
3. Run the full candidate refresh if you want to discover newly active or recently added USGS groundwater wells.
4. Rerun the BLM distance updater after any full candidate refresh.
5. Run the GitHub Action and inspect the hosted summary.
6. Rebuild final BRIM map only if UI code changed.

Useful hosted summary check:

```js
fetch("https://dbo99.github.io/brim-live-data-feeds/data/usgs_groundwater_latest_ca_summary.json?cb=" + Date.now())
  .then(r => r.json())
  .then(j => console.log(j));
```

## Expected BLM-distance sanity counts from June 2026 build

These values are not permanent targets, but they are useful for detecting obvious pipeline failures:

```text
2,263 groundwater candidates
85 on BLM-CA managed lands
406 within 1 mile
1,047 within 5 miles
0 missing BLM distance
BLM source: 04_processed_data/rds/blm_managed_core_3310.rds
BLM source EPSG: 3310
```

## Common troubleshooting

### BLM filters return zero wells

Likely cause: the hosted GeoJSON does not yet contain BLM fields.

Check that both were committed/pushed before the Action ran:

```text
brim-live-data-feeds/data/input/usgs_groundwater_latest_index_ca.csv
brim-live-data-feeds/scripts/build_usgs_groundwater_latest_ca.R
```

Then inspect the hosted summary for BLM fields.

### 30_export fails during metadata bind_rows

The script should normalize monitoring-location metadata chunks before binding. If a future failure mentions columns such as `revision_created` with mixed datetime/character types, check that the script includes the `pt_normalize_monitoring_location_raw()` helper.

### BLM distance updater fails because the BLM RDS is missing

Do not silently use an old raw shapefile. Rebuild the core BLM preprocessing so this file exists and is current:

```text
04_processed_data/rds/blm_managed_core_3310.rds
```

### The map UI changed but data did not

Run:

```r
local({
  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
  setwd("C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2")

  source("run_build_map.r")
  build_final_map_only()
})
```

### The data changed but map UI did not

Rerun the GitHub Action and hard-refresh/reopen the map. You do not need to rebuild final BRIM HTML unless helper/UI code changed.
