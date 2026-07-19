# BRIM USGS Groundwater Maintenance Guide

## Layer roles

BRIM intentionally keeps two USGS groundwater layers with different jobs.

### Static USGS Wells

The static layer is the broad reference backbone. It is built into the BRIM map cache from the large USGS groundwater-well dataset. It is meant to answer: "what USGS groundwater sites exist in this area?"

It should change rarely. It includes inactive/historical wells and is clustered for statewide performance.

Current static display conventions:

- Fill color = most recent groundwater level used by the static display, in feet below ground/land surface.
- Bright green ring = the static well is included in the Ops Live groundwater recent-feed candidate list.
- Dashed outline = exact-coordinate co-located/nested well group.
- Popup `USGS source status` = source metadata only; it does not control the green ring.

### Ops Live groundwater

The Ops Live layer is the recent/analytical groundwater layer. It is downloaded by the browser from the BRIM-hosted GitHub GeoJSON feed. It is meant to answer: "which wells have recent groundwater-level measurements, and how do the latest values compare with history?"

Ops Live includes:

- Latest USGS groundwater field measurements for parameter 72019.
- A lookback-based candidate list, currently about two years.
- Historical water-year summaries and percentiles where RF029 history is available.
- Co-located/nested grouping at identical coordinates.
- Hover and popup mini plots.

## File responsibilities

### Local BRIM project

`02_preprocess/30_export_usgs_groundwater_live_inputs.R`

Builds the Ops Live candidate input CSV. It reads the static well backbone and supplements it with modern USGS Water Data API discovery for recent parameter 72019 field measurements. Run this when you want to refresh the candidate universe, such as quarterly, annually, or before an important release.

Output:

`brim-live-data-feeds/data/input/usgs_groundwater_latest_index_ca.csv`

`02_preprocess/31_build_usgs_groundwater_history_summary.r`

Builds/updates the local historical groundwater cache and compact history summary. This is local and incremental. It should not be a daily GitHub job.

Output:

`brim-live-data-feeds/data/input/usgs_groundwater_history_summary_ca.csv`

`05_map_build/02_cache_blocks/03_cache_stream_usgs_points.r`

Builds the map-ready static USGS well cache. It joins the live candidate CSV so static wells can show the green recent-feed ring and use newer Ops Live MR WL values where available.

Output:

`04_processed_data/cache/latest/usgs_wells_map.rds`

`03_functions/leaflet_ops_live_usgs_groundwater_helpers.r`

Draws and formats the browser-side Ops Live groundwater layer.

`03_functions/leaflet_layer_helpers.r`

Draws the cached static USGS Wells layer and other local layers.

### Live feed repository

`brim-live-data-feeds/scripts/build_usgs_groundwater_latest_ca.R`

GitHub Action script that refreshes the hosted latest groundwater GeoJSON for the current candidate list. It does not discover brand-new candidate wells by itself.

Inputs:

- `data/input/usgs_groundwater_latest_index_ca.csv`
- `data/input/usgs_groundwater_history_summary_ca.csv`

Outputs:

- `docs/data/usgs_groundwater_latest_ca.geojson`
- `docs/data/usgs_groundwater_latest_ca_summary.json`

## Update workflows

### Normal GitHub refresh

Use this when the candidate list is current and you only need latest values refreshed.

1. Push the current `data/input` CSVs to `brim-live-data-feeds` if they changed.
2. Run the GitHub Action: `Build USGS CA groundwater latest GeoJSON`.
3. No BRIM rebuild is required for the Ops Live layer to fetch the new hosted GeoJSON.

Important: an already-built BRIM HTML will fetch the updated Ops Live GeoJSON, but its embedded static layer will not automatically update green rings or static popup values until you rebuild BRIM locally.

### Periodic candidate/history refresh

Use this quarterly, annually, before a major release, or when you know new wells were activated.

From the PortaTreasure2 root:

```r
source("02_preprocess/30_export_usgs_groundwater_live_inputs.R")
source("02_preprocess/31_build_usgs_groundwater_history_summary.r")

local({
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd("brim-live-data-feeds")
  source("scripts/build_usgs_groundwater_latest_ca.R")
})
```

Then rebuild BRIM so the static layer gets current recent-feed rings and synced MR WL values:

```r
source("run_build_map.r")
rebuild_core_cache_and_map()
```

Commit/push these to the live-feed repo when changed:

- `data/input/usgs_groundwater_latest_index_ca.csv`
- `data/input/usgs_groundwater_history_summary_ca.csv`
- `scripts/build_usgs_groundwater_latest_ca.R` only when code changed
- `.github/workflows/build-usgs-groundwater-latest-ca.yml` only when workflow code changed

### Static 44k reference layer refresh

The large static reference layer should not be rebuilt casually from source. It is the broad site universe and is safe to leave stable for long periods.

Refresh or append to the static 44k layer only when you intentionally want to change the reference backbone, such as adding newly discovered sites that are not present in the static layer. That should be handled by a dedicated appender/QA workflow, not by replacing the whole dataset without review.

## Interpretation notes

- `USGS source status` is metadata and may be stale or administrative.
- `Recent feed: yes` means BRIM found the well in the Ops Live recent groundwater candidate list.
- The green ring follows `Recent feed: yes`, not source status.
- Water levels are depth to water in feet below ground/land surface. Larger positive numbers are deeper.
- Water-year means are simple averages of available measurements in each water year.
- Seasonal comparisons use prior measurements near the same water-day window, not meteorological seasons.
