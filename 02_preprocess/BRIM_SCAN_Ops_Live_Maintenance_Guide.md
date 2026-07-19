# BRIM SCAN Ops Live maintenance guide

This guide describes how to update and maintain the BRIM **SCAN Soil Moisture Ops Live** layer.

The key idea is that SCAN has two different update workflows:

1. **Current/latest update** — refreshes the latest soil-moisture values and the current-water-year trace.
2. **Historical-context update** — refreshes the percentile ribbons and monthly context used in popup plots.

Do not run the historical-context workflow every time you want fresh current values. It is mainly for annual maintenance, station-list changes, or changes to the statistical logic.

---

## 1. What files the SCAN layer uses

### 1.1 Current/latest files

These are used for the latest values, map symbols, hover text, and the current-water-year black line.

Local GitHub repo location:

```text
brim-live-data-feeds/docs/data/
```

Files:

```text
scan_soil_moisture_latest.geojson
scan_soil_moisture_latest_summary.json
scan_soil_moisture_current_wy_trace.csv
scan_soil_moisture_current_wy_trace_summary.json
scan_depth_style.csv
```

These are the files that normally change during routine live/latest updates.

### 1.2 Historical-context files

These are used for the popup plots.

Local GitHub repo source/input copies:

```text
brim-live-data-feeds/data/input/
```

Files:

```text
scan_sms_waterday_percentiles.csv
scan_sms_monthly_context.csv
```

Published/browser copies:

```text
brim-live-data-feeds/docs/data/
```

Files:

```text
scan_sms_waterday_percentiles.csv
scan_sms_monthly_context.csv
```

Keep both copies. The `data/input/` copies are the source/input copies for the live-feed repo. The `docs/data/` copies are what BRIM fetches through GitHub Pages.

### 1.3 GitHub Pages path reminder

A file stored locally in:

```text
brim-live-data-feeds/docs/data/scan_depth_style.csv
```

is fetched by BRIM as:

```text
https://dbo99.github.io/brim-live-data-feeds/data/scan_depth_style.csv
```

That is normal. The public URL does not include `docs`.

---

## 2. Routine update: refresh current/latest SCAN values

Use this when you want the map to show the latest reported SCAN values and current-water-year trace.

This does **not** update the historical percentile ribbons or monthly context.

### Step 1 — Trigger the SCAN GitHub Action

Recommended method:

1. Open the `brim-live-data-feeds` GitHub repository.
2. Go to **Actions**.
3. Open the SCAN soil-moisture workflow.
   - The workflow name should be the one for building SCAN soil-moisture latest/current data.
   - If the exact name changes, use the workflow that references SCAN soil moisture or `build_scan_soil_moisture_latest.R`.
4. Click **Run workflow**.
5. Wait for the workflow to finish successfully.

If the workflow runs correctly, it should update the current/latest files in:

```text
docs/data/
```

### Step 2 — Check the public URLs

From the BRIM root in R:

```r
scan_public_urls <- c(
  latest = "https://dbo99.github.io/brim-live-data-feeds/data/scan_soil_moisture_latest.geojson",
  trace = "https://dbo99.github.io/brim-live-data-feeds/data/scan_soil_moisture_current_wy_trace.csv",
  style = "https://dbo99.github.io/brim-live-data-feeds/data/scan_depth_style.csv",
  latest_summary = "https://dbo99.github.io/brim-live-data-feeds/data/scan_soil_moisture_latest_summary.json",
  trace_summary = "https://dbo99.github.io/brim-live-data-feeds/data/scan_soil_moisture_current_wy_trace_summary.json"
)

check_public_scan_url <- function(u) {
  x <- tryCatch(
    readLines(u, n = 2, warn = FALSE),
    error = function(e) paste("ERROR:", conditionMessage(e))
  )
  data.frame(
    url = u,
    first_line = substr(x[1], 1, 120),
    stringsAsFactors = FALSE
  )
}

do.call(rbind, lapply(scan_public_urls, check_public_scan_url))
```

If any row shows an error or a GitHub 404 page, that file is not published correctly.

### Step 3 — Reload BRIM

You usually do **not** need to rebuild the BRIM HTML after a latest/current update. The existing BRIM HTML fetches these live-feed files from GitHub Pages.

Open or reload the BRIM HTML and turn on:

```text
Soil Moisture: SCAN Latest
```

Check that the values and feed build time look current.

---

## 3. Local fallback: refresh current/latest files without GitHub Actions

Use this only if the GitHub Action is unavailable or if you intentionally want to build the latest feed locally.

### Step 1 — Run the local latest-feed builder

From the BRIM root in R:

```r
local({
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  setwd("brim-live-data-feeds")
  source("scripts/build_scan_soil_moisture_latest.R")
})
```

### Step 2 — Confirm the local output files exist

From the BRIM root in R:

```r
scan_latest_docs_files <- c(
  "brim-live-data-feeds/docs/data/scan_soil_moisture_latest.geojson",
  "brim-live-data-feeds/docs/data/scan_soil_moisture_latest_summary.json",
  "brim-live-data-feeds/docs/data/scan_soil_moisture_current_wy_trace.csv",
  "brim-live-data-feeds/docs/data/scan_soil_moisture_current_wy_trace_summary.json",
  "brim-live-data-feeds/docs/data/scan_depth_style.csv"
)

file.info(scan_latest_docs_files)[, c("size", "mtime")]
```

All five files should have real sizes and recent modification times.

### Step 3 — Manually upload the five current/latest files

In the GitHub web interface, upload/replace these files under:

```text
brim-live-data-feeds/docs/data/
```

Files to upload:

```text
scan_soil_moisture_latest.geojson
scan_soil_moisture_latest_summary.json
scan_soil_moisture_current_wy_trace.csv
scan_soil_moisture_current_wy_trace_summary.json
scan_depth_style.csv
```

Commit message suggestion:

```text
Update SCAN latest soil moisture feed
```

### Step 4 — Check public URLs and reload BRIM

Use the public URL check from Section 2.2, then reload the existing BRIM HTML.

---

## 4. Annual update: refresh historical percentile/context files

Use this after the water year ends, preferably **mid-October to early November**.

Purpose:

- Include the just-completed water year in the historical context.
- Exclude the new current water year from percentile calculations.
- Refresh Plot A monthly context and Plot B daily percentile ribbons.

Do not use this as the routine daily/latest refresh.

### Step 1 — Run the historical-context wrapper

From the BRIM root in R:

```r
source("02_preprocess/37_update_scan_ops_live_historical_context.r")
```

This runs the context workflow:

```text
34_build_scan_history_summary.r
35_export_scan_monthly_context_for_ops_live.r
36_publish_scan_context_to_docs_data.r
```

Expected outputs include:

```text
04_processed_data/qa/scan_ops_live_context_update_manifest.csv
04_processed_data/qa/scan_ops_live_context_update_files.csv
```

### Step 2 — Inspect the manifest

```r
readr::read_csv(
  "04_processed_data/qa/scan_ops_live_context_update_manifest.csv",
  show_col_types = FALSE
) |>
  print(n = Inf)

readr::read_csv(
  "04_processed_data/qa/scan_ops_live_context_update_files.csv",
  show_col_types = FALSE
) |>
  print(n = Inf)
```

Look for these ideas in the console/manifests:

```text
Current water year excluded from historical context: WY<current>
Completed water years included through: WY<previous>
```

After October 1, the current water year should roll over automatically. For example, after October 1, 2026:

```text
Current water year excluded: WY2027
Completed water years included through: WY2026
```

### Step 3 — Confirm the four context files exist locally

```r
scan_context_files <- c(
  "brim-live-data-feeds/data/input/scan_sms_waterday_percentiles.csv",
  "brim-live-data-feeds/data/input/scan_sms_monthly_context.csv",
  "brim-live-data-feeds/docs/data/scan_sms_waterday_percentiles.csv",
  "brim-live-data-feeds/docs/data/scan_sms_monthly_context.csv"
)

file.info(scan_context_files)[, c("size", "mtime")]
```

All four files should exist.

### Step 4 — Manually upload the four context files

Only do this if you intend to publish the regenerated historical context.

Upload/replace the source/input copies:

```text
brim-live-data-feeds/data/input/scan_sms_waterday_percentiles.csv
brim-live-data-feeds/data/input/scan_sms_monthly_context.csv
```

Upload/replace the published/browser copies:

```text
brim-live-data-feeds/docs/data/scan_sms_waterday_percentiles.csv
brim-live-data-feeds/docs/data/scan_sms_monthly_context.csv
```

Commit message suggestion:

```text
Update SCAN soil moisture historical context
```

### Step 5 — Check public URLs

From the BRIM root in R:

```r
scan_context_public_urls <- c(
  daily_pct = "https://dbo99.github.io/brim-live-data-feeds/data/scan_sms_waterday_percentiles.csv",
  monthly = "https://dbo99.github.io/brim-live-data-feeds/data/scan_sms_monthly_context.csv"
)

check_public_scan_url <- function(u) {
  x <- tryCatch(
    readLines(u, n = 2, warn = FALSE),
    error = function(e) paste("ERROR:", conditionMessage(e))
  )
  data.frame(
    url = u,
    first_line = substr(x[1], 1, 120),
    stringsAsFactors = FALSE
  )
}

do.call(rbind, lapply(scan_context_public_urls, check_public_scan_url))
```

### Step 6 — Reload BRIM

You usually do **not** need to rebuild the BRIM HTML after replacing historical-context CSVs. Reload the existing BRIM HTML and check a few SCAN popups.

---

## 5. After adding, removing, or changing SCAN stations

Use this when the SCAN station list changes, not for normal current/latest updates.

### Step 1 — Re-export the SCAN live station index

From the BRIM root in R:

```r
source("02_preprocess/33_export_scan_live_inputs.r")
```

This updates:

```text
brim-live-data-feeds/data/input/scan_station_index.csv
```

### Step 2 — Manually upload the station index

Upload/replace this file in GitHub:

```text
brim-live-data-feeds/data/input/scan_station_index.csv
```

Commit message suggestion:

```text
Update SCAN live station index
```

### Step 3 — Refresh historical context

Run:

```r
source("02_preprocess/37_update_scan_ops_live_historical_context.r")
```

Then upload the four context files listed in Section 4.4.

### Step 4 — Refresh current/latest feed

Trigger the SCAN GitHub Action, or use the local fallback in Section 3.

### Step 5 — Rebuild BRIM only if the static station/reference layer changed

If only the Ops Live data changed, reload the existing BRIM HTML.

If the static station/reference layer changed, rebuild the BRIM map:

```r
source("run_build_map.r")
build_final_map_only()
```

If the cache itself changed, use the appropriate cache rebuild function from `run_build_map.r`.

---

## 6. Troubleshooting: popup plots disappear

The most common cause is that one or more required files are missing from GitHub Pages.

### Step 1 — Check all five public plot/layer URLs

```r
scan_public_urls <- c(
  latest = "https://dbo99.github.io/brim-live-data-feeds/data/scan_soil_moisture_latest.geojson",
  trace = "https://dbo99.github.io/brim-live-data-feeds/data/scan_soil_moisture_current_wy_trace.csv",
  style = "https://dbo99.github.io/brim-live-data-feeds/data/scan_depth_style.csv",
  daily_pct = "https://dbo99.github.io/brim-live-data-feeds/data/scan_sms_waterday_percentiles.csv",
  monthly = "https://dbo99.github.io/brim-live-data-feeds/data/scan_sms_monthly_context.csv"
)

check_public_scan_url <- function(u) {
  x <- tryCatch(
    readLines(u, n = 2, warn = FALSE),
    error = function(e) paste("ERROR:", conditionMessage(e))
  )
  data.frame(
    url = u,
    first_line = substr(x[1], 1, 120),
    stringsAsFactors = FALSE
  )
}

do.call(rbind, lapply(scan_public_urls, check_public_scan_url))
```

If a row returns `ERROR` or a GitHub 404 page, upload that missing file to:

```text
brim-live-data-feeds/docs/data/
```

### Step 2 — If points draw but plots do not

Check these first:

```text
scan_soil_moisture_current_wy_trace.csv
scan_depth_style.csv
scan_sms_waterday_percentiles.csv
scan_sms_monthly_context.csv
```

The points can draw from the latest GeoJSON even when plot CSVs are missing.

### Step 3 — If values look stale

Refresh the current/latest feed using Section 2 or Section 3.

### Step 4 — If ribbons look stale after water-year rollover

Run the annual historical-context update in Section 4.

---

## 7. Quick reference

### For fresh current values

Preferred method:

```text
GitHub Actions → SCAN soil moisture latest workflow → Run workflow
```

Then reload BRIM.

### For local latest-value fallback

```r
local({
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  setwd("brim-live-data-feeds")
  source("scripts/build_scan_soil_moisture_latest.R")
})
```

Then manually upload the five current/latest files to `docs/data/`.

### For annual historical context

```r
source("02_preprocess/37_update_scan_ops_live_historical_context.r")
```

Then manually upload the four context files to `data/input/` and `docs/data/`.

### For missing popup plots

Check public URLs, especially:

```text
scan_soil_moisture_current_wy_trace.csv
scan_depth_style.csv
scan_sms_waterday_percentiles.csv
scan_sms_monthly_context.csv
```

---

## 8. Recommended schedule

| Task | Frequency | Main script/action | Upload needed? |
|---|---:|---|---|
| Refresh current/latest SCAN values | Daily or as needed | GitHub Action preferred | No, if GitHub Action works |
| Local latest-feed fallback | As needed | `scripts/build_scan_soil_moisture_latest.R` | Yes, five `docs/data` files |
| Historical context update | Annually, mid-Oct to early Nov | `37_update_scan_ops_live_historical_context.r` | Yes, four context files if publishing |
| Station-index update | When station list changes | `33_export_scan_live_inputs.r` | Yes, station index plus refreshed outputs |
| BRIM HTML rebuild | Only after code/static-map changes | `build_final_map_only()` | No live-data upload by itself |
