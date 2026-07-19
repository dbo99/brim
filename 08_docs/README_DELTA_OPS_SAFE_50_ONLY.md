# Delta Ops safe 50 script only

Use this after manually restoring the two X2 RDS files from backup.

This patch does NOT touch the restored RDS files. It only replaces:

- `02_preprocess/50_build_delta_ops_x2_lookup.R`

and adds a safety check script:

- `05_map_build/dev_sandbox/check_delta_ops_x2_safety.R`

The replacement 50_ script only writes:

- `brim-live-data-feeds/data/input/x2_river_km_lookup.csv`
- optional sandbox copy, if the sandbox folder exists

It does not write:
- `04_processed_data/rds/x2_km_wgs84.rds`
- `04_processed_data/cache/latest/x2_km_map.rds`

Run safety check from PortaTreasure2 root:

```r
local({
  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
  setwd("C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2")
  source("05_map_build/dev_sandbox/check_delta_ops_x2_safety.R")
})
```
