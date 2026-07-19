BRIM Delta Ops sandbox starter

This is a parser/output sandbox only, not a BRIM patch.

Files:
- scripts/build_delta_ops_daily_summary_sandbox.R
- data/input/delta_ops_static_locations.csv

Run from the live-feed repo root after copying the files into their matching folders:

  Rscript scripts/build_delta_ops_daily_summary_sandbox.R

Expected outputs:
- docs/data/delta_ops_daily_summary.json
- docs/data/delta_ops_daily_summary_summary.json
- docs/data/delta_ops_daily_summary_features.geojson

Optional later input:
- data/input/x2_river_km_lookup.csv with columns: river_km, lon, lat

The parser skips reservoir-release values and keeps San Luis Reservoir total/SWP/CVP split only.
