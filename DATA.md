# BRIM data guide

## Data tiers

1. **Tracked source and curated inputs** — code, configuration, reviewed
   decision tables, small provenance files, and representative fixtures.
2. **External production inputs** — large raw rasters, shapefiles, GDBs,
   GeoPackages, and downloaded observations.
3. **External derived products** — production RDS/GPKG files and map caches.
4. **Generated products** — final HTML, QA figures, sandbox maps, and logs.

The complete exclusions are listed in `EXTERNAL_DATA_MANIFEST.csv`.

## Curated SWRCB correction input

The small SWRCB BLM water-right correction CSV is tracked because the core
cache build stops rather than silently dropping its official membership and
face-value corrections.

## Updating BLM land status

For a replacement BLM-California land-status shapefile:

1. Place the full shapefile family under `01_raw_data/blm/` in a production
   or reproducibility checkout.
2. Update `SRC$blm_fedlands` in `00_config/config_source_files.r`.
3. Run `02_preprocess/01_blm_managed_and_held.r`.
4. Refresh every downstream BLM-derived percentage, distance, clipping, and
   conveyance field before rebuilding the map.

The current convenience helper refreshes the principal BLM map layer but is
not yet a complete orchestration runner for every downstream dependency.

## Data policy

Do not commit production geospatial inputs or generated map caches to normal
Git history. Keep them in the active/reproducibility project, documented
external storage, or a controlled data-release mechanism.
