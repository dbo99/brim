# BRIM data guide

## Data tiers

1. **Tracked source and curated inputs** — code, configuration, reviewed
   decision tables, small provenance files, and representative fixtures.
2. **External production inputs** — large raw rasters, shapefiles, GDBs,
   GeoPackages, and downloaded observations.
3. **External derived products** — production RDS/GPKG files and map caches.
4. **Generated products** — final HTML, QA figures, sandbox maps, and logs.

The complete exclusions are listed in `EXTERNAL_DATA_MANIFEST.csv`.

## Major water-supply basin external geometry

Phase B2 tracks only reviewed manifests and configuration. The official
`CBRFC_Basins` shapefile family, `CBRFC_Outlets.zip`, original WBD HUC2 14/15
archives, retained California RDS inputs, generated 23-feature RDS/cache files,
QA CSVs, and rendered PNGs remain external. Their expected sizes, hashes,
feature counts, schemas, CRS, URLs, and timestamps are recorded in
`00_config/major_water_supply_basin_source_manifest.csv` and the focused QA
provenance. Do not commit these raw or generated geospatial products. The large
CBRFC zones archive is intentionally not a dependency.

## UIC aquifer-exemption products

The controlled UIC pipeline writes generated data only to ignored tiers:

- `01_raw_data/uic_aquifer_exemptions/raw_snapshots/<snapshot_id>/` —
  timestamped immutable raw source snapshots;
- `04_processed_data/uic_aquifer_exemptions/candidate/<candidate_id>/` —
  full standardized, map-ready, label, metadata, comparison, and QA products;
- `04_processed_data/uic_aquifer_exemptions/approved/` — an explicitly
  approved comparison baseline for the controlled research workflow;
- `04_processed_data/uic_aquifer_exemptions/archive/` — prior approved
  products retained for rollback;
- `04_processed_data/uic_aquifer_exemptions/checks/` and `qa/` — current
  service status, difference reports, and standalone review maps.

The repository tracks only the pipeline code and configuration. Do not commit
downloaded UIC GeoJSON, RDS products, approval archives, or sandbox/final HTML.
The normal BRIM build reads none of these products. Production polygons and
EPA reference points are authoritative External rows fetched on demand in the
browser. EPA county-location records remain retrieval/QA products only because
their county geometry is not an aquifer-exemption boundary.

## Curated SWRCB correction input

The small SWRCB BLM water-right correction CSV is tracked because the core
cache build stops rather than silently dropping its official membership and
face-value corrections.

## Curated Bulletin 118 SGMA crosswalk

`00_config/bulletin118_sgma_2019_priority_crosswalk.csv` is a tracked,
attribute-only snapshot of DWR's final 2019 SGMA basin-prioritization table.
It contains 515 basin/subbasin codes, normalized priority, source OBJECTID,
service URL, and access date. It contains no geometry. Refresh it only with
`02_preprocess/68_refresh_bulletin118_sgma_2019_priority.R`; that script
requires the exact published 515-row and 46/48/11/410 category contracts before
overwriting the tracked crosswalk.

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
