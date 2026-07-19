# PortaTreasure2 R/Leaflet Map Program — Handoff Document

**Prepared:** 2026-04-29  
**Project:** PortaTreasure2  
**Current status:** Advanced, working R-based standalone Leaflet webmap workflow. The final HTML map was recently built successfully and opened as expected. The current project has moved beyond the original legacy PortaTreasure concept, so the `07_legacy_scripts/` folder should be treated as historical reference only, not as active code. The current contents of `08_docs/` are superseded by this handoff.

---

## 1. One-paragraph summary

PortaTreasure2 is an R-based geospatial preprocessing and Leaflet map-building program for a statewide California/BLM-centered water-resources reference map. It reads raw GIS and tabular datasets, preprocesses them into clean RDS/GPKG layers, builds map-ready cached RDS layers with simplified geometry, popups, styling fields, and labels, and exports a timestamped standalone HTML Leaflet webmap. The map includes BLM lands and offices, HUC watersheds, Bulletin 118 groundwater basins, counties, CNRFC gages and basins, USGS streamgages and wells, CalSim3 arcs, major conveyance features, DRECP/ACEC/allotments/conservation reference layers, PRISM/BCMv8 HUC climate/recharge summaries, and a sidecar workflow for cached latest USGS groundwater-level measurements.

---

## 2. Current intended source folders

Active source-code folders:

```text
00_config/
02_preprocess/
03_functions/
05_map_build/
run_build_map.r  # currently just a lightweight setup/skeleton script
cleanup_generated_outputs_before_backup.r
```

Folders to ignore for this handoff unless specifically needed later:

```text
07_legacy_scripts/      # historical only; current workflow has moved beyond this
08_docs/                # superseded by this handoff
futurelayerstoadd/      # explicitly excluded from current handoff
```

Generated or raw folders that should not generally be uploaded to a new chat unless diagnosing a specific problem:

```text
01_raw_data/            # raw shapefiles, rasters, large CSVs/TXTs
04_processed_data/      # RDS caches, GPKGs, QA outputs, sidecar tables
06_output/              # generated HTML maps
```

---

## 3. High-level architecture

The project is organized as a staged pipeline:

```text
Raw data in 01_raw_data/
        ↓
02_preprocess/*.r scripts
        ↓
Full-resolution processed RDS/GPKG in 04_processed_data/rds and gpkg
        ↓
05_map_build/02_build_core_map_cache.r
        ↓
Map-ready simplified/enriched RDS caches in 04_processed_data/cache/latest
        ↓
05_map_build/05_build_label_cache.r
        ↓
Label RDS cache in 04_processed_data/cache/latest/labels_all_map.rds
        ↓
05_map_build/04_build_portatreasure2_core_map.r
        ↓
Standalone timestamped Leaflet HTML in 06_output/html
```

The division of labor is important:

- **Preprocessors** create stable analytical/processed data products from raw inputs.
- **Core map cache builder** creates lightweight display-ready layers, popups, and styling fields.
- **Label cache builder** creates separate label point layers with zoom thresholds.
- **Final HTML builder** reads only cached layers and writes the final map.

---

## 4. Key build commands

### Build final HTML only
Use when cached map-ready layers already exist and only final HTML display settings changed:

```r
source("05_map_build/04_build_portatreasure2_core_map.r")
```

If a helper function has changed but cached data has not changed, this can be enough.

### Rebuild core cache and final map
Use when popup text, styling, joined attributes, reference layers, latest water-level sidecar joins, or map-ready cache logic changed:

```r
rebuild_core_cache_and_map()
```

This helper exists in the broader working environment/script set and has been used successfully. If a new chat cannot find it, manually run:

```r
source("05_map_build/02_build_core_map_cache.r")
source("05_map_build/05_build_label_cache.r")
source("05_map_build/04_build_portatreasure2_core_map.r")
```

### Rerun preprocessors
Use only when raw data changed or a preprocessor changed. Examples:

```r
source("02_preprocess/10_calsim3_arcs.r")
source("02_preprocess/12_major_conveyance.r")
source("02_preprocess/13_huc_climate_recharge_summary.r")
source("02_preprocess/14_huc4_huc2_from_huc6_climate_rollup.r")
source("02_preprocess/15_usgs_gw_latest_water_levels.r")
```

---

## 5. Configuration files

### `00_config/config_paths.r`
Defines all project directories with `here::here()`. It creates missing folders automatically and prints the root/raw/rds/html paths. The central directory object is `DIR`, with entries such as:

```r
DIR$raw
DIR$rds
DIR$gpkg
DIR$cache_last
DIR$qa
DIR$html
```

### `00_config/config_source_files.r`
Defines the active raw source files in the `SRC` list. Important entries include:

- `SRC$reference_layers_manifest`
- `SRC$blm_fedlands`
- `SRC$blm_offices`
- `SRC$field_office_outer`
- `SRC$cnrfc_stream_gages`
- `SRC$cnrfc_precip_gages`
- `SRC$cnrfc_clip_buffer`
- `SRC$cnrfc_basins`
- `SRC$huc2` through `SRC$huc12`
- `SRC$calsim3_arcs`
- `SRC$major_conveyance`
- `SRC$bull118_gw`
- `SRC$counties`
- `SRC$project_areas`

### `00_config/config_run_flags.r`
Contains broad workflow flags. Current important settings include:

- `RUN$rebuild_preprocess = FALSE`
- `RUN$rebuild_geom_cache = FALSE`
- `RUN$rebuild_popups = TRUE`
- `RUN$rebuild_labels = TRUE`
- `RUN$build_map = TRUE`
- `RUN$self_contained_html = TRUE`
- `RUN$cluster_points = TRUE`
- `RUN$show_huc10_12 = TRUE`

### `00_config/config_map_display.r`
Controls final map behavior:

- Default California-centered view: `lng = -119.77`, `lat = 36.74`, `zoom = 6`.
- `default_base_group = "No Basemap"`.
- `zoom_snap = 0.5`, `zoom_delta = 0.5`.
- `self_contained_html = TRUE`.
- `open_after_save = FALSE`.
- Labels enabled by default.
- All overlays start hidden: `default_visible_overlays = character(0)`.
- Layer switches include project areas, BLM offices, field-office outer boundaries, HUC10/12, CNRFC points/basins, USGS points, CalSim3 arcs, reference layers, and major conveyance.

### `00_config/config_labels.r`
Central label configuration:

- `LABELS` controls default label styling and global behavior.
- `LABEL_ZOOM` stores label layer zoom thresholds.
- `LABEL_INCLUDE` controls whether label layers are built/added.
- `LABEL_FIELDS` explicitly maps label IDs to attribute fields.

Important label decisions:

- USGS wells are intentionally excluded from labels because the layer is too dense.
- Manifest-driven reference labels currently include `fedwilderness`, `acec`, and `allotments`.
- Major conveyance labels use `Pname`.

### `00_config/reference_layers_manifest.csv`
Manifest for 11 mostly reference/admin/conservation layers. Columns:

```text
nickname, geom_type, filename, namecolumn, colorbycolumn, popup, mb, folder, simplify_keep, label_field
```

Current rows include trails, monuments, CA Desert NCL, wilderness study areas, federal wilderness, DRECP, ACECs, grazing allotments, Wild & Scenic Rivers, Groundwater Sustainability Plan Areas, and adjudicated groundwater basins.

Important manifest corrections already made:

- `fedwilderness` uses `ManagingAg` rather than the initially assumed `ManagingAgency_ca`.
- `wsr` uses `SMA_ID` and `CATEGORY_c` rather than the initially assumed `_ca` fields.
- `gwbasins_adjd` has `colorbycolumn = none`.

---

## 6. Preprocessing scripts

### `02_preprocess/01_blm_managed_and_held.r`
Reads BLM federal lands, splits BLM-managed and BLM-held features using configured fields, dissolves managed/held lands, creates a BLM-managed core polygon and held/managed difference polygons, and saves RDS/GPKG/QA outputs.

Key outputs:

```text
04_processed_data/rds/blm_managed_core_3310.rds
04_processed_data/rds/blm_held_managed_diffs_3310.rds
```

Important fields:

```r
MNGD_FIELD <- "SMA_ID"
HELD_FIELD <- "HOLD_ID"
```

### `02_preprocess/02_huc_gw_county_pct_blm.r`
Reads HUC2/HUC4/HUC6/HUC8/HUC10/HUC12, Bulletin 118 groundwater basins, counties, and the BLM core polygon. Computes BLM-managed area and percent BLM for each polygon layer. Adds HUC parent metadata. Saves full-resolution processed RDS/GPKG/QA.

Key outputs:

```text
huc_all_full.rds
bull118gw_full.rds
county_full.rds
```

### `02_preprocess/03_cnrfc_stream_gages.r`
Parses CNRFC river/stream gage text file. Extracts station IDs, names/descriptions, lat/lon/elevation, gage type/classes, clips to California buffer, and saves an sf RDS/GPKG/QA.

Key output:

```text
cnrfc_stream_gages_wgs84.rds
```

### `02_preprocess/04_cnrfc_precip_gages.r`
Parses CNRFC precipitation gage text file with similar logic to the stream gage parser. Clips to CNRFC California buffer.

Key output:

```text
cnrfc_precip_gages_wgs84.rds
```

### `02_preprocess/05_usgs_streamgages_wells.r`
Processes previously downloaded USGS streamgage and groundwater-well metadata/parameter files into final USGS point RDS products. It does not fetch the new latest groundwater-level sidecar table; that is handled by script 15.

Key outputs likely include:

```text
USGS_SW_final.rds
USGS_GW_final.rds
```

### `02_preprocess/06_blm_offices.r`
Reads BLM office CSV and creates office point layer. Supports office types such as California State Office, District Office, and Field Office. Used by final map to draw different office symbols.

### `02_preprocess/07_project_areas_placeholder.r`
Creates an empty project-area layer if no project-area shapefile exists. This lets the map build even when no active project polygons are supplied.

### `02_preprocess/08_cnrfc_basins.r`
Reads CNRFC basin polygons and prepares them for mapping.

### `02_preprocess/09_field_office_outer.r`
Processes BLM field-office outer boundary layer.

### `02_preprocess/10_calsim3_arcs.r`
Processes CalSim3 model arc lines. Current work added more `Type` categories beyond Channel/Diversion.

Current intended CalSim3 types and colors in map cache/build:

```text
Channel   = blue
Diversion = red
Return    = green
Inflow    = baby blue
```

Key output:

```text
calsim3_arcs_wgs84.rds
```

### `02_preprocess/11_reference_layers_batch.r`
Manifest-driven preprocessor for the 11 reference layers. It reads `reference_layers_manifest.csv`, loads shapefiles from folders listed in the manifest, repairs invalid geometries, transforms to WGS84, applies layer-specific `simplify_keep`, standardizes metadata fields, builds popup metadata, and writes latest/timestamped RDS products.

Important design decision:

- This is intentionally a single batch preprocessor for miscellaneous reference layers rather than one preprocessor per data type, but independent editability is preserved in the manifest through `simplify_keep`, `colorbycolumn`, `popup`, `label_field`, etc.

Key output pattern:

```text
reference_<nickname>_wgs84.rds
```

### `02_preprocess/12_major_conveyance.r`
Processes `01_raw_data/conveyance/majorconveyance.shp`. It slims attributes, groups operator categories, dissolves by `Pname + Operator`, and saves major conveyance linework.

Operator grouping:

```text
CCWD + Local -> Local / CCWD
Federal      -> Federal
State        -> State
Fed/State    -> Fed/State
other        -> Other / unknown
```

Final symbology:

- Federal = blue
- State = orange
- Local/CCWD = purple
- Fed/State = two dashed overlays, blue and orange
- Other = gray

### `02_preprocess/13_huc_climate_recharge_summary.r`
Heavy raster extraction script for PRISM precipitation and BCMv8 recharge summaries by HUC level. It summarizes raster depths to HUC polygons as annual mean areal depth and volume.

Inputs:

```text
01_raw_data/raster/prism_ppt_us_30s_2020_avg_30y.tif
01_raw_data/raster/rch1991_2020_ave.asc
04_processed_data/rds/huc_all_full.rds
```

Outputs per HUC level:

```text
huc<level>_climate_recharge_full.rds
huc<level>_climate_recharge_table.rds
```

Important values:

- `map_in` / `map_mm` = PRISM mean annual precipitation
- `ppt_acft` / `ppt_kaf` = precipitation volume
- `rech_in` / `rech_mm` = BCMv8 mean annual recharge
- `rech_acft` / `rech_kaf` = recharge volume
- `rech_eff_pct` = recharge volume / precip volume × 100
- `ppt_valid_frac` / `rech_valid_frac` = valid raster coverage fraction

Important implementation notes:

- Batch-level raster cropping was added to avoid enormous raster crops for large HUCs.
- HUC10 and HUC12 were direct extractions and are good to go.
- HUC8 and HUC6 were also direct extracted later.
- HUC6 extraction was extremely slow (~12 hours), so exact HUC4/HUC2 extraction was abandoned.

Current direct-extraction status:

```text
HUC10: direct extraction completed
HUC12: direct extraction completed
HUC8: direct extraction completed
HUC6: direct extraction completed
HUC4: rollup from HUC6
HUC2: rollup from HUC6, excluding Pacific Northwest Region
```

### `02_preprocess/14_huc4_huc2_from_huc6_climate_rollup.r`
Creates HUC4 and HUC2 climate/recharge tables by rolling up direct HUC6 summaries. This avoids slow direct raster extraction of very large parent HUCs.

Rollup method:

```text
parent precip volume = sum(child HUC6 precip volumes)
parent recharge volume = sum(child HUC6 recharge volumes)
parent precip depth = parent precip volume / summed valid PRISM area
parent recharge depth = parent recharge volume / summed valid BCMv8 area
parent recharge efficiency = parent recharge volume / parent precip volume × 100
```

BCMv8 display rule:

- Recharge is not reported when valid BCMv8 coverage is below 75%.
- A popup note is used instead.

Key outputs:

```text
huc4_climate_recharge_table.rds
huc2_climate_recharge_table.rds
```

### `02_preprocess/15_usgs_gw_latest_water_levels.r`
Sidecar fetch script for latest cached USGS groundwater-level field measurements. It does **not** modify `USGS_GW_final.rds`. It creates a separate lookup table that is joined during core cache building.

Current important design decisions:

- Uses `dataRetrieval::read_waterdata_field_measurements()`.
- Uses `parameter_code = "72019"`.
- Values are displayed as `ft bgs`.
- Uses an API key stored as `API_USGS_PAT`.
- Uses batch/chunk files to make long runs resumable.
- Future-proof version includes `CHUNK_MODE` and `FETCH_RUN_ID` logic.
- Empty successful batches should be saved so they are not re-requested.
- Final latest table should not be overwritten unless all expected chunks exist.

Key outputs:

```text
usgs_gw_latest_water_levels.rds
usgs_gw_latest_water_levels_<timestamp>.rds
usgs_gw_latest_water_levels_raw_chunks_<timestamp or run-id>/batch_####.rds
```

Map integration:

- Joined to `usgs_gw_map` by `site_no` in `02_build_core_map_cache.r`.
- Popup line says latest cached USGS water level and notes that it is not live.
- Hover text shows water level, month/year, and active/inactive status.

Important caveat:

- If a full fetch is interrupted, do not allow the partial sidecar to overwrite the full table. Otherwise the map may show a latitude-like break because `site_no` order roughly tracks latitude.

### `02_preprocess/15b_rescue_missing_gw_wl_batches.r`
Rescue script for a small number of problematic groundwater-level batches. It can try whole-batch requests and then individual site requests. It writes successful partial chunks and QA for failed/skipped sites. Current hardcoded example refers to missing batches 1333 and 1459 in a 2026-04-28 chunk folder.

Recommended future improvement:

- Fold the individual-site fallback logic into script 15 so missing batches can be automatically rescued or skipped with QA rather than requiring a separate script.

---

## 7. Function helper files

### `03_functions/cache_helpers.r`
Small reusable cache helpers:

- `make_timestamp()`
- `timestamped_name()`
- `save_rds_cached()`
- `read_rds_checked()`

### `03_functions/spatial_helpers.r`
Spatial utility functions used in preprocessors and cache builders. Handles things such as CRS checks, geometry cleaning/simplification helpers, and Leaflet preparation.

### `03_functions/popup_helpers.r`
All popup-builder and popup-enrichment functions. Important functions:

- `pt_make_huc_popups()`
- `pt_make_gw_popups()`
- `pt_make_county_popups()`
- `pt_make_blm_core_popups()`
- `pt_enrich_blm_diffs()`
- `pt_make_cnrfc_stream_popups()`
- `pt_make_cnrfc_precip_popups()`
- `pt_make_usgs_stream_popups()`
- `pt_summarize_usgs_params()`
- `pt_make_usgs_well_popups()`
- `pt_summarize_usgs_well_params()`
- `pt_make_blm_office_popups()`
- `pt_make_project_area_popups()`
- `pt_make_cnrfc_basin_popups()`
- `pt_make_field_office_outer_popups()`
- `pt_make_calsim3_arc_popups()`
- `pt_make_reference_layer_popups()`
- `pt_make_major_conveyance_popups()`

Recent popup decisions:

- USGS streamgage/well popups no longer show massive raw parameter lists. They summarize common data types instead.
- HUC popups include PRISM/BCMv8 climate/recharge blocks when joined.
- HUC popups include `cw3e qpf` link for HUC8.
- USGS streamgage popups include `USGS Site`, `usgs dash`, and `noaa nwm` links.
- Groundwater-well popups include latest cached water-level values when available.

### `03_functions/leaflet_core_helpers.r`
Core Leaflet setup functions:

- `pt_init_map()`
- `pt_add_panes()`
- `pt_add_basemaps()`
- `pt_base_groups()`
- `pt_add_radar()`
- `pt_add_cgs_geology()`
- `pt_add_layer_control()`
- `pt_add_mouse_coordinates()`

Important display decisions:

- Basemaps include USGS Hydrography, USGS Topo, USGS National Map variants, Esri basemaps, NASA MODIS Terra True Color, CartoDB Positron, OpenStreetMap, and No Basemap.
- No Basemap is the default base layer.
- CA geology is a tile/service layer added for visual reference only and hidden by default.

### `03_functions/leaflet_layer_helpers.r`
Layer drawing helpers. Important functions:

- `pt_add_blm_layers()`
- `pt_add_county_gw_layers()`
- `pt_add_huc_layers()` / `pt_add_huc_layer()`
- `pt_add_cnrfc_layers()`
- `pt_add_usgs_layers()`
- `pt_add_project_area_layer()`
- `pt_add_blm_office_layer()`
- `pt_add_cnrfc_basin_layer()`
- `pt_add_field_office_outer_layer()`
- `pt_add_calsim3_arc_layer()`
- `pt_reference_overlay_groups()`
- `pt_add_reference_layers()`
- `pt_add_major_conveyance_layer()`

Current USGS wells styling concept:

- Fill color should represent cached water-level depth only.
- No cached water level should use neutral fill.
- Outline can show status for no-water-level points.
- Hover text should include WL value/date and status when available.

Suggested style logic:

```text
Cached WL fill:
  <0 ft bgs       blue
  0–100 ft bgs    green
  100–300 ft bgs  orange
  300–600 ft bgs  red
  >600 ft bgs     purple
No cached WL:
  neutral gray/white fill
  active outline green
  inactive outline gray
```

### `03_functions/label_helpers.r`
Creates label sf objects from polygon centroids/points and explicit label fields.

### `03_functions/leaflet_label_helpers.r`
Adds label layers to Leaflet. Labels are stored as clustered label-only markers and shown/hidden via layer control and zoom behavior.

---

## 8. Map-build scripts

### `05_map_build/00_preview_cnrfc_points.r`
Preview script for CNRFC point layers.

### `05_map_build/01_preview_blm_huc_gw_county.r`
Preview script for BLM/HUC/GW/county processed layers.

### `05_map_build/02_build_core_map_cache.r`
Critical script. Reads full processed RDS layers and creates map-ready cached RDS layers.

Major duties:

- Reads processed full-resolution layers.
- Reads manifest-driven reference layers.
- Simplifies map geometry using per-layer `SIMPLIFY_KEEP` settings.
- Adds popups and style columns.
- Joins HUC climate/recharge tables.
- Masks HUC6 BCMv8 recharge where valid raster coverage is below 75%.
- Joins latest cached USGS groundwater-level sidecar table.
- Builds well hover text and water-level styling fields.
- Styles CNRFC points, basins, field office outer boundary, CalSim3 arcs, major conveyance, and reference layers.
- Writes latest and timestamped map-ready caches.
- Writes QA summary.

Key outputs in `04_processed_data/cache/latest/`:

```text
blm_core_map.rds
blm_diffs_map.rds
blm_offices_map.rds
calsim3_arcs_map.rds
cnrfc_basins_map.rds
cnrfc_precip_map.rds
cnrfc_stream_map.rds
county_map.rds
field_office_outer_map.rds
gw_bull118_map.rds
huc_all_map.rds
labels_all_map.rds
major_conveyance_map.rds
project_areas_map.rds
reference_layers_all_map.rds
usgs_streamgages_map.rds
usgs_wells_map.rds
```

### `05_map_build/03_preview_core_cache_map.r`
Preview script for map-ready core cache layers.

### `05_map_build/04_build_portatreasure2_core_map.r`
Final HTML builder. Reads only map-ready cached RDS files and builds the full Leaflet HTML.

Important behavior:

- Uses `MAP_DISPLAY` settings.
- Builds overlay group list in layer-control order.
- Adds basemaps, radar, geology, vector layers, labels, scale bar, mouse coordinates.
- Adds a `*` info button explaining asterisked BLM/subset layers.
- Adds a `**` info button with PRISM/BCMv8 citations and processing notes.
- Hides overlays by default.
- Saves self-contained HTML through a temporary folder, then copies to `06_output/html` to avoid OneDrive/htmlwidgets file clutter.

Current successful output size was about 180 MB after recent additions.

### `05_map_build/05_build_label_cache.r`
Builds label cache from map-ready layers and label config. It uses explicit `LABEL_FIELDS` and writes `labels_all_map.rds`.

---

## 9. Current map layers and display logic

### Basemaps and tile overlays

Base layers:

- USGS Hydrography
- USGS Topo
- USGS National Map (Imagery)
- USGS National Map (Imagery+Topo)
- Esri World Topographic
- Esri World Street Map
- Esri World Imagery
- NASA MODIS Terra True Color
- CartoDB Positron
- OpenStreetMap
- No Basemap

Tile overlays:

- NEXRAD Radar, hidden by default
- CA Geology visual-only layer, hidden by default and listed near the bottom of overlay controls

### Core BLM/admin layers

- Project areas: placeholder layer allowed to be empty.
- BLM Offices: distinct symbols for state/district/field offices.
- BLM Field Office outer boundary.
- BLM-CA Managed core.
- BLM Held/Managed Differences: popups distinguish “held not managed” and “managed not held”; uses fill/stroke styling.
- Counties.

### Groundwater and HUC layers

- GW – Bull. 118 with robust popup including basin/subbasin details, area, BLM %, and Google search.
- HUC2/HUC4/HUC6/HUC8/HUC10/HUC12, all with popups; HUC layers are asterisked with `**` because they include PRISM/BCMv8 climate/recharge data where available.
- HUC10/HUC12 direct extraction values are most detailed; HUC4/HUC2 are rolled up from HUC6.

### Climate/recharge popup values

HUC climate/recharge popup block includes:

- PRISM 1991–2020 mean annual precipitation (`map_in`)
- Precip volume (`ppt_kaf`)
- BCMv8 1991–2020 recharge (`rech_in`) when valid coverage is adequate
- Recharge volume (`rech_kaf`) when valid coverage is adequate
- Recharge efficiency (`rech_eff_pct`) when valid coverage is adequate
- Raster valid area fractions
- Recharge note if BCMv8 coverage is inadequate

Map-level `**` button carries full PRISM and BCMv8 citations.

### CNRFC layers

- CNRFC Basins
- CNRFC Stream Gages
- CNRFC Precip Gages

CNRFC basin popups include RFC RVF, RFC WY, and Google/NOAA-style links where available.

### USGS layers

- USGS Streamgages
- USGS Wells

USGS streamgage popups are trimmed and include links to USGS Site, USGS Dashboard, and NOAA/NWM.

USGS wells:

- Popup no longer prints huge parameter lists.
- Popup summarizes common data.
- Latest cached field-measured groundwater level is shown if joined.
- Hover text shows WL/date/status when available.
- Sidecar table is not live; it reflects the latest values available when script 15 was last run.

### CalSim3 and conveyance

- CalSim3 arcs include Channel, Diversion, Return, and Inflow types.
- Major Conveyance layer is dissolved by `Pname + Operator` and labelled by `Pname`.
- Fed/State conveyance is symbolized as alternating dashed Federal/State colors.

### Manifest-driven reference layers

- National Scenic/Historic Trails *
- National Monuments *
- CA Desert National Conservation Lands
- Wilderness Study Areas *
- Federal Wilderness *
- DRECP
- ACECs
- Grazing Allotments
- Wild & Scenic Rivers *
- Groundwater Sustainability Plan Areas
- Adjudicated Groundwater Basins

Asterisk note:

- `*` indicates BLM GIS-based coverage or BLM-focused subset, not a complete statewide inventory.

---

## 10. Important data-processing caveats

### Self-contained HTML size

The map is large. Recent output was about 180 MB. The biggest contributors are likely:

- USGS wells (~44k points)
- BLM core geometry
- reference layer geometry such as monuments/federal wilderness/GSPs
- HUC12 geometry and labels
- CalSim3 arcs

Trimming USGS parameter lists materially reduced file size.

### USGS groundwater-level sidecar is cached, not live

The groundwater-level script fetches latest available field measurements at script runtime. The final HTML does not make live API calls. Popup wording should continue to say “Latest cached USGS water level” and “not live.”

### Latest water level can be old

Many “latest” groundwater measurements are historic. Popup and hover include the date so users can assess recency.

### Inactive wells can have water levels

Inactive means the site is not currently active, not that it lacks historic measurements. Therefore inactive wells may have cached water-level values.

### Partial groundwater-level fetches can cause geographic artifacts

Because USGS site numbers roughly encode/track latitude, an interrupted fetch can create an artificial latitude break if a partial full table is written. The future-proof script should avoid this by not overwriting the final table unless all expected chunks exist.

### BCMv8 domain limitations

BCMv8 recharge raster is hydrologic-California-focused. For HUCs that extend substantially outside its valid raster domain, recharge values are suppressed in the popup and replaced with a note. PRISM precipitation is broader CONUS coverage and remains useful where valid.

### HUC4/HUC2 climate/recharge values are rollups

HUC4 and HUC2 values are rolled up from direct HUC6 extraction. This is a defensible volume-based rollup, not a crude average, but should be described accurately.

---

## 11. Seasonal update workflows

### Updating groundwater-level sidecar after a future season

Use script 15. Recommended future-proof approach:

```r
RUN_MODE <- "full"
CHUNK_MODE <- "named"
FETCH_RUN_ID <- "2026_fall"  # or similar unique seasonal ID
```

Then:

```r
source("02_preprocess/15_usgs_gw_latest_water_levels.r")
rebuild_core_cache_and_map()
```

Use the same `FETCH_RUN_ID` to resume a partially completed seasonal fetch. Change `FETCH_RUN_ID` to force a new seasonal fetch.

### Updating HUC climate/recharge values

Only needed if PRISM/BCMv8 inputs or HUC geometry change.

Rerun direct raster extraction only for desired HUC levels:

```r
source("02_preprocess/13_huc_climate_recharge_summary.r")
```

Then recreate HUC4/HUC2 rollups if HUC6 changed:

```r
source("02_preprocess/14_huc4_huc2_from_huc6_climate_rollup.r")
```

Then rebuild map cache and final HTML.

### Updating reference layers

Edit `reference_layers_manifest.csv` for field names, colors, popup fields, labels, or simplification levels. Then run:

```r
source("02_preprocess/11_reference_layers_batch.r")
rebuild_core_cache_and_map()
```

### Updating major conveyance

If `majorconveyance.shp` changes:

```r
source("02_preprocess/12_major_conveyance.r")
rebuild_core_cache_and_map()
```

### Updating CalSim3 arcs

If CalSim3 source layer or Type filtering/styling changes:

```r
source("02_preprocess/10_calsim3_arcs.r")
rebuild_core_cache_and_map()
```

---

## 12. Cleanup / backup guidance

The project includes `cleanup_generated_outputs_before_backup.r` for removing old generated outputs before zipping/backing up the working directory. Use dry run first. Do not delete expensive analytical outputs casually.

Important files to protect:

```text
huc*_climate_recharge_full.rds
huc*_climate_recharge_table.rds
usgs_gw_latest_water_levels*.rds
usgs_gw_latest_water_levels_raw_chunks_*/
```

The latest non-timestamped files are essential for rebuilds. Timestamped archives are useful when heavy raster/API work would be costly to reproduce.

---

## 13. Known issues / next improvements

### Fold rescue logic into script 15

`15b_rescue_missing_gw_wl_batches.r` exists because one or two batches failed due to a `cli::pb_eta` formatting issue from the USGS/dataRetrieval stack. The desired long-term behavior is for script 15 to:

1. Try whole-batch request.
2. If batch fails, try site-by-site.
3. Save partial successes.
4. Write failed site IDs to QA.
5. Save a chunk so the full combine can proceed if only a handful of sites fail.

### Improve USGS streamgage latest-flow sidecar

The same sidecar approach should be created for streamgage flow:

```text
02_preprocess/16_usgs_streamgage_latest_flow.r
  -> usgs_streamgages_latest_flow.rds
```

Suggested behavior:

- Fetch latest continuous discharge where available.
- Join to `usgs_sw_map` in core cache.
- Popup says cached/latest flow and timestamp, not live.
- Hover shows cfs and time.
- Symbology uses size/color by discharge magnitude.

Potential streamgage symbology:

```text
0 or dry/no flow: gray
<10 cfs: light blue
10–100 cfs: blue
100–1,000 cfs: green
1,000–10,000 cfs: orange
>10,000 cfs: red/purple
radius = scaled log10(cfs + 1)
```

### Improve well symbology

Recommended final logic:

- Fill color only means cached water-level depth.
- Neutral fill means no cached water level.
- Outline shows active/inactive only for no-water-level points.
- Hover text always includes status.

### Add legend

A custom legend would be useful for:

- USGS well water-level depth classes
- Streamgage latest flow classes, when added
- CalSim3 arc types
- Major conveyance operator types
- HUC line colors

### Better build runner

`run_build_map.r` is currently a setup skeleton. A future improvement is to create explicit high-level commands:

```r
build_final_map_only()
rebuild_core_cache_and_map()
run_all_preprocessors()
refresh_gw_water_levels_and_map()
refresh_huc_climate_rollups_and_map()
```

Some of these functions have been used interactively, but the formal runner should be made explicit and documented.

---

## 14. New-chat orientation instructions

When opening a new ChatGPT chat for this project, provide:

1. This handoff document.
2. The current active script folders:

```text
00_config.zip
02_preprocess.zip
03_functions.zip
05_map_build.zip
```

3. The source-file inventory if needed.
4. Any specific logs/errors from the current task.

Do **not** upload large raw data, RDS caches, or HTML unless the task requires diagnosing a data-specific failure.

Tell the new chat:

> We are working on PortaTreasure2, an R/Leaflet map-building workflow. Ignore `07_legacy_scripts`, `08_docs`, and `futurelayerstoadd` unless explicitly needed. The active architecture is config → preprocessors → map-ready cache → label cache → final standalone HTML. Keep heavy data products separate from map cache, and avoid modifying master RDS files when a sidecar table can be joined instead.

---

## 15. Current file inventory, active focus

Active files from the latest inventory include:

```text
00_config/config_labels.r
00_config/config_map_display.r
00_config/config_paths.r
00_config/config_run_flags.r
00_config/config_source_files.r
00_config/reference_layers_manifest.csv

02_preprocess/01_blm_managed_and_held.r
02_preprocess/02_huc_gw_county_pct_blm.r
02_preprocess/03_cnrfc_stream_gages.r
02_preprocess/04_cnrfc_precip_gages.r
02_preprocess/05_usgs_streamgages_wells.r
02_preprocess/06_blm_offices.r
02_preprocess/07_project_areas_placeholder.r
02_preprocess/08_cnrfc_basins.r
02_preprocess/09_field_office_outer.r
02_preprocess/10_calsim3_arcs.r
02_preprocess/11_reference_layers_batch.r
02_preprocess/12_major_conveyance.r
02_preprocess/13_huc_climate_recharge_summary.r
02_preprocess/14_huc4_huc2_from_huc6_climate_rollup.r
02_preprocess/15_usgs_gw_latest_water_levels.r
02_preprocess/15b_rescue_missing_gw_wl_batches.r

03_functions/cache_helpers.r
03_functions/label_helpers.r
03_functions/leaflet_core_helpers.r
03_functions/leaflet_label_helpers.r
03_functions/leaflet_layer_helpers.r
03_functions/popup_helpers.r
03_functions/spatial_helpers.r

05_map_build/00_preview_cnrfc_points.r
05_map_build/01_preview_blm_huc_gw_county.r
05_map_build/02_build_core_map_cache.r
05_map_build/03_preview_core_cache_map.r
05_map_build/04_build_portatreasure2_core_map.r
05_map_build/05_build_label_cache.r

cleanup_generated_outputs_before_backup.r
run_build_map.r
```

Legacy and future-layer folders should not be treated as active for this handoff.

---

## 16. “If something breaks” troubleshooting guide

### Final HTML build succeeds, then R crashes

If console shows the HTML saved and `[DONE] Build final HTML`, the output probably succeeded. Restart R and verify the HTML file exists and opens. Low-level errors like `cannot have attributes on a CHARSXP` after save may be R/RStudio/package instability with large htmlwidgets output rather than map failure.

### `popup_html` missing

Usually means the core cache did not add popup fields before saving. Rerun:

```r
source("05_map_build/02_build_core_map_cache.r")
```

Then rebuild final map.

### `stroke_col` or style column missing

Usually a style field was expected by a Leaflet helper but not created in cache. Check the cache object names and add fallback style columns in `02_build_core_map_cache.r`.

### HUC popup missing climate/recharge block

Check whether the corresponding table exists:

```r
list.files("04_processed_data/rds", pattern = "huc.*climate_recharge_table.*rds")
```

HUC4/HUC2 require script 14 rollup. HUC6/HUC8/HUC10/HUC12 require script 13 direct extraction.

### USGS wells show a latitude cutoff in cached WL values

Likely the water-level sidecar table is partial. Check chunk completeness and do not overwrite final table unless all chunks exist.

### Rate limit errors from USGS

Ensure API key is available:

```r
nchar(Sys.getenv("API_USGS_PAT"))
```

Do not paste the key into chat. Use `.Renviron`.

### OneDrive permission errors while writing chunks

Use per-batch chunk files rather than overwriting one partial RDS repeatedly. Avoid building huge outputs directly in OneDrive temp folders when possible.

---

## 17. Recommended immediate next steps

1. Finish rescuing/combining the USGS groundwater-level chunks.
2. Rebuild core cache and final HTML.
3. Verify no artificial latitude break remains.
4. Set final well symbology to neutral no-WL fill + colored WL depth fill.
5. Create `16_usgs_streamgage_latest_flow.r` sidecar workflow.
6. Add a map legend for wells and streamgages once both recent-data layers are stable.

