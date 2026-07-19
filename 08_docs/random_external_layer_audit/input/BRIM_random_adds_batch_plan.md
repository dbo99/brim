# BRIM Random Adds - Suggested Audit/Build Batches

This is a starting point, not a hard rule. Re-rank after actual service audits.

## Batch 1: high desire + high ease + clear service

- AML_CA_FEATURES
- CA_GDE_NCCAG
- IMPAIRED_WATERS
- NOHRSC_SNOW
- SPC_FIREWX
- SPC_MESO_DISC
- NWM_SOIL_MOISTURE
- BETTER_WIND_OBS
- HIGHWAYS_NHS
- BLM_RECREATION

## Batch 2: high desire but source/data-path work needed

- AML_PAMP
- ACTIVE_MINES_SMARA
- INSAR_SUBSIDENCE
- SPORT_LIS
- QUICKDRI
- VEGDRI
- NDFD_WIND
- CALGEM_WELLS
- DRECP
- BLM_RENEWABLE_PROJECTS
- RAILROADS
- TRANSMISSION_LINES
- FRAP_VEG_FIRE

## Batch 3: compact/explore/link or lower priority

- SPC_CONVECTIVE
- WPC_WSSI
- CPC_HAZARDS
- CPC_WEEK34
- CPC_MONTH_SEASON
- CPC_NLDAS_SOIL
- CROP_CASMA
- SMAP_GRACE
- NOAA_STAR_VHI
- NOAA_STAR_NDVI
- NASA_GIBS_NDVI
- SUBSTATIONS
- SWPC_SPACE
- SWPC_AURORA

## General implementation classes

- direct_arcgis: FeatureServer/MapServer layer can be added directly.
- tiled_raster: ArcGIS MapServer/ImageServer export or tile route.
- wms_wmts: WMS/WMTS with named layer/time dimension.
- fetched_geojson: query service and cache/serve GeoJSON.
- static_preprocess: download/process into BRIM static layer.
- link_only: compact source/product link, no map geometry.
- defer: no stable public source, too sensitive, too duplicative, or too expensive.

## High-value caveats to preserve

- AML_CA_FEATURES: locations are obfuscated/generalized and incomplete; screening only.
- CA_GDE_NCCAG: screening/start point only; not field-verified GDE determinations.
- IMPAIRED_WATERS: verify latest/current listing cycle before relying on the service.
- NWM_SOIL_MOISTURE: saturation/current wetness, not percentile/anomaly.
- NOHRSC_SNOW: modeled/gridded snow product, not station observations.
- SPC layers: forecast outlook/discussion polygons, not warnings/incidents.
