# 42_audit_random_external_layer_candidates.R
# -----------------------------------------------------------------------------
# Purpose:
#   One-off/O&M audit helper for the BRIM "random adds" external-layer handoff.
#   This script DOES NOT modify the BRIM map, external_service_catalog.csv, cache
#   products, or any GitHub-hosted feed products.
#
#   It reads a candidate inventory, compares candidates to the current BRIM
#   External Layers catalog, optionally probes public ArcGIS REST endpoints, and
#   writes ranked audit outputs that can guide small, low-risk future patches.
#
# Expected project root:
#   C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2
#
# Inputs, by default:
#   08_docs/random_external_layer_audit/input/BRIM_random_adds_master_inventory.csv
#   00_config/external_service_catalog.csv
#
# Outputs:
#   08_docs/random_external_layer_audit/output/random_external_layer_audit_<timestamp>.csv
#   08_docs/random_external_layer_audit/output/random_external_layer_audit_<timestamp>.json
#   08_docs/random_external_layer_audit/output/random_external_layer_audit_<timestamp>.html
#   08_docs/random_external_layer_audit/output/random_external_layer_ranked_plan_<timestamp>.csv
#
# Important path note:
#   Files under 08_docs are QA/audit reports and handoff inputs only.
#   The production BRIM map build does not read these audit files.
#
# Safe defaults / controls:
#   BRIM_RANDOM_ADDS_LIVE_PROBE      TRUE/FALSE; default TRUE
#   BRIM_RANDOM_ADDS_TIMEOUT_SEC     seconds per public request; default 12
#   BRIM_RANDOM_ADDS_MAX_LAYER_PROBE maximum layer-detail probes per service; default 10
#   BRIM_RANDOM_ADDS_INVENTORY       optional override inventory CSV path
# -----------------------------------------------------------------------------

# ---- Small utilities ---------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

nonempty <- function(x) {
  !is.null(x) && length(x) > 0 && !is.na(x[1]) && nzchar(trimws(as.character(x[1])))
}

as_chr <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("")
  paste(as.character(x), collapse = "; ")
}

clean_chr <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  trimws(x)
}

split_multi <- function(x) {
  x <- clean_chr(x)
  if (!nzchar(x)) return(character(0))
  parts <- unlist(strsplit(x, "\\s*;\\s*|\\s*\\|\\s*", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

parse_layer_ids <- function(x) {
  x <- clean_chr(x)
  if (!nzchar(x)) return(character(0))
  if (grepl("audit\\s+all", x, ignore.case = TRUE)) return("__AUDIT_ALL__")
  nums <- regmatches(x, gregexpr("\\d+", x, perl = TRUE))[[1]]
  unique(nums[nzchar(nums)])
}

normalize_url <- function(x) {
  x <- clean_chr(x)
  x <- sub("#.*$", "", x)
  x <- sub("\\?.*$", "", x)
  x <- gsub("/+$", "", x)
  tolower(x)
}

append_pjson <- function(url) {
  url <- clean_chr(url)
  if (!nzchar(url)) return("")
  sep <- if (grepl("\\?", url)) "&" else "?"
  paste0(url, sep, "f=pjson")
}

append_legend_pjson <- function(url) {
  url <- clean_chr(url)
  if (!nzchar(url)) return("")
  url <- sub("\\?.*$", "", url)
  url <- gsub("/+$", "", url)
  paste0(url, "/legend?f=pjson")
}

arcgis_service_type <- function(url, guess = "") {
  z <- paste(url, guess, collapse = " ")
  if (grepl("FeatureServer", z, ignore.case = TRUE)) return("FeatureServer")
  if (grepl("ImageServer", z, ignore.case = TRUE)) return("ImageServer")
  if (grepl("MapServer", z, ignore.case = TRUE)) return("MapServer")
  if (grepl("WMTS", z, ignore.case = TRUE)) return("WMTS")
  if (grepl("WMS", z, ignore.case = TRUE)) return("WMS")
  if (grepl("API", z, ignore.case = TRUE)) return("API")
  "Other"
}

is_arcgis <- function(url) {
  grepl("/(FeatureServer|MapServer|ImageServer)(/|$)", clean_chr(url), ignore.case = TRUE)
}

is_exact_arcgis_layer_url <- function(url) {
  grepl("/(FeatureServer|MapServer)/[0-9]+/?$", normalize_url(url), ignore.case = TRUE)
}

service_root_from_url <- function(url) {
  url <- clean_chr(url)
  if (!nzchar(url)) return("")
  url <- sub("\\?.*$", "", url)
  url <- gsub("/+$", "", url)
  sub("/(FeatureServer|MapServer)/[0-9]+$", "/\\1", url, ignore.case = TRUE)
}

layer_id_from_exact_url <- function(url) {
  z <- normalize_url(url)
  m <- regmatches(z, regexpr("/(featureserver|mapserver)/([0-9]+)$", z, perl = TRUE, ignore.case = TRUE))
  if (!length(m) || m == "") return(NA_character_)
  sub("^.*(/featureserver|/mapserver)/", "", m, ignore.case = TRUE)
}

fmt_wkid <- function(sr) {
  if (is.null(sr)) return("")
  wkid <- sr$latestWkid %||% sr$wkid %||% sr$wkt %||% ""
  as_chr(wkid)
}

fmt_extent <- function(ext) {
  if (is.null(ext)) return("")
  vals <- c(ext$xmin, ext$ymin, ext$xmax, ext$ymax)
  if (length(vals) < 4 || any(vapply(vals, is.null, logical(1)))) return("")
  paste(round(as.numeric(vals), 4), collapse = ", ")
}

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

# ---- Project paths -----------------------------------------------------------

find_project_root <- function() {
  candidates <- c(
    getwd(),
    normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE),
    "C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2"
  )
  for (p in unique(candidates)) {
    if (file.exists(file.path(p, "00_config", "external_service_catalog.csv"))) {
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
    }
  }
  stop(
    "Could not find BRIM project root. Please setwd() to PortaTreasure2 or edit find_project_root()."
  )
}


# ---- Bundled fallback inventory ---------------------------------------------
# This keeps EL_001 runnable even if only the script file was copied from the
# patch ZIP and the audit input folder was missed during unzip/copy.
# The preferred state is still to keep the CSV under 08_docs/random_external_layer_audit/input/.

write_bundled_random_adds_inventory <- function(path) {
  bundled_lines <- c(
  "id,title,provider,theme,desire,strength,custom,ease,linkOnly,likelyPanel,uiTier,serviceRootUrl,layerIds,exactLayerUrls,landingPage,secondaryUrl,serviceTypeGuess,sourceStatus,geometryGuess,currentTake,firstAuditTask,batch",
  "AML_CA_FEATURES,CA mapped abandoned mine features,California DOC / Division of Mine Reclamation,AML / hazards / water quality,5,4.5,3.5,4,No,External or Hazards/Water Quality,A,https://services2.arcgis.com/zr3KAIbsRSUyARHG/ArcGIS/rest/services/Mapped_Abandoned_Mine_Features/FeatureServer,0,https://services2.arcgis.com/zr3KAIbsRSUyARHG/ArcGIS/rest/services/Mapped_Abandoned_Mine_Features/FeatureServer/0,https://www.conservation.ca.gov/dmr/abandoned_mine_lands,https://services2.arcgis.com/zr3KAIbsRSUyARHG/ArcGIS/rest/services/Mapped_Abandoned_Mine_Features/FeatureServer?f=pjson,ArcGIS FeatureServer,Known service lead,Point,Core BLM screening layer; high relevance to BLM-CA and water-quality screening. Must caveat generalized/obfuscated/incomplete locations.,\"Inspect fields, status categories, query/GeoJSON support, symbology, and mandatory caveat text.\",Batch 1",
  "AML_PAMP,Principal Areas of Mine Pollution / mine pollution areas,California DOC / Division of Mine Reclamation,AML / water quality,5,4,3,2.5,Maybe,Water Quality / AML,A/B,,,,https://www.conservation.ca.gov/dmr/abandoned_mine_lands/pamp,https://maps.conservation.ca.gov/dmr/,Unknown; likely downloadable/static or request-limited,Needs source/data-path audit,Polygon/point/tabular unknown,Potentially more hydrology-relevant than mine points if accessible.,Find actual downloadable GIS/service endpoint and determine public BRIM appropriateness.,Batch 2",
  "ACTIVE_MINES_SMARA,Active mines / SMARA / DOC mine status,California DOC / Division of Mine Reclamation,Mining / water quality / reclamation,4,4,3.5,3.5,No/Maybe,Mining / Water Quality,B,,,,https://maps.conservation.ca.gov/dmr/,https://www.conservation.ca.gov/dmr,ArcGIS service likely; endpoint TBD,Needs service endpoint audit,Point/polygon,\"AML alone is not enough; current regulated mining matters for pits, tailings, reclamation, and water quality.\",Identify mine-status service and layer IDs from DOC/DMR map services.,Batch 2",
  "CA_GDE_NCCAG,California NCCAG / groundwater-dependent ecosystems / iGDE,California DWR,Groundwater / ecology / springs / wetlands,5,4.5,4,4,No,Groundwater / Biota / Hydro Screening,A,https://gis.water.ca.gov/arcgis/rest/services/Biota/i02_NaturalCommunitiesCommonlyAssociatedWithGroundwater/MapServer,\"0,1\",https://gis.water.ca.gov/arcgis/rest/services/Biota/i02_NaturalCommunitiesCommonlyAssociatedWithGroundwater/MapServer/0 ; https://gis.water.ca.gov/arcgis/rest/services/Biota/i02_NaturalCommunitiesCommonlyAssociatedWithGroundwater/MapServer/1,https://groundwaterresourcehub.org/where-we-work/california/mapping-indicators-gdes/,https://gis.water.ca.gov/arcgis/rest/services/Biota/i02_NaturalCommunitiesCommonlyAssociatedWithGroundwater/MapServer?f=pjson,ArcGIS MapServer,Known service lead,Polygon,\"One of the biggest hydro/ecology gaps; valuable for springs, wetlands, phreatophytes, SGMA/GDE context. Screening/start-point only.\",\"Confirm layer names, fields, legends, display scale, and DWR/NCCAG disclaimer language.\",Batch 1",
  "IMPAIRED_WATERS,303(d) / 305(b) impaired waters / TMDL context,California State Water Resources Control Board,Water quality,5,4.5,4,4,No,Water Quality,A,https://gispublic.waterboards.ca.gov/arcgis/rest/services/Water_Quality/Impaired_waters/MapServer,Audit all,,https://www.waterboards.ca.gov/water_issues/programs/tmdl/integrated2010.shtml,https://gispublic.waterboards.ca.gov/arcgis/rest/services/Water_Quality/Impaired_waters/MapServer?f=pjson,ArcGIS MapServer,Known service lead but currency must be verified,Mixed point/line/polygon or line/polygon,Core hydro screening layer; high BRIM value. Currency/listing cycle must be verified.,Verify current/best Water Boards service for latest 303(d)/305(b)/TMDL layers and identify sublayers.,Batch 1",
  "INSAR_SUBSIDENCE,DWR / TRE Altamira InSAR land subsidence,California DWR / CNRA / TRE Altamira,Groundwater / infrastructure,4.5,4.5,3.5,3,No/Maybe,Groundwater / Central Valley,A/B,,,,https://data.cnra.ca.gov/dataset/tre-altamira-insar-subsidence,https://water.ca.gov/Programs/Groundwater-Management/Data-and-Tools,Download/static raster/vector or service TBD,Needs direct service/data-path audit,Raster or point/grid,\"High-value groundwater stress/subsidence layer, especially Central Valley and infrastructure context.\",\"Resolve current CNRA/DWR data URL, format, time slices, and best summary layer for BRIM.\",Batch 2",
  "NOHRSC_SNOW,NOHRSC snow analysis \u2014 snow depth and SWE,NOAA / NWS / NOHRSC,Snow / water supply,4.5,5,4,4.5,No,Ops Live / Snow,A,https://mapservices.weather.noaa.gov/raster/rest/services/snow/NOHRSC_Snow_Analysis/MapServer,\"Audit 0,3,4,7 and all named layers\",https://mapservices.weather.noaa.gov/raster/rest/services/snow/NOHRSC_Snow_Analysis/MapServer/0 ; https://mapservices.weather.noaa.gov/raster/rest/services/snow/NOHRSC_Snow_Analysis/MapServer/3 ; https://mapservices.weather.noaa.gov/raster/rest/services/snow/NOHRSC_Snow_Analysis/MapServer/4 ; https://mapservices.weather.noaa.gov/raster/rest/services/snow/NOHRSC_Snow_Analysis/MapServer/7,https://www.nohrsc.noaa.gov/,https://mapservices.weather.noaa.gov/raster/rest/services/snow/NOHRSC_Snow_Analysis/MapServer?f=pjson,ArcGIS MapServer raster/mosaic,Known service lead,Raster/mosaic,Strong easy add; complements BRIM station SWE layer with gridded snow context.,\"Identify correct Snow Depth and SWE layer IDs, units, legend, transparency, and exportImage behavior.\",Batch 1",
  "SPC_FIREWX,SPC fire weather outlooks,NOAA / NWS / Storm Prediction Center,Fire / wind / forecast,4.5,5,4,4.5,No,Fire / Forecasts,A/B,https://mapservices.weather.noaa.gov/vector/rest/services/fire_weather/SPC_firewx/MapServer,Audit all Day 1\u20138 sublayers,,https://www.spc.noaa.gov/products/fire_wx/,https://mapservices.weather.noaa.gov/vector/rest/services/fire_weather/SPC_firewx/MapServer?f=pjson,ArcGIS MapServer vector polygons,Known service lead,Polygon,\"Very strong BLM/BRIM fit, especially with wind, fire, drought, and hazards.\",\"Inspect sublayer IDs, category fields, update/valid time fields, legend, and source-link fields.\",Batch 1",
  "SPC_MESO_DISC,SPC mesoscale discussions,NOAA / NWS / Storm Prediction Center,Active weather / severe / fire-wind / winter,4,4.5,4,4.5,Maybe,Forecasts / Active Weather,A/B,https://mapservices.weather.noaa.gov/vector/rest/services/outlooks/spc_mesoscale_discussion/MapServer,Audit all,,https://www.spc.noaa.gov/products/md/,https://mapservices.weather.noaa.gov/vector/rest/services/outlooks/spc_mesoscale_discussion/MapServer?f=pjson,ArcGIS MapServer vector polygons,Known service lead,Polygon,Worth real estate even if initially a link; excellent active-event situational awareness.,\"Check fields for MD number, issue/valid/expire times, discussion URL, and current/expired behavior.\",Batch 1",
  "SPC_CONVECTIVE,SPC convective outlooks,NOAA / NWS / Storm Prediction Center,Severe weather outlooks,3.5,4.5,4,4.5,No,Forecasts / Severe Weather,B,https://mapservices.weather.noaa.gov/vector/rest/services/outlooks/SPC_wx_outlks/MapServer,Audit all Day 1\u20138 categorical/probabilistic sublayers,,https://www.spc.noaa.gov/products/outlook/,https://mapservices.weather.noaa.gov/vector/rest/services/outlooks/SPC_wx_outlks/MapServer?f=pjson,ArcGIS MapServer vector polygons,Known service lead,Polygon,Popular and easy; useful situational context but less CA-core than fire weather.,Identify day/risk/probability layer IDs and valid-time fields.,Batch 3",
  "WPC_WSSI,WPC Winter Storm Severity Index,NOAA / NWS / WPC,Snow impacts / winter forecast,3.5,4.5,4,4,No,Forecasts / Snow Impacts,B,https://mapservices.weather.noaa.gov/vector/rest/services/outlooks/wpc_wssi/MapServer,Audit all,,https://www.wpc.ncep.noaa.gov/wwd/wssi/wssi.php,https://mapservices.weather.noaa.gov/vector/rest/services/outlooks/wpc_wssi/MapServer?f=pjson,ArcGIS MapServer vector/raster unknown; audit,Known service lead,Polygon/raster categories,Impact framing is decision-friendly; useful but seasonal.,\"Inspect layer IDs, category values, legend, and day/time sublayers.\",Batch 3",
  "CPC_HAZARDS,CPC Day 8\u201314 hazards / rapid-onset drought / week-2 hazards,NOAA / NWS / CPC,Climate hazards / drought / heat / wind,4,4,3.5,3.5,No/Maybe,Forecasts / Drought / Hazards,B,,,,https://www.cpc.ncep.noaa.gov/products/predictions/threats/threats.php,https://www.drought.gov/forecasts,Unknown; likely GIS download/feed or web product,Needs service/data-path audit,Polygon/raster unknown,\"Good compact NOAA next-week risk layer, especially heat, wind, snow, and rapid-onset drought.\",Resolve ArcGIS/GeoJSON/shapefile feed and category mapping if available.,Batch 3",
  "CPC_WEEK34,CPC Week 3\u20134 outlooks,NOAA / NWS / CPC,Climate outlook,3.5,4,3,3.5,Maybe,Climate Outlooks,B/C,,,,https://www.cpc.ncep.noaa.gov/products/predictions/WK34/,https://www.cpc.ncep.noaa.gov/,Unknown,Needs service/data-path audit,Raster/polygon outlook,\"Useful planning context, but compact only.\",Find GIS-ready service/download if available and compare to existing CPC layers in BRIM.,Batch 3",
  "CPC_MONTH_SEASON,CPC monthly / seasonal outlooks,NOAA / NWS / CPC,Climate outlook,3,4,3,3.5,Maybe,Climate Outlooks,C,,,,https://www.cpc.ncep.noaa.gov/products/predictions/long_range/,https://www.climate.gov/maps-data/dataset/temperature-precipitation-and-drought-outlooks-prepared-maps,Unknown,Needs service/data-path audit,Raster/polygon outlook,\"Good background context, not main real estate.\",Find best GIS-ready source and decide map-layer vs link-only.,Batch 3",
  "NWM_SOIL_MOISTURE,NOAA National Water Model Land Analysis near-surface soil moisture saturation,NOAA / NWS / Office of Water Prediction,Soil moisture / current wetness,4,4.5,3.5,5,No,Ops Live / Soil Moisture,A/B,https://mapservices.weather.noaa.gov/raster/rest/services/obs/NWM_Land_Analysis/MapServer,\"Audit 0,3 and all named soil moisture layers\",https://mapservices.weather.noaa.gov/raster/rest/services/obs/NWM_Land_Analysis/MapServer/0 ; https://mapservices.weather.noaa.gov/raster/rest/services/obs/NWM_Land_Analysis/MapServer/3,https://water.noaa.gov/about/nwm,https://mapservices.weather.noaa.gov/raster/rest/services/obs/NWM_Land_Analysis/MapServer?f=pjson,ArcGIS MapServer raster,Known service lead,Raster,Easiest soil-moisture add; good first pass despite lower interpretability than percentiles.,\"Confirm correct layer ID, units, legend, exportImage parameters, and update timestamp metadata.\",Batch 1",
  "SPORT_LIS,NASA SPoRT-LIS soil moisture percentiles,NASA SPoRT / Drought.gov,Soil moisture / drought / fire / watershed,4.5,4.5,3.5,3.5,No/Maybe,Drought / Soil Moisture,A/B,,,,https://www.drought.gov/topics/soil-moisture,https://weather.msfc.nasa.gov/sport/case_studies/lis_CONUS.html,Raster/GIS download or service TBD,Needs service/data-path audit,Raster,Best soil-moisture meaning because percentiles/anomalies beat raw saturation for decisions.,Find actual current SPoRT-LIS data/service endpoint and identify shallow/root-zone percentile layers.,Batch 2",
  "CPC_NLDAS_SOIL,CPC NLDAS Noah soil moisture percentiles,NOAA / CPC / Drought.gov,Soil moisture / drought,3.5,4,3.5,3.5,No/Maybe,Drought / Soil Moisture,B,,,,https://www.drought.gov/indicators/soil-moisture-product-dashboard/data,https://www.cpc.ncep.noaa.gov/products/Soilmst_Monitoring/,GeoTIFF/derived data or service TBD,Needs service/data-path audit,Raster,Good interpretable NOAA/CPC backup.,Find GIS-ready endpoint/download and compare redundancy with SPoRT-LIS and NWM.,Batch 3",
  "CROP_CASMA,Crop-CASMA soil moisture anomaly,USDA NASS / NASA / Drought.gov,Soil moisture / ag drought / anomaly,3.5,4,3,3,Maybe,Soil Moisture / Ag Drought,B/C,,,,https://www.drought.gov/indicators/soil-moisture-product-dashboard/data,https://nassgeo.csiss.gmu.edu/CropCASMA/,GeoTIFF/web app/API TBD,Needs service/data-path audit,Raster,\"Anomaly is useful, but behind SPoRT/NWM/CPC for BRIM priority.\",Check if current anomaly raster is available in a BRIM-friendly endpoint.,Batch 3",
  "SMAP_GRACE,SMAP / GRACE broad soil moisture products,NASA / Drought.gov,Broad drought / remote sensing,2.5,4,2.5,2.5,Maybe,Drought Explore,C,,,,https://www.drought.gov/indicators/soil-moisture-product-dashboard/data,https://grace.jpl.nasa.gov/data/get-data/,Raster/download/API TBD,Source pool only unless easy endpoint found,Raster,\"Scientifically useful, but probably too coarse for first-round BRIM.\",Only audit after higher-priority soil moisture/vegetation products.,Batch 3",
  "BETTER_WIND_OBS,Better wind \u2014 NOAA surface observations / MADIS wind barbs,NOAA / NWS / MADIS,Wind observations,4,4,3.5,4,No,Obs / Wind,A,https://mapservices.weather.noaa.gov/vector/rest/services/obs/surface_obs/MapServer,Audit wind barb / station sublayers,,https://madis.ncep.noaa.gov/,https://mapservices.weather.noaa.gov/vector/rest/services/obs/surface_obs/MapServer?f=pjson,ArcGIS MapServer vector,Known service lead,Point/barbs,Best near-term fix for weak sparse wind layer.,\"Identify wind-specific sublayers, fields, scales, symbology options, and whether custom symbolization is possible.\",Batch 1",
  "NDFD_WIND,NDFD / gridded forecast wind,NOAA / NWS / NDFD,Wind forecast,4,4,3.5,3.5,No/Maybe,Forecasts / Wind,A/B,,,,https://digital.weather.gov/,https://digital.weather.gov/staticpages/mapservices.php,WMS/ArcGIS/other service TBD,Needs service audit,Raster/grid,Probably more useful than more station points if implemented cleanly.,\"Resolve best forecast wind endpoint, layer names, valid-time controls, and legend.\",Batch 2",
  "QUICKDRI,QuickDRI,NDMC / Drought.gov,Flash drought / recent drydown,4,4,3.5,3.5,No/Maybe,Drought / Vegetation Stress,A/B,,,,https://www.drought.gov/data-maps-tools/quick-drought-response-index-quickdri,https://quickdri.unl.edu/,Raster/download/service TBD,Needs service/data-path audit,Raster,Strongest recent drydown / flash drought vegetation-drought layer.,Find GIS endpoint/download and determine tiles/raster overlay feasibility.,Batch 2",
  "VEGDRI,VegDRI,NDMC / Drought.gov,Vegetation drought stress / rangeland,4,4,3.5,3,No/Maybe,Drought / Vegetation Stress,A/B,,,,https://vegdri.unl.edu/,https://www.drought.gov/topics/vegetation,Raster/download/service TBD,Needs service/data-path audit,Raster,Better than raw NDVI because drought stress is already interpreted.,\"Find GIS endpoint/download, category legend, and current update cadence.\",Batch 2",
  "NOAA_STAR_VHI,NOAA STAR VHI / VCI / TCI vegetation health,NOAA STAR / Drought.gov,Vegetation stress / drought / fire risk,3.5,4,3,3,No/Maybe,Drought / Vegetation,B,,,,https://www.drought.gov/data-maps-tools/noaa-star-global-vegetation-health-products,https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH/index.php,GeoTIFF/NetCDF/images/service TBD,Needs service/data-path audit,Raster,Interpreted vegetation-health family; better than raw greenness.,Identify VHI/VCI/TCI data formats and whether web tiles or static preprocessing are best.,Batch 3",
  "NOAA_STAR_NDVI,NOAA STAR NDVI / greenness products,NOAA STAR / Drought.gov,Raw greenness / vegetation context,3,3.5,2.5,3,Maybe,Vegetation Explore,C/B,,,,https://www.drought.gov/data-maps-tools/ndvi-greenness-maps,https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH/index.php,GeoTIFF/KML/images/service TBD,Needs service/data-path audit,Raster,\"Keep, but raw greenness needs anomaly/context to be decision-useful.\",Check whether anomaly/percentile-style products are available from same source.,Batch 3",
  "NASA_GIBS_NDVI,NASA GIBS MODIS/VIIRS NDVI-style imagery,NASA EOSDIS GIBS,Satellite / vegetation imagery,3,4,3,3.5,Maybe,Satellite / Vegetation Explore,C,https://gibs.earthdata.nasa.gov/wmts/epsg3857/best/wmts.cgi,Audit NDVI/vegetation layer names,,https://www.earthdata.nasa.gov/engage/open-data-services-software/earthdata-developer-portal/gibs-api,https://gibs.earthdata.nasa.gov/,WMTS/WMS,Known service family; layer names TBD,Raster tiles,Technically promising web imagery but should stay explore-tier unless date/legend is clean.,\"Fetch WMTS capabilities, identify NDVI/vegetation layers, time dimension, and Leaflet syntax.\",Batch 3",
  "RAILROADS,Railroads,FRA / BTS / NTAD / Esri service lead,ROW / infrastructure,4,4,3.5,4,No,ROW / Infrastructure,B,https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Railroads/FeatureServer,Audit all/0,https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Railroads/FeatureServer/0,https://railroads.dot.gov/rail-network-development/maps-and-data/maps-geographic-information-system/maps-geographic,,ArcGIS FeatureServer,Service lead; authority/stability audit needed,Line,Good compact ROW/context layer.,\"Verify service source, date, fields, query support, and CA extent performance.\",Batch 2",
  "HIGHWAYS_NHS,Major highways / National Highway System,Caltrans,ROW / access / infrastructure,4,4.5,4,4,No,ROW / Access / Infrastructure,A/B,https://caltrans-gis.dot.ca.gov/arcgis/rest/services/CHhighway/National_Highway_System/FeatureServer,Audit all/0,,https://gisdata-caltrans.opendata.arcgis.com/,https://caltrans-gis.dot.ca.gov/arcgis/rest/services/CHhighway/National_Highway_System/FeatureServer?f=pjson,ArcGIS FeatureServer,Known service lead,Line,Cleaner than all-roads and useful for ROW/access screening.,\"Inspect layer IDs/fields, scale visibility, and whether functional class alternatives are needed.\",Batch 1",
  "TRANSMISSION_LINES,Electric transmission lines,HIFLD / EIA / state source TBD,ROW / energy / fire / infrastructure,4,3.5,3.5,3.5,No/Maybe,ROW / Energy / Fire,B,,,,https://hifld-geoplatform.opendata.arcgis.com/,https://www.eia.gov/maps/layer_info-m.php,FeatureServer/download TBD,Needs authority/stability audit,Line,\"High relevance, but vet source before trusting in BRIM.\",\"Compare HIFLD, EIA, CEC, and authoritative CA transmission services; choose best public endpoint.\",Batch 2",
  "SUBSTATIONS,Electric substations,HIFLD / EIA / state source TBD,Infrastructure / energy,3.5,3.5,3,3.5,Maybe,Infrastructure / Energy Explore,C/B,,,,https://hifld-geoplatform.opendata.arcgis.com/,https://www.eia.gov/maps/layer_info-m.php,FeatureServer/download TBD,Needs source authority audit,Point,Useful but compact/explore treatment is safer.,Determine whether public display is appropriate and whether fields are useful.,Batch 3",
  "CALGEM_WELLS,\"CalGEM oil/gas/geothermal wells, fields, leases, notices, permits\",California DOC / CalGEM,Energy / water quality / legacy infrastructure,4.5,4.5,4,4,No,Energy / Water Quality / Infrastructure,A/B,https://gis.conservation.ca.gov/server/rest/services/WellSTAR/Notices/MapServer,\"Audit 0,1,3,5,6,7 and find well-status/fields/leases services\",https://gis.conservation.ca.gov/server/rest/services/WellSTAR/Notices/MapServer/0 ; https://gis.conservation.ca.gov/server/rest/services/WellSTAR/Notices/MapServer/1 ; https://gis.conservation.ca.gov/server/rest/services/WellSTAR/Notices/MapServer/3,https://maps.conservation.ca.gov/doggr/,https://gis.conservation.ca.gov/server/rest/services/WellSTAR/Notices/MapServer?f=pjson,ArcGIS MapServer,\"Known service lead, but full well services need audit\",Point/polygon,Strong hydro/contamination/legacy infrastructure relevance.,\"Find best CalGEM services for well locations/status, fields, leases, and permits; define compact layer set.\",Batch 2",
  "DRECP,DRECP / renewable energy zones / conservation designations,CEC / BLM / CDFW / USFWS / Data Basin,Renewable energy / desert / ROW / conservation,4.5,4,3.5,3.5,No/Maybe,Renewable Energy / Desert / ROW,A/B,,,,https://www.energy.ca.gov/programs-and-topics/programs/desert-renewable-energy-conservation-plan,https://drecp.databasin.org/,FeatureServer/download/Data Basin services TBD,Needs service/data-path audit,Polygon,\"Very BLM-CA relevant for desert ROW, solar/wind/geothermal, washes, groundwater, and conservation.\",Resolve authoritative layer package/service for DRECP BLM land-use plan designations and renewable energy areas.,Batch 2",
  "BLM_RENEWABLE_PROJECTS,BLM CA renewable energy projects,BLM eGIS / GeoPlatform,Renewable energy / ROW / cumulative projects,4,4,3.5,3.5,No/Maybe,Renewable Energy / ROW,B,,,,https://gbp-blm-egis.hub.arcgis.com/,https://gbp-blm-egis.hub.arcgis.com/maps/9b663af4613847d7a3ec1c1a81a02c85,ArcGIS FeatureServer likely; endpoint TBD,Needs endpoint audit from hub item,Point/polygon,Useful for cumulative development context and BLM ROW work.,Open hub item metadata and locate exact data/service URL and fields.,Batch 2",
  "BLM_RECREATION,BLM CA recreation sites / recreation areas / OHV,BLM California,Recreation / access / water-use impacts,3.5,4.5,4,4.5,No,BLM / Recreation / Access,B/C,https://gis.blm.gov/caarcgis/rest/services/Recreation/BLM_CA_RECS/FeatureServer,\"0,1\",https://gis.blm.gov/caarcgis/rest/services/Recreation/BLM_CA_RECS/FeatureServer/0 ; https://gis.blm.gov/caarcgis/rest/services/Recreation/BLM_CA_RECS/FeatureServer/1,https://www.blm.gov/visit,https://gis.blm.gov/caarcgis/rest/services/Recreation/BLM_CA_RECS/FeatureServer?f=pjson,ArcGIS FeatureServer,Known service lead,Point/polygon,\"Useful near springs, water access, campgrounds, OHV, recreation areas, and visitor impacts.\",Inspect fields/categories and choose a compact subset or category styling.,Batch 1",
  "FRAP_VEG_FIRE,\"CAL FIRE / FRAP vegetation, fuels, burn severity\",CAL FIRE / FRAP,Fire / vegetation / watershed condition,3.5,4,3.5,3.5,No/Maybe,Fire / Watershed Condition,B/C,,,,https://hub-calfire-forestry.hub.arcgis.com/search?tags=frap,https://frap.fire.ca.gov/mapping/gis-data/,FeatureServer/raster/download TBD,Needs FRAP source audit,Raster/polygon,\"Fuels, vegetation, and burn severity matter for watershed condition and post-fire hydrology.\",Find authoritative FRAP services for vegetation/fuels/burn severity and choose non-duplicative BRIM layers.,Batch 2",
  "SWPC_SPACE,\"SWPC space-weather alerts, watches, warnings, K-index\",NOAA / Space Weather Prediction Center,NOAA / infrastructure / communications,2.5,4,2,2.5,Yes/Maybe,NOAA Explore / Infrastructure Link,C/link,,,,https://www.swpc.noaa.gov/products/alerts-watches-and-warnings,https://www.swpc.noaa.gov/products/planetary-k-index,JSON/status pages/API TBD,Needs only light audit,Non-map/status,\"Mostly link/status value, not a core BRIM map layer.\",Check whether compact status link is worthwhile; otherwise defer.,Batch 3",
  "SWPC_AURORA,SWPC aurora 30-minute forecast,NOAA / Space Weather Prediction Center,Space weather / novelty / broad awareness,2,3.5,2.5,2.5,Yes/Maybe,NOAA Explore,C/link,,,,https://www.swpc.noaa.gov/products/aurora-30-minute-forecast,https://services.swpc.noaa.gov/,Image/JSON/geospatial feed TBD,Low-priority audit,Raster/forecast image,Real geospatial forecast concept but mostly novelty for BRIM.,Defer unless adding compact NOAA explore bucket.,Batch 3"
)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(bundled_lines, path, useBytes = TRUE)
  invisible(path)
}

project_root <- find_project_root()
message("BRIM project root: ", project_root)

input_default <- file.path(
  project_root,
  "08_docs", "random_external_layer_audit", "input",
  "BRIM_random_adds_master_inventory.csv"
)
inventory_csv <- Sys.getenv("BRIM_RANDOM_ADDS_INVENTORY", unset = input_default)
external_catalog_csv <- file.path(project_root, "00_config", "external_service_catalog.csv")
output_dir <- file.path(project_root, "08_docs", "random_external_layer_audit", "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(inventory_csv) && identical(normalizePath(inventory_csv, winslash = "/", mustWork = FALSE), normalizePath(input_default, winslash = "/", mustWork = FALSE))) {
  message("Inventory CSV not found at expected path; writing bundled EL_001 fallback inventory to:")
  message("  ", inventory_csv)
  write_bundled_random_adds_inventory(inventory_csv)
}

if (!file.exists(inventory_csv)) {
  stop(
    "Inventory CSV not found: ", inventory_csv, "\n",
    "If you intentionally keep the inventory somewhere else, rerun with:\n",
    "  Sys.setenv(BRIM_RANDOM_ADDS_INVENTORY = 'full/path/to/BRIM_random_adds_master_inventory.csv')"
  )
}
if (!file.exists(external_catalog_csv)) stop("External catalog not found: ", external_catalog_csv)

# ---- Settings ----------------------------------------------------------------

live_probe <- toupper(Sys.getenv("BRIM_RANDOM_ADDS_LIVE_PROBE", unset = "TRUE")) %in% c("TRUE", "T", "1", "YES", "Y")
timeout_sec <- as.numeric(Sys.getenv("BRIM_RANDOM_ADDS_TIMEOUT_SEC", unset = "12"))
max_layer_probe <- as.integer(Sys.getenv("BRIM_RANDOM_ADDS_MAX_LAYER_PROBE", unset = "10"))
if (!is.finite(timeout_sec) || timeout_sec <= 0) timeout_sec <- 12
if (is.na(max_layer_probe) || max_layer_probe < 0) max_layer_probe <- 10

message("Live ArcGIS probe: ", live_probe)
message("Timeout per request: ", timeout_sec, " sec")
message("Max layer-detail probes per service: ", max_layer_probe)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required for EL_001 audit JSON outputs/probes.")
}

# ---- Input data --------------------------------------------------------------

inventory <- read.csv(inventory_csv, stringsAsFactors = FALSE, check.names = FALSE)
catalog <- read.csv(external_catalog_csv, stringsAsFactors = FALSE, check.names = FALSE)

required_cols <- c("id", "title", "provider", "theme", "desire", "strength", "custom", "ease", "linkOnly", "likelyPanel", "uiTier", "serviceRootUrl", "layerIds", "exactLayerUrls", "landingPage", "secondaryUrl", "serviceTypeGuess", "currentTake", "firstAuditTask", "batch")
missing_cols <- setdiff(required_cols, names(inventory))
if (length(missing_cols)) {
  stop("Inventory is missing required columns: ", paste(missing_cols, collapse = ", "))
}

catalog_url_cols <- intersect(c("service_url", "source_page", "identify_url", "legend_url"), names(catalog))
catalog_urls <- unique(normalize_url(unlist(catalog[catalog_url_cols], use.names = FALSE)))
catalog_urls <- catalog_urls[nzchar(catalog_urls)]

catalog_display <- if ("display_name" %in% names(catalog)) catalog$display_name else character(nrow(catalog))
catalog_theme <- if ("theme" %in% names(catalog)) catalog$theme else character(nrow(catalog))

# ---- Public endpoint helpers -------------------------------------------------

fetch_text <- function(url, timeout = 12) {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(timeout, old_timeout %||% timeout))
  con <- NULL
  out <- tryCatch({
    con <- url(url, open = "rb")
    txt <- readLines(con, warn = FALSE, encoding = "UTF-8")
    paste(txt, collapse = "\n")
  }, error = function(e) {
    structure(NA_character_, error_message = conditionMessage(e))
  }, finally = {
    if (!is.null(con)) try(close(con), silent = TRUE)
  })
  out
}

probe_json <- function(url) {
  if (!live_probe || !nzchar(clean_chr(url))) {
    return(list(ok = NA, token_required = NA, error = "live_probe_disabled", data = NULL))
  }
  txt <- fetch_text(url, timeout = timeout_sec)
  if (is.na(txt[1])) {
    return(list(ok = FALSE, token_required = NA, error = attr(txt, "error_message") %||% "fetch_failed", data = NULL))
  }
  dat <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) structure(NULL, error_message = conditionMessage(e))
  )
  if (is.null(dat)) {
    return(list(ok = FALSE, token_required = NA, error = attr(dat, "error_message") %||% "json_parse_failed", data = NULL))
  }
  token_required <- FALSE
  if (!is.null(dat$error)) {
    code <- dat$error$code %||% NA
    msg <- paste(dat$error$message %||% "", dat$error$details %||% "", collapse = " ")
    token_required <- code %in% c(498, 499) || grepl("token", msg, ignore.case = TRUE)
    return(list(ok = FALSE, token_required = token_required, error = paste("ArcGIS error", code, msg), data = dat))
  }
  list(ok = TRUE, token_required = FALSE, error = "", data = dat)
}

probe_legend <- function(service_root_url) {
  if (!live_probe || !is_arcgis(service_root_url)) return(NA)
  res <- probe_json(append_legend_pjson(service_root_url))
  if (isTRUE(res$ok)) return(TRUE)
  if (isFALSE(res$ok)) return(FALSE)
  NA
}

# ---- Heuristics --------------------------------------------------------------

existing_matches_for_candidate <- function(candidate_urls, title) {
  candidate_norms <- unique(normalize_url(candidate_urls))
  candidate_norms <- candidate_norms[nzchar(candidate_norms)]
  if (!length(candidate_norms)) return("")

  root_norms <- unique(normalize_url(vapply(candidate_norms, service_root_from_url, character(1))))
  root_norms <- root_norms[nzchar(root_norms)]

  hit <- rep(FALSE, nrow(catalog))
  for (col in catalog_url_cols) {
    cu <- normalize_url(catalog[[col]])
    hit <- hit | cu %in% candidate_norms | cu %in% root_norms
    cu_root <- normalize_url(vapply(cu, service_root_from_url, character(1)))
    hit <- hit | cu_root %in% candidate_norms | cu_root %in% root_norms
  }

  # Light name overlap check. URL matches are the main signal; name matches are labeled as possible.
  if (nzchar(clean_chr(title)) && "display_name" %in% names(catalog)) {
    words <- unique(tolower(unlist(strsplit(gsub("[^A-Za-z0-9 ]", " ", title), "\\s+"))))
    words <- words[nchar(words) >= 6]
    if (length(words) >= 2) {
      nm <- tolower(catalog$display_name)
      name_hit <- vapply(seq_along(nm), function(i) sum(words %in% unlist(strsplit(gsub("[^A-Za-z0-9 ]", " ", nm[i]), "\\s+"))) >= 2, logical(1))
      # Mark possible name overlaps only if no URL hit is found.
      if (!any(hit)) hit <- name_hit
    }
  }

  if (!any(hit)) return("")
  paste(unique(paste0(catalog_display[hit], " [", catalog_theme[hit], "]")), collapse = " | ")
}

choose_popup_fields <- function(layer_data) {
  if (is.null(layer_data$fields)) return(character(0))
  fields <- vapply(layer_data$fields, function(f) f$name %||% "", character(1))
  fields <- fields[nzchar(fields)]
  lower <- tolower(fields)
  priority_patterns <- c("name", "title", "type", "status", "class", "category", "agency", "year", "date", "valid", "expire", "acres", "id")
  score <- rep(0, length(fields))
  for (i in seq_along(priority_patterns)) {
    score <- score + ifelse(grepl(priority_patterns[i], lower), length(priority_patterns) - i + 1, 0)
  }
  fields <- fields[order(score, decreasing = TRUE)]
  fields <- fields[score[order(score, decreasing = TRUE)] > 0]
  unique(head(fields, 8))
}

candidate_caveat <- function(id, title, current_take) {
  key <- paste(id, title, current_take, sep = " ")
  if (grepl("AML_CA_FEATURES", key, ignore.case = TRUE)) return("Locations may be generalized/obfuscated and incomplete; screening only, not a site-specific inventory.")
  if (grepl("GDE|NCCAG|groundwater-dependent", key, ignore.case = TRUE)) return("Screening/start-point layer only; not a field-verified GDE determination.")
  if (grepl("IMPAIRED|303|305|TMDL", key, ignore.case = TRUE)) return("Verify current Integrated Report/listing cycle and layer currency before relying on status.")
  if (grepl("NWM.*SOIL|SOIL.*MOISTURE", key, ignore.case = TRUE)) return("Current/model wetness or saturation context; not a station observation or percentile/anomaly unless explicitly documented.")
  if (grepl("NOHRSC", key, ignore.case = TRUE)) return("Modeled/gridded snow product; complements but does not replace station observations.")
  if (grepl("SPC", key, ignore.case = TRUE)) return("Forecast/outlook/discussion polygons; not warnings, incidents, or observed conditions.")
  if (grepl("grazing|allotment|pasture", key, ignore.case = TRUE)) return("BLM CA grazing allotments/pastures are handled elsewhere; do not add from this inventory.")
  ""
}

rank_score <- function(row, service_type, existing_match, implementation_path) {
  desire <- suppressWarnings(as.numeric(row[["desire"]]))
  strength <- suppressWarnings(as.numeric(row[["strength"]]))
  custom <- suppressWarnings(as.numeric(row[["custom"]]))
  ease <- suppressWarnings(as.numeric(row[["ease"]]))
  base <- 2.0 * desire + 1.25 * strength + 1.1 * custom + 1.1 * ease
  if (nzchar(existing_match)) base <- base - 1.5
  if (implementation_path %in% c("link_only", "defer")) base <- base - 3
  if (service_type %in% c("FeatureServer", "MapServer")) base <- base + 1.25
  if (service_type %in% c("ImageServer", "WMTS", "WMS")) base <- base + 0.5
  round(base, 2)
}

recommend_impl <- function(row, service_type, root_url, exact_urls, layer_ids, existing_match, token_required) {
  id <- clean_chr(row[["id"]])
  link_only <- clean_chr(row[["linkOnly"]])
  guess <- clean_chr(row[["serviceTypeGuess"]])
  if (grepl("grazing|allotment|pasture", paste(id, row[["title"]]), ignore.case = TRUE)) return("defer")
  if (isTRUE(token_required)) return("defer")
  if (nzchar(existing_match)) return("audit_existing_catalog_row")
  if (grepl("^yes$", link_only, ignore.case = TRUE)) return("link_only")
  if (service_type %in% c("FeatureServer", "MapServer") && (nzchar(root_url) || length(exact_urls) || length(layer_ids))) return("direct_arcgis")
  if (service_type == "ImageServer") return("tiled_raster")
  if (service_type %in% c("WMS", "WMTS")) return("wms_wmts")
  if (grepl("download|static|GeoTIFF|Shapefile", guess, ignore.case = TRUE)) return("static_preprocess")
  if (nzchar(clean_chr(row[["landingPage"]]))) return("endpoint_discovery_or_link_only")
  "defer"
}

recommend_panel <- function(row, impl) {
  lp <- clean_chr(row[["likelyPanel"]])
  theme <- clean_chr(row[["theme"]])
  text <- paste(lp, theme)
  if (impl == "audit_existing_catalog_row") return("keep existing panel; audit External/Ops duplication")
  if (grepl("Ops Live|Forecast|Fire|Snow|Active Weather", text, ignore.case = TRUE)) return("External first; consider Ops/both after QA")
  if (impl %in% c("link_only", "endpoint_discovery_or_link_only")) return("External compact/explore")
  "External"
}

source_link_plan <- function(row, root_url) {
  lp <- clean_chr(row[["landingPage"]])
  if (nzchar(lp)) return("Use provider landing/product page for user-facing srce; retain REST endpoint in audit metadata.")
  if (nzchar(root_url)) return("Use service root only if no clearer provider/product page is available.")
  "No source link until a stable provider page is resolved."
}

legend_plan <- function(service_type, legend_supported, root_url) {
  if (isTRUE(legend_supported)) return("Use ArcGIS legend endpoint or provider legend if clearer.")
  if (service_type %in% c("FeatureServer", "MapServer", "ImageServer") && nzchar(root_url)) return("Likely supports ArcGIS legend; verify visually before adding row-level lgnd.")
  "No row-level legend until layer rendering is confirmed."
}

scale_plan <- function(row, geometry_type, service_type) {
  txt <- paste(row[["theme"]], row[["currentTake"]], geometry_type, service_type)
  if (grepl("polygon|mixed|raster|mosaic|large|all", txt, ignore.case = TRUE)) return("Prefer current-view and/or min-zoom guardrails for first add; avoid full-state heavy loads.")
  if (grepl("point", txt, ignore.case = TRUE)) return("Likely OK as current-view or full load after response-size check.")
  "Decide after service/layer count and extent are verified."
}

patch_bucket <- function(id, batch, impl, score, existing_match, service_type) {
  if (nzchar(existing_match)) return("EL_004_existing_row_upgrades")
  if (impl %in% c("defer", "endpoint_discovery_or_link_only")) return("EL_005_endpoint_discovery_or_defer")
  if (impl == "link_only") return("EL_006_compact_explore_links")
  early <- c("AML_CA_FEATURES", "CA_GDE_NCCAG", "NOHRSC_SNOW", "SPC_FIREWX", "SPC_MESO_DISC", "NWM_SOIL_MOISTURE", "HIGHWAYS_NHS", "BLM_RECREATION")
  if (id %in% early && score >= 15) return("EL_003_first_low_risk_add_batch")
  if (grepl("Batch 1", batch, ignore.case = TRUE)) return("EL_003_or_EL_004_after_review")
  if (service_type %in% c("FeatureServer", "MapServer", "ImageServer")) return("EL_005_second_wave_service_layers")
  "EL_006_compact_explore_links"
}

# ---- Candidate audit ---------------------------------------------------------

message("Reading inventory rows: ", nrow(inventory))
message("Reading existing External catalog rows: ", nrow(catalog))

records <- vector("list", nrow(inventory))
summary_rows <- vector("list", nrow(inventory))

for (i in seq_len(nrow(inventory))) {
  row <- inventory[i, , drop = FALSE]
  id <- clean_chr(row[["id"]])
  title <- clean_chr(row[["title"]])
  message("[", i, "/", nrow(inventory), "] Auditing ", id, " - ", title)

  exact_urls <- split_multi(row[["exactLayerUrls"]])
  root_url <- clean_chr(row[["serviceRootUrl"]])
  landing_page <- clean_chr(row[["landingPage"]])
  secondary_url <- clean_chr(row[["secondaryUrl"]])

  if (!nzchar(root_url) && length(exact_urls)) root_url <- service_root_from_url(exact_urls[1])
  if (is_exact_arcgis_layer_url(root_url)) root_url <- service_root_from_url(root_url)

  layer_ids_input <- parse_layer_ids(row[["layerIds"]])
  exact_ids <- unique(na.omit(vapply(exact_urls, layer_id_from_exact_url, character(1))))
  if (length(layer_ids_input) == 0 && length(exact_ids)) layer_ids_input <- exact_ids

  candidate_urls <- c(root_url, exact_urls, landing_page, secondary_url)
  existing_match <- existing_matches_for_candidate(candidate_urls, title)
  service_type <- arcgis_service_type(c(root_url, exact_urls, secondary_url), row[["serviceTypeGuess"]])

  root_probe <- list(ok = NA, token_required = NA, error = "", data = NULL)
  legend_supported <- NA
  root_layer_count <- NA_integer_
  root_layer_ids <- character(0)
  root_layer_names <- character(0)

  if (live_probe && is_arcgis(root_url)) {
    root_probe <- probe_json(append_pjson(root_url))
    legend_supported <- probe_legend(root_url)
  }

  root_data <- root_probe$data
  if (!is.null(root_data$layers)) {
    root_layer_count <- length(root_data$layers)
    root_layer_ids <- vapply(root_data$layers, function(z) as_chr(z$id), character(1))
    root_layer_names <- vapply(root_data$layers, function(z) as_chr(z$name), character(1))
  }

  ids_to_consider <- layer_ids_input
  if (length(ids_to_consider) == 1 && ids_to_consider == "__AUDIT_ALL__") {
    ids_to_consider <- root_layer_ids
  }
  ids_to_consider <- unique(ids_to_consider[nzchar(ids_to_consider)])

  layer_probe_ids <- head(ids_to_consider, max_layer_probe)
  layer_probes <- list()
  if (live_probe && is_arcgis(root_url) && length(layer_probe_ids) && service_type %in% c("FeatureServer", "MapServer")) {
    for (lid in layer_probe_ids) {
      layer_url <- paste0(gsub("/+$", "", root_url), "/", lid)
      layer_probes[[lid]] <- probe_json(append_pjson(layer_url))
    }
  }

  layer_datas <- lapply(layer_probes, function(x) if (isTRUE(x$ok)) x$data else NULL)
  layer_datas <- layer_datas[!vapply(layer_datas, is.null, logical(1))]

  layer_names_to_add <- character(0)
  if (length(ids_to_consider) && length(root_layer_ids)) {
    idx <- match(ids_to_consider, root_layer_ids)
    layer_names_to_add <- root_layer_names[!is.na(idx)]
    names(layer_names_to_add) <- ids_to_consider[!is.na(idx)]
  }
  if (!length(layer_names_to_add) && length(layer_datas)) {
    layer_names_to_add <- vapply(layer_datas, function(d) d$name %||% "", character(1))
  }

  geometry_type <- ""
  if (length(layer_datas)) {
    geometry_type <- paste(unique(vapply(layer_datas, function(d) d$geometryType %||% d$type %||% "", character(1))), collapse = "; ")
  } else if (!is.null(root_data$rasterFunctionInfos) || service_type == "ImageServer") {
    geometry_type <- "Raster/ImageServer"
  } else {
    geometry_type <- clean_chr(row[["geometryGuess"]])
  }

  spatial_reference <- ""
  extent <- ""
  if (length(layer_datas)) {
    spatial_reference <- paste(unique(vapply(layer_datas, function(d) fmt_wkid(d$extent$spatialReference %||% d$sourceSpatialReference %||% d$spatialReference), character(1))), collapse = "; ")
    extent <- paste(unique(vapply(layer_datas, function(d) fmt_extent(d$extent %||% d$fullExtent), character(1))), collapse = " | ")
  }
  if (!nzchar(spatial_reference) && !is.null(root_data$spatialReference)) spatial_reference <- fmt_wkid(root_data$spatialReference)
  if (!nzchar(extent)) extent <- fmt_extent(root_data$fullExtent %||% root_data$initialExtent)

  time_enabled <- NA
  if (length(layer_datas)) {
    time_enabled <- any(vapply(layer_datas, function(d) !is.null(d$timeInfo), logical(1)))
  } else if (!is.null(root_data$timeInfo)) {
    time_enabled <- TRUE
  }

  caps <- paste(
    root_data$capabilities %||% "",
    vapply(layer_datas, function(d) d$capabilities %||% "", character(1)),
    collapse = " "
  )
  supported_formats <- paste(
    root_data$supportedQueryFormats %||% "",
    vapply(layer_datas, function(d) d$supportedQueryFormats %||% "", character(1)),
    collapse = " "
  )
  query_supported <- if (nzchar(trimws(caps))) grepl("Query", caps, ignore.case = TRUE) else NA
  geojson_or_pbf_supported <- if (nzchar(trimws(supported_formats))) grepl("GeoJSON|PBF", supported_formats, ignore.case = TRUE) else NA
  token_required <- root_probe$token_required

  popup_fields <- character(0)
  if (length(layer_datas)) {
    popup_fields <- unique(unlist(lapply(layer_datas, choose_popup_fields), use.names = FALSE))
    popup_fields <- head(popup_fields, 10)
  }

  impl <- recommend_impl(row, service_type, root_url, exact_urls, ids_to_consider, existing_match, token_required)
  panel <- recommend_panel(row, impl)
  ui_tier <- clean_chr(row[["uiTier"]])
  if (!nzchar(ui_tier)) ui_tier <- ifelse(impl == "link_only", "C", "B")
  caveat <- candidate_caveat(id, title, row[["currentTake"]])
  source_plan <- source_link_plan(row, root_url)
  legend_text <- legend_plan(service_type, legend_supported, root_url)
  scale_text <- scale_plan(row, geometry_type, service_type)

  if (length(layer_names_to_add)) {
    label <- paste0(title, " | ", clean_chr(row[["provider"]]))
  } else {
    label <- title
  }

  symbolization <- if (length(layer_datas) && any(vapply(layer_datas, function(d) !is.null(d$drawingInfo), logical(1)))) {
    "Start with provider renderer; only override after field/popup QA."
  } else if (grepl("polygon|line|point", geometry_type, ignore.case = TRUE)) {
    "Use simple BRIM default style initially; add field-based renderer only after QA."
  } else if (grepl("raster|image|mosaic", geometry_type, ignore.case = TRUE)) {
    "Use provider raster renderer/legend and opacity control."
  } else {
    "To be determined after endpoint/layer fields are resolved."
  }

  perf <- c()
  if (!is.na(root_layer_count) && root_layer_count > 10) perf <- c(perf, paste0("Service has ", root_layer_count, " sublayers; add only selected IDs."))
  if (grepl("polygon|mixed|raster|mosaic", geometry_type, ignore.case = TRUE)) perf <- c(perf, "Use current-view/min-zoom or raster export guardrails for first map add.")
  if (length(ids_to_consider) > max_layer_probe) perf <- c(perf, paste0("Only first ", max_layer_probe, " layer details probed; inspect remaining layers before patching."))
  perf <- paste(perf, collapse = " ")

  reject_reason <- ""
  if (impl == "defer") reject_reason <- "Deferred by audit heuristic: no safe public map implementation path, token issue, duplicate/sensitive layer, or handled elsewhere."
  if (impl == "endpoint_discovery_or_link_only") reject_reason <- "Endpoint discovery needed before map-layer add; can remain compact/link-only meanwhile."

  score <- rank_score(row, service_type, existing_match, impl)
  bucket <- patch_bucket(id, clean_chr(row[["batch"]]), impl, score, existing_match, service_type)

  audit_status <- if (isTRUE(root_probe$ok) || impl %in% c("link_only", "audit_existing_catalog_row")) "complete" else if (impl %in% c("defer", "endpoint_discovery_or_link_only")) "deferred" else "in_progress"

  records[[i]] <- list(
    candidate_id = id,
    audit_status = audit_status,
    resolved_title = title,
    resolved_provider = clean_chr(row[["provider"]]),
    resolved_service_root_url = root_url,
    resolved_exact_layer_urls = as.list(exact_urls),
    resolved_landing_page = landing_page,
    service_type = service_type,
    layer_ids_to_add = as.list(ids_to_consider),
    layer_names_to_add = as.list(unname(layer_names_to_add)),
    geometry_or_raster_type = geometry_type,
    spatial_reference = spatial_reference,
    extent = extent,
    update_frequency = "Not audited yet; inspect provider landing page or service metadata before production add.",
    time_enabled = if (is.na(time_enabled)) NULL else isTRUE(time_enabled),
    query_supported = if (is.na(query_supported)) NULL else isTRUE(query_supported),
    geojson_or_pbf_supported = if (is.na(geojson_or_pbf_supported)) NULL else isTRUE(geojson_or_pbf_supported),
    legend_supported = if (is.na(legend_supported)) NULL else isTRUE(legend_supported),
    token_required = if (is.na(token_required)) NULL else isTRUE(token_required),
    recommended_implementation_path = impl,
    recommended_panel = panel,
    recommended_ui_tier = ui_tier,
    recommended_label = label,
    popup_fields = as.list(popup_fields),
    symbolization_plan = symbolization,
    legend_plan = legend_text,
    source_link_plan = source_plan,
    scale_visibility_plan = scale_text,
    mandatory_caveats = caveat,
    performance_notes = perf,
    security_or_sensitivity_notes = if (grepl("AML|mine", paste(id, title), ignore.case = TRUE)) "Avoid implying precise safe/unsafe site locations; use screening-only caveats." else "",
    reject_or_defer_reason = reject_reason,
    first_patch_notes = clean_chr(row[["firstAuditTask"]])
  )

  summary_rows[[i]] <- data.frame(
    candidate_id = id,
    title = title,
    provider = clean_chr(row[["provider"]]),
    theme = clean_chr(row[["theme"]]),
    input_batch = clean_chr(row[["batch"]]),
    desire = suppressWarnings(as.numeric(row[["desire"]])),
    strength = suppressWarnings(as.numeric(row[["strength"]])),
    custom = suppressWarnings(as.numeric(row[["custom"]])),
    ease = suppressWarnings(as.numeric(row[["ease"]])),
    link_only_input = clean_chr(row[["linkOnly"]]),
    likely_panel_input = clean_chr(row[["likelyPanel"]]),
    ui_tier_input = ui_tier,
    service_type = service_type,
    service_root_url = root_url,
    exact_layer_urls = paste(exact_urls, collapse = " ; "),
    layer_ids_to_add = paste(ids_to_consider, collapse = ","),
    layer_names_to_add = paste(unname(layer_names_to_add), collapse = " | "),
    root_probe_ok = ifelse(is.na(root_probe$ok), "", as.character(root_probe$ok)),
    root_probe_error = root_probe$error %||% "",
    root_layer_count = root_layer_count,
    geometry_or_raster_type = geometry_type,
    spatial_reference = spatial_reference,
    extent = extent,
    time_enabled = ifelse(is.na(time_enabled), "", as.character(isTRUE(time_enabled))),
    query_supported = ifelse(is.na(query_supported), "", as.character(isTRUE(query_supported))),
    geojson_or_pbf_supported = ifelse(is.na(geojson_or_pbf_supported), "", as.character(isTRUE(geojson_or_pbf_supported))),
    legend_supported = ifelse(is.na(legend_supported), "", as.character(isTRUE(legend_supported))),
    token_required = ifelse(is.na(token_required), "", as.character(isTRUE(token_required))),
    existing_catalog_match = existing_match,
    recommended_implementation_path = impl,
    recommended_panel = panel,
    recommended_ui_tier = ui_tier,
    recommended_label = label,
    score = score,
    recommended_patch_bucket = bucket,
    mandatory_caveats = caveat,
    performance_notes = perf,
    source_link_plan = source_plan,
    legend_plan = legend_text,
    scale_visibility_plan = scale_text,
    reject_or_defer_reason = reject_reason,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

audit_df <- do.call(rbind, summary_rows)
audit_df <- audit_df[order(audit_df$recommended_patch_bucket, -audit_df$score, audit_df$candidate_id), ]
ranked_plan <- audit_df[order(-audit_df$score, audit_df$recommended_patch_bucket, audit_df$candidate_id), ]

# ---- Write outputs -----------------------------------------------------------

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
audit_csv <- file.path(output_dir, paste0("random_external_layer_audit_", timestamp, ".csv"))
audit_json <- file.path(output_dir, paste0("random_external_layer_audit_", timestamp, ".json"))
ranked_csv <- file.path(output_dir, paste0("random_external_layer_ranked_plan_", timestamp, ".csv"))
html_file <- file.path(output_dir, paste0("random_external_layer_audit_", timestamp, ".html"))
latest_csv <- file.path(output_dir, "random_external_layer_audit_latest.csv")
latest_ranked_csv <- file.path(output_dir, "random_external_layer_ranked_plan_latest.csv")
latest_json <- file.path(output_dir, "random_external_layer_audit_latest.json")
latest_html <- file.path(output_dir, "random_external_layer_audit_latest.html")

write.csv(audit_df, audit_csv, row.names = FALSE, na = "")
write.csv(ranked_plan, ranked_csv, row.names = FALSE, na = "")
write.csv(audit_df, latest_csv, row.names = FALSE, na = "")
write.csv(ranked_plan, latest_ranked_csv, row.names = FALSE, na = "")
jsonlite::write_json(records, audit_json, auto_unbox = TRUE, pretty = TRUE, null = "null")
jsonlite::write_json(records, latest_json, auto_unbox = TRUE, pretty = TRUE, null = "null")

# Compact HTML report for quick review in a browser.
make_link <- function(url, label = "open") {
  url <- clean_chr(url)
  if (!nzchar(url)) return("")
  paste0('<a href="', html_escape(url), '" target="_blank" rel="noopener noreferrer">', html_escape(label), '</a>')
}

short_table <- audit_df[, c(
  "candidate_id", "title", "input_batch", "service_type", "existing_catalog_match",
  "recommended_implementation_path", "recommended_panel", "recommended_ui_tier",
  "score", "recommended_patch_bucket", "root_probe_ok", "root_layer_count",
  "layer_ids_to_add", "mandatory_caveats", "performance_notes"
)]
short_table <- short_table[order(short_table$recommended_patch_bucket, -short_table$score), ]

rows_html <- apply(short_table, 1, function(x) {
  paste0(
    "<tr>",
    paste0("<td>", html_escape(x), "</td>", collapse = ""),
    "</tr>"
  )
})

bucket_counts <- as.data.frame(table(audit_df$recommended_patch_bucket), stringsAsFactors = FALSE)
names(bucket_counts) <- c("bucket", "n")
bucket_items <- paste0("<li><b>", html_escape(bucket_counts$bucket), "</b>: ", bucket_counts$n, "</li>", collapse = "\n")

html <- paste0(
'<!doctype html>\n<html><head><meta charset="utf-8">\n',
'<title>BRIM random external layer audit</title>\n',
'<style>\n',
'body{font-family:Arial,Helvetica,sans-serif;margin:22px;line-height:1.35;color:#222;}\n',
'h1{font-size:22px;margin-bottom:4px;} h2{font-size:17px;margin-top:22px;}\n',
'.note{background:#f6f7f8;border-left:4px solid #777;padding:10px 12px;margin:12px 0;}\n',
'table{border-collapse:collapse;width:100%;font-size:12px;}\n',
'th,td{border:1px solid #ddd;padding:5px 6px;vertical-align:top;} th{background:#f0f0f0;position:sticky;top:0;}\n',
'</style></head><body>\n',
'<h1>BRIM random external layer audit</h1>\n',
'<div class="note">Generated: ', html_escape(as.character(Sys.time())), '<br>',
'Live probe: ', html_escape(as.character(live_probe)), '; timeout: ', timeout_sec, ' sec; max layer probes/service: ', max_layer_probe, '<br>',
'This report is advisory only. It does not modify BRIM catalog or map behavior.</div>\n',
'<h2>Patch-bucket counts</h2><ul>', bucket_items, '</ul>\n',
'<h2>Output files</h2><ul>',
'<li>Audit CSV: ', html_escape(basename(audit_csv)), '</li>',
'<li>Ranked plan CSV: ', html_escape(basename(ranked_csv)), '</li>',
'<li>Audit JSON: ', html_escape(basename(audit_json)), '</li>',
'</ul>\n',
'<h2>Candidate audit table</h2>\n',
'<table><thead><tr>', paste0('<th>', html_escape(names(short_table)), '</th>', collapse = ''), '</tr></thead><tbody>\n',
paste(rows_html, collapse = '\n'),
'</tbody></table>\n',
'</body></html>\n'
)
writeLines(html, html_file, useBytes = TRUE)
file.copy(html_file, latest_html, overwrite = TRUE)

message("\nEL_001 audit complete.")
message("Wrote audit CSV:    ", audit_csv)
message("Wrote ranked CSV:   ", ranked_csv)
message("Wrote audit JSON:   ", audit_json)
message("Wrote HTML report:  ", html_file)
message("\nReview the latest outputs here:")
message("  ", latest_csv)
message("  ", latest_ranked_csv)
message("  ", latest_html)
message("\nNo BRIM map files, cache files, or GitHub feed products were modified.")
