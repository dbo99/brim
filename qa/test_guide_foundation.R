#!/usr/bin/env Rscript

# Focused source-only BRIM Guide contracts. No map build, cache mutation,
# preprocessor, or network access occurs here.

fail <- function(message) stop(message, call. = FALSE)
assert_true <- function(value, message) if (!isTRUE(value)) fail(message)
assert_identical <- function(actual, expected, message) {
  if (!identical(actual, expected)) {
    fail(paste0(message, "\nExpected: ", paste(expected, collapse = ", "),
                "\nActual: ", paste(actual, collapse = ", ")))
  }
}
assert_error <- function(expression, pattern, message) {
  error <- tryCatch({ force(expression); NULL }, error = identity)
  if (is.null(error) || !grepl(pattern, conditionMessage(error), fixed = TRUE)) fail(message)
}

suppressPackageStartupMessages(library(dplyr))
source(file.path("00_config", "config_map_display.r"))
source(file.path("00_config", "config_local_layer_registry.r"))
source(file.path("03_functions", "leaflet_layer_local_core_helpers.r"))
source(file.path("03_functions", "leaflet_core_helpers.r"))
source(file.path("03_functions", "leaflet_loading_helpers.r"))
source(file.path("03_functions", "leaflet_ops_live_layer_definition_helpers.r"))
source(file.path("03_functions", "leaflet_tools_adddata_helpers.r"))
source(file.path("03_functions", "bulletin118_data_helpers.r"))
source(file.path("03_functions", "leaflet_huc_theme_helpers.r"))
source(file.path("03_functions", "leaflet_bulletin118_theme_helpers.r"))
source(file.path("03_functions", "leaflet_guide_helpers.r"))

flag_enabled <- function(flag) {
  is.na(flag) || !nzchar(flag) || isTRUE(MAP_DISPLAY[[flag]])
}
enabled_rows <- vapply(LOCAL_LAYER_REGISTRY$map_display_flag, flag_enabled, logical(1))
runtime_groups <- unique(pt_layer_group_name(LOCAL_LAYER_REGISTRY$canonical_group[enabled_rows]))
runtime_groups <- setdiff(runtime_groups, "Channels – Wild & Scenic Rivers")
runtime_groups <- c(
  runtime_groups,
  "Channels – Wild & Scenic Rivers | BLM-CA lines",
  "Channels – Wild & Scenic Rivers | USFS/interagency segments",
  "Channels – Wild & Scenic River corridors | BLM-CA",
  "Channels – Wild & Scenic River corridors | USFS/LSRS areas",
  "Channels – Wild & Scenic River legal-status corridors | USFS/LSRS",
  "Basins – CNRFC Product Availability",
  "Labels: explicit non-Product test"
)

catalog_model <- pt_build_layer_explorer_metadata()
assert_identical(catalog_model$catalogAuthority, "DESCRIPTIVE_ONLY",
                 "Descriptive catalog authority changed")
assert_identical(catalog_model$runtimeAuthority, "UNCHANGED",
                 "Descriptive catalog became runtime authority")
assert_identical(catalog_model$runtimeControl, "NONE",
                 "Descriptive catalog gained runtime controls")
assert_identical(length(catalog_model$records), 26L,
                 "Descriptive catalog must remain 26 rows")

bundle <- pt_build_guide_bundle(runtime_groups, MAP_DISPLAY, "default")
product_ids <- vapply(bundle$products, `[[`, character(1), "id")
product_paths <- vapply(bundle$products, `[[`, character(1), "pathLabel")
product_subsystems <- vapply(bundle$products, `[[`, character(1), "subsystem")
resource_registry <- pt_guide_read_resource_registry()
relationship_registry <- pt_guide_read_product_resource_relationship_registry(
  product_ids, resource_registry
)
relationship_registry_ids <- vapply(
  relationship_registry$products, `[[`, character(1), "product_id"
)
enrichment <- pt_guide_read_product_enrichment()
raw_resource_registry <- jsonlite::fromJSON(
  file.path("00_config", "guide_resources.json"), simplifyVector = FALSE
)
# R17C1 fixtures: the exact bounded delta from accepted R17B. Historical
# snapshots below reverse only this delta; the real registry reader above and
# every negative fixture still use the actual worktree and tracked-path policy.
r17c1_added_ids <- unlist(jsonlite::fromJSON("[\"resource_usgs_usgs_california_river_basin_schematics_collection\",\"resource_noaa_nws_graphical_forecasts\",\"resource_polarwx_tropical\",\"resource_brightband_operational_weatherbench\",\"resource_geolibre\",\"resource_noaa_wpc_excessive_rainfall_outlook\"]", simplifyVector = FALSE), use.names = FALSE)
r17c1_retired_ids <- unlist(jsonlite::fromJSON("[\"resource_noaa_cnrfc_forcing_csv_service\",\"resource_noaa_cnrfc_hourly_hefs_csv_service\"]", simplifyVector = FALSE), use.names = FALSE)
r17c1_parent_preimages <- jsonlite::fromJSON("{\"resource_dwr_cdec\":{\"access_points\":[{\"role\":\"canonical\",\"label\":\"California Data Exchange Center (CDEC)\",\"url\":\"https://cdec.water.ca.gov/\"},{\"role\":\"configured_view\",\"label\":\"CDEC Reservoir Conditions\",\"url\":\"https://cdec.water.ca.gov/resapp/RescondMain\"}],\"aliases\":[],\"migration_aliases\":[],\"search_aliases\":[]},\"resource_noaa_cnrfc\":{\"access_points\":[{\"role\":\"canonical\",\"label\":\"California-Nevada River Forecast Center\",\"url\":\"https://www.cnrfc.noaa.gov/\"}],\"aliases\":[],\"migration_aliases\":[],\"search_aliases\":[]},\"resource_nrcs_nwcc\":{\"access_points\":[{\"role\":\"canonical\",\"label\":\"NRCS National Water and Climate Center\",\"url\":\"https://nrcs.usda.gov/programs-initiatives/sswsf-snow-survey-and-water-supply-forecasting-program/national-water-and\"}],\"aliases\":[],\"migration_aliases\":[],\"search_aliases\":[]}}", simplifyVector = FALSE)
r17c1_retired_records <- jsonlite::fromJSON("[{\"id\":\"resource_noaa_cnrfc_hourly_hefs_csv_service\",\"aliases\":[],\"migration_aliases\":[\"res.noaa.cnrfc-hourly-hefs-csv.service\"],\"search_aliases\":[\"CNRFC Hourly HEFS CSV\",\"cnrfc.noaa.gov\"],\"order\":187,\"title\":\"CNRFC Hourly HEFS CSV\",\"providers\":[{\"name\":\"National Oceanic and Atmospheric Administration\",\"role\":\"display_provider\"}],\"summary\":\"Download hourly Hydrologic Ensemble Forecast Service products in CSV format.\",\"canonical_url\":\"https://www.cnrfc.noaa.gov/ensembleHourlyProductCSV.php\",\"access_points\":[{\"role\":\"canonical\",\"label\":\"Canonical landing URL\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleHourlyProductCSV.php\"}],\"resource_type\":\"data_service_or_api\",\"temporal_character\":\"unknown\",\"resource_granularity\":\"platform\",\"subject_tags\":[\"Weather & Forecasts\"],\"information_type_tags\":[],\"variables\":[\"probabilistic river forecast\"],\"use_scopes\":[\"Forecasting\",\"Hydrologic conditions\",\"Flood risk\",\"Emergency planning\",\"Data integration\"],\"geographic_scope\":{\"scope_type\":\"national\",\"names\":[\"California\",\"United States with California-focused regional products\"]},\"access_class\":\"public\",\"public_source_references\":[{\"role\":\"official_source\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleHourlyProductCSV.php\"}],\"publication_state\":\"published\"},{\"id\":\"resource_noaa_cnrfc_forcing_csv_service\",\"aliases\":[],\"migration_aliases\":[\"res.noaa.cnrfc-forcing-csv.service\"],\"search_aliases\":[\"CNRFC Forcings CSV\",\"cnrfc.noaa.gov\"],\"order\":202,\"title\":\"CNRFC Forcing CSV\",\"providers\":[{\"name\":\"National Oceanic and Atmospheric Administration\",\"role\":\"display_provider\"}],\"summary\":\"Download forecast forcing products in CSV format.\",\"canonical_url\":\"https://www.cnrfc.noaa.gov/forcingProductCSV.php\",\"access_points\":[{\"role\":\"canonical\",\"label\":\"Canonical landing URL\",\"url\":\"https://www.cnrfc.noaa.gov/forcingProductCSV.php\"}],\"resource_type\":\"data_service_or_api\",\"temporal_character\":\"unknown\",\"resource_granularity\":\"platform\",\"subject_tags\":[\"Weather & Forecasts\"],\"information_type_tags\":[],\"variables\":[\"precipitation forecast\",\"temperature forecast\"],\"use_scopes\":[\"Forecasting\",\"Hydrologic conditions\",\"Flood risk\",\"Emergency planning\",\"Data integration\"],\"geographic_scope\":{\"scope_type\":\"national\",\"names\":[\"California\",\"United States with California-focused regional products\"]},\"access_class\":\"public\",\"public_source_references\":[{\"role\":\"official_source\",\"url\":\"https://www.cnrfc.noaa.gov/forcingProductCSV.php\"}],\"publication_state\":\"published\"}]", simplifyVector = FALSE)
r17c1_hash <- function(value) digest::digest(jsonlite::toJSON(
  value, auto_unbox = TRUE, null = "null", na = "null", pretty = FALSE, digits = NA
), algo = "sha256", serialize = FALSE)
# A5 is an explicit URL-keyed overlay on the frozen C1/A4 expectations.
# Its strict inverse is used only for historical fixtures; live validation stays intact.
a5_label_changes <- jsonlite::fromJSON("[{\"station_code\":\"CEGC1\",\"old_label\":\"Trinity\",\"new_label\":\"Trinity River — Trinity Lake — FNF water-year trend (CEGC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=CEGC1&prodID=9\"},{\"station_code\":\"CMPC1\",\"old_label\":\"Mokelumne\",\"new_label\":\"Mokelumne River — Pardee Reservoir — FNF water-year trend (CMPC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=CMPC1&prodID=9\"},{\"station_code\":\"EXQC1\",\"old_label\":\"Merced\",\"new_label\":\"Merced River — Lake McClure / New Exchequer — FNF water-year trend (EXQC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=EXQC1&prodID=9\"},{\"station_code\":\"FOLC1\",\"old_label\":\"American\",\"new_label\":\"American River — Folsom Lake — FNF water-year trend (FOLC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=FOLC1&prodID=9\"},{\"station_code\":\"FRAC1\",\"old_label\":\"San Joaquin\",\"new_label\":\"San Joaquin River — Millerton Reservoir — FNF water-year trend (FRAC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=FRAC1&prodID=9\"},{\"station_code\":\"HLEC1\",\"old_label\":\"Yuba\",\"new_label\":\"Yuba River — Englebright Reservoir — FNF water-year trend (HLEC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=HLEC1&prodID=9\"},{\"station_code\":\"ISAC1\",\"old_label\":\"Kern\",\"new_label\":\"Kern River — Lake Isabella — FNF water-year trend (ISAC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=ISAC1&prodID=9\"},{\"station_code\":\"MHBC1\",\"old_label\":\"Cosumnes\",\"new_label\":\"Cosumnes River — Michigan Bar — FNF water-year trend (MHBC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=MHBC1&prodID=9\"},{\"station_code\":\"NDPC1\",\"old_label\":\"Tuolumne\",\"new_label\":\"Tuolumne River — New Don Pedro Reservoir — FNF water-year trend (NDPC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=NDPC1&prodID=9\"},{\"station_code\":\"NMSC1\",\"old_label\":\"Stanislaus\",\"new_label\":\"Stanislaus River — New Melones Reservoir — FNF water-year trend (NMSC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=NMSC1&prodID=9\"},{\"station_code\":\"ORDC1\",\"old_label\":\"Feather\",\"new_label\":\"Feather River — Lake Oroville — FNF water-year trend (ORDC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=ORDC1&prodID=9\"},{\"station_code\":\"PFTC1\",\"old_label\":\"Kings\",\"new_label\":\"Kings River — Pine Flat Reservoir — FNF water-year trend (PFTC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=PFTC1&prodID=9\"},{\"station_code\":\"SCSC1\",\"old_label\":\"Tule\",\"new_label\":\"Tule River — Lake Success — FNF water-year trend (SCSC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=SCSC1&prodID=9\"},{\"station_code\":\"SHDC1\",\"old_label\":\"Sacramento/McCloud/Pit\",\"new_label\":\"Sacramento River — Shasta Lake — FNF water-year trend (SHDC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=SHDC1&prodID=9\"},{\"station_code\":\"TMDC1\",\"old_label\":\"Kaweah\",\"new_label\":\"Kaweah River — Lake Kaweah — FNF water-year trend (TMDC1)\",\"url\":\"https://www.cnrfc.noaa.gov/ensembleProduct.php?id=TMDC1&prodID=9\"}]", simplifyVector = FALSE)
a5_new_actions <- jsonlite::fromJSON("[{\"role\":\"configured_view\",\"label\":\"Water Resources — Regional Forecast Map\",\"url\":\"https://www.cnrfc.noaa.gov/water_resources_update.php\"},{\"role\":\"configured_view\",\"label\":\"Daily Basin QPF & Freezing Levels — Days 1–6 (HD6RSA)\",\"url\":\"https://www.cnrfc.noaa.gov/awipsProducts/RNOHD6RSA.php\"}]", simplifyVector = FALSE)
a5_summary_append <- " Selected shortcuts include full natural flow (FNF) water-year trend plots for named forecast points; use the Water Resources map for the wider network."
a5_expected_action <- function(action) {
  for (change in a5_label_changes) if (identical(action$url, change$url)) {
    assert_identical(action$label, change$old_label, "Frozen C1 station label changed")
    assert_identical(action$role, "configured_view", "Frozen C1 station role changed")
    action$label <- change$new_label
  }
  action
}
a5_reverse_resource <- function(record) {
  if (!identical(record$id, "resource_noaa_cnrfc")) return(record)
  assert_identical(length(record$access_points), 23L, "A5 CNRFC must have 23 actions")
  assert_identical(record$access_points[2:3], a5_new_actions,
                   "A5 new entry points or their second/third placement changed")
  assert_true(endsWith(record$summary, a5_summary_append), "A5 summary append changed")
  record$summary <- substr(record$summary, 1L, nchar(record$summary) - nchar(a5_summary_append))
  assert_true(!grepl(a5_summary_append, record$summary, fixed = TRUE), "A5 summary append duplicated")
  record$access_points <- record$access_points[-c(2L, 3L)]
  for (change in a5_label_changes) {
    indexes <- which(vapply(record$access_points, function(action) identical(action$url, change$url), logical(1)))
    assert_identical(length(indexes), 1L, "A5 station URL is absent or duplicated")
    assert_identical(record$access_points[[indexes]]$role, "configured_view", "A5 station role changed")
    assert_identical(record$access_points[[indexes]]$label, change$new_label, "A5 station label changed")
    record$access_points[[indexes]]$label <- change$old_label
  }
  record
}
a5_reverse_registry <- function(raw) {
  raw$resources <- lapply(raw$resources, a5_reverse_resource)
  raw
}
a5_historical_registry <- function(raw) {
  # The existing reader validates the historical copy in invocation-owned temp storage.
  fixture <- tempfile("a5-historical-registry-", fileext = ".json")
  writeLines(as.character(jsonlite::toJSON(a5_reverse_registry(raw),
    auto_unbox = TRUE, null = "null", na = "null", pretty = FALSE, digits = NA)), fixture)
  pt_guide_read_resource_registry(fixture)
}
r17c1_strip_resource <- function(record) {
  record <- a5_reverse_resource(record)
  before <- r17c1_parent_preimages[[record$id]]
  if (!is.null(before)) for (field in names(before)) record[[field]] <- before[[field]]
  record
}
r17c1_r17b_raw <- raw_resource_registry
r17c1_r17b_raw$resources <- lapply(Filter(function(record) {
  !record$id %in% r17c1_added_ids
}, r17c1_r17b_raw$resources), r17c1_strip_resource)
for (record in r17c1_retired_records) {
  r17c1_r17b_raw$resources <- append(r17c1_r17b_raw$resources,
                                    list(record), after = record$order - 1L)
}
for (i in seq_along(r17c1_r17b_raw$resources)) r17c1_r17b_raw$resources[[i]]$order <- i
assert_identical(r17c1_hash(r17c1_r17b_raw),
                 "6b1ece99034e9153f7a38983962d7d8e9839c118b1cc28bd6b7a6f7b306e525d", "R17C1 changed a field outside its exact Resource delta")
assert_identical(r17c1_hash(a5_reverse_registry(raw_resource_registry)),
                 "ea850773107f0f821b33942b5900e055df91a25fb348c83902218ff4ae7e18dc", "The approved R17C1 Resource snapshot changed")

registry_ids <- vapply(resource_registry, `[[`, character(1), "id")
registry_states <- vapply(resource_registry, `[[`, character(1), "publication_state")
staged_registry <- unclass(resource_registry)[registry_states == "staged"]
staged_registry_ids <- vapply(staged_registry, `[[`, character(1), "id")
held_staged_registry <- staged_registry[vapply(
  staged_registry, function(record) record$order <= 72L, logical(1)
)]
r15c_target_registry <- unclass(resource_registry)[vapply(
  resource_registry, function(record) record$order > 72L, logical(1)
)]
r15c_target_registry_ids <- vapply(r15c_target_registry, `[[`, character(1), "id")
r15b_selected_replacement_ids <- c(
  "resource_dwr_snowtrax_platform",
  "resource_santa_barbara_county_public_works_santa_barbara_county_real_time_hydrology_platform"
)
r16b_added_resource_ids <- c(
  "resource_usace_usace_water_management_data_platform",
  "resource_usace_sacramento_district_water_control_data_system",
  "resource_usace_los_angeles_district_water_management_platform",
  "resource_usbr_central_valley_operations_office_platform",
  "resource_usbr_lower_colorado_river_operations",
  "resource_usbr_upper_colorado_basin_water_operations",
  "resource_usbr_colorado_river_basin_hub",
  "resource_usbr_klamath_project_water_operations_platform",
  "resource_usbr_truckee_river_operating_agreement_platform",
  "resource_usbr_reclamation_information_sharing_environment_platform",
  "resource_usbr_central_valley_project_water_supply_program",
  "resource_usbr_reclamation_hydromet_platform",
  "resource_usbr_reclamation_agrimet_platform"
)
r16b_retired_resource_ids <- c(
  "resource_usace_usace_warm_springs_dam_lake_sonoma_hourly_data_product",
  "resource_usace_usace_terminus_dam_lake_kaweah_hourly_data_product",
  "resource_usace_usace_hidden_dam_hensley_lake_hourly_data_product",
  "resource_usace_usace_sacramento_river_clear_creek_hourly_data_product",
  "resource_usace_usace_farmington_dam_hourly_data_product",
  "resource_usace_usace_pine_flat_lake_hourly_data_product",
  "resource_usbr_cvp_swp_long_term_operations_record_of_decision_product"
)
r16b_spk_id <- "resource_usace_sacramento_district_water_control_data_system"
r17b_cnrfc_resource_id <- "resource_noaa_cnrfc"
r17b_cnrfc_resource_summary_before <- paste(
  "Operational river, precipitation, temperature, snow-level, and water-supply",
  "forecasting for California and Nevada."
)
r17b_cnrfc_resource_summary_after <- paste(
  "Operational river, reservoir-inflow, precipitation, temperature,",
  "freezing-level, and short- to long-term water-supply forecasting for California and Nevada."
)
r17b_cnrfc_canonical_url_before <- "https://cnrfc.noaa.gov/"
r17b_cnrfc_canonical_url_after <- "https://www.cnrfc.noaa.gov/"
r17b_cnrfc_fnf_processing_before <- paste(
  "BRIM reads the prepared basin artifact, retains its river, reservoir, and",
  "CNRFC/NWS identifiers, and applies reviewed display geometry for the map."
)
r17b_cnrfc_fnf_processing_after <- paste(
  "BRIM created the displayed FNF geometries by grouping and dissolving downloadable CNRFC subbasin geometries.",
  "The current pipeline thins vertex density for map performance and retains river, reservoir, and CNRFC/NWS identifiers.",
  "Popups link separately to the CDEC/DWR Full Natural Flow report and to CNRFC water-year ensemble reservoir-inflow plots.",
  "The CNRFC plots display observed values alongside forecast traces for unregulated/full natural flow."
)
r17b_cnrfc_fnf_boundary_limitation <- paste(
  "Where CDEC and CNRFC FNF products represent the same river-reservoir system,",
  "BRIM uses a common display geometry; minor differences in agency watershed delineations are not represented separately."
)
r15b_rejected_canonical_ids <- c(
  "resource_sacramento_county_water_resources_sacramento_county_rainfall_and_stream_levels_dashboards_collection",
  "resource_kern_river_watermaster_kern_river_watermaster_platform",
  "resource_dwr_cdec_reservoir_conditions_dashboard",
  "resource_napa_county_flood_control_napa_valley_rainfall_and_stream_monitoring_platform",
  "resource_dwr_snowtrax_isnobal_dashboard",
  "resource_santa_barbara_county_public_works_santa_barbara_county_real_time_hydrology_map_viewer"
)
r15b_authorized_access_points <- list(
  resource_dwr_cdec = list(
    role = "configured_view", label = "CDEC Reservoir Conditions",
    url = "https://cdec.water.ca.gov/resapp/RescondMain"
  ),
  resource_napa_county_flood_control_napa_valley_rainfall_and_stream_monitoring_map_viewer = list(
    role = "configured_view", label = "Napa Valley Rainfall and Stream Monitoring Home",
    url = "https://napa.onerain.com/"
  ),
  resource_dwr_snowtrax_platform = list(
    role = "configured_view", label = "SnowTrax iSnobal",
    url = "https://snow.water.ca.gov/isnobal"
  ),
  resource_santa_barbara_county_public_works_santa_barbara_county_real_time_hydrology_platform = list(
    role = "configured_view", label = "Santa Barbara County Real-Time Hydrology – Map",
    url = "https://rain.cosbpw.net/map"
  )
)

assert_identical(names(pt_guide_supported_profiles()), "default",
                 "Guide must name only the one actual current build profile")
assert_identical(bundle$profileId, "default", "Default profile ID changed")
assert_identical(bundle$schemaVersion, 4L, "Structured Guide schema version changed")
assert_identical(
  bundle$authority$coverage,
  "ALL_INCLUDED_NON_BASEMAP_VISIBLE_PRODUCTS_WITH_SOURCE_BACKED_VITALS",
  "Guide coverage authority no longer declares source-backed vitals"
)
assert_identical(bundle$counts$products, 270L,
                 "Current default Guide should derive 270 post-basemap Products")
assert_true(setequal(relationship_registry_ids, product_ids) &&
              length(relationship_registry_ids) == length(product_ids),
            "The sole relationship registry does not equal the compiled Product universe")
assert_identical(bundle$counts$articles, 7L,
                 "Current Guide must include the seven maintained Methods")
assert_identical(bundle$counts$resources, 210L,
                 "Current Guide must include all 210 published Resources")
assert_identical(length(resource_registry), 215L,
                 "Canonical Resource registry must validate all 215 records")
assert_identical(raw_resource_registry$schema_version, 3L,
                 "Canonical Resource registry schema version changed")
assert_identical(sum(registry_states == "published"), 210L,
                 "Canonical Resource registry published count changed")
assert_identical(sum(registry_states == "staged"), 5L,
                 "Canonical Resource registry staged count changed")
staged_registry_json <- jsonlite::toJSON(
  held_staged_registry, auto_unbox = TRUE, null = "null", na = "null",
  pretty = FALSE, digits = NA
)
assert_identical(
  digest::digest(staged_registry_json, algo = "sha256", serialize = FALSE),
  "b76edaea607d39160f83855d3e8ab09d06dcf9e86fa0da6c7e732b45afdde807",
  "The exact five-record held staged registry contract changed"
)
assert_identical(length(r15c_target_registry), 143L,
                 "The post-baseline authority must publish exactly 143 Resources")
assert_identical(
  digest::digest(paste0(paste(r15c_target_registry_ids, collapse = "\n"), "\n"),
                 algo = "sha256", serialize = FALSE),
  "ad89c408956727320038090e799deee98b2a70697fa993351031d061d092e1a2",
  "The exact ordered R16B post-baseline Resource ID set changed"
)
assert_true(all(vapply(r15c_target_registry, function(record) {
  identical(record$publication_state, "published")
}, logical(1))), "Every R15C target Resource must be published")
assert_true(!any(r15b_rejected_canonical_ids %in% registry_ids),
            "A removed, duplicate, or subordinate R15B proposal remains canonical")
assert_true(setequal(registry_ids[registry_ids %in% r16b_added_resource_ids],
                     r16b_added_resource_ids) &&
              sum(registry_ids %in% r16b_added_resource_ids) == 13L,
            "The exact 13 R16B canonical additions changed")
assert_true(!any(r16b_retired_resource_ids %in% registry_ids),
            "An exact R16B merged Resource remains canonical")
assert_identical(
  registry_ids[registry_ids %in% r15b_selected_replacement_ids],
  r15b_selected_replacement_ids,
  "The final broad R15B replacement pair is missing or reordered"
)
assert_identical(sum(vapply(r15c_target_registry, function(record) {
  !length(record$subject_tags)
}, logical(1))), 17L,
"The precipitation micro-pass empty target subject-set count changed")
assert_true(!"resource_nasa_nasa_grace_map_comparison_slider_viewer" %in% registry_ids,
            "The blocked GRACE comparison-slider access point became a Resource")
assert_identical(sum(registry_ids == "resource_noaa_noaa_sea_level_rise_viewer_viewer"), 1L,
                 "The NOAA Sea Level Rise target replacement is not present exactly once")
relationship_resource_representations <- vapply(
  relationship_registry$resources,
  function(record) if (is.null(record$map_representation)) {
    "not_yet_reviewed"
  } else {
    record$map_representation
  },
  character(1)
)
assert_identical(length(relationship_registry$resources), 215L,
                 "Relationship Resource authority must contain all 215 Resources")
assert_identical(unname(as.integer(table(factor(
  relationship_resource_representations,
  levels = c("direct_match_in_brim", "selected_products_in_brim",
             "not_currently_mapped_in_brim", "not_yet_reviewed")
)))), c(3L, 24L, 183L, 5L),
"Full relationship Resource representation counts changed")
assert_identical(bundle$counts$updates, 3L,
                 "Current Guide must include the three verified Updates")
expected_current_resource_ids <- c(
  "resource_doi",
  "resource_blm_california",
  "resource_prism_normals",
  "resource_usgs_bcmv8",
  "resource_dwr_bulletin118_sgma_2019",
  "resource_calfire_fire_perimeters",
  "resource_nifc_wfigs_current",
  "resource_usgs_water_dashboard",
  "resource_noaa_nwps",
  "resource_noaa_goes_image_viewer",
  "resource_cira_slider",
  "resource_nasa_worldview",
  "resource_climate_engine",
  "resource_nasa_smap_mission",
  "resource_nasa_smap_data",
  "resource_nasa_smap_l3_enhanced_soil_moisture",
  "resource_noaa_smops",
  "resource_noaa_smops_maps",
  "resource_noaa_cpc_soil_moisture",
  "resource_nidis_soil_moisture_dashboard",
  "resource_ncsmmn_network_map",
  "resource_ncsmmn_soil_moisture_portal",
  "resource_nrcs_scan",
  "resource_nasa_grace_tellus",
  "resource_nasa_grace_data",
  "resource_nasa_grace_analysis_tool",
  "resource_nasa_grace_groundwater_soil_moisture",
  "resource_nidis_grace_groundwater_soil_moisture",
  "resource_noaa_vegetation_health",
  "resource_usda_crop_casma",
  "resource_usda_vegscape",
  "resource_usgs_quickdri",
  "resource_usgs_vegdri"
)
expected_newly_published_ids <- c(
  "resource_aso_airborne_snow_observatories",
  "resource_dwr_california_groundwater_live",
  "resource_dwr_california_water_watch",
  "resource_dwr_casgem",
  "resource_dwr_cdec",
  "resource_dwr_cimis",
  "resource_dwr_groundwater_sustainability_agencies",
  "resource_dwr_water_data_library",
  "resource_epa_cyanweb",
  "resource_ismn",
  "resource_nasa_asf_displacement_portal",
  "resource_nasa_cyfi_explorer",
  "resource_nasa_ecostress_data_resources",
  "resource_nasa_firms_global_fire_map",
  "resource_nasa_nldas_drought_monitor",
  "resource_nasa_opera_products",
  "resource_nasa_stream_water_quality_tool",
  "resource_nasa_swot_hydrology_resources",
  "resource_nidis_soil_moisture_resources",
  "resource_noaa_cnrfc",
  "resource_noaa_coastwatch_data_portal",
  "resource_noaa_coastwatch_erddap",
  "resource_noaa_cpc_forecasts_outlooks",
  "resource_noaa_wpc_qpf",
  "resource_nrcs_nwcc",
  "resource_nrcs_snow_survey_water_supply_forecasting",
  "resource_tu_wien_soil_moisture_viewer",
  "resource_usace_cwms_data_api",
  "resource_usbr",
  "resource_usgs_groundwater_watch",
  "resource_usgs_national_hydrography_products",
  "resource_usgs_streamstats",
  "resource_usgs_water_data_nation",
  "resource_usgs_water_quality_portal"
)
expected_held_staged_ids <- c(
  "resource_nasa_giovanni",
  "resource_nrcs_web_soil_survey",
  "resource_usda_cropland_data_layer",
  "resource_usgs_earthexplorer",
  "resource_usgs_water_data_apis"
)
expected_resource_ids <- c(
  expected_current_resource_ids, expected_newly_published_ids,
  r15c_target_registry_ids
)
assert_identical(vapply(bundle$resources, `[[`, character(1), "id"), expected_resource_ids,
                 "Current Resource ID set/order changed")
assert_identical(vapply(held_staged_registry, `[[`, character(1), "id"),
                 expected_held_staged_ids,
                 "The five held staged Resource IDs changed")
raw_registry_ids <- vapply(raw_resource_registry$resources, `[[`, character(1), "id")
raw_current_resources <- raw_resource_registry$resources[match(
  expected_current_resource_ids, raw_registry_ids
)]
raw_newly_published <- r17c1_r17b_raw$resources[match(
  expected_newly_published_ids, raw_registry_ids
)]
compact_json <- function(value) jsonlite::toJSON(
  value, auto_unbox = TRUE, null = "null", na = "null",
  pretty = FALSE, digits = NA
)
strip_publication_state <- function(record) {
  record$publication_state <- NULL
  record
}
strip_r15b_authorized_access_point <- function(record) {
  expected <- r15b_authorized_access_points[[record$id]]
  if (!is.null(expected)) {
    record$access_points <- Filter(function(access_point) {
      !identical(access_point, expected)
    }, record$access_points)
  }
  record
}
strip_r16b_existing_resource_access_points <- function(record) {
  if (identical(record$id, "resource_usbr")) {
    added_urls <- c(
      "https://www.usbr.gov/main/water/",
      "https://www.usbr.gov/mp/wateroperations.html"
    )
    record$access_points <- Filter(function(access_point) {
      !access_point$url %in% added_urls
    }, record$access_points)
  }
  record
}
precipitation_micro_pass_resource_ids <- c(
  "resource_prism_normals",
  "resource_cw3e_cw3e_micro_rain_radar_snow_levels_dashboard",
  "resource_scwa_solano_county_flood_monitoring_map_viewer",
  "resource_rcfcwcd_riverside_county_rainfall_map_viewer",
  "resource_contra_costa_county_flood_control_and_wa_contra_costa_county_rainmap_viewer",
  "resource_cocorahs_cocorahs_other",
  "resource_lacpw_los_angeles_county_precipitation_data_platform",
  "resource_marin_county_flood_control_marin_county_rainfall_and_creek_data_dashboards_collection",
  "resource_santa_barbara_county_public_works_santa_barbara_county_real_time_hydrology_platform",
  "resource_santa_cruz_county_flood_control_santa_cruz_county_hydrologic_monitoring_map_viewer",
  "resource_napa_county_flood_control_napa_valley_rainfall_and_stream_monitoring_map_viewer"
)
strip_precipitation_micro_pass <- function(record) {
  if (record$id %in% precipitation_micro_pass_resource_ids) {
    record$subject_tags <- Filter(function(value) {
      !identical(value, "Precipitation")
    }, record$subject_tags)
  }
  record
}
strip_r17b_cnrfc_resource_summary_correction <- function(record) {
  if (identical(record$id, r17b_cnrfc_resource_id)) {
    record$summary <- r17b_cnrfc_resource_summary_before
  }
  record
}
strip_r17b_cnrfc_canonical_host_repair <- function(record) {
  if (identical(record$id, r17b_cnrfc_resource_id)) {
    record$canonical_url <- r17b_cnrfc_canonical_url_before
    record$access_points <- lapply(record$access_points, function(point) {
      if (identical(point$role, "canonical")) {
        point$url <- r17b_cnrfc_canonical_url_before
      }
      point
    })
    record$public_source_references <- lapply(
      record$public_source_references,
      function(reference) {
        if (identical(reference$role, "official_source")) {
          reference$url <- r17b_cnrfc_canonical_url_before
        }
        reference
      }
    )
  }
  record
}
cnrfc_raw_resource <- raw_resource_registry$resources[[match(
  r17b_cnrfc_resource_id,
  vapply(raw_resource_registry$resources, `[[`, character(1), "id")
)]]
assert_identical(cnrfc_raw_resource$canonical_url, r17b_cnrfc_canonical_url_after,
                 "The repaired CNRFC canonical URL changed")
assert_identical(cnrfc_raw_resource$access_points[[1]]$url,
                 r17b_cnrfc_canonical_url_after,
                 "The repaired CNRFC canonical access point changed")
assert_identical(cnrfc_raw_resource$public_source_references[[1]]$url,
                 r17b_cnrfc_canonical_url_after,
                 "The repaired CNRFC official-source reference changed")
assert_true(!grepl(r17b_cnrfc_canonical_url_before,
                   compact_json(cnrfc_raw_resource), fixed = TRUE),
            "The non-resolving bare CNRFC root remains in the canonical Resource")
reconstructed_pre_host_registry <- r17c1_r17b_raw
reconstructed_pre_host_registry$resources <- lapply(
  reconstructed_pre_host_registry$resources,
  strip_r17b_cnrfc_canonical_host_repair
)
assert_identical(
  digest::digest(compact_json(reconstructed_pre_host_registry),
                 algo = "sha256", serialize = FALSE),
  "c604267fe017d58bb33efd50a6a5d2341d39f2633624c328fd21cd9edb68bba9",
  "The Resource registry changed beyond the exact three-field CNRFC host repair"
)
reconstructed_pre_copy_registry <- reconstructed_pre_host_registry
reconstructed_cnrfc_index <- match(r17b_cnrfc_resource_id, vapply(
  reconstructed_pre_copy_registry$resources, `[[`, character(1), "id"
))
assert_identical(
  reconstructed_pre_copy_registry$resources[[reconstructed_cnrfc_index]]$summary,
  r17b_cnrfc_resource_summary_after,
  "The exact corrected CNRFC Resource summary is absent"
)
reconstructed_pre_copy_registry$resources[[reconstructed_cnrfc_index]]$summary <-
  r17b_cnrfc_resource_summary_before
assert_identical(
  digest::digest(compact_json(reconstructed_pre_copy_registry),
                 algo = "sha256", serialize = FALSE),
  "5d106ee503cd66942ce9aee21ab57449214b706a40fd02bbcd1102cd92b62b43",
  "The Resource registry changed beyond the one CNRFC summary correction"
)
assert_true(!grepl(r17b_cnrfc_resource_summary_before,
                   compact_json(raw_resource_registry), fixed = TRUE),
            "The superseded CNRFC Resource summary remains in authority")
assert_identical(
  digest::digest(compact_json(lapply(raw_current_resources, strip_precipitation_micro_pass)),
                 algo = "sha256", serialize = FALSE),
  "a86067ab046b93a165fd5944f85079ca7140fbb34efc0d0add79b0921f260190",
  "A current published Resource changed from the accepted R9 baseline"
)
assert_identical(
  digest::digest(compact_json(lapply(lapply(lapply(
    lapply(lapply(lapply(raw_newly_published, strip_r15b_authorized_access_point),
           strip_r16b_existing_resource_access_points), strip_precipitation_micro_pass),
    strip_r17b_cnrfc_resource_summary_correction),
    strip_r17b_cnrfc_canonical_host_repair), strip_publication_state
  )),
                 algo = "sha256", serialize = FALSE),
  "cdc21369a808d4619313034e9e07127572f4429ac26bbef6eb99365ef570cdca",
  paste0(
    "A newly published Resource field changed beyond publication_state and the exact ",
    "USBR canonical-host repair"
  )
)
for (owner_id in names(r15b_authorized_access_points)) {
  record <- resource_registry[[match(owner_id, registry_ids)]]
  expected <- r15b_authorized_access_points[[owner_id]]
  matches <- vapply(record$access_points, identical, logical(1), expected)
  assert_identical(sum(matches), 1L, paste(
    "The authorized subordinate access point is not present exactly once on", owner_id
  ))
}
goes_resource <- bundle$resources[[match(
  "resource_noaa_goes_image_viewer", expected_resource_ids
)]]
assert_identical(vapply(goes_resource$accessPoints, `[[`, character(1), "url"), c(
  "https://www.star.nesdis.noaa.gov/GOES/",
  "https://www.star.nesdis.noaa.gov/GOES/sector.php?sat=G18&sector=psw",
  "https://www.star.nesdis.noaa.gov/GOES/sector.php?sat=G18&sector=wus",
  "https://www.star.nesdis.noaa.gov/GOES/sector_band.php?sat=G18&sector=psw&band=GEOCOLOR&length=24&dim=1",
  "https://www.star.nesdis.noaa.gov/GOES/sector_band.php?sat=G18&sector=psw&band=FireTemperature&length=12&dim=1"
), "NOAA GOES browser access-point order changed")
usbr_resource <- bundle$resources[[match("resource_usbr", expected_resource_ids)]]
assert_identical(usbr_resource$canonicalUrl, "https://www.usbr.gov/",
                 "The projected USBR canonical action changed")
assert_identical(usbr_resource$accessPoints[[1]]$url, "https://www.usbr.gov/",
                 "The rendered USBR Official Resource href changed")
cdec_resource <- bundle$resources[[match("resource_dwr_cdec", expected_resource_ids)]]
assert_identical(
  cdec_resource$accessPoints[[2]], r15b_authorized_access_points$resource_dwr_cdec,
  "The rendered CDEC Reservoir Conditions access point changed"
)
r15c_http_only_exceptions <- pt_guide_http_only_resource_url_exceptions()
assert_identical(length(r15c_http_only_exceptions), 3L,
                 "The bundle validator HTTP-only policy must contain exactly three pairs")
for (resource_id in names(r15c_http_only_exceptions)) {
  resource <- bundle$resources[[match(resource_id, expected_resource_ids)]]
  approved_url <- unname(r15c_http_only_exceptions[[resource_id]])
  assert_true(!is.null(resource), paste("Missing HTTP-only bundle Resource:", resource_id))
  assert_identical(resource$canonicalUrl, approved_url,
                   paste("The HTTP-only bundle canonical URL changed for", resource_id))
  assert_true(approved_url %in% vapply(resource$accessPoints, `[[`, character(1), "url"),
              paste("The exact HTTP-only access action is missing for", resource_id))
}
assert_true(is.list(pt_validate_guide_bundle(bundle)),
            "The final bundle validator rejected an exact approved HTTP-only pair")

clone_bundle <- function(value) unserialize(serialize(value, NULL))
bundle_with_canonical_url <- function(resource_id, url) {
  fixture <- clone_bundle(bundle)
  resource_index <- match(resource_id, vapply(fixture$resources, `[[`, character(1), "id"))
  assert_true(!is.na(resource_index), paste("Missing bundle URL fixture Resource:", resource_id))
  canonical_index <- match(
    "canonical",
    vapply(fixture$resources[[resource_index]]$accessPoints, `[[`, character(1), "role")
  )
  assert_true(!is.na(canonical_index), paste("Missing bundle canonical access point for:", resource_id))
  fixture$resources[[resource_index]]$canonicalUrl <- url
  fixture$resources[[resource_index]]$accessPoints[[canonical_index]]$url <- url
  fixture
}
bundle_url_rejections <- list(
  wrong_path = c(
    resource_tid_turlock_irrigation_district_wiski_web_platform =
      "http://wiskiweb.tid.org/different.htm"
  ),
  wrong_id = c(resource_doi = "http://wiskiweb.tid.org/index.htm"),
  same_host_different_path = c(
    resource_krwa_kings_river_water_association_platform =
      "http://kingsriverwater.org/data"
  ),
  suffix_match = c(
    resource_tid_turlock_irrigation_district_wiski_web_platform =
      "http://wiskiweb.tid.org/index.htm/extra"
  ),
  substring_match = c(
    resource_tid_turlock_irrigation_district_wiski_web_platform =
      "http://example.gov/?next=http://wiskiweb.tid.org/index.htm"
  ),
  host_variant = c(
    resource_krwa_kings_river_water_association_platform =
      "http://www.kingsriverwater.org/"
  ),
  arbitrary_fourth = c(resource_doi = "http://example.gov/resource"),
  relative = c(resource_doi = "/resource"),
  scheme_relative = c(resource_doi = "//example.gov/resource"),
  malformed = c(resource_doi = "http:///missing-host")
)
for (fixture_name in names(bundle_url_rejections)) {
  probe <- bundle_url_rejections[[fixture_name]]
  assert_error(
    pt_validate_guide_bundle(bundle_with_canonical_url(names(probe), unname(probe))),
    "exact labeled public URL actions",
    paste("The final bundle validator accepted a prohibited public URL fixture:", fixture_name)
  )
}
arbitrary_access_bundle <- clone_bundle(bundle)
arbitrary_access_resource_index <- match(
  "resource_dwr_cdec",
  vapply(arbitrary_access_bundle$resources, `[[`, character(1), "id")
)
arbitrary_access_bundle$resources[[arbitrary_access_resource_index]]$accessPoints[[2]]$url <-
  "http://example.gov/access"
assert_error(
  pt_validate_guide_bundle(arbitrary_access_bundle),
  "exact labeled public URL actions",
  "The final bundle validator accepted an arbitrary HTTP access-point action"
)
assert_true(is.list(pt_validate_guide_bundle(bundle_with_canonical_url(
  "resource_doi", "https://example.gov/resource"
))), "The final bundle validator rejected an ordinary valid public HTTPS URL")
assert_true(!any(grepl("sector=pnw", vapply(
  goes_resource$accessPoints, `[[`, character(1), "url"
), fixed = TRUE)), "A Pacific Northwest GOES access point reached the browser")
assert_identical(goes_resource$canonicalUrl, "https://www.star.nesdis.noaa.gov/GOES/",
                 "NOAA GOES canonical Official Resource action changed")
assert_true(all(vapply(bundle$resources, function(resource) {
  identical(
    names(resource),
    c(
      "kind", "id", "title", "aliases", "provider", "providers", "summary",
      "canonicalUrl", "accessPoints", "resourceType", "resourceTypeLabel",
      "temporalCharacter", "temporalCharacterLabel", "resourceGranularity",
      "subjectTags", "informationTypeTags", "variables", "useScopes", "geographicScope",
      "mapReviewState", "mapRepresentation", "representedProducts", "searchText"
    )
  )
}, logical(1))), "Resource browser projection is not the exact public 23-field shape")
resource_metadata_vocabularies <- pt_guide_resource_metadata_vocabularies()
assert_true(all(vapply(bundle$resources, function(resource) {
  canonical <- resource_registry[[match(resource$id, registry_ids)]]
  identical(resource$resourceType, canonical$resource_type) &&
    identical(
      resource$resourceTypeLabel,
      unname(resource_metadata_vocabularies$resource_type[[resource$resourceType]])
    ) &&
    identical(resource$temporalCharacter, canonical$temporal_character) &&
    identical(
      resource$temporalCharacterLabel,
      unname(resource_metadata_vocabularies$temporal_character[[
        resource$temporalCharacter
      ]])
    ) &&
    identical(resource$geographicScope$scopeType, canonical$geographic_scope$scope_type) &&
    identical(
      resource$geographicScope$scopeLabel,
      unname(resource_metadata_vocabularies$geographic_scope_class[[
        resource$geographicScope$scopeType
      ]])
    ) &&
    identical(
      resource$geographicScope$names,
      unname(as.character(unlist(canonical$geographic_scope$names, use.names = FALSE)))
    )
}, logical(1))), "Controlled Resource metadata browser projection changed")
assert_true(all(vapply(bundle$resources, function(resource) {
  nzchar(resource$searchText) &&
    grepl(pt_guide_normalize_resource_search(resource$title), resource$searchText,
          fixed = TRUE) &&
    !grepl(resource$id, resource$searchText, fixed = TRUE)
}, logical(1))), "Resource semantic search projection is incomplete or contains stable IDs")
assert_identical(
  vapply(Filter(function(resource) identical(resource$temporalCharacter, "unknown"),
                bundle$resources), `[[`, character(1), "id"),
  registry_ids[registry_states == "published" & vapply(
    resource_registry,
    function(resource) identical(resource$temporal_character, "unknown"),
    logical(1)
  )],
  "Temporal unknown Resource projection changed"
)
assert_identical(
  vapply(Filter(function(resource) identical(resource$geographicScope$scopeType, "unknown"),
                bundle$resources), `[[`, character(1), "id"),
  registry_ids[registry_states == "published" & vapply(
    resource_registry,
    function(resource) identical(resource$geographic_scope$scope_type, "unknown"),
    logical(1)
  )],
  "Geography unknown Resource projection changed"
)
assert_true(all(vapply(bundle$resources[match(
  expected_newly_published_ids,
  vapply(bundle$resources, `[[`, character(1), "id")
)], function(resource) !length(resource$informationTypeTags), logical(1))),
"A newly published Resource received an inferred Information Type")
metadata_counts <- function(values) {
  counts <- table(values)
  setNames(as.integer(counts), names(counts))
}
assert_identical(
  metadata_counts(vapply(bundle$resources, `[[`, character(1), "resourceType")),
  c(
    analysis_tool = 11L, dashboard = 19L, data_portal_or_catalog = 81L,
    data_service_or_api = 6L, dataset_or_collection = 22L,
    documentation_or_guide = 2L, organization_homepage = 4L,
    program_or_mission = 21L, report_or_publication = 9L,
    viewer_or_explorer = 35L
  ),
  "Published Resource Type distribution changed"
)
assert_identical(
  metadata_counts(vapply(bundle$resources, `[[`, character(1), "temporalCharacter")),
  c(
    climatology_or_normals = 1L, current_or_near_real_time = 15L,
    forecast = 5L, historical_archive = 8L, mixed = 33L,
    static_reference = 10L, unknown = 138L
  ),
  "Published temporal-character distribution changed"
)
assert_identical(
  metadata_counts(vapply(bundle$resources, function(resource) {
    resource$geographicScope$scopeType
  }, character(1))),
  c(global = 49L, local = 42L, multi_state = 10L, multinational = 5L,
    national = 66L, regional = 3L, state = 31L, unknown = 4L),
  "Published geographic-scope distribution changed"
)
assert_true(requireNamespace("digest", quietly = TRUE),
            "digest is required for the captured Resource-payload regression contract")
resource_projection_json <- jsonlite::toJSON(
  bundle$resources,
  auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE, digits = NA
)
assert_true(nchar(resource_projection_json, type = "bytes") < 600000L,
            "Browser Resource projection exceeds the focused payload review threshold")
assert_identical(sum(product_subsystems == "External Layers"), 176L,
                 "External visible Product projection changed")
assert_identical(sum(product_subsystems == "Ops Live"), 47L,
                 "Ops Live active Product projection changed")
assert_identical(sum(product_subsystems == "Tools"), 4L,
                 "Tools Product projection changed")
assert_identical(sum(product_subsystems == "Basemaps / Local Layers"), 43L,
                 "Local Product projection changed")
product_entity_types <- vapply(bundle$products, `[[`, character(1), "entityType")
assert_identical(sum(product_entity_types == "Layer"), 266L,
                 "Guide layer entity projection changed")
assert_identical(sum(product_entity_types == "Tool"), 4L,
                 "Guide tool entity projection changed")
assert_true(!anyDuplicated(product_ids) && all(nzchar(product_ids)),
            "Every included Product needs one unique stable ID")
assert_identical(
  digest::digest(paste(product_ids, collapse = "\n"), algo = "sha256", serialize = FALSE),
  "a003f1882614ca2cef2487afda234ec31701d4e6be91741ffc8ac617388782da",
  "Exact ordered 270-ID Product universe changed"
)
assert_true(!anyDuplicated(product_paths[nzchar(product_paths)]) &&
              all(!nzchar(product_paths) == (product_entity_types == "Tool")),
            "Every Layer needs one unique verified path and Tools must omit synthetic paths")
assert_true(!any(grepl("(^| / )Points( / |$)", product_paths)),
            "Guide exposes stale Points terminology")
assert_true(any(grepl("Monitoring Sites/Records", product_paths, fixed = TRUE)),
            "Guide omitted current Monitoring Sites/Records terminology")
assert_true(sum(product_ids == "calsim3_network") == 1L,
            "CalSim arc/node children must reconcile to one visible Product")
assert_true("cnrfc_basin_product_availability" %in% product_ids,
            "Custom current-source CNRFC availability Product is missing")
assert_true(sum(grepl("^wild_scenic_river", product_ids)) == 5L,
            "Five current Wild & Scenic River control Products were not reconciled")
assert_true(!any(grepl("^Labels", product_paths)),
            "Label-only overlay rows must remain explicit non-Products")
runtime_basemaps <- pt_guide_basemap_products()
runtime_basemap_ids <- vapply(runtime_basemaps, `[[`, character(1), "id")
assert_identical(runtime_basemap_ids, pt_guide_basemap_ids(),
                 "Runtime-derived basemap identity changed")
assert_identical(vapply(runtime_basemaps, `[[`, character(1), "title"), pt_base_groups(),
                 "Guide-side runtime basemap adapter no longer reflects pt_base_groups()")
assert_true(!any(runtime_basemap_ids %in% product_ids) &&
              !any(vapply(bundle$products, `[[`, character(1), "brimSection") == "Basemap"),
            "Basemap records entered the projected Guide corpus")
assert_identical(pt_guide_supported_profiles()$default$excluded_ids, runtime_basemap_ids,
                 "Default profile does not explicitly exclude every runtime basemap Product")

method_ids <- vapply(bundle$articles, `[[`, character(1), "id")
method_titles <- vapply(bundle$articles, `[[`, character(1), "title")
assert_identical(
  method_ids,
  c(
    "method_how_brim_works",
    "method_display_geometry_generalization",
    "method_scan_soil_moisture_statistical_context",
    "method_snow_pillow_swe_statistical_context",
    "method_usgs_groundwater_history_summaries",
    "method_brim_live_update_timing",
    "method_brim_under_the_hood"
  ),
  "Initial Method IDs/order changed"
)
assert_identical(
  method_titles,
  c(
    "How BRIM Works",
    "Display Geometry & Generalization",
    "SCAN Soil Moisture Statistical Context",
    "Snow-Pillow SWE Statistical Context",
    "USGS Groundwater History Summaries",
    "BRIM Live Update Timing",
    "BRIM Under the Hood"
  ),
  "Initial Method titles/order changed"
)
assert_true(all(vapply(bundle$articles, function(article) {
  length(article$sections) > 0L && all(vapply(article$sections, function(section) {
    nzchar(section$title) &&
      (length(section$paragraphs) > 0L || length(section$items) > 0L || !is.null(section$table))
  }, logical(1)))
}, logical(1))), "Every maintained Method needs substantive structured content")

update_ids <- vapply(bundle$updates, `[[`, character(1), "id")
update_titles <- vapply(bundle$updates, `[[`, character(1), "title")
update_dates <- vapply(bundle$updates, `[[`, character(1), "date")
assert_identical(
  update_ids,
  c("update_read_only_layer_explorer", "update_nbm_accumulated_qpf", "update_nbm_legend_links"),
  "Verified Update IDs/order changed"
)
assert_identical(
  update_titles,
  c("Read-only Layer Explorer added", "NBM accumulated QPF forecast windows added", "NBM legend links simplified"),
  "Verified Update titles/order changed"
)
assert_identical(update_dates, c("2026-08-24", "2026-08-20", "2026-08-19"),
                 "Verified Updates are not reverse chronological")

markers <- pt_guide_descriptive_markers()
without_enrichment <- c(
  pt_guide_local_products(runtime_groups, list()),
  pt_guide_external_products(catalog_markers = list()),
  pt_guide_ops_products(MAP_DISPLAY, list()),
  pt_guide_basemap_products(),
  pt_guide_tool_products(MAP_DISPLAY)
)
without_enrichment <- Filter(function(product) !product$id %in% runtime_basemap_ids,
                             without_enrichment)
assert_identical(
  vapply(without_enrichment, `[[`, character(1), "id"),
  product_ids,
  "Absence of descriptive enrichment suppressed or created a Product"
)
assert_true(length(markers) == 26L,
            "Descriptive enrichment seam must retain all 26 maintained markers")

ops_definitions <- pt_guide_ops_source_definitions()
ops_registry <- pt_ops_live_guide_identity_registry()
assert_identical(length(ops_definitions), 48L,
                 "Current addOpsLayer definition count changed without Guide reconciliation")
assert_true(setequal(vapply(ops_definitions, `[[`, character(1), "source_token"),
                           ops_registry$source_token),
            "Ops stable identity registry is not in source-definition parity")
assert_true(!"ops_nws_surface_wind_barbs" %in% product_ids,
            "Hard-disabled Surface Wind Barbs leaked into default Guide")
assert_true(!any(grepl("(^| / )PT_[A-Z0-9_]+($| / )", product_paths, perl = TRUE)),
            "An internal Ops constant leaked into a user-facing Guide path")
assert_identical(
  bundle$products[[match("winter_storm_levels", product_ids)]]$pathLabel,
  "Ops Live / Forecasts / Outlooks / Weather Forecasts / Outlooks / NBM Snow Levels",
  "NBM Snow Levels path did not resolve its runtime display-name constant"
)

quick_members <- function(item) {
  if (identical(item$entryKind, "collection")) item$memberIds else item$productId
}
quick_product_ids <- unlist(lapply(bundle$quickAccess, quick_members), use.names = FALSE)
assert_true(all(quick_product_ids %in% product_ids),
            "Quick Access references an unavailable Product")
assert_identical(vapply(bundle$quickAccess, `[[`, character(1), "label"),
                 c(
                   "HUC8 – PRISM/BCMv8", "Groundwater Basins – Bulletin 118",
                   "Fire Perimeters", "NBM Snow Levels", "Water-Supply Basin Forecasts",
                   "Delta Operations", "USGS Streamflow", "USGS Groundwater",
                   "USDA / SCAN Soil Moisture", "Snow-Pillow SWE",
                   "Water conveyance | BRIM mapped"
                 ),
                 "Verified Quick Access configuration changed")
assert_identical(vapply(bundle$quickAccess, `[[`, character(1), "entryKind"),
                 c("layer", "layer", "collection", "layer", "layer", "layer",
                   "collection", "collection", "layer", "layer", "layer"),
                 "Quick Access layer/tool/collection classification changed")
assert_identical(vapply(bundle$quickAccess, `[[`, character(1), "typeLabel"),
                 c("Layer", "Layer", "Collection · 3 layers", "Layer", "Layer", "Layer",
                   "Collection · 2 BRIM views", "Collection · 2 BRIM views",
                   "Layer", "Layer", "Layer"),
                 "Quick Access visible type labels changed")
fire <- bundle$quickAccess[[which(vapply(bundle$quickAccess, `[[`, character(1), "id") == "quick_fire_perimeters")]]
expected_fire_ids <- c("EXT070", "EXT072", "EXT074")
assert_identical(fire$memberIds, expected_fire_ids,
                 "Fire Perimeters collection must use explicit stable IDs")
assert_true(!length(setdiff(expected_fire_ids, product_ids)),
            "A required Fire Perimeters layer is missing from the current profile")
assert_true(!length(setdiff(fire$memberIds, expected_fire_ids)),
            "An unrelated layer entered the Fire Perimeters collection")
streamflow_quick <- bundle$quickAccess[[match(
  "quick_usgs_streamflow", vapply(bundle$quickAccess, `[[`, character(1), "id")
)]]
groundwater_quick <- bundle$quickAccess[[match(
  "quick_usgs_groundwater", vapply(bundle$quickAccess, `[[`, character(1), "id")
)]]
assert_identical(streamflow_quick$memberIds, c("usgs_streamgages", "ops_streamflow_usgs_ca"),
                 "USGS Streamflow did not retain distinct Local and Ops Live Product IDs")
assert_identical(groundwater_quick$memberIds, c("usgs_wells", "product-ops-usgs-groundwater"),
                 "USGS Groundwater did not retain distinct Local and Ops Live Product IDs")
assert_identical(vapply(bundle$products[match(streamflow_quick$memberIds, product_ids)], `[[`, character(1), "brimSection"),
                 c("Local", "Ops Live"), "USGS Streamflow collection paths lost subsystem identity")
assert_identical(vapply(bundle$products[match(groundwater_quick$memberIds, product_ids)], `[[`, character(1), "brimSection"),
                 c("Local", "Ops Live"), "USGS Groundwater collection paths lost subsystem identity")
assert_true(all(vapply(bundle$quickAccess, function(item) nzchar(item$summary), logical(1))),
            "Every curated Quick Access entry requires a useful summary")
assert_true(grepl("Three complementary perimeter layers", fire$summary, fixed = TRUE) &&
              grepl("coverage and currency differ", fire$summary, fixed = TRUE),
            "Fire Perimeters collection does not explain why its three layers differ")

record_by_id <- function(records, id) {
  records[[match(id, vapply(records, `[[`, character(1), "id"))]]
}
section_by_id <- function(record, id) {
  record$sections[[match(id, vapply(record$sections, `[[`, character(1), "id"))]]
}
section_text <- function(record) {
  paste(unlist(lapply(record$sections, function(section) {
    c(section$title, section$paragraphs, section$items)
  }), use.names = FALSE), collapse = " ")
}

onboarding_basic <- record_by_id(without_enrichment, "acec")
onboarding_reapplied <- record_by_id(pt_guide_apply_product_enrichment(
  without_enrichment, list(), relationship_registry, resource_registry, product_ids
), "acec")
assert_true(identical(onboarding_basic$contentTier, "STRUCTURED_BASIC") &&
              identical(onboarding_reapplied$contentTier, "STRUCTURED_BASIC"),
            "Optional enrichment changed the default onboarding tier for an unenriched Product")

scan <- record_by_id(bundle$products, "ops_scan_soil_moisture")
assert_identical(scan$title, "SCAN Soil Moisture",
                 "SCAN Guide display title changed")
assert_identical(scan$subject, "Soil Moisture",
                 "SCAN lacks the explicit Soil Moisture subject tag")
assert_true(grepl("Soil moisture | USDA NRCS SCAN | Ca/Nv", scan$pathLabel, fixed = TRUE),
            "SCAN exact runtime path was not preserved")

how_brim_works <- record_by_id(bundle$articles, "method_how_brim_works")
how_brim_text <- section_text(how_brim_works)
assert_identical(how_brim_works$relatedProductIds, character(0),
                 "Generic How BRIM Works Method retained arbitrary Product relationships")
assert_identical(how_brim_works$sections[[2]]$title, "Using BRIM Guide",
                 "How BRIM Works lost the BRIM Guide surface name")
assert_identical(
  how_brim_works$sections[[2]]$paragraphs,
  "Use BRIM Guide to find layers and tools and to understand their sources, preparation, timing, and limitations. BRIM Guide explains map content but does not turn layers on or change map settings.",
  "How BRIM Works BRIM Guide explanation changed"
)
assert_true(all(vapply(
  c("LOCAL LAYERS", "OPS LIVE", "EXTERNAL LAYERS", "TOOLS",
    "sources, preparation, timing, and limitations"),
  function(probe) grepl(probe, how_brim_text, fixed = TRUE),
  logical(1)
)), "How BRIM Works does not explain the four practical layer/tool families")
assert_true(!grepl("PRISM|BCMv8|Guide I1|read-only index|compiler|implementation foundation",
                   how_brim_text, ignore.case = TRUE, perl = TRUE),
            "How BRIM Works contains HUC-specific or implementation-facing language")

display_geometry_method <- record_by_id(bundle$articles, "method_display_geometry_generalization")
assert_identical(display_geometry_method$relatedProductIds, c("huc8", "gw_bull118"),
                 "Topic-specific Method-to-Product relationships changed")

live_timing <- record_by_id(bundle$articles, "method_brim_live_update_timing")
expected_schedule_url <- "https://github.com/dbo99/brim-live-data-feeds/blob/main/docs/PRODUCTS.md#inventory-at-a-glance"
assert_identical(length(live_timing$externalLinks), 1L,
                 "BRIM Live Update Timing must have exactly one schedule link")
assert_identical(live_timing$externalLinks[[1]]$label, "View current BRIM Live schedule",
                 "BRIM Live schedule link lost its user-facing label")
assert_identical(live_timing$externalLinks[[1]]$role, "Current product schedule",
                 "BRIM Live schedule link lost its role label")
assert_identical(live_timing$externalLinks[[1]]$url, expected_schedule_url,
                 "BRIM Live schedule link no longer matches the verified public authority")
assert_true(all(vapply(
  c("fetch attempts", "successful publication", "model cycles", "valid times"),
  function(probe) grepl(probe, section_text(live_timing), fixed = TRUE),
  logical(1)
)), "BRIM Live timing semantics were conflated or omitted")

external_tool <- record_by_id(bundle$products, "tool_external_gis_overlay")
local_tool <- record_by_id(bundle$products, "tool_local_gis_upload")
measure_tool <- record_by_id(bundle$products, "tool_measure")
draw_tool <- record_by_id(bundle$products, "tool_teaching_markup")
tool_products <- Filter(function(product) identical(product$entityType, "Tool"), bundle$products)
assert_true(setequal(vapply(tool_products, `[[`, character(1), "id"), c(
  "tool_measure", "tool_teaching_markup", "tool_external_gis_overlay",
  "tool_local_gis_upload"
)), "Current Guide Tool inventory is not the exact four interactive utilities")
assert_true(all(vapply(tool_products, function(product) {
  nzchar(product$summary) && nzchar(product$accessHint) &&
    !nzchar(product$pathLabel) && identical(product$brimSection, "Tools") &&
    "Tool / Workflow" %in% product$informationTypes &&
    any(vapply(product$sections, function(section) {
      identical(section$id, "capabilities") && length(section$items) > 0L
    }, logical(1))) &&
    !grepl("controller|leaflet_|pt_", paste(product$summary, product$accessHint),
           ignore.case = TRUE, perl = TRUE)
}, logical(1))), "Every Tool needs an action summary, verified access hint, capabilities, and no fabricated path")
assert_true(all(c(external_tool$contentTier, local_tool$contentTier) == "SOURCE_BACKED_RICH"),
            "Upload tools lost source-backed structured Guide content")
assert_identical(external_tool$accessHint,
                 "Open External Layers, then Advanced manual URL add.",
                 "External GIS URL Overlay access hint changed")
assert_identical(local_tool$accessHint,
                 "Open Local GIS Uploads, then Upload local GIS file.",
                 "Local GIS File Upload access hint changed")
assert_true(all(vapply(
  c("FeatureServer", "MapServer", "ImageServer", "GeoJSON", "SQL filters",
    "Clear external", "three temporary external overlays"),
  function(probe) grepl(probe, section_text(external_tool), fixed = TRUE),
  logical(1)
)), "External GIS URL Overlay capabilities are incomplete")
assert_true(all(vapply(
  c("zipped shapefiles", "EPSG:4326", "Original geometry", "quantile",
    "equal-interval", "Hover and popup", "Clear uploads", "50 MB", "25,000 features"),
  function(probe) grepl(probe, section_text(local_tool), fixed = TRUE),
  logical(1)
)), "Local GIS File Upload capabilities are incomplete")
assert_true(grepl("distances and polygon areas", measure_tool$summary, fixed = TRUE) &&
              all(vapply(c("Distance or Area", "Finish", "Clear"), function(probe) {
                grepl(probe, section_text(measure_tool), fixed = TRUE)
              }, logical(1))), "Measure lacks its plain action summary or maintained controls")
assert_true(grepl("freehand map markup and text labels", draw_tool$summary, fixed = TRUE) &&
              all(vapply(c("color", "brush size", "Label mode", "Undo", "Clear"), function(probe) {
                grepl(probe, section_text(draw_tool), fixed = TRUE)
              }, logical(1))), "Draw / Label lacks its plain action summary or maintained controls")

huc8 <- record_by_id(bundle$products, "huc8")
assert_identical(huc8$contentTier, "SOURCE_BACKED_RICH", "HUC8 lost source-backed Guide detail")
assert_true(nzchar(huc8$summary) && length(huc8$sections) == 4L,
            "HUC8 curated summary/section floor is incomplete")
assert_true(all(vapply(
  unname(vapply(PT_HUC_THEME_REGISTRY, `[[`, character(1), "label")),
  function(label) grepl(label, section_text(huc8), fixed = TRUE), logical(1)
)), "HUC8 display modes drifted from current theme authority")
huc8_text <- section_text(huc8)
assert_true(all(vapply(
  c("EPSG:3310", "exact polygon weights", "Geometry-free climate/recharge tables",
    "200 m distance tolerance", "1991–2020 precipitation normal vM5", "BCMv8"),
  function(probe) grepl(probe, huc8_text, fixed = TRUE),
  logical(1)
)), "HUC8 processing, geometry, or provenance content is incomplete")
assert_identical(
  huc8$relatedArticleIds,
  c("method_how_brim_works", "method_display_geometry_generalization"),
  "HUC8 Method relationships changed"
)
assert_identical(
  huc8$relatedResourceIds,
  c(
    "resource_prism_normals", "resource_usgs_bcmv8", "resource_blm_california",
    "resource_usgs_national_hydrography_products"
  ),
  "HUC8 Resource relationships changed"
)
assert_identical(
  vapply(huc8$relatedResources, `[[`, character(1), "relationshipRole"),
  c(
    "direct_match_in_brim", "direct_match_in_brim", "source_reference",
    "selected_product_from_broader_resource"
  ),
  "HUC8 Resource roles changed"
)
prism_resource <- record_by_id(bundle$resources, "resource_prism_normals")
bcm_resource <- record_by_id(bundle$resources, "resource_usgs_bcmv8")
assert_identical(prism_resource$canonicalUrl, "https://prism.oregonstate.edu/normals/",
                 "HUC8 PRISM link changed")
assert_identical(bcm_resource$canonicalUrl, "https://www.sciencebase.gov/catalog/item/5f29c62d82cef313ed9edb39",
                 "HUC8 BCMv8 ScienceBase link changed")

bulletin118 <- record_by_id(bundle$products, "gw_bull118")
assert_identical(bulletin118$contentTier, "SOURCE_BACKED_RICH", "Bulletin 118 lost source-backed Guide detail")
assert_true(nzchar(bulletin118$summary) && length(bulletin118$sections) == 3L,
            "Bulletin 118 curated summary/section floor is incomplete")
assert_true(all(vapply(
  c("Basins only", "DWR SGMA 2019 Basin Prioritization", "BLM-managed land — %"),
  function(label) grepl(label, section_text(bulletin118), fixed = TRUE), logical(1)
)), "Bulletin 118 display modes changed")
bulletin_text <- section_text(bulletin118)
assert_true(all(vapply(
  c("515 retained basin records", "exact basin/subbasin code", "20 m distance tolerance",
    "final 2019 SGMA categories", "authoritative sources"),
  function(probe) grepl(probe, bulletin_text, fixed = TRUE),
  logical(1)
)), "Bulletin 118 controls, processing, geometry, or limitations are incomplete")
assert_identical(
  bulletin118$relatedArticleIds,
  c("method_how_brim_works", "method_display_geometry_generalization"),
  "Bulletin 118 Method relationships changed"
)

fire_recent <- record_by_id(bundle$products, "EXT070")
fire_all <- record_by_id(bundle$products, "EXT072")
fire_current <- record_by_id(bundle$products, "EXT074")
assert_true(all(c(fire_recent$contentTier, fire_all$contentTier, fire_current$contentTier) == "SOURCE_BACKED_RICH"),
            "Fire Perimeters Products lost curated Guide detail")
assert_true(grepl("CAL FIRE", section_text(fire_recent), fixed = TRUE) &&
              grepl("full historical CAL FIRE", section_text(fire_all), fixed = TRUE) &&
              grepl("current-view WFIGS", section_text(fire_current), fixed = TRUE) &&
              grepl("prescribed-fire records are excluded", section_text(fire_current), fixed = TRUE),
            "Fire Products do not retain distinct source, processing, and limitation content")
assert_identical(fire_recent$relatedResourceIds, "resource_calfire_fire_perimeters",
                 "CAL FIRE perimeter Resource relationship changed")
assert_identical(fire_all$relatedResourceIds, "resource_calfire_fire_perimeters",
                 "CAL FIRE historical perimeter Resource relationship changed")
assert_identical(fire_current$relatedResourceIds, "resource_nifc_wfigs_current",
                 "NIFC current perimeter Resource relationship changed")

generalization_method <- record_by_id(bundle$articles, "method_display_geometry_generalization")
generalization_table <- section_by_id(generalization_method, "current_portfolio")$table
assert_identical(length(generalization_table$rows), 27L,
                 "Generalization Method must expose all 27 current public disclosure rows")
assert_true(all(vapply(generalization_table$rows, function(row) {
  nzchar(row$layer) && nzchar(row$parameter) && nzchar(row$disclosure)
}, logical(1))), "Generalization Method table contains an incomplete public row")

scan_relationship_record <- relationship_registry$products[[match(
  "ops_scan_soil_moisture", relationship_registry_ids
)]]
scan_canonical_relationship <- scan_relationship_record$resource_links[[1]]
assert_identical(
  scan_canonical_relationship,
  list(
    resource_id = "resource_nrcs_scan",
    relationship_role = "direct_match_in_brim",
    evidence_refs = "00_config/guide_product_enrichment.json"
  ),
  "The exact SCAN relationship changed in sole relationship authority"
)
assert_identical(scan_relationship_record$delivery_class, "brim_managed",
                 "The explicit SCAN delivery decision changed")
delivery_decisions <- stats::setNames(
  vapply(relationship_registry$products, `[[`, character(1), "delivery_class"),
  relationship_registry_ids
)
assert_identical(unname(delivery_decisions[c(
  "ops_streamflow_usgs_ca", "product-ops-usgs-groundwater", "winter_storm_levels"
)]), rep("brim_managed", 3L),
"An explicit BRIM-managed delivery decision changed")
assert_identical(unname(delivery_decisions[["ops_streamflow_multiagency"]]),
                 "brim_enhanced",
                 "The explicit multi-agency streamflow delivery decision changed")
assert_true(
  "resource_nrcs_scan" %in% scan$relatedResourceIds &&
    any(vapply(scan$relatedResources, function(relationship) {
      identical(relationship$id, "resource_nrcs_scan") &&
        identical(relationship$relationshipRole, "direct_match_in_brim") &&
        identical(relationship$deliveryClass, "brim_managed")
    }, logical(1))),
  "The published exact SCAN direct relationship is missing"
)
exact_relationships <- unlist(lapply(bundle$products, function(product) {
  lapply(product$relatedResources, function(relationship) {
    c(list(productId = product$id), relationship)
  })
}), recursive = FALSE)
exact_relationship_roles <- vapply(
  exact_relationships, `[[`, character(1), "relationshipRole"
)
assert_identical(length(exact_relationships), 96L,
                 "Projected Product relationships must contain exactly 96 rows")
assert_identical(unname(as.integer(table(factor(
  exact_relationship_roles,
  levels = c("direct_match_in_brim", "selected_product_from_broader_resource",
             "source_reference")
)))), c(13L, 64L, 19L), "Projected Product relationship-role counts changed")
assert_true(!anyDuplicated(vapply(exact_relationships, function(relationship) {
  paste(relationship$productId, relationship$id, sep = "\r")
}, character(1))), "A duplicate projected Product-Resource pair was compiled")
assert_identical(sum(vapply(bundle$products, function(product) {
  length(product$relatedResources) > 0L
}, logical(1))), 72L, "Compiled Products-with-Resource-links count changed")
assert_identical(sum(vapply(bundle$resources, function(resource) {
  length(resource$representedProducts) > 0L
}, logical(1))), 27L, "Compiled Resources-with-Product-links count changed")
d10_product <- bundle$products[[match("ops_cdec_reservoir_storage", product_ids)]]
assert_identical(vapply(d10_product$relatedResources, `[[`, character(1), "id"),
                 c("resource_dwr_cdec", r16b_spk_id, r17b_cnrfc_resource_id),
                 "D10 must preserve CDEC/SPK and add only the exact CNRFC source Resource")
assert_identical(vapply(d10_product$relatedResources, `[[`, character(1),
                        "relationshipRole"),
                 c("selected_product_from_broader_resource", "source_reference",
                   "source_reference"),
                 "The compiled D10 relationship roles changed")
r17b_product_relationships <- function(product_id) {
  product <- bundle$products[[match(product_id, product_ids)]]
  list(
    title = product$title,
    ids = vapply(product$relatedResources, `[[`, character(1), "id"),
    roles = vapply(product$relatedResources, `[[`, character(1), "relationshipRole")
  )
}
assert_identical(r17b_product_relationships("cnrfc_stream"), list(
  title = "CNRFC river/reservoir catalog",
  ids = r17b_cnrfc_resource_id,
  roles = "selected_product_from_broader_resource"
), "The compiled CNRFC stream relationship changed")
assert_identical(r17b_product_relationships("cnrfc_precip_weather_station_catalog"), list(
  title = "CNRFC weather station catalog",
  ids = r17b_cnrfc_resource_id,
  roles = "selected_product_from_broader_resource"
), "The compiled CNRFC weather-station relationship changed")
assert_identical(r17b_product_relationships("ops_major_water_supply_forecasts"), list(
  title = "Major Water-Supply Basin Forecasts",
  ids = c("resource_noaa_nwps", "resource_usgs_national_hydrography_products",
          r17b_cnrfc_resource_id),
  roles = rep("source_reference", 3L)
), "The compiled major water-supply CNRFC source relationship changed")
spk_resource <- bundle$resources[[match(r16b_spk_id, expected_resource_ids)]]
assert_identical(spk_resource$mapRepresentation, "selected_products_in_brim",
                 "The SPK parent must be selected-products-in-BRIM")
assert_true(any(vapply(spk_resource$representedProducts, function(product) {
  identical(product$productId, "ops_cdec_reservoir_storage") &&
    identical(product$relationshipRole, "source_reference")
}, logical(1))), "The exact SPK reverse source relationship is missing")
drought_product <- bundle$products[[match("ops_us_drought_monitor", product_ids)]]
assert_identical(
  lapply(drought_product$relatedResources, function(relationship) list(
    id = relationship$id,
    relationshipRole = relationship$relationshipRole,
    deliveryClass = relationship$deliveryClass
  )),
  list(list(
    id = "resource_climate_and_drought_data_providers_drought_gov_california_dashboard",
    relationshipRole = "selected_product_from_broader_resource",
    deliveryClass = "brim_enhanced"
  )),
  "The compiled Drought.gov California relationship fixture changed"
)
cocorahs_products <- bundle$products[match(
  c("product-ops-cocorahs-ca-daily", "ops_cocorahs_conus_daily"), product_ids
)]
assert_true(all(vapply(cocorahs_products, function(product) {
  length(product$relatedResources) == 1L &&
    identical(product$relatedResources[[1]]$id, "resource_cocorahs_cocorahs_other") &&
    identical(product$relatedResources[[1]]$relationshipRole,
              "selected_product_from_broader_resource") &&
    identical(product$relatedResources[[1]]$deliveryClass, "brim_managed")
}, logical(1))), "The compiled two-Product CoCoRaHS relationship fixture changed")
california_water_watch_product_ids <- c("brim_mapped_conveyance", "ops_delta_snapshot")
assert_true(all(vapply(bundle$products[match(california_water_watch_product_ids, product_ids)],
                       function(product) !length(product$relatedResources), logical(1))),
            "An unsupported California Water Watch Product relationship remains")
cocorahs_resource <- bundle$resources[[match(
  "resource_cocorahs_cocorahs_other", expected_resource_ids
)]]
drought_resource <- bundle$resources[[match(
  "resource_climate_and_drought_data_providers_drought_gov_california_dashboard",
  expected_resource_ids
)]]
california_water_watch_resource <- bundle$resources[[match(
  "resource_dwr_california_water_watch", expected_resource_ids
)]]
assert_identical(cocorahs_resource$mapRepresentation, "selected_products_in_brim",
                 "CoCoRaHS Resource map representation changed")
assert_identical(vapply(cocorahs_resource$representedProducts, `[[`, character(1), "productId"),
                 c("product-ops-cocorahs-ca-daily", "ops_cocorahs_conus_daily"),
                 "CoCoRaHS Resource must represent exactly the two maintained Products")
assert_identical(drought_resource$mapRepresentation, "selected_products_in_brim",
                 "Drought.gov California Resource map representation changed")
assert_identical(vapply(drought_resource$representedProducts, `[[`, character(1), "productId"),
                 "ops_us_drought_monitor",
                 "Drought.gov California Resource must represent only U.S. Drought Monitor")
assert_identical(california_water_watch_resource$mapRepresentation,
                 "not_currently_mapped_in_brim",
                 "California Water Watch must remain not currently mapped")
assert_identical(length(california_water_watch_resource$representedProducts), 0L,
                 "California Water Watch retained an unsupported represented Product")
integrated_report_resource <- bundle$resources[[match(
  "resource_swrcb_impaired_waters_and_tmdls_program", expected_resource_ids
)]]
assert_identical(integrated_report_resource$title,
                 "California Integrated Reports & Impaired Waters",
                 "The evergreen Integrated Report title changed")
assert_identical(integrated_report_resource$canonicalUrl,
                 paste0("https://www.waterboards.ca.gov/water_issues/programs/",
                        "water_quality_assessment/"),
                 "The evergreen Integrated Report action changed")
assert_identical(integrated_report_resource$mapRepresentation,
                 "selected_products_in_brim",
                 "The Integrated Report map representation was not derived from exact links")
assert_identical(
  vapply(integrated_report_resource$representedProducts, `[[`, character(1), "productId"),
  c("SWRCB_2024_IR_LINES", "SWRCB_2024_IR_POLYGONS"),
  "The Integrated Report parent must represent exactly its two 2024 Products"
)
assert_true(all(vapply(integrated_report_resource$representedProducts, function(product) {
  identical(product$relationshipRole, "selected_product_from_broader_resource")
}, logical(1))), "An Integrated Report Product has the wrong relationship role")
assert_true(!grepl("integrated2010.shtml", compact_json(integrated_report_resource),
                   fixed = TRUE),
            "The obsolete 2010 Integrated Report action entered the browser payload")
resource_representation_ids <- function(value) vapply(Filter(function(resource) {
  identical(resource$mapRepresentation, value)
}, bundle$resources), `[[`, character(1), "id")
assert_identical(length(resource_representation_ids("direct_match_in_brim")), 3L,
                 "Direct-match Resource count changed")
assert_identical(length(resource_representation_ids("selected_products_in_brim")), 24L,
                 "Selected-products Resource count changed")
assert_identical(length(resource_representation_ids("not_currently_mapped_in_brim")), 183L,
                 "Not-currently-mapped Resource count changed")
assert_true(all(vapply(bundle$resources, function(resource) {
  identical(resource$mapReviewState, "reviewed")
}, logical(1))), "A published Resource lacks reviewed map-presence authority")
reverse_relationships <- unlist(lapply(bundle$resources, `[[`, "representedProducts"),
                                recursive = FALSE)
assert_identical(length(reverse_relationships), 96L,
                 "Resource reverse index must preserve all 96 exact rows")
cnrfc_resource <- bundle$resources[[match(r17b_cnrfc_resource_id, expected_resource_ids)]]
assert_identical(cnrfc_resource$summary, paste0(r17b_cnrfc_resource_summary_after, a5_summary_append),
                 "The compiled CNRFC Resource summary changed")
assert_identical(length(cnrfc_resource$accessPoints), 23L, "A5 compiled CNRFC action count changed")
assert_identical(cnrfc_resource$accessPoints[2:3], a5_new_actions, "A5 compiled entry points/placement changed")
for (change in a5_label_changes) {
  matches <- Filter(function(action) identical(action$url, change$url), cnrfc_resource$accessPoints)
  assert_identical(matches, list(list(role = "configured_view", label = change$new_label, url = change$url)),
                   "A5 compiled station label/URL mapping changed")
}
for (query in c("full natural flow", "FNF water-year trend", "Water Resources", "HD6RSA", "freezing levels",
               vapply(a5_label_changes, `[[`, character(1), "new_label"))) {
  assert_true(grepl(pt_guide_normalize_resource_search(query), cnrfc_resource$searchText, fixed = TRUE),
              paste("A5 compiled CNRFC search text lost", query))
}
assert_identical(cnrfc_resource$canonicalUrl, r17b_cnrfc_canonical_url_after,
                 "The compiled CNRFC official action changed")
assert_identical(cnrfc_resource$accessPoints[[1]], list(
  role = "canonical", label = "Official Resource",
  url = r17b_cnrfc_canonical_url_after
), "The rendered CNRFC Official Resource action changed")
assert_identical(length(unique(vapply(
  cnrfc_resource$accessPoints, function(point) {
    sub("/+$", "", tolower(point$url))
  }, character(1)
))), length(cnrfc_resource$accessPoints),
"The compiled CNRFC access actions contain a normalized duplicate")
assert_identical(cnrfc_resource$mapReviewState, "reviewed",
                 "The CNRFC Resource review state changed")
assert_identical(cnrfc_resource$mapRepresentation, "selected_products_in_brim",
                 "The CNRFC Resource representation changed")
assert_identical(vapply(cnrfc_resource$representedProducts, `[[`, character(1),
                        "productId"), c(
  "cnrfc_fnf_delta", "cnrfc_stream", "cnrfc_precip_weather_station_catalog",
  "cnrfc_basin_product_availability", "ops_cnrfc_forecast_points",
  "ops_major_water_supply_forecasts", "ops_cdec_reservoir_storage"
), "The CNRFC Resource must represent exactly seven current Products")
assert_identical(vapply(cnrfc_resource$representedProducts, `[[`, character(1),
                        "relationshipRole"), c(
  "selected_product_from_broader_resource",
  "selected_product_from_broader_resource",
  "selected_product_from_broader_resource",
  "selected_product_from_broader_resource",
  "direct_match_in_brim", "source_reference", "source_reference"
), "The seven CNRFC reverse relationship roles changed")
r17b_cnrfc_detail_ids <- c(
  "cnrfc_basin_product_availability", "cnrfc_fnf_delta",
  "cnrfc_precip_weather_station_catalog", "cnrfc_stream"
)
r17b_already_rich_cnrfc_ids <- c(
  "ops_cnrfc_forecast_points", "ops_major_water_supply_forecasts",
  "ops_cdec_reservoir_storage"
)
assert_identical(length(r17b_cnrfc_detail_ids), 4L,
                 "R17B Product-detail enrichment must remain below its five-Product ceiling")
assert_true(all(r17b_cnrfc_detail_ids %in% names(enrichment)) &&
              all(r17b_already_rich_cnrfc_ids %in% names(enrichment)),
            "The seven CNRFC-related Products do not have the expected enrichment coverage")
r17b_cnrfc_detail_products <- bundle$products[match(r17b_cnrfc_detail_ids, product_ids)]
assert_identical(vapply(r17b_cnrfc_detail_products, `[[`, character(1), "title"), c(
  "CNRFC Product Availability", "CNRFC FNF Sha/Tri/west Sierra Basins",
  "CNRFC weather station catalog", "CNRFC river/reservoir catalog"
), "R17B Product-detail enrichment changed a Product title")
assert_true(all(vapply(r17b_cnrfc_detail_products, function(product) {
  identical(product$subsystem, "Basemaps / Local Layers") &&
    identical(product$contentTier, "SOURCE_BACKED_RICH") &&
    identical(product$informationTypes, "Static Reference") &&
    nzchar(product$summary) &&
    identical(vapply(product$sections, `[[`, character(1), "id"), c(
      "capabilities", "time_period", "preparation", "geometry_limitations"
    )) &&
    all(vapply(product$sections, function(section) {
      length(section$items) > 0L && all(nzchar(section$items)) &&
        length(section$paragraphs) == 0L && is.null(section$table)
    }, logical(1))) &&
    length(product$relatedArticleIds) > 0L &&
    length(product$relatedResources) == 1L &&
    identical(product$relatedResources[[1]]$id, r17b_cnrfc_resource_id) &&
    identical(product$relatedResources[[1]]$relationshipRole,
              "selected_product_from_broader_resource") &&
    identical(product$relatedResources[[1]]$deliveryClass, "brim_managed")
}, logical(1))),
"Every R17B CNRFC detail record must compile through the standard rich-detail sections without changing relationship authority")
assert_identical(vapply(r17b_cnrfc_detail_products, `[[`, character(1), "pathLabel"), c(
  "Basemaps / Local Layers / Basins / CNRFC Product Availability",
  "Basemaps / Local Layers / Basins / CNRFC FNF Sha/Tri/west Sierra Basins",
  "Basemaps / Local Layers / Monitoring Sites/Records / CNRFC weather station catalog",
  "Basemaps / Local Layers / Monitoring Sites/Records / CNRFC river/reservoir catalog"
), "R17B Product-detail enrichment changed a Product path")
assert_identical(lapply(r17b_cnrfc_detail_products, `[[`, "subjectTags"), list(
  "Surface Water", "Surface Water", "Precipitation", "Surface Water"
), "R17B Product-detail enrichment changed controlled Product subjects")
cnrfc_fnf_detail <- r17b_cnrfc_detail_products[[match(
  "cnrfc_fnf_delta", r17b_cnrfc_detail_ids
)]]
cnrfc_fnf_preparation <- cnrfc_fnf_detail$sections[[match(
  "preparation", vapply(cnrfc_fnf_detail$sections, `[[`, character(1), "id")
)]]
cnrfc_fnf_geometry <- cnrfc_fnf_detail$sections[[match(
  "geometry_limitations", vapply(cnrfc_fnf_detail$sections, `[[`, character(1), "id")
)]]
assert_identical(cnrfc_fnf_preparation$items, r17b_cnrfc_fnf_processing_after,
                 "The exact four-sentence CNRFC FNF preparation text changed")
assert_identical(sum(cnrfc_fnf_geometry$items == r17b_cnrfc_fnf_boundary_limitation), 1L,
                 "The CNRFC FNF boundary limitation is absent or duplicated")
assert_true(!grepl(r17b_cnrfc_fnf_processing_before,
                   compact_json(cnrfc_fnf_detail), fixed = TRUE),
            "The superseded CNRFC FNF preparation text remains in the compiled detail")
baseline_rich_count <- 24L
baseline_basic_count <- 246L
baseline_editorial_count <- 0L
wave1_local_ids <- c(
  "acec", "adjudicated_gw_basins", "blm_core", "blm_offices", "ca_desert_ncl",
  "calsim3_network", "drecp", "federal_wilderness", "field_office_outer", "gsp_areas",
  "national_monuments", "national_scenic_historic_trails", "rwqcb_regions",
  "swrcb_name_text_candidates", "swrcb_pod_spatial_matches", "swrcb_wr_list_official",
  "usgs_streamgages", "usgs_wells", "water_districts", "wilderness_study_areas"
)
wave1_external_ids <- c(
  "CGS_AP_FAULT_TRACES", "CGS_AP_FAULT_ZONES", "CGS_GEOLOGY_MAP_TILED",
  "DWR_TRE_ALTAMIRA_ANNUAL_RATE_MOSAIC", "DWR_TRE_ALTAMIRA_POINT_LOCATIONS_2026Q1",
  "DWR_TRE_ALTAMIRA_TOTAL_SINCE_2015_MOSAIC", "EPA_NPL_BOUNDARIES", "EPA_SEMS_POINTS",
  "EXT051", "EXT131", "EXT132", "SWRCB_2024_IR_LINES", "SWRCB_2024_IR_POLYGONS",
  "UIC_CALGEM_POST_LIVE", "UIC_CALGEM_PRIMACY_LIVE", "UIC_EPA_LIVE",
  "UIC_EPA_REFERENCE_POINTS", "USFWS_NWR_BOUNDARIES", "USGS_QFAULTS_VISUAL",
  "USGS_RECENT_EARTHQUAKES_24H"
)
wave1_ops_ids <- c(
  "nbm_qpf", "ops_cdec_reservoir_storage", "ops_cnrfc_forecast_points",
  "ops_hrrr_surface_wind", "ops_nws_watches_warnings_advisories",
  "ops_nws_weather_stations", "ops_observed_metar_wind", "ops_qpe_mrms_1day",
  "ops_qpe_mrms_1hr", "ops_qpe_mrms_3day", "ops_qpe_rfc_1day", "ops_qpe_rfc_7day",
  "ops_radar_iem_nexrad", "ops_radar_noaa_mrms", "ops_streamflow_multiagency",
  "ops_us_drought_monitor", "ops_wpc_ero_day_1", "ops_wpc_qpf_3day",
  "ops_wpc_qpf_day_1", "product-ops-nbm-accumulated-qpf"
)
wave1_tool_ids <- character(0)
wave1_rich_ids <- c(wave1_local_ids, wave1_external_ids, wave1_ops_ids, wave1_tool_ids)
assert_identical(length(wave1_rich_ids), 60L,
                 "GUIDE-I2A2 Wave 1 must deepen exactly 60 Products")
assert_true(!anyDuplicated(wave1_rich_ids) && all(wave1_rich_ids %in% product_ids),
            "GUIDE-I2A2 Wave 1 IDs must be unique current-profile Products")
assert_true(length(wave1_rich_ids) >= 45L && length(wave1_rich_ids) <= 90L &&
              all(c(length(wave1_local_ids), length(wave1_external_ids),
                    length(wave1_ops_ids)) >= 12L),
            "GUIDE-I2A2 Wave 1 missed its target range or subsystem coverage floor")
assert_identical(
  c(Local = length(wave1_local_ids), External = length(wave1_external_ids),
    `Ops Live` = length(wave1_ops_ids), Tools = length(wave1_tool_ids)),
  c(Local = 20L, External = 20L, `Ops Live` = 20L, Tools = 0L),
  "GUIDE-I2A2 Wave 1 subsystem selection changed"
)
assert_identical(length(enrichment),
                 baseline_rich_count + length(wave1_rich_ids) + length(r17b_cnrfc_detail_ids),
                 "GUIDE-I2A2 enrichment inventory changed without review")
assert_true(all(wave1_rich_ids %in% names(enrichment)),
            "A GUIDE-I2A2 Wave 1 Product lacks a source-backed enrichment record")
assert_true(all(vapply(enrichment, function(record) {
  length(record$source_refs) > 0L &&
    all(nzchar(as.character(unlist(record$source_refs, use.names = FALSE))))
}, logical(1))), "Every enriched field set must identify current source authority")
assert_true(all(vapply(enrichment[wave1_rich_ids], function(record) {
  length(record$method_ids) > 0L && all(record$method_ids %in% method_ids)
}, logical(1))), "Every GUIDE-I2A2 Method relationship must resolve to a maintained Method")
assert_true("brim_mapped_conveyance" %in% names(enrichment) &&
              "brim_mapped_conveyance" %in% product_ids &&
              "brim_mapped_conveyance" %in% quick_product_ids &&
              !"major_conveyance" %in% product_ids,
            "Quick Access must use the current BRIM-mapped conveyance Product, not its legacy source layer")

content_tiers <- table(vapply(bundle$products, `[[`, character(1), "contentTier"))
assert_identical(as.integer(content_tiers[c("SOURCE_BACKED_RICH", "STRUCTURED_BASIC")]),
                 c(baseline_rich_count + length(wave1_rich_ids) +
                     length(r17b_cnrfc_detail_ids),
                   baseline_basic_count - length(wave1_rich_ids) -
                     length(r17b_cnrfc_detail_ids)),
                 "GUIDE-I2A2 content-tier counts changed")
assert_true(!"EDITORIAL_REVIEW_REQUIRED" %in% names(content_tiers),
            "Unexpected editorial-review tier entered the current profile")
assert_identical(baseline_editorial_count, 0L,
                 "GUIDE-I2A2 baseline editorial-review count changed")
wave1_products <- bundle$products[match(wave1_rich_ids, product_ids)]
assert_true(all(vapply(wave1_products, function(product) {
  identical(product$contentTier, "SOURCE_BACKED_RICH") && nzchar(product$summary) &&
    length(product$sections) >= 2L && all(vapply(product$sections, function(section) {
      nzchar(section$title) &&
        (length(section$paragraphs) > 0L || length(section$items) > 0L || !is.null(section$table))
    }, logical(1)))
}, logical(1))),
"Every GUIDE-I2A2 Wave 1 Product needs a concise summary and at least two meaningful detail areas")
wave1_user_text <- paste(vapply(wave1_products, function(product) {
  paste(product$summary, section_text(product))
}, character(1)), collapse = " ")
assert_true(!grepl(
  "compiler|I2A|read-only index|implementation record|registry authority|/(Users|home|private|tmp|Volumes)/|00_config|03_functions|qa/|source_repo|worktree|audit staging",
  wave1_user_text, ignore.case = TRUE, perl = TRUE
), "GUIDE-I2A2 Wave 1 content contains developer-facing language or a machine/source path")
assert_true(all(vapply(bundle$products, function(product) {
  all(nzchar(c(product$id, product$title, product$entityType, product$brimSection,
               product$provider))) &&
    (nzchar(product$pathLabel) || nzchar(product$summary)) &&
    length(product$informationTypes) > 0L
}, logical(1))), "A Product missed the structured minimum content floor")
assert_true(!any(vapply(bundle$products, function(product) {
  nzchar(product$summary) && grepl(
    "This Product is available in BRIM|may be useful for analysis|See the map for more information",
    product$summary, ignore.case = TRUE, perl = TRUE
  )
}, logical(1))), "A generic filler summary entered the Product inventory")

controlled_subjects <- c(
  "Groundwater", "Surface Water", "Water Quality", "Snow & SWE", "Soil Moisture",
  "Precipitation", "Weather & Forecasts", "Fire Weather", "Climate & Drought",
  "Fire & Burn Areas", "Ecology & Habitat", "Air Quality", "Water Rights",
  "Geology & Geophysics", "Conservation Lands & Designations",
  "Land Ownership & Administration", "Energy & Minerals", "Infrastructure & Conveyance"
)
allowed_information_types <- c(
  "Static Reference", "Live Observation", "Forecast / Outlook",
  "Model / Simulation", "Historical Context", "Screening / Derived",
  "External On-Demand Service", "Tool / Workflow"
)
all_subjects <- unique(unlist(lapply(bundle$products, `[[`, "subjectTags"), use.names = FALSE))
all_information_types <- unique(unlist(lapply(bundle$products, `[[`, "informationTypes"), use.names = FALSE))
assert_true(setequal(all_subjects, controlled_subjects) &&
              identical(pt_guide_subject_vocabulary(), controlled_subjects) &&
              !any(c("Map Tools & Workflows", "Land & Administrative Context",
                     "Geology & Subsidence", "Infrastructure") %in% all_subjects),
            "Primary Subject vocabulary is not explicit and controlled")
assert_true(setequal(all_information_types, allowed_information_types) &&
              identical(pt_guide_information_type_vocabulary(), allowed_information_types) &&
              !any(grepl("Data / Guidance Mode|Guidance Method|Interactive workflow|Static reference",
                         all_information_types, fixed = FALSE, perl = TRUE)),
            "Information Type vocabulary is not user-facing or controlled")
assert_true(any(vapply(bundle$products, function(product) length(product$subjectTags) > 1L, logical(1))),
            "Explicit taxonomy does not support multiple subject tags")

subject_counts <- table(unlist(lapply(bundle$products, `[[`, "subjectTags"), use.names = FALSE))
expected_subject_counts <- c(
  "Groundwater" = 40L, "Surface Water" = 54L, "Water Quality" = 7L,
  "Snow & SWE" = 8L, "Soil Moisture" = 2L, "Precipitation" = 28L,
  "Weather & Forecasts" = 54L, "Fire Weather" = 8L, "Climate & Drought" = 25L,
  "Fire & Burn Areas" = 12L, "Ecology & Habitat" = 34L, "Air Quality" = 6L,
  "Water Rights" = 4L, "Geology & Geophysics" = 12L,
  "Conservation Lands & Designations" = 24L, "Land Ownership & Administration" = 51L,
  "Energy & Minerals" = 19L, "Infrastructure & Conveyance" = 10L
)
assert_identical(as.integer(subject_counts[names(expected_subject_counts)]), unname(expected_subject_counts),
                 "Corrected Primary Subject counts changed")
assert_true(all(subject_counts <= 0.25 * length(bundle$products)),
            "A Primary Subject exceeds the 25 percent focused-review threshold")
precipitation_product_ids <- vapply(Filter(function(product) {
  "Precipitation" %in% product$subjectTags
}, bundle$products), `[[`, character(1), "id")
assert_true(setequal(precipitation_product_ids, c(
  "huc2", "huc4", "huc6", "huc8", "huc10", "huc12",
  "cnrfc_precip_weather_station_catalog", "EXT140", "EXT142",
  "ops_radar_iem_nexrad", "ops_radar_noaa_mrms", "ops_qpe_mrms_1hr",
  "ops_qpe_mrms_1day", "ops_qpe_mrms_3day", "ops_qpe_rfc_1day",
  "ops_qpe_rfc_7day", "product-ops-cocorahs-ca-daily",
  "ops_cocorahs_conus_daily", "ops_wpc_qpf_day_1", "ops_wpc_qpf_day_2",
  "ops_wpc_qpf_day_3", "ops_wpc_qpf_3day", "ops_wpc_qpf_7day",
  "ops_wpc_ero_day_1", "ops_cpc_6_10_precipitation",
  "ops_cpc_8_14_precipitation", "product-ops-nbm-accumulated-qpf", "nbm_qpf"
)), "The exact 28-Product Precipitation membership changed")
subject_lengths <- lengths(lapply(bundle$products, `[[`, "subjectTags"))
assert_identical(sum(subject_lengths == 0L), 8L, "Zero-subject Product count changed")
assert_identical(sum(subject_lengths == 1L), 154L, "One-subject Product count changed")
assert_identical(sum(subject_lengths > 1L), 108L, "Multi-subject Product count changed")
subjectless_ids <- product_ids[subject_lengths == 0L]
assert_true(setequal(subjectless_ids, c(
  "EPA_NPL_BOUNDARIES", "EPA_SEMS_POINTS", "EXT116", "EXT117",
  "tool_measure", "tool_teaching_markup", "tool_external_gis_overlay", "tool_local_gis_upload"
)), "Unresolved or intentional no-domain-subject Product set changed")
assert_true(all(vapply(c("EPA_NPL_BOUNDARIES", "EPA_SEMS_POINTS"), function(id) {
  identical(record_by_id(bundle$products, id)$contentTier, "SOURCE_BACKED_RICH")
}, logical(1))) && all(vapply(c("EXT116", "EXT117"), function(id) {
  identical(record_by_id(bundle$products, id)$contentTier, "STRUCTURED_BASIC")
}, logical(1))) && !"Contaminated Sites & Remediation" %in% all_subjects,
"Focused taxonomy review must deepen the two contaminated-site records without inventing a sparse subject, while retaining recreation/access review")

fire_weather_ids <- vapply(Filter(function(product) {
  "Fire Weather" %in% product$subjectTags
}, bundle$products), `[[`, character(1), "id")
assert_true(setequal(fire_weather_ids, c("EXT066", "EXT067", "EXT068", "EXT069", "EXT071", "EXT073", "EXT075", "EXT076")),
            "Fire Weather taxonomy does not match the structured External subgroup")
assert_true(all(vapply(Filter(function(product) product$id %in% fire_weather_ids, bundle$products), function(product) {
  all(c("Fire Weather", "Weather & Forecasts") %in% product$subjectTags)
}, logical(1))), "Fire-weather outlooks lost multi-tag Weather & Forecasts membership")
matches_filters <- function(product, brim_section = "", subject = "", information_type = "") {
  (!nzchar(brim_section) || identical(product$brimSection, brim_section)) &&
    (!nzchar(subject) || subject %in% product$subjectTags) &&
    (!nzchar(information_type) || information_type %in% product$informationTypes)
}
combined_fire_weather <- Filter(function(product) matches_filters(
  product, "External", "Fire Weather", "Forecast / Outlook"
), bundle$products)
assert_true(setequal(vapply(combined_fire_weather, `[[`, character(1), "id"), fire_weather_ids) &&
              !anyDuplicated(vapply(combined_fire_weather, `[[`, character(1), "id")),
            "Combined Where/Subject/Information Type filtering duplicates or loses Products")
assert_identical(length(Filter(matches_filters, bundle$products)), length(bundle$products),
                 "Clearing filters does not restore the complete projected Product inventory")
replace_selection <- function(current, selected) if (identical(current, selected)) "" else selected
assert_identical(replace_selection("", "Groundwater"), "Groundwater",
                 "Selecting an inactive facet value did not activate it")
assert_identical(replace_selection("Groundwater", "Soil Moisture"), "Soil Moisture",
                 "Selecting a second value did not replace the first within its facet group")
assert_identical(replace_selection("Soil Moisture", "Soil Moisture"), "",
                 "Selecting the active value did not clear its facet group")
local_soil <- Filter(function(product) matches_filters(
  product, brim_section = "Local", subject = "Soil Moisture"
), bundle$products)
assert_true(all(vapply(local_soil, function(product) {
  identical(product$brimSection, "Local") && "Soil Moisture" %in% product$subjectTags
}, logical(1))), "Local + Soil Moisture returned a Product outside both selected facets")
local_groundwater <- Filter(function(product) matches_filters(
  product, brim_section = "Local", subject = "Groundwater"
), bundle$products)
assert_true(length(local_groundwater) > 0L && all(vapply(local_groundwater, function(product) {
  identical(product$brimSection, "Local") && "Groundwater" %in% product$subjectTags
}, logical(1))), "Where in BRIM and Primary Subject are not AND-across")
assert_identical(scan$subjectTags, "Soil Moisture",
                 "SCAN Soil Moisture gained an unrelated inferred subject")
assert_true(all(c("Live Observation", "Historical Context") %in% scan$informationTypes),
            "SCAN Information Type tags are incomplete")

huc_climate_ids <- c("huc2", "huc4", "huc6", "huc8", "huc10", "huc12")
assert_true(all(vapply(huc_climate_ids, function(id) {
  product <- record_by_id(bundle$products, id)
  all(c("Surface Water", "Precipitation", "Climate & Drought") %in% product$subjectTags)
}, logical(1))), "A HUC level with the shared PRISM/BCMv8 themes lacks Climate & Drought taxonomy")
blm_sma <- record_by_id(bundle$products, "tool_blm_sma_context")
assert_identical(blm_sma$subjectTags, "Land Ownership & Administration",
                 "BLM Surface Management Agency has an incorrect subject")
assert_identical(blm_sma$entityType, "Layer",
                 "BLM Surface Management Agency must be presented as a Layer")
assert_identical(blm_sma$brimSection, "External",
                 "BLM Surface Management Agency Where in BRIM location is incorrect")
assert_identical(blm_sma$subsystem, "External Layers",
                 "BLM Surface Management Agency retained its implementation-owner subsystem")
assert_identical(
  blm_sma$pathLabel,
  "External Layers / Federal Land Status / Fed/State Surface Management Agency (SMA)",
  "BLM Surface Management Agency does not use its exact current user-facing path"
)
assert_true(!"Tool / Workflow" %in% blm_sma$informationTypes &&
              "External On-Demand Service" %in% blm_sma$informationTypes &&
              !grepl("Tools /|Teaching|Reference tool", blm_sma$pathLabel, perl = TRUE) &&
              grepl("adjustable-opacity reference overlay", blm_sma$summary, fixed = TRUE),
            "BLM Surface Management Agency retains Tool presentation or lacks its layer purpose")
integrated_report_ids <- c("SWRCB_2024_IR_LINES", "SWRCB_2024_IR_POLYGONS")
assert_true(all(vapply(integrated_report_ids, function(id) {
  product <- record_by_id(bundle$products, id)
  setequal(product$subjectTags, c("Surface Water", "Water Quality")) &&
    !"Groundwater" %in% product$subjectTags &&
    identical(product$subsystem, "External Layers") &&
    identical(vapply(product$relatedResources, `[[`, character(1), "id"),
              "resource_swrcb_impaired_waters_and_tmdls_program") &&
    identical(vapply(product$relatedResources, `[[`, character(1), "relationshipRole"),
              "selected_product_from_broader_resource")
}, logical(1))), "SWRCB Integrated Report taxonomy is not exact Water Quality and Surface Water")
assert_identical(
  vapply(bundle$products[grepl("Integrated Report", vapply(
    bundle$products, `[[`, character(1), "title"
  ), fixed = TRUE)], `[[`, character(1), "id"),
  integrated_report_ids,
  "The complete 270-Product universe has an unexpected Integrated Report Product"
)
multiagency_streamflow <- record_by_id(bundle$products, "ops_streamflow_multiagency")
assert_identical(multiagency_streamflow$subjectTags, "Surface Water",
                 "Multi-agency streamflow inherited an unrelated Snow & SWE subject")
cpc_ids <- c(
  "ops_cpc_6_10_temperature", "ops_cpc_6_10_precipitation",
  "ops_cpc_8_14_temperature", "ops_cpc_8_14_precipitation"
)
assert_true(all(vapply(cpc_ids, function(id) {
  product <- record_by_id(bundle$products, id)
  all(c("Climate & Drought", "Weather & Forecasts") %in% product$subjectTags) &&
    !"Land Ownership & Administration" %in% product$subjectTags &&
    "Forecast / Outlook" %in% product$informationTypes
}, logical(1))), "CPC outlook taxonomy contains an unrelated subject or misses forecast/climate context")
seismicity_ids <- c(
  "CGS_AP_FAULT_ZONES", "CGS_AP_FAULT_TRACES", "USGS_QFAULTS_VISUAL",
  "USGS_RECENT_EARTHQUAKES_24H", "USGS_RECENT_EARTHQUAKES_7D"
)
assert_true(all(vapply(seismicity_ids, function(id) {
  "Geology & Geophysics" %in% record_by_id(bundle$products, id)$subjectTags
}, logical(1))), "Seismicity/fault probes are missing Geology & Geophysics")
conservation_ids <- c(
  "federal_wilderness", "wilderness_study_areas", "acec", "ca_desert_ncl",
  "national_monuments", "EXT131", "EXT132", "USFWS_NWR_BOUNDARIES", "WSR_BLM_CA_CORRIDORS"
)
assert_true(all(vapply(conservation_ids, function(id) {
  "Conservation Lands & Designations" %in% record_by_id(bundle$products, id)$subjectTags
}, logical(1))), "A maintained conservation-land/designation probe lacks its explicit subject")
energy_ids <- c("EXT114", "EXT145", "EXT147", "EXT148", "EXT119", "EXT137", "UIC_EPA_LIVE")
assert_true(all(vapply(energy_ids, function(id) {
  "Energy & Minerals" %in% record_by_id(bundle$products, id)$subjectTags
}, logical(1))), "An oil/gas, UIC, mineral, geothermal, or renewable-energy probe lacks Energy & Minerals")
model_ids <- c(
  "calsim3_network", sprintf("EXT%03d", 31:38), "EXT057", "EXT058", "EXT059", "EXT124",
  "ops_hrrr_surface_wind", "product-ops-nbm-accumulated-qpf", "winter_storm_levels", "nbm_qpf",
  "ops_nbm_wind_guidance", "ops_gfs_surface_wind"
)
assert_true(all(vapply(model_ids, function(id) {
  "Model / Simulation" %in% record_by_id(bundle$products, id)$informationTypes
}, logical(1))), "A verified CalSim/C2VSim/NOHRSC/NWM/NOAA model probe lacks Model / Simulation")

rich_probe_ids <- c(
  "huc8", "gw_bull118", "EXT070", "EXT072", "EXT074", "blm_diffs",
  "wild_scenic_rivers_blm_ca_lines", "wild_scenic_rivers_usfs_interagency_segments",
  "wild_scenic_river_corridors_blm_ca", "wild_scenic_river_corridors_usfs_lsrs",
  "wild_scenic_river_legal_status_corridors_usfs_lsrs", "ops_scan_soil_moisture",
  "ops_snow_pillow_swe", "ops_streamflow_usgs_ca", "product-ops-usgs-groundwater",
  "winter_storm_levels", "ops_major_water_supply_forecasts", "ops_delta_snapshot",
  "brim_mapped_conveyance", "usgs_streamgages", "usgs_wells",
  "tool_local_gis_upload", "tool_external_gis_overlay"
)
assert_true(all(vapply(rich_probe_ids, function(id) {
  product <- record_by_id(bundle$products, id)
  !is.null(product) && identical(product$contentTier, "SOURCE_BACKED_RICH") &&
    nzchar(product$summary) && length(product$sections) >= 2L &&
    all(vapply(product$sections, function(section) length(section$items) > 0L, logical(1)))
}, logical(1))), "A profile-included high-priority probe missed source-backed rich content")

conveyance <- record_by_id(bundle$products, "brim_mapped_conveyance")
assert_identical(conveyance$pathLabel,
                 "Basemaps / Local Layers / Channels / Water conveyance | BRIM mapped",
                 "Current BRIM-mapped conveyance path changed")
assert_true(grepl("curated statewide water-conveyance layer", conveyance$summary, fixed = TRUE) &&
              grepl("multiple reviewed source datasets", conveyance$summary, fixed = TRUE) &&
              grepl("various reviewed source datasets", section_text(conveyance), fixed = TRUE),
            "BRIM-mapped conveyance summary no longer describes its curated multi-source role")

timing_probe_ids <- c(
  "ops_scan_soil_moisture", "ops_snow_pillow_swe", "ops_streamflow_usgs_ca",
  "product-ops-usgs-groundwater", "winter_storm_levels",
  "ops_major_water_supply_forecasts", "ops_delta_snapshot"
)
assert_true(all(vapply(timing_probe_ids, function(id) {
  product <- record_by_id(bundle$products, id)
  text <- section_text(product)
  grepl("fetch time", text, ignore.case = TRUE) &&
    grepl("distinct", text, ignore.case = TRUE) &&
    grepl("not a guarantee|do(es)? not guarantee|does not establish a producer schedule guarantee", text,
          ignore.case = TRUE, perl = TRUE)
}, logical(1))), "Timing probes conflate timestamp meanings or imply producer guarantees")

assert_true(all(vapply(bundle$quickAccess, function(item) {
  item$entryKind %in% c("layer", "tool", "collection") &&
    all(quick_members(item) %in% product_ids)
}, logical(1))), "Quick Access entries lost typed stable-ID membership")

duplicate_id <- bundle
duplicate_id$products[[2]]$id <- duplicate_id$products[[1]]$id
assert_error(pt_validate_guide_bundle(duplicate_id),
             "record IDs must be nonblank and unique",
             "Duplicate Guide IDs did not fail validation")
duplicate_path <- bundle
duplicate_path$products[[2]]$pathLabel <- duplicate_path$products[[1]]$pathLabel
assert_error(pt_validate_guide_bundle(duplicate_path),
             "nonblank Product paths must be unique",
             "Duplicate Guide paths did not fail validation")
synthetic_tool_path <- bundle
synthetic_tool_index <- match("tool_measure", vapply(
  synthetic_tool_path$products, `[[`, character(1), "id"
))
synthetic_tool_path$products[[synthetic_tool_index]]$pathLabel <-
  "Tools / Measurement / Distance and area measurement"
assert_error(pt_validate_guide_bundle(synthetic_tool_path),
             "Tools use action summaries and must not expose synthetic layer paths",
             "A Tool synthetic layer path passed bundle validation")
missing_tool_action <- bundle
missing_tool_action$products[[synthetic_tool_index]]$summary <- ""
assert_error(pt_validate_guide_bundle(missing_tool_action),
             "verified path or source-backed purpose/action summary",
             "A Tool without an action summary passed bundle validation")
missing_fire <- bundle
missing_fire$quickAccess[[match("quick_fire_perimeters", vapply(
  missing_fire$quickAccess, `[[`, character(1), "id"
))]]$memberIds <- c(expected_fire_ids, "EXT_DOES_NOT_EXIST")
assert_error(pt_validate_guide_bundle(missing_fire),
             "Quick Access references an unavailable Product",
             "Missing Fire Perimeters stable ID did not fail validation")
empty_quick <- bundle
empty_quick_index <- match("quick_fire_perimeters", vapply(
  empty_quick$quickAccess, `[[`, character(1), "id"
))
empty_quick$quickAccess[[empty_quick_index]]$memberIds <- character(0)
assert_error(pt_validate_guide_bundle(empty_quick),
             "Quick Access entries require valid typed identity, labels, summaries, and exact Product membership",
             "An empty Quick Access collection passed bundle validation")
empty_quick_projected <- pt_project_guide_bundle(empty_quick, profile_id = "empty_quick_projection_test")
assert_true(!empty_quick$quickAccess[[empty_quick_index]]$id %in% vapply(
  empty_quick_projected$quickAccess, `[[`, character(1), "id"
), "An empty Quick Access collection survived profile projection")

excluded <- c(
  "huc8", "EXT070", "resource_usgs_water_dashboard",
  "method_how_brim_works", "update_read_only_layer_explorer", "quick_groundwater_basins"
)
projected <- pt_project_guide_bundle(bundle, excluded, profile_id = "synthetic_projection_test")
pt_validate_guide_bundle(projected)
projected_json <- jsonlite::toJSON(projected, auto_unbox = TRUE, null = "null", na = "null")
for (probe in excluded) {
  assert_true(!grepl(probe, projected_json, fixed = TRUE),
              paste("Profile exclusion leaked into embedded payload:", probe))
}
assert_true(!grepl("quick_huc8", projected_json, fixed = TRUE),
            "Unavailable single Quick Access entry leaked after Product projection")
projected_fire <- projected$quickAccess[[match(
  "quick_fire_perimeters", vapply(projected$quickAccess, `[[`, character(1), "id")
)]]
assert_identical(projected_fire$memberIds, c("EXT072", "EXT074"),
                 "Profile projection did not remove only the excluded collection member")
assert_identical(projected$counts$products, bundle$counts$products - 2L,
                 "Projected Product count was not generated after exclusion")
retained_ids <- setdiff(product_ids, c("huc8", "EXT070"))
assert_identical(vapply(projected$products, `[[`, character(1), "id"), retained_ids,
                 "Stable IDs/order changed for retained Products")
assert_true(all(!vapply(projected$products, function(product) {
  "resource_usgs_water_dashboard" %in% product$relatedResourceIds
}, logical(1))), "Excluded Resource relationships leaked into Products")
assert_true(all(!vapply(projected$products, function(product) {
  "method_how_brim_works" %in% product$relatedArticleIds
}, logical(1))), "Excluded Method relationships leaked into Products")
assert_true(all(vapply(projected$products, function(product) {
  identical(
    unname(product$relatedResourceIds),
    unname(vapply(product$relatedResources, `[[`, character(1), "id"))
  )
}, logical(1))), "Projected role-labeled Resource relationships became inconsistent")
fire_projected <- pt_project_guide_bundle(bundle, "EXT072", profile_id = "fire_projection_test")
fire_projected_entry <- fire_projected$quickAccess[[match(
  "quick_fire_perimeters", vapply(fire_projected$quickAccess, `[[`, character(1), "id")
)]]
assert_identical(fire_projected_entry$memberIds, c("EXT070", "EXT074"),
                 "Partially projected Fire Perimeters collection did not retain exact available members")

guide_js <- paste(readLines(file.path("03_functions", "js", "leaflet_brim_guide.js"), warn = FALSE), collapse = "\n")
guide_css <- paste(readLines(file.path("03_functions", "css", "leaflet_brim_guide.css"), warn = FALSE), collapse = "\n")
guide_r <- paste(readLines(file.path("03_functions", "leaflet_guide_helpers.r"), warn = FALSE), collapse = "\n")
loading_r <- paste(readLines(file.path("03_functions", "leaflet_loading_helpers.r"), warn = FALSE), collapse = "\n")
map_r <- paste(readLines(file.path("05_map_build", "04_build_portatreasure2_core_map.r"), warn = FALSE), collapse = "\n")
panel_js <- paste(readLines(file.path("03_functions", "js", "leaflet_tools_adddata_panel.js"), warn = FALSE), collapse = "\n")
bundle_json <- jsonlite::toJSON(bundle, auto_unbox = TRUE, null = "null", na = "null")
assert_true(all(vapply(c(
  "deliveryClass", "coverageDisposition", "relationshipRole",
  "direct_match_in_brim", "selected_product_from_broader_resource", "source_reference",
  "mapReviewState", "mapRepresentation", "representedProducts"
), function(field) grepl(field, bundle_json, fixed = TRUE), logical(1))),
"Required schema-v2 public relationship fields are missing from the browser payload")
assert_true(!any(vapply(c(
  "temporary_r12a_legacy_public_projection", "relationshipFlags", "brimLinked",
  "beyondBrim", "displayedInBrim", "usedByBrim", "relatedExternalResource"
), function(field) grepl(field, bundle_json, fixed = TRUE), logical(1))),
"A retired R12A public relationship field remains in the browser payload")
resource_payload_json <- jsonlite::toJSON(
  bundle$resources, auto_unbox = TRUE, null = "null", na = "null",
  pretty = FALSE, digits = NA
)
architecture_text <- gsub("[[:space:]]+", " ", paste(readLines(
  file.path("08_docs", "BRIM_DEVELOPMENT_ARCHITECTURE.md"), warn = FALSE
), collapse = " "))
architecture_provenance_statement <- paste(
  "BRIM's CNRFC FNF display geometries were created by grouping and dissolving downloadable CNRFC subbasin geometries outside the current scripted preprocessing pipeline.",
  "The current pipeline reads and generalizes the prepared geometry while retaining river, reservoir, and CNRFC/NWS identifiers.",
  "Where CDEC and CNRFC FNF products represent the same river-reservoir system, BRIM uses a common display geometry; minor differences in agency watershed delineations are not represented separately."
)
assert_identical(lengths(regmatches(
  architecture_text,
  gregexpr(architecture_provenance_statement, architecture_text, fixed = TRUE)
)), 1L, "The exact architecture provenance statement is absent or duplicated")
assert_true(all(vapply(c(
  "These are BRIM's final grouped display geometries",
  "CNRFC provides the downloadable source subbasins and the forecast/FNF context",
  "The current preprocessor does not reconstruct the original grouping or dissolve",
  "the exact historical GIS toolchain is not established",
  "BRIM does not assert that CDEC and CNRFC FNF values or source boundaries are always identical"
), function(probe) grepl(probe, architecture_text, fixed = TRUE), logical(1))),
"Architecture provenance does not preserve the required ownership, pipeline, tool, and limitation distinctions")
a5_a4_browser_json <- jsonlite::toJSON(pt_guide_resource_browser_records(
  pt_guide_resource_published_records(a5_historical_registry(raw_resource_registry)),
  bundle$products, relationship_registry), auto_unbox = TRUE, null = "null", na = "null",
  pretty = FALSE, digits = NA)
assert_identical(
  digest::digest(a5_a4_browser_json, algo = "sha256", serialize = FALSE),
  "f3570eb76f80712e65b64fae1139f196599d3740dc7ad538ae0a55dc4be2e8a5",
  paste0(
    "The exact R17C1 relationship-enriched 210-Resource browser payload changed"
  )
)
staged_migration_aliases <- unlist(lapply(staged_registry, function(record) {
  unname(as.character(unlist(record$migration_aliases, use.names = FALSE)))
}), use.names = FALSE)
staged_access_urls <- unlist(lapply(staged_registry, function(record) {
  vapply(record$access_points, `[[`, character(1), "url")
}), use.names = FALSE)
assert_true(!any(vapply(
  c(staged_registry_ids, staged_migration_aliases, staged_access_urls),
  function(probe) grepl(probe, bundle_json, fixed = TRUE), logical(1)
)), "Staged IDs, migration aliases, or access points leaked into the serialized bundle")
assert_true(all(vapply(bundle$products, function(product) {
  !any(product$relatedResourceIds %in% staged_registry_ids)
}, logical(1))), "A staged Resource relationship leaked into current Product relationships")
subject_rule_source <- paste(deparse(body(pt_guide_subject_tags)), collapse = "\n")
assert_true(!grepl("grepl|tolower|structured_values|!length\\(tags\\)", subject_rule_source, perl = TRUE) &&
              grepl("row$theme", guide_r, fixed = TRUE) &&
              grepl('pt_guide_subject_tags(row_id, "Local")', guide_r, fixed = TRUE) &&
              grepl('id, "Ops Live"', guide_r, fixed = TRUE),
            "Subject taxonomy still inherits broad path/group text or an unmatched-record fallback")
assert_true(!grepl(
  "Map Tools & Workflows|Land & Administrative Context|Geology & Subsidence|\"Infrastructure\"",
  bundle_json, perl = TRUE
), "Retired or generic taxonomy labels remain in the embedded Guide payload")

assert_true(!grepl("fetch\\s*\\(", guide_js, perl = TRUE),
            "Guide browser source must not request Guide data at runtime")
assert_true(!grepl("\\b(addLayer|removeLayer|show_on_map|configure_on_map)\\b", guide_js, perl = TRUE),
            "GUIDE-I1 browser source contains a map-activation hook")
assert_true(grepl("mapActions = \"DEFERRED_TO_I2\"", guide_r, fixed = TRUE),
            "GUIDE-I1 map-action boundary is not explicit")
assert_true(grepl("window.BRIM_GUIDE", guide_js, fixed = TRUE),
            "Guide host API is missing")
assert_true(all(vapply(c("A", "B", "C", "D"), function(letter) {
  grepl(paste0("['\"]", letter, "['\"]"), guide_js, perl = TRUE)
}, logical(1))), "A-D Guide navigation is incomplete")
assert_true(grepl("resetHome", guide_js, fixed = TRUE) &&
              grepl("state.query = ''", guide_js, fixed = TRUE),
            "BRIM Guide Home reset is incomplete")
assert_true(grepl("event.key === 'Escape'", guide_js, fixed = TRUE) &&
              grepl("state.previousFocus.focus", guide_js, fixed = TRUE),
            "Escape/focus restoration lifecycle is missing")
assert_true(grepl("function asArray", guide_js, fixed = TRUE) &&
              grepl("function quickProductIds", guide_js, fixed = TRUE) &&
              grepl("asArray(item.memberIds)", guide_js, fixed = TRUE),
            "Singleton Guide payload fields are not normalized at the browser boundary")
assert_true(grepl("preventScroll: true", guide_js, fixed = TRUE) &&
              grepl("main.scrollTop = desiredScroll", guide_js, fixed = TRUE),
            "Guide focus can displace the current mobile view")
assert_true(!grepl("legacy_notes|open_legacy_notes|openPtNotes|pt-map-notes|map-notes-overlay",
                   paste(guide_js, guide_r, map_r, panel_js), ignore.case = TRUE, perl = TRUE),
            "Retired Notes runtime content or wiring remains in the current build")
assert_true(grepl("pt-map-guide-btn", map_r, fixed = TRUE) &&
              grepl("Open BRIM Guide", map_r, fixed = TRUE) &&
              grepl("guideButton.textContent = 'BRIM Guide'", map_r, fixed = TRUE),
            "Upper-left Guide control identity is incomplete")
assert_identical(length(gregexpr("window.BRIM_GUIDE.open(guideButton)", map_r, fixed = TRUE)[[1]]), 1L,
                 "Current map build must contain exactly one primary Guide opener")
assert_true(!grepl("pt-layer-explorer-btn|window.BRIM_GUIDE.open", panel_js, perl = TRUE),
            "External Layers retains a duplicate Guide entry or handler")
assert_true(grepl("searchResults", guide_js, fixed = TRUE) &&
              grepl("filtered", guide_js, fixed = TRUE) &&
              grepl("function renderResults", guide_js, fixed = TRUE),
            "Search and browse do not share one result renderer")
assert_true(grepl("function productMatchesFilters", guide_js, fixed = TRUE) &&
              grepl("function filteredProducts", guide_js, fixed = TRUE) &&
              grepl("function visibleProducts", guide_js, fixed = TRUE) &&
              grepl("state.filters.brimSection", guide_js, fixed = TRUE) &&
              !grepl("state.filters.entityType", guide_js, fixed = TRUE) &&
              grepl("state.filters.subject[0]", guide_js, fixed = TRUE) &&
              grepl("state.filters.informationType[0]", guide_js, fixed = TRUE) &&
              grepl("facetField === 'brimSection'", guide_js, fixed = TRUE) &&
              grepl("? [facetValue] : (selected ? [] : [facetValue])", guide_js, fixed = TRUE) &&
              !grepl("concat(facetValue)|ctrlKey|metaKey|long-press|longpress",
                     guide_js, ignore.case = TRUE, perl = TRUE) &&
              grepl("radioGroup ? 'radiogroup' : 'group'", guide_js, fixed = TRUE) &&
              grepl("option.setAttribute('role', 'radio')", guide_js, fixed = TRUE) &&
              grepl("option.setAttribute('aria-checked'", guide_js, fixed = TRUE) &&
              grepl("browseRadios[nextBrowseRadioIndex].click()", guide_js, fixed = TRUE) &&
              grepl("aria-pressed", guide_js, fixed = TRUE) &&
              grepl("facet-toggle", guide_js, fixed = TRUE) &&
              grepl("filter-remove", guide_js, fixed = TRUE) &&
              grepl("clear-all", guide_js, fixed = TRUE),
            "Visible single-select-within/AND-across facet behavior or accessibility is incomplete")
assert_true(grepl("searchResults.filter", guide_js, fixed = TRUE) &&
              grepl("filtered = visibleProducts()", guide_js, fixed = TRUE) &&
              grepl("renderResults(filtered", guide_js, fixed = TRUE),
            "Search, filters, and A-Z do not use the same projected Product set")
assert_true(grepl("brim-guide__close--left", guide_js, fixed = TRUE) &&
              grepl("brim-guide__close--right", guide_js, fixed = TRUE) &&
              grepl(".brim-guide__close--left", guide_css, fixed = TRUE),
            "Accepted desktop/mobile close treatment is incomplete")
assert_true(grepl("brim-guide__rail", guide_js, fixed = TRUE) &&
              grepl("brim-guide__utility", guide_js, fixed = TRUE) &&
              grepl("width: 96vw", guide_css, fixed = TRUE) &&
              grepl("height: 94vh", guide_css, fixed = TRUE) &&
              !grepl("brim-guide__header|brim-guide__footer", guide_js, perl = TRUE),
            "Accepted V4 rail-and-utility shell contract is incomplete")
assert_true(grepl("https://www.doi.gov/", guide_js, fixed = TRUE) &&
              grepl("https://www.blm.gov/california", guide_js, fixed = TRUE) &&
              grepl("link.title = definition[2]", guide_js, fixed = TRUE) &&
              grepl("link.setAttribute('aria-label', definition[2])", guide_js, fixed = TRUE),
            "Lower-rail DOI/BLM image-link accessibility contract is incomplete")
assert_true(grepl("Find in layer list", guide_js, fixed = TRUE) &&
              grepl("products.sort(function(a, b)", guide_js, fixed = TRUE) &&
              grepl("normalize(a.title)", guide_js, fixed = TRUE) &&
              grepl("String(a.id || '')", guide_js, fixed = TRUE) &&
              grepl("renderResults(filtered, 'No layers or tools match the active filters.', 'index')", guide_js, fixed = TRUE) &&
              !grepl("products.slice(0, 10)", guide_js, fixed = TRUE),
            "Exact-path locator or complete stable A-Z layer/tool index is missing")
assert_true(grepl("brim-guide__results--index", guide_css, fixed = TRUE) &&
              grepl("overflow-y: auto", guide_css, fixed = TRUE) &&
              grepl("All layers and tools A to Z", guide_js, fixed = TRUE) &&
              grepl("list.tabIndex = 0", guide_js, fixed = TRUE),
            "A-Z inventory is not a bounded accessible scroll region")
assert_true(grepl("grid-template-columns: minmax(0, 3fr) minmax(280px, 2fr)", guide_css, fixed = TRUE) &&
              grepl("brim-guide__filters", guide_css, fixed = TRUE) &&
              grepl("brim-guide__facet-button", guide_css, fixed = TRUE) &&
              grepl("white-space: normal", guide_css, fixed = TRUE) &&
              grepl("grid-template-columns: 1fr", guide_css, fixed = TRUE) &&
              grepl("max-width: 980px", guide_css, fixed = TRUE),
            "Browse-dominant desktop, stacked facets, readable labels, or intermediate stacking is missing")
assert_true(grepl("function renderStructuredSections", guide_js, fixed = TRUE) &&
              grepl("product.sections", guide_js, fixed = TRUE) &&
              grepl("article.title", guide_js, fixed = TRUE) &&
              grepl("relationshipRoleLabel(relationship.relationshipRole)", guide_js,
                    fixed = TRUE) &&
              grepl("quickDefinition.summary", guide_js, fixed = TRUE),
            "Generic structured content, Method, Resource-role, or Quick summary rendering is incomplete")
assert_true(grepl("function productResultContext", guide_js, fixed = TRUE) &&
              grepl("recordType(record) === 'Tool'", guide_js, fixed = TRUE) &&
              grepl("record.summary || record.accessHint", guide_js, fixed = TRUE) &&
              grepl("record.pathLabel || record.summary", guide_js, fixed = TRUE) &&
              grepl("What this tool does", guide_js, fixed = TRUE) &&
              grepl("How to open it", guide_js, fixed = TRUE) &&
              grepl("entityType === 'Tool' && product.accessHint", guide_js, fixed = TRUE) &&
              grepl("entityType = recordType(product)", guide_js, fixed = TRUE),
            "Generic Layer/Tool secondary-line or Tool detail rendering is incomplete")
assert_true(grepl("item.entryKind === 'collection'", guide_js, fixed = TRUE) &&
              grepl("item.typeLabel", guide_js, fixed = TRUE) &&
              grepl("data-guide-entry-kind", guide_js, fixed = TRUE) &&
              grepl("Collection · 2 BRIM views", projected_json, fixed = TRUE),
            "Quick Access does not expose layer/tool/collection distinctions")
assert_true(grepl("asArray(record.externalLinks)", guide_js, fixed = TRUE) &&
              grepl("link.target = '_blank'", guide_js, fixed = TRUE) &&
              grepl("link.rel = 'noopener noreferrer'", guide_js, fixed = TRUE) &&
              grepl("externalLink.role", guide_js, fixed = TRUE),
            "Method external links lack the generic role-labeled safe-link convention")
assert_true(!grepl("huc8|gw_bull118|EXT070|EXT074|PRISM/BCMv8|Bulletin 118", guide_js,
                   perl = TRUE),
            "Product-specific Guide content leaked into the generic browser renderer")
assert_true(!grepl("tool_(blm_sma_context|local_gis_upload|external_gis_overlay|measure|teaching_markup)|EXT072", guide_js,
                   fixed = FALSE, perl = TRUE),
            "New content introduced a record-specific browser branch")
assert_true(!grepl("source_refs|sourceRefs|runtimeStatus|freshnessStatus|nextExpectedUpdate",
                   projected_json, fixed = FALSE, perl = TRUE),
            "Source evidence or generalized runtime-status fields leaked into the browser payload")
assert_true(all(vapply(
  c("Search layers, tools, methods, resources, and updates",
    "Browse BRIM layers & tools", "All Layers & Tools A–Z", "Information Type",
    "Where in BRIM", "Primary Subject", "Model / Simulation", "Clear all",
    "Related Layers & Tools", "Open email draft"),
  function(probe) grepl(probe, guide_js, fixed = TRUE),
  logical(1)
)), "Layer/tool terminology or contact action is incomplete")
assert_true(!grepl("filter-select|Entity type|filters-clear|Clear filters",
                   paste(guide_js, guide_css), ignore.case = TRUE, perl = TRUE),
            "Superseded Product Entity Type or large Clear Filters UI remains")
assert_true(grepl("brim-guide__resource-facet-choices", guide_js, fixed = TRUE) &&
              grepl("resourceFacetChoices", guide_js, fixed = TRUE) &&
              grepl("aria-pressed", guide_js, fixed = TRUE) &&
              grepl("brim-guide__resource-sort", guide_js, fixed = TRUE),
            "Required visible Resource facet choices or native sort control are missing")
assert_true(grepl("resourceExplorerModel.resourceTypeLabel", guide_js, fixed = TRUE) &&
              grepl("resource.temporalCharacter !== 'unknown'", guide_js, fixed = TRUE) &&
              grepl("'Temporal character', resource.temporalCharacterLabel", guide_js,
                    fixed = TRUE) &&
              grepl("resource.geographicScope.scopeType !== 'unknown'", guide_js,
                    fixed = TRUE) &&
              grepl("resource.geographicScope.scopeLabel", guide_js, fixed = TRUE),
            "Controlled labels or unknown temporal/geography detail omission changed")
assert_true(!grepl("'Temporal character', 'temporalCharacter'", guide_js, fixed = TRUE) &&
              !grepl("'Geographic scope', 'geographicScope'", guide_js, fixed = TRUE) &&
              !grepl("'Named geography', 'geographic", guide_js, fixed = TRUE),
            "A deferred temporal or geographic Resource facet was activated")
assert_true(grepl("activeSummary.hidden = !", guide_js, fixed = TRUE) &&
              grepl("activeSummary.hidden = !hasActiveFilters()", guide_js, fixed = TRUE) &&
              grepl("Clear all A Explore filters", guide_js, fixed = TRUE) &&
              grepl("searchInput.focus()", guide_js, fixed = TRUE) &&
              grepl("state.filters = { brimSection: [], subject: [], informationType: [] }",
                    guide_js, fixed = TRUE) &&
              grepl("state.filters[chipField] = []", guide_js, fixed = TRUE) &&
              grepl("aria-pressed", guide_js, fixed = TRUE) &&
              grepl("aria-live", guide_js, fixed = TRUE) &&
              grepl("aria-hidden", guide_js, fixed = TRUE),
            "Contextual clear, selected state, chips, or focus behavior is incomplete")
assert_true(grepl("shown · filtered", guide_js, fixed = TRUE) &&
              grepl("filtered.length + ' of ' + products.length", guide_js, fixed = TRUE) &&
              grepl("filtered.length !== products.length", guide_js, fixed = TRUE) &&
              grepl("'02', 'All Layers & Tools A–Z', subsetStatus, true", guide_js, fixed = TRUE) &&
              grepl("detailNode.setAttribute('role', 'status')", guide_js, fixed = TRUE) &&
              grepl("detailNode.setAttribute('aria-live', 'polite')", guide_js, fixed = TRUE) &&
              grepl("display: block; grid-column: 2; justify-self: start", guide_css, fixed = TRUE) &&
              !grepl(".brim-guide__section-detail {\n    display: none;", guide_css, fixed = TRUE),
            "A–Z full/subset status is not derived from the projected Product universe or exposed accessibly")
assert_true(!grepl("Search Products|Browse BRIM Products|Find a Product|Data / guidance mode|Product family|Guide I1",
                   guide_js, fixed = FALSE, perl = TRUE),
            "Retired Guide-facing Product or implementation terminology remains")
assert_true(grepl("mailto:doconnor@blm.gov", guide_js, fixed = TRUE) &&
              grepl("encodeURIComponent('BRIM Guide feedback')", guide_js, fixed = TRUE) &&
              grepl("does not send or store the message", guide_js, fixed = TRUE),
            "Honest encoded BRIM Guide contact mailto is incomplete")
assert_true(grepl("--guide-ui: Inter", guide_css, fixed = TRUE) &&
              grepl("--guide-condensed:", guide_css, fixed = TRUE) &&
              grepl("--guide-reading: Garamond", guide_css, fixed = TRUE),
            "Accepted V4 typography stacks are incomplete")
assert_true(grepl(".brim-guide__page-heading--product h1", guide_css, fixed = TRUE) &&
              grepl("font-family: var(--guide-ui)", guide_css, fixed = TRUE) &&
              grepl(".brim-guide__result-title", guide_css, fixed = TRUE) &&
              grepl(".brim-guide__path", guide_css, fixed = TRUE),
            "Product titles, result titles, or paths are not assigned to ordinary UI typography")
assert_identical(
  length(gregexpr("var(--guide-reading)", guide_css, fixed = TRUE)[[1]]),
  1L,
  "Serif reading stack must be used only for maintained Method paragraphs"
)
assert_true(grepl(".brim-guide__method-body .brim-guide__structured-section p", guide_css, fixed = TRUE) &&
              !grepl("@font-face|SFMono-Regular|monospace", guide_css, perl = TRUE),
            "Serif scope or no-webfont/no-monospace UI boundary changed")
assert_true(grepl("record.title], 1200", guide_js, fixed = TRUE) &&
              grepl("aliases, 1100", guide_js, fixed = TRUE) &&
              grepl("normalize(record.pathLabel) === query", guide_js, fixed = TRUE) &&
              grepl("asArray(record.subjectTags), 650", guide_js, fixed = TRUE) &&
              grepl("asArray(record.informationTypes), 600", guide_js, fixed = TRUE),
            "Deterministic V4 search weight ordering changed")
score_source <- sub("^[\\s\\S]*?function scoreRecord", "function scoreRecord", guide_js, perl = TRUE)
score_source <- sub("function search[\\s\\S]*$", "", score_source, perl = TRUE)
assert_true(!grepl("record\\.(id|url)", score_source, perl = TRUE),
            "Runtime IDs or URLs entered the semantic search index")
assert_true(!grepl("pathGroups|var path =|path.join|record.family", score_source, fixed = FALSE, perl = TRUE) &&
              grepl("record.kind === 'Product' ? '' : structuredText(record)", score_source, fixed = TRUE),
            "Broad path, family, or Product section text remains token-indexed")
assert_true(grepl("query.length >= 5", guide_js, fixed = TRUE) &&
              grepl("editDistanceAtMostOne", guide_js, fixed = TRUE),
            "Conservative title/alias typo recovery changed")
product_search_text <- function(product) tolower(paste(c(
  product$title, product$aliases, product$subjectTags, product$informationTypes,
  product$provider, product$searchTerms, product$summary
), collapse = " "))
climate_search_ids <- vapply(Filter(function(product) {
  grepl("climate", product_search_text(product), fixed = TRUE)
}, bundle$products), `[[`, character(1), "id")
climate_facet_ids <- vapply(Filter(function(product) {
  "Climate & Drought" %in% product$subjectTags
}, bundle$products), `[[`, character(1), "id")
assert_identical(length(climate_search_ids), 27L, "Climate semantic-search count changed")
assert_identical(length(climate_facet_ids), 25L, "Climate & Drought facet count changed")
assert_true(all(huc_climate_ids %in% climate_search_ids) &&
              all(huc_climate_ids %in% climate_facet_ids) &&
              setequal(setdiff(climate_search_ids, climate_facet_ids),
                       c("EXT094", "ops_scan_soil_moisture")),
            "Climate search/facet difference is not explained by maintained alias/title metadata")
soil_only_products <- Filter(function(product) {
  "Soil Moisture" %in% product$subjectTags && !"Snow & SWE" %in% product$subjectTags
}, bundle$products)
assert_true(length(soil_only_products) > 0L && all(!vapply(soil_only_products, function(product) {
  grepl("(^|[^a-z])snow([^a-z]|$)", product_search_text(product), perl = TRUE)
}, logical(1))), "A soil-moisture-only Product inherits snow through an ordinary search field")
assert_true(all(c("SCAN", "soil climate analysis network") %in% scan$aliases) &&
              "Soil Moisture" %in% scan$subjectTags &&
              grepl("snow", scan$pathLabel, ignore.case = TRUE),
            "SCAN does not prove alias/subject search independent of its exact broad-parent path")
forbidden_search_ids <- vapply(Filter(function(product) {
  any(grepl("https?://|controller|PT_[A-Z]", product$searchTerms,
            ignore.case = TRUE, perl = TRUE))
}, bundle$products), `[[`, character(1), "id")
assert_true(!length(forbidden_search_ids), paste0(
  "URL, controller, or runtime-symbol text entered Product semantic search terms: ",
  paste(forbidden_search_ids, collapse = ", ")
))
record_kinds <- unique(vapply(
  c(bundle$products, bundle$articles, bundle$resources, bundle$updates),
  `[[`, character(1), "kind"
))
assert_identical(record_kinds, c("Product", "Article", "Resource", "Update"),
                 "Search/result record types are not distinguishable")

accepted_description <- paste0(
  "An integrated, water-resources-centered portal for project screening, ",
  "landscape and situational awareness, and decision support."
)
assert_identical(pt_brim_application_identity()$description, accepted_description,
                 "Shared application description changed")
assert_identical(length(gregexpr(accepted_description, loading_r, fixed = TRUE)[[1]]), 1L,
                 "Accepted description must have one current-source literal owner")
assert_true(!grepl(accepted_description, guide_r, fixed = TRUE),
            "Guide duplicated the shared description literal")
assert_true(grepl("pt_build_guide_bundle", map_r, fixed = TRUE) &&
              grepl("pt_add_brim_guide", map_r, fixed = TRUE),
            "Guide is not integrated into the current final-map build")
assert_true(!grepl("\\b276\\b", guide_r, perl = TRUE),
            "Production Guide payload contains a hardcoded fixture count")

payload_bytes <- nchar(jsonlite::toJSON(bundle, auto_unbox = TRUE, null = "null", na = "null"), type = "bytes")
baseline_payload_bytes <- 313498L
payload_growth_bytes <- payload_bytes - baseline_payload_bytes
js_bytes <- file.info(file.path("03_functions", "js", "leaflet_brim_guide.js"))$size
css_bytes <- file.info(file.path("03_functions", "css", "leaflet_brim_guide.css"))$size
assert_true(payload_bytes <= 835000L, "R17C1 default Guide payload exceeds hard review threshold")
assert_true(payload_growth_bytes >= 0L && payload_growth_bytes <= 521502L,
            "GUIDE-I2B-R17C1 embedded payload growth exceeds the review threshold")
assert_true((js_bytes + css_bytes) <= 200000L, "Guide JS + CSS exceeds hard review threshold")
assert_true(!grepl("/(Users|home|private|tmp|Volumes)/", projected_json, perl = TRUE),
            "Machine-local path leaked into Guide payload")
assert_true(!grepl("prototype diagnostic|editorial review|source_file", projected_json,
                   ignore.case = TRUE, perl = TRUE),
            "Prototype/source/audit diagnostics leaked into Guide payload")
assert_true(!grepl("\\b(rollback|defect)\\b|threshold-enforcement|QA inputs|producer QA", projected_json,
                   ignore.case = TRUE, perl = TRUE),
            "Developer-facing quality or rollback terminology leaked into Guide payload")

cat("GUIDE-I2B-R17C1 CNRFC/WPC foundation contracts passed.\n")
cat("PROFILE_ID=default\n")
cat("PRODUCTS=", bundle$counts$products, "\n", sep = "")
cat("PRODUCT_UNIVERSE=270_UNIQUE\n")
cat("PRODUCT_SUBSYSTEM_COUNTS=43_LOCAL,176_EXTERNAL,47_OPS_LIVE,4_TOOLS\n")
cat("ARTICLES=", bundle$counts$articles, "\n", sep = "")
cat("RESOURCES=", bundle$counts$resources, "\n", sep = "")
cat("RESOURCE_REGISTRY_SCHEMA_VERSION=3\n")
cat("REGISTRY_RESOURCES=215_TOTAL,210_PUBLISHED,5_STAGED\n")
cat("R16B_CANONICAL_CHANGES=13_ADDED,7_MERGED,123_ACCESS_POINTS\n")
cat("R15B_TOTAL_CANONICAL_URL_CHANGES=26\n")
cat("R15B_ACCESS_POINT_ADDITIONS=4\n")
cat("R15B_TOTAL_URL_BEARING_FIELDS_CHANGED=82\n")
cat("USBR_PROJECTED_ACTION=https://www.usbr.gov/\n")
cat("EXACT_RELATIONSHIP_ROWS=96\n")
cat("RELATIONSHIP_ROLE_COUNTS=13_DIRECT,64_SELECTED,19_SOURCE_REFERENCE\n")
cat("PRODUCTS_WITH_RESOURCE_LINKS=72\n")
cat("RESOURCES_WITH_PRODUCT_LINKS=27\n")
cat("CNRFC_RELATED_PRODUCTS=7\n")
cat("RESOURCE_REPRESENTATION_COUNTS=3_DIRECT,24_SELECTED,183_NOT_MAPPED\n")
cat("RESOURCE_PRESET_COUNTS=27_IN_BRIM_MAP,183_BEYOND_THE_MAP,210_ALL\n")
cat("STAGED_RESOURCE_LEAKAGE=0\n")
cat("NEWLY_PUBLISHED_IDS_IN_CANONICAL_ORDER=PASS\n")
cat("WAVE2_INFORMATION_TYPE_INFERENCE=NONE\n")
cat("RESOURCE_METADATA_LABEL_PROJECTION=PASS\n")
cat("TEMPORAL_SEARCH_EXCLUSION=PASS\n")
cat("UNKNOWN_DETAIL_OMISSION_CONTRACT=PASS\n")
cat("UPDATES=", bundle$counts$updates, "\n", sep = "")
cat("QUICK_ACCESS=", bundle$counts$quickAccess, "\n", sep = "")
cat("EMBEDDED_PAYLOAD_BYTES=", payload_bytes, "\n", sep = "")
cat("EMBEDDED_PAYLOAD_GROWTH_BYTES=", payload_growth_bytes, "\n", sep = "")
cat("GUIDE_JS_BYTES=", js_bytes, "\n", sep = "")
cat("GUIDE_CSS_BYTES=", css_bytes, "\n", sep = "")
cat("BROWSER_RESOURCE_SHA256=",
    digest::digest(resource_payload_json, algo = "sha256", serialize = FALSE), "\n", sep = "")
