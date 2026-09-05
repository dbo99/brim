#!/usr/bin/env Rscript

# Focused source-only contracts for the canonical schema-v3 Resource registry,
# sole Product-centric relationship registry, and public Resource projection.

fail <- function(message) stop(message, call. = FALSE)
assert_true <- function(value, message) if (!isTRUE(value)) fail(message)
assert_identical <- function(actual, expected, message) {
  if (!identical(actual, expected)) {
    fail(paste0(
      message, "\nExpected: ", paste(expected, collapse = ", "),
      "\nActual: ", paste(actual, collapse = ", ")
    ))
  }
}
assert_error <- function(expression, pattern, message) {
  error <- tryCatch({ force(expression); NULL }, error = identity)
  if (is.null(error) || !grepl(pattern, conditionMessage(error), fixed = TRUE)) {
    fail(paste0(message, if (is.null(error)) "\nNo error was raised." else paste0(
      "\nExpected error containing: ", pattern,
      "\nActual error: ", conditionMessage(error)
    )))
  }
}

assert_true(requireNamespace("jsonlite", quietly = TRUE),
            "jsonlite is required for registry QA")
assert_true(requireNamespace("digest", quietly = TRUE),
            "digest is required for registry QA")
source(file.path("03_functions", "leaflet_guide_helpers.r"))

registry_path <- file.path("00_config", "guide_resources.json")
raw_registry <- jsonlite::fromJSON(registry_path, simplifyVector = FALSE)
registry <- pt_guide_read_resource_registry(registry_path)
published <- pt_guide_resource_published_records(registry)
relationship_registry_path <- file.path(
  "00_config", "guide_product_resource_relationships.json"
)
raw_relationship_registry <- jsonlite::fromJSON(
  relationship_registry_path, simplifyVector = FALSE
)
relationship_product_ids <- vapply(
  raw_relationship_registry$products, `[[`, character(1), "product_id"
)
relationship_registry <- pt_guide_read_product_resource_relationship_registry(
  relationship_product_ids, registry, relationship_registry_path
)

expected_ids <- c(
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
expected_wave2_ids <- c(
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
  "resource_nasa_giovanni",
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
  "resource_nrcs_web_soil_survey",
  "resource_tu_wien_soil_moisture_viewer",
  "resource_usace_cwms_data_api",
  "resource_usbr",
  "resource_usda_cropland_data_layer",
  "resource_usgs_earthexplorer",
  "resource_usgs_groundwater_watch",
  "resource_usgs_national_hydrography_products",
  "resource_usgs_streamstats",
  "resource_usgs_water_data_apis",
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
r15b_removed_resource_ids <- c(
  "resource_sacramento_county_water_resources_sacramento_county_rainfall_and_stream_levels_dashboards_collection",
  "resource_kern_river_watermaster_kern_river_watermaster_platform"
)
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
r16b_access_point_family_counts <- c(
  resource_usace_sacramento_district_water_control_data_system = 30L,
  resource_usace_los_angeles_district_water_management_platform = 5L,
  resource_usbr_central_valley_operations_office_platform = 50L,
  resource_usbr = 2L,
  resource_usbr_cvp_long_term_operations_program = 1L,
  resource_usbr_lower_colorado_river_operations = 5L,
  resource_usbr_upper_colorado_basin_water_operations = 6L,
  resource_usbr_colorado_river_basin_hub = 3L,
  resource_usbr_klamath_project_water_operations_platform = 3L,
  resource_usbr_truckee_river_operating_agreement_platform = 2L,
  resource_usbr_reclamation_information_sharing_environment_platform = 5L,
  resource_usbr_central_valley_project_water_supply_program = 3L,
  resource_usbr_reclamation_hydromet_platform = 4L,
  resource_usbr_reclamation_agrimet_platform = 4L
)
r16b_recall_only_product_ids <- c(
  "cnrfc_fnf_delta", "cnrfc_stream", "cnrfc_precip_weather_station_catalog",
  "cnrfc_basin_product_availability", "EXT011", "EXT013", "ops_delta_snapshot",
  "ops_cnrfc_forecast_points", "ops_major_water_supply_forecasts"
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
r17b_newly_linked_product_ids <- c(
  "cnrfc_stream", "cnrfc_precip_weather_station_catalog"
)
r17b_existing_products_gaining_cnrfc <- c(
  "ops_major_water_supply_forecasts", "ops_cdec_reservoir_storage"
)
r17b_target_product_ids <- c(
  r17b_newly_linked_product_ids, r17b_existing_products_gaining_cnrfc
)
integrated_report_resource_id <- "resource_swrcb_impaired_waters_and_tmdls_program"
integrated_report_product_ids <- c("SWRCB_2024_IR_LINES", "SWRCB_2024_IR_POLYGONS")
integrated_report_canonical_url <- paste0(
  "https://www.waterboards.ca.gov/water_issues/programs/",
  "water_quality_assessment/"
)
integrated_report_access_points <- list(
  list(
    role = "canonical",
    label = "Surface Water Quality Assessment Program",
    url = integrated_report_canonical_url
  ),
  list(
    role = "configured_view",
    label = "2024 California Integrated Report — EPA partial approval / partial disapproval",
    url = paste0(integrated_report_canonical_url, "2024-integrated-report.html")
  ),
  list(
    role = "configured_view",
    label = "2026 California Integrated Report — State Board approved; submitted to EPA",
    url = paste0(integrated_report_canonical_url, "2026_integrated_report.html")
  )
)
integrated_report_search_aliases <- c(
  "Impaired Waters and TMDLs", "303(d)", "305(b)",
  "California Integrated Report", "impaired waters", "TMDL",
  "surface water quality assessment"
)
r16b_cvo_ordinary_labels <- c(
  "Coordinated Operations Agreement Accounting",
  "Federal Share of San Luis Reservoir",
  "Millerton Full-Natural Flow",
  "Normal Full-Natural Flow",
  "San Luis Unit Operations",
  "Shasta Flood-Control Diagram",
  "Shasta Full-Natural Flow",
  "Term 91 Status"
)
r15b_accepted_candidate_ids <- c(
  "resource_dwr_cdec_reservoir_conditions_dashboard",
  "resource_dwr_snowtrax_platform",
  "resource_dwr_snowtrax_isnobal_dashboard",
  "resource_noaa_cnrfc_quick_summary_dashboard",
  "resource_ebmud_east_bay_mud_reservoir_releases_other",
  "resource_napa_county_flood_control_napa_valley_rainfall_and_stream_monitoring_platform",
  "resource_santa_barbara_county_public_works_santa_barbara_county_real_time_hydrology_platform"
)
r15b_rejected_canonical_ids <- c(
  r15b_removed_resource_ids,
  "resource_dwr_cdec_reservoir_conditions_dashboard",
  "resource_napa_county_flood_control_napa_valley_rainfall_and_stream_monitoring_platform",
  "resource_dwr_snowtrax_isnobal_dashboard",
  "resource_santa_barbara_county_public_works_santa_barbara_county_real_time_hydrology_map_viewer"
)
r15b_authorized_access_points <- list(
  resource_dwr_cdec = list(
    role = "configured_view",
    label = "CDEC Reservoir Conditions",
    url = "https://cdec.water.ca.gov/resapp/RescondMain"
  ),
  resource_napa_county_flood_control_napa_valley_rainfall_and_stream_monitoring_map_viewer = list(
    role = "configured_view",
    label = "Napa Valley Rainfall and Stream Monitoring Home",
    url = "https://napa.onerain.com/"
  ),
  resource_dwr_snowtrax_platform = list(
    role = "configured_view",
    label = "SnowTrax iSnobal",
    url = "https://snow.water.ca.gov/isnobal"
  ),
  resource_santa_barbara_county_public_works_santa_barbara_county_real_time_hydrology_platform = list(
    role = "configured_view",
    label = "Santa Barbara County Real-Time Hydrology – Map",
    url = "https://rain.cosbpw.net/map"
  )
)
r15b_parent_canonical_urls <- c(
  resource_dwr_cdec = "https://cdec.water.ca.gov/",
  resource_napa_county_flood_control_napa_valley_rainfall_and_stream_monitoring_map_viewer =
    "https://napa.onerain.com/map",
  resource_dwr_snowtrax_platform = "https://snow.water.ca.gov/",
  resource_santa_barbara_county_public_works_santa_barbara_county_real_time_hydrology_platform =
    "https://rain.cosbpw.net/"
)
r15b_url_repairs <- c(
  resource_water_quality_and_ecosystem_data_provide_safe_to_swim_map_viewer = "https://www.mywaterquality.ca.gov/safe-to-swim/content/interactive_map/index.html",
  resource_usace_usace_warm_springs_dam_lake_sonoma_hourly_data_product = "https://www.spk-wc.usace.army.mil/fcgi-bin/hourly.py?report=wrs",
  resource_wrd_water_replenishment_district_watermaster_platform = "https://www.wrd.org/about-watermaster",
  resource_troa_truckee_river_operating_agreement_platform = "https://www.troa.net/",
  resource_epa_epa_waters_geoviewer_viewer = "https://www.epa.gov/waterdata/waters-geoviewer",
  resource_cdfa_california_department_of_food_and_agriculture_frep_program = "https://www.cdfa.ca.gov/is/ffldrs/FREP/",
  resource_six_basins_watermaster_six_basins_watermaster_platform = "https://www.6bwm.com/",
  resource_agricultural_water_and_evapotranspiratio_openet_data_explorer_viewer = "https://explore.etdata.org/",
  resource_cal_oes_california_flood_preparedness_program = "https://www.caloes.ca.gov/office-of-the-director/operations/planning-preparedness-prevention/planning-preparedness/",
  resource_usace_usace_terminus_dam_lake_kaweah_hourly_data_product = "https://www.spk-wc.usace.army.mil/fcgi-bin/hourly.py?report=trm",
  resource_usgs_usgs_site_inventory_service_other = "https://waterservices.usgs.gov/test-tools/?service=site",
  resource_tid_turlock_irrigation_district_wiski_web_platform = "http://wiskiweb.tid.org/index.htm",
  resource_sdcwa_san_diego_county_water_authority_surface_water_other = "https://www.sdcwa.org/your-water/local-water-supplies/surface-water/",
  resource_krwa_kings_river_water_association_platform = "http://kingsriverwater.org/",
  resource_usace_usace_hidden_dam_hensley_lake_hourly_data_product = "https://www.spk-wc.usace.army.mil/fcgi-bin/hourly.py?report=hid",
  resource_cw3e_cw3e_west_wrf_accumulated_snow_product = "https://cw3e.ucsd.edu/west-wrf_accumulatedsnow/",
  resource_troa_truckee_river_daily_watermaster_report_product = "https://www.troa.net/reports/dwmr/",
  resource_usgs_usgs_3d_elevation_program_program = "https://www.usgs.gov/3d-elevation-program/what-3dep",
  resource_usace_usace_sacramento_river_clear_creek_hourly_data_product = "https://www.spk-wc.usace.army.mil/fcgi-bin/hourly.py?report=scc",
  resource_noaa_cnrfc_hourly_hefs_csv_service = "https://www.cnrfc.noaa.gov/ensembleHourlyProductCSV.php",
  resource_ocpw_orange_county_hydrology_data_portal_platform = "http://hydstra.ocpublicworks.com/web.htm",
  resource_usace_usace_farmington_dam_hourly_data_product = "https://www.spk-wc.usace.army.mil/fcgi-bin/hourly.py?report=frm",
  resource_nasa_nasa_grace_interactive_browsers_and_data_access_collection = "https://grace.jpl.nasa.gov/data-analysis-tool/",
  resource_noaa_cnrfc_forcing_csv_service = "https://www.cnrfc.noaa.gov/forcingProductCSV.php",
  resource_usace_usace_pine_flat_lake_hourly_data_product = "https://www.spk-wc.usace.army.mil/fcgi-bin/hourly.py?report=pnf"
)
r15b_http_only_exceptions <- c(
  resource_tid_turlock_irrigation_district_wiski_web_platform =
    "http://wiskiweb.tid.org/index.htm",
  resource_krwa_kings_river_water_association_platform =
    "http://kingsriverwater.org/",
  resource_ocpw_orange_county_hydrology_data_portal_platform =
    "http://hydstra.ocpublicworks.com/web.htm"
)
r15b_usbr_resource_id <- "resource_usbr"
r15b_usbr_canonical_url_before <- "https://usbr.gov/"
r15b_usbr_canonical_url_after <- "https://www.usbr.gov/"
r15b_checker_artifact_ids <- c(
  "resource_sonoma_water_sonoma_county_sonoma_county_alert2_dashboards_collection",
  "resource_marin_county_flood_control_marin_county_rainfall_and_creek_data_dashboards_collection",
  "resource_santa_cruz_county_flood_control_santa_cruz_county_hydrologic_monitoring_map_viewer",
  "resource_san_joaquin_county_san_joaquin_county_hydrometeorological_data_map_viewer",
  "resource_napa_county_flood_control_napa_valley_rainfall_and_stream_monitoring_map_viewer"
)
expected_newly_published_ids <- expected_wave2_ids[
  !expected_wave2_ids %in% expected_held_staged_ids
]
expected_published_ids_before_r15c <- c(expected_ids, expected_newly_published_ids)
expected_baseline_resource_ids <- c(expected_ids, expected_wave2_ids)
wave1_ids <- expected_ids[10:33]
required_fields <- c(
  "id", "aliases", "migration_aliases", "search_aliases", "order", "title",
  "providers", "summary", "canonical_url", "access_points", "resource_type",
  "temporal_character", "resource_granularity", "subject_tags",
  "information_type_tags", "variables", "use_scopes", "geographic_scope",
  "access_class", "public_source_references", "publication_state"
)
browser_fields <- c(
  "kind", "id", "title", "aliases", "provider", "providers", "summary",
  "canonicalUrl", "accessPoints", "resourceType", "resourceTypeLabel",
  "temporalCharacter", "temporalCharacterLabel", "resourceGranularity",
  "subjectTags", "informationTypeTags", "variables", "useScopes", "geographicScope",
  "mapReviewState", "mapRepresentation", "representedProducts", "searchText"
)

assert_identical(raw_registry$schema_version, 3L, "Registry schema marker changed")
assert_true(inherits(registry, "pt_guide_resource_registry"),
            "Registry reader did not mark validated records")
assert_identical(length(registry), 211L, "Registry must contain exactly 211 Resources")
registry_ids <- vapply(registry, `[[`, character(1), "id")
published_ids <- vapply(published, `[[`, character(1), "id")
staged <- unclass(registry)[vapply(registry, `[[`, character(1), "publication_state") == "staged"]
staged_ids <- vapply(staged, `[[`, character(1), "id")
current_registry <- unclass(registry)[match(expected_ids, registry_ids)]
wave2_registry <- unclass(registry)[match(expected_wave2_ids, registry_ids)]
newly_published <- unclass(registry)[match(expected_newly_published_ids, registry_ids)]
r15c_target_registry <- unclass(registry)[seq.int(
  length(expected_baseline_resource_ids) + 1L, length(registry)
)]
r15c_target_registry_ids <- vapply(r15c_target_registry, `[[`, character(1), "id")
held_staged <- staged[match(expected_held_staged_ids, staged_ids)]
assert_identical(registry_ids[seq_along(expected_baseline_resource_ids)],
                 expected_baseline_resource_ids,
                 "The accepted 72-Resource baseline ID set/order changed")
assert_identical(
  digest::digest(paste0(paste(r15c_target_registry_ids, collapse = "\n"), "\n"),
                 algo = "sha256", serialize = FALSE),
  "49e451cd07255cc589d5b4973dcedc03eed99bcd277a59f0270a375d8ef3c18d",
  "The exact ordered post-baseline 139-Resource target ID set changed"
)
expected_published_ids <- c(expected_published_ids_before_r15c, r15c_target_registry_ids)
assert_identical(published_ids, expected_published_ids,
                 "The exact 206 published Resources changed or reordered")
assert_identical(vapply(held_staged, `[[`, character(1), "id"), expected_held_staged_ids,
                 "The exact five held staged Resources changed or reordered")
assert_identical(published_ids[published_ids %in% r15c_target_registry_ids],
                 r15c_target_registry_ids,
                 "The exact R15C target Resources are not all published in contract order")
assert_identical(length(r15c_target_registry_ids), 139L,
                 "The post-baseline authority must contain exactly 139 Resources")
assert_true(setequal(registry_ids[registry_ids %in% r16b_added_resource_ids],
                     r16b_added_resource_ids) &&
              sum(registry_ids %in% r16b_added_resource_ids) == 13L,
            "The exact 13 R16B canonical additions changed")
assert_true(!any(r16b_retired_resource_ids %in% registry_ids),
            "An exact R16B merged Resource remains canonical")
assert_identical(
  digest::digest(paste0(paste(registry_ids, collapse = "\n"), "\n"),
                 algo = "sha256", serialize = FALSE),
  "1a42cb88a6092b72e57b0a442e9c038b7083bdeb5034d6ca5c0e958e719d5d3f",
  "The exact ordered R16B 211-Resource identity set changed"
)
assert_true(!any(r15b_removed_resource_ids %in% registry_ids),
            "An R15A-invalid Resource remains in canonical authority")
assert_true(!any(r15b_rejected_canonical_ids %in% registry_ids),
            "A removed, duplicate, or subordinate R15B proposal remains canonical")
assert_identical(registry_ids[registry_ids %in% r15b_selected_replacement_ids],
                 r15b_selected_replacement_ids,
                 "The two selected R15B replacements are missing or reordered")
assert_true(all(r15b_selected_replacement_ids %in% r15b_accepted_candidate_ids),
            "An R15B replacement is outside the accepted R13 candidate corpus")
assert_true(!"resource_nasa_nasa_grace_map_comparison_slider_viewer" %in% registry_ids,
            "The blocked GRACE comparison-slider access point became a Resource")
assert_identical(sum(registry_ids == "resource_noaa_noaa_sea_level_rise_viewer_viewer"), 1L,
                 "The deterministic NOAA Sea Level Rise replacement is not present exactly once")
assert_true(!anyDuplicated(registry_ids), "Registry Resource IDs are duplicated")
assert_true(all(vapply(registry, function(record) {
  identical(names(record), required_fields)
}, logical(1))), "Registry required/allowed fields changed")
assert_identical(
  as.integer(vapply(registry, `[[`, numeric(1), "order")),
  seq_along(registry),
  "Registry order is not unique, complete, and in file order"
)
publication_states <- vapply(registry, `[[`, character(1), "publication_state")
assert_identical(sum(publication_states == "published"), 206L,
                 "Published Resource count must be 206")
assert_identical(sum(publication_states == "staged"), 5L,
                 "Staged Resource count must be 5")
assert_true(all(vapply(r15c_target_registry, function(record) {
  identical(record$publication_state, "published")
}, logical(1))), "Every R15C target Resource must be published")
assert_identical(sum(vapply(r15c_target_registry, function(record) {
  !length(record$subject_tags)
}, logical(1))), 11L,
"The precipitation micro-pass empty subject-set count changed")
precipitation_resource_ids <- vapply(Filter(function(record) {
  "Precipitation" %in% unname(as.character(unlist(
    record$subject_tags, use.names = FALSE
  )))
}, published), `[[`, character(1), "id")
assert_true(setequal(precipitation_resource_ids, c(
  "resource_prism_normals",
  "resource_dwr_california_water_watch", "resource_dwr_cdec",
  "resource_noaa_cnrfc", "resource_noaa_cpc_forecasts_outlooks",
  "resource_noaa_wpc_qpf", "resource_nrcs_snow_survey_water_supply_forecasting",
  "resource_usace_sacramento_district_water_control_data_system",
  "resource_usace_los_angeles_district_water_management_platform",
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
)) && length(precipitation_resource_ids) == 19L,
"The exact 19-Resource Precipitation membership changed")
assert_true(all(vapply(registry[match(wave1_ids, registry_ids)], function(record) {
  identical(record$publication_state, "published")
}, logical(1))), "The exact approved 24-Resource cohort was not published")
assert_true(all(vapply(newly_published, function(record) {
  identical(record$publication_state, "published")
}, logical(1))), "The exact approved 34-Resource R10 cohort was not published")
assert_identical(names(pt_guide_supported_profiles()), "default",
                 "Publication was incorrectly implemented as another profile")

expected_metadata_vocabularies <- list(
  resource_type = c(
    program_or_mission = "Program or mission",
    dataset_or_collection = "Dataset or collection",
    data_portal_or_catalog = "Data portal or catalog",
    viewer_or_explorer = "Viewer or explorer",
    dashboard = "Dashboard",
    analysis_tool = "Analysis tool",
    data_service_or_api = "Data service or API",
    documentation_or_guide = "Documentation or guide",
    organization_homepage = "Organization homepage",
    report_or_publication = "Report or publication"
  ),
  temporal_character = c(
    current_or_near_real_time = "Current or near-real-time",
    forecast = "Forecast",
    historical_archive = "Historical archive",
    climatology_or_normals = "Climatology or normals",
    static_reference = "Static reference",
    mixed = "Mixed",
    unknown = "Unknown"
  ),
  geographic_scope_class = c(
    global = "Global",
    multinational = "Multinational",
    national = "National",
    multi_state = "Multi-state",
    state = "State",
    regional = "Regional",
    local = "Local",
    unknown = "Unknown"
  )
)
assert_identical(
  pt_guide_resource_metadata_vocabularies(), expected_metadata_vocabularies,
  "Controlled Resource metadata vocabularies or exact labels changed"
)

expected_resource_types <- c(
  "organization_homepage", "organization_homepage", "dataset_or_collection",
  "dataset_or_collection", "dataset_or_collection", "dataset_or_collection",
  "dataset_or_collection", "dashboard", "data_portal_or_catalog",
  "viewer_or_explorer", "viewer_or_explorer", "viewer_or_explorer", "analysis_tool",
  "program_or_mission", "data_portal_or_catalog", "dataset_or_collection",
  "program_or_mission", "viewer_or_explorer", "data_portal_or_catalog", "dashboard",
  "viewer_or_explorer", "viewer_or_explorer", "program_or_mission",
  "program_or_mission", "data_portal_or_catalog", "analysis_tool", "dashboard",
  "dashboard", "data_portal_or_catalog", "viewer_or_explorer", "viewer_or_explorer",
  "dashboard", "dashboard"
)
expected_temporal_characters <- c(
  "static_reference", "unknown", "climatology_or_normals", "unknown",
  "static_reference", "current_or_near_real_time", "current_or_near_real_time",
  "current_or_near_real_time", "mixed", "current_or_near_real_time",
  "current_or_near_real_time", "mixed", "historical_archive", "static_reference",
  "static_reference", "mixed", "current_or_near_real_time",
  "current_or_near_real_time", "mixed", "unknown", "static_reference",
  "current_or_near_real_time", "mixed", "mixed", "historical_archive",
  "historical_archive", "mixed", "unknown", "mixed", "current_or_near_real_time",
  "mixed", "current_or_near_real_time", "current_or_near_real_time"
)
expected_geographic_scope_types <- c(
  "national", "state", "unknown", "state", "state", "state", "national",
  "national", "national", "multinational", "global", "global", "national",
  "global", "global", "global", "global", "global", "national", "national",
  "national", "national", "national", "global", "global", "global", "global",
  "national", "global", "national", "national", "national", "national"
)
expected_geographic_names <- lapply(c(
  "United States", "California", "", "California", "California", "California",
  "United States", "United States", "United States",
  "Western Hemisphere;United States", "", "",
  "United States;Western United States;California", "", "", "", "", "",
  "United States", "United States", "United States", "United States",
  "United States;California;Nevada", "", "", "", "Conterminous United States",
  "United States", "", "Conterminous United States", "Conterminous United States",
  "Conterminous United States", "Conterminous United States"
), function(value) {
  if (nzchar(value)) strsplit(value, ";", fixed = TRUE)[[1]] else character(0)
})

assert_identical(
  vapply(current_registry, `[[`, character(1), "resource_type"), expected_resource_types,
  "The exact current-33 Resource Type assignments changed"
)
assert_identical(
  vapply(current_registry, `[[`, character(1), "temporal_character"),
  expected_temporal_characters,
  "The exact current-33 temporal-character assignments changed"
)
assert_identical(
  vapply(current_registry, function(record) record$geographic_scope$scope_type, character(1)),
  expected_geographic_scope_types,
  "The exact current-33 geographic scope-class assignments changed"
)
assert_identical(
  lapply(current_registry, function(record) {
    unname(as.character(unlist(record$geographic_scope$names, use.names = FALSE)))
  }),
  expected_geographic_names,
  "The exact current-33 named-geography assignments changed"
)
expected_temporal_unknown_ids <- c(
  "resource_blm_california", "resource_usgs_bcmv8",
  "resource_nidis_soil_moisture_dashboard",
  "resource_nidis_grace_groundwater_soil_moisture"
)
assert_identical(
  expected_ids[expected_temporal_characters == "unknown"],
  expected_temporal_unknown_ids,
  "Temporal-character unknown Resource IDs changed"
)
assert_identical(
  expected_ids[expected_geographic_scope_types == "unknown"],
  "resource_prism_normals",
  "Geographic-scope unknown Resource ID changed"
)
assert_true(all(vapply(registry, function(record) {
  names <- unname(as.character(unlist(record$geographic_scope$names, use.names = FALSE)))
  !anyDuplicated(tolower(trimws(names))) && all(nzchar(names))
}, logical(1))), "Named geographies are not normalized and unique after trim/case folding")
assert_true(!"other" %in% expected_geographic_scope_types,
            "The prohibited other geography scope entered the registry")
assert_true(!any(c("update_cadence", "cadence", "update_frequency", "time_mode") %in%
                   unique(unlist(lapply(raw_registry$resources, names)))),
            "An unauthorized cadence/time-mode field entered canonical Resources")

metadata_counts <- function(values) {
  counts <- table(values)
  setNames(as.integer(counts), names(counts))
}
assert_identical(
  metadata_counts(vapply(published, `[[`, character(1), "resource_type")),
  c(
    analysis_tool = 9L, dashboard = 19L, data_portal_or_catalog = 81L,
    data_service_or_api = 8L, dataset_or_collection = 21L,
    documentation_or_guide = 2L, organization_homepage = 4L,
    program_or_mission = 21L, report_or_publication = 9L,
    viewer_or_explorer = 32L
  ),
  "Published Resource Type distribution changed"
)
assert_identical(
  metadata_counts(vapply(published, `[[`, character(1), "temporal_character")),
  c(
    climatology_or_normals = 1L, current_or_near_real_time = 15L,
    forecast = 2L, historical_archive = 7L, mixed = 31L,
    static_reference = 10L, unknown = 140L
  ),
  "Published temporal-character distribution changed"
)
assert_identical(
  metadata_counts(vapply(published, function(record) {
    record$geographic_scope$scope_type
  }, character(1))),
  c(global = 46L, local = 42L, multi_state = 10L, multinational = 5L,
    national = 66L, regional = 3L, state = 30L, unknown = 4L),
  "Published geographic-scope distribution changed"
)

compact_json <- function(value) jsonlite::toJSON(
  value,
  auto_unbox = TRUE, null = "null", na = "null", pretty = FALSE, digits = NA
)
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
cnrfc_raw_resource <- raw_registry$resources[[match(
  r17b_cnrfc_resource_id,
  vapply(raw_registry$resources, `[[`, character(1), "id")
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
reconstructed_pre_host_registry <- raw_registry
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
                   compact_json(raw_registry), fixed = TRUE),
            "The superseded CNRFC Resource summary remains in authority")
normalize_r15c_publication_state <- function(record) {
  if (record$id %in% r15c_target_registry_ids) {
    record$publication_state <- "staged"
  }
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
raw_registry_ids <- vapply(
  raw_registry$resources, `[[`, character(1), "id"
)
r15b_surviving_url_repairs <- r15b_url_repairs[
  !names(r15b_url_repairs) %in% r16b_retired_resource_ids
]
r15b_repaired_records <- raw_registry$resources[match(
  names(r15b_surviving_url_repairs), raw_registry_ids
)]
assert_identical(length(r15b_repaired_records), 19L,
                 "The exact 19 surviving R15B URL repairs changed")
for (i in seq_along(r15b_repaired_records)) {
  record <- r15b_repaired_records[[i]]
  expected_url <- unname(r15b_surviving_url_repairs[[record$id]])
  canonical_access_points <- Filter(function(access_point) {
    identical(access_point$role, "canonical")
  }, record$access_points)
  official_source_references <- Filter(function(reference) {
    identical(reference$role, "official_source")
  }, record$public_source_references)
  assert_identical(record$canonical_url, expected_url,
                   paste("R15B canonical URL repair changed:", record$id))
  assert_identical(length(canonical_access_points), 1L,
                   paste("R15B canonical access-point count changed:", record$id))
  assert_identical(canonical_access_points[[1]]$url, expected_url,
                   paste("R15B canonical access-point repair changed:", record$id))
  assert_identical(length(official_source_references), 1L,
                   paste("R15B official-source reference count changed:", record$id))
  assert_identical(official_source_references[[1]]$url, expected_url,
                   paste("R15B official-source repair changed:", record$id))
}
assert_identical(length(r15b_url_repairs), 25L,
                 "The historical R15A deterministic repair count changed")
assert_true(!r15b_usbr_resource_id %in% names(r15b_url_repairs),
            "The separate USBR host repair was folded into the R15A 25-repair count")
usbr_record <- raw_registry$resources[[match(r15b_usbr_resource_id, raw_registry_ids)]]
usbr_canonical_access_points <- Filter(function(access_point) {
  identical(access_point$role, "canonical")
}, usbr_record$access_points)
usbr_official_source_references <- Filter(function(reference) {
  identical(reference$role, "official_source")
}, usbr_record$public_source_references)
assert_identical(usbr_record$canonical_url, r15b_usbr_canonical_url_after,
                 "The USBR canonical host repair changed")
assert_identical(length(usbr_canonical_access_points), 1L,
                 "The USBR canonical access-point count changed")
assert_identical(usbr_canonical_access_points[[1]]$url, r15b_usbr_canonical_url_after,
                 "The USBR canonical access-point host repair changed")
assert_identical(length(usbr_official_source_references), 1L,
                 "The USBR official-source reference count changed")
assert_identical(usbr_official_source_references[[1]]$url,
                 r15b_usbr_canonical_url_after,
                 "The USBR official-source host repair changed")
strip_authorized_endpoint_fields <- function(record) {
  record$canonical_url <- NULL
  record$access_points <- lapply(record$access_points, function(access_point) {
    if (identical(access_point$role, "canonical")) access_point$url <- NULL
    access_point
  })
  record$public_source_references <- lapply(
    record$public_source_references,
    function(reference) {
      if (identical(reference$role, "official_source")) reference$url <- NULL
      reference
    }
  )
  record
}
assert_identical(
  digest::digest(compact_json(lapply(
    lapply(lapply(r15b_repaired_records, normalize_r15c_publication_state),
           strip_precipitation_micro_pass),
    strip_authorized_endpoint_fields
  )), algo = "sha256", serialize = FALSE),
  "fdc7bfdd164554bc4e15f827520ab072a1ddf971130cf4237af30bc1c413b7d5",
  "A protected field changed in the 19 surviving repaired R15B Resources"
)
assert_identical(
  digest::digest(compact_json(strip_authorized_endpoint_fields(
    strip_r16b_existing_resource_access_points(usbr_record)
  )),
                 algo = "sha256", serialize = FALSE),
  "23a5b927aec4c904a75b01d695d463c0c428955038b255c6c2a5c19ae0a65802",
  "A protected USBR field changed during the canonical-host repair"
)
assert_identical(
  digest::digest(compact_json(lapply(lapply(raw_registry$resources[match(
    r15b_checker_artifact_ids, raw_registry_ids
  )], normalize_r15c_publication_state), strip_r15b_authorized_access_point) |>
    lapply(strip_precipitation_micro_pass)),
  algo = "sha256", serialize = FALSE),
  "2bad10e8701add940bbd47f2f56f61eb8d644bb97d14041e232d68d4104ae60a",
  "An R15A OneRain checker-artifact field outside the authorized Napa access point changed"
)
r15b_replacement_records_without_order <- lapply(
  raw_registry$resources[match(r15b_selected_replacement_ids, raw_registry_ids)],
  function(record) {
    record <- normalize_r15c_publication_state(record)
    record <- strip_r15b_authorized_access_point(record)
    record <- strip_precipitation_micro_pass(record)
    record$order <- NULL
    record
  }
)
assert_identical(
  digest::digest(compact_json(r15b_replacement_records_without_order),
                 algo = "sha256", serialize = FALSE),
  "cc7d86bcce4cf69c81d5328be3cc201f0bf4f770df237e7c17cd8ef9a05c5366",
  paste0(
    "An R15B replacement differs from its exact accepted R13 schema-v3 record ",
    "outside the authorized subordinate access point"
  )
)
raw_current <- raw_registry$resources[match(expected_ids, vapply(
  raw_registry$resources, `[[`, character(1), "id"
))]
assert_identical(
  digest::digest(compact_json(lapply(raw_current, strip_precipitation_micro_pass)),
                 algo = "sha256", serialize = FALSE),
  "a86067ab046b93a165fd5944f85079ca7140fbb34efc0d0add79b0921f260190",
  "A current published Resource changed from the accepted R9 baseline"
)
current_non_goes_json <- compact_json(lapply(raw_current[vapply(
  raw_current, `[[`, character(1), "id"
) != "resource_noaa_goes_image_viewer"], strip_precipitation_micro_pass))
assert_identical(
  digest::digest(current_non_goes_json, algo = "sha256", serialize = FALSE),
  "ebe346a3e01dc1c98a30710438edb4f5178a23d10480be434853edb2afb4a5ab",
  "A non-GOES current Resource changed from the R8 baseline"
)
raw_goes_without_access <- raw_current[[match(
  "resource_noaa_goes_image_viewer", vapply(raw_current, `[[`, character(1), "id")
)]]
raw_goes_without_access$access_points <- NULL
assert_identical(
  digest::digest(compact_json(raw_goes_without_access), algo = "sha256", serialize = FALSE),
  "eda688b9168714c865e0a5668ef153fc52ffac976a6ffe7ac841722e76d6fbb7",
  "A NOAA GOES field other than access_points changed from the R8 baseline"
)

strip_publication_state <- function(record) {
  record$publication_state <- NULL
  record
}
newly_published_without_state_json <- compact_json(lapply(lapply(lapply(
  lapply(lapply(lapply(newly_published, strip_r15b_authorized_access_point),
         strip_r16b_existing_resource_access_points), strip_precipitation_micro_pass),
  strip_r17b_cnrfc_resource_summary_correction),
  strip_r17b_cnrfc_canonical_host_repair), strip_publication_state
))
assert_identical(
  digest::digest(newly_published_without_state_json, algo = "sha256", serialize = FALSE),
  "cdc21369a808d4619313034e9e07127572f4429ac26bbef6eb99365ef570cdca",
  paste0(
    "A newly published Resource field changed beyond publication_state and the exact ",
    "USBR canonical-host repair"
  )
)
baseline_registry <- lapply(
  raw_registry$resources[seq_along(expected_baseline_resource_ids)],
  strip_r15b_authorized_access_point
)
baseline_registry <- lapply(baseline_registry, strip_r16b_existing_resource_access_points)
baseline_registry <- lapply(baseline_registry, strip_precipitation_micro_pass)
baseline_registry <- lapply(baseline_registry, strip_r17b_cnrfc_resource_summary_correction)
baseline_registry <- lapply(baseline_registry, strip_r17b_cnrfc_canonical_host_repair)
assert_identical(
  digest::digest(compact_json(baseline_registry), algo = "sha256", serialize = FALSE),
  "bcc56bd0930cbc09ba47a513e1512d0a6e0767d90281813e50d1b518baeac181",
  "A baseline Resource changed beyond the exact USBR canonical-host repair"
)
r15c_target_without_state_json <- compact_json(lapply(
  lapply(raw_registry$resources[73:211], strip_precipitation_micro_pass),
  strip_publication_state
))
assert_identical(
  digest::digest(r15c_target_without_state_json, algo = "sha256", serialize = FALSE),
  "d8d110e410970bea6e47123fa5f301bd8416f7ede807efa9b1a128c85a8b7284",
  "The exact R16B post-baseline Resource authority changed"
)
held_json <- compact_json(held_staged)
assert_identical(
  digest::digest(held_json, algo = "sha256", serialize = FALSE),
  "b76edaea607d39160f83855d3e8ab09d06dcf9e86fa0da6c7e732b45afdde807",
  "A held staged Resource changed from the accepted R9 baseline"
)
assert_identical(as.integer(vapply(wave2_registry, `[[`, numeric(1), "order")), 34:72,
                 "Wave-2 Resource orders must remain exactly 34 through 72")
assert_identical(as.integer(vapply(newly_published, `[[`, numeric(1), "order")),
                 setdiff(34:72, c(48L, 61L, 65L, 66L, 70L)),
                 "The exact R10 publication orders changed")
assert_true(all(vapply(wave2_registry, function(record) {
  !length(record$aliases) &&
    !length(record$migration_aliases) && !length(record$search_aliases)
}, logical(1))), "Wave-2 empty alias contract changed")
assert_true(all(vapply(wave2_registry, function(record) {
  !length(record$information_type_tags)
}, logical(1))), "Every Wave-2 Resource must retain an empty Information Type array")
assert_identical(vapply(Filter(function(record) {
  !length(record$subject_tags)
}, wave2_registry), `[[`, character(1), "id"), expected_held_staged_ids,
"The exact three subject-review and two taxonomy-blocked Resources changed")
assert_true(!"resource_noaa_wpc_excessive_rainfall_outlook" %in% registry_ids,
            "The unresolved WPC Excessive Rainfall Outlook entered the registry")
assert_identical(wave2_registry[[match("resource_dwr_cdec", expected_wave2_ids)]]$resource_granularity,
                 "unknown", "CDEC intake granularity was not normalized to schema-v3 unknown")
assert_true(all(vapply(c(
  "resource_noaa_coastwatch_erddap", "resource_usgs_water_data_apis"
), function(id) identical(wave2_registry[[match(id, expected_wave2_ids)]]$resource_granularity, "platform"),
logical(1))), "Service intake granularities were not normalized to schema-v3 platform")

final_aliases <- unlist(lapply(registry, function(record) {
  unname(as.character(unlist(record$aliases, use.names = FALSE)))
}), use.names = FALSE)
migration_aliases <- unlist(lapply(registry, function(record) {
  unname(as.character(unlist(record$migration_aliases, use.names = FALSE)))
}), use.names = FALSE)
assert_true(setequal(final_aliases, r16b_retired_resource_ids) &&
              length(final_aliases) == length(r16b_retired_resource_ids),
            "The exact seven retired stable IDs are not preserved as final-ID aliases")
baseline_migration_aliases <- unlist(lapply(
  unclass(registry)[seq_along(expected_baseline_resource_ids)], function(record) {
    unname(as.character(unlist(record$migration_aliases, use.names = FALSE)))
  }
), use.names = FALSE)
assert_identical(length(baseline_migration_aliases), 26L,
                 "The accepted baseline migration alias inventory changed")
assert_true(!anyDuplicated(migration_aliases), "Migration aliases are duplicated")

all_access_valid <- vapply(registry, function(record) {
  urls <- vapply(record$access_points, `[[`, character(1), "url")
  roles <- vapply(record$access_points, `[[`, character(1), "role")
  labels <- vapply(record$access_points, `[[`, character(1), "label")
  approved_http_url <- unname(r15b_http_only_exceptions[record$id])
  all(vapply(urls, function(url) {
    grepl("^https://", url) ||
      (length(approved_http_url) == 1L && !is.na(approved_http_url) &&
         identical(url, approved_http_url))
  }, logical(1))) &&
    !anyDuplicated(urls) && all(nzchar(labels)) &&
    sum(roles == "canonical") == 1L &&
    identical(urls[[match("canonical", roles)]], record$canonical_url)
}, logical(1))
assert_true(all(all_access_valid),
            "Registry access-point URLs/labels/canonical identity changed")
r16b_access_points <- lapply(names(r16b_access_point_family_counts), function(id) {
  points <- registry[[match(id, registry_ids)]]$access_points
  points[vapply(points, `[[`, character(1), "role") != "canonical"]
})
names(r16b_access_points) <- names(r16b_access_point_family_counts)
assert_identical(vapply(r16b_access_points, length, integer(1)),
                 r16b_access_point_family_counts,
                 "The exact R16B access-point family counts changed")
assert_identical(sum(vapply(r16b_access_points, length, integer(1))), 123L,
                 "R16B must add exactly 123 curated access points")
assert_identical(
  digest::digest(compact_json(r16b_access_points), algo = "sha256", serialize = FALSE),
  "7a7c306078255c57c82cd123e7106c93fdef2ec95045d3def0b2da8647e85940",
  "The exact ordered R16B access-point authority changed"
)
r16b_access_urls <- unlist(lapply(r16b_access_points, function(points) {
  vapply(points, `[[`, character(1), "url")
}), use.names = FALSE)
assert_true(!anyDuplicated(tolower(sub("/$", "", r16b_access_urls))),
            "R16B contains a duplicate normalized access-point action")
cvo_points <- r16b_access_points$resource_usbr_central_valley_operations_office_platform
cvo_labels <- vapply(cvo_points, `[[`, character(1), "label")
assert_identical(length(cvo_points), 50L, "The CVO child contract must contain 50 actions")
assert_identical(sum(cvo_labels %in% r16b_cvo_ordinary_labels), 8L,
                 "The CVO child contract must retain exactly eight ordinary actions")
assert_identical(sum(!cvo_labels %in% r16b_cvo_ordinary_labels), 42L,
                 "The CVO child contract must retain exactly 42 searchable actions")
spk <- registry[[match(r16b_spk_id, registry_ids)]]
spk_labels <- vapply(spk$access_points, `[[`, character(1), "label")
spk_urls <- vapply(spk$access_points, `[[`, character(1), "url")
assert_true(all(c(
  "Bear Dam & Reservoir Hourly Data", "Burns Dam & Reservoir Hourly Data",
  "Mariposa Dam & Reservoir Hourly Data", "Success Dam & Lake Hourly Data"
) %in% spk_labels), "The exact official SPK report-code labels changed")
assert_identical(spk_labels[[match(
  "https://water.usace.army.mil/office/spk/reports/getreport.html?type=hourly&report=scc",
  spk_urls
)]], "Success Dam & Lake Hourly Data", "SPK report code scc is mislabeled")
assert_true(any(grepl("Legacy/standby", spk_labels, fixed = TRUE)) &&
              any(!grepl("Legacy/standby", spk_labels, fixed = TRUE)),
            "Current and legacy SPK lifecycle labels are not both present")
public_spk_search_text <- paste(c(spk$title, spk$summary, spk$search_aliases, spk_labels),
                                collapse = " ")
assert_true(!grepl("Sacramento River / Clear Creek", public_spk_search_text, fixed = TRUE),
            "The false scc phrase entered public SPK search metadata")
assert_true(!"resource_usace_usace_water_control_manuals_other" %in% registry_ids,
            "The held Water Control Manuals candidate entered canonical authority")
integrated_report <- registry[[match(integrated_report_resource_id, registry_ids)]]
assert_identical(sum(registry_ids == integrated_report_resource_id), 1L,
                 "The statewide Integrated Report parent is not unique")
assert_identical(integrated_report$title,
                 "California Integrated Reports & Impaired Waters",
                 "The evergreen Integrated Report title changed")
assert_identical(integrated_report$canonical_url, integrated_report_canonical_url,
                 "The evergreen Integrated Report canonical action changed")
assert_identical(integrated_report$access_points, integrated_report_access_points,
                 "The exact current Integrated Report access points changed")
assert_identical(unname(as.character(unlist(
  integrated_report$search_aliases, use.names = FALSE
))), integrated_report_search_aliases,
                 "The exact Integrated Report search aliases changed")
assert_identical(unname(as.character(unlist(
  integrated_report$subject_tags, use.names = FALSE
))),
                 c("Surface Water", "Water Quality", "Ecology & Habitat"),
                 "The Integrated Report retained an unsupported subject classification")
assert_identical(integrated_report$temporal_character, "mixed",
                 "The recurring Integrated Report temporal character changed")
assert_identical(unname(as.character(unlist(
  integrated_report$information_type_tags, use.names = FALSE
))), character(0),
                 "The Integrated Report gained an unsupported Information Type")
integrated_report_urls <- c(
  integrated_report$canonical_url,
  vapply(integrated_report$access_points, `[[`, character(1), "url"),
  vapply(integrated_report$public_source_references, `[[`, character(1), "url")
)
assert_true(!any(grepl("integrated2010.shtml", integrated_report_urls, fixed = TRUE)),
            "The obsolete 2010 Integrated Report action remains active")
assert_identical(length(unique(tolower(sub("/$", "", integrated_report_urls[1:4])))), 3L,
                 "The current Integrated Report canonical/access actions are duplicated")
integrated_report_owner_ids <- vapply(Filter(function(record) {
  text <- paste(c(
    record$title, record$canonical_url, record$search_aliases,
    vapply(record$access_points, `[[`, character(1), "label"),
    vapply(record$access_points, `[[`, character(1), "url")
  ), collapse = " ")
  grepl("water_quality_assessment|California Integrated Report|impaired waters",
        text, ignore.case = TRUE, perl = TRUE)
}, registry), `[[`, character(1), "id")
assert_identical(integrated_report_owner_ids, integrated_report_resource_id,
                 "A duplicate current statewide Integrated Report owner exists")
for (owner_id in names(r15b_authorized_access_points)) {
  record <- registry[[match(owner_id, registry_ids)]]
  expected <- r15b_authorized_access_points[[owner_id]]
  matches <- vapply(record$access_points, identical, logical(1), expected)
  assert_identical(sum(matches), 1L, paste(
    "The authorized subordinate access point is not present exactly once on", owner_id
  ))
  normalized_urls <- tolower(sub("/$", "", vapply(
    record$access_points, `[[`, character(1), "url"
  )))
  assert_identical(sum(normalized_urls == tolower(sub("/$", "", expected$url))), 1L,
                   paste("A normalized duplicate access point remains on", owner_id))
  assert_identical(record$canonical_url, unname(r15b_parent_canonical_urls[[owner_id]]),
                   paste("The canonical parent URL changed for", owner_id))
}
assert_identical(length(r15b_authorized_access_points), 4L,
                 "The exact R15B access-point addition count changed")
assert_identical(
  pt_guide_http_only_resource_url_exceptions(), r15b_http_only_exceptions,
  "The exact three-entry HTTP-only Resource exception contract changed"
)
http_url_fields <- unlist(lapply(registry, function(record) c(
  record$canonical_url,
  vapply(record$access_points, `[[`, character(1), "url"),
  vapply(record$public_source_references, `[[`, character(1), "url")
)), use.names = FALSE)
assert_identical(sum(grepl("^http://", http_url_fields)), 9L,
                 "The exact three HTTP-only actions must occupy nine URL-bearing fields")
unauthorized_http_canonical_ids <- registry_ids[vapply(registry, function(record) {
  grepl("^http://", record$canonical_url) &&
    !identical(record$canonical_url, unname(r15b_http_only_exceptions[record$id]))
}, logical(1))]
assert_identical(length(unauthorized_http_canonical_ids), 0L,
                 "An unauthorized HTTP canonical URL entered the registry")

duplicate_removal_ids <- c(
  "resource_aso_airborne_snow_observatories",
  "resource_dwr_casgem",
  "resource_dwr_water_data_library",
  "resource_noaa_cnrfc",
  "resource_noaa_cpc_forecasts_outlooks",
  "resource_noaa_wpc_qpf",
  "resource_nrcs_nwcc",
  "resource_nrcs_snow_survey_water_supply_forecasting",
  "resource_nrcs_web_soil_survey",
  "resource_usace_cwms_data_api",
  "resource_usbr",
  "resource_usgs_national_hydrography_products",
  "resource_usgs_streamstats",
  "resource_usgs_water_quality_portal"
)
assert_identical(length(duplicate_removal_ids), 14L,
                 "R7C duplicate access-point removal count changed")
assert_true(all(vapply(duplicate_removal_ids, function(id) {
  record <- wave2_registry[[match(id, expected_wave2_ids)]]
  record <- strip_r16b_existing_resource_access_points(record)
  length(record$access_points) == 1L &&
    identical(record$access_points[[1]]$role, "canonical") &&
    identical(record$access_points[[1]]$url, record$canonical_url)
}, logical(1))), "A deduplicated R7C access point was restored or changed")

goes <- registry[[match("resource_noaa_goes_image_viewer", registry_ids)]]
expected_goes_urls <- c(
  "https://www.star.nesdis.noaa.gov/GOES/",
  "https://www.star.nesdis.noaa.gov/GOES/sector.php?sat=G18&sector=psw",
  "https://www.star.nesdis.noaa.gov/GOES/sector.php?sat=G18&sector=wus",
  "https://www.star.nesdis.noaa.gov/GOES/sector_band.php?sat=G18&sector=psw&band=GEOCOLOR&length=24&dim=1",
  "https://www.star.nesdis.noaa.gov/GOES/sector_band.php?sat=G18&sector=psw&band=FireTemperature&length=12&dim=1"
)
expected_goes_labels <- c(
  "NOAA GOES Image Viewer",
  "GOES-West Pacific Southwest — all products",
  "GOES-West U.S. Pacific Coast — all products",
  "GOES-West Pacific Southwest GeoColor",
  "GOES-West Pacific Southwest Fire Temperature"
)
assert_identical(vapply(goes$access_points, `[[`, character(1), "url"),
                 expected_goes_urls, "GOES access-point URL order changed")
assert_identical(vapply(goes$access_points, `[[`, character(1), "label"),
                 expected_goes_labels, "GOES access-point label order changed")
assert_identical(vapply(goes$access_points, `[[`, character(1), "role"),
                 c("canonical", rep("configured_view", 4L)),
                 "GOES schema-v3 access-point roles changed")
assert_true(!any(grepl("sector=pnw", expected_goes_urls, fixed = TRUE)),
            "A Pacific Northwest GOES URL remains")
assert_identical(sum(grepl("/sector_band.php", expected_goes_urls, fixed = TRUE) &
                       grepl("sector=psw", expected_goes_urls, fixed = TRUE)), 2L,
                 "GOES must retain exactly two Pacific Southwest configured views")
assert_identical(goes$canonical_url, "https://www.star.nesdis.noaa.gov/GOES/",
                 "The canonical NOAA GOES Official Resource URL changed")

relationship_json <- jsonlite::toJSON(
  raw_relationship_registry, auto_unbox = TRUE, null = "null", na = "null",
  pretty = FALSE, digits = NA
)
assert_identical(raw_relationship_registry$schema_version, 2L,
                 "Product-Resource relationship schema version changed")
assert_identical(length(relationship_product_ids), 270L,
                 "Product-Resource relationship Product count changed")
assert_identical(
  digest::digest(file = relationship_registry_path, algo = "sha256"),
  "49239c8b3a264c834c24efaeddc115906651eb69158166210d9f1d54944ccda5",
  "The completed four-action R17B relationship file changed byte-for-byte"
)
assert_identical(length(raw_relationship_registry$resources), 211L,
                 "Product-Resource relationship Resource count changed")
assert_true(!anyDuplicated(relationship_product_ids),
            "Product-Resource relationship Product IDs are not unique")
strip_r17b_relationship_actions <- function(record) {
  if (record$product_id %in% r17b_newly_linked_product_ids) {
    record$coverage_review_state <- "not_yet_reviewed"
    record["coverage_disposition"] <- list(NULL)
    record$coverage_evidence_basis <- "not_yet_reviewed"
    record$coverage_evidence_refs <- list()
    record$resource_links <- list()
  } else if (record$product_id %in% r17b_existing_products_gaining_cnrfc) {
    record$resource_links <- Filter(function(link) {
      !identical(link$resource_id, r17b_cnrfc_resource_id)
    }, record$resource_links)
  }
  record
}
baseline_relationship_products <- lapply(
  raw_relationship_registry$products, strip_r17b_relationship_actions
)
baseline_d10_index <- match(
  "ops_cdec_reservoir_storage",
  vapply(baseline_relationship_products, `[[`, character(1), "product_id")
)
baseline_relationship_products[[baseline_d10_index]]$resource_links <- Filter(
  function(link) !identical(link$resource_id, r16b_spk_id),
  baseline_relationship_products[[baseline_d10_index]]$resource_links
)
baseline_relationship_registry <- list(
  schema_version = raw_relationship_registry$schema_version,
  products = baseline_relationship_products,
  resources = raw_relationship_registry$resources[seq_along(expected_baseline_resource_ids)]
)
assert_identical(
  digest::digest(compact_json(baseline_relationship_registry),
                 algo = "sha256", serialize = FALSE),
  "e9f4281c3587ab7fefcad8e2b44141cd4ac31635bc01199be2fde3043dc0ddd3",
  "The schema marker, 270 Products, corrected links, or accepted 72 Resource reviews changed"
)
assert_identical(
  digest::digest(compact_json(raw_relationship_registry$resources[73:211]),
                 algo = "sha256", serialize = FALSE),
  "f830ee50be605a74baf0218267304aeb2056afa29009e534f93a606f1076dd85",
  "The exact R16B corrected post-baseline relationship Resource authority changed"
)
assert_identical(
  digest::digest(paste0(paste(relationship_product_ids, collapse = "\n"), "\n"),
                 algo = "sha256", serialize = FALSE),
  "abfb270f371fc02bcd1daa4da3de035f49e461d0b458518d5b3a38b4b1aa2cd0",
  "The ordered Product relationship universe changed"
)

delivery_classes <- vapply(
  relationship_registry$products, `[[`, character(1), "delivery_class"
)
review_states <- vapply(
  relationship_registry$products, `[[`, character(1), "coverage_review_state"
)
coverage_dispositions <- vapply(relationship_registry$products, function(record) {
  if (is.null(record$coverage_disposition)) "not_yet_reviewed" else record$coverage_disposition
}, character(1))
canonical_links <- unlist(lapply(relationship_registry$products, function(record) {
  lapply(record$resource_links, function(link) c(list(product_id = record$product_id), link))
}), recursive = FALSE)
canonical_link_roles <- vapply(
  canonical_links, `[[`, character(1), "relationship_role"
)
assert_identical(unname(as.integer(table(factor(
  delivery_classes,
  levels = c("brim_managed", "brim_enhanced", "provider_hosted", "not_applicable")
)))), c(61L, 150L, 55L, 4L), "Delivery classification counts changed")
assert_identical(unname(as.integer(table(factor(
  review_states, levels = c("reviewed", "not_yet_reviewed")
)))), c(75L, 195L), "Coverage review-state counts changed")
assert_identical(unname(as.integer(table(factor(
  coverage_dispositions,
  levels = c(
    "direct_resource_match", "selected_product_from_broader_resource",
    "multiple_source_resources", "provenance_only_no_public_resource",
    "internal_no_external_resource", "not_yet_reviewed"
  )
)))), c(10L, 52L, 8L, 1L, 4L, 195L),
"Coverage-disposition counts changed")
assert_identical(length(canonical_links), 94L,
                 "Canonical Product-Resource link count changed")
assert_identical(unname(as.integer(table(factor(
  canonical_link_roles,
  levels = c("direct_match_in_brim", "selected_product_from_broader_resource",
             "source_reference")
)))), c(13L, 62L, 19L), "Canonical relationship-role counts changed")
assert_true(!anyDuplicated(vapply(canonical_links, function(link) {
  paste(link$product_id, link$resource_id, sep = "\r")
}, character(1))), "A duplicate Product-Resource pair entered canonical authority")
assert_identical(sum(vapply(relationship_registry$products, function(record) {
  length(record$resource_links) > 0L
}, logical(1))), 70L, "Products-with-Resource-links count changed")
assert_identical(length(unique(vapply(canonical_links, `[[`, character(1),
                                      "resource_id"))), 26L,
                 "Resources-with-Product-links count changed")
assert_true(!any(vapply(canonical_links, function(link) {
  link$resource_id %in% r15b_selected_replacement_ids
}, logical(1))), "An R15B replacement gained an unauthorized Product link")
d10_relationship <- relationship_registry$products[[match(
  "ops_cdec_reservoir_storage", relationship_product_ids
)]]
assert_identical(
  vapply(d10_relationship$resource_links, `[[`, character(1), "resource_id"),
  c("resource_dwr_cdec", r16b_spk_id, r17b_cnrfc_resource_id),
  "D10 must preserve CDEC/SPK and add only the exact CNRFC source Resource"
)
assert_identical(
  vapply(d10_relationship$resource_links, `[[`, character(1), "relationship_role"),
  c("selected_product_from_broader_resource", "source_reference", "source_reference"),
  "The exact D10 relationship roles changed"
)
r17b_relationship_fixture <- function(product_id) {
  record <- relationship_registry$products[[match(product_id, relationship_product_ids)]]
  list(
    review_state = record$coverage_review_state,
    disposition = record$coverage_disposition,
    evidence_basis = record$coverage_evidence_basis,
    resources = vapply(record$resource_links, `[[`, character(1), "resource_id"),
    roles = vapply(record$resource_links, `[[`, character(1), "relationship_role")
  )
}
assert_identical(r17b_relationship_fixture("cnrfc_stream"), list(
  review_state = "reviewed",
  disposition = "selected_product_from_broader_resource",
  evidence_basis = "reviewed_evidence",
  resources = r17b_cnrfc_resource_id,
  roles = "selected_product_from_broader_resource"
), "The exact CNRFC stream relationship fixture changed")
assert_identical(r17b_relationship_fixture("cnrfc_precip_weather_station_catalog"), list(
  review_state = "reviewed",
  disposition = "selected_product_from_broader_resource",
  evidence_basis = "reviewed_evidence",
  resources = r17b_cnrfc_resource_id,
  roles = "selected_product_from_broader_resource"
), "The exact CNRFC weather-station relationship fixture changed")
assert_identical(r17b_relationship_fixture("ops_major_water_supply_forecasts"), list(
  review_state = "reviewed",
  disposition = "selected_product_from_broader_resource",
  evidence_basis = "reviewed_evidence",
  resources = c(
    "resource_noaa_nwps", "resource_usgs_national_hydrography_products",
    r17b_cnrfc_resource_id
  ),
  roles = rep("source_reference", 3L)
), "The exact major water-supply CNRFC source fixture changed")
cnrfc_related_product_ids <- vapply(Filter(function(link) {
  identical(link$resource_id, r17b_cnrfc_resource_id)
}, canonical_links), `[[`, character(1), "product_id")
assert_identical(cnrfc_related_product_ids, c(
  "cnrfc_fnf_delta", "cnrfc_stream", "cnrfc_precip_weather_station_catalog",
  "cnrfc_basin_product_availability", "ops_cnrfc_forecast_points",
  "ops_major_water_supply_forecasts", "ops_cdec_reservoir_storage"
), "The CNRFC Resource must have exactly seven related Products")
drought_relationship <- relationship_registry$products[[match(
  "ops_us_drought_monitor", relationship_product_ids
)]]
assert_identical(
  list(
    review_state = drought_relationship$coverage_review_state,
    disposition = drought_relationship$coverage_disposition,
    evidence_basis = drought_relationship$coverage_evidence_basis,
    resources = vapply(drought_relationship$resource_links, `[[`, character(1), "resource_id"),
    roles = vapply(drought_relationship$resource_links, `[[`, character(1), "relationship_role")
  ),
  list(
    review_state = "reviewed",
    disposition = "selected_product_from_broader_resource",
    evidence_basis = "maintainer_clarification",
    resources = "resource_climate_and_drought_data_providers_drought_gov_california_dashboard",
    roles = "selected_product_from_broader_resource"
  ),
  "The exact Drought.gov California relationship fixture changed"
)
cocorahs_product_ids <- c("product-ops-cocorahs-ca-daily", "ops_cocorahs_conus_daily")
assert_true(all(vapply(cocorahs_product_ids, function(product_id) {
  record <- relationship_registry$products[[match(product_id, relationship_product_ids)]]
  identical(record$coverage_review_state, "reviewed") &&
    identical(record$coverage_disposition, "selected_product_from_broader_resource") &&
    identical(record$coverage_evidence_basis, "maintainer_clarification") &&
    identical(vapply(record$resource_links, `[[`, character(1), "resource_id"),
              "resource_cocorahs_cocorahs_other") &&
    identical(vapply(record$resource_links, `[[`, character(1), "relationship_role"),
              "selected_product_from_broader_resource")
}, logical(1))), "The exact two-Product CoCoRaHS relationship fixture changed")
california_water_watch_removed_product_ids <- c("brim_mapped_conveyance", "ops_delta_snapshot")
assert_true(all(vapply(california_water_watch_removed_product_ids, function(product_id) {
  record <- relationship_registry$products[[match(product_id, relationship_product_ids)]]
  identical(record$coverage_review_state, "not_yet_reviewed") &&
    is.null(record$coverage_disposition) &&
    identical(record$coverage_evidence_basis, "not_yet_reviewed") &&
    !length(record$coverage_evidence_refs) && !length(record$resource_links)
}, logical(1))), "The unsupported California Water Watch relationship fixture changed")
usbr_product_links <- Filter(function(link) identical(link$resource_id, "resource_usbr"),
                             canonical_links)
assert_identical(vapply(usbr_product_links, `[[`, character(1), "product_id"),
                 c("CVPIA_WETLAND_HABITAT_REFUGES", "CVPIA_REFUGE_DELIVERY_POINTS"),
                 "The exact two existing USBR Product links changed")
integrated_report_products <- relationship_registry$products[match(
  integrated_report_product_ids, relationship_product_ids
)]
assert_true(all(vapply(integrated_report_products, function(record) {
  identical(record$coverage_review_state, "reviewed") &&
    identical(record$coverage_disposition, "selected_product_from_broader_resource") &&
    identical(record$coverage_evidence_basis, "reviewed_evidence") &&
    identical(vapply(record$resource_links, `[[`, character(1), "resource_id"),
              integrated_report_resource_id) &&
    identical(vapply(record$resource_links, `[[`, character(1), "relationship_role"),
              "selected_product_from_broader_resource")
}, logical(1))), "The exact two 2024 Integrated Report Product links changed")
external_catalog <- read.csv(
  file.path("00_config", "external_service_catalog.csv"),
  stringsAsFactors = FALSE, check.names = FALSE
)
integrated_report_source_rows <- external_catalog[
  external_catalog$external_layer_id %in% integrated_report_product_ids,
  , drop = FALSE
]
assert_identical(integrated_report_source_rows$external_layer_id,
                 integrated_report_product_ids,
                 "The tracked Integrated Report Product source rows changed")
assert_identical(integrated_report_source_rows$display_name, c(
  "2024 Integrated Report | SWRCB lines",
  "2024 Integrated Report | SWRCB polygons"
), "The exact Integrated Report Product titles changed")
assert_identical(integrated_report_source_rows$external_group,
                 rep("GW / SW Data Products", 2L),
                 "The Integrated Report Product subsystem source changed")
assert_identical(integrated_report_source_rows$service_type, rep("feature", 2L),
                 "The Integrated Report Products are no longer feature geometry")
assert_identical(integrated_report_source_rows$service_url, c(
  paste0(
    "https://gispublic.waterboards.ca.gov/portalserver/rest/services/Hosted/",
    "Draft_2024_Integrated_Report_Lines/FeatureServer/1"
  ),
  paste0(
    "https://gispublic.waterboards.ca.gov/portalserver/rest/services/Hosted/",
    "2024_Integrated_Report_Polygons/FeatureServer/0"
  )
), "The exact official Integrated Report service actions changed")
assert_identical(integrated_report_source_rows$source_page,
                 rep(integrated_report_access_points[[2]]$url, 2L),
                 "The Integrated Report Products lost their exact official cycle page")
assert_true(!any(grepl("2026.*(INTEGRATED|_IR_)|(INTEGRATED|_IR_).*2026",
                       relationship_product_ids, ignore.case = TRUE, perl = TRUE)),
            "A 2026 Integrated Report Product was inferred without tracked authority")
assert_true(all(vapply(r16b_recall_only_product_ids, function(product_id) {
  links <- relationship_registry$products[[match(
    product_id, relationship_product_ids
  )]]$resource_links
  !any(vapply(links, function(link) link$resource_id %in% r16b_added_resource_ids,
              logical(1)))
}, logical(1))), "A recall-only Product candidate gained an inferred R16B Resource link")

swrcb_relationship <- relationship_registry$products[[match(
  "swrcb_wr_list_official", relationship_product_ids
)]]
assert_identical(swrcb_relationship$delivery_class, "brim_managed",
                 "The reviewed SWRCB delivery decision changed")
assert_identical(swrcb_relationship$coverage_review_state, "reviewed",
                 "The reviewed SWRCB coverage state changed")
assert_identical(swrcb_relationship$coverage_disposition,
                 "provenance_only_no_public_resource",
                 "The reviewed SWRCB provenance-only decision changed")
assert_identical(length(swrcb_relationship$resource_links), 0L,
                 "The SWRCB provenance-only decision gained a fake Resource")

relationship_resources <- relationship_registry$resources
relationship_resource_ids <- vapply(
  relationship_resources, `[[`, character(1), "resource_id"
)
assert_identical(relationship_resource_ids, registry_ids,
                 "Relationship Resource universe/order changed")
published_relationship_resources <- relationship_resources[
  match(published_ids, relationship_resource_ids)
]
published_map_states <- vapply(
  published_relationship_resources, `[[`, character(1), "map_review_state"
)
published_representations <- vapply(
  published_relationship_resources, `[[`, character(1), "map_representation"
)
assert_true(all(published_map_states == "reviewed"),
            "Every published Resource must have a reviewed representation")
assert_identical(unname(as.integer(table(factor(
  published_representations,
  levels = c("direct_match_in_brim", "selected_products_in_brim",
             "not_currently_mapped_in_brim")
)))), c(3L, 23L, 180L), "Published Resource representation counts changed")
staged_relationship_resources <- relationship_resources[
  match(expected_held_staged_ids, relationship_resource_ids)
]
assert_true(all(vapply(staged_relationship_resources, function(record) {
  identical(record$map_review_state, "not_yet_reviewed") &&
    is.null(record$map_representation) && !length(record$evidence_refs)
}, logical(1))), "Staged Resources must retain explicit deferred review records")
r15c_target_relationship_resources <- relationship_resources[
  match(r15c_target_registry_ids, relationship_resource_ids)
]
assert_true(all(vapply(r15c_target_relationship_resources, function(record) {
  selected_post_baseline_ids <- c(
    r16b_spk_id,
    "resource_cocorahs_cocorahs_other",
    "resource_climate_and_drought_data_providers_drought_gov_california_dashboard",
    integrated_report_resource_id
  )
  expected_representation <- if (record$resource_id %in% selected_post_baseline_ids) {
    "selected_products_in_brim"
  } else {
    "not_currently_mapped_in_brim"
  }
  identical(record$map_review_state, "reviewed") &&
    identical(record$map_representation, expected_representation) &&
    length(record$evidence_refs) > 0L
}, logical(1))), "Every post-baseline Resource must retain its exact reviewed representation")
selected_replacement_relationships <- relationship_resources[
  match(r15b_selected_replacement_ids, relationship_resource_ids)
]
assert_true(all(vapply(selected_replacement_relationships, function(record) {
  identical(record$map_review_state, "reviewed") &&
    identical(record$map_representation, "not_currently_mapped_in_brim") &&
    length(record$evidence_refs) > 0L
}, logical(1))), "An R15B replacement relationship record is not reviewed and not-mapped")
all_resource_representations <- vapply(relationship_resources, function(record) {
  if (is.null(record$map_representation)) "not_yet_reviewed" else record$map_representation
}, character(1))
assert_identical(unname(as.integer(table(factor(
  all_resource_representations,
  levels = c("direct_match_in_brim", "selected_products_in_brim",
             "not_currently_mapped_in_brim", "not_yet_reviewed")
)))), c(3L, 23L, 180L, 5L),
"Full Resource representation counts changed")

direct_resource_ids <- c(
  "resource_calfire_fire_perimeters", "resource_nifc_wfigs_current",
  "resource_nrcs_scan"
)
selected_resource_ids <- c(
  "resource_blm_california", "resource_prism_normals", "resource_usgs_bcmv8",
  "resource_dwr_bulletin118_sgma_2019", "resource_usgs_water_dashboard",
  "resource_noaa_nwps", "resource_noaa_goes_image_viewer",
  "resource_dwr_california_groundwater_live",
  "resource_dwr_casgem", "resource_dwr_cdec",
  "resource_dwr_groundwater_sustainability_agencies",
  "resource_nasa_firms_global_fire_map", "resource_noaa_cnrfc",
  "resource_noaa_cpc_forecasts_outlooks", "resource_noaa_wpc_qpf",
  "resource_nrcs_snow_survey_water_supply_forecasting", "resource_usbr",
  "resource_usgs_national_hydrography_products", "resource_usgs_water_data_nation",
  r16b_spk_id, "resource_cocorahs_cocorahs_other",
  integrated_report_resource_id,
  "resource_climate_and_drought_data_providers_drought_gov_california_dashboard"
)
representation_ids <- function(value) vapply(Filter(function(record) {
  identical(record$map_representation, value)
}, published_relationship_resources), `[[`, character(1), "resource_id")
assert_identical(representation_ids("direct_match_in_brim"), direct_resource_ids,
                 "Exact direct-match Resource membership changed")
assert_identical(representation_ids("selected_products_in_brim"), selected_resource_ids,
                 "Exact selected-products Resource membership changed")

enrichment_raw <- jsonlite::fromJSON(
  file.path("00_config", "guide_product_enrichment.json"), simplifyVector = FALSE
)
assert_true(all(!vapply(enrichment_raw$products, function(record) {
  "resource_relationships" %in% names(record)
}, logical(1))), "Old relationship authority remains in Product enrichment")
enrichment_without_precipitation_micro_pass <- enrichment_raw
ero_index <- match("ops_wpc_ero_day_1", vapply(
  enrichment_without_precipitation_micro_pass$products, `[[`, character(1), "stable_id"
))
enrichment_without_precipitation_micro_pass$products[[ero_index]]$subject_tags <- Filter(
  function(value) !identical(value, "Precipitation"),
  enrichment_without_precipitation_micro_pass$products[[ero_index]]$subject_tags
)
r17b_cnrfc_detail_ids <- c(
  "cnrfc_basin_product_availability", "cnrfc_fnf_delta",
  "cnrfc_precip_weather_station_catalog", "cnrfc_stream"
)
r17b_cnrfc_detail_records <- Filter(function(record) {
  record$stable_id %in% r17b_cnrfc_detail_ids
}, enrichment_raw$products)
cnrfc_fnf_enrichment_index <- match("cnrfc_fnf_delta", vapply(
  enrichment_raw$products, `[[`, character(1), "stable_id"
))
cnrfc_fnf_enrichment <- enrichment_raw$products[[cnrfc_fnf_enrichment_index]]
assert_identical(cnrfc_fnf_enrichment$processing, list(r17b_cnrfc_fnf_processing_after),
                 "The exact four-sentence CNRFC FNF processing entry changed")
assert_identical(sum(vapply(
  cnrfc_fnf_enrichment$geometry_limitations, identical, logical(1),
  r17b_cnrfc_fnf_boundary_limitation
)), 1L, "The exact CNRFC FNF boundary limitation is absent or duplicated")
assert_true(!grepl(r17b_cnrfc_fnf_processing_before,
                   compact_json(enrichment_raw), fixed = TRUE),
            "The superseded CNRFC FNF processing sentence remains in authority")
reconstructed_pre_copy_enrichment <- enrichment_raw
reconstructed_pre_copy_enrichment$products[[cnrfc_fnf_enrichment_index]]$processing <-
  list(r17b_cnrfc_fnf_processing_before)
reconstructed_pre_copy_enrichment$products[[cnrfc_fnf_enrichment_index]]$geometry_limitations <-
  Filter(function(value) !identical(value, r17b_cnrfc_fnf_boundary_limitation),
         reconstructed_pre_copy_enrichment$products[[cnrfc_fnf_enrichment_index]]$geometry_limitations)
assert_identical(
  digest::digest(compact_json(reconstructed_pre_copy_enrichment),
                 algo = "sha256", serialize = FALSE),
  "c05f103c2b1eda0f55bcc28f912d814235dfe335e86f5644e41178fc99a2f4c7",
  "Product enrichment changed beyond cnrfc_fnf_delta processing and geometry_limitations"
)
assert_identical(vapply(r17b_cnrfc_detail_records, `[[`, character(1), "stable_id"),
                 r17b_cnrfc_detail_ids,
                 "The exact R17B CNRFC Product-detail enrichment set changed")
assert_true(length(r17b_cnrfc_detail_records) <= 5L &&
              all(vapply(r17b_cnrfc_detail_records, function(record) {
                identical(record$editorial_state, "SOURCE_BACKED_RICH") &&
                  identical(unname(as.character(unlist(
                    record$information_type_tags, use.names = FALSE
                  ))), "Static Reference") &&
                  nzchar(record$summary) &&
                  all(c("capabilities", "timing", "processing",
                        "geometry_limitations", "method_ids", "source_refs") %in%
                        names(record)) &&
                  all(vapply(record[c(
                    "capabilities", "timing", "processing", "geometry_limitations",
                    "method_ids", "source_refs"
                  )], function(values) {
                    values <- unname(as.character(unlist(values, use.names = FALSE)))
                    length(values) > 0L && all(nzchar(values))
                  }, logical(1)))
              }, logical(1))),
            "R17B CNRFC Product detail is incomplete, unsupported, or above its ceiling")
enrichment_without_precipitation_micro_pass$products <- Filter(function(record) {
  !record$stable_id %in% r17b_cnrfc_detail_ids
}, enrichment_without_precipitation_micro_pass$products)
enrichment_json <- jsonlite::toJSON(
  enrichment_without_precipitation_micro_pass,
  auto_unbox = TRUE, null = "null", na = "null",
  pretty = FALSE, digits = NA
)
assert_identical(
  digest::digest(enrichment_json, algo = "sha256", serialize = FALSE),
  "e83984f53fc4db9fb1c3916c3c871b19ccf78c1f9404db3c2a5e4520868ce500",
  "Product enrichment changed outside the approved Precipitation subject and R17B CNRFC detail additions"
)

product_fixtures <- lapply(relationship_product_ids, function(product_id) {
  list(
    id = product_id,
    title = paste("Eligible Product", product_id),
    entityType = "Layer"
  )
})
browser_records <- pt_guide_resource_browser_records(
  published, product_fixtures, relationship_registry
)
projected_cnrfc_resource <- browser_records[[match(
  r17b_cnrfc_resource_id, expected_published_ids
)]]
assert_identical(projected_cnrfc_resource$summary, r17b_cnrfc_resource_summary_after,
                 "The projected CNRFC Resource summary changed")
assert_identical(projected_cnrfc_resource$canonicalUrl,
                 r17b_cnrfc_canonical_url_after,
                 "The projected CNRFC official action changed")
assert_identical(projected_cnrfc_resource$accessPoints[[1]], list(
  role = "canonical", label = "Official Resource",
  url = r17b_cnrfc_canonical_url_after
), "The rendered CNRFC Official Resource action changed")
assert_identical(length(unique(vapply(
  projected_cnrfc_resource$accessPoints, function(point) {
    sub("/+$", "", tolower(point$url))
  }, character(1)
))), length(projected_cnrfc_resource$accessPoints),
"The projected CNRFC access actions contain a normalized duplicate")
assert_identical(vapply(browser_records, `[[`, character(1), "id"), expected_published_ids,
                 "Browser projection changed Resource order or identity")
assert_true(all(vapply(browser_records, function(record) {
  identical(names(record), browser_fields)
}, logical(1))), "Resource browser projection is not the exact 23-field shape")
assert_true(all(vapply(seq_along(browser_records), function(index) {
  record <- browser_records[[index]]
  canonical <- registry[[match(record$id, registry_ids)]]
  identical(
    record$resourceTypeLabel,
    unname(expected_metadata_vocabularies$resource_type[[record$resourceType]])
  ) &&
    identical(record$temporalCharacter, canonical$temporal_character) &&
    identical(
      record$temporalCharacterLabel,
      unname(expected_metadata_vocabularies$temporal_character[[
        record$temporalCharacter
      ]])
    ) &&
    identical(
      record$geographicScope$scopeLabel,
      unname(expected_metadata_vocabularies$geographic_scope_class[[
        record$geographicScope$scopeType
      ]])
    )
}, logical(1))), "Build-derived controlled Resource metadata labels changed")
projected_relationships <- unlist(lapply(browser_records, `[[`, "representedProducts"),
                                  recursive = FALSE)
projected_roles <- vapply(projected_relationships, `[[`, character(1), "relationshipRole")
assert_identical(length(projected_relationships), 94L,
                 "Browser reverse relationship index lost exact rows")
assert_identical(unname(as.integer(table(factor(
  projected_roles,
  levels = c("direct_match_in_brim", "selected_product_from_broader_resource",
             "source_reference")
)))), c(13L, 62L, 19L), "Projected relationship-role counts changed")
assert_true(all(vapply(projected_relationships, function(relationship) {
  identical(names(relationship), c(
    "productId", "title", "deliveryClass", "coverageDisposition",
    "relationshipRole", "sourceResourceIds"
  ))
}, logical(1))), "Represented Product public shape changed")
assert_identical(
  vapply(Filter(function(record) {
    identical(record$mapRepresentation, "direct_match_in_brim")
  }, browser_records), `[[`, character(1), "id"),
  direct_resource_ids, "Projected direct-match Resource membership changed"
)
assert_identical(sum(vapply(browser_records, function(record) {
  identical(record$mapRepresentation, "selected_products_in_brim")
}, logical(1))), 23L, "Projected selected-products Resource count changed")
assert_identical(sum(vapply(browser_records, function(record) {
  identical(record$mapRepresentation, "not_currently_mapped_in_brim")
}, logical(1))), 180L, "Projected not-mapped Resource count changed")

assert_true(all(vapply(browser_records, function(record) {
  identical(record$accessPoints[[1]], list(
    role = "canonical", label = "Official Resource", url = record$canonicalUrl
  )) &&
    identical(record$aliases,
              unname(as.character(unlist(registry[[match(record$id, registry_ids)]]$search_aliases)))) &&
    identical(record$providers,
              lapply(registry[[match(record$id, registry_ids)]]$providers,
                     function(provider) list(name = provider$name, role = provider$role)))
}, logical(1))), "Canonical action, human aliases, or provider projection changed")
projected_goes <- browser_records[[match("resource_noaa_goes_image_viewer", expected_published_ids)]]
assert_identical(vapply(projected_goes$accessPoints, `[[`, character(1), "url"),
                 expected_goes_urls, "GOES access points changed in browser projection")
projected_usbr <- browser_records[[match(r15b_usbr_resource_id, expected_published_ids)]]
assert_identical(projected_usbr$canonicalUrl, r15b_usbr_canonical_url_after,
                 "The projected USBR canonical action changed")
assert_identical(projected_usbr$accessPoints[[1]]$url, r15b_usbr_canonical_url_after,
                 "The rendered USBR Official Resource href changed")
projected_cdec <- browser_records[[match("resource_dwr_cdec", expected_published_ids)]]
assert_identical(
  projected_cdec$accessPoints[[2]], r15b_authorized_access_points$resource_dwr_cdec,
  "The rendered CDEC Reservoir Conditions access point changed"
)

assert_identical(pt_guide_normalize_resource_search("Weather—Water / Forecast"),
                 "weather water forecast",
                 "Resource search normalization is not deterministic punctuation-to-space ASCII")
browser_json <- jsonlite::toJSON(
  browser_records, auto_unbox = TRUE, null = "null", na = "null",
  pretty = FALSE, digits = NA
)
assert_identical(length(browser_records), 206L,
                 "Browser projection must contain exactly 206 published Resources")
assert_true(!any(vapply(staged_ids, function(id) {
  grepl(id, browser_json, fixed = TRUE)
}, logical(1))), "A staged Resource ID entered the browser projection")
assert_identical(
  digest::digest(browser_json, algo = "sha256", serialize = FALSE),
  "6f21971ed818771f02a45be0ad6e70834702869690e1d140dda68e8f448a1686",
  "The exact R17B relationship-enriched 206-Resource browser payload changed"
)
forbidden_fields <- c(
  "migration_aliases", "publication_state", "public_source_references",
  "canonical_url", "access_class", "source_refs", "editorial_state",
  "verification", "freshness", "lifecycle", "relatedProductIds",
  "relatedProducts", "relationshipFlags", "brimLinked", "beyondBrim",
  "displayedInBrim", "usedByBrim", "relatedExternalResource"
)
assert_true(!any(vapply(forbidden_fields, function(field) {
  grepl(paste0('"', field, '"'), browser_json, fixed = TRUE)
}, logical(1))), "A prohibited registry/editorial/runtime field entered browser Resources")
assert_true(!any(vapply(migration_aliases, function(alias) {
  grepl(alias, browser_json, fixed = TRUE)
}, logical(1))), "A migration alias leaked into browser Resources")
assert_true(all(vapply(browser_records, function(record) {
  !grepl("https://", record$searchText, fixed = TRUE) &&
    !grepl(record$id, record$searchText, fixed = TRUE)
}, logical(1))), "Resource search text contains a URL or stable ID")

helper_source <- paste(readLines(
  file.path("03_functions", "leaflet_guide_helpers.r"), warn = FALSE
), collapse = "\n")
assert_true(!grepl("pt_guide_related_resources", helper_source, fixed = TRUE),
            "Provider/title Resource relationship heuristic remains")
assert_true(!grepl("pt_guide_resource_product_relationships", helper_source, fixed = TRUE),
            "Legacy untyped Resource reverse map remains")
assert_true(!grepl("related_resource_ids =", helper_source, fixed = TRUE),
            "Product constructors still accept a parallel Resource relationship authority")
for (approved_url in unname(r15b_http_only_exceptions)) {
  assert_identical(
    lengths(regmatches(
      helper_source,
      gregexpr(approved_url, helper_source, fixed = TRUE)
    )),
    1L,
    paste("An HTTP-only exception URL has more than one helper authority:", approved_url)
  )
}

write_registry_fixture <- function(value) {
  path <- tempfile("guide_resource_registry_", tmpdir = tempdir(), fileext = ".json")
  jsonlite::write_json(
    value, path, auto_unbox = TRUE, null = "null", na = "null",
    pretty = TRUE, digits = NA
  )
  path
}
expect_invalid <- function(value, pattern, message) {
  assert_error(pt_guide_read_resource_registry(write_registry_fixture(value)), pattern, message)
}
fresh_registry <- function() jsonlite::fromJSON(registry_path, simplifyVector = FALSE)
registry_with_canonical_url <- function(resource_id, url) {
  fixture <- fresh_registry()
  fixture_ids <- vapply(fixture$resources, `[[`, character(1), "id")
  resource_index <- match(resource_id, fixture_ids)
  assert_true(!is.na(resource_index), paste("Missing URL-policy fixture Resource:", resource_id))
  canonical_index <- match(
    "canonical",
    vapply(fixture$resources[[resource_index]]$access_points, `[[`, character(1), "role")
  )
  assert_true(!is.na(canonical_index), paste("Missing canonical access point for:", resource_id))
  fixture$resources[[resource_index]]$canonical_url <- url
  fixture$resources[[resource_index]]$access_points[[canonical_index]]$url <- url
  fixture
}

assert_identical(length(r15b_http_only_exceptions), 3L,
                 "The HTTP-only exception map must contain exactly three entries")
for (resource_id in names(r15b_http_only_exceptions)) {
  approved_url <- unname(r15b_http_only_exceptions[[resource_id]])
  canonical_record <- registry[[match(resource_id, registry_ids)]]
  assert_identical(
    canonical_record$canonical_url,
    approved_url,
    paste("The registry loader did not retain an exact HTTP-only Resource pair:", resource_id)
  )
  assert_identical(
    pt_guide_validate_public_resource_url(
      approved_url, "Approved HTTP-only fixture", resource_id
    ),
    approved_url,
    paste("An exact approved HTTP-only Resource pair failed:", resource_id)
  )
}
assert_error(
  pt_guide_validate_public_resource_url(
    "http://wiskiweb.tid.org/different.htm", "Wrong URL fixture",
    "resource_tid_turlock_irrigation_district_wiski_web_platform"
  ),
  "exact reviewed HTTP-only Resource exception",
  "An approved Resource ID accepted a different HTTP path"
)
assert_error(
  pt_guide_validate_public_resource_url(
    "http://wiskiweb.tid.org/index.htm", "Wrong ID fixture", "resource_doi"
  ),
  "exact reviewed HTTP-only Resource exception",
  "An unapproved Resource ID accepted an approved HTTP URL"
)
assert_error(
  pt_guide_validate_public_resource_url(
    "http://example.gov/resource", "Fourth HTTP fixture", "resource_doi"
  ),
  "exact reviewed HTTP-only Resource exception",
  "An arbitrary fourth HTTP canonical URL was accepted"
)
assert_error(
  pt_guide_validate_public_resource_url(
    "http://www.kingsriverwater.org/", "Host wildcard fixture",
    "resource_krwa_kings_river_water_association_platform"
  ),
  "exact reviewed HTTP-only Resource exception",
  "A host-only or wildcard HTTP exception was accepted"
)
assert_identical(
  pt_guide_validate_public_resource_url(
    "https://example.gov/resource", "Ordinary HTTPS fixture", "resource_doi"
  ),
  "https://example.gov/resource",
  "An ordinary valid public HTTPS URL no longer passes"
)

registry_url_rejections <- list(
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
for (fixture_name in names(registry_url_rejections)) {
  probe <- registry_url_rejections[[fixture_name]]
  expect_invalid(
    registry_with_canonical_url(names(probe), unname(probe)),
    "exact reviewed HTTP-only Resource exception",
    paste("The registry loader accepted a prohibited public URL fixture:", fixture_name)
  )
}
valid_https_registry <- pt_guide_read_resource_registry(write_registry_fixture(
  registry_with_canonical_url("resource_doi", "https://example.gov/resource")
))
assert_identical(
  valid_https_registry[[match("resource_doi", vapply(
    valid_https_registry, `[[`, character(1), "id"
  ))]]$canonical_url,
  "https://example.gov/resource",
  "The registry loader rejected an ordinary valid public HTTPS URL"
)

bad <- fresh_registry()
bad$schema_version <- 1L
expect_invalid(bad, "schema_version 3", "Unsupported schema version was accepted")
bad <- fresh_registry()
bad$resources[[1]]$summary <- NULL
expect_invalid(bad, "missing required field", "Missing required field was accepted")
bad <- fresh_registry()
bad$resources[[1]]$temporal_character <- NULL
expect_invalid(bad, "missing required field", "Missing temporal character was accepted")
bad <- fresh_registry()
bad$resources[[1]]$unexpected_field <- "unexpected"
expect_invalid(bad, "unsupported field", "Unknown Resource field was accepted")
bad <- fresh_registry()
bad$resources[[1]]$resource_type <- "interactive_map"
expect_invalid(bad, "uncontrolled Resource type, temporal character, or granularity",
               "A prototype Resource Type was accepted")
bad <- fresh_registry()
bad$resources[[1]]$temporal_character <- "near_real_time"
expect_invalid(bad, "uncontrolled Resource type, temporal character, or granularity",
               "An uncontrolled temporal character was accepted")
bad <- fresh_registry()
bad$resources[[1]]$geographic_scope$scope_type <- "other"
expect_invalid(bad, "uncontrolled or inconsistent geographic scope",
               "The prohibited other geographic scope was accepted")
bad <- fresh_registry()
bad$resources[[1]]$geographic_scope$names <- list("United States", "united states")
expect_invalid(bad, "uncontrolled or inconsistent geographic scope",
               "Case-insensitive duplicate named geographies were accepted")
bad <- fresh_registry()
bad$resources[[2]]$id <- bad$resources[[1]]$id
expect_invalid(bad, "globally unique", "Duplicate stable ID was accepted")
bad <- fresh_registry()
bad$resources[[1]]$publication_state <- "staged"
validated_staged <- pt_guide_read_resource_registry(write_registry_fixture(bad))
assert_identical(length(pt_guide_resource_published_records(validated_staged)), 205L,
                 "Publication projection did not exclude a staged negative fixture")
bad <- fresh_registry()
bad$resources[[1]]$canonical_url <- "https://localhost/private"
bad$resources[[1]]$access_points[[1]]$url <- "https://localhost/private"
expect_invalid(bad, "local, private, or restricted host",
               "Private URL was accepted")
bad <- fresh_registry()
bad$resources[[1]]$access_points[[1]]$url <- "http://example.gov/access"
expect_invalid(bad, "exact reviewed HTTP-only Resource exception",
               "An arbitrary HTTP access-point URL was accepted")
bad <- fresh_registry()
bad_fixture_ids <- vapply(bad$resources, `[[`, character(1), "id")
bad_usbr_index <- match(r15b_usbr_resource_id, bad_fixture_ids)
bad$resources[[bad_usbr_index]]$public_source_references[[1]]$url <-
  "http://example.gov/source"
expect_invalid(bad, "exact reviewed HTTP-only Resource exception",
               "An arbitrary HTTP public-source reference was accepted")
bad <- fresh_registry()
bad$resources[[1]]$summary <- paste0("Local file /", "Users/example/resource.csv")
expect_invalid(bad, "machine-local filesystem path",
               "Machine-local path was accepted")
bad <- fresh_registry()
bad$resources[[1]]$relatedProductIds <- "huc8"
expect_invalid(bad, "prohibited authority/provenance field",
               "Product relationship authority entered the Resource registry")

assert_error(
  pt_guide_resource_browser_records(
    unclass(published), product_fixtures, relationship_registry
  ),
  "requires a validated registry",
  "Browser projection accepted an unvalidated registry"
)
duplicate_products <- product_fixtures
duplicate_products[[2]]$id <- duplicate_products[[1]]$id
assert_error(
  pt_guide_resource_browser_records(
    published, duplicate_products, relationship_registry
  ),
  "unique eligible Product IDs",
  "Browser projection accepted duplicate eligible Product IDs"
)

all_relationship_evidence_refs <- unique(c(
  unlist(lapply(relationship_registry$products, `[[`, "delivery_evidence_refs"), use.names = FALSE),
  unlist(lapply(relationship_registry$products, `[[`, "coverage_evidence_refs"), use.names = FALSE),
  unlist(lapply(canonical_links, `[[`, "evidence_refs"), use.names = FALSE),
  unlist(lapply(relationship_registry$resources, `[[`, "evidence_refs"), use.names = FALSE)
))
tracked_paths <- pt_guide_relationship_registry_tracked_paths(".")
if (is.null(tracked_paths)) {
  assert_true(all(file.exists(all_relationship_evidence_refs)),
              "A synced relationship evidence reference is unavailable")
} else {
  assert_true(all(all_relationship_evidence_refs %in% tracked_paths),
              "A relationship evidence reference is not a tracked repository path")
}

clone_value <- function(value) jsonlite::fromJSON(
  jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null", digits = NA),
  simplifyVector = FALSE
)
validate_relationship_fixture <- function(value, product_ids) {
  pt_guide_validate_product_resource_relationship_registry(
    value, product_ids, registry,
    repository_root = ".", tracked_paths = tracked_paths
  )
}
fixture_evidence <- "qa/test_guide_resource_registry.R"
fixture_link <- function(resource_id, role = "selected_product_from_broader_resource") {
  list(
    resource_id = resource_id,
    relationship_role = role,
    evidence_refs = list(fixture_evidence)
  )
}
fixture_product <- function(
    product_id, delivery_class, disposition = "selected_product_from_broader_resource",
    links = list(), review_state = "reviewed") {
  not_reviewed <- identical(review_state, "not_yet_reviewed")
  list(
    product_id = product_id,
    delivery_class = delivery_class,
    delivery_evidence_refs = list(fixture_evidence),
    coverage_review_state = review_state,
    coverage_disposition = if (not_reviewed) NULL else disposition,
    coverage_evidence_basis = if (not_reviewed) "not_yet_reviewed" else "reviewed_evidence",
    coverage_evidence_refs = if (not_reviewed) list() else list(fixture_evidence),
    resource_links = links
  )
}
timber_fixture <- list(
  schema_version = 2L,
  products = list(
    fixture_product(
      "fixture_timber_canopy", "brim_managed",
      links = list(fixture_link("resource_blm_california"))
    ),
    fixture_product(
      "fixture_timber_harvest", "provider_hosted",
      links = list(fixture_link("resource_blm_california"))
    ),
    fixture_product(
      "fixture_timber_composite", "brim_enhanced",
      disposition = "multiple_source_resources",
      links = list(
        fixture_link("resource_blm_california"),
        fixture_link("resource_doi", "source_reference")
      )
    ),
    fixture_product(
      "fixture_timber_pending", "brim_managed",
      review_state = "not_yet_reviewed"
    ),
    fixture_product(
      "fixture_timber_provenance", "provider_hosted",
      disposition = "provenance_only_no_public_resource"
    )
  ),
  resources = lapply(registry_ids, function(resource_id) list(
    resource_id = resource_id,
    map_review_state = "reviewed",
    map_representation = if (resource_id %in% c(
      "resource_blm_california", "resource_doi"
    )) "selected_products_in_brim" else "not_currently_mapped_in_brim",
    evidence_refs = list(fixture_evidence)
  ))
)
timber_ids <- vapply(timber_fixture$products, `[[`, character(1), "product_id")
validated_timber <- validate_relationship_fixture(timber_fixture, timber_ids)
assert_identical(length(validated_timber$products), 5L,
                 "Synthetic timber-family onboarding fixture did not validate")
assert_identical(sum(vapply(validated_timber$products, function(record) {
  any(vapply(record$resource_links, function(link) {
    identical(link$resource_id, "resource_blm_california")
  }, logical(1)))
}, logical(1))), 3L, "Many-Products-to-one-Resource fixture changed")
assert_identical(length(validated_timber$products[[3]]$resource_links), 2L,
                 "One-Product-to-many-Resources fixture changed")
assert_true(identical(validated_timber$products[[1]]$coverage_disposition,
                      validated_timber$products[[2]]$coverage_disposition) &&
              !identical(validated_timber$products[[1]]$delivery_class,
                         validated_timber$products[[2]]$delivery_class),
            "Delivery classification became coupled to Resource coverage")
assert_identical(length(validated_timber$products[[4]]$resource_links), 0L,
                 "Not-yet-reviewed fixture gained a Resource link")
assert_identical(length(validated_timber$products[[5]]$resource_links), 0L,
                 "Provenance-only fixture gained a fake Resource link")

bad_relationships <- clone_value(timber_fixture)
bad_relationships$products <- bad_relationships$products[-5]
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "must equal the complete Product universe", "A missing Product record was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$products[[1]]$product_id <- "fixture_timber_unknown"
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "must equal the complete Product universe", "An unknown Product ID was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$products[[1]]$resource_links[[1]]$resource_id <- "resource_unknown"
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "unavailable canonical Resource", "An unknown Resource ID was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$products[[6]] <- clone_value(bad_relationships$products[[1]])
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "duplicate Product records", "A duplicate Product record was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$products[[1]]$resource_links[[2]] <-
  clone_value(bad_relationships$products[[1]]$resource_links[[1]])
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "duplicate Product/Resource/role link", "A duplicate Resource link was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$products[[1]]$unexpected <- "field"
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "unsupported field", "An unknown relationship field was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$products[[4]]$coverage_disposition <- "direct_resource_match"
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "not_yet_reviewed requires null disposition", "Invalid not-yet-reviewed coverage was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$products[[1]]$coverage_disposition <- "direct_resource_match"
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "direct coverage requires", "Direct coverage without a direct link was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$products[[1]]$resource_links <- list()
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "selected coverage requires", "Selected coverage without a Resource was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$products[[3]]$resource_links <-
  bad_relationships$products[[3]]$resource_links[1]
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "multiple-source coverage requires", "Multiple-source coverage with one Resource was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$products[[5]]$resource_links <- list(fixture_link("resource_doi"))
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "no-public-Resource coverage requires zero", "Provenance-only coverage with a link was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$products[[1]]$delivery_evidence_refs <- list(paste0(
  "/", "Users/example/private.csv"
))
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "normalized repository-relative", "A machine-local evidence path was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$products[[1]]$product_id <- "raw bookmark provenance"
unsafe_ids <- timber_ids
unsafe_ids[[1]] <- "raw bookmark provenance"
assert_error(
  validate_relationship_fixture(bad_relationships, unsafe_ids),
  "raw bookmark material", "Raw bookmark provenance was accepted"
)

bad_relationships <- clone_value(timber_fixture)
bad_relationships$resources <- bad_relationships$resources[-1]
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "must equal the complete Resource universe",
  "A relationship registry missing one Resource review record was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$resources[[1]]$map_representation <- "direct_match_in_brim"
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "direct representation requires",
  "A direct Resource representation without a direct Product link was accepted"
)
bad_relationships <- clone_value(timber_fixture)
bad_relationships$resources[[1]]$legacy_relationship_flags <- list()
assert_error(
  validate_relationship_fixture(bad_relationships, timber_ids),
  "unsupported field",
  "A legacy Resource relationship field was accepted"
)

assert_true(!grepl('"resource_relationships"', helper_source, fixed = TRUE),
            "The compiler retains an old canonical Product-enrichment relationship read")
assert_true(!grepl("pt_guide_r12a_temporary_legacy_public_relationship_projection",
                   helper_source, fixed = TRUE),
            "The temporary R12A compiler adapter remains")
assert_true(!grepl("temporary_r12a_legacy_public_projection", relationship_json,
                   fixed = TRUE),
            "A temporary R12A compatibility object remains in canonical authority")

cat("GUIDE-I2B-R17B CNRFC relationship-enrichment contracts passed.\n")
cat("RESOURCE_SCHEMA_VERSION=3\n")
cat("RELATIONSHIP_SCHEMA_VERSION=2\n")
cat("TOTAL_RESOURCES=211\n")
cat("PUBLISHED_RESOURCES=206\n")
cat("STAGED_RESOURCES=5\n")
cat("R16B_CANONICAL_ADDITIONS=13\n")
cat("R16B_CANONICAL_MERGES=7\n")
cat("R16B_CURATED_ACCESS_POINTS=123\n")
cat("R16B_CVO_ACCESS_POINTS=42_SEARCHABLE,8_ORDINARY\n")
cat("R15B_URL_REPAIRS=25\n")
cat("R15B_ADDITIONAL_USBR_HOST_REPAIR=1\n")
cat("R15B_TOTAL_CANONICAL_URL_CHANGES=26\n")
cat("CANONICAL_URL_ACTIONS_CHANGED=26\n")
cat("HTTP_ONLY_EXCEPTION_COUNT=3\n")
cat("UNAUTHORIZED_HTTP_CANONICAL_URL_COUNT=0\n")
cat("HTTPS_DEFAULT_VALIDATOR=PASS\n")
cat("R15B_REMOVED_RESOURCES=2\n")
cat("R15B_SELECTED_REPLACEMENTS=2\n")
cat("R15B_PRODUCT_LINK_ACTIONS=0\n")
cat("R10_NEWLY_PUBLISHED_RESOURCES=34\n")
cat("R15C_NEWLY_PUBLISHED_RESOURCES=133\n")
cat("HELD_STAGED_RESOURCES=5\n")
cat("STAGED_SUBJECT_REVIEW=3\n")
cat("STAGED_TAXONOMY_BLOCKED=2\n")
cat("DUPLICATE_ACCESS_POINTS_REMOVED=14\n")
cat("MAP_REPRESENTATION=3_DIRECT,23_SELECTED,180_NOT_MAPPED\n")
cat("PRESET_COUNTS=26,180,206\n")
cat("RESOURCE_TYPE_VALUES=10_OF_10_CURRENT\n")
cat("TEMPORAL_CHARACTER_VALUES=7_OF_7_CURRENT\n")
cat("TEMPORAL_UNKNOWN_IDS=141_EXACT\n")
cat("GEOGRAPHIC_SCOPE_VALUES=8_OF_8_CURRENT\n")
cat("GEOGRAPHY_UNKNOWN_IDS=4_EXACT\n")
cat("HELD_CONTRACT_MATCH=5_OF_5_STAGED\n")
cat("PROTECTED_FIELD_EQUIVALENCE=PASS\n")
cat("CURRENT_33_EQUIVALENCE=PASS\n")
cat("R10_NEWLY_PUBLISHED_34_ONLY_PUBLICATION_STATE=PASS\n")
cat("HELD_5_EQUIVALENCE=PASS\n")
cat("CURRENT_32_NON_GOES_EQUIVALENCE=PASS\n")
cat("GOES_ALLOWED_CHANGED_FIELD=access_points_ONLY\n")
cat("STAGED_BROWSER_LEAKAGE=0\n")
cat("BROWSER_RESOURCE_FIELDS=23\n")
cat("BROWSER_RESOURCE_BYTES=", nchar(browser_json, type = "bytes"), "\n", sep = "")
cat("BROWSER_RESOURCE_SHA256=",
    digest::digest(browser_json, algo = "sha256", serialize = FALSE), "\n", sep = "")
cat("DEFAULT_PROFILE_ONLY=YES\n")
cat("RELATIONSHIP_HEURISTICS=0\n")
cat("PRODUCT_RELATIONSHIP_RECORDS=270\n")
cat("CANONICAL_RESOURCE_LINKS=94\n")
cat("PRODUCTS_WITH_RESOURCE_LINKS=70\n")
cat("RESOURCES_WITH_PRODUCT_LINKS=26\n")
cat("RELATIONSHIP_ROLE_COUNTS=13_DIRECT,62_SELECTED,19_SOURCE_REFERENCE\n")
cat("CNRFC_RELATED_PRODUCTS=7\n")
cat("RELATIONSHIP_RESOURCE_RECORDS=211\n")
cat("ALL_RESOURCE_REPRESENTATIONS=3_DIRECT,23_SELECTED,180_NOT_MAPPED,5_NOT_REVIEWED\n")
cat("LEGACY_PUBLIC_PROJECTIONS=0\n")
cat("SYNTHETIC_TIMBER_ONBOARDING=PASS\n")
cat("R12B_ADAPTER_REMOVED=PASS\n")
