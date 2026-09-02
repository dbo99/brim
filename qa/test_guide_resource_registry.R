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
expected_newly_published_ids <- expected_wave2_ids[
  !expected_wave2_ids %in% expected_held_staged_ids
]
expected_published_ids <- c(expected_ids, expected_newly_published_ids)
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
assert_identical(length(registry), 205L, "Registry must contain exactly 205 Resources")
registry_ids <- vapply(registry, `[[`, character(1), "id")
published_ids <- vapply(published, `[[`, character(1), "id")
staged <- unclass(registry)[vapply(registry, `[[`, character(1), "publication_state") == "staged"]
staged_ids <- vapply(staged, `[[`, character(1), "id")
current_registry <- unclass(registry)[match(expected_ids, registry_ids)]
wave2_registry <- unclass(registry)[match(expected_wave2_ids, registry_ids)]
newly_published <- unclass(registry)[match(expected_newly_published_ids, registry_ids)]
r14_registry <- unclass(registry)[seq.int(length(expected_baseline_resource_ids) + 1L,
                                          length(registry))]
r14_registry_ids <- vapply(r14_registry, `[[`, character(1), "id")
held_staged <- staged[match(expected_held_staged_ids, staged_ids)]
assert_identical(registry_ids[seq_along(expected_baseline_resource_ids)],
                 expected_baseline_resource_ids,
                 "The accepted 72-Resource baseline ID set/order changed")
assert_identical(
  digest::digest(paste0(paste(r14_registry_ids, collapse = "\n"), "\n"),
                 algo = "sha256", serialize = FALSE),
  "c1f168983e13817b9852dd03301d340fbdf7ab53b169affcbd16a0919cc98267",
  "The exact ordered R14 133-Resource target ID set changed"
)
assert_identical(published_ids, expected_published_ids,
                 "The exact 67 published Resources changed or reordered")
assert_identical(vapply(held_staged, `[[`, character(1), "id"), expected_held_staged_ids,
                 "The exact five held staged Resources changed or reordered")
assert_identical(staged_ids[staged_ids %in% r14_registry_ids], r14_registry_ids,
                 "The exact R14 target Resources are not all staged in contract order")
assert_identical(length(r14_registry_ids), 133L,
                 "R14 must add exactly 133 target Resources")
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
assert_identical(sum(publication_states == "published"), 67L,
                 "Published Resource count must be 67")
assert_identical(sum(publication_states == "staged"), 138L,
                 "Staged Resource count must be 138")
assert_true(all(vapply(r14_registry, function(record) {
  identical(record$publication_state, "staged")
}, logical(1))), "Every R14 target Resource must remain staged")
assert_identical(sum(vapply(r14_registry, function(record) {
  !length(record$subject_tags)
}, logical(1))), 15L,
"The approved 15 empty subject sets were not preserved exactly for staging")
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
    analysis_tool = 3L, dashboard = 9L, data_portal_or_catalog = 15L,
    data_service_or_api = 2L, dataset_or_collection = 10L,
    organization_homepage = 4L, program_or_mission = 8L,
    report_or_publication = 1L, viewer_or_explorer = 15L
  ),
  "Published Resource Type distribution changed"
)
assert_identical(
  metadata_counts(vapply(published, `[[`, character(1), "temporal_character")),
  c(
    climatology_or_normals = 1L, current_or_near_real_time = 15L,
    forecast = 2L, historical_archive = 7L, mixed = 18L,
    static_reference = 9L, unknown = 15L
  ),
  "Published temporal-character distribution changed"
)
assert_identical(
  metadata_counts(vapply(published, function(record) {
    record$geographic_scope$scope_type
  }, character(1))),
  c(global = 20L, multi_state = 2L, multinational = 4L, national = 29L,
    state = 11L, unknown = 1L),
  "Published geographic-scope distribution changed"
)

compact_json <- function(value) jsonlite::toJSON(
  value,
  auto_unbox = TRUE, null = "null", na = "null", pretty = FALSE, digits = NA
)
raw_current <- raw_registry$resources[match(expected_ids, vapply(
  raw_registry$resources, `[[`, character(1), "id"
))]
assert_identical(
  digest::digest(compact_json(raw_current), algo = "sha256", serialize = FALSE),
  "a86067ab046b93a165fd5944f85079ca7140fbb34efc0d0add79b0921f260190",
  "A current published Resource changed from the accepted R9 baseline"
)
current_non_goes_json <- compact_json(raw_current[vapply(
  raw_current, `[[`, character(1), "id"
) != "resource_noaa_goes_image_viewer"])
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
newly_published_without_state_json <- compact_json(lapply(
  newly_published, strip_publication_state
))
assert_identical(
  digest::digest(newly_published_without_state_json, algo = "sha256", serialize = FALSE),
  "d726b498b4292dcc46f720e2c166704c362539e312d357cb21a617a41ea6de7a",
  "A newly published Resource field other than publication_state changed from R9"
)
baseline_registry <- raw_registry$resources[seq_along(expected_baseline_resource_ids)]
assert_identical(
  digest::digest(compact_json(baseline_registry), algo = "sha256", serialize = FALSE),
  "de214a316a4842bc76db1276beaa8d6ec4d2f66679d25d8ae6a2d90bdfa58113",
  "One of the accepted 72 baseline Resources changed during R14 staging"
)
assert_identical(
  digest::digest(compact_json(raw_registry$resources[73:205]),
                 algo = "sha256", serialize = FALSE),
  "d09228500cf73aa111336f5d2541bcb9a6ad771509b23a85917be5e0145a7a48",
  "An R14 schema-v3 Resource record differs from the reviewed target contract"
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
assert_true(!length(final_aliases), "Unreviewed final-ID aliases entered the registry")
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
  all(grepl("^https://", urls)) && !anyDuplicated(urls) && all(nzchar(labels)) &&
    sum(roles == "canonical") == 1L &&
    identical(urls[[match("canonical", roles)]], record$canonical_url)
}, logical(1))
assert_true(all(all_access_valid),
            "Registry access-point URLs/labels/canonical identity changed")

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
assert_identical(length(raw_relationship_registry$resources), 205L,
                 "Product-Resource relationship Resource count changed")
assert_true(!anyDuplicated(relationship_product_ids),
            "Product-Resource relationship Product IDs are not unique")
baseline_relationship_registry <- list(
  schema_version = raw_relationship_registry$schema_version,
  products = raw_relationship_registry$products,
  resources = raw_relationship_registry$resources[seq_along(expected_baseline_resource_ids)]
)
assert_identical(
  digest::digest(compact_json(baseline_relationship_registry),
                 algo = "sha256", serialize = FALSE),
  "6b4847dbd038cb23161bcda385d7385f894d137c9f1de8f6957607874d3cec9b",
  "The schema marker, 270 Products, links, or accepted 72 Resource reviews changed from R12B"
)
assert_identical(
  digest::digest(compact_json(raw_relationship_registry$resources[73:205]),
                 algo = "sha256", serialize = FALSE),
  "b6f366a98d54f26d1ba2f3463d74174f8288a50f4e9d41e8ffe0f9b1bfd0b9d8",
  "An R14 schema-v2 Resource relationship record differs from the reviewed contract"
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
)))), c(70L, 200L), "Coverage review-state counts changed")
assert_identical(unname(as.integer(table(factor(
  coverage_dispositions,
  levels = c(
    "direct_resource_match", "selected_product_from_broader_resource",
    "multiple_source_resources", "provenance_only_no_public_resource",
    "internal_no_external_resource", "not_yet_reviewed"
  )
)))), c(10L, 47L, 8L, 1L, 4L, 200L),
"Coverage-disposition counts changed")
assert_identical(length(canonical_links), 86L,
                 "Canonical Product-Resource link count changed")
assert_identical(unname(as.integer(table(factor(
  canonical_link_roles,
  levels = c("direct_match_in_brim", "selected_product_from_broader_resource",
             "source_reference")
)))), c(13L, 57L, 16L), "Canonical relationship-role counts changed")

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
)))), c(3L, 20L, 44L), "Published Resource representation counts changed")
staged_relationship_resources <- relationship_resources[
  match(expected_held_staged_ids, relationship_resource_ids)
]
assert_true(all(vapply(staged_relationship_resources, function(record) {
  identical(record$map_review_state, "not_yet_reviewed") &&
    is.null(record$map_representation) && !length(record$evidence_refs)
}, logical(1))), "Staged Resources must retain explicit deferred review records")
r14_relationship_resources <- relationship_resources[
  match(r14_registry_ids, relationship_resource_ids)
]
assert_true(all(vapply(r14_relationship_resources, function(record) {
  identical(record$map_review_state, "reviewed") &&
    identical(record$map_representation, "not_currently_mapped_in_brim") &&
    length(record$evidence_refs) > 0L
}, logical(1))), "Every R14 Resource must retain its reviewed not-mapped relationship record")
all_resource_representations <- vapply(relationship_resources, function(record) {
  if (is.null(record$map_representation)) "not_yet_reviewed" else record$map_representation
}, character(1))
assert_identical(unname(as.integer(table(factor(
  all_resource_representations,
  levels = c("direct_match_in_brim", "selected_products_in_brim",
             "not_currently_mapped_in_brim", "not_yet_reviewed")
)))), c(3L, 20L, 177L, 5L),
"Full Resource representation counts changed")

direct_resource_ids <- c(
  "resource_calfire_fire_perimeters", "resource_nifc_wfigs_current",
  "resource_nrcs_scan"
)
selected_resource_ids <- c(
  "resource_blm_california", "resource_prism_normals", "resource_usgs_bcmv8",
  "resource_dwr_bulletin118_sgma_2019", "resource_usgs_water_dashboard",
  "resource_noaa_nwps", "resource_noaa_goes_image_viewer",
  "resource_dwr_california_groundwater_live", "resource_dwr_california_water_watch",
  "resource_dwr_casgem", "resource_dwr_cdec",
  "resource_dwr_groundwater_sustainability_agencies",
  "resource_nasa_firms_global_fire_map", "resource_noaa_cnrfc",
  "resource_noaa_cpc_forecasts_outlooks", "resource_noaa_wpc_qpf",
  "resource_nrcs_snow_survey_water_supply_forecasting", "resource_usbr",
  "resource_usgs_national_hydrography_products", "resource_usgs_water_data_nation"
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
enrichment_json <- jsonlite::toJSON(
  enrichment_raw, auto_unbox = TRUE, null = "null", na = "null",
  pretty = FALSE, digits = NA
)
assert_identical(
  digest::digest(enrichment_json, algo = "sha256", serialize = FALSE),
  "e83984f53fc4db9fb1c3916c3c871b19ccf78c1f9404db3c2a5e4520868ce500",
  "Unrelated Product enrichment changed during relationship-authority removal"
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
assert_identical(length(projected_relationships), 86L,
                 "Browser reverse relationship index lost exact rows")
assert_identical(unname(as.integer(table(factor(
  projected_roles,
  levels = c("direct_match_in_brim", "selected_product_from_broader_resource",
             "source_reference")
)))), c(13L, 57L, 16L), "Projected relationship-role counts changed")
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
}, logical(1))), 20L, "Projected selected-products Resource count changed")
assert_identical(sum(vapply(browser_records, function(record) {
  identical(record$mapRepresentation, "not_currently_mapped_in_brim")
}, logical(1))), 44L, "Projected not-mapped Resource count changed")

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

assert_identical(pt_guide_normalize_resource_search("Weather—Water / Forecast"),
                 "weather water forecast",
                 "Resource search normalization is not deterministic punctuation-to-space ASCII")
browser_json <- jsonlite::toJSON(
  browser_records, auto_unbox = TRUE, null = "null", na = "null",
  pretty = FALSE, digits = NA
)
assert_identical(length(browser_records), 67L,
                 "Browser projection must contain exactly 67 published Resources")
assert_true(!any(vapply(staged_ids, function(id) {
  grepl(id, browser_json, fixed = TRUE)
}, logical(1))), "A staged Resource ID entered the browser projection")
assert_identical(
  digest::digest(browser_json, algo = "sha256", serialize = FALSE),
  "66e7a271c738ea9626aa6cad39ece66d043afa5c8eba88cdf62f74abbcb0a1c2",
  "The browser Resource payload changed beyond the exact R12B public projection"
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
assert_identical(length(pt_guide_resource_published_records(validated_staged)), 66L,
                 "Publication projection did not exclude a staged negative fixture")
bad <- fresh_registry()
bad$resources[[1]]$canonical_url <- "https://localhost/private"
bad$resources[[1]]$access_points[[1]]$url <- "https://localhost/private"
expect_invalid(bad, "local, private, or restricted host",
               "Private URL was accepted")
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
bad_relationships$products[[1]]$delivery_evidence_refs <- list("/Users/example/private.csv")
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

cat("GUIDE-I2B-R14 target-200 staging contracts passed.\n")
cat("RESOURCE_SCHEMA_VERSION=3\n")
cat("RELATIONSHIP_SCHEMA_VERSION=2\n")
cat("TOTAL_RESOURCES=205\n")
cat("PUBLISHED_RESOURCES=67\n")
cat("STAGED_RESOURCES=138\n")
cat("R14_NEW_STAGED_RESOURCES=133\n")
cat("NEWLY_PUBLISHED_RESOURCES=34\n")
cat("HELD_STAGED_RESOURCES=5\n")
cat("STAGED_SUBJECT_REVIEW=3\n")
cat("STAGED_TAXONOMY_BLOCKED=2\n")
cat("DUPLICATE_ACCESS_POINTS_REMOVED=14\n")
cat("MAP_REPRESENTATION=3_DIRECT,20_SELECTED,44_NOT_MAPPED\n")
cat("PRESET_COUNTS=23,44,67\n")
cat("RESOURCE_TYPE_VALUES=9_OF_10_CURRENT\n")
cat("TEMPORAL_CHARACTER_VALUES=7_OF_7_CURRENT\n")
cat("TEMPORAL_UNKNOWN_IDS=15_EXACT\n")
cat("GEOGRAPHIC_SCOPE_VALUES=6_OF_8_CURRENT\n")
cat("GEOGRAPHY_UNKNOWN_IDS=1_EXACT\n")
cat("PUBLICATION_TRANSITIONS=34_OF_34_EXACT\n")
cat("HELD_CONTRACT_MATCH=5_OF_5_STAGED\n")
cat("PROTECTED_FIELD_EQUIVALENCE=PASS\n")
cat("CURRENT_33_EQUIVALENCE=PASS\n")
cat("NEWLY_PUBLISHED_34_ONLY_PUBLICATION_STATE=PASS\n")
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
cat("CANONICAL_RESOURCE_LINKS=86\n")
cat("RELATIONSHIP_RESOURCE_RECORDS=205\n")
cat("ALL_RESOURCE_REPRESENTATIONS=3_DIRECT,20_SELECTED,177_NOT_MAPPED,5_NOT_REVIEWED\n")
cat("LEGACY_PUBLIC_PROJECTIONS=0\n")
cat("SYNTHETIC_TIMBER_ONBOARDING=PASS\n")
cat("R12B_ADAPTER_REMOVED=PASS\n")
