#!/usr/bin/env Rscript

# Focused source-only contracts for the canonical schema-v3 Resource registry,
# exact Product-enrichment relationships, and the public Resource projection.

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
  "subjectTags", "informationTypes", "variables", "useScopes", "geographicScope",
  "relatedProducts", "relationshipFlags", "searchText"
)

assert_identical(raw_registry$schema_version, 3L, "Registry schema marker changed")
assert_true(inherits(registry, "pt_guide_resource_registry"),
            "Registry reader did not mark validated records")
assert_identical(length(registry), 33L, "Registry must contain exactly 33 Resources")
registry_ids <- vapply(registry, `[[`, character(1), "id")
published_ids <- vapply(published, `[[`, character(1), "id")
assert_identical(registry_ids, expected_ids, "Complete Resource ID set/order changed")
assert_identical(published_ids, expected_ids,
                 "All 33 Resources must be published in registry order")
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
assert_identical(sum(publication_states == "published"), 33L,
                 "Published Resource count must be 33")
assert_identical(sum(publication_states == "staged"), 0L,
                 "No staged Resource may remain after R6 publication")
assert_true(all(vapply(registry[match(wave1_ids, registry_ids)], function(record) {
  identical(record$publication_state, "published")
}, logical(1))), "The exact approved 24-Resource cohort was not published")
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
  vapply(registry, `[[`, character(1), "resource_type"), expected_resource_types,
  "The exact current-33 Resource Type assignments changed"
)
assert_identical(
  vapply(registry, `[[`, character(1), "temporal_character"),
  expected_temporal_characters,
  "The exact current-33 temporal-character assignments changed"
)
assert_identical(
  vapply(registry, function(record) record$geographic_scope$scope_type, character(1)),
  expected_geographic_scope_types,
  "The exact current-33 geographic scope-class assignments changed"
)
assert_identical(
  lapply(registry, function(record) {
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
  registry_ids[expected_temporal_characters == "unknown"],
  expected_temporal_unknown_ids,
  "Temporal-character unknown Resource IDs changed"
)
assert_identical(
  registry_ids[expected_geographic_scope_types == "unknown"],
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

protected_fields <- c(
  "id", "aliases", "migration_aliases", "search_aliases", "order", "title",
  "providers", "summary", "canonical_url", "access_points", "resource_granularity",
  "subject_tags", "information_type_tags", "variables", "use_scopes", "access_class",
  "public_source_references", "publication_state"
)
protected_json <- jsonlite::toJSON(
  lapply(raw_registry$resources, function(record) record[protected_fields]),
  auto_unbox = TRUE, null = "null", na = "null", pretty = FALSE, digits = NA
)
assert_identical(
  digest::digest(protected_json, algo = "sha256", serialize = FALSE),
  "10f1d4a6f4115af1ea5e8d439058f0648325676b3cd8ad8f354b94fdde8bda1b",
  "Protected current-33 Resource fields or granularity changed from the R8 baseline"
)

final_aliases <- unlist(lapply(registry, function(record) {
  unname(as.character(unlist(record$aliases, use.names = FALSE)))
}), use.names = FALSE)
migration_aliases <- unlist(lapply(registry, function(record) {
  unname(as.character(unlist(record$migration_aliases, use.names = FALSE)))
}), use.names = FALSE)
assert_true(!length(final_aliases), "Unreviewed final-ID aliases entered the registry")
assert_identical(length(migration_aliases), 26L,
                 "Migration alias inventory changed")
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

goes <- registry[[match("resource_noaa_goes_image_viewer", registry_ids)]]
expected_goes_urls <- c(
  "https://www.star.nesdis.noaa.gov/GOES/",
  "https://www.star.nesdis.noaa.gov/GOES/sector_band.php?band=GEOCOLOR&dim=1&length=24&sat=G18&sector=pnw",
  "https://www.star.nesdis.noaa.gov/GOES/sector_band.php?band=FireTemperature&dim=1&length=12&sat=G18&sector=psw",
  "https://www.star.nesdis.noaa.gov/GOES/sector_band.php?band=GEOCOLOR&dim=1&length=24&sat=G18&sector=psw",
  "https://www.star.nesdis.noaa.gov/GOES/sector_band.php?band=GEOCOLOR&dim=1&length=24&sat=G18&sector=wus"
)
assert_identical(vapply(goes$access_points, `[[`, character(1), "url"),
                 expected_goes_urls, "GOES configured-view URLs changed")
assert_identical(vapply(goes$access_points, `[[`, character(1), "role"),
                 c("canonical", rep("configured_view", 4L)),
                 "GOES configured-view roles changed")
assert_identical(sum(grepl("sector=psw", expected_goes_urls, fixed = TRUE)), 2L,
                 "GOES must retain exactly two Pacific Southwest configured views")
assert_true(any(grepl("band=FireTemperature", expected_goes_urls, fixed = TRUE)) &&
              any(grepl("band=GEOCOLOR", expected_goes_urls[grepl(
                "sector=psw", expected_goes_urls, fixed = TRUE
              )], fixed = TRUE)),
            "GOES Pacific Southwest query semantics changed")

enrichment <- pt_guide_read_product_enrichment()
relationship_rows <- unlist(lapply(names(enrichment), function(product_id) {
  lapply(enrichment[[product_id]]$resource_relationships, function(relationship) {
    c(list(productId = product_id), relationship)
  })
}), recursive = FALSE)
relationship_types <- vapply(relationship_rows, `[[`, character(1), "relationship_type")
assert_identical(length(relationship_rows), 17L,
                 "Exact Product-enrichment relationship row count changed")
assert_identical(sum(relationship_types == "displayed_in_brim"), 0L,
                 "Displayed-in-BRIM relationship count changed")
assert_identical(sum(relationship_types == "used_by_brim"), 7L,
                 "Used-by-BRIM relationship count changed")
assert_identical(sum(relationship_types == "related_external_resource"), 10L,
                 "Related-external relationship count changed")

product_fixtures <- lapply(names(enrichment), function(product_id) {
  relationships <- lapply(enrichment[[product_id]]$resource_relationships,
                          function(relationship) list(
    id = relationship$id,
    role = relationship$role,
    relationshipType = relationship$relationship_type,
    useScope = relationship$use_scope
  ))
  list(
    id = product_id,
    title = paste("Eligible Product", product_id),
    entityType = "Layer",
    relatedResources = unname(relationships),
    relatedResourceIds = if (length(relationships)) {
      vapply(relationships, `[[`, character(1), "id")
    } else character(0)
  )
})
browser_records <- pt_guide_resource_browser_records(published, product_fixtures)
assert_identical(vapply(browser_records, `[[`, character(1), "id"), expected_ids,
                 "Browser projection changed Resource order or identity")
assert_true(all(vapply(browser_records, function(record) {
  identical(names(record), browser_fields)
}, logical(1))), "Resource browser projection is not the exact 22-field shape")
assert_true(all(vapply(seq_along(browser_records), function(index) {
  record <- browser_records[[index]]
  identical(
    record$resourceTypeLabel,
    unname(expected_metadata_vocabularies$resource_type[[record$resourceType]])
  ) &&
    identical(record$temporalCharacter, expected_temporal_characters[[index]]) &&
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
assert_true(all(vapply(browser_records, function(record) {
  expected_search <- pt_guide_normalize_resource_search(list(
    record$title, record$aliases, record$provider,
    vapply(record$providers, `[[`, character(1), "name"), record$summary,
    vapply(record$accessPoints, `[[`, character(1), "label"),
    record$resourceTypeLabel, record$resourceGranularity, record$subjectTags,
    record$informationTypes, record$variables, record$useScopes,
    record$geographicScope$scopeLabel, record$geographicScope$names
  ))
  identical(record$searchText, expected_search)
}, logical(1))), "Temporal character entered searchText or label search drifted")

projected_relationships <- unlist(lapply(browser_records, `[[`, "relatedProducts"),
                                  recursive = FALSE)
projected_types <- vapply(projected_relationships, `[[`, character(1), "relationshipType")
assert_identical(length(projected_relationships), 17L,
                 "Browser reverse relationship index lost exact rows")
assert_identical(sum(projected_types == "displayed_in_brim"), 0L,
                 "Projected displayed relationship count changed")
assert_identical(sum(projected_types == "used_by_brim"), 7L,
                 "Projected used relationship count changed")
assert_identical(sum(projected_types == "related_external_resource"), 10L,
                 "Projected external relationship count changed")

flag_ids <- function(field) vapply(Filter(function(record) {
  isTRUE(record$relationshipFlags[[field]])
}, browser_records), `[[`, character(1), "id")
expected_used <- c(
  "resource_prism_normals", "resource_usgs_bcmv8",
  "resource_dwr_bulletin118_sgma_2019", "resource_calfire_fire_perimeters",
  "resource_nifc_wfigs_current", "resource_nrcs_scan"
)
expected_related <- c(
  "resource_blm_california", "resource_usgs_water_dashboard", "resource_noaa_nwps"
)
expected_brim_linked <- c(expected_used, expected_related)
assert_true(setequal(flag_ids("brimLinked"), expected_brim_linked),
            "BRIM-linked Resource IDs changed")
assert_true(setequal(flag_ids("usedByBrim"), expected_used),
            "Used Resource IDs changed")
assert_identical(length(flag_ids("displayedInBrim")), 0L,
                 "Available-in-BRIM Resource count must remain zero")
assert_true(setequal(flag_ids("relatedExternalResource"), expected_related),
            "Related Resource IDs changed")
assert_identical(length(flag_ids("beyondBrim")), 24L,
                 "Beyond BRIM count must be 24")
assert_true(!length(intersect(flag_ids("brimLinked"), flag_ids("beyondBrim"))) &&
              setequal(c(flag_ids("brimLinked"), flag_ids("beyondBrim")), expected_ids),
            "BRIM-linked and Beyond BRIM must be disjoint exhaustive complements")

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
projected_goes <- browser_records[[match("resource_noaa_goes_image_viewer", expected_ids)]]
assert_identical(vapply(projected_goes$accessPoints, `[[`, character(1), "url"),
                 expected_goes_urls, "GOES access points changed in browser projection")

assert_identical(pt_guide_normalize_resource_search("Weather—Water / Forecast"),
                 "weather water forecast",
                 "Resource search normalization is not deterministic punctuation-to-space ASCII")
browser_json <- jsonlite::toJSON(
  browser_records, auto_unbox = TRUE, null = "null", na = "null",
  pretty = FALSE, digits = NA
)
forbidden_fields <- c(
  "migration_aliases", "publication_state", "public_source_references",
  "canonical_url", "access_class", "source_refs", "editorial_state",
  "verification", "freshness", "lifecycle", "relatedProductIds"
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
assert_identical(length(pt_guide_resource_published_records(validated_staged)), 32L,
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
  pt_guide_resource_browser_records(unclass(published), product_fixtures),
  "requires a validated registry",
  "Browser projection accepted an unvalidated registry"
)
duplicate_products <- product_fixtures
duplicate_products[[2]]$id <- duplicate_products[[1]]$id
assert_error(
  pt_guide_resource_browser_records(published, duplicate_products),
  "unique eligible Product IDs",
  "Browser projection accepted duplicate eligible Product IDs"
)

cat("GUIDE-I2B-R8 exact Resource metadata registry/projection contracts passed.\n")
cat("SCHEMA_VERSION=3\n")
cat("TOTAL_RESOURCES=33\n")
cat("PUBLISHED_RESOURCES=33\n")
cat("STAGED_RESOURCES=0\n")
cat("EXACT_RELATIONSHIP_ROWS=17\n")
cat("DISPLAYED_IN_BRIM=0\n")
cat("USED_BY_BRIM=7\n")
cat("RELATED_EXTERNAL_RESOURCE=10\n")
cat("RELATIONSHIP_SUBTYPE_UNIQUE_COUNTS=0_DISPLAYED,6_USED,3_RELATED\n")
cat("BRIM_LINKED_RESOURCE_IDS=9\n")
cat("BEYOND_BRIM_RESOURCE_IDS=24\n")
cat("PRESET_COUNTS=33,9,24\n")
cat("RESOURCE_TYPE_VALUES=7_OF_10_CURRENT\n")
cat("TEMPORAL_CHARACTER_VALUES=6_OF_7_CURRENT\n")
cat("TEMPORAL_UNKNOWN_IDS=4_EXACT\n")
cat("GEOGRAPHIC_SCOPE_VALUES=5_OF_8_CURRENT\n")
cat("GEOGRAPHY_UNKNOWN_IDS=1_EXACT\n")
cat("CONTRACT_MATCH=33_OF_33\n")
cat("PROTECTED_FIELD_EQUIVALENCE=PASS\n")
cat("BROWSER_RESOURCE_FIELDS=22\n")
cat("BROWSER_RESOURCE_BYTES=", nchar(browser_json, type = "bytes"), "\n", sep = "")
cat("BROWSER_RESOURCE_SHA256=",
    digest::digest(browser_json, algo = "sha256", serialize = FALSE), "\n", sep = "")
cat("DEFAULT_PROFILE_ONLY=YES\n")
cat("RELATIONSHIP_HEURISTICS=0\n")
