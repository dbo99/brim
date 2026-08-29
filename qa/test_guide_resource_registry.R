#!/usr/bin/env Rscript

# Focused source-only contracts for the canonical schema-v2 Guide Resource registry.
# Temporary negative fixtures stay under R's session temp directory.

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

assert_true(requireNamespace("jsonlite", quietly = TRUE), "jsonlite is required for registry QA")
assert_true(requireNamespace("digest", quietly = TRUE), "digest is required for registry QA")
source(file.path("03_functions", "leaflet_guide_helpers.r"))

registry_path <- file.path("00_config", "guide_resources.json")
raw_registry <- jsonlite::fromJSON(registry_path, simplifyVector = FALSE)
registry <- pt_guide_read_resource_registry(registry_path)
published <- pt_guide_resource_published_records(registry)
staged <- unclass(registry)[vapply(registry, function(record) {
  identical(record$publication_state, "staged")
}, logical(1))]

expected_published_ids <- c(
  "resource_doi",
  "resource_blm_california",
  "resource_prism_normals",
  "resource_usgs_bcmv8",
  "resource_dwr_bulletin118_sgma_2019",
  "resource_calfire_fire_perimeters",
  "resource_nifc_wfigs_current",
  "resource_usgs_water_dashboard",
  "resource_noaa_nwps"
)
expected_staged_ids <- c(
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
required_fields <- c(
  "id", "aliases", "migration_aliases", "search_aliases", "order", "title",
  "providers", "summary", "canonical_url", "access_points", "resource_type",
  "resource_granularity", "subject_tags", "information_type_tags", "variables",
  "use_scopes", "geographic_scope", "access_class", "public_source_references",
  "publication_state"
)

assert_identical(raw_registry$schema_version, 2L, "Registry schema/version marker changed")
assert_true(inherits(registry, "pt_guide_resource_registry"),
            "Registry reader did not mark the returned records as validated")
assert_identical(length(registry), 33L, "Registry must contain exactly 33 Resources")
registry_ids <- vapply(registry, `[[`, character(1), "id")
published_ids <- vapply(published, `[[`, character(1), "id")
staged_ids <- vapply(staged, `[[`, character(1), "id")
assert_identical(published_ids, expected_published_ids,
                 "Published Resource ID set/order changed")
assert_identical(staged_ids, expected_staged_ids,
                 "Staged Resource ID set/order changed")
assert_identical(registry_ids, c(expected_published_ids, expected_staged_ids),
                 "Complete Resource registry ID set/order changed")
assert_true(!anyDuplicated(registry_ids), "Registry primary IDs are not unique")
assert_true(all(vapply(registry, function(record) identical(names(record), required_fields), logical(1))),
            "Registry required/allowed field contract changed")
registry_order <- as.integer(vapply(registry, `[[`, numeric(1), "order"))
assert_identical(registry_order, seq_along(registry),
                 "Registry explicit order is not unique, complete, and in file order")
publication_states <- vapply(registry, `[[`, character(1), "publication_state")
assert_identical(sum(publication_states == "published"), 9L,
                 "Published Resource count changed")
assert_identical(sum(publication_states == "staged"), 24L,
                 "Staged Resource count changed")

final_aliases <- unlist(lapply(registry, function(record) {
  unname(as.character(unlist(record$aliases, use.names = FALSE)))
}), use.names = FALSE)
migration_aliases <- unlist(lapply(registry, function(record) {
  unname(as.character(unlist(record$migration_aliases, use.names = FALSE)))
}), use.names = FALSE)
assert_true(!length(final_aliases), "Unreviewed final Resource aliases entered the registry")
assert_identical(length(migration_aliases), 26L,
                 "Migration alias inventory must contain two reconciled existing keys and 24 Wave 1 keys")
assert_true(!anyDuplicated(c(registry_ids, final_aliases)),
            "Primary IDs and final aliases collide")
assert_true(!anyDuplicated(migration_aliases),
            "Migration aliases are not globally unique")
assert_identical(
  unname(as.character(unlist(registry[[8]]$migration_aliases))),
  "res.usgs.usgs-national-water-dashboard.dashboard",
  "USGS dashboard migration alias changed"
)
assert_identical(
  unname(as.character(unlist(registry[[9]]$migration_aliases))),
  "res.noaa.national-water-prediction-service.viewer",
  "NOAA NWPS migration alias changed"
)
assert_true(!"res.climate-and-drought-data-providers.prism-climate-data.platform" %in%
              migration_aliases,
            "The broad PRISM platform candidate was incorrectly aliased to PRISM normals")

assert_true(all(vapply(published, function(record) {
  identical(record$access_class, "public") &&
    identical(record$publication_state, "published") &&
    identical(record$resource_type, "unknown") &&
    identical(record$resource_granularity, "unknown") &&
    !length(record$search_aliases) &&
    !length(record$subject_tags) &&
    !length(record$information_type_tags) &&
    !length(record$variables) &&
    !length(record$use_scopes) &&
    identical(record$geographic_scope$scope_type, "unknown") &&
    !length(record$geographic_scope$names) &&
    length(record$access_points) == 1L &&
    identical(record$access_points[[1]]$role, "canonical") &&
    identical(record$access_points[[1]]$label, record$title) &&
    identical(record$access_points[[1]]$url, record$canonical_url)
}, logical(1))), "Published records did not retain conservative schema-v2 defaults")

staged_fragment_pretty <- jsonlite::toJSON(
  list(resources = unname(staged)),
  auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE, digits = NA
)
staged_fragment_minified <- jsonlite::toJSON(
  list(resources = unname(staged)),
  auto_unbox = TRUE, null = "null", na = "null", pretty = FALSE, digits = NA
)
assert_identical(nchar(staged_fragment_pretty, type = "bytes") + 1L, 49486L,
                 "Formatted Wave 1 registry fragment changed from R3 evidence")
assert_identical(nchar(staged_fragment_minified, type = "bytes"), 34278L,
                 "Minified Wave 1 registry fragment changed from R3 evidence")
access_counts <- vapply(staged, function(record) length(record$access_points), integer(1))
assert_identical(sum(access_counts), 30L, "Wave 1 access-point count changed")
assert_identical(max(access_counts), 5L, "Wave 1 maximum access-point count changed")

all_access_valid <- vapply(registry, function(record) {
  urls <- vapply(record$access_points, `[[`, character(1), "url")
  roles <- vapply(record$access_points, `[[`, character(1), "role")
  labels <- vapply(record$access_points, `[[`, character(1), "label")
  all(grepl("^https://", urls)) &&
    !anyDuplicated(urls) &&
    all(nzchar(labels)) &&
    sum(roles == "canonical") == 1L &&
    identical(urls[[match("canonical", roles)]], record$canonical_url)
}, logical(1))
assert_true(all(all_access_valid),
            "Registry access points are not unique labeled public HTTPS URLs with one canonical match")
goes <- registry[[match("resource_noaa_goes_image_viewer", registry_ids)]]
expected_goes_urls <- c(
  "https://www.star.nesdis.noaa.gov/GOES/",
  "https://www.star.nesdis.noaa.gov/GOES/sector_band.php?band=GEOCOLOR&dim=1&length=24&sat=G18&sector=pnw",
  "https://www.star.nesdis.noaa.gov/GOES/sector_band.php?band=FireTemperature&dim=1&length=12&sat=G18&sector=psw",
  "https://www.star.nesdis.noaa.gov/GOES/sector_band.php?band=GEOCOLOR&dim=1&length=24&sat=G18&sector=psw",
  "https://www.star.nesdis.noaa.gov/GOES/sector_band.php?band=GEOCOLOR&dim=1&length=24&sat=G18&sector=wus"
)
assert_identical(vapply(goes$access_points, `[[`, character(1), "url"), expected_goes_urls,
                 "Corrected GOES configured-view URLs changed")
assert_identical(
  vapply(goes$access_points, `[[`, character(1), "role"),
  c("canonical", rep("configured_view", 4L)),
  "GOES configured-view role structure changed"
)
grace_conditions <- registry[[
  match("resource_nasa_grace_groundwater_soil_moisture", registry_ids)
]]
assert_identical(
  vapply(grace_conditions$access_points, `[[`, character(1), "role"),
  c("canonical", "archive", "comparison_viewer"),
  "GRACE conditions access-point family decision changed"
)
nidis_soil <- registry[[match("resource_nidis_soil_moisture_dashboard", registry_ids)]]
assert_identical(
  sum(vapply(nidis_soil$providers, `[[`, character(1), "role") == "publisher"),
  2L,
  "Repeated publisher roles with unique provider names were not retained"
)
vegdri <- registry[[match("resource_usgs_vegdri", registry_ids)]]
assert_identical(
  sum(vapply(vegdri$providers, `[[`, character(1), "role") == "partner"),
  2L,
  "Repeated partner roles with unique provider names were not retained"
)

expected_browser <- list(
  list(kind = "Resource", id = "resource_doi", title = "U.S. Department of the Interior",
       provider = "U.S. Department of the Interior", summary = "Department-level information and programs.",
       url = "https://www.doi.gov/", relatedProductIds = character(0)),
  list(kind = "Resource", id = "resource_blm_california", title = "BLM California",
       provider = "Bureau of Land Management",
       summary = "Official BLM California programs, offices, and public information.",
       url = "https://www.blm.gov/california", relatedProductIds = c("huc8", "gw_bull118")),
  list(kind = "Resource", id = "resource_prism_normals", title = "PRISM 1991–2020 Climate Normals",
       provider = "PRISM Climate Group, Oregon State University",
       summary = "Official PRISM 30-year normals access and documentation for the 1991–2020 period.",
       url = "https://prism.oregonstate.edu/normals/", relatedProductIds = "huc8"),
  list(kind = "Resource", id = "resource_usgs_bcmv8",
       title = "USGS Basin Characterization Model (BCMv8)", provider = "U.S. Geological Survey",
       summary = "Official BCMv8 model and data-release context for hydrologic California.",
       url = "https://www.sciencebase.gov/catalog/item/5f29c62d82cef313ed9edb39",
       relatedProductIds = "huc8"),
  list(kind = "Resource", id = "resource_dwr_bulletin118_sgma_2019",
       title = "DWR Bulletin 118 SGMA 2019 Basin Prioritization",
       provider = "California Department of Water Resources",
       summary = "Official final 2019 SGMA basin-prioritization service used for BRIM's exact code-based attribute join.",
       url = "https://gis.water.ca.gov/arcgis/rest/services/Geoscientific/i08_B118_SGMA_2019_Basin_Prioritization/MapServer",
       relatedProductIds = "gw_bull118"),
  list(kind = "Resource", id = "resource_calfire_fire_perimeters",
       title = "CAL FIRE FRAP Fire Perimeters", provider = "CAL FIRE / FRAP",
       summary = "Recent large California fire-perimeter/burn-scar context from CAL FIRE FRAP. Current-view snapshot; zoom in first. Public service can take 30+ seconds or return blank at broad extents. Use source dates and CAL FIRE cautions before statistics/reporting.",
       url = "https://www.fire.ca.gov/what-we-do/fire-resource-assessment-program/fire-perimeters",
       relatedProductIds = c("EXT070", "EXT072")),
  list(kind = "Resource", id = "resource_nifc_wfigs_current",
       title = "NIFC WFIGS Current Interagency Fire Perimeters", provider = "NIFC / WFIGS",
       summary = "Current-view interagency wildfire/complex perimeter snapshot. Perimeters are operational and incomplete; refresh after panning/zooming and check source dates. BRIM filters to wildfire/complex records where the service supports SQL.",
       url = "https://data-nifc.opendata.arcgis.com/datasets/nifc::wfigs-current-interagency-fire-perimeters/about",
       relatedProductIds = "EXT074"),
  list(kind = "Resource", id = "resource_usgs_water_dashboard",
       title = "USGS National Water Dashboard", provider = "U.S. Geological Survey",
       summary = "Official current water information and station context from USGS.",
       url = "https://dashboard.waterdata.usgs.gov/",
       relatedProductIds = "product-ops-usgs-groundwater"),
  list(kind = "Resource", id = "resource_noaa_nwps",
       title = "NOAA National Water Prediction Service", provider = "NOAA / National Weather Service",
       summary = "Official river observations, forecasts, and water-prediction context.",
       url = "https://water.noaa.gov/", relatedProductIds = character(0))
)
browser_records <- pt_guide_resource_browser_records(published)
assert_identical(browser_records, expected_browser,
                 "Registry adaptation changed the pre-existing browser-visible Resource projection")
assert_true(all(vapply(browser_records, function(record) {
  identical(names(record), c("kind", "id", "title", "provider", "summary", "url", "relatedProductIds"))
}, logical(1))), "Registry-only fields leaked into the browser Resource projection")
browser_json <- jsonlite::toJSON(
  browser_records, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE, digits = NA
)
assert_identical(nchar(browser_json, type = "bytes"), 3659L,
                 "Browser Resource serialization byte count changed from the captured baseline")
assert_identical(
  digest::digest(browser_json, algo = "sha256", serialize = FALSE),
  "b5b1307349df5f65182695e06afbb739bf29ae0ce65f0cd6ca4552c7333ce827",
  "Browser Resource serialization changed from the captured baseline"
)
assert_true(!any(vapply(staged_ids, function(id) grepl(id, browser_json, fixed = TRUE), logical(1))),
            "A staged Resource ID leaked into the browser Resource serialization")
staged_browser_terms <- unique(c(
  migration_aliases,
  unlist(lapply(staged, function(record) c(
    unlist(record$search_aliases, use.names = FALSE),
    record$title,
    record$summary,
    vapply(record$access_points, `[[`, character(1), "url")
  )), use.names = FALSE)
))
assert_true(!any(vapply(staged_browser_terms, function(term) {
  nzchar(term) && grepl(term, browser_json, fixed = TRUE)
}, logical(1))), "Staged aliases, text, or access points leaked into browser Resources")
assert_identical(names(pt_guide_supported_profiles()), "default",
                 "The staged publication gate was incorrectly implemented as another profile")

assert_error(
  pt_guide_resource_published_records(unclass(registry)),
  "requires a validated registry",
  "Publication projection accepted an unvalidated registry"
)
unknown_state_registry <- registry
unknown_state_registry[[1]]$publication_state <- "unknown"
assert_error(
  pt_guide_resource_published_records(unknown_state_registry),
  "unknown publication state",
  "Publication projection accepted an unknown state"
)
assert_error(
  pt_guide_resource_browser_records(registry),
  "published records only",
  "Browser adaptation accepted staged registry records"
)

write_registry_fixture <- function(value) {
  path <- tempfile("guide_resource_registry_", tmpdir = tempdir(), fileext = ".json")
  jsonlite::write_json(
    value, path, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE, digits = NA
  )
  path
}
expect_invalid <- function(value, pattern, message) {
  path <- write_registry_fixture(value)
  assert_error(pt_guide_read_resource_registry(path), pattern, message)
}
fresh_registry <- function() jsonlite::fromJSON(registry_path, simplifyVector = FALSE)
with_primary_url <- function(value, url) {
  value$resources[[1]]$canonical_url <- url
  value$resources[[1]]$access_points[[1]]$url <- url
  value
}

missing_path <- tempfile("missing_guide_resource_registry_", tmpdir = tempdir(), fileext = ".json")
assert_error(pt_guide_read_resource_registry(missing_path), "Missing canonical Guide Resource registry",
             "Missing registry did not fail clearly")
malformed_path <- tempfile("malformed_guide_resource_registry_", tmpdir = tempdir(), fileext = ".json")
writeLines("{", malformed_path, useBytes = TRUE)
assert_error(pt_guide_read_resource_registry(malformed_path), "Malformed canonical Guide Resource registry",
             "Malformed registry did not fail clearly")

bad <- fresh_registry()
bad$schema_version <- 1L
expect_invalid(bad, "schema_version 2", "Unsupported schema version was accepted")
bad <- fresh_registry()
bad$resources[[1]]$summary <- NULL
expect_invalid(bad, "missing required field", "Missing required Resource field was accepted")
bad <- fresh_registry()
bad$resources[[1]]$unexpected_field <- "unexpected"
expect_invalid(bad, "unsupported field", "Unknown Resource field was accepted")
bad <- fresh_registry()
bad$resources[[1]]$providers[[1]]$unexpected_field <- "unexpected"
expect_invalid(bad, "unsupported field", "Unknown nested provider field was accepted")

bad <- fresh_registry()
bad$resources[[2]]$id <- bad$resources[[1]]$id
expect_invalid(bad, "globally unique", "Duplicate primary Resource ID was accepted")
bad <- fresh_registry()
bad$resources[[1]]$id <- "Resource_Invalid"
expect_invalid(bad, "resource_* stable-ID syntax", "Invalid primary Resource ID syntax was accepted")
bad <- fresh_registry()
bad$resources[[1]]$aliases <- list(expected_published_ids[[2]])
expect_invalid(bad, "globally unique", "Final alias collision with a primary ID was accepted")
bad <- fresh_registry()
bad$resources[[1]]$aliases <- list("invalid alias")
expect_invalid(bad, "aliases must be unique resource_* stable IDs", "Invalid final alias syntax was accepted")
bad <- fresh_registry()
bad$resources[[1]]$migration_aliases <- list(migration_aliases[[1]])
expect_invalid(bad, "migration aliases must be globally unique", "Duplicate migration alias was accepted")
bad <- fresh_registry()
bad$resources[[1]]$migration_aliases <- list("candidate.invalid")
expect_invalid(bad, "reviewed res.* intake keys", "Invalid migration alias namespace was accepted")
bad <- fresh_registry()
bad$resources[[10]]$search_aliases <- list("duplicate", "duplicate")
expect_invalid(bad, "search_aliases must be unique", "Duplicate search alias was accepted")
bad <- fresh_registry()
bad$resources[[2]]$order <- 1L
expect_invalid(bad, "order must be unique, complete", "Duplicate explicit order was accepted")
bad <- fresh_registry()
bad$resources[[33]]$order <- 34L
expect_invalid(bad, "order must be unique, complete", "Incomplete explicit order was accepted")
bad <- fresh_registry()
bad$resources <- bad$resources[c(2, 1, 3:33)]
expect_invalid(bad, "order must be unique, complete", "File order inconsistent with explicit order was accepted")

controlled_failures <- list(
  resource_type = list(index = 1L, value = "website", pattern = "uncontrolled Resource type"),
  resource_granularity = list(index = 1L, value = "national", pattern = "uncontrolled Resource type"),
  publication_state = list(index = 1L, value = "reviewed", pattern = "uncontrolled publication_state"),
  access_class = list(index = 1L, value = "restricted", pattern = "access_class must be public")
)
for (field in names(controlled_failures)) {
  case <- controlled_failures[[field]]
  bad <- fresh_registry()
  bad$resources[[case$index]][[field]] <- case$value
  expect_invalid(bad, case$pattern, paste0("Uncontrolled ", field, " was accepted"))
}
bad <- fresh_registry()
bad$resources[[10]]$subject_tags <- list("Remote Sensing")
expect_invalid(bad, "uncontrolled or duplicate subject", "Uncontrolled subject was accepted")
bad <- fresh_registry()
bad$resources[[10]]$information_type_tags <- list("Imagery")
expect_invalid(bad, "uncontrolled or duplicate Information Type", "Uncontrolled Information Type was accepted")
bad <- fresh_registry()
bad$resources[[10]]$providers[[1]]$role <- "owner"
expect_invalid(bad, "uncontrolled role", "Uncontrolled provider role was accepted")
bad <- fresh_registry()
bad$resources[[10]]$providers[[2]] <- bad$resources[[10]]$providers[[1]]
expect_invalid(bad, "unique provider (name, role) pairs", "Duplicate provider name/role pair was accepted")
bad <- fresh_registry()
bad$resources[[10]]$providers <- list()
expect_invalid(bad, "requires a providers array", "Missing provider structure was accepted")
bad <- fresh_registry()
bad$resources[[10]]$access_points[[1]]$role <- "download"
expect_invalid(bad, "uncontrolled role", "Uncontrolled access-point role was accepted")
bad <- fresh_registry()
bad$resources[[10]]$access_points[[1]]$label <- ""
expect_invalid(bad, "nonblank trimmed string", "Blank access-point label was accepted")
bad <- fresh_registry()
bad$resources[[10]]$access_points[[2]]$url <- bad$resources[[10]]$access_points[[1]]$url
expect_invalid(bad, "unique access-point URLs", "Duplicate access-point URL was accepted")
bad <- fresh_registry()
bad$resources[[1]]$access_points[[1]]$url <- "https://www.doi.gov/other"
expect_invalid(bad, "matching canonical_url", "Canonical access-point mismatch was accepted")
bad <- fresh_registry()
bad$resources[[10]]$geographic_scope$scope_type <- "regional"
expect_invalid(bad, "uncontrolled or inconsistent geographic scope",
               "Uncontrolled geographic scope was accepted")
bad <- fresh_registry()
bad$resources[[10]]$geographic_scope$names <- list("United States", "United States")
expect_invalid(bad, "uncontrolled or inconsistent geographic scope",
               "Duplicate geographic names were accepted")
bad <- fresh_registry()
bad$resources[[1]]$geographic_scope$names <- list("United States")
expect_invalid(bad, "uncontrolled or inconsistent geographic scope",
               "Named unknown geographic scope was accepted")
bad <- fresh_registry()
bad$resources[[10]]$geographic_scope$names <- list()
expect_invalid(bad, "uncontrolled or inconsistent geographic scope",
               "Unnamed controlled geographic scope was accepted")

url_cases <- list(
  "http://example.gov/data" = "public https:// URL",
  "https://localhost/data" = "local, private, or restricted host",
  "https://127.0.0.1/data" = "local, private, or restricted host",
  "https://10.2.3.4/data" = "local, private, or restricted host",
  "https://internal.example.gov/data" = "local, private, or restricted host",
  "https://user:secret@example.gov/data" = "URL credentials",
  "https://example.gov/data?token=secret" = "signed-query material",
  "https://example.gov/data?X-Amz-Signature=secret" = "signed-query material",
  "https://example.gov/data?AWSAccessKeyId=secret" = "signed-query material"
)
for (url in names(url_cases)) {
  expect_invalid(
    with_primary_url(fresh_registry(), url), url_cases[[url]],
    paste0("Unsafe or restricted URL was accepted: ", url)
  )
}
bad <- fresh_registry()
bad$resources[[1]]$summary <- "Local review file: /Users/example/intake/bookmarks.xlsx"
expect_invalid(bad, "machine-local filesystem path", "Machine-local path was accepted")
bad <- fresh_registry()
bad$resources[[1]]$summary <- "password=example-secret"
expect_invalid(bad, "credentials or secret material", "Credential-like material was accepted")

prohibited_fields <- c(
  "raw_bookmark_id", "source_record_id", "candidate_id", "provenance_note",
  "relatedProductIds", "product_relationships", "profile_id", "lifecycle_status", "freshness"
)
for (field in prohibited_fields) {
  bad <- fresh_registry()
  bad$resources[[1]][[field]] <- "not allowed"
  expect_invalid(
    bad, "prohibited authority/provenance field",
    paste0("Prohibited registry field was accepted: ", field)
  )
}

query_url <- "https://data.example.gov/view?theme=public&sector=water&ltmpl=wide"
query_registry <- with_primary_url(fresh_registry(), query_url)
query_path <- write_registry_fixture(query_registry)
query_round_trip <- pt_guide_read_resource_registry(query_path)[[1]]
assert_identical(query_round_trip$canonical_url, query_url,
                 "Canonical URL query components were decoded or corrupted")
assert_identical(query_round_trip$access_points[[1]]$url, query_url,
                 "Access-point URL query components were decoded or corrupted")
assert_true(grepl("&sector=", query_round_trip$canonical_url, fixed = TRUE) &&
              grepl("&ltmpl=", query_round_trip$canonical_url, fixed = TRUE),
            "Literal &sector= or &ltmpl= regression component did not round-trip")

reference_registry <- fresh_registry()
reference_registry$resources[[1]]$public_source_references <- list(list(
  url = query_url, role = "official_source"
))
reference_path <- write_registry_fixture(reference_registry)
reference_round_trip <- pt_guide_read_resource_registry(reference_path)[[1]]
assert_identical(reference_round_trip$public_source_references[[1]]$url, query_url,
                 "Public source-reference URL query was decoded or corrupted")
bad <- fresh_registry()
bad$resources[[1]]$public_source_references <- list(list(
  url = "https://example.gov/data?sig=secret", role = "official_source"
))
expect_invalid(bad, "signed-query material", "Signed public source reference was accepted")

bad_relationships <- pt_guide_resource_product_relationships()
bad_relationships$resource_doi <- NULL
assert_error(
  pt_guide_resource_browser_records(published, bad_relationships),
  "explicit compiler relationship reconciliation",
  "Published registry/relationship drift bypassed explicit compiler reconciliation"
)

enrichment <- pt_guide_read_product_enrichment()
scan_enrichment <- enrichment["ops_scan_soil_moisture"]
scan_relationship <- scan_enrichment[[1]]$resource_relationships[[1]]
assert_identical(
  scan_relationship,
  list(
    id = "resource_nrcs_scan",
    role = "Observation network and official station context",
    relationship_type = "used_by_brim",
    use_scope = "observation_source_and_station_context"
  ),
  "The staged SCAN relationship changed"
)
scan_product <- list(
  id = "ops_scan_soil_moisture", summary = "", accessHint = "",
  subjectTags = character(0), subject = "", informationTypes = "Live Observation",
  mode = "Live Observation", searchTerms = character(0), sections = list(),
  relatedArticleIds = character(0), relatedResourceIds = character(0),
  relatedResources = list(), contentTier = "STRUCTURED_BASIC"
)
scan_applied <- pt_guide_apply_product_enrichment(
  list(scan_product), scan_enrichment,
  resource_registry = registry,
  product_universe_ids = "ops_scan_soil_moisture"
)[[1]]
assert_true(!length(scan_applied$relatedResources) &&
              !length(scan_applied$relatedResourceIds),
            "The staged SCAN relationship was not pruned before Product projection")
bad_scan_enrichment <- scan_enrichment
bad_scan_enrichment[[1]]$resource_relationships[[1]]$id <- "resource_unavailable"
assert_error(
  pt_guide_apply_product_enrichment(
    list(scan_product), bad_scan_enrichment,
    resource_registry = registry,
    product_universe_ids = "ops_scan_soil_moisture"
  ),
  "unavailable Resource",
  "An unknown relationship Resource ID was accepted before pruning"
)
assert_error(
  pt_guide_apply_product_enrichment(
    list(scan_product), scan_enrichment,
    resource_registry = registry,
    product_universe_ids = "different_product"
  ),
  "exact unique Product universe",
  "A relationship Product outside the exact Product universe was accepted"
)

cat("GUIDE-I2B-R4 schema-v2 staged Resource registry contracts passed.\n")
cat("SCHEMA_VERSION=2\n")
cat("TOTAL_RESOURCES=33\n")
cat("PUBLISHED_RESOURCES=9\n")
cat("STAGED_RESOURCES=24\n")
cat("PUBLISHED_IDS_AND_ORDER=EXACT_CURRENT_NINE\n")
cat("STAGED_IDS_AND_ORDER=EXACT_R3_LIST\n")
cat("CURRENT_BROWSER_RESOURCE_BYTES=", nchar(browser_json, type = "bytes"), "\n", sep = "")
cat("CURRENT_BROWSER_RESOURCE_SHA256=",
    digest::digest(browser_json, algo = "sha256", serialize = FALSE), "\n", sep = "")
cat("CURRENT_BROWSER_RESOURCE_SHAPE=kind,id,title,provider,summary,url,relatedProductIds\n")
cat("STAGED_BROWSER_LEAKAGE=0\n")
cat("DEFAULT_PROFILE_ONLY=YES\n")
cat("STAGED_RELATIONSHIP_BROWSER_LEAKAGE=0\n")
