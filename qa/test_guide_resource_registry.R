#!/usr/bin/env Rscript

# Focused source-only contracts for the canonical Guide Resource registry.
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

expected_ids <- c(
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
required_fields <- c(
  "id", "aliases", "order", "title", "providers", "summary", "canonical_url",
  "access_points", "resource_type", "resource_granularity", "public_source_references"
)

assert_identical(raw_registry$schema_version, 1L, "Registry schema/version marker changed")
assert_identical(length(registry), 9L, "Registry must contain exactly nine primary Resources")
registry_ids <- vapply(registry, `[[`, character(1), "id")
assert_identical(registry_ids, expected_ids, "Registry Resource ID set/order changed")
assert_true(!anyDuplicated(registry_ids), "Registry primary IDs are not unique")
assert_true(all(vapply(registry, function(record) identical(names(record), required_fields), logical(1))),
            "Registry required/allowed field contract changed")
registry_aliases <- unlist(lapply(registry, function(record) {
  unname(as.character(unlist(record$aliases, use.names = FALSE)))
}), use.names = FALSE)
assert_true(!length(registry_aliases), "Unreviewed Resource aliases entered the foundation registry")
assert_true(!anyDuplicated(c(registry_ids, registry_aliases)),
            "Registry aliases collide with primary IDs or other aliases")
registry_order <- as.integer(vapply(registry, `[[`, numeric(1), "order"))
assert_identical(registry_order, seq_along(registry),
                 "Registry explicit order is not unique, complete, and in file order")
assert_true(all(vapply(registry, function(record) {
  identical(record$resource_type, "unknown") &&
    identical(record$resource_granularity, "unknown") &&
    length(record$providers) == 1L &&
    identical(record$providers[[1]]$role, "display_provider") &&
    length(record$access_points) == 1L &&
    identical(record$access_points[[1]]$role, "canonical") &&
    identical(record$access_points[[1]]$url, record$canonical_url) &&
    !length(record$public_source_references)
}, logical(1))), "Foundation records do not use the conservative controlled metadata contract")

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
browser_records <- pt_guide_resource_browser_records(registry)
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
bad$schema_version <- 2L
expect_invalid(bad, "schema_version 1", "Unsupported schema version was accepted")
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
bad$resources[[1]]$aliases <- list(expected_ids[[2]])
expect_invalid(bad, "globally unique", "Alias collision with a primary ID was accepted")
bad <- fresh_registry()
bad$resources[[1]]$aliases <- list("resource_shared_alias")
bad$resources[[2]]$aliases <- list("resource_shared_alias")
expect_invalid(bad, "globally unique", "Duplicate aliases across Resources were accepted")
bad <- fresh_registry()
bad$resources[[1]]$aliases <- list("invalid alias")
expect_invalid(bad, "aliases must be unique resource_* stable IDs", "Invalid alias syntax was accepted")
bad <- fresh_registry()
bad$resources[[2]]$order <- 1L
expect_invalid(bad, "order must be unique, complete", "Duplicate explicit order was accepted")
bad <- fresh_registry()
bad$resources[[9]]$order <- 10L
expect_invalid(bad, "order must be unique, complete", "Incomplete explicit order was accepted")
bad <- fresh_registry()
bad$resources <- bad$resources[c(2, 1, 3:9)]
expect_invalid(bad, "order must be unique, complete", "File order inconsistent with explicit order was accepted")

bad <- fresh_registry()
bad$resources[[1]]$resource_type <- "website"
expect_invalid(bad, "uncontrolled Resource type", "Uncontrolled Resource type was accepted")
bad <- fresh_registry()
bad$resources[[1]]$resource_granularity <- "national"
expect_invalid(bad, "uncontrolled Resource type", "Uncontrolled Resource granularity was accepted")
bad <- fresh_registry()
bad$resources[[1]]$providers[[1]]$role <- "owner"
expect_invalid(bad, "uncontrolled role", "Uncontrolled provider role was accepted")
bad <- fresh_registry()
bad$resources[[1]]$providers <- list()
expect_invalid(bad, "requires a providers array", "Missing provider structure was accepted")
bad <- fresh_registry()
bad$resources[[1]]$access_points[[1]]$role <- "download"
expect_invalid(bad, "uncontrolled role", "Uncontrolled access-point role was accepted")
bad <- fresh_registry()
bad$resources[[1]]$access_points[[1]]$url <- "https://www.doi.gov/other"
expect_invalid(bad, "matching canonical_url", "Canonical access-point mismatch was accepted")

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
  pt_guide_resource_browser_records(registry, bad_relationships),
  "explicit compiler relationship reconciliation",
  "Registry expansion or relationship drift bypassed explicit compiler reconciliation"
)

cat("GUIDE-I2B-R2 Resource registry contracts passed.\n")
cat("SCHEMA_VERSION=1\n")
cat("RESOURCES=", length(registry), "\n", sep = "")
cat("ALIASES=", length(registry_aliases), "\n", sep = "")
cat("BROWSER_RESOURCE_BYTES=", nchar(browser_json, type = "bytes"), "\n", sep = "")
cat("BROWSER_RESOURCE_SHA256=",
    digest::digest(browser_json, algo = "sha256", serialize = FALSE), "\n", sep = "")
