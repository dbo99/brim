#!/usr/bin/env Rscript

# Focused source-only GUIDE-I1 contracts. No map build, cache mutation,
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

assert_identical(names(pt_guide_supported_profiles()), "default",
                 "Guide must name only the one actual current build profile")
assert_identical(bundle$profileId, "default", "Default profile ID changed")
assert_identical(bundle$counts$products, 280L,
                 "Current default build should derive 280 included Products")
assert_identical(sum(product_subsystems == "External Layers"), 175L,
                 "External visible Product projection changed")
assert_identical(sum(product_subsystems == "Ops Live"), 47L,
                 "Ops Live active Product projection changed")
assert_identical(sum(product_subsystems == "Tools"), 5L,
                 "Tools Product projection changed")
assert_identical(sum(product_subsystems == "Basemaps / Local Layers"), 53L,
                 "Local plus basemap Product projection changed")
assert_true(!anyDuplicated(product_ids) && all(nzchar(product_ids)),
            "Every included Product needs one unique stable ID")
assert_true(!anyDuplicated(product_paths) && all(nzchar(product_paths)),
            "Every included Product needs one unique exact path")
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

markers <- pt_guide_descriptive_markers()
without_enrichment <- c(
  pt_guide_local_products(runtime_groups, list()),
  pt_guide_external_products(catalog_markers = list()),
  pt_guide_ops_products(MAP_DISPLAY, list()),
  pt_guide_basemap_products(),
  pt_guide_tool_products(MAP_DISPLAY)
)
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

quick_product_ids <- unlist(lapply(bundle$quickAccess, `[[`, "productIds"), use.names = FALSE)
assert_true(all(quick_product_ids %in% product_ids),
            "Quick Access references an unavailable Product")
assert_identical(vapply(bundle$quickAccess, `[[`, character(1), "label"),
                 c("HUC8 watersheds", "Groundwater basins", "Fire Perimeters"),
                 "Verified Quick Access configuration changed")
fire <- bundle$quickAccess[[which(vapply(bundle$quickAccess, `[[`, character(1), "id") == "quick_fire_perimeters")]]
assert_identical(fire$productIds, c("EXT070", "EXT074"),
                 "Fire Perimeters collection must use explicit stable IDs")

duplicate_id <- bundle
duplicate_id$products[[2]]$id <- duplicate_id$products[[1]]$id
assert_error(pt_validate_guide_bundle(duplicate_id),
             "record IDs must be nonblank and unique",
             "Duplicate Guide IDs did not fail validation")
duplicate_path <- bundle
duplicate_path$products[[2]]$pathLabel <- duplicate_path$products[[1]]$pathLabel
assert_error(pt_validate_guide_bundle(duplicate_path),
             "Product paths must be nonblank and unique",
             "Duplicate Guide paths did not fail validation")

excluded <- c(
  "huc8", "EXT070", "resource_usgs_water_dashboard",
  "article_getting_started", "update_guide_foundation", "quick_groundwater_basins"
)
projected <- pt_project_guide_bundle(bundle, excluded, profile_id = "synthetic_projection_test")
pt_validate_guide_bundle(projected)
projected_json <- jsonlite::toJSON(projected, auto_unbox = TRUE, null = "null", na = "null")
for (probe in excluded) {
  assert_true(!grepl(probe, projected_json, fixed = TRUE),
              paste("Profile exclusion leaked into embedded payload:", probe))
}
assert_true(!grepl("quick_huc8|quick_fire_perimeters", projected_json, perl = TRUE),
            "Unavailable Quick Access items leaked after Product projection")
assert_identical(projected$counts$products, bundle$counts$products - 2L,
                 "Projected Product count was not generated after exclusion")
retained_ids <- setdiff(product_ids, c("huc8", "EXT070"))
assert_identical(vapply(projected$products, `[[`, character(1), "id"), retained_ids,
                 "Stable IDs/order changed for retained Products")
assert_true(all(!vapply(projected$products, function(product) {
  "resource_usgs_water_dashboard" %in% product$relatedResourceIds
}, logical(1))), "Excluded Resource relationships leaked into Products")

guide_js <- paste(readLines(file.path("03_functions", "js", "leaflet_brim_guide.js"), warn = FALSE), collapse = "\n")
guide_css <- paste(readLines(file.path("03_functions", "css", "leaflet_brim_guide.css"), warn = FALSE), collapse = "\n")
guide_r <- paste(readLines(file.path("03_functions", "leaflet_guide_helpers.r"), warn = FALSE), collapse = "\n")
loading_r <- paste(readLines(file.path("03_functions", "leaflet_loading_helpers.r"), warn = FALSE), collapse = "\n")
map_r <- paste(readLines(file.path("05_map_build", "04_build_portatreasure2_core_map.r"), warn = FALSE), collapse = "\n")

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
              grepl("asArray(item.productIds)", guide_js, fixed = TRUE),
            "Singleton Guide payload fields are not normalized at the browser boundary")
assert_true(grepl("preventScroll: true", guide_js, fixed = TRUE) &&
              grepl("main.scrollTop = desiredScroll", guide_js, fixed = TRUE),
            "Guide focus can displace the current mobile view")
assert_true(grepl("open_legacy_notes", guide_js, fixed = TRUE) &&
              grepl("pt-map-notes-btn", guide_js, fixed = TRUE),
            "Legacy Notes is not reachable from Guide")
assert_true(grepl("renderResults(searchResults", guide_js, fixed = TRUE) &&
              grepl("renderResults(filtered", guide_js, fixed = TRUE),
            "Search and browse do not share one result renderer")
assert_true(grepl("brim-guide__close--left", guide_js, fixed = TRUE) &&
              grepl("brim-guide__close--right", guide_js, fixed = TRUE) &&
              grepl(".brim-guide__close--left", guide_css, fixed = TRUE),
            "Accepted desktop/mobile close treatment is incomplete")
assert_true(grepl("record.title], 1200", guide_js, fixed = TRUE) &&
              grepl("aliases, 1100", guide_js, fixed = TRUE) &&
              grepl("path, 800", guide_js, fixed = TRUE) &&
              grepl("record.subject], 650", guide_js, fixed = TRUE) &&
              grepl("record.mode], 600", guide_js, fixed = TRUE),
            "Deterministic V4 search weight ordering changed")
score_source <- sub("^[\\s\\S]*?function scoreRecord", "function scoreRecord", guide_js, perl = TRUE)
score_source <- sub("function search[\\s\\S]*$", "", score_source, perl = TRUE)
assert_true(!grepl("record\\.(id|url)", score_source, perl = TRUE),
            "Runtime IDs or URLs entered the semantic search index")
record_kinds <- unique(vapply(
  c(bundle$products, bundle$articles, bundle$resources, bundle$updates),
  `[[`, character(1), "kind"
))
assert_identical(record_kinds, c("Product", "Article", "Resource", "Update"),
                 "Search/result record types are not distinguishable")

accepted_description <- paste0(
  "A hydrology-centered browser map for water-resource screening, ",
  "live conditions, and resource-review support."
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
js_bytes <- file.info(file.path("03_functions", "js", "leaflet_brim_guide.js"))$size
css_bytes <- file.info(file.path("03_functions", "css", "leaflet_brim_guide.css"))$size
assert_true(payload_bytes <= 400000L, "Default Guide payload exceeds hard review threshold")
assert_true((js_bytes + css_bytes) <= 200000L, "Guide JS + CSS exceeds hard review threshold")
assert_true(!grepl("/(Users|home|private|tmp|Volumes)/", projected_json, perl = TRUE),
            "Machine-local path leaked into Guide payload")
assert_true(!grepl("prototype diagnostic|editorial review|source_file", projected_json,
                   ignore.case = TRUE, perl = TRUE),
            "Prototype/source/audit diagnostics leaked into Guide payload")

cat("GUIDE-I1 foundation contracts passed.\n")
cat("PROFILE_ID=default\n")
cat("PRODUCTS=", bundle$counts$products, "\n", sep = "")
cat("ARTICLES=", bundle$counts$articles, "\n", sep = "")
cat("RESOURCES=", bundle$counts$resources, "\n", sep = "")
cat("UPDATES=", bundle$counts$updates, "\n", sep = "")
cat("QUICK_ACCESS=", bundle$counts$quickAccess, "\n", sep = "")
cat("EMBEDDED_PAYLOAD_BYTES=", payload_bytes, "\n", sep = "")
cat("GUIDE_JS_BYTES=", js_bytes, "\n", sep = "")
cat("GUIDE_CSS_BYTES=", css_bytes, "\n", sep = "")
