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

assert_identical(names(pt_guide_supported_profiles()), "default",
                 "Guide must name only the one actual current build profile")
assert_identical(bundle$profileId, "default", "Default profile ID changed")
assert_identical(bundle$schemaVersion, 2L, "Structured Guide schema version changed")
assert_identical(
  bundle$authority$coverage,
  "ALL_INCLUDED_VISIBLE_PRODUCTS_WITH_CURATED_QUICK_ACCESS_CONTENT_FLOOR",
  "Guide coverage authority no longer declares the curated Quick Access floor"
)
assert_identical(bundle$counts$products, 280L,
                 "Current default build should derive 280 included Products")
assert_identical(bundle$counts$articles, 7L,
                 "Current Guide must include the seven maintained Methods")
assert_identical(bundle$counts$resources, 9L,
                 "Current Guide must include the nine maintained Resources")
assert_identical(bundle$counts$updates, 3L,
                 "Current Guide must include the three verified Updates")
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
    "SCAN Soil-Moisture Statistical Context",
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
                 c("HUC8 – PRISM/BCMv8", "Groundwater Basins – Bulletin 118", "Fire Perimeters"),
                 "Verified Quick Access configuration changed")
fire <- bundle$quickAccess[[which(vapply(bundle$quickAccess, `[[`, character(1), "id") == "quick_fire_perimeters")]]
assert_identical(fire$productIds, c("EXT070", "EXT074"),
                 "Fire Perimeters collection must use explicit stable IDs")
assert_true(all(vapply(bundle$quickAccess, function(item) nzchar(item$summary), logical(1))),
            "Every curated Quick Access entry requires a useful summary")
assert_true(grepl("complementary perimeter Products", fire$summary, fixed = TRUE) &&
              grepl("coverage and currency differ", fire$summary, fixed = TRUE),
            "Fire Perimeters collection does not explain why its two Products differ")

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

huc8 <- record_by_id(bundle$products, "huc8")
assert_identical(huc8$contentTier, "curated", "HUC8 lost curated Guide detail")
assert_true(nzchar(huc8$summary) && length(huc8$sections) == 6L,
            "HUC8 curated summary/section floor is incomplete")
assert_identical(
  section_by_id(huc8, "huc8_display_modes")$items,
  unname(vapply(PT_HUC_THEME_REGISTRY, `[[`, character(1), "label")),
  "HUC8 display modes drifted from current theme authority"
)
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
  c("resource_prism_normals", "resource_usgs_bcmv8", "resource_blm_california"),
  "HUC8 Resource relationships changed"
)
assert_identical(
  vapply(huc8$relatedResources, `[[`, character(1), "role"),
  c("Precipitation source", "Recharge model and source", "BLM program context"),
  "HUC8 Resource roles changed"
)
prism_resource <- record_by_id(bundle$resources, "resource_prism_normals")
bcm_resource <- record_by_id(bundle$resources, "resource_usgs_bcmv8")
assert_identical(prism_resource$url, "https://prism.oregonstate.edu/normals/",
                 "HUC8 PRISM link changed")
assert_identical(bcm_resource$url, "https://www.sciencebase.gov/catalog/item/5f29c62d82cef313ed9edb39",
                 "HUC8 BCMv8 ScienceBase link changed")

bulletin118 <- record_by_id(bundle$products, "gw_bull118")
assert_identical(bulletin118$contentTier, "curated", "Bulletin 118 lost curated Guide detail")
assert_true(nzchar(bulletin118$summary) && length(bulletin118$sections) == 5L,
            "Bulletin 118 curated summary/section floor is incomplete")
assert_identical(
  section_by_id(bulletin118, "bulletin118_display_modes")$items,
  c("Basins only", "DWR SGMA 2019 Basin Prioritization", "BLM-managed land — %"),
  "Bulletin 118 display modes changed"
)
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
fire_current <- record_by_id(bundle$products, "EXT074")
assert_true(all(c(fire_recent$contentTier, fire_current$contentTier) == "curated"),
            "Fire Perimeters Products lost curated Guide detail")
assert_true(grepl("CAL FIRE", section_text(fire_recent), fixed = TRUE) &&
              grepl("current-view WFIGS", section_text(fire_current), fixed = TRUE) &&
              grepl("prescribed-fire records are excluded", section_text(fire_current), fixed = TRUE),
            "Fire Products do not retain distinct source, processing, and limitation content")
assert_identical(fire_recent$relatedResourceIds, "resource_calfire_fire_perimeters",
                 "CAL FIRE perimeter Resource relationship changed")
assert_identical(fire_current$relatedResourceIds, "resource_nifc_wfigs_current",
                 "NIFC current perimeter Resource relationship changed")

generalization_method <- record_by_id(bundle$articles, "method_display_geometry_generalization")
generalization_table <- section_by_id(generalization_method, "current_portfolio")$table
assert_identical(length(generalization_table$rows), 27L,
                 "Generalization Method must expose all 27 current public disclosure rows")
assert_true(all(vapply(generalization_table$rows, function(row) {
  nzchar(row$layer) && nzchar(row$parameter) && nzchar(row$disclosure)
}, logical(1))), "Generalization Method table contains an incomplete public row")

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
  "method_how_brim_works", "update_read_only_layer_explorer", "quick_groundwater_basins"
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
assert_true(all(!vapply(projected$products, function(product) {
  "method_how_brim_works" %in% product$relatedArticleIds
}, logical(1))), "Excluded Method relationships leaked into Products")
assert_true(all(vapply(projected$products, function(product) {
  identical(
    unname(product$relatedResourceIds),
    unname(vapply(product$relatedResources, `[[`, character(1), "id"))
  )
}, logical(1))), "Projected role-labeled Resource relationships became inconsistent")

guide_js <- paste(readLines(file.path("03_functions", "js", "leaflet_brim_guide.js"), warn = FALSE), collapse = "\n")
guide_css <- paste(readLines(file.path("03_functions", "css", "leaflet_brim_guide.css"), warn = FALSE), collapse = "\n")
guide_r <- paste(readLines(file.path("03_functions", "leaflet_guide_helpers.r"), warn = FALSE), collapse = "\n")
loading_r <- paste(readLines(file.path("03_functions", "leaflet_loading_helpers.r"), warn = FALSE), collapse = "\n")
map_r <- paste(readLines(file.path("05_map_build", "04_build_portatreasure2_core_map.r"), warn = FALSE), collapse = "\n")
panel_js <- paste(readLines(file.path("03_functions", "js", "leaflet_tools_adddata_panel.js"), warn = FALSE), collapse = "\n")

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
assert_true(!grepl("legacy_notes|open_legacy_notes|openPtNotes|pt-map-notes|map-notes-overlay",
                   paste(guide_js, guide_r, map_r, panel_js), ignore.case = TRUE, perl = TRUE),
            "Retired Notes runtime content or wiring remains in the current build")
assert_true(grepl("pt-map-guide-btn", map_r, fixed = TRUE) &&
              grepl("Open BRIM Guide", map_r, fixed = TRUE) &&
              grepl("guideButton.textContent = 'Guide'", map_r, fixed = TRUE),
            "Upper-left Guide control identity is incomplete")
assert_identical(length(gregexpr("window.BRIM_GUIDE.open(guideButton)", map_r, fixed = TRUE)[[1]]), 1L,
                 "Current map build must contain exactly one primary Guide opener")
assert_true(!grepl("pt-layer-explorer-btn|window.BRIM_GUIDE.open", panel_js, perl = TRUE),
            "External Layers retains a duplicate Guide entry or handler")
assert_true(grepl("searchResults", guide_js, fixed = TRUE) &&
              grepl("filtered", guide_js, fixed = TRUE) &&
              grepl("function renderResults", guide_js, fixed = TRUE),
            "Search and browse do not share one result renderer")
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
              grepl("products.slice(0, 10)", guide_js, fixed = TRUE),
            "Product locator or initial compact Product index is missing")
assert_true(grepl("function renderStructuredSections", guide_js, fixed = TRUE) &&
              grepl("product.sections", guide_js, fixed = TRUE) &&
              grepl("article.title", guide_js, fixed = TRUE) &&
              grepl("relationship.role", guide_js, fixed = TRUE) &&
              grepl("quickDefinition.summary", guide_js, fixed = TRUE),
            "Generic structured content, Method, Resource-role, or Quick summary rendering is incomplete")
assert_true(!grepl("huc8|gw_bull118|EXT070|EXT074|PRISM/BCMv8|Bulletin 118", guide_js,
                   perl = TRUE),
            "Product-specific Guide content leaked into the generic browser renderer")
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
