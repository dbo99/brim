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
assert_identical(sum(product_subsystems == "Basemaps / Local Layers"), 43L,
                 "Local Product projection changed")
product_entity_types <- vapply(bundle$products, `[[`, character(1), "entityType")
assert_identical(sum(product_entity_types == "Layer"), 265L,
                 "Guide layer entity projection changed")
assert_identical(sum(product_entity_types == "Tool"), 5L,
                 "Guide tool entity projection changed")
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

scan <- record_by_id(bundle$products, "ops_scan_soil_moisture")
assert_identical(scan$title, "SCAN Soil Moisture",
                 "SCAN Guide display title changed")
assert_identical(scan$subject, "Soil Moisture",
                 "SCAN lacks the explicit Soil Moisture subject tag")
assert_true(grepl("Soil moisture | USDA NRCS SCAN | Ca/Nv", scan$pathLabel, fixed = TRUE),
            "SCAN exact runtime path was not preserved")

how_brim_works <- record_by_id(bundle$articles, "method_how_brim_works")
how_brim_text <- section_text(how_brim_works)
assert_true(all(vapply(
  c("LOCAL LAYERS", "OPS LIVE", "EXTERNAL LAYERS", "TOOLS",
    "sources, preparation, timing, and limitations"),
  function(probe) grepl(probe, how_brim_text, fixed = TRUE),
  logical(1)
)), "How BRIM Works does not explain the four practical layer/tool families")
assert_true(!grepl("PRISM|BCMv8|Guide I1|read-only index|compiler|implementation foundation",
                   how_brim_text, ignore.case = TRUE, perl = TRUE),
            "How BRIM Works contains HUC-specific or implementation-facing language")

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
assert_true(all(c(external_tool$contentTier, local_tool$contentTier) == "SOURCE_BACKED_RICH"),
            "Upload tools lost source-backed structured Guide content")
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

enrichment <- pt_guide_read_product_enrichment()
assert_identical(length(enrichment), 21L,
                 "GUIDE-I2A enrichment inventory changed without review")
assert_true(all(vapply(enrichment, function(record) {
  length(record$source_refs) > 0L &&
    all(nzchar(as.character(unlist(record$source_refs, use.names = FALSE))))
}, logical(1))), "Every enriched field set must identify current source authority")
assert_true("brim_mapped_conveyance" %in% names(enrichment) &&
              "brim_mapped_conveyance" %in% product_ids &&
              "brim_mapped_conveyance" %in% quick_product_ids &&
              !"major_conveyance" %in% product_ids,
            "Quick Access must use the current BRIM-mapped conveyance Product, not its legacy source layer")

content_tiers <- table(vapply(bundle$products, `[[`, character(1), "contentTier"))
assert_identical(as.integer(content_tiers[c("SOURCE_BACKED_RICH", "STRUCTURED_BASIC")]),
                 c(21L, 249L), "GUIDE-I2A content-tier counts changed")
assert_true(!"EDITORIAL_REVIEW_REQUIRED" %in% names(content_tiers),
            "Unexpected editorial-review tier entered the current profile")
assert_true(all(vapply(bundle$products, function(product) {
  all(nzchar(c(product$id, product$title, product$entityType, product$brimSection,
               product$pathLabel, product$provider))) &&
    length(product$subjectTags) > 0L && length(product$informationTypes) > 0L
}, logical(1))), "A Product missed the structured minimum content floor")
assert_true(!any(vapply(bundle$products, function(product) {
  nzchar(product$summary) && grepl(
    "This Product is available in BRIM|may be useful for analysis|See the map for more information",
    product$summary, ignore.case = TRUE, perl = TRUE
  )
}, logical(1))), "A generic filler summary entered the Product inventory")

allowed_information_types <- c(
  "Static Reference", "Live Observation", "Forecast / Outlook",
  "Historical Context", "Screening / Derived", "External On-Demand Service",
  "Tool / Workflow"
)
all_information_types <- unique(unlist(lapply(bundle$products, `[[`, "informationTypes"), use.names = FALSE))
assert_true(all(all_information_types %in% allowed_information_types) &&
              !any(grepl("Data / Guidance Mode|Guidance Method|Interactive workflow|Static reference",
                         all_information_types, fixed = FALSE, perl = TRUE)),
            "Information Type vocabulary is not user-facing or controlled")
assert_true(any(vapply(bundle$products, function(product) length(product$subjectTags) > 1L, logical(1))),
            "Explicit taxonomy does not support multiple subject tags")

fire_weather_ids <- vapply(Filter(function(product) {
  "Fire Weather" %in% product$subjectTags
}, bundle$products), `[[`, character(1), "id")
assert_true(setequal(fire_weather_ids, c("EXT066", "EXT067", "EXT068", "EXT069", "EXT071", "EXT073", "EXT075", "EXT076")),
            "Fire Weather taxonomy does not match the structured External subgroup")
assert_true(all(vapply(Filter(function(product) product$id %in% fire_weather_ids, bundle$products), function(product) {
  all(c("Fire Weather", "Weather & Forecasts") %in% product$subjectTags)
}, logical(1))), "Fire-weather outlooks lost multi-tag Weather & Forecasts membership")
matches_filters <- function(product, brim_sections = character(0), subjects = character(0),
                            information_types = character(0)) {
  (!length(brim_sections) || product$brimSection %in% brim_sections) &&
    (!length(subjects) || any(subjects %in% product$subjectTags)) &&
    (!length(information_types) || any(information_types %in% product$informationTypes))
}
combined_fire_weather <- Filter(function(product) matches_filters(
  product, "External", "Fire Weather", "Forecast / Outlook"
), bundle$products)
assert_true(setequal(vapply(combined_fire_weather, `[[`, character(1), "id"), fire_weather_ids) &&
              !anyDuplicated(vapply(combined_fire_weather, `[[`, character(1), "id")),
            "Combined Where/Subject/Information Type filtering duplicates or loses Products")
assert_identical(length(Filter(matches_filters, bundle$products)), length(bundle$products),
                 "Clearing filters does not restore the complete projected Product inventory")
subject_or <- Filter(function(product) matches_filters(
  product, subjects = c("Groundwater", "Soil Moisture")
), bundle$products)
assert_true(all(vapply(subject_or, function(product) {
  any(c("Groundwater", "Soil Moisture") %in% product$subjectTags)
}, logical(1))) && !anyDuplicated(vapply(subject_or, `[[`, character(1), "id")),
"Primary Subject multi-select is not OR-within without duplication")
local_soil <- Filter(function(product) matches_filters(
  product, brim_sections = "Local", subjects = "Soil Moisture"
), bundle$products)
assert_true(all(vapply(local_soil, function(product) {
  identical(product$brimSection, "Local") && "Soil Moisture" %in% product$subjectTags
}, logical(1))), "Local + Soil Moisture returned a Product outside both selected facets")
local_groundwater <- Filter(function(product) matches_filters(
  product, brim_sections = "Local", subjects = "Groundwater"
), bundle$products)
assert_true(length(local_groundwater) > 0L && all(vapply(local_groundwater, function(product) {
  identical(product$brimSection, "Local") && "Groundwater" %in% product$subjectTags
}, logical(1))), "Where in BRIM and Primary Subject are not AND-across")
assert_identical(scan$subjectTags, "Soil Moisture",
                 "SCAN Soil Moisture gained an unrelated inferred subject")
assert_true(all(c("Live Observation", "Historical Context") %in% scan$informationTypes),
            "SCAN Information Type tags are incomplete")

rich_probe_ids <- c(
  "huc8", "gw_bull118", "EXT070", "EXT072", "EXT074", "blm_diffs",
  "wild_scenic_rivers_blm_ca_lines", "wild_scenic_rivers_usfs_interagency_segments",
  "wild_scenic_river_corridors_blm_ca", "wild_scenic_river_corridors_usfs_lsrs",
  "wild_scenic_river_legal_status_corridors_usfs_lsrs", "ops_scan_soil_moisture",
  "ops_snow_pillow_swe", "ops_streamflow_usgs_ca", "product-ops-usgs-groundwater",
  "winter_storm_levels", "ops_major_water_supply_forecasts", "ops_delta_snapshot",
  "brim_mapped_conveyance", "tool_local_gis_upload", "tool_external_gis_overlay"
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
             "Product paths must be nonblank and unique",
             "Duplicate Guide paths did not fail validation")
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
assert_true(grepl("function productMatchesFilters", guide_js, fixed = TRUE) &&
              grepl("function filteredProducts", guide_js, fixed = TRUE) &&
              grepl("function visibleProducts", guide_js, fixed = TRUE) &&
              grepl("state.filters.brimSection", guide_js, fixed = TRUE) &&
              !grepl("state.filters.entityType", guide_js, fixed = TRUE) &&
              grepl("state.filters.subject.some", guide_js, fixed = TRUE) &&
              grepl("state.filters.informationType.some", guide_js, fixed = TRUE) &&
              grepl("asArray(product.subjectTags).indexOf", guide_js, fixed = TRUE) &&
              grepl("asArray(product.informationTypes).indexOf", guide_js, fixed = TRUE) &&
              grepl("facet-toggle", guide_js, fixed = TRUE) &&
              grepl("filter-remove", guide_js, fixed = TRUE) &&
              grepl("clear-all", guide_js, fixed = TRUE),
            "Visible multi-select Where/Subject/Information Type filtering is incomplete")
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
              grepl("relationship.role", guide_js, fixed = TRUE) &&
              grepl("quickDefinition.summary", guide_js, fixed = TRUE),
            "Generic structured content, Method, Resource-role, or Quick summary rendering is incomplete")
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
assert_true(!grepl("tool_local_gis_upload|tool_external_gis_overlay|EXT072", guide_js,
                   fixed = FALSE, perl = TRUE),
            "New content introduced a record-specific browser branch")
assert_true(!grepl("source_refs|sourceRefs|runtimeStatus|freshnessStatus|nextExpectedUpdate",
                   projected_json, fixed = FALSE, perl = TRUE),
            "Source evidence or generalized runtime-status fields leaked into the browser payload")
assert_true(all(vapply(
  c("Search layers, tools, methods, resources, and updates",
    "Browse BRIM layers & tools", "All Layers & Tools A–Z", "Information Type",
    "Where in BRIM", "Primary Subject", "Clear all",
    "Related Layers & Tools", "Open email draft"),
  function(probe) grepl(probe, guide_js, fixed = TRUE),
  logical(1)
)), "Layer/tool terminology or contact action is incomplete")
assert_true(!grepl("node\\(['\"]select|createElement\\(['\"]select|<select|filter-select|Entity type|filters-clear|Clear filters",
                   paste(guide_js, guide_css), ignore.case = TRUE, perl = TRUE),
            "Superseded dropdown, Entity Type, or large Clear Filters UI remains")
assert_true(grepl("activeSummary.hidden = !", guide_js, fixed = TRUE) &&
              grepl("searchInput.focus()", guide_js, fixed = TRUE) &&
              grepl("aria-pressed", guide_js, fixed = TRUE) &&
              grepl("aria-live", guide_js, fixed = TRUE) &&
              grepl("aria-hidden", guide_js, fixed = TRUE),
            "Contextual clear, selected state, chips, or focus behavior is incomplete")
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
assert_true(!any(grepl("https?://|controller|PT_[A-Z]", unlist(lapply(
  bundle$products, `[[`, "searchTerms"
), use.names = FALSE), ignore.case = TRUE, perl = TRUE)),
"URL, controller, or runtime-symbol text entered Product semantic search terms")
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
assert_true(!grepl("\\b(rollback|defect)\\b|threshold-enforcement|QA inputs|producer QA", projected_json,
                   ignore.case = TRUE, perl = TRUE),
            "Developer-facing quality or rollback terminology leaked into Guide payload")

cat("GUIDE-I2A source-backed vitals contracts passed.\n")
cat("PROFILE_ID=default\n")
cat("PRODUCTS=", bundle$counts$products, "\n", sep = "")
cat("ARTICLES=", bundle$counts$articles, "\n", sep = "")
cat("RESOURCES=", bundle$counts$resources, "\n", sep = "")
cat("UPDATES=", bundle$counts$updates, "\n", sep = "")
cat("QUICK_ACCESS=", bundle$counts$quickAccess, "\n", sep = "")
cat("EMBEDDED_PAYLOAD_BYTES=", payload_bytes, "\n", sep = "")
cat("GUIDE_JS_BYTES=", js_bytes, "\n", sep = "")
cat("GUIDE_CSS_BYTES=", css_bytes, "\n", sep = "")
