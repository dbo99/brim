#!/usr/bin/env Rscript

fail <- function(message) stop(message, call. = FALSE)
expect_true <- function(value, message) if (!isTRUE(value)) fail(message)
expect_error <- function(expr, pattern) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  if (is.null(error) || !grepl(pattern, conditionMessage(error), fixed = TRUE)) {
    fail(paste0("Expected error containing '", pattern, "'."))
  }
}

source(file.path("03_functions", "layer_capability_helpers.r"))
source(file.path("03_functions", "leaflet_tools_adddata_helpers.r"))

qa_path <- file.path("04_processed_data", "qa", "layer_capability_coverage_latest.csv")
map <- pt_add_tools_adddata_panel(
  leaflet::leaflet(),
  list(add_tools_adddata_panel = TRUE, add_blm_sma_context_overlay = FALSE)
)
payload <- map$jsHooks$render[[2]]$data
expect_true(file.exists(qa_path), "Ordinary final-map panel assembly did not write the capability QA artifact.")
expect_true(identical(names(payload$capability_definitions), c("LGND", "INFO")), "Capability definitions were not embedded in the runtime payload.")
expect_true(payload$capability_definitions$LGND$label == "Map legend available", "LGND wording drifted.")
expect_true(payload$capability_definitions$INFO$label == "Feature details available by hover or click", "INFO wording drifted.")
expect_true(identical(pt_capability_diagnostic_severities(), c("info", "warning", "error")), "Diagnostic severity contract changed.")

coverage <- utils::read.csv(qa_path, stringsAsFactors = FALSE, check.names = FALSE)
layers <- coverage[coverage$record_type == "layer", , drop = FALSE]
groups <- coverage[coverage$record_type == "group", , drop = FALSE]
subgroups <- coverage[coverage$record_type == "subgroup", , drop = FALSE]
required_columns <- pt_layer_capability_output_columns()
expect_true(identical(names(coverage), required_columns), "Coverage artifact schema changed unexpectedly.")
expect_true(nrow(layers) == 175L, "External-visible baseline is no longer 175 layers.")
expect_true(nrow(groups) == 22L && nrow(subgroups) == 54L, "Capability hierarchy baseline changed.")
expect_true(!anyDuplicated(layers$layer_key), "Layer capability keys are not unique.")
expect_true(!anyDuplicated(layers$stable_layer_id), "Stable External IDs are not unique.")
expect_true(all(startsWith(layers$layer_key, "external:")), "External layer keys are not panel-qualified.")
expect_true(sum(layers$has_legend) == 27L, "LGND effective-catalog baseline changed.")
expect_true(sum(layers$has_feature_info) == 96L, "INFO effective-catalog baseline changed.")
expect_true(sum(layers$has_legend & layers$has_feature_info) == 26L, "Combined LGND/INFO baseline changed.")
expect_true(sum(layers$has_legend & !layers$has_feature_info) == 1L, "LGND-only baseline changed.")
expect_true(sum(!layers$has_legend & layers$has_feature_info) == 70L, "INFO-only baseline changed.")
expect_true(sum(!layers$has_legend & !layers$has_feature_info) == 78L, "Neither-capability baseline changed.")
expect_true(all(layers$legend_renderable[layers$has_legend]), "LGND was granted without a renderable BRIM legend.")
expect_true(all(layers$info_content_meaningful == layers$has_feature_info), "INFO was granted without meaningful runtime content.")
expect_true(all(!layers$has_legend[layers$legend_type == "provider_link"]), "Provider legend links were counted as BRIM LGND.")
expect_true(all(layers$has_legend[layers$legend_type %in% c("inline", "dynamic_map_card")]), "Renderable legend types did not receive LGND.")
style_notes <- layers[layers$legend_adapter == "generic_categorical", , drop = FALSE]
expect_true(nrow(style_notes) == 19L && all(style_notes$legend_type == "style_note") && all(style_notes$legend_renderable) && !any(style_notes$has_legend), "Generic categorical style notes did not remain renderable and LGND-negative.")
expected_style_note_ids <- c(
  "EXT143", "EXT144", "EXT131", "CVPIA_REFUGE_DELIVERY_POINTS", "EXT138", "EXT137",
  "USGS_DEBRIS_FLOW_HAZARD_SEGMENTS", "USGS_DEBRIS_FLOW_PROBABILITY_BASINS",
  "EXT115", "EXT118", "EXT102", "EXT116", "EXT117", "EXT048",
  "WSR_BLM_CA_CORRIDORS", "WSR_CA_STATE_ONLY_LINES", "WSR_USFS_LSRS_AREAS_CA",
  "WSR_USFS_INTERAGENCY_SEGMENTS_CA", "WSR_USFS_LSRS_LEGAL_STATUS_CA"
)
expect_true(setequal(style_notes$stable_layer_id, expected_style_note_ids), "Generic categorical style-note layer identities changed.")
lgnd_only <- layers[layers$has_legend & !layers$has_feature_info, , drop = FALSE]
expect_true(nrow(lgnd_only) == 1L && lgnd_only$stable_layer_id == "EXT021", "EXT021 is not the single LGND-only layer.")

generic_ids <- c("EXT048", "EXT051", sprintf("EXT%03d", 81:100), "EXT105", "EXT106")
generic_popup <- layers[match(generic_ids, layers$stable_layer_id), , drop = FALSE]
expect_true(nrow(generic_popup) == 24L && !anyNA(generic_popup$stable_layer_id), "Known generic-popup baseline IDs are missing.")
expect_true(!any(generic_popup$has_feature_info) && !any(generic_popup$info_content_meaningful), "Unverified generic popup routes received INFO.")
expect_true(all(generic_popup$info_adapter == "shared_vector_feature") && all(grepl("popup route present", generic_popup$content_basis, fixed = TRUE)), "Generic popup runtime pathway metadata was not retained.")
expect_true(all(grepl("generic_popup_content_unverified", generic_popup$diagnostic_code, fixed = TRUE)), "Unverified generic popup diagnostics are missing.")
expect_true(all(grepl("warning", generic_popup$diagnostic_severity, fixed = TRUE)), "Generic popup diagnostics are not warnings.")

provider <- layers[layers$provider_legend_available, , drop = FALSE]
expect_true(nrow(provider) == 89L && all(grepl("provider_legend_reference_only", provider$diagnostic_code, fixed = TRUE)), "Provider legend metadata baseline changed.")
expect_true(all(vapply(seq_len(nrow(provider)), function(i) {
  codes <- strsplit(provider$diagnostic_code[i], ";", fixed = TRUE)[[1]]
  severities <- strsplit(provider$diagnostic_severity[i], ";", fixed = TRUE)[[1]]
  severities[match("provider_legend_reference_only", codes)] == "info"
}, logical(1))), "Provider legend references are not informational.")

specialized <- layers[grepl("specialized_style_without_registered_legend", layers$diagnostic_code, fixed = TRUE), , drop = FALSE]
specialized_family <- vapply(strsplit(specialized$diagnostic_detail, " | ", fixed = TRUE), function(details) {
  hit <- details[grepl("Specialized ", details, fixed = TRUE)][1]
  sub("^Specialized ([^ ]+) styling.*$", "\\1", hit)
}, character(1))
expect_true(sum(specialized_family == "spc_forecast") == 12L && sum(specialized_family == "airnow_aqi") == 4L && sum(specialized_family == "usgs_earthquake") == 2L, "Specialized-style family diagnostics changed.")

uic <- layers[grepl("^UIC_", layers$stable_layer_id), , drop = FALSE]
expect_true(nrow(uic) == 4L && all(uic$legend_adapter == "uic_explorer") && all(uic$info_adapter == "uic_bespoke"), "UIC bespoke capability adapters changed.")
mlrs <- layers[grepl("BLM MLRS", layers$display_name, fixed = TRUE), , drop = FALSE]
expect_true(nrow(mlrs) == 6L && all(mlrs$legend_adapter == "mlrs_mineral_cases") && all(mlrs$info_adapter == "mlrs_aggregate"), "MLRS aggregate adapters changed.")
dwr_raster <- layers[grepl("DWR/TRE", layers$display_name, fixed = TRUE) & grepl("mosaic", layers$display_name, fixed = TRUE), , drop = FALSE]
expect_true(nrow(dwr_raster) == 2L && all(dwr_raster$feature_info_identify) && all(dwr_raster$has_feature_info), "Formatted DWR/TRE raster identify did not qualify for INFO.")
ravg <- layers[grepl("^RAVG post-fire vegetation", layers$display_name), , drop = FALSE]
expect_true(nrow(ravg) == 3L && !any(ravg$has_feature_info), "Generic ImageServer pixel identify was incorrectly counted as INFO.")
expect_true(any(layers$feature_info_hover) && any(layers$feature_info_popup) && any(layers$feature_info_identify), "Expected hover, popup, and identify INFO paths are not represented.")

expect_true(sum(groups$layer_count) == nrow(layers), "Group layer totals do not reconcile.")
expect_true(sum(groups$legend_count) == sum(layers$has_legend), "Group LGND totals do not reconcile.")
expect_true(sum(groups$info_count) == sum(layers$has_feature_info), "Group INFO totals do not reconcile.")
expect_true(sum(subgroups$layer_count) == nrow(layers), "Subgroup layer totals do not reconcile.")
expect_true(sum(subgroups$legend_count) == sum(layers$has_legend), "Subgroup LGND totals do not reconcile.")
expect_true(sum(subgroups$info_count) == sum(layers$has_feature_info), "Subgroup INFO totals do not reconcile.")
expect_true(!anyNA(layers[c("group_layer_count", "group_legend_count", "group_info_count", "subgroup_layer_count", "subgroup_legend_count", "subgroup_info_count")]), "Layer hierarchy totals were not populated.")
expect_true(all(layers$layer_count == 1L & layers$legend_count == as.integer(layers$has_legend) & layers$info_count == as.integer(layers$has_feature_info)), "Layer record count semantics changed.")
expect_true(all(groups$layer_count == groups$group_layer_count & groups$legend_count == groups$group_legend_count & groups$info_count == groups$group_info_count), "Group record count semantics changed.")
expect_true(all(subgroups$layer_count == subgroups$subgroup_layer_count & subgroups$legend_count == subgroups$subgroup_legend_count & subgroups$info_count == subgroups$subgroup_info_count), "Subgroup record count semantics changed.")

catalog <- do.call(rbind, lapply(payload$catalog, function(record) {
  as.data.frame(lapply(record, function(value) {
    if (!length(value) || is.null(value)) "" else as.character(value[[1]])
  }), stringsAsFactors = FALSE, check.names = FALSE)
}))
first <- pt_finalize_external_layer_capabilities(catalog, write_qa = FALSE, build_timestamp = "fixed", git_head = "fixed")$coverage
second <- pt_finalize_external_layer_capabilities(catalog, write_qa = FALSE, build_timestamp = "fixed", git_head = "fixed")$coverage
expect_true(identical(first, second), "Capability finalization is not deterministic.")

registry <- pt_new_layer_capability_registry()
registry <- pt_register_layer_capability(
  registry, "external", layers$stable_layer_id[layers$has_legend][1],
  has_legend = TRUE,
  legend_adapter = layers$legend_adapter[layers$has_legend][1],
  registration_source = "qa_explicit_registration",
  content_basis = "Existing registered renderer"
)
registered <- pt_finalize_external_layer_capabilities(catalog, registrations = registry, write_qa = FALSE, build_timestamp = "fixed", git_head = "fixed")$coverage
registered_layer <- registered[registered$layer_key == registry$layer_key[1] & registered$record_type == "layer", , drop = FALSE]
expect_true(grepl("qa_explicit_registration", registered_layer$registration_source, fixed = TRUE), "Valid explicit registration was not applied.")

local_registry <- pt_register_layer_capability(pt_new_layer_capability_registry(), "local", "LOCAL_TEST")
ops_registry <- pt_register_layer_capability(pt_new_layer_capability_registry(), "ops_live", "OPS_TEST")
expect_true(local_registry$layer_key == "local:LOCAL_TEST" && ops_registry$layer_key == "ops_live:OPS_TEST", "Panel-neutral registration keys are not stable.")
synthetic_local <- first[first$record_type == "layer", , drop = FALSE][1, ]
synthetic_local$panel <- "local"
synthetic_local$stable_layer_id <- "LOCAL_TEST"
synthetic_local$layer_key <- "local:LOCAL_TEST"
synthetic_local$group_key <- "local:group:test"
synthetic_local$group <- "Test"
synthetic_local$subgroup_key <- "local:group:test:subgroup:test"
synthetic_local$subgroup <- "Test"
synthetic_local$build_timestamp <- "fixed"
synthetic_local$git_head <- "fixed"
local_finalized <- pt_finalize_layer_capability_rows(synthetic_local)
expect_true(nrow(local_finalized$coverage) == 3L && local_finalized$layer_rows$layer_key == "local:LOCAL_TEST", "Panel-neutral finalizer rejected a synthetic Local record.")
unknown_adapter_row <- synthetic_local
unknown_adapter_row$legend_adapter <- "unknown_test_adapter"
expect_error(pt_finalize_layer_capability_rows(unknown_adapter_row), "unknown legend adapters")
expect_error(pt_validate_capability_registrations(local_registry, layers$layer_key), "unknown layer keys")
conflict <- rbind(registry, transform(registry, has_feature_info = TRUE, feature_info_popup = TRUE, info_adapter = "qa"))
expect_error(pt_validate_capability_registrations(conflict, layers$layer_key), "Conflicting duplicate")

bad_missing <- catalog
visible_index <- which(tolower(bad_missing$primary_panel) %in% c("external", "both"))[1]
bad_missing$external_layer_id[visible_index] <- ""
expect_error(pt_finalize_external_layer_capabilities(bad_missing, write_qa = FALSE), "require stable external_layer_id")
bad_duplicate <- catalog
visible_indices <- which(tolower(bad_duplicate$primary_panel) %in% c("external", "both"))[1:2]
bad_duplicate$external_layer_id[visible_indices[2]] <- bad_duplicate$external_layer_id[visible_indices[1]]
expect_error(pt_finalize_external_layer_capabilities(bad_duplicate, write_qa = FALSE), "Duplicate External-visible")

base_record <- catalog[match("EXT097", catalog$external_layer_id), , drop = FALSE]
expect_true(!pt_external_info_resolution(base_record)$has_feature_info, "Broad popup flags still establish INFO.")
configured_popup <- base_record; configured_popup$popup_fields <- "NAME"
expect_true(pt_external_info_resolution(configured_popup)$has_feature_info, "Configured popup fields did not establish INFO.")
configured_link <- base_record; configured_link$popup_link_template <- "https://example.test/{OBJECTID}"
expect_true(pt_external_info_resolution(configured_link)$has_feature_info, "Feature-specific link did not establish INFO.")

external_js <- file.path("03_functions", "js", "leaflet_tools_adddata_panel.js")
renderer_copy <- tempfile(fileext = ".js")
renderer_text <- readLines(external_js, warn = FALSE)
renderer_text <- sub("function ptDroughtMonitorLegendHtml", "function ptDroughtMonitorLegendMissing", renderer_text, fixed = TRUE)
writeLines(renderer_text, renderer_copy)
expect_error(pt_validate_legend_adapter_implementations(external_js_path = renderer_copy), "no renderer implementation")

cat("Layer capability registry and effective-catalog QA tests passed.\n")
