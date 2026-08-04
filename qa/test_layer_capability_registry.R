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
legend_summary_path <- file.path("04_processed_data", "qa", "layer_legend_summary_latest.md")
map <- pt_add_tools_adddata_panel(
  leaflet::leaflet(),
  list(add_tools_adddata_panel = TRUE, add_blm_sma_context_overlay = FALSE)
)
payload <- map$jsHooks$render[[2]]$data
expect_true(file.exists(qa_path), "Ordinary final-map panel assembly did not write the capability QA artifact.")
expect_true(file.exists(legend_summary_path), "Ordinary capability finalization did not write the legend summary.")
written_legend_summary <- readLines(legend_summary_path, warn = FALSE)
expect_true(identical(written_legend_summary[1], "# BRIM External Layer Legend Summary"), "Legend summary title is not standardized exactly.")
written_git_line <- written_legend_summary[startsWith(written_legend_summary, "- Git HEAD: `")]
expect_true(length(written_git_line) == 1L && written_git_line != "- Git HEAD: ``", "Written legend summary contains empty Git metadata.")
expect_true(identical(names(payload$capability_definitions), c("LGND", "INFO")), "Capability definitions were not embedded in the runtime payload.")
expect_true(payload$capability_definitions$LGND$label == "BRIM map legend available", "LGND wording drifted.")
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
renamed_group_expectations <- data.frame(
  group = c("Geology / Geophysics", "Hydro Basins / Admin Bnds"),
  group_key = c(
    "external:group:geology_geophysics_seismicity",
    "external:group:hydrologic_basins_admin_boundaries"
  ),
  layer_count = c(10L, 14L),
  stringsAsFactors = FALSE
)
renamed_groups <- groups[match(renamed_group_expectations$group, groups$group), , drop = FALSE]
expect_true(
  !anyNA(renamed_groups$group) &&
    identical(renamed_groups$group_key, renamed_group_expectations$group_key) &&
    identical(renamed_groups$layer_count, renamed_group_expectations$layer_count),
  "Renamed External group labels or their stable historical keys changed."
)
expect_true(
  !any(layers$group %in% c(
    "Geology / Geophysics / Seismicity",
    "Hydrologic Basins / Admin Boundaries"
  )),
  "A retired External group display label remains in the effective catalog."
)
expect_true(
  setequal(
    unique(layers$subgroup[layers$group == "Geology / Geophysics"]),
    c("Faults / seismicity", "Geology / maps", "Land subsidence")
  ) && setequal(
    unique(layers$subgroup[layers$group == "Hydro Basins / Admin Bnds"]),
    c("1water", "Groundwater", "Surface water", "Watershed condition")
  ),
  "A subgroup label changed while renaming its External parent group."
)
expect_true(!anyDuplicated(layers$layer_key), "Layer capability keys are not unique.")
expect_true(!anyDuplicated(layers$stable_layer_id), "Stable External IDs are not unique.")
expect_true(all(startsWith(layers$layer_key, "external:")), "External layer keys are not panel-qualified.")
expect_true(sum(layers$has_legend) == 13L, "Strict automatic-BRIM LGND baseline changed.")
expect_true(sum(layers$has_feature_info) == 120L, "INFO effective-catalog baseline changed.")
expect_true(sum(layers$has_legend & layers$has_feature_info) == 12L, "Combined LGND/INFO baseline changed.")
expect_true(sum(layers$has_legend & !layers$has_feature_info) == 1L, "LGND-only baseline changed.")
expect_true(sum(!layers$has_legend & layers$has_feature_info) == 108L, "INFO-only baseline changed.")
expect_true(sum(!layers$has_legend & !layers$has_feature_info) == 54L, "Neither-capability baseline changed.")
expected_status_counts <- c(
  brim_automatic = 3L, brim_shared_automatic = 10L,
  brim_hidden_or_manual = 7L, partial_brim_legend = 4L,
  renderer_available_unmounted = 0L, style_mapping_available = 18L,
  style_note_only = 22L, provider_reference_only = 69L, none = 42L
)
actual_status_counts <- table(factor(layers$legend_status, levels = names(expected_status_counts)))
expect_true(identical(as.integer(actual_status_counts), unname(expected_status_counts)), "Legend-status counts changed.")
expect_true(all(layers$legend_renderable[layers$has_legend]), "LGND was granted without a renderable BRIM legend.")
expect_true(all(layers$info_content_meaningful == layers$has_feature_info), "INFO was granted without meaningful runtime content.")
expect_true(all(layers$info_content_basis[layers$has_feature_info] != "none") && all(layers$info_content_quality[layers$has_feature_info] != "none"), "INFO was granted without a normalized content basis or quality.")
expect_true(all(!layers$has_legend[layers$legend_type == "provider_link"]), "Provider legend links were counted as BRIM LGND.")
expect_true(identical(
  layers$has_legend,
  pt_strict_has_legend(layers$legend_status, layers$legend_automatic_mount, layers$legend_keyed_symbology)
), "LGND was not derived exclusively from strict legend state.")
expect_true(all(!layers$has_legend | layers$legend_status %in% c("brim_automatic", "brim_shared_automatic")), "Non-automatic legend states received LGND.")
expect_true(all(layers$legend_automatic_mount[layers$has_legend]) && all(layers$legend_keyed_symbology[layers$has_legend]), "Automatic LGND rows lack automatic mounting or keyed symbology.")
expect_true(all(nzchar(layers$legend_renderer_key[layers$has_legend])) && all(nzchar(layers$legend_mount_path[layers$has_legend])), "Automatic LGND rows lack renderer or mount-path evidence.")
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
expected_lgnd_ids <- c(
  sprintf("EXT%d", 145:150), "UIC_EPA_LIVE", "UIC_CALGEM_POST_LIVE",
  "UIC_CALGEM_PRIMACY_LIVE", "UIC_EPA_REFERENCE_POINTS",
  "EXT033", "EXT021", "EXT047"
)
expect_true(setequal(layers$stable_layer_id[layers$has_legend], expected_lgnd_ids), "Strict automatic-BRIM LGND layer identities changed.")
expected_lost_status <- c(
  CALIPC_INVASIVE_SPECIES_BY_QUAD = "style_note_only",
  CALIPC_TERRESTRIAL_INVASION_BY_QUAD = "style_note_only",
  EXT114 = "brim_hidden_or_manual", EXT070 = "brim_hidden_or_manual",
  EXT072 = "brim_hidden_or_manual", EXT074 = "style_note_only",
  DWR_TRE_ALTAMIRA_POINT_LOCATIONS_2026Q1 = "brim_hidden_or_manual",
  SWRCB_2024_IR_LINES = "brim_hidden_or_manual",
  SWRCB_2024_IR_POLYGONS = "brim_hidden_or_manual",
  EXT056 = "brim_hidden_or_manual", EXT139 = "partial_brim_legend",
  EXT140 = "partial_brim_legend", EXT141 = "partial_brim_legend",
  EXT142 = "partial_brim_legend"
)
lost <- layers[match(names(expected_lost_status), layers$stable_layer_id), , drop = FALSE]
expect_true(!anyNA(lost$stable_layer_id) && !any(lost$has_legend), "A hidden, prose-only, or partial former LGND layer still qualifies.")
expect_true(identical(lost$legend_status, unname(expected_lost_status)), "Former LGND layers did not retain their audited internal statuses.")
lgnd_only <- layers[layers$has_legend & !layers$has_feature_info, , drop = FALSE]
expect_true(nrow(lgnd_only) == 1L && lgnd_only$stable_layer_id == "EXT021", "EXT021 is not the single LGND-only layer.")

generic_ids <- c("EXT048", "EXT051", sprintf("EXT%03d", 81:100), "EXT105", "EXT106")
generic_popup <- layers[match(generic_ids, layers$stable_layer_id), , drop = FALSE]
expect_true(nrow(generic_popup) == 24L && !anyNA(generic_popup$stable_layer_id), "Known generic-popup baseline IDs are missing.")
expect_true(all(generic_popup$has_feature_info) && all(generic_popup$feature_info_popup) && all(generic_popup$info_content_meaningful), "Bound generic vector popups did not receive INFO.")
expect_true(all(generic_popup$info_adapter == "shared_vector_feature") && all(grepl("ptPopupFromProperties", generic_popup$content_basis, fixed = TRUE)), "Generic popup runtime pathway metadata was not retained.")
expect_true(all(generic_popup$info_content_basis == "generic_attribute_popup") && !any(generic_popup$info_fields_curated) && all(generic_popup$info_content_quality == "uncurated"), "Generic popup curation metadata is incorrect.")
expect_true(all(grepl("generic_popup_fields_uncurated", generic_popup$diagnostic_code, fixed = TRUE)), "Uncurated generic popup diagnostics are missing.")
expect_true(!any(grepl("warning", generic_popup$diagnostic_severity, fixed = TRUE)), "Uncurated generic popup diagnostics are not informational.")
expect_true(!any(grepl("generic_popup_content_unverified", layers$diagnostic_code, fixed = TRUE)), "Superseded generic-popup warning remains in normalized output.")
ext048 <- generic_popup[generic_popup$stable_layer_id == "EXT048", , drop = FALSE]
expect_true(nrow(ext048) == 1L && ext048$has_feature_info, "EXT048 did not receive INFO from its shared vector popup pathway.")

provider <- layers[layers$provider_legend_available, , drop = FALSE]
provider_only <- layers[layers$provider_legend_reference_only, , drop = FALSE]
expect_true(nrow(provider) == 92L && nrow(provider_only) == 89L, "Provider legend metadata baseline changed.")
expect_true(all(nzchar(provider$provider_legend_url)) && all(nzchar(provider$provider_legend_scope)), "Provider legend URL or scope metadata is incomplete.")
expect_true(!any(provider_only$has_legend) && all(grepl("provider_legend_reference_only", provider_only$diagnostic_code, fixed = TRUE)), "Provider-only references produced LGND or lost their diagnostic.")
expect_true(all(vapply(seq_len(nrow(provider)), function(i) {
  if (!isTRUE(provider$provider_legend_reference_only[i])) return(TRUE)
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
automatic_families <- layers[layers$stable_layer_id %in% expected_lgnd_ids, , drop = FALSE]
expect_true(all(automatic_families$has_legend) && all(automatic_families$legend_keyed_symbology), "A verified MLRS, UIC, WCR, SGMA, or subsidence automatic legend failed strict keyed qualification.")
expect_true(all(mlrs$legend_status == "brim_shared_automatic" & mlrs$legend_surface == "shared_map_card"), "MLRS shared automatic surface metadata changed.")
expect_true(all(uic$legend_status == "brim_shared_automatic" & uic$legend_surface == "leaflet_control"), "UIC shared automatic surface metadata changed.")
dedicated <- layers[match(c("EXT047", "EXT021", "EXT033"), layers$stable_layer_id), , drop = FALSE]
expect_true(all(dedicated$legend_status == "brim_automatic") && all(dedicated$legend_surface == "map_card"), "WCR, SGMA, or subsidence dedicated automatic surface metadata changed.")
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
expected_group_info <- c(
  "Ecology / Habitat / Species" = 28L,
  "Water Districts / Service / Planning" = 1L,
  "Water Rights" = 1L
)
actual_group_info <- groups$info_count[match(names(expected_group_info), groups$group)]
expect_true(identical(as.integer(actual_group_info), unname(expected_group_info)), "Restored generic-popup group INFO totals changed.")
expected_subgroup_info <- c(
  "Ecology / Habitat / Species\rBLM" = 4L,
  "Ecology / Habitat / Species\rCDFW" = 16L,
  "Ecology / Habitat / Species\rUSFWS" = 2L,
  "Water Districts / Service / Planning\rDistricts / Service" = 1L,
  "Water Rights\rPoints of Diversion" = 1L
)
subgroup_identity <- paste(subgroups$group, subgroups$subgroup, sep = "\r")
actual_subgroup_info <- subgroups$info_count[match(names(expected_subgroup_info), subgroup_identity)]
expect_true(identical(as.integer(actual_subgroup_info), unname(expected_subgroup_info)), "Restored generic-popup subgroup INFO totals changed.")

catalog <- do.call(rbind, lapply(payload$catalog, function(record) {
  as.data.frame(lapply(record, function(value) {
    if (!length(value) || is.null(value)) "" else as.character(value[[1]])
  }), stringsAsFactors = FALSE, check.names = FALSE)
}))
catalog_ext048 <- catalog[catalog$external_layer_id == "EXT048", , drop = FALSE]
expect_true(nrow(catalog_ext048) == 1L && catalog_ext048$external_display_num == "143" && pt_capability_truth(catalog_ext048$has_feature_info), "External display #143 / EXT048 did not receive build-resolved INFO.")
user_visible_catalog_fields <- intersect(c(
  "agency", "program", "theme", "external_group", "external_subgroup",
  "display_name", "notes", "geographic_scope", "pt2_usage_note",
  "large_layer_warning", "legend_note", "popup_aliases",
  "popup_link_label", "hover_aliases", "best_use",
  "useful_for_visualization", "style_legend_title",
  "field_curation_notes", "load_note", "load_audit_basis"
), names(catalog))
user_visible_catalog_text <- unlist(
  catalog[user_visible_catalog_fields],
  use.names = FALSE
)
legacy_brand_pattern <- "PortaTreasure2|(^|[^A-Za-z0-9_])PT2([^A-Za-z0-9_]|$)"
expect_true(
  !any(grepl(legacy_brand_pattern, user_visible_catalog_text, perl = TRUE)),
  "A user-visible External/Ops catalog value retains legacy branding."
)
expect_true(
  "pt2_usage_note" %in% names(catalog),
  "The permitted internal pt2_usage_note schema field was renamed."
)
first_result <- pt_finalize_external_layer_capabilities(catalog, write_qa = FALSE, build_timestamp = "fixed", git_head = "fixed")
second_result <- pt_finalize_external_layer_capabilities(catalog, write_qa = FALSE, build_timestamp = "fixed", git_head = "fixed")
first <- first_result$coverage
second <- second_result$coverage
expect_true(identical(first, second), "Capability finalization is not deterministic.")
expect_true(identical(first_result$legend_summary_markdown, second_result$legend_summary_markdown), "Legend summary is not deterministic with fixed inputs.")
expect_true(identical(first_result$legend_summary_markdown[1], "# BRIM External Layer Legend Summary"), "Generated legend summary title is not standardized exactly.")
legend_summary <- paste(first_result$legend_summary_markdown, collapse = "\n")
required_headings <- c(
  "## Build metadata", "## Overall counts", "## Legend status counts",
  "## Counts by group", "## Counts by subgroup",
  "## Automatic BRIM legends", "## Provider-reference-only layers",
  "## Future legend candidates"
)
expect_true(all(vapply(required_headings, function(x) grepl(x, legend_summary, fixed = TRUE), logical(1))), "Legend summary is missing required status sections.")
expect_true(grepl(provider_only$provider_legend_url[1], legend_summary, fixed = TRUE), "Legend summary omitted provider legend references.")
expect_true(!grepl("usefulness score|priority score|remove/keep|retirement score|recommendation", legend_summary, ignore.case = TRUE), "Legend summary contains subjective thinning recommendations.")

git_executable <- Sys.which("git")
expect_true(nzchar(git_executable), "Controlled Git metadata QA requires a Git executable.")
git_fixture_dir <- tempfile("brim_git_workspace_")
dir.create(git_fixture_dir)
run_fixture_git <- function(args) {
  out <- suppressWarnings(system2(
    git_executable,
    c("-C", shQuote(git_fixture_dir), args),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  expect_true(identical(as.integer(status), 0L), paste("Controlled Git fixture command failed:", paste(args, collapse = " ")))
  unname(out)
}
invisible(run_fixture_git(c("init", "--quiet")))
invisible(run_fixture_git(c("config", "user.name", "BRIM-QA")))
invisible(run_fixture_git(c("config", "user.email", "brim-qa@example.invalid")))
writeLines("BRIM Git metadata fixture", file.path(git_fixture_dir, "fixture.txt"))
invisible(run_fixture_git(c("add", "fixture.txt")))
invisible(run_fixture_git(c("commit", "--quiet", "-m", "fixture")))
fixture_expected_head <- run_fixture_git(c("rev-parse", "HEAD"))
fixture_git_head <- pt_capability_git_head(git_fixture_dir)
expect_true(
  length(fixture_expected_head) == 1L &&
    grepl("^[0-9a-fA-F]{40}$", fixture_expected_head) &&
    identical(fixture_git_head, fixture_expected_head),
  "Controlled Git checkout did not retain its actual HEAD SHA."
)
fixture_git_rows <- first_result$layer_rows
fixture_git_rows$git_head <- fixture_git_head
fixture_git_summary <- pt_layer_legend_summary_markdown(fixture_git_rows, first_result$catalog)
expect_true(any(fixture_git_summary == paste0("- Git HEAD: `", fixture_git_head, "`")), "Legend summary did not retain the controlled Git fixture SHA.")

explicit_git_head <- paste(rep("b", 40L), collapse = "")
expect_true(identical(pt_capability_git_head_display(explicit_git_head), explicit_git_head), "Explicitly supplied Git SHA was not retained.")
explicit_git_rows <- first_result$layer_rows
explicit_git_rows$git_head <- explicit_git_head
explicit_git_summary <- pt_layer_legend_summary_markdown(explicit_git_rows, first_result$catalog)
expect_true(any(explicit_git_summary == paste0("- Git HEAD: `", explicit_git_head, "`")), "Legend summary did not retain an explicitly supplied Git SHA.")

non_git_dir <- tempfile("brim_non_git_workspace_")
dir.create(non_git_dir)
unavailable_git_head <- pt_capability_git_head(non_git_dir)
expect_true(identical(unavailable_git_head, PT_CAPABILITY_GIT_HEAD_UNAVAILABLE), "Non-Git workspace did not produce explicit unavailable Git metadata.")
unavailable_git_rows <- first_result$layer_rows
unavailable_git_rows$git_head <- ""
unavailable_git_summary <- pt_layer_legend_summary_markdown(unavailable_git_rows, first_result$catalog)
expect_true(any(unavailable_git_summary == paste0("- Git HEAD: `", PT_CAPABILITY_GIT_HEAD_UNAVAILABLE, "`")), "Legend summary did not render explicit unavailable Git text.")
expect_true(!any(unavailable_git_summary == "- Git HEAD: ``"), "Legend summary rendered an empty Git HEAD.")

context_git_head <- pt_capability_git_head(getwd())
if (grepl("^[0-9a-fA-F]{40}$", context_git_head)) {
  message("Contextual Git checkout HEAD detected: ", context_git_head)
} else {
  message("Contextual Git HEAD assertion skipped: project workspace is not a Git checkout.")
}

variant_rows <- first_result$layer_rows
variant_rows$build_timestamp <- "different timestamp"
variant_rows$git_head <- paste(rep("a", 40L), collapse = "")
variant_summary <- pt_layer_legend_summary_markdown(variant_rows, first_result$catalog)
without_build_metadata <- function(report) {
  report[!startsWith(report, "- Build timestamp: `") & !startsWith(report, "- Git HEAD: `")]
}
expect_true(identical(
  without_build_metadata(first_result$legend_summary_markdown),
  without_build_metadata(variant_summary)
), "Legend summary changed outside timestamp and Git metadata.")

strict_seed <- first[first$record_type == "layer" & first$has_legend, , drop = FALSE][1, , drop = FALSE]
not_keyed <- strict_seed
not_keyed$legend_keyed_symbology <- FALSE
expect_true(!pt_finalize_layer_capability_rows(not_keyed)$layer_rows$has_legend, "An automatic surface without keyed symbology received LGND.")
not_mounted <- strict_seed
not_mounted$legend_status <- "renderer_available_unmounted"
not_mounted$legend_automatic_mount <- FALSE
expect_true(!pt_finalize_layer_capability_rows(not_mounted)$layer_rows$has_legend, "An unmounted renderer received LGND.")
provider_seed <- strict_seed
provider_seed$legend_status <- "provider_reference_only"
expect_true(!pt_finalize_layer_capability_rows(provider_seed)$layer_rows$has_legend, "A provider-reference status received LGND.")
hidden_seed <- strict_seed
hidden_seed$legend_status <- "brim_hidden_or_manual"
expect_true(!pt_finalize_layer_capability_rows(hidden_seed)$layer_rows$has_legend, "A hidden/manual renderer received LGND.")

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
generic_resolution <- pt_external_info_resolution(base_record)
expect_true(generic_resolution$has_feature_info && generic_resolution$feature_info_popup, "Bound shared vector popup did not establish INFO.")
expect_true(generic_resolution$info_content_basis == "generic_attribute_popup" && !generic_resolution$info_fields_curated && generic_resolution$info_content_quality == "uncurated", "Generic vector popup was not distinguished from curated INFO.")
bare_flags <- base_record
bare_flags$service_type <- "hub"
bare_flags$default_load_mode <- ""
expect_true(!pt_external_info_resolution(bare_flags)$has_feature_info, "Bare clickable/supports-popups flags without a vector runtime handler received INFO.")
configured_popup <- base_record; configured_popup$popup_fields <- "NAME"
configured_resolution <- pt_external_info_resolution(configured_popup)
expect_true(configured_resolution$has_feature_info && configured_resolution$info_fields_curated && configured_resolution$info_content_quality == "curated", "Configured popup fields did not establish distinct curated INFO.")
configured_link <- base_record; configured_link$popup_link_template <- "https://example.test/{OBJECTID}"
expect_true(pt_external_info_resolution(configured_link)$has_feature_info, "Feature-specific link did not establish INFO.")

external_js <- file.path("03_functions", "js", "leaflet_tools_adddata_panel.js")
renderer_copy <- tempfile(fileext = ".js")
renderer_text <- readLines(external_js, warn = FALSE)
renderer_text <- sub("function ptDroughtMonitorLegendHtml", "function ptDroughtMonitorLegendMissing", renderer_text, fixed = TRUE)
writeLines(renderer_text, renderer_copy)
expect_error(pt_validate_legend_adapter_implementations(external_js_path = renderer_copy), "no renderer implementation")

cat("Layer capability registry and effective-catalog QA tests passed.\n")
