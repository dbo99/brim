#!/usr/bin/env Rscript

fail <- function(message) stop(message, call. = FALSE)
expect_true <- function(value, message) if (!isTRUE(value)) fail(message)

source(file.path("03_functions", "layer_capability_helpers.r"))
source(file.path("03_functions", "leaflet_tools_adddata_helpers.r"))

inventory_path <- file.path("04_processed_data", "qa", "layer_inventory_latest.csv")
capability_path <- file.path("04_processed_data", "qa", "layer_capability_coverage_latest.csv")
legend_summary_path <- file.path("04_processed_data", "qa", "layer_legend_summary_latest.md")
map <- pt_add_tools_adddata_panel(
  leaflet::leaflet(),
  list(add_tools_adddata_panel = TRUE, add_blm_sma_context_overlay = FALSE)
)
payload <- map$jsHooks$render[[2]]$data
expect_true(file.exists(inventory_path), "Ordinary final-map panel assembly did not write the inventory artifact.")
expect_true(file.exists(legend_summary_path), "Ordinary capability finalization did not write the legend summary artifact.")

inventory <- utils::read.csv(inventory_path, stringsAsFactors = FALSE, check.names = FALSE)
coverage <- utils::read.csv(capability_path, stringsAsFactors = FALSE, check.names = FALSE)
layers <- coverage[coverage$record_type == "layer", , drop = FALSE]
expect_true(identical(names(inventory), pt_layer_inventory_output_columns()), "Inventory artifact schema changed unexpectedly.")
expect_true(nrow(inventory) == 175L, "External inventory baseline is no longer 175 layers.")
expect_true(!anyDuplicated(inventory$layer_key), "Inventory layer keys are duplicated.")
expect_true(identical(inventory$layer_key, layers$layer_key), "Inventory does not join one-to-one in capability-layer order.")
expect_true(all(inventory$stable_layer_id == layers$stable_layer_id), "Stable IDs drifted across the inventory/capability join.")
expect_true(sum(inventory$has_legend) == 13L, "Inventory strict LGND baseline changed.")
expect_true(sum(inventory$has_feature_info) == 120L, "Inventory INFO baseline changed.")
expect_true(sum(inventory$has_both) == 12L, "Inventory combined LGND/INFO baseline changed.")
expect_true(sum(inventory$has_neither) == 54L, "Inventory neither-capability baseline changed.")
expect_true(identical(
  inventory$has_legend,
  pt_strict_has_legend(inventory$legend_status, inventory$legend_automatic_mount, inventory$legend_keyed_symbology)
), "Inventory LGND values do not follow strict legend state.")
provider_inventory <- inventory[inventory$provider_legend_available, , drop = FALSE]
expect_true(nrow(provider_inventory) == 92L && all(nzchar(provider_inventory$provider_legend_url)), "Provider legend URLs are not fully represented in inventory.")
expect_true(sum(inventory$provider_legend_reference_only) == 89L && !any(inventory$has_legend[inventory$provider_legend_reference_only]), "Provider-reference-only inventory semantics changed.")
generic_ids <- c("EXT048", "EXT051", sprintf("EXT%03d", 81:100), "EXT105", "EXT106")
generic_inventory <- inventory[match(paste0("external:", generic_ids), inventory$layer_key), , drop = FALSE]
expect_true(!anyNA(generic_inventory$layer_key) && all(generic_inventory$has_feature_info), "Generic shared-vector INFO roster is incomplete in inventory.")
expect_true(all(generic_inventory$info_content_basis == "generic_attribute_popup") && !any(generic_inventory$info_fields_curated) && all(generic_inventory$info_content_quality == "uncurated"), "Inventory lost the generic-popup curation distinction.")

catalog <- do.call(rbind, lapply(payload$catalog, function(record) {
  as.data.frame(lapply(record, function(value) {
    if (!length(value) || is.null(value)) "" else as.character(value[[1]])
  }), stringsAsFactors = FALSE, check.names = FALSE)
}))
fixed_capabilities <- pt_finalize_external_layer_capabilities(
  catalog, write_qa = FALSE, build_timestamp = "fixed", git_head = "fixed"
)
first <- pt_finalize_external_layer_inventory(
  fixed_capabilities$catalog, fixed_capabilities$layer_rows, write_qa = FALSE
)$rows
second <- pt_finalize_external_layer_inventory(
  fixed_capabilities$catalog, fixed_capabilities$layer_rows, write_qa = FALSE
)$rows
expect_true(identical(first, second), "Inventory output is not deterministic with fixed build metadata.")

visible_catalog <- fixed_capabilities$catalog[match(first$layer_key, paste0(
  "external:", fixed_capabilities$catalog$external_layer_id
)), , drop = FALSE]
expect_true(identical(first$service_url, visible_catalog$service_url), "Original service URLs were not preserved exactly.")
normalized_again <- lapply(first$service_url, pt_normalize_service_url)
expect_true(identical(first$normalized_service_root, vapply(normalized_again, `[[`, character(1), "root")), "Service-root normalization is not deterministic.")

endpoint_expected <- table(first$normalized_service_endpoint)
expect_true(all(first$exact_endpoint_match_count == as.integer(endpoint_expected[first$normalized_service_endpoint])), "Exact endpoint counts do not reconcile.")
family_expected <- table(first$service_family_key)
expect_true(all(first$service_family_layer_count == as.integer(family_expected[first$service_family_key])), "Service-family counts do not reconcile.")
same_host <- split(first, first$service_host)
same_host_distinct_services <- Filter(function(x) nrow(x) > 1L && length(unique(x$normalized_service_root)) > 1L, same_host)
expect_true(length(same_host_distinct_services) > 0L, "Test catalog lacks a same-host distinct-service case.")
expect_true(all(vapply(same_host_distinct_services, function(x) {
  all(vapply(split(x$service_family_key, x$normalized_service_root), function(keys) length(unique(keys)) == 1L, logical(1))) &&
    length(unique(x$service_family_key)) == length(unique(x$normalized_service_root))
}, logical(1))), "Unrelated services sharing a host were collapsed into one family.")

known_flags <- pt_inventory_flag_codes()
actual_flags <- unique(unlist(strsplit(inventory$inventory_flag, ";", fixed = TRUE)))
actual_flags <- actual_flags[nzchar(actual_flags)]
expect_true(!length(setdiff(actual_flags, known_flags)), "Inventory contains an unsupported/manual flag.")
flag_source <- paste(deparse(body(pt_inventory_flags_for_row)), collapse = "\n")
expect_true(!grepl("EXT[0-9]", flag_source), "Inventory flag rules contain manual External layer IDs.")
expect_true(all(grepl("no_lgnd_no_info", inventory$inventory_flag[inventory$has_neither], fixed = TRUE)), "Neither-capability flags are incomplete.")
expect_true(all(grepl("shared_service_family", inventory$inventory_flag[inventory$service_family_layer_count > 1L], fixed = TRUE)), "Shared-family flags are incomplete.")
expect_true(all(grepl("exact_duplicate_endpoint", inventory$inventory_flag[inventory$exact_endpoint_match_count > 1L], fixed = TRUE)), "Duplicate-endpoint flags are incomplete.")
expect_true(all(grepl("metadata_incomplete", inventory$inventory_flag[
  !inventory$description_present | !inventory$source_note_present |
    !inventory$update_note_present | !inventory$official_source_link_present
], fixed = TRUE)), "Metadata-completeness flags are incomplete.")

synthetic <- first[1, , drop = FALSE]
synthetic$panel <- "local"
synthetic$layer_key <- "local:LOCAL_TEST"
synthetic$stable_layer_id <- "LOCAL_TEST"
synthetic$primary_panel <- "local"
synthetic$service_url <- synthetic$service_host <- ""
synthetic$normalized_service_endpoint <- synthetic$normalized_service_root <- ""
synthetic$service_layer_number <- synthetic$service_family_key <- ""
synthetic$service_family_layer_count <- synthetic$exact_endpoint_match_count <- 0L
synthetic$build_timestamp <- synthetic$git_head <- "fixed"
local_result <- pt_finalize_layer_inventory_rows(synthetic, write_qa = FALSE)$rows
expect_true(nrow(local_result) == 1L && local_result$layer_key == "local:LOCAL_TEST", "Panel-neutral inventory writer rejected a synthetic Local record.")

cat("Layer inventory QA tests passed without remote requests.\n")
