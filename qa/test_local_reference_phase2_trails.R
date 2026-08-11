#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(htmltools)
})

source("00_config/config_local_reference_interactions.r")
source("00_config/config_labels.r")
source("03_functions/label_helpers.r")
source("03_functions/local_reference_interaction_helpers.r")

expect_equal <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    stop(
      label, " failed. Actual: ", paste(actual, collapse = ", "),
      "; expected: ", paste(expected, collapse = ", ")
    )
  }
}

pt_validate_local_reference_config()
pt_validate_local_reference_trails_research()

registry <- pt_local_reference_config_row(
  layer_id = "national_scenic_historic_trails"
)
expect_equal(registry$implementation_status, "phase2_trails", "Trails implementation status")
expect_equal(registry$color_basis, "trail_identity", "Trails color basis")
expect_equal(registry$category_heading, "Trail", "Trails category heading")
expect_equal(registry$primary_count_label, "trails", "Trails primary count label")
expect_equal(registry$popup_layout, "tabbed_card", "Trails tabbed popup layout")
expect_equal(
  LOCAL_REFERENCE_INTERACTION_REGISTRY$popup_layout[[4]],
  "tabbed_card",
  "WSA reuses the tabbed popup layout"
)
stopifnot(
  isTRUE(registry$auto_supported),
  isTRUE(registry$auto_default),
  isTRUE(registry$feature_selection_supported),
  isTRUE(registry$auto_zoom_supported),
  isTRUE(registry$auto_zoom_default),
  isTRUE(registry$retention_enabled),
  !isTRUE(registry$show_category_count),
  !isTRUE(registry$show_component_count)
)
expect_equal(
  unlist(registry$feature_search_fields[[1]], use.names = FALSE),
  c(
    "pt_trails_official_name", "pt_trails_common_name",
    "pt_trails_abbreviation", "pt_trails_alias_search", "pt_trails_nlcs_id"
  ),
  "Trails approved search fields"
)

categories <- pt_local_reference_categories("national_scenic_historic_trails")
expect_equal(
  categories$category_key,
  c(sprintf("nlcs%06d", 280:285), "unknown"),
  "six stable identity keys plus internal unknown"
)
expect_equal(
  categories$label,
  c(
    "California", "Pony Express", "Old Spanish", "Juan Bautista de Anza",
    "Pacific Crest", "Butterfield Overland", "Unknown trail identity"
  ),
  "Trails category labels"
)
stopifnot(
  length(unique(categories$stroke_color[1:6])) == 6L,
  all(categories$dash_array[1:6] == ""),
  all(categories$legend_swatch_style[1:6] == "line"),
  !categories$include_when_absent[[7]]
)

fixture <- utils::read.csv(
  file.path("qa", "fixtures", "local_reference_trails_source_snapshot.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = character()
)
expected_ids <- sprintf("NLCS%06d", 280:285)
expect_equal(fixture$NLCS_ID, expected_ids, "authoritative semantic ID inventory")
expect_equal(sum(fixture$fixture_geometry_components), 177L, "source geometry components")
stopifnot(!anyDuplicated(fixture$NLCS_ID), !anyDuplicated(fixture$GlobalID))

retained <- pt_local_reference_retained_source_fields("trails", names(fixture))
expect_equal(length(retained), 10L, "Trails retained source field count")
stopifnot(all(retained %in% names(fixture)))

make_multiline <- function(component_count, row_index) {
  lines <- lapply(seq_len(component_count), function(component_index) {
    x0 <- -124 + row_index * 0.5 + component_index * 0.0001
    y0 <- 32 + row_index * 0.4 + component_index * 0.0001
    matrix(c(x0, y0, x0 + 0.00005, y0 + 0.00005), ncol = 2, byrow = TRUE)
  })
  sf::st_multilinestring(lines)
}

source_sf <- sf::st_sf(
  fixture[, setdiff(names(fixture), "fixture_geometry_components"), drop = FALSE],
  geometry = sf::st_sfc(lapply(seq_len(nrow(fixture)), function(i) {
    make_multiline(fixture$fixture_geometry_components[[i]], i)
  }), crs = 4326)
)
source_sf$pt_display_name <- "National Scenic/Historic Trails *"
source_sf$pt_nickname <- "trails"
source_sf$pt_geom_type <- "polyline"

retained_only <- pt_prepare_local_reference_trails(
  source_sf,
  validate_snapshot = TRUE,
  build_display = FALSE
)
stopifnot(
  !"popup_html" %in% names(retained_only),
  !"pt_reference_hover_html" %in% names(retained_only)
)

trails <- pt_prepare_local_reference_trails(source_sf, validate_snapshot = TRUE)
expect_equal(trails$pt_trails_nlcs_id, fixture$NLCS_ID, "preserved NLCS_ID")
expect_equal(trails$pt_trails_source_name, fixture$NLCS_NAME, "preserved raw NLCS_NAME")
expect_equal(trails$pt_trails_source_segment, fixture$NSHT_SGMNT_NO, "preserved source segment")
expect_equal(trails$pt_trails_global_id, fixture$GlobalID, "local GlobalID coverage")
expect_equal(
  trails$pt_local_reference_geometry_components,
  fixture$fixture_geometry_components,
  "prepared geometry components"
)
stopifnot(
  all(trails$pt_trails_join_method == "exact_nlcs_id"),
  !anyDuplicated(trails$pt_local_reference_semantic_key),
  !anyDuplicated(trails$pt_local_reference_geometry_key),
  all(grepl("^trail:nlcs_id:nlcs", trails$pt_local_reference_semantic_key)),
  all(grepl("^trail:globalid:", trails$pt_local_reference_geometry_key)),
  !any(grepl("globalid", trails$pt_local_reference_semantic_key, fixed = TRUE))
)

reference <- pt_local_reference_trails_reference()
narrative <- pt_local_reference_trails_narrative_provenance()
expect_equal(trails$pt_trails_official_name, reference$official_name, "separate official display names")
expect_equal(nrow(narrative), 18L, "compact narrative provenance rows")
expect_equal(
  as.integer(table(narrative$narrative_field)),
  rep.int(6L, 3L),
  "three narrative contracts for six trails"
)
expect_equal(
  sort(unique(narrative$narrative_field)),
  c(
    "historic_or_scenic_significance", "indigenous_context",
    "trail_summary_short"
  ),
  "expected narrative fields"
)
stopifnot(
  all(narrative$normal_popup_approved),
  all(narrative$source_applicability == "direct_trail"),
  all(grepl("^SRC[0-9]{3}$", narrative$source_register_id)),
  all(grepl("^https://", narrative$source_url)),
  setequal(
    unique(narrative$text_treatment),
    c("curated_summary", "attributed_paraphrase")
  )
)
expect_equal(
  as.integer(table(factor(
    trails$pt_trails_designation_class,
    levels = c("National Historic Trail", "National Scenic Trail")
  ))),
  c(5L, 1L),
  "historic/scenic totals"
)
expect_equal(
  trails$pt_trails_administering_agency,
  c(
    "National Park Service", "National Park Service",
    "National Park Service and Bureau of Land Management (joint trail-wide administration)",
    "National Park Service", "U.S. Forest Service", "National Park Service"
  ),
  "accepted trail-wide administering agencies"
)
expect_equal(
  trails$pt_trails_blm_role,
  c(
    "local_land_manager", "local_land_manager", "program_administrator",
    "local_land_manager", "local_land_manager", "unknown"
  ),
  "accepted BLM roles"
)
stopifnot(
  !any(grepl("Field Office", trails$pt_trails_hover_admin_summary, fixed = TRUE)),
  !any(trails$pt_trails_partner_organization %in% trails$pt_trails_administering_agency)
)
butterfield <- trails[trails$pt_trails_nlcs_id == "NLCS000285", , drop = FALSE]
stopifnot(
  butterfield$pt_trails_blm_role == "unknown",
  grepl("data stewardship", butterfield$pt_trails_blm_role_summary, fixed = TRUE),
  butterfield$pt_trails_management_plan_status == "draft_not_final",
  grepl("2018", butterfield$pt_trails_closest_final_study_title, fixed = TRUE),
  grepl("eight-state", butterfield$pt_trails_source_notes, fixed = TRUE)
)

style_match <- match(trails$pt_local_reference_category_key, categories$category_key)
expect_equal(style_match, 1:6, "exact style assignment by NLCS_ID")
expect_equal(trails$line_col, categories$stroke_color[1:6], "central line colors")
expect_equal(trails$line_weight, categories$stroke_weight[1:6], "central line weights")

expect_equal(
  lengths(strsplit(trails$pt_reference_hover_text, "\n", fixed = TRUE)),
  rep(3L, 6),
  "three-line hover text"
)
expect_equal(
  lengths(regmatches(
    trails$pt_reference_hover_html,
    gregexpr('<div class="pt-trails-hover-line ', trails$pt_reference_hover_html, fixed = TRUE)
  )),
  rep(3L, 6),
  "three trusted hover blocks"
)
stopifnot(
  !any(grepl("<br|&lt;br", trails$pt_reference_hover_html, ignore.case = TRUE)),
  all(grepl("Admin:", trails$pt_reference_hover_text, fixed = TRUE)),
  grepl("Admin: NPS + BLM", trails$pt_reference_hover_text[[3]], fixed = TRUE),
  grepl("Admin: USFS · BLM manages portions", trails$pt_reference_hover_text[[5]], fixed = TRUE),
  grepl("Admin: NPS · BLM role not verified", trails$pt_reference_hover_text[[6]], fixed = TRUE)
)
injection <- pt_local_reference_trails_hover_html(
  "<img src=x onerror=alert(1)>",
  "National Historic Trail",
  "1992",
  "Admin: NPS"
)
stopifnot(!grepl("<img", injection, fixed = TRUE), grepl("&lt;img", injection, fixed = TRUE))

count_fixed <- function(text, pattern) {
  lengths(regmatches(text, gregexpr(pattern, text, fixed = TRUE)))
}
expect_equal(
  count_fixed(trails$popup_html, "data-pt-lr-popup-tab="),
  rep(4L, 6),
  "four popup tabs"
)
expect_equal(
  count_fixed(trails$popup_html, "data-pt-lr-popup-panel="),
  rep(4L, 6),
  "four popup panels"
)
stopifnot(
  all(grepl("data-pt-lr-tabbed-popup", trails$popup_html, fixed = TRUE)),
  all(grepl('role="tablist"', trails$popup_html, fixed = TRUE)),
  all(grepl('aria-selected="true" tabindex="0">Overview', trails$popup_html, fixed = TRUE)),
  all(grepl('role="tabpanel"', trails$popup_html, fixed = TRUE)),
  all(grepl("Management", trails$popup_html, fixed = TRUE)),
  all(grepl("History &amp; context", trails$popup_html, fixed = TRUE)),
  all(grepl("Resources", trails$popup_html, fixed = TRUE)),
  all(grepl("Route representation", trails$popup_html, fixed = TRUE)),
  sum(grepl("Historical significance", trails$popup_html, fixed = TRUE)) == 5L,
  sum(grepl("History and cultural context", trails$popup_html, fixed = TRUE)) == 1L,
  all(grepl("Indigenous and Tribal context", trails$popup_html, fixed = TRUE)),
  all(grepl("Attributed paraphrase", trails$popup_html, fixed = TRUE)),
  all(grepl("Raw NLCS_NAME:", trails$popup_html, fixed = TRUE)),
  all(grepl("GlobalID (geometry/audit only):", trails$popup_html, fixed = TRUE)),
  all(grepl("Partner:", trails$popup_html, fixed = TRUE)),
  !any(grepl("Significance and context", trails$popup_html, fixed = TRUE)),
  !any(grepl("Designation and administration", trails$popup_html, fixed = TRUE)),
  !any(grepl("pt-lr-popup-label\">:</span>", trails$popup_html, fixed = TRUE)),
  !any(grepl(
    "Interpretation should|Visitor language should|should not be reduced|more than 70 Tribal",
    trails$popup_html,
    ignore.case = TRUE
  )),
  !any(grepl(
    "congress_search|courtlistener|google_scholar|wikipedia|web_search|nepa_search",
    trails$popup_html,
    ignore.case = TRUE
  )),
  grepl("draft, not final", butterfield$popup_html, fixed = TRUE),
  grepl("Special Resource Study", butterfield$popup_html, fixed = TRUE),
  grepl("BLM role not verified", butterfield$pt_reference_hover_text, fixed = TRUE)
)

california_index <- match("NLCS000280", trails$pt_trails_nlcs_id)
california_narrative <- narrative[
  narrative$source_nlcs_id == "NLCS000280" &
    narrative$narrative_field == "indigenous_context",
  ,
  drop = FALSE
]
expect_equal(
  reference$indigenous_context[[1]],
  paste(
    "The route network crosses the homelands of many Indigenous nations.",
    "Interpretation should address Indigenous persistence as well as",
    "displacement, violence, disease, resource loss, and other lasting effects",
    "of mass westward migration."
  ),
  "package-origin California narrative retained for audit"
)
expect_equal(
  trails$pt_trails_indigenous_context[[california_index]],
  california_narrative$narrative_text[[1]],
  "approved California narrative drives display"
)
stopifnot(
  california_narrative$source_register_id == "SRC012",
  california_narrative$package_origin_line == 2L,
  california_narrative$override_origin_line == 4L,
  grepl("Four National Historic Trails", california_narrative$source_title, fixed = TRUE),
  grepl(california_narrative$source_url, trails$popup_html[[california_index]], fixed = TRUE),
  !grepl(reference$indigenous_context[[1]], trails$popup_html[[california_index]], fixed = TRUE)
)

trail_layers <- list(trails = trails)
payload <- pt_local_reference_controller_payload(
  trail_layers,
  pt_build_registered_local_reference_label_children(trail_layers)
)
expect_equal(length(payload), 1L, "single Trails payload")
trail_payload <- payload[[1]]
expect_equal(trail_payload$semantic_labels$semantic_feature_count, 6L, "six Trail labels")
expect_equal(trail_payload$semantic_labels$anchor_count, 6L, "six Trail anchors")
expect_equal(
  trail_payload$semantic_labels$anchor_strategy,
  "line_semantic_longest_component_midpoint",
  "Trail anchor strategy"
)
expect_equal(length(trail_payload$categories), 6L, "six visible category rows")
expect_equal(length(trail_payload$features), 6L, "six semantic feature catalog rows")
expect_equal(length(trail_payload$records), 6L, "six source records")
expect_equal(trail_payload$primary_count_mode, "semantic_feature", "semantic primary counts")
expect_equal(trail_payload$primary_count_label, "trails", "plain-language count label")
expect_equal(trail_payload$popup_layout, "tabbed_card", "Trails popup payload layout")
stopifnot(
  !trail_payload$show_category_count,
  !trail_payload$show_component_count,
  sum(vapply(trail_payload$records, `[[`, integer(1), "geometry_component_count")) == 177L
)

search_text <- vapply(trail_payload$features, `[[`, character(1), "search_text")
for (query in c(
  "pct", "pacific crest", "anza", "juan bautista de anza",
  "pony express", "butterfield", "old spanish", "california trail"
)) {
  stopifnot(any(grepl(query, search_text, fixed = TRUE)))
}
stopifnot(all(vapply(trail_payload$features, function(feature) {
  length(feature$semantic_feature_bounds) == 4L &&
    all(is.finite(feature$semantic_feature_bounds))
}, logical(1))))

qa <- pt_local_reference_trails_reconciliation_qa(trails)
expect_equal(nrow(qa), 6L, "six-row reconciliation")
expect_equal(sum(qa$geometry_component_count), 177L, "reconciliation components")
expect_equal(qa$join_method, rep("exact_nlcs_id", 6), "reconciliation join method")
expect_equal(sum(nzchar(qa$unresolved_discrepancies)), 5L, "officialized package-name notes")

controller_source <- paste(readLines(
  file.path("03_functions", "js", "brim_local_reference_controller.js"),
  warn = FALSE
), collapse = "\n")
drawing_source <- paste(readLines(
  file.path("03_functions", "leaflet_layer_local_reference_helpers.r"),
  warn = FALSE
), collapse = "\n")
stopifnot(
  grepl("pt-trails-hover-tooltip", controller_source, fixed = TRUE),
  grepl("pt-trails-hover-tooltip", drawing_source, fixed = TRUE),
  grepl('lapply(x$pt_reference_hover_html, htmltools::HTML)', drawing_source, fixed = TRUE),
  grepl('layerId = ~pt_local_reference_geometry_key', drawing_source, fixed = TRUE),
  grepl('className = "pt-local-reference-tabbed-popup"', drawing_source, fixed = TRUE),
  grepl('autoPan = TRUE', drawing_source, fixed = TRUE),
  grepl('autoPanPaddingTopLeft = c(16, 84)', drawing_source, fixed = TRUE),
  grepl('autoPanPaddingBottomRight = c(16, 24)', drawing_source, fixed = TRUE),
  !grepl('popupOptions = leaflet::popupOptions(autoPan = FALSE)', drawing_source, fixed = TRUE),
  grepl("pointer:coarse", controller_source, fixed = TRUE),
  grepl("layerData.show_category_count", controller_source, fixed = TRUE),
  grepl("activatePopupTab", controller_source, fixed = TRUE),
  grepl("closeLayerPopup", controller_source, fixed = TRUE),
  grepl("pt-lr-tabbed-popup-open", controller_source, fixed = TRUE),
  grepl("listen(map, 'popupclose', onAnyPopupClose)", controller_source, fixed = TRUE),
  grepl("ArrowRight", controller_source, fixed = TRUE),
  grepl("preventScroll", controller_source, fixed = TRUE),
  grepl("max-height:min(72vh,620px)", controller_source, fixed = TRUE),
  grepl("height:var(--pt-lr-popup-panel-height,auto)", controller_source, fixed = TRUE),
  grepl("max-height:min(54vh,450px)", controller_source, fixed = TRUE),
  grepl("max-height:min(50vh,390px)", controller_source, fixed = TRUE),
  !grepl(".pt-lr-popup-panel-scroll{height:min(", controller_source, fixed = TRUE),
  grepl("tallestNaturalPanelHeight", controller_source, fixed = TRUE),
  grepl("var floor = Math.min(112, cap)", controller_source, fixed = TRUE),
  grepl("naturalHeight + 1", controller_source, fixed = TRUE),
  grepl("window.ResizeObserver", controller_source, fixed = TRUE),
  grepl("document.fonts.ready", controller_source, fixed = TRUE),
  grepl("pt-lr-popup-measuring", controller_source, fixed = TRUE),
  grepl("state.popup._updateLayout", controller_source, fixed = TRUE),
  grepl("onTabbedPopupDetailsToggle", controller_source, fixed = TRUE),
  grepl("listenDom(el, 'toggle', onTabbedPopupDetailsToggle, true)", controller_source, fixed = TRUE),
  grepl("teardownInactiveLayer", controller_source, fixed = TRUE),
  grepl("detachOwnedGeometry", controller_source, fixed = TRUE),
  grepl("group_member_layer_count", controller_source, fixed = TRUE),
  grepl("attached_owned_layer_count", controller_source, fixed = TRUE),
  !grepl("cloneNode(true)", controller_source, fixed = TRUE),
  grepl("grid-template-columns:repeat(2", controller_source, fixed = TRUE),
  grepl(
    ".leaflet-container.pt-lr-tabbed-popup-open .leaflet-control-container{visibility:hidden}",
    controller_source,
    fixed = TRUE
  ),
  grepl(
    ".pt-local-reference-tabbed-popup-card button:enabled,.pt-local-reference-tabbed-popup-card summary{cursor:pointer}",
    controller_source,
    fixed = TRUE
  ),
  grepl(
    ".pt-local-reference-tabbed-popup-card button:disabled{cursor:not-allowed}",
    controller_source,
    fixed = TRUE
  )
)

tracked_sources <- c(
  "00_config/config_local_reference_interactions.r",
  "00_config/local_reference_trails_narrative_provenance.csv",
  "03_functions/local_reference_interaction_helpers.r",
  "02_preprocess/11_reference_layers_batch.r",
  "05_map_build/02_cache_blocks/05_cache_final_point_tweaks.r",
  "05_map_build/07_refresh_local_reference_trails_cache.r",
  "03_functions/leaflet_layer_local_reference_helpers.r",
  "03_functions/js/brim_local_reference_controller.js"
)
source_text <- paste(unlist(lapply(tracked_sources, readLines, warn = FALSE)), collapse = "\n")
stopifnot(
  !grepl(paste0("/", "Users", "/"), source_text, fixed = TRUE)
)
## Generic discovery/search URLs remain excluded from the Trails sidecars and
## popup contract. Federal Wilderness uses its own keyed secondary-research
## fields in the shared helper, so their names are no longer forbidden globally.
stopifnot(!any(grepl(
  "congress_search_url|courtlistener_search_url|web_search_url",
  names(pt_local_reference_trails_reference())
)))

message("Local Reference Phase 2 Trails R/config/join/hover/popup tests passed.")
