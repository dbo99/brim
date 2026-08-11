#!/usr/bin/env Rscript

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

count_fixed <- function(text, pattern) {
  lengths(regmatches(text, gregexpr(pattern, text, fixed = TRUE)))
}

pt_validate_local_reference_config()
expect_equal(
  LOCAL_REFERENCE_INTERACTION_REGISTRY$layer_id,
  PT_LOCAL_REFERENCE_LAYER_IDS,
  "exact 11-layer registry"
)
stopifnot(all(c(
  "primary_count_mode", "primary_count_label",
  "show_component_count", "show_category_count", "category_filter_visible", "category_count_mode",
  "component_count_label",
  "category_heading", "card_caution", "popup_layout",
  "feature_selection_supported", "feature_selection_mode",
  "feature_search_fields", "feature_display_field",
  "auto_zoom_supported", "auto_zoom_default", "zoom_padding", "zoom_max",
  "preserve_view_on_reset", "distinguish_units_supported", "filter_facets", "quick_views"
) %in% names(LOCAL_REFERENCE_INTERACTION_REGISTRY)))
stopifnot(all(LOCAL_REFERENCE_INTERACTION_REGISTRY$primary_count_mode == "semantic_feature"))
wsa_registry <- LOCAL_REFERENCE_INTERACTION_REGISTRY[
  LOCAL_REFERENCE_INTERACTION_REGISTRY$layer_id == "wilderness_study_areas",
  , drop = FALSE
]
expect_equal(wsa_registry$primary_count_label, "Wilderness Study Areas", "WSA primary count label")
expect_equal(wsa_registry$popup_layout, "tabbed_card", "WSA tabbed popup layout")
expect_equal(
  which(LOCAL_REFERENCE_INTERACTION_REGISTRY$popup_layout == "tabbed_card"),
  c(1L, 2L, 3L, 4L, 5L, 7L),
  "active Trails, National Monuments, CA Desert NCL, WSA, Federal Wilderness, and ACEC use the shared tabbed popup shell"
)
stopifnot(!isTRUE(wsa_registry$show_component_count))
stopifnot(isTRUE(wsa_registry$feature_selection_supported))
expect_equal(wsa_registry$feature_selection_mode, "semantic_feature_multi", "WSA selection mode")
expect_equal(wsa_registry$feature_display_field, "pt_wsa_name", "WSA selection display field")
expect_equal(
  unlist(wsa_registry$feature_search_fields[[1]], use.names = FALSE),
  c("NLCS_NAME", "WSACODE_ca", "CASEFILE_N", "NLCS_ID", "GlobalID"),
  "WSA approved selection search fields"
)
stopifnot(isTRUE(wsa_registry$auto_zoom_supported))
stopifnot(isTRUE(wsa_registry$auto_zoom_default))
expect_equal(wsa_registry$zoom_padding, 36, "WSA zoom padding")
expect_equal(wsa_registry$zoom_max, 12, "WSA maximum zoom")
stopifnot(isTRUE(wsa_registry$preserve_view_on_reset))
expect_equal(
  which(LOCAL_REFERENCE_INTERACTION_REGISTRY$feature_selection_supported),
  c(1L, 2L, 3L, 4L, 5L, 7L),
  "current selection-enabled rows"
)
expect_equal(
  which(LOCAL_REFERENCE_INTERACTION_REGISTRY$auto_zoom_supported),
  c(1L, 2L, 3L, 4L, 5L, 7L),
  "current Auto-zoom-enabled rows"
)
expect_equal(
  wsa_registry$category_heading,
  "BLM recommendation for wilderness designation",
  "WSA category heading"
)
expect_equal(
  sum(nzchar(LOCAL_REFERENCE_INTERACTION_REGISTRY$category_heading)),
  3L,
  "three configured category headings"
)

fixture_path <- file.path(
  "qa", "fixtures", "local_reference_wsa_source_snapshot.csv"
)
fixture <- utils::read.csv(
  fixture_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("")
)
stopifnot(nrow(fixture) == 63L)
expect_equal(sum(!is.na(fixture$NLCS_ID) & nzchar(fixture$NLCS_ID)), 61L, "NLCS ID count")
expect_equal(
  sum((is.na(fixture$NLCS_ID) | !nzchar(fixture$NLCS_ID)) & nzchar(fixture$GlobalID)),
  2L,
  "GlobalID fallback count"
)
expect_equal(sum(fixture$fixture_geometry_components), 104L, "geometry component snapshot")
expect_equal(
  as.integer(table(factor(
    ifelse(is.na(fixture$WSA_RCMND), "", fixture$WSA_RCMND),
    levels = c("Suitable", "Non-suitable", "No recommendation", "")
  ))),
  c(4L, 46L, 11L, 2L),
  "raw WSA_RCMND source values"
)

retained <- pt_local_reference_retained_source_fields(
  "wildernessstudyarea",
  names(fixture)
)
expect_equal(length(retained), 13L, "focused retained field count")
stopifnot(all(retained %in% names(fixture)))

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("The Phase 1 WSA test requires the project's sf dependency.")
}

make_fixture_geometry <- function(component_count, area_acres, row_index) {
  component_count <- as.integer(component_count)
  area_m2 <- as.numeric(area_acres) * 4046.8564224
  side <- sqrt(area_m2 / component_count)
  polygons <- lapply(seq_len(component_count), function(component_index) {
    x0 <- row_index * 200000 + component_index * (side + 20)
    y0 <- row_index * 1000
    list(matrix(c(
      x0, y0,
      x0 + side, y0,
      x0 + side, y0 + side,
      x0, y0 + side,
      x0, y0
    ), ncol = 2, byrow = TRUE))
  })
  sf::st_multipolygon(polygons)
}

fixture_geometry <- sf::st_sfc(
  lapply(seq_len(nrow(fixture)), function(i) {
    make_fixture_geometry(
      fixture$fixture_geometry_components[[i]],
      fixture$fixture_calculated_geometry_acres[[i]],
      i
    )
  }),
  crs = 3310
)
source_sf <- sf::st_sf(
  fixture[, setdiff(names(fixture), c(
    "fixture_geometry_components", "fixture_calculated_geometry_acres"
  )), drop = FALSE],
  geometry = fixture_geometry
)
source_sf$pt_display_name <- "Wilderness Study Areas *"
source_sf$pt_geom_type <- "polygon"

wsa_retained <- pt_prepare_local_reference_wsa(
  source_sf,
  validate_snapshot = TRUE,
  build_display = FALSE
)
stopifnot(!"popup_html" %in% names(wsa_retained))
stopifnot(!"pt_reference_hover_text" %in% names(wsa_retained))

wsa <- pt_prepare_local_reference_wsa(source_sf, validate_snapshot = TRUE)
expect_equal(sum(wsa$pt_local_reference_geometry_components), 104L, "prepared geometry component count")
expect_equal(
  wsa$pt_wsa_source_gis_acres,
  suppressWarnings(as.numeric(fixture$GIS_ACRES)),
  "unchanged raw GIS_ACRES values"
)
expect_equal(
  as.integer(table(factor(
    wsa$pt_wsa_join_status,
    levels = c("matched_exact_alias", "unmatched_manual_review", "source_only")
  ))),
  c(59L, 2L, 2L),
  "source join coverage"
)
expect_equal(
  as.integer(table(factor(
    wsa$pt_local_reference_category_key,
    levels = c("suitable", "non_suitable", "no_recommendation", "unknown")
  ))),
  c(4L, 46L, 11L, 2L),
  "recommendation category coverage"
)
expect_equal(
  as.integer(table(factor(
    wsa$pt_wsa_recommendation_raw,
    levels = c("Suitable", "Non-suitable", "No recommendation", "")
  ))),
  c(4L, 46L, 11L, 2L),
  "retained raw WSA_RCMND values"
)
expect_equal(
  count_fixed(wsa$popup_html, "data-pt-lr-popup-tab="),
  rep(4L, 63L),
  "WSA four-tab structure"
)
expect_equal(
  count_fixed(wsa$popup_html, "data-pt-lr-popup-panel="),
  rep(4L, 63L),
  "WSA four-panel structure"
)
stopifnot(
  all(grepl("data-pt-lr-tabbed-popup", wsa$popup_html, fixed = TRUE)),
  all(grepl("pt-local-reference-tabbed-popup-card pt-wsa-popup", wsa$popup_html, fixed = TRUE)),
  all(grepl('aria-label="Wilderness Study Area details"', wsa$popup_html, fixed = TRUE)),
  all(grepl('aria-selected="true" tabindex="0">Overview', wsa$popup_html, fixed = TRUE)),
  all(grepl('>Recommendation</button>', wsa$popup_html, fixed = TRUE)),
  all(grepl('>Management</button>', wsa$popup_html, fixed = TRUE)),
  all(grepl('>Sources &amp; details</button>', wsa$popup_html, fixed = TRUE)),
  !any(grepl("<h3></h3>|<span class=\"pt-lr-popup-label\"></span>", wsa$popup_html))
)

suppressed_tab_fixture <- pt_local_reference_tabbed_popup(
  popup_key = "conditional-fixture",
  title = "Conditional fixture",
  designation_badge = "Fixture",
  tabs = list(
    list(key = "overview", label = "Overview", html = "<p>Present</p>"),
    list(key = "empty", label = "Empty section", html = "")
  )
)
expect_equal(
  count_fixed(suppressed_tab_fixture, "data-pt-lr-popup-tab="),
  1L,
  "empty tab suppression"
)
expect_equal(
  count_fixed(suppressed_tab_fixture, "data-pt-lr-popup-panel="),
  1L,
  "empty panel suppression"
)
stopifnot(!grepl("Empty section", suppressed_tab_fixture, fixed = TRUE))

key_basis <- ifelse(nzchar(wsa$pt_wsa_nlcs_id), "NLCS_ID", "GlobalID")
expect_equal(as.integer(table(factor(key_basis, levels = c("NLCS_ID", "GlobalID")))), c(61L, 2L), "feature-key basis")
stopifnot(!anyDuplicated(wsa$pt_local_reference_feature_key))
stopifnot(!anyDuplicated(wsa$pt_local_reference_geometry_key))
stopifnot(all(grepl(":geometry:1$", wsa$pt_local_reference_geometry_key)))
stopifnot(all(wsa$pt_management_local_managing_agency == "Bureau of Land Management"))
stopifnot(all(wsa$pt_management_blm_role == "local_land_manager"))
stopifnot(all(wsa$pt_management_role_confidence == "verified_layer_family"))
stopifnot(all(grepl("Local managing agency:</span> Bureau of Land Management", wsa$popup_html, fixed = TRUE)))
verified_office <- nzchar(pt_local_reference_clean_chr(wsa$managing_office))
expect_equal(sum(verified_office), 8L, "directly verified responsible-office count")
expect_equal(
  sum(grepl("BLM office:", wsa$pt_reference_hover_text, fixed = TRUE)),
  8L,
  "hover responsible-office coverage"
)
expect_equal(
  sum(grepl("Verified responsible office:</span>", wsa$popup_html, fixed = TRUE)),
  8L,
  "popup responsible-office coverage"
)
stopifnot(all(grepl("Wilderness Study Area", wsa$pt_wsa_designation_subtitle, fixed = TRUE)))
stopifnot(all(grepl("FLPMA", wsa$pt_reference_hover_text, fixed = TRUE)))
stopifnot(!any(grepl(
  "GIS area|GIS acre",
  wsa$pt_reference_hover_text,
  ignore.case = TRUE
)))
positive_source_area <- is.finite(wsa$pt_wsa_source_gis_acres) &
  wsa$pt_wsa_source_gis_acres > 0
expected_hover_area <- pt_local_reference_format_square_miles_from_acres(
  wsa$pt_wsa_source_gis_acres
)
stopifnot(all(vapply(which(positive_source_area), function(i) {
  grepl(expected_hover_area[[i]], wsa$pt_reference_hover_text[[i]], fixed = TRUE)
}, logical(1))))
stopifnot(all(vapply(which(!positive_source_area), function(i) {
  !grepl("mi²", wsa$pt_reference_hover_text[[i]], fixed = TRUE)
}, logical(1))))
stopifnot(!any(grepl("Case file:</span> Not stated|WSA code:</span> Not stated|>NA<|<NA>", wsa$popup_html)))
expect_equal(
  sum(grepl("Raw WSA_RCMND source value:</span>", wsa$popup_html, fixed = TRUE)),
  63L,
  "popup technical raw recommendation coverage"
)
stopifnot(all(grepl("Recommendation context:</span>", wsa$popup_html, fixed = TRUE)))
stopifnot(all(grepl("FLPMA §603 study areas and §202 areas", wsa$popup_html, fixed = TRUE)))
stopifnot(!any(grepl("protections continue pending Congressional action", wsa$popup_html, fixed = TRUE)))
stopifnot(all(lengths(strsplit(wsa$pt_reference_hover_text, "\n", fixed = TRUE)) <= 3L))
stopifnot(all(lengths(strsplit(wsa$pt_reference_hover_text[verified_office], "\n", fixed = TRUE)) == 3L))
stopifnot(all(lengths(strsplit(wsa$pt_reference_hover_text[!verified_office], "\n", fixed = TRUE)) == 2L))
stopifnot(all(!grepl("\n", wsa$pt_reference_hover_html, fixed = TRUE)))
stopifnot(all(!grepl("<br", wsa$pt_reference_hover_html, fixed = TRUE)))
stopifnot(all(!grepl("&lt;br", wsa$pt_reference_hover_html, fixed = TRUE)))
stopifnot(all(grepl('<div class="pt-wsa-hover-lines">', wsa$pt_reference_hover_html, fixed = TRUE)))
expect_equal(
  sum(lengths(regmatches(
    wsa$pt_reference_hover_html,
    gregexpr('<div class="pt-wsa-hover-line ', wsa$pt_reference_hover_html, fixed = TRUE)
  ))),
  sum(lengths(strsplit(wsa$pt_reference_hover_text, "\n", fixed = TRUE))),
  "hover HTML semantic line blocks"
)

buffalo <- wsa[pt_local_reference_normalize_text(wsa$pt_wsa_name) == "buffalo hills", , drop = FALSE]
expect_equal(nrow(buffalo), 1L, "Buffalo Hills record")
stopifnot(grepl("BLM office: Eagle Lake / Applegate", buffalo$pt_reference_hover_text, fixed = TRUE))
stopifnot(grepl("BLM office: Eagle Lake / Applegate", buffalo$pt_reference_hover_html, fixed = TRUE))
stopifnot(grepl(
  "FLPMA §603 · Recommended non-suitable · ~74.2 mi²",
  buffalo$pt_reference_hover_text,
  fixed = TRUE
))
stopifnot(grepl(
  "GIS acreage:</span> 47,510.7 acres",
  buffalo$popup_html,
  fixed = TRUE
))
stopifnot(identical(
  lengths(regmatches(
    buffalo$pt_reference_hover_html,
    gregexpr('<div class="pt-wsa-hover-line ', buffalo$pt_reference_hover_html, fixed = TRUE)
  )),
  3L
))
stopifnot(!grepl("Field Office", buffalo$pt_reference_hover_text, fixed = TRUE))
stopifnot(grepl(
  "Verified responsible office:</span> Eagle Lake Field Office / Applegate Field Office",
  buffalo$popup_html,
  fixed = TRUE
))

rod_present <- nzchar(pt_local_reference_clean_chr(wsa$pt_wsa_rod_date))
expect_equal(sum(rod_present), 61L, "source ROD-date coverage")
expect_equal(
  sum(grepl("Record of Decision date:</span>", wsa$popup_html, fixed = TRUE)),
  61L,
  "visible popup ROD-date coverage"
)

manual_names <- PT_LOCAL_REFERENCE_WSA_MANUAL_REVIEW$source_name
manual <- wsa[
  pt_local_reference_normalize_text(wsa$pt_wsa_name) %in%
    pt_local_reference_normalize_text(manual_names),
  , drop = FALSE
]
expect_equal(nrow(manual), 2L, "manual-review source records")
stopifnot(all(manual$pt_wsa_join_status == "unmatched_manual_review"))
stopifnot(all(is.na(manual$reference_name)))
stopifnot(!any(grepl("San Benito Mountain ISA|Trinity Alps Subunit", manual$popup_html)))

source_only <- wsa[wsa$pt_wsa_name %in% PT_LOCAL_REFERENCE_WSA_SOURCE_ONLY, , drop = FALSE]
expect_equal(nrow(source_only), 2L, "source-only records")
stopifnot(all(source_only$pt_wsa_join_status == "source_only"))

joins <- pt_local_reference_wsa_join_qa(wsa)
seed_exceptions <- joins$seed[joins$seed$seed_status != "matched_exact_alias", , drop = FALSE]
expect_equal(
  seed_exceptions$seed_reference_name,
  c(
    "Brushy Mountain / English Ridge Subunit",
    "Carson Iceberg",
    "San Benito Mountain ISA",
    "Trinity Alps Subunit"
  ),
  "seed exceptions in seed order"
)
expect_equal(
  seed_exceptions$seed_status,
  c("seed_only", "seed_only", "unmatched_manual_review", "unmatched_manual_review"),
  "seed exception statuses"
)

red <- wsa[pt_local_reference_normalize_text(wsa$pt_wsa_name) == "red mountain", , drop = FALSE]
expect_equal(nrow(red), 1L, "Red Mountain record")
stopifnot(identical(red$pt_wsa_source_gis_acres[[1]], 0))
stopifnot(abs(red$pt_wsa_calculated_geometry_acres[[1]] - 317.863062) < 0.01)
stopifnot(grepl("GIS acreage:</span> 0 acres", red$popup_html, fixed = TRUE))
stopifnot(!grepl("GIS acreage:</span> 0.0 acres", red$popup_html, fixed = TRUE))
stopifnot(grepl("Approximate geometry-derived anomaly:</b>", red$popup_html, fixed = TRUE))
stopifnot(grepl("source anomaly only", red$popup_html, fixed = TRUE))
stopifnot(grepl("FLPMA not stated · Not stated", red$pt_reference_hover_text, fixed = TRUE))
stopifnot(!grepl("mi²|GIS area|GIS acre", red$pt_reference_hover_text, ignore.case = TRUE))

category <- pt_local_reference_categories("wilderness_study_areas")
expect_equal(
  category$label,
  c(
    "Recommended suitable", "Recommended non-suitable",
    "No recommendation", "Not stated"
  ),
  "WSA visible category labels"
)
stopifnot(!any(category$label %in% c("Suitable", "Non-suitable")))
expect_equal(
  category$source_values,
  c("SUITABLE", "NON-SUITABLE|NON SUITABLE", "NO RECOMMENDATION", ""),
  "unchanged WSA_RCMND category source values"
)
style_match <- match(wsa$pt_local_reference_category_key, category$category_key)
stopifnot(identical(wsa$fill_col, category$fill_color[style_match]))
stopifnot(identical(wsa$line_col, category$stroke_color[style_match]))
stopifnot(identical(wsa$fill_opacity, category$fill_opacity[style_match]))
stopifnot(identical(wsa$line_weight, category$stroke_weight[style_match]))
stopifnot(identical(wsa$line_dash, category$dash_array[style_match]))

wsa_layers <- list(wildernessstudyarea = wsa)
payload <- pt_local_reference_controller_payload(
  wsa_layers,
  pt_build_registered_local_reference_label_children(wsa_layers)
)
expect_equal(length(payload), 1L, "Phase 1 controller payload count")
expect_equal(payload[[1]]$layer_id, "wilderness_study_areas", "WSA controller layer")
expect_equal(payload[[1]]$semantic_labels$semantic_feature_count, 63L, "WSA semantic labels")
expect_equal(payload[[1]]$semantic_labels$anchor_count, 63L, "WSA label anchors")
expect_equal(
  payload[[1]]$semantic_labels$anchor_strategy,
  "polygon_semantic_point_on_surface",
  "WSA anchor strategy"
)
stopifnot(isTRUE(payload[[1]]$auto_supported), isTRUE(payload[[1]]$auto_default))
stopifnot(isTRUE(payload[[1]]$feature_selection_supported))
expect_equal(payload[[1]]$feature_selection_mode, "semantic_feature_multi", "WSA payload selection mode")
expect_equal(
  payload[[1]]$feature_search_fields,
  c("NLCS_NAME", "WSACODE_ca", "CASEFILE_N", "NLCS_ID", "GlobalID"),
  "WSA payload search fields"
)
expect_equal(payload[[1]]$feature_display_field, "pt_wsa_name", "WSA payload display field")
stopifnot(isTRUE(payload[[1]]$auto_zoom_supported))
stopifnot(isTRUE(payload[[1]]$auto_zoom_default))
expect_equal(payload[[1]]$zoom_padding, 36, "WSA payload zoom padding")
expect_equal(payload[[1]]$zoom_max, 12, "WSA payload maximum zoom")
stopifnot(isTRUE(payload[[1]]$preserve_view_on_reset))
expect_equal(payload[[1]]$primary_count_mode, "semantic_feature", "WSA primary count mode")
expect_equal(payload[[1]]$primary_count_label, "Wilderness Study Areas", "WSA payload count label")
stopifnot(!isTRUE(payload[[1]]$show_component_count))
expect_equal(
  payload[[1]]$category_heading,
  "BLM recommendation for wilderness designation",
  "WSA payload category heading"
)
expect_equal(
  payload[[1]]$caution,
  paste(
    "Historical recommendation, not current WSA status.",
    "Management continues under the applicable FLPMA authority;",
    "verify current plans, closures, and field-office direction."
  ),
  "WSA card caution"
)
stopifnot(!grepl("protections continue pending Congressional action", payload[[1]]$caution, fixed = TRUE))
expect_equal(length(payload[[1]]$records), 63L, "WSA controller records")
expect_equal(length(payload[[1]]$features), 63L, "WSA semantic feature catalog")
expect_equal(length(payload[[1]]$categories), 4L, "WSA controller categories")
payload_feature_keys <- vapply(
  payload[[1]]$features,
  function(feature) feature$semantic_feature_key,
  character(1)
)
expect_equal(
  payload_feature_keys,
  wsa$pt_local_reference_semantic_key,
  "WSA semantic feature catalog order"
)
stopifnot(!anyDuplicated(payload_feature_keys))
stopifnot(all(vapply(payload[[1]]$features, function(feature) {
  identical(length(feature$semantic_feature_bounds), 4L) &&
    all(is.finite(feature$semantic_feature_bounds))
}, logical(1))))
stopifnot(!any(vapply(payload[[1]]$records, function(record) {
  "semantic_feature_bounds" %in% names(record)
}, logical(1))))

multipart_index <- which.max(wsa$pt_local_reference_geometry_components)
multipart_feature <- payload[[1]]$features[[multipart_index]]
multipart_bbox <- sf::st_bbox(sf::st_transform(wsa[multipart_index, ], 4326))
multipart_expected_bounds <- unname(as.numeric(c(
  multipart_bbox[["ymin"]], multipart_bbox[["xmin"]],
  multipart_bbox[["ymax"]], multipart_bbox[["xmax"]]
)))
stopifnot(isTRUE(all.equal(
  multipart_feature$semantic_feature_bounds,
  multipart_expected_bounds,
  tolerance = 1e-10
)))
expect_equal(
  multipart_feature$geometry_component_count,
  wsa$pt_local_reference_geometry_components[[multipart_index]],
  "multipart WSA semantic bounds component coverage"
)

unknown_payload_features <- Filter(
  function(feature) "unknown" %in% feature$category_keys,
  payload[[1]]$features
)
expect_equal(length(unknown_payload_features), 2L, "Not-stated selection catalog coverage")
stopifnot(all(vapply(unknown_payload_features, function(feature) {
  nzchar(feature$display_name) && nzchar(feature$search_text)
}, logical(1))))
expect_equal(
  vapply(payload[[1]]$categories, function(x) x$label, character(1)),
  c(
    "Recommended suitable", "Recommended non-suitable",
    "No recommendation", "Not stated"
  ),
  "WSA payload category labels"
)

qa_dir <- tempfile("local-reference-phase1-qa-")
qa_paths <- pt_write_local_reference_wsa_qa(wsa, qa_dir)
stopifnot(length(qa_paths) == 3L, all(file.exists(qa_paths)))

artifact_dir <- file.path(
  "qa", "artifacts", "local_reference_phase1_checkpoint"
)
artifact_registry <- utils::read.csv(
  file.path(artifact_dir, "local_reference_registry_contract.csv"),
  stringsAsFactors = FALSE
)
expect_equal(artifact_registry$layer_id, PT_LOCAL_REFERENCE_LAYER_IDS, "registry QA artifact scope")
stopifnot(all(c(
  "count_mode", "primary_count_mode", "primary_count_label",
  "show_component_count", "component_count_label", "category_heading",
  "feature_selection_supported", "feature_selection_mode",
  "feature_search_fields", "feature_display_field",
  "auto_zoom_supported", "auto_zoom_default", "zoom_padding", "zoom_max",
  "preserve_view_on_reset"
) %in% names(artifact_registry)))
artifact_wsa_registry <- artifact_registry[
  artifact_registry$layer_id == "wilderness_study_areas",
  , drop = FALSE
]
expect_equal(
  artifact_wsa_registry$primary_count_label,
  "Wilderness Study Areas",
  "registry QA WSA count label"
)
stopifnot(!artifact_wsa_registry$show_component_count)
stopifnot(artifact_wsa_registry$feature_selection_supported)
expect_equal(
  artifact_wsa_registry$feature_selection_mode,
  "semantic_feature_multi",
  "registry QA WSA selection mode"
)
stopifnot(artifact_wsa_registry$auto_zoom_supported)
stopifnot(artifact_wsa_registry$auto_zoom_default)
expect_equal(
  artifact_wsa_registry$category_heading,
  "BLM recommendation for wilderness designation",
  "registry QA WSA category heading"
)
artifact_tokens <- utils::read.csv(
  file.path(artifact_dir, "local_reference_category_tokens.csv"),
  stringsAsFactors = FALSE
)
stopifnot(all(c(
  "fill_color", "stroke_color", "fill_opacity", "stroke_weight",
  "dash_array", "legend_swatch_style"
) %in% names(artifact_tokens)))
stopifnot(all(artifact_tokens$provisional))
artifact_payload_size <- utils::read.csv(
  file.path(artifact_dir, "payload_size_estimate.csv"),
  stringsAsFactors = FALSE
)
payload_size_value <- function(metric) {
  as.integer(artifact_payload_size$value[
    artifact_payload_size$metric == metric
  ])
}
stopifnot(
  payload_size_value("controller_payload_json_bytes") >= 40000L,
  payload_size_value("controller_payload_json_bytes") < 50000L
)
## The accepted Phase 1 artifact is historical. Later Local Reference phases
## intentionally extend the shared engine/controller, so retain the recorded
## checkpoint values without requiring current byte sizes to remain frozen.
stopifnot(
  payload_size_value("filter_engine_js_bytes") > 0L,
  as.integer(file.info(file.path(
    "03_functions", "js", "brim_local_reference_filter_engine.js"
  ))$size) >= payload_size_value("filter_engine_js_bytes"),
  payload_size_value("controller_js_bytes") > 0L,
  as.integer(file.info(file.path(
    "03_functions", "js", "brim_local_reference_controller.js"
  ))$size) >= payload_size_value("controller_js_bytes")
)
artifact_source <- utils::read.csv(
  file.path(artifact_dir, "wsa_source_join.csv"),
  stringsAsFactors = FALSE
)
artifact_seed <- utils::read.csv(
  file.path(artifact_dir, "wsa_seed_coverage.csv"),
  stringsAsFactors = FALSE
)
expect_equal(nrow(artifact_source), 63L, "source QA artifact rows")
expect_equal(nrow(artifact_seed), 63L, "seed QA artifact rows")
stopifnot("raw_wsa_rcmnd" %in% names(artifact_source))
expect_equal(
  as.integer(table(factor(
    artifact_source$raw_wsa_rcmnd,
    levels = c("Suitable", "Non-suitable", "No recommendation", "")
  ))),
  c(4L, 46L, 11L, 2L),
  "source QA raw WSA_RCMND values"
)

message("Local Reference Phase 1 R tests passed.")
