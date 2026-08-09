suppressPackageStartupMessages({
  library(sf)
  library(htmltools)
})

source("00_config/config_local_reference_interactions.r")
source("03_functions/local_reference_interaction_helpers.r")

pt_validate_local_reference_config()
pt_validate_local_reference_acec_research()

components <- pt_local_reference_acec_components()
reference <- pt_local_reference_acec_reference()
wsa_context <- pt_local_reference_acec_wsa_name_context()
overlap_pairs <- pt_local_reference_acec_overlap_pairs()
current_field_offices <- pt_local_reference_acec_current_field_offices()
field_office_context <- pt_local_reference_acec_field_office_context()
reference_index <- match(components$acec_id, reference$acec_id)

overlap_participants <- sort(unique(c(
  overlap_pairs$acec_id_a,
  overlap_pairs$acec_id_b
)))
stopifnot(
  nrow(overlap_pairs) == 27L,
  length(overlap_participants) == 39L,
  sum(overlap_pairs$relationship_type == "containment") == 2L,
  all(overlap_pairs$overlap_area_m2 > 1),
  any(overlap_pairs$overlap_area_m2 < 2),
  any(overlap_pairs$overlap_area_acres > 6000),
  any(
    overlap_pairs$acec_id_a == "acec-163713939580c4" &
      overlap_pairs$acec_id_b == "acec-d716ed657cb21c" &
      overlap_pairs$relationship_type == "containment"
  )
)

cady_wsa <- wsa_context[wsa_context$official_acec_name == "Cady Mountains WSA", ]
cerro_gordo_wsa <- wsa_context[wsa_context$official_acec_name == "Cerro Gordo WSA", ]
stopifnot(
  nrow(wsa_context) == 11L,
  sum(wsa_context$wsa_name_context_status == "current_wsa") == 6L,
  sum(wsa_context$wsa_name_context_status == "historical_name_only") == 5L,
  setequal(
    unique(wsa_context$wsa_name_context_status),
    c("current_wsa", "historical_name_only")
  ),
  identical(cady_wsa$wsa_name_context_status, "historical_name_only"),
  !nzchar(pt_local_reference_clean_chr(cady_wsa$current_wsa_nlcs_id)),
  grepl("not a current Wilderness Study Area", cady_wsa$popup_wording, fixed = TRUE),
  identical(cerro_gordo_wsa$wsa_name_context_status, "current_wsa"),
  identical(cerro_gordo_wsa$current_wsa_nlcs_id, "NLCS000219"),
  identical(cerro_gordo_wsa$popup_wording, "Also a current BLM Wilderness Study Area.")
)

source_fixture <- data.frame(
  OBJECTID = components$source_objectid,
  GlobalID = components$source_globalid,
  ACEC_NAME = components$source_name,
  LUP_NAME = components$component_governing_plan,
  NEPA_NUM = "",
  ROD_DATE = reference$designation_decision_date[reference_index],
  GIS_ACRES = components$component_gis_acres,
  ADMIN_ST = "CA",
  CA_ADMIN_unit_code = reference$source_admin_unit_code[reference_index],
  BLM_MODIFY_DATE = components$component_blm_modify_date,
  last_edited_date = components$component_last_verified,
  ACEC_RLVNCE_CUL = "",
  ACEC_RLVNCE_FRSC = "",
  ACEC_RLVNCE_HIS = "",
  ACEC_RLVNCE_NHAZ = "",
  ACEC_RLVNCE_NPRO = "",
  ACEC_RLVNCE_NSYS = "",
  ACEC_RLVNCE_SCE = "",
  ACEC_RLVNCE_WRSC = "",
  SPCL_MGMT_ATTN_RX_PRTCT = "",
  SPCL_MGMT_ATTN_RX_PRVNT = "",
  fixture_x = seq_len(nrow(components)),
  fixture_y = seq_len(nrow(components)) * 2,
  marker = paste0("raw-", seq_len(nrow(components))),
  pt_nickname = "acec",
  pt_display_name = "ACECs",
  pt_geom_type = "polygon",
  stringsAsFactors = FALSE
)
fixture <- sf::st_as_sf(
  source_fixture, coords = c("fixture_x", "fixture_y"), crs = 3310,
  remove = FALSE
)
attr(fixture, "pt_acec_candidate_metadata") <- list(test_fixture = TRUE)

enriched <- pt_prepare_local_reference_acec(
  fixture, validate_snapshot = TRUE, build_display = FALSE
)
prepared <- pt_prepare_local_reference_acec(
  fixture, validate_snapshot = TRUE, build_display = TRUE
)

runtime_fields <- c(
  "pt_nickname", "pt_display_name", "pt_geom_type", "component_id", "acec_id",
  "pt_local_reference_feature_key", "pt_local_reference_semantic_key",
  "pt_local_reference_geometry_key", "pt_local_reference_geometry_components",
  "pt_local_reference_category_key", "pt_local_reference_category_label",
  "fill_col", "line_col", "fill_opacity", "line_weight", "line_dash",
  "pt_legend_swatch_style", "pt_acec_official_name", "pt_acec_legacy_name",
  "pt_acec_aliases", "pt_acec_governing_plan", "pt_acec_planning_framework",
  "pt_acec_source_admin_unit", "pt_acec_field_office_context",
  "pt_acec_field_office_context_names", "pt_acec_value_families",
  "pt_acec_global_id", "pt_reference_label_text", "pt_reference_hover_html",
  "pt_reference_hover_text", "geometry"
)
stopifnot(
  nrow(prepared) == 238L,
  length(unique(prepared$acec_id)) == 238L,
  !anyDuplicated(prepared$component_id),
  identical(enriched$marker, source_fixture$marker),
  setequal(names(prepared), runtime_fields),
  identical(attr(prepared, "sf_column"), "geometry"),
  is.list(attr(prepared, "pt_acec_candidate_metadata")),
  !"popup_html" %in% names(prepared),
  all(prepared$pt_local_reference_category_key == "acec"),
  sum(prepared$pt_local_reference_geometry_components) == 613L,
  sum(prepared$pt_local_reference_geometry_components > 1L) == 82L,
  sum(prepared$pt_acec_planning_framework == "drecp") == 128L,
  sum(grepl("(^|;)water_aquatic(;|$)", prepared$pt_acec_value_families)) == 34L,
  sum(grepl("(^|;)wildlife_and_habitat(;|$)", prepared$pt_acec_value_families)) == 156L,
  sum(grepl("(^|;)botanical_or_ecological(;|$)", prepared$pt_acec_value_families)) == 192L,
  sum(grepl("(^|;)cultural_archaeological_historic(;|$)", prepared$pt_acec_value_families)) == 126L,
  sum(grepl("(^|;)scenic(;|$)", prepared$pt_acec_value_families)) == 73L,
  sum(grepl("(^|;)other_or_unresolved(;|$)", prepared$pt_acec_value_families)) == 5L,
  all(nzchar(prepared$pt_reference_hover_html)),
  all(grepl("mi²", prepared$pt_reference_hover_html, fixed = TRUE)),
  all(grepl("Plan:", prepared$pt_reference_hover_html, fixed = TRUE)),
  all(grepl("Values:", prepared$pt_reference_hover_html, fixed = TRUE)),
  !any(grepl("\\bacres?\\b", prepared$pt_reference_hover_html, ignore.case = TRUE)),
  !any(grepl(
    "Decision year:|Currently mapped as designated|Source administrative unit|GlobalID",
    prepared$pt_reference_hover_html,
    ignore.case = TRUE
  )),
  !any(grepl("_[[:alnum:]]", prepared$pt_reference_hover_html))
)

black_mountain <- prepared[prepared$pt_acec_official_name == "Black Mountain", ]
stopifnot(
  nrow(black_mountain) == 2L,
  length(unique(black_mountain$acec_id)) == 2L,
  length(unique(black_mountain$pt_acec_governing_plan)) == 2L
)

office_keys <- current_field_offices$office_key[order(current_field_offices$sort_order)]
office_names <- current_field_offices$current_official_name[
  order(current_field_offices$sort_order)
]
office_expected <- c(11L, 11L, 26L, 46L, 18L, 5L, 8L, 21L, 13L, 27L, 29L, 16L, 36L, 9L)
office_observed <- vapply(office_keys, function(key) {
  sum(grepl(
    paste0("(^|;)", key, "(;|$)"),
    prepared$pt_acec_field_office_context
  ))
}, integer(1))
fish_value <- grepl(
  "(^|;)water_aquatic(;|$)", prepared$pt_acec_value_families
)
wildlife_value <- grepl(
  "(^|;)wildlife_and_habitat(;|$)", prepared$pt_acec_value_families
)
stopifnot(
  identical(unname(office_observed), office_expected),
  identical(office_names, sort(office_names, method = "radix")),
  nrow(current_field_offices) == 14L,
  nrow(field_office_context) == 276L,
  length(unique(field_office_context$acec_id)) == 238L,
  all(c(
    "barstow_field_office", "needles_field_office", "ridgecrest_field_office",
    "central_coast_field_office"
  ) %in% office_keys),
  !any(grepl(
    "Hollister|Alturas|Susanville",
    paste(
      current_field_offices$current_official_name,
      prepared$pt_acec_field_office_context_names
    ),
    ignore.case = TRUE
  )),
  sum(nzchar(pt_local_reference_clean_chr(reference$blm_field_offices))) == 112L,
  sum(!nzchar(pt_local_reference_clean_chr(reference$blm_field_offices))) == 126L,
  sum(fish_value | wildlife_value) == 160L,
  sum(fish_value & wildlife_value) == 30L,
  identical(PT_LOCAL_REFERENCE_ACEC_FACETS[[3]]$count_mode, "semantic_feature")
)

payload <- pt_local_reference_controller_payload(list(acec = prepared))
acec_payload <- payload[[1]]$acec
first_value_facet <- payload[[1]]$records[[1]]$facet_values$relevant_value_family
quick_view_keys <- vapply(
  payload[[1]]$quick_views, `[[`, character(1), "quick_view_key"
)
value_swatches <- vapply(
  payload[[1]]$facets[[1]]$values, `[[`, character(1), "swatch_color"
)
semantic_names <- vapply(
  acec_payload$semantics, `[[`, character(1), "official_acec_name"
)
semantic_cady <- acec_payload$semantics[[match("Cady Mountains WSA", semantic_names)]]
semantic_cerro <- acec_payload$semantics[[match("Cerro Gordo WSA", semantic_names)]]
semantic_non_wsa <- acec_payload$semantics[[which(!grepl(" WSA", semantic_names, fixed = TRUE))[[1]]]]
stopifnot(
  length(payload) == 1L,
  identical(payload[[1]]$layer_id, "acec"),
  length(payload[[1]]$records) == 238L,
  length(payload[[1]]$features) == 238L,
  length(payload[[1]]$facets) == 3L,
  length(payload[[1]]$quick_views) == 6L,
  identical(
    quick_view_keys,
    c(
      "fish_aquatic", "drecp", "wildlife_habitat", "cultural_historic",
      "scenic", "natural_systems"
    )
  ),
  identical(
    value_swatches,
    c("#3B82A0", "#7A9A4A", "#4F8C68", "#A66A43", "#8A6DAA", "#B58A3D")
  ),
  identical(payload[[1]]$facets[[1]]$thematic_style, list(
    multiple_selection = "neutral",
    fill_opacity = 0.22,
    stroke_weight = 2,
    stroke_darken = 0.28
  )),
  !isTRUE(payload[[1]]$category_filter_visible),
  identical(payload[[1]]$primary_count_label, "ACECs"),
  length(first_value_facet) >= 1L,
  length(acec_payload$semantics) == 238L,
  length(acec_payload$components) == 238L,
  length(acec_payload$values) == 867L,
  length(acec_payload$documents) == 32L,
  length(acec_payload$management) == 246L,
  length(acec_payload$planning) == 238L,
  length(acec_payload$relationships) == 476L,
  length(acec_payload$offices) == 238L,
  length(acec_payload$current_field_offices) == 14L,
  length(acec_payload$field_office_context) == 276L,
  length(acec_payload$access) == 238L,
  length(acec_payload$overlap_pairs) == 27L,
  identical(
    acec_payload$overlap_style,
    PT_LOCAL_REFERENCE_ACEC_OVERLAP_STYLE
  ),
  identical(
    acec_payload$field_office_presentation$additional_office_minimum_percent,
    1
  ),
  identical(
    acec_payload$area_presentation$primary_area,
    "current BLM source GIS acreage"
  ),
  identical(
    acec_payload$area_presentation$material_difference_minimum_acres,
    10
  ),
  identical(
    acec_payload$area_presentation$material_difference_minimum_percent,
    0.5
  ),
  identical(semantic_cady$wsa_name_context_status, "historical_name_only"),
  grepl("not a current Wilderness Study Area", semantic_cady$popup_wording, fixed = TRUE),
  identical(semantic_cerro$wsa_name_context_status, "current_wsa"),
  identical(semantic_cerro$current_wsa_nlcs_id, "NLCS000219"),
  identical(semantic_cerro$popup_wording, "Also a current BLM Wilderness Study Area."),
  !nzchar(semantic_non_wsa$wsa_name_context_status),
  !nzchar(semantic_non_wsa$popup_wording),
  grepl("planning-designation boundary", acec_payload$caveats$boundary, fixed = TRUE),
  identical(
    acec_payload$caveats$field_office_context,
    paste(
      "Derived from spatial intersection with verified current BLM field-office",
      "boundaries; this does not by itself establish administrative responsibility."
    )
  ),
  grepl("not verified", acec_payload$caveats$rna, fixed = TRUE)
)

qa <- pt_local_reference_acec_qa(prepared)
stopifnot(identical(as.integer(qa$value), c(238L, 238L, 613L, 82L, 128L, 34L, 3L)))

cat("ACEC identity, WSA-name context, facets, hover, and normalized payload tests passed.\n")
