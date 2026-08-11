# Focused source/runtime contracts for Local Reference Phase 6 California
# Desert National Conservation Lands.

suppressPackageStartupMessages({
  library(sf)
  library(htmltools)
})

source("00_config/config_local_reference_interactions.r")
source("00_config/config_labels.r")
source("03_functions/label_helpers.r")
source("03_functions/local_reference_interaction_helpers.r")

pt_validate_local_reference_config()

registry <- pt_local_reference_config_row(layer_id = "ca_desert_ncl")
stopifnot(
  identical(registry$implementation_status[[1]], "phase6_desert_ncl"),
  identical(registry$source_nickname[[1]], "cadesert_ncl"),
  identical(registry$color_basis[[1]], "neutral_program_context_or_optional_unit_identity"),
  isTRUE(registry$feature_selection_supported[[1]]),
  isTRUE(registry$auto_zoom_supported[[1]]),
  isTRUE(registry$retention_enabled[[1]]),
  isTRUE(registry$distinguish_units_supported[[1]]),
  identical(registry$popup_layout[[1]], "tabbed_card"),
  !isTRUE(registry$category_filter_visible[[1]])
)

facets <- registry$filter_facets[[1]]
stopifnot(
  identical(
    vapply(facets, `[[`, character(1), "facet_key"),
    c("field_office_context", "related_designation_overlap")
  ),
  identical(facets[[1]]$label, "BLM Field Office context"),
  !isTRUE(facets[[1]]$collapsible),
  identical(facets[[1]]$layout_columns, 2L),
  identical(facets[[2]]$label, "Related designation overlap"),
  !isTRUE(facets[[2]]$collapsible),
  identical(facets[[2]]$layout_columns, 2L),
  identical(
    facets[[2]]$values$value_key,
    c(
      "acec", "federal_wilderness", "national_monuments",
      "wilderness_study_areas", "national_trails", "wild_scenic_river"
    )
  ),
  identical(
    registry$dashboard_summary[[1]],
    "11 mapped units · 10 DRECP subareas + Desert Lily Preserve"
  ),
  length(registry$quick_views[[1]]) == 0L
)

categories <- pt_local_reference_categories("ca_desert_ncl")
stopifnot(
  identical(categories$category_key, c("ca_desert_ncl", "unknown")),
  identical(categories$fill_color[[1]], "#B89C6A"),
  identical(categories$stroke_color[[1]], "#6F5632"),
  isTRUE(categories$provisional[[1]])
)

registration <- pt_local_reference_label_registration(
  source_nickname = "cadesert_ncl"
)
stopifnot(
  identical(registration$label_id[[1]], "cadesert_ncl"),
  identical(registration$anchor_strategy[[1]], "polygon_semantic_point_on_surface"),
  !isTRUE(registration$visible_component_aware[[1]])
)

processed_dir <- Sys.getenv(
  "BRIM_DESERT_NCL_PROCESSED_DIR",
  unset = file.path("04_processed_data", "rds")
)
candidate_path <- file.path(processed_dir, "reference_cadesert_ncl_wgs84.rds")
if (!file.exists(candidate_path)) {
  stop("Focused California Desert NCL processed RDS is missing: ", candidate_path)
}

PT_LOCAL_REFERENCE_DESERT_NCL_FIELD_OFFICE_LOOKUP_PATH <- file.path(
  processed_dir, "reference_cadesert_ncl_field_office_lookup.csv"
)
PT_LOCAL_REFERENCE_DESERT_NCL_FIELD_OFFICE_CONTEXT_PATH <- file.path(
  processed_dir, "reference_cadesert_ncl_field_office_context.csv"
)
PT_LOCAL_REFERENCE_DESERT_NCL_RELATED_CONTEXT_PATH <- file.path(
  processed_dir, "reference_cadesert_ncl_related_designation_context.csv"
)

candidate <- readRDS(candidate_path)
prepared <- pt_prepare_local_reference_desert_ncl(
  candidate,
  validate_snapshot = TRUE,
  build_display = TRUE
)
metadata <- attr(prepared, "pt_desert_ncl_candidate_metadata")
stopifnot(
  nrow(prepared) == 11L,
  identical(as.character(prepared$NLCS_ID), sprintf("NLCS%06d", 2009:2019)),
  length(unique(prepared$pt_local_reference_semantic_key)) == 11L,
  length(unique(prepared$pt_local_reference_geometry_key)) == 11L,
  sum(prepared$pt_local_reference_geometry_components) == 173L,
  sum(prepared$pt_cdncl_unit_type_key == "drecp_ecoregion_subarea") == 10L,
  sum(prepared$pt_cdncl_unit_type_key == "desert_lily_source_record") == 1L,
  identical(as.numeric(metadata$simplify_tolerance_m), 2),
  identical(as.integer(metadata$raw_vertices), 84155L),
  identical(as.integer(metadata$display_vertices), 32168L),
  isTRUE(metadata$exact_part_retention),
  isTRUE(metadata$exact_hole_retention),
  all(sf::st_is_valid(prepared)),
  !any(sf::st_is_empty(prepared)),
  all(nzchar(prepared$pt_cdncl_field_office_names)),
  all(nzchar(prepared$pt_cdncl_related_designation_facets)),
  all(nzchar(prepared$pt_reference_label_text)),
  all(grepl("^geom:nlcs", prepared$component_id))
)

related_facet_keys <- c(
  "acec", "federal_wilderness", "national_monuments",
  "wilderness_study_areas", "national_trails", "wild_scenic_river"
)
related_facet_counts <- stats::setNames(vapply(related_facet_keys, function(key) {
  sum(vapply(strsplit(
    prepared$pt_cdncl_related_designation_facets, "|", fixed = TRUE
  ), function(values) key %in% values, logical(1)))
}, integer(1)), related_facet_keys)
stopifnot(identical(
  related_facet_counts,
  c(
    acec = 11L, federal_wilderness = 0L, national_monuments = 7L,
    wilderness_study_areas = 1L, national_trails = 6L,
    wild_scenic_river = 4L
  )
))

office_context <- pt_local_reference_desert_ncl_field_office_context()
relationships <- pt_local_reference_desert_ncl_related_context()
desert_lily <- relationships[
  relationships$nlcs_id == "NLCS002012" &
    relationships$related_layer_key == "acec" &
    relationships$related_feature_name == "Desert Lily Preserve",
  , drop = FALSE
]
stopifnot(
  nrow(office_context) == 26L,
  all(as.numeric(office_context$intersection_area_m2) > 0),
  setequal(unique(office_context$nlcs_id), prepared$NLCS_ID),
  nrow(relationships) == 277L,
  all(tolower(relationships$management_inference_prohibited) == "true"),
  nrow(desert_lily) == 1L,
  as.numeric(desert_lily$percent_of_unit_area) > 99
)

labels <- pt_make_local_reference_labels(prepared, registration)
stopifnot(
  inherits(labels, "sf"),
  nrow(labels) == 11L,
  length(unique(labels$semantic_feature_key)) == 11L
)
pt_validate_local_reference_label_anchors(labels, prepared, registration)

payload <- pt_local_reference_controller_payload(
  list(cadesert_ncl = prepared),
  list(cadesert_ncl = labels)
)
stopifnot(
  length(payload) == 1L,
  identical(payload[[1]]$layer_id[[1]], "ca_desert_ncl"),
  length(payload[[1]]$records) == 11L,
  length(payload[[1]]$features) == 11L,
  length(payload[[1]]$facets) == 2L,
  length(payload[[1]]$quick_views) == 0L,
  identical(payload[[1]]$semantic_labels$semantic_feature_count, 11L),
  identical(payload[[1]]$semantic_labels$anchor_count, 11L),
  length(payload[[1]]$desert_ncl$semantics) == 11L,
  length(payload[[1]]$desert_ncl$field_office_context) == 26L,
  length(payload[[1]]$desert_ncl$related_context) == 277L,
  identical(
    payload[[1]]$dashboard_summary[[1]],
    "11 mapped units · 10 DRECP subareas + Desert Lily Preserve"
  ),
  is.null(payload[[1]]$acec),
  is.null(payload[[1]]$federal_wilderness)
)

# The accepted aggregate reference cache predates the dashboard facet field.
# Final-map payload assembly must derive it from the unchanged relationship
# sidecar without requiring a cache refresh.
legacy_cache_shape <- prepared[
  , setdiff(names(prepared), "pt_cdncl_related_designation_facets"),
  drop = FALSE
]
legacy_payload <- pt_local_reference_controller_payload(
  list(cadesert_ncl = legacy_cache_shape),
  list(cadesert_ncl = labels)
)
legacy_related_values <- lapply(legacy_payload[[1]]$records, function(record) {
  record$facet_values$related_designation_overlap
})
stopifnot(
  length(legacy_payload) == 1L,
  length(legacy_related_values) == 11L,
  sum(vapply(legacy_related_values, function(values) {
    "federal_wilderness" %in% values
  }, logical(1))) == 0L,
  sum(vapply(legacy_related_values, function(values) {
    "national_monuments" %in% values
  }, logical(1))) == 7L
)

controller_source <- paste(readLines(
  file.path("03_functions", "js", "brim_local_reference_controller.js"),
  warn = FALSE
), collapse = "\n")
stopifnot(
  grepl("function buildDesertNclPopup", controller_source, fixed = TRUE),
  grepl("Related designations & sources", controller_source, fixed = TRUE),
  grepl("Geometry lineage GlobalID", controller_source, fixed = TRUE),
  grepl("Source OBJECTID (diagnostic only)", controller_source, fixed = TRUE),
  grepl("pt-cdncl-identity-cue", controller_source, fixed = TRUE),
  grepl("pt-cdncl-related-group", controller_source, fixed = TRUE),
  grepl("Distinguish mapped units", controller_source, fixed = TRUE),
  !grepl("off by default", controller_source, fixed = TRUE),
  grepl(
    'data-pt-local-reference-layer="ca_desert_ncl"]{width:330px;max-height:none;overflow:visible',
    controller_source,
    fixed = TRUE
  ),
  grepl('.pt-lr-map-details>summary::before{content:"▸";position:absolute;left:7px', controller_source, fixed = TRUE)
)

message("California Desert NCL focused source/runtime tests passed.")
