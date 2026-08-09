suppressPackageStartupMessages({
  library(sf)
  library(htmltools)
})

source("00_config/config_local_reference_interactions.r")
source("00_config/config_labels.r")
source("03_functions/label_helpers.r")
source("03_functions/local_reference_interaction_helpers.r")

pt_validate_local_reference_config()
pt_validate_local_reference_fw_research()

components <- pt_local_reference_fw_components()
reference <- pt_local_reference_fw_reference()
designations <- pt_local_reference_fw_designations()
documents <- pt_local_reference_fw_documents()

source_fixture <- data.frame(
  OBJECTID = seq_len(nrow(components)),
  GlobalID = components$source_globalid,
  FAU_ID = components$source_semantic_id,
  NLCS_ID = components$source_nlcs_id,
  NLCS_NAME = components$source_name,
  ADMIN_ST = components$geographic_state,
  ManagingAg = components$managing_agency_code,
  managing_agency = components$managing_agency,
  GIS_Acres = components$source_gis_acres,
  Modify_Dat = components$source_modify_date,
  marker = paste0("raw-", seq_len(nrow(components))),
  pt_nickname = "fedwilderness",
  pt_display_name = "Federal Wilderness *",
  pt_geom_type = "polygon",
  stringsAsFactors = FALSE
)
fixture <- sf::st_as_sf(
  source_fixture,
  coords = c("OBJECTID", "FAU_ID"),
  crs = 3310,
  remove = FALSE
)
enriched <- pt_prepare_local_reference_federal_wilderness(
  fixture,
  validate_snapshot = TRUE,
  build_display = FALSE
)
prepared <- pt_prepare_local_reference_federal_wilderness(
  fixture,
  validate_snapshot = TRUE,
  build_display = TRUE
)

runtime_fields <- c(
  "pt_nickname", "pt_display_name", "pt_geom_type",
  "component_id", "wilderness_id",
  "pt_local_reference_feature_key", "pt_local_reference_semantic_key",
  "pt_local_reference_geometry_key", "pt_local_reference_geometry_components",
  "pt_local_reference_category_key", "pt_local_reference_category_label",
  "fill_col", "line_col", "fill_opacity", "line_weight", "line_dash",
  "pt_legend_swatch_style", "pt_fw_management_pattern",
  "pt_fw_designation_history", "pt_fw_geographic_context",
  "pt_reference_label_text", "pt_reference_hover_html",
  "pt_reference_hover_text", "geometry"
)
stopifnot(
  nrow(prepared) == 197L,
  length(unique(prepared$wilderness_id)) == 158L,
  !anyDuplicated(prepared$component_id),
  identical(enriched$marker, source_fixture$marker),
  setequal(names(prepared), runtime_fields),
  !"popup_html" %in% names(prepared),
  identical(sort(unique(prepared$pt_local_reference_category_key)),
            c("blm", "fws", "nps", "usfs")),
  sum(prepared$pt_fw_geographic_context == "western_nevada_context") == 3L,
  length(unique(prepared$wilderness_id[
    prepared$pt_fw_management_pattern == "shared_multi_agency"
  ])) == 14L,
  length(unique(prepared$wilderness_id[
    prepared$pt_fw_designation_history == "has_subsequent_law"
  ])) == 22L,
  all(prepared$pt_local_reference_geometry_components == 1L),
  all(nzchar(prepared$pt_reference_hover_html)),
  all(grepl("Approx. area: ", prepared$pt_reference_hover_html, fixed = TRUE)),
  all(grepl(" mi²", prepared$pt_reference_hover_html, fixed = TRUE)),
  !any(grepl("acre", prepared$pt_reference_hover_html, ignore.case = TRUE))
)

santa_index <- which(enriched$pt_fw_official_name == "Santa Rosa Wilderness")
santa_hover <- prepared$pt_reference_hover_html[santa_index]
santa_expected_area <- sub(
  "^~", "",
  pt_local_reference_format_square_miles_from_acres(
    unique(enriched$pt_fw_official_reference_acres[santa_index])
  )
)
stopifnot(
  length(santa_index) > 1L,
  length(unique(santa_hover)) == 1L,
  grepl(paste0("Approx. area: ", santa_expected_area), santa_hover[[1]], fixed = TRUE),
  length(unique(enriched$pt_fw_source_gis_acres[santa_index])) > 1L
)

expected_colors <- c(
  blm = "#B8860B", usfs = "#228B22", nps = "#54278F", fws = "#1F78B4"
)
observed_colors <- unique(sf::st_drop_geometry(prepared)[, c(
  "pt_local_reference_category_key", "fill_col"
)])
observed_colors <- stats::setNames(
  observed_colors$fill_col,
  observed_colors$pt_local_reference_category_key
)
stopifnot(identical(observed_colors[names(expected_colors)], expected_colors))

status_counts <- table(designations$validation_status)
prior_blank_ids <- reference$wilderness_id[
  !nzchar(pt_local_reference_clean_chr(reference$designation_date))
]
resolved_prior_blanks <- designations[
  designations$wilderness_id %in% prior_blank_ids,
  ,
  drop = FALSE
]
focus_expected <- data.frame(
  wilderness_id = c(
    "fw-95", "fw-802", "fw-943", "fw-1031", "fw-1350", "fw-1491",
    "fw-1623", "fw-1706", "fw-2077", "fw-2294", "fw-3100",
    "fw-3154", "fw-3695", "fw-3937"
  ),
  resolved_original_designation_date = c(
    "1964-09-03", "1994-10-31", "1964-09-03", "2011-01-13",
    "1978-02-24", "1990-11-28", "1990-11-28", "1964-09-03",
    "1964-09-03", "1989-12-05", "1964-09-03", "1984-09-28",
    "1969-08-18", "1964-09-03"
  ),
  original_public_law = c(
    "Public Law 88-577", "Public Law 103-433", "Public Law 88-577",
    "Public Law 109-362", "Public Law 95-237", "Public Law 101-628",
    "Public Law 101-628", "Public Law 88-577", "Public Law 88-577",
    "Public Law 101-195", "Public Law 88-577", "Public Law 98-425",
    "Public Law 91-58", "Public Law 88-577"
  ),
  stringsAsFactors = FALSE
)
focus_observed <- designations[
  match(focus_expected$wilderness_id, designations$wilderness_id),
  names(focus_expected),
  drop = FALSE
]
rownames(focus_observed) <- NULL
stopifnot(
  nrow(designations) == 158L,
  !anyDuplicated(designations$wilderness_id),
  identical(unname(status_counts[["PASS"]]), 154L),
  identical(unname(status_counts[["PASS WITH DOCUMENTED EXPLANATION"]]), 4L),
  !any(grepl("WARNING|FAIL|UNRESOLVED", designations$validation_status)),
  length(prior_blank_ids) == 10L,
  all(nzchar(resolved_prior_blanks$resolved_original_designation_date)),
  identical(focus_observed, focus_expected),
  all(as.integer(substr(
    designations$resolved_original_designation_date, 1, 4
  )) == designations$resolved_designation_year)
)

corrected_docs <- documents[documents$document_id %in% c(
  "doc-f5c961f53d89", "doc-02d95f08f985",
  "doc-b56de391c0e3", "doc-6d8aa46caa5f"
), c("document_id", "publication_date", "document_title")]
stopifnot(
  nrow(documents) == 360L,
  any(grepl("Public Law 101-195", corrected_docs$document_title, fixed = TRUE)),
  any(grepl("Public Law 95-237", corrected_docs$document_title, fixed = TRUE)),
  any(grepl("Public Law 98-425", corrected_docs$document_title, fixed = TRUE)),
  setequal(corrected_docs$publication_date, c("1989-12-05", "1978-02-24", "1984-09-28"))
)

federal_layers <- list(fedwilderness = prepared)
payload <- pt_local_reference_controller_payload(
  federal_layers,
  pt_build_registered_local_reference_label_children(federal_layers)
)
federal_payload <- payload[[1]]$federal_wilderness
stopifnot(
  length(payload) == 1L,
  length(payload[[1]]$records) == 197L,
  length(payload[[1]]$features) == 158L,
  payload[[1]]$semantic_labels$semantic_feature_count == 158L,
  payload[[1]]$semantic_labels$anchor_count == 197L,
  isTRUE(payload[[1]]$semantic_labels$visible_component_aware),
  length(payload[[1]]$facets) == 3L,
  identical(payload[[1]]$category_count_mode, "geometry_component"),
  isTRUE(payload[[1]]$distinguish_units_supported),
  identical(payload[[1]]$primary_count_label, "named wildernesses"),
  identical(payload[[1]]$component_count_label, "mapped components"),
  length(federal_payload$semantics) == 158L,
  length(federal_payload$components) == 197L,
  length(federal_payload$documents) == 360L,
  length(federal_payload$agencies) == 4L,
  length(federal_payload$offices) < length(federal_payload$components),
  nzchar(federal_payload$templates$govinfo_public_law)
)

cat("Federal Wilderness 197/158 geometry, resolved designation, hover, and normalized payload tests passed.\n")
