#!/usr/bin/env Rscript

# Build the complete 20-monument evidence-to-style matrix. The matrix audits
# semantic agency membership separately from the reviewed geometry display
# role, preventing source publisher or a duplicate whole boundary from
# silently selecting an agency color.

source("00_config/config_paths.r")
source("00_config/config_local_reference_interactions.r")
source("03_functions/spatial_helpers.r")
source("03_functions/local_reference_interaction_helpers.r")

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(htmltools)
})

candidate_path <- Sys.getenv(
  "BRIM_NM_CANDIDATE_RDS",
  unset = file.path(DIR$rds, "reference_monuments_wgs84.rds")
)
output_path <- Sys.getenv(
  "BRIM_NM_STYLE_QA_CSV",
  unset = file.path(
    DIR$qa,
    paste0(
      "local_reference_national_monument_agency_style_matrix_",
      format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"
    )
  )
)
candidate_path <- normalizePath(path.expand(candidate_path), mustWork = TRUE)
if (file.exists(output_path)) stop("Refusing to overwrite style QA: ", output_path)

candidate <- readRDS(candidate_path)
map <- pt_prepare_local_reference_national_monuments(
  candidate, validate_snapshot = TRUE, build_display = TRUE
)
reference <- pt_local_reference_nm_reference()
management <- pt_local_reference_nm_management()
roles <- pt_local_reference_nm_source_geometry_roles()

collapse_unique <- function(x, separator = " | ") {
  values <- unique(trimws(as.character(x)))
  values <- values[!is.na(values) & nzchar(values)]
  paste(values, collapse = separator)
}
source_record <- function(x) paste(
  x$source_key, x$source_object_id, x$source_identifier, x$source_name,
  sep = ":"
)
styles <- PT_LOCAL_REFERENCE_NATIONAL_MONUMENT_CATEGORIES
style_fill <- stats::setNames(styles$fill_color, styles$category_key)
style_stroke <- stats::setNames(styles$stroke_color, styles$category_key)
shared_whole_ids <- c(
  "nm_ca_berryessa_snow_mountain", "nm_ca_santa_rosa_san_jacinto"
)
shared_component_ids <- c("nm_ca_sand_to_snow", "nm_ca_tule_lake")

rows <- lapply(seq_len(nrow(reference)), function(index) {
  semantic <- reference[index, , drop = FALSE]
  id <- semantic$monument_id[[1]]
  displayed <- map[map$monument_id == id, , drop = FALSE]
  evidence <- management[management$monument_id == id, , drop = FALSE]
  source_roles <- roles[
    !is.na(roles$monument_id) & roles$monument_id == id,
    , drop = FALSE
  ]
  agencies <- sort(strsplit(
    tolower(semantic$administering_agencies[[1]]), "|", fixed = TRUE
  )[[1]], method = "radix")
  display_agencies <- ifelse(agencies == "usfws", "fws", agencies)
  categories <- sort(unique(displayed$pt_local_reference_category_key), method = "radix")
  display_roles <- sort(unique(displayed$pt_nm_display_geometry_role), method = "radix")
  expected_categories <- if (id %in% shared_whole_ids) {
    "shared_multi"
  } else if (id %in% shared_component_ids) {
    display_agencies
  } else {
    agencies
  }
  checks <- c(
    length(agencies) >= 1L,
    setequal(categories, expected_categories),
    if (id %in% shared_whole_ids) {
      nrow(displayed) == 1L && identical(display_roles, "complete_semantic_boundary")
    } else if (id %in% shared_component_ids) {
      nrow(displayed) == 2L && identical(display_roles, "agency_component")
    } else {
      nrow(displayed) == 1L && identical(display_roles, "complete_semantic_boundary")
    },
    all(displayed$pt_nm_administering_agencies == paste(agencies, collapse = "|")),
    !any(categories == "nps") || "nps" %in% agencies,
    nrow(evidence) == length(agencies),
    setequal(tolower(evidence$agency), agencies),
    nrow(source_roles) >= 1L,
    all(categories %in% names(style_fill)),
    all(displayed$fill_col == unname(style_fill[categories[match(
      displayed$pt_local_reference_category_key, categories
    )]]))
  )
  data.frame(
    monument_id = id,
    monument_name = semantic$canonical_name[[1]],
    documented_administering_agencies = paste(agencies, collapse = "|"),
    evidence_sources = collapse_unique(paste(
      evidence$agency, evidence$evidence_basis, evidence$official_unit_page,
      sep = " — "
    )),
    source_geometry_records = collapse_unique(source_record(source_roles)),
    source_geometry_roles = collapse_unique(paste(
      source_roles$source_key, source_roles$source_object_id,
      source_roles$geometry_role,
      ifelse(tolower(source_roles$use_for_semantic_display) == "true", "selected", "not_selected"),
      sep = ":"
    )),
    final_display_geometry_roles = paste(display_roles, collapse = "|"),
    final_display_record_count = nrow(displayed),
    final_style_categories = paste(categories, collapse = "|"),
    final_fill_colors = collapse_unique(unname(style_fill[categories])),
    final_stroke_colors = collapse_unique(unname(style_stroke[categories])),
    semantic_filter_memberships = paste(agencies, collapse = "|"),
    expected_visible_semantic_label_count = 1L,
    source_role_and_style_agree = all(checks),
    validation_result = if (all(checks)) "PASS" else "FAIL",
    stringsAsFactors = FALSE
  )
})
matrix <- do.call(rbind, rows)

if (nrow(matrix) != 20L || anyDuplicated(matrix$monument_id) ||
    any(matrix$validation_result != "PASS") ||
    sum(matrix$final_style_categories == "shared_multi") != 2L ||
    !identical(
      matrix$final_style_categories[
        matrix$monument_id == "nm_ca_sand_to_snow"
      ],
      "blm|usfs"
    ) ||
    !identical(
      matrix$final_style_categories[
        matrix$monument_id == "nm_ca_tule_lake"
      ],
      "fws|nps"
    ) || any(
      grepl("(^|\\|)nps(\\||$)", matrix$final_style_categories) &
        !grepl(
          "(^|\\|)nps(\\||$)",
          matrix$documented_administering_agencies
        )
    )) {
  failed <- matrix$monument_id[matrix$validation_result != "PASS"]
  stop(
    "National Monument agency/style matrix failed: ",
    if (length(failed)) paste(failed, collapse = ", ") else "summary contract"
  )
}
utils::write.csv(matrix, output_path, row.names = FALSE, na = "")
message("National Monument agency/style matrix PASS: ", output_path)
