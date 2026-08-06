suppressPackageStartupMessages(library(sf))

args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || !nzchar(args[[1]])) {
  stop(
    "Usage: Rscript qa/qa_local_reference_federal_wilderness_candidate.R ",
    "<candidate-or-final-rds> [output-csv]"
  )
}
candidate_path <- normalizePath(args[[1]], mustWork = TRUE)
output_path <- if (length(args) >= 2L && nzchar(args[[2]])) args[[2]] else ""

source("00_config/config_local_reference_interactions.r")
source("03_functions/local_reference_interaction_helpers.r")

x <- readRDS(candidate_path)
if (!inherits(x, "sf")) stop("Federal Wilderness candidate must be an sf object.")
enriched <- pt_prepare_local_reference_federal_wilderness(
  x,
  validate_snapshot = TRUE,
  build_display = FALSE
)
prepared <- pt_prepare_local_reference_federal_wilderness(
  x,
  validate_snapshot = TRUE,
  build_display = TRUE
)

qa <- data.frame(
  check = character(0),
  pass = logical(0),
  observed = character(0),
  expected = character(0),
  stringsAsFactors = FALSE
)
add_check <- function(check, pass, observed, expected) {
  qa <<- rbind(qa, data.frame(
    check = check,
    pass = isTRUE(pass),
    observed = as.character(observed),
    expected = as.character(expected),
    stringsAsFactors = FALSE
  ))
}

add_check("component_count", nrow(prepared) == 197L, nrow(prepared), 197L)
add_check(
  "semantic_wilderness_count",
  length(unique(prepared$wilderness_id)) == 158L,
  length(unique(prepared$wilderness_id)),
  158L
)
add_check(
  "unique_normalized_globalid",
  !anyDuplicated(prepared$component_id) && all(nzchar(prepared$component_id)),
  length(unique(prepared$component_id)),
  197L
)
add_check(
  "valid_nonempty_geometry",
  all(sf::st_is_valid(prepared)) && !any(sf::st_is_empty(prepared)),
  paste(sum(!sf::st_is_valid(prepared)), sum(sf::st_is_empty(prepared)), sep = " invalid/empty="),
  "0 invalid/empty=0"
)
add_check(
  "keep_half",
  identical(unique(as.numeric(x$pt_simplify_keep)), 0.5),
  paste(unique(x$pt_simplify_keep), collapse = ","),
  "0.5"
)

death_id <- "blmca-0a2dbf0b-0713-4537-a0ce-3117a7ffcff9"
inyo_nps_id <- "blmca-8d35079b-6e34-4046-b730-fdbd861edfe4"
death <- prepared[prepared$component_id == death_id, , drop = FALSE]
inyo_nps <- prepared[prepared$component_id == inyo_nps_id, , drop = FALSE]
inyo_point <- suppressWarnings(sf::st_point_on_surface(inyo_nps))
death_hits_inyo_point <- if (nrow(death) == 1L && nrow(inyo_point) == 1L) {
  lengths(sf::st_intersects(death, inyo_point)) > 0L
} else {
  TRUE
}
intersection_acres <- if (nrow(death) == 1L && nrow(inyo_nps) == 1L) {
  intersection <- suppressWarnings(sf::st_intersection(
    sf::st_transform(death, 3310),
    sf::st_transform(inyo_nps, 3310)
  ))
  if (nrow(intersection)) sum(as.numeric(sf::st_area(intersection))) / 4046.8564224 else 0
} else {
  Inf
}
add_check(
  "death_valley_inyo_identity",
  nrow(death) == 1L && death$wilderness_id == "fw-873" &&
    nrow(inyo_nps) == 1L && inyo_nps$wilderness_id == "fw-1641" &&
    !death_hits_inyo_point && intersection_acres < 0.01,
  paste(
    "death", nrow(death), death$wilderness_id,
    "inyo", nrow(inyo_nps), inyo_nps$wilderness_id,
    "overlap_acres", round(intersection_acres, 6)
  ),
  "separate Death Valley and Inyo NPS components with no material overlap"
)

santa_current_ids <- c(
  "blmca-c68eb923-95da-4a40-ad19-9cf4627928d3",
  "blmca-df17bcfe-a779-4e95-b77a-f0bae580504a"
)
santa_legacy_id <- "blmca-13178a36-e944-4843-a50f-47e2f3c6c6fa"
santa <- prepared[
  prepared$component_id %in% santa_current_ids,
  , drop = FALSE
]
add_check(
  "santa_rosa_current_split",
  nrow(santa) == 2L && setequal(santa$component_id, santa_current_ids) &&
    all(santa$wilderness_id == "fw-3154") &&
    !santa_legacy_id %in% prepared$component_id,
  paste(sort(santa$component_id), collapse = ";"),
  paste(sort(santa_current_ids), collapse = ";")
)

validity_names <- c("White Mountains Wilderness", "Joshua Tree Wilderness")
validity_rows <- enriched[enriched$pt_fw_official_name %in% validity_names, , drop = FALSE]
add_check(
  "white_mountains_joshua_tree_valid",
  all(validity_names %in% validity_rows$pt_fw_official_name) &&
    all(sf::st_is_valid(validity_rows)) && !any(sf::st_is_empty(validity_rows)),
  paste(table(validity_rows$pt_fw_official_name), collapse = ";"),
  "both present, valid, and nonempty"
)

high_rock_names <- c(
  "East Fork High Rock Canyon Wilderness",
  "High Rock Canyon Wilderness",
  "Little High Rock Canyon Wilderness"
)
high_rock <- enriched[
  enriched$pt_fw_official_name %in% high_rock_names,
  , drop = FALSE
]
high_rock_context_ok <- nrow(high_rock) == 3L &&
  all(high_rock$pt_fw_state_label == "Nevada") &&
  all(high_rock$pt_fw_blm_office == "Black Rock Field Office") &&
  all(high_rock$pt_fw_management_district == "Winnemucca District") &&
  all(high_rock$pt_fw_brim_inclusion == "Western Nevada context")
add_check(
  "high_rock_western_nevada_context",
  nrow(high_rock) == 3L && all(high_rock$pt_fw_admin_state == "NV") &&
    all(high_rock$pt_fw_agency_name == "BLM") && high_rock_context_ok,
  paste(sort(high_rock$pt_fw_official_name), collapse = ";"),
  paste(sort(high_rock_names), collapse = ";")
)

required_raw_fields <- c(
  "OBJECTID", "NLCS_ID", "NLCS_NAME", "CASEFILE_N", "ADMIN_ST", "DESIG_DATE",
  "GlobalID", "PublicLaw_", "ManagingAg", "HasBlmLand", "Modify_Dat",
  "GIS_Acres", "FAU_ID", "SMA_ID", "Shape_Leng", "Shape_Area"
)
add_check(
  "original_source_attributes_retained",
  all(required_raw_fields %in% names(enriched)),
  paste(intersect(required_raw_fields, names(enriched)), collapse = ";"),
  paste(required_raw_fields, collapse = ";")
)

if (nzchar(output_path)) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(qa, output_path, row.names = FALSE, na = "")
}
print(qa, row.names = FALSE)
if (!all(qa$pass)) {
  stop("One or more Federal Wilderness candidate QA checks failed.")
}
cat("Federal Wilderness candidate migration and geometry QA passed.\n")
