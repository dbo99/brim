#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(sf))

source("00_config/config_local_reference_interactions.r")
source("03_functions/local_reference_interaction_helpers.r")
source("02_preprocess/69_acec_pipeline/build_acec_field_office_context.R")

required_path <- function(variable) {
  path <- Sys.getenv(variable, unset = "")
  if (!nzchar(path) || !file.exists(path)) {
    stop(variable, " must name an existing focused field-office-context QA input.")
  }
  normalizePath(path, winslash = "/")
}

acec_raw_path <- required_path("BRIM_ACEC_RAW_GEOJSON")
field_office_rds_path <- required_path("BRIM_FIELD_OFFICE_RDS")
tracked <- pt_local_reference_acec_field_office_context()
current_offices <- pt_local_reference_acec_current_field_offices()
source_offices <- pt_local_reference_acec_offices()

derived_path <- tempfile("brim_acec_field_office_context_", fileext = ".csv")
qa_path <- tempfile("brim_acec_field_office_context_qa_", fileext = ".csv")
on.exit(unlink(c(derived_path, qa_path)), add = TRUE)
derived <- build_acec_field_office_context(
  current_raw_geojson = acec_raw_path,
  field_office_rds = field_office_rds_path,
  relationship_output_path = derived_path,
  qa_output_path = qa_path
)
derived_table <- utils::read.csv(
  derived_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
stable_relationship_order <- function(x) {
  x <- x[order(x$acec_id, x$current_field_office_code), , drop = FALSE]
  rownames(x) <- NULL
  x
}
qa <- utils::read.csv(qa_path, stringsAsFactors = FALSE, check.names = FALSE)

office_counts <- table(factor(
  tracked$current_field_office_key,
  levels = current_offices$office_key[order(current_offices$sort_order)]
))
expected_office_counts <- c(
  11L, 11L, 26L, 46L, 18L, 5L, 8L,
  21L, 13L, 27L, 29L, 16L, 36L, 9L
)
context_counts <- table(factor(
  qa$acec_context_class,
  levels = c(
    "wholly_within_one_field_office", "crosses_field_office_boundaries",
    "partial_spatial_match_review_required", "no_spatial_match_review_required"
  )
))
source_counts <- table(factor(
  qa$source_context_qa_class,
  levels = c(
    "exact_single", "source_in_multiple", "district_only_contextualized",
    "source_spatial_disagreement", "no_spatial_match"
  )
))
mattole <- qa[qa$official_acec_name == "Mattole Beach", , drop = FALSE]
district_only <- qa[!qa$source_is_current_field_office, , drop = FALSE]
current_text <- paste(
  current_offices$office_key,
  current_offices$current_official_name,
  tracked$current_field_office_name
)

stopifnot(
  identical(
    stable_relationship_order(derived_table),
    stable_relationship_order(tracked)
  ),
  identical(
    current_offices$current_official_name[order(current_offices$sort_order)],
    sort(current_offices$current_official_name, method = "radix")
  ),
  nrow(tracked) == 276L,
  length(unique(tracked$acec_id)) == 238L,
  !anyDuplicated(paste(
    tracked$acec_id,
    tracked$current_field_office_code,
    sep = "|"
  )),
  identical(unname(as.integer(office_counts)), expected_office_counts),
  identical(as.integer(context_counts), c(202L, 35L, 1L, 0L)),
  identical(as.integer(source_counts), c(106L, 6L, 126L, 0L, 0L)),
  sum(source_offices$source_admin_unit_code %in% current_offices$office_code) == 112L,
  sum(!source_offices$source_admin_unit_code %in% current_offices$office_code) == 126L,
  nrow(district_only) == 126L,
  all(district_only$current_field_office_count >= 1L),
  nrow(mattole) == 1L,
  identical(
    mattole$acec_context_class,
    "partial_spatial_match_review_required"
  ),
  mattole$retained_context_percent > 83,
  mattole$retained_context_percent < 84,
  all(c(
    "barstow_field_office", "needles_field_office", "ridgecrest_field_office",
    "central_coast_field_office"
  ) %in% tracked$current_field_office_key),
  !any(grepl("Hollister|Alturas|Susanville", current_text, ignore.case = TRUE)),
  all(tracked$intersection_area_m2 > 100),
  all(tracked$relationship_method == "positive-area intersection in EPSG:3310"),
  all(tracked$minimum_intersection_area_m2 == 100),
  all(tracked$complete_coverage_percent == 99.5)
)

cat(
  "ACEC field-office context QA passed: 276 relationships / 238 ACECs; ",
  "202 one-office, 35 cross-boundary, 1 partial review; source QA ",
  "106 exact + 6 compatible multi + 126 district-only; 14 current offices; ",
  "no superseded names.\n",
  sep = ""
)
