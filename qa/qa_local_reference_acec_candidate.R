#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(sf))

source("00_config/config_local_reference_interactions.r")
source("03_functions/local_reference_interaction_helpers.r")
source("02_preprocess/69_acec_pipeline/build_acec_current_candidate.R")

required_path <- function(variable) {
  path <- Sys.getenv(variable, unset = "")
  if (!nzchar(path) || !file.exists(path)) {
    stop(variable, " must name an existing focused ACEC QA input.")
  }
  normalizePath(path, winslash = "/")
}

candidate_path <- required_path("BRIM_ACEC_CANDIDATE_RDS")
candidate <- readRDS(candidate_path)
metadata <- attr(candidate, "pt_acec_candidate_metadata")
source_fields <- c(
  "OBJECTID", "GlobalID", "ACEC_NAME", "LUP_NAME", "NEPA_NUM", "ROD_DATE",
  "GIS_ACRES", "ADMIN_ST", "CA_ADMIN_unit_code", "BLM_MODIFY_DATE",
  "last_edited_date", "ACEC_RLVNCE_CUL", "ACEC_RLVNCE_FRSC",
  "ACEC_RLVNCE_HIS", "ACEC_RLVNCE_NHAZ", "ACEC_RLVNCE_NPRO",
  "ACEC_RLVNCE_NSYS", "ACEC_RLVNCE_SCE", "ACEC_RLVNCE_WRSC",
  "ACEC_IMPRTNCE_CNTRBTN", "ACEC_IMPRTNCE_IMPRTNCE", "ACEC_IMPRTNCE_QLTS",
  "ACEC_IMPRTNCE_THRT", "SPCL_MGMT_ATTN_RX_PRTCT", "SPCL_MGMT_ATTN_RX_PRVNT",
  "Shape__Area", "Shape__Length"
)

stopifnot(
  inherits(candidate, "sf"),
  nrow(candidate) == 238L,
  length(unique(candidate$acec_id)) == 238L,
  !anyDuplicated(candidate$component_id),
  setequal(candidate$component_id, pt_local_reference_acec_components()$component_id),
  all(source_fields %in% names(candidate)),
  all(sf::st_is_valid(candidate)),
  !any(sf::st_is_empty(candidate)),
  sum(pt_acec_part_count(candidate)) == 613L,
  is.list(metadata),
  identical(as.integer(metadata$source_record_count), 238L),
  identical(as.integer(metadata$semantic_acec_count), 238L),
  identical(as.integer(metadata$raw_geometry_parts), 613L),
  identical(as.integer(metadata$processed_geometry_parts), 613L),
  identical(as.integer(metadata$raw_invalid_geometries), 14L),
  identical(as.integer(metadata$processed_invalid_geometries), 0L),
  identical(as.numeric(metadata$simplify_tolerance_m), 1),
  identical(metadata$historical_geometry_role, "QA baseline only"),
  identical(metadata$replacement_authorized, FALSE)
)

prepared <- pt_prepare_local_reference_acec(
  candidate, validate_snapshot = TRUE, build_display = TRUE
)
payload <- pt_local_reference_controller_payload(list(acec = prepared))[[1]]
stopifnot(
  nrow(prepared) == 238L,
  sum(prepared$pt_local_reference_geometry_components) == 613L,
  length(payload$features) == 238L,
  length(payload$records) == 238L,
  length(payload$acec$semantics) == 238L,
  length(payload$acec$values) == 867L,
  length(payload$acec$management) == 246L
)

cat("Focused ACEC current-candidate geometry, source-retention, and payload QA passed.\n")
