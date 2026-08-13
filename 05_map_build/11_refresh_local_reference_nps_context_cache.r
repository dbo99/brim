#!/usr/bin/env Rscript

# ==== 11_refresh_local_reference_nps_context_cache.r =======================
## Promote one reviewed, external NPS Park/Preserve context candidate into the
## isolated build cache. This focused bridge does not alter the shared Local
## Reference cache or any production artifact.

source("00_config/config_paths.r")
source("00_config/config_local_reference_interactions.r")
source("03_functions/cache_helpers.r")
source("03_functions/local_reference_interaction_helpers.r")
source("03_functions/polygon_generalization_helpers.r")

suppressPackageStartupMessages(library(sf))

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Focused NPS context cache refresh requires digest.")
}

candidate_path <- Sys.getenv("BRIM_NPS_CONTEXT_CANDIDATE_RDS", unset = "")
expected_sha256 <- tolower(Sys.getenv(
  "BRIM_NPS_CONTEXT_CANDIDATE_SHA256", unset = ""
))
if (!nzchar(candidate_path) || !nzchar(expected_sha256)) {
  stop(
    "Set BRIM_NPS_CONTEXT_CANDIDATE_RDS and ",
    "BRIM_NPS_CONTEXT_CANDIDATE_SHA256 explicitly."
  )
}
candidate_path <- normalizePath(path.expand(candidate_path), mustWork = TRUE)
if (!grepl("^[0-9a-f]{64}$", expected_sha256)) {
  stop("BRIM_NPS_CONTEXT_CANDIDATE_SHA256 must be a lowercase SHA-256 hash.")
}
actual_sha256 <- digest::digest(
  file = candidate_path, algo = "sha256", serialize = FALSE
)
if (!identical(actual_sha256, expected_sha256)) {
  stop(
    "NPS context candidate hash mismatch; expected ", expected_sha256,
    ", received ", actual_sha256, "."
  )
}

candidate <- readRDS(candidate_path)
pt_validate_local_reference_nps_context(candidate)
metadata <- attr(candidate, "pt_nps_context_candidate_metadata")
if (!identical(sort(as.character(metadata$target_codes)), c(
  "CHIS", "DEVA", "JOTR", "KICA", "LAVO",
  "MOJA", "PINN", "REDW", "SEQU", "YOSE"
)) || !identical(as.character(metadata$cross_border_units_retained_whole), "DEVA")) {
  stop("NPS context target universe or whole-unit cross-border contract differs.")
}
candidate <- pt_apply_reviewed_polygon_geometry_to_nps_context(
  candidate,
  require_reviewed = TRUE
)

run_ts <- make_timestamp()
timestamped_path <- file.path(
  DIR$cache_enr,
  timestamped_name("nps_park_preserve_context_map", "rds", run_ts)
)
latest_path <- file.path(DIR$cache_last, "nps_park_preserve_context_map.rds")
qa_path <- file.path(
  DIR$qa, paste0("nps_park_preserve_context_cache_", run_ts, ".csv")
)
if (file.exists(timestamped_path) || file.exists(qa_path)) {
  stop("Refusing to overwrite timestamped NPS context cache output.")
}

save_rds_cached(
  candidate,
  timestamped_path = timestamped_path,
  latest_path = latest_path
)
cached <- readRDS(latest_path)
pt_validate_local_reference_nps_context(cached)
cached_sha256 <- digest::digest(cached, algo = "sha256", serialize = TRUE)
source_object_sha256 <- digest::digest(candidate, algo = "sha256", serialize = TRUE)
if (!identical(cached_sha256, source_object_sha256)) {
  stop("NPS context latest cache does not serialize to the reviewed object.")
}

inventory <- sf::st_drop_geometry(candidate$boundaries)
qa <- data.frame(
  run_timestamp = run_ts,
  unit_code = inventory$unit_code,
  unit_name = inventory$unit_name,
  unit_type = inventory$unit_type_label,
  states = inventory$states,
  raw_tract_count = as.integer(inventory$raw_tract_count),
  boundary_feature_count = 1L,
  land_interest_feature_count = 1L,
  candidate_file_sha256 = actual_sha256,
  acquisition_snapshot_id = as.character(metadata$acquisition_snapshot_id),
  acquisition_boundary_simplify_tolerance_m = as.numeric(
    metadata$selected_boundary_simplify_tolerance_m
  ),
  acquisition_land_interest_simplify_tolerance_m = as.numeric(
    metadata$selected_land_interest_simplify_tolerance_m
  ),
  reviewed_portfolio_tolerance_m = as.numeric(
    metadata$polygon_generalization_selected_tolerance_m
  ),
  reviewed_park_artifact_sha256 = as.character(
    metadata$polygon_generalization_park_artifact_sha256
  ),
  reviewed_preserve_artifact_sha256 = as.character(
    metadata$polygon_generalization_preserve_artifact_sha256
  ),
  production_release_authorized = isTRUE(metadata$production_release_authorized),
  status = "PASS",
  stringsAsFactors = FALSE
)
utils::write.csv(qa, qa_path, row.names = FALSE, na = "")

message("Focused NPS Park/Preserve context cache refresh complete.")
message("Reviewed candidate: ", candidate_path)
message("Candidate SHA-256: ", actual_sha256)
message("Timestamped cache: ", timestamped_path)
message("Latest cache: ", latest_path)
message("QA inventory: ", qa_path)
