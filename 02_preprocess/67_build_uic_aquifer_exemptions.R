# BRIM entry point for the controlled UIC aquifer-exemptions pipeline.
#
# Examples:
#   source("02_preprocess/67_build_uic_aquifer_exemptions.R")
#   run_uic_pipeline("check_only")
#   run_uic_pipeline("refresh_candidate")
#
# Candidate approval is intentionally explicit:
#   run_uic_pipeline(
#     "approve_candidate",
#     candidate_id = "candidate_YYYYMMDDTHHMMSSZ",
#     confirm = "APPROVE UIC CANDIDATE"
#   )

BRIM_PROJECT_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
pipeline_entry <- file.path(
  BRIM_PROJECT_ROOT,
  "02_preprocess",
  "67_uic_aquifer_exemptions_pipeline",
  "run_pipeline.R"
)
if (!file.exists(pipeline_entry)) {
  stop(
    "Run this entry point from the BRIM repository root. Missing: ",
    pipeline_entry,
    call. = FALSE
  )
}
source(pipeline_entry, local = FALSE)
