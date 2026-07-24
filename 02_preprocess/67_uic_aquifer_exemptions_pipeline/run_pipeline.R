# Controlled UIC aquifer-exemptions pipeline entry point.
#
# Source this file, then call run_uic_pipeline(). Nothing runs on source.

if (!exists("BRIM_PROJECT_ROOT", inherits = TRUE)) {
  current <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  candidates <- unique(c(
    current,
    dirname(current),
    dirname(dirname(current)),
    dirname(dirname(dirname(current)))
  ))
  roots <- candidates[
    file.exists(file.path(candidates, "README.md")) &
      dir.exists(file.path(candidates, "02_preprocess"))
  ]
  if (!length(roots)) {
    stop("Could not locate BRIM project root. Set BRIM_PROJECT_ROOT first.")
  }
  BRIM_PROJECT_ROOT <- roots[[1]]
}

UIC_PIPELINE_ROOT <- file.path(
  BRIM_PROJECT_ROOT,
  "02_preprocess",
  "67_uic_aquifer_exemptions_pipeline"
)
UIC_RAW_SNAPSHOT_DIR <- file.path(
  BRIM_PROJECT_ROOT, "01_raw_data", "uic_aquifer_exemptions", "raw_snapshots"
)
UIC_DATA_ROOT <- file.path(
  BRIM_PROJECT_ROOT, "04_processed_data", "uic_aquifer_exemptions"
)
UIC_CANDIDATE_DIR <- file.path(UIC_DATA_ROOT, "candidate")
UIC_APPROVED_DIR <- file.path(UIC_DATA_ROOT, "approved")
UIC_ARCHIVE_DIR <- file.path(UIC_DATA_ROOT, "archive")
UIC_CHECK_DIR <- file.path(UIC_DATA_ROOT, "checks")
UIC_QA_DIR <- file.path(UIC_DATA_ROOT, "qa")

uic_scripts <- c(
  "00_helpers.R",
  "01_verify_services.R",
  "02_download_snapshots.R",
  "03_process_sources.R",
  "04_standardize_fields.R",
  "05_compare_to_previous.R",
  "06_run_qa.R",
  "07_export_approved_caches.R",
  "08_build_sandbox_map.R"
)
for (script in uic_scripts) {
  source(file.path(UIC_PIPELINE_ROOT, "scripts", script), local = FALSE)
}

run_uic_pipeline <- function(
    mode = c(
      "check_only",
      "refresh_candidate",
      "force_rebuild",
      "approve_candidate",
      "build_sandbox_map"
    ),
    candidate_id = NULL,
    confirm = NULL,
    data_variant = c("candidate", "approved")
) {
  mode <- match.arg(mode)
  data_variant <- match.arg(data_variant)
  uic_ensure_dirs(c(
    UIC_RAW_SNAPSHOT_DIR,
    UIC_CANDIDATE_DIR,
    UIC_ARCHIVE_DIR,
    UIC_CHECK_DIR,
    UIC_QA_DIR
  ))

  if (identical(mode, "approve_candidate")) {
    return(uic_approve_candidate(candidate_id, confirm))
  }
  if (identical(mode, "build_sandbox_map")) {
    if (identical(data_variant, "candidate") && is.null(candidate_id)) {
      candidate_id <- uic_latest_candidate_id()
    }
    return(uic_build_sandbox_map(candidate_id, data_variant))
  }

  run_id <- uic_stamp_now()
  report_dir <- file.path(UIC_CHECK_DIR, run_id)
  uic_ensure_dirs(report_dir)
  service_check <- uic_verify_services(report_dir)
  count_comparison <- uic_compare_check_to_approved(
    service_check$inventory,
    report_dir
  )
  latest_service_status <- uic_write_latest_service_status(
    service_check$inventory,
    count_comparison
  )
  if (identical(mode, "check_only")) {
    record_comparison <- uic_compare_live_to_approved(
      service_check,
      report_dir
    )
    return(list(
      run_id = run_id,
      ok = service_check$ok,
      service_inventory = service_check$inventory,
      count_comparison = count_comparison,
      record_comparison = record_comparison,
      latest_service_status = latest_service_status,
      report_dir = report_dir
    ))
  }
  if (!service_check$ok) {
    stop(
      "Candidate refresh refused because at least one configured service failed ",
      "availability, schema, geometry-type, or object-ID reconciliation checks. ",
      "See ", report_dir,
      call. = FALSE
    )
  }
  candidate_id <- candidate_id %||% paste0("candidate_", run_id)
  snapshot_id <- paste0("snapshot_", run_id)
  if (dir.exists(uic_candidate_paths(candidate_id)$root)) {
    stop("Candidate ID already exists: ", candidate_id)
  }
  snapshot_dir <- uic_download_snapshots(snapshot_id, service_check)
  raw_objects <- uic_process_sources(snapshot_id, candidate_id)
  standardized <- uic_standardize_outputs(raw_objects, candidate_id)
  comparison <- uic_compare_candidate(candidate_id)
  qa <- uic_run_qa(standardized, candidate_id, service_check$inventory)
  paths <- uic_candidate_paths(candidate_id)
  uic_json_write(
    list(
      pipeline_version = UIC_PIPELINE_VERSION,
      candidate_id = candidate_id,
      snapshot_id = snapshot_id,
      retrieval_utc = service_check$inventory$checked_utc[[1]],
      candidate_built_utc = uic_utc_now(),
      mode = mode,
      qa_passed = isTRUE(qa$passed[[1]]),
      approved = FALSE,
      approval_requires = list(
        candidate_id = candidate_id,
        confirmation_phrase = UIC_APPROVAL_PHRASE
      ),
      snapshot_dir = snapshot_dir
    ),
    file.path(paths$metadata, "candidate.json")
  )
  list(
    candidate_id = candidate_id,
    snapshot_id = snapshot_id,
    qa_passed = isTRUE(qa$passed[[1]]),
    comparison = comparison,
    candidate_dir = paths$root,
    report_dir = report_dir
  )
}
