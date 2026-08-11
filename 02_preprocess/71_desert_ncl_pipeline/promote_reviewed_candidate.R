#!/usr/bin/env Rscript

# Guarded promotion of one reviewed Desert NCL candidate into an isolated
# build workspace. Existing destinations are copied to a timestamped rollback
# directory before replacement. This script must never target production.

CDNCL_EXPECTED_IDS <- sprintf("NLCS%06d", 2009:2019)
CDNCL_EXPECTED_TOLERANCE_M <- 2

cdncl_promote_stop <- function(...) stop(paste0(...), call. = FALSE)

cdncl_promote_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

cdncl_promote_args <- function(args) {
  values <- list(candidate_dir = NULL, destination_rds = NULL)
  keys <- c("--candidate-dir" = "candidate_dir", "--destination-rds" = "destination_rds")
  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    if (!key %in% names(keys) || i == length(args)) {
      cdncl_promote_stop(
        "Usage: promote_reviewed_candidate.R --candidate-dir PATH --destination-rds PATH"
      )
    }
    values[[keys[[key]]]] <- args[[i + 1L]]
    i <- i + 2L
  }
  if (any(!vapply(values, function(x) !is.null(x) && nzchar(trimws(x)), logical(1)))) {
    cdncl_promote_stop("Both promotion paths are required.")
  }
  values
}

cdncl_promote_copy <- function(source, destination, rollback_dir) {
  if (!file.exists(source)) cdncl_promote_stop("Missing candidate file: ", source)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(destination)) {
    backup <- file.path(rollback_dir, basename(destination))
    if (!file.copy(destination, backup, overwrite = FALSE, copy.mode = TRUE)) {
      cdncl_promote_stop("Could not create rollback copy for: ", destination)
    }
  }
  temporary <- paste0(destination, ".candidate-copy")
  if (file.exists(temporary)) {
    cdncl_promote_stop("Refusing unresolved temporary promotion path: ", temporary)
  }
  if (!file.copy(source, temporary, overwrite = FALSE, copy.mode = TRUE)) {
    cdncl_promote_stop("Could not copy candidate to temporary destination: ", temporary)
  }
  if (!identical(cdncl_promote_sha256(source), cdncl_promote_sha256(temporary))) {
    cdncl_promote_stop("Post-copy hash mismatch for: ", destination)
  }
  if (!file.rename(temporary, destination)) {
    cdncl_promote_stop("Could not atomically install: ", destination)
  }
  invisible(destination)
}

cdncl_promote_main <- function() {
  if (!requireNamespace("digest", quietly = TRUE)) cdncl_promote_stop("digest is required.")
  arguments <- cdncl_promote_args(commandArgs(trailingOnly = TRUE))
  candidate_dir <- normalizePath(arguments$candidate_dir, mustWork = TRUE)
  destination <- normalizePath(
    arguments$destination_rds,
    mustWork = FALSE,
    winslash = "/"
  )
  if (grepl("/BRIM_v0\\.38/", destination, fixed = FALSE) &&
      !grepl("/BRIM_v0\\.38_codex_ship/", destination, fixed = FALSE)) {
    cdncl_promote_stop("Production destination is prohibited: ", destination)
  }
  candidate_rds <- file.path(candidate_dir, "reference_cadesert_ncl_wgs84_candidate.rds")
  summary_path <- file.path(candidate_dir, "candidate_summary.json")
  if (!file.exists(candidate_rds) || !file.exists(summary_path)) {
    cdncl_promote_stop("Candidate RDS or summary is missing.")
  }
  summary <- jsonlite::fromJSON(summary_path, simplifyVector = TRUE)
  candidate <- readRDS(candidate_rds)
  metadata <- attr(candidate, "pt_desert_ncl_candidate_metadata")
  if (!inherits(candidate, "sf") || nrow(candidate) != 11L ||
      !identical(as.character(candidate$NLCS_ID), CDNCL_EXPECTED_IDS) ||
      is.null(metadata) || !identical(as.numeric(metadata$simplify_tolerance_m), 2) ||
      !isTRUE(metadata$exact_part_retention) || !isTRUE(metadata$exact_hole_retention) ||
      isTRUE(metadata$production_release_authorized) ||
      !identical(summary$status, "PASS") ||
      !identical(cdncl_promote_sha256(candidate_rds), summary$candidate_rds_sha256)) {
    cdncl_promote_stop("Candidate does not satisfy the reviewed isolated-promotion contract.")
  }

  stamp <- format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ")
  rollback_dir <- file.path(dirname(destination), "rollback_desert_ncl", stamp)
  dir.create(rollback_dir, recursive = TRUE, showWarnings = FALSE)
  cdncl_promote_copy(candidate_rds, destination, rollback_dir)

  sidecars <- c(
    field_office_lookup = "field_office_lookup.csv",
    field_office_context = "field_office_context.csv",
    related_designation_context = "related_designation_context.csv",
    candidate_summary = "candidate_summary.json"
  )
  promoted <- c(destination_rds = destination)
  for (key in names(sidecars)) {
    source <- file.path(candidate_dir, sidecars[[key]])
    extension <- tools::file_ext(source)
    target <- file.path(
      dirname(destination),
      paste0("reference_cadesert_ncl_", key, ".", extension)
    )
    cdncl_promote_copy(source, target, rollback_dir)
    promoted[[key]] <- target
  }
  qa <- data.frame(
    artifact = names(promoted),
    path = unname(promoted),
    sha256 = vapply(unname(promoted), cdncl_promote_sha256, character(1)),
    stringsAsFactors = FALSE
  )
  qa_path <- file.path(
    dirname(destination),
    paste0("reference_cadesert_ncl_promotion_", stamp, ".csv")
  )
  utils::write.csv(qa, qa_path, row.names = FALSE, na = "")
  message(destination)
  message(rollback_dir)
  message(qa_path)
}

cdncl_promote_main()
