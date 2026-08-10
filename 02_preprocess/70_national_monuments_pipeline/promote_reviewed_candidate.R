# ==== promote_reviewed_candidate.R ==========================================
## Promote one QA-reviewed National Monuments candidate into an isolated BRIM
## integration workspace. This is not a production-release script.

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || all(is.na(x))) y else x
}

nm_promote_stop <- function(...) stop(paste0(...), call. = FALSE)
nm_promote_assert <- function(ok, message) {
  if (!isTRUE(ok)) nm_promote_stop(message)
}
nm_promote_utc_now <- function() {
  format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}
nm_promote_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

promote_reviewed_national_monuments_candidate <- function(
  candidate_path,
  target_path,
  backup_dir,
  expected_candidate_sha256
) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    nm_promote_stop("Candidate promotion requires digest.")
  }
  candidate_path <- normalizePath(candidate_path, mustWork = TRUE)
  target_dir <- dirname(target_path)
  if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
  target_path <- file.path(normalizePath(target_dir, mustWork = TRUE), basename(target_path))
  if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)
  backup_dir <- normalizePath(backup_dir, mustWork = TRUE)

  actual_candidate_sha256 <- nm_promote_sha256(candidate_path)
  nm_promote_assert(
    identical(actual_candidate_sha256, expected_candidate_sha256),
    paste0(
      "Candidate SHA-256 differs. Expected ", expected_candidate_sha256,
      "; found ", actual_candidate_sha256, "."
    )
  )
  candidate <- readRDS(candidate_path)
  metadata <- attr(candidate, "pt_national_monuments_candidate_metadata")
  nm_promote_assert(inherits(candidate, "sf"), "Candidate is not an sf object.")
  nm_promote_assert(
    nrow(candidate) == 22L && length(unique(candidate$monument_id)) == 20L,
    "Candidate does not retain 20 semantic IDs in 22 reviewed display geometries."
  )
  nm_promote_assert(
    !is.null(metadata) &&
      identical(as.numeric(metadata$simplify_tolerance_m), 1) &&
      identical(as.integer(metadata$display_polygon_parts), 24432L) &&
      identical(as.integer(metadata$display_vertices), 175540L) &&
      isTRUE(metadata$exact_part_retention),
    "Candidate metadata differs from the reviewed one-metre geometry contract."
  )
  nm_promote_assert(
    all(sf::st_is_valid(candidate)) && !any(sf::st_is_empty(candidate)),
    "Candidate contains invalid or empty geometry."
  )

  promoted_utc <- nm_promote_utc_now()
  backup_path <- ""
  original_target_sha256 <- ""
  if (file.exists(target_path)) {
    original_target_sha256 <- nm_promote_sha256(target_path)
    backup_path <- file.path(
      backup_dir,
      paste0(
        tools::file_path_sans_ext(basename(target_path)), "_before_",
        gsub("[-:]", "", gsub("T|Z", "", promoted_utc)), ".rds"
      )
    )
    nm_promote_assert(!file.exists(backup_path), "Refusing to overwrite promotion backup.")
    nm_promote_assert(
      file.copy(target_path, backup_path, overwrite = FALSE, copy.mode = TRUE),
      "Could not preserve the prior integration RDS."
    )
    nm_promote_assert(
      identical(nm_promote_sha256(backup_path), original_target_sha256),
      "Prior integration RDS backup hash differs from its source."
    )
  }

  metadata$integration_candidate_promoted_utc <- promoted_utc
  metadata$integration_candidate_source_path <- basename(candidate_path)
  metadata$integration_candidate_source_sha256 <- actual_candidate_sha256
  metadata$integration_candidate <- TRUE
  metadata$production_release_authorized <- FALSE
  metadata$replacement_authorized <- TRUE
  attr(candidate, "pt_national_monuments_candidate_metadata") <- metadata

  temporary_path <- tempfile(
    pattern = paste0(basename(target_path), "."),
    tmpdir = target_dir
  )
  on.exit(if (file.exists(temporary_path)) unlink(temporary_path), add = TRUE)
  saveRDS(candidate, temporary_path, compress = "xz")
  nm_promote_assert(
    file.copy(temporary_path, target_path, overwrite = TRUE, copy.mode = TRUE),
    "Could not write the isolated integration RDS."
  )
  promoted_sha256 <- nm_promote_sha256(target_path)
  promoted_check <- readRDS(target_path)
  promoted_metadata <- attr(
    promoted_check, "pt_national_monuments_candidate_metadata"
  )
  nm_promote_assert(
    inherits(promoted_check, "sf") && nrow(promoted_check) == 22L &&
      isTRUE(promoted_metadata$integration_candidate) &&
      !isTRUE(promoted_metadata$production_release_authorized),
    "Post-write integration RDS validation failed."
  )

  manifest <- list(
    status = "PASS",
    promoted_utc = promoted_utc,
    scope = "isolated integration workspace only; not production release",
    candidate_file = basename(candidate_path),
    candidate_sha256 = actual_candidate_sha256,
    target_file = basename(target_path),
    target_sha256 = promoted_sha256,
    prior_target_sha256 = original_target_sha256,
    backup_file = if (nzchar(backup_path)) basename(backup_path) else "",
    backup_sha256 = if (nzchar(backup_path)) nm_promote_sha256(backup_path) else "",
    semantic_count = length(unique(promoted_check$monument_id)),
    display_geometry_count = nrow(promoted_check),
    display_polygon_parts = as.integer(promoted_metadata$display_polygon_parts),
    display_vertices = as.integer(promoted_metadata$display_vertices),
    production_release_authorized = FALSE
  )
  manifest_path <- file.path(
    backup_dir,
    paste0(
      "national_monuments_integration_promotion_",
      gsub("[-:]", "", gsub("T|Z", "", promoted_utc)), ".json"
    )
  )
  nm_promote_assert(!file.exists(manifest_path), "Refusing to overwrite promotion manifest.")
  jsonlite::write_json(
    manifest, manifest_path, pretty = TRUE, auto_unbox = TRUE, digits = 17
  )
  message("National Monuments candidate promoted to isolated integration RDS.")
  message("Target SHA-256: ", promoted_sha256)
  message("Promotion manifest: ", manifest_path)
  invisible(manifest)
}

nm_promote_parse_args <- function(arguments) {
  values <- list(
    candidate = NULL, target = NULL, backup_dir = NULL,
    expected_candidate_sha256 = NULL
  )
  index <- 1L
  while (index <= length(arguments)) {
    key <- arguments[[index]]
    allowed <- c(
      "--candidate", "--target", "--backup-dir",
      "--expected-candidate-sha256"
    )
    if (!key %in% allowed) nm_promote_stop("Unknown argument: ", key)
    if (index == length(arguments)) nm_promote_stop("Missing value for ", key)
    name <- gsub("-", "_", sub("^--", "", key))
    values[[name]] <- arguments[[index + 1L]]
    index <- index + 2L
  }
  if (any(vapply(values, is.null, logical(1)))) {
    nm_promote_stop(
      "--candidate, --target, --backup-dir, and ",
      "--expected-candidate-sha256 are required."
    )
  }
  values
}

if (sys.nframe() == 0L) {
  arguments <- nm_promote_parse_args(commandArgs(trailingOnly = TRUE))
  promote_reviewed_national_monuments_candidate(
    candidate_path = arguments$candidate,
    target_path = arguments$target,
    backup_dir = arguments$backup_dir,
    expected_candidate_sha256 = arguments$expected_candidate_sha256
  )
}
