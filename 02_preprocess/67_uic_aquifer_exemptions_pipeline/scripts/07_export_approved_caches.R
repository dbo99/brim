# Deliberate candidate promotion with archive and rollback metadata.

uic_approve_candidate <- function(candidate_id, confirm) {
  if (is.na(candidate_id) || !nzchar(candidate_id)) {
    stop("approve_candidate requires an explicit candidate_id.", call. = FALSE)
  }
  if (!identical(confirm, UIC_APPROVAL_PHRASE)) {
    stop(
      "Approval refused. Pass confirm = \"", UIC_APPROVAL_PHRASE, "\".",
      call. = FALSE
    )
  }
  paths <- uic_candidate_paths(candidate_id)
  if (!dir.exists(paths$root)) stop("Candidate does not exist: ", candidate_id)
  qa_path <- file.path(paths$qa, "qa_status.csv")
  if (!file.exists(qa_path)) stop("Candidate has no QA status: ", candidate_id)
  qa <- utils::read.csv(qa_path, stringsAsFactors = FALSE)
  if (!isTRUE(qa$passed[[1]])) {
    stop("Candidate QA did not pass; approval refused.", call. = FALSE)
  }
  approval_utc <- uic_utc_now()
  archive_id <- NA_character_
  if (dir.exists(UIC_APPROVED_DIR) &&
      length(list.files(UIC_APPROVED_DIR, all.files = TRUE, no.. = TRUE))) {
    archive_id <- paste0(
      "approved_", gsub("[^0-9]", "", approval_utc),
      "_before_", candidate_id
    )
    archive_target <- file.path(UIC_ARCHIVE_DIR, archive_id)
    if (dir.exists(archive_target)) stop("Archive target already exists: ", archive_target)
    uic_copy_tree(UIC_APPROVED_DIR, archive_target)
  }

  staging <- paste0(UIC_APPROVED_DIR, "_staging_", candidate_id)
  if (dir.exists(staging)) stop("Approval staging directory already exists: ", staging)
  uic_copy_tree(paths$root, staging)

  registry <- uic_read_config("source_registry.csv")
  status <- lapply(seq_len(nrow(registry)), function(i) {
    source <- registry[i, , drop = FALSE]
    key <- source$source_key
    full_path <- file.path(staging, "full", paste0(key, "_standardized.rds"))
    object <- readRDS(full_path)
    object$approved_snapshot_utc <- rep(approval_utc, nrow(object))
    object$source_status <- rep("approved research snapshot", nrow(object))
    object$popup_html <- gsub(
      "Candidate snapshot · not approved",
      "Approved research snapshot",
      object$popup_html,
      fixed = TRUE
    )
    saveRDS(object, full_path)
    map_path <- file.path(staging, "map_ready", paste0(key, "_map.rds"))
    if (file.exists(map_path)) {
      map_object <- readRDS(map_path)
      map_object$approved_snapshot_utc <- rep(approval_utc, nrow(map_object))
      map_object$source_status <- rep("approved research snapshot", nrow(map_object))
      map_object$popup_html <- gsub(
        "Candidate snapshot · not approved",
        "Approved research snapshot",
        map_object$popup_html,
        fixed = TRUE
      )
      saveRDS(map_object, map_path)
    }
    label_path <- file.path(staging, "labels", paste0(key, "_labels.rds"))
    if (file.exists(label_path)) {
      label_object <- readRDS(label_path)
      label_object$approved_snapshot_utc <- rep(
        approval_utc, nrow(label_object)
      )
      label_object$source_status <- rep(
        "approved research snapshot", nrow(label_object)
      )
      label_object$popup_html <- gsub(
        "Candidate snapshot · not approved",
        "Approved research snapshot",
        label_object$popup_html,
        fixed = TRUE
      )
      saveRDS(label_object, label_path)
    }
    data.frame(
      source_key = key,
      source_family = source$source_family,
      feature_count = nrow(object),
      retrieval_utc = object$retrieval_utc[[1]],
      approved_snapshot_utc = approval_utc,
      source_status = "approved research snapshot",
      stringsAsFactors = FALSE
    )
  })
  status <- do.call(rbind, status)
  uic_csv_write(status, file.path(staging, "metadata", "source_status.csv"))
  uic_json_write(
    list(
      pipeline_version = UIC_PIPELINE_VERSION,
      candidate_id = candidate_id,
      approved_snapshot_utc = approval_utc,
      archived_previous_approval = archive_id,
      confirmation_phrase = UIC_APPROVAL_PHRASE,
      rollback = if (is.na(archive_id)) NULL else file.path("archive", archive_id)
    ),
    file.path(staging, "metadata", "approval.json")
  )

  previous <- paste0(UIC_APPROVED_DIR, "_previous_", candidate_id)
  if (dir.exists(UIC_APPROVED_DIR)) {
    if (!file.rename(UIC_APPROVED_DIR, previous)) {
      stop("Could not move current approved directory aside; staging retained.")
    }
  }
  if (!file.rename(staging, UIC_APPROVED_DIR)) {
    if (dir.exists(previous)) file.rename(previous, UIC_APPROVED_DIR)
    stop("Could not promote approval staging directory; previous approval restored.")
  }
  if (dir.exists(previous)) unlink(previous, recursive = TRUE)
  list(
    candidate_id = candidate_id,
    approval_utc = approval_utc,
    archived_previous_approval = archive_id,
    approved_dir = UIC_APPROVED_DIR
  )
}
