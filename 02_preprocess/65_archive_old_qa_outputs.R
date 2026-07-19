# ---- BRIM QA output archive helper ------------------------------------------
# Purpose:
#   Move older timestamped QA artifacts out of 04_processed_data/qa and into one
#   concentrated archive folder under 99_archive/qa_outputs/.  This keeps the
#   active QA folder focused on *_latest and untimestamped current-reference
#   files without deleting the historical run outputs.
#
# Usage from the BRIM project root:
#   source("02_preprocess/65_archive_old_qa_outputs.R")
#   archive_old_qa_outputs(dry_run = TRUE)   # preview only
#   archive_old_qa_outputs(dry_run = FALSE)  # move candidates to 99_archive

archive_old_qa_outputs <- function(root = getwd(),
                                   dry_run = TRUE,
                                   qa_rel = file.path("04_processed_data", "qa"),
                                   archive_rel = file.path("99_archive", "qa_outputs"),
                                   archive_run = format(Sys.time(), "%Y%m%d_%H%M%S"),
                                   verbose = TRUE) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)

  if (!file.exists(file.path(root, "run_build_map.r"))) {
    stop("This does not look like the BRIM project root: ", root,
         "\nExpected to find run_build_map.r")
  }

  qa_dir <- file.path(root, qa_rel)
  if (!dir.exists(qa_dir)) {
    stop("QA folder not found: ", qa_dir)
  }

  qa_dir <- normalizePath(qa_dir, winslash = "/", mustWork = TRUE)
  archive_root <- file.path(root, archive_rel)
  archive_run_dir <- file.path(archive_root, archive_run)

  rel_path <- function(x, base) {
    x <- normalizePath(x, winslash = "/", mustWork = FALSE)
    base <- normalizePath(base, winslash = "/", mustWork = TRUE)
    prefix <- paste0(base, "/")
    ifelse(startsWith(x, prefix), substring(x, nchar(prefix) + 1L), x)
  }

  dir_size <- function(path) {
    if (!dir.exists(path)) return(NA_real_)
    files <- list.files(path, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE)
    files <- files[file.exists(files) & !dir.exists(files)]
    if (!length(files)) return(0)
    sum(file.info(files)$size, na.rm = TRUE)
  }

  is_under_any <- function(paths, dirs) {
    if (!length(paths) || !length(dirs)) return(rep(FALSE, length(paths)))
    paths <- normalizePath(paths, winslash = "/", mustWork = FALSE)
    dirs <- normalizePath(dirs, winslash = "/", mustWork = FALSE)
    vapply(paths, function(p) {
      any(startsWith(paste0(p, "/"), paste0(dirs, "/")))
    }, logical(1))
  }

  unique_destination <- function(dest) {
    if (!file.exists(dest)) return(dest)
    parent <- dirname(dest)
    base <- basename(dest)
    i <- 1L
    repeat {
      candidate <- file.path(parent, paste0(base, "__dup_", i))
      if (!file.exists(candidate)) return(candidate)
      i <- i + 1L
    }
  }

  copy_dir_to_exact_destination <- function(src, dest) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    entries <- list.files(src, all.files = TRUE, full.names = TRUE, no.. = TRUE)
    if (!length(entries)) return(TRUE)
    all(file.copy(entries, dest, recursive = TRUE, copy.date = TRUE, overwrite = FALSE))
  }

  all_dirs <- list.dirs(qa_dir, recursive = TRUE, full.names = TRUE)
  all_dirs <- setdiff(all_dirs, qa_dir)

  # Do not recurse into any prior archive folder if one exists under QA.
  qa_archive_dir <- file.path(qa_dir, "archive")
  if (dir.exists(qa_archive_dir)) {
    all_dirs <- all_dirs[!is_under_any(all_dirs, qa_archive_dir) &
                           normalizePath(all_dirs, winslash = "/", mustWork = FALSE) !=
                           normalizePath(qa_archive_dir, winslash = "/", mustWork = FALSE)]
  }

  # Timestamped QA folders are usually run folders like 20260609_102055.
  timestamp_dir_paths <- all_dirs[grepl("^[0-9]{8}_[0-9]{6}$", basename(all_dirs))]

  # If timestamp folders ever nest, move only the top-most timestamp folder.
  if (length(timestamp_dir_paths) > 1L) {
    nested_in_other_timestamp_dir <- vapply(timestamp_dir_paths, function(p) {
      parents <- setdiff(timestamp_dir_paths, p)
      is_under_any(p, parents)
    }, logical(1))
    timestamp_dir_paths <- timestamp_dir_paths[!nested_in_other_timestamp_dir]
  }

  all_files <- list.files(qa_dir, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  all_files <- all_files[file.exists(all_files) & !dir.exists(all_files)]
  if (dir.exists(qa_archive_dir)) {
    all_files <- all_files[!is_under_any(all_files, qa_archive_dir)]
  }

  # Timestamped QA files are usually named like *_20260624_134845.csv.
  # Keep *_latest.* and untimestamped reference/summary files in the active QA folder.
  in_timestamp_dir <- is_under_any(all_files, timestamp_dir_paths)
  timestamp_file_paths <- all_files[
    !in_timestamp_dir &
      !grepl("_latest(\\.|$)", basename(all_files), ignore.case = TRUE) &
      grepl("_[0-9]{8}_[0-9]{6}(\\.[^.]+)?$", basename(all_files))
  ]

  candidate_paths <- c(timestamp_dir_paths, timestamp_file_paths)
  candidate_types <- c(rep("dir", length(timestamp_dir_paths)), rep("file", length(timestamp_file_paths)))

  if (!length(candidate_paths)) {
    if (verbose) message("No timestamped QA files/folders found to archive.")
    return(invisible(data.frame()))
  }

  candidate_rel <- vapply(candidate_paths, rel_path, character(1), base = qa_dir)
  candidate_dest <- file.path(archive_run_dir, qa_rel, candidate_rel)
  candidate_size <- ifelse(
    candidate_types == "dir",
    vapply(candidate_paths, dir_size, numeric(1)),
    file.info(candidate_paths)$size
  )

  manifest <- data.frame(
    item_type = candidate_types,
    source = candidate_paths,
    source_relative_to_qa = candidate_rel,
    destination = candidate_dest,
    size_bytes = as.numeric(candidate_size),
    moved = FALSE,
    stringsAsFactors = FALSE
  )

  if (verbose) {
    total_mb <- sum(manifest$size_bytes, na.rm = TRUE) / 1024^2
    message("QA archive candidates: ", nrow(manifest),
            " item(s), about ", round(total_mb, 1), " MB")
    message("Archive destination: ", archive_run_dir)
    if (dry_run) message("Dry run only; no files were moved.")
  }

  if (dry_run) {
    return(invisible(manifest))
  }

  dir.create(archive_run_dir, recursive = TRUE, showWarnings = FALSE)

  for (i in seq_len(nrow(manifest))) {
    src <- manifest$source[i]
    dest <- unique_destination(manifest$destination[i])
    manifest$destination[i] <- dest

    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

    ok <- file.rename(src, dest)

    # file.rename can fail across volumes; fall back to copy + unlink.
    if (!ok) {
      if (manifest$item_type[i] == "dir") {
        ok <- copy_dir_to_exact_destination(src, dest)
        if (ok) unlink(src, recursive = TRUE, force = TRUE)
      } else {
        ok <- file.copy(src, dest, overwrite = FALSE, copy.date = TRUE)
        if (ok) unlink(src, force = TRUE)
      }
    }

    manifest$moved[i] <- isTRUE(ok) && file.exists(dest) && !file.exists(src)
  }

  manifest_path <- file.path(archive_run_dir, "qa_archive_manifest.csv")
  utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")

  if (verbose) {
    message("Moved ", sum(manifest$moved), " of ", nrow(manifest), " item(s).")
    message("Manifest written: ", manifest_path)
    if (any(!manifest$moved)) {
      warning("Some QA archive candidates were not moved. Check the manifest.")
    }
  }

  invisible(manifest)
}
