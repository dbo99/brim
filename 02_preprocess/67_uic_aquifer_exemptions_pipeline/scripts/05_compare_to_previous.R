# Compare a candidate or current service check with the approved snapshot.

uic_compare_records <- function(candidate, approved, source_key) {
  candidate_rows <- sf::st_drop_geometry(candidate)
  approved_rows <- sf::st_drop_geometry(approved)
  candidate_ids <- as.character(candidate_rows$source_id)
  approved_ids <- as.character(approved_rows$source_id)
  common <- intersect(candidate_ids, approved_ids)
  candidate_match <- match(common, candidate_ids)
  approved_match <- match(common, approved_ids)
  attribute_changed <- candidate_rows$attribute_hash[candidate_match] !=
    approved_rows$attribute_hash[approved_match]
  geometry_changed <- candidate_rows$geometry_hash[candidate_match] !=
    approved_rows$geometry_hash[approved_match]
  data.frame(
    source_key = source_key,
    candidate_count = nrow(candidate),
    approved_count = nrow(approved),
    added_ids = length(setdiff(candidate_ids, approved_ids)),
    removed_ids = length(setdiff(approved_ids, candidate_ids)),
    common_ids = length(common),
    attribute_changed_ids = sum(attribute_changed, na.rm = TRUE),
    geometry_changed_ids = sum(geometry_changed, na.rm = TRUE),
    meaningful_change = length(setdiff(candidate_ids, approved_ids)) > 0 ||
      length(setdiff(approved_ids, candidate_ids)) > 0 ||
      any(attribute_changed, na.rm = TRUE) ||
      any(geometry_changed, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

uic_compare_candidate <- function(candidate_id) {
  registry <- uic_read_config("source_registry.csv")
  paths <- uic_candidate_paths(candidate_id)
  summaries <- lapply(seq_len(nrow(registry)), function(i) {
    key <- registry$source_key[[i]]
    candidate_path <- file.path(paths$full, paste0(key, "_standardized.rds"))
    approved_path <- file.path(UIC_APPROVED_DIR, "full", paste0(key, "_standardized.rds"))
    if (!file.exists(approved_path)) {
      candidate <- readRDS(candidate_path)
      return(data.frame(
        source_key = key,
        candidate_count = nrow(candidate),
        approved_count = NA_integer_,
        added_ids = NA_integer_,
        removed_ids = NA_integer_,
        common_ids = NA_integer_,
        attribute_changed_ids = NA_integer_,
        geometry_changed_ids = NA_integer_,
        meaningful_change = NA,
        comparison_status = "no approved snapshot",
        stringsAsFactors = FALSE
      ))
    }
    row <- uic_compare_records(
      readRDS(candidate_path),
      readRDS(approved_path),
      key
    )
    row$comparison_status <- if (row$meaningful_change) "review required" else "no detected change"
    row
  })
  summary <- do.call(rbind, summaries)
  uic_csv_write(summary, file.path(paths$qa, "approved_comparison.csv"))
  lines <- c(
    "# UIC candidate comparison",
    "",
    paste0("Candidate: `", candidate_id, "`"),
    paste0("Compared UTC: ", uic_utc_now()),
    "",
    "| Source | Candidate | Approved | Added IDs | Removed IDs | Attribute changes | Geometry changes | Status |",
    "|---|---:|---:|---:|---:|---:|---:|---|",
    vapply(seq_len(nrow(summary)), function(i) {
      x <- summary[i, ]
      paste0(
        "| ", x$source_key, " | ", x$candidate_count, " | ",
        ifelse(is.na(x$approved_count), "—", x$approved_count), " | ",
        ifelse(is.na(x$added_ids), "—", x$added_ids), " | ",
        ifelse(is.na(x$removed_ids), "—", x$removed_ids), " | ",
        ifelse(is.na(x$attribute_changed_ids), "—", x$attribute_changed_ids), " | ",
        ifelse(is.na(x$geometry_changed_ids), "—", x$geometry_changed_ids), " | ",
        x$comparison_status, " |"
      )
    }, character(1)),
    "",
    "A changed count is evidence for review, not an automatic failure or approval."
  )
  uic_text_write(lines, file.path(paths$qa, "approved_comparison.md"))
  summary
}

uic_compare_check_to_approved <- function(service_inventory, report_dir) {
  approved_status_path <- file.path(UIC_APPROVED_DIR, "metadata", "source_status.csv")
  if (!file.exists(approved_status_path)) {
    comparison <- transform(
      service_inventory[, c("source_key", "feature_count")],
      approved_feature_count = NA_integer_,
      count_delta = NA_integer_,
      status = "no approved snapshot"
    )
  } else {
    approved <- utils::read.csv(approved_status_path, stringsAsFactors = FALSE)
    comparison <- merge(
      service_inventory[, c(
        "source_key", "available", "feature_count", "required_fields_ok",
        "geometry_ok", "ids_complete"
      )],
      approved[, c("source_key", "feature_count")],
      by = "source_key",
      all.x = TRUE,
      suffixes = c("_current", "_approved")
    )
    comparison$count_delta <- comparison$feature_count_current -
      comparison$feature_count_approved
    comparison$status <- ifelse(
      !comparison$available,
      "service unavailable",
      ifelse(
        !comparison$required_fields_ok | !comparison$geometry_ok | !comparison$ids_complete,
        "schema review required",
        ifelse(comparison$count_delta == 0, "count unchanged; record comparison pending", "upstream count changed")
      )
    )
  }
  uic_csv_write(comparison, file.path(report_dir, "approved_count_comparison.csv"))
  comparison
}

uic_compare_live_to_approved <- function(service_check, report_dir) {
  registry <- uic_read_config("source_registry.csv")
  if (!dir.exists(UIC_APPROVED_DIR)) {
    result <- data.frame(
      source_key = registry$source_key,
      comparison_status = "no approved snapshot",
      stringsAsFactors = FALSE
    )
    uic_csv_write(result, file.path(report_dir, "live_record_comparison.csv"))
    return(result)
  }
  temporary_root <- tempfile("uic_check_only_")
  uic_ensure_dirs(temporary_root)
  on.exit(unlink(temporary_root, recursive = TRUE), add = TRUE)
  summaries <- lapply(seq_len(nrow(registry)), function(i) {
    source <- registry[i, , drop = FALSE]
    key <- source$source_key
    approved_path <- file.path(
      UIC_APPROVED_DIR, "full", paste0(key, "_standardized.rds")
    )
    if (!file.exists(approved_path) || !isTRUE(service_check$services[[key]]$ok)) {
      return(data.frame(
        source_key = key,
        comparison_status = if (file.exists(approved_path)) {
          "service unavailable; approved snapshot unchanged"
        } else {
          "approved source product missing"
        },
        stringsAsFactors = FALSE
      ))
    }
    geojson <- file.path(temporary_root, paste0(key, ".geojson"))
    uic_download_geojson(source, geojson, service_check$services[[key]])
    current <- suppressWarnings(sf::st_read(
      geojson, quiet = TRUE, stringsAsFactors = FALSE
    ))
    if (is.na(sf::st_crs(current))) sf::st_crs(current) <- 4326
    current <- sf::st_transform(current, 3310)
    validity <- uic_make_valid(current)
    current <- validity$data
    current$.uic_source_key <- key
    current$.uic_retrieval_utc <- service_check$inventory$checked_utc[
      service_check$inventory$source_key == key
    ][[1]]
    current$.uic_geometry_hash <- uic_geometry_hash(current)
    current$.uic_source_order <- seq_len(nrow(current))
    current <- uic_standardize_source(current, source)
    row <- uic_compare_records(current, readRDS(approved_path), key)
    row$comparison_status <- if (row$meaningful_change) {
      "upstream record change detected; approved snapshot unchanged"
    } else {
      "no detected record change"
    }
    row
  })
  all_names <- unique(unlist(lapply(summaries, names), use.names = FALSE))
  summaries <- lapply(summaries, function(x) {
    for (name in setdiff(all_names, names(x))) x[[name]] <- NA
    x[, all_names, drop = FALSE]
  })
  result <- do.call(rbind, summaries)
  uic_csv_write(result, file.path(report_dir, "live_record_comparison.csv"))
  result
}

uic_write_latest_service_status <- function(service_inventory, count_comparison) {
  comparison_status <- setNames(
    count_comparison$status,
    count_comparison$source_key
  )
  latest <- data.frame(
    source_key = service_inventory$source_key,
    service_check_utc = service_inventory$checked_utc,
    current_feature_count = service_inventory$feature_count,
    available = service_inventory$available,
    required_fields_ok = service_inventory$required_fields_ok,
    geometry_ok = service_inventory$geometry_ok,
    ids_complete = service_inventory$ids_complete,
    source_status = unname(comparison_status[service_inventory$source_key]),
    stringsAsFactors = FALSE
  )
  latest$source_status[is.na(latest$source_status)] <- ifelse(
    latest$available[is.na(latest$source_status)],
    "service checked; comparison unavailable",
    "service unavailable"
  )
  uic_csv_write(
    latest,
    file.path(UIC_CHECK_DIR, "latest_service_status.csv")
  )
  latest
}
