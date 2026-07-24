# Read raw GeoJSON, preserve attributes, repair processed geometry, and hash it.

uic_process_sources <- function(snapshot_id, candidate_id) {
  uic_require_packages(c("sf", "digest"))
  registry <- uic_read_config("source_registry.csv")
  paths <- uic_candidate_paths(candidate_id)
  uic_ensure_dirs(unname(paths))
  snapshot_dir <- file.path(UIC_RAW_SNAPSHOT_DIR, snapshot_id)
  snapshot_manifest <- utils::read.csv(
    file.path(snapshot_dir, "snapshot_manifest.csv"),
    stringsAsFactors = FALSE
  )
  repair_rows <- list()
  objects <- lapply(seq_len(nrow(registry)), function(i) {
    source <- registry[i, , drop = FALSE]
    source_key <- source$source_key
    input <- file.path(snapshot_dir, "geojson", paste0(source_key, ".geojson"))
    object <- suppressWarnings(sf::st_read(input, quiet = TRUE, stringsAsFactors = FALSE))
    if (nrow(object) != snapshot_manifest$feature_count[
      snapshot_manifest$source_key == source_key
    ]) {
      stop("Raw/manifest feature-count mismatch for ", source_key)
    }
    source_crs <- sf::st_crs(object)
    if (is.na(source_crs)) sf::st_crs(object) <- 4326
    object <- sf::st_transform(object, 3310)
    validity <- uic_make_valid(object)
    object <- validity$data
    if (validity$invalid_after) {
      stop(source_key, " has invalid geometry after repair.")
    }
    oid_name <- source$source_id_field
    if (!oid_name %in% names(object)) {
      oid_name <- if ("OBJECTID" %in% names(object)) "OBJECTID" else names(object)[[1]]
    }
    object$.uic_source_key <- source_key
    object$.uic_retrieval_utc <- snapshot_manifest$retrieval_utc[
      snapshot_manifest$source_key == source_key
    ][[1]]
    object$.uic_geometry_hash <- uic_geometry_hash(object)
    object$.uic_source_order <- seq_len(nrow(object))
    repaired_ids <- if (length(validity$repaired_rows)) {
      as.character(object[[oid_name]][validity$repaired_rows])
    } else {
      character()
    }
    repair_rows[[source_key]] <<- data.frame(
      source_key = rep(source_key, length(repaired_ids)),
      source_record_id = repaired_ids,
      repair_method = rep("sf::st_make_valid", length(repaired_ids)),
      stringsAsFactors = FALSE
    )
    saveRDS(object, file.path(paths$full, paste0(source_key, "_processed.rds")))
    object
  })
  names(objects) <- registry$source_key
  repairs <- do.call(rbind, repair_rows)
  if (is.null(repairs)) {
    repairs <- data.frame(
      source_key = character(),
      source_record_id = character(),
      repair_method = character()
    )
  }
  uic_csv_write(repairs, file.path(paths$qa, "geometry_repairs.csv"))
  objects
}
