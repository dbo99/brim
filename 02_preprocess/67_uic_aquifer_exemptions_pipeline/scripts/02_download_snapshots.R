# Retrieve complete timestamped raw source snapshots using object-ID batches.

uic_download_snapshots <- function(snapshot_id, service_check) {
  uic_require_packages("digest")
  registry <- uic_read_config("source_registry.csv")
  snapshot_dir <- file.path(UIC_RAW_SNAPSHOT_DIR, snapshot_id)
  if (dir.exists(snapshot_dir)) {
    stop("Immutable raw snapshot already exists: ", snapshot_dir, call. = FALSE)
  }
  uic_ensure_dirs(c(
    snapshot_dir,
    file.path(snapshot_dir, "geojson"),
    file.path(snapshot_dir, "metadata")
  ))
  manifest <- lapply(seq_len(nrow(registry)), function(i) {
    source <- registry[i, , drop = FALSE]
    info <- service_check$services[[source$source_key]]
    if (!isTRUE(info$ok)) {
      stop("Service verification failed for ", source$source_key, call. = FALSE)
    }
    destination <- file.path(
      snapshot_dir, "geojson", paste0(source$source_key, ".geojson")
    )
    started <- Sys.time()
    uic_download_geojson(source, destination, info)
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    data.frame(
      snapshot_id = snapshot_id,
      source_key = source$source_key,
      retrieval_utc = uic_utc_now(),
      feature_count = info$feature_count,
      object_id_field = info$object_id_field,
      geometry_type = info$geometry_type,
      elapsed_seconds = round(elapsed, 3),
      bytes = file.info(destination)$size,
      sha256 = digest::digest(
        file = destination,
        algo = "sha256",
        serialize = FALSE
      ),
      relative_path = file.path("geojson", basename(destination)),
      stringsAsFactors = FALSE
    )
  })
  manifest <- do.call(rbind, manifest)
  uic_csv_write(manifest, file.path(snapshot_dir, "snapshot_manifest.csv"))
  uic_json_write(
    list(
      pipeline_version = UIC_PIPELINE_VERSION,
      snapshot_id = snapshot_id,
      retrieval_utc = uic_utc_now(),
      source_count = nrow(registry),
      feature_count = sum(manifest$feature_count),
      immutable = TRUE
    ),
    file.path(snapshot_dir, "snapshot.json")
  )
  snapshot_dir
}
