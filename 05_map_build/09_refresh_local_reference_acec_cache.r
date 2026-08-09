# ==== 09_refresh_local_reference_acec_cache.r ===============================
## Replace only the ACEC child in the shared Local Reference cache and its
## matching label child. Every sibling is serialized and hashed before any
## write; an unexpected sibling change fails closed.

source("00_config/config_paths.r")
source("00_config/config_local_reference_interactions.r")
source("00_config/config_labels.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")
source("03_functions/label_helpers.r")
source("03_functions/local_reference_interaction_helpers.r")

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(htmltools)
})

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Focused ACEC cache refresh requires digest.")
}

pt_validate_local_reference_config()
RUN_TS <- make_timestamp()
ACEC_NICKNAME <- "acec"
processed_path <- file.path(DIR$rds, "reference_acec_wgs84.rds")
reference_latest_path <- file.path(DIR$cache_last, "reference_layers_all_map.rds")
labels_latest_path <- file.path(DIR$cache_last, "labels_all_map.rds")

object_sha256 <- function(x) digest::digest(x, algo = "sha256", serialize = TRUE)

hash_children <- function(x) {
  if (!is.list(x)) stop("Focused ACEC cache refresh requires a named list cache.")
  stats::setNames(vapply(x, object_sha256, character(1)), names(x))
}

assert_only_child_changed <- function(
  before, after, child, cache_label, require_child_change = TRUE
) {
  all_names <- union(names(before), names(after))
  before_hash <- hash_children(before)
  after_hash <- hash_children(after)
  changed <- vapply(all_names, function(name) {
    if (!name %in% names(before_hash) || !name %in% names(after_hash)) return(TRUE)
    !identical(before_hash[[name]], after_hash[[name]])
  }, logical(1))
  changed_names <- all_names[changed]
  valid_changed_names <- identical(changed_names, child) ||
    (!isTRUE(require_child_change) && length(changed_names) == 0L)
  if (!valid_changed_names) {
    stop(
      cache_label, " safety contract failed; changed child set was: ",
      if (length(changed_names)) paste(changed_names, collapse = ", ") else "none"
    )
  }
  data.frame(
    cache = cache_label,
    cache_child = all_names,
    before_sha256 = unname(before_hash[all_names]),
    after_sha256 = unname(after_hash[all_names]),
    changed = changed,
    stringsAsFactors = FALSE
  )
}

reference_before <- read_rds_checked(
  reference_latest_path, "existing shared Local Reference cache"
)
labels_before <- read_rds_checked(labels_latest_path, "existing shared label cache")
if (!ACEC_NICKNAME %in% names(reference_before) ||
    !ACEC_NICKNAME %in% names(labels_before)) {
  stop("Existing shared caches must both contain an `acec` child.")
}

source_layer <- read_rds_checked(processed_path, "reviewed current ACEC processed RDS")
metadata <- attr(source_layer, "pt_acec_candidate_metadata")
if (!inherits(source_layer, "sf") || nrow(source_layer) != 238L ||
    length(unique(source_layer$acec_id)) != 238L ||
    is.null(metadata) || !identical(as.numeric(metadata$simplify_tolerance_m), 1)) {
  stop("Reviewed ACEC processed RDS must retain the 238-row one-metre candidate contract.")
}
if (!identical(as.integer(metadata$processed_geometry_parts), 613L) ||
    !identical(as.integer(metadata$processed_invalid_geometries), 0L)) {
  stop("Reviewed ACEC processed RDS must retain 613 valid polygon parts.")
}

acec_map <- pt_prepare_local_reference_acec(
  source_layer, validate_snapshot = TRUE, build_display = TRUE
)
reference_after <- reference_before
reference_after[[ACEC_NICKNAME]] <- acec_map

acec_label_registration <- pt_local_reference_label_registration(
  source_nickname = ACEC_NICKNAME
)
acec_labels <- pt_make_local_reference_labels(
  acec_map,
  acec_label_registration
)
if (!inherits(acec_labels, "sf") || nrow(acec_labels) != 238L ||
    length(unique(acec_labels$semantic_feature_key)) != 238L ||
    any(!nzchar(pt_local_reference_clean_chr(acec_labels$label_text)))) {
  stop("ACEC label child must retain 238 nonblank semantic labels.")
}
pt_validate_local_reference_label_anchors(
  acec_labels,
  acec_map,
  acec_label_registration
)
labels_after <- labels_before
labels_after[[ACEC_NICKNAME]] <- acec_labels

reference_hash_qa <- assert_only_child_changed(
  reference_before, reference_after, ACEC_NICKNAME, "reference_layers_all_map"
)
labels_hash_qa <- assert_only_child_changed(
  labels_before, labels_after, ACEC_NICKNAME, "labels_all_map",
  require_child_change = FALSE
)
labels_changed <- any(labels_hash_qa$changed)

save_rds_cached(
  reference_after,
  timestamped_path = file.path(
    DIR$cache_enr, timestamped_name("reference_layers_all_map", "rds", RUN_TS)
  ),
  latest_path = reference_latest_path
)
if (labels_changed) {
  save_rds_cached(
    labels_after,
    timestamped_path = file.path(
      DIR$cache_enr, timestamped_name("labels_all_map", "rds", RUN_TS)
    ),
    latest_path = labels_latest_path
  )
}

hash_qa_path <- file.path(
  DIR$qa, paste0("local_reference_acec_cache_child_hashes_", RUN_TS, ".csv")
)
snapshot_qa_path <- file.path(
  DIR$qa, paste0("local_reference_acec_cache_snapshot_", RUN_TS, ".csv")
)
category_qa_path <- file.path(
  DIR$qa, paste0("local_reference_acec_cache_category_counts_", RUN_TS, ".csv")
)
utils::write.csv(
  rbind(reference_hash_qa, labels_hash_qa), hash_qa_path,
  row.names = FALSE, na = ""
)
utils::write.csv(
  pt_local_reference_acec_qa(acec_map), snapshot_qa_path,
  row.names = FALSE, na = ""
)
utils::write.csv(
  pt_local_reference_category_qa(acec_map, "acec"), category_qa_path,
  row.names = FALSE, na = ""
)

message("Focused ACEC cache refresh complete.")
message("Refreshed reference cache child: ", ACEC_NICKNAME)
message(
  if (labels_changed) "Refreshed label cache child: " else
    "Label cache remained byte-identical: ",
  ACEC_NICKNAME
)
message("Child-hash QA: ", hash_qa_path)
message("Snapshot QA: ", snapshot_qa_path)
