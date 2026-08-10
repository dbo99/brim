# ==== 10_refresh_local_reference_national_monuments_cache.r ================
## Replace only the National Monuments child in the shared Local Reference
## cache and add/replace its matching semantic-label child. Every sibling is
## serialized and hashed before any write; unexpected changes fail closed.

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
  stop("Focused National Monuments cache refresh requires digest.")
}

pt_validate_local_reference_config()
RUN_TS <- make_timestamp()
MONUMENTS_NICKNAME <- "monuments"
processed_path <- file.path(DIR$rds, "reference_monuments_wgs84.rds")
reference_latest_path <- file.path(
  DIR$cache_last, "reference_layers_all_map.rds"
)
labels_latest_path <- file.path(DIR$cache_last, "labels_all_map.rds")

object_sha256 <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

hash_children <- function(x) {
  if (!is.list(x)) {
    stop("Focused National Monuments cache refresh requires a named list cache.")
  }
  stats::setNames(vapply(x, object_sha256, character(1)), names(x))
}

assert_only_child_changed <- function(
  before,
  after,
  child,
  cache_label,
  require_child_change = TRUE
) {
  all_names <- union(names(before), names(after))
  before_hash <- hash_children(before)
  after_hash <- hash_children(after)
  changed <- vapply(all_names, function(name) {
    if (!name %in% names(before_hash) || !name %in% names(after_hash)) {
      return(TRUE)
    }
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
if (!MONUMENTS_NICKNAME %in% names(reference_before)) {
  stop("Existing shared Local Reference cache has no `monuments` child.")
}

source_layer <- read_rds_checked(
  processed_path, "reviewed current National Monuments processed RDS"
)
metadata <- attr(source_layer, "pt_national_monuments_candidate_metadata")
if (!inherits(source_layer, "sf") || nrow(source_layer) != 22L ||
    length(unique(source_layer$monument_id)) != 20L ||
    is.null(metadata) ||
    !identical(as.numeric(metadata$simplify_tolerance_m), 1) ||
    !identical(as.integer(metadata$display_polygon_parts), 24432L) ||
    !identical(as.integer(metadata$display_vertices), 175540L) ||
    !isTRUE(metadata$exact_part_retention)) {
  stop(
    "Reviewed National Monuments RDS must retain the accepted ",
    "20 semantics / 22 display geometries / 24,432 parts / ",
    "175,540 vertices / one-metre contract."
  )
}

monuments_map <- pt_prepare_local_reference_national_monuments(
  source_layer,
  validate_snapshot = TRUE,
  build_display = TRUE
)
reference_after <- reference_before
reference_after[[MONUMENTS_NICKNAME]] <- monuments_map

monuments_label_registration <- pt_local_reference_label_registration(
  source_nickname = MONUMENTS_NICKNAME
)
monuments_labels <- pt_make_local_reference_labels(
  monuments_map,
  monuments_label_registration
)
if (!inherits(monuments_labels, "sf") || nrow(monuments_labels) != 22L ||
    length(unique(monuments_labels$semantic_feature_key)) != 20L ||
    any(!nzchar(pt_local_reference_clean_chr(monuments_labels$label_text)))) {
  stop(
    "National Monuments label child must retain 22 component-aware anchors ",
    "for 20 semantic monuments."
  )
}
pt_validate_local_reference_label_anchors(
  monuments_labels,
  monuments_map,
  monuments_label_registration
)
labels_after <- labels_before
labels_after[[MONUMENTS_NICKNAME]] <- monuments_labels

## Match the canonical full label builder's semantic-child order without
## rebuilding or reserializing any protected sibling child.
semantic_order <- LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY$source_nickname
semantic_order <- semantic_order[semantic_order %in% names(labels_after)]
first_semantic <- min(match(semantic_order, names(labels_after)), na.rm = TRUE)
before_semantic <- names(labels_after)[seq_len(first_semantic - 1L)]
after_semantic <- names(labels_after)[
  !names(labels_after) %in% c(before_semantic, semantic_order)
]
canonical_label_order <- c(before_semantic, semantic_order, after_semantic)
if (anyDuplicated(canonical_label_order) ||
    !setequal(canonical_label_order, names(labels_after))) {
  stop("Focused National Monuments label ordering contract failed.")
}
labels_after <- labels_after[canonical_label_order]

reference_hash_qa <- assert_only_child_changed(
  reference_before,
  reference_after,
  MONUMENTS_NICKNAME,
  "reference_layers_all_map",
  require_child_change = FALSE
)
labels_hash_qa <- assert_only_child_changed(
  labels_before,
  labels_after,
  MONUMENTS_NICKNAME,
  "labels_all_map",
  require_child_change = FALSE
)

save_rds_cached(
  reference_after,
  timestamped_path = file.path(
    DIR$cache_enr,
    timestamped_name("reference_layers_all_map", "rds", RUN_TS)
  ),
  latest_path = reference_latest_path
)
save_rds_cached(
  labels_after,
  timestamped_path = file.path(
    DIR$cache_enr,
    timestamped_name("labels_all_map", "rds", RUN_TS)
  ),
  latest_path = labels_latest_path
)

hash_qa_path <- file.path(
  DIR$qa,
  paste0("local_reference_national_monuments_cache_child_hashes_", RUN_TS, ".csv")
)
snapshot_qa_path <- file.path(
  DIR$qa,
  paste0("local_reference_national_monuments_cache_snapshot_", RUN_TS, ".csv")
)
category_qa_path <- file.path(
  DIR$qa,
  paste0("local_reference_national_monuments_cache_category_counts_", RUN_TS, ".csv")
)
label_qa_path <- file.path(
  DIR$qa,
  paste0("local_reference_national_monuments_label_anchors_", RUN_TS, ".csv")
)
utils::write.csv(
  rbind(reference_hash_qa, labels_hash_qa),
  hash_qa_path,
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  pt_local_reference_nm_qa(monuments_map),
  snapshot_qa_path,
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  pt_local_reference_category_qa(monuments_map, "national_monuments"),
  category_qa_path,
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  sf::st_drop_geometry(monuments_labels),
  label_qa_path,
  row.names = FALSE,
  na = ""
)

message("Focused National Monuments cache refresh complete.")
message("Refreshed only reference and label child: ", MONUMENTS_NICKNAME)
message("Child-hash QA: ", hash_qa_path)
message("Snapshot QA: ", snapshot_qa_path)
message("Label-anchor QA: ", label_qa_path)
