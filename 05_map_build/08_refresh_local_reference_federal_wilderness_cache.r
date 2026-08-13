# ==== 08_refresh_local_reference_federal_wilderness_cache.r =================
##
## Replace only the Federal Wilderness child in the shared Local Reference
## cache and its matching label child. Every sibling is serialized and hashed
## before writing; any sibling change fails closed.

source("00_config/config_paths.r")
source("00_config/config_local_reference_interactions.r")
source("00_config/config_labels.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")
source("03_functions/label_helpers.r")
source("03_functions/local_reference_interaction_helpers.r")
source("03_functions/polygon_generalization_helpers.r")

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(htmltools)
})

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Focused Federal Wilderness cache refresh requires digest.")
}

pt_validate_local_reference_config()
RUN_TS <- make_timestamp()
FEDERAL_WILDERNESS_NICKNAME <- "fedwilderness"
processed_path <- file.path(
  DIR$rds, "reference_fedwilderness_wgs84.rds"
)
reference_latest_path <- file.path(
  DIR$cache_last, "reference_layers_all_map.rds"
)
labels_latest_path <- file.path(
  DIR$cache_last, "labels_all_map.rds"
)

object_sha256 <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

hash_children <- function(x) {
  if (!is.list(x)) stop("Focused cache refresh requires a named list cache.")
  stats::setNames(vapply(x, object_sha256, character(1)), names(x))
}

assert_only_child_changed <- function(before, after, child, cache_label) {
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
  unexpected_changes <- setdiff(changed_names, child)
  if (length(unexpected_changes) > 0L) {
    stop(
      cache_label, " safety contract failed; changed child set was: ",
      paste(changed_names, collapse = ", ")
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
  reference_latest_path,
  "existing shared Local Reference cache"
)
labels_before <- read_rds_checked(
  labels_latest_path,
  "existing shared label cache"
)
if (!FEDERAL_WILDERNESS_NICKNAME %in% names(reference_before)) {
  stop("Existing shared Local Reference cache has no `fedwilderness` child.")
}

source_layer <- read_rds_checked(
  processed_path,
  "reviewed Federal Wilderness processed RDS"
)
if (!inherits(source_layer, "sf") || nrow(source_layer) != 197L ||
    length(unique(source_layer$wilderness_id)) != 158L) {
  stop("Reviewed Federal Wilderness processed RDS must retain 197 components and 158 wildernesses.")
}
keep_value <- unique(suppressWarnings(as.numeric(source_layer$pt_simplify_keep)))
keep_value <- keep_value[is.finite(keep_value)]
if (!identical(keep_value, 0.5)) {
  stop("Federal Wilderness cache acceptance requires pt_simplify_keep=0.5.")
}

federal_wilderness_map <- pt_prepare_local_reference_federal_wilderness(
  source_layer,
  validate_snapshot = TRUE,
  build_display = TRUE
)
federal_wilderness_map <- pt_apply_reviewed_polygon_geometry(
  "federal_wilderness", federal_wilderness_map, require_reviewed = TRUE
)
reference_after <- reference_before
reference_after[[FEDERAL_WILDERNESS_NICKNAME]] <- federal_wilderness_map

federal_wilderness_label_registration <-
  pt_local_reference_label_registration(
    source_nickname = FEDERAL_WILDERNESS_NICKNAME
  )
federal_wilderness_labels <- pt_make_local_reference_labels(
  federal_wilderness_map,
  federal_wilderness_label_registration
)
if (!inherits(federal_wilderness_labels, "sf") ||
    nrow(federal_wilderness_labels) != 197L ||
    length(unique(federal_wilderness_labels$semantic_feature_key)) != 158L ||
    any(!nzchar(pt_local_reference_clean_chr(federal_wilderness_labels$label_text)))) {
  stop(
    "Federal Wilderness label child must retain 197 component anchors for ",
    "158 nonblank semantic wilderness labels."
  )
}
pt_validate_local_reference_label_anchors(
  federal_wilderness_labels,
  federal_wilderness_map,
  federal_wilderness_label_registration
)
labels_after <- labels_before
labels_after[[FEDERAL_WILDERNESS_NICKNAME]] <- federal_wilderness_labels

reference_hash_qa <- assert_only_child_changed(
  reference_before,
  reference_after,
  FEDERAL_WILDERNESS_NICKNAME,
  "reference_layers_all_map"
)
labels_hash_qa <- assert_only_child_changed(
  labels_before,
  labels_after,
  FEDERAL_WILDERNESS_NICKNAME,
  "labels_all_map"
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
  paste0("local_reference_federal_wilderness_cache_child_hashes_", RUN_TS, ".csv")
)
utils::write.csv(
  rbind(reference_hash_qa, labels_hash_qa),
  hash_qa_path,
  row.names = FALSE,
  na = ""
)
snapshot_qa_paths <- pt_write_local_reference_fw_qa(
  federal_wilderness_map,
  DIR$qa,
  prefix = paste0("local_reference_federal_wilderness_cache_", RUN_TS)
)

message("Focused Federal Wilderness cache refresh complete.")
message("Refreshed only reference and label child: ", FEDERAL_WILDERNESS_NICKNAME)
message("Child-hash QA: ", hash_qa_path)
message("Snapshot QA: ", paste(snapshot_qa_paths, collapse = ", "))
