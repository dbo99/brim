#!/usr/bin/env Rscript

# ==== 13_refresh_polygon_generalization_portfolio_caches.r ==================
## Apply the reviewed V3 polygon geometry bundle to the narrow set of existing
## map-ready caches. This script never regenerates geometry and never contacts
## a remote service. It validates every pinned parent, reviewed artifact, and
## source-row crosswalk before staging any cache output.

source("00_config/config_paths.r")
source("00_config/config_labels.r")
source("03_functions/cache_helpers.r")
source("03_functions/label_helpers.r")
source("03_functions/polygon_generalization_helpers.r")

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Polygon portfolio cache refresh requires digest.")
}

RUN_TS <- make_timestamp()
REGISTRY <- pt_polygon_generalization_read_registry()
CROSSWALK <- pt_polygon_generalization_read_crosswalk()

if (!identical(nrow(REGISTRY), 29L) ||
    sum(REGISTRY$action == "replace_geometry") != 24L ||
    sum(REGISTRY$action == "retain_current") != 5L) {
  stop("Polygon portfolio registry does not reproduce the accepted 29/24/5 contract.")
}

object_sha256 <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

read_latest <- function(filename, label) {
  read_rds_checked(file.path(DIR$cache_last, filename), label)
}

apply_reviewed <- function(layer_id, x) {
  pt_apply_reviewed_polygon_geometry(
    layer_id,
    x,
    require_reviewed = TRUE,
    registry = REGISTRY,
    crosswalk = CROSSWALK
  )
}

# ---- Load only portfolio-owned current caches ------------------------------

cache_before <- list(
  gw_bull118_map = read_latest(
    "gw_bull118_map.rds", "Bulletin 118 map cache"
  ),
  cnrfc_basin_product_availability_map = read_latest(
    "cnrfc_basin_product_availability_map.rds",
    "CNRFC Product Availability map cache"
  ),
  cnrfc_fnf_delta_map = read_latest(
    "cnrfc_fnf_delta_map.rds", "CNRFC FNF map cache"
  ),
  huc_all_map = read_latest("huc_all_map.rds", "HUC family map cache"),
  reference_layers_all_map = read_latest(
    "reference_layers_all_map.rds", "shared Local Reference map cache"
  ),
  rwqcb_regions_map = read_latest(
    "rwqcb_regions_map.rds", "RWQCB Regions map cache"
  ),
  nps_park_preserve_context_map = read_latest(
    "nps_park_preserve_context_map.rds", "NPS Park/Preserve context map cache"
  ),
  labels_all_map = read_latest("labels_all_map.rds", "canonical label cache")
)

reference_children <- c(
  gsp_areas = "gsps",
  adjudicated_gw_basins = "gwbasins_adjd",
  wsr_corridor_blm = "wsr_corridor_blm",
  wsr_corridor_lsrs_area = "wsr_corridor_lsrs_area",
  wsr_corridor_lsrs_status = "wsr_corridor_lsrs_status",
  national_monuments = "monuments",
  ca_desert_ncl = "cadesert_ncl",
  wilderness_study_areas = "wildernessstudyarea",
  federal_wilderness = "fedwilderness",
  drecp = "drecp",
  acec = "acec",
  grazing_allotments = "allotments"
)
missing_reference_children <- setdiff(
  unname(reference_children), names(cache_before$reference_layers_all_map)
)
if (length(missing_reference_children)) {
  stop(
    "Shared Local Reference cache is missing portfolio child(ren): ",
    paste(missing_reference_children, collapse = ", ")
  )
}
if (!identical(
  names(cache_before$huc_all_map),
  c("huc2", "huc4", "huc6", "huc8", "huc10", "huc12")
)) {
  stop("HUC cache child set/order differs from the reviewed portfolio contract.")
}

# ---- Replace geometry while retaining each prepared object's attributes ----

cache_after <- cache_before
cache_after$gw_bull118_map <- apply_reviewed(
  "bulletin118", cache_before$gw_bull118_map
)
cache_after$cnrfc_basin_product_availability_map <- apply_reviewed(
  "cnrfc_product_availability",
  cache_before$cnrfc_basin_product_availability_map
)
cache_after$cnrfc_fnf_delta_map <- apply_reviewed(
  "cnrfc_fnf_delta", cache_before$cnrfc_fnf_delta_map
)
cache_after$huc_all_map <- lapply(names(cache_before$huc_all_map), function(id) {
  apply_reviewed(id, cache_before$huc_all_map[[id]])
})
names(cache_after$huc_all_map) <- names(cache_before$huc_all_map)

for (layer_id in names(reference_children)) {
  child <- reference_children[[layer_id]]
  cache_after$reference_layers_all_map[[child]] <- apply_reviewed(
    layer_id,
    cache_before$reference_layers_all_map[[child]]
  )
}
cache_after$rwqcb_regions_map <- apply_reviewed(
  "rwqcb_regions", cache_before$rwqcb_regions_map
)
cache_after$nps_park_preserve_context_map <-
  pt_apply_reviewed_polygon_geometry_to_nps_context(
    cache_before$nps_park_preserve_context_map,
    require_reviewed = TRUE
  )

# CNRFC Product labels must be anchored to the exact reviewed Product display
# geometry while retaining the established child ID, parent group, zoom, and
# label text contract.
if (!"cnrfc_basins" %in% names(cache_before$labels_all_map)) {
  stop("Canonical label cache is missing the protected cnrfc_basins child.")
}
product <- cache_after$cnrfc_basin_product_availability_map
if (!"cnrfc_id" %in% names(product) || anyNA(product$cnrfc_id) ||
    anyDuplicated(product$cnrfc_id)) {
  stop("Reviewed CNRFC Product geometry requires complete unique cnrfc_id values.")
}
product$Basin <- as.character(product$cnrfc_id)
product_labels <- pt_make_polygon_labels(
  product,
  label_id = "cnrfc_basins",
  label_field = "Basin"
)
label_cfg <- pt_label_cfg("cnrfc_basins")
if (nrow(product_labels) != nrow(product) ||
    !setequal(product_labels$label_text, product$cnrfc_id) ||
    !identical(unique(product_labels$parent_group), label_cfg$parent_group) ||
    !identical(unique(product_labels$label_group), label_cfg$label_group) ||
    !identical(unique(product_labels$min_zoom), label_cfg$min_zoom) ||
    !identical(unique(product_labels$max_zoom), label_cfg$max_zoom)) {
  stop("CNRFC Product label child differs from its established identity/lifecycle contract.")
}
product_order <- match(product_labels$label_text, as.character(product$cnrfc_id))
inside <- lengths(sf::st_within(product_labels, product[product_order, ])) == 1L
if (!all(inside)) {
  stop("One or more CNRFC Product label anchors fall outside reviewed display geometry.")
}
cache_after$labels_all_map$cnrfc_basins <- product_labels

# ---- Protected sibling proof ------------------------------------------------

reference_before_hash <- vapply(
  cache_before$reference_layers_all_map, object_sha256, character(1)
)
reference_after_hash <- vapply(
  cache_after$reference_layers_all_map, object_sha256, character(1)
)
protected_reference <- setdiff(
  names(cache_before$reference_layers_all_map), unname(reference_children)
)
if (!identical(
  reference_before_hash[protected_reference],
  reference_after_hash[protected_reference]
)) {
  stop("A protected shared Local Reference cache sibling changed.")
}

label_before_hash <- vapply(cache_before$labels_all_map, object_sha256, character(1))
label_after_hash <- vapply(cache_after$labels_all_map, object_sha256, character(1))
protected_labels <- setdiff(names(cache_before$labels_all_map), "cnrfc_basins")
if (!identical(
  label_before_hash[protected_labels], label_after_hash[protected_labels]
)) {
  stop("A protected canonical label-cache sibling changed.")
}

# ---- Stage, verify, then atomically replace latest files --------------------

output_files <- paste0(names(cache_after), ".rds")
latest_paths <- file.path(DIR$cache_last, output_files)
timestamped_paths <- file.path(
  DIR$cache_enr,
  vapply(
    names(cache_after), timestamped_name, character(1),
    ext = "rds", timestamp = RUN_TS
  )
)
stage_dir <- file.path(
  DIR$cache_enr, paste0(".polygon_generalization_portfolio_stage_", RUN_TS)
)
rollback_dir <- file.path(
  DIR$cache_enr, paste0("polygon_generalization_portfolio_rollback_", RUN_TS)
)
if (dir.exists(stage_dir) || dir.exists(rollback_dir) ||
    any(file.exists(timestamped_paths))) {
  stop("Refusing to overwrite an existing portfolio stage/rollback/timestamped output.")
}
if (!dir.create(stage_dir, recursive = FALSE)) stop("Could not create stage directory.")
stage_paths <- file.path(stage_dir, output_files)
for (index in seq_along(cache_after)) {
  saveRDS(cache_after[[index]], stage_paths[[index]])
  staged <- readRDS(stage_paths[[index]])
  if (!identical(object_sha256(staged), object_sha256(cache_after[[index]]))) {
    stop("Staged cache object verification failed: ", output_files[[index]])
  }
}
stage_file_hash <- vapply(
  stage_paths, pt_polygon_generalization_sha256_file, character(1)
)

if (!dir.create(rollback_dir, recursive = FALSE)) {
  stop("Could not create portfolio rollback directory.")
}
rollback_paths <- file.path(rollback_dir, output_files)
if (!all(file.copy(latest_paths, rollback_paths, overwrite = FALSE))) {
  stop("Could not preserve every pre-refresh latest cache in the rollback directory.")
}

committed_latest <- character(0)
committed_timestamped <- character(0)
commit_error <- tryCatch({
  for (index in seq_along(cache_after)) {
    if (!file.copy(stage_paths[[index]], timestamped_paths[[index]], overwrite = FALSE)) {
      stop("Could not write timestamped cache: ", timestamped_paths[[index]])
    }
    committed_timestamped <- c(committed_timestamped, timestamped_paths[[index]])

    atomic_temp <- paste0(latest_paths[[index]], ".portfolio_", RUN_TS, ".tmp")
    if (file.exists(atomic_temp) ||
        !file.copy(stage_paths[[index]], atomic_temp, overwrite = FALSE) ||
        !identical(
          pt_polygon_generalization_sha256_file(atomic_temp),
          stage_file_hash[[index]]
        ) ||
        !file.rename(atomic_temp, latest_paths[[index]])) {
      stop("Atomic latest-cache replacement failed: ", latest_paths[[index]])
    }
    committed_latest <- c(committed_latest, latest_paths[[index]])
  }
  NULL
}, error = function(error) error)

if (inherits(commit_error, "error")) {
  recovery_ok <- all(file.copy(rollback_paths, latest_paths, overwrite = TRUE))
  if (length(committed_timestamped)) unlink(committed_timestamped, force = TRUE)
  stop(
    conditionMessage(commit_error),
    if (recovery_ok) " Pre-refresh latest caches were restored." else
      " AUTOMATIC ROLLBACK FAILED; use the preserved rollback directory.",
    call. = FALSE
  )
}

latest_file_hash <- vapply(
  latest_paths, pt_polygon_generalization_sha256_file, character(1)
)
if (!identical(unname(latest_file_hash), unname(stage_file_hash))) {
  recovery_ok <- all(file.copy(rollback_paths, latest_paths, overwrite = TRUE))
  stop(
    "Post-commit latest-cache hash verification failed.",
    if (recovery_ok) " Pre-refresh latest caches were restored." else
      " AUTOMATIC ROLLBACK FAILED; use the preserved rollback directory.",
    call. = FALSE
  )
}

qa <- data.frame(
  run_timestamp = RUN_TS,
  cache_file = output_files,
  before_object_sha256 = vapply(cache_before, object_sha256, character(1)),
  after_object_sha256 = vapply(cache_after, object_sha256, character(1)),
  latest_file_sha256 = unname(latest_file_hash),
  rollback_file = rollback_paths,
  status = "PASS",
  stringsAsFactors = FALSE
)
qa_path <- file.path(
  DIR$qa, paste0("polygon_generalization_portfolio_refresh_", RUN_TS, ".csv")
)
utils::write.csv(qa, qa_path, row.names = FALSE, na = "")
unlink(stage_paths, force = TRUE)
unlink(stage_dir, recursive = FALSE, force = TRUE)

message("Reviewed polygon portfolio cache refresh complete.")
message("Updated latest caches: ", paste(latest_paths, collapse = ", "))
message("Rollback bundle: ", rollback_dir)
message("QA: ", qa_path)
