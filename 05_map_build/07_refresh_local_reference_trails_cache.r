# ==== 07_refresh_local_reference_trails_cache.r ================================
##
## PURPOSE:
##   Refresh only the National Scenic/Historic Trails child of the existing shared
##   Local reference-layer map cache after the focused Trails preprocessor runs.
##
## SAFETY CONTRACT:
##   - Reads the existing reference_layers_all_map cache as the baseline.
##   - Replaces only its `trails` child.
##   - Fails before writing unless every sibling child's serialized SHA-256
##     remains unchanged and the Trails snapshot contract passes.
##   - Does not rebuild or write any other core-cache product.

source("00_config/config_paths.r")
source("00_config/config_local_reference_interactions.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")
source("03_functions/local_reference_interaction_helpers.r")

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(htmltools)
  library(rmapshaper)
})

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("The focused Trails cache refresh requires the project's digest package.")
}

pt_validate_local_reference_config()

RUN_TS <- make_timestamp()
TRAILS_NICKNAME <- "trails"
SHARED_REFERENCE_NICKNAMES <- c(
  TRAILS_NICKNAME, "monuments", "cadesert_ncl", "wildernessstudyarea",
  "fedwilderness", "drecp", "acec", "allotments"
)

processed_path <- file.path(
  DIR$rds,
  paste0("reference_", TRAILS_NICKNAME, "_wgs84.rds")
)
cache_latest_path <- file.path(DIR$cache_last, "reference_layers_all_map.rds")
cache_timestamped_path <- file.path(
  DIR$cache_enr,
  timestamped_name("reference_layers_all_map", "rds", timestamp = RUN_TS)
)

reference_layers_before <- read_rds_checked(
  cache_latest_path,
  "existing shared Local reference-layer map cache"
)
if (!is.list(reference_layers_before) || !TRAILS_NICKNAME %in% names(reference_layers_before)) {
  stop("Existing shared reference cache does not contain `", TRAILS_NICKNAME, "`.")
}
missing_shared <- setdiff(SHARED_REFERENCE_NICKNAMES, names(reference_layers_before))
if (length(missing_shared)) {
  stop(
    "Existing shared reference cache is missing approved shared child(ren): ",
    paste(missing_shared, collapse = ", ")
  )
}

object_sha256 <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}
before_hash <- vapply(reference_layers_before, object_sha256, character(1))

trails_source <- read_rds_checked(processed_path, "focused Trails processed output")
if (!inherits(trails_source, "sf") || nrow(trails_source) != 6L) {
  stop("Focused Trails processed output must be an sf object with 6 source features.")
}
source_nickname <- unique(as.character(trails_source$pt_nickname))
if (!identical(source_nickname, TRAILS_NICKNAME)) {
  stop(
    "Focused Trails processed output has unexpected nickname(s): ",
    paste(source_nickname, collapse = ", ")
  )
}

keep_value <- unique(suppressWarnings(as.numeric(trails_source$pt_simplify_keep)))
keep_value <- keep_value[!is.na(keep_value)]
if (length(keep_value) != 1L) {
  stop("Focused Trails processed output requires one non-missing simplification value.")
}
display_name <- unique(as.character(trails_source$pt_display_name))
if (length(display_name) != 1L) {
  stop("Focused Trails processed output requires one display name.")
}

trails_map <- pt_prepare_local_reference_trails(
  trails_source,
  validate_snapshot = TRUE
)
trails_map <- simplify_sf_for_web(
  trails_map,
  keep = keep_value,
  layer_label = display_name
)

reference_layers_after <- reference_layers_before
reference_layers_after[[TRAILS_NICKNAME]] <- trails_map
after_hash <- vapply(reference_layers_after, object_sha256, character(1))

hash_qa <- data.frame(
  cache_child = names(reference_layers_before),
  before_sha256 = unname(before_hash),
  after_sha256 = unname(after_hash[names(reference_layers_before)]),
  changed = unname(before_hash != after_hash[names(reference_layers_before)]),
  shared_reference_child = names(reference_layers_before) %in% SHARED_REFERENCE_NICKNAMES,
  stringsAsFactors = FALSE
)
changed_children <- hash_qa$cache_child[hash_qa$changed]
if (!identical(changed_children, TRAILS_NICKNAME)) {
  stop(
    "Focused cache safety contract failed; changed child set was: ",
    paste(changed_children, collapse = ", ")
  )
}

shared_siblings <- setdiff(SHARED_REFERENCE_NICKNAMES, TRAILS_NICKNAME)
if (any(hash_qa$changed[match(shared_siblings, hash_qa$cache_child)])) {
  stop("One or more of the seven shared reference-cache siblings changed.")
}

save_rds_cached(
  x = reference_layers_after,
  timestamped_path = cache_timestamped_path,
  latest_path = cache_latest_path
)

hash_qa_path <- file.path(
  DIR$qa,
  paste0("local_reference_trails_cache_child_hashes_", RUN_TS, ".csv")
)
utils::write.csv(hash_qa, hash_qa_path, row.names = FALSE, na = "")

trails_qa_paths <- pt_write_local_reference_trails_qa(
  trails_map,
  DIR$qa,
  prefix = paste0("local_reference_trails_cache_", RUN_TS)
)

message("Focused Trails cache refresh complete.")
message("Updated latest cache: ", cache_latest_path)
message("Saved timestamped cache: ", cache_timestamped_path)
message("Saved child-hash QA: ", hash_qa_path)
message("Saved Trails cache QA: ", paste(trails_qa_paths, collapse = ", "))
print(hash_qa[hash_qa$cache_child %in% SHARED_REFERENCE_NICKNAMES, ], row.names = FALSE)
