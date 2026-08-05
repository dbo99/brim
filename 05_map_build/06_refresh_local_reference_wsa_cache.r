# ==== 06_refresh_local_reference_wsa_cache.r ================================
##
## PURPOSE:
##   Refresh only the Wilderness Study Areas child of the existing shared
##   Local reference-layer map cache after the focused WSA preprocessor runs.
##
## SAFETY CONTRACT:
##   - Reads the existing reference_layers_all_map cache as the baseline.
##   - Replaces only its `wildernessstudyarea` child.
##   - Fails before writing unless every sibling child's serialized SHA-256
##     remains unchanged and the WSA snapshot contract passes.
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
  stop("The focused WSA cache refresh requires the project's digest package.")
}

pt_validate_local_reference_config()

RUN_TS <- make_timestamp()
WSA_NICKNAME <- "wildernessstudyarea"
SHARED_REFERENCE_NICKNAMES <- c(
  "trails", "monuments", "cadesert_ncl", WSA_NICKNAME,
  "fedwilderness", "drecp", "acec", "allotments"
)

processed_path <- file.path(
  DIR$rds,
  paste0("reference_", WSA_NICKNAME, "_wgs84.rds")
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
if (!is.list(reference_layers_before) || !WSA_NICKNAME %in% names(reference_layers_before)) {
  stop("Existing shared reference cache does not contain `", WSA_NICKNAME, "`.")
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

wsa_source <- read_rds_checked(processed_path, "focused WSA processed output")
if (!inherits(wsa_source, "sf") || nrow(wsa_source) != 63L) {
  stop("Focused WSA processed output must be an sf object with 63 source features.")
}
source_nickname <- unique(as.character(wsa_source$pt_nickname))
if (!identical(source_nickname, WSA_NICKNAME)) {
  stop(
    "Focused WSA processed output has unexpected nickname(s): ",
    paste(source_nickname, collapse = ", ")
  )
}

keep_value <- unique(suppressWarnings(as.numeric(wsa_source$pt_simplify_keep)))
keep_value <- keep_value[!is.na(keep_value)]
if (length(keep_value) != 1L) {
  stop("Focused WSA processed output requires one non-missing simplification value.")
}
display_name <- unique(as.character(wsa_source$pt_display_name))
if (length(display_name) != 1L) {
  stop("Focused WSA processed output requires one display name.")
}

wsa_map <- pt_prepare_local_reference_wsa(
  wsa_source,
  validate_snapshot = TRUE
)
wsa_map <- simplify_sf_for_web(
  wsa_map,
  keep = keep_value,
  layer_label = display_name
)

reference_layers_after <- reference_layers_before
reference_layers_after[[WSA_NICKNAME]] <- wsa_map
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
if (!identical(changed_children, WSA_NICKNAME)) {
  stop(
    "Focused cache safety contract failed; changed child set was: ",
    paste(changed_children, collapse = ", ")
  )
}

shared_siblings <- setdiff(SHARED_REFERENCE_NICKNAMES, WSA_NICKNAME)
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
  paste0("local_reference_wsa_cache_child_hashes_", RUN_TS, ".csv")
)
utils::write.csv(hash_qa, hash_qa_path, row.names = FALSE, na = "")

wsa_qa_paths <- pt_write_local_reference_wsa_qa(
  wsa_map,
  DIR$qa,
  prefix = paste0("local_reference_wsa_cache_", RUN_TS)
)

message("Focused WSA cache refresh complete.")
message("Updated latest cache: ", cache_latest_path)
message("Saved timestamped cache: ", cache_timestamped_path)
message("Saved child-hash QA: ", hash_qa_path)
message("Saved WSA cache QA: ", paste(wsa_qa_paths, collapse = ", "))
print(hash_qa[hash_qa$cache_child %in% SHARED_REFERENCE_NICKNAMES, ], row.names = FALSE)
