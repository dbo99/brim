#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tibble)
})

source("00_config/config_labels.r")
source("03_functions/label_helpers.r")

required_file <- function(variable) {
  path <- Sys.getenv(variable, unset = "")
  if (!nzchar(path) || !file.exists(path)) {
    stop(variable, " must name an existing file.")
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

production_path <- required_file("BRIM_PRODUCTION_LABEL_CACHE")
isolated_old_path <- required_file("BRIM_OLD_ISOLATED_LABEL_CACHE")
canonical_path <- required_file("BRIM_CANONICAL_LABEL_CACHE")
reference_path <- required_file("BRIM_LABEL_REFERENCE_CACHE")

production <- readRDS(production_path)
isolated_old <- readRDS(isolated_old_path)
canonical <- readRDS(canonical_path)
reference <- readRDS(reference_path)

expected_children <- c(
  "huc2", "huc4", "huc6", "huc8", "huc10", "huc12",
  "gw_bull118", "county", "project_areas", "cnrfc_basins",
  "field_office_outer", "acec", "fedwilderness", "monuments",
  "wildernessstudyarea", "trails", "water_districts",
  "cnrfc_stream", "cnrfc_precip"
)
expected_rows <- c(
  huc2 = 4L, huc4 = 16L, huc6 = 24L, huc8 = 140L,
  huc10 = 1128L, huc12 = 5065L, gw_bull118 = 515L,
  county = 58L, project_areas = 0L, cnrfc_basins = 345L,
  field_office_outer = 14L, acec = 238L, fedwilderness = 197L, monuments = 22L,
  wildernessstudyarea = 63L, trails = 6L, water_districts = 3483L,
  cnrfc_stream = 2047L, cnrfc_precip = 3137L
)
if (!identical(names(canonical), expected_children)) {
  stop("Canonical aggregate child set/order is not exact.")
}
actual_rows <- vapply(canonical, nrow, integer(1))
if (!identical(actual_rows, expected_rows)) {
  stop("Canonical aggregate child row counts are not exact.")
}
stopifnot(
  !"major_conveyance" %in% names(canonical),
  identical(length(production), 18L),
  identical(length(isolated_old), 18L)
)

target_children <- LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY$source_nickname
expected_semantics <- c(
  acec = 238L, fedwilderness = 158L, monuments = 20L,
  wildernessstudyarea = 63L, trails = 6L
)
actual_semantics <- vapply(
  canonical[target_children],
  function(x) length(unique(as.character(x$semantic_feature_key))),
  integer(1)
)
if (!identical(actual_semantics, expected_semantics)) {
  stop("Canonical Local Reference semantic counts are not exact.")
}

for (index in seq_len(nrow(LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY))) {
  registration <- LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY[index, , drop = FALSE]
  nickname <- as.character(registration$source_nickname[[1]])
  pt_validate_local_reference_label_anchors(
    canonical[[nickname]], reference[[nickname]], registration
  )
}

same_text_multiset <- function(left, right) {
  if (!inherits(left, "sf") || !inherits(right, "sf") ||
      !"label_text" %in% names(left) || !"label_text" %in% names(right)) {
    return(FALSE)
  }
  identical(
    sort(as.character(left$label_text), na.last = TRUE),
    sort(as.character(right$label_text), na.last = TRUE)
  )
}

object_hash <- function(x) {
  digest::digest(x, algo = "sha256", serialize = TRUE)
}

all_names <- unique(c(
  names(production), names(isolated_old), names(canonical)
))
inventory <- lapply(all_names, function(child) {
  prod <- production[[child]]
  old <- isolated_old[[child]]
  current <- canonical[[child]]
  data.frame(
    child = child,
    classification = if (child == "major_conveyance") {
      "retired_omitted"
    } else if (child %in% target_children) {
      "registered_semantic_regenerated"
    } else {
      "protected_semantics_regenerated_from_current_accepted_cache"
    },
    production_rows = if (is.null(prod)) NA_integer_ else nrow(prod),
    old_isolated_rows = if (is.null(old)) NA_integer_ else nrow(old),
    canonical_rows = if (is.null(current)) NA_integer_ else nrow(current),
    canonical_semantic_features = if (
      is.null(current) || !"semantic_feature_key" %in% names(current)
    ) NA_integer_ else length(unique(current$semantic_feature_key)),
    production_text_multiset_preserved = if (
      is.null(prod) || is.null(current)
    ) NA else same_text_multiset(prod, current),
    old_isolated_text_multiset_preserved = if (
      is.null(old) || is.null(current)
    ) NA else same_text_multiset(old, current),
    production_child_identical = if (
      is.null(prod) || is.null(current)
    ) NA else identical(prod, current),
    old_isolated_child_identical = if (
      is.null(old) || is.null(current)
    ) NA else identical(old, current),
    canonical_child_sha256 = if (
      is.null(current)
    ) NA_character_ else object_hash(current),
    stringsAsFactors = FALSE
  )
}) |>
  dplyr::bind_rows()

protected <- setdiff(
  intersect(expected_children, names(production)),
  target_children
)
protected_rows <- inventory[inventory$child %in% protected, , drop = FALSE]
if (any(!protected_rows$production_text_multiset_preserved)) {
  stop("A protected unrelated child changed its label-text multiset.")
}

output_path <- Sys.getenv("BRIM_LABEL_RECONCILIATION_QA", unset = "")
if (nzchar(output_path)) {
  utils::write.csv(inventory, output_path, row.names = FALSE, na = "")
  message("Wrote label-cache reconciliation QA: ", output_path)
}

message("Canonical label-cache reconciliation QA passed.")
print(inventory, row.names = FALSE)
