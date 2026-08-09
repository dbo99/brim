#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(sf))

source("00_config/config_local_reference_interactions.r")
source("03_functions/local_reference_interaction_helpers.r")
source("02_preprocess/69_acec_pipeline/build_acec_overlap_pairs.R")

required_path <- function(variable) {
  path <- Sys.getenv(variable, unset = "")
  if (!nzchar(path) || !file.exists(path)) {
    stop(variable, " must name an existing focused ACEC overlap QA input.")
  }
  normalizePath(path, winslash = "/")
}

raw_path <- required_path("BRIM_ACEC_RAW_GEOJSON")
display_path <- required_path("BRIM_ACEC_DISPLAY_RDS")
enrichment_zip <- required_path("BRIM_ACEC_ENRICHMENT_ZIP")
components <- pt_local_reference_acec_components()
tracked_pairs <- pt_local_reference_acec_overlap_pairs()

normalize_guid <- function(x) {
  tolower(gsub("[{}[:space:]]", "", trimws(as.character(x))))
}

prepare_current_or_historical <- function(x) {
  x$component_id <- paste0("blmca-", normalize_guid(x$GlobalID))
  x$acec_id <- components$acec_id[match(x$component_id, components$component_id)]
  stopifnot(!anyNA(x$acec_id), !anyDuplicated(x$acec_id))
  sf::st_make_valid(sf::st_transform(
    x[, c("acec_id", "component_id", "ACEC_NAME")],
    3310
  ))
}

overlap_inventory <- function(x) {
  candidates <- sf::st_intersects(x)
  pair_index <- do.call(rbind, lapply(seq_len(nrow(x)), function(i) {
    j <- candidates[[i]]
    j <- j[j > i]
    if (length(j)) cbind(i = i, j = j) else NULL
  }))
  areas <- vapply(seq_len(nrow(pair_index)), function(k) {
    intersection <- suppressWarnings(sf::st_intersection(
      sf::st_geometry(x[pair_index[k, "i"], , drop = FALSE]),
      sf::st_geometry(x[pair_index[k, "j"], , drop = FALSE])
    ))
    if (!length(intersection) || all(sf::st_is_empty(intersection))) return(0)
    sum(as.numeric(sf::st_area(intersection)))
  }, numeric(1))
  data.frame(
    acec_id_a = pmin(
      x$acec_id[pair_index[, "i"]],
      x$acec_id[pair_index[, "j"]]
    ),
    acec_id_b = pmax(
      x$acec_id[pair_index[, "i"]],
      x$acec_id[pair_index[, "j"]]
    ),
    overlap_area_m2 = areas,
    stringsAsFactors = FALSE
  )
}

pair_keys <- function(inventory, threshold_m2 = 1) {
  selected <- inventory[inventory$overlap_area_m2 > threshold_m2, ]
  paste(selected$acec_id_a, selected$acec_id_b, sep = "|")
}

participant_count <- function(inventory, threshold_m2 = 1) {
  selected <- inventory[inventory$overlap_area_m2 > threshold_m2, ]
  length(unique(c(selected$acec_id_a, selected$acec_id_b)))
}

vertex_count <- function(x) {
  sum(vapply(sf::st_geometry(x), function(geometry) {
    nrow(sf::st_coordinates(geometry))
  }, integer(1)))
}

raw <- sf::st_read(raw_path, quiet = TRUE, stringsAsFactors = FALSE)
raw_invalid <- sum(!sf::st_is_valid(raw))
current_full <- prepare_current_or_historical(raw)
current_inventory <- overlap_inventory(current_full)

display <- readRDS(display_path)
stopifnot(inherits(display, "sf"), all(c("acec_id", "component_id") %in% names(display)))
display_projected <- sf::st_transform(
  display[, c("acec_id", "component_id", "ACEC_NAME")],
  3310
)
display_inventory <- overlap_inventory(display_projected)

zip_listing <- utils::unzip(enrichment_zip, list = TRUE)$Name
historical_entry <- zip_listing[
  grepl("/source_snapshot/supplied_acec\\.shp$", zip_listing)
]
stopifnot(length(historical_entry) == 1L)
historical <- sf::st_read(
  paste0("/vsizip/", enrichment_zip, "/", historical_entry),
  quiet = TRUE,
  stringsAsFactors = FALSE
)
historical_full <- prepare_current_or_historical(historical)
historical_inventory <- overlap_inventory(historical_full)

temporary_output <- tempfile("brim_acec_overlap_pairs_", fileext = ".csv")
on.exit(unlink(temporary_output), add = TRUE)
derived <- build_acec_overlap_pairs(
  current_raw_geojson = raw_path,
  output_path = temporary_output
)
derived_pairs <- utils::read.csv(
  temporary_output,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

current_keys <- pair_keys(current_inventory)
display_keys <- pair_keys(display_inventory)
historical_keys <- pair_keys(historical_inventory)
tracked_keys <- paste(
  tracked_pairs$acec_id_a,
  tracked_pairs$acec_id_b,
  sep = "|"
)

stopifnot(
  raw_invalid == 14L,
  all(sf::st_is_valid(current_full)),
  all(sf::st_is_valid(display_projected)),
  nrow(current_inventory) == 119L,
  sum(current_inventory$overlap_area_m2 > 0) == 72L,
  sum(current_inventory$overlap_area_m2 == 0) == 47L,
  sum(current_inventory$overlap_area_m2 > 0 &
        current_inventory$overlap_area_m2 <= 1) == 45L,
  length(current_keys) == 27L,
  participant_count(current_inventory) == 39L,
  length(display_keys) == 57L,
  participant_count(display_inventory) == 69L,
  length(intersect(current_keys, display_keys)) == 22L,
  length(setdiff(current_keys, display_keys)) == 5L,
  length(setdiff(display_keys, current_keys)) == 35L,
  setequal(current_keys, tracked_keys),
  setequal(current_keys, historical_keys),
  identical(derived_pairs, tracked_pairs),
  identical(derived$metrics$connected_component_sizes,
            c(7L, 6L, 5L, 3L, rep(2L, 9L))),
  derived$metrics$maximum_degree == 4L,
  sum(tracked_pairs$relationship_type == "containment") == 2L,
  vertex_count(raw) == 217760L,
  vertex_count(display) == 61066L
)

cat(
  "ACEC overlap QA passed: 27 current full-resolution pairs / 39 participants; ",
  "historical pair set exact; display simplification inventory 57 pairs / ",
  "69 participants with 35 display-only threshold artifacts.\n",
  sep = ""
)
