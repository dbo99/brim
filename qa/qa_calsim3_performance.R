#!/usr/bin/env Rscript

## Read-only retained-product diagnostics for the Local CalSim3.0 network.
##
## Usage:
##   Rscript qa/qa_calsim3_performance.R \
##     /path/to/calsim3_arcs_map.rds \
##     /path/to/calsim3_nodes_map.rds \
##     /temporary/output/directory
##
## This script never modifies either retained cache. It writes only diagnostic
## CSV/text files to the explicitly supplied output directory. R timings and
## structural payload estimates are not production browser timings.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    paste(
      "Usage: Rscript qa/qa_calsim3_performance.R",
      "/path/to/calsim3_arcs_map.rds",
      "/path/to/calsim3_nodes_map.rds",
      "/temporary/output/directory"
    ),
    call. = FALSE
  )
}

arc_path <- normalizePath(args[[1]], mustWork = TRUE)
node_path <- normalizePath(args[[2]], mustWork = TRUE)
output_dir <- normalizePath(
  args[[3]],
  mustWork = FALSE
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, mustWork = TRUE)

required_packages <- c("sf", "units")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing required QA package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages(library(sf))

arcs <- readRDS(arc_path)
nodes <- readRDS(node_path)
if (!inherits(arcs, "sf") || !inherits(nodes, "sf")) {
  stop("Both CalSim3 retained products must be sf objects.", call. = FALSE)
}
if (nrow(arcs) == 0L || nrow(nodes) == 0L) {
  stop("CalSim3 retained products must be non-empty.", call. = FALSE)
}

arc_attrs <- sf::st_drop_geometry(arcs)
node_attrs <- sf::st_drop_geometry(nodes)
arc_geometry_types <- as.character(sf::st_geometry_type(
  arcs,
  by_geometry = TRUE
))
node_geometry_types <- as.character(sf::st_geometry_type(
  nodes,
  by_geometry = TRUE
))
node_coordinates <- sf::st_coordinates(nodes)
arc_coordinates <- sf::st_coordinates(arcs)

valid_node_coordinate <- is.finite(node_coordinates[, "X"]) &
  is.finite(node_coordinates[, "Y"]) &
  abs(node_coordinates[, "X"]) <= 180 &
  abs(node_coordinates[, "Y"]) <= 90
exact_coordinate_key <- paste0(
  sprintf("%a", node_coordinates[, "X"]),
  "_",
  sprintf("%a", node_coordinates[, "Y"])
)
coordinate_7dp_key <- paste0(
  sprintf("%.7f", node_coordinates[, "X"]),
  "_",
  sprintf("%.7f", node_coordinates[, "Y"])
)
coordinate_6dp_key <- paste0(
  sprintf("%.6f", node_coordinates[, "X"]),
  "_",
  sprintf("%.6f", node_coordinates[, "Y"])
)
exact_coordinate_table <- table(exact_coordinate_key)
coordinate_7dp_table <- table(coordinate_7dp_key)
coordinate_6dp_table <- table(coordinate_6dp_key)
exact_duplicate_table <- exact_coordinate_table[
  exact_coordinate_table > 1L
]
exact_duplicate_sizes <- as.integer(exact_duplicate_table)

node_id_field <- if ("node_id_display" %in% names(node_attrs)) {
  "node_id_display"
} else if ("CalSim3_ID" %in% names(node_attrs)) {
  "CalSim3_ID"
} else {
  NA_character_
}
arc_id_field <- if ("Arc_ID" %in% names(arc_attrs)) {
  "Arc_ID"
} else {
  NA_character_
}

clean_id <- function(x) {
  out <- trimws(as.character(x))
  out[is.na(out) | out == ""] <- NA_character_
  out
}

node_id <- if (!is.na(node_id_field)) {
  clean_id(node_attrs[[node_id_field]])
} else {
  rep(NA_character_, nrow(nodes))
}
arc_id <- if (!is.na(arc_id_field)) {
  clean_id(arc_attrs[[arc_id_field]])
} else {
  rep(NA_character_, nrow(arcs))
}
node_id_table <- table(node_id, useNA = "no")
arc_id_table <- table(arc_id, useNA = "no")

## Arc_ID is audited as source data, never used to synthesize Finder endpoints.
## The common convention is type-prefixed, but retained exceptions are valid
## analytical rows and must remain searchable through explicit FromNode/ToNode.
arc_type <- clean_id(arc_attrs$Type)
arc_from <- clean_id(arc_attrs$FromNode)
arc_to <- clean_id(arc_attrs$ToNode)
expected_prefix <- unname(c(
  Channel = "C_",
  Diversion = "D_",
  Return = "R_",
  Inflow = "I_"
)[arc_type])
simple_expected_id <- ifelse(
  arc_type == "Channel",
  paste0("C_", ifelse(is.na(arc_from), "", arc_from)),
  ifelse(
    arc_type == "Diversion",
    paste0(
      "D_",
      ifelse(is.na(arc_from), "", arc_from),
      "_",
      ifelse(is.na(arc_to), "", arc_to)
    ),
    ifelse(
      arc_type == "Return",
      paste0(
        "R_",
        ifelse(is.na(arc_from), "", arc_from),
        "_",
        ifelse(is.na(arc_to), "", arc_to)
      ),
      ifelse(
        arc_type == "Inflow",
        paste0("I_", ifelse(is.na(arc_to), "", arc_to)),
        NA_character_
      )
    )
  )
)
arc_id_convention <- data.frame(
  row_number = seq_len(nrow(arc_attrs)),
  Arc_ID = arc_id,
  Type = arc_type,
  FromNode = arc_from,
  ToNode = arc_to,
  expected_type_prefix = expected_prefix,
  type_prefix_matches = !is.na(arc_id) &
    !is.na(expected_prefix) &
    startsWith(arc_id, expected_prefix),
  simple_expected_id = simple_expected_id,
  simple_convention_matches = !is.na(arc_id) &
    !is.na(simple_expected_id) &
    arc_id == simple_expected_id,
  stringsAsFactors = FALSE
)
arc_id_convention_summary <- do.call(
  rbind,
  lapply(
    sort(unique(arc_type)),
    function(type_value) {
      rows <- arc_id_convention$Type == type_value
      data.frame(
        Type = type_value,
        rows = sum(rows),
        type_prefix_matches = sum(
          arc_id_convention$type_prefix_matches[rows],
          na.rm = TRUE
        ),
        simple_convention_matches = sum(
          arc_id_convention$simple_convention_matches[rows],
          na.rm = TRUE
        ),
        explicit_from_values = sum(!is.na(arc_id_convention$FromNode[rows])),
        explicit_to_values = sum(!is.na(arc_id_convention$ToNode[rows])),
        stringsAsFactors = FALSE
      )
    }
  )
)

duplicate_id_rows <- function(attrs, ids, id_field, layer) {
  duplicate_values <- names(which(table(ids, useNA = "no") > 1L))
  if (!length(duplicate_values)) {
    return(data.frame(
      layer = character(0),
      id_field = character(0),
      id_value = character(0),
      row_number = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  hit <- which(ids %in% duplicate_values)
  data.frame(
    layer = layer,
    id_field = id_field,
    id_value = ids[hit],
    row_number = hit,
    stringsAsFactors = FALSE
  )
}

duplicate_ids <- rbind(
  duplicate_id_rows(
    arc_attrs,
    arc_id,
    arc_id_field,
    "calsim3_arcs"
  ),
  duplicate_id_rows(
    node_attrs,
    node_id,
    node_id_field,
    "calsim3_nodes"
  )
)

distance_started <- proc.time()[["elapsed"]]
distance_matrix_m <- units::drop_units(sf::st_distance(nodes))
diag(distance_matrix_m) <- Inf
nearest_node_m <- apply(distance_matrix_m, 1L, min)
distance_elapsed <- proc.time()[["elapsed"]] - distance_started
distance_threshold_m <- c(0.1, 1, 5, 10, 25, 50, 100, 250, 500)
near_distribution <- data.frame(
  threshold_m = distance_threshold_m,
  unordered_node_pairs = vapply(
    distance_threshold_m,
    function(threshold) sum(distance_matrix_m <= threshold) / 2,
    numeric(1)
  ),
  nodes_with_nearest_neighbor_within_threshold = vapply(
    distance_threshold_m,
    function(threshold) sum(nearest_node_m <= threshold),
    numeric(1)
  ),
  stringsAsFactors = FALSE
)

field_bytes <- function(x, field) {
  if (!field %in% names(x)) return(NA_real_)
  sum(
    nchar(enc2utf8(as.character(x[[field]])), type = "bytes"),
    na.rm = TRUE
  )
}

wkt_bytes <- function(x) {
  text <- sf::st_as_text(sf::st_geometry(x), digits = 16)
  sum(nchar(enc2utf8(text), type = "bytes"), na.rm = TRUE)
}

metrics <- data.frame(
  metric = c(
    "analytical_arc_records",
    "analytical_node_records",
    "total_network_records",
    "valid_map_node_coordinates",
    "unique_exact_node_coordinates",
    "unique_7_decimal_node_coordinates",
    "unique_6_decimal_node_coordinates",
    "exact_duplicate_coordinate_groups",
    "records_at_exact_duplicate_coordinates",
    "maximum_records_at_exact_coordinate",
    "distinct_arc_ids",
    "duplicate_arc_id_values",
    "missing_arc_ids",
    "distinct_node_ids",
    "duplicate_node_id_values",
    "missing_node_ids",
    "arc_coordinate_rows",
    "arc_retained_cache_bytes",
    "node_retained_cache_bytes",
    "arc_r_object_bytes",
    "node_r_object_bytes",
    "arc_geometry_wkt_proxy_bytes",
    "node_geometry_wkt_proxy_bytes",
    "arc_popup_bytes",
    "arc_hover_bytes",
    "node_popup_bytes",
    "node_hover_bytes",
    "minimum_nearest_node_distance_m",
    "nearest_distance_1pct_m",
    "nearest_distance_median_m",
    "nearest_distance_99pct_m",
    "nearest_distance_maximum_m",
    "distance_matrix_elapsed_seconds",
    "leaflet_arc_path_objects",
    "leaflet_node_marker_objects",
    "shared_calsim_canvas_renderers",
    "dom_icons_per_individual_node_above_transition"
  ),
  value = c(
    nrow(arcs),
    nrow(nodes),
    nrow(arcs) + nrow(nodes),
    sum(valid_node_coordinate),
    length(exact_coordinate_table),
    length(coordinate_7dp_table),
    length(coordinate_6dp_table),
    length(exact_duplicate_table),
    sum(exact_duplicate_sizes),
    if (length(exact_duplicate_sizes)) max(exact_duplicate_sizes) else 1L,
    length(arc_id_table),
    sum(arc_id_table > 1L),
    sum(is.na(arc_id)),
    length(node_id_table),
    sum(node_id_table > 1L),
    sum(is.na(node_id)),
    nrow(arc_coordinates),
    file.info(arc_path)$size,
    file.info(node_path)$size,
    as.numeric(object.size(arcs)),
    as.numeric(object.size(nodes)),
    wkt_bytes(arcs),
    wkt_bytes(nodes),
    field_bytes(arc_attrs, "popup_html"),
    field_bytes(arc_attrs, "hover_text"),
    field_bytes(node_attrs, "popup_html"),
    field_bytes(node_attrs, "hover_text"),
    min(nearest_node_m),
    unname(stats::quantile(nearest_node_m, 0.01)),
    stats::median(nearest_node_m),
    unname(stats::quantile(nearest_node_m, 0.99)),
    max(nearest_node_m),
    distance_elapsed,
    nrow(arcs),
    nrow(nodes),
    1L,
    0L
  ),
  note = c(
    "retained map-facing line rows",
    "retained map-facing point rows",
    "combined Local layer catalog count",
    "finite WGS84 point coordinates",
    "exact retained numeric coordinate pairs",
    "diagnostic only; display never rounds",
    "diagnostic only; display never rounds",
    "exact groups with more than one analytical record",
    "analytical records represented by exact duplicate groups",
    "bounded same-location group size",
    "Arc_ID values; repeats are reported, not removed",
    "Arc_ID values occurring more than once",
    "blank or missing Arc_ID rows",
    "node_id_display/CalSim3_ID values; repeats are reported, not removed",
    "node ID values occurring more than once",
    "blank or missing node ID rows",
    "map-facing line coordinate vertices",
    "compressed RDS file",
    "compressed RDS file",
    "in-memory retained sf object",
    "in-memory retained sf object",
    "structural text proxy, not exact HTML contribution",
    "structural text proxy, not exact HTML contribution",
    "eager retained popup text",
    "eager retained hover text",
    "eager retained popup text",
    "eager retained hover text",
    "geodesic sf distance",
    "geodesic sf distance",
    "geodesic sf distance",
    "geodesic sf distance",
    "geodesic sf distance",
    "R diagnostic timing only; not browser performance",
    "one native Leaflet polyline per retained arc row",
    "one native Leaflet circle marker per retained node row",
    "one pane-scoped Canvas shared by retained arcs and exact nodes",
    "individual nodes are Canvas paths; low-zoom cluster icon count is view-dependent"
  ),
  stringsAsFactors = FALSE
)

count_field <- function(attrs, field, layer) {
  if (!field %in% names(attrs)) {
    return(data.frame(
      layer = layer,
      field = field,
      value = "<missing>",
      rows = NA_integer_,
      stringsAsFactors = FALSE
    ))
  }
  value <- trimws(as.character(attrs[[field]]))
  value[is.na(value) | value == ""] <- "<blank>"
  tab <- sort(table(value), decreasing = TRUE)
  data.frame(
    layer = layer,
    field = field,
    value = names(tab),
    rows = as.integer(tab),
    stringsAsFactors = FALSE
  )
}

class_counts <- rbind(
  count_field(arc_attrs, "Type", "calsim3_arcs"),
  count_field(
    node_attrs,
    "calsim3_node_group",
    "calsim3_nodes"
  )
)

required_arc_fields <- c(
  "Arc_ID", "Name", "Type", "FromNode", "ToNode",
  "line_col", "line_weight", "popup_html", "hover_text"
)
required_node_fields <- c(
  "node_id_display", "node_description", "calsim3_node_group",
  "node_fill_col", "node_stroke_col", "node_radius",
  "popup_html", "hover_text"
)
field_presence <- rbind(
  data.frame(
    layer = "calsim3_arcs",
    field = required_arc_fields,
    present = required_arc_fields %in% names(arc_attrs),
    stringsAsFactors = FALSE
  ),
  data.frame(
    layer = "calsim3_nodes",
    field = required_node_fields,
    present = required_node_fields %in% names(node_attrs),
    stringsAsFactors = FALSE
  )
)

integrity <- data.frame(
  check = c(
    "all_arc_rows_are_linestring",
    "all_node_rows_are_point",
    "all_node_rows_have_valid_coordinates",
    "node_coordinate_rows_reconcile",
    "exact_coordinate_counts_reconcile",
    "arc_class_counts_reconcile",
    "node_class_counts_reconcile",
    "all_required_popup_hover_style_fields_present",
    "all_arc_types_are_nonblank",
    "all_node_groups_are_nonblank",
    "explicit_finder_endpoint_fields_present",
    "no_arc_ids_missing",
    "no_node_ids_missing"
  ),
  passed = c(
    all(arc_geometry_types == "LINESTRING"),
    all(node_geometry_types == "POINT"),
    all(valid_node_coordinate),
    nrow(node_coordinates) == nrow(nodes),
    sum(exact_coordinate_table) == nrow(nodes),
    sum(class_counts$rows[
      class_counts$layer == "calsim3_arcs"
    ]) == nrow(arcs),
    sum(class_counts$rows[
      class_counts$layer == "calsim3_nodes"
    ]) == nrow(nodes),
    all(field_presence$present),
    !any(is.na(arc_type)),
    !any(
      is.na(clean_id(node_attrs$calsim3_node_group))
    ),
    all(c("FromNode", "ToNode") %in% names(arc_attrs)),
    !any(is.na(arc_id)),
    !any(is.na(node_id))
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  metrics,
  file.path(output_dir, "calsim3_performance_metrics.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  class_counts,
  file.path(output_dir, "calsim3_feature_class_counts.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  near_distribution,
  file.path(output_dir, "calsim3_near_coordinate_distribution.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  duplicate_ids,
  file.path(output_dir, "calsim3_duplicate_identifier_rows.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  arc_id_convention,
  file.path(output_dir, "calsim3_arc_id_convention_rows.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  arc_id_convention_summary,
  file.path(output_dir, "calsim3_arc_id_convention_summary.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  field_presence,
  file.path(output_dir, "calsim3_required_field_presence.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  integrity,
  file.path(output_dir, "calsim3_integrity_checks.csv"),
  row.names = FALSE,
  na = ""
)

summary_lines <- c(
  "# CalSim3 retained-product source diagnostic",
  "",
  paste("- Arc rows:", nrow(arcs)),
  paste("- Node rows:", nrow(nodes)),
  paste("- Valid node coordinates:", sum(valid_node_coordinate)),
  paste("- Unique exact node coordinates:", length(exact_coordinate_table)),
  paste("- Exact duplicate-coordinate groups:", length(exact_duplicate_table)),
  paste("- Records at exact duplicate coordinates:", sum(exact_duplicate_sizes)),
  paste("- Minimum nearest-node distance (m):", round(min(nearest_node_m), 3)),
  paste("- Arc cache bytes:", file.info(arc_path)$size),
  paste("- Node cache bytes:", file.info(node_path)$size),
  paste(
    "- Arc IDs matching their type prefix:",
    sum(arc_id_convention$type_prefix_matches),
    "of",
    nrow(arc_id_convention)
  ),
  paste(
    "- Arc IDs matching the simple endpoint convention:",
    sum(arc_id_convention$simple_convention_matches),
    "of",
    nrow(arc_id_convention)
  ),
  paste(
    "- Arc IDs requiring explicit retained fields rather than parsing:",
    sum(!arc_id_convention$simple_convention_matches)
  ),
  paste("- All integrity checks passed:", all(integrity$passed)),
  "",
  "Structural counts and R timings do not prove production browser latency."
)
writeLines(
  summary_lines,
  file.path(output_dir, "calsim3_performance_summary.md")
)

if (!all(integrity$passed)) {
  print(integrity)
  stop("One or more CalSim3 retained-product checks failed.", call. = FALSE)
}

message("CalSim3 retained-product diagnostics passed.")
message("Output directory: ", output_dir)
