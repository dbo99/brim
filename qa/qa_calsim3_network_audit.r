# ==== qa_calsim3_network_audit.r ============================================
##
## PURPOSE:
##   Fast, focused QA for the local CalSim3 network layer.  This is intentionally
##   much lighter than the full payload audit and is safe to run often while
##   refining CalSim3 symbology, hovers, and labels.
##
## OUTPUTS:
##   qa/calsim3_arc_type_counts_<timestamp>.csv
##   qa/calsim3_node_group_counts_<timestamp>.csv
##   qa/calsim3_node_description_counts_<timestamp>.csv
##   qa/calsim3_node_demand_code_counts_<timestamp>.csv
##   qa/calsim3_field_inventory_<timestamp>.csv

source("00_config/config_paths.r")
source("03_functions/cache_helpers.r")

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(tibble)
})

RUN_TS <- make_timestamp()

arc_path <- file.path(DIR$cache_last, "calsim3_arcs_map.rds")
node_path <- file.path(DIR$cache_last, "calsim3_nodes_map.rds")

if (!file.exists(arc_path)) {
  stop("Missing CalSim3 arc cache: ", arc_path)
}

if (!file.exists(node_path)) {
  stop("Missing CalSim3 node cache: ", node_path)
}

arcs <- readRDS(arc_path)
nodes <- readRDS(node_path)

pt_drop <- function(x) {
  if (inherits(x, "sf")) sf::st_drop_geometry(x) else as.data.frame(x)
}

pt_count_field <- function(x, field, label) {
  df <- pt_drop(x)

  if (!field %in% names(df)) {
    return(tibble::tibble(
      field = field,
      value = "<missing>",
      rows = NA_integer_,
      share = NA_real_,
      note = paste("Missing field for", label)
    ))
  }

  out <- df |>
    dplyr::mutate(
      value = trimws(as.character(.data[[field]])),
      value = dplyr::if_else(is.na(.data$value) | .data$value == "", "<blank>", .data$value)
    ) |>
    dplyr::count(.data$value, name = "rows", sort = TRUE) |>
    dplyr::mutate(
      field = field,
      share = .data$rows / sum(.data$rows),
      note = label
    ) |>
    dplyr::select(.data$field, .data$value, .data$rows, .data$share, .data$note)

  out
}

pt_extract_demand_code <- function(node_id) {
  id <- toupper(trimws(as.character(node_id)))
  out <- rep(NA_character_, length(id))
  hit <- grepl("^[0-9]{2}[A-Z]?_(PA|PU|PR|SA|SU|NA|NU|NR)[0-9]*$", id)
  out[hit] <- sub("^[0-9]{2}[A-Z]?_(PA|PU|PR|SA|SU|NA|NU|NR)[0-9]*$", "\\1", id[hit])
  out[is.na(out) | out == ""] <- "<not demand-code pattern>"
  out
}

arc_type_counts <- pt_count_field(arcs, "Type", "CalSim3 arc Type")
node_group_counts <- pt_count_field(nodes, "calsim3_node_group", "CalSim3 node grouped symbology")
node_description_counts <- pt_count_field(nodes, "node_description", "Raw/source node description")

node_df <- pt_drop(nodes)
node_id_field <- if ("node_id_display" %in% names(node_df)) {
  "node_id_display"
} else if ("CalSim3_ID" %in% names(node_df)) {
  "CalSim3_ID"
} else {
  NA_character_
}

node_demand_code_counts <- if (is.na(node_id_field)) {
  tibble::tibble(
    field = "node_id_display",
    value = "<missing>",
    rows = NA_integer_,
    share = NA_real_,
    note = "Cannot classify demand-code suffixes because node ID field is missing"
  )
} else {
  node_df |>
    dplyr::mutate(value = pt_extract_demand_code(.data[[node_id_field]])) |>
    dplyr::count(.data$value, name = "rows", sort = TRUE) |>
    dplyr::mutate(
      field = "demand_code_from_node_id",
      share = .data$rows / sum(.data$rows),
      note = paste0("Parsed from ", node_id_field, "; examples include PU/PA/NA/NU/SA/SU/PR/NR")
    ) |>
    dplyr::select(.data$field, .data$value, .data$rows, .data$share, .data$note)
}

field_inventory <- dplyr::bind_rows(
  tibble::tibble(
    layer = "calsim3_arcs",
    rows = if (inherits(arcs, "sf")) nrow(arcs) else NA_integer_,
    field = names(pt_drop(arcs)),
    nonblank_rows = vapply(pt_drop(arcs), function(v) {
      vv <- trimws(as.character(v))
      sum(!is.na(vv) & vv != "")
    }, numeric(1))
  ),
  tibble::tibble(
    layer = "calsim3_nodes",
    rows = if (inherits(nodes, "sf")) nrow(nodes) else NA_integer_,
    field = names(pt_drop(nodes)),
    nonblank_rows = vapply(pt_drop(nodes), function(v) {
      vv <- trimws(as.character(v))
      sum(!is.na(vv) & vv != "")
    }, numeric(1))
  )
) |>
  dplyr::mutate(run_timestamp = RUN_TS)

for (x in list(arc_type_counts, node_group_counts, node_description_counts, node_demand_code_counts)) {
  x$run_timestamp <- RUN_TS
}

out_arc_type <- file.path(DIR$qa, paste0("calsim3_arc_type_counts_", RUN_TS, ".csv"))
out_node_group <- file.path(DIR$qa, paste0("calsim3_node_group_counts_", RUN_TS, ".csv"))
out_node_desc <- file.path(DIR$qa, paste0("calsim3_node_description_counts_", RUN_TS, ".csv"))
out_node_demand <- file.path(DIR$qa, paste0("calsim3_node_demand_code_counts_", RUN_TS, ".csv"))
out_fields <- file.path(DIR$qa, paste0("calsim3_field_inventory_", RUN_TS, ".csv"))

readr::write_csv(arc_type_counts, out_arc_type)
readr::write_csv(node_group_counts, out_node_group)
readr::write_csv(node_description_counts, out_node_desc)
readr::write_csv(node_demand_code_counts, out_node_demand)
readr::write_csv(field_inventory, out_fields)

message("CalSim3 network QA complete.")
message("  Arc type counts:        ", out_arc_type)
message("  Node group counts:      ", out_node_group)
message("  Node description counts:", out_node_desc)
message("  Node demand-code counts:", out_node_demand)
message("  Field inventory:        ", out_fields)
message("")
message("Arc Type counts:")
print(arc_type_counts, n = Inf)
message("")
message("Node group counts:")
print(node_group_counts, n = Inf)
message("")
message("Node demand-code counts:")
print(node_demand_code_counts, n = Inf)
