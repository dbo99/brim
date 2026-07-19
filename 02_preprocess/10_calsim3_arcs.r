# ==== 10_calsim3_arcs.r ======================================================
##
## PURPOSE:
##   Prepare CalSim3 model arcs and nodes for PortaTreasure2.
##
## INPUTS:
##   01_raw_data/calsim3/calsim3arcs.shp
##   01_raw_data/calsim3/calsim3nodes.shp
##
## ARCS FILTER:
##   Keep:
##     Type == "Channel"
##     Type == "Diversion"
##     Type == "Return"
##     Type == "Inflow"
##
## NODE DESIGN:
##   Keep a lean point layer with only the fields needed for map hovers,
##   popups, and grouped symbology.  The full raw shapefile is intentionally not
##   carried into the map cache.
##
## OUTPUTS:
##   04_processed_data/rds/calsim3_arcs_wgs84.rds
##   04_processed_data/rds/calsim3_arcs_wgs84_<timestamp>.rds
##   04_processed_data/rds/calsim3_nodes_wgs84.rds
##   04_processed_data/rds/calsim3_nodes_wgs84_<timestamp>.rds
##   04_processed_data/gpkg/calsim3_arcs.gpkg       optional
##   04_processed_data/gpkg/calsim3_nodes.gpkg      optional
##   04_processed_data/qa/calsim3_arcs_nodes_qa_<timestamp>.csv
##

# ==== 1. Load configuration and helper functions =============================

source("00_config/config_paths.r")
source("00_config/config_run_flags.r")
source("00_config/config_source_files.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(tibble)
})

# ==== 3. User-facing switches ================================================

WRITE_GPKG <- FALSE
WRITE_QA   <- TRUE

RUN_TS <- make_timestamp()

# ==== 4. Define output paths =================================================

out_arcs_rds_latest <- file.path(DIR$rds, "calsim3_arcs_wgs84.rds")

out_arcs_rds_timestamped <- file.path(
  DIR$rds,
  timestamped_name("calsim3_arcs_wgs84", "rds", RUN_TS)
)

out_nodes_rds_latest <- file.path(DIR$rds, "calsim3_nodes_wgs84.rds")

out_nodes_rds_timestamped <- file.path(
  DIR$rds,
  timestamped_name("calsim3_nodes_wgs84", "rds", RUN_TS)
)

out_arcs_gpkg <- file.path(DIR$gpkg, "calsim3_arcs.gpkg")
out_nodes_gpkg <- file.path(DIR$gpkg, "calsim3_nodes.gpkg")

out_qa <- file.path(
  DIR$qa,
  paste0("calsim3_arcs_nodes_qa_", RUN_TS, ".csv")
)

# ==== 5. Shared helpers ======================================================

pt_first_nonblank_field <- function(x, candidate_fields) {
  n <- nrow(x)
  out <- rep(NA_character_, n)
  hit_fields <- candidate_fields[candidate_fields %in% names(x)]

  if (length(hit_fields) == 0) {
    return(out)
  }

  for (nm in hit_fields) {
    vals <- as.character(x[[nm]])
    vals <- trimws(vals)
    vals[is.na(vals) | vals == "" | vals == "NA"] <- NA_character_

    fill <- is.na(out) & !is.na(vals)
    out[fill] <- vals[fill]
  }

  out
}

# ==== 6. Check inputs ========================================================

if (!file.exists(SRC$calsim3_arcs)) {
  stop("Missing CalSim3 arcs shapefile: ", SRC$calsim3_arcs)
}

if (!file.exists(SRC$calsim3_nodes)) {
  stop("Missing CalSim3 nodes shapefile: ", SRC$calsim3_nodes)
}

message("Reading CalSim3 arcs:")
message("  ", SRC$calsim3_arcs)

message("Reading CalSim3 nodes:")
message("  ", SRC$calsim3_nodes)

# ==== 7. Read raw layers =====================================================

calsim3_arcs_raw <- sf::st_read(
  SRC$calsim3_arcs,
  quiet = TRUE
)

calsim3_nodes_raw <- sf::st_read(
  SRC$calsim3_nodes,
  quiet = TRUE
)

# ==== 8. Validate required fields ===========================================

if (!"Type" %in% names(calsim3_arcs_raw)) {
  stop(
    "CalSim3 arcs layer is missing required field: Type\n",
    "Available fields: ",
    paste(names(calsim3_arcs_raw), collapse = ", ")
  )
}

if (!"NodeDescri" %in% names(calsim3_nodes_raw)) {
  stop(
    "CalSim3 nodes layer is missing required field: NodeDescri\n",
    "Available fields: ",
    paste(names(calsim3_nodes_raw), collapse = ", ")
  )
}

message("Raw CalSim3 arc rows: ", nrow(calsim3_arcs_raw))
message("Raw CalSim3 node rows: ", nrow(calsim3_nodes_raw))

message("Raw arc Type counts:")
print(table(calsim3_arcs_raw$Type, useNA = "always"))

message("Raw node NodeDescri counts:")
print(table(calsim3_nodes_raw$NodeDescri, useNA = "always"))

# ==== 9. Filter, slim, and clean CalSim3 arcs ================================
##
## Keep only the fields needed for styling and popups.

required_arc_fields <- c("Type", "Name", "Arc_ID", "FromNode", "ToNode")
missing_arc_fields <- setdiff(required_arc_fields, names(calsim3_arcs_raw))

if (length(missing_arc_fields) > 0) {
  stop(
    "CalSim3 arcs layer is missing required field(s): ",
    paste(missing_arc_fields, collapse = ", "),
    "\nAvailable fields: ",
    paste(names(calsim3_arcs_raw), collapse = ", ")
  )
}

calsim3_arcs <- calsim3_arcs_raw |>
  dplyr::filter(.data$Type %in% c("Channel", "Diversion", "Return", "Inflow")) |>
  dplyr::select(
    Name,
    Arc_ID,
    Type,
    FromNode,
    ToNode,
    geometry
  ) |>
  to_wgs84() |>
  clean_sf_for_leaflet() |>
  dplyr::mutate(
    source = "CalSim3 arcs"
  )

message("CalSim3 arc rows after Type filter: ", nrow(calsim3_arcs))
message("Filtered arc Type counts:")
print(table(calsim3_arcs$Type, useNA = "always"))

# ==== 10. Slim and clean CalSim3 nodes =======================================
##
## The node shapefile is kept intentionally lean for the final HTML.  The map
## needs only a stable node code, original node description, river name, and
## comments.
##
## REQUIRED FIELDS:
##   CalSim3_ID  = node code shown as the first popup/hover row
##   NodeDescri  = detailed node type used for grouped symbology
##
## OPTIONAL FIELDS:
##   Riv_Name    = river/location name where available
##   Comment     = model/source comment where available

node_required_fields <- c("CalSim3_ID", "NodeDescri")
node_missing_fields <- setdiff(node_required_fields, names(calsim3_nodes_raw))

if (length(node_missing_fields) > 0) {
  stop(
    "CalSim3 nodes layer is missing required field(s): ",
    paste(node_missing_fields, collapse = ", "),
    "\nAvailable fields: ",
    paste(names(calsim3_nodes_raw), collapse = ", ")
  )
}

pt_node_chr <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == "" | x == "NA"] <- NA_character_
  x
}

pt_optional_node_col <- function(x, nm) {
  if (nm %in% names(x)) {
    pt_node_chr(x[[nm]])
  } else {
    rep(NA_character_, nrow(x))
  }
}

calsim3_nodes <- calsim3_nodes_raw |>
  dplyr::mutate(
    node_id_display = pt_node_chr(.data$CalSim3_ID),
    node_description = pt_node_chr(.data$NodeDescri),
    riv_name_display = if ("Riv_Name" %in% names(calsim3_nodes_raw)) {
      pt_node_chr(.data$Riv_Name)
    } else {
      NA_character_
    },
    comment_display = if ("Comment" %in% names(calsim3_nodes_raw)) {
      pt_node_chr(.data$Comment)
    } else {
      NA_character_
    },
    node_id_display = dplyr::if_else(
      is.na(.data$node_id_display) | .data$node_id_display == "",
      paste0("CalSim3 node ", dplyr::row_number()),
      .data$node_id_display
    )
  ) |>
  dplyr::select(
    node_id_display,
    node_description,
    CalSim3_ID,
    NodeDescri,
    riv_name_display,
    comment_display,
    dplyr::any_of(c("Riv_Name", "Comment")),
    geometry
  ) |>
  to_wgs84() |>
  clean_sf_for_leaflet() |>
  dplyr::mutate(
    source = "CalSim3 nodes"
  )

message("CalSim3 node rows saved: ", nrow(calsim3_nodes))
message("Node fields retained:")
print(names(calsim3_nodes))

# ==== 11. Save RDS outputs ===================================================

save_rds_cached(
  x = calsim3_arcs,
  timestamped_path = out_arcs_rds_timestamped,
  latest_path = out_arcs_rds_latest
)

save_rds_cached(
  x = calsim3_nodes,
  timestamped_path = out_nodes_rds_timestamped,
  latest_path = out_nodes_rds_latest
)

# ==== 12. Save GPKG outputs ==================================================
##
## GPKG writing can fail if an older file already exists, especially if it is
## open in ArcGIS Pro, QGIS, Windows Explorer preview, or being synced by
## OneDrive. Delete the old GPKG first, then write a fresh one.

if (WRITE_GPKG) {

  for (path in c(out_arcs_gpkg, out_nodes_gpkg)) {
    if (file.exists(path)) {
      message("Existing GPKG found; deleting before rewrite:")
      message("  ", path)

      removed <- file.remove(path)

      if (!removed) {
        stop(
          "Could not delete existing GPKG. Close ArcGIS/QGIS/Explorer previews ",
          "or pause OneDrive sync, then rerun.\n",
          path
        )
      }

      Sys.sleep(0.5)
    }
  }

  sf::st_write(
    calsim3_arcs,
    dsn = out_arcs_gpkg,
    layer = "calsim3_arcs",
    quiet = TRUE
  )

  sf::st_write(
    calsim3_nodes,
    dsn = out_nodes_gpkg,
    layer = "calsim3_nodes",
    quiet = TRUE
  )

  message("Saved GPKG: ", out_arcs_gpkg)
  message("Saved GPKG: ", out_nodes_gpkg)
}

# ==== 13. Save QA output =====================================================

if (WRITE_QA) {

  arc_type_counts <- calsim3_arcs |>
    sf::st_drop_geometry() |>
    dplyr::count(Type, name = "rows") |>
    dplyr::mutate(
      check = paste0("arc_type_", Type),
      value = as.character(rows)
    ) |>
    dplyr::select(check, value)

  node_descr_counts <- calsim3_nodes |>
    sf::st_drop_geometry() |>
    dplyr::count(node_description, name = "rows") |>
    dplyr::mutate(
      check = paste0(
        "node_description_",
        dplyr::coalesce(node_description, "NA")
      ),
      value = as.character(rows)
    ) |>
    dplyr::select(check, value)

  qa <- dplyr::bind_rows(
    tibble::tibble(
      check = c(
        "arc_source_file",
        "node_source_file",
        "raw_arc_rows",
        "filtered_arc_rows",
        "raw_node_rows",
        "node_rows",
        "arc_output_crs_epsg",
        "node_output_crs_epsg"
      ),
      value = c(
        basename(SRC$calsim3_arcs),
        basename(SRC$calsim3_nodes),
        as.character(nrow(calsim3_arcs_raw)),
        as.character(nrow(calsim3_arcs)),
        as.character(nrow(calsim3_nodes_raw)),
        as.character(nrow(calsim3_nodes)),
        as.character(sf::st_crs(calsim3_arcs)$epsg),
        as.character(sf::st_crs(calsim3_nodes)$epsg)
      )
    ),
    arc_type_counts,
    node_descr_counts
  ) |>
    dplyr::mutate(run_timestamp = RUN_TS)

  readr::write_csv(qa, out_qa)

  message("Saved QA CSV: ", out_qa)
  print(qa, n = Inf)
}

# ==== 14. Final summary ======================================================

message("\nDone: CalSim3 arcs/nodes preprocessing complete.")
message("Arc rows saved: ", nrow(calsim3_arcs))
message("Node rows saved: ", nrow(calsim3_nodes))
message("Latest arc RDS:")
message("  ", out_arcs_rds_latest)
message("Latest node RDS:")
message("  ", out_nodes_rds_latest)
