# ==== 56_audit_cnrfc_river_reservoir_local_vs_ops.R =========================
##
## PURPOSE:
##   Quick, no-network QA audit to decide whether the legacy Local layer
##   "CNRFC River & Reservoir" still adds meaningful coverage beyond the newer
##   Ops Live CNRFC river/reservoir forecast-point layer.
##
## WHY:
##   The Local layer is useful only if it exposes additional fragmented / limited
##   time-series or catalog records near project areas. If it mostly duplicates
##   the Ops layer, it is a good candidate to hide from the main Local panel.
##
## INPUTS EXPECTED, WHEN PRESENT:
##   04_processed_data/cache/latest/cnrfc_stream_map.rds
##   04_processed_data/rds/cnrfc_active_river_reservoir_forecast_points_map.rds
##   04_processed_data/rds/cnrfc_river_reservoir_forecast_points.rds
##   04_processed_data/rds/cnrfc_river_reservoir_catalog_review.rds
##
## OUTPUTS:
##   04_processed_data/qa/cnrfc_river_reservoir_local_vs_ops_summary_latest.csv
##   04_processed_data/qa/cnrfc_river_reservoir_local_only_latest.csv
##   04_processed_data/qa/cnrfc_river_reservoir_ops_only_latest.csv
##   timestamped copies of the same outputs
## ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

if (file.exists("00_config/config_paths.r")) {
  source("00_config/config_paths.r")
}

pt_qa_dir <- if (exists("DIR") && !is.null(DIR$qa)) DIR$qa else file.path("04_processed_data", "qa")
pt_rds_dir <- if (exists("DIR") && !is.null(DIR$rds)) DIR$rds else file.path("04_processed_data", "rds")
dir.create(pt_qa_dir, recursive = TRUE, showWarnings = FALSE)

RUN_TS <- format(Sys.time(), "%Y%m%d_%H%M%S")

pt_first_existing_path <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

pt_read_rds_safe <- function(path, label) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    message(label, " not found; skipping: ", path)
    return(tibble())
  }
  message("Reading ", label, ": ", path)
  x <- readRDS(path)
  if (inherits(x, "sf")) {
    x <- sf::st_drop_geometry(x)
  }
  tibble::as_tibble(x)
}

pt_pick_col <- function(x, candidates) {
  hit <- candidates[candidates %in% names(x)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

pt_clean_id <- function(x) {
  x <- as.character(x)
  x <- trimws(toupper(x))
  x[x %in% c("", "NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_make_id_index <- function(x, source_label) {
  if (nrow(x) == 0) {
    return(tibble(source = character(), cnrfc_id = character(), display_name = character()))
  }
  id_col <- pt_pick_col(
    x,
    c(
      "cnrfc_id", "nwsid", "nws_id", "NWSID", "id", "station_id",
      "feature_id", "forecast_point_id", "location_id", "site_id"
    )
  )
  name_col <- pt_pick_col(
    x,
    c(
      "display_name", "name", "station_name", "feature_name", "description",
      "site_name", "river_name", "reservoir_name"
    )
  )
  if (is.na(id_col)) {
    warning(source_label, ": no likely ID column found. Columns: ", paste(names(x), collapse = ", "))
    return(tibble(source = source_label, cnrfc_id = character(), display_name = character()))
  }
  out <- tibble(
    source = source_label,
    cnrfc_id = pt_clean_id(x[[id_col]]),
    display_name = if (!is.na(name_col)) as.character(x[[name_col]]) else NA_character_
  ) %>%
    filter(!is.na(cnrfc_id)) %>%
    group_by(source, cnrfc_id) %>%
    summarise(
      display_name = dplyr::first(na.omit(display_name)),
      .groups = "drop"
    )
  out$display_name[is.na(out$display_name)] <- ""
  out
}

local_path <- pt_first_existing_path(c(
  file.path("04_processed_data", "cache", "latest", "cnrfc_stream_map.rds"),
  file.path(pt_rds_dir, "cnrfc_stream_map.rds")
))

ops_active_path <- pt_first_existing_path(c(
  file.path(pt_rds_dir, "cnrfc_active_river_reservoir_forecast_points_map.rds")
))

ops_all_path <- pt_first_existing_path(c(
  file.path(pt_rds_dir, "cnrfc_river_reservoir_forecast_points.rds")
))

catalog_review_path <- pt_first_existing_path(c(
  file.path(pt_rds_dir, "cnrfc_river_reservoir_catalog_review.rds")
))

local_idx <- pt_read_rds_safe(local_path, "Local CNRFC River & Reservoir layer") %>%
  pt_make_id_index("local_cnrfc_river_reservoir")

ops_active_idx <- pt_read_rds_safe(ops_active_path, "Ops active CNRFC river/reservoir map layer") %>%
  pt_make_id_index("ops_active_river_reservoir")

ops_all_idx <- pt_read_rds_safe(ops_all_path, "All CNRFC river/reservoir forecast-point records") %>%
  pt_make_id_index("river_reservoir_forecast_points_all")

catalog_review_idx <- pt_read_rds_safe(catalog_review_path, "CNRFC river/reservoir catalog-review records") %>%
  pt_make_id_index("river_reservoir_catalog_review")

local_ids <- unique(local_idx$cnrfc_id)
ops_active_ids <- unique(ops_active_idx$cnrfc_id)
ops_all_ids <- unique(ops_all_idx$cnrfc_id)
catalog_review_ids <- unique(catalog_review_idx$cnrfc_id)

local_only_vs_ops_active <- setdiff(local_ids, ops_active_ids)
ops_active_only_vs_local <- setdiff(ops_active_ids, local_ids)
local_overlap_ops_active <- intersect(local_ids, ops_active_ids)

summary <- tribble(
  ~run_timestamp, ~metric, ~value,
  RUN_TS, "local_cnrfc_river_reservoir_ids", length(local_ids),
  RUN_TS, "ops_active_river_reservoir_ids", length(ops_active_ids),
  RUN_TS, "ops_all_river_reservoir_forecast_point_ids", length(ops_all_ids),
  RUN_TS, "catalog_review_ids", length(catalog_review_ids),
  RUN_TS, "local_overlap_ops_active_ids", length(local_overlap_ops_active),
  RUN_TS, "local_only_vs_ops_active_ids", length(local_only_vs_ops_active),
  RUN_TS, "ops_active_only_vs_local_ids", length(ops_active_only_vs_local),
  RUN_TS, "local_only_also_in_ops_all_forecast_points", sum(local_only_vs_ops_active %in% ops_all_ids),
  RUN_TS, "local_only_also_in_catalog_review", sum(local_only_vs_ops_active %in% catalog_review_ids)
)

local_only <- local_idx %>%
  filter(cnrfc_id %in% local_only_vs_ops_active) %>%
  mutate(
    also_in_ops_all_forecast_points = cnrfc_id %in% ops_all_ids,
    also_in_catalog_review = cnrfc_id %in% catalog_review_ids
  ) %>%
  arrange(cnrfc_id)

ops_only <- ops_active_idx %>%
  filter(cnrfc_id %in% ops_active_only_vs_local) %>%
  arrange(cnrfc_id)

summary_latest <- file.path(pt_qa_dir, "cnrfc_river_reservoir_local_vs_ops_summary_latest.csv")
summary_ts <- file.path(pt_qa_dir, paste0("cnrfc_river_reservoir_local_vs_ops_summary_", RUN_TS, ".csv"))
local_only_latest <- file.path(pt_qa_dir, "cnrfc_river_reservoir_local_only_latest.csv")
local_only_ts <- file.path(pt_qa_dir, paste0("cnrfc_river_reservoir_local_only_", RUN_TS, ".csv"))
ops_only_latest <- file.path(pt_qa_dir, "cnrfc_river_reservoir_ops_only_latest.csv")
ops_only_ts <- file.path(pt_qa_dir, paste0("cnrfc_river_reservoir_ops_only_", RUN_TS, ".csv"))

readr::write_csv(summary, summary_latest)
readr::write_csv(summary, summary_ts)
readr::write_csv(local_only, local_only_latest)
readr::write_csv(local_only, local_only_ts)
readr::write_csv(ops_only, ops_only_latest)
readr::write_csv(ops_only, ops_only_ts)

message("CNRFC river/reservoir Local-vs-Ops audit complete.")
message("  Summary:    ", summary_latest)
message("  Local-only: ", local_only_latest)
message("  Ops-only:   ", ops_only_latest)
print(summary, n = Inf)
