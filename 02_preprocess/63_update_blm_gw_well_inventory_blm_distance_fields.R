# ==== 63_update_blm_gw_well_inventory_blm_distance_fields.R ================
##
## PURPOSE:
##   Precompute BLM-CA proximity fields for the two small Local groundwater-well
##   inventory layers added from BLM source data:
##
##     1. BLM-drilled wells | NOC database
##     2. GW wells | 2025 Mojave-BLM field check
##
## WHY:
##   These layers are small (~419 records after 048a normalization), but their
##   on/off-BLM status should still be calculated from BRIM's current BLM
##   managed-lands geometry rather than from any source-provided allotment,
##   historic BLM-land overlay, or contractor field assumption.
##
##   Keeping this as a sidecar preprocessor matches the Springs, SWRCB POD,
##   CNRFC, and Local USGS groundwater distance patterns.  When BLM managed
##   lands change, rerun this script and rebuild the map; normal final HTML
##   builds should only join the compact CSV sidecar.
##
## CANONICAL CURRENT-BLM DISTANCE SOURCE:
##   04_processed_data/rds/blm_managed_core_3310.rds
##
##   IMPORTANT: on_blm_ca and dist_to_blm_* are calculated ONLY from this
##   current BRIM managed-lands RDS.  The NOC and 2025 Mojave-BLM records are
##   treated the same way: BRIM projects the normalized point coordinates to
##   EPSG:3310 and compares them to the current BLM core lands geometry.
##
## INPUTS:
##   04_processed_data/rds/blm_gw_well_inventory_combined_wgs84.rds
##   04_processed_data/rds/blm_managed_core_3310.rds
##
## OPTIONAL OVERRIDES:
##   BLM_GW_WELL_INV_SOURCE_RDS             alternate normalized combined RDS
##   BLM_GW_WELL_INV_BLM_LANDS_RDS          alternate BLM managed-lands RDS
##   BLM_GW_WELL_INV_DISTANCE_CACHE_DIR     alternate cache output directory
##   BLM_GW_WELL_INV_DISTANCE_QA_DIR        alternate QA output directory
##
## OUTPUTS:
##   Main join/cache output:
##     04_processed_data/cache/latest/blm_gw_well_inventory_blm_distance_fields.csv
##
##   QA / audit outputs:
##     04_processed_data/qa/blm_gw_well_inventory_blm_distance_summary_latest.csv
##     04_processed_data/qa/blm_gw_well_inventory_blm_distance_by_source_latest.csv
##     04_processed_data/qa/blm_gw_well_inventory_blm_distance_bins_by_source_latest.csv
##     04_processed_data/qa/blm_gw_well_inventory_blm_distance_preview_latest.csv
##
## HOW TO RUN FROM PORTATREASURE2 ROOT:
##   source("02_preprocess/63_update_blm_gw_well_inventory_blm_distance_fields.R")
##
## PREFERRED RUNNER HELPERS:
##   source("run_build_map.r")
##   update_blm_gw_well_inventory_blm_distances()
##   refresh_blm_gw_well_inventory_and_map()
## ============================================================================

required_pkgs <- c("dplyr", "readr", "tibble", "sf")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(sf)
})

if (file.exists("00_config/config_paths.r")) {
  source("00_config/config_paths.r")
}


# ==== 1. Paths and run metadata ==============================================

pt_first_existing_path_63 <- function(paths) {
  paths <- as.character(paths)
  paths <- paths[!is.na(paths) & nzchar(paths)]
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

well_inventory_rds <- Sys.getenv(
  "BLM_GW_WELL_INV_SOURCE_RDS",
  unset = pt_first_existing_path_63(c(
    file.path("04_processed_data", "rds", "blm_gw_well_inventory_combined_wgs84.rds")
  ))
)

blm_lands_rds <- Sys.getenv(
  "BLM_GW_WELL_INV_BLM_LANDS_RDS",
  unset = file.path("04_processed_data", "rds", "blm_managed_core_3310.rds")
)

cache_dir <- Sys.getenv(
  "BLM_GW_WELL_INV_DISTANCE_CACHE_DIR",
  unset = file.path("04_processed_data", "cache", "latest")
)

qa_dir <- Sys.getenv(
  "BLM_GW_WELL_INV_DISTANCE_QA_DIR",
  unset = file.path("04_processed_data", "qa")
)

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

target_crs <- 3310
run_start <- Sys.time()
run_time <- format(run_start, "%Y-%m-%d %H:%M:%S %Z")

out_distance_csv <- file.path(cache_dir, "blm_gw_well_inventory_blm_distance_fields.csv")
out_summary_csv <- file.path(qa_dir, "blm_gw_well_inventory_blm_distance_summary_latest.csv")
out_by_source_csv <- file.path(qa_dir, "blm_gw_well_inventory_blm_distance_by_source_latest.csv")
out_bins_csv <- file.path(qa_dir, "blm_gw_well_inventory_blm_distance_bins_by_source_latest.csv")
out_preview_csv <- file.path(qa_dir, "blm_gw_well_inventory_blm_distance_preview_latest.csv")


# ==== 2. Small helpers ========================================================

pt_log_63 <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " | ", ...)
}

pt_elapsed_63 <- function(start) {
  secs <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  if (is.na(secs)) return("unknown")
  if (secs < 60) return(paste0(round(secs, 1), " sec"))
  paste0(floor(secs / 60), " min ", round(secs %% 60, 1), " sec")
}

pt_fmt_time_63 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  as.character(file.info(path)$mtime[[1]])
}

pt_bool_chr_63 <- function(x) {
  ifelse(is.na(x), NA_character_, ifelse(x, "true", "false"))
}

pt_clean_chr_63 <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NaN", "NULL", "null", "undefined")] <- NA_character_
  x
}

pt_first_col_63 <- function(df, candidates, default = NA_character_) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(default, nrow(df)))
  pt_clean_chr_63(df[[hit[[1]]]])
}

pt_bbox_text_63 <- function(x) {
  bb <- sf::st_bbox(x)
  paste(
    paste0(names(bb), "=", format(round(as.numeric(bb), 2), scientific = FALSE)),
    collapse = "; "
  )
}

pt_distance_label_63 <- function(on_blm, dist_mi) {
  dplyr::case_when(
    is.na(dist_mi) ~ NA_character_,
    isTRUE(on_blm) ~ "on BLM",
    dist_mi < 0.01 ~ "<0.01 mi",
    dist_mi < 10 ~ paste0(formatC(dist_mi, format = "f", digits = 2), " mi"),
    TRUE ~ paste0(formatC(dist_mi, format = "f", digits = 1), " mi")
  )
}

pt_dist_bin_63 <- function(on_blm, dist_mi) {
  dplyr::case_when(
    is.na(dist_mi) ~ "missing",
    on_blm ~ "on BLM",
    dist_mi <= 1 ~ "off BLM, <=1 mi",
    dist_mi <= 5 ~ "off BLM, >1 to <=5 mi",
    TRUE ~ "off BLM, >5 mi"
  )
}


# ==== 3. Read inputs and validate minimum fields =============================

if (is.na(well_inventory_rds) || !file.exists(well_inventory_rds)) {
  stop(
    "Missing normalized BLM groundwater-well inventory RDS. Expected: ",
    file.path("04_processed_data", "rds", "blm_gw_well_inventory_combined_wgs84.rds"),
    "\nRun source(\"02_preprocess/18_blm_groundwater_well_inventory.r\") first.",
    call. = FALSE
  )
}

if (!file.exists(blm_lands_rds)) {
  stop("Missing current BLM managed-lands RDS: ", blm_lands_rds, call. = FALSE)
}

pt_log_63("Reading normalized BLM groundwater-well inventory: ", well_inventory_rds)
wells <- readRDS(well_inventory_rds)

if (!inherits(wells, "sf")) {
  stop("Normalized BLM groundwater-well inventory RDS is not an sf object: ", well_inventory_rds, call. = FALSE)
}

required_cols <- c("record_uid", "source_key", "source_display", "well_name_display")
missing_cols <- setdiff(required_cols, names(wells))
if (length(missing_cols) > 0) {
  stop(
    "Normalized well inventory is missing expected field(s): ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

# Drop empty geometries defensively.  048a should already have removed bad
# coordinates, but this keeps the distance script safe if sources change later.
wells <- wells[!sf::st_is_empty(wells), ]

pt_log_63("Reading CURRENT BLM managed lands: ", blm_lands_rds)
blm <- readRDS(blm_lands_rds)

if (!inherits(blm, "sf")) {
  stop("BLM managed-lands RDS is not an sf object: ", blm_lands_rds, call. = FALSE)
}


# ==== 4. Project geometries and compute current-BLM on/off ===================

step_start <- Sys.time()
pt_log_63("Step 1/4: projecting wells and BLM lands to EPSG:", target_crs, "...")

# BRIM's BLM core is normally already EPSG:3310, but transforming explicitly is
# cheap for this small point set and protects against future source drift.
wells_3310 <- sf::st_transform(wells, target_crs)
blm_3310 <- sf::st_transform(blm, target_crs)

# Make BLM geometry valid at feature level.  Do NOT dissolve to one giant
# multipolygon; feature-level geometry lets GEOS use spatial indexing for the
# intersection and nearest-feature lookups.
blm_3310 <- suppressWarnings(sf::st_make_valid(blm_3310))

pt_log_63("  Well records: ", nrow(wells_3310))
pt_log_63("  BLM features: ", nrow(blm_3310))
pt_log_63("  Well bbox EPSG:3310: ", pt_bbox_text_63(wells_3310))
pt_log_63("Step 1/4 complete (", pt_elapsed_63(step_start), ").")

step_start <- Sys.time()
pt_log_63("Step 2/4: testing intersections with CURRENT BLM-CA managed lands...")

on_blm <- lengths(sf::st_intersects(wells_3310, blm_3310, sparse = TRUE)) > 0
off_idx <- which(!on_blm)

pt_log_63("  On BLM: ", sum(on_blm, na.rm = TRUE))
pt_log_63("  Off BLM: ", length(off_idx))
pt_log_63("Step 2/4 complete (", pt_elapsed_63(step_start), ").")


# ==== 5. Compute nearest-feature distance for off-BLM records ================

step_start <- Sys.time()
pt_log_63("Step 3/4: calculating distance to nearest CURRENT BLM feature for off-BLM records...")

dist_m <- rep(0, nrow(wells_3310))

if (length(off_idx) > 0) {
  nearest_idx <- sf::st_nearest_feature(wells_3310[off_idx, ], blm_3310)
  dist_m[off_idx] <- as.numeric(sf::st_distance(
    wells_3310[off_idx, ],
    blm_3310[nearest_idx, ],
    by_element = TRUE
  ))
}

dist_ft <- dist_m * 3.280839895
dist_mi <- dist_ft / 5280

pt_log_63("Step 3/4 complete (", pt_elapsed_63(step_start), ").")


# ==== 6. Build compact cache sidecar and QA outputs ==========================

step_start <- Sys.time()
pt_log_63("Step 4/4: writing cache and QA outputs...")

attrs <- sf::st_drop_geometry(wells)

out <- tibble::tibble(
  record_uid = attrs$record_uid,
  source_key = attrs$source_key,
  source_display = attrs$source_display,
  layer_name = pt_first_col_63(attrs, "layer_name"),
  well_name_display = attrs$well_name_display,
  longitude = suppressWarnings(as.numeric(pt_first_col_63(attrs, "longitude"))),
  latitude = suppressWarnings(as.numeric(pt_first_col_63(attrs, "latitude"))),
  on_blm_ca = on_blm,
  on_blm_ca_chr = pt_bool_chr_63(on_blm),
  dist_to_blm_m = dist_m,
  dist_to_blm_ft = dist_ft,
  dist_to_blm_mi = dist_mi,
  blm_distance_label = vapply(seq_along(dist_mi), function(i) {
    pt_distance_label_63(on_blm[[i]], dist_mi[[i]])
  }, character(1)),
  blm_distance_bin = pt_dist_bin_63(on_blm, dist_mi),
  blm_distance_method = "Current BRIM BLM managed-lands RDS; EPSG:3310; on-BLM via st_intersects; off-BLM via st_nearest_feature/st_distance.",
  blm_distance_run_time = run_time,
  input_well_inventory_rds = well_inventory_rds,
  input_well_inventory_mtime = pt_fmt_time_63(well_inventory_rds),
  input_blm_lands_rds = blm_lands_rds,
  input_blm_lands_mtime = pt_fmt_time_63(blm_lands_rds)
)

summary_qa <- tibble::tibble(
  run_time = run_time,
  well_inventory_rds = well_inventory_rds,
  well_inventory_mtime = pt_fmt_time_63(well_inventory_rds),
  blm_lands_rds = blm_lands_rds,
  blm_lands_mtime = pt_fmt_time_63(blm_lands_rds),
  on_blm_ca_definition = "Calculated by BRIM from current blm_managed_core_3310.rds; not copied from source data.",
  total_records = nrow(out),
  rows_with_distance = sum(!is.na(out$dist_to_blm_mi)),
  on_blm = sum(out$on_blm_ca, na.rm = TRUE),
  off_blm = sum(!out$on_blm_ca, na.rm = TRUE),
  within_1_mi = sum(out$dist_to_blm_mi <= 1, na.rm = TRUE),
  within_5_mi = sum(out$dist_to_blm_mi <= 5, na.rm = TRUE),
  max_distance_mi = max(out$dist_to_blm_mi, na.rm = TRUE),
  elapsed = pt_elapsed_63(run_start)
)

# Keep the QA wrangling future-friendly for current tidyselect/dplyr.
# Older versions of this script used `.data$field` inside grouping/selecting
# positions.  That works today, but tidyselect 1.2.0 deprecates `.data` in
# those positions.  Use ordinary bare columns for dplyr data-masking verbs and
# `all_of()` for the explicit select list below.
by_source_qa <- out |>
  dplyr::group_by(source_key, source_display, layer_name) |>
  dplyr::summarise(
    records = dplyr::n(),
    rows_with_distance = sum(!is.na(dist_to_blm_mi)),
    on_blm = sum(on_blm_ca, na.rm = TRUE),
    off_blm = sum(!on_blm_ca, na.rm = TRUE),
    within_1_mi = sum(dist_to_blm_mi <= 1, na.rm = TRUE),
    within_5_mi = sum(dist_to_blm_mi <= 5, na.rm = TRUE),
    max_distance_mi = max(dist_to_blm_mi, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(source_key)

bins_qa <- out |>
  dplyr::count(
    source_key,
    source_display,
    layer_name,
    blm_distance_bin,
    name = "n"
  ) |>
  dplyr::arrange(source_key, blm_distance_bin)

preview_cols <- c(
  "record_uid", "source_key", "source_display",
  "well_name_display", "longitude", "latitude",
  "on_blm_ca", "dist_to_blm_mi", "blm_distance_label",
  "blm_distance_bin"
)

preview_qa <- out |>
  dplyr::arrange(dplyr::desc(dist_to_blm_mi)) |>
  dplyr::select(dplyr::all_of(preview_cols)) |>
  utils::head(50)

readr::write_csv(out, out_distance_csv, na = "")
readr::write_csv(summary_qa, out_summary_csv, na = "")
readr::write_csv(by_source_qa, out_by_source_csv, na = "")
readr::write_csv(bins_qa, out_bins_csv, na = "")
readr::write_csv(preview_qa, out_preview_csv, na = "")

pt_log_63("Wrote BLM GW well inventory distance cache: ", out_distance_csv)
pt_log_63("Wrote BLM GW well inventory summary QA: ", out_summary_csv)
pt_log_63("Wrote BLM GW well inventory by-source QA: ", out_by_source_csv)
pt_log_63("Wrote BLM GW well inventory distance-bin QA: ", out_bins_csv)
pt_log_63("Wrote BLM GW well inventory preview QA: ", out_preview_csv)
pt_log_63("Step 4/4 complete (", pt_elapsed_63(step_start), ").")

pt_log_63("")
pt_log_63("Done: BLM GW well inventory BLM distance fields complete.")
pt_log_63(
  "  ", nrow(out), " well record(s) written; ",
  sum(!is.na(out$dist_to_blm_mi)), " rows with distance; ",
  sum(out$on_blm_ca, na.rm = TRUE), " on BLM; ",
  sum(out$dist_to_blm_mi <= 1, na.rm = TRUE), " within 1 mi; ",
  sum(out$dist_to_blm_mi <= 5, na.rm = TRUE), " within 5 mi."
)
