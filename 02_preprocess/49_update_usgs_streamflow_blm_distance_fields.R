# ==== 49_update_usgs_streamflow_blm_distance_fields.R ========================
##
## PURPOSE:
##   Add/refresh approximate precomputed BLM-CA relationship fields for the
##   BRIM Ops Live USGS streamflow station index WITHOUT fetching USGS data.
##
## DESIGN:
##   This mirrors the groundwater BLM-distance updater pattern, but is scoped to
##   the USGS streamflow live-feed station index. Keep this lightweight and
##   separate from the heavier streamgage-index export so distance math can be
##   refreshed independently after the BLM/core map preprocessing workflow.
##
## CANONICAL DISTANCE SOURCE:
##   04_processed_data/rds/blm_managed_core_3310.rds
##
##   This is the dissolved projected BLM-CA managed-lands layer used for
##   screening distance math. It should be maintained by the BLM/core map
##   preprocessing workflow and kept aligned with the visible BLM-CA Managed
##   map layer.
##
## INPUTS:
##   brim-live-data-feeds/data/input/usgs_streamgages_index_ca.csv
##   04_processed_data/rds/blm_managed_core_3310.rds
##
## OPTIONAL OVERRIDES:
##   USGS_STREAMFLOW_LIVE_INDEX_CSV       station-index CSV to update
##   USGS_STREAMFLOW_LIVE_QA_DIR          QA output folder
##   USGS_STREAMFLOW_BLM_LANDS_RDS        alternate processed BLM managed-lands RDS
##
## OUTPUTS:
##   Updates the station-index CSV in place with:
##     on_blm_ca
##     dist_to_blm_mi
##     dist_to_blm_ft
##
## DISTANCE NOTE:
##   Distances are approximate screening distances to BLM-CA managed lands.
##   Geometry is calculated in EPSG:3310 (California Albers), but results still
##   depend on source boundary date, gage coordinate precision, and source-scale
##   geometry.
##
## QA OUTPUTS:
##   04_processed_data/qa/usgs_streamflow_blm_distance_summary.csv
##   04_processed_data/qa/usgs_streamflow_blm_distance_preview.csv
##
## HOW TO RUN FROM PORTATREASURE2 ROOT:
##   source("02_preprocess/49_update_usgs_streamflow_blm_distance_fields.R")
## ============================================================================

required_pkgs <- c("dplyr", "readr", "tibble", "sf")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(sf)
})

# ---- Paths ------------------------------------------------------------------

out_input_csv <- Sys.getenv(
  "USGS_STREAMFLOW_LIVE_INDEX_CSV",
  unset = "brim-live-data-feeds/data/input/usgs_streamgages_index_ca.csv"
)

qa_dir <- Sys.getenv(
  "USGS_STREAMFLOW_LIVE_QA_DIR",
  unset = "04_processed_data/qa"
)

blm_lands_rds <- Sys.getenv(
  "USGS_STREAMFLOW_BLM_LANDS_RDS",
  unset = "04_processed_data/rds/blm_managed_core_3310.rds"
)

target_crs <- 3310

# ---- Helpers ----------------------------------------------------------------

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || is.na(a) || !nzchar(as.character(a))) b else a
}

pt_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

pt_bool_chr <- function(x) {
  ifelse(is.na(x), NA_character_, ifelse(x, "true", "false"))
}

pt_fmt_time <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  as.character(file.info(path)$mtime[[1]])
}

pt_bbox_text <- function(x) {
  bb <- sf::st_bbox(x)
  paste(
    paste0(names(bb), "=", format(round(as.numeric(bb), 2), scientific = FALSE)),
    collapse = "; "
  )
}

pt_as_blm_sf <- function(x, source_label) {
  if (inherits(x, "sfc")) {
    x <- sf::st_sf(source = source_label, geometry = x)
  }

  if (!inherits(x, "sf")) {
    stop("BLM managed-lands object is not an sf/sfc object. Source: ", source_label)
  }

  if (nrow(x) == 0) {
    stop("BLM managed-lands object has zero features. Source: ", source_label)
  }

  crs <- sf::st_crs(x)
  if (is.na(crs)) {
    stop("BLM managed-lands object has no CRS. Source: ", source_label)
  }

  x <- sf::st_make_valid(x)
  if (is.na(sf::st_crs(x)$epsg) || sf::st_crs(x)$epsg != target_crs) {
    message(
      "Transforming BLM managed-lands geometry from ",
      sf::st_crs(x)$input %||% paste0("EPSG:", sf::st_crs(x)$epsg),
      " to EPSG:", target_crs, "."
    )
    x <- sf::st_transform(x, target_crs)
  }

  x
}

pt_read_blm_managed <- function() {
  if (nzchar(blm_lands_rds) && file.exists(blm_lands_rds)) {
    message("Reading canonical BLM-CA managed lands RDS for streamflow distance: ", blm_lands_rds)
    x <- readRDS(blm_lands_rds)
    return(list(
      data = pt_as_blm_sf(x, blm_lands_rds),
      source_type = "processed_rds",
      source_path = blm_lands_rds,
      source_modified = pt_fmt_time(blm_lands_rds)
    ))
  }

  stop(
    "Canonical BLM-CA managed lands RDS not found: ", blm_lands_rds,
    "\nExpected the BLM/core map preprocessing workflow to create/update this file.",
    "\nNo shapefile fallback is used for the streamflow Ops Live distance fields; ",
    "set USGS_STREAMFLOW_BLM_LANDS_RDS only to another vetted processed EPSG:3310 RDS."
  )
}

# ---- Read streamflow station index -----------------------------------------

if (!file.exists(out_input_csv)) {
  stop(
    "USGS streamflow live station-index CSV not found: ", out_input_csv,
    "\nRun 02_preprocess/29_export_usgs_streamflow_live_inputs.R first, or point ",
    "USGS_STREAMFLOW_LIVE_INDEX_CSV to the station-index CSV to update."
  )
}

message("Reading USGS streamflow live station-index CSV: ", out_input_csv)
sfidx <- readr::read_csv(
  out_input_csv,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

needed <- c("latitude", "longitude")
if (!all(needed %in% names(sfidx))) {
  stop("Streamflow station-index CSV is missing required coordinate column(s): ", paste(setdiff(needed, names(sfidx)), collapse = ", "))
}

sfidx <- sfidx |>
  mutate(
    latitude_num = pt_num(.data$latitude),
    longitude_num = pt_num(.data$longitude)
  )

coord_ok <- !is.na(sfidx$latitude_num) & !is.na(sfidx$longitude_num)
if (!any(coord_ok)) {
  stop("Streamflow station-index CSV has no rows with usable latitude/longitude.")
}

message("Streamflow station-index rows: ", nrow(sfidx))
message("Rows with usable coordinates: ", sum(coord_ok))

# ---- Read and prepare BLM managed lands ------------------------------------

blm_src <- pt_read_blm_managed()
blm_3310 <- blm_src$data

message("BLM distance source type: ", blm_src$source_type)
message("BLM distance source path: ", blm_src$source_path)
message("BLM distance source modified: ", blm_src$source_modified %||% "not available")
message("BLM geometry rows/features before final dissolve: ", nrow(blm_3310))
message("BLM geometry CRS EPSG: ", sf::st_crs(blm_3310)$epsg)
message("BLM geometry bbox: ", pt_bbox_text(blm_3310))

blm_geom <- sf::st_geometry(blm_3310)
if (length(blm_geom) > 1) {
  message("Dissolving BLM geometry records to one managed-lands geometry...")
  blm_geom <- sf::st_union(blm_geom)
}

if (!inherits(blm_geom, "sfc")) {
  blm_geom <- sf::st_sfc(blm_geom, crs = sf::st_crs(blm_3310))
}

if (is.na(sf::st_crs(blm_geom))) {
  sf::st_crs(blm_geom) <- sf::st_crs(blm_3310)
}

if (is.na(sf::st_crs(blm_geom)$epsg) || sf::st_crs(blm_geom)$epsg != target_crs) {
  blm_geom <- sf::st_transform(blm_geom, target_crs)
}

# ---- Build points and calculate relationship -------------------------------

pts <- sf::st_as_sf(
  sfidx[coord_ok, , drop = FALSE],
  coords = c("longitude_num", "latitude_num"),
  crs = 4326,
  remove = FALSE
) |>
  sf::st_transform(target_crs)

message("Calculating approximate distance to BLM-CA managed lands for ", nrow(pts), " streamflow point(s)...")

on_blm <- lengths(sf::st_intersects(pts, blm_geom)) > 0
dist_m <- as.numeric(sf::st_distance(pts, blm_geom))

dist_m[on_blm] <- 0

out_vals <- tibble::tibble(
  row_id = which(coord_ok),
  on_blm_ca = on_blm,
  dist_to_blm_mi = round(dist_m / 1609.344, 3),
  dist_to_blm_ft = round(dist_m * 3.280839895, 0)
)

# ---- Write updated CSV and QA ----------------------------------------------

sfidx$on_blm_ca <- NA_character_
sfidx$dist_to_blm_mi <- NA_real_
sfidx$dist_to_blm_ft <- NA_real_

sfidx$on_blm_ca[out_vals$row_id] <- pt_bool_chr(out_vals$on_blm_ca)
sfidx$dist_to_blm_mi[out_vals$row_id] <- out_vals$dist_to_blm_mi
sfidx$dist_to_blm_ft[out_vals$row_id] <- out_vals$dist_to_blm_ft

sfidx <- sfidx |>
  select(-any_of(c("latitude_num", "longitude_num")))

dir.create(dirname(out_input_csv), recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(sfidx, out_input_csv)

summary_tbl <- tibble::tibble(
  metric = c(
    "streamflow station-index rows",
    "rows with usable coordinates",
    "on BLM-CA managed lands",
    "within 1 mile of BLM-CA managed lands",
    "within 5 miles of BLM-CA managed lands",
    "within 10 miles of BLM-CA managed lands",
    "missing BLM distance",
    "BLM distance source type",
    "BLM distance source path",
    "BLM distance source modified",
    "BLM source EPSG",
    "BLM source feature rows",
    "BLM source geometry type",
    "BLM source bbox"
  ),
  value = c(
    as.character(nrow(sfidx)),
    as.character(sum(coord_ok)),
    as.character(sum(out_vals$on_blm_ca, na.rm = TRUE)),
    as.character(sum(out_vals$dist_to_blm_mi <= 1, na.rm = TRUE)),
    as.character(sum(out_vals$dist_to_blm_mi <= 5, na.rm = TRUE)),
    as.character(sum(out_vals$dist_to_blm_mi <= 10, na.rm = TRUE)),
    as.character(sum(is.na(sfidx$dist_to_blm_mi))),
    blm_src$source_type,
    blm_src$source_path,
    blm_src$source_modified %||% NA_character_,
    as.character(sf::st_crs(blm_3310)$epsg),
    as.character(nrow(blm_3310)),
    paste(unique(as.character(sf::st_geometry_type(blm_3310))), collapse = ", "),
    pt_bbox_text(blm_3310)
  )
)

preview_tbl <- sfidx |>
  mutate(dist_to_blm_mi_num = pt_num(.data$dist_to_blm_mi)) |>
  arrange(.data$dist_to_blm_mi_num) |>
  select(any_of(c(
    "site_no", "station_nm", "latitude", "longitude", "site_type", "status",
    "on_blm_ca", "dist_to_blm_mi", "dist_to_blm_ft"
  ))) |>
  head(100)

readr::write_csv(summary_tbl, file.path(qa_dir, "usgs_streamflow_blm_distance_summary.csv"))
readr::write_csv(preview_tbl, file.path(qa_dir, "usgs_streamflow_blm_distance_preview.csv"))

message("Saved updated streamflow live station-index CSV: ", out_input_csv)
message("Saved BLM distance QA summary: ", file.path(qa_dir, "usgs_streamflow_blm_distance_summary.csv"))
message("Saved BLM distance QA preview: ", file.path(qa_dir, "usgs_streamflow_blm_distance_preview.csv"))
message(
  "Streamflow BLM distance fields populated: ", sum(!is.na(sfidx$dist_to_blm_mi)),
  " rows; ", sum(out_vals$on_blm_ca, na.rm = TRUE), " on BLM; ",
  sum(out_vals$dist_to_blm_mi <= 1, na.rm = TRUE), " within 1 mi; ",
  sum(out_vals$dist_to_blm_mi <= 5, na.rm = TRUE), " within 5 mi."
)
print(summary_tbl)
