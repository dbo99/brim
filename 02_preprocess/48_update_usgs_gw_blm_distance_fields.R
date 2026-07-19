# ==== 48_update_usgs_gw_blm_distance_fields.R (OPS LIVE ONLY) ================
##
## PURPOSE:
##   Add/refresh approximate precomputed BLM-CA relationship fields for the
##   BRIM Ops Live USGS groundwater candidate index WITHOUT fetching USGS data.
##
## SCOPE NOTE:
##   This script is for Ops Live only. The full Local USGS Wells catalog uses
##   02_preprocess/60_update_usgs_gw_local_blm_distance_fields.R instead.
##
## DESIGN:
##   The visible local map layer is "BLM-CA Managed".  For groundwater distance
##   filtering, use the same processed BLM managed-lands product family, but the
##   projected EPSG:3310 RDS rather than the WGS84 Leaflet display cache.
##
## CANONICAL DISTANCE SOURCE:
##   04_processed_data/rds/blm_managed_core_3310.rds
##
##   This file should be overwritten by the BLM/core map preprocessing workflow
##   whenever the active BLM-CA managed-lands source is updated.  That keeps the
##   groundwater distance fields aligned with the map's BLM-CA Managed layer.
##
## INPUTS:
##   brim-live-data-feeds/data/input/usgs_groundwater_latest_index_ca.csv
##   04_processed_data/rds/blm_managed_core_3310.rds
##
## OPTIONAL OVERRIDES:
##   USGS_GW_LIVE_INDEX_CSV       candidate CSV to update
##   USGS_GW_LIVE_QA_DIR          QA output folder
##   USGS_GW_BLM_LANDS_RDS        alternate processed BLM managed-lands RDS
##   USGS_GW_BLM_LANDS_SHP        explicit shapefile override; only used when
##                                USGS_GW_BLM_LANDS_RDS is missing or blank
##
## OUTPUTS:
##   Updates the candidate CSV in place with:
##     on_blm_ca
##     dist_to_blm_mi
##     dist_to_blm_ft
##
## DISTANCE NOTE:
##   Distances are approximate screening distances to BLM-CA managed lands.
##   Geometry is calculated in EPSG:3310 (California Albers), but results still
##   depend on source boundary date, well coordinate precision, and source-scale
##   geometry.
##
## QA OUTPUTS:
##   04_processed_data/qa/usgs_groundwater_blm_distance_summary.csv
##   04_processed_data/qa/usgs_groundwater_blm_distance_preview.csv
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
  "USGS_GW_LIVE_INDEX_CSV",
  unset = "brim-live-data-feeds/data/input/usgs_groundwater_latest_index_ca.csv"
)

qa_dir <- Sys.getenv(
  "USGS_GW_LIVE_QA_DIR",
  unset = "04_processed_data/qa"
)

blm_lands_rds <- Sys.getenv(
  "USGS_GW_BLM_LANDS_RDS",
  unset = "04_processed_data/rds/blm_managed_core_3310.rds"
)

blm_lands_shp <- Sys.getenv("USGS_GW_BLM_LANDS_SHP", unset = "")

target_crs <- 3310

# ---- Helpers ----------------------------------------------------------------

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

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || is.na(a) || !nzchar(as.character(a))) b else a
}

pt_read_blm_managed <- function() {
  if (nzchar(blm_lands_rds) && file.exists(blm_lands_rds)) {
    message("Reading canonical BLM-CA managed lands RDS for distance: ", blm_lands_rds)
    x <- readRDS(blm_lands_rds)
    return(list(
      data = pt_as_blm_sf(x, blm_lands_rds),
      source_type = "processed_rds",
      source_path = blm_lands_rds,
      source_modified = pt_fmt_time(blm_lands_rds)
    ))
  }

  if (nzchar(blm_lands_shp) && file.exists(blm_lands_shp)) {
    warning(
      "Canonical BLM RDS was not found; using explicit USGS_GW_BLM_LANDS_SHP override. ",
      "For long-term robustness, prefer updating 04_processed_data/rds/blm_managed_core_3310.rds."
    )
    message("Reading explicit BLM-CA managed lands shapefile override: ", blm_lands_shp)
    x <- sf::st_read(blm_lands_shp, quiet = TRUE)
    return(list(
      data = pt_as_blm_sf(x, blm_lands_shp),
      source_type = "explicit_shapefile_override",
      source_path = blm_lands_shp,
      source_modified = pt_fmt_time(blm_lands_shp)
    ))
  }

  stop(
    "Canonical BLM-CA managed lands RDS not found: ", blm_lands_rds,
    "\nExpected the BLM/core map preprocessing workflow to create/update this file.",
    "\nSet USGS_GW_BLM_LANDS_RDS to another processed EPSG:3310 RDS, or set ",
    "USGS_GW_BLM_LANDS_SHP only as an explicit override."
  )
}

# ---- Read candidate index ---------------------------------------------------

if (!file.exists(out_input_csv)) {
  stop(
    "Groundwater live candidate CSV not found: ", out_input_csv,
    "\nRun 02_preprocess/30_export_usgs_groundwater_live_inputs.R first, or point ",
    "USGS_GW_LIVE_INDEX_CSV to the candidate CSV to update."
  )
}

message("Reading groundwater live candidate CSV: ", out_input_csv)
gw <- readr::read_csv(
  out_input_csv,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

needed <- c("latitude", "longitude")
if (!all(needed %in% names(gw))) {
  stop("Candidate CSV is missing required coordinate column(s): ", paste(setdiff(needed, names(gw)), collapse = ", "))
}

gw <- gw |>
  mutate(
    latitude_num = pt_num(.data$latitude),
    longitude_num = pt_num(.data$longitude)
  )

coord_ok <- !is.na(gw$latitude_num) & !is.na(gw$longitude_num)
if (!any(coord_ok)) {
  stop("Candidate CSV has no rows with usable latitude/longitude.")
}

message("Groundwater candidate rows: ", nrow(gw))
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

# The canonical RDS is expected to already be dissolved to one MULTIPOLYGON.
# Still collapse to a single sfc geometry defensively if a future source has
# multiple records. Keep the result as an sfc object; do not drop to a bare sfg
# geometry because sf::st_as_sfc() is not reliable for all sfg classes here.
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
  gw[coord_ok, , drop = FALSE],
  coords = c("longitude_num", "latitude_num"),
  crs = 4326,
  remove = FALSE
) |>
  sf::st_transform(target_crs)

message("Calculating approximate distance to BLM-CA managed lands for ", nrow(pts), " groundwater point(s)...")

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

gw$on_blm_ca <- NA_character_
gw$dist_to_blm_mi <- NA_real_
gw$dist_to_blm_ft <- NA_real_

gw$on_blm_ca[out_vals$row_id] <- pt_bool_chr(out_vals$on_blm_ca)
gw$dist_to_blm_mi[out_vals$row_id] <- out_vals$dist_to_blm_mi
gw$dist_to_blm_ft[out_vals$row_id] <- out_vals$dist_to_blm_ft

gw <- gw |>
  select(-any_of(c("latitude_num", "longitude_num")))

dir.create(dirname(out_input_csv), recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(gw, out_input_csv)

summary_tbl <- tibble::tibble(
  metric = c(
    "groundwater candidate rows",
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
    as.character(nrow(gw)),
    as.character(sum(coord_ok)),
    as.character(sum(out_vals$on_blm_ca, na.rm = TRUE)),
    as.character(sum(out_vals$dist_to_blm_mi <= 1, na.rm = TRUE)),
    as.character(sum(out_vals$dist_to_blm_mi <= 5, na.rm = TRUE)),
    as.character(sum(out_vals$dist_to_blm_mi <= 10, na.rm = TRUE)),
    as.character(sum(is.na(gw$dist_to_blm_mi))),
    blm_src$source_type,
    blm_src$source_path,
    blm_src$source_modified %||% NA_character_,
    as.character(sf::st_crs(blm_3310)$epsg),
    as.character(nrow(blm_3310)),
    paste(unique(as.character(sf::st_geometry_type(blm_3310))), collapse = ", "),
    pt_bbox_text(blm_3310)
  )
)

preview_tbl <- gw |>
  mutate(dist_to_blm_mi_num = pt_num(.data$dist_to_blm_mi)) |>
  arrange(.data$dist_to_blm_mi_num) |>
  select(any_of(c(
    "site_no", "station_nm", "latitude", "longitude", "latest_wl_ft_bgs",
    "latest_age_days", "on_blm_ca", "dist_to_blm_mi", "dist_to_blm_ft"
  ))) |>
  head(100)

readr::write_csv(summary_tbl, file.path(qa_dir, "usgs_groundwater_blm_distance_summary.csv"))
readr::write_csv(preview_tbl, file.path(qa_dir, "usgs_groundwater_blm_distance_preview.csv"))

message("Saved updated groundwater live candidate CSV: ", out_input_csv)
message("Saved BLM distance QA summary: ", file.path(qa_dir, "usgs_groundwater_blm_distance_summary.csv"))
message("Saved BLM distance QA preview: ", file.path(qa_dir, "usgs_groundwater_blm_distance_preview.csv"))
message(
  "Groundwater BLM distance fields populated: ", sum(!is.na(gw$dist_to_blm_mi)),
  " rows; ", sum(out_vals$on_blm_ca, na.rm = TRUE), " on BLM; ",
  sum(out_vals$dist_to_blm_mi <= 1, na.rm = TRUE), " within 1 mi; ",
  sum(out_vals$dist_to_blm_mi <= 5, na.rm = TRUE), " within 5 mi."
)
print(summary_tbl)
