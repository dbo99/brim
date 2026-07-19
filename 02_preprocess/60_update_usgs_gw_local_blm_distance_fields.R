# ==== 60_update_usgs_gw_local_blm_distance_fields.R =========================
##
## PURPOSE:
##   Precompute BLM-CA proximity fields for the Local USGS groundwater catalog
##   layer only:
##     - Local layer: USGS Wells (~44.2k)
##
## WHY:
##   These fields support the Local USGS Wells legend/filter controls. Distance
##   calculations are spatially expensive and should not run during
##   build_final_map_only(). This script writes a compact CSV cache that the
##   final map build simply joins by site_no.
##
## IMPORTANT SCOPE NOTE:
##   This is the Local catalog counterpart to the older Ops Live script:
##     02_preprocess/48_update_usgs_gw_blm_distance_fields.R
##
##   48_ updates the Ops Live groundwater candidate index CSV used by the
##   current/recent groundwater feed. 60_ updates the full Local catalog cache
##   used by the static/cached USGS Wells layer. Keep these separate so each can
##   be refreshed independently and so the larger Local catalog distance product
##   does not slow down normal map builds.
##
## CANONICAL DISTANCE SOURCE:
##   04_processed_data/rds/blm_managed_core_3310.rds
##
##   Refresh that RDS through the BLM/core preprocessing workflow whenever BLM
##   managed-lands geometry changes, then rerun this script so Local USGS Wells
##   filters use the updated BLM boundary.
##
## INPUTS:
##   04_processed_data/cache/latest/usgs_wells_map.rds
##   04_processed_data/rds/blm_managed_core_3310.rds
##
## OPTIONAL OVERRIDES:
##   USGS_GW_LOCAL_MAP_RDS                 alternate Local USGS Wells map RDS
##   USGS_GW_LOCAL_BLM_LANDS_RDS           alternate BLM managed-lands RDS
##   USGS_GW_LOCAL_BLM_DISTANCE_CACHE_DIR  alternate cache output directory
##   USGS_GW_LOCAL_BLM_DISTANCE_QA_DIR     alternate QA output directory
##
## OUTPUTS:
##   04_processed_data/cache/latest/usgs_gw_local_blm_distance_fields.csv
##   04_processed_data/qa/usgs_gw_local_blm_distance_summary_latest.csv
##   04_processed_data/qa/usgs_gw_local_blm_distance_preview_latest.csv
##
## HOW TO RUN FROM PORTATREASURE2 ROOT:
##   source("02_preprocess/60_update_usgs_gw_local_blm_distance_fields.R")
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

if (file.exists("00_config/config_paths.r")) {
  source("00_config/config_paths.r")
}

# ---- Paths ------------------------------------------------------------------

pt_first_existing_path_60 <- function(paths) {
  paths <- as.character(paths)
  paths <- paths[!is.na(paths) & nzchar(paths)]
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

local_gw_rds <- Sys.getenv(
  "USGS_GW_LOCAL_MAP_RDS",
  unset = pt_first_existing_path_60(c(
    file.path("04_processed_data", "cache", "latest", "usgs_wells_map.rds"),
    file.path("04_processed_data", "rds", "usgs_wells_map.rds"),
    file.path("04_processed_data", "rds", "USGS_GW_final.rds")
  ))
)

blm_lands_rds <- Sys.getenv(
  "USGS_GW_LOCAL_BLM_LANDS_RDS",
  unset = file.path("04_processed_data", "rds", "blm_managed_core_3310.rds")
)

cache_dir <- Sys.getenv(
  "USGS_GW_LOCAL_BLM_DISTANCE_CACHE_DIR",
  unset = file.path("04_processed_data", "cache", "latest")
)

qa_dir <- Sys.getenv(
  "USGS_GW_LOCAL_BLM_DISTANCE_QA_DIR",
  unset = file.path("04_processed_data", "qa")
)

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

target_crs <- 3310
run_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

# ---- Helpers ----------------------------------------------------------------

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || is.na(a) || !nzchar(as.character(a))) b else a
}

pt_num_60 <- function(x) suppressWarnings(as.numeric(as.character(x)))

pt_bool_chr_60 <- function(x) {
  ifelse(is.na(x), NA_character_, ifelse(x, "true", "false"))
}

pt_fmt_time_60 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  as.character(file.info(path)$mtime[[1]])
}

pt_bbox_text_60 <- function(x) {
  bb <- sf::st_bbox(x)
  paste(
    paste0(names(bb), "=", format(round(as.numeric(bb), 2), scientific = FALSE)),
    collapse = "; "
  )
}

pt_clean_site_no_60 <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\.0$", "", x)
  x <- gsub("^USGS[-_: /]*", "", x, ignore.case = TRUE)
  x <- gsub("^NWIS[-_: /]*", "", x, ignore.case = TRUE)
  x <- gsub("\\s+", "", x)
  x[x %in% c("", "NA", "NaN", "NULL", "null", "undefined")] <- NA_character_
  x
}

pt_first_col_60 <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit[[1]]]])
}

pt_as_blm_sf_60 <- function(x) {
  if (inherits(x, "sfc")) {
    x <- sf::st_sf(source = "blm_managed_core_3310", geometry = x)
  }

  if (!inherits(x, "sf") || nrow(x) == 0) {
    stop("BLM managed-lands object is not a non-empty sf/sfc object: ", blm_lands_rds)
  }

  if (is.na(sf::st_crs(x))) {
    stop("BLM managed-lands object has no CRS: ", blm_lands_rds)
  }

  x <- sf::st_make_valid(x)
  if (is.na(sf::st_crs(x)$epsg) || sf::st_crs(x)$epsg != target_crs) {
    message("Transforming BLM managed-lands geometry to EPSG:", target_crs, ".")
    x <- sf::st_transform(x, target_crs)
  }

  x
}

pt_latlon_from_object_60 <- function(x) {
  n <- if (is.data.frame(x)) nrow(x) else 0L
  out <- tibble(lon = rep(NA_real_, n), lat = rep(NA_real_, n))
  if (n == 0) return(out)

  df <- as.data.frame(x, stringsAsFactors = FALSE)

  lon_candidates <- c(
    "pt_lng", "pt_lon", "lon", "longitude", "dec_long_va", "site_longitude",
    "gwpop_lon", "gwpop_longitude", "lng", "x"
  )
  lat_candidates <- c(
    "pt_lat", "lat", "latitude", "dec_lat_va", "site_latitude",
    "gwpop_lat", "gwpop_latitude", "y"
  )

  lon_hit <- lon_candidates[lon_candidates %in% names(df)]
  lat_hit <- lat_candidates[lat_candidates %in% names(df)]

  if (length(lon_hit) > 0 && length(lat_hit) > 0) {
    out$lon <- pt_num_60(df[[lon_hit[[1]]]])
    out$lat <- pt_num_60(df[[lat_hit[[1]]]])
    return(out)
  }

  if (inherits(x, "sf")) {
    coords <- tryCatch({
      g <- x
      if (is.na(sf::st_crs(g))) g <- sf::st_set_crs(g, 4326)
      g <- sf::st_transform(g, 4326)
      as.data.frame(sf::st_coordinates(g))
    }, error = function(e) NULL)

    if (is.data.frame(coords) && all(c("X", "Y") %in% names(coords)) && nrow(coords) == n) {
      out$lon <- pt_num_60(coords$X)
      out$lat <- pt_num_60(coords$Y)
    }
  }

  out
}

pt_make_points_sf_60 <- function(x) {
  ll <- pt_latlon_from_object_60(x)
  ok <- !is.na(ll$lon) & !is.na(ll$lat) &
    abs(ll$lat) <= 90 & abs(ll$lon) <= 180 &
    !(ll$lat == 0 & ll$lon == 0)

  if (!any(ok)) {
    return(list(points = NULL, ok = ok, latlon = ll))
  }

  pts_df <- data.frame(row_id = which(ok), lon = ll$lon[ok], lat = ll$lat[ok])
  pts <- sf::st_as_sf(pts_df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  pts <- sf::st_transform(pts, target_crs)

  list(points = pts, ok = ok, latlon = ll)
}

# ---- Main -------------------------------------------------------------------

if (is.na(local_gw_rds) || !nzchar(local_gw_rds) || !file.exists(local_gw_rds)) {
  stop(
    "Local USGS Wells map RDS not found. Checked default cache/RDS locations.\n",
    "Expected 04_processed_data/cache/latest/usgs_wells_map.rds.\n",
    "Run the core cache build first, or set USGS_GW_LOCAL_MAP_RDS."
  )
}

if (!file.exists(blm_lands_rds)) {
  stop(
    "Missing BLM managed-lands RDS: ", blm_lands_rds, "\n",
    "Refresh BLM/core preprocessing first, or set USGS_GW_LOCAL_BLM_LANDS_RDS."
  )
}

message("Reading Local USGS Wells source: ", local_gw_rds)
gw <- readRDS(local_gw_rds)
if (!inherits(gw, "sf") && !is.data.frame(gw)) {
  stop("Local USGS Wells source is not an sf object or data frame: ", local_gw_rds)
}

gw_df <- as.data.frame(gw, stringsAsFactors = FALSE)
site_no <- pt_first_col_60(gw_df, c("site_no", "gwpop_site_no", "monitoring_location_id", "site", "site_id"))
site_no <- pt_clean_site_no_60(site_no)

if (all(is.na(site_no))) {
  stop(
    "Could not find usable USGS site number field in Local GW object. Columns: ",
    paste(names(gw_df), collapse = ", ")
  )
}

message("Local USGS Wells rows: ", format(nrow(gw_df), big.mark = ","))

message("Reading canonical BLM-CA managed lands RDS for Local USGS GW distance: ", blm_lands_rds)
blm <- readRDS(blm_lands_rds)
blm <- pt_as_blm_sf_60(blm)

message("BLM distance source path: ", blm_lands_rds)
message("BLM distance source modified: ", pt_fmt_time_60(blm_lands_rds) %||% "not available")
message("BLM geometry rows/features before final dissolve: ", nrow(blm))
message("BLM geometry CRS EPSG: ", sf::st_crs(blm)$epsg)
message("BLM geometry bbox: ", pt_bbox_text_60(blm))

blm_geom <- sf::st_geometry(blm)
if (length(blm_geom) > 1) {
  message("Dissolving BLM geometry records to one managed-lands geometry...")
  blm_geom <- sf::st_union(blm_geom)
}

if (!inherits(blm_geom, "sfc")) {
  blm_geom <- sf::st_sfc(blm_geom, crs = sf::st_crs(blm))
}

if (is.na(sf::st_crs(blm_geom))) {
  sf::st_crs(blm_geom) <- sf::st_crs(blm)
}

if (is.na(sf::st_crs(blm_geom)$epsg) || sf::st_crs(blm_geom)$epsg != target_crs) {
  blm_geom <- sf::st_transform(blm_geom, target_crs)
}

pts_info <- pt_make_points_sf_60(gw)
pts <- pts_info$points
ll <- pts_info$latlon

out <- tibble(
  site_no = site_no,
  station_nm = pt_first_col_60(gw_df, c("station_nm", "gwpop_station_nm", "station_name", "name")),
  lat = ll$lat,
  lon = ll$lon,
  on_blm_ca = NA_character_,
  dist_to_blm_mi = NA_real_,
  dist_to_blm_ft = NA_real_,
  blm_distance_run_time = run_time,
  blm_source_rds = blm_lands_rds,
  blm_source_modified = pt_fmt_time_60(blm_lands_rds),
  local_gw_source_rds = local_gw_rds,
  local_gw_source_modified = pt_fmt_time_60(local_gw_rds)
)

if (!inherits(pts, "sf") || nrow(pts) == 0) {
  warning("No Local USGS Wells rows had usable coordinates; writing empty distance fields.")
} else {
  message("Rows with usable coordinates: ", format(nrow(pts), big.mark = ","))
  message("Calculating approximate distance to BLM-CA managed lands for Local USGS Wells...")

  inside <- lengths(sf::st_intersects(pts, blm_geom, sparse = TRUE)) > 0
  nearest <- sf::st_nearest_feature(pts, blm_geom)
  dist_m <- as.numeric(sf::st_distance(sf::st_geometry(pts), sf::st_geometry(blm_geom)[nearest], by_element = TRUE))
  dist_m[inside] <- 0

  out$on_blm_ca[pts$row_id] <- pt_bool_chr_60(inside)
  out$dist_to_blm_mi[pts$row_id] <- round(dist_m / 1609.344, 3)
  out$dist_to_blm_ft[pts$row_id] <- round(dist_m * 3.280839895, 0)
}

## Keep one row per site_no for safe joins during final map build.
## If the Local catalog ever contains duplicate site_no values, the first row is
## retained and QA still reports the source row count above.
out_join <- out |>
  filter(!is.na(.data$site_no), .data$site_no != "") |>
  distinct(site_no, .keep_all = TRUE)

out_path <- file.path(cache_dir, "usgs_gw_local_blm_distance_fields.csv")
readr::write_csv(out_join, out_path)

summary_tbl <- tibble(
  metric = c(
    "local_usgs_gw_rows",
    "unique_site_no_rows_written",
    "rows_with_usable_coordinates",
    "rows_with_distance",
    "on_BLM_CA_managed_lands",
    "within_1_mile_of_BLM_CA_managed_lands",
    "within_5_miles_of_BLM_CA_managed_lands",
    "within_10_miles_of_BLM_CA_managed_lands",
    "missing_BLM_distance",
    "local_gw_source_rds",
    "local_gw_source_modified",
    "BLM_distance_source_rds",
    "BLM_distance_source_modified",
    "BLM_source_EPSG",
    "BLM_source_feature_rows",
    "BLM_source_geometry_type",
    "BLM_source_bbox"
  ),
  value = c(
    as.character(nrow(gw_df)),
    as.character(nrow(out_join)),
    as.character(if (inherits(pts, "sf")) nrow(pts) else 0L),
    as.character(sum(!is.na(out_join$dist_to_blm_mi))),
    as.character(sum(tolower(as.character(out_join$on_blm_ca)) == "true", na.rm = TRUE)),
    as.character(sum(out_join$dist_to_blm_mi <= 1, na.rm = TRUE)),
    as.character(sum(out_join$dist_to_blm_mi <= 5, na.rm = TRUE)),
    as.character(sum(out_join$dist_to_blm_mi <= 10, na.rm = TRUE)),
    as.character(sum(is.na(out_join$dist_to_blm_mi))),
    local_gw_rds,
    pt_fmt_time_60(local_gw_rds) %||% NA_character_,
    blm_lands_rds,
    pt_fmt_time_60(blm_lands_rds) %||% NA_character_,
    as.character(sf::st_crs(blm)$epsg),
    as.character(nrow(blm)),
    paste(unique(as.character(sf::st_geometry_type(blm))), collapse = ", "),
    pt_bbox_text_60(blm)
  )
)

preview_tbl <- out_join |>
  mutate(dist_sort = suppressWarnings(as.numeric(.data$dist_to_blm_mi))) |>
  arrange(.data$dist_sort) |>
  select(site_no, station_nm, lat, lon, on_blm_ca, dist_to_blm_mi, dist_to_blm_ft) |>
  head(100)

summary_path <- file.path(qa_dir, "usgs_gw_local_blm_distance_summary_latest.csv")
preview_path <- file.path(qa_dir, "usgs_gw_local_blm_distance_preview_latest.csv")

readr::write_csv(summary_tbl, summary_path)
readr::write_csv(preview_tbl, preview_path)

message("Saved Local USGS GW BLM-distance cache: ", out_path)
message("Saved Local USGS GW BLM-distance QA summary: ", summary_path)
message("Saved Local USGS GW BLM-distance QA preview: ", preview_path)
message(
  "Local USGS GW BLM distance fields populated: ",
  format(sum(!is.na(out_join$dist_to_blm_mi)), big.mark = ","), " rows; ",
  format(sum(tolower(as.character(out_join$on_blm_ca)) == "true", na.rm = TRUE), big.mark = ","), " on BLM; ",
  format(sum(out_join$dist_to_blm_mi <= 1, na.rm = TRUE), big.mark = ","), " within 1 mi; ",
  format(sum(out_join$dist_to_blm_mi <= 5, na.rm = TRUE), big.mark = ","), " within 5 mi."
)
print(summary_tbl)
