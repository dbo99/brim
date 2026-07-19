# ==== 59_update_cnrfc_local_blm_distance_fields.R ===========================
##
## PURPOSE:
##   Precompute BLM-CA proximity fields for the Local CNRFC point catalogs:
##     - CNRFC river/reservoir catalog
##     - CNRFC weather station catalog
##
## WHY:
##   These fields support Local legend/filter controls.  Distance calculations
##   are spatially expensive and should not run during build_final_map_only().
##   This script writes compact CSV caches that the final map build simply joins.
##
## CANONICAL DISTANCE SOURCE:
##   04_processed_data/rds/blm_managed_core_3310.rds
##
##   Refresh that RDS through the BLM/core preprocessing workflow whenever BLM
##   managed-lands geometry changes, then rerun this script so the CNRFC Local
##   filters use the updated boundary.
##
## INPUTS:
##   04_processed_data/cache/latest/cnrfc_stream_map.rds
##   04_processed_data/cache/latest/cnrfc_precip_weather_station_catalog_local_map.rds
##   04_processed_data/rds/blm_managed_core_3310.rds
##
## OPTIONAL OVERRIDES:
##   CNRFC_STREAM_LOCAL_RDS             alternate river/reservoir catalog RDS
##   CNRFC_WEATHER_LOCAL_RDS            alternate weather catalog RDS
##   CNRFC_LOCAL_BLM_LANDS_RDS          alternate BLM managed-lands RDS
##   CNRFC_LOCAL_BLM_DISTANCE_CACHE_DIR alternate cache output directory
##   CNRFC_LOCAL_BLM_DISTANCE_QA_DIR    alternate QA output directory
##
## OUTPUTS:
##   04_processed_data/cache/latest/cnrfc_river_reservoir_blm_distance_fields.csv
##   04_processed_data/cache/latest/cnrfc_weather_station_blm_distance_fields.csv
##   04_processed_data/qa/cnrfc_local_blm_distance_summary_latest.csv
##   04_processed_data/qa/cnrfc_local_blm_distance_preview_latest.csv
##
## HOW TO RUN FROM PORTATREASURE2 ROOT:
##   source("02_preprocess/59_update_cnrfc_local_blm_distance_fields.R")
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

pt_first_existing_path_59 <- function(paths) {
  paths <- as.character(paths)
  paths <- paths[!is.na(paths) & nzchar(paths)]
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

stream_rds <- Sys.getenv(
  "CNRFC_STREAM_LOCAL_RDS",
  unset = pt_first_existing_path_59(c(
    file.path("04_processed_data", "cache", "latest", "cnrfc_stream_map.rds"),
    file.path("04_processed_data", "rds", "cnrfc_stream_map.rds")
  ))
)

weather_rds <- Sys.getenv(
  "CNRFC_WEATHER_LOCAL_RDS",
  unset = pt_first_existing_path_59(c(
    file.path("04_processed_data", "cache", "latest", "cnrfc_precip_weather_station_catalog_local_map.rds"),
    file.path("04_processed_data", "rds", "cnrfc_precip_weather_station_catalog_local_map.rds")
  ))
)

blm_lands_rds <- Sys.getenv(
  "CNRFC_LOCAL_BLM_LANDS_RDS",
  unset = file.path("04_processed_data", "rds", "blm_managed_core_3310.rds")
)

cache_dir <- Sys.getenv(
  "CNRFC_LOCAL_BLM_DISTANCE_CACHE_DIR",
  unset = file.path("04_processed_data", "cache", "latest")
)

qa_dir <- Sys.getenv(
  "CNRFC_LOCAL_BLM_DISTANCE_QA_DIR",
  unset = file.path("04_processed_data", "qa")
)

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

target_crs <- 3310
run_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

# ---- Helpers ----------------------------------------------------------------

pt_num_59 <- function(x) suppressWarnings(as.numeric(as.character(x)))

pt_bool_chr_59 <- function(x) {
  ifelse(is.na(x), NA_character_, ifelse(x, "true", "false"))
}

pt_fmt_time_59 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  as.character(file.info(path)$mtime[[1]])
}

pt_cnrfc_first_col_59 <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit[[1]]]])
}

pt_cnrfc_norm_key_part_59 <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("\\s+", "", x)
  x[x %in% c("", "NA", "NAN", "NULL", "UNDEFINED")] <- NA_character_
  x
}

pt_cnrfc_point_latlon_59 <- function(x, lon_col = NULL, lat_col = NULL) {
  n <- if (is.data.frame(x)) nrow(x) else 0L
  out <- tibble(lon = rep(NA_real_, n), lat = rep(NA_real_, n))
  if (n == 0) return(out)

  if (!is.null(lon_col) && !is.null(lat_col) && all(c(lon_col, lat_col) %in% names(x))) {
    out$lon <- pt_num_59(x[[lon_col]])
    out$lat <- pt_num_59(x[[lat_col]])
    return(out)
  }

  lon_candidates <- c("lon", "longitude", "dec_long_va", "site_longitude", "lng", "x")
  lat_candidates <- c("lat", "latitude", "dec_lat_va", "site_latitude", "y")
  lon_hit <- lon_candidates[lon_candidates %in% names(x)]
  lat_hit <- lat_candidates[lat_candidates %in% names(x)]

  if (length(lon_hit) > 0 && length(lat_hit) > 0) {
    out$lon <- pt_num_59(x[[lon_hit[[1]]]])
    out$lat <- pt_num_59(x[[lat_hit[[1]]]])
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
      out$lon <- pt_num_59(coords$X)
      out$lat <- pt_num_59(coords$Y)
    }
  }

  out
}

pt_cnrfc_blm_join_key_59 <- function(x, dataset = c("river_reservoir", "weather_station"),
                                     lon_col = NULL, lat_col = NULL) {
  dataset <- match.arg(dataset)
  df <- as.data.frame(x, stringsAsFactors = FALSE)
  ll <- pt_cnrfc_point_latlon_59(x, lon_col = lon_col, lat_col = lat_col)

  id <- if (dataset == "river_reservoir") {
    pt_cnrfc_first_col_59(df, c("nwsid", "cnrfc_id", "station_id", "id", "site_id"))
  } else {
    pt_cnrfc_first_col_59(df, c("cnrfc_id", "station_id", "nwsid", "site_id", "id"))
  }

  id <- pt_cnrfc_norm_key_part_59(id)
  id[is.na(id)] <- "NOID"

  lon_txt <- ifelse(is.na(ll$lon), "NA", sprintf("%.5f", ll$lon))
  lat_txt <- ifelse(is.na(ll$lat), "NA", sprintf("%.5f", ll$lat))

  paste(id, lat_txt, lon_txt, sep = "|")
}

pt_as_blm_sf_59 <- function(x) {
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

pt_make_points_sf_59 <- function(x, lon_col = NULL, lat_col = NULL) {
  ll <- pt_cnrfc_point_latlon_59(x, lon_col = lon_col, lat_col = lat_col)
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

pt_compute_distance_table_59 <- function(x, dataset = c("river_reservoir", "weather_station"),
                                         source_rds, blm, lon_col = NULL, lat_col = NULL) {
  dataset <- match.arg(dataset)
  df <- as.data.frame(x, stringsAsFactors = FALSE)
  ll <- pt_cnrfc_point_latlon_59(x, lon_col = lon_col, lat_col = lat_col)
  keys <- pt_cnrfc_blm_join_key_59(x, dataset = dataset, lon_col = lon_col, lat_col = lat_col)

  station_id <- if (dataset == "river_reservoir") {
    pt_cnrfc_first_col_59(df, c("nwsid", "cnrfc_id", "station_id", "id", "site_id"))
  } else {
    pt_cnrfc_first_col_59(df, c("cnrfc_id", "station_id", "nwsid", "site_id", "id"))
  }

  station_name <- if (dataset == "river_reservoir") {
    pt_cnrfc_first_col_59(df, c("nickname", "display_name", "station_name", "name"))
  } else {
    pt_cnrfc_first_col_59(df, c("display_name", "station_name", "name"))
  }

  elev_ft <- pt_cnrfc_first_col_59(df, c("elev_ft", "elevation_ft", "elevation"))

  out <- tibble(
    dataset = dataset,
    row_id = seq_len(nrow(df)),
    pt_cnrfc_blm_join_key = keys,
    station_id = station_id,
    station_name = station_name,
    lat = ll$lat,
    lon = ll$lon,
    elev_ft = pt_num_59(elev_ft),
    on_blm_ca = NA,
    dist_to_blm_mi = NA_real_,
    dist_to_blm_ft = NA_real_,
    blm_distance_run_time = run_time,
    blm_source_rds = blm_lands_rds,
    blm_source_modified = pt_fmt_time_59(blm_lands_rds),
    point_source_rds = source_rds,
    point_source_modified = pt_fmt_time_59(source_rds)
  )

  pts_info <- pt_make_points_sf_59(x, lon_col = lon_col, lat_col = lat_col)
  pts <- pts_info$points

  if (!inherits(pts, "sf") || nrow(pts) == 0) {
    return(out)
  }

  message("Computing BLM distances for ", dataset, ": ", format(nrow(pts), big.mark = ","), " points.")

  inside <- lengths(sf::st_intersects(pts, blm, sparse = TRUE)) > 0
  nearest <- sf::st_nearest_feature(pts, blm)
  dist_m <- as.numeric(sf::st_distance(sf::st_geometry(pts), sf::st_geometry(blm)[nearest], by_element = TRUE))
  dist_m[inside] <- 0

  out$on_blm_ca[pts$row_id] <- pt_bool_chr_59(inside)
  out$dist_to_blm_mi[pts$row_id] <- round(dist_m / 1609.344, 3)
  out$dist_to_blm_ft[pts$row_id] <- round(dist_m * 3.280839895, 0)

  out
}

pt_write_dataset_59 <- function(x, dataset, source_rds, out_file, blm, lon_col = NULL, lat_col = NULL) {
  if (is.na(source_rds) || !nzchar(source_rds) || !file.exists(source_rds)) {
    warning("Skipping ", dataset, ": source RDS not found.")
    return(NULL)
  }

  message("Reading ", dataset, " source: ", source_rds)
  obj <- readRDS(source_rds)
  dist_tbl <- pt_compute_distance_table_59(
    obj,
    dataset = dataset,
    source_rds = source_rds,
    blm = blm,
    lon_col = lon_col,
    lat_col = lat_col
  )

  readr::write_csv(dist_tbl, out_file)
  message(
    "Saved ", dataset, " BLM-distance cache: ", out_file,
    " (", format(sum(!is.na(dist_tbl$dist_to_blm_mi)), big.mark = ","), " rows with distance; ",
    format(sum(tolower(as.character(dist_tbl$on_blm_ca)) == "true", na.rm = TRUE), big.mark = ","), " on BLM)."
  )

  dist_tbl
}

# ---- Main -------------------------------------------------------------------

if (!file.exists(blm_lands_rds)) {
  stop("Missing BLM managed-lands RDS: ", blm_lands_rds)
}

message("Reading canonical BLM-CA managed lands RDS for CNRFC local distance: ", blm_lands_rds)
blm <- readRDS(blm_lands_rds)
blm <- pt_as_blm_sf_59(blm)

stream_out <- file.path(cache_dir, "cnrfc_river_reservoir_blm_distance_fields.csv")
weather_out <- file.path(cache_dir, "cnrfc_weather_station_blm_distance_fields.csv")

stream_tbl <- pt_write_dataset_59(
  x = NULL,
  dataset = "river_reservoir",
  source_rds = stream_rds,
  out_file = stream_out,
  blm = blm
)

weather_tbl <- pt_write_dataset_59(
  x = NULL,
  dataset = "weather_station",
  source_rds = weather_rds,
  out_file = weather_out,
  blm = blm,
  lon_col = "lon",
  lat_col = "lat"
)

all_tbl <- dplyr::bind_rows(stream_tbl, weather_tbl)

if (nrow(all_tbl) > 0) {
  summary_tbl <- all_tbl |>
    group_by(dataset) |>
    summarise(
      run_time = first(blm_distance_run_time),
      point_source_rds = first(point_source_rds),
      point_source_modified = first(point_source_modified),
      blm_source_rds = first(blm_source_rds),
      blm_source_modified = first(blm_source_modified),
      rows = dplyr::n(),
      rows_with_distance = sum(!is.na(dist_to_blm_mi)),
      on_blm = sum(tolower(as.character(on_blm_ca)) == "true", na.rm = TRUE),
      within_1_mi = sum(dist_to_blm_mi <= 1, na.rm = TRUE),
      within_5_mi = sum(dist_to_blm_mi <= 5, na.rm = TRUE),
      within_10_mi = sum(dist_to_blm_mi <= 10, na.rm = TRUE),
      missing_distance = sum(is.na(dist_to_blm_mi)),
      .groups = "drop"
    )

  preview_tbl <- all_tbl |>
    mutate(dist_sort = suppressWarnings(as.numeric(dist_to_blm_mi))) |>
    arrange(dataset, dist_sort) |>
    select(dataset, station_id, station_name, lat, lon, elev_ft, on_blm_ca, dist_to_blm_mi, dist_to_blm_ft) |>
    group_by(dataset) |>
    slice_head(n = 50) |>
    ungroup()

  summary_path <- file.path(qa_dir, "cnrfc_local_blm_distance_summary_latest.csv")
  preview_path <- file.path(qa_dir, "cnrfc_local_blm_distance_preview_latest.csv")

  readr::write_csv(summary_tbl, summary_path)
  readr::write_csv(preview_tbl, preview_path)

  message("Saved CNRFC local BLM-distance QA summary: ", summary_path)
  message("Saved CNRFC local BLM-distance QA preview: ", preview_path)
  print(summary_tbl)
} else {
  warning("No CNRFC local distance tables were produced.")
}
