# ==== 62_update_springs_blm_distance_fields.R ===============================
##
## PURPOSE:
##   Precompute BLM-CA proximity fields for the Local Springs layer.
##
## WHY:
##   The Springs layer is a dense Local point layer (~27k records in the current
##   cache).  BLM managed-lands geometry changes over time through acquisitions,
##   disposals, corrections, and administrative updates.  Distance calculations
##   are spatially expensive and should not run during build_final_map_only().
##
##   This script writes a compact CSV sidecar keyed by spring_id.  The visible
##   map/legend can later join that sidecar and expose standard BRIM filters such
##   as:
##     BLM max mi: any / on / off / <=1 / <=5
##
## DESIGN NOTES:
##   - Mirrors the same general maintenance pattern used by:
##       59_update_cnrfc_local_blm_distance_fields.R
##       60_update_usgs_gw_local_blm_distance_fields.R
##       61_update_swrcb_pod_blm_distance_fields.R
##   - Reads the normalized Springs source object from 17_springs.r.
##   - Does NOT deduplicate co-located spring records.  Source provenance is
##     preserved and the map can aggregate hover/popup display later.
##   - Keeps elevation availability in QA only for now.  NHD currently has no
##     usable elevation field in the local input, so an elevation filter should
##     not be exposed until elevation coverage is broader or clearly scoped.
##
## CANONICAL CURRENT-BLM DISTANCE SOURCE:
##   04_processed_data/rds/blm_managed_core_3310.rds
##
##   IMPORTANT: on_blm_ca and dist_to_blm_* are calculated ONLY from this
##   current BRIM BLM managed-lands geometry. They are NOT copied from NHD, the
##   2015-16 Mojave survey, or any older source-specific BLM lands overlay.
##   Survey records and NHD records are treated the same way: BRIM projects their
##   coordinates to EPSG:3310 and compares them to the current BLM core lands RDS.
##
##   Refresh that RDS through the BLM/core preprocessing workflow whenever BLM
##   managed-lands geometry changes, then rerun this script so the Springs Local
##   filters use the updated boundary.
##
## INPUTS:
##   04_processed_data/rds/springs_combined_wgs84.rds
##   04_processed_data/rds/blm_managed_core_3310.rds
##
## OPTIONAL OVERRIDES:
##   SPRINGS_BLM_SOURCE_RDS             alternate normalized Springs RDS
##   SPRINGS_BLM_LANDS_RDS              alternate BLM managed-lands RDS
##   SPRINGS_BLM_DISTANCE_CACHE_DIR     alternate cache output directory
##   SPRINGS_BLM_DISTANCE_QA_DIR        alternate QA output directory
##
## OUTPUTS:
##   Main join/cache output:
##     04_processed_data/cache/latest/springs_blm_distance_fields.csv
##
##   QA / audit outputs:
##     04_processed_data/qa/springs_blm_distance_summary_latest.csv
##     04_processed_data/qa/springs_blm_distance_by_source_latest.csv
##     04_processed_data/qa/springs_blm_distance_bins_by_source_latest.csv
##     04_processed_data/qa/springs_blm_distance_preview_latest.csv
##     04_processed_data/qa/springs_elevation_by_source_latest.csv
##
## HOW TO RUN FROM PORTATREASURE2 ROOT:
##   source("02_preprocess/62_update_springs_blm_distance_fields.R")
##
## PREFERRED RUNNER HELPERS:
##   source("run_build_map.r")
##   update_springs_blm_distances()
##   refresh_springs_blm_distances_and_map()
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


# ==== 1. Paths and run metadata ==============================================

pt_first_existing_path_62 <- function(paths) {
  paths <- as.character(paths)
  paths <- paths[!is.na(paths) & nzchar(paths)]
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

springs_rds <- Sys.getenv(
  "SPRINGS_BLM_SOURCE_RDS",
  unset = pt_first_existing_path_62(c(
    file.path("04_processed_data", "rds", "springs_combined_wgs84.rds"),
    file.path("04_processed_data", "cache", "latest", "springs_map.rds")
  ))
)

blm_lands_rds <- Sys.getenv(
  "SPRINGS_BLM_LANDS_RDS",
  unset = file.path("04_processed_data", "rds", "blm_managed_core_3310.rds")
)

cache_dir <- Sys.getenv(
  "SPRINGS_BLM_DISTANCE_CACHE_DIR",
  unset = file.path("04_processed_data", "cache", "latest")
)

qa_dir <- Sys.getenv(
  "SPRINGS_BLM_DISTANCE_QA_DIR",
  unset = file.path("04_processed_data", "qa")
)

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

target_crs <- 3310
run_start <- Sys.time()
run_time <- format(run_start, "%Y-%m-%d %H:%M:%S %Z")


# ==== 2. Small helpers ========================================================

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || is.na(a) || !nzchar(as.character(a))) b else a
}

pt_num_62 <- function(x) suppressWarnings(as.numeric(gsub(",", "", as.character(x))))

pt_bool_chr_62 <- function(x) {
  ifelse(is.na(x), NA_character_, ifelse(x, "true", "false"))
}

pt_fmt_time_62 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  as.character(file.info(path)$mtime[[1]])
}

pt_elapsed_62 <- function(start) {
  secs <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  if (is.na(secs)) return("unknown")
  if (secs < 60) return(paste0(round(secs, 1), " sec"))
  paste0(floor(secs / 60), " min ", round(secs %% 60, 1), " sec")
}

pt_log_62 <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " | ", ...)
}

pt_bbox_text_62 <- function(x) {
  bb <- sf::st_bbox(x)
  paste(
    paste0(names(bb), "=", format(round(as.numeric(bb), 2), scientific = FALSE)),
    collapse = "; "
  )
}

pt_clean_chr_62 <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NaN", "NULL", "null", "undefined")] <- NA_character_
  x
}

pt_first_col_62 <- function(df, candidates, default = NA_character_) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(default, nrow(df)))
  pt_clean_chr_62(df[[hit[[1]]]])
}

pt_source_key_62 <- function(df) {
  raw_key <- if ("spring_source_key" %in% names(df)) {
    tolower(trimws(as.character(df$spring_source_key)))
  } else {
    rep(NA_character_, nrow(df))
  }

  raw_display <- if ("spring_source_display" %in% names(df)) {
    as.character(df$spring_source_display)
  } else if ("spring_source" %in% names(df)) {
    as.character(df$spring_source)
  } else {
    rep(NA_character_, nrow(df))
  }

  combo <- tolower(paste(raw_key, raw_display))

  dplyr::case_when(
    raw_key %in% c("nhd") | grepl("\\bnhd\\b", combo) ~ "nhd",
    raw_key %in% c("survey_2015_16", "mojave_2015_16") |
      grepl("2015|2016|2015.?16|mojave|zdon", combo) ~ "survey_2015_16",
    TRUE ~ "other"
  )
}

pt_source_display_62 <- function(df, key) {
  display <- if ("spring_source_display" %in% names(df)) {
    pt_clean_chr_62(df$spring_source_display)
  } else if ("spring_source" %in% names(df)) {
    pt_clean_chr_62(df$spring_source)
  } else {
    rep(NA_character_, nrow(df))
  }

  dplyr::case_when(
    key == "nhd" ~ "NHD",
    key == "survey_2015_16" ~ "2015–16 Mojave survey",
    !is.na(display) ~ display,
    TRUE ~ "Unknown source"
  )
}

pt_as_blm_sf_62 <- function(x) {
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
    pt_log_62("Transforming BLM managed-lands geometry to EPSG:", target_crs, ".")
    x <- sf::st_transform(x, target_crs)
  }

  x
}

pt_latlon_from_object_62 <- function(x) {
  n <- if (is.data.frame(x)) nrow(x) else 0L
  out <- tibble(lon = rep(NA_real_, n), lat = rep(NA_real_, n))
  if (n == 0) return(out)

  df <- as.data.frame(x, stringsAsFactors = FALSE)

  lon_candidates <- c("pt_lng", "pt_lon", "lon", "longitude", "lng", "x", "X")
  lat_candidates <- c("pt_lat", "lat", "latitude", "y", "Y")

  lon_hit <- lon_candidates[lon_candidates %in% names(df)]
  lat_hit <- lat_candidates[lat_candidates %in% names(df)]

  if (length(lon_hit) > 0 && length(lat_hit) > 0) {
    out$lon <- pt_num_62(df[[lon_hit[[1]]]])
    out$lat <- pt_num_62(df[[lat_hit[[1]]]])
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
      out$lon <- pt_num_62(coords$X)
      out$lat <- pt_num_62(coords$Y)
    }
  }

  out
}

pt_make_points_sf_62 <- function(x) {
  ll <- pt_latlon_from_object_62(x)
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


# ==== 3. Read inputs ==========================================================

if (is.na(springs_rds) || !nzchar(springs_rds) || !file.exists(springs_rds)) {
  stop(
    "Springs source RDS not found. Checked default RDS/cache locations.\n",
    "Expected 04_processed_data/rds/springs_combined_wgs84.rds.\n",
    "Run 17_springs.r first, or set SPRINGS_BLM_SOURCE_RDS."
  )
}

if (!file.exists(blm_lands_rds)) {
  stop(
    "Missing BLM managed-lands RDS: ", blm_lands_rds, "\n",
    "Refresh BLM/core preprocessing first, or set SPRINGS_BLM_LANDS_RDS."
  )
}

pt_log_62("Reading normalized Springs source: ", springs_rds)
springs <- readRDS(springs_rds)
if (!inherits(springs, "sf") && !is.data.frame(springs)) {
  stop("Springs source is not an sf object or data frame: ", springs_rds)
}

springs_df <- as.data.frame(springs, stringsAsFactors = FALSE)
pt_log_62("Springs rows: ", format(nrow(springs_df), big.mark = ","))

## Keep one stable ID per spring source record.  The normalized 17_springs.r
## script writes spring_id, but this fallback keeps the distance updater usable
## against older caches during transition.
if (!"spring_id" %in% names(springs_df)) {
  springs_df$spring_id <- paste0("spring_", sprintf("%06d", seq_len(nrow(springs_df))))
}
spring_id <- pt_clean_chr_62(springs_df$spring_id)

source_key <- pt_source_key_62(springs_df)
source_display <- pt_source_display_62(springs_df, source_key)

spring_name_display <- pt_first_col_62(
  springs_df,
  c("spring_name_display", "spring_name", "gnis_name", "name"),
  default = NA_character_
)

gnis_id <- pt_first_col_62(springs_df, c("gnis_id", "gnisid"), default = NA_character_)
elevation_ft <- if ("elevation_ft" %in% names(springs_df)) pt_num_62(springs_df$elevation_ft) else rep(NA_real_, nrow(springs_df))
elevation_known <- !is.na(elevation_ft)

pt_log_62("Source split: NHD=", sum(source_key == "nhd", na.rm = TRUE),
          "; 2015–16 survey=", sum(source_key == "survey_2015_16", na.rm = TRUE),
          "; other=", sum(source_key == "other", na.rm = TRUE), ".")

pt_log_62("Reading canonical BLM-CA managed lands RDS for Springs distance: ", blm_lands_rds)
blm <- readRDS(blm_lands_rds)
blm <- pt_as_blm_sf_62(blm)

pt_log_62("BLM distance source path: ", blm_lands_rds)
pt_log_62("BLM distance source modified: ", pt_fmt_time_62(blm_lands_rds) %||% "not available")
pt_log_62("BLM geometry rows/features available for current-BLM spatial indexing: ", nrow(blm))
pt_log_62("BLM geometry CRS EPSG: ", sf::st_crs(blm)$epsg)
pt_log_62("BLM geometry bbox: ", pt_bbox_text_62(blm))


# ==== 4. Prepare BLM geometry and spring points ==============================
##
## PERFORMANCE NOTE:
##   The first 047b version dissolved all BLM managed-land polygons into one
##   very complex multipart geometry, then asked GEOS to calculate the distance
##   from every Springs point to that single geometry.  That mirrors some older
##   BRIM preprocessors, but it can become painfully slow for a dense statewide
##   source such as NHD Springs (~27k records).
##
##   For Springs, keep the BLM polygons as feature-level geometries.  sf/GEOS can
##   then use its spatial index to:
##     1. test whether each point intersects any CURRENT BRIM BLM polygon; and
##     2. find the nearest CURRENT BRIM BLM feature only for points that are not
##        on BLM.
##
##   This is deliberately different from trusting any source-provided on/off BLM
##   attribute.  Some source inventories were compiled against older BLM lands
##   files or for different project purposes.  This updater recomputes the
##   screening relationship against BRIM's current BLM managed-lands cache every
##   time it is run.
##
##   The exact distance calculation is also chunked.  The first 047b version had
##   one opaque long-running distance step; chunking gives progress messages,
##   limits memory pressure, and makes it obvious which phase is slow if a future
##   BLM lands refresh produces unusually complex geometry.

## Drop any empty BLM geometries before spatial indexing.  Empty geometries are
## uncommon in the canonical BLM core cache, but guarding here keeps this updater
## safe against future land-cache refreshes or partial test fixtures.
blm <- blm[!sf::st_is_empty(blm), , drop = FALSE]
if (nrow(blm) == 0) {
  stop("BLM managed-lands object has no non-empty geometries after cleanup: ", blm_lands_rds)
}

blm_geom <- sf::st_geometry(blm)
if (is.na(sf::st_crs(blm_geom))) {
  sf::st_crs(blm_geom) <- sf::st_crs(blm)
}

## pt_as_blm_sf_62() should already have transformed to EPSG:3310, but keep the
## defensive check here because this script is meant to be rerun frequently when
## the BLM lands cache changes.
if (is.na(sf::st_crs(blm_geom)$epsg) || sf::st_crs(blm_geom)$epsg != target_crs) {
  blm <- sf::st_transform(blm, target_crs)
  blm_geom <- sf::st_geometry(blm)
}

pts_info <- pt_make_points_sf_62(springs)
pts <- pts_info$points
ll <- pts_info$latlon


# ==== 5. Calculate distance fields ===========================================
##
## on_blm_ca is written as character "true" / "false" rather than logical to
## match the conservative CSV sidecar style already used by other Local dense
## point layers.  Browser code can safely compare strings without R/JS logical
## conversion surprises.

out <- tibble(
  spring_id = spring_id,
  spring_source_key = source_key,
  spring_source_display = source_display,
  spring_name_display = spring_name_display,
  gnis_id = gnis_id,
  elevation_ft = elevation_ft,
  elevation_known = elevation_known,
  has_usable_coordinates = pts_info$ok,
  lat = ll$lat,
  lon = ll$lon,
  on_blm_ca = NA_character_,
  dist_to_blm_mi = NA_real_,
  dist_to_blm_ft = NA_real_,
  blm_distance_run_time = run_time,
  blm_source_rds = blm_lands_rds,
  blm_source_modified = pt_fmt_time_62(blm_lands_rds),
  springs_source_rds = springs_rds,
  springs_source_modified = pt_fmt_time_62(springs_rds)
)

if (!inherits(pts, "sf") || nrow(pts) == 0) {
  warning("No Springs rows had usable coordinates; writing empty distance fields.")
} else {
  pt_log_62("Rows with usable coordinates: ", format(nrow(pts), big.mark = ","),
            " of ", format(nrow(springs_df), big.mark = ","), ".")

  step <- Sys.time()
  pt_log_62("Step 1/4: testing Springs intersections with BLM-CA managed lands...")

  ## This returns a sparse list of BLM feature hits for each point.  A point with
  ## one or more hits is on BLM and gets distance 0 immediately; no expensive
  ## distance calculation is needed for those rows.
  inside <- lengths(sf::st_intersects(pts, blm_geom, sparse = TRUE)) > 0

  pt_log_62("Step 1/4 complete: ", format(sum(inside, na.rm = TRUE), big.mark = ","),
            " Springs records on BLM (", pt_elapsed_62(step), ").")

  outside_idx <- which(!inside)
  dist_m <- rep(NA_real_, nrow(pts))
  dist_m[inside] <- 0

  step <- Sys.time()
  pt_log_62("Step 2/4: finding nearest CURRENT BLM feature for off-BLM Springs records...")

  ## Only off-BLM records need nearest-feature lookup or non-zero distance.
  ## 'inside' above was calculated from st_intersects(pts, current BLM geometry),
  ## so this is BRIM/current-BLM state, not an old source inventory flag.
  if (length(outside_idx) == 0) {
    nearest_outside <- integer(0)
    pt_log_62("Step 2/4 complete: all usable Springs records are on current BLM; no nearest-feature search needed (",
              pt_elapsed_62(step), ").")
  } else {
    nearest_outside <- sf::st_nearest_feature(pts[outside_idx, , drop = FALSE], blm)
    pt_log_62("Step 2/4 complete for ", format(length(outside_idx), big.mark = ","),
              " off-BLM records (", pt_elapsed_62(step), ").")
  }

  step <- Sys.time()
  pt_log_62("Step 3/4: calculating projected distances for off-BLM records in EPSG:",
            target_crs, " using chunked nearest-feature pairs...")

  if (length(outside_idx) == 0) {
    pt_log_62("Step 3/4 complete: skipped because all usable Springs records are on current BLM (",
              pt_elapsed_62(step), ").")
  } else {
    ## Calculate element-wise distances from each off-BLM point to its nearest
    ## CURRENT BLM feature.  Chunking is not mathematically different from one
    ## vectorized st_distance(..., by_element = TRUE) call, but it is safer for
    ## dense Local layers because it keeps memory bounded and prints progress
    ## during long runs.
    chunk_size <- suppressWarnings(as.integer(Sys.getenv(
      "SPRINGS_BLM_DISTANCE_CHUNK_SIZE",
      unset = "2500"
    )))
    if (is.na(chunk_size) || chunk_size < 100L) chunk_size <- 2500L

    outside_pos <- seq_along(outside_idx)
    chunks <- split(outside_pos, ceiling(outside_pos / chunk_size))

    for (chunk_i in seq_along(chunks)) {
      pos <- chunks[[chunk_i]]
      idx <- outside_idx[pos]
      nearest_i <- nearest_outside[pos]

      chunk_start <- Sys.time()
      pt_log_62(
        "  Step 3/4 chunk ", chunk_i, "/", length(chunks),
        ": ", format(length(idx), big.mark = ","),
        " off-BLM records..."
      )

      dist_m[idx] <- as.numeric(sf::st_distance(
        sf::st_geometry(pts[idx, , drop = FALSE]),
        sf::st_geometry(blm)[nearest_i],
        by_element = TRUE
      ))

      pt_log_62(
        "  Step 3/4 chunk ", chunk_i, "/", length(chunks),
        " complete (", pt_elapsed_62(chunk_start), ")."
      )
    }

    pt_log_62("Step 3/4 complete for ", format(length(outside_idx), big.mark = ","),
              " off-BLM records (", pt_elapsed_62(step), ").")
  }

  out$on_blm_ca[pts$row_id] <- pt_bool_chr_62(inside)
  out$dist_to_blm_mi[pts$row_id] <- round(dist_m / 1609.344, 3)
  out$dist_to_blm_ft[pts$row_id] <- round(dist_m * 3.280839895, 0)
}

## Keep one row per spring_id for safe joins during final map build.  Duplicate
## IDs should not occur from 17_springs.r, but distinct() makes the sidecar safe
## if an older transitional cache is used.
out_join <- out |>
  filter(!is.na(.data$spring_id), .data$spring_id != "") |>
  distinct(spring_id, .keep_all = TRUE)


# ==== 6. Write cache and QA outputs ==========================================

step <- Sys.time()
pt_log_62("Step 4/4: writing cache and QA outputs...")

out_path <- file.path(cache_dir, "springs_blm_distance_fields.csv")
readr::write_csv(out_join, out_path)

summary_tbl <- tibble(
  metric = c(
    "springs_rows",
    "unique_spring_id_rows_written",
    "nhd_source_rows",
    "survey_2015_16_source_rows",
    "other_source_rows",
    "named_records",
    "records_with_gnis_id",
    "records_with_elevation_ft",
    "rows_with_usable_coordinates",
    "rows_with_distance",
    "on_BLM_CA_managed_lands",
    "within_1_mile_of_BLM_CA_managed_lands",
    "within_5_miles_of_BLM_CA_managed_lands",
    "within_10_miles_of_BLM_CA_managed_lands",
    "missing_BLM_distance",
    "springs_source_rds",
    "springs_source_modified",
    "BLM_distance_source_rds",
    "BLM_distance_source_modified",
    "on_blm_ca_definition",
    "BLM_source_EPSG",
    "BLM_source_feature_rows",
    "BLM_source_geometry_type",
    "BLM_source_bbox"
  ),
  value = c(
    as.character(nrow(springs_df)),
    as.character(nrow(out_join)),
    as.character(sum(source_key == "nhd", na.rm = TRUE)),
    as.character(sum(source_key == "survey_2015_16", na.rm = TRUE)),
    as.character(sum(source_key == "other", na.rm = TRUE)),
    as.character(sum(!is.na(spring_name_display))),
    as.character(sum(!is.na(gnis_id))),
    as.character(sum(elevation_known, na.rm = TRUE)),
    as.character(if (inherits(pts, "sf")) nrow(pts) else 0L),
    as.character(sum(!is.na(out_join$dist_to_blm_mi))),
    as.character(sum(tolower(as.character(out_join$on_blm_ca)) == "true", na.rm = TRUE)),
    as.character(sum(out_join$dist_to_blm_mi <= 1, na.rm = TRUE)),
    as.character(sum(out_join$dist_to_blm_mi <= 5, na.rm = TRUE)),
    as.character(sum(out_join$dist_to_blm_mi <= 10, na.rm = TRUE)),
    as.character(sum(is.na(out_join$dist_to_blm_mi))),
    springs_rds,
    pt_fmt_time_62(springs_rds) %||% NA_character_,
    blm_lands_rds,
    pt_fmt_time_62(blm_lands_rds) %||% NA_character_,
    "Computed by BRIM from current BLM managed-lands RDS using point-in-polygon; not copied from source inventory",
    as.character(sf::st_crs(blm)$epsg),
    as.character(nrow(blm)),
    paste(unique(as.character(sf::st_geometry_type(blm))), collapse = ", "),
    pt_bbox_text_62(blm)
  )
)

source_summary_tbl <- out_join |>
  mutate(
    on_blm_flag = tolower(as.character(.data$on_blm_ca)) == "true",
    has_dist = !is.na(.data$dist_to_blm_mi)
  ) |>
  group_by(.data$spring_source_key, .data$spring_source_display) |>
  summarise(
    records = n(),
    named_records = sum(!is.na(.data$spring_name_display)),
    gnis_id_records = sum(!is.na(.data$gnis_id)),
    elevation_records = sum(.data$elevation_known, na.rm = TRUE),
    rows_with_distance = sum(.data$has_dist),
    on_blm = sum(.data$on_blm_flag, na.rm = TRUE),
    within_1_mi = sum(.data$dist_to_blm_mi <= 1, na.rm = TRUE),
    within_5_mi = sum(.data$dist_to_blm_mi <= 5, na.rm = TRUE),
    within_10_mi = sum(.data$dist_to_blm_mi <= 10, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(.data$spring_source_key)

distance_bins_by_source_tbl <- out_join |>
  mutate(
    distance_bin = dplyr::case_when(
      is.na(.data$dist_to_blm_mi) ~ "missing",
      .data$dist_to_blm_mi == 0 ~ "on BLM / 0 mi",
      .data$dist_to_blm_mi <= 1 ~ ">0-1 mi",
      .data$dist_to_blm_mi <= 5 ~ ">1-5 mi",
      .data$dist_to_blm_mi <= 10 ~ ">5-10 mi",
      TRUE ~ ">10 mi"
    )
  ) |>
  group_by(.data$spring_source_key, .data$spring_source_display, .data$distance_bin) |>
  summarise(records = n(), .groups = "drop") |>
  arrange(.data$spring_source_key, .data$distance_bin)

preview_tbl <- out_join |>
  mutate(dist_sort = suppressWarnings(as.numeric(.data$dist_to_blm_mi))) |>
  arrange(.data$dist_sort) |>
  select(
    spring_id,
    spring_source_key,
    spring_source_display,
    spring_name_display,
    gnis_id,
    elevation_ft,
    elevation_known,
    has_usable_coordinates,
    lat,
    lon,
    on_blm_ca,
    dist_to_blm_mi,
    dist_to_blm_ft,
    blm_distance_run_time,
    blm_source_rds,
    blm_source_modified,
    springs_source_rds,
    springs_source_modified
  ) |>
  head(100)

elevation_by_source_tbl <- out_join |>
  group_by(.data$spring_source_key, .data$spring_source_display) |>
  summarise(
    records = n(),
    elevation_nonmissing = sum(!is.na(.data$elevation_ft)),
    elevation_min_ft = suppressWarnings(ifelse(
      all(is.na(.data$elevation_ft)),
      NA_real_,
      min(.data$elevation_ft, na.rm = TRUE)
    )),
    elevation_max_ft = suppressWarnings(ifelse(
      all(is.na(.data$elevation_ft)),
      NA_real_,
      max(.data$elevation_ft, na.rm = TRUE)
    )),
    .groups = "drop"
  ) |>
  arrange(.data$spring_source_key)

summary_path <- file.path(qa_dir, "springs_blm_distance_summary_latest.csv")
source_summary_path <- file.path(qa_dir, "springs_blm_distance_by_source_latest.csv")
distance_bins_path <- file.path(qa_dir, "springs_blm_distance_bins_by_source_latest.csv")
preview_path <- file.path(qa_dir, "springs_blm_distance_preview_latest.csv")
elevation_path <- file.path(qa_dir, "springs_elevation_by_source_latest.csv")

readr::write_csv(summary_tbl, summary_path)
readr::write_csv(source_summary_tbl, source_summary_path)
readr::write_csv(distance_bins_by_source_tbl, distance_bins_path)
readr::write_csv(preview_tbl, preview_path)
readr::write_csv(elevation_by_source_tbl, elevation_path)

pt_log_62("Wrote Springs BLM distance cache: ", out_path)
pt_log_62("Wrote Springs BLM distance summary QA: ", summary_path)
pt_log_62("Wrote Springs BLM distance by-source QA: ", source_summary_path)
pt_log_62("Wrote Springs BLM distance bin QA: ", distance_bins_path)
pt_log_62("Wrote Springs BLM distance preview QA: ", preview_path)
pt_log_62("Wrote Springs elevation by-source QA: ", elevation_path)
pt_log_62("Step 4/4 complete (", pt_elapsed_62(step), ").")

message("\nDone: Springs BLM distance fields complete.")
message("  ", format(nrow(out_join), big.mark = ","), " spring record(s) written; ",
        format(sum(!is.na(out_join$dist_to_blm_mi)), big.mark = ","), " rows with distance; ",
        format(sum(tolower(as.character(out_join$on_blm_ca)) == "true", na.rm = TRUE), big.mark = ","), " on BLM; ",
        format(sum(out_join$dist_to_blm_mi <= 1, na.rm = TRUE), big.mark = ","), " within 1 mi; ",
        format(sum(out_join$dist_to_blm_mi <= 5, na.rm = TRUE), big.mark = ","), " within 5 mi.")
