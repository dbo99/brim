# ==== 61_update_swrcb_pod_blm_distance_fields.R =============================
##
## PURPOSE:
##   Precompute BLM-CA proximity fields for the Local SWRCB / CalWATRS
##   water-right point-of-diversion (POD) layers:
##     - SWRCB 2026 BLM WR list records
##     - SWRCB add'l PODs spatially matched to BLM
##     - SWRCB add'l BLM name/text-match candidates
##
## WHY:
##   These fields will support compact Local legend/filter controls such as
##   "on BLM" and "max miles to BLM".  BLM managed lands change through
##   acquisitions, disposals, corrections, and administrative updates, so this
##   script is intended to be rerun routinely after the canonical BLM lands RDS
##   is refreshed.  Distance calculations are intentionally kept out of
##   build_final_map_only() so normal HTML builds stay fast and predictable.
##
## DESIGN NOTES:
##   - This script mirrors the successful Local USGS GW distance-cache pattern
##     in 60_update_usgs_gw_local_blm_distance_fields.R.
##   - It writes a compact CSV sidecar keyed by pt_swrcb_layer_id.  The final
##     map build simply joins that sidecar to the SWRCB map-ready object.
##   - The three SWRCB POD layers remain separate in the UI for provenance.
##     This script calculates one shared distance cache because all three source
##     layers come from the same SWRCB/CalWATRS POD map-ready object.
##
## CANONICAL DISTANCE SOURCE:
##   04_processed_data/rds/blm_managed_core_3310.rds
##
##   Refresh that RDS through the BLM/core preprocessing workflow whenever BLM
##   managed-lands geometry changes, then rerun this script so SWRCB POD filters
##   use the updated BLM boundary.
##
## INPUTS:
##   04_processed_data/cache/latest/swrcb_pod_wr_blm_map.rds
##   04_processed_data/rds/blm_managed_core_3310.rds
##
## OPTIONAL OVERRIDES:
##   SWRCB_POD_BLM_MAP_RDS                 alternate SWRCB POD map-ready RDS
##   SWRCB_POD_BLM_LANDS_RDS               alternate BLM managed-lands RDS
##   SWRCB_POD_BLM_DISTANCE_CACHE_DIR      alternate cache output directory
##   SWRCB_POD_BLM_DISTANCE_QA_DIR         alternate QA output directory
##
## OUTPUTS:
##   Main join/cache output:
##     04_processed_data/cache/latest/swrcb_pod_blm_distance_fields.csv
##
##   QA / audit outputs:
##     04_processed_data/qa/swrcb_pod_blm_distance_summary_latest.csv
##     04_processed_data/qa/swrcb_pod_blm_distance_by_source_latest.csv
##     04_processed_data/qa/swrcb_pod_blm_distance_preview_latest.csv
##     04_processed_data/qa/swrcb_pod_source_classification_latest.csv
##     04_processed_data/qa/swrcb_pod_status_by_source_latest.csv
##     04_processed_data/qa/swrcb_pod_reason_by_source_latest.csv
##     04_processed_data/qa/swrcb_pod_distance_bins_by_source_latest.csv
##
##   The extra QA files are deliberately redundant.  They make it easy to see
##   whether a source-count change, such as name/text candidates changing from
##   an expected prior count, came from the current SWRCB map-ready object, the
##   source-classification rule, coordinate filtering, or the BLM distance join.
##
## HOW TO RUN FROM PORTATREASURE2 ROOT:
##   source("02_preprocess/61_update_swrcb_pod_blm_distance_fields.R")
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

pt_first_existing_path_61 <- function(paths) {
  paths <- as.character(paths)
  paths <- paths[!is.na(paths) & nzchar(paths)]
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

swrcb_pod_rds <- Sys.getenv(
  "SWRCB_POD_BLM_MAP_RDS",
  unset = pt_first_existing_path_61(c(
    file.path("04_processed_data", "cache", "latest", "swrcb_pod_wr_blm_map.rds"),
    file.path("04_processed_data", "rds", "swrcb_pod_wr_blm_map.rds")
  ))
)

blm_lands_rds <- Sys.getenv(
  "SWRCB_POD_BLM_LANDS_RDS",
  unset = file.path("04_processed_data", "rds", "blm_managed_core_3310.rds")
)

cache_dir <- Sys.getenv(
  "SWRCB_POD_BLM_DISTANCE_CACHE_DIR",
  unset = file.path("04_processed_data", "cache", "latest")
)

qa_dir <- Sys.getenv(
  "SWRCB_POD_BLM_DISTANCE_QA_DIR",
  unset = file.path("04_processed_data", "qa")
)

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

target_crs <- 3310
run_start <- Sys.time()
run_time <- format(run_start, "%Y-%m-%d %H:%M:%S %Z")

# ---- Small helpers ----------------------------------------------------------

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || is.na(a) || !nzchar(as.character(a))) b else a
}

pt_num_61 <- function(x) suppressWarnings(as.numeric(as.character(x)))

pt_boolish_61 <- function(x) {
  if (is.logical(x)) return(x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "y")
}

pt_bool_chr_61 <- function(x) {
  ifelse(is.na(x), NA_character_, ifelse(x, "true", "false"))
}

pt_fmt_time_61 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  as.character(file.info(path)$mtime[[1]])
}

pt_elapsed_61 <- function(start) {
  secs <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  if (is.na(secs)) return("unknown")
  if (secs < 60) return(paste0(round(secs, 1), " sec"))
  paste0(floor(secs / 60), " min ", round(secs %% 60, 1), " sec")
}

pt_log_61 <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " | ", ...)
}

pt_bbox_text_61 <- function(x) {
  bb <- sf::st_bbox(x)
  paste(
    paste0(names(bb), "=", format(round(as.numeric(bb), 2), scientific = FALSE)),
    collapse = "; "
  )
}

pt_first_col_61 <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit[[1]]]])
}

pt_clean_id_61 <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NaN", "NULL", "null", "undefined")] <- NA_character_
  x
}

pt_as_blm_sf_61 <- function(x) {
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
    pt_log_61("Transforming BLM managed-lands geometry to EPSG:", target_crs, ".")
    x <- sf::st_transform(x, target_crs)
  }

  x
}

pt_latlon_from_object_61 <- function(x) {
  n <- if (is.data.frame(x)) nrow(x) else 0L
  out <- tibble(lon = rep(NA_real_, n), lat = rep(NA_real_, n))
  if (n == 0) return(out)

  df <- as.data.frame(x, stringsAsFactors = FALSE)

  lon_candidates <- c(
    "pt_lng", "pt_lon", "lon", "longitude", "dec_long_va", "LONGITUDE",
    "x", "X", "lng"
  )
  lat_candidates <- c(
    "pt_lat", "lat", "latitude", "dec_lat_va", "LATITUDE",
    "y", "Y"
  )

  lon_hit <- lon_candidates[lon_candidates %in% names(df)]
  lat_hit <- lat_candidates[lat_candidates %in% names(df)]

  if (length(lon_hit) > 0 && length(lat_hit) > 0) {
    out$lon <- pt_num_61(df[[lon_hit[[1]]]])
    out$lat <- pt_num_61(df[[lat_hit[[1]]]])
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
      out$lon <- pt_num_61(coords$X)
      out$lat <- pt_num_61(coords$Y)
    }
  }

  out
}

pt_make_points_sf_61 <- function(x) {
  ll <- pt_latlon_from_object_61(x)
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

pt_source_key_61 <- function(df) {
  n <- nrow(df)
  reason <- if ("blm_include_reason_display" %in% names(df)) {
    trimws(as.character(df$blm_include_reason_display))
  } else {
    rep("Unknown", n)
  }

  list_flag <- if ("swrcb_2026_blm_wr_list" %in% names(df)) {
    pt_boolish_61(df$swrcb_2026_blm_wr_list)
  } else {
    rep(FALSE, n)
  }
  list_flag[is.na(list_flag)] <- FALSE

  dplyr::case_when(
    list_flag ~ "official",
    !list_flag & reason %in% c("Spatial + name match", "Spatial only") ~ "spatial",
    !list_flag & reason %in% c("Name match only") ~ "name_candidate",
    TRUE ~ "other"
  )
}

pt_status_key_61 <- function(v) {
  s <- tolower(trimws(as.character(v)))

  ## Match browser legend/filter semantics exactly.  The order matters because
  ## the string "Inactive / cancelled" contains the substring "active".
  dplyr::case_when(
    grepl("inactive|cancel", s) ~ "inactive",
    grepl("pending", s) ~ "pending",
    grepl("active|recognized", s) ~ "active",
    TRUE ~ "unknown"
  )
}

pt_face_bin_61 <- function(face_afy) {
  dplyr::case_when(
    is.na(face_afy)                    ~ "missing",
    !is.na(face_afy) & face_afy == 0   ~ "0",
    face_afy > 0    & face_afy <= 1    ~ ">0–1",
    face_afy > 1    & face_afy <= 10   ~ ">1–10",
    face_afy > 10   & face_afy <= 100  ~ ">10–100",
    face_afy > 100  & face_afy <= 1000 ~ ">100–1,000",
    face_afy > 1000                    ~ ">1,000",
    TRUE                               ~ "other"
  )
}

# ---- Main -------------------------------------------------------------------

if (is.na(swrcb_pod_rds) || !nzchar(swrcb_pod_rds) || !file.exists(swrcb_pod_rds)) {
  stop(
    "SWRCB POD map RDS not found. Checked default cache/RDS locations.\n",
    "Expected 04_processed_data/cache/latest/swrcb_pod_wr_blm_map.rds.\n",
    "Run the SWRCB POD preprocessing/core cache first, or set SWRCB_POD_BLM_MAP_RDS."
  )
}

if (!file.exists(blm_lands_rds)) {
  stop(
    "Missing BLM managed-lands RDS: ", blm_lands_rds, "\n",
    "Refresh BLM/core preprocessing first, or set SWRCB_POD_BLM_LANDS_RDS."
  )
}

pt_log_61("Reading SWRCB / CalWATRS POD source: ", swrcb_pod_rds)
swrcb <- readRDS(swrcb_pod_rds)
if (!inherits(swrcb, "sf") && !is.data.frame(swrcb)) {
  stop("SWRCB POD source is not an sf object or data frame: ", swrcb_pod_rds)
}

swrcb_df <- as.data.frame(swrcb, stringsAsFactors = FALSE)
pt_log_61("SWRCB POD rows: ", format(nrow(swrcb_df), big.mark = ","))

## The browser layer creates this same fallback ID when the cache does not
## already carry one.  The monthly distance cache should be rerun after the
## SWRCB POD map-ready object is refreshed, so this row-order fallback remains
## safe and transparent.
if (!"pt_swrcb_layer_id" %in% names(swrcb_df)) {
  swrcb_df$pt_swrcb_layer_id <- paste0("swrcb_pod_wr_", seq_len(nrow(swrcb_df)))
}
pt_swrcb_layer_id <- pt_clean_id_61(swrcb_df$pt_swrcb_layer_id)

source_key <- pt_source_key_61(swrcb_df)
valid_source <- source_key %in% c("official", "spatial", "name_candidate")

status_raw <- pt_first_col_61(swrcb_df, c("swrcb_status_group", "status_group", "wr_status"))
status_key <- pt_status_key_61(status_raw)
face_afy_vec <- if ("face_afy" %in% names(swrcb_df)) pt_num_61(swrcb_df$face_afy) else rep(NA_real_, nrow(swrcb_df))
face_bin <- pt_face_bin_61(face_afy_vec)

pt_log_61("Source split: official=", sum(source_key == "official", na.rm = TRUE),
          "; spatial=", sum(source_key == "spatial", na.rm = TRUE),
          "; name/text=", sum(source_key == "name_candidate", na.rm = TRUE),
          "; other/excluded=", sum(!valid_source, na.rm = TRUE), ".")

pt_log_61("Reading canonical BLM-CA managed lands RDS for SWRCB POD distance: ", blm_lands_rds)
blm <- readRDS(blm_lands_rds)
blm <- pt_as_blm_sf_61(blm)

pt_log_61("BLM distance source path: ", blm_lands_rds)
pt_log_61("BLM distance source modified: ", pt_fmt_time_61(blm_lands_rds) %||% "not available")
pt_log_61("BLM geometry rows/features before final dissolve: ", nrow(blm))
pt_log_61("BLM geometry CRS EPSG: ", sf::st_crs(blm)$epsg)
pt_log_61("BLM geometry bbox: ", pt_bbox_text_61(blm))

blm_geom <- sf::st_geometry(blm)
if (length(blm_geom) > 1) {
  step <- Sys.time()
  pt_log_61("Dissolving BLM geometry records to one managed-lands geometry...")
  blm_geom <- sf::st_union(blm_geom)
  pt_log_61("BLM dissolve complete (", pt_elapsed_61(step), ").")
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

pts_info <- pt_make_points_sf_61(swrcb)
pts <- pts_info$points
ll <- pts_info$latlon

out <- tibble(
  pt_swrcb_layer_id = pt_swrcb_layer_id,
  swrcb_source_key = source_key,
  water_right = pt_first_col_61(swrcb_df, c("swrcbpop_water_right", "water_right_id", "wr_id", "right_id")),
  holder = pt_first_col_61(swrcb_df, c("swrcbpop_holder", "primary_owner", "holder", "owner_name")),
  face_afy = face_afy_vec,
  face_bin = face_bin,
  swrcb_status_group = status_raw,
  swrcb_status_key = status_key,
  blm_include_reason_display = pt_first_col_61(swrcb_df, c("blm_include_reason_display", "include_reason", "screening_reason")),
  source_classification_note = dplyr::case_when(
    source_key == "official" ~ "SWRCB 2026 BLM WR list flag is true",
    source_key == "spatial" ~ "Additional BRIM spatial match: reason is Spatial + name match or Spatial only",
    source_key == "name_candidate" ~ "Additional BRIM name/text candidate: reason is Name match only",
    TRUE ~ "Not displayed by current three-source SWRCB layer rules"
  ),
  has_usable_coordinates = pts_info$ok,
  lat = ll$lat,
  lon = ll$lon,
  on_blm_ca = NA_character_,
  dist_to_blm_mi = NA_real_,
  dist_to_blm_ft = NA_real_,
  blm_distance_run_time = run_time,
  blm_source_rds = blm_lands_rds,
  blm_source_modified = pt_fmt_time_61(blm_lands_rds),
  swrcb_pod_source_rds = swrcb_pod_rds,
  swrcb_pod_source_modified = pt_fmt_time_61(swrcb_pod_rds)
)

if (!inherits(pts, "sf") || nrow(pts) == 0) {
  warning("No SWRCB POD rows had usable coordinates; writing empty distance fields.")
} else {
  pt_log_61("Rows with usable coordinates: ", format(nrow(pts), big.mark = ","),
            " of ", format(nrow(swrcb_df), big.mark = ","), ".")

  step <- Sys.time()
  pt_log_61("Step 1/4: testing POD intersections with BLM-CA managed lands...")
  inside <- lengths(sf::st_intersects(pts, blm_geom, sparse = TRUE)) > 0
  pt_log_61("Step 1/4 complete: ", format(sum(inside, na.rm = TRUE), big.mark = ","),
            " PODs on BLM (", pt_elapsed_61(step), ").")

  step <- Sys.time()
  pt_log_61("Step 2/4: finding nearest BLM geometry for all PODs...")
  nearest <- sf::st_nearest_feature(pts, blm_geom)
  pt_log_61("Step 2/4 complete (", pt_elapsed_61(step), ").")

  step <- Sys.time()
  pt_log_61("Step 3/4: calculating projected distances in EPSG:", target_crs, "...")
  dist_m <- as.numeric(sf::st_distance(sf::st_geometry(pts), sf::st_geometry(blm_geom)[nearest], by_element = TRUE))
  dist_m[inside] <- 0
  pt_log_61("Step 3/4 complete (", pt_elapsed_61(step), ").")

  out$on_blm_ca[pts$row_id] <- pt_bool_chr_61(inside)
  out$dist_to_blm_mi[pts$row_id] <- round(dist_m / 1609.344, 3)
  out$dist_to_blm_ft[pts$row_id] <- round(dist_m * 3.280839895, 0)
}

## Keep one row per marker ID for safe joins during final map build.
out_join <- out |>
  filter(!is.na(.data$pt_swrcb_layer_id), .data$pt_swrcb_layer_id != "") |>
  distinct(pt_swrcb_layer_id, .keep_all = TRUE)

step <- Sys.time()
pt_log_61("Step 4/4: writing cache and QA outputs...")

out_path <- file.path(cache_dir, "swrcb_pod_blm_distance_fields.csv")
readr::write_csv(out_join, out_path)

summary_tbl <- tibble(
  metric = c(
    "swrcb_pod_rows",
    "unique_pt_swrcb_layer_id_rows_written",
    "official_source_rows",
    "additional_spatial_source_rows",
    "additional_name_text_source_rows",
    "other_excluded_source_rows",
    "active_status_rows",
    "pending_status_rows",
    "inactive_cancelled_status_rows",
    "unknown_other_status_rows",
    "rows_with_usable_coordinates",
    "rows_with_distance",
    "on_BLM_CA_managed_lands",
    "within_1_mile_of_BLM_CA_managed_lands",
    "within_5_miles_of_BLM_CA_managed_lands",
    "within_10_miles_of_BLM_CA_managed_lands",
    "missing_BLM_distance",
    "swrcb_pod_source_rds",
    "swrcb_pod_source_modified",
    "BLM_distance_source_rds",
    "BLM_distance_source_modified",
    "BLM_source_EPSG",
    "BLM_source_feature_rows",
    "BLM_source_geometry_type",
    "BLM_source_bbox"
  ),
  value = c(
    as.character(nrow(swrcb_df)),
    as.character(nrow(out_join)),
    as.character(sum(source_key == "official", na.rm = TRUE)),
    as.character(sum(source_key == "spatial", na.rm = TRUE)),
    as.character(sum(source_key == "name_candidate", na.rm = TRUE)),
    as.character(sum(!valid_source, na.rm = TRUE)),
    as.character(sum(status_key == "active", na.rm = TRUE)),
    as.character(sum(status_key == "pending", na.rm = TRUE)),
    as.character(sum(status_key == "inactive", na.rm = TRUE)),
    as.character(sum(status_key == "unknown", na.rm = TRUE)),
    as.character(if (inherits(pts, "sf")) nrow(pts) else 0L),
    as.character(sum(!is.na(out_join$dist_to_blm_mi))),
    as.character(sum(tolower(as.character(out_join$on_blm_ca)) == "true", na.rm = TRUE)),
    as.character(sum(out_join$dist_to_blm_mi <= 1, na.rm = TRUE)),
    as.character(sum(out_join$dist_to_blm_mi <= 5, na.rm = TRUE)),
    as.character(sum(out_join$dist_to_blm_mi <= 10, na.rm = TRUE)),
    as.character(sum(is.na(out_join$dist_to_blm_mi))),
    swrcb_pod_rds,
    pt_fmt_time_61(swrcb_pod_rds) %||% NA_character_,
    blm_lands_rds,
    pt_fmt_time_61(blm_lands_rds) %||% NA_character_,
    as.character(sf::st_crs(blm)$epsg),
    as.character(nrow(blm)),
    paste(unique(as.character(sf::st_geometry_type(blm))), collapse = ", "),
    pt_bbox_text_61(blm)
  )
)

source_summary_tbl <- out_join |>
  mutate(
    on_blm_flag = tolower(as.character(.data$on_blm_ca)) == "true",
    has_dist = !is.na(.data$dist_to_blm_mi)
  ) |>
  group_by(.data$swrcb_source_key) |>
  summarise(
    records = n(),
    rows_with_distance = sum(.data$has_dist),
    on_blm = sum(.data$on_blm_flag, na.rm = TRUE),
    within_1_mi = sum(.data$dist_to_blm_mi <= 1, na.rm = TRUE),
    within_5_mi = sum(.data$dist_to_blm_mi <= 5, na.rm = TRUE),
    within_10_mi = sum(.data$dist_to_blm_mi <= 10, na.rm = TRUE),
    .groups = "drop"
  )

preview_tbl <- out_join |>
  mutate(dist_sort = suppressWarnings(as.numeric(.data$dist_to_blm_mi))) |>
  arrange(.data$dist_sort) |>
  select(
    pt_swrcb_layer_id,
    swrcb_source_key,
    water_right,
    holder,
    face_afy,
    face_bin,
    swrcb_status_group,
    swrcb_status_key,
    blm_include_reason_display,
    source_classification_note,
    has_usable_coordinates,
    lat,
    lon,
    on_blm_ca,
    dist_to_blm_mi,
    dist_to_blm_ft
  ) |>
  head(100)

classification_tbl <- out_join |>
  select(
    pt_swrcb_layer_id,
    swrcb_source_key,
    source_classification_note,
    blm_include_reason_display,
    water_right,
    holder,
    face_afy,
    face_bin,
    swrcb_status_group,
    swrcb_status_key,
    has_usable_coordinates,
    lat,
    lon,
    on_blm_ca,
    dist_to_blm_mi,
    dist_to_blm_ft,
    blm_distance_run_time,
    blm_source_rds,
    blm_source_modified,
    swrcb_pod_source_rds,
    swrcb_pod_source_modified
  ) |>
  arrange(.data$swrcb_source_key, .data$water_right, .data$pt_swrcb_layer_id)

status_by_source_tbl <- out_join |>
  mutate(
    on_blm_flag = tolower(as.character(.data$on_blm_ca)) == "true",
    has_dist = !is.na(.data$dist_to_blm_mi)
  ) |>
  group_by(.data$swrcb_source_key, .data$swrcb_status_key, .data$swrcb_status_group) |>
  summarise(
    records = n(),
    rows_with_distance = sum(.data$has_dist),
    on_blm = sum(.data$on_blm_flag, na.rm = TRUE),
    within_1_mi = sum(.data$dist_to_blm_mi <= 1, na.rm = TRUE),
    within_5_mi = sum(.data$dist_to_blm_mi <= 5, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(.data$swrcb_source_key, .data$swrcb_status_key, .data$swrcb_status_group)

reason_by_source_tbl <- out_join |>
  group_by(.data$swrcb_source_key, .data$blm_include_reason_display) |>
  summarise(records = n(), .groups = "drop") |>
  arrange(.data$swrcb_source_key, desc(.data$records), .data$blm_include_reason_display)

distance_bins_by_source_tbl <- out_join |>
  mutate(
    distance_bin = dplyr::case_when(
      is.na(.data$dist_to_blm_mi) ~ "missing",
      .data$dist_to_blm_mi == 0 ~ "on BLM / 0 mi",
      .data$dist_to_blm_mi <= 1 ~ ">0–1 mi",
      .data$dist_to_blm_mi <= 5 ~ ">1–5 mi",
      .data$dist_to_blm_mi <= 10 ~ ">5–10 mi",
      TRUE ~ ">10 mi"
    )
  ) |>
  group_by(.data$swrcb_source_key, .data$distance_bin) |>
  summarise(records = n(), .groups = "drop") |>
  arrange(.data$swrcb_source_key, .data$distance_bin)

summary_path <- file.path(qa_dir, "swrcb_pod_blm_distance_summary_latest.csv")
source_summary_path <- file.path(qa_dir, "swrcb_pod_blm_distance_by_source_latest.csv")
preview_path <- file.path(qa_dir, "swrcb_pod_blm_distance_preview_latest.csv")
classification_path <- file.path(qa_dir, "swrcb_pod_source_classification_latest.csv")
status_by_source_path <- file.path(qa_dir, "swrcb_pod_status_by_source_latest.csv")
reason_by_source_path <- file.path(qa_dir, "swrcb_pod_reason_by_source_latest.csv")
distance_bins_by_source_path <- file.path(qa_dir, "swrcb_pod_distance_bins_by_source_latest.csv")

readr::write_csv(summary_tbl, summary_path)
readr::write_csv(source_summary_tbl, source_summary_path)
readr::write_csv(preview_tbl, preview_path)
readr::write_csv(classification_tbl, classification_path)
readr::write_csv(status_by_source_tbl, status_by_source_path)
readr::write_csv(reason_by_source_tbl, reason_by_source_path)
readr::write_csv(distance_bins_by_source_tbl, distance_bins_by_source_path)

pt_log_61("Step 4/4 complete (", pt_elapsed_61(step), ").")
pt_log_61("Saved SWRCB POD BLM-distance cache: ", out_path)
pt_log_61("Saved SWRCB POD BLM-distance QA summary: ", summary_path)
pt_log_61("Saved SWRCB POD BLM-distance by-source QA: ", source_summary_path)
pt_log_61("Saved SWRCB POD BLM-distance QA preview: ", preview_path)
pt_log_61("Saved SWRCB POD source-classification QA: ", classification_path)
pt_log_61("Saved SWRCB POD status-by-source QA: ", status_by_source_path)
pt_log_61("Saved SWRCB POD reason-by-source QA: ", reason_by_source_path)
pt_log_61("Saved SWRCB POD distance-bin QA: ", distance_bins_by_source_path)
pt_log_61(
  "SWRCB POD BLM distance fields populated: ",
  format(sum(!is.na(out_join$dist_to_blm_mi)), big.mark = ","), " rows; ",
  format(sum(tolower(as.character(out_join$on_blm_ca)) == "true", na.rm = TRUE), big.mark = ","), " on BLM; ",
  format(sum(out_join$dist_to_blm_mi <= 1, na.rm = TRUE), big.mark = ","), " within 1 mi; ",
  format(sum(out_join$dist_to_blm_mi <= 5, na.rm = TRUE), big.mark = ","), " within 5 mi."
)
pt_log_61("Total elapsed: ", pt_elapsed_61(run_start), ".")

print(summary_tbl)
print(source_summary_tbl)
print(status_by_source_tbl)
print(reason_by_source_tbl)
print(distance_bins_by_source_tbl)
