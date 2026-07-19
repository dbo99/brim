# ==== 13_huc_climate_recharge_summary.r ======================================
##
## PURPOSE:
##   Summarize 1991–2020 mean annual precipitation and groundwater recharge
##   rasters to any PortaTreasure2 HUC level:
##
##     HUC2, HUC4, HUC6, HUC8, HUC10, HUC12
##
## INPUT RASTERS:
##   PRISM mean annual precipitation normal, 1991–2020:
##     01_raw_data/raster/prism_ppt_us_30s_2020_avg_30y.tif
##
##   USGS BCMv8 recharge, 1991–2020:
##     01_raw_data/raster/rch1991_2020_ave.asc
##
## INPUT POLYGONS:
##   04_processed_data/rds/huc_all_full.rds
##
## OUTPUTS:
##   For each selected HUC level:
##
##     04_processed_data/rds/hucXX_climate_recharge_full.rds
##     04_processed_data/rds/hucXX_climate_recharge_table.rds
##
##   plus timestamped versions and QA CSV outputs.
##
## METRICS:
##   For each HUC polygon:
##
##     map_mm          mean annual precipitation depth, mm/yr
##     map_in          mean annual precipitation depth, in/yr
##     ppt_acft        mean annual precipitation volume, acre-ft/yr
##     ppt_kaf         mean annual precipitation volume, thousand acre-ft/yr
##
##     rech_mm         mean annual recharge depth, mm/yr
##     rech_in         mean annual recharge depth, in/yr
##     rech_acft       mean annual recharge volume, acre-ft/yr
##     rech_kaf        mean annual recharge volume, thousand acre-ft/yr
##
##     rech_eff_pct    recharge efficiency = recharge volume / precip volume * 100
##
## NOTES:
##   - Extraction uses exact polygon weights.
##   - Volumes are computed from raster cell values and intersected cell areas.
##   - Polygon area fields are computed in EPSG:3310.
##   - Full outputs keep geometry transformed to EPSG:4326.
##   - Table outputs drop geometry and are intended for lightweight joins to
##     existing simplified HUC layers in the Leaflet map cache.
##

# ==== 1. Load configuration and helper functions =============================

source("00_config/config_paths.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
})

# ==== 3. User controls =======================================================
##
## Set TRUE only for the HUC levels you want to process in this run.
##
## Suggested immediate run:
##   HUC2/HUC4/HUC6/HUC8 = TRUE
##   HUC10/HUC12         = FALSE
##
RUN_HUC6  <- TRUE
RUN_HUC8  <- FALSE
RUN_HUC10 <- FALSE
RUN_HUC12 <- FALSE

## Chunking controls memory during exact extraction.
##
## Smaller batches are safer but slower. Coarser HUC layers are tiny, so 50–100
## is fine. HUC12 should usually be 50 or lower if rerun.

BATCH_SIZE_BY_HUC <- c(
  huc2  = 1,
  huc4  = 2,
  huc6  = 5,
  huc8  = 10,
  huc10 = 50,
  huc12 = 25
)

## Use one timestamp for all outputs from this run.

RUN_TS <- make_timestamp()

WRITE_QA <- TRUE
WRITE_CSV_TABLES <- TRUE

## Raster NoData sentinels used defensively, especially for BCMv8 ASC files.

NA_FIX <- c(32767, -9999, -9998, -999)

## Unit conversions.

MM_PER_INCH <- 25.4
ACREFT_M3 <- 1233.48184
SQM_PER_SQMI <- 2589988.110336
SQM_PER_ACRE <- 4046.8564224

## Some ASCII rasters may lack CRS metadata. BCMv8 recharge is expected to be
## EPSG:3310. PRISM should normally have CRS metadata in the GeoTIFF / sidecar
## files, but a defensive fallback is provided for geographic PRISM grids.

ASSUME_BCM_EPSG_IF_MISSING <- 3310
ASSUME_PRISM_EPSG_IF_MISSING <- 4269

## Minimum valid raster coverage fraction below which values are flagged.

LOW_VALID_FRACTION_WARN <- 0.80

## terra memory/progress options.

terra::terraOptions(progress = 1, memfrac = 0.70)

# ==== 4. Input paths =========================================================

PRISM_RASTER <- file.path(
  DIR$raw,
  "raster",
  "prism_ppt_us_30s_2020_avg_30y.tif"
)

BCM_RECH_RASTER <- file.path(
  DIR$raw,
  "raster",
  "rch1991_2020_ave.asc"
)

HUC_FULL_RDS <- file.path(
  DIR$rds,
  "huc_all_full.rds"
)

# ==== 5. Output helpers ======================================================

save_latest_and_timestamped <- function(x, base_name) {
  
  save_rds_cached(
    x = x,
    timestamped_path = file.path(
      DIR$rds,
      timestamped_name(base_name, "rds", RUN_TS)
    ),
    latest_path = file.path(
      DIR$rds,
      paste0(base_name, ".rds")
    )
  )
}

save_geometry_free_table <- function(x, base_name) {
  
  tbl <- sf::st_drop_geometry(x)
  
  save_rds_cached(
    x = tbl,
    timestamped_path = file.path(
      DIR$rds,
      timestamped_name(base_name, "rds", RUN_TS)
    ),
    latest_path = file.path(
      DIR$rds,
      paste0(base_name, ".rds")
    )
  )
}

# ==== 6. General helper functions ===========================================

pt_stop_if_missing <- function(path, label) {
  if (!file.exists(path)) {
    stop("Missing ", label, ":\n  ", path)
  }
}

pt_clean_raster_values <- function(r, nodata_values = NA_FIX) {
  
  for (v in nodata_values) {
    r <- terra::ifel(r == v, NA, r)
  }
  
  ## Both precipitation and recharge should be non-negative.
  r <- terra::ifel(r < 0, NA, r)
  
  r
}

pt_assign_crs_if_missing <- function(r, epsg, label) {
  
  cr <- terra::crs(r, proj = TRUE)
  
  if (!is.na(cr) && nzchar(cr)) {
    return(list(r = r, crs_assumed = FALSE))
  }
  
  message(label, " raster has missing CRS metadata; assigning EPSG:", epsg)
  terra::crs(r) <- sf::st_crs(epsg)$wkt
  
  list(r = r, crs_assumed = TRUE)
}

pt_prepare_prism_raster <- function(path) {
  
  message("Reading PRISM precipitation raster:")
  message("  ", path)
  
  r <- terra::rast(path)
  
  cr <- terra::crs(r, proj = TRUE)
  
  if (is.na(cr) || !nzchar(cr)) {
    message(
      "PRISM raster has missing CRS metadata; assigning EPSG:",
      ASSUME_PRISM_EPSG_IF_MISSING
    )
    terra::crs(r) <- sf::st_crs(ASSUME_PRISM_EPSG_IF_MISSING)$wkt
    crs_assumed <- TRUE
  } else {
    crs_assumed <- FALSE
  }
  
  names(r) <- "map_mm_raw"
  r <- pt_clean_raster_values(r)
  names(r) <- "map_mm"
  
  list(r = r, crs_assumed = crs_assumed)
}

pt_prepare_bcm_recharge_raster <- function(path) {
  
  message("Reading BCMv8 recharge raster:")
  message("  ", path)
  
  r <- terra::rast(path)
  
  crs_info <- pt_assign_crs_if_missing(
    r = r,
    epsg = ASSUME_BCM_EPSG_IF_MISSING,
    label = "BCMv8 recharge"
  )
  
  r <- crs_info$r
  names(r) <- "rech_mm_raw"
  r <- pt_clean_raster_values(r)
  names(r) <- "rech_mm"
  
  list(r = r, crs_assumed = crs_info$crs_assumed)
}

pt_get_value_col <- function(ex, wanted_name) {
  
  if (wanted_name %in% names(ex)) {
    return(wanted_name)
  }
  
  possible <- setdiff(names(ex), c("ID", "cell", "weight", "cell_area_m2"))
  
  if (length(possible) < 1) {
    stop("Could not identify raster value column from extract() output.")
  }
  
  possible[1]
}

pt_calc_intersection_area_m2 <- function(weight, cell_area_m2) {
  
  ## terra::extract(weights/exact) should usually return fractional weights.
  ## Some workflows may return area-like weights. This mirrors the defensive
  ## logic already used in the user's BCMv8 scripts.
  
  wmax <- suppressWarnings(max(weight, na.rm = TRUE))
  
  if (!is.finite(wmax)) {
    return(rep(NA_real_, length(weight)))
  }
  
  weight_is_area <- isTRUE(wmax > 1.01)
  
  if (weight_is_area) {
    as.numeric(weight)
  } else {
    as.numeric(weight) * as.numeric(cell_area_m2)
  }
}

pt_selected_huc_levels <- function() {
  
  run_tbl <- tibble::tibble(
    level = c(2, 4, 6, 8, 10, 12),
    layer_id = paste0("huc", level),
    run = c(
      RUN_HUC2,
      RUN_HUC4,
      RUN_HUC6,
      RUN_HUC8,
      RUN_HUC10,
      RUN_HUC12
    ),
    batch_size = as.integer(BATCH_SIZE_BY_HUC[paste0("huc", level)])
  )
  
  run_tbl |>
    dplyr::filter(.data$run)
}

filter_huc_for_climate_run <- function(x, huc_level) {
  
  ## HUC2 filter:
  ## Keep California Region, Great Basin Region, and Lower Colorado Region.
  ## Drop Pacific Northwest Region.
  ##
  ## HUC4/HUC6/HUC8 are intentionally left unfiltered.
  
  if (huc_level != 2) {
    return(x)
  }
  
  candidate_name_cols <- c(
    "huc2_name",
    "name",
    "Name",
    "region",
    "Region"
  )
  
  name_col <- candidate_name_cols[candidate_name_cols %in% names(x)][1]
  
  if (is.na(name_col) || length(name_col) == 0) {
    stop(
      "Could not find a HUC2 name/region column for filtering.\n",
      "Available fields: ",
      paste(names(x), collapse = ", ")
    )
  }
  
  keep_regions <- c(
    "California Region",
    "Great Basin Region",
    "Lower Colorado Region"
  )
  
  message("Filtering HUC2 using field: ", name_col)
  message("Keeping HUC2 regions: ", paste(keep_regions, collapse = ", "))
  
  x |>
    dplyr::filter(.data[[name_col]] %in% keep_regions)
}

# ==== 7. Raster summarization function =======================================
##
## This is the heavy GIS function.
##
## IMPORTANT PERFORMANCE FIX:
##   The raster is cropped separately for each batch, not once for the full HUC
##   layer. This prevents HUC2/HUC4/HUC6/HUC8 runs from creating unnecessarily
##   huge western-U.S. raster crops.

summarize_raster_to_polygons <- function(
    r_mm,
    polygons_sf,
    id_col,
    metric_prefix,
    batch_size = 100,
    raster_label = "raster"
) {
  
  stopifnot(inherits(r_mm, "SpatRaster"))
  stopifnot(inherits(polygons_sf, "sf"))
  stopifnot(id_col %in% names(polygons_sf))
  
  message("\nSummarizing ", raster_label, " to polygons...")
  message("  polygons: ", nrow(polygons_sf))
  message("  batch size: ", batch_size)
  
  value_col <- paste0(metric_prefix, "_mm")
  names(r_mm) <- value_col
  
  raster_crs <- terra::crs(r_mm, proj = TRUE)
  
  if (is.na(raster_crs) || !nzchar(raster_crs)) {
    stop("Raster CRS is missing for ", raster_label, ".")
  }
  
  polygons_r <- polygons_sf |>
    sf::st_make_valid() |>
    sf::st_transform(raster_crs)
  
  polygons_r$.pt_row_id <- seq_len(nrow(polygons_r))
  
  polygons_area <- polygons_sf |>
    sf::st_make_valid() |>
    sf::st_transform(3310)
  
  poly_area_tbl <- tibble::tibble(
    .pt_row_id = seq_len(nrow(polygons_area)),
    poly_area_m2 = as.numeric(sf::st_area(polygons_area))
  )
  
  batches <- split(
    seq_len(nrow(polygons_r)),
    ceiling(seq_len(nrow(polygons_r)) / batch_size)
  )
  
  out_list <- vector("list", length(batches))
  
  for (b in seq_along(batches)) {
    
    idx <- batches[[b]]
    chunk <- polygons_r[idx, , drop = FALSE]
    chunk_ids <- chunk$.pt_row_id
    
    message(
      "  batch ", b, "/", length(batches),
      " | rows ", min(idx), "–", max(idx)
    )
    
    ## Crop to this batch only.
    message("    cropping raster to batch extent...")
    r_mm_crop <- terra::crop(r_mm, terra::vect(chunk))
    
    message("    building batch cell-area raster...")
    r_area <- terra::cellSize(r_mm_crop, unit = "m")
    names(r_area) <- "cell_area_m2"
    
    r_stack_crop <- c(r_mm_crop, r_area)
    
    ex <- terra::extract(
      r_stack_crop,
      terra::vect(chunk),
      cells = TRUE,
      weights = TRUE,
      exact = TRUE
    )
    
    if (is.null(ex) || nrow(ex) == 0) {
      
      out_list[[b]] <- tibble::tibble(
        .pt_row_id = chunk_ids,
        extracted_area_m2 = 0,
        valid_area_m2 = 0,
        depth_area_sum_mm_m2 = NA_real_,
        volume_m3 = NA_real_
      )
      
      rm(r_mm_crop, r_area, r_stack_crop)
      invisible(gc())
      next
    }
    
    if (!all(c("ID", "weight", "cell_area_m2") %in% names(ex))) {
      stop(
        "extract() did not return expected columns for ", raster_label,
        ". Got: ", paste(names(ex), collapse = ", ")
      )
    }
    
    actual_value_col <- pt_get_value_col(ex, value_col)
    
    ex_tbl <- tibble::as_tibble(ex) |>
      dplyr::mutate(
        .pt_row_id = chunk_ids[.data$ID],
        value_mm = as.numeric(.data[[actual_value_col]]),
        int_area_m2 = pt_calc_intersection_area_m2(
          weight = .data$weight,
          cell_area_m2 = .data$cell_area_m2
        ),
        is_valid = !is.na(.data$value_mm) &
          !is.na(.data$int_area_m2) &
          .data$int_area_m2 > 0
      )
    
    out_list[[b]] <- ex_tbl |>
      dplyr::group_by(.data$.pt_row_id) |>
      dplyr::summarise(
        extracted_area_m2 = sum(.data$int_area_m2, na.rm = TRUE),
        valid_area_m2 = sum(.data$int_area_m2[.data$is_valid], na.rm = TRUE),
        depth_area_sum_mm_m2 = sum(
          .data$value_mm[.data$is_valid] *
            .data$int_area_m2[.data$is_valid],
          na.rm = TRUE
        ),
        volume_m3 = sum(
          (.data$value_mm[.data$is_valid] / 1000) *
            .data$int_area_m2[.data$is_valid],
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    rm(r_mm_crop, r_area, r_stack_crop, ex, ex_tbl)
    invisible(gc())
  }
  
  summary_tbl <- dplyr::bind_rows(out_list) |>
    dplyr::right_join(
      tibble::tibble(.pt_row_id = seq_len(nrow(polygons_sf))),
      by = ".pt_row_id"
    ) |>
    dplyr::left_join(poly_area_tbl, by = ".pt_row_id") |>
    dplyr::arrange(.data$.pt_row_id) |>
    dplyr::mutate(
      mean_mm = dplyr::if_else(
        .data$valid_area_m2 > 0,
        .data$depth_area_sum_mm_m2 / .data$valid_area_m2,
        NA_real_
      ),
      mean_in = .data$mean_mm / MM_PER_INCH,
      acft = .data$volume_m3 / ACREFT_M3,
      kaf = .data$acft / 1000,
      valid_frac = dplyr::if_else(
        .data$poly_area_m2 > 0,
        .data$valid_area_m2 / .data$poly_area_m2,
        NA_real_
      ),
      extracted_frac = dplyr::if_else(
        .data$poly_area_m2 > 0,
        .data$extracted_area_m2 / .data$poly_area_m2,
        NA_real_
      )
    ) |>
    dplyr::transmute(
      .pt_row_id,
      !!paste0(metric_prefix, "_mm") := .data$mean_mm,
      !!paste0(metric_prefix, "_in") := .data$mean_in,
      !!paste0(metric_prefix, "_acft") := .data$acft,
      !!paste0(metric_prefix, "_kaf") := .data$kaf,
      !!paste0(metric_prefix, "_valid_frac") := .data$valid_frac,
      !!paste0(metric_prefix, "_extracted_frac") := .data$extracted_frac,
      !!paste0(metric_prefix, "_valid_area_m2") := .data$valid_area_m2
    )
  
  summary_tbl
}

# ==== 8. Build one HUC-level summary =========================================

build_huc_climate_recharge <- function(
    huc_sf,
    huc_level,
    prism_r,
    rech_r,
    batch_size
) {
  
  huc_code_col <- paste0("huc", huc_level)
  huc_name_col <- paste0("huc", huc_level, "_name")
  
  if (!huc_code_col %in% names(huc_sf)) {
    stop("Missing HUC code column: ", huc_code_col)
  }
  
  if (!huc_name_col %in% names(huc_sf)) {
    stop("Missing HUC name column: ", huc_name_col)
  }
  
  message("\n============================================================")
  message("Building HUC", huc_level, " climate/recharge summary")
  message("Rows: ", nrow(huc_sf))
  message("============================================================")
  
  ## Use a row ID for safe joins, because HUC names/codes may not be unique
  ## in every possible future layer.
  huc_work <- huc_sf |>
    sf::st_make_valid() |>
    dplyr::mutate(.pt_row_id = dplyr::row_number())
  
  ## Area fields in EPSG:3310.
  huc_area_3310 <- huc_work |>
    sf::st_transform(3310)
  
  area_m2_vec <- as.numeric(sf::st_area(huc_area_3310))
  
  area_tbl <- tibble::tibble(
    .pt_row_id = huc_work$.pt_row_id,
    area_m2 = area_m2_vec,
    area_sqmi = area_m2_vec / SQM_PER_SQMI,
    area_acres = area_m2_vec / SQM_PER_ACRE
  )
  
  ## PRISM precipitation summary.
  ppt_tbl <- summarize_raster_to_polygons(
    r_mm = prism_r,
    polygons_sf = huc_work,
    id_col = huc_code_col,
    metric_prefix = "ppt",
    batch_size = batch_size,
    raster_label = paste0("PRISM precipitation HUC", huc_level)
  )
  
  ## BCMv8 recharge summary.
  rech_tbl <- summarize_raster_to_polygons(
    r_mm = rech_r,
    polygons_sf = huc_work,
    id_col = huc_code_col,
    metric_prefix = "rech",
    batch_size = batch_size,
    raster_label = paste0("BCMv8 recharge HUC", huc_level)
  )
  
  out <- huc_work |>
    dplyr::left_join(area_tbl, by = ".pt_row_id") |>
    dplyr::left_join(ppt_tbl, by = ".pt_row_id") |>
    dplyr::left_join(rech_tbl, by = ".pt_row_id") |>
    dplyr::mutate(
      ## User-friendly aliases.
      map_mm = .data$ppt_mm,
      map_in = .data$ppt_in,
      ppt_acft = .data$ppt_acft,
      ppt_kaf = .data$ppt_kaf,
      
      rech_eff_pct = dplyr::if_else(
        !is.na(.data$rech_acft) &
          !is.na(.data$ppt_acft) &
          .data$ppt_acft > 0,
        100 * .data$rech_acft / .data$ppt_acft,
        NA_real_
      ),
      
      climate_summary_source = "PRISM precipitation normal + BCMv8 recharge, 1991-2020",
      climate_summary_run_ts = RUN_TS
    ) |>
    dplyr::select(-dplyr::all_of(".pt_row_id")) |>
    sf::st_transform(4326)
  
  out
}

# ==== 9. QA helper ===========================================================

make_huc_qa_row <- function(x, layer_id) {
  
  df <- sf::st_drop_geometry(x)
  
  tibble::tibble(
    layer_id = layer_id,
    rows = nrow(df),
    area_sqmi_sum = sum(df$area_sqmi, na.rm = TRUE),
    
    ppt_mean_in_min = min(df$map_in, na.rm = TRUE),
    ppt_mean_in_mean = mean(df$map_in, na.rm = TRUE),
    ppt_mean_in_max = max(df$map_in, na.rm = TRUE),
    ppt_kaf_sum = sum(df$ppt_kaf, na.rm = TRUE),
    
    rech_mean_in_min = min(df$rech_in, na.rm = TRUE),
    rech_mean_in_mean = mean(df$rech_in, na.rm = TRUE),
    rech_mean_in_max = max(df$rech_in, na.rm = TRUE),
    rech_kaf_sum = sum(df$rech_kaf, na.rm = TRUE),
    
    rech_eff_pct_min = min(df$rech_eff_pct, na.rm = TRUE),
    rech_eff_pct_mean = mean(df$rech_eff_pct, na.rm = TRUE),
    rech_eff_pct_max = max(df$rech_eff_pct, na.rm = TRUE),
    
    ppt_low_valid_count = sum(df$ppt_valid_frac < LOW_VALID_FRACTION_WARN, na.rm = TRUE),
    rech_low_valid_count = sum(df$rech_valid_frac < LOW_VALID_FRACTION_WARN, na.rm = TRUE),
    
    run_timestamp = RUN_TS
  )
}

# ==== 10. Input checks =======================================================

pt_stop_if_missing(PRISM_RASTER, "PRISM precipitation raster")
pt_stop_if_missing(BCM_RECH_RASTER, "BCMv8 recharge raster")
pt_stop_if_missing(HUC_FULL_RDS, "HUC full RDS")

levels_to_run <- pt_selected_huc_levels()

if (nrow(levels_to_run) == 0) {
  stop("No HUC levels selected. Set at least one RUN_HUC* switch to TRUE.")
}

message("Selected HUC levels:")
print(levels_to_run)

# ==== 11. Read inputs ========================================================

message("Reading HUC full RDS:")
message("  ", HUC_FULL_RDS)

huc_all <- readRDS(HUC_FULL_RDS)

if (!is.list(huc_all)) {
  stop("Expected huc_all_full.rds to be a list containing HUC layers.")
}

missing_huc_layers <- setdiff(levels_to_run$layer_id, names(huc_all))

if (length(missing_huc_layers) > 0) {
  stop(
    "huc_all_full.rds is missing selected HUC layer(s): ",
    paste(missing_huc_layers, collapse = ", "),
    "\nAvailable layers: ",
    paste(names(huc_all), collapse = ", ")
  )
}

prism_info <- pt_prepare_prism_raster(PRISM_RASTER)
rech_info  <- pt_prepare_bcm_recharge_raster(BCM_RECH_RASTER)

prism_r <- prism_info$r
rech_r  <- rech_info$r

message("\nRaster summary:")
message("  PRISM CRS assumed? ", prism_info$crs_assumed)
message("  BCMv8 CRS assumed? ", rech_info$crs_assumed)
message("  PRISM raster: ", terra::nrow(prism_r), " rows x ", terra::ncol(prism_r), " cols")
message("  BCMv8 raster: ", terra::nrow(rech_r), " rows x ", terra::ncol(rech_r), " cols")

# ==== 12. Process selected HUC levels ========================================

qa_rows <- list()

for (i in seq_len(nrow(levels_to_run))) {
  
  lvl <- levels_to_run$level[i]
  layer_id <- levels_to_run$layer_id[i]
  batch_size <- levels_to_run$batch_size[i]
  
  huc_input <- filter_huc_for_climate_run(
    x = huc_all[[layer_id]],
    huc_level = lvl
  )
  
  huc_summary <- build_huc_climate_recharge(
    huc_sf = huc_input,
    huc_level = lvl,
    prism_r = prism_r,
    rech_r = rech_r,
    batch_size = batch_size
  )
  
  rm(huc_input)
  
  save_latest_and_timestamped(
    x = huc_summary,
    base_name = paste0(layer_id, "_climate_recharge_full")
  )
  
  save_geometry_free_table(
    x = huc_summary,
    base_name = paste0(layer_id, "_climate_recharge_table")
  )
  
  if (WRITE_CSV_TABLES) {
    
    out_csv <- file.path(
      DIR$qa,
      paste0(layer_id, "_climate_recharge_table_", RUN_TS, ".csv")
    )
    
    huc_summary |>
      sf::st_drop_geometry() |>
      readr::write_csv(out_csv)
    
    message("Saved ", toupper(layer_id), " table CSV: ", out_csv)
  }
  
  qa_rows[[layer_id]] <- make_huc_qa_row(
    x = huc_summary,
    layer_id = layer_id
  )
  
  ## Drop this level before moving to the next one.
  rm(huc_summary)
  invisible(gc())
}

# ==== 13. QA summary =========================================================

if (WRITE_QA) {
  
  qa <- dplyr::bind_rows(qa_rows) |>
    dplyr::mutate(
      prism_crs_assumed = prism_info$crs_assumed,
      bcm_crs_assumed = rech_info$crs_assumed
    )
  
  out_qa <- file.path(
    DIR$qa,
    paste0("huc_climate_recharge_summary_qa_", RUN_TS, ".csv")
  )
  
  readr::write_csv(qa, out_qa)
  
  message("\nSaved QA CSV: ", out_qa)
  print(qa)
}

# ==== 14. Final summary ======================================================

message("\nDone: HUC climate/recharge summaries built.")

for (layer_id in levels_to_run$layer_id) {
  message("Latest ", toupper(layer_id), " full output:")
  message("  ", file.path(DIR$rds, paste0(layer_id, "_climate_recharge_full.rds")))
  message("Latest ", toupper(layer_id), " table output:")
  message("  ", file.path(DIR$rds, paste0(layer_id, "_climate_recharge_table.rds")))
}

message("\nSuggested QA checks:")
message('  h2 <- readRDS("04_processed_data/rds/huc2_climate_recharge_table.rds")')
message('  h4 <- readRDS("04_processed_data/rds/huc4_climate_recharge_table.rds")')
message('  h6 <- readRDS("04_processed_data/rds/huc6_climate_recharge_table.rds")')
message('  h8 <- readRDS("04_processed_data/rds/huc8_climate_recharge_table.rds")')
message('  summary(h8[, c("map_in", "ppt_kaf", "rech_in", "rech_kaf", "rech_eff_pct")])')
