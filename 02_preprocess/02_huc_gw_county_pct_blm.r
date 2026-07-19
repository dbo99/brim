# ==== 02_huc_gw_county_pct_blm.r ============================================
##
## PURPOSE:
##   Calculate percent BLM-managed land for:
##
##     1. HUC2, HUC4, HUC6, HUC8, HUC10, HUC12
##     2. Bulletin 118 groundwater basins/subbasins
##     3. California counties
##
## INPUTS:
##   Raw boundary files are declared explicitly in:
##
##     00_config/config_source_files.r
##
##   BLM-managed core polygon is produced by:
##
##     02_preprocess/01_blm_managed_and_held.r
##
## OUTPUTS:
##   04_processed_data/rds/huc_all_full.rds
##   04_processed_data/rds/huc_all_full_<timestamp>.rds
##   04_processed_data/rds/bull118gw_full.rds
##   04_processed_data/rds/bull118gw_full_<timestamp>.rds
##   04_processed_data/rds/county_full.rds
##   04_processed_data/rds/county_full_<timestamp>.rds
##   04_processed_data/gpkg/huc_gw_county_pct_blm.gpkg
##   04_processed_data/qa/huc_gw_county_pct_blm_qa_<timestamp>.csv
##
## IMPORTANT:
##   - Area/intersection math is performed in EPSG:3310, California Albers.
##   - Outputs are transformed to EPSG:4326 for Leaflet/web mapping.
##   - This script uses the dissolved BLM core RDS rather than independently
##     reading a raw BLM shapefile. That keeps all %BLM math tied to one
##     explicit BLM land-status snapshot.
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
  library(purrr)
  library(readr)
})

# ==== 3. User-facing switches ================================================

PROCESS_DEEP <- RUN$show_huc10_12
WRITE_GPKG   <- TRUE
WRITE_QA     <- TRUE

## Chunk size for polygon/BLM intersections.
## Smaller chunks reduce memory spikes; larger chunks may run faster.
INTERSECT_CHUNK_N <- 400

## Square meters per square mile.
SQMI_M2 <- 2.58999e6

# ==== 4. Define input and output paths =======================================

rds_blm_core <- file.path(DIR$rds, "blm_managed_core_3310.rds")

out_huc_latest <- file.path(DIR$rds, "huc_all_full.rds")
out_huc_timestamped <- file.path(
  DIR$rds,
  timestamped_name("huc_all_full", "rds")
)

out_gw_latest <- file.path(DIR$rds, "bull118gw_full.rds")
out_gw_timestamped <- file.path(
  DIR$rds,
  timestamped_name("bull118gw_full", "rds")
)

out_county_latest <- file.path(DIR$rds, "county_full.rds")
out_county_timestamped <- file.path(
  DIR$rds,
  timestamped_name("county_full", "rds")
)

out_gpkg <- file.path(DIR$gpkg, "huc_gw_county_pct_blm.gpkg")

out_qa <- file.path(
  DIR$qa,
  timestamped_name("huc_gw_county_pct_blm_qa", "csv")
)

# ==== 5. Check required files =================================================

required_sources <- c(
  SRC$huc2,
  SRC$huc4,
  SRC$huc6,
  SRC$huc8,
  if (PROCESS_DEEP) c(SRC$huc10, SRC$huc12),
  SRC$bull118_gw,
  SRC$counties,
  rds_blm_core
)

missing_sources <- required_sources[!file.exists(required_sources)]

if (length(missing_sources) > 0) {
  stop(
    "Missing required input file(s):\n",
    paste(missing_sources, collapse = "\n")
  )
}

message("All required HUC/GW/county/BLM inputs found.")

# ==== 6. Helper functions ====================================================

## ---- 6.1 Text cleanup -------------------------------------------------------
sanitize_text <- function(x) {
  if (is.character(x)) {
    iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  } else {
    x
  }
}

## ---- 6.2 Case-insensitive field finder -------------------------------------
find_field <- function(actual_names, requested_name, layer_label) {
  
  hit <- actual_names[tolower(actual_names) == tolower(requested_name)]
  
  if (length(hit) == 0) {
    stop(
      "[", layer_label, "] missing expected field: ", requested_name,
      "\nAvailable fields: ",
      paste(actual_names, collapse = ", ")
    )
  }
  
  hit[1]
}

## ---- 6.3 Safe HUC reader ----------------------------------------------------
safe_read_huc <- function(path, id_field, name_field, layer_name) {
  
  message("Reading ", toupper(layer_name), ": ", path)
  
  shp <- sf::st_read(path, quiet = TRUE)
  
  id_actual <- find_field(names(shp), id_field, layer_name)
  name_actual <- find_field(names(shp), name_field, layer_name)
  
  out <- shp |>
    dplyr::transmute(
      !!layer_name := as.character(.data[[id_actual]]),
      name = sanitize_text(.data[[name_actual]])
    )
  
  ## Also create a level-specific name column, e.g. huc8_name.
  ## This makes downstream popup code simpler and more explicit.
  out[[paste0(layer_name, "_name")]] <- out$name
  
  out
}

## ---- 6.4 Safe groundwater basin reader -------------------------------------
safe_read_gw <- function(path) {
  
  message("Reading Bulletin 118 groundwater basins: ", path)
  
  shp <- sf::st_read(path, quiet = TRUE)
  
  subbasin_field <- find_field(names(shp), "Basin_Subb", "Bulletin 118 GW")
  name_field     <- find_field(names(shp), "Basin_Su_1", "Bulletin 118 GW")
  basin_field    <- find_field(names(shp), "Basin_Numb", "Bulletin 118 GW")
  
  shp |>
    dplyr::transmute(
      basin_num     = as.character(.data[[basin_field]]),
      basin_name    = sanitize_text(.data[[name_field]]),
      subbasin_num  = as.character(.data[[subbasin_field]]),
      subbasin_name = sanitize_text(.data[[name_field]])
    ) |>
    dplyr::mutate(
      label = paste0(subbasin_num, " - ", subbasin_name)
    )
}

## ---- 6.5 Safe county reader -------------------------------------------------
safe_read_county <- function(path) {
  
  message("Reading California counties: ", path)
  
  shp <- sf::st_read(path, quiet = TRUE)
  
  name_field <- find_field(names(shp), "COUNTY_NAM", "California counties")
  
  shp |>
    dplyr::transmute(
      county_name = sanitize_text(.data[[name_field]])
    ) |>
    dplyr::group_by(county_name) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
    sf::st_as_sf() |>
    dplyr::mutate(label = paste0(county_name, " County"))
}

## ---- 6.6 Area / %BLM calculator --------------------------------------------
compute_pct_blm <- function(layer_sf, blm_core_3310, layer_label,
                            chunk_n = INTERSECT_CHUNK_N) {
  
  message("Computing %BLM for: ", layer_label)
  
  if (!inherits(layer_sf, "sf")) {
    stop(layer_label, " is not an sf object.")
  }
  
  if (nrow(layer_sf) == 0) {
    warning(layer_label, " has zero rows.")
    return(layer_sf)
  }
  
  ## Disable s2 during planar area/intersection math.
  old_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(FALSE)
  
  ## Transform analysis layers to California Albers.
  layer_3310 <- layer_sf |>
    to_ca_albers() |>
    sf::st_zm(drop = TRUE, what = "ZM") |>
    make_valid_if_needed()
  
  blm_3310 <- blm_core_3310 |>
    to_ca_albers() |>
    sf::st_zm(drop = TRUE, what = "ZM") |>
    make_valid_if_needed()
  
  ## Keep only geometry from the BLM core.
  ## The BLM core should already be one dissolved row, but unioning here makes
  ## the function robust if that ever changes.
  blm_union <- sf::st_union(blm_3310)
  
  blm_sf <- sf::st_sf(
    blm_id = 1,
    geometry = blm_union
  )
  
  sf::st_crs(blm_sf) <- sf::st_crs(layer_3310)
  
  ## Add internal row ID so intersection results can be joined back safely.
  layer_3310$..rowid <- seq_len(nrow(layer_3310))
  
  total_m2 <- as.numeric(sf::st_area(layer_3310))
  blm_m2   <- numeric(nrow(layer_3310))
  
  row_chunks <- split(
    seq_len(nrow(layer_3310)),
    ceiling(seq_len(nrow(layer_3310)) / chunk_n)
  )
  
  for (i in seq_along(row_chunks)) {
    
    idx <- row_chunks[[i]]
    chunk <- layer_3310[idx, , drop = FALSE]
    
    message(
      "  chunk ", i, " of ", length(row_chunks),
      " (", length(idx), " feature(s))"
    )
    
    ## Prefilter to features that touch BLM at all.
    touches_blm <- lengths(sf::st_intersects(chunk, blm_sf)) > 0
    
    if (!any(touches_blm)) next
    
    chunk_touch <- chunk[touches_blm, c("..rowid")]
    
    inter <- suppressWarnings(
      try(sf::st_intersection(chunk_touch, blm_sf), silent = TRUE)
    )
    
    if (inherits(inter, "try-error") || !inherits(inter, "sf") || nrow(inter) == 0) {
      warning("Intersection failed or returned zero rows for chunk ", i)
      next
    }
    
    ## Keep polygonal intersection pieces only.
    inter <- suppressWarnings(sf::st_collection_extract(inter, "POLYGON"))
    inter <- inter[!sf::st_is_empty(inter), , drop = FALSE]
    
    if (nrow(inter) == 0) next
    
    inter$..area_m2 <- as.numeric(sf::st_area(inter))
    
    sums <- inter |>
      sf::st_drop_geometry() |>
      dplyr::group_by(..rowid) |>
      dplyr::summarise(
        blm_area_m2 = sum(..area_m2, na.rm = TRUE),
        .groups = "drop"
      )
    
    blm_m2[sums$..rowid] <- blm_m2[sums$..rowid] + sums$blm_area_m2
  }
  
  layer_3310$total_area_sqmi <- total_m2 / SQMI_M2
  layer_3310$blm_area_sqmi   <- blm_m2 / SQMI_M2
  
  layer_3310$percentBLMland <- dplyr::if_else(
    layer_3310$total_area_sqmi > 0,
    100 * layer_3310$blm_area_sqmi / layer_3310$total_area_sqmi,
    0
  )
  
  ## Clamp tiny numerical artifacts to [0, 100].
  layer_3310$percentBLMland <- pmin(
    pmax(layer_3310$percentBLMland, 0),
    100
  )
  
  layer_3310$percentBLMland <- round(layer_3310$percentBLMland, 2)
  
  layer_3310$..rowid <- NULL
  
  ## Return web-map-friendly WGS84 output.
  layer_3310 |>
    sf::st_transform(4326)
}

## ---- 6.7 HUC parent metadata ------------------------------------------------
add_huc_parent_metadata <- function(huc_all) {
  
  huc_levels <- sort(as.integer(gsub("^huc", "", names(huc_all))))
  
  for (child_level in huc_levels) {
    
    child_name <- paste0("huc", child_level)
    
    if (child_level == 2 || !child_name %in% names(huc_all)) next
    
    child <- huc_all[[child_name]]
    child_code_col <- child_name
    child_codes <- as.character(child[[child_code_col]])
    
    for (parent_level in huc_levels) {
      
      if (parent_level >= child_level) next
      
      parent_name <- paste0("huc", parent_level)
      
      if (!parent_name %in% names(huc_all)) next
      
      parent <- huc_all[[parent_name]] |>
        sf::st_drop_geometry()
      
      parent_code_col <- parent_name
      parent_name_col <- paste0(parent_name, "_name")
      
      parent_codes <- as.character(parent[[parent_code_col]])
      
      new_code_col <- paste0("prnt_huc", parent_level, "_code")
      new_name_col <- paste0("prnt_huc", parent_level, "_name")
      new_pct_col  <- paste0("prnt_huc", parent_level, "_pctBLM")
      new_tot_col  <- paste0("prnt_huc", parent_level, "_tot")
      new_blm_col  <- paste0("prnt_huc", parent_level, "_blm")
      
      child[[new_code_col]] <- substr(child_codes, 1, parent_level)
      
      match_idx <- match(child[[new_code_col]], parent_codes)
      
      child[[new_name_col]] <- parent[[parent_name_col]][match_idx]
      child[[new_pct_col]]  <- parent$percentBLMland[match_idx]
      child[[new_tot_col]]  <- parent$total_area_sqmi[match_idx]
      child[[new_blm_col]]  <- parent$blm_area_sqmi[match_idx]
    }
    
    huc_all[[child_name]] <- child
  }
  
  huc_all
}

## ---- 6.8 QA summary helper --------------------------------------------------
summarize_layer_qa <- function(x, layer_id) {
  
  df <- sf::st_drop_geometry(x)
  
  tibble::tibble(
    layer_id = layer_id,
    rows = nrow(df),
    total_area_sum_mi2 = round(sum(df$total_area_sqmi, na.rm = TRUE), 3),
    blm_area_sum_mi2 = round(sum(df$blm_area_sqmi, na.rm = TRUE), 3),
    pct_min = round(min(df$percentBLMland, na.rm = TRUE), 3),
    pct_mean = round(mean(df$percentBLMland, na.rm = TRUE), 3),
    pct_max = round(max(df$percentBLMland, na.rm = TRUE), 3)
  )
}

# ==== 7. Read input layers ===================================================

blm_core_3310 <- read_rds_checked(rds_blm_core, "BLM managed core")

huc_layers <- list(
  huc2 = safe_read_huc(SRC$huc2, "huc2", "name", "huc2"),
  huc4 = safe_read_huc(SRC$huc4, "huc4", "name", "huc4"),
  huc6 = safe_read_huc(SRC$huc6, "huc6", "name", "huc6"),
  huc8 = safe_read_huc(SRC$huc8, "huc8", "name", "huc8")
)

if (PROCESS_DEEP) {
  huc_layers$huc10 <- safe_read_huc(SRC$huc10, "huc10", "name", "huc10")
  huc_layers$huc12 <- safe_read_huc(SRC$huc12, "huc12", "name", "huc12")
}

gw_basin <- safe_read_gw(SRC$bull118_gw)
county   <- safe_read_county(SRC$counties)

# ==== 8. Compute %BLM ========================================================

huc_all <- purrr::imap(
  huc_layers,
  ~ compute_pct_blm(
    layer_sf = .x,
    blm_core_3310 = blm_core_3310,
    layer_label = toupper(.y)
  )
)

gw_full <- compute_pct_blm(
  layer_sf = gw_basin,
  blm_core_3310 = blm_core_3310,
  layer_label = "Bulletin 118 groundwater basins"
)

county_full <- compute_pct_blm(
  layer_sf = county,
  blm_core_3310 = blm_core_3310,
  layer_label = "California counties"
)

# ==== 9. Add HUC parent metadata ============================================

huc_all <- add_huc_parent_metadata(huc_all)

# ==== 10. Save RDS outputs ===================================================

if (RUN$use_timestamped_outputs) {
  
  save_rds_cached(
    x = huc_all,
    timestamped_path = out_huc_timestamped,
    latest_path = out_huc_latest
  )
  
  save_rds_cached(
    x = gw_full,
    timestamped_path = out_gw_timestamped,
    latest_path = out_gw_latest
  )
  
  save_rds_cached(
    x = county_full,
    timestamped_path = out_county_timestamped,
    latest_path = out_county_latest
  )
  
} else {
  
  saveRDS(huc_all, out_huc_latest)
  saveRDS(gw_full, out_gw_latest)
  saveRDS(county_full, out_county_latest)
  
  message("Saved latest RDS: ", out_huc_latest)
  message("Saved latest RDS: ", out_gw_latest)
  message("Saved latest RDS: ", out_county_latest)
}

# ==== 11. Save GPKG output ===================================================

if (WRITE_GPKG) {
  
  if (file.exists(out_gpkg)) {
    file.remove(out_gpkg)
  }
  
  purrr::iwalk(huc_all, function(layer, layer_name) {
    sf::st_write(
      layer,
      dsn = out_gpkg,
      layer = layer_name,
      quiet = TRUE
    )
  })
  
  sf::st_write(
    gw_full,
    dsn = out_gpkg,
    layer = "bull118gw_full",
    quiet = TRUE
  )
  
  sf::st_write(
    county_full,
    dsn = out_gpkg,
    layer = "county_full",
    quiet = TRUE
  )
  
  message("Saved GPKG: ", out_gpkg)
}

# ==== 12. Save QA output =====================================================

if (WRITE_QA) {
  
  qa_huc <- purrr::imap_dfr(
    huc_all,
    ~ summarize_layer_qa(.x, .y)
  )
  
  qa <- dplyr::bind_rows(
    qa_huc,
    summarize_layer_qa(gw_full, "bull118gw"),
    summarize_layer_qa(county_full, "county")
  ) |>
    dplyr::mutate(
      blm_core_source = basename(rds_blm_core),
      run_timestamp = make_timestamp()
    )
  
  readr::write_csv(qa, out_qa)
  
  message("Saved QA CSV: ", out_qa)
  print(qa)
}

# ==== 13. Final summary ======================================================

message("\nDone: HUC/GW/county %BLM preprocessing complete.")
message("Latest HUC RDS:")
message("  ", out_huc_latest)
message("Latest GW RDS:")
message("  ", out_gw_latest)
message("Latest county RDS:")
message("  ", out_county_latest)

message("\nHUC layers saved:")
print(names(huc_all))

message("\nRows by layer:")
print(purrr::map_int(huc_all, nrow))
message("GW rows:     ", nrow(gw_full))
message("County rows: ", nrow(county_full))