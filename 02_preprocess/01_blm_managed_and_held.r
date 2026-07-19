# ==== 01_blm_managed_and_held.r =============================================
##
## PURPOSE:
##   Create two foundational BLM land-status layers for PortaTreasure2:
##
##   1. BLM-managed core lands:
##      A single dissolved polygon of lands managed by BLM-California.
##
##   2. Held-vs-managed difference layer:
##      A small diagnostic layer showing:
##        - lands held by BLM but not managed by BLM
##        - lands managed by BLM but not held by BLM
##
## INPUT:
##   Defined explicitly in:
##     00_config/config_source_files.r
##
## OUTPUTS:
##   04_processed_data/rds/blm_managed_core_3310.rds
##   04_processed_data/rds/blm_managed_core_3310_<timestamp>.rds
##   04_processed_data/rds/blm_held_vs_managed_diffs_3310.rds
##   04_processed_data/rds/blm_held_vs_managed_diffs_3310_<timestamp>.rds
##   04_processed_data/gpkg/blm_managed_and_held.gpkg
##   04_processed_data/qa/blm_managed_and_held_qa_<timestamp>.csv
##
## IMPORTANT:
##   Heavy area/overlay math is done in EPSG:3310, California Albers.
##   Leaflet display copies can be created later in the map-cache stage.
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
  library(readr)
  library(lwgeom)
  library(terra)
})

# ==== 3. User-facing switches ================================================

WRITE_GPKG <- TRUE
WRITE_QA   <- TRUE

## BLM land-status schema.
## These are expected fields in the federal lands source shapefile.
MNGD_FIELD <- "SMA_ID"
HELD_FIELD <- "HOLD_ID"
BLM_VALUE  <- 2

## Precision snapping is OFF by default.
## Turn on only if there are cosmetic hairline slivers after dissolves.
DO_PRECISION_SNAP <- FALSE
PRECISION_M       <- 0.5

# ==== 4. Define output paths =================================================

out_core_latest <- file.path(
  DIR$rds,
  "blm_managed_core_3310.rds"
)

out_core_timestamped <- file.path(
  DIR$rds,
  timestamped_name("blm_managed_core_3310", "rds")
)

out_diffs_latest <- file.path(
  DIR$rds,
  "blm_held_vs_managed_diffs_3310.rds"
)

out_diffs_timestamped <- file.path(
  DIR$rds,
  timestamped_name("blm_held_vs_managed_diffs_3310", "rds")
)

out_gpkg <- file.path(
  DIR$gpkg,
  "blm_managed_and_held.gpkg"
)

out_qa <- file.path(
  DIR$qa,
  timestamped_name("blm_managed_and_held_qa", "csv")
)

# ==== 5. Check required input =================================================

if (!file.exists(SRC$blm_fedlands)) {
  stop("Missing BLM/federal lands source shapefile: ", SRC$blm_fedlands)
}

message("Using BLM/federal lands source:")
message("  ", SRC$blm_fedlands)

# ==== 6. Read and prepare source layer =======================================

## Heavy overlay math should use planar geometry in California Albers.
old_s2 <- sf::sf_use_s2()
on.exit(sf::sf_use_s2(old_s2), add = TRUE)
sf::sf_use_s2(FALSE)

fed <- sf::st_read(SRC$blm_fedlands, quiet = TRUE) |>
  sf::st_transform(3310) |>
  sf::st_zm(drop = TRUE, what = "ZM") |>
  sf::st_make_valid()

## Confirm the required land-status fields are present.
if (!all(c(MNGD_FIELD, HELD_FIELD) %in% names(fed))) {
  stop(
    "Expected fields not found. Required: ",
    paste(c(MNGD_FIELD, HELD_FIELD), collapse = ", "),
    "\nAvailable fields: ",
    paste(names(fed), collapse = ", ")
  )
}

message("Raw federal lands rows: ", nrow(fed))

# ==== 7. Helper functions ====================================================

clean_for_union <- function(x) {
  
  x |>
    sf::st_collection_extract("POLYGON") |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    sf::st_make_valid() |>
    sf::st_buffer(0)
}

safe_union <- function(x, label = "unnamed layer") {
  
  message("Dissolving: ", label)
  
  x <- clean_for_union(x)
  
  ## Attempt 1: standard sf union.
  out <- try(sf::st_union(x), silent = TRUE)
  
  if (!inherits(out, "try-error")) {
    return(sf::st_make_valid(out))
  }
  
  message("  sf::st_union failed; trying subdivide + union.")
  
  ## Attempt 2: subdivide before union.
  x_sub <- try(
    lwgeom::st_subdivide(x, max_vertices = 2000) |>
      sf::st_collection_extract("POLYGON") |>
      sf::st_make_valid(),
    silent = TRUE
  )
  
  if (!inherits(x_sub, "try-error")) {
    out <- try(sf::st_union(x_sub), silent = TRUE)
    
    if (!inherits(out, "try-error")) {
      return(sf::st_make_valid(out))
    }
  }
  
  message("  subdivide + union failed; trying terra dissolve.")
  
  ## Attempt 3: terra dissolve fallback.
  vx <- terra::vect(sf::st_as_sf(x))
  vt <- terra::aggregate(vx, dissolve = TRUE)
  
  sf::st_as_sf(vt) |>
    sf::st_union() |>
    sf::st_make_valid()
}

safe_difference <- function(a, b, label = "difference") {
  
  message("Computing difference: ", label)
  
  a <- a |> sf::st_make_valid() |> sf::st_buffer(0)
  b <- b |> sf::st_make_valid() |> sf::st_buffer(0)
  
  ## Attempt 1: standard sf difference.
  out <- try(sf::st_difference(a, b), silent = TRUE)
  
  if (!inherits(out, "try-error")) {
    return(sf::st_make_valid(out))
  }
  
  message("  sf::st_difference failed; trying terra::erase fallback.")
  
  ## Attempt 2: terra erase fallback.
  av <- terra::vect(sf::st_as_sf(a))
  bv <- terra::vect(sf::st_as_sf(b))
  tv <- terra::erase(av, bv)
  
  sf::st_as_sf(tv) |>
    sf::st_collection_extract("POLYGON") |>
    sf::st_make_valid()
}

area_mi2 <- function(x) {
  sum(as.numeric(sf::st_area(x)), na.rm = TRUE) / 2.58999e6
}

fmt_mi2 <- function(x, digits = 1) {
  format(
    round(x, digits),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE,
    nsmall = digits
  )
}

to_single_sf <- function(geom, category) {
  
  if (is.null(geom) || length(geom) == 0 || all(sf::st_is_empty(geom))) {
    return(sf::st_sf(
      category = character(0),
      geometry = sf::st_sfc(crs = sf::st_crs(3310))
    ))
  }
  
  sf::st_sf(
    category = category,
    geometry = sf::st_sfc(geom, crs = sf::st_crs(3310))
  )
}

# ==== 8. Split managed and held BLM lands ====================================

managed <- fed |>
  dplyr::filter(suppressWarnings(as.integer(.data[[MNGD_FIELD]])) == BLM_VALUE)

held <- fed |>
  dplyr::filter(suppressWarnings(as.integer(.data[[HELD_FIELD]])) == BLM_VALUE)

message("Rows managed by BLM: ", nrow(managed))
message("Rows held by BLM:    ", nrow(held))

if (nrow(managed) == 0) {
  stop("No BLM-managed rows found. Check MNGD_FIELD and BLM_VALUE.")
}

if (nrow(held) == 0) {
  warning("No BLM-held rows found. Difference layer may be empty.")
}

# ==== 9. Dissolve managed and held lands =====================================

managed_union <- safe_union(managed, "BLM-managed lands")
held_union    <- safe_union(held, "BLM-held lands")

if (DO_PRECISION_SNAP) {
  managed_union <- sf::st_set_precision(managed_union, PRECISION_M)
  held_union    <- sf::st_set_precision(held_union, PRECISION_M)
}

# ==== 10. Build BLM-managed core layer =======================================

managed_area <- area_mi2(managed_union)

blm_managed_core <- to_single_sf(
  geom = managed_union,
  category = paste0("BLM-managed (core) (", fmt_mi2(managed_area, 1), " mi²)")
)

# ==== 11. Build held-vs-managed difference layer =============================

held_not_managed <- safe_difference(
  held_union,
  managed_union,
  label = "held by BLM but not managed by BLM"
)

managed_not_held <- safe_difference(
  managed_union,
  held_union,
  label = "managed by BLM but not held by BLM"
)

held_not_managed_area <- area_mi2(held_not_managed)
managed_not_held_area <- area_mi2(managed_not_held)

diff_held_not_managed <- to_single_sf(
  geom = held_not_managed,
  category = paste0(
    "Unmanaged but held (",
    fmt_mi2(held_not_managed_area, 2),
    " mi²)"
  )
)

diff_managed_not_held <- to_single_sf(
  geom = managed_not_held,
  category = paste0(
    "Managed but not held (",
    fmt_mi2(managed_not_held_area, 2),
    " mi²)"
  )
)

blm_diffs <- rbind(diff_held_not_managed, diff_managed_not_held) |>
  dplyr::filter(!sf::st_is_empty(geometry))

# ==== 12. Save RDS outputs ===================================================

if (RUN$use_timestamped_outputs) {
  
  save_rds_cached(
    x = blm_managed_core,
    timestamped_path = out_core_timestamped,
    latest_path = out_core_latest
  )
  
  save_rds_cached(
    x = blm_diffs,
    timestamped_path = out_diffs_timestamped,
    latest_path = out_diffs_latest
  )
  
} else {
  
  saveRDS(blm_managed_core, out_core_latest)
  saveRDS(blm_diffs, out_diffs_latest)
  
  message("Saved latest RDS: ", out_core_latest)
  message("Saved latest RDS: ", out_diffs_latest)
}

# ==== 13. Save GPKG output ===================================================

if (WRITE_GPKG) {
  
  sf::st_write(
    blm_managed_core,
    dsn = out_gpkg,
    layer = "blm_managed_core_3310",
    delete_layer = TRUE,
    quiet = TRUE
  )
  
  sf::st_write(
    blm_diffs,
    dsn = out_gpkg,
    layer = "blm_held_vs_managed_diffs_3310",
    delete_layer = TRUE,
    quiet = TRUE
  )
  
  message("Saved GPKG: ", out_gpkg)
}

# ==== 14. Save QA output =====================================================

qa <- tibble::tibble(
  check = c(
    "source_file",
    "raw_rows",
    "managed_rows",
    "held_rows",
    "managed_area_mi2",
    "held_not_managed_area_mi2",
    "managed_not_held_area_mi2",
    "core_output_rows",
    "diff_output_rows"
  ),
  value = c(
    basename(SRC$blm_fedlands),
    as.character(nrow(fed)),
    as.character(nrow(managed)),
    as.character(nrow(held)),
    as.character(round(managed_area, 3)),
    as.character(round(held_not_managed_area, 3)),
    as.character(round(managed_not_held_area, 3)),
    as.character(nrow(blm_managed_core)),
    as.character(nrow(blm_diffs))
  )
)

if (WRITE_QA) {
  readr::write_csv(qa, out_qa)
  message("Saved QA CSV: ", out_qa)
}

# ==== 15. Final summary ======================================================

message("\nDone: BLM managed/held preprocessing complete.")
message("Managed core rows: ", nrow(blm_managed_core))
message("Difference rows:   ", nrow(blm_diffs))
message("Managed area:      ", fmt_mi2(managed_area, 1), " mi²")
message("Latest core RDS:")
message("  ", out_core_latest)
message("Latest diffs RDS:")
message("  ", out_diffs_latest)