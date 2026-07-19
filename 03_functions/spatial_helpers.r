# ==== spatial_helpers.r ======================================================
##
## PURPOSE:
##   Shared spatial utilities for preprocessing and map-building.
##
## PRINCIPLES:
##   - Heavy spatial math should happen in projected CRS, usually EPSG:3310.
##   - Leaflet display layers should be EPSG:4326.
##   - Geometry validity should be checked in one place, not scattered across
##     every script.
##

# ---- Load required spatial packages ----------------------------------------
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

# ---- Validate only when needed ---------------------------------------------
make_valid_if_needed <- function(x) {
  
  if (!inherits(x, "sf")) return(x)
  
  invalid <- !sf::st_is_valid(x)
  invalid[is.na(invalid)] <- FALSE
  
  if (any(invalid)) {
    message("Repairing invalid geometries: ", sum(invalid), " feature(s)")
    x[invalid, ] <- sf::st_make_valid(x[invalid, ])
  }
  
  x
}

# ---- Clean sf object for web mapping ---------------------------------------
clean_sf_for_leaflet <- function(x) {
  
  if (!inherits(x, "sf")) return(x)
  
  x <- sf::st_zm(x, drop = TRUE, what = "ZM")
  x <- make_valid_if_needed(x)
  
  bad <- sf::st_is_empty(x) |
    sf::st_geometry_type(x) %in% c("GEOMETRY", "GEOMETRYCOLLECTION")
  
  x[!bad, , drop = FALSE]
}

# ---- Transform to WGS84 for Leaflet ----------------------------------------
to_wgs84 <- function(x) {
  
  if (!inherits(x, "sf")) return(x)
  
  crs <- sf::st_crs(x)
  
  if (is.na(crs)) {
    warning("Input CRS is missing; assuming it is already lon/lat.")
    return(clean_sf_for_leaflet(x))
  }
  
  if (sf::st_is_longlat(x)) {
    clean_sf_for_leaflet(x)
  } else {
    clean_sf_for_leaflet(sf::st_transform(x, 4326))
  }
}

# ---- Transform to California Albers for area/intersection math --------------
to_ca_albers <- function(x) {
  
  if (!inherits(x, "sf")) return(x)
  
  x <- make_valid_if_needed(x)
  
  crs <- sf::st_crs(x)
  
  if (is.na(crs)) {
    stop("Cannot transform to EPSG:3310 because input CRS is missing.")
  }
  
  if (!identical(crs$epsg, 3310L)) {
    sf::st_transform(x, 3310)
  } else {
    x
  }
}


# ---- Simplify sf object for web display -------------------------------------
simplify_sf_for_web <- function(x, keep = 0.05, layer_label = "unnamed layer") {
  
  if (!inherits(x, "sf")) return(x)
  
  ## If keep is near 1, skip simplification.
  if (is.null(keep) || is.na(keep) || keep >= 0.999 || nrow(x) == 0) {
    message("Skipping simplification for ", layer_label)
    return(clean_sf_for_leaflet(x))
  }
  
  if (!requireNamespace("rmapshaper", quietly = TRUE)) {
    install.packages("rmapshaper")
  }
  
  message("Simplifying ", layer_label, " with keep = ", keep)
  
  x <- sf::st_zm(x, drop = TRUE, what = "ZM")
  x <- make_valid_if_needed(x)
  
  out <- try(
    rmapshaper::ms_simplify(
      input = x,
      keep = keep,
      keep_shapes = TRUE,
      explode = FALSE
    ),
    silent = TRUE
  )
  
  if (inherits(out, "try-error") || !inherits(out, "sf") || nrow(out) == 0) {
    warning("Simplification failed for ", layer_label, "; returning unsimplified layer.")
    return(clean_sf_for_leaflet(x))
  }
  
  clean_sf_for_leaflet(out)
}