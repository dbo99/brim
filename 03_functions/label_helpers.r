# ==== label_helpers.r ========================================================
##
## PURPOSE:
##   Build reusable label sf objects for PortaTreasure2.
##
## DESIGN:
##   Labels are built as point layers with:
##     - label_id
##     - parent_group
##     - label_group
##     - label_text
##     - min_zoom / max_zoom
##     - geometry
##
##   The final Leaflet map will add these labels silently, then a custom
##   JavaScript control will show/hide them based on:
##     1. whether the global Labels toggle is on
##     2. whether the parent layer is visible
##     3. whether the current zoom meets the threshold in config_labels.r
##

# ==== 1. Empty label sf helper ===============================================

pt_empty_label_sf <- function() {
  
  sf::st_sf(
    label_id = character(0),
    parent_group = character(0),
    label_group = character(0),
    label_text = character(0),
    min_zoom = numeric(0),
    max_zoom = numeric(0),
    geometry = sf::st_sfc(crs = 4326)
  )
}

# ==== 2. Label config lookup =================================================

pt_label_cfg <- function(label_id, label_zoom = LABEL_ZOOM) {
  
  hit <- label_zoom |>
    dplyr::filter(.data$label_id == !!label_id)
  
  if (nrow(hit) == 0) {
    stop("No LABEL_ZOOM row found for label_id: ", label_id)
  }
  
  hit[1, , drop = FALSE]
}

# ==== 3. Field existence helper ==============================================

pt_first_existing_field <- function(x, fields) {
  
  hit <- fields[fields %in% names(x)]
  
  if (length(hit) == 0) {
    return(NA_character_)
  }
  
  hit[1]
}

# ==== 4. Clean label text ====================================================

pt_clean_label_text <- function(x) {
  
  x <- as.character(x)
  x <- trimws(x)
  x[is.na(x) | x == "" | x == "NA"] <- NA_character_
  x
}

# ==== 5. Polygon label builder ===============================================
##
## Uses st_point_on_surface() rather than centroid, so labels stay inside odd
## polygon shapes as often as possible.
##
## Label points are calculated in EPSG:3310 when possible, then transformed back
## to WGS84 for Leaflet.

pt_make_polygon_labels <- function(x, label_id, label_field,
                                   label_zoom = LABEL_ZOOM) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    message("Skipping polygon labels for ", label_id, ": zero rows.")
    return(pt_empty_label_sf())
  }
  
  cfg <- pt_label_cfg(label_id, label_zoom)
  
  if (!label_field %in% names(x)) {
    message(
      "Skipping polygon labels for ", label_id,
      ": label field not found: ", label_field
    )
    return(pt_empty_label_sf())
  }
  
  labels <- pt_clean_label_text(x[[label_field]])
  keep <- !is.na(labels)
  
  if (!any(keep)) {
    message("Skipping polygon labels for ", label_id, ": no nonblank labels.")
    return(pt_empty_label_sf())
  }
  
  x_keep <- x[keep, , drop = FALSE]
  labels <- labels[keep]
  
  label_pts <- x_keep |>
    sf::st_transform(3310) |>
    sf::st_point_on_surface() |>
    sf::st_transform(4326)
  
  sf::st_sf(
    label_id = label_id,
    parent_group = cfg$parent_group,
    label_group = cfg$label_group,
    label_text = labels,
    min_zoom = cfg$min_zoom,
    max_zoom = cfg$max_zoom,
    geometry = sf::st_geometry(label_pts)
  )
}

# ==== 6. Point label builder ==================================================

pt_make_point_labels <- function(x, label_id, label_field,
                                 label_zoom = LABEL_ZOOM) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    message("Skipping point labels for ", label_id, ": zero rows.")
    return(pt_empty_label_sf())
  }
  
  cfg <- pt_label_cfg(label_id, label_zoom)
  
  if (!label_field %in% names(x)) {
    message(
      "Skipping point labels for ", label_id,
      ": label field not found: ", label_field
    )
    return(pt_empty_label_sf())
  }
  
  labels <- pt_clean_label_text(x[[label_field]])
  keep <- !is.na(labels)
  
  if (!any(keep)) {
    message("Skipping point labels for ", label_id, ": no nonblank labels.")
    return(pt_empty_label_sf())
  }
  
  x_keep <- x[keep, , drop = FALSE]
  labels <- labels[keep]
  
  x_keep <- x_keep |>
    sf::st_transform(4326)
  
  sf::st_sf(
    label_id = label_id,
    parent_group = cfg$parent_group,
    label_group = cfg$label_group,
    label_text = labels,
    min_zoom = cfg$min_zoom,
    max_zoom = cfg$max_zoom,
    geometry = sf::st_geometry(x_keep)
  )
}