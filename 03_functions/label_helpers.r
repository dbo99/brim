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

pt_empty_semantic_label_sf <- function() {
  sf::st_sf(
    label_id = character(0),
    parent_group = character(0),
    label_group = character(0),
    label_text = character(0),
    semantic_feature_key = character(0),
    geometry_key = character(0),
    label_record_key = character(0),
    anchor_strategy = character(0),
    anchor_priority = integer(0),
    anchor_weight = numeric(0),
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

pt_label_geometry_only <- function(x) {
  sf::st_sf(geometry = sf::st_geometry(x))
}

pt_sort_label_sf <- function(x) {
  if (!inherits(x, "sf") || nrow(x) < 2L) return(x)
  coords <- sf::st_coordinates(x)
  if (nrow(coords) != nrow(x)) {
    stop("Label cache rows must each contain exactly one point geometry.")
  }
  keys <- if ("semantic_feature_key" %in% names(x)) {
    list(
      as.character(x$semantic_feature_key),
      as.integer(x$anchor_priority),
      as.character(x$geometry_key),
      as.character(x$label_record_key)
    )
  } else {
    list(as.character(x$label_text), coords[, 1], coords[, 2])
  }
  x[do.call(order, c(keys, list(na.last = TRUE, method = "radix"))), , drop = FALSE]
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
  
  ## Operate on geometry only. This makes the constant-attribute contract
  ## explicit and avoids sf's meaningful warning about attributes on derived
  ## point geometries.
  label_pts <- x_keep |>
    pt_label_geometry_only() |>
    sf::st_transform(3310) |>
    sf::st_point_on_surface() |>
    sf::st_transform(4326)
  
  pt_sort_label_sf(sf::st_sf(
    label_id = label_id,
    parent_group = cfg$parent_group,
    label_group = cfg$label_group,
    label_text = labels,
    min_zoom = cfg$min_zoom,
    max_zoom = cfg$max_zoom,
    geometry = sf::st_geometry(label_pts)
  ))
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
  
  pt_sort_label_sf(sf::st_sf(
    label_id = label_id,
    parent_group = cfg$parent_group,
    label_group = cfg$label_group,
    label_text = labels,
    min_zoom = cfg$min_zoom,
    max_zoom = cfg$max_zoom,
    geometry = sf::st_geometry(x_keep)
  ))
}

# ==== 7. Local Reference semantic labels ====================================

pt_local_reference_label_registration <- function(
    layer_id = NULL,
    source_nickname = NULL,
    registry = LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY) {
  if (!is.data.frame(registry) || !nrow(registry)) {
    stop("Local Reference semantic-label registry is missing or empty.")
  }
  keep <- rep(TRUE, nrow(registry))
  if (!is.null(layer_id)) {
    keep <- keep & registry$layer_id == as.character(layer_id)
  }
  if (!is.null(source_nickname)) {
    keep <- keep & registry$source_nickname == as.character(source_nickname)
  }
  hit <- registry[keep, , drop = FALSE]
  if (nrow(hit) != 1L) {
    stop(
      "Expected exactly one Local Reference label registration; found ",
      nrow(hit), "."
    )
  }
  hit
}

pt_validate_local_reference_label_registration <- function(registration, x) {
  required_registration <- c(
    "layer_id", "source_nickname", "label_id", "semantic_id_field",
    "geometry_id_field", "label_text_field", "anchor_strategy",
    "visible_component_aware", "lbl_available"
  )
  missing_registration <- setdiff(required_registration, names(registration))
  if (nrow(registration) != 1L || length(missing_registration)) {
    stop(
      "Invalid Local Reference label registration; missing: ",
      paste(missing_registration, collapse = ", ")
    )
  }
  required_fields <- unname(unlist(registration[1, c(
    "semantic_id_field", "geometry_id_field", "label_text_field"
  )], use.names = FALSE))
  missing_fields <- setdiff(required_fields, names(x))
  if (!inherits(x, "sf") || !nrow(x) || length(missing_fields)) {
    stop(
      "Registered Local Reference label source is invalid; missing fields: ",
      paste(missing_fields, collapse = ", ")
    )
  }
  invisible(TRUE)
}

pt_local_reference_label_source <- function(x, registration) {
  pt_validate_local_reference_label_registration(registration, x)
  semantic_field <- as.character(registration$semantic_id_field[[1]])
  geometry_field <- as.character(registration$geometry_id_field[[1]])
  label_field <- as.character(registration$label_text_field[[1]])
  out <- sf::st_sf(
    semantic_feature_key = trimws(as.character(x[[semantic_field]])),
    geometry_key = trimws(as.character(x[[geometry_field]])),
    label_text = pt_clean_label_text(x[[label_field]]),
    geometry = sf::st_geometry(x)
  )
  bad <- !nzchar(out$semantic_feature_key) |
    !nzchar(out$geometry_key) | is.na(out$label_text)
  if (any(bad)) {
    stop(
      registration$source_nickname[[1]],
      " label source has blank semantic IDs, geometry IDs, or public labels."
    )
  }
  label_counts <- vapply(
    split(out$label_text, out$semantic_feature_key),
    function(values) length(unique(values)),
    integer(1)
  )
  if (any(label_counts != 1L)) {
    stop(
      registration$source_nickname[[1]],
      " has inconsistent public label text within a semantic feature."
    )
  }
  out
}

pt_make_semantic_polygon_anchors <- function(source, visible_component_aware) {
  projected <- sf::st_transform(pt_label_geometry_only(source), 3310)
  area <- as.numeric(sf::st_area(projected))
  semantic_keys <- unique(source$semantic_feature_key)

  if (isTRUE(visible_component_aware)) {
    points <- sf::st_point_on_surface(projected)
    priorities <- integer(nrow(source))
    for (semantic_key in semantic_keys) {
      index <- which(source$semantic_feature_key == semantic_key)
      ranked <- order(
        -area[index], source$geometry_key[index],
        method = "radix"
      )
      priorities[index[ranked]] <- seq_along(index)
    }
    return(list(
      source = source,
      points = sf::st_transform(points, 4326),
      priority = priorities,
      weight = area
    ))
  }

  rows <- lapply(semantic_keys, function(semantic_key) {
    index <- which(source$semantic_feature_key == semantic_key)
    ranked <- order(-area[index], source$geometry_key[index], method = "radix")
    representative <- index[ranked[[1]]]
    union_geometry <- sf::st_union(sf::st_geometry(projected[index, , drop = FALSE]))
    point <- sf::st_point_on_surface(union_geometry)
    list(
      semantic_feature_key = semantic_key,
      geometry_key = source$geometry_key[[representative]],
      label_text = source$label_text[[representative]],
      anchor_weight = sum(area[index]),
      geometry = point[[1]]
    )
  })
  reduced <- sf::st_sf(
    semantic_feature_key = vapply(rows, `[[`, character(1), "semantic_feature_key"),
    geometry_key = vapply(rows, `[[`, character(1), "geometry_key"),
    label_text = vapply(rows, `[[`, character(1), "label_text"),
    geometry = sf::st_sfc(lapply(rows, `[[`, "geometry"), crs = 3310)
  )
  list(
    source = reduced,
    points = sf::st_transform(pt_label_geometry_only(reduced), 4326),
    priority = rep.int(1L, nrow(reduced)),
    weight = vapply(rows, `[[`, numeric(1), "anchor_weight")
  )
}

pt_make_semantic_polygon_largest_component_anchors <- function(source) {
  projected <- sf::st_transform(pt_label_geometry_only(source), 3310)
  area <- as.numeric(sf::st_area(projected))
  semantic_keys <- unique(source$semantic_feature_key)
  selected <- vapply(semantic_keys, function(semantic_key) {
    index <- which(source$semantic_feature_key == semantic_key)
    ranked <- order(-area[index], source$geometry_key[index], method = "radix")
    index[ranked[[1]]]
  }, integer(1))
  reduced <- source[selected, , drop = FALSE]
  points <- suppressWarnings(sf::st_point_on_surface(projected[selected, , drop = FALSE]))
  list(
    source = reduced,
    points = sf::st_transform(points, 4326),
    priority = rep.int(1L, nrow(reduced)),
    weight = area[selected]
  )
}

pt_make_semantic_line_anchors <- function(source) {
  projected <- sf::st_transform(pt_label_geometry_only(source), 3310)
  semantic_keys <- unique(source$semantic_feature_key)
  rows <- lapply(semantic_keys, function(semantic_key) {
    source_index <- which(source$semantic_feature_key == semantic_key)
    candidates <- list()
    for (index in source_index) {
      parts <- suppressWarnings(sf::st_cast(
        sf::st_geometry(projected[index, , drop = FALSE]),
        "LINESTRING"
      ))
      if (!length(parts)) next
      lengths <- as.numeric(sf::st_length(parts))
      for (part_index in seq_along(parts)) {
        candidates[[length(candidates) + 1L]] <- list(
          geometry_key = source$geometry_key[[index]],
          label_text = source$label_text[[index]],
          part_index = part_index,
          length = lengths[[part_index]],
          geometry = parts[[part_index]]
        )
      }
    }
    if (!length(candidates)) {
      stop("No line component available for semantic label: ", semantic_key)
    }
    candidate_order <- order(
      -vapply(candidates, `[[`, numeric(1), "length"),
      vapply(candidates, `[[`, character(1), "geometry_key"),
      vapply(candidates, `[[`, integer(1), "part_index"),
      method = "radix"
    )
    chosen <- candidates[[candidate_order[[1]]]]
    sampled <- sf::st_line_sample(
      sf::st_sfc(chosen$geometry, crs = 3310),
      sample = 0.5
    )
    point <- suppressWarnings(sf::st_cast(sampled, "POINT"))
    if (!length(point)) {
      stop("Could not derive line midpoint for semantic label: ", semantic_key)
    }
    list(
      semantic_feature_key = semantic_key,
      geometry_key = chosen$geometry_key,
      label_text = chosen$label_text,
      anchor_weight = chosen$length,
      geometry = point[[1]]
    )
  })
  reduced <- sf::st_sf(
    semantic_feature_key = vapply(rows, `[[`, character(1), "semantic_feature_key"),
    geometry_key = vapply(rows, `[[`, character(1), "geometry_key"),
    label_text = vapply(rows, `[[`, character(1), "label_text"),
    geometry = sf::st_sfc(lapply(rows, `[[`, "geometry"), crs = 3310)
  )
  list(
    source = reduced,
    points = sf::st_transform(pt_label_geometry_only(reduced), 4326),
    priority = rep.int(1L, nrow(reduced)),
    weight = vapply(rows, `[[`, numeric(1), "anchor_weight")
  )
}

pt_make_local_reference_labels <- function(
    x,
    registration,
    label_zoom = LABEL_ZOOM) {
  source <- pt_local_reference_label_source(x, registration)
  strategy <- as.character(registration$anchor_strategy[[1]])
  visible_component_aware <- isTRUE(registration$visible_component_aware[[1]])
  anchor_result <- switch(
    strategy,
    polygon_semantic_point_on_surface =
      pt_make_semantic_polygon_anchors(source, FALSE),
    polygon_visible_component_point_on_surface =
      pt_make_semantic_polygon_anchors(source, TRUE),
    polygon_semantic_largest_component_point_on_surface =
      pt_make_semantic_polygon_largest_component_anchors(source),
    line_semantic_longest_component_midpoint =
      pt_make_semantic_line_anchors(source),
    stop("Unsupported Local Reference label anchor strategy: ", strategy)
  )
  if (!identical(
    visible_component_aware,
    identical(strategy, "polygon_visible_component_point_on_surface")
  )) {
    stop("Visible-component-aware registration does not match its anchor strategy.")
  }
  label_id <- as.character(registration$label_id[[1]])
  cfg <- pt_label_cfg(label_id, label_zoom)
  anchor_source <- anchor_result$source
  record_keys <- paste(
    label_id,
    anchor_source$semantic_feature_key,
    anchor_source$geometry_key,
    sep = "::"
  )
  if (anyDuplicated(record_keys)) {
    stop("Local Reference label record keys must be unique: ", label_id)
  }
  out <- sf::st_sf(
    label_id = rep(label_id, nrow(anchor_source)),
    parent_group = rep(as.character(cfg$parent_group[[1]]), nrow(anchor_source)),
    label_group = rep(as.character(cfg$label_group[[1]]), nrow(anchor_source)),
    label_text = anchor_source$label_text,
    semantic_feature_key = anchor_source$semantic_feature_key,
    geometry_key = anchor_source$geometry_key,
    label_record_key = record_keys,
    anchor_strategy = rep(strategy, nrow(anchor_source)),
    anchor_priority = as.integer(anchor_result$priority),
    anchor_weight = as.numeric(anchor_result$weight),
    min_zoom = rep(as.numeric(cfg$min_zoom[[1]]), nrow(anchor_source)),
    max_zoom = rep(as.numeric(cfg$max_zoom[[1]]), nrow(anchor_source)),
    geometry = sf::st_geometry(anchor_result$points)
  )
  pt_sort_label_sf(out)
}

pt_validate_local_reference_label_anchors <- function(
    labels,
    x,
    registration,
    tolerance_m = 0.5) {
  pt_validate_local_reference_label_registration(registration, x)
  required <- c(
    "semantic_feature_key", "geometry_key", "label_record_key",
    "anchor_priority", "anchor_strategy"
  )
  missing <- setdiff(required, names(labels))
  if (!inherits(labels, "sf") || !nrow(labels) || length(missing)) {
    stop(
      "Registered Local Reference labels are invalid; missing: ",
      paste(missing, collapse = ", ")
    )
  }
  source <- pt_local_reference_label_source(x, registration)
  source_projected <- sf::st_transform(pt_label_geometry_only(source), 3310)
  labels_projected <- sf::st_transform(pt_label_geometry_only(labels), 3310)
  visible_component_aware <- isTRUE(
    registration$visible_component_aware[[1]]
  )
  distance_m <- vapply(seq_len(nrow(labels)), function(index) {
    matches <- if (visible_component_aware) {
      which(source$geometry_key == labels$geometry_key[[index]])
    } else {
      which(
        source$semantic_feature_key == labels$semantic_feature_key[[index]]
      )
    }
    if (!length(matches)) return(Inf)
    min(as.numeric(sf::st_distance(
      labels_projected[index, , drop = FALSE],
      source_projected[matches, , drop = FALSE]
    )))
  }, numeric(1))
  semantic_count <- length(unique(labels$semantic_feature_key))
  expected_semantic_count <- length(unique(source$semantic_feature_key))
  expected_anchor_count <- if (visible_component_aware) {
    nrow(source)
  } else {
    expected_semantic_count
  }
  failures <- which(!is.finite(distance_m) | distance_m > tolerance_m)
  if (semantic_count != expected_semantic_count ||
      nrow(labels) != expected_anchor_count || length(failures)) {
    stop(
      registration$source_nickname[[1]],
      " semantic-label anchor contract failed: semantic ",
      semantic_count, "/", expected_semantic_count,
      ", anchors ", nrow(labels), "/", expected_anchor_count,
      ", off-geometry anchors ", length(failures), "."
    )
  }
  data.frame(
    layer_id = as.character(registration$layer_id[[1]]),
    label_id = as.character(registration$label_id[[1]]),
    anchor_strategy = as.character(registration$anchor_strategy[[1]]),
    visible_component_aware = visible_component_aware,
    semantic_count = semantic_count,
    anchor_count = nrow(labels),
    max_anchor_distance_m = max(distance_m),
    tolerance_m = as.numeric(tolerance_m),
    stringsAsFactors = FALSE
  )
}

pt_build_registered_local_reference_label_children <- function(
    reference_layers,
    registry = LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY) {
  if (!is.list(reference_layers) || !is.data.frame(registry)) {
    stop("Registered Local Reference label generation requires list inputs.")
  }
  out <- list()
  for (index in seq_len(nrow(registry))) {
    registration <- registry[index, , drop = FALSE]
    nickname <- as.character(registration$source_nickname[[1]])
    if (!isTRUE(registration$lbl_available[[1]]) ||
        !nickname %in% names(reference_layers)) {
      next
    }
    labels <- pt_make_local_reference_labels(
      reference_layers[[nickname]],
      registration
    )
    pt_validate_local_reference_label_anchors(
      labels,
      reference_layers[[nickname]],
      registration
    )
    out[[nickname]] <- labels
  }
  out
}
