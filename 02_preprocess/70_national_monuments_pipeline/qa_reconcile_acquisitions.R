#!/usr/bin/env Rscript

# Tiered reconciliation of two completed National Monuments acquisitions.
#
# Acceptance hierarchy:
#   1. Semantically identical requests must have byte-identical raw responses.
#   2. Counts, IDs, null state, categorical values, dates, and structure match.
#   3. Floating numeric attributes and coordinates use conservative tolerances.
#   4. Derived area, perimeter, and projected bounds use metric tolerances.

NMQ_NUMERIC_ABS_TOLERANCE <- 1e-12
NMQ_NUMERIC_REL_TOLERANCE <- 1e-12
NMQ_NATIVE_PROJECTED_TOLERANCE_METERS <- 1e-6
NMQ_GEOGRAPHIC_TOLERANCE_DEGREES <- 1e-10
NMQ_AREA_ABS_TOLERANCE_SQUARE_METERS <- 0.01
NMQ_AREA_REL_TOLERANCE <- 1e-10
NMQ_LENGTH_ABS_TOLERANCE_METERS <- 1e-4
NMQ_LENGTH_REL_TOLERANCE <- 1e-10
NMQ_BOUNDS_TOLERANCE_METERS <- 1e-6
NMQ_GEOGRAPHIC_WKIDS <- c(4269L, 4326L)

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || all(is.na(x))) y else x
}

nmq_stop <- function(...) stop(paste0(...), call. = FALSE)

nmq_require_packages <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    nmq_stop("Missing required R package(s): ", paste(missing, collapse = ", "))
  }
}

nmq_read_json <- function(path) {
  if (!file.exists(path)) nmq_stop("Missing JSON file: ", path)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

nmq_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

nmq_scalar_text <- function(value) {
  if (is.null(value) || !length(value) || is.na(value[[1]])) return(NA_character_)
  value <- value[[1]]
  if (is.numeric(value) && is.finite(value) && value == trunc(value)) {
    return(format(value, scientific = FALSE, trim = TRUE))
  }
  as.character(value)
}

nmq_normalized_text <- function(value) {
  value <- as.character(value %||% "")
  value <- chartr(
    "áàâäãåÁÀÂÄÃÅéèêëÉÈÊËíìîïÍÌÎÏóòôöõÓÒÔÖÕúùûüÚÙÛÜñÑçÇýÿÝ",
    "aaaaaaAAAAAAeeeeEEEEiiiiIIIIoooooOOOOOuuuuUUUUnNcCyyY",
    value
  )
  value <- iconv(value, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  value[is.na(value)] <- ""
  trimws(gsub("[^a-z0-9]+", " ", tolower(value), perl = TRUE))
}

nmq_field_value <- function(attributes, field) {
  index <- match(tolower(field), tolower(names(attributes)))
  if (is.na(index)) nmq_stop("Missing field ", field)
  attributes[[index]]
}

nmq_source_map <- function(manifest) {
  output <- list()
  for (source in manifest$sources %||% list()) {
    output[[as.character(source$source_key)]] <- source
  }
  output
}

nmq_assert_completed <- function(snapshot) {
  complete_path <- file.path(snapshot, "COMPLETE.json")
  failed_path <- file.path(snapshot, "FAILED.json")
  if (!file.exists(complete_path)) nmq_stop("Snapshot has no COMPLETE.json: ", snapshot)
  if (file.exists(failed_path)) nmq_stop("Snapshot also has FAILED.json: ", snapshot)
  complete <- nmq_read_json(complete_path)
  manifest <- nmq_read_json(file.path(snapshot, "acquisition_manifest.json"))
  if (!identical(complete$status, "PASS") || !identical(manifest$status, "PASS")) {
    nmq_stop("Snapshot is not PASS: ", snapshot)
  }
  manifest
}

nmq_numeric_close <- function(left, right, absolute_tolerance, relative_tolerance) {
  difference <- abs(left - right)
  scale <- pmax(abs(left), abs(right))
  difference <= absolute_tolerance + relative_tolerance * scale
}

nmq_normalize_request_value <- function(value) {
  if (is.list(value)) {
    if (!is.null(names(value))) value <- value[order(names(value), method = "radix")]
    return(lapply(value, nmq_normalize_request_value))
  }
  as.character(value)
}

nmq_request_contract <- function(request) {
  parameters <- request$parameters %||% list()
  # returnIdsOnly already precludes geometry. Treat an explicit false value as
  # semantically equivalent to omission, while retaining literal-URL reporting.
  if (identical(tolower(as.character(parameters$returnIdsOnly %||% "")), "true") &&
      identical(tolower(as.character(parameters$returnGeometry %||% "")), "false")) {
    parameters$returnGeometry <- NULL
  }
  if (length(parameters)) parameters <- parameters[order(names(parameters), method = "radix")]
  list(
    method = toupper(as.character(request$method)),
    base_url = sub("/+$", "", as.character(request$base_url)),
    parameters = lapply(parameters, nmq_normalize_request_value)
  )
}

nmq_canonical_json <- function(value) {
  if (is.numeric(value)) {
    return(vapply(value, function(number) {
      if (is.na(number)) return(NA_character_)
      format(number, digits = 17, scientific = FALSE, trim = TRUE)
    }, character(1)))
  }
  if (!is.list(value)) return(value)
  if (!is.null(names(value))) value <- value[order(names(value), method = "radix")]
  lapply(value, nmq_canonical_json)
}

nmq_raw_path_for_stem <- function(snapshot, source_key, stem) {
  raw_dir <- file.path(snapshot, source_key, "raw")
  candidates <- list.files(
    raw_dir,
    pattern = paste0("^", stem, "\\.(json|geojson)$"),
    full.names = TRUE
  )
  if (length(candidates) != 1L) {
    nmq_stop("Expected one raw response for ", source_key, "/", stem)
  }
  candidates[[1]]
}

nmq_compare_raw_responses <- function(reference_root, candidate_root, source_key) {
  reference_request_dir <- file.path(reference_root, source_key, "requests")
  candidate_request_dir <- file.path(candidate_root, source_key, "requests")
  reference_files <- list.files(
    reference_request_dir, pattern = "\\.request\\.json$", full.names = TRUE
  )
  candidate_files <- list.files(
    candidate_request_dir, pattern = "\\.request\\.json$", full.names = TRUE
  )
  reference_names <- sort(basename(reference_files), method = "radix")
  candidate_names <- sort(basename(candidate_files), method = "radix")
  request_set_equal <- identical(reference_names, candidate_names)
  comparisons <- list()
  if (request_set_equal) {
    for (name in reference_names) {
      reference_request <- nmq_read_json(file.path(reference_request_dir, name))
      candidate_request <- nmq_read_json(file.path(candidate_request_dir, name))
      request_equal <- identical(
        nmq_request_contract(reference_request),
        nmq_request_contract(candidate_request)
      )
      literal_request_equal <- identical(
        toupper(as.character(reference_request$method)),
        toupper(as.character(candidate_request$method))
      ) && identical(
        as.character(reference_request$requested_url),
        as.character(candidate_request$requested_url)
      )
      stem <- sub("\\.request\\.json$", "", name)
      reference_response <- nmq_raw_path_for_stem(reference_root, source_key, stem)
      candidate_response <- nmq_raw_path_for_stem(candidate_root, source_key, stem)
      reference_hash <- nmq_sha256(reference_response)
      candidate_hash <- nmq_sha256(candidate_response)
      hash_equal <- identical(reference_hash, candidate_hash)
      parsed_equal <- identical(
        nmq_canonical_json(nmq_read_json(reference_response)),
        nmq_canonical_json(nmq_read_json(candidate_response))
      )
      comparisons[[length(comparisons) + 1L]] <- list(
        request = name,
        semantically_equivalent_request = request_equal,
        literal_requested_url_equal = literal_request_equal,
        response_role = if (grepl("native_geometry", name, fixed = TRUE)) {
          "native_geometry"
        } else if (grepl("geojson_geometry_epsg4326", name, fixed = TRUE)) {
          "epsg4326_geometry"
        } else {
          "metadata_count_id_or_attribute_discovery"
        },
        reference_response_sha256 = reference_hash,
        candidate_response_sha256 = candidate_hash,
        raw_response_hash_equal = hash_equal,
        parsed_response_semantically_equal = parsed_equal
      )
    }
  }
  definitions_equal <- request_set_equal && all(vapply(comparisons, function(item) {
    isTRUE(item$semantically_equivalent_request)
  }, logical(1)))
  literal <- Filter(function(item) isTRUE(item$literal_requested_url_equal), comparisons)
  literal_hashes_equal <- length(literal) > 0L && all(vapply(literal, function(item) {
    isTRUE(item$raw_response_hash_equal)
  }, logical(1)))
  semantic_payloads_equal <- request_set_equal && all(vapply(comparisons, function(item) {
    isTRUE(item$parsed_response_semantically_equal)
  }, logical(1)))
  geometry <- Filter(function(item) {
    item$response_role %in% c("native_geometry", "epsg4326_geometry")
  }, comparisons)
  list(
    request_count_reference = length(reference_names),
    request_count_candidate = length(candidate_names),
    request_set_equal = request_set_equal,
    semantically_equivalent_request_definitions = definitions_equal,
    exact_literal_request_count = length(literal),
    all_exact_literal_request_raw_hashes_equal = literal_hashes_equal,
    all_parsed_response_payloads_semantically_equal = semantic_payloads_equal,
    total_raw_hash_equal_count = sum(vapply(comparisons, function(item) {
      isTRUE(item$raw_response_hash_equal)
    }, logical(1))),
    geometry_response_count = length(geometry),
    all_geometry_raw_response_hashes_equal = length(geometry) == 2L &&
      all(vapply(geometry, function(item) isTRUE(item$raw_response_hash_equal), logical(1))),
    comparisons = comparisons
  )
}

nmq_feature_map <- function(features, oid_field, geojson) {
  output <- list()
  for (feature in features) {
    attributes <- if (geojson) feature$properties else feature$attributes
    oid <- nmq_scalar_text(nmq_field_value(attributes, oid_field))
    if (is.na(oid) || !nzchar(oid)) nmq_stop("Blank object ID in feature set")
    if (!is.null(output[[oid]])) nmq_stop("Duplicate object ID: ", oid)
    output[[oid]] <- feature
  }
  output
}

nmq_field_types <- function(native_object) {
  output <- character()
  for (field in native_object$fields %||% list()) {
    output[[tolower(as.character(field$name))]] <- as.character(field$type)
  }
  output
}

nmq_compare_attributes <- function(reference, candidate, field_types, oid_field) {
  reference_names <- sort(names(reference), method = "radix")
  candidate_names <- sort(names(candidate), method = "radix")
  fields_equal <- identical(reference_names, candidate_names)
  exact_equal <- fields_equal
  numeric_within_tolerance <- fields_equal
  null_state_equal <- fields_equal
  mismatches <- list()
  max_absolute_difference <- 0
  max_relative_difference <- 0
  if (!fields_equal) {
    return(list(
      field_sets_equal = FALSE,
      null_state_equal = FALSE,
      identity_categorical_date_exact = FALSE,
      numeric_within_tolerance = FALSE,
      max_numeric_absolute_difference = NA_real_,
      max_numeric_relative_difference = NA_real_,
      mismatches = list(list(
        field = "<schema>",
        reference_only = as.list(setdiff(reference_names, candidate_names)),
        candidate_only = as.list(setdiff(candidate_names, reference_names))
      ))
    ))
  }

  exact_numeric_types <- c(
    "esriFieldTypeOID", "esriFieldTypeInteger", "esriFieldTypeSmallInteger",
    "esriFieldTypeDate", "esriFieldTypeBigInteger"
  )
  tolerant_numeric_types <- c("esriFieldTypeDouble", "esriFieldTypeSingle")
  for (field in reference_names) {
    left <- reference[[field]]
    right <- candidate[[field]]
    left_null <- is.null(left) || !length(left) || all(is.na(left))
    right_null <- is.null(right) || !length(right) || all(is.na(right))
    if (!identical(left_null, right_null)) {
      null_state_equal <- FALSE
      exact_equal <- FALSE
      numeric_within_tolerance <- FALSE
      mismatches[[length(mismatches) + 1L]] <- list(
        field = field, issue = "null_state_mismatch"
      )
      next
    }
    if (left_null) next
    type <- unname(field_types[[tolower(field)]]) %||% ""
    both_numeric <- is.numeric(left) && is.numeric(right)
    if (type %in% tolerant_numeric_types || (both_numeric && !type %in% exact_numeric_types)) {
      left_number <- as.numeric(left[[1]])
      right_number <- as.numeric(right[[1]])
      difference <- abs(left_number - right_number)
      relative <- difference / max(abs(left_number), abs(right_number), .Machine$double.eps)
      max_absolute_difference <- max(max_absolute_difference, difference)
      max_relative_difference <- max(max_relative_difference, relative)
      close <- nmq_numeric_close(
        left_number,
        right_number,
        NMQ_NUMERIC_ABS_TOLERANCE,
        NMQ_NUMERIC_REL_TOLERANCE
      )
      if (!close) {
        numeric_within_tolerance <- FALSE
        mismatches[[length(mismatches) + 1L]] <- list(
          field = field,
          issue = "numeric_tolerance_exceeded",
          absolute_difference = difference,
          relative_difference = relative
        )
      }
    } else {
      equal <- identical(nmq_scalar_text(left), nmq_scalar_text(right))
      if (!equal) {
        exact_equal <- FALSE
        mismatches[[length(mismatches) + 1L]] <- list(
          field = field,
          issue = if (identical(tolower(field), tolower(oid_field))) {
            "object_id_mismatch"
          } else {
            "identity_categorical_or_date_mismatch"
          },
          reference = nmq_scalar_text(left),
          candidate = nmq_scalar_text(right)
        )
      }
    }
  }
  list(
    field_sets_equal = fields_equal,
    null_state_equal = null_state_equal,
    identity_categorical_date_exact = exact_equal,
    numeric_within_tolerance = numeric_within_tolerance,
    max_numeric_absolute_difference = max_absolute_difference,
    max_numeric_relative_difference = max_relative_difference,
    mismatches = mismatches
  )
}

nmq_same_structure <- function(left, right) {
  if (is.list(left) != is.list(right)) return(FALSE)
  if (!is.list(left)) return(length(left) == length(right))
  if (length(left) != length(right)) return(FALSE)
  left_named <- !is.null(names(left))
  right_named <- !is.null(names(right))
  if (!identical(left_named, right_named)) return(FALSE)
  if (left_named) {
    if (!setequal(names(left), names(right))) return(FALSE)
    left <- left[sort(names(left), method = "radix")]
    right <- right[sort(names(right), method = "radix")]
  }
  if (!length(left)) return(TRUE)
  all(mapply(nmq_same_structure, left, right, SIMPLIFY = TRUE, USE.NAMES = FALSE))
}

nmq_coordinates <- function(geometry, geojson) {
  if (is.null(geometry)) return(NULL)
  if (geojson) return(geometry$coordinates)
  geometry$rings %||% geometry$curveRings %||%
    geometry$paths %||% geometry$points %||% geometry$coordinates
}

nmq_geometry_counts <- function(geometry, geojson) {
  coordinates <- nmq_coordinates(geometry, geojson)
  if (is.null(coordinates)) {
    return(list(parts = 0L, rings = 0L, coordinate_values = 0L))
  }
  if (!geojson) {
    return(list(
      parts = length(coordinates),
      rings = length(coordinates),
      coordinate_values = length(unlist(coordinates, use.names = FALSE))
    ))
  }
  type <- as.character(geometry$type %||% "")
  if (identical(type, "Polygon")) {
    parts <- 1L
    rings <- length(coordinates)
  } else if (identical(type, "MultiPolygon")) {
    parts <- length(coordinates)
    rings <- sum(vapply(coordinates, length, integer(1)))
  } else {
    parts <- length(coordinates)
    rings <- NA_integer_
  }
  list(
    parts = parts,
    rings = rings,
    coordinate_values = length(unlist(coordinates, use.names = FALSE))
  )
}

nmq_compare_geometry <- function(reference, candidate, geojson, tolerance) {
  reference_null <- is.null(reference)
  candidate_null <- is.null(candidate)
  null_state_equal <- identical(reference_null, candidate_null)
  if (!null_state_equal || reference_null) {
    return(list(
      null_empty_state_equal = null_state_equal,
      geometry_type_equal = null_state_equal,
      coordinate_structure_equal = null_state_equal,
      part_ring_coordinate_counts_equal = null_state_equal,
      reference_counts = nmq_geometry_counts(reference, geojson),
      candidate_counts = nmq_geometry_counts(candidate, geojson),
      max_absolute_coordinate_difference = if (null_state_equal) 0 else NA_real_,
      coordinate_tolerance = tolerance,
      coordinates_within_tolerance = null_state_equal
    ))
  }
  reference_coordinates <- nmq_coordinates(reference, geojson)
  candidate_coordinates <- nmq_coordinates(candidate, geojson)
  reference_empty <- is.null(reference_coordinates) || !length(reference_coordinates)
  candidate_empty <- is.null(candidate_coordinates) || !length(candidate_coordinates)
  empty_equal <- identical(reference_empty, candidate_empty)
  type_equal <- if (geojson) {
    identical(as.character(reference$type), as.character(candidate$type))
  } else {
    setequal(names(reference), names(candidate))
  }
  structure_equal <- empty_equal && nmq_same_structure(
    reference_coordinates, candidate_coordinates
  )
  reference_counts <- nmq_geometry_counts(reference, geojson)
  candidate_counts <- nmq_geometry_counts(candidate, geojson)
  counts_equal <- identical(reference_counts, candidate_counts)
  max_difference <- NA_real_
  within <- FALSE
  if (structure_equal) {
    left <- as.numeric(unlist(reference_coordinates, use.names = FALSE))
    right <- as.numeric(unlist(candidate_coordinates, use.names = FALSE))
    max_difference <- if (length(left)) max(abs(left - right)) else 0
    within <- is.finite(max_difference) && max_difference <= tolerance
  }
  list(
    null_empty_state_equal = null_state_equal && empty_equal,
    geometry_type_equal = type_equal,
    coordinate_structure_equal = structure_equal,
    part_ring_coordinate_counts_equal = counts_equal,
    reference_counts = reference_counts,
    candidate_counts = candidate_counts,
    max_absolute_coordinate_difference = max_difference,
    coordinate_tolerance = tolerance,
    coordinates_within_tolerance = within
  )
}

nmq_compare_feature_file <- function(
  reference_path,
  candidate_path,
  oid_field,
  geojson,
  coordinate_tolerance
) {
  reference_object <- nmq_read_json(reference_path)
  candidate_object <- nmq_read_json(candidate_path)
  field_types <- nmq_field_types(if (geojson) {
    nmq_read_json(sub(
      "scoped_features_epsg4326\\.geojson$",
      "scoped_features_native_arcgis.json",
      reference_path
    ))
  } else reference_object)
  reference_map <- nmq_feature_map(reference_object$features %||% list(), oid_field, geojson)
  candidate_map <- nmq_feature_map(candidate_object$features %||% list(), oid_field, geojson)
  reference_ids <- sort(names(reference_map), method = "radix")
  candidate_ids <- sort(names(candidate_map), method = "radix")
  ids_equal <- identical(reference_ids, candidate_ids)
  attribute_results <- list()
  geometry_results <- list()
  if (ids_equal) {
    for (oid in reference_ids) {
      reference_attributes <- if (geojson) {
        reference_map[[oid]]$properties
      } else {
        reference_map[[oid]]$attributes
      }
      candidate_attributes <- if (geojson) {
        candidate_map[[oid]]$properties
      } else {
        candidate_map[[oid]]$attributes
      }
      attribute_results[[oid]] <- nmq_compare_attributes(
        reference_attributes, candidate_attributes, field_types, oid_field
      )
      geometry_results[[oid]] <- nmq_compare_geometry(
        reference_map[[oid]]$geometry,
        candidate_map[[oid]]$geometry,
        geojson,
        coordinate_tolerance
      )
    }
  }
  attribute_ok <- ids_equal && all(vapply(attribute_results, function(result) {
    isTRUE(result$field_sets_equal) && isTRUE(result$null_state_equal) &&
      isTRUE(result$identity_categorical_date_exact) &&
      isTRUE(result$numeric_within_tolerance)
  }, logical(1)))
  geometry_ok <- ids_equal && all(vapply(geometry_results, function(result) {
    isTRUE(result$null_empty_state_equal) && isTRUE(result$geometry_type_equal) &&
      isTRUE(result$coordinate_structure_equal) &&
      isTRUE(result$part_ring_coordinate_counts_equal) &&
      isTRUE(result$coordinates_within_tolerance)
  }, logical(1)))
  list(
    reference_feature_count = length(reference_map),
    candidate_feature_count = length(candidate_map),
    source_ids_exact = ids_equal,
    missing_candidate_ids = as.list(setdiff(reference_ids, candidate_ids)),
    unexpected_candidate_ids = as.list(setdiff(candidate_ids, reference_ids)),
    identity_categorical_date_null_parity = attribute_ok,
    numeric_attributes_within_tolerance = attribute_ok && all(vapply(
      attribute_results, function(result) isTRUE(result$numeric_within_tolerance), logical(1)
    )),
    max_numeric_attribute_absolute_difference = if (length(attribute_results)) {
      max(vapply(attribute_results, function(result) {
        result$max_numeric_absolute_difference %||% 0
      }, numeric(1)), na.rm = TRUE)
    } else NA_real_,
    max_numeric_attribute_relative_difference = if (length(attribute_results)) {
      max(vapply(attribute_results, function(result) {
        result$max_numeric_relative_difference %||% 0
      }, numeric(1)), na.rm = TRUE)
    } else NA_real_,
    structural_geometry_parity = geometry_ok,
    coordinates_within_tolerance = geometry_ok,
    max_absolute_coordinate_difference = if (length(geometry_results)) {
      max(vapply(geometry_results, function(result) {
        result$max_absolute_coordinate_difference %||% NA_real_
      }, numeric(1)), na.rm = TRUE)
    } else NA_real_,
    coordinate_tolerance = coordinate_tolerance,
    attribute_details_by_object_id = attribute_results,
    geometry_details_by_object_id = geometry_results
  )
}

nmq_sf_oid <- function(x, oid_field) {
  index <- match(tolower(oid_field), tolower(names(x)))
  if (is.na(index)) nmq_stop("GeoJSON sf object lacks OID field ", oid_field)
  as.character(x[[index]])
}

nmq_compare_spatial_metrics <- function(reference_path, candidate_path, oid_field) {
  reference <- sf::st_read(reference_path, quiet = TRUE, stringsAsFactors = FALSE)
  candidate <- sf::st_read(candidate_path, quiet = TRUE, stringsAsFactors = FALSE)
  reference_ids <- nmq_sf_oid(reference, oid_field)
  candidate_ids <- nmq_sf_oid(candidate, oid_field)
  candidate <- candidate[match(reference_ids, candidate_ids), , drop = FALSE]
  reference <- sf::st_transform(reference, 3310)
  candidate <- sf::st_transform(candidate, 3310)
  reference_area <- as.numeric(sf::st_area(reference))
  candidate_area <- as.numeric(sf::st_area(candidate))
  area_difference <- abs(reference_area - candidate_area)
  area_relative <- area_difference / pmax(
    abs(reference_area), abs(candidate_area), .Machine$double.eps
  )
  reference_length <- as.numeric(sf::st_length(sf::st_boundary(reference)))
  candidate_length <- as.numeric(sf::st_length(sf::st_boundary(candidate)))
  length_difference <- abs(reference_length - candidate_length)
  length_relative <- length_difference / pmax(
    abs(reference_length), abs(candidate_length), .Machine$double.eps
  )
  bounds_difference <- vapply(seq_len(nrow(reference)), function(index) {
    max(abs(
      unname(sf::st_bbox(reference[index, ])) -
        unname(sf::st_bbox(candidate[index, ]))
    ))
  }, numeric(1))
  list(
    analysis_crs = "EPSG:3310",
    max_area_absolute_difference_square_meters = max(area_difference),
    max_area_relative_difference = max(area_relative),
    area_within_tolerance = all(
      area_difference <= NMQ_AREA_ABS_TOLERANCE_SQUARE_METERS +
        NMQ_AREA_REL_TOLERANCE * pmax(abs(reference_area), abs(candidate_area))
    ),
    max_perimeter_absolute_difference_meters = max(length_difference),
    max_perimeter_relative_difference = max(length_relative),
    perimeter_within_tolerance = all(
      length_difference <= NMQ_LENGTH_ABS_TOLERANCE_METERS +
        NMQ_LENGTH_REL_TOLERANCE * pmax(abs(reference_length), abs(candidate_length))
    ),
    max_bounds_absolute_difference_meters = max(bounds_difference),
    bounds_within_tolerance = all(bounds_difference <= NMQ_BOUNDS_TOLERANCE_METERS)
  )
}

nmq_native_tolerance <- function(source) {
  spatial_reference <- source$native_spatial_reference %||% list()
  wkid <- suppressWarnings(as.integer(
    spatial_reference$latestWkid %||% spatial_reference$wkid %||% NA_integer_
  ))
  if (!is.na(wkid) && wkid %in% NMQ_GEOGRAPHIC_WKIDS) {
    list(value = NMQ_GEOGRAPHIC_TOLERANCE_DEGREES, units = "degrees", wkid = wkid)
  } else {
    list(value = NMQ_NATIVE_PROJECTED_TOLERANCE_METERS, units = "meters", wkid = wkid)
  }
}

nmq_compare_source <- function(reference_root, candidate_root, reference, candidate) {
  source_key <- as.character(reference$source_key)
  if (!identical(source_key, as.character(candidate$source_key))) {
    nmq_stop("Source-key mismatch")
  }
  reference_ids <- sort(as.character(unlist(reference$target_object_ids)))
  candidate_ids <- sort(as.character(unlist(candidate$target_object_ids)))
  reference_identifiers <- sort(nmq_normalized_text(unlist(reference$target_identifiers)))
  candidate_identifiers <- sort(nmq_normalized_text(unlist(candidate$target_identifiers)))
  contract <- list(
    discovery_count_exact = identical(
      as.integer(reference$discovery_feature_count),
      as.integer(candidate$discovery_feature_count)
    ),
    discovery_id_count_exact = identical(
      as.integer(reference$discovery_object_id_count),
      as.integer(candidate$discovery_object_id_count)
    ),
    target_count_exact = identical(
      as.integer(reference$target_feature_count),
      as.integer(candidate$target_feature_count)
    ),
    target_object_ids_exact = identical(reference_ids, candidate_ids),
    normalized_target_identifiers_exact = identical(
      reference_identifiers, candidate_identifiers
    ),
    missing_candidate_ids = as.list(setdiff(reference_ids, candidate_ids)),
    unexpected_candidate_ids = as.list(setdiff(candidate_ids, reference_ids))
  )
  raw <- nmq_compare_raw_responses(reference_root, candidate_root, source_key)
  native_tolerance <- nmq_native_tolerance(reference)
  native <- nmq_compare_feature_file(
    file.path(reference_root, as.character(reference$combined_native_path)),
    file.path(candidate_root, as.character(candidate$combined_native_path)),
    as.character(reference$object_id_field),
    geojson = FALSE,
    coordinate_tolerance = native_tolerance$value
  )
  native$coordinate_units <- native_tolerance$units
  native$native_wkid <- native_tolerance$wkid
  geojson_reference <- file.path(
    reference_root, as.character(reference$combined_geojson_path)
  )
  geojson_candidate <- file.path(
    candidate_root, as.character(candidate$combined_geojson_path)
  )
  geojson <- nmq_compare_feature_file(
    geojson_reference,
    geojson_candidate,
    as.character(reference$object_id_field),
    geojson = TRUE,
    coordinate_tolerance = NMQ_GEOGRAPHIC_TOLERANCE_DEGREES
  )
  geojson$coordinate_units <- "degrees"
  metrics <- nmq_compare_spatial_metrics(
    geojson_reference,
    geojson_candidate,
    as.character(reference$object_id_field)
  )
  contract_ok <- all(unlist(contract[c(
    "discovery_count_exact", "discovery_id_count_exact", "target_count_exact",
    "target_object_ids_exact", "normalized_target_identifiers_exact"
  )]))
  native_ok <- isTRUE(native$source_ids_exact) &&
    isTRUE(native$identity_categorical_date_null_parity) &&
    isTRUE(native$numeric_attributes_within_tolerance) &&
    isTRUE(native$structural_geometry_parity) &&
    isTRUE(native$coordinates_within_tolerance)
  geojson_ok <- isTRUE(geojson$source_ids_exact) &&
    isTRUE(geojson$identity_categorical_date_null_parity) &&
    isTRUE(geojson$numeric_attributes_within_tolerance) &&
    isTRUE(geojson$structural_geometry_parity) &&
    isTRUE(geojson$coordinates_within_tolerance)
  metrics_ok <- isTRUE(metrics$area_within_tolerance) &&
    isTRUE(metrics$perimeter_within_tolerance) &&
    isTRUE(metrics$bounds_within_tolerance)
  pass <- contract_ok && isTRUE(raw$semantically_equivalent_request_definitions) &&
    isTRUE(raw$all_exact_literal_request_raw_hashes_equal) &&
    isTRUE(raw$all_parsed_response_payloads_semantically_equal) &&
    native_ok && geojson_ok && metrics_ok
  list(
    source_key = source_key,
    status = if (pass) "PASS" else "FAIL",
    exact_identity_contract = contract,
    raw_authoritative_response_parity = raw,
    native_arcgis_parity = native,
    epsg4326_geojson_parity = geojson,
    derived_spatial_metric_parity = metrics
  )
}

nmq_parse_args <- function(arguments) {
  values <- list(reference = NULL, candidate = NULL, output = NULL)
  index <- 1L
  while (index <= length(arguments)) {
    key <- arguments[[index]]
    if (!(key %in% c("--reference", "--candidate", "--output"))) {
      nmq_stop("Unknown argument: ", key)
    }
    if (index == length(arguments)) nmq_stop("Missing value for ", key)
    values[[sub("^--", "", key)]] <- arguments[[index + 1L]]
    index <- index + 2L
  }
  if (is.null(values$reference) || is.null(values$candidate)) {
    nmq_stop("--reference and --candidate are required")
  }
  values
}

nmq_main <- function(arguments = commandArgs(trailingOnly = TRUE)) {
  nmq_require_packages(c("jsonlite", "digest", "sf"))
  arguments <- nmq_parse_args(arguments)
  reference_root <- normalizePath(arguments$reference, mustWork = TRUE)
  candidate_root <- normalizePath(arguments$candidate, mustWork = TRUE)
  reference_manifest <- nmq_assert_completed(reference_root)
  candidate_manifest <- nmq_assert_completed(candidate_root)
  reference_sources <- nmq_source_map(reference_manifest)
  candidate_sources <- nmq_source_map(candidate_manifest)
  source_keys <- sort(names(reference_sources), method = "radix")
  if (!identical(source_keys, sort(names(candidate_sources), method = "radix"))) {
    nmq_stop("Snapshot source sets differ")
  }
  sources <- lapply(source_keys, function(source_key) {
    message("Reconciling ", source_key)
    nmq_compare_source(
      reference_root,
      candidate_root,
      reference_sources[[source_key]],
      candidate_sources[[source_key]]
    )
  })
  status <- if (all(vapply(sources, function(source) {
    identical(source$status, "PASS")
  }, logical(1)))) "PASS" else "FAIL"
  max_numeric <- max(vapply(sources, function(source) {
    max(
      source$native_arcgis_parity$max_numeric_attribute_absolute_difference,
      source$epsg4326_geojson_parity$max_numeric_attribute_absolute_difference,
      na.rm = TRUE
    )
  }, numeric(1)), na.rm = TRUE)
  max_native <- max(vapply(sources, function(source) {
    source$native_arcgis_parity$max_absolute_coordinate_difference
  }, numeric(1)), na.rm = TRUE)
  max_geojson <- max(vapply(sources, function(source) {
    source$epsg4326_geojson_parity$max_absolute_coordinate_difference
  }, numeric(1)), na.rm = TRUE)
  raw_pair_count <- sum(vapply(sources, function(source) {
    source$raw_authoritative_response_parity$request_count_reference
  }, numeric(1)))
  exact_literal_raw_count <- sum(vapply(sources, function(source) {
    source$raw_authoritative_response_parity$exact_literal_request_count
  }, numeric(1)))
  total_raw_hash_equal_count <- sum(vapply(sources, function(source) {
    source$raw_authoritative_response_parity$total_raw_hash_equal_count
  }, numeric(1)))
  geometry_raw_count <- sum(vapply(sources, function(source) {
    source$raw_authoritative_response_parity$geometry_response_count
  }, numeric(1)))
  report <- list(
    qa_contract = "BRIM National Monuments tiered acquisition reconciliation",
    checked_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    reference_snapshot = reference_root,
    reference_snapshot_id = reference_manifest$snapshot_id,
    candidate_snapshot = candidate_root,
    candidate_snapshot_id = candidate_manifest$snapshot_id,
    tolerance_contract = list(
      numeric_attribute_absolute = NMQ_NUMERIC_ABS_TOLERANCE,
      numeric_attribute_relative = NMQ_NUMERIC_REL_TOLERANCE,
      native_projected_coordinates_meters = NMQ_NATIVE_PROJECTED_TOLERANCE_METERS,
      geographic_coordinates_degrees = NMQ_GEOGRAPHIC_TOLERANCE_DEGREES,
      area_absolute_square_meters = NMQ_AREA_ABS_TOLERANCE_SQUARE_METERS,
      area_relative = NMQ_AREA_REL_TOLERANCE,
      perimeter_absolute_meters = NMQ_LENGTH_ABS_TOLERANCE_METERS,
      perimeter_relative = NMQ_LENGTH_REL_TOLERANCE,
      projected_bounds_meters = NMQ_BOUNDS_TOLERANCE_METERS
    ),
    summary = list(
      total_semantically_equivalent_raw_response_pairs = raw_pair_count,
      exact_literal_request_raw_response_count = exact_literal_raw_count,
      exact_literal_request_raw_response_hash_parity = all(vapply(sources, function(source) {
        isTRUE(source$raw_authoritative_response_parity$all_exact_literal_request_raw_hashes_equal)
      }, logical(1))),
      total_raw_response_hash_equal_count = total_raw_hash_equal_count,
      all_parsed_response_payloads_semantically_equal = all(vapply(sources, function(source) {
        isTRUE(source$raw_authoritative_response_parity$all_parsed_response_payloads_semantically_equal)
      }, logical(1))),
      geometry_raw_response_count = geometry_raw_count,
      exact_geometry_raw_response_hash_parity = geometry_raw_count == 8L &&
        all(vapply(sources, function(source) {
          isTRUE(source$raw_authoritative_response_parity$all_geometry_raw_response_hashes_equal)
        }, logical(1))),
      exact_count_id_normalized_identifier_parity = all(vapply(sources, function(source) {
        all(unlist(source$exact_identity_contract[c(
          "discovery_count_exact", "discovery_id_count_exact", "target_count_exact",
          "target_object_ids_exact", "normalized_target_identifiers_exact"
        )]))
      }, logical(1))),
      exact_identity_categorical_date_null_parity = all(vapply(sources, function(source) {
        isTRUE(source$native_arcgis_parity$identity_categorical_date_null_parity) &&
          isTRUE(source$epsg4326_geojson_parity$identity_categorical_date_null_parity)
      }, logical(1))),
      max_numeric_attribute_absolute_difference = max_numeric,
      max_native_coordinate_absolute_difference = max_native,
      max_epsg4326_coordinate_absolute_difference_degrees = max_geojson,
      structural_geometry_parity = all(vapply(sources, function(source) {
        isTRUE(source$native_arcgis_parity$structural_geometry_parity) &&
          isTRUE(source$epsg4326_geojson_parity$structural_geometry_parity)
      }, logical(1))),
      all_differences_within_tolerance = identical(status, "PASS"),
      real_service_data_discrepancy = !identical(status, "PASS")
    ),
    status = status,
    sources = sources
  )
  output_text <- paste0(jsonlite::toJSON(
    report, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 17
  ), "\n")
  if (!is.null(arguments$output)) {
    output_path <- path.expand(arguments$output)
    if (file.exists(output_path)) nmq_stop("Refusing to overwrite QA report: ", output_path)
    if (!dir.exists(dirname(output_path)) &&
        !dir.create(dirname(output_path), recursive = TRUE)) {
      nmq_stop("Could not create QA report directory: ", dirname(output_path))
    }
    writeLines(output_text, output_path, useBytes = TRUE)
  }
  cat(output_text)
  if (!identical(status, "PASS")) {
    nmq_stop("Acquisition reconciliation failed; see the emitted report")
  }
  invisible(report)
}

if (sys.nframe() == 0L) nmq_main()
