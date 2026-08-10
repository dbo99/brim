#!/usr/bin/env Rscript

# Acquire immutable, complete ArcGIS snapshots for BRIM National Monuments.
#
# This is the canonical BRIM acquisition entry point. It follows the repository's
# established httr2/jsonlite ArcGIS patterns and adds exact object-ID verification,
# immutable raw-response preservation, deterministic adaptive batching, and a
# COMPLETE/FAILED gate. It does not create or modify production data.

NM_ACQUISITION_VERSION <- "BRIM_NATIONAL_MONUMENTS_ACQUISITION_20260809_02"
NM_USER_AGENT <- "BRIM-National-Monuments-Acquisition-R/1.0"
NM_TRANSIENT_HTTP <- c(408L, 425L, 429L, 500L, 502L, 503L, 504L)

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || all(is.na(x))) y else x
}

nm_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

nm_require_packages <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    nm_stop(
      "Missing required R package(s): ", paste(missing, collapse = ", "),
      ". Install them explicitly before running this focused pipeline."
    )
  }
  invisible(TRUE)
}

nm_utc_now <- function() {
  format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}

nm_snapshot_stamp <- function() {
  format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ")
}

nm_parent_service_url <- function(layer_url) {
  sub("/[0-9]+/?$", "", sub("/+$", "", layer_url))
}

nm_query_url <- function(layer_url) {
  paste0(sub("/+$", "", layer_url), "/query")
}

nm_ensure_dir <- function(path, must_be_new = FALSE) {
  if (must_be_new && file.exists(path)) {
    nm_stop("Refusing to overwrite existing path: ", path)
  }
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE)) {
    nm_stop("Could not create directory: ", path)
  }
  invisible(path)
}

nm_write_raw_new <- function(payload, path) {
  if (file.exists(path)) nm_stop("Refusing to overwrite existing file: ", path)
  nm_ensure_dir(dirname(path))
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(payload, connection)
  invisible(path)
}

nm_write_text_new <- function(text, path) {
  nm_write_raw_new(charToRaw(enc2utf8(text)), path)
}

nm_write_json_new <- function(value, path, pretty = TRUE) {
  text <- jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    pretty = pretty,
    null = "null",
    # Seventeen significant digits preserve an IEEE-754 double round trip.
    digits = 17,
    na = "null",
    force = TRUE
  )
  nm_write_text_new(paste0(text, "\n"), path)
}

nm_sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

nm_sha256_raw <- function(payload) {
  digest::digest(payload, algo = "sha256", serialize = FALSE)
}

nm_relative_path <- function(path, root) {
  normalized_path <- normalizePath(path, mustWork = FALSE)
  normalized_root <- normalizePath(root, mustWork = TRUE)
  prefix <- paste0(normalized_root, .Platform$file.sep)
  if (!startsWith(normalized_path, prefix)) {
    nm_stop("Path is outside snapshot root: ", normalized_path)
  }
  substring(normalized_path, nchar(prefix) + 1L)
}

nm_normalized_text <- function(value) {
  value <- as.character(value %||% "")
  # macOS iconv transliterates some acute vowels as apostrophe-separated text
  # (for example, "á" -> "'a'"). Normalize common Latin diacritics first so
  # source-name matching is deterministic across R runtimes and locales.
  value <- chartr(
    "áàâäãåÁÀÂÄÃÅéèêëÉÈÊËíìîïÍÌÎÏóòôöõÓÒÔÖÕúùûüÚÙÛÜñÑçÇýÿÝ",
    "aaaaaaAAAAAAeeeeEEEEiiiiIIIIoooooOOOOOuuuuUUUUnNcCyyY",
    value
  )
  value <- iconv(value, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  value[is.na(value)] <- ""
  value <- tolower(value)
  value <- gsub("[^a-z0-9]+", " ", value, perl = TRUE)
  trimws(value)
}

nm_canonical_oid <- function(value) {
  if (is.null(value) || !length(value) || is.na(value[[1]]) ||
      is.logical(value[[1]])) {
    nm_stop("Invalid object ID value")
  }
  value <- value[[1]]
  if (is.numeric(value) && is.finite(value) && value == trunc(value)) {
    return(format(value, scientific = FALSE, trim = TRUE))
  }
  value <- trimws(as.character(value))
  if (!nzchar(value)) nm_stop("Blank object ID encountered")
  value
}

nm_sort_oids <- function(values) {
  values <- as.character(values)
  numeric_values <- suppressWarnings(as.numeric(values))
  values[order(is.na(numeric_values), numeric_values, values, method = "radix")]
}

nm_field_value <- function(attributes, candidates) {
  if (is.null(attributes) || !is.list(attributes) || !length(attributes)) {
    return(NULL)
  }
  attribute_names <- tolower(names(attributes))
  for (candidate in candidates) {
    index <- match(tolower(candidate), attribute_names)
    if (!is.na(index)) {
      value <- attributes[[index]]
      if (!is.null(value) && length(value) && !all(is.na(value)) &&
          any(nzchar(trimws(as.character(value))))) {
        return(value)
      }
    }
  }
  NULL
}

nm_arcgis_error <- function(value) {
  if (!is.list(value) || is.null(value$error)) return(NULL)
  error <- value$error %||% list()
  parts <- c(
    as.character(error$code %||% "unknown"),
    as.character(error$message %||% ""),
    as.character(unlist(error$details %||% list(), use.names = FALSE))
  )
  parts <- parts[!is.na(parts) & nzchar(parts)]
  paste(parts, collapse = " | ")
}

nm_parse_json <- function(payload, path) {
  text <- rawToChar(payload)
  text <- sub("^\\ufeff", "", text)
  value <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = identity
  )
  if (inherits(value, "error")) {
    nm_stop("Malformed JSON in ", path, ": ", conditionMessage(value))
  }
  error <- nm_arcgis_error(value)
  if (!is.null(error)) nm_stop("ArcGIS error in ", path, ": ", error)
  if (!is.list(value) || is.null(names(value))) {
    nm_stop("Expected a JSON object in ", path)
  }
  value
}

nm_new_context <- function(run_dir, source_key) {
  environment <- new.env(parent = emptyenv())
  environment$run_dir <- run_dir
  environment$source_key <- source_key
  environment$source_dir <- file.path(run_dir, source_key)
  environment$counter <- 0L
  environment$files <- list()
  environment$transfer_limit_encountered <- FALSE
  environment$split_batches <- 0L
  environment
}

nm_next_paths <- function(context, label, extension) {
  context$counter <- context$counter + 1L
  stem <- sprintf("%04d_%s", context$counter, label)
  list(
    request = file.path(context$source_dir, "requests", paste0(stem, ".request.json")),
    response = file.path(context$source_dir, "raw", paste0(stem, ".", extension))
  )
}

nm_record_file <- function(context, path, role) {
  info <- file.info(path)
  item <- list(
    path = nm_relative_path(path, context$run_dir),
    role = role,
    bytes = unname(as.numeric(info$size)),
    sha256 = nm_sha256_file(path)
  )
  context$files[[length(context$files) + 1L]] <- item
  invisible(item)
}

nm_headers_list <- function(headers) {
  values <- as.list(headers)
  names(values) <- names(headers)
  lapply(values, as.character)
}

nm_http_get <- function(
  context,
  label,
  base_url,
  parameters,
  extension,
  timeout_seconds,
  retries
) {
  paths <- nm_next_paths(context, label, extension)
  request <- httr2::request(base_url)
  if (length(parameters)) {
    request <- do.call(httr2::req_url_query, c(list(request), parameters))
  }
  request <- request |>
    httr2::req_user_agent(NM_USER_AGENT) |>
    httr2::req_headers(
      Accept = "application/json, application/geo+json"
    ) |>
    httr2::req_timeout(seconds = timeout_seconds) |>
    httr2::req_error(is_error = function(response) FALSE)

  nm_write_json_new(
    list(
      method = "GET",
      base_url = base_url,
      parameters = parameters,
      requested_url = request$url,
      requested_utc = nm_utc_now(),
      user_agent = NM_USER_AGENT
    ),
    paths$request
  )
  nm_record_file(context, paths$request, "request_definition")

  last_error <- NULL
  for (attempt in seq_len(retries)) {
    response <- tryCatch(httr2::req_perform(request), error = identity)
    if (inherits(response, "error")) {
      last_error <- conditionMessage(response)
    } else {
      status <- httr2::resp_status(response)
      payload <- httr2::resp_body_raw(response)
      if (status >= 200L && status < 300L && length(payload)) {
        nm_write_raw_new(payload, paths$response)
        nm_record_file(context, paths$response, "untouched_http_response")
        response_metadata <- paste0(paths$response, ".response.json")
        nm_write_json_new(
          list(
            status = status,
            headers = nm_headers_list(httr2::resp_headers(response)),
            final_url = httr2::resp_url(response),
            retrieved_utc = nm_utc_now(),
            bytes = length(payload),
            sha256 = nm_sha256_raw(payload)
          ),
          response_metadata
        )
        nm_record_file(context, response_metadata, "response_metadata")
        return(list(payload = payload, path = paths$response))
      }

      error_path <- file.path(
        context$source_dir,
        "errors",
        sprintf("%04d_%s_attempt_%d.http_error", context$counter, label, attempt)
      )
      error_payload <- if (length(payload)) payload else charToRaw(paste0("HTTP ", status))
      nm_write_raw_new(error_payload, error_path)
      nm_record_file(context, error_path, "http_error_response")
      last_error <- paste0("HTTP ", status)
      if (!(status %in% NM_TRANSIENT_HTTP)) break
    }
    if (attempt < retries) Sys.sleep(min(2 ^ (attempt - 1L), 8))
  }
  nm_stop(
    "HTTP request failed after ", retries, " attempt(s): ", request$url,
    ": ", last_error %||% "unknown error"
  )
}

nm_fetch_json <- function(context, label, base_url, parameters, timeout_seconds, retries) {
  result <- nm_http_get(
    context, label, base_url, parameters, "json", timeout_seconds, retries
  )
  list(value = nm_parse_json(result$payload, result$path), path = result$path)
}

nm_fetch_geojson <- function(context, label, base_url, parameters, timeout_seconds, retries) {
  result <- nm_http_get(
    context, label, base_url, parameters, "geojson", timeout_seconds, retries
  )
  value <- nm_parse_json(result$payload, result$path)
  if (!identical(value$type, "FeatureCollection") || !is.list(value$features)) {
    nm_stop("Expected GeoJSON FeatureCollection in ", result$path)
  }
  list(value = value, path = result$path)
}

nm_is_transfer_limited <- function(response) {
  isTRUE(response$exceededTransferLimit) ||
    isTRUE(response$exceeded_transfer_limit)
}

nm_feature_ids <- function(features, oid_field, geojson = FALSE) {
  if (!is.list(features)) return(character())
  vapply(features, function(feature) {
    attributes <- if (geojson) feature$properties else feature$attributes
    nm_canonical_oid(nm_field_value(attributes, oid_field))
  }, character(1))
}

nm_geometry_present <- function(feature, geojson = FALSE) {
  geometry <- feature$geometry
  if (!is.list(geometry) || !length(geometry)) return(FALSE)
  if (geojson) {
    return(
      length(geometry$type %||% character()) > 0L &&
        nzchar(as.character(geometry$type[[1]])) &&
        length(geometry$coordinates %||% list()) > 0L
    )
  }
  any(vapply(geometry, function(value) {
    !is.null(value) && length(value) > 0L
  }, logical(1)))
}

nm_validate_features <- function(
  features,
  oid_field,
  requested_ids,
  label,
  require_geometry,
  geojson
) {
  if (!is.list(features)) nm_stop(label, " response has no features array")
  returned <- nm_feature_ids(features, oid_field, geojson)
  if (anyDuplicated(returned)) nm_stop(label, " returned duplicate object IDs")
  if (!setequal(returned, requested_ids) || length(returned) != length(requested_ids)) {
    missing <- setdiff(requested_ids, returned)
    unexpected <- setdiff(returned, requested_ids)
    nm_stop(
      label, " object-ID mismatch; missing=", paste(nm_sort_oids(missing), collapse = ","),
      "; unexpected=", paste(nm_sort_oids(unexpected), collapse = ",")
    )
  }
  if (require_geometry && !all(vapply(features, nm_geometry_present, logical(1), geojson = geojson))) {
    nm_stop(label, " contains null or empty geometry")
  }
  invisible(TRUE)
}

nm_fetch_arcgis_batches <- function(
  context,
  layer_url,
  oid_field,
  object_ids,
  purpose,
  return_geometry,
  timeout_seconds,
  retries,
  out_sr = NULL,
  geojson = FALSE
) {
  collected <- list()

  fetch_one <- function(requested, split_depth = 0L) {
    parameters <- list(
      objectIds = paste(requested, collapse = ","),
      outFields = "*",
      returnGeometry = if (return_geometry) "true" else "false",
      f = if (geojson) "geojson" else "pjson"
    )
    if (!is.null(out_sr)) parameters$outSR <- as.character(out_sr)
    if (return_geometry && !geojson) {
      parameters$returnZ <- "true"
      parameters$returnM <- "true"
    }
    label <- sprintf(
      "%s_batch_%04d_depth_%d",
      purpose,
      length(collected) + 1L,
      split_depth
    )
    result <- if (geojson) {
      nm_fetch_geojson(
        context, label, nm_query_url(layer_url), parameters,
        timeout_seconds, retries
      )
    } else {
      nm_fetch_json(
        context, label, nm_query_url(layer_url), parameters,
        timeout_seconds, retries
      )
    }
    response <- result$value
    features <- response$features %||% list()
    returned <- tryCatch(
      nm_feature_ids(features, oid_field, geojson),
      error = function(error) character()
    )
    exceeded <- nm_is_transfer_limited(response)
    incomplete <- !setequal(returned, requested) ||
      length(returned) != length(requested) || anyDuplicated(returned)

    if (exceeded || incomplete) {
      context$transfer_limit_encountered <-
        context$transfer_limit_encountered || exceeded
      if (length(requested) <= 1L) {
        nm_stop(
          purpose, " could not retrieve complete object ID ", requested[[1]],
          "; transfer_limit=", exceeded,
          "; returned=", paste(returned, collapse = ",")
        )
      }
      midpoint <- floor(length(requested) / 2L)
      context$split_batches <- context$split_batches + 1L
      fetch_one(requested[seq_len(midpoint)], split_depth + 1L)
      fetch_one(requested[seq.int(midpoint + 1L, length(requested))], split_depth + 1L)
      return(invisible(NULL))
    }

    nm_validate_features(
      features,
      oid_field,
      requested,
      purpose,
      require_geometry = return_geometry,
      geojson = geojson
    )
    collected <<- c(collected, features)
    invisible(NULL)
  }

  batch_size <- max(1L, min(200L, length(object_ids)))
  batch_number <- ceiling(seq_along(object_ids) / batch_size)
  for (batch in split(object_ids, batch_number)) fetch_one(unname(batch))
  collected
}

nm_source_target_selection <- function(source, features, oid_field) {
  expected_values <- as.character(unlist(source$target_values, use.names = FALSE))
  expected_keys <- nm_normalized_text(expected_values)
  if (any(!nzchar(expected_keys)) || anyDuplicated(expected_keys)) {
    nm_stop(source$source_key, " has invalid or duplicate normalized target values")
  }
  target_fields <- as.character(unlist(source$target_fields, use.names = FALSE))
  if (isTRUE(source$require_discovery_exact_target_set)) {
    discovered_values <- unlist(lapply(features, function(feature) {
      values <- vapply(target_fields, function(field) {
        value <- nm_field_value(feature$attributes, field)
        if (is.null(value) || !length(value)) "" else as.character(value[[1]])
      }, character(1))
      values[nzchar(trimws(values))]
    }), use.names = FALSE)
    discovered_keys <- unique(nm_normalized_text(discovered_values))
    discovered_keys <- discovered_keys[nzchar(discovered_keys)]
    missing_keys <- setdiff(expected_keys, discovered_keys)
    unexpected_keys <- setdiff(discovered_keys, expected_keys)
    if (length(missing_keys) || length(unexpected_keys)) {
      nm_stop(
        source$source_key, " discovery universe differs from the exact target set; missing=",
        paste(missing_keys, collapse = ","), "; unexpected=",
        paste(unexpected_keys, collapse = ",")
      )
    }
  }
  selected_ids <- character()
  selected_values <- character()
  selected_features <- list()

  for (feature in features) {
    attributes <- feature$attributes
    matched_value <- NULL
    for (field in target_fields) {
      candidate <- nm_field_value(attributes, field)
      if (!is.null(candidate) && nm_normalized_text(candidate[[1]]) %in% expected_keys) {
        matched_value <- as.character(candidate[[1]])
        break
      }
    }
    if (is.null(matched_value)) next
    selected_ids <- c(
      selected_ids,
      nm_canonical_oid(nm_field_value(attributes, oid_field))
    )
    selected_values <- c(selected_values, matched_value)
    selected_features[[length(selected_features) + 1L]] <- feature
  }

  actual_keys <- unique(nm_normalized_text(selected_values))
  missing_keys <- setdiff(expected_keys, actual_keys)
  if (length(missing_keys)) {
    missing_values <- expected_values[match(missing_keys, expected_keys)]
    nm_stop(
      source$source_key, " is missing target values: ",
      paste(missing_values, collapse = ", ")
    )
  }
  if (!length(selected_ids)) nm_stop(source$source_key, " selected no target object IDs")
  if (anyDuplicated(selected_ids)) {
    nm_stop(source$source_key, " target selection duplicated object IDs")
  }
  list(
    object_ids = nm_sort_oids(selected_ids),
    selected_values = sort(unique(selected_values), method = "radix"),
    features = selected_features
  )
}

nm_object_id_field <- function(layer_metadata, ids_response = NULL) {
  value <- layer_metadata$objectIdField %||%
    layer_metadata$objectIdFieldName %||%
    ids_response$objectIdFieldName
  if (!is.null(value) && length(value) && nzchar(as.character(value[[1]]))) {
    return(as.character(value[[1]]))
  }
  fields <- layer_metadata$fields %||% list()
  for (field in fields) {
    if (identical(field$type, "esriFieldTypeOID")) return(as.character(field$name))
  }
  NULL
}

nm_attributes_to_data_frame <- function(features, oid_field) {
  fields <- unique(unlist(lapply(features, function(feature) {
    names(feature$attributes)
  }), use.names = FALSE))
  rows <- lapply(features, function(feature) {
    attributes <- feature$attributes
    values <- lapply(fields, function(field) {
      value <- nm_field_value(attributes, field)
      if (is.null(value) || !length(value) || all(is.na(value))) return(NA_character_)
      if (length(value) == 1L && !is.list(value)) {
        if (is.numeric(value)) {
          return(format(value, digits = 17, scientific = FALSE, trim = TRUE))
        }
        return(as.character(value))
      }
      jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null")
    })
    names(values) <- fields
    as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
  })
  output <- do.call(rbind, rows)
  oid_values <- vapply(features, function(feature) {
    nm_canonical_oid(nm_field_value(feature$attributes, oid_field))
  }, character(1))
  output[order(match(oid_values, nm_sort_oids(oid_values))), , drop = FALSE]
}

nm_write_csv_new <- function(value, path) {
  if (file.exists(path)) nm_stop("Refusing to overwrite existing file: ", path)
  nm_ensure_dir(dirname(path))
  utils::write.csv(value, path, row.names = FALSE, na = "")
  invisible(path)
}

nm_acquire_source <- function(run_dir, source, timeout_seconds, retries) {
  source_key <- as.character(source$source_key)
  context <- nm_new_context(run_dir, source_key)
  nm_ensure_dir(context$source_dir, must_be_new = TRUE)
  started <- nm_utc_now()
  layer_url <- sub("/+$", "", as.character(source$layer_url))

  service_metadata <- nm_fetch_json(
    context,
    "service_metadata",
    nm_parent_service_url(layer_url),
    list(f = "pjson"),
    timeout_seconds,
    retries
  )$value
  layer_metadata <- nm_fetch_json(
    context,
    "layer_metadata",
    layer_url,
    list(f = "pjson"),
    timeout_seconds,
    retries
  )$value

  if (!(as.character(layer_metadata$type %||% "") %in% c("Feature Layer", "Table"))) {
    nm_stop(source_key, " unexpected layer type: ", layer_metadata$type %||% "null")
  }
  geometry_type <- as.character(layer_metadata$geometryType %||% "")
  expected_geometry_type <- as.character(source$expected_geometry_type)
  if (!identical(geometry_type, expected_geometry_type)) {
    nm_stop(
      source_key, " geometry type ", geometry_type,
      " != ", expected_geometry_type
    )
  }

  discovery_where <- as.character(source$discovery_where)
  count_response <- nm_fetch_json(
    context,
    "discovery_count",
    nm_query_url(layer_url),
    list(
      where = discovery_where,
      returnCountOnly = "true",
      f = "pjson"
    ),
    timeout_seconds,
    retries
  )$value
  service_count <- suppressWarnings(as.integer(count_response$count %||% NA_integer_))
  if (is.na(service_count) || service_count <= 0L) {
    nm_stop(source_key, " invalid or empty count response: ", count_response$count %||% "null")
  }

  ids_response <- nm_fetch_json(
    context,
    "discovery_ids",
    nm_query_url(layer_url),
    list(
      where = discovery_where,
      returnIdsOnly = "true",
      returnGeometry = "false",
      f = "pjson"
    ),
    timeout_seconds,
    retries
  )$value
  oid_field <- nm_object_id_field(layer_metadata, ids_response)
  if (is.null(oid_field)) nm_stop(source_key, " layer metadata has no object-ID field")
  raw_ids <- ids_response$objectIds
  if (!is.list(raw_ids)) nm_stop(source_key, " ID response has no objectIds array")
  discovery_ids <- nm_sort_oids(vapply(raw_ids, nm_canonical_oid, character(1)))
  if (length(discovery_ids) != service_count || anyDuplicated(discovery_ids)) {
    nm_stop(
      source_key, " count/ID mismatch: count=", service_count,
      "; ids=", length(discovery_ids),
      "; unique=", length(unique(discovery_ids))
    )
  }

  discovery_features <- nm_fetch_arcgis_batches(
    context,
    layer_url,
    oid_field,
    discovery_ids,
    "discovery_attributes",
    return_geometry = FALSE,
    timeout_seconds = timeout_seconds,
    retries = retries
  )
  if (length(discovery_features) != service_count) {
    nm_stop(
      source_key, " discovery attributes ", length(discovery_features),
      " != count ", service_count
    )
  }

  selection <- nm_source_target_selection(
    source, discovery_features, oid_field
  )
  target_ids <- selection$object_ids
  native_features <- nm_fetch_arcgis_batches(
    context,
    layer_url,
    oid_field,
    target_ids,
    "native_geometry",
    return_geometry = TRUE,
    timeout_seconds = timeout_seconds,
    retries = retries
  )
  if (length(native_features) != length(target_ids)) {
    nm_stop(source_key, " native geometry count mismatch")
  }

  geojson_features <- nm_fetch_arcgis_batches(
    context,
    layer_url,
    oid_field,
    target_ids,
    "geojson_geometry_epsg4326",
    return_geometry = TRUE,
    timeout_seconds = timeout_seconds,
    retries = retries,
    out_sr = 4326L,
    geojson = TRUE
  )
  if (length(geojson_features) != length(target_ids)) {
    nm_stop(source_key, " GeoJSON geometry count mismatch")
  }

  native_ids <- nm_feature_ids(native_features, oid_field, FALSE)
  native_features <- native_features[match(nm_sort_oids(native_ids), native_ids)]
  geojson_ids <- nm_feature_ids(geojson_features, oid_field, TRUE)
  geojson_features <- geojson_features[match(nm_sort_oids(geojson_ids), geojson_ids)]

  combined_geojson <- file.path(
    context$source_dir,
    "derived",
    "scoped_features_epsg4326.geojson"
  )
  nm_write_json_new(
    list(type = "FeatureCollection", features = geojson_features),
    combined_geojson
  )
  nm_record_file(context, combined_geojson, "derived_combined_geojson_epsg4326")

  combined_native <- file.path(
    context$source_dir,
    "derived",
    "scoped_features_native_arcgis.json"
  )
  nm_write_json_new(
    list(
      objectIdFieldName = oid_field,
      geometryType = geometry_type,
      spatialReference = layer_metadata$extent$spatialReference,
      fields = layer_metadata$fields,
      features = native_features
    ),
    combined_native
  )
  nm_record_file(context, combined_native, "derived_combined_native_arcgis_json")

  selected_attributes <- file.path(
    context$source_dir,
    "derived",
    "selected_attribute_inventory.csv"
  )
  nm_write_csv_new(
    nm_attributes_to_data_frame(selection$features, oid_field),
    selected_attributes
  )
  nm_record_file(context, selected_attributes, "derived_selected_attribute_inventory")

  file_manifest <- file.path(context$source_dir, "file_manifest.json")
  nm_write_json_new(context$files, file_manifest)
  nm_record_file(context, file_manifest, "source_file_manifest")

  capabilities <- layer_metadata$advancedQueryCapabilities %||% list()
  service_name <- service_metadata$serviceDescription %||%
    service_metadata$mapName %||%
    service_metadata$name
  list(
    source_key = source_key,
    agency = as.character(source$agency),
    layer_url = layer_url,
    service_url = nm_parent_service_url(layer_url),
    discovery_where = discovery_where,
    discovery_scope = as.character(source$discovery_scope),
    retrieval_started_utc = started,
    retrieval_completed_utc = nm_utc_now(),
    service_name = service_name,
    layer_name = layer_metadata$name,
    geometry_type = geometry_type,
    native_spatial_reference = layer_metadata$extent$spatialReference,
    object_id_field = oid_field,
    max_record_count = layer_metadata$maxRecordCount,
    supports_pagination = capabilities$supportsPagination,
    discovery_feature_count = service_count,
    discovery_object_id_count = length(discovery_ids),
    target_feature_count = length(target_ids),
    target_object_ids = as.list(target_ids),
    target_identifier_fields = source$target_fields,
    target_identifiers = as.list(selection$selected_values),
    expected_target_identifiers = source$target_values,
    transfer_limit_encountered = context$transfer_limit_encountered,
    adaptive_batch_splits = context$split_batches,
    raw_response_count = sum(vapply(context$files, function(item) {
      identical(item$role, "untouched_http_response")
    }, logical(1))),
    combined_geojson_path = nm_relative_path(combined_geojson, run_dir),
    combined_geojson_bytes = unname(as.numeric(file.info(combined_geojson)$size)),
    combined_geojson_sha256 = nm_sha256_file(combined_geojson),
    combined_native_path = nm_relative_path(combined_native, run_dir),
    combined_native_bytes = unname(as.numeric(file.info(combined_native)$size)),
    combined_native_sha256 = nm_sha256_file(combined_native),
    geometry_role_to_review = as.character(source$geometry_role_to_review),
    status = "PASS"
  )
}

nm_usage <- function() {
  paste(
    "Usage:",
    "Rscript acquire_authoritative_sources.R --output-root PATH",
    "[--config PATH] [--snapshot-id ID] [--timeout SECONDS] [--retries N]"
  )
}

nm_parse_args <- function(arguments, default_config) {
  values <- list(
    output_root = NULL,
    config = default_config,
    snapshot_id = NULL,
    timeout = 180L,
    retries = 3L
  )
  index <- 1L
  while (index <= length(arguments)) {
    key <- arguments[[index]]
    if (!(key %in% c(
      "--output-root", "--config", "--snapshot-id", "--timeout", "--retries"
    ))) {
      nm_stop("Unknown argument: ", key, "\n", nm_usage())
    }
    if (index == length(arguments)) nm_stop("Missing value for ", key, "\n", nm_usage())
    value <- arguments[[index + 1L]]
    name <- sub("^--", "", key)
    name <- gsub("-", "_", name, fixed = TRUE)
    values[[name]] <- value
    index <- index + 2L
  }
  if (is.null(values$output_root) || !nzchar(values$output_root)) {
    nm_stop("--output-root is required.\n", nm_usage())
  }
  values$timeout <- suppressWarnings(as.integer(values$timeout))
  values$retries <- suppressWarnings(as.integer(values$retries))
  if (is.na(values$timeout) || values$timeout <= 0L) nm_stop("--timeout must be positive")
  if (is.na(values$retries) || values$retries <= 0L) nm_stop("--retries must be positive")
  values
}

nm_run_configured_acquisition <- function(
  arguments,
  default_config,
  required_source_keys,
  implementation_version = NM_ACQUISITION_VERSION
) {
  nm_require_packages(c("httr2", "jsonlite", "digest"))
  arguments <- nm_parse_args(arguments, default_config)
  config_path <- normalizePath(arguments$config, mustWork = TRUE)
  config <- jsonlite::fromJSON(
    config_path,
    simplifyVector = FALSE
  )
  source_keys <- if (is.list(config$sources)) {
    vapply(config$sources, function(source) {
      as.character(source$source_key %||% "")
    }, character(1))
  } else {
    character()
  }
  if (any(!nzchar(source_keys)) || anyDuplicated(source_keys) ||
      !setequal(source_keys, required_source_keys) ||
      length(source_keys) != length(required_source_keys)) {
    nm_stop(
      basename(config_path), " must define exactly these authoritative sources: ",
      paste(required_source_keys, collapse = ", ")
    )
  }

  snapshot_id <- arguments$snapshot_id %||% nm_snapshot_stamp()
  if (!grepl("^[A-Za-z0-9._-]+$", snapshot_id)) {
    nm_stop("snapshot-id may contain only letters, digits, dot, underscore, and dash")
  }
  output_root <- normalizePath(
    path.expand(arguments$output_root),
    mustWork = FALSE
  )
  nm_ensure_dir(output_root)
  run_dir <- file.path(output_root, snapshot_id)
  nm_ensure_dir(run_dir, must_be_new = TRUE)
  started <- nm_utc_now()

  result <- tryCatch({
    copied_config <- file.path(run_dir, "source_config.json")
    nm_write_raw_new(readBin(config_path, what = "raw", n = file.info(config_path)$size), copied_config)
    summaries <- lapply(config$sources, function(source) {
      message("Acquiring ", source$source_key, " from ", source$layer_url)
      nm_acquire_source(
        run_dir,
        source,
        timeout_seconds = arguments$timeout,
        retries = arguments$retries
      )
    })
    manifest <- list(
      pipeline_version = config$pipeline_version,
      acquisition_implementation = implementation_version,
      canonical_runtime = "R",
      snapshot_id = snapshot_id,
      retrieval_started_utc = started,
      retrieval_completed_utc = nm_utc_now(),
      source_config_path = nm_relative_path(copied_config, run_dir),
      source_config_sha256 = nm_sha256_file(copied_config),
      immutable = TRUE,
      status = "PASS",
      sources = summaries
    )
    nm_write_json_new(manifest, file.path(run_dir, "acquisition_manifest.json"))
    nm_write_json_new(
      list(
        snapshot_id = snapshot_id,
        completed_utc = nm_utc_now(),
        status = "PASS",
        source_count = length(summaries)
      ),
      file.path(run_dir, "COMPLETE.json")
    )
    cat(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 17), "\n")
    invisible(manifest)
  }, error = function(error) {
    failure_path <- file.path(run_dir, "FAILED.json")
    if (!file.exists(failure_path)) {
      nm_write_json_new(
        list(
          snapshot_id = snapshot_id,
          failed_utc = nm_utc_now(),
          status = "FAIL",
          error_type = class(error)[[1]],
          error = conditionMessage(error)
        ),
        failure_path
      )
    }
    stop(error)
  })
  invisible(result)
}

nm_main <- function(arguments = commandArgs(trailingOnly = TRUE)) {
  command_arguments <- commandArgs(trailingOnly = FALSE)
  file_argument <- command_arguments[grepl("^--file=", command_arguments)]
  script_path <- if (length(file_argument)) {
    normalizePath(sub("^--file=", "", file_argument[[1]]), mustWork = TRUE)
  } else {
    normalizePath("02_preprocess/70_national_monuments_pipeline/acquire_authoritative_sources.R", mustWork = TRUE)
  }
  pipeline_dir <- dirname(script_path)
  nm_run_configured_acquisition(
    arguments = arguments,
    default_config = file.path(pipeline_dir, "source_config.json"),
    required_source_keys = c(
      "blm_current", "usfs_current", "usfs_legal_status", "nps_current"
    )
  )
}

if (sys.nframe() == 0L) {
  tryCatch(
    nm_main(),
    error = function(error) {
      message("ACQUISITION FAIL: ", conditionMessage(error))
      quit(status = 1L, save = "no")
    }
  )
}
