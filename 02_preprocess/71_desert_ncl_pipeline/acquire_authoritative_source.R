#!/usr/bin/env Rscript

# Acquire one immutable, complete BLM California Desert NCL snapshot.
# The raw HTTP response bodies are preserved byte-for-byte. No production or
# processed artifact is read or modified by this acquisition stage.

CDNCL_ACQUISITION_VERSION <- "BRIM_CA_DESERT_NCL_ACQUISITION_20260810_01"
CDNCL_USER_AGENT <- "BRIM-CA-Desert-NCL-Acquisition-R/1.0"

cdncl_stop <- function(...) stop(paste0(...), call. = FALSE)

cdncl_require <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    cdncl_stop("Missing required R package(s): ", paste(missing, collapse = ", "))
  }
}

cdncl_utc <- function(format = "%Y-%m-%dT%H:%M:%SZ") {
  format(Sys.time(), tz = "UTC", format = format)
}

cdncl_sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

cdncl_write_raw_new <- function(value, path) {
  if (file.exists(path)) cdncl_stop("Refusing to overwrite: ", path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(value, connection)
  invisible(path)
}

cdncl_write_json_new <- function(value, path) {
  text <- jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null",
    digits = 17,
    force = TRUE
  )
  cdncl_write_raw_new(charToRaw(enc2utf8(paste0(text, "\n"))), path)
}

cdncl_parse_json_raw <- function(value, label) {
  parsed <- tryCatch(
    jsonlite::fromJSON(rawToChar(value), simplifyVector = FALSE),
    error = identity
  )
  if (inherits(parsed, "error")) {
    cdncl_stop("Malformed JSON in ", label, ": ", conditionMessage(parsed))
  }
  if (!is.null(parsed$error)) {
    cdncl_stop(
      "ArcGIS error in ", label, ": ",
      paste(unlist(parsed$error, use.names = FALSE), collapse = " | ")
    )
  }
  parsed
}

cdncl_fetch <- function(base_url, parameters, stem, run_dir, accept) {
  request <- httr2::request(base_url)
  request <- do.call(httr2::req_url_query, c(list(request), parameters)) |>
    httr2::req_user_agent(CDNCL_USER_AGENT) |>
    httr2::req_headers(Accept = accept) |>
    httr2::req_timeout(seconds = 120) |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_error(is_error = function(response) FALSE)
  request_path <- file.path(run_dir, "requests", paste0(stem, ".request.json"))
  cdncl_write_json_new(
    list(
      method = "GET",
      base_url = base_url,
      parameters = parameters,
      requested_url = request$url,
      requested_utc = cdncl_utc(),
      user_agent = CDNCL_USER_AGENT
    ),
    request_path
  )
  response <- httr2::req_perform(request)
  status <- httr2::resp_status(response)
  payload <- httr2::resp_body_raw(response)
  if (status < 200L || status >= 300L || !length(payload)) {
    cdncl_stop("HTTP request failed for ", stem, ": status ", status)
  }
  response_path <- file.path(run_dir, "raw", paste0(stem, ".json"))
  cdncl_write_raw_new(payload, response_path)
  headers <- as.list(httr2::resp_headers(response))
  headers <- lapply(headers, as.character)
  cdncl_write_json_new(
    list(
      status = status,
      headers = headers,
      final_url = httr2::resp_url(response),
      retrieved_utc = cdncl_utc(),
      bytes = length(payload),
      sha256 = digest::digest(payload, algo = "sha256", serialize = FALSE)
    ),
    paste0(response_path, ".response.json")
  )
  list(path = response_path, payload = payload)
}

cdncl_files_manifest <- function(run_dir) {
  files <- list.files(run_dir, recursive = TRUE, full.names = TRUE)
  files <- files[basename(files) != "acquisition_manifest.json"]
  root <- paste0(normalizePath(run_dir, mustWork = TRUE), .Platform$file.sep)
  lapply(sort(files), function(path) {
    info <- file.info(path)
    list(
      path = substring(normalizePath(path, mustWork = TRUE), nchar(root) + 1L),
      bytes = unname(as.numeric(info$size)),
      sha256 = cdncl_sha256_file(path)
    )
  })
}

cdncl_parse_args <- function(args) {
  output_root <- NULL
  config_path <- file.path(
    "02_preprocess", "71_desert_ncl_pipeline", "source_config.json"
  )
  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    if (!key %in% c("--output-root", "--config") || i == length(args)) {
      cdncl_stop("Usage: acquire_authoritative_source.R --output-root PATH [--config PATH]")
    }
    value <- args[[i + 1L]]
    if (key == "--output-root") output_root <- value else config_path <- value
    i <- i + 2L
  }
  if (is.null(output_root) || !nzchar(trimws(output_root))) {
    cdncl_stop("--output-root is required.")
  }
  list(output_root = output_root, config_path = config_path)
}

cdncl_main <- function() {
  cdncl_require(c("httr2", "jsonlite", "digest"))
  arguments <- cdncl_parse_args(commandArgs(trailingOnly = TRUE))
  if (!file.exists(arguments$config_path)) {
    cdncl_stop("Missing source config: ", arguments$config_path)
  }
  config <- jsonlite::fromJSON(arguments$config_path, simplifyVector = TRUE)
  dir.create(arguments$output_root, recursive = TRUE, showWarnings = FALSE)
  stamp <- cdncl_utc("%Y%m%dT%H%M%SZ")
  run_dir <- file.path(arguments$output_root, stamp)
  if (file.exists(run_dir)) cdncl_stop("Refusing to overwrite snapshot: ", run_dir)
  dir.create(run_dir, recursive = TRUE)
  started <- cdncl_utc()

  tryCatch({
    layer <- cdncl_fetch(
      config$layer_url,
      list(f = "pjson"),
      "001_layer_metadata",
      run_dir,
      "application/json"
    )
    layer_json <- cdncl_parse_json_raw(layer$payload, "layer metadata")
    if (!identical(as.character(layer_json$geometryType), config$expected_geometry_type)) {
      cdncl_stop("Authoritative geometry type changed: ", layer_json$geometryType)
    }

    query_url <- paste0(sub("/+$", "", config$layer_url), "/query")
    count <- cdncl_fetch(
      query_url,
      list(where = config$where, returnCountOnly = "true", f = "json"),
      "002_count",
      run_dir,
      "application/json"
    )
    count_json <- cdncl_parse_json_raw(count$payload, "count response")
    ids <- cdncl_fetch(
      query_url,
      list(where = config$where, returnIdsOnly = "true", f = "json"),
      "003_object_ids",
      run_dir,
      "application/json"
    )
    ids_json <- cdncl_parse_json_raw(ids$payload, "object-ID response")
    object_ids <- sort(unique(as.character(unlist(ids_json$objectIds))))
    if (as.integer(count_json$count) != length(object_ids)) {
      cdncl_stop("Count/object-ID completeness mismatch.")
    }

    geometry <- cdncl_fetch(
      query_url,
      list(
        where = config$where,
        outFields = paste(config$out_fields, collapse = ","),
        returnGeometry = "true",
        outSR = as.character(config$out_sr),
        f = "geojson"
      ),
      "004_features_epsg3310",
      run_dir,
      "application/geo+json, application/json"
    )
    geojson <- cdncl_parse_json_raw(geometry$payload, "GeoJSON response")
    if (!identical(as.character(geojson$type), "FeatureCollection")) {
      cdncl_stop("Expected a GeoJSON FeatureCollection.")
    }
    features <- geojson$features
    properties <- lapply(features, `[[`, "properties")
    feature_ids <- sort(unique(vapply(properties, function(row) {
      as.character(row$OBJECTID)
    }, character(1))))
    nlcs_ids <- sort(unique(vapply(properties, function(row) {
      trimws(as.character(row$NLCS_ID))
    }, character(1))))
    global_ids <- vapply(properties, function(row) {
      trimws(as.character(row$GlobalID))
    }, character(1))
    geometries_present <- vapply(features, function(feature) {
      is.list(feature$geometry) && length(feature$geometry$coordinates) > 0L
    }, logical(1))

    if (length(features) != as.integer(count_json$count) ||
        !identical(feature_ids, object_ids) || any(!geometries_present) ||
        any(!nzchar(global_ids)) || anyDuplicated(global_ids)) {
      cdncl_stop("Feature completeness, geometry, OBJECTID, or GlobalID gate failed.")
    }
    expected_ids <- sort(as.character(config$expected_nlcs_ids))
    inventory_changed <- as.integer(count_json$count) !=
      as.integer(config$expected_record_count) || !identical(nlcs_ids, expected_ids)
    if (inventory_changed) {
      cdncl_stop(
        "Authoritative NLCS_ID universe materially differs from the expected 11: ",
        paste(nlcs_ids, collapse = ", ")
      )
    }

    complete <- list(
      status = "PASS",
      acquisition_version = CDNCL_ACQUISITION_VERSION,
      started_utc = started,
      completed_utc = cdncl_utc(),
      source_key = config$source_key,
      layer_url = config$layer_url,
      where = config$where,
      out_fields = as.list(config$out_fields),
      out_sr = as.integer(config$out_sr),
      record_count = length(features),
      object_ids = as.list(object_ids),
      nlcs_ids = as.list(nlcs_ids),
      inventory_changed = FALSE
    )
    cdncl_write_json_new(complete, file.path(run_dir, "COMPLETE.json"))
    manifest <- complete
    manifest$files <- cdncl_files_manifest(run_dir)
    cdncl_write_json_new(
      manifest,
      file.path(run_dir, "acquisition_manifest.json")
    )
    message(run_dir)
  }, error = function(error) {
    cdncl_write_json_new(
      list(
        status = "FAIL",
        acquisition_version = CDNCL_ACQUISITION_VERSION,
        started_utc = started,
        failed_utc = cdncl_utc(),
        message = conditionMessage(error)
      ),
      file.path(run_dir, "FAILED.json")
    )
    stop(error)
  })
}

cdncl_main()
