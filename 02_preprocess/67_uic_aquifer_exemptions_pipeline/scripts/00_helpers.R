# Shared helpers for the controlled UIC aquifer-exemptions pipeline.

UIC_PIPELINE_VERSION <- "UIC_PIPELINE_67_20260723_02"
UIC_APPROVAL_PHRASE <- "APPROVE UIC CANDIDATE"

uic_utc_now <- function() {
  format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}

uic_stamp_now <- function() {
  format(Sys.time(), tz = "UTC", format = "%Y%m%dT%H%M%SZ")
}

uic_blank_to_na <- function(x) {
  y <- trimws(as.character(x))
  y[is.na(x) | y == ""] <- NA_character_
  y
}

uic_california_counties <- function() {
  c(
    "Alameda", "Alpine", "Amador", "Butte", "Calaveras", "Colusa",
    "Contra Costa", "Del Norte", "El Dorado", "Fresno", "Glenn", "Humboldt",
    "Imperial", "Inyo", "Kern", "Kings", "Lake", "Lassen", "Los Angeles",
    "Madera", "Marin", "Mariposa", "Mendocino", "Merced", "Modoc", "Mono",
    "Monterey", "Napa", "Nevada", "Orange", "Placer", "Plumas", "Riverside",
    "Sacramento", "San Benito", "San Bernardino", "San Diego",
    "San Francisco", "San Joaquin", "San Luis Obispo", "San Mateo",
    "Santa Barbara", "Santa Clara", "Santa Cruz", "Shasta", "Sierra",
    "Siskiyou", "Solano", "Sonoma", "Stanislaus", "Sutter", "Tehama",
    "Trinity", "Tulare", "Tuolumne", "Ventura", "Yolo", "Yuba"
  )
}

uic_normalize_california_counties <- function(x) {
  official <- uic_california_counties()
  lookup <- stats::setNames(official, tolower(official))
  raw <- uic_blank_to_na(x)
  vapply(raw, function(value) {
    if (is.na(value)) return(NA_character_)
    normalized <- gsub(
      "\\bcount(?:y|ies)\\b",
      "",
      value,
      ignore.case = TRUE,
      perl = TRUE
    )
    normalized <- gsub(
      "\\s+(?:and|&)\\s+",
      ";",
      normalized,
      ignore.case = TRUE,
      perl = TRUE
    )
    normalized <- gsub("[,;/|]+", ";", normalized, perl = TRUE)
    parts <- trimws(strsplit(normalized, ";", fixed = TRUE)[[1]])
    parts <- gsub("\\s+", " ", parts, perl = TRUE)
    parts <- parts[nzchar(parts)]
    matched <- unname(lookup[tolower(parts)])
    if (!length(parts) || any(is.na(matched))) return(NA_character_)
    paste(unique(matched), collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}

uic_first_nonblank <- function(...) {
  values <- list(...)
  n <- max(vapply(values, length, integer(1)))
  out <- rep(NA_character_, n)
  for (value in values) {
    value <- rep_len(uic_blank_to_na(value), n)
    use <- is.na(out) & !is.na(value)
    out[use] <- value[use]
  }
  out
}

uic_col <- function(x, name, default = NA_character_) {
  if (name %in% names(x)) x[[name]] else rep(default, nrow(x))
}

uic_require_packages <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop(
      "Missing required R packages: ", paste(missing, collapse = ", "),
      ". Install them explicitly before running the UIC pipeline.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

uic_ensure_dirs <- function(paths) {
  for (path in paths) {
    if (!dir.exists(path) && !dir.create(path, recursive = TRUE)) {
      stop("Could not create directory: ", path, call. = FALSE)
    }
  }
  invisible(paths)
}

uic_read_config <- function(name) {
  path <- file.path(UIC_PIPELINE_ROOT, "config", name)
  if (!file.exists(path)) stop("Missing UIC config: ", path, call. = FALSE)
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}

uic_json_write <- function(x, path, pretty = TRUE) {
  uic_require_packages("jsonlite")
  uic_ensure_dirs(dirname(path))
  tmp <- paste0(path, ".tmp")
  jsonlite::write_json(
    x,
    path = tmp,
    auto_unbox = TRUE,
    pretty = pretty,
    null = "null",
    digits = NA,
    na = "null"
  )
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) stop("Could not finalize JSON: ", path)
  invisible(path)
}

uic_csv_write <- function(x, path) {
  uic_ensure_dirs(dirname(path))
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

uic_text_write <- function(lines, path) {
  uic_ensure_dirs(dirname(path))
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  invisible(path)
}

uic_arcgis_request <- function(url, params = list(), method = "GET", fatal = TRUE) {
  uic_require_packages(c("httr2", "jsonlite"))
  request <- httr2::request(url) |>
    httr2::req_timeout(seconds = 180) |>
    httr2::req_retry(max_tries = 3)
  if (identical(method, "POST")) {
    request <- do.call(httr2::req_body_form, c(list(request), params))
  } else if (length(params)) {
    request <- do.call(httr2::req_url_query, c(list(request), params))
  }
  response <- tryCatch(httr2::req_perform(request), error = identity)
  if (inherits(response, "error")) {
    result <- list(
      ok = FALSE,
      status_code = NA_integer_,
      data = NULL,
      message = conditionMessage(response),
      final_url = url
    )
    if (fatal) stop(result$message, "\nURL: ", url, call. = FALSE)
    return(result)
  }
  body <- httr2::resp_body_string(response)
  data <- tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = identity
  )
  error_message <- NULL
  if (inherits(data, "error")) {
    error_message <- paste("Invalid JSON:", conditionMessage(data))
    data <- NULL
  } else if (!is.null(data$error)) {
    error_message <- paste(
      c(
        paste0("ArcGIS error ", data$error$code %||% "unknown"),
        data$error$message %||% "",
        unlist(data$error$details %||% list(), use.names = FALSE)
      ),
      collapse = " | "
    )
  }
  status <- httr2::resp_status(response)
  ok <- status >= 200 && status < 300 && is.null(error_message)
  result <- list(
    ok = ok,
    status_code = status,
    data = data,
    message = error_message %||% paste0("HTTP ", status),
    final_url = httr2::resp_url(response),
    body_text = body
  )
  if (!ok && fatal) stop(result$message, "\nURL: ", result$final_url, call. = FALSE)
  result
}

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || all(is.na(x))) y else x
}

uic_service_metadata <- function(source_row, fatal = FALSE) {
  result <- uic_arcgis_request(
    source_row$layer_url,
    list(f = "pjson"),
    fatal = fatal
  )
  if (!result$ok) return(result)
  count <- uic_arcgis_request(
    paste0(source_row$layer_url, "/query"),
    list(
      where = source_row$where_clause,
      returnCountOnly = "true",
      f = "json"
    ),
    fatal = FALSE
  )
  ids <- uic_arcgis_request(
    paste0(source_row$layer_url, "/query"),
    list(
      where = source_row$where_clause,
      returnIdsOnly = "true",
      returnGeometry = "false",
      f = "json"
    ),
    fatal = FALSE
  )
  meta <- result$data
  fields <- vapply(meta$fields %||% list(), function(x) x$name %||% "", character(1))
  object_id_field <- meta$objectIdField %||%
    meta$objectIdFieldName %||%
    ids$data$objectIdFieldName %||%
    NA_character_
  list(
    ok = isTRUE(count$ok) && isTRUE(ids$ok),
    status_code = result$status_code,
    message = if (count$ok && ids$ok) "OK" else paste(count$message, ids$message),
    metadata = meta,
    fields = fields,
    feature_count = if (count$ok) as.integer(count$data$count) else NA_integer_,
    object_ids = if (ids$ok) sort(as.numeric(unlist(ids$data$objectIds))) else numeric(),
    object_id_field = object_id_field,
    geometry_type = meta$geometryType %||% NA_character_,
    max_record_count = as.integer(meta$maxRecordCount %||% 1000L),
    supports_pagination = isTRUE(meta$advancedQueryCapabilities$supportsPagination),
    spatial_reference = meta$extent$spatialReference$latestWkid %||%
      meta$extent$spatialReference$wkid %||%
      NA_integer_,
    last_edit_ms = meta$editingInfo$lastEditDate %||%
      meta$editingInfo$lastEditDateTime %||%
      NA_real_,
    checked_utc = uic_utc_now()
  )
}

uic_required_fields <- function(source_row) {
  fields <- strsplit(source_row$required_fields %||% "", ";", fixed = TRUE)[[1]]
  fields[nzchar(fields)]
}

uic_download_geojson <- function(source_row, destination, service_info = NULL) {
  service_info <- service_info %||% uic_service_metadata(source_row, fatal = TRUE)
  if (!isTRUE(service_info$ok)) {
    stop("Cannot download unavailable service: ", source_row$source_key)
  }
  ids <- service_info$object_ids
  oid <- service_info$object_id_field
  if (!length(ids) && service_info$feature_count > 0) {
    stop("Service returned a count but no object IDs: ", source_row$source_key)
  }
  batch_size <- max(1L, min(service_info$max_record_count, 500L))
  chunks <- split(ids, ceiling(seq_along(ids) / batch_size))
  features <- list()
  for (chunk in chunks) {
    where <- if (length(chunk)) {
      paste0(oid, " IN (", paste(format(chunk, scientific = FALSE), collapse = ","), ")")
    } else {
      source_row$where_clause
    }
    response <- uic_arcgis_request(
      paste0(source_row$layer_url, "/query"),
      list(
        where = where,
        outFields = "*",
        returnGeometry = "true",
        outSR = "4326",
        orderByFields = paste(oid, "ASC"),
        f = "geojson"
      ),
      method = "POST",
      fatal = TRUE
    )
    features <- c(features, response$data$features %||% list())
  }
  if (length(features) != service_info$feature_count) {
    stop(
      source_row$source_key, ": downloaded ", length(features),
      " records but service reported ", service_info$feature_count, "."
    )
  }
  object <- list(type = "FeatureCollection", features = features)
  uic_json_write(object, destination, pretty = FALSE)
  invisible(destination)
}

uic_make_valid <- function(x) {
  valid_before <- sf::st_is_valid(x)
  repaired <- which(is.na(valid_before) | !valid_before)
  if (length(repaired)) {
    x[repaired, ] <- sf::st_make_valid(x[repaired, ])
  }
  list(
    data = x,
    repaired_rows = repaired,
    invalid_after = sum(!sf::st_is_valid(x), na.rm = TRUE)
  )
}

uic_geometry_hash <- function(x) {
  uic_require_packages(c("sf", "digest"))
  analysis <- sf::st_transform(x, 3310)
  analysis <- sf::st_set_precision(analysis, 0.01)
  binary <- sf::st_as_binary(sf::st_geometry(analysis), EWKB = TRUE)
  vapply(
    binary,
    function(value) digest::digest(value, algo = "sha256", serialize = FALSE),
    character(1)
  )
}

uic_attribute_hash <- function(x, fields) {
  uic_require_packages("digest")
  fields <- intersect(fields, names(x))
  if (!length(fields)) return(rep(NA_character_, nrow(x)))
  values <- lapply(fields, function(field) uic_blank_to_na(x[[field]]))
  rows <- do.call(paste, c(values, sep = "\u001f"))
  vapply(
    rows,
    function(value) digest::digest(value, algo = "sha256", serialize = FALSE),
    character(1)
  )
}

uic_html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

uic_popup_row <- function(label, value, allow_html = FALSE) {
  value <- uic_blank_to_na(value)
  out <- rep("", length(value))
  keep <- !is.na(value)
  shown <- if (allow_html) value[keep] else uic_html_escape(value[keep])
  out[keep] <- paste0(
    "<tr><th>", uic_html_escape(label), "</th><td>", shown, "</td></tr>"
  )
  out
}

uic_link_html <- function(url, label) {
  url <- uic_blank_to_na(url)
  out <- rep("", length(url))
  keep <- !is.na(url) & grepl("^(https?|ftp)://", url, ignore.case = TRUE)
  suffix <- ifelse(grepl("^ftp://", url[keep], ignore.case = TRUE), " (legacy FTP; unverified)", "")
  out[keep] <- paste0(
    "<a href='", uic_html_escape(url[keep]), "' target='_blank' rel='noopener'>",
    uic_html_escape(label), suffix, "</a>"
  )
  out
}

uic_warning <- function(historic = FALSE) {
  standard <- paste(
    "Aquifer-exemption polygons show mapped surface footprints only.",
    "Exemptions may be limited to particular formations, zones, depths,",
    "elevations, or structural boundaries. A surface intersection does not",
    "establish that all underlying groundwater is exempt. Review the applicable",
    "EPA Record of Decision and supporting documents for project-level interpretation."
  )
  if (!historic) return(standard)
  paste(
    standard,
    "This CalGEM layer is a partial historic shaded subset and is not a complete",
    "map of all 1983 primacy exemptions."
  )
}

uic_candidate_paths <- function(candidate_id) {
  root <- file.path(UIC_CANDIDATE_DIR, candidate_id)
  list(
    root = root,
    full = file.path(root, "full"),
    map_ready = file.path(root, "map_ready"),
    labels = file.path(root, "labels"),
    qa = file.path(root, "qa"),
    metadata = file.path(root, "metadata")
  )
}

uic_latest_candidate_id <- function() {
  if (!dir.exists(UIC_CANDIDATE_DIR)) return(NA_character_)
  ids <- basename(list.dirs(UIC_CANDIDATE_DIR, recursive = FALSE, full.names = TRUE))
  ids <- sort(ids[nzchar(ids)])
  if (length(ids)) tail(ids, 1) else NA_character_
}

uic_dir_size <- function(path) {
  files <- list.files(path, recursive = TRUE, full.names = TRUE)
  if (!length(files)) return(0)
  sum(file.info(files)$size, na.rm = TRUE)
}

uic_copy_tree <- function(from, to) {
  if (!dir.exists(from)) stop("Missing source directory: ", from)
  uic_ensure_dirs(to)
  files <- list.files(from, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  relative <- substring(files, nchar(normalizePath(from, winslash = "/")) + 2L)
  for (i in seq_along(files)) {
    target <- file.path(to, relative[[i]])
    uic_ensure_dirs(dirname(target))
    if (!file.copy(files[[i]], target, overwrite = FALSE, copy.date = TRUE)) {
      stop("Could not copy ", files[[i]], " to ", target)
    }
  }
  invisible(to)
}
