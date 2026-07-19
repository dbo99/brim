# ==== 30_export_usgs_groundwater_live_inputs.R ==============================
##
## PURPOSE:
##   Export a compact, daily-refresh-ready USGS groundwater input table for the
##   BRIM live-data feed repository.
##
## DESIGN:
##   This is intentionally a pre-feed/export step, not the GitHub retrieval job.
##   It reads the existing BRIM static USGS wells layer and the existing latest
##   water-level RDS, then supplements that older/local backbone with a modern
##   USGS Water Data API discovery query for recent California groundwater-level
##   field measurements (parameter 72019).  This prevents newly activated or
##   recently added USGS wells from being invisible simply because they were not
##   present in the older static BRIM wells cache.
##
## DEFAULT FILTER:
##   Include wells with a latest water-level measurement <= 800 days old when:
##     - the static BRIM status is active, OR
##     - the well was discovered through recent USGS Water Data API 72019 field
##       measurements.
##
## OUTPUTS:
##   brim-live-data-feeds/data/input/usgs_groundwater_latest_index_ca.csv
##   04_processed_data/qa/usgs_groundwater_live_candidate_summary.csv
##   04_processed_data/qa/usgs_groundwater_live_candidate_preview.csv
##   04_processed_data/qa/usgs_groundwater_available_construction_fields.csv
##   04_processed_data/qa/usgs_groundwater_api_discovered_additions.csv
##
## NOTES:
##   - Screen/perforation/open-interval fields are preserved when found.
##   - The script is defensive about column names because older preprocessing
##     runs may have slightly different latest-water-level field names.
##   - History fields are not fetched here; this export creates the stable input
##     backbone for the GitHub daily feed and the RF029 history/sparkline work.
##   - The API-discovery supplement is intentionally controlled by environment
##     switches so it can be disabled if USGS API behavior changes.
## ============================================================================

# ---- 1. Packages ------------------------------------------------------------

required_pkgs <- c("dplyr", "readr", "tibble", "lubridate", "sf", "dataRetrieval")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(lubridate)
  library(sf)
  library(dataRetrieval)
})

# ---- 2. Paths and switches --------------------------------------------------

wells_map_rds <- Sys.getenv(
  "USGS_GW_WELLS_MAP_RDS",
  unset = "04_processed_data/cache/latest/usgs_wells_map.rds"
)

latest_wl_rds <- Sys.getenv(
  "USGS_GW_LATEST_WL_RDS",
  unset = "04_processed_data/rds/usgs_gw_latest_water_levels.rds"
)

out_input_csv <- Sys.getenv(
  "USGS_GW_LIVE_INDEX_CSV",
  unset = "brim-live-data-feeds/data/input/usgs_groundwater_latest_index_ca.csv"
)

qa_dir <- Sys.getenv(
  "USGS_GW_LIVE_QA_DIR",
  unset = "04_processed_data/qa"
)

recent_days <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_LIVE_RECENT_DAYS",
  unset = "800"
)))
if (is.na(recent_days) || recent_days < 1) recent_days <- 800L

active_only <- tolower(Sys.getenv(
  "USGS_GW_LIVE_ACTIVE_ONLY",
  unset = "true"
)) %in% c("true", "t", "1", "yes", "y")

preview_n <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_LIVE_PREVIEW_N",
  unset = "100"
)))
if (is.na(preview_n) || preview_n < 1) preview_n <- 100L

## RF036:
##   Supplement the older/local static BRIM wells cache with a modern Water Data
##   API discovery query for recent California groundwater-level field
##   measurements.  This is the durable update path for newly active or newly
##   added USGS groundwater wells.
api_discovery_enabled <- tolower(Sys.getenv(
  "USGS_GW_API_DISCOVERY_ENABLED",
  unset = "true"
)) %in% c("true", "t", "1", "yes", "y")

api_discovery_lookback_days <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_API_DISCOVERY_LOOKBACK_DAYS",
  unset = "800"
)))
if (is.na(api_discovery_lookback_days) || api_discovery_lookback_days < 1) {
  api_discovery_lookback_days <- 800L
}

api_parameter_code <- Sys.getenv(
  "USGS_GW_API_PARAMETER_CODE",
  unset = "72019"
)
api_parameter_code <- trimws(as.character(api_parameter_code))
if (!nzchar(api_parameter_code)) api_parameter_code <- "72019"

api_discovery_bbox_raw <- Sys.getenv(
  "USGS_GW_API_DISCOVERY_BBOX",
  unset = "-124.6,32.3,-113.8,42.2"
)

api_chunk_size <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_API_CHUNK_SIZE",
  unset = "250"
)))
if (is.na(api_chunk_size) || api_chunk_size < 1) api_chunk_size <- 250L

api_request_pause_sec <- suppressWarnings(as.numeric(Sys.getenv(
  "USGS_GW_API_REQUEST_PAUSE_SEC",
  unset = "0.15"
)))
if (is.na(api_request_pause_sec) || api_request_pause_sec < 0) api_request_pause_sec <- 0.15

known_active_test_site_raw <- Sys.getenv(
  "USGS_GW_KNOWN_ACTIVE_TEST_SITE",
  unset = "362402116280901"
)

# ---- 3. Small helpers -------------------------------------------------------

pt_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

pt_site_no <- function(x) {
  x <- pt_chr(x)
  x <- gsub("\\.0$", "", x)
  x <- gsub("[^0-9]", "", x)
  x[nchar(x) == 0] <- NA_character_
  x
}

known_active_test_site <- pt_site_no(known_active_test_site_raw)

pt_ml_id <- function(site_no) {
  site_no <- pt_site_no(site_no)
  out <- ifelse(!is.na(site_no) & site_no != "", paste0("USGS-", site_no), NA_character_)
  out[!is.na(out)]
}

pt_chunks <- function(x, n) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(list())
  split(x, ceiling(seq_along(x) / n))
}

pt_parse_bbox <- function(x) {
  vals <- suppressWarnings(as.numeric(strsplit(as.character(x), ",", fixed = TRUE)[[1]]))
  if (length(vals) != 4 || any(is.na(vals))) {
    stop(
      "USGS_GW_API_DISCOVERY_BBOX must contain four comma-separated numbers: xmin,ymin,xmax,ymax. Got: ",
      x
    )
  }
  vals
}

pt_coalesce_char <- function(...) {
  dplyr::coalesce(...)
}

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) rep(NA_character_, nrow(df)) else df[[hit[[1]]]]
}

first_existing_num <- function(df, candidates) {
  pt_num(first_existing(df, candidates))
}

as_plain_table <- function(x) {
  if (inherits(x, "sf")) {
    coords <- sf::st_coordinates(x)
    out <- sf::st_drop_geometry(x)

    if (!"longitude" %in% names(out) && ncol(coords) >= 1) out$longitude <- coords[, 1]
    if (!"latitude" %in% names(out) && ncol(coords) >= 2) out$latitude <- coords[, 2]
    return(out)
  }

  as.data.frame(x)
}

pt_empty_api_latest <- function() {
  tibble::tibble(
    site_no = character(),
    api_latest_wl_ft_bgs = numeric(),
    api_latest_wl_datetime_utc = as.POSIXct(character()),
    api_latest_wl_date = as.Date(character()),
    api_latest_wl_status = character(),
    api_latest_wl_qualifier = character(),
    api_latest_wl_units = character(),
    api_latest_wl_source = character(),
    api_discovered_recent = logical()
  )
}

pt_empty_api_metadata <- function() {
  tibble::tibble(
    site_no = character(),
    api_station_nm = character(),
    api_latitude = numeric(),
    api_longitude = numeric(),
    api_status = character(),
    api_well_depth_ft = numeric(),
    api_hole_depth_ft = numeric(),
    api_aqfr_cd = character(),
    api_aqfr_type_cd = character(),
    api_nat_aqfr_cd = character()
  )
}


pt_normalize_monitoring_location_raw <- function(x) {
  ## GW_30_001:
  ##   dataRetrieval and direct OGC metadata fallbacks can return the same
  ##   Water Data API property with different R classes (for example
  ##   revision_created as POSIXct in one chunk and character in another).
  ##   The downstream metadata parser re-coerces needed fields with pt_chr()
  ##   and pt_num(), so normalize chunk outputs to character before bind_rows()
  ##   to prevent class-mismatch failures during long refresh runs.
  if (is.null(x) || nrow(x) == 0) return(tibble::tibble())
  x <- as_plain_table(x)
  x <- tibble::as_tibble(x)

  for (nm in names(x)) {
    col <- x[[nm]]

    if (is.data.frame(col)) {
      x[[nm]] <- vapply(seq_len(nrow(x)), function(i) {
        paste(unlist(col[i, , drop = FALSE]), collapse = "; ")
      }, character(1))
    } else if (is.list(col) && !inherits(col, c("POSIXct", "POSIXt", "Date"))) {
      x[[nm]] <- vapply(col, function(v) {
        if (is.null(v) || length(v) == 0) return(NA_character_)
        paste(unlist(v), collapse = ",")
      }, character(1))
    } else {
      x[[nm]] <- as.character(col)
    }
  }

  x
}

pt_urlencode <- function(x) {
  utils::URLencode(as.character(x), reserved = TRUE)
}

pt_ogc_features_to_table <- function(obj) {
  if (is.null(obj) || is.null(obj$features)) {
    return(tibble::tibble())
  }

  features <- obj$features

  if (is.data.frame(features)) {
    raw <- tibble::as_tibble(features)

    prop_cols <- grep("^properties\\.", names(raw), value = TRUE)
    if (length(prop_cols) > 0) {
      for (nm in prop_cols) {
        clean_nm <- sub("^properties\\.", "", nm)
        if (!clean_nm %in% names(raw)) {
          raw[[clean_nm]] <- raw[[nm]]
        }
      }
    }

    if ("properties" %in% names(raw) && is.data.frame(raw$properties)) {
      raw <- dplyr::bind_cols(
        dplyr::select(raw, -dplyr::all_of("properties")),
        tibble::as_tibble(raw$properties)
      )
    }

    # jsonlite::fromJSON(flatten = TRUE) can preserve GeoJSON coordinates as
    # a list-column, matrix/data-frame, or already-flattened fields depending
    # on response shape. Preserve lon/lat if available so API-discovered wells
    # can be added to the live candidate index without relying on older caches.
    if ("geometry.coordinates" %in% names(raw)) {
      coords <- raw[["geometry.coordinates"]]

      if (is.list(coords)) {
        lon <- vapply(coords, function(x) {
          x <- unlist(x)
          if (length(x) >= 1) suppressWarnings(as.numeric(x[[1]])) else NA_real_
        }, numeric(1))
        lat <- vapply(coords, function(x) {
          x <- unlist(x)
          if (length(x) >= 2) suppressWarnings(as.numeric(x[[2]])) else NA_real_
        }, numeric(1))

        if (!"longitude" %in% names(raw)) raw$longitude <- lon
        if (!"latitude" %in% names(raw)) raw$latitude <- lat
      }
    }

  } else if (is.list(features)) {
    raw <- dplyr::bind_rows(lapply(features, function(feature) {
      props_i <- feature$properties
      if (is.null(props_i)) props_i <- list()

      coords_i <- feature$geometry$coordinates
      if (!is.null(coords_i) && length(coords_i) >= 2) {
        props_i$longitude <- suppressWarnings(as.numeric(coords_i[[1]]))
        props_i$latitude <- suppressWarnings(as.numeric(coords_i[[2]]))
      }

      tibble::as_tibble(props_i)
    }))
  } else {
    raw <- tibble::tibble()
  }

  raw
}

pt_metadata_requested_match_count <- function(raw, requested_site_ids) {
  if (is.null(raw) || nrow(raw) == 0) return(0L)

  site_col <- pick_col(raw, c("monitoring_location_id", "monitoring_location_number", "site_no", "id"))
  if (is.na(site_col)) return(0L)

  got <- unique(pt_site_no(raw[[site_col]]))
  req <- unique(pt_site_no(requested_site_ids))

  got <- got[!is.na(got) & got != ""]
  req <- req[!is.na(req) & req != ""]

  length(intersect(got, req))
}

pt_fetch_recent_api_latest_direct <- function(start_date, end_date, bbox) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    warning("jsonlite is not available; cannot use direct USGS OGC fallback for API discovery.")
    return(tibble::tibble())
  }

  base_url <- "https://api.waterdata.usgs.gov/ogcapi/v0/collections/field-measurements/items"

  props <- c(
    "monitoring_location_id",
    "parameter_code",
    "time",
    "value",
    "unit_of_measure",
    "qualifier",
    "approval_status",
    "measuring_agency"
  )

  query <- c(
    f = "json",
    lang = "en-US",
    skipGeometry = "TRUE",
    bbox = paste(bbox, collapse = ","),
    properties = paste(props, collapse = ","),
    parameter_code = api_parameter_code,
    time = paste0(start_date, "/", end_date),
    limit = "50000"
  )

  url <- paste0(
    base_url,
    "?",
    paste(names(query), vapply(query, pt_urlencode, character(1)), sep = "=", collapse = "&")
  )

  message("USGS groundwater API discovery fallback request:")
  message(url)

  obj <- tryCatch({
    jsonlite::fromJSON(url, flatten = TRUE)
  }, error = function(e) {
    warning("Direct USGS OGC fallback request failed: ", conditionMessage(e))
    NULL
  })

  raw <- pt_ogc_features_to_table(obj)

  message("USGS groundwater API discovery fallback raw rows: ", nrow(raw))
  raw
}

pt_fetch_recent_api_latest <- function() {
  if (!api_discovery_enabled) {
    message("USGS groundwater API discovery disabled by USGS_GW_API_DISCOVERY_ENABLED.")
    return(pt_empty_api_latest())
  }

  bbox <- pt_parse_bbox(api_discovery_bbox_raw)

  if (!exists("read_waterdata_field_measurements", where = asNamespace("dataRetrieval"), inherits = FALSE)) {
    warning("dataRetrieval is missing read_waterdata_field_measurements(); using direct USGS OGC fallback for API discovery.")
    raw_direct <- pt_fetch_recent_api_latest_direct(
      Sys.Date() - api_discovery_lookback_days,
      Sys.Date() + 1,
      bbox
    )

    if (nrow(raw_direct) == 0) {
      return(pt_empty_api_latest())
    }

    raw <- raw_direct
  } else {

  start_date <- Sys.Date() - api_discovery_lookback_days
  end_date <- Sys.Date() + 1

  message(
    "Discovering recent USGS groundwater field measurements: parameter ", api_parameter_code,
    ", time ", start_date, "/", end_date,
    ", bbox ", paste(bbox, collapse = ",")
  )

    raw <- tryCatch({
      dataRetrieval::read_waterdata_field_measurements(
        parameter_code = api_parameter_code,
        time = paste0(start_date, "/", end_date),
        bbox = bbox,
        properties = c(
          "monitoring_location_id",
          "parameter_code",
          "time",
          "value",
          "unit_of_measure",
          "qualifier",
          "approval_status",
          "measuring_agency"
        ),
        skipGeometry = TRUE
      )
    }, error = function(e) {
      warning(
        "USGS groundwater API discovery through dataRetrieval failed; trying direct USGS OGC fallback. Original error: ",
        conditionMessage(e)
      )
      pt_fetch_recent_api_latest_direct(start_date, end_date, bbox)
    })
  }

  message("USGS groundwater API discovery raw rows: ", nrow(raw))

  needed <- c("monitoring_location_id", "time", "value")
  if (nrow(raw) == 0 || !all(needed %in% names(raw))) {
    warning(
      "USGS groundwater API discovery returned no usable rows/columns. Missing: ",
      paste(setdiff(needed, names(raw)), collapse = ", ")
    )
    return(pt_empty_api_latest())
  }

  out <- raw |>
    mutate(
      site_no = pt_site_no(.data$monitoring_location_id),
      api_latest_wl_ft_bgs = pt_num(.data$value),
      api_latest_wl_datetime_utc = suppressWarnings(lubridate::as_datetime(.data$time, tz = "UTC")),
      api_latest_wl_date = as.Date(.data$api_latest_wl_datetime_utc),
      api_latest_wl_status = pt_chr(if ("approval_status" %in% names(raw)) .data$approval_status else NA_character_),
      api_latest_wl_qualifier = pt_chr(if ("qualifier" %in% names(raw)) .data$qualifier else NA_character_),
      api_latest_wl_units = pt_chr(if ("unit_of_measure" %in% names(raw)) .data$unit_of_measure else NA_character_),
      api_latest_wl_source = "USGS Water Data API field-measurements 72019",
      api_discovered_recent = TRUE
    ) |>
    filter(
      !is.na(.data$site_no),
      !is.na(.data$api_latest_wl_ft_bgs),
      !is.na(.data$api_latest_wl_datetime_utc)
    ) |>
    arrange(.data$site_no, .data$api_latest_wl_datetime_utc) |>
    group_by(.data$site_no) |>
    slice_tail(n = 1) |>
    ungroup() |>
    select(
      site_no,
      api_latest_wl_ft_bgs,
      api_latest_wl_datetime_utc,
      api_latest_wl_date,
      api_latest_wl_status,
      api_latest_wl_qualifier,
      api_latest_wl_units,
      api_latest_wl_source,
      api_discovered_recent
    )

  message("USGS groundwater API discovery latest sites: ", nrow(out))
  out
}

pt_fetch_monitoring_location_metadata_direct <- function(site_ids, chunk_label = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    warning("jsonlite is not available; cannot use direct USGS OGC monitoring-location metadata fallback.")
    return(tibble::tibble())
  }

  site_ids <- unique(as.character(site_ids))
  site_ids <- site_ids[!is.na(site_ids) & site_ids != ""]
  if (length(site_ids) == 0) return(tibble::tibble())

  ## Direct OGC requests work for small ID lists but can silently return zero
  ## rows for very long comma-separated id= queries.  Keep the fallback robust
  ## by sub-chunking the requested monitoring locations.  dataRetrieval remains
  ## the preferred path above; this direct path is only an escape hatch when a
  ## dataRetrieval chunk fails or returns no requested sites.
  direct_chunk_size <- suppressWarnings(as.integer(Sys.getenv(
    "USGS_GW_DIRECT_METADATA_CHUNK_SIZE",
    unset = "25"
  )))
  if (is.na(direct_chunk_size) || direct_chunk_size < 1) direct_chunk_size <- 25L

  site_chunks <- pt_chunks(site_ids, direct_chunk_size)
  base_url <- "https://api.waterdata.usgs.gov/ogcapi/v0/collections/monitoring-locations/items"
  raw_list <- vector("list", length(site_chunks))

  for (j in seq_along(site_chunks)) {
    query <- c(
      f = "json",
      lang = "en-US",
      limit = "50000",
      id = paste(site_chunks[[j]], collapse = ",")
    )

    url <- paste0(
      base_url,
      "?",
      paste(names(query), vapply(query, pt_urlencode, character(1)), sep = "=", collapse = "&")
    )

    label <- if (!is.null(chunk_label) && nzchar(chunk_label)) {
      paste0(chunk_label, ", subchunk ", j, "/", length(site_chunks))
    } else {
      paste0("subchunk ", j, "/", length(site_chunks))
    }

    message("USGS monitoring-location metadata fallback request ", label, ":")
    message(url)

    obj <- tryCatch({
      jsonlite::fromJSON(url, flatten = TRUE)
    }, error = function(e) {
      warning("Direct USGS monitoring-location metadata fallback failed for ", label, ": ", conditionMessage(e))
      NULL
    })

    raw_j <- pt_ogc_features_to_table(obj)
    match_j <- pt_metadata_requested_match_count(raw_j, site_chunks[[j]])
    message(
      "USGS monitoring-location metadata fallback raw rows ", label, ": ", nrow(raw_j),
      "; requested-site matches: ", match_j
    )

    if (nrow(raw_j) > 0 && match_j == 0) {
      warning(
        "Direct USGS monitoring-location metadata fallback ", label,
        " returned rows but no requested-site matches; dropping this subchunk."
      )
      raw_j <- tibble::tibble()
    }

    raw_list[[j]] <- pt_normalize_monitoring_location_raw(raw_j)
    if (j < length(site_chunks) && api_request_pause_sec > 0) Sys.sleep(api_request_pause_sec)
  }

  raw <- dplyr::bind_rows(lapply(raw_list, pt_normalize_monitoring_location_raw))
  if (!is.null(chunk_label) && nzchar(chunk_label)) {
    message("USGS monitoring-location metadata fallback combined raw rows ", chunk_label, ": ", nrow(raw))
  } else {
    message("USGS monitoring-location metadata fallback combined raw rows: ", nrow(raw))
  }

  raw
}

pt_fetch_monitoring_location_metadata <- function(site_no) {
  site_ids <- pt_ml_id(site_no)

  if (!api_discovery_enabled || length(site_ids) == 0) {
    return(pt_empty_api_metadata())
  }

  chunks <- pt_chunks(site_ids, api_chunk_size)
  message("Fetching USGS monitoring-location metadata in ", length(chunks), " chunk(s).")

  raw_list <- vector("list", length(chunks))

  has_dataretrieval_ml <- exists(
    "read_waterdata_monitoring_location",
    where = asNamespace("dataRetrieval"),
    inherits = FALSE
  )

  if (!has_dataretrieval_ml) {
    warning(
      "dataRetrieval is missing read_waterdata_monitoring_location(); ",
      "using direct USGS OGC metadata fallback for all chunks."
    )
  }

  for (i in seq_along(chunks)) {
    label <- paste0(i, " of ", length(chunks), " | sites: ", length(chunks[[i]]))
    message("  monitoring-location chunk ", label)

    raw_i <- tibble::tibble()
    dataretrieval_match_n <- 0L

    if (has_dataretrieval_ml) {
      ## Prefer the USGS/dataRetrieval client first.  If the client hits the
      ## intermittent cli/progress-bar error seen in R 4.4.x sessions, or if it
      ## returns rows that do not include the requested site IDs, fall back to a
      ## direct OGC request for this chunk only.
      raw_i <- tryCatch({
        out <- dataRetrieval::read_waterdata_monitoring_location(
          monitoring_location_id = chunks[[i]]
        ) |>
          as_plain_table()

        dataretrieval_match_n <- pt_metadata_requested_match_count(out, chunks[[i]])
        message(
          "    dataRetrieval metadata rows: ", nrow(out),
          "; requested-site matches: ", dataretrieval_match_n
        )

        out
      }, error = function(e) {
        warning(
          "USGS monitoring-location metadata chunk ", i, "/", length(chunks),
          " failed through dataRetrieval; trying direct OGC fallback. Original error: ",
          conditionMessage(e)
        )
        tibble::tibble()
      })
    }

    dataretrieval_ok <- nrow(raw_i) > 0 && dataretrieval_match_n > 0

    if (!dataretrieval_ok && has_dataretrieval_ml && nrow(raw_i) > 0 && dataretrieval_match_n == 0) {
      warning(
        "USGS monitoring-location metadata chunk ", i, "/", length(chunks),
        " returned no requested-site matches through dataRetrieval; trying direct OGC fallback."
      )
    }

    if (!dataretrieval_ok) {
      raw_i <- pt_fetch_monitoring_location_metadata_direct(
        site_ids = chunks[[i]],
        chunk_label = paste0(i, "/", length(chunks))
      )

      match_n <- pt_metadata_requested_match_count(raw_i, chunks[[i]])
      message(
        "    fallback metadata rows: ", nrow(raw_i),
        "; requested-site matches: ", match_n
      )

      if (nrow(raw_i) > 0 && match_n == 0) {
        warning(
          "USGS monitoring-location metadata fallback chunk ", i, "/", length(chunks),
          " returned rows but no requested-site matches; dropping this chunk."
        )
        raw_i <- tibble::tibble()
      }
    }

    raw_list[[i]] <- pt_normalize_monitoring_location_raw(raw_i)

    if (i < length(chunks) && api_request_pause_sec > 0) Sys.sleep(api_request_pause_sec)
  }

  raw <- dplyr::bind_rows(lapply(raw_list, pt_normalize_monitoring_location_raw))
  message("USGS monitoring-location metadata rows: ", nrow(raw))

  if (nrow(raw) == 0) {
    return(pt_empty_api_metadata())
  }

  out <- tibble::tibble(
    site_no = pt_site_no(first_existing(raw, c("monitoring_location_id", "monitoring_location_number", "site_no", "id"))),
    api_station_nm = pt_chr(first_existing(raw, c("monitoring_location_name", "station_nm", "name", "site_name"))),
    api_latitude = first_existing_num(raw, c("latitude", "lat", "location_latitude", "dec_lat_va")),
    api_longitude = first_existing_num(raw, c("longitude", "lon", "location_longitude", "dec_long_va")),
    api_status = pt_chr(first_existing(raw, c("status", "site_status", "monitoring_location_status", "active_status"))),
    api_well_depth_ft = first_existing_num(raw, c("well_constructed_depth", "well_depth_ft", "well_depth", "well_depth_va")),
    api_hole_depth_ft = first_existing_num(raw, c("hole_constructed_depth", "hole_depth_ft", "hole_depth", "hole_depth_va")),
    api_aqfr_cd = pt_chr(first_existing(raw, c("aquifer_code", "aqfr_cd"))),
    api_aqfr_type_cd = pt_chr(first_existing(raw, c("aquifer_type_code", "aqfr_type_cd"))),
    api_nat_aqfr_cd = pt_chr(first_existing(raw, c("national_aquifer_code", "nat_aqfr_cd")))
  ) |>
    dplyr::filter(!is.na(.data$site_no)) |>
    dplyr::select(
      site_no,
      api_station_nm,
      api_latitude,
      api_longitude,
      api_status,
      api_well_depth_ft,
      api_hole_depth_ft,
      api_aqfr_cd,
      api_aqfr_type_cd,
      api_nat_aqfr_cd
    ) |>
    dplyr::distinct(.data$site_no, .keep_all = TRUE)

  message("USGS monitoring-location metadata unique sites: ", nrow(out))
  out
}

# ---- 4. Read existing BRIM groundwater products -----------------------------

if (!file.exists(wells_map_rds)) {
  stop("USGS wells map RDS not found: ", wells_map_rds)
}

if (!file.exists(latest_wl_rds)) {
  stop("USGS latest water-level RDS not found: ", latest_wl_rds)
}

wells_map <- readRDS(wells_map_rds) |>
  as_plain_table()

latest_wl <- readRDS(latest_wl_rds) |>
  as_plain_table()

message("USGS wells map rows: ", nrow(wells_map))
message("USGS latest water-level rows: ", nrow(latest_wl))

if (!"site_no" %in% names(wells_map)) stop("wells_map is missing site_no")
if (!"site_no" %in% names(latest_wl)) stop("latest_wl is missing site_no")

# ---- 5. Auto-detect latest water-level fields -------------------------------

wl_col <- pick_col(latest_wl, c(
  "latest_wl_ft_bgs", "latest_wl", "lev_va", "water_level_ft_bgs",
  "wl_ft_bgs", "latest_wl_ft", "level_ft_bgs"
))

datetime_col <- pick_col(latest_wl, c(
  "latest_wl_datetime", "latest_datetime", "latest_wl_time", "datetime",
  "dateTime", "time", "lev_dt_time"
))

date_col <- pick_col(latest_wl, c(
  "latest_wl_date", "latest_date", "measurement_date", "lev_dt", "date"
))

if (is.na(wl_col)) {
  stop("Could not identify latest water-level value column in latest_wl. Columns: ", paste(names(latest_wl), collapse = ", "))
}

if (is.na(datetime_col) && is.na(date_col)) {
  stop("Could not identify latest water-level date/datetime column in latest_wl. Columns: ", paste(names(latest_wl), collapse = ", "))
}

message("Using groundwater WL column: ", wl_col)
message("Using groundwater datetime column: ", ifelse(is.na(datetime_col), "none", datetime_col))
message("Using groundwater date column: ", ifelse(is.na(date_col), "none", date_col))

latest_wl2 <- latest_wl |>
  mutate(
    site_no = pt_site_no(.data$site_no),
    latest_wl_ft_bgs = pt_num(.data[[wl_col]]),
    latest_datetime_raw = if (!is.na(datetime_col)) as.character(.data[[datetime_col]]) else NA_character_,
    latest_date_raw = if (!is.na(date_col)) as.character(.data[[date_col]]) else NA_character_,
    latest_wl_datetime_utc = suppressWarnings(lubridate::as_datetime(latest_datetime_raw, tz = "UTC")),
    latest_wl_date = suppressWarnings(as.Date(latest_date_raw)),
    latest_wl_date = dplyr::coalesce(.data$latest_wl_date, as.Date(.data$latest_wl_datetime_utc))
  ) |>
  select(
    site_no,
    latest_wl_ft_bgs,
    latest_wl_datetime_utc,
    latest_wl_date,
    everything()
  ) |>
  distinct(.data$site_no, .keep_all = TRUE)

# ---- 6. Preserve useful well construction/aquifer fields --------------------

construction_candidates <- c(
  "well_depth_ft", "hole_depth_ft",
  "screen_top_ft", "screen_bottom_ft", "screen_interval",
  "open_top_ft", "open_bottom_ft", "open_interval",
  "perf_top_ft", "perf_bottom_ft", "perforation_top_ft", "perforation_bottom_ft",
  "top_open_interval_ft", "bot_open_interval_ft", "bottom_open_interval_ft",
  "aqfr_cd", "aqfr_type_cd", "nat_aqfr_cd", "aquifer", "aquifer_name"
)

available_construction_cols <- intersect(construction_candidates, names(wells_map))

# ---- 7. Build candidate table -----------------------------------------------

wells_tbl <- wells_map |>
  mutate(
    site_no = pt_site_no(.data$site_no),
    station_nm = pt_chr(first_existing(dplyr::pick(dplyr::everything()), c("station_nm", "name", "site_name"))),
    latitude = first_existing_num(dplyr::pick(dplyr::everything()), c("latitude", "lat", "dec_lat_va")),
    longitude = first_existing_num(dplyr::pick(dplyr::everything()), c("longitude", "lon", "dec_long_va")),
    status = pt_chr(first_existing(dplyr::pick(dplyr::everything()), c("status", "site_status", "active_status"))),
    status_clean = tolower(status),
    well_depth_ft = first_existing_num(dplyr::pick(dplyr::everything()), c("well_depth_ft", "well_depth", "well_depth_va")),
    hole_depth_ft = first_existing_num(dplyr::pick(dplyr::everything()), c("hole_depth_ft", "hole_depth", "hole_depth_va")),
    screen_top_ft = first_existing_num(dplyr::pick(dplyr::everything()), c("screen_top_ft", "open_top_ft", "perf_top_ft", "top_open_interval_ft")),
    screen_bottom_ft = first_existing_num(dplyr::pick(dplyr::everything()), c("screen_bottom_ft", "open_bottom_ft", "perf_bottom_ft", "bot_open_interval_ft", "bottom_open_interval_ft")),
    aqfr_cd = pt_chr(first_existing(dplyr::pick(dplyr::everything()), c("aqfr_cd"))),
    aqfr_type_cd = pt_chr(first_existing(dplyr::pick(dplyr::everything()), c("aqfr_type_cd"))),
    nat_aqfr_cd = pt_chr(first_existing(dplyr::pick(dplyr::everything()), c("nat_aqfr_cd"))),
    in_static_wells_map = TRUE
  ) |>
  select(
    site_no, station_nm, latitude, longitude, status,
    well_depth_ft, hole_depth_ft, screen_top_ft, screen_bottom_ft,
    aqfr_cd, aqfr_type_cd, nat_aqfr_cd, in_static_wells_map,
    any_of(setdiff(available_construction_cols, c(
      "well_depth_ft", "hole_depth_ft", "screen_top_ft", "screen_bottom_ft",
      "open_top_ft", "open_bottom_ft", "perf_top_ft", "perf_bottom_ft",
      "aqfr_cd", "aqfr_type_cd", "nat_aqfr_cd"
    )))
  ) |>
  filter(!is.na(.data$site_no), !is.na(.data$latitude), !is.na(.data$longitude)) |>
  distinct(.data$site_no, .keep_all = TRUE)

# ---- 7A. RF036: modern Water Data API discovery supplement ------------------
##
## The existing static BRIM wells layer is valuable because it often carries
## well/hole depth and aquifer fields.  However, it can miss newly active wells
## or wells added to the modern USGS Water Data API after the static cache was
## built.  This supplement discovers recent CA groundwater-level field
## measurements directly from the Water Data API and adds any missing locations
## to the live-feed candidate list.

api_latest <- pt_fetch_recent_api_latest()
api_metadata <- pt_fetch_monitoring_location_metadata(api_latest$site_no)

api_static_additions <- api_metadata |>
  filter(!.data$site_no %in% wells_tbl$site_no) |>
  transmute(
    site_no = .data$site_no,
    station_nm = .data$api_station_nm,
    latitude = .data$api_latitude,
    longitude = .data$api_longitude,
    status = dplyr::coalesce(.data$api_status, "active"),
    well_depth_ft = .data$api_well_depth_ft,
    hole_depth_ft = .data$api_hole_depth_ft,
    screen_top_ft = NA_real_,
    screen_bottom_ft = NA_real_,
    aqfr_cd = .data$api_aqfr_cd,
    aqfr_type_cd = .data$api_aqfr_type_cd,
    nat_aqfr_cd = .data$api_nat_aqfr_cd,
    in_static_wells_map = FALSE
  ) |>
  filter(!is.na(.data$site_no), !is.na(.data$latitude), !is.na(.data$longitude)) |>
  distinct(.data$site_no, .keep_all = TRUE)

message("USGS API-discovered additions not in static wells map: ", nrow(api_static_additions))

wells_tbl2 <- bind_rows(wells_tbl, api_static_additions) |>
  distinct(.data$site_no, .keep_all = TRUE)

gw <- wells_tbl2 |>
  left_join(latest_wl2, by = "site_no") |>
  left_join(api_latest, by = "site_no") |>
  mutate(
    api_discovered_recent = dplyr::coalesce(.data$api_discovered_recent, FALSE),
    in_static_wells_map = dplyr::coalesce(.data$in_static_wells_map, FALSE),
    latest_wl_ft_bgs = dplyr::coalesce(.data$api_latest_wl_ft_bgs, .data$latest_wl_ft_bgs),
    latest_wl_datetime_utc = dplyr::coalesce(.data$api_latest_wl_datetime_utc, .data$latest_wl_datetime_utc),
    latest_wl_date = dplyr::coalesce(.data$api_latest_wl_date, .data$latest_wl_date),
    latest_wl_status = dplyr::coalesce(.data$api_latest_wl_status, pt_chr(first_existing(dplyr::pick(dplyr::everything()), c("latest_wl_status")))),
    latest_wl_qualifier = dplyr::coalesce(.data$api_latest_wl_qualifier, pt_chr(first_existing(dplyr::pick(dplyr::everything()), c("latest_wl_qualifier")))),
    latest_wl_units = dplyr::coalesce(.data$api_latest_wl_units, pt_chr(first_existing(dplyr::pick(dplyr::everything()), c("latest_wl_units")))),
    latest_wl_source = dplyr::coalesce(.data$api_latest_wl_source, pt_chr(first_existing(dplyr::pick(dplyr::everything()), c("latest_wl_source"))), "BRIM static latest groundwater RDS"),
    latest_age_days = as.numeric(Sys.Date() - .data$latest_wl_date),
    has_latest_wl = !is.na(.data$latest_wl_ft_bgs),
    is_active_status = tolower(as.character(.data$status)) == "active",
    is_recent_latest = .data$has_latest_wl & !is.na(.data$latest_age_days) & .data$latest_age_days <= recent_days,
    include_live_gw = .data$is_recent_latest & if (active_only) (.data$is_active_status | .data$api_discovered_recent) else TRUE,
    candidate_source = dplyr::case_when(
      .data$in_static_wells_map & .data$api_discovered_recent ~ "static_wells_map + recent_api_72019",
      .data$in_static_wells_map ~ "static_wells_map",
      .data$api_discovered_recent ~ "recent_api_72019_only",
      TRUE ~ "unknown"
    ),
    usgs_monitoring_location_url = paste0("https://waterdata.usgs.gov/monitoring-location/", .data$site_no, "/"),
    usgs_gw_levels_url = paste0("https://waterdata.usgs.gov/monitoring-location/USGS-", .data$site_no, "/#dataTypeId=measurements-72019-0&period=P1Y")
  )

# ---- 8. QA summaries --------------------------------------------------------


summary_tbl <- tibble::tibble(
  metric = c(
    "all wells in static map",
    "recent API 72019 sites discovered",
    "recent API 72019 sites not in static map",
    "combined candidate backbone sites before filtering",
    "wells with latest water-level value",
    "latest measurement <= 30 days",
    "latest measurement <= 90 days",
    "latest measurement <= 1 year",
    "latest measurement <= 2 years",
    "latest measurement <= 3 years",
    "latest measurement <= 5 years",
    "status marked active",
    "API-discovered recent measurement",
    "status active + latest <= 1 year",
    "status active + latest <= 2 years",
    "status active + latest <= 5 years",
    "selected from static wells map only",
    "selected from static wells map + recent API 72019",
    "selected from recent API 72019 only",
    paste0("known active test site ", known_active_test_site, " selected"),
    paste0("selected live groundwater candidates (active_only=", active_only, ", recent_days=", recent_days, ")")
  ),
  count = c(
    nrow(wells_tbl),
    nrow(api_latest),
    nrow(api_static_additions),
    nrow(wells_tbl2),
    sum(gw$has_latest_wl, na.rm = TRUE),
    sum(gw$has_latest_wl & gw$latest_age_days <= 30, na.rm = TRUE),
    sum(gw$has_latest_wl & gw$latest_age_days <= 90, na.rm = TRUE),
    sum(gw$has_latest_wl & gw$latest_age_days <= 365, na.rm = TRUE),
    sum(gw$has_latest_wl & gw$latest_age_days <= 365 * 2, na.rm = TRUE),
    sum(gw$has_latest_wl & gw$latest_age_days <= 365 * 3, na.rm = TRUE),
    sum(gw$has_latest_wl & gw$latest_age_days <= 365 * 5, na.rm = TRUE),
    sum(gw$is_active_status, na.rm = TRUE),
    sum(gw$api_discovered_recent, na.rm = TRUE),
    sum(gw$is_active_status & gw$has_latest_wl & gw$latest_age_days <= 365, na.rm = TRUE),
    sum(gw$is_active_status & gw$has_latest_wl & gw$latest_age_days <= 365 * 2, na.rm = TRUE),
    sum(gw$is_active_status & gw$has_latest_wl & gw$latest_age_days <= 365 * 5, na.rm = TRUE),
    sum(gw$include_live_gw & gw$candidate_source == "static_wells_map", na.rm = TRUE),
    sum(gw$include_live_gw & gw$candidate_source == "static_wells_map + recent_api_72019", na.rm = TRUE),
    sum(gw$include_live_gw & gw$candidate_source == "recent_api_72019_only", na.rm = TRUE),
    sum(gw$include_live_gw & gw$site_no == known_active_test_site, na.rm = TRUE),
    sum(gw$include_live_gw, na.rm = TRUE)
  )
)

api_additions_qa <- gw |>
  filter(.data$candidate_source == "recent_api_72019_only") |>
  arrange(.data$latest_age_days, .data$site_no) |>
  select(
    site_no, station_nm, latitude, longitude, status,
    latest_wl_ft_bgs, latest_wl_datetime_utc, latest_wl_date, latest_age_days,
    well_depth_ft, hole_depth_ft, aqfr_cd, aqfr_type_cd, nat_aqfr_cd,
    candidate_source, usgs_gw_levels_url
  )

live_candidates <- gw |>
  filter(.data$include_live_gw) |>
  arrange(.data$latest_age_days, .data$site_no) |>
  select(
    site_no, station_nm, latitude, longitude, status,
    latest_wl_ft_bgs, latest_wl_datetime_utc, latest_wl_date, latest_age_days,
    well_depth_ft, hole_depth_ft, screen_top_ft, screen_bottom_ft,
    aqfr_cd, aqfr_type_cd, nat_aqfr_cd,
    candidate_source, in_static_wells_map, api_discovered_recent,
    latest_wl_status, latest_wl_qualifier, latest_wl_units, latest_wl_source,
    usgs_monitoring_location_url, usgs_gw_levels_url,
    -starts_with("api_latest_"),
    everything()
  )

if (nrow(live_candidates) == 0) {
  stop("No live groundwater candidates passed the filter. Check status/recent-days settings.")
}

# ---- 9. Write outputs -------------------------------------------------------

dir.create(dirname(out_input_csv), recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(live_candidates, out_input_csv)
readr::write_csv(summary_tbl, file.path(qa_dir, "usgs_groundwater_live_candidate_summary.csv"))
readr::write_csv(head(live_candidates, preview_n), file.path(qa_dir, "usgs_groundwater_live_candidate_preview.csv"))
readr::write_csv(api_additions_qa, file.path(qa_dir, "usgs_groundwater_api_discovered_additions.csv"))
readr::write_csv(
  tibble::tibble(field = available_construction_cols),
  file.path(qa_dir, "usgs_groundwater_available_construction_fields.csv")
)

message("Saved groundwater live input CSV: ", out_input_csv)
message("Saved QA summary: ", file.path(qa_dir, "usgs_groundwater_live_candidate_summary.csv"))
message("Saved API-discovered additions QA: ", file.path(qa_dir, "usgs_groundwater_api_discovered_additions.csv"))
message("Selected live groundwater candidate rows: ", nrow(live_candidates))
print(summary_tbl)
