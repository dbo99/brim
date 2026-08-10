#!/usr/bin/env Rscript

# Build the reviewed 20-monument BRIM candidate from the COMPLETE primary
# acquisition plus the focused COMPLETE USFWS Tule Lake repair acquisition.
#
# Raw snapshots are read-only. Source-role reconciliation, validity repair,
# simplification, enrichment, and all QA products are written to a new external
# output directory. Candidate creation never promotes a processed/cache file.

NM_CANDIDATE_VERSION <- "BRIM_NATIONAL_MONUMENTS_CANDIDATE_20260809_03"
NM_EXPECTED_SEMANTIC_COUNT <- 20L
NM_EXPECTED_DISPLAY_GEOMETRY_COUNT <- 22L
NM_EXPECTED_ROLE_COUNT <- 37L
NM_SELECTED_SIMPLIFY_TOLERANCE_M <- 1
NM_BENCHMARK_TOLERANCES_M <- c(0, 0.25, 0.5, 1)
NM_ACRES_PER_SQUARE_METER <- 1 / 4046.8564224

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || all(is.na(x))) y else x
}

nm_build_stop <- function(...) stop(paste0(...), call. = FALSE)

nm_build_assert <- function(ok, message) {
  if (!isTRUE(ok)) nm_build_stop(message)
}

nm_build_require_packages <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    nm_build_stop("Missing required R package(s): ", paste(missing, collapse = ", "))
  }
}

nm_build_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

nm_build_utc_now <- function() {
  format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}

nm_build_read_csv <- function(path) {
  if (!file.exists(path)) nm_build_stop("Missing config table: ", path)
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
}

nm_build_read_json <- function(path) {
  if (!file.exists(path)) nm_build_stop("Missing JSON: ", path)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

nm_build_assert_unused <- function(paths) {
  existing <- paths[file.exists(paths)]
  nm_build_assert(
    !length(existing),
    paste("Refusing to overwrite candidate output(s):", paste(existing, collapse = ", "))
  )
}

nm_build_clean <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x %in% c("NA", "N/A", "<NA>", "NULL")] <- ""
  x
}

nm_build_normalize_text <- function(x) {
  x <- nm_build_clean(x)
  x <- gsub("[–—]", "-", x, perl = TRUE)
  x <- chartr(
    "áàâäãåÁÀÂÄÃÅéèêëÉÈÊËíìîïÍÌÎÏóòôöõÓÒÔÖÕúùûüÚÙÛÜñÑçÇýÿÝ",
    "aaaaaaAAAAAAeeeeEEEEiiiiIIIIoooooOOOOOuuuuUUUUnNcCyyY",
    x
  )
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  trimws(gsub("[^a-z0-9]+", " ", tolower(x), perl = TRUE))
}

nm_build_part_count_geometry <- function(geometry, crs) {
  length(suppressWarnings(sf::st_cast(sf::st_sfc(geometry, crs = crs), "POLYGON")))
}

nm_build_part_count <- function(x) {
  crs <- sf::st_crs(x)
  vapply(sf::st_geometry(x), nm_build_part_count_geometry, integer(1), crs = crs)
}

nm_build_hole_count_geometry <- function(geometry) {
  type <- as.character(sf::st_geometry_type(geometry))
  if (identical(type, "POLYGON")) return(as.integer(max(0, length(geometry) - 1L)))
  if (identical(type, "MULTIPOLYGON")) {
    return(sum(vapply(geometry, function(polygon) {
      as.integer(max(0, length(polygon) - 1L))
    }, integer(1))))
  }
  0L
}

nm_build_hole_count <- function(x) {
  vapply(sf::st_geometry(x), nm_build_hole_count_geometry, integer(1))
}

nm_build_vertex_count <- function(x) {
  vapply(sf::st_geometry(x), function(geometry) {
    nrow(sf::st_coordinates(geometry))
  }, integer(1))
}

nm_build_geometry_hash <- function(x) {
  vapply(sf::st_as_binary(sf::st_geometry(x), EWKB = TRUE), function(value) {
    digest::digest(value, algo = "sha256", serialize = FALSE)
  }, character(1))
}

nm_build_source_manifest_map <- function(manifest) {
  output <- list()
  for (source in manifest$sources) output[[source$source_key]] <- source
  output
}

nm_build_validate_snapshot <- function(snapshot_dir) {
  snapshot_dir <- normalizePath(snapshot_dir, mustWork = TRUE)
  nm_build_assert(
    file.exists(file.path(snapshot_dir, "COMPLETE.json")),
    "Acquisition snapshot has no COMPLETE.json."
  )
  nm_build_assert(
    !file.exists(file.path(snapshot_dir, "FAILED.json")),
    "Acquisition snapshot contains FAILED.json."
  )
  complete <- nm_build_read_json(file.path(snapshot_dir, "COMPLETE.json"))
  manifest <- nm_build_read_json(file.path(snapshot_dir, "acquisition_manifest.json"))
  nm_build_assert(
    identical(complete$status, "PASS") && identical(manifest$status, "PASS"),
    "Acquisition snapshot is not PASS."
  )
  sources <- nm_build_source_manifest_map(manifest)
  nm_build_assert(
    identical(
      sort(names(sources)),
      sort(c("blm_current", "usfs_current", "usfs_legal_status", "nps_current"))
    ),
    "Acquisition snapshot does not contain the exact four required sources."
  )
  expected_counts <- c(
    blm_current = 10L,
    usfs_current = 9L,
    usfs_legal_status = 10L,
    nps_current = 7L
  )
  actual_counts <- vapply(names(expected_counts), function(source_key) {
    as.integer(sources[[source_key]]$target_feature_count)
  }, integer(1))
  nm_build_assert(
    identical(actual_counts, expected_counts),
    paste("Snapshot scoped counts differ:", paste(names(actual_counts), actual_counts, collapse = "; "))
  )
  list(root = snapshot_dir, manifest = manifest, sources = sources)
}

nm_build_validate_tule_snapshot <- function(snapshot_dir) {
  snapshot_dir <- normalizePath(snapshot_dir, mustWork = TRUE)
  nm_build_assert(
    file.exists(file.path(snapshot_dir, "COMPLETE.json")),
    "Tule Lake acquisition snapshot has no COMPLETE.json."
  )
  nm_build_assert(
    !file.exists(file.path(snapshot_dir, "FAILED.json")),
    "Tule Lake acquisition snapshot contains FAILED.json."
  )
  complete <- nm_build_read_json(file.path(snapshot_dir, "COMPLETE.json"))
  manifest <- nm_build_read_json(file.path(snapshot_dir, "acquisition_manifest.json"))
  sources <- nm_build_source_manifest_map(manifest)
  nm_build_assert(
    identical(complete$status, "PASS") && identical(manifest$status, "PASS") &&
      identical(names(sources), "fws_tule_lake"),
    "Tule Lake acquisition snapshot is not the exact one-source PASS contract."
  )
  source <- sources$fws_tule_lake
  nm_build_assert(
    identical(as.integer(source$target_feature_count), 1L) &&
      identical(as.character(unlist(source$target_object_ids)), "135") &&
      identical(
        as.character(unlist(source$target_identifiers)),
        "bd04754c-21fd-4e14-bcca-72d1d63a563f"
      ) &&
      !isTRUE(source$transfer_limit_encountered) &&
      identical(as.integer(source$adaptive_batch_splits), 0L),
    "Tule Lake USFWS snapshot count, IDs, or completeness state changed."
  )
  list(root = snapshot_dir, manifest = manifest, sources = sources)
}

nm_build_config_paths <- function(config_dir) {
  prefix <- file.path(config_dir, "local_reference_national_monuments_")
  c(
    aliases = paste0(prefix, "aliases.csv"),
    components = paste0(prefix, "components.csv"),
    designation_history = paste0(prefix, "designation_history.csv"),
    documents = paste0(prefix, "documents.csv"),
    external_services = paste0(prefix, "external_service_candidates.csv"),
    geometry_trust = paste0(prefix, "geometry_trust_metadata.csv"),
    label_registration = paste0(prefix, "label_registration.csv"),
    management = paste0(prefix, "management.csv"),
    reference = paste0(prefix, "reference.csv"),
    relationships = paste0(prefix, "relationships.csv"),
    source_register = paste0(prefix, "source_register.csv"),
    ui_filters = paste0(prefix, "ui_filter_lookup.csv"),
    values = paste0(prefix, "values.csv"),
    source_roles = paste0(prefix, "source_geometry_roles.csv")
  )
}

nm_build_validate_research <- function(config_paths) {
  tables <- lapply(config_paths, nm_build_read_csv)
  reference <- tables$reference
  components <- tables$components
  history <- tables$designation_history
  values <- tables$values
  roles <- tables$source_roles
  nm_build_assert(
    nrow(reference) == 20L && !anyDuplicated(reference$monument_id),
    "National Monument reference must contain 20 unique semantic monuments."
  )
  nm_build_assert(
    nrow(components) == 34L,
    "Modeled component table must retain 34 conceptual/source component rows."
  )
  nm_build_assert(
    nrow(history) == 37L && nrow(values) == 74L,
    "Designation-history/value counts differ from the reviewed 37/74 contract."
  )
  authority <- table(reference$original_authority_type)
  nm_build_assert(
    identical(as.integer(authority[c("Act of Congress", "Presidential proclamation")]), c(3L, 17L)),
    "Creation-authority counts differ from the reviewed 3 congressional / 17 presidential contract."
  )
  agency_membership <- function(agency) sum(grepl(
    paste0("(^|\\|)", agency, "(\\||$)"), reference$administering_agencies
  ))
  nm_build_assert(
    identical(
      c(
        BLM = agency_membership("BLM"),
        USFS = agency_membership("USFS"),
        NPS = agency_membership("NPS"),
        USFWS = agency_membership("USFWS")
      ),
      c(BLM = 9L, USFS = 7L, NPS = 7L, USFWS = 1L)
    ),
    "Administering-agency semantic counts differ from 9/7/7/1."
  )
  nm_build_assert(
    nrow(roles) == NM_EXPECTED_ROLE_COUNT &&
      sum(tolower(roles$use_for_semantic_display) == "true") ==
        NM_EXPECTED_DISPLAY_GEOMETRY_COUNT,
    "Source-role table must retain 37 records and 22 selected display geometries."
  )
  list(tables = tables, hashes = vapply(config_paths, nm_build_sha256, character(1)))
}

nm_build_read_source <- function(snapshot, source_key, native = FALSE) {
  source <- snapshot$sources[[source_key]]
  relative <- if (native) source$combined_native_path else source$combined_geojson_path
  path <- file.path(snapshot$root, as.character(relative))
  x <- sf::st_read(path, quiet = TRUE, stringsAsFactors = FALSE)
  nm_build_assert(
    inherits(x, "sf") && nrow(x) == as.integer(source$target_feature_count),
    paste0(source_key, " geometry count differs from its PASS manifest.")
  )
  list(data = x, path = path, sha256 = nm_build_sha256(path))
}

nm_build_oid <- function(x) {
  candidates <- c("OBJECTID", "objectid")
  field <- candidates[candidates %in% names(x)][1]
  if (is.na(field)) nm_build_stop("Source geometry has no recognized OBJECTID field.")
  as.character(x[[field]])
}

nm_build_source_name <- function(x, source_key) {
  if (identical(source_key, "blm_current")) return(nm_build_clean(x$NCA_NAME))
  if (source_key %in% c("usfs_current", "usfs_legal_status")) {
    return(nm_build_clean(x$areaname))
  }
  if (identical(source_key, "fws_tule_lake")) return(nm_build_clean(x$DESNAME))
  nm_build_clean(x$UNIT_NAME)
}

nm_build_source_identifier <- function(x, source_key) {
  if (identical(source_key, "blm_current")) return(nm_build_clean(x$NLCS_ID))
  if (identical(source_key, "usfs_current")) return(nm_build_clean(x$othnatldesgid))
  if (identical(source_key, "usfs_legal_status")) {
    return(gsub("[{}]", "", nm_build_clean(x$othnatldesgstatusid)))
  }
  if (identical(source_key, "fws_tule_lake")) {
    return(nm_build_clean(x$GlobalID_2))
  }
  nm_build_clean(x$UNIT_CODE)
}

nm_build_source_agency <- function(source_key) {
  if (startsWith(source_key, "blm")) "BLM" else
    if (startsWith(source_key, "usfs")) "USFS" else
      if (startsWith(source_key, "fws")) "USFWS" else "NPS"
}

nm_build_source_profile <- function(source_key, native, derivative, source_manifest) {
  native_data <- native$data
  derivative_data <- derivative$data
  native_driver_geometry_readable <- !all(sf::st_is_empty(native_data))
  geometry_data <- if (native_driver_geometry_readable) native_data else derivative_data
  geometry_qa_basis <- if (native_driver_geometry_readable) {
    "native ArcGIS JSON parsed by sf/GDAL"
  } else {
    "untouched EPSG:4326 GeoJSON response; sf/GDAL exposed empty native ArcGIS geometry"
  }
  projected <- sf::st_transform(derivative_data, 3310)
  geometry_parts <- nm_build_part_count(geometry_data)
  geometry_holes <- nm_build_hole_count(geometry_data)
  geometry_vertices <- nm_build_vertex_count(geometry_data)
  geometry_hashes <- nm_build_geometry_hash(geometry_data)
  data.frame(
    source_key = source_key,
    agency = nm_build_source_agency(source_key),
    source_feature_count = nrow(native_data),
    unique_source_identifier_count = length(unique(nm_build_source_identifier(native_data, source_key))),
    native_crs = sf::st_crs(native_data)$input,
    epsg4326_derivative_crs = sf::st_crs(derivative_data)$input,
    native_sf_driver_geometry_readable = native_driver_geometry_readable,
    geometry_qa_basis = geometry_qa_basis,
    geometry_types = paste(sort(unique(as.character(sf::st_geometry_type(geometry_data)))), collapse = ";"),
    multipart_feature_count = sum(geometry_parts > 1L),
    polygon_part_count = sum(geometry_parts),
    interior_ring_count = sum(geometry_holes),
    vertex_count = sum(geometry_vertices),
    invalid_native_feature_count = if (native_driver_geometry_readable) sum(!sf::st_is_valid(native_data)) else NA_integer_,
    invalid_epsg4326_feature_count = sum(!sf::st_is_valid(derivative_data)),
    null_or_empty_feature_count = sum(sf::st_is_empty(geometry_data)),
    duplicate_geometry_count = sum(duplicated(geometry_hashes)),
    calculated_acres_epsg3310 = sum(as.numeric(sf::st_area(projected))) * NM_ACRES_PER_SQUARE_METER,
    bbox_xmin = unname(sf::st_bbox(geometry_data)[["xmin"]]),
    bbox_ymin = unname(sf::st_bbox(geometry_data)[["ymin"]]),
    bbox_xmax = unname(sf::st_bbox(geometry_data)[["xmax"]]),
    bbox_ymax = unname(sf::st_bbox(geometry_data)[["ymax"]]),
    retrieval_started_utc = as.character(source_manifest$retrieval_started_utc),
    retrieval_completed_utc = as.character(source_manifest$retrieval_completed_utc),
    native_derived_sha256 = native$sha256,
    epsg4326_derived_sha256 = derivative$sha256,
    transfer_limit_encountered = isTRUE(source_manifest$transfer_limit_encountered),
    adaptive_batch_splits = as.integer(source_manifest$adaptive_batch_splits),
    stringsAsFactors = FALSE
  )
}

nm_build_source_feature_profile <- function(source_key, native, derivative, roles) {
  native_data <- native$data
  derivative_data <- derivative$data
  native_oid <- nm_build_oid(native_data)
  derivative_oid <- nm_build_oid(derivative_data)
  derivative_data <- derivative_data[match(native_oid, derivative_oid), , drop = FALSE]
  role_rows <- roles[roles$source_key == source_key, , drop = FALSE]
  role_index <- match(native_oid, as.character(role_rows$source_object_id))
  nm_build_assert(!anyNA(role_index), paste0(source_key, " source records lack reviewed roles."))
  native_driver_geometry_readable <- !all(sf::st_is_empty(native_data))
  geometry_data <- if (native_driver_geometry_readable) native_data else derivative_data
  projected <- sf::st_transform(derivative_data, 3310)
  data.frame(
    source_key = source_key,
    source_object_id = native_oid,
    source_identifier = nm_build_source_identifier(native_data, source_key),
    source_name = nm_build_source_name(native_data, source_key),
    monument_id = role_rows$monument_id[role_index],
    geometry_role = role_rows$geometry_role[role_index],
    use_for_semantic_display = tolower(role_rows$use_for_semantic_display[role_index]) == "true",
    native_sf_driver_geometry_readable = native_driver_geometry_readable,
    geometry_qa_basis = if (native_driver_geometry_readable) "native_arcgis_json" else "epsg4326_geojson_raw_response",
    qa_geometry_type = as.character(sf::st_geometry_type(geometry_data)),
    qa_polygon_parts = nm_build_part_count(geometry_data),
    qa_interior_rings = nm_build_hole_count(geometry_data),
    qa_vertices = nm_build_vertex_count(geometry_data),
    native_valid = if (native_driver_geometry_readable) sf::st_is_valid(native_data) else NA,
    native_validity_reason = if (native_driver_geometry_readable) sf::st_is_valid(native_data, reason = TRUE) else NA_character_,
    epsg4326_valid = sf::st_is_valid(derivative_data),
    epsg4326_validity_reason = sf::st_is_valid(derivative_data, reason = TRUE),
    null_or_empty = sf::st_is_empty(geometry_data),
    qa_geometry_sha256 = nm_build_geometry_hash(geometry_data),
    calculated_acres_epsg3310 = as.numeric(sf::st_area(projected)) * NM_ACRES_PER_SQUARE_METER,
    role_basis = role_rows$role_basis[role_index],
    stringsAsFactors = FALSE
  )
}

nm_build_standardize_selected <- function(source_key, x, roles) {
  oid <- nm_build_oid(x)
  role_rows <- roles[roles$source_key == source_key, , drop = FALSE]
  role_index <- match(oid, as.character(role_rows$source_object_id))
  selected <- !is.na(role_index) &
    tolower(role_rows$use_for_semantic_display[role_index]) == "true"
  x <- x[selected, , drop = FALSE]
  role_index <- role_index[selected]
  oid <- oid[selected]
  source_status <- if (source_key %in% c("usfs_current", "usfs_legal_status")) {
    nm_build_clean(x$boundarystatus)
  } else if (identical(source_key, "nps_current")) {
    nm_build_clean(x$Status)
  } else if (identical(source_key, "fws_tule_lake")) {
    nm_build_clean(x$DESTYPE)
  } else {
    rep("Current public boundary", nrow(x))
  }
  acreage_field <- c("gis_acres", "GISACRES")
  acreage_field <- acreage_field[acreage_field %in% names(x)][1]
  source_gis_acres <- if (!is.na(acreage_field)) {
    suppressWarnings(as.numeric(x[[acreage_field]]))
  } else {
    rep(NA_real_, nrow(x))
  }
  geometry_role <- role_rows$geometry_role[role_index]
  display_agency_key <- rep("", nrow(x))
  agency_component <- geometry_role == "agency_component_primary"
  if (any(agency_component)) {
    if (identical(source_key, "usfs_current")) {
      nm_build_assert(
        "areaid" %in% names(x),
        "USFS agency-component geometry requires the reviewed AREAID field."
      )
      display_agency_key[agency_component] <- ifelse(
        toupper(nm_build_clean(x$areaid[agency_component])) == "BLM",
        "blm", "usfs"
      )
    } else if (identical(source_key, "nps_current")) {
      display_agency_key[agency_component] <- "nps"
    } else if (identical(source_key, "fws_tule_lake")) {
      display_agency_key[agency_component] <- "fws"
    } else {
      nm_build_stop("Unreviewed agency-component source: ", source_key)
    }
  }
  sf::st_sf(
    monument_id = role_rows$monument_id[role_index],
    component_id = paste0("nmgeom-", source_key, "-", oid),
    source_key = rep(source_key, nrow(x)),
    source_agency = rep(nm_build_source_agency(source_key), nrow(x)),
    source_object_id = oid,
    source_identifier = nm_build_source_identifier(x, source_key),
    source_name = nm_build_source_name(x, source_key),
    source_boundary_status = source_status,
    source_gis_acres = source_gis_acres,
    geometry_role = geometry_role,
    display_agency_key = display_agency_key,
    geometry = sf::st_geometry(x)
  )
}

nm_build_repair_and_simplify <- function(selected, tolerance_m) {
  projected <- sf::st_transform(selected, 3310)
  raw_parts <- nm_build_part_count(projected)
  raw_holes <- nm_build_hole_count(projected)
  raw_vertices <- nm_build_vertex_count(projected)
  raw_valid <- sf::st_is_valid(projected)
  raw_area <- as.numeric(sf::st_area(projected))
  repaired <- projected
  repaired[!raw_valid, ] <- sf::st_make_valid(projected[!raw_valid, ])
  nm_build_assert(
    nrow(repaired) == nrow(projected) &&
      identical(repaired$component_id, projected$component_id) &&
      !any(sf::st_is_empty(repaired)) && all(sf::st_is_valid(repaired)),
    "Validity repair changed identity or left invalid/empty selected geometry."
  )
  repaired_parts <- nm_build_part_count(repaired)
  repaired_holes <- nm_build_hole_count(repaired)
  repaired_vertices <- nm_build_vertex_count(repaired)
  repaired_area <- as.numeric(sf::st_area(repaired))
  display <- sf::st_simplify(
    repaired,
    dTolerance = as.numeric(tolerance_m),
    preserveTopology = TRUE
  )
  nm_build_assert(
    nrow(display) == nrow(repaired) &&
      identical(display$component_id, repaired$component_id) &&
      !any(sf::st_is_empty(display)) && all(sf::st_is_valid(display)),
    "Display simplification changed identity or created invalid/empty geometry."
  )
  display_parts <- nm_build_part_count(display)
  display_holes <- nm_build_hole_count(display)
  display_vertices <- nm_build_vertex_count(display)
  display_area <- as.numeric(sf::st_area(display))
  nm_build_assert(
    identical(display_parts, repaired_parts),
    "Selected simplification failed exact polygon-part retention."
  )
  qa <- data.frame(
    monument_id = selected$monument_id,
    component_id = selected$component_id,
    source_key = selected$source_key,
    source_object_id = selected$source_object_id,
    raw_projected_valid = raw_valid,
    repair_applied = !raw_valid,
    repaired_valid = sf::st_is_valid(repaired),
    display_valid = sf::st_is_valid(display),
    raw_polygon_parts = raw_parts,
    repaired_polygon_parts = repaired_parts,
    display_polygon_parts = display_parts,
    raw_interior_rings = raw_holes,
    repaired_interior_rings = repaired_holes,
    display_interior_rings = display_holes,
    raw_vertices = raw_vertices,
    repaired_vertices = repaired_vertices,
    display_vertices = display_vertices,
    raw_calculated_acres = raw_area * NM_ACRES_PER_SQUARE_METER,
    repaired_calculated_acres = repaired_area * NM_ACRES_PER_SQUARE_METER,
    repair_area_change_acres = (repaired_area - raw_area) * NM_ACRES_PER_SQUARE_METER,
    display_calculated_acres = display_area * NM_ACRES_PER_SQUARE_METER,
    display_area_change_acres = (display_area - repaired_area) * NM_ACRES_PER_SQUARE_METER,
    display_area_change_percent = 100 * (display_area - repaired_area) / repaired_area,
    simplify_method = "sf::st_simplify(preserveTopology=TRUE)",
    simplify_crs = "EPSG:3310",
    simplify_tolerance_m = as.numeric(tolerance_m),
    stringsAsFactors = FALSE
  )
  display$semantic_calculated_acres <- repaired_area * NM_ACRES_PER_SQUARE_METER
  display$source_raw_projected_valid <- raw_valid
  display$source_repair_applied <- !raw_valid
  display$source_raw_polygon_parts <- raw_parts
  display$repaired_polygon_parts <- repaired_parts
  display$display_polygon_parts <- display_parts
  display$source_raw_vertices <- raw_vertices
  display$repaired_vertices <- repaired_vertices
  display$display_vertices <- display_vertices
  display$display_area_change_acres <- qa$display_area_change_acres
  list(raw = projected, repaired = repaired, display = display, qa = qa)
}

nm_build_tule_lake_component_qa <- function(processed, source_urls) {
  component_rows <- processed$repaired$monument_id == "nm_ca_tule_lake"
  raw <- processed$raw[component_rows, , drop = FALSE]
  repaired <- processed$repaired[component_rows, , drop = FALSE]
  display <- processed$display[component_rows, , drop = FALSE]
  nm_build_assert(
    nrow(raw) == 2L &&
      setequal(raw$source_key, c("nps_current", "fws_tule_lake")) &&
      setequal(raw$display_agency_key, c("nps", "fws")),
    "Tule Lake must contain exact NPS and USFWS agency-component records."
  )

  polygon_parts <- function(x) {
    out <- suppressWarnings(sf::st_cast(x, "POLYGON"))
    out$source_part_index <- seq_len(nrow(out))
    out
  }
  order_by_area <- function(x) {
    x[order(as.numeric(sf::st_area(x)), decreasing = TRUE), , drop = FALSE]
  }
  constituent_rows <- function(
    raw_feature,
    repaired_feature,
    display_feature,
    component_keys,
    component_names,
    planning_acres
  ) {
    raw_parts <- order_by_area(polygon_parts(raw_feature))
    repaired_parts <- order_by_area(polygon_parts(repaired_feature))
    display_parts <- order_by_area(polygon_parts(display_feature))
    nm_build_assert(
      nrow(raw_parts) == length(component_keys) &&
        nrow(repaired_parts) == length(component_keys) &&
        nrow(display_parts) == length(component_keys),
      "Tule Lake constituent part count changed during repair or simplification."
    )
    data.frame(
      monument_id = "nm_ca_tule_lake",
      constituent_key = component_keys,
      constituent_name = component_names,
      administering_agency = raw_feature$source_agency[[1]],
      source_key = raw_feature$source_key[[1]],
      authoritative_layer = unname(source_urls[raw_feature$source_key[[1]]]),
      source_object_id = raw_feature$source_object_id[[1]],
      source_identifier = raw_feature$source_identifier[[1]],
      source_part_index = raw_parts$source_part_index,
      component_geometry_role = raw_feature$geometry_role[[1]],
      source_feature_polygon_parts = nrow(raw_parts),
      raw_polygon_parts = rep(1L, nrow(raw_parts)),
      repaired_polygon_parts = rep(1L, nrow(repaired_parts)),
      display_polygon_parts = rep(1L, nrow(display_parts)),
      raw_interior_rings = nm_build_hole_count(raw_parts),
      repaired_interior_rings = nm_build_hole_count(repaired_parts),
      display_interior_rings = nm_build_hole_count(display_parts),
      raw_vertices = nm_build_vertex_count(raw_parts),
      repaired_vertices = nm_build_vertex_count(repaired_parts),
      display_vertices = nm_build_vertex_count(display_parts),
      raw_acres_epsg3310 = as.numeric(sf::st_area(raw_parts)) * NM_ACRES_PER_SQUARE_METER,
      repaired_acres_epsg3310 = as.numeric(sf::st_area(repaired_parts)) * NM_ACRES_PER_SQUARE_METER,
      display_acres_epsg3310 = as.numeric(sf::st_area(display_parts)) * NM_ACRES_PER_SQUARE_METER,
      planning_extent_control_acres = planning_acres,
      raw_valid = sf::st_is_valid(raw_parts),
      repaired_valid = sf::st_is_valid(repaired_parts),
      display_valid = sf::st_is_valid(display_parts),
      repair_applied = !sf::st_is_valid(raw_parts),
      simplification_part_count_changed = rep(FALSE, nrow(raw_parts)),
      simplification_hole_count_changed =
        nm_build_hole_count(repaired_parts) != nm_build_hole_count(display_parts),
      simplify_method = "sf::st_simplify(preserveTopology=TRUE)",
      simplify_crs = "EPSG:3310",
      simplify_tolerance_m = NM_SELECTED_SIMPLIFY_TOLERANCE_M,
      stringsAsFactors = FALSE
    )
  }

  nps_index <- which(raw$source_key == "nps_current")
  fws_index <- which(raw$source_key == "fws_tule_lake")
  output <- rbind(
    constituent_rows(
      raw[nps_index, ], repaired[nps_index, ], display[nps_index, ],
      "segregation_center", "Tule Lake Segregation Center", 37
    ),
    constituent_rows(
      raw[fws_index, ], repaired[fws_index, ], display[fws_index, ],
      c("peninsula_castle_rock", "camp_tulelake"),
      c("The Peninsula / Castle Rock", "Camp Tulelake"),
      c(1277, 66)
    )
  )
  fws_acres <- output$repaired_acres_epsg3310[
    output$administering_agency == "USFWS"
  ]
  nm_build_assert(
    nrow(output) == 3L && !anyDuplicated(output$constituent_key) &&
      all(output$raw_valid) && all(output$repaired_valid) && all(output$display_valid) &&
      fws_acres[[1]] > 1200 && fws_acres[[1]] < 1350 &&
      fws_acres[[2]] > 40 && fws_acres[[2]] < 90 &&
      sum(output$repaired_acres_epsg3310) > 1300,
    "Tule Lake constituent geometry failed identity, validity, or planning-extent controls."
  )
  output
}

nm_build_benchmark <- function(repaired, tolerances) {
  raw_area <- as.numeric(sf::st_area(repaired))
  raw_parts <- nm_build_part_count(repaired)
  raw_vertices <- nm_build_vertex_count(repaired)
  do.call(rbind, lapply(tolerances, function(tolerance) {
    candidate <- if (tolerance == 0) repaired else sf::st_simplify(
      repaired, dTolerance = tolerance, preserveTopology = TRUE
    )
    area <- as.numeric(sf::st_area(candidate))
    parts <- nm_build_part_count(candidate)
    vertices <- nm_build_vertex_count(candidate)
    data.frame(
      tolerance_m = tolerance,
      feature_count = nrow(candidate),
      polygon_part_count = sum(parts),
      exact_part_retention = identical(parts, raw_parts),
      vertex_count = sum(vertices),
      vertex_reduction_count = sum(raw_vertices) - sum(vertices),
      vertex_reduction_percent = 100 * (sum(raw_vertices) - sum(vertices)) / sum(raw_vertices),
      invalid_feature_count = sum(!sf::st_is_valid(candidate)),
      empty_feature_count = sum(sf::st_is_empty(candidate)),
      maximum_absolute_area_change_acres = max(abs(area - raw_area)) * NM_ACRES_PER_SQUARE_METER,
      maximum_absolute_area_change_percent = max(abs(area - raw_area) / raw_area * 100),
      total_area_change_acres = sum(area - raw_area) * NM_ACRES_PER_SQUARE_METER,
      selected = identical(as.numeric(tolerance), NM_SELECTED_SIMPLIFY_TOLERANCE_M),
      stringsAsFactors = FALSE
    )
  }))
}

nm_build_overlap_area_acres <- function(left, right) {
  intersection <- suppressWarnings(sf::st_intersection(
    sf::st_geometry(left), sf::st_geometry(right)
  ))
  if (!length(intersection) || all(sf::st_is_empty(intersection))) return(0)
  sum(as.numeric(sf::st_area(intersection))) * NM_ACRES_PER_SQUARE_METER
}

nm_build_symdiff_area_acres <- function(left, right) {
  difference <- suppressWarnings(sf::st_sym_difference(
    sf::st_union(sf::st_geometry(left)),
    sf::st_union(sf::st_geometry(right))
  ))
  if (!length(difference) || all(sf::st_is_empty(difference))) return(0)
  sum(as.numeric(sf::st_area(difference))) * NM_ACRES_PER_SQUARE_METER
}

nm_build_overlap_reconciliation <- function(blm, usfs_current, usfs_legal) {
  blm <- sf::st_transform(blm, 3310)
  usfs_current <- sf::st_transform(usfs_current, 3310)
  usfs_legal <- sf::st_transform(usfs_legal, 3310)
  fix <- function(x) {
    invalid <- !sf::st_is_valid(x)
    x[invalid, ] <- sf::st_make_valid(x[invalid, ])
    x
  }
  blm <- fix(blm)
  usfs_current <- fix(usfs_current)
  usfs_legal <- fix(usfs_legal)
  shared <- list(
    list(key = "berryessa_snow_mountain", blm = "NLCS000318", usfs = "Berryessa Snow Mountain"),
    list(key = "sand_to_snow", blm = "NLCS002020", usfs = "Sand to Snow"),
    list(key = "santa_rosa_san_jacinto", blm = "NLCS000314", usfs = "Santa Rosa and San Jacinto")
  )
  rows <- lapply(shared, function(item) {
    left <- blm[blm$NLCS_ID == item$blm, ]
    right <- usfs_current[usfs_current$areaname == item$usfs, ]
    left_union <- sf::st_sf(geometry = sf::st_union(sf::st_geometry(left)))
    right_union <- sf::st_sf(geometry = sf::st_union(sf::st_geometry(right)))
    left_area <- as.numeric(sf::st_area(left_union)) * NM_ACRES_PER_SQUARE_METER
    right_area <- as.numeric(sf::st_area(right_union)) * NM_ACRES_PER_SQUARE_METER
    overlap <- nm_build_overlap_area_acres(left_union, right_union)
    data.frame(
      comparison = item$key,
      comparison_type = "BLM complete boundary versus USFS current publication",
      left_feature_count = nrow(left),
      right_feature_count = nrow(right),
      left_area_acres = left_area,
      right_area_acres = right_area,
      overlap_area_acres = overlap,
      left_overlap_percent = 100 * overlap / left_area,
      right_overlap_percent = 100 * overlap / right_area,
      symmetric_difference_acres = nm_build_symdiff_area_acres(left_union, right_union),
      display_decision = switch(
        item$key,
        berryessa_snow_mountain = paste(
          "Current complete BLM boundary selected once with neutral shared style;",
          "provisional USFS components omit about 14,587 acres and remain context only"
        ),
        sand_to_snow = paste(
          "Two final non-overlapping USFS-published agency components selected;",
          "BLM complete boundary retained as QA context only"
        ),
        santa_rosa_san_jacinto = paste(
          "BLM complete boundary selected once with neutral shared style;",
          "near-identical USFS whole boundary excluded as a duplicate"
        )
      ),
      stringsAsFactors = FALSE
    )
  })
  legal_names <- sort(unique(usfs_current$areaname), method = "radix")
  legal_rows <- lapply(legal_names, function(name) {
    current <- sf::st_sf(geometry = sf::st_union(sf::st_geometry(
      usfs_current[usfs_current$areaname == name, ]
    )))
    legal <- sf::st_sf(geometry = sf::st_union(sf::st_geometry(
      usfs_legal[usfs_legal$areaname == name, ]
    )))
    data.frame(
      comparison = paste0("usfs_legal_", nm_build_normalize_text(name)),
      comparison_type = "USFS current boundary versus legal-status union",
      left_feature_count = sum(usfs_current$areaname == name),
      right_feature_count = sum(usfs_legal$areaname == name),
      left_area_acres = as.numeric(sf::st_area(current)) * NM_ACRES_PER_SQUARE_METER,
      right_area_acres = as.numeric(sf::st_area(legal)) * NM_ACRES_PER_SQUARE_METER,
      overlap_area_acres = nm_build_overlap_area_acres(current, legal),
      left_overlap_percent = 100,
      right_overlap_percent = 100,
      symmetric_difference_acres = nm_build_symdiff_area_acres(current, legal),
      display_decision = "Current USFS merged boundary selected where primary; legal-status geometry retained as audit/history only",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, c(rows, legal_rows))
}

nm_build_semantic_overlap_qa <- function(repaired) {
  pairs <- utils::combn(seq_len(nrow(repaired)), 2L)
  rows <- lapply(seq_len(ncol(pairs)), function(index) {
    left <- pairs[1, index]
    right <- pairs[2, index]
    if (!sf::st_intersects(repaired[left, ], repaired[right, ], sparse = FALSE)[1, 1]) {
      return(NULL)
    }
    area <- nm_build_overlap_area_acres(repaired[left, ], repaired[right, ])
    if (!is.finite(area) || area <= 0.000247105) return(NULL)
    data.frame(
      left_monument_id = repaired$monument_id[[left]],
      right_monument_id = repaired$monument_id[[right]],
      overlap_area_acres = area,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(
      left_monument_id = character(),
      right_monument_id = character(),
      overlap_area_acres = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

nm_build_migration_crosswalk <- function(existing_raw_path, reference, roles, selected_repaired) {
  existing <- if (tolower(tools::file_ext(existing_raw_path)) == "rds") {
    readRDS(existing_raw_path)
  } else {
    sf::st_read(existing_raw_path, quiet = TRUE, stringsAsFactors = FALSE)
  }
  nm_build_assert(inherits(existing, "sf"), "Existing BRIM comparison layer is not sf.")
  nm_build_assert(nrow(existing) == 19L, "Existing BRIM National Monument raw layer must contain 19 rows.")
  blm_roles <- roles[
    roles$source_key == "blm_current" & nzchar(roles$monument_id),
    c("source_identifier", "monument_id"), drop = FALSE
  ]
  old_monument_id <- blm_roles$monument_id[
    match(as.character(existing$NLCS_ID), blm_roles$source_identifier)
  ]
  nm_build_assert(!anyNA(old_monument_id), "Existing raw NLCS IDs do not all resolve exactly.")
  existing_projected <- sf::st_transform(existing, 3310)
  old_invalid <- !sf::st_is_valid(existing_projected)
  existing_projected[old_invalid, ] <- sf::st_make_valid(existing_projected[old_invalid, ])
  rows <- lapply(seq_len(nrow(reference)), function(index) {
    monument_id <- reference$monument_id[[index]]
    old_rows <- which(old_monument_id == monument_id)
    new_row <- selected_repaired[selected_repaired$monument_id == monument_id, ]
    exact_id <- length(old_rows) > 0L
    old_names <- if (exact_id) paste(sort(unique(existing$NLCS_NAME[old_rows])), collapse = " | ") else ""
    old_agencies <- if (exact_id) paste(sort(unique(existing$AGENCY_COD[old_rows])), collapse = "|") else ""
    symdiff <- area_delta <- NA_real_
    if (exact_id && nrow(new_row) >= 1L) {
      old_union <- sf::st_sf(geometry = sf::st_union(sf::st_geometry(existing_projected[old_rows, ])))
      new_union <- sf::st_sf(geometry = sf::st_union(sf::st_geometry(new_row)))
      symdiff <- nm_build_symdiff_area_acres(old_union, new_union)
      area_delta <- (
        as.numeric(sf::st_area(new_union)) - as.numeric(sf::st_area(old_union))
      ) * NM_ACRES_PER_SQUARE_METER
    }
    data.frame(
      monument_id = monument_id,
      canonical_name = reference$canonical_name[[index]],
      existing_row_count = length(old_rows),
      exact_durable_id_join = exact_id,
      name_only_candidate_join = FALSE,
      existing_names = old_names,
      existing_agencies = old_agencies,
      renamed_for_public_display = exact_id &&
        !identical(nm_build_normalize_text(old_names), nm_build_normalize_text(reference$canonical_name[[index]])),
      boundary_symmetric_difference_acres = symdiff,
      current_minus_existing_area_acres = area_delta,
      migration_status = if (exact_id) {
        "current semantic retained; legacy component rows replaced by one reconciled semantic boundary"
      } else {
        "current semantic missing from legacy BRIM and added from authoritative USFS/NPS source"
      },
      ambiguous = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

build_national_monuments_candidate <- function(
  snapshot_dir,
  tule_snapshot_dir,
  output_dir,
  config_dir = "00_config",
  existing_raw_path = NULL,
  simplify_tolerance_m = NM_SELECTED_SIMPLIFY_TOLERANCE_M
) {
  nm_build_require_packages(c("sf", "jsonlite", "digest"))
  snapshot <- nm_build_validate_snapshot(snapshot_dir)
  tule_snapshot <- nm_build_validate_tule_snapshot(tule_snapshot_dir)
  config_paths <- nm_build_config_paths(config_dir)
  research <- nm_build_validate_research(config_paths)
  tables <- research$tables
  roles <- tables$source_roles
  reference <- tables$reference
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_paths <- c(
    candidate_rds = file.path(output_dir, "reference_monuments_wgs84_current_candidate.rds"),
    candidate_gpkg = file.path(output_dir, "reference_monuments_wgs84_current_candidate.gpkg"),
    source_profile = file.path(output_dir, "national_monuments_source_profile.csv"),
    feature_profile = file.path(output_dir, "national_monuments_source_feature_geometry_qa.csv"),
    roles = file.path(output_dir, "national_monuments_source_geometry_roles_reconciled.csv"),
    normalized = file.path(output_dir, "national_monuments_normalized_geometry_qa.csv"),
    benchmark = file.path(output_dir, "national_monuments_simplification_benchmark.csv"),
    source_overlap = file.path(output_dir, "national_monuments_source_overlap_reconciliation.csv"),
    semantic_overlap = file.path(output_dir, "national_monuments_semantic_overlap_qa.csv"),
    tule_components = file.path(output_dir, "national_monuments_tule_lake_component_geometry_qa.csv"),
    migration = file.path(output_dir, "national_monuments_existing_brim_migration_crosswalk.csv"),
    summary = file.path(output_dir, "national_monuments_candidate_summary.json")
  )
  nm_build_assert_unused(output_paths)

  source_keys <- c(
    "blm_current", "usfs_current", "usfs_legal_status", "nps_current",
    "fws_tule_lake"
  )
  source_snapshots <- c(
    stats::setNames(rep(list(snapshot), 4L), source_keys[1:4]),
    list(fws_tule_lake = tule_snapshot)
  )
  native <- stats::setNames(lapply(source_keys, function(key) {
    nm_build_read_source(source_snapshots[[key]], key, native = TRUE)
  }), source_keys)
  derivative <- stats::setNames(lapply(source_keys, function(key) {
    nm_build_read_source(source_snapshots[[key]], key, native = FALSE)
  }), source_keys)

  source_profile <- do.call(rbind, lapply(source_keys, function(key) {
    nm_build_source_profile(
      key, native[[key]], derivative[[key]], source_snapshots[[key]]$sources[[key]]
    )
  }))
  feature_profile <- do.call(rbind, lapply(source_keys, function(key) {
    nm_build_source_feature_profile(key, native[[key]], derivative[[key]], roles)
  }))
  nm_build_assert(
    nrow(feature_profile) == NM_EXPECTED_ROLE_COUNT &&
      !any(feature_profile$null_or_empty) && !anyDuplicated(paste(feature_profile$source_key, feature_profile$source_object_id)),
    "Source feature QA failed the 37-record complete/non-empty/unique contract."
  )

  selected <- do.call(rbind, list(
    nm_build_standardize_selected("blm_current", derivative$blm_current$data, roles),
    nm_build_standardize_selected("usfs_current", derivative$usfs_current$data, roles),
    nm_build_standardize_selected("nps_current", derivative$nps_current$data, roles),
    nm_build_standardize_selected("fws_tule_lake", derivative$fws_tule_lake$data, roles)
  ))
  selected <- selected[
    order(selected$monument_id, selected$component_id, method = "radix"),
    , drop = FALSE
  ]
  nm_build_assert(
    !anyDuplicated(selected$component_id) &&
      length(unique(selected$monument_id)) == NM_EXPECTED_SEMANTIC_COUNT &&
      setequal(selected$monument_id, reference$monument_id),
    "Selected display geometry set does not cover exactly 20 semantic monuments."
  )
  selected_counts <- table(selected$monument_id)
  nm_build_assert(
    nrow(selected) == NM_EXPECTED_DISPLAY_GEOMETRY_COUNT &&
      identical(unname(selected_counts["nm_ca_sand_to_snow"]), 2L) &&
      identical(unname(selected_counts["nm_ca_tule_lake"]), 2L) &&
      all(selected_counts[
        !names(selected_counts) %in% c("nm_ca_sand_to_snow", "nm_ca_tule_lake")
      ] == 1L),
    "Only Sand to Snow and Tule Lake may use two selected agency-component records."
  )
  processed <- nm_build_repair_and_simplify(selected, simplify_tolerance_m)
  benchmark <- nm_build_benchmark(processed$repaired, NM_BENCHMARK_TOLERANCES_M)
  nm_build_assert(
    benchmark$exact_part_retention[benchmark$selected] &&
      benchmark$invalid_feature_count[benchmark$selected] == 0L &&
      benchmark$empty_feature_count[benchmark$selected] == 0L,
    "Selected one-metre simplification did not pass part/validity/empty QA."
  )

  candidate <- sf::st_transform(processed$display, 4326)
  reference_index <- match(candidate$monument_id, reference$monument_id)
  candidate$canonical_name <- reference$canonical_name[reference_index]
  candidate$pt_nickname <- "monuments"
  candidate$pt_display_name <- "National Monuments *"
  candidate$pt_geom_type <- "polygon"
  candidate$pt_namecolumn <- "canonical_name"
  candidate$pt_colorbycolumn <- "pt_nm_display_agency_key"
  candidate$pt_popup_spec <- ""
  candidate$pt_label_field <- "canonical_name"
  candidate$pt_simplify_keep <- 1
  candidate$pt_nm_simplify_tolerance_m <- as.numeric(simplify_tolerance_m)
  candidate$source <- "Reference layer"

  source_overlap <- nm_build_overlap_reconciliation(
    derivative$blm_current$data,
    derivative$usfs_current$data,
    derivative$usfs_legal_status$data
  )
  semantic_overlap <- nm_build_semantic_overlap_qa(processed$repaired)
  source_urls <- stats::setNames(vapply(source_keys, function(key) {
    as.character(source_snapshots[[key]]$sources[[key]]$layer_url)
  }, character(1)), source_keys)
  tule_components <- nm_build_tule_lake_component_qa(processed, source_urls)
  migration <- if (!is.null(existing_raw_path)) {
    nm_build_migration_crosswalk(existing_raw_path, reference, roles, processed$repaired)
  } else {
    data.frame(note = "Existing BRIM raw path not supplied", stringsAsFactors = FALSE)
  }

  candidate_metadata <- list(
    candidate_version = NM_CANDIDATE_VERSION,
    built_utc = nm_build_utc_now(),
    source_snapshot_id = snapshot$manifest$snapshot_id,
    source_retrieval_started_utc = snapshot$manifest$retrieval_started_utc,
    source_retrieval_completed_utc = snapshot$manifest$retrieval_completed_utc,
    source_manifest_sha256 = nm_build_sha256(file.path(snapshot$root, "acquisition_manifest.json")),
    tule_lake_source_snapshot_id = tule_snapshot$manifest$snapshot_id,
    tule_lake_source_retrieval_started_utc = tule_snapshot$manifest$retrieval_started_utc,
    tule_lake_source_retrieval_completed_utc = tule_snapshot$manifest$retrieval_completed_utc,
    tule_lake_source_manifest_sha256 = nm_build_sha256(
      file.path(tule_snapshot$root, "acquisition_manifest.json")
    ),
    source_counts = as.list(stats::setNames(source_profile$source_feature_count, source_profile$source_key)),
    selected_semantic_count = length(unique(candidate$monument_id)),
    selected_geometry_feature_count = nrow(candidate),
    modeled_component_row_count = nrow(tables$components),
    raw_selected_projected_invalid_count = sum(!processed$qa$raw_projected_valid),
    repaired_invalid_count = sum(!processed$qa$repaired_valid),
    raw_selected_polygon_parts = sum(processed$qa$raw_polygon_parts),
    repaired_polygon_parts = sum(processed$qa$repaired_polygon_parts),
    display_polygon_parts = sum(processed$qa$display_polygon_parts),
    raw_selected_vertices = sum(processed$qa$raw_vertices),
    repaired_vertices = sum(processed$qa$repaired_vertices),
    display_vertices = sum(processed$qa$display_vertices),
    repair_area_change_acres = sum(processed$qa$repair_area_change_acres),
    display_area_change_acres = sum(processed$qa$display_area_change_acres),
    maximum_absolute_display_area_change_acres = max(abs(processed$qa$display_area_change_acres)),
    maximum_absolute_display_area_change_percent = max(abs(processed$qa$display_area_change_percent)),
    simplify_method = "sf::st_simplify(preserveTopology=TRUE)",
    simplify_crs = "EPSG:3310",
    simplify_tolerance_m = as.numeric(simplify_tolerance_m),
    exact_part_retention = identical(
      processed$qa$repaired_polygon_parts,
      processed$qa$display_polygon_parts
    ),
    source_geometry_role = "authoritative current boundary after reviewed multi-source role selection",
    display_geometry_role = paste(
      "one reconciled complete boundary per semantic National Monument except",
      "Sand to Snow and Tule Lake, each represented by truthful agency components"
    ),
    tule_lake_constituent_count = nrow(tule_components),
    tule_lake_semantic_acres = sum(tule_components$repaired_acres_epsg3310),
    config_sha256 = as.list(research$hashes),
    replacement_authorized = FALSE
  )
  attr(candidate, "pt_national_monuments_candidate_metadata") <- candidate_metadata

  saveRDS(candidate, output_paths[["candidate_rds"]], compress = "xz")
  sf::st_write(
    candidate,
    output_paths[["candidate_gpkg"]],
    layer = "national_monuments_candidate",
    quiet = TRUE
  )
  utils::write.csv(source_profile, output_paths[["source_profile"]], row.names = FALSE, na = "")
  utils::write.csv(feature_profile, output_paths[["feature_profile"]], row.names = FALSE, na = "")
  utils::write.csv(roles, output_paths[["roles"]], row.names = FALSE, na = "")
  utils::write.csv(processed$qa, output_paths[["normalized"]], row.names = FALSE, na = "")
  utils::write.csv(benchmark, output_paths[["benchmark"]], row.names = FALSE, na = "")
  utils::write.csv(source_overlap, output_paths[["source_overlap"]], row.names = FALSE, na = "")
  utils::write.csv(semantic_overlap, output_paths[["semantic_overlap"]], row.names = FALSE, na = "")
  utils::write.csv(tule_components, output_paths[["tule_components"]], row.names = FALSE, na = "")
  utils::write.csv(migration, output_paths[["migration"]], row.names = FALSE, na = "")
  candidate_metadata$candidate_rds_bytes <- unname(file.info(output_paths[["candidate_rds"]])$size)
  candidate_metadata$candidate_rds_sha256 <- nm_build_sha256(output_paths[["candidate_rds"]])
  candidate_metadata$candidate_gpkg_bytes <- unname(file.info(output_paths[["candidate_gpkg"]])$size)
  candidate_metadata$candidate_gpkg_sha256 <- nm_build_sha256(output_paths[["candidate_gpkg"]])
  jsonlite::write_json(
    candidate_metadata,
    output_paths[["summary"]],
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null",
    digits = 17
  )
  message("National Monuments candidate built; no processed/cache file was promoted.")
  invisible(list(
    candidate = candidate,
    metadata = candidate_metadata,
    outputs = output_paths
  ))
}

nm_build_parse_args <- function(arguments) {
  values <- list(
    snapshot = NULL, tule_snapshot = NULL, output_dir = NULL,
    config_dir = "00_config", existing_raw = NULL
  )
  index <- 1L
  while (index <= length(arguments)) {
    key <- arguments[[index]]
    if (!(key %in% c(
      "--snapshot", "--tule-snapshot", "--output-dir", "--config-dir", "--existing-raw"
    ))) {
      nm_build_stop("Unknown argument: ", key)
    }
    if (index == length(arguments)) nm_build_stop("Missing value for ", key)
    values[[gsub("-", "_", sub("^--", "", key))]] <- arguments[[index + 1L]]
    index <- index + 2L
  }
  if (is.null(values$snapshot) || is.null(values$tule_snapshot) || is.null(values$output_dir)) {
    nm_build_stop("--snapshot, --tule-snapshot, and --output-dir are required")
  }
  values
}

if (sys.nframe() == 0L) {
  arguments <- nm_build_parse_args(commandArgs(trailingOnly = TRUE))
  build_national_monuments_candidate(
    snapshot_dir = arguments$snapshot,
    tule_snapshot_dir = arguments$tule_snapshot,
    output_dir = arguments$output_dir,
    config_dir = arguments$config_dir,
    existing_raw_path = arguments$existing_raw
  )
}
