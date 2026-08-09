# ==== build_acec_current_candidate.R =======================================
## Focused, fail-closed builder for the current authoritative BLM California
## ACEC snapshot. Candidate creation and promotion are deliberately separate.

pt_acec_required_packages <- c("sf", "jsonlite", "digest")
pt_acec_missing_packages <- pt_acec_required_packages[
  !vapply(pt_acec_required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(pt_acec_missing_packages)) {
  stop("ACEC pipeline requires: ", paste(pt_acec_missing_packages, collapse = ", "))
}

pt_acec_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

pt_acec_sha256 <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}

pt_acec_normalize_guid <- function(x) {
  value <- tolower(gsub("[{}[:space:]]", "", trimws(as.character(x))))
  value[is.na(value) | !nzchar(value)] <- NA_character_
  value
}

pt_acec_component_id <- function(x) {
  value <- pt_acec_normalize_guid(x)
  ifelse(startsWith(value, "blmca-"), value, paste0("blmca-", value))
}

pt_acec_clean_compare <- function(x) {
  value <- trimws(as.character(x))
  value[is.na(value) | value %in% c("NA", "N/A", "<NA>")] <- ""
  value
}

pt_acec_epoch_as_package_stamp <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  out <- rep("", length(value))
  valid <- is.finite(value)
  out[valid] <- format(
    as.POSIXct(value[valid] / 1000, origin = "1970-01-01", tz = "UTC"),
    tz = "America/Los_Angeles", format = "%Y/%m/%d %H:%M:%S+00"
  )
  out
}

pt_acec_vertex_count <- function(x) {
  vapply(sf::st_geometry(x), function(geometry) {
    tryCatch(nrow(sf::st_coordinates(geometry)), error = function(e) NA_integer_)
  }, integer(1))
}

pt_acec_part_count <- function(x) {
  vapply(seq_len(nrow(x)), function(i) {
    length(suppressWarnings(sf::st_cast(sf::st_geometry(x[i, , drop = FALSE]), "POLYGON")))
  }, integer(1))
}

pt_acec_assert_unused <- function(paths) {
  existing <- paths[file.exists(paths)]
  pt_acec_assert(!length(existing), paste(
    "Refusing to overwrite ACEC candidate output(s):", paste(existing, collapse = ", ")
  ))
}

build_acec_current_candidate <- function(
  raw_geojson,
  enrichment_zip,
  config_dir = "00_config",
  output_dir,
  simplify_tolerance_m = 1,
  expected_records = 238L,
  expected_parts = 613L
) {
  expected_package_sha256 <-
    "caa62b56366a4490c8df6cb3bb051b8eb3970c9b55f36b3872de9e973424fb09"
  simplify_tolerance_m <- as.numeric(simplify_tolerance_m)
  pt_acec_assert(
    length(simplify_tolerance_m) == 1L && is.finite(simplify_tolerance_m) &&
      simplify_tolerance_m >= 0 && simplify_tolerance_m <= 5,
    "simplify_tolerance_m must be one finite value from zero through five metres."
  )
  pt_acec_assert(file.exists(raw_geojson), paste("Missing raw snapshot:", raw_geojson))
  pt_acec_assert(file.exists(enrichment_zip), paste("Missing enrichment ZIP:", enrichment_zip))
  pt_acec_assert(
    identical(pt_acec_sha256(enrichment_zip), expected_package_sha256),
    "ACEC enrichment ZIP checksum differs from the reviewed package."
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_paths <- c(
    candidate = file.path(output_dir, "reference_acec_wgs84_current_candidate.rds"),
    geometry_qa = file.path(output_dir, "acec_current_geometry_qa.csv"),
    reconciliation_qa = file.path(output_dir, "acec_current_reconciliation_qa.csv"),
    summary = file.path(output_dir, "acec_current_candidate_summary.json")
  )
  pt_acec_assert_unused(output_paths)

  package_dir <- tempfile("brim_acec_enrichment_")
  dir.create(package_dir, recursive = TRUE)
  on.exit(unlink(package_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(enrichment_zip, exdir = package_dir)
  package_root <- dirname(list.files(
    package_dir, pattern = "^brim_ca_acec_components\\.csv$",
    recursive = TRUE, full.names = TRUE
  )[[1]])
  historical_shapefile <- file.path(package_root, "source_snapshot", "supplied_acec.shp")
  historical_records_path <- file.path(
    package_root, "source_snapshot", "blm_ca_acec_live_records.csv"
  )
  pt_acec_assert(file.exists(historical_shapefile), "Package historical shapefile is missing.")
  pt_acec_assert(file.exists(historical_records_path), "Package source-record baseline is missing.")

  source_raw <- sf::st_read(raw_geojson, quiet = TRUE, stringsAsFactors = FALSE)
  expected_source_fields <- c(
    "OBJECTID", "GlobalID", "ACEC_NAME", "LUP_NAME", "NEPA_NUM", "ROD_DATE",
    "GIS_ACRES", "ADMIN_ST", "CA_ADMIN_unit_code", "BLM_MODIFY_DATE",
    "last_edited_date", "ACEC_RLVNCE_CUL", "ACEC_RLVNCE_FRSC",
    "ACEC_RLVNCE_HIS", "ACEC_RLVNCE_NHAZ", "ACEC_RLVNCE_NPRO",
    "ACEC_RLVNCE_NSYS", "ACEC_RLVNCE_SCE", "ACEC_RLVNCE_WRSC",
    "ACEC_IMPRTNCE_CNTRBTN", "ACEC_IMPRTNCE_IMPRTNCE", "ACEC_IMPRTNCE_QLTS",
    "ACEC_IMPRTNCE_THRT", "SPCL_MGMT_ATTN_RX_PRTCT", "SPCL_MGMT_ATTN_RX_PRVNT",
    "Shape__Area", "Shape__Length"
  )
  source_fields <- setdiff(names(source_raw), attr(source_raw, "sf_column"))
  pt_acec_assert(
    identical(sort(source_fields), sort(expected_source_fields)),
    paste("Current ACEC source schema differs; fields:", paste(source_fields, collapse = ", "))
  )
  pt_acec_assert(nrow(source_raw) == expected_records, "Current ACEC snapshot must contain 238 records.")
  normalized_gid <- pt_acec_normalize_guid(source_raw$GlobalID)
  pt_acec_assert(!anyNA(normalized_gid) && !anyDuplicated(normalized_gid),
                 "Current ACEC GlobalIDs must be complete and unique.")
  pt_acec_assert(!is.na(sf::st_crs(source_raw)), "Current ACEC snapshot CRS is missing.")
  source_raw <- sf::st_transform(source_raw, 4326)
  source_raw$component_id <- pt_acec_component_id(source_raw$GlobalID)

  components <- utils::read.csv(
    file.path(config_dir, "local_reference_acec_components.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  reference <- utils::read.csv(
    file.path(config_dir, "local_reference_acec_reference.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  pt_acec_assert(nrow(components) == expected_records && !anyDuplicated(components$component_id),
                 "ACEC component crosswalk must contain 238 unique component IDs.")
  pt_acec_assert(nrow(reference) == expected_records && !anyDuplicated(reference$acec_id),
                 "ACEC semantic reference must contain 238 unique ACEC IDs.")
  pt_acec_assert(setequal(source_raw$component_id, components$component_id),
                 "Fresh source GlobalIDs do not exactly match the reviewed component crosswalk.")
  source_raw$acec_id <- components$acec_id[match(source_raw$component_id, components$component_id)]
  pt_acec_assert(setequal(source_raw$acec_id, reference$acec_id),
                 "Fresh geometry and semantic ACEC IDs are not an exact set match.")
  pt_acec_assert(
    sum(reference$official_acec_name == "Black Mountain") == 2L &&
      length(unique(reference$acec_id[reference$official_acec_name == "Black Mountain"])) == 2L,
    "The two Black Mountain ACECs must remain separate."
  )

  historical <- sf::st_read(historical_shapefile, quiet = TRUE, stringsAsFactors = FALSE)
  pt_acec_assert(nrow(historical) == expected_records, "Historical QA baseline must contain 238 records.")
  historical_parts <- pt_acec_part_count(historical)
  historical_valid <- sf::st_is_valid(historical)
  pt_acec_assert(sum(historical_parts) == expected_parts,
                 "Historical QA baseline must retain 613 polygon parts.")
  pt_acec_assert(sum(!historical_valid) == 14L,
                 "Historical QA baseline validity count differs from the reviewed package.")
  historical_records <- utils::read.csv(
    historical_records_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  historical_gid <- pt_acec_normalize_guid(historical_records$GlobalID)
  pt_acec_assert(setequal(normalized_gid, historical_gid),
                 "Fresh and package snapshot GlobalID sets differ.")
  historical_index <- match(normalized_gid, historical_gid)
  meaningful_fields <- c(
    "ACEC_NAME", "LUP_NAME", "NEPA_NUM", "ADMIN_ST", "CA_ADMIN_unit_code",
    "ACEC_RLVNCE_CUL", "ACEC_RLVNCE_FRSC", "ACEC_RLVNCE_HIS",
    "ACEC_RLVNCE_NHAZ", "ACEC_RLVNCE_NPRO", "ACEC_RLVNCE_NSYS",
    "ACEC_RLVNCE_SCE", "ACEC_RLVNCE_WRSC", "ACEC_IMPRTNCE_CNTRBTN",
    "ACEC_IMPRTNCE_IMPRTNCE", "ACEC_IMPRTNCE_QLTS", "ACEC_IMPRTNCE_THRT",
    "SPCL_MGMT_ATTN_RX_PRTCT", "SPCL_MGMT_ATTN_RX_PRVNT"
  )
  meaningful_match <- vapply(seq_len(nrow(source_raw)), function(i) {
    all(vapply(meaningful_fields, function(field) {
      identical(
        pt_acec_clean_compare(source_raw[[field]][[i]]),
        pt_acec_clean_compare(historical_records[[field]][[historical_index[[i]]]])
      )
    }, logical(1)))
  }, logical(1))
  date_match <- vapply(c("ROD_DATE", "BLM_MODIFY_DATE", "last_edited_date"), function(field) {
    identical(
      pt_acec_epoch_as_package_stamp(source_raw[[field]]),
      pt_acec_clean_compare(historical_records[[field]][historical_index])
    )
  }, logical(1))
  pt_acec_assert(all(meaningful_match) && all(date_match),
                 "Fresh source attributes differ materially from the package snapshot.")

  raw_valid <- sf::st_is_valid(source_raw)
  raw_parts <- pt_acec_part_count(source_raw)
  raw_vertices <- pt_acec_vertex_count(source_raw)
  pt_acec_assert(sum(raw_parts) == expected_parts, "Fresh source must retain 613 polygon parts.")
  pt_acec_assert(sum(!raw_valid) == 14L, "Fresh source invalid-geometry count must remain 14.")

  processing_3310 <- sf::st_transform(source_raw, 3310)
  raw_area_acres <- as.numeric(sf::st_area(processing_3310)) / 4046.8564224
  processing_3310 <- sf::st_make_valid(processing_3310)
  pt_acec_assert(nrow(processing_3310) == expected_records,
                 "st_make_valid changed ACEC feature count.")
  if (simplify_tolerance_m > 0) {
    processing_3310 <- sf::st_simplify(
      processing_3310, dTolerance = simplify_tolerance_m, preserveTopology = TRUE
    )
  }
  processed_valid <- sf::st_is_valid(processing_3310)
  processed_parts <- pt_acec_part_count(processing_3310)
  processed_vertices <- pt_acec_vertex_count(processing_3310)
  processed_area_acres <- as.numeric(sf::st_area(processing_3310)) / 4046.8564224
  pt_acec_assert(nrow(processing_3310) == expected_records &&
                   setequal(processing_3310$component_id, source_raw$component_id),
                 "ACEC geometry processing changed record identity.")
  pt_acec_assert(all(processed_valid) && !any(sf::st_is_empty(processing_3310)),
                 "Processed ACEC candidate must be valid and non-empty.")
  pt_acec_assert(sum(processed_parts) == expected_parts,
                 "Processed ACEC candidate must retain all 613 polygon parts.")

  candidate <- sf::st_transform(processing_3310, 4326)
  candidate$pt_nickname <- "acec"
  candidate$pt_display_name <- "ACECs"
  candidate$pt_geom_type <- "polygon"
  candidate$pt_namecolumn <- "ACEC_NAME"
  candidate$pt_colorbycolumn <- "ACEC_NAME"
  candidate$pt_popup_spec <- ""
  candidate$pt_label_field <- "ACEC_NAME"
  candidate$pt_simplify_keep <- 1
  candidate$source <- "Reference layer"
  candidate_metadata <- list(
    source_authority = paste0(
      "https://gis.blm.gov/caarcgis/rest/services/Planning/",
      "BLM_CA_ACEC/FeatureServer/0"
    ),
    raw_snapshot = normalizePath(raw_geojson, winslash = "/"),
    raw_snapshot_sha256 = pt_acec_sha256(raw_geojson),
    enrichment_zip_sha256 = expected_package_sha256,
    source_record_count = nrow(source_raw),
    semantic_acec_count = length(unique(candidate$acec_id)),
    raw_geometry_parts = sum(raw_parts),
    processed_geometry_parts = sum(processed_parts),
    raw_invalid_geometries = sum(!raw_valid),
    processed_invalid_geometries = sum(!processed_valid),
    raw_vertex_count = sum(raw_vertices),
    processed_vertex_count = sum(processed_vertices),
    simplify_method = "sf::st_simplify(preserveTopology=TRUE)",
    simplify_tolerance_m = simplify_tolerance_m,
    source_geometry_provenance = "fresh current BLM FeatureServer snapshot",
    historical_geometry_role = "QA baseline only",
    replacement_authorized = FALSE
  )
  attr(candidate, "pt_acec_candidate_metadata") <- candidate_metadata

  geometry_qa <- data.frame(
    component_id = source_raw$component_id,
    acec_id = source_raw$acec_id,
    source_name = source_raw$ACEC_NAME,
    raw_geometry_valid = raw_valid,
    processed_geometry_valid = processed_valid,
    raw_geometry_part_count = raw_parts,
    processed_geometry_part_count = processed_parts,
    raw_vertex_count = raw_vertices,
    processed_vertex_count = processed_vertices,
    raw_area_acres = round(raw_area_acres, 6),
    processed_area_acres = round(processed_area_acres, 6),
    area_change_acres = round(processed_area_acres - raw_area_acres, 6),
    area_change_percent = round(
      100 * (processed_area_acres - raw_area_acres) / raw_area_acres, 6
    ),
    simplify_tolerance_m = simplify_tolerance_m,
    stringsAsFactors = FALSE
  )
  reconciliation_qa <- data.frame(
    component_id = source_raw$component_id,
    acec_id = source_raw$acec_id,
    current_name = source_raw$ACEC_NAME,
    package_snapshot_name = historical_records$ACEC_NAME[historical_index],
    exact_globalid_match = !is.na(historical_index),
    normalized_name_match = tolower(gsub("[^a-z0-9]+", " ", source_raw$ACEC_NAME)) ==
      tolower(gsub("[^a-z0-9]+", " ", historical_records$ACEC_NAME[historical_index])),
    meaningful_attribute_match = meaningful_match,
    rod_date_match = pt_acec_epoch_as_package_stamp(source_raw$ROD_DATE) ==
      pt_acec_clean_compare(historical_records$ROD_DATE[historical_index]),
    blm_modify_date_match = pt_acec_epoch_as_package_stamp(source_raw$BLM_MODIFY_DATE) ==
      pt_acec_clean_compare(historical_records$BLM_MODIFY_DATE[historical_index]),
    last_edited_date_match = pt_acec_epoch_as_package_stamp(source_raw$last_edited_date) ==
      pt_acec_clean_compare(historical_records$last_edited_date[historical_index]),
    current_gis_acres = source_raw$GIS_ACRES,
    package_snapshot_gis_acres = historical_records$GIS_ACRES[historical_index],
    service_shape_area_drift_m2 = source_raw$Shape__Area -
      historical_records$Shape_Area[historical_index],
    service_shape_length_drift_m = source_raw$Shape__Length -
      historical_records$Shape_Length[historical_index],
    stringsAsFactors = FALSE
  )

  saveRDS(candidate, output_paths[["candidate"]], compress = "xz")
  utils::write.csv(geometry_qa, output_paths[["geometry_qa"]], row.names = FALSE, na = "")
  utils::write.csv(
    reconciliation_qa, output_paths[["reconciliation_qa"]], row.names = FALSE, na = ""
  )
  candidate_metadata$candidate_sha256 <- pt_acec_sha256(output_paths[["candidate"]])
  candidate_metadata$max_absolute_area_change_acres <- max(abs(geometry_qa$area_change_acres))
  candidate_metadata$max_absolute_area_change_percent <- max(abs(geometry_qa$area_change_percent))
  candidate_metadata$all_current_globalids_match_package_snapshot <-
    all(reconciliation_qa$exact_globalid_match)
  candidate_metadata$all_meaningful_attributes_match_package_snapshot <-
    all(reconciliation_qa$meaningful_attribute_match)
  candidate_metadata$all_source_dates_match_package_snapshot_in_service_timezone <-
    all(reconciliation_qa$rod_date_match) && all(reconciliation_qa$blm_modify_date_match) &&
    all(reconciliation_qa$last_edited_date_match)
  candidate_metadata$maximum_service_shape_area_serialization_drift_m2 <-
    max(abs(reconciliation_qa$service_shape_area_drift_m2), na.rm = TRUE)
  jsonlite::write_json(candidate_metadata, output_paths[["summary"]], pretty = TRUE, auto_unbox = TRUE)
  message("ACEC candidate built; current processed RDS was not changed.")
  invisible(c(list(data = candidate), as.list(output_paths)))
}

promote_acec_candidate <- function(candidate_path, final_path, archive_path) {
  pt_acec_assert(file.exists(candidate_path), paste("Missing reviewed candidate:", candidate_path))
  candidate <- readRDS(candidate_path)
  pt_acec_assert(inherits(candidate, "sf") && nrow(candidate) == 238L,
                 "Reviewed ACEC candidate must be an sf object with 238 rows.")
  pt_acec_assert(length(unique(candidate$acec_id)) == 238L,
                 "Reviewed ACEC candidate must contain 238 semantic ACECs.")
  pt_acec_assert(sum(pt_acec_part_count(candidate)) == 613L,
                 "Reviewed ACEC candidate must retain 613 polygon parts.")
  pt_acec_assert(all(sf::st_is_valid(candidate)),
                 "Reviewed ACEC candidate must contain only valid geometries.")
  pt_acec_assert(!identical(
    normalizePath(candidate_path), normalizePath(final_path, mustWork = FALSE)
  ), "Candidate and final paths must be different.")
  dir.create(dirname(final_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(archive_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(final_path)) {
    pt_acec_assert(!file.exists(archive_path), paste("Refusing to overwrite archive:", archive_path))
    pt_acec_assert(file.copy(final_path, archive_path, overwrite = FALSE, copy.mode = TRUE),
                   "Could not archive the prior processed ACEC RDS.")
    pt_acec_assert(identical(pt_acec_sha256(final_path), pt_acec_sha256(archive_path)),
                   "Archived ACEC RDS hash differs from the prior final RDS.")
  }
  pt_acec_assert(file.copy(candidate_path, final_path, overwrite = TRUE, copy.mode = TRUE),
                 "Could not promote the reviewed ACEC candidate.")
  pt_acec_assert(identical(pt_acec_sha256(candidate_path), pt_acec_sha256(final_path)),
                 "Promoted ACEC RDS hash differs from the reviewed candidate.")
  invisible(list(final_path = final_path, archive_path = archive_path))
}
