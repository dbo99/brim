# ==== build_federal_wilderness_197_candidate.R =============================
##
## Focused, fail-closed builder for the accepted 197-component Federal
## Wilderness source. Geometry is simplified (when explicitly requested)
## before enrichment. The accepted realistic-build setting is keep=0.5.
##
## This file does not run on source(). Candidate creation and promotion are
## separate calls so a reviewed candidate can never replace the current
## processed RDS accidentally.

pt_fw_required_packages <- c("sf", "dplyr", "jsonlite", "digest")
pt_fw_missing_packages <- pt_fw_required_packages[
  !vapply(pt_fw_required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(pt_fw_missing_packages)) {
  stop("Federal Wilderness pipeline requires: ", paste(pt_fw_missing_packages, collapse = ", "))
}

pt_fw_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

pt_fw_normalize_guid <- function(x) {
  value <- tolower(trimws(as.character(x)))
  value <- gsub("[{}]", "", value)
  ifelse(is.na(value) | !nzchar(value), NA_character_, paste0("blmca-", value))
}

pt_fw_vertex_count <- function(x) {
  vapply(sf::st_geometry(x), function(geometry) {
    tryCatch(nrow(sf::st_coordinates(geometry)), error = function(e) NA_integer_)
  }, integer(1))
}

pt_fw_sha256 <- function(path) {
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}

pt_fw_validate_input_hashes <- function(source_zip, config_dir) {
  expected <- c(
    source_zip = "cdfe3330e3828adee3a0e24a4644ea2421cde5c9cf2f9b9decf7922554374f6f",
    local_reference_federal_wilderness_components.csv = "d3d09160bba08a6ca277d5627321b2804963d85574d272fa4e8de66bd7355d46",
    local_reference_federal_wilderness_reference.csv = "89d13dea5110460b68bca9e11d1f7f3f4dde9a4b4b9acf2c62580bb38c3c38d1",
    local_reference_federal_wilderness_designation_validation.csv = "f1b9e883fa395ef6dd0fcb071354930e2f1012dd8fbd3d97d5ff13da82f4cfd4",
    local_reference_federal_wilderness_documents.csv = "6d8baa00f44497de2ea89d43b361ec6301c80683beb837a38f27901a40462581",
    local_reference_federal_wilderness_common_policy_language.csv = "9f13199af2803330bb75de2c54bbbee25ac1b718cb3f6f18752dc30e26edd465",
    local_reference_federal_wilderness_source_register.csv = "a24b4b6aa4b1bdab6e4a3ec5fc63be3742d3e836429bd24bb7fc63cde23aa2d0"
  )
  paths <- c(
    source_zip = source_zip,
    stats::setNames(
      file.path(config_dir, names(expected)[-1]),
      names(expected)[-1]
    )
  )
  missing <- paths[!file.exists(paths)]
  pt_fw_assert(!length(missing), paste("Missing accepted input(s):", paste(missing, collapse = ", ")))
  actual <- vapply(paths, pt_fw_sha256, character(1))
  pt_fw_assert(
    identical(unname(actual[names(expected)]), unname(expected)),
    paste(
      "Federal Wilderness input checksum mismatch:",
      paste(names(expected), actual[names(expected)], sep = "=", collapse = "; ")
    )
  )
  actual
}

build_federal_wilderness_197_candidate <- function(
  source_zip,
  config_dir = "00_config",
  output_dir,
  simplify_keep = 0.5,
  expected_components = 197L,
  expected_wildernesses = 158L
) {
  simplify_keep <- as.numeric(simplify_keep)
  pt_fw_assert(
    length(simplify_keep) == 1L && is.finite(simplify_keep) &&
      simplify_keep > 0 && simplify_keep <= 1,
    "simplify_keep must be one finite value greater than zero and at most one."
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  input_hashes <- pt_fw_validate_input_hashes(source_zip, config_dir)

  unzip_dir <- tempfile("brim_federal_wilderness_197_")
  dir.create(unzip_dir, recursive = TRUE)
  on.exit(unlink(unzip_dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(source_zip, exdir = unzip_dir)
  shapefile <- list.files(
    unzip_dir, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE
  )
  pt_fw_assert(length(shapefile) == 1L, "Accepted source ZIP must contain exactly one shapefile.")

  source_raw <- sf::st_read(shapefile, quiet = TRUE, stringsAsFactors = FALSE)
  original_attribute_names <- setdiff(names(source_raw), attr(source_raw, "sf_column"))
  expected_source_fields <- c(
    "OBJECTID", "GlobalID", "FAU_ID", "NLCS_NAME", "ADMIN_ST", "ManagingAg",
    "GIS_Acres", "Shape_Leng", "Shape_Area"
  )
  missing_source_fields <- setdiff(expected_source_fields, names(source_raw))
  pt_fw_assert(
    !length(missing_source_fields),
    paste("Accepted source schema is missing:", paste(missing_source_fields, collapse = ", "))
  )
  pt_fw_assert(nrow(source_raw) == expected_components, "Accepted source must contain 197 components.")
  pt_fw_assert(!anyNA(source_raw$GlobalID) && !anyDuplicated(source_raw$GlobalID),
               "GlobalID must be complete and unique across all 197 components.")
  pt_fw_assert(length(unique(source_raw$FAU_ID)) == expected_wildernesses,
               "FAU_ID must identify exactly 158 named wildernesses.")
  pt_fw_assert(sum(source_raw$ADMIN_ST == "CA", na.rm = TRUE) == 194L &&
                 sum(source_raw$ADMIN_ST == "NV", na.rm = TRUE) == 3L,
               "Accepted source must retain 194 California and three Nevada components.")
  expected_agencies <- c(`4` = 2L, `5` = 15L, `6` = 105L, `8` = 75L)
  actual_agencies <- table(factor(as.character(source_raw$ManagingAg), levels = names(expected_agencies)))
  pt_fw_assert(identical(as.integer(actual_agencies), unname(expected_agencies)),
               "ManagingAg counts differ from the accepted 2/15/105/75 contract.")
  pt_fw_assert(!is.na(sf::st_crs(source_raw)), "Accepted source CRS is missing.")
  pt_fw_assert(identical(sf::st_crs(source_raw)$epsg, 3310L),
               "Accepted source must be EPSG:3310; do not assign or infer its CRS.")

  source_raw$component_id <- pt_fw_normalize_guid(source_raw$GlobalID)
  source_raw$wilderness_id <- paste0("fw-", as.character(source_raw$FAU_ID))
  source_3310 <- source_raw
  raw_valid <- sf::st_is_valid(source_3310)
  raw_vertices <- pt_fw_vertex_count(source_3310)
  raw_area_acres <- as.numeric(sf::st_area(source_3310)) / 4046.8564224

  processing_geom <- sf::st_make_valid(source_3310)
  pt_fw_assert(nrow(processing_geom) == expected_components, "st_make_valid changed feature count.")
  if (simplify_keep < 1) {
    pt_fw_assert(requireNamespace("rmapshaper", quietly = TRUE),
                 "rmapshaper is required only when simplify_keep is below one.")
    processing_geom <- rmapshaper::ms_simplify(
      processing_geom,
      keep = simplify_keep,
      keep_shapes = TRUE,
      explode = FALSE
    )
  }
  pt_fw_assert(nrow(processing_geom) == expected_components, "Geometry processing changed feature count.")
  pt_fw_assert(setequal(processing_geom$component_id, source_3310$component_id),
               "Geometry processing changed component identity.")
  pt_fw_assert(!any(sf::st_is_empty(processing_geom)), "Geometry processing created empty geometry.")
  pt_fw_assert(all(sf::st_is_valid(processing_geom)), "Processed geometry remains invalid.")

  processed_vertices <- pt_fw_vertex_count(processing_geom)
  processed_area_acres <- as.numeric(sf::st_area(processing_geom)) / 4046.8564224
  geometry_qa <- data.frame(
    component_id = source_3310$component_id,
    wilderness_id = source_3310$wilderness_id,
    source_name = source_3310$NLCS_NAME,
    admin_state = source_3310$ADMIN_ST,
    raw_geometry_valid = raw_valid,
    processed_geometry_valid = sf::st_is_valid(processing_geom),
    raw_vertex_count = raw_vertices,
    processed_vertex_count = processed_vertices,
    vertex_reduction_count = raw_vertices - processed_vertices,
    raw_area_acres = round(raw_area_acres, 4),
    processed_area_acres = round(processed_area_acres, 4),
    area_change_acres = round(processed_area_acres - raw_area_acres, 4),
    simplify_keep = simplify_keep,
    stringsAsFactors = FALSE
  )

  components_path <- file.path(config_dir, "local_reference_federal_wilderness_components.csv")
  reference_path <- file.path(config_dir, "local_reference_federal_wilderness_reference.csv")
  components <- utils::read.csv(components_path, stringsAsFactors = FALSE, check.names = FALSE)
  reference <- utils::read.csv(reference_path, stringsAsFactors = FALSE, check.names = FALSE)
  pt_fw_assert(nrow(components) == expected_components && !anyDuplicated(components$component_id),
               "Component enrichment must contain exactly 197 unique component IDs.")
  pt_fw_assert(nrow(reference) == expected_wildernesses && !anyDuplicated(reference$wilderness_id),
               "Semantic enrichment must contain exactly 158 unique wilderness IDs.")
  pt_fw_assert(setequal(processing_geom$component_id, components$component_id),
               "Geometry and component enrichment IDs are not an exact set match.")
  component_pair_source <- paste(processing_geom$component_id, processing_geom$wilderness_id, sep = "|")
  component_pair_lookup <- paste(components$component_id, components$wilderness_id, sep = "|")
  pt_fw_assert(setequal(component_pair_source, component_pair_lookup),
               "A component-to-wilderness relationship differs from the accepted crosswalk.")
  pt_fw_assert(setequal(unique(processing_geom$wilderness_id), reference$wilderness_id),
               "Geometry and semantic enrichment wilderness IDs are not an exact set match.")

  candidate <- dplyr::left_join(
    processing_geom,
    components,
    by = c("component_id", "wilderness_id")
  )
  candidate <- dplyr::left_join(candidate, reference, by = "wilderness_id")
  pt_fw_assert(nrow(candidate) == expected_components, "Enrichment multiplied or removed geometry.")
  pt_fw_assert(!anyNA(candidate$official_name), "Semantic enrichment join is incomplete.")
  pt_fw_assert(all(original_attribute_names %in% names(candidate)),
               "One or more untouched source attributes were not retained.")

  candidate <- sf::st_transform(candidate, 4326)
  candidate$pt_nickname <- "fedwilderness"
  candidate$pt_display_name <- "Federal Wilderness *"
  candidate$pt_geom_type <- "polygon"
  candidate$pt_namecolumn <- "pt_fw_official_name"
  candidate$pt_colorbycolumn <- "ManagingAg"
  candidate$pt_popup_spec <- ""
  candidate$pt_label_field <- "pt_fw_official_name"
  candidate$pt_simplify_keep <- simplify_keep
  candidate$source <- "Reference layer"

  candidate_path <- file.path(
    output_dir, "reference_fedwilderness_wgs84_197_candidate.rds"
  )
  geometry_qa_path <- file.path(
    output_dir, "federal_wilderness_197_geometry_qa.csv"
  )
  summary_path <- file.path(
    output_dir, "federal_wilderness_197_candidate_summary.json"
  )
  saveRDS(candidate, candidate_path, compress = "xz")
  utils::write.csv(geometry_qa, geometry_qa_path, row.names = FALSE, na = "")
  summary <- list(
    component_count = nrow(candidate),
    semantic_wilderness_count = length(unique(candidate$wilderness_id)),
    california_components = sum(candidate$ADMIN_ST == "CA", na.rm = TRUE),
    nevada_components = sum(candidate$ADMIN_ST == "NV", na.rm = TRUE),
    managing_agency_counts = as.list(stats::setNames(
      as.integer(actual_agencies), names(actual_agencies)
    )),
    simplify_keep = simplify_keep,
    raw_invalid_geometries = sum(!raw_valid),
    processed_invalid_geometries = sum(!sf::st_is_valid(candidate)),
    raw_vertex_count = sum(raw_vertices, na.rm = TRUE),
    processed_vertex_count = sum(processed_vertices, na.rm = TRUE),
    original_source_attributes_retained = original_attribute_names,
    input_sha256 = as.list(input_hashes),
    candidate_sha256 = pt_fw_sha256(candidate_path),
    replacement_authorized = FALSE
  )
  jsonlite::write_json(summary, summary_path, pretty = TRUE, auto_unbox = TRUE)
  message("Federal Wilderness candidate built; current processed RDS was not changed.")
  invisible(list(
    data = candidate,
    candidate_path = candidate_path,
    geometry_qa_path = geometry_qa_path,
    summary_path = summary_path
  ))
}

promote_federal_wilderness_197_candidate <- function(
  candidate_path,
  final_path,
  archive_path
) {
  pt_fw_assert(file.exists(candidate_path), paste("Missing reviewed candidate:", candidate_path))
  candidate <- readRDS(candidate_path)
  pt_fw_assert(inherits(candidate, "sf") && nrow(candidate) == 197L,
               "Reviewed candidate must be an sf object with exactly 197 rows.")
  pt_fw_assert(length(unique(candidate$wilderness_id)) == 158L,
               "Reviewed candidate must contain exactly 158 named wildernesses.")
  pt_fw_assert(!identical(normalizePath(candidate_path), normalizePath(final_path, mustWork = FALSE)),
               "Candidate and final paths must be different.")
  dir.create(dirname(final_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(archive_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(final_path)) {
    pt_fw_assert(!file.exists(archive_path), paste("Refusing to overwrite archive:", archive_path))
    pt_fw_assert(file.copy(final_path, archive_path, overwrite = FALSE, copy.mode = TRUE),
                 "Could not archive the prior processed Federal Wilderness RDS.")
    pt_fw_assert(identical(pt_fw_sha256(final_path), pt_fw_sha256(archive_path)),
                 "Archived processed RDS hash differs from the prior final RDS.")
  }
  pt_fw_assert(file.copy(candidate_path, final_path, overwrite = TRUE, copy.mode = TRUE),
               "Could not promote the reviewed candidate.")
  pt_fw_assert(identical(pt_fw_sha256(candidate_path), pt_fw_sha256(final_path)),
               "Promoted processed RDS hash differs from the reviewed candidate.")
  invisible(list(final_path = final_path, archive_path = archive_path))
}
