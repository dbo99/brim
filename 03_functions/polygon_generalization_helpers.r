# ==== polygon_generalization_helpers.r ======================================
## Apply the reviewed polygon-geometry portfolio without rerunning research
## algorithms. Production cache writers call this helper immediately before
## saving and fail closed when a required artifact or lineage contract differs.

PT_POLYGON_GENERALIZATION_REGISTRY_RELATIVE <- file.path(
  "00_config", "polygon_generalization_portfolio_v3.csv"
)
PT_POLYGON_GENERALIZATION_CROSSWALK_RELATIVE <- file.path(
  "00_config", "polygon_generalization_source_row_crosswalk_v3.csv"
)
PT_POLYGON_GENERALIZATION_BUNDLE_RELATIVE <- file.path(
  "04_processed_data", "rds", "reviewed_polygon_geometry", "portfolio_v3"
)
PT_POLYGON_GENERALIZATION_HASH_CACHE <- new.env(parent = emptyenv())
PT_POLYGON_GENERALIZATION_OBJECT_CACHE <- new.env(parent = emptyenv())

pt_polygon_generalization_root <- function() {
  if (exists("PT_PROJECT_ROOT", inherits = TRUE)) {
    return(get("PT_PROJECT_ROOT", inherits = TRUE))
  }
  normalizePath(".", winslash = "/", mustWork = TRUE)
}

pt_polygon_generalization_read_registry <- function(
    path = file.path(
      pt_polygon_generalization_root(),
      PT_POLYGON_GENERALIZATION_REGISTRY_RELATIVE
    )) {
  if (!file.exists(path)) stop("Missing polygon portfolio registry: ", path)
  registry <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = ""
  )
  registry$public_disclosure[is.na(registry$public_disclosure)] <- ""
  registry$candidate_bundle_key[is.na(registry$candidate_bundle_key)] <- ""
  registry$candidate_sha256[is.na(registry$candidate_sha256)] <- ""
  registry$parent_component[is.na(registry$parent_component)] <- ""
  required <- c(
    "portfolio_version", "layer_id", "brim_layer_name", "action",
    "parent_artifact_key",
    "source_parent_relative_path", "parent_component", "parent_sha256",
    "candidate_bundle_key", "candidate_sha256", "expected_geometry_rows",
    "expected_semantic_features", "expected_vertices", "cache_relative_path",
    "cache_child", "match_strategy", "match_fields", "fingerprint_fields",
    "disclosure_required", "public_disclosure", "disclosure_ui_owner"
  )
  missing <- setdiff(required, names(registry))
  if (length(missing)) {
    stop("Polygon portfolio registry is missing column(s): ", paste(missing, collapse = ", "))
  }
  if (nrow(registry) != 29L || anyDuplicated(registry$layer_id)) {
    stop("Polygon portfolio registry must contain exactly 29 unique layer rows.")
  }
  expected_actions <- c(replace_geometry = 24L, retain_current = 5L)
  action_counts <- table(factor(registry$action, levels = names(expected_actions)))
  if (!identical(as.integer(action_counts), unname(expected_actions))) {
    stop("Polygon portfolio registry must contain 24 replacements and 5 retained layers.")
  }
  required_note <- "Check authoritative source for boundary-sensitive use."
  disclosed <- tolower(registry$disclosure_required) == "yes"
  if (any(disclosed & !endsWith(registry$public_disclosure, required_note))) {
    stop("Every public polygon disclosure must end with the canonical warning.")
  }
  if (any(!disclosed & nzchar(registry$public_disclosure))) {
    stop("A non-disclosed polygon row contains public disclosure text.")
  }
  registry
}

pt_polygon_generalization_registry_row <- function(layer_id, registry = NULL) {
  if (is.null(registry)) registry <- pt_polygon_generalization_read_registry()
  row <- registry[registry$layer_id == as.character(layer_id), , drop = FALSE]
  if (nrow(row) != 1L) stop("Unknown or duplicated polygon portfolio layer: ", layer_id)
  row
}

pt_polygon_generalization_read_crosswalk <- function(
    path = file.path(
      pt_polygon_generalization_root(),
      PT_POLYGON_GENERALIZATION_CROSSWALK_RELATIVE
    )) {
  if (!file.exists(path)) stop("Missing polygon portfolio crosswalk: ", path)
  crosswalk <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = ""
  )
  required <- c(
    "portfolio_version", "layer_id", "study_id", "semantic_feature_key",
    "geometry_key", "match_strategy", "match_key",
    "retained_business_attributes", "source_row_fingerprint",
    "baseline_display_fingerprint", "parent_artifact_key", "parent_sha256"
  )
  missing <- setdiff(required, names(crosswalk))
  if (length(missing)) {
    stop("Polygon portfolio crosswalk is missing column(s): ", paste(missing, collapse = ", "))
  }
  if (anyDuplicated(paste(crosswalk$layer_id, crosswalk$study_id, sep = "\r"))) {
    stop("Polygon portfolio crosswalk layer/study IDs are not unique.")
  }
  if (anyDuplicated(paste(crosswalk$layer_id, crosswalk$geometry_key, sep = "\r"))) {
    stop("Polygon portfolio crosswalk geometry keys are not unique.")
  }
  crosswalk
}

pt_polygon_generalization_sha256_file <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Polygon portfolio validation requires the digest package.")
  }
  normalized <- normalizePath(path, winslash = "/", mustWork = TRUE)
  info <- file.info(normalized)
  cache_key <- paste(normalized, info$size, as.numeric(info$mtime), sep = "\r")
  if (!exists(cache_key, envir = PT_POLYGON_GENERALIZATION_HASH_CACHE, inherits = FALSE)) {
    assign(
      cache_key,
      digest::digest(file = normalized, algo = "sha256", serialize = FALSE),
      envir = PT_POLYGON_GENERALIZATION_HASH_CACHE
    )
  }
  get(cache_key, envir = PT_POLYGON_GENERALIZATION_HASH_CACHE, inherits = FALSE)
}

pt_polygon_generalization_read_rds <- function(path) {
  normalized <- normalizePath(path, winslash = "/", mustWork = TRUE)
  info <- file.info(normalized)
  cache_key <- paste(normalized, info$size, as.numeric(info$mtime), sep = "\r")
  if (!exists(
    cache_key,
    envir = PT_POLYGON_GENERALIZATION_OBJECT_CACHE,
    inherits = FALSE
  )) {
    assign(
      cache_key,
      readRDS(normalized),
      envir = PT_POLYGON_GENERALIZATION_OBJECT_CACHE
    )
  }
  get(cache_key, envir = PT_POLYGON_GENERALIZATION_OBJECT_CACHE, inherits = FALSE)
}

pt_polygon_generalization_normalize_value <- function(value) {
  value <- as.character(value)
  value[is.na(value)] <- "<NA>"
  enc2utf8(trimws(gsub("[[:space:]]+", " ", value)))
}

pt_polygon_generalization_choose_field <- function(x, specification) {
  alternatives <- strsplit(as.character(specification), "/", fixed = TRUE)[[1]]
  found <- alternatives[alternatives %in% names(x)]
  if (!length(found)) {
    stop("Prepared polygon layer is missing key field alternative(s): ", specification)
  }
  found[[1]]
}

pt_polygon_generalization_business_tuple <- function(x, specification) {
  specification <- as.character(specification)
  if (is.na(specification) || !nzchar(specification)) return(rep("", nrow(x)))
  parts <- strsplit(specification, "|", fixed = TRUE)[[1]]
  values <- lapply(parts, function(part) {
    field <- pt_polygon_generalization_choose_field(x, part)
    pt_polygon_generalization_normalize_value(x[[field]])
  })
  do.call(paste, c(values, sep = "\u001f"))
}

pt_polygon_generalization_canonical_wkb <- function(x) {
  if (!inherits(x, "sf")) stop("Polygon portfolio input must be sf.")
  geometry <- sf::st_zm(sf::st_geometry(x), drop = TRUE, what = "ZM")
  if (is.na(sf::st_crs(geometry))) stop("Polygon portfolio input is missing CRS.")
  if (sf::st_crs(geometry) != sf::st_crs(4326)) {
    geometry <- sf::st_transform(geometry, 4326)
  }
  sf::st_as_binary(geometry, EWKB = TRUE)
}

pt_polygon_generalization_source_fingerprint <- function(
    layer_id, parent_sha256, business_tuple, x) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Polygon portfolio fingerprinting requires the digest package.")
  }
  wkb <- pt_polygon_generalization_canonical_wkb(x)
  vapply(seq_along(wkb), function(index) {
    prefix <- charToRaw(enc2utf8(paste(
      layer_id,
      parent_sha256,
      business_tuple[[index]],
      sep = "\u001f"
    )))
    digest::digest(c(prefix, wkb[[index]]), algo = "sha256", serialize = FALSE)
  }, character(1))
}

pt_polygon_generalization_vertex_count <- function(x) {
  sum(vapply(sf::st_geometry(x), function(geometry) {
    nrow(sf::st_coordinates(geometry))
  }, integer(1)))
}

pt_polygon_generalization_match_keys <- function(x, registry_row) {
  strategy <- as.character(registry_row$match_strategy)
  if (identical(strategy, "single_feature")) return(rep("single", nrow(x)))
  if (identical(strategy, "business_key")) {
    return(pt_polygon_generalization_business_tuple(x, registry_row$match_fields))
  }
  if (identical(strategy, "source_fingerprint")) {
    tuple <- pt_polygon_generalization_business_tuple(
      x,
      registry_row$fingerprint_fields
    )
    return(pt_polygon_generalization_source_fingerprint(
      registry_row$layer_id,
      registry_row$parent_sha256,
      tuple,
      x
    ))
  }
  stop("Unsupported polygon portfolio match strategy: ", strategy)
}

pt_polygon_generalization_parent <- function(
    layer_id,
    project_root = pt_polygon_generalization_root(),
    registry = NULL) {
  row <- pt_polygon_generalization_registry_row(layer_id, registry)
  path <- file.path(project_root, row$source_parent_relative_path)
  if (!file.exists(path) && startsWith(row$source_parent_relative_path, "research_provenance/")) {
    path <- file.path(
      project_root,
      PT_POLYGON_GENERALIZATION_BUNDLE_RELATIVE,
      row$source_parent_relative_path
    )
  }
  if (!file.exists(path)) stop("Missing pinned polygon parent for ", layer_id, ": ", path)
  actual_hash <- pt_polygon_generalization_sha256_file(path)
  if (!identical(actual_hash, tolower(row$parent_sha256))) {
    stop(
      "Pinned polygon parent hash mismatch for ", layer_id,
      "; expected ", row$parent_sha256, ", received ", actual_hash, "."
    )
  }
  ## A single pinned parent can own several portfolio rows (all HUC levels and
  ## both NPS context classes). Cache the immutable RDS object after its hash is
  ## validated so a focused refresh does not repeatedly deserialize it.
  parent <- pt_polygon_generalization_read_rds(path)
  component <- as.character(row$parent_component)
  if (!is.na(component) && nzchar(component)) {
    if (grepl("^nps_", layer_id)) {
      if (!is.list(parent) || !all(c("boundaries", "land_interest") %in% names(parent))) {
        stop("NPS polygon parent does not contain boundaries/land_interest.")
      }
      parent <- rbind(
        parent$boundaries[parent$boundaries$unit_type_key == component, ],
        parent$land_interest[parent$land_interest$unit_type_key == component, ]
      )
    } else {
      if (!is.list(parent) || !component %in% names(parent)) {
        stop("Pinned polygon parent is missing component `", component, "`.")
      }
      parent <- parent[[component]]
    }
  }
  if (!inherits(parent, "sf")) stop("Pinned polygon parent is not sf for ", layer_id, ".")
  parent
}

pt_polygon_generalization_validate_candidate <- function(candidate, row) {
  if (!inherits(candidate, "sf")) stop("Reviewed polygon candidate is not sf.")
  expected_rows <- as.integer(row$expected_geometry_rows)
  if (nrow(candidate) != expected_rows) {
    stop("Reviewed polygon candidate row count differs for ", row$layer_id, ".")
  }
  if (!identical(names(candidate), c("study_id", attr(candidate, "sf_column")))) {
    stop("Reviewed polygon candidate must contain only study_id and geometry.")
  }
  expected_study_ids <- sprintf("feature_%05d", seq_len(expected_rows))
  if (!identical(as.character(candidate$study_id), expected_study_ids)) {
    stop("Reviewed polygon candidate study IDs differ for ", row$layer_id, ".")
  }
  if (is.na(sf::st_crs(candidate)) || sf::st_crs(candidate) != sf::st_crs(4326)) {
    stop("Reviewed polygon candidate must use EPSG:4326 for ", row$layer_id, ".")
  }
  types <- as.character(sf::st_geometry_type(candidate))
  if (!all(types %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Reviewed polygon candidate contains a non-polygon geometry type.")
  }
  if (any(sf::st_is_empty(candidate))) stop("Reviewed polygon candidate contains empty geometry.")
  valid <- sf::st_is_valid(candidate)
  if (any(is.na(valid) | !valid)) stop("Reviewed polygon candidate contains invalid geometry.")
  vertices <- pt_polygon_generalization_vertex_count(candidate)
  if (!identical(as.integer(vertices), as.integer(row$expected_vertices))) {
    stop(
      "Reviewed polygon candidate vertex count differs for ", row$layer_id,
      "; expected ", row$expected_vertices, ", received ", vertices, "."
    )
  }
  invisible(TRUE)
}

pt_apply_reviewed_polygon_geometry <- function(
    layer_id,
    prepared_layer,
    require_reviewed = TRUE,
    project_root = pt_polygon_generalization_root(),
    registry = NULL,
    crosswalk = NULL) {
  row <- pt_polygon_generalization_registry_row(layer_id, registry)
  ## `require_reviewed` is retained for caller compatibility, but it can never
  ## waive a registered replacement. It has no effect on retain-current rows.
  if (identical(row$action, "retain_current")) return(prepared_layer)
  if (!identical(row$action, "replace_geometry")) {
    stop("Unsupported polygon portfolio action for ", layer_id, ": ", row$action)
  }
  if (!inherits(prepared_layer, "sf")) stop(layer_id, " prepared display layer is not sf.")

  ## Validate the exact high-fidelity parent independently from the prepared
  ## display object. This prevents applying a reviewed child to a republished
  ## or otherwise changed source lineage that merely has the same row count.
  invisible(pt_polygon_generalization_parent(layer_id, project_root, registry))

  bundle_path <- file.path(
    project_root,
    PT_POLYGON_GENERALIZATION_BUNDLE_RELATIVE,
    row$candidate_bundle_key
  )
  if (!file.exists(bundle_path)) {
    stop("Missing required reviewed polygon artifact for ", layer_id, ": ", bundle_path)
  }
  actual_hash <- pt_polygon_generalization_sha256_file(bundle_path)
  if (!identical(actual_hash, tolower(row$candidate_sha256))) {
    stop(
      "Reviewed polygon artifact hash mismatch for ", layer_id,
      "; expected ", row$candidate_sha256, ", received ", actual_hash, "."
    )
  }
  candidate <- readRDS(bundle_path)
  pt_polygon_generalization_validate_candidate(candidate, row)

  if (is.null(crosswalk)) crosswalk <- pt_polygon_generalization_read_crosswalk()
  layer_crosswalk <- crosswalk[crosswalk$layer_id == layer_id, , drop = FALSE]
  expected_rows <- as.integer(row$expected_geometry_rows)
  if (nrow(layer_crosswalk) != expected_rows) {
    stop("Polygon portfolio crosswalk row count differs for ", layer_id, ".")
  }
  if (!identical(as.character(layer_crosswalk$study_id), as.character(candidate$study_id))) {
    stop("Polygon portfolio crosswalk/candidate study IDs differ for ", layer_id, ".")
  }
  if (!all(layer_crosswalk$parent_artifact_key == row$parent_artifact_key) ||
      !all(tolower(layer_crosswalk$parent_sha256) == tolower(row$parent_sha256))) {
    stop("Polygon portfolio crosswalk parent lineage differs for ", layer_id, ".")
  }
  semantic_count <- length(unique(layer_crosswalk$semantic_feature_key))
  if (!identical(as.integer(semantic_count), as.integer(row$expected_semantic_features))) {
    stop("Polygon portfolio semantic-feature count differs for ", layer_id, ".")
  }

  if (identical(as.character(row$match_strategy), "source_fingerprint")) {
    tuple <- pt_polygon_generalization_business_tuple(
      prepared_layer,
      row$fingerprint_fields
    )
    prepared_fingerprints <- pt_polygon_generalization_source_fingerprint(
      row$layer_id,
      row$parent_sha256,
      tuple,
      prepared_layer
    )
    source_index <- match(prepared_fingerprints, layer_crosswalk$source_row_fingerprint)
    baseline_index <- match(
      prepared_fingerprints,
      layer_crosswalk$baseline_display_fingerprint
    )
    resolved_index <- ifelse(!is.na(source_index), source_index, baseline_index)
    if (anyNA(resolved_index) || anyDuplicated(resolved_index)) {
      stop(
        "Prepared polygon fingerprints do not map one-to-one to the reviewed ",
        "source/current-display crosswalk for ", layer_id, "."
      )
    }
    match_keys <- layer_crosswalk$match_key[resolved_index]
  } else {
    match_keys <- pt_polygon_generalization_match_keys(prepared_layer, row)
  }
  if (length(match_keys) != nrow(prepared_layer) || anyNA(match_keys) ||
      any(!nzchar(match_keys)) || anyDuplicated(match_keys)) {
    stop("Prepared polygon match keys are incomplete or duplicated for ", layer_id, ".")
  }
  if (!setequal(match_keys, layer_crosswalk$match_key)) {
    stop("Prepared polygon match-key set differs from the reviewed crosswalk for ", layer_id, ".")
  }
  candidate_order <- match(match_keys, layer_crosswalk$match_key)
  if (anyNA(candidate_order) || anyDuplicated(candidate_order)) {
    stop("Prepared polygon crosswalk alignment failed for ", layer_id, ".")
  }

  original_attributes <- sf::st_drop_geometry(prepared_layer)
  original_crs <- sf::st_crs(prepared_layer)
  replacement_geometry <- sf::st_geometry(candidate)[candidate_order]
  if (original_crs != sf::st_crs(candidate)) {
    replacement_geometry <- sf::st_transform(replacement_geometry, original_crs)
  }
  sf::st_geometry(prepared_layer) <- replacement_geometry
  if (!identical(sf::st_drop_geometry(prepared_layer), original_attributes)) {
    stop("Polygon geometry replacement altered retained attributes for ", layer_id, ".")
  }
  if (nrow(prepared_layer) != expected_rows || any(sf::st_is_empty(prepared_layer))) {
    stop("Polygon geometry replacement row/empty contract failed for ", layer_id, ".")
  }
  valid <- sf::st_is_valid(prepared_layer)
  if (any(is.na(valid) | !valid)) stop("Polygon geometry replacement is invalid for ", layer_id, ".")
  attr(prepared_layer, "pt_polygon_generalization_portfolio") <- list(
    version = as.character(row$portfolio_version),
    layer_id = layer_id,
    artifact_sha256 = actual_hash,
    parent_sha256 = as.character(row$parent_sha256)
  )
  prepared_layer
}

pt_apply_reviewed_polygon_geometry_to_nps_context <- function(
    nps_context,
    require_reviewed = TRUE,
    project_root = pt_polygon_generalization_root()) {
  if (!is.list(nps_context) || !all(c("boundaries", "land_interest") %in% names(nps_context))) {
    stop("NPS context portfolio overlay requires boundaries and land_interest.")
  }
  metadata <- attr(nps_context, "pt_nps_context_candidate_metadata")
  combined <- rbind(nps_context$boundaries, nps_context$land_interest)
  parks <- combined[combined$unit_type_key == "national_park", , drop = FALSE]
  preserve <- combined[combined$unit_type_key == "national_preserve", , drop = FALSE]
  parks <- pt_apply_reviewed_polygon_geometry(
    "nps_national_parks_context", parks, require_reviewed, project_root
  )
  preserve <- pt_apply_reviewed_polygon_geometry(
    "nps_national_preserve_context", preserve, require_reviewed, project_root
  )
  combined <- rbind(parks, preserve)
  boundaries <- combined[combined$context_geometry_role == "legislative_boundary", , drop = FALSE]
  land_interest <- combined[combined$context_geometry_role == "nps_land_or_interest", , drop = FALSE]
  boundaries <- boundaries[match(nps_context$boundaries$context_geometry_key, boundaries$context_geometry_key), ]
  land_interest <- land_interest[match(nps_context$land_interest$context_geometry_key, land_interest$context_geometry_key), ]
  if (anyNA(boundaries$context_geometry_key) || anyNA(land_interest$context_geometry_key)) {
    stop("NPS context portfolio overlay could not restore the prepared row order.")
  }
  registry <- pt_polygon_generalization_read_registry()
  park_row <- pt_polygon_generalization_registry_row(
    "nps_national_parks_context", registry
  )
  preserve_row <- pt_polygon_generalization_registry_row(
    "nps_national_preserve_context", registry
  )
  metadata$polygon_generalization_portfolio_version <-
    as.character(park_row$portfolio_version)
  metadata$polygon_generalization_selected_tolerance_m <- 25
  metadata$polygon_generalization_park_artifact_sha256 <-
    as.character(park_row$candidate_sha256)
  metadata$polygon_generalization_preserve_artifact_sha256 <-
    as.character(preserve_row$candidate_sha256)
  out <- list(boundaries = boundaries, land_interest = land_interest)
  attr(out, "pt_nps_context_candidate_metadata") <- metadata
  out
}

pt_polygon_generalization_public_note <- function(layer_id, required = TRUE) {
  row <- pt_polygon_generalization_registry_row(layer_id)
  note <- as.character(row$public_disclosure)
  if (isTRUE(required) && (!identical(tolower(row$disclosure_required), "yes") || !nzchar(note))) {
    stop("No approved public polygon disclosure for ", layer_id, ".")
  }
  note
}

pt_apply_polygon_generalization_disclosures <- function(registry) {
  if (!is.data.frame(registry) || !"layer_id" %in% names(registry)) {
    stop("Local Reference disclosure overlay requires a layer registry.")
  }
  registry$generalization_disclosure <- vapply(registry$layer_id, function(layer_id) {
    portfolio_id <- switch(
      layer_id,
      national_monuments = "national_monuments",
      ca_desert_ncl = "ca_desert_ncl",
      wilderness_study_areas = "wilderness_study_areas",
      federal_wilderness = "federal_wilderness",
      drecp = "drecp",
      acec = "acec",
      grazing_allotments = "grazing_allotments",
      counties = "counties",
      rwqcb_regions = "rwqcb_regions",
      water_districts = "water_districts",
      ""
    )
    if (!nzchar(portfolio_id)) "" else pt_polygon_generalization_public_note(portfolio_id)
  }, character(1))
  registry
}
