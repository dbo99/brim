#!/usr/bin/env Rscript

# Build a reviewable California Desert NCL candidate from one COMPLETE raw
# snapshot. All outputs go to a new external directory; this script never
# promotes processed data or mutates shared caches.

CDNCL_CANDIDATE_VERSION <- "BRIM_CA_DESERT_NCL_CANDIDATE_20260810_01"
CDNCL_EXPECTED_IDS <- sprintf("NLCS%06d", 2009:2019)
CDNCL_SIMPLIFY_TOLERANCES_M <- c(0, 1, 2, 5, 10, 20, 50)
CDNCL_SELECTED_SIMPLIFY_TOLERANCE_M <- 2
CDNCL_ACRES_PER_M2 <- 1 / 4046.8564224
CDNCL_MILES_PER_M <- 1 / 1609.344

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || all(is.na(x))) y else x
}

cdncl_build_stop <- function(...) stop(paste0(...), call. = FALSE)

cdncl_build_assert <- function(value, message) {
  if (!isTRUE(value)) cdncl_build_stop(message)
}

cdncl_build_require <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    cdncl_build_stop("Missing required R package(s): ", paste(missing, collapse = ", "))
  }
}

cdncl_build_clean <- function(x) {
  value <- trimws(as.character(x))
  value[is.na(value) | value %in% c("NA", "N/A", "<NA>", "NULL")] <- ""
  value
}

cdncl_build_key <- function(x) {
  value <- tolower(cdncl_build_clean(x))
  value <- gsub("[^a-z0-9]+", "_", value)
  gsub("^_+|_+$", "", value)
}

cdncl_build_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

cdncl_build_read_csv <- function(path) {
  if (!file.exists(path)) cdncl_build_stop("Missing table: ", path)
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
}

cdncl_build_read_json <- function(path, simplify = TRUE) {
  if (!file.exists(path)) cdncl_build_stop("Missing JSON: ", path)
  jsonlite::fromJSON(path, simplifyVector = simplify)
}

cdncl_build_write_json <- function(value, path) {
  jsonlite::write_json(
    value,
    path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null",
    digits = 17
  )
}

cdncl_build_part_count_geometry <- function(geometry, crs) {
  length(suppressWarnings(sf::st_cast(sf::st_sfc(geometry, crs = crs), "POLYGON")))
}

cdncl_build_part_count <- function(x) {
  crs <- sf::st_crs(x)
  vapply(sf::st_geometry(x), cdncl_build_part_count_geometry, integer(1), crs = crs)
}

cdncl_build_hole_count_geometry <- function(geometry) {
  type <- as.character(sf::st_geometry_type(geometry))
  if (identical(type, "POLYGON")) return(as.integer(max(0, length(geometry) - 1L)))
  if (identical(type, "MULTIPOLYGON")) {
    return(sum(vapply(geometry, function(polygon) {
      as.integer(max(0, length(polygon) - 1L))
    }, integer(1))))
  }
  0L
}

cdncl_build_hole_count <- function(x) {
  vapply(sf::st_geometry(x), cdncl_build_hole_count_geometry, integer(1))
}

cdncl_build_vertex_count <- function(x) {
  vapply(sf::st_geometry(x), function(geometry) {
    nrow(sf::st_coordinates(geometry))
  }, integer(1))
}

cdncl_build_geometry_hash <- function(x) {
  vapply(sf::st_as_binary(sf::st_geometry(x), EWKB = TRUE), function(value) {
    digest::digest(value, algo = "sha256", serialize = FALSE)
  }, character(1))
}

cdncl_build_geometry_stats <- function(x, prefix) {
  area_m2 <- as.numeric(sf::st_area(x))
  bbox <- lapply(sf::st_geometry(x), sf::st_bbox)
  data.frame(
    nlcs_id = x$NLCS_ID,
    geometry_stage = prefix,
    geometry_type = as.character(sf::st_geometry_type(x)),
    polygon_parts = cdncl_build_part_count(x),
    holes = cdncl_build_hole_count(x),
    vertices = cdncl_build_vertex_count(x),
    area_m2 = area_m2,
    area_acres = area_m2 * CDNCL_ACRES_PER_M2,
    source_shape_area_m2 = suppressWarnings(as.numeric(x$Shape__Area)),
    source_vs_calculated_area_m2 =
      area_m2 - suppressWarnings(as.numeric(x$Shape__Area)),
    valid = sf::st_is_valid(x),
    empty = sf::st_is_empty(x),
    geometry_sha256 = cdncl_build_geometry_hash(x),
    extent_xmin = vapply(bbox, function(value) unname(value[["xmin"]]), numeric(1)),
    extent_ymin = vapply(bbox, function(value) unname(value[["ymin"]]), numeric(1)),
    extent_xmax = vapply(bbox, function(value) unname(value[["xmax"]]), numeric(1)),
    extent_ymax = vapply(bbox, function(value) unname(value[["ymax"]]), numeric(1)),
    stringsAsFactors = FALSE
  )
}

cdncl_build_pairwise_overlap <- function(x) {
  rows <- list()
  cursor <- 0L
  for (left in seq_len(nrow(x) - 1L)) {
    for (right in seq.int(left + 1L, nrow(x))) {
      intersection <- suppressWarnings(sf::st_intersection(
        sf::st_geometry(x[left, ]),
        sf::st_geometry(x[right, ])
      ))
      area_m2 <- if (length(intersection) && !all(sf::st_is_empty(intersection))) {
        sum(as.numeric(sf::st_area(intersection)))
      } else {
        0
      }
      if (!is.finite(area_m2) || area_m2 <= 1) next
      cursor <- cursor + 1L
      rows[[cursor]] <- data.frame(
        left_nlcs_id = x$NLCS_ID[[left]],
        right_nlcs_id = x$NLCS_ID[[right]],
        overlap_area_m2 = area_m2,
        overlap_area_acres = area_m2 * CDNCL_ACRES_PER_M2,
        left_percent = 100 * area_m2 / as.numeric(sf::st_area(x[left, ])),
        right_percent = 100 * area_m2 / as.numeric(sf::st_area(x[right, ])),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(data.frame(
      left_nlcs_id = character(), right_nlcs_id = character(),
      overlap_area_m2 = numeric(), overlap_area_acres = numeric(),
      left_percent = numeric(), right_percent = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

cdncl_build_containment <- function(x) {
  contains <- sf::st_within(x, x, sparse = TRUE)
  rows <- list()
  cursor <- 0L
  for (child in seq_along(contains)) {
    parents <- setdiff(contains[[child]], child)
    for (parent in parents) {
      cursor <- cursor + 1L
      rows[[cursor]] <- data.frame(
        contained_nlcs_id = x$NLCS_ID[[child]],
        containing_nlcs_id = x$NLCS_ID[[parent]],
        relationship = "within",
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(data.frame(
      contained_nlcs_id = character(), containing_nlcs_id = character(),
      relationship = character(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

cdncl_build_simplification_qa <- function(raw) {
  raw_area <- as.numeric(sf::st_area(raw))
  raw_parts <- cdncl_build_part_count(raw)
  raw_holes <- cdncl_build_hole_count(raw)
  raw_vertices <- cdncl_build_vertex_count(raw)
  rows <- lapply(CDNCL_SIMPLIFY_TOLERANCES_M, function(tolerance) {
    candidate <- if (tolerance == 0) raw else sf::st_simplify(
      raw,
      dTolerance = tolerance,
      preserveTopology = TRUE
    )
    candidate <- sf::st_cast(candidate, "MULTIPOLYGON", warn = FALSE)
    area <- as.numeric(sf::st_area(candidate))
    parts <- cdncl_build_part_count(candidate)
    holes <- cdncl_build_hole_count(candidate)
    vertices <- cdncl_build_vertex_count(candidate)
    data.frame(
      tolerance_m = tolerance,
      total_parts = sum(parts),
      total_holes = sum(holes),
      total_vertices = sum(vertices),
      vertex_reduction_percent = 100 * (1 - sum(vertices) / sum(raw_vertices)),
      total_area_change_acres = sum(area - raw_area) * CDNCL_ACRES_PER_M2,
      maximum_unit_absolute_area_change_acres =
        max(abs(area - raw_area)) * CDNCL_ACRES_PER_M2,
      maximum_unit_absolute_area_change_percent =
        max(abs(100 * (area - raw_area) / raw_area)),
      invalid_geometries = sum(!sf::st_is_valid(candidate)),
      empty_geometries = sum(sf::st_is_empty(candidate)),
      exact_part_retention = identical(parts, raw_parts),
      exact_hole_retention = identical(holes, raw_holes),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

cdncl_build_existing_comparison <- function(raw, existing_path) {
  empty <- data.frame(
    nlcs_id = character(), current_name = character(),
    current_area_acres = numeric(), fresh_area_acres = numeric(),
    area_change_acres = numeric(), symmetric_difference_acres = numeric(),
    current_geometry_sha256 = character(), fresh_geometry_sha256 = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(existing_path) || !nzchar(existing_path) || !file.exists(existing_path)) {
    return(empty)
  }
  existing <- readRDS(existing_path)
  cdncl_build_assert(inherits(existing, "sf"), "Existing comparison object is not sf.")
  name_field <- intersect(c("NLCS_NAME", "pt_cdncl_raw_name"), names(existing))[[1]]
  index <- match(cdncl_build_clean(raw$NLCS_NAME), cdncl_build_clean(existing[[name_field]]))
  cdncl_build_assert(!anyNA(index), "Fresh names do not fully match the existing BRIM layer.")
  existing <- existing[index, , drop = FALSE]
  existing <- sf::st_transform(sf::st_make_valid(existing), 3310)
  fresh_area <- as.numeric(sf::st_area(raw))
  existing_area <- as.numeric(sf::st_area(existing))
  symmetric <- vapply(seq_len(nrow(raw)), function(i) {
    geometry <- suppressWarnings(sf::st_sym_difference(
      sf::st_geometry(raw[i, ]), sf::st_geometry(existing[i, ])
    ))
    if (!length(geometry) || all(sf::st_is_empty(geometry))) return(0)
    sum(as.numeric(sf::st_area(geometry))) * CDNCL_ACRES_PER_M2
  }, numeric(1))
  data.frame(
    nlcs_id = raw$NLCS_ID,
    current_name = cdncl_build_clean(existing[[name_field]]),
    current_area_acres = existing_area * CDNCL_ACRES_PER_M2,
    fresh_area_acres = fresh_area * CDNCL_ACRES_PER_M2,
    area_change_acres = (fresh_area - existing_area) * CDNCL_ACRES_PER_M2,
    symmetric_difference_acres = symmetric,
    current_geometry_sha256 = cdncl_build_geometry_hash(existing),
    fresh_geometry_sha256 = cdncl_build_geometry_hash(raw),
    stringsAsFactors = FALSE
  )
}

cdncl_build_office_context <- function(raw, office_path) {
  cdncl_build_assert(file.exists(office_path), "Missing accepted field-office RDS.")
  offices <- readRDS(office_path)
  cdncl_build_assert(
    inherits(offices, "sf") && all(c("ADM_UNIT_C", "ADMU_NAME") %in% names(offices)),
    "Field-office RDS lacks its accepted identity fields."
  )
  offices <- sf::st_transform(sf::st_make_valid(offices), 3310)
  raw_area <- stats::setNames(as.numeric(sf::st_area(raw)), raw$NLCS_ID)
  rows <- list()
  cursor <- 0L
  hits <- sf::st_intersects(raw, offices)
  for (i in seq_len(nrow(raw))) {
    for (j in hits[[i]]) {
      intersection <- suppressWarnings(sf::st_intersection(
        sf::st_geometry(raw[i, ]), sf::st_geometry(offices[j, ])
      ))
      area_m2 <- if (length(intersection) && !all(sf::st_is_empty(intersection))) {
        sum(as.numeric(sf::st_area(intersection)))
      } else 0
      if (!is.finite(area_m2) || area_m2 <= 1) next
      cursor <- cursor + 1L
      rows[[cursor]] <- data.frame(
        nlcs_id = raw$NLCS_ID[[i]],
        office_key = cdncl_build_key(offices$ADM_UNIT_C[[j]]),
        office_code = cdncl_build_clean(offices$ADM_UNIT_C[[j]]),
        office_name = cdncl_build_clean(offices$ADMU_NAME[[j]]),
        office_globalid = if ("GlobalID" %in% names(offices)) {
          cdncl_build_clean(offices$GlobalID[[j]])
        } else "",
        intersection_area_m2 = area_m2,
        intersection_area_acres = area_m2 * CDNCL_ACRES_PER_M2,
        percent_of_unit_area = 100 * area_m2 / raw_area[[raw$NLCS_ID[[i]]]],
        relationship_method = "positive-area intersection in EPSG:3310",
        context_only_not_management_assignment = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }
  context <- if (length(rows)) do.call(rbind, rows) else data.frame()
  cdncl_build_assert(nrow(context) > 0L, "No positive-area field-office context was derived.")
  context <- context[order(
    match(context$nlcs_id, CDNCL_EXPECTED_IDS),
    -context$percent_of_unit_area,
    context$office_name
  ), , drop = FALSE]
  rownames(context) <- NULL
  context$display_context <- ave(
    context$percent_of_unit_area,
    context$nlcs_id,
    FUN = function(value) {
      selected <- value >= 1
      if (!any(selected)) selected[which.max(value)] <- TRUE
      selected
    }
  ) == 1
  office_lookup <- unique(context[c(
    "office_key", "office_code", "office_name", "office_globalid"
  )])
  office_lookup <- office_lookup[order(office_lookup$office_name), , drop = FALSE]
  list(context = context, offices = office_lookup)
}

cdncl_build_first_field <- function(x, candidates, fallback = "") {
  field <- intersect(candidates, names(x))
  if (!length(field)) return(rep(fallback, nrow(x)))
  cdncl_build_clean(x[[field[[1]]]])
}

cdncl_build_related_spec <- function(root) {
  list(
    list(key = "national_monuments", label = "National Monuments", mode = "area",
      file = "reference_monuments_wgs84.rds",
      ids = c("monument_id", "NLCS_ID", "GlobalID"),
      names = c("pt_nm_canonical_name", "canonical_name", "NLCS_NAME")),
    list(key = "federal_wilderness", label = "Federal Wilderness", mode = "area",
      file = "reference_fedwilderness_wgs84.rds",
      ids = c("wilderness_id", "FAU_ID", "NLCS_ID", "GlobalID"),
      names = c("pt_fw_official_name", "NLCS_NAME")),
    list(key = "wilderness_study_areas", label = "Wilderness Study Areas", mode = "area",
      file = "reference_wildernessstudyarea_wgs84.rds",
      ids = c("NLCS_ID", "WSACODE_ca", "GlobalID"),
      names = c("pt_wsa_name", "NLCS_NAME")),
    list(key = "acec", label = "ACECs", mode = "area",
      file = "reference_acec_wgs84.rds",
      ids = c("acec_id", "NLCS_ID", "GlobalID"),
      names = c("pt_acec_official_name", "ACEC_NAME", "NLCS_NAME")),
    list(key = "national_trails", label = "National Scenic/Historic Trails", mode = "length",
      file = "reference_trails_wgs84.rds",
      ids = c("NLCS_ID", "GlobalID"),
      names = c("pt_trails_official_name", "NLCS_NAME")),
    list(key = "wsr_blm_lines", label = "Wild & Scenic Rivers — BLM lines", mode = "length",
      file = "reference_wsr_blm_lines_wgs84.rds",
      ids = c("SMA_ID", "NLCS_ID"), names = c("NLCS_NAME", "pt_reference_label_text")),
    list(key = "wsr_segments", label = "Wild & Scenic Rivers — segments", mode = "length",
      file = "reference_wsr_segments_wgs84.rds",
      ids = c("SEGMENT_ID", "RIVER_ID"), names = c("WSR_RIVER1", "GNIS_NAME")),
    list(key = "wsr_corridor_blm", label = "Wild & Scenic River corridors — BLM", mode = "area",
      file = "reference_wsr_corridor_blm_wgs84.rds",
      ids = c("SMA_ID", "NLCS_ID"), names = c("NLCS_NAME", "pt_reference_label_text")),
    list(key = "wsr_corridor_lsrs_area", label = "Wild & Scenic River corridors — LSRS", mode = "area",
      file = "reference_wsr_corridor_lsrs_area_wgs84.rds",
      ids = c("AREAID", "WILDSCENIC"), names = c("RIVER", "DESIGNATED")),
    list(key = "wsr_corridor_lsrs_status", label = "Wild & Scenic River legal-status corridors", mode = "area",
      file = "reference_wsr_corridor_lsrs_status_wgs84.rds",
      ids = c("AREAID", "WILDSCENIC", "LOCALCASEI"), names = c("AREANAME", "CASENAME"))
  )
}

cdncl_build_related_context <- function(raw, related_root) {
  specs <- cdncl_build_related_spec(related_root)
  unit_area <- stats::setNames(as.numeric(sf::st_area(raw)), raw$NLCS_ID)
  rows <- list()
  cursor <- 0L
  for (spec in specs) {
    path <- file.path(related_root, spec$file)
    if (!file.exists(path)) cdncl_build_stop("Missing accepted relationship source: ", path)
    related <- readRDS(path)
    cdncl_build_assert(
      inherits(related, "sf"),
      paste0("Relationship source is not sf: ", path)
    )
    related <- sf::st_transform(sf::st_make_valid(related), 3310)
    related_id <- cdncl_build_first_field(related, spec$ids)
    related_name <- cdncl_build_first_field(related, spec$names)
    missing_id <- !nzchar(related_id)
    related_id[missing_id] <- paste0(spec$key, "-row-", which(missing_id))
    hits <- sf::st_intersects(raw, related)
    for (i in seq_len(nrow(raw))) {
      for (j in hits[[i]]) {
        intersection <- suppressWarnings(sf::st_intersection(
          sf::st_geometry(raw[i, ]), sf::st_geometry(related[j, ])
        ))
        if (!length(intersection) || all(sf::st_is_empty(intersection))) next
        metric <- if (identical(spec$mode, "length")) {
          sum(as.numeric(sf::st_length(intersection)))
        } else {
          sum(as.numeric(sf::st_area(intersection)))
        }
        if (!is.finite(metric) || metric <= 1) next
        cursor <- cursor + 1L
        area_acres <- if (identical(spec$mode, "area")) {
          metric * CDNCL_ACRES_PER_M2
        } else NA_real_
        overlap_percent <- if (identical(spec$mode, "area")) {
          100 * metric / unit_area[[raw$NLCS_ID[[i]]]]
        } else NA_real_
        length_miles <- if (identical(spec$mode, "length")) {
          metric * CDNCL_MILES_PER_M
        } else NA_real_
        rows[[cursor]] <- data.frame(
          nlcs_id = raw$NLCS_ID[[i]],
          related_layer_key = spec$key,
          related_layer_label = spec$label,
          related_feature_id = related_id[[j]],
          related_feature_name = related_name[[j]],
          relationship_type = if (identical(spec$mode, "area")) {
            "positive_area_overlap"
          } else "line_intersection_within_unit",
          overlap_area_m2 = if (identical(spec$mode, "area")) metric else NA_real_,
          overlap_area_acres = area_acres,
          percent_of_unit_area = overlap_percent,
          intersection_length_m = if (identical(spec$mode, "length")) metric else NA_real_,
          intersection_length_miles = length_miles,
          derivation_method = if (identical(spec$mode, "area")) {
            "positive-area intersection in EPSG:3310"
          } else "line intersection and length within unit in EPSG:3310",
          normal_popup_suitable = if (identical(spec$mode, "area")) {
            area_acres >= 1
          } else length_miles >= 0.1,
          identity_and_geometry_remain_in_related_layer = TRUE,
          management_inference_prohibited = TRUE,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) return(data.frame())
  relationships <- do.call(rbind, rows)
  relationships <- relationships[order(
    match(relationships$nlcs_id, CDNCL_EXPECTED_IDS),
    relationships$related_layer_label,
    relationships$related_feature_name,
    relationships$related_feature_id
  ), , drop = FALSE]
  rownames(relationships) <- NULL
  relationships
}

cdncl_build_parse_args <- function(args) {
  values <- list(
    snapshot = NULL,
    output_dir = NULL,
    existing_rds = "",
    field_office_rds = "",
    related_rds_root = "",
    config_dir = "00_config"
  )
  keys <- c(
    "--snapshot" = "snapshot", "--output-dir" = "output_dir",
    "--existing-rds" = "existing_rds", "--field-office-rds" = "field_office_rds",
    "--related-rds-root" = "related_rds_root", "--config-dir" = "config_dir"
  )
  i <- 1L
  while (i <= length(args)) {
    key <- args[[i]]
    if (!key %in% names(keys) || i == length(args)) {
      cdncl_build_stop("Unknown or incomplete argument: ", key)
    }
    values[[keys[[key]]]] <- args[[i + 1L]]
    i <- i + 2L
  }
  required <- c("snapshot", "output_dir", "field_office_rds", "related_rds_root")
  missing <- required[!vapply(values[required], function(x) {
    !is.null(x) && nzchar(trimws(x))
  }, logical(1))]
  if (length(missing)) {
    cdncl_build_stop("Missing required argument(s): ", paste(missing, collapse = ", "))
  }
  values
}

cdncl_build_main <- function() {
  cdncl_build_require(c("sf", "jsonlite", "digest"))
  arguments <- cdncl_build_parse_args(commandArgs(trailingOnly = TRUE))
  snapshot <- normalizePath(arguments$snapshot, mustWork = TRUE)
  cdncl_build_assert(
    file.exists(file.path(snapshot, "COMPLETE.json")) &&
      !file.exists(file.path(snapshot, "FAILED.json")),
    "Snapshot is not COMPLETE."
  )
  complete <- cdncl_build_read_json(file.path(snapshot, "COMPLETE.json"))
  cdncl_build_assert(
    identical(complete$status, "PASS") &&
      identical(as.integer(complete$record_count), 11L) &&
      identical(sort(as.character(complete$nlcs_ids)), CDNCL_EXPECTED_IDS),
    "Snapshot completion contract changed."
  )
  if (file.exists(arguments$output_dir)) {
    cdncl_build_stop("Refusing to overwrite candidate output directory: ", arguments$output_dir)
  }
  dir.create(arguments$output_dir, recursive = TRUE)

  config_prefix <- file.path(arguments$config_dir, "local_reference_desert_ncl_")
  reference <- cdncl_build_read_csv(paste0(config_prefix, "reference.csv"))
  source_identity <- cdncl_build_read_csv(paste0(config_prefix, "source_identity.csv"))
  aliases <- cdncl_build_read_csv(paste0(config_prefix, "aliases.csv"))
  policies <- cdncl_build_read_csv(paste0(config_prefix, "common_policy_language.csv"))
  documents <- cdncl_build_read_csv(paste0(config_prefix, "documents.csv"))
  unit_documents <- cdncl_build_read_csv(paste0(config_prefix, "unit_documents.csv"))
  sources <- cdncl_build_read_csv(paste0(config_prefix, "source_register.csv"))
  cdncl_build_assert(
    nrow(reference) == 11L && nrow(source_identity) == 11L &&
      !anyDuplicated(reference$nlcs_id) && !anyDuplicated(source_identity$nlcs_id) &&
      identical(sort(reference$nlcs_id), CDNCL_EXPECTED_IDS),
    "Canonical research reference must retain the exact 11 NLCS IDs."
  )
  cdncl_build_assert(
    nrow(aliases) == 24L && nrow(documents) == 13L &&
      nrow(unit_documents) == 112L && nrow(policies) == 9L && nrow(sources) == 19L,
    "Canonical package lookup counts differ from 24/13/112/9/19."
  )

  geojson_path <- file.path(snapshot, "raw", "004_features_epsg3310.json")
  raw <- suppressWarnings(sf::st_read(geojson_path, quiet = TRUE, stringsAsFactors = FALSE))
  cdncl_build_assert(inherits(raw, "sf") && nrow(raw) == 11L, "Fresh GeoJSON must contain 11 sf rows.")
  cdncl_build_assert(sf::st_crs(raw)$epsg == 3310L, "Fresh geometry must declare EPSG:3310.")
  raw$NLCS_ID <- cdncl_build_clean(raw$NLCS_ID)
  raw$NLCS_NAME <- cdncl_build_clean(raw$NLCS_NAME)
  raw$GlobalID <- cdncl_build_clean(raw$GlobalID)
  raw <- raw[match(CDNCL_EXPECTED_IDS, raw$NLCS_ID), , drop = FALSE]
  cdncl_build_assert(
    !anyNA(raw$NLCS_ID) && identical(raw$NLCS_ID, CDNCL_EXPECTED_IDS) &&
      !anyDuplicated(raw$GlobalID) && all(nzchar(raw$GlobalID)) &&
      all(!sf::st_is_empty(raw)),
    "Fresh semantic identity, GlobalID lineage, or nonempty geometry gate failed."
  )
  reference <- reference[match(raw$NLCS_ID, reference$nlcs_id), , drop = FALSE]
  source_identity <- source_identity[
    match(raw$NLCS_ID, source_identity$nlcs_id), , drop = FALSE
  ]
  cdncl_build_assert(
    identical(raw$NLCS_NAME, reference$raw_source_name) &&
      identical(raw$NLCS_NAME, source_identity$raw_nlcs_name) &&
      identical(raw$GlobalID, source_identity$source_globalid) &&
      identical(as.character(raw$OBJECTID), as.character(source_identity$source_objectid)),
    "Fresh source names, GlobalID lineage, or diagnostic OBJECTIDs differ from the package identity crosswalk."
  )

  source_raw_qa <- cdncl_build_geometry_stats(raw, "source_raw_untouched")
  source_raw_qa$validity_reason <- sf::st_is_valid(raw, reason = TRUE)
  valid_raw <- sf::st_make_valid(raw)
  valid_raw <- sf::st_cast(valid_raw, "MULTIPOLYGON", warn = FALSE)
  cdncl_build_assert(
    nrow(valid_raw) == 11L && all(sf::st_is_valid(valid_raw)) &&
      all(!sf::st_is_empty(valid_raw)),
    "Fresh geometry validity repair did not retain 11 valid nonempty records."
  )
  raw_qa <- cdncl_build_geometry_stats(valid_raw, "fresh_valid_unsimplified")
  overlaps <- cdncl_build_pairwise_overlap(valid_raw)
  containment <- cdncl_build_containment(valid_raw)
  simplification_qa <- cdncl_build_simplification_qa(valid_raw)
  selected_qa <- simplification_qa[
    simplification_qa$tolerance_m == CDNCL_SELECTED_SIMPLIFY_TOLERANCE_M,
    , drop = FALSE
  ]
  cdncl_build_assert(
    nrow(selected_qa) == 1L && selected_qa$invalid_geometries == 0L &&
      selected_qa$empty_geometries == 0L && selected_qa$exact_part_retention &&
      selected_qa$exact_hole_retention &&
      selected_qa$maximum_unit_absolute_area_change_percent <= 0.01,
    "Selected 2-metre display simplification fails topology or area gates."
  )
  display <- sf::st_simplify(
    valid_raw,
    dTolerance = CDNCL_SELECTED_SIMPLIFY_TOLERANCE_M,
    preserveTopology = TRUE
  )
  display <- sf::st_cast(display, "MULTIPOLYGON", warn = FALSE)
  display_qa <- cdncl_build_geometry_stats(display, "display_2m")

  existing_comparison <- cdncl_build_existing_comparison(
    valid_raw, arguments$existing_rds
  )
  office <- cdncl_build_office_context(valid_raw, arguments$field_office_rds)
  relationships <- cdncl_build_related_context(valid_raw, arguments$related_rds_root)
  cdncl_build_assert(nrow(relationships) > 0L, "No current related-layer context was derived.")

  alias_rows <- aliases[
    tolower(cdncl_build_clean(aliases$recommended_for_search)) == "true",
    , drop = FALSE
  ]
  alias_by_id <- stats::setNames(lapply(raw$NLCS_ID, function(id) {
    paste(unique(cdncl_build_clean(alias_rows$alias[alias_rows$nlcs_id == id])), collapse = " | ")
  }), raw$NLCS_ID)
  office_display_context <- office$context[office$context$display_context, , drop = FALSE]
  office_names_by_id <- stats::setNames(lapply(raw$NLCS_ID, function(id) {
    paste(unique(office_display_context$office_name[
      office_display_context$nlcs_id == id
    ]), collapse = " | ")
  }), raw$NLCS_ID)
  office_keys_by_id <- stats::setNames(lapply(raw$NLCS_ID, function(id) {
    paste(unique(office_display_context$office_key[
      office_display_context$nlcs_id == id
    ]), collapse = "|")
  }), raw$NLCS_ID)
  office_class_by_id <- stats::setNames(vapply(raw$NLCS_ID, function(id) {
    count <- sum(office_display_context$nlcs_id == id)
    if (count > 1L) "crosses_field_office_boundaries" else "one_primary_field_office_context"
  }, character(1)), raw$NLCS_ID)
  relation_families_by_id <- stats::setNames(lapply(raw$NLCS_ID, function(id) {
    paste(unique(relationships$related_layer_key[relationships$nlcs_id == id]), collapse = "|")
  }), raw$NLCS_ID)
  monument_overlap_by_id <- stats::setNames(vapply(raw$NLCS_ID, function(id) {
    any(relationships$nlcs_id == id &
      relationships$related_layer_key == "national_monuments" &
      relationships$normal_popup_suitable)
  }, logical(1)), raw$NLCS_ID)

  reference_index <- match(display$NLCS_ID, reference$nlcs_id)
  normalized_globalid <- tolower(gsub("[{}[:space:]]", "", display$GlobalID))
  display$semantic_unit_id <- display$NLCS_ID
  display$component_id <- paste0(
    "geom:", tolower(display$NLCS_ID), ":", normalized_globalid
  )
  display$pt_cdncl_raw_name <- display$NLCS_NAME
  display$pt_cdncl_display_name <- reference$standardized_display_name[reference_index]
  display$pt_cdncl_aliases <- unname(unlist(alias_by_id[display$NLCS_ID]))
  display$pt_cdncl_global_id <- display$GlobalID
  display$pt_cdncl_source_objectid <- as.character(display$OBJECTID)
  display$pt_cdncl_unit_type_key <- ifelse(
    display$NLCS_ID == "NLCS002012",
    "desert_lily_source_record",
    "drecp_ecoregion_subarea"
  )
  display$pt_cdncl_unit_type_label <- ifelse(
    display$NLCS_ID == "NLCS002012",
    "BLM source-layer Desert Lily record",
    "DRECP ecoregion subarea"
  )
  display$pt_cdncl_field_office_keys <- unname(unlist(office_keys_by_id[display$NLCS_ID]))
  display$pt_cdncl_field_office_names <- unname(unlist(office_names_by_id[display$NLCS_ID]))
  display$pt_cdncl_field_office_context_class <- unname(
    office_class_by_id[display$NLCS_ID]
  )
  display$pt_cdncl_relationship_families <- unname(unlist(
    relation_families_by_id[display$NLCS_ID]
  ))
  display$pt_cdncl_monument_overlap <- ifelse(
    unname(monument_overlap_by_id[display$NLCS_ID]), "yes", "no"
  )
  display$pt_cdncl_source_shape_area_m2 <- suppressWarnings(as.numeric(display$Shape__Area))
  display$pt_cdncl_calculated_raw_area_acres <- raw_qa$area_acres[
    match(display$NLCS_ID, raw_qa$nlcs_id)
  ]
  display$pt_cdncl_official_reported_acres <- suppressWarnings(as.numeric(
    reference$official_reported_acres[reference_index]
  ))
  display$pt_cdncl_last_verified <- format(Sys.Date(), "%Y-%m-%d")
  display$pt_reference_label_text <- display$pt_cdncl_display_name
  display$pt_reference_hover_text <- paste0(
    display$pt_cdncl_display_name, " • ", display$pt_cdncl_unit_type_label
  )
  display <- sf::st_transform(display, 4326)

  metadata <- list(
    candidate_version = CDNCL_CANDIDATE_VERSION,
    created_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    source_snapshot = snapshot,
    source_geojson_sha256 = cdncl_build_sha256(geojson_path),
    semantic_record_count = nrow(display),
    semantic_ids = display$NLCS_ID,
    raw_polygon_parts = sum(raw_qa$polygon_parts),
    raw_holes = sum(raw_qa$holes),
    raw_vertices = sum(raw_qa$vertices),
    display_polygon_parts = sum(display_qa$polygon_parts),
    display_holes = sum(display_qa$holes),
    display_vertices = sum(display_qa$vertices),
    simplify_tolerance_m = CDNCL_SELECTED_SIMPLIFY_TOLERANCE_M,
    exact_part_retention = isTRUE(selected_qa$exact_part_retention),
    exact_hole_retention = isTRUE(selected_qa$exact_hole_retention),
    maximum_unit_absolute_area_change_percent =
      selected_qa$maximum_unit_absolute_area_change_percent,
    field_office_relationship_count = nrow(office$context),
    related_designation_relationship_count = nrow(relationships),
    production_release_authorized = FALSE
  )
  attr(display, "pt_desert_ncl_candidate_metadata") <- metadata

  candidate_rds <- file.path(arguments$output_dir, "reference_cadesert_ncl_wgs84_candidate.rds")
  candidate_gpkg <- file.path(arguments$output_dir, "reference_cadesert_ncl_wgs84_candidate.gpkg")
  saveRDS(display, candidate_rds, version = 3)
  suppressWarnings(sf::st_write(
    display,
    candidate_gpkg,
    layer = "ca_desert_ncl",
    quiet = TRUE,
    delete_dsn = FALSE
  ))
  utils::write.csv(raw_qa, file.path(arguments$output_dir, "geometry_raw_qa.csv"), row.names = FALSE, na = "")
  utils::write.csv(source_raw_qa, file.path(arguments$output_dir, "geometry_source_raw_qa.csv"), row.names = FALSE, na = "")
  utils::write.csv(display_qa, file.path(arguments$output_dir, "geometry_display_qa.csv"), row.names = FALSE, na = "")
  utils::write.csv(simplification_qa, file.path(arguments$output_dir, "simplification_benchmark.csv"), row.names = FALSE, na = "")
  utils::write.csv(overlaps, file.path(arguments$output_dir, "within_layer_overlap_qa.csv"), row.names = FALSE, na = "")
  utils::write.csv(containment, file.path(arguments$output_dir, "within_layer_containment_qa.csv"), row.names = FALSE, na = "")
  utils::write.csv(existing_comparison, file.path(arguments$output_dir, "existing_brim_geometry_comparison.csv"), row.names = FALSE, na = "")
  utils::write.csv(office$offices, file.path(arguments$output_dir, "field_office_lookup.csv"), row.names = FALSE, na = "")
  utils::write.csv(office$context, file.path(arguments$output_dir, "field_office_context.csv"), row.names = FALSE, na = "")
  utils::write.csv(relationships, file.path(arguments$output_dir, "related_designation_context.csv"), row.names = FALSE, na = "")
  utils::write.csv(reference, file.path(arguments$output_dir, "semantic_reference.csv"), row.names = FALSE, na = "")
  utils::write.csv(aliases, file.path(arguments$output_dir, "aliases.csv"), row.names = FALSE, na = "")
  utils::write.csv(documents, file.path(arguments$output_dir, "documents.csv"), row.names = FALSE, na = "")
  utils::write.csv(unit_documents, file.path(arguments$output_dir, "unit_documents.csv"), row.names = FALSE, na = "")
  utils::write.csv(policies, file.path(arguments$output_dir, "common_policy_language.csv"), row.names = FALSE, na = "")
  utils::write.csv(sources, file.path(arguments$output_dir, "source_register.csv"), row.names = FALSE, na = "")

  output_files <- sort(list.files(arguments$output_dir, full.names = TRUE))
  manifest <- data.frame(
    filename = basename(output_files),
    bytes = unname(file.info(output_files)$size),
    sha256 = vapply(output_files, cdncl_build_sha256, character(1)),
    stringsAsFactors = FALSE
  )
  utils::write.csv(manifest, file.path(arguments$output_dir, "candidate_manifest.csv"), row.names = FALSE, na = "")
  metadata$candidate_rds_sha256 <- cdncl_build_sha256(candidate_rds)
  metadata$candidate_gpkg_sha256 <- cdncl_build_sha256(candidate_gpkg)
  metadata$status <- "PASS"
  cdncl_build_write_json(metadata, file.path(arguments$output_dir, "candidate_summary.json"))
  message(arguments$output_dir)
}

cdncl_build_main()
