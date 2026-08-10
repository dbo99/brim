#!/usr/bin/env Rscript

# Build the focused National Park / National Preserve context derivative from
# one COMPLETE NPS Land Resources Division snapshot. Raw sources remain
# untouched. Legislative boundaries are outlines; only reviewed NPS fee and
# less-than-fee tract interests contribute to the subordinate fill.

NPS_CONTEXT_CANDIDATE_VERSION <-
  "BRIM_NPS_PARK_PRESERVE_CONTEXT_CANDIDATE_20260809_01"
NPS_CONTEXT_CODES <- c(
  "CHIS", "DEVA", "JOTR", "KICA", "LAVO",
  "MOJA", "PINN", "REDW", "SEQU", "YOSE"
)
NPS_CONTEXT_TOLERANCES_M <- c(0, 1, 2, 5, 10)
NPS_CONTEXT_SELECTED_BOUNDARY_TOLERANCE_M <- 2
NPS_CONTEXT_SELECTED_LAND_INTEREST_TOLERANCE_M <- 5
NPS_CONTEXT_MAX_AREA_RELATIVE_CHANGE <- 1e-4
NPS_CONTEXT_SQUARE_METERS_PER_SQUARE_MILE <- 2589988.110336

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x) || all(is.na(x))) y else x
}

npc_stop <- function(...) stop(paste0(...), call. = FALSE)
npc_assert <- function(ok, message) {
  if (!isTRUE(ok)) npc_stop(message)
}
npc_utc_now <- function() {
  format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}
npc_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}
npc_write_json_new <- function(value, path) {
  npc_assert(!file.exists(path), paste0("Refusing to overwrite: ", path))
  jsonlite::write_json(
    value, path, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = 17
  )
}
npc_write_csv_new <- function(value, path) {
  npc_assert(!file.exists(path), paste0("Refusing to overwrite: ", path))
  utils::write.csv(value, path, row.names = FALSE, na = "")
}
npc_read_json <- function(path) {
  npc_assert(file.exists(path), paste0("Missing JSON: ", path))
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}
npc_source_map <- function(manifest) {
  stats::setNames(manifest$sources, vapply(
    manifest$sources, `[[`, character(1), "source_key"
  ))
}
npc_vertices <- function(x) {
  sum(vapply(sf::st_geometry(x), function(geometry) {
    nrow(sf::st_coordinates(geometry))
  }, integer(1)))
}
npc_polygonal <- function(geometry) {
  geometry <- suppressWarnings(sf::st_make_valid(geometry))
  geometry <- suppressWarnings(sf::st_collection_extract(geometry, "POLYGON"))
  geometry <- geometry[!sf::st_is_empty(geometry)]
  npc_assert(length(geometry) > 0L, "Polygon operation produced empty geometry.")
  suppressMessages(suppressWarnings(sf::st_union(geometry)))
}
npc_parts <- function(geometry) {
  length(suppressWarnings(sf::st_cast(geometry, "POLYGON", warn = FALSE)))
}
npc_holes <- function(geometry) {
  polygons <- suppressWarnings(sf::st_cast(geometry, "POLYGON", warn = FALSE))
  sum(vapply(polygons, function(polygon) {
    as.integer(max(0L, length(polygon) - 1L))
  }, integer(1)))
}
npc_metric <- function(geometry) {
  c(
    parts = npc_parts(geometry),
    holes = npc_holes(geometry),
    vertices = nrow(sf::st_coordinates(geometry)),
    area_m2 = as.numeric(sf::st_area(geometry))
  )
}
npc_simplify <- function(geometry, tolerance) {
  if (!is.finite(tolerance) || tolerance <= 0) return(npc_polygonal(geometry))
  npc_polygonal(sf::st_simplify(
    geometry,
    dTolerance = tolerance,
    preserveTopology = TRUE
  ))
}
npc_union_rows <- function(x) {
  npc_assert(inherits(x, "sf") && nrow(x) > 0L, "Cannot union an empty tract class.")
  npc_polygonal(sf::st_geometry(x))
}
npc_repair_display_sf <- function(x) {
  repaired <- lapply(sf::st_geometry(x), function(geometry) {
    npc_polygonal(sf::st_sfc(geometry, crs = sf::st_crs(x)))[[1]]
  })
  sf::st_geometry(x) <- sf::st_sfc(repaired, crs = sf::st_crs(x))
  x
}
npc_intersection <- function(left, right) {
  npc_polygonal(suppressWarnings(sf::st_intersection(left, right)))
}
npc_difference <- function(left, right) {
  npc_polygonal(suppressWarnings(sf::st_difference(left, right)))
}

npc_validate_snapshot <- function(snapshot_dir) {
  snapshot_dir <- normalizePath(snapshot_dir, mustWork = TRUE)
  npc_assert(file.exists(file.path(snapshot_dir, "COMPLETE.json")),
             "Snapshot has no COMPLETE.json.")
  npc_assert(!file.exists(file.path(snapshot_dir, "FAILED.json")),
             "Snapshot contains FAILED.json.")
  complete <- npc_read_json(file.path(snapshot_dir, "COMPLETE.json"))
  manifest <- npc_read_json(file.path(snapshot_dir, "acquisition_manifest.json"))
  npc_assert(identical(complete$status, "PASS") && identical(manifest$status, "PASS"),
             "Snapshot is not PASS.")
  sources <- npc_source_map(manifest)
  expected_keys <- c(
    "nps_park_preserve_boundaries", "nps_park_preserve_tracts"
  )
  npc_assert(setequal(names(sources), expected_keys) && length(sources) == 2L,
             "Snapshot does not contain the exact two NPS context sources.")
  npc_assert(as.integer(sources$nps_park_preserve_boundaries$target_feature_count) == 10L,
             "Boundary source count is not 10.")
  npc_assert(as.integer(sources$nps_park_preserve_tracts$target_feature_count) > 0L,
             "Tract source is unexpectedly empty.")
  for (source in sources) {
    npc_assert(identical(source$status, "PASS"), "A source manifest is not PASS.")
    npc_assert(
      setequal(as.character(unlist(source$target_identifiers)), NPS_CONTEXT_CODES),
      paste0(source$source_key, " target-code set differs.")
    )
  }
  list(root = snapshot_dir, manifest = manifest, sources = sources)
}

npc_read_source <- function(snapshot, key) {
  source <- snapshot$sources[[key]]
  path <- file.path(snapshot$root, as.character(source$combined_geojson_path))
  npc_assert(file.exists(path), paste0("Missing acquired GeoJSON: ", path))
  npc_assert(identical(npc_sha256(path), as.character(source$combined_geojson_sha256)),
             paste0(key, " combined GeoJSON hash differs from its manifest."))
  x <- sf::st_read(path, quiet = TRUE, stringsAsFactors = FALSE)
  npc_assert(inherits(x, "sf") && nrow(x) == as.integer(source$target_feature_count),
             paste0(key, " row count differs from its manifest."))
  x
}

npc_classify_interest <- function(value) {
  value <- trimws(as.character(value))
  value[is.na(value)] <- ""
  mapping <- stats::setNames(
    c(
      "nps_fee", "nps_less_than_fee", "other_federal",
      "public_nonfederal", "private", "acquisition_deferred",
      "no_information"
    ),
    c(
      "Federal Land (Fee)", "Federal Land (Less than Fee)",
      "Other Federal Land", "Public", "Private",
      "Acquisition Deferred", ""
    )
  )
  unexpected <- setdiff(unique(value), names(mapping))
  npc_assert(!length(unexpected), paste0(
    "Unreviewed NPS Interest value(s): ", paste(unexpected, collapse = ", ")
  ))
  unname(mapping[value])
}

npc_build <- function(snapshot_dir, output_root, candidate_id) {
  required_packages <- c("sf", "dplyr", "jsonlite", "digest")
  missing <- required_packages[!vapply(
    required_packages, requireNamespace, logical(1), quietly = TRUE
  )]
  npc_assert(!length(missing), paste0(
    "Missing required R package(s): ", paste(missing, collapse = ", ")
  ))
  snapshot <- npc_validate_snapshot(snapshot_dir)
  output_root <- normalizePath(path.expand(output_root), mustWork = FALSE)
  if (!dir.exists(output_root)) dir.create(output_root, recursive = TRUE)
  output_dir <- file.path(output_root, candidate_id)
  npc_assert(!file.exists(output_dir), paste0("Refusing to overwrite: ", output_dir))
  npc_assert(dir.create(output_dir, recursive = TRUE), "Could not create candidate directory.")
  started <- npc_utc_now()

  result <- tryCatch({
    old_s2 <- sf::sf_use_s2(FALSE)
    on.exit(sf::sf_use_s2(old_s2), add = TRUE)
    boundaries_raw <- npc_read_source(snapshot, "nps_park_preserve_boundaries")
    tracts_raw <- npc_read_source(snapshot, "nps_park_preserve_tracts")
    boundary_required <- c(
      "OBJECTID", "UNIT_CODE", "UNIT_NAME", "STATE", "UNIT_TYPE", "Status", "AreaID"
    )
    tract_required <- c(
      "OBJECTID", "TRACT_ID", "ALPHA", "ESTATE", "Interest", "GlobalID", "AreaID"
    )
    npc_assert(!length(setdiff(boundary_required, names(boundaries_raw))),
               "Boundary source is missing required attributes.")
    npc_assert(!length(setdiff(tract_required, names(tracts_raw))),
               "Tract source is missing required attributes.")
    npc_assert(!anyDuplicated(boundaries_raw$OBJECTID) &&
                 !anyDuplicated(tracts_raw$OBJECTID),
               "Source object IDs are not unique.")
    npc_assert(setequal(boundaries_raw$UNIT_CODE, NPS_CONTEXT_CODES) &&
                 setequal(tracts_raw$ALPHA, NPS_CONTEXT_CODES),
               "Boundary/tract target-code coverage differs.")
    npc_assert(all(boundaries_raw$Status == "Official"),
               "A target unit boundary is not Official.")
    npc_assert(
      setequal(unique(boundaries_raw$UNIT_TYPE),
               c("National Parks", "National Preserves")),
      "Target boundary types differ from Parks/Preserves."
    )
    type_counts <- table(boundaries_raw$UNIT_TYPE)
    npc_assert(type_counts[["National Parks"]] == 9L &&
                 type_counts[["National Preserves"]] == 1L,
               "Current unit-type counts differ from 9 parks / 1 preserve.")

    tracts_raw$display_class <- npc_classify_interest(tracts_raw$Interest)
    class_levels <- c(
      "nps_fee", "nps_less_than_fee", "other_federal",
      "public_nonfederal", "private", "acquisition_deferred", "no_information"
    )
    class_inventory <- as.data.frame(table(
      factor(tracts_raw$display_class, levels = class_levels)
    ), stringsAsFactors = FALSE)
    names(class_inventory) <- c("display_class", "tract_count")
    class_inventory$source_interest <- c(
      "Federal Land (Fee)", "Federal Land (Less than Fee)",
      "Other Federal Land", "Public", "Private",
      "Acquisition Deferred", "blank/null"
    )
    class_inventory$public_interpretation <- c(
      "NPS fee land; fill after conservative non-NPS status masking",
      "NPS less-than-fee interest; fill as interest, never call fee ownership",
      "another federal agency; do not fill as NPS",
      "state/local/quasi-public; do not fill as NPS",
      "private; do not fill as NPS",
      "acquisition deferred; do not fill as NPS",
      "no status information; do not fill as NPS"
    )

    raw_invalid <- c(
      boundaries = sum(!sf::st_is_valid(boundaries_raw)),
      tracts = sum(!sf::st_is_valid(tracts_raw))
    )
    raw_empty <- c(
      boundaries = sum(sf::st_is_empty(boundaries_raw)),
      tracts = sum(sf::st_is_empty(tracts_raw))
    )
    raw_vertices <- c(
      boundaries = npc_vertices(boundaries_raw),
      tracts = npc_vertices(tracts_raw)
    )
    npc_assert(!any(raw_empty), "Raw source contains empty target geometry.")

    boundaries <- sf::st_make_valid(boundaries_raw)
    boundaries <- suppressWarnings(sf::st_collection_extract(
      boundaries, "POLYGON"
    ))
    boundaries <- sf::st_transform(boundaries, 3310)
    tracts <- sf::st_make_valid(tracts_raw)
    tracts <- suppressWarnings(sf::st_collection_extract(tracts, "POLYGON"))
    tracts <- sf::st_transform(tracts, 3310)
    npc_assert(nrow(boundaries) == 10L && nrow(tracts) == nrow(tracts_raw),
               "Validity repair changed the source record count.")

    boundary_geometries <- list()
    interest_geometries <- list()
    unit_rows <- list()
    benchmark_rows <- list()
    class_count <- function(unit_tracts, key) {
      sum(unit_tracts$display_class == key)
    }

    for (code in sort(NPS_CONTEXT_CODES)) {
      message("Deriving NPS context unit ", code)
      boundary_row <- boundaries[boundaries$UNIT_CODE == code, , drop = FALSE]
      unit_tracts <- tracts[tracts$ALPHA == code, , drop = FALSE]
      npc_assert(nrow(boundary_row) == 1L && nrow(unit_tracts) > 0L,
                 paste0(code, " source coverage is incomplete."))
      boundary <- npc_union_rows(boundary_row)
      fee <- npc_union_rows(unit_tracts[
        unit_tracts$display_class == "nps_fee", , drop = FALSE
      ])
      less_rows <- unit_tracts[
        unit_tracts$display_class == "nps_less_than_fee", , drop = FALSE
      ]
      non_nps_rows <- unit_tracts[unit_tracts$display_class %in% c(
        "other_federal", "public_nonfederal", "private",
        "acquisition_deferred", "no_information"
      ), , drop = FALSE]
      non_nps <- npc_union_rows(non_nps_rows)
      fee_inside <- npc_intersection(fee, boundary)
      non_nps_inside <- npc_intersection(non_nps, boundary)
      fee_non_nps_overlap <- suppressWarnings(sf::st_intersection(
        fee_inside, non_nps_inside
      ))
      fee_non_nps_overlap_m2 <- if (length(fee_non_nps_overlap)) {
        sum(as.numeric(sf::st_area(fee_non_nps_overlap)))
      } else 0
      fee_display <- npc_difference(fee_inside, non_nps_inside)
      interest_display <- fee_display
      less_non_nps_overlap_m2 <- 0
      less_source_area_m2 <- 0
      less_inside_area_m2 <- 0
      if (nrow(less_rows)) {
        less_source <- npc_union_rows(less_rows)
        less_source_area_m2 <- as.numeric(sf::st_area(less_source))
        less_intersection <- suppressWarnings(sf::st_intersection(
          less_source, boundary
        ))
        less_polygons <- suppressWarnings(sf::st_collection_extract(
          sf::st_make_valid(less_intersection), "POLYGON"
        ))
        less_polygons <- less_polygons[!sf::st_is_empty(less_polygons)]
        if (length(less_polygons)) {
          less_inside <- npc_polygonal(less_polygons)
          less_inside_area_m2 <- as.numeric(sf::st_area(less_inside))
          less_overlap <- suppressWarnings(sf::st_intersection(
            less_inside, non_nps_inside
          ))
          less_non_nps_overlap_m2 <- if (length(less_overlap)) {
            sum(as.numeric(sf::st_area(less_overlap)))
          } else 0
          # Less-than-fee is an NPS interest over an underlying ownership. Keep
          # that explicit interest even when an ownership tract overlaps it.
          interest_display <- npc_polygonal(c(fee_display, less_inside))
        }
      }

      boundary_base <- npc_metric(boundary)
      interest_base <- npc_metric(interest_display)
      simplified_by_role <- list()
      for (role in c("legislative_boundary", "nps_land_or_interest")) {
        base_geometry <- if (role == "legislative_boundary") boundary else interest_display
        base_metric <- if (role == "legislative_boundary") boundary_base else interest_base
        selected_tolerance <- if (role == "legislative_boundary") {
          NPS_CONTEXT_SELECTED_BOUNDARY_TOLERANCE_M
        } else {
          NPS_CONTEXT_SELECTED_LAND_INTEREST_TOLERANCE_M
        }
        for (tolerance in NPS_CONTEXT_TOLERANCES_M) {
          simplified <- npc_simplify(base_geometry, tolerance)
          metric <- npc_metric(simplified)
          benchmark_rows[[length(benchmark_rows) + 1L]] <- data.frame(
            unit_code = code,
            geometry_role = role,
            tolerance_m = tolerance,
            polygon_parts = as.integer(metric[["parts"]]),
            holes = as.integer(metric[["holes"]]),
            vertices = as.integer(metric[["vertices"]]),
            area_m2 = as.numeric(metric[["area_m2"]]),
            area_relative_change = abs(
              as.numeric(metric[["area_m2"]]) - as.numeric(base_metric[["area_m2"]])
            ) / as.numeric(base_metric[["area_m2"]]),
            exact_part_retention = metric[["parts"]] == base_metric[["parts"]],
            exact_hole_retention = metric[["holes"]] == base_metric[["holes"]],
            stringsAsFactors = FALSE
          )
          if (tolerance == selected_tolerance) {
            simplified_by_role[[role]] <- simplified
          }
        }
      }
      selected_rows <- Filter(function(row) {
        row$unit_code == code && (
          (row$geometry_role == "legislative_boundary" &&
             row$tolerance_m == NPS_CONTEXT_SELECTED_BOUNDARY_TOLERANCE_M) ||
          (row$geometry_role == "nps_land_or_interest" &&
             row$tolerance_m == NPS_CONTEXT_SELECTED_LAND_INTEREST_TOLERANCE_M)
        )
      }, benchmark_rows)
      selected_rows <- do.call(rbind, selected_rows)
      npc_assert(all(selected_rows$exact_part_retention) &&
                   all(selected_rows$exact_hole_retention) &&
                   max(selected_rows$area_relative_change) <=
                     NPS_CONTEXT_MAX_AREA_RELATIVE_CHANGE,
                 paste0(code, " selected simplification failed its geometry gate."))

      boundary_geometries[[code]] <- simplified_by_role$legislative_boundary[[1]]
      interest_geometries[[code]] <- simplified_by_role$nps_land_or_interest[[1]]
      non_nps_area_m2 <- as.numeric(sf::st_area(non_nps_inside))
      unit_rows[[code]] <- data.frame(
        unit_code = code,
        unit_name = as.character(boundary_row$UNIT_NAME[[1]]),
        unit_type_key = if (boundary_row$UNIT_TYPE[[1]] == "National Parks") {
          "national_park"
        } else "national_preserve",
        unit_type_label = if (boundary_row$UNIT_TYPE[[1]] == "National Parks") {
          "National Park"
        } else "National Preserve",
        states = as.character(boundary_row$STATE[[1]]),
        boundary_object_id = as.character(boundary_row$OBJECTID[[1]]),
        area_id = as.integer(boundary_row$AreaID[[1]]),
        official_status = as.character(boundary_row$Status[[1]]),
        raw_tract_count = nrow(unit_tracts),
        nps_fee_tract_count = class_count(unit_tracts, "nps_fee"),
        nps_less_than_fee_tract_count = class_count(unit_tracts, "nps_less_than_fee"),
        other_federal_tract_count = class_count(unit_tracts, "other_federal"),
        public_nonfederal_tract_count = class_count(unit_tracts, "public_nonfederal"),
        private_tract_count = class_count(unit_tracts, "private"),
        acquisition_deferred_tract_count = class_count(unit_tracts, "acquisition_deferred"),
        no_information_tract_count = class_count(unit_tracts, "no_information"),
        legislative_boundary_area_sq_mi =
          as.numeric(boundary_base[["area_m2"]]) /
          NPS_CONTEXT_SQUARE_METERS_PER_SQUARE_MILE,
        displayed_land_interest_area_sq_mi =
          as.numeric(interest_base[["area_m2"]]) /
          NPS_CONTEXT_SQUARE_METERS_PER_SQUARE_MILE,
        non_nps_status_area_sq_mi = non_nps_area_m2 /
          NPS_CONTEXT_SQUARE_METERS_PER_SQUARE_MILE,
        fee_non_nps_overlap_resolved_sq_mi = fee_non_nps_overlap_m2 /
          NPS_CONTEXT_SQUARE_METERS_PER_SQUARE_MILE,
        less_than_fee_underlying_status_overlap_sq_mi =
          less_non_nps_overlap_m2 / NPS_CONTEXT_SQUARE_METERS_PER_SQUARE_MILE,
        less_than_fee_source_area_sq_mi = less_source_area_m2 /
          NPS_CONTEXT_SQUARE_METERS_PER_SQUARE_MILE,
        less_than_fee_inside_boundary_area_sq_mi = less_inside_area_m2 /
          NPS_CONTEXT_SQUARE_METERS_PER_SQUARE_MILE,
        official_page = paste0("https://www.nps.gov/", tolower(code), "/index.htm"),
        stringsAsFactors = FALSE
      )
    }

    unit_inventory <- do.call(rbind, unit_rows)
    rownames(unit_inventory) <- NULL
    benchmark <- do.call(rbind, benchmark_rows)
    boundary_geometry <- sf::st_sfc(
      unname(boundary_geometries), crs = 3310
    )
    interest_geometry <- sf::st_sfc(
      unname(interest_geometries), crs = 3310
    )
    boundaries_display <- sf::st_sf(unit_inventory, geometry = boundary_geometry) |>
      sf::st_transform(4326)
    land_interest_display <- sf::st_sf(unit_inventory, geometry = interest_geometry) |>
      sf::st_transform(4326)
    boundary_transform_invalid <- boundaries_display$unit_code[
      !sf::st_is_valid(boundaries_display)
    ]
    land_transform_invalid <- land_interest_display$unit_code[
      !sf::st_is_valid(land_interest_display)
    ]
    boundaries_before_transform_repair <- boundaries_display
    land_before_transform_repair <- land_interest_display
    boundaries_display <- npc_repair_display_sf(boundaries_display)
    land_interest_display <- npc_repair_display_sf(land_interest_display)
    repair_area_relative_change <- function(before, after) {
      before_area <- as.numeric(sf::st_area(sf::st_transform(before, 3310)))
      after_area <- as.numeric(sf::st_area(sf::st_transform(after, 3310)))
      abs(after_area - before_area) / before_area
    }
    transform_repair_relative <- c(
      repair_area_relative_change(
        boundaries_before_transform_repair, boundaries_display
      ),
      repair_area_relative_change(
        land_before_transform_repair, land_interest_display
      )
    )
    npc_assert(
      max(transform_repair_relative) <= 1e-6,
      "Final EPSG:4326 validity repair exceeded the area-change gate."
    )
    boundaries_display$context_geometry_role <- "legislative_boundary"
    land_interest_display$context_geometry_role <- "nps_land_or_interest"
    boundaries_display$context_geometry_key <- paste0(
      "nps_context_boundary_", tolower(boundaries_display$unit_code)
    )
    land_interest_display$context_geometry_key <- paste0(
      "nps_context_land_interest_", tolower(land_interest_display$unit_code)
    )
    boundary_bad <- boundaries_display$unit_code[
      !sf::st_is_valid(boundaries_display) | sf::st_is_empty(boundaries_display)
    ]
    land_bad <- land_interest_display$unit_code[
      !sf::st_is_valid(land_interest_display) |
        sf::st_is_empty(land_interest_display)
    ]
    npc_assert(!length(boundary_bad) && !length(land_bad), paste0(
      "Final context display geometry is invalid or empty; boundary=",
      paste(boundary_bad, collapse = ","), "; land_interest=",
      paste(land_bad, collapse = ",")
    ))

    selected_benchmark <- benchmark[
      (benchmark$geometry_role == "legislative_boundary" &
         benchmark$tolerance_m == NPS_CONTEXT_SELECTED_BOUNDARY_TOLERANCE_M) |
        (benchmark$geometry_role == "nps_land_or_interest" &
           benchmark$tolerance_m ==
             NPS_CONTEXT_SELECTED_LAND_INTEREST_TOLERANCE_M),
      , drop = FALSE
    ]
    metadata <- list(
      candidate_version = NPS_CONTEXT_CANDIDATE_VERSION,
      acquisition_snapshot_id = snapshot$manifest$snapshot_id,
      acquisition_manifest_sha256 = npc_sha256(file.path(
        snapshot$root, "acquisition_manifest.json"
      )),
      built_utc = npc_utc_now(),
      target_codes = sort(NPS_CONTEXT_CODES),
      park_count = 9L,
      preserve_count = 1L,
      cross_border_units_retained_whole = "DEVA",
      raw_boundary_count = nrow(boundaries_raw),
      raw_tract_count = nrow(tracts_raw),
      raw_boundary_vertices = unname(raw_vertices[["boundaries"]]),
      raw_tract_vertices = unname(raw_vertices[["tracts"]]),
      raw_boundary_invalid_repaired = unname(raw_invalid[["boundaries"]]),
      raw_tract_invalid_repaired = unname(raw_invalid[["tracts"]]),
      selected_boundary_simplify_tolerance_m =
        NPS_CONTEXT_SELECTED_BOUNDARY_TOLERANCE_M,
      selected_land_interest_simplify_tolerance_m =
        NPS_CONTEXT_SELECTED_LAND_INTEREST_TOLERANCE_M,
      boundary_display_feature_count = nrow(boundaries_display),
      land_interest_display_feature_count = nrow(land_interest_display),
      total_display_feature_count = nrow(boundaries_display) + nrow(land_interest_display),
      pre_transform_display_vertices = sum(selected_benchmark$vertices),
      pre_transform_display_polygon_parts = sum(selected_benchmark$polygon_parts),
      pre_transform_display_holes = sum(selected_benchmark$holes),
      display_vertices = npc_vertices(boundaries_display) +
        npc_vertices(land_interest_display),
      display_polygon_parts = sum(vapply(
        c(sf::st_geometry(boundaries_display), sf::st_geometry(land_interest_display)),
        function(geometry) npc_parts(sf::st_sfc(geometry, crs = 4326)),
        integer(1)
      )),
      display_holes = sum(vapply(
        c(sf::st_geometry(boundaries_display), sf::st_geometry(land_interest_display)),
        function(geometry) npc_holes(sf::st_sfc(geometry, crs = 4326)),
        integer(1)
      )),
      maximum_area_relative_change = max(selected_benchmark$area_relative_change),
      exact_part_retention = all(selected_benchmark$exact_part_retention),
      exact_hole_retention = all(selected_benchmark$exact_hole_retention),
      epsg4326_transform_invalid_boundary_units =
        paste(boundary_transform_invalid, collapse = "|"),
      epsg4326_transform_invalid_land_interest_units =
        paste(land_transform_invalid, collapse = "|"),
      epsg4326_transform_repair_count = length(boundary_transform_invalid) +
        length(land_transform_invalid),
      maximum_epsg4326_transform_repair_area_relative_change =
        max(transform_repair_relative),
      classification = stats::setNames(
        class_inventory$public_interpretation,
        class_inventory$display_class
      ),
      precedence = paste(
        "Federal Land (Fee) is masked wherever authoritative private, public,",
        "other-federal, deferred, or no-information status overlaps it; explicit",
        "less-than-fee records remain visible as NPS interests, not fee ownership."
      ),
      production_release_authorized = FALSE
    )
    candidate <- list(
      boundaries = boundaries_display[order(boundaries_display$unit_code), ],
      land_interest = land_interest_display[order(land_interest_display$unit_code), ]
    )
    attr(candidate, "pt_nps_context_candidate_metadata") <- metadata

    candidate_path <- file.path(output_dir, "nps_park_preserve_context_candidate.rds")
    saveRDS(candidate, candidate_path, compress = "xz")
    gpkg_path <- file.path(output_dir, "nps_park_preserve_context_candidate.gpkg")
    sf::st_write(candidate$land_interest, gpkg_path,
                 layer = "nps_land_interest", quiet = TRUE)
    sf::st_write(candidate$boundaries, gpkg_path,
                 layer = "legislative_boundaries", append = TRUE, quiet = TRUE)
    class_path <- file.path(output_dir, "tract_status_classification.csv")
    unit_path <- file.path(output_dir, "unit_inventory.csv")
    benchmark_path <- file.path(output_dir, "simplification_benchmark.csv")
    npc_write_csv_new(class_inventory, class_path)
    npc_write_csv_new(unit_inventory, unit_path)
    npc_write_csv_new(benchmark, benchmark_path)

    artifact_paths <- c(candidate_path, gpkg_path, class_path, unit_path, benchmark_path)
    manifest <- list(
      status = "PASS",
      candidate_version = NPS_CONTEXT_CANDIDATE_VERSION,
      candidate_id = candidate_id,
      built_started_utc = started,
      built_completed_utc = npc_utc_now(),
      source_snapshot = snapshot$manifest$snapshot_id,
      source_snapshot_path = snapshot$root,
      source_counts = list(boundaries = nrow(boundaries_raw), tracts = nrow(tracts_raw)),
      display_counts = list(
        boundaries = nrow(candidate$boundaries),
        land_interest = nrow(candidate$land_interest),
        total = nrow(candidate$boundaries) + nrow(candidate$land_interest)
      ),
      geometry_qa = metadata[c(
        "raw_boundary_vertices", "raw_tract_vertices",
        "raw_boundary_invalid_repaired", "raw_tract_invalid_repaired",
        "selected_boundary_simplify_tolerance_m",
        "selected_land_interest_simplify_tolerance_m", "display_vertices",
        "display_polygon_parts", "display_holes",
        "maximum_area_relative_change", "exact_part_retention",
        "exact_hole_retention", "epsg4326_transform_repair_count",
        "maximum_epsg4326_transform_repair_area_relative_change"
      )],
      artifacts = lapply(artifact_paths, function(path) list(
        path = basename(path),
        bytes = unname(as.numeric(file.info(path)$size)),
        sha256 = npc_sha256(path)
      )),
      production_release_authorized = FALSE
    )
    manifest_path <- file.path(output_dir, "candidate_manifest.json")
    npc_write_json_new(manifest, manifest_path)
    npc_write_json_new(list(
      status = "PASS",
      candidate_id = candidate_id,
      completed_utc = npc_utc_now(),
      candidate_sha256 = npc_sha256(candidate_path)
    ), file.path(output_dir, "COMPLETE.json"))
    cat(jsonlite::toJSON(
      manifest, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = 17
    ), "\n")
    invisible(manifest)
  }, error = function(error) {
    failed_path <- file.path(output_dir, "FAILED.json")
    if (!file.exists(failed_path)) {
      npc_write_json_new(list(
        status = "FAIL",
        candidate_id = candidate_id,
        failed_utc = npc_utc_now(),
        error = conditionMessage(error)
      ), failed_path)
    }
    stop(error)
  })
  invisible(result)
}

npc_parse_args <- function(arguments) {
  values <- list(snapshot = NULL, output_root = NULL, candidate_id = NULL)
  index <- 1L
  while (index <= length(arguments)) {
    key <- arguments[[index]]
    allowed <- c("--snapshot", "--output-root", "--candidate-id")
    if (!key %in% allowed) npc_stop("Unknown argument: ", key)
    if (index == length(arguments)) npc_stop("Missing value for ", key)
    values[[gsub("-", "_", sub("^--", "", key))]] <- arguments[[index + 1L]]
    index <- index + 2L
  }
  if (any(vapply(values, is.null, logical(1)))) {
    npc_stop("--snapshot, --output-root, and --candidate-id are required.")
  }
  npc_assert(grepl("^[A-Za-z0-9._-]+$", values$candidate_id),
             "candidate-id contains unsupported characters.")
  values
}

if (sys.nframe() == 0L) {
  arguments <- npc_parse_args(commandArgs(trailingOnly = TRUE))
  tryCatch(
    npc_build(arguments$snapshot, arguments$output_root, arguments$candidate_id),
    error = function(error) {
      message("NPS CONTEXT CANDIDATE FAIL: ", conditionMessage(error))
      quit(status = 1L, save = "no")
    }
  )
}
