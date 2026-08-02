# ==== 69_build_major_water_supply_basin_geometry.R ==========================
#
# Build the reviewed static geometry foundation for CNRFC water-year FNF
# forecast watersheds and Sacramento/San Joaquin index watersheds.
#
# This script assembles geometry only. It must never calculate forecast values.
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
})

required_namespaces <- c("digest", "rmapshaper")
missing_namespaces <- required_namespaces[
  !vapply(required_namespaces, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_namespaces) > 0L) {
  stop(
    "Install the required package(s) before building major-basin geometry: ",
    paste(missing_namespaces, collapse = ", ")
  )
}

source(file.path("00_config", "config_paths.r"))
source(file.path("03_functions", "cache_helpers.r"))
source(file.path("03_functions", "spatial_helpers.r"))

MAJOR_BASIN_GEOMETRY_BUILD_VERSION <- "2026-07-31.4"
SQ_METERS_PER_SQ_MILE <- 2589988.110336
ACRES_PER_SQ_MILE <- 640
DERIVED_CLEANUP_POLICY <-
  "derived_watershed_holes_and_microscopic_parts"
DERIVED_PART_AREA_THRESHOLD_SQ_MI <- 0.01
DERIVED_CLEANUP_AREA_CHANGE_GUARDRAIL_PCT <- 1.0
POST_SIMPLIFY_AREA_CHANGE_GUARDRAIL_PCT <- 0.01
CBRFC_DISPLAY_AREA_CHANGE_GUARDRAIL_PCT <- 0.05
OUTLET_COORDINATE_TOLERANCE_METERS <- 250

CATALOG_PATH <- file.path(
  DIR$config,
  "major_water_supply_basin_geometry_catalog.csv"
)
MANIFEST_PATH <- file.path(
  DIR$config,
  "major_water_supply_basin_component_manifest.csv"
)
SOURCE_MANIFEST_PATH <- file.path(
  DIR$config,
  "major_water_supply_basin_source_manifest.csv"
)
CBRFC_SELECTOR_PATH <- file.path(
  DIR$config,
  "major_water_supply_basin_cbrfc_selector_manifest.csv"
)
PRODUCT_MAPPING_PATH <- file.path(
  DIR$config,
  "major_water_supply_basin_product_mapping.csv"
)
PHASE_B1_HASH_PATH <- file.path(
  DIR$config,
  "major_water_supply_basin_phase_b1_geometry_hashes.csv"
)
CBRFC_BASINS_PATH <- file.path(
  DIR$raw,
  "cbrfc_forecast_geometry",
  "CBRFC_Basins.shp"
)
CBRFC_OUTLETS_PATH <- paste0(
  "/vsizip/",
  file.path(
    DIR$raw,
    "cbrfc_forecast_geometry",
    "CBRFC_Outlets.zip",
    "CBRFC_Outlets",
    "CBRFC_Outlets.shp"
  )
)
HUC2_PATHS <- c(
  HUC2_14_UPPER_COLORADO_CONTEXT = paste0(
    "/vsizip/",
    file.path(DIR$raw, "co_riv", "WBD_14_HU2_Shape.zip", "Shape", "WBDHU2.shp")
  ),
  HUC2_15_LOWER_COLORADO_CONTEXT = paste0(
    "/vsizip/",
    file.path(DIR$raw, "co_riv", "WBD_15_HU2_Shape.zip", "Shape", "WBDHU2.shp")
  )
)
FNF_FULL_PATH <- file.path(DIR$rds, "cnrfc_fnf_delta_wgs84.rds")
FNF_MAP_PATH <- file.path(DIR$cache_last, "cnrfc_fnf_delta_map.rds")
MODEL_FULL_PATH <- file.path(DIR$rds, "cnrfc_basins_wgs84.rds")

FNF_SOURCE_OBJECT <- "04_processed_data/rds/cnrfc_fnf_delta_wgs84.rds"
MODEL_SOURCE_OBJECT <- "04_processed_data/rds/cnrfc_basins_wgs84.rds"
DERIVED_SOURCE_OBJECT <- "derived_geometry_3310"

OUTPUT_3310_PATH <- file.path(
  DIR$rds,
  "major_water_supply_basin_geometry_3310.rds"
)
OUTPUT_MAP_PATH <- file.path(
  DIR$cache_last,
  "major_water_supply_basin_geometry_map.rds"
)

EXPECTED_ORIGINAL_IDS <- c(
  "SCSC1_FNF", "TMDC1_FNF", "SHDC1_FNF", "CEGC1_FNF", "ORDC1_FNF",
  "HLEC1_FNF", "FOLC1_FNF", "NDPC1_FNF", "EXQC1_FNF", "FRAC1_FNF",
  "PFTC1_FNF", "ISAC1_FNF", "MHBC1_FNF", "CMPC1_FNF", "NMSC1_FNF"
)
DERIVED_BUILD_ORDER <- c(
  "BDBC1_FNF", "SACC0_FNF", "VNSC0_FNF", "MLIC0_FNF"
)
CBRFC_OPERATIONAL_IDS <- c(
  "GLDA3_CBRFC_MODELED_UPSTREAM",
  "LKSA3_CBRFC_LOCAL_INTERVENING"
)
HUC2_CONTEXT_IDS <- names(HUC2_PATHS)
EXPECTED_GEOMETRY_IDS <- c(
  EXPECTED_ORIGINAL_IDS,
  DERIVED_BUILD_ORDER,
  CBRFC_OPERATIONAL_IDS,
  HUC2_CONTEXT_IDS
)
EXPECTED_COMPONENTS <- list(
  BDBC1_FNF = c(
    "SHDC1_FNF", "WHSC1_MODEL", "BDBC1_MODEL", "CWAC1_MODEL",
    "CWCC1_MODEL", "COTC1_MODEL", "KWKC1_MODEL", "RDGC1_MODEL"
  ),
  SACC0_FNF = c("BDBC1_FNF", "ORDC1_FNF", "HLEC1_FNF", "FOLC1_FNF"),
  VNSC0_FNF = c("FRAC1_FNF", "EXQC1_FNF", "NDPC1_FNF", "NMSC1_FNF"),
  MLIC0_FNF = c("SACC0_FNF", "VNSC0_FNF")
)

CATALOG_COLUMNS <- c(
  "schema_version", "geometry_id", "forecast_key", "rfc", "nws_lid",
  "product_type", "geometry_role", "view_group", "display_name", "river_name",
  "reservoir_name", "source_geometry_type", "geometry_source",
  "source_id_field", "source_id_value", "component_manifest_id", "source_url",
  "observed_url", "generalization_note", "include_in_display", "display_order",
  "display_simplify_keep", "cleanup_policy"
)
MANIFEST_COLUMNS <- c(
  "schema_version", "component_manifest_id", "derived_geometry_id",
  "component_order", "component_geometry_id", "component_source_role",
  "source_object", "source_id_field", "source_id_value", "operation",
  "required", "decision_note"
)

assert_true <- function(condition, message_text) {
  if (!isTRUE(condition)) stop(message_text, call. = FALSE)
  invisible(TRUE)
}

parse_reviewed_logical <- function(x, field_name) {
  normalized <- toupper(trimws(as.character(x)))
  assert_true(
    all(normalized %in% c("TRUE", "FALSE")),
    paste0(field_name, " must contain only TRUE or FALSE.")
  )
  normalized == "TRUE"
}

assert_required_columns <- function(x, required, label) {
  missing <- setdiff(required, names(x))
  assert_true(
    length(missing) == 0L,
    paste0(label, " is missing required column(s): ", paste(missing, collapse = ", "))
  )
}

assert_unique_key <- function(x, columns, label) {
  key <- do.call(paste, c(x[columns], sep = "\r"))
  duplicated_rows <- which(duplicated(key) | duplicated(key, fromLast = TRUE))
  assert_true(
    length(duplicated_rows) == 0L,
    paste0(
      label, " must be unique by ", paste(columns, collapse = " + "),
      "; duplicate row(s): ", paste(duplicated_rows, collapse = ", ")
    )
  )
}

sha256_file <- function(path) {
  digest::digest(
    object = path,
    algo = "sha256",
    serialize = FALSE,
    file = TRUE
  )
}

count_vertices <- function(geometry) {
  coordinates <- sf::st_coordinates(sf::st_geometry(geometry))
  if (is.null(dim(coordinates))) 0L else nrow(coordinates)
}

count_polygon_parts <- function(geometry) {
  polygons <- suppressWarnings(
    sf::st_cast(sf::st_geometry(geometry), "POLYGON", warn = FALSE)
  )
  length(polygons)
}

area_square_miles <- function(geometry) {
  as.numeric(sf::st_area(geometry)) / SQ_METERS_PER_SQ_MILE
}

count_polygon_holes <- function(geometry) {
  features <- sf::st_geometry(geometry)
  sum(vapply(features, function(feature) {
    if (inherits(feature, "POLYGON")) {
      return(max(length(feature) - 1L, 0L))
    }
    if (inherits(feature, "MULTIPOLYGON")) {
      return(sum(vapply(feature, function(polygon) {
        max(length(polygon) - 1L, 0L)
      }, integer(1))))
    }
    stop(
      "Cannot count holes in non-polygonal geometry: ",
      paste(class(feature), collapse = "/"),
      call. = FALSE
    )
  }, integer(1)))
}

fill_polygon_holes_sfg <- function(feature) {
  if (inherits(feature, "POLYGON")) {
    return(sf::st_polygon(list(feature[[1]])))
  }
  if (inherits(feature, "MULTIPOLYGON")) {
    return(sf::st_multipolygon(lapply(feature, function(polygon) {
      list(polygon[[1]])
    })))
  }
  stop(
    "Cannot fill holes in non-polygonal geometry: ",
    paste(class(feature), collapse = "/"),
    call. = FALSE
  )
}

cleanup_derived_geometry <- function(geometry, geometry_id) {
  before <- sf::st_sf(
    geometry_id = geometry_id,
    geometry = sf::st_sfc(geometry, crs = 3310)
  )
  before <- normalize_polygon_feature(before, paste0(geometry_id, " pre-cleanup"))
  assert_true(
    all(sf::st_is_valid(before)) && !any(sf::st_is_empty(before)),
    paste0(geometry_id, " pre-cleanup geometry must be valid and non-empty.")
  )

  area_before <- area_square_miles(before)
  holes_before <- count_polygon_holes(before)
  parts_before <- count_polygon_parts(before)
  vertices_before <- count_vertices(before)

  filled_sfg <- fill_polygon_holes_sfg(sf::st_geometry(before)[[1]])
  filled <- sf::st_sf(
    geometry_id = geometry_id,
    geometry = sf::st_sfc(filled_sfg, crs = 3310)
  )
  filled <- normalize_polygon_feature(filled, paste0(geometry_id, " hole-filled"))
  area_filled <- area_square_miles(filled)

  parts <- suppressWarnings(
    sf::st_cast(sf::st_geometry(filled), "POLYGON", warn = FALSE)
  )
  assert_true(length(parts) > 0L, paste0(geometry_id, " has no parts after hole filling."))
  part_areas <- as.numeric(sf::st_area(parts)) / SQ_METERS_PER_SQ_MILE
  largest_part <- which.max(part_areas)
  keep_part <- part_areas >= DERIVED_PART_AREA_THRESHOLD_SQ_MI
  keep_part[[largest_part]] <- TRUE
  removed_part_areas <- part_areas[!keep_part]

  assert_true(
    length(removed_part_areas) == 0L ||
      all(removed_part_areas < DERIVED_PART_AREA_THRESHOLD_SQ_MI),
    paste0(geometry_id, " cleanup attempted to remove a meaningful part.")
  )
  assert_true(
    keep_part[[largest_part]],
    paste0(geometry_id, " cleanup must always retain the largest part.")
  )

  cleaned_geometry <- sf::st_union(parts[keep_part])
  cleaned <- sf::st_sf(
    geometry_id = geometry_id,
    geometry = cleaned_geometry
  )
  cleaned <- normalize_polygon_feature(cleaned, paste0(geometry_id, " cleaned"))
  cleaned <- make_valid_if_needed(cleaned)
  assert_true(
    nrow(cleaned) == 1L && all(sf::st_is_valid(cleaned)) &&
      !any(sf::st_is_empty(cleaned)),
    paste0(geometry_id, " cleanup produced empty or invalid geometry.")
  )

  area_after <- area_square_miles(cleaned)
  total_change <- area_after - area_before
  total_change_pct <- 100 * total_change / area_before
  holes_after <- count_polygon_holes(cleaned)
  assert_true(
    holes_after == 0L,
    paste0(geometry_id, " still contains holes after approved cleanup.")
  )
  assert_true(
    abs(total_change_pct) <= DERIVED_CLEANUP_AREA_CHANGE_GUARDRAIL_PCT,
    paste0(
      geometry_id, " cleanup changed area by ",
      format(round(total_change_pct, 6), trim = TRUE),
      "%, exceeding the ", DERIVED_CLEANUP_AREA_CHANGE_GUARDRAIL_PCT,
      "% safety guardrail."
    )
  )

  metrics <- data.frame(
    geometry_id = geometry_id,
    cleanup_applied = TRUE,
    cleanup_holes_before = holes_before,
    cleanup_holes_after = holes_after,
    cleanup_part_count_before = parts_before,
    cleanup_part_count_after = count_polygon_parts(cleaned),
    cleanup_detached_parts_removed = length(removed_part_areas),
    cleanup_detached_area_removed_sq_mi = sum(removed_part_areas),
    cleanup_largest_removed_part_sq_mi = if (
      length(removed_part_areas) == 0L
    ) 0 else max(removed_part_areas),
    cleanup_area_added_holes_sq_mi = area_filled - area_before,
    cleanup_total_area_change_sq_mi = total_change,
    cleanup_total_area_change_pct = total_change_pct,
    cleanup_vertex_count_before = vertices_before,
    cleanup_vertex_count_after = count_vertices(cleaned),
    cleanup_part_area_threshold_sq_mi = DERIVED_PART_AREA_THRESHOLD_SQ_MI,
    cleanup_area_guardrail_pct = DERIVED_CLEANUP_AREA_CHANGE_GUARDRAIL_PCT,
    stringsAsFactors = FALSE
  )

  list(
    geometry = sf::st_geometry(cleaned)[[1]],
    metrics = metrics
  )
}

polygon_members <- function(feature) {
  if (inherits(feature, "POLYGON")) return(list(feature))
  if (inherits(feature, "MULTIPOLYGON")) return(unclass(feature))
  stop(
    "Expected polygonal geometry, found: ",
    paste(class(feature), collapse = "/"),
    call. = FALSE
  )
}

interior_ring_metrics <- function(geometry, geometry_id) {
  polygons <- polygon_members(sf::st_geometry(geometry)[[1]])
  rows <- list()
  row_index <- 0L

  for (part_index in seq_along(polygons)) {
    polygon <- polygons[[part_index]]
    if (length(polygon) <= 1L) next

    for (ring_index in seq.int(2L, length(polygon))) {
      row_index <- row_index + 1L
      ring <- sf::st_sfc(
        sf::st_polygon(list(polygon[[ring_index]])),
        crs = 3310
      )
      area_sq_mi <- area_square_miles(ring)
      rows[[row_index]] <- data.frame(
        geometry_id = geometry_id,
        artifact_type = "interior_ring",
        artifact_index = row_index,
        owner_part = part_index,
        action = "fill",
        area_sq_mi = area_sq_mi,
        area_acres = area_sq_mi * ACRES_PER_SQ_MILE,
        inside_retained_exterior_footprint = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }

  dplyr::bind_rows(rows)
}

simplify_once_for_display <- function(x, keep, layer_label) {
  assert_true(
    inherits(x, "sf") && nrow(x) == 1L,
    paste0(layer_label, " display simplification requires one sf feature.")
  )
  input <- sf::st_zm(x, drop = TRUE, what = "ZM")
  input <- make_valid_if_needed(input)

  simplified <- try(
    rmapshaper::ms_simplify(
      input = input,
      keep = keep,
      keep_shapes = TRUE,
      explode = FALSE
    ),
    silent = TRUE
  )
  assert_true(
    !inherits(simplified, "try-error") &&
      inherits(simplified, "sf") && nrow(simplified) == 1L,
    paste0("Display simplification failed for ", layer_label, ".")
  )

  raw_geometry_type <- as.character(sf::st_geometry_type(simplified)[[1]])
  raw_valid <- isTRUE(sf::st_is_valid(simplified)[[1]])
  validity_repair_applied <- !raw_valid
  if (validity_repair_applied) simplified <- sf::st_make_valid(simplified)

  if (any(sf::st_geometry_type(simplified) == "GEOMETRYCOLLECTION")) {
    polygonal <- suppressWarnings(
      sf::st_collection_extract(sf::st_geometry(simplified), "POLYGON")
    )
    assert_true(
      length(polygonal) > 0L && !all(sf::st_is_empty(polygonal)),
      paste0(layer_label, " validity repair produced no polygonal geometry.")
    )
    simplified <- sf::st_sf(
      sf::st_drop_geometry(simplified)[1, , drop = FALSE],
      geometry = sf::st_union(polygonal)
    )
  }

  simplified <- normalize_polygon_feature(
    simplified,
    paste0(layer_label, " simplified display")
  )
  assert_true(
    all(sf::st_is_valid(simplified)) && !any(sf::st_is_empty(simplified)),
    paste0(layer_label, " simplified display must be valid and non-empty.")
  )

  list(
    geometry = simplified,
    raw_geometry_type = raw_geometry_type,
    raw_valid = raw_valid,
    validity_repair_applied = validity_repair_applied,
    valid_geometry_type = as.character(sf::st_geometry_type(simplified)[[1]])
  )
}

normalize_derived_display_geometry <- function(x, geometry_id) {
  before <- normalize_polygon_feature(
    x,
    paste0(geometry_id, " post-simplification pre-normalization")
  )
  assert_true(
    all(sf::st_is_valid(before)) && !any(sf::st_is_empty(before)),
    paste0(geometry_id, " pre-normalization display must be valid and non-empty.")
  )

  area_before <- area_square_miles(before)
  holes_before <- count_polygon_holes(before)
  parts_before <- count_polygon_parts(before)
  vertices_before <- count_vertices(before)
  hole_artifacts <- interior_ring_metrics(before, geometry_id)

  filled <- sf::st_sf(
    geometry_id = geometry_id,
    geometry = sf::st_sfc(
      fill_polygon_holes_sfg(sf::st_geometry(before)[[1]]),
      crs = 3310
    )
  )
  filled <- normalize_polygon_feature(
    filled,
    paste0(geometry_id, " post-simplification hole-filled")
  )
  area_filled <- area_square_miles(filled)

  parts <- suppressWarnings(
    sf::st_cast(sf::st_geometry(filled), "POLYGON", warn = FALSE)
  )
  assert_true(
    length(parts) > 0L,
    paste0(geometry_id, " has no display parts after hole filling.")
  )
  part_areas <- as.numeric(sf::st_area(parts)) / SQ_METERS_PER_SQ_MILE
  largest_part <- which.max(part_areas)
  keep_part <- part_areas >= DERIVED_PART_AREA_THRESHOLD_SQ_MI
  keep_part[[largest_part]] <- TRUE
  removed_indices <- which(!keep_part)
  removed_part_areas <- part_areas[removed_indices]

  assert_true(
    length(removed_part_areas) == 0L ||
      all(removed_part_areas < DERIVED_PART_AREA_THRESHOLD_SQ_MI),
    paste0(geometry_id, " post-simplification normalization attempted to remove a meaningful part.")
  )
  assert_true(
    keep_part[[largest_part]],
    paste0(geometry_id, " post-simplification normalization must retain the largest part.")
  )

  removed_artifacts <- if (length(removed_indices) == 0L) {
    data.frame()
  } else {
    data.frame(
      geometry_id = geometry_id,
      artifact_type = "detached_polygon_part",
      artifact_index = seq_along(removed_indices),
      owner_part = removed_indices,
      action = "remove",
      area_sq_mi = removed_part_areas,
      area_acres = removed_part_areas * ACRES_PER_SQ_MILE,
      inside_retained_exterior_footprint = FALSE,
      stringsAsFactors = FALSE
    )
  }

  normalized <- sf::st_sf(
    geometry_id = geometry_id,
    geometry = sf::st_union(parts[keep_part])
  )
  if (!all(sf::st_is_valid(normalized))) {
    normalized <- sf::st_make_valid(normalized)
  }
  if (any(sf::st_geometry_type(normalized) == "GEOMETRYCOLLECTION")) {
    polygonal <- suppressWarnings(
      sf::st_collection_extract(sf::st_geometry(normalized), "POLYGON")
    )
    assert_true(
      length(polygonal) > 0L && !all(sf::st_is_empty(polygonal)),
      paste0(geometry_id, " final normalization produced no polygonal geometry.")
    )
    normalized <- sf::st_sf(
      geometry_id = geometry_id,
      geometry = sf::st_union(polygonal)
    )
  }
  normalized <- normalize_polygon_feature(
    normalized,
    paste0(geometry_id, " final normalized display")
  )

  final_part_areas <- as.numeric(sf::st_area(suppressWarnings(
    sf::st_cast(sf::st_geometry(normalized), "POLYGON", warn = FALSE)
  ))) / SQ_METERS_PER_SQ_MILE
  final_largest_part <- which.max(final_part_areas)
  assert_true(
    length(final_part_areas) == 1L ||
      all(final_part_areas[-final_largest_part] >=
        DERIVED_PART_AREA_THRESHOLD_SQ_MI),
    paste0(geometry_id, " final display retains a microscopic detached part.")
  )
  assert_true(
    all(sf::st_is_valid(normalized)) && !any(sf::st_is_empty(normalized)) &&
      count_polygon_holes(normalized) == 0L,
    paste0(geometry_id, " final display is empty, invalid, or contains holes.")
  )

  area_after <- area_square_miles(normalized)
  area_change <- area_after - area_before
  area_change_pct <- 100 * area_change / area_before
  assert_true(
    abs(area_change_pct) <= POST_SIMPLIFY_AREA_CHANGE_GUARDRAIL_PCT,
    paste0(
      geometry_id, " post-simplification normalization changed area by ",
      format(round(area_change_pct, 8), trim = TRUE),
      "%, exceeding the ", POST_SIMPLIFY_AREA_CHANGE_GUARDRAIL_PCT,
      "% guardrail."
    )
  )

  metrics <- data.frame(
    geometry_id = geometry_id,
    post_simplify_normalization_applied = TRUE,
    post_simplify_holes_before = holes_before,
    post_simplify_holes_after = count_polygon_holes(normalized),
    post_simplify_parts_before = parts_before,
    post_simplify_parts_after = count_polygon_parts(normalized),
    post_simplify_detached_parts_removed = length(removed_part_areas),
    post_simplify_detached_area_removed_sq_mi = sum(removed_part_areas),
    post_simplify_largest_removed_part_sq_mi = if (
      length(removed_part_areas) == 0L
    ) 0 else max(removed_part_areas),
    post_simplify_hole_area_added_sq_mi = area_filled - area_before,
    post_simplify_normalization_area_change_sq_mi = area_change,
    post_simplify_normalization_area_change_pct = area_change_pct,
    post_simplify_vertices_before = vertices_before,
    post_simplify_vertices_after = count_vertices(normalized),
    post_simplify_part_area_threshold_sq_mi =
      DERIVED_PART_AREA_THRESHOLD_SQ_MI,
    post_simplify_area_guardrail_pct =
      POST_SIMPLIFY_AREA_CHANGE_GUARDRAIL_PCT,
    stringsAsFactors = FALSE
  )

  list(
    geometry = normalized,
    metrics = metrics,
    artifacts = dplyr::bind_rows(hole_artifacts, removed_artifacts)
  )
}

atomic_replace <- function(path, writer) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = paste0(".", basename(path), "."),
    tmpdir = dirname(path)
  )
  backup <- NULL
  on.exit({
    if (file.exists(temporary)) unlink(temporary)
  }, add = TRUE)

  writer(temporary)
  assert_true(file.exists(temporary), paste0("Writer did not create: ", temporary))

  if (file.exists(path)) {
    backup <- tempfile(
      pattern = paste0(".", basename(path), ".backup."),
      tmpdir = dirname(path)
    )
    assert_true(
      file.rename(path, backup),
      paste0("Could not move existing output aside for atomic replacement: ", path)
    )
  }

  replaced <- file.rename(temporary, path)
  if (!replaced && !is.null(backup) && file.exists(backup)) {
    restored <- file.rename(backup, path)
    if (!restored) {
      stop(
        "Atomic replacement failed and the previous output remains at: ",
        backup,
        call. = FALSE
      )
    }
  }
  assert_true(replaced, paste0("Atomic replacement failed: ", path))

  if (!is.null(backup) && file.exists(backup)) unlink(backup)
  invisible(path)
}

atomic_save_rds <- function(x, path) {
  atomic_replace(path, function(temporary) saveRDS(x, temporary))
}

atomic_write_csv <- function(x, path) {
  atomic_replace(
    path,
    function(temporary) readr::write_csv(x, temporary, na = "")
  )
}

read_and_validate_catalog <- function(path = CATALOG_PATH) {
  assert_true(file.exists(path), paste0("Missing geometry catalog: ", path))
  catalog <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = character()
  )
  assert_required_columns(catalog, CATALOG_COLUMNS, "Geometry catalog")
  catalog <- catalog[, CATALOG_COLUMNS]

  catalog$include_in_display <- parse_reviewed_logical(
    catalog$include_in_display,
    "Geometry catalog include_in_display"
  )
  catalog$display_order <- suppressWarnings(as.integer(catalog$display_order))
  catalog$display_simplify_keep <- suppressWarnings(
    as.numeric(catalog$display_simplify_keep)
  )

  assert_true(nrow(catalog) == 23L, "Geometry catalog must contain exactly 23 rows.")
  nonblank_catalog_columns <- c(
    "schema_version", "geometry_id", "geometry_role", "view_group", "display_name",
    "source_geometry_type", "geometry_source", "source_id_field",
    "source_id_value", "source_url", "generalization_note", "cleanup_policy"
  )
  assert_true(
    all(vapply(catalog[nonblank_catalog_columns], function(column) {
      all(nzchar(trimws(column)))
    }, logical(1))),
    "Geometry catalog contains an unexpected blank identity or source value."
  )
  assert_true(
    !anyNA(catalog$display_order) && !anyNA(catalog$display_simplify_keep),
    "Geometry catalog display_order and display_simplify_keep must be numeric."
  )
  assert_unique_key(catalog, "geometry_id", "Geometry catalog")
  forecast_rows <- nzchar(catalog$forecast_key)
  assert_unique_key(catalog[forecast_rows, , drop = FALSE], "forecast_key", "Geometry catalog")
  assert_unique_key(catalog, "display_order", "Geometry catalog")
  assert_true(
    identical(catalog$geometry_id, EXPECTED_GEOMETRY_IDS),
    "Geometry catalog IDs or order differ from the reviewed 19 California plus four additive records."
  )
  assert_true(
    all(grepl("^(CNRFC|CBRFC):", catalog$forecast_key[forecast_rows])),
    "Every populated catalog forecast_key must be a literal producer key."
  )
  assert_true(
    !any(catalog$forecast_key[forecast_rows] %in% c(catalog$nws_lid, catalog$geometry_id)),
    "A forecast_key cannot be a bare LID or a geometry_id."
  )
  original <- catalog$geometry_id %in% EXPECTED_ORIGINAL_IDS
  assert_true(
    all(catalog$geometry_role[original] == "fnf_forecast_watershed") &&
      all(catalog$view_group[original] == "major_basin") &&
      all(catalog$include_in_display[original]),
    "Original FNF rows must be displayed major-basin forecast watersheds."
  )
  cbrfc <- catalog$geometry_id %in% CBRFC_OPERATIONAL_IDS
  assert_true(
    all(catalog$source_geometry_type[cbrfc] == "cbrfc_reviewed_dissolve") &&
      all(catalog$display_simplify_keep[cbrfc] == 0.05) &&
      all(catalog$include_in_display[cbrfc]),
    "CBRFC operational rows must use reviewed selector dissolves and keep=0.05."
  )
  context <- catalog$geometry_id %in% HUC2_CONTEXT_IDS
  assert_true(
    all(catalog$geometry_role[context] == "context_only") &&
      all(catalog$source_geometry_type[context] == "usgs_wbd_huc2") &&
      all(catalog$forecast_key[context] == "") &&
      all(catalog$product_type[context] == "") &&
      all(catalog$display_simplify_keep[context] == 0.01),
    "HUC2 rows must be context-only, unmapped catalog records at keep=0.01."
  )
  assert_true(
    all(
      catalog$source_geometry_type[original] == "retained_assembled_fnf" &
        catalog$geometry_source[original] == FNF_SOURCE_OBJECT &
        catalog$source_id_field[original] == "nws5id" &
        catalog$source_id_value[original] == catalog$nws_lid[original] &
        catalog$component_manifest_id[original] == ""
    ),
    "Original FNF catalog rows have an incorrect or ambiguous source contract."
  )
  assert_true(
    all(catalog$display_simplify_keep[original] == 0.20) &&
      all(catalog$cleanup_policy[original] == "none"),
    "Original FNF rows must use keep=0.20 and cleanup_policy=none."
  )
  bdbc <- catalog$geometry_id == "BDBC1_FNF"
  assert_true(
    catalog$geometry_role[bdbc] == "fnf_forecast_watershed" &&
      catalog$view_group[bdbc] == "index_component" &&
      !catalog$include_in_display[bdbc],
    "BDBC1_FNF catalog identity fields do not match the reviewed contract."
  )
  index <- catalog$geometry_id %in% c("SACC0_FNF", "VNSC0_FNF", "MLIC0_FNF")
  assert_true(
    all(catalog$geometry_role[index] == "fnf_index_watershed") &&
      all(catalog$view_group[index] == "index") &&
      all(catalog$include_in_display[index]),
    "Index rows must be displayed fnf_index_watershed features."
  )
  derived <- catalog$geometry_id %in% DERIVED_BUILD_ORDER
  assert_true(
    all(
      catalog$source_geometry_type[derived] == "derived_dissolve" &
        catalog$geometry_source[derived] ==
          "00_config/major_water_supply_basin_component_manifest.csv" &
        catalog$source_id_field[derived] == "component_manifest_id" &
        catalog$source_id_value[derived] ==
          catalog$component_manifest_id[derived]
    ),
    "Derived catalog rows have an incorrect component-manifest source contract."
  )
  assert_true(
    all(catalog$display_simplify_keep[derived] == 0.10) &&
      all(catalog$cleanup_policy[derived] == DERIVED_CLEANUP_POLICY),
    paste0(
      "Derived rows must use keep=0.10 and cleanup_policy=",
      DERIVED_CLEANUP_POLICY,
      "."
    )
  )
  assert_true(
    all(catalog$display_simplify_keep > 0 &
      catalog$display_simplify_keep <= 1),
    "Every display_simplify_keep must be greater than zero and at most one."
  )

  catalog[order(catalog$display_order), , drop = FALSE]
}

read_and_validate_manifest <- function(catalog, path = MANIFEST_PATH) {
  assert_true(file.exists(path), paste0("Missing component manifest: ", path))
  manifest <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = character()
  )
  assert_required_columns(manifest, MANIFEST_COLUMNS, "Component manifest")
  manifest <- manifest[, MANIFEST_COLUMNS]

  manifest$required <- parse_reviewed_logical(
    manifest$required,
    "Component manifest required"
  )
  manifest$component_order <- suppressWarnings(as.integer(manifest$component_order))

  assert_true(nrow(manifest) == 18L, "Component manifest must contain exactly 18 rows.")
  assert_true(
    !anyNA(manifest$component_order),
    "Component manifest component_order values must be integers."
  )
  assert_unique_key(
    manifest,
    c("component_manifest_id", "component_order"),
    "Component manifest"
  )
  assert_unique_key(
    manifest,
    c("derived_geometry_id", "component_geometry_id"),
    "Component manifest"
  )
  assert_true(
    setequal(unique(manifest$derived_geometry_id), DERIVED_BUILD_ORDER),
    "Component manifest must describe exactly the four reviewed derived geometries."
  )
  assert_true(
    all(manifest$operation == "union") && all(manifest$required),
    "Every reviewed geometry component must be a required union component."
  )

  for (derived_id in DERIVED_BUILD_ORDER) {
    rows <- manifest[manifest$derived_geometry_id == derived_id, , drop = FALSE]
    rows <- rows[order(rows$component_order), , drop = FALSE]
    expected <- EXPECTED_COMPONENTS[[derived_id]]
    assert_true(
      identical(rows$component_order, seq_along(expected)) &&
        identical(rows$component_geometry_id, expected),
      paste0("Wrong component roster or order for ", derived_id, ".")
    )
    catalog_manifest <- catalog$component_manifest_id[
      catalog$geometry_id == derived_id
    ]
    assert_true(
      length(unique(rows$component_manifest_id)) == 1L &&
        identical(unique(rows$component_manifest_id), catalog_manifest),
      paste0("Catalog/manifest ID mismatch for ", derived_id, ".")
    )
  }

  fnf_rows <- manifest$source_object == FNF_SOURCE_OBJECT
  model_rows <- manifest$source_object == MODEL_SOURCE_OBJECT
  derived_rows <- manifest$source_object == DERIVED_SOURCE_OBJECT
  assert_true(
    all(fnf_rows | model_rows | derived_rows),
    "Component manifest contains an unapproved source object."
  )
  assert_true(
    all(
      manifest$component_source_role[fnf_rows] == "fnf_forecast_watershed" &
        manifest$source_id_field[fnf_rows] == "nws5id" &
        manifest$component_geometry_id[fnf_rows] ==
          paste0(manifest$source_id_value[fnf_rows], "_FNF")
    ),
    "Assembled FNF components have an incorrect source role, field, or ID."
  )
  assert_true(
    all(
      manifest$component_source_role[model_rows] == "cnrfc_model_subbasin" &
        manifest$source_id_field[model_rows] == "Basin" &
        manifest$component_geometry_id[model_rows] ==
          paste0(manifest$source_id_value[model_rows], "_MODEL")
    ),
    "Detailed model components have an incorrect source role, field, or ID."
  )
  assert_true(
    all(
      manifest$source_id_field[derived_rows] == "geometry_id" &
        manifest$component_geometry_id[derived_rows] ==
          manifest$source_id_value[derived_rows]
    ),
    "Derived components must select by geometry_id."
  )
  for (row_index in which(derived_rows)) {
    referenced_id <- manifest$source_id_value[[row_index]]
    expected_role <- catalog$geometry_role[catalog$geometry_id == referenced_id]
    assert_true(
      length(expected_role) == 1L &&
        manifest$component_source_role[[row_index]] == expected_role,
      paste0("Wrong derived source role for component ", referenced_id, ".")
    )
  }

  manifest[order(
    match(manifest$derived_geometry_id, DERIVED_BUILD_ORDER),
    manifest$component_order
  ), , drop = FALSE]
}

read_character_config <- function(path, required_columns, label) {
  assert_true(file.exists(path), paste0("Missing ", label, ": ", path))
  value <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = character()
  )
  assert_required_columns(value, required_columns, label)
  value
}

read_and_validate_source_manifest <- function() {
  columns <- c(
    "schema_version", "source_id", "source_family", "component_name",
    "relative_path", "retrieval_url", "retrieved_at_utc",
    "server_last_modified_utc", "size_bytes", "sha256", "feature_count",
    "geometry_type", "schema_fields", "crs", "required", "decision_note"
  )
  manifest <- read_character_config(
    SOURCE_MANIFEST_PATH, columns, "Phase B2 source manifest"
  )
  manifest$required <- parse_reviewed_logical(manifest$required, "Source required")
  manifest$size_bytes <- suppressWarnings(as.numeric(manifest$size_bytes))
  assert_true(nrow(manifest) == 8L, "Source manifest must contain eight reviewed files.")
  assert_unique_key(manifest, "source_id", "Source manifest")
  assert_true(all(manifest$required), "Every Phase B2 source file is required.")
  for (index in seq_len(nrow(manifest))) {
    path <- file.path(DIR$root, manifest$relative_path[[index]])
    assert_true(file.exists(path), paste0("Missing reviewed source file: ", path))
    assert_true(
      identical(unname(file.info(path)$size), manifest$size_bytes[[index]]) &&
        identical(sha256_file(path), manifest$sha256[[index]]),
      paste0("Source size/hash mismatch: ", path)
    )
  }
  manifest
}

read_and_validate_cbrfc_selectors <- function() {
  columns <- c(
    "schema_version", "selector_manifest_id", "geometry_id",
    "selector_order", "source_id", "selection_field", "selection_value",
    "expected_match_count", "operation", "required", "decision_note"
  )
  selectors <- read_character_config(
    CBRFC_SELECTOR_PATH, columns, "CBRFC selector manifest"
  )
  selectors$selector_order <- suppressWarnings(as.integer(selectors$selector_order))
  selectors$expected_match_count <- suppressWarnings(
    as.integer(selectors$expected_match_count)
  )
  selectors$required <- parse_reviewed_logical(selectors$required, "Selector required")
  assert_true(
    nrow(selectors) == 5L && !anyNA(selectors$selector_order) &&
      !anyNA(selectors$expected_match_count) && all(selectors$required) &&
      all(selectors$operation == "union") &&
      all(selectors$source_id == "CBRFC_BASINS"),
    "CBRFC selector manifest differs from the five reviewed union selectors."
  )
  assert_unique_key(
    selectors, c("selector_manifest_id", "selector_order"), "CBRFC selectors"
  )
  selectors[order(
    match(selectors$geometry_id, CBRFC_OPERATIONAL_IDS),
    selectors$selector_order
  ), , drop = FALSE]
}

geometry_sha256 <- function(x) {
  raw <- sf::st_as_binary(sf::st_geometry(x), EWKB = TRUE)[[1]]
  digest::digest(raw, algo = "sha256", serialize = FALSE)
}

geometry_serialized_bytes <- function(x) {
  length(serialize(sf::st_geometry(x), NULL, version = 3))
}

dissolve_selected_5070 <- function(selected, geometry_id) {
  selected_5070 <- sf::st_transform(selected, 5070)
  unioned <- sf::st_sf(
    geometry_id = geometry_id,
    geometry = sf::st_union(sf::st_geometry(selected_5070))
  )
  unioned <- make_valid_if_needed(unioned)
  normalize_polygon_feature(unioned, geometry_id)
}

build_phase_b2_geometry <- function(source_manifest, selectors) {
  basins <- sf::st_read(CBRFC_BASINS_PATH, quiet = TRUE)
  outlets <- sf::st_read(CBRFC_OUTLETS_PATH, quiet = TRUE)
  validate_source_sf(
    basins,
    c("ch5_id", "segment", "fgid", "reg", "x_outlet", "y_outlet"),
    "CBRFC operational basins"
  )
  validate_source_sf(outlets, c("field_2", "x_outlet", "y_outlet"), "CBRFC outlets")
  assert_true(
    nrow(basins) == 537L && nrow(outlets) == 534L &&
      identical(as.integer(sf::st_crs(basins)$epsg), 4269L) &&
      identical(as.integer(sf::st_crs(outlets)$epsg), 4269L),
    "CBRFC basin/outlet count or EPSG:4269 source contract changed."
  )
  assert_true(
    identical(
      paste(names(sf::st_drop_geometry(basins)), collapse = ";"),
      source_manifest$schema_fields[source_manifest$source_id == "CBRFC_BASINS_SHP"]
    ) && identical(
      paste(names(sf::st_drop_geometry(outlets)), collapse = ";"),
      source_manifest$schema_fields[source_manifest$source_id == "CBRFC_OUTLETS_ZIP"]
    ),
    "CBRFC basin or outlet attribute schema changed from the recorded manifest."
  )

  geometry_by_id <- list()
  selector_audit <- list()
  selected_by_id <- list()
  for (geometry_id in CBRFC_OPERATIONAL_IDS) {
    recipe <- selectors[selectors$geometry_id == geometry_id, , drop = FALSE]
    selected_indices <- integer()
    for (index in seq_len(nrow(recipe))) {
      field <- recipe$selection_field[[index]]
      value <- recipe$selection_value[[index]]
      assert_true(field %in% names(basins), paste0("Missing CBRFC selector field: ", field))
      matches <- which(as.character(basins[[field]]) == value)
      assert_true(
        length(matches) == recipe$expected_match_count[[index]],
        paste0(geometry_id, " selector ", field, "=", value, " count changed.")
      )
      selected_indices <- c(selected_indices, matches)
      selector_audit[[paste(geometry_id, index)]] <- data.frame(
        geometry_id = geometry_id,
        selector_order = recipe$selector_order[[index]],
        selection_field = field,
        selection_value = value,
        expected_match_count = recipe$expected_match_count[[index]],
        actual_match_count = length(matches),
        stringsAsFactors = FALSE
      )
    }
    assert_true(
      length(selected_indices) == length(unique(selected_indices)),
      paste0(geometry_id, " selector groups overlap unexpectedly.")
    )
    selected <- basins[sort(selected_indices), , drop = FALSE]
    selected_by_id[[geometry_id]] <- selected
    union_5070 <- dissolve_selected_5070(selected, geometry_id)
    area_sq_mi <- area_square_miles(union_5070)
    expected_area <- if (geometry_id == CBRFC_OPERATIONAL_IDS[[1]]) {
      107832.882
    } else {
      59009.121
    }
    assert_true(
      abs(area_sq_mi - expected_area) <= 5,
      paste0(geometry_id, " area differs by more than five square miles from reconnaissance.")
    )
    if (geometry_id == "GLDA3_CBRFC_MODELED_UPSTREAM") {
      assert_true(
        nrow(selected) == 203L && identical(unique(as.character(selected$reg)), "UC"),
        "GLDA3 must select exactly 203 UC basins and no GSL/LC basin."
      )
    } else {
      assert_true(
        nrow(selected) == 79L && identical(unique(as.character(selected$reg)), "LC") &&
          setequal(unique(as.character(selected$fgid)), c("MEAD", "LITCOL", "VIRGIN", "MUDLV")),
        "LKSA3 must contain only the reviewed 79 LC basin records and four groups."
      )
    }
    union_3310 <- sf::st_transform(union_5070, 3310)
    geometry_by_id[[geometry_id]] <- sf::st_geometry(union_3310)[[1]]
  }

  huc_sources <- list()
  huc_benchmark <- list()
  huc_candidates <- list()
  for (geometry_id in HUC2_CONTEXT_IDS) {
    huc <- sf::st_read(HUC2_PATHS[[geometry_id]], quiet = TRUE)
    expected_huc <- if (geometry_id == HUC2_CONTEXT_IDS[[1]]) "14" else "15"
    assert_true(
      nrow(huc) == 1L && identical(as.character(huc$huc2[[1]]), expected_huc) &&
        identical(as.integer(sf::st_crs(huc)$epsg), 4269L),
      paste0(geometry_id, " must resolve to one matching EPSG:4269 HUC2 feature.")
    )
    source_id <- if (expected_huc == "14") "WBD_HUC2_14_ZIP" else "WBD_HUC2_15_ZIP"
    assert_true(
      identical(
        paste(names(sf::st_drop_geometry(huc)), collapse = ";"),
        source_manifest$schema_fields[source_manifest$source_id == source_id]
      ),
      paste0(geometry_id, " attribute schema changed from the recorded manifest.")
    )
    huc_3310 <- normalize_polygon_feature(sf::st_transform(huc, 3310), geometry_id)
    geometry_by_id[[geometry_id]] <- sf::st_geometry(huc_3310)[[1]]
    huc_sources[[geometry_id]] <- huc_3310
    huc_candidates[[geometry_id]] <- list()
    for (keep in c(0.005, 0.01, 0.02, 0.05)) {
      candidate <- simplify_once_for_display(
        huc_3310, keep, paste0(geometry_id, " benchmark keep=", keep)
      )$geometry
      key <- format(keep, nsmall = 3, trim = TRUE)
      huc_candidates[[geometry_id]][[key]] <- candidate
      huc_benchmark[[paste(geometry_id, key)]] <- data.frame(
        geometry_id = geometry_id,
        simplify_keep = keep,
        source_area_sq_mi = area_square_miles(huc_3310),
        display_area_sq_mi = area_square_miles(candidate),
        area_change_pct = 100 *
          (area_square_miles(candidate) - area_square_miles(huc_3310)) /
          area_square_miles(huc_3310),
        source_vertices = count_vertices(huc_3310),
        display_vertices = count_vertices(candidate),
        source_parts = count_polygon_parts(huc_3310),
        display_parts = count_polygon_parts(candidate),
        source_holes = count_polygon_holes(huc_3310),
        display_holes = count_polygon_holes(candidate),
        display_valid = all(sf::st_is_valid(candidate)),
        display_serialized_bytes = geometry_serialized_bytes(candidate),
        stringsAsFactors = FALSE
      )
    }
  }
  lower_bbox <- sf::st_bbox(sf::st_transform(huc_sources[[HUC2_CONTEXT_IDS[[2]]]], 4326))
  assert_true(
    lower_bbox[["xmin"]] < -115.5 && lower_bbox[["ymin"]] < 33,
    "HUC2 15 no longer includes the expected eastern/southeastern California extent."
  )

  outlet_audit <- lapply(c(GLDA3 = CBRFC_OPERATIONAL_IDS[[1]], LKSA3 = CBRFC_OPERATIONAL_IDS[[2]]), function(geometry_id) {
    lid <- if (grepl("GLDA3", geometry_id)) "GLDA3" else "LKSA3"
    outlet <- outlets[startsWith(as.character(outlets$field_2), lid), , drop = FALSE]
    assert_true(nrow(outlet) == 1L, paste0(lid, " must resolve once in CBRFC outlets."))
    union_5070 <- sf::st_transform(
      sf::st_sf(geometry_id = geometry_id, geometry = sf::st_sfc(geometry_by_id[[geometry_id]], crs = 3310)),
      5070
    )
    outlet_5070 <- sf::st_transform(outlet, 5070)
    distance <- as.numeric(sf::st_distance(outlet_5070, union_5070))
    assert_true(
      distance <= OUTLET_COORDINATE_TOLERANCE_METERS,
      paste0(lid, " outlet is outside the documented coordinate tolerance.")
    )
    data.frame(
      geometry_id = geometry_id,
      nws_lid = lid,
      outlet_source_id = as.character(outlet$field_2[[1]]),
      distance_to_union_meters = distance,
      coordinate_tolerance_meters = OUTLET_COORDINATE_TOLERANCE_METERS,
      arbitrary_buffer_applied = FALSE,
      passed = TRUE,
      stringsAsFactors = FALSE
    )
  })

  future_selected <- basins[
    as.character(basins$reg) == "UC" |
      as.character(basins$fgid) %in% c("MEAD", "LITCOL", "VIRGIN", "MUDLV"),
    , drop = FALSE
  ]
  assert_true(nrow(future_selected) == 282L, "Deferred total-Mead selector must resolve 282 basins.")
  future_union <- dissolve_selected_5070(future_selected, "FUTURE_TOTAL_MEAD_QA_ONLY")
  future_audit <- data.frame(
    candidate_id = "FUTURE_TOTAL_MEAD_QA_ONLY",
    retained_geometry = FALSE,
    forecast_value_attached = FALSE,
    selector = "reg == UC OR fgid in MEAD|LITCOL|VIRGIN|MUDLV",
    basin_count = nrow(future_selected),
    area_sq_mi = area_square_miles(future_union),
    reconnaissance_area_sq_mi = 166842.003,
    stringsAsFactors = FALSE
  )

  local_source <- sf::st_sf(
    geometry_id = CBRFC_OPERATIONAL_IDS[[2]],
    geometry = sf::st_sfc(geometry_by_id[[CBRFC_OPERATIONAL_IDS[[2]]]], crs = 3310)
  )
  local_holes <- interior_ring_metrics(local_source, CBRFC_OPERATIONAL_IDS[[2]])
  assert_true(
    nrow(local_holes) == 1L && abs(local_holes$area_sq_mi[[1]] - 1.486) <= 0.02,
    "LKSA3 source must retain the reviewed approximately 1.486-square-mile interior gap."
  )

  list(
    geometry_by_id = geometry_by_id,
    selector_audit = dplyr::bind_rows(selector_audit),
    selected_by_id = selected_by_id,
    source_hole_audit = local_holes,
    outlet_audit = dplyr::bind_rows(outlet_audit),
    future_audit = future_audit,
    huc_benchmark = dplyr::bind_rows(huc_benchmark),
    huc_candidates = huc_candidates,
    huc_sources = huc_sources
  )
}

open_qa_png <- function(path, width, height) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(
    filename = path,
    width = width,
    height = height,
    res = 150,
    bg = "white"
  )
}

render_phase_b2_maps <- function(baseline, display, phase_b2, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    glda3 = file.path(output_dir, "glda3_cbrfc_modeled_upstream_comparison.png"),
    lksa3 = file.path(output_dir, "lksa3_local_intervening_comparison.png"),
    huc2 = file.path(output_dir, "huc2_14_15_simplification_comparison.png"),
    forecast = file.path(output_dir, "combined_california_colorado_forecast_geometry.png"),
    context = file.path(output_dir, "combined_forecast_plus_huc2_context.png")
  )

  for (geometry_id in CBRFC_OPERATIONAL_IDS) {
    path <- if (geometry_id == CBRFC_OPERATIONAL_IDS[[1]]) paths[["glda3"]] else paths[["lksa3"]]
    source <- baseline[baseline$geometry_id == geometry_id, , drop = FALSE]
    final <- sf::st_transform(
      display[display$geometry_id == geometry_id, , drop = FALSE], 3310
    )
    open_qa_png(path, 1800, 900)
    graphics::par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
    plot(sf::st_geometry(source), col = "#4C78A8AA", border = "#1F3552", main = "Unsimplified source union")
    plot(sf::st_geometry(final), col = "#F28E2BAA", border = "#7A3E00", main = "Reviewed display geometry")
    grDevices::dev.off()
  }

  open_qa_png(paths[["huc2"]], 2200, 1200)
  graphics::par(mfrow = c(2, 4), mar = c(1, 1, 3, 1))
  for (geometry_id in HUC2_CONTEXT_IDS) {
    for (keep in c(0.005, 0.01, 0.02, 0.05)) {
      key <- format(keep, nsmall = 3, trim = TRUE)
      candidate <- phase_b2$huc_candidates[[geometry_id]][[key]]
      plot(
        sf::st_geometry(candidate),
        col = "#B8DEB8", border = "#27632A",
        main = paste(sub("_CONTEXT$", "", geometry_id), "keep=", keep)
      )
    }
  }
  grDevices::dev.off()

  mapped_ids <- setdiff(
    unique(readr::read_csv(
      PRODUCT_MAPPING_PATH,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )$geometry_id),
    "BDBC1_FNF"
  )
  mapped <- display[display$geometry_id %in% mapped_ids, , drop = FALSE]
  mapped <- mapped[order(mapped$display_order), , drop = FALSE]
  colors <- ifelse(mapped$rfc == "CBRFC", "#E15759AA", "#4E79A7AA")
  open_qa_png(paths[["forecast"]], 1800, 1200)
  plot(sf::st_geometry(mapped), col = colors, border = "#333333", lwd = 0.5, axes = FALSE)
  graphics::title("California and Colorado product-mapped forecast geometry")
  graphics::legend(
    "bottomleft", legend = c("CNRFC", "CBRFC"),
    fill = c("#4E79A7AA", "#E15759AA"), bty = "n"
  )
  grDevices::dev.off()

  context <- display[display$geometry_id %in% HUC2_CONTEXT_IDS, , drop = FALSE]
  open_qa_png(paths[["context"]], 1800, 1200)
  plot(sf::st_geometry(context), col = "#D9D9D955", border = "#666666", lwd = 1, axes = FALSE)
  plot(sf::st_geometry(mapped), col = colors, border = "#333333", lwd = 0.5, add = TRUE)
  graphics::title("Product-mapped forecasts with HUC2 14/15 context")
  grDevices::dev.off()

  assert_true(
    all(file.exists(paths)) && all(file.info(paths)$size > 1000),
    "One or more rendered Phase B2 QA maps were not created."
  )
  paths
}

validate_source_sf <- function(x, required_fields, label) {
  assert_true(inherits(x, "sf"), paste0(label, " must be an sf object."))
  assert_required_columns(x, required_fields, label)
  assert_true(!is.na(sf::st_crs(x)), paste0(label, " has no CRS."))
  assert_true(nrow(x) > 0L, paste0(label, " is empty."))
  invisible(TRUE)
}

normalize_polygon_feature <- function(x, label) {
  x <- make_valid_if_needed(x)
  assert_true(nrow(x) == 1L, paste0(label, " must contain exactly one feature."))
  assert_true(!sf::st_is_empty(x)[[1]], paste0(label, " geometry is empty."))

  geometry_type <- as.character(sf::st_geometry_type(x, by_geometry = TRUE))
  if (geometry_type == "GEOMETRYCOLLECTION") {
    geometry <- suppressWarnings(
      sf::st_collection_extract(sf::st_geometry(x), "POLYGON")
    )
    x <- sf::st_sf(sf::st_drop_geometry(x), geometry = geometry)
    geometry_type <- as.character(sf::st_geometry_type(x, by_geometry = TRUE))
  }
  assert_true(
    geometry_type %in% c("POLYGON", "MULTIPOLYGON"),
    paste0(label, " must resolve to polygonal geometry, not ", geometry_type, ".")
  )
  x
}

select_source_component <- function(
  manifest_row,
  fnf_3310,
  model_3310,
  derived_3310
) {
  source_object <- manifest_row$source_object[[1]]
  source_role <- manifest_row$component_source_role[[1]]
  id_field <- manifest_row$source_id_field[[1]]
  id_value <- manifest_row$source_id_value[[1]]
  component_id <- manifest_row$component_geometry_id[[1]]

  if (source_object == FNF_SOURCE_OBJECT) {
    assert_true(
      source_role == "fnf_forecast_watershed" && id_field == "nws5id",
      paste0("Ambiguous assembled-FNF selection contract for ", component_id, ".")
    )
    source <- fnf_3310
  } else if (source_object == MODEL_SOURCE_OBJECT) {
    assert_true(
      source_role == "cnrfc_model_subbasin" && id_field == "Basin",
      paste0("Ambiguous model-subbasin selection contract for ", component_id, ".")
    )
    source <- model_3310
  } else if (source_object == DERIVED_SOURCE_OBJECT) {
    assert_true(
      id_field == "geometry_id",
      paste0("Ambiguous derived-geometry selection contract for ", component_id, ".")
    )
    source <- derived_3310
  } else {
    stop("Unapproved source object for ", component_id, ": ", source_object)
  }

  assert_required_columns(source, id_field, paste0("Source for ", component_id))
  matches <- which(as.character(source[[id_field]]) == id_value)
  assert_true(
    length(matches) == 1L,
    paste0(
      component_id, " must resolve exactly once using source_object=",
      source_object, ", role=", source_role, ", ", id_field, "=", id_value,
      "; found ", length(matches), "."
    )
  )

  selected <- normalize_polygon_feature(
    source[matches, , drop = FALSE],
    component_id
  )
  list(
    geometry = sf::st_geometry(selected)[[1]],
    match_count = length(matches),
    geometry_type = as.character(sf::st_geometry_type(selected)[[1]])
  )
}

dissolve_components <- function(component_geometries, derived_id) {
  component_sf <- sf::st_sf(
    geometry = sf::st_sfc(component_geometries, crs = 3310)
  )
  component_sf <- make_valid_if_needed(component_sf)
  dissolved <- sf::st_union(sf::st_geometry(component_sf))
  dissolved <- sf::st_make_valid(dissolved)

  if (any(sf::st_geometry_type(dissolved) == "GEOMETRYCOLLECTION")) {
    dissolved <- suppressWarnings(
      sf::st_collection_extract(dissolved, "POLYGON")
    )
    dissolved <- sf::st_union(dissolved)
  }

  output <- sf::st_sf(geometry_id = derived_id, geometry = dissolved)
  output <- normalize_polygon_feature(output, derived_id)
  sf::st_geometry(output)[[1]]
}

build_major_water_supply_basin_geometry <- function() {
  message(
    "Building major water-supply basin geometry (",
    MAJOR_BASIN_GEOMETRY_BUILD_VERSION,
    ")"
  )

  required_inputs <- c(
    CATALOG_PATH,
    MANIFEST_PATH,
    SOURCE_MANIFEST_PATH,
    CBRFC_SELECTOR_PATH,
    PRODUCT_MAPPING_PATH,
    PHASE_B1_HASH_PATH,
    file.path(DIR$raw, "cbrfc_forecast_geometry", "CBRFC_Basins.shp"),
    file.path(DIR$raw, "cbrfc_forecast_geometry", "CBRFC_Outlets.zip"),
    file.path(DIR$raw, "co_riv", "WBD_14_HU2_Shape.zip"),
    file.path(DIR$raw, "co_riv", "WBD_15_HU2_Shape.zip"),
    FNF_FULL_PATH,
    MODEL_FULL_PATH
  )
  missing_inputs <- required_inputs[!file.exists(required_inputs)]
  assert_true(
    length(missing_inputs) == 0L,
    paste0("Missing required input(s):\n  ", paste(missing_inputs, collapse = "\n  "))
  )

  hash_paths <- c(
    geometry_catalog = CATALOG_PATH,
    component_manifest = MANIFEST_PATH,
    source_manifest = SOURCE_MANIFEST_PATH,
    cbrfc_selector_manifest = CBRFC_SELECTOR_PATH,
    product_mapping = PRODUCT_MAPPING_PATH,
    phase_b1_geometry_hashes = PHASE_B1_HASH_PATH,
    cbrfc_basins_shp = file.path(DIR$raw, "cbrfc_forecast_geometry", "CBRFC_Basins.shp"),
    cbrfc_basins_shx = file.path(DIR$raw, "cbrfc_forecast_geometry", "CBRFC_Basins.shx"),
    cbrfc_basins_dbf = file.path(DIR$raw, "cbrfc_forecast_geometry", "CBRFC_Basins.dbf"),
    cbrfc_basins_prj = file.path(DIR$raw, "cbrfc_forecast_geometry", "CBRFC_Basins.prj"),
    cbrfc_basins_qpj = file.path(DIR$raw, "cbrfc_forecast_geometry", "CBRFC_Basins.qpj"),
    cbrfc_outlets_zip = file.path(DIR$raw, "cbrfc_forecast_geometry", "CBRFC_Outlets.zip"),
    wbd_huc2_14_zip = file.path(DIR$raw, "co_riv", "WBD_14_HU2_Shape.zip"),
    wbd_huc2_15_zip = file.path(DIR$raw, "co_riv", "WBD_15_HU2_Shape.zip"),
    cnrfc_fnf_delta_wgs84 = FNF_FULL_PATH,
    cnrfc_basins_wgs84 = MODEL_FULL_PATH
  )
  if (file.exists(FNF_MAP_PATH)) {
    hash_paths <- c(hash_paths, cnrfc_fnf_delta_map = FNF_MAP_PATH)
  }
  input_hashes_before <- vapply(hash_paths, sha256_file, character(1))

  catalog <- read_and_validate_catalog()
  manifest <- read_and_validate_manifest(catalog)
  source_manifest <- read_and_validate_source_manifest()
  cbrfc_selectors <- read_and_validate_cbrfc_selectors()
  fnf_full <- readRDS(FNF_FULL_PATH)
  model_full <- readRDS(MODEL_FULL_PATH)

  validate_source_sf(fnf_full, c("nws5id", "River", "res"), "Assembled FNF source")
  validate_source_sf(model_full, "Basin", "Detailed CNRFC model-subbasin source")
  assert_true(
    all(sf::st_is_valid(fnf_full)) && !any(sf::st_is_empty(fnf_full)),
    paste0(
      "Authoritative assembled FNF source geometry must already be valid and ",
      "non-empty; it is not repaired or cleaned in this pipeline."
    )
  )
  assert_true(
    nrow(fnf_full) == 15L && length(unique(as.character(fnf_full$nws5id))) == 15L,
    "Assembled FNF source must contain exactly 15 unique forecast-watershed LIDs."
  )
  assert_true(
    setequal(paste0(as.character(fnf_full$nws5id), "_FNF"), EXPECTED_ORIGINAL_IDS),
    "Assembled FNF source LIDs do not match the reviewed 15-feature catalog."
  )

  original_catalog <- catalog[catalog$geometry_id %in% EXPECTED_ORIGINAL_IDS, ]
  original_matches <- vapply(
    original_catalog$source_id_value,
    function(id_value) sum(as.character(fnf_full$nws5id) == id_value),
    integer(1)
  )
  assert_true(
    all(original_matches == 1L),
    "Every cataloged original FNF LID must occur exactly once in the retained source."
  )
  for (row_index in seq_len(nrow(original_catalog))) {
    source_index <- which(
      as.character(fnf_full$nws5id) ==
        original_catalog$source_id_value[[row_index]]
    )
    assert_true(
      identical(
        original_catalog$river_name[[row_index]],
        as.character(fnf_full$River[[source_index]])
      ) &&
      (
        (
          original_catalog$geometry_id[[row_index]] == "MHBC1_FNF" &&
            original_catalog$reservoir_name[[row_index]] == ""
        ) || identical(
          original_catalog$reservoir_name[[row_index]],
          as.character(fnf_full$res[[source_index]])
        )
      ),
      paste0(
        "Catalog river/reservoir names do not match the authoritative FNF ",
        "source for ",
        original_catalog$geometry_id[[row_index]],
        "."
      )
    )
  }

  model_manifest <- manifest[manifest$source_object == MODEL_SOURCE_OBJECT, ]
  model_matches <- vapply(
    model_manifest$source_id_value,
    function(id_value) sum(as.character(model_full$Basin) == id_value),
    integer(1)
  )
  assert_true(
    all(model_matches == 1L),
    "Every required detailed model component must occur exactly once."
  )

  fnf_3310 <- sf::st_transform(fnf_full, 3310)
  model_3310 <- to_ca_albers(model_full)
  validate_source_sf(fnf_3310, "nws5id", "EPSG:3310 assembled FNF source")
  validate_source_sf(model_3310, "Basin", "EPSG:3310 detailed model source")
  assert_true(
    identical(as.integer(sf::st_crs(fnf_3310)$epsg), 3310L) &&
      identical(as.integer(sf::st_crs(model_3310)$epsg), 3310L),
    "Processing sources must be EPSG:3310."
  )

  derived_3310 <- sf::st_sf(
    geometry_id = character(),
    geometry = sf::st_sfc(crs = 3310)
  )
  component_resolution <- list()
  cleanup_metrics <- list()
  resolution_index <- 0L

  for (derived_id in DERIVED_BUILD_ORDER) {
    recipe <- manifest[manifest$derived_geometry_id == derived_id, , drop = FALSE]
    recipe <- recipe[order(recipe$component_order), , drop = FALSE]
    components <- vector("list", nrow(recipe))

    for (row_index in seq_len(nrow(recipe))) {
      resolution <- select_source_component(
        recipe[row_index, , drop = FALSE],
        fnf_3310,
        model_3310,
        derived_3310
      )
      components[[row_index]] <- resolution$geometry
      resolution_index <- resolution_index + 1L
      component_resolution[[resolution_index]] <- data.frame(
        derived_geometry_id = derived_id,
        component_order = recipe$component_order[[row_index]],
        component_geometry_id = recipe$component_geometry_id[[row_index]],
        component_source_role = recipe$component_source_role[[row_index]],
        source_object = recipe$source_object[[row_index]],
        source_id_field = recipe$source_id_field[[row_index]],
        source_id_value = recipe$source_id_value[[row_index]],
        match_count = resolution$match_count,
        source_geometry_type = resolution$geometry_type,
        stringsAsFactors = FALSE
      )
    }

    dissolved <- dissolve_components(components, derived_id)
    cleaned <- cleanup_derived_geometry(dissolved, derived_id)
    cleanup_metrics[[derived_id]] <- cleaned$metrics
    next_feature <- sf::st_sf(
      geometry_id = derived_id,
      geometry = sf::st_sfc(cleaned$geometry, crs = 3310)
    )
    derived_3310 <- rbind(derived_3310, next_feature)
  }
  derived_cleanup_metrics <- dplyr::bind_rows(cleanup_metrics)
  assert_true(
    nrow(derived_cleanup_metrics) == 4L &&
      setequal(derived_cleanup_metrics$geometry_id, DERIVED_BUILD_ORDER),
    "Cleanup metrics must contain exactly the four derived geometries."
  )

  original_geometry_by_id <- setNames(
    lapply(EXPECTED_ORIGINAL_IDS, function(geometry_id) {
      lid <- sub("_FNF$", "", geometry_id)
      matches <- which(as.character(fnf_3310$nws5id) == lid)
      assert_true(
        length(matches) == 1L,
        paste0("Original geometry must resolve exactly once: ", geometry_id)
      )
      selected <- normalize_polygon_feature(
        fnf_3310[matches, , drop = FALSE],
        geometry_id
      )
      sf::st_geometry(selected)[[1]]
    }),
    EXPECTED_ORIGINAL_IDS
  )
  derived_geometry_by_id <- setNames(
    lapply(seq_len(nrow(derived_3310)), function(index) {
      sf::st_geometry(derived_3310)[[index]]
    }),
    derived_3310$geometry_id
  )
  phase_b2 <- build_phase_b2_geometry(source_manifest, cbrfc_selectors)
  baseline_geometry_by_id <- c(
    original_geometry_by_id,
    derived_geometry_by_id,
    phase_b2$geometry_by_id
  )
  baseline_geometry <- baseline_geometry_by_id[catalog$geometry_id]
  assert_true(
    all(vapply(baseline_geometry, inherits, logical(1), what = "sfg")),
    "Could not resolve all catalog geometry for the baseline output."
  )
  baseline <- sf::st_sf(
    catalog,
    geometry = sf::st_sfc(baseline_geometry, crs = 3310)
  )
  baseline <- make_valid_if_needed(baseline)

  display_geometry_by_id <- setNames(vector("list", nrow(catalog)), catalog$geometry_id)
  final_display_3310_by_id <- setNames(
    vector("list", nrow(catalog)),
    catalog$geometry_id
  )
  simplified_stage_metrics <- list()
  post_simplify_metrics <- list()
  post_simplify_artifacts <- list()

  for (index in seq_len(nrow(catalog))) {
    geometry_id <- catalog$geometry_id[[index]]
    keep <- catalog$display_simplify_keep[[index]]
    feature <- baseline[index, , drop = FALSE]
    simplification <- simplify_once_for_display(
      feature,
      keep = keep,
      layer_label = geometry_id
    )
    simplified <- simplification$geometry
    simplification_area_change_pct <- 100 *
      (area_square_miles(simplified) - area_square_miles(feature)) /
      area_square_miles(feature)
    if (geometry_id %in% CBRFC_OPERATIONAL_IDS) {
      assert_true(
        abs(simplification_area_change_pct) <=
          CBRFC_DISPLAY_AREA_CHANGE_GUARDRAIL_PCT,
        paste0(
          geometry_id, " display simplification changed area by ",
          format(round(simplification_area_change_pct, 8), trim = TRUE),
          "%, exceeding the 0.05% guardrail."
        )
      )
    }
    simplified_stage_metrics[[geometry_id]] <- data.frame(
      geometry_id = geometry_id,
      simplified_pre_normalization_area_sq_mi = area_square_miles(simplified),
      simplified_pre_normalization_vertex_count = count_vertices(simplified),
      simplified_pre_normalization_part_count = count_polygon_parts(simplified),
      simplified_pre_normalization_hole_count = count_polygon_holes(simplified),
      simplify_raw_geometry_type = simplification$raw_geometry_type,
      simplify_raw_valid = simplification$raw_valid,
      simplify_validity_repair_applied =
        simplification$validity_repair_applied,
      simplify_valid_geometry_type = simplification$valid_geometry_type,
      stringsAsFactors = FALSE
    )

    if (geometry_id %in% DERIVED_BUILD_ORDER) {
      normalized <- normalize_derived_display_geometry(simplified, geometry_id)
      final_3310 <- normalized$geometry
      post_simplify_metrics[[geometry_id]] <- normalized$metrics
      if (nrow(normalized$artifacts) > 0L) {
        post_simplify_artifacts[[geometry_id]] <- normalized$artifacts
      }
    } else {
      if (geometry_id == "LKSA3_CBRFC_LOCAL_INTERVENING") {
        assert_true(
          count_polygon_holes(feature) == 1L &&
            count_polygon_holes(simplified) == 0L &&
            count_polygon_parts(feature) == count_polygon_parts(simplified),
          paste0(
            geometry_id,
            " must preserve one source gap, lose it only through reviewed ",
            "keep=0.05 simplification, and retain all polygon parts."
          )
        )
      }
      final_3310 <- simplified
      post_simplify_metrics[[geometry_id]] <- data.frame(
        geometry_id = geometry_id,
        post_simplify_normalization_applied = FALSE,
        post_simplify_holes_before = NA_integer_,
        post_simplify_holes_after = NA_integer_,
        post_simplify_parts_before = NA_integer_,
        post_simplify_parts_after = NA_integer_,
        post_simplify_detached_parts_removed = 0L,
        post_simplify_detached_area_removed_sq_mi = 0,
        post_simplify_largest_removed_part_sq_mi = 0,
        post_simplify_hole_area_added_sq_mi = 0,
        post_simplify_normalization_area_change_sq_mi = 0,
        post_simplify_normalization_area_change_pct = 0,
        post_simplify_vertices_before = NA_integer_,
        post_simplify_vertices_after = NA_integer_,
        post_simplify_part_area_threshold_sq_mi = NA_real_,
        post_simplify_area_guardrail_pct = NA_real_,
        stringsAsFactors = FALSE
      )
    }

    final_display_3310_by_id[[geometry_id]] <- sf::st_geometry(final_3310)[[1]]
    final_wgs84 <- to_wgs84(final_3310)
    assert_true(
      nrow(final_wgs84) == 1L &&
        identical(as.integer(sf::st_crs(final_wgs84)$epsg), 4326L),
      paste0(
        "Display geometry did not resolve to one EPSG:4326 feature: ",
        geometry_id
      )
    )
    display_geometry_by_id[[geometry_id]] <- sf::st_geometry(final_wgs84)[[1]]
  }

  simplified_stage_metrics <- dplyr::bind_rows(simplified_stage_metrics)
  all_post_simplify_metrics <- dplyr::bind_rows(post_simplify_metrics)
  post_simplify_artifacts <- dplyr::bind_rows(post_simplify_artifacts)
  display_hole_exception_metrics <- data.frame(
    geometry_id = catalog$geometry_id,
    display_hole_exception_applied =
      catalog$geometry_id == "LKSA3_CBRFC_LOCAL_INTERVENING",
    source_hole_count = vapply(seq_len(nrow(baseline)), function(index) {
      count_polygon_holes(baseline[index, , drop = FALSE])
    }, integer(1)),
    display_hole_count = vapply(catalog$geometry_id, function(geometry_id) {
      feature <- sf::st_sf(
        geometry_id = geometry_id,
        geometry = sf::st_sfc(final_display_3310_by_id[[geometry_id]], crs = 3310)
      )
      count_polygon_holes(feature)
    }, integer(1)),
    approved_source_hole_area_sq_mi = ifelse(
      catalog$geometry_id == "LKSA3_CBRFC_LOCAL_INTERVENING",
      phase_b2$source_hole_audit$area_sq_mi[[1]],
      0
    ),
    display_hole_exception_policy = ifelse(
      catalog$geometry_id == "LKSA3_CBRFC_LOCAL_INTERVENING",
      "keep_0.05_simplification_fills_reviewed_source_gap_no_explicit_fill_operation",
      "none"
    ),
    stringsAsFactors = FALSE
  )
  display <- sf::st_sf(
    catalog,
    geometry = sf::st_sfc(
      display_geometry_by_id[catalog$geometry_id],
      crs = 4326
    )
  )
  baseline_valid <- sf::st_is_valid(baseline)
  display_valid <- sf::st_is_valid(display)
  assert_true(
    all(baseline_valid) && all(display_valid) &&
      !any(sf::st_is_empty(baseline)) && !any(sf::st_is_empty(display)),
    "Final geometry outputs must be valid and non-empty."
  )

  build_timestamp <- make_timestamp()
  metrics <- lapply(seq_len(nrow(catalog)), function(index) {
    geometry_id <- catalog$geometry_id[[index]]
    baseline_feature <- baseline[index, , drop = FALSE]
    display_feature <- display[index, , drop = FALSE]
    display_3310 <- sf::st_sf(
      geometry_id = geometry_id,
      geometry = sf::st_sfc(
        final_display_3310_by_id[[geometry_id]],
        crs = 3310
      )
    )
    simplified_metric <- simplified_stage_metrics[
      simplified_stage_metrics$geometry_id == geometry_id,
      ,
      drop = FALSE
    ]
    baseline_area <- area_square_miles(baseline_feature)
    simplified_area <-
      simplified_metric$simplified_pre_normalization_area_sq_mi[[1]]
    display_area <- area_square_miles(display_3310)
    baseline_parts <- count_polygon_parts(baseline_feature)
    display_parts <- count_polygon_parts(display_feature)

    data.frame(
      geometry_id = geometry_id,
      build_version = MAJOR_BASIN_GEOMETRY_BUILD_VERSION,
      build_timestamp = build_timestamp,
      processing_crs = 3310L,
      source_processing_crs = if (
        geometry_id %in% CBRFC_OPERATIONAL_IDS
      ) 5070L else 3310L,
      display_crs = 4326L,
      simplification_keep = catalog$display_simplify_keep[[index]],
      cleanup_policy = catalog$cleanup_policy[[index]],
      baseline_area_sq_mi = baseline_area,
      simplified_pre_normalization_area_sq_mi = simplified_area,
      display_area_sq_mi = display_area,
      simplification_area_change_pct =
        100 * (simplified_area - baseline_area) / baseline_area,
      final_display_total_area_change_pct =
        100 * (display_area - baseline_area) / baseline_area,
      baseline_vertex_count = count_vertices(baseline_feature),
      display_vertex_count = count_vertices(display_feature),
      baseline_serialized_bytes = geometry_serialized_bytes(baseline_feature),
      display_serialized_bytes = geometry_serialized_bytes(display_feature),
      baseline_part_count = baseline_parts,
      display_part_count = display_parts,
      multipart_changed = baseline_parts != display_parts,
      display_geometry_source = if (
        geometry_id %in% DERIVED_BUILD_ORDER
      ) {
        paste0(
          "cleaned_derived_union_ms_simplify_keep_0.10_",
          "post_simplify_normalized"
        )
      } else if (geometry_id %in% EXPECTED_ORIGINAL_IDS) {
        "authoritative_cnrfc_fnf_delta_wgs84_ms_simplify_keep_0.20"
      } else if (geometry_id %in% CBRFC_OPERATIONAL_IDS) {
        "official_cbrfc_basins_union_epsg5070_ms_simplify_keep_0.05"
      } else {
        "original_usgs_wbd_huc2_archive_ms_simplify_keep_0.01"
      },
      display_source_geometry_id = geometry_id,
      component_count = if (
        geometry_id %in% DERIVED_BUILD_ORDER
      ) length(EXPECTED_COMPONENTS[[geometry_id]]) else NA_integer_,
      stringsAsFactors = FALSE
    )
  })
  metrics <- dplyr::bind_rows(metrics)
  metrics <- dplyr::left_join(
    metrics,
    dplyr::select(
      simplified_stage_metrics,
      -simplified_pre_normalization_area_sq_mi
    ),
    by = "geometry_id"
  )
  metrics <- dplyr::left_join(
    metrics,
    all_post_simplify_metrics,
    by = "geometry_id"
  )
  metrics <- dplyr::left_join(
    metrics,
    display_hole_exception_metrics,
    by = "geometry_id"
  )
  original_cleanup_metrics <- data.frame(
    geometry_id = setdiff(catalog$geometry_id, DERIVED_BUILD_ORDER),
    cleanup_applied = FALSE,
    cleanup_holes_before = NA_integer_,
    cleanup_holes_after = NA_integer_,
    cleanup_part_count_before = NA_integer_,
    cleanup_part_count_after = NA_integer_,
    cleanup_detached_parts_removed = 0L,
    cleanup_detached_area_removed_sq_mi = 0,
    cleanup_largest_removed_part_sq_mi = 0,
    cleanup_area_added_holes_sq_mi = 0,
    cleanup_total_area_change_sq_mi = 0,
    cleanup_total_area_change_pct = 0,
    cleanup_vertex_count_before = NA_integer_,
    cleanup_vertex_count_after = NA_integer_,
    cleanup_part_area_threshold_sq_mi = NA_real_,
    cleanup_area_guardrail_pct = NA_real_,
    stringsAsFactors = FALSE
  )
  all_cleanup_metrics <- dplyr::bind_rows(
    original_cleanup_metrics,
    derived_cleanup_metrics
  )
  metrics <- dplyr::left_join(
    metrics,
    all_cleanup_metrics,
    by = "geometry_id"
  )
  assert_true(
    nrow(metrics) == 23L && !anyNA(metrics$cleanup_applied) &&
      !anyNA(metrics$post_simplify_normalization_applied),
    paste0(
      "Cleanup and post-simplification provenance must resolve for all 23 ",
      "geometry rows."
    )
  )

  provenance <- data.frame(
    input_name = names(hash_paths),
    input_path = unname(hash_paths),
    sha256 = unname(input_hashes_before),
    build_version = MAJOR_BASIN_GEOMETRY_BUILD_VERSION,
    build_timestamp = build_timestamp,
    stringsAsFactors = FALSE
  )
  provenance_hashes <- paste(
    paste(provenance$input_name, provenance$sha256, sep = "="),
    collapse = ";"
  )

  metric_fields <- setdiff(names(metrics), "geometry_id")
  for (field in metric_fields) {
    baseline[[field]] <- metrics[[field]]
    display[[field]] <- metrics[[field]]
  }
  baseline$input_sha256 <- provenance_hashes
  display$input_sha256 <- provenance_hashes

  component_resolution <- dplyr::bind_rows(component_resolution)
  geometry_qa <- metrics
  geometry_qa$input_sha256 <- provenance_hashes
  product_mapping <- read_character_config(
    PRODUCT_MAPPING_PATH,
    c(
      "schema_version", "forecast_key", "source_family", "product_type",
      "product_family", "forecast_period", "geometry_id",
      "view_applicability", "measure_applicability", "default_metric",
      "allowed_popup_metrics", "source_link_authority", "source_link_roles",
      "display_order", "decision_note"
    ),
    "product mapping"
  )
  product_mapping$display_order <- suppressWarnings(as.integer(product_mapping$display_order))
  assert_true(
    nrow(product_mapping) == 54L && !anyDuplicated(product_mapping$forecast_key) &&
      !anyNA(product_mapping$display_order) &&
      all(product_mapping$geometry_id %in% catalog$geometry_id) &&
      length(unique(product_mapping$geometry_id)) == 20L,
    "Product mapping must contain 54 unique literal keys mapped to 20 retained geometries."
  )
  product_mapping_audit <- product_mapping
  product_mapping_audit$geometry_exists <-
    product_mapping_audit$geometry_id %in% catalog$geometry_id
  product_mapping_audit$forecast_capable <-
    !product_mapping_audit$geometry_id %in% HUC2_CONTEXT_IDS
  product_mapping_audit$browser_authority <-
    "major_water_supply_basin_product_mapping.csv"

  geometry_inventory <- sf::st_drop_geometry(baseline)[, c(
    "geometry_id", "geometry_role", "view_group", "include_in_display",
    "display_order", "display_simplify_keep", "cleanup_policy"
  )]
  geometry_inventory$product_mapped <-
    geometry_inventory$geometry_id %in% product_mapping$geometry_id
  geometry_inventory$inventory_class <- ifelse(
    geometry_inventory$geometry_role == "context_only",
    "context_only",
    ifelse(geometry_inventory$product_mapped, "product_mapped", "internal_unmapped")
  )

  feature_hash_audit <- dplyr::bind_rows(lapply(seq_len(nrow(catalog)), function(index) {
    data.frame(
      order = index,
      geometry_id = catalog$geometry_id[[index]],
      baseline_sha256 = geometry_sha256(baseline[index, , drop = FALSE]),
      display_sha256 = geometry_sha256(display[index, , drop = FALSE]),
      stringsAsFactors = FALSE
    )
  }))
  phase_b1_hashes <- read_character_config(
    PHASE_B1_HASH_PATH,
    c("order", "geometry_id", "baseline_sha256", "display_sha256"),
    "Phase B1 geometry hash baseline"
  )
  current_b1 <- feature_hash_audit[
    feature_hash_audit$geometry_id %in% phase_b1_hashes$geometry_id,
    , drop = FALSE
  ]
  current_b1 <- current_b1[match(phase_b1_hashes$geometry_id, current_b1$geometry_id), ]
  assert_true(
    identical(current_b1$geometry_id, phase_b1_hashes$geometry_id) &&
      identical(current_b1$baseline_sha256, phase_b1_hashes$baseline_sha256) &&
      identical(current_b1$display_sha256, phase_b1_hashes$display_sha256),
    "A Phase B1 California authoritative or display geometry hash changed."
  )
  feature_hash_audit$phase_b1_hash_preserved <- ifelse(
    feature_hash_audit$geometry_id %in% phase_b1_hashes$geometry_id,
    TRUE,
    NA
  )

  hole_part_audit <- geometry_qa[, c(
    "geometry_id", "baseline_part_count", "display_part_count",
    "source_hole_count", "display_hole_count",
    "approved_source_hole_area_sq_mi", "display_hole_exception_applied",
    "display_hole_exception_policy", "simplification_area_change_pct"
  )]

  input_hashes_before_write <- vapply(hash_paths, sha256_file, character(1))
  assert_true(
    identical(input_hashes_before, input_hashes_before_write),
    "An authoritative input changed while geometry was being assembled."
  )

  qa_paths <- list(
    geometry_latest = file.path(
      DIR$qa,
      "major_water_supply_basin_geometry_qa_latest.csv"
    ),
    resolution_latest = file.path(
      DIR$qa,
      "major_water_supply_basin_component_resolution_latest.csv"
    ),
    provenance_latest = file.path(
      DIR$qa,
      "major_water_supply_basin_input_provenance_latest.csv"
    ),
    post_simplify_artifacts_latest = file.path(
      DIR$qa,
      "major_water_supply_basin_post_simplify_artifacts_latest.csv"
    ),
    inventory_latest = file.path(
      DIR$qa, "major_water_supply_basin_geometry_inventory_latest.csv"
    ),
    mapping_latest = file.path(
      DIR$qa, "major_water_supply_basin_product_mapping_audit_latest.csv"
    ),
    source_manifest_latest = file.path(
      DIR$qa, "major_water_supply_basin_source_manifest_latest.csv"
    ),
    selector_latest = file.path(
      DIR$qa, "major_water_supply_basin_cbrfc_selector_audit_latest.csv"
    ),
    feature_hash_latest = file.path(
      DIR$qa, "major_water_supply_basin_per_feature_hash_audit_latest.csv"
    ),
    hole_part_latest = file.path(
      DIR$qa, "major_water_supply_basin_hole_part_audit_latest.csv"
    ),
    outlet_latest = file.path(
      DIR$qa, "major_water_supply_basin_outlet_proximity_audit_latest.csv"
    ),
    huc_benchmark_latest = file.path(
      DIR$qa, "major_water_supply_basin_huc2_simplification_benchmark_latest.csv"
    ),
    future_total_latest = file.path(
      DIR$qa, "major_water_supply_basin_future_total_mead_candidate_latest.csv"
    ),
    geometry_timestamped = file.path(
      DIR$qa,
      paste0("major_water_supply_basin_geometry_qa_", build_timestamp, ".csv")
    ),
    resolution_timestamped = file.path(
      DIR$qa,
      paste0(
        "major_water_supply_basin_component_resolution_",
        build_timestamp,
        ".csv"
      )
    ),
    provenance_timestamped = file.path(
      DIR$qa,
      paste0(
        "major_water_supply_basin_input_provenance_",
        build_timestamp,
        ".csv"
      )
    ),
    post_simplify_artifacts_timestamped = file.path(
      DIR$qa,
      paste0(
        "major_water_supply_basin_post_simplify_artifacts_",
        build_timestamp,
        ".csv"
      )
    )
  )

  atomic_save_rds(baseline, OUTPUT_3310_PATH)
  atomic_save_rds(display, OUTPUT_MAP_PATH)
  atomic_write_csv(geometry_qa, qa_paths$geometry_latest)
  atomic_write_csv(component_resolution, qa_paths$resolution_latest)
  atomic_write_csv(provenance, qa_paths$provenance_latest)
  atomic_write_csv(
    post_simplify_artifacts,
    qa_paths$post_simplify_artifacts_latest
  )
  atomic_write_csv(geometry_inventory, qa_paths$inventory_latest)
  atomic_write_csv(product_mapping_audit, qa_paths$mapping_latest)
  atomic_write_csv(source_manifest, qa_paths$source_manifest_latest)
  atomic_write_csv(phase_b2$selector_audit, qa_paths$selector_latest)
  atomic_write_csv(feature_hash_audit, qa_paths$feature_hash_latest)
  atomic_write_csv(hole_part_audit, qa_paths$hole_part_latest)
  atomic_write_csv(phase_b2$outlet_audit, qa_paths$outlet_latest)
  atomic_write_csv(phase_b2$huc_benchmark, qa_paths$huc_benchmark_latest)
  atomic_write_csv(phase_b2$future_audit, qa_paths$future_total_latest)
  atomic_write_csv(geometry_qa, qa_paths$geometry_timestamped)
  atomic_write_csv(provenance, qa_paths$provenance_timestamped)
  atomic_write_csv(
    post_simplify_artifacts,
    qa_paths$post_simplify_artifacts_timestamped
  )
  rendered_maps <- render_phase_b2_maps(
    baseline,
    display,
    phase_b2,
    file.path(DIR$qa, "major_water_supply_basin_geometry_maps")
  )

  input_hashes_after <- vapply(hash_paths, sha256_file, character(1))
  assert_true(
    identical(input_hashes_before, input_hashes_after),
    "An authoritative input hash changed during output writing."
  )

  message("Saved unsimplified EPSG:3310 geometry: ", OUTPUT_3310_PATH)
  message("Saved WGS84 map geometry:             ", OUTPUT_MAP_PATH)
  message("Saved focused QA tables under:        ", DIR$qa)
  invisible(
    list(
      geometry_3310 = baseline,
      geometry_map = display,
      geometry_qa = geometry_qa,
      cleanup_metrics = all_cleanup_metrics,
      post_simplify_metrics = all_post_simplify_metrics,
      post_simplify_artifacts = post_simplify_artifacts,
      component_resolution = component_resolution,
      geometry_inventory = geometry_inventory,
      product_mapping_audit = product_mapping_audit,
      feature_hash_audit = feature_hash_audit,
      selector_audit = phase_b2$selector_audit,
      outlet_audit = phase_b2$outlet_audit,
      huc_benchmark = phase_b2$huc_benchmark,
      future_total_mead_candidate = phase_b2$future_audit,
      rendered_maps = rendered_maps,
      provenance = provenance
    )
  )
}

build_major_water_supply_basin_geometry()
