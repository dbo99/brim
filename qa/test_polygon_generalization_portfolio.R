#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(sf))

read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

expect_error <- function(expression, pattern) {
  error <- tryCatch({
    force(expression)
    NULL
  }, error = function(error) error)
  if (is.null(error) || !grepl(pattern, conditionMessage(error), fixed = TRUE)) {
    stop("Expected error containing: ", pattern, call. = FALSE)
  }
}

source("03_functions/polygon_generalization_helpers.r")
registry <- pt_polygon_generalization_read_registry()
crosswalk <- pt_polygon_generalization_read_crosswalk()
machine_path_pattern <- "(^/|^~|^[A-Za-z]:[/\\\\])"

replacement <- registry$action == "replace_geometry"
retained <- registry$action == "retain_current"
undisclosed <- registry$disclosure_required == "no"
expected_retained <- c(
  "blm_field_office_boundaries", "blm_ca_managed",
  "blm_held_managed_differences", "counties", "water_districts"
)
stopifnot(
  nrow(registry) == 29L,
  sum(replacement) == 24L,
  sum(retained) == 5L,
  identical(registry$layer_id[retained], expected_retained),
  setequal(
    registry$layer_id[undisclosed],
    c("blm_ca_managed", "blm_held_managed_differences")
  ),
  all(registry$action %in% c("replace_geometry", "retain_current")),
  all(nzchar(registry$accepted_method)),
  all(grepl("^[0-9a-f]{64}$", registry$parent_sha256)),
  all(grepl("^[0-9a-f]{64}$", registry$candidate_sha256[replacement])),
  all(nzchar(registry$candidate_bundle_key[replacement])),
  all(!nzchar(registry$candidate_bundle_key[retained])),
  nrow(crosswalk) == 8971L,
  setequal(unique(crosswalk$layer_id), registry$layer_id[replacement]),
  !anyDuplicated(paste(crosswalk$layer_id, crosswalk$study_id, sep = "\r")),
  !anyDuplicated(paste(crosswalk$layer_id, crosswalk$geometry_key, sep = "\r")),
  all(grepl("^[0-9a-f]{64}$", crosswalk$source_row_fingerprint)),
  !any(grepl(machine_path_pattern, unlist(registry), perl = TRUE)),
  !any(grepl(machine_path_pattern, unlist(crosswalk), perl = TRUE))
)

crosswalk_count <- table(crosswalk$layer_id)
stopifnot(all(vapply(registry$layer_id[replacement], function(layer_id) {
  identical(
    unname(as.integer(crosswalk_count[[layer_id]])),
    as.integer(registry$expected_geometry_rows[registry$layer_id == layer_id])
  )
}, logical(1))))

for (layer_id in registry$layer_id[replacement]) {
  rows <- crosswalk[crosswalk$layer_id == layer_id, , drop = FALSE]
  stopifnot(identical(
    rows$study_id,
    sprintf("feature_%05d", seq_len(nrow(rows)))
  ))
  expected_semantic <- registry$expected_semantic_features[
    registry$layer_id == layer_id
  ]
  stopifnot(length(unique(rows$semantic_feature_key)) == expected_semantic)
}

ambiguous <- registry$layer_id[registry$match_strategy == "source_fingerprint"]
stopifnot(setequal(ambiguous, c(
  "gsp_areas", "wsr_corridor_blm", "grazing_allotments",
  "water_districts"
)))
replacement_ambiguous <- intersect(ambiguous, registry$layer_id[replacement])
stopifnot(all(vapply(replacement_ambiguous, function(layer_id) {
  rows <- crosswalk[crosswalk$layer_id == layer_id, , drop = FALSE]
  all(nzchar(rows$baseline_display_fingerprint))
}, logical(1))))

rwqcb <- pt_polygon_generalization_registry_row("rwqcb_regions", registry)
stopifnot(
  identical(rwqcb$parameter_type, "distance_tolerance_m"),
  identical(as.numeric(rwqcb$parameter_value), 100),
  identical(as.integer(rwqcb$expected_geometry_rows), 9L),
  identical(as.integer(rwqcb$expected_semantic_features), 9L),
  identical(as.integer(rwqcb$expected_vertices), 32031L),
  identical(as.integer(rwqcb$expected_browser_geometry_bytes), 1178655L),
  identical(
    rwqcb$candidate_bundle_key,
    "candidates/rwqcb_regions/geos_coverage_100m_boundary_generalized_geometry_only.rds"
  ),
  identical(
    rwqcb$candidate_sha256,
    "b7c263024b9c6cd296b063a9386d589b8d89e4fb4cc937c0e32aea9a37e3850e"
  )
)

manifest <- utils::read.csv(
  "EXTERNAL_DATA_MANIFEST.csv", stringsAsFactors = FALSE, check.names = FALSE
)
manifest_path <- manifest[[1]]
required_manifest_paths <- file.path(
  PT_POLYGON_GENERALIZATION_BUNDLE_RELATIVE,
  registry$candidate_bundle_key[replacement]
)
required_parent_paths <- file.path(
  PT_POLYGON_GENERALIZATION_BUNDLE_RELATIVE,
  unique(registry$source_parent_relative_path[
    startsWith(registry$source_parent_relative_path, "research_provenance/")
  ])
)
stopifnot(
  file.path(
    PT_POLYGON_GENERALIZATION_BUNDLE_RELATIVE,
    "bundle_manifest.csv"
  ) %in% manifest_path,
  all(required_manifest_paths %in% manifest_path),
  all(required_parent_paths %in% manifest_path),
  !any(grepl("rwqcb_regions.*15", manifest_path, ignore.case = TRUE))
)

# ---- Functional geometry-only overlay fixture ------------------------------

fixture_root <- tempfile("polygon_portfolio_fixture_")
dir.create(file.path(
  fixture_root, PT_POLYGON_GENERALIZATION_BUNDLE_RELATIVE, "candidates", "fixture"
), recursive = TRUE)
dir.create(file.path(fixture_root, "parents"), recursive = TRUE)
square <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(matrix(c(
    xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin
  ), ncol = 2, byrow = TRUE)))
}
parent <- sf::st_sf(
  semantic_id = c("A", "B"),
  fill_field = c("red", "blue"),
  selected = c(FALSE, TRUE),
  geometry = sf::st_sfc(square(0, 0, 1, 1), square(2, 0, 3, 1), crs = 4326)
)
prepared <- parent[c(2, 1), ]
candidate <- sf::st_sf(
  study_id = c("feature_00001", "feature_00002"),
  geometry = sf::st_sfc(square(20, 0, 21, 1), square(10, 0, 11, 1), crs = 4326)
)
parent_path <- file.path(fixture_root, "parents", "fixture_parent.rds")
candidate_path <- file.path(
  fixture_root, PT_POLYGON_GENERALIZATION_BUNDLE_RELATIVE,
  "candidates", "fixture", "geometry_only.rds"
)
saveRDS(parent, parent_path)
saveRDS(candidate, candidate_path)
parent_hash <- pt_polygon_generalization_sha256_file(parent_path)
candidate_hash <- pt_polygon_generalization_sha256_file(candidate_path)
fixture_registry <- data.frame(
  portfolio_version = "test",
  layer_id = "fixture",
  action = "replace_geometry",
  parent_artifact_key = "fixture_parent",
  source_parent_relative_path = "parents/fixture_parent.rds",
  parent_component = "",
  parent_sha256 = parent_hash,
  candidate_bundle_key = "candidates/fixture/geometry_only.rds",
  candidate_sha256 = candidate_hash,
  expected_geometry_rows = 2L,
  expected_semantic_features = 2L,
  expected_vertices = pt_polygon_generalization_vertex_count(candidate),
  cache_relative_path = "fixture.rds",
  cache_child = "",
  match_strategy = "business_key",
  match_fields = "semantic_id",
  fingerprint_fields = "semantic_id|fill_field",
  disclosure_required = "yes",
  public_disclosure = paste(
    "Fixture geometry is generalized.",
    "Check authoritative source for boundary-sensitive use."
  ),
  disclosure_ui_owner = "fixture",
  stringsAsFactors = FALSE
)
fixture_crosswalk <- data.frame(
  portfolio_version = "test",
  layer_id = "fixture",
  study_id = candidate$study_id,
  semantic_feature_key = c("B", "A"),
  geometry_key = c("fixture:B", "fixture:A"),
  match_strategy = "business_key",
  match_key = c("B", "A"),
  retained_business_attributes = c("B|blue", "A|red"),
  source_row_fingerprint = rep("unused", 2),
  baseline_display_fingerprint = rep("", 2),
  parent_artifact_key = "fixture_parent",
  parent_sha256 = parent_hash,
  stringsAsFactors = FALSE
)
attributes_before <- sf::st_drop_geometry(prepared)
result <- pt_apply_reviewed_polygon_geometry(
  "fixture", prepared, TRUE, fixture_root,
  registry = fixture_registry, crosswalk = fixture_crosswalk
)
result_with_nonrequired_flag <- pt_apply_reviewed_polygon_geometry(
  "fixture", prepared, FALSE, fixture_root,
  registry = fixture_registry, crosswalk = fixture_crosswalk
)
stopifnot(
  isTRUE(all.equal(
    sf::st_drop_geometry(result), attributes_before,
    check.attributes = FALSE
  )),
  identical(result$semantic_id, c("B", "A")),
  all.equal(
    sf::st_bbox(result),
    sf::st_bbox(sf::st_sf(
      geometry = sf::st_sfc(square(20, 0, 21, 1), square(10, 0, 11, 1), crs = 4326)
    )),
    check.attributes = FALSE
  ) == TRUE
)
stopifnot(
  identical(
    sf::st_as_binary(sf::st_geometry(result_with_nonrequired_flag)),
    sf::st_as_binary(sf::st_geometry(result))
  ),
  isTRUE(all.equal(
    sf::st_drop_geometry(result_with_nonrequired_flag), attributes_before,
    check.attributes = FALSE
  ))
)

retain_registry <- fixture_registry
retain_registry$action <- "retain_current"
retain_registry$candidate_bundle_key <- ""
retain_registry$candidate_sha256 <- ""
retained <- pt_apply_reviewed_polygon_geometry(
  "fixture", prepared, FALSE, fixture_root,
  registry = retain_registry, crosswalk = fixture_crosswalk
)
stopifnot(identical(retained, prepared))

prepared_snapshot <- serialize(prepared, NULL)
expect_overlay_error <- function(expression, pattern) {
  expect_error(expression, pattern)
  stopifnot(identical(serialize(prepared, NULL), prepared_snapshot))
}

invisible(file.rename(candidate_path, paste0(candidate_path, ".missing")))
expect_overlay_error(
  pt_apply_reviewed_polygon_geometry(
    "fixture", prepared, TRUE, fixture_root,
    registry = fixture_registry, crosswalk = fixture_crosswalk
  ),
  "Missing required reviewed polygon artifact"
)
expect_overlay_error(
  pt_apply_reviewed_polygon_geometry(
    "fixture", prepared, FALSE, fixture_root,
    registry = fixture_registry, crosswalk = fixture_crosswalk
  ),
  "Missing required reviewed polygon artifact"
)
invisible(file.rename(paste0(candidate_path, ".missing"), candidate_path))
bad_registry <- fixture_registry
bad_registry$candidate_sha256 <- paste(rep("0", 64), collapse = "")
expect_overlay_error(
  pt_apply_reviewed_polygon_geometry(
    "fixture", prepared, TRUE, fixture_root,
    registry = bad_registry, crosswalk = fixture_crosswalk
  ),
  "Reviewed polygon artifact hash mismatch"
)
expect_overlay_error(
  pt_apply_reviewed_polygon_geometry(
    "fixture", prepared, FALSE, fixture_root,
    registry = bad_registry, crosswalk = fixture_crosswalk
  ),
  "Reviewed polygon artifact hash mismatch"
)
bad_registry <- fixture_registry
bad_registry$parent_sha256 <- paste(rep("0", 64), collapse = "")
expect_overlay_error(
  pt_apply_reviewed_polygon_geometry(
    "fixture", prepared, TRUE, fixture_root,
    registry = bad_registry, crosswalk = fixture_crosswalk
  ),
  "Pinned polygon parent hash mismatch"
)
bad_crosswalk <- fixture_crosswalk[-2, , drop = FALSE]
expect_overlay_error(
  pt_apply_reviewed_polygon_geometry(
    "fixture", prepared, TRUE, fixture_root,
    registry = fixture_registry, crosswalk = bad_crosswalk
  ),
  "crosswalk row count differs"
)
bad_crosswalk <- fixture_crosswalk
bad_crosswalk$match_key[[2]] <- bad_crosswalk$match_key[[1]]
expect_overlay_error(
  pt_apply_reviewed_polygon_geometry(
    "fixture", prepared, TRUE, fixture_root,
    registry = fixture_registry, crosswalk = bad_crosswalk
  ),
  "match-key set differs"
)
bad_crosswalk <- fixture_crosswalk
bad_crosswalk$match_key[[2]] <- "missing-semantic-key"
expect_overlay_error(
  pt_apply_reviewed_polygon_geometry(
    "fixture", prepared, TRUE, fixture_root,
    registry = fixture_registry, crosswalk = bad_crosswalk
  ),
  "match-key set differs"
)
original_candidate <- readRDS(candidate_path)
saveRDS(original_candidate[1, ], candidate_path)
row_count_registry <- fixture_registry
row_count_registry$candidate_sha256 <-
  pt_polygon_generalization_sha256_file(candidate_path)
expect_overlay_error(
  pt_apply_reviewed_polygon_geometry(
    "fixture", prepared, TRUE, fixture_root,
    registry = row_count_registry, crosswalk = fixture_crosswalk
  ),
  "candidate row count differs"
)
saveRDS(original_candidate, candidate_path)

# ---- Every legitimate writer is guarded before save ------------------------

guard_scripts <- c(
  "05_map_build/06_refresh_local_reference_wsa_cache.r",
  "05_map_build/08_refresh_local_reference_federal_wilderness_cache.r",
  "05_map_build/09_refresh_local_reference_acec_cache.r",
  "05_map_build/10_refresh_local_reference_national_monuments_cache.r",
  "05_map_build/11_refresh_local_reference_nps_context_cache.r",
  "05_map_build/12_refresh_local_reference_desert_ncl_cache.r"
)
for (path in guard_scripts) {
  source_text <- read_text(path)
  overlay_position <- regexpr("pt_apply_reviewed_polygon_geometry", source_text, fixed = TRUE)[[1]]
  save_position <- regexpr("save_rds_cached", source_text, fixed = TRUE)[[1]]
  stopifnot(overlay_position > 0L, save_position > overlay_position)
}

core <- read_text("05_map_build/02_build_core_map_cache.r")
refresh <- read_text("05_map_build/13_refresh_polygon_generalization_portfolio_caches.r")
label_builder <- read_text("05_map_build/05_build_label_cache.r")
wsr_helper <- read_text("03_functions/leaflet_layer_local_reference_helpers.r")
local_config <- read_text("00_config/config_local_reference_interactions.r")
stopifnot(
  grepl("pt_apply_reviewed_polygon_geometry", core, fixed = TRUE),
  grepl("apply_reviewed", refresh, fixed = TRUE),
  grepl("protected_reference", refresh, fixed = TRUE),
  grepl("protected_labels", refresh, fixed = TRUE),
  grepl("rollback_dir", refresh, fixed = TRUE),
  grepl("file.rename(atomic_temp", refresh, fixed = TRUE),
  grepl("product$Basin <- as.character(product$cnrfc_id)", refresh, fixed = TRUE),
  grepl("cnrfc_product_availability$Basin <- as.character", label_builder, fixed = TRUE),
  !grepl("GEOSCoverageSimplifyVW|ms_simplify|st_simplify", refresh),
  !grepl("download.file|httr|curl|arc_open|st_read", refresh)
)

runtime_contract_tokens <- c(
  "fill_product_availability", "fill_forecast_group",
  "fill_water_supply_availability", "fill_ensemble_availability",
  "fill_qpf_snow_level_availability", "fill_temperature_availability",
  "percentBLMland", "sgma_2019_priority", "ppt_in_fill_col",
  "rech_kaf_fill_col", "wsr_class", "wsr_status", "wsr_action_year",
  "wsr_orv_fish"
)
runtime_source <- paste(
  read_text("03_functions/leaflet_layer_local_reference_helpers.r"),
  read_text("03_functions/leaflet_bulletin118_theme_helpers.r"),
  read_text("03_functions/leaflet_huc_theme_helpers.r"),
  local_config
)
stopifnot(all(vapply(
  runtime_contract_tokens,
  grepl,
  logical(1),
  x = runtime_source,
  fixed = TRUE
)))
stopifnot(
  grepl("disclosures.forEach", wsr_helper, fixed = TRUE),
  grepl("data-wsr-disclosure-source", wsr_helper, fixed = TRUE),
  grepl("activeState[key] ? 'block' : 'none'", wsr_helper, fixed = TRUE),
  grepl("wsr_corridor_blm", wsr_helper, fixed = TRUE),
  grepl("wsr_corridor_lsrs_area", wsr_helper, fixed = TRUE),
  grepl("wsr_corridor_lsrs_status", wsr_helper, fixed = TRUE)
)

message(
  "Polygon portfolio tests passed: 29 rows, 24 replacements, 5 retained, ",
  nrow(crosswalk), " explicit crosswalk rows, 6 focused-writer guards."
)
