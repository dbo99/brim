#!/usr/bin/env Rscript

## Deterministic source/fixture checks for the authoritative SWRCB 2026
## face-value resolver. No production cache is read or written.

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

assert_numeric_equal <- function(actual, expected, message, tolerance = 1e-12) {
  same_na <- is.na(actual) && is.na(expected)
  same_value <- !is.na(actual) && !is.na(expected) &&
    abs(actual - expected) <= tolerance
  assert_true(same_na || same_value, message)
}

assert_error <- function(expr, pattern, message) {
  error <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = identity
  )
  assert_true(inherits(error, "error"), paste0(message, " (no error)"))
  assert_true(
    grepl(pattern, conditionMessage(error), ignore.case = TRUE, perl = TRUE),
    paste0(message, " (unexpected error: ", conditionMessage(error), ")")
  )
  invisible(error)
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  "qa/test_swrcb_face_value_resolution.R"
}
project_root <- normalizePath(
  file.path(dirname(script_path), ".."),
  mustWork = TRUE
)
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

required_packages <- c("dplyr", "readr", "sf", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing required fixture package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

source("03_functions/swrcb_face_value_helpers.r")

make_rows <- function(ids, values) {
  data.frame(
    "Water Right/Claim ID" = ids,
    "Face value" = values,
    Status = rep("Active", length(ids)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

make_override <- function(
    id = "S014142",
    value = "0.4",
    basis = "conflicting_authoritative_row",
    note = paste(
      "Reviewed authoritative evidence selects the documented 0.4 AFY",
      "source row for this test fixture."
    ),
    reference = "SWRCB reviewed source memorandum, fixture item S014142",
    reviewed_date = "2026-07-29") {
  data.frame(
    swrcb_wr_id_norm = id,
    resolved_face_afy = value,
    resolution_type = "reviewed_authoritative_override",
    value_basis = basis,
    resolution_note = note,
    source_reference = reference,
    reviewed_date = reviewed_date,
    stringsAsFactors = FALSE
  )
}

lookup_row <- function(result, id) {
  row <- result$lookup[result$lookup$swrcb_wr_id_norm == id, , drop = FALSE]
  assert_true(nrow(row) == 1L, paste("Expected one lookup row for", id))
  row
}

# ---- Resolver classification fixtures --------------------------------------

fixture_rows <- make_rows(
  c(
    "SINGLE", "DUP_SAME", "DUP_SAME", "RAW_SAME", "RAW_SAME",
    "ONE_USABLE", "ONE_USABLE", "BLANK", "BLANK",
    "AUTO_01", "AUTO_01", "AUTO_0153", "AUTO_0153",
    "AUTO_0061", "AUTO_0061", "ZERO_ONE", "ZERO_ONE", "ZERO_FIVE",
    "ZERO_FIVE", "THREE_VALUES", "THREE_VALUES", "THREE_VALUES",
    "POSITIVE_CONFLICT", "POSITIVE_CONFLICT"
  ),
  c(
    "2.5", "4", "4", "1", "1.0",
    "3", "", "", NA, "0", "0.1", "0", "0.0153",
    "0", "0.0061", "0", "1", "0",
    "5", "0", "0.1", "0.2",
    "0.0061", "0.4"
  )
)

fixture_result <- pt_build_swrcb_face_value_lookup(fixture_rows)

single <- lookup_row(fixture_result, "SINGLE")
assert_numeric_equal(single$face_afy_2026, 2.5, "Single row did not resolve")
assert_true(
  single$face_value_2026_resolution == "single_row",
  "Single row classification changed"
)

duplicate_same <- lookup_row(fixture_result, "DUP_SAME")
assert_numeric_equal(
  duplicate_same$face_afy_2026,
  4,
  "Duplicate identical values did not resolve"
)
assert_true(
  duplicate_same$face_value_2026_resolution == "duplicate_rows_same_value",
  "Duplicate identical-value classification changed"
)

raw_same <- lookup_row(fixture_result, "RAW_SAME")
assert_numeric_equal(
  raw_same$face_afy_2026,
  1,
  "Different raw strings with the same numeric value did not resolve"
)
assert_true(
  raw_same$face_value_2026_distinct_count == 1L &&
    raw_same$face_value_2026_raw_values == "1 | 1.0",
  "Raw-string provenance was not retained independently of numeric equality"
)

one_usable <- lookup_row(fixture_result, "ONE_USABLE")
assert_true(
  one_usable$face_afy_2026 == 3 &&
    one_usable$face_value_2026_usable_value_row_count == 1L &&
    one_usable$face_value_2026_resolution == "single_row",
  "One usable value plus a blank row was mislabeled as an equal duplicate"
)

blank <- lookup_row(fixture_result, "BLANK")
assert_true(
  is.na(blank$face_afy_2026) &&
    blank$face_value_2026_resolution == "blank_or_missing",
  "Blank-only authoritative rows were not kept distinct"
)

for (case in list(
  c("AUTO_01", 0.1),
  c("AUTO_0153", 0.0153),
  c("AUTO_0061", 0.0061)
)) {
  row <- lookup_row(fixture_result, case[[1]])
  assert_numeric_equal(
    row$face_afy_2026,
    as.numeric(case[[2]]),
    paste("Zero-truncation fixture did not resolve:", case[[1]])
  )
  assert_true(
    row$face_value_2026_conflict &&
      row$face_value_2026_automatic_resolution &&
      row$face_value_2026_resolution ==
        "duplicate_zero_truncation_resolved" &&
      row$spreadsheet_row_count == 2L &&
      row$face_value_2026_distinct_count == 2L,
    paste("Zero-truncation provenance incomplete:", case[[1]])
  )
}

for (id in c("ZERO_ONE", "ZERO_FIVE", "THREE_VALUES")) {
  row <- lookup_row(fixture_result, id)
  assert_true(
    is.na(row$face_afy_2026) &&
      row$face_value_2026_resolution ==
        "unresolved_authoritative_conflict" &&
      !row$face_value_2026_automatic_resolution,
    paste("Over-broad zero-truncation rule incorrectly resolved", id)
  )
}

positive_conflict <- lookup_row(fixture_result, "POSITIVE_CONFLICT")
assert_true(
  positive_conflict$face_value_2026_numeric_values == "0.0061 | 0.4" &&
    is.na(positive_conflict$face_afy_2026) &&
    positive_conflict$face_value_2026_resolution ==
      "unresolved_authoritative_conflict",
  "Positive-versus-positive conflict did not remain unresolved"
)

assert_error(
  pt_parse_swrcb_face_values("not-a-number", ids = "BAD"),
  "strictly parse",
  "Nonblank invalid authoritative value was accepted"
)
assert_error(
  pt_parse_swrcb_face_values("1,2", ids = "BAD_GROUPING"),
  "strictly parse",
  "Malformed grouped authoritative value was accepted"
)

missing_id_warned <- FALSE
missing_id_result <- withCallingHandlers(
  pt_build_swrcb_face_value_lookup(make_rows(c("", "VALID"), c("9", "2"))),
  warning = function(warning) {
    missing_id_warned <<- TRUE
    invokeRestart("muffleWarning")
  }
)
assert_true(
  missing_id_warned &&
    nrow(missing_id_result$invalid_id_rows) == 1L &&
    identical(missing_id_result$lookup$swrcb_wr_id_norm, "VALID"),
  "Missing water-right ID was not retained in invalid-ID QA"
)

# ---- Reviewed override validation ------------------------------------------

positive_rows <- make_rows(
  c("S014142", "S014142"),
  c("0.0061", "0.4")
)
valid_override_result <- pt_build_swrcb_face_value_lookup(
  positive_rows,
  overrides = make_override()
)
resolved_positive <- lookup_row(valid_override_result, "S014142")
assert_numeric_equal(
  resolved_positive$face_afy_2026,
  0.4,
  "Valid reviewed override did not resolve the positive conflict"
)
assert_true(
  resolved_positive$face_value_2026_conflict &&
    resolved_positive$face_value_2026_reviewed_override_used &&
    resolved_positive$face_value_2026_resolution ==
      "reviewed_authoritative_override" &&
    resolved_positive$face_value_2026_numeric_values == "0.0061 | 0.4",
  "Reviewed override erased original conflict provenance"
)

assert_error(
  pt_validate_swrcb_face_value_override_table(
    rbind(make_override(), make_override())
  ),
  "duplicate IDs",
  "Duplicate override IDs were accepted"
)
assert_error(
  pt_validate_swrcb_face_value_override_table(make_override(id = "")),
  "blank water-right IDs",
  "Blank override ID was accepted"
)
assert_error(
  pt_validate_swrcb_face_value_override_table(
    make_override(value = "not-a-number")
  ),
  "strictly parse",
  "Nonnumeric override value was accepted"
)
assert_error(
  pt_validate_swrcb_face_value_override_table(make_override(value = "Inf")),
  "strictly parse",
  "Nonfinite override value was accepted"
)
assert_error(
  pt_validate_swrcb_face_value_override_table(make_override(value = "-0.4")),
  "nonnegative",
  "Negative override value was accepted"
)
assert_error(
  pt_validate_swrcb_face_value_override_table(make_override(value = "TBD")),
  "placeholder",
  "Placeholder override value was accepted"
)
assert_error(
  pt_validate_swrcb_face_value_override_table(make_override(note = "")),
  "meaningful resolution_note",
  "Missing override resolution note was accepted"
)
assert_error(
  pt_validate_swrcb_face_value_override_table(make_override(reference = "")),
  "meaningful source_reference",
  "Missing override source reference was accepted"
)
assert_error(
  pt_validate_swrcb_face_value_override_table(
    make_override(reviewed_date = "07/29/2026")
  ),
  "ISO reviewed_date",
  "Invalid override reviewed date was accepted"
)
assert_error(
  pt_build_swrcb_face_value_lookup(
    make_rows("NOT_CONFLICTED", "1"),
    overrides = make_override(id = "NOT_CONFLICTED", value = "1")
  ),
  "not current unresolved",
  "Irrelevant override ID was accepted"
)
assert_error(
  pt_build_swrcb_face_value_lookup(
    positive_rows,
    overrides = make_override(value = "0.2")
  ),
  "not represented",
  "Unsupported override value was accepted without independent evidence"
)

# ---- Public fallback and source-display policy ------------------------------

selection_lookup <- pt_build_swrcb_face_value_lookup(
  make_rows(
    c("PRESENT", "BLANK", "CONFLICT", "CONFLICT", "AUTO", "AUTO"),
    c("2", "", "0.2", "0.3", "0", "0.1")
  )
)$lookup
selection <- pt_reconcile_swrcb_face_values(
  water_right_ids = c("PRESENT", "ABSENT", "BLANK", "CONFLICT", "AUTO"),
  public_face_afy = c(20, 9, 8, 7, 0),
  public_face_units = rep("acre-feet per year", 5),
  lookup = selection_lookup,
  stop_on_unresolved = FALSE
)

assert_numeric_equal(
  selection$selected_face_afy[selection$swrcb_wr_id_norm == "PRESENT"],
  2,
  "Authoritative single value did not supersede public value"
)
absent_index <- selection$swrcb_wr_id_norm == "ABSENT"
assert_numeric_equal(
  selection$selected_face_afy[absent_index],
  9,
  "Public fallback failed for ID absent from authoritative export"
)
assert_true(
  selection$public_value_allowed[absent_index] &&
    selection$selected_face_value_source[absent_index] ==
      "SWRCB/CalWATRS public data; ID absent from 2026 export",
  "Absent-ID public provenance is incorrect"
)
blank_index <- selection$swrcb_wr_id_norm == "BLANK"
assert_numeric_equal(
  selection$selected_face_afy[blank_index],
  8,
  "Documented present-but-blank public policy changed"
)
assert_true(
  selection$public_value_allowed[blank_index] &&
    selection$selected_face_value_source[blank_index] ==
      "SWRCB/CalWATRS public data; 2026 export value blank or missing",
  "Present-but-blank condition was conflated with absent ID"
)
conflict_index <- selection$swrcb_wr_id_norm == "CONFLICT"
assert_true(
  is.na(selection$selected_face_afy[conflict_index]) &&
    !selection$public_value_allowed[conflict_index] &&
    selection$selected_face_value_source[conflict_index] ==
      "Unresolved SWRCB-provided 2026 BLM export conflict",
  "Unresolved authoritative conflict reached public fallback"
)
auto_index <- selection$swrcb_wr_id_norm == "AUTO"
assert_numeric_equal(
  selection$selected_face_afy[auto_index],
  0.1,
  "Automatic-resolution selection changed"
)
assert_true(
  selection$selected_face_value_source[auto_index] ==
    paste(
      "SWRCB-provided 2026 BLM export;",
      "fractional zero conflict resolved"
    ),
  "Automatic-resolution source display is incorrect"
)

reviewed_selection <- pt_reconcile_swrcb_face_values(
  "S014142",
  public_face_afy = 0,
  public_face_units = "acre-feet per year",
  lookup = valid_override_result$lookup
)
assert_true(
  reviewed_selection$selected_face_value_source ==
    "SWRCB-provided 2026 BLM export; reviewed conflict resolution",
  "Reviewed-resolution source display is incorrect"
)

unordered_result <- pt_build_swrcb_face_value_lookup(
  make_rows(c("z", "a", "M"), c("3", "1", "2"))
)
assert_true(
  identical(
    unordered_result$lookup$swrcb_wr_id_norm,
    c("A", "M", "Z")
  ),
  "Resolver output ordering is not deterministic"
)

# ---- Build gate writes nothing after an unresolved conflict -----------------

gate_dir <- tempfile("swrcb_face_value_gate_")
dir.create(gate_dir)
gate_source <- file.path(gate_dir, "authoritative.csv")
gate_override <- file.path(gate_dir, "overrides.csv")
gate_output <- file.path(gate_dir, "map_cache.rds")
readr::write_csv(positive_rows, gate_source)
readr::write_csv(pt_empty_swrcb_face_value_overrides(), gate_override)

fake_cache_build <- function() {
  result <- pt_read_swrcb_2026_blm_wr_lookup(
    gate_source,
    gate_override,
    stop_on_unresolved = TRUE
  )
  saveRDS(result, gate_output)
}
gate_error <- assert_error(
  fake_cache_build(),
  "S014142.*0.0061.*0.4",
  "Unresolved conflict did not block the fixture cache build"
)
assert_true(
  !file.exists(gate_output),
  "Fixture cache was written after an unresolved authoritative conflict"
)

# ---- Actual immutable-source audit -----------------------------------------

authoritative_csv <- file.path(
  "01_raw_data",
  "swrcb_water_rights",
  "All_your_water_right_7_1_2026_8_56_30.csv"
)
authoritative_xlsx <- paste0(authoritative_csv, ".xlsx")
tracked_override <- file.path(
  "00_config",
  "swrcb_2026_face_value_conflict_resolutions.csv"
)

source_hashes <- unname(tools::md5sum(c(authoritative_csv, authoritative_xlsx)))
assert_true(
  identical(
    source_hashes,
    c(
      "77805f683c036a94e6cfc3534d52ad9e",
      "6599fb78caba1e41a651d0326cd4df4e"
    )
  ),
  "Immutable SWRCB CSV/XLSX source hashes changed"
)

actual <- pt_read_swrcb_2026_blm_wr_lookup(
  authoritative_csv,
  tracked_override,
  stop_on_unresolved = FALSE
)
actual_conflicts <- actual$lookup[
  actual$lookup$face_value_2026_conflict,
  ,
  drop = FALSE
]
assert_true(
  identical(
    actual_conflicts$swrcb_wr_id_norm,
    c("A022008", "S014140", "S014141", "S014142")
  ),
  "Actual-source audit did not identify exactly four expected conflicts"
)
assert_true(
  identical(
    actual_conflicts$face_value_2026_resolution[1:3],
    rep("duplicate_zero_truncation_resolved", 3)
  ) &&
    isTRUE(all.equal(
      actual_conflicts$face_afy_2026[1:3],
      c(0.1, 0.0153, 0.0061),
      tolerance = 1e-12
    )),
  "Actual-source zero-truncation classifications changed"
)
assert_true(
  actual_conflicts$swrcb_wr_id_norm[4] == "S014142" &&
    actual_conflicts$face_value_2026_numeric_values[4] ==
      "0.0061 | 0.4" &&
    actual_conflicts$face_value_2026_resolution[4] ==
      "unresolved_authoritative_conflict",
  "Actual-source S014142 conflict was not classified positive-versus-positive"
)
assert_true(
  nrow(actual$duplicate_conflicts) == 8L &&
    all(c(
      "face_value_2026_numeric_values",
      "face_value_2026_resolution",
      "face_value_2026_automatic_resolution",
      "face_value_2026_reviewed_override_used"
    ) %in% names(actual$duplicate_conflicts)),
  "Duplicate-conflict QA did not retain all eight original conflict rows"
)
assert_true(
  actual$summary$original_conflicting_ids == 4L &&
    actual$summary$
      automatically_resolved_zero_truncation_conflicts == 3L &&
    actual$summary$reviewed_override_resolutions == 0L &&
    actual$summary$unresolved_blocking_conflicts == 1L,
  "Actual-source deterministic summary changed"
)

# ---- Corrected retained-cache preservation fixture --------------------------

conflict_fixture_rows <- make_rows(
  c(
    "A022008", "A022008", "S014140", "S014140",
    "S014141", "S014141", "S014142", "S014142", "UNCHANGED"
  ),
  c("0", "0.1", "0", "0.0153", "0", "0.0061", "0.0061", "0.4", "7")
)
corrected_lookup <- pt_build_swrcb_face_value_lookup(
  conflict_fixture_rows,
  overrides = make_override()
)$lookup

cache_before <- sf::st_as_sf(
  data.frame(
    water_right_id = c(
      "A022008", "S014140", "S014141", "S014142", "UNCHANGED", "ABSENT"
    ),
    pod_feature_id = c("149528", "131113", "156464", "160361", "U1", "A1"),
    pt_swrcb_layer_id = paste0("swrcb_pod_wr_", 1:6),
    official_list = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    screening_class = c(
      "official", "official", "official", "official", "official", "spatial"
    ),
    status_group = c(
      "Active / recognized", "Active / recognized", "Active / recognized",
      "Active / recognized", "Pending", "Inactive / cancelled"
    ),
    dist_to_blm_mi = c(0, 0.1, 0.2, 0.3, 1, 2),
    official_layer = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    spatial_layer = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
    name_layer = rep(FALSE, 6),
    public_face_afy = c(0.1, 0, 0, 0, 7, 8),
    lon = -120 + seq_len(6) / 100,
    lat = 35 + seq_len(6) / 100,
    stringsAsFactors = FALSE
  ),
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

cache_after <- cache_before
corrected_selection <- pt_reconcile_swrcb_face_values(
  cache_before$water_right_id,
  cache_before$public_face_afy,
  rep("acre-feet per year", nrow(cache_before)),
  corrected_lookup
)
cache_after$face_afy <- corrected_selection$selected_face_afy
cache_after$face_value_source_display <-
  corrected_selection$selected_face_value_source

old_source <- ifelse(
  cache_before$water_right_id %in%
    c("A022008", "S014140", "S014141", "S014142"),
  paste(
    "SWRCB/CalWATRS public fallback;",
    "conflicting 2026 duplicate values"
  ),
  ifelse(
    cache_before$water_right_id == "UNCHANGED",
    "SWRCB-provided 2026 BLM export",
    "SWRCB/CalWATRS public data; ID absent from 2026 export"
  )
)
changed_rows <- cache_before$public_face_afy != cache_after$face_afy |
  old_source != cache_after$face_value_source_display
assert_true(
  identical(
    cache_before$water_right_id[changed_rows],
    c("A022008", "S014140", "S014141", "S014142")
  ),
  "Corrected cache fixture changed rows outside the four conflict IDs"
)
assert_true(
  identical(sf::st_geometry(cache_before), sf::st_geometry(cache_after)) &&
    identical(sf::st_crs(cache_before), sf::st_crs(cache_after)),
  "Corrected cache fixture changed geometry or CRS"
)
preserved_columns <- c(
  "water_right_id", "pod_feature_id", "pt_swrcb_layer_id",
  "official_list", "screening_class", "status_group", "dist_to_blm_mi",
  "official_layer", "spatial_layer", "name_layer"
)
assert_true(
  identical(
    sf::st_drop_geometry(cache_before)[, preserved_columns],
    sf::st_drop_geometry(cache_after)[, preserved_columns]
  ),
  "Corrected cache fixture changed retained identity/classification fields"
)
assert_true(
  identical(
    sf::st_drop_geometry(cache_before)[, c(
      "official_layer", "spatial_layer", "name_layer"
    )],
    sf::st_drop_geometry(cache_after)[, c(
      "official_layer", "spatial_layer", "name_layer"
    )]
  ),
  "Three Local SWRCB layer memberships changed"
)

# ---- Static integration and unchanged-preprocessor checks -------------------

preprocessor_paths <- c(
  "02_preprocess/16_swrcb_waterrights_pod.r",
  "02_preprocess/61_update_swrcb_pod_blm_distance_fields.R"
)
preprocessor_hashes <- unname(tools::md5sum(preprocessor_paths))
assert_true(
  identical(
    preprocessor_hashes,
    c(
      "06ce962353e9af2c849cce7c3c9d03d5",
      "690de75168c03217f8b4aa61b116b1d7"
    )
  ),
  "SWRCB preprocessors 16_ or 61_ were modified by this source patch"
)

preprocessor_16 <- paste(readLines(preprocessor_paths[1], warn = FALSE),
                         collapse = "\n")
assert_true(
  grepl("ALLOW_DOWNLOADS <- FALSE", preprocessor_16, fixed = TRUE) &&
    grepl("REFRESH_EXISTING_CACHE <- FALSE", preprocessor_16, fixed = TRUE) &&
    grepl("wr_face_value_amount", preprocessor_16, fixed = TRUE) &&
    !grepl("pt_read_swrcb_2026_blm_wr_lookup", preprocessor_16, fixed = TRUE),
  "Preprocessor 16_ static public-source contract changed"
)

preprocessor_61 <- paste(readLines(preprocessor_paths[2], warn = FALSE),
                         collapse = "\n")
for (field in c(
  "pt_swrcb_layer_id", "water_right", "face_afy", "face_bin",
  "dist_to_blm_mi", "dist_to_blm_ft", "blm_distance_run_time"
)) {
  assert_true(
    grepl(field, preprocessor_61, fixed = TRUE),
    paste("Distance sidecar schema lost field", field)
  )
}
assert_true(
  !grepl("pt_read_swrcb_2026_blm_wr_lookup", preprocessor_61, fixed = TRUE),
  "Distance preprocessor acquired duplicate face-value resolution logic"
)

cache_builder_lines <- readLines(
  "05_map_build/02_cache_blocks/04_cache_admin_water_reference_layers.r",
  warn = FALSE
)
cache_builder <- paste(cache_builder_lines, collapse = "\n")
core_builder_lines <- readLines(
  "05_map_build/02_build_core_map_cache.r",
  warn = FALSE
)
core_builder <- paste(core_builder_lines, collapse = "\n")
assert_true(
  grepl(
    "source(\"03_functions/swrcb_face_value_helpers.r\")",
    core_builder,
    fixed = TRUE
  ) &&
    grepl("stop_on_unresolved = TRUE", cache_builder, fixed = TRUE) &&
    grepl("pt_reconcile_swrcb_face_values", cache_builder, fixed = TRUE),
  "Core-cache integration is missing the authoritative resolver/build gate"
)
overlay_block_line <- grep(
  'source("05_map_build/02_cache_blocks/04_cache_admin_water_reference_layers.r")',
  core_builder_lines,
  fixed = TRUE
)
swrcb_cache_save_line <- grep(
  'save_map_cache(swrcb_pod_wr_blm_map, "swrcb_pod_wr_blm_map")',
  core_builder_lines,
  fixed = TRUE
)
assert_true(
  length(overlay_block_line) == 1L &&
    length(swrcb_cache_save_line) == 1L &&
    overlay_block_line < swrcb_cache_save_line,
  "Core-cache build gate no longer precedes the SWRCB map-cache save"
)

layer_helper <- paste(readLines(
  "03_functions/leaflet_layer_local_swrcb_helpers.r",
  warn = FALSE
), collapse = "\n")
for (consumer in c(
  "pt_swrcb_face_bin", "pt_swrcb_radius", "swrcbpop_face_value",
  "swrcbpop_face_value_source", "face_afy"
)) {
  assert_true(
    grepl(consumer, layer_helper, fixed = TRUE),
    paste("Downstream SWRCB consumer missing:", consumer)
  )
}

active_r_files <- list.files(
  c("00_config", "02_preprocess", "03_functions", "05_map_build"),
  pattern = "[.][rR]$",
  recursive = TRUE,
  full.names = TRUE
)
active_r_files <- c(active_r_files, "run_build_map.r")
active_r_files <- normalizePath(active_r_files, mustWork = TRUE)
old_warning_text <- paste(
  "SWRCB/CalWATRS public fallback;",
  "conflicting 2026 duplicate values"
)
old_warning_hits <- vapply(active_r_files, function(path) {
  any(grepl(
    old_warning_text,
    readLines(path, warn = FALSE),
    fixed = TRUE
  ))
}, logical(1))
assert_true(
  !any(old_warning_hits),
  paste(
    "Previous public-conflict-fallback text remains in active R code:",
    paste(active_r_files[old_warning_hits], collapse = ", ")
  )
)

message(
  "PASS: SWRCB authoritative face-value resolver, build gate, provenance, ",
  "actual-source audit, and preservation fixtures"
)
