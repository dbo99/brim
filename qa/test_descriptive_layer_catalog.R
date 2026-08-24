#!/usr/bin/env Rscript

## Source-only parity checks for the descriptive ten-record catalog mirror:
## the retained six-layer HUC proof and four heterogeneous proof roles.
## This test reads current authored source and the catalog. It does not read
## retained caches, build BRIM, access a network, or execute a preprocessor.

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

assert_identical <- function(actual, expected, message) {
  if (!identical(actual, expected)) {
    stop(
      message,
      "\nExpected: ", paste(expected, collapse = ", "),
      "\nActual: ", paste(actual, collapse = ", "),
      call. = FALSE
    )
  }
}

read_source_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

assert_contains <- function(text, needle, message) {
  assert_true(grepl(needle, text, fixed = TRUE), message)
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  "qa/test_descriptive_layer_catalog.R"
}
project_root <- normalizePath(
  file.path(dirname(script_path), ".."),
  winslash = "/",
  mustWork = TRUE
)
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

expected_head <- "26175854a77654e3a454ec060915f357c4a2c9d1"
expected_tree <- "2561e86af2aeda2f8f970926cfe682b160ca76de"
expected_huc_ids <- c("huc2", "huc4", "huc6", "huc8", "huc10", "huc12")
proof_ids <- c(
  ordinary_local = "gw_bull118",
  external = "EXT143",
  ops_live = "ops_u_s_drought_monitor",
  custom_controller = "usgs_streamgages"
)
expected_ids <- c(expected_huc_ids, unname(proof_ids))
expected_counts <- c(4L, 16L, 24L, 140L, 1128L, 5065L)
names(expected_counts) <- expected_huc_ids
expected_columns <- c(
  "candidate_stable_id",
  "runtime_display_name",
  "architecture",
  "catalog_path",
  "definition_authority",
  "assembly_authority",
  "legend_authority",
  "label_authority",
  "popup_authority",
  "filter_authority",
  "lifecycle_summary",
  "notes_or_resources",
  "domain_review_fields",
  "confidence",
  "gap"
)
allowed_changed_paths <- c(
  "08_docs/BRIM_DEVELOPMENT_ARCHITECTURE.md",
  "08_docs/DOCUMENTATION_INDEX.md",
  "08_docs/catalog/BRIM_LAYER_CATALOG.csv",
  "qa/test_descriptive_layer_catalog.R"
)
catalog_path <- "08_docs/catalog/BRIM_LAYER_CATALOG.csv"

# ---- Baseline and exact working-scope gate ---------------------------------

git_value <- function(args, label) {
  value <- system2("git", args, stdout = TRUE, stderr = TRUE)
  assert_true(
    is.null(attr(value, "status")) && length(value) == 1L,
    paste("Could not read Git", label)
  )
  value[[1]]
}

assert_identical(git_value(c("rev-parse", "HEAD"), "HEAD"), expected_head,
                 "M02 baseline HEAD changed")
assert_identical(
  git_value(c("rev-parse", "HEAD^{tree}"), "tree"),
  expected_tree,
  "M02 baseline tree changed"
)

status_lines <- system2(
  "git",
  c("status", "--porcelain=v1", "--untracked-files=all"),
  stdout = TRUE,
  stderr = TRUE
)
assert_true(is.null(attr(status_lines, "status")), "Could not read Git status")
status_paths <- if (length(status_lines)) substring(status_lines, 4L) else character(0)
assert_identical(
  sort(status_paths),
  sort(allowed_changed_paths),
  "M02 changed-path scope is not exactly the four authorized files"
)
assert_true(
  all(substr(status_lines, 1L, 1L) %in% c(" ", "?")),
  "M02 files must remain unstaged"
)

# ---- Exact CSV serialization and schema ------------------------------------

assert_true(file.exists(catalog_path), "Descriptive layer catalog is missing")
catalog_size <- file.info(catalog_path)$size
assert_true(is.finite(catalog_size) && catalog_size > 0, "Catalog is empty")
catalog_raw <- readBin(catalog_path, what = "raw", n = catalog_size)
utf8_bom <- as.raw(c(0xef, 0xbb, 0xbf))
assert_true(
  length(catalog_raw) < 3L || !identical(catalog_raw[seq_len(3L)], utf8_bom),
  "Catalog must not contain a UTF-8 BOM"
)
assert_true(!any(catalog_raw == as.raw(0x0d)), "Catalog must use LF, not CRLF")
assert_true(!any(catalog_raw == as.raw(0x00)), "Catalog contains a NUL byte")

catalog_text <- rawToChar(catalog_raw)
assert_true(
  !is.na(iconv(catalog_text, from = "UTF-8", to = "UTF-8", sub = NA_character_)),
  "Catalog is not valid UTF-8"
)
assert_true(endsWith(catalog_text, "\n"), "Catalog must end with LF")
assert_true(
  !grepl("\n\n", catalog_text, fixed = TRUE),
  "Catalog contains a blank record"
)

field_connection <- textConnection(catalog_text)
field_counts <- count.fields(
  field_connection,
  sep = ",",
  quote = "\"",
  blank.lines.skip = FALSE,
  comment.char = ""
)
close(field_connection)
if (length(field_counts) && tail(field_counts, 1L) == 0L &&
    endsWith(catalog_text, "\n")) {
  field_counts <- head(field_counts, -1L)
}
assert_identical(field_counts, rep(15L, 11L),
                 "Catalog records do not each contain exactly 15 fields")

catalog <- utils::read.csv(
  catalog_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = character(),
  colClasses = "character",
  fileEncoding = "UTF-8",
  comment.char = "",
  quote = "\"",
  strip.white = FALSE,
  blank.lines.skip = FALSE,
  fill = FALSE
)
assert_identical(names(catalog), expected_columns, "Catalog header changed")
assert_true(nrow(catalog) == 10L, "Catalog must contain exactly ten data rows")
assert_identical(catalog$candidate_stable_id, expected_ids,
                 "Catalog IDs or deterministic row order changed")
assert_true(!anyDuplicated(catalog$candidate_stable_id), "Catalog IDs are not unique")
assert_true(
  identical(catalog$candidate_stable_id[seq_along(expected_huc_ids)],
            expected_huc_ids),
  "The retained HUC rows changed identity or order"
)

catalog_huc <- catalog[
  match(expected_huc_ids, catalog$candidate_stable_id),
  ,
  drop = FALSE
]
catalog_proof <- catalog[
  match(unname(proof_ids), catalog$candidate_stable_id),
  ,
  drop = FALSE
]
assert_true(!anyNA(catalog_huc$candidate_stable_id), "A retained HUC row is missing")
assert_true(!anyNA(catalog_proof$candidate_stable_id),
            "A heterogeneous proof row is missing")

catalog_values <- unlist(catalog, use.names = FALSE)
assert_true(!anyNA(catalog_values), "Catalog contains parser-generated NA")
assert_true(all(nzchar(catalog_values)), "Catalog contains an empty cell")
assert_true(
  all(catalog_values == trimws(catalog_values)),
  "Catalog contains an untrimmed cell"
)

reserved_nulls <- c("UNKNOWN", "NOT_APPLICABLE", "DOMAIN_REVIEW_REQUIRED")
alternate_null <- grepl(
  "^(n/?a|tbd|unknown|not[ _-]?applicable|domain[ _-]?review[ _-]?required)$",
  catalog_values,
  ignore.case = TRUE
)
assert_true(
  !any(alternate_null & !catalog_values %in% reserved_nulls),
  "Catalog contains an alternate null-token spelling"
)
reserved_locations <- which(catalog_values %in% reserved_nulls)
if (length(reserved_locations)) {
  value_matrix <- as.matrix(catalog)
  allowed_reserved_columns <- c(
    "definition_authority", "assembly_authority", "legend_authority",
    "label_authority", "popup_authority", "filter_authority",
    "lifecycle_summary", "notes_or_resources", "domain_review_fields"
  )
  assert_true(
    all(colnames(value_matrix)[col(value_matrix)[reserved_locations]] %in%
          allowed_reserved_columns),
    "Reserved null token occurs in a field that requires a known value"
  )
}

roundtrip_lines <- character(0)
roundtrip_connection <- textConnection("roundtrip_lines", "w", local = TRUE)
utils::write.table(
  catalog,
  roundtrip_connection,
  sep = ",",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  qmethod = "double",
  na = ""
)
close(roundtrip_connection)
roundtrip_read_connection <- textConnection(paste(roundtrip_lines, collapse = "\n"))
roundtrip_catalog <- utils::read.csv(
  roundtrip_read_connection,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = character(),
  colClasses = "character",
  comment.char = "",
  quote = "\""
)
close(roundtrip_read_connection)
assert_true(identical(roundtrip_catalog, catalog),
            "CSV reader/writer round trip changed values or ordering")

# ---- Current registry identity and display-name parity ----------------------

registry_env <- new.env(parent = baseenv())
sys.source("00_config/config_local_layer_registry.r", envir = registry_env)
registry <- registry_env$LOCAL_LAYER_REGISTRY
assert_true(is.data.frame(registry), "LOCAL_LAYER_REGISTRY did not load")
assert_true(!anyDuplicated(registry$layer_id), "Current registry IDs are not unique")

registry_huc <- registry[
  grepl("^huc[0-9]+$", registry$layer_id),
  ,
  drop = FALSE
]
assert_identical(registry_huc$layer_id, expected_huc_ids,
                 "Current registry HUC family is not the exact six-layer scope")
assert_true(nrow(registry_huc) == 6L, "A seventh registry HUC layer was found")
assert_true(all(registry_huc$category == "Basins"), "HUC registry category changed")
assert_identical(
  registry_huc$canonical_group,
  paste0("Basins – ", toupper(expected_huc_ids), " – PRISM/BCMv8"),
  "HUC canonical groups changed"
)
assert_true(all(registry_huc$cache_file == "huc_all_map.rds"),
            "HUC cache-file registry contract changed")
assert_true(all(registry_huc$geometry_family == "polygon"),
            "HUC geometry-family registry contract changed")
assert_true(all(is.na(registry_huc$map_display_flag[seq_len(4L)])),
            "HUC2-HUC8 unexpectedly gained display flags")
assert_identical(
  registry_huc$map_display_flag[5:6],
  c("add_huc10", "add_huc12"),
  "HUC10/HUC12 display-flag registry contract changed"
)

huc_test_lines <- readLines(
  "qa/test_huc_blm_theme.R",
  warn = FALSE,
  encoding = "UTF-8"
)
count_start <- grep("^expected_counts <- list\\($", huc_test_lines)
assert_true(length(count_start) == 1L, "Could not locate current HUC count fixture")
count_end_relative <- which(huc_test_lines[(count_start + 1L):length(huc_test_lines)] == ")")
assert_true(length(count_end_relative) > 0L, "Could not close current HUC count fixture")
count_block <- huc_test_lines[
  count_start:(count_start + count_end_relative[[1]])
]
count_rows <- grep(
  "^[[:space:]]*huc(2|4|6|8|10|12)[[:space:]]*=[[:space:]]*c\\(",
  count_block,
  value = TRUE
)
assert_true(length(count_rows) == 6L, "Current HUC count fixture is not six rows")
fixture_ids <- sub(
  "^[[:space:]]*(huc(2|4|6|8|10|12)).*$",
  "\\1",
  count_rows
)
fixture_counts <- vapply(count_rows, function(line) {
  payload <- sub("^.*c\\(([^)]*)\\).*$", "\\1", line)
  value_tokens <- trimws(strsplit(payload, ",", fixed = TRUE)[[1]])
  value_tokens <- sub("L$", "", value_tokens)
  values <- as.integer(value_tokens)
  assert_true(!anyNA(values), "Could not parse current HUC count fixture")
  sum(values)
}, integer(1))
names(fixture_counts) <- fixture_ids
fixture_counts <- fixture_counts[expected_huc_ids]
assert_identical(fixture_counts, expected_counts,
                 "Current HUC supporting counts changed")

runtime_display_names <- paste(
  registry_huc$canonical_group,
  vapply(
    unname(fixture_counts),
    registry_env$pt_format_local_layer_count,
    character(1)
  )
)
assert_identical(
  catalog_huc$runtime_display_name,
  unname(runtime_display_names),
  "Catalog display names do not match the current registry/count formatter"
)

# ---- Current assembly, visibility, and same-family membership ---------------

display_env <- new.env(parent = baseenv())
sys.source("00_config/config_map_display.r", envir = display_env)
assert_true(isTRUE(display_env$MAP_DISPLAY$add_huc10),
            "MAP_DISPLAY$add_huc10 is not TRUE")
assert_true(isTRUE(display_env$MAP_DISPLAY$add_huc12),
            "MAP_DISPLAY$add_huc12 is not TRUE")
assert_true(
  !any(grepl("HUC(2|4|6|8|10|12)",
             display_env$MAP_DISPLAY$default_visible_overlays)),
  "A HUC layer is no longer default hidden"
)

builder_lines <- readLines(
  "05_map_build/04_build_portatreasure2_core_map.r",
  warn = FALSE,
  encoding = "UTF-8"
)
builder_text <- paste(builder_lines, collapse = "\n")
overlay_start <- grep("^CORE_OVERLAY_GROUPS <- c\\($", builder_lines)
assert_true(length(overlay_start) == 1L, "Could not locate CORE_OVERLAY_GROUPS")
overlay_end_relative <- which(builder_lines[(overlay_start + 1L):length(builder_lines)] == ")")
assert_true(length(overlay_end_relative) > 0L, "Could not close CORE_OVERLAY_GROUPS")
overlay_block <- paste(
  builder_lines[overlay_start:(overlay_start + overlay_end_relative[[1]])],
  collapse = "\n"
)
overlay_matches <- regmatches(
  overlay_block,
  gregexpr('"HUC(2|4|6|8|10|12)"', overlay_block, perl = TRUE)
)[[1]]
overlay_ids <- tolower(gsub('"', "", overlay_matches, fixed = TRUE))
assert_identical(overlay_ids, expected_huc_ids,
                 "CORE_OVERLAY_GROUPS HUC membership/order changed")

for (id in expected_huc_ids) {
  level <- toupper(id)
  count_line <- paste0(
    '"Basins – ', level,
    ' – PRISM/BCMv8" = pt_count_huc_rows(layers$huc_all, "', id, '")'
  )
  assert_contains(
    builder_text,
    count_line,
    paste("Current builder lost the exact count key for", id)
  )
}
assert_contains(builder_text, "m <- pt_add_huc_layers(",
                "Current builder no longer assembles HUC layers")
assert_contains(builder_text, "m <- pt_add_huc_theme_controls(",
                "Current builder no longer installs HUC theme controls")

source_paths <- c(
  polygon = "03_functions/leaflet_layer_local_polygon_helpers.r",
  theme_r = "03_functions/leaflet_huc_theme_helpers.r",
  theme_js = "03_functions/js/brim_huc_theme_control.js",
  labels = "00_config/config_labels.r",
  label_builder = "05_map_build/05_build_label_cache.r",
  popup = "03_functions/popup_helpers.r",
  popup_cache = "05_map_build/02_cache_blocks/02_cache_huc_climate_theme.r"
)
source_text <- lapply(source_paths, read_source_text)
scoped_huc_text <- paste(c(builder_text, unlist(source_text)), collapse = "\n")
source_huc_matches <- unique(regmatches(
  scoped_huc_text,
  gregexpr("\\bhuc[0-9]+\\b", scoped_huc_text, perl = TRUE)
)[[1]])
visible_source_huc_ids <- registry$layer_id[registry$layer_id %in% source_huc_matches]
visible_source_huc_ids <- visible_source_huc_ids[
  grepl("^huc[0-9]+$", visible_source_huc_ids)
]
assert_identical(visible_source_huc_ids, expected_huc_ids,
                 "Scoped runtime source and registry no longer agree on six HUC layers")

# ---- Architecture and authority-reference parity ----------------------------

expected_architecture <- paste(
  c(
    "LOCAL", "LOCAL_STANDARD_LEAFLET", "ACTIVE_CURRENT_RUNTIME",
    "USER_VISIBLE", "CUSTOM_CONTROLLER", "NON_GENERIC_RENDERER"
  ),
  collapse = ";"
)
assert_true(all(catalog_huc$architecture == expected_architecture),
            "Catalog HUC architecture changed")
architecture_tokens <- strsplit(catalog_huc$architecture, ";", fixed = TRUE)
assert_true(
  all(vapply(architecture_tokens, function(tokens) {
    sum(tokens == "CUSTOM_CONTROLLER") == 1L &&
      sum(tokens == "NON_GENERIC_RENDERER") == 1L
  }, logical(1))),
  "A HUC architecture marker is missing or duplicated"
)

compact_polygon <- gsub("[[:space:]]+", "", source_text$polygon)
compact_theme_js <- gsub("[[:space:]]+", "", source_text$theme_js)
assert_contains(source_text$polygon, "pt_add_huc_layer <- function",
                "Current HUC construction helper is missing")
assert_contains(compact_polygon, "L.canvas({pane:'pane_huc'})",
                "Current R HUC Canvas bootstrap is missing")
assert_contains(source_text$theme_js, "window.BRIM_HUC_LOCAL",
                "Current custom HUC controller global is missing")
assert_contains(source_text$theme_js, "function destroy()",
                "Current HUC controller destroy owner is missing")
assert_contains(compact_theme_js, "L.canvas({pane:'pane_huc'})",
                "Current shared HUC Canvas renderer is missing")

assert_true(all(catalog_huc$catalog_path == "Local/Water/Hydrologic units"),
            "Catalog path changed")
assert_true(
  all(nzchar(catalog$catalog_path)) &&
    all(catalog$catalog_path == trimws(catalog$catalog_path)) &&
    !any(startsWith(catalog$catalog_path, "/")) &&
    !any(endsWith(catalog$catalog_path, "/")) &&
    !any(grepl("//", catalog$catalog_path, fixed = TRUE)),
  "Catalog path is empty, untrimmed, or has an invalid boundary/separator"
)

extract_reference_paths <- function(value) {
  tokens <- unlist(strsplit(value, "[[:space:];,]+", perl = TRUE), use.names = FALSE)
  tokens <- sub(":.*$", "", tokens)
  tokens <- gsub("^[('\\\"]+|[)'\\\"]+$", "", tokens, perl = TRUE)
  unique(tokens[grepl("\\.(r|R|js|md|rds|csv)$", tokens)])
}

authority_columns <- c(
  "definition_authority", "assembly_authority", "legend_authority",
  "label_authority", "popup_authority", "filter_authority",
  "notes_or_resources"
)
for (row_index in seq_len(nrow(catalog_huc))) {
  id <- catalog_huc$candidate_stable_id[[row_index]]
  for (column in authority_columns) {
    references <- extract_reference_paths(catalog_huc[[column]][[row_index]])
    assert_true(
      length(references) > 0L,
      paste(id, column, "contains no parseable authority path")
    )
    for (reference in references) {
      generated_contract <- startsWith(reference, "04_processed_data/") ||
        identical(reference, "labels_all_map.rds")
      local_a02_evidence <- startsWith(reference, "A02_")
      if (!generated_contract && !local_a02_evidence) {
        assert_true(
          file.exists(reference),
          paste(id, column, "references missing current path", reference)
        )
      }
    }
  }

  assert_contains(
    catalog_huc$definition_authority[[row_index]],
    paste0("LOCAL_LAYER_REGISTRY[layer_id=", id, "]"),
    paste(id, "definition authority lost its registry key")
  )
  assert_contains(
    catalog_huc$definition_authority[[row_index]],
    paste0("pt_add_huc_layer(code_col=", id, ")"),
    paste(id, "definition authority lost its construction key")
  )
  assert_contains(
    catalog_huc$assembly_authority[[row_index]],
    "pt_register_local_layer_feature_counts",
    paste(id, "assembly authority does not name the current count owner")
  )
  assert_contains(
    catalog_huc$assembly_authority[[row_index]],
    paste0("pt_count_huc_rows key ", id),
    paste(id, "assembly authority lost its current count key")
  )
  assert_contains(
    catalog_huc$assembly_authority[[row_index]],
    paste0("CORE_OVERLAY_GROUPS ", if (id %in% c("huc10", "huc12")) "conditional " else "",
           toupper(id), " entry"),
    paste(id, "assembly authority lost its overlay entry")
  )
  assert_contains(
    catalog_huc$legend_authority[[row_index]],
    "PT_HUC_THEME_REGISTRY",
    paste(id, "legend authority lost the HUC theme registry")
  )
  assert_contains(
    catalog_huc$label_authority[[row_index]],
    paste0("LABEL_ZOOM/LABEL_INCLUDE/LABEL_FIELDS[", id, "]"),
    paste(id, "label authority lost its label key")
  )
  assert_contains(
    catalog_huc$popup_authority[[row_index]],
    paste0("popup_html for ", id),
    paste(id, "popup authority lost its cache key")
  )
  assert_contains(
    catalog_huc$filter_authority[[row_index]],
    paste0("theme payload ", id),
    paste(id, "filter authority lost its theme key")
  )
  for (phase in c(
    "Initialize/register", "activate/update/filter/tooltip/popup/clear/destroy",
    "BRIM_HUC_LOCAL", "generation scheduler", "timers", "state", "DOM",
    "Clear Local/Clear All", "shared renderer=L.canvas pane_huc",
    "network=NOT_APPLICABLE", "live=NOT_APPLICABLE"
  )) {
    assert_contains(
      catalog_huc$lifecycle_summary[[row_index]],
      phase,
      paste(id, "lifecycle summary lost", phase)
    )
  }
}

for (symbol in c(
  "PT_HUC_THEME_REGISTRY", "pt_huc_blm_legend_one", "pt_huc_legend_one",
  "pt_build_huc_theme_data", "pt_add_huc_theme_controls"
)) {
  assert_contains(source_text$theme_r, symbol,
                  paste("Current HUC legend/theme source lost", symbol))
}
for (symbol in c("pt_make_huc_popups", "pt_make_huc_hover_tooltips")) {
  assert_contains(source_text$popup, symbol,
                  paste("Current HUC popup source lost", symbol))
}
assert_contains(source_text$popup_cache, "popup_html <- pt_make_huc_popups",
                "Current HUC popup cache assembly changed")
assert_contains(source_text$polygon, "popup = ~popup_html",
                "Current HUC popup binding changed")
assert_contains(source_text$polygon, "label = ~pt_huc_hover_html",
                "Current HUC hover binding changed")

label_child_start <- regexpr("expected_children <- unique(c(", source_text$label_builder,
                             fixed = TRUE)[[1]]
assert_true(label_child_start > 0L, "Could not locate label-cache child contract")
label_child_slice <- substr(
  source_text$label_builder,
  label_child_start,
  min(nchar(source_text$label_builder), label_child_start + 900L)
)
label_child_ids <- unique(regmatches(
  label_child_slice,
  gregexpr('"huc[0-9]+"', label_child_slice, perl = TRUE)
)[[1]])
label_child_ids <- gsub('"', "", label_child_ids, fixed = TRUE)
assert_identical(label_child_ids, expected_huc_ids,
                 "Label-cache expected HUC child set changed")

for (id in expected_huc_ids) {
  assert_true(
    grepl(paste0('"', id, '"'), source_text$labels, fixed = TRUE),
    paste("LABEL_ZOOM lost", id)
  )
  assert_true(
    grepl(paste0(id, "[[:space:]]*=[[:space:]]*TRUE"), source_text$labels),
    paste("LABEL_INCLUDE lost", id)
  )
  assert_true(
    grepl(paste0('"', id, '_name"'), source_text$labels, fixed = TRUE),
    paste("LABEL_FIELDS lost", id)
  )
}

assert_true(
  all(catalog_huc$domain_review_fields ==
        "purpose|audience|suitability|scientific_limitations_beyond_current_source"),
  "Domain-review field list changed"
)
assert_true(all(catalog_huc$confidence == "HIGH"),
            "HUC mechanical confidence changed")
assert_true(
  all(catalog_huc$gap ==
        "A02_CONTROLLER_RENDERER_DESCRIPTION_CORRECTED_BY_CURRENT_SOURCE"),
  "Current-source controller/renderer correction was not preserved"
)

# ---- Four-role heterogeneous proof parity ----------------------------------

expected_proof_display_names <- c(
  "Basins – GW Basins, Bulletin 118 (515)",
  "CARB air districts",
  "U.S. Drought Monitor",
  "Points – USGS streamgages (~2.4k)"
)
expected_proof_architectures <- c(
  "LOCAL;LOCAL_STANDARD_LEAFLET;ACTIVE_CURRENT_RUNTIME;USER_VISIBLE;ORDINARY_LEAFLET;NON_GENERIC_RENDERER",
  "EXTERNAL;EXTERNAL_CATALOG_BRIDGE;ACTIVE_ENABLED;USER_VISIBLE;SHARED_CONTROLLER;GENERIC_CATEGORICAL_RENDERER",
  "OPS_LIVE;OPS_LIVE_CUSTOM_CONTROLLER;ACTIVE_CURRENT_RUNTIME;USER_VISIBLE;CUSTOM_CONTROLLER;NON_GENERIC_RENDERER",
  "LOCAL;LOCAL_CUSTOM_CONTROLLER;ACTIVE_CURRENT_RUNTIME;USER_VISIBLE;CUSTOM_CONTROLLER;NON_GENERIC_RENDERER"
)
expected_proof_paths <- c(
  "Local/Water/Groundwater basins",
  "External/Air / Smoke/CARB context",
  "Ops Live/Drought",
  "Local/Water/Monitoring"
)
assert_identical(catalog_proof$candidate_stable_id, unname(proof_ids),
                 "Heterogeneous proof role IDs or order changed")
assert_identical(catalog_proof$runtime_display_name,
                 expected_proof_display_names,
                 "Heterogeneous display-name parity failed")
assert_identical(catalog_proof$architecture, expected_proof_architectures,
                 "Heterogeneous architecture parity failed")
assert_identical(catalog_proof$catalog_path, expected_proof_paths,
                 "Heterogeneous catalog-path parity failed")
assert_true(all(catalog_proof$confidence == "HIGH"),
            "Heterogeneous proof confidence changed")
assert_true(
  all(catalog_proof$domain_review_fields ==
        "purpose|audience|suitability|scientific_limitations_beyond_current_source"),
  "Heterogeneous domain-review semantics changed"
)

for (row_index in seq_len(nrow(catalog_proof))) {
  id <- catalog_proof$candidate_stable_id[[row_index]]
  for (column in authority_columns) {
    value <- catalog_proof[[column]][[row_index]]
    if (value %in% reserved_nulls) next
    references <- extract_reference_paths(value)
    assert_true(
      length(references) > 0L,
      paste(id, column, "contains no parseable authority path")
    )
    for (reference in references) {
      generated_contract <- startsWith(reference, "04_processed_data/") ||
        identical(reference, "labels_all_map.rds")
      assert_true(
        generated_contract || file.exists(reference),
        paste(id, column, "references missing current path", reference)
      )
    }
  }
}

local_proof_registry <- registry[
  match(c("gw_bull118", "usgs_streamgages"), registry$layer_id),
  ,
  drop = FALSE
]
assert_identical(local_proof_registry$layer_id,
                 c("gw_bull118", "usgs_streamgages"),
                 "Selected Local proof registry IDs changed")
assert_identical(
  local_proof_registry$canonical_group,
  c("Basins – GW Basins, Bulletin 118", "Points – USGS streamgages"),
  "Selected Local proof canonical groups changed"
)
assert_identical(
  local_proof_registry$cache_file,
  c("gw_bull118_map.rds", "usgs_streamgages_map.rds"),
  "Selected Local proof cache contracts changed"
)
assert_identical(local_proof_registry$geometry_family, c("polygon", "point"),
                 "Selected Local proof geometry families changed")

assert_contains(
  builder_text,
  '"Basins – GW Basins, Bulletin 118" = pt_count_sf_rows(layers$gw)',
  "Bulletin 118 count authority changed"
)
assert_contains(
  builder_text,
  '"Points – USGS streamgages" = pt_count_sf_rows(layers$usgs_sw)',
  "USGS streamgage count authority changed"
)
for (symbol in c(
  '"GW – Bull. 118"', "pt_add_county_gw_layers(",
  '"USGS streamgages"', "pt_add_usgs_layers(",
  "pt_add_usgs_streamgage_catalog_legend("
)) {
  assert_contains(builder_text, symbol,
                  paste("Selected Local assembly lost", symbol))
}

bulletin_text <- read_source_text("03_functions/bulletin118_data_helpers.r")
assert_contains(bulletin_text, "PT_BULLETIN118_EXPECTED_ROWS <- 515L",
                "Bulletin 118 exact current count contract changed")
assert_contains(source_text$polygon, "pt_add_county_gw_layers <- function",
                "Bulletin 118 construction helper is missing")
assert_contains(source_text$polygon, "r._brimBulletin118=true",
                "Bulletin 118 non-generic Canvas feature renderer is missing")
assert_contains(source_text$polygon, "popup = ~popup_html",
                "Bulletin 118 popup binding is missing")
assert_contains(source_text$polygon, "label = ~pt_gw_hover_html",
                "Bulletin 118 hover binding is missing")

external_catalog <- utils::read.csv(
  "00_config/external_service_catalog.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
external_row <- external_catalog[
  external_catalog$external_layer_id == proof_ids[["external"]],
  ,
  drop = FALSE
]
assert_true(nrow(external_row) == 1L, "EXT143 source row is not unique")
assert_identical(external_row$display_name, "CARB air districts",
                 "EXT143 display name changed")
assert_identical(external_row$external_group, "Air / Smoke",
                 "EXT143 External group changed")
assert_identical(external_row$external_subgroup, "CARB context",
                 "EXT143 External subgroup changed")
assert_identical(external_row$service_type, "feature",
                 "EXT143 service type changed")
assert_identical(external_row$default_load_mode, "current_view",
                 "EXT143 load mode changed")
assert_identical(external_row$default_style_method, "categorical_distinct",
                 "EXT143 style method changed")
assert_identical(tolower(external_row$supports_popups), "true",
            "EXT143 popup capability changed")

external_r_text <- read_source_text("03_functions/leaflet_tools_adddata_helpers.r")
external_js_text <- read_source_text("03_functions/js/leaflet_tools_adddata_panel.js")
assert_contains(external_r_text, "pt_add_tools_adddata_panel <- function",
                "External catalog assembly helper is missing")
assert_contains(external_r_text,
                'catalog_path <- file.path("00_config", "external_service_catalog.csv")',
                "External catalog source embedding changed")
for (symbol in c(
  "window.ptOpsExternalCatalogBridge", "ptRenderCustomLayerList",
  "generic_categorical", "ptGenericCategoricalLegendHtml",
  "ptExternalFeatureCollectionLayer", "ptOnEachFeature",
  "ptTooltipFromProperties", "ptPopupFromProperties"
)) {
  assert_contains(external_js_text, symbol,
                  paste("EXT143 shared External authority lost", symbol))
}

ops_text <- read_source_text(
  "03_functions/leaflet_ops_live_layer_definition_helpers.r"
)
drought_start <- regexpr("name: 'U.S. Drought Monitor'", ops_text, fixed = TRUE)[[1]]
assert_true(drought_start > 0L, "Ops Live Drought Monitor definition is missing")
drought_slice <- substr(
  ops_text,
  drought_start,
  min(nchar(ops_text), drought_start + 1400L)
)
for (symbol in c(
  "CatalogPromotedExternalLayer", "ops_us_drought_monitor_current",
  "U.S. Drought Monitor (current)",
  "USDM_current/FeatureServer/0"
)) {
  assert_contains(drought_slice, symbol,
                  paste("Ops Live Drought Monitor lost", symbol))
}
for (symbol in c(
  "onAdd: function", "onRemove: function", "forceRemove: function",
  "window.ptOpsExternalCatalogBridge", "ptOpsClearPromotedSlowTimers",
  "activeLegendDefs"
)) {
  assert_contains(ops_text, symbol,
                  paste("Ops Live controller authority lost", symbol))
}
assert_contains(builder_text, "m <- pt_add_ops_live_layers(",
                "Final builder lost Ops Live assembly")

usgs_text <- read_source_text("03_functions/leaflet_layer_local_usgs_helpers.r")
for (symbol in c(
  "pt_add_usgs_streamgage_catalog_legend <- function",
  "pt_add_usgs_streamgage_browser_layer <- function",
  "window.BRIM_USGS_SW_LOCAL", "applyFilters: function",
  "setActive: function", "makePopup", "makeTooltip",
  "pt_usgs_streamgages_dummy", "pt_usgs_streamgages_label_dummy"
)) {
  assert_contains(usgs_text, symbol,
                  paste("USGS custom-controller authority lost", symbol))
}
assert_true(
  grepl("CUSTOM_CONTROLLER", catalog_proof$architecture[[4]], fixed = TRUE) &&
    grepl("NON_GENERIC_RENDERER", catalog_proof$architecture[[4]], fixed = TRUE),
  "USGS custom/non-generic catalog markers are missing"
)
assert_true(
  !grepl("CUSTOM_CONTROLLER", catalog_proof$architecture[[1]], fixed = TRUE),
  "Ordinary Local proof was incorrectly marked as a custom controller"
)
assert_true(
  grepl("SHARED_CONTROLLER", catalog_proof$architecture[[2]], fixed = TRUE) &&
    grepl("GENERIC_CATEGORICAL_RENDERER",
          catalog_proof$architecture[[2]], fixed = TRUE),
  "External shared/generic catalog markers are missing"
)
assert_true(
  grepl("CUSTOM_CONTROLLER", catalog_proof$architecture[[3]], fixed = TRUE) &&
    grepl("NON_GENERIC_RENDERER", catalog_proof$architecture[[3]], fixed = TRUE),
  "Ops Live custom/non-generic catalog markers are missing"
)

assert_identical(
  c(
    catalog_proof$legend_authority[[1]],
    catalog_proof$filter_authority[[1]],
    catalog_proof$label_authority[[2]],
    catalog_proof$label_authority[[3]]
  ),
  rep("NOT_APPLICABLE", 4L),
  "Selected proof null/not-applicable semantics changed"
)
assert_contains(catalog_proof$gap[[3]], "NORMALIZED_LIVE_STATUS_UNKNOWN",
                "Ops Live unknown status gap was lost")
assert_contains(catalog_proof$lifecycle_summary[[3]],
                "producer confirmation=NOT_APPLICABLE",
                "Ops Live producer-side nonclaim was lost")
assert_contains(catalog_proof$gap[[4]],
                "DUMMY_ANCHORS_EXCLUDED_RUNTIME_SCAFFOLDING",
                "USGS scaffolding exclusion was lost")
assert_true(
  !any(catalog$candidate_stable_id %in%
         c("pt_usgs_streamgages_dummy", "pt_usgs_streamgages_label_dummy")),
  "A USGS runtime-scaffolding anchor became a catalog record"
)

# ---- Scaffolding, executable-content, and path-safety exclusions ------------

scaffolding_pattern <- paste(
  c("dummy", "placeholder", "pt_empty_", "label_dummy"),
  collapse = "|"
)
assert_true(
  !any(grepl(scaffolding_pattern,
             catalog$candidate_stable_id,
             ignore.case = TRUE)),
  "Catalog maps a runtime-scaffolding construct as a stable record"
)
assert_true(
  all(catalog_huc$candidate_stable_id %in% registry_huc$layer_id) &&
    all(catalog_huc$candidate_stable_id %in% overlay_ids),
  "A catalog row does not map to current visible runtime evidence"
)

executable_patterns <- c(
  "<script",
  "javascript:",
  "^#!",
  "source[[:space:]]*\\(",
  "sys\\.source[[:space:]]*\\(",
  "eval[[:space:]]*\\(",
  "parse[[:space:]]*\\(",
  "system[[:space:]]*\\(",
  "system2[[:space:]]*\\(",
  "function[[:space:]]*\\(",
  "=>",
  "on(click|load)[[:space:]]*="
)
for (pattern in executable_patterns) {
  assert_true(
    !any(grepl(pattern, catalog_values, ignore.case = TRUE, perl = TRUE)),
    paste("Catalog contains executable-content indicator", pattern)
  )
}
assert_true(
  !any(grepl("^[=+@-]", catalog_values, perl = TRUE)),
  "Catalog contains a spreadsheet-formula prefix"
)

absolute_path_patterns <- c(
  "(^|[[:space:];=])/(Users|home|private|tmp|var|opt|Volumes)/",
  "[A-Za-z]:[\\\\/]",
  "^\\\\\\\\"
)
for (pattern in absolute_path_patterns) {
  assert_true(
    !any(grepl(pattern, catalog_values, perl = TRUE)),
    paste("Catalog contains a local absolute machine path matching", pattern)
  )
}
assert_true(
  !any(grepl("BRIM_rehabilitation_audit_staging|A02_layer_metadata_census_3d262c6",
             catalog_values)),
  "Catalog contains an internal evidence-staging path"
)

# ---- Mandatory descriptive-only/no-runtime-consumption proof ----------------

runtime_dirs <- c("00_config", "02_preprocess", "03_functions", "05_map_build")
runtime_files <- c(
  "run_build_map.r",
  unlist(lapply(runtime_dirs, function(directory) {
    list.files(
      directory,
      pattern = "\\.(R|r|js|css|sh)$",
      recursive = TRUE,
      full.names = TRUE
    )
  }), use.names = FALSE)
)
runtime_files <- unique(runtime_files[file.exists(runtime_files)])
runtime_catalog_references <- runtime_files[vapply(runtime_files, function(path) {
  text <- read_source_text(path)
  grepl("BRIM_LAYER_CATALOG", text, fixed = TRUE) ||
    grepl("08_docs/catalog", text, fixed = TRUE)
}, logical(1))]
assert_true(
  length(runtime_catalog_references) == 0L,
  paste(
    "Production/runtime source consumes or references the descriptive catalog:",
    paste(runtime_catalog_references, collapse = ", ")
  )
)

runtime_auto_interpreters <- runtime_files[vapply(runtime_files, function(path) {
  text <- read_source_text(path)
  grepl(
    "(source|sys\\.source|read\\.csv|read_csv|read\\.delim|fromJSON|yaml\\.load|list\\.files|dir)[[:space:]]*\\([^)]*08_docs",
    text,
    ignore.case = TRUE,
    perl = TRUE
  )
}, logical(1))]
assert_true(
  length(runtime_auto_interpreters) == 0L,
  paste(
    "Production/runtime source auto-interprets 08_docs:",
    paste(runtime_auto_interpreters, collapse = ", ")
  )
)

assert_true(
  !"SOURCE_MANIFEST.csv" %in% status_paths,
  "SOURCE_MANIFEST.csv must remain byte-for-byte unchanged for NOT_REQUIRED"
)

message("Descriptive ten-record layer catalog parity passed.")
message("HETEROGENEOUS_DESCRIPTIVE_MECHANISM_PROVEN_FOR_SELECTED_RECORDS")
message("CATALOG_AUTHORITY=DESCRIPTIVE_ONLY")
message("RUNTIME_AUTHORITY=UNCHANGED")
message("RUNTIME_CATALOG_CONSUMPTION=NONE")
