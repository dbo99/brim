#!/usr/bin/env Rscript

## Read-only retained-product audit for HUC2/4/6/8/10/12.
##
## Usage:
##   Rscript qa/qa_huc_family_performance.R \
##     /path/to/huc_all_full.rds \
##     /path/to/huc_all_map.rds \
##     /optional/output/directory
##
## The first input is the analytical product and the second is the map-facing
## core cache. This script never modifies either input.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop(
    paste(
      "Supply analytical huc_all_full.rds and map-facing huc_all_map.rds paths.",
      "An optional third argument selects the output directory."
    ),
    call. = FALSE
  )
}

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Package 'sf' is required.", call. = FALSE)
}

analytical_path <- normalizePath(args[[1]], mustWork = TRUE)
map_path <- normalizePath(args[[2]], mustWork = TRUE)
output_dir <- if (length(args) >= 3L && nzchar(args[[3]])) {
  args[[3]]
} else {
  file.path(tempdir(), "brim_huc_family_profile")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, mustWork = TRUE)

analytical <- readRDS(analytical_path)
map_cache <- readRDS(map_path)
huc_names <- paste0("huc", c(2L, 4L, 6L, 8L, 10L, 12L))

validate_family <- function(x, object_name) {
  if (!is.list(x)) {
    stop(object_name, " is not a named HUC list.", call. = FALSE)
  }
  missing_levels <- setdiff(huc_names, names(x))
  if (length(missing_levels)) {
    stop(
      object_name,
      " is missing: ",
      paste(missing_levels, collapse = ", "),
      call. = FALSE
    )
  }
  not_sf <- huc_names[!vapply(x[huc_names], inherits, logical(1), "sf")]
  if (length(not_sf)) {
    stop(
      object_name,
      " contains non-sf level(s): ",
      paste(not_sf, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

validate_family(analytical, "Analytical HUC product")
validate_family(map_cache, "Map HUC cache")

coordinate_metrics <- function(x) {
  if (!nrow(x)) {
    return(c(multipart_features = 0, rings = 0, coordinate_rows = 0))
  }

  geometry <- sf::st_geometry(x)
  geometry_types <- as.character(sf::st_geometry_type(geometry))
  multipart_features <- sum(geometry_types == "MULTIPOLYGON")
  multipolygon <- suppressWarnings(sf::st_cast(geometry, "MULTIPOLYGON"))
  coordinates <- sf::st_coordinates(multipolygon)

  if (!nrow(coordinates)) {
    return(c(
      multipart_features = multipart_features,
      rings = 0,
      coordinate_rows = 0
    ))
  }

  hierarchy_columns <- grep("^L[0-9]+$", colnames(coordinates), value = TRUE)
  rings <- if (length(hierarchy_columns)) {
    nrow(unique(coordinates[, hierarchy_columns, drop = FALSE]))
  } else {
    NA_integer_
  }

  c(
    multipart_features = multipart_features,
    rings = rings,
    coordinate_rows = nrow(coordinates)
  )
}

popup_parent_order_ok <- function(x, level) {
  if (!"popup_html" %in% names(x)) return(NA)
  expected <- rev(c(2L, 4L, 6L, 8L, 10L)[
    c(2L, 4L, 6L, 8L, 10L) < level
  ])
  if (!length(expected)) {
    return(all(!grepl("<b>Parent HUC", x$popup_html, fixed = TRUE)))
  }

  all(vapply(as.character(x$popup_html), function(popup) {
    positions <- vapply(expected, function(parent_level) {
      regexpr(
        paste0("<b>Parent HUC", parent_level, ":</b>"),
        popup,
        fixed = TRUE
      )[[1]]
    }, integer(1))
    all(positions > 0L) && identical(positions, sort(positions))
  }, logical(1)))
}

one_level <- function(x, nm, product) {
  level <- as.integer(sub("^huc", "", nm))
  code <- if (nm %in% names(x)) as.character(x[[nm]]) else character(0)
  valid_code <- !is.na(code) & nzchar(code)
  geometry_types <- sort(unique(as.character(sf::st_geometry_type(x))))
  coord <- coordinate_metrics(x)
  popup_bytes <- if ("popup_html" %in% names(x)) {
    sum(nchar(as.character(x$popup_html), type = "bytes"), na.rm = TRUE)
  } else {
    NA_real_
  }
  theme_columns <- paste0(
    rep(c("ppt_in", "ppt_kaf", "rech_in", "rech_kaf"), each = 3L),
    rep(c("_fill_col", "_fill_label", "_bin"), times = 4L)
  )
  present_theme_columns <- intersect(theme_columns, names(x))
  missing_theme_values <- if (length(present_theme_columns)) {
    sum(vapply(present_theme_columns, function(column) {
      value <- x[[column]]
      sum(is.na(value) | (is.character(value) & !nzchar(value)))
    }, numeric(1)))
  } else {
    NA_real_
  }
  expected_parent_levels <- c(2L, 4L, 6L, 8L, 10L)[
    c(2L, 4L, 6L, 8L, 10L) < level
  ]
  expected_parent_columns <- unlist(lapply(expected_parent_levels, function(parent) {
    paste0(
      "prnt_huc",
      parent,
      c("_code", "_name", "_pctBLM", "_tot", "_blm")
    )
  }), use.names = FALSE)
  parent_column_missing_count <- length(setdiff(expected_parent_columns, names(x)))
  parent_value_missing_count <- if (
    length(expected_parent_columns) &&
      parent_column_missing_count == 0L
  ) {
    sum(vapply(expected_parent_columns, function(column) {
      value <- x[[column]]
      sum(is.na(value) | (is.character(value) & !nzchar(value)))
    }, numeric(1)))
  } else if (length(expected_parent_columns)) {
    NA_real_
  } else {
    0
  }
  theme_triplet_issue_count <- if (length(present_theme_columns)) {
    sum(vapply(c("ppt_in", "ppt_kaf", "rech_in", "rech_kaf"), function(theme) {
      columns <- paste0(theme, c("_fill_col", "_fill_label", "_bin"))
      if (!all(columns %in% names(x))) return(nrow(x))
      availability <- cbind(
        !is.na(x[[columns[[1]]]]) & nzchar(as.character(x[[columns[[1]]]])),
        !is.na(x[[columns[[2]]]]) & nzchar(as.character(x[[columns[[2]]]])),
        !is.na(x[[columns[[3]]]])
      )
      sum(rowSums(availability) != 0L & rowSums(availability) != 3L)
    }, numeric(1)))
  } else {
    NA_real_
  }

  data.frame(
    product = product,
    huc_layer = nm,
    feature_count = nrow(x),
    valid_code_count = sum(valid_code),
    distinct_valid_code_count = length(unique(code[valid_code])),
    browser_polygon_objects = nrow(x),
    empty_geometry_count = sum(sf::st_is_empty(x)),
    invalid_geometry_count = sum(!sf::st_is_valid(x)),
    geometry_types = paste(geometry_types, collapse = ";"),
    multipart_feature_count = unname(coord[["multipart_features"]]),
    ring_count = unname(coord[["rings"]]),
    coordinate_row_count = unname(coord[["coordinate_rows"]]),
    r_object_bytes = as.numeric(object.size(x)),
    popup_bytes = popup_bytes,
    parent_column_missing_count = parent_column_missing_count,
    parent_value_missing_count = parent_value_missing_count,
    theme_column_count = length(present_theme_columns),
    missing_theme_value_count = missing_theme_values,
    theme_triplet_issue_count = theme_triplet_issue_count,
    style_descriptor_count = if (
      all(paste0(c("ppt_in", "ppt_kaf", "rech_in", "rech_kaf"), "_fill_col") %in% names(x))
    ) nrow(x) else 0L,
    nearest_parent_first_popup = popup_parent_order_ok(x, level),
    stringsAsFactors = FALSE
  )
}

metrics <- do.call(rbind, c(
  lapply(huc_names, function(nm) {
    one_level(analytical[[nm]], nm, "analytical")
  }),
  lapply(huc_names, function(nm) {
    one_level(map_cache[[nm]], nm, "map")
  })
))

reconciliation <- do.call(rbind, lapply(huc_names, function(nm) {
  analytical_codes <- as.character(analytical[[nm]][[nm]])
  map_codes <- as.character(map_cache[[nm]][[nm]])
  analytical_codes <- unique(analytical_codes[
    !is.na(analytical_codes) & nzchar(analytical_codes)
  ])
  map_codes <- unique(map_codes[!is.na(map_codes) & nzchar(map_codes)])

  data.frame(
    huc_layer = nm,
    analytical_feature_count = nrow(analytical[[nm]]),
    map_feature_count = nrow(map_cache[[nm]]),
    missing_from_map = length(setdiff(analytical_codes, map_codes)),
    unexpected_in_map = length(setdiff(map_codes, analytical_codes)),
    exact_code_set_match = setequal(analytical_codes, map_codes),
    stringsAsFactors = FALSE
  )
}))

write.csv(
  metrics,
  file.path(output_dir, "huc_family_metrics.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  reconciliation,
  file.path(output_dir, "huc_family_reconciliation.csv"),
  row.names = FALSE,
  na = ""
)

cat("HUC family retained-product audit complete.\n")
cat("Analytical input:", analytical_path, "\n")
cat("Map input:", map_path, "\n")
cat("Output:", output_dir, "\n")
print(metrics, row.names = FALSE)
print(reconciliation, row.names = FALSE)
