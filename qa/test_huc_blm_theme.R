#!/usr/bin/env Rscript

## Focused fixed-%BLM fixture and optional retained HUC cache audit.
##
## Usage:
##   Rscript qa/test_huc_blm_theme.R
##   Rscript qa/test_huc_blm_theme.R \
##     /path/to/huc_all_map.rds /path/to/gw_bull118_map.rds
##
## The optional retained product is read only.

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  "qa/test_huc_blm_theme.R"
}
project_root <- normalizePath(
  file.path(dirname(script_path), ".."),
  mustWork = TRUE
)
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

required_packages <- c("dplyr", "purrr", "sf", "tibble")
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

source("03_functions/blm_pct_theme_helpers.r")
source("03_functions/leaflet_layer_local_core_helpers.r")
source("03_functions/leaflet_huc_theme_helpers.r")

huc_names <- paste0("huc", c(2L, 4L, 6L, 8L, 10L, 12L))
expected_counts <- list(
  huc2 = c(0L, 1L, 2L, 1L, 0L, 0L, 0L, 0L, 0L),
  huc4 = c(1L, 1L, 9L, 2L, 1L, 2L, 0L, 0L, 0L),
  huc6 = c(2L, 3L, 11L, 1L, 5L, 1L, 1L, 0L, 0L),
  huc8 = c(19L, 34L, 41L, 17L, 13L, 13L, 2L, 1L, 0L),
  huc10 = c(407L, 219L, 147L, 116L, 67L, 71L, 55L, 46L, 0L),
  huc12 = c(2743L, 519L, 384L, 339L, 262L, 210L, 218L, 390L, 0L)
)
expected_counts <- lapply(expected_counts, function(counts) {
  stats::setNames(counts, PT_BLM_PCT_BIN_LEVELS)
})
expected_total <- Reduce(`+`, expected_counts)
representative_values <- stats::setNames(
  c(0, 0.5, 2, 10, 20, 40, 60, 80, NA_real_),
  PT_BLM_PCT_BIN_LEVELS
)
thresholds <- c(0, 1, 50, 70, 75, 100)
expected_threshold_counts <- list(
  huc2 = c(4L, 3L, 0L, 0L, 0L, 0L),
  huc4 = c(16L, 14L, 0L, 0L, 0L, 0L),
  huc6 = c(24L, 19L, 1L, 0L, 0L, 0L),
  huc8 = c(140L, 87L, 3L, 1L, 1L, 0L),
  huc10 = c(1128L, 502L, 101L, 59L, 46L, 0L),
  huc12 = c(5065L, 1805L, 608L, 431L, 390L, 7L)
)
expected_total_threshold_counts <- c(6377L, 2430L, 713L, 491L, 437L, 7L)

count_bins <- function(values) {
  unname(as.integer(table(factor(
    pt_blm_pct_bin(values),
    levels = PT_BLM_PCT_BIN_LEVELS
  ))))
}

count_thresholds <- function(values) {
  values <- suppressWarnings(as.numeric(values))
  vapply(thresholds, function(threshold) {
    if (threshold == 0) {
      return(sum(!is.finite(values) | values >= threshold))
    }
    sum(is.finite(values) & values >= threshold)
  }, integer(1))
}

assert_level_counts <- function(huc_all, label) {
  assert_true(is.list(huc_all), paste(label, "is not a HUC list"))
  assert_true(
    identical(intersect(huc_names, names(huc_all)), huc_names),
    paste(label, "does not contain all six HUC levels in order")
  )

  combined <- numeric(0)
  for (nm in huc_names) {
    x <- huc_all[[nm]]
    assert_true(
      "percentBLMland" %in% names(x),
      paste(label, nm, "is missing percentBLMland")
    )
    values <- suppressWarnings(as.numeric(x$percentBLMland))
    assert_true(
      nrow(x) == sum(expected_counts[[nm]]),
      paste(label, nm, "feature count changed")
    )
    assert_true(
      all(is.na(values) | (values >= 0 & values <= 100)),
      paste(label, nm, "has a percentBLMland value outside 0..100")
    )
    assert_true(
      identical(count_bins(values), unname(expected_counts[[nm]])),
      paste(label, nm, "fixed %BLM bin counts changed")
    )
    assert_true(
      identical(count_thresholds(values), expected_threshold_counts[[nm]]),
      paste(label, nm, "minimum %BLM threshold counts changed")
    )
    combined <- c(combined, values)
  }

  assert_true(length(combined) == 6377L, paste(label, "total is not 6,377"))
  assert_true(sum(is.na(combined)) == 0L, paste(label, "has unexpected missing %BLM"))
  assert_true(
    identical(count_bins(combined), unname(expected_total)),
    paste(label, "combined fixed %BLM counts changed")
  )
  assert_true(
    identical(count_thresholds(combined), expected_total_threshold_counts),
    paste(label, "combined minimum %BLM threshold counts changed")
  )
  invisible(TRUE)
}

make_level_fixture <- function(nm) {
  counts <- expected_counts[[nm]]
  values_by_bin <- lapply(seq_along(counts), function(i) {
    rep(unname(representative_values[[i]]), counts[[i]])
  })

  exact_one_count <- expected_threshold_counts[[nm]][[2]] -
    sum(counts[seq.int(3L, 8L)])
  if (exact_one_count > 0L) {
    values_by_bin[[2]][seq_len(exact_one_count)] <- 1
  }

  at_least_70_count <- expected_threshold_counts[[nm]][[4]] - counts[[8]]
  if (at_least_70_count > 0L) {
    values_by_bin[[7]][seq_len(at_least_70_count)] <- 70
  }

  exact_100_count <- expected_threshold_counts[[nm]][[6]]
  if (exact_100_count > 0L) {
    values_by_bin[[8]][seq_len(exact_100_count)] <- 100
  }

  values <- unlist(values_by_bin, use.names = FALSE)
  n <- length(values)
  df <- data.frame(
    percentBLMland = values,
    ppt_in_fill_col = rep("#77AADD", n),
    ppt_in_fill_label = rep("fixture precip", n),
    ppt_in_bin = rep(1L, n),
    ppt_kaf_fill_col = rep("#2255AA", n),
    ppt_kaf_fill_label = rep("fixture precip volume", n),
    ppt_kaf_bin = rep(1L, n),
    rech_in_fill_col = rep("#88CC88", n),
    rech_in_fill_label = rep("fixture recharge", n),
    rech_in_bin = rep(1L, n),
    rech_kaf_fill_col = rep("#228833", n),
    rech_kaf_fill_label = rep("fixture recharge volume", n),
    rech_kaf_bin = rep(1L, n),
    stringsAsFactors = FALSE
  )
  df[[nm]] <- paste0(nm, "_", seq_len(n))
  geometry <- sf::st_sfc(
    lapply(seq_len(n), function(i) {
      sf::st_point(c(i %% 100, i %/% 100))
    }),
    crs = 4326
  )
  sf::st_sf(df, geometry = geometry)
}

fixture <- stats::setNames(lapply(huc_names, make_level_fixture), huc_names)
assert_level_counts(fixture, "HUC %BLM fixture")

theme_data <- pt_build_huc_theme_data(fixture)
theme_ids <- vapply(theme_data$data$themes, `[[`, character(1), "id")
assert_true(
  identical(
    theme_ids,
    c("none", "blm_pct", "ppt_in", "ppt_kaf", "rech_in", "rech_kaf")
  ),
  "HUC theme order changed or an option was duplicated"
)
assert_true(sum(theme_ids == "blm_pct") == 1L, "%BLM theme must exist once")
assert_true(
  identical(theme_data$data$default_theme, "none"),
  "Boundaries-only must be the fresh-controller default"
)
assert_true(
  identical(
    vapply(theme_data$data$themes, `[[`, character(1), "label"),
    c(
      "Boundaries only (no fill)",
      "BLM-managed land \u2014 %",
      "PRISM precip - in/yr",
      "PRISM precip - kaf/yr",
      "BCMv8 recharge - in/yr",
      "BCMv8 recharge - kaf/yr"
    )
  ),
  "HUC theme display labels or order changed"
)
assert_true(theme_data$lookup_count == 6377L, "HUC lookup lost retained rows")

lookup <- theme_data$data$lookup
lookup_ids <- vapply(lookup, `[[`, character(1), "layer_id")
assert_true(!anyDuplicated(lookup_ids), "HUC lookup contains duplicate level/code keys")

for (nm in huc_names) {
  source_values <- fixture[[nm]]$percentBLMland
  expected_colors <- unname(PT_BLM_PCT_COLORS[
    as.character(pt_blm_pct_bin(source_values))
  ])
  records <- lookup[vapply(
    lookup,
    function(record) identical(record$huc_layer, nm),
    logical(1)
  )]
  actual_colors <- vapply(records, `[[`, character(1), "blm_pct")
  actual_values <- vapply(records, function(record) {
    if (is.null(record$percent_blm)) NA_real_ else as.numeric(record$percent_blm)
  }, numeric(1))
  assert_true(
    identical(actual_colors, expected_colors),
    paste(nm, "lookup does not use the shared fixed classifier")
  )
  assert_true(
    identical(actual_values, as.numeric(source_values)),
    paste(nm, "lookup changed retained %BLM values")
  )

  legend_matches <- theme_data$data$legends[vapply(
    theme_data$data$legends,
    function(record) {
      identical(record$huc_layer, nm) &&
        identical(record$theme, "blm_pct")
    },
    logical(1)
  )]
  assert_true(length(legend_matches) == 1L, paste(nm, "has duplicate %BLM legends"))
  rows <- legend_matches[[1]]$legend$rows
  assert_true(
    identical(vapply(rows, `[[`, character(1), "label"), PT_BLM_PCT_BIN_LEVELS),
    paste(nm, "%BLM legend labels changed")
  )
  assert_true(
    identical(vapply(rows, `[[`, character(1), "color"), unname(PT_BLM_PCT_COLORS)),
    paste(nm, "%BLM legend colors changed")
  )
  assert_true(
    identical(
      vapply(rows, `[[`, integer(1), "count"),
      unname(expected_counts[[nm]])
    ),
    paste(nm, "%BLM active-level legend counts changed")
  )
  assert_true(
    is.null(legend_matches[[1]]$legend$note),
    paste(nm, "%BLM legend must not add a bottom note")
  )
}

assert_true(
  !any(vapply(theme_data$data$legends, function(record) {
    !is.null(record$legend$note)
  }, logical(1))),
  "HUC legend payload retained an explanatory note"
)

bulletin_theme_source <- paste(
  readLines("03_functions/leaflet_bulletin118_theme_helpers.r", warn = FALSE),
  collapse = "\n"
)
assert_true(
  grepl("pt_blm_pct_bin", bulletin_theme_source, fixed = TRUE) &&
    grepl("pt_blm_pct_legend_rows", bulletin_theme_source, fixed = TRUE),
  "Bulletin 118 does not consume the shared %BLM classifier"
)

args <- commandArgs(trailingOnly = TRUE)
if (length(args)) {
  retained_path <- normalizePath(args[[1]], mustWork = TRUE)
  retained <- readRDS(retained_path)
  assert_level_counts(retained, "Retained HUC map cache")
  retained_theme_data <- pt_build_huc_theme_data(retained)
  assert_true(
    retained_theme_data$lookup_count == 6377L,
    "Retained HUC browser lookup is not one row per feature"
  )
  cat("Retained HUC %BLM audit: PASS\n")
  cat("Input:", retained_path, "\n")
}

if (length(args) >= 2L) {
  bulletin_path <- normalizePath(args[[2]], mustWork = TRUE)
  bulletin <- readRDS(bulletin_path)
  assert_true(nrow(bulletin) == 515L, "Retained Bulletin 118 total is not 515")
  assert_true(
    "percentBLMland" %in% names(bulletin),
    "Retained Bulletin 118 cache is missing percentBLMland"
  )
  bulletin_values <- suppressWarnings(as.numeric(bulletin$percentBLMland))
  assert_true(
    all(is.finite(bulletin_values)),
    "Retained Bulletin 118 has unexpected missing/nonfinite %BLM"
  )
  assert_true(
    all(bulletin_values >= 0 & bulletin_values <= 100),
    "Retained Bulletin 118 has %BLM outside 0..100"
  )
  assert_true(
    identical(
      count_bins(bulletin_values),
      c(279L, 66L, 36L, 32L, 23L, 29L, 26L, 24L, 0L)
    ),
    "Retained Bulletin 118 revised-bin counts changed"
  )
  assert_true(
    identical(
      count_thresholds(bulletin_values),
      c(515L, 170L, 50L, 30L, 24L, 0L)
    ),
    "Retained Bulletin 118 threshold counts changed"
  )
  cat("Retained Bulletin 118 shared %BLM audit: PASS\n")
  cat("Input:", bulletin_path, "\n")
}

cat("HUC fixed-%BLM theme fixture: PASS\n")
