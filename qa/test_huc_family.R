#!/usr/bin/env Rscript

## Deterministic fixture checks for the six-level HUC family.
##
## This test does not read or write retained production caches. It verifies:
##   - nearest-parent-first popup order for HUC2/4/6/8/10/12;
##   - preservation of popup scientific fields;
##   - unique browser theme IDs and complete theme/legend records;
##   - hidden-group-before-polygons call order; and
##   - explicit Canvas renderer serialization in HUC path options.

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  "qa/test_huc_family.R"
}
project_root <- normalizePath(
  file.path(dirname(script_path), ".."),
  mustWork = TRUE
)
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

required_packages <- c(
  "dplyr", "htmltools", "htmlwidgets", "leaflet", "purrr", "sf", "tibble"
)
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

source("03_functions/leaflet_layer_local_core_helpers.r")
source("03_functions/leaflet_huc_theme_helpers.r")
source("03_functions/popup_helpers.r")
source("03_functions/leaflet_layer_local_polygon_helpers.r")

huc_levels <- c(2L, 4L, 6L, 8L, 10L, 12L)
theme_names <- c("ppt_in", "ppt_kaf", "rech_in", "rech_kaf")

fixture_polygon <- sf::st_polygon(list(matrix(
  c(
    -121, 36,
    -120, 36,
    -120, 37,
    -121, 37,
    -121, 36
  ),
  ncol = 2,
  byrow = TRUE
)))

make_huc_fixture <- function(level) {
  nm <- paste0("huc", level)
  df <- data.frame(
    percentBLMland = 12.34,
    total_area_sqmi = 456.78,
    blm_area_sqmi = 56.37,
    map_in = 18.7,
    ppt_kaf = 33.7,
    rech_in = 1.57,
    rech_kaf = 2.82,
    rech_eff_pct = 8.4,
    ppt_valid_frac = 1,
    rech_valid_frac = 1,
    ppt_in_fill_col = "#77AADD",
    ppt_in_fill_label = "10.0 - 20.0",
    ppt_in_bin = 1L,
    ppt_kaf_fill_col = "#2255AA",
    ppt_kaf_fill_label = "20.0 - 40.0",
    ppt_kaf_bin = 2L,
    rech_in_fill_col = "#88CC88",
    rech_in_fill_label = "1.0 - 2.0",
    rech_in_bin = 3L,
    rech_kaf_fill_col = "#228833",
    rech_kaf_fill_label = "2.0 - 4.0",
    rech_kaf_bin = 4L,
    stringsAsFactors = FALSE
  )
  df[[nm]] <- sprintf(paste0("%0", level, "d"), level)
  df[[paste0(nm, "_name")]] <- paste("Fixture", toupper(nm))

  for (parent_level in c(2L, 4L, 6L, 8L, 10L)) {
    prefix <- paste0("prnt_huc", parent_level)
    df[[paste0(prefix, "_code")]] <- paste0("P", sprintf("%02d", parent_level))
    df[[paste0(prefix, "_name")]] <- paste("Parent", parent_level)
    df[[paste0(prefix, "_pctBLM")]] <- parent_level + 0.25
    df[[paste0(prefix, "_tot")]] <- parent_level * 100
    df[[paste0(prefix, "_blm")]] <- parent_level * 10
  }

  sf::st_sf(
    df,
    geometry = sf::st_sfc(fixture_polygon, crs = 4326)
  )
}

huc_all <- stats::setNames(
  lapply(huc_levels, make_huc_fixture),
  paste0("huc", huc_levels)
)

for (level in huc_levels) {
  nm <- paste0("huc", level)
  popup <- pt_make_huc_popups(huc_all[[nm]], level)[[1]]
  expected_parents <- rev(c(2L, 4L, 6L, 8L, 10L)[
    c(2L, 4L, 6L, 8L, 10L) < level
  ])

  actual_positions <- vapply(expected_parents, function(parent_level) {
    regexpr(
      paste0("<b>Parent HUC", parent_level, ":</b>"),
      popup,
      fixed = TRUE
    )[[1]]
  }, integer(1))

  if (length(expected_parents)) {
    assert_true(
      all(actual_positions > 0L),
      paste("Missing parent popup row for", toupper(nm))
    )
    assert_true(
      identical(actual_positions, sort(actual_positions)),
      paste("Parent popup order is not nearest-first for", toupper(nm))
    )
  } else {
    assert_true(
      !grepl("<b>Parent HUC", popup, fixed = TRUE),
      "HUC2 must not show a parent HUC row"
    )
  }

  for (required_text in c(
    "%BLM-CA:", "Total area:", "BLM-CA area:",
    "Mean annual precip:", "Precip volume:",
    "Mean annual recharge:", "Recharge volume:",
    "Recharge efficiency:", "Raster valid area:"
  )) {
    assert_true(
      grepl(required_text, popup, fixed = TRUE),
      paste("Popup lost required scientific field:", required_text, "for", nm)
    )
  }

  huc_all[[nm]]$popup_html <- popup
}

theme_data <- pt_build_huc_theme_data(huc_all)
assert_true(
  identical(theme_data$lookup_count, length(huc_levels)),
  "Theme lookup count does not equal fixture feature count"
)

lookup <- theme_data$data$lookup
lookup_ids <- vapply(lookup, `[[`, character(1), "layer_id")
assert_true(!anyDuplicated(lookup_ids), "Theme lookup IDs are not unique")
assert_true(
  length(theme_data$data$levels) == length(huc_levels),
  "Theme level metadata is incomplete"
)
assert_true(
  length(theme_data$data$legends) == length(huc_levels) * length(theme_names),
  "Theme legend records are incomplete"
)

for (record in lookup) {
  assert_true(
    all(vapply(theme_names, function(theme) {
      is.character(record[[theme]]) && nzchar(record[[theme]])
    }, logical(1))),
    paste("Theme colors are incomplete for", record$layer_id)
  )
}

for (level_record in theme_data$data$levels) {
  assert_true(
    identical(as.integer(level_record$expected_count), 1L),
    paste("Unexpected fixture count for", level_record$huc_layer)
  )
  assert_true(
    identical(
      level_record$group_name,
      pt_huc_group_name(level_record$huc_layer)
    ),
    paste("Theme/drawing group mismatch for", level_record$huc_layer)
  )
}

embedded_widget <- pt_add_huc_theme_controls(
  leaflet::leaflet(),
  huc_all = huc_all
)
embedded_hook <- embedded_widget$jsHooks$render[[1]]
source_js <- paste(
  readLines("03_functions/js/brim_huc_theme_control.js", warn = FALSE),
  collapse = "\n"
)
assert_true(
  identical(embedded_hook$code, source_js),
  "Embedded HUC controller does not match the parsed standalone JavaScript"
)
assert_true(
  length(embedded_hook$data$levels) == length(huc_levels),
  "Embedded HUC controller payload lost level metadata"
)

fixture_display <- list(
  default_visible_overlays = character(0),
  add_huc10 = TRUE,
  add_huc12 = TRUE
)
widget <- leaflet::leaflet()
widget <- pt_add_huc_layer(
  m = widget,
  huc_all = huc_all,
  nm = "huc12",
  map_display = fixture_display
)

methods <- vapply(widget$x$calls, `[[`, character(1), "method")
assert_true(
  identical(methods, c("hideGroup", "addPolygons")),
  paste(
    "HUC registration must hide the group before addPolygons; got:",
    paste(methods, collapse = ", ")
  )
)

polygon_options <- widget$x$calls[[2]]$args[[4]]
assert_true(
  inherits(polygon_options$renderer, "JS_EVAL"),
  "HUC renderer was not serialized as an explicit JavaScript renderer"
)
assert_true(
  grepl("L.canvas", as.character(polygon_options$renderer), fixed = TRUE),
  "HUC path options do not explicitly construct a Canvas renderer"
)
assert_true(
  identical(polygon_options$pane, "pane_huc"),
  "HUC Canvas renderer lost the dedicated pane"
)
assert_true(
  "calls.1.args.3.renderer" %in% htmlwidgets::JSEvals(widget$x),
  "htmlwidgets did not register the HUC renderer for JavaScript evaluation"
)
assert_true(
  identical(widget$x$calls[[2]]$args[[5]], huc_all$huc12$popup_html),
  "HUC polygon registration lost the retained popup binding"
)
assert_true(
  length(widget$x$calls[[2]]$args[[7]]) == nrow(huc_all$huc12),
  "HUC polygon registration lost the hover-label binding"
)

family_widget <- pt_add_huc_layers(
  m = leaflet::leaflet(),
  huc_all = huc_all,
  map_display = fixture_display
)
family_methods <- vapply(family_widget$x$calls, `[[`, character(1), "method")
assert_true(
  identical(
    family_methods,
    rep(c("hideGroup", "addPolygons"), times = length(huc_levels))
  ),
  "One or more HUC levels lost the hidden-before-add lifecycle"
)
family_polygon_calls <- family_widget$x$calls[
  family_methods == "addPolygons"
]
assert_true(
  all(vapply(family_polygon_calls, function(call) {
    inherits(call$args[[4]]$renderer, "JS_EVAL") &&
      grepl("L.canvas", as.character(call$args[[4]]$renderer), fixed = TRUE)
  }, logical(1))),
  "One or more HUC levels lost the explicit Canvas renderer"
)
assert_true(
  length(htmlwidgets::JSEvals(family_widget$x)) == length(huc_levels),
  "One or more HUC Canvas renderers were not marked for embedded evaluation"
)

cat("HUC family R fixture: PASS\n")
