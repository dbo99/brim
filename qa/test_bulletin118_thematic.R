#!/usr/bin/env Rscript

## Deterministic source/fixture checks for Bulletin 118 SGMA enrichment and
## thematic-card preparation. No production cache is read or written.

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  "qa/test_bulletin118_thematic.R"
}
project_root <- normalizePath(
  file.path(dirname(script_path), ".."),
  mustWork = TRUE
)
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

required_packages <- c(
  "dplyr", "htmltools", "htmlwidgets", "leaflet", "sf"
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

source("03_functions/bulletin118_data_helpers.r")
source("03_functions/popup_helpers.r")
source("03_functions/leaflet_layer_local_core_helpers.r")
source("03_functions/leaflet_bulletin118_theme_helpers.r")
source("03_functions/leaflet_layer_local_polygon_helpers.r")

crosswalk_path <- "00_config/bulletin118_sgma_2019_priority_crosswalk.csv"
crosswalk <- pt_read_bulletin118_sgma_crosswalk(crosswalk_path)
assert_true(nrow(crosswalk) == 515L, "Tracked crosswalk lost 515-row contract")
assert_true(
  !anyDuplicated(crosswalk$basin_subbasin_number),
  "Tracked crosswalk contains duplicate keys"
)
assert_true(
  identical(
    unique(crosswalk$source_service),
    PT_BULLETIN118_SGMA_SERVICE
  ) &&
    all(grepl(
      "^\\d{4}-\\d{2}-\\d{2}$",
      crosswalk$source_accessed
    )),
  "Tracked crosswalk lost source URL/access-date provenance"
)

refresh_source <- paste(
  readLines(
    "02_preprocess/68_refresh_bulletin118_sgma_2019_priority.R",
    warn = FALSE
  ),
  collapse = "\n"
)
for (required_refresh_contract in c(
  'REQUIRED_SOURCE_FIELDS <- c(',
  '"OBJECTID"',
  '"Basin_Subbasin_Number"',
  '"Priority"',
  'returnGeometry = "false"',
  'outFields = paste(REQUIRED_SOURCE_FIELDS, collapse = ",")',
  'if ("geometry" %in% names(query$features)'
)) {
  assert_true(
    grepl(required_refresh_contract, refresh_source, fixed = TRUE),
    paste("Refresh script lost contract:", required_refresh_contract)
  )
}

priority_counts <- table(factor(
  crosswalk$sgma_2019_priority,
  levels = PT_BULLETIN118_PRIORITY_LEVELS
))
assert_true(
  identical(
    unname(as.integer(priority_counts)),
    c(46L, 48L, 11L, 410L)
  ),
  "Tracked crosswalk lost official priority counts"
)

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
fixture_geometry <- sf::st_sfc(
  rep(list(fixture_polygon), nrow(crosswalk)),
  crs = 4326
)
fixture_blm_values <- rep(
  c(0, NA, 0.5, 3, 25, 69, 70, 90, 100),
  length.out = nrow(crosswalk)
)

gw <- sf::st_sf(
  basin_num = sub("\\..*$", "", crosswalk$basin_subbasin_number),
  basin_name = paste("Fixture basin", seq_len(nrow(crosswalk))),
  subbasin_name = paste("Fixture subbasin", seq_len(nrow(crosswalk))),
  label = paste(
    crosswalk$basin_subbasin_number,
    paste("Fixture subbasin", seq_len(nrow(crosswalk))),
    sep = " - "
  ),
  subbasin_num = crosswalk$basin_subbasin_number,
  total_area_sqmi = seq_len(nrow(crosswalk)) + 100,
  blm_area_sqmi = seq_len(nrow(crosswalk)) / 10,
  percentBLMland = fixture_blm_values,
  popup_html = paste("legacy popup", seq_len(nrow(crosswalk))),
  geometry = fixture_geometry
)
gw$basin_name[[1]] <- "Indian Wells Valley"
gw$subbasin_name[[1]] <- "Indian Wells Valley"
gw$label[[1]] <- paste(
  gw$subbasin_num[[1]],
  "Indian Wells Valley",
  sep = " - "
)

before <- gw
enriched <- pt_enrich_bulletin118_sgma_2019(gw, crosswalk)
assert_true(nrow(enriched) == nrow(before), "Join changed row count")
assert_true(
  identical(enriched$subbasin_num, before$subbasin_num),
  "Join changed feature order"
)
for (nm in names(before)) {
  assert_true(
    identical(enriched[[nm]], before[[nm]]),
    paste("Join changed existing fixture field:", nm)
  )
}
assert_true(
  identical(sf::st_geometry(enriched), sf::st_geometry(before)),
  "Join changed fixture geometry"
)
assert_true(
  identical(
    as.integer(table(sf::st_geometry_type(enriched))),
    as.integer(table(sf::st_geometry_type(before)))
  ),
  "Join changed fixture geometry-type counts"
)
assert_true(
  sum(sf::st_is_empty(enriched)) == sum(sf::st_is_empty(before)),
  "Join changed empty-geometry count"
)
assert_true(
  sum(sf::st_is_valid(enriched)) == sum(sf::st_is_valid(before)),
  "Join changed valid-geometry count"
)

enriched$popup_html <- pt_make_gw_popups(enriched)
assert_true(
  all(vapply(
    enriched$popup_html,
    function(html) {
      lengths(regmatches(
        html,
        gregexpr(
          "DWR SGMA 2019 priority:",
          html,
          fixed = TRUE
        )
      )) == 1L
    },
    logical(1)
  )),
  "Priority row must appear exactly once in every popup"
)
assert_true(
  all(grepl(
    "DWR SGMA 2019 source",
    enriched$popup_html,
    fixed = TRUE
  )),
  "DWR source link is missing from one or more popups"
)
assert_true(
  all(grepl("%BLM-CA:", enriched$popup_html, fixed = TRUE)) &&
    all(grepl("BLM:", enriched$popup_html, fixed = TRUE)) &&
    all(grepl("Total:", enriched$popup_html, fixed = TRUE)),
  "Existing Bulletin 118 popup area/%BLM rows were lost"
)

known_high <- which(enriched$sgma_2019_priority == "High")[[1]]
assert_true(
  grepl(
    "DWR SGMA 2019 priority:</b> High",
    enriched$popup_html[[known_high]],
    fixed = TRUE
  ),
  "Known High example did not render the correct popup priority"
)

fallback <- enriched[1, , drop = FALSE]
fallback$sgma_2019_priority <- NA_character_
fallback_popup <- pt_make_gw_popups(fallback)[[1]]
assert_true(
  grepl(
    "DWR SGMA 2019 priority:</b> No matched value",
    fallback_popup,
    fixed = TRUE
  ),
  "Popup defensive unmatched fallback is missing"
)

boundary_values <- c(NA, 0, 0.01, 1, 1.01, 5, 5.01, 15, 15.01, 30, 30.01, 50, 50.01)
expected_bins <- c(
  "Missing", "0%", ">0\u20131%", ">0\u20131%", ">1\u20135%", ">1\u20135%",
  ">5\u201315%", ">5\u201315%", ">15\u201330%", ">15\u201330%",
  ">30\u201350%", ">30\u201350%", ">50%"
)
assert_true(
  identical(
    as.character(pt_bulletin118_blm_bin(boundary_values)),
    expected_bins
  ),
  "Fixed %BLM bin boundaries changed"
)

theme_data <- pt_build_bulletin118_theme_data(enriched)
assert_true(
  identical(names(theme_data$themes), c("basins", "sgma_2019", "blm_pct")),
  "Bulletin 118 must expose exactly three themes"
)
assert_true(
  identical(theme_data$default_theme, "basins"),
  "Bulletin 118 default theme must be Basins only"
)
assert_true(
  is.null(theme_data$themes$basins$note) &&
    is.null(theme_data$themes$blm_pct$note),
  "Basins-only and %BLM legends must not retain bottom notes"
)
assert_true(
  identical(
    theme_data$themes$sgma_2019$note,
    paste(
      "Final 2019 DWR categories; counts describe the retained",
      "515-basin snapshot."
    )
  ),
  "DWR source/year note was removed or changed"
)
assert_true(
  length(theme_data$records) == 515L &&
    !anyDuplicated(vapply(
      theme_data$records,
      `[[`,
      character(1),
      "layer_id"
    )),
  "Bulletin 118 theme records are incomplete or non-unique"
)
record_percent_blm <- vapply(
  theme_data$records,
  function(record) {
    if (is.null(record$percent_blm)) NA_real_ else as.numeric(record$percent_blm)
  },
  numeric(1)
)
assert_true(
  identical(record_percent_blm, fixture_blm_values),
  "Browser payload changed retained %BLM values"
)
assert_true(
  identical(theme_data$records[[1]]$basin_name, "Indian Wells Valley") &&
    grepl(
      "Indian Wells Valley",
      theme_data$records[[1]]$display_label,
      fixed = TRUE
    ) &&
    identical(
      theme_data$records[[1]]$subbasin_num,
      gw$subbasin_num[[1]]
    ),
  "Browser search payload lost retained Indian Wells Valley names/code"
)
for (threshold in c(0, 1, 50, 70, 90, 100)) {
  expected_count <- if (threshold == 0) {
    nrow(gw)
  } else {
    sum(!is.na(fixture_blm_values) & fixture_blm_values >= threshold)
  }
  payload_count <- if (threshold == 0) {
    length(record_percent_blm)
  } else {
    sum(!is.na(record_percent_blm) & record_percent_blm >= threshold)
  }
  assert_true(
    identical(payload_count, expected_count),
    paste("Browser %BLM payload count changed at threshold", threshold)
  )
}
assert_true(
  identical(
    unname(PT_BULLETIN118_PRIORITY_COLORS[PT_BULLETIN118_PRIORITY_LEVELS]),
    c("#FF0000", "#FFFF00", "#55FF00", "#0070FF")
  ),
  "Official DWR priority colors changed"
)
assert_true(
  identical(
    unname(PT_BULLETIN118_BLM_COLORS),
    c(
      "#F5F5F5", "#FFF7BC", "#FEE391", "#FEC44F",
      "#FE9929", "#D95F0E", "#993404", "#9E9E9E"
    )
  ),
  "Fixed %BLM palette changed"
)

embedded <- pt_add_bulletin118_theme_controls(
  leaflet::leaflet(),
  enriched
)
embedded_hook <- embedded$jsHooks$render[[1]]
controller_source <- paste(
  readLines(
    "03_functions/js/brim_bulletin118_theme_control.js",
    warn = FALSE
  ),
  collapse = "\n"
)
assert_true(
  identical(embedded_hook$code, controller_source),
  "Embedded Bulletin 118 controller differs from standalone JavaScript"
)
assert_true(
  length(embedded_hook$data$records) == 515L,
  "Embedded thematic payload lost feature records"
)

county <- sf::st_sf(
  county_name = "Fixture",
  popup_html = "Fixture county",
  geometry = sf::st_sfc(fixture_polygon, crs = 4326)
)
widget <- pt_add_county_gw_layers(
  m = leaflet::leaflet(),
  county = county,
  gw = enriched,
  map_display = list(default_visible_overlays = character(0))
)
methods <- vapply(widget$x$calls, `[[`, character(1), "method")
assert_true(
  identical(methods, c("hideGroup", "addPolygons", "addPolygons")),
  paste(
    "Bulletin 118 must hide before polygon registration; got:",
    paste(methods, collapse = ", ")
  )
)
gw_call <- widget$x$calls[[3]]
gw_options <- gw_call$args[[4]]
assert_true(
  inherits(gw_options$renderer, "JS_EVAL") &&
    grepl("L.canvas", as.character(gw_options$renderer), fixed = TRUE),
  "Bulletin 118 path options lost explicit Canvas rendering"
)
assert_true(
  identical(gw_options$pane, "pane_gw"),
  "Bulletin 118 Canvas lost pane_gw"
)
assert_true(
  identical(gw_options$className, "pt-bulletin118-feature"),
  "Bulletin 118 feature class changed"
)
assert_true(
  identical(gw_call$args[[5]], enriched$popup_html),
  "Bulletin 118 polygon registration lost popup binding"
)
assert_true(
  length(gw_call$args[[2]]) == 515L &&
    !anyDuplicated(gw_call$args[[2]]),
  "Bulletin 118 polygon layer IDs are incomplete or non-unique"
)

controller_text <- readLines(
  "03_functions/js/brim_bulletin118_theme_control.js",
  warn = FALSE
)
controller_text <- paste(controller_text, collapse = "\n")
for (required_text in c(
  "Basins only",
  "DWR SGMA 2019 Basin Prioritization",
  "BLM-managed land \\u2014 %",
  "pt-bulletin118-theme-card",
  "pt:measureinteractionchange",
  "overlayadd",
  "overlayremove",
  "cancelStyleWork",
  "makeDetachable",
  "Minimum BLM-managed land",
  'min="0" max="100" step="1" value="0"',
  "Find basin or subbasin\\u2026",
  "groupRoot.addLayer",
  "groupRoot.removeLayer",
  "fitBounds",
  "openPopup"
)) {
  assert_true(
    grepl(required_text, controller_text, fixed = TRUE),
    paste("Controller source lost contract:", required_text)
  )
}
assert_true(
  !grepl("map.eachLayer", controller_text, fixed = TRUE),
  "Bulletin 118 controller must not use a map-wide layer scan"
)
assert_true(
  !grepl(
    "fillOpacity\\s*:\\s*0\\s*[,}]",
    controller_text,
    perl = TRUE
  ),
  "Bulletin 118 filtering must remove polygons instead of hiding Canvas paths"
)

cat("Bulletin 118 thematic R fixture: PASS\n")
