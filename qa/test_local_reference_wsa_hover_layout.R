#!/usr/bin/env Rscript

source("00_config/config_local_reference_interactions.r")
source("03_functions/local_reference_interaction_helpers.r")

fixture <- utils::read.csv(
  file.path("qa", "fixtures", "local_reference_wsa_hover_edge_cases.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = character()
)
stopifnot(nrow(fixture) == 8L)

stopifnot(identical(
  pt_local_reference_format_square_miles_from_acres(c(
    47510.7, 307.2, 23.04, 0, NA, "", "not available"
  )),
  c("~74.2 mi²", "~0.48 mi²", "~0.036 mi²", "", "", "", "")
))

hover <- pt_local_reference_wsa_hover_text(
  name = fixture$name,
  flpma_section = fixture$flpma_section,
  recommendation_label = fixture$recommendation_label,
  source_gis_acres = fixture$source_gis_acres,
  managing_office = fixture$managing_office
)
hover_html <- pt_local_reference_wsa_hover_html(hover)
lines <- strsplit(hover, "\n", fixed = TRUE)
line_count <- lengths(lines)
stopifnot(identical(line_count, fixture$expected_semantic_lines))
stopifnot(all(line_count <= 3L))
stopifnot(all(!grepl("\n", hover_html, fixed = TRUE)))
stopifnot(all(!grepl("<br", hover_html, fixed = TRUE)))
stopifnot(all(!grepl("&lt;br", hover_html, fixed = TRUE)))
stopifnot(!any(grepl("GIS area|GIS acre", hover, ignore.case = TRUE)))
stopifnot(all(grepl('<div class="pt-wsa-hover-lines">', hover_html, fixed = TRUE)))
stopifnot(identical(
  lengths(regmatches(
    hover_html,
    gregexpr('<div class="pt-wsa-hover-line ', hover_html, fixed = TRUE)
  )),
  line_count
))
stopifnot(all(vapply(seq_along(lines), function(i) {
  identical(lines[[i]][[1]], fixture$name[[i]])
}, logical(1))))

dual_index <- match("two_verified_offices", fixture$case_id)
stopifnot(identical(
  lines[[dual_index]][[3]],
  paste0("BLM office: ", fixture$expected_compact_office[[dual_index]])
))
stopifnot(!grepl("Field Office", lines[[dual_index]][[3]], fixed = TRUE))
stopifnot(identical(
  lines[[dual_index]][[2]],
  "FLPMA §603 · Recommended non-suitable · ~74.2 mi²"
))

wrapping_index <- match("long_word_wrapping", fixture$case_id)
stopifnot(!isTRUE(fixture$unbroken_name[[wrapping_index]]))
stopifnot(nchar(lines[[wrapping_index]][[1]]) > 70L)

small_index <- match("small_area", fixture$case_id)
very_small_index <- match("very_small_area", fixture$case_id)
zero_index <- match("narrow_viewport", fixture$case_id)
null_index <- match("null_area", fixture$case_id)
nonnumeric_index <- match("nonnumeric_area", fixture$case_id)
stopifnot(grepl("~0.48 mi²", lines[[small_index]][[2]], fixed = TRUE))
stopifnot(grepl("~0.036 mi²", lines[[very_small_index]][[2]], fixed = TRUE))
stopifnot(!grepl("mi²", lines[[zero_index]][[2]], fixed = TRUE))
stopifnot(!grepl("mi²", lines[[null_index]][[2]], fixed = TRUE))
stopifnot(!grepl("mi²", lines[[nonnumeric_index]][[2]], fixed = TRUE))

injection_html <- pt_local_reference_wsa_hover_html(
  "<img src=x onerror=alert(1)> Wilderness Study Area\nFLPMA §603 · Recommended suitable · ~0.00156 mi²"
)
stopifnot(!grepl("<img", injection_html, fixed = TRUE))
stopifnot(grepl("&lt;img", injection_html, fixed = TRUE))

controller_source <- paste(
  readLines(
    file.path("03_functions", "js", "brim_local_reference_controller.js"),
    warn = FALSE
  ),
  collapse = "\n"
)
drawing_source <- paste(
  readLines(
    file.path("03_functions", "leaflet_layer_local_reference_helpers.r"),
    warn = FALSE
  ),
  collapse = "\n"
)

stopifnot(grepl(".leaflet-tooltip.pt-wsa-hover-tooltip", controller_source, fixed = TRUE))
stopifnot(grepl("white-space:normal!important", controller_source, fixed = TRUE))
stopifnot(grepl("width:fit-content!important", controller_source, fixed = TRUE))
stopifnot(grepl(
  "min-width:min(220px,calc(100vw - 32px))!important",
  controller_source,
  fixed = TRUE
))
stopifnot(grepl(
  "max-width:min(320px,calc(100vw - 32px))!important",
  controller_source,
  fixed = TRUE
))
stopifnot(grepl("overflow-wrap:break-word!important", controller_source, fixed = TRUE))
stopifnot(!grepl("overflow-wrap:anywhere", controller_source, fixed = TRUE))
stopifnot(grepl("word-break:normal!important", controller_source, fixed = TRUE))
stopifnot(grepl("line-height:1.3!important", controller_source, fixed = TRUE))
stopifnot(grepl(".pt-wsa-hover-line{display:block", controller_source, fixed = TRUE))
stopifnot(grepl(
  "@media (pointer:coarse){.leaflet-tooltip.pt-wsa-hover-tooltip,.leaflet-tooltip.pt-trails-hover-tooltip{display:none!important}",
  controller_source,
  fixed = TRUE
))
stopifnot(
  grepl('"pt-wsa-hover-tooltip"', drawing_source, fixed = TRUE),
  grepl('identical(nm, "fedwilderness")', drawing_source, fixed = TRUE)
)
stopifnot(grepl(
  'lapply(x$pt_reference_hover_html, htmltools::HTML)',
  drawing_source,
  fixed = TRUE
))
stopifnot(grepl(
  'className = "pt-local-reference-tabbed-popup"',
  drawing_source,
  fixed = TRUE
))
stopifnot(grepl(
  'autoPan = TRUE',
  drawing_source,
  fixed = TRUE
))
stopifnot(grepl(
  'keepInView = TRUE',
  drawing_source,
  fixed = TRUE
))
stopifnot(grepl(
  'autoPanPaddingTopLeft = c(16, 84)',
  drawing_source,
  fixed = TRUE
))
stopifnot(grepl(
  'autoPanPaddingBottomRight = c(16, 24)',
  drawing_source,
  fixed = TRUE
))
stopifnot(!grepl(
  'popupOptions = leaflet::popupOptions(autoPan = FALSE)',
  drawing_source,
  fixed = TRUE
))
stopifnot(grepl(
  '(?s)leaflet::addPolylines\\(.{1,500}label = ~pt_reference_hover_text',
  drawing_source,
  perl = TRUE
))
stopifnot(grepl('"white-space" = "normal"', drawing_source, fixed = TRUE))
stopifnot(grepl('"width" = "fit-content"', drawing_source, fixed = TRUE))
stopifnot(grepl(
  '"min-width" = "min(220px, calc(100vw - 32px))"',
  drawing_source,
  fixed = TRUE
))
stopifnot(grepl(
  '"max-width" = "min(320px, calc(100vw - 32px))"',
  drawing_source,
  fixed = TRUE
))
stopifnot(grepl('"overflow-wrap" = "break-word"', drawing_source, fixed = TRUE))
stopifnot(grepl('"word-break" = "normal"', drawing_source, fixed = TRUE))
stopifnot(grepl("popupopen", controller_source, fixed = TRUE))
stopifnot(grepl("closeTooltip", controller_source, fixed = TRUE))

message("Local Reference WSA hover layout contract tests passed.")
