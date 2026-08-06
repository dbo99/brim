#!/usr/bin/env Rscript

read_source <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

controller_path <- file.path(
  "03_functions", "js", "brim_local_reference_controller.js"
)
wsa_fixture_path <- file.path(
  "qa", "fixtures", "local_reference_wsa_popup_dom.html"
)
trails_fixture_path <- file.path(
  "qa", "fixtures", "local_reference_trails_popup_dom.html"
)

stopifnot(all(file.exists(c(
  controller_path, wsa_fixture_path, trails_fixture_path
))))

controller <- read_source(controller_path)
wsa_fixture <- read_source(wsa_fixture_path)
trails_fixture <- read_source(trails_fixture_path)

## The htmlwidgets callback must remain syntactically valid JavaScript.
wrapped_controller <- paste0("(", controller, ");")
controller_check <- tempfile(fileext = ".js")
writeLines(wrapped_controller, controller_check, useBytes = TRUE)
on.exit(unlink(controller_check), add = TRUE)
node_status <- system2("node", c("--check", controller_check))
stopifnot(identical(node_status, 0L))

stopifnot(
  grepl(
    ".pt-lr-popup-panel-scroll{height:var(--pt-lr-popup-panel-height,auto)",
    controller,
    fixed = TRUE
  ),
  grepl("max-height:min(54vh,450px)", controller, fixed = TRUE),
  grepl("max-height:min(50vh,390px)", controller, fixed = TRUE),
  !grepl(
    ".pt-lr-popup-panel-scroll{height:min(",
    controller,
    fixed = TRUE
  ),
  grepl("var floor = Math.min(112, cap)", controller, fixed = TRUE),
  grepl("Math.min(naturalHeight + 1, cap)", controller, fixed = TRUE),
  grepl("tallestNaturalPanelHeight", controller, fixed = TRUE),
  grepl("pt-lr-popup-measuring", controller, fixed = TRUE),
  grepl("panelStates", controller, fixed = TRUE),
  grepl("window.ResizeObserver", controller, fixed = TRUE),
  grepl("document.fonts.ready", controller, fixed = TRUE),
  grepl("scheduleTabbedPopupLayout(state, 80)", controller, fixed = TRUE),
  grepl("onTabbedPopupDetailsToggle", controller, fixed = TRUE),
  grepl("listenDom(el, 'toggle', onTabbedPopupDetailsToggle, true)", controller, fixed = TRUE),
  grepl("state.popup._updateLayout", controller, fixed = TRUE),
  grepl("state.popup._updatePosition", controller, fixed = TRUE),
  grepl("state.popup._adjustPan", controller, fixed = TRUE),
  !grepl("cloneNode(true)", controller, fixed = TRUE)
)

stopifnot(
  grepl("Wall Canyon Wilderness Study Area", wsa_fixture, fixed = TRUE),
  grepl("Buffalo Hills Wilderness Study Area", wsa_fixture, fixed = TRUE),
  grepl("Red Mountain", wsa_fixture, fixed = TRUE),
  grepl("wall_canyon_all_panels_fit_without_scroll", wsa_fixture, fixed = TRUE),
  grepl("stable_height_across_tabs", wsa_fixture, fixed = TRUE),
  grepl("map_center_zoom_stable_across_tabs", wsa_fixture, fixed = TRUE),
  grepl("resize_recalculated", wsa_fixture, fixed = TRUE),
  grepl("empty_tabs_suppressed", wsa_fixture, fixed = TRUE),
  grepl("no_layout_flash", wsa_fixture, fixed = TRUE),
  grepl("no_temporary_measurement_state", wsa_fixture, fixed = TRUE),
  grepl("desktop_disclosure_cycles", wsa_fixture, fixed = TRUE),
  grepl("narrow_disclosure_cycles", wsa_fixture, fixed = TRUE),
  grepl("component_counts_across_10_cycles", wsa_fixture, fixed = TRUE),
  grepl("layer_off_zero_owned_objects", wsa_fixture, fixed = TRUE),
  grepl("async_errors", wsa_fixture, fixed = TRUE),
  grepl("short_trail_tracks_content", trails_fixture, fixed = TRUE),
  grepl("long_trail_capped", trails_fixture, fixed = TRUE),
  grepl("resources_internal_scroll", trails_fixture, fixed = TRUE),
  grepl("long_tab_and_sticky_header_inside_viewport", trails_fixture, fixed = TRUE),
  grepl("desktop_disclosure_cycles", trails_fixture, fixed = TRUE),
  grepl("narrow_disclosure_cycles", trails_fixture, fixed = TRUE),
  grepl("component_counts_across_10_cycles", trails_fixture, fixed = TRUE),
  grepl("layer_off_zero_owned_objects", trails_fixture, fixed = TRUE),
  grepl("async_errors", trails_fixture, fixed = TRUE)
)

stopifnot(
  !grepl(paste0("/", "Users", "/"), controller, fixed = TRUE),
  !grepl(paste0("/", "Users", "/"), wsa_fixture, fixed = TRUE),
  !grepl(paste0("/", "Users", "/"), trails_fixture, fixed = TRUE)
)

message("Local Reference shared popup-height source/fixture tests passed.")
