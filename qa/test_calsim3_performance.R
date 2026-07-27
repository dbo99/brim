#!/usr/bin/env Rscript

## Deterministic source fixture for CalSim3 rendering and declustering.
##
## This does not claim production browser timing. It verifies:
##   - exact retained-coordinate reconciliation;
##   - direct zoom-9 declustering when no exact duplicates exist;
##   - the exact-coordinate-only fallback when duplicates do exist;
##   - hidden-group registration before line/point creation;
##   - browser-only unique layer keys and one shared pane Canvas renderer;
##   - compact Explorer indexes using explicit retained endpoint fields;
##   - retained popup/hover bindings and stable cluster identity; and
##   - retained-marker zoom-radius tiers and distinct Live apply semantics; and
##   - shared inline/Explorer labels with bounded viewport rendering; and
##   - controller metadata needed for browser reconciliation.

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  "qa/test_calsim3_performance.R"
}
project_root <- normalizePath(
  file.path(dirname(script_path), ".."),
  mustWork = TRUE
)
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

required_packages <- c(
  "dplyr", "htmlwidgets", "jsonlite", "leaflet", "sf"
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

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
})

source("03_functions/leaflet_layer_local_core_helpers.r")
source("03_functions/popup_helpers.r")
source("03_functions/leaflet_layer_local_reference_helpers.r")

fixture_display <- list(
  add_calsim3_arcs = TRUE,
  add_calsim3_nodes = TRUE,
  add_labels = TRUE,
  default_visible_overlays = character(0)
)

arc_geometry <- sf::st_sfc(
  sf::st_linestring(matrix(
    c(-121, 37, -120.5, 37.5, -120, 38),
    ncol = 2,
    byrow = TRUE
  )),
  sf::st_linestring(matrix(
    c(-120, 36, -119.5, 36.5),
    ncol = 2,
    byrow = TRUE
  )),
  crs = 4326
)
arcs <- sf::st_sf(
  Name = c("Fixture channel", "Fixture return"),
  Arc_ID = c("C_FIXTURE", "R_FIXTURE"),
  Type = c("Channel", "Return"),
  FromNode = c("NODE_A", "NODE_B"),
  ToNode = c("NODE_B", "NODE_C"),
  line_col = c("#1F78B4", "#33A02C"),
  line_weight = c(1.4, 1.0),
  popup_html = c("<b>Fixture channel</b>", "<b>Fixture return</b>"),
  hover_text = c("Fixture channel\nType: Channel", "Fixture return\nType: Return"),
  geometry = arc_geometry
)

make_nodes <- function(with_exact_duplicate = FALSE) {
  node_df <- data.frame(
    node_id_display = c("NODE_A", "NODE_B", "NODE_C", "NODE_D"),
    node_description = c(
      "conveyance-channel",
      "storage-reservoir",
      "demand-agricultural-surface-water",
      "return-flow"
    ),
    riv_name_display = c("Fixture River", "", "", ""),
    comment_display = c("Fixture comment", "", "", ""),
    calsim3_node_group = c(
      "Conveyance",
      "Storage / Reservoir",
      "Project demand – ag",
      "Return flow"
    ),
    node_fill_col = c("#1F78B4", "#6A3D9A", "#D95F02", "#33A02C"),
    node_stroke_col = c("#08519C", "#3F007D", "#7F3B08", "#1B7837"),
    node_radius = c(4.5, 5.5, 5.0, 4.5),
    popup_html = paste0("<b>NODE_", LETTERS[1:4], "</b><br/>fixture popup"),
    hover_text = paste0("NODE_", LETTERS[1:4], "\nfixture hover"),
    x = c(-121, -120.5, -120, -119.5),
    y = c(37, 37.5, 38, 38.5),
    stringsAsFactors = FALSE
  )
  if (with_exact_duplicate) {
    node_df$x[[2]] <- node_df$x[[1]]
    node_df$y[[2]] <- node_df$y[[1]]
  }
  sf::st_as_sf(node_df, coords = c("x", "y"), crs = 4326)
}

unique_nodes <- make_nodes(FALSE)
duplicate_nodes <- make_nodes(TRUE)

arc_popup_fixture <- pt_make_calsim3_arc_popups(arcs)
node_popup_fixture <- pt_make_calsim3_node_popups(unique_nodes)
assert_true(
  length(arc_popup_fixture) == nrow(arcs) &&
    all(grepl("Arc:", arc_popup_fixture, fixed = TRUE)) &&
    all(grepl("Type:", arc_popup_fixture, fixed = TRUE)),
  "CalSim3 arc popup fields were not preserved"
)
assert_true(
  length(node_popup_fixture) == nrow(unique_nodes) &&
    all(grepl("Node group:", node_popup_fixture, fixed = TRUE)) &&
    all(grepl("Node description:", node_popup_fixture, fixed = TRUE)) &&
    all(grepl(
      "https://data.cnra.ca.gov/dataset/calsim-3",
      node_popup_fixture,
      fixed = TRUE
    )),
  "CalSim3 node popup fields or documentation links were not preserved"
)

unique_metrics <- pt_calsim3_node_coordinate_metrics(unique_nodes)
duplicate_metrics <- pt_calsim3_node_coordinate_metrics(duplicate_nodes)

assert_true(
  unique_metrics$analyticalRecordCount == nrow(unique_nodes) &&
    unique_metrics$validCoordinateCount == nrow(unique_nodes) &&
    unique_metrics$uniqueExactCoordinateCount == nrow(unique_nodes) &&
    unique_metrics$exactDuplicateLocationCount == 0L,
  "Unique-node coordinate metrics did not reconcile"
)
assert_true(
  duplicate_metrics$analyticalRecordCount == nrow(duplicate_nodes) &&
    duplicate_metrics$uniqueExactCoordinateCount == 3L &&
    duplicate_metrics$exactDuplicateLocationCount == 1L &&
    duplicate_metrics$recordsAtExactDuplicateLocations == 2L &&
    duplicate_metrics$maxRecordsAtExactLocation == 2L,
  "Exact duplicate-coordinate metrics did not preserve every record"
)

arc_widget <- pt_add_calsim3_arc_layer(
  m = leaflet::leaflet(),
  calsim3_arcs = arcs,
  map_display = fixture_display
)
arc_methods <- vapply(arc_widget$x$calls, `[[`, character(1), "method")
assert_true(
  identical(arc_methods, c("hideGroup", "addPolylines")),
  "CalSim3 arcs must hide the group before addPolylines"
)
arc_call <- arc_widget$x$calls[[2]]
arc_options <- arc_call$args[[4]]
assert_true(
  identical(arc_options$pane, PT_CALSIM3_PANE) &&
    isTRUE(arc_options$interactive) &&
    is.null(arc_options$renderer),
  "CalSim3 arcs lost the shared interactive CalSim pane contract"
)
assert_true(
  identical(
    arc_call$args[[2]],
    pt_calsim3_browser_layer_ids("arc", nrow(arcs))
  ) &&
    length(unique(arc_call$args[[2]])) == nrow(arcs),
  "CalSim3 arcs lost their unique browser-only layer keys"
)
assert_true(
  identical(arc_call$args[[5]], arcs$popup_html) &&
    identical(arc_call$args[[7]], arcs$hover_text),
  "CalSim3 arc popup or hover bindings changed"
)

unique_widget <- pt_add_calsim3_node_layer(
  m = leaflet::leaflet(),
  calsim3_nodes = unique_nodes,
  map_display = fixture_display
)
unique_methods <- vapply(
  unique_widget$x$calls,
  `[[`,
  character(1),
  "method"
)
assert_true(
  identical(unique_methods, c("hideGroup", "addCircleMarkers")),
  "CalSim3 nodes must hide the group before addCircleMarkers"
)

unique_call <- unique_widget$x$calls[[2]]
unique_options <- unique_call$args[[6]]
unique_cluster_options <- unique_call$args[[7]]
assert_true(
  identical(unique_options$pane, PT_CALSIM3_PANE) &&
    isTRUE(unique_options$interactive) &&
    is.null(unique_options$renderer) &&
    identical(unique_options$pane, arc_options$pane),
  "CalSim3 nodes lost the shared interactive CalSim pane contract"
)
assert_true(
  identical(
    unique_call$args[[4]],
    pt_calsim3_browser_layer_ids("node", nrow(unique_nodes))
  ) &&
    length(unique(unique_call$args[[4]])) == nrow(unique_nodes),
  "CalSim3 nodes lost their unique browser-only layer keys"
)
assert_true(
  identical(
    unique_cluster_options$disableClusteringAtZoom,
    PT_CALSIM3_DECLUSTER_ZOOM
  ) &&
    identical(unique_cluster_options$maxClusterRadius, 55) &&
    identical(unique_cluster_options$animate, FALSE) &&
    identical(unique_cluster_options$animateAddingMarkers, FALSE),
  "Unique CalSim3 nodes are not configured for controlled zoom-9 declustering"
)
assert_true(
  identical(unique_call$args[[8]], PT_CALSIM3_CLUSTER_ID),
  "CalSim3 cluster identity changed"
)
assert_true(
  identical(unique_call$args[[9]], unique_nodes$popup_html) &&
    identical(unique_call$args[[11]], unique_nodes$hover_text),
  "CalSim3 node popup or hover bindings changed"
)

label_widget <- pt_add_calsim3_label_companion(
  m = leaflet::leaflet(),
  map_display = fixture_display
)
label_methods <- vapply(
  label_widget$x$calls,
  `[[`,
  character(1),
  "method"
)
assert_true(
  identical(label_methods, c("addCircleMarkers", "hideGroup")),
  "CalSim3 label companion must register once and default off"
)
label_call <- label_widget$x$calls[[1]]
assert_true(
  identical(label_call$args[[4]], "pt_calsim3_label_dummy") &&
    identical(label_call$args[[5]], pt_calsim3_label_group_name()) &&
    identical(label_call$args[[6]]$pane, "pane_labels_pts") &&
    identical(label_call$args[[6]]$interactive, FALSE) &&
    identical(
      label_widget$x$calls[[2]]$args[[1]],
      pt_calsim3_label_group_name()
    ),
  "CalSim3 label companion lost its hidden noninteractive shared-label contract"
)

duplicate_widget <- pt_add_calsim3_node_layer(
  m = leaflet::leaflet(),
  calsim3_nodes = duplicate_nodes,
  map_display = fixture_display
)
duplicate_cluster_options <- duplicate_widget$x$calls[[2]]$args[[7]]
assert_true(
  length(duplicate_widget$x$calls[[2]]$args[[1]]) == nrow(duplicate_nodes) &&
    length(duplicate_widget$x$calls[[2]]$args[[9]]) == nrow(duplicate_nodes),
  "Exact-coordinate fallback lost analytical markers or popup records"
)
assert_true(
  is.null(duplicate_cluster_options$disableClusteringAtZoom) &&
    identical(duplicate_cluster_options$zoomToBoundsOnClick, FALSE) &&
    identical(duplicate_cluster_options$spiderfyOnMaxZoom, FALSE) &&
    inherits(duplicate_cluster_options$maxClusterRadius, "JS_EVAL") &&
    grepl(
      "zoom < 9 ? 55 : 0.00000100",
      as.character(duplicate_cluster_options$maxClusterRadius),
      fixed = TRUE
    ),
  "Exact-coordinate CalSim3 fallback does not isolate same-location groups at zoom 9"
)

controller_widget <- pt_add_calsim3_cluster_controller(
  m = duplicate_widget,
  calsim3_arcs = arcs,
  calsim3_nodes = duplicate_nodes,
  map_display = fixture_display
)
assert_true(
  length(controller_widget$jsHooks$render) == 1L,
  "CalSim3 browser controller was not installed exactly once"
)
controller_hook <- controller_widget$jsHooks$render[[1]]
controller_data <- controller_hook$data
assert_true(
  controller_data$arcRecordCount == nrow(arcs) &&
    identical(controller_data$paneName, PT_CALSIM3_PANE) &&
    identical(
      controller_data$canvasTolerance,
      PT_CALSIM3_CANVAS_TOLERANCE
    ) &&
    identical(
      controller_data$labelGroupName,
      pt_calsim3_label_group_name()
    ) &&
    identical(controller_data$labelMinZoom, 11L) &&
    identical(controller_data$labelCap, 160L) &&
    identical(controller_data$labelNodeCap, 96L) &&
    identical(controller_data$labelArcCap, 64L) &&
    identical(controller_data$labelGridDegrees, 0.25) &&
    identical(controller_data$labelViewportPadRatio, 0.15) &&
    controller_data$analyticalRecordCount == nrow(duplicate_nodes) &&
    controller_data$validCoordinateCount == nrow(duplicate_nodes) &&
    controller_data$uniqueExactCoordinateCount == 3L &&
    controller_data$recordsAtExactDuplicateLocations == 2L &&
    identical(controller_data$transitionZoom, 9L) &&
    identical(controller_data$sameLocationMode, "exact-coordinate-spiderfy"),
  "CalSim3 controller reconciliation metadata is incomplete"
)
assert_true(
  is.data.frame(controller_data$arcRecords) &&
    nrow(controller_data$arcRecords) == nrow(arcs) &&
    identical(
      names(controller_data$arcRecords),
      c(
        "lid", "id", "fromNode", "toNode", "name", "type",
        "color", "weight"
      )
    ) &&
    identical(controller_data$arcRecords$id, arcs$Arc_ID) &&
    identical(controller_data$arcRecords$fromNode, arcs$FromNode) &&
    identical(controller_data$arcRecords$toNode, arcs$ToNode) &&
    identical(controller_data$arcRecords$name, arcs$Name) &&
    identical(controller_data$arcRecords$type, arcs$Type) &&
    length(unique(controller_data$arcRecords$lid)) == nrow(arcs),
  paste(
    "CalSim3 arc Explorer index must retain explicit IDs, endpoints, names,",
    "types, and one unique browser key per analytical row"
  )
)
assert_true(
  is.data.frame(controller_data$nodeRecords) &&
    nrow(controller_data$nodeRecords) == nrow(unique_nodes) &&
    identical(
      names(controller_data$nodeRecords),
      c(
        "lid", "id", "description", "river", "comment", "group",
        "fill", "stroke", "radius"
      )
    ) &&
    identical(
      controller_data$nodeRecords$id,
      duplicate_nodes$node_id_display
    ) &&
    identical(
      controller_data$nodeRecords$description,
      duplicate_nodes$node_description
    ) &&
    identical(
      controller_data$nodeRecords$river,
      duplicate_nodes$riv_name_display
    ) &&
    identical(
      controller_data$nodeRecords$comment,
      duplicate_nodes$comment_display
    ) &&
    length(unique(controller_data$nodeRecords$lid)) ==
      nrow(duplicate_nodes),
  "CalSim3 node Explorer index is incomplete or changed retained semantics"
)
assert_true(
  identical(controller_data$arcTypeOrder, c("Channel", "Return")) &&
    identical(
      controller_data$nodeGroupOrder,
      c(
        "Conveyance",
        "Storage / Reservoir",
        "Project demand – ag",
        "Return flow"
      )
    ),
  "CalSim3 Explorer filter classes lost their stable retained-data order"
)
assert_true(
    grepl("BRIM_CALSIM3_LOCAL", controller_hook$code, fixed = TRUE) &&
    grepl("shared-arcs-nodes", controller_hook$code, fixed = TRUE) &&
    grepl("renderer.options.tolerance", controller_hook$code, fixed = TRUE) &&
    grepl(
      "sharedPaneRenderer._container.style.pointerEvents = 'none'",
      controller_hook$code,
      fixed = TRUE
    ) &&
    grepl("mapEventIsBackground", controller_hook$code, fixed = TRUE) &&
    grepl("globalInteractionState", controller_hook$code, fixed = TRUE) &&
    grepl("pt:measureinteractionchange", controller_hook$code, fixed = TRUE) &&
    grepl("overlayremove", controller_hook$code, fixed = TRUE) &&
    grepl("CalSim3.0 Network Explorer", controller_hook$code, fixed = TRUE) &&
    grepl("nodeRadiusTierForZoom", controller_hook$code, fixed = TRUE) &&
    grepl("scale: 0.58", controller_hook$code, fixed = TRUE) &&
    grepl("scale: 0.76", controller_hook$code, fixed = TRUE) &&
    grepl("record.layer.setRadius(radius)", controller_hook$code, fixed = TRUE) &&
    grepl("listen(map, 'zoomend'", controller_hook$code, fixed = TRUE) &&
    grepl("pt-calsim3-live-apply", controller_hook$code, fixed = TRUE) &&
    grepl("pt-calsim3-labels", controller_hook$code, fixed = TRUE) &&
    grepl("buildLabelIndex", controller_hook$code, fixed = TRUE) &&
    grepl("viewportLabelCandidates", controller_hook$code, fixed = TRUE) &&
    grepl("scheduleLabelRender", controller_hook$code, fixed = TRUE) &&
    grepl("pane_labels_pts", controller_hook$code, fixed = TRUE) &&
    grepl("pane_labels_poly", controller_hook$code, fixed = TRUE) &&
    grepl("interactive: false", controller_hook$code, fixed = TRUE) &&
    grepl("checked> Live apply", controller_hook$code, fixed = TRUE) &&
    grepl(
      "Hover controls tooltips only; click popups remain available.",
      controller_hook$code,
      fixed = TRUE
    ) &&
    grepl("record.fromNode", controller_hook$code, fixed = TRUE) &&
    grepl("record.toNode", controller_hook$code, fixed = TRUE),
  paste(
    "CalSim3 controller lost pass-through interaction routing, diagnostics,",
    "Measure, layer-off, radius-tier, Live apply, shared-label, or popup wiring"
  )
)

core_source <- paste(
  readLines("03_functions/leaflet_core_helpers.r", warn = FALSE),
  collapse = "\n"
)
label_config_source <- paste(
  readLines("00_config/config_labels.r", warn = FALSE),
  collapse = "\n"
)
map_build_source <- paste(
  readLines(
    "05_map_build/04_build_portatreasure2_core_map.r",
    warn = FALSE
  ),
  collapse = "\n"
)
assert_true(
  grepl(
    "{main: 'CalSim3.0', label: 'CalSim3.0'}",
    core_source,
    fixed = TRUE
  ) &&
    length(gregexpr(
      "\"CalSim3.0\"",
      label_config_source,
      fixed = TRUE
    )[[1]]) >= 2L &&
    grepl("\"Labels: CalSim3.0\"", map_build_source, fixed = TRUE) &&
    grepl("pt_add_calsim3_label_companion(", map_build_source, fixed = TRUE),
  "CalSim is missing its shared Local-row label registration or map wiring"
)

bad_nodes <- duplicate_nodes
sf::st_geometry(bad_nodes)[[1]] <- sf::st_point(c(NA_real_, 37))
bad_result <- try(
  pt_calsim3_node_coordinate_metrics(bad_nodes),
  silent = TRUE
)
assert_true(
  inherits(bad_result, "try-error"),
  "CalSim3 coordinate QA must refuse to silently omit invalid records"
)

cat("CalSim3 performance R fixture: PASS\n")
