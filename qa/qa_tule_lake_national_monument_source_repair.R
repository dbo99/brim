#!/usr/bin/env Rscript

# Focused, non-promoting QA for the Tule Lake National Monument source repair.
# This script validates the two agency geometry records, three authoritative
# constituent parts, and one semantic label identity. It never mutates caches.

source("00_config/config_paths.r")
source("00_config/config_local_reference_interactions.r")
source("00_config/config_labels.r")
source("03_functions/spatial_helpers.r")
source("03_functions/label_helpers.r")

suppressPackageStartupMessages({
  library(sf)
})

required_packages <- c("digest", "htmlwidgets", "jsonlite", "leaflet")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing focused Tule Lake QA packages: ", paste(missing_packages, collapse = ", "))
}

parse_args <- function(arguments) {
  values <- list(candidate = NULL, component_qa = NULL, output_dir = NULL)
  index <- 1L
  while (index <= length(arguments)) {
    key <- arguments[[index]]
    if (!key %in% c("--candidate", "--component-qa", "--output-dir")) {
      stop("Unknown argument: ", key)
    }
    if (index == length(arguments)) stop("Missing value for ", key)
    values[[gsub("-", "_", sub("^--", "", key))]] <- arguments[[index + 1L]]
    index <- index + 2L
  }
  if (any(vapply(values, is.null, logical(1)))) {
    stop("--candidate, --component-qa, and --output-dir are required")
  }
  values
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
candidate_path <- normalizePath(args$candidate, mustWork = TRUE)
component_qa_path <- normalizePath(args$component_qa, mustWork = TRUE)
output_dir <- normalizePath(args$output_dir, mustWork = TRUE)

output_paths <- c(
  labels = file.path(output_dir, "tule_lake_semantic_label_qa.csv"),
  summary = file.path(output_dir, "tule_lake_source_repair_qa_summary.json"),
  map = file.path(output_dir, "tule_lake_source_repair_qa_map.html"),
  map_png = file.path(output_dir, "tule_lake_source_repair_qa_map.png")
)
existing <- output_paths[file.exists(output_paths)]
if (length(existing)) stop("Refusing to overwrite focused QA output: ", paste(existing, collapse = ", "))

candidate <- readRDS(candidate_path)
component_qa <- utils::read.csv(
  component_qa_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
tule <- candidate[candidate$monument_id == "nm_ca_tule_lake", , drop = FALSE]
if (!inherits(tule, "sf") || nrow(tule) != 2L ||
    !setequal(tule$source_key, c("nps_current", "fws_tule_lake")) ||
    !setequal(tule$source_agency, c("NPS", "USFWS")) ||
    !setequal(tule$display_agency_key, c("nps", "fws")) ||
    any(tule$geometry_role != "agency_component_primary") ||
    any(sf::st_is_empty(tule)) || any(!sf::st_is_valid(tule))) {
  stop("Tule Lake candidate does not contain the exact valid NPS + USFWS component contract.")
}

required_component_columns <- c(
  "constituent_key", "administering_agency", "source_key",
  "raw_polygon_parts", "repaired_polygon_parts", "display_polygon_parts",
  "raw_vertices", "repaired_vertices", "display_vertices",
  "repaired_acres_epsg3310", "raw_valid", "repaired_valid", "display_valid",
  "simplification_part_count_changed", "simplification_hole_count_changed"
)
if (nrow(component_qa) != 3L ||
    length(setdiff(required_component_columns, names(component_qa))) ||
    !setequal(
      component_qa$constituent_key,
      c("segregation_center", "peninsula_castle_rock", "camp_tulelake")
    ) ||
    !setequal(component_qa$administering_agency, c("NPS", "USFWS")) ||
    any(!component_qa$raw_valid) || any(!component_qa$repaired_valid) ||
    any(!component_qa$display_valid) ||
    any(component_qa$simplification_part_count_changed) ||
    any(component_qa$simplification_hole_count_changed)) {
  stop("Tule Lake three-constituent geometry QA contract failed.")
}

label_source <- candidate
label_source$pt_local_reference_semantic_key <- label_source$monument_id
label_source$pt_local_reference_geometry_key <- label_source$component_id
label_source$pt_reference_label_text <- label_source$canonical_name
registration <- pt_local_reference_label_registration(source_nickname = "monuments")
labels <- pt_make_local_reference_labels(label_source, registration)
label_validation <- pt_validate_local_reference_label_anchors(
  labels, label_source, registration
)
tule_labels <- labels[
  labels$semantic_feature_key == "nm_ca_tule_lake",
  , drop = FALSE
]
visible_tule_label <- tule_labels[
  tule_labels$anchor_priority == min(tule_labels$anchor_priority),
  , drop = FALSE
]
if (nrow(tule_labels) != 2L ||
    length(unique(tule_labels$semantic_feature_key)) != 1L ||
    length(unique(tule_labels$label_text)) != 1L ||
    nrow(visible_tule_label) != 1L) {
  stop("Tule Lake label QA did not resolve two component anchors to one semantic label.")
}

utils::write.csv(
  sf::st_drop_geometry(tule_labels),
  output_paths[["labels"]],
  row.names = FALSE,
  na = ""
)

style_fill <- c(nps = "#54278F", fws = "#1F78B4")
style_stroke <- c(nps = "#54278F", fws = "#1F78B4")
popup <- paste0(
  "<strong>", tule$canonical_name, "</strong><br>",
  "Agency component: ", tule$source_agency, "<br>",
  "Source: ", tule$source_key, " / OBJECTID ", tule$source_object_id, "<br>",
  "Mapped agency-component area: ",
  format(round(tule$semantic_calculated_acres, 3), big.mark = ","), " acres"
)
map <- leaflet::leaflet(tule) |>
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
  leaflet::addPolygons(
    fillColor = unname(style_fill[tule$display_agency_key]),
    color = unname(style_stroke[tule$display_agency_key]),
    fillOpacity = 0.20,
    weight = 2,
    popup = popup,
    group = "Tule Lake agency components"
  ) |>
  leaflet::addLabelOnlyMarkers(
    data = visible_tule_label,
    label = visible_tule_label$label_text,
    labelOptions = leaflet::labelOptions(
      noHide = TRUE,
      direction = "center",
      textOnly = TRUE,
      style = list("font-weight" = "bold", "font-size" = "12px")
    ),
    group = "One semantic label"
  ) |>
  leaflet::addLegend(
    position = "bottomright",
    colors = unname(style_fill[c("nps", "fws")]),
    labels = c("NPS — Segregation Center", "USFWS — Peninsula/Castle Rock + Camp Tulelake"),
    title = "Authoritative components",
    opacity = 0.8
  ) |>
  leaflet::addLayersControl(
    overlayGroups = c("Tule Lake agency components", "One semantic label"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  ) |>
  leaflet::fitBounds(
    lng1 = sf::st_bbox(tule)[["xmin"]],
    lat1 = sf::st_bbox(tule)[["ymin"]],
    lng2 = sf::st_bbox(tule)[["xmax"]],
    lat2 = sf::st_bbox(tule)[["ymax"]]
  )
htmlwidgets::saveWidget(map, output_paths[["map"]], selfcontained = FALSE)

grDevices::png(
  output_paths[["map_png"]],
  width = 1800,
  height = 1100,
  res = 150,
  bg = "white"
)
graphics::par(mar = c(3.5, 3.5, 4.5, 1.5))
plot(
  sf::st_geometry(tule),
  col = grDevices::adjustcolor(
    unname(style_fill[tule$display_agency_key]),
    alpha.f = 0.35
  ),
  border = unname(style_stroke[tule$display_agency_key]),
  lwd = 2.5,
  axes = TRUE,
  main = "Tule Lake National Monument — authoritative agency components"
)
graphics::mtext(
  "NPS Segregation Center + USFWS Peninsula/Castle Rock and Camp Tulelake",
  side = 3,
  line = 0.7,
  cex = 0.9
)
graphics::legend(
  "bottomleft",
  legend = c(
    "NPS — Segregation Center",
    "USFWS — Peninsula/Castle Rock + Camp Tulelake"
  ),
  fill = grDevices::adjustcolor(unname(style_fill[c("nps", "fws")]), alpha.f = 0.35),
  border = unname(style_stroke[c("nps", "fws")]),
  bg = "white",
  cex = 0.85
)
label_xy <- sf::st_coordinates(visible_tule_label)
graphics::text(
  label_xy[[1]],
  label_xy[[2]],
  labels = visible_tule_label$label_text[[1]],
  font = 2,
  cex = 0.8,
  pos = 3
)
grDevices::dev.off()

summary <- list(
  status = "PASS",
  candidate_path = candidate_path,
  candidate_sha256 = digest::digest(file = candidate_path, algo = "sha256", serialize = FALSE),
  semantic_monument_id = "nm_ca_tule_lake",
  semantic_monument_count = length(unique(tule$monument_id)),
  agency_geometry_record_count = nrow(tule),
  constituent_geometry_part_count = nrow(component_qa),
  administering_agencies = sort(unique(component_qa$administering_agency)),
  repaired_semantic_acres = sum(component_qa$repaired_acres_epsg3310),
  display_semantic_acres = sum(component_qa$display_acres_epsg3310),
  raw_vertices = sum(component_qa$raw_vertices),
  repaired_vertices = sum(component_qa$repaired_vertices),
  display_vertices = sum(component_qa$display_vertices),
  repaired_polygon_parts = sum(component_qa$repaired_polygon_parts),
  display_polygon_parts = sum(component_qa$display_polygon_parts),
  semantic_label_count = length(unique(tule_labels$semantic_feature_key)),
  component_aware_label_anchor_count = nrow(tule_labels),
  visible_semantic_label_count = nrow(visible_tule_label),
  label_anchor_validation = label_validation,
  cache_mutated = FALSE,
  ui_filter_model_changed = FALSE,
  canonical_fws_fill_color = PT_LOCAL_REFERENCE_ACCEPTED_AGENCY_STYLES$fill_color[
    PT_LOCAL_REFERENCE_ACCEPTED_AGENCY_STYLES$category_key == "fws"
  ],
  canonical_fws_stroke_color = PT_LOCAL_REFERENCE_ACCEPTED_AGENCY_STYLES$stroke_color[
    PT_LOCAL_REFERENCE_ACCEPTED_AGENCY_STYLES$category_key == "fws"
  ],
  qa_map_png_sha256 = digest::digest(
    file = output_paths[["map_png"]], algo = "sha256", serialize = FALSE
  )
)
jsonlite::write_json(
  summary,
  output_paths[["summary"]],
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null",
  digits = 17
)

message("Focused Tule Lake source-repair QA PASS: ", output_paths[["summary"]])
message("Focused Tule Lake QA map: ", output_paths[["map"]])
message("Focused Tule Lake QA image: ", output_paths[["map_png"]])
