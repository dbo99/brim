#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(leaflet)
  library(htmltools)
})

source("00_config/config_local_reference_interactions.r")
source("03_functions/local_reference_interaction_helpers.r")
source("03_functions/leaflet_layer_local_core_helpers.r")
source("03_functions/leaflet_layer_local_reference_helpers.r")

required_path <- function(variable) {
  path <- Sys.getenv(variable, unset = "")
  if (!nzchar(path) || !file.exists(path)) {
    stop(variable, " must name an existing focused WSA QA input.")
  }
  path
}

raw_path <- required_path("BRIM_WSA_RAW_SOURCE")
processed_path <- required_path("BRIM_WSA_PROCESSED")
map_cache_path <- required_path("BRIM_WSA_MAP_CACHE")

raw <- sf::st_read(raw_path, quiet = TRUE)
processed <- readRDS(processed_path)
reference_cache <- readRDS(map_cache_path)
map_ready <- reference_cache[["wildernessstudyarea"]]

stopifnot(inherits(raw, "sf"), inherits(processed, "sf"), inherits(map_ready, "sf"))
stopifnot(nrow(raw) == 63L, nrow(processed) == 63L, nrow(map_ready) == 63L)

expected_names <- c("Red Mountain", "Trinity Alps (Subunit 4)")
expected_keys <- c(
  "wsa:globalid:eab306b9-62de-406c-9e79-fa7692fd729d",
  "wsa:globalid:f713da90-b15b-4cdb-9a29-cb1c158626d7"
)
expected_globalids <- c(
  "{EAB306B9-62DE-406C-9E79-FA7692FD729D}",
  "{F713DA90-B15B-4CDB-9A29-CB1C158626D7}"
)
expected_geometry_keys <- paste0(expected_keys, ":geometry:1")

unknown <- map_ready[
  map_ready$pt_local_reference_category_key == "unknown",
  , drop = FALSE
]
stopifnot(nrow(unknown) == 2L)
stopifnot(identical(unknown$pt_wsa_name, expected_names))
stopifnot(identical(unknown$pt_local_reference_feature_key, expected_keys))
stopifnot(identical(unknown$pt_local_reference_semantic_key, expected_keys))
stopifnot(identical(unknown$pt_local_reference_geometry_key, expected_geometry_keys))
stopifnot(identical(unknown$pt_wsa_global_id, expected_globalids))
stopifnot(all(!nzchar(unknown$pt_wsa_nlcs_id)))
stopifnot(all(!nzchar(unknown$pt_wsa_recommendation_raw)))
stopifnot(identical(unknown$pt_local_reference_geometry_components, c(1L, 1L)))
stopifnot(length(unique(unknown$pt_local_reference_semantic_key)) == 2L)
stopifnot(sum(unknown$pt_local_reference_geometry_components) == 2L)
stopifnot(!any(sf::st_is_empty(unknown)))
stopifnot(all(sf::st_is_valid(unknown)))
stopifnot(all(as.character(sf::st_geometry_type(unknown)) == "MULTIPOLYGON"))
stopifnot(!any(grepl(
  "GIS area|GIS acre",
  unknown$pt_reference_hover_text,
  ignore.case = TRUE
)))
red_unknown <- unknown[unknown$pt_wsa_name == "Red Mountain", , drop = FALSE]
trinity_unknown <- unknown[
  unknown$pt_wsa_name == "Trinity Alps (Subunit 4)",
  , drop = FALSE
]
stopifnot(nrow(red_unknown) == 1L, nrow(trinity_unknown) == 1L)
stopifnot(!grepl("mi²", red_unknown$pt_reference_hover_text, fixed = TRUE))
stopifnot(grepl("GIS acreage:</span> 0 acres", red_unknown$popup_html, fixed = TRUE))
stopifnot(grepl(
  "Approximate geometry-derived anomaly:</b>",
  red_unknown$popup_html,
  fixed = TRUE
))
expected_trinity_area <- pt_local_reference_format_square_miles_from_acres(
  trinity_unknown$pt_wsa_source_gis_acres
)
stopifnot(nzchar(expected_trinity_area))
stopifnot(grepl(
  expected_trinity_area,
  trinity_unknown$pt_reference_hover_text,
  fixed = TRUE
))

normalize_globalid <- function(x) {
  tolower(gsub("[{}[:space:]]", "", as.character(x)))
}
raw_index <- match(normalize_globalid(expected_globalids), normalize_globalid(raw$GlobalID))
stopifnot(!anyNA(raw_index))
stopifnot(identical(
  unknown$pt_wsa_source_gis_acres,
  suppressWarnings(as.numeric(raw$GIS_ACRES[raw_index]))
))
stopifnot(identical(as.character(raw$NLCS_NAME[raw_index]), expected_names))
stopifnot(all(is.na(raw$NLCS_ID[raw_index])))
stopifnot(all(is.na(raw$WSA_RCMND[raw_index])))

processed_index <- match(expected_keys, processed$pt_local_reference_feature_key)
stopifnot(!anyNA(processed_index))
stopifnot(identical(processed$pt_local_reference_category_key[processed_index], c("unknown", "unknown")))
stopifnot(!any(sf::st_is_empty(processed[processed_index, ])))
stopifnot(all(sf::st_is_valid(processed[processed_index, ])))

payload <- pt_local_reference_controller_payload(
  list(wildernessstudyarea = map_ready)
)[[1]]
stopifnot(isTRUE(payload$feature_selection_supported))
stopifnot(identical(payload$feature_selection_mode, "semantic_feature_multi"))
stopifnot(isTRUE(payload$auto_zoom_supported), isTRUE(payload$auto_zoom_default))
stopifnot(identical(payload$zoom_padding, 36), identical(payload$zoom_max, 12))
stopifnot(length(payload$records) == 63L)
stopifnot(length(payload$features) == 63L)
unknown_records <- Filter(
  function(record) identical(record$category_key, "unknown"),
  payload$records
)
unknown_features <- Filter(
  function(feature) "unknown" %in% feature$category_keys,
  payload$features
)
stopifnot(length(unknown_records) == 2L)
stopifnot(length(unknown_features) == 2L)
stopifnot(identical(
  vapply(unknown_records, function(record) record$feature_key, character(1)),
  expected_keys
))
stopifnot(identical(
  vapply(unknown_records, function(record) record$geometry_key, character(1)),
  expected_geometry_keys
))
stopifnot(identical(
  vapply(unknown_records, function(record) record$geometry_component_count, integer(1)),
  c(1L, 1L)
))
stopifnot(identical(
  vapply(unknown_features, function(feature) feature$semantic_feature_key, character(1)),
  expected_keys
))
stopifnot(identical(
  vapply(unknown_features, function(feature) feature$display_name, character(1)),
  expected_names
))
stopifnot(all(vapply(unknown_features, function(feature) {
  length(feature$semantic_feature_bounds) == 4L &&
    all(is.finite(feature$semantic_feature_bounds)) &&
    feature$semantic_feature_bounds[[1]] < feature$semantic_feature_bounds[[3]] &&
    feature$semantic_feature_bounds[[2]] < feature$semantic_feature_bounds[[4]]
}, logical(1))))
unknown_wgs84 <- sf::st_transform(unknown, 4326)
for (i in seq_along(unknown_features)) {
  bounds <- sf::st_bbox(unknown_wgs84[i, ])
  expected_bounds <- unname(as.numeric(c(
    bounds[["ymin"]], bounds[["xmin"]], bounds[["ymax"]], bounds[["xmax"]]
  )))
  stopifnot(isTRUE(all.equal(
    unknown_features[[i]]$semantic_feature_bounds,
    expected_bounds,
    tolerance = 1e-10
  )))
}

unknown_category <- Filter(
  function(category) identical(category$category_key, "unknown"),
  payload$categories
)[[1]]
stopifnot(identical(unknown_category$label, "Not stated"))
stopifnot(identical(unknown_category$fill_color, "#B0B0B0"))
stopifnot(identical(unknown_category$stroke_color, "#6B6B6B"))
stopifnot(identical(unknown_category$fill_opacity, 0.14))
stopifnot(identical(unknown_category$stroke_weight, 1.4))
stopifnot(identical(unknown_category$dash_array, "2,3"))

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The focused WSA Not-stated test requires jsonlite.")
}
payload_path <- tempfile("wsa-not-stated-payload-", fileext = ".json")
on.exit(unlink(payload_path), add = TRUE)
jsonlite::write_json(payload, payload_path, auto_unbox = TRUE, digits = NA)
filter_test <- system2(
  "node",
  c("qa/test_local_reference_wsa_not_stated.js", payload_path),
  stdout = TRUE,
  stderr = TRUE
)
if (!identical(attr(filter_test, "status"), NULL)) {
  stop(paste(filter_test, collapse = "\n"))
}
cat(paste(filter_test, collapse = "\n"), "\n")

probe <- leaflet::leaflet() |>
  leaflet::addMapPane("pane_lines", 480)
probe <- pt_add_reference_layers(
  probe,
  list(wildernessstudyarea = map_ready),
  list(add_reference_layers = TRUE, add_labels = FALSE)
)
polygon_call <- which(vapply(
  probe$x$calls,
  function(call) identical(call$method, "addPolygons"),
  logical(1)
))
stopifnot(length(polygon_call) == 1L)
polygon_args <- probe$x$calls[[polygon_call]]$args
leaflet_geometry <- polygon_args[[1]]
leaflet_ids <- polygon_args[[2]]
leaflet_options <- polygon_args[[4]]
leaflet_popup_options <- polygon_args[[6]]
leaflet_index <- match(expected_geometry_keys, leaflet_ids)

stopifnot(length(leaflet_ids) == 63L)
stopifnot(!anyNA(leaflet_index))
stopifnot(all(lengths(leaflet_geometry[leaflet_index]) > 0L))
stopifnot(identical(leaflet_options$pane, "pane_lines"))
stopifnot(identical(leaflet_popup_options$autoPan, TRUE))
stopifnot(identical(leaflet_popup_options$keepInView, TRUE))
stopifnot(identical(leaflet_popup_options$autoPanPaddingTopLeft, c(16, 84)))
stopifnot(identical(leaflet_popup_options$autoPanPaddingBottomRight, c(16, 24)))
stopifnot(identical(
  leaflet_popup_options$className,
  "pt-local-reference-tabbed-popup"
))
stopifnot(identical(leaflet_options$fillColor[leaflet_index], c("#B0B0B0", "#B0B0B0")))
stopifnot(identical(leaflet_options$color[leaflet_index], c("#6B6B6B", "#6B6B6B")))
stopifnot(identical(leaflet_options$fillOpacity[leaflet_index], c(0.14, 0.14)))
stopifnot(identical(leaflet_options$weight[leaflet_index], c(1.4, 1.4)))
stopifnot(identical(leaflet_options$dashArray[leaflet_index], c("2,3", "2,3")))

cat("Actual WSA Not-stated source/cache/payload/Leaflet assertions passed.\n")
