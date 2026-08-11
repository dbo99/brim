#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tibble)
})

source("00_config/config_labels.r")
source("00_config/config_local_reference_interactions.r")
source("03_functions/label_helpers.r")
source("03_functions/leaflet_label_helpers.r")
source("03_functions/local_reference_interaction_helpers.r")

expect_identical <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    stop(
      label, " failed. Actual: ", paste(actual, collapse = ", "),
      "; expected: ", paste(expected, collapse = ", ")
    )
  }
}

square <- function(xmin, ymin, size) {
  sf::st_polygon(list(matrix(c(
    xmin, ymin,
    xmin + size, ymin,
    xmin + size, ymin + size,
    xmin, ymin + size,
    xmin, ymin
  ), ncol = 2, byrow = TRUE)))
}

make_polygon_layer <- function(semantic, geometry_key, label, geometry) {
  sf::st_sf(
    pt_local_reference_semantic_key = semantic,
    pt_local_reference_geometry_key = geometry_key,
    pt_reference_label_text = label,
    geometry = sf::st_sfc(geometry, crs = 4326)
  )
}

expect_identical(
  LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY$layer_id,
  c(
    "acec", "federal_wilderness", "national_monuments", "ca_desert_ncl",
    "wilderness_study_areas", "national_scenic_historic_trails",
    "grazing_allotments", "counties", "rwqcb_regions", "water_districts"
  ),
  "registered Local Reference label layers"
)
stopifnot(
  all(LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY$lbl_available),
  identical(as.numeric(pt_label_cfg("allotments")$min_zoom), 10),
  identical(as.numeric(pt_label_cfg("water_districts")$min_zoom), 13),
  identical(
    INLINE_LABEL_PAIRS$min_zoom[
      match(
        c("Grazing Allotments", "Water Districts"),
        INLINE_LABEL_PAIRS$main_name
      )
    ],
    c(10, 13)
  ),
  identical(
    LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY$visible_component_aware,
    c(FALSE, TRUE, TRUE, FALSE, FALSE, rep(FALSE, 5))
  ),
  all(c(
    "National Monuments", "CA Desert National Conservation Lands",
    "Wilderness Study Areas",
    "National Scenic/Historic Trails", "Grazing Allotments", "Counties",
    "RWQCB Regions", "Water Districts"
  ) %in% INLINE_LABEL_PAIRS$main_name)
)

acec <- make_polygon_layer(
  c("acec-1", "acec-2"), c("acec-g1", "acec-g2"),
  c("Alpha ACEC", "Beta ACEC"),
  list(square(-120, 35, 0.2), square(-119, 36, 0.3))
)
federal <- make_polygon_layer(
  c("fw-shared", "fw-shared", "fw-single"),
  c("fw-blm", "fw-usfs", "fw-nps"),
  c("Shared Wilderness", "Shared Wilderness", "Single Wilderness"),
  list(
    square(-120, 35, 0.1),
    square(-119, 36, 0.4),
    square(-118, 37, 0.2)
  )
)
monuments <- make_polygon_layer(
  "nm-ca-example", "nmgeom-example", "Example National Monument",
  list(square(-117, 35, 0.25))
)
desert_ncl <- make_polygon_layer(
  "NLCS002009", "geom:nlcs002009", "Basin and Range",
  list(square(-116.5, 34.5, 0.2))
)
wsa_union <- sf::st_union(sf::st_sfc(
  square(-121, 35, 0.1), square(-120.5, 35.5, 0.2), crs = 4326
))[[1]]
wsa <- make_polygon_layer(
  "wsa-1", "wsa-g1", "Multipart WSA", list(wsa_union)
)
trail_geometry <- sf::st_multilinestring(list(
  matrix(c(-121, 34, -120.9, 34.1), ncol = 2, byrow = TRUE),
  matrix(c(-120, 35, -118, 35), ncol = 2, byrow = TRUE)
))
trails <- sf::st_sf(
  pt_local_reference_semantic_key = "trail-1",
  pt_local_reference_geometry_key = "trail-g1",
  pt_reference_label_text = "Example National Historic Trail",
  geometry = sf::st_sfc(trail_geometry, crs = 4326)
)
allotments <- make_polygon_layer(
  c("allotment-1", "allotment-1"), c("allotment-g1", "allotment-g2"),
  c("Example Allotment", "Example Allotment"),
  list(square(-117, 36, 0.1), square(-116.8, 36.2, 0.2))
)
county <- make_polygon_layer(
  "county-1", "county-g1", "Example County", list(square(-116, 36, 0.4))
)
rwqcb <- make_polygon_layer(
  "rwqcb-1", "rwqcb-g1", "Region 1 · North Coast",
  list(square(-115, 36, 0.4))
)
water <- make_polygon_layer(
  c("water-1", "water-1"), c("water-g1", "water-g2"),
  c("Example Water District", "Example Water District"),
  list(square(-114, 36, 0.1), square(-113.8, 36.2, 0.2))
)

layers <- list(
  acec = acec,
  fedwilderness = federal,
  monuments = monuments,
  cadesert_ncl = desert_ncl,
  wildernessstudyarea = wsa,
  trails = trails,
  allotments = allotments,
  county = county,
  rwqcb_regions = rwqcb,
  water_districts = water
)
warnings_seen <- character(0)
labels <- withCallingHandlers(
  pt_build_registered_local_reference_label_children(layers),
  warning = function(warning) {
    warnings_seen <<- c(warnings_seen, conditionMessage(warning))
    invokeRestart("muffleWarning")
  }
)
expect_identical(warnings_seen, character(0), "explicit geometry-only anchors")
expect_identical(names(labels), names(layers), "registered child order")
expect_identical(
  vapply(labels, nrow, integer(1)),
  c(
    acec = 2L, fedwilderness = 3L, monuments = 1L, cadesert_ncl = 1L,
    wildernessstudyarea = 1L, trails = 1L, allotments = 1L, county = 1L,
    rwqcb_regions = 1L, water_districts = 1L
  ),
  "anchor counts"
)
expect_identical(
  vapply(labels, function(x) length(unique(x$semantic_feature_key)), integer(1)),
  c(
    acec = 2L, fedwilderness = 2L, monuments = 1L, cadesert_ncl = 1L,
    wildernessstudyarea = 1L, trails = 1L, allotments = 1L, county = 1L,
    rwqcb_regions = 1L, water_districts = 1L
  ),
  "semantic label counts"
)

federal_labels <- labels$fedwilderness
shared <- federal_labels[
  federal_labels$semantic_feature_key == "fw-shared", , drop = FALSE
]
expect_identical(shared$geometry_key, c("fw-usfs", "fw-blm"), "area-ranked anchors")
expect_identical(shared$anchor_priority, c(1L, 2L), "visible anchor priority")
expect_identical(
  unique(shared$label_text), "Shared Wilderness", "one public semantic name"
)

trail_distance <- as.numeric(sf::st_distance(
  sf::st_transform(labels$trails, 3310),
  sf::st_transform(trails, 3310)
))
stopifnot(
  trail_distance <= 0.5,
  identical(
    labels$trails$anchor_strategy,
    "line_semantic_longest_component_midpoint"
  )
)

labels_again <- pt_build_registered_local_reference_label_children(layers)
stopifnot(identical(labels, labels_again))

## Runtime threshold metadata comes from the shared registry, while the cached
## anchor geometry is reused unchanged. This fixture deliberately simulates the
## previously accepted Water cache metadata to prove no anchor rebuild is needed.
stale_water_labels <- labels
stale_water_labels$water_districts$min_zoom <- 12
water_payload <- pt_local_reference_semantic_label_payload(
  labels_all = stale_water_labels,
  x = water,
  registry_row = LOCAL_REFERENCE_INTERACTION_REGISTRY[
    LOCAL_REFERENCE_INTERACTION_REGISTRY$layer_id == "water_districts",
    , drop = FALSE
  ]
)
stopifnot(
  identical(water_payload$min_zoom, 13),
  identical(
    pt_label_disable_zoom(
      "water_districts",
      stale_water_labels$water_districts
    ),
    13
  ),
  identical(water_payload$anchor_count, nrow(labels$water_districts))
)

actual_reference_path <- Sys.getenv("BRIM_LABEL_REFERENCE_CACHE", unset = "")
if (nzchar(actual_reference_path)) {
  if (!file.exists(actual_reference_path)) {
    stop("BRIM_LABEL_REFERENCE_CACHE does not exist: ", actual_reference_path)
  }
  actual_reference <- readRDS(actual_reference_path)
  actual_labels <- pt_build_registered_local_reference_label_children(
    actual_reference
  )
  expect_identical(
    vapply(actual_labels, nrow, integer(1)),
    c(
      acec = 238L, fedwilderness = 197L, monuments = 22L, cadesert_ncl = 11L,
      wildernessstudyarea = 63L, trails = 6L
    ),
    "actual anchor counts"
  )
  expect_identical(
    vapply(
      actual_labels,
      function(x) length(unique(x$semantic_feature_key)),
      integer(1)
    ),
    c(
      acec = 238L, fedwilderness = 158L, monuments = 20L, cadesert_ncl = 11L,
      wildernessstudyarea = 63L, trails = 6L
    ),
    "actual semantic counts"
  )
}

actual_label_path <- Sys.getenv("BRIM_CANONICAL_LABEL_CACHE", unset = "")
if (nzchar(actual_label_path)) {
  if (!file.exists(actual_label_path)) {
    stop("BRIM_CANONICAL_LABEL_CACHE does not exist: ", actual_label_path)
  }
  canonical <- readRDS(actual_label_path)
  required_children <- c(
    "acec", "fedwilderness", "monuments", "cadesert_ncl",
    "wildernessstudyarea", "trails"
  )
  stopifnot(
    all(required_children %in% names(canonical)),
    !"major_conveyance" %in% names(canonical)
  )
  expect_identical(
    vapply(canonical[required_children], nrow, integer(1)),
    c(
      acec = 238L, fedwilderness = 197L, monuments = 22L, cadesert_ncl = 11L,
      wildernessstudyarea = 63L, trails = 6L
    ),
    "canonical registered child counts"
  )
}

message("Local Reference semantic-label registry/anchor tests passed.")
