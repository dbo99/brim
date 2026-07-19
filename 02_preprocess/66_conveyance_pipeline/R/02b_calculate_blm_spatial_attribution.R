# ==== 02b_calculate_blm_spatial_attribution.R ===============================
#
# PURPOSE
#   Populate the reserved conveyance BLM relationship fields using BRIM's
#   accepted dissolved "BLM-CA Managed" polygon.
#
# CALCULATION CRS
#   EPSG:3310 (NAD83 / California Albers)
#
# SEGMENT FIELDS
#   blm_crosses
#   blm_length_mi
#   blm_pct_length
#   blm_nearest_mi
#   blm_crossing_count
#
# FACILITY FIELDS
#   facility_crosses_blm
#   facility_length_on_blm_mi
#   facility_min_blm_distance_mi
#
# NOTES
#   - A line component must overlap BLM for at least 10 metres to count.
#   - Boundary-only touches and smaller slivers remain visible in QA.
#   - Field-office attribution is deliberately separate and remains blank.
# ==============================================================================

BLM_ATTRIBUTION_VERSION <- "CONVEYANCE_BLM_ATTRIBUTION_20260715_01"
BLM_MIN_COMPONENT_M <- 10
METRES_PER_MILE <- 1609.344

message(
  "Calculating conveyance relationship to BLM-CA Managed land..."
)
message("BLM attribution version: ", BLM_ATTRIBUTION_VERSION)
message(
  "Minimum counted BLM intersection component: ",
  BLM_MIN_COMPONENT_M,
  " m"
)

blm_managed <- readRDS(BLM_MANAGED_RDS)

if (!inherits(blm_managed, "sf") || nrow(blm_managed) == 0L) {
  stop(
    "BLM managed-land source is not a non-empty sf object:\n  ",
    BLM_MANAGED_RDS
  )
}

if (any(sf::st_is_empty(blm_managed))) {
  stop("BLM managed-land source contains empty geometry.")
}

if (any(!sf::st_is_valid(blm_managed))) {
  stop("BLM managed-land source contains invalid geometry.")
}

if (!inherits(segments, "sf") || nrow(segments) == 0L) {
  stop("Conveyance segments are not available for BLM attribution.")
}

if (anyDuplicated(segments$segment_id)) {
  stop("Conveyance segment IDs are not unique.")
}

if (any(sf::st_is_empty(segments))) {
  stop("Conveyance segments contain empty geometry.")
}

if (any(!sf::st_is_valid(segments))) {
  stop("Conveyance segments contain invalid geometry.")
}

segments_3310 <- suppressWarnings(
  sf::st_transform(segments, 3310)
)

blm_3310 <- suppressWarnings(
  sf::st_transform(blm_managed, 3310)
)

blm_union <- sf::st_sf(
  source = "BLM-CA Managed",
  geometry = sf::st_union(sf::st_geometry(blm_3310))
)

segment_length_m <- as.numeric(
  sf::st_length(segments_3310)
)

if (any(!is.finite(segment_length_m) | segment_length_m <= 0)) {
  stop(
    "One or more conveyance segments have a nonpositive ",
    "California Albers length."
  )
}

candidate_matrix <- sf::st_intersects(
  segments_3310,
  blm_union,
  sparse = FALSE
)

candidate_intersects <- as.logical(candidate_matrix[, 1])

message(
  "Segments intersecting or touching BLM: ",
  sum(candidate_intersects),
  " of ",
  nrow(segments_3310)
)

candidate_segments <- segments_3310[
  candidate_intersects,
  c(
    "segment_id",
    "facility_id",
    "canonical_name"
  )
]

clipped <- if (nrow(candidate_segments) > 0L) {
  suppressWarnings(
    sf::st_intersection(
      candidate_segments,
      sf::st_geometry(blm_union)
    )
  )
} else {
  candidate_segments[0, ]
}

empty_line_parts <- sf::st_sf(
  segment_id = character(),
  facility_id = character(),
  canonical_name = character(),
  piece_length_m = numeric(),
  geometry = sf::st_sfc(crs = 3310)
)

line_parts <- empty_line_parts

if (nrow(clipped) > 0L) {
  clipped_type <- as.character(
    sf::st_geometry_type(clipped)
  )

  direct_lines <- clipped[
    clipped_type %in% c(
      "LINESTRING",
      "MULTILINESTRING"
    ),
  ]

  if (nrow(direct_lines) > 0L) {
    direct_lines <- suppressWarnings(
      sf::st_cast(
        direct_lines,
        "LINESTRING",
        warn = FALSE
      )
    )
  }

  collection_lines <- clipped[0, ]

  collections <- clipped[
    clipped_type == "GEOMETRYCOLLECTION",
  ]

  if (nrow(collections) > 0L) {
    collection_lines <- suppressWarnings(
      sf::st_collection_extract(
        collections,
        "LINESTRING",
        warn = FALSE
      )
    )

    if (nrow(collection_lines) > 0L) {
      collection_lines <- suppressWarnings(
        sf::st_cast(
          collection_lines,
          "LINESTRING",
          warn = FALSE
        )
      )
    }
  }

  line_parts <- dplyr::bind_rows(
    direct_lines,
    collection_lines
  )

  if (nrow(line_parts) > 0L) {
    line_parts$piece_length_m <- as.numeric(
      sf::st_length(line_parts)
    )

    line_parts <- line_parts |>
      dplyr::filter(
        is.finite(piece_length_m),
        piece_length_m > 0
      )
  } else {
    line_parts <- empty_line_parts
  }
}

qualifying_parts <- line_parts |>
  dplyr::filter(
    piece_length_m >= BLM_MIN_COMPONENT_M
  )

small_parts <- line_parts |>
  dplyr::filter(
    piece_length_m < BLM_MIN_COMPONENT_M
  )

intersection_summary <- if (nrow(qualifying_parts) > 0L) {
  qualifying_parts |>
    sf::st_drop_geometry() |>
    dplyr::group_by(segment_id) |>
    dplyr::summarise(
      blm_length_m = sum(piece_length_m),
      blm_crossing_count = dplyr::n(),
      .groups = "drop"
    )
} else {
  tibble::tibble(
    segment_id = character(),
    blm_length_m = numeric(),
    blm_crossing_count = integer()
  )
}

blm_length_m <- rep(0, nrow(segments_3310))
blm_crossing_count <- rep(0L, nrow(segments_3310))

if (nrow(intersection_summary) > 0L) {
  matched_segment <- match(
    intersection_summary$segment_id,
    segments_3310$segment_id
  )

  if (anyNA(matched_segment)) {
    stop(
      "BLM intersection output contains an unknown segment ID."
    )
  }

  blm_length_m[matched_segment] <-
    intersection_summary$blm_length_m

  blm_crossing_count[matched_segment] <-
    as.integer(intersection_summary$blm_crossing_count)
}

nearest_m <- as.numeric(
  sf::st_distance(
    segments_3310,
    blm_union
  )[, 1]
)

blm_crosses <- blm_length_m >= BLM_MIN_COMPONENT_M

segments$blm_crosses <- blm_crosses
segments$blm_length_mi <- blm_length_m / METRES_PER_MILE
segments$blm_pct_length <- pmin(
  100,
  100 * blm_length_m / segment_length_m
)
segments$blm_nearest_mi <- ifelse(
  blm_crosses,
  0,
  nearest_m / METRES_PER_MILE
)
segments$blm_crossing_count <- blm_crossing_count
segments$blm_field_offices <- NA_character_

blm_touches_only <- (
  candidate_intersects &
    !segments$blm_crosses &
    segments$blm_nearest_mi == 0
)

facility_blm_summary <- tibble::tibble(
  facility_id = segments$facility_id,
  calculated_segment_length_mi = (
    segment_length_m / METRES_PER_MILE
  ),
  blm_crosses = segments$blm_crosses,
  blm_length_mi = segments$blm_length_mi,
  blm_nearest_mi = segments$blm_nearest_mi,
  blm_crossing_count = segments$blm_crossing_count
) |>
  dplyr::group_by(facility_id) |>
  dplyr::summarise(
    facility_crosses_blm = any(blm_crosses),
    facility_length_mi_calculated = sum(
      calculated_segment_length_mi,
      na.rm = TRUE
    ),
    facility_length_on_blm_mi = sum(
      blm_length_mi,
      na.rm = TRUE
    ),
    facility_pct_length_on_blm = 100 * (
      facility_length_on_blm_mi /
        pmax(facility_length_mi_calculated, 1e-9)
    ),
    facility_min_blm_distance_mi = min(
      blm_nearest_mi,
      na.rm = TRUE
    ),
    facility_crossing_segment_count = sum(
      blm_crosses
    ),
    facility_crossing_count = sum(
      blm_crossing_count
    ),
    .groups = "drop"
  )

facility_match <- match(
  facilities$facility_id,
  facility_blm_summary$facility_id
)

if (anyNA(facility_match)) {
  stop(
    "One or more canonical facilities lack a BLM facility summary."
  )
}

facilities$facility_crosses_blm <-
  facility_blm_summary$facility_crosses_blm[facility_match]

facilities$facility_length_on_blm_mi <-
  facility_blm_summary$facility_length_on_blm_mi[facility_match]

facilities$facility_min_blm_distance_mi <-
  facility_blm_summary$facility_min_blm_distance_mi[facility_match]

facilities$facility_blm_field_offices <- NA_character_

qa_blm_spatial_facility_preview <- facilities |>
  dplyr::select(
    facility_id,
    canonical_name,
    segment_count,
    facility_crosses_blm,
    length_mi,
    facility_length_on_blm_mi,
    facility_min_blm_distance_mi
  ) |>
  dplyr::left_join(
    facility_blm_summary |>
      dplyr::select(
        facility_id,
        facility_length_mi_calculated,
        facility_pct_length_on_blm,
        facility_crossing_segment_count,
        facility_crossing_count
      ),
    by = "facility_id"
  ) |>
  dplyr::rename(
    facility_segment_count = segment_count,
    facility_length_mi_source = length_mi
  )

distance_bin <- dplyr::case_when(
  segments$blm_crosses ~ "on/crosses BLM",
  blm_touches_only ~ "touches boundary only",
  segments$blm_nearest_mi <= 1 ~ ">0-1 mi",
  segments$blm_nearest_mi <= 5 ~ ">1-5 mi",
  segments$blm_nearest_mi <= 15 ~ ">5-15 mi",
  TRUE ~ ">15 mi"
)

distance_bin_order <- c(
  "on/crosses BLM",
  "touches boundary only",
  ">0-1 mi",
  ">1-5 mi",
  ">5-15 mi",
  ">15 mi"
)

qa_blm_spatial_distance_bins <- tibble::tibble(
  distance_bin = factor(
    distance_bin,
    levels = distance_bin_order
  )
) |>
  dplyr::count(
    distance_bin,
    name = "segment_count",
    .drop = FALSE
  ) |>
  dplyr::mutate(
    distance_bin = as.character(distance_bin)
  )

small_part_summary <- if (nrow(small_parts) > 0L) {
  small_parts |>
    sf::st_drop_geometry() |>
    dplyr::group_by(segment_id) |>
    dplyr::summarise(
      small_piece_count = dplyr::n(),
      small_piece_total_m = sum(piece_length_m),
      small_piece_max_m = max(piece_length_m),
      .groups = "drop"
    )
} else {
  tibble::tibble(
    segment_id = character(),
    small_piece_count = integer(),
    small_piece_total_m = numeric(),
    small_piece_max_m = numeric()
  )
}

qa_blm_spatial_touch_or_sliver <- tibble::tibble(
  segment_id = segments$segment_id,
  facility_id = segments$facility_id,
  canonical_name = segments$canonical_name,
  calculated_length_mi = segment_length_m / METRES_PER_MILE,
  blm_crosses = segments$blm_crosses,
  blm_length_mi = segments$blm_length_mi,
  blm_nearest_mi = segments$blm_nearest_mi,
  blm_touches_only = blm_touches_only
) |>
  dplyr::filter(
    blm_touches_only |
      segment_id %in% small_part_summary$segment_id
  ) |>
  dplyr::left_join(
    small_part_summary,
    by = "segment_id"
  )

qa_blm_spatial_summary <- data.frame(
  metric = c(
    "blm_attribution_version",
    "blm_source",
    "minimum_counted_component_m",
    "segment_rows",
    "facility_rows",
    "segments_intersect_or_touch_blm",
    "segments_crossing_blm",
    "segments_touching_boundary_only",
    "facilities_crossing_blm",
    "total_conveyance_length_mi",
    "total_conveyance_length_on_blm_mi",
    "small_intersection_piece_count",
    "small_intersection_piece_length_m"
  ),
  value = as.character(
    c(
      BLM_ATTRIBUTION_VERSION,
      normalizePath(
        BLM_MANAGED_RDS,
        winslash = "/",
        mustWork = TRUE
      ),
      BLM_MIN_COMPONENT_M,
      nrow(segments),
      nrow(facilities),
      sum(candidate_intersects),
      sum(segments$blm_crosses),
      sum(blm_touches_only),
      sum(facilities$facility_crosses_blm),
      sum(segment_length_m / METRES_PER_MILE),
      sum(segments$blm_length_mi),
      nrow(small_parts),
      sum(
        small_parts$piece_length_m,
        na.rm = TRUE
      )
    )
  ),
  stringsAsFactors = FALSE
)

message(
  "BLM attribution complete: ",
  sum(segments$blm_crosses),
  " crossing segments; ",
  sum(facilities$facility_crosses_blm),
  " crossing facilities; ",
  format(
    round(sum(segments$blm_length_mi), 2),
    nsmall = 2
  ),
  " conveyance miles on BLM-managed land."
)

if (nrow(qa_blm_spatial_touch_or_sliver) > 0L) {
  message(
    "BLM touch/sliver QA rows: ",
    nrow(qa_blm_spatial_touch_or_sliver)
  )
}
