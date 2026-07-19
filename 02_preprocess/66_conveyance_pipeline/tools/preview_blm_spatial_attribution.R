# ==== preview_blm_spatial_attribution.R ======================================
#
# PURPOSE
#   Non-destructive QA pilot for attaching BLM-managed-land relationship fields
#   to the accepted BRIM conveyance segments.
#
# IMPORTANT
#   This script does NOT overwrite the canonical GeoPackage, the production
#   conveyance RDS exports, or any BRIM map cache. It writes preview/QA outputs
#   only. After review, the accepted logic will be moved into the reproducible
#   conveyance pipeline before canonical outputs are written.
#
# CANONICAL BLM SOURCE
#   04_processed_data/rds/blm_managed_core_3310.rds
#
# INPUT CONVEYANCE SOURCE
#   02_preprocess/66_conveyance_pipeline/output/brim_exports/
#     conveyance_segments_brim.rds
#
# METRICS PILOTED
#   blm_crosses
#   blm_length_mi
#   blm_pct_length
#   blm_nearest_mi
#   blm_crossing_count
#
# DESIGN
#   - all spatial calculations use EPSG:3310;
#   - a line must have at least 10 m of intersection length to count as crossing;
#   - tiny intersection pieces are retained in QA but excluded from BLM length
#     and crossing counts;
#   - touching the BLM boundary can therefore have nearest distance 0 while
#     blm_crosses remains FALSE;
#   - field-office attribution is intentionally deferred to a separate review.
# ==============================================================================

SCRIPT_VERSION <- "CONVEYANCE_BLM_PREVIEW_20260715_03"

required_packages <- c("sf", "dplyr", "readr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install required package(s), then rerun: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(tibble)
})

resolve_current_script <- function() {
  frames <- sys.frames()

  for (index in rev(seq_along(frames))) {
    candidate <- frames[[index]]$ofile

    if (!is.null(candidate) && nzchar(candidate)) {
      return(
        normalizePath(
          candidate,
          winslash = "/",
          mustWork = TRUE
        )
      )
    }
  }

  stop("Run this file with source().")
}

SCRIPT_FILE <- resolve_current_script()
PIPELINE_ROOT <- dirname(dirname(SCRIPT_FILE))
BRIM_PROJECT_ROOT <- dirname(dirname(PIPELINE_ROOT))

SEGMENTS_RDS <- file.path(
  PIPELINE_ROOT,
  "output",
  "brim_exports",
  "conveyance_segments_brim.rds"
)

BLM_RDS <- file.path(
  BRIM_PROJECT_ROOT,
  "04_processed_data",
  "rds",
  "blm_managed_core_3310.rds"
)

CSV_DIR <- file.path(PIPELINE_ROOT, "output", "csv")
QA_DIR <- file.path(PIPELINE_ROOT, "output", "qa")

dir.create(CSV_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(QA_DIR, recursive = TRUE, showWarnings = FALSE)

TARGET_CRS <- 3310
METRES_PER_MILE <- 1609.344
MIN_BLM_COMPONENT_M <- suppressWarnings(
  as.numeric(
    Sys.getenv(
      "CONVEYANCE_BLM_MIN_COMPONENT_M",
      unset = "10"
    )
  )
)

if (
  !is.finite(MIN_BLM_COMPONENT_M) ||
  MIN_BLM_COMPONENT_M < 0
) {
  stop("CONVEYANCE_BLM_MIN_COMPONENT_M must be a nonnegative number.")
}

for (path in c(SEGMENTS_RDS, BLM_RDS)) {
  if (!file.exists(path)) {
    stop("Missing required input:\n  ", path)
  }
}

message("Conveyance BLM preview version: ", SCRIPT_VERSION)
message("BRIM root: ", BRIM_PROJECT_ROOT)
message("Minimum counted BLM line component: ", MIN_BLM_COMPONENT_M, " m")

segments <- readRDS(SEGMENTS_RDS)
blm <- readRDS(BLM_RDS)

if (!inherits(segments, "sf") || nrow(segments) == 0L) {
  stop("Conveyance segment input is not a non-empty sf object.")
}

if (!inherits(blm, "sf") || nrow(blm) == 0L) {
  stop("BLM managed-land input is not a non-empty sf object.")
}

required_segment_fields <- c(
  "segment_id",
  "facility_id",
  "canonical_name",
  "length_mi"
)

missing_segment_fields <- setdiff(
  required_segment_fields,
  names(segments)
)

if (length(missing_segment_fields) > 0L) {
  stop(
    "Conveyance segment input is missing required fields: ",
    paste(missing_segment_fields, collapse = ", ")
  )
}

if (anyDuplicated(segments$segment_id)) {
  stop("Conveyance segment IDs are not unique.")
}

if (any(sf::st_is_empty(segments))) {
  stop("Conveyance segment input contains empty geometry.")
}

if (any(!sf::st_is_valid(segments))) {
  stop("Conveyance segment input contains invalid geometry.")
}

if (any(!sf::st_is_valid(blm))) {
  stop("BLM managed-land input contains invalid geometry.")
}

segments_3310 <- suppressWarnings(
  sf::st_transform(segments, TARGET_CRS)
)

blm_3310 <- suppressWarnings(
  sf::st_transform(blm, TARGET_CRS)
)

# Keep one authoritative dissolved BLM geometry even if a future source refresh
# happens to produce multiple rows.
blm_union <- sf::st_sf(
  source = "BLM-CA Managed",
  geometry = sf::st_union(sf::st_geometry(blm_3310))
)

segment_length_m <- as.numeric(
  sf::st_length(segments_3310)
)

if (any(!is.finite(segment_length_m) | segment_length_m <= 0)) {
  stop("One or more conveyance segments have nonpositive calculated length.")
}

message("Finding segments that intersect or touch BLM-managed land...")

candidate_matrix <- sf::st_intersects(
  segments_3310,
  blm_union,
  sparse = FALSE
)

candidate_intersects <- as.logical(candidate_matrix[, 1])

message(
  "Intersection/touch candidates: ",
  sum(candidate_intersects),
  " of ",
  nrow(segments_3310)
)

candidate_segments <- segments_3310[
  candidate_intersects,
  c("segment_id", "facility_id", "canonical_name")
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

line_parts <- candidate_segments[0, ]
line_parts$piece_length_m <- numeric()

if (nrow(clipped) > 0L) {
  clipped_type <- as.character(
    sf::st_geometry_type(clipped)
  )

  direct_lines <- clipped[
    clipped_type %in% c("LINESTRING", "MULTILINESTRING"),
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

  collections <- clipped[
    clipped_type == "GEOMETRYCOLLECTION",
  ]

  collection_lines <- clipped[0, ]

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
        is.finite(.data$piece_length_m),
        .data$piece_length_m > 0
      )
  }
}

qualifying_parts <- line_parts |>
  dplyr::filter(
    .data$piece_length_m >= MIN_BLM_COMPONENT_M
  )

small_parts <- line_parts |>
  dplyr::filter(
    .data$piece_length_m < MIN_BLM_COMPONENT_M
  )

intersection_summary <- if (nrow(qualifying_parts) > 0L) {
  qualifying_parts |>
    sf::st_drop_geometry() |>
    dplyr::group_by(.data$segment_id) |>
    dplyr::summarise(
      blm_length_m = sum(.data$piece_length_m),
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

message("Calculating minimum distance to BLM-managed land...")

distance_matrix <- sf::st_distance(
  segments_3310,
  blm_union
)

nearest_m <- as.numeric(distance_matrix[, 1])

# The BRIM-ready segment export already reserves blank BLM fields. Remove those
# placeholders here so the calculated preview columns do not acquire .x/.y
# suffixes during the join.
reserved_blm_fields <- c(
  "blm_crosses",
  "blm_length_mi",
  "blm_pct_length",
  "blm_nearest_mi",
  "blm_crossing_count",
  "blm_field_offices"
)

segment_preview <- segments_3310 |>
  sf::st_drop_geometry() |>
  dplyr::select(-dplyr::any_of(reserved_blm_fields)) |>
  dplyr::mutate(
    calculated_length_m = segment_length_m,
    calculated_length_mi = .data$calculated_length_m / METRES_PER_MILE
  ) |>
  dplyr::left_join(
    intersection_summary,
    by = "segment_id"
  ) |>
  dplyr::mutate(
    blm_length_m = dplyr::coalesce(.data$blm_length_m, 0),
    blm_crossing_count = dplyr::coalesce(
      as.integer(.data$blm_crossing_count),
      0L
    ),
    blm_crosses = .data$blm_length_m >= MIN_BLM_COMPONENT_M,
    blm_length_mi = .data$blm_length_m / METRES_PER_MILE,
    blm_pct_length = pmin(
      100,
      100 * .data$blm_length_m / .data$calculated_length_m
    ),
    blm_nearest_mi = ifelse(
      .data$blm_crosses,
      0,
      nearest_m / METRES_PER_MILE
    ),
    blm_intersects_or_touches = candidate_intersects,
    blm_touches_only = (
      .data$blm_intersects_or_touches &
        !.data$blm_crosses &
        .data$blm_nearest_mi == 0
    ),
    source_vs_calculated_length_pct_diff = 100 * (
      .data$calculated_length_mi - .data$length_mi
    ) / pmax(.data$calculated_length_mi, 1e-9)
  )

facility_preview <- segment_preview |>
  dplyr::group_by(
    .data$facility_id,
    .data$canonical_name
  ) |>
  dplyr::summarise(
    facility_segment_count = dplyr::n(),
    facility_crosses_blm = any(.data$blm_crosses),
    facility_length_mi = sum(
      .data$calculated_length_mi,
      na.rm = TRUE
    ),
    facility_length_on_blm_mi = sum(
      .data$blm_length_mi,
      na.rm = TRUE
    ),
    facility_pct_length_on_blm = 100 * (
      .data$facility_length_on_blm_mi /
        pmax(.data$facility_length_mi, 1e-9)
    ),
    facility_min_blm_distance_mi = min(
      .data$blm_nearest_mi,
      na.rm = TRUE
    ),
    facility_crossing_segment_count = sum(
      .data$blm_crosses
    ),
    facility_crossing_count = sum(
      .data$blm_crossing_count
    ),
    .groups = "drop"
  )

distance_bin <- dplyr::case_when(
  segment_preview$blm_crosses ~ "on/crosses BLM",
  segment_preview$blm_touches_only ~ "touches boundary only",
  segment_preview$blm_nearest_mi <= 1 ~ ">0–1 mi",
  segment_preview$blm_nearest_mi <= 5 ~ ">1–5 mi",
  segment_preview$blm_nearest_mi <= 15 ~ ">5–15 mi",
  TRUE ~ ">15 mi"
)

# Store summary values as text because the table intentionally combines a
# script-version label with numeric counts and measurements.
summary_table <- tibble::tibble(
  metric = c(
    "script_version",
    "segment_rows",
    "facility_rows",
    "blm_source_rows",
    "minimum_counted_component_m",
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
      SCRIPT_VERSION,
      nrow(segment_preview),
      nrow(facility_preview),
      nrow(blm),
      MIN_BLM_COMPONENT_M,
      sum(candidate_intersects),
      sum(segment_preview$blm_crosses),
      sum(segment_preview$blm_touches_only),
      sum(facility_preview$facility_crosses_blm),
      sum(segment_preview$calculated_length_mi),
      sum(segment_preview$blm_length_mi),
      nrow(small_parts),
      sum(
        small_parts$piece_length_m,
        na.rm = TRUE
      )
    )
  )
)

distance_bins <- tibble::tibble(
  distance_bin = distance_bin
) |>
  dplyr::count(
    .data$distance_bin,
    name = "segment_count"
  )

touch_or_sliver <- segment_preview |>
  dplyr::filter(
    .data$blm_touches_only |
      .data$segment_id %in% small_parts$segment_id
  ) |>
  dplyr::select(
    .data$segment_id,
    .data$facility_id,
    .data$canonical_name,
    .data$calculated_length_mi,
    .data$blm_crosses,
    .data$blm_length_mi,
    .data$blm_nearest_mi,
    .data$blm_touches_only
  ) |>
  dplyr::left_join(
    small_parts |>
      sf::st_drop_geometry() |>
      dplyr::group_by(.data$segment_id) |>
      dplyr::summarise(
        small_piece_count = dplyr::n(),
        small_piece_total_m = sum(.data$piece_length_m),
        small_piece_max_m = max(.data$piece_length_m),
        .groups = "drop"
      ),
    by = "segment_id"
  )

preview_geometry <- segments_3310 |>
  dplyr::select(
    .data$segment_id,
    geometry
  ) |>
  dplyr::left_join(
    segment_preview |>
      dplyr::select(
        .data$segment_id,
        .data$blm_crosses,
        .data$blm_length_mi,
        .data$blm_pct_length,
        .data$blm_nearest_mi,
        .data$blm_crossing_count,
        .data$blm_touches_only
      ),
    by = "segment_id"
  )

readr::write_csv(
  segment_preview,
  file.path(
    CSV_DIR,
    "qa_blm_spatial_segment_preview.csv"
  ),
  na = ""
)

readr::write_csv(
  facility_preview,
  file.path(
    CSV_DIR,
    "qa_blm_spatial_facility_preview.csv"
  ),
  na = ""
)

readr::write_csv(
  summary_table,
  file.path(
    CSV_DIR,
    "qa_blm_spatial_summary.csv"
  ),
  na = ""
)

readr::write_csv(
  distance_bins,
  file.path(
    CSV_DIR,
    "qa_blm_spatial_distance_bins.csv"
  ),
  na = ""
)

readr::write_csv(
  touch_or_sliver,
  file.path(
    CSV_DIR,
    "qa_blm_spatial_touch_or_sliver.csv"
  ),
  na = ""
)

saveRDS(
  preview_geometry,
  file.path(
    QA_DIR,
    "conveyance_segments_blm_preview_3310.rds"
  )
)

message("\nBLM spatial preview completed.")
message("No canonical or BRIM-ready conveyance outputs were overwritten.")
message("\nSummary:")
print(summary_table, n = Inf)
message("\nDistance bins:")
print(distance_bins, n = Inf)
message(
  "\nReview these files:\n  ",
  file.path(CSV_DIR, "qa_blm_spatial_summary.csv"),
  "\n  ",
  file.path(CSV_DIR, "qa_blm_spatial_distance_bins.csv"),
  "\n  ",
  file.path(CSV_DIR, "qa_blm_spatial_touch_or_sliver.csv"),
  "\n  ",
  file.path(CSV_DIR, "qa_blm_spatial_facility_preview.csv")
)
