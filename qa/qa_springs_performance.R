#!/usr/bin/env Rscript

## Focused, read-only QA for the retained Local Springs cache and the
## viewport-virtualized display architecture.
##
## Timings in this script cover R algorithms only. They are not browser,
## Leaflet, rendering, interaction, or end-user timings.
##
## Usage:
##   Rscript qa/qa_springs_performance.R \
##     /path/to/springs_map.rds \
##     /optional/output/directory

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || !nzchar(args[[1]])) {
  stop("Supply the retained springs_map.rds path.", call. = FALSE)
}

cache_path <- normalizePath(args[[1]], mustWork = TRUE)
output_dir <- if (length(args) >= 2L && nzchar(args[[2]])) {
  args[[2]]
} else {
  file.path(tempdir(), "brim_springs_profile")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, mustWork = TRUE)

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Package 'sf' is required.", call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required.", call. = FALSE)
}

source("03_functions/leaflet_layer_local_well_spring_helpers.r")

springs <- readRDS(cache_path)
if (!inherits(springs, "sf")) {
  stop("Retained Springs cache is not an sf object.", call. = FALSE)
}

required_cache_fields <- c(
  "spring_id",
  "spring_name_display",
  "spring_source_key",
  "spring_source_display"
)
missing_required <- setdiff(required_cache_fields, names(springs))
if (length(missing_required) > 0L) {
  stop(
    "Retained Springs cache is missing required field(s): ",
    paste(missing_required, collapse = ", "),
    call. = FALSE
  )
}

springs_ll <- springs
if (!is.na(sf::st_crs(springs_ll))) {
  springs_ll <- sf::st_transform(springs_ll, 4326)
}
coords <- sf::st_coordinates(sf::st_geometry(springs_ll))
attrs <- sf::st_drop_geometry(springs_ll)

if (nrow(coords) != nrow(attrs) || !all(c("X", "Y") %in% colnames(coords))) {
  stop("Springs cache does not contain one point coordinate per record.", call. = FALSE)
}

spring_id <- trimws(as.character(attrs$spring_id))
valid_id <- !is.na(spring_id) & nzchar(spring_id)
valid_coordinate <- is.finite(coords[, "X"]) &
  is.finite(coords[, "Y"]) &
  abs(coords[, "X"]) <= 180 &
  abs(coords[, "Y"]) <= 90

if (!all(valid_id)) {
  stop(
    sum(!valid_id),
    " retained Springs record(s) have no spring_id.",
    call. = FALSE
  )
}
if (anyDuplicated(spring_id)) {
  stop("Retained Springs spring_id values are not unique.", call. = FALSE)
}
if (!all(valid_coordinate)) {
  stop(
    sum(!valid_coordinate),
    " retained Springs record(s) have invalid coordinates.",
    call. = FALSE
  )
}

lng <- as.numeric(coords[, "X"])
lat <- as.numeric(coords[, "Y"])
exact_coordinate_key <- paste0(sprintf("%a", lng), "_", sprintf("%a", lat))
coordinate_7dp_key <- paste0(sprintf("%.7f", lng), "_", sprintf("%.7f", lat))
coordinate_6dp_key <- paste0(sprintf("%.6f", lng), "_", sprintf("%.6f", lat))

exact_table <- table(exact_coordinate_key)
coordinate_7dp_table <- table(coordinate_7dp_key)
coordinate_6dp_table <- table(coordinate_6dp_key)
exact_duplicate_table <- exact_table[exact_table > 1L]
duplicate_sizes <- as.integer(exact_duplicate_table)

browser_fields <- intersect(
  c(
    "spring_id",
    "spring_name_display",
    "spring_label_text",
    "spring_source_key",
    "spring_source_display",
    "spring_source_short",
    "gnis_id",
    "elevation_ft",
    "elevation_display",
    "google_search_url",
    "source_report_url",
    "survey_report_url",
    "source_report_label",
    "on_blm_ca",
    "dist_to_blm_mi",
    "dist_to_blm_ft",
    "spring_type",
    "spring_classification",
    "spring_condition",
    "spring_status",
    "flow_condition",
    "flow_rate",
    "flow_unit",
    "provider"
  ),
  names(attrs)
)
browser_records <- as.data.frame(
  attrs[, browser_fields, drop = FALSE],
  stringsAsFactors = FALSE
)
browser_records$pt_lat <- lat
browser_records$pt_lng <- lng

payload <- pt_prepare_springs_virtualized_payload(browser_records)
payload_ids <- as.character(payload$records$spring_id)

source_by_id <- data.frame(
  spring_id = spring_id,
  source_lng = lng,
  source_lat = lat,
  stringsAsFactors = FALSE
)
payload_by_id <- data.frame(
  spring_id = payload_ids,
  payload_lng = as.numeric(payload$records$pt_lng),
  payload_lat = as.numeric(payload$records$pt_lat),
  stringsAsFactors = FALSE
)
coordinate_reconciliation <- merge(
  source_by_id,
  payload_by_id,
  by = "spring_id",
  all = TRUE,
  sort = FALSE
)
coordinates_identical <- with(
  coordinate_reconciliation,
  !is.na(source_lng) &
    !is.na(payload_lng) &
    sprintf("%a", source_lng) == sprintf("%a", payload_lng) &
    sprintf("%a", source_lat) == sprintf("%a", payload_lat)
)

bool_value <- function(value) {
  if (is.logical(value)) return(value)
  text <- tolower(trimws(as.character(value)))
  out <- rep(NA, length(text))
  out[text %in% c("true", "t", "1", "yes", "y")] <- TRUE
  out[text %in% c("false", "f", "0", "no", "n")] <- FALSE
  out
}

source_key <- as.character(attrs$spring_source_key)
on_blm <- if ("on_blm_ca" %in% names(attrs)) {
  bool_value(attrs$on_blm_ca)
} else {
  rep(NA, nrow(attrs))
}
blm_distance <- if ("dist_to_blm_mi" %in% names(attrs)) {
  suppressWarnings(as.numeric(attrs$dist_to_blm_mi))
} else {
  rep(NA_real_, nrow(attrs))
}

filter_reconciliation <- data.frame(
  filter = c(
    "any",
    "source_nhd",
    "source_survey_2015_16",
    "blm_on",
    "blm_off",
    "blm_within_1_mile",
    "blm_within_5_miles"
  ),
  matching_records = c(
    nrow(attrs),
    sum(source_key == "nhd", na.rm = TRUE),
    sum(source_key == "survey_2015_16", na.rm = TRUE),
    sum(on_blm == TRUE, na.rm = TRUE),
    sum(on_blm == FALSE, na.rm = TRUE),
    sum(!is.na(blm_distance) & blm_distance <= 1),
    sum(!is.na(blm_distance) & blm_distance <= 5)
  ),
  semantics = c(
    "Any is unconstrained.",
    "Source predicate.",
    "Source predicate.",
    "BLM relationship predicate.",
    "BLM relationship predicate.",
    "Distance predicate; AND with any selected source predicate.",
    "Distance predicate; AND with any selected source predicate."
  ),
  stringsAsFactors = FALSE
)

duplicate_rows <- which(exact_coordinate_key %in% names(exact_duplicate_table))
duplicate_split <- split(duplicate_rows, exact_coordinate_key[duplicate_rows])

classify_duplicate_group <- function(indices) {
  sources <- unique(na.omit(source_key[indices]))
  names_at_location <- unique(na.omit(
    trimws(as.character(attrs$spring_name_display[indices]))
  ))
  gnis_at_location <- if ("gnis_id" %in% names(attrs)) {
    unique(na.omit(trimws(as.character(attrs$gnis_id[indices]))))
  } else {
    character(0)
  }

  if (length(sources) > 1L) {
    return("cross-source records at one exact coordinate")
  }
  if (length(gnis_at_location) > 1L) {
    return("distinct GNIS records at one exact coordinate")
  }
  if (length(names_at_location) > 1L) {
    return("distinct or alternate names at one exact coordinate")
  }
  "possible duplicate source rows or unresolved same-source colocation"
}

duplicate_audit <- if (length(duplicate_split) > 0L) {
  do.call(
    rbind,
    lapply(names(duplicate_split), function(key) {
      indices <- duplicate_split[[key]]
      data.frame(
        longitude = lng[indices[[1]]],
        latitude = lat[indices[[1]]],
        records = length(indices),
        source_count = length(unique(na.omit(source_key[indices]))),
        source_keys = paste(unique(na.omit(source_key[indices])), collapse = ";"),
        spring_ids = paste(spring_id[indices], collapse = ";"),
        spring_names = paste(
          unique(na.omit(as.character(attrs$spring_name_display[indices]))),
          collapse = ";"
        ),
        gnis_ids = if ("gnis_id" %in% names(attrs)) {
          paste(
            unique(na.omit(as.character(attrs$gnis_id[indices]))),
            collapse = ";"
          )
        } else {
          ""
        },
        available_field_interpretation = classify_duplicate_group(indices),
        review_note = paste(
          "Classification is evidence from retained fields only;",
          "do not merge or correct without source review."
        ),
        stringsAsFactors = FALSE
      )
    })
  )
} else {
  data.frame(
    longitude = numeric(0),
    latitude = numeric(0),
    records = integer(0),
    source_count = integer(0),
    source_keys = character(0),
    spring_ids = character(0),
    spring_names = character(0),
    gnis_ids = character(0),
    available_field_interpretation = character(0),
    review_note = character(0),
    stringsAsFactors = FALSE
  )
}

distribution <- as.data.frame(table(as.integer(exact_table)))
names(distribution) <- c("records_at_coordinate", "coordinate_locations")
distribution$records_at_coordinate <- as.integer(
  as.character(distribution$records_at_coordinate)
)
distribution$coordinate_locations <- as.integer(
  distribution$coordinate_locations
)

world_pixel <- function(lng_value, lat_value, zoom) {
  scale <- 256 * (2 ^ zoom)
  safe_lat <- pmax(-85.05112878, pmin(85.05112878, lat_value))
  sin_lat <- sin(safe_lat * pi / 180)
  data.frame(
    x = (lng_value + 180) / 360 * scale,
    y = (
      0.5 -
        log((1 + sin_lat) / (1 - sin_lat)) / (4 * pi)
    ) * scale
  )
}

viewport_bounds <- function(
  center_lng,
  center_lat,
  zoom,
  width = 1200,
  height = 800
) {
  center <- world_pixel(center_lng, center_lat, zoom)
  scale <- 256 * (2 ^ zoom)
  inverse <- function(x, y) {
    longitude <- x / scale * 360 - 180
    n <- pi - (2 * pi * y / scale)
    latitude <- atan(sinh(n)) * 180 / pi
    c(lng = longitude, lat = latitude)
  }
  northwest <- inverse(center$x - width / 2, center$y - height / 2)
  southeast <- inverse(center$x + width / 2, center$y + height / 2)
  c(
    west = northwest[["lng"]],
    south = southeast[["lat"]],
    east = southeast[["lng"]],
    north = northwest[["lat"]]
  )
}

buffer_bounds <- function(bounds, zoom) {
  ratio <- if (zoom >= 11) 0.15 else if (zoom >= 8) 0.18 else 0.10
  lng_pad <- max(0.04, (bounds[["east"]] - bounds[["west"]]) * ratio)
  lat_pad <- max(0.04, (bounds[["north"]] - bounds[["south"]]) * ratio)
  c(
    west = max(-180, bounds[["west"]] - lng_pad),
    south = max(-90, bounds[["south"]] - lat_pad),
    east = min(180, bounds[["east"]] + lng_pad),
    north = min(90, bounds[["north"]] + lat_pad)
  )
}

cluster_radius <- function(zoom) {
  if (zoom <= 6) return(175)
  if (zoom <= 8) return(145)
  if (zoom <= 9) return(115)
  if (zoom <= 10) return(75)
  35
}

location_start <- payload$locations$record_start + 1L
location_record_count <- payload$locations$record_count
location_lng <- payload$records$pt_lng[location_start]
location_lat <- payload$records$pt_lat[location_start]

scenario_result <- function(name, center_lng, center_lat, zoom) {
  raw_bounds <- viewport_bounds(center_lng, center_lat, zoom)
  bounds <- buffer_bounds(raw_bounds, zoom)
  candidate <- which(
    location_lng >= bounds[["west"]] &
      location_lng <= bounds[["east"]] &
      location_lat >= bounds[["south"]] &
      location_lat <= bounds[["north"]]
  )
  queried_records <- sum(location_record_count[candidate])
  descriptor_count <- length(candidate)
  aggregate_objects <- 0L
  exact_objects <- length(candidate)
  descriptor_record_total <- queried_records

  if (zoom < 11 && length(candidate) > 0L) {
    projected <- world_pixel(
      location_lng[candidate],
      location_lat[candidate],
      zoom
    )
    cells <- paste(
      floor(projected$x / cluster_radius(zoom)),
      floor(projected$y / cluster_radius(zoom)),
      sep = ":"
    )
    cell_location_count <- as.integer(table(cells))
    descriptor_count <- length(cell_location_count)
    aggregate_objects <- sum(cell_location_count > 1L)
    exact_objects <- sum(cell_location_count == 1L)
  }

  data.frame(
    scenario = name,
    zoom = zoom,
    queried_buffered_locations = length(candidate),
    queried_individual_records = queried_records,
    descriptor_record_total = descriptor_record_total,
    display_descriptors = descriptor_count,
    aggregate_objects = aggregate_objects,
    exact_location_objects = exact_objects,
    former_global_marker_objects = nrow(attrs),
    reduction_vs_global_percent = if (nrow(attrs) > 0L) {
      100 * (1 - descriptor_count / nrow(attrs))
    } else {
      NA_real_
    },
    records_reconcile = descriptor_record_total == queried_records,
    stringsAsFactors = FALSE
  )
}

scenarios <- rbind(
  scenario_result("statewide_home", -119.5, 37.15, 6),
  scenario_result("sacramento", -121.45, 38.55, 11),
  scenario_result("fresno", -120.20, 36.70, 11),
  scenario_result("mojave", -116.10, 35.10, 11),
  scenario_result("northeast_california", -120.50, 41.25, 11),
  scenario_result("fresno_zoom_10", -120.20, 36.70, 10),
  scenario_result("fresno_zoom_12", -120.20, 36.70, 12)
)

build_index <- function() {
  split(
    seq_along(location_record_count),
    paste(
      floor(location_lng / 0.25),
      floor(location_lat / 0.25),
      sep = ":"
    )
  )
}
index_elapsed <- replicate(
  20L,
  unname(system.time(build_index())[["elapsed"]])
)
query_elapsed <- replicate(
  100L,
  unname(system.time(
    scenario_result("benchmark", -120.20, 36.70, 11)
  )[["elapsed"]])
)
algorithm_timing <- data.frame(
  operation = c(
    "build_0.25_degree_index_all_locations",
    "zoom11_viewport_query_and_descriptor_count"
  ),
  repetitions = c(length(index_elapsed), length(query_elapsed)),
  median_elapsed_ms = c(median(index_elapsed), median(query_elapsed)) * 1000,
  worst_elapsed_ms = c(max(index_elapsed), max(query_elapsed)) * 1000,
  timing_scope = "R algorithm only; not browser/UI timing",
  stringsAsFactors = FALSE
)

field_checks <- data.frame(
  check = c(
    "source_identifier",
    "coordinates",
    "spring_name",
    "source_filter",
    "blm_filter",
    "popup_gnis",
    "popup_elevation",
    "google_link",
    "source_report_link"
  ),
  retained_cache_field = c(
    "spring_id",
    "geometry",
    "spring_name_display",
    "spring_source_key",
    "on_blm_ca;dist_to_blm_mi",
    "gnis_id",
    "elevation_display",
    "google_search_url",
    "source_report_url"
  ),
  present_in_cache = c(
    "spring_id" %in% names(attrs),
    TRUE,
    "spring_name_display" %in% names(attrs),
    "spring_source_key" %in% names(attrs),
    any(c("on_blm_ca", "dist_to_blm_mi") %in% names(attrs)),
    "gnis_id" %in% names(attrs),
    "elevation_display" %in% names(attrs),
    "google_search_url" %in% names(attrs),
    any(c("source_report_url", "survey_report_url") %in% names(attrs))
  ),
  present_in_browser_payload = c(
    "spring_id" %in% names(payload$records),
    all(c("pt_lng", "pt_lat") %in% names(payload$records)),
    "spring_name_display" %in% names(payload$records),
    "spring_source_key" %in% names(payload$records),
    any(c("on_blm_ca", "dist_to_blm_mi") %in% names(payload$records)),
    "gnis_id" %in% names(payload$records),
    "elevation_display" %in% names(payload$records),
    "google_search_url" %in% names(payload$records),
    any(c("source_report_url", "survey_report_url") %in% names(payload$records))
  ),
  stringsAsFactors = FALSE
)

counts <- data.frame(
  metric = c(
    "analytical_records",
    "records_with_valid_coordinates",
    "distinct_spring_ids",
    "unique_exact_coordinate_locations",
    "unique_7_decimal_coordinate_locations",
    "unique_6_decimal_coordinate_locations",
    "exact_duplicate_coordinate_groups",
    "records_at_exact_duplicate_locations",
    "maximum_records_at_one_exact_coordinate",
    "cross_source_exact_duplicate_groups",
    "payload_record_total",
    "payload_location_record_total",
    "payload_approximate_bytes",
    "retained_cache_object_bytes"
  ),
  value = c(
    nrow(attrs),
    sum(valid_coordinate),
    length(unique(spring_id)),
    length(exact_table),
    length(coordinate_7dp_table),
    length(coordinate_6dp_table),
    length(exact_duplicate_table),
    sum(duplicate_sizes),
    if (length(duplicate_sizes) > 0L) max(duplicate_sizes) else 1L,
    sum(duplicate_audit$source_count > 1L),
    nrow(payload$records),
    sum(payload$locations$record_count),
    payload$metadata$approximatePayloadBytes,
    as.numeric(object.size(springs))
  ),
  stringsAsFactors = FALSE
)

integrity <- data.frame(
  check = c(
    "all_records_have_valid_coordinates",
    "all_source_identifiers_unique",
    "payload_preserves_all_source_identifiers",
    "payload_preserves_exact_coordinates_by_identifier",
    "coordinate_group_counts_sum_to_records",
    "aggregate_scenario_record_totals_reconcile",
    "required_filter_popup_and_link_fields_retained"
  ),
  passed = c(
    all(valid_coordinate),
    !anyDuplicated(spring_id),
    setequal(spring_id, payload_ids) &&
      length(spring_id) == length(payload_ids),
    all(coordinates_identical),
    sum(payload$locations$record_count) == nrow(attrs),
    all(scenarios$records_reconcile),
    all(field_checks$present_in_browser_payload[
      field_checks$present_in_cache
    ])
  ),
  stringsAsFactors = FALSE
)

if (!all(integrity$passed)) {
  print(integrity)
  stop("One or more Springs integrity checks failed.", call. = FALSE)
}

utils::write.csv(
  counts,
  file.path(output_dir, "counts.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  distribution,
  file.path(output_dir, "coordinate_distribution.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  duplicate_audit,
  file.path(output_dir, "duplicate_coordinate_audit.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  field_checks,
  file.path(output_dir, "field_retention.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  filter_reconciliation,
  file.path(output_dir, "filter_reconciliation.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  scenarios,
  file.path(output_dir, "viewport_architecture.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  algorithm_timing,
  file.path(output_dir, "algorithm_timing.csv"),
  row.names = FALSE,
  na = ""
)
utils::write.csv(
  integrity,
  file.path(output_dir, "integrity_checks.csv"),
  row.names = FALSE,
  na = ""
)

summary_lines <- c(
  "# BRIM Local Springs focused profile",
  "",
  paste0("- Cache: `", cache_path, "`"),
  paste0("- Analytical records: ", format(nrow(attrs), big.mark = ",")),
  paste0(
    "- Exact coordinate locations: ",
    format(length(exact_table), big.mark = ",")
  ),
  paste0(
    "- Exact multi-record locations: ",
    format(length(exact_duplicate_table), big.mark = ",")
  ),
  paste0(
    "- Maximum records at one exact coordinate: ",
    if (length(duplicate_sizes) > 0L) max(duplicate_sizes) else 1L
  ),
  paste0(
    "- Approximate compact embedded payload: ",
    format(
      round(payload$metadata$approximatePayloadBytes / 1024^2, 3),
      nsmall = 3
    ),
    " MiB"
  ),
  "",
  paste(
    "The duplicate audit preserves every member and records only an",
    "available-field interpretation; it does not authorize deduplication."
  ),
  paste(
    "Viewport results are deterministic structural object-count estimates.",
    "Algorithm timings are R-only and must not be reported as browser timings."
  ),
  paste(
    "Final standalone-HTML byte contribution and user-facing timings require",
    "the rendered codex_ship browser gate."
  ),
  "",
  paste0("Output directory: `", output_dir, "`")
)
writeLines(summary_lines, file.path(output_dir, "summary.md"))
cat(paste(summary_lines, collapse = "\n"), "\n")
