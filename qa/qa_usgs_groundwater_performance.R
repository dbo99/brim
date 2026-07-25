#!/usr/bin/env Rscript

## Focused, read-only profiling for the retained Local USGS groundwater cache.
##
## This script measures record/location integrity and the bounded object counts
## produced by the selected viewport/grid architecture. Its timings cover only
## index/query/descriptor algorithms in R; they are deliberately not reported
## as browser or user-interface timings.
##
## Usage:
##   Rscript qa/qa_usgs_groundwater_performance.R \
##     /path/to/usgs_wells_map.rds \
##     /optional/output/directory

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || !nzchar(args[[1]])) {
  stop("Supply the retained usgs_wells_map.rds path.", call. = FALSE)
}

cache_path <- normalizePath(args[[1]], mustWork = TRUE)
output_dir <- if (length(args) >= 2L && nzchar(args[[2]])) {
  args[[2]]
} else {
  file.path(tempdir(), "brim_usgs_groundwater_profile")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, mustWork = TRUE)

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Package 'sf' is required.", call. = FALSE)
}

wells <- readRDS(cache_path)
if (!inherits(wells, "sf")) {
  stop("Retained groundwater cache is not an sf object.", call. = FALSE)
}
if (!"site_no" %in% names(wells)) {
  stop("Retained groundwater cache has no site_no field.", call. = FALSE)
}

wells_ll <- wells
if (!is.na(sf::st_crs(wells_ll))) {
  wells_ll <- sf::st_transform(wells_ll, 4326)
}
coords <- sf::st_coordinates(sf::st_geometry(wells_ll))
site_no <- as.character(wells_ll$site_no)
valid <- (
  !is.na(site_no) &
    nzchar(site_no) &
    is.finite(coords[, "X"]) &
    is.finite(coords[, "Y"])
)
site_no <- site_no[valid]
lng <- as.numeric(coords[valid, "X"])
lat <- as.numeric(coords[valid, "Y"])

coord_key <- paste0(sprintf("%.7f", lng), "_", sprintf("%.7f", lat))
ord <- order(coord_key, site_no, na.last = TRUE)
site_no <- site_no[ord]
lng <- lng[ord]
lat <- lat[ord]
coord_key <- coord_key[ord]

runs <- rle(coord_key)
loc_start <- cumsum(c(1L, head(runs$lengths, -1L)))
loc_count <- as.integer(runs$lengths)
loc_lng <- lng[loc_start]
loc_lat <- lat[loc_start]

nested_sizes <- loc_count[loc_count > 1L]
nested_distribution <- as.data.frame(table(nested_sizes))
names(nested_distribution) <- c("site_records_at_location", "location_count")
nested_distribution$site_records_at_location <- as.integer(
  as.character(nested_distribution$site_records_at_location)
)
nested_distribution$location_count <- as.integer(
  nested_distribution$location_count
)

counts <- data.frame(
  metric = c(
    "rows_with_valid_site_and_coordinate",
    "distinct_site_numbers",
    "unique_coordinate_locations_7dp",
    "nested_locations",
    "site_records_at_nested_locations",
    "non_nested_locations",
    "maximum_nested_location_size",
    "retained_r_object_bytes"
  ),
  value = c(
    length(site_no),
    length(unique(site_no)),
    length(loc_count),
    length(nested_sizes),
    sum(nested_sizes),
    sum(loc_count == 1L),
    if (length(nested_sizes)) max(nested_sizes) else 1L,
    as.numeric(object.size(wells))
  ),
  stringsAsFactors = FALSE
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

viewport_bounds <- function(center_lng, center_lat, zoom, width = 1200, height = 800) {
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

scenario_result <- function(name, center_lng, center_lat, zoom) {
  raw_bounds <- viewport_bounds(center_lng, center_lat, zoom)
  bounds <- buffer_bounds(raw_bounds, zoom)

  candidate <- which(
    loc_lng >= bounds[["west"]] &
      loc_lng <= bounds[["east"]] &
      loc_lat >= bounds[["south"]] &
      loc_lat <= bounds[["north"]]
  )

  descriptor_count <- length(candidate)
  cluster_count <- 0L
  exact_count <- length(candidate)

  if (zoom < 11 && length(candidate)) {
    projected <- world_pixel(loc_lng[candidate], loc_lat[candidate], zoom)
    radius <- cluster_radius(zoom)
    cells <- paste(
      floor(projected$x / radius),
      floor(projected$y / radius),
      sep = ":"
    )
    cell_location_count <- as.integer(table(cells))
    descriptor_count <- length(cell_location_count)
    cluster_count <- sum(cell_location_count > 1L)
    exact_count <- sum(cell_location_count == 1L)
  }

  data.frame(
    scenario = name,
    zoom = zoom,
    viewport_west = raw_bounds[["west"]],
    viewport_south = raw_bounds[["south"]],
    viewport_east = raw_bounds[["east"]],
    viewport_north = raw_bounds[["north"]],
    queried_buffered_locations = length(candidate),
    queried_individual_sites = sum(loc_count[candidate]),
    display_descriptors = descriptor_count,
    aggregate_objects = cluster_count,
    exact_location_objects = exact_count,
    current_global_markercluster_objects = length(loc_count),
    reduction_vs_global_percent = if (length(loc_count)) {
      100 * (1 - descriptor_count / length(loc_count))
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
}

scenarios <- rbind(
  scenario_result("statewide_home", -119.5, 37.15, 6),
  scenario_result("moderate_sacramento", -121.45, 38.55, 11),
  scenario_result("dense_fresno", -120.20, 36.70, 11),
  scenario_result("dense_kern_oil_field", -119.00, 35.40, 11),
  scenario_result("sparse_northeast_california", -120.50, 41.25, 11),
  scenario_result("dense_fresno_zoom_10", -120.20, 36.70, 10),
  scenario_result("dense_fresno_zoom_12", -120.20, 36.70, 12)
)

build_index <- function() {
  split(seq_along(loc_count), paste(floor(loc_lng / 0.25), floor(loc_lat / 0.25), sep = ":"))
}

index_elapsed <- replicate(
  20L,
  unname(system.time(build_index())[["elapsed"]])
)

query_descriptor_once <- function() {
  invisible(scenario_result("benchmark_dense_fresno", -120.20, 36.70, 11))
}
query_elapsed <- replicate(
  100L,
  unname(system.time(query_descriptor_once())[["elapsed"]])
)

algorithm_timing <- data.frame(
  operation = c(
    "build_0.25_degree_index_all_locations",
    "dense_zoom11_query_and_descriptor_count"
  ),
  repetitions = c(length(index_elapsed), length(query_elapsed)),
  median_elapsed_ms = c(median(index_elapsed), median(query_elapsed)) * 1000,
  worst_elapsed_ms = c(max(index_elapsed), max(query_elapsed)) * 1000,
  timing_scope = "R algorithm only; not browser/UI timing",
  stringsAsFactors = FALSE
)

write.csv(
  counts,
  file.path(output_dir, "counts.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  nested_distribution,
  file.path(output_dir, "nested_distribution.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  scenarios,
  file.path(output_dir, "viewport_architecture.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  algorithm_timing,
  file.path(output_dir, "algorithm_timing.csv"),
  row.names = FALSE,
  na = ""
)

summary_lines <- c(
  "# BRIM Local USGS groundwater focused profile",
  "",
  paste0("- Cache: `", cache_path, "`"),
  paste0("- Individual retained site records: ", format(length(site_no), big.mark = ",")),
  paste0("- Distinct site numbers: ", format(length(unique(site_no)), big.mark = ",")),
  paste0("- Unique 7-decimal coordinate locations: ", format(length(loc_count), big.mark = ",")),
  paste0("- Nested/co-located locations: ", format(length(nested_sizes), big.mark = ",")),
  paste0("- Site records at nested locations: ", format(sum(nested_sizes), big.mark = ",")),
  paste0("- Maximum nested group size: ", max(loc_count)),
  "",
  "The viewport table compares bounded display descriptors with the current",
  "global MarkerCluster object population. Algorithm timings are R-only focused",
  "measurements and must not be represented as browser interaction timings.",
  "",
  paste0("Output directory: `", output_dir, "`")
)
writeLines(summary_lines, file.path(output_dir, "summary.md"))

cat(paste(summary_lines, collapse = "\n"), "\n")
