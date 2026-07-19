# ==== 57_audit_cnrfc_river_reservoir_coordinate_alignment.R ================
##
## PURPOSE:
##   Diagnose CNRFC Local river/reservoir catalog coordinates vs Ops Live
##   active CNRFC river/reservoir forecast-point coordinates, with tight
##   distance bins and focus rows for map QA.
##
## WHY:
##   The Local catalog and Ops Live layer can share CNRFC IDs, but appear offset
##   on the map. This script checks whether the mismatch is ID-based, coordinate-
##   based, caused by clustering, or caused by symbolic/manual points. It also
##   writes detailed bins because a coarse 50 m threshold hides too much.
##
## OUTPUTS:
##   04_processed_data/qa/cnrfc_river_reservoir_coordinate_alignment_latest.csv
##   04_processed_data/qa/cnrfc_river_reservoir_coordinate_alignment_detail_bins_latest.csv
##   04_processed_data/qa/cnrfc_river_reservoir_coordinate_alignment_percentiles_latest.csv
##   04_processed_data/qa/cnrfc_river_reservoir_coordinate_alignment_offsets_gt10m_latest.csv
##   04_processed_data/qa/cnrfc_river_reservoir_coordinate_alignment_offsets_gt25m_latest.csv
##   04_processed_data/qa/cnrfc_river_reservoir_coordinate_alignment_top_offsets_latest.csv
##   04_processed_data/qa/cnrfc_river_reservoir_coordinate_alignment_focus_ids_latest.csv
##   04_processed_data/qa/cnrfc_active_xml_coordinate_field_inventory_latest.csv
##   04_processed_data/qa/cnrfc_active_xml_coordinate_field_inventory_focus_wide_latest.csv
##   plus timestamped copies.
## ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

if (file.exists("00_config/config_paths.r")) {
  source("00_config/config_paths.r")
}

pt_qa_dir <- if (exists("DIR") && !is.null(DIR$qa)) DIR$qa else file.path("04_processed_data", "qa")
pt_rds_dir <- if (exists("DIR") && !is.null(DIR$rds)) DIR$rds else file.path("04_processed_data", "rds")
dir.create(pt_qa_dir, recursive = TRUE, showWarnings = FALSE)

RUN_TS <- format(Sys.time(), "%Y%m%d_%H%M%S")
FOCUS_IDS <- unique(toupper(c(
  "HOUC1", "SACC1", "SAC", "ORDC1", "DWRC1", "FPOC1", "GEYC1", "LLKC1", "DRIC1",
  getOption("BRIM_CNRFC_COORD_AUDIT_FOCUS_IDS", character())
)))
FOCUS_IDS <- FOCUS_IDS[nzchar(FOCUS_IDS)]

pt_latest_file <- function(pattern, dir = pt_qa_dir) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  files[order(file.info(files)$mtime, decreasing = TRUE)][[1]]
}

pt_first_existing_path <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

pt_read_rds_safe <- function(path, label) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    message(label, " not found: ", path)
    return(tibble())
  }
  message("Reading ", label, ": ", path)
  x <- readRDS(path)
  if (inherits(x, "sf")) {
    x <- sf::st_drop_geometry(x)
  }
  tibble::as_tibble(x)
}

pt_pick_col <- function(x, candidates) {
  hit <- candidates[candidates %in% names(x)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

pt_clean_id <- function(x) {
  x <- as.character(x)
  x <- trimws(toupper(x))
  x[x %in% c("", "NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

pt_has_text <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

pt_first_nonempty <- function(x) {
  y <- as.character(x)
  y <- y[pt_has_text(y)]
  if (length(y) == 0) NA_character_ else y[[1]]
}

pt_haversine_m <- function(lat1, lon1, lat2, lon2) {
  r <- 6371008.8
  to_rad <- pi / 180
  p1 <- lat1 * to_rad
  p2 <- lat2 * to_rad
  dp <- (lat2 - lat1) * to_rad
  dl <- (lon2 - lon1) * to_rad
  a <- sin(dp / 2)^2 + cos(p1) * cos(p2) * sin(dl / 2)^2
  2 * r * atan2(sqrt(a), sqrt(1 - a))
}

pt_bearing_deg <- function(lat1, lon1, lat2, lon2) {
  to_rad <- pi / 180
  to_deg <- 180 / pi
  p1 <- lat1 * to_rad
  p2 <- lat2 * to_rad
  dl <- (lon2 - lon1) * to_rad
  y <- sin(dl) * cos(p2)
  x <- cos(p1) * sin(p2) - sin(p1) * cos(p2) * cos(dl)
  (atan2(y, x) * to_deg + 360) %% 360
}

pt_offset_detail_class <- function(m) {
  case_when(
    is.na(m) ~ "no_comparable_coordinates",
    m <= 1 ~ "exact_or_submeter_<=1m",
    m <= 5 ~ "very_close_1_5m",
    m <= 10 ~ "close_5_10m",
    m <= 25 ~ "low_offset_10_25m",
    m <= 50 ~ "modest_offset_25_50m",
    m <= 100 ~ "visible_offset_50_100m",
    m <= 250 ~ "concerning_offset_100_250m",
    m <= 500 ~ "large_offset_250_500m",
    m <= 1000 ~ "very_large_offset_500m_1km",
    TRUE ~ "extreme_offset_gt_1km"
  )
}

pt_make_point_index <- function(x, source_label) {
  if (nrow(x) == 0) {
    return(tibble(
      source = character(), cnrfc_id = character(), display_name = character(),
      lat = numeric(), lon = numeric(), coordinate_source_type = character(),
      role = character(), product_summary = character()
    ))
  }

  id_col <- pt_pick_col(x, c(
    "cnrfc_id", "nwsid", "nws_id", "NWSID", "id", "station_id",
    "feature_id", "forecast_point_id", "location_id", "site_id"
  ))
  name_col <- pt_pick_col(x, c(
    "display_name", "name", "station_name", "feature_name", "description",
    "site_name", "river_name", "reservoir_name", "popup_title"
  ))
  lat_col <- pt_pick_col(x, c("lat", "latitude", "y", "ycoord", "active_xml_lat"))
  lon_col <- pt_pick_col(x, c("lon", "lng", "longitude", "x", "xcoord", "active_xml_lon"))
  coord_src_col <- pt_pick_col(x, c("coordinate_source_type", "coordinate_source", "coord_source"))
  role_col <- pt_pick_col(x, c("map_point_role", "cnrfc_role", "point_role", "feature_type", "gtype", "raw_cnrfc_type"))
  product_col <- pt_pick_col(x, c("map_product_summary", "product_summary", "active_xml_product_labels", "active_xml_subtypes"))

  if (is.na(id_col)) {
    warning(source_label, ": no likely ID column found. Columns: ", paste(names(x), collapse = ", "))
    return(tibble(
      source = character(), cnrfc_id = character(), display_name = character(),
      lat = numeric(), lon = numeric(), coordinate_source_type = character(),
      role = character(), product_summary = character()
    ))
  }

  out <- tibble(
    source = source_label,
    cnrfc_id = pt_clean_id(x[[id_col]]),
    display_name = if (!is.na(name_col)) as.character(x[[name_col]]) else NA_character_,
    lat = if (!is.na(lat_col)) pt_num(x[[lat_col]]) else NA_real_,
    lon = if (!is.na(lon_col)) pt_num(x[[lon_col]]) else NA_real_,
    coordinate_source_type = if (!is.na(coord_src_col)) as.character(x[[coord_src_col]]) else NA_character_,
    role = if (!is.na(role_col)) as.character(x[[role_col]]) else NA_character_,
    product_summary = if (!is.na(product_col)) as.character(x[[product_col]]) else NA_character_
  ) %>%
    filter(!is.na(.data$cnrfc_id), !is.na(.data$lat), !is.na(.data$lon)) %>%
    group_by(.data$source, .data$cnrfc_id) %>%
    summarise(
      display_name = pt_first_nonempty(.data$display_name),
      lat = dplyr::first(.data$lat),
      lon = dplyr::first(.data$lon),
      coordinate_source_type = pt_first_nonempty(.data$coordinate_source_type),
      role = pt_first_nonempty(.data$role),
      product_summary = pt_first_nonempty(.data$product_summary),
      duplicate_rows = dplyr::n(),
      .groups = "drop"
    )

  out$display_name[is.na(out$display_name)] <- ""
  out$coordinate_source_type[is.na(out$coordinate_source_type)] <- ""
  out$role[is.na(out$role)] <- ""
  out$product_summary[is.na(out$product_summary)] <- ""
  out
}

local_path <- pt_first_existing_path(c(
  file.path("04_processed_data", "cache", "latest", "cnrfc_stream_map.rds"),
  file.path(pt_rds_dir, "cnrfc_stream_map.rds")
))

ops_active_path <- pt_first_existing_path(c(
  file.path(pt_rds_dir, "cnrfc_active_river_reservoir_forecast_points_map.rds")
))

matrix_path <- pt_first_existing_path(c(
  file.path(pt_rds_dir, "cnrfc_forecast_point_product_availability_matrix.rds")
))

catalog_review_path <- pt_first_existing_path(c(
  file.path(pt_rds_dir, "cnrfc_river_reservoir_catalog_review.rds")
))

local_raw <- pt_read_rds_safe(local_path, "Local CNRFC river/reservoir catalog map")
ops_raw <- pt_read_rds_safe(ops_active_path, "Ops active CNRFC river/reservoir map")
matrix_raw <- pt_read_rds_safe(matrix_path, "CNRFC forecast point matrix")
catalog_raw <- pt_read_rds_safe(catalog_review_path, "CNRFC catalog review")

message("\nColumn scan for coordinate-ish fields:")
for (nm in c("local_raw", "ops_raw", "matrix_raw", "catalog_raw")) {
  x <- get(nm)
  hits <- names(x)[grepl("lat|lon|lng|coord|print", names(x), ignore.case = TRUE)]
  message("  ", nm, ": ", if (length(hits)) paste(hits, collapse = ", ") else "<none>")
}

local_idx <- pt_make_point_index(local_raw, "local_catalog")
ops_idx <- pt_make_point_index(ops_raw, "ops_live")

alignment <- full_join(
  local_idx %>% rename(
    local_name = .data$display_name,
    local_lat = .data$lat,
    local_lon = .data$lon,
    local_coordinate_source_type = .data$coordinate_source_type,
    local_role = .data$role,
    local_product_summary = .data$product_summary,
    local_duplicate_rows = .data$duplicate_rows
  ) %>% select(-.data$source),
  ops_idx %>% rename(
    ops_name = .data$display_name,
    ops_lat = .data$lat,
    ops_lon = .data$lon,
    ops_coordinate_source_type = .data$coordinate_source_type,
    ops_role = .data$role,
    ops_product_summary = .data$product_summary,
    ops_duplicate_rows = .data$duplicate_rows
  ) %>% select(-.data$source),
  by = "cnrfc_id"
) %>%
  mutate(
    in_local_catalog = !is.na(.data$local_lat) & !is.na(.data$local_lon),
    in_ops_live = !is.na(.data$ops_lat) & !is.na(.data$ops_lon),
    is_manual_water_supply_index = str_detect(
      paste(
        coalesce(.data$ops_coordinate_source_type, ""),
        coalesce(.data$ops_role, ""),
        coalesce(.data$ops_product_summary, ""),
        coalesce(.data$ops_name, "")
      ),
      regex("manual_water_supply_index|water_supply_index|water supply index", ignore_case = TRUE)
    ),
    coordinate_offset_m = dplyr::if_else(
      .data$in_local_catalog & .data$in_ops_live,
      pt_haversine_m(.data$local_lat, .data$local_lon, .data$ops_lat, .data$ops_lon),
      NA_real_
    ),
    coordinate_offset_km = .data$coordinate_offset_m / 1000,
    local_minus_ops_lat_deg = dplyr::if_else(.data$in_local_catalog & .data$in_ops_live, .data$local_lat - .data$ops_lat, NA_real_),
    local_minus_ops_lon_deg = dplyr::if_else(.data$in_local_catalog & .data$in_ops_live, .data$local_lon - .data$ops_lon, NA_real_),
    offset_bearing_from_local_to_ops_deg = dplyr::if_else(
      .data$in_local_catalog & .data$in_ops_live & !is.na(.data$coordinate_offset_m) & .data$coordinate_offset_m > 0,
      pt_bearing_deg(.data$local_lat, .data$local_lon, .data$ops_lat, .data$ops_lon),
      NA_real_
    ),
    offset_class_detail = case_when(
      !.data$in_local_catalog ~ "ops_only",
      !.data$in_ops_live ~ "local_only",
      .data$is_manual_water_supply_index ~ paste0("manual_wsi_symbolic_point__", pt_offset_detail_class(.data$coordinate_offset_m)),
      TRUE ~ pt_offset_detail_class(.data$coordinate_offset_m)
    ),
    offset_class_legacy = case_when(
      !.data$in_local_catalog ~ "ops_only",
      !.data$in_ops_live ~ "local_only",
      is.na(.data$coordinate_offset_m) ~ "unknown",
      .data$coordinate_offset_m <= 50 ~ "same_or_near_same_<=50m",
      .data$coordinate_offset_m <= 250 ~ "small_offset_50_250m",
      .data$coordinate_offset_m <= 1000 ~ "moderate_offset_250m_1km",
      TRUE ~ "large_offset_gt_1km"
    ),
    offset_class = .data$offset_class_detail
  ) %>%
  arrange(desc(coalesce(.data$coordinate_offset_m, -1)))

write_dual_csv <- function(x, stem) {
  latest <- file.path(pt_qa_dir, paste0(stem, "_latest.csv"))
  ts <- file.path(pt_qa_dir, paste0(stem, "_", RUN_TS, ".csv"))
  readr::write_csv(x, latest)
  readr::write_csv(x, ts)
  invisible(latest)
}

alignment_latest <- write_dual_csv(alignment, "cnrfc_river_reservoir_coordinate_alignment")

alignment_detail_bins <- alignment %>%
  count(.data$offset_class_detail, .data$is_manual_water_supply_index, name = "ids") %>%
  arrange(.data$is_manual_water_supply_index, desc(.data$ids), .data$offset_class_detail)

detail_bins_latest <- write_dual_csv(alignment_detail_bins, "cnrfc_river_reservoir_coordinate_alignment_detail_bins")

coord_src_summary <- alignment %>%
  filter(.data$in_ops_live) %>%
  count(.data$ops_coordinate_source_type, .data$offset_class_detail, .data$is_manual_water_supply_index, name = "ids") %>%
  arrange(.data$ops_coordinate_source_type, .data$is_manual_water_supply_index, desc(.data$ids))

coord_src_latest <- write_dual_csv(coord_src_summary, "cnrfc_river_reservoir_coordinate_alignment_by_ops_source")

percentiles <- alignment %>%
  filter(.data$in_local_catalog, .data$in_ops_live, !is.na(.data$coordinate_offset_m)) %>%
  mutate(summary_group = if_else(.data$is_manual_water_supply_index, "manual water-supply index", "active XML forecast/reservoir point")) %>%
  group_by(.data$summary_group) %>%
  summarise(
    compared_ids = dplyr::n(),
    min_m = min(.data$coordinate_offset_m, na.rm = TRUE),
    p50_m = as.numeric(stats::quantile(.data$coordinate_offset_m, 0.50, na.rm = TRUE, names = FALSE)),
    p75_m = as.numeric(stats::quantile(.data$coordinate_offset_m, 0.75, na.rm = TRUE, names = FALSE)),
    p90_m = as.numeric(stats::quantile(.data$coordinate_offset_m, 0.90, na.rm = TRUE, names = FALSE)),
    p95_m = as.numeric(stats::quantile(.data$coordinate_offset_m, 0.95, na.rm = TRUE, names = FALSE)),
    p99_m = as.numeric(stats::quantile(.data$coordinate_offset_m, 0.99, na.rm = TRUE, names = FALSE)),
    max_m = max(.data$coordinate_offset_m, na.rm = TRUE),
    ids_gt_10m = sum(.data$coordinate_offset_m > 10, na.rm = TRUE),
    ids_gt_25m = sum(.data$coordinate_offset_m > 25, na.rm = TRUE),
    ids_gt_50m = sum(.data$coordinate_offset_m > 50, na.rm = TRUE),
    ids_gt_100m = sum(.data$coordinate_offset_m > 100, na.rm = TRUE),
    ids_gt_250m = sum(.data$coordinate_offset_m > 250, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(.data$summary_group)

percentiles_latest <- write_dual_csv(percentiles, "cnrfc_river_reservoir_coordinate_alignment_percentiles")

offsets_gt10 <- alignment %>%
  filter(.data$in_local_catalog, .data$in_ops_live, !.data$is_manual_water_supply_index, !is.na(.data$coordinate_offset_m), .data$coordinate_offset_m > 10) %>%
  arrange(desc(.data$coordinate_offset_m))

offsets_gt25 <- alignment %>%
  filter(.data$in_local_catalog, .data$in_ops_live, !.data$is_manual_water_supply_index, !is.na(.data$coordinate_offset_m), .data$coordinate_offset_m > 25) %>%
  arrange(desc(.data$coordinate_offset_m))

offsets_gt50 <- alignment %>%
  filter(.data$in_local_catalog, .data$in_ops_live, !.data$is_manual_water_supply_index, !is.na(.data$coordinate_offset_m), .data$coordinate_offset_m > 50) %>%
  arrange(desc(.data$coordinate_offset_m))

offsets_gt10_latest <- write_dual_csv(offsets_gt10, "cnrfc_river_reservoir_coordinate_alignment_offsets_gt10m")
offsets_gt25_latest <- write_dual_csv(offsets_gt25, "cnrfc_river_reservoir_coordinate_alignment_offsets_gt25m")
offsets_gt50_latest <- write_dual_csv(offsets_gt50, "cnrfc_river_reservoir_coordinate_alignment_offsets_gt50m")

top_offsets <- alignment %>%
  filter(.data$in_local_catalog, .data$in_ops_live) %>%
  arrange(desc(.data$coordinate_offset_m)) %>%
  slice_head(n = 100)

top_offsets_latest <- write_dual_csv(top_offsets, "cnrfc_river_reservoir_coordinate_alignment_top_offsets")

focus_rows <- alignment %>%
  filter(
    .data$cnrfc_id %in% FOCUS_IDS |
      str_detect(coalesce(.data$local_name, ""), regex("I Street|Sacramento|Shasta|Dwinnell|Cache|Hough|Freeport", ignore_case = TRUE)) |
      str_detect(coalesce(.data$ops_name, ""), regex("I Street|Sacramento|Shasta|Dwinnell|Cache|Hough|Freeport", ignore_case = TRUE))
  ) %>%
  arrange(.data$cnrfc_id)

focus_latest <- write_dual_csv(focus_rows, "cnrfc_river_reservoir_coordinate_alignment_focus_ids")

message("\nCoordinate alignment detail bins:")
print(alignment_detail_bins, n = Inf)
message("\nCoordinate offset percentiles:")
print(percentiles, n = Inf, width = Inf)
message("\nOps coordinate-source detail summary:")
print(coord_src_summary, n = Inf)
message("\nFocus rows:")
print(
  focus_rows %>%
    select(
      .data$cnrfc_id, .data$local_name, .data$ops_name, .data$local_lat, .data$local_lon,
      .data$ops_lat, .data$ops_lon, .data$coordinate_offset_m,
      .data$offset_class_detail, .data$is_manual_water_supply_index,
      .data$ops_coordinate_source_type, .data$ops_role, .data$ops_product_summary
    ),
  n = Inf,
  width = Inf
)
message("\nNon-manual active XML offsets >10 m:")
print(
  offsets_gt10 %>%
    select(
      .data$cnrfc_id, .data$local_name, .data$ops_name, .data$local_lat, .data$local_lon,
      .data$ops_lat, .data$ops_lon, .data$coordinate_offset_m,
      .data$offset_class_detail, .data$offset_bearing_from_local_to_ops_deg,
      .data$ops_coordinate_source_type, .data$ops_role
    ) %>%
    slice_head(n = 50),
  n = 50,
  width = Inf
)

# Optional live XML field inventory. This is diagnostic; it does not change files used by the map.
pt_xml_inventory <- function(focus_ids = FOCUS_IDS) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    message("xml2 not available; skipping live XML coordinate-field inventory.")
    return(tibble())
  }
  endpoints <- tibble::tribble(
    ~endpoint_key, ~url,
    "river_forecast_combined",  "https://www.cnrfc.noaa.gov/data/kml/riverFcst.xml",
    "reservoir_inflows",        "https://www.cnrfc.noaa.gov/data/kml/rsvrInflow.xml",
    "reservoir_releases",       "https://www.cnrfc.noaa.gov/data/kml/rsvrRelease.xml",
    "ensemble_forecast_points", "https://www.cnrfc.noaa.gov/data/kml/ensPoints.xml"
  )

  rows <- list()
  row_i <- 0L
  for (ep_i in seq_len(nrow(endpoints))) {
    ep <- endpoints[ep_i, ]
    message("Fetching XML field inventory: ", ep$endpoint_key, " -> ", ep$url)
    doc <- tryCatch(xml2::read_xml(ep$url), error = function(e) {
      message("  XML fetch/parse failed: ", conditionMessage(e))
      NULL
    })
    if (is.null(doc)) next
    nodes <- xml2::xml_find_all(doc, ".//*")
    for (node_i in seq_along(nodes)) {
      node <- nodes[[node_i]]
      attrs <- xml2::xml_attrs(node)
      kids <- xml2::xml_children(node)
      kid_names <- xml2::xml_name(kids)
      kid_vals <- vapply(kids, function(k) xml2::xml_text(k, trim = TRUE), character(1))
      blob <- paste(
        xml2::xml_name(node),
        paste(names(attrs), attrs, collapse = " "),
        paste(kid_names, kid_vals, collapse = " "),
        xml2::xml_text(node, trim = TRUE),
        collapse = " "
      )
      blob_upper <- toupper(blob)
      matched_ids <- focus_ids[vapply(focus_ids, function(id) stringr::str_detect(blob_upper, stringr::fixed(id)), logical(1))]
      has_coordish <- any(grepl("lat|lon|lng|coord|print", c(names(attrs), kid_names), ignore.case = TRUE)) ||
        grepl("lat|lon|lng|coord|print", blob, ignore.case = TRUE)
      if (length(matched_ids) == 0 && !has_coordish) next
      if (length(matched_ids) == 0 && has_coordish) {
        idish <- any(grepl("id|name|nws|location|lid", c(names(attrs), kid_names), ignore.case = TRUE))
        if (!idish) next
      }
      fields <- tibble(
        field_name = c(names(attrs), kid_names),
        field_value = c(as.character(attrs), kid_vals)
      ) %>%
        filter(
          grepl("id|name|river|res|station|lat|lon|lng|coord|print|elev", .data$field_name, ignore.case = TRUE) |
            vapply(.data$field_value, function(v) any(vapply(focus_ids, function(id) stringr::str_detect(toupper(as.character(v)), stringr::fixed(id)), logical(1))), logical(1))
        )
      if (nrow(fields) == 0) next
      for (j in seq_len(nrow(fields))) {
        row_i <- row_i + 1L
        rows[[row_i]] <- tibble(
          run_timestamp = RUN_TS,
          endpoint_key = ep$endpoint_key,
          endpoint_url = ep$url,
          node_index = node_i,
          node_name = xml2::xml_name(node),
          matched_focus_ids = paste(matched_ids, collapse = ";"),
          field_name = fields$field_name[[j]],
          field_value = fields$field_value[[j]]
        )
      }
    }
  }
  if (length(rows) == 0) tibble() else bind_rows(rows)
}

xml_inventory <- pt_xml_inventory()
xml_latest <- write_dual_csv(xml_inventory, "cnrfc_active_xml_coordinate_field_inventory")

xml_focus_wide <- tibble()
if (nrow(xml_inventory) > 0 && requireNamespace("tidyr", quietly = TRUE)) {
  xml_focus_wide <- xml_inventory %>%
    filter(pt_has_text(.data$matched_focus_ids)) %>%
    mutate(field_name = make.names(.data$field_name)) %>%
    group_by(.data$endpoint_key, .data$endpoint_url, .data$node_index, .data$node_name, .data$matched_focus_ids, .data$field_name) %>%
    summarise(field_value = pt_first_nonempty(.data$field_value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = field_name, values_from = field_value) %>%
    arrange(.data$matched_focus_ids, .data$endpoint_key, .data$node_index)
}
xml_focus_wide_latest <- write_dual_csv(xml_focus_wide, "cnrfc_active_xml_coordinate_field_inventory_focus_wide")

if (nrow(xml_focus_wide) > 0) {
  message("\nXML coordinate/id field inventory focus rows, wide:")
  print(xml_focus_wide, n = 80, width = Inf)
} else if (nrow(xml_inventory) > 0) {
  message("\nXML coordinate/id field inventory focus rows, long:")
  print(
    xml_inventory %>%
      filter(pt_has_text(.data$matched_focus_ids)) %>%
      select(.data$endpoint_key, .data$matched_focus_ids, .data$node_name, .data$field_name, .data$field_value) %>%
      slice_head(n = 80),
    n = 80,
    width = Inf
  )
}

message("\nCNRFC river/reservoir coordinate alignment audit complete.")
message("  Alignment:       ", alignment_latest)
message("  Detail bins:     ", detail_bins_latest)
message("  Percentiles:     ", percentiles_latest)
message("  Offsets >10m:    ", offsets_gt10_latest)
message("  Offsets >25m:    ", offsets_gt25_latest)
message("  Offsets >50m:    ", offsets_gt50_latest)
message("  Top offsets:     ", top_offsets_latest)
message("  Focus IDs:       ", focus_latest)
message("  XML fields:      ", xml_latest)
message("  XML focus wide:  ", xml_focus_wide_latest)
