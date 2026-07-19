# ==== 32_audit_usgs_groundwater_static_live_architecture.r ==================
##
## PURPOSE:
##   Non-destructive audit of BRIM's USGS groundwater architecture after the
##   Ops Live groundwater refresh work.
##
## WHAT THIS DOES:
##   - Compares the static USGS groundwater backbone against the newer live
##     groundwater candidate/feed inputs.
##   - Identifies live/API-discovered wells that are missing from the static
##     map-ready groundwater layer.
##   - Summarizes exact-coordinate / co-located / nested well groups in both
##     static and live groundwater layers.
##   - Inventories BRIM USGS scripts for legacy dataRetrieval calls that should
##     be modernized in future patches.
##   - Writes QA CSVs only. It does NOT overwrite USGS_GW_final.rds,
##     usgs_wells_map.rds, the core map cache, or the final HTML.
##
## DESIGN DECISION:
##   This is intentionally RF038's safe first step. Static groundwater changes
##   should be appender-first and QA-driven, not destructive. Use the outputs of
##   this script to decide what RF039/RF040 should promote into the static layer.
##
## HOW TO RUN FROM PortaTreasure2 ROOT:
##   source("02_preprocess/32_audit_usgs_groundwater_static_live_architecture.r")
## ============================================================================

# ---- 1. Packages ------------------------------------------------------------

required_pkgs <- c("dplyr", "readr", "tibble", "jsonlite")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(jsonlite)
})

sf_available <- requireNamespace("sf", quietly = TRUE)

# ---- 2. Paths ---------------------------------------------------------------

qa_dir <- Sys.getenv(
  "USGS_GW_ARCH_QA_DIR",
  unset = "04_processed_data/qa"
)

dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

static_final_rds <- Sys.getenv(
  "USGS_GW_STATIC_FINAL_RDS",
  unset = "04_processed_data/rds/USGS_GW_final.rds"
)

static_map_rds <- Sys.getenv(
  "USGS_GW_STATIC_MAP_RDS",
  unset = "04_processed_data/cache/latest/usgs_wells_map.rds"
)

live_candidate_csv <- Sys.getenv(
  "USGS_GW_LIVE_CANDIDATE_CSV",
  unset = "brim-live-data-feeds/data/input/usgs_groundwater_latest_index_ca.csv"
)

history_summary_csv <- Sys.getenv(
  "USGS_GW_HISTORY_SUMMARY_CSV",
  unset = "brim-live-data-feeds/data/input/usgs_groundwater_history_summary_ca.csv"
)

live_geojson <- Sys.getenv(
  "USGS_GW_LIVE_GEOJSON",
  unset = "brim-live-data-feeds/docs/data/usgs_groundwater_latest_ca.geojson"
)

source_scan_dirs <- c(
  "02_preprocess",
  "brim-live-data-feeds/scripts"
)

run_time_utc <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

# ---- 3. Small helpers -------------------------------------------------------

pt_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

pt_site_no <- function(x) {
  x <- pt_chr(x)
  x <- gsub("\\.0$", "", x)
  x <- gsub("^USGS-", "", x, ignore.case = TRUE)
  x <- gsub("[^0-9]", "", x)
  x[nchar(x) == 0] <- NA_character_
  x
}

pt_first_existing <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) rep(NA_character_, nrow(df)) else df[[hit[[1]]]]
}

pt_first_existing_num <- function(df, candidates) {
  pt_num(pt_first_existing(df, candidates))
}

pt_read_table <- function(path, label) {
  if (!file.exists(path)) {
    warning("Missing ", label, ": ", path)
    return(tibble::tibble())
  }

  x <- readRDS(path)

  coords <- NULL
  if (sf_available && inherits(x, "sf")) {
    coords <- tryCatch(sf::st_coordinates(x), error = function(e) NULL)
    x <- sf::st_drop_geometry(x)
  }

  x <- tibble::as_tibble(x)

  if (!"longitude" %in% names(x) && !is.null(coords) && ncol(coords) >= 1) {
    x$longitude <- coords[, 1]
  }
  if (!"latitude" %in% names(x) && !is.null(coords) && ncol(coords) >= 2) {
    x$latitude <- coords[, 2]
  }

  x
}

pt_read_csv_if_exists <- function(path, label) {
  if (!file.exists(path)) {
    warning("Missing ", label, ": ", path)
    return(tibble::tibble())
  }

  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
}

pt_standardize_wells <- function(df, source_label) {
  if (nrow(df) == 0) return(tibble::tibble())

  out <- df |>
    mutate(
      source_label = source_label,
      site_no = pt_site_no(pt_first_existing(cur_data_all(), c("site_no", "monitoring_location_id", "monitoring_location_number"))),
      station_nm = pt_chr(pt_first_existing(cur_data_all(), c("station_nm", "monitoring_location_name", "name", "site_name"))),
      latitude = pt_first_existing_num(cur_data_all(), c("latitude", "lat", "dec_lat_va", "location_latitude")),
      longitude = pt_first_existing_num(cur_data_all(), c("longitude", "lon", "dec_long_va", "location_longitude")),
      status = pt_chr(pt_first_existing(cur_data_all(), c("status", "site_status", "siteStatus", "siteStatus_download", "monitoring_location_status", "active_status"))),
      mr_wl_ft_bgs = pt_first_existing_num(cur_data_all(), c("latest_wl_ft_bgs", "mr_wl_ft_bgs", "lev_va", "wl_ft_bgs", "water_level_ft_bgs")),
      mr_wl_date = pt_chr(pt_first_existing(cur_data_all(), c("latest_wl_date", "mr_wl_date", "lev_dt", "measurement_date", "date"))),
      well_depth_ft = pt_first_existing_num(cur_data_all(), c("well_depth_ft", "well_constructed_depth", "well_depth", "well_depth_va")),
      hole_depth_ft = pt_first_existing_num(cur_data_all(), c("hole_depth_ft", "hole_constructed_depth", "hole_depth", "hole_depth_va")),
      aqfr_cd = pt_chr(pt_first_existing(cur_data_all(), c("aqfr_cd", "aquifer_code"))),
      aqfr_type_cd = pt_chr(pt_first_existing(cur_data_all(), c("aqfr_type_cd", "aquifer_type_code"))),
      nat_aqfr_cd = pt_chr(pt_first_existing(cur_data_all(), c("nat_aqfr_cd", "national_aquifer_code"))),
      param_list = pt_chr(pt_first_existing(cur_data_all(), c("param_list", "parameters", "parameter_list"))),
      measurements = pt_chr(pt_first_existing(cur_data_all(), c("measurements", "measurement_list", "common_data")))
    ) |>
    filter(!is.na(.data$site_no)) |>
    mutate(
      lon_key = ifelse(is.na(.data$longitude), NA_character_, sprintf("%.7f", .data$longitude)),
      lat_key = ifelse(is.na(.data$latitude), NA_character_, sprintf("%.7f", .data$latitude))
    ) |>
    distinct(.data$site_no, .keep_all = TRUE)

  out
}

pt_nested_groups <- function(df, source_label) {
  if (nrow(df) == 0 || !all(c("lon_key", "lat_key") %in% names(df))) {
    return(tibble::tibble())
  }

  df |>
    filter(!is.na(.data$lon_key), !is.na(.data$lat_key)) |>
    group_by(.data$lon_key, .data$lat_key) |>
    summarise(
      source_label = source_label,
      n_wells = n(),
      sample_site_nos = paste(head(.data$site_no, 12), collapse = "; "),
      sample_station_names = paste(head(na.omit(.data$station_nm), 6), collapse = " | "),
      min_wl_ft_bgs = suppressWarnings(min(.data$mr_wl_ft_bgs, na.rm = TRUE)),
      max_wl_ft_bgs = suppressWarnings(max(.data$mr_wl_ft_bgs, na.rm = TRUE)),
      min_well_depth_ft = suppressWarnings(min(.data$well_depth_ft, na.rm = TRUE)),
      max_well_depth_ft = suppressWarnings(max(.data$well_depth_ft, na.rm = TRUE)),
      statuses = paste(sort(unique(na.omit(.data$status))), collapse = "; "),
      .groups = "drop"
    ) |>
    mutate(
      min_wl_ft_bgs = ifelse(is.finite(.data$min_wl_ft_bgs), .data$min_wl_ft_bgs, NA_real_),
      max_wl_ft_bgs = ifelse(is.finite(.data$max_wl_ft_bgs), .data$max_wl_ft_bgs, NA_real_),
      min_well_depth_ft = ifelse(is.finite(.data$min_well_depth_ft), .data$min_well_depth_ft, NA_real_),
      max_well_depth_ft = ifelse(is.finite(.data$max_well_depth_ft), .data$max_well_depth_ft, NA_real_)
    ) |>
    filter(.data$n_wells > 1) |>
    arrange(desc(.data$n_wells), .data$lon_key, .data$lat_key)
}

pt_count_hits <- function(txt, pattern) {
  if (length(txt) == 0 || is.na(pattern) || !nzchar(pattern)) return(0L)
  m <- gregexpr(pattern, txt, fixed = TRUE)
  sum(vapply(m, function(z) if (z[[1]] < 0) 0L else length(z), integer(1)))
}

pt_inventory_dataretrieval_calls <- function() {
  files <- unlist(lapply(source_scan_dirs, function(d) {
    if (!dir.exists(d)) return(character(0))
    list.files(d, pattern = "(?i)usgs|nwis|waterdata", full.names = TRUE, recursive = FALSE)
  }))

  files <- files[file.exists(files)]
  files <- files[grepl("\\.(r|R)$", files)]

  if (length(files) == 0) {
    return(tibble::tibble())
  }

  legacy_calls <- c("whatNWISdata", "readNWISsite", "readNWISuv", "readNWISdv")
  modern_calls <- c(
    "read_waterdata_field_measurements",
    "read_waterdata_monitoring_location",
    "read_waterdata_latest_continuous",
    "read_waterdata_continuous",
    "read_waterdata_daily",
    "read_waterdata_ts_meta"
  )

  bind_rows(lapply(files, function(f) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    legacy_counts <- vapply(legacy_calls, function(p) pt_count_hits(txt, p), integer(1))
    modern_counts <- vapply(modern_calls, function(p) pt_count_hits(txt, p), integer(1))

    tibble::tibble(
      file = f,
      legacy_total = sum(legacy_counts),
      modern_total = sum(modern_counts),
      whatNWISdata = legacy_counts[["whatNWISdata"]],
      readNWISsite = legacy_counts[["readNWISsite"]],
      readNWISuv = legacy_counts[["readNWISuv"]],
      readNWISdv = legacy_counts[["readNWISdv"]],
      read_waterdata_field_measurements = modern_counts[["read_waterdata_field_measurements"]],
      read_waterdata_monitoring_location = modern_counts[["read_waterdata_monitoring_location"]],
      read_waterdata_latest_continuous = modern_counts[["read_waterdata_latest_continuous"]],
      read_waterdata_continuous = modern_counts[["read_waterdata_continuous"]],
      read_waterdata_daily = modern_counts[["read_waterdata_daily"]],
      read_waterdata_ts_meta = modern_counts[["read_waterdata_ts_meta"]],
      modernization_priority = dplyr::case_when(
        legacy_counts[["whatNWISdata"]] > 0 | legacy_counts[["readNWISsite"]] > 0 ~ "high: legacy site/metadata discovery",
        legacy_counts[["readNWISuv"]] > 0 | legacy_counts[["readNWISdv"]] > 0 ~ "high: legacy timeseries retrieval",
        sum(legacy_counts) == 0 & sum(modern_counts) > 0 ~ "modern API already used",
        TRUE ~ "no obvious dataRetrieval calls found"
      )
    )
  })) |>
    arrange(desc(.data$legacy_total), .data$file)
}

# ---- 4. Read datasets -------------------------------------------------------

message("Reading static and live groundwater products...")

static_final <- pt_standardize_wells(
  pt_read_table(static_final_rds, "static final groundwater RDS"),
  "static_final_USGS_GW_final"
)

static_map <- pt_standardize_wells(
  pt_read_table(static_map_rds, "static map-ready groundwater RDS"),
  "static_map_usgs_wells_map"
)

live_candidates_raw <- pt_read_csv_if_exists(live_candidate_csv, "live groundwater candidate CSV")
live_candidates <- pt_standardize_wells(live_candidates_raw, "ops_live_candidate_csv")

history_summary <- pt_read_csv_if_exists(history_summary_csv, "groundwater history summary CSV") |>
  mutate(site_no = pt_site_no(.data$site_no))

# ---- 5. Overlap / append-preview QA ----------------------------------------

static_reference <- if (nrow(static_map) > 0) static_map else static_final

live_only <- live_candidates |>
  anti_join(static_reference |> select(.data$site_no), by = "site_no") |>
  left_join(
    history_summary |>
      select(any_of(c(
        "site_no", "hist_record_count", "hist_start_date", "hist_end_date",
        "hist_sufficient_for_plot", "hist_sufficient_for_por_percentile",
        "hist_sufficient_for_seasonal_percentile"
      ))),
    by = "site_no"
  ) |>
  arrange(.data$source_label, .data$site_no)

overlap <- live_candidates |>
  inner_join(
    static_reference |>
      select(
        site_no,
        static_station_nm = station_nm,
        static_latitude = latitude,
        static_longitude = longitude,
        static_status = status,
        static_mr_wl_ft_bgs = mr_wl_ft_bgs,
        static_mr_wl_date = mr_wl_date,
        static_well_depth_ft = well_depth_ft,
        static_hole_depth_ft = hole_depth_ft,
        static_nat_aqfr_cd = nat_aqfr_cd
      ),
    by = "site_no"
  ) |>
  transmute(
    site_no,
    live_station_nm = .data$station_nm,
    static_station_nm,
    live_status = .data$status,
    static_status,
    live_mr_wl_ft_bgs = .data$mr_wl_ft_bgs,
    static_mr_wl_ft_bgs,
    live_mr_wl_date = .data$mr_wl_date,
    static_mr_wl_date,
    live_well_depth_ft = .data$well_depth_ft,
    static_well_depth_ft,
    live_hole_depth_ft = .data$hole_depth_ft,
    static_hole_depth_ft,
    live_nat_aqfr_cd = .data$nat_aqfr_cd,
    static_nat_aqfr_cd,
    wl_abs_diff_ft = abs(.data$live_mr_wl_ft_bgs - .data$static_mr_wl_ft_bgs),
    name_differs = !is.na(.data$live_station_nm) & !is.na(.data$static_station_nm) & .data$live_station_nm != .data$static_station_nm,
    live_fills_static_blank_name = !is.na(.data$live_station_nm) & (is.na(.data$static_station_nm) | .data$static_station_nm == ""),
    live_fills_static_blank_well_depth = !is.na(.data$live_well_depth_ft) & is.na(.data$static_well_depth_ft),
    live_fills_static_blank_hole_depth = !is.na(.data$live_hole_depth_ft) & is.na(.data$static_hole_depth_ft)
  ) |>
  arrange(desc(.data$wl_abs_diff_ft), .data$site_no)

# ---- 6. Nested/co-located coordinate QA ------------------------------------

static_nested <- pt_nested_groups(static_reference, "static_reference")
live_nested <- pt_nested_groups(live_candidates, "ops_live_candidates")

# ---- 7. dataRetrieval modernization inventory ------------------------------

modernization_inventory <- pt_inventory_dataretrieval_calls()

# ---- 8. Summary table -------------------------------------------------------

summary_tbl <- tibble::tibble(
  metric = c(
    "run time UTC",
    "static final USGS_GW_final rows",
    "static map-ready usgs_wells_map rows",
    "static reference rows used for comparison",
    "live candidate rows",
    "live candidates also in static reference",
    "live candidates missing from static reference",
    "history-summary rows",
    "static exact-coordinate nested groups",
    "static records inside nested groups",
    "live exact-coordinate nested groups",
    "live records inside nested groups",
    "USGS scripts scanned for dataRetrieval calls",
    "USGS scripts with legacy dataRetrieval calls",
    "USGS scripts using modern read_waterdata calls"
  ),
  value = as.character(c(
    run_time_utc,
    nrow(static_final),
    nrow(static_map),
    nrow(static_reference),
    nrow(live_candidates),
    nrow(overlap),
    nrow(live_only),
    nrow(history_summary),
    nrow(static_nested),
    sum(static_nested$n_wells, na.rm = TRUE),
    nrow(live_nested),
    sum(live_nested$n_wells, na.rm = TRUE),
    nrow(modernization_inventory),
    sum(modernization_inventory$legacy_total > 0, na.rm = TRUE),
    sum(modernization_inventory$modern_total > 0, na.rm = TRUE)
  ))
)

# ---- 9. Write outputs -------------------------------------------------------

out_summary <- file.path(qa_dir, "usgs_groundwater_architecture_audit_summary.csv")
out_live_only <- file.path(qa_dir, "usgs_groundwater_live_only_append_preview.csv")
out_overlap <- file.path(qa_dir, "usgs_groundwater_static_live_overlap_preview.csv")
out_static_nested <- file.path(qa_dir, "usgs_groundwater_static_nested_coord_groups.csv")
out_live_nested <- file.path(qa_dir, "usgs_groundwater_live_nested_coord_groups.csv")
out_inventory <- file.path(qa_dir, "usgs_dataretrieval_modernization_inventory.csv")

readr::write_csv(summary_tbl, out_summary)
readr::write_csv(live_only, out_live_only)
readr::write_csv(overlap, out_overlap)
readr::write_csv(static_nested, out_static_nested)
readr::write_csv(live_nested, out_live_nested)
readr::write_csv(modernization_inventory, out_inventory)

message("Saved architecture audit summary: ", out_summary)
message("Saved live-only append preview: ", out_live_only)
message("Saved static/live overlap preview: ", out_overlap)
message("Saved static nested-coordinate groups: ", out_static_nested)
message("Saved live nested-coordinate groups: ", out_live_nested)
message("Saved dataRetrieval modernization inventory: ", out_inventory)
message("USGS groundwater architecture audit complete.")
print(summary_tbl)
