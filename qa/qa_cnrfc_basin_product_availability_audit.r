# ==== qa_cnrfc_basin_product_availability_audit.r ===========================
##
## PURPOSE:
##   Build a sampled, live CNRFC basin product-availability matrix.
##
## WHY THIS EXISTS:
##   The earlier CNRFC product-intelligence audits intentionally built a broad
##   candidate universe.  This script starts turning candidate links into more
##   useful BRIM knowledge: for a sampled set of CNRFC basin IDs, which CNRFC
##   basin-scale products actually appear available right now?
##
##   This is audit-only.  It does not change map popups, symbology, caches, or
##   HTML output.
##
## IMPORTANT DESIGN NOTE:
##   A CNRFC/NWS-style ID can appear in multiple contexts: basin polygon, river
##   point, reservoir point, precip catalog, existing BRIM reservoir crosswalk,
##   etc.  This script starts from basin/FNF-basin feature rows but preserves
##   ID-collision context so later map design can avoid treating same-ID objects
##   as identical.
##
## OUTPUTS:
##   04_processed_data/qa/cnrfc_basin_product_availability_long_<timestamp>.csv
##   04_processed_data/qa/cnrfc_basin_product_availability_matrix_<timestamp>.csv
##   04_processed_data/qa/cnrfc_basin_product_availability_bins_<timestamp>.csv
##   04_processed_data/qa/cnrfc_basin_water_resources_update_matches_<timestamp>.csv
##   04_processed_data/qa/cnrfc_basin_product_availability_summary_<timestamp>.csv
##
## NOTE:
##   If available, this script now reuses the latest
##   cnrfc_wru_xml_product_matrix_*.csv written by
##   qa_cnrfc_water_resources_update_kml_audit().  That XML matrix is treated
##   as stronger WR Update evidence than simple page-text ID matching.

# ==== 1. Project and package setup ==========================================

if (!dir.exists("00_config") || !dir.exists("04_processed_data")) {
  stop(
    "Run this script from the BRIM project root. Expected folders like ",
    "00_config/ and 04_processed_data/ were not found."
  )
}

source("00_config/config_paths.r")
source("03_functions/cache_helpers.r")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(tidyr)
})

RUN_TS <- make_timestamp()

pt_log <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " | ", ...)
}

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
}

pt_chr <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

pt_clean_id <- function(x) {
  x <- toupper(pt_chr(x))
  x[x %in% c("", "NA", "N/A", "NULL", "NONE", "<BLANK>")] <- NA_character_
  x
}

pt_first_nonblank <- function(x) {
  x <- pt_chr(x)
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else x[[1]]
}

pt_first_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[[1]]
}

pt_latest_file <- function(pattern, dir = DIR$qa) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  files[order(file.info(files)$mtime, decreasing = TRUE)][[1]]
}

pt_url_id <- function(id) {
  utils::URLencode(pt_clean_id(id), reserved = TRUE)
}

pt_compact_text <- function(txt, max_n = 220) {
  txt <- gsub("<script[\\s\\S]*?</script>", " ", txt, ignore.case = TRUE)
  txt <- gsub("<style[\\s\\S]*?</style>", " ", txt, ignore.case = TRUE)
  txt <- gsub("<[^>]+>", " ", txt)
  txt <- gsub("&nbsp;", " ", txt, fixed = TRUE)
  txt <- gsub("\\s+", " ", txt)
  txt <- trimws(txt)
  substr(txt, 1, max_n)
}

# ==== 2. Options from run_build_map.r =======================================

MAX_IDS_OPT <- getOption("BRIM_CNRFC_BASIN_MAX_IDS", 25L)
MAX_IDS <- suppressWarnings(as.integer(MAX_IDS_OPT))
if (length(MAX_IDS_OPT) == 1 && is.infinite(MAX_IDS_OPT)) MAX_IDS <- Inf
if (length(MAX_IDS) == 0 || is.na(MAX_IDS) || MAX_IDS < 0) MAX_IDS <- 25L

EXPLICIT_IDS <- pt_clean_id(getOption(
  "BRIM_CNRFC_BASIN_IDS",
  c("PITC1", "NWMC1", "ORDC1", "HLEC1", "NMSC1", "SCSC1", "SHDC1", "TMDC1")
))
EXPLICIT_IDS <- EXPLICIT_IDS[!is.na(EXPLICIT_IDS)]

SAMPLE_STRATEGY <- tolower(pt_chr(getOption("BRIM_CNRFC_BASIN_SAMPLE_STRATEGY", "priority"))[[1]])
if (!SAMPLE_STRATEGY %in% c("priority", "wru_mixed", "alphabetical", "random", "all")) {
  SAMPLE_STRATEGY <- "priority"
}

SAMPLE_SEED <- suppressWarnings(as.integer(getOption("BRIM_CNRFC_BASIN_SAMPLE_SEED", 20260624L)))
if (length(SAMPLE_SEED) == 0 || is.na(SAMPLE_SEED)) SAMPLE_SEED <- 20260624L

PRODUCT_MODE <- tolower(pt_chr(getOption("BRIM_CNRFC_BASIN_PRODUCT_MODE", "full"))[[1]])
if (!PRODUCT_MODE %in% c("full", "high_value", "wy_focus", "minimal")) {
  PRODUCT_MODE <- "full"
}

PRODUCT_IDS_OPT <- getOption("BRIM_CNRFC_BASIN_PRODUCT_IDS", NULL)
if (is.null(PRODUCT_IDS_OPT)) {
  PRODUCT_IDS <- switch(
    PRODUCT_MODE,
    full = 1:12,
    high_value = c(1L, 3L, 4L, 9L, 10L, 11L, 12L),
    wy_focus = c(4L, 9L, 10L, 11L),
    minimal = integer(0),
    1:12
  )
} else {
  PRODUCT_IDS <- suppressWarnings(as.integer(PRODUCT_IDS_OPT))
}
PRODUCT_IDS <- sort(unique(PRODUCT_IDS[!is.na(PRODUCT_IDS) & PRODUCT_IDS > 0]))

CHECK_WRH <- isTRUE(getOption("BRIM_CNRFC_BASIN_CHECK_WEATHER_GOV", TRUE))
CHECK_WRU <- isTRUE(getOption("BRIM_CNRFC_BASIN_CHECK_WATER_RESOURCES_UPDATE", TRUE))
USE_WRU_XML_PRODUCTS <- isTRUE(getOption("BRIM_CNRFC_BASIN_USE_WRU_XML_PRODUCTS", TRUE))
REQUEST_DELAY_SEC <- suppressWarnings(as.numeric(getOption("BRIM_CNRFC_BASIN_REQUEST_DELAY_SEC", 0.15)))
if (is.na(REQUEST_DELAY_SEC) || REQUEST_DELAY_SEC < 0) REQUEST_DELAY_SEC <- 0

RESUME_FROM_CACHE <- isTRUE(getOption("BRIM_CNRFC_BASIN_RESUME_FROM_CACHE", TRUE))
FORCE_REFRESH <- isTRUE(getOption("BRIM_CNRFC_BASIN_FORCE_REFRESH", FALSE))
FORCE_REFRESH_IDS <- pt_clean_id(getOption("BRIM_CNRFC_BASIN_FORCE_REFRESH_IDS", character(0)))
FORCE_REFRESH_IDS <- FORCE_REFRESH_IDS[!is.na(FORCE_REFRESH_IDS)]
CACHE_MAX_AGE_DAYS <- suppressWarnings(as.numeric(getOption("BRIM_CNRFC_BASIN_CACHE_MAX_AGE_DAYS", 30)))
if (length(CACHE_MAX_AGE_DAYS) == 0 || is.na(CACHE_MAX_AGE_DAYS) || CACHE_MAX_AGE_DAYS < 0) CACHE_MAX_AGE_DAYS <- 30
CACHE_FILE <- file.path(DIR$qa, "cnrfc_basin_product_availability_check_cache.csv")
CACHE_PARSER_VERSION <- "basin_availability_20260624_patch021"

pt_log("CNRFC basin product-availability options:")
pt_log("  max_basin_ids      = ", MAX_IDS)
pt_log("  sample_strategy    = ", SAMPLE_STRATEGY)
pt_log("  sample_seed        = ", SAMPLE_SEED)
pt_log("  explicit basin IDs = ", paste(EXPLICIT_IDS, collapse = ", "))
pt_log("  product_mode       = ", PRODUCT_MODE)
pt_log("  product IDs        = ", if (length(PRODUCT_IDS) == 0) "<none>" else paste(PRODUCT_IDS, collapse = ", "))
pt_log("  check_weather_gov  = ", CHECK_WRH)
pt_log("  check_water_update = ", CHECK_WRU)
pt_log("  use_wru_xml_matrix = ", USE_WRU_XML_PRODUCTS)
pt_log("  request_delay_sec  = ", REQUEST_DELAY_SEC)
pt_log("  resume_from_cache  = ", RESUME_FROM_CACHE)
pt_log("  force_refresh      = ", FORCE_REFRESH)
pt_log("  force_refresh_ids  = ", if (length(FORCE_REFRESH_IDS) == 0) "<none>" else paste(FORCE_REFRESH_IDS, collapse = ", "))
pt_log("  cache_max_age_days = ", CACHE_MAX_AGE_DAYS)
pt_log("  cache_file         = ", CACHE_FILE)

# ==== 3. Load latest base audit outputs =====================================

feature_master_path <- pt_latest_file("^cnrfc_feature_master_.*\\.csv$")
id_collision_path <- pt_latest_file("^cnrfc_id_collision_check_.*\\.csv$")

if (is.na(feature_master_path) || !file.exists(feature_master_path)) {
  stop(
    "No cnrfc_feature_master_*.csv found in ", DIR$qa, ".\n",
    "Run qa_cnrfc_product_intelligence_audit() first."
  )
}

pt_log("Using feature master: ", feature_master_path)
if (!is.na(id_collision_path)) pt_log("Using collision file: ", id_collision_path)

feature_master <- readr::read_csv(feature_master_path, show_col_types = FALSE)
id_collision <- if (!is.na(id_collision_path) && file.exists(id_collision_path)) {
  readr::read_csv(id_collision_path, show_col_types = FALSE)
} else {
  tibble()
}

pt_log("Feature master rows loaded: ", nrow(feature_master))


# Optional: reuse the latest Water Resources Update XML product matrix.
# This is the product-rich output from qa_cnrfc_water_resources_update_kml_audit().
# It is preferred over simple page-text ID matching because the WRU page is a
# map/data-table application and most product evidence lives in custom XML files.
wru_xml_matrix_path <- if (USE_WRU_XML_PRODUCTS) {
  pt_latest_file("^cnrfc_wru_xml_product_matrix_.*\\.csv$")
} else {
  NA_character_
}

wru_xml_matrix <- tibble()
if (USE_WRU_XML_PRODUCTS && !is.na(wru_xml_matrix_path) && file.exists(wru_xml_matrix_path)) {
  pt_log("Using latest WRU XML product matrix: ", wru_xml_matrix_path)
  wru_xml_matrix <- readr::read_csv(wru_xml_matrix_path, show_col_types = FALSE)
  if (!"cnrfc_id" %in% names(wru_xml_matrix)) {
    pt_log("  WRU XML matrix did not contain cnrfc_id; ignoring it.")
    wru_xml_matrix <- tibble()
  } else {
    wru_xml_matrix <- wru_xml_matrix |>
      mutate(cnrfc_id = pt_clean_id(.data$cnrfc_id)) |>
      filter(!is.na(.data$cnrfc_id)) |>
      distinct(.data$cnrfc_id, .keep_all = TRUE)

    needed_wru_cols <- c(
      "in_wru_xml_products",
      "wru_xml_record_count",
      "wru_xml_files",
      "wru_xml_product_groups",
      "wru_xml_record_tags",
      "wru_xml_labels"
    )
    for (.nm in setdiff(needed_wru_cols, names(wru_xml_matrix))) {
      wru_xml_matrix[[.nm]] <- NA
    }

    pt_log("  WRU XML product IDs loaded: ", nrow(wru_xml_matrix))
  }
} else if (USE_WRU_XML_PRODUCTS) {
  pt_log("No cnrfc_wru_xml_product_matrix_*.csv found yet; WR Update audit will fall back to shared-page text matching.")
} else {
  pt_log("WRU XML product matrix reuse disabled by option.")
}

# Make this audit tolerant of older/smaller feature-master exports.
needed_feature_cols <- c(
  "cnrfc_id", "feature_type", "has_valid_coordinate", "display_name",
  "river_name", "location_name", "forecast_group", "river_group",
  "lat", "lon", "elev_ft", "inventory_sources"
)
for (.nm in setdiff(needed_feature_cols, names(feature_master))) {
  feature_master[[.nm]] <- NA
}

# ==== 4. Build basin/FNF sample pool ========================================

pt_log("Building basin/FNF basin sample pool from feature master.")

basin_pool <- feature_master |>
  mutate(
    cnrfc_id = pt_clean_id(.data$cnrfc_id),
    feature_type = tolower(pt_chr(.data$feature_type)),
    has_valid_coordinate = as.logical(.data$has_valid_coordinate)
  ) |>
  filter(
    !is.na(.data$cnrfc_id),
    .data$feature_type %in% c("cnrfc_basin", "cnrfc_fnf_sierra_delta_basin")
  ) |>
  group_by(.data$cnrfc_id) |>
  summarise(
    feature_types = paste(sort(unique(.data$feature_type)), collapse = ";"),
    display_name = pt_first_nonblank(.data$display_name),
    river_name = pt_first_nonblank(.data$river_name),
    location_name = pt_first_nonblank(.data$location_name),
    forecast_group = pt_first_nonblank(.data$forecast_group),
    river_group = pt_first_nonblank(.data$river_group),
    lat = pt_first_num(.data$lat),
    lon = pt_first_num(.data$lon),
    elev_ft = pt_first_num(.data$elev_ft),
    has_valid_coordinate = any(.data$has_valid_coordinate, na.rm = TRUE),
    inventory_sources = paste(sort(unique(na.omit(.data$inventory_sources))), collapse = ";"),
    .groups = "drop"
  )

# Add WRU XML context before sampling so broader samples can intentionally mix
# product-rich and product-poor basins.
wru_sample_context <- tibble()
if (nrow(wru_xml_matrix) > 0) {
  wru_sample_context <- wru_xml_matrix |>
    transmute(
      cnrfc_id = .data$cnrfc_id,
      sample_wru_xml_products = as.logical(.data$in_wru_xml_products),
      sample_wru_xml_record_count = suppressWarnings(as.integer(.data$wru_xml_record_count)),
      sample_wru_xml_product_groups = pt_chr(.data$wru_xml_product_groups)
    ) |>
    mutate(
      sample_wru_xml_products = ifelse(is.na(.data$sample_wru_xml_products), FALSE, .data$sample_wru_xml_products),
      sample_wru_xml_record_count = ifelse(is.na(.data$sample_wru_xml_record_count), 0L, .data$sample_wru_xml_record_count)
    ) |>
    distinct(.data$cnrfc_id, .keep_all = TRUE)
}

if (nrow(wru_sample_context) > 0) {
  basin_pool <- basin_pool |>
    left_join(wru_sample_context, by = "cnrfc_id")
} else {
  basin_pool$sample_wru_xml_products <- FALSE
  basin_pool$sample_wru_xml_record_count <- 0L
  basin_pool$sample_wru_xml_product_groups <- NA_character_
}

basin_pool <- basin_pool |>
  mutate(
    sample_wru_xml_products = ifelse(is.na(.data$sample_wru_xml_products), FALSE, .data$sample_wru_xml_products),
    sample_wru_xml_record_count = ifelse(is.na(.data$sample_wru_xml_record_count), 0L, .data$sample_wru_xml_record_count),
    sample_wru_xml_product_groups = pt_chr(.data$sample_wru_xml_product_groups),
    explicit_order = match(.data$cnrfc_id, EXPLICIT_IDS),
    sample_priority = case_when(
      !is.na(.data$explicit_order) ~ .data$explicit_order,
      grepl("fnf_sierra_delta", .data$feature_types, ignore.case = TRUE) ~ 1000L,
      .data$sample_wru_xml_products ~ 1500L,
      TRUE ~ 2000L
    )
  )

missing_explicit <- setdiff(EXPLICIT_IDS, basin_pool$cnrfc_id)
if (length(missing_explicit) > 0) {
  pt_log("Explicit IDs not found as basin/FNF rows but will still be probed: ", paste(missing_explicit, collapse = ", "))
  basin_pool <- bind_rows(
    basin_pool,
    tibble(
      cnrfc_id = missing_explicit,
      feature_types = "explicit_id_not_found_as_basin",
      display_name = NA_character_,
      river_name = NA_character_,
      location_name = NA_character_,
      forecast_group = NA_character_,
      river_group = NA_character_,
      lat = NA_real_,
      lon = NA_real_,
      elev_ft = NA_real_,
      has_valid_coordinate = FALSE,
      inventory_sources = NA_character_,
      sample_wru_xml_products = FALSE,
      sample_wru_xml_record_count = 0L,
      sample_wru_xml_product_groups = NA_character_,
      explicit_order = match(missing_explicit, EXPLICIT_IDS),
      sample_priority = match(missing_explicit, EXPLICIT_IDS)
    )
  )
}

basin_pool <- basin_pool |>
  distinct(.data$cnrfc_id, .keep_all = TRUE)

# Sampling modes:
#   priority     = current stable ordering: explicit IDs, FNF basins, WRU-positive basins, others
#   wru_mixed    = explicit IDs first, then deterministic interleave of FNF / WRU-positive / WRU-negative basins
#   alphabetical = stable alphabetical check
#   random       = explicit IDs first, then seeded random sample
#   all          = all basin/FNF IDs, regardless of max_basin_ids
if (SAMPLE_STRATEGY == "wru_mixed") {
  basin_ordered <- basin_pool |>
    mutate(
      sample_bucket = case_when(
        !is.na(.data$explicit_order) ~ 0L,
        grepl("fnf_sierra_delta", .data$feature_types, ignore.case = TRUE) ~ 1L,
        .data$sample_wru_xml_products ~ 2L,
        TRUE ~ 3L
      )
    ) |>
    group_by(.data$sample_bucket) |>
    arrange(.data$cnrfc_id, .by_group = TRUE) |>
    mutate(bucket_rank = row_number()) |>
    ungroup() |>
    mutate(
      sample_rank = ifelse(
        .data$sample_bucket == 0L,
        .data$explicit_order,
        100000L + (.data$bucket_rank * 10L) + .data$sample_bucket
      )
    ) |>
    arrange(.data$sample_rank, .data$sample_priority, .data$cnrfc_id)
} else if (SAMPLE_STRATEGY == "alphabetical") {
  basin_ordered <- basin_pool |>
    arrange(.data$cnrfc_id)
} else if (SAMPLE_STRATEGY == "random") {
  set.seed(SAMPLE_SEED)
  explicit_part <- basin_pool |>
    filter(!is.na(.data$explicit_order)) |>
    arrange(.data$explicit_order, .data$cnrfc_id)
  random_part <- basin_pool |>
    filter(is.na(.data$explicit_order)) |>
    slice_sample(prop = 1)
  basin_ordered <- bind_rows(explicit_part, random_part)
} else {
  basin_ordered <- basin_pool |>
    arrange(.data$sample_priority, .data$cnrfc_id)
}

if (SAMPLE_STRATEGY == "all" || !is.finite(MAX_IDS)) {
  basin_sample <- basin_ordered
} else {
  basin_sample <- basin_ordered |>
    slice_head(n = MAX_IDS)
}

pt_log("Basin/FNF IDs available: ", nrow(basin_pool))
pt_log("Basin/FNF IDs selected for live product audit: ", nrow(basin_sample))

sample_context <- basin_sample |>
  summarise(
    selected = n(),
    fnf_ids = sum(grepl("fnf_sierra_delta", .data$feature_types, ignore.case = TRUE), na.rm = TRUE),
    wru_xml_yes = sum(.data$sample_wru_xml_products, na.rm = TRUE),
    wru_xml_no = sum(!.data$sample_wru_xml_products, na.rm = TRUE),
    explicit_ids = sum(!is.na(.data$explicit_order), na.rm = TRUE)
  )
pt_log(
  "Sample composition: explicit=", sample_context$explicit_ids[[1]],
  "; FNF=", sample_context$fnf_ids[[1]],
  "; WRU XML yes=", sample_context$wru_xml_yes[[1]],
  "; WRU XML no=", sample_context$wru_xml_no[[1]]
)

if (nrow(basin_sample) == 0) {
  stop("No basin/FNF IDs selected for product availability audit.")
}

# ==== 5. URL helpers and page classifiers ===================================

PRODUCT_LABELS <- tibble(
  product_id = 1:12,
  product_key = c(
    "short_range_peaks",
    "threshold_plot",
    "ten_day_probability",
    "ten_day_accum_volume",
    "four_by_five_day_probability",
    "monthly_probability",
    "seasonal_trend_plot",
    "water_year_trend_plot",
    "water_year_trend_plot_or_related",
    "water_year_accumulated_volume",
    "multi_water_year_accumulated_volume",
    "historical_flows"
  ),
  product_label = c(
    "Short-range peaks",
    "Threshold plot",
    "10-day probability",
    "10-day accumulated volume",
    "4x5-day probability",
    "Monthly probability",
    "Seasonal trend plot",
    "WY trend plot",
    "WY trend plot / related",
    "WY accumulated volume",
    "Multi-WY accumulated volume",
    "Historical flows"
  )
)


pt_expected_product_keys <- function() {
  keys <- c(
    "ensemble_landing_page",
    PRODUCT_LABELS$product_key[PRODUCT_LABELS$product_id %in% PRODUCT_IDS],
    "basin_mean_temperature_forecast",
    "six_day_qpf_snow_level_summary_row"
  )
  if (CHECK_WRU) keys <- c(keys, if (nrow(wru_xml_matrix) > 0) "water_resources_update_xml_products" else "water_resources_update_page_match")
  if (CHECK_WRH) keys <- c(keys, "wrh_time_series_viewer", "nws_api_recent_observation_smoke_check")
  unique(keys)
}

pt_cache_key_vec <- function(cnrfc_id, product_family, product_key, product_id, product_url) {
  paste(
    pt_clean_id(cnrfc_id),
    pt_chr(product_family),
    pt_chr(product_key),
    ifelse(is.na(product_id), "", as.character(product_id)),
    pt_chr(product_url),
    sep = "||"
  )
}

pt_normalize_cache_table <- function(x, source_label = "unknown") {
  if (is.null(x) || nrow(x) == 0) return(tibble())

  needed_cols <- c(
    "cnrfc_id", "product_family", "product_key", "product_label", "product_id", "product_url",
    "status_code", "bytes", "availability_class", "issuance_hint", "page_excerpt", "error",
    "display_name", "river_name", "location_name", "forecast_group", "river_group",
    "feature_types", "inventory_sources", "checked_pacific", "run_timestamp",
    "wru_xml_record_count", "wru_xml_files", "wru_xml_product_groups", "wru_xml_record_tags", "wru_xml_labels",
    "from_cache", "cache_source", "cache_parser_version", "cache_key", "cache_written_pacific"
  )
  for (.nm in setdiff(needed_cols, names(x))) x[[.nm]] <- NA

  x |>
    mutate(
      cnrfc_id = pt_clean_id(.data$cnrfc_id),
      product_family = pt_chr(.data$product_family),
      product_key = pt_chr(.data$product_key),
      product_url = pt_chr(.data$product_url),
      product_id = suppressWarnings(as.integer(.data$product_id)),
      from_cache = as.logical(.data$from_cache),
      from_cache = ifelse(is.na(.data$from_cache), FALSE, .data$from_cache),
      cache_source = ifelse(is.na(.data$cache_source) | .data$cache_source == "", source_label, .data$cache_source),
      cache_parser_version = ifelse(is.na(.data$cache_parser_version) | .data$cache_parser_version == "", "legacy_or_seed", .data$cache_parser_version),
      cache_key = pt_cache_key_vec(.data$cnrfc_id, .data$product_family, .data$product_key, .data$product_id, .data$product_url)
    ) |>
    filter(!is.na(.data$cnrfc_id), .data$product_key != "")
}

pt_seed_cache_from_latest_long <- function() {
  latest_long <- pt_latest_file("^cnrfc_basin_product_availability_long_.*\\.csv$")
  if (is.na(latest_long) || !file.exists(latest_long)) return(tibble())
  pt_log("Seeding availability cache from latest long table: ", latest_long)
  x <- tryCatch(readr::read_csv(latest_long, show_col_types = FALSE), error = function(e) tibble())
  pt_normalize_cache_table(x, source_label = paste0("seed_latest_long:", basename(latest_long)))
}

pt_parse_checked_time <- function(x) {
  x <- pt_chr(x)
  x <- gsub("\\s+(PDT|PST|UTC)$", "", x)
  suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles"))
}

pt_load_availability_cache <- function() {
  if (!RESUME_FROM_CACHE) return(tibble())

  cache_rows <- tibble()
  if (file.exists(CACHE_FILE)) {
    pt_log("Reading availability cache: ", CACHE_FILE)
    cache_rows <- tryCatch(readr::read_csv(CACHE_FILE, show_col_types = FALSE), error = function(e) {
      pt_log("  Cache read failed; will try latest long-table seed. Error: ", conditionMessage(e))
      tibble()
    })
    cache_rows <- pt_normalize_cache_table(cache_rows, source_label = paste0("cache_file:", basename(CACHE_FILE)))
  }

  if (nrow(cache_rows) == 0) {
    cache_rows <- pt_seed_cache_from_latest_long()
  }

  if (nrow(cache_rows) == 0) {
    pt_log("No usable availability cache rows found.")
    return(tibble())
  }

  checked_time <- pt_parse_checked_time(cache_rows$checked_pacific)
  keep_age <- is.na(checked_time) | as.numeric(difftime(Sys.time(), checked_time, units = "days")) <= CACHE_MAX_AGE_DAYS
  cache_rows <- cache_rows[keep_age, , drop = FALSE]

  cache_rows <- cache_rows |>
    group_by(.data$cache_key) |>
    slice_tail(n = 1) |>
    ungroup()

  pt_log("Usable cache rows loaded: ", nrow(cache_rows), "; IDs: ", length(unique(cache_rows$cnrfc_id)))
  cache_rows
}

pt_cached_rows_for_id <- function(id, expected_keys, cache_rows) {
  if (!RESUME_FROM_CACHE || FORCE_REFRESH || nrow(cache_rows) == 0) return(tibble())
  id <- pt_clean_id(id)[[1]]
  if (is.na(id) || id %in% FORCE_REFRESH_IDS) return(tibble())

  x <- cache_rows |>
    filter(.data$cnrfc_id == id, .data$product_key %in% expected_keys)

  present_keys <- unique(x$product_key)
  if (!all(expected_keys %in% present_keys)) return(tibble())

  x |>
    group_by(.data$product_key) |>
    slice_tail(n = 1) |>
    ungroup() |>
    mutate(
      cached_source_run_timestamp = .data$run_timestamp,
      run_timestamp = RUN_TS,
      from_cache = TRUE,
      cache_source = ifelse(is.na(.data$cache_source) | .data$cache_source == "", "availability_cache", .data$cache_source)
    )
}

pt_write_availability_cache <- function(existing_cache, new_rows) {
  if (!RESUME_FROM_CACHE) return(invisible(NULL))

  dir.create(dirname(CACHE_FILE), showWarnings = FALSE, recursive = TRUE)
  now_txt <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  out <- bind_rows(existing_cache, new_rows) |>
    pt_normalize_cache_table(source_label = "current_run") |>
    mutate(
      cache_parser_version = ifelse(.data$from_cache, .data$cache_parser_version, CACHE_PARSER_VERSION),
      cache_written_pacific = now_txt,
      cache_key = pt_cache_key_vec(.data$cnrfc_id, .data$product_family, .data$product_key, .data$product_id, .data$product_url)
    ) |>
    group_by(.data$cache_key) |>
    slice_tail(n = 1) |>
    ungroup()

  readr::write_csv(out, CACHE_FILE)
  pt_log("Availability cache written: ", CACHE_FILE, " | rows: ", nrow(out), " | IDs: ", length(unique(out$cnrfc_id)))
  invisible(out)
}

pt_ensemble_url <- function(id, prod_id = NA_integer_) {
  if (is.na(prod_id)) {
    paste0("https://www.cnrfc.noaa.gov/ensembleProduct.php?id=", pt_url_id(id))
  } else {
    paste0("https://www.cnrfc.noaa.gov/ensembleProduct.php?id=", pt_url_id(id), "&prodID=", prod_id, "&years=1")
  }
}

pt_temp_url <- function(id) paste0("https://www.cnrfc.noaa.gov/temperaturePlots_hc.php?id=", pt_url_id(id))
pt_wrh_url <- function(id) paste0("https://www.weather.gov/wrh/timeseries?site=", pt_url_id(id))
pt_nws_api_url <- function(id) paste0("https://api.weather.gov/stations/", pt_url_id(id), "/observations?limit=1")
QPF_SNOW_LEVEL_URL <- "https://www.cnrfc.noaa.gov/awipsProducts/RNOHD6RSA.php"
WATER_RESOURCES_UPDATE_URL <- "https://www.cnrfc.noaa.gov/water_resources_update.php"

pt_decode_raw <- function(content) {
  if (length(content) >= 2 &&
      as.integer(content[[1]]) == 31L &&
      as.integer(content[[2]]) == 139L) {
    content <- tryCatch(memDecompress(content, type = "gzip"), error = function(e) content)
  }

  tryCatch(
    rawToChar(content),
    error = function(e) paste(rawToChar(content, multiple = TRUE), collapse = "")
  )
}

pt_fetch_text <- function(url, timeout_sec = 10) {
  if (requireNamespace("curl", quietly = TRUE)) {
    h <- curl::new_handle(
      timeout = timeout_sec,
      followlocation = TRUE,
      useragent = "BRIM CNRFC basin product availability QA audit (internal BRIM development)",
      httpheader = c("Accept-Encoding" = "identity")
    )
    res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
    if (inherits(res, "error")) {
      return(list(ok = FALSE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = conditionMessage(res)))
    }
    txt <- pt_decode_raw(res$content)
    return(list(ok = TRUE, status_code = res$status_code, bytes = length(res$content), text = txt, error = NA_character_))
  }

  old_timeout <- getOption("timeout")
  options(timeout = max(timeout_sec, old_timeout %||% 60))
  on.exit(options(timeout = old_timeout), add = TRUE)
  txt <- tryCatch({
    con <- url(url, open = "rb")
    on.exit(close(con), add = TRUE)
    paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, error = function(e) e)

  if (inherits(txt, "error")) {
    return(list(ok = FALSE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = conditionMessage(txt)))
  }
  list(ok = TRUE, status_code = NA_integer_, bytes = nchar(txt, type = "bytes"), text = txt, error = NA_character_)
}

pt_page_has_negative <- function(txt) {
  grepl(
    "Selected Ensemble Product NOT Available|NOT Available for this Location|not available for this location|Selected .* NOT Available|No data available|Invalid|not a valid",
    txt,
    ignore.case = TRUE
  )
}

pt_classify_ensemble <- function(txt, status_code) {
  if (!is.na(status_code) && status_code >= 400) return("http_error")
  if (pt_page_has_negative(txt)) return("not_available_for_location")
  if (!grepl("CALIFORNIA-NEVADA RIVER FORECAST CENTER|CNRFC|Ensemble", txt, ignore.case = TRUE)) return("unexpected_page")
  if (grepl("Issuance Time|Graphic Created|Median Forecast|Water Year|Short-Range|Long-Range", txt, ignore.case = TRUE)) return("available")
  "page_loads_uncertain"
}

pt_classify_temp <- function(txt, status_code) {
  if (!is.na(status_code) && status_code >= 400) return("http_error")
  if (pt_page_has_negative(txt)) return("not_available_for_location")
  if (!grepl("CALIFORNIA-NEVADA RIVER FORECAST CENTER|CNRFC", txt, ignore.case = TRUE)) return("unexpected_page")
  if (grepl("Temperature|Graphic Created|Basin Temperature|Local Upper Basin|Upper Basin|Lower Basin", txt, ignore.case = TRUE)) return("available")
  "page_loads_uncertain"
}

pt_classify_water_resources_update <- function(txt, status_code) {
  if (!is.na(status_code) && status_code >= 400) return("water_resources_update_http_error")
  if (!nzchar(txt)) return("water_resources_update_empty_response")

  # The WR Update page appears to be a large, map/app-like page.  Be permissive
  # here: this classifier says the shared page loaded, not that a given basin ID
  # was found.  ID/product proof is handled separately.
  if (grepl(
    paste(
      "Water Resources Update",
      "water_resources_update",
      "Raw ESP Water Supply Forecast",
      "current water year forecasts",
      "Forecast Flow Volume",
      "Accumulated Forecast",
      "Water Year",
      "ESP",
      sep = "|"
    ),
    txt,
    ignore.case = TRUE
  )) {
    return("water_resources_update_page_loaded")
  }

  if (grepl("CNRFC|CALIFORNIA-NEVADA RIVER FORECAST CENTER|cnrfc\\.noaa\\.gov", txt, ignore.case = TRUE)) {
    return("water_resources_update_page_loaded_unparsed")
  }

  "water_resources_update_unrecognized_response"
}

pt_extract_urls_for_id <- function(txt, id, base = "https://www.cnrfc.noaa.gov") {
  # Lightweight, best-effort helper for QA only.  It looks for href values on
  # the shared water resources update page that include the basin/location ID.
  # The main availability decision still comes from whether the ID appears on
  # the page; direct links are retained as supporting audit evidence.
  id <- pt_clean_id(id)
  if (is.na(id) || !nzchar(txt)) return(NA_character_)
  hrefs <- unlist(regmatches(txt, gregexpr('href=["\'][^"\']+["\']', txt, ignore.case = TRUE, perl = TRUE)))
  if (length(hrefs) == 0) return(NA_character_)
  hrefs <- gsub('^href=["\']|["\']$', '', hrefs, ignore.case = TRUE)
  hrefs <- hrefs[grepl(id, hrefs, fixed = TRUE)]
  if (length(hrefs) == 0) return(NA_character_)
  hrefs <- ifelse(grepl('^https?://', hrefs), hrefs, paste0(base, ifelse(startsWith(hrefs, '/'), '', '/'), hrefs))
  paste(unique(hrefs), collapse = ';')
}

pt_classify_wrh_page <- function(txt, status_code) {
  if (!is.na(status_code) && status_code >= 400) return("wrh_viewer_http_error")
  if (!grepl("Time Series Viewer", txt, fixed = TRUE)) return("wrh_viewer_unexpected_page")
  if (grepl("not a valid station identifier|invalid station identifier|not a valid station", txt, ignore.case = TRUE)) {
    return("wrh_viewer_invalid_station_identifier")
  }
  if (grepl("Data availability varies by station", txt, fixed = TRUE)) return("wrh_viewer_page_loads_generic_app")
  "wrh_viewer_page_loads"
}

pt_classify_api_obs <- function(txt, status_code) {
  if (!is.na(status_code) && status_code == 404) return("nws_api_station_not_found")
  if (!is.na(status_code) && status_code >= 400) return("nws_api_http_error")
  if (grepl('"features"\\s*:\\s*\\[\\s*\\]', txt)) return("nws_api_station_found_no_recent_observation")
  if (grepl('"features"\\s*:\\s*\\[\\s*\\{', txt)) return("nws_api_recent_observation_available")
  if (grepl('"temperature"', txt) || grepl('"precipitationLastHour"', txt) || grepl('"relativeHumidity"', txt)) return("nws_api_observation_json_like")
  "nws_api_unclassified_response"
}

pt_extract_api_elements <- function(txt) {
  elems <- c(
    temperature = grepl('"temperature"', txt),
    dewpoint = grepl('"dewpoint"', txt),
    relative_humidity = grepl('"relativeHumidity"', txt),
    wind_speed = grepl('"windSpeed"', txt),
    wind_gust = grepl('"windGust"', txt),
    precip_last_hour = grepl('"precipitationLastHour"', txt),
    precip_last_3hr = grepl('"precipitationLast3Hours"', txt),
    precip_last_6hr = grepl('"precipitationLast6Hours"', txt)
  )
  paste(names(elems)[elems], collapse = ";")
}

pt_extract_issuance <- function(txt) {
  txt2 <- pt_compact_text(txt, max_n = 2000)
  m <- regexpr("Issuance Time[: ]+[^A-Z]*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)?[^<]{0,80}", txt2, ignore.case = TRUE)
  if (m[[1]] > 0) substr(txt2, m[[1]], m[[1]] + attr(m, "match.length") - 1) else NA_character_
}


EXPECTED_PRODUCT_KEYS <- pt_expected_product_keys()
availability_cache <- pt_load_availability_cache()
pt_log("Expected product keys for this run: ", paste(EXPECTED_PRODUCT_KEYS, collapse = ", "))

cache_probe <- if (nrow(availability_cache) > 0) {
  purrr::map_lgl(basin_sample$cnrfc_id, function(.id) nrow(pt_cached_rows_for_id(.id, EXPECTED_PRODUCT_KEYS, availability_cache)) > 0)
} else {
  rep(FALSE, nrow(basin_sample))
}
pt_log("Basin IDs already complete in cache for this product set: ", sum(cache_probe), " / ", nrow(basin_sample))
pt_log("Basin IDs needing live checks for this product set: ", nrow(basin_sample) - sum(cache_probe), " / ", nrow(basin_sample))

# ==== 6. Fetch shared CNRFC pages once ======================================

pt_log("Fetching CNRFC 6-day QPF / snow-level summary once: ", QPF_SNOW_LEVEL_URL)
qpf_fetch <- pt_fetch_text(QPF_SNOW_LEVEL_URL, timeout_sec = 12)
if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
qpf_text <- qpf_fetch$text
qpf_fetch_class <- if (!qpf_fetch$ok) {
  "fetch_error"
} else if (!is.na(qpf_fetch$status_code) && qpf_fetch$status_code >= 400) {
  "http_error"
} else if (grepl("6-Day|QPF|Snow Level|Freezing", qpf_text, ignore.case = TRUE)) {
  "summary_page_loaded"
} else {
  "summary_page_uncertain"
}
pt_log("QPF/snow-level summary fetch class: ", qpf_fetch_class, "; bytes: ", qpf_fetch$bytes %||% NA_integer_)

if (CHECK_WRU && nrow(wru_xml_matrix) > 0) {
  # Preferred path: use the parsed WRU XML product matrix from the dedicated WRU audit.
  # Do not refetch the large map page just to do weaker text matching.
  wru_fetch <- list(ok = TRUE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = NA_character_)
  wru_text <- ""
  wru_fetch_class <- "water_resources_update_xml_matrix_loaded"
  pt_log("Water Resources Update source: latest XML product matrix; page-text fetch skipped.")
} else if (CHECK_WRU) {
  pt_log("Fetching CNRFC Daily Water Resources Update once: ", WATER_RESOURCES_UPDATE_URL)
  wru_fetch <- pt_fetch_text(WATER_RESOURCES_UPDATE_URL, timeout_sec = 15)
  if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
  wru_text <- wru_fetch$text
  wru_fetch_class <- if (!wru_fetch$ok) {
    "water_resources_update_fetch_error"
  } else {
    pt_classify_water_resources_update(wru_text, wru_fetch$status_code)
  }
  pt_log("Water Resources Update fetch class: ", wru_fetch_class, "; bytes: ", wru_fetch$bytes %||% NA_integer_, "; decoded chars: ", nchar(wru_text, type = "chars"))
} else {
  wru_fetch <- list(ok = FALSE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = NA_character_)
  wru_text <- ""
  wru_fetch_class <- "not_checked"
  pt_log("Skipping CNRFC Daily Water Resources Update fetch by option.")
}

# ==== 7. Live product checks =================================================

pt_log("Starting live product checks. This can take a few minutes depending on sample size.")

cached_basin_ids_used <- character(0)
live_basin_ids_checked <- character(0)

availability_long <- purrr::map_dfr(seq_len(nrow(basin_sample)), function(i) {
  row <- basin_sample[i, ]
  id <- row$cnrfc_id[[1]]

  cached_rows <- pt_cached_rows_for_id(id, EXPECTED_PRODUCT_KEYS, availability_cache)
  if (nrow(cached_rows) > 0) {
    cached_basin_ids_used <<- c(cached_basin_ids_used, id)
    pt_log("  basin ", i, "/", nrow(basin_sample), ": ", id, " | using cached rows (", nrow(cached_rows), ")")
    return(cached_rows)
  }

  live_basin_ids_checked <<- c(live_basin_ids_checked, id)
  pt_log("  basin ", i, "/", nrow(basin_sample), ": ", id, " | live check | ", row$display_name[[1]] %||% "")

  rows <- list()

  # Ensemble landing page.
  url <- pt_ensemble_url(id)
  x <- pt_fetch_text(url, timeout_sec = 10)
  if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
  rows[[length(rows) + 1]] <- tibble(
    cnrfc_id = id,
    product_family = "ensemble",
    product_key = "ensemble_landing_page",
    product_label = "Ensemble products landing page",
    product_id = NA_integer_,
    product_url = url,
    status_code = x$status_code,
    bytes = x$bytes,
    availability_class = pt_classify_ensemble(x$text, x$status_code),
    issuance_hint = pt_extract_issuance(x$text),
    page_excerpt = pt_compact_text(x$text),
    error = x$error
  )

  # Specific ensemble product IDs.
  for (pid in PRODUCT_IDS) {
    label_row <- PRODUCT_LABELS |> filter(.data$product_id == pid)
    product_key <- if (nrow(label_row) > 0) label_row$product_key[[1]] else paste0("product_", pid)
    product_label <- if (nrow(label_row) > 0) label_row$product_label[[1]] else paste0("CNRFC ensemble product ", pid)
    url <- pt_ensemble_url(id, pid)
    x <- pt_fetch_text(url, timeout_sec = 10)
    if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
    rows[[length(rows) + 1]] <- tibble(
      cnrfc_id = id,
      product_family = "ensemble_product",
      product_key = product_key,
      product_label = product_label,
      product_id = pid,
      product_url = url,
      status_code = x$status_code,
      bytes = x$bytes,
      availability_class = pt_classify_ensemble(x$text, x$status_code),
      issuance_hint = pt_extract_issuance(x$text),
      page_excerpt = pt_compact_text(x$text),
      error = x$error
    )
  }

  # Basin mean temperature page.
  url <- pt_temp_url(id)
  x <- pt_fetch_text(url, timeout_sec = 10)
  if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
  rows[[length(rows) + 1]] <- tibble(
    cnrfc_id = id,
    product_family = "basin_temperature",
    product_key = "basin_mean_temperature_forecast",
    product_label = "Basin mean temperature forecast",
    product_id = NA_integer_,
    product_url = url,
    status_code = x$status_code,
    bytes = x$bytes,
    availability_class = pt_classify_temp(x$text, x$status_code),
    issuance_hint = pt_extract_issuance(x$text),
    page_excerpt = pt_compact_text(x$text),
    error = x$error
  )

  # 6-day QPF/snow-level page reference.  This is one shared CNRFC page, but
  # a basin/station ID appearing in the page text is useful for future popup
  # ranking and possible symbology.
  id_regex <- paste0("\\b", stringr::str_replace_all(id, "([\\W])", "\\\\\\1"), "\\b")
  has_qpf_row <- qpf_fetch$ok && grepl(id_regex, qpf_text, ignore.case = FALSE, perl = TRUE)
  rows[[length(rows) + 1]] <- tibble(
    cnrfc_id = id,
    product_family = "qpf_snow_level_summary",
    product_key = "six_day_qpf_snow_level_summary_row",
    product_label = "6-day QPF / snow-level summary row",
    product_id = NA_integer_,
    product_url = QPF_SNOW_LEVEL_URL,
    status_code = qpf_fetch$status_code,
    bytes = qpf_fetch$bytes,
    availability_class = case_when(
      !qpf_fetch$ok ~ "summary_fetch_error",
      qpf_fetch_class %in% c("http_error", "summary_page_uncertain") ~ qpf_fetch_class,
      has_qpf_row ~ "id_found_in_summary_page",
      TRUE ~ "summary_page_loaded_id_not_found"
    ),
    issuance_hint = NA_character_,
    page_excerpt = if (has_qpf_row) paste0(id, " appears in the 6-day QPF / snow-level summary text.") else NA_character_,
    error = qpf_fetch$error
  )

  # Daily Water Resources Update. Prefer the parsed XML product matrix from
  # qa_cnrfc_water_resources_update_kml_audit(); fall back to page-text matching
  # only when that XML matrix does not exist yet.
  if (CHECK_WRU) {
    wru_hit <- if (nrow(wru_xml_matrix) > 0) {
      wru_xml_matrix |> filter(.data$cnrfc_id == id)
    } else {
      tibble()
    }

    if (nrow(wru_hit) > 0) {
      has_wru_xml <- isTRUE(as.logical(wru_hit$in_wru_xml_products[[1]]))
      wru_record_count <- suppressWarnings(as.integer(wru_hit$wru_xml_record_count[[1]]))
      rows[[length(rows) + 1]] <- tibble(
        cnrfc_id = id,
        product_family = "water_resources_update",
        product_key = "water_resources_update_xml_products",
        product_label = "CNRFC Water Resources Update XML products",
        product_id = NA_integer_,
        product_url = WATER_RESOURCES_UPDATE_URL,
        status_code = wru_fetch$status_code,
        bytes = wru_fetch$bytes,
        availability_class = if (has_wru_xml && !is.na(wru_record_count) && wru_record_count > 0) {
          "water_resources_update_xml_products_found"
        } else {
          "water_resources_update_xml_products_not_found"
        },
        issuance_hint = NA_character_,
        page_excerpt = if (has_wru_xml && !is.na(wru_record_count) && wru_record_count > 0) {
          paste0(
            id, " found in WRU XML product matrix: ",
            wru_record_count, " record(s); files = ", pt_chr(wru_hit$wru_xml_files[[1]]),
            "; groups = ", pt_chr(wru_hit$wru_xml_product_groups[[1]])
          )
        } else {
          paste0(id, " was present in the WRU XML matrix but had no product rows.")
        },
        error = NA_character_,
        wru_xml_record_count = wru_record_count,
        wru_xml_files = pt_chr(wru_hit$wru_xml_files[[1]]),
        wru_xml_product_groups = pt_chr(wru_hit$wru_xml_product_groups[[1]]),
        wru_xml_record_tags = pt_chr(wru_hit$wru_xml_record_tags[[1]]),
        wru_xml_labels = pt_chr(wru_hit$wru_xml_labels[[1]])
      )
    } else if (nrow(wru_xml_matrix) > 0) {
      rows[[length(rows) + 1]] <- tibble(
        cnrfc_id = id,
        product_family = "water_resources_update",
        product_key = "water_resources_update_xml_products",
        product_label = "CNRFC Water Resources Update XML products",
        product_id = NA_integer_,
        product_url = WATER_RESOURCES_UPDATE_URL,
        status_code = wru_fetch$status_code,
        bytes = wru_fetch$bytes,
        availability_class = "water_resources_update_xml_products_not_found",
        issuance_hint = NA_character_,
        page_excerpt = paste0(id, " not found in latest WRU XML product matrix."),
        error = NA_character_,
        wru_xml_record_count = 0L,
        wru_xml_files = NA_character_,
        wru_xml_product_groups = NA_character_,
        wru_xml_record_tags = NA_character_,
        wru_xml_labels = NA_character_
      )
    } else {
      id_regex <- paste0("\\b", stringr::str_replace_all(id, "([\\W])", "\\\\\\1"), "\\b")
      has_wru_id <- wru_fetch$ok && grepl(id_regex, wru_text, ignore.case = FALSE, perl = TRUE)
      direct_links <- pt_extract_urls_for_id(wru_text, id)
      rows[[length(rows) + 1]] <- tibble(
        cnrfc_id = id,
        product_family = "water_resources_update",
        product_key = "water_resources_update_page_match",
        product_label = "CNRFC Daily Water Resources Update",
        product_id = NA_integer_,
        product_url = if (!is.na(direct_links) && nzchar(direct_links)) direct_links else WATER_RESOURCES_UPDATE_URL,
        status_code = wru_fetch$status_code,
        bytes = wru_fetch$bytes,
        availability_class = case_when(
          !wru_fetch$ok ~ "water_resources_update_fetch_error",
          wru_fetch_class %in% c("water_resources_update_http_error", "water_resources_update_empty_response", "water_resources_update_unrecognized_response", "water_resources_update_page_uncertain") ~ wru_fetch_class,
          has_wru_id ~ "water_resources_update_page_loaded_id_found",
          TRUE ~ "water_resources_update_page_loaded_id_not_found"
        ),
        issuance_hint = NA_character_,
        page_excerpt = case_when(
          has_wru_id && !is.na(direct_links) && nzchar(direct_links) ~ paste0(id, " appears on the Daily Water Resources Update page; direct URL(s) containing ID retained."),
          has_wru_id ~ paste0(id, " appears on the Daily Water Resources Update page."),
          TRUE ~ NA_character_
        ),
        error = wru_fetch$error
      )
    }
  }

  # Weather.gov WRH / NWS API smoke checks, optional.
  if (CHECK_WRH) {
    url <- pt_wrh_url(id)
    x <- pt_fetch_text(url, timeout_sec = 8)
    if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
    rows[[length(rows) + 1]] <- tibble(
      cnrfc_id = id,
      product_family = "weather_gov_wrh",
      product_key = "wrh_time_series_viewer",
      product_label = "Weather.gov WRH Time Series Viewer",
      product_id = NA_integer_,
      product_url = url,
      status_code = x$status_code,
      bytes = x$bytes,
      availability_class = pt_classify_wrh_page(x$text, x$status_code),
      issuance_hint = NA_character_,
      page_excerpt = pt_compact_text(x$text),
      error = x$error
    )

    url <- pt_nws_api_url(id)
    x <- pt_fetch_text(url, timeout_sec = 8)
    if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
    rows[[length(rows) + 1]] <- tibble(
      cnrfc_id = id,
      product_family = "weather_gov_wrh",
      product_key = "nws_api_recent_observation_smoke_check",
      product_label = "NWS API recent observation smoke check",
      product_id = NA_integer_,
      product_url = url,
      status_code = x$status_code,
      bytes = x$bytes,
      availability_class = pt_classify_api_obs(x$text, x$status_code),
      issuance_hint = pt_extract_api_elements(x$text),
      page_excerpt = pt_compact_text(x$text),
      error = x$error
    )
  }

  bind_rows(rows) |>
    mutate(
      display_name = row$display_name[[1]],
      river_name = row$river_name[[1]],
      location_name = row$location_name[[1]],
      forecast_group = row$forecast_group[[1]],
      river_group = row$river_group[[1]],
      feature_types = row$feature_types[[1]],
      inventory_sources = row$inventory_sources[[1]],
      checked_pacific = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      run_timestamp = RUN_TS,
      from_cache = FALSE,
      cache_source = "current_live_check",
      cache_parser_version = CACHE_PARSER_VERSION,
      cache_written_pacific = NA_character_,
      cache_key = pt_cache_key_vec(.data$cnrfc_id, .data$product_family, .data$product_key, .data$product_id, .data$product_url)
    )
})

pt_log("Product rows complete. Rows: ", nrow(availability_long))
pt_log("  basin IDs served from cache: ", length(unique(cached_basin_ids_used)))
pt_log("  basin IDs checked live:       ", length(unique(live_basin_ids_checked)))
updated_cache <- pt_write_availability_cache(availability_cache, availability_long)

# ==== 8. Matrix and observed bins ===========================================

availability_flags <- availability_long |>
  mutate(
    available_flag = .data$availability_class %in% c(
      "available",
      "page_loads_uncertain",
      "id_found_in_summary_page",
      "water_resources_update_page_loaded_id_found",
      "water_resources_update_xml_products_found",
      "wrh_viewer_page_loads",
      "wrh_viewer_page_loads_generic_app",
      "nws_api_recent_observation_available",
      "nws_api_observation_json_like"
    ),
    unavailable_flag = .data$availability_class %in% c(
      "not_available_for_location",
      "summary_page_loaded_id_not_found",
      "water_resources_update_page_loaded_id_not_found",
      "water_resources_update_xml_products_not_found",
      "wrh_viewer_invalid_station_identifier",
      "nws_api_station_not_found",
      "nws_api_station_found_no_recent_observation"
    ),
    status_compact = case_when(
      .data$availability_class %in% c("available", "id_found_in_summary_page", "water_resources_update_page_loaded_id_found",
      "water_resources_update_xml_products_found", "nws_api_recent_observation_available", "nws_api_observation_json_like") ~ "yes",
      .data$availability_class %in% c("page_loads_uncertain", "wrh_viewer_page_loads", "wrh_viewer_page_loads_generic_app") ~ "maybe",
      .data$unavailable_flag ~ "no",
      TRUE ~ "unknown"
    )
  )

matrix_status <- availability_flags |>
  select(cnrfc_id, product_key, status_compact) |>
  distinct() |>
  tidyr::pivot_wider(
    names_from = "product_key",
    values_from = "status_compact",
    values_fill = "not_checked"
  )

matrix_core <- basin_sample |>
  select(
    cnrfc_id,
    display_name,
    river_name,
    location_name,
    forecast_group,
    river_group,
    feature_types,
    lat,
    lon,
    elev_ft,
    inventory_sources,
    sample_wru_xml_products,
    sample_wru_xml_record_count,
    sample_wru_xml_product_groups
  ) |>
  left_join(matrix_status, by = "cnrfc_id")

# Add collision context if available.
if (nrow(id_collision) > 0 && "cnrfc_id" %in% names(id_collision)) {
  collision_context <- id_collision
  if ("feature_type_count" %in% names(collision_context)) {
    collision_context$collision_feature_type_count <- collision_context$feature_type_count
  }
  if ("inventory_rows" %in% names(collision_context)) {
    collision_context$collision_inventory_rows <- collision_context$inventory_rows
  }
  if ("feature_types" %in% names(collision_context)) {
    collision_context$collision_feature_types <- collision_context$feature_types
  }
  if ("inventory_sources" %in% names(collision_context)) {
    collision_context$collision_inventory_sources <- collision_context$inventory_sources
  }

  matrix_core <- matrix_core |>
    left_join(
      collision_context |>
        select(any_of(c(
          "cnrfc_id",
          "collision_feature_type_count",
          "collision_inventory_rows",
          "collision_feature_types",
          "collision_inventory_sources"
        ))),
      by = "cnrfc_id"
    )
}

# Observed product-family booleans.
product_summary <- availability_flags |>
  group_by(.data$cnrfc_id) |>
  summarise(
    has_ensemble_landing = any(.data$product_key == "ensemble_landing_page" & .data$status_compact %in% c("yes", "maybe"), na.rm = TRUE),
    has_any_ensemble_product = any(.data$product_family == "ensemble_product" & .data$status_compact == "yes", na.rm = TRUE),
    has_wy_or_water_supply_product = any(
      .data$product_key %in% c(
        "water_year_trend_plot",
        "water_year_trend_plot_or_related",
        "water_year_accumulated_volume",
        "multi_water_year_accumulated_volume",
        "water_resources_update_page_match",
        "water_resources_update_xml_products"
      ) & .data$status_compact == "yes",
      na.rm = TRUE
    ),
    has_water_resources_update = any(.data$product_key %in% c("water_resources_update_page_match", "water_resources_update_xml_products") & .data$status_compact == "yes", na.rm = TRUE),
    has_basin_temperature = any(.data$product_key == "basin_mean_temperature_forecast" & .data$status_compact %in% c("yes", "maybe"), na.rm = TRUE),
    has_qpf_snow_level_row = any(.data$product_key == "six_day_qpf_snow_level_summary_row" & .data$status_compact == "yes", na.rm = TRUE),
    has_weather_gov_recent_observation = any(.data$product_key == "nws_api_recent_observation_smoke_check" & .data$status_compact == "yes", na.rm = TRUE),
    wrh_invalid_station = any(.data$product_key == "wrh_time_series_viewer" & .data$availability_class == "wrh_viewer_invalid_station_identifier", na.rm = TRUE),
    ensemble_products_yes = paste(sort(unique(.data$product_label[.data$product_family == "ensemble_product" & .data$status_compact == "yes"])), collapse = "; "),
    ensemble_products_no = paste(sort(unique(.data$product_label[.data$product_family == "ensemble_product" & .data$status_compact == "no"])), collapse = "; "),
    .groups = "drop"
  ) |>
  mutate(
    product_bin_observed = case_when(
      .data$has_basin_temperature & .data$has_wy_or_water_supply_product & .data$has_qpf_snow_level_row ~
        "temp + WY/water-supply + QPF/snow-level",
      .data$has_basin_temperature & .data$has_wy_or_water_supply_product ~
        "temp + WY/water-supply",
      .data$has_wy_or_water_supply_product & .data$has_qpf_snow_level_row ~
        "WY/water-supply + QPF/snow-level",
      .data$has_basin_temperature & .data$has_qpf_snow_level_row ~
        "temp + QPF/snow-level",
      .data$has_basin_temperature & .data$has_any_ensemble_product ~
        "temp + other ensemble products",
      .data$has_wy_or_water_supply_product ~
        "WY/water-supply products only",
      .data$has_basin_temperature ~
        "basin temperature only",
      .data$has_qpf_snow_level_row ~
        "QPF/snow-level row only",
      .data$has_any_ensemble_product ~
        "other ensemble products only",
      .data$has_ensemble_landing ~
        "ensemble landing only / products uncertain",
      TRUE ~
        "no confirmed basin products in this sample"
    )
  )

availability_matrix <- matrix_core |>
  left_join(product_summary, by = "cnrfc_id") |>
  mutate(run_timestamp = RUN_TS)

bin_counts <- availability_matrix |>
  count(.data$product_bin_observed, name = "basins", sort = TRUE) |>
  mutate(run_timestamp = RUN_TS)

# ==== 9. Write outputs =======================================================

out_long <- file.path(DIR$qa, paste0("cnrfc_basin_product_availability_long_", RUN_TS, ".csv"))
out_matrix <- file.path(DIR$qa, paste0("cnrfc_basin_product_availability_matrix_", RUN_TS, ".csv"))
out_bins <- file.path(DIR$qa, paste0("cnrfc_basin_product_availability_bins_", RUN_TS, ".csv"))
out_wru <- file.path(DIR$qa, paste0("cnrfc_basin_water_resources_update_matches_", RUN_TS, ".csv"))
out_summary <- file.path(DIR$qa, paste0("cnrfc_basin_product_availability_summary_", RUN_TS, ".csv"))

readr::write_csv(availability_long, out_long)
readr::write_csv(availability_matrix, out_matrix)
readr::write_csv(bin_counts, out_bins)
readr::write_csv(
  availability_long |>
    filter(.data$product_family == "water_resources_update") |>
    select(any_of(c(
      "cnrfc_id", "display_name", "feature_types", "product_url", "availability_class",
      "page_excerpt", "wru_xml_record_count", "wru_xml_files", "wru_xml_product_groups", "wru_xml_record_tags", "wru_xml_labels", "status_code", "bytes", "error", "checked_pacific", "run_timestamp"
    ))),
  out_wru
)

summary_rows <- bind_rows(
  tibble(section = "inputs", metric = "feature_master_path", value = feature_master_path),
  tibble(section = "inputs", metric = "id_collision_path", value = id_collision_path %||% NA_character_),
  tibble(section = "options", metric = "max_basin_ids", value = as.character(MAX_IDS)),
  tibble(section = "options", metric = "sample_strategy", value = SAMPLE_STRATEGY),
  tibble(section = "options", metric = "sample_seed", value = as.character(SAMPLE_SEED)),
  tibble(section = "options", metric = "product_mode", value = PRODUCT_MODE),
  tibble(section = "options", metric = "product_ids", value = if (length(PRODUCT_IDS) == 0) "<none>" else paste(PRODUCT_IDS, collapse = ",")),
  tibble(section = "options", metric = "resume_from_cache", value = as.character(RESUME_FROM_CACHE)),
  tibble(section = "options", metric = "force_refresh", value = as.character(FORCE_REFRESH)),
  tibble(section = "options", metric = "force_refresh_ids", value = if (length(FORCE_REFRESH_IDS) == 0) "<none>" else paste(FORCE_REFRESH_IDS, collapse = ",")),
  tibble(section = "options", metric = "cache_max_age_days", value = as.character(CACHE_MAX_AGE_DAYS)),
  tibble(section = "cache", metric = "cache_file", value = CACHE_FILE),
  tibble(section = "cache", metric = "cache_rows_loaded", value = as.character(nrow(availability_cache))),
  tibble(section = "cache", metric = "cache_ids_loaded", value = as.character(length(unique(availability_cache$cnrfc_id)))),
  tibble(section = "cache", metric = "cached_basin_ids_used", value = as.character(length(unique(cached_basin_ids_used)))),
  tibble(section = "cache", metric = "live_basin_ids_checked", value = as.character(length(unique(live_basin_ids_checked)))),
  tibble(section = "cache", metric = "cache_rows_written", value = if (exists("updated_cache") && !is.null(updated_cache) && nrow(updated_cache) > 0) as.character(nrow(updated_cache)) else NA_character_),
  tibble(section = "counts", metric = "basin_pool_ids", value = as.character(nrow(basin_pool))),
  tibble(section = "counts", metric = "basin_sample_ids", value = as.character(nrow(basin_sample))),
  tibble(section = "sample_composition", metric = "explicit_ids", value = as.character(sample_context$explicit_ids[[1]])),
  tibble(section = "sample_composition", metric = "fnf_ids", value = as.character(sample_context$fnf_ids[[1]])),
  tibble(section = "sample_composition", metric = "wru_xml_yes", value = as.character(sample_context$wru_xml_yes[[1]])),
  tibble(section = "sample_composition", metric = "wru_xml_no", value = as.character(sample_context$wru_xml_no[[1]])),
  tibble(section = "counts", metric = "availability_long_rows", value = as.character(nrow(availability_long))),
  tibble(section = "qpf_summary", metric = "qpf_fetch_class", value = qpf_fetch_class),
  tibble(section = "qpf_summary", metric = "qpf_url", value = QPF_SNOW_LEVEL_URL),
  tibble(section = "water_resources_update", metric = "wru_fetch_class", value = wru_fetch_class),
  tibble(section = "water_resources_update", metric = "wru_url", value = WATER_RESOURCES_UPDATE_URL),
  tibble(section = "water_resources_update", metric = "wru_xml_matrix_path", value = wru_xml_matrix_path %||% NA_character_),
  tibble(section = "water_resources_update", metric = "wru_xml_matrix_ids", value = as.character(nrow(wru_xml_matrix))),
  bin_counts |> transmute(section = "observed_product_bins", metric = .data$product_bin_observed, value = as.character(.data$basins)),
  availability_flags |>
    count(.data$product_family, .data$product_key, .data$availability_class, name = "rows") |>
    transmute(
      section = "availability_classes",
      metric = paste(.data$product_family, .data$product_key, .data$availability_class, sep = " | "),
      value = as.character(.data$rows)
    )
)
readr::write_csv(summary_rows, out_summary)

# ==== 10. Console summary ====================================================

message("CNRFC basin product availability audit complete.")
message("  Long table:   ", out_long)
message("  Matrix:       ", out_matrix)
message("  Bin counts:   ", out_bins)
message("  WR Update:    ", out_wru)
message("  Summary:      ", out_summary)
message("  Cache:        ", if (RESUME_FROM_CACHE) CACHE_FILE else "<disabled>")
message("")
message("Cache / live-check summary:")
message("  IDs from cache: ", length(unique(cached_basin_ids_used)))
message("  IDs live checked: ", length(unique(live_basin_ids_checked)))
message("")

message("Observed product bins in this sample:")
print(bin_counts, n = Inf)

message("")
message("Availability classes by product family/key:")
print(
  availability_flags |>
    count(.data$product_family, .data$product_key, .data$availability_class, name = "rows", sort = TRUE),
  n = 80
)

message("")
message("Example basin matrix rows:")
print(
  availability_matrix |>
    select(any_of(c(
      "cnrfc_id", "display_name", "feature_types", "product_bin_observed",
      "has_basin_temperature", "has_wy_or_water_supply_product", "has_water_resources_update",
      "has_qpf_snow_level_row", "has_weather_gov_recent_observation", "wrh_invalid_station", "ensemble_products_yes"
    ))) |>
    head(20),
  n = 20
)

message("")
message("Interpretation note:")
message("  This is a sampled live availability matrix, not final production symbology yet.")
message("  Use product_bin_observed, the WR Update table, and the long table to keep drilling into stable basin/product patterns before map coloring/popups.")
