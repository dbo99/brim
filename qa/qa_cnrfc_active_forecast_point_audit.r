# ==== qa_cnrfc_active_forecast_point_audit.r ================================
##
## PURPOSE:
##   Audit the active CNRFC homepage forecast-point marker products that should
##   eventually be the authority for BRIM's CNRFC river/reservoir forecast-point
##   layer.
##
## WHAT THIS CHECKS:
##   - Official River Points       (default CNRFC homepage)
##   - Unofficial River Points     (?product=otherPointsFcst)
##   - Reservoir Inflows           (?product=rsvrInflow)
##   - Reservoir Releases          (?product=rsvrRelease)
##   - Three hand-curated Water Supply Index points
##
## DESIGN NOTE:
##   This is an audit, not final map integration.  CNRFC pages are rendered with
##   server-side HTML plus JavaScript.  The parser therefore starts with a safe
##   known-ID scan against the current BRIM CNRFC feature inventory, writes
##   context snippets, and compares found IDs against the expected marker counts
##   visible in the CNRFC UI.  If a future parser finds a cleaner embedded marker
##   array or endpoint, this audit can be tightened.
##
## OUTPUTS:
##   04_processed_data/qa/cnrfc_active_forecast_page_fetch_inventory_*.csv
##   04_processed_data/qa/cnrfc_active_forecast_id_scan_*.csv
##   04_processed_data/qa/cnrfc_active_forecast_product_matrix_*.csv
##   04_processed_data/qa/cnrfc_active_forecast_product_bins_*.csv
##   04_processed_data/qa/cnrfc_active_forecast_marker_context_*.csv
##   04_processed_data/qa/cnrfc_active_forecast_summary_*.csv

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
  library(tibble)
  library(stringr)
})

RUN_TS <- make_timestamp()

dir.create(DIR$qa, showWarnings = FALSE, recursive = TRUE)

opt <- function(name, default = NULL) {
  getOption(name, default)
}

FETCH_PAGES <- isTRUE(opt("BRIM_CNRFC_ACTIVE_FETCH_PAGES", TRUE))
REQUEST_DELAY_SEC <- suppressWarnings(as.numeric(opt("BRIM_CNRFC_ACTIVE_REQUEST_DELAY_SEC", 0.10)))
if (is.na(REQUEST_DELAY_SEC) || REQUEST_DELAY_SEC < 0) REQUEST_DELAY_SEC <- 0.10
MAX_CONTEXT_CHARS <- suppressWarnings(as.integer(opt("BRIM_CNRFC_ACTIVE_MAX_CONTEXT_CHARS", 180L)))
if (is.na(MAX_CONTEXT_CHARS) || MAX_CONTEXT_CHARS < 40L) MAX_CONTEXT_CHARS <- 180L

pt_log <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " | ", ...)
}

pt_latest_file <- function(pattern, dir = DIR$qa) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  files[order(file.info(files)$mtime, decreasing = TRUE)][[1]]
}

pt_read_csv_or_empty <- function(path) {
  if (is.na(path) || !file.exists(path)) return(tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

pt_read_rds_or_csv <- function(rds_path, csv_pattern) {
  if (!is.na(rds_path) && file.exists(rds_path)) {
    return(readRDS(rds_path))
  }
  csv_path <- pt_latest_file(csv_pattern)
  pt_read_csv_or_empty(csv_path)
}

pt_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

pt_has_text <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

pt_collapse <- function(x) {
  x <- unique(trimws(as.character(x[!is.na(x) & nzchar(trimws(as.character(x)))])))
  if (length(x) == 0) return(NA_character_)
  paste(sort(x), collapse = ";")
}

pt_first_nonempty <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) return(NA_character_)
  trimws(x[[1]])
}

pt_safe_detect <- function(x, pattern) {
  stringr::str_detect(pt_chr(x), stringr::regex(pattern, ignore_case = TRUE))
}

pt_clean_text <- function(x) {
  x <- iconv(x, from = "", to = "UTF-8", sub = "byte")
  x[is.na(x)] <- ""
  x
}

pt_extract_title <- function(txt) {
  m <- stringr::str_match(txt, stringr::regex("<title[^>]*>(.*?)</title>", ignore_case = TRUE, dotall = TRUE))
  if (is.na(m[1, 2])) return(NA_character_)
  out <- gsub("<[^>]+>", " ", m[1, 2])
  out <- gsub("\\s+", " ", out)
  trimws(out)
}

pt_extract_marker_count <- function(txt) {
  m <- stringr::str_match_all(txt, stringr::regex("Markers\\s*:?\\s*([0-9,]+)", ignore_case = TRUE))[[1]]
  if (nrow(m) == 0) return(NA_integer_)
  vals <- suppressWarnings(as.integer(gsub(",", "", m[, 2])))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_integer_)
  vals[[1]]
}

pt_fetch_url <- function(url) {
  out <- list(
    url = url,
    status_code = NA_integer_,
    http_ok = FALSE,
    bytes = NA_integer_,
    content_type = NA_character_,
    final_url = url,
    text = "",
    error = NA_character_
  )
  tryCatch({
    if (requireNamespace("curl", quietly = TRUE)) {
      h <- curl::new_handle(
        useragent = "BRIM CNRFC active forecast-point audit",
        followlocation = TRUE,
        timeout = 60
      )
      res <- curl::curl_fetch_memory(url, handle = h)
      out$status_code <- as.integer(res$status_code)
      out$http_ok <- out$status_code >= 200L && out$status_code < 300L
      out$bytes <- length(res$content)
      out$content_type <- if (!is.null(res$headers)) {
        hdr <- rawToChar(res$headers)
        ct <- stringr::str_match(hdr, stringr::regex("content-type:\\s*([^\\r\\n]+)", ignore_case = TRUE))[, 2]
        ifelse(is.na(ct), NA_character_, trimws(ct))
      } else {
        NA_character_
      }
      if (!is.null(res$url)) out$final_url <- res$url
      raw <- res$content
    } else {
      con <- url(url, open = "rb")
      on.exit(close(con), add = TRUE)
      raw <- readBin(con, what = "raw", n = 50000000L)
      out$status_code <- 200L
      out$http_ok <- TRUE
      out$bytes <- length(raw)
    }
    raw <- raw[raw != as.raw(0)]
    txt <- rawToChar(raw, multiple = FALSE)
    out$text <- pt_clean_text(txt)
    out
  }, error = function(e) {
    out$error <- conditionMessage(e)
    out
  })
}

pt_count_id_in_text <- function(txt, id) {
  if (!nzchar(txt) || is.na(id) || !nzchar(id)) return(0L)
  pattern <- paste0("(?<![A-Z0-9])", id, "(?![A-Z0-9])")
  m <- gregexpr(pattern, txt, perl = TRUE)
  if (length(m) == 0 || identical(m[[1]], -1L)) return(0L)
  length(m[[1]])
}

pt_first_context <- function(txt, id, n = MAX_CONTEXT_CHARS) {
  if (!nzchar(txt) || is.na(id) || !nzchar(id)) return(NA_character_)
  pattern <- paste0("(?<![A-Z0-9])", id, "(?![A-Z0-9])")
  m <- regexpr(pattern, txt, perl = TRUE)
  if (m[[1]] < 0) return(NA_character_)
  start <- max(1L, as.integer(m[[1]]) - n)
  end <- min(nchar(txt), as.integer(m[[1]]) + nchar(id) + n)
  ctx <- substr(txt, start, end)
  ctx <- gsub("\\s+", " ", ctx)
  trimws(ctx)
}

# ==== 2. Active product definitions =========================================

active_products <- tibble::tribble(
  ~active_product_key,       ~active_product_label,       ~url,                                                    ~expected_marker_count, ~active_product_class,
  "official_river_points",   "Official River Points",     "https://www.cnrfc.noaa.gov/",                         102L,                   "official_river",
  "unofficial_river_points", "Unofficial River Points",   "https://www.cnrfc.noaa.gov/?product=otherPointsFcst", 185L,                   "unofficial_river",
  "reservoir_inflows",       "Reservoir Inflows",         "https://www.cnrfc.noaa.gov/?product=rsvrInflow",       104L,                   "reservoir_inflow",
  "reservoir_releases",      "Reservoir Releases",        "https://www.cnrfc.noaa.gov/?product=rsvrRelease",       61L,                    "reservoir_release"
)

manual_water_supply_index_points <- tibble::tribble(
  ~cnrfc_id, ~display_name,                                     ~active_product_key,   ~active_product_label, ~active_product_class, ~lat,        ~lon,          ~symbolic_location, ~url,
  "SACC0",  "Sacramento Valley - Water Supply Index",          "water_supply_index", "Water Supply Index",  "water_supply_index",  38.455664,   -121.5016200,  "sac river input", "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=SACC0&prodID=9",
  "VNSC0",  "San Joaquin Valley - Water Supply Index",         "water_supply_index", "Water Supply Index",  "water_supply_index",  37.67601249, -121.2662907,  "sj river input",  "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=VNSC0&prodID=9",
  "MLIC0",  "Central Valley - Water Supply Index",             "water_supply_index", "Water Supply Index",  "water_supply_index",  38.055198,   -121.9119040,  "delta outflow",   "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=MLIC0&prodID=9"
)

# ==== 3. Feature inventory ===================================================

feature_master <- pt_read_rds_or_csv(
  file.path(DIR$rds, "cnrfc_feature_master.rds"),
  "^cnrfc_feature_master_[0-9_]+\\.csv$"
)

if (nrow(feature_master) == 0) {
  stop(
    "No CNRFC feature master found. Run preprocess_cnrfc_feature_inventory() first, ",
    "or run qa_cnrfc_product_intelligence_audit() to create the source CSV."
  )
}

for (nm in c(
  "feature_type", "cnrfc_id", "display_name", "river_name", "location_name",
  "forecast_group", "river_group", "raw_family", "raw_kind", "raw_class1",
  "raw_class2", "datatransmission", "lat", "lon", "elev_ft", "inventory_sources"
)) {
  if (!nm %in% names(feature_master)) feature_master[[nm]] <- NA
}

feature_priority <- c(
  cnrfc_river_point = 1L,
  cnrfc_reservoir_point = 2L,
  cnrfc_stream_or_reservoir_point_cache = 3L,
  brim_reservoir_layer_cdec_cnrfc_crosswalk = 4L,
  cnrfc_special_point = 5L,
  cnrfc_basin = 6L,
  cnrfc_fnf_sierra_delta_basin = 7L,
  cnrfc_precip_station_cache = 8L,
  cnrfc_precip_station_catalog = 9L,
  cnrfc_precip_station_recent_index = 10L
)

fm <- feature_master %>%
  mutate(
    cnrfc_id = toupper(trimws(as.character(.data$cnrfc_id))),
    feature_type = as.character(.data$feature_type),
    feature_priority = unname(feature_priority[.data$feature_type]),
    feature_priority = if_else(is.na(.data$feature_priority), 99L, .data$feature_priority),
    lat = suppressWarnings(as.numeric(.data$lat)),
    lon = suppressWarnings(as.numeric(.data$lon)),
    elev_ft = suppressWarnings(as.numeric(.data$elev_ft))
  ) %>%
  filter(!is.na(.data$cnrfc_id), nzchar(.data$cnrfc_id))

feature_summary <- fm %>%
  arrange(.data$cnrfc_id, .data$feature_priority) %>%
  group_by(.data$cnrfc_id) %>%
  summarise(
    display_name = pt_first_nonempty(.data$display_name),
    river_name = pt_first_nonempty(.data$river_name),
    location_name = pt_first_nonempty(.data$location_name),
    forecast_group = pt_first_nonempty(.data$forecast_group),
    river_group = pt_first_nonempty(.data$river_group),
    feature_types = pt_collapse(.data$feature_type),
    inventory_sources = pt_collapse(.data$inventory_sources),
    raw_families = pt_collapse(.data$raw_family),
    raw_kinds = pt_collapse(.data$raw_kind),
    raw_class1_values = pt_collapse(.data$raw_class1),
    raw_class2_values = pt_collapse(.data$raw_class2),
    datatransmission_types = pt_collapse(.data$datatransmission),
    lat = suppressWarnings(as.numeric(.data$lat[which(!is.na(.data$lat))[1]])),
    lon = suppressWarnings(as.numeric(.data$lon[which(!is.na(.data$lon))[1]])),
    elev_ft = suppressWarnings(as.numeric(.data$elev_ft[which(!is.na(.data$elev_ft))[1]])),
    has_river_point = any(.data$feature_type == "cnrfc_river_point", na.rm = TRUE),
    has_reservoir_point = any(.data$feature_type == "cnrfc_reservoir_point", na.rm = TRUE),
    has_stream_or_reservoir_cache_point = any(.data$feature_type == "cnrfc_stream_or_reservoir_point_cache", na.rm = TRUE),
    has_reservoir_crosswalk = any(.data$feature_type == "brim_reservoir_layer_cdec_cnrfc_crosswalk", na.rm = TRUE),
    has_precip_station = any(.data$feature_type %in% c("cnrfc_precip_station_cache", "cnrfc_precip_station_catalog", "cnrfc_precip_station_recent_index"), na.rm = TRUE),
    has_basin_polygon = any(.data$feature_type == "cnrfc_basin", na.rm = TRUE),
    has_fnf_basin = any(.data$feature_type == "cnrfc_fnf_sierra_delta_basin", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    display_name = case_when(
      pt_has_text(.data$display_name) ~ .data$display_name,
      pt_has_text(.data$river_name) & pt_has_text(.data$location_name) ~ paste(.data$river_name, "-", .data$location_name),
      TRUE ~ .data$cnrfc_id
    )
  )

known_ids <- sort(unique(c(feature_summary$cnrfc_id, manual_water_supply_index_points$cnrfc_id)))
known_ids <- known_ids[!is.na(known_ids) & nzchar(known_ids) & nchar(known_ids) >= 4]

pt_log("Known CNRFC IDs for page scan: ", length(known_ids))

# ==== 4. Fetch active product pages and scan known IDs =======================

fetch_inventory <- tibble()
id_scan <- tibble()
context_rows <- tibble()

if (FETCH_PAGES) {
  for (i in seq_len(nrow(active_products))) {
    prod <- active_products[i, ]
    pt_log("Fetching active CNRFC product page [", i, "/", nrow(active_products), "]: ", prod$active_product_label)
    res <- pt_fetch_url(prod$url)
    txt <- res$text

    page_title <- pt_extract_title(txt)
    marker_count_page <- pt_extract_marker_count(txt)

    counts <- vapply(known_ids, function(id) pt_count_id_in_text(txt, id), integer(1))
    found <- names(counts)[counts > 0]

    fetch_inventory <- bind_rows(fetch_inventory, tibble(
      run_timestamp = RUN_TS,
      active_product_key = prod$active_product_key,
      active_product_label = prod$active_product_label,
      active_product_class = prod$active_product_class,
      url = prod$url,
      final_url = res$final_url,
      status_code = res$status_code,
      http_ok = res$http_ok,
      bytes = res$bytes,
      decoded_chars = nchar(txt),
      content_type = res$content_type,
      page_title = page_title,
      marker_count_expected_from_manual_ui = prod$expected_marker_count,
      marker_count_detected_from_page_text = marker_count_page,
      known_ids_found = length(found),
      id_count_minus_expected = length(found) - prod$expected_marker_count,
      error = res$error
    ))

    if (length(found) > 0) {
      tmp <- tibble(
        run_timestamp = RUN_TS,
        active_product_key = prod$active_product_key,
        active_product_label = prod$active_product_label,
        active_product_class = prod$active_product_class,
        cnrfc_id = found,
        text_count_case_sensitive = as.integer(counts[found]),
        url = prod$url
      )
      id_scan <- bind_rows(id_scan, tmp)

      ctx <- tibble(
        run_timestamp = RUN_TS,
        active_product_key = prod$active_product_key,
        active_product_label = prod$active_product_label,
        active_product_class = prod$active_product_class,
        cnrfc_id = found,
        first_context = vapply(found, function(id) pt_first_context(txt, id), character(1)),
        url = prod$url
      )
      context_rows <- bind_rows(context_rows, ctx)
    }

    if (REQUEST_DELAY_SEC > 0 && i < nrow(active_products)) Sys.sleep(REQUEST_DELAY_SEC)
  }
} else {
  pt_log("Fetch disabled by option; writing manual water-supply index rows only.")
}

manual_scan <- manual_water_supply_index_points %>%
  transmute(
    run_timestamp = RUN_TS,
    active_product_key = .data$active_product_key,
    active_product_label = .data$active_product_label,
    active_product_class = .data$active_product_class,
    cnrfc_id = .data$cnrfc_id,
    text_count_case_sensitive = NA_integer_,
    url = .data$url
  )

id_scan_all <- bind_rows(id_scan, manual_scan) %>%
  distinct(.data$active_product_key, .data$cnrfc_id, .keep_all = TRUE)

# ==== 5. Active product matrix ==============================================

product_flags <- id_scan_all %>%
  mutate(value = TRUE) %>%
  select(
    "cnrfc_id",
    "active_product_key",
    "active_product_class",
    "value",
    "text_count_case_sensitive",
    "url"
  )

active_matrix <- product_flags %>%
  group_by(.data$cnrfc_id) %>%
  summarise(
    active_product_keys = pt_collapse(.data$active_product_key),
    active_product_classes = pt_collapse(.data$active_product_class),
    active_product_urls = pt_collapse(.data$url),
    active_page_text_hit_count = suppressWarnings(sum(as.integer(.data$text_count_case_sensitive), na.rm = TRUE)),
    active_official_river_point = any(.data$active_product_key == "official_river_points", na.rm = TRUE),
    active_unofficial_river_point = any(.data$active_product_key == "unofficial_river_points", na.rm = TRUE),
    active_reservoir_inflow = any(.data$active_product_key == "reservoir_inflows", na.rm = TRUE),
    active_reservoir_release = any(.data$active_product_key == "reservoir_releases", na.rm = TRUE),
    active_water_supply_index = any(.data$active_product_key == "water_supply_index", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(feature_summary, by = "cnrfc_id") %>%
  left_join(
    manual_water_supply_index_points %>%
      transmute(
        cnrfc_id = .data$cnrfc_id,
        manual_index_display_name = .data$display_name,
        manual_index_lat = .data$lat,
        manual_index_lon = .data$lon,
        manual_index_symbolic_location = .data$symbolic_location
      ),
    by = "cnrfc_id"
  ) %>%
  mutate(
    display_name = case_when(
      .data$active_water_supply_index & pt_has_text(.data$manual_index_display_name) ~ .data$manual_index_display_name,
      pt_has_text(.data$display_name) ~ .data$display_name,
      TRUE ~ .data$cnrfc_id
    ),
    lat = if_else(.data$active_water_supply_index & !is.na(.data$manual_index_lat), .data$manual_index_lat, .data$lat),
    lon = if_else(.data$active_water_supply_index & !is.na(.data$manual_index_lon), .data$manual_index_lon, .data$lon),
    active_river_reservoir_forecast_point = .data$active_official_river_point |
      .data$active_unofficial_river_point |
      .data$active_reservoir_inflow |
      .data$active_reservoir_release |
      .data$active_water_supply_index,
    active_forecast_point_subtype = case_when(
      .data$active_water_supply_index ~ "water_supply_index",
      .data$active_reservoir_inflow & .data$active_reservoir_release ~ "reservoir_inflow_and_release",
      .data$active_reservoir_inflow ~ "reservoir_inflow",
      .data$active_reservoir_release ~ "reservoir_release",
      .data$active_official_river_point & .data$active_unofficial_river_point ~ "official_and_unofficial_river",
      .data$active_official_river_point ~ "official_river",
      .data$active_unofficial_river_point ~ "unofficial_river",
      TRUE ~ "active_product_other"
    ),
    homepage_marker_authority_class = case_when(
      .data$active_water_supply_index ~ "manual water-supply index point",
      .data$active_official_river_point | .data$active_unofficial_river_point ~ "active CNRFC river forecast marker",
      .data$active_reservoir_inflow | .data$active_reservoir_release ~ "active CNRFC reservoir forecast marker",
      TRUE ~ "active CNRFC product-page ID"
    ),
    deterministic_forecast_status = case_when(
      .data$active_water_supply_index ~ "water-supply index product; ordinary deterministic point forecast not assumed",
      .data$active_river_reservoir_forecast_point ~ "deterministic forecast assumed from active CNRFC forecast marker",
      TRUE ~ NA_character_
    ),
    ensemble_forecast_status = case_when(
      .data$active_water_supply_index ~ "CNRFC water-supply index ensemble product link known",
      .data$active_river_reservoir_forecast_point ~ "not checked; deterministic-only vs deterministic+ensemble still requires later product check",
      TRUE ~ NA_character_
    ),
    run_timestamp = RUN_TS
  ) %>%
  select(all_of(c(
    "cnrfc_id", "display_name", "lat", "lon", "elev_ft",
    "feature_types", "raw_families", "raw_kinds", "raw_class1_values", "raw_class2_values",
    "datatransmission_types", "inventory_sources",
    "active_river_reservoir_forecast_point", "active_forecast_point_subtype",
    "homepage_marker_authority_class", "active_product_keys", "active_product_classes", "active_product_urls",
    "active_page_text_hit_count",
    "active_official_river_point", "active_unofficial_river_point",
    "active_reservoir_inflow", "active_reservoir_release", "active_water_supply_index",
    "manual_index_symbolic_location", "deterministic_forecast_status", "ensemble_forecast_status",
    "has_river_point", "has_reservoir_point", "has_stream_or_reservoir_cache_point",
    "has_reservoir_crosswalk", "has_precip_station", "has_basin_polygon", "has_fnf_basin",
    "run_timestamp"
  )))

active_bins <- active_matrix %>%
  count(
    .data$active_forecast_point_subtype,
    .data$homepage_marker_authority_class,
    name = "points"
  ) %>%
  arrange(.data$homepage_marker_authority_class, .data$active_forecast_point_subtype) %>%
  mutate(run_timestamp = RUN_TS)

summary_tbl <- bind_rows(
  tibble(run_timestamp = RUN_TS, metric = "active_product_pages_attempted", value = nrow(active_products)),
  tibble(run_timestamp = RUN_TS, metric = "active_product_pages_http_ok", value = sum(fetch_inventory$http_ok, na.rm = TRUE)),
  tibble(run_timestamp = RUN_TS, metric = "active_product_id_rows", value = nrow(id_scan_all)),
  tibble(run_timestamp = RUN_TS, metric = "active_river_reservoir_forecast_point_ids", value = nrow(active_matrix)),
  tibble(run_timestamp = RUN_TS, metric = "manual_water_supply_index_points", value = sum(active_matrix$active_water_supply_index, na.rm = TRUE)),
  tibble(run_timestamp = RUN_TS, metric = "official_river_point_ids_found", value = sum(active_matrix$active_official_river_point, na.rm = TRUE)),
  tibble(run_timestamp = RUN_TS, metric = "unofficial_river_point_ids_found", value = sum(active_matrix$active_unofficial_river_point, na.rm = TRUE)),
  tibble(run_timestamp = RUN_TS, metric = "reservoir_inflow_ids_found", value = sum(active_matrix$active_reservoir_inflow, na.rm = TRUE)),
  tibble(run_timestamp = RUN_TS, metric = "reservoir_release_ids_found", value = sum(active_matrix$active_reservoir_release, na.rm = TRUE))
)

# ==== 6. Write outputs =======================================================

out_fetch <- file.path(DIR$qa, paste0("cnrfc_active_forecast_page_fetch_inventory_", RUN_TS, ".csv"))
out_scan <- file.path(DIR$qa, paste0("cnrfc_active_forecast_id_scan_", RUN_TS, ".csv"))
out_matrix <- file.path(DIR$qa, paste0("cnrfc_active_forecast_product_matrix_", RUN_TS, ".csv"))
out_bins <- file.path(DIR$qa, paste0("cnrfc_active_forecast_product_bins_", RUN_TS, ".csv"))
out_context <- file.path(DIR$qa, paste0("cnrfc_active_forecast_marker_context_", RUN_TS, ".csv"))
out_summary <- file.path(DIR$qa, paste0("cnrfc_active_forecast_summary_", RUN_TS, ".csv"))

latest_matrix <- file.path(DIR$qa, "cnrfc_active_forecast_product_matrix_latest.csv")
latest_bins <- file.path(DIR$qa, "cnrfc_active_forecast_product_bins_latest.csv")
latest_summary <- file.path(DIR$qa, "cnrfc_active_forecast_summary_latest.csv")

readr::write_csv(fetch_inventory, out_fetch)
readr::write_csv(id_scan_all, out_scan)
readr::write_csv(active_matrix, out_matrix)
readr::write_csv(active_bins, out_bins)
readr::write_csv(context_rows, out_context)
readr::write_csv(summary_tbl, out_summary)

readr::write_csv(active_matrix, latest_matrix)
readr::write_csv(active_bins, latest_bins)
readr::write_csv(summary_tbl, latest_summary)

# Write RDS copies for later 53_ integration, but do not modify 53_ yet.
saveRDS(active_matrix, file.path(DIR$rds, "cnrfc_active_forecast_product_matrix.rds"))
saveRDS(active_bins, file.path(DIR$rds, "cnrfc_active_forecast_product_bins.rds"))
saveRDS(summary_tbl, file.path(DIR$rds, "cnrfc_active_forecast_summary.rds"))

pt_log("CNRFC active forecast-point audit complete.")
message("  Page fetch inventory: ", out_fetch)
message("  ID scan:              ", out_scan)
message("  Active matrix:        ", out_matrix)
message("  Active bins:          ", out_bins)
message("  Context snippets:     ", out_context)
message("  Summary:              ", out_summary)

message("\nPage fetch / expected-count comparison:")
print(fetch_inventory %>%
        select(
          "active_product_label",
          "status_code",
          "marker_count_expected_from_manual_ui",
          "marker_count_detected_from_page_text",
          "known_ids_found",
          "id_count_minus_expected"
        ), n = Inf)

message("\nActive forecast-point bins:")
print(active_bins, n = Inf)

message("\nActive forecast-point summary:")
print(summary_tbl, n = Inf)

message("\nInterpretation note:")
message("  This is an active homepage/product-page audit.  It should become the authority for ")
message("  53_ river/reservoir forecast-point filtering after we inspect the output.")
message("  If known_ids_found is much larger than the visible marker count, the page is probably ")
message("  carrying non-marker IDs in scripts and the context CSV should be reviewed before integration.")
