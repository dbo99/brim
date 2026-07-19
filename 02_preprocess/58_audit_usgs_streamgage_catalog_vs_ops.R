# ==== 58_audit_usgs_streamgage_catalog_vs_ops.R =============================
##
## PURPOSE:
##   Build a reproducible, updateable audit between the Local USGS streamgage
##   catalog layer and the current Ops Live USGS streamflow snapshot.
##
## WHY THIS EXISTS:
##   BRIM has two different but complementary USGS stream layers:
##     * Local catalog = broad screening inventory with station metadata,
##       active/inactive status, period-of-record fields, and durable USGS links.
##     * Ops Live = current/recent operational streamflow snapshot used for
##       live-condition screening.
##
##   The live streamflow GeoJSON intentionally carries the full station-index
##   universe so BRIM can show no-value/stale/current distinctions without
##   dropping sites.  Therefore Local-vs-Ops membership is not a meaningful halo
##   for streamgages.  The useful sync is current-value availability:
##     - Is there latest-continuous discharge and/or stage?
##     - How old is the latest current-value signal?
##     - Is the site active/inactive in the Local catalog?
##
## IMPORTANT WORDING:
##   Treat Ops fields as the current/recent-value snapshot, not as timeless
##   operational status.  Re-running this audit after refreshing the static
##   station index and live feed should update current-value counts.
##
## INPUTS EXPECTED:
##   Local catalog RDS:
##     04_processed_data/cache/latest/usgs_streamgages_map.rds
##
##   Ops Live current snapshot:
##     MAP_DISPLAY$ops_usgs_streamflow_latest_url
##     usually https://dbo99.github.io/brim-live-data-feeds/data/usgs_streamflow_latest_ca.geojson
##
## OUTPUTS:
##   04_processed_data/qa/usgs_streamgage_catalog_vs_ops_sync_summary_latest.csv
##   04_processed_data/qa/usgs_streamgage_catalog_vs_ops_sync_matrix_latest.csv
##   04_processed_data/qa/usgs_streamgage_catalog_only_latest.csv
##   04_processed_data/qa/usgs_streamgage_ops_only_latest.csv
##   04_processed_data/qa/usgs_streamgage_sync_column_inventory_latest.csv
##   04_processed_data/qa/usgs_streamgage_sync_por_field_candidates_latest.csv
##   timestamped copies of the same outputs
##
## NEXT SCRIPT/PATCH SHOULD USE:
##   usgs_streamgage_catalog_vs_ops_sync_matrix_latest.csv
##   and usgs_streamgage_current_value_bins_latest.csv to drive Local catalog
##   legend/filter design.  The Local streamgage layer should emphasize
##   active/inactive, POR, BLM proximity, and clear handoff to Ops Live for
##   current/recent flow or stage values.
## ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(sf)
})

if (file.exists("00_config/config_paths.r")) {
  source("00_config/config_paths.r")
}
if (file.exists("00_config/config_map_display.r")) {
  source("00_config/config_map_display.r")
}

pt_qa_dir <- if (exists("DIR") && !is.null(DIR$qa)) DIR$qa else file.path("04_processed_data", "qa")
pt_cache_last_dir <- if (exists("DIR") && !is.null(DIR$cache_last)) DIR$cache_last else file.path("04_processed_data", "cache", "latest")
dir.create(pt_qa_dir, recursive = TRUE, showWarnings = FALSE)

RUN_TS <- format(Sys.time(), "%Y%m%d_%H%M%S")

pt_first_existing_path <- function(paths) {
  paths <- as.character(paths)
  paths <- paths[!is.na(paths) & nzchar(paths)]
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

pt_clean_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NaN", "NULL", "null", "undefined")] <- NA_character_
  x
}

pt_clean_site_no <- function(x) {
  x <- toupper(pt_clean_chr(x))
  x <- stringr::str_replace(x, "^USGS[-_: /]*", "")
  x <- stringr::str_replace(x, "^NWIS[-_: /]*", "")
  x <- stringr::str_replace_all(x, "\\s+", "")
  x[x %in% c("", "NA", "NAN", "NULL")] <- NA_character_
  x
}

pt_boolish <- function(x) {
  xx <- tolower(pt_clean_chr(x))
  dplyr::case_when(
    xx %in% c("true", "t", "1", "yes", "y") ~ TRUE,
    xx %in% c("false", "f", "0", "no", "n") ~ FALSE,
    TRUE ~ NA
  )
}

pt_pick_col <- function(x, candidates) {
  nms <- names(x)
  hit <- candidates[candidates %in% nms]
  if (length(hit) > 0) return(hit[[1]])

  ## Case-insensitive fallback, preserving the actual source column name.
  lower_lookup <- stats::setNames(nms, tolower(nms))
  lower_hit <- tolower(candidates)[tolower(candidates) %in% names(lower_lookup)]
  if (length(lower_hit) > 0) return(unname(lower_lookup[[lower_hit[[1]]]]))

  NA_character_
}

pt_as_year <- function(x) {
  x_chr <- pt_clean_chr(x)
  out <- suppressWarnings(as.integer(x_chr))

  ## Extract a leading 4-digit year from common date strings such as
  ## 1995-10-01, 10/01/1995, or text containing a year.
  need <- is.na(out) & !is.na(x_chr)
  if (any(need)) {
    y1 <- suppressWarnings(as.integer(stringr::str_extract(x_chr[need], "(?<!\\d)(18|19|20)\\d{2}(?!\\d)")))
    out[need] <- y1
  }

  this_year <- as.integer(format(Sys.Date(), "%Y")) + 1L
  out[!is.na(out) & (out < 1800L | out > this_year)] <- NA_integer_
  out
}

pt_first_non_na_numeric <- function(df, cols) {
  cols <- cols[cols %in% names(df)]
  if (length(cols) == 0) return(rep(NA_real_, nrow(df)))
  out <- rep(NA_real_, nrow(df))
  for (cc in cols) {
    val <- suppressWarnings(as.numeric(df[[cc]]))
    idx <- is.na(out) & !is.na(val)
    out[idx] <- val[idx]
  }
  out
}

pt_first_non_na_year <- function(df, cols) {
  cols <- cols[cols %in% names(df)]
  if (length(cols) == 0) return(rep(NA_integer_, nrow(df)))
  out <- rep(NA_integer_, nrow(df))
  for (cc in cols) {
    val <- pt_as_year(df[[cc]])
    idx <- is.na(out) & !is.na(val)
    out[idx] <- val[idx]
  }
  out
}

pt_geometry_lon_lat <- function(x, prefix) {
  if (!inherits(x, "sf") || nrow(x) == 0) {
    x[[paste0(prefix, "_lon")]] <- NA_real_
    x[[paste0(prefix, "_lat")]] <- NA_real_
    return(x)
  }

  xx <- x
  if (!is.na(sf::st_crs(xx))) {
    xx <- sf::st_transform(xx, 4326)
  }
  coords <- suppressWarnings(sf::st_coordinates(xx))
  if (is.null(dim(coords)) || nrow(coords) != nrow(xx)) {
    coords <- matrix(NA_real_, nrow = nrow(xx), ncol = 2)
  }
  x[[paste0(prefix, "_lon")]] <- coords[, 1]
  x[[paste0(prefix, "_lat")]] <- coords[, 2]
  x
}

pt_read_local_streamgages <- function() {
  local_path <- pt_first_existing_path(c(
    file.path(pt_cache_last_dir, "usgs_streamgages_map.rds"),
    file.path("04_processed_data", "cache", "latest", "usgs_streamgages_map.rds")
  ))

  if (is.na(local_path)) {
    stop("Local USGS streamgage catalog RDS not found. Expected usgs_streamgages_map.rds under 04_processed_data/cache/latest.")
  }

  message("Reading Local USGS streamgage catalog: ", local_path)
  x <- readRDS(local_path)
  x <- pt_geometry_lon_lat(x, "local")
  if (inherits(x, "sf")) x <- sf::st_drop_geometry(x)
  x <- tibble::as_tibble(x)
  attr(x, "source_path") <- local_path
  x
}

pt_read_ops_streamflow <- function() {
  ops_url <- NA_character_
  if (exists("MAP_DISPLAY") && !is.null(MAP_DISPLAY$ops_usgs_streamflow_latest_url)) {
    ops_url <- MAP_DISPLAY$ops_usgs_streamflow_latest_url
  }
  if (is.na(ops_url) || !nzchar(ops_url)) {
    ops_url <- "https://dbo99.github.io/brim-live-data-feeds/data/usgs_streamflow_latest_ca.geojson"
  }

  message("Reading current Ops USGS streamflow snapshot: ", ops_url)

  ## sf::st_read handles GeoJSON FeatureCollections cleanly and preserves point
  ## geometry.  If the network request fails, the stop message is intentional:
  ## this audit is meant to document the Ops streamflow source, not guess it.
  x <- tryCatch(
    sf::st_read(ops_url, quiet = TRUE),
    error = function(e) {
      stop(
        "Could not read Ops USGS streamflow GeoJSON from ", ops_url,
        ". Check internet access or the configured MAP_DISPLAY$ops_usgs_streamflow_latest_url. Original error: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  x <- pt_geometry_lon_lat(x, "ops")
  if (inherits(x, "sf")) x <- sf::st_drop_geometry(x)
  x <- tibble::as_tibble(x)
  attr(x, "source_url") <- ops_url
  x
}

pt_column_inventory <- function(local_raw, ops_raw) {
  tibble(
    source = c(rep("local_catalog", length(names(local_raw))), rep("ops_current_snapshot", length(names(ops_raw)))),
    column = c(names(local_raw), names(ops_raw)),
    column_lower = tolower(c(names(local_raw), names(ops_raw))),
    likely_role = dplyr::case_when(
      stringr::str_detect(column_lower, "site(_|\\.)?no|site(_|\\.)?number|monitoring") ~ "site id",
      stringr::str_detect(column_lower, "station|name|site_nm|site_name") ~ "station name",
      stringr::str_detect(column_lower, "status|active|inactive") ~ "status/activity",
      stringr::str_detect(column_lower, "por|period|record|start|begin|first|oldest|end|last|latest|count|measure|year|date") ~ "period-of-record / recency candidate",
      stringr::str_detect(column_lower, "blm|distance|dist") ~ "BLM proximity candidate",
      stringr::str_detect(column_lower, "lat|lon|long|coord") ~ "coordinate candidate",
      TRUE ~ "other"
    )
  )
}

local_raw <- pt_read_local_streamgages()
ops_raw <- pt_read_ops_streamflow()

local_site_col <- pt_pick_col(local_raw, c(
  "site_no", "usgsswpop_site_no", "site_number", "site_id", "monitoring_location_id", "monitoringLocationIdentifier"
))
ops_site_col <- pt_pick_col(ops_raw, c(
  "site_no", "site_number", "site_id", "monitoring_location_id", "monitoringLocationIdentifier",
  "monitoringLocationIdentifier.identifier", "properties.site_no"
))

if (is.na(local_site_col)) {
  stop("Could not find a site ID column in Local USGS streamgage catalog. Columns: ", paste(names(local_raw), collapse = ", "))
}
if (is.na(ops_site_col)) {
  stop("Could not find a site ID column in Ops USGS streamflow snapshot. Columns: ", paste(names(ops_raw), collapse = ", "))
}

local_name_col <- pt_pick_col(local_raw, c("name", "station_nm", "site_name", "usgsswpop_name", "monitoringLocationName"))
ops_name_col <- pt_pick_col(ops_raw, c("name", "station_nm", "site_name", "monitoringLocationName"))
local_status_col <- pt_pick_col(local_raw, c("status", "usgsswpop_status", "site_status", "active_status"))
ops_status_col <- pt_pick_col(ops_raw, c("status", "site_status", "flow_status", "condition", "obs_status"))

## Current/recent value fields written by build_usgs_streamflow_latest_ca.R.
## These are more meaningful than plain Ops GeoJSON membership because that
## GeoJSON intentionally includes the full station-index universe.
ops_latest_status_col <- pt_pick_col(ops_raw, c("latest_status", "ops_latest_status"))
ops_q_cfs_col <- pt_pick_col(ops_raw, c("q_cfs", "latest_q_cfs", "discharge_cfs"))
ops_q_age_col <- pt_pick_col(ops_raw, c("q_obs_age_hours", "q_age_hours", "latest_q_age_hours"))
ops_stage_age_col <- pt_pick_col(ops_raw, c("stage_obs_age_hours", "stage_age_hours"))
ops_has_q_col <- pt_pick_col(ops_raw, c("has_latest_iv_q", "has_q", "has_latest_discharge"))
ops_has_stage_col <- pt_pick_col(ops_raw, c("has_latest_iv_stage", "has_stage", "has_latest_stage"))

local_start_cols <- names(local_raw)[stringr::str_detect(tolower(names(local_raw)), "start|begin|first|oldest")]
local_end_cols <- names(local_raw)[stringr::str_detect(tolower(names(local_raw)), "end|last|latest|recent|newest")]
local_years_cols <- names(local_raw)[stringr::str_detect(tolower(names(local_raw)), "por.*year|years.*record|record.*years|period.*years|n_year|num_year|yrs")]
local_count_cols <- names(local_raw)[stringr::str_detect(tolower(names(local_raw)), "count|n_obs|num_obs|measurements|records")]

## Favor the popup fields we already know are present in recent caches.
local_start_cols <- unique(c("usgsswpop_start_date", local_start_cols))
local_end_cols <- unique(c("usgsswpop_end_date", local_end_cols))
local_count_cols <- unique(c("usgsswpop_count_nu", local_count_cols))

local_idx <- local_raw %>%
  mutate(
    site_no = pt_clean_site_no(.data[[local_site_col]]),
    local_name = if (!is.na(local_name_col)) pt_clean_chr(.data[[local_name_col]]) else NA_character_,
    local_status = if (!is.na(local_status_col)) pt_clean_chr(.data[[local_status_col]]) else NA_character_,
    local_status_lower = tolower(local_status),
    local_por_start_year_guess = pt_first_non_na_year(cur_data_all(), local_start_cols),
    local_por_end_year_guess = pt_first_non_na_year(cur_data_all(), local_end_cols),
    local_por_years_existing_guess = pt_first_non_na_numeric(cur_data_all(), local_years_cols),
    local_record_count_guess = pt_first_non_na_numeric(cur_data_all(), local_count_cols),
    local_por_years_guess = dplyr::case_when(
      !is.na(local_por_years_existing_guess) ~ as.numeric(local_por_years_existing_guess),
      !is.na(local_por_start_year_guess) & !is.na(local_por_end_year_guess) ~ as.numeric(local_por_end_year_guess - local_por_start_year_guess + 1L),
      TRUE ~ NA_real_
    ),
    local_is_active = dplyr::case_when(
      stringr::str_detect(local_status_lower, "active") & !stringr::str_detect(local_status_lower, "inactive") ~ TRUE,
      stringr::str_detect(local_status_lower, "inactive|discontinued") ~ FALSE,
      TRUE ~ NA
    )
  ) %>%
  filter(!is.na(site_no)) %>%
  group_by(site_no) %>%
  summarise(
    local_name = dplyr::first(stats::na.omit(local_name)),
    local_status = dplyr::first(stats::na.omit(local_status)),
    local_is_active = dplyr::first(stats::na.omit(local_is_active)),
    local_lat = dplyr::first(stats::na.omit(local_lat)),
    local_lon = dplyr::first(stats::na.omit(local_lon)),
    local_por_start_year_guess = suppressWarnings(min(local_por_start_year_guess, na.rm = TRUE)),
    local_por_end_year_guess = suppressWarnings(max(local_por_end_year_guess, na.rm = TRUE)),
    local_por_years_guess = suppressWarnings(max(local_por_years_guess, na.rm = TRUE)),
    local_record_count_guess = suppressWarnings(max(local_record_count_guess, na.rm = TRUE)),
    local_duplicate_rows = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(
    local_por_start_year_guess = ifelse(is.infinite(local_por_start_year_guess), NA_real_, local_por_start_year_guess),
    local_por_end_year_guess = ifelse(is.infinite(local_por_end_year_guess), NA_real_, local_por_end_year_guess),
    local_por_years_guess = ifelse(is.infinite(local_por_years_guess), NA_real_, local_por_years_guess),
    local_record_count_guess = ifelse(is.infinite(local_record_count_guess), NA_real_, local_record_count_guess)
  )

ops_idx <- ops_raw %>%
  mutate(
    site_no = pt_clean_site_no(.data[[ops_site_col]]),
    ops_name = if (!is.na(ops_name_col)) pt_clean_chr(.data[[ops_name_col]]) else NA_character_,
    ops_status = if (!is.na(ops_status_col)) pt_clean_chr(.data[[ops_status_col]]) else NA_character_,
    ops_latest_status = if (!is.na(ops_latest_status_col)) pt_clean_chr(.data[[ops_latest_status_col]]) else NA_character_,
    ops_q_cfs = if (!is.na(ops_q_cfs_col)) suppressWarnings(as.numeric(.data[[ops_q_cfs_col]])) else NA_real_,
    ops_q_obs_age_hours = if (!is.na(ops_q_age_col)) suppressWarnings(as.numeric(.data[[ops_q_age_col]])) else NA_real_,
    ops_stage_obs_age_hours = if (!is.na(ops_stage_age_col)) suppressWarnings(as.numeric(.data[[ops_stage_age_col]])) else NA_real_,
    ops_has_latest_iv_q_raw = if (!is.na(ops_has_q_col)) pt_boolish(.data[[ops_has_q_col]]) else NA,
    ops_has_latest_iv_stage_raw = if (!is.na(ops_has_stage_col)) pt_boolish(.data[[ops_has_stage_col]]) else NA,
    ops_has_latest_iv_q = dplyr::case_when(
      ops_has_latest_iv_q_raw %in% TRUE ~ TRUE,
      ops_has_latest_iv_q_raw %in% FALSE ~ FALSE,
      !is.na(ops_q_cfs) ~ TRUE,
      TRUE ~ FALSE
    ),
    ops_has_latest_iv_stage = dplyr::case_when(
      ops_has_latest_iv_stage_raw %in% TRUE ~ TRUE,
      ops_has_latest_iv_stage_raw %in% FALSE ~ FALSE,
      !is.na(ops_stage_obs_age_hours) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(!is.na(site_no)) %>%
  group_by(site_no) %>%
  summarise(
    ops_name = dplyr::first(stats::na.omit(ops_name)),
    ops_status = dplyr::first(stats::na.omit(ops_status)),
    ops_latest_status = dplyr::first(stats::na.omit(ops_latest_status)),
    ops_lat = dplyr::first(stats::na.omit(ops_lat)),
    ops_lon = dplyr::first(stats::na.omit(ops_lon)),
    ops_has_latest_iv_q = any(ops_has_latest_iv_q %in% TRUE, na.rm = TRUE),
    ops_has_latest_iv_stage = any(ops_has_latest_iv_stage %in% TRUE, na.rm = TRUE),
    ops_q_obs_age_hours = suppressWarnings(min(ops_q_obs_age_hours, na.rm = TRUE)),
    ops_stage_obs_age_hours = suppressWarnings(min(ops_stage_obs_age_hours, na.rm = TRUE)),
    ops_q_cfs = dplyr::first(stats::na.omit(ops_q_cfs)),
    ops_duplicate_rows = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(
    ops_q_obs_age_hours = ifelse(is.infinite(ops_q_obs_age_hours), NA_real_, ops_q_obs_age_hours),
    ops_stage_obs_age_hours = ifelse(is.infinite(ops_stage_obs_age_hours), NA_real_, ops_stage_obs_age_hours),
    ops_current_discharge_class = dplyr::case_when(
      ops_has_latest_iv_q %in% TRUE & !is.na(ops_q_obs_age_hours) & ops_q_obs_age_hours <= 6 ~ "latest_discharge_<=6h",
      ops_has_latest_iv_q %in% TRUE & !is.na(ops_q_obs_age_hours) & ops_q_obs_age_hours <= 24 ~ "latest_discharge_6_24h",
      ops_has_latest_iv_q %in% TRUE & !is.na(ops_q_obs_age_hours) & ops_q_obs_age_hours <= 72 ~ "latest_discharge_24_72h",
      ops_has_latest_iv_q %in% TRUE ~ "latest_discharge_age_unknown_or_gt72h",
      tolower(ops_latest_status) == "no_recent_iv_discharge" ~ "no_recent_iv_discharge",
      TRUE ~ "no_latest_discharge"
    ),
    ops_current_data_age_hours = pmin(
      ifelse(ops_has_latest_iv_q %in% TRUE, ops_q_obs_age_hours, Inf),
      ifelse(ops_has_latest_iv_stage %in% TRUE, ops_stage_obs_age_hours, Inf),
      na.rm = TRUE
    ),
    ops_current_data_age_hours = ifelse(is.infinite(ops_current_data_age_hours), NA_real_, ops_current_data_age_hours),
    ops_current_data_class = dplyr::case_when(
      (ops_has_latest_iv_q %in% TRUE | ops_has_latest_iv_stage %in% TRUE) & !is.na(ops_current_data_age_hours) & ops_current_data_age_hours <= 6 ~ "latest_data_<=6h",
      (ops_has_latest_iv_q %in% TRUE | ops_has_latest_iv_stage %in% TRUE) & !is.na(ops_current_data_age_hours) & ops_current_data_age_hours <= 24 ~ "latest_data_6_24h",
      (ops_has_latest_iv_q %in% TRUE | ops_has_latest_iv_stage %in% TRUE) & !is.na(ops_current_data_age_hours) & ops_current_data_age_hours <= 72 ~ "latest_data_24_72h",
      (ops_has_latest_iv_q %in% TRUE | ops_has_latest_iv_stage %in% TRUE) ~ "latest_data_age_unknown_or_gt72h",
      tolower(ops_latest_status) == "no_recent_iv_discharge" ~ "no_recent_iv_data",
      TRUE ~ "no_latest_data"
    )
  )

local_ids <- unique(local_idx$site_no)
ops_ids <- unique(ops_idx$site_no)

sync_matrix <- full_join(local_idx, ops_idx, by = "site_no") %>%
  mutate(
    in_local_catalog = site_no %in% local_ids,
    in_ops_streamflow_source = site_no %in% ops_ids,
    sync_class = dplyr::case_when(
      in_local_catalog & in_ops_streamflow_source ~ "local_and_ops_streamflow_source",
      in_local_catalog & !in_ops_streamflow_source ~ "local_catalog_only",
      !in_local_catalog & in_ops_streamflow_source ~ "ops_streamflow_source_only",
      TRUE ~ "unknown"
    ),
    ops_streamflow_source_source_url = attr(ops_raw, "source_url"),
    ops_streamflow_source_audit_timestamp = RUN_TS
  ) %>%
  arrange(desc(in_ops_streamflow_source), desc(in_local_catalog), site_no)

local_only <- sync_matrix %>%
  filter(in_local_catalog, !in_ops_streamflow_source) %>%
  arrange(site_no)

ops_only <- sync_matrix %>%
  filter(!in_local_catalog, in_ops_streamflow_source) %>%
  arrange(site_no)

current_value_bins <- sync_matrix %>%
  filter(in_local_catalog) %>%
  count(local_is_active, ops_current_data_class, ops_current_discharge_class, name = "sites") %>%
  mutate(run_timestamp = RUN_TS, .before = 1) %>%
  arrange(desc(sites), local_is_active, ops_current_data_class, ops_current_discharge_class)

summary <- tibble::tribble(
  ~run_timestamp, ~metric, ~value,
  RUN_TS, "local_usgs_streamgage_catalog_ids", length(local_ids),
  RUN_TS, "ops_streamflow_source_station_index_ids", length(ops_ids),
  RUN_TS, "local_overlap_ops_streamflow_source_ids", length(intersect(local_ids, ops_ids)),
  RUN_TS, "local_only_vs_ops_streamflow_source_ids", length(setdiff(local_ids, ops_ids)),
  RUN_TS, "ops_streamflow_source_only_vs_local_ids", length(setdiff(ops_ids, local_ids)),
  RUN_TS, "local_active_ids", sum(local_idx$local_is_active %in% TRUE, na.rm = TRUE),
  RUN_TS, "local_inactive_ids", sum(local_idx$local_is_active %in% FALSE, na.rm = TRUE),
  RUN_TS, "local_unknown_activity_ids", sum(is.na(local_idx$local_is_active)),
  RUN_TS, "local_active_in_ops_streamflow_source_ids", sum(local_idx$local_is_active %in% TRUE & local_idx$site_no %in% ops_ids, na.rm = TRUE),
  RUN_TS, "local_inactive_in_ops_streamflow_source_ids", sum(local_idx$local_is_active %in% FALSE & local_idx$site_no %in% ops_ids, na.rm = TRUE),
  RUN_TS, "local_with_por_start_year_guess", sum(!is.na(local_idx$local_por_start_year_guess)),
  RUN_TS, "local_with_por_end_year_guess", sum(!is.na(local_idx$local_por_end_year_guess)),
  RUN_TS, "local_with_por_years_guess", sum(!is.na(local_idx$local_por_years_guess)),
  RUN_TS, "local_with_record_count_guess", sum(!is.na(local_idx$local_record_count_guess)),
  RUN_TS, "ops_with_latest_iv_discharge", sum(ops_idx$ops_has_latest_iv_q %in% TRUE, na.rm = TRUE),
  RUN_TS, "ops_with_latest_iv_stage", sum(ops_idx$ops_has_latest_iv_stage %in% TRUE, na.rm = TRUE),
  RUN_TS, "ops_q_age_le_6h", sum(ops_idx$ops_has_latest_iv_q %in% TRUE & ops_idx$ops_q_obs_age_hours <= 6, na.rm = TRUE),
  RUN_TS, "ops_q_age_le_24h", sum(ops_idx$ops_has_latest_iv_q %in% TRUE & ops_idx$ops_q_obs_age_hours <= 24, na.rm = TRUE),
  RUN_TS, "ops_q_age_le_72h", sum(ops_idx$ops_has_latest_iv_q %in% TRUE & ops_idx$ops_q_obs_age_hours <= 72, na.rm = TRUE),
  RUN_TS, "ops_no_recent_iv_discharge", sum(ops_idx$ops_current_discharge_class == "no_recent_iv_discharge", na.rm = TRUE),
  RUN_TS, "ops_with_latest_iv_data", sum(ops_idx$ops_has_latest_iv_q %in% TRUE | ops_idx$ops_has_latest_iv_stage %in% TRUE, na.rm = TRUE),
  RUN_TS, "ops_data_age_le_6h", sum((ops_idx$ops_has_latest_iv_q %in% TRUE | ops_idx$ops_has_latest_iv_stage %in% TRUE) & ops_idx$ops_current_data_age_hours <= 6, na.rm = TRUE),
  RUN_TS, "ops_data_age_le_24h", sum((ops_idx$ops_has_latest_iv_q %in% TRUE | ops_idx$ops_has_latest_iv_stage %in% TRUE) & ops_idx$ops_current_data_age_hours <= 24, na.rm = TRUE),
  RUN_TS, "ops_data_age_le_72h", sum((ops_idx$ops_has_latest_iv_q %in% TRUE | ops_idx$ops_has_latest_iv_stage %in% TRUE) & ops_idx$ops_current_data_age_hours <= 72, na.rm = TRUE),
  RUN_TS, "local_active_with_latest_iv_discharge", sum(sync_matrix$local_is_active %in% TRUE & sync_matrix$ops_has_latest_iv_q %in% TRUE, na.rm = TRUE),
  RUN_TS, "local_inactive_with_latest_iv_discharge", sum(sync_matrix$local_is_active %in% FALSE & sync_matrix$ops_has_latest_iv_q %in% TRUE, na.rm = TRUE),
  RUN_TS, "local_active_with_latest_iv_data", sum(sync_matrix$local_is_active %in% TRUE & (sync_matrix$ops_has_latest_iv_q %in% TRUE | sync_matrix$ops_has_latest_iv_stage %in% TRUE), na.rm = TRUE),
  RUN_TS, "local_inactive_with_latest_iv_data", sum(sync_matrix$local_is_active %in% FALSE & (sync_matrix$ops_has_latest_iv_q %in% TRUE | sync_matrix$ops_has_latest_iv_stage %in% TRUE), na.rm = TRUE)
)

column_inventory <- pt_column_inventory(local_raw, ops_raw)

por_field_candidates <- column_inventory %>%
  filter(likely_role %in% c("period-of-record / recency candidate", "status/activity", "BLM proximity candidate")) %>%
  arrange(source, likely_role, column)

pt_write_latest_and_ts <- function(x, stem) {
  latest <- file.path(pt_qa_dir, paste0(stem, "_latest.csv"))
  ts <- file.path(pt_qa_dir, paste0(stem, "_", RUN_TS, ".csv"))
  readr::write_csv(x, latest)
  readr::write_csv(x, ts)
  invisible(latest)
}

summary_path <- pt_write_latest_and_ts(summary, "usgs_streamgage_catalog_vs_ops_sync_summary")
matrix_path <- pt_write_latest_and_ts(sync_matrix, "usgs_streamgage_catalog_vs_ops_sync_matrix")
local_only_path <- pt_write_latest_and_ts(local_only, "usgs_streamgage_catalog_only")
ops_only_path <- pt_write_latest_and_ts(ops_only, "usgs_streamgage_ops_only")
column_path <- pt_write_latest_and_ts(column_inventory, "usgs_streamgage_sync_column_inventory")
por_path <- pt_write_latest_and_ts(por_field_candidates, "usgs_streamgage_sync_por_field_candidates")
current_bins_path <- pt_write_latest_and_ts(current_value_bins, "usgs_streamgage_current_value_bins")

message("USGS streamgage Local-vs-Ops current-data audit complete.")
message("  Summary:       ", summary_path)
message("  Sync matrix:   ", matrix_path)
message("  Local-only:    ", local_only_path)
message("  Ops-only:      ", ops_only_path)
message("  Column scan:   ", column_path)
message("  POR candidates:", por_path)
message("  Current-discharge bins:  ", current_bins_path)
message("")
message("Site ID columns used:")
message("  Local: ", local_site_col)
message("  Ops:   ", ops_site_col)
message("")
message("Likely Local POR/filter fields used:")
message("  start columns: ", paste(local_start_cols[local_start_cols %in% names(local_raw)], collapse = ", "))
message("  end columns:   ", paste(local_end_cols[local_end_cols %in% names(local_raw)], collapse = ", "))
message("  years columns: ", paste(local_years_cols[local_years_cols %in% names(local_raw)], collapse = ", "))
message("  count columns: ", paste(local_count_cols[local_count_cols %in% names(local_raw)], collapse = ", "))
message("")
message("Summary:")
print(summary, n = Inf)
message("")
message("Station-index overlap classes:")
print(sync_matrix %>% count(sync_class, name = "sites") %>% arrange(desc(sites)), n = Inf)
message("")
message("Current-discharge classes:")
print(current_value_bins, n = Inf)
