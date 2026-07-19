# ==== 31_build_usgs_groundwater_history_summary.r ============================
##
## PURPOSE:
##   Build a reusable, local/preprocessed USGS groundwater history summary for
##   the BRIM Ops Live groundwater layer.
##
## DESIGN:
##   This is intentionally NOT a daily GitHub Action job.  Groundwater history is
##   slower-changing, heavier to retrieve, and best handled as a structured local
##   preprocessing step.  The daily GitHub feed should remain latest-only and can
##   optionally join the compact history-summary CSV created here.
##
## INPUT:
##   brim-live-data-feeds/data/input/usgs_groundwater_latest_index_ca.csv
##
## MAIN OUTPUT FOR LIVE FEED ENRICHMENT:
##   brim-live-data-feeds/data/input/usgs_groundwater_history_summary_ca.csv
##
## LOCAL CACHE / QA OUTPUTS:
##   04_processed_data/rds/usgs_groundwater_history_field_measurements_72019_ca.rds
##   04_processed_data/rds/usgs_groundwater_history_water_year_summary_ca.rds
##   04_processed_data/qa/usgs_groundwater_history_water_year_summary_ca.csv
##   04_processed_data/qa/usgs_groundwater_history_site_summary_ca.csv
##   04_processed_data/qa/usgs_groundwater_history_run_summary.csv
##
## UPDATE / RENEWAL STRATEGY:
##   - The current live candidate index controls which wells are summarized for
##     BRIM.  When an old well is reactivated or a new well becomes relevant, the
##     upstream candidate-index export should be rerun and committed to the live
##     feed repo.
##   - This script is incremental.  It reuses the cached historical measurement
##     table, fully backfills candidate wells not yet in the cache, and refreshes
##     a recent tail window for all current candidate wells.  This means newly
##     added wells are backfilled without redownloading the entire history for
##     every existing well.
##   - The raw history cache is allowed to retain wells that are no longer in the
##     current candidate CSV.  Current summary outputs are filtered to the current
##     candidate list.
##
## METRICS:
##   - annual and water-year mean depth to water, ft bgs
##   - compact water-year mean JSON for future inline SVG popup plots
##   - latest depth-to-water percentile relative to prior POR measurements
##   - latest depth-to-water percentile relative to prior measurements within a
##     water-day-of-year moving window, with Feb. 29 removed from the 365-day
##     index
##
## IMPORTANT HYDROLOGY NOTE:
##   Larger depth-to-water values mean deeper groundwater. Percentiles here are
##   therefore described as "deeper percentiles." A value near 90 means the
##   latest measurement is deeper than most prior measurements, not wetter.
## ============================================================================

# ---- 1. Packages ------------------------------------------------------------

required_pkgs <- c(
  "dataRetrieval", "dplyr", "readr", "lubridate", "jsonlite", "tibble", "curl"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "))
}

required_dataretrieval_funs <- c("read_waterdata_field_measurements")

missing_dataretrieval_funs <- required_dataretrieval_funs[
  !vapply(required_dataretrieval_funs, exists, logical(1), where = asNamespace("dataRetrieval"), inherits = FALSE)
]

if (length(missing_dataretrieval_funs) > 0) {
  stop(
    "Installed dataRetrieval package is too old. Missing function(s): ",
    paste(missing_dataretrieval_funs, collapse = ", "),
    "\nUpdate dataRetrieval from CRAN, then rerun."
  )
}

options(
  cli.progress_show_after = Inf,
  cli.progress_handlers = "none"
)

suppressPackageStartupMessages({
  library(dataRetrieval)
  library(dplyr)
  library(readr)
  library(lubridate)
  library(jsonlite)
  library(tibble)
  library(curl)
})

# ---- 2. Paths and switches --------------------------------------------------

candidate_csv <- Sys.getenv(
  "USGS_GW_HISTORY_CANDIDATE_CSV",
  unset = "brim-live-data-feeds/data/input/usgs_groundwater_latest_index_ca.csv"
)

raw_history_rds <- Sys.getenv(
  "USGS_GW_HISTORY_RAW_RDS",
  unset = "04_processed_data/rds/usgs_groundwater_history_field_measurements_72019_ca.rds"
)

wy_summary_rds <- Sys.getenv(
  "USGS_GW_HISTORY_WY_RDS",
  unset = "04_processed_data/rds/usgs_groundwater_history_water_year_summary_ca.rds"
)

out_history_summary_csv <- Sys.getenv(
  "USGS_GW_HISTORY_SUMMARY_CSV",
  unset = "brim-live-data-feeds/data/input/usgs_groundwater_history_summary_ca.csv"
)

qa_dir <- Sys.getenv(
  "USGS_GW_HISTORY_QA_DIR",
  unset = "04_processed_data/qa"
)

gw_parameter_code <- Sys.getenv(
  "USGS_GW_PARAMETER_CODE",
  unset = "72019"
)

history_start_date <- as.Date(Sys.getenv(
  "USGS_GW_HISTORY_START_DATE",
  unset = "1900-01-01"
))
if (is.na(history_start_date)) history_start_date <- as.Date("1900-01-01")

history_end_date <- as.Date(Sys.getenv(
  "USGS_GW_HISTORY_END_DATE",
  unset = as.character(Sys.Date() + 1)
))
if (is.na(history_end_date)) history_end_date <- Sys.Date() + 1

refresh_recent_days <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_HISTORY_REFRESH_RECENT_DAYS",
  unset = "540"
)))
if (is.na(refresh_recent_days) || refresh_recent_days < 30) refresh_recent_days <- 540L

chunk_size <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_HISTORY_CHUNK_SIZE",
  unset = "40"
)))
if (is.na(chunk_size) || chunk_size < 1) chunk_size <- 40L

request_pause_sec <- suppressWarnings(as.numeric(Sys.getenv(
  "USGS_GW_HISTORY_REQUEST_PAUSE_SEC",
  unset = "0.25"
)))
if (is.na(request_pause_sec) || request_pause_sec < 0) request_pause_sec <- 0.25

force_full_refresh <- tolower(Sys.getenv(
  "USGS_GW_HISTORY_FORCE_FULL_REFRESH",
  unset = "false"
)) %in% c("true", "t", "1", "yes", "y")

seasonal_window_days <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_HISTORY_SEASONAL_WINDOW_DAYS",
  unset = "45"
)))
if (is.na(seasonal_window_days) || seasonal_window_days < 1) seasonal_window_days <- 45L
seasonal_window_days <- min(seasonal_window_days, 182L)

min_por_n <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_HISTORY_MIN_POR_N",
  unset = "5"
)))
if (is.na(min_por_n) || min_por_n < 1) min_por_n <- 5L

min_seasonal_n <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_HISTORY_MIN_SEASONAL_N",
  unset = "10"
)))
if (is.na(min_seasonal_n) || min_seasonal_n < 1) min_seasonal_n <- 10L

min_trend_years <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_HISTORY_MIN_TREND_YEARS",
  unset = "10"
)))
if (is.na(min_trend_years) || min_trend_years < 3) min_trend_years <- 10L

max_plot_years <- suppressWarnings(as.integer(Sys.getenv(
  "USGS_GW_HISTORY_MAX_PLOT_YEARS",
  unset = "35"
)))
if (is.na(max_plot_years) || max_plot_years < 5) max_plot_years <- 35L

run_time <- Sys.time()
run_time_utc <- format(lubridate::with_tz(run_time, "UTC"), "%Y-%m-%dT%H:%M:%SZ")

# ---- 3. Small helpers -------------------------------------------------------

pt_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_site_no <- function(x) {
  x <- pt_chr(x)
  x <- gsub("\\.0$", "", x)
  x <- gsub("[^0-9]", "", x)
  x[nchar(x) == 0] <- NA_character_
  x
}

pt_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

pt_chunks <- function(x, n) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & x != ""]
  split(x, ceiling(seq_along(x) / n))
}

pt_ensure_cols <- function(df, cols) {
  for (nm in cols) {
    if (!nm %in% names(df)) df[[nm]] <- NA_character_
  }
  df
}

pt_usgs_monitoring_location_id <- function(site_ids) {
  site_ids <- pt_site_no(site_ids)
  out <- ifelse(!is.na(site_ids) & site_ids != "", paste0("USGS-", site_ids), NA_character_)
  out[!is.na(out)]
}

pt_waterdata_time_interval <- function(start_date, end_date) {
  paste0(as.character(start_date), "/", as.character(end_date))
}

pt_has_cols <- function(df, cols) {
  all(cols %in% names(df))
}

pt_compact_code <- function(qualifier, approval_status) {
  qualifier <- pt_chr(qualifier)
  approval_status <- pt_chr(approval_status)

  dplyr::case_when(
    !is.na(qualifier) & !is.na(approval_status) ~ paste0(qualifier, "; ", approval_status),
    !is.na(qualifier) ~ qualifier,
    !is.na(approval_status) ~ approval_status,
    TRUE ~ NA_character_
  )
}

pt_water_year <- function(date) {
  date <- as.Date(date)
  yr <- lubridate::year(date)
  mo <- lubridate::month(date)
  as.integer(ifelse(mo >= 10, yr + 1L, yr))
}

pt_water_day_365 <- function(date) {
  date <- as.Date(date)
  out <- rep(NA_integer_, length(date))
  ok <- !is.na(date)
  if (!any(ok)) return(out)

  d <- date[ok]
  y <- lubridate::year(d)
  m <- lubridate::month(d)

  wy_start_year <- ifelse(m >= 10, y, y - 1L)
  wy_start <- as.Date(paste0(wy_start_year, "-10-01"))
  raw_day <- as.integer(d - wy_start) + 1L

  feb29 <- as.Date(rep(NA_character_, length(d)))
  leap_yr <- wy_start_year + 1L
  leap_idx <- lubridate::leap_year(leap_yr)
  feb29[leap_idx] <- as.Date(paste0(leap_yr[leap_idx], "-02-29"))

  is_feb29 <- !is.na(feb29) & d == feb29
  after_feb29 <- !is.na(feb29) & d > feb29

  day365 <- raw_day - ifelse(after_feb29, 1L, 0L)
  day365[is_feb29] <- NA_integer_

  out[ok] <- day365
  out
}

pt_circular_day_distance <- function(a, b, period = 365) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  abs(((a - b + period / 2) %% period) - period / 2)
}

pt_deeper_percentile <- function(latest_value, baseline_values) {
  latest_value <- pt_num(latest_value)
  baseline_values <- pt_num(baseline_values)
  baseline_values <- baseline_values[is.finite(baseline_values)]
  if (!is.finite(latest_value) || length(baseline_values) == 0) return(NA_real_)
  round(100 * mean(baseline_values <= latest_value), 1)
}

pt_safe_lm_slope <- function(x, y, min_n) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(unique(x)) < min_n || length(y) < min_n) return(NA_real_)

  fit <- tryCatch(stats::lm(y ~ x), error = function(e) NULL)
  if (is.null(fit)) return(NA_real_)
  slope <- unname(stats::coef(fit)[[2]])
  if (!is.finite(slope)) NA_real_ else round(slope, 4)
}

pt_wy_json <- function(wy, mean_ft_bgs, n_measurements) {
  if (length(wy) == 0) return(NA_character_)

  items <- lapply(seq_along(wy), function(i) {
    list(
      wy = as.integer(wy[[i]]),
      mean_ft_bgs = round(as.numeric(mean_ft_bgs[[i]]), 2),
      n = as.integer(n_measurements[[i]])
    )
  })

  as.character(jsonlite::toJSON(items, auto_unbox = TRUE, null = "null", na = "null", digits = 8))
}

pt_empty_history <- function() {
  tibble::tibble(
    site_no = character(),
    monitoring_location_id = character(),
    parameter_code = character(),
    obs_datetime_utc = as.POSIXct(character(), tz = "UTC"),
    obs_date = as.Date(character()),
    wl_ft_bgs = numeric(),
    wl_code = character(),
    unit_of_measure = character(),
    observing_procedure = character(),
    vertical_datum = character(),
    measuring_agency = character(),
    field_visit_id = character(),
    last_modified_utc = as.POSIXct(character(), tz = "UTC")
  )
}

# ---- 4. USGS field-measurement retrieval helpers ---------------------------

pt_ogc_query_url <- function(collection, params) {
  base <- paste0(
    "https://api.waterdata.usgs.gov/ogcapi/v0/collections/",
    collection,
    "/items"
  )

  params <- params[!vapply(params, function(x) is.null(x) || length(x) == 0, logical(1))]
  params <- lapply(params, function(x) paste(as.character(x), collapse = ","))

  query <- paste(
    paste0(
      names(params),
      "=",
      vapply(params, utils::URLencode, character(1), reserved = TRUE)
    ),
    collapse = "&"
  )

  paste0(base, "?", query)
}

pt_fetch_ogc_properties <- function(url, label = url) {
  message("Requesting direct Water Data API fallback: ", label)

  h <- curl::new_handle(
    timeout = 90,
    connecttimeout = 20,
    useragent = "BRIM groundwater history preprocessor"
  )

  api_key <- Sys.getenv("API_USGS_PAT")
  if (nzchar(api_key)) {
    curl::handle_setheaders(h, "X-Api-Key" = api_key)
  }

  resp <- tryCatch(
    curl::curl_fetch_memory(url, handle = h),
    error = function(e) e
  )

  if (inherits(resp, "error")) {
    warning("Direct Water Data API fallback failed for ", label, ": ", conditionMessage(resp))
    return(tibble::tibble())
  }

  if (!is.null(resp$status_code) && resp$status_code >= 400) {
    warning("Direct Water Data API fallback returned HTTP ", resp$status_code, " for ", label)
    return(tibble::tibble())
  }

  x <- tryCatch(
    jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = TRUE),
    error = function(e) e
  )

  if (inherits(x, "error")) {
    warning("Could not parse direct Water Data API fallback JSON for ", label, ": ", conditionMessage(x))
    return(tibble::tibble())
  }

  if (!"features" %in% names(x) || is.null(x$features) || length(x$features) == 0) {
    return(tibble::tibble())
  }

  props <- NULL

  if (is.data.frame(x$features) && "properties" %in% names(x$features)) {
    props <- x$features$properties
  } else if (is.list(x$features) && !is.null(x$features$properties)) {
    props <- x$features$properties
  }

  if (is.null(props)) return(tibble::tibble())

  tibble::as_tibble(props)
}

pt_fetch_field_measurements_chunk_direct <- function(site_ids, start_date, end_date, label) {
  url <- pt_ogc_query_url(
    collection = "field-measurements",
    params = list(
      f = "json",
      lang = "en-US",
      skipGeometry = "TRUE",
      properties = paste(
        c(
          "monitoring_location_id",
          "parameter_code",
          "time",
          "value",
          "unit_of_measure",
          "qualifier",
          "approval_status",
          "observing_procedure",
          "vertical_datum",
          "measuring_agency",
          "field_visit_id",
          "last_modified"
        ),
        collapse = ","
      ),
      monitoring_location_id = paste(pt_usgs_monitoring_location_id(site_ids), collapse = ","),
      parameter_code = gw_parameter_code,
      time = pt_waterdata_time_interval(start_date, end_date),
      limit = "50000"
    )
  )

  pt_fetch_ogc_properties(url, label = paste0("groundwater history chunk ", label))
}

pt_fetch_field_measurements_chunk <- function(site_ids, start_date, end_date, label) {
  tryCatch({
    dataRetrieval::read_waterdata_field_measurements(
      monitoring_location_id = pt_usgs_monitoring_location_id(site_ids),
      parameter_code = gw_parameter_code,
      time = pt_waterdata_time_interval(start_date, end_date),
      properties = c(
        "monitoring_location_id",
        "parameter_code",
        "time",
        "value",
        "unit_of_measure",
        "qualifier",
        "approval_status",
        "observing_procedure",
        "vertical_datum",
        "measuring_agency",
        "field_visit_id",
        "last_modified"
      ),
      skipGeometry = TRUE
    )
  }, error = function(e) {
    warning(
      "USGS groundwater history chunk failed through dataRetrieval for ",
      label,
      ": ",
      conditionMessage(e),
      ". Trying direct OGC API fallback."
    )
    pt_fetch_field_measurements_chunk_direct(
      site_ids = site_ids,
      start_date = start_date,
      end_date = end_date,
      label = label
    )
  })
}

pt_normalize_history <- function(raw) {
  if (is.null(raw) || nrow(raw) == 0) return(pt_empty_history())

  needed <- c("monitoring_location_id", "parameter_code", "time", "value")
  if (!pt_has_cols(raw, needed)) {
    warning(
      "USGS field-measurements output did not include expected columns: ",
      paste(setdiff(needed, names(raw)), collapse = ", "),
      ". Skipping this chunk."
    )
    return(pt_empty_history())
  }

  raw |>
    dplyr::mutate(
      monitoring_location_id = pt_chr(.data$monitoring_location_id),
      site_no = pt_site_no(.data$monitoring_location_id),
      parameter_code = as.character(.data$parameter_code),
      obs_datetime_utc = suppressWarnings(lubridate::as_datetime(.data$time, tz = "UTC")),
      obs_date = as.Date(.data$obs_datetime_utc),
      wl_ft_bgs = pt_num(.data$value),
      wl_code = pt_compact_code(
        if ("qualifier" %in% names(raw)) .data$qualifier else NA_character_,
        if ("approval_status" %in% names(raw)) .data$approval_status else NA_character_
      ),
      unit_of_measure = if ("unit_of_measure" %in% names(raw)) pt_chr(.data$unit_of_measure) else NA_character_,
      observing_procedure = if ("observing_procedure" %in% names(raw)) pt_chr(.data$observing_procedure) else NA_character_,
      vertical_datum = if ("vertical_datum" %in% names(raw)) pt_chr(.data$vertical_datum) else NA_character_,
      measuring_agency = if ("measuring_agency" %in% names(raw)) pt_chr(.data$measuring_agency) else NA_character_,
      field_visit_id = if ("field_visit_id" %in% names(raw)) pt_chr(.data$field_visit_id) else NA_character_,
      last_modified_raw = if ("last_modified" %in% names(raw)) pt_chr(.data$last_modified) else NA_character_,
      last_modified_utc = suppressWarnings(lubridate::as_datetime(.data$last_modified_raw, tz = "UTC"))
    ) |>
    dplyr::filter(
      !is.na(.data$site_no),
      .data$parameter_code == gw_parameter_code,
      !is.na(.data$obs_datetime_utc),
      !is.na(.data$wl_ft_bgs)
    ) |>
    dplyr::transmute(
      site_no = .data$site_no,
      monitoring_location_id = .data$monitoring_location_id,
      parameter_code = .data$parameter_code,
      obs_datetime_utc = .data$obs_datetime_utc,
      obs_date = .data$obs_date,
      wl_ft_bgs = .data$wl_ft_bgs,
      wl_code = .data$wl_code,
      unit_of_measure = .data$unit_of_measure,
      observing_procedure = .data$observing_procedure,
      vertical_datum = .data$vertical_datum,
      measuring_agency = .data$measuring_agency,
      field_visit_id = .data$field_visit_id,
      last_modified_utc = .data$last_modified_utc
    )
}

pt_fetch_history <- function(site_ids, start_date, end_date, label_prefix = "history") {
  site_ids <- unique(pt_site_no(site_ids))
  site_ids <- site_ids[!is.na(site_ids)]

  if (length(site_ids) == 0) return(pt_empty_history())

  chunks <- pt_chunks(site_ids, chunk_size)
  message("Fetching USGS groundwater history for ", length(site_ids), " site(s) in ", length(chunks), " chunk(s).")
  message("USGS field-measurements time interval: ", pt_waterdata_time_interval(start_date, end_date))

  out <- vector("list", length(chunks))

  for (i in seq_along(chunks)) {
    message("  ", label_prefix, " chunk ", i, " of ", length(chunks), " | sites: ", length(chunks[[i]]))
    raw <- pt_fetch_field_measurements_chunk(
      site_ids = chunks[[i]],
      start_date = start_date,
      end_date = end_date,
      label = paste0(label_prefix, " ", i, "/", length(chunks))
    )
    out[[i]] <- pt_normalize_history(raw)
    if (i < length(chunks) && request_pause_sec > 0) Sys.sleep(request_pause_sec)
  }

  dplyr::bind_rows(out)
}

pt_deduplicate_history <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(pt_empty_history())

  x |>
    dplyr::mutate(
      site_no = pt_site_no(.data$site_no),
      obs_datetime_utc = suppressWarnings(lubridate::as_datetime(.data$obs_datetime_utc, tz = "UTC")),
      obs_date = as.Date(.data$obs_datetime_utc),
      wl_ft_bgs = pt_num(.data$wl_ft_bgs)
    ) |>
    dplyr::filter(
      !is.na(.data$site_no),
      !is.na(.data$obs_datetime_utc),
      !is.na(.data$wl_ft_bgs)
    ) |>
    dplyr::arrange(.data$site_no, .data$obs_datetime_utc, .data$field_visit_id) |>
    dplyr::distinct(
      .data$site_no,
      .data$obs_datetime_utc,
      .data$wl_ft_bgs,
      .data$field_visit_id,
      .keep_all = TRUE
    )
}

# ---- 5. Read current candidate set -----------------------------------------

if (!file.exists(candidate_csv)) {
  stop(
    "Groundwater candidate CSV not found: ", candidate_csv,
    "\nRun RF027a / 30_export_usgs_groundwater_live_inputs.R first."
  )
}

candidate_raw <- readr::read_csv(
  candidate_csv,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

candidate_raw <- pt_ensure_cols(
  candidate_raw,
  c("site_no", "station_nm", "latitude", "longitude", "status")
)

candidates <- candidate_raw |>
  dplyr::transmute(
    site_no = pt_site_no(.data$site_no),
    station_nm = pt_chr(.data$station_nm),
    latitude = pt_num(.data$latitude),
    longitude = pt_num(.data$longitude),
    status = pt_chr(.data$status)
  ) |>
  dplyr::filter(!is.na(.data$site_no)) |>
  dplyr::distinct(.data$site_no, .keep_all = TRUE) |>
  dplyr::arrange(.data$site_no)

if (nrow(candidates) == 0) {
  stop("No valid candidate groundwater sites found in: ", candidate_csv)
}

message("Current groundwater candidate sites: ", nrow(candidates))

# ---- 6. Read existing raw history cache and decide retrieval plan ------------

cached_history <- pt_empty_history()

if (file.exists(raw_history_rds) && !force_full_refresh) {
  cached_history <- tryCatch(readRDS(raw_history_rds), error = function(e) {
    warning("Could not read existing groundwater history cache: ", conditionMessage(e))
    pt_empty_history()
  })
  cached_history <- pt_deduplicate_history(cached_history)
  message("Existing groundwater history cache rows: ", nrow(cached_history))
  message("Existing groundwater history cache sites: ", length(unique(cached_history$site_no)))
} else if (force_full_refresh) {
  message("USGS_GW_HISTORY_FORCE_FULL_REFRESH is TRUE; ignoring existing cache.")
} else {
  message("No existing groundwater history cache found. First run will backfill current candidates.")
}

cached_current_sites <- intersect(unique(cached_history$site_no), candidates$site_no)
missing_full_sites <- setdiff(candidates$site_no, cached_current_sites)

recent_start_date <- max(history_start_date, history_end_date - refresh_recent_days)

message("Candidate sites already represented in cache: ", length(cached_current_sites))
message("Candidate sites needing full history backfill: ", length(missing_full_sites))
message("Recent-tail refresh start date: ", as.character(recent_start_date))

# ---- 7. Retrieve missing full histories and recent tail ---------------------

full_fetch_sites <- if (force_full_refresh || nrow(cached_history) == 0) candidates$site_no else missing_full_sites

full_history_new <- pt_empty_history()
recent_history_new <- pt_empty_history()

if (length(full_fetch_sites) > 0) {
  full_history_new <- pt_fetch_history(
    site_ids = full_fetch_sites,
    start_date = history_start_date,
    end_date = history_end_date,
    label_prefix = "full-history"
  )
} else {
  message("No full-history backfill needed for current candidates.")
}

if (!(force_full_refresh || nrow(cached_history) == 0)) {
  recent_history_new <- pt_fetch_history(
    site_ids = candidates$site_no,
    start_date = recent_start_date,
    end_date = history_end_date,
    label_prefix = "recent-refresh"
  )
} else {
  message("Skipping separate recent refresh because full-history fetch already covered current candidates.")
}

history_all <- dplyr::bind_rows(
  cached_history,
  full_history_new,
  recent_history_new
) |>
  pt_deduplicate_history() |>
  dplyr::arrange(.data$site_no, .data$obs_datetime_utc)

message("Combined groundwater history cache rows: ", nrow(history_all))
message("Combined groundwater history cache sites: ", length(unique(history_all$site_no)))

# Save raw-ish normalized cache before current-candidate filtering so dropped
# sites can be retained for future reactivation without a full redownload.
dir.create(dirname(raw_history_rds), recursive = TRUE, showWarnings = FALSE)
saveRDS(history_all, raw_history_rds)
message("Saved groundwater history cache: ", raw_history_rds)

history_current <- history_all |>
  dplyr::filter(.data$site_no %in% candidates$site_no) |>
  dplyr::mutate(
    calendar_year = lubridate::year(.data$obs_date),
    water_year = pt_water_year(.data$obs_date),
    water_day_365 = pt_water_day_365(.data$obs_date)
  )

if (nrow(history_current) == 0) {
  stop("No historical groundwater rows found for current candidates after retrieval/cache merge.")
}

# ---- 8. Build annual and water-year summaries -------------------------------

calendar_summary <- history_current |>
  dplyr::group_by(.data$site_no, .data$calendar_year) |>
  dplyr::summarise(
    cal_mean_wl_ft_bgs = mean(.data$wl_ft_bgs, na.rm = TRUE),
    cal_median_wl_ft_bgs = stats::median(.data$wl_ft_bgs, na.rm = TRUE),
    cal_min_wl_ft_bgs = min(.data$wl_ft_bgs, na.rm = TRUE),
    cal_max_wl_ft_bgs = max(.data$wl_ft_bgs, na.rm = TRUE),
    cal_n_measurements = dplyr::n(),
    cal_first_date = as.character(min(.data$obs_date, na.rm = TRUE)),
    cal_last_date = as.character(max(.data$obs_date, na.rm = TRUE)),
    .groups = "drop"
  )

wy_summary <- history_current |>
  dplyr::group_by(.data$site_no, .data$water_year) |>
  dplyr::summarise(
    wy_mean_wl_ft_bgs = mean(.data$wl_ft_bgs, na.rm = TRUE),
    wy_median_wl_ft_bgs = stats::median(.data$wl_ft_bgs, na.rm = TRUE),
    wy_min_wl_ft_bgs = min(.data$wl_ft_bgs, na.rm = TRUE),
    wy_max_wl_ft_bgs = max(.data$wl_ft_bgs, na.rm = TRUE),
    wy_n_measurements = dplyr::n(),
    wy_first_date = as.character(min(.data$obs_date, na.rm = TRUE)),
    wy_last_date = as.character(max(.data$obs_date, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$site_no, .data$water_year)

# ---- 9. Build site-level history metrics -----------------------------------

pt_site_stats <- function(df) {
  df <- df |>
    dplyr::arrange(.data$obs_datetime_utc)

  latest <- df[nrow(df), , drop = FALSE]
  past <- df |>
    dplyr::filter(.data$obs_datetime_utc < latest$obs_datetime_utc[[1]])

  if (nrow(past) == 0) past <- df

  past_values <- past$wl_ft_bgs[is.finite(past$wl_ft_bgs)]
  latest_value <- latest$wl_ft_bgs[[1]]

  por_pct <- if (length(past_values) >= min_por_n) {
    pt_deeper_percentile(latest_value, past_values)
  } else {
    NA_real_
  }

  latest_wday <- latest$water_day_365[[1]]
  seasonal <- tibble::tibble()

  if (!is.na(latest_wday)) {
    seasonal <- past |>
      dplyr::filter(
        !is.na(.data$water_day_365),
        pt_circular_day_distance(.data$water_day_365, latest_wday) <= seasonal_window_days
      )
  }

  seasonal_values <- seasonal$wl_ft_bgs[is.finite(seasonal$wl_ft_bgs)]

  seasonal_pct <- if (length(seasonal_values) >= min_seasonal_n) {
    pt_deeper_percentile(latest_value, seasonal_values)
  } else {
    NA_real_
  }

  tibble::tibble(
    hist_has_history = TRUE,
    hist_record_count = nrow(df),
    hist_first_wl_date = as.character(min(df$obs_date, na.rm = TRUE)),
    hist_last_wl_date = as.character(max(df$obs_date, na.rm = TRUE)),
    hist_por_years = round(as.numeric(max(df$obs_date, na.rm = TRUE) - min(df$obs_date, na.rm = TRUE)) / 365.25, 2),
    hist_latest_wl_ft_bgs = latest_value,
    hist_latest_wl_datetime_utc = format(lubridate::with_tz(latest$obs_datetime_utc[[1]], "UTC"), "%Y-%m-%dT%H:%M:%SZ"),
    hist_latest_wl_date = as.character(latest$obs_date[[1]]),
    hist_latest_water_year = latest$water_year[[1]],
    hist_latest_water_day_365 = latest_wday,
    hist_por_baseline_n = length(past_values),
    hist_por_median_ft_bgs = if (length(past_values) > 0) stats::median(past_values, na.rm = TRUE) else NA_real_,
    hist_por_mean_ft_bgs = if (length(past_values) > 0) mean(past_values, na.rm = TRUE) else NA_real_,
    hist_por_deeper_pctile = por_pct,
    hist_latest_vs_por_median_ft = if (length(past_values) > 0) latest_value - stats::median(past_values, na.rm = TRUE) else NA_real_,
    hist_seasonal_window_days = seasonal_window_days,
    hist_seasonal_baseline_n = length(seasonal_values),
    hist_seasonal_median_ft_bgs = if (length(seasonal_values) > 0) stats::median(seasonal_values, na.rm = TRUE) else NA_real_,
    hist_seasonal_mean_ft_bgs = if (length(seasonal_values) > 0) mean(seasonal_values, na.rm = TRUE) else NA_real_,
    hist_seasonal_deeper_pctile = seasonal_pct,
    hist_latest_vs_seasonal_median_ft = if (length(seasonal_values) > 0) latest_value - stats::median(seasonal_values, na.rm = TRUE) else NA_real_,
    hist_percentile_note = paste0(
      "Depth-to-water percentile uses prior measurements only when available; larger percentiles mean deeper groundwater. Seasonal percentile uses +/-",
      seasonal_window_days,
      " water-day window; Feb. 29 is removed from the 365-day water-year index."
    )
  )
}

site_stats <- history_current |>
  dplyr::group_by(.data$site_no) |>
  dplyr::group_modify(~pt_site_stats(.x)) |>
  dplyr::ungroup()

wy_trend <- wy_summary |>
  dplyr::group_by(.data$site_no) |>
  dplyr::summarise(
    hist_wy_mean_n_years = dplyr::n(),
    hist_wy_mean_first_year = min(.data$water_year, na.rm = TRUE),
    hist_wy_mean_last_year = max(.data$water_year, na.rm = TRUE),
    hist_trend_wy_mean_ft_per_year = pt_safe_lm_slope(.data$water_year, .data$wy_mean_wl_ft_bgs, min_trend_years),
    .groups = "drop"
  )

plot_json <- wy_summary |>
  dplyr::arrange(.data$site_no, .data$water_year) |>
  dplyr::group_by(.data$site_no) |>
  dplyr::slice_tail(n = max_plot_years) |>
  dplyr::summarise(
    hist_plot_wy_mean_json = pt_wy_json(.data$water_year, .data$wy_mean_wl_ft_bgs, .data$wy_n_measurements),
    hist_plot_wy_mean_n_years = dplyr::n(),
    .groups = "drop"
  )

latest_wy_mean <- site_stats |>
  dplyr::select(site_no, hist_latest_water_year) |>
  dplyr::left_join(
    wy_summary |>
      dplyr::select(
        site_no,
        water_year,
        hist_latest_water_year_mean_ft_bgs = wy_mean_wl_ft_bgs,
        hist_latest_water_year_n = wy_n_measurements
      ),
    by = c("site_no" = "site_no", "hist_latest_water_year" = "water_year")
  )

history_summary <- candidates |>
  dplyr::select(site_no) |>
  dplyr::left_join(site_stats, by = "site_no") |>
  dplyr::left_join(wy_trend, by = "site_no") |>
  dplyr::left_join(plot_json, by = "site_no") |>
  dplyr::left_join(latest_wy_mean, by = c("site_no", "hist_latest_water_year")) |>
  dplyr::mutate(
    hist_has_history = dplyr::coalesce(.data$hist_has_history, FALSE),
    hist_sufficient_for_por_percentile = !is.na(.data$hist_por_deeper_pctile),
    hist_sufficient_for_seasonal_percentile = !is.na(.data$hist_seasonal_deeper_pctile),
    hist_sufficient_for_plot = !is.na(.data$hist_plot_wy_mean_json) & .data$hist_plot_wy_mean_n_years >= 5,
    hist_summary_run_time_utc = run_time_utc,
    hist_summary_source = "USGS Water Data API field-measurements parameter 72019; BRIM RF029 local history preprocessor"
  ) |>
  dplyr::arrange(.data$site_no)

# ---- 10. Write outputs ------------------------------------------------------

dir.create(dirname(out_history_summary_csv), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(wy_summary_rds), recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(wy_summary, wy_summary_rds)

readr::write_csv(history_summary, out_history_summary_csv)
readr::write_csv(wy_summary, file.path(qa_dir, "usgs_groundwater_history_water_year_summary_ca.csv"))
readr::write_csv(
  site_stats |>
    dplyr::left_join(wy_trend, by = "site_no") |>
    dplyr::arrange(.data$site_no),
  file.path(qa_dir, "usgs_groundwater_history_site_summary_ca.csv")
)

run_summary <- tibble::tibble(
  metric = c(
    "run time UTC",
    "current candidate sites",
    "raw history cache rows after update",
    "raw history cache sites after update",
    "current candidate history rows",
    "current candidate sites with any history",
    "sites with >= 5 measurements",
    "sites with POR percentile",
    "sites with seasonal percentile",
    "sites sufficient for plot",
    "sites full-backfilled this run",
    "full-history rows fetched this run",
    "recent-refresh rows fetched this run",
    "history start date",
    "history end date",
    "recent refresh days",
    "seasonal window days",
    "max plot years"
  ),
  value = as.character(c(
    run_time_utc,
    nrow(candidates),
    nrow(history_all),
    length(unique(history_all$site_no)),
    nrow(history_current),
    sum(history_summary$hist_has_history, na.rm = TRUE),
    sum(history_summary$hist_record_count >= 5, na.rm = TRUE),
    sum(history_summary$hist_sufficient_for_por_percentile, na.rm = TRUE),
    sum(history_summary$hist_sufficient_for_seasonal_percentile, na.rm = TRUE),
    sum(history_summary$hist_sufficient_for_plot, na.rm = TRUE),
    length(full_fetch_sites),
    nrow(full_history_new),
    nrow(recent_history_new),
    as.character(history_start_date),
    as.character(history_end_date),
    refresh_recent_days,
    seasonal_window_days,
    max_plot_years
  ))
)

readr::write_csv(run_summary, file.path(qa_dir, "usgs_groundwater_history_run_summary.csv"))

message("Saved live-feed history summary CSV: ", out_history_summary_csv)
message("Saved raw groundwater history cache: ", raw_history_rds)
message("Saved water-year summary RDS: ", wy_summary_rds)
message("Saved QA run summary: ", file.path(qa_dir, "usgs_groundwater_history_run_summary.csv"))
message("USGS groundwater history summary complete.")
print(run_summary)
