# ==== SWE020a_cdec_sensor82_all_cdec_source_choice_audit.R ==================
#
# Read-only audit for CDEC SWE sensor #3 (SNOW WC) vs sensor #82 (SNO ADJ).
#
# Purpose:
#   1) Audit #82 completeness and cleanliness across CDEC/CCSS snow sensors.
#   2) Test a station-level candidate rule:
#        prefer #82 if it is available and not stale/lagged relative to #3;
#        otherwise fall back to #3.
#   3) Write QA CSVs and a readable ridgeline PDF.
#
# This script does NOT change BRIM products. It only writes QA and figure files.
#
# Run from PortaTreasure2 project root:
#   source("SWE020a_cdec_sensor82_all_cdec_source_choice_audit.R")
#
# Useful switches:
#   Sys.setenv(CDEC82_AUDIT_STATION_LIMIT = "25")   # quick sample
#   Sys.setenv(CDEC82_AUDIT_STATION_LIMIT = "")     # all stations (default)
#   Sys.setenv(CDEC82_AUDIT_REFRESH_CACHE = "true") # refetch instead of cache
# ============================================================================

local({
  old_wd <- getwd()
  old_opts <- options(na.print = "NA")
  on.exit({
    setwd(old_wd)
    options(old_opts)
  }, add = TRUE)

  if (basename(getwd()) == "brim-live-data-feeds") setwd("..")

  if (!dir.exists("00_config") || !dir.exists("brim-live-data-feeds")) {
    stop("Run this script from the PortaTreasure2 / BRIM project root.")
  }

  pkgs <- c(
    "dplyr", "tidyr", "purrr", "readr", "tibble", "stringr",
    "lubridate", "jsonlite", "curl", "ggplot2", "ggridges", "scales"
  )

  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing required package(s): ", paste(missing, collapse = ", "),
      "\nInstall them first, for example: install.packages(c('",
      paste(missing, collapse = "','"), "'))"
    )
  }

  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(readr)
    library(tibble)
    library(stringr)
    library(lubridate)
    library(jsonlite)
    library(curl)
    library(ggplot2)
    library(ggridges)
    library(scales)
  })

  # ---- switches ------------------------------------------------------------

  STATION_INDEX_CSV <- "brim-live-data-feeds/data/input/snow_pillow_station_index.csv"

  HISTORY_START_DATE <- as.Date(Sys.getenv("CDEC82_AUDIT_START_DATE", unset = "1990-10-01"))
  HISTORY_END_DATE <- as.Date(Sys.getenv("CDEC82_AUDIT_END_DATE", unset = as.character(Sys.Date())))

  station_limit_chr <- Sys.getenv("CDEC82_AUDIT_STATION_LIMIT", unset = NA_character_)
  STATION_LIMIT <- suppressWarnings(as.integer(station_limit_chr))
  if (length(station_limit_chr) == 0 || is.na(station_limit_chr) || trimws(station_limit_chr) == "") {
    STATION_LIMIT <- NA_integer_
  }

  REFRESH_CACHE <- tolower(Sys.getenv("CDEC82_AUDIT_REFRESH_CACHE", unset = "false")) %in%
    c("true", "t", "1", "yes", "y")

  SOURCE_LAG_DAYS <- suppressWarnings(as.integer(Sys.getenv("CDEC82_SOURCE_LAG_DAYS", unset = "7")))
  if (is.na(SOURCE_LAG_DAYS) || SOURCE_LAG_DAYS < 0) SOURCE_LAG_DAYS <- 7L

  RECENT_DAYS <- suppressWarnings(as.integer(Sys.getenv("CDEC82_RECENT_DAYS", unset = "7")))
  if (is.na(RECENT_DAYS) || RECENT_DAYS < 0) RECENT_DAYS <- 7L

  QC_HIGH_IN <- suppressWarnings(as.numeric(Sys.getenv("CDEC82_QC_HIGH_IN", unset = "250")))
  if (is.na(QC_HIGH_IN) || QC_HIGH_IN < 50) QC_HIGH_IN <- 250

  PLOT_CAP_IN <- suppressWarnings(as.numeric(Sys.getenv("CDEC82_PLOT_CAP_IN", unset = "150")))
  if (is.na(PLOT_CAP_IN) || PLOT_CAP_IN < 25) PLOT_CAP_IN <- 150

  SKIP_PDF <- tolower(Sys.getenv("CDEC82_AUDIT_SKIP_PDF", unset = "false")) %in%
    c("true", "t", "1", "yes", "y")

  REQUEST_PAUSE_SEC <- suppressWarnings(as.numeric(Sys.getenv("CDEC82_REQUEST_PAUSE_SEC", unset = "0.15")))
  if (is.na(REQUEST_PAUSE_SEC) || REQUEST_PAUSE_SEC < 0) REQUEST_PAUSE_SEC <- 0.15

  FETCH_ATTEMPTS <- suppressWarnings(as.integer(Sys.getenv("CDEC82_FETCH_ATTEMPTS", unset = "4")))
  if (is.na(FETCH_ATTEMPTS) || FETCH_ATTEMPTS < 1) FETCH_ATTEMPTS <- 4L

  # ---- paths ---------------------------------------------------------------

  qa_dir <- file.path("04_processed_data", "qa", "cdec_sensor82_source_choice_audit")
  fig_dir <- file.path("06_output", "figures", "snow_pillow_sensor82_audit")
  cache_dir <- file.path("01_raw_data", "snow_soil_climate", "cache", "cdec_sensor3_vs82_audit")

  dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # ---- helpers -------------------------------------------------------------

  pt_chr <- function(x) {
    x <- as.character(x)
    x <- stringr::str_squish(x)
    x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
    x
  }

  pt_num <- function(x) {
    suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
  }

  pt_date <- function(x) {
    if (inherits(x, "Date")) return(as.Date(x))
    if (inherits(x, "POSIXt")) return(as.Date(x))
    x <- pt_chr(x)
    out <- suppressWarnings(as.Date(x))
    if (all(is.na(out))) out <- suppressWarnings(as.Date(lubridate::ymd_hms(x, quiet = TRUE, tz = "UTC")))
    if (all(is.na(out))) out <- suppressWarnings(as.Date(lubridate::ymd(x, quiet = TRUE)))
    out
  }

  water_year <- function(date) {
    date <- as.Date(date)
    yr <- as.integer(format(date, "%Y"))
    mo <- as.integer(format(date, "%m"))
    ifelse(mo >= 10L, yr + 1L, yr)
  }

  water_day <- function(date) {
    date <- as.Date(date)
    wy <- water_year(date)
    wy_start <- as.Date(sprintf("%d-10-01", wy - 1L))
    as.integer(date - wy_start + 1L)
  }

  norm_name <- function(x) gsub("[^a-z0-9]", "", tolower(x))

  first_existing_col <- function(x, choices) {
    nms <- names(x)
    nms_norm <- norm_name(nms)
    choices_norm <- norm_name(choices)
    hit <- match(choices_norm, nms_norm)
    hit <- hit[!is.na(hit)]
    if (length(hit) == 0) NA_character_ else nms[hit[[1]]]
  }

  cdec_url <- function(station_id, sensor_num, start_date, end_date) {
    paste0(
      "https://cdec.water.ca.gov/dynamicapp/req/JSONDataServlet?",
      "Stations=", utils::URLencode(station_id, reserved = TRUE),
      "&SensorNums=", sensor_num,
      "&dur_code=D",
      "&Start=", format(as.Date(start_date), "%Y-%m-%d"),
      "&End=", format(as.Date(end_date), "%Y-%m-%d")
    )
  }

  cache_path <- function(station_id, sensor_num, start_date, end_date) {
    file.path(
      cache_dir,
      paste0(
        "cdec_", station_id,
        "_sensor", sensor_num,
        "_", format(as.Date(start_date), "%Y%m%d"),
        "_", format(as.Date(end_date), "%Y%m%d"),
        ".rds"
      )
    )
  }

  empty_cdec <- function() {
    tibble(
      provider_station_id = character(),
      sensor_num = integer(),
      obs_date = as.Date(character()),
      raw_swe_in = numeric()
    )
  }

  standardize_cdec <- function(raw, station_id, sensor_num) {
    if (is.null(raw) || length(raw) == 0) return(empty_cdec())
    raw <- tibble::as_tibble(raw)
    if (nrow(raw) == 0) return(empty_cdec())

    station_col <- first_existing_col(raw, c("stationId", "station_id", "station", "id", "STATION_ID"))
    date_col <- first_existing_col(raw, c("date", "datetime", "obs_date", "obsDate", "eventDate", "DATE", "DATE TIME"))
    value_col <- first_existing_col(raw, c("value", "sensorValue", "obsValue", "VALUE", "snow_water_content", "SWE"))

    if (is.na(date_col) || is.na(value_col)) {
      warning(
        "Could not parse CDEC columns for ", station_id, " sensor ", sensor_num,
        ". Columns: ", paste(names(raw), collapse = ", ")
      )
      return(empty_cdec())
    }

    tibble(
      provider_station_id = if (!is.na(station_col)) pt_chr(raw[[station_col]]) else station_id,
      sensor_num = as.integer(sensor_num),
      obs_date = pt_date(raw[[date_col]]),
      raw_swe_in = pt_num(raw[[value_col]])
    ) |>
      filter(!is.na(obs_date)) |>
      mutate(provider_station_id = coalesce(provider_station_id, station_id))
  }

  fetch_cdec_sensor <- function(station_id, sensor_num) {
    cp <- cache_path(station_id, sensor_num, HISTORY_START_DATE, HISTORY_END_DATE)

    if (!REFRESH_CACHE && file.exists(cp)) {
      x <- try(readRDS(cp), silent = TRUE)
      if (!inherits(x, "try-error") && is.data.frame(x)) return(tibble::as_tibble(x))
    }

    url <- cdec_url(station_id, sensor_num, HISTORY_START_DATE, HISTORY_END_DATE)

    for (i in seq_len(FETCH_ATTEMPTS)) {
      Sys.sleep(REQUEST_PAUSE_SEC * i)

      out <- tryCatch({
        h <- curl::new_handle(
          useragent = "BRIM CDEC sensor 3 vs 82 source-choice audit",
          timeout = 120,
          connecttimeout = 30,
          followlocation = TRUE
        )
        txt <- rawToChar(curl::curl_fetch_memory(url, handle = h)$content)
        raw <- jsonlite::fromJSON(txt, flatten = TRUE)
        standardize_cdec(raw, station_id = station_id, sensor_num = sensor_num)
      }, error = function(e) {
        structure(list(message = conditionMessage(e)), class = "cdec_fetch_error")
      })

      if (!inherits(out, "cdec_fetch_error")) {
        saveRDS(out, cp)
        return(out)
      }

      message(
        "CDEC fetch failed: ", station_id,
        " sensor ", sensor_num,
        " attempt ", i, "/", FETCH_ATTEMPTS,
        " — ", out$message
      )
    }

    warning("CDEC fetch failed after retries: ", station_id, " sensor ", sensor_num)
    out <- empty_cdec()
    saveRDS(out, cp)
    out
  }

  latest_delta_from_trace <- function(trace_df) {
    if (!is.data.frame(trace_df) || nrow(trace_df) == 0) {
      return(tibble(
        cdec_id = character(),
        latest_selected_date = as.Date(character()),
        latest_selected_swe_in = numeric(),
        selected_delta_1day_in = numeric(),
        selected_delta_3day_in = numeric(),
        selected_delta_7day_in = numeric()
      ))
    }

    obs <- trace_df |>
      filter(!is.na(valid_swe_in), !is.na(obs_date)) |>
      arrange(cdec_id, obs_date)

    if (nrow(obs) == 0) {
      return(tibble(
        cdec_id = character(),
        latest_selected_date = as.Date(character()),
        latest_selected_swe_in = numeric(),
        selected_delta_1day_in = numeric(),
        selected_delta_3day_in = numeric(),
        selected_delta_7day_in = numeric()
      ))
    }

    latest <- obs |>
      arrange(cdec_id, desc(obs_date)) |>
      group_by(cdec_id) |>
      slice(1) |>
      ungroup() |>
      transmute(
        cdec_id,
        latest_selected_date = obs_date,
        latest_selected_swe_in = valid_swe_in
      )

    delta_for <- function(days_back) {
      latest |>
        left_join(obs, by = "cdec_id", relationship = "many-to-many") |>
        filter(obs_date <= latest_selected_date - days_back) |>
        arrange(cdec_id, desc(obs_date)) |>
        group_by(cdec_id) |>
        slice(1) |>
        ungroup() |>
        transmute(
          cdec_id,
          delta = latest_selected_swe_in - valid_swe_in
        )
    }

    d1 <- delta_for(1L) |> transmute(cdec_id, selected_delta_1day_in = delta)
    d3 <- delta_for(3L) |> transmute(cdec_id, selected_delta_3day_in = delta)
    d7 <- delta_for(7L) |> transmute(cdec_id, selected_delta_7day_in = delta)

    latest |>
      left_join(d1, by = "cdec_id") |>
      left_join(d3, by = "cdec_id") |>
      left_join(d7, by = "cdec_id")
  }

  safe_min_date <- function(x) {
    x <- as.Date(x)
    if (all(is.na(x))) as.Date(NA) else min(x, na.rm = TRUE)
  }

  safe_max_date <- function(x) {
    x <- as.Date(x)
    if (all(is.na(x))) as.Date(NA) else max(x, na.rm = TRUE)
  }

  safe_max_num <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  }

  safe_mean_num <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  }

  safe_quantile_num <- function(x, p) {
    x <- suppressWarnings(as.numeric(x))
    if (all(is.na(x))) NA_real_ else as.numeric(quantile(x, p, na.rm = TRUE))
  }

  # ---- read station index --------------------------------------------------

  cat("\n==== Read CDEC station index ====\n")
  if (!file.exists(STATION_INDEX_CSV)) stop("Missing station index: ", STATION_INDEX_CSV)

  station_index <- readr::read_csv(STATION_INDEX_CSV, show_col_types = FALSE)

  cdec_stations <- station_index |>
    filter(live_provider_key == "cdec_snow_sensor") |>
    transmute(
      station_uid,
      provider_station_id = pt_chr(provider_station_id),
      cdec_id = coalesce(pt_chr(cdec_id), pt_chr(provider_station_id)),
      station_name,
      elevation_ft = suppressWarnings(as.numeric(elevation_ft)),
      period_of_record,
      official_station_url,
      official_data_url
    ) |>
    filter(!is.na(cdec_id)) |>
    distinct(cdec_id, .keep_all = TRUE)

  focus_ids <- c(
    "SNM", "HRS", "RCC", "TUN", "SSM", "MED", "RTL",
    "BIM", "BSH", "GIN", "GOL", "MHP", "SIL",
    "ALP", "LVT", "MDW", "BGP", "BCB", "BLA"
  )

  cdec_stations <- cdec_stations |>
    mutate(focus_rank = match(cdec_id, focus_ids)) |>
    arrange(is.na(focus_rank), focus_rank, cdec_id) |>
    select(-focus_rank)

  if (!is.na(STATION_LIMIT) && STATION_LIMIT > 0) {
    audit_stations <- cdec_stations |> slice_head(n = STATION_LIMIT)
  } else {
    audit_stations <- cdec_stations
  }

  cat("CDEC stations in index: ", nrow(cdec_stations), "\n", sep = "")
  cat("Audit stations: ", nrow(audit_stations), "\n", sep = "")
  cat("Window: ", as.character(HISTORY_START_DATE), " to ", as.character(HISTORY_END_DATE), "\n", sep = "")
  cat("Source-choice lag threshold: ", SOURCE_LAG_DAYS, " days\n", sep = "")
  cat("Recent/current reporting threshold: ", RECENT_DAYS, " days from audit end date\n", sep = "")
  cat("QC: valid SWE is >=0 and <=", QC_HIGH_IN, " in; plot cap ", PLOT_CAP_IN, " in\n", sep = "")

  readr::write_csv(audit_stations, file.path(qa_dir, "cdec82_audit_station_inventory.csv"))

  # ---- fetch ---------------------------------------------------------------

  cat("\n==== Fetch CDEC sensor #3 and #82 ====\n")

  raw <- purrr::map_dfr(seq_len(nrow(audit_stations)), function(i) {
    st <- audit_stations[i, ]
    cat("[", i, "/", nrow(audit_stations), "] ", st$cdec_id, " ", st$station_name, "\n", sep = "")

    bind_rows(
      fetch_cdec_sensor(st$cdec_id, 3L),
      fetch_cdec_sensor(st$cdec_id, 82L)
    ) |>
      mutate(
        station_uid = st$station_uid,
        provider_station_id = st$provider_station_id,
        cdec_id = st$cdec_id,
        station_name = st$station_name,
        elevation_ft = st$elevation_ft,
        period_of_record = st$period_of_record
      )
  })

  raw_out <- file.path(qa_dir, "cdec82_audit_raw_long.csv")
  readr::write_csv(raw, raw_out)

  obs <- raw |>
    mutate(
      sensor_num = as.integer(sensor_num),
      sensor_label = case_when(
        sensor_num == 3L ~ "#3 SNOW WC / raw daily SWE",
        sensor_num == 82L ~ "#82 SNO ADJ / revised daily SWE",
        TRUE ~ paste("Sensor", sensor_num)
      ),
      obs_date = as.Date(obs_date),
      water_year = water_year(obs_date),
      water_day = water_day(obs_date),
      sentinel_or_negative = !is.na(raw_swe_in) & raw_swe_in < 0,
      implausibly_high = !is.na(raw_swe_in) & raw_swe_in > QC_HIGH_IN,
      valid_swe_in = ifelse(
        is.na(raw_swe_in) | raw_swe_in < 0 | raw_swe_in > QC_HIGH_IN,
        NA_real_,
        raw_swe_in
      ),
      plot_height = pmin(valid_swe_in, PLOT_CAP_IN) / PLOT_CAP_IN
    )

  valid_out <- file.path(qa_dir, "cdec82_audit_valid_long.csv")
  readr::write_csv(obs, valid_out)

  # ---- summaries -----------------------------------------------------------

  station_sensor_summary <- obs |>
    group_by(cdec_id, station_name, sensor_num, sensor_label) |>
    summarise(
      rows = n(),
      valid_days = sum(!is.na(valid_swe_in)),
      first_valid = safe_min_date(obs_date[!is.na(valid_swe_in)]),
      last_valid = safe_max_date(obs_date[!is.na(valid_swe_in)]),
      latest_valid_swe_in = {
        idx <- which(!is.na(valid_swe_in) & obs_date == safe_max_date(obs_date[!is.na(valid_swe_in)]))
        if (length(idx) == 0) NA_real_ else valid_swe_in[idx[[1]]]
      },
      water_years_with_valid = n_distinct(water_year[!is.na(valid_swe_in)]),
      sentinel_or_negative_days = sum(sentinel_or_negative, na.rm = TRUE),
      implausibly_high_days = sum(implausibly_high, na.rm = TRUE),
      max_raw_swe_in = safe_max_num(raw_swe_in),
      max_valid_swe_in = safe_max_num(valid_swe_in),
      .groups = "drop"
    )

  wide <- obs |>
    select(cdec_id, station_name, obs_date, sensor_num, valid_swe_in, raw_swe_in) |>
    mutate(sensor_num = paste0("sensor_", sensor_num)) |>
    distinct(cdec_id, station_name, obs_date, sensor_num, .keep_all = TRUE) |>
    pivot_wider(
      names_from = sensor_num,
      values_from = c(valid_swe_in, raw_swe_in)
    )

  needed_cols <- c(
    "valid_swe_in_sensor_3", "valid_swe_in_sensor_82",
    "raw_swe_in_sensor_3", "raw_swe_in_sensor_82"
  )
  for (nm in needed_cols) {
    if (!nm %in% names(wide)) wide[[nm]] <- NA_real_
  }

  pair_summary <- wide |>
    group_by(cdec_id, station_name) |>
    summarise(
      days_sensor3_valid = sum(!is.na(valid_swe_in_sensor_3)),
      days_sensor82_valid = sum(!is.na(valid_swe_in_sensor_82)),
      days_both_valid = sum(!is.na(valid_swe_in_sensor_3) & !is.na(valid_swe_in_sensor_82)),
      days_3_only = sum(!is.na(valid_swe_in_sensor_3) & is.na(valid_swe_in_sensor_82)),
      days_82_only = sum(is.na(valid_swe_in_sensor_3) & !is.na(valid_swe_in_sensor_82)),
      pct_82_vs_3_days = round(100 * days_sensor82_valid / pmax(days_sensor3_valid, 1), 1),
      latest_3_date = safe_max_date(obs_date[!is.na(valid_swe_in_sensor_3)]),
      latest_82_date = safe_max_date(obs_date[!is.na(valid_swe_in_sensor_82)]),
      latest_3_swe_in = {
        d <- safe_max_date(obs_date[!is.na(valid_swe_in_sensor_3)])
        vals <- valid_swe_in_sensor_3[obs_date == d]
        vals <- vals[!is.na(vals)]
        if (length(vals) == 0) NA_real_ else vals[[1]]
      },
      latest_82_swe_in = {
        d <- safe_max_date(obs_date[!is.na(valid_swe_in_sensor_82)])
        vals <- valid_swe_in_sensor_82[obs_date == d]
        vals <- vals[!is.na(vals)]
        if (length(vals) == 0) NA_real_ else vals[[1]]
      },
      mean_abs_diff = safe_mean_num(abs(valid_swe_in_sensor_82 - valid_swe_in_sensor_3)),
      median_abs_diff = if (all(is.na(abs(valid_swe_in_sensor_82 - valid_swe_in_sensor_3)))) NA_real_ else median(abs(valid_swe_in_sensor_82 - valid_swe_in_sensor_3), na.rm = TRUE),
      p95_abs_diff = safe_quantile_num(abs(valid_swe_in_sensor_82 - valid_swe_in_sensor_3), 0.95),
      max_abs_diff = safe_max_num(abs(valid_swe_in_sensor_82 - valid_swe_in_sensor_3)),
      max_raw_sensor3 = safe_max_num(raw_swe_in_sensor_3),
      max_raw_sensor82 = safe_max_num(raw_swe_in_sensor_82),
      .groups = "drop"
    ) |>
    mutate(
      latest_82_lag_vs_3_days = as.integer(latest_3_date - latest_82_date),
      latest_3_age_days = as.integer(HISTORY_END_DATE - latest_3_date),
      latest_82_age_days = as.integer(HISTORY_END_DATE - latest_82_date),
      sensor3_recent = !is.na(latest_3_age_days) & latest_3_age_days <= RECENT_DAYS,
      sensor82_recent = !is.na(latest_82_age_days) & latest_82_age_days <= RECENT_DAYS,
      selected_source = case_when(
        !is.na(latest_82_date) & (is.na(latest_3_date) | latest_82_date >= latest_3_date - SOURCE_LAG_DAYS) ~ "sensor_82_sno_adj",
        !is.na(latest_3_date) ~ "sensor_3_snow_wc_fallback",
        TRUE ~ "no_valid_cdec_swe"
      ),
      selected_sensor_num = case_when(
        selected_source == "sensor_82_sno_adj" ~ 82L,
        selected_source == "sensor_3_snow_wc_fallback" ~ 3L,
        TRUE ~ NA_integer_
      ),
      selected_source_reason = case_when(
        selected_source == "sensor_82_sno_adj" & is.na(latest_3_date) ~ "#82 has data; #3 has no valid data",
        selected_source == "sensor_82_sno_adj" & !is.na(latest_3_date) & latest_82_date >= latest_3_date ~ "#82 is as current or newer than #3",
        selected_source == "sensor_82_sno_adj" ~ paste0("#82 is within ", SOURCE_LAG_DAYS, " days of #3"),
        selected_source == "sensor_3_snow_wc_fallback" & is.na(latest_82_date) ~ "#82 has no valid data; fallback to #3",
        selected_source == "sensor_3_snow_wc_fallback" ~ paste0("#82 lags #3 by >", SOURCE_LAG_DAYS, " days; fallback to #3"),
        TRUE ~ "no valid #3 or #82 data"
      ),
      selected_latest_date = if_else(selected_sensor_num == 82L, latest_82_date, latest_3_date),
      selected_latest_swe_in = if_else(selected_sensor_num == 82L, latest_82_swe_in, latest_3_swe_in),
      selected_age_days = as.integer(HISTORY_END_DATE - selected_latest_date),
      selected_recent = !is.na(selected_age_days) & selected_age_days <= RECENT_DAYS,
      mean_abs_diff = round(mean_abs_diff, 3),
      median_abs_diff = round(median_abs_diff, 3),
      p95_abs_diff = round(p95_abs_diff, 3),
      max_abs_diff = round(max_abs_diff, 3)
    ) |>
    arrange(selected_source, desc(latest_82_lag_vs_3_days), cdec_id)

  selected_trace <- obs |>
    left_join(pair_summary |> select(cdec_id, selected_sensor_num), by = "cdec_id") |>
    filter(!is.na(selected_sensor_num), sensor_num == selected_sensor_num) |>
    filter(!is.na(valid_swe_in)) |>
    arrange(cdec_id, obs_date)

  selected_deltas <- latest_delta_from_trace(selected_trace)

  latest_source_choice <- pair_summary |>
    left_join(selected_deltas, by = "cdec_id") |>
    left_join(
      audit_stations |>
        select(cdec_id, station_uid, provider_station_id, elevation_ft, period_of_record),
      by = "cdec_id"
    ) |>
    select(
      cdec_id, station_uid, provider_station_id, station_name, elevation_ft, period_of_record,
      days_sensor3_valid, days_sensor82_valid, days_both_valid, pct_82_vs_3_days,
      latest_3_date, latest_3_swe_in, latest_3_age_days, sensor3_recent,
      latest_82_date, latest_82_swe_in, latest_82_age_days, sensor82_recent,
      latest_82_lag_vs_3_days,
      selected_source, selected_sensor_num, selected_source_reason,
      selected_latest_date, selected_latest_swe_in, selected_age_days, selected_recent,
      selected_delta_1day_in, selected_delta_3day_in, selected_delta_7day_in,
      mean_abs_diff, median_abs_diff, p95_abs_diff, max_abs_diff,
      max_raw_sensor3, max_raw_sensor82
    )

  by_wy_summary <- obs |>
    filter(!is.na(valid_swe_in)) |>
    group_by(cdec_id, station_name, sensor_num, sensor_label, water_year) |>
    summarise(
      valid_days = n_distinct(obs_date),
      first_water_day = min(water_day, na.rm = TRUE),
      last_water_day = max(water_day, na.rm = TRUE),
      water_day_span = last_water_day - first_water_day + 1L,
      max_swe_in = max(valid_swe_in, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(cdec_id, sensor_num, water_year)

  # ---- write CSVs ----------------------------------------------------------

  station_sensor_out <- file.path(qa_dir, "cdec82_station_sensor_summary.csv")
  pair_out <- file.path(qa_dir, "cdec82_pair_summary.csv")
  latest_choice_out <- file.path(qa_dir, "cdec82_candidate_latest_source_choice.csv")
  selected_trace_out <- file.path(qa_dir, "cdec82_candidate_selected_trace.csv")
  by_wy_out <- file.path(qa_dir, "cdec82_by_water_year_summary.csv")

  readr::write_csv(station_sensor_summary, station_sensor_out)
  readr::write_csv(pair_summary, pair_out)
  readr::write_csv(latest_source_choice, latest_choice_out)
  readr::write_csv(selected_trace, selected_trace_out)
  readr::write_csv(by_wy_summary, by_wy_out)

  # ---- console QA ----------------------------------------------------------

  cat("\n==== Candidate source-choice counts ====\n")
  latest_source_choice |>
    count(selected_source, selected_recent, name = "stations") |>
    arrange(selected_source, desc(selected_recent)) |>
    print(n = Inf, width = Inf)

  cat("\n==== #82 completeness vs #3 summary ====\n")
  latest_source_choice |>
    summarise(
      stations = n(),
      total_days_sensor3_valid = sum(days_sensor3_valid, na.rm = TRUE),
      total_days_sensor82_valid = sum(days_sensor82_valid, na.rm = TRUE),
      total_days_both_valid = sum(days_both_valid, na.rm = TRUE),
      median_pct_82_vs_3_days = median(pct_82_vs_3_days, na.rm = TRUE),
      min_pct_82_vs_3_days = min(pct_82_vs_3_days, na.rm = TRUE),
      max_pct_82_vs_3_days = max(pct_82_vs_3_days, na.rm = TRUE),
      stations_82_at_least_95pct_of_3 = sum(pct_82_vs_3_days >= 95, na.rm = TRUE),
      stations_82_less_than_50pct_of_3 = sum(pct_82_vs_3_days < 50, na.rm = TRUE)
    ) |>
    print(width = Inf)

  cat("\n==== Stations falling back to #3 under candidate rule ====\n")
  latest_source_choice |>
    filter(selected_source == "sensor_3_snow_wc_fallback") |>
    arrange(desc(latest_82_lag_vs_3_days), cdec_id) |>
    select(
      cdec_id, station_name,
      latest_3_date, latest_3_swe_in, latest_3_age_days,
      latest_82_date, latest_82_swe_in, latest_82_age_days,
      latest_82_lag_vs_3_days, selected_source_reason,
      max_raw_sensor3, max_raw_sensor82
    ) |>
    print(n = Inf, width = Inf)

  cat("\n==== Selected-source latest/delta examples ====\n")
  latest_source_choice |>
    arrange(selected_source, cdec_id) |>
    select(
      cdec_id, station_name, selected_source, selected_latest_date, selected_latest_swe_in,
      selected_delta_1day_in, selected_delta_3day_in, selected_delta_7day_in,
      selected_source_reason
    ) |>
    print(n = 40, width = Inf)

  cat("\n==== Stations with large cleaned #3 vs #82 differences ====\n")
  latest_source_choice |>
    filter(!is.na(mean_abs_diff)) |>
    arrange(desc(mean_abs_diff), desc(max_abs_diff)) |>
    select(
      cdec_id, station_name, days_both_valid, mean_abs_diff, median_abs_diff,
      p95_abs_diff, max_abs_diff, max_raw_sensor3, max_raw_sensor82
    ) |>
    print(n = 40, width = Inf)

  cat("\n==== Sensor-level high/sentinel counts ====\n")
  station_sensor_summary |>
    select(
      cdec_id, station_name, sensor_num, valid_days,
      sentinel_or_negative_days, implausibly_high_days,
      max_raw_swe_in, max_valid_swe_in, last_valid
    ) |>
    arrange(desc(implausibly_high_days), desc(sentinel_or_negative_days), cdec_id, sensor_num) |>
    print(n = 80, width = Inf)

  # ---- fixed-scale ridgeline PDF ------------------------------------------

  pdf_path <- NA_character_

  if (!SKIP_PDF) {
    ref_start <- as.Date("2024-10-01")
    month_dates <- as.Date(c(
      "2024-10-01", "2024-11-01", "2024-12-01",
      "2025-01-01", "2025-02-01", "2025-03-01",
      "2025-04-01", "2025-05-01", "2025-06-01",
      "2025-07-01", "2025-08-01", "2025-09-01"
    ))
    month_breaks <- as.integer(month_dates - ref_start + 1L)
    month_labels <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")

    make_station_plot <- function(station_id) {
      dat <- obs |>
        filter(cdec_id == station_id, !is.na(valid_swe_in), !is.na(water_year), water_year >= 1991) |>
        mutate(
          sensor_label = factor(
            sensor_label,
            levels = c("#3 SNOW WC / raw daily SWE", "#82 SNO ADJ / revised daily SWE")
          ),
          water_year_f = factor(water_year, levels = sort(unique(water_year)))
        )

      st <- audit_stations |> filter(cdec_id == station_id) |> slice(1)
      choice <- latest_source_choice |> filter(cdec_id == station_id) |> slice(1)
      selected_lab <- if (nrow(choice) == 0) "selected source: unavailable" else {
        paste0(
          "candidate selected source: ", choice$selected_source,
          "; latest selected SWE: ", ifelse(is.na(choice$selected_latest_swe_in), "NA", round(choice$selected_latest_swe_in, 1)),
          " in on ", ifelse(is.na(choice$selected_latest_date), "NA", as.character(choice$selected_latest_date))
        )
      }

      ggplot(
        dat,
        aes(
          x = water_day,
          y = water_year_f,
          height = plot_height,
          group = interaction(sensor_num, water_year)
        )
      ) +
        ggridges::geom_ridgeline(
          scale = 0.88,
          min_height = 0,
          linewidth = 0.25,
          alpha = 0.72,
          fill = "gray70",
          color = "gray25",
          na.rm = TRUE
        ) +
        facet_wrap(~ sensor_label, nrow = 1) +
        scale_x_continuous(
          breaks = month_breaks,
          labels = month_labels,
          limits = c(1, 366),
          expand = c(0.005, 0)
        ) +
        labs(
          title = paste0(st$station_name, " (", station_id, ") — CDEC sensor #3 vs #82"),
          subtitle = paste0(
            "Daily SWE by water year; ridge height normalized to ", PLOT_CAP_IN,
            " in cap; valid audit values are >=0 and <=", QC_HIGH_IN,
            " in. ", selected_lab
          ),
          x = "Water day / month",
          y = "Water year",
          caption = "Read-only audit. #82 is SNO ADJ/revised daily SWE; #3 is SNOW WC/raw daily SWE."
        ) +
        theme_minimal(base_size = 9) +
        theme(
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", size = 8),
          plot.title = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 8),
          axis.text.y = element_text(size = 5.5),
          plot.caption = element_text(size = 7, color = "gray35")
        )
    }

    pdf_path <- file.path(
      fig_dir,
      paste0("cdec_sensor3_vs82_source_choice_ridgelines_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    )

    cat("\n==== Write fixed-scale ridgeline PDF ====\n")
    cat(pdf_path, "\n")

    grDevices::pdf(pdf_path, width = 8.5, height = 11, onefile = TRUE)
    for (sid in audit_stations$cdec_id) {
      print(make_station_plot(sid))
    }
    grDevices::dev.off()
  }

  # ---- manifest ------------------------------------------------------------

  manifest <- tibble(
    run_time = format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE),
    history_start_date = as.character(HISTORY_START_DATE),
    history_end_date = as.character(HISTORY_END_DATE),
    stations_in_audit = nrow(audit_stations),
    source_lag_days = SOURCE_LAG_DAYS,
    recent_days = RECENT_DAYS,
    qc_high_in = QC_HIGH_IN,
    plot_cap_in = PLOT_CAP_IN,
    raw_long_csv = raw_out,
    valid_long_csv = valid_out,
    station_sensor_summary_csv = station_sensor_out,
    pair_summary_csv = pair_out,
    latest_source_choice_csv = latest_choice_out,
    selected_trace_csv = selected_trace_out,
    by_water_year_summary_csv = by_wy_out,
    ridgeline_pdf = pdf_path
  )

  manifest_out <- file.path(qa_dir, "cdec82_audit_manifest.csv")
  readr::write_csv(manifest, manifest_out)

  cat("\n==== Wrote audit files ====\n")
  cat(raw_out, "\n")
  cat(valid_out, "\n")
  cat(station_sensor_out, "\n")
  cat(pair_out, "\n")
  cat(latest_choice_out, "\n")
  cat(selected_trace_out, "\n")
  cat(by_wy_out, "\n")
  if (!SKIP_PDF) cat(pdf_path, "\n")
  cat(manifest_out, "\n")

  cat("\nDone: SWE020a CDEC #82 source-choice audit complete.\n")
})
