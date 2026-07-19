# ==== cdec_sensor3_vs82_audit.R =============================================
# Read-only audit of CDEC SWE sensor #3 (SNOW WC) vs sensor #82 (SNO ADJ/revised)
# for BRIM snow-pillow / SWE evaluation.
#
# Run from PortaTreasure2 / BRIM project root:
#   source("cdec_sensor3_vs82_audit.R")
#
# Optional switches before source():
#   Sys.setenv(CDEC82_AUDIT_ALL_CDEC = "TRUE")        # audit all CDEC stations
#   Sys.setenv(CDEC82_AUDIT_STATION_LIMIT = "25")     # default sample size
#   Sys.setenv(CDEC82_AUDIT_START_DATE = "1990-10-01")
#   Sys.setenv(CDEC82_AUDIT_END_DATE = "2026-06-09")
# ============================================================================

local({
  old_wd <- getwd()
  old_opts <- options(na.print = "NA")
  on.exit({
    setwd(old_wd)
    options(old_opts)
  }, add = TRUE)

  if (basename(getwd()) == "brim-live-data-feeds") setwd("..")

  if (!dir.exists("brim-live-data-feeds") || !dir.exists("04_processed_data")) {
    stop("Run this script from the PortaTreasure2 / BRIM project root.")
  }

  pkgs <- c(
    "dplyr", "tidyr", "purrr", "readr", "tibble", "stringr",
    "lubridate", "jsonlite", "curl", "ggplot2", "ggridges", "scales"
  )

  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing required R package(s): ", paste(missing, collapse = ", "),
      "\nInstall first, e.g.: install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "), "))"
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

  qa_dir <- file.path("04_processed_data", "qa", "cdec_sensor3_vs82_audit")
  fig_dir <- file.path("06_output", "figures", "snow_pillow_sensor3_vs82_audit")
  cache_dir <- file.path("01_raw_data", "snow_soil_climate", "cache", "cdec_sensor3_vs82_audit")

  dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  station_index_path <- "brim-live-data-feeds/data/input/snow_pillow_station_index.csv"

  HISTORY_START_DATE <- as.Date(Sys.getenv("CDEC82_AUDIT_START_DATE", unset = "1990-10-01"))
  HISTORY_END_DATE <- as.Date(Sys.getenv("CDEC82_AUDIT_END_DATE", unset = as.character(Sys.Date())))

  SAMPLE_N <- suppressWarnings(as.integer(Sys.getenv("CDEC82_AUDIT_STATION_LIMIT", unset = "25")))
  if (is.na(SAMPLE_N) || SAMPLE_N < 1) SAMPLE_N <- 25L

  DO_ALL <- tolower(Sys.getenv("CDEC82_AUDIT_ALL_CDEC", unset = "false")) %in% c("true", "t", "1", "yes", "y")

  # Visualization cap only; raw values and QA counts are still written.
  PLOT_CAP_IN <- suppressWarnings(as.numeric(Sys.getenv("CDEC82_AUDIT_PLOT_CAP_IN", unset = "150")))
  if (is.na(PLOT_CAP_IN) || PLOT_CAP_IN < 50) PLOT_CAP_IN <- 150

  # Matches current BRIM historical-context philosophy: impossible-high values
  # are invalid for context products.
  QC_HIGH_IN <- suppressWarnings(as.numeric(Sys.getenv("CDEC82_AUDIT_QC_HIGH_IN", unset = "250")))
  if (is.na(QC_HIGH_IN) || QC_HIGH_IN < 50) QC_HIGH_IN <- 250

  REQUEST_PAUSE_SEC <- suppressWarnings(as.numeric(Sys.getenv("CDEC82_AUDIT_REQUEST_PAUSE_SEC", unset = "0.25")))
  if (is.na(REQUEST_PAUSE_SEC) || REQUEST_PAUSE_SEC < 0) REQUEST_PAUSE_SEC <- 0.25

  # ---- helpers --------------------------------------------------------------

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
    if (all(is.na(out))) {
      out <- suppressWarnings(as.Date(lubridate::ymd_hms(x, quiet = TRUE, tz = "UTC")))
    }
    if (all(is.na(out))) {
      out <- suppressWarnings(as.Date(lubridate::ymd(x, quiet = TRUE)))
    }
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

  col_chr <- function(df, nm) {
    if (nm %in% names(df)) pt_chr(df[[nm]]) else rep(NA_character_, nrow(df))
  }

  col_num <- function(df, nm) {
    if (nm %in% names(df)) pt_num(df[[nm]]) else rep(NA_real_, nrow(df))
  }

  first_existing_col <- function(x, choices) {
    nms <- names(x)
    nms_clean <- make.names(tolower(nms))
    choices_clean <- make.names(tolower(choices))
    hit <- match(choices_clean, nms_clean)
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
    date_col <- first_existing_col(raw, c("date", "datetime", "obs_date", "Date", "DATE"))
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
      mutate(provider_station_id = dplyr::coalesce(provider_station_id, station_id))
  }

  fetch_cdec_sensor <- function(station_id, sensor_num,
                                start_date = HISTORY_START_DATE,
                                end_date = HISTORY_END_DATE,
                                attempts = 4,
                                sleep_sec = REQUEST_PAUSE_SEC) {
    cp <- cache_path(station_id, sensor_num, start_date, end_date)

    if (file.exists(cp)) {
      x <- try(readRDS(cp), silent = TRUE)
      if (!inherits(x, "try-error") && is.data.frame(x)) return(tibble::as_tibble(x))
    }

    url <- cdec_url(station_id, sensor_num, start_date, end_date)

    for (i in seq_len(attempts)) {
      out <- tryCatch({
        h <- curl::new_handle(
          useragent = "BRIM CDEC sensor 3 vs 82 audit",
          timeout = 90,
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
        " attempt ", i, "/", attempts,
        " — ", out$message
      )
      Sys.sleep(sleep_sec * i)
    }

    warning("CDEC fetch failed after retries: ", station_id, " sensor ", sensor_num)
    out <- empty_cdec()
    saveRDS(out, cp)
    out
  }

  # ---- station sample -------------------------------------------------------

  cat("\n==== Read CDEC station index ====\n")
  station_index <- readr::read_csv(station_index_path, show_col_types = FALSE)

  cdec_id_vec <- dplyr::coalesce(col_chr(station_index, "cdec_id"), col_chr(station_index, "provider_station_id"))

  cdec_stations <- tibble(
    station_uid = col_chr(station_index, "station_uid"),
    live_provider_key = col_chr(station_index, "live_provider_key"),
    provider_station_id = col_chr(station_index, "provider_station_id"),
    cdec_id = cdec_id_vec,
    station_name = col_chr(station_index, "station_name"),
    elevation_ft = col_num(station_index, "elevation_ft"),
    period_of_record = col_chr(station_index, "period_of_record"),
    official_station_url = col_chr(station_index, "official_station_url"),
    official_data_url = col_chr(station_index, "official_data_url")
  ) |>
    filter(live_provider_key == "cdec_snow_sensor", !is.na(cdec_id)) |>
    distinct(cdec_id, .keep_all = TRUE) |>
    arrange(cdec_id)

  focus_ids <- c(
    "SNM", "HRS", "RCC", "TUN", "SSM", "MED", "RTL",
    "BIM", "BSH", "GIN", "GOL", "MHP", "SIL",
    "ALP", "LVT", "MDW", "BGP", "BCB", "BLA"
  )

  focus <- cdec_stations |> filter(cdec_id %in% focus_ids)

  if (DO_ALL) {
    audit_stations <- cdec_stations
  } else {
    remaining <- cdec_stations |> filter(!cdec_id %in% focus$cdec_id)
    set.seed(82)
    add_n <- max(0L, SAMPLE_N - nrow(focus))
    add <- if (nrow(remaining) > 0 && add_n > 0) {
      remaining |> slice_sample(n = min(add_n, nrow(remaining)))
    } else {
      remaining |> slice_head(n = 0)
    }

    audit_stations <- bind_rows(focus, add) |>
      distinct(cdec_id, .keep_all = TRUE) |>
      slice_head(n = SAMPLE_N)
  }

  cat("CDEC stations in full index: ", nrow(cdec_stations), "\n", sep = "")
  cat("Audit stations: ", nrow(audit_stations), "\n", sep = "")
  cat("Window: ", as.character(HISTORY_START_DATE), " to ", as.character(HISTORY_END_DATE), "\n", sep = "")
  cat("Plot cap: ", PLOT_CAP_IN, " in; QA high threshold: ", QC_HIGH_IN, " in\n", sep = "")

  readr::write_csv(audit_stations, file.path(qa_dir, "cdec_sensor3_vs82_audit_station_sample.csv"))

  # ---- fetch ---------------------------------------------------------------

  cat("\n==== Fetch sensor 3 and 82 ====\n")
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

  raw_out <- file.path(qa_dir, "cdec_sensor3_vs82_audit_raw_long.csv")
  readr::write_csv(raw, raw_out)

  obs <- raw |>
    mutate(
      sensor_label = case_when(
        sensor_num == 3L ~ "Sensor #3 — SNOW WC / raw daily SWE",
        sensor_num == 82L ~ "Sensor #82 — SNO ADJ / revised daily SWE",
        TRUE ~ paste("Sensor", sensor_num)
      ),
      water_year = water_year(obs_date),
      water_day = water_day(obs_date),
      sentinel_or_negative = !is.na(raw_swe_in) & raw_swe_in < 0,
      implausibly_high = !is.na(raw_swe_in) & raw_swe_in > QC_HIGH_IN,
      valid_swe_in = ifelse(
        is.na(raw_swe_in) | raw_swe_in < 0 | raw_swe_in > QC_HIGH_IN,
        NA_real_,
        raw_swe_in
      ),
      plot_swe_in = pmin(valid_swe_in, PLOT_CAP_IN)
    )

  valid_out <- file.path(qa_dir, "cdec_sensor3_vs82_audit_valid_long.csv")
  readr::write_csv(obs, valid_out)

  # ---- QA summaries --------------------------------------------------------

  station_summary <- obs |>
    group_by(cdec_id, station_name, sensor_num, sensor_label) |>
    summarise(
      rows = n(),
      valid_days = sum(!is.na(valid_swe_in)),
      first_valid = suppressWarnings(min(obs_date[!is.na(valid_swe_in)], na.rm = TRUE)),
      last_valid = suppressWarnings(max(obs_date[!is.na(valid_swe_in)], na.rm = TRUE)),
      water_years_with_valid = n_distinct(water_year[!is.na(valid_swe_in)]),
      sentinel_or_negative_days = sum(sentinel_or_negative, na.rm = TRUE),
      implausibly_high_days = sum(implausibly_high, na.rm = TRUE),
      max_raw_swe_in = suppressWarnings(max(raw_swe_in, na.rm = TRUE)),
      max_valid_swe_in = suppressWarnings(max(valid_swe_in, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(
      first_valid = if_else(is.infinite(as.numeric(first_valid)), as.Date(NA), as.Date(first_valid)),
      last_valid = if_else(is.infinite(as.numeric(last_valid)), as.Date(NA), as.Date(last_valid)),
      max_raw_swe_in = ifelse(is.infinite(max_raw_swe_in), NA_real_, max_raw_swe_in),
      max_valid_swe_in = ifelse(is.infinite(max_valid_swe_in), NA_real_, max_valid_swe_in)
    )

  daily_wide <- obs |>
    select(cdec_id, station_name, obs_date, sensor_num, valid_swe_in, raw_swe_in) |>
    mutate(sensor_num = paste0("sensor_", sensor_num)) |>
    pivot_wider(names_from = sensor_num, values_from = c(valid_swe_in, raw_swe_in))

  # Ensure columns exist even when one sensor returns no rows in a small sample.
  for (nm in c("valid_swe_in_sensor_3", "valid_swe_in_sensor_82", "raw_swe_in_sensor_3", "raw_swe_in_sensor_82")) {
    if (!nm %in% names(daily_wide)) daily_wide[[nm]] <- NA_real_
  }

  pair_summary <- daily_wide |>
    group_by(cdec_id, station_name) |>
    summarise(
      days_sensor3_valid = sum(!is.na(valid_swe_in_sensor_3)),
      days_sensor82_valid = sum(!is.na(valid_swe_in_sensor_82)),
      days_both_valid = sum(!is.na(valid_swe_in_sensor_3) & !is.na(valid_swe_in_sensor_82)),
      days_3_only = sum(!is.na(valid_swe_in_sensor_3) & is.na(valid_swe_in_sensor_82)),
      days_82_only = sum(is.na(valid_swe_in_sensor_3) & !is.na(valid_swe_in_sensor_82)),
      pct_82_vs_3_days = round(100 * days_sensor82_valid / pmax(days_sensor3_valid, 1), 1),
      latest_3_date = suppressWarnings(max(obs_date[!is.na(valid_swe_in_sensor_3)], na.rm = TRUE)),
      latest_82_date = suppressWarnings(max(obs_date[!is.na(valid_swe_in_sensor_82)], na.rm = TRUE)),
      mean_abs_diff = mean(abs(valid_swe_in_sensor_82 - valid_swe_in_sensor_3), na.rm = TRUE),
      p95_abs_diff = as.numeric(quantile(abs(valid_swe_in_sensor_82 - valid_swe_in_sensor_3), 0.95, na.rm = TRUE)),
      max_abs_diff = max(abs(valid_swe_in_sensor_82 - valid_swe_in_sensor_3), na.rm = TRUE),
      max_raw_sensor3 = max(raw_swe_in_sensor_3, na.rm = TRUE),
      max_raw_sensor82 = max(raw_swe_in_sensor_82, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      latest_3_date = if_else(is.infinite(as.numeric(latest_3_date)), as.Date(NA), as.Date(latest_3_date)),
      latest_82_date = if_else(is.infinite(as.numeric(latest_82_date)), as.Date(NA), as.Date(latest_82_date)),
      mean_abs_diff = ifelse(is.nan(mean_abs_diff) | is.infinite(mean_abs_diff), NA_real_, round(mean_abs_diff, 3)),
      p95_abs_diff = ifelse(is.nan(p95_abs_diff) | is.infinite(p95_abs_diff), NA_real_, round(p95_abs_diff, 3)),
      max_abs_diff = ifelse(is.infinite(max_abs_diff), NA_real_, round(max_abs_diff, 3)),
      max_raw_sensor3 = ifelse(is.infinite(max_raw_sensor3), NA_real_, max_raw_sensor3),
      max_raw_sensor82 = ifelse(is.infinite(max_raw_sensor82), NA_real_, max_raw_sensor82)
    ) |>
    arrange(pct_82_vs_3_days, desc(days_sensor3_valid))

  by_wy_summary <- obs |>
    filter(!is.na(valid_swe_in)) |>
    group_by(cdec_id, station_name, sensor_num, sensor_label, water_year) |>
    summarise(
      valid_days = n_distinct(obs_date),
      first_day = min(water_day, na.rm = TRUE),
      last_day = max(water_day, na.rm = TRUE),
      day_span = last_day - first_day + 1L,
      max_swe_in = max(valid_swe_in, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(cdec_id, sensor_num, water_year)

  readr::write_csv(station_summary, file.path(qa_dir, "cdec_sensor3_vs82_station_sensor_summary.csv"))
  readr::write_csv(pair_summary, file.path(qa_dir, "cdec_sensor3_vs82_pair_summary.csv"))
  readr::write_csv(by_wy_summary, file.path(qa_dir, "cdec_sensor3_vs82_by_water_year_summary.csv"))

  cat("\n==== Pair summary: #82 completeness vs #3 ====\n")
  pair_summary |>
    select(
      cdec_id, station_name,
      days_sensor3_valid, days_sensor82_valid, pct_82_vs_3_days,
      latest_3_date, latest_82_date,
      mean_abs_diff, p95_abs_diff, max_abs_diff,
      max_raw_sensor3, max_raw_sensor82
    ) |>
    print(n = Inf, width = Inf)

  cat("\n==== Stations where #82 is sparse relative to #3 ====\n")
  pair_summary |>
    arrange(pct_82_vs_3_days, desc(days_sensor3_valid)) |>
    select(cdec_id, station_name, days_sensor3_valid, days_sensor82_valid, pct_82_vs_3_days, latest_3_date, latest_82_date) |>
    print(n = 25, width = Inf)

  cat("\n==== Stations with large cleaned #3 vs #82 differences ====\n")
  pair_summary |>
    filter(!is.na(mean_abs_diff)) |>
    arrange(desc(mean_abs_diff), desc(max_abs_diff)) |>
    select(cdec_id, station_name, days_sensor3_valid, days_sensor82_valid, days_both_valid, mean_abs_diff, p95_abs_diff, max_abs_diff) |>
    print(n = 25, width = Inf)

  cat("\n==== Sensor-level high/sentinel counts ====\n")
  station_summary |>
    select(cdec_id, station_name, sensor_num, valid_days, sentinel_or_negative_days, implausibly_high_days, max_raw_swe_in, max_valid_swe_in) |>
    arrange(desc(implausibly_high_days), desc(sentinel_or_negative_days), cdec_id, sensor_num) |>
    print(n = 60, width = Inf)

  # ---- PDF -----------------------------------------------------------------

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
      filter(cdec_id == station_id, !is.na(water_year), water_year >= 1991) |>
      mutate(
        water_year_f = factor(water_year, levels = sort(unique(water_year))),
        sensor_label = factor(
          sensor_label,
          levels = c("Sensor #3 — SNOW WC / raw daily SWE", "Sensor #82 — SNO ADJ / revised daily SWE")
        )
      )

    st_name <- audit_stations$station_name[match(station_id, audit_stations$cdec_id)]
    st_elev <- audit_stations$elevation_ft[match(station_id, audit_stations$cdec_id)]

    ggplot(
      dat,
      aes(
        x = water_day,
        y = water_year_f,
        height = plot_swe_in,
        group = interaction(sensor_num, water_year)
      )
    ) +
      ggridges::geom_ridgeline(
        scale = 0.88,
        min_height = 0,
        linewidth = 0.25,
        alpha = 0.85,
        na.rm = TRUE
      ) +
      facet_wrap(~ sensor_label, nrow = 1, drop = FALSE) +
      scale_x_continuous(
        breaks = month_breaks,
        labels = month_labels,
        limits = c(1, 366),
        expand = c(0.005, 0)
      ) +
      labs(
        title = paste0(st_name, " (", station_id, ") — CDEC sensor #3 vs #82"),
        subtitle = paste0(
          "Daily SWE by water year; values <0 or >", QC_HIGH_IN,
          " in treated as invalid for audit; plotted heights clipped at ",
          PLOT_CAP_IN, " in. Elev. ", scales::comma(st_elev), " ft."
        ),
        x = "Water day / month",
        y = "Water year",
        caption = "Purpose: visual audit only. This does not change BRIM source logic."
      ) +
      theme_minimal(base_size = 9) +
      theme(
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 11),
        axis.text.y = element_text(size = 6),
        plot.caption = element_text(size = 7, color = "gray35")
      )
  }

  pdf_path <- file.path(
    fig_dir,
    paste0("cdec_sensor3_vs82_ridgeline_audit_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
  )

  cat("\n==== Write ridgeline PDF ====\n")
  cat(pdf_path, "\n")

  grDevices::pdf(pdf_path, width = 8.5, height = 11, onefile = TRUE)
  for (sid in audit_stations$cdec_id) {
    print(make_station_plot(sid))
  }
  grDevices::dev.off()

  cat("\n==== Wrote audit files ====\n")
  cat(raw_out, "\n")
  cat(valid_out, "\n")
  cat(file.path(qa_dir, "cdec_sensor3_vs82_station_sensor_summary.csv"), "\n")
  cat(file.path(qa_dir, "cdec_sensor3_vs82_pair_summary.csv"), "\n")
  cat(file.path(qa_dir, "cdec_sensor3_vs82_by_water_year_summary.csv"), "\n")
  cat(pdf_path, "\n")

  cat("\nNext: skim the PDF for whether #82 is complete, cleaner, sparse, or station-specific.\n")
})
