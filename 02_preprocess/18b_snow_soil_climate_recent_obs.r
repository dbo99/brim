# ==== 18b_snow_soil_climate_recent_obs.r ====================================
##
## PURPOSE:
##   Fetch recent observation data for PT2 snow / soil climate station layers.
##
## INPUTS:
##   04_processed_data/rds/scan_stations_wgs84.rds
##   04_processed_data/rds/snow_pillows_wgs84.rds
##
## OUTPUTS:
##   04_processed_data/rds/scan_stations_recent_wgs84.rds
##   04_processed_data/rds/snow_pillows_recent_wgs84.rds
##
## DESIGN:
##   - Keep station-location metadata separate from recent observations.
##   - Fetch recent observations only when this script is run.
##   - Join recent observations back to the station-location sf objects.
##   - Fail softly where individual stations/sources do not return data.
##
## RECENT DATA:
##   SCAN:
##     - Fetch recent NRCS soil moisture using soilDB::fetchSCAN(report = "SMS").
##     - Preserve multiple depths where available.
##     - Select one preferred depth for map symbology.
##
##   Snow pillows:
##     - Fetch recent SNOTEL SWE with soilDB::fetchSCAN(report = "WTEQ").
##     - Fetch recent SNOTEL snow depth with soilDB::fetchSCAN(report = "SNWD").
##     - Fetch recent CDEC SWE with sharpshootR::CDECquery(sensor = 3).
##     - Compute latest value, 24-hour delta, and 48-hour delta when available.
##
## IMPORTANT:
##   This is a sidecar refresh script. It should not be called by the final map
##   build. Run it manually when you want recent snow/soil data refreshed.
## ============================================================================


# ==== 1. Load configuration ==================================================

source("00_config/config_paths.r")
source("03_functions/spatial_helpers.r")


# ==== 2. Load packages =======================================================

packages <- c(
  "sf",
  "dplyr",
  "tidyr",
  "stringr",
  "tibble",
  "readr",
  "purrr",
  "lubridate",
  "soilDB",
  "sharpshootR"
)

missing_packages <- packages[!packages %in% rownames(installed.packages())]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(readr)
  library(purrr)
  library(lubridate)
  library(soilDB)
  library(sharpshootR)
})


# ==== 3. User-facing switches ================================================

FETCH_SCAN_SOIL_MOISTURE <- TRUE
FETCH_SNOTEL_SWE         <- TRUE
FETCH_CDEC_SWE           <- TRUE

WRITE_RDS <- TRUE
WRITE_QA  <- TRUE

## Recent-data lookback.
## For daily data, this provides enough history to compute latest, 24-hr delta,
## and 48-hr delta even if the latest station value is delayed by a day or two.
LOOKBACK_DAYS <- 10

## SCAN selected-depth logic.
## Standard SCAN depths are commonly 2, 4, 8, 20, and 40 inches.
## The selected depth is used for map color/symbols. All available depths are
## summarized into popup-friendly fields.
PREFERRED_SCAN_DEPTH_IN <- c(20, 8, 4, 2, 40)

## CDEC snow water content sensor code.
## Sensor 3 is commonly used for snow water content / SWE in inches.
CDEC_SWE_SENSOR <- 3
CDEC_DAILY_INTERVAL <- "D"

## Be polite to services when looping station-by-station.
REQUEST_PAUSE_SEC <- 0.15


# ==== 4. Paths ================================================================

SCAN_STATIONS_RDS <- file.path(DIR$rds, "scan_stations_wgs84.rds")
SNOW_PILLOWS_RDS  <- file.path(DIR$rds, "snow_pillows_wgs84.rds")

OUT_SCAN_RECENT_RDS <- file.path(DIR$rds, "scan_stations_recent_wgs84.rds")
OUT_SNOW_RECENT_RDS <- file.path(DIR$rds, "snow_pillows_recent_wgs84.rds")

OUT_SCAN_QA_CSV <- file.path(DIR$qa, "scan_recent_obs_summary.csv")
OUT_SNOW_QA_CSV <- file.path(DIR$qa, "snow_pillows_recent_obs_summary.csv")
OUT_SCAN_DEPTH_QA_CSV <- file.path(DIR$qa, "scan_recent_soil_moisture_depth_counts.csv")


# ==== 5. Helper functions =====================================================

clean_chr <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  dplyr::na_if(x, "")
}

clean_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

safe_date <- function(x) {
  
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  
  out <- suppressWarnings(as.Date(x))
  
  if (all(is.na(out))) {
    out <- suppressWarnings(lubridate::as_date(lubridate::ymd_hms(x, quiet = TRUE)))
  }
  
  if (all(is.na(out))) {
    out <- suppressWarnings(lubridate::as_date(lubridate::ymd(x, quiet = TRUE)))
  }
  
  out
}

first_existing_col <- function(df, candidates) {
  
  ## Case-insensitive exact column matching.
  ##
  ## This matters because soilDB::fetchSCAN() can return columns such as:
  ##   Site, Date, Time, value, depth, sensor.id, datetime
  ##
  ## while our candidate lists may use lower-case names.
  
  nm <- names(df)
  nm_lower <- tolower(nm)
  cand_lower <- tolower(candidates)
  
  hit_idx <- match(cand_lower, nm_lower)
  hit_idx <- hit_idx[!is.na(hit_idx)]
  
  if (length(hit_idx) == 0) {
    NA_character_
  } else {
    nm[hit_idx[1]]
  }
}

get_col_or_na <- function(df, candidates) {
  nm <- first_existing_col(df, candidates)
  if (is.na(nm)) rep(NA, nrow(df)) else df[[nm]]
}

fmt_num <- function(x, digits = 1) {
  
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x))
  
  ok <- !is.na(x)
  
  out[ok] <- formatC(
    x[ok],
    format = "f",
    digits = digits,
    big.mark = ","
  )
  
  out <- sub("(\\.\\d*?)0+$", "\\1", out)
  out <- sub("\\.$", "", out)
  
  out
}

fmt_signed <- function(x, digits = 1) {
  
  x <- suppressWarnings(as.numeric(x))
  out <- fmt_num(abs(x), digits = digits)
  out <- ifelse(is.na(x), NA_character_, paste0(ifelse(x > 0, "+", ifelse(x < 0, "-", "")), out))
  out
}

extract_soildb_table <- function(x, preferred_name = NULL) {
  
  ## soilDB::fetchSCAN() returns a list of data.frames in current versions.
  ## The desired table is usually named by report, e.g. SMS, WTEQ, SNWD.
  if (inherits(x, "try-error") || is.null(x)) {
    return(tibble::tibble())
  }
  
  if (is.data.frame(x)) {
    return(tibble::as_tibble(x))
  }
  
  if (is.list(x)) {
    
    if (!is.null(preferred_name) && preferred_name %in% names(x) && is.data.frame(x[[preferred_name]])) {
      return(tibble::as_tibble(x[[preferred_name]]))
    }
    
    df_names <- names(x)[vapply(x, is.data.frame, logical(1))]
    df_names <- setdiff(df_names, "metadata")
    
    if (length(df_names) > 0) {
      return(tibble::as_tibble(x[[df_names[1]]]))
    }
  }
  
  tibble::tibble()
}

standardize_soildb_obs <- function(df, value_name = "value") {
  
  ## Return a stable empty schema when standardization fails.
  empty_out <- tibble::tibble(
    station_id = character(),
    obs_date = as.Date(character()),
    value = numeric(),
    depth_cm = numeric()
  )
  
  if (nrow(df) == 0) {
    return(empty_out)
  }
  
  df <- tibble::as_tibble(df)
  
  site_col <- first_existing_col(
    df,
    c(
      "Site",
      "site",
      "site.code",
      "site_code",
      "site_num",
      "siteNum",
      "station",
      "station_id",
      "site_id",
      "sitenum"
    )
  )
  
  date_col <- first_existing_col(
    df,
    c(
      "Date",
      "date",
      "datetime",
      "date_time",
      "reading_date",
      "beginDate",
      "begin_date"
    )
  )
  
  value_col <- first_existing_col(
    df,
    c(
      "value",
      "Value",
      "val",
      "reading",
      "SMS",
      "WTEQ",
      "SNWD"
    )
  )
  
  depth_col <- first_existing_col(
    df,
    c(
      "depth",
      "Depth",
      "sensor_depth_cm",
      "depth_cm",
      "sensorDepth",
      "sensor_depth",
      "soil_depth_cm",
      "sensor_depths"
    )
  )
  
  if (is.na(site_col) || is.na(date_col) || is.na(value_col)) {
    warning(
      "Could not standardize soilDB observation table. Available columns: ",
      paste(names(df), collapse = ", ")
    )
    return(empty_out)
  }
  
  out <- tibble::tibble(
    station_id = clean_chr(df[[site_col]]),
    obs_date = safe_date(df[[date_col]]),
    value = clean_num(df[[value_col]])
  )
  
  if (!is.na(depth_col)) {
    out$depth_cm <- clean_num(df[[depth_col]])
  } else {
    out$depth_cm <- NA_real_
  }
  
  out |>
    dplyr::filter(!is.na(station_id), !is.na(obs_date))
}

latest_and_deltas <- function(df, id_col = "station_id", value_col = "value", date_col = "obs_date") {
  
  if (nrow(df) == 0) {
    return(tibble::tibble())
  }
  
  df <- df |>
    dplyr::filter(!is.na(.data[[value_col]]), !is.na(.data[[date_col]])) |>
    dplyr::arrange(.data[[id_col]], .data[[date_col]])
  
  if (nrow(df) == 0) {
    return(tibble::tibble())
  }
  
  latest_tbl <- df |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::slice_max(.data[[date_col]], n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      station_id = .data[[id_col]],
      latest_date = .data[[date_col]],
      latest_value = .data[[value_col]]
    )
  
  previous_tbl <- df |>
    dplyr::inner_join(
      latest_tbl,
      by = stats::setNames("station_id", id_col)
    ) |>
    dplyr::mutate(
      days_before_latest = as.integer(latest_date - .data[[date_col]])
    )
  
  d1 <- previous_tbl |>
    dplyr::filter(days_before_latest >= 1) |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::slice_min(days_before_latest, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      station_id = .data[[id_col]],
      value_24hr_ref = .data[[value_col]],
      date_24hr_ref = .data[[date_col]]
    )
  
  d2 <- previous_tbl |>
    dplyr::filter(days_before_latest >= 2) |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::slice_min(days_before_latest, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      station_id = .data[[id_col]],
      value_48hr_ref = .data[[value_col]],
      date_48hr_ref = .data[[date_col]]
    )
  
  latest_tbl |>
    dplyr::left_join(d1, by = "station_id") |>
    dplyr::left_join(d2, by = "station_id") |>
    dplyr::mutate(
      delta_24hr = latest_value - value_24hr_ref,
      delta_48hr = latest_value - value_48hr_ref
    )
}


# ==== 6. Read station location files =========================================

if (!file.exists(SCAN_STATIONS_RDS)) {
  stop("Missing SCAN station file: ", SCAN_STATIONS_RDS, call. = FALSE)
}

if (!file.exists(SNOW_PILLOWS_RDS)) {
  stop("Missing snow pillow station file: ", SNOW_PILLOWS_RDS, call. = FALSE)
}

scan_stations <- readRDS(SCAN_STATIONS_RDS)
snow_pillows <- readRDS(SNOW_PILLOWS_RDS)

scan_site_nums <- scan_stations |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::pull(site_num) |>
  clean_chr() |>
  unique() |>
  stats::na.omit() |>
  as.character()

snotel_site_nums <- snow_pillows |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::filter(station_type == "snotel") |>
  dplyr::pull(site_num) |>
  clean_chr() |>
  unique() |>
  stats::na.omit() |>
  as.character()

cdec_ids <- snow_pillows |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::filter(station_type == "cdec_snow_sensor") |>
  dplyr::pull(cdec_id) |>
  clean_chr() |>
  unique() |>
  stats::na.omit() |>
  as.character()

message("SCAN stations to query: ", length(scan_site_nums))
message("SNOTEL stations to query: ", length(snotel_site_nums))
message("CDEC snow sensors to query: ", length(cdec_ids))


# ==== 7. Fetch SCAN soil moisture ============================================

scan_sms_recent <- tibble::tibble()
scan_depth_summary <- tibble::tibble()

if (FETCH_SCAN_SOIL_MOISTURE && length(scan_site_nums) > 0) {
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  message("Fetching recent SCAN soil moisture with soilDB::fetchSCAN(report = 'SMS')...")
  
  scan_sms_raw <- try(
    soilDB::fetchSCAN(
      site.code = scan_site_nums,
      year = current_year,
      report = "SMS",
      timeseries = "Daily",
      tz = "UTC"
    ),
    silent = TRUE
  )
  
  scan_sms_tbl <- extract_soildb_table(scan_sms_raw, preferred_name = "SMS")
  scan_sms_obs <- standardize_soildb_obs(scan_sms_tbl) |>
    dplyr::filter(obs_date >= Sys.Date() - LOOKBACK_DAYS)
  
  if (nrow(scan_sms_obs) > 0) {
    
    ## Convert depth from cm to nearest standard SCAN inch depth.
    ## soilDB docs indicate below-ground sensor depth is converted to cm.
    scan_sms_obs <- scan_sms_obs |>
      dplyr::mutate(
        depth_in_raw = depth_cm / 2.54,
        depth_in = dplyr::case_when(
          is.na(depth_in_raw) ~ NA_real_,
          abs(depth_in_raw - 2)  <= 1 ~ 2,
          abs(depth_in_raw - 4)  <= 1 ~ 4,
          abs(depth_in_raw - 8)  <= 2 ~ 8,
          abs(depth_in_raw - 20) <= 3 ~ 20,
          abs(depth_in_raw - 40) <= 5 ~ 40,
          TRUE ~ round(depth_in_raw)
        )
      )
    
    scan_depth_summary <- scan_sms_obs |>
      dplyr::count(depth_in, name = "obs_count") |>
      dplyr::arrange(depth_in)
    
    latest_by_depth <- scan_sms_obs |>
      dplyr::filter(!is.na(value), !is.na(depth_in)) |>
      dplyr::group_by(station_id, depth_in) |>
      dplyr::slice_max(obs_date, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        depth_label = paste0("sms_", depth_in, "in_pct")
      ) |>
      dplyr::select(station_id, depth_in, depth_label, value, obs_date)
    
    scan_wide_values <- latest_by_depth |>
      dplyr::select(station_id, depth_label, value) |>
      tidyr::pivot_wider(
        names_from = depth_label,
        values_from = value
      )
    
    scan_wide_dates <- latest_by_depth |>
      dplyr::mutate(depth_label = paste0(depth_label, "_date")) |>
      dplyr::select(station_id, depth_label, obs_date) |>
      tidyr::pivot_wider(
        names_from = depth_label,
        values_from = obs_date
      )
    
    selected_depth <- latest_by_depth |>
      dplyr::mutate(
        preferred_rank = match(depth_in, PREFERRED_SCAN_DEPTH_IN)
      ) |>
      dplyr::filter(!is.na(preferred_rank)) |>
      dplyr::group_by(station_id) |>
      dplyr::arrange(preferred_rank, dplyr::desc(obs_date), .by_group = TRUE) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        station_id,
        scan_selected_soil_moisture_depth_in = depth_in,
        scan_selected_soil_moisture_pct = value,
        scan_selected_soil_moisture_date = obs_date
      )
    
    all_depth_text <- latest_by_depth |>
      dplyr::arrange(station_id, depth_in) |>
      dplyr::mutate(
        txt = paste0(
          depth_in,
          ' in: ',
          fmt_num(value, 1),
          '%',
          ' (',
          obs_date,
          ')'
        )
      ) |>
      dplyr::group_by(station_id) |>
      dplyr::summarize(
        scan_soil_moisture_depth_summary = paste(txt, collapse = "; "),
        scan_soil_moisture_depths_available = paste(sort(unique(depth_in)), collapse = ", "),
        .groups = "drop"
      )
    
    scan_sms_recent <- selected_depth |>
      dplyr::left_join(scan_wide_values, by = "station_id") |>
      dplyr::left_join(scan_wide_dates, by = "station_id") |>
      dplyr::left_join(all_depth_text, by = "station_id")
  }
}


# ---- AWDB REST helpers for SNOTEL recent data -------------------------------

fetch_awdb_recent_element <- function(station_triplets,
                                      element_code,
                                      begin_date,
                                      end_date,
                                      chunk_size = 40,
                                      pause_sec = REQUEST_PAUSE_SEC) {
  
  station_triplets <- clean_chr(station_triplets)
  station_triplets <- unique(station_triplets[!is.na(station_triplets)])
  
  empty_out <- tibble::tibble(
    station_id = character(),
    station_triplet = character(),
    obs_date = as.Date(character()),
    value = numeric(),
    element_code = character()
  )
  
  if (length(station_triplets) == 0) {
    return(empty_out)
  }
  
  chunks <- split(
    station_triplets,
    ceiling(seq_along(station_triplets) / chunk_size)
  )
  
  out <- purrr::map(chunks, function(trips) {
    
    Sys.sleep(pause_sec)
    
    awdb_url <- paste0(
      "https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1/data?",
      "stationTriplets=", URLencode(paste(trips, collapse = ","), reserved = TRUE),
      "&elements=", URLencode(element_code, reserved = TRUE),
      "&duration=DAILY",
      "&beginDate=", URLencode(as.character(begin_date), reserved = TRUE),
      "&endDate=", URLencode(as.character(end_date), reserved = TRUE),
      "&periodRef=END"
    )
    
    resp <- try(
      jsonlite::fromJSON(awdb_url, flatten = TRUE),
      silent = TRUE
    )
    
    if (inherits(resp, "try-error") || is.null(resp) || !is.data.frame(resp) || nrow(resp) == 0) {
      return(empty_out)
    }
    
    parse_awdb_data_response(resp, element_code = element_code)
  })
  
  dplyr::bind_rows(out)
}


parse_awdb_data_response <- function(resp, element_code) {
  
  empty_out <- tibble::tibble(
    station_id = character(),
    station_triplet = character(),
    obs_date = as.Date(character()),
    value = numeric(),
    element_code = character()
  )
  
  if (!is.data.frame(resp) || !"stationTriplet" %in% names(resp) || !"data" %in% names(resp)) {
    return(empty_out)
  }
  
  pieces <- vector("list", nrow(resp))
  
  for (i in seq_len(nrow(resp))) {
    
    trip <- as.character(resp$stationTriplet[i])
    station_id <- stringr::str_extract(trip, "^\\d+")
    
    data_i <- resp$data[[i]]
    
    if (!is.data.frame(data_i) || nrow(data_i) == 0 || !"values" %in% names(data_i)) {
      pieces[[i]] <- empty_out
      next
    }
    
    value_pieces <- vector("list", nrow(data_i))
    
    for (j in seq_len(nrow(data_i))) {
      
      vals <- data_i$values[[j]]
      
      if (!is.data.frame(vals) || nrow(vals) == 0) {
        value_pieces[[j]] <- empty_out
        next
      }
      
      vals <- tibble::as_tibble(vals)
      
      date_col <- first_existing_col(vals, c("date", "Date", "datetime", "obsDate"))
      value_col <- first_existing_col(vals, c("value", "Value"))
      
      ## Fallback for odd two-column structures.
      if (is.na(date_col) && ncol(vals) >= 1) {
        date_col <- names(vals)[1]
      }
      
      if (is.na(value_col) && ncol(vals) >= 2) {
        value_col <- names(vals)[2]
      }
      
      if (is.na(date_col) || is.na(value_col)) {
        value_pieces[[j]] <- empty_out
        next
      }
      
      value_pieces[[j]] <- tibble::tibble(
        station_id = station_id,
        station_triplet = trip,
        obs_date = safe_date(vals[[date_col]]),
        value = clean_num(vals[[value_col]]),
        element_code = element_code
      ) |>
        dplyr::filter(!is.na(obs_date))
    }
    
    pieces[[i]] <- dplyr::bind_rows(value_pieces)
  }
  
  dplyr::bind_rows(pieces)
}

# ==== 8. Fetch SNOTEL SWE and snow depth =====================================
##
## Use AWDB REST directly for SNOTEL WTEQ/SNWD.
##
## Why not soilDB::fetchSCAN(report = "WTEQ") here?
##   In testing, soilDB returned many "first five rows are empty: giving up"
##   messages and then 0 records for recent SNOTEL SWE. AWDB REST successfully
##   returns recent WTEQ values by station_triplet for the same stations.

snotel_swe_recent <- tibble::tibble()
snotel_depth_recent <- tibble::tibble()

if (FETCH_SNOTEL_SWE && length(snotel_site_nums) > 0) {
  
  begin_date <- Sys.Date() - LOOKBACK_DAYS
  end_date <- Sys.Date()
  
  snotel_triplets <- snow_pillows |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::filter(
      station_type == "snotel",
      !is.na(station_triplet)
    ) |>
    dplyr::pull(station_triplet) |>
    clean_chr() |>
    unique()
  
  message("Fetching recent SNOTEL SWE with AWDB REST element WTEQ...")
  
  snotel_wteq_obs <- fetch_awdb_recent_element(
    station_triplets = snotel_triplets,
    element_code = "WTEQ",
    begin_date = begin_date,
    end_date = end_date
  )
  
  snotel_swe_recent <- latest_and_deltas(
    snotel_wteq_obs,
    id_col = "station_triplet",
    value_col = "value",
    date_col = "obs_date"
  ) |>
    dplyr::rename(
      latest_swe_in = latest_value,
      latest_swe_date = latest_date,
      swe_delta_24hr_in = delta_24hr,
      swe_delta_48hr_in = delta_48hr,
      swe_value_24hr_ref = value_24hr_ref,
      swe_value_48hr_ref = value_48hr_ref
    )
  
  message("Fetching recent SNOTEL snow depth with AWDB REST element SNWD...")
  
  snotel_snwd_obs <- fetch_awdb_recent_element(
    station_triplets = snotel_triplets,
    element_code = "SNWD",
    begin_date = begin_date,
    end_date = end_date
  )
  
  snotel_depth_recent <- latest_and_deltas(
    snotel_snwd_obs,
    id_col = "station_triplet",
    value_col = "value",
    date_col = "obs_date"
  ) |>
    dplyr::transmute(
      station_id,
      latest_snow_depth_in = latest_value,
      latest_snow_depth_date = latest_date
    )
  
  message("SNOTEL WTEQ rows returned: ", nrow(snotel_wteq_obs))
  message("SNOTEL SNWD rows returned: ", nrow(snotel_snwd_obs))
}

# ==== 9. Fetch CDEC SWE ======================================================

cdec_swe_recent <- tibble::tibble()

if (FETCH_CDEC_SWE && length(cdec_ids) > 0) {
  
  start_date <- Sys.Date() - LOOKBACK_DAYS
  end_date <- Sys.Date()
  
  message("Fetching recent CDEC SWE with sharpshootR::CDECquery(sensor = ", CDEC_SWE_SENSOR, ")...")
  
  cdec_obs_list <- purrr::map(cdec_ids, function(id) {
    
    Sys.sleep(REQUEST_PAUSE_SEC)
    
    out <- try(
      sharpshootR::CDECquery(
        id = id,
        sensor = CDEC_SWE_SENSOR,
        interval = CDEC_DAILY_INTERVAL,
        start = as.character(start_date),
        end = as.character(end_date)
      ),
      silent = TRUE
    )
    
    if (inherits(out, "try-error") || is.null(out) || !is.data.frame(out) || nrow(out) == 0) {
      return(tibble::tibble())
    }
    
    tibble::as_tibble(out)
  })
  
  cdec_obs_raw <- dplyr::bind_rows(cdec_obs_list)
  
  if (nrow(cdec_obs_raw) > 0) {
    
    station_col <- first_existing_col(cdec_obs_raw, c("station_id", "id", "station"))
    value_col <- first_existing_col(cdec_obs_raw, c("value", "VALUE"))
    date_col <- first_existing_col(cdec_obs_raw, c("datetime", "date", "obs_date"))
    
    if (!is.na(station_col) && !is.na(value_col) && !is.na(date_col)) {
      
      cdec_obs <- cdec_obs_raw |>
        dplyr::transmute(
          station_id = clean_chr(.data[[station_col]]),
          obs_date = safe_date(.data[[date_col]]),
          value = clean_num(.data[[value_col]])
        ) |>
        dplyr::filter(!is.na(station_id), !is.na(obs_date))
      
      cdec_swe_recent <- latest_and_deltas(cdec_obs) |>
        dplyr::rename(
          latest_swe_in = latest_value,
          latest_swe_date = latest_date,
          swe_delta_24hr_in = delta_24hr,
          swe_delta_48hr_in = delta_48hr,
          swe_value_24hr_ref = value_24hr_ref,
          swe_value_48hr_ref = value_48hr_ref
        )
    } else {
      warning(
        "Could not standardize CDEC SWE response. Columns were: ",
        paste(names(cdec_obs_raw), collapse = ", ")
      )
    }
  }
}


# ==== 10. Join recent observations to station locations ======================

scan_recent <- scan_stations |>
  dplyr::left_join(
    scan_sms_recent,
    by = c("station_id" = "station_id")
  ) |>
  dplyr::mutate(
    scan_recent_data_status = dplyr::case_when(
      !is.na(scan_selected_soil_moisture_pct) ~ "recent soil moisture available",
      TRUE ~ "no recent soil moisture found"
    )
  )

snow_recent <- snow_pillows |>
  dplyr::mutate(
    join_station_id = dplyr::case_when(
      station_type == "snotel" ~ as.character(station_triplet),
      station_type == "cdec_snow_sensor" ~ as.character(cdec_id),
      TRUE ~ as.character(station_id)
    )
  )

snow_recent_obs <- dplyr::bind_rows(
  snotel_swe_recent |>
    dplyr::mutate(obs_source_system = "NRCS SNOTEL"),
  cdec_swe_recent |>
    dplyr::mutate(obs_source_system = "CDEC")
)

snow_recent <- snow_recent |>
  dplyr::left_join(
    snow_recent_obs,
    by = c("join_station_id" = "station_id")
  ) |>
  dplyr::left_join(
    snotel_depth_recent,
    by = c("join_station_id" = "station_id")
  ) |>
  dplyr::mutate(
    snow_recent_data_status = dplyr::case_when(
      !is.na(latest_swe_in) ~ "recent SWE available",
      TRUE ~ "no recent SWE found"
    ),
    swe_delta_24hr_class = dplyr::case_when(
      is.na(swe_delta_24hr_in) ~ "unknown",
      swe_delta_24hr_in >= 0.5 ~ "increase",
      swe_delta_24hr_in <= -0.5 ~ "decrease",
      TRUE ~ "little/no change"
    ),
    swe_delta_48hr_class = dplyr::case_when(
      is.na(swe_delta_48hr_in) ~ "unknown",
      swe_delta_48hr_in >= 0.5 ~ "increase",
      swe_delta_48hr_in <= -0.5 ~ "decrease",
      TRUE ~ "little/no change"
    )
  ) |>
  dplyr::select(-join_station_id)


# ==== 11. QA summaries =======================================================

scan_qa <- tibble::tibble(
  metric = c(
    "scan_station_rows",
    "scan_rows_with_recent_soil_moisture",
    "scan_rows_without_recent_soil_moisture"
  ),
  value = c(
    nrow(scan_recent),
    sum(!is.na(scan_recent$scan_selected_soil_moisture_pct)),
    sum(is.na(scan_recent$scan_selected_soil_moisture_pct))
  )
)

snow_qa <- tibble::tibble(
  metric = c(
    "snow_pillow_rows",
    "snow_rows_with_recent_swe",
    "snow_rows_without_recent_swe",
    "snotel_rows_with_recent_swe",
    "cdec_rows_with_recent_swe"
  ),
  value = c(
    nrow(snow_recent),
    sum(!is.na(snow_recent$latest_swe_in)),
    sum(is.na(snow_recent$latest_swe_in)),
    sum(snow_recent$station_type == "snotel" & !is.na(snow_recent$latest_swe_in)),
    sum(snow_recent$station_type == "cdec_snow_sensor" & !is.na(snow_recent$latest_swe_in))
  )
)

message("\nSCAN recent observation QA:")
print(scan_qa, width = 1200)

message("\nSCAN soil-moisture depth observation counts:")
print(scan_depth_summary, n = Inf)

message("\nSnow pillow recent observation QA:")
print(snow_qa, width = 1200)

message("\nSnow pillow 24-hour delta class counts:")
print(
  snow_recent |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::count(station_type, swe_delta_24hr_class, name = "n") |>
    dplyr::arrange(station_type, swe_delta_24hr_class),
  n = Inf
)


# ==== 12. Save outputs =======================================================

if (WRITE_QA) {
  readr::write_csv(scan_qa, OUT_SCAN_QA_CSV)
  readr::write_csv(snow_qa, OUT_SNOW_QA_CSV)
  readr::write_csv(scan_depth_summary, OUT_SCAN_DEPTH_QA_CSV)
  
  message("Saved SCAN recent QA: ", OUT_SCAN_QA_CSV)
  message("Saved snow pillow recent QA: ", OUT_SNOW_QA_CSV)
  message("Saved SCAN depth QA: ", OUT_SCAN_DEPTH_QA_CSV)
}

if (WRITE_RDS) {
  saveRDS(scan_recent, OUT_SCAN_RECENT_RDS)
  saveRDS(snow_recent, OUT_SNOW_RECENT_RDS)
  
  message("Saved SCAN recent RDS: ", OUT_SCAN_RECENT_RDS)
  message("Saved snow pillow recent RDS: ", OUT_SNOW_RECENT_RDS)
}


# ==== 13. Final summary ======================================================

message("\nDone: recent snow / soil climate observations refresh complete.")
message("Outputs:")
message("  ", OUT_SCAN_RECENT_RDS)
message("  ", OUT_SNOW_RECENT_RDS)