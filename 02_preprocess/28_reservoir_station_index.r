# ==== 28_reservoir_station_index.r ===========================================
##
## PURPOSE:
##   Build a current CDEC-first reservoir/station index for BRIM.
##
## WHY THIS EXISTS:
##   Reservoir data are central to BRIM.  This script separates stable station
##   identity/crosswalk work from later live or near-live storage feeds.
##
## DESIGN:
##   1. Fetch current CDEC reservoir metadata from active reservoir reports:
##        - HourlyRes
##        - DailyRes
##   2. Treat legacy CSVs in 01_raw_data/cdec/ as optional enrichment and QA,
##      not as the authoritative source of current station metadata.
##   3. Write a clean station index that future CDEC/CNRFC/USGS live-data work
##      can join against.
##
## IMPORTANT:
##   This script does NOT fetch current reservoir storage values.
##   It builds station identity, coordinates, operators, aliases, and portal
##   links.  Current storage should be handled by a separate live/static GeoJSON
##   feed later.
##
## INPUTS:
##   Current web reports:
##     https://cdec.water.ca.gov/reportapp/javareports?name=HourlyRes
##     https://cdec.water.ca.gov/reportapp/javareports?name=DailyRes
##
##   Optional legacy enrichment files:
##     01_raw_data/cdec/cdec_res_table_savedXYs.csv
##     01_raw_data/cdec/cdec_to_nws_key.csv
##     01_raw_data/cdec/cdec_usgs_cnrfc.csv
##     01_raw_data/cdec/cdec_meta_5.22.19.csv
##
## OUTPUTS:
##   04_processed_data/rds/cdec_reservoir_station_index_wgs84.rds
##   04_processed_data/rds/cdec_reservoir_station_index_wgs84_<timestamp>.rds
##   04_processed_data/rds/cdec_reservoir_station_alias_crosswalk.rds
##   04_processed_data/qa/cdec_reservoir_station_index_qa_<timestamp>.csv
##   04_processed_data/qa/cdec_reservoir_station_coordinate_qa_<timestamp>.csv
##   04_processed_data/qa/cdec_reservoir_station_legacy_compare_<timestamp>.csv
## ============================================================================

# ==== 1. Load configuration and helper functions =============================

source("00_config/config_paths.r")
source("00_config/config_run_flags.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(sf)
})

# ==== 3. User-facing switches ================================================

WRITE_GPKG <- TRUE
WRITE_QA   <- TRUE

RUN_TS <- make_timestamp()

# ==== 4. Define paths ========================================================

raw_cdec_dir <- file.path(DIR$raw, "cdec")

legacy_paths <- list(
  mid_res_xy  = file.path(raw_cdec_dir, "cdec_res_table_savedXYs.csv"),
  cdec_nws    = file.path(raw_cdec_dir, "cdec_to_nws_key.csv"),
  usgs_cnrfc  = file.path(raw_cdec_dir, "cdec_usgs_cnrfc.csv"),
  legacy_meta = file.path(raw_cdec_dir, "cdec_meta_5.22.19.csv")
)

out_index_latest <- file.path(
  DIR$rds,
  "cdec_reservoir_station_index_wgs84.rds"
)

out_index_timestamped <- file.path(
  DIR$rds,
  timestamped_name("cdec_reservoir_station_index_wgs84", "rds", RUN_TS)
)

out_alias_latest <- file.path(
  DIR$rds,
  "cdec_reservoir_station_alias_crosswalk.rds"
)

out_cdec_cnrfc_crosswalk_latest <- file.path(
  DIR$rds,
  "cdec_cnrfc_reservoir_crosswalk.rds"
)

## General CDEC<->CNRFC station crosswalk used for popup enrichment.
## This is broader than the reservoir-only crosswalk and can include river
## stations such as CDEC SCO <-> CNRFC/NWS SCOC1.  It is enrichment only; it
## does not create station bones for the CDEC reservoir index.
out_cdec_cnrfc_station_crosswalk_latest <- file.path(
  DIR$rds,
  "cdec_cnrfc_station_crosswalk.rds"
)

out_cdec_cnrfc_crosswalk_qa <- file.path(
  DIR$qa,
  paste0("cdec_cnrfc_reservoir_crosswalk_qa_", RUN_TS, ".csv")
)

out_cdec_cnrfc_station_crosswalk_qa <- file.path(
  DIR$qa,
  paste0("cdec_cnrfc_station_crosswalk_qa_", RUN_TS, ".csv")
)

out_gpkg <- file.path(
  DIR$gpkg,
  "cdec_reservoir_station_index.gpkg"
)

out_qa <- file.path(
  DIR$qa,
  paste0("cdec_reservoir_station_index_qa_", RUN_TS, ".csv")
)

out_coord_qa <- file.path(
  DIR$qa,
  paste0("cdec_reservoir_station_coordinate_qa_", RUN_TS, ".csv")
)

out_compare <- file.path(
  DIR$qa,
  paste0("cdec_reservoir_station_legacy_compare_", RUN_TS, ".csv")
)

# ==== 5. Small utility helpers ===============================================

pt_na_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_collapse_unique <- function(x) {
  x <- pt_na_chr(x)
  x <- sort(unique(x[!is.na(x)]))
  if (length(x) == 0) NA_character_ else paste(x, collapse = "; ")
}

pt_clean_name <- function(x) {
  x <- pt_na_chr(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_to_title(x)
  x
}

pt_norm_station_id <- function(x) {
  x <- toupper(pt_na_chr(x))
  x <- gsub("[^A-Z0-9]", "", x)
  x[x == ""] <- NA_character_
  x
}

pt_norm_res_name <- function(x) {
  ## Normalize reservoir/station names only for QA/crosswalk matching.  This is
  ## intentionally lossy and should never replace the original names in outputs.
  x <- tolower(pt_na_chr(x))
  x <- stringr::str_replace_all(x, "\\([^)]*\\)", " ")
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", " ")
  x <- stringr::str_squish(x)

  ## Remove common generic words that differ across CDEC/CNRFC labels.
  stop_words <- c(
    "lake", "reservoir", "rsvr", "dam", "forebay", "afterbay",
    "near", "nr", "at", "above", "below", "blw", "blo", "upper", "lower",
    "north", "south", "east", "west", "fork", "river", "creek", "canal",
    "usbr", "dwr", "avg", "24hr", "24", "hr"
  )

  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) return(NA_character_)
    toks <- unlist(strsplit(s, "\\s+"), use.names = FALSE)
    toks <- toks[!(toks %in% stop_words)]
    toks <- toks[nchar(toks) > 1]
    toks <- unique(toks)
    if (length(toks) == 0) return(NA_character_)
    paste(sort(toks), collapse = " ")
  }, character(1))
}

pt_token_score <- function(a, b) {
  a <- pt_norm_res_name(a)
  b <- pt_norm_res_name(b)

  vapply(seq_along(a), function(i) {
    if (is.na(a[i]) || is.na(b[i])) return(0)
    aa <- unlist(strsplit(a[i], "\\s+"), use.names = FALSE)
    bb <- unlist(strsplit(b[i], "\\s+"), use.names = FALSE)
    if (length(aa) == 0 || length(bb) == 0) return(0)
    length(intersect(aa, bb)) / length(unique(c(aa, bb)))
  }, numeric(1))
}

pt_read_csv_optional <- function(path, label) {
  if (!file.exists(path)) {
    message("Optional legacy ", label, " file not found; skipping: ", path)
    return(NULL)
  }
  message("Reading optional legacy ", label, ": ", path)
  readr::read_csv(path, show_col_types = FALSE, guess_max = 10000)
}


pt_valid_lonlat <- function(lat, lon) {
  lat <- suppressWarnings(as.numeric(lat))
  lon <- suppressWarnings(as.numeric(lon))

  is.finite(lat) &
    is.finite(lon) &
    lat >= -90 & lat <= 90 &
    lon >= -180 & lon <= 180
}

pt_valid_brim_lonlat <- function(lat, lon) {
  ## BRIM is California-centered, but useful water-resource context can extend
  ## modestly beyond California.  These broad western-US bounds catch obvious
  ## placeholder values such as 99.999 / -999.999 without rejecting normal
  ## nearby CDEC stations.
  lat <- suppressWarnings(as.numeric(lat))
  lon <- suppressWarnings(as.numeric(lon))

  pt_valid_lonlat(lat, lon) &
    lat >= 30 & lat <= 43 &
    lon >= -126 & lon <= -113
}

pt_is_cdec_summary_record <- function(cdec_id, reservoir_name) {
  cdec_id <- toupper(pt_na_chr(cdec_id))
  nm <- tolower(pt_na_chr(reservoir_name))

  dplyr::coalesce(
    cdec_id %in% c("SWV", "SJT") |
      grepl("statewide.*storage|storage.*estimate|\\btotal\\b", nm),
    FALSE
  )
}

# ==== 6. Robust simple downloader ============================================

pt_fetch_text <- function(url, label = url, timeout_sec = 30, retries = 3) {
  message("Fetching ", label, ": ", url)
  
  user_agent <- paste(
    "Mozilla/5.0",
    "BRIM reservoir station index builder",
    "R",
    getRversion()
  )
  
  last_error <- NULL
  
  for (attempt in seq_len(retries)) {
    message("  attempt ", attempt, " of ", retries)
    
    txt <- tryCatch({
      if (requireNamespace("curl", quietly = TRUE)) {
        h <- curl::new_handle(
          useragent = user_agent,
          followlocation = TRUE,
          timeout = timeout_sec,
          connecttimeout = timeout_sec,
          httpheader = c(
            "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
            "Cache-Control" = "no-cache",
            "Pragma" = "no-cache"
          )
        )
        raw <- curl::curl_fetch_memory(url, handle = h)$content
        rawToChar(raw)
      } else {
        con <- base::url(url, open = "rb")
        on.exit(close(con), add = TRUE)
        raw <- readBin(con, what = "raw", n = 10^8)
        rawToChar(raw)
      }
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      NULL
    })
    
    if (!is.null(txt) && nzchar(txt)) {
      return(txt)
    }
    
    Sys.sleep(1 + attempt)
  }
  
  stop("Could not fetch ", label, ". Last error: ", last_error)
}

# ==== 7. Parse CDEC active reservoir reports =================================

pt_html_to_lines <- function(html) {
  txt <- html
  txt <- gsub("(?i)<br\\s*/?>", "\n", txt, perl = TRUE)
  txt <- gsub("(?i)</tr>|</p>|</div>|</li>", "\n", txt, perl = TRUE)
  txt <- gsub("<[^>]+>", " ", txt)

  ## CDEC report pages sometimes emit non-breaking spaces as both
  ## ``&nbsp;`` and ``&nbsp`` (without the semicolon).  The no-semicolon
  ## form prevented the row parser from seeing normal whitespace between
  ## longitude, county, and operator fields.  Normalize both forms before
  ## line squishing and regex parsing.
  txt <- gsub("(?i)&nbsp;?", " ", txt, perl = TRUE)
  txt <- gsub("(?i)&(ensp|emsp);?", " ", txt, perl = TRUE)
  txt <- gsub("&#160;", " ", txt, fixed = TRUE)
  txt <- gsub("&#xa0;", " ", txt, ignore.case = TRUE)
  txt <- gsub("&amp;", "&", txt, fixed = TRUE)
  txt <- gsub("&quot;", "\"", txt, fixed = TRUE)
  txt <- gsub("&#39;", "'", txt, fixed = TRUE)
  txt <- gsub("\u00a0", " ", txt, fixed = TRUE)
  
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
  lines <- stringr::str_squish(lines)
  lines[nzchar(lines)]
}

pt_ca_counties_upper <- toupper(c(
  "Alameda", "Alpine", "Amador", "Butte", "Calaveras", "Colusa",
  "Contra Costa", "Del Norte", "El Dorado", "Fresno", "Glenn", "Humboldt",
  "Imperial", "Inyo", "Kern", "Kings", "Lake", "Lassen", "Los Angeles",
  "Madera", "Marin", "Mariposa", "Mendocino", "Merced", "Modoc", "Mono",
  "Monterey", "Napa", "Nevada", "Orange", "Placer", "Plumas", "Riverside",
  "Sacramento", "San Benito", "San Bernardino", "San Diego", "San Francisco",
  "San Joaquin", "San Luis Obispo", "San Mateo", "Santa Barbara", "Santa Clara",
  "Santa Cruz", "Shasta", "Sierra", "Siskiyou", "Solano", "Sonoma", "Stanislaus",
  "Sutter", "Tehama", "Trinity", "Tulare", "Tuolumne", "Ventura", "Yolo", "Yuba"
))

pt_split_county_operator <- function(rest) {
  rest <- stringr::str_squish(rest)
  
  ## Prefer the longest county names first so SAN LUIS OBISPO wins before SAN.
  counties <- pt_ca_counties_upper[order(nchar(pt_ca_counties_upper), decreasing = TRUE)]
  
  for (cty in counties) {
    pattern <- paste0("^", stringr::str_replace_all(cty, " ", "\\\\s+"), "\\b")
    if (grepl(pattern, rest, ignore.case = FALSE, perl = TRUE)) {
      operator <- trimws(sub(pattern, "", rest, perl = TRUE))
      return(list(county = stringr::str_to_title(cty), operator_agency = operator))
    }
  }
  
  list(county = NA_character_, operator_agency = rest)
}

pt_is_basin_header <- function(line) {
  if (!nzchar(line)) return(FALSE)
  if (grepl("ACTIVE|REPORT|GENERATED|SORTED|STATION|LATITUDE|LONGITUDE|COUNTY|OPERATOR|FEET|SKIP|MENU|SEARCH", line, ignore.case = TRUE)) {
    return(FALSE)
  }
  
  ## Basin headers in the CDEC report are uppercase river/basin labels.
  has_letters <- grepl("[A-Z]", line)
  mostly_upper <- line == toupper(line)
  short_enough <- nchar(line) <= 55
  has_letters && mostly_upper && short_enough
}

pt_parse_cdec_res_report <- function(html, report_name, frequency_label) {
  lines <- pt_html_to_lines(html)

  ## Drop obvious non-report/navigation lines before row parsing.  This makes
  ## the parser less sensitive to the surrounding CDEC web-template text.
  lines <- lines[!grepl("^(Menu|Contact|Search|Home|Query Tools|Reservoirs|Stations|Weather|Skip to|Saving your location)", lines, ignore.case = TRUE)]
  
  current_basin <- NA_character_
  out <- list()
  
  ## CDEC report rows currently render as:
  ##
  ##   SHASTA DAM (USBR)SHA 1067 40.717999-122.419998   SHASTA   US Bureau...
  ##
  ## The station-name anchor can be stripped without a space before the CDEC
  ## station ID (for example, "(USBR)SHA").  The parser therefore allows zero
  ## or more spaces between the station name and the 2- to 5-character CDEC ID.
  ## It also allows zero or more spaces between latitude and longitude because
  ## CDEC commonly prints them as "40.717999-122.419998".
  row_pattern <- paste0(
    "^(.+?)\\s*",                    # station name; may touch station ID
    "([A-Z0-9]{2,5})\\s+",           # CDEC station id
    "(-?\\d{1,5})\\s+",            # elevation
    "(-?\\d{1,2}\\.\\d{3,6})\\s*", # latitude
    "(-?\\d{2,3}\\.\\d{3,6})\\s+", # longitude
    "(.+)$"                           # county + operator agency
  )
  
  for (line in lines) {
    m <- stringr::str_match(line, row_pattern)
    
    if (!all(is.na(m))) {
      county_operator <- pt_split_county_operator(m[, 7])
      
      out[[length(out) + 1]] <- tibble::tibble(
        cdec_id = m[, 3],
        reservoir_name = pt_clean_name(m[, 2]),
        cdec_station_name = stringr::str_squish(m[, 2]),
        elevation_ft = suppressWarnings(as.numeric(m[, 4])),
        latitude = suppressWarnings(as.numeric(m[, 5])),
        longitude = suppressWarnings(as.numeric(m[, 6])),
        county = county_operator$county,
        operator_agency = county_operator$operator_agency,
        river_basin_cdec = current_basin,
        cdec_report = report_name,
        cdec_frequency = frequency_label
      )
      next
    }
    
    if (pt_is_basin_header(line)) {
      current_basin <- stringr::str_to_title(line)
    }
  }
  
  if (length(out) == 0) {
    sample_lines <- utils::head(lines[grepl("\\d{1,2}\\.\\d{3,6}\\s*-?\\d{2,3}\\.\\d{3,6}", lines)], 8)
    warning(
      "No station rows parsed from CDEC report: ", report_name,
      if (length(sample_lines) > 0) paste0(". Sample coordinate-bearing lines: ", paste(sample_lines, collapse = " | ")) else ""
    )
    return(tibble::tibble())
  }
  
  dplyr::bind_rows(out) |>
    dplyr::distinct(.data$cdec_id, .data$cdec_report, .keep_all = TRUE)
}

# ==== 8. Fetch and combine current CDEC metadata =============================

hourly_url <- "https://cdec.water.ca.gov/reportapp/javareports?name=HourlyRes"
daily_url  <- "https://cdec.water.ca.gov/reportapp/javareports?name=DailyRes"

hourly_html <- pt_fetch_text(hourly_url, label = "CDEC HourlyRes active reservoir report")
daily_html  <- pt_fetch_text(daily_url,  label = "CDEC DailyRes active reservoir report")

cdec_hourly <- pt_parse_cdec_res_report(
  hourly_html,
  report_name = "HourlyRes",
  frequency_label = "hourly"
)

cdec_daily <- pt_parse_cdec_res_report(
  daily_html,
  report_name = "DailyRes",
  frequency_label = "daily"
)

message("CDEC hourly reservoir rows parsed: ", nrow(cdec_hourly))
message("CDEC daily reservoir rows parsed:  ", nrow(cdec_daily))

if (nrow(cdec_hourly) == 0 && nrow(cdec_daily) == 0) {
  stop("No current CDEC reservoir metadata could be parsed. Check CDEC report format or network access.")
}

cdec_current_all <- dplyr::bind_rows(cdec_hourly, cdec_daily) |>
  dplyr::mutate(
    cdec_id = toupper(.data$cdec_id),
    source_rank = dplyr::case_when(
      .data$cdec_frequency == "hourly" ~ 1L,
      .data$cdec_frequency == "daily"  ~ 2L,
      TRUE ~ 9L
    )
  )

cdec_current <- cdec_current_all |>
  dplyr::arrange(.data$cdec_id, .data$source_rank) |>
  dplyr::group_by(.data$cdec_id) |>
  dplyr::summarise(
    reservoir_name = dplyr::first(.data$reservoir_name),
    cdec_station_name = dplyr::first(.data$cdec_station_name),
    elevation_ft = dplyr::first(.data$elevation_ft),
    latitude = dplyr::first(.data$latitude),
    longitude = dplyr::first(.data$longitude),
    county = dplyr::first(.data$county),
    operator_agency = dplyr::first(.data$operator_agency),
    river_basin_cdec = dplyr::first(.data$river_basin_cdec),
    has_hourly_reservoir_report = any(.data$cdec_frequency == "hourly"),
    has_daily_reservoir_report = any(.data$cdec_frequency == "daily"),
    cdec_metadata_source = paste(sort(unique(.data$cdec_report)), collapse = "; "),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    coord_source = "current CDEC active reservoir report",
    cdec_station_url = paste0(
      "https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=",
      .data$cdec_id
    ),
    cdec_sensor15_hourly_url = paste0(
      "https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=",
      .data$cdec_id,
      "&SensorNums=15&dur_code=H&Start=&End=now"
    ),
    cdec_sensor15_daily_url = paste0(
      "https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=",
      .data$cdec_id,
      "&SensorNums=15&dur_code=D&Start=&End=now"
    ),
    cdec_latest_storage_table_url = "https://cdec.water.ca.gov/dynamicapp/getAll?sens_num=15"
  )

# ==== 9. Read optional legacy enrichment/crosswalk files =====================

legacy_mid_xy <- pt_read_csv_optional(legacy_paths$mid_res_xy, "reservoir midpoint XY")
legacy_cdec_nws <- pt_read_csv_optional(legacy_paths$cdec_nws, "CDEC-to-NWS key")
legacy_usgs_cnrfc <- pt_read_csv_optional(legacy_paths$usgs_cnrfc, "CDEC/USGS/CNRFC crosswalk")
legacy_meta <- pt_read_csv_optional(legacy_paths$legacy_meta, "CDEC 2019 station metadata")

# ---- 9.1 Legacy reservoir midpoint coordinates ------------------------------

legacy_mid_summary <- if (!is.null(legacy_mid_xy)) {
  legacy_mid_xy |>
    dplyr::transmute(
      cdec_id = toupper(as.character(.data$res_id_cdec)),
      legacy_reservoir_name_mid_xy = as.character(.data$res_name_cdec),
      legacy_midres_lat = suppressWarnings(as.numeric(.data$lat_middleres_wgs84)),
      legacy_midres_lon = suppressWarnings(as.numeric(.data$lon_middleres_wgs84))
    ) |>
    dplyr::distinct(.data$cdec_id, .keep_all = TRUE)
} else {
  tibble::tibble(
    cdec_id = character(),
    legacy_reservoir_name_mid_xy = character(),
    legacy_midres_lat = numeric(),
    legacy_midres_lon = numeric()
  )
}

# ---- 9.2 Legacy CDEC-to-NWS crosswalk ---------------------------------------

legacy_nws_summary <- if (!is.null(legacy_cdec_nws)) {
  legacy_cdec_nws |>
    dplyr::transmute(
      cdec_id = toupper(as.character(.data$id_cdec)),
      nws_id = toupper(as.character(.data$id_nws)),
      legacy_name_cdec_nws = as.character(.data$name_cdec)
    ) |>
    dplyr::filter(!is.na(.data$cdec_id), .data$cdec_id != "") |>
    dplyr::group_by(.data$cdec_id) |>
    dplyr::summarise(
      legacy_nws_ids = pt_collapse_unique(.data$nws_id),
      legacy_cdec_names_from_nws_key = pt_collapse_unique(.data$legacy_name_cdec_nws),
      .groups = "drop"
    )
} else {
  tibble::tibble(
    cdec_id = character(),
    legacy_nws_ids = character(),
    legacy_cdec_names_from_nws_key = character()
  )
}

# ---- 9.3 Legacy CDEC/USGS/CNRFC crosswalk -----------------------------------

legacy_usgs_summary <- if (!is.null(legacy_usgs_cnrfc)) {
  legacy_usgs_cnrfc |>
    dplyr::transmute(
      cdec_id = toupper(as.character(.data$id_cdec)),
      nws_id_from_crosswalk = toupper(as.character(.data$id_nws)),
      usgs_id = as.character(.data$id_usgs),
      legacy_name_cdec_crosswalk = as.character(.data$name_cdec),
      legacy_name_usgs = as.character(.data$name_usgs),
      legacy_basin_cdec = as.character(.data$basin_cdec),
      legacy_group_cdec = as.character(.data$group_cdec),
      legacy_gage_operator = as.character(.data$gage_oper_cdec),
      legacy_county_cdec = as.character(.data$county_cdec),
      legacy_station_lat = suppressWarnings(as.numeric(.data$lat_cdec)),
      legacy_station_lon = suppressWarnings(as.numeric(.data$lon_cdec))
    ) |>
    dplyr::filter(!is.na(.data$cdec_id), .data$cdec_id != "") |>
    dplyr::group_by(.data$cdec_id) |>
    dplyr::summarise(
      legacy_nws_ids_from_crosswalk = pt_collapse_unique(.data$nws_id_from_crosswalk),
      legacy_usgs_ids = pt_collapse_unique(.data$usgs_id),
      legacy_cdec_names_from_crosswalk = pt_collapse_unique(.data$legacy_name_cdec_crosswalk),
      legacy_usgs_names = pt_collapse_unique(.data$legacy_name_usgs),
      legacy_basin_cdec = pt_collapse_unique(.data$legacy_basin_cdec),
      legacy_group_cdec = pt_collapse_unique(.data$legacy_group_cdec),
      legacy_gage_operator = pt_collapse_unique(.data$legacy_gage_operator),
      legacy_county_cdec = pt_collapse_unique(.data$legacy_county_cdec),
      legacy_station_lat = dplyr::first(.data$legacy_station_lat[!is.na(.data$legacy_station_lat)]),
      legacy_station_lon = dplyr::first(.data$legacy_station_lon[!is.na(.data$legacy_station_lon)]),
      .groups = "drop"
    )
} else {
  tibble::tibble(
    cdec_id = character(),
    legacy_nws_ids_from_crosswalk = character(),
    legacy_usgs_ids = character(),
    legacy_cdec_names_from_crosswalk = character(),
    legacy_usgs_names = character(),
    legacy_basin_cdec = character(),
    legacy_group_cdec = character(),
    legacy_gage_operator = character(),
    legacy_county_cdec = character(),
    legacy_station_lat = numeric(),
    legacy_station_lon = numeric()
  )
}

# ---- 9.4 Legacy 2019 CDEC metadata ------------------------------------------

legacy_meta_summary <- if (!is.null(legacy_meta)) {
  legacy_meta |>
    dplyr::transmute(
      cdec_id = toupper(as.character(.data$id_cdec)),
      legacy_meta_name_cdec = as.character(.data$name_cdec),
      legacy_meta_basin_cdec = as.character(.data$basin_cdec),
      legacy_meta_group_cdec = as.character(.data$group_cdec),
      legacy_meta_operator = as.character(.data$gage_oper_cdec),
      legacy_meta_county = as.character(.data$county_cdec),
      legacy_meta_lat = suppressWarnings(as.numeric(.data$lat_cdec)),
      legacy_meta_lon = suppressWarnings(as.numeric(.data$lon_cdec)),
      legacy_meta_elev_ft = suppressWarnings(as.numeric(.data$gelev_ft_cdec))
    ) |>
    dplyr::filter(!is.na(.data$cdec_id), .data$cdec_id != "") |>
    dplyr::distinct(.data$cdec_id, .keep_all = TRUE)
} else {
  tibble::tibble(
    cdec_id = character(),
    legacy_meta_name_cdec = character(),
    legacy_meta_basin_cdec = character(),
    legacy_meta_group_cdec = character(),
    legacy_meta_operator = character(),
    legacy_meta_county = character(),
    legacy_meta_lat = numeric(),
    legacy_meta_lon = numeric(),
    legacy_meta_elev_ft = numeric()
  )
}

# ==== 10. Build reconciled station index =====================================

index_tbl <- cdec_current |>
  dplyr::left_join(legacy_mid_summary, by = "cdec_id") |>
  dplyr::left_join(legacy_nws_summary, by = "cdec_id") |>
  dplyr::left_join(legacy_usgs_summary, by = "cdec_id") |>
  dplyr::left_join(legacy_meta_summary, by = "cdec_id") |>
  dplyr::mutate(
    ## Combined aliases are for display/search only; the source-specific fields
    ## above are retained for QA.
    alias_names = purrr::pmap_chr(
      list(
        .data$cdec_station_name,
        .data$legacy_reservoir_name_mid_xy,
        .data$legacy_cdec_names_from_nws_key,
        .data$legacy_cdec_names_from_crosswalk,
        .data$legacy_usgs_names,
        .data$legacy_meta_name_cdec
      ),
      function(...) pt_collapse_unique(unlist(list(...)))
    ),
    nws_id = dplyr::coalesce(.data$legacy_nws_ids, .data$legacy_nws_ids_from_crosswalk),
    usgs_id = .data$legacy_usgs_ids,
    has_legacy_midres_xy = !is.na(.data$legacy_midres_lat) & !is.na(.data$legacy_midres_lon),
    has_legacy_nws_crosswalk = !is.na(.data$nws_id),
    has_legacy_usgs_crosswalk = !is.na(.data$usgs_id),
    cnrfc_reservoir_inflow_url = dplyr::if_else(
      !is.na(.data$nws_id) & !grepl(";", .data$nws_id),
      paste0("https://www.cnrfc.noaa.gov/reservoir.php?id=", .data$nws_id),
      NA_character_
    ),
    cnrfc_reservoir_outflow_url = dplyr::if_else(
      !is.na(.data$nws_id) & !grepl(";", .data$nws_id),
      paste0("https://www.cnrfc.noaa.gov/reservoirRelease.php?id=", .data$nws_id),
      NA_character_
    ),
    usgs_site_url = dplyr::if_else(
      !is.na(.data$usgs_id) & !grepl(";", .data$usgs_id),
      paste0("https://waterdata.usgs.gov/monitoring-location/", .data$usgs_id),
      NA_character_
    ),
    index_build_timestamp = RUN_TS,

    ## Coordinate and station-type QA ----------------------------------------
    ##
    ## CDEC active-reservoir reports can include summary records, not just
    ## physical reservoir stations.  For example, statewide or basin-total
    ## summaries may have placeholder coordinates or representative coordinates
    ## that should not become mapped station points.  Keep them in the table
    ## for auditability, but exclude them from the sf geometry output.
    is_cdec_summary_record = pt_is_cdec_summary_record(.data$cdec_id, .data$reservoir_name),
    coord_valid_wgs84 = pt_valid_lonlat(.data$latitude, .data$longitude),
    coord_valid_brim_region = pt_valid_brim_lonlat(.data$latitude, .data$longitude),
    map_geometry_ok = .data$coord_valid_brim_region & !.data$is_cdec_summary_record,
    coord_qa_note = dplyr::case_when(
      .data$is_cdec_summary_record ~ "CDEC summary/total record; retained in index table but not mapped as a station point.",
      !.data$coord_valid_wgs84 ~ "Invalid WGS84 coordinate values; retained in index table but not mapped.",
      !.data$coord_valid_brim_region ~ "Coordinate outside broad BRIM western-US mapping bounds; retained in index table but not mapped.",
      TRUE ~ "Mapped using current CDEC active-reservoir report coordinates."
    ),

    data_quality_note = dplyr::case_when(
      has_hourly_reservoir_report ~ "Current CDEC HourlyRes metadata; legacy fields are enrichment/QA only.",
      has_daily_reservoir_report ~ "Current CDEC DailyRes metadata; legacy fields are enrichment/QA only.",
      TRUE ~ "Legacy-only record; review before use."
    )
  ) |>
  dplyr::arrange(.data$reservoir_name, .data$cdec_id)

## Convert to sf using current CDEC station/dam coordinates.
index_sf <- index_tbl |>
  dplyr::filter(.data$map_geometry_ok) |>
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  clean_sf_for_leaflet()

# ==== 11. Build alias/crosswalk table ========================================

alias_tbl <- index_tbl |>
  dplyr::select(
    dplyr::any_of(c(
      "cdec_id",
      "reservoir_name",
      "cdec_station_name",
      "nws_id",
      "usgs_id",
      "alias_names",
      "has_hourly_reservoir_report",
      "has_daily_reservoir_report",
      "has_legacy_midres_xy",
      "has_legacy_nws_crosswalk",
      "has_legacy_usgs_crosswalk",
      "is_cdec_summary_record",
      "map_geometry_ok",
      "coord_qa_note",
      "cdec_station_url",
      "cdec_sensor15_hourly_url",
      "cdec_sensor15_daily_url",
      "cnrfc_reservoir_inflow_url",
      "cnrfc_reservoir_outflow_url",
      "usgs_site_url"
    ))
  )


# ==== 11.1 Build CDEC-CNRFC station and reservoir crosswalks =================
##
## PURPOSE:
##   Build CDEC<->CNRFC enrichment tables for BRIM popups and future live-data
##   joins.
##
## DESIGN PRINCIPLES:
##   - The reservoir station index above remains CDEC-current-first.  Current
##     CDEC HourlyRes/DailyRes metadata are still the station-index backbone.
##   - Legacy CDEC/NWS/USGS CSVs are used here only as crosswalk/enrichment
##     evidence.  They are not treated as complete and they do not create new
##     CDEC reservoir-index bones.
##   - The general station crosswalk can include rivers as well as reservoirs
##     because CNRFC river/reservoir popups benefit from CDEC portal links too.
##   - USGS links are intentionally not joined to CNRFC popups here.  The USGS
##     point layers are large and separate, and this patch keeps that system
##     untouched.
##
## OUTPUTS:
##   cdec_cnrfc_station_crosswalk.rds      = general popup enrichment table
##   cdec_cnrfc_reservoir_crosswalk.rds    = reservoir-focused subset retained
##                                         for reservoir workflows/compatibility

cnrfc_stream_rds_path <- file.path(
  DIR$rds,
  "CNRFC_allstreamgages_mostlyCaonly_wgs84.rds"
)

pt_split_semicolon_ids <- function(x) {
  x <- pt_na_chr(x)
  if (all(is.na(x))) return(character(0))
  out <- unlist(strsplit(paste(x[!is.na(x)], collapse = ";"), ";"), use.names = FALSE)
  out <- pt_norm_station_id(out)
  sort(unique(out[!is.na(out)]))
}

## ---- 11.1A General direct CDEC<->CNRFC key rows ----------------------------
##
## These rows come from the legacy key/crosswalk files and are the most useful
## source for river stations.  Example: CDEC SCO <-> CNRFC SCOC1.

station_key_rows <- tibble::tibble()

if (!is.null(legacy_cdec_nws)) {
  station_key_rows <- dplyr::bind_rows(
    station_key_rows,
    legacy_cdec_nws |>
      dplyr::transmute(
        cdec_id = pt_norm_station_id(.data$id_cdec),
        nws_id = pt_norm_station_id(.data$id_nws),
        cdec_station_name_key = as.character(.data$name_cdec),
        crosswalk_source = "legacy_cdec_to_nws_key"
      )
  )
}

if (!is.null(legacy_usgs_cnrfc)) {
  station_key_rows <- dplyr::bind_rows(
    station_key_rows,
    legacy_usgs_cnrfc |>
      dplyr::transmute(
        cdec_id = pt_norm_station_id(.data$id_cdec),
        nws_id = pt_norm_station_id(.data$id_nws),
        cdec_station_name_key = as.character(.data$name_cdec),
        crosswalk_source = "legacy_cdec_usgs_cnrfc_key"
      )
  )
}

station_key_rows <- station_key_rows |>
  dplyr::filter(
    !is.na(.data$cdec_id), .data$cdec_id != "",
    !is.na(.data$nws_id), .data$nws_id != ""
  ) |>
  dplyr::group_by(.data$nws_id, .data$cdec_id) |>
  dplyr::summarise(
    cdec_station_name_key = pt_collapse_unique(.data$cdec_station_name_key),
    crosswalk_source = pt_collapse_unique(.data$crosswalk_source),
    .groups = "drop"
  )

## Small CDEC station-name lookup for crosswalk display.  Current reservoir
## metadata win where available; legacy metadata fill river/non-reservoir names.
cdec_station_name_lookup <- dplyr::bind_rows(
  index_tbl |>
    dplyr::transmute(
      cdec_id = .data$cdec_id,
      cdec_station_name_lookup = .data$cdec_station_name,
      cdec_lookup_source = "current_cdec_reservoir_report"
    ),
  if (!is.null(legacy_meta)) {
    legacy_meta |>
      dplyr::transmute(
        cdec_id = pt_norm_station_id(.data$id_cdec),
        cdec_station_name_lookup = as.character(.data$name_cdec),
        cdec_lookup_source = "legacy_cdec_meta_2019"
      )
  } else {
    tibble::tibble(cdec_id = character(), cdec_station_name_lookup = character(), cdec_lookup_source = character())
  },
  if (!is.null(legacy_usgs_cnrfc)) {
    legacy_usgs_cnrfc |>
      dplyr::transmute(
        cdec_id = pt_norm_station_id(.data$id_cdec),
        cdec_station_name_lookup = as.character(.data$name_cdec),
        cdec_lookup_source = "legacy_cdec_usgs_cnrfc"
      )
  } else {
    tibble::tibble(cdec_id = character(), cdec_station_name_lookup = character(), cdec_lookup_source = character())
  },
  if (!is.null(legacy_cdec_nws)) {
    legacy_cdec_nws |>
      dplyr::transmute(
        cdec_id = pt_norm_station_id(.data$id_cdec),
        cdec_station_name_lookup = as.character(.data$name_cdec),
        cdec_lookup_source = "legacy_cdec_to_nws"
      )
  } else {
    tibble::tibble(cdec_id = character(), cdec_station_name_lookup = character(), cdec_lookup_source = character())
  }
) |>
  dplyr::filter(!is.na(.data$cdec_id), .data$cdec_id != "") |>
  dplyr::group_by(.data$cdec_id) |>
  dplyr::summarise(
    cdec_station_name_lookup = pt_collapse_unique(.data$cdec_station_name_lookup),
    cdec_lookup_source = pt_collapse_unique(.data$cdec_lookup_source),
    .groups = "drop"
  )

## Current CNRFC point context, used only to keep/display current CNRFC point
## attributes and to classify whether a match is river/reservoir/special.
cnrfc_current_context <- tibble::tibble()

if (file.exists(cnrfc_stream_rds_path)) {

  message("Reading CNRFC river/reservoir points for CDEC-CNRFC crosswalk: ", cnrfc_stream_rds_path)

  cnrfc_stream_for_xwalk <- readRDS(cnrfc_stream_rds_path) |>
    clean_sf_for_leaflet()

  if (inherits(cnrfc_stream_for_xwalk, "sf") && nrow(cnrfc_stream_for_xwalk) > 0) {
    cnrfc_current_context <- cnrfc_stream_for_xwalk |>
      dplyr::mutate(
        nws_id = pt_norm_station_id(.data$nwsid),
        cnrfc_point_role = dplyr::case_when(
          tolower(trimws(as.character(.data$gage_class1))) == "reservoir" ~ "reservoir",
          tolower(trimws(as.character(.data$gage_class1))) == "river" ~ "river",
          tolower(trimws(as.character(.data$gage_class1))) == "special" ~ "special",
          TRUE ~ "other"
        ),
        cnrfc_station_name = dplyr::coalesce(
          pt_na_chr(.data$nickname),
          paste(pt_na_chr(.data$channel), pt_na_chr(.data$loc), sep = " - "),
          pt_na_chr(.data$channel),
          pt_na_chr(.data$loc),
          .data$nws_id
        ),
        cnrfc_channel = pt_na_chr(.data$channel),
        cnrfc_location = pt_na_chr(.data$loc),
        cnrfc_gage_class1 = pt_na_chr(.data$gage_class1),
        cnrfc_gage_class2 = pt_na_chr(.data$gage_class2),
        cnrfc_gage_type = pt_na_chr(.data$gage_type)
      ) |>
      sf::st_drop_geometry() |>
      dplyr::select(
        nws_id,
        cnrfc_station_name,
        cnrfc_channel,
        cnrfc_location,
        cnrfc_point_role,
        cnrfc_gage_class1,
        cnrfc_gage_class2,
        cnrfc_gage_type
      ) |>
      dplyr::filter(!is.na(.data$nws_id), .data$nws_id != "") |>
      dplyr::distinct(.data$nws_id, .keep_all = TRUE)
  }

} else {
  message("CNRFC stream/reservoir RDS not found; CDEC-CNRFC current-context join skipped: ", cnrfc_stream_rds_path)
}

## General station crosswalk.  Restrict to current CNRFC IDs when available so
## stale legacy keys do not clutter BRIM popups.  If current CNRFC context is
## unavailable, still write the direct-key table for QA/review.
cdec_cnrfc_station_crosswalk <- station_key_rows |>
  dplyr::left_join(cnrfc_current_context, by = "nws_id") |>
  dplyr::left_join(
    index_tbl |>
      dplyr::select(
        dplyr::any_of(c(
          "cdec_id",
          "cdec_station_name",
          "cdec_station_url",
          "cdec_sensor15_hourly_url",
          "cdec_sensor15_daily_url",
          "has_hourly_reservoir_report",
          "has_daily_reservoir_report"
        ))
      ),
    by = "cdec_id"
  ) |>
  dplyr::left_join(cdec_station_name_lookup, by = "cdec_id") |>
  dplyr::mutate(
    cdec_station_name = dplyr::coalesce(
      pt_na_chr(.data$cdec_station_name),
      pt_na_chr(.data$cdec_station_name_key),
      pt_na_chr(.data$cdec_station_name_lookup)
    ),
    cdec_station_url = dplyr::coalesce(
      pt_na_chr(.data$cdec_station_url),
      paste0("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=", .data$cdec_id)
    ),
    cdec_sensor15_hourly_url = dplyr::if_else(
      !is.na(.data$has_hourly_reservoir_report) & .data$has_hourly_reservoir_report,
      dplyr::coalesce(
        pt_na_chr(.data$cdec_sensor15_hourly_url),
        paste0(
          "https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=",
          .data$cdec_id,
          "&SensorNums=15&dur_code=H&Start=&End=now"
        )
      ),
      NA_character_
    ),
    cdec_sensor15_daily_url = dplyr::if_else(
      !is.na(.data$has_daily_reservoir_report) & .data$has_daily_reservoir_report,
      dplyr::coalesce(
        pt_na_chr(.data$cdec_sensor15_daily_url),
        paste0(
          "https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=",
          .data$cdec_id,
          "&SensorNums=15&dur_code=D&Start=&End=now"
        )
      ),
      NA_character_
    ),
    match_method = "legacy_direct_cdec_nws_key",
    match_confidence = "high",
    match_distance_m = NA_real_,
    name_score = NA_real_,
    review_note = dplyr::case_when(
      !is.na(.data$cnrfc_point_role) ~ "Direct legacy CDEC<->NWS/CNRFC key matched a current CNRFC river/reservoir point.",
      TRUE ~ "Direct legacy CDEC<->NWS/CNRFC key did not match a current CNRFC point in this BRIM input; retained for QA only."
    ),
    use_in_cnrfc_popup = !is.na(.data$cnrfc_point_role)
  ) |>
  dplyr::arrange(.data$nws_id, .data$cdec_id) |>
  dplyr::distinct(.data$nws_id, .data$cdec_id, .keep_all = TRUE)

if (nrow(cnrfc_current_context) > 0) {
  cdec_cnrfc_station_crosswalk <- cdec_cnrfc_station_crosswalk |>
    dplyr::filter(.data$use_in_cnrfc_popup)
}

## ---- 11.1B Reservoir spatial/name candidates retained from prior workflow ---
##
## The direct legacy key is excellent for rivers, but it is not complete for
## reservoirs.  Keep a conservative spatial/name candidate step for CNRFC
## reservoir points, using the current CDEC reservoir index as the CDEC side.

cdec_cnrfc_candidates <- tibble::tibble()

if (exists("cnrfc_stream_for_xwalk") && inherits(cnrfc_stream_for_xwalk, "sf") && nrow(cnrfc_stream_for_xwalk) > 0) {

  cnrfc_reservoir_pts <- cnrfc_stream_for_xwalk |>
    dplyr::filter(tolower(trimws(as.character(.data$gage_class1))) == "reservoir") |>
    dplyr::mutate(
      nws_id = pt_norm_station_id(.data$nwsid),
      cnrfc_reservoir_name = dplyr::coalesce(
        pt_na_chr(.data$nickname),
        pt_na_chr(.data$loc),
        pt_na_chr(.data$channel),
        .data$nws_id
      ),
      cnrfc_channel = pt_na_chr(.data$channel),
      cnrfc_location = pt_na_chr(.data$loc),
      cnrfc_gage_type = pt_na_chr(.data$gage_type)
    ) |>
    dplyr::filter(!is.na(.data$nws_id))

  cdec_match_pts <- index_sf |>
    dplyr::filter(.data$map_geometry_ok) |>
    dplyr::mutate(
      cdec_match_name = dplyr::coalesce(
        pt_na_chr(.data$legacy_reservoir_name_mid_xy),
        pt_na_chr(.data$reservoir_name),
        pt_na_chr(.data$cdec_station_name),
        .data$cdec_id
      )
    )

  if (nrow(cnrfc_reservoir_pts) > 0 && nrow(cdec_match_pts) > 0) {

    cnrfc_3310 <- sf::st_transform(cnrfc_reservoir_pts, 3310)
    cdec_3310  <- sf::st_transform(cdec_match_pts, 3310)

    nearest_idx <- sf::st_nearest_feature(cnrfc_3310, cdec_3310)
    nearest_distance_m <- as.numeric(sf::st_distance(
      sf::st_geometry(cnrfc_3310),
      sf::st_geometry(cdec_3310[nearest_idx, ]),
      by_element = TRUE
    ))

    cnrfc_tbl <- cnrfc_reservoir_pts |>
      sf::st_drop_geometry() |>
      dplyr::select(
        nws_id,
        cnrfc_reservoir_name,
        cnrfc_channel,
        cnrfc_location,
        cnrfc_gage_type
      )

    cdec_tbl <- cdec_match_pts[nearest_idx, ] |>
      sf::st_drop_geometry() |>
      dplyr::select(
        cdec_id,
        reservoir_name,
        cdec_station_name,
        cdec_match_name,
        cdec_station_url,
        cdec_sensor15_hourly_url,
        cdec_sensor15_daily_url,
        has_hourly_reservoir_report,
        has_daily_reservoir_report,
        legacy_reservoir_name_mid_xy,
        alias_names
      )

    cdec_cnrfc_candidates <- dplyr::bind_cols(cnrfc_tbl, cdec_tbl) |>
      dplyr::mutate(
        match_distance_m = nearest_distance_m,
        name_score = pt_token_score(.data$cnrfc_reservoir_name, .data$cdec_match_name),
        match_method = dplyr::case_when(
          .data$match_distance_m <= 1000 ~ "nearest_reservoir_point_le_1km",
          .data$match_distance_m <= 5000 & .data$name_score >= 0.20 ~ "nearest_reservoir_point_le_5km_name_support",
          .data$match_distance_m <= 10000 & .data$name_score >= 0.35 ~ "nearest_reservoir_point_le_10km_strong_name_support",
          TRUE ~ "nearest_reservoir_point_review_only"
        ),
        match_confidence = dplyr::case_when(
          .data$match_method == "nearest_reservoir_point_le_1km" ~ "high",
          .data$match_method %in% c(
            "nearest_reservoir_point_le_5km_name_support",
            "nearest_reservoir_point_le_10km_strong_name_support"
          ) ~ "medium",
          TRUE ~ "review"
        ),
        review_note = dplyr::case_when(
          .data$match_confidence == "high" ~ "Accepted: CNRFC reservoir point is within 1 km of current CDEC reservoir station point.",
          .data$match_confidence == "medium" ~ "Accepted: nearby CNRFC/CDEC reservoir points with supporting name-token overlap.",
          TRUE ~ "Review only: nearest CDEC reservoir was not close enough or name support was weak."
        )
      )
  }
}

accepted_spatial_cdec_cnrfc <- cdec_cnrfc_candidates |>
  dplyr::filter(.data$match_confidence %in% c("high", "medium")) |>
  dplyr::transmute(
    nws_id,
    cdec_id,
    cnrfc_reservoir_name,
    cdec_station_name,
    cdec_station_url,
    cdec_sensor15_hourly_url,
    cdec_sensor15_daily_url,
    match_method,
    match_confidence,
    match_distance_m,
    name_score,
    review_note
  )

## Reservoir-focused crosswalk retained for reservoir workflows and to preserve
## the spatial/name matches that may not exist in the direct legacy keys.
cdec_cnrfc_crosswalk <- dplyr::bind_rows(
  cdec_cnrfc_station_crosswalk |>
    dplyr::filter(tolower(as.character(.data$cnrfc_point_role)) == "reservoir") |>
    dplyr::transmute(
      nws_id,
      cdec_id,
      cnrfc_reservoir_name = .data$cnrfc_station_name,
      cdec_station_name,
      cdec_station_url,
      cdec_sensor15_hourly_url,
      cdec_sensor15_daily_url,
      match_method,
      match_confidence,
      match_distance_m,
      name_score,
      review_note
    ),
  accepted_spatial_cdec_cnrfc
) |>
  dplyr::mutate(
    match_rank = dplyr::case_when(
      grepl("legacy_direct", .data$match_method) ~ 1L,
      .data$match_confidence == "high" ~ 2L,
      .data$match_confidence == "medium" ~ 3L,
      TRUE ~ 9L
    )
  ) |>
  dplyr::arrange(.data$nws_id, .data$match_rank, .data$match_distance_m) |>
  dplyr::group_by(.data$nws_id) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(-dplyr::any_of("match_rank"))

## Add accepted spatial reservoir matches into the general station crosswalk only
## where a direct station key did not already provide a CDEC link for that CNRFC
## point.  This lets CNRFC popups use one general table.
spatial_station_additions <- cdec_cnrfc_crosswalk |>
  dplyr::anti_join(
    cdec_cnrfc_station_crosswalk |>
      dplyr::select("nws_id"),
    by = "nws_id"
  ) |>
  dplyr::left_join(cnrfc_current_context, by = "nws_id") |>
  dplyr::mutate(
    crosswalk_source = "spatial_reservoir_candidate",
    cdec_station_name_key = NA_character_,
    cdec_station_name_lookup = .data$cdec_station_name,
    cdec_lookup_source = "current_cdec_reservoir_report",
    use_in_cnrfc_popup = TRUE
  )

cdec_cnrfc_station_crosswalk <- dplyr::bind_rows(
  cdec_cnrfc_station_crosswalk,
  spatial_station_additions |>
    dplyr::select(dplyr::any_of(names(cdec_cnrfc_station_crosswalk)))
) |>
  dplyr::mutate(
    match_rank = dplyr::case_when(
      grepl("legacy_direct", .data$match_method) ~ 1L,
      .data$match_confidence == "high" ~ 2L,
      .data$match_confidence == "medium" ~ 3L,
      TRUE ~ 9L
    )
  ) |>
  dplyr::arrange(.data$nws_id, .data$match_rank, .data$match_distance_m) |>
  dplyr::group_by(.data$nws_id) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(-dplyr::any_of("match_rank"))

cdec_cnrfc_station_crosswalk_qa <- dplyr::bind_rows(
  cdec_cnrfc_station_crosswalk |>
    dplyr::mutate(candidate_source = "accepted_general_station_crosswalk"),
  station_key_rows |>
    dplyr::anti_join(cdec_cnrfc_station_crosswalk, by = c("nws_id", "cdec_id")) |>
    dplyr::mutate(
      candidate_source = "legacy_direct_key_not_used",
      cnrfc_station_name = NA_character_,
      cnrfc_point_role = NA_character_,
      cnrfc_gage_class1 = NA_character_,
      cnrfc_gage_class2 = NA_character_,
      cnrfc_gage_type = NA_character_,
      cdec_station_name = .data$cdec_station_name_key,
      cdec_station_url = paste0("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=", .data$cdec_id),
      cdec_sensor15_hourly_url = NA_character_,
      cdec_sensor15_daily_url = NA_character_,
      match_method = "legacy_direct_cdec_nws_key_unmatched_current_cnrfc",
      match_confidence = "review",
      match_distance_m = NA_real_,
      name_score = NA_real_,
      review_note = "Legacy key row did not match a current CNRFC river/reservoir point in this BRIM input.",
      use_in_cnrfc_popup = FALSE
    )
)

cdec_cnrfc_crosswalk_qa <- dplyr::bind_rows(
  cdec_cnrfc_candidates |>
    dplyr::mutate(candidate_source = "spatial_name_candidate"),
  cdec_cnrfc_crosswalk |>
    dplyr::mutate(
      candidate_source = "accepted_reservoir_crosswalk",
      reservoir_name = NA_character_,
      cdec_match_name = .data$cdec_station_name,
      has_hourly_reservoir_report = NA,
      has_daily_reservoir_report = NA,
      legacy_reservoir_name_mid_xy = NA_character_,
      alias_names = NA_character_
    )
) |>
  dplyr::arrange(.data$nws_id, .data$match_confidence, .data$match_distance_m)

message("CDEC-CNRFC general station crosswalk accepted rows: ", nrow(cdec_cnrfc_station_crosswalk))
if (nrow(cdec_cnrfc_station_crosswalk) > 0) {
  print(
    cdec_cnrfc_station_crosswalk |>
      dplyr::count(.data$cnrfc_point_role, .data$match_confidence, .data$match_method, name = "n") |>
      dplyr::arrange(.data$cnrfc_point_role, .data$match_confidence, .data$match_method),
    n = Inf
  )
}

message("CDEC-CNRFC reservoir crosswalk accepted rows: ", nrow(cdec_cnrfc_crosswalk))
if (nrow(cdec_cnrfc_crosswalk) > 0) {
  print(
    cdec_cnrfc_crosswalk |>
      dplyr::count(.data$match_confidence, .data$match_method, name = "n") |>
      dplyr::arrange(.data$match_confidence, .data$match_method),
    n = Inf
  )
}

# ==== 12. QA and legacy comparison outputs ===================================


legacy_compare <- index_tbl |>
  dplyr::transmute(
    cdec_id,
    reservoir_name,
    current_name = cdec_station_name,
    current_latitude = latitude,
    current_longitude = longitude,
    current_county = county,
    current_operator = operator_agency,
    current_basin = river_basin_cdec,
    is_cdec_summary_record,
    map_geometry_ok,
    coord_qa_note,
    has_hourly_reservoir_report,
    has_daily_reservoir_report,
    legacy_midres_lat,
    legacy_midres_lon,
    legacy_station_lat,
    legacy_station_lon,
    legacy_meta_lat,
    legacy_meta_lon,
    legacy_nws_ids,
    legacy_nws_ids_from_crosswalk,
    legacy_usgs_ids,
    legacy_cdec_names_from_nws_key,
    legacy_cdec_names_from_crosswalk,
    legacy_usgs_names,
    legacy_meta_name_cdec,
    legacy_fields_add_value = dplyr::case_when(
      !is.na(legacy_nws_ids) | !is.na(legacy_nws_ids_from_crosswalk) | !is.na(legacy_usgs_ids) ~ "crosswalk/alias value",
      !is.na(legacy_midres_lat) & !is.na(legacy_midres_lon) ~ "reservoir-center coordinate value",
      !is.na(legacy_meta_name_cdec) ~ "legacy metadata only",
      TRUE ~ "no legacy match"
    )
  )

coord_qa <- index_tbl |>
  dplyr::filter(!.data$map_geometry_ok) |>
  dplyr::select(
    dplyr::any_of(c(
      "cdec_id",
      "reservoir_name",
      "cdec_station_name",
      "latitude",
      "longitude",
      "is_cdec_summary_record",
      "coord_valid_wgs84",
      "coord_valid_brim_region",
      "map_geometry_ok",
      "coord_qa_note",
      "county",
      "operator_agency",
      "has_hourly_reservoir_report",
      "has_daily_reservoir_report",
      "cdec_metadata_source",
      "cdec_station_url"
    ))
  )

qa_tbl <- tibble::tibble(
  check = c(
    "cdec_hourly_rows_parsed",
    "cdec_daily_rows_parsed",
    "current_unique_cdec_ids",
    "index_rows_with_geometry",
    "records_with_hourly_report",
    "records_with_daily_report",
    "records_with_legacy_nws_crosswalk",
    "records_with_legacy_usgs_crosswalk",
    "records_with_legacy_midres_xy",
    "records_flagged_cdec_summary",
    "records_with_invalid_wgs84_coordinates",
    "records_outside_brim_mapping_bounds",
    "records_not_mapped",
    "legacy_cdec_to_nws_rows_matching_current_ids",
    "legacy_usgs_cnrfc_rows_matching_current_ids",
    "cdec_cnrfc_station_crosswalk_rows",
    "cdec_cnrfc_station_crosswalk_river_rows",
    "cdec_cnrfc_station_crosswalk_reservoir_rows",
    "cdec_cnrfc_crosswalk_accepted_rows",
    "cdec_cnrfc_crosswalk_high_confidence_rows",
    "cdec_cnrfc_crosswalk_medium_confidence_rows",
    "cdec_cnrfc_crosswalk_review_only_candidates"
  ),
  value = c(
    nrow(cdec_hourly),
    nrow(cdec_daily),
    nrow(cdec_current),
    nrow(index_sf),
    sum(index_tbl$has_hourly_reservoir_report, na.rm = TRUE),
    sum(index_tbl$has_daily_reservoir_report, na.rm = TRUE),
    sum(index_tbl$has_legacy_nws_crosswalk, na.rm = TRUE),
    sum(index_tbl$has_legacy_usgs_crosswalk, na.rm = TRUE),
    sum(index_tbl$has_legacy_midres_xy, na.rm = TRUE),
    sum(index_tbl$is_cdec_summary_record, na.rm = TRUE),
    sum(!index_tbl$coord_valid_wgs84, na.rm = TRUE),
    sum(index_tbl$coord_valid_wgs84 & !index_tbl$coord_valid_brim_region, na.rm = TRUE),
    sum(!index_tbl$map_geometry_ok, na.rm = TRUE),
    if (!is.null(legacy_cdec_nws)) sum(toupper(as.character(legacy_cdec_nws$id_cdec)) %in% index_tbl$cdec_id, na.rm = TRUE) else 0,
    if (!is.null(legacy_usgs_cnrfc)) sum(toupper(as.character(legacy_usgs_cnrfc$id_cdec)) %in% index_tbl$cdec_id, na.rm = TRUE) else 0,
    nrow(cdec_cnrfc_station_crosswalk),
    sum(cdec_cnrfc_station_crosswalk$cnrfc_point_role == "river", na.rm = TRUE),
    sum(cdec_cnrfc_station_crosswalk$cnrfc_point_role == "reservoir", na.rm = TRUE),
    nrow(cdec_cnrfc_crosswalk),
    sum(cdec_cnrfc_crosswalk$match_confidence == "high", na.rm = TRUE),
    sum(cdec_cnrfc_crosswalk$match_confidence == "medium", na.rm = TRUE),
    sum(cdec_cnrfc_crosswalk_qa$match_confidence == "review", na.rm = TRUE)
  ),
  run_timestamp = RUN_TS
)

# ==== 13. Write outputs ======================================================

save_rds_cached(
  x = index_sf,
  timestamped_path = out_index_timestamped,
  latest_path = out_index_latest
)

saveRDS(alias_tbl, out_alias_latest)
message("Saved alias/crosswalk RDS: ", out_alias_latest)

saveRDS(cdec_cnrfc_station_crosswalk, out_cdec_cnrfc_station_crosswalk_latest)
message("Saved CDEC-CNRFC general station crosswalk RDS: ", out_cdec_cnrfc_station_crosswalk_latest)

saveRDS(cdec_cnrfc_crosswalk, out_cdec_cnrfc_crosswalk_latest)
message("Saved CDEC-CNRFC reservoir crosswalk RDS: ", out_cdec_cnrfc_crosswalk_latest)

if (WRITE_GPKG) {
  sf::st_write(
    index_sf,
    dsn = out_gpkg,
    layer = "cdec_reservoir_station_index",
    delete_layer = TRUE,
    quiet = TRUE
  )
  message("Saved GPKG layer: ", out_gpkg)
}

if (WRITE_QA) {
  readr::write_csv(qa_tbl, out_qa)
  readr::write_csv(coord_qa, out_coord_qa)
  readr::write_csv(legacy_compare, out_compare)
  readr::write_csv(cdec_cnrfc_station_crosswalk_qa, out_cdec_cnrfc_station_crosswalk_qa)
  readr::write_csv(cdec_cnrfc_crosswalk_qa, out_cdec_cnrfc_crosswalk_qa)
  message("Saved QA CSV: ", out_qa)
  message("Saved coordinate QA CSV: ", out_coord_qa)
  message("Saved legacy comparison CSV: ", out_compare)
  message("Saved CDEC-CNRFC general station crosswalk QA CSV: ", out_cdec_cnrfc_station_crosswalk_qa)
  message("Saved CDEC-CNRFC reservoir crosswalk QA CSV: ", out_cdec_cnrfc_crosswalk_qa)
}

# ==== 14. Final console summary =============================================

message("\nDone: CDEC reservoir station index built.")
message("Latest station-index RDS:")
message("  ", out_index_latest)
message("Rows saved with mapped geometry: ", nrow(index_sf))
message("Rows retained in nonspatial index table: ", nrow(index_tbl))
message("Rows not mapped because of summary/coordinate QA: ", sum(!index_tbl$map_geometry_ok, na.rm = TRUE))
message("Hourly report records: ", sum(index_sf$has_hourly_reservoir_report, na.rm = TRUE))
message("Daily report records:  ", sum(index_sf$has_daily_reservoir_report, na.rm = TRUE))
message("Legacy CNRFC/NWS crosswalk matches: ", sum(index_sf$has_legacy_nws_crosswalk, na.rm = TRUE))
message("Legacy USGS crosswalk matches: ", sum(index_sf$has_legacy_usgs_crosswalk, na.rm = TRUE))
message("Accepted CDEC-CNRFC general station crosswalk rows: ", nrow(cdec_cnrfc_station_crosswalk))
message("Accepted CDEC-CNRFC reservoir crosswalk rows: ", nrow(cdec_cnrfc_crosswalk))
message("CDEC-CNRFC general station crosswalk latest RDS:")
message("  ", out_cdec_cnrfc_station_crosswalk_latest)
message("CDEC-CNRFC reservoir crosswalk latest RDS:")
message("  ", out_cdec_cnrfc_crosswalk_latest)
