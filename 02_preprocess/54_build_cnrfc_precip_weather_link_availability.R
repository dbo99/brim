# ==== 54_build_cnrfc_precip_weather_link_availability.R =====================
##
## PURPOSE:
##   Audit NOAA/NWS link availability for the CNRFC precip/weather station
##   catalog. This is an intelligence/preprocessor step, not a map UI edit.
##
## PATCH 041f FOCUS:
##   Freeze the ASOS/GOES station-link audit into production-style map products.
##   The Ops Live product now contains only stations with verified recent NWS
##   station-observation evidence. Checked-but-not-verified ASOS/GOES stations
##   remain in QA/archive outputs and in the broader Local catalog, but are not
##   drawn in Ops Live by default.
##
## OUTPUTS:
##   04_processed_data/rds/cnrfc_precip_weather_noaa_link_candidates.rds
##   04_processed_data/rds/cnrfc_precip_weather_noaa_link_probe.rds
##   04_processed_data/rds/cnrfc_precip_weather_noaa_link_matrix.rds
##   04_processed_data/rds/cnrfc_precip_weather_noaa_link_bins.rds
##   04_processed_data/rds/cnrfc_precip_weather_noaa_link_summary.rds
##
##   CSV copies are written to 04_processed_data/qa/ with timestamped and
##   *_latest.csv names, including a small known-station diagnostic table.
## ============================================================================

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
  library(curl)
})

RUN_TS <- make_timestamp()
CACHE_SCHEMA_VERSION <- "20260625_patch040g_onehourp_value_evidence_not_authority_v1"
PT_CNRFC_ONEHOURP_KML_URL <- "https://www.cnrfc.noaa.gov/data/kml/1hour_ge.kml"

PT_KNOWN_DIAGNOSTIC_IDS <- c(
  "BKSC1", "BDMC1", "LBIC1", "SFXC1",
  "PVF", "SMF", "SFO",
  "KPVF", "KSMF", "KSFO"
)

PT_STATION_DATA_TERMS <- c(
  "temperature", "dew point", "relative humidity", "wind speed", "wind direction",
  "accumulated precipitation", "one hour precipitation", "precipitation",
  "snow depth", "snow-water equivalent", "water temperature", "metar", "asos/awos"
)

PT_WRH_NEGATIVE_PATTERNS <- c(
  "not a valid station",
  "invalid station",
  "station identifier[^.]{0,80}not",
  "no station found",
  "station[^.]{0,80}was not found",
  "unable to find station",
  "unknown station"
)

PT_API_NEGATIVE_PATTERNS <- c(
  "not found",
  "invalid",
  "parameter 'stationid' is invalid",
  "not a recognized station",
  "no station found"
)

PT_RAW_SOURCE_CODE_LABELS <- c(
  Z = "ASOS (airport)",
  G = "GOES",
  R = "ALERT (event only)",
  M = "Other / not parsed",
  P = "Other / not parsed",
  W = "Other / not parsed"
)

PT_RAW_SOURCE_CODE_DETAILS <- c(
  Z = "ASOS/AWOS airport-style station",
  G = "GOES-telemetered precip/weather station",
  R = "ALERT/event station",
  M = "Manual/other meteorological station",
  P = "Precipitation/other station",
  W = "Weather/other station"
)

dir.create(DIR$rds, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR$qa, showWarnings = FALSE, recursive = TRUE)

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

pt_write_csv_pair <- function(x, stem) {
  ts_path <- file.path(DIR$qa, paste0(stem, "_", RUN_TS, ".csv"))
  latest_path <- file.path(DIR$qa, paste0(stem, "_latest.csv"))
  readr::write_csv(x, ts_path, na = "")
  readr::write_csv(x, latest_path, na = "")
  invisible(list(timestamped = ts_path, latest = latest_path))
}

pt_write_csv_archive <- function(x, stem, archive_subdir = "cnrfc_precip_weather_audit_archive") {
  archive_dir <- file.path(DIR$qa, archive_subdir)
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  archive_path <- file.path(archive_dir, paste0(stem, "_", RUN_TS, ".csv"))
  readr::write_csv(x, archive_path, na = "")
  invisible(archive_path)
}

pt_write_rds <- function(x, filename) {
  out <- file.path(DIR$rds, filename)
  saveRDS(x, out)
  invisible(out)
}

pt_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

pt_bool <- function(x) {
  if (is.logical(x)) return(ifelse(is.na(x), FALSE, x))
  txt <- tolower(pt_chr(x))
  txt %in% c("true", "t", "1", "yes", "y")
}

pt_first_nonempty <- function(x, default = "") {
  x <- pt_chr(x)
  x <- x[nzchar(x)]
  if (length(x) == 0) return(default)
  x[[1]]
}

pt_first_non_na_num <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(default)
  x[[1]]
}

pt_empty_precip_fixed <- function() {
  tibble(
    cnrfc_id = character(),
    raw_precip_source_code = character(),
    raw_precip_station_name = character(),
    raw_precip_lat = numeric(),
    raw_precip_lon = numeric(),
    raw_precip_elev_ft = numeric(),
    raw_precip_state = character(),
    raw_precip_file = character(),
    in_class_coded_precip_station_file = logical(),
    raw_precip_source_detail = character(),
    raw_source_class = character()
  )
}

pt_empty_current_snapshot <- function() {
  tibble(
    cnrfc_id = character(),
    current_snapshot_lat = numeric(),
    current_snapshot_lon = numeric(),
    current_snapshot_file = character(),
    in_current_nwsid_snapshot = logical(),
    current_snapshot_zero_coord = logical()
  )
}

pt_empty_onehourp_kml <- function() {
  tibble(
    cnrfc_id = character(),
    onehourp_kml_name = character(),
    onehourp_kml_description = character(),
    onehourp_kml_lat = numeric(),
    onehourp_kml_lon = numeric(),
    onehourp_kml_value = character(),
    onehourp_kml_url = character(),
    onehourp_kml_status_code = integer(),
    onehourp_kml_bytes = integer(),
    onehourp_kml_error = character(),
    in_current_onehourp_kml = logical()
  )
}

pt_empty_river_catalog <- function() {
  tibble(
    cnrfc_id = character(),
    river_catalog_transmission_code = character(),
    river_catalog_stream_or_feature = character(),
    river_catalog_location = character(),
    river_catalog_lat = numeric(),
    river_catalog_lon = numeric(),
    river_catalog_elev_ft = numeric(),
    river_catalog_primary_type = character(),
    river_catalog_secondary_type = character(),
    river_catalog_short_label = character(),
    river_catalog_file = character(),
    in_cnrfc_river_reservoir_catalog = logical()
  )
}

pt_get_col <- function(dat, nm, default = NA_character_) {
  if (nm %in% names(dat)) dat[[nm]] else rep(default, nrow(dat))
}

pt_clean_id <- function(x) {
  x <- toupper(pt_chr(x))
  x[x %in% c("", "NA", "N/A", "NULL", "NONE", "<BLANK>")] <- ""
  x
}

pt_urlencode <- function(x) {
  utils::URLencode(pt_chr(x), reserved = TRUE)
}

pt_text_excerpt <- function(text, max_chars = 360L) {
  txt <- pt_chr(text)
  if (!nzchar(txt)) return("")
  txt <- gsub("<script[\\s\\S]*?</script>", " ", txt, ignore.case = TRUE, perl = TRUE)
  txt <- gsub("<style[\\s\\S]*?</style>", " ", txt, ignore.case = TRUE, perl = TRUE)
  txt <- gsub("<[^>]+>", " ", txt, perl = TRUE)
  txt <- gsub("&nbsp;|&#160;", " ", txt, ignore.case = TRUE)
  txt <- gsub("&amp;", "&", txt, ignore.case = TRUE)
  txt <- gsub("&lt;", "<", txt, ignore.case = TRUE)
  txt <- gsub("&gt;", ">", txt, ignore.case = TRUE)
  txt <- gsub("\\s+", " ", txt, perl = TRUE)
  txt <- trimws(txt)
  substr(txt, 1L, max_chars)
}

pt_match_names <- function(text_low, named_patterns) {
  hits <- vapply(
    named_patterns,
    function(pat) grepl(pat, text_low, ignore.case = TRUE, perl = TRUE),
    logical(1)
  )
  names(named_patterns)[hits]
}

pt_named_patterns <- function(x) {
  stats::setNames(x, x)
}

pt_status_ok <- function(status_code) {
  !is.na(status_code) && status_code >= 200L && status_code < 400L
}

# ---- Input catalog and raw station authority -------------------------------

pt_find_raw_file <- function(patterns, prefer_not = character(0), root_dirs = c("01_raw_data", ".")) {
  roots <- root_dirs[dir.exists(root_dirs)]
  if (length(roots) == 0) return(NA_character_)

  files <- unique(unlist(lapply(
    roots,
    function(root) list.files(root, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  )))
  if (length(files) == 0) return(NA_character_)

  b <- basename(files)
  keep <- rep(FALSE, length(files))
  for (pat in patterns) {
    keep <- keep | grepl(pat, b, ignore.case = TRUE, perl = TRUE)
  }
  files <- files[keep]
  if (length(files) == 0) return(NA_character_)

  if (length(prefer_not) > 0) {
    bad <- rep(FALSE, length(files))
    for (pat in prefer_not) {
      bad <- bad | grepl(pat, basename(files), ignore.case = TRUE, perl = TRUE)
    }
    if (any(!bad)) files <- files[!bad]
  }

  files[order(file.info(files)$mtime, decreasing = TRUE)][[1]]
}

pt_fetch_text_basic <- function(url, timeout_sec = 25L) {
  url <- pt_chr(url)
  if (!nzchar(url)) {
    return(list(ok = FALSE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = "blank URL"))
  }
  h <- curl::new_handle(
    useragent = "BRIM CNRFC current onehourP observed-value KML QA/QC",
    timeout = timeout_sec,
    followlocation = TRUE
  )
  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
  if (inherits(res, "error")) {
    return(list(ok = FALSE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = conditionMessage(res)))
  }
  txt <- tryCatch(rawToChar(res$content), error = function(e) "")
  txt <- iconv(txt, from = "", to = "UTF-8", sub = "byte")
  list(
    ok = pt_status_ok(res$status_code),
    status_code = as.integer(res$status_code),
    bytes = length(res$content),
    text = txt,
    error = NA_character_
  )
}

pt_xmlish_decode <- function(x) {
  x <- pt_chr(x)
  x <- gsub("<!\\[CDATA\\[|\\]\\]>", "", x, perl = TRUE)
  x <- gsub("<[^>]+>", " ", x, perl = TRUE)
  x <- gsub("&nbsp;|&#160;", " ", x, ignore.case = TRUE)
  x <- gsub("&amp;", "&", x, ignore.case = TRUE)
  x <- gsub("&lt;", "<", x, ignore.case = TRUE)
  x <- gsub("&gt;", ">", x, ignore.case = TRUE)
  x <- gsub("&quot;", '"', x, ignore.case = TRUE)
  x <- gsub("&#39;|&apos;", "'", x, ignore.case = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

pt_extract_cnrfc_id_from_kml <- function(name, description) {
  txt <- paste(pt_chr(name), pt_chr(description), sep = " ")
  txt <- gsub("<[^>]+>", " ", txt, perl = TRUE)
  txt_up <- toupper(txt)

  # Prefer explicit id/site fields or URL query parameters if the KML popup has them.
  explicit <- stringr::str_match(
    txt_up,
    "(?:NWSID|NWS ID|STATION ID|SHEF ID|SITE|ID|GAGE|GAUGE)[^A-Z0-9]{0,12}([A-Z0-9_]{3,12})"
  )[, 2]
  explicit <- explicit[!is.na(explicit) & nzchar(explicit)]
  if (length(explicit) > 0) return(explicit[[1]])

  query_id <- stringr::str_match(txt_up, "[?&](?:ID|SITE)=([A-Z0-9_]{3,12})")[, 2]
  query_id <- query_id[!is.na(query_id) & nzchar(query_id)]
  if (length(query_id) > 0) return(query_id[[1]])

  # Many CNRFC KML files put the station ID in the Placemark name. Use that next.
  nm <- toupper(pt_xmlish_decode(name))
  nm_first <- stringr::str_match(nm, "^([A-Z0-9_]{3,12})\\b")[, 2]
  if (!is.na(nm_first) && nzchar(nm_first)) return(nm_first)

  # Last resort: find station-looking tokens, excluding common product/UI words.
  tokens <- unlist(stringr::str_extract_all(txt_up, "\\b[A-Z0-9_]{3,12}\\b"))
  tokens <- tokens[!tokens %in% c(
    "KML", "XML", "HTML", "HTTP", "HTTPS", "CNRFC", "NOAA", "NWS", "WRH", "UTC", "PST", "PDT",
    "ONEHOURP", "PRECIP", "RAIN", "DATA", "DATE", "TIME", "VALUE", "HOUR", "HOURS", "MAP",
    "LAT", "LON", "LNG", "STYLE", "TABLE", "WIDTH", "HEIGHT", "COLOR", "BLACK", "WHITE"
  )]
  tokens <- tokens[!grepl("^[0-9]+$", tokens)]
  if (length(tokens) > 0) return(tokens[[1]])

  ""
}

pt_parse_cnrfc_onehourp_kml <- function(url = PT_CNRFC_ONEHOURP_KML_URL, enabled = TRUE, timeout_sec = 25L) {
  if (!isTRUE(enabled)) return(pt_empty_onehourp_kml())

  fetched <- pt_fetch_text_basic(url, timeout_sec = timeout_sec)
  qa_fetch <- tibble(
    source = "current_cnrfc_onehourP_kml",
    url = url,
    status_code = fetched$status_code,
    bytes = fetched$bytes,
    ok = fetched$ok,
    error = fetched$error,
    run_timestamp = RUN_TS
  )
  pt_write_csv_pair(qa_fetch, "cnrfc_precip_weather_onehourP_kml_fetch_status")

  if (!isTRUE(fetched$ok) || !nzchar(fetched$text)) {
    warning("Could not fetch current CNRFC onehourP observed-value KML: ", pt_chr(fetched$error))
    return(pt_empty_onehourp_kml())
  }

  kml <- fetched$text
  raw_path <- file.path(DIR$qa, paste0("cnrfc_onehourP_current_raw_", RUN_TS, ".kml"))
  latest_raw_path <- file.path(DIR$qa, "cnrfc_onehourP_current_raw_latest.kml")
  writeLines(kml, raw_path, useBytes = TRUE)
  writeLines(kml, latest_raw_path, useBytes = TRUE)

  placemarks <- stringr::str_extract_all(kml, regex("<Placemark[\\s\\S]*?</Placemark>", ignore_case = TRUE))[[1]]
  if (length(placemarks) == 0) {
    warning("Fetched onehourP KML, but no <Placemark> blocks were found. This can be normal during dry hours; do not treat this as a failed station-inventory fetch.")
    return(pt_empty_onehourp_kml())
  }

  rows <- lapply(placemarks, function(pm) {
    name_raw <- stringr::str_match(pm, regex("<name[^>]*>([\\s\\S]*?)</name>", ignore_case = TRUE))[, 2]
    desc_raw <- stringr::str_match(pm, regex("<description[^>]*>([\\s\\S]*?)</description>", ignore_case = TRUE))[, 2]
    coord <- stringr::str_match(pm, regex("<coordinates[^>]*>\\s*([-+0-9.]+)\\s*,\\s*([-+0-9.]+)(?:\\s*,\\s*([-+0-9.]+))?\\s*</coordinates>", ignore_case = TRUE))

    desc_clean <- pt_xmlish_decode(desc_raw)
    value <- stringr::str_match(desc_clean, "(?i)(?:1[- ]?hour|one[- ]?hour|precip(?:itation)?)\\D{0,30}([-+]?\\d+(?:\\.\\d+)?)")
    value <- value[, 2]

    tibble(
      cnrfc_id = pt_clean_id(pt_extract_cnrfc_id_from_kml(name_raw, desc_raw)),
      onehourp_kml_name = pt_xmlish_decode(name_raw),
      onehourp_kml_description = desc_clean,
      onehourp_kml_lat = suppressWarnings(as.numeric(coord[, 3])),
      onehourp_kml_lon = suppressWarnings(as.numeric(coord[, 2])),
      onehourp_kml_value = pt_chr(value),
      onehourp_kml_url = url,
      onehourp_kml_status_code = fetched$status_code,
      onehourp_kml_bytes = fetched$bytes,
      onehourp_kml_error = pt_chr(fetched$error),
      in_current_onehourp_kml = TRUE
    )
  })

  out <- bind_rows(rows) %>%
    filter(nzchar(.data$cnrfc_id)) %>%
    mutate(
      onehourp_kml_zero_coord = is.na(.data$onehourp_kml_lat) | is.na(.data$onehourp_kml_lon) |
        .data$onehourp_kml_lat == 0 | .data$onehourp_kml_lon == 0
    ) %>%
    arrange(.data$cnrfc_id) %>%
    distinct(.data$cnrfc_id, .keep_all = TRUE)

  out
}

pt_parse_cnrfc_precip_fixed_file <- function(path) {
  if (is.na(path) || !file.exists(path)) return(pt_empty_precip_fixed())
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) return(pt_empty_precip_fixed())

  # Example:
  # BKSC1 G "Black Springs              " 38.37750 -120.18889 6500 CA
  m <- stringr::str_match(
    lines,
    '^\\s*(\\S+)\\s+(\\S+)\\s+"([^"]*)"\\s+([-+]?\\d+(?:\\.\\d+)?)\\s+([-+]?\\d+(?:\\.\\d+)?)\\s+([-+]?\\d+)\\s+(\\S+)\\s*$'
  )
  good <- !is.na(m[, 1])
  if (!any(good)) {
    warning("No rows parsed from class-coded precip/weather station file: ", path)
    return(pt_empty_precip_fixed())
  }

  out <- tibble(
    cnrfc_id = pt_clean_id(m[good, 2]),
    raw_precip_source_code = toupper(pt_chr(m[good, 3])),
    raw_precip_station_name = trimws(m[good, 4]),
    raw_precip_lat = suppressWarnings(as.numeric(m[good, 5])),
    raw_precip_lon = suppressWarnings(as.numeric(m[good, 6])),
    raw_precip_elev_ft = suppressWarnings(as.numeric(m[good, 7])),
    raw_precip_state = toupper(pt_chr(m[good, 8])),
    raw_precip_file = normalizePath(path, winslash = "/", mustWork = FALSE),
    in_class_coded_precip_station_file = TRUE
  ) %>%
    filter(nzchar(.data$cnrfc_id)) %>%
    mutate(
      raw_precip_source_detail = dplyr::case_when(
        .data$raw_precip_source_code %in% names(PT_RAW_SOURCE_CODE_DETAILS) ~ unname(PT_RAW_SOURCE_CODE_DETAILS[.data$raw_precip_source_code]),
        TRUE ~ "Other / not parsed"
      ),
      raw_source_class = dplyr::case_when(
        .data$raw_precip_source_code %in% names(PT_RAW_SOURCE_CODE_LABELS) ~ unname(PT_RAW_SOURCE_CODE_LABELS[.data$raw_precip_source_code]),
        TRUE ~ "Other / not parsed"
      )
    ) %>%
    arrange(.data$cnrfc_id, .data$raw_precip_source_code) %>%
    distinct(.data$cnrfc_id, .keep_all = TRUE)

  out
}

pt_parse_cnrfc_current_nwsid_file <- function(path) {
  if (is.na(path) || !file.exists(path)) return(pt_empty_current_snapshot())
  dat <- tryCatch(
    readr::read_delim(path, delim = ";", trim_ws = TRUE, col_types = cols(.default = "c"), show_col_types = FALSE),
    error = function(e) tibble()
  )
  if (nrow(dat) == 0) return(pt_empty_current_snapshot())

  nm <- tolower(gsub("\\s+", "", names(dat)))
  id_col <- which(nm %in% c("nwsid", "id", "cnrfc_id"))[1]
  lat_col <- which(nm %in% c("lat", "latitude"))[1]
  lon_col <- which(nm %in% c("lon", "longitude"))[1]
  if (is.na(id_col) || is.na(lat_col) || is.na(lon_col)) return(pt_empty_current_snapshot())

  tibble(
    cnrfc_id = pt_clean_id(dat[[id_col]]),
    current_snapshot_lat = suppressWarnings(as.numeric(dat[[lat_col]])),
    current_snapshot_lon = suppressWarnings(as.numeric(dat[[lon_col]])),
    current_snapshot_file = normalizePath(path, winslash = "/", mustWork = FALSE),
    in_current_nwsid_snapshot = TRUE
  ) %>%
    filter(nzchar(.data$cnrfc_id)) %>%
    mutate(
      current_snapshot_zero_coord = is.na(.data$current_snapshot_lat) | is.na(.data$current_snapshot_lon) |
        .data$current_snapshot_lat == 0 | .data$current_snapshot_lon == 0
    ) %>%
    arrange(.data$cnrfc_id) %>%
    distinct(.data$cnrfc_id, .keep_all = TRUE)
}

pt_parse_cnrfc_river_catalog_file <- function(path) {
  if (is.na(path) || !file.exists(path)) return(pt_empty_river_catalog())
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) return(pt_empty_river_catalog())

  # Example:
  # EXQC1 G "Merced River" "Exchequer Reservoir" 37.58500 -120.26722 879 Reservoir Reservoir
  m <- stringr::str_match(
    lines,
    '^\\s*(\\S+)\\s+(\\S+)\\s+"([^"]*)"\\s+"([^"]*)"\\s+([-+]?\\d+(?:\\.\\d+)?)\\s+([-+]?\\d+(?:\\.\\d+)?)\\s+([-+]?\\d+)\\s+(\\S+)\\s+(\\S+)(?:\\s+"([^"]*)")?.*$'
  )
  good <- !is.na(m[, 1])
  if (!any(good)) return(pt_empty_river_catalog())

  tibble(
    cnrfc_id = pt_clean_id(m[good, 2]),
    river_catalog_transmission_code = toupper(pt_chr(m[good, 3])),
    river_catalog_stream_or_feature = trimws(m[good, 4]),
    river_catalog_location = trimws(m[good, 5]),
    river_catalog_lat = suppressWarnings(as.numeric(m[good, 6])),
    river_catalog_lon = suppressWarnings(as.numeric(m[good, 7])),
    river_catalog_elev_ft = suppressWarnings(as.numeric(m[good, 8])),
    river_catalog_primary_type = pt_chr(m[good, 9]),
    river_catalog_secondary_type = pt_chr(m[good, 10]),
    river_catalog_short_label = trimws(pt_chr(m[good, 11])),
    river_catalog_file = normalizePath(path, winslash = "/", mustWork = FALSE),
    in_cnrfc_river_reservoir_catalog = TRUE
  ) %>%
    filter(nzchar(.data$cnrfc_id)) %>%
    arrange(.data$cnrfc_id) %>%
    distinct(.data$cnrfc_id, .keep_all = TRUE)
}

pt_read_cnrfc_raw_authority <- function(use_current_onehourp_kml = TRUE, kml_timeout_sec = 25L) {
  precip_fixed_path <- pt_find_raw_file(
    patterns = c("^gage_data_precip.*\\.txt$"),
    prefer_not = c("morecurrent", "current")
  )
  current_path <- pt_find_raw_file(
    patterns = c("^gage_data_precip.*morecurrent.*\\.txt$", "^gage_data_precip_morecurrent.*\\.txt$")
  )
  river_path <- pt_find_raw_file(
    patterns = c("^gage_data_river.*\\.txt$")
  )

  onehourp_current <- pt_parse_cnrfc_onehourp_kml(
    url = PT_CNRFC_ONEHOURP_KML_URL,
    enabled = use_current_onehourp_kml,
    timeout_sec = kml_timeout_sec
  )
  precip_fixed <- pt_parse_cnrfc_precip_fixed_file(precip_fixed_path)
  current_snapshot <- pt_parse_cnrfc_current_nwsid_file(current_path)
  river_catalog <- pt_parse_cnrfc_river_catalog_file(river_path)

  pt_log("Raw/current CNRFC station-authority files:")
  pt_log("  current onehourP KML:           ", PT_CNRFC_ONEHOURP_KML_URL, " (", nrow(onehourp_current), " parsed IDs)")
  pt_log("  class-coded precip/weather file: ", ifelse(is.na(precip_fixed_path), "not found", precip_fixed_path), " (", nrow(precip_fixed), " parsed IDs)")
  pt_log("  current NWSID snapshot:          ", ifelse(is.na(current_path), "not found", current_path), " (", nrow(current_snapshot), " parsed IDs)")
  pt_log("  river/reservoir catalog:         ", ifelse(is.na(river_path), "not found", river_path), " (", nrow(river_catalog), " parsed IDs)")

  list(
    onehourp_current = onehourp_current,
    precip_fixed = precip_fixed,
    current_snapshot = current_snapshot,
    river_catalog = river_catalog,
    paths = tibble(
      source = c("current_onehourP_kml", "class_coded_precip_weather", "current_nwsid_snapshot", "river_reservoir_catalog"),
      path = c(PT_CNRFC_ONEHOURP_KML_URL, precip_fixed_path, current_path, river_path),
      parsed_ids = c(nrow(onehourp_current), nrow(precip_fixed), nrow(current_snapshot), nrow(river_catalog))
    )
  )
}

pt_read_precip_weather_catalog <- function() {
  rds_candidates <- c(
    file.path(DIR$rds, "cnrfc_precip_weather_stations_map.rds"),
    file.path(DIR$rds, "cnrfc_precip_weather_stations.rds")
  )
  rds_candidates <- rds_candidates[file.exists(rds_candidates)]
  if (length(rds_candidates) > 0) {
    pt_log("Reading precip/weather catalog RDS: ", rds_candidates[[1]])
    return(readRDS(rds_candidates[[1]]))
  }

  csv_path <- pt_latest_file("^cnrfc_precip_weather_stations.*\\.csv$")
  if (!is.na(csv_path) && file.exists(csv_path)) {
    pt_log("Reading precip/weather catalog CSV: ", csv_path)
    return(readr::read_csv(csv_path, show_col_types = FALSE))
  }

  stop(
    "Could not find cnrfc_precip_weather_stations_map.rds, ",
    "cnrfc_precip_weather_stations.rds, or latest precip/weather station CSV. ",
    "Run preprocess_cnrfc_forecast_point_product_availability() first."
  )
}

# ---- Candidate construction -------------------------------------------------

pt_source_class <- function(id, hint, transmission, raw_class1 = "", raw_class2 = "", raw_source_class = "") {
  raw_source_class <- pt_chr(raw_source_class)
  if (nzchar(raw_source_class)) return(raw_source_class)

  id <- toupper(pt_chr(id))
  txt <- paste(hint, transmission, raw_class1, raw_class2, sep = ";")
  txt <- toupper(pt_chr(txt))

  tokens <- unique(unlist(strsplit(txt, "[^A-Z0-9]+")))
  tokens <- tokens[nzchar(tokens)]

  if ("Z" %in% tokens || grepl("ASOS|AWOS|AIRPORT|METAR", txt)) {
    return("ASOS (airport)")
  }
  if ("G" %in% tokens || grepl("GOES", txt)) {
    return("GOES")
  }
  if ("R" %in% tokens || grepl("ALERT|EVENT", txt)) {
    return("ALERT (event only)")
  }
  if (grepl("^[A-Z]{3}$", id)) {
    return("ASOS (airport)")
  }
  "Other / not parsed"
}

pt_wrh_site_id <- function(id, source_class) {
  id <- toupper(pt_chr(id))
  if (source_class == "ASOS (airport)") {
    if (grepl("^[A-Z]{3}$", id)) return(paste0("K", id))
    if (grepl("^K[A-Z]{3}$", id)) return(id)
  }
  id
}

pt_low_site_candidates <- function(cnrfc_id, wrh_site_id) {
  ids <- unique(tolower(pt_chr(c(wrh_site_id, cnrfc_id))))
  ids[nzchar(ids)]
}

pt_low_url <- function(site_id) {
  # Vector-safe helper. This is used inside dplyr/ifelse-style calls, so it
  # must return one URL per input value rather than assuming a single site ID.
  site_id <- pt_chr(site_id)
  out <- rep(NA_character_, length(site_id))
  ok <- nzchar(site_id)
  out[ok] <- paste0("https://www.weather.gov/wrh/LowTimeseries?site=", pt_urlencode(tolower(site_id[ok])))
  out
}

pt_existing_catalog_base <- function(dat) {
  dat <- as_tibble(dat)
  n <- nrow(dat)
  if (n == 0) stop("Precip/weather station catalog has zero rows.")

  id <- pt_clean_id(pt_get_col(dat, "cnrfc_id"))
  display_name <- pt_chr(pt_get_col(dat, "display_name"))
  hint <- pt_chr(pt_get_col(dat, "precip_weather_station_type_hint"))
  transmission <- pt_chr(pt_get_col(dat, "datatransmission_types"))
  raw_class1 <- pt_chr(pt_get_col(dat, "raw_class1_values"))
  raw_class2 <- pt_chr(pt_get_col(dat, "raw_class2_values"))
  has_wru <- pt_bool(pt_get_col(dat, "has_wru_precip_products", FALSE))
  has_current_wru <- pt_bool(pt_get_col(dat, "has_current_wru_precip_product", has_wru))
  lat <- suppressWarnings(as.numeric(pt_get_col(dat, "lat", NA_real_)))
  lon <- suppressWarnings(as.numeric(pt_get_col(dat, "lon", NA_real_)))

  fallback_source_class <- vapply(
    seq_len(n),
    function(i) pt_source_class(id[[i]], hint[[i]], transmission[[i]], raw_class1[[i]], raw_class2[[i]]),
    character(1)
  )

  tibble(
    cnrfc_id = id,
    display_name_catalog = display_name,
    catalog_source_class = fallback_source_class,
    catalog_source_hint_raw = ifelse(nzchar(hint), hint, transmission),
    catalog_lat = lat,
    catalog_lon = lon,
    has_wru_precip_products = has_wru,
    has_current_wru_precip_product = has_current_wru
  ) %>%
    filter(nzchar(.data$cnrfc_id)) %>%
    group_by(.data$cnrfc_id) %>%
    summarise(
      display_name_catalog = pt_first_nonempty(.data$display_name_catalog),
      catalog_source_class = pt_first_nonempty(.data$catalog_source_class, default = "Other / not parsed"),
      catalog_source_hint_raw = paste(unique(.data$catalog_source_hint_raw[nzchar(.data$catalog_source_hint_raw)]), collapse = "; "),
      catalog_lat = pt_first_non_na_num(.data$catalog_lat),
      catalog_lon = pt_first_non_na_num(.data$catalog_lon),
      has_wru_precip_products = any(.data$has_wru_precip_products, na.rm = TRUE),
      has_current_wru_precip_product = any(.data$has_current_wru_precip_product, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      display_name_catalog = ifelse(is.na(.data$display_name_catalog), "", .data$display_name_catalog),
      catalog_source_class = ifelse(is.na(.data$catalog_source_class), "Other / not parsed", .data$catalog_source_class),
      catalog_source_hint_raw = ifelse(is.na(.data$catalog_source_hint_raw), "", .data$catalog_source_hint_raw)
    )
}

pt_make_exclusion_inventory <- function(base, onehourp_current, raw_precip, current_snapshot, river_catalog, candidate_ids) {
  all_ids <- sort(unique(pt_clean_id(c(
    base$cnrfc_id,
    onehourp_current$cnrfc_id,
    raw_precip$cnrfc_id,
    current_snapshot$cnrfc_id,
    river_catalog$cnrfc_id
  ))))
  all_ids <- all_ids[nzchar(all_ids)]

  tibble(cnrfc_id = all_ids) %>%
    left_join(base, by = "cnrfc_id") %>%
    left_join(onehourp_current, by = "cnrfc_id") %>%
    left_join(raw_precip, by = "cnrfc_id") %>%
    left_join(current_snapshot, by = "cnrfc_id") %>%
    left_join(river_catalog, by = "cnrfc_id") %>%
    mutate(
      in_current_onehourp_kml = ifelse(is.na(.data$in_current_onehourp_kml), FALSE, .data$in_current_onehourp_kml),
      in_class_coded_precip_station_file = ifelse(is.na(.data$in_class_coded_precip_station_file), FALSE, .data$in_class_coded_precip_station_file),
      in_current_nwsid_snapshot = ifelse(is.na(.data$in_current_nwsid_snapshot), FALSE, .data$in_current_nwsid_snapshot),
      in_cnrfc_river_reservoir_catalog = ifelse(is.na(.data$in_cnrfc_river_reservoir_catalog), FALSE, .data$in_cnrfc_river_reservoir_catalog),
      has_wru_precip_products = ifelse(is.na(.data$has_wru_precip_products), FALSE, .data$has_wru_precip_products),
      has_current_wru_precip_product = ifelse(is.na(.data$has_current_wru_precip_product), FALSE, .data$has_current_wru_precip_product),
      audit_candidate_included = .data$cnrfc_id %in% candidate_ids,
      exclusion_reason = case_when(
        .data$audit_candidate_included ~ "included_in_station_audit_candidate_inventory",
        .data$in_class_coded_precip_station_file & !.data$in_current_onehourp_kml ~ "deferred_class_coded_historical_not_in_current_onehourP_kml",
        !.data$in_class_coded_precip_station_file & .data$in_cnrfc_river_reservoir_catalog ~ "deferred_river_reservoir_product_id_not_station_authority",
        !.data$in_class_coded_precip_station_file & .data$has_current_wru_precip_product ~ "deferred_wru_current_product_only_not_station_authority",
        !.data$in_class_coded_precip_station_file & .data$has_wru_precip_products ~ "deferred_wru_product_only_not_station_authority",
        !.data$in_class_coded_precip_station_file & .data$in_current_nwsid_snapshot ~ "deferred_current_nwsid_snapshot_only_not_station_authority",
        TRUE ~ "deferred_not_in_class_coded_precip_station_file"
      ),
      run_timestamp = RUN_TS
    ) %>%
    filter(!.data$audit_candidate_included)
}

pt_make_candidates <- function(dat,
                               raw_authority,
                               include_current_snapshot_extra_ids = FALSE,
                               include_class_coded_historical_extra_ids = FALSE,
                               use_current_onehourp_kml_as_authority = FALSE) {
  base <- pt_existing_catalog_base(dat)
  onehourp_current <- raw_authority$onehourp_current
  raw_precip <- raw_authority$precip_fixed
  current_snapshot <- raw_authority$current_snapshot
  river_catalog <- raw_authority$river_catalog

  if (isTRUE(use_current_onehourp_kml_as_authority) && nrow(onehourp_current) > 0) {
    warning("Using onehourP KML as station authority because use_current_onehourP_kml_as_authority=TRUE. This is not recommended for normal runs; onehourP KML can be empty or value-filtered.")
    seed <- onehourp_current %>%
      left_join(raw_precip, by = "cnrfc_id") %>%
      transmute(
        cnrfc_id = .data$cnrfc_id,
        raw_precip_source_code = .data$raw_precip_source_code,
        raw_precip_station_name = dplyr::coalesce(na_if(.data$raw_precip_station_name, ""), na_if(.data$onehourp_kml_name, "")),
        raw_precip_lat = dplyr::coalesce(.data$raw_precip_lat, .data$onehourp_kml_lat),
        raw_precip_lon = dplyr::coalesce(.data$raw_precip_lon, .data$onehourp_kml_lon),
        raw_precip_elev_ft = .data$raw_precip_elev_ft,
        raw_precip_state = .data$raw_precip_state,
        raw_precip_file = .data$raw_precip_file,
        in_class_coded_precip_station_file = ifelse(is.na(.data$in_class_coded_precip_station_file), FALSE, .data$in_class_coded_precip_station_file),
        raw_precip_source_detail = dplyr::coalesce(na_if(.data$raw_precip_source_detail, ""), "Current CNRFC onehourP KML observed-value feature; source class not in historical file"),
        raw_source_class = dplyr::coalesce(na_if(.data$raw_source_class, ""), NA_character_),
        in_current_onehourp_kml = TRUE,
        onehourp_kml_name = .data$onehourp_kml_name,
        onehourp_kml_description = .data$onehourp_kml_description,
        onehourp_kml_lat = .data$onehourp_kml_lat,
        onehourp_kml_lon = .data$onehourp_kml_lon,
        onehourp_kml_value = .data$onehourp_kml_value,
        onehourp_kml_url = .data$onehourp_kml_url,
        station_authority_basis = "current_cnrfc_onehourP_kml_observed_value_opt_in_authority"
      )

    if (isTRUE(include_class_coded_historical_extra_ids) && nrow(raw_precip) > 0) {
      extra_hist <- raw_precip %>%
        anti_join(seed %>% select(.data$cnrfc_id), by = "cnrfc_id") %>%
        mutate(
          in_current_onehourp_kml = FALSE,
          onehourp_kml_name = NA_character_,
          onehourp_kml_description = NA_character_,
          onehourp_kml_lat = NA_real_,
          onehourp_kml_lon = NA_real_,
          onehourp_kml_value = NA_character_,
          onehourp_kml_url = NA_character_,
          station_authority_basis = "class_coded_precip_weather_file_historical_extra_opt_in"
        )
      seed <- bind_rows(seed, extra_hist)
    }
  } else if (nrow(raw_precip) > 0) {
    if (nrow(onehourp_current) == 0) {
      warning(
        "Current CNRFC onehourP KML had zero parsed Placemark IDs. Treating that as observed-value evidence only and using class-coded gage_data_precip*.txt as station authority."
      )
    }
    seed <- raw_precip %>%
      mutate(
        in_current_onehourp_kml = FALSE,
        onehourp_kml_name = NA_character_,
        onehourp_kml_description = NA_character_,
        onehourp_kml_lat = NA_real_,
        onehourp_kml_lon = NA_real_,
        onehourp_kml_value = NA_character_,
        onehourp_kml_url = NA_character_,
        station_authority_basis = "class_coded_precip_weather_file_authority"
      )
  } else {
    warning(
      "Neither current onehourP KML nor class-coded gage_data_precip*.txt file was available. Falling back to the existing 53_ catalog, which may include WRU/product/basin IDs."
    )
    seed <- base %>%
      transmute(
        cnrfc_id = .data$cnrfc_id,
        raw_precip_source_code = NA_character_,
        raw_precip_station_name = .data$display_name_catalog,
        raw_precip_lat = .data$catalog_lat,
        raw_precip_lon = .data$catalog_lon,
        raw_precip_elev_ft = NA_real_,
        raw_precip_state = NA_character_,
        raw_precip_file = NA_character_,
        in_class_coded_precip_station_file = FALSE,
        raw_precip_source_detail = "Fallback from 53_ catalog; raw station authority missing",
        raw_source_class = .data$catalog_source_class,
        in_current_onehourp_kml = FALSE,
        onehourp_kml_name = NA_character_,
        onehourp_kml_description = NA_character_,
        onehourp_kml_lat = NA_real_,
        onehourp_kml_lon = NA_real_,
        onehourp_kml_value = NA_character_,
        onehourp_kml_url = NA_character_,
        station_authority_basis = "fallback_existing_53_catalog"
      )
  }

  if (isTRUE(include_current_snapshot_extra_ids) && nrow(current_snapshot) > 0) {
    extra <- current_snapshot %>%
      anti_join(seed %>% select(.data$cnrfc_id), by = "cnrfc_id") %>%
      anti_join(river_catalog %>% select(.data$cnrfc_id), by = "cnrfc_id") %>%
      filter(!.data$current_snapshot_zero_coord) %>%
      transmute(
        cnrfc_id = .data$cnrfc_id,
        raw_precip_source_code = NA_character_,
        raw_precip_station_name = NA_character_,
        raw_precip_lat = .data$current_snapshot_lat,
        raw_precip_lon = .data$current_snapshot_lon,
        raw_precip_elev_ft = NA_real_,
        raw_precip_state = NA_character_,
        raw_precip_file = NA_character_,
        in_class_coded_precip_station_file = FALSE,
        raw_precip_source_detail = "Current NWSID snapshot extra; not primary station authority",
        raw_source_class = "Other / not parsed",
        in_current_onehourp_kml = FALSE,
        onehourp_kml_name = NA_character_,
        onehourp_kml_description = NA_character_,
        onehourp_kml_lat = NA_real_,
        onehourp_kml_lon = NA_real_,
        onehourp_kml_value = NA_character_,
        onehourp_kml_url = NA_character_,
        station_authority_basis = "current_snapshot_extra_nonriver_opt_in"
      )
    seed <- bind_rows(seed, extra)
  }

  candidates <- seed %>%
    left_join(base, by = "cnrfc_id") %>%
    left_join(current_snapshot, by = "cnrfc_id") %>%
    left_join(river_catalog, by = "cnrfc_id") %>%
    mutate(
      in_current_onehourp_kml = ifelse(is.na(.data$in_current_onehourp_kml), FALSE, .data$in_current_onehourp_kml),
      in_class_coded_precip_station_file = ifelse(is.na(.data$in_class_coded_precip_station_file), FALSE, .data$in_class_coded_precip_station_file),
      in_current_nwsid_snapshot = ifelse(is.na(.data$in_current_nwsid_snapshot), FALSE, .data$in_current_nwsid_snapshot),
      current_snapshot_zero_coord = ifelse(is.na(.data$current_snapshot_zero_coord), NA, .data$current_snapshot_zero_coord),
      in_cnrfc_river_reservoir_catalog = ifelse(is.na(.data$in_cnrfc_river_reservoir_catalog), FALSE, .data$in_cnrfc_river_reservoir_catalog),
      has_wru_precip_products = ifelse(is.na(.data$has_wru_precip_products), FALSE, .data$has_wru_precip_products),
      has_current_wru_precip_product = ifelse(is.na(.data$has_current_wru_precip_product), FALSE, .data$has_current_wru_precip_product),
      display_name = dplyr::coalesce(
        na_if(.data$raw_precip_station_name, ""),
        na_if(.data$display_name_catalog, ""),
        .data$cnrfc_id
      ),
      source_class = mapply(
        pt_source_class,
        .data$cnrfc_id,
        .data$catalog_source_hint_raw,
        "",
        "",
        "",
        .data$raw_source_class,
        USE.NAMES = FALSE
      ),
      source_hint_raw = trimws(paste(
        ifelse(!is.na(.data$raw_precip_source_code) & nzchar(.data$raw_precip_source_code), paste0("raw_code=", .data$raw_precip_source_code), ""),
        ifelse(!is.na(.data$raw_precip_source_detail) & nzchar(.data$raw_precip_source_detail), .data$raw_precip_source_detail, ""),
        ifelse(!is.na(.data$catalog_source_hint_raw) & nzchar(.data$catalog_source_hint_raw), paste0("catalog=", .data$catalog_source_hint_raw), ""),
        sep = "; "
      )),
      lat = dplyr::coalesce(.data$onehourp_kml_lat, .data$raw_precip_lat, .data$current_snapshot_lat, .data$catalog_lat, .data$river_catalog_lat),
      lon = dplyr::coalesce(.data$onehourp_kml_lon, .data$raw_precip_lon, .data$current_snapshot_lon, .data$catalog_lon, .data$river_catalog_lon),
      station_authority_note = case_when(
        .data$in_current_onehourp_kml & .data$in_class_coded_precip_station_file ~ "current CNRFC onehourP map station; also found in class-coded historical precip/weather file",
        .data$in_current_onehourp_kml ~ "current CNRFC onehourP map station; not matched to class-coded historical file",
        .data$in_class_coded_precip_station_file & .data$in_cnrfc_river_reservoir_catalog ~ "class-coded precip/weather station; also appears in CNRFC river/reservoir catalog",
        .data$in_class_coded_precip_station_file ~ "class-coded CNRFC precip/weather station",
        .data$station_authority_basis == "current_snapshot_extra_nonriver_opt_in" ~ "current NWSID snapshot extra; opt-in only",
        TRUE ~ "fallback candidate; raw station authority missing"
      ),
      wrh_site_id = mapply(pt_wrh_site_id, .data$cnrfc_id, .data$source_class, USE.NAMES = FALSE),
      low_wrh_site_id = tolower(.data$wrh_site_id),
      low_original_site_id = tolower(.data$cnrfc_id),
      station_probe_scope = case_when(
        .data$source_class %in% c("ASOS (airport)", "GOES") ~ "station_probe_priority",
        .data$source_class == "ALERT (event only)" ~ "alert_event_defer",
        TRUE ~ "station_candidate_low_conf"
      ),
      probe_eligible_default = .data$station_probe_scope == "station_probe_priority",
      noaa_link_audit_priority = case_when(
        .data$source_class == "ASOS (airport)" ~ 1L,
        .data$source_class == "GOES" ~ 2L,
        .data$source_class == "ALERT (event only)" ~ 4L,
        TRUE ~ 5L
      ),
      noaa_link_audit_priority_label = case_when(
        .data$source_class == "ASOS (airport)" ~ "ASOS/airport: test K-prefixed WRH user URL and NWS API observation endpoints first",
        .data$source_class == "GOES" ~ "GOES: test five-character CNRFC/NWS ID with WRH user URL and NWS API observation endpoints first",
        .data$source_class == "ALERT (event only)" ~ "ALERT/event: retain in Local catalog; live API probing deferred by default",
        TRUE ~ "Other precip/weather: retain in Local catalog; test later or on demand"
      ),
      wrh_timeseries_url = ifelse(nzchar(.data$wrh_site_id), paste0("https://www.weather.gov/wrh/timeseries?site=", pt_urlencode(.data$wrh_site_id)), NA_character_),
      wrh_timeseries_tabular_url = ifelse(nzchar(.data$wrh_site_id), paste0("https://www.weather.gov/wrh/timeseries?site=", pt_urlencode(.data$wrh_site_id), "&hours=48&units=english&chart=off&headers=on&obs=tabular&hourly=false&pview=standard"), NA_character_),
      wrh_low_site_id = ifelse(nzchar(.data$low_wrh_site_id), .data$low_wrh_site_id, NA_character_),
      wrh_low_timeseries_url = ifelse(nzchar(.data$low_wrh_site_id), pt_low_url(.data$low_wrh_site_id), NA_character_),
      wrh_low_original_site_id = ifelse(nzchar(.data$low_original_site_id), .data$low_original_site_id, NA_character_),
      wrh_low_original_timeseries_url = ifelse(nzchar(.data$low_original_site_id), pt_low_url(.data$low_original_site_id), NA_character_),
      nws_api_latest_observation_url = ifelse(nzchar(.data$wrh_site_id), paste0("https://api.weather.gov/stations/", pt_urlencode(.data$wrh_site_id), "/observations/latest"), NA_character_),
      nws_api_recent_observations_url = ifelse(nzchar(.data$wrh_site_id), paste0("https://api.weather.gov/stations/", pt_urlencode(.data$wrh_site_id), "/observations?limit=1"), NA_character_),
      nws_rr5_rsa_source_text_url = "https://forecast.weather.gov/product.php?site=NWS&issuedby=RSA&product=RR5&format=ci&version=1&glossary=1",
      cnrfc_observed_precip_map_url = "https://www.cnrfc.noaa.gov/rainfall_data.php",
      run_timestamp = RUN_TS
    ) %>%
    select(-any_of(c("low_wrh_site_id", "low_original_site_id"))) %>%
    arrange(.data$noaa_link_audit_priority, .data$cnrfc_id) %>%
    distinct(.data$cnrfc_id, .keep_all = TRUE)

  exclusions <- pt_make_exclusion_inventory(base, onehourp_current, raw_precip, current_snapshot, river_catalog, candidates$cnrfc_id)

  list(candidates = candidates, exclusions = exclusions)
}

# ---- HTTP fetch and probe classification -----------------------------------

pt_fetch_text <- function(url, timeout_sec = 20L) {
  url <- pt_chr(url)
  if (!nzchar(url)) {
    return(list(ok = FALSE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = "blank URL"))
  }
  h <- curl::new_handle(
    followlocation = TRUE,
    timeout = timeout_sec,
    useragent = "BRIM CNRFC NOAA link audit (internal BRIM build; polite diagnostic)",
    httpheader = c(
      "Accept" = "text/html,application/json;q=0.9,*/*;q=0.8"
    )
  )
  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
  if (inherits(res, "error")) {
    return(list(ok = FALSE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = conditionMessage(res)))
  }
  txt <- tryCatch(rawToChar(res$content), error = function(e) "")
  list(
    ok = TRUE,
    status_code = as.integer(res$status_code),
    bytes = length(res$content),
    text = txt,
    error = NA_character_
  )
}

pt_detect_wrh <- function(status_code, text, site_id = "") {
  txt <- pt_chr(text)
  txt_low <- tolower(txt)
  site_low <- tolower(pt_chr(site_id))

  negative_hits <- pt_match_names(txt_low, pt_named_patterns(PT_WRH_NEGATIVE_PATTERNS))
  station_header <- grepl("weather conditions for", txt_low, fixed = TRUE) ||
    grepl("current conditions at", txt_low, fixed = TRUE) ||
    grepl("time series viewer:\\s*weather conditions", txt_low, perl = TRUE)
  site_id_present <- nzchar(site_low) && grepl(site_low, txt_low, fixed = TRUE)
  shell_loaded <- grepl("time series viewer", txt_low, fixed = TRUE) ||
    grepl("station identifier as a variable", txt_low, fixed = TRUE)
  generic_about_page <- grepl("general information", txt_low, fixed = TRUE) &&
    grepl("data availability varies by station", txt_low, fixed = TRUE)
  data_term_hits <- PT_STATION_DATA_TERMS[grepl(paste(PT_STATION_DATA_TERMS, collapse = "|"), txt_low, ignore.case = TRUE)]

  positive_hits <- c(
    if (station_header) "station_header" else character(0),
    if (site_id_present) "site_id_present" else character(0),
    if (shell_loaded) "time_series_viewer_shell" else character(0),
    if (generic_about_page) "generic_about_page" else character(0),
    if (length(data_term_hits) > 0) paste0("data_terms:", paste(unique(data_term_hits), collapse = ",")) else character(0)
  )

  class <- if (!pt_status_ok(status_code)) {
    "http_not_ok"
  } else if (length(negative_hits) > 0) {
    "wrh_negative_or_invalid_station_text"
  } else if (station_header) {
    "wrh_station_specific_static_page_loaded"
  } else if (shell_loaded) {
    "wrh_shell_or_help_loaded_not_station_verified"
  } else {
    "page_loaded_unclassified"
  }

  list(
    class = class,
    positive_signals = paste(unique(positive_hits), collapse = "; "),
    negative_signals = paste(unique(negative_hits), collapse = "; "),
    excerpt = pt_text_excerpt(txt)
  )
}

pt_detect_api <- function(status_code, text, endpoint = c("latest", "recent")) {
  endpoint <- match.arg(endpoint)
  txt <- pt_chr(text)
  txt_low <- tolower(txt)

  negative_hits <- pt_match_names(txt_low, pt_named_patterns(PT_API_NEGATIVE_PATTERNS))
  has_json_feature <- grepl('"type"\\s*:\\s*"feature"', txt_low, perl = TRUE) &&
    grepl('"properties"', txt_low, fixed = TRUE)
  has_feature_collection <- grepl('"type"\\s*:\\s*"featurecollection"', txt_low, perl = TRUE)
  has_nonempty_features <- grepl('"features"\\s*:\\s*\\[\\s*\\{', txt_low, perl = TRUE)
  has_timestamp <- grepl('"timestamp"', txt_low, fixed = TRUE)

  positive_hits <- c(
    if (has_json_feature) "geojson_feature" else character(0),
    if (has_feature_collection) "geojson_feature_collection" else character(0),
    if (has_nonempty_features) "nonempty_features" else character(0),
    if (has_timestamp) "observation_timestamp" else character(0)
  )

  class <- if (!pt_status_ok(status_code)) {
    "http_not_ok"
  } else if (length(negative_hits) > 0) {
    "api_negative_or_invalid_station_text"
  } else if (endpoint == "latest" && has_json_feature && has_timestamp) {
    "api_latest_observation_loaded"
  } else if (endpoint == "recent" && has_feature_collection && has_nonempty_features) {
    "api_recent_observation_loaded"
  } else if (endpoint == "recent" && has_feature_collection && !has_nonempty_features) {
    "api_recent_observations_empty"
  } else {
    "api_loaded_unclassified"
  }

  list(
    class = class,
    positive_signals = paste(unique(positive_hits), collapse = "; "),
    negative_signals = paste(unique(negative_hits), collapse = "; "),
    excerpt = pt_text_excerpt(txt)
  )
}

pt_best_wrh_static_class <- function(main_class, tabular_class, low_class, low_original_class) {
  classes <- pt_chr(c(main_class, tabular_class, low_class, low_original_class))
  if (any(classes == "wrh_station_specific_static_page_loaded", na.rm = TRUE)) {
    return("wrh_station_specific_static_page_loaded")
  }
  if (any(classes == "wrh_shell_or_help_loaded_not_station_verified", na.rm = TRUE)) {
    return("wrh_shell_or_help_loaded_not_station_verified")
  }
  if (any(classes == "wrh_negative_or_invalid_station_text", na.rm = TRUE)) {
    return("wrh_negative_or_invalid_station_text")
  }
  if (any(classes == "page_loaded_unclassified", na.rm = TRUE)) {
    return("page_loaded_unclassified")
  }
  "http_not_ok"
}

pt_final_class <- function(best_wrh_class, api_latest_class, api_recent_class, source_class) {
  if (api_latest_class == "api_latest_observation_loaded") {
    return("verified_api_latest_observation")
  }
  if (api_recent_class == "api_recent_observation_loaded") {
    return("verified_api_recent_observation")
  }
  if (best_wrh_class == "wrh_station_specific_static_page_loaded") {
    return("verified_wrh_static_station_page")
  }
  if (best_wrh_class == "wrh_shell_or_help_loaded_not_station_verified" && source_class %in% c("ASOS (airport)", "GOES")) {
    return("candidate_wrh_user_link_api_not_verified")
  }
  if (best_wrh_class == "wrh_shell_or_help_loaded_not_station_verified") {
    return("low_confidence_wrh_user_link_candidate")
  }
  if (best_wrh_class == "wrh_negative_or_invalid_station_text") {
    return("wrh_static_probe_negative_api_not_verified")
  }
  "not_verified"
}

# ---- Cache helpers ----------------------------------------------------------

pt_cache_fresh <- function(dat, max_age_days) {
  if (!nrow(dat) || !"probe_timestamp" %in% names(dat)) return(rep(FALSE, nrow(dat)))
  ts <- suppressWarnings(as.POSIXct(dat$probe_timestamp, tz = "UTC"))
  age_ok <- !is.na(ts) & as.numeric(difftime(Sys.time(), ts, units = "days")) <= max_age_days
  if ("probe_schema_version" %in% names(dat)) {
    schema_ok <- pt_chr(dat$probe_schema_version) == CACHE_SCHEMA_VERSION
  } else {
    schema_ok <- rep(FALSE, nrow(dat))
  }
  age_ok & schema_ok
}

pt_select_probe_ids <- function(candidates, cache, max_live_checks, force_refresh, force_refresh_ids, diagnostic_ids) {
  force_refresh_ids <- unique(pt_clean_id(force_refresh_ids))
  diagnostic_ids <- unique(pt_clean_id(diagnostic_ids))
  diagnostic_ids <- diagnostic_ids[nzchar(diagnostic_ids)]

  force_refresh_ids <- unique(c(force_refresh_ids, diagnostic_ids))

  cache_fresh_ids <- character(0)
  if (nrow(cache) > 0 && !force_refresh) {
    cache <- cache %>% filter(.data$probe_is_fresh)
    cache_fresh_ids <- unique(pt_clean_id(cache$cnrfc_id))
  }

  target <- candidates %>%
    mutate(
      force_refresh_target = .data$cnrfc_id %in% force_refresh_ids |
        .data$wrh_site_id %in% force_refresh_ids,
      diagnostic_probe = .data$cnrfc_id %in% diagnostic_ids |
        .data$wrh_site_id %in% diagnostic_ids,
      already_fresh = .data$cnrfc_id %in% cache_fresh_ids,
      default_probe_ok = ifelse("probe_eligible_default" %in% names(.), .data$probe_eligible_default, TRUE)
    ) %>%
    filter(.data$force_refresh_target | .data$diagnostic_probe | (.data$default_probe_ok & !.data$already_fresh)) %>%
    arrange(desc(.data$diagnostic_probe), .data$noaa_link_audit_priority, desc(.data$in_class_coded_precip_station_file), .data$cnrfc_id)

  if (is.finite(max_live_checks)) {
    target <- target %>% head(as.integer(max_live_checks))
  }
  target$cnrfc_id
}

# ---- Live probe -------------------------------------------------------------

pt_probe_candidates <- function(candidates, ids, request_delay_sec, timeout_sec, include_api_latest) {
  ids <- unique(pt_clean_id(ids))
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) return(tibble())

  rows <- candidates %>% filter(.data$cnrfc_id %in% ids)
  out <- vector("list", nrow(rows))

  for (i in seq_len(nrow(rows))) {
    row <- rows[i, ]
    pt_log("NOAA link probe [", i, "/", nrow(rows), "]: ", row$cnrfc_id, " -> ", row$wrh_site_id)

    wrh <- pt_fetch_text(row$wrh_timeseries_url, timeout_sec = timeout_sec)
    wrh_diag <- pt_detect_wrh(wrh$status_code, wrh$text, row$wrh_site_id)

    wrh_tabular <- pt_fetch_text(row$wrh_timeseries_tabular_url, timeout_sec = timeout_sec)
    wrh_tabular_diag <- pt_detect_wrh(wrh_tabular$status_code, wrh_tabular$text, row$wrh_site_id)

    wrh_low <- pt_fetch_text(row$wrh_low_timeseries_url, timeout_sec = timeout_sec)
    wrh_low_diag <- pt_detect_wrh(wrh_low$status_code, wrh_low$text, row$wrh_low_site_id)

    low_original_needed <- !identical(pt_chr(row$wrh_low_original_site_id), pt_chr(row$wrh_low_site_id))
    if (low_original_needed) {
      wrh_low_original <- pt_fetch_text(row$wrh_low_original_timeseries_url, timeout_sec = timeout_sec)
      wrh_low_original_diag <- pt_detect_wrh(wrh_low_original$status_code, wrh_low_original$text, row$wrh_low_original_site_id)
    } else {
      wrh_low_original <- list(status_code = NA_integer_, bytes = NA_integer_, text = "", error = NA_character_)
      wrh_low_original_diag <- list(class = "not_checked_same_as_wrh_site", positive_signals = "", negative_signals = "", excerpt = "")
    }

    api_latest_status <- NA_integer_
    api_latest_bytes <- NA_integer_
    api_latest_class <- "not_checked"
    api_latest_error <- NA_character_
    api_latest_positive <- ""
    api_latest_negative <- ""
    api_latest_excerpt <- ""

    api_recent_status <- NA_integer_
    api_recent_bytes <- NA_integer_
    api_recent_class <- "not_checked"
    api_recent_error <- NA_character_
    api_recent_positive <- ""
    api_recent_negative <- ""
    api_recent_excerpt <- ""

    if (isTRUE(include_api_latest)) {
      api_latest <- pt_fetch_text(row$nws_api_latest_observation_url, timeout_sec = timeout_sec)
      api_latest_diag <- pt_detect_api(api_latest$status_code, api_latest$text, endpoint = "latest")
      api_latest_status <- api_latest$status_code
      api_latest_bytes <- api_latest$bytes
      api_latest_class <- api_latest_diag$class
      api_latest_error <- api_latest$error
      api_latest_positive <- api_latest_diag$positive_signals
      api_latest_negative <- api_latest_diag$negative_signals
      api_latest_excerpt <- api_latest_diag$excerpt

      api_recent <- pt_fetch_text(row$nws_api_recent_observations_url, timeout_sec = timeout_sec)
      api_recent_diag <- pt_detect_api(api_recent$status_code, api_recent$text, endpoint = "recent")
      api_recent_status <- api_recent$status_code
      api_recent_bytes <- api_recent$bytes
      api_recent_class <- api_recent_diag$class
      api_recent_error <- api_recent$error
      api_recent_positive <- api_recent_diag$positive_signals
      api_recent_negative <- api_recent_diag$negative_signals
      api_recent_excerpt <- api_recent_diag$excerpt
    }

    best_wrh_class <- pt_best_wrh_static_class(
      wrh_diag$class,
      wrh_tabular_diag$class,
      wrh_low_diag$class,
      wrh_low_original_diag$class
    )
    final_class <- pt_final_class(best_wrh_class, api_latest_class, api_recent_class, row$source_class)

    out[[i]] <- tibble(
      cnrfc_id = row$cnrfc_id,
      wrh_site_id = row$wrh_site_id,
      source_class = row$source_class,
      has_wru_precip_products = row$has_wru_precip_products,
      has_current_wru_precip_product = row$has_current_wru_precip_product,
      wrh_timeseries_url = row$wrh_timeseries_url,
      wrh_status_code = wrh$status_code,
      wrh_bytes = wrh$bytes,
      wrh_availability_class = wrh_diag$class,
      wrh_positive_signals = wrh_diag$positive_signals,
      wrh_negative_signals = wrh_diag$negative_signals,
      wrh_excerpt = wrh_diag$excerpt,
      wrh_probe_error = wrh$error,
      wrh_timeseries_tabular_url = row$wrh_timeseries_tabular_url,
      wrh_tabular_status_code = wrh_tabular$status_code,
      wrh_tabular_bytes = wrh_tabular$bytes,
      wrh_tabular_availability_class = wrh_tabular_diag$class,
      wrh_tabular_positive_signals = wrh_tabular_diag$positive_signals,
      wrh_tabular_negative_signals = wrh_tabular_diag$negative_signals,
      wrh_tabular_excerpt = wrh_tabular_diag$excerpt,
      wrh_tabular_probe_error = wrh_tabular$error,
      wrh_low_site_id = row$wrh_low_site_id,
      wrh_low_timeseries_url = row$wrh_low_timeseries_url,
      wrh_low_status_code = wrh_low$status_code,
      wrh_low_bytes = wrh_low$bytes,
      wrh_low_availability_class = wrh_low_diag$class,
      wrh_low_positive_signals = wrh_low_diag$positive_signals,
      wrh_low_negative_signals = wrh_low_diag$negative_signals,
      wrh_low_excerpt = wrh_low_diag$excerpt,
      wrh_low_probe_error = wrh_low$error,
      wrh_low_original_site_id = row$wrh_low_original_site_id,
      wrh_low_original_timeseries_url = row$wrh_low_original_timeseries_url,
      wrh_low_original_status_code = wrh_low_original$status_code,
      wrh_low_original_bytes = wrh_low_original$bytes,
      wrh_low_original_availability_class = wrh_low_original_diag$class,
      wrh_low_original_positive_signals = wrh_low_original_diag$positive_signals,
      wrh_low_original_negative_signals = wrh_low_original_diag$negative_signals,
      wrh_low_original_excerpt = wrh_low_original_diag$excerpt,
      wrh_low_original_probe_error = wrh_low_original$error,
      wrh_best_static_availability_class = best_wrh_class,
      nws_api_latest_observation_url = row$nws_api_latest_observation_url,
      api_status_code = api_latest_status,
      api_bytes = api_latest_bytes,
      api_availability_class = api_latest_class,
      api_positive_signals = api_latest_positive,
      api_negative_signals = api_latest_negative,
      api_excerpt = api_latest_excerpt,
      api_probe_error = api_latest_error,
      nws_api_recent_observations_url = row$nws_api_recent_observations_url,
      api_recent_status_code = api_recent_status,
      api_recent_bytes = api_recent_bytes,
      api_recent_availability_class = api_recent_class,
      api_recent_positive_signals = api_recent_positive,
      api_recent_negative_signals = api_recent_negative,
      api_recent_excerpt = api_recent_excerpt,
      api_recent_probe_error = api_recent_error,
      noaa_station_data_availability_class = final_class,
      probe_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      probe_schema_version = CACHE_SCHEMA_VERSION,
      run_timestamp = RUN_TS
    )

    if (request_delay_sec > 0) Sys.sleep(request_delay_sec)
  }

  bind_rows(out)
}

# ---- Options ----------------------------------------------------------------

MAX_LIVE_CHECKS <- getOption("BRIM_CNRFC_NOAA_LINK_MAX_LIVE_CHECKS", 30L)
FORCE_REFRESH <- isTRUE(getOption("BRIM_CNRFC_NOAA_LINK_FORCE_REFRESH", FALSE))
FORCE_REFRESH_IDS <- getOption("BRIM_CNRFC_NOAA_LINK_FORCE_REFRESH_IDS", character(0))
DIAGNOSTIC_IDS <- getOption("BRIM_CNRFC_NOAA_LINK_DIAGNOSTIC_IDS", PT_KNOWN_DIAGNOSTIC_IDS)
CACHE_MAX_AGE_DAYS <- getOption("BRIM_CNRFC_NOAA_LINK_CACHE_MAX_AGE_DAYS", 14L)
REQUEST_DELAY_SEC <- getOption("BRIM_CNRFC_NOAA_LINK_REQUEST_DELAY_SEC", 0.05)
TIMEOUT_SEC <- getOption("BRIM_CNRFC_NOAA_LINK_TIMEOUT_SEC", 20L)
INCLUDE_API_LATEST <- isTRUE(getOption("BRIM_CNRFC_NOAA_LINK_INCLUDE_API_LATEST", TRUE))
USE_CURRENT_ONEHOURP_KML <- isTRUE(getOption("BRIM_CNRFC_NOAA_LINK_USE_CURRENT_ONEHOURP_KML", TRUE))
USE_CURRENT_ONEHOURP_KML_AS_AUTHORITY <- isTRUE(getOption("BRIM_CNRFC_NOAA_LINK_USE_CURRENT_ONEHOURP_KML_AS_AUTHORITY", FALSE))
INCLUDE_CURRENT_SNAPSHOT_EXTRA_IDS <- isTRUE(getOption("BRIM_CNRFC_NOAA_LINK_INCLUDE_CURRENT_SNAPSHOT_EXTRA_IDS", FALSE))
INCLUDE_CLASS_CODED_HISTORICAL_EXTRA_IDS <- isTRUE(getOption("BRIM_CNRFC_NOAA_LINK_INCLUDE_CLASS_CODED_HISTORICAL_EXTRA_IDS", FALSE))

pt_log("NOAA/NWS link audit options:")
pt_log("  max_live_checks    = ", MAX_LIVE_CHECKS)
pt_log("  force_refresh      = ", FORCE_REFRESH)
pt_log("  force_refresh_ids  = ", paste(pt_clean_id(FORCE_REFRESH_IDS), collapse = ", "))
pt_log("  diagnostic_ids     = ", paste(pt_clean_id(DIAGNOSTIC_IDS), collapse = ", "))
pt_log("  include_api_latest = ", INCLUDE_API_LATEST)
pt_log("  use_current_onehourP_kml = ", USE_CURRENT_ONEHOURP_KML)
pt_log("  use_current_onehourP_kml_as_authority = ", USE_CURRENT_ONEHOURP_KML_AS_AUTHORITY)
pt_log("  include_current_snapshot_extra_ids = ", INCLUDE_CURRENT_SNAPSHOT_EXTRA_IDS)
pt_log("  include_class_coded_historical_extra_ids = ", INCLUDE_CLASS_CODED_HISTORICAL_EXTRA_IDS)

# ---- Build candidates -------------------------------------------------------

catalog <- pt_read_precip_weather_catalog()
pt_log("Precip/weather station catalog rows from 53_: ", nrow(catalog))

raw_authority <- pt_read_cnrfc_raw_authority(use_current_onehourp_kml = USE_CURRENT_ONEHOURP_KML, kml_timeout_sec = TIMEOUT_SEC)
made <- pt_make_candidates(
  catalog,
  raw_authority = raw_authority,
  include_current_snapshot_extra_ids = INCLUDE_CURRENT_SNAPSHOT_EXTRA_IDS,
  include_class_coded_historical_extra_ids = INCLUDE_CLASS_CODED_HISTORICAL_EXTRA_IDS,
  use_current_onehourp_kml_as_authority = USE_CURRENT_ONEHOURP_KML_AS_AUTHORITY
)
candidates <- made$candidates
excluded_ids <- made$exclusions
station_inventory <- candidates

pt_log("NOAA/NWS precip/weather station audit candidate IDs: ", nrow(candidates))
pt_log("Deferred / excluded broader CNRFC IDs: ", nrow(excluded_ids))

candidate_bins <- candidates %>%
  count(
    .data$source_class,
    .data$raw_precip_source_code,
    .data$station_probe_scope,
    .data$station_authority_basis,
    .data$in_current_onehourp_kml,
    .data$in_current_nwsid_snapshot,
    .data$in_cnrfc_river_reservoir_catalog,
    .data$noaa_link_audit_priority_label,
    name = "stations"
  ) %>%
  arrange(.data$source_class, .data$raw_precip_source_code, desc(.data$stations)) %>%
  mutate(run_timestamp = RUN_TS)

exclusion_bins <- excluded_ids %>%
  count(
    .data$exclusion_reason,
    .data$in_current_nwsid_snapshot,
    .data$in_cnrfc_river_reservoir_catalog,
    .data$has_wru_precip_products,
    name = "ids"
  ) %>%
  arrange(desc(.data$ids)) %>%
  mutate(run_timestamp = RUN_TS)

# ---- Cache and live probe ---------------------------------------------------

cache_path <- file.path(DIR$qa, "cnrfc_precip_weather_noaa_link_probe_cache.csv")
cache <- pt_read_csv_or_empty(cache_path)
if (nrow(cache) > 0) {
  cache <- cache %>%
    mutate(
      cnrfc_id = pt_clean_id(.data$cnrfc_id),
      probe_is_fresh = pt_cache_fresh(., CACHE_MAX_AGE_DAYS)
    )
} else {
  cache <- tibble()
}

probe_ids <- pt_select_probe_ids(
  candidates = candidates,
  cache = cache,
  max_live_checks = MAX_LIVE_CHECKS,
  force_refresh = FORCE_REFRESH,
  force_refresh_ids = FORCE_REFRESH_IDS,
  diagnostic_ids = DIAGNOSTIC_IDS
)
pt_log("NOAA/NWS link probe IDs selected this run: ", length(probe_ids))

probe_new <- pt_probe_candidates(
  candidates = candidates,
  ids = probe_ids,
  request_delay_sec = REQUEST_DELAY_SEC,
  timeout_sec = TIMEOUT_SEC,
  include_api_latest = INCLUDE_API_LATEST
)

cache_base <- if (nrow(cache) > 0) cache %>% select(-any_of("probe_is_fresh")) else tibble()
cache_updated <- bind_rows(cache_base, probe_new) %>%
  mutate(cnrfc_id = pt_clean_id(.data$cnrfc_id)) %>%
  arrange(desc(.data$probe_timestamp)) %>%
  distinct(.data$cnrfc_id, .keep_all = TRUE)

readr::write_csv(cache_updated, cache_path, na = "")

probe_current <- cache_updated %>%
  mutate(probe_is_fresh = pt_cache_fresh(., CACHE_MAX_AGE_DAYS))

# ---- Join and summarize -----------------------------------------------------

probe_join_cols <- c(
  "cnrfc_id",
  "wrh_status_code",
  "wrh_bytes",
  "wrh_availability_class",
  "wrh_positive_signals",
  "wrh_negative_signals",
  "wrh_excerpt",
  "wrh_timeseries_tabular_url",
  "wrh_tabular_status_code",
  "wrh_tabular_bytes",
  "wrh_tabular_availability_class",
  "wrh_tabular_positive_signals",
  "wrh_tabular_negative_signals",
  "wrh_tabular_excerpt",
  "wrh_low_site_id",
  "wrh_low_timeseries_url",
  "wrh_low_status_code",
  "wrh_low_bytes",
  "wrh_low_availability_class",
  "wrh_low_positive_signals",
  "wrh_low_negative_signals",
  "wrh_low_excerpt",
  "wrh_low_original_site_id",
  "wrh_low_original_timeseries_url",
  "wrh_low_original_status_code",
  "wrh_low_original_bytes",
  "wrh_low_original_availability_class",
  "wrh_low_original_positive_signals",
  "wrh_low_original_negative_signals",
  "wrh_low_original_excerpt",
  "wrh_best_static_availability_class",
  "api_status_code",
  "api_bytes",
  "api_availability_class",
  "api_positive_signals",
  "api_negative_signals",
  "api_excerpt",
  "api_recent_status_code",
  "api_recent_bytes",
  "api_recent_availability_class",
  "api_recent_positive_signals",
  "api_recent_negative_signals",
  "api_recent_excerpt",
  "noaa_station_data_availability_class",
  "probe_timestamp",
  "probe_is_fresh",
  "probe_schema_version"
)

matrix <- candidates %>%
  left_join(
    probe_current %>% select(any_of(probe_join_cols)),
    by = "cnrfc_id"
  ) %>%
  mutate(
    wrh_timeseries_probe_status = case_when(
      is.na(.data$wrh_best_static_availability_class) ~ "not_checked_yet",
      .data$wrh_best_static_availability_class == "wrh_station_specific_static_page_loaded" ~ "WRH static station page loaded",
      .data$wrh_best_static_availability_class == "wrh_shell_or_help_loaded_not_station_verified" ~ "WRH shell/help loaded; station data not statically verified",
      .data$wrh_best_static_availability_class == "wrh_negative_or_invalid_station_text" ~ "WRH static probe returned negative/invalid text",
      TRUE ~ .data$wrh_best_static_availability_class
    ),
    noaa_timeseries_map_candidate = case_when(
      .data$noaa_station_data_availability_class == "verified_api_latest_observation" ~ "verified NWS API latest observation; use WRH user link in popup",
      .data$noaa_station_data_availability_class == "verified_api_recent_observation" ~ "verified NWS API recent observation; use WRH user link in popup",
      .data$noaa_station_data_availability_class == "verified_wrh_static_station_page" ~ "verified WRH static station page; use WRH user link in popup",
      .data$noaa_station_data_availability_class == "candidate_wrh_user_link_api_not_verified" ~ "ASOS/GOES WRH user-link candidate; API not verified yet",
      .data$source_class %in% c("ASOS (airport)", "GOES") ~ "likely WRH time-series candidate; not verified yet",
      .data$station_probe_scope == "alert_event_defer" ~ "Local catalog station; live API probing deferred by default",
      TRUE ~ "Local catalog station; lower-priority time-series candidate"
    ),
    run_timestamp = RUN_TS
  )


# ---- Map-ready products -----------------------------------------------------

pt_first_nonblank <- function(...) {
  vals <- c(...)
  vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
  if (length(vals) == 0) return(NA_character_)
  vals[[1]]
}

pt_availability_confidence <- function(class_value, source_class, probe_scope) {
  class_value <- ifelse(is.na(class_value), "", class_value)
  source_class <- ifelse(is.na(source_class), "", source_class)
  probe_scope <- ifelse(is.na(probe_scope), "", probe_scope)

  dplyr::case_when(
    class_value %in% c("verified_api_latest_observation", "verified_api_recent_observation", "verified_wrh_static_station_page") ~ "verified_recent_station_data",
    class_value == "candidate_wrh_user_link_api_not_verified" ~ "station_link_candidate_checked_not_verified",
    source_class %in% c("ASOS (airport)", "GOES") ~ "high_confidence_station_link_candidate_not_checked",
    probe_scope == "alert_event_defer" ~ "local_catalog_alert_event_station_deferred",
    TRUE ~ "local_catalog_lower_confidence_station"
  )
}

pt_make_precip_weather_map_products <- function(matrix) {
  valid_xy <- matrix %>%
    filter(!is.na(.data$lat), !is.na(.data$lon),
           abs(.data$lat) <= 90, abs(.data$lon) <= 180,
           !(.data$lat == 0 & .data$lon == 0)) %>%
    mutate(
      elev_ft = dplyr::coalesce(.data$raw_precip_elev_ft, .data$river_catalog_elev_ft),
      availability_confidence = pt_availability_confidence(
        .data$noaa_station_data_availability_class,
        .data$source_class,
        .data$station_probe_scope
      ),
      noaa_api_verified = .data$noaa_station_data_availability_class %in% c(
        "verified_api_latest_observation",
        "verified_api_recent_observation",
        "verified_wrh_static_station_page"
      ),
      noaa_api_checked_not_verified = .data$noaa_station_data_availability_class == "candidate_wrh_user_link_api_not_verified",
      noaa_api_not_checked = is.na(.data$noaa_station_data_availability_class),
      is_ops_timeseries_candidate = .data$station_probe_scope == "station_probe_priority",
      is_ops_timeseries_verified = .data$noaa_api_verified,
      is_ops_timeseries_checked_candidate = .data$noaa_api_checked_not_verified,
      is_ops_timeseries_unchecked_candidate = .data$is_ops_timeseries_candidate & .data$noaa_api_not_checked,
      map_station_role = case_when(
        .data$is_ops_timeseries_verified ~ "verified_noaa_station_timeseries",
        .data$is_ops_timeseries_checked_candidate ~ "candidate_noaa_station_timeseries_checked_not_verified",
        .data$is_ops_timeseries_unchecked_candidate ~ "candidate_noaa_station_timeseries_not_checked",
        .data$station_probe_scope == "alert_event_defer" ~ "local_catalog_alert_event_station",
        TRUE ~ "local_catalog_station_candidate"
      ),
      map_symbol_fill_class = case_when(
        .data$source_class == "GOES" ~ "goes",
        .data$source_class == "ASOS (airport)" ~ "asos",
        .data$source_class == "ALERT (event only)" ~ "alert",
        TRUE ~ "other"
      ),
      precip_weather_station_type_hint = .data$source_class,
      datatransmission_types = dplyr::coalesce(.data$raw_precip_source_code, .data$source_hint_raw),
      also_active_river_reservoir_forecast_point = .data$in_cnrfc_river_reservoir_catalog,
      cnrfc_source_url = dplyr::coalesce(.data$cnrfc_observed_precip_map_url, "https://www.cnrfc.noaa.gov/rainfall_data.php"),
      nws_api_latest_url = .data$nws_api_latest_observation_url,
      nws_api_recent_url = .data$nws_api_recent_observations_url,
      local_catalog_note = case_when(
        .data$source_class == "ALERT (event only)" ~ "Event/ALERT-style station retained for project-area screening; live API check deferred by default.",
        .data$source_class %in% c("ASOS (airport)", "GOES") ~ "Priority NWS/WRH time-series candidate from the CNRFC class-coded precip/weather station catalog.",
        TRUE ~ "Lower-confidence CNRFC precip/weather catalog station; review source links before relying on time-series availability."
      ),
      ops_live_note = case_when(
        .data$is_ops_timeseries_verified ~ "NWS API latest/recent observation verified; WRH time-series user link is included.",
        .data$is_ops_timeseries_checked_candidate ~ "WRH user-link candidate, but NWS API observation check did not verify recent data in this audit run.",
        .data$is_ops_timeseries_unchecked_candidate ~ "ASOS/GOES station-link candidate not checked in the current live-audit cache yet.",
        TRUE ~ "Not included in default Ops Live time-series candidate set."
      ),
      map_popup_note = case_when(
        .data$is_ops_timeseries_candidate ~ .data$ops_live_note,
        TRUE ~ .data$local_catalog_note
      )
    )

  keep_cols <- c(
    "cnrfc_id", "display_name", "lat", "lon", "elev_ft",
    "source_class", "raw_precip_source_code", "raw_precip_source_detail", "source_hint_raw",
    "precip_weather_station_type_hint", "datatransmission_types",
    "station_authority_basis", "station_authority_note",
    "station_probe_scope", "probe_eligible_default",
    "wrh_site_id", "wrh_timeseries_url", "wrh_timeseries_tabular_url",
    "nws_api_latest_url", "nws_api_recent_url",
    "api_availability_class", "api_recent_availability_class", "noaa_station_data_availability_class",
    "noaa_timeseries_map_candidate", "availability_confidence",
    "noaa_api_verified", "noaa_api_checked_not_verified", "noaa_api_not_checked",
    "is_ops_timeseries_candidate", "is_ops_timeseries_verified", "is_ops_timeseries_checked_candidate", "is_ops_timeseries_unchecked_candidate",
    "has_wru_precip_products", "has_current_wru_precip_product",
    "in_current_nwsid_snapshot", "in_cnrfc_river_reservoir_catalog", "also_active_river_reservoir_forecast_point",
    "in_current_onehourp_kml", "onehourp_kml_value", "onehourp_kml_url",
    "map_station_role", "map_symbol_fill_class", "cnrfc_source_url",
    "cnrfc_observed_precip_map_url", "nws_rr5_rsa_source_text_url",
    "local_catalog_note", "ops_live_note", "map_popup_note", "run_timestamp"
  )

  local_map <- valid_xy %>%
    select(any_of(keep_cols)) %>%
    arrange(.data$source_class, .data$cnrfc_id)

  ops_all_candidates <- local_map %>%
    filter(.data$is_ops_timeseries_candidate) %>%
    arrange(
      desc(.data$is_ops_timeseries_verified),
      desc(.data$is_ops_timeseries_checked_candidate),
      .data$source_class,
      .data$cnrfc_id
    )

  # Production Ops Live product: verified recent station-observation evidence only.
  # Checked-but-not-verified ASOS/GOES rows stay in QA/local products, but are not
  # drawn in Ops Live by default.
  ops_map <- ops_all_candidates %>%
    filter(.data$is_ops_timeseries_verified) %>%
    arrange(.data$source_class, .data$cnrfc_id)

  ops_checked_not_verified_qa <- ops_all_candidates %>%
    filter(.data$is_ops_timeseries_checked_candidate) %>%
    arrange(.data$source_class, .data$cnrfc_id)

  ops_unchecked_candidates_qa <- ops_all_candidates %>%
    filter(.data$is_ops_timeseries_unchecked_candidate) %>%
    arrange(.data$source_class, .data$cnrfc_id)

  list(
    local_map = local_map,
    ops_map = ops_map,
    ops_all_candidates = ops_all_candidates,
    ops_checked_not_verified_qa = ops_checked_not_verified_qa,
    ops_unchecked_candidates_qa = ops_unchecked_candidates_qa
  )
}

map_products <- pt_make_precip_weather_map_products(matrix)
local_catalog_map <- map_products$local_map
ops_timeseries_map <- map_products$ops_map
ops_timeseries_all_candidates_qa <- map_products$ops_all_candidates
ops_timeseries_checked_not_verified_qa <- map_products$ops_checked_not_verified_qa
ops_timeseries_unchecked_candidates_qa <- map_products$ops_unchecked_candidates_qa

map_product_bins <- bind_rows(
  local_catalog_map %>%
    count(product = "local_catalog", .data$source_class, .data$availability_confidence, name = "stations"),
  ops_timeseries_map %>%
    count(product = "ops_timeseries", .data$source_class, .data$availability_confidence, name = "stations")
) %>%
  arrange(.data$product, .data$source_class, desc(.data$stations)) %>%
  mutate(run_timestamp = RUN_TS)

bins <- matrix %>%
  count(
    .data$source_class,
    .data$has_wru_precip_products,
    .data$wrh_timeseries_probe_status,
    .data$wrh_best_static_availability_class,
    .data$api_availability_class,
    .data$api_recent_availability_class,
    .data$noaa_station_data_availability_class,
    .data$noaa_timeseries_map_candidate,
    name = "stations"
  ) %>%
  arrange(.data$source_class, desc(.data$stations)) %>%
  mutate(run_timestamp = RUN_TS)

diagnostic_ids_clean <- unique(pt_clean_id(DIAGNOSTIC_IDS))
known_station_diagnostic <- probe_current %>%
  filter(.data$cnrfc_id %in% diagnostic_ids_clean | .data$wrh_site_id %in% diagnostic_ids_clean) %>%
  arrange(match(.data$cnrfc_id, diagnostic_ids_clean), .data$cnrfc_id) %>%
  select(any_of(c(
    "cnrfc_id",
    "wrh_site_id",
    "source_class",
    "wrh_timeseries_url",
    "wrh_status_code",
    "wrh_bytes",
    "wrh_availability_class",
    "wrh_positive_signals",
    "wrh_negative_signals",
    "wrh_excerpt",
    "wrh_timeseries_tabular_url",
    "wrh_tabular_status_code",
    "wrh_tabular_availability_class",
    "wrh_tabular_positive_signals",
    "wrh_tabular_negative_signals",
    "wrh_tabular_excerpt",
    "wrh_low_timeseries_url",
    "wrh_low_status_code",
    "wrh_low_availability_class",
    "wrh_low_positive_signals",
    "wrh_low_negative_signals",
    "wrh_low_excerpt",
    "wrh_low_original_timeseries_url",
    "wrh_low_original_status_code",
    "wrh_low_original_availability_class",
    "wrh_low_original_positive_signals",
    "wrh_low_original_negative_signals",
    "wrh_low_original_excerpt",
    "wrh_best_static_availability_class",
    "nws_api_latest_observation_url",
    "api_status_code",
    "api_availability_class",
    "api_positive_signals",
    "api_negative_signals",
    "api_excerpt",
    "nws_api_recent_observations_url",
    "api_recent_status_code",
    "api_recent_availability_class",
    "api_recent_positive_signals",
    "api_recent_negative_signals",
    "api_recent_excerpt",
    "noaa_station_data_availability_class",
    "probe_timestamp",
    "probe_is_fresh",
    "probe_schema_version"
  )))

summary <- tibble(
  run_timestamp = RUN_TS,
  metric = c(
    "current_onehourP_kml_observed_value_placemark_ids",
    "current_onehourP_kml_ids_matched_to_class_coded_file",
    "class_coded_precip_weather_file_ids",
    "class_coded_ids_without_current_onehourP_observed_value",
    "current_nwsid_snapshot_ids",
    "river_reservoir_catalog_ids",
    "candidate_station_ids",
    "candidate_ids_also_in_river_reservoir_catalog",
    "default_probe_eligible_station_ids",
    "deferred_excluded_broader_cnrfc_ids",
    "deferred_class_coded_historical_not_in_current_onehourP_kml",
    "deferred_river_reservoir_only_ids",
    "deferred_current_snapshot_only_ids",
    "cache_rows_after_update",
    "probe_ids_selected_this_run",
    "fresh_probe_rows",
    "known_station_diagnostic_rows",
    "wrh_station_specific_static_pages_loaded",
    "wrh_shell_or_help_loaded_not_station_verified",
    "wrh_low_negative_or_invalid_responses",
    "api_latest_observation_loaded",
    "api_recent_observation_loaded",
    "verified_api_or_wrh_station_data",
    "local_catalog_map_rows",
    "ops_timeseries_map_rows",
    "ops_timeseries_all_candidate_rows",
    "ops_timeseries_verified_rows",
    "ops_timeseries_checked_not_verified_rows",
    "ops_timeseries_unchecked_candidate_rows",
    "asos_airport_candidates",
    "goes_candidates",
    "alert_event_candidates",
    "other_unknown_candidates"
  ),
  value = c(
    nrow(raw_authority$onehourp_current),
    sum(raw_authority$onehourp_current$cnrfc_id %in% raw_authority$precip_fixed$cnrfc_id, na.rm = TRUE),
    nrow(raw_authority$precip_fixed),
    sum(!raw_authority$precip_fixed$cnrfc_id %in% raw_authority$onehourp_current$cnrfc_id, na.rm = TRUE),
    nrow(raw_authority$current_snapshot),
    nrow(raw_authority$river_catalog),
    nrow(candidates),
    sum(candidates$in_cnrfc_river_reservoir_catalog, na.rm = TRUE),
    sum(candidates$probe_eligible_default, na.rm = TRUE),
    nrow(excluded_ids),
    sum(excluded_ids$exclusion_reason == "deferred_class_coded_historical_not_in_current_onehourP_kml", na.rm = TRUE),
    sum(excluded_ids$exclusion_reason == "deferred_river_reservoir_product_id_not_station_authority", na.rm = TRUE),
    sum(excluded_ids$exclusion_reason == "deferred_current_nwsid_snapshot_only_not_station_authority", na.rm = TRUE),
    nrow(cache_updated),
    length(probe_ids),
    sum(probe_current$probe_is_fresh, na.rm = TRUE),
    nrow(known_station_diagnostic),
    sum(matrix$wrh_best_static_availability_class == "wrh_station_specific_static_page_loaded", na.rm = TRUE),
    sum(matrix$wrh_best_static_availability_class == "wrh_shell_or_help_loaded_not_station_verified", na.rm = TRUE),
    sum(matrix$wrh_low_availability_class == "wrh_negative_or_invalid_station_text" |
          matrix$wrh_low_original_availability_class == "wrh_negative_or_invalid_station_text", na.rm = TRUE),
    sum(matrix$api_availability_class == "api_latest_observation_loaded", na.rm = TRUE),
    sum(matrix$api_recent_availability_class == "api_recent_observation_loaded", na.rm = TRUE),
    sum(matrix$noaa_station_data_availability_class %in% c(
      "verified_api_latest_observation",
      "verified_api_recent_observation",
      "verified_wrh_static_station_page"
    ), na.rm = TRUE),
    nrow(local_catalog_map),
    nrow(ops_timeseries_map),
    nrow(ops_timeseries_all_candidates_qa),
    sum(ops_timeseries_all_candidates_qa$is_ops_timeseries_verified, na.rm = TRUE),
    nrow(ops_timeseries_checked_not_verified_qa),
    nrow(ops_timeseries_unchecked_candidates_qa),
    sum(matrix$source_class == "ASOS (airport)", na.rm = TRUE),
    sum(matrix$source_class == "GOES", na.rm = TRUE),
    sum(matrix$source_class == "ALERT (event only)", na.rm = TRUE),
    sum(matrix$source_class == "Other / not parsed", na.rm = TRUE)
  )
)

# ---- Write outputs ----------------------------------------------------------

onehourp_rds <- pt_write_rds(raw_authority$onehourp_current, "cnrfc_precip_weather_current_onehourP_kml_inventory.rds")
inventory_rds <- pt_write_rds(station_inventory, "cnrfc_precip_weather_station_inventory.rds")
excluded_rds <- pt_write_rds(excluded_ids, "cnrfc_precip_weather_noaa_link_excluded_ids.rds")
raw_paths_rds <- pt_write_rds(raw_authority$paths, "cnrfc_precip_weather_raw_source_paths.rds")
cand_rds <- pt_write_rds(candidates, "cnrfc_precip_weather_noaa_link_candidates.rds")
probe_rds <- pt_write_rds(probe_current, "cnrfc_precip_weather_noaa_link_probe.rds")
matrix_rds <- pt_write_rds(matrix, "cnrfc_precip_weather_noaa_link_matrix.rds")
local_catalog_map_rds <- pt_write_rds(local_catalog_map, "cnrfc_precip_weather_station_catalog_local_map.rds")
ops_timeseries_map_rds <- pt_write_rds(ops_timeseries_map, "cnrfc_precip_weather_noaa_timeseries_ops_map.rds")
ops_timeseries_all_candidates_rds <- pt_write_rds(ops_timeseries_all_candidates_qa, "cnrfc_precip_weather_noaa_timeseries_ops_all_candidates_qa.rds")
ops_timeseries_checked_not_verified_rds <- pt_write_rds(ops_timeseries_checked_not_verified_qa, "cnrfc_precip_weather_noaa_timeseries_checked_not_verified_qa.rds")
ops_timeseries_unchecked_candidates_rds <- pt_write_rds(ops_timeseries_unchecked_candidates_qa, "cnrfc_precip_weather_noaa_timeseries_unchecked_candidates_qa.rds")
# Compatibility path used by the existing Ops Live map build.  This should now
# contain verified NOAA/NWS/WRH time-series rows only, not the older WRU-heavy catalog
# and not checked-but-not-verified ASOS/GOES candidates.
ops_timeseries_compat_rds <- pt_write_rds(ops_timeseries_map, "cnrfc_precip_weather_stations_map.rds")
map_product_bins_rds <- pt_write_rds(map_product_bins, "cnrfc_precip_weather_map_product_bins.rds")
bins_rds <- pt_write_rds(bins, "cnrfc_precip_weather_noaa_link_bins.rds")
summary_rds <- pt_write_rds(summary, "cnrfc_precip_weather_noaa_link_summary.rds")

pt_write_csv_pair(raw_authority$onehourp_current, "cnrfc_precip_weather_current_onehourP_kml_inventory")
pt_write_csv_pair(station_inventory, "cnrfc_precip_weather_station_inventory")
pt_write_csv_pair(excluded_ids, "cnrfc_precip_weather_noaa_link_excluded_ids")
pt_write_csv_pair(exclusion_bins, "cnrfc_precip_weather_noaa_link_exclusion_bins")
pt_write_csv_pair(raw_authority$paths, "cnrfc_precip_weather_raw_source_paths")
pt_write_csv_pair(candidates, "cnrfc_precip_weather_noaa_link_candidates")
pt_write_csv_pair(candidate_bins, "cnrfc_precip_weather_noaa_link_candidate_bins")
pt_write_csv_pair(probe_current, "cnrfc_precip_weather_noaa_link_probe")
pt_write_csv_pair(matrix, "cnrfc_precip_weather_noaa_link_matrix")
pt_write_csv_pair(local_catalog_map, "cnrfc_precip_weather_station_catalog_local_map")
pt_write_csv_pair(ops_timeseries_map, "cnrfc_precip_weather_noaa_timeseries_ops_map")
pt_write_csv_pair(ops_timeseries_all_candidates_qa, "cnrfc_precip_weather_noaa_timeseries_ops_all_candidates_qa")
pt_write_csv_pair(ops_timeseries_checked_not_verified_qa, "cnrfc_precip_weather_noaa_timeseries_checked_not_verified_qa")
pt_write_csv_pair(ops_timeseries_unchecked_candidates_qa, "cnrfc_precip_weather_noaa_timeseries_unchecked_candidates_qa")
pt_write_csv_pair(map_product_bins, "cnrfc_precip_weather_map_product_bins")
pt_write_csv_pair(bins, "cnrfc_precip_weather_noaa_link_bins")
pt_write_csv_pair(summary, "cnrfc_precip_weather_noaa_link_summary")
pt_write_csv_pair(known_station_diagnostic, "cnrfc_precip_weather_noaa_link_known_station_diagnostic")

# Stronger timestamped audit archive: compact, easy-to-recover CSV copies of
# the expensive live-audit products.  The normal *_latest.csv files remain for
# quick inspection; this archive is intended for durable bindable QA/history.
probe_archive_csv <- pt_write_csv_archive(probe_current, "cnrfc_precip_weather_noaa_link_probe")
matrix_archive_csv <- pt_write_csv_archive(matrix, "cnrfc_precip_weather_noaa_link_matrix")
local_catalog_archive_csv <- pt_write_csv_archive(local_catalog_map, "cnrfc_precip_weather_station_catalog_local_map")
ops_map_archive_csv <- pt_write_csv_archive(ops_timeseries_map, "cnrfc_precip_weather_noaa_timeseries_ops_map_verified_only")
ops_all_candidates_archive_csv <- pt_write_csv_archive(ops_timeseries_all_candidates_qa, "cnrfc_precip_weather_noaa_timeseries_ops_all_candidates_qa")
ops_checked_not_verified_archive_csv <- pt_write_csv_archive(ops_timeseries_checked_not_verified_qa, "cnrfc_precip_weather_noaa_timeseries_checked_not_verified_qa")

pt_log("CNRFC precip/weather NOAA/NWS link availability preprocess complete.")
message("  onehourP observed-value KML RDS: ", onehourp_rds, " (", nrow(raw_authority$onehourp_current), " rows)")
message("  Inventory RDS:  ", inventory_rds, " (", nrow(station_inventory), " rows)")
message("  Excluded RDS:   ", excluded_rds, " (", nrow(excluded_ids), " rows)")
message("  Candidates RDS: ", cand_rds, " (", nrow(candidates), " rows)")
message("  Probe RDS:      ", probe_rds, " (", nrow(probe_current), " rows)")
message("  Matrix RDS:     ", matrix_rds, " (", nrow(matrix), " rows)")
message("  Local map RDS:  ", local_catalog_map_rds, " (", nrow(local_catalog_map), " rows)")
message("  Ops map RDS:    ", ops_timeseries_map_rds, " (", nrow(ops_timeseries_map), " verified rows)")
message("  Ops all-candidate QA RDS: ", ops_timeseries_all_candidates_rds, " (", nrow(ops_timeseries_all_candidates_qa), " rows)")
message("  Ops checked-not-verified QA RDS: ", ops_timeseries_checked_not_verified_rds, " (", nrow(ops_timeseries_checked_not_verified_qa), " rows)")
message("  Ops unchecked-candidate QA RDS: ", ops_timeseries_unchecked_candidates_rds, " (", nrow(ops_timeseries_unchecked_candidates_qa), " rows)")
message("  Ops compat RDS: ", ops_timeseries_compat_rds, " (", nrow(ops_timeseries_map), " verified rows)")
message("  Audit archive folder: ", file.path(DIR$qa, "cnrfc_precip_weather_audit_archive"))
message("  Map bins RDS:   ", map_product_bins_rds, " (", nrow(map_product_bins), " rows)")
message("  Bins RDS:       ", bins_rds, " (", nrow(bins), " rows)")
message("  Summary RDS:    ", summary_rds, " (", nrow(summary), " rows)")

message("\nRaw source paths:")
print(raw_authority$paths, n = 10, width = Inf)

message("\nSource-class candidate bins:")
print(candidate_bins, n = 30, width = Inf)

message("\nDeferred/excluded ID bins:")
print(exclusion_bins, n = 30, width = Inf)

message("\nNOAA/NWS link availability bins:")
print(bins, n = 30, width = Inf)

message("\nKnown-station diagnostic rows:")
print(
  known_station_diagnostic %>%
    select(any_of(c(
      "cnrfc_id",
      "wrh_site_id",
      "source_class",
      "wrh_best_static_availability_class",
      "api_availability_class",
      "api_recent_availability_class",
      "noaa_station_data_availability_class"
    ))),
  n = 30,
  width = Inf
)

message("\nNOAA/NWS link availability summary:")
print(summary, n = 30, width = Inf)

message("\nInterpretation note:")
message("  The popup/user link remains the NWS WRH time-series page.")
message("  WRH pages are browser-oriented, so the static WRH probes are diagnostic only.")
message("  Official api.weather.gov latest/recent observations are used as stronger machine-readable evidence that station data are present.")
message("  The onehourP KML is an observed-value layer, not complete station authority; it can be empty during dry hours.")
message("  The class-coded gage_data_precip*.txt file is the station-authority source when available.")
message("  gage_data_precip_morecurrent*.txt is retained as context but does not expand the audit by default because it includes broader CNRFC IDs.")
message("  River/reservoir/product-only IDs are written to cnrfc_precip_weather_noaa_link_excluded_ids_latest.csv for review.")
message("  Ops Live map outputs now use verified recent NWS station-observation rows only.")
message("  Checked-but-not-verified ASOS/GOES rows are retained in QA/archive outputs and in the Local catalog, not Ops Live.")
message("  A separate precip-time-series availability mini-audit is scaffolded in 02_preprocess/55_build_cnrfc_precip_weather_precip_timeseries_availability.R.")
