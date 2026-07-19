# ==== qa_cnrfc_water_resources_update_kml_audit.r ============================
##
## PURPOSE:
##   Drill into CNRFC's Daily Water Resources Update page after the first
##   diagnostic showed that station IDs and product-link logic are embedded in
##   the page.  This audit inventories page controls, inline JavaScript ID
##   arrays, likely /data/kml/*.xml layers, marker IDs, and product links.
##
## OUTPUTS:
##   04_processed_data/qa/cnrfc_wru_control_inventory_<timestamp>.csv
##   04_processed_data/qa/cnrfc_wru_js_array_summary_<timestamp>.csv
##   04_processed_data/qa/cnrfc_wru_js_array_id_inventory_<timestamp>.csv
##   04_processed_data/qa/cnrfc_wru_kml_fetch_inventory_<timestamp>.csv
##   04_processed_data/qa/cnrfc_wru_kml_marker_id_inventory_<timestamp>.csv
##   04_processed_data/qa/cnrfc_wru_kml_product_links_<timestamp>.csv
##   04_processed_data/qa/cnrfc_wru_xml_product_records_<timestamp>.csv
##   04_processed_data/qa/cnrfc_wru_xml_product_matrix_<timestamp>.csv
##   04_processed_data/qa/cnrfc_wru_basin_match_matrix_<timestamp>.csv
##   04_processed_data/qa/cnrfc_wru_audit_summary_<timestamp>.csv

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
})

RUN_TS <- make_timestamp()
WRU_URL <- "https://www.cnrfc.noaa.gov/water_resources_update.php"
BASE_URL <- "https://www.cnrfc.noaa.gov"
MAX_KML_FILES <- as.integer(getOption("BRIM_CNRFC_WRU_KML_MAX_FILES", 100L))
FETCH_KML <- isTRUE(getOption("BRIM_CNRFC_WRU_KML_FETCH", TRUE))
REQUEST_DELAY_SEC <- as.numeric(getOption("BRIM_CNRFC_WRU_KML_REQUEST_DELAY_SEC", 0.10))
MAX_CONTEXT_IDS <- as.integer(getOption("BRIM_CNRFC_WRU_KML_MAX_CONTEXT_IDS", 120L))

pt_log <- function(...) message(format(Sys.time(), "%H:%M:%S"), " | ", ...)

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

pt_latest_file <- function(pattern, dir = DIR$qa) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  files[order(file.info(files)$mtime, decreasing = TRUE)][[1]]
}

pt_first_nonblank <- function(x) {
  x <- pt_chr(x)
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else x[[1]]
}

pt_sanitize_text <- function(txt) {
  if (is.null(txt) || length(txt) == 0) return("")
  txt <- paste(as.character(txt), collapse = "\n")
  out <- suppressWarnings(iconv(txt, from = "", to = "UTF-8", sub = "byte"))
  if (length(out) == 0 || is.na(out)) {
    out <- suppressWarnings(iconv(txt, from = "latin1", to = "UTF-8", sub = "byte"))
  }
  if (length(out) == 0 || is.na(out)) out <- ""
  Encoding(out) <- "UTF-8"
  out
}

pt_decode_raw <- function(content) {
  if (length(content) >= 2 && as.integer(content[[1]]) == 31L && as.integer(content[[2]]) == 139L) {
    content <- tryCatch(memDecompress(content, type = "gzip"), error = function(e) content)
  }
  txt <- tryCatch(
    rawToChar(content),
    error = function(e) paste(rawToChar(content, multiple = TRUE), collapse = "")
  )
  pt_sanitize_text(txt)
}

pt_fetch_text <- function(url, timeout_sec = 25) {
  if (requireNamespace("curl", quietly = TRUE)) {
    h <- curl::new_handle(
      timeout = timeout_sec,
      followlocation = TRUE,
      useragent = "BRIM CNRFC Water Resources Update KML audit (internal BRIM development)",
      httpheader = c("Accept-Encoding" = "identity")
    )
    res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
    if (inherits(res, "error")) {
      return(list(ok = FALSE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = conditionMessage(res)))
    }
    return(list(
      ok = TRUE,
      status_code = res$status_code,
      bytes = length(res$content),
      text = pt_decode_raw(res$content),
      error = NA_character_
    ))
  }

  txt <- tryCatch({
    con <- url(url, open = "rb")
    on.exit(close(con), add = TRUE)
    paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, error = function(e) e)

  if (inherits(txt, "error")) {
    return(list(ok = FALSE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = conditionMessage(txt)))
  }
  txt <- pt_sanitize_text(txt)
  list(ok = TRUE, status_code = NA_integer_, bytes = nchar(txt, type = "bytes"), text = txt, error = NA_character_)
}

pt_norm_url <- function(x, base = BASE_URL) {
  x <- pt_chr(x)
  ifelse(
    grepl("^https?://", x, ignore.case = TRUE),
    x,
    ifelse(startsWith(x, "//"), paste0("https:", x), paste0(base, ifelse(startsWith(x, "/"), "", "/"), x))
  )
}

pt_extract_attrs_from_tag <- function(tag) {
  m <- stringr::str_match_all(tag, "([A-Za-z_:][-A-Za-z0-9_:.]*)\\s*=\\s*(['\"])(.*?)\\2")[[1]]
  if (nrow(m) == 0) return(tibble(attr = character(), value = character()))
  tibble(attr = tolower(m[, 2]), value = m[, 4])
}

pt_attr_value <- function(tag, attr) {
  attrs <- pt_extract_attrs_from_tag(tag)
  out <- attrs$value[attrs$attr == tolower(attr)]
  if (length(out) == 0) NA_character_ else out[[1]]
}

pt_extract_tags <- function(txt, tag) {
  pattern <- paste0("<", tag, "\\b[^>]*>")
  m <- gregexpr(pattern, txt, ignore.case = TRUE, perl = TRUE)[[1]]
  if (length(m) == 0 || m[[1]] < 0) return(character())
  regmatches(txt, gregexpr(pattern, txt, ignore.case = TRUE, perl = TRUE))[[1]]
}

pt_label_for <- function(txt, id) {
  if (is.na(id) || !nzchar(id)) return(NA_character_)
  pat <- paste0("<label[^>]*for\\s*=\\s*['\"]", gsub("([\\W])", "\\\\\\1", id, perl = TRUE), "['\"][^>]*>([\\s\\S]*?)</label>")
  m <- stringr::str_match(txt, regex(pat, ignore_case = TRUE))
  if (is.na(m[1, 2])) return(NA_character_)
  lab <- gsub("<[^>]+>", " ", m[1, 2])
  lab <- gsub("&nbsp;", " ", lab, fixed = TRUE)
  lab <- gsub("\\s+", " ", lab)
  trimws(lab)
}

pt_count_pattern <- function(txt, pattern, ignore.case = TRUE) {
  if (!nzchar(txt)) return(0L)
  m <- gregexpr(pattern, txt, ignore.case = ignore.case, perl = TRUE)[[1]]
  if (length(m) == 0 || m[[1]] < 0) return(0L)
  length(m)
}

pt_extract_id_tokens <- function(txt) {
  if (!nzchar(txt)) return(character())
  # CNRFC station IDs include California C1/C0, Oregon O3, Nevada N2, etc.
  m <- gregexpr("\\b[A-Z]{3,5}[A-Z0-9][0-9]\\b", txt, perl = TRUE)[[1]]
  if (length(m) == 0 || m[[1]] < 0) return(character())
  unique(regmatches(txt, gregexpr("\\b[A-Z]{3,5}[A-Z0-9][0-9]\\b", txt, perl = TRUE))[[1]])
}

pt_extract_href_like <- function(txt) {
  if (!nzchar(txt)) return(character())
  pats <- c(
    "href\\s*=\\s*['\"]([^'\"]+)['\"]",
    "src\\s*=\\s*['\"]([^'\"]+)['\"]",
    "https?://[^'\" <>]+",
    "/[A-Za-z0-9_./?=&%:-]+\\.php[?A-Za-z0-9_./?=&%:-]*"
  )
  vals <- character()
  for (pat in pats) {
    m <- stringr::str_match_all(txt, pat)[[1]]
    if (nrow(m) > 0) vals <- c(vals, if (ncol(m) >= 2) m[, 2] else m[, 1])
  }
  unique(vals[nzchar(vals)])
}

pt_compact <- function(txt, n = 240) {
  txt <- pt_chr(txt)
  txt <- gsub("<[^>]+>", " ", txt)
  txt <- gsub("&nbsp;", " ", txt, fixed = TRUE)
  txt <- gsub("\\s+", " ", txt)
  substr(trimws(txt), 1, n)
}

pt_product_family_from_url <- function(url) {
  case_when(
    grepl("rawESP", url, ignore.case = TRUE) ~ "raw_esp_text",
    grepl("ensembleProduct", url, ignore.case = TRUE) ~ "ensemble_product",
    grepl("water_supply", url, ignore.case = TRUE) ~ "official_water_supply",
    grepl("esp", url, ignore.case = TRUE) ~ "esp_related",
    TRUE ~ "other"
  )
}


pt_attr_lookup <- function(attrs, nm) {
  if (is.null(attrs) || nrow(attrs) == 0) return(NA_character_)
  vals <- attrs$value[tolower(attrs$attr) == tolower(nm)]
  vals <- vals[!is.na(vals) & vals != ""]
  if (length(vals) == 0) NA_character_ else vals[[1]]
}

pt_xml_product_group <- function(kml_file, label = NA_character_, tag_name = NA_character_) {
  txt <- paste(kml_file %||% "", label %||% "", tag_name %||% "", sep = " | ")
  case_when(
    grepl("espfcstWY|water year.*volume|water year volume", txt, ignore.case = TRUE) ~ "forecast_water_year_volume",
    grepl("espfcst|water supply|seasonal volume|wsfcst", txt, ignore.case = TRUE) ~ "forecast_seasonal_volume",
    grepl("flow2dateWYprev|previous water year", txt, ignore.case = TRUE) ~ "previous_water_year_flow_to_date",
    grepl("flow2dateWY|observed water year flow|flow to date", txt, ignore.case = TRUE) ~ "observed_water_year_flow_to_date",
    grepl("PNS|percent.*average|percent.*normal", txt, ignore.case = TRUE) ~ "precip_or_flow_percent_average",
    grepl("PPS|precip.*inch|qpe", txt, ignore.case = TRUE) ~ "precip_inches_or_qpe",
    grepl("reservoir|storage|capacity", txt, ignore.case = TRUE) ~ "reservoir_storage",
    grepl("forecast.*flow", txt, ignore.case = TRUE) ~ "forecast_flow",
    TRUE ~ "wru_xml_product"
  )
}

pt_xml_records_from_body <- function(kml_file, kml_url, label, body) {
  empty <- tibble(
    kml_file = character(),
    kml_url = character(),
    label = character(),
    record_index = integer(),
    record_tag = character(),
    cnrfc_id = character(),
    product_group = character(),
    amount = character(),
    river_name = character(),
    station_name = character(),
    latitude = character(),
    longitude = character(),
    print_issue = character(),
    forecast_period = character(),
    probability_volume = character(),
    average_volume = character(),
    median_volume = character(),
    percent_median = character(),
    percent_average = character(),
    attr_names = character(),
    attr_text = character(),
    record_excerpt = character()
  )
  body <- pt_sanitize_text(body)
  if (!nzchar(body)) return(empty)

  tags <- regmatches(body, gregexpr("<[A-Za-z][A-Za-z0-9_:-]*\\b[^<>]*>", body, perl = TRUE))[[1]]
  if (length(tags) == 0 || (length(tags) == 1 && tags[[1]] == "")) return(empty)

  tags <- tags[!grepl("^</", tags)]
  tags <- tags[grepl("\\b(ident|nwsid|stationid|stationId|id)\\s*=", tags, ignore.case = TRUE, perl = TRUE)]
  if (length(tags) == 0) return(empty)

  out <- purrr::map_dfr(seq_along(tags), function(i) {
    tag <- tags[[i]]
    tag_name <- stringr::str_match(tag, "^<\\s*([A-Za-z][A-Za-z0-9_:-]*)")[, 2]
    attrs <- pt_extract_attrs_from_tag(tag)
    id <- pt_attr_lookup(attrs, "ident")
    if (is.na(id)) id <- pt_attr_lookup(attrs, "nwsid")
    if (is.na(id)) id <- pt_attr_lookup(attrs, "stationid")
    if (is.na(id)) id <- pt_attr_lookup(attrs, "stationId")
    if (is.na(id)) id <- pt_attr_lookup(attrs, "id")
    id <- pt_clean_id(id)
    if (is.na(id)) return(tibble())

    attr_text <- if (nrow(attrs) > 0) {
      paste(paste0(attrs$attr, "=", attrs$value), collapse = " | ")
    } else {
      NA_character_
    }

    tibble(
      kml_file = kml_file,
      kml_url = kml_url,
      label = label,
      record_index = i,
      record_tag = tag_name,
      cnrfc_id = id,
      product_group = pt_xml_product_group(kml_file, label, tag_name),
      amount = pt_attr_lookup(attrs, "amount"),
      river_name = pt_attr_lookup(attrs, "riverName"),
      station_name = pt_attr_lookup(attrs, "stationName"),
      latitude = pt_attr_lookup(attrs, "latitude"),
      longitude = pt_attr_lookup(attrs, "longitude"),
      print_issue = pt_attr_lookup(attrs, "printIssue"),
      forecast_period = pt_attr_lookup(attrs, "printFcstPd"),
      probability_volume = pt_attr_lookup(attrs, "probVol"),
      average_volume = pt_attr_lookup(attrs, "avg"),
      median_volume = pt_attr_lookup(attrs, "median"),
      percent_median = pt_attr_lookup(attrs, "percentMedian"),
      percent_average = pt_attr_lookup(attrs, "percentAvg"),
      attr_names = if (nrow(attrs) > 0) paste(sort(unique(attrs$attr)), collapse = ";") else NA_character_,
      attr_text = attr_text,
      record_excerpt = substr(gsub("\\s+", " ", tag), 1, 350)
    )
  })

  if (nrow(out) == 0) empty else out
}

# ==== 2. Fetch page ==========================================================

pt_log("Fetching CNRFC Daily Water Resources Update: ", WRU_URL)
fetch <- pt_fetch_text(WRU_URL, timeout_sec = 30)
txt <- pt_sanitize_text(fetch$text)
pt_log("Fetch status: ", fetch$status_code %||% NA_integer_, "; bytes: ", fetch$bytes %||% NA_integer_, "; decoded chars: ", nchar(txt, type = "chars", allowNA = TRUE))

# ==== 3. Control inventory ===================================================

pt_log("Inventorying page controls and likely KML/XML layer IDs...")
input_tags <- pt_extract_tags(txt, "input")
controls <- tibble(tag = input_tags) |>
  mutate(
    id = map_chr(.data$tag, ~ pt_attr_value(.x, "id")),
    name = map_chr(.data$tag, ~ pt_attr_value(.x, "name")),
    type = map_chr(.data$tag, ~ pt_attr_value(.x, "type")),
    value = map_chr(.data$tag, ~ pt_attr_value(.x, "value")),
    description = map_chr(.data$tag, ~ pt_attr_value(.x, "description")),
    onclick = map_chr(.data$tag, ~ pt_attr_value(.x, "onclick")),
    label = map_chr(.data$id, ~ pt_label_for(txt, .x)),
    control_text = paste(.data$id, .data$name, .data$type, .data$value, .data$description, .data$label, .data$onclick, sep = " | "),
    likely_kml_file = .data$id,
    is_toggle_kml = grepl("toggleKML1|addMarkersFromXML1", .data$onclick, ignore.case = TRUE),
    is_water_resources_candidate = grepl(
      "water supply|water year|forecast water|forecast flow|accumulated|seasonal volume|raw esp|esp|reservoir storage|percent of capacity|percent of normal|observed flow|water resources|storage",
      .data$control_text,
      ignore.case = TRUE
    ),
    candidate_priority = case_when(
      .data$is_toggle_kml & .data$is_water_resources_candidate ~ 1L,
      .data$is_water_resources_candidate ~ 2L,
      .data$is_toggle_kml ~ 3L,
      TRUE ~ 9L
    )
  ) |>
  filter(!is.na(.data$id), .data$id != "") |>
  arrange(.data$candidate_priority, .data$id)

pt_log("Controls found: ", nrow(controls), "; likely WR/KML controls: ", sum(controls$candidate_priority <= 2, na.rm = TRUE))

kml_candidates <- controls |>
  filter(.data$candidate_priority <= 2, !is.na(.data$likely_kml_file), .data$likely_kml_file != "") |>
  distinct(kml_file = .data$likely_kml_file, .keep_all = TRUE) |>
  mutate(kml_url = paste0(BASE_URL, "/data/kml/", .data$kml_file, ".xml")) |>
  arrange(.data$candidate_priority, .data$kml_file)

if (is.finite(MAX_KML_FILES) && !is.na(MAX_KML_FILES) && MAX_KML_FILES > 0) {
  kml_candidates_to_fetch <- kml_candidates |> head(MAX_KML_FILES)
} else {
  kml_candidates_to_fetch <- kml_candidates
}

# ==== 4. Inline JavaScript arrays ===========================================

pt_log("Extracting embedded JavaScript arrays with CNRFC IDs...")
array_matches <- stringr::str_match_all(
  txt,
  regex("(?:var\\s+)?([A-Za-z_$][A-Za-z0-9_$]*)\\s*=\\s*\\[([\\s\\S]*?)\\]\\s*;", ignore_case = FALSE)
)[[1]]

if (nrow(array_matches) > 0) {
  js_arrays_raw <- tibble(
    array_index = seq_len(nrow(array_matches)),
    array_name = array_matches[, 2],
    array_body = array_matches[, 3]
  ) |>
    mutate(
      id_tokens = map(.data$array_body, pt_extract_id_tokens),
      id_count = map_int(.data$id_tokens, length),
      array_excerpt = substr(gsub("\\s+", " ", .data$array_body), 1, 300),
      product_hint = case_when(
        grepl("trend.*seas|seas.*trend|season", .data$array_name, ignore.case = TRUE) ~ "seasonal_trend_or_water_supply",
        grepl("trend.*wy|wy.*trend|water.*year", .data$array_name, ignore.case = TRUE) ~ "water_year_trend_or_forecast",
        grepl("esp", .data$array_name, ignore.case = TRUE) ~ "esp_related",
        grepl("res|storage", .data$array_name, ignore.case = TRUE) ~ "reservoir_or_storage",
        TRUE ~ "id_array"
      )
    ) |>
    filter(.data$id_count > 0)
} else {
  js_arrays_raw <- tibble(array_index = integer(), array_name = character(), array_body = character(), id_tokens = list(), id_count = integer(), array_excerpt = character(), product_hint = character())
}

js_array_summary <- js_arrays_raw |>
  transmute(array_index, array_name, product_hint, id_count, array_excerpt)

js_array_ids <- js_arrays_raw |>
  select(all_of(c("array_index", "array_name", "product_hint", "id_tokens"))) |>
  tidyr::unnest(id_tokens, keep_empty = FALSE) |>
  rename(cnrfc_id = id_tokens) |>
  distinct()

pt_log("Embedded ID arrays found: ", nrow(js_array_summary), "; array-ID rows: ", nrow(js_array_ids))

# ==== 5. Fetch candidate KML/XML layers =====================================

kml_fetch <- tibble(
  kml_file = character(),
  kml_url = character(),
  label = character(),
  description = character(),
  status_code = integer(),
  ok = logical(),
  http_ok = logical(),
  bytes = integer(),
  error = character(),
  decoded_chars = integer(),
  placemark_count = integer(),
  id_count = integer(),
  href_count = integer(),
  body_excerpt = character()
)
kml_marker_ids <- tibble(
  kml_file = character(),
  marker_index = integer(),
  cnrfc_id = character(),
  marker_name = character(),
  marker_desc_excerpt = character(),
  coordinates = character()
)
kml_links <- tibble(
  kml_file = character(),
  marker_index = integer(),
  cnrfc_id = character(),
  url = character(),
  product_family = character(),
  prod_id = character()
)

xml_product_records <- tibble(
  kml_file = character(),
  kml_url = character(),
  label = character(),
  record_index = integer(),
  record_tag = character(),
  cnrfc_id = character(),
  product_group = character(),
  amount = character(),
  river_name = character(),
  station_name = character(),
  latitude = character(),
  longitude = character(),
  print_issue = character(),
  forecast_period = character(),
  probability_volume = character(),
  average_volume = character(),
  median_volume = character(),
  percent_median = character(),
  percent_average = character(),
  attr_names = character(),
  attr_text = character(),
  record_excerpt = character()
)

if (FETCH_KML && nrow(kml_candidates_to_fetch) > 0) {
  pt_log("Fetching candidate WRU KML/XML files: ", nrow(kml_candidates_to_fetch), " of ", nrow(kml_candidates))

  for (i in seq_len(nrow(kml_candidates_to_fetch))) {
    kml_file <- kml_candidates_to_fetch$kml_file[[i]]
    kml_url <- kml_candidates_to_fetch$kml_url[[i]]
    label <- kml_candidates_to_fetch$label[[i]] %||% kml_candidates_to_fetch$description[[i]] %||% kml_file
    pt_log("  [", i, "/", nrow(kml_candidates_to_fetch), "] ", kml_file, " — ", label)

    res <- pt_fetch_text(kml_url, timeout_sec = 20)
    body <- pt_sanitize_text(res$text)
    if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)

    placemark_count <- pt_count_pattern(body, "<Placemark", ignore.case = TRUE)
    ids_all <- pt_extract_id_tokens(body)
    hrefs_all <- pt_extract_href_like(body)

    kml_fetch <- bind_rows(kml_fetch, tibble(
      kml_file = kml_file,
      kml_url = kml_url,
      label = label,
      description = kml_candidates_to_fetch$description[[i]],
      status_code = res$status_code,
      ok = res$ok,
      http_ok = !is.na(res$status_code) && res$status_code >= 200 && res$status_code < 300,
      bytes = res$bytes,
      error = res$error,
      decoded_chars = nchar(body, type = "chars", allowNA = TRUE),
      placemark_count = placemark_count,
      id_count = length(ids_all),
      href_count = length(hrefs_all),
      body_excerpt = substr(gsub("\\s+", " ", body), 1, 350)
    ))

    if (!is.na(res$status_code) && res$status_code >= 200 && res$status_code < 300) {
      xml_tbl <- pt_xml_records_from_body(kml_file, kml_url, label, body)
      if (nrow(xml_tbl) > 0) {
        xml_product_records <- bind_rows(xml_product_records, xml_tbl)
      }
    }

    # Marker-level extraction.  Regex is intentionally tolerant because CNRFC
    # descriptions are often HTML/CDATA assembled from legacy JavaScript.
    pm <- regmatches(body, gregexpr("<Placemark[\\s\\S]*?</Placemark>", body, ignore.case = TRUE, perl = TRUE))[[1]]
    if (length(pm) > 0 && !(length(pm) == 1 && pm[[1]] == "")) {
      marker_tbl <- map_dfr(seq_along(pm), function(j) {
        block <- pm[[j]]
        nm <- stringr::str_match(block, regex("<name[^>]*>([\\s\\S]*?)</name>", ignore_case = TRUE))[, 2]
        desc <- stringr::str_match(block, regex("<description[^>]*>([\\s\\S]*?)</description>", ignore_case = TRUE))[, 2]
        coords <- stringr::str_match(block, regex("<coordinates[^>]*>([\\s\\S]*?)</coordinates>", ignore_case = TRUE))[, 2]
        ids <- pt_extract_id_tokens(block)
        if (length(ids) == 0) ids <- NA_character_
        tibble(
          kml_file = kml_file,
          marker_index = j,
          cnrfc_id = ids,
          marker_name = pt_compact(nm, 160),
          marker_desc_excerpt = pt_compact(desc, 260),
          coordinates = pt_compact(coords, 120)
        )
      })
      kml_marker_ids <- bind_rows(kml_marker_ids, marker_tbl |> filter(!is.na(.data$cnrfc_id)))

      link_tbl <- map_dfr(seq_along(pm), function(j) {
        block <- pm[[j]]
        ids <- pt_extract_id_tokens(block)
        hrefs <- pt_extract_href_like(block)
        if (length(hrefs) == 0) return(tibble())
        tibble(
          kml_file = kml_file,
          marker_index = j,
          cnrfc_id = if (length(ids) == 0) NA_character_ else paste(ids, collapse = ";"),
          url = pt_norm_url(hrefs),
          product_family = pt_product_family_from_url(pt_norm_url(hrefs)),
          prod_id = stringr::str_match(pt_norm_url(hrefs), "prodID=([0-9]+)")[, 2]
        )
      })
      kml_links <- bind_rows(kml_links, link_tbl)
    }
  }
} else {
  pt_log("Skipping KML/XML fetch; FETCH_KML is FALSE or no candidates were found.")
}

# ==== 6. Match against basin/FNF feature context =============================

feature_master_path <- pt_latest_file("^cnrfc_feature_master_.*\\.csv$")
matrix_path <- pt_latest_file("^cnrfc_basin_product_availability_matrix_.*\\.csv$")

feature_ids <- tibble()
if (!is.na(feature_master_path) && file.exists(feature_master_path)) {
  fm <- readr::read_csv(feature_master_path, show_col_types = FALSE, progress = FALSE)
  needed <- c("cnrfc_id", "feature_type", "display_name")
  for (nm in setdiff(needed, names(fm))) fm[[nm]] <- NA_character_
  feature_ids <- fm |>
    mutate(cnrfc_id = pt_clean_id(.data$cnrfc_id), feature_type = tolower(pt_chr(.data$feature_type))) |>
    filter(!is.na(.data$cnrfc_id), .data$feature_type %in% c("cnrfc_basin", "cnrfc_fnf_sierra_delta_basin")) |>
    group_by(.data$cnrfc_id) |>
    summarise(
      feature_types = paste(sort(unique(.data$feature_type)), collapse = ";"),
      display_name = pt_first_nonblank(.data$display_name),
      .groups = "drop"
    )
}

matrix_ids <- tibble()
if (!is.na(matrix_path) && file.exists(matrix_path)) {
  mx <- readr::read_csv(matrix_path, show_col_types = FALSE, progress = FALSE)
  if ("cnrfc_id" %in% names(mx)) {
    matrix_ids <- mx |>
      mutate(cnrfc_id = pt_clean_id(.data$cnrfc_id)) |>
      filter(!is.na(.data$cnrfc_id)) |>
      transmute(cnrfc_id, from_latest_matrix = TRUE)
  }
}

id_pool <- bind_rows(feature_ids |> mutate(from_feature_master = TRUE), matrix_ids) |>
  group_by(.data$cnrfc_id) |>
  summarise(
    feature_types = paste(sort(unique(na.omit(.data$feature_types))), collapse = ";"),
    display_name = pt_first_nonblank(.data$display_name),
    from_feature_master = any(.data$from_feature_master %||% FALSE, na.rm = TRUE),
    from_latest_matrix = any(.data$from_latest_matrix %||% FALSE, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(.data$cnrfc_id)

if (is.finite(MAX_CONTEXT_IDS) && !is.na(MAX_CONTEXT_IDS) && MAX_CONTEXT_IDS > 0) {
  id_pool_scan <- id_pool |> head(MAX_CONTEXT_IDS)
} else {
  id_pool_scan <- id_pool
}

js_summary_by_id <- js_array_ids |>
  group_by(.data$cnrfc_id) |>
  summarise(
    in_wru_js_arrays = TRUE,
    wru_js_array_names = paste(sort(unique(.data$array_name)), collapse = ";"),
    wru_js_product_hints = paste(sort(unique(.data$product_hint)), collapse = ";"),
    .groups = "drop"
  )

if (nrow(kml_marker_ids) > 0 && "cnrfc_id" %in% names(kml_marker_ids)) {
  kml_summary_by_id <- kml_marker_ids |>
    mutate(cnrfc_id = pt_clean_id(.data$cnrfc_id)) |>
    filter(!is.na(.data$cnrfc_id)) |>
    group_by(.data$cnrfc_id) |>
    summarise(
      in_wru_kml = TRUE,
      wru_kml_files = paste(sort(unique(.data$kml_file)), collapse = ";"),
      wru_kml_marker_count = n(),
      .groups = "drop"
    )
} else {
  kml_summary_by_id <- tibble(
    cnrfc_id = character(),
    in_wru_kml = logical(),
    wru_kml_files = character(),
    wru_kml_marker_count = integer()
  )
}

if (nrow(kml_links) > 0 && "cnrfc_id" %in% names(kml_links)) {
  link_summary_by_id <- kml_links |>
    mutate(
      id_list = strsplit(pt_chr(.data$cnrfc_id), ";", fixed = TRUE)
    ) |>
    tidyr::unnest(id_list, keep_empty = FALSE) |>
    mutate(id_list = pt_clean_id(.data$id_list)) |>
    filter(!is.na(.data$id_list)) |>
    group_by(cnrfc_id = .data$id_list) |>
    summarise(
      wru_product_link_count = n(),
      wru_product_families = paste(sort(unique(.data$product_family)), collapse = ";"),
      wru_prod_ids = paste(sort(unique(na.omit(.data$prod_id))), collapse = ";"),
      .groups = "drop"
    )
} else {
  link_summary_by_id <- tibble(
    cnrfc_id = character(),
    wru_product_link_count = integer(),
    wru_product_families = character(),
    wru_prod_ids = character()
  )
}


if (nrow(xml_product_records) > 0 && "cnrfc_id" %in% names(xml_product_records)) {
  xml_summary_by_id <- xml_product_records |>
    mutate(cnrfc_id = pt_clean_id(.data$cnrfc_id)) |>
    filter(!is.na(.data$cnrfc_id)) |>
    group_by(.data$cnrfc_id) |>
    summarise(
      in_wru_xml_products = TRUE,
      wru_xml_record_count = n(),
      wru_xml_files = paste(sort(unique(.data$kml_file)), collapse = ";"),
      wru_xml_product_groups = paste(sort(unique(.data$product_group)), collapse = ";"),
      wru_xml_record_tags = paste(sort(unique(.data$record_tag)), collapse = ";"),
      wru_xml_labels = paste(sort(unique(.data$label)), collapse = " | "),
      .groups = "drop"
    )
} else {
  xml_summary_by_id <- tibble(
    cnrfc_id = character(),
    in_wru_xml_products = logical(),
    wru_xml_record_count = integer(),
    wru_xml_files = character(),
    wru_xml_product_groups = character(),
    wru_xml_record_tags = character(),
    wru_xml_labels = character()
  )
}

basin_match <- id_pool_scan |>
  left_join(js_summary_by_id, by = "cnrfc_id") |>
  left_join(kml_summary_by_id, by = "cnrfc_id") |>
  left_join(link_summary_by_id, by = "cnrfc_id") |>
  left_join(xml_summary_by_id, by = "cnrfc_id") |>
  mutate(
    in_wru_js_arrays = coalesce(.data$in_wru_js_arrays, FALSE),
    in_wru_kml = coalesce(.data$in_wru_kml, FALSE),
    in_wru_xml_products = coalesce(.data$in_wru_xml_products, FALSE),
    wru_product_link_count = coalesce(.data$wru_product_link_count, 0L),
    wru_xml_record_count = coalesce(.data$wru_xml_record_count, 0L),
    appears_in_wru_audit = .data$in_wru_js_arrays | .data$in_wru_kml | .data$in_wru_xml_products | .data$wru_product_link_count > 0
  )

# ==== 7. Write outputs ========================================================

out_controls <- file.path(DIR$qa, paste0("cnrfc_wru_control_inventory_", RUN_TS, ".csv"))
out_js_summary <- file.path(DIR$qa, paste0("cnrfc_wru_js_array_summary_", RUN_TS, ".csv"))
out_js_ids <- file.path(DIR$qa, paste0("cnrfc_wru_js_array_id_inventory_", RUN_TS, ".csv"))
out_kml_fetch <- file.path(DIR$qa, paste0("cnrfc_wru_kml_fetch_inventory_", RUN_TS, ".csv"))
out_kml_ids <- file.path(DIR$qa, paste0("cnrfc_wru_kml_marker_id_inventory_", RUN_TS, ".csv"))
out_kml_links <- file.path(DIR$qa, paste0("cnrfc_wru_kml_product_links_", RUN_TS, ".csv"))
out_xml_records <- file.path(DIR$qa, paste0("cnrfc_wru_xml_product_records_", RUN_TS, ".csv"))
out_xml_matrix <- file.path(DIR$qa, paste0("cnrfc_wru_xml_product_matrix_", RUN_TS, ".csv"))
out_basin_match <- file.path(DIR$qa, paste0("cnrfc_wru_basin_match_matrix_", RUN_TS, ".csv"))
out_summary <- file.path(DIR$qa, paste0("cnrfc_wru_audit_summary_", RUN_TS, ".csv"))

readr::write_csv(controls, out_controls)
readr::write_csv(js_array_summary, out_js_summary)
readr::write_csv(js_array_ids, out_js_ids)
readr::write_csv(kml_fetch, out_kml_fetch)
readr::write_csv(kml_marker_ids, out_kml_ids)
readr::write_csv(kml_links, out_kml_links)
readr::write_csv(xml_product_records, out_xml_records)
readr::write_csv(xml_summary_by_id, out_xml_matrix)
readr::write_csv(basin_match, out_basin_match)

# Precompute summary values outside tibble().  This avoids accidental masking
# where a newly-created column named `controls` can hide the controls table.
controls_n <- nrow(controls)
likely_wru_controls_n <- if ("candidate_priority" %in% names(controls)) {
  sum(controls[["candidate_priority"]] <= 2, na.rm = TRUE)
} else {
  0L
}
kml_files_ok_n <- if ("ok" %in% names(kml_fetch)) sum(kml_fetch[["ok"]] %||% FALSE, na.rm = TRUE) else 0L
kml_files_http_ok_n <- if ("http_ok" %in% names(kml_fetch)) sum(kml_fetch[["http_ok"]] %||% FALSE, na.rm = TRUE) else 0L
kml_files_with_placemarks_n <- if ("placemark_count" %in% names(kml_fetch)) sum(kml_fetch[["placemark_count"]] > 0, na.rm = TRUE) else 0L
xml_product_record_rows_n <- nrow(xml_product_records)
xml_product_ids_n <- if (nrow(xml_summary_by_id) > 0) nrow(xml_summary_by_id) else 0L
basins_seen_in_js_n <- if ("in_wru_js_arrays" %in% names(basin_match)) sum(basin_match[["in_wru_js_arrays"]], na.rm = TRUE) else 0L
basins_seen_in_kml_n <- if ("in_wru_kml" %in% names(basin_match)) sum(basin_match[["in_wru_kml"]], na.rm = TRUE) else 0L
basins_seen_in_xml_n <- if ("in_wru_xml_products" %in% names(basin_match)) sum(basin_match[["in_wru_xml_products"]], na.rm = TRUE) else 0L
basins_with_product_links_n <- if ("wru_product_link_count" %in% names(basin_match)) sum(basin_match[["wru_product_link_count"]] > 0, na.rm = TRUE) else 0L

summary_tbl <- tibble(
  run_timestamp = RUN_TS,
  page_status_code = fetch$status_code,
  page_bytes = fetch$bytes,
  controls = controls_n,
  likely_wru_controls = likely_wru_controls_n,
  kml_candidates = nrow(kml_candidates),
  kml_files_attempted = nrow(kml_candidates_to_fetch),
  kml_files_ok = kml_files_ok_n,
  kml_files_http_ok = kml_files_http_ok_n,
  kml_files_with_placemarks = kml_files_with_placemarks_n,
  xml_product_record_rows = xml_product_record_rows_n,
  xml_product_ids = xml_product_ids_n,
  kml_marker_id_rows = nrow(kml_marker_ids),
  kml_product_link_rows = nrow(kml_links),
  js_arrays_with_ids = nrow(js_array_summary),
  js_array_id_rows = nrow(js_array_ids),
  basin_context_rows = nrow(basin_match),
  basins_seen_in_js = basins_seen_in_js_n,
  basins_seen_in_kml = basins_seen_in_kml_n,
  basins_seen_in_xml = basins_seen_in_xml_n,
  basins_with_product_links = basins_with_product_links_n
)
readr::write_csv(summary_tbl, out_summary)

# ==== 8. Console summary ======================================================

message("CNRFC Water Resources Update KML/control audit complete.")
message("  Control inventory: ", out_controls)
message("  JS array summary:  ", out_js_summary)
message("  JS array IDs:      ", out_js_ids)
message("  KML fetch:         ", out_kml_fetch)
message("  KML marker IDs:    ", out_kml_ids)
message("  KML product links: ", out_kml_links)
message("  XML product rows:  ", out_xml_records)
message("  XML product matrix:", out_xml_matrix)
message("  Basin match:       ", out_basin_match)
message("  Summary:           ", out_summary)
message("")

message("Summary:")
print(summary_tbl, width = Inf)

message("")
message("Likely WRU controls / KML candidates:")
print(
  kml_candidates |>
    select(any_of(c("kml_file", "label", "description", "candidate_priority", "kml_url"))) |>
    head(25),
  n = 25,
  width = Inf
)

message("")
message("Top embedded JS arrays with IDs:")
print(
  js_array_summary |>
    arrange(desc(.data$id_count)) |>
    head(20),
  n = 20,
  width = Inf
)

if (nrow(kml_fetch) > 0) {
  message("")
  message("KML/XML fetch results:")
  print(
    kml_fetch |>
      select(any_of(c("kml_file", "status_code", "http_ok", "bytes", "placemark_count", "id_count", "href_count", "label"))) |>
      head(30),
    n = 30,
    width = Inf
  )
}

if (nrow(xml_product_records) > 0) {
  message("")
  message("WRU XML product records by file/product group:")
  print(
    xml_product_records |>
      count(.data$kml_file, .data$product_group, name = "records") |>
      arrange(desc(.data$records), .data$kml_file) |>
      head(30),
    n = 30,
    width = Inf
  )
}

message("")
message("First basin/FNF WRU match rows:")
print(
  basin_match |>
    select(any_of(c(
      "cnrfc_id", "display_name", "feature_types",
      "in_wru_js_arrays", "wru_js_product_hints",
      "in_wru_kml", "wru_kml_files",
      "in_wru_xml_products", "wru_xml_product_groups", "wru_xml_record_count", "wru_xml_files",
      "wru_product_link_count", "wru_product_families", "wru_prod_ids",
      "appears_in_wru_audit"
    ))) |>
    head(30),
  n = 30,
  width = Inf
)

message("")
message("Interpretation note:")
message("  The WRU layer files are mostly custom XML, not true KML Placemarks; use XML product rows/matrix as the main data source.")
message("  KML marker/product-link rows may remain empty even when XML product rows are rich and useful.")
