# ==== qa_cnrfc_active_forecast_xml_parse_audit.r =============================
##
## PURPOSE:
##   Parse CNRFC active forecast-point XML sources more specifically than the
##   broad page scan in Patch 030.  Patch 031 found that reservoir inflow and
##   release marker sets are available as product-specific XML endpoints with
##   counts matching the visible UI.  Patch 033 adds the ensemble forecast
##   points XML endpoint supplied from the CNRFC map download menu.  The river
##   forecast endpoint appears to be a combined XML product for "Fcst and Other
##   Pts" and needs field/record inspection before it can be used as
##   official-vs-unofficial authority.
##
## OUTPUTS:
##   04_processed_data/qa/cnrfc_active_forecast_xml_endpoint_fetch_*.csv
##   04_processed_data/qa/cnrfc_active_forecast_xml_record_long_*.csv
##   04_processed_data/qa/cnrfc_active_forecast_xml_id_matrix_*.csv
##   04_processed_data/qa/cnrfc_active_forecast_xml_bins_*.csv
##   04_processed_data/qa/cnrfc_active_forecast_xml_field_inventory_*.csv
##   04_processed_data/qa/cnrfc_active_forecast_xml_summary_*.csv
##
## DESIGN NOTES:
##   - This is still QA / source-discovery work.  It does not modify the map.
##   - It is intentionally generic because CNRFC's XML files are not guaranteed
##     to use one stable schema across products.
##   - Once this reliably identifies the active marker records, the stable logic
##     can be wired into 53_build_cnrfc_forecast_point_product_availability.R.

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
  library(tidyr)
})

RUN_TS <- make_timestamp()
dir.create(DIR$qa, showWarnings = FALSE, recursive = TRUE)

opt <- function(name, default = NULL) getOption(name, default)

REQUEST_DELAY_SEC <- suppressWarnings(as.numeric(opt("BRIM_CNRFC_ACTIVE_XML_REQUEST_DELAY_SEC", 0.10)))
if (is.na(REQUEST_DELAY_SEC) || REQUEST_DELAY_SEC < 0) REQUEST_DELAY_SEC <- 0.10

ENDPOINT_TIMEOUT_SEC <- suppressWarnings(as.numeric(opt("BRIM_CNRFC_ACTIVE_XML_ENDPOINT_TIMEOUT_SEC", 60)))
if (is.na(ENDPOINT_TIMEOUT_SEC) || ENDPOINT_TIMEOUT_SEC <= 0) ENDPOINT_TIMEOUT_SEC <- 60

MAX_RECORD_TEXT_CHARS <- suppressWarnings(as.integer(opt("BRIM_CNRFC_ACTIVE_XML_MAX_RECORD_TEXT_CHARS", 1800L)))
if (is.na(MAX_RECORD_TEXT_CHARS) || MAX_RECORD_TEXT_CHARS < 200L) MAX_RECORD_TEXT_CHARS <- 1800L

pt_log <- function(...) message(format(Sys.time(), "%H:%M:%S"), " | ", ...)

pt_clean_text <- function(x) {
  x <- iconv(x, from = "", to = "UTF-8", sub = "byte")
  x[is.na(x)] <- ""
  x
}

pt_fetch_url <- function(url, timeout = ENDPOINT_TIMEOUT_SEC) {
  out <- list(
    url = url,
    final_url = url,
    status_code = NA_integer_,
    http_ok = FALSE,
    bytes = NA_integer_,
    decoded_chars = NA_integer_,
    content_type = NA_character_,
    text = "",
    error = NA_character_
  )

  tryCatch({
    if (requireNamespace("curl", quietly = TRUE)) {
      h <- curl::new_handle(
        useragent = "BRIM CNRFC active forecast XML parse audit",
        followlocation = TRUE,
        timeout = timeout
      )
      res <- curl::curl_fetch_memory(url, handle = h)
      out$status_code <- as.integer(res$status_code)
      out$http_ok <- out$status_code >= 200L && out$status_code < 300L
      out$bytes <- length(res$content)
      if (!is.null(res$url)) out$final_url <- res$url
      out$content_type <- if (!is.null(res$headers)) {
        hdr <- rawToChar(res$headers)
        ct <- stringr::str_match(hdr, stringr::regex("content-type:\\s*([^\\r\\n]+)", ignore_case = TRUE))[, 2]
        ifelse(is.na(ct), NA_character_, trimws(ct))
      } else {
        NA_character_
      }
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
    txt <- pt_clean_text(txt)
    out$text <- txt
    out$decoded_chars <- nchar(txt)
    out
  }, error = function(e) {
    out$error <- conditionMessage(e)
    out
  })
}

pt_looks_like_html <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(FALSE)
  stringr::str_detect(txt, stringr::regex("<html|<!doctype|404 Not Found|CNRFC - California Nevada", ignore_case = TRUE))
}

pt_id_tokens <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(character())
  ids <- unlist(regmatches(txt, gregexpr("(?<![A-Z0-9])[A-Z0-9]{4,6}(?![A-Z0-9])", txt, perl = TRUE)))
  ids <- ids[grepl("^[A-Z][A-Z0-9]{3,5}$", ids)]
  ids <- ids[grepl("[0-9]$", ids)]
  sort(unique(ids))
}

pt_first_nonempty <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) return(NA_character_)
  trimws(x[[1]])
}

pt_pick_id <- function(field_tbl, blob) {
  if (nrow(field_tbl) > 0) {
    pref <- field_tbl %>%
      filter(stringr::str_detect(field_name, stringr::regex("^(id|ident|identifier|nwsli|station|station_id|lid|location|location_id)$", ignore_case = TRUE))) %>%
      mutate(tokens = lapply(field_value, pt_id_tokens))
    for (i in seq_len(nrow(pref))) {
      if (length(pref$tokens[[i]]) > 0) return(pref$tokens[[i]][[1]])
    }
  }
  ids <- pt_id_tokens(blob)
  if (length(ids) == 0) return(NA_character_)
  ids[[1]]
}

pt_pick_field <- function(field_tbl, patterns) {
  if (nrow(field_tbl) == 0) return(NA_character_)
  pat <- paste(patterns, collapse = "|")
  vals <- field_tbl %>%
    filter(stringr::str_detect(field_name, stringr::regex(pat, ignore_case = TRUE))) %>%
    pull(field_value)
  pt_first_nonempty(vals)
}

pt_parse_num <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_real_)
  suppressWarnings(as.numeric(gsub("[^0-9.\\-]+", "", x)))
}

pt_compact <- function(x, n = 260L) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  substr(x, 1, n)
}

pt_node_path <- function(node) {
  if (!requireNamespace("xml2", quietly = TRUE)) return(NA_character_)
  anc <- xml2::xml_find_all(node, "ancestor-or-self::*")
  paste(xml2::xml_name(anc), collapse = "/")
}

pt_node_field_tbl <- function(node) {
  attrs <- xml2::xml_attrs(node)
  attr_tbl <- tibble(field_source = character(), field_name = character(), field_value = character())
  if (length(attrs) > 0) {
    attr_tbl <- tibble(
      field_source = "attribute",
      field_name = names(attrs),
      field_value = as.character(attrs)
    )
  }

  kids <- xml2::xml_children(node)
  kid_tbl <- tibble(field_source = character(), field_name = character(), field_value = character())
  if (length(kids) > 0) {
    kid_tbl <- tibble(
      field_source = "child",
      field_name = xml2::xml_name(kids),
      field_value = vapply(kids, function(k) pt_compact(xml2::xml_text(k), 600L), character(1))
    ) %>%
      filter(!is.na(field_value), nzchar(trimws(field_value)))
  }

  bind_rows(attr_tbl, kid_tbl)
}

pt_infer_river_officiality <- function(endpoint_key, field_tbl, blob) {
  if (!identical(endpoint_key, "river_forecast_combined")) return(NA_character_)
  text <- paste(blob, collapse = " ")
  if (stringr::str_detect(text, stringr::regex("unofficial|other point|otherpoint|other_points|other pts", ignore_case = TRUE))) {
    return("unofficial_or_other_point_hint")
  }
  if (stringr::str_detect(text, stringr::regex("official|river forecast|forecast point|fcst", ignore_case = TRUE))) {
    return("official_or_forecast_point_hint")
  }
  "riverFcst XML combined; official/unofficial split not parsed"
}

pt_infer_record_status <- function(endpoint_key, node_name, path, field_tbl, blob) {
  ids <- pt_id_tokens(blob)
  has_lat <- !is.na(pt_pick_field(field_tbl, c("^lat$", "latitude", "lat_deg", "ycoord", "y_coord")))
  has_lon <- !is.na(pt_pick_field(field_tbl, c("^lon$", "^lng$", "longitude", "lon_deg", "xcoord", "x_coord")))
  markerish <- stringr::str_detect(paste(node_name, path, blob), stringr::regex("marker|placemark|point|station|site|gage|reservoir|river|location", ignore_case = TRUE))
  if (length(ids) == 0) return("no_id")
  if (length(ids) > 4) return("many_ids_probably_container")
  if (has_lat && has_lon) return("candidate_marker_record_with_lat_lon")
  if (markerish && nchar(blob) <= MAX_RECORD_TEXT_CHARS) return("candidate_marker_record_markerish")
  "id_found_but_not_markerish"
}

# ==== 2. XML endpoints =======================================================

xml_endpoints <- tibble::tribble(
  ~endpoint_key,                ~active_product_label,                         ~url,                                                          ~expected_visible_markers, ~expected_count_note,
  "river_forecast_combined",    "River Forecast Data for Fcst and Other Pts",   "https://www.cnrfc.noaa.gov/data/kml/riverFcst.xml",       287L, "User UI: 102 official river + 185 unofficial river = 287 combined",
  "other_points_candidate",     "Possible Other/Unofficial River Points XML",   "https://www.cnrfc.noaa.gov/data/kml/otherPointsFcst.xml", 185L, "Candidate guessed endpoint; may be absent or redirect/HTML",
  "reservoir_inflows",          "Reservoir Inflow Points",                    "https://www.cnrfc.noaa.gov/data/kml/rsvrInflow.xml",             104L, "User UI: 104 reservoir inflow markers",
  "reservoir_releases",         "Reservoir Release Points",                   "https://www.cnrfc.noaa.gov/data/kml/rsvrRelease.xml",             61L, "User UI: 61 reservoir release markers",
  "ensemble_forecast_points",   "Ensemble Forecast Points",                   "https://www.cnrfc.noaa.gov/data/kml/ensPoints.xml",              NA_integer_, "User supplied CNRFC XML endpoint; visible marker count not yet recorded"
)

manual_water_supply_index <- tibble::tribble(
  ~cnrfc_id, ~active_forecast_point_subtype, ~active_product_label, ~display_name, ~symbolic_location_label, ~latitude, ~longitude, ~source_url,
  "SACC0", "water_supply_index", "Water Supply Index", "Sacramento Valley Water Supply Index", "sac river input", 38.455664, -121.501620, "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=SACC0&prodID=9",
  "VNSC0", "water_supply_index", "Water Supply Index", "San Joaquin Valley Water Supply Index", "sj river input", 37.67601249, -121.2662907, "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=VNSC0&prodID=9",
  "MLIC0", "water_supply_index", "Water Supply Index", "Central Valley Water Supply Index", "delta outflow", 38.055198, -121.911904, "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=MLIC0&prodID=9"
)

# ==== 3. Fetch and parse XML =================================================

endpoint_fetch <- tibble()
xml_record_long <- tibble()
field_long <- tibble()

for (i in seq_len(nrow(xml_endpoints))) {
  ep <- xml_endpoints[i, ]
  pt_log("Fetching/parsing XML endpoint [", i, "/", nrow(xml_endpoints), "]: ", ep$active_product_label)
  res <- pt_fetch_url(ep$url)
  txt <- res$text
  looks_html <- pt_looks_like_html(txt)

  endpoint_fetch <- bind_rows(endpoint_fetch, tibble(
    run_timestamp = RUN_TS,
    endpoint_key = ep$endpoint_key,
    active_product_label = ep$active_product_label,
    url = ep$url,
    final_url = res$final_url,
    status_code = res$status_code,
    http_ok = res$http_ok,
    bytes = res$bytes,
    decoded_chars = res$decoded_chars,
    content_type = res$content_type,
    looks_like_html = looks_html,
    expected_visible_markers = ep$expected_visible_markers,
    expected_count_note = ep$expected_count_note,
    raw_id_token_count = length(pt_id_tokens(txt)),
    sample_ids = paste(head(pt_id_tokens(txt), 30), collapse = ";"),
    error = res$error
  ))

  if (!res$http_ok || looks_html || !nzchar(txt)) {
    if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
    next
  }

  if (!requireNamespace("xml2", quietly = TRUE)) {
    endpoint_fetch <- endpoint_fetch %>% mutate(error = ifelse(endpoint_key == ep$endpoint_key, "Package xml2 is not installed; cannot parse XML records.", error))
    if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
    next
  }

  doc <- tryCatch(xml2::read_xml(txt), error = function(e) e)
  if (inherits(doc, "error")) {
    endpoint_fetch <- endpoint_fetch %>% mutate(error = ifelse(endpoint_key == ep$endpoint_key, paste0("XML parse error: ", conditionMessage(doc)), error))
    if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
    next
  }

  nodes <- xml2::xml_find_all(doc, "//*")

  for (j in seq_along(nodes)) {
    node <- nodes[[j]]
    node_name <- xml2::xml_name(node)
    path <- pt_node_path(node)
    fields <- pt_node_field_tbl(node)
    node_text <- pt_compact(xml2::xml_text(node), MAX_RECORD_TEXT_CHARS)
    attr_blob <- if (nrow(fields) > 0) paste(fields$field_name, fields$field_value, sep = "=", collapse = " | ") else ""
    blob <- pt_compact(paste(attr_blob, node_text), MAX_RECORD_TEXT_CHARS)
    record_status <- pt_infer_record_status(ep$endpoint_key, node_name, path, fields, blob)

    if (record_status %in% c("candidate_marker_record_with_lat_lon", "candidate_marker_record_markerish", "id_found_but_not_markerish", "many_ids_probably_container")) {
      cnrfc_id <- pt_pick_id(fields, blob)
      lat <- pt_parse_num(pt_pick_field(fields, c("^lat$", "latitude", "lat_deg", "ycoord", "y_coord")))
      lon <- pt_parse_num(pt_pick_field(fields, c("^lon$", "^lng$", "longitude", "lon_deg", "xcoord", "x_coord")))
      display_name <- pt_pick_field(fields, c("name", "title", "description", "river", "location", "station"))
      record_type <- pt_pick_field(fields, c("type", "class", "category", "group", "product", "icon", "status"))
      all_ids <- pt_id_tokens(blob)

      xml_record_long <- bind_rows(xml_record_long, tibble(
        run_timestamp = RUN_TS,
        endpoint_key = ep$endpoint_key,
        active_product_label = ep$active_product_label,
        endpoint_url = ep$url,
        node_index = j,
        node_name = node_name,
        node_path = path,
        record_status = record_status,
        cnrfc_id = cnrfc_id,
        all_id_tokens = paste(all_ids, collapse = ";"),
        id_token_count = length(all_ids),
        latitude = lat,
        longitude = lon,
        display_name_hint = display_name,
        record_type_hint = record_type,
        river_officiality_guess = pt_infer_river_officiality(ep$endpoint_key, fields, blob),
        record_excerpt = blob
      ))

      if (nrow(fields) > 0 && record_status %in% c("candidate_marker_record_with_lat_lon", "candidate_marker_record_markerish")) {
        field_long <- bind_rows(field_long, fields %>% mutate(
          run_timestamp = RUN_TS,
          endpoint_key = ep$endpoint_key,
          active_product_label = ep$active_product_label,
          node_index = j,
          node_name = node_name,
          cnrfc_id = cnrfc_id,
          .before = 1
        ))
      }
    }
  }

  if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
}

# ==== 4. Build ID matrix and bins ============================================

if (nrow(xml_record_long) == 0) {
  xml_record_long <- tibble(
    run_timestamp = character(), endpoint_key = character(), active_product_label = character(),
    endpoint_url = character(), node_index = integer(), node_name = character(), node_path = character(),
    record_status = character(), cnrfc_id = character(), all_id_tokens = character(), id_token_count = integer(),
    latitude = numeric(), longitude = numeric(), display_name_hint = character(), record_type_hint = character(),
    river_officiality_guess = character(), record_excerpt = character()
  )
}

if (nrow(field_long) == 0) {
  field_long <- tibble(
    run_timestamp = character(), endpoint_key = character(), active_product_label = character(),
    node_index = integer(), node_name = character(), cnrfc_id = character(),
    field_source = character(), field_name = character(), field_value = character()
  )
}

candidate_records <- xml_record_long %>%
  filter(record_status %in% c("candidate_marker_record_with_lat_lon", "candidate_marker_record_markerish")) %>%
  filter(!is.na(cnrfc_id), nzchar(cnrfc_id))

# Prefer the most marker-like record for each endpoint/id if XML repeats the ID
# in nested tags.
id_matrix <- candidate_records %>%
  mutate(
    marker_quality_rank = case_when(
      record_status == "candidate_marker_record_with_lat_lon" ~ 1L,
      record_status == "candidate_marker_record_markerish" ~ 2L,
      TRUE ~ 9L
    )
  ) %>%
  arrange(endpoint_key, cnrfc_id, marker_quality_rank, node_index) %>%
  group_by(endpoint_key, cnrfc_id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    active_forecast_point_subtype = case_when(
      endpoint_key == "reservoir_inflows" ~ "reservoir_inflow",
      endpoint_key == "reservoir_releases" ~ "reservoir_release",
      endpoint_key == "ensemble_forecast_points" ~ "ensemble_forecast_point",
      endpoint_key == "river_forecast_combined" ~ "river_forecast_combined_fcst_and_other_pts",
      endpoint_key == "other_points_candidate" ~ "unofficial_or_other_river_candidate",
      TRUE ~ endpoint_key
    ),
    homepage_marker_authority_class = case_when(
      endpoint_key %in% c("reservoir_inflows", "reservoir_releases") ~ "active CNRFC product XML exact-count match candidate",
      endpoint_key == "ensemble_forecast_points" ~ "active CNRFC ensemble forecast XML candidate; expected UI count not yet recorded",
      endpoint_key == "river_forecast_combined" ~ "active CNRFC riverFcst XML combined official+unofficial candidate",
      endpoint_key == "other_points_candidate" ~ "candidate direct XML endpoint; verify count before use",
      TRUE ~ "review"
    )
  ) %>%
  select(
    run_timestamp, endpoint_key, active_product_label, active_forecast_point_subtype,
    homepage_marker_authority_class, cnrfc_id, display_name_hint, latitude, longitude,
    record_type_hint, river_officiality_guess, record_status, node_name, node_path,
    endpoint_url, record_excerpt
  )

manual_index_matrix <- manual_water_supply_index %>%
  mutate(
    run_timestamp = RUN_TS,
    endpoint_key = "manual_water_supply_index",
    homepage_marker_authority_class = "manual water-supply index point",
    river_officiality_guess = NA_character_,
    record_status = "manual_symbolic_point",
    node_name = NA_character_,
    node_path = NA_character_,
    endpoint_url = source_url,
    record_excerpt = paste(display_name, symbolic_location_label, sep = " | "),
    record_type_hint = active_forecast_point_subtype,
    display_name_hint = display_name
  ) %>%
  select(
    run_timestamp, endpoint_key, active_product_label, active_forecast_point_subtype,
    homepage_marker_authority_class, cnrfc_id, display_name_hint, latitude, longitude,
    record_type_hint, river_officiality_guess, record_status, node_name, node_path,
    endpoint_url, record_excerpt
  )

id_matrix_plus_manual <- bind_rows(id_matrix, manual_index_matrix)

bins <- id_matrix_plus_manual %>%
  count(active_forecast_point_subtype, homepage_marker_authority_class, river_officiality_guess, name = "points") %>%
  arrange(desc(points), active_forecast_point_subtype) %>%
  mutate(run_timestamp = RUN_TS, .before = 1)

endpoint_count_compare <- id_matrix %>%
  count(endpoint_key, active_product_label, name = "parsed_unique_ids") %>%
  right_join(xml_endpoints, by = c("endpoint_key", "active_product_label")) %>%
  mutate(
    parsed_unique_ids = coalesce(parsed_unique_ids, 0L),
    parsed_minus_expected = if_else(is.na(expected_visible_markers), NA_integer_, parsed_unique_ids - expected_visible_markers),
    count_match_class = case_when(
      is.na(expected_visible_markers) & parsed_unique_ids > 0 ~ "parsed IDs; no expected UI count supplied yet",
      is.na(expected_visible_markers) & parsed_unique_ids == 0 ~ "no parsed marker IDs; no expected UI count supplied",
      parsed_unique_ids == expected_visible_markers ~ "exact expected visible-marker count",
      abs(parsed_minus_expected) <= 5 ~ "near expected visible-marker count",
      parsed_unique_ids == 0 ~ "no parsed marker IDs",
      TRUE ~ "count differs; inspect XML fields/record logic"
    ),
    run_timestamp = RUN_TS,
    .before = 1
  ) %>%
  select(run_timestamp, endpoint_key, active_product_label, parsed_unique_ids, expected_visible_markers, parsed_minus_expected, count_match_class, url, expected_count_note)

field_inventory <- field_long %>%
  group_by(endpoint_key, active_product_label, field_source, field_name) %>%
  summarise(
    candidate_records_with_field = n_distinct(paste(node_index, cnrfc_id, sep = "|")),
    unique_values = n_distinct(field_value, na.rm = TRUE),
    sample_values = paste(head(unique(field_value[!is.na(field_value) & nzchar(field_value)]), 12), collapse = " || "),
    .groups = "drop"
  ) %>%
  arrange(endpoint_key, field_source, field_name) %>%
  mutate(run_timestamp = RUN_TS, .before = 1)

summary_rows <- tibble(
  run_timestamp = RUN_TS,
  metric = c(
    "xml_endpoints_attempted",
    "xml_endpoints_http_ok_non_html",
    "xml_endpoints_exact_expected_count",
    "xml_endpoints_near_expected_count",
    "parsed_active_xml_unique_endpoint_id_rows",
    "parsed_active_xml_unique_ids_any_endpoint",
    "manual_water_supply_index_points",
    "ensemble_forecast_point_ids",
    "active_xml_plus_manual_rows"
  ),
  value = c(
    nrow(xml_endpoints),
    sum(endpoint_fetch$http_ok & !endpoint_fetch$looks_like_html, na.rm = TRUE),
    sum(endpoint_count_compare$count_match_class == "exact expected visible-marker count", na.rm = TRUE),
    sum(endpoint_count_compare$count_match_class %in% c("exact expected visible-marker count", "near expected visible-marker count"), na.rm = TRUE),
    nrow(id_matrix),
    n_distinct(id_matrix$cnrfc_id),
    nrow(manual_water_supply_index),
    n_distinct(id_matrix$cnrfc_id[id_matrix$endpoint_key == "ensemble_forecast_points"]),
    nrow(id_matrix_plus_manual)
  )
)

# ==== 5. Write outputs =======================================================

out_fetch <- file.path(DIR$qa, paste0("cnrfc_active_forecast_xml_endpoint_fetch_", RUN_TS, ".csv"))
out_records <- file.path(DIR$qa, paste0("cnrfc_active_forecast_xml_record_long_", RUN_TS, ".csv"))
out_matrix <- file.path(DIR$qa, paste0("cnrfc_active_forecast_xml_id_matrix_", RUN_TS, ".csv"))
out_bins <- file.path(DIR$qa, paste0("cnrfc_active_forecast_xml_bins_", RUN_TS, ".csv"))
out_fields <- file.path(DIR$qa, paste0("cnrfc_active_forecast_xml_field_inventory_", RUN_TS, ".csv"))
out_count_compare <- file.path(DIR$qa, paste0("cnrfc_active_forecast_xml_count_compare_", RUN_TS, ".csv"))
out_summary <- file.path(DIR$qa, paste0("cnrfc_active_forecast_xml_summary_", RUN_TS, ".csv"))

readr::write_csv(endpoint_fetch, out_fetch)
readr::write_csv(xml_record_long, out_records)
readr::write_csv(id_matrix_plus_manual, out_matrix)
readr::write_csv(bins, out_bins)
readr::write_csv(field_inventory, out_fields)
readr::write_csv(endpoint_count_compare, out_count_compare)
readr::write_csv(summary_rows, out_summary)

# Latest convenience copies.
readr::write_csv(endpoint_fetch, file.path(DIR$qa, "cnrfc_active_forecast_xml_endpoint_fetch_latest.csv"))
readr::write_csv(id_matrix_plus_manual, file.path(DIR$qa, "cnrfc_active_forecast_xml_id_matrix_latest.csv"))
readr::write_csv(bins, file.path(DIR$qa, "cnrfc_active_forecast_xml_bins_latest.csv"))
readr::write_csv(field_inventory, file.path(DIR$qa, "cnrfc_active_forecast_xml_field_inventory_latest.csv"))
readr::write_csv(endpoint_count_compare, file.path(DIR$qa, "cnrfc_active_forecast_xml_count_compare_latest.csv"))
readr::write_csv(summary_rows, file.path(DIR$qa, "cnrfc_active_forecast_xml_summary_latest.csv"))

pt_log("CNRFC active forecast XML parse audit complete.")
message("  Endpoint fetch:       ", out_fetch)
message("  Record long:          ", out_records)
message("  Active ID matrix:     ", out_matrix)
message("  Bins:                 ", out_bins)
message("  Field inventory:      ", out_fields)
message("  Count compare:        ", out_count_compare)
message("  Summary:              ", out_summary)

message("\nXML endpoint count comparison:")
print(endpoint_count_compare, n = Inf, width = Inf)

message("\nActive XML marker bins:")
print(bins, n = Inf, width = Inf)

message("\nMost useful XML fields by endpoint:")
field_top <- field_inventory %>%
  group_by(endpoint_key, active_product_label) %>%
  slice_max(order_by = candidate_records_with_field, n = 12, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(endpoint_key, desc(candidate_records_with_field))
print(field_top, n = 60, width = Inf)

message("\nXML parse summary:")
print(summary_rows, n = Inf)

message("\nInterpretation note:")
message("  Reservoir inflow/release XML endpoints should now parse as exact active marker sets if CNRFC schema is stable.")
message("  ensPoints.xml is now included as a candidate authority for deterministic+ensemble forecast-point status.")
message("  The riverFcst XML likely represents the combined 'Fcst and Other Pts' download.  Use the field inventory")
message("  to determine whether official vs unofficial can be split directly, or whether separate UI-selected XML")
message("  downloads are needed from the CNRFC page.")
