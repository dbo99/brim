# ==== qa_cnrfc_active_forecast_source_diagnostic.r ===========================
##
## PURPOSE:
##   Diagnose where the CNRFC homepage/product pages store their active marker
##   data.  Patch 030 showed that a full-page known-ID scan is too broad because
##   every product page appears to carry a shared catalog/script payload.  This
##   diagnostic does NOT classify active forecast points.  It looks for the
##   actual marker source: embedded JavaScript arrays, external script files, or
##   product-specific XML/JSON/KML endpoints.
##
## OUTPUTS:
##   04_processed_data/qa/cnrfc_active_source_page_fetch_inventory_*.csv
##   04_processed_data/qa/cnrfc_active_source_script_src_inventory_*.csv
##   04_processed_data/qa/cnrfc_active_source_script_fetch_inventory_*.csv
##   04_processed_data/qa/cnrfc_active_source_keyword_context_*.csv
##   04_processed_data/qa/cnrfc_active_source_endpoint_inventory_*.csv
##   04_processed_data/qa/cnrfc_active_source_candidate_endpoint_fetch_*.csv
##   04_processed_data/qa/cnrfc_active_source_array_candidate_inventory_*.csv
##   04_processed_data/qa/cnrfc_active_source_summary_*.csv

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

opt <- function(name, default = NULL) getOption(name, default)

FETCH_EXTERNAL_SCRIPTS <- isTRUE(opt("BRIM_CNRFC_ACTIVE_SRC_FETCH_EXTERNAL_SCRIPTS", TRUE))
MAX_EXTERNAL_SCRIPTS <- suppressWarnings(as.integer(opt("BRIM_CNRFC_ACTIVE_SRC_MAX_EXTERNAL_SCRIPTS", 30L)))
if (is.na(MAX_EXTERNAL_SCRIPTS) || MAX_EXTERNAL_SCRIPTS < 0L) MAX_EXTERNAL_SCRIPTS <- 30L
FETCH_CANDIDATE_ENDPOINTS <- isTRUE(opt("BRIM_CNRFC_ACTIVE_SRC_FETCH_CANDIDATE_ENDPOINTS", TRUE))
REQUEST_DELAY_SEC <- suppressWarnings(as.numeric(opt("BRIM_CNRFC_ACTIVE_SRC_REQUEST_DELAY_SEC", 0.10)))
if (is.na(REQUEST_DELAY_SEC) || REQUEST_DELAY_SEC < 0) REQUEST_DELAY_SEC <- 0.10
MAX_CONTEXT_CHARS <- suppressWarnings(as.integer(opt("BRIM_CNRFC_ACTIVE_SRC_MAX_CONTEXT_CHARS", 220L)))
if (is.na(MAX_CONTEXT_CHARS) || MAX_CONTEXT_CHARS < 40L) MAX_CONTEXT_CHARS <- 220L

pt_log <- function(...) message(format(Sys.time(), "%H:%M:%S"), " | ", ...)

pt_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
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

pt_fetch_url <- function(url, timeout = 60) {
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
        useragent = "BRIM CNRFC active source diagnostic",
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

pt_host_base <- function(url) {
  m <- stringr::str_match(url, "^(https?://[^/]+)")
  ifelse(is.na(m[1, 2]), "https://www.cnrfc.noaa.gov", m[1, 2])
}

pt_dir_base <- function(url) {
  host <- pt_host_base(url)
  path <- sub("^https?://[^/]+", "", url)
  path <- sub("[?#].*$", "", path)
  if (!grepl("/", path)) return(paste0(host, "/"))
  dir <- sub("/[^/]*$", "/", path)
  paste0(host, dir)
}

pt_resolve_url <- function(ref, page_url = "https://www.cnrfc.noaa.gov/") {
  ref <- trimws(as.character(ref))
  if (is.na(ref) || !nzchar(ref)) return(NA_character_)
  if (grepl("^https?://", ref, ignore.case = TRUE)) return(ref)
  if (grepl("^//", ref)) return(paste0("https:", ref))
  if (startsWith(ref, "/")) return(paste0(pt_host_base(page_url), ref))
  paste0(pt_dir_base(page_url), ref)
}

pt_extract_attr <- function(txt, tag, attr) {
  # Attribute extractor tolerant of quote style and attribute order.
  pat <- paste0("<", tag, "\\b[^>]*\\s", attr, "\\s*=\\s*(['\\\"])(.*?)\\1")
  m <- stringr::str_match_all(txt, stringr::regex(pat, ignore_case = TRUE, dotall = TRUE))[[1]]
  if (nrow(m) == 0) return(character())
  unique(m[, 3])
}

pt_extract_inline_scripts <- function(txt) {
  m <- stringr::str_match_all(txt, stringr::regex("<script\\b([^>]*)>(.*?)</script>", ignore_case = TRUE, dotall = TRUE))[[1]]
  if (nrow(m) == 0) return(tibble(script_index = integer(), attrs = character(), script_text = character()))
  tibble(
    script_index = seq_len(nrow(m)),
    attrs = m[, 2],
    script_text = m[, 3]
  ) %>%
    filter(!stringr::str_detect(.data$attrs, stringr::regex("\\bsrc\\s*=", ignore_case = TRUE))) %>%
    mutate(script_text = pt_clean_text(.data$script_text))
}

pt_count_regex <- function(txt, pattern) {
  if (is.na(txt) || !nzchar(txt)) return(0L)
  m <- stringr::str_count(txt, stringr::regex(pattern, ignore_case = TRUE))
  as.integer(ifelse(is.na(m), 0L, m))
}

pt_first_context <- function(txt, pattern, n = MAX_CONTEXT_CHARS) {
  if (is.na(txt) || !nzchar(txt)) return(NA_character_)
  m <- regexpr(pattern, txt, perl = TRUE, ignore.case = TRUE)
  if (m[[1]] < 0) return(NA_character_)
  start <- max(1L, as.integer(m[[1]]) - n)
  end <- min(nchar(txt), as.integer(m[[1]]) + attr(m, "match.length") + n)
  ctx <- substr(txt, start, end)
  ctx <- gsub("\\s+", " ", ctx)
  trimws(ctx)
}

pt_count_ids <- function(txt) {
  # CNRFC/NWSLI-ish tokens: 4-6 uppercase alphanumerics ending in state/region digit/letter.
  if (is.na(txt) || !nzchar(txt)) return(0L)
  ids <- unlist(regmatches(txt, gregexpr("(?<![A-Z0-9])[A-Z0-9]{4,6}(?![A-Z0-9])", txt, perl = TRUE)))
  ids <- ids[grepl("[A-Z]", ids) & grepl("[0-9]", ids)]
  length(unique(ids))
}

pt_extract_id_tokens <- function(txt, max_ids = 25L) {
  if (is.na(txt) || !nzchar(txt)) return(NA_character_)
  ids <- unlist(regmatches(txt, gregexpr("(?<![A-Z0-9])[A-Z0-9]{4,6}(?![A-Z0-9])", txt, perl = TRUE)))
  ids <- ids[grepl("[A-Z]", ids) & grepl("[0-9]", ids)]
  ids <- sort(unique(ids))
  if (length(ids) == 0) return(NA_character_)
  paste(head(ids, max_ids), collapse = ";")
}

pt_extract_candidate_refs <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(character())
  # Quoted strings that look like possible app data/script endpoints.
  m1 <- stringr::str_match_all(
    txt,
    stringr::regex("['\\\"]([^'\\\"]*(?:data/|\\.php|\\.xml|\\.json|\\.kml|\\.geojson|\\.js)[^'\\\"]*)['\\\"]", ignore_case = TRUE)
  )[[1]]
  out <- if (nrow(m1) > 0) m1[, 2] else character()
  out <- out[!grepl("^(javascript:|mailto:|#)", out, ignore.case = TRUE)]
  unique(out)
}

pt_array_candidates <- function(txt, source_name, source_url, max_chars = 100000L) {
  if (is.na(txt) || !nzchar(txt)) return(tibble())
  # Shallow variable/object assignments ending at semicolon.  This is diagnostic only.
  m <- stringr::str_match_all(
    substr(txt, 1, min(nchar(txt), max_chars)),
    stringr::regex("(?:var\\s+|let\\s+|const\\s+)?([A-Za-z_$][A-Za-z0-9_$]*)\\s*=\\s*(\\[[\\s\\S]{0,30000}?\\]|\\{[\\s\\S]{0,30000}?\\})\\s*;", ignore_case = FALSE)
  )[[1]]
  if (nrow(m) == 0) return(tibble())
  tibble(
    source_name = source_name,
    source_url = source_url,
    candidate_name = m[, 2],
    char_count = nchar(m[, 3]),
    id_token_count = vapply(m[, 3], pt_count_ids, integer(1)),
    sample_ids = vapply(m[, 3], pt_extract_id_tokens, character(1)),
    has_marker_word = stringr::str_detect(m[, 3], stringr::regex("marker|point|gage|forecast|reservoir|river", ignore_case = TRUE)),
    excerpt = substr(gsub("\\s+", " ", m[, 3]), 1, 360)
  ) %>%
    filter(.data$id_token_count > 0 | .data$has_marker_word)
}

# ==== 2. Product pages and known manual counts ===============================

active_products <- tibble::tribble(
  ~active_product_key,       ~active_product_label,       ~url,                                                    ~expected_marker_count, ~active_product_class,
  "official_river_points",   "Official River Points",     "https://www.cnrfc.noaa.gov/",                         102L,                   "official_river",
  "unofficial_river_points", "Unofficial River Points",   "https://www.cnrfc.noaa.gov/?product=otherPointsFcst", 185L,                   "unofficial_river",
  "reservoir_inflows",       "Reservoir Inflows",         "https://www.cnrfc.noaa.gov/?product=rsvrInflow",       104L,                   "reservoir_inflow",
  "reservoir_releases",      "Reservoir Releases",        "https://www.cnrfc.noaa.gov/?product=rsvrRelease",       61L,                    "reservoir_release"
)

keywords <- c(
  "otherPointsFcst", "rsvrInflow", "rsvrRelease", "product=", "Markers", "marker",
  "Official River", "Unofficial River", "Reservoir Inflow", "Reservoir Release",
  "river", "reservoir", "forecast", "gage", "NWSLI", "station",
  "L\\.marker", "google\\.maps\\.Marker", "createMarker", "addMarker",
  "markerArray", "gmarkers", "pointArray", "data/kml", "json", "xml", "kml"
)

# Candidate direct data endpoints to test.  These are intentionally broad and
# diagnostic; successful candidates should be inspected before integration.
endpoint_stems <- tibble::tribble(
  ~active_product_key,       ~stem,
  "official_river_points",   "officialRiverPoints",
  "official_river_points",   "OfficialRiverPoints",
  "official_river_points",   "riverPointsFcst",
  "official_river_points",   "RiverPointsFcst",
  "official_river_points",   "pointsFcst",
  "official_river_points",   "fcstPoints",
  "official_river_points",   "riverForecastPoints",
  "official_river_points",   "RiverForecastPoints",
  "official_river_points",   "riverFcst",
  "official_river_points",   "RiverFcst",
  "unofficial_river_points", "otherPointsFcst",
  "unofficial_river_points", "OtherPointsFcst",
  "unofficial_river_points", "unofficialRiverPoints",
  "unofficial_river_points", "UnofficialRiverPoints",
  "reservoir_inflows",       "rsvrInflow",
  "reservoir_inflows",       "RsvrInflow",
  "reservoir_inflows",       "reservoirInflow",
  "reservoir_inflows",       "ReservoirInflow",
  "reservoir_inflows",       "rsvrInflows",
  "reservoir_inflows",       "reservoirInflows",
  "reservoir_releases",      "rsvrRelease",
  "reservoir_releases",      "RsvrRelease",
  "reservoir_releases",      "reservoirRelease",
  "reservoir_releases",      "ReservoirRelease",
  "reservoir_releases",      "rsvrReleases",
  "reservoir_releases",      "reservoirReleases"
)

candidate_endpoint_urls <- endpoint_stems %>%
  tidyr::crossing(ext = c("xml", "json", "kml", "geojson")) %>%
  mutate(
    candidate_url = paste0("https://www.cnrfc.noaa.gov/data/kml/", .data$stem, ".", .data$ext)
  ) %>%
  left_join(active_products %>% select(.data$active_product_key, .data$active_product_label, .data$expected_marker_count), by = "active_product_key")

# ==== 3. Fetch pages ==========================================================

page_fetch_inventory <- tibble()
script_src_inventory <- tibble()
inline_script_inventory <- tibble()
keyword_context <- tibble()
endpoint_inventory <- tibble()
array_candidate_inventory <- tibble()
page_texts <- list()

for (i in seq_len(nrow(active_products))) {
  prod <- active_products[i, ]
  pt_log("Fetching active CNRFC product page [", i, "/", nrow(active_products), "]: ", prod$active_product_label)
  res <- pt_fetch_url(prod$url)
  txt <- res$text
  page_texts[[prod$active_product_key]] <- txt

  scripts <- pt_extract_attr(txt, "script", "src")
  links <- unique(c(
    pt_extract_attr(txt, "a", "href"),
    pt_extract_attr(txt, "link", "href"),
    pt_extract_attr(txt, "form", "action"),
    pt_extract_attr(txt, "iframe", "src")
  ))
  endpoint_refs <- pt_extract_candidate_refs(txt)

  page_fetch_inventory <- bind_rows(page_fetch_inventory, tibble(
    run_timestamp = RUN_TS,
    active_product_key = prod$active_product_key,
    active_product_label = prod$active_product_label,
    url = prod$url,
    final_url = res$final_url,
    status_code = res$status_code,
    http_ok = res$http_ok,
    bytes = res$bytes,
    decoded_chars = res$decoded_chars,
    content_type = res$content_type,
    page_title = pt_extract_title(txt),
    expected_marker_count = prod$expected_marker_count,
    script_src_count = length(scripts),
    link_like_count = length(links),
    candidate_endpoint_ref_count = length(endpoint_refs),
    page_hash = as.character(abs(sum(utf8ToInt(substr(txt, 1, min(nchar(txt), 500000L))), na.rm = TRUE))),
    error = res$error
  ))

  if (length(scripts) > 0) {
    script_src_inventory <- bind_rows(script_src_inventory, tibble(
      run_timestamp = RUN_TS,
      active_product_key = prod$active_product_key,
      active_product_label = prod$active_product_label,
      script_index = seq_along(scripts),
      script_src = scripts,
      script_url = vapply(scripts, pt_resolve_url, character(1), page_url = prod$url)
    ))
  }

  inline <- pt_extract_inline_scripts(txt)
  if (nrow(inline) > 0) {
    inline_script_inventory <- bind_rows(inline_script_inventory, inline %>%
      mutate(
        run_timestamp = RUN_TS,
        active_product_key = prod$active_product_key,
        active_product_label = prod$active_product_label,
        char_count = nchar(.data$script_text),
        id_token_count = vapply(.data$script_text, pt_count_ids, integer(1)),
        sample_ids = vapply(.data$script_text, pt_extract_id_tokens, character(1)),
        keyword_marker_count = vapply(.data$script_text, pt_count_regex, integer(1), pattern = "marker|point|gage|forecast|reservoir|river"),
        excerpt = substr(gsub("\\s+", " ", .data$script_text), 1, 500)
      ) %>%
        select(-.data$script_text))

    for (j in seq_len(nrow(inline))) {
      arr <- pt_array_candidates(inline$script_text[[j]],
                                 paste0(prod$active_product_key, ":inline_script_", inline$script_index[[j]]),
                                 prod$url)
      if (nrow(arr) > 0) {
        array_candidate_inventory <- bind_rows(array_candidate_inventory, arr %>% mutate(run_timestamp = RUN_TS, active_product_key = prod$active_product_key, active_product_label = prod$active_product_label))
      }
    }
  }

  if (length(endpoint_refs) > 0) {
    endpoint_inventory <- bind_rows(endpoint_inventory, tibble(
      run_timestamp = RUN_TS,
      active_product_key = prod$active_product_key,
      active_product_label = prod$active_product_label,
      source_type = "page_html_quoted_ref",
      ref = endpoint_refs,
      resolved_url = vapply(endpoint_refs, pt_resolve_url, character(1), page_url = prod$url)
    ))
  }

  for (kw in keywords) {
    ct <- pt_count_regex(txt, kw)
    if (ct > 0) {
      keyword_context <- bind_rows(keyword_context, tibble(
        run_timestamp = RUN_TS,
        active_product_key = prod$active_product_key,
        active_product_label = prod$active_product_label,
        source_type = "page_html",
        source_name = "page_html",
        source_url = prod$url,
        keyword = kw,
        count = ct,
        first_context = pt_first_context(txt, kw)
      ))
    }
  }

  if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
}

# ==== 4. Fetch external scripts =============================================

script_fetch_inventory <- tibble()

if (FETCH_EXTERNAL_SCRIPTS && nrow(script_src_inventory) > 0 && MAX_EXTERNAL_SCRIPTS > 0) {
  script_unique <- script_src_inventory %>%
    distinct(.data$script_url) %>%
    filter(!is.na(.data$script_url), nzchar(.data$script_url)) %>%
    slice_head(n = MAX_EXTERNAL_SCRIPTS)

  pt_log("Fetching external scripts: ", nrow(script_unique), " of ", n_distinct(script_src_inventory$script_url))

  for (i in seq_len(nrow(script_unique))) {
    surl <- script_unique$script_url[[i]]
    pt_log("  script [", i, "/", nrow(script_unique), "]: ", basename(strsplit(surl, "[?#]")[[1]][1]))
    res <- pt_fetch_url(surl, timeout = 60)
    txt <- res$text
    endpoint_refs <- pt_extract_candidate_refs(txt)

    script_fetch_inventory <- bind_rows(script_fetch_inventory, tibble(
      run_timestamp = RUN_TS,
      script_url = surl,
      final_url = res$final_url,
      status_code = res$status_code,
      http_ok = res$http_ok,
      bytes = res$bytes,
      decoded_chars = res$decoded_chars,
      content_type = res$content_type,
      id_token_count = pt_count_ids(txt),
      sample_ids = pt_extract_id_tokens(txt),
      candidate_endpoint_ref_count = length(endpoint_refs),
      marker_keyword_count = pt_count_regex(txt, "marker|point|gage|forecast|reservoir|river"),
      product_keyword_count = pt_count_regex(txt, "otherPointsFcst|rsvrInflow|rsvrRelease|official|unofficial"),
      error = res$error
    ))

    if (length(endpoint_refs) > 0) {
      endpoint_inventory <- bind_rows(endpoint_inventory, tibble(
        run_timestamp = RUN_TS,
        active_product_key = NA_character_,
        active_product_label = NA_character_,
        source_type = "external_script_quoted_ref",
        ref = endpoint_refs,
        resolved_url = vapply(endpoint_refs, pt_resolve_url, character(1), page_url = surl)
      ))
    }

    for (kw in keywords) {
      ct <- pt_count_regex(txt, kw)
      if (ct > 0) {
        keyword_context <- bind_rows(keyword_context, tibble(
          run_timestamp = RUN_TS,
          active_product_key = NA_character_,
          active_product_label = NA_character_,
          source_type = "external_script",
          source_name = basename(strsplit(surl, "[?#]")[[1]][1]),
          source_url = surl,
          keyword = kw,
          count = ct,
          first_context = pt_first_context(txt, kw)
        ))
      }
    }

    arr <- pt_array_candidates(txt, basename(strsplit(surl, "[?#]")[[1]][1]), surl, max_chars = 300000L)
    if (nrow(arr) > 0) {
      array_candidate_inventory <- bind_rows(array_candidate_inventory, arr %>% mutate(run_timestamp = RUN_TS, active_product_key = NA_character_, active_product_label = NA_character_))
    }

    if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
  }
}

endpoint_inventory <- endpoint_inventory %>%
  distinct(.data$source_type, .data$ref, .data$resolved_url, .keep_all = TRUE)

# ==== 5. Candidate direct endpoint fetch =====================================

candidate_endpoint_fetch <- tibble()

if (FETCH_CANDIDATE_ENDPOINTS && nrow(candidate_endpoint_urls) > 0) {
  pt_log("Fetching candidate direct data endpoints: ", nrow(candidate_endpoint_urls))

  for (i in seq_len(nrow(candidate_endpoint_urls))) {
    row <- candidate_endpoint_urls[i, ]
    res <- pt_fetch_url(row$candidate_url, timeout = 45)
    txt <- res$text
    looks_like_html <- stringr::str_detect(txt, stringr::regex("<html|<!doctype|404 Not Found|CNRFC - California Nevada", ignore_case = TRUE))

    candidate_endpoint_fetch <- bind_rows(candidate_endpoint_fetch, tibble(
      run_timestamp = RUN_TS,
      active_product_key = row$active_product_key,
      active_product_label = row$active_product_label,
      expected_marker_count = row$expected_marker_count,
      stem = row$stem,
      ext = row$ext,
      candidate_url = row$candidate_url,
      final_url = res$final_url,
      status_code = res$status_code,
      http_ok = res$http_ok,
      bytes = res$bytes,
      decoded_chars = res$decoded_chars,
      content_type = res$content_type,
      looks_like_html = looks_like_html,
      id_token_count = pt_count_ids(txt),
      sample_ids = pt_extract_id_tokens(txt),
      marker_keyword_count = pt_count_regex(txt, "marker|point|gage|forecast|reservoir|river|station"),
      href_count = pt_count_regex(txt, "href\\s*="),
      error = res$error
    ))

    if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)
  }
}

# ==== 6. Page comparison summary ============================================

page_compare <- tibble()
if (length(page_texts) > 1) {
  keys <- names(page_texts)
  for (i in seq_along(keys)) {
    for (j in seq_along(keys)) {
      if (j <= i) next
      a <- page_texts[[keys[[i]]]]
      b <- page_texts[[keys[[j]]]]
      n <- min(nchar(a), nchar(b))
      same_prefix_chars <- 0L
      if (n > 0) {
        aa <- strsplit(substr(a, 1, n), "", fixed = TRUE)[[1]]
        bb <- strsplit(substr(b, 1, n), "", fixed = TRUE)[[1]]
        mismatch <- which(aa != bb)
        same_prefix_chars <- if (length(mismatch) == 0) n else mismatch[[1]] - 1L
      }
      page_compare <- bind_rows(page_compare, tibble(
        run_timestamp = RUN_TS,
        page_a = keys[[i]],
        page_b = keys[[j]],
        chars_a = nchar(a),
        chars_b = nchar(b),
        same_prefix_chars = same_prefix_chars,
        length_delta = nchar(a) - nchar(b)
      ))
    }
  }
}

# ==== 7. Write outputs =======================================================

out_page_fetch <- file.path(DIR$qa, paste0("cnrfc_active_source_page_fetch_inventory_", RUN_TS, ".csv"))
out_script_src <- file.path(DIR$qa, paste0("cnrfc_active_source_script_src_inventory_", RUN_TS, ".csv"))
out_script_fetch <- file.path(DIR$qa, paste0("cnrfc_active_source_script_fetch_inventory_", RUN_TS, ".csv"))
out_context <- file.path(DIR$qa, paste0("cnrfc_active_source_keyword_context_", RUN_TS, ".csv"))
out_endpoint <- file.path(DIR$qa, paste0("cnrfc_active_source_endpoint_inventory_", RUN_TS, ".csv"))
out_candidate_fetch <- file.path(DIR$qa, paste0("cnrfc_active_source_candidate_endpoint_fetch_", RUN_TS, ".csv"))
out_arrays <- file.path(DIR$qa, paste0("cnrfc_active_source_array_candidate_inventory_", RUN_TS, ".csv"))
out_page_compare <- file.path(DIR$qa, paste0("cnrfc_active_source_page_compare_", RUN_TS, ".csv"))
out_summary <- file.path(DIR$qa, paste0("cnrfc_active_source_summary_", RUN_TS, ".csv"))

readr::write_csv(page_fetch_inventory, out_page_fetch)
readr::write_csv(script_src_inventory, out_script_src)
readr::write_csv(script_fetch_inventory, out_script_fetch)
readr::write_csv(keyword_context, out_context)
readr::write_csv(endpoint_inventory, out_endpoint)
readr::write_csv(candidate_endpoint_fetch, out_candidate_fetch)
readr::write_csv(array_candidate_inventory, out_arrays)
readr::write_csv(page_compare, out_page_compare)

summary_rows <- tibble(
  run_timestamp = RUN_TS,
  metric = c(
    "product_pages_attempted",
    "product_pages_http_ok",
    "unique_script_src_urls",
    "external_scripts_fetched",
    "endpoint_refs_found",
    "array_candidates_found",
    "candidate_direct_endpoints_tested",
    "candidate_direct_endpoints_http_ok_non_html",
    "candidate_direct_endpoints_near_expected_count"
  ),
  value = c(
    nrow(active_products),
    sum(page_fetch_inventory$http_ok, na.rm = TRUE),
    n_distinct(script_src_inventory$script_url),
    nrow(script_fetch_inventory),
    nrow(endpoint_inventory),
    nrow(array_candidate_inventory),
    nrow(candidate_endpoint_fetch),
    sum(candidate_endpoint_fetch$http_ok & !candidate_endpoint_fetch$looks_like_html, na.rm = TRUE),
    sum(
      candidate_endpoint_fetch$http_ok &
        !candidate_endpoint_fetch$looks_like_html &
        !is.na(candidate_endpoint_fetch$id_token_count) &
        !is.na(candidate_endpoint_fetch$expected_marker_count) &
        abs(candidate_endpoint_fetch$id_token_count - candidate_endpoint_fetch$expected_marker_count) <= 10,
      na.rm = TRUE
    )
  )
)
readr::write_csv(summary_rows, out_summary)

pt_log("CNRFC active forecast source diagnostic complete.")
message("  Page fetch inventory:       ", out_page_fetch)
message("  Script src inventory:       ", out_script_src)
message("  Script fetch inventory:     ", out_script_fetch)
message("  Keyword context:            ", out_context)
message("  Endpoint inventory:         ", out_endpoint)
message("  Candidate endpoint fetch:   ", out_candidate_fetch)
message("  Array candidate inventory:  ", out_arrays)
message("  Page compare:               ", out_page_compare)
message("  Summary:                    ", out_summary)

message("\nSource diagnostic summary:")
print(summary_rows, n = Inf)

if (nrow(candidate_endpoint_fetch) > 0) {
  message("\nBest candidate direct endpoints:")
  best <- candidate_endpoint_fetch %>%
    filter(.data$http_ok, !.data$looks_like_html) %>%
    mutate(
      expected_delta = abs(.data$id_token_count - .data$expected_marker_count)
    ) %>%
    arrange(.data$expected_delta, desc(.data$id_token_count), .data$active_product_key) %>%
    select(
      .data$active_product_label, .data$stem, .data$ext, .data$status_code,
      .data$bytes, .data$id_token_count, .data$expected_marker_count,
      .data$expected_delta, .data$candidate_url, .data$sample_ids
    ) %>%
    slice_head(n = 20)
  print(best, n = 20, width = Inf)
}

if (nrow(script_fetch_inventory) > 0) {
  message("\nExternal scripts most likely to contain marker/product logic:")
  script_top <- script_fetch_inventory %>%
    arrange(desc(.data$product_keyword_count), desc(.data$marker_keyword_count), desc(.data$id_token_count)) %>%
    select(.data$script_url, .data$status_code, .data$bytes, .data$id_token_count, .data$marker_keyword_count, .data$product_keyword_count, .data$sample_ids) %>%
    slice_head(n = 12)
  print(script_top, n = 12, width = Inf)
}

message("\nInterpretation note:")
message("  Patch 030's broad known-ID scan was too permissive.  Use this diagnostic to find")
message("  a product-specific XML/JSON/KML endpoint or JavaScript data object before wiring")
message("  active homepage marker authority into 53_.")
