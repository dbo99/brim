# ==== qa_cnrfc_water_resources_update_diagnostic.r ==========================
##
## PURPOSE:
##   Diagnose what R actually receives from the CNRFC Daily Water Resources
##   Update page.  The basin availability audit proved the page returns a large
##   200 response, but the first parser did not recognize basin IDs/product
##   links.  This script writes small CSVs that inventory page text, links,
##   scripts, keywords, and CNRFC ID occurrences before we attempt a smarter
##   parser.
##
## OUTPUTS:
##   04_processed_data/qa/cnrfc_water_resources_update_page_diagnostic_<timestamp>.csv
##   04_processed_data/qa/cnrfc_water_resources_update_link_inventory_<timestamp>.csv
##   04_processed_data/qa/cnrfc_water_resources_update_script_inventory_<timestamp>.csv
##   04_processed_data/qa/cnrfc_water_resources_update_keyword_context_<timestamp>.csv
##   04_processed_data/qa/cnrfc_water_resources_update_id_scan_<timestamp>.csv

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
MAX_CONTEXT_IDS <- as.integer(getOption("BRIM_CNRFC_WRU_MAX_CONTEXT_IDS", 60L))
CONTEXT_IDS <- toupper(as.character(getOption(
  "BRIM_CNRFC_WRU_CONTEXT_IDS",
  c("PITC1", "NWMC1", "ORDC1", "HLEC1", "NMSC1", "FOLC1", "SHDC1", "AKYC1")
)))
REQUEST_DELAY_SEC <- as.numeric(getOption("BRIM_CNRFC_WRU_REQUEST_DELAY_SEC", 0.0))

pt_log <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " | ", ...)
}

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

  # NOAA/CNRFC pages occasionally include bytes that are not valid UTF-8.
  # Normalize once at the boundary so later tolower(), grepl(), nchar(), and
  # stringr calls do not fail with "invalid multibyte string".  Keep a
  # visible byte escape rather than silently dropping characters.
  out <- suppressWarnings(iconv(txt, from = "", to = "UTF-8", sub = "byte"))
  if (length(out) == 0 || is.na(out)) {
    out <- suppressWarnings(iconv(txt, from = "latin1", to = "UTF-8", sub = "byte"))
  }
  if (length(out) == 0 || is.na(out)) {
    out <- paste(strsplit(txt, "", fixed = TRUE)[[1]], collapse = "")
  }
  Encoding(out) <- "UTF-8"
  out
}

pt_decode_raw <- function(content) {
  if (length(content) >= 2 &&
      as.integer(content[[1]]) == 31L &&
      as.integer(content[[2]]) == 139L) {
    content <- tryCatch(memDecompress(content, type = "gzip"), error = function(e) content)
  }

  txt <- tryCatch(
    rawToChar(content),
    error = function(e) paste(rawToChar(content, multiple = TRUE), collapse = "")
  )
  pt_sanitize_text(txt)
}

pt_fetch_text <- function(url, timeout_sec = 20) {
  if (requireNamespace("curl", quietly = TRUE)) {
    h <- curl::new_handle(
      timeout = timeout_sec,
      followlocation = TRUE,
      useragent = "BRIM CNRFC Water Resources Update diagnostic (internal BRIM development)",
      httpheader = c("Accept-Encoding" = "identity")
    )
    res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
    if (inherits(res, "error")) {
      return(list(ok = FALSE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = conditionMessage(res)))
    }
    txt <- pt_decode_raw(res$content)
    return(list(
      ok = TRUE,
      status_code = res$status_code,
      bytes = length(res$content),
      text = txt,
      error = NA_character_
    ))
  }

  old_timeout <- getOption("timeout")
  options(timeout = max(timeout_sec, old_timeout %||% 60))
  on.exit(options(timeout = old_timeout), add = TRUE)
  txt <- tryCatch({
    con <- url(url, open = "rb")
    on.exit(close(con), add = TRUE)
    paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, error = function(e) e)

  if (inherits(txt, "error")) {
    return(list(ok = FALSE, status_code = NA_integer_, bytes = NA_integer_, text = "", error = conditionMessage(txt)))
  }
  list(ok = TRUE, status_code = NA_integer_, bytes = nchar(txt, type = "bytes"), text = txt, error = NA_character_)
}

pt_compact_text <- function(txt, max_n = 600) {
  txt <- gsub("<script[\\s\\S]*?</script>", " ", txt, ignore.case = TRUE)
  txt <- gsub("<style[\\s\\S]*?</style>", " ", txt, ignore.case = TRUE)
  txt <- gsub("<[^>]+>", " ", txt)
  txt <- gsub("&nbsp;", " ", txt, fixed = TRUE)
  txt <- gsub("&amp;", "&", txt, fixed = TRUE)
  txt <- gsub("\\s+", " ", txt)
  txt <- trimws(txt)
  substr(txt, 1, max_n)
}

pt_count_pattern <- function(txt, pattern, ignore.case = TRUE) {
  if (!nzchar(txt)) return(0L)
  m <- gregexpr(pattern, txt, ignore.case = ignore.case, perl = TRUE)
  if (length(m) == 0 || length(m[[1]]) == 0 || m[[1]][[1]] < 0) return(0L)
  length(m[[1]])
}

pt_extract_attrs <- function(txt, attr, base = "https://www.cnrfc.noaa.gov") {
  if (!nzchar(txt)) return(tibble(attr = character(), value = character(), url_normalized = character()))

  pattern <- paste0(attr, "\\s*=\\s*['\\\"]([^'\\\"]+)['\\\"]")
  m <- stringr::str_match_all(txt, pattern)[[1]]
  if (nrow(m) == 0) return(tibble(attr = character(), value = character(), url_normalized = character()))

  vals <- unique(m[, 2])
  norm <- ifelse(
    grepl("^https?://", vals, ignore.case = TRUE),
    vals,
    ifelse(
      startsWith(vals, "//"),
      paste0("https:", vals),
      paste0(base, ifelse(startsWith(vals, "/"), "", "/"), vals)
    )
  )

  tibble(attr = attr, value = vals, url_normalized = norm)
}

pt_extract_inline_scripts <- function(txt, max_chars = 700) {
  if (!nzchar(txt)) return(tibble(script_index = integer(), script_excerpt = character()))
  m <- gregexpr("<script[^>]*>[\\s\\S]*?</script>", txt, ignore.case = TRUE, perl = TRUE)
  scripts <- regmatches(txt, m)[[1]]
  if (length(scripts) == 0 || (length(scripts) == 1 && scripts[[1]] == "")) {
    return(tibble(script_index = integer(), script_excerpt = character()))
  }
  tibble(
    script_index = seq_along(scripts),
    script_excerpt = substr(gsub("\\s+", " ", scripts), 1, max_chars),
    chars = nchar(scripts, type = "chars"),
    has_cnrfc_id_pattern = grepl("\\b[A-Z]{3,4}C1\\b", scripts),
    has_ensemble_product = grepl("ensembleProduct|prodID", scripts, ignore.case = TRUE),
    has_water_supply_terms = grepl("Water Year|Forecast Flow|Accumulated|Volume|ESP|water supply", scripts, ignore.case = TRUE),
    has_geojson_or_marker_terms = grepl("geojson|marker|circleMarker|lat|lon|longitude|latitude", scripts, ignore.case = TRUE)
  )
}

pt_context_snippets <- function(txt, label, pattern, max_matches = 12L, window = 160L) {
  if (!nzchar(txt)) return(tibble(label = character(), snippet = character()))
  m <- gregexpr(pattern, txt, ignore.case = TRUE, perl = TRUE)[[1]]
  if (length(m) == 0 || m[[1]] < 0) return(tibble(label = label, snippet = character()))
  m <- head(m, max_matches)
  tibble(
    label = label,
    match_index = seq_along(m),
    snippet = purrr::map_chr(m, function(pos) {
      lo <- max(1L, pos - window)
      hi <- min(nchar(txt, type = "chars"), pos + window)
      snip <- substr(txt, lo, hi)
      snip <- gsub("\\s+", " ", snip)
      trimws(snip)
    })
  )
}

pt_title <- function(txt) {
  m <- stringr::str_match(txt, "<title[^>]*>([\\s\\S]*?)</title>")
  if (is.na(m[1, 2])) return(NA_character_)
  pt_compact_text(m[1, 2], max_n = 300)
}

# ==== 2. Fetch WR Update page ===============================================

pt_log("Fetching CNRFC Daily Water Resources Update: ", WRU_URL)
fetch <- pt_fetch_text(WRU_URL, timeout_sec = 25)
if (REQUEST_DELAY_SEC > 0) Sys.sleep(REQUEST_DELAY_SEC)

txt <- pt_sanitize_text(fetch$text)
pt_log("Fetch status: ", fetch$status_code %||% NA_integer_, "; bytes: ", fetch$bytes %||% NA_integer_, "; decoded chars: ", nchar(txt, type = "chars", allowNA = TRUE))

# ==== 3. Page diagnostic =====================================================

keyword_patterns <- c(
  cnrfc = "CNRFC|CALIFORNIA-NEVADA RIVER FORECAST CENTER|cnrfc\\.noaa\\.gov",
  water_resources_update = "Water Resources Update|water_resources_update",
  raw_esp = "Raw ESP|ESP Water Supply",
  water_year = "Water Year",
  forecast_flow_volume = "Forecast Flow Volume|Forecast Flow|Accumulated Forecast",
  accumulated_volume = "Accumulated|Volume",
  ensemble_product = "ensembleProduct|prodID",
  hydrograph = "hydrograph|river forecast|forecast point",
  leaflet_or_map = "Leaflet|leaflet|L\\.map|marker|circleMarker|geojson|GeoJSON",
  station_id_pattern = "\\b[A-Z]{3,4}C1\\b"
)

diag <- tibble(
  url = WRU_URL,
  ok = fetch$ok,
  status_code = fetch$status_code,
  bytes = fetch$bytes,
  decoded_chars = nchar(txt, type = "chars"),
  page_title = pt_title(txt),
  error = fetch$error,
  compact_page_excerpt = pt_compact_text(txt, max_n = 900),
  raw_start_excerpt = substr(txt, 1, 900)
)

for (nm in names(keyword_patterns)) {
  diag[[paste0("has_", nm)]] <- pt_count_pattern(txt, keyword_patterns[[nm]]) > 0
  diag[[paste0("count_", nm)]] <- pt_count_pattern(txt, keyword_patterns[[nm]])
}

# ==== 4. Link and script inventories ========================================

links <- bind_rows(
  pt_extract_attrs(txt, "href"),
  pt_extract_attrs(txt, "src"),
  pt_extract_attrs(txt, "action")
) |>
  mutate(
    contains_cnrfc_id_pattern = grepl("\\b[A-Z]{3,4}C1\\b", .data$url_normalized),
    contains_ensemble_product = grepl("ensembleProduct|prodID", .data$url_normalized, ignore.case = TRUE),
    contains_water_resources_update = grepl("water_resources_update", .data$url_normalized, ignore.case = TRUE),
    contains_water_supply_terms = grepl("Water|Forecast|Volume|ESP|supply|accum", .data$url_normalized, ignore.case = TRUE)
  ) |>
  distinct()

scripts <- pt_extract_inline_scripts(txt)

# ==== 5. Keyword contexts ====================================================

contexts <- bind_rows(
  pt_context_snippets(txt, "water_resources_update", "Water Resources Update|water_resources_update"),
  pt_context_snippets(txt, "raw_esp", "Raw ESP|ESP Water Supply"),
  pt_context_snippets(txt, "forecast_volume", "Forecast Flow Volume|Accumulated Forecast|Accumulated|Volume"),
  pt_context_snippets(txt, "ensemble_product", "ensembleProduct|prodID"),
  pt_context_snippets(txt, "station_id_pattern", "\\b[A-Z]{3,4}C1\\b"),
  pt_context_snippets(txt, "geojson_marker", "geojson|GeoJSON|marker|circleMarker|latitude|longitude|lat|lon")
)

# ==== 6. CNRFC ID scan ========================================================

feature_master_path <- pt_latest_file("^cnrfc_feature_master_.*\\.csv$")
matrix_path <- pt_latest_file("^cnrfc_basin_product_availability_matrix_.*\\.csv$")

feature_ids <- tibble()
if (!is.na(feature_master_path) && file.exists(feature_master_path)) {
  pt_log("Reading latest feature master for ID scan: ", feature_master_path)
  fm <- readr::read_csv(feature_master_path, show_col_types = FALSE, progress = FALSE)
  needed <- c("cnrfc_id", "feature_type", "display_name")
  for (.nm in setdiff(needed, names(fm))) fm[[.nm]] <- NA_character_
  feature_ids <- fm |>
    mutate(
      cnrfc_id = pt_clean_id(.data$cnrfc_id),
      feature_type = tolower(pt_chr(.data$feature_type))
    ) |>
    filter(
      !is.na(.data$cnrfc_id),
      .data$feature_type %in% c("cnrfc_basin", "cnrfc_fnf_sierra_delta_basin")
    ) |>
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

context_ids <- tibble(cnrfc_id = pt_clean_id(CONTEXT_IDS), from_explicit_context = TRUE) |>
  filter(!is.na(.data$cnrfc_id))

id_pool <- bind_rows(
  feature_ids |> mutate(from_feature_master = TRUE),
  matrix_ids,
  context_ids
) |>
  group_by(.data$cnrfc_id) |>
  summarise(
    feature_types = paste(sort(unique(na.omit(.data$feature_types))), collapse = ";"),
    display_name = pt_first_nonblank(.data$display_name),
    from_feature_master = any(.data$from_feature_master %||% FALSE, na.rm = TRUE),
    from_latest_matrix = any(.data$from_latest_matrix %||% FALSE, na.rm = TRUE),
    from_explicit_context = any(.data$from_explicit_context %||% FALSE, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    priority = case_when(
      .data$from_explicit_context ~ 1L,
      .data$from_latest_matrix ~ 2L,
      TRUE ~ 3L
    )
  ) |>
  arrange(.data$priority, .data$cnrfc_id)

if (is.finite(MAX_CONTEXT_IDS) && !is.na(MAX_CONTEXT_IDS) && MAX_CONTEXT_IDS > 0) {
  id_pool <- id_pool |> head(MAX_CONTEXT_IDS)
}

all_urls_text <- paste(links$url_normalized, collapse = "\n")
id_scan <- id_pool |>
  mutate(
    text_count_case_sensitive = purrr::map_int(.data$cnrfc_id, ~ pt_count_pattern(txt, paste0("\\b", .x, "\\b"), ignore.case = FALSE)),
    text_count_case_insensitive = purrr::map_int(.data$cnrfc_id, ~ pt_count_pattern(txt, paste0("\\b", .x, "\\b"), ignore.case = TRUE)),
    link_count_case_sensitive = purrr::map_int(.data$cnrfc_id, ~ pt_count_pattern(all_urls_text, paste0("\\b", .x, "\\b"), ignore.case = FALSE)),
    link_count_case_insensitive = purrr::map_int(.data$cnrfc_id, ~ pt_count_pattern(all_urls_text, paste0("\\b", .x, "\\b"), ignore.case = TRUE)),
    appears_anywhere = .data$text_count_case_insensitive > 0 | .data$link_count_case_insensitive > 0
  )

# ==== 7. Write outputs ========================================================

out_diag <- file.path(DIR$qa, paste0("cnrfc_water_resources_update_page_diagnostic_", RUN_TS, ".csv"))
out_links <- file.path(DIR$qa, paste0("cnrfc_water_resources_update_link_inventory_", RUN_TS, ".csv"))
out_scripts <- file.path(DIR$qa, paste0("cnrfc_water_resources_update_script_inventory_", RUN_TS, ".csv"))
out_context <- file.path(DIR$qa, paste0("cnrfc_water_resources_update_keyword_context_", RUN_TS, ".csv"))
out_idscan <- file.path(DIR$qa, paste0("cnrfc_water_resources_update_id_scan_", RUN_TS, ".csv"))

readr::write_csv(diag, out_diag)
readr::write_csv(links, out_links)
readr::write_csv(scripts, out_scripts)
readr::write_csv(contexts, out_context)
readr::write_csv(id_scan, out_idscan)

# ==== 8. Console summary ======================================================

message("CNRFC Water Resources Update diagnostic complete.")
message("  Page diagnostic: ", out_diag)
message("  Link inventory:  ", out_links)
message("  Script inventory:", out_scripts)
message("  Keyword context: ", out_context)
message("  ID scan:         ", out_idscan)
message("")

message("Page keyword summary:")
print(
  diag |>
    select(any_of(c(
      "status_code", "bytes", "decoded_chars", "page_title",
      "has_cnrfc", "count_cnrfc",
      "has_water_resources_update", "count_water_resources_update",
      "has_raw_esp", "count_raw_esp",
      "has_forecast_flow_volume", "count_forecast_flow_volume",
      "has_ensemble_product", "count_ensemble_product",
      "has_station_id_pattern", "count_station_id_pattern"
    ))),
  width = Inf
)

message("")
message("First ID scan rows:")
print(
  id_scan |>
    select(any_of(c(
      "cnrfc_id", "display_name", "feature_types",
      "from_explicit_context", "from_latest_matrix",
      "text_count_case_sensitive", "link_count_case_sensitive",
      "appears_anywhere"
    ))) |>
    head(25),
  n = 25,
  width = Inf
)

message("")
message("Interpretation note:")
message("  If the page has 200/large bytes but zero CNRFC IDs/links, the WR Update map likely uses a separate")
message("  script/data endpoint or a non-obvious encoded object. Use the link/script/context CSVs to find that next.")
