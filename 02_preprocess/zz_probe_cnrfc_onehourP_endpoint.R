# zz_probe_cnrfc_onehourP_endpoint.R
# Purpose:
#   Discover whether the CNRFC onehourP interactive map exposes station IDs
#   in page HTML, linked JavaScript, or referenced data endpoints.

library(httr)
library(stringr)
library(dplyr)
library(readr)
library(tibble)

root_dir <- "C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2"
setwd(root_dir)

out_dir <- file.path(root_dir, "04_processed_data", "qa", "cnrfc_onehourP_page_probe")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

base_url <- "https://www.cnrfc.noaa.gov"
page_url <- "https://www.cnrfc.noaa.gov/?product=onehourP&lng=-118.241"

fetch_text <- function(url) {
  message("Fetching: ", url)
  
  resp <- httr::GET(
    url,
    httr::user_agent("BRIM CNRFC station inventory QA/QC; contact: BLM California hydrology"),
    httr::timeout(30)
  )
  
  status <- httr::status_code(resp)
  raw <- httr::content(resp, as = "raw")
  
  txt <- rawToChar(raw)
  txt <- iconv(txt, from = "", to = "UTF-8", sub = "byte")
  
  list(
    url = url,
    status = status,
    bytes = length(raw),
    text = txt
  )
}

page <- fetch_text(page_url)

writeLines(
  page$text,
  file.path(out_dir, "cnrfc_onehourP_page_raw.html"),
  useBytes = TRUE
)

message("Page status: ", page$status)
message("Page bytes:  ", page$bytes)

# Pull linked scripts and same-site URLs from the page.
script_srcs <- str_match_all(page$text, "<script[^>]+src=[\"']([^\"']+)[\"']")[[1]][, 2]
hrefs <- str_match_all(page$text, "(?:href|src)=[\"']([^\"']+)[\"']")[[1]][, 2]

normalize_url <- function(x) {
  x <- trimws(x)
  x <- x[!is.na(x) & nzchar(x)]
  
  dplyr::case_when(
    str_detect(x, "^https?://") ~ x,
    str_starts(x, "//") ~ paste0("https:", x),
    str_starts(x, "/") ~ paste0(base_url, x),
    TRUE ~ paste0(base_url, "/", x)
  )
}

linked_urls <- normalize_url(unique(c(script_srcs, hrefs)))

# Keep mostly same-site and JS/PHP-ish files that might define the map/data.
linked_urls <- linked_urls[
  str_detect(linked_urls, fixed("cnrfc.noaa.gov")) &
    str_detect(linked_urls, "\\.(js|php|html|json|xml)|product=|onehourP|map|precip|gage|obs")
]

linked_tbl <- tibble(url = linked_urls) %>%
  distinct()

readr::write_csv(
  linked_tbl,
  file.path(out_dir, "cnrfc_onehourP_linked_urls.csv")
)

message("Linked same-site candidate URLs: ", nrow(linked_tbl))

linked_texts <- list()

if (nrow(linked_tbl) > 0) {
  for (i in seq_len(nrow(linked_tbl))) {
    u <- linked_tbl$url[i]
    
    x <- tryCatch(
      fetch_text(u),
      error = function(e) {
        message("  FAILED: ", conditionMessage(e))
        NULL
      }
    )
    
    if (!is.null(x)) {
      safe_name <- paste0("linked_", sprintf("%03d", i), "_", gsub("[^A-Za-z0-9]+", "_", basename(u)), ".txt")
      if (nchar(safe_name) > 120) safe_name <- paste0("linked_", sprintf("%03d", i), ".txt")
      
      writeLines(
        x$text,
        file.path(out_dir, safe_name),
        useBytes = TRUE
      )
      
      linked_texts[[length(linked_texts) + 1]] <- x
    }
    
    Sys.sleep(0.15)
  }
}

all_docs <- c(
  list(page),
  linked_texts
)

# Search all downloaded text for likely data endpoint references.
endpoint_patterns <- c(
  "onehourP",
  "product",
  "precip",
  "gage",
  "station",
  "nwsid",
  "NWSID",
  "lat",
  "lng",
  "lon",
  "json",
  "xml",
  "php"
)

endpoint_hits <- bind_rows(lapply(all_docs, function(x) {
  lines <- unlist(strsplit(x$text, "\n", fixed = TRUE))
  
  hit <- Reduce(`|`, lapply(endpoint_patterns, function(p) {
    str_detect(lines, fixed(p, ignore_case = TRUE))
  }))
  
  tibble(
    source_url = x$url,
    line_number = which(hit),
    line = lines[hit]
  )
}))

readr::write_csv(
  endpoint_hits,
  file.path(out_dir, "cnrfc_onehourP_endpoint_keyword_hits.csv")
)

# Extract URL-looking things from all downloaded text.
all_text <- paste(vapply(all_docs, `[[`, character(1), "text"), collapse = "\n")

url_hits <- str_extract_all(
  all_text,
  "(?:https?://[^\"' <>]+|/[A-Za-z0-9_./?=&%-]+|[A-Za-z0-9_./-]+\\.php\\?[^\"' <>]+)"
)[[1]]

url_hits <- unique(url_hits[!is.na(url_hits) & nzchar(url_hits)])

url_hits_tbl <- tibble(raw_url = url_hits) %>%
  mutate(
    normalized_url = normalize_url(raw_url),
    mentions_onehourP = str_detect(raw_url, fixed("onehourP", ignore_case = TRUE)),
    mentions_precip = str_detect(raw_url, fixed("precip", ignore_case = TRUE)),
    mentions_gage = str_detect(raw_url, fixed("gage", ignore_case = TRUE)),
    mentions_station = str_detect(raw_url, fixed("station", ignore_case = TRUE))
  ) %>%
  arrange(desc(mentions_onehourP), desc(mentions_precip), desc(mentions_gage), raw_url)

readr::write_csv(
  url_hits_tbl,
  file.path(out_dir, "cnrfc_onehourP_url_hits.csv")
)

# Crude NWSID candidate extraction.
# This is intentionally broad; it is a discovery list, not final authority.
id_hits <- str_extract_all(
  all_text,
  "\\b[A-Z0-9]{3,5}(?:C1|N2|O3|U1|I1|A3|B1|MX)?\\b|\\bK[A-Z]{3}\\b"
)[[1]]

id_hits_tbl <- tibble(candidate_id = id_hits) %>%
  filter(!is.na(candidate_id), nzchar(candidate_id)) %>%
  count(candidate_id, sort = TRUE) %>%
  filter(
    !candidate_id %in% c(
      "HTML", "HEAD", "BODY", "HTTP", "HTTPS", "TRUE", "FALSE",
      "NULL", "JSON", "XML", "PHP", "CSS", "IMG", "PNG", "JPG",
      "DIV", "SPAN", "TYPE", "TEXT", "DATA", "DATE", "TIME",
      "LAT", "LON", "LNG", "MAP", "URL"
    )
  )

readr::write_csv(
  id_hits_tbl,
  file.path(out_dir, "cnrfc_onehourP_candidate_ids_regex_only.csv")
)

message("")
message("Wrote discovery outputs to:")
message("  ", out_dir)
message("")
message("Most important files:")
message("  cnrfc_onehourP_url_hits.csv")
message("  cnrfc_onehourP_endpoint_keyword_hits.csv")
message("  cnrfc_onehourP_candidate_ids_regex_only.csv")
message("")
message("Next: paste the top 30 rows of cnrfc_onehourP_url_hits.csv and any obvious endpoint_keyword_hits rows mentioning onehourP, precip, gage, station, or json.")