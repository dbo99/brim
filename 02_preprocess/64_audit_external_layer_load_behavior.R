# ==== 64_audit_external_layer_load_behavior.R ================================
#
# PURPOSE:
#   Build a repeatable QA/audit table for BRIM External Layers load behavior.
#   The output is meant to support later UI work such as green/yellow/red/gray
#   load-behavior badges in the External Layers panel.
#
# IMPORTANT DESIGN NOTES:
#   * This is a QA/audit script, not part of the standard BRIM map build.
#   * It does not modify the External catalog or the final HTML.
#   * It reads the current working copy of:
#       00_config/external_service_catalog.csv
#   * It filters rows the same way the External front end does:
#       primary_panel in {external, both}
#     Disabled and Ops-only rows do not consume visible display numbers.
#   * The default audit is static/heuristic so it is fast and safe.
#   * Optional live ArcGIS metadata checks can be enabled with:
#       Sys.setenv(BRIM_EXTERNAL_AUDIT_LIVE = "TRUE")
#     before sourcing this script. Live checks are intentionally metadata-only;
#     they do not query or download features.
#   * Optional empirical smoke tests can be enabled with:
#       Sys.setenv(BRIM_EXTERNAL_AUDIT_SMOKE = "TRUE")
#     before sourcing this script. Smoke tests make one lightweight provider
#     request per visible External row using the row's service type/load mode.
#     They are not run during normal BRIM builds.
#   * Optional multi-extent smoke tests can be enabled with:
#       Sys.setenv(BRIM_EXTERNAL_AUDIT_MULTI_EXTENT = "TRUE")
#     before sourcing this script. Multi-extent tests repeat the empirical smoke
#     test over several California/BRIM-ish view extents and optional repeats to
#     produce a more stable green/yellow/orange/red/gray recommendation.
#
# OUTPUTS:
#   04_processed_data/qa/external_layer_load_behavior_audit_latest.csv
#   04_processed_data/qa/external_layer_load_behavior_summary_latest.csv
#   04_processed_data/qa/external_layer_load_behavior_demo_candidates_latest.csv
#   04_processed_data/qa/external_layer_load_behavior_live_metadata_latest.csv
#     (only when live metadata checks are enabled and curl/jsonlite exist)
#   04_processed_data/qa/external_layer_load_behavior_smoke_latest.csv
#   04_processed_data/qa/external_layer_load_behavior_smoke_summary_latest.csv
#   04_processed_data/qa/external_layer_load_behavior_badge_recommendations_latest.csv
#     (only when empirical smoke tests are enabled and curl/jsonlite exist)
#   04_processed_data/qa/external_layer_load_behavior_multi_extent_tests_latest.csv
#   04_processed_data/qa/external_layer_load_behavior_multi_extent_summary_latest.csv
#   04_processed_data/qa/external_layer_load_behavior_badge_recommendations_multi_latest.csv
#   04_processed_data/qa/external_layer_load_behavior_badge_spread_latest.csv
#     (only when multi-extent smoke tests are enabled and curl/jsonlite exist)
#
# INTERPRETATION:
#   load_badge_heuristic:
#     green  = likely fast/reliable enough for demos
#     yellow = moderate; usually OK but may need zoom/seconds of patience
#     orange = slow/extent-sensitive; usable but not a first-click demo choice
#     red    = heavy/slow/intermittent; pretest before demo or warn users
#     gray   = unknown/insufficient metadata
#
#   This is a responsiveness/usability score, not a data quality score.
#

# ---- 0. Lightweight setup ----------------------------------------------------

if (file.exists("00_config/config_paths.r")) {
  source("00_config/config_paths.r")
} else if (requireNamespace("here", quietly = TRUE)) {
  root_guess <- here::here()
  if (file.exists(file.path(root_guess, "00_config/config_paths.r"))) {
    setwd(root_guess)
    source("00_config/config_paths.r")
  }
}

pt_log_064 <- function(...) {
  cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")
}

pt_clean_chr_064 <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

pt_num_or_na_064 <- function(x) {
  suppressWarnings(as.numeric(pt_clean_chr_064(x)))
}

pt_boolish_true_064 <- function(x) {
  tolower(pt_clean_chr_064(x)) %in% c("true", "yes", "1", "y")
}

pt_first_nonblank_064 <- function(...) {
  vals <- list(...)
  for (v in vals) {
    v <- pt_clean_chr_064(v)
    if (length(v) > 0 && !is.na(v[1]) && v[1] != "") return(v[1])
  }
  ""
}

pt_has_col_064 <- function(df, nm) nm %in% names(df)
pt_col_064 <- function(df, nm, default = "") {
  if (pt_has_col_064(df, nm)) df[[nm]] else rep(default, nrow(df))
}

pt_required_catalog_cols_064 <- c(
  "agency",
  "program",
  "theme",
  "external_group",
  "external_group_order",
  "external_subgroup",
  "external_subgroup_order",
  "external_layer_id",
  "display_name",
  "service_type",
  "service_url",
  "supports_popups",
  "default_clickable",
  "default_opacity",
  "notes",
  "source_page",
  "geographic_scope",
  "pt2_usage_note",
  "primary_panel",
  "priority",
  "default_load_mode",
  "where_clause",
  "large_layer_warning",
  "min_zoom_live",
  "min_zoom_current_view",
  "legend_url",
  "legend_note",
  "best_use",
  "useful_for_visualization",
  "popup_fields",
  "popup_aliases",
  "popup_link_template",
  "popup_link_label",
  "identify_url",
  "hover_fields",
  "hover_aliases",
  "hover_bold_fields",
  "hover_no_label_fields",
  "hover_round_fields",
  "hover_show_native_field_names",
  "default_label_field",
  "out_fields",
  "style_field_candidates",
  "default_style_field",
  "default_style_method",
  "style_units",
  "style_legend_title",
  "style_direction",
  "field_curation_notes",
  "show_native_field_names"
)

catalog_path <- file.path("00_config", "external_service_catalog.csv")
qa_dir <- file.path("04_processed_data", "qa")

if (!file.exists(catalog_path)) {
  stop("External service catalog not found: ", catalog_path)
}

if (!dir.exists(qa_dir)) dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

pt_log_064("Reading External catalog:", catalog_path)

catalog_df <- utils::read.csv(
  catalog_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

for (nm in pt_required_catalog_cols_064) {
  if (!nm %in% names(catalog_df)) catalog_df[[nm]] <- ""
}

catalog_df <- catalog_df[pt_required_catalog_cols_064]

# ---- 1. Mirror front-end visibility and ordering -----------------------------

catalog_df$service_url <- pt_clean_chr_064(catalog_df$service_url)
catalog_df$display_name <- pt_clean_chr_064(catalog_df$display_name)
catalog_df$agency <- pt_clean_chr_064(catalog_df$agency)
catalog_df$theme <- pt_clean_chr_064(catalog_df$theme)
catalog_df$external_group <- pt_clean_chr_064(catalog_df$external_group)
catalog_df$external_group_order <- pt_clean_chr_064(catalog_df$external_group_order)
catalog_df$external_subgroup <- pt_clean_chr_064(catalog_df$external_subgroup)
catalog_df$external_subgroup_order <- pt_clean_chr_064(catalog_df$external_subgroup_order)
catalog_df$external_layer_id <- pt_clean_chr_064(catalog_df$external_layer_id)
catalog_df$program <- pt_clean_chr_064(catalog_df$program)
catalog_df$service_type <- tolower(pt_clean_chr_064(catalog_df$service_type))
catalog_df$primary_panel <- tolower(pt_clean_chr_064(catalog_df$primary_panel))
catalog_df$default_load_mode <- tolower(pt_clean_chr_064(catalog_df$default_load_mode))

catalog_df <- catalog_df[
  !is.na(catalog_df$service_url) & catalog_df$service_url != "" &
    !is.na(catalog_df$display_name) & catalog_df$display_name != "",
  ,
  drop = FALSE
]

catalog_df$priority_num <- pt_num_or_na_064(catalog_df$priority)
catalog_df$priority_num[is.na(catalog_df$priority_num)] <- 999
catalog_df$external_group_order_num <- pt_num_or_na_064(catalog_df$external_group_order)
catalog_df$external_group_order_num[is.na(catalog_df$external_group_order_num)] <- 999
catalog_df$external_subgroup_order_num <- pt_num_or_na_064(catalog_df$external_subgroup_order)
catalog_df$external_subgroup_order_num[is.na(catalog_df$external_subgroup_order_num)] <- 999

catalog_df <- catalog_df[order(
  catalog_df$external_group_order_num,
  catalog_df$external_group,
  catalog_df$external_subgroup_order_num,
  catalog_df$external_subgroup,
  catalog_df$priority_num,
  catalog_df$agency,
  catalog_df$theme,
  catalog_df$display_name
), , drop = FALSE]

missing_ext_id <- is.na(catalog_df$external_layer_id) | catalog_df$external_layer_id == ""
catalog_df$external_layer_id[missing_ext_id] <- sprintf("EXT%03d", which(missing_ext_id))

catalog_df$front_end_visible <- catalog_df$primary_panel %in% c("external", "both")
catalog_df$external_display_num <- ""
catalog_df$external_display_num[catalog_df$front_end_visible] <- as.character(seq_len(sum(catalog_df$front_end_visible)))

visible_df <- catalog_df[catalog_df$front_end_visible, , drop = FALSE]

pt_log_064("Visible External rows:", nrow(visible_df), "of total catalog rows:", nrow(catalog_df))

# ---- 2. Static classification helpers ---------------------------------------

pt_endpoint_family_064 <- function(service_type, service_url) {
  service_type <- tolower(pt_clean_chr_064(service_type))
  u <- tolower(pt_clean_chr_064(service_url))

  if (service_type == "image" || grepl("/imageserver", u, fixed = TRUE)) return("imageserver")
  if (service_type == "geojson" || grepl("geojson", u, fixed = TRUE)) return("geojson")
  if (service_type == "feature" || grepl("/featureserver", u, fixed = TRUE)) return("featureserver")
  if (service_type == "map" || grepl("/mapserver", u, fixed = TRUE)) return("mapserver")
  if (service_type == "hub" || grepl("hub.arcgis.com|arcgis.com/home/item", u)) return("hub_page")

  "unknown"
}

pt_has_text_064 <- function(text, pattern) {
  grepl(pattern, tolower(pt_clean_chr_064(text)), perl = TRUE)
}

pt_score_one_064 <- function(row) {
  row <- as.list(row)

  nm <- pt_clean_chr_064(row$display_name)
  group <- pt_clean_chr_064(row$external_group)
  subgroup <- pt_clean_chr_064(row$external_subgroup)
  st <- tolower(pt_clean_chr_064(row$service_type))
  mode <- tolower(pt_clean_chr_064(row$default_load_mode))
  url <- pt_clean_chr_064(row$service_url)
  family <- pt_endpoint_family_064(st, url)
  min_live <- suppressWarnings(as.numeric(row$min_zoom_live))
  min_cv <- suppressWarnings(as.numeric(row$min_zoom_current_view))
  large_warning <- pt_clean_chr_064(row$large_layer_warning)
  where_clause <- pt_clean_chr_064(row$where_clause)
  notes_all <- paste(
    pt_clean_chr_064(row$notes),
    pt_clean_chr_064(row$pt2_usage_note),
    pt_clean_chr_064(row$field_curation_notes),
    pt_clean_chr_064(row$legend_note),
    sep = " | "
  )
  row_text <- paste(nm, group, subgroup, st, mode, family, notes_all, sep = " | ")

  points <- 0
  reasons <- character()
  unknown <- FALSE

  add <- function(n, reason) {
    points <<- points + n
    reasons <<- c(reasons, paste0(ifelse(n >= 0, "+", ""), n, " ", reason))
  }

  if (family == "unknown" && st == "") {
    unknown <- TRUE
    reasons <- c(reasons, "unknown service family/type")
  }

  if (mode == "tiled") {
    add(10, "tiled/cache-style visual layer")
  } else if (family == "geojson") {
    add(22, "GeoJSON/live feed")
  } else if (family == "imageserver") {
    add(58, "ImageServer visual raster")
  } else if (mode == "current_view") {
    add(44, "current-view feature/query snapshot")
  } else if (mode == "visual") {
    add(25, "visual provider-rendered layer")
  } else if (mode == "live") {
    add(35, "live feature/feed layer")
  } else if (family %in% c("featureserver", "mapserver")) {
    add(35, "ArcGIS service with unspecified/other load mode")
  } else {
    add(40, "fallback load-mode score")
  }

  if (family == "featureserver" && mode == "current_view") add(8, "FeatureServer current-view request")
  if (family == "mapserver" && mode == "current_view") add(12, "MapServer current-view query can be finicky")
  if (family == "imageserver" && pt_has_text_064(row_text, "ravg|subsidence|sar|altamira|image")) {
    add(12, "known visual-raster redraw/use case")
  }
  if (large_warning != "") add(18, "catalog large-layer warning present")
  if (!is.na(min_cv) && min_cv >= 12) add(12, "high current-view zoom gate")
  if (!is.na(min_cv) && min_cv >= 9 && min_cv < 12) add(7, "moderate current-view zoom gate")
  if (!is.na(min_live) && min_live >= 10) add(7, "live-layer zoom gate")
  if (where_clause != "") add(-2, "provider SQL filter may reduce load")

  if (pt_has_text_064(row_text, "huge|heavy|slow|intermittent|feature cap|capped|very large|zoom-gated|zoom gated")) {
    add(14, "notes/name suggest heavy or capped behavior")
  }
  if (pt_has_text_064(row_text, "debris|post-fire|post fire")) add(7, "post-fire/debris datasets can be spatially heavy")
  if (pt_has_text_064(row_text, "earthquake.*24 hours|past 24 hours")) add(-5, "short-window earthquake feed")
  if (pt_has_text_064(row_text, "ca geology|geologic map")) add(-8, "known simple tiled/visual reference use")
  if (pt_has_text_064(row_text, "npl boundaries|superfund.*boundaries")) add(-4, "moderate polygon boundary layer")

  if (unknown) {
    badge <- "gray"
    score_label <- "unknown / unaudited"
    demo_note <- "Audit first before demo."
  } else if (points <= 30) {
    badge <- "green"
    score_label <- "fast / reliable"
    demo_note <- "Good demo candidate."
  } else if (points <= 60) {
    badge <- "yellow"
    score_label <- "moderate"
    demo_note <- "Usually OK; allow a few seconds or pre-zoom."
  } else {
    badge <- "red"
    score_label <- "heavy / slow"
    demo_note <- "Pretest before demo; warn users or zoom in first."
  }

  data.frame(
    endpoint_family = family,
    load_points_heuristic = points,
    load_badge_heuristic = badge,
    load_score_label = score_label,
    demo_note = demo_note,
    heuristic_reasons = paste(reasons, collapse = "; "),
    stringsAsFactors = FALSE
  )
}

# ---- 3. Build static/heuristic audit ----------------------------------------

pt_log_064("Scoring visible External rows with static heuristics...")

score_rows <- lapply(seq_len(nrow(visible_df)), function(i) pt_score_one_064(visible_df[i, , drop = FALSE]))
score_df <- do.call(rbind, score_rows)

visible_out <- cbind(
  visible_df[, c(
    "external_display_num",
    "external_layer_id",
    "display_name",
    "external_group",
    "external_subgroup",
    "agency",
    "service_type",
    "default_load_mode",
    "service_url",
    "primary_panel",
    "min_zoom_live",
    "min_zoom_current_view",
    "large_layer_warning",
    "where_clause",
    "legend_url",
    "legend_note"
  ), drop = FALSE],
  score_df
)

# Keep numeric display sorting in the output CSV even though display numbers are
# stored as strings for parity with the browser-side catalog records.
visible_out$external_display_num_num <- suppressWarnings(as.numeric(visible_out$external_display_num))
visible_out <- visible_out[order(visible_out$external_display_num_num), , drop = FALSE]
visible_out$external_display_num_num <- NULL

# ---- 4. Optional live ArcGIS metadata checks --------------------------------

run_live <- tolower(Sys.getenv("BRIM_EXTERNAL_AUDIT_LIVE", "FALSE")) %in% c("true", "1", "yes", "y")
live_meta_out <- data.frame()

pt_arcgis_json_url_064 <- function(url) {
  url <- sub("/+$", "", pt_clean_chr_064(url))
  paste0(url, if (grepl("\\?", url)) "&" else "?", "f=pjson")
}

pt_live_metadata_one_064 <- function(row, timeout_sec = 8) {
  row <- as.list(row)
  family <- pt_endpoint_family_064(row$service_type, row$service_url)
  url <- pt_clean_chr_064(row$service_url)

  out <- data.frame(
    external_display_num = pt_clean_chr_064(row$external_display_num),
    external_layer_id = pt_clean_chr_064(row$external_layer_id),
    display_name = pt_clean_chr_064(row$display_name),
    endpoint_family = family,
    live_metadata_checked = FALSE,
    live_metadata_status = "skipped",
    http_status = NA_integer_,
    metadata_seconds = NA_real_,
    metadata_error = "",
    max_record_count = NA_real_,
    geometry_type = "",
    layer_count = NA_integer_,
    supports_query = NA,
    stringsAsFactors = FALSE
  )

  if (!family %in% c("featureserver", "mapserver", "imageserver")) {
    out$metadata_error <- "metadata check skipped for non-ArcGIS endpoint family"
    return(out)
  }

  if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    out$metadata_error <- "curl/jsonlite not installed; live metadata checks skipped"
    return(out)
  }

  json_url <- pt_arcgis_json_url_064(url)
  h <- curl::new_handle(
    timeout = timeout_sec,
    connecttimeout = min(5, timeout_sec),
    useragent = "BRIM external layer load-behavior audit"
  )

  start <- Sys.time()
  result <- tryCatch({
    res <- curl::curl_fetch_memory(json_url, handle = h)
    elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    txt <- rawToChar(res$content)
    meta <- jsonlite::fromJSON(txt, simplifyVector = FALSE)

    out$live_metadata_checked <- TRUE
    out$live_metadata_status <- if (res$status_code >= 200 && res$status_code < 300) "ok" else "http_error"
    out$http_status <- as.integer(res$status_code)
    out$metadata_seconds <- elapsed

    if (!is.null(meta$error$message)) {
      out$live_metadata_status <- "arcgis_error"
      out$metadata_error <- as.character(meta$error$message)
    }

    if (!is.null(meta$maxRecordCount)) out$max_record_count <- suppressWarnings(as.numeric(meta$maxRecordCount))
    if (!is.null(meta$geometryType)) out$geometry_type <- as.character(meta$geometryType)
    if (!is.null(meta$layers) && is.list(meta$layers)) out$layer_count <- length(meta$layers)

    caps <- tolower(paste(c(meta$capabilities, meta$supportedQueryFormats), collapse = " "))
    if (nzchar(caps)) out$supports_query <- grepl("query|geojson|json", caps)

    out
  }, error = function(e) {
    elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    out$live_metadata_checked <- TRUE
    out$live_metadata_status <- "request_error"
    out$metadata_seconds <- elapsed
    out$metadata_error <- conditionMessage(e)
    out
  })

  result
}

if (run_live) {
  pt_log_064("Running optional live ArcGIS metadata checks...")
  pt_log_064("Set BRIM_EXTERNAL_AUDIT_LIVE=FALSE to skip this in future runs.")

  max_live <- suppressWarnings(as.integer(Sys.getenv("BRIM_EXTERNAL_AUDIT_MAX_ROWS", "10000")))
  if (is.na(max_live) || max_live <= 0) max_live <- nrow(visible_out)

  rows_to_check <- visible_out[seq_len(min(max_live, nrow(visible_out))), , drop = FALSE]

  live_meta_rows <- vector("list", nrow(rows_to_check))
  for (i in seq_len(nrow(rows_to_check))) {
    pt_log_064("  metadata", i, "/", nrow(rows_to_check), "-", rows_to_check$display_name[i])
    live_meta_rows[[i]] <- pt_live_metadata_one_064(rows_to_check[i, , drop = FALSE])
  }

  live_meta_out <- do.call(rbind, live_meta_rows)

  live_path <- file.path(qa_dir, "external_layer_load_behavior_live_metadata_latest.csv")
  utils::write.csv(live_meta_out, live_path, row.names = FALSE)
  pt_log_064("Wrote live metadata QA:", live_path)

  visible_out <- merge(
    visible_out,
    live_meta_out[, c(
      "external_display_num",
      "external_layer_id",
      "live_metadata_checked",
      "live_metadata_status",
      "http_status",
      "metadata_seconds",
      "metadata_error",
      "max_record_count",
      "geometry_type",
      "layer_count",
      "supports_query"
    ), drop = FALSE],
    by = c("external_display_num", "external_layer_id"),
    all.x = TRUE,
    sort = FALSE
  )

  visible_out <- visible_out[order(suppressWarnings(as.numeric(visible_out$external_display_num))), , drop = FALSE]
}


# ---- 5. Optional empirical smoke-test checks --------------------------------

run_smoke <- tolower(Sys.getenv("BRIM_EXTERNAL_AUDIT_SMOKE", "FALSE")) %in% c("true", "1", "yes", "y")
smoke_out <- data.frame()

pt_query_string_064 <- function(params) {
  keep <- !vapply(params, function(x) length(x) == 0 || is.null(x) || is.na(x) || as.character(x) == "", logical(1))
  params <- params[keep]

  paste(
    names(params),
    vapply(params, function(x) utils::URLencode(as.character(x), reserved = TRUE), character(1)),
    sep = "=",
    collapse = "&"
  )
}

pt_url_with_params_064 <- function(base_url, params) {
  qs <- pt_query_string_064(params)
  if (!nzchar(qs)) return(base_url)
  paste0(base_url, if (grepl("\\?", base_url)) "&" else "?", qs)
}

pt_arcgis_parent_layer_064 <- function(url) {
  url <- sub("/+$", "", pt_clean_chr_064(url))
  m <- regexec("^(.*?/(?:MapServer|FeatureServer))(?:/(\\d+))?$", url, ignore.case = TRUE)
  parts <- regmatches(url, m)[[1]]

  if (length(parts) >= 2) {
    return(list(
      parent = parts[2],
      layer_id = if (length(parts) >= 3) parts[3] else ""
    ))
  }

  list(parent = url, layer_id = "")
}

pt_fetch_memory_064 <- function(url, timeout_sec = 15) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("curl package is not installed")
  }

  h <- curl::new_handle(
    timeout = timeout_sec,
    connecttimeout = min(6, timeout_sec),
    useragent = "BRIM external layer smoke-test audit"
  )

  start <- Sys.time()
  res <- curl::curl_fetch_memory(url, handle = h)
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  list(
    status_code = as.integer(res$status_code),
    content = res$content,
    seconds = elapsed,
    bytes = length(res$content)
  )
}

pt_parse_json_safely_064 <- function(raw_content) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package is not installed")
  }

  txt <- rawToChar(raw_content)
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

pt_arcgis_query_url_064 <- function(url, row, bbox, feature_cap) {
  where <- pt_clean_chr_064(row$where_clause)
  if (!nzchar(where)) where <- "1=1"

  pt_url_with_params_064(
    paste0(sub("/+$", "", pt_clean_chr_064(url)), "/query"),
    list(
      f = "json",
      where = where,
      outFields = "*",
      returnGeometry = "true",
      geometryType = "esriGeometryEnvelope",
      geometry = bbox,
      inSR = "4326",
      spatialRel = "esriSpatialRelIntersects",
      outSR = "4326",
      resultRecordCount = as.character(feature_cap)
    )
  )
}

pt_arcgis_map_export_url_064 <- function(url, bbox, image_size = "800,600") {
  info <- pt_arcgis_parent_layer_064(url)
  params <- list(
    f = "json",
    bbox = bbox,
    bboxSR = "4326",
    imageSR = "4326",
    size = image_size,
    format = "png32",
    transparent = "true"
  )

  if (nzchar(info$layer_id)) {
    params$layers <- paste0("show:", info$layer_id)
  }

  pt_url_with_params_064(paste0(info$parent, "/export"), params)
}

pt_arcgis_image_export_url_064 <- function(url, bbox, image_size = "800,600") {
  pt_url_with_params_064(
    paste0(sub("/+$", "", pt_clean_chr_064(url)), "/exportImage"),
    list(
      f = "json",
      bbox = bbox,
      bboxSR = "4326",
      imageSR = "4326",
      size = image_size,
      format = "png32",
      transparent = "true"
    )
  )
}

pt_smoke_badge_064 <- function(success, seconds, bytes, features_returned, hit_feature_cap, status) {
  if (!isTRUE(success)) return("red")
  if (tolower(pt_clean_chr_064(status)) %in% c("skipped", "unsupported", "not_tested")) return("gray")

  seconds <- suppressWarnings(as.numeric(seconds))
  bytes <- suppressWarnings(as.numeric(bytes))
  features_returned <- suppressWarnings(as.numeric(features_returned))
  hit_feature_cap <- isTRUE(hit_feature_cap)

  if (hit_feature_cap) return("red")
  if (!is.na(seconds) && seconds >= 12) return("red")
  if (!is.na(bytes) && bytes >= 8 * 1024^2) return("red")
  if (!is.na(features_returned) && features_returned >= 1800) return("red")

  if (!is.na(seconds) && seconds >= 4) return("yellow")
  if (!is.na(bytes) && bytes >= 2 * 1024^2) return("yellow")
  if (!is.na(features_returned) && features_returned >= 500) return("yellow")

  "green"
}

pt_smoke_note_064 <- function(badge, seconds, features_returned, hit_feature_cap, response_bytes, error_message) {
  if (nzchar(pt_clean_chr_064(error_message))) {
    return(paste("Smoke test failed or incomplete:", error_message))
  }

  sec_txt <- if (!is.na(seconds)) paste0(round(seconds, 2), " sec") else "unknown time"
  feat_txt <- if (!is.na(features_returned)) paste0(", ", features_returned, " feature(s)") else ""
  cap_txt <- if (isTRUE(hit_feature_cap)) ", hit feature cap" else ""
  byte_txt <- if (!is.na(response_bytes)) paste0(", ", round(response_bytes / 1024^2, 2), " MB") else ""

  paste0("Smoke test: ", badge, " (", sec_txt, feat_txt, cap_txt, byte_txt, ").")
}

pt_smoke_one_064 <- function(row, bbox, timeout_sec, feature_cap, image_size) {
  row <- as.list(row)
  family <- pt_endpoint_family_064(row$service_type, row$service_url)
  mode <- tolower(pt_clean_chr_064(row$default_load_mode))
  url <- pt_clean_chr_064(row$service_url)

  out <- data.frame(
    external_display_num = pt_clean_chr_064(row$external_display_num),
    external_layer_id = pt_clean_chr_064(row$external_layer_id),
    display_name = pt_clean_chr_064(row$display_name),
    endpoint_family = family,
    default_load_mode = mode,
    smoke_checked = FALSE,
    smoke_status = "skipped",
    smoke_test_mode = "",
    smoke_http_status = NA_integer_,
    smoke_seconds = NA_real_,
    smoke_response_bytes = NA_real_,
    smoke_features_returned = NA_real_,
    smoke_hit_feature_cap = FALSE,
    smoke_extent_wgs84 = bbox,
    smoke_error = "",
    load_badge_smoke = "gray",
    load_note_smoke = "Smoke test skipped or unsupported.",
    stringsAsFactors = FALSE
  )

  if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    out$smoke_status <- "skipped"
    out$smoke_error <- "curl/jsonlite not installed; empirical smoke tests skipped"
    return(out)
  }

  # Choose one provider request that approximates the BRIM front-end path.
  # This is intentionally conservative: it is a QA smoke test, not a full
  # browser-render benchmark.
  smoke_url <- ""

  if (family == "geojson") {
    smoke_url <- url
    out$smoke_test_mode <- "geojson_fetch"
  } else if (family %in% c("featureserver", "mapserver") && mode == "current_view") {
    smoke_url <- pt_arcgis_query_url_064(url, row, bbox, feature_cap)
    out$smoke_test_mode <- paste0(family, "_current_view_query")
  } else if (family == "featureserver" && mode %in% c("live", "", "default")) {
    smoke_url <- pt_arcgis_query_url_064(url, row, bbox, feature_cap)
    out$smoke_test_mode <- "featureserver_live_query"
  } else if (family == "mapserver" && mode %in% c("visual", "tiled", "", "default")) {
    smoke_url <- pt_arcgis_map_export_url_064(url, bbox, image_size)
    out$smoke_test_mode <- paste0("mapserver_", ifelse(mode == "tiled", "tile_proxy_export", "visual_export"))
  } else if (family == "imageserver") {
    smoke_url <- pt_arcgis_image_export_url_064(url, bbox, image_size)
    out$smoke_test_mode <- "imageserver_export_image"
  } else {
    out$smoke_status <- "unsupported"
    out$smoke_error <- paste("No empirical smoke-test path for endpoint family/load mode:", family, mode)
    out$load_badge_smoke <- "gray"
    out$load_note_smoke <- out$smoke_error
    return(out)
  }

  out$smoke_checked <- TRUE

  result <- tryCatch({
    fetched <- pt_fetch_memory_064(smoke_url, timeout_sec = timeout_sec)
    out$smoke_http_status <- fetched$status_code
    out$smoke_seconds <- fetched$seconds
    out$smoke_response_bytes <- fetched$bytes

    if (fetched$status_code < 200 || fetched$status_code >= 300) {
      out$smoke_status <- "http_error"
      out$smoke_error <- paste("HTTP", fetched$status_code)
    } else {
      parsed <- tryCatch(pt_parse_json_safely_064(fetched$content), error = function(e) NULL)

      if (is.null(parsed) && family != "geojson") {
        out$smoke_status <- "json_parse_failed"
        out$smoke_error <- "response was not parseable JSON"
      } else if (!is.null(parsed$error$message)) {
        out$smoke_status <- "arcgis_error"
        out$smoke_error <- as.character(parsed$error$message)
      } else {
        out$smoke_status <- "ok"

        if (!is.null(parsed$features) && is.list(parsed$features)) {
          out$smoke_features_returned <- length(parsed$features)
        }

        if (!is.null(parsed$exceededTransferLimit)) {
          out$smoke_hit_feature_cap <- isTRUE(parsed$exceededTransferLimit)
        }

        if (!is.na(out$smoke_features_returned) && out$smoke_features_returned >= feature_cap) {
          out$smoke_hit_feature_cap <- TRUE
        }
      }
    }

    out
  }, error = function(e) {
    out$smoke_status <- "request_error"
    out$smoke_error <- conditionMessage(e)
    out
  })

  success <- identical(result$smoke_status, "ok")
  result$load_badge_smoke <- pt_smoke_badge_064(
    success = success,
    seconds = result$smoke_seconds,
    bytes = result$smoke_response_bytes,
    features_returned = result$smoke_features_returned,
    hit_feature_cap = result$smoke_hit_feature_cap,
    status = result$smoke_status
  )
  result$load_note_smoke <- pt_smoke_note_064(
    badge = result$load_badge_smoke,
    seconds = result$smoke_seconds,
    features_returned = result$smoke_features_returned,
    hit_feature_cap = result$smoke_hit_feature_cap,
    response_bytes = result$smoke_response_bytes,
    error_message = result$smoke_error
  )

  result
}

if (run_smoke) {
  pt_log_064("Running optional empirical External-layer smoke tests...")
  pt_log_064("Set BRIM_EXTERNAL_AUDIT_SMOKE=FALSE to skip this in future runs.")

  smoke_bbox <- pt_clean_chr_064(Sys.getenv("BRIM_EXTERNAL_AUDIT_BBOX", "-124.5,32.0,-113.5,42.2"))
  smoke_timeout <- suppressWarnings(as.numeric(Sys.getenv("BRIM_EXTERNAL_AUDIT_SMOKE_TIMEOUT", "15")))
  if (is.na(smoke_timeout) || smoke_timeout <= 0) smoke_timeout <- 15
  smoke_cap <- suppressWarnings(as.integer(Sys.getenv("BRIM_EXTERNAL_AUDIT_SMOKE_FEATURE_CAP", "2000")))
  if (is.na(smoke_cap) || smoke_cap <= 0) smoke_cap <- 2000L
  smoke_size <- pt_clean_chr_064(Sys.getenv("BRIM_EXTERNAL_AUDIT_SMOKE_IMAGE_SIZE", "800,600"))
  max_smoke <- suppressWarnings(as.integer(Sys.getenv("BRIM_EXTERNAL_AUDIT_SMOKE_MAX_ROWS", "10000")))
  if (is.na(max_smoke) || max_smoke <= 0) max_smoke <- nrow(visible_out)

  rows_to_smoke <- visible_out[seq_len(min(max_smoke, nrow(visible_out))), , drop = FALSE]

  smoke_rows <- vector("list", nrow(rows_to_smoke))
  for (i in seq_len(nrow(rows_to_smoke))) {
    pt_log_064("  smoke", i, "/", nrow(rows_to_smoke), "-", rows_to_smoke$display_name[i])
    smoke_rows[[i]] <- pt_smoke_one_064(
      rows_to_smoke[i, , drop = FALSE],
      bbox = smoke_bbox,
      timeout_sec = smoke_timeout,
      feature_cap = smoke_cap,
      image_size = smoke_size
    )
  }

  smoke_out <- do.call(rbind, smoke_rows)

  smoke_path <- file.path(qa_dir, "external_layer_load_behavior_smoke_latest.csv")
  utils::write.csv(smoke_out, smoke_path, row.names = FALSE)
  pt_log_064("Wrote empirical smoke-test QA:", smoke_path)

  visible_out <- merge(
    visible_out,
    smoke_out[, c(
      "external_display_num",
      "external_layer_id",
      "smoke_checked",
      "smoke_status",
      "smoke_test_mode",
      "smoke_http_status",
      "smoke_seconds",
      "smoke_response_bytes",
      "smoke_features_returned",
      "smoke_hit_feature_cap",
      "smoke_extent_wgs84",
      "smoke_error",
      "load_badge_smoke",
      "load_note_smoke"
    ), drop = FALSE],
    by = c("external_display_num", "external_layer_id"),
    all.x = TRUE,
    sort = FALSE
  )

  visible_out <- visible_out[order(suppressWarnings(as.numeric(visible_out$external_display_num))), , drop = FALSE]

  visible_out$load_badge_recommended <- ifelse(
    !is.na(visible_out$load_badge_smoke) & visible_out$load_badge_smoke != "" & visible_out$load_badge_smoke != "gray",
    visible_out$load_badge_smoke,
    visible_out$load_badge_heuristic
  )
  visible_out$load_note_recommended <- ifelse(
    !is.na(visible_out$load_note_smoke) & visible_out$load_note_smoke != "" & visible_out$load_badge_smoke != "gray",
    visible_out$load_note_smoke,
    visible_out$demo_note
  )

  smoke_summary <- as.data.frame(table(visible_out$load_badge_recommended), stringsAsFactors = FALSE)
  names(smoke_summary) <- c("load_badge_recommended", "n_layers")
  smoke_summary <- smoke_summary[order(match(smoke_summary$load_badge_recommended, c("green", "yellow", "red", "gray"))), , drop = FALSE]

  smoke_summary_path <- file.path(qa_dir, "external_layer_load_behavior_smoke_summary_latest.csv")
  utils::write.csv(smoke_summary, smoke_summary_path, row.names = FALSE)
  pt_log_064("Wrote empirical smoke-test summary:", smoke_summary_path)

  badge_recs <- visible_out[, c(
    "external_display_num",
    "external_layer_id",
    "display_name",
    "external_group",
    "external_subgroup",
    "service_type",
    "default_load_mode",
    "load_badge_heuristic",
    "load_badge_smoke",
    "load_badge_recommended",
    "load_note_recommended",
    "smoke_status",
    "smoke_test_mode",
    "smoke_seconds",
    "smoke_response_bytes",
    "smoke_features_returned",
    "smoke_hit_feature_cap",
    "smoke_error",
    "service_url"
  ), drop = FALSE]

  badge_path <- file.path(qa_dir, "external_layer_load_behavior_badge_recommendations_latest.csv")
  utils::write.csv(badge_recs, badge_path, row.names = FALSE)
  pt_log_064("Wrote badge recommendation QA:", badge_path)
}



# ---- 6. Optional multi-extent empirical smoke-test checks -------------------

run_multi <- tolower(Sys.getenv("BRIM_EXTERNAL_AUDIT_MULTI_EXTENT", Sys.getenv("BRIM_EXTERNAL_AUDIT_MULTI", "FALSE"))) %in% c("true", "1", "yes", "y")
multi_out <- data.frame()
multi_summary_out <- data.frame()

pt_default_multi_extents_064 <- function() {
  data.frame(
    extent_name = c(
      "california_statewide",
      "sacramento_valley",
      "sierra_foothills",
      "mojave_desert",
      "project_scale_sacramento"
    ),
    bbox_wgs84 = c(
      "-124.5,32.0,-113.5,42.2",
      "-122.8,37.4,-119.9,40.0",
      "-121.7,36.2,-118.2,39.6",
      "-118.6,33.8,-114.0,36.9",
      "-121.75,38.30,-121.20,38.85"
    ),
    stringsAsFactors = FALSE
  )
}

pt_parse_multi_extents_064 <- function(x) {
  x <- pt_clean_chr_064(x)
  if (!nzchar(x)) return(pt_default_multi_extents_064())

  # Optional format:
  #   name=xmin,ymin,xmax,ymax;name2=xmin,ymin,xmax,ymax
  parts <- unlist(strsplit(x, ";", fixed = TRUE), use.names = FALSE)
  parts <- pt_clean_chr_064(parts)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(pt_default_multi_extents_064())

  rows <- lapply(seq_along(parts), function(i) {
    bit <- parts[i]
    if (grepl("=", bit, fixed = TRUE)) {
      nm <- sub("=.*$", "", bit)
      bb <- sub("^[^=]*=", "", bit)
    } else {
      nm <- paste0("custom_", i)
      bb <- bit
    }
    data.frame(extent_name = pt_clean_chr_064(nm), bbox_wgs84 = pt_clean_chr_064(bb), stringsAsFactors = FALSE)
  })

  out <- do.call(rbind, rows)
  good <- grepl("^-?\\d+(?:\\.\\d+)?,-?\\d+(?:\\.\\d+)?,-?\\d+(?:\\.\\d+)?,-?\\d+(?:\\.\\d+)?$", out$bbox_wgs84)
  out <- out[good, , drop = FALSE]
  if (!nrow(out)) pt_default_multi_extents_064() else out
}

pt_q_064 <- function(x, prob) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, type = 7))
}

pt_max_or_na_064 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  max(x, na.rm = TRUE)
}

pt_mean_or_na_064 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  mean(x, na.rm = TRUE)
}

pt_multi_badge_064 <- function(success_rate, median_seconds, p90_seconds, max_seconds,
                               p90_response_mb, max_response_mb,
                               max_features, feature_cap_rate, n_success, n_tests) {
  reasons <- character()
  badge <- "green"

  add_reason <- function(txt) reasons <<- c(reasons, txt)

  if (is.na(n_tests) || n_tests <= 0 || is.na(n_success)) {
    return(list(badge = "gray", reasons = "no multi-extent tests completed"))
  }
  if (n_success <= 0) {
    return(list(badge = "red", reasons = "no successful smoke-test requests"))
  }

  if (!is.na(success_rate) && success_rate < 0.75) add_reason("success rate below 75%")
  if (!is.na(p90_seconds) && p90_seconds >= 12) add_reason("p90 request time >= 12 sec")
  if (!is.na(max_seconds) && max_seconds >= 15) add_reason("at least one request reached the timeout window")
  if (!is.na(feature_cap_rate) && feature_cap_rate >= 0.50) add_reason("feature cap in at least half of successful tests")
  if (!is.na(max_response_mb) && max_response_mb >= 12) add_reason("max response >= 12 MB")
  if (!is.na(max_features) && max_features >= 2000) add_reason("max features near/above 2,000 cap")

  if (length(reasons)) {
    return(list(badge = "red", reasons = paste(reasons, collapse = "; ")))
  }

  if (!is.na(success_rate) && success_rate < 0.95) add_reason("some extent/repeat failures")
  if (!is.na(p90_seconds) && p90_seconds >= 6) add_reason("p90 request time >= 6 sec")
  if (!is.na(median_seconds) && median_seconds >= 4) add_reason("median request time >= 4 sec")
  if (!is.na(feature_cap_rate) && feature_cap_rate > 0) add_reason("feature cap in at least one test")
  if (!is.na(max_response_mb) && max_response_mb >= 6) add_reason("max response >= 6 MB")
  if (!is.na(max_features) && max_features >= 1800) add_reason("max features near cap")

  if (length(reasons)) {
    return(list(badge = "orange", reasons = paste(reasons, collapse = "; ")))
  }

  if (!is.na(p90_seconds) && p90_seconds >= 3) add_reason("p90 request time >= 3 sec")
  if (!is.na(median_seconds) && median_seconds >= 1.5) add_reason("median request time >= 1.5 sec")
  if (!is.na(p90_response_mb) && p90_response_mb >= 2) add_reason("p90 response >= 2 MB")
  if (!is.na(max_features) && max_features >= 500) add_reason("feature count can be moderate")

  if (length(reasons)) {
    return(list(badge = "yellow", reasons = paste(reasons, collapse = "; ")))
  }

  list(badge = "green", reasons = "fast and stable across tested extents/repeats")
}

pt_multi_summary_one_064 <- function(d) {
  d <- d[order(d$smoke_repeat, d$smoke_extent_name), , drop = FALSE]

  ok <- d$smoke_status == "ok"
  n_tests <- nrow(d)
  n_checked <- sum(isTRUE(d$smoke_checked) | d$smoke_checked %in% TRUE, na.rm = TRUE)
  n_success <- sum(ok, na.rm = TRUE)
  n_failed <- n_tests - n_success
  success_rate <- if (n_tests > 0) n_success / n_tests else NA_real_

  sec <- suppressWarnings(as.numeric(d$smoke_seconds[ok]))
  bytes <- suppressWarnings(as.numeric(d$smoke_response_bytes[ok]))
  mb <- bytes / 1024^2
  feats <- suppressWarnings(as.numeric(d$smoke_features_returned[ok]))
  cap_vec <- d$smoke_hit_feature_cap[ok]
  cap_vec[is.na(cap_vec)] <- FALSE
  feature_cap_rate <- if (length(cap_vec)) mean(cap_vec) else 0

  median_seconds <- pt_q_064(sec, 0.50)
  p75_seconds <- pt_q_064(sec, 0.75)
  p90_seconds <- pt_q_064(sec, 0.90)
  max_seconds <- pt_max_or_na_064(sec)
  median_response_mb <- pt_q_064(mb, 0.50)
  p90_response_mb <- pt_q_064(mb, 0.90)
  max_response_mb <- pt_max_or_na_064(mb)
  median_features <- pt_q_064(feats, 0.50)
  max_features <- pt_max_or_na_064(feats)

  failed <- d[!ok, , drop = FALSE]
  failed_extents <- paste(unique(pt_clean_chr_064(failed$smoke_extent_name)), collapse = "; ")
  error_sample <- paste(unique(pt_clean_chr_064(failed$smoke_error[pt_clean_chr_064(failed$smoke_error) != ""])), collapse = " | ")
  if (nchar(error_sample) > 240) error_sample <- paste0(substr(error_sample, 1, 237), "...")

  badge_info <- pt_multi_badge_064(
    success_rate = success_rate,
    median_seconds = median_seconds,
    p90_seconds = p90_seconds,
    max_seconds = max_seconds,
    p90_response_mb = p90_response_mb,
    max_response_mb = max_response_mb,
    max_features = max_features,
    feature_cap_rate = feature_cap_rate,
    n_success = n_success,
    n_tests = n_tests
  )

  sec_txt <- if (!is.na(median_seconds)) paste0("median ", round(median_seconds, 2), " sec") else "median n/a"
  p90_txt <- if (!is.na(p90_seconds)) paste0("p90 ", round(p90_seconds, 2), " sec") else "p90 n/a"
  max_feat_txt <- if (!is.na(max_features)) paste0("max features ", round(max_features)) else "max features n/a"
  cap_txt <- if (!is.na(feature_cap_rate) && feature_cap_rate > 0) paste0("; cap rate ", round(100 * feature_cap_rate), "%") else ""

  data.frame(
    external_display_num = d$external_display_num[1],
    external_layer_id = d$external_layer_id[1],
    display_name = d$display_name[1],
    external_group = d$external_group[1],
    external_subgroup = d$external_subgroup[1],
    service_type = d$service_type[1],
    default_load_mode = d$default_load_mode[1],
    n_tests = n_tests,
    n_checked = n_checked,
    n_success = n_success,
    n_failed = n_failed,
    success_rate = success_rate,
    median_seconds = median_seconds,
    p75_seconds = p75_seconds,
    p90_seconds = p90_seconds,
    max_seconds = max_seconds,
    median_response_mb = median_response_mb,
    p90_response_mb = p90_response_mb,
    max_response_mb = max_response_mb,
    median_features = median_features,
    max_features = max_features,
    feature_cap_rate = feature_cap_rate,
    failed_extents = failed_extents,
    error_sample = error_sample,
    load_badge_multi = badge_info$badge,
    load_note_multi = paste0("Multi-extent smoke test: ", badge_info$badge, " (", n_success, "/", n_tests, " ok; ", sec_txt, "; ", p90_txt, "; ", max_feat_txt, cap_txt, ")."),
    multi_badge_reasons = badge_info$reasons,
    stringsAsFactors = FALSE
  )
}

if (run_multi) {
  pt_log_064("Running optional multi-extent empirical External-layer smoke tests...")
  pt_log_064("Set BRIM_EXTERNAL_AUDIT_MULTI_EXTENT=FALSE to skip this in future runs.")

  multi_extents <- pt_parse_multi_extents_064(Sys.getenv("BRIM_EXTERNAL_AUDIT_MULTI_EXTENTS", ""))
  multi_timeout <- suppressWarnings(as.numeric(Sys.getenv("BRIM_EXTERNAL_AUDIT_MULTI_TIMEOUT", Sys.getenv("BRIM_EXTERNAL_AUDIT_SMOKE_TIMEOUT", "15"))))
  if (is.na(multi_timeout) || multi_timeout <= 0) multi_timeout <- 15
  multi_cap <- suppressWarnings(as.integer(Sys.getenv("BRIM_EXTERNAL_AUDIT_MULTI_FEATURE_CAP", Sys.getenv("BRIM_EXTERNAL_AUDIT_SMOKE_FEATURE_CAP", "2000"))))
  if (is.na(multi_cap) || multi_cap <= 0) multi_cap <- 2000L
  multi_size <- pt_clean_chr_064(Sys.getenv("BRIM_EXTERNAL_AUDIT_MULTI_IMAGE_SIZE", Sys.getenv("BRIM_EXTERNAL_AUDIT_SMOKE_IMAGE_SIZE", "800,600")))
  multi_repeats <- suppressWarnings(as.integer(Sys.getenv("BRIM_EXTERNAL_AUDIT_MULTI_REPEATS", "2")))
  if (is.na(multi_repeats) || multi_repeats <= 0) multi_repeats <- 2L
  max_multi <- suppressWarnings(as.integer(Sys.getenv("BRIM_EXTERNAL_AUDIT_MULTI_MAX_ROWS", "10000")))
  if (is.na(max_multi) || max_multi <= 0) max_multi <- nrow(visible_out)

  rows_to_multi <- visible_out[seq_len(min(max_multi, nrow(visible_out))), , drop = FALSE]
  total_tests <- nrow(rows_to_multi) * nrow(multi_extents) * multi_repeats
  pt_log_064("  rows:", nrow(rows_to_multi), "| extents:", nrow(multi_extents), "| repeats:", multi_repeats, "| total requests:", total_tests)
  pt_log_064("  timeout per request:", multi_timeout, "sec")

  multi_rows <- vector("list", total_tests)
  k <- 0L
  for (i in seq_len(nrow(rows_to_multi))) {
    for (r in seq_len(multi_repeats)) {
      for (e in seq_len(nrow(multi_extents))) {
        k <- k + 1L
        pt_log_064("  multi", k, "/", total_tests, "-", rows_to_multi$display_name[i], "|", multi_extents$extent_name[e], "| repeat", r)
        tmp <- pt_smoke_one_064(
          rows_to_multi[i, , drop = FALSE],
          bbox = multi_extents$bbox_wgs84[e],
          timeout_sec = multi_timeout,
          feature_cap = multi_cap,
          image_size = multi_size
        )
        tmp$smoke_extent_name <- multi_extents$extent_name[e]
        tmp$smoke_repeat <- r
        tmp$external_group <- rows_to_multi$external_group[i]
        tmp$external_subgroup <- rows_to_multi$external_subgroup[i]
        tmp$service_type <- rows_to_multi$service_type[i]
        tmp$service_url <- rows_to_multi$service_url[i]
        multi_rows[[k]] <- tmp
      }
    }
  }

  multi_out <- do.call(rbind, multi_rows)
  multi_out <- multi_out[order(
    suppressWarnings(as.numeric(multi_out$external_display_num)),
    multi_out$smoke_repeat,
    multi_out$smoke_extent_name
  ), , drop = FALSE]

  multi_path <- file.path(qa_dir, "external_layer_load_behavior_multi_extent_tests_latest.csv")
  utils::write.csv(multi_out, multi_path, row.names = FALSE)
  pt_log_064("Wrote multi-extent smoke-test request QA:", multi_path)

  split_keys <- paste(multi_out$external_display_num, multi_out$external_layer_id, sep = "||")
  multi_summary_rows <- lapply(split(multi_out, split_keys), pt_multi_summary_one_064)
  multi_summary_out <- do.call(rbind, multi_summary_rows)
  multi_summary_out <- multi_summary_out[order(suppressWarnings(as.numeric(multi_summary_out$external_display_num))), , drop = FALSE]

  # Attach the one-shot/static recommendations where available, so the review CSV
  # can compare all stages side-by-side.
  compare_cols <- c(
    "external_display_num", "external_layer_id", "load_badge_heuristic",
    "load_badge_smoke", "load_badge_recommended", "load_note_recommended", "heuristic_reasons"
  )
  compare_cols <- compare_cols[compare_cols %in% names(visible_out)]
  multi_recs <- merge(
    multi_summary_out,
    visible_out[, compare_cols, drop = FALSE],
    by = c("external_display_num", "external_layer_id"),
    all.x = TRUE,
    sort = FALSE
  )
  multi_recs <- multi_recs[order(suppressWarnings(as.numeric(multi_recs$external_display_num))), , drop = FALSE]
  multi_recs$load_badge_final_candidate <- multi_recs$load_badge_multi
  multi_recs$load_note_final_candidate <- multi_recs$load_note_multi

  multi_summary_path <- file.path(qa_dir, "external_layer_load_behavior_multi_extent_summary_latest.csv")
  utils::write.csv(multi_summary_out, multi_summary_path, row.names = FALSE)
  pt_log_064("Wrote multi-extent smoke-test summary:", multi_summary_path)

  multi_recs_path <- file.path(qa_dir, "external_layer_load_behavior_badge_recommendations_multi_latest.csv")
  utils::write.csv(multi_recs, multi_recs_path, row.names = FALSE)
  pt_log_064("Wrote multi-extent badge recommendation QA:", multi_recs_path)

  badge_levels <- c("green", "yellow", "orange", "red", "gray")
  badge_spread <- as.data.frame(table(factor(multi_recs$load_badge_multi, levels = badge_levels)), stringsAsFactors = FALSE)
  names(badge_spread) <- c("load_badge_multi", "n_layers")
  badge_spread <- badge_spread[badge_spread$n_layers > 0, , drop = FALSE]

  # Add simple timing distribution by final candidate badge for quick review.
  timing_spread <- do.call(rbind, lapply(split(multi_recs, multi_recs$load_badge_multi), function(d) {
    data.frame(
      load_badge_multi = d$load_badge_multi[1],
      n_layers = nrow(d),
      median_of_median_seconds = pt_q_064(d$median_seconds, 0.50),
      p90_of_p90_seconds = pt_q_064(d$p90_seconds, 0.90),
      max_of_max_seconds = pt_max_or_na_064(d$max_seconds),
      median_success_rate = pt_q_064(d$success_rate, 0.50),
      n_with_any_feature_cap = sum(suppressWarnings(as.numeric(d$feature_cap_rate)) > 0, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  timing_spread <- timing_spread[order(match(timing_spread$load_badge_multi, badge_levels)), , drop = FALSE]

  spread_path <- file.path(qa_dir, "external_layer_load_behavior_badge_spread_latest.csv")
  utils::write.csv(timing_spread, spread_path, row.names = FALSE)
  pt_log_064("Wrote multi-extent badge spread QA:", spread_path)

  # Merge the multi-extent recommendation into the main audit output too.
  visible_out <- merge(
    visible_out,
    multi_recs[, c(
      "external_display_num", "external_layer_id", "n_tests", "n_success", "n_failed",
      "success_rate", "median_seconds", "p90_seconds", "max_seconds",
      "max_features", "feature_cap_rate", "load_badge_multi", "load_note_multi",
      "multi_badge_reasons", "load_badge_final_candidate", "load_note_final_candidate"
    ), drop = FALSE],
    by = c("external_display_num", "external_layer_id"),
    all.x = TRUE,
    sort = FALSE
  )
  visible_out <- visible_out[order(suppressWarnings(as.numeric(visible_out$external_display_num))), , drop = FALSE]

  pt_log_064("Multi-extent badge summary:")
  print(badge_spread, row.names = FALSE)
}

# ---- 7. Write QA outputs -----------------------------------------------------

audit_path <- file.path(qa_dir, "external_layer_load_behavior_audit_latest.csv")
summary_path <- file.path(qa_dir, "external_layer_load_behavior_summary_latest.csv")
demo_path <- file.path(qa_dir, "external_layer_load_behavior_demo_candidates_latest.csv")

utils::write.csv(visible_out, audit_path, row.names = FALSE)
pt_log_064("Wrote External load-behavior audit:", audit_path)

summary_df <- as.data.frame(table(visible_out$load_badge_heuristic), stringsAsFactors = FALSE)
names(summary_df) <- c("load_badge_heuristic", "n_layers")
summary_df <- summary_df[order(match(summary_df$load_badge_heuristic, c("green", "yellow", "red", "gray"))), , drop = FALSE]
utils::write.csv(summary_df, summary_path, row.names = FALSE)
pt_log_064("Wrote External load-behavior summary:", summary_path)

demo_df <- visible_out[visible_out$load_badge_heuristic %in% c("green", "yellow"), , drop = FALSE]
demo_df <- demo_df[order(
  match(demo_df$load_badge_heuristic, c("green", "yellow")),
  demo_df$external_group,
  demo_df$external_subgroup,
  suppressWarnings(as.numeric(demo_df$external_display_num))
), , drop = FALSE]
utils::write.csv(demo_df, demo_path, row.names = FALSE)
pt_log_064("Wrote demo-candidate list:", demo_path)

pt_log_064("")
pt_log_064("Done: External layer load-behavior audit complete.")
pt_log_064("Badge summary:")
print(summary_df, row.names = FALSE)

if (!run_live) {
  pt_log_064("")
  pt_log_064("Optional live metadata check was skipped.")
  pt_log_064("To run it later:")
  pt_log_064("  Sys.setenv(BRIM_EXTERNAL_AUDIT_LIVE = 'TRUE')")
  pt_log_064("  source('02_preprocess/64_audit_external_layer_load_behavior.R')")
}

if (!run_smoke) {
  pt_log_064("")
  pt_log_064("Optional empirical smoke test was skipped.")
  pt_log_064("To run it later:")
  pt_log_064("  Sys.setenv(BRIM_EXTERNAL_AUDIT_SMOKE = 'TRUE')")
  pt_log_064("  source('02_preprocess/64_audit_external_layer_load_behavior.R')")
}

if (!run_multi) {
  pt_log_064("")
  pt_log_064("Optional multi-extent empirical smoke test was skipped.")
  pt_log_064("To run it later:")
  pt_log_064("  Sys.setenv(BRIM_EXTERNAL_AUDIT_MULTI_EXTENT = 'TRUE')")
  pt_log_064("  source('02_preprocess/64_audit_external_layer_load_behavior.R')")
}
