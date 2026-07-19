# ==== 24_inventory_pt2_external_overlay_mapserver_sublayers.r =================
##
## PURPOSE:
##   Discover sublayers inside MapServer services used by the PortaTreasure2
##   Tools / External Overlays catalog.
##
## WHY:
##   Many MapServer catalog rows are service-level URLs such as:
##
##     .../MapServer
##
##   Those are good for visual overlays, but they often do not expose useful
##   fields in the field-curation QA sheet.  To curate popups, style fields, and
##   more precise catalog rows, we often need layer-specific URLs such as:
##
##     .../MapServer/0
##     .../MapServer/3
##
## OUTPUTS:
##   04_processed_data/qa/pt2_external_overlay_mapserver_sublayer_inventory_latest.csv
##   04_processed_data/qa/pt2_external_overlay_mapserver_sublayer_inventory_YYYYMMDD_HHMMSS.csv
##   04_processed_data/qa/pt2_external_overlay_mapserver_sublayer_candidates_latest.csv
##   04_processed_data/qa/pt2_external_overlay_mapserver_sublayer_candidates_YYYYMMDD_HHMMSS.csv
##
## WORKFLOW:
##   1. Run this script.
##   2. Review the inventory/candidate CSV.
##   3. Promote only useful sublayer rows into your candidate catalog or manually
##      add approved rows to 00_config/external_service_catalog.csv.
##

# ==== 1. User switches ========================================================

RUN_HTTP_METADATA_CHECKS <- TRUE

## If TRUE, script reads the field-curation QA sheet if present because it has
## richer diagnostics.  If not present, it falls back to the production catalog.
USE_FIELD_CURATION_QA_IF_PRESENT <- TRUE

## Fetch metadata for each discovered sublayer.  This is useful but creates more
## requests.  Keep TRUE for normal use.
FETCH_SUBLAYER_METADATA <- TRUE

## Limit while debugging.  Set to Inf for all MapServer parent rows.
MAX_PARENT_ROWS <- Inf

REQUEST_SLEEP_SECONDS <- 0.10

HTTP_TIMEOUT_SECONDS <- 20

# ==== 2. Packages =============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(jsonlite)
})

# ==== 3. Paths ================================================================

catalog_path <- file.path("00_config", "external_service_catalog.csv")

field_qa_path <- file.path(
  "04_processed_data",
  "qa",
  "pt2_external_overlay_field_curation_test_sheet_latest.csv"
)

qa_dir <- file.path("04_processed_data", "qa")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

inventory_latest_path <- file.path(
  qa_dir,
  "pt2_external_overlay_mapserver_sublayer_inventory_latest.csv"
)

inventory_timestamp_path <- file.path(
  qa_dir,
  paste0("pt2_external_overlay_mapserver_sublayer_inventory_", timestamp, ".csv")
)

candidate_latest_path <- file.path(
  qa_dir,
  "pt2_external_overlay_mapserver_sublayer_candidates_latest.csv"
)

candidate_timestamp_path <- file.path(
  qa_dir,
  paste0("pt2_external_overlay_mapserver_sublayer_candidates_", timestamp, ".csv")
)

# ==== 4. Utility functions ====================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

as_chr <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("")
  x <- as.character(x)
  x <- x[!is.na(x)]
  paste(x, collapse = "; ")
}

normalize_url <- function(url) {
  if (is.null(url) || length(url) == 0) {
    return("")
  }

  url <- as.character(url)
  url[is.na(url)] <- ""

  url |>
    stringr::str_trim() |>
    stringr::str_replace("/+$", "")
}

add_pjson <- function(url) {
  url <- normalize_url(url)[1]

  if (url == "") {
    return("")
  }

  if (isTRUE(stringr::str_detect(url, "\\?"))) {
    paste0(url, "&f=pjson")
  } else {
    paste0(url, "?f=pjson")
  }
}

safe_from_json_url <- function(url, timeout_seconds = HTTP_TIMEOUT_SECONDS) {
  url <- normalize_url(url)[1]

  if (url == "") {
    return(list(.error = "Blank or missing URL."))
  }

  old_timeout <- getOption("timeout")
  options(timeout = timeout_seconds)
  on.exit(options(timeout = old_timeout), add = TRUE)

  tryCatch(
    {
      jsonlite::fromJSON(
        txt = add_pjson(url),
        simplifyVector = FALSE
      )
    },
    error = function(e) {
      list(.error = conditionMessage(e))
    }
  )
}

mapserver_root_url <- function(url) {
  url <- normalize_url(url)[1]

  if (!isTRUE(stringr::str_detect(url, regex("/MapServer", ignore_case = TRUE)))) {
    return("")
  }

  stringr::str_replace(
    url,
    regex("(/MapServer)(/\\d+)?$", ignore_case = TRUE),
    "\\1"
  )
}

mapserver_sublayer_id <- function(url) {
  url <- normalize_url(url)[1]
  m <- stringr::str_match(url, regex("/MapServer/(\\d+)$", ignore_case = TRUE))
  if (is.na(m[1, 2])) "" else m[1, 2]
}

derive_legend_url <- function(url) {
  root <- mapserver_root_url(url)
  if (root == "") "" else paste0(root, "/legend")
}

extent_summary <- function(extent) {
  if (is.null(extent) || !is.list(extent)) {
    return("")
  }

  vals <- c(
    xmin = extent$xmin %||% NA,
    ymin = extent$ymin %||% NA,
    xmax = extent$xmax %||% NA,
    ymax = extent$ymax %||% NA
  )

  if (all(is.na(vals))) {
    return("")
  }

  paste(names(vals), round(as.numeric(vals), 4), sep = "=", collapse = "; ")
}

field_table <- function(fields) {
  if (is.null(fields) || length(fields) == 0) {
    return(tibble::tibble(name = character(), type = character(), alias = character()))
  }

  purrr::map_dfr(fields, function(f) {
    tibble::tibble(
      name = as.character(f$name %||% ""),
      type = as.character(f$type %||% ""),
      alias = as.character(f$alias %||% f$name %||% "")
    )
  }) |>
    dplyr::filter(name != "")
}

collapse_fields <- function(x, max_n = Inf) {
  x <- x[!is.na(x) & x != ""]
  if (is.finite(max_n)) {
    x <- utils::head(x, max_n)
  }
  paste(unique(x), collapse = "; ")
}

is_numeric_type <- function(type) {
  stringr::str_detect(
    stringr::str_to_lower(type),
    "double|single|integer|smallinteger|oid"
  )
}

is_date_type <- function(type) {
  stringr::str_detect(stringr::str_to_lower(type), "date")
}

is_string_type <- function(type) {
  stringr::str_detect(stringr::str_to_lower(type), "string")
}

suggest_popup_fields <- function(ft, max_n = 12) {
  if (nrow(ft) == 0) return("")

  names_l <- stringr::str_to_lower(ft$name)
  aliases_l <- stringr::str_to_lower(ft$alias)

  priority_patterns <- c(
    "name|site|station|stn|well|swn|id|code",
    "date|time|year|season",
    "depth|elev|elevation|level|gse|rpe|measure|msmt|value|result",
    "basin|watershed|river|stream|county|agency|owner|holder|status|type|use",
    "area|acres|miles|length|rank|score|risk|priority|category|class"
  )

  picked <- character()

  for (pat in priority_patterns) {
    idx <- which(stringr::str_detect(names_l, pat) | stringr::str_detect(aliases_l, pat))
    picked <- c(picked, ft$name[idx])
  }

  if (length(picked) == 0) {
    picked <- ft$name
  }

  collapse_fields(picked, max_n = max_n)
}

suggest_label_field <- function(ft) {
  if (nrow(ft) == 0) return("")

  names_l <- stringr::str_to_lower(ft$name)
  aliases_l <- stringr::str_to_lower(ft$alias)

  priority_patterns <- c(
    "site.*name|station.*name|well.*name|name$|common.*name|scientific.*name",
    "site.*code|station.*id|stn.*id|well.*id|objectid|globalid",
    "river|stream|watershed|basin|agency|owner|holder"
  )

  for (pat in priority_patterns) {
    idx <- which(stringr::str_detect(names_l, pat) | stringr::str_detect(aliases_l, pat))
    if (length(idx) > 0) return(ft$name[idx[1]])
  }

  ft$name[1]
}

suggest_style_fields <- function(ft, max_n = 12) {
  if (nrow(ft) == 0) return("")

  ft_num <- ft |>
    dplyr::filter(is_numeric_type(type))

  if (nrow(ft_num) == 0) return("")

  names_l <- stringr::str_to_lower(ft_num$name)
  aliases_l <- stringr::str_to_lower(ft_num$alias)

  avoid <- stringr::str_detect(names_l, "^objectid$|globalid|^fid$|^id$|_id$|code$")

  valuable_patterns <- c(
    "depth|elev|elevation|level|gse|rpe|measure|msmt",
    "risk|score|rank|priority|richness|diversity|rarity|irreplace",
    "area|acres|miles|length|count|density|percent|pct|ratio",
    "flow|cfs|stage|storage|volume|capacity|precip|recharge"
  )

  picked <- character()

  for (pat in valuable_patterns) {
    idx <- which((stringr::str_detect(names_l, pat) | stringr::str_detect(aliases_l, pat)) & !avoid)
    picked <- c(picked, ft_num$name[idx])
  }

  if (length(picked) == 0) {
    picked <- ft_num$name[!avoid]
  }

  collapse_fields(picked, max_n = max_n)
}

# ==== 5. Read parent catalog/QA rows =========================================

input_path <- catalog_path

if (
  isTRUE(USE_FIELD_CURATION_QA_IF_PRESENT) &&
    file.exists(field_qa_path)
) {
  input_path <- field_qa_path
}

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path)
}

message("Reading parent external overlay rows:")
message("  ", input_path)

parents <- readr::read_csv(
  input_path,
  show_col_types = FALSE,
  progress = FALSE
) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

needed_cols <- c(
  "agency", "program", "theme", "display_name", "service_type", "service_url",
  "supports_popups", "default_clickable", "default_opacity", "notes",
  "source_page", "priority", "default_load_mode", "where_clause",
  "large_layer_warning", "min_zoom_live", "min_zoom_current_view", "legend_url",
  "legend_note"
)

for (nm in setdiff(needed_cols, names(parents))) {
  parents[[nm]] <- ""
}

parents <- parents |>
  dplyr::mutate(
    service_url = normalize_url(service_url),
    service_type = stringr::str_to_lower(stringr::str_trim(service_type)),
    mapserver_root = purrr::map_chr(service_url, mapserver_root_url)
  ) |>
  dplyr::filter(
    service_type == "map",
    mapserver_root != ""
  ) |>
  dplyr::distinct(display_name, service_url, .keep_all = TRUE)

if (is.finite(MAX_PARENT_ROWS)) {
  parents <- parents |>
    dplyr::slice_head(n = MAX_PARENT_ROWS)
}

message("MapServer parent rows to inspect: ", nrow(parents))

# ==== 6. Inventory sublayers ==================================================

inventory <- purrr::pmap_dfr(
  parents,
  function(...) {

    parent <- list(...)

    Sys.sleep(REQUEST_SLEEP_SECONDS)

    root_url <- normalize_url(parent$mapserver_root)[1]
    parent_service_url <- normalize_url(parent$service_url)[1]

    service_meta <- safe_from_json_url(root_url)

    if (!is.null(service_meta$.error)) {
      return(tibble::tibble(
        parent_agency = parent$agency,
        parent_program = parent$program,
        parent_theme = parent$theme,
        parent_display_name = parent$display_name,
        parent_service_url = parent_service_url,
        mapserver_root = root_url,
        sublayer_id = "",
        sublayer_name = "",
        sublayer_url = "",
        sublayer_parent_layer_id = "",
        sublayer_default_visibility = "",
        sublayer_min_scale = "",
        sublayer_max_scale = "",
        sublayer_type = "",
        metadata_read_ok = "FALSE",
        metadata_error = service_meta$.error,
        metadata_geometry_type = "",
        metadata_field_count = "",
        metadata_all_fields = "",
        metadata_numeric_fields = "",
        metadata_date_fields = "",
        metadata_string_fields = "",
        suggested_popup_fields = "",
        suggested_label_field = "",
        suggested_style_fields = "",
        legend_url = derive_legend_url(root_url)
      ))
    }

    layers <- service_meta$layers %||% list()

    if (length(layers) == 0) {
      return(tibble::tibble(
        parent_agency = parent$agency,
        parent_program = parent$program,
        parent_theme = parent$theme,
        parent_display_name = parent$display_name,
        parent_service_url = parent_service_url,
        mapserver_root = root_url,
        sublayer_id = "",
        sublayer_name = "",
        sublayer_url = "",
        sublayer_parent_layer_id = "",
        sublayer_default_visibility = "",
        sublayer_min_scale = "",
        sublayer_max_scale = "",
        sublayer_type = "",
        metadata_read_ok = "FALSE",
        metadata_error = "No sublayers listed in service metadata.",
        metadata_geometry_type = "",
        metadata_field_count = "",
        metadata_all_fields = "",
        metadata_numeric_fields = "",
        metadata_date_fields = "",
        metadata_string_fields = "",
        suggested_popup_fields = "",
        suggested_label_field = "",
        suggested_style_fields = "",
        legend_url = derive_legend_url(root_url)
      ))
    }

    purrr::map_dfr(layers, function(layer_info) {

      sublayer_id <- as_chr(layer_info$id %||% "")
      sublayer_name <- as_chr(layer_info$name %||% "")
      sublayer_url <- paste0(root_url, "/", sublayer_id)

      sub_out <- tibble::tibble(
        parent_agency = parent$agency,
        parent_program = parent$program,
        parent_theme = parent$theme,
        parent_display_name = parent$display_name,
        parent_service_url = parent_service_url,
        mapserver_root = root_url,
        sublayer_id = sublayer_id,
        sublayer_name = sublayer_name,
        sublayer_url = sublayer_url,
        sublayer_parent_layer_id = as_chr(layer_info$parentLayerId %||% ""),
        sublayer_default_visibility = as_chr(layer_info$defaultVisibility %||% ""),
        sublayer_min_scale = as_chr(layer_info$minScale %||% ""),
        sublayer_max_scale = as_chr(layer_info$maxScale %||% ""),
        sublayer_type = as_chr(layer_info$type %||% ""),
        metadata_read_ok = "",
        metadata_error = "",
        metadata_geometry_type = "",
        metadata_field_count = "",
        metadata_all_fields = "",
        metadata_numeric_fields = "",
        metadata_date_fields = "",
        metadata_string_fields = "",
        suggested_popup_fields = "",
        suggested_label_field = "",
        suggested_style_fields = "",
        legend_url = derive_legend_url(root_url)
      )

      if (!isTRUE(FETCH_SUBLAYER_METADATA)) {
        return(sub_out)
      }

      Sys.sleep(REQUEST_SLEEP_SECONDS)

      sub_meta <- safe_from_json_url(sublayer_url)

      if (!is.null(sub_meta$.error)) {
        sub_out$metadata_read_ok <- "FALSE"
        sub_out$metadata_error <- sub_meta$.error
        return(sub_out)
      }

      ft <- field_table(sub_meta$fields)

      numeric_fields <- ft |>
        dplyr::filter(is_numeric_type(type)) |>
        dplyr::pull(name)

      date_fields <- ft |>
        dplyr::filter(is_date_type(type)) |>
        dplyr::pull(name)

      string_fields <- ft |>
        dplyr::filter(is_string_type(type)) |>
        dplyr::pull(name)

      sub_out$metadata_read_ok <- "TRUE"
      sub_out$metadata_geometry_type <- as_chr(sub_meta$geometryType %||% "")
      sub_out$metadata_field_count <- as_chr(nrow(ft))
      sub_out$metadata_all_fields <- collapse_fields(ft$name)
      sub_out$metadata_numeric_fields <- collapse_fields(numeric_fields)
      sub_out$metadata_date_fields <- collapse_fields(date_fields)
      sub_out$metadata_string_fields <- collapse_fields(string_fields)
      sub_out$suggested_popup_fields <- suggest_popup_fields(ft)
      sub_out$suggested_label_field <- suggest_label_field(ft)
      sub_out$suggested_style_fields <- suggest_style_fields(ft)

      sub_out
    })
  }
)

# ==== 7. Build catalog-shaped candidate rows =================================

candidate_catalog <- inventory |>
  dplyr::filter(
    sublayer_url != "",
    metadata_read_ok == "TRUE"
  ) |>
  dplyr::mutate(
    agency = dplyr::if_else(parent_agency == "CNRA", "DWR / CNRA", parent_agency),
    program = parent_program,
    theme = parent_theme,
    display_name = paste0(parent_display_name, " — ", sublayer_name),
    service_type = "map",
    service_url = sublayer_url,
    supports_popups = "FALSE",
    default_clickable = "FALSE",
    default_opacity = "0.60",
    notes = paste0(
      "MapServer sublayer candidate discovered from parent service: ",
      parent_display_name,
      ". Review before promoting."
    ),
    source_page = sublayer_url,
    priority = "900",
    default_load_mode = "visual",
    where_clause = "",
    large_layer_warning = "",
    min_zoom_live = "",
    min_zoom_current_view = "",
    legend_note = "Service-rendered MapServer legend.",
    best_use = "MapServer sublayer visual overlay / possible attribute source",
    useful_for_visualization = "service-rendered",
    popup_fields = suggested_popup_fields,
    popup_aliases = "",
    hover_fields = suggested_label_field,
    default_label_field = suggested_label_field,
    out_fields = suggested_popup_fields,
    style_field_candidates = suggested_style_fields,
    default_style_field = "",
    default_style_method = "",
    style_units = "",
    style_legend_title = "",
    style_direction = "",
    field_curation_notes = "Sublayer candidate. Confirm drawing behavior, legend, and attribute usefulness before approval."
  ) |>
  dplyr::select(
    agency,
    program,
    theme,
    display_name,
    service_type,
    service_url,
    supports_popups,
    default_clickable,
    default_opacity,
    notes,
    source_page,
    priority,
    default_load_mode,
    where_clause,
    large_layer_warning,
    min_zoom_live,
    min_zoom_current_view,
    legend_url,
    legend_note,
    best_use,
    useful_for_visualization,
    popup_fields,
    popup_aliases,
    hover_fields,
    default_label_field,
    out_fields,
    style_field_candidates,
    default_style_field,
    default_style_method,
    style_units,
    style_legend_title,
    style_direction,
    field_curation_notes
  ) |>
  dplyr::distinct(display_name, service_url, .keep_all = TRUE)

# ==== 8. Write outputs ========================================================

readr::write_csv(inventory, inventory_latest_path)
readr::write_csv(inventory, inventory_timestamp_path)

readr::write_csv(candidate_catalog, candidate_latest_path)
readr::write_csv(candidate_catalog, candidate_timestamp_path)

message("Wrote MapServer sublayer inventory:")
message("  ", inventory_latest_path)
message("  ", inventory_timestamp_path)
message("Rows: ", nrow(inventory))

message("Wrote MapServer sublayer candidate catalog:")
message("  ", candidate_latest_path)
message("  ", candidate_timestamp_path)
message("Rows: ", nrow(candidate_catalog))
